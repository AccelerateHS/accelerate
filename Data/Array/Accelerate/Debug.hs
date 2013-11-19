{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS -fno-warn-unused-imports #-}
{-# OPTIONS -fno-warn-unused-binds   #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Debug
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--               [2009..2013] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Embedded array processing language: debugging support (internal). This module
-- provides functionality that is useful for developers of the library.  It is
-- not meant for library users.
--

module Data.Array.Accelerate.Debug (

  -- * Dynamic debugging flags
  dump_sharing, dump_simpl_stats, dump_simpl_iterations, verbose,
  queryFlag, setFlag,

  -- * Tracing
  traceMessage, traceEvent, tracePure,

  -- * Statistics
  inline, ruleFired, knownBranch, betaReduce, substitution, simplifierDone, fusionDone,
  resetSimplCount, simplCount,

) where

-- standard libraries
import Data.Function                            ( on )
import Data.IORef
import Data.Label
import Data.List                                ( groupBy, sortBy, isPrefixOf )
import Data.Ord                                 ( comparing )
import Numeric
import Text.PrettyPrint
import System.CPUTime
import System.Environment
import System.IO.Unsafe                         ( unsafePerformIO )
import qualified Data.Map                       as Map

import Debug.Trace                              ( traceIO, traceEventIO )


-- -----------------------------------------------------------------------------
-- Flag option parsing

data FlagSpec flag = Option String              -- external form
                            flag                -- internal form

data Flags = Flags
  {
    -- debugging
    _dump_sharing               :: !Bool                -- sharing recovery phase
  , _dump_simpl_stats           :: !Bool                -- statistics form fusion/simplification
  , _dump_simpl_iterations      :: !Bool                -- output from each simplifier iteration
  , _verbose                    :: !Bool                -- additional, uncategorised status messages

    -- functionality / phase control
  , _acc_sharing                :: !(Maybe Bool)        -- recover sharing of array computations
  , _exp_sharing                :: !(Maybe Bool)        -- recover sharing of scalar expressions
  , _fusion                     :: !(Maybe Bool)        -- fuse array expressions
  , _simplify                   :: !(Maybe Bool)        -- simplify scalar expressions
  }

$(mkLabels [''Flags])

allFlags :: [FlagSpec (Flags -> Flags)]
allFlags
  =  map (enable  'd') dflags
  ++ map (enable  'f') fflags ++ map (disable 'f') fflags
  where
    enable  p (Option f go) = Option ('-':p:f)        (go True)
    disable p (Option f go) = Option ('-':p:"no-"++f) (go False)


-- These @-d\<blah\>@ flags can be reversed with @-dno-\<blah\>@
--
dflags :: [FlagSpec (Bool -> Flags -> Flags)]
dflags =
  [
    Option "dump-sharing"               (set dump_sharing)              -- print sharing recovery trace
  , Option "dump-simpl-stats"           (set dump_simpl_stats)          -- dump simplifier stats
  , Option "dump-simpl-iterations"      (set dump_simpl_iterations)     -- dump output from each simplifier iteration
  , Option "verbose"                    (set verbose)                   -- print additional information
  ]

-- These @-f\<blah\>@ flags can be reversed with @-fno-\<blah\>@
--
fflags :: [FlagSpec (Bool -> Flags -> Flags)]
fflags =
  [ Option "acc-sharing"                (set' acc_sharing)              -- sharing of array computations
  , Option "exp-sharing"                (set' exp_sharing)              -- sharing of scalar expressions
  , Option "fusion"                     (set' fusion)                   -- fusion of array computations
  , Option "simplify"                   (set' simplify)                 -- scalar expression simplification
--  , Option "unfolding-use-threshold"                                  -- the magic cut-off figure for inlining
  ]
  where
    set' f v = set f (Just v)


initialise :: IO Flags
initialise = parse `fmap` getArgs
  where
    defaults            = Flags False False False False Nothing Nothing Nothing Nothing
    parse               = foldl parse1 defaults
    parse1 opts this    =
      case filter (\(Option flag _) -> this `isPrefixOf` flag) allFlags of
        [Option _ go]   -> go opts
        _               -> opts         -- not specified, or ambiguous


-- Indicates which tracing messages are to be emitted, and which phases are to
-- be run.
--
{-# NOINLINE options #-}
options :: IORef Flags
options = unsafePerformIO $ newIORef =<< initialise

-- Query the status of a flag
--
queryFlag :: (Flags :-> a) -> IO a
queryFlag f = get f `fmap` readIORef options

-- Set the status of a debug flag
--
setFlag :: (Flags :-> a) -> a -> IO ()
setFlag f v = modifyIORef' options (set f v)


-- Execute an action only if the corresponding flag is set
--
when :: (Flags :-> Bool) -> IO () -> IO ()
#ifdef ACCELERATE_DEBUG
when f action = do
  enabled <- queryFlag f
  if enabled then action
             else return ()
#else
when _ _        = return ()
#endif


-- -----------------------------------------------------------------------------
-- Trace messages

-- Emit a trace message if the corresponding debug flag is set.
--
traceMessage :: (Flags :-> Bool) -> String -> IO ()
#ifdef ACCELERATE_DEBUG
traceMessage f str
  = when f
  $ do psec     <- getCPUTime
       let sec   = fromIntegral psec * 1E-12 :: Double
       traceIO   $ showFFloat (Just 2) sec (':':str)
#else
traceMessage _ _ = return ()
#endif

-- Emit a message to the event log if the corresponding debug flag is set
--
traceEvent :: (Flags :-> Bool) -> String -> IO ()
#ifdef ACCELERATE_DEBUG
traceEvent f str = when f (traceEventIO str)
#else
traceEvent _ _   = return ()
#endif

-- Emit a trace message from a pure computation
--
tracePure :: (Flags :-> Bool) -> String -> a -> a
#ifdef ACCELERATE_DEBUG
tracePure f msg next = unsafePerformIO (traceMessage f msg) `seq` next
#else
tracePure _ _   next = next
#endif


-- -----------------------------------------------------------------------------
-- Recording statistics

ruleFired, inline, knownBranch, betaReduce, substitution :: String -> a -> a
inline          = annotate Inline
ruleFired       = annotate RuleFired
knownBranch     = annotate KnownBranch
betaReduce      = annotate BetaReduce
substitution    = annotate Substitution

simplifierDone, fusionDone :: a -> a
simplifierDone  = tick SimplifierDone
fusionDone      = tick FusionDone


-- Add an entry to the statistics counters
--
tick :: Tick -> a -> a
#ifdef ACCELERATE_DEBUG
tick t next = unsafePerformIO (modifyIORef' statistics (simplTick t)) `seq` next
#else
tick _ next = next
#endif

-- Add an entry to the statistics counters with an annotation
--
annotate :: (Id -> Tick) -> String -> a -> a
annotate name ctx = tick (name (Id ctx))


-- Simplifier counts
-- -----------------

data SimplStats
  = Simple         {-# UNPACK #-} !Int          -- when we don't want detailed stats

  | Detail {
      ticks     :: {-# UNPACK #-} !Int,         -- total ticks
      details   :: !TickCount                   -- how many of each type
    }

instance Show SimplStats where
  show = render . pprSimplCount


-- Stores the current statistics counters
--
{-# NOINLINE statistics #-}
statistics :: IORef SimplStats
statistics = unsafePerformIO $ newIORef =<< initSimplCount


-- Initialise the statistics counters. If we are dumping the stats
-- (-ddump-simpl-stats) record extra information, else just a total tick count.
--
initSimplCount :: IO SimplStats
#ifdef ACCELERATE_DEBUG
initSimplCount = do
  d <- queryFlag dump_simpl_stats
  return $! if d then Detail { ticks = 0, details = Map.empty }
                 else Simple 0
#else
initSimplCount = return $! Simple 0
#endif

-- Reset the statistics counters. Do this at the beginning at each HOAS -> de
-- Bruijn conversion + optimisation pass.
--
resetSimplCount :: IO ()
#ifdef ACCELERATE_DEBUG
resetSimplCount = writeIORef statistics =<< initSimplCount
#else
resetSimplCount = return ()
#endif

-- Tick a counter
--
simplTick :: Tick -> SimplStats -> SimplStats
simplTick _ (Simple n)     = Simple (n+1)
simplTick t (Detail n dts) = Detail (n+1) (dts `addTick` t)

-- Pretty print the tick counts. Remarkably reminiscent of GHC style...
--
pprSimplCount :: SimplStats -> Doc
pprSimplCount (Simple n)     = text "Total ticks:" <+> int n
pprSimplCount (Detail n dts)
  = vcat [ text "Total ticks:" <+> int n
         , text ""
         , pprTickCount dts
         ]

simplCount :: IO Doc
simplCount = pprSimplCount `fmap` readIORef statistics


-- Ticks
-- -----

type TickCount = Map.Map Tick Int

data Id = Id String
  deriving (Eq, Ord)

data Tick
  = Inline              Id
  | RuleFired           Id
  | KnownBranch         Id
  | BetaReduce          Id
  | Substitution        Id

  -- tick at each iteration
  | SimplifierDone
  | FusionDone
  deriving (Eq, Ord)


addTick :: TickCount -> Tick -> TickCount
addTick tc t =
  let x = 1 + Map.findWithDefault 0 t tc
  in  x `seq` Map.insert t x tc

pprTickCount :: TickCount -> Doc
pprTickCount counts =
  vcat (map pprTickGroup groups)
  where
    groups  = groupBy sameTag (Map.toList counts)
    sameTag = (==) `on` tickToTag . fst

pprTickGroup :: [(Tick,Int)] -> Doc
pprTickGroup []    = error "pprTickGroup"
pprTickGroup group =
  hang (int groupTotal <+> text groupName)
     2 (vcat [ int n <+> pprTickCtx t | (t,n) <- sortBy (flip (comparing snd)) group ])
  where
    groupName  = tickToStr (fst (head group))
    groupTotal = sum [n | (_,n) <- group]

tickToTag :: Tick -> Int
tickToTag Inline{}              = 0
tickToTag RuleFired{}           = 1
tickToTag KnownBranch{}         = 2
tickToTag BetaReduce{}          = 3
tickToTag Substitution{}        = 4
tickToTag SimplifierDone        = 99
tickToTag FusionDone            = 100

tickToStr :: Tick -> String
tickToStr Inline{}              = "Inline"
tickToStr RuleFired{}           = "RuleFired"
tickToStr KnownBranch{}         = "KnownBranch"
tickToStr BetaReduce{}          = "BetaReduce"
tickToStr Substitution{}        = "Substitution"
tickToStr SimplifierDone        = "SimplifierDone"
tickToStr FusionDone            = "FusionDone"

pprTickCtx :: Tick -> Doc
pprTickCtx (Inline v)           = pprId v
pprTickCtx (RuleFired v)        = pprId v
pprTickCtx (KnownBranch v)      = pprId v
pprTickCtx (BetaReduce v)       = pprId v
pprTickCtx (Substitution v)     = pprId v
pprTickCtx SimplifierDone       = empty
pprTickCtx FusionDone           = empty

pprId :: Id -> Doc
pprId (Id s) = text s

