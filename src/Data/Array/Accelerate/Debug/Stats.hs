{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Simpl
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tick-count statistics collection of the compiler passes, for debugging
-- purposes.
--

module Data.Array.Accelerate.Debug.Stats (

  simplCount, resetSimplCount, dumpSimplStats,
  inline, ruleFired, knownBranch, betaReduce, substitution, simplifierDone, fusionDone,

) where

import Data.Array.Accelerate.Debug.Flags
import Data.Array.Accelerate.Debug.Trace

import Data.Function                                      ( on )
import Data.IORef
import Data.List                                          ( groupBy, sortBy )
import Data.Map                                           ( Map )
import Data.Ord                                           ( comparing )
import Data.Text                                          ( Text )
import Data.Text.Prettyprint.Doc                          hiding ( annotate, Doc )
-- import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc.Render.String
import System.IO.Unsafe
import qualified Data.Map                                 as Map
import qualified Data.Text.Prettyprint.Doc                as Pretty


-- Recording statistics
-- --------------------

ruleFired, inline, knownBranch, betaReduce, substitution :: Text -> a -> a
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
{-# NOINLINE tick #-}
tick t expr = unsafeDupablePerformIO $ do
  modifyIORef' statistics (simplTick t)
  return expr
#else
{-# INLINE tick #-}
tick _ expr = expr
#endif

-- Add an entry to the statistics counters with an annotation
--
annotate :: (Id -> Tick) -> Text -> a -> a
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
  show = show . pprSimplCount


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
  d <- getFlag dump_simpl_stats
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

-- Display simplifier statistics. The counts are reset afterwards.
--
{-# INLINEABLE dumpSimplStats #-}
dumpSimplStats :: IO ()
#ifdef ACCELERATE_DEBUG
dumpSimplStats = do
  when dump_simpl_stats $ do
    stats <- simplCount
    putTraceMsg (renderString (layoutPretty defaultLayoutOptions stats))
    resetSimplCount
#else
dumpSimplStats = return ()
#endif



-- Tick a counter
--
simplTick :: Tick -> SimplStats -> SimplStats
simplTick _ (Simple n)     = Simple (n+1)
simplTick t (Detail n dts) = Detail (n+1) (dts `addTick` t)

-- Pretty print the tick counts. Remarkably reminiscent of GHC style...
--
pprSimplCount :: SimplStats -> Doc
pprSimplCount (Simple n)     = "Total ticks:" <+> pretty n
pprSimplCount (Detail n dts)
  = vcat [ "Total ticks:" <+> pretty n
         , mempty
         , pprTickCount dts
         ]

simplCount :: IO Doc
simplCount = pprSimplCount `fmap` readIORef statistics


-- Ticks
-- -----

type Doc       = Pretty.Doc ()
type TickCount = Map Tick Int

data Id = Id Text
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
  Map.alter f t tc
  where
    f Nothing  = Just 1
    f (Just x) = let x' = x+1 in x' `seq` Just x'

pprTickCount :: TickCount -> Doc
pprTickCount counts =
  vcat (map pprTickGroup groups)
  where
    groups  = groupBy sameTag (Map.toList counts)
    sameTag = (==) `on` tickToTag . fst

pprTickGroup :: [(Tick,Int)] -> Doc
pprTickGroup []  = error "pprTickGroup"
pprTickGroup grp =
  hang 2 (vcat $ (pretty groupTotal <+> groupName)
               : [ pretty n <+> pprTickCtx t | (t,n) <- sortBy (flip (comparing snd)) grp ])
  where
    groupName  = tickToStr (fst (head grp))
    groupTotal = sum [n | (_,n) <- grp]

tickToTag :: Tick -> Int
tickToTag Inline{}              = 0
tickToTag RuleFired{}           = 1
tickToTag KnownBranch{}         = 2
tickToTag BetaReduce{}          = 3
tickToTag Substitution{}        = 4
tickToTag SimplifierDone        = 99
tickToTag FusionDone            = 100

tickToStr :: Tick -> Doc
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
pprTickCtx SimplifierDone       = mempty
pprTickCtx FusionDone           = mempty

pprId :: Id -> Doc
pprId (Id s) = pretty s

