{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS -fno-warn-unused-imports #-}
{-# OPTIONS -fno-warn-unused-binds   #-}
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

  -- * Conditional tracing
  dump_sharing, dump_simpl_stats, dump_simpl_iterations, verbose,
  queryFlag, setFlag,

  traceMessage, traceEvent, tracePure,

) where

-- standard libraries
import Data.IORef
import Data.Label
import Data.List
import Numeric
import System.CPUTime
import System.Environment
import System.IO.Unsafe                         ( unsafePerformIO )

-- friends
import Data.Array.Accelerate.Pretty ()

#if __GLASGOW_HASKELL__ >= 704
import Debug.Trace                              ( traceIO, traceEventIO )
#else
import Debug.Trace                              ( putTraceMsg )

traceIO :: String -> IO ()
traceIO = putTraceMsg

traceEventIO :: String -> IO ()
traceEventIO = traceIO
#endif


-- -----------------------------------------------------------------------------
-- Flag option parsing

data FlagSpec flag = Option String              -- external form
                            flag                -- internal form

data Flags = Flags
  {
    -- debugging
    _dump_sharing               :: Bool         -- sharing recovery phase
  , _dump_simpl_stats           :: Bool         -- statistics form fusion/simplification
  , _dump_simpl_iterations      :: Bool         -- output from each simplifier iteration
  , _verbose                    :: Bool         -- additional, uncategorised status messages

    -- functionality / phase control
  , _acc_sharing                :: Maybe Bool   -- recover sharing of array computations
  , _exp_sharing                :: Maybe Bool   -- recover sharing of scalar expressions
  , _fusion                     :: Maybe Bool   -- fuse array expressions
  , _simplify                   :: Maybe Bool   -- simplify scalar expressions
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


-- Trace messages
-- --------------

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

