{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Flags
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2014] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Option parsing for debug flags
--

module Data.Array.Accelerate.Debug.Flags (

  Flags, Mode,
  acc_sharing, exp_sharing, fusion, simplify, flush_cache, fast_math, verbose,
  dump_sharing, dump_simpl_stats, dump_simpl_iterations, dump_vectorisation,
  dump_gc, dump_gc_stats, debug_cc, dump_cc, dump_asm, dump_exec, dump_sched,

  queryFlag, setFlag, setFlags, clearFlag, clearFlags,

) where

import Data.Label
import Data.List
import Data.IORef
import Text.PrettyPrint                         hiding ( Mode )
import System.Environment
import System.IO.Unsafe

import Debug.Trace


data FlagSpec flag = Option String              -- external form
                            flag                -- internal form

-- The runtime debug and control options supported by Accelerate. This is a bit
-- awkward, as we process both frontend as well as backend option flags, but
-- gives some control over error messages and overlapping options.
--
fclabels [d|
  data Flags = Flags
    {
      -- Functionality and phase control
      -- -------------------------------
      --
      -- These are Maybe types because they will only override the backend
      -- options if the user specifies a value
      --
      acc_sharing               :: !(Maybe Bool)        -- recover sharing of array computations
    , exp_sharing               :: !(Maybe Bool)        -- recover sharing of scalar expressions
    , fusion                    :: !(Maybe Bool)        -- fuse array expressions
    , simplify                  :: !(Maybe Bool)        -- simplify scalar expressions
--    , unfolding_use_threshold   :: !(Maybe Int)         -- the magic cut-off figure for inlining
    , flush_cache               :: !(Maybe Bool)        -- delete persistent compilation cache(s)
    , fast_math                 :: !(Maybe Bool)        -- use faster, less precise math library operations

      -- Debug trace
      -- -----------
    , verbose                   :: !Bool                -- be very chatty

      -- optimisation and simplification
    , dump_sharing              :: !Bool                -- sharing recovery phase
    , dump_simpl_stats          :: !Bool                -- statistics form fusion/simplification
    , dump_simpl_iterations     :: !Bool                -- output from each simplifier iteration
    , dump_vectorisation        :: !Bool                -- output from the vectoriser

      -- garbage collection
    , dump_gc                   :: !Bool                -- dump GC trace
    , dump_gc_stats             :: !Bool                -- output GC statistics

      -- code generation / compilation
    , debug_cc                  :: !Bool                -- compile with debug symbols
    , dump_cc                   :: !Bool                -- compilation trace
    , dump_asm                  :: !Bool                -- dump generated code

      -- execution
    , dump_exec                 :: !Bool                -- dump execution trace
    , dump_sched                :: !Bool                -- dump scheduler trace
    }
 |]


allFlags :: [FlagSpec (Flags -> Flags)]
allFlags
  =  map (enable  'd') dflags
  ++ map (enable  'f') fflags ++ map (disable 'f') fflags
  where
    enable  p (Option f go) = Option ('-':p:f)        (go True)
    disable p (Option f go) = Option ('-':p:"no-"++f) (go False)


-- These @-f\<blah\>@ phase control flags can be reversed with @-fno-\<blah\>@
--
fflags :: [FlagSpec (Bool -> Flags -> Flags)]
fflags =
  [ Option "acc-sharing"                (set' acc_sharing)
  , Option "exp-sharing"                (set' exp_sharing)
  , Option "fusion"                     (set' fusion)
  , Option "simplify"                   (set' simplify)
  , Option "flush-cache"                (set' flush_cache)
  , Option "fast-math"                  (set' fast_math)
  ]
  where
    set' f v = set f (Just v)

-- These debugging flags default to off and can be enable with @-d\<blah\>@
--
dflags :: [FlagSpec (Bool -> Flags -> Flags)]
dflags =
  [ Option "verbose"                    (set verbose)
  , Option "dump-sharing"               (set dump_sharing)
  , Option "dump-simpl-stats"           (set dump_simpl_stats)
  , Option "dump-simpl-iterations"      (set dump_simpl_iterations)
  , Option "dump-vectorisation"         (set dump_vectorisation)
  , Option "dump-gc"                    (set dump_gc)
  , Option "dump-gc-stats"              (set dump_gc_stats)
  , Option "debug-cc"                   (set debug_cc)
  , Option "dump-cc"                    (set dump_cc)
  , Option "dump-asm"                   (set dump_asm)
  , Option "dump-exec"                  (set dump_exec)
  , Option "dump-sched"                 (set dump_sched)
  ]


-- Initialise the debugging flags structure. This reads from both the command
-- line arguments as well as the environment variable "ACCELERATE_FLAGS".
-- Where applicable, options on the command line take precedence.
--
initialiseFlags :: IO Flags
initialiseFlags = do
  argv  <- getArgs
  env   <- maybe [] words `fmap` lookupEnv "ACCELERATE_FLAGS"
  return $ parse (env ++ argv)
  where
    n                   = Nothing
    f                   = False
    defaults            = Flags n n n n n n f f f f f f f f f f f f

    parse               = foldl parse1 defaults
    parse1 opts this    =
      case filter (\(Option flag _) -> this `isPrefixOf` flag) allFlags of
        [Option _ go]   -> go opts
        []              -> trace unknown opts
        alts            -> trace (ambiguous alts) opts

      where
        unknown         = render $ text "Unknown option:" <+> quotes (text this)
        ambiguous alts  = render $
          vcat [ text "Ambiguous option:" <+> quotes (text this)
               , text ""
               , text "Did you mean one of these?"
               , nest 4 $ vcat (map (\(Option s _) -> text s) alts)
               ]


{-# NOINLINE _flags #-}
_flags :: IORef Flags
_flags = unsafePerformIO $ newIORef =<< initialiseFlags

{-# INLINE queryFlag #-}
queryFlag :: (Flags :-> a) -> IO a
queryFlag f = get f `fmap` readIORef _flags


type Mode = Flags :-> Bool

setFlag, clearFlag :: Mode -> IO ()
setFlag f   = setFlags [f]
clearFlag f = clearFlags [f]

setFlags, clearFlags :: [Mode] -> IO ()
setFlags f   = modifyIORef _flags (\opt -> foldr (flip set True)  opt f)
clearFlags f = modifyIORef _flags (\opt -> foldr (flip set False) opt f)


{--
{-# INLINE mode #-}
mode :: Mode -> Bool
#ifdef ACCELERATE_DEBUG
mode f = unsafePerformIO $! queryFlag f
#else
mode _ = False
#endif

{-# INLINE option #-}
option :: (Flags :-> Maybe a) -> Maybe a
#ifdef ACCELERATE_DEBUG
option f = unsafePerformIO $! queryFlag f
#else
option _ = Nothing
#endif
--}

