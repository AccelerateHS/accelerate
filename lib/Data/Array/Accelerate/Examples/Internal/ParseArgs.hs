{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}
-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal.ParseArgs
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Examples.Internal.ParseArgs (

  -- * Options processing
  parseArgs,
  Options, optBackend, optBenchmark, optTest, optHelp, optCriterion, optTestFramework,

  module System.Console.GetOpt,

  -- * Executing programs
  Backend(..),
  run, run1, run2,

) where

import qualified Data.Array.Accelerate.Examples.Internal.Criterion.Config       as Criterion
import qualified Data.Array.Accelerate.Examples.Internal.TestFramework.Config   as TestFramework

import Data.List
import Data.Label
import Data.Monoid
import System.Exit
import System.Console.GetOpt
import Text.PrettyPrint.ANSI.Leijen

import Data.Array.Accelerate                            ( Arrays, Acc )
import qualified Data.Array.Accelerate                  as A
import qualified Data.Array.Accelerate.Interpreter      as Interp
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import qualified Data.Array.Accelerate.LLVM.Native      as CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.LLVM.PTX         as PTX
#endif
#ifdef ACCELERATE_LLVM_MULTIDEV_BACKEND
import qualified Data.Array.Accelerate.LLVM.Multi       as Multi
#endif


-- Generic program options
-- -----------------------

data Options = Options
  {
    _optBackend         :: Backend                      -- ^ Accelerate backend to execute programs with
  , _optBenchmark       :: Bool                         -- ^ Should benchmarks be run?
  , _optTest            :: Bool                         -- ^ Should tests be run?
  , _optHelp            :: Bool                         -- ^ Display help message (and exit)?
  --
  , _optCriterion       :: Criterion.Config             -- ^ Options for criterion benchmarks
  , _optTestFramework   :: TestFramework.Config         -- ^ Options for test-framework
  }


-- Multiple backend support
-- ------------------------
--
-- This section is all that should need editing to add support for new backends
-- to the accelerate-examples package.
--

-- | Execute Accelerate expressions
--
run :: Arrays a => Backend -> Acc a -> a
run Interpreter = Interp.run
#ifdef ACCELERATE_CUDA_BACKEND
run CUDA        = CUDA.run
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
run CPU         = CPU.run
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
run PTX         = PTX.run
#endif
#ifdef ACCELERATE_LLVM_MULTIDEV_BACKEND
run Multi       = Multi.run
#endif


run1 :: (Arrays a, Arrays b) => Backend -> (Acc a -> Acc b) -> a -> b
run1 Interpreter f = Interp.run1 f
#ifdef ACCELERATE_CUDA_BACKEND
run1 CUDA        f = CUDA.run1 f
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
run1 CPU         f = CPU.run1 f
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
run1 PTX         f = PTX.run1 f
#endif
#ifdef ACCELERATE_LLVM_MULTIDEV_BACKEND
run1 Multi       f = Multi.run1 f
#endif

run2 :: (Arrays a, Arrays b, Arrays c) => Backend -> (Acc a -> Acc b -> Acc c) -> a -> b -> c
run2 backend f x y = run1 backend (A.uncurry f) (x,y)


-- | The set of backends available to execute the program. The example programs
--   all choose 'maxBound' as the default, so there should be some honesty in
--   how this list is sorted.
--
data Backend = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
             | CUDA
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
             | CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
             | PTX
#endif
#ifdef ACCELERATE_LLVM_MULTIDEV_BACKEND
             | Multi
#endif
  deriving (Eq, Bounded)


-- The choice of show instance is important because this will be used to
-- generate the command line flag.
--
instance Show Backend where
  show Interpreter      = "interpreter"
#ifdef ACCELERATE_CUDA_BACKEND
  show CUDA             = "cuda"
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  show CPU              = "llvm-cpu"
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
  show PTX              = "llvm-gpu"
#endif
#ifdef ACCELERATE_LLVM_MULTIDEV_BACKEND
  show Multi            = "llvm-multi"
#endif


-- This TH splice must go before the first use of any of the accessors that are
-- generated are used, and after all of the data declarations are in scope.
--
$(mkLabels [''Options])


-- The set of available backnds. This will be used for both the command line
-- options as well as the fancy header generation.
--
availableBackends :: [OptDescr (Options -> Options)]
availableBackends =
  [ Option  [] [show Interpreter]
            (NoArg (set optBackend Interpreter))
            "reference implementation (sequential)"

#ifdef ACCELERATE_CUDA_BACKEND
  , Option  [] [show CUDA]
            (NoArg (set optBackend CUDA))
            "implementation for NVIDIA GPUs (parallel)"
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  , Option  [] [show CPU]
            (NoArg (set optBackend CPU))
            "LLVM based implementation for multicore CPUs (parallel)"
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
  , Option  [] [show PTX]
            (NoArg (set optBackend PTX))
            "LLVM based implementation for NVIDIA GPUs (parallel)"
#endif
#ifdef ACCELERATE_LLVM_MULTIDEV_BACKEND
  , Option  [] [show Multi]
            (NoArg (set optBackend Multi))
            "LLVM based multi-device implementation using CPUs and GPUs (parallel)"
#endif
  ]


-- Options parsing infrastructure
-- ------------------------------

defaultOptions :: Options
defaultOptions = Options
  {
    _optBackend         = maxBound
  , _optTest            = True
#ifndef ACCELERATE_ENABLE_GUI
  , _optBenchmark       = True
#else
  , _optBenchmark       = False
#endif
  , _optHelp            = False
  , _optCriterion       = Criterion.defaultConfig
  , _optTestFramework   = TestFramework.defaultConfig
  }

options :: [OptDescr (Options -> Options)]
options = availableBackends ++
  [
    Option  [] ["benchmark"]
            (OptArg (set optBenchmark . maybe True read) "BOOL")
            (describe optBenchmark "enable benchmark mode")

  , Option  [] ["test"]
            (OptArg (set optTest  . maybe True read) "BOOL")
            (describe optTest "enable test mode")

  , Option  "h?" ["help"]
            (NoArg  (set optHelp True))
            "show help message"
  ]
  where
    describe f msg
      = msg ++ " (" ++ show (get f defaultOptions) ++ ")"


-- | Format a (console) string as bold text. Assume the user has configured
-- their terminal colours to something that looks good (and avoids the light vs.
-- dark background debate).
--
sectionHeader :: String -> String
sectionHeader = show . bold . text

-- | Generate the list of available (and the selected) Accelerate backends.
--
fancyHeader :: Options -> [String] -> [String] -> String
fancyHeader opts header footer = intercalate "\n" (header ++ body ++ footer)
  where
    active this         = if this == show (get optBackend opts) then "*" else ""
    (ss,bs,ds)          = unzip3 $ map (\(b,d) -> (active b, b, d)) $ concatMap extract availableBackends
    table               = zipWith3 paste (sameLen ss) (sameLen bs) ds
    paste x y z         = "  " ++ x ++ "  " ++ y ++ "  " ++ z
    sameLen xs          = flushLeft ((maximum . map length) xs) xs
    flushLeft n xs      = [ take n (x ++ repeat ' ') | x <- xs ]
    --
    extract (Option _ los _ descr) =
      let losFmt  = intercalate ", " los
      in  case lines descr of
            []          -> [(losFmt, "")]
            (x:xs)      -> (losFmt, x) : [ ("",x') | x' <- xs ]
    --
    body                = "Available backends:" : table


-- | Strip the short option arguments that have a required or optional argument.
-- Because we use several different options groups, the flag and its argument
-- get separated. The user is required to instead use a --flag=value format.
--
stripShortOpts :: [OptDescr a] -> [OptDescr a]
stripShortOpts = map strip
  where
    strip (Option _ long arg@(ReqArg _ _) desc) = Option [] long arg desc
    strip (Option _ long arg@(OptArg _ _) desc) = Option [] long arg desc
    strip x                                     = x


-- | Strip the operational part of the options description structure, so that
-- the option lists can be combined for the purposes of displaying the usage
-- information.
--
_stripArgDescr :: [OptDescr a] -> [OptDescr b]
_stripArgDescr = map strip
  where
    strip (Option s l (NoArg _)    d) = Option s l (NoArg  undefined)   d
    strip (Option s l (ReqArg _ a) d) = Option s l (ReqArg undefined a) d
    strip (Option s l (OptArg _ a) d) = Option s l (OptArg undefined a) d


-- | Process the command line arguments and return a tuple consisting of the
-- user options structure, accelerate-examples options (including options for
-- criterion and test-framework), and a list of unrecognised command line
-- arguments.
--
-- Since criterion and test-framework both bail if they encounter unrecognised
-- options, we run getOpt' ourselves. This means that the error messages might
-- be slightly different.
--
-- Any command line arguments following a "--" are not processed, but are
-- included as part of the unprocessed arguments returned on output.
--
parseArgs :: [OptDescr (config -> config)]      -- ^ the user option descriptions
          -> config                             -- ^ user default option set
          -> [String]                           -- ^ header text
          -> [String]                           -- ^ footer text
          -> [String]                           -- ^ command line arguments
          -> IO (config, Options, [String])
parseArgs programOptions programConfig header footer args =
  let
      -- The option "--list" is ambiguous. It is handled by criterion only when
      -- benchmarks are being run, but if passed to test framework during option
      -- processing it will be consumed and treated as the "--list-tests" flag.
      --
      (argv, rest) =
        let (x,  y)     = span (/= "--") args
            (ls, x')    = partition (== "--list") x
        in
        (x', ls ++ y)

      criterionOptions      = stripShortOpts $ Criterion.defaultOptions ++ Criterion.extraOptions
      testframeworkOptions  = stripShortOpts $ TestFramework.defaultOptions

      helpMsg []  = helpMsg'
      helpMsg err = unlines [ concat err, helpMsg' ]

      section (sectionHeader -> str) opts = usageInfo str opts

      helpMsg'    = unlines
        [ fancyHeader defaultOptions header []
        , ""
        , section "Options:"                    options
        , section "Program options:"            programOptions
        , section "Criterion options:"          criterionOptions
        , Criterion.regressHelp
        , ""
        , section "Test-Framework options:"     testframeworkOptions
        ]

  in do
  -- In the first round process options for the user program. Processing the
  -- user options first means that we can still handle any short or long options
  -- that take arguments but which were not joined with an equals sign; e.g.
  --
  --   "-f blah" or "--foo blah".
  --
  -- Following this phase we must disallow short options with arguments, and
  -- only long options in the form "--foo=blah" will work correctly. This is
  -- because getOpt is splitting the unrecognised options ("--foo") from the
  -- non-option arguments ("blah").
  --
  (c1,non,u1)   <- case getOpt' Permute programOptions argv of
      (opts,n,u,[]) -> case foldr id programConfig opts of
        conf      -> return (conf,n,u)
      (_,_,_,err) -> error (helpMsg err)

  -- The standard accelerate-examples options
  --
  (c2,u2)       <- case getOpt' Permute options u1 of
      (opts,_,u,[]) -> return (foldr id defaultOptions opts, u)
      (_,_,_,err)   -> error (helpMsg err)

  -- Criterion options
  --
  (c3,u3)       <- case getOpt' Permute Criterion.defaultOptions u2 of
      (opts,_,u,[]) -> return (foldr id Criterion.defaultConfig opts, u)
      (_,_,_,err)   -> error  (helpMsg err)

  -- Test framework options
  --
  (c4,u4)       <- case getOpt' Permute testframeworkOptions u3 of
      (opts,_,u,[]) | Just os <- sequence opts
                    -> return (mconcat os, u)
      (_,_,_,err)   -> error  (helpMsg err)

  -- Show the help message if that was requested. This is done last so that the
  -- header is not shown twice in the case of a subsequent options parse error.
  --
  if get optHelp c2
     then putStr   (helpMsg []) >> exitSuccess
     else putStrLn (fancyHeader c2 header footer)

  return (c1, c2 { _optCriterion = c3, _optTestFramework = c4 }, u4 ++ non ++ rest)

