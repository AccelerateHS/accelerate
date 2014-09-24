{-# LANGUAGE CPP           #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}

module ParseArgs (

  module ParseArgs,
  module System.Console.GetOpt,

) where

import ParseArgs.Criterion                              as Criterion

import Data.List
import Data.Label
import System.Exit
import System.Console.GetOpt

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


availableBackends :: (f :-> Backend) -> [OptDescr (f -> f)]
availableBackends backend =
  [ Option  [] [show Interpreter]
            (NoArg (set backend Interpreter))
            "reference implementation (sequential)"

#ifdef ACCELERATE_CUDA_BACKEND
  , Option  [] [show CUDA]
            (NoArg (set backend CUDA))
            "implementation for NVIDIA GPUs (parallel)"
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  , Option  [] [show CPU]
            (NoArg (set backend CPU))
            "LLVM based implementation for multicore CPUs (parallel)"
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
  , Option  [] [show PTX]
            (NoArg (set backend PTX))
            "LLVM based implementation for NVIDIA GPUs (parallel)"
#endif
#ifdef ACCELERATE_LLVM_MULTIDEV_BACKEND
  , Option  [] [show Multi]
            (NoArg (set backend Multi))
            "LLVM based multi-device implementation using CPUs and GPUs (parallel)"
#endif
  ]


-- | Complete the options set by appending a description of the available
--   execution backends.
--
withBackends :: (f :-> Backend) -> [OptDescr (f -> f)] -> [OptDescr (f -> f)]
withBackends backend xs = availableBackends backend ++ xs


-- | Create the help text including a list of the available (and selected)
--   Accelerate backends.
--
fancyHeader :: (config :-> Backend) -> config -> [String] -> [String] -> String
fancyHeader backend opts header footer = unlines (header ++ body ++ footer)
  where
    active this         = if this == show (get backend opts) then "*" else ""
    (ss,bs,ds)          = unzip3 $ map (\(b,d) -> (active b, b, d)) $ concatMap extract (availableBackends backend)
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
    body   = "Available backends:" : table


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


-- | Process the command line arguments and return a tuple consisting of the
-- options structure, options for Criterion, and a list of unrecognised and
-- non-options.
--
-- We drop any command line arguments following a "--".
--
parseArgs :: (config :-> Bool)                  -- ^ access a help flag from the options structure
          -> (config :-> Backend)               -- ^ access the chosen backend from the options structure
          -> [OptDescr (config -> config)]      -- ^ the option descriptions
          -> config                             -- ^ default option set
          -> [String]                           -- ^ header text
          -> [String]                           -- ^ footer text
          -> [String]                           -- ^ command line arguments
          -> IO (config, Criterion.Config, [String])
parseArgs help backend (withBackends backend -> options) config header footer args =
  let
      (argv, rest)      = span (/= "--") args
      criterionOptions  = stripShortOpts Criterion.defaultOptions

      helpMsg err
        =  concat err
        ++ usageInfo (unlines header)               options
        ++ usageInfo "\nGeneric criterion options:" (criterionOptions ++ Criterion.extraOptions)
        ++ Criterion.regressHelp

  in do

  -- Process options for the main program. Any non-options will be split out
  -- here. Unrecognised options get passed to criterion.
  --
  (conf,non,u1) <- case getOpt' Permute options argv of
      (opts,n,u,[]) -> case foldr id config opts of
        conf | False <- get help conf
          -> putStrLn (fancyHeader backend conf header footer) >> return (conf,n,u)
        _ -> putStrLn (helpMsg [])                             >> exitSuccess
      --
      (_,_,_,err) -> error (helpMsg err)

  -- Criterion option processing
  --
  (cconf,_,u2) <- case getOpt' Permute criterionOptions u1 of
      (opts,n,u,[]) -> return (foldr id Criterion.defaultConfig opts, n, u)
      (_,_,_,err)   -> error  (helpMsg err)

  return (conf, cconf, non ++ u2 ++ rest)

