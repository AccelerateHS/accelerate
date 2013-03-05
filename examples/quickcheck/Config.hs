{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Config (

  -- options & test configuration
  Options,
  parseArgs,
  optBackend, double, float, int64, int32, int16, int8,

  -- running tests
  run, run1

) where

import Data.Char
import Data.List
import Data.Label
import Control.Monad
import Test.Framework
import System.Console.GetOpt                            ( OptDescr(..), ArgDescr(..) )

import Data.Array.Accelerate                            ( Arrays, Acc )
import qualified Data.Array.Accelerate.Interpreter      as Interp

#ifdef ACCELERATE_CUDA_BACKEND
import qualified Foreign.CUDA.Analysis                  as CUDA
import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif

data Backend = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
             | CUDA
#endif
  deriving (Eq, Enum, Bounded, Show)


data Options = Options
  {
    _optBackend         :: !Backend,
    _double             :: !Bool,
    _float              :: !Bool,
    _int64              :: !Bool,
    _int32              :: !Bool,
    _int16              :: !Bool,
    _int8               :: !Bool
  }
  deriving Show

$( mkLabels [''Options] )

defaultOptions :: Options
defaultOptions =  Options
  { _optBackend         = maxBound
  , _double             = True
  , _float              = True
  , _int64              = True
  , _int32              = True
  , _int16              = False
  , _int8               = False
  }


backends :: [OptDescr (Options -> Options)]
backends =
  [ Option [] ["interpreter"]           (NoArg (set optBackend Interpreter))    "reference implementation (sequential)"
#ifdef ACCELERATE_CUDA_BACKEND
  , Option [] ["cuda"]                  (NoArg (set optBackend CUDA))           "implementation for NVIDIA GPUs (parallel)"
#endif
  ]


run :: Arrays a => Options -> Acc a -> a
run opts = case _optBackend opts of
  Interpreter   -> Interp.run
#ifdef ACCELERATE_CUDA_BACKEND
  CUDA          -> CUDA.run
#endif

run1 :: (Arrays a, Arrays b) => Options -> (Acc a -> Acc b) -> a -> b
run1 opts f = case _optBackend opts of
  Interpreter   -> head . Interp.stream f . return
#ifdef ACCELERATE_CUDA_BACKEND
  CUDA          -> CUDA.run1 f
#endif


-- Display a very basic usage message info: specifically, list the available
-- backends and highlight the active one.
--
usageInfo :: Options -> String
usageInfo opts = unlines (header : table)
  where
    active this         = if this == map toLower (show $ get optBackend opts) then "*" else ""
    (ss,bs,ds)          = unzip3 $ map (\(b,d) -> (active b, b, d)) $ concatMap extract backends
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
    header              = intercalate "\n" $
      [ "accelerate-quickcheck (c) 2012 The Accelerate Team"
      , ""
      , "Usage: accelerate-quickcheck [BACKEND] [OPTIONS]"
      , ""
      , "Available backends:"
      ]

-- Once the backend has been selected, we might need to enable or disable
-- certain classes of tests.
--
-- CUDA: double precision is only supported on certain kinds of hardware. We
--       can't get the exact device the backend will choose to run on, but we do
--       know that it will pick the most capable device available.
--
configureBackend :: Options -> IO Options
configureBackend opts = case _optBackend opts of
  Interpreter   -> return opts
#ifdef ACCELERATE_CUDA_BACKEND
  CUDA          -> do
    CUDA.initialise []
    n           <- CUDA.count
    devs        <- mapM CUDA.device [0 .. n-1]
    props       <- mapM CUDA.props devs
    return      $! opts { _double = any (\dev -> CUDA.computeCapability dev >= CUDA.Compute 1 3) props }
#endif


parseArgs :: [String] -> IO (Options, RunnerOptions)
parseArgs argv = do
  args                  <- interpretArgs argv
  (options, runner)     <- case args of
    Left msg            -> error msg
    Right (opts, rest)  -> (,opts) `liftM` configureBackend (foldl parse1 defaultOptions rest)
  --
  putStrLn      $ usageInfo options
  return        $ (options, runner)
  where
    parse1 opts x       =
      case filter (\(Option _ [f] _ _) -> map toLower x `isPrefixOf` f) backends of
        [Option _ _ (NoArg go) _]   -> go opts
        _                           -> opts

