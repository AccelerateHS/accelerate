{-# LANGUAGE CPP           #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}

module ParseArgs (

  -- Options parsing
  Backend(..), parseArgs, parseArgs',
  module System.Console.GetOpt,

  -- Evaluating expressions
  run, run1,

) where

import Data.Char
import Data.List
import Data.Label
import Data.Monoid
import System.Exit
import System.Console.GetOpt
import qualified Test.Framework                         as TestFramework
import qualified Criterion.Main                         as Criterion
import qualified Criterion.Config                       as Criterion

import Data.Array.Accelerate                            ( Arrays, Acc )
import qualified Data.Array.Accelerate.Interpreter      as Interp
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif


-- | Execute Accelerate expressions
--
run :: Arrays a => Backend -> Acc a -> a
run Interpreter = Interp.run
#ifdef ACCELERATE_CUDA_BACKEND
run CUDA        = CUDA.run
#endif


run1 :: (Arrays a, Arrays b) => Backend -> (Acc a -> Acc b) -> a -> b
run1 Interpreter f = head . Interp.stream f . return
#ifdef ACCELERATE_CUDA_BACKEND
run1 CUDA        f = CUDA.run1 f
#endif


-- | The set of backends available to execute the program
--
data Backend = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
             | CUDA
#endif
  deriving (Eq, Bounded, Show)

availableBackends :: (f :-> Backend) -> [OptDescr (f -> f)]
availableBackends backend =
  [ Option  [] ["interpreter"]
            (NoArg (set backend Interpreter))
            "reference implementation (sequential)"

#ifdef ACCELERATE_CUDA_BACKEND
  , Option  [] ["cuda"]
            (NoArg (set backend CUDA))
            "implementation for NVIDIA GPUs (parallel)"
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
    active this         = if this == map toLower (show (get backend opts)) then "*" else ""
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
parseArgs help backend (withBackends backend -> options) config header footer (takeWhile (/= "--") -> argv) =
  let
      helpMsg err = concat err
        ++ usageInfo (unlines header)               options
        ++ usageInfo "\nGeneric criterion options:" Criterion.defaultOptions

  in do

  -- Process options for the main program. Any non-options will be split out
  -- here. Unrecognised options get passed to criterion.
  --
  (conf,non,u)  <- case getOpt' Permute options argv of
      (opts,n,u,[]) -> case foldr id config opts of
        conf | False <- get help conf
          -> putStrLn (fancyHeader backend conf header footer) >> return (conf,n,u)
        _ -> putStrLn (helpMsg [])                             >> exitSuccess
      --
      (_,_,_,err) -> error (helpMsg err)

  -- Criterion
  --
  -- TODO: don't bail on unrecognised options. Print to screen, or return for
  --       further processing (e.g. test-framework).
  --
  (cconf, _)    <- Criterion.parseArgs Criterion.defaultConfig Criterion.defaultOptions u

  return (conf, cconf, non)


-- | Same as 'parseArgs', but also return options for test-framework.
--
-- Since Criterion and test-framework both bail if they encounter unrecognised
-- options, we run getOpt' ourselves. This means error messages might be a bit
-- different.
--
parseArgs' :: (config :-> Bool)                  -- ^ access a help flag from the options structure
           -> (config :-> Backend)               -- ^ access the chosen backend from the options structure
           -> [OptDescr (config -> config)]      -- ^ the option descriptions
           -> config                             -- ^ default option set
           -> [String]                           -- ^ header text
           -> [String]                           -- ^ footer text
           -> [String]                           -- ^ command line arguments
           -> IO (config, Criterion.Config, TestFramework.RunnerOptions, [String])
parseArgs' help backend (withBackends backend -> options) config header footer (takeWhile (/= "--") -> argv) =
  let
      helpMsg err = concat err
        ++ usageInfo (unlines header)                    options
        ++ usageInfo "\nGeneric criterion options:"      Criterion.defaultOptions
        ++ usageInfo "\nGeneric test-framework options:" TestFramework.optionsDescription

  in do

  -- In the first round process options for the main program. Any non-options
  -- will be split out here so we can ignore them later. Unrecognised options
  -- get passed to criterion and test-framework.
  --
  (conf,non,u)  <- case getOpt' Permute options argv of
    (opts,n,u,[]) -> case foldr id config opts of
      conf | False <- get help conf
        -> putStrLn (fancyHeader backend conf header footer) >> return (conf,n,u)
      _ -> putStrLn (helpMsg [])                             >> exitSuccess
    --
    (_,_,_,err) -> error (helpMsg err)

  -- Test Framework
  (tfconf, u')  <- case getOpt' Permute TestFramework.optionsDescription u of
    (oas,_,u',[]) | Just os <- sequence oas
                -> return (mconcat os, u')
    (_,_,_,err) -> error (helpMsg err)

  -- Criterion
  (cconf, _)    <- Criterion.parseArgs Criterion.defaultConfig Criterion.defaultOptions u'

  return (conf, cconf, tfconf, non)

