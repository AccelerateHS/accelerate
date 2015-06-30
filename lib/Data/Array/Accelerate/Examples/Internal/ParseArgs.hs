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
  Options, optBackend, optTest, optBenchmark, optCodespeed, optHostname,
  optVariant, optHelp, optCriterion, optTestFramework,

  module System.Console.GetOpt,

  -- * Executing programs
  module Data.Array.Accelerate.Examples.Internal.Backend,

) where

import Data.Array.Accelerate.Debug                                              ( accInit )
import Data.Array.Accelerate.Examples.Internal.Backend
import qualified Data.Array.Accelerate.Examples.Internal.Criterion.Config       as Criterion
import qualified Data.Array.Accelerate.Examples.Internal.TestFramework.Config   as TestFramework

import Data.List
import Data.Label
import Data.Monoid
import Control.Monad
import System.Exit
import System.Environment
import System.Console.GetOpt
import Text.PrettyPrint.ANSI.Leijen
import Prelude

#ifdef ACCELERATE_ENABLE_CODESPEED
import Data.Char
import Network.BSD
import System.IO.Unsafe
#endif

-- Generic program options
-- -----------------------

data Options = Options
  {
    _optBackend         :: Backend                      -- ^ Accelerate backend to execute programs with
  , _optTest            :: Bool                         -- ^ Should tests be run?
  , _optBenchmark       :: Bool                         -- ^ Should benchmarks be run?
  , _optCodespeed       :: Maybe String                 -- ^ URL of codespeed server to upload results to
  , _optHostname        :: String                       -- ^ Machine name to use for reported results
  , _optVariant         :: String                       -- ^ Variant to use for reported results
  , _optHelp            :: Bool                         -- ^ Display help message (and exit)?
  --
  , _optTestFramework   :: TestFramework.Config         -- ^ Options for test-framework
  , _optCriterion       :: Criterion.Config             -- ^ Options for criterion benchmarks
  }

$(mkLabels [''Options])


-- Options parsing infrastructure
-- ------------------------------

defaultOptions :: Options
defaultOptions = Options
  {
    _optBackend         = backend
  , _optTest            = True
#ifndef ACCELERATE_ENABLE_GUI
  , _optBenchmark       = True
#else
  , _optBenchmark       = False
#endif
  , _optCodespeed       = Nothing
  , _optVariant         = variant
  , _optHostname        = hostname
  , _optHelp            = False
  , _optCriterion       = Criterion.defaultConfig
  , _optTestFramework   = TestFramework.defaultConfig backend
  }
  where
    backend     = defaultBackend
#ifdef ACCELERATE_ENABLE_CODESPEED
    variant     = "accelerate-" ++ show (_optBackend defaultOptions)
    hostname    = unsafePerformIO $ do
      h <- getHostName
      return $ map toLower $ takeWhile (/= '.') h
#else
    variant     = []
    hostname    = []
#endif


options :: [OptDescr (Options -> Options)]
options = availableBackends optBackend ++
  [
    Option  [] ["benchmark"]
            (OptArg (set optBenchmark . maybe True read) "BOOL")
            (describe optBenchmark "enable benchmark mode")

  , Option  [] ["test"]
            (OptArg (set optTest  . maybe True read) "BOOL")
            (describe optTest "enable test mode")

#ifdef ACCELERATE_ENABLE_CODESPEED
  , Option  [] ["upload"]
            (ReqArg (set optCodespeed . Just) "URL")
            "address of codespeed server to upload benchmark results"

  , Option  [] ["hostname"]
            (ReqArg (set optHostname) "HOSTNAME")
            (describe optHostname "hostname to use for reported results")

  , Option  [] ["variant"]
            (ReqArg (set optVariant) "STRING")
            (describe optVariant "variant to use for reported results")
#endif

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
    (ss,bs,ds)          = unzip3 $ map (\(b,d) -> (active b, b, d)) $ concatMap extract (availableBackends optBackend)
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


-- | Extract the option flags
--
extractOptFlags :: [OptDescr a] -> [String]
extractOptFlags = concatMap extract
  where
    extract (Option short long _ _) = map (\s -> '-':s:[]) short ++ map ("--"++) long


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
          -> IO (config, Options, [String])
parseArgs programOptions programConfig header footer = do
  accInit
  args <- getArgs

  let
      -- The option "--list" is ambiguous. It is handled by criterion only when
      -- benchmarks are being run, but if passed to test framework during option
      -- processing it will be consumed and treated as the "--list-tests" flag.
      --
      (argv, rest) =
        let (x,  y)     = span (/= "--") args
            (ls, x')    = partition (== "--list") x
        in
        (x', ls ++ dropWhile (== "--") y)

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

  -- Issue a warning if there are any unrecognised options. Criterion will error
  -- if we are in benchmark mode and there is anything it doesn't understand,
  -- and the error message is somewhat confusing.
  --
  let eco       = extractOptFlags Criterion.extraOptions
      (yes,no)  = partition (\x -> takeWhile (/= '=') x `elem` eco) u4

  unless (null no) $ do
    putStrLn "Warning: unrecognised options"
    putStrLn $ unlines $ map ("  "++) no

  return (c1, c2 { _optCriterion = c3, _optTestFramework = c4 }, yes ++ non ++ rest)

