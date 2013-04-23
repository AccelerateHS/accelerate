{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (

  parseArgs, Config, configStrings, configDict, configMaxWords, configSkipWords,
  run, run1

) where

import Data.Char
import Data.List
import Data.Label
import System.Exit
import System.Console.GetOpt
import qualified Criterion.Main                         as Criterion
import qualified Criterion.Config                       as Criterion

import Data.Array.Accelerate                            ( Arrays, Acc )
import qualified Data.Array.Accelerate.Interpreter      as Interp
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif

-- | Program configuration
--
data Backend = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
             | CUDA
#endif
  deriving (Bounded, Show)


data Config
  = Config
  {
    -- How to execute the search
    _configBackend              :: Backend

    -- Input data
  , _configStrings              :: [String]
  , _configDict                 :: FilePath
  , _configMaxWords             :: Maybe Int
  , _configSkipWords            :: Int

  , _configHelp                 :: Bool
  }
  deriving Show

$(mkLabels [''Config])

defaultConfig :: Config
defaultConfig = Config
  {
    _configBackend              = maxBound
  , _configStrings              = []
  , _configDict                 = []
  , _configMaxWords             = Nothing
  , _configSkipWords            = 0
  , _configHelp                 = False
  }


-- | Execute Accelerate expressions
--
run :: Arrays a => Config -> Acc a -> a
run config =
  case _configBackend config of
    Interpreter -> Interp.run
#ifdef ACCELERATE_CUDA_BACKEND
    CUDA        -> CUDA.run
#endif


run1 :: (Arrays a, Arrays b) => Config -> (Acc a -> Acc b) -> a -> b
run1 config f =
  case _configBackend config of
    Interpreter -> head . Interp.stream f . return
#ifdef ACCELERATE_CUDA_BACKEND
    CUDA        -> CUDA.run1 f
#endif

-- | The set of backends available to execute the program
--
backends :: [OptDescr (Config -> Config)]
backends =
  [ Option  [] ["interpreter"]
            (NoArg (set configBackend Interpreter))
            "reference implementation (sequential)"

#ifdef ACCELERATE_CUDA_BACKEND
  , Option  [] ["cuda"]
            (NoArg (set configBackend CUDA))
            "implementation for NVIDIA GPUs (parallel)"
#endif
  ]


-- | The set of available command-line options
--
defaultOptions :: [OptDescr (Config -> Config)]
defaultOptions = backends ++
  [ Option      ['s'] []
                (ReqArg (modify configStrings . (:)) "STRING")
                "Lookup the plain text of a given checksum"

  , Option      ['d'] ["dictionary"]
                (ReqArg (set configDict) "FILE")
                "Plain text word list to search against"

  , Option      ['j'] ["skip-words"]
                (ReqArg (set configSkipWords . read) "INT")
                "Skip this many entries from the start of the word list"

  , Option      ['n'] ["max-words"]
                (ReqArg (set configMaxWords . Just . read) "INT")
                "Use at most this many words from the list"

  , Option      ['h', '?'] ["help"]
                (NoArg (set configHelp True))
                "show this help message"
  ]


-- | Process the command line options
--
basicHeader :: String
basicHeader = unlines
  [ "accelerate-hashcat (c) [2013] The Accelerate Team"
  , ""
  , "Usage: accelerate-hashcat -d dictionary [OPTIONS] [file ...]"
  ]

fancyHeader :: Config -> String
fancyHeader opts = unlines (header : table)
  where
    active this         = if this == map toLower (show $ get configBackend opts) then "*" else ""
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
    header = intercalate "\n" [ basicHeader, "Available backends:" ]


parseArgs :: [String] -> IO (Config, Criterion.Config, [String])
parseArgs argv =
  let
      helpMsg err = concat err
        ++ usageInfo basicHeader                    defaultOptions
        ++ usageInfo "\nGeneric criterion options:" Criterion.defaultOptions

  in case getOpt' Permute defaultOptions argv of
      (o,n,u,[])  -> do

        -- pass unrecognised options to criterion
        (cconf, rest)   <- Criterion.parseArgs Criterion.defaultConfig Criterion.defaultOptions u
        let files        = n ++ rest

        case foldr id defaultConfig o of
          conf | False <- get configHelp conf
               , False <- null (get configDict conf)
               , False <- null (get configStrings conf ++ files)
            -> putStrLn (fancyHeader conf) >> return (conf, cconf, files)
          _ -> putStrLn (helpMsg [])       >> exitSuccess

      (_,_,_,err) -> error (helpMsg err)

