module Main where

import Config
import Digest
import MD5
import Monitoring
import ParseArgs
import Util

import Data.Label
import Text.Printf
import Control.Monad
import Control.Applicative
import Criterion.Measurement
import System.IO
import System.Environment
import Data.Array.Accelerate                        ( Z(..), (:.)(..) )
import Data.Array.Accelerate.AST                    ( Idx(..) )
import qualified Data.Array.Accelerate              as A
import qualified Data.Array.Accelerate.Array.Sugar  as Sugar
import qualified Data.ByteString.Lazy.Char8         as L


main :: IO ()
main = do
  beginMonitoring
  argv                  <- getArgs
  (conf, _cconf, files) <- parseArgs configHelp configBackend options defaults header footer argv

  -- Read the plain text word lists. This creates a vector of MD5 chunks ready
  -- for hashing.
  --
  putStr "Loading dictionary... " >> hFlush stdout
  (tdict, dict) <- time $ readDict conf (get configDict conf)

  let (Z :. _ :. entries) = A.arrayShape dict
  putStrLn $ printf "%d words in %s" entries (secs tdict)

  -- Attempt to recover one hash at a time by comparing it to entries in the
  -- database. This rehashes the entire word list every time, rather than
  -- pre-computing the hashes and performing a lookup. That approach, known as a
  -- rainbow table, while much faster when multiple iterations of the hashing
  -- function are applied, but is defeated by salting passwords. This is true
  -- even if the salt is known, so long as it is unique for each password.
  --
  let backend = get configBackend conf

      recover hash =
        let abcd = readMD5 hash
            idx  = run1 backend l (A.fromList Z [abcd])
            l digest = A.asnd $ A.loop
                     $ A.toStream (A.constant (Z :. Sugar.All :. stream)) (A.use dict)
                     $ A.toStream (A.constant (Z :. stream)) (iota (Sugar.size (Sugar.shape dict)))
                     $ A.zipWithStream (hashcat digest) (SuccIdx ZeroIdx) ZeroIdx
                     $ A.mapStream (A.unit . md5Round) (SuccIdx (SuccIdx ZeroIdx))
                     $ A.foldStream (A.zipWith max) (A.unit (-1)) (SuccIdx ZeroIdx)
                     $ A.emptyLoop
            iota n = A.generate (A.index1 (A.constant n)) A.unindex1
            stream = maxBound :: Int
        --
        in case idx `A.indexArray` Z of
             -1 -> Nothing
             n  -> Just (extract dict n)

      recoverAll :: [L.ByteString] -> IO (Int,Int)
      recoverAll =
        foldM (\(i,n) h -> maybe (return (i,n+1)) (\t -> showText h t >> return (i+1,n+1)) (recover h)) (0,0)

      showText hash text = do
        L.putStr hash >> putStr ": " >> L.putStrLn text

  -- Queue up all the message digests to process
  --
  digests <- concat . (map L.pack (get configStrings conf) :)
          <$> mapM (\f -> L.lines `fmap` L.readFile f) files

  -- Run the lookup for each unknown hash against the given wordlists.
  --
  putStrLn "Beginning recovery..."
  (trec, (r, t)) <- time (recoverAll digests)

  -- And print a summary of results
  --
  let percent = fromIntegral r / fromIntegral t * 100.0 :: Double
      persec  = (fromIntegral t * fromIntegral entries) / trec
  putStrLn $ printf "\nRecovered %d/%d (%.2f %%) digests in %s, %s"
                      r t percent
                      (showFFloatSIBase (Just 2) 1000 trec   "s")
                      (showFFloatSIBase (Just 2) 1000 persec "Hash/sec")

  when (r == t) $ putStrLn "All hashes recovered (:"

{-
module Main where

import Config
import Digest
import MD5
import Monitoring
import ParseArgs
import Util

import Data.Label
import Text.Printf
import Control.Monad
import Control.Applicative
import Criterion.Measurement
import System.IO
import System.Environment
import Data.Array.Accelerate                    ( Z(..), (:.)(..) )
import qualified Data.Array.Accelerate          as A
import qualified Data.ByteString.Lazy.Char8     as L


main :: IO ()
main = do
  beginMonitoring
  argv                  <- getArgs
  (conf, _cconf, files) <- parseArgs configHelp configBackend options defaults header footer argv

  -- Read the plain text word lists. This creates a vector of MD5 chunks ready
  -- for hashing.
  --
  putStr "Loading dictionary... " >> hFlush stdout
  (tdict, dict) <- time $ readDict conf (get configDict conf)

  let (Z :. _ :. entries) = A.arrayShape dict
  putStrLn $ printf "%d words in %s" entries (secs tdict)

  -- Attempt to recover one hash at a time by comparing it to entries in the
  -- database. This rehashes the entire word list every time, rather than
  -- pre-computing the hashes and performing a lookup. That approach, known as a
  -- rainbow table, while much faster when multiple iterations of the hashing
  -- function are applied, but is defeated by salting passwords. This is true
  -- even if the salt is known, so long as it is unique for each password.
  --
  let backend = get configBackend conf

      recover :: Acc (Vector Word8) -> Acc (Vector Word8)
      recover hash =
        let abcd = readMD5 hash
            idx  = hashcat (A.use dict)) abcd
        --
        in case idx `A.indexArray` Z of
             -1 -> lift (False, [])
             n  -> lift (True, extract dict n)

      recoverAll :: [L.ByteString] -> IO (Int,Int)
      recoverAll digests =
        foldM f (0,0) digests
        where
          f :: (Int, Int) -> L.ByteString -> IO (Int, Int)
          f (i,n) h = 
            case recover h of
              Nothing -> return (i,n+1) 
              Just t -> showText h t >> return (i+1,n+1)

      recoverAllLoop :: [Vector Word8] -> Scalar (Int, Int)
      receverAllLoop digests
        = A.loop
        $ A.useList digests
        $ A.mapStream (hashcat (A.use dict) . readMD5) ZeroIdx
        $ A.
        $ A.collectStream f ZeroIdx
        $ A.mapStream (A.map g) ZeroIdx
        $ A.foldStream (A.zipWith h) (unit (0,0)) ZeroIdx
        $ A.emptyLoop
        where
          f :: Vector Word8 -> IO ()
          f 

          g :: Vector Word8 -> Scalar (Int, Int)
          g x = let b = first elem of x
                in (A.fromIntegral b, 1)

          h :: Scalar (Int, Int) -> Scalar (Int, Int) -> Scalar (Int, Int)
          h x y = let (a,b) = unlift x
                      (c,d) = unlift y
                  in (a+c,b+d)

      showText hash text = do
        L.putStr hash >> putStr ": " >> L.putStrLn text

  -- Queue up all the message digests to process
  --
  digests <- concat . (map L.pack (get configStrings conf) :)
          <$> mapM (\f -> L.lines `fmap` L.readFile f) files

  -- Run the lookup for each unknown hash against the given wordlists.
  --
  putStrLn "Beginning recovery..."
  (trec, (r, t)) <- time (recoverAll digests)

  -- And print a summary of results
  --
  let percent = fromIntegral r / fromIntegral t * 100.0 :: Double
      persec  = (fromIntegral t * fromIntegral entries) / trec
  putStrLn $ printf "\nRecovered %d/%d (%.2f %%) digests in %s, %s"
                      r t percent
                      (showFFloatSIBase (Just 2) 1000 trec   "s")
                      (showFFloatSIBase (Just 2) 1000 persec "Hash/sec")

  when (r == t) $ putStrLn "All hashes recovered (:"

-}