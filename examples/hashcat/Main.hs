{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Config
import Digest
import MD5

import Data.Array.Accelerate                            ( Z(..), (:.)(..) )
import Data.Array.Accelerate.Examples.Internal          as A
import qualified Data.Array.Accelerate                  as A

import Data.Label
import Text.Printf
import Control.Monad
import Control.Applicative
import Criterion.Measurement
import System.IO
import qualified Data.ByteString.Lazy.Char8             as L

import Prelude                                          as P


main :: IO ()
main = do
  initializeTime
  beginMonitoring
  (conf, opts, files)   <- parseArgs options defaults header footer

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
  let backend = get optBackend opts

{--
      recoverSeq hash =
        let abcd = readMD5 hash
            idx  = run1 backend l (A.fromList Z [abcd])
            l digest = A.collect
                     $ A.foldSeq max (-1)
                     $ A.zipWithSeq (hashcatWord digest)
                           (A.toSeq (Z :. All :. Split) (A.use dict))
                           (A.toSeq (Z :. Split) (iota (Sugar.size (Sugar.shape dict))))

            iota n = A.generate (A.index1 (A.constant n)) A.unindex1
        --
        in case idx `A.indexArray` Z of
             -1 -> Nothing
             n  -> Just (extract dict n)

      recoverAll :: [L.ByteString] -> IO (Int,Int)
      recoverAll =
        if get configNoSeq conf
        then go recover
        else go recoverSeq
        where go rec = foldM (\(i,n) h -> maybe (return (i,n+1)) (\t -> showText h t >> return (i+1,n+1)) (rec h)) (0,0)
--}

      recover hash =
        let abcd = readMD5 hash
            idx  = run1 backend (hashcatDict (A.use dict)) (A.fromList Z [abcd])
        --
        in case idx `A.indexArray` Z of
             -1 -> Nothing
             n  -> Just (extract dict n)

      recoverAll :: [L.ByteString] -> IO (Int,Int)
      recoverAll = foldM (\(i,n) h -> maybe (return (i,n+1)) (\t -> showText h t >> return (i+1,n+1)) (recover h)) (0,0)

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

time :: IO a -> IO (Double, a)
time action = do
  start  <- getTime
  result <- action
  end    <- getTime
  let !delta = end - start
  return (delta, result)

