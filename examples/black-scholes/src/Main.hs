module Main where

import Random
import BlackScholes

import Criterion.Main
import Control.Exception
import System.Random.MWC
import Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate	    as Acc
import qualified Data.Array.Accelerate.CUDA as CUDA

import System.IO.Unsafe


-- A lazier version of 'Control.Monad.sequence'
--
sequence' :: [IO a] -> IO [a]
sequence' ms = foldr k (return []) ms
    where k m m' = do { x <- m; xs <- unsafeInterleaveIO m'; return (x:xs) }


doTest :: GenIO -> Int -> IO Benchmark
doTest gen m = do
  let n = m * 1000000

  v_sp <- randomVectorR gen (5,30) n
  v_os <- randomVectorR gen (1,100) n
  v_oy <- randomVectorR gen (0.25,10) n

  a_sp <- Acc.use `fmap` convertVector v_sp
  a_os <- Acc.use `fmap` convertVector v_os
  a_oy <- Acc.use `fmap` convertVector v_oy

  let a_psy = CUDA.run
	    . Acc.zipWith (\z t -> let (x,y) = Acc.untuple t :: (Exp Float,Exp Float) in Acc.tuple (x,y,z)) a_oy
	    $ Acc.zip a_sp a_os
  _ <- evaluate (a_psy `Acc.indexArray` 0)

  return $ bgroup ("n=" ++ shows m "M")
    [ bench "accelerate" $ whnf (CUDA.run . blackscholes) a_psy
    , bench "cuda-sdk"   $ blackscholes_sdk v_sp v_os v_oy
    ]

main :: IO ()
main = do
  gen <- create
  defaultMain =<< sequence' (map (doTest gen) [2,4..18])

