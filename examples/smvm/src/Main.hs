
module Main where

import Util
import Vector
import Matrix
import SMVM

import System.Environment
import System.Random.MWC
import Criterion.Config
import Criterion.Main
import qualified Data.Vector.Unboxed              as V
import qualified Data.Array.Accelerate.CUDA       as CUDA


benchmark :: GenIO -> FilePath -> IO Benchmark
benchmark gen name = do
  -- sparse-matrix
  (segd', smat') <- readCSRMatrix name
  let (ind',val') = V.unzip smat'

  segd <- convertVector segd'
  ind  <- convertVector ind'
  val  <- convertVector val'
  let smat = (segd, (ind,val))

  -- vector
  vec' <- uniformVector gen (V.length segd')
  vec  <- convertVector vec'

  -- multiply!
  return $ bench name
         $ whnf (CUDA.run . smvm smat) vec


main :: IO ()
main = do
  gen  <- create
  (cfg,mtx) <- parseArgs defaultConfig defaultOptions =<< getArgs
  case mtx of
       [] -> putStrLn "Usage: <prog> [matrix.mtx]+"
       _  -> defaultMainWith cfg (return ()) =<< sequence' (map (benchmark gen) mtx)

