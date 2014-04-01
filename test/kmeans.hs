{-# LANGUAGE CPP #-}
--
-- K-Means sample. This uses the CUDA backend to execute the program.
--
-- Run the generate-samples program first to create some random data.
--

import Prelude                                          as P
import Criterion.Main
import Data.Binary                                      (decodeFile)
import System.Mem

import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.CUDA                       as CUDA
import Data.Array.Accelerate.Math.Kmeans
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import Data.Array.Accelerate.LLVM.Native                as CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import Data.Array.Accelerate.LLVM.PTX                   as GPU
#endif


main :: IO ()
main = do
  points'   <- decodeFile "points.bin"
  clusters' <- read `fmap` readFile "clusters"
  let nclusters = length clusters'
      npoints   = length points'

      clusters :: Vector (Cluster Float)
      clusters  = A.fromList (Z:.nclusters) clusters'

      points :: Vector (Point Float)
      points    = A.fromList (Z:.npoints)   points'

      step      = CUDA.run1 (kmeans nclusters (use points))

  clusters `seq` points `seq` performGC

  putStrLn $ "number of points: " P.++ show npoints

  putStrLn $ "final clusters (CUDA):\n" P.++
    unlines (P.map show . A.toList $ CUDA.run1 step clusters)

#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  putStrLn $ "final clusters (LLVM-CPU):\n" P.++
    unlines (P.map show . A.toList $ CPU.run1 step clusters)
#endif

#ifdef ACCELERATE_LLVM_PTX_BACKEND
  putStrLn $ "final clusters (LLVM-GPU):\n" P.++
    unlines (P.map show . A.toList $ GPU.run1 step clusters)
#endif

  defaultMain
    [ bench "kmeans/CUDA"       $ whnf (CUDA.run1   step) clusters
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
    , bench "kmeans/llvm-cpu"   $ whnf (CPU.run1 step) clusters
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
    , bench "kmeans/llvm-gpu"   $ whnf (GPU.run1 step) clusters
#endif
    ]

