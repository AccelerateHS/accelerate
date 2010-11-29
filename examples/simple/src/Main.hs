{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude hiding (filter)

import DotP
import Filter
import Random
import SAXPY
import SMVM
import ScanSeg

import Data.Array.Accelerate                       (Acc,Z(..),(:.)(..))
import qualified Data.Array.Accelerate             as Acc
import qualified Data.Array.Accelerate.CUDA        as CUDA
import qualified Data.Array.Accelerate.Interpreter as Interp

import Data.Array.Unboxed       (IArray, UArray, Ix, elems, indices, (!))
import System.Random.MWC        (create, uniform, GenIO)
import Control.Exception        (evaluate)
import Control.DeepSeq
import Criterion.Main


instance (Ix dim, IArray UArray e) => NFData (UArray dim e) where
  rnf a = a ! head (indices a) `seq` ()


-- Generate a benchmark test iff the reference and accelerate tests succeed.
--
{-- actually this isn't so good, since toIArray exposes ElemRepr...
benchmark
  :: (IArray UArray e, Ix ix, Acc.Elem ix, Acc.Ix dim, Acc.Elem e)
  => String
  -> (e -> e -> Bool)
  -> (() -> UArray ix e)
  -> (() -> Acc (Acc.Array dim e))
  -> IO Benchmark
--}
benchmark name sim ref acc = do
  putStr "Interpreter : " ; v1 <- validate sim (ref ()) (Acc.toIArray $ Interp.run (acc ()))
  putStr "CUDA        : " ; v2 <- validate sim (ref ()) (Acc.toIArray $ CUDA.run   (acc ()))
  if not (v1 && v2)
     then return $ bgroup "" []
     else return $ bgroup name
                     [ bench "ref"  $ nf ref ()
                     , bench "cuda" $ whnf (CUDA.run . acc) ()
                     ]


-- Tests
--
test_dotp :: GenIO -> Int -> IO Benchmark
test_dotp gen n = do
  putStrLn $ "== Dot Product (n = " ++ shows n ") =="
  xs  <- randomVectorR (-1,1) gen n
  ys  <- randomVectorR (-1,1) gen n
  xs' <- convertVector xs
  ys' <- convertVector ys
  benchmark "dotp" similar (run_ref xs ys) (run_acc xs' ys')
  where
    {-# NOINLINE run_ref #-}
    run_ref x y () = dotp_ref x y
    run_acc x y () = dotp x y

test_saxpy :: GenIO -> Int -> IO Benchmark
test_saxpy gen n = do
  putStrLn $ "== SAXPY (n = " ++ shows n ") =="
  xs    <- randomVectorR (-1,1) gen n
  ys    <- randomVectorR (-1,1) gen n
  xs'   <- convertVector xs
  ys'   <- convertVector ys
  alpha <- uniform gen
  benchmark "saxpy" similar (run_ref alpha xs ys) (run_acc alpha xs' ys')
  where
    {-# NOINLINE run_ref #-}
    run_ref alpha x y () = saxpy_ref alpha x y
    run_acc alpha x y () = saxpy alpha x y


test_filter :: GenIO -> Int -> IO Benchmark
test_filter gen n = do
  putStrLn $ "== Filter (n = " ++ shows n ") =="
  xs  <- randomVectorR (0,1::Float) gen n
  xs' <- convertVector xs
  benchmark "filter" similar (run_ref xs) (run_acc xs')
  where
    {-# NOINLINE run_ref #-}
    run_ref x () = filter_ref (< 0.5) x
    run_acc x () = filter (Acc.<* 0.5) x


test_smvm :: GenIO -> (Int,Int) -> (Int,Int) -> IO Benchmark
test_smvm gen (n,m) (rows,cols) = do
  putStr $ "== SMVM (" ++ shows rows " x " ++ shows cols ", "
  vec   <- randomVectorR (-1,1) gen cols
  segd  <- randomVectorR (n,m)  gen rows
  let nnz = sum (elems segd)
  putStrLn $ shows nnz " non-zeros) =="
  inds  <- randomVectorR (0,cols) gen nnz
  vals  <- randomVectorR (-1,1)   gen nnz
  segd' <- convertVector segd
  vec'  <- convertVector vec
  mat'  <- let v = Acc.fromList (Z:.nnz) (zip (elems inds) (elems vals))
           in  evaluate (v `Acc.indexArray` (Z:.0)) >> return v
  benchmark "smvm" similar (run_ref segd inds vals vec) (run_acc segd' mat' vec')
  where
    {-# NOINLINE run_ref #-}
    run_ref d i x v () = smvm_ref (d, (i,x)) v
    run_acc d x v   () = smvm (d,x) v


test_scanlSeg :: GenIO -> Int -> Int -> IO Benchmark
test_scanlSeg gen n r = do
  putStr $ "== Segmented Prescan (" ++ shows n " segments, "
  seg  <- randomVectorR (0,r-1) gen n :: IO (UArray Int Int)
  seg' <- convertVector seg
  let ne = sum (elems seg)
  putStrLn $ shows ne " elements)"
  xs   <- randomVectorR (-1,1) gen ne :: IO (UArray Int Float)
  xs'  <- convertVector xs
  benchmark "prescanlSeg" similar (run_ref xs seg) (run_acc xs' seg')
  where
    {-# NOINLINE run_ref #-}
    run_ref x s () = prefixSumSeg_ref x s
    run_acc x s () = prefixSumSeg x s


-- Main
--
main :: IO ()
main = do
  putStrLn "Data.Array.Accelerate: simple examples"
  putStrLn "--------------------------------------"

  gen <- create
  defaultMain =<< sequence
    [ test_dotp     gen 100000
    , test_saxpy    gen 100000
    , test_filter   gen 10000
    , test_smvm     gen (0,42) (2400,400)
--    , test_scanlSeg gen 100 200
    ]

