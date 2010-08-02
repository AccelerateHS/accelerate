{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (filter)

import DotP
import Filter
import Random
import SAXPY
import SMVM

import Data.Array.Accelerate                       (Acc)
import qualified Data.Array.Accelerate             as Acc
import qualified Data.Array.Accelerate.CUDA        as CUDA
import qualified Data.Array.Accelerate.Interpreter as Interp

import Data.Array.Unboxed       (IArray, UArray, Ix, elems, indices, (!))
import System.Random.MWC        (create, uniform, GenIO)
import Control.Exception        (evaluate)
import Control.DeepSeq
import Criterion.Main


instance (Ix dim, IArray UArray e) => NFData (UArray dim e) where
  rnf a = a ! (head (indices a)) `seq` ()


-- Generate a benchmark test iff the reference and accelerate tests succeed.
--
benchmark
  :: (IArray UArray e, Ix dim, Acc.Ix dim, Acc.Elem e)
  => String
  -> (e -> e -> Bool)
  -> (() -> UArray dim e)
  -> (() -> Acc (Acc.Array dim e))
  -> IO Benchmark

benchmark name sim ref acc = do
  putStr "Interpreter : " ; v1 <- validate sim (ref ())  (Acc.toIArray $  Interp.run (acc ()))
  putStr "CUDA        : " ; v2 <- validate sim (ref ()) . Acc.toIArray =<< (CUDA.run (acc ()))
  if not (v1 && v2)
     then return $ bgroup "" []
     else return $ bgroup name
                     [ bench "ref"  $ nf ref ()
                     , bench "cuda" $ (CUDA.run . acc) ()
                     ]


-- Tests
--
test_dotp :: GenIO -> Int -> IO Benchmark
test_dotp gen n = do
  putStrLn $ "== Dot Product (n = " ++ shows n ") =="
  xs  <- randomVector gen id n
  ys  <- randomVector gen id n
  xs' <- convertVector xs
  ys' <- convertVector ys
  benchmark "dotp" similar (run_ref xs ys) (run_acc xs' ys')
  where
    run_ref x y () = dotp_ref x y
    run_acc x y () = dotp x y


test_saxpy :: GenIO -> Int -> IO Benchmark
test_saxpy gen n = do
  putStrLn $ "== SAXPY (n = " ++ shows n ") =="
  xs    <- randomVector gen id n
  ys    <- randomVector gen id n
  xs'   <- convertVector xs
  ys'   <- convertVector ys
  alpha <- uniform gen
  benchmark "saxpy" similar (run_ref alpha xs ys) (run_acc alpha xs' ys')
  where
    run_ref alpha x y () = saxpy_ref alpha x y
    run_acc alpha x y () = saxpy alpha x y


test_filter :: GenIO -> Int -> IO Benchmark
test_filter gen n = do
  putStrLn $ "== Filter (n = " ++ shows n ") =="
  xs  <- randomVector gen id n :: IO (UArray Int Float)
  xs' <- convertVector xs
  benchmark "filter" similar (run_ref xs) (run_acc xs')
  where
    run_ref x () = filter_ref (< 0.5) x
    run_acc x () = filter (Acc.<* 0.5) x


test_smvm :: GenIO -> (Int,Int) -> (Int,Int) -> IO Benchmark
test_smvm gen (n,m) (rows,cols) = do
  putStr $ "== SMVM (" ++ shows rows " x " ++ shows cols ", "
  vec   <- randomVector gen id cols
  segd  <- randomVector gen (\x -> (abs x `rem` (m-n)) + n) rows
  let nnz = sum (elems segd)
  putStrLn $ shows nnz " non-zeros) =="
  inds  <- randomVector gen (\x -> abs x `rem` cols) nnz
  vals  <- randomVector gen id nnz
  segd' <- convertVector segd
  vec'  <- convertVector vec
  mat'  <- let v = Acc.fromList nnz (zip (elems inds) (elems vals))
           in  evaluate (v `Acc.indexArray` 0) >> return v
  benchmark "smvm" similar (run_ref segd inds vals vec) (run_acc segd' mat' vec')
  where
    run_ref d i x v () = smvm_ref (d, (i,x)) v
    run_acc d x v   () = smvm (d,x) v


-- Main
--
main :: IO ()
main = do
  putStrLn "Data.Array.Accelerate: simple examples"
  putStrLn "--------------------------------------"

  gen <- create
  defaultMain =<< sequence
    [ test_dotp   gen 100000
    , test_saxpy  gen 100000
    , test_filter gen 1800
    , test_smvm   gen (0,42) (2400,400)
    ]

