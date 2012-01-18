{-# LANGUAGE FlexibleContexts #-}

module SMVM where

import Random
import SMVM.Matrix

import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate           (Vector, Segments, Acc)
import qualified Data.Array.Accelerate as Acc
import qualified Data.Vector.Unboxed   as V


-- Sparse-matrix vector multiplication
-- -----------------------------------

type SparseVector a = (Vector Int, Vector a)
type SparseMatrix a = (Segments, SparseVector a)

smvmAcc :: SparseMatrix Float -> Vector Float -> Acc (Vector Float)
smvmAcc (segd', (inds', vals')) vec'
  = let
      segd     = Acc.use segd'
      inds     = Acc.use inds'
      vals     = Acc.use vals'
      vec      = Acc.use vec'
      ---
      vecVals  = Acc.backpermute (Acc.shape inds) (\i -> Acc.index1 $ inds Acc.! i) vec
      products = Acc.zipWith (*) vecVals vals
    in
    Acc.foldSeg (+) 0 products segd


-- The reference version will be slow, with many conversions between
-- array/vector/list representations. This will likely skew heap usage
-- calculations, but oh well...
--
type USparseMatrix a = (UArray Int Int, (UArray Int Int, UArray Int a))

smvmRef :: USparseMatrix Float -> UArray Int Float -> UArray Int Float
smvmRef (segd, (inds, values)) vec
  = listArray (0, rangeSize (bounds segd) - 1)
    [sum [ values!i * vec!(inds!i) | i <- range seg] | seg <- segd' ]
  where
    segbegin = scanl  (+) 0 $ elems segd
    segend   = scanl1 (+)   $ elems segd
    segd'    = zipWith (\x y -> (x,y-1)) segbegin segend


-- Main
-- ----

run :: Maybe FilePath -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
run f = withSystemRandom $ \gen ->  do
  -- sparse-matrix
  (segd', smat') <- maybe (randomCSRMatrix gen 512 512) (readCSRMatrix gen) f
  let (ind',val') = V.unzip smat'

  segd <- convertVector segd'
  ind  <- convertVector ind'
  val  <- convertVector val'
  let smat = (segd, (ind,val))

  -- vector
  vec' <- uniformVector gen (V.length segd')
  vec  <- convertVector vec'

  -- multiply!
  return (run_ref (v2a segd', (v2a ind',v2a val')) (v2a vec'), run_acc smat vec)
  where
    {-# NOINLINE run_ref #-}
    run_ref smat vec () = smvmRef smat vec
    run_acc smat vec () = smvmAcc smat vec
    --
    v2a :: (V.Unbox a, IArray UArray a) => V.Vector a -> UArray Int a
    v2a vec = listArray (0, V.length vec - 1) $ V.toList vec

