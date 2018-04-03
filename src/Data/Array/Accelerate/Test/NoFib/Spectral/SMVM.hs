{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Spectral.SMVM
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Spectral.SMVM (

  test_smvm,

) where

import Data.Proxy
import Data.Typeable
import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Array.Sugar                            as S
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.Similar

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Test.Tasty
import Test.Tasty.Hedgehog


test_smvm :: RunN -> TestTree
test_smvm runN =
  testGroup "smvm"
    [ at (Proxy::Proxy TestHalf)   $ testElt f16
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
      testProperty (show (typeOf (undefined :: a))) $ test_smvm' runN e


test_smvm' :: (A.Num e, P.Num e, Similar e) => RunN -> Gen e -> Property
test_smvm' runN e =
  property $ do
    (smat, cols) <- forAll (sparseMatrix e)
    vec          <- forAll (array (Z:.cols) e)
    --
    let !go = runN smvm in go smat vec ~~~ smvmRef smat vec


sparseMatrix :: Elt e => Gen e -> Gen (SparseMatrix e, Int)
sparseMatrix e = do
  rows  <- Gen.int (Range.linear 1 256)
  cols  <- Gen.int (Range.linear 1 256)
  seg   <- array (Z:.rows) (Gen.int (Range.linear 0 cols))
  let nnz = P.sum (S.toList seg)
  smat  <- array (Z:.nnz) ((,) <$> Gen.int (Range.linear 0 (cols-1)) <*> e)
  return ((seg,smat), cols)


type SparseVector e = Vector (Int, e)
type SparseMatrix e = (Segments Int, SparseVector e)

smvm :: A.Num a => Acc (SparseMatrix a) -> Acc (Vector a) -> Acc (Vector a)
smvm smat vec
  = let (segd, svec)    = unlift smat
        (inds, vals)    = A.unzip svec

        vecVals         = A.gather inds vec
        products        = A.zipWith (*) vecVals vals
    in
    foldSeg (+) 0 products segd

smvmRef :: (Elt a, P.Num a) => SparseMatrix a -> Vector a -> Vector a
smvmRef (segd, smat) vec =
  fromList (S.shape segd)
           [ P.sum [ val * indexArray vec (Z :. i) | (i,val) <- row ]
                   | row <- splitPlaces (toList segd) (toList smat) ]

