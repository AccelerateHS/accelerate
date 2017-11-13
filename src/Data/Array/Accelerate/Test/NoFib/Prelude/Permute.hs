{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude.Permute
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude.Permute (

  test_permute

) where

import Control.Monad
import Data.Proxy
import Data.Typeable
import System.IO.Unsafe
import Prelude                                                  as P
import qualified Data.Set                                       as Set

import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Array.Sugar                        as S
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.Similar

import Hedgehog
import qualified Hedgehog.Gen                                   as Gen
import qualified Hedgehog.Range                                 as Range

import Test.Tasty
import Test.Tasty.Hedgehog


test_permute :: RunN -> TestTree
test_permute runN =
  testGroup "permute"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (Similar a, P.Num a, A.Num a)
        => Gen a
        -> TestTree
    testElt e =
      testGroup (show (typeOf (undefined :: a)))
        [ testDim dim1
        , testDim dim2
        , testDim dim3
        ]
      where
        testDim
            :: forall sh. (Shape sh, Slice sh, P.Eq sh)
            => Gen (sh:.Int)
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank (undefined::(sh:.Int))))
            [
              testProperty "scatter->DIM1"      $ test_scatter runN sh dim1 e
            , testProperty "scatter->DIM2"      $ test_scatter runN sh dim2 e
            , testProperty "scatter->DIM3"      $ test_scatter runN sh dim3 e
            , testProperty "accumulate->DIM1"   $ test_accumulate runN sh dim1 e
            , testProperty "accumulate->DIM2"   $ test_accumulate runN sh dim2 e
            , testProperty "accumulate->DIM3"   $ test_accumulate runN sh dim3 e
            ]


test_scatter
    :: forall sh sh' e. (Shape sh, Shape sh', P.Eq sh', Similar e, Elt e)
    => RunN
    -> Gen sh
    -> Gen sh'
    -> Gen e
    -> Property
test_scatter runN dim dim' e =
  property $ do
    sh  <- forAll dim
    sh' <- forAll (dim' `except` \v -> S.size v P.== 0)
    let
        n   = S.size sh
        n'  = S.size sh'
        --
        shfl seen i
          | i P.>= n  = return []
          | otherwise = do
              t  <- Gen.choice [ return  (-1)
                               , Gen.int (Range.linear 0 (n'-1))
                               ]
              ts <- shfl (Set.insert t seen) (i+1)
              --
              case Set.member t seen of
                True  -> return (S.ignore          : ts)
                False -> return (S.fromIndex sh' t : ts)
    --
    def <- forAll (array sh' e)
    new <- forAll (array sh  e)
    ix  <- forAll (fromList sh <$> shfl (Set.singleton (-1)) 0)
    --
    let !go = runN $ \i d v -> A.permute const d (i A.!) v
    go ix def new ~~~ permuteRef const def (ix S.!) new


test_accumulate
    :: (Shape sh, Shape sh', P.Eq sh', Similar e, P.Num e, A.Num e)
    => RunN
    -> Gen sh
    -> Gen sh'
    -> Gen e
    -> Property
test_accumulate runN dim dim' e =
  property $ do
    sh  <- forAll dim
    sh' <- forAll (dim' `except` \v -> S.size v P.== 0)
    let
        n'  = S.size sh'
        def = S.fromFunction sh' (const 0)
    --
    xs  <- forAll (array sh e)
    ix  <- forAll (array sh (Gen.choice [ return S.ignore
                                        , S.fromIndex sh' <$> Gen.int (Range.linear 0 (n'-1))
                                        ]))
    let !go = runN $ \i d v -> A.permute (+) d (i A.!) v
    go ix def xs ~~~ permuteRef (+) def (ix S.!) xs


permuteRef
    :: (Shape sh, Shape sh', P.Eq sh', Elt e)
    => (e -> e -> e)
    -> Array sh' e
    -> (sh -> sh')
    -> Array sh e
    -> Array sh' e
permuteRef f def@(Array _ aold) p arr@(Array _ anew) =
  unsafePerformIO $ do
    let
        sh  = S.shape arr
        sh' = S.shape def
        n   = S.size sh
        --
        go !i
          | i P.>= n  = return ()
          | otherwise = do
              let ix  = S.fromIndex sh i
                  ix' = p ix
              --
              unless (ix' P.== S.ignore) $ do
                let i'  = S.toIndex sh' ix'
                x  <- toElt <$> unsafeReadArrayData anew i
                x' <- toElt <$> unsafeReadArrayData aold i'
                unsafeWriteArrayData aold i' (fromElt (f x x'))
              --
              go (i+1)
    --
    go 0
    return def

