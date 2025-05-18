{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude.Permute
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude.Permute (

  test_permute

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Sugar.Array                            as S
import Data.Array.Accelerate.Sugar.Elt                              as S
import Data.Array.Accelerate.Sugar.Shape                            as S
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.Similar
import qualified Data.Array.Accelerate.Representation.Array         as R

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Test.Tasty
import Test.Tasty.Hedgehog

import System.IO.Unsafe
import Prelude                                                      as P
import qualified Data.Set                                           as Set


test_permute :: RunN -> TestTree
test_permute runN =
  testGroup "permute"
    [ at @TestInt8   $ testElt i8
    , at @TestInt16  $ testElt i16
    , at @TestInt32  $ testElt i32
    , at @TestInt64  $ testElt i64
    , at @TestWord8  $ testElt w8
    , at @TestWord16 $ testElt w16
    , at @TestWord32 $ testElt w32
    , at @TestWord64 $ testElt w64
    , at @TestHalf   $ testElt f16
    , at @TestFloat  $ testElt f32
    , at @TestDouble $ testElt f64
    ]
  where
    testElt
        :: forall a. (Similar a, P.Num a, A.Num a, Show a)
        => Gen a
        -> TestTree
    testElt e =
      testGroup (show (eltR @a))
        [ testDim dim1
        , testDim dim2
        , testDim dim3
        ]
      where
        testDim
            :: forall sh. (Shape sh, Show sh)
            => Gen (sh:.Int)
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank @(sh:.Int)))
            [
              testProperty "scatter->DIM1"      $ test_scatter runN sh dim1 e
            , testProperty "scatter->DIM2"      $ test_scatter runN sh dim2 e
            , testProperty "scatter->DIM3"      $ test_scatter runN sh dim3 e
            , testProperty "accumulate->DIM1"   $ test_accumulate runN sh dim1 e
            , testProperty "accumulate->DIM2"   $ test_accumulate runN sh dim2 e
            , testProperty "accumulate->DIM3"   $ test_accumulate runN sh dim3 e
            ]


{-# NOINLINE test_scatter #-}
test_scatter
    :: forall sh sh' e. (Shape sh, Shape sh', Show sh, Show sh', P.Eq sh', Similar e, Elt e, Show e)
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
          | i P.>= n  = pure []
          | otherwise = do
              t  <- Gen.choice [ pure (-1)
                               , Gen.int (Range.linear 0 (n'-1))
                               ]
              ts <- shfl (Set.insert t seen) (i+1)
              --
              case Set.member t seen of
                True  -> pure (Nothing                  : ts)
                False -> pure (Just (S.fromIndex sh' t) : ts)
    --
    def <- forAll (array sh' e)
    new <- forAll (array sh  e)
    ix  <- forAll (fromList sh P.<$> shfl (Set.singleton (-1)) 0)
    --
    let !go = runN $ \i d v -> A.permute const d (i A.!) v
    go ix def new ~~~ permuteRef const def (ix S.!) new


{-# NOINLINE test_accumulate #-}
test_accumulate
    :: (Shape sh, Shape sh', Show sh, Show sh', P.Eq sh', Similar e, P.Num e, A.Num e, Show e)
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
    ix  <- forAll (array sh (Gen.choice [ pure Nothing
                                        , Just . S.fromIndex sh' P.<$> Gen.int (Range.linear 0 (n'-1))
                                        ]))
    let !go = runN $ \i d v -> A.permute (+) d (i A.!) v
    go ix def xs ~~~ permuteRef (+) def (ix S.!) xs


permuteRef
    :: forall sh sh' e. (Shape sh, Shape sh', P.Eq sh', Elt e)
    => (e -> e -> e)
    -> Array sh' e
    -> (sh -> Maybe sh')
    -> Array sh e
    -> Array sh' e
permuteRef f def@(Array (R.Array _ aold)) p arr@(Array (R.Array _ anew)) =
  unsafePerformIO $ do
    let
        tp  = S.eltR @e
        sh  = S.shape arr
        sh' = S.shape def
        n   = S.size sh
        --
        go !i
          | i P.>= n  = pure ()
          | otherwise = do
              let ix  = S.fromIndex sh i
              case p ix of
                Nothing  -> pure ()
                Just ix' -> do
                  let i'  = S.toIndex sh' ix'
                  x  <- toElt P.<$> readArrayData tp anew i
                  x' <- toElt P.<$> readArrayData tp aold i'
                  writeArrayData tp aold i' (fromElt (f x x'))
              --
              go (i+1)
    --
    go 0
    pure def

