{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue364
-- Copyright   : [2009..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/364
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue364 (

  test_issue364

) where

import Data.Typeable
import Prelude                                            ( fromInteger, show )
import qualified Prelude                                  as P
#if __GLASGOW_HASKELL__ == 800
import Prelude                                            ( fail )
#endif

import Data.Array.Accelerate                              hiding ( fromInteger )
import Data.Array.Accelerate.Array.Sugar                  as Sugar
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config

import Hedgehog

import Test.Tasty
import Test.Tasty.HUnit


test_issue364 :: RunN -> TestTree
test_issue364 runN =
  testGroup "issue364"
    [ at @TestInt8   $ testElt i8
    , at @TestInt16  $ testElt i16
    , at @TestInt32  $ testElt i32
    , at @TestInt64  $ testElt i64
    -- , at @TestHalf   $ testElt f16
    , at @TestFloat  $ testElt f32
    , at @TestDouble $ testElt f64
    ]
    where
      testElt :: forall e. (Num e, Eq e, P.Num e, P.Enum e, P.Eq e)
              => Gen e
              -> TestTree
      testElt _ =
        testGroup (show (typeOf (undefined :: e)))
          [ testCase "A" $ expectedArray @_ @e Z 64 @=? runN (scanl iappend one) (intervalArray Z 64)
          , testCase "B" $ expectedArray @_ @e Z 65 @=? runN (scanl iappend one) (intervalArray Z 65) -- failed for integral types
          ]


-- interval of summations monoid
--
one,top :: Num e => Exp (e, e)
one = T2 (-1) (-1)
top = T2 (-2) (-2)

iappend :: (Num e, Eq e) => Exp (e,e) -> Exp (e,e) -> Exp (e,e)
iappend x y =
  if x == one             then y   else
  if y == one             then x   else
  if x == top || y == top then top
    else
      let T2 x1 x2 = x
          T2 y1 y2 = y
       in
       if x2 + 1 == y1
         then T2 x1 y2
         else top

intervalArray :: (Shape sh, Elt e, P.Num e, P.Enum e)
    => sh
    -> Int
    -> Array (sh:.Int) (e,e)
intervalArray sh n
  = fromList (sh:.n)
  . P.concat
  $ P.replicate (Sugar.size sh) [ (i,i) | i <- [0.. (P.fromIntegral n-1)] ]

expectedArray :: (Shape sh, Elt e, P.Num e, P.Enum e)
    => sh
    -> Int
    -> Array (sh:.Int) (e,e)
expectedArray sh n
  = fromList (sh:.n+1)
  $ P.concat
  $ P.replicate (Sugar.size sh) $ (-1,-1) : [ (0,i) | i <- [0 .. P.fromIntegral n - 1] ]

