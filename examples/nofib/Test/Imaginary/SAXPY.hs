{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Imaginary.SAXPY (

  test_saxpy,

) where

import Config
import Test.Base
import QuickCheck.Arbitrary.Array                               ()

import Prelude                                                  as P
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Examples.Internal                  as A

import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2


test_saxpy :: Backend -> Config -> Test
test_saxpy backend opt = testGroup "saxpy" $ catMaybes
  [ testElt configInt8   (undefined :: Int8)
  , testElt configInt16  (undefined :: Int16)
  , testElt configInt32  (undefined :: Int32)
  , testElt configInt64  (undefined :: Int64)
  , testElt configWord8  (undefined :: Word8)
  , testElt configWord16 (undefined :: Word16)
  , testElt configWord32 (undefined :: Word32)
  , testElt configWord64 (undefined :: Word64)
  , testElt configFloat  (undefined :: Float)
  , testElt configDouble (undefined :: Double)
  ]
  where
    testElt :: forall a. (Elt a, IsNum a, Similar a, Arbitrary a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just
      $ testProperty (show (typeOf (undefined :: a))) (run_saxpy :: a -> Vector a -> Vector a -> Property)

    run_saxpy alpha xs ys =
      toList (run2 backend (saxpy (constant alpha)) xs ys)
      ~?=
      [ alpha * x + y | x <- toList xs | y <- toList ys ]


-- Accelerate implementation ---------------------------------------------------

saxpy :: (Elt e, IsNum e)
      => Exp e
      -> Acc (Vector e)
      -> Acc (Vector e)
      -> Acc (Vector e)
saxpy alpha xs ys =
  let alpha' = the (unit alpha)
  in
  A.zipWith (\x y -> alpha' * x + y) xs ys

