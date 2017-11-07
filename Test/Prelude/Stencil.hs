{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Test.Prelude.Stencil (

  test_stencil,

) where

import Prelude                                                  as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck                                          ( Arbitrary )
import Test.HUnit                                               ( (@?=) )
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Config
import QuickCheck.Arbitrary.Array                               ()

import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.IO.Data.Array.IArray               as A
import Data.Array.Accelerate.IO.Data.Array.Unboxed              as A
import Data.Array.Accelerate.Examples.Internal                  as A
import Data.Array.Unboxed                                       as IArray hiding ( Array )
import qualified Data.Array.IArray                              as IArray


-- TODO:
--
--  * Tests for boundary conditions: Mirror and Wrap
--  * Higher dimensional stencils
--

--
-- Stencil ---------------------------------------------------------------------
--

test_stencil :: Backend -> Config -> Test
test_stencil backend opt = testGroup "stencil" $ catMaybes
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
  , testBoundary
  ]
  where
    testElt :: forall a. (P.Num a, A.Num a, Similar a, Arbitrary a, IArray UArray a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testProperty "1D"                   (test_stencil1D  :: Array DIM1 a -> Property)
          , testProperty "2D 3x3 dense"         (test_stencil2D1 :: Array DIM2 a -> Property)
          , testProperty "2D 3x3 cross"         (test_stencil2D2 :: Array DIM2 a -> Property)
          , testProperty "2D non-symmetric"     (test_stencil2D3 :: Array DIM2 (a,a) -> Property)
          ]

    -- 1D Stencil
    --
    test_stencil1D :: (P.Num a, A.Num a, Similar a, IArray UArray a) => Vector a -> Property
    test_stencil1D vec = toList (acc vec) ~?= elems (ref (toUArray Nothing vec))
      where
        pattern (x,y,z) = x + z - 2 * y

        acc xs = run1 backend (stencil pattern clamp) xs

        ref :: (P.Num e, IArray UArray e) => UArray Int e -> UArray Int e
        ref xs =
          let (minx,maxx)   = bounds xs
              clamp x       = Right (minx `P.max` x `P.min` maxx)
          in
          stencil1DRef pattern clamp xs

    -- 2D Stencil
    --
    test_stencil2D1 :: (P.Num a, A.Num a, Similar a, IArray UArray a) => Array DIM2 a -> Property
    test_stencil2D1 vec = toList (acc vec) ~?= elems (ref (toUArray Nothing vec))
      where
        pattern ( (t1, t2, t3)
                , (l , m,  r )
                , (b1, b2, b3)
                )
                = (t1 + t2 + t3 - l + 4*m - r - b1 - b2 - b3)

        acc xs = run1 backend (stencil pattern (function (const 0))) xs

        ref :: (P.Num a, IArray UArray a) => UArray (Int,Int) a -> UArray (Int,Int) a
        ref xs =
          let
              sh                    = bounds xs
              constant ix
                | inRange sh ix     = Right ix
                | otherwise         = Left 0

          in
          stencil2DRef pattern constant xs


    test_stencil2D2 :: (P.Num a, A.Num a, Similar a, IArray UArray a) => Array DIM2 a -> Property
    test_stencil2D2 vec = toList (acc vec) ~?= elems (ref (toUArray Nothing vec))
      where
        pattern ( (_, t, _)
                , (l, m, r)
                , (_, b, _)
                )
                = (t + l + r + b - 4 * m)

        acc xs =
          let pattern' :: A.Num a => Stencil3x3 a -> Exp a
              pattern' = pattern
          in
          run1 backend (stencil pattern' clamp) xs

        ref :: (P.Num a, IArray UArray a) => UArray (Int,Int) a -> UArray (Int,Int) a
        ref xs =
          let ((minu,minv),(maxu,maxv)) = bounds xs
              clamp (u,v) = Right (minu `P.max` u `P.min` maxu
                                  ,minv `P.max` v `P.min` maxv)
          in
          stencil2DRef pattern clamp xs

    test_stencil2D3 :: (P.Num a, A.Num a, Similar a) => Array DIM2 (a,a) -> Property
    test_stencil2D3 vec = toList (acc vec) ~?= elems (ref (toIArray Nothing vec))
      where
        pattern :: forall a. A.Num a => Stencil3x3 (a,a) -> Exp a
        pattern ( (_, _, _) , (x, _, _) , (y, _, z))
          = let (x1,x2) = unlift x
                (y1,y2) = unlift y
                (z1,z2) = unlift z
            in
            x1 - y2 + y1 - z2 + z1 - x2

        pattern' ( (_, _, _) , (x, _, _) , (y, _, z))
          = let (x1,x2) = x
                (y1,y2) = y
                (z1,z2) = z
            in
            x1 - y2 + y1 - z2 + z1 - x2

        acc xs = run1 backend (stencil pattern (function (\_ -> constant (0,0)))) xs

        ref :: P.Num a => IArray.Array (Int,Int) (a,a) -> IArray.Array (Int,Int) a
        ref xs =
          let
              sh                    = bounds xs
              constant ix
                | inRange sh ix     = Right ix
                | otherwise         = Left (0,0)
          in
          stencil2DRef pattern' constant xs

    -- If the constant boundary is not properly implemented,
    -- then this will lead to a segmentation fault.
    testBoundary :: Maybe Test
    testBoundary = Just . testCase "boundary segfault" $ do
      let f ((x,_,_,_,_),_,_,_,_) = x
          b = function (const 0)
          s = stencil (f::Stencil5x5 Int -> Exp Int) b (A.fill (lift (Z:.1:.1000000 :: DIM2)) (0::Exp Int))
          a = run backend s
      indexArray a (Z:.0:.0) @?= 0

--
-- Reference implementation
--
stencil1DRef
    :: (IArray array a, IArray array b)
    => ((a,a,a) -> b)
    -> (Int -> Either a Int)
    -> array Int a
    -> array Int b
stencil1DRef pattern boundary xs =
  let
      indexAt ix = case boundary ix of
        Left e          -> e
        Right ix'       -> xs IArray.! ix'

      f ix = let x = indexAt (ix-1)
                 y = indexAt ix
                 z = indexAt (ix+1)
             in
             pattern (x,y,z)
   in
  array (bounds xs) [(ix, f ix) | ix <- indices xs]


stencil2DRef :: (IArray array a, IArray array b)
    => (((a,a,a), (a,a,a), (a,a,a)) -> b)
    -> ((Int,Int) -> Either a (Int,Int))
    -> array (Int,Int) a
    -> array (Int,Int) b
stencil2DRef pattern boundary xs =
  let
      indexAt ix = case boundary ix of
        Left e          -> e
        Right ix'       -> xs IArray.! ix'

      f (y,x) = let t1 = indexAt (y-1,x-1)
                    t2 = indexAt (y-1,x  )
                    t3 = indexAt (y-1,x+1)
                    l  = indexAt (y,  x-1)
                    m  = indexAt (y,  x  )
                    r  = indexAt (y,  x+1)
                    b1 = indexAt (y+1,x-1)
                    b2 = indexAt (y+1,x  )
                    b3 = indexAt (y+1,x+1)
                in
                pattern ((t1,t2,t3), (l,m,r), (b1,b2,b3))
  in
  array (bounds xs) [(ix, f ix) | ix <- indices xs]

