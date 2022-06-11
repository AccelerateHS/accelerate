{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Spectral.RadixSort
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Spectral.RadixSort (

  test_radixsort,

) where

import Data.Function
import Data.List                                                    ( sortBy )
import Prelude                                                      as P
import qualified Data.Bits                                          as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Bits                              as A
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.Similar

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Test.Tasty
import Test.Tasty.Hedgehog


test_radixsort :: RunN -> TestTree
test_radixsort runN =
  testGroup "radixsort"
    [ at @TestInt8   $ testElt i8
    , at @TestInt16  $ testElt i16
    , at @TestInt32  $ testElt i32
    , at @TestInt64  $ testElt i64
    , at @TestWord8  $ testElt w8
    , at @TestWord16 $ testElt w16
    , at @TestWord32 $ testElt w32
    , at @TestWord64 $ testElt w64
    -- , at @TestFloat  $ testElt f32
    -- , at @TestDouble $ testElt f64
    ]
  where
    testElt :: forall a. (Similar a, P.Ord a, Radix a, Show a)
        => Gen a
        -> TestTree
    testElt e =
      testGroup (show (eltR @a))
        [ testProperty "ascending"    $ test_sort_ascending runN e
        , testProperty "descending"   $ test_sort_descending runN e
        , testProperty "key-value"    $ test_sort_keyval runN e f32
        ]

test_sort_ascending
    :: (P.Ord e, Radix e, Similar e, Show e)
    => RunN
    -> Gen e
    -> Property
test_sort_ascending runN e =
  property $ do
    sh <- forAll ((Z :.) P.<$> Gen.int (Range.linear 0 128))  -- just pick a small array; the algorithm is terrible
    xs <- forAll (array sh e)
    let !go = runN radixsort in go xs ~~~ sortRef P.compare xs

test_sort_descending
    :: (P.Ord e, Radix e, Similar e, Show e)
    => RunN
    -> Gen e
    -> Property
test_sort_descending runN e =
  property $ do
    sh <- forAll ((Z :.) P.<$> Gen.int (Range.linear 0 128))
    xs <- forAll (array sh e)
    let !go = runN (radixsortBy complement) in go xs ~~~ sortRef (flip P.compare) xs

test_sort_keyval
    :: (P.Ord k, Radix k, Similar k, Show k, Elt v, Similar v, Show v)
    => RunN
    -> Gen k
    -> Gen v
    -> Property
test_sort_keyval runN key val =
  property $ do
    sh <- forAll ((Z :.) P.<$> Gen.int (Range.linear 0 128))
    xs <- forAll (array sh ((,) P.<$> key P.<*> val))
    let !go = runN (radixsortBy A.fst) in go xs ~~~ sortRef (P.compare `on` P.fst) xs


class A.Bits e => Radix e where
  passes :: e {- dummy -} -> Int
  radix  :: Exp Int -> Exp e -> Exp Int

instance Radix Int8 where
  passes = P.finiteBitSize
  radix  = radixOfSigned

instance Radix Int16 where
  passes = P.finiteBitSize
  radix  = radixOfSigned

instance Radix Int32 where
  passes = P.finiteBitSize
  radix  = radixOfSigned

instance Radix Int64 where
  passes = P.finiteBitSize
  radix  = radixOfSigned

instance Radix Word8 where
  passes = P.finiteBitSize
  radix  = radixOfUnsigned

instance Radix Word16 where
  passes = P.finiteBitSize
  radix  = radixOfUnsigned

instance Radix Word32 where
  passes = P.finiteBitSize
  radix  = radixOfUnsigned

instance Radix Word64 where
  passes = P.finiteBitSize
  radix  = radixOfUnsigned

radixOfSigned
    :: forall e. (Radix e, A.Bounded e, A.Integral e, A.FromIntegral Int e, A.FromIntegral e Int)
    => Exp Int
    -> Exp e
    -> Exp Int
radixOfSigned i e = i A.== (passes' - 1) ? (radix' (e `xor` minBound), radix' e)
   where
     radix' x = A.fromIntegral $ (x `A.shiftR` A.fromIntegral i) .&. 1
     passes'  = constant (passes (undefined :: e))

radixOfUnsigned
    :: (Radix e, A.Integral e, A.FromIntegral Int e, A.FromIntegral e Int)
    => Exp Int
    -> Exp e
    -> Exp Int
radixOfUnsigned i e = A.fromIntegral $ (e `A.shiftR` A.fromIntegral i) .&. 1


-- A simple (parallel) radix sort implementation [1].
--
-- [1] G. E. Blelloch. "Prefix sums and their applications." Technical Report
--     CMU-CS-90-190. Carnegie Mellon University. 1990.
--
radixsort :: Radix a => Acc (Vector a) -> Acc (Vector a)
radixsort = radixsortBy id

radixsortBy :: forall a r. (Elt a, Radix r) => (Exp a -> Exp r) -> Acc (Vector a) -> Acc (Vector a)
radixsortBy rdx arr = foldr1 (>->) (P.map radixPass [0..p-1]) arr
  where
    p = passes (undefined :: r)
    --
    deal f x      = let (a,b)   = unlift x in (f A.== 0) ? (a,b)
    radixPass k v = let k'      = unit (constant k)
                        flags   = A.map (radix (the k') . rdx) v
                        idown   = prescanl (+) 0 . A.map (xor 1)        $ flags
                        iup     = A.map (size v - 1 -) . prescanr (+) 0 $ flags
                        index   = A.zipWith deal flags (A.zip idown iup)
                    in
                    permute const v (\ix -> Just_ (index1 (index!ix))) v


-- This is rather slow. Speeding up the reference implementation by using, say,
-- vector-algorithms, does not significantly change the runtime.
--
sortRef :: Elt a => (a -> a -> Ordering) -> Vector a -> Vector a
sortRef cmp xs = fromList (arrayShape xs) (sortBy cmp (toList xs))

