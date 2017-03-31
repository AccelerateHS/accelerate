{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
--
-- Radix sort for a subclass of element types
--

module Test.Spectral.RadixSort (

  test_radixsort,

) where

import Prelude                                                  as P
import Data.Bits                                                as P ( finiteBitSize )
import Data.List
import Data.Label
import Data.Maybe
import Data.Function
import Data.Typeable
import Test.QuickCheck                                          ( Arbitrary )
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import QuickCheck.Arbitrary.Array                               ()
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Data.Bits                          as A
import Data.Array.Accelerate.Examples.Internal                  as A


--
-- Radix sort ------------------------------------------------------------------
--
-- This is rather slow. Speeding up the reference implementation by using, say,
-- vector-algorithms, does not significantly change the runtime. Thus, we stick
-- to the simple list-based representation for the time being.
--

test_radixsort :: Backend -> Config -> Test
test_radixsort backend opt = testGroup "radix sort" $ catMaybes
  [ testElt configInt8   (undefined :: Int8)
  , testElt configInt16  (undefined :: Int16)
  , testElt configInt32  (undefined :: Int32)
  , testElt configInt64  (undefined :: Int64)
  , testElt configWord8  (undefined :: Word8)
  , testElt configWord16 (undefined :: Word16)
  , testElt configWord32 (undefined :: Word32)
  , testElt configWord64 (undefined :: Word64)
  ]
  where
    testElt :: forall a. (Radix a, P.Ord a, A.Integral a, Similar a, Arbitrary a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [
            testProperty "ascending"    (test_ascending  :: Vector a -> Property)
          , testProperty "descending"   (test_descending :: Vector a -> Property)
          , testProperty "(key,val)"    (test_keyval     :: Vector (a,Float) -> Property)
          ]

    test_ascending  xs = toList (run1 backend radixsort xs)                ~?= sort (toList xs)
    test_descending xs = toList (run1 backend (radixsortBy complement) xs) ~?= sortBy (flip compare) (toList xs)
    test_keyval     xs = toList (run1 backend (radixsortBy A.fst) xs)      ~?= sortBy (compare `on` P.fst) (toList xs)


-- Implementation
-- --------------

class A.Bits e => Radix e where
  passes :: e {- dummy -} -> Int
  radix  :: Exp Int -> Exp e -> Exp Int

#define signed(ty)                                                             \
instance Radix ty where ;                                                      \
  passes = P.finiteBitSize ;                                                   \
  radix  = radixOfSigned ;

#define unsigned(ty)                                                           \
instance Radix ty where ;                                                      \
  passes = P.finiteBitSize ;                                                   \
  radix  = radixOfUnsigned ;

signed(Int)
signed(Int8)
signed(Int16)
signed(Int32)
signed(Int64)
unsigned(Word)
unsigned(Word8)
unsigned(Word16)
unsigned(Word32)
unsigned(Word64)

radixOfSigned :: forall e. (Radix e, A.Bounded e, A.Integral e, A.FromIntegral e Int) => Exp Int -> Exp e -> Exp Int
radixOfSigned i e = i A.== (passes' - 1) ? (radix' (e `xor` minBound), radix' e)
   where
     radix' x = A.fromIntegral $ (x `A.shiftR` i) .&. 1
     passes'  = constant (passes (undefined :: e))

radixOfUnsigned :: (Radix e, A.Integral e, A.FromIntegral e Int) => Exp Int -> Exp e -> Exp Int
radixOfUnsigned i e = A.fromIntegral $ (e `A.shiftR` i) .&. 1


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
                    permute const v (\ix -> index1 (index!ix)) v


