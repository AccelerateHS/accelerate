{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
--
-- Radix sort for a subclass of element types
--

module Radix where

import Random

import Prelude                  as P
import Data.Array.Accelerate    as A

import Data.Bits
import Data.List                ( sort )
import Data.Array.Unboxed       ( IArray, UArray, listArray, bounds, elems )
import System.Random.MWC


-- Radix sort
-- ----------

class Elt e => Radix e where
  passes :: e {- dummy -} -> Int
  radix  :: Exp Int -> Exp e -> Exp Int

instance Radix Int32 where
  passes    = bitSize
  radix i e = i ==* (passes' - 1) ? (radix' (e `xor` minBound), radix' e)
    where
      radix' x = A.fromIntegral $ (x `A.shiftR` i) .&. 1
      passes'  = constant (passes (undefined :: Int32))

-- For IEEE-754 floating-point representation. Unsafe, but widely supported.
-- TLM: unsafeCoerce does not work in the CUDA backend.
--
-- instance Radix Float where
--   passes _  = 32
--   radix i e = let x = (unsafeCoerce e :: Exp Int32)
--               in  i ==* 31 ? (radix' (x `xor` minBound), radix' (floatFlip x))
--     where
--       floatFlip x = x `testBit` 31 ? (complement x, x)  -- twos-complement negative numbers
--       radix'    x = x `testBit` i  ? (1,0)

--
-- A simple (parallel) radix sort implementation [1].
--
-- [1] G. E. Blelloch. "Prefix sums and their applications." Technical Report
--     CMU-CS-90-190. Carnegie Mellon University. 1990.
--
sortAcc :: Radix a => Acc (Vector a) -> Acc (Vector a)
sortAcc = sortAccBy id

sortAccBy
    :: forall a r. (Elt a, Radix r)
    => (Exp a -> Exp r)
    -> Acc (Vector a)
    -> Acc (Vector a)
sortAccBy rdx arr = foldr1 (>->) (P.map radixPass [0..p-1]) arr
  where
    p = passes (undefined :: r)
    --
    deal f x      = let (a,b) = unlift x in (f ==* 0) ? (a,b)
    radixPass k v = let k'    = unit (constant k)
                        flags = A.map (radix (the k') . rdx) v
                        idown = prescanl (+) 0 . A.map (xor 1)        $ flags
                        iup   = A.map (size v - 1 -) . prescanr (+) 0 $ flags
                        index = A.zipWith deal flags (A.zip idown iup)
                    in
                    permute const v (\ix -> index1 (index!ix)) v


sortRef :: (Ord a, IArray UArray a) => UArray Int a -> UArray Int a
sortRef xs = listArray (bounds xs) $ sort (elems xs)


-- Main
-- ----

run :: Int -> IO (() -> UArray Int Int32, () -> Acc (Vector Int32))
run n = withSystemRandom $ \gen -> do
  vec  <- randomUArrayR (minBound,maxBound) gen n
  vec' <- use `fmap` convertUArray vec
  --
  return (run_ref vec, run_acc vec')
  where
    {-# NOINLINE run_ref #-}
    run_ref xs () = sortRef xs
    run_acc xs () = sortAcc xs

