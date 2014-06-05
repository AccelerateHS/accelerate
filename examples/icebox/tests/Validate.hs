{-# LANGUAGE FlexibleContexts, ParallelListComp #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Validate (Similar(..), validate, validate') where

import Data.Int
import Data.Word
import Data.Array.IArray
import Foreign.C.Types
import Foreign.Storable
import Control.Exception                (assert)
import Unsafe.Coerce

class Similar a where
  sim :: a -> a -> Bool

instance Similar Int     where sim = (==)
instance Similar Int8    where sim = (==)
instance Similar Int16   where sim = (==)
instance Similar Int32   where sim = (==)
instance Similar Int64   where sim = (==)
instance Similar Word    where sim = (==)
instance Similar Word8   where sim = (==)
instance Similar Word16  where sim = (==)
instance Similar Word32  where sim = (==)
instance Similar Word64  where sim = (==)
instance Similar CShort  where sim = (==)
instance Similar CUShort where sim = (==)
instance Similar CInt    where sim = (==)
instance Similar CUInt   where sim = (==)
instance Similar CLong   where sim = (==)
instance Similar CULong  where sim = (==)
instance Similar CLLong  where sim = (==)
instance Similar CULLong where sim = (==)

instance Similar Bool    where sim = (==)
instance Similar Char    where sim = (==)
instance Similar CChar   where sim = (==)
instance Similar CSChar  where sim = (==)
instance Similar CUChar  where sim = (==)

instance Similar Float   where sim = absoluteOrRelative
instance Similar CFloat  where sim = absoluteOrRelative
instance Similar Double  where sim = absoluteOrRelative
instance Similar CDouble where sim = absoluteOrRelative

instance (Similar a, Similar b) => Similar (a,b) where
  (x,y) `sim` (u,v) = x `sim` u && y `sim` v

--
-- http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm
--

absoluteOrRelative :: (Fractional a, Ord a) => a -> a -> Bool
absoluteOrRelative u v
  | abs (u-v) < epsilonAbs = True
  | abs u > abs v          = abs ((u-v) / u) < epsilonRel
  | otherwise              = abs ((v-u) / v) < epsilonRel
  where
    epsilonRel = 0.001
    epsilonAbs = 0.00001


-- Comparisons using lexicographically ordered floating-point numbers
-- reinterpreted as twos-complement integers.
--
lexicographic32 :: (Num a, Storable a) => Int -> a -> a -> Bool
lexicographic32 maxUlps a b
  = assert (sizeOf a == 4 && maxUlps > 0 && maxUlps < 4 * 1024 * 1024)
  $ intDiff < fromIntegral maxUlps
  where
    intDiff = abs (toInt a - toInt b)
    toInt x | x' < 0    = 0x80000000 - x'
            | otherwise = x'
            where x'    = unsafeCoerce x :: Int32


lexicographic64 :: (Num a, Storable a) => Int -> a -> a -> Bool
lexicographic64 maxUlps a b
  = assert (sizeOf a == 8 && maxUlps > 0 && maxUlps < 8 * 1024 * 1024)
  $ intDiff < fromIntegral maxUlps
  where
    intDiff = abs (toInt a - toInt b)
    toInt x | x' < 0    = 0x8000000000000000 - x'
            | otherwise = x'
            where x'    = unsafeCoerce x :: Int64


-- Compare two vectors element-wise for equality, for a given measure of
-- similarity. The index and values are returned for pairs that fail.
--
validate
  :: (IArray array e, Ix ix, Similar e)
  => array ix e
  -> array ix e
  -> [(ix,(e,e))]
validate ref arr = validate' (assocs ref) (elems arr)

validate' :: (Ix ix, Similar e) => [(ix,e)] -> [e] -> [(ix,(e,e))]
validate' ref arr =
  filter (not . uncurry sim . snd) [ (i,(x,y)) | (i,x) <- ref | y <- arr ]

