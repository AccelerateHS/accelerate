{-# LANGUAGE FlexibleContexts #-}

module Filter (filter, filter_ref) where

import Prelude   hiding (replicate, scanl, zip, map, filter, max, min, not, zipWith)
import qualified Prelude

import Data.Array.Unboxed (UArray)
import Data.Array.IArray  (IArray)
import qualified Data.Array.IArray as IArray

import Data.Array.Accelerate

filter :: Elem a 
       => (Exp a -> Exp Bool) 
       -> Acc (Vector a)
       -> Acc (Vector a)
filter p arr
  = let flags               = map (boolToInt . p) arr
        (targetIdx, length) = scanl (+) 0 flags
        arr'                = backpermute (length!(constant ())) id arr
    in
    permute const arr' (\ix -> flags!ix ==* 0 ? (ignore, targetIdx!ix)) arr
    -- FIXME: This is abusing 'permute' in that the first two arguments are
    --        only justified because we know the permutation function will
    --        write to each location in the target exactly once.
    --        Instead, we should have a primitive that directly encodes the
    --        compaction pattern of the permutation function.

filter_ref :: IArray UArray e 
           => (e -> Bool)
           -> UArray Int e 
           -> UArray Int e
filter_ref p xs
  = let xs' = Prelude.filter p (IArray.elems xs)
    in
    IArray.listArray (0, length xs' - 1) xs'
