{-# LANGUAGE FlexibleContexts #-}

module Filter where

import Random

import System.Random.MWC
import Data.Array.Unboxed       (IArray, UArray, elems, listArray)
import Data.Array.Accelerate    as Acc


-- Filter
-- ------
filterAcc :: Elt a
          => (Exp a -> Exp Bool)
          -> Vector a
          -> Acc (Vector a)
filterAcc p vec
  = let arr              = Acc.use vec
        flags            = Acc.map (boolToInt . p) arr
        (targetIdx, len) = Acc.scanl' (+) 0 flags
        arr'             = Acc.backpermute (index1 $ the len) id arr
    in
    Acc.permute const arr' (\ix -> flags!ix ==* 0 ? (ignore, index1 $ targetIdx!ix)) arr
    -- FIXME: This is abusing 'permute' in that the first two arguments are
    --        only justified because we know the permutation function will
    --        write to each location in the target exactly once.
    --        Instead, we should have a primitive that directly encodes the
    --        compaction pattern of the permutation function.


filterRef :: IArray UArray e
          => (e -> Bool)
          -> UArray Int e
          -> UArray Int e
filterRef p xs
  = let xs' = Prelude.filter p (elems xs)
    in
    listArray (0, Prelude.length xs' - 1) xs'


-- Main
-- ----

run :: Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
run n = withSystemRandom $ \gen -> do
  vec  <- randomUArrayR (-1,1) gen n
  vec' <- convertUArray vec
  --
  return (run_ref vec, run_acc vec')
  where
    {-# NOINLINE run_ref #-}
    run_ref xs () = filterRef (> 0) xs
    run_acc xs () = filterAcc (>*0) xs

