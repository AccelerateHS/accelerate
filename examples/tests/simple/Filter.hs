{-# LANGUAGE FlexibleContexts #-}

module Filter where

import Random

import System.Random.MWC
import Data.Array.Unboxed       (IArray, UArray, elems, listArray)
import Data.Array.Accelerate    as Acc


-- Filter
-- ------
filterAcc :: Elt a => (Exp a -> Exp Bool) -> Vector a -> Acc (Vector a)
filterAcc p vec = Acc.filter p (use vec)


filterRef :: IArray UArray e => (e -> Bool) -> UArray Int e -> UArray Int e
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

