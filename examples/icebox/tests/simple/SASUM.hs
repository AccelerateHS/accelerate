
module SASUM where

import Random

import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc


-- Sum of absolute values
-- ----------------------
sasumAcc :: Vector Float -> Acc (Scalar Float)
sasumAcc xs
  = Acc.fold (+) 0 . Acc.map abs $ Acc.use xs

sasumRef :: UArray Int Float -> UArray () Float
sasumRef xs
  = listArray ((), ()) [Prelude.sum . Prelude.map abs $ elems xs]


-- Main
-- ----

run :: Int -> IO (() -> UArray () Float, () -> Acc (Scalar Float))
run n = withSystemRandom $ \gen -> do
  vec  <- randomUArrayR (-1,1) gen n
  vec' <- convertUArray vec
  --
  return (run_ref vec, run_acc vec')
  where
    {-# NOINLINE run_ref #-}
    run_ref xs () = sasumRef xs
    run_acc xs () = sasumAcc xs

