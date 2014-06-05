{-# LANGUAGE FlexibleContexts #-}

module Map where

import Random

import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc


-- Tests
-- -----
sqAcc, absAcc :: Vector Float -> Acc (Vector Float)
absAcc = Acc.map abs . Acc.use
sqAcc  = Acc.map (\x -> x * x) . Acc.use

plusAcc :: Exp Float -> Vector Float -> Acc (Vector Float)
plusAcc alpha = Acc.map (+ alpha) . Acc.use


toUA :: (IArray UArray a, IArray UArray b) => ([a] -> [b]) -> UArray Int a -> UArray Int b
toUA f xs = listArray (bounds xs) $ f (elems xs)

sqRef, absRef :: UArray Int Float -> UArray Int Float
absRef = toUA $ Prelude.map abs
sqRef  = toUA $ Prelude.map (\x -> x*x)

plusRef :: Float -> UArray Int Float -> UArray Int Float
plusRef alpha = toUA $ Prelude.map (+alpha)


-- Main
-- ----
run :: String -> Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
run alg n = withSystemRandom $ \gen -> do
  vec   <- randomUArrayR (-1,1) gen n
  vec'  <- convertUArray vec
  alpha <- uniform gen
  --
  let go f g = return (run_ref f vec, run_acc g vec')
  case alg of
    "abs"    -> go absRef absAcc
    "plus"   -> go (plusRef alpha) (plusAcc $ constant alpha)
    "square" -> go sqRef sqAcc
    x        -> error $ "unknown variant: " ++ x

  where
    {-# NOINLINE run_ref #-}
    run_ref f xs () = f xs
    run_acc f xs () = f xs

