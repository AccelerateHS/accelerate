{-# LANGUAGE FlexibleContexts #-}

module Vector where

import Random

import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc


-- Tests
-- -----

initAcc, tailAcc :: Vector Float -> Acc (Vector Float)
initAcc = Acc.init . Acc.use
tailAcc = Acc.tail . Acc.use

takeAcc, dropAcc :: Int -> Vector Float -> Acc (Vector Float)
takeAcc n = (Acc.take $ constant n) . Acc.use
dropAcc n = (Acc.drop $ constant n) . Acc.use

slitAcc :: Int -> Int -> Vector Float -> Acc (Vector Float)
slitAcc i n = (Acc.slit (constant i) (constant n)) . Acc.use

toUA :: (IArray UArray a, IArray UArray b) => ([a] -> [b]) -> UArray Int a -> UArray Int b
toUA f xs = listArray (bounds xs) $ f (elems xs)

initRef, tailRef :: UArray Int Float -> UArray Int Float
initRef = toUA $ Prelude.init
tailRef = toUA $ Prelude.tail

takeRef, dropRef :: Int -> UArray Int Float -> UArray Int Float
takeRef n = toUA $ Prelude.take n
dropRef n = toUA $ Prelude.drop n

slitRef :: Int -> Int -> UArray Int Float -> UArray Int Float
slitRef i n = toUA $ (Prelude.take n . Prelude.drop i)


-- Main
-- ----
run :: String -> Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
run alg n = withSystemRandom $ \gen -> do
  vec   <- randomUArrayR (-1,1) gen n
  vec'  <- convertUArray vec
  ri0   <- uniform gen  -- ri = random int
  ri1   <- uniform gen

  --
  let go f g = return (run_ref f vec, run_acc g vec')
      m   = (abs ri0) `mod` n
      len = (abs ri1) `mod` (n - m)

  case alg of
    "init"   -> go initRef initAcc
    "tail"   -> go tailRef tailAcc
    "take"   -> go (takeRef m) (takeAcc m)
    "drop"   -> go (dropRef m) (dropAcc m)
    "slit"   -> go (slitRef m len) (slitAcc m len)
    x        -> error $ "unknown variant: " ++ x

  where
    {-# NOINLINE run_ref #-}
    run_ref f xs () = f xs
    run_acc f xs () = f xs

