{-# LANGUAGE FlexibleContexts #-}

module Gather where

import Random

import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc hiding ((!))
import Prelude               as P


-- Tests
-- -----

gatherAcc :: Vector Int -> Vector Float -> Acc (Vector Float)
gatherAcc mapV inputV = Acc.gather (use mapV) (use inputV)

gatherIfAcc :: Vector Int -> Vector Int -> Vector Float -> Vector Float -> Acc (Vector Float)
gatherIfAcc mapV maskV defaultV inputV
 = Acc.gatherIf (use mapV) (use maskV) evenAcc (use defaultV) (use inputV)

evenAcc :: Exp Int -> Exp Bool
evenAcc v = (v `mod` 2) ==* 0


gatherRef :: UArray Int Int -> UArray Int Float -> UArray Int Float
gatherRef mapV inputV = amap (\ix -> inputV ! ix) mapV

gatherIfRef :: UArray Int Int -> UArray Int Int -> UArray Int Float -> UArray Int Float -> UArray Int Float
gatherIfRef mapV maskV defaultV inputV
  = listArray (bounds mapV)
  $ P.map (\(mIx, mV, dV) -> if evenRef mV then (inputV ! mIx) else dV)
  $ P.zip3 mapL maskL defaultL
  where
    mapL     = elems mapV
    maskL    = elems maskV
    defaultL = elems defaultV

evenRef :: Int -> Bool
evenRef = even


-- Main
-- ----
run :: String -> Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
run alg n = withSystemRandom $ \gen -> do
  vec       <- randomUArrayR (-1, 1) gen n
  vec'      <- convertUArray vec

  mapV      <- randomUArrayR (0, n - 1) gen n
  mapV'     <- convertUArray mapV

  maskV     <- randomUArrayR (0, n) gen n
  maskV'    <- convertUArray maskV

  defaultV  <- randomUArrayR (-1, 1) gen n
  defaultV' <- convertUArray defaultV

  --
  let go f g = return (run_ref f vec, run_acc g vec')

  case alg of
    "gather"    -> go (gatherRef mapV) (gatherAcc mapV')
    "gather-if" -> go (gatherIfRef mapV maskV defaultV) (gatherIfAcc mapV' maskV' defaultV')
    x           -> error $ "unknown variant: " ++ x

  where
    {-# NOINLINE run_ref #-}
    run_ref f xs () = f xs
    run_acc f xs () = f xs


