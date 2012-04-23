{-# LANGUAGE FlexibleContexts #-}

module Scatter where

import Random

import Control.Monad
import Data.Array.ST
import System.Random.MWC
import Data.Array.Unboxed
import qualified Data.Array.MArray  as M
import Data.Array.Accelerate        as Acc hiding ((!))
import Prelude                      as P


-- Tests
-- -----

scatterAcc :: Vector Int -> Vector Float -> Vector Float -> Acc (Vector Float)
scatterAcc mapV defaultV inputV = Acc.scatter (use mapV) (use defaultV) (use inputV)

scatterIfAcc :: Vector Int -> Vector Int -> Vector Float -> Vector Float -> Acc (Vector Float)
scatterIfAcc mapV maskV defaultV inputV
 = Acc.scatterIf (use mapV) (use maskV) evenAcc (use defaultV) (use inputV)

evenAcc :: Exp Int -> Exp Bool
evenAcc v = (v `mod` 2) ==* 0


scatterRef :: UArray Int Int -> UArray Int Float -> UArray Int Float -> UArray Int Float
scatterRef mapV defaultV inputV = runSTUArray $ do
  mu <- M.thaw defaultV
  forM_ (P.zip [0..] $ elems mapV) $ \(inIx, outIx) -> do
    writeArray mu outIx (inputV ! inIx)
  return mu

scatterIfRef :: UArray Int Int -> UArray Int Int -> UArray Int Float -> UArray Int Float -> UArray Int Float
scatterIfRef mapV maskV defaultV inputV = runSTUArray $ do
  mu <- M.thaw defaultV
  forM_ (P.zip [0..] $ elems mapV) $ \(inIx, outIx) -> do
    when (evenRef (maskV ! inIx)) $ do
      writeArray mu outIx (inputV ! inIx)
  return mu

evenRef :: Int -> Bool
evenRef = even


-- Main
-- ----
run :: String -> Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
run alg n = withSystemRandom $ \gen -> do
  let m = 2 * n

  vec       <- randomUArrayR (-1, 1) gen n
  vec'      <- convertUArray vec

  mapV      <- randomUArrayR (0, m - 1) gen n
  mapV'     <- convertUArray mapV

  maskV     <- randomUArrayR (0, n) gen n
  maskV'    <- convertUArray maskV

  defaultV  <- randomUArrayR (-1, 1) gen m
  defaultV' <- convertUArray defaultV

  --
  let go f g = return (run_ref f vec, run_acc g vec')

  case alg of
    "scatter"    -> go (scatterRef mapV defaultV) (scatterAcc mapV' defaultV')
    "scatter-if" -> go (scatterIfRef mapV maskV defaultV) (scatterIfAcc mapV' maskV' defaultV')
    x           -> error $ "unknown variant: " ++ x

  where
    {-# NOINLINE run_ref #-}
    run_ref f xs () = f xs
    run_acc f xs () = f xs



