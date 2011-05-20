{-# LANGUAGE FlexibleContexts #-}

module Stencil2 where

import Control.Monad
import Control.Exception
import System.Random.MWC
import Data.Array.Unboxed           hiding (Array)
import Data.Array.Accelerate        hiding (round, min, max, fromIntegral)
import qualified Data.Array.IArray  as IArray



stencil2D2 :: Floating (Exp a) => Stencil3x3 a -> Stencil3x3 a -> Exp a
stencil2D2 ((_,t,_), (_,x,_), (_,b,_))
           ((_,_,_), (l,y,r), (_,_,_)) = t + b + l + r - ((x+y) / 2)


test_stencil2_2D :: Int -> IO (() -> UArray (Int,Int) Float, () -> Acc (Array DIM2 Float))
test_stencil2_2D n2 = withSystemRandom $ \gen -> do
  let n = round $ sqrt (fromIntegral n2 :: Double)
      m = n * 2
      u = m `div` 3
      v = n + m
  m1  <- listArray ((0,0),(n-1,m-1)) `fmap` replicateM (n*m) (uniformR (-1,1) gen) :: IO (UArray (Int,Int) Float)
  m2  <- listArray ((0,0),(u-1,v-1)) `fmap` replicateM (u*v) (uniformR (-1,1) gen) :: IO (UArray (Int,Int) Float)
  m1' <- let m1' = fromIArray m1 in evaluate (m1' `indexArray` (Z:.0:.0)) >> return m1'
  m2' <- let m2' = fromIArray m2 in evaluate (m2' `indexArray` (Z:.0:.0)) >> return m2'
  --
  return (\() -> run_ref m1 m2, \() -> run_acc m1' m2')
  where
    run_acc xs ys = stencil2 stencil2D2 Mirror (use xs) Wrap (use ys)
    run_ref xs ys =
      let (_,(n,m)) = bounds xs
          (_,(u,v)) = bounds ys
          sh        = ((0,0), (n `min` u, m `min` v))

          -- boundary conditions are placed on the *source* arrays
          --
          get1 (x,y) = xs IArray.! (mirror n x, mirror m y)
          get2 (x,y) = ys IArray.! (wrap   u x, wrap   v y)

          mirror sz i
            | i < 0     = -i
            | i > sz    = sz - (i-sz)
            | otherwise = i

          wrap sz i
            | i < 0     = sz + i + 1
            | i > sz    = i - sz - 1
            | otherwise = i

          f (ix,iy) = let t = get1 (ix,  iy-1)
                          b = get1 (ix,  iy+1)
                          x = get1 (ix,  iy)
                          l = get2 (ix-1,iy)
                          r = get2 (ix+1,iy)
                          y = get2 (ix,  iy)
                    in
                    t + b + l + r - ((x+y) / 2)
      in
      array sh [(ix, f ix) | ix <- range sh]

-- Main
-- ----

run2D :: String -> Int -> IO (() -> UArray (Int,Int) Float, () -> Acc (Array DIM2 Float))
run2D "2D" = test_stencil2_2D
run2D x    = error $ "unknown variant: " ++ x

