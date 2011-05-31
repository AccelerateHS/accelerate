{-# LANGUAGE FlexibleContexts #-}

module Stencil where

import Random

import Control.Monad
import Control.Exception
import System.Random.MWC

import Data.Array.Unboxed hiding (Array)
import qualified Data.Array.IArray as IArray

import Data.Array.Accelerate hiding (min, max, round, fromIntegral)
import qualified Data.Array.Accelerate as Acc



-- Stencil Tests
-- -------------

-- 1D --------------------------------------------------------------------------

stencil1D :: Floating a
          => (a, a, a) -> a
stencil1D (x, y, z) = (x + z - 2 * y) / 2

test_stencil1D :: Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
test_stencil1D n = withSystemRandom $ \gen -> do
  vec  <- randomUArrayR (-1,1) gen n
  vec' <- convertUArray vec
  return (\() -> run_ref vec, \() -> run_acc vec')
  where
    run_acc   = stencil stencil1D Clamp . use
    run_ref v =
      let (minx,maxx) = bounds v
          clamp x     = minx `max` x `min` maxx

          f ix = let x = v IArray.! clamp (ix-1)
                     y = v IArray.! ix
                     z = v IArray.! clamp (ix+1)
                 in
                 (x + z - 2 * y) / 2
      in
      array (bounds v) [(ix, f ix) | ix <- indices v]


-- 2D --------------------------------------------------------------------------

stencil2D :: Floating (Exp a)
          => Stencil3x3 a -> Exp a
stencil2D ( (t1, t2, t3)
          , (l , m,  r )
          , (b1, b2, b3)
          )
          = (t1/2 + t2 + t3/2 + l + r + b1/2 + b2 + b3/2 - 4 * m) / 4

test_stencil2D :: Int -> IO (() -> IArray.Array (Int,Int) Float, () -> Acc (Array DIM2 Float))
test_stencil2D n2 = withSystemRandom $ \gen -> do
  let n = round . (/3) . sqrt $ (fromIntegral n2 :: Double)
      m = n * 4
  mat  <- listArray ((0,0),(n-1,m-1)) `fmap` replicateM (n*m) (uniformR (-1,1) gen) :: IO (IArray.Array (Int,Int) Float)
  mat' <- let v = fromIArray mat                                                    :: Array DIM2 Float
          in  evaluate (v `indexArray` (Z:.0:.0)) >> return v
  --
  return (\() -> run_ref mat, \() -> run_acc mat')
  where
    run_acc     = stencil stencil2D (Constant 0) . use
    run_ref arr =
      let get ix
            | inRange (bounds arr) ix = arr IArray.! ix
            | otherwise               = 0

          f (x,y) = let t1 = get (x-1,y-1)
                        t2 = get (x,  y-1)
                        t3 = get (x+1,y-1)
                        l  = get (x-1,y)
                        m  = get (x,  y)
                        r  = get (x+1,y)
                        b1 = get (x-1,y+1)
                        b2 = get (x,  y+1)
                        b3 = get (x+1,y+1)
                    in
                    (t1/2 + t2 + t3/2 + l + r + b1/2 + b2 + b3/2 - 4 * m) / 4
      in
      array (bounds arr) [(ix, f ix) | ix <- indices arr]



stencil2D5 :: Floating (Exp a)
           => Stencil3x3 a -> Exp a
stencil2D5 ( (_, t, _)
           , (l, m, r)
           , (_, b, _)
           )
           = (t + l + r + b - 4 * m) / 4

test_stencil2D5 :: Int -> IO (() -> IArray.Array (Int,Int) Float, () -> Acc (Array DIM2 Float))
test_stencil2D5 n2 = withSystemRandom $ \gen -> do
  let n = round . sqrt $ (fromIntegral n2 :: Double)
  mat  <- listArray ((0,0),(n-1,n-1)) `fmap` replicateM (n*n) (uniformR (-1,1) gen) :: IO (IArray.Array (Int,Int) Float)
  mat' <- let m = fromIArray mat                                                    :: Array DIM2 Float
          in  evaluate (m `indexArray` (Z:.0:.0)) >> return m
  --
  return (\() -> run_ref mat, \() -> run_acc mat')
  where
    run_acc     = stencil stencil2D5 Clamp . use
    run_ref arr =
      let ((minx,miny),(maxx,maxy)) = bounds arr
          clamp (x,y) = (minx `max` x `min` maxx
                        ,miny `max` y `min` maxy)
          f (x,y)     = let t = arr IArray.! clamp (x,y-1)
                            b = arr IArray.! clamp (x,y+1)
                            l = arr IArray.! clamp (x-1,y)
                            r = arr IArray.! clamp (x+1,y)
                            m = arr IArray.! (x,y)
                        in
                        (t + l + r + b - 4 * m) / 4
      in
      array (bounds arr) [(ix, f ix) | ix <- indices arr]



stencil2Dpair :: Stencil3x3 (Int,Float) -> Exp Float
stencil2Dpair ( (_, _, _)
              , (x, _, _)
              , (y, _, z)
              )
  = let (x1,x2) = unlift x :: (Exp Int, Exp Float)
        (y1,y2) = unlift y
        (z1,z2) = unlift z
    in
    (x2 * Acc.fromIntegral x1 + y2 * Acc.fromIntegral z1 - z2 * Acc.fromIntegral y1)

test_stencil2Dpair :: Int -> IO (() -> IArray.Array (Int,Int) Float, () -> Acc (Array DIM2 Float))
test_stencil2Dpair n2 = withSystemRandom $ \gen -> do
  let n = round (fromIntegral n2 ** 0.5 :: Double)
      m = 2 * n
  mat  <- listArray ((0,0),(n-1,m-1)) `fmap` replicateM (n*m) (uniformR ((-100,0), (100,0)) gen) :: IO (IArray.Array (Int,Int) (Int,Float))
  mat' <- let a = fromIArray mat
          in  evaluate (a `indexArray` (Z:.0:.0)) >> return a
  --
  return (\() -> run_ref mat, \() -> run_acc mat')
  where
    run_acc     = stencil stencil2Dpair Wrap . use
    run_ref arr =
      let ((minx,miny),(maxx,maxy)) = bounds arr
          wrap (m,n) i
            | i < m     = n + (i-m)
            | i > n     = i - n + m
            | otherwise = i

          get (x,y) = arr IArray.! ( wrap (minx,maxx) x, wrap (miny,maxy) y)
          f   (x,y) = let (a1,a2) = get (x-1,y)
                          (b1,b2) = get (x-1,y+1)
                          (c1,c2) = get (x+1,y+1)
                      in
                      (a2 * fromIntegral a1 + b2 * fromIntegral c1 - c2 * fromIntegral b1)
      in
      array (bounds arr) [(ix, f ix) | ix <- indices arr]


-- 3D --------------------------------------------------------------------------

stencil3D :: Num (Exp a)
          => Stencil3x3x3 a -> Exp a
stencil3D (front, back, _) =      -- 'b4' is the focal point
  let ((f1, f2, _),
       (f3, f4, _),
       _          ) = front
      ((b1, b2, _),
       (b3, b4, _),
       _          ) = back
  in
  f1 + f2 + f3 + f4 + b1 + b2 + b3 + b4

test_stencil3D :: Int -> IO (() -> UArray (Int,Int,Int) Float, () -> Acc (Array DIM3 Float))
test_stencil3D n3 = withSystemRandom $ \gen -> do
  let u = round (fromIntegral n3 ** (1/3) :: Double)
      v = u `div` 2
      w = u * 3
  arr  <- listArray ((0,0,0), (u-1, v-1, w-1)) `fmap` replicateM (u*v*w) (uniformR (-1,1) gen) :: IO (UArray (Int,Int,Int) Float)
  arr' <- let a = fromIArray arr
          in  evaluate (a `indexArray` (Z:.0:.0:.0)) >> return a
  --
  return (\() -> run_ref arr, \() -> run_acc arr')
  where
    run_acc     = stencil stencil3D Mirror . use
    run_ref arr =
      let ((minx,miny,minz),(maxx,maxy,maxz)) = bounds arr
          mirror (m,n) i
            | i < m     = -i + m
            | i > n     = n - (i-n+2)
            | otherwise = i

          get (x,y,z) = arr IArray.! ( mirror (minx,maxx) x, mirror (miny,maxy) y, mirror (minz,maxz) z)
          f   (x,y,z) = let f1 = get (x-1,y-1,z-1)
                            f2 = get (x,  y-1,z-1)
                            f3 = get (x-1,y,  z-1)
                            f4 = get (x,  y,  z-1)
                            b1 = get (x-1,y-1,z  )
                            b2 = get (x,  y-1,z  )
                            b3 = get (x-1,y,  z  )
                            b4 = get (x,  y,  z  )
                        in
                        f1 + f2 + f3 + f4 + b1 + b2 + b3 + b4
      in
      array (bounds arr) [(ix, f ix) | ix <- indices arr]


-- Main
-- ----

run :: String -> Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
run "1D" = test_stencil1D
run x    = error $ "unknown variant: " ++ x


run2D :: String -> Int -> IO (() -> IArray.Array (Int,Int) Float, () -> Acc (Array DIM2 Float))
run2D "2D"        = test_stencil2D
run2D "3x3-cross" = test_stencil2D5
run2D "3x3-pair"  = test_stencil2Dpair
run2D x    = error $ "unknown variant: " ++ x


run3D :: String -> Int -> IO (() -> UArray (Int,Int,Int) Float, () -> Acc (Array DIM3 Float))
run3D "3D" = test_stencil3D
run3D x    = error $ "unknown variant: " ++ x

