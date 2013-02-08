{-# LANGUAGE RankNTypes #-}

module Random.Array (

  randomArray, randomArrayWithSeed

) where

import Config

import Prelude                                  as P
import Control.Monad
import Control.Monad.ST
import System.Random.MWC
import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Array.Data         as A
import Data.Array.Accelerate.Array.Sugar        as Sugar


-- Random splattered array using the system's source of random numbers
--
randomArray :: DIM2 -> R -> IO (Matrix R)
randomArray sh r =
  withSystemRandom . asGenIO $ \gen -> do
    seed   <- save gen
    return $! randomArrayWithSeed sh r seed


-- Random splattered array using the given random seed
--
randomArrayWithSeed
    :: DIM2                     -- shape
    -> R                        -- outer radius
    -> Seed
    -> Matrix R
randomArrayWithSeed sh@(Z :. height :. width) radiusMax seed
  = let
        n               = width * height
        (adata,_)       = runArrayData $ do
          gen <- restore seed
          arr <- newArrayData n

          -- initialise array to zeros
          iter sh (\ix -> unsafeWriteArrayData arr (Sugar.toIndex sh ix) ((),0)) (>>) (return ())

          -- splatter with some random data
          let m' = (P.fromIntegral (width * height)) / (radiusMax * radiusMax)
              m  = P.round m' `P.max` 1 :: Int

              go i
                | i > m         = return ()
                | otherwise     = do
                    a   <- uniform gen
                    r   <- uniformR (1,radiusMax) gen
                    y   <- uniformR (0,height-1)  gen
                    x   <- uniformR (0,width-1)   gen
                    splat sh (Z:.y:.x) r a arr
                    go (i+1)

          go 0
          return (arr, undefined)

    in adata `seq` Array (fromElt sh) adata


-- Draw a filled circle of given radius and origin. The circle can add or remove
-- substance.
--
splat
    :: DIM2                             -- array size
    -> DIM2                             -- origin
    -> R                                -- radius
    -> Bool                             -- negative space?
    -> MutableArrayData s (EltRepr R)
    -> ST s ()
splat sh@(Z :. height :. width) (Z :. originY :. originX) radius negative adata
  = do
       let c = P.ceiling radius
           v = if negative then ((),0) else ((),1)

           write (Z:.y:.x) =
             let mx     = x - c
                 my     = y - c
                 x'     = mx + originX
                 y'     = my + originY
                 r      = sqrt (P.fromIntegral $ mx*mx + my*my)
             in
             when (x' >= 0 && y' >= 0 && x' < width && y' < height && r < radius)
              $ unsafeWriteArrayData adata (Sugar.toIndex sh (Z:.y':.x')) v

       iter (Z :. 2*c :. 2*c) write (>>) (return ())

