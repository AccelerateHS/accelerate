{-# LANGUAGE ViewPatterns #-}

--
-- Generators for various distributions of particle positions
--
module Random.Position
  where

import Common.Type

import Control.Monad.ST                         ( ST )
import System.Random.MWC                        ( GenST, uniformR )
import Data.Array.Accelerate.Array.Sugar        as A


-- | Points distributed as a disc
--
disc :: Position -> R -> sh -> GenST s -> ST s Position
disc (originX, originY, originZ) radiusMax _ix gen
  = do  radius          <- uniformR (0,radiusMax) gen
        theta           <- uniformR (0, pi)       gen
        phi             <- uniformR (0, 2*pi)     gen

        return ( originX + radius * sin theta * cos phi
               , originY + radius * sin theta * sin phi
               , originZ + radius * cos theta )


-- | A point cloud with areas of high and low density
--
cloud :: Shape sh => (Int,Int) -> R -> sh -> GenST s -> ST s Position
cloud (fromIntegral -> sizeX, fromIntegral -> sizeY) radiusMax ix gen
  = let
        blob (sx,sy,sz) r
          = disc (sx * sizeX, sy * sizeY, sz * (sizeX `min` sizeY))
                 (radiusMax * r)

    in case A.size ix `mod` 5 of
        0 -> blob ( 0.25, 0.25, 0.25) 1.00 ix gen
        1 -> blob (-0.10, 0.10, 0.10) 0.60 ix gen
        2 -> blob (-0.05, 0.30,-0.30) 0.35 ix gen
        3 -> blob (-0.20,-0.12,-0.12) 0.45 ix gen
        _ -> blob ( 0.15,-0.10, 0.20) 0.75 ix gen

