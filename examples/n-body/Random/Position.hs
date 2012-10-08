
--
-- Generators for various distributions of particle positions
--
module Random.Position
  where

import Common.Type

import Control.Monad.ST                         ( ST )
import System.Random.MWC                        ( GenST, uniformR )
import Data.Array.Accelerate.Array.Sugar        as A


-- | A uniform distribution of points between some minimum and maximum bounds
--
uniform :: R -> R -> sh -> GenST s -> ST s Position
uniform pointMin pointMax _ix
  = uniformR ((pointMin, pointMin), (pointMax, pointMax))


-- | Points distributed as a disc
--
disc :: Position -> R -> sh -> GenST s -> ST s Position
disc (originX, originY) radiusMax _ix gen
  = do  radius  <- uniformR (0,radiusMax) gen
        theta   <- uniformR (-pi, pi)     gen

        return ( originX + radius * cos theta
               , originY + radius * sin theta )


-- | A point cloud with areas of high and low density
--
cloud :: Shape sh => sh -> GenST s -> ST s Position
cloud ix gen
  = case A.size ix `mod` 5 of
      0 -> disc (250,250) 200 ix gen
      1 -> disc (100,100) 80  ix gen
      2 -> disc (150,300) 30  ix gen
      3 -> disc (500,120) 30  ix gen
      _ -> disc (300,200) 150 ix gen

