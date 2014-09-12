--
-- Adapted from K-Means sample from "Parallel and Concurrent Programming in
-- Haskell", (c) Simon Marlow, 2013.
--

module TestCore where

import Data.List
import Data.Word

-- -----------------------------------------------------------------------------
-- Points

type Point = (Double, Double)

zeroPoint :: Point
zeroPoint = (0,0)


-----------------------------------------------------------------------------
-- Clusters

type Cluster = (Word32, Point)

makeCluster :: Int -> [Point] -> Cluster
makeCluster clid points
  = ( fromIntegral clid
    , (a / fromIntegral count, b / fromIntegral count))
  where
    (a,b) = foldl' addPoint zeroPoint points
    count = length points

    addPoint :: Point -> Point -> Point
    addPoint (x,y) (u,v) = (x+u,y+v)


