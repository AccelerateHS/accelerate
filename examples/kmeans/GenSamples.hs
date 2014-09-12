--
-- Adapted from K-Means sample from "Parallel and Concurrent Programming in
-- Haskell", (c) Simon Marlow, 2013.
--
-- ./generate-samples 5 50000 100000 1010
--

import Control.Monad
import Data.Array
import Data.Binary
import Data.List
import Data.Word
import Data.Random.Normal
import System.Environment
import System.IO
import System.Random


-- Points
-- ------

type Point = (Float, Float)

zeroPoint :: Point
zeroPoint = (0,0)


-- Clusters
-----------

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


-- Generate random points
-- ----------------------

minX, maxX, minY, maxY, minSD, maxSD :: Float
minX = -10
maxX = 10
minY = -10
maxY = 10
minSD = 1.5
maxSD = 2.0

main :: IO ()
main = do
    n: minp: maxp: rest <- fmap (fmap read) getArgs

    case rest of
        [seed] -> setStdGen (mkStdGen seed)
        _      -> return ()

    nps <- replicateM n (randomRIO (minp, maxp))
    xs  <- replicateM n (randomRIO (minX, maxX))
    ys  <- replicateM n (randomRIO (minY, maxY))
    sds <- replicateM n (randomRIO (minSD, maxSD))

    let params = zip5 nps xs ys sds sds

    -- first generate a set of points for each set of sample parameters
    ss <- mapM (\(a,b,c,d,e) -> generate2DSamples a b c d e) params
    let points = concat ss

    -- dump all the points into the file "points"
    hsamp <- openFile "points" WriteMode
    mapM_ (printPoint hsamp) points
    hClose hsamp

    encodeFile "points.bin" points

    -- generate the initial clusters by assigning each point to random
    -- cluster.
    gen <- newStdGen
    let
        rand_clusters = randomRs (0,n-1) gen :: [Int]
        arr = accumArray (flip (:)) [] (0,n-1) $
                zip rand_clusters points
        clusters = map (uncurry makeCluster) (assocs arr)
    writeFile "clusters" (show clusters)

    -- so we can tell what the answer should be:
    writeFile "params" (show params)


printPoint :: Handle -> Point -> IO ()
printPoint h (x,y) = do
  hPutStr h (show x)
  hPutChar h ' '
  hPutStr h (show y)
  hPutChar h '\n'

generate2DSamples
    :: Int                      -- number of samples to generate
    -> Float -> Float           -- X and Y of the mean
    -> Float -> Float           -- X and Y standard deviations
    -> IO [Point]
generate2DSamples n mx my sdx sdy = do
  gen <- newStdGen
  let (genx, geny) = split gen
      xsamples = normals' (mx,sdx) genx
      ysamples = normals' (my,sdy) geny
  return (zipWith (,) (take n xsamples) ysamples)

