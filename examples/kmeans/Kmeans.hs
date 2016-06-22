{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Math.Kmeans
-- Copyright   : [2014] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- An implementation of K-means clustering in Accelerate using Lloyd's
-- algorithm. This heuristic technique finds a solution by iteratively improving
-- an initial guess. The algorithm takes as input the number of clusters to find
-- and makes an initial guess at the center of each cluster. Then proceeds as
-- follows:
--
--   1. Assign each point to the cluster to which it is closest. This forms the
--   new set of clusters.
--
--   2. Compute the centroid of the cluster (the average of all points in the
--   cluster.
--
--   3. Repeat until the centroid locations stabilise (or some maximum iteration
--   limit is reached).
--
-- Adapted from K-Means sample from "Parallel and Concurrent Programming in
-- Haskell", (c) Simon Marlow, 2013.
--

module Kmeans ( Point, Cluster, Id, kmeans )
  where

import Prelude                                          as P
import Data.Array.Accelerate                            as A


-- This implementation works on 2D points. In future, generalise this to some
-- arbitrary "feature vector".
--
type Point a = (a, a)

-- Clusters consist of the centroid location as well as its identifier
--
type Id = Word32
type Cluster a = (Id, (a, a))

idOfCluster :: Elt a => Exp (Cluster a) -> Exp Id
idOfCluster = A.fst

centroidOfCluster :: Elt a => Exp (Cluster a) -> Exp (Point a)
centroidOfCluster = A.snd

-- We'll use this as an intermediate structure; it contains the number of points
-- in the set as well as the sum of the x and y coordinates respectively.
--
type PointSum a = (Word32, (a, a))


-- Get the distance (squared) between two points. Since we only compare this
-- value, we elide the square root.
--
distance :: A.Num a => Exp (Point a) -> Exp (Point a) -> Exp a
distance u v =
  let (x1,y1) = unlift u
      (x2,y2) = unlift v
  in
  (x1-x2)^(2::Int) + (y1-y2)^(2::Int)


-- For each of the given points, return the cluster Id that that that point is
-- closest to.
--
findClosestCluster
    :: forall a. (A.RealFloat a, P.RealFloat a)
    => Acc (Vector (Cluster a))
    -> Acc (Vector (Point a))
    -> Acc (Vector Id)
findClosestCluster clusters points =
  A.map (\p -> A.fst $ A.sfoldl (nearest p) z (constant Z) clusters) points
  where
    z = constant (-1, inf)

    nearest :: Exp (Point a) -> Exp (Id, a) -> Exp (Cluster a) -> Exp (Id, a)
    nearest p st c =
      let d  = A.snd st
          d' = distance p (centroidOfCluster c)
      in
      d' A.<* d ? ( lift (idOfCluster c, d') , st )


-- Given a vector of points and a vector of clusters we, we first locate the
-- closest cluster to each point, assign that point to their closest cluster,
-- and compute the centroid of the cluster. This yields the new centroid
-- locations.
--
makeNewClusters
    :: forall a. (A.RealFloat a, P.RealFloat a, A.FromIntegral Word32 a)
    => Acc (Vector (Point a))
    -> Acc (Vector (Cluster a))
    -> Acc (Vector (Cluster a))
makeNewClusters points clusters
  = pointSumToCluster
  . makePointSum
  . findClosestCluster clusters
  $ points

-- TLM: This setup might be quicker, because it forces the result of
--      findClosestCluster to be evaluated, which overall reduces memory
--      traffic. However, this is hitting a sharing recovery bug in Accelerate
--      so can't be used right now ):
--
--      As per tip in issue #148, we can get around the bug by making the first
--      argument to pipe closed. It turns out that the first version is quicker!
--
--  = A.uncurry findClosestCluster >-> pointSumToCluster . makePointSum $ A.lift (clusters, points)
  where
    npts        = size points
    nclusters   = size clusters

    -- Turn the PointSum intermediate structure into the clusters, by averaging
    -- the cumulative (x,y) positions.
    --
    pointSumToCluster :: Acc (Vector (PointSum a)) -> Acc (Vector (Cluster a))
    pointSumToCluster ps =
      A.generate (A.shape ps)
                 (\ix -> lift (A.fromIntegral (unindex1 ix), average (ps ! ix)))

    average :: Exp (PointSum a) -> Exp (Point a)
    average ps =
      let (n, xy) = unlift ps   :: (Exp Word32, Exp (Point a))
          (x, y)  = unlift xy
      in
      lift (x / A.fromIntegral n, y / A.fromIntegral n) -- TLM: what if there are no points in the cluster??

    -- Reduce along the rows of 'pointSum' to get the cumulative (x,y) position
    -- and number of points assigned to each centroid.
    --
    makePointSum :: Acc (Vector Id) -> Acc (Vector (PointSum a))
    makePointSum = A.fold1 addPointSum . compute . pointSum

    -- The point sum is an intermediate 2D array (it gets fused away, so does
    -- not exist in memory). The points are laid out along the innermost
    -- dimension (rows), and down the column is each of the clusters.
    --
    -- For each point, we put its (x,y) coordinates into the row corresponding
    -- to whichever cluster it is closest to, and zeros in each of the other
    -- rows.
    --
    pointSum :: Acc (Vector Id) -> Acc (Array DIM2 (PointSum a))
    pointSum nearest =
      A.generate (lift (Z:.nclusters:.npts))
                 (\ix -> let Z:.i:.j = unlift ix    :: Z :. Exp Int :. Exp Int
                             near    = nearest ! index1 j

                             yes     = lift (constant 1, points ! index1 j)
                             no      = constant (0, (0, 0))
                         in
                         near ==* A.fromIntegral i ? ( yes, no ))

    addPointSum :: Exp (PointSum a) -> Exp (PointSum a) -> Exp (PointSum a)
    addPointSum x y =
      let (c1, u) = unlift x    :: (Exp Word32, Exp (Point a))
          (c2, v) = unlift y    :: (Exp Word32, Exp (Point a))
          (x1,y1) = unlift u    :: (Exp a, Exp a)
          (x2,y2) = unlift v    :: (Exp a, Exp a)
      in
      lift (c1+c2, lift (x1+x2, y1+y2) :: Exp (Point a))

{--
    -- Alternative to computing the PointSum structure.
    --
    -- This method uses a forward permutation with atomic instructions to create
    -- the array directly (this method is closer to what one might write
    -- sequentially). This avoids a parallel reduction, but has very high
    -- contention. Overall performance much lower, as:
    --
    --   number of clusters << number of points
    --
    makePointSum :: Acc (Vector (PointSum a))
    makePointSum = A.permute addPointSum zeros near input
      where
        zeros   = A.fill (constant (Z:.nclusters)) (constant (0,(0,0)))
        input   = A.zip (A.fill (A.shape points) (constant 1)) points
        near ix = index1 (A.fromIntegral (nearest ! ix))
--}


-- To complete the k-means algorithm, we loop repeatedly generating new clusters
-- positions, until the positions converge (or some maximum iteration limit is
-- reached?)
--
kmeans :: forall a. (A.RealFloat a, P.RealFloat a, A.FromIntegral Word32 a)
       => Acc (Vector (Point a))        -- the points to cluster
       -> Acc (Vector (Cluster a))      -- initial cluster positions (guess)
       -> Acc (Vector (Cluster a))
kmeans points clusters
  = A.asnd
  $ A.awhile (A.uncurry keepGoing)
             (\cs -> let (_, old) = unlift cs   :: (Acc (Vector (Cluster a)), Acc (Vector (Cluster a)))
                         new      = makeNewClusters points old
                     in
                     lift (old,new))
             (lift (clusters, makeNewClusters points clusters))
  where
    keepGoing :: Acc (Vector (Cluster a)) -> Acc (Vector (Cluster a)) -> Acc (Scalar Bool)
    keepGoing xs ys
      = A.or
      $ A.zipWith (\c1 c2 -> let (x1,y1) = unlift (centroidOfCluster c1)
                                 (x2,y2) = unlift (centroidOfCluster c2)
                             in
                             abs (x1-x2) >* 0.01 ||* abs (y1-y2) >* 0.01) xs ys


-- The largest non-infinite floating point number
--
inf :: forall a. P.RealFloat a => a
inf = P.encodeFloat m n
  where
    a           = undefined :: a
    b           = P.floatRadix a
    e           = P.floatDigits a
    (_, e')     = P.floatRange a
    m           = b ^ e - 1
    n           = e' - e

