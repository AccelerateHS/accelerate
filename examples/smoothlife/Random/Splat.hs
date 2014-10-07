{-# LANGUAGE TypeOperators #-}

module Random.Splat
  where

import Config

import Control.Monad
import System.Random.MWC
import Data.Array.Accelerate                            ( DIM2, Z(..), (:.)(..) )
import Data.Array.Accelerate.Examples.Internal          ( (:~>) )


-- Generate a random point on the grid by checking which circles it overlaps.
--
splat :: [(DIM2, R, Bool)] -> DIM2 :~> R
splat circles ix _ =
  let overlaps          = filter (inside ix) circles
      negative (_,_,s)  = s
  in
  return $ case overlaps of
                []  -> 0
                xs  -> if negative (last xs) then 0 else 1


-- Generate some random circles with centre position somewhere inside the grid
-- and some maximum radius. The boolean represents whether life should be
-- created or destroyed inside this region.
--
randomCircles :: DIM2 -> R -> R -> IO [(DIM2, R, Bool)]
randomCircles (Z :. height :. width) radiusMin radiusMax =
  withSystemRandom . asGenIO $ \gen ->
    let
        circle  = do
          a     <- uniform gen
          r     <- uniformR (radiusMin, radiusMax) gen
          y     <- uniformR (0, height-1) gen
          x     <- uniformR (0, width-1)  gen
          return $ (Z:.y:.x, r, a)

        n       = 1 `max` round ( fromIntegral (width * height) / (radiusMax * radiusMax) )
    in
    replicateM n circle


-- Test if a point is inside the given circle, given by centre coordinate and
-- radius.
--
inside :: DIM2 -> (DIM2, R, Bool) -> Bool
inside (Z:.y:.x) (Z:.cy:.cx, r, _) =
  let
      dx        = fromIntegral $ cx - x
      dy        = fromIntegral $ cy - y
  in
  dx*dx + dy*dy <= r*r

