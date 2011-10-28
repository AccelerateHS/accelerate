module Velocity
   ( velocitySteps )
   where

import Data.Array.Repa

import Model
import Constants
import Stages
import FieldElt

velocitySteps :: VelocityField -> Maybe (Source (Float, Float)) -> VelocityField
velocitySteps vf@(Array _ [Region RangeAll GenManifest{}]) vs = vf'
   where
      vf'@(Array _ [Region RangeAll GenManifest{}]) = vf8 `deepSeqArray` (setBoundary vf8)
      vf8@(Array _ [Region RangeAll GenManifest{}]) = vf7 `deepSeqArray` (project vf7)
      vf7@(Array _ [Region RangeAll GenManifest{}]) = vf6 `deepSeqArray` (setBoundary vf6)
      vf6@(Array _ [Region RangeAll GenManifest{}]) = vf5 `deepSeqArray` (advection vf5 vf5)
      vf5@(Array _ [Region RangeAll GenManifest{}]) = vf4 `deepSeqArray` (setBoundary vf4)
      vf4@(Array _ [Region RangeAll GenManifest{}]) = vf3 `deepSeqArray` (project vf3)
      vf3@(Array _ [Region RangeAll GenManifest{}]) = vf2 `deepSeqArray` (setBoundary vf2)
      vf2@(Array _ [Region RangeAll GenManifest{}]) = vf1 `deepSeqArray` (diffusion vf1 visc)
      vf1@(Array _ [Region RangeAll GenManifest{}]) = vf `deepSeqArray` addSources vs vf
velocitySteps _ _ = error "Non-manifest array given to velocitySteps"

setBoundary :: VelocityField -> VelocityField
setBoundary f@(Array _ [Region RangeAll GenManifest{}])
 = force $
   rebuild f $    -- Rebuilds the VelocityField, grabbing new values for
                  -- edges
   setBoundary' $ -- Modifies values of border elements appropriately
   grabBorders f  -- Grabs border elements and puts them into array

-- Takes the original VelocityField and the array of edges and replaces
-- edge values with new values
rebuild :: VelocityField -> VelocityField -> VelocityField
{-# INLINE rebuild #-}
rebuild f@(Array _ [Region RangeAll GenManifest{}]) e@(Array _ [Region RangeAll GenManifest{}])
 = f `deepSeqArray` e `deepSeqArray` backpermuteDft f rebuildPosMap e
rebuild _ _ = error "Non-manifest array given to rebuild"

rebuildPosMap :: DIM2 -> Maybe DIM2
{-# INLINE rebuildPosMap #-}
rebuildPosMap (Z:.j:.i)
   | j == 0          = Just (Z:.0:.i)
   | j == widthI - 1 = Just (Z:.1:.i)
   | i == 0          = if j == 0 then
                        Just (Z:.0:.0)
                       else if j == end then
                        Just (Z:.1:.0)
                       else Just (Z:.2:.j)
   | i == widthI - 1 = if j == 0 then
                        Just (Z:.0:.(widthI-1))
                       else if j == end then
                        Just (Z:.1:.(widthI-1))
                       else Just (Z:.3:.j)
   | otherwise       = Nothing
   where
      end = widthI - 1

setBoundary' :: VelocityField -> VelocityField
{-# INLINE setBoundary' #-}
setBoundary' e@(Array _ [Region RangeAll GenManifest{}])
 = e `deepSeqArray` force $ traverse e id revBoundary
setBoundary' _ = error "Non-manifest array given to setBoundary'"

-- Based on position in edges array set the velocity accordingly
revBoundary :: (DIM2 -> (Float,Float)) -> DIM2 -> (Float,Float)
{-# INLINE revBoundary #-}
revBoundary loc pos@(Z:.j:.i)
   | j == 0    = if i == 0 then
                  grabCornerCase loc (Z:.2:.1) (Z:.0:.1)
                  else if i == end then
                     grabCornerCase loc (Z:.0:.(widthI-2)) (Z:.3:.1)
                  else (-p1,p2)
   | j == 1    = if i == 0 then
                  grabCornerCase loc (Z:.2:.(widthI-2)) (Z:.1:.1)
                 else if i == end then
                  grabCornerCase loc (Z:.1:.(widthI-2)) (Z:.3:.(widthI-2))
                 else (-p1,p2)
   | j == 2    = (p1,-p2)
   | j == 3    = (p1,-p2)
   | otherwise = error "Incorrect position given to revBoundary"
   where
      (p1,p2) = loc pos
      end     = widthI - 1

-- Corner cases are special and are calculated with this function
grabCornerCase :: (DIM2 -> (Float, Float)) -> DIM2 -> DIM2 -> (Float, Float)
{-# INLINE grabCornerCase #-}
grabCornerCase loc pos1 pos2
 = (p1*q1,p2*q2) ~*~ 0.5
 where
   (p1,p2) = loc pos1
   (q1,q2) = loc pos2

-- Grabs the border elements of the VelocityField and outputs them as
-- one array, for ease of adding back into the original VelocityField later
grabBorders :: VelocityField -> VelocityField
{-# INLINE grabBorders #-}
grabBorders f@(Array _ [Region RangeAll GenManifest{}])
 = f `deepSeqArray` force $ backpermute (Z:.4:.widthI) edgeCases f
grabBorders _
 = error "Non-manifest array given to grabBorders"

-- Maps a position in the edges array to what they were in the original
-- array
edgeCases :: DIM2 -> DIM2
{-# INLINE edgeCases #-}
edgeCases (Z:.j:.i)
   | j == 0    = (Z:.0         :.i)
   | j == 1    = (Z:.(widthI-1):.i)
   | j == 2    = (Z:.i         :.0)
   | j == 3    = (Z:.i         :.(widthI-1))
   | otherwise = error "Incorrect coordinate given in setBoundary"
