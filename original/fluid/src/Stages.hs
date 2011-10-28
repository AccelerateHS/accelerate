-- Defines the fluid simulation stages, generalised for both
--  Density and Velocity Fields
{-# LANGUAGE BangPatterns #-}
module Stages
   ( addSources
   , diffusion
   , advection
   , project
   , insertSource
   ) where

import Data.Array.Repa as A
import Data.Array.Repa.Stencil
import System.IO.Unsafe

import Model
import FieldElt  as E
import Constants

{-# SPECIALIZE addSources :: Maybe (Source Float) -> Field Float 
                          -> Field Float #-}
{-# SPECIALIZE addSources :: Maybe (Source (Float, Float))
                          -> Field (Float, Float) 
                          -> Field (Float, Float) #-}
-- Addition of forces stage for simulation
addSources :: (FieldElt a) => Maybe (Source a) -> Field a -> Field a
addSources (Just (Source aim mul)) field@(Array _ [Region RangeAll GenManifest{}])
   = {-# SCC "addSources" #-}
     field `deepSeqArray` force $ 
      traverse field id (insertSource aim mul)
addSources Nothing field@(Array _ [Region RangeAll GenManifest{}])
   = field `deepSeqArray` field
addSources _ _
   = error "Non-manifest array passed to addSources"

--
-- PROJECTION
--
project :: Field (Float, Float) -> Field (Float, Float)
{-# INLINE project #-}
project f@(Array _ [Region RangeAll GenManifest{}])
   = {-# SCC "project" #-}
     f `deepSeqArray` div `deepSeqArray` p `deepSeqArray` f' `deepSeqArray`
     f'
   where
      f'@(Array _ [Region RangeAll GenManifest{}])  = force $ traverse f id (project' p)
      p@(Array _ [Region RangeAll GenManifest{}])   = force $ linearSolver div div 1 4 repeats
      div@(Array _ [Region RangeAll GenManifest{}]) = force $ fromFunction (Z:.widthI:.widthI) (genDiv f)

      repeats = 20
project _
   = error "Non-manifest array given to project"

-- Helper for project, used to subtract gradient field from regular field
--  to create mass conserving field
project' :: Field Float
         -> (DIM2 -> (Float, Float)) -> DIM2 -> (Float, Float)
{-# INLINE project' #-}
project' p@(Array _ [Region RangeAll GenManifest{}]) locate pos@(Z:.j:.i)
   = p `deepSeqArray` 
     (locate pos) ~-~ (0.5 * widthF * (p0 - p1),
                       0.5 * widthF * (p2 - p3))
   where
      p0 = useIf (i < widthI-1) (p ! (Z:.j  :.i+1))
      p1 = useIf (i >        0) (p ! (Z:.j  :.i-1))
      p2 = useIf (j < widthI-1) (p ! (Z:.j+1:.i  ))
      p3 = useIf (j >        0) (p ! (Z:.j-1:.i  ))
project' _ _ _ = error "Non-manifest array given to project'"

-- Used to create approximate for gradient field
genDiv :: VelocityField -> DIM2 -> Float
{-# INLINE genDiv #-}
genDiv f@(Array _ [Region RangeAll GenManifest{}]) (Z:.j:.i)
   = {-f `deepSeqArray`-} (-0.5 * ((u0 - u1) + (v0 - v1))) / widthF
   where
      (u0, _) = useIf (i < widthI-1) (f ! (Z:.j  :.i+1))
      (u1, _) = useIf (i >        0) (f ! (Z:.j  :.i-1))
      ( _,v0) = useIf (j < widthI-1) (f ! (Z:.j+1:.i  ))
      ( _,v1) = useIf (j >        0) (f ! (Z:.j-1:.i  ))
genDiv _ _ = error "Non-manifest array given to genDiv"

--
-- ADVECTION
--

{-# SPECIALIZE advection :: VelocityField -> Field Float -> Field Float #-}
{-# SPECIALIZE advection :: VelocityField -> Field (Float, Float) -> Field (Float, Float) #-}

advection :: (FieldElt a) => VelocityField -> Field a -> Field a
advection vf@(Array _ [Region RangeAll GenManifest{}]) f@(Array _ [Region RangeAll GenManifest{}])
 = {-# SCC "advection" #-}
 vf `deepSeqArray` f `deepSeqArray` f' `deepSeqArray` f'
 where
   f'@(Array _ [Region RangeAll GenManifest{}]) = force $ traverse f id (advection' vf f)
advection _ _ = error "Non-manifest array given to advection"

{-# SPECIALIZE advection' :: VelocityField -> Field Float -> (DIM2 -> Float) -> DIM2 -> Float #-}
{-# SPECIALIZE advection' :: VelocityField -> Field (Float, Float) -> (DIM2 -> (Float,Float)) -> DIM2 -> (Float,Float) #-}
-- Helper to calculate density to be placed at each point
advection' :: (FieldElt a) => VelocityField -> Field a
           -> (DIM2 -> a) -> DIM2 -> a
--{-# INLINE advection' #-}
advection' vf@(Array _ [Region RangeAll GenManifest{}]) orig@(Array _ [Region RangeAll GenManifest{}]) locate pos@(Z:.j:.i)
 = vf `deepSeqArray` orig `deepSeqArray`
     (((d00 ~*~ t0) ~+~ (d01 ~*~ t1)) ~*~ s0) ~+~
     (((d10 ~*~ t0) ~+~ (d11 ~*~ t1)) ~*~ s1)
   where
      -- backtrack densities to point based on velocity field
      --  and make sure they are in field
      x = checkLocation $ ((fromIntegral i) - dt0 * u)
      y = checkLocation $ ((fromIntegral j) - dt0 * v)

      -- calculate discrete locations surrounding point
      i0 = truncate x
      i1 = i0 + 1

      j0 = truncate y
      j1 = j0 + 1

      -- calculate ratio point is between the discrete locations
      s1 = x - (fromIntegral i0)
      s0 = 1 - s1

      t1 = y - (fromIntegral j0)
      t0 = 1 - t1

      -- grab values from grid surrounding advected point
      d00 = orig ! (Z:.j0:.i0)
      d01 = orig ! (Z:.j1:.i0)
      d10 = orig ! (Z:.j0:.i1)
      d11 = orig ! (Z:.j1:.i1)

      -- helper values
      dt0    = dt * widthF
      (u, v) = vf ! pos
advection' _ _ _ _ = error "Non-manifest array given to advection'"

-- Safety check that we are within the simulation area
checkLocation :: Float -> Float
{-# INLINE checkLocation #-}
checkLocation x
   | x < 0.5          = 0.5
   | x > widthF - 1.5 = widthF - 1.5
   | otherwise        = x

--
-- DIFFUSION
--

{-# SPECIALIZE diffusion :: Field Float -> Float -> Field Float #-}
{-# SPECIALIZE diffusion :: Field (Float, Float) -> Float -> Field (Float, Float) #-}

diffusion :: (FieldElt a) => Field a -> Float -> Field a
diffusion f@(Array _ [Region RangeAll GenManifest{}]) !rate = {-# SCC "diffusion" #-}
            f' `deepSeqArray` f'
            where
               a       = dt * rate * widthF * widthF
               c       = 1 + 4 * a
               repeats = 20
               f'@(Array _ [Region RangeAll GenManifest{}])
                       = f `deepSeqArray`force $  linearSolver f f a c repeats
diffusion _ _ = error "Non-manifest array given to diffusion"

--
-- LINEAR SOLVER
--

{-# SPECIALIZE linearSolver :: Field Float -> Field Float -> Float -> Float -> Int -> Field Float #-}
{-# SPECIALIZE linearSolver :: Field (Float, Float) -> Field (Float, Float) -> Float -> Float -> Int -> Field (Float, Float) #-}

linearSolver :: (FieldElt a)
             => Field a
             -> Field a
             -> Float
             -> Float
             -> Int
             -> Field a
-- If linearSolver would not actually change anything by running, skip
linearSolver origF@(Array _ [Region RangeAll GenManifest{}]) _  !0  !_  !_
   = origF
-- If linearSolver has finished loop
linearSolver _     f@(Array _ [Region RangeAll GenManifest{}])  !_  !_  !0
   = f
-- Calculate intermediate array
linearSolver origF@(Array _ [Region RangeAll GenManifest{}]) f@(Array _ [Region RangeAll GenManifest{}]) !a !c !i
   = {-# SCC "linearSolver" #-}
   f `deepSeqArray` origF `deepSeqArray` f' `deepSeqArray` f'' `deepSeqArray`
   linearSolver origF f'' a c (i-1)
   where
      f''@(Array _ [Region RangeAll GenManifest{}]) = c' `seq` force $ A.zipWith (zipFunc c') origF f'
      f'@(Array _ [Region RangeAll GenManifest{}])  = force $ mapStencil2 (BoundConst E.zero) (linearSolverStencil a c) (force f)
      c'  = 1/c

linearSolver _ _ _ _ _ = error "Non-manifest array in linearSolver"

{-# SPECIALIZE zipFunc :: Float -> Float -> Float -> Float #-}
{-# SPECIALIZE zipFunc :: Float -> (Float,Float) -> (Float,Float) -> (Float,Float) #-}
-- Function is specialised, rather than inlined, as GHC would not inline the function
--  otherwise
zipFunc :: (FieldElt a) => Float -> a -> a -> a
zipFunc !c !orig !new = new ~+~ (orig ~*~ c)

linearSolverStencil :: (FieldElt a) => Float -> Float -> Stencil DIM2 a
{-# INLINE linearSolverStencil #-}
linearSolverStencil a c =
   StencilStatic (Z:.3:.3) E.zero $
      (\ix val acc ->
         case linearSolverCoeffs a c ix of
            Nothing -> acc
            Just coeff -> acc ~+~ (val ~*~ coeff))
         
linearSolverCoeffs :: Float -> Float
                   -> DIM2  -> Maybe Float
{-# INLINE linearSolverCoeffs #-}
linearSolverCoeffs a c (Z:.j:.i)
   | i ==  1, j ==  0 = Just (a/c)
   | i == -1, j ==  0 = Just (a/c)
   | i ==  0, j ==  1 = Just (a/c)
   | i ==  0, j == -1 = Just (a/c)
   | otherwise        = Nothing

-- Helper function for inserting a source
insertSource :: (FieldElt a) => DIM2 -> a -> (DIM2 -> a) -> DIM2 -> a
{-# INLINE insertSource #-}
insertSource !aim !mul locate !pos
   | aim == pos = addSource (locate pos) mul
   | otherwise  = locate pos
