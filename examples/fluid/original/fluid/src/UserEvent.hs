module UserEvent
   ( userEvent )
   where

import Data.Array.Repa as A
import Graphics.Gloss.Interface.Game as G

import Constants
import Model as M

-- Handler for user events from gameInWindow
userEvent :: Event -> Model -> Model
userEvent (EventKey key keyState mods (x, y)) m@(Model df ds vf vs cl sp cb)
   | MouseButton G.LeftButton <- key
   , Down                     <- keyState
   = let (x',y') = windowToModel (x,y) in
      Model
      { densityField   = df
      , densitySource  = Just (Source (Z:.y':.x') 1)
      , velocityField  = vf
      , velocitySource = vs
      , clickLoc       = cl
      , stepsPassed    = sp
      , currButton     = M.LeftButton
      }

   | MouseButton G.LeftButton <- key
   , Up                       <- keyState
   = Model
      { densityField   = df
      , densitySource  = ds
      , velocityField  = vf
      , velocitySource = vs
      , clickLoc       = cl
      , stepsPassed    = sp
      , currButton     = M.None
      }

   | MouseButton G.RightButton <- key
   , Down                      <- keyState
   = let (x',y') = windowToModel (x,y) in
      Model
      { densityField   = df
      , densitySource  = ds
      , velocityField  = vf
      , velocitySource = vs
      , clickLoc       = Just (x',y')
      , stepsPassed    = sp
      , currButton     = M.RightButton
      }

   | MouseButton G.RightButton <- key
   , Up                        <- keyState
   = let (Just (locX, locY)) = cl in
      let (x',y') = windowToModel (x,y) in
         Model
         { densityField   = df
         , densitySource  = ds
         , velocityField  = vf
         , velocitySource = Just (Source (Z:.locY:.locX)
                                 (fromIntegral (locX-x'),fromIntegral (locY-y')))
         , clickLoc       = Nothing
         , stepsPassed    = sp
         , currButton     = M.None
         }

   | Char 'r' <- key
   , Down     <- keyState
   = model

   | Char 'q' <- key
   , Down     <- keyState
   = error "Quitting"

userEvent (EventMotion (x,y)) m@(Model df ds vf vs cl sp M.LeftButton)
   = let (x',y') = windowToModel (x,y) in
      Model
      { densityField = df
      , densitySource = Just (Source (Z:.y':.x') 1)
      , velocityField = vf
      , velocitySource = vs
      , clickLoc = cl
      , stepsPassed = sp
      , currButton = M.LeftButton
      }
userEvent (EventMotion (x,y)) m@(Model df ds vf vs (Just (clx,cly)) sp M.RightButton)
   = let (x',y') = windowToModel (x,y) in
      Model
      { densityField   = df
      , densitySource  = ds
      , velocityField  = vf
      , velocitySource = Just (Source (Z:.y':.x')
                              (fromIntegral (clx-x'), fromIntegral (cly-y')))
      , clickLoc       = Just (x',y')
      , stepsPassed    = sp
      , currButton     = M.RightButton
      }

userEvent _ m
   = m

-- Converts a window location to the corresponding location in the
--    simulation.
-- NOTE: Only gives proper mapping if the window width and the width of the
--    simulator are multiples
windowToModel :: (Float,Float) -> (Int,Int)
windowToModel (x,y) = (x',y')
   where
      x' = round ((x + ((fromIntegral windowWidth) / 2)) / scaleX)
      y' = round ((y + ((fromIntegral windowHeight) / 2)) / scaleY)

