module Constants
   ( widthI
   , widthF
   , dt
   , diff
   , windowWidth
   , windowHeight
   , scaleX
   , scaleY
   , visc
   , newDensity
   , newVelocity
   -- Arguments: defined as IORefs
   , widthArg
   , dtArg
   , diffArg
   , viscArg
   , windowWidthArg
   , densArg
   , velArg
   )
   where

import System.IO.Unsafe
import Data.IORef

widthArg :: IORef Int
{-# NOINLINE widthArg #-}
widthArg = unsafePerformIO $ newIORef 100

widthI :: Int
{-# INLINE widthI #-}
widthI = unsafePerformIO $ readIORef widthArg

widthF :: Float
{-# INLINE widthF #-}
widthF = fromIntegral widthI

dtArg :: IORef Float
{-# NOINLINE dtArg #-}
dtArg = unsafePerformIO $ newIORef 0.1

dt :: Float
{-# INLINE dt #-}
dt = unsafePerformIO $ readIORef dtArg

diffArg :: IORef Float
{-# NOINLINE diffArg #-}
diffArg = unsafePerformIO $ newIORef 0

diff :: Float
{-# INLINE diff #-}
diff = unsafePerformIO $ readIORef diffArg

viscArg :: IORef Float
{-# NOINLINE viscArg #-}
viscArg = unsafePerformIO $ newIORef 0

visc :: Float
{-# INLINE visc #-}
visc = unsafePerformIO $ readIORef viscArg

windowWidthArg :: IORef Int
{-# NOINLINE windowWidthArg #-}
windowWidthArg = unsafePerformIO $ newIORef 500

windowWidth :: Int
{-# INLINE windowWidth #-}
windowWidth = unsafePerformIO $ readIORef windowWidthArg

windowHeight :: Int
{-# INLINE windowHeight #-}
windowHeight = windowWidth

scaleX :: Float
{-# INLINE scaleX #-}
scaleX = fromIntegral (windowWidth `div` widthI)

scaleY :: Float
{-# INLINE scaleY #-}
scaleY = scaleX

densArg :: IORef Float
{-# NOINLINE densArg #-}
densArg = unsafePerformIO $ newIORef 100

newDensity :: Float
{-# INLINE newDensity #-}
newDensity = unsafePerformIO $ readIORef densArg

velArg :: IORef (Float, Float)
{-# NOINLINE velArg #-}
velArg = unsafePerformIO $ newIORef (20, 20)

newVelocity :: (Float, Float)
{-# INLINE newVelocity #-}
newVelocity = unsafePerformIO $ readIORef velArg
