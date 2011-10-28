{-# LANGUAGE FlexibleInstances #-}

-- Defines operations for use with different field types

module FieldElt where

import Data.Array.Repa

import Constants

class (Elt a) => FieldElt a where
   (~+~)     :: a    -> a     -> a
   (~-~)     :: a    -> a     -> a
   (~*~)     :: a    -> Float -> a
   (~/~)     :: a    -> Float -> a
   addIf     :: Bool -> a     -> a -> a
   useIf     :: Bool -> a     -> a
   negate    :: a    -> a
   addSource :: a    -> a     -> a
   zero      :: a

instance FieldElt Float where
   {-# INLINE (~+~) #-}
   (~+~) a b   = a + b
   {-# INLINE (~-~) #-}
   (~-~) a b   = a - b
   {-# INLINE (~*~) #-}
   (~*~) a b   = a * b
   {-# INLINE (~/~) #-}
   (~/~) a b   = a / b

   {-# INLINE addIf #-}
   addIf True  a b = a + b
   addIf False _ b = b

   {-# INLINE useIf #-}
   useIf True  a = a
   useIf False _ = 0

   {-# INLINE negate #-}
   negate a    = 0 ~-~ a
   {-# INLINE addSource #-}
   addSource a mul = a ~+~ (newDensity * dt * mul)

   {-# INLINE zero #-}
   zero = 0

instance FieldElt (Float, Float) where
   {-# INLINE (~+~) #-}
   (~+~) (a1, a2) (b1, b2) = c1 `seq` c2 `seq` (c1, c2)
                            where
                              c1 = a1 + b1
                              c2 = a2 + b2
   {-# INLINE (~-~) #-}
   (~-~) (a1, a2) (b1, b2) = c1 `seq` c2 `seq` (c1, c2)
                            where
                              c1 = a1 - b1
                              c2 = a2 - b2
   {-# INLINE (~*~) #-}
   (~*~) (a1, a2)  b       = c1 `seq` c2 `seq` (c1, c2)
                            where
                              c1 = a1 * b
                              c2 = a2 * b
   {-# INLINE (~/~) #-}
   (~/~) (a1, a2)  b       = c1 `seq` c2 `seq` (c1, c2)
                           where
                              c1 = a1 / b
                              c2 = a2 / b

   {-# INLINE addIf #-}
   addIf True  a b  = a ~+~ b
   addIf False a b  = b 

   {-# INLINE useIf #-}
   useIf True  a = a
   useIf False _ = (0, 0)

   {-# INLINE negate #-}
   negate (a1, a2) = (~-~) (0, 0) (a1, a2)
   {-# INLINE addSource #-}
   addSource (a,b) (mulA, mulB)
      = let (newA, newB) = newVelocity in
         (a+(newA*dt*(-mulA)), b+(newB*dt*(-mulB)))

   {-# INLINE zero #-}
   zero = (0, 0)
