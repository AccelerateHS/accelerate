{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Scene.Color
  where


-- frenemies
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple                              ( Tuple(..), TupleIdx(..), IsTuple(..), )
import Data.Array.Accelerate.Array.Sugar                        ( Elt(..), EltRepr, EltRepr' )

-- standard library
import Prelude                                                  as P
import Data.Bits
import Data.Typeable


-- | An abstract colour value
type Color = RGB Float


-- | Same as 'Graphics.Gloss.Accelerate.Data.Color.RGBA', but colours doesn't
--   have an alpha component.
--
--   That module now also exports an 'RGB' type, but we can't use it because the
--   'Lift' and 'Unlift' instances are mysteriously not exported (???).
--
data RGB a = RGB a a a
  deriving (Show, Eq, Typeable)


instance Num a => Num (RGB a) where
  (+) (RGB r1 g1 b1 ) (RGB r2 g2 b2)
        = RGB (r1 + r2) (g1 + g2) (b1 + b2)

  (-) (RGB r1 g1 b1) (RGB r2 g2 b2)
        = RGB (r1 - r2) (g1 - g2) (b1 - b2)

  (*) (RGB r1 g1 b1) (RGB r2 g2 b2)
        = RGB (r1 * r2) (g1 * g2) (b1 * b2)

  abs (RGB r1 g1 b1)
        = RGB (abs r1) (abs g1) (abs b1)

  signum (RGB r1 g1 b1)
        = RGB (signum r1) (signum g1) (signum b1)

  fromInteger i
        = let f = fromInteger i
          in  RGB f f f

instance (Elt a, IsNum a) => Num (Exp (RGB a)) where
  (+)           = lift2 ((+) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  (-)           = lift2 ((-) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  (*)           = lift2 ((*) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  abs           = lift1 (abs :: RGB (Exp a) -> RGB (Exp a))
  signum        = lift1 (signum :: RGB (Exp a) -> RGB (Exp a))
  fromInteger i = let f = constant (fromInteger i)
                  in lift $ RGB f f f

-- Represent colours in Accelerate as a 4-tuple
--
type instance EltRepr  (RGB a) = EltRepr (a, a, a)
type instance EltRepr' (RGB a) = EltRepr (a, a, a)

instance Elt a => Elt (RGB a) where
  eltType (_ :: RGB a)          = eltType (undefined :: (a,a,a))
  toElt c                       = let (r,g,b) = toElt c in RGB r g b
  fromElt (RGB r g b)           = fromElt (r,g,b)

  eltType' (_ :: RGB a)         = eltType' (undefined :: (a,a,a))
  toElt' c                      = let (r,g,b) = toElt' c in RGB r g b
  fromElt' (RGB r g b)          = fromElt' (r,g,b)

instance IsTuple (RGB a) where
  type TupleRepr (RGB a)        = ((((),a), a), a)
  fromTuple (RGB r g b)         = ((((), r), g), b)
  toTuple ((((),r),g),b)        = RGB r g b

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (RGB a) where
  type Plain (RGB a)    = RGB (Plain a)
  lift (RGB r g b)      = Exp . Tuple $ NilTup `SnocTup` lift r `SnocTup` lift g `SnocTup` lift b

instance Elt a => Unlift Exp (RGB (Exp a)) where
  unlift c      = let r = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` c
                      g = Exp $ SuccTupIdx ZeroTupIdx `Prj` c
                      b = Exp $ ZeroTupIdx `Prj` c
                  in RGB r g b


-- | Make a custom color.
--   You promise that all components are clamped to the range [0..1]
--
rawColor :: Exp Float -> Exp Float -> Exp Float -> Exp Color
rawColor r g b = lift (RGB r g b)


-- | Take the RGBA components of a color.
rgbOfColor :: Exp Color -> (Exp Float, Exp Float, Exp Float)
rgbOfColor c
  = let (RGB r g b) = unlift c
    in  (r, g, b)


-- Color functions ------------------------------------------------------------

-- | Mix two colors with the given ratios.
--
mixColors
    :: Exp Float        -- ^ Ratio of first color.
    -> Exp Float        -- ^ Ratio of second color.
    -> Exp Color        -- ^ First color.
    -> Exp Color        -- ^ Second color.
    -> Exp Color        -- ^ Resulting color.

mixColors ratio1 ratio2 c1 c2
  = let RGB r1 g1 b1            = unlift c1
        RGB r2 g2 b2            = unlift c2

        total   = ratio1 + ratio2
        m1      = ratio1 / total
        m2      = ratio2 / total
   in
   rawColor (m1 * r1 + m2 * r2)
            (m1 * g1 + m2 * g2)
            (m1 * b1 + m2 * b2)


-- | Clamp components of a color into the required range.
--
clampColor :: Exp Color -> Exp Color
clampColor cc
  = let (r, g, b)       = rgbOfColor cc
    in  rawColor (min 1 r) (min 1 g) (min 1 b)


-- | Normalise a color to the value of its largest RGB component.
--
normaliseColor :: Exp Color -> Exp Color
normaliseColor cc
  = let (r, g, b)       = rgbOfColor cc
        m               = P.maximum [r, g, b]
    in  rawColor (r / m) (g / m) (b / m)


-- | Convert a color into a packed RGBA value.
--
packRGBA :: Exp Color -> Exp Word32
packRGBA c
  = let (r, g, b)       = rgbOfColor c
    in  word32OfFloat r `A.shiftL` 24
    .|. word32OfFloat g `A.shiftL` 16
    .|. word32OfFloat b `A.shiftL` 8
    .|. 0xFF

word32OfFloat :: Exp Float -> Exp Word32
word32OfFloat f = A.truncate (f * 255)


-- | Add RGB components of a color component-wise, then normalise them to the
--   highest resulting one.
--
addColors :: Exp Color -> Exp Color -> Exp Color
addColors c1 c2
  = let RGB r1 g1 b1    = unlift c1
        RGB r2 g2 b2    = unlift c2
    in
    normaliseColor $ rawColor (r1 + r2) (g1 + g2) (b1 + b2)


-- Pre-defined colors ---------------------------------------------------------

black, white :: Exp Color
black           = rawColor 0.0 0.0 0.0
white           = rawColor 1.0 1.0 1.0

