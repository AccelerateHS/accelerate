{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Array.Accelerate.Unsafe
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Operations which may be unsafe. Use with care.
--
-- @since 1.2.0.0
--

module Data.Array.Accelerate.Unsafe (

  -- ** Unsafe operations
  Coerce, coerce,
  undef,

) where

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Shape


-- | The class 'Coercible' reinterprets the bits of a value or array of values
-- as that of a different type.
--
-- At the expression level, this allows you to convert a value between any two
-- types whose underlying representations have the same bit size at each
-- component.
--
-- For example:
--
-- > coerce (x :: Exp Double)         :: Exp Word64
-- > coerce (x :: Exp (Int64,Float))  :: Exp (Complex Float, Word32)
--
-- Furthermore, as we typically declare newtype wrappers similarly to:
--
-- > type instance EltR (Sum a) = ((), EltR a)
--
-- This can be used instead of the newtype constructor, to go from the newtype's
-- abstract type to the concrete type by dropping the extra @()@ from the
-- representation, and vice-versa.
--
-- At the array level this may also entail changing the size of the innermost
-- dimension.
--
-- For example:
--
-- > coerce (x :: Acc (Vector Float)) :: Acc (Vector (Complex Float))
--
-- will result in an array with half as many elements, as each element now
-- consists of two values (the real and imaginary values laid out consecutively
-- in memory, and now interpreted as a single packed 'Vec 2 Float'). For this to
-- be safe, the size of 'x' must therefore be even.
--
-- Note that when applied at the array level 'coerce' prevents array fusion.
-- Therefore if the bit size of the source and target value types is the same,
-- then:
--
-- > map f . coerce . map g
--
-- will result in two kernels being executed, whereas:
--
-- > map f . map coerce . map g
--
-- will fuse into a single kernel.
--
-- @since 1.4.0.0
--
class Coercible f a b where
  coerce :: f a -> f b

instance Acoerce (EltR a) (EltR b) => Coercible Acc (Array (sh :. Int) a) (Array (sh :. Int) b) where
  coerce = mkAcoerce

instance Coerce (EltR a) (EltR b) => Coercible Exp a b where
  coerce = mkCoerce

