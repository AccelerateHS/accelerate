{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Bounded
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Bounded (

  Bounded,
  P.minBound, P.maxBound,

) where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import qualified Prelude                                            as P


-- | Name the upper and lower limits of a type. Types which are not totally
-- ordered may still have upper and lower bounds.
--
type Bounded a = (Elt a, P.Bounded (Exp a))


instance P.Bounded (Exp ()) where
  minBound = constant ()
  maxBound = constant ()

instance P.Bounded (Exp Int) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp Int8) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp Int16) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp Int32) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp Int64) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp Word) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp Word8) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp Word16) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp Word32) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp Word64) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp CShort) where
  minBound = mkBitcast (mkMinBound @Int16)
  maxBound = mkBitcast (mkMaxBound @Int16)

instance P.Bounded (Exp CUShort) where
  minBound = mkBitcast (mkMinBound @Word16)
  maxBound = mkBitcast (mkMaxBound @Word16)

instance P.Bounded (Exp CInt) where
  minBound = mkBitcast (mkMinBound @Int32)
  maxBound = mkBitcast (mkMaxBound @Int32)

instance P.Bounded (Exp CUInt) where
  minBound = mkBitcast (mkMinBound @Word32)
  maxBound = mkBitcast (mkMaxBound @Word32)

instance P.Bounded (Exp CLong) where
  minBound = mkBitcast (mkMinBound @HTYPE_CLONG)
  maxBound = mkBitcast (mkMaxBound @HTYPE_CLONG)

instance P.Bounded (Exp CULong) where
  minBound = mkBitcast (mkMinBound @HTYPE_CULONG)
  maxBound = mkBitcast (mkMaxBound @HTYPE_CULONG)

instance P.Bounded (Exp CLLong) where
  minBound = mkBitcast (mkMinBound @Int64)
  maxBound = mkBitcast (mkMaxBound @Int64)

instance P.Bounded (Exp CULLong) where
  minBound = mkBitcast (mkMinBound @Word64)
  maxBound = mkBitcast (mkMaxBound @Word64)

instance P.Bounded (Exp Bool) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp Char) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp CChar) where
  minBound = mkBitcast (mkMinBound @HTYPE_CCHAR)
  maxBound = mkBitcast (mkMaxBound @HTYPE_CCHAR)

instance P.Bounded (Exp CSChar) where
  minBound = mkBitcast (mkMinBound @Int8)
  maxBound = mkBitcast (mkMaxBound @Int8)

instance P.Bounded (Exp CUChar) where
  minBound = mkBitcast (mkMinBound @Word8)
  maxBound = mkBitcast (mkMaxBound @Word8)

instance (Bounded a, Bounded b)
    => P.Bounded (Exp (a,b)) where
  minBound = tup2 (P.minBound, P.minBound)
  maxBound = tup2 (P.maxBound, P.maxBound)

instance (Bounded a, Bounded b, Bounded c)
    => P.Bounded (Exp (a,b,c)) where
  minBound = tup3 (P.minBound, P.minBound, P.minBound)
  maxBound = tup3 (P.maxBound, P.maxBound, P.maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d)
    => P.Bounded (Exp (a,b,c,d)) where
  minBound = tup4 (P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup4 (P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e)
    => P.Bounded (Exp (a,b,c,d,e)) where
  minBound = tup5 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup5 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f)
    => P.Bounded (Exp (a,b,c,d,e,f)) where
  minBound = tup6 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup6 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g)
    => P.Bounded (Exp (a,b,c,d,e,f,g)) where
  minBound = tup7 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup7 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h)
    => P.Bounded (Exp (a,b,c,d,e,f,g,h)) where
  minBound = tup8 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup8 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i)
    => P.Bounded (Exp (a,b,c,d,e,f,g,h,i)) where
  minBound = tup9 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup9 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j)
    => P.Bounded (Exp (a,b,c,d,e,f,g,h,i,j)) where
  minBound = tup10 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup10 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k)
    => P.Bounded (Exp (a,b,c,d,e,f,g,h,i,j,k)) where
  minBound = tup11 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup11 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l)
    => P.Bounded (Exp (a,b,c,d,e,f,g,h,i,j,k,l)) where
  minBound = tup12 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup12 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m)
    => P.Bounded (Exp (a,b,c,d,e,f,g,h,i,j,k,l,m)) where
  minBound = tup13 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup13 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m, Bounded n)
    => P.Bounded (Exp (a,b,c,d,e,f,g,h,i,j,k,l,m,n)) where
  minBound = tup14 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup14 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m, Bounded n, Bounded o)
    => P.Bounded (Exp (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)) where
  minBound = tup15 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup15 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m, Bounded n, Bounded o, Bounded p)
    => P.Bounded (Exp (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)) where
  minBound = tup16 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup16 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

