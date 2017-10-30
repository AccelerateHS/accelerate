{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Bounded
-- Copyright   : [2016..2017] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Bounded (

  Bounded,
  P.minBound, P.maxBound,

) where

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
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp CUShort) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp CInt) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp CUInt) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp CLong) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp CULong) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp CLLong) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp CULLong) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp Bool) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp Char) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp CChar) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp CSChar) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance P.Bounded (Exp CUChar) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance ( P.Bounded (Exp a), P.Bounded (Exp b)
         , Elt a, Elt b
         ) => P.Bounded (Exp (a,b)) where
  minBound = tup2 (P.minBound, P.minBound)
  maxBound = tup2 (P.maxBound, P.maxBound)

instance ( P.Bounded (Exp a), P.Bounded (Exp b), P.Bounded (Exp c)
         , Elt a, Elt b, Elt c
         ) => P.Bounded (Exp (a,b,c)) where
  minBound = tup3 (P.minBound, P.minBound, P.minBound)
  maxBound = tup3 (P.maxBound, P.maxBound, P.maxBound)

instance ( P.Bounded (Exp a), P.Bounded (Exp b), P.Bounded (Exp c), P.Bounded (Exp d)
         , Elt a, Elt b, Elt c, Elt d
         ) => P.Bounded (Exp (a,b,c,d)) where
  minBound = tup4 (P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup4 (P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance ( P.Bounded (Exp a), P.Bounded (Exp b), P.Bounded (Exp c), P.Bounded (Exp d), P.Bounded (Exp e)
         , Elt a, Elt b, Elt c, Elt d, Elt e
         ) => P.Bounded (Exp (a,b,c,d,e)) where
  minBound = tup5 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup5 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance ( P.Bounded (Exp a), P.Bounded (Exp b), P.Bounded (Exp c), P.Bounded (Exp d), P.Bounded (Exp e), P.Bounded (Exp f)
         , Elt a, Elt b, Elt c, Elt d, Elt e, Elt f
         ) => P.Bounded (Exp (a,b,c,d,e,f)) where
  minBound = tup6 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup6 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance ( P.Bounded (Exp a), P.Bounded (Exp b), P.Bounded (Exp c), P.Bounded (Exp d), P.Bounded (Exp e), P.Bounded (Exp f), P.Bounded (Exp g)
         , Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g
         ) => P.Bounded (Exp (a,b,c,d,e,f,g)) where
  minBound = tup7 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup7 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance ( P.Bounded (Exp a), P.Bounded (Exp b), P.Bounded (Exp c), P.Bounded (Exp d), P.Bounded (Exp e), P.Bounded (Exp f), P.Bounded (Exp g), P.Bounded (Exp h)
         , Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h
         ) => P.Bounded (Exp (a,b,c,d,e,f,g,h)) where
  minBound = tup8 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup8 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance ( P.Bounded (Exp a), P.Bounded (Exp b), P.Bounded (Exp c), P.Bounded (Exp d), P.Bounded (Exp e), P.Bounded (Exp f), P.Bounded (Exp g), P.Bounded (Exp h), P.Bounded (Exp i)
         , Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i
         ) => P.Bounded (Exp (a,b,c,d,e,f,g,h,i)) where
  minBound = tup9 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup9 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance ( P.Bounded (Exp a), P.Bounded (Exp b), P.Bounded (Exp c), P.Bounded (Exp d), P.Bounded (Exp e), P.Bounded (Exp f), P.Bounded (Exp g), P.Bounded (Exp h), P.Bounded (Exp i), P.Bounded (Exp j)
         , Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j
         ) => P.Bounded (Exp (a,b,c,d,e,f,g,h,i,j)) where
  minBound = tup10 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup10 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance ( P.Bounded (Exp a), P.Bounded (Exp b), P.Bounded (Exp c), P.Bounded (Exp d), P.Bounded (Exp e), P.Bounded (Exp f), P.Bounded (Exp g), P.Bounded (Exp h), P.Bounded (Exp i), P.Bounded (Exp j), P.Bounded (Exp k)
         , Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k
         ) => P.Bounded (Exp (a,b,c,d,e,f,g,h,i,j,k)) where
  minBound = tup11 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup11 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance ( P.Bounded (Exp a), P.Bounded (Exp b), P.Bounded (Exp c), P.Bounded (Exp d), P.Bounded (Exp e), P.Bounded (Exp f), P.Bounded (Exp g), P.Bounded (Exp h), P.Bounded (Exp i), P.Bounded (Exp j), P.Bounded (Exp k), P.Bounded (Exp l)
         , Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l
         ) => P.Bounded (Exp (a,b,c,d,e,f,g,h,i,j,k,l)) where
  minBound = tup12 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup12 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance ( P.Bounded (Exp a), P.Bounded (Exp b), P.Bounded (Exp c), P.Bounded (Exp d), P.Bounded (Exp e), P.Bounded (Exp f), P.Bounded (Exp g), P.Bounded (Exp h), P.Bounded (Exp i), P.Bounded (Exp j), P.Bounded (Exp k), P.Bounded (Exp l), P.Bounded (Exp m)
         , Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m
         ) => P.Bounded (Exp (a,b,c,d,e,f,g,h,i,j,k,l,m)) where
  minBound = tup13 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup13 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance ( P.Bounded (Exp a), P.Bounded (Exp b), P.Bounded (Exp c), P.Bounded (Exp d), P.Bounded (Exp e), P.Bounded (Exp f), P.Bounded (Exp g), P.Bounded (Exp h), P.Bounded (Exp i), P.Bounded (Exp j), P.Bounded (Exp k), P.Bounded (Exp l), P.Bounded (Exp m), P.Bounded (Exp n)
         , Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n
         ) => P.Bounded (Exp (a,b,c,d,e,f,g,h,i,j,k,l,m,n)) where
  minBound = tup14 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup14 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

instance ( P.Bounded (Exp a), P.Bounded (Exp b), P.Bounded (Exp c), P.Bounded (Exp d), P.Bounded (Exp e), P.Bounded (Exp f), P.Bounded (Exp g), P.Bounded (Exp h), P.Bounded (Exp i), P.Bounded (Exp j), P.Bounded (Exp k), P.Bounded (Exp l), P.Bounded (Exp m), P.Bounded (Exp n), P.Bounded (Exp o)
         , Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n, Elt o
         ) => P.Bounded (Exp (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)) where
  minBound = tup15 (P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound, P.minBound)
  maxBound = tup15 (P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound, P.maxBound)

