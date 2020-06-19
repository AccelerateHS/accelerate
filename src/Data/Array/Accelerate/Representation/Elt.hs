{-# LANGUAGE GADTs           #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Representation.Elt
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Representation.Elt
  where

import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type
import Data.Primitive.Vec

import Control.Monad.ST
import Data.List                                                    ( intercalate )
import Data.Primitive.ByteArray
import Foreign.Storable
import Language.Haskell.TH


undefElt :: TypeR t -> t
undefElt = tuple
  where
    tuple :: TypeR t -> t
    tuple TupRunit         = ()
    tuple (TupRpair ta tb) = (tuple ta, tuple tb)
    tuple (TupRsingle t)   = scalar t

    scalar :: ScalarType t -> t
    scalar (SingleScalarType t) = single t
    scalar (VectorScalarType t) = vector t

    vector :: VectorType t -> t
    vector (VectorType n t) = runST $ do
      mba           <- newByteArray (n * bytesElt (TupRsingle (SingleScalarType t)))
      ByteArray ba# <- unsafeFreezeByteArray mba
      return (Vec ba#)

    single :: SingleType t -> t
    single (NumSingleType t) = num t

    num :: NumType t -> t
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType t -> t
    integral TypeInt    = 0
    integral TypeInt8   = 0
    integral TypeInt16  = 0
    integral TypeInt32  = 0
    integral TypeInt64  = 0
    integral TypeWord   = 0
    integral TypeWord8  = 0
    integral TypeWord16 = 0
    integral TypeWord32 = 0
    integral TypeWord64 = 0

    floating :: FloatingType t -> t
    floating TypeHalf   = 0
    floating TypeFloat  = 0
    floating TypeDouble = 0

bytesElt :: TypeR e -> Int
bytesElt = tuple
  where
    tuple :: TypeR t -> Int
    tuple TupRunit         = 0
    tuple (TupRpair ta tb) = tuple ta + tuple tb
    tuple (TupRsingle t)   = scalar t

    scalar :: ScalarType t -> Int
    scalar (SingleScalarType t) = single t
    scalar (VectorScalarType t) = vector t

    vector :: VectorType t -> Int
    vector (VectorType n t) = n * single t

    single :: SingleType t -> Int
    single (NumSingleType t) = num t

    num :: NumType t -> Int
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType t -> Int
    integral TypeInt    = sizeOf (undefined::Int)
    integral TypeInt8   = 1
    integral TypeInt16  = 2
    integral TypeInt32  = 4
    integral TypeInt64  = 8
    integral TypeWord   = sizeOf (undefined::Word)
    integral TypeWord8  = 1
    integral TypeWord16 = 2
    integral TypeWord32 = 4
    integral TypeWord64 = 8

    floating :: FloatingType t -> Int
    floating TypeHalf   = 2
    floating TypeFloat  = 4
    floating TypeDouble = 8

showElt :: TypeR e -> e -> String
showElt t v = showsElt t v ""

showsElt :: TypeR e -> e -> ShowS
showsElt = tuple
  where
    tuple :: TypeR e -> e -> ShowS
    tuple TupRunit         ()       = showString "()"
    tuple (TupRpair t1 t2) (e1, e2) = showString "(" . tuple t1 e1 . showString ", " . tuple t2 e2 . showString ")"
    tuple (TupRsingle tp)  val      = scalar tp val

    scalar :: ScalarType e -> e -> ShowS
    scalar (SingleScalarType t) e = showString $ single t e
    scalar (VectorScalarType t) e = showString $ vector t e

    single :: SingleType e -> e -> String
    single (NumSingleType t) e = num t e

    num :: NumType e -> e -> String
    num (IntegralNumType t) e = integral t e
    num (FloatingNumType t) e = floating t e

    integral :: IntegralType e -> e -> String
    integral TypeInt    e = show e
    integral TypeInt8   e = show e
    integral TypeInt16  e = show e
    integral TypeInt32  e = show e
    integral TypeInt64  e = show e
    integral TypeWord   e = show e
    integral TypeWord8  e = show e
    integral TypeWord16 e = show e
    integral TypeWord32 e = show e
    integral TypeWord64 e = show e

    floating :: FloatingType e -> e -> String
    floating TypeHalf   e = show e
    floating TypeFloat  e = show e
    floating TypeDouble e = show e

    vector :: VectorType (Vec n a) -> Vec n a -> String
    vector (VectorType _ s) vec
      | SingleDict <- singleDict s
      = "<" ++ intercalate ", " (single s <$> listOfVec vec) ++ ">"

liftElt :: TypeR t -> t -> Q (TExp t)
liftElt TupRunit         ()    = [|| () ||]
liftElt (TupRsingle t)   x     = [|| $$(liftScalar t x) ||]
liftElt (TupRpair ta tb) (a,b) = [|| ($$(liftElt ta a), $$(liftElt tb b)) ||]

