{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Data.List                                                    ( intercalate )
import Language.Haskell.TH


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
    single (NumSingleType t)    e = num t e
    single (NonNumSingleType t) e = nonnum t e

    num :: NumType e -> e -> String
    num (IntegralNumType t) e = integral t e
    num (FloatingNumType t) e = floating t e

    integral :: IntegralType e -> e -> String
    integral TypeInt{}    e = show e
    integral TypeInt8{}   e = show e
    integral TypeInt16{}  e = show e
    integral TypeInt32{}  e = show e
    integral TypeInt64{}  e = show e
    integral TypeWord{}   e = show e
    integral TypeWord8{}  e = show e
    integral TypeWord16{} e = show e
    integral TypeWord32{} e = show e
    integral TypeWord64{} e = show e

    floating :: FloatingType e -> e -> String
    floating TypeHalf{}   e = show e
    floating TypeFloat{}  e = show e
    floating TypeDouble{} e = show e

    nonnum :: NonNumType e -> e -> String
    nonnum TypeChar e = show e
    nonnum TypeBool e = show e

    vector :: VectorType (Vec n a) -> Vec n a -> String
    vector (VectorType _ s) vec
      | SingleDict <- singleDict s
      = "<" ++ intercalate ", " (single s <$> listOfVec vec) ++ ">"

liftElt :: TypeR t -> t -> Q (TExp t)
liftElt TupRunit         ()    = [|| () ||]
liftElt (TupRsingle t)   x     = [|| $$(liftScalar t x) ||]
liftElt (TupRpair ta tb) (a,b) = [|| ($$(liftElt ta a), $$(liftElt tb b)) ||]


