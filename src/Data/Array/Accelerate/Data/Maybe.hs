{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Maybe
-- Copyright   : [2018] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Data.Maybe (

  Maybe(..),
  maybe, fromMaybe, isJust, isNothing,

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Language                               hiding ( chr )
import Data.Array.Accelerate.Lift
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Char
import Data.Maybe                                                   ( Maybe(..) )
import Foreign.C.Types
import Prelude                                                      ( (.), ($), undefined )


-- | Returns 'True' if the argument is 'Nothing'
--
isNothing :: Elt a => Exp (Maybe a) -> Exp Bool
isNothing x =
  let t = Exp $ SuccTupIdx ZeroTupIdx `Prj` x
  in  t == 0

-- | Returns 'True' if the argument is of the form @Just _@
--
isJust :: Elt a => Exp (Maybe a) -> Exp Bool
isJust x =
  let t = Exp $ SuccTupIdx ZeroTupIdx `Prj` x
  in  t /= 0

-- | The 'fromMaybe' function takes a default value and a 'Maybe' value. If the
-- 'Maybe' is 'Nothing', the default value is returned; otherwise, it returns
-- the value contained in the 'Maybe'.
--
fromMaybe :: Elt a => Exp a -> Exp (Maybe a) -> Exp a
fromMaybe d x =
  let t = Exp $ SuccTupIdx ZeroTupIdx `Prj` x
      v = Exp $ ZeroTupIdx `Prj` x
  in
  cond (t == 0) d v

-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
-- value. If the 'Maybe' value is nothing, the default value is returned;
-- otherwise, it applies the function to the value inside the 'Just' and returns
-- the result
--
maybe :: (Elt a, Elt b) => Exp b -> (Exp a -> Exp b) -> Exp (Maybe a) -> Exp b
maybe d f x =
  let t = Exp $ SuccTupIdx ZeroTupIdx `Prj` x
      v = Exp $ ZeroTupIdx `Prj` x
  in
  cond (t == 0) d (f v)


type instance EltRepr (Maybe a) = (Word8, EltRepr a)

instance Elt a => Elt (Maybe a) where
  eltType _ = TypeRpair (eltType (undefined::Word8)) (eltType (undefined::a))
  toElt (0,_) = Nothing
  toElt (_,x) = Just (toElt x)
  fromElt Nothing  = (0, undef (eltType (undefined::a)))
  fromElt (Just a) = (1, fromElt a)

instance Elt a => IsProduct Elt (Maybe a) where
  type ProdRepr (Maybe a) = ProdRepr (Word8, a)
  toProd _ (((),0),_) = Nothing
  toProd _ (_,     x) = Just x
  fromProd _ Nothing  = (((), 0), toElt (undef (eltType (undefined::a))))
  fromProd _ (Just a) = (((), 1), a)
  prod cst _ = prod cst (undefined :: (Word8,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Maybe a) where
  type Plain (Maybe a) = Maybe (Plain a)
  lift Nothing  = Exp . Tuple $ NilTup `SnocTup` constant 0 `SnocTup` constant (toElt (undef (eltType (undefined::Plain a))))
  lift (Just x) = Exp . Tuple $ NilTup `SnocTup` constant 1 `SnocTup` lift x


-- Sometimes we need a default value for the Nothing case. We just fill this
-- with zeros, though it would be better if we can actually do nothing, and
-- leave those value in memory undefined.
--

undef :: TupleType t -> t
undef TypeRunit         = ()
undef (TypeRpair ta tb) = (undef ta, undef tb)
undef (TypeRscalar s)   = scalar s

scalar :: ScalarType t -> t
scalar (SingleScalarType t) = single t
scalar (VectorScalarType t) = vector t

single :: SingleType t -> t
single (NumSingleType    t) = num t
single (NonNumSingleType t) = nonnum t

vector :: VectorType t -> t
vector (Vector2Type t)  = let x = single t in V2 x x
vector (Vector3Type t)  = let x = single t in V3 x x x
vector (Vector4Type t)  = let x = single t in V4 x x x x
vector (Vector8Type t)  = let x = single t in V8 x x x x x x x x
vector (Vector16Type t) = let x = single t in V16 x x x x x x x x x x x x x x x x

num :: NumType t -> t
num (IntegralNumType t) | IntegralDict <- integralDict t = 0
num (FloatingNumType t) | FloatingDict <- floatingDict t = 0

nonnum :: NonNumType t -> t
nonnum TypeBool{}   = False
nonnum TypeChar{}   = chr 0
nonnum TypeCChar{}  = CChar 0
nonnum TypeCSChar{} = CSChar 0
nonnum TypeCUChar{} = CUChar 0

