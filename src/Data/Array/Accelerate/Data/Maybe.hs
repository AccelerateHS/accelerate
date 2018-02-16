{-# LANGUAGE CPP                   #-}
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
-- @since 1.2.0.0
--

module Data.Array.Accelerate.Data.Maybe (

  Maybe(..),
  maybe, isJust, isNothing, fromMaybe, fromJust,

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Language                               hiding ( chr )
import Data.Array.Accelerate.Lift
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Ord

import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Monoid
#if __GLASGOW_HASKELL__ >= 800
import Data.Array.Accelerate.Data.Semigroup
#endif

import Data.Char
import Data.Maybe                                                   ( Maybe(..) )
import Foreign.C.Types
import Prelude                                                      ( (.), ($), undefined )


-- | Returns 'True' if the argument is 'Nothing'
--
isNothing :: Elt a => Exp (Maybe a) -> Exp Bool
isNothing x = tag x == 0

-- | Returns 'True' if the argument is of the form @Just _@
--
isJust :: Elt a => Exp (Maybe a) -> Exp Bool
isJust x = tag x == 1

-- | The 'fromMaybe' function takes a default value and a 'Maybe' value. If the
-- 'Maybe' is 'Nothing', the default value is returned; otherwise, it returns
-- the value contained in the 'Maybe'.
--
fromMaybe :: Elt a => Exp a -> Exp (Maybe a) -> Exp a
fromMaybe d x = cond (isNothing x) d (fromJust x)

-- | The 'fromJust' function extracts the element out of the 'Just' constructor.
-- If the argument was actually 'Nothing', you will get an undefined value
-- instead.
--
fromJust :: Elt a => Exp (Maybe a) -> Exp a
fromJust x = Exp $ ZeroTupIdx `Prj` x

-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
-- value. If the 'Maybe' value is nothing, the default value is returned;
-- otherwise, it applies the function to the value inside the 'Just' and returns
-- the result
--
maybe :: (Elt a, Elt b) => Exp b -> (Exp a -> Exp b) -> Exp (Maybe a) -> Exp b
maybe d f x = cond (isNothing x) d (f (fromJust x))


instance Functor Maybe where
  fmap f x = cond (isNothing x) (constant Nothing) (lift (Just (f (fromJust x))))

instance Eq a => Eq (Maybe a) where
  ma == mb = cond (isNothing ma && isNothing mb) (constant True)
           $ cond (isJust ma    && isJust mb)    (fromJust ma == fromJust mb)
           $ constant False

instance Ord a => Ord (Maybe a) where
  compare ma mb = cond (isJust ma && isJust mb)
                       (compare (fromJust ma) (fromJust mb))
                       (compare (tag ma) (tag mb))

instance (Monoid (Exp a), Elt a) => Monoid (Exp (Maybe a)) where
  mempty        = constant Nothing
  mappend ma mb = cond (isNothing ma) mb
                $ cond (isNothing mb) ma
                $ lift (Just (fromJust ma `mappend` fromJust mb))

#if __GLASGOW_HASKELL__ >= 800
instance (Semigroup (Exp a), Elt a) => Semigroup (Exp (Maybe a)) where
  ma <> mb = cond (isNothing ma) mb
           $ cond (isNothing mb) mb
           $ lift (Just (fromJust ma <> fromJust mb))
#endif


tag :: Elt a => Exp (Maybe a) -> Exp Word8
tag x = Exp $ SuccTupIdx ZeroTupIdx `Prj` x


type instance EltRepr (Maybe a) = (Word8, EltRepr a)

instance Elt a => Elt (Maybe a) where
  eltType _ = TypeRpair (eltType (undefined::Word8)) (eltType (undefined::a))
  toElt (0,_) = Nothing
  toElt (_,x) = Just (toElt x)
  fromElt Nothing  = (0, undef' (eltType (undefined::a)))
  fromElt (Just a) = (1, fromElt a)

instance Elt a => IsProduct Elt (Maybe a) where
  type ProdRepr (Maybe a) = ProdRepr (Word8, a)
  toProd _ (((),0),_) = Nothing
  toProd _ (_,     x) = Just x
  fromProd _ Nothing  = (((), 0), toElt (undef' (eltType (undefined::a))))
  fromProd _ (Just a) = (((), 1), a)
  prod cst _ = prod cst (undefined :: (Word8,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Maybe a) where
  type Plain (Maybe a) = Maybe (Plain a)
  lift Nothing  = Exp . Tuple $ NilTup `SnocTup` constant 0 `SnocTup` undef
  lift (Just x) = Exp . Tuple $ NilTup `SnocTup` constant 1 `SnocTup` lift x


-- We need an undefined value for the Nothing case. We just fill this with
-- zeros, though it would be better if we can actually do nothing, and leave
-- those value in memory undefined.
--
undef' :: TupleType t -> t
undef' TypeRunit         = ()
undef' (TypeRpair ta tb) = (undef' ta, undef' tb)
undef' (TypeRscalar s)   = scalar s

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

