{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Either
-- Copyright   : [2018] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- @since 1.2.0.0
--

module Data.Array.Accelerate.Data.Either (

  Either(..),
  left, right,
  either, isLeft, isRight, fromLeft, fromRight, lefts, rights,

) where

import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar                            hiding ( (!), shape, ignore, toIndex )
import Data.Array.Accelerate.Language                               hiding ( chr )
import Data.Array.Accelerate.Prelude                                hiding ( filter )
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord

import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Monoid
#if __GLASGOW_HASKELL__ >= 800
import Data.Array.Accelerate.Data.Semigroup
#endif

import Data.Char
import Data.Either                                                  ( Either(..) )
import Data.Maybe
import Data.Typeable
import Foreign.C.Types
import Prelude                                                      ( (.), ($), const, undefined, otherwise )


-- | Lift a value into the 'Left' constructor
--
left :: forall a b. (Elt a, Elt b) => Exp a -> Exp (Either a b)
left a = lift (Left a :: Either (Exp a) (Exp b))

-- | Lift a value into the 'Right' constructor
--
right :: forall a b. (Elt a, Elt b) => Exp b -> Exp (Either a b)
right b = lift (Right b :: Either (Exp a) (Exp b))
--
-- See Note: [lifting Nothing]


-- | Return 'True' if the argument is a 'Left'-value
--
isLeft :: (Elt a, Elt b) => Exp (Either a b) -> Exp Bool
isLeft x = tag x == 0

-- | Return 'True' if the argument is a 'Right'-value
--
isRight :: (Elt a, Elt b) => Exp (Either a b) -> Exp Bool
isRight x = tag x == 1

-- | The 'fromLeft' function extracts the element out of the 'Left' constructor.
-- If the argument was actually 'Right', you will get an undefined value
-- instead.
--
fromLeft :: (Elt a, Elt b) => Exp (Either a b) -> Exp a
fromLeft x = Exp $ SuccTupIdx ZeroTupIdx `Prj` x

-- | The 'fromRight' function extracts the element out of the 'Right'
-- constructor. If the argument was actually 'Left', you will get an undefined
-- value instead.
--
fromRight :: (Elt a, Elt b) => Exp (Either a b) -> Exp b
fromRight x = Exp $ ZeroTupIdx `Prj` x

-- | The 'either' function performs case analysis on the 'Either' type. If the
-- value is @'Left' a@, apply the first function to @a@; if it is @'Right' b@,
-- apply the second function to @b@.
--
either :: (Elt a, Elt b, Elt c) => (Exp a -> Exp c) -> (Exp b -> Exp c) -> Exp (Either a b) -> Exp c
either f g x =
  cond (isLeft x) (f (fromLeft x)) (g (fromRight x))


-- | Extract from the array of 'Either' all of the 'Left' elements, together
-- with a segment descriptor indicating how many elements along each dimension
-- were returned.
--
lefts :: (Shape sh, Slice sh, Elt a, Elt b)
      => Acc (Array (sh:.Int) (Either a b))
      -> Acc (Vector a, Array sh Int)
lefts es = filter' (map isLeft es) (map fromLeft es)

-- | Extract from the array of 'Either' all of the 'Right' elements, together
-- with a segment descriptor indicating how many elements along each dimension
-- were returned.
--
rights :: (Shape sh, Slice sh, Elt a, Elt b)
       => Acc (Array (sh:.Int) (Either a b))
       -> Acc (Vector b, Array sh Int)
rights es = filter' (map isRight es) (map fromRight es)


filter'
    :: forall sh e. (Shape sh, Slice sh, Elt e)
    => Acc (Array (sh:.Int) Bool)     -- tags
    -> Acc (Array (sh:.Int) e)        -- values
    -> Acc (Vector e, Array sh Int)
filter' keep arr
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = let
        (target, len)   = unlift $ scanl' (+) 0 (map boolToInt keep)
        prj ix          = keep!ix ? ( index1 (target!ix), ignore )
        dummy           = fill (index1 (the len)) undef
        result          = permute const dummy prj arr
    in
    null keep ?| ( lift (emptyArray, fill (constant Z) 0)
                 , lift (result, len)
                 )
  | otherwise
  = let
        sz              = indexTail (shape arr)
        (target, len)   = unlift $ scanl' (+) 0 (map boolToInt keep)
        (offset, valid) = unlift $ scanl' (+) 0 (flatten len)
        prj ix          = cond (keep!ix)
                               (index1 $ offset!index1 (toIndex sz (indexTail ix)) + target!ix)
                               ignore
        dummy           = fill (index1 (the valid)) undef
        result          = permute const dummy prj arr
    in
    null keep ?| ( lift (emptyArray, fill sz 0)
                 , lift (result, len)
                 )

instance Elt a => Functor (Either a) where
  fmap f = either left (right . f)

instance (Eq a, Eq b) => Eq (Either a b) where
  ex == ey = isLeft  ex && isLeft  ey ? ( fromLeft ex  == fromLeft ey
           , isRight ex && isRight ey ? ( fromRight ex == fromRight ey
           , {- else -}                   constant False ))

instance (Ord a, Ord b) => Ord (Either a b) where
  compare ex ey = isLeft  ex && isLeft  ey ? ( compare (fromLeft ex) (fromLeft ey)
                , isRight ex && isRight ey ? ( compare (fromRight ex) (fromRight ey)
                , {- else -}                   compare (tag ex) (tag ey) ))

#if __GLASGOW_HASKELL__ >= 800
instance (Elt a, Elt b) => Semigroup (Exp (Either a b)) where
  ex <> ey = isLeft ex ? ( ey, ex )
#endif

tag :: (Elt a, Elt b) => Exp (Either a b) -> Exp Word8
tag x = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` x

type instance EltRepr (Either a b) = TupleRepr (Word8, EltRepr a, EltRepr b)

instance (Elt a, Elt b) => Elt (Either a b) where
  eltType _ = eltType (undefined::(Word8,a,b))
  toElt ((((),0),a),_)  = Left  (toElt a)
  toElt (_         ,b)  = Right (toElt b)
  fromElt (Left a)      = ((((),0), fromElt a), undef' (eltType (undefined::b)))
  fromElt (Right b)     = ((((),1), undef' (eltType (undefined::a))), fromElt b)

instance (Elt a, Elt b) => IsProduct Elt (Either a b) where
  type ProdRepr (Either a b) = ProdRepr (Word8, a, b)
  toProd _ ((((),0),a),_) = Left a
  toProd _ (_         ,b) = Right b
  fromProd _ (Left a)   = ((((), 0), a), toElt (undef' (eltType (undefined::b))))
  fromProd _ (Right b)  = ((((), 1), toElt (undef' (eltType (undefined::a)))), b)
  prod cst _ = prod cst (undefined::(Word8,a,b))

instance (Lift Exp a, Lift Exp b, Elt (Plain a), Elt (Plain b)) => Lift Exp (Either a b) where
  type Plain (Either a b) = Either (Plain a) (Plain b)
  lift (Left a)  = Exp . Tuple $ NilTup `SnocTup` constant 0 `SnocTup` lift a `SnocTup` undef
  lift (Right b) = Exp . Tuple $ NilTup `SnocTup` constant 1 `SnocTup` undef  `SnocTup` lift b


-- Utilities
-- ---------

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


emptyArray :: (Shape sh, Elt e) => Acc (Array sh e)
emptyArray = fill (constant empty) undef

matchShapeType :: forall s t. (Shape s, Shape t) => s -> t -> Maybe (s :~: t)
matchShapeType _ _
  | Just Refl <- matchTupleType (eltType (undefined::s)) (eltType (undefined::t))
  = gcast Refl

matchShapeType _ _
  = Nothing

