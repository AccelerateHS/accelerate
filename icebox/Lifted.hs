{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Lifted
-- Copyright   : [2012..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lifted array representation. Vector of arrays represented as segmented
-- vectors.
--

module Data.Array.Accelerate.Array.Lifted (

  Vector'(..), LiftedArray,

  LiftedTupleRepr,

  IsConstrained(..),

  isArraysFlat,

  elements', shapes', empty', length', drop', vec2Vec', fromList', toList'

) where

import Prelude                                                  hiding ( concat )

-- friends
import Data.Array.Accelerate.Array.Sugar
import qualified Data.Array.Accelerate.Array.Representation     as Repr


-- Lifted arrays
-- ----------------
--
-- We specify a special new type of surface tuple to represent the lifted version of members of the
-- `Arrays' class. We do this in order to convince the type checker that the lifted arrays or tuples
-- of arrays, are still members of the 'Arrays' class.

newtype Vector' a = Vector' (LiftedRepr (ArrRepr a) a)

type family LiftedRepr r a where
  LiftedRepr ()     ()                 = ((),Scalar Int)
  LiftedRepr (Array sh e) (Array sh e) = (((),Segments sh), Vector e)
  LiftedRepr (l,r) a                   = LiftedTupleRepr (TupleRepr a)

type family LiftedTupleRepr t :: Type
type instance LiftedTupleRepr () = ()
type instance LiftedTupleRepr (b, a) = (LiftedTupleRepr b, Vector' a)

type LiftedArray sh e = Vector' (Array sh e)

instance Arrays t => Arrays (Vector' t) where
  type ArrRepr (Vector' t) = ArrRepr (TupleRepr (Vector' t))
  arrays _ = arrs (prod (Proxy :: Proxy Arrays) (undefined :: Vector' t))
    where
      arrs :: forall a. ProdR Arrays a -> ArraysR (ArrRepr a)
      arrs ProdRunit     = ArraysRunit
      arrs (ProdRsnoc t) = ArraysRpair (ArraysRpair ArraysRunit (arrs t)) (arrays t')
        where t' :: (a ~ (l,r)) => r
              t' = undefined
  flavour _ = case flavour (undefined :: t) of
                ArraysFunit  -> ArraysFtuple
                ArraysFarray -> ArraysFtuple
                ArraysFtuple | ProdRsnoc _ <- prod (Proxy :: Proxy Arrays) (undefined::t)
                             -> ArraysFtuple
                             | otherwise -> error "Absurd"
  --
  fromArr (Vector' vt) = fa (prod (Proxy :: Proxy Arrays) (undefined :: Vector' t)) vt
    where
      fa :: forall a. ProdR Arrays a -> a -> ArrRepr a
      fa ProdRunit     ()    = ()
      fa (ProdRsnoc t) (l,a) = (((), fa t l), fromArr a)
  toArr = Vector' . ta (prod (Proxy :: Proxy Arrays) (undefined :: Vector' t))
    where
      ta :: forall a. ProdR Arrays a -> ArrRepr a -> a
      ta ProdRunit     ()         = ()
      ta (ProdRsnoc t) (((),l),a) = (ta t l, toArr a)

data IsConstrained c where
  IsC :: c => IsConstrained c

type IsTypeableArrRepr t = IsConstrained (Typeable (ArrRepr t))

type IsArraysFlat t = IsConstrained (Arrays (Vector' t))

isTypeableArrRepr :: forall t. Arrays t => {- dummy -} t -> IsTypeableArrRepr (Vector' t)
isTypeableArrRepr _ =
  case flavour (undefined :: t) of
    ArraysFunit  -> IsC
    ArraysFarray -> IsC
    ArraysFtuple | IsC <- isT (prod (Proxy :: Proxy Arrays) (undefined :: Vector' t))
                 -> IsC
  where
    isT :: ProdR Arrays t' -> IsTypeableArrRepr t'
    isT ProdRunit                    = IsC
    isT (ProdRsnoc t) | IsC <- isT t = IsC

isArraysFlat :: forall t. Arrays t => {- dummy -} t -> IsArraysFlat t
isArraysFlat t = case flavour t of
                   ArraysFunit  -> IsC
                   ArraysFtuple | IsC <- isTypeableArrRepr t
                                -> IsC
                   ArraysFarray -> IsC


-- Useful helper-functions (not exported)
-- --------------------------------------

scalar :: Elt a => a -> Scalar a
scalar n = fromList Z [n]

emptyVec :: Elt a => Vector a
emptyVec = fromList (Z :. (0 :: Int)) []

flatten :: Array sh e -> Vector e
flatten (Array sh e) = Array ((), Repr.size sh) e


-- Useful helper-functions for Vector'
-- ----------------------------------

-- Get all the elements. O(1).
--
elements' :: Vector' (Array sh e) -> Vector e
elements' (Vector' (_, elts)) = elts

-- Get all the shapes. O(1).
--
shapes' :: Vector' (Array sh a) -> Vector sh
shapes' (Vector' (((), shapes), _)) = shapes

-- The empty Vector'. O(1).
empty' :: forall a. Arrays a => Vector' a
empty' = Vector' $
  case flavour (undefined :: a) of
    ArraysFunit  -> ((), scalar 0)
    ArraysFarray -> (((), emptyVec), emptyVec)
    ArraysFtuple -> tup (prod (Proxy :: Proxy Arrays) (undefined :: a))
  where
    tup :: forall t. ProdR Arrays t -> LiftedTupleRepr t
    tup ProdRunit = ()
    tup (ProdRsnoc t) = (tup t, empty')

-- Number of arrays in Vector'. O(1).
--
length' :: forall a. Arrays a => Vector' a -> Int
length' (Vector' x) =
  case flavour (undefined :: a) of
    ArraysFunit  | ((), n) <- x
                 -> n ! Z
    ArraysFarray | (((), Array ((), n) _), _) <- x
                 -> n
    ArraysFtuple -> tup (prod (Proxy :: Proxy Arrays) (undefined :: a)) x
  where
    tup :: forall t. ProdR Arrays t -> LiftedTupleRepr t -> Int
    tup ProdRunit () = error "unreachable"
    tup (ProdRsnoc _) (_, b) = length' b

-- Drop a number of arrays from a Vector'.
--
drop' :: forall a. Arrays a
      -- Implementation specific drop for basic vectors:
      => (forall e. Elt e => Int -> Vector e -> Vector e)
      -- Implementation specific segments-to-offsets:
      -> (forall sh. Shape sh => Segments sh -> Vector Int)
      -> Int -> Vector' a -> Vector' a
drop' dropVec s2o k (Vector' x) = Vector' $
  case flavour (undefined :: a) of
    ArraysFunit |  ((), n                         ) <- x
                -> ((), scalar (n ! Z - k `max` 0))
    ArraysFarray | (((), segs), vals) <- x
                 , Array ((), n) _ <- segs
                 , k < n
                 -> let offsets = s2o segs
                        k' = offsets ! (Z :. k)
                    in (((), dropVec k segs), dropVec k' vals)
    ArraysFarray -> (((), emptyVec), emptyVec)
    ArraysFtuple -> tup (prod (Proxy :: Proxy Arrays) (undefined :: a)) x
  where
    tup :: forall t. ProdR Arrays t -> LiftedTupleRepr t -> LiftedTupleRepr t
    tup ProdRunit () = ()
    tup (ProdRsnoc t) (a, b) = (tup t a, drop' dropVec s2o k b)

-- Convert a vector to a Vector' of scalars.
--
vec2Vec' :: Elt e => Vector e -> Vector' (Scalar e)
vec2Vec' v = Vector' (((), undefined), v) -- TODO undefined Vector of Z's?

toList' :: forall a. Arrays a
        -- Implementation-specific fetchAll:
        => (forall sh e. (Shape sh, Elt e) => Segments sh -> Vector e -> [Array sh e])
        -> Vector' a -> [a]
toList' fetchAll (Vector' x) =
  case flavour (undefined :: a) of
    ArraysFunit | ((), n) <- x -> replicate (n ! Z) ()
    ArraysFarray | (((), lens), vals) <- x
                 -> fetchAll lens vals
    ArraysFtuple -> map (toProd (Proxy :: Proxy Arrays)) (tup (prod (Proxy :: Proxy Arrays) (undefined :: a)) x)
  where
    tup :: forall t. ProdR Arrays t -> LiftedTupleRepr t -> [t]
    tup ProdRunit () = repeat ()
    tup (ProdRsnoc t) (a, b) = tup t a `zip` toList' fetchAll b

fromList' :: forall a. Arrays a
          -- Implementation specific concat
          => (forall e. Elt e => [Vector e] -> Vector e)
          -> [a] -> Vector' a
fromList' concat xs = Vector' $
  case flavour (undefined :: a) of
    ArraysFunit -> ((), scalar (length xs))
    ArraysFarray ->
      let segs = map shape xs
          vals = concat (map flatten xs)
      in (((), fromList (Z :. length segs) segs), vals)
    ArraysFtuple -> tup (prod (Proxy :: Proxy Arrays) (undefined :: a)) (map (fromProd (Proxy :: Proxy Arrays)) xs)
  where
    tup :: forall t. ProdR Arrays t -> [t] -> LiftedTupleRepr t
    tup ProdRunit _     = ()
    tup (ProdRsnoc t) a = (tup t (Prelude.map fst a), fromList' concat (map snd a))

{-
map' :: (Arrays a, Arrays b)
     => (forall e. Elt e => [Vector e] -> Vector e)
     -> (forall sh e. (Shape sh, Elt e) => Segments sh -> Vector e -> [Array sh e])
     -> (a -> b) -> Vector' a -> Vector' b
map' concat fetchAll f x = fromList' concat $ map f (toList' fetchAll x)
-}

{-
helper :: forall a r. Arrays a
       => (Scalar Int -> r ())
       -> (forall sh e. (Shape sh, Elt e) => Segments sh -> Vector e -> r (Array sh e))
       -> r ()
       -> (forall t s. r t -> r s -> r (t, s))
       -> (forall t. r (ProdRepr t) -> r t)
       -> Vector' a -> r a
helper units arr unit pair fix (Vector' x) =
  case flavour (undefined :: a) of
    ArraysFunit | ((), n) <- x -> units n
    ArraysFarray | (((), lens), vals) <- x
                 -> arr lens vals
    ArraysFtuple -> fix $ tup (prod (Proxy :: Proxy Arrays) (undefined :: a)) x
  where
    tup :: forall t. ProdR Arrays t -> LiftedTupleRepr t -> r t
    tup ProdRunit () = unit
    tup (ProdRsnoc t) (x, y) = tup t x `pair` helper units arr unit pair fix y
-}

