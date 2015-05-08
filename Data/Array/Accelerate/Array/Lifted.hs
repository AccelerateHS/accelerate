{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Lifted
-- Copyright   : [2012..2013] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell, Robert Clifton-Everest
-- License     : BSD3
--
-- Maintainer  : Robert Clifton-Everest <robertce@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lifted array representation. Vector of arrays represented as segmented vectors.
--

module Data.Array.Accelerate.Array.Lifted (

  Vector'(..), LiftedArray,

  LiftedTupleRepr,

  Regular(..), RegularTupleRepr,

  IsConstrained(..),

  isArraysFlat,

  elements', shape', empty', length', drop', vec2Regular, fromList', toList', unit', the'

) where

import Prelude                                                  hiding ( concat )
import Data.Typeable

-- friends
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import qualified Data.Array.Accelerate.Array.Representation     as Repr

-- Lifted arrays
-- ----------------
--
-- We specify a special new type of surface tuple to represent the lifted version of members of the
-- `Arrays' class. We do this in order to convince the type checker that the lifted arrays or tuples
-- of arrays, are still members of the 'Arrays' class.

newtype Vector' a = Vector' (LiftedRepr (ArrRepr a) a)
  deriving Typeable

type family LiftedRepr r a where
  LiftedRepr ()     ()                 = ((),Scalar Int)
  LiftedRepr (Array sh e) (Array sh e) = (((),Segments sh), Vector e)
  LiftedRepr (l,r) a                   = LiftedTupleRepr (TupleRepr a)

type family LiftedTupleRepr t :: *
type instance LiftedTupleRepr () = ()
type instance LiftedTupleRepr (b, a) = (LiftedTupleRepr b, Vector' a)

type LiftedArray sh e = Vector' (Array sh e)

instance Arrays t => IsProduct Arrays (Vector' t) where
  type ProdRepr (Vector' t) = LiftedRepr (ArrRepr t) t
  fromProd _ (Vector' t) = t
  toProd _ = Vector'
  prod _ _ = case flavour (undefined :: t) of
                ArraysFunit  -> ProdRsnoc ProdRunit
                ArraysFarray -> ProdRsnoc (ProdRsnoc ProdRunit)
                ArraysFtuple -> tup $ prod (Proxy :: Proxy Arrays) (undefined :: t)
    where
      tup :: forall a. ProdR Arrays a -> ProdR Arrays (LiftedTupleRepr a)
      tup ProdRunit     = ProdRunit
      tup (ProdRsnoc t) = swiz
        where
          swiz :: forall l r. (a ~ (l,r), Arrays r) => ProdR Arrays (LiftedTupleRepr a)
          swiz | IsC <- isArraysFlat (undefined :: r)
               = ProdRsnoc (tup t)


type instance ArrRepr (Vector' a) = ArrRepr (TupleRepr (Vector' a))


instance (Arrays t, Typeable (ArrRepr (Vector' t))) => Arrays (Vector' t) where
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

-- Lifted regular arrays.
-- ----------------------
--
-- Similar to Vector' but we assume all sub arrays are the same size, thus there
-- is no need to store all segment descriptors.
--

newtype Regular a = Regular (RegularRepr (ArrRepr a) a)
  deriving Typeable

type family RegularRepr r a where
  RegularRepr ()     ()                 = ((),Scalar Int)
  RegularRepr (Array sh e) (Array sh e) = ((((),Scalar Int), Scalar sh), Vector e)
  RegularRepr (l,r) a                   = RegularTupleRepr (TupleRepr a)

type family RegularTupleRepr t :: *
type instance RegularTupleRepr () = ()
type instance RegularTupleRepr (b, a) = (RegularTupleRepr b, Regular a)

instance Arrays t => IsProduct Arrays (Regular t) where
  type ProdRepr (Regular t) = RegularRepr (ArrRepr t) t
  fromProd _ (Regular t) = t
  toProd _ = Regular
  prod _ _ = case flavour (undefined :: t) of
                ArraysFunit  -> ProdRsnoc ProdRunit
                ArraysFarray -> ProdRsnoc (ProdRsnoc (ProdRsnoc ProdRunit))
                ArraysFtuple -> tup $ prod (Proxy :: Proxy Arrays) (undefined :: t)
    where
      tup :: forall a. ProdR Arrays a -> ProdR Arrays (RegularTupleRepr a)
      tup ProdRunit     = ProdRunit
      tup (ProdRsnoc t) = swiz
        where
          swiz :: forall l r. (a ~ (l,r), Arrays r) => ProdR Arrays (RegularTupleRepr a)
          swiz | IsC <- isArraysFlat (undefined :: r)
               = ProdRsnoc (tup t)

type instance ArrRepr (Regular a) = ArrRepr (TupleRepr (Regular a))


instance (Arrays t, Typeable (ArrRepr (Regular t))) => Arrays (Regular t) where
  arrays _ = arrs (prod (Proxy :: Proxy Arrays) (undefined :: Regular t))
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
  fromArr (Regular vt) = fa (prod (Proxy :: Proxy Arrays) (undefined :: Regular t)) vt
    where
      fa :: forall a. ProdR Arrays a -> a -> ArrRepr a
      fa ProdRunit     ()    = ()
      fa (ProdRsnoc t) (l,a) = (((), fa t l), fromArr a)
  toArr = Regular . ta (prod (Proxy :: Proxy Arrays) (undefined :: Regular t))
    where
      ta :: forall a. ProdR Arrays a -> ArrRepr a -> a
      ta ProdRunit     ()         = ()
      ta (ProdRsnoc t) (((),l),a) = (ta t l, toArr a)

data IsConstrained c where
  IsC :: c => IsConstrained c

type IsTypeableArrRepr t = IsConstrained (Typeable (ArrRepr t))

type IsArraysFlat t = IsConstrained (Arrays (Vector' t), Arrays (Regular t))

isTypeableVector' :: forall t. Arrays t => {- dummy -} t -> IsTypeableArrRepr (Vector' t)
isTypeableVector' _ =
  case flavour (undefined :: t) of
    ArraysFunit  -> IsC
    ArraysFarray -> IsC
    ArraysFtuple | IsC <- isT (prod (Proxy :: Proxy Arrays) (undefined :: Vector' t))
                 -> IsC
  where
    isT :: ProdR Arrays t' -> IsTypeableArrRepr t'
    isT ProdRunit                    = IsC
    isT (ProdRsnoc t) | IsC <- isT t = IsC

isTypeableRegular :: forall t. Arrays t => {- dummy -} t -> IsTypeableArrRepr (Regular t)
isTypeableRegular _ =
  case flavour (undefined :: t) of
    ArraysFunit  -> IsC
    ArraysFarray -> IsC
    ArraysFtuple | IsC <- isT (prod (Proxy :: Proxy Arrays) (undefined :: Regular t))
                 -> IsC
  where
    isT :: ProdR Arrays t' -> IsTypeableArrRepr t'
    isT ProdRunit                    = IsC
    isT (ProdRsnoc t) | IsC <- isT t = IsC

isArraysFlat :: forall t. Arrays t => {- dummy -} t -> IsArraysFlat t
isArraysFlat t = case flavour t of
                   ArraysFunit  -> IsC
                   ArraysFtuple | IsC <- isTypeableVector' t
                                , IsC <- isTypeableRegular t
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


-- Useful helper-functions for Regular
-- ----------------------------------

-- Get all the elements. O(1).
--
elements' :: Regular (Array sh e) -> Vector e
elements' (Regular (_, elts)) = elts

-- Get all the shapes. O(1).
--
shape' :: Elt sh => Regular (Array sh a) -> sh
shape' (Regular ((_, shape), _)) = shape ! Z

-- The empty Regular. O(1).
empty' :: forall a. Arrays a => Regular a
empty' =
  case flavour (undefined :: a) of
    ArraysFunit  -> Regular ((), scalar 0)
    ArraysFarray -> newRegularArray 0 emptyS emptyVec
    ArraysFtuple -> Regular $ tup (prod (Proxy :: Proxy Arrays) (undefined :: a))
  where
    tup :: forall t. ProdR Arrays t -> RegularTupleRepr t
    tup ProdRunit = ()
    tup (ProdRsnoc t) = (tup t, empty')

newRegularArray :: Elt sh => Int -> sh -> Vector e -> Regular (Array sh e)
newRegularArray n sh vals = Regular ((((),scalar n), scalar sh), vals)

-- Number of arrays in Regular. O(1).
--
length' :: forall a. Arrays a => Regular a -> Int
length' (Regular x) =
  case flavour (undefined :: a) of
    ArraysFunit  | ((), n) <- x
                 -> n ! Z
    ArraysFarray | ((((), n), _), _) <- x
                 -> n ! Z
    ArraysFtuple -> tup (prod (Proxy :: Proxy Arrays) (undefined :: a)) x
  where
    tup :: forall t. ProdR Arrays t -> RegularTupleRepr t -> Int
    tup ProdRunit () = error "unreachable"
    tup (ProdRsnoc _) (_, b) = length' b

-- Drop a number of arrays from a Regular.
--
drop' :: forall a. Arrays a
      -- Implementation specific drop for basic vectors:
      => (forall e. Elt e => Int -> Vector e -> Vector e)
      -> Int -> Regular a -> Regular a
drop' dropVec k x =
  case flavour (undefined :: a) of
    ArraysFunit -> Regular ((), scalar (length' x - k `max` 0))
    ArraysFarray | k < length' x
                 -> let sh = shape' x
                        k' = k * size sh
                    in newRegularArray (length' x - k) sh (dropVec k' (elements' x))
                 | otherwise -> empty'
    ArraysFtuple -> Regular $
                      tup (prod (Proxy :: Proxy Arrays) (undefined :: a))
                          (fromProd (Proxy :: Proxy Arrays) x)
  where
    tup :: forall t. ProdR Arrays t -> RegularTupleRepr t -> RegularTupleRepr t
    tup ProdRunit () = ()
    tup (ProdRsnoc t) (a, b) = (tup t a, drop' dropVec k b)

-- Convert a vector to a Regular of scalars.
--
vec2Regular :: Elt e => Vector e -> Regular (Scalar e)
vec2Regular v = newRegularArray (size (shape v)) Z v

toList' :: forall a. Arrays a
        -- Implementation-specific fetchAll:
        => (forall sh e. (Shape sh, Elt e) => sh -> Vector e -> [Array sh e])
        -> Regular a -> [a]
toList' fetchAll x =
  case flavour (undefined :: a) of
    ArraysFunit  -> replicate (length' x) ()
    ArraysFarray -> fetchAll (shape' x) (elements' x)
    ArraysFtuple -> let proxy = (Proxy :: Proxy Arrays)
                    in map (toProd proxy)
                           (tup (prod proxy (undefined :: a)) (fromProd proxy x))
  where
    tup :: forall t. ProdR Arrays t -> RegularTupleRepr t -> [t]
    tup ProdRunit () = repeat ()
    tup (ProdRsnoc t) (a, b) = tup t a `zip` toList' fetchAll b

fromList' :: forall a. Arrays a
          -- Implementation specific concat
          => (forall e. Elt e => [Vector e] -> Vector e)
          -> [a] -> Regular a
fromList' concat xs =
  case flavour (undefined :: a) of
    ArraysFunit -> Regular ((), scalar (length xs))
    ArraysFarray | not (null xs) ->
      let sh   = shape (head xs)
          vals = concat (map flatten xs)
      in newRegularArray (length xs) sh vals
                 | otherwise -> empty'
    ArraysFtuple -> Regular $ tup (prod (Proxy :: Proxy Arrays) (undefined :: a)) (map (fromProd (Proxy :: Proxy Arrays)) xs)
  where
    tup :: forall t. ProdR Arrays t -> [t] -> RegularTupleRepr t
    tup ProdRunit _     = ()
    tup (ProdRsnoc t) a = (tup t (Prelude.map fst a), fromList' concat (map snd a))

the' :: forall a. Arrays a
     => Regular a -> a
the' x =
  case flavour (undefined :: a) of
    ArraysFunit  -> ()
    ArraysFarray | length' x == 1
                 , Array _ vals <- elements' x
                 -> Array (fromElt (shape' x)) vals
                 | otherwise
                 -> $internalError "the'" "non-unit regular nested array"
    ArraysFtuple -> let proxy = (Proxy :: Proxy Arrays)
                    in toProd proxy (tup (prod proxy (undefined :: a)) (fromProd proxy x))
  where
    tup :: forall t. ProdR Arrays t -> RegularTupleRepr t -> t
    tup ProdRunit () = ()
    tup (ProdRsnoc t) (a, b) = (tup t a, the' b)

unit' :: forall a. Arrays a
      => a -> Regular a
unit' xs =
  case flavour (undefined :: a) of
    ArraysFunit -> Regular ((), scalar 1)
    ArraysFarray ->
      let seg = shape xs
          vals = flatten xs
      in newRegularArray 1 seg vals
    ArraysFtuple -> Regular $ tup (prod (Proxy :: Proxy Arrays) (undefined :: a)) (fromProd (Proxy :: Proxy Arrays) xs)
  where
    tup :: forall t. ProdR Arrays t -> t -> RegularTupleRepr t
    tup ProdRunit _          = ()
    tup (ProdRsnoc t) (a, b) = (tup t a, unit' b)

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
