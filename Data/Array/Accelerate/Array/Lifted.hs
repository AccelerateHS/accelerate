{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
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

  Irregular, IrregularTupleRepr,

  Regular, RegularTupleRepr,

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

newtype Irregular a = Irregular (Irregular' a (ArrRepr a))

type family Irregular' a a' where
  Irregular' ()           ()           = ((),Scalar Int)
  Irregular' (Array sh e) (Array sh e) = (((),Segments sh), Vector e)
  Irregular' a            (l,r)        = IrregularTupleRepr (TupleRepr a)

type family IrregularTupleRepr t where
  IrregularTupleRepr ()     = ()
  IrregularTupleRepr (b, a) = (IrregularTupleRepr b, Irregular a)

instance Arrays a => IsProduct Arrays (Irregular a) where
  type ProdRepr (Irregular a) = Irregular' a (ArrRepr a)
  fromProd _ (Irregular a) = a
  toProd   _               = Irregular
  prod     _ _             =
    case flavour (undefined :: a) of
      ArraysFunit  -> ProdRsnoc ProdRunit
      ArraysFarray -> ProdRsnoc (ProdRsnoc ProdRunit)
      ArraysFtuple -> tup (prod arraysP (undefined :: a))
        where
          tup :: ProdR Arrays t -> ProdR Arrays (IrregularTupleRepr t)
          tup ProdRunit      = ProdRunit
          tup (ProdRsnoc pr) = ProdRsnoc (tup pr)

type instance ArrRepr (Irregular a) = ArrRepr (Irregular' a (ArrRepr a))

instance Arrays a => Arrays (Irregular a) where
  arrays _ = tup (prod arraysP (undefined :: Irregular a))
    where
      tup :: forall t. ProdR Arrays t -> ArraysR (ArrRepr t)
      tup ProdRunit      = ArraysRunit
      tup (ProdRsnoc pr) = ArraysRpair (ArraysRpair ArraysRunit (tup pr)) ar
        where ar :: forall e l. t ~ (l,e) => ArraysR (ArrRepr e)
              ar = arrays (undefined :: e)
  flavour _ = case flavour (undefined :: a) of
    ArraysFunit  -> ArraysFtuple
    ArraysFarray -> ArraysFtuple
    ArraysFtuple ->
      case arrays (undefined :: Irregular a) of
        ArraysRpair _ _ -> ArraysFtuple
        _               -> error "Unreachable"

  toArr a = Irregular (tA (prod arraysP (undefined :: Irregular a)) a)
    where
      tA :: ProdR Arrays t -> ArrRepr t -> t
      tA ProdRunit      ()          = ()
      tA (ProdRsnoc pr) (((),ar),a) = (tA pr ar, toArr a)

  fromArr (Irregular a) = fA (prod arraysP (undefined :: Irregular a)) a
    where
      fA :: ProdR Arrays t -> t -> ArrRepr t
      fA ProdRunit      ()    = ()
      fA (ProdRsnoc pr) (l,a) = (((),fA pr l), fromArr a)

-- Lifted regular arrays.
-- ----------------------
--
-- Similar to Vector' but we assume all sub arrays are the same size, thus there
-- is no need to store all segment descriptors.
--

newtype Regular a = Regular (Regular' a (ArrRepr a))

type family Regular' a a' where
  Regular' ()           ()           = ((),Scalar Int)
  Regular' (Array sh e) (Array sh e) = ((),Array (sh:.Int) e)
  Regular' a            (l,r)        = RegularTupleRepr (TupleRepr a)

type family RegularTupleRepr t where
  RegularTupleRepr ()     = ()
  RegularTupleRepr (b, a) = (RegularTupleRepr b, Regular a)

instance Arrays a => IsProduct Arrays (Regular a) where
  type ProdRepr (Regular a) = Regular' a (ArrRepr a)
  fromProd _ (Regular a) = a
  toProd   _               = Regular
  prod     _ _             =
    case flavour (undefined :: a) of
      ArraysFunit  -> ProdRsnoc ProdRunit
      ArraysFarray -> ProdRsnoc ProdRunit
      ArraysFtuple -> tup (prod arraysP (undefined :: a))
        where
          tup :: ProdR Arrays t -> ProdR Arrays (RegularTupleRepr t)
          tup ProdRunit      = ProdRunit
          tup (ProdRsnoc pr) = ProdRsnoc (tup pr)

type instance ArrRepr (Regular a) = ArrRepr (Regular' a (ArrRepr a))

instance Arrays a => Arrays (Regular a) where
  arrays _ = tup (prod arraysP (undefined :: Regular a))
    where
      tup :: forall t. ProdR Arrays t -> ArraysR (ArrRepr t)
      tup ProdRunit      = ArraysRunit
      tup (ProdRsnoc pr) = ArraysRpair (ArraysRpair ArraysRunit (tup pr)) ar
        where ar :: forall e l. t ~ (l,e) => ArraysR (ArrRepr e)
              ar = arrays (undefined :: e)
  flavour _ = case flavour (undefined :: a) of
    ArraysFunit  -> ArraysFtuple
    ArraysFarray -> ArraysFtuple
    ArraysFtuple ->
      case arrays (undefined :: Regular a) of
        ArraysRpair _ _ -> ArraysFtuple
        _               -> error "Unreachable"

  toArr a = Regular (tA (prod arraysP (undefined :: Regular a)) a)
    where
      tA :: ProdR Arrays t -> ArrRepr t -> t
      tA ProdRunit      ()          = ()
      tA (ProdRsnoc pr) (((),ar),a) = (tA pr ar, toArr a)

  fromArr (Regular a) = fA (prod arraysP (undefined :: Regular a)) a
    where
      fA :: ProdR Arrays t -> t -> ArrRepr t
      fA ProdRunit      ()    = ()
      fA (ProdRsnoc pr) (l,a) = (((),fA pr l), fromArr a)

-- Useful helper-functions (not exported)
-- --------------------------------------

scalar :: Elt a => a -> Scalar a
scalar n = fromList Z [n]

emptyVec :: Elt a => Vector a
emptyVec = fromList (Z :. (0 :: Int)) []

flatten :: Array sh e -> Vector e
flatten (Array sh e) = Array ((), Repr.size sh) e

reshape :: (Shape sh, Shape sh') => sh' -> Array sh e -> Array sh' e
reshape sh' (Array sh e) | size sh' == Repr.size sh
                         = Array (fromElt sh') e
                         | otherwise
                         = $internalError "reshape" "shape mismatch"

arraysP :: Proxy Arrays
arraysP = Proxy


-- Useful helper-functions for Regular
-- ----------------------------------

-- Extract the array out of a regular nested vector.
--
fromRegular :: Regular (Array sh e) -> Array (sh:.Int) e
fromRegular (Regular ((),a)) = a

-- Turn an array into a regular nested vector
--
toRegular :: Array (sh:.Int) e -> Regular (Array sh e)
toRegular = Regular . ((),)

-- Get all the elements. O(1).
--
elements' :: Regular (Array sh e) -> Vector e
elements' = flatten . fromRegular

-- Get all the shapes. O(1).
--
shape' :: Shape sh => Regular (Array sh a) -> sh
shape' = listToShape . init . shapeToList . shape . fromRegular

-- The empty Regular. O(1).
empty' :: forall a. Arrays a => Regular a
empty' =
  case flavour (undefined :: a) of
    ArraysFunit  -> Regular ((),scalar 0)
    ArraysFarray -> newRegularArray 0 empty emptyVec
    ArraysFtuple -> Regular $ tup (prod arraysP (undefined :: a))
  where
    tup :: forall t. ProdR Arrays t -> RegularTupleRepr t
    tup ProdRunit = ()
    tup (ProdRsnoc t) = (tup t, empty')

newRegularArray :: Shape sh => Int -> sh -> Vector e -> Regular (Array sh e)
newRegularArray n sh = toRegular . reshape sh'
  where
    sh' = listToShape (shapeToList sh ++ [n])

-- Number of arrays in Regular. O(1).
--
length' :: forall a. Arrays a => Regular a -> Int
length' (Regular x) =
  case flavour (undefined :: a) of
    ArraysFunit  -> snd x ! Z
    ArraysFarray -> last (shapeToList (shape (snd x)))
    ArraysFtuple -> tup (prod arraysP (undefined :: a)) x
  where
    tup :: forall t. ProdR Arrays t -> RegularTupleRepr t -> Int
    tup ProdRunit       ()     = error "unreachable"
    tup (ProdRsnoc _) (_, b) = length' b

-- Drop a number of arrays from a Regular.
--
drop' :: forall a. Arrays a
      -- Implementation specific drop for basic vectors:
      => (forall e. Elt e => Int -> Vector e -> Vector e)
      -> Int -> Regular a -> Regular a
drop' dropVec k x =
  let l = length' x
  in case flavour (undefined :: a) of
    ArraysFunit -> Regular ((),scalar (l - k `max` 0))
    ArraysFarray | k < l
                 -> let sh = shape' x
                        k' = k * size sh
                    in newRegularArray (l - k) sh (dropVec k' (elements' x))
                 | otherwise -> empty'
    ArraysFtuple | Regular x' <- x
                 -> Regular $ tup (prod arraysP (undefined :: a)) x'
  where
    tup :: forall t. ProdR Arrays t -> RegularTupleRepr t -> RegularTupleRepr t
    tup ProdRunit            () = ()
    tup (ProdRsnoc t') (a, b) = (tup t' a, drop' dropVec k b)

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
    ArraysFtuple | Regular x' <- x
                 -> let proxy = arraysP
                    in map (toProd proxy)
                           (tup (prod proxy (undefined :: a)) x')
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
    ArraysFunit -> Regular ((),scalar (length xs))
    ArraysFarray | not (null xs) ->
      let sh   = shape (head xs)
          vals = concat (map flatten xs)
      in newRegularArray (length xs) sh vals
                 | otherwise -> empty'
    ArraysFtuple -> Regular $ tup (prod arraysP (undefined :: a)) (map (fromProd arraysP) xs)
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
    ArraysFtuple | Regular x' <- x
                 -> toProd arraysP (tup (prod arraysP (undefined :: a)) x')
  where
    tup :: forall t. ProdR Arrays t -> RegularTupleRepr t -> t
    tup ProdRunit () = ()
    tup (ProdRsnoc t) (a, b) = (tup t a, the' b)

unit' :: forall a. Arrays a
      => a -> Regular a
unit' xs =
  case flavour (undefined :: a) of
    ArraysFunit  -> Regular ((),scalar 1)
    ArraysFarray ->
      let seg = shape xs
          vals = flatten xs
      in newRegularArray 1 seg vals
    ArraysFtuple -> Regular $ tup (prod arraysP (undefined :: a)) (fromProd arraysP xs)
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
    ArraysFtuple -> fix $ tup (prod arraysP (undefined :: a)) x
  where
    tup :: forall t. ProdR Arrays t -> LiftedTupleRepr t -> r t
    tup ProdRunit () = unit
    tup (ProdRsnoc t) (x, y) = tup t x `pair` helper units arr unit pair fix y
-}
