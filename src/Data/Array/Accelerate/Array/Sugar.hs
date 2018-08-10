{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
#if __GLASGOW_HASKELL__ <= 708
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Sugar
-- Copyright   : [2008..2017] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2017] Trevor L. McDonell
--               [2013..2017] Robert Clifton-Everest
--               [2014..2014] Frederik M. Madsen
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Array.Sugar (

  -- * Array representation
  Array(..), Scalar, Vector, Matrix, Segments,
  Arrays(..), ArraysR(..),

  -- * Class of supported surface element types and their mapping to representation types
  Elt(..),

  -- * Derived functions
  liftToElt, liftToElt2, sinkFromElt, sinkFromElt2,

  -- * Array shapes
  DIM0, DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7, DIM8, DIM9,

  -- * Array indexing and slicing
  Z(..), (:.)(..), All(..), Split(..), Any(..), Divide(..), Shape(..), Slice(..), Division(..),

  -- * Array shape query, indexing, and conversions
  shape, reshape, (!), (!!), allocateArray, fromFunction, fromFunctionM, fromList, toList, concatVectors,

  -- * Tuples
  TupleR, TupleRepr, tuple,
  Tuple(..), IsTuple, fromTuple, toTuple,
  Atuple(..), IsAtuple, fromAtuple, toAtuple,

  -- * Miscellaneous
  showShape, Foreign(..), sliceShape, enumSlices,

) where

-- standard library
import Control.DeepSeq
import Data.Typeable
import System.IO.Unsafe                                         ( unsafePerformIO )
import Language.Haskell.TH                                      hiding ( Foreign )
import Prelude                                                  hiding ( (!!) )
import qualified Data.Vector.Unboxed                            as U

import GHC.Exts                                                 ( IsList )
import GHC.Generics
import GHC.TypeLits
import qualified GHC.Exts                                       as GHC

-- friends
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Orphans                            ()
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Representation     as Repr

-- $setup
-- >>> :seti -XOverloadedLists

-- Surface types representing array indices and slices
-- ---------------------------------------------------
--
-- Array indices are snoc type lists. That is, they're backwards -- the
-- end-of-list token, 'Z', occurs first.  For example, the type of a rank-2
-- array index is @Z :. Int :. Int@.
--
-- In Accelerate the rightmost dimension is the /fastest varying/ or innermost.

-- | Rank-0 index
--
data Z = Z
  deriving (Typeable, Show, Eq)

-- | Increase an index rank by one dimension. The ':.' operator is used to
-- construct both values and types.
--
infixl 3 :.
data tail :. head = !tail :. !head
  deriving (Typeable, Eq)

-- We don't we use a derived Show instance for (:.) because this will insert
-- parenthesis to demonstrate which order the operator is applied, i.e.:
--
--   (((Z :. z) :. y) :. x)
--
-- This is fine, but I find it a little unsightly. Instead, we drop all
-- parenthesis and just display the shape thus:
--
--   Z :. z :. y :. x
--
-- and then require the down-stream user to wrap the whole thing in parentheses.
-- This works fine for the most important case, which is to show Acc and Exp
-- expressions via the pretty printer, although Show-ing a Shape directly
-- results in no parenthesis being displayed.
--
-- One way around this might be to have specialised instances for DIM1, DIM2,
-- etc.
--
instance (Show sh, Show sz) => Show (sh :. sz) where
  show (sh :. sz) = show sh ++ " :. " ++ show sz

-- | Marker for entire dimensions in 'Data.Array.Accelerate.Language.slice' and
-- 'Data.Array.Accelerate.Language.replicate' descriptors.
--
-- Occurrences of 'All' indicate the dimensions into which the array's existing
-- extent will be placed unchanged.
--
-- See 'Data.Array.Accelerate.Language.slice' and
-- 'Data.Array.Accelerate.Language.replicate' for examples.
--
data All = All
  deriving (Typeable, Show, Eq)

-- | Marker for arbitrary dimensions in 'Data.Array.Accelerate.Language.slice'
-- and 'Data.Array.Accelerate.Language.replicate' descriptors.
--
-- 'Any' can be used in the leftmost position of a slice instead of 'Z',
-- indicating that any dimensionality is admissible in that position.
--
-- See 'Data.Array.Accelerate.Language.slice' and
-- 'Data.Array.Accelerate.Language.replicate' for examples.
--
data Any sh = Any
  deriving (Typeable, Show, Eq)

-- | Marker for splitting along an entire dimension in division descriptors.
--
-- For example, when used in a division descriptor passed to
-- 'Data.Array.Accelerate.toSeq', a `Split` indicates that the array should be
-- divided along this dimension forming the elements of the output sequence.
--
data Split = Split
  deriving (Typeable, Show, Eq)

-- | Marker for arbitrary shapes in slices descriptors, where it is desired to
-- split along an unknown number of dimensions.
--
-- For example, in the following definition, 'Divide' matches against any shape
-- and flattens everything but the innermost dimension.
--
-- > vectors :: (Shape sh, Elt e) => Acc (Array (sh:.Int) e) -> Seq [Vector e]
-- > vectors = toSeq (Divide :. All)
--
data Divide sh = Divide
  deriving (Typeable, Show, Eq)


-- Scalar elements
-- ---------------

-- | The 'Elt' class characterises the allowable array element types, and hence
-- the types which can appear in scalar Accelerate expressions of type
-- 'Data.Array.Accelerate.Exp'.
--
-- Accelerate arrays consist of simple atomic types as well as nested tuples
-- thereof, stored efficiently in memory as consecutive unpacked elements
-- without pointers. It roughly consists of:
--
--  * Signed and unsigned integers (8, 16, 32, and 64-bits wide)
--  * Floating point numbers (half, single, and double precision)
--  * 'Char'
--  * 'Bool'
--  * ()
--  * Shapes formed from 'Z' and (':.')
--  * Nested tuples of all of these, currently up to 15-elements wide
--
-- Adding new instances for 'Elt' consists of explaining to Accelerate how to
-- map between your data type and a (tuple of) primitive values. For examples
-- see:
--
--  * "Data.Array.Accelerate.Data.Complex"
--  * "Data.Array.Accelerate.Data.Monoid"
--  * <https://hackage.haskell.org/package/linear-accelerate linear-accelerate>
--  * <https://hackage.haskell.org/package/colour-accelerate colour-accelerate>
--
-- For simple product types it is possible to derive 'Elt' automatically, for
-- example:
--
-- > data Point = Point Int Float
-- >   deriving (Show, Generic, Elt)
--
class (Show a, Typeable a, Typeable (EltRepr a), ArrayElt (EltRepr a)) => Elt a where
  -- | Type representation mapping, which explains how to convert a type from
  -- the surface type into the internal representation type consisting only of
  -- simple primitive types, unit '()', and pair '(,)'.
  --
  type EltRepr a :: *
  type EltRepr a = GEltRepr () (Rep a)
  --
  eltType  :: TupleType (EltRepr a)
  fromElt  :: a -> EltRepr a
  toElt    :: EltRepr a -> a

  {-# INLINE eltType #-}
  default eltType
    :: (GElt (Rep a), EltRepr a ~ GEltRepr () (Rep a))
    => TupleType (EltRepr a)
  eltType = geltType @(Rep a) TypeRunit

  {-# INLINE fromElt #-}
  default fromElt
    :: (Generic a, GElt (Rep a), EltRepr a ~ GEltRepr () (Rep a))
    => a
    -> EltRepr a
  fromElt = gfromElt () . from

  {-# INLINE toElt #-}
  default toElt
    :: (Generic a, GElt (Rep a), EltRepr a ~ GEltRepr () (Rep a))
    => EltRepr a
    -> a
  toElt = to . snd . gtoElt @(Rep a) @()


class GElt (f :: * -> *) where
  type GEltRepr t f
  geltType :: TupleType t -> TupleType (GEltRepr t f)
  gfromElt :: t -> f a -> GEltRepr t f
  gtoElt   :: GEltRepr t f -> (t, f a)

instance GElt U1 where
  type GEltRepr t U1 = t
  geltType t    =  t
  gfromElt t U1 =  t
  gtoElt   t    = (t, U1)

instance GElt a => GElt (M1 i c a) where
  type GEltRepr t (M1 i c a) = GEltRepr t a
  geltType          = geltType @a
  gfromElt t (M1 x) = gfromElt t x
  gtoElt         x  = let (t, x1) = gtoElt x in (t, M1 x1)

instance Elt a => GElt (K1 i a) where
  type GEltRepr t (K1 i a) = (t, EltRepr a)
  geltType t        = TypeRpair t (eltType @a)
  gfromElt t (K1 x) = (t, fromElt x)
  gtoElt     (t, x) = (t, K1 (toElt x))

instance (GElt a, GElt b) => GElt (a :*: b) where
  type GEltRepr t (a :*: b) = GEltRepr (GEltRepr t a) b
  geltType             = geltType @b . geltType @a
  gfromElt t (a :*: b) = gfromElt (gfromElt t a) b
  gtoElt t =
    let (t1, b) = gtoElt t
        (t2, a) = gtoElt t1
    in
    (t2, a :*: b)


-- Note: [Deriving Elt]
--
-- We can't use the cunning generalised newtype deriving mechanism, because the
-- generated 'eltType' function does not type check. For example, it will
-- generate the following implementation for 'CShort':
--
-- > eltType
-- >   = coerce
-- >       @(TupleType (EltRepr Int16))
-- >       @(TupleType (EltRepr CShort))
-- >       (eltType :: TupleType (EltRepr CShort))
--
-- Which yields the error "couldn't match type type 'EltRepr a0' with 'Int16'".
-- Since this function returns a type family type, the type signature on the
-- result is not enough to fix the type 'a'. Instead, we require the use of
-- (visible) type applications:
--
-- > eltType
-- >   = coerce
-- >       @(TupleType (EltRepr Int16))
-- >       @(TupleType (EltRepr CShort))
-- >       (eltType @(EltRepr CShort))
--
-- Note that this does not affect deriving instances via 'Generic'
--
-- Instances for basic types are generated at the end of this module.
--

instance Elt () where
  type EltRepr () = ()
  eltType   = TypeRunit
  fromElt   = id
  toElt     = id

instance Elt Z where
  type EltRepr Z = ()
  eltType    = TypeRunit
  fromElt Z  = ()
  toElt ()   = Z

instance (Elt t, Elt h) => Elt (t:.h) where
  type EltRepr (t:.h) = (EltRepr t, EltRepr h)
  eltType         = TypeRpair (eltType @t) (eltType @h)
  fromElt (t:.h)  = (fromElt t, fromElt h)
  toElt (t, h)    = toElt t :. toElt h

instance Elt All where
  type EltRepr All = ()
  eltType       = TypeRunit
  fromElt All   = ()
  toElt ()      = All

instance Elt (Any Z) where
  type EltRepr (Any Z) = ()
  eltType       = TypeRunit
  fromElt _     = ()
  toElt _       = Any

instance Shape sh => Elt (Any (sh:.Int)) where
  type EltRepr (Any (sh:.Int)) = (EltRepr (Any sh), ())
  eltType       = TypeRpair (eltType @(Any sh)) TypeRunit
  fromElt _     = (fromElt (Any @sh), ())
  toElt _       = Any

instance (Elt a, Elt b) => Elt (a, b)
instance (Elt a, Elt b, Elt c) => Elt (a, b, c)
instance (Elt a, Elt b, Elt c, Elt d) => Elt (a, b, c, d)
instance (Elt a, Elt b, Elt c, Elt d, Elt e) => Elt (a, b, c, d, e)
instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f) => Elt (a, b, c, d, e, f)
instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
  => Elt (a, b, c, d, e, f, g)
instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
  => Elt (a, b, c, d, e, f, g, h)
instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
  => Elt (a, b, c, d, e, f, g, h, i)
instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j)
  => Elt (a, b, c, d, e, f, g, h, i, j)
instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k)
  => Elt (a, b, c, d, e, f, g, h, i, j, k)
instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l)
  => Elt (a, b, c, d, e, f, g, h, i, j, k, l)
instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m)
  => Elt (a, b, c, d, e, f, g, h, i, j, k, l, m)
instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n)
  => Elt (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n, Elt o)
  => Elt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n, Elt o, Elt p)
  => Elt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

--  Convenience functions
--
singletonScalarType :: IsScalar a => TupleType a
singletonScalarType = TypeRscalar scalarType

{-# INLINE liftToElt #-}
liftToElt :: (Elt a, Elt b)
          => (EltRepr a -> EltRepr b)
          -> (a -> b)
liftToElt f = toElt . f . fromElt

{-# INLINE liftToElt2 #-}
liftToElt2 :: (Elt a, Elt b, Elt c)
           => (EltRepr a -> EltRepr b -> EltRepr c)
           -> (a -> b -> c)
liftToElt2 f x y = toElt $ f (fromElt x) (fromElt y)

{-# INLINE sinkFromElt #-}
sinkFromElt :: (Elt a, Elt b)
            => (a -> b)
            -> (EltRepr a -> EltRepr b)
sinkFromElt f = fromElt . f . toElt

{-# INLINE sinkFromElt2 #-}
sinkFromElt2 :: (Elt a, Elt b, Elt c)
             => (a -> b -> c)
             -> (EltRepr a -> EltRepr b -> EltRepr c)
sinkFromElt2 f x y = fromElt $ f (toElt x) (toElt y)

-- {-# RULES
-- "fromElt/toElt" forall e. fromElt (toElt e) = e
-- "toElt/fromElt" forall e. toElt (fromElt e) = e
-- #-}


-- Foreign functions
-- -----------------

-- Class for backends to choose their own representation of foreign functions.
-- By default it has no instances. If a backend wishes to have an FFI it must
-- provide an instance.
--
class Typeable asm => Foreign asm where

  -- Backends should be able to produce a string representation of the foreign
  -- function for pretty printing, typically the name of the function.
  strForeign :: asm args -> String
  strForeign _ = "<foreign>"

  -- Backends which want to support compile-time embedding must be able to lift
  -- the foreign function into Template Haskell
  liftForeign :: asm args -> Q (TExp (asm args))
  liftForeign _ = $internalError "liftForeign" "not supported by this backend"


-- Tuple representation
-- --------------------

-- |The tuple representation is equivalent to the product representation.
--
type TupleRepr a = ProdRepr a
type TupleR a    = ProdR Elt a
type IsTuple     = IsProduct Elt
type IsAtuple    = IsProduct Arrays

-- |We represent tuples as heterogeneous lists, typed by a type list.
--
data Tuple c t where
  NilTup  ::                              Tuple c ()
  SnocTup :: Elt t => Tuple c s -> c t -> Tuple c (s, t)

-- TLM: It is irritating that we need a separate data type for tuples of scalars
--   vs. arrays, purely to carry the class constraint.
--
-- | Tuples of Arrays.  Note that this carries the `Arrays` class
--   constraint rather than `Elt` in the case of tuples of scalars.
--
data Atuple c t where
  NilAtup  ::                                  Atuple c ()
  SnocAtup :: Arrays a => Atuple c s -> c a -> Atuple c (s, a)

-- |Tuple reification
--
tuple :: forall tup. IsTuple tup => TupleR (TupleRepr tup)
tuple = prod @Elt @tup

fromTuple :: IsTuple tup => tup -> TupleRepr tup
fromTuple = fromProd @Elt

toTuple :: IsTuple tup => TupleRepr tup -> tup
toTuple = toProd @Elt

fromAtuple :: IsAtuple tup => tup -> TupleRepr tup
fromAtuple = fromProd @Arrays

toAtuple :: IsAtuple tup => TupleRepr tup -> tup
toAtuple = toProd @Arrays


-- Arrays
-- ------

-- | The 'Arrays' class characterises the types which can appear in collective
-- Accelerate computations of type 'Data.Array.Accelerate.Acc'.
--
-- 'Arrays' consists of nested tuples of individual 'Array's, currently up to
-- 16-elements wide. Accelerate computations can thereby return multiple
-- results.
--
class (Typeable a, Typeable (ArrRepr a)) => Arrays a where
  -- | Type representation mapping, which explains how to convert from the
  -- surface type into the internal representation type, which consists only of
  -- 'Array', and '()' and '(,)' as type-level nil and snoc.
  --
  type ArrRepr a :: *
  type ArrRepr a = GArrRepr () (Rep a)

  arrays   :: ArraysR (ArrRepr a)
  toArr    :: ArrRepr  a -> a
  fromArr  :: a -> ArrRepr  a

  {-# INLINE arrays #-}
  default arrays
    :: (GArrays (Rep a), ArrRepr a ~ GArrRepr () (Rep a))
    => ArraysR (ArrRepr a)
  arrays = garrays @(Rep a) ArraysRunit

  {-# INLINE toArr #-}
  default toArr
    :: (Generic a, GArrays (Rep a), ArrRepr a ~ GArrRepr () (Rep a))
    => ArrRepr a -> a
  toArr = to . snd . gtoArr @(Rep a) @()

  {-# INLINE fromArr #-}
  default fromArr
    :: (Generic a, GArrays (Rep a), ArrRepr a ~ GArrRepr () (Rep a))
    => a -> ArrRepr a
  fromArr = (`gfromArr` ()) . from

  -- flavour :: ArraysFlavour a
  -- default flavour
  --   :: (Generic a, GArrays (Rep a), GArrFlav (Rep a) ~ a, ArrRepr a ~ GArrRepr () (Rep a))
  --   => a -> ArraysFlavour a
  -- flavour _ = gflavour @(Rep a)


class GArrays (f :: * -> *) where
  type GArrRepr t f
  garrays  :: ArraysR t -> ArraysR (GArrRepr t f)
  gfromArr :: f a -> t -> GArrRepr t f
  gtoArr   :: GArrRepr t f -> (t, f a)

instance GArrays U1 where
  type GArrRepr t U1 = t
  garrays       =  id
  gfromArr U1   =  id
  gtoArr      t = (t, U1)

instance GArrays a => GArrays (M1 i c a) where
  type GArrRepr t (M1 i c a) = GArrRepr t a
  garrays         = garrays @a
  gfromArr (M1 x) = gfromArr x
  gtoArr       x  = let (t, x1) = gtoArr x in (t, M1 x1)

instance Arrays a => GArrays (K1 i a) where
  type GArrRepr t (K1 i a) = (t, ArrRepr a)
  garrays         t = ArraysRpair t (arrays @a)
  gfromArr (K1 x) t = (t, fromArr x)
  gtoArr   (t, x)   = (t, K1 (toArr x))

instance (GArrays a, GArrays b) => GArrays (a :*: b) where
  type GArrRepr t (a :*: b) = GArrRepr (GArrRepr t a) b
  garrays            = garrays @b . garrays @a
  gfromArr (a :*: b) = gfromArr b . gfromArr a
  gtoArr t =
    let (t1, b) = gtoArr t
        (t2, a) = gtoArr t1
    in
    (t2, a :*: b)


instance Arrays () where
  type ArrRepr () = ()
  arrays  = ArraysRunit
  fromArr = id
  toArr   = id

instance (Shape sh, Elt e) => Arrays (Array sh e) where
  type ArrRepr (Array sh e) = Array sh e
  arrays  = ArraysRarray
  fromArr = id
  toArr   = id

instance (Arrays a, Arrays b) => Arrays (a, b)
instance (Arrays a, Arrays b, Arrays c) => Arrays (a, b, c)
instance (Arrays a, Arrays b, Arrays c, Arrays d) => Arrays (a, b, c, d)
instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e) => Arrays (a, b, c, d, e)
instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f)
  => Arrays (a, b, c, d, e, f)
instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g)
  => Arrays (a, b, c, d, e, f, g)
instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h)
  => Arrays (a, b, c, d, e, f, g, h)
instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i)
  => Arrays (a, b, c, d, e, f, g, h, i)
instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j)
  => Arrays (a, b, c, d, e, f, g, h, i, j)
instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k)
  => Arrays (a, b, c, d, e, f, g, h, i, j, k)
instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l)
  => Arrays (a, b, c, d, e, f, g, h, i, j, k, l)
instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m)
  => Arrays (a, b, c, d, e, f, g, h, i, j, k, l, m)
instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m, Arrays n)
  => Arrays (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m, Arrays n, Arrays o)
  => Arrays (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m, Arrays n, Arrays o, Arrays p)
  => Arrays (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)


-- Array type reification
--
data ArraysR arrs where
  ArraysRunit  ::                                   ArraysR ()
  ArraysRarray :: (Shape sh, Elt e) =>              ArraysR (Array sh e)
  ArraysRpair  :: ArraysR arrs1 -> ArraysR arrs2 -> ArraysR (arrs1, arrs2)

-- data ArraysFlavour arrs where
--   ArraysFunit  ::                                          ArraysFlavour ()
--   ArraysFarray :: (Shape sh, Elt e)                     => ArraysFlavour (Array sh e)
--   ArraysFtuple :: (IsAtuple arrs, ArrRepr arrs ~ (l,r)) => ArraysFlavour arrs

-- {-# RULES
-- "fromArr/toArr" forall a. fromArr (toArr a) = a
-- "toArr/fromArr" forall a. toArr (fromArr a) = a
-- #-}


-- | Dense, regular, multi-dimensional arrays.
--
-- The 'Array' is the core computational unit of Accelerate; all programs in
-- Accelerate take zero or more arrays as input and produce one or more arrays
-- as output. The 'Array' type has two type parameters:
--
--  * /sh/: is the shape of the array, tracking the dimensionality and extent of
--    each dimension of the array; for example, 'DIM1' for one-dimensional
--    'Vector's, 'DIM2' for two-dimensional matrices, and so on.
--  * /e/: represents the type of each element of the array; for example,
--    'Int', 'Float', et cetera.
--
-- Array data is store unboxed in an unzipped struct-of-array representation.
-- Elements are laid out in row-major order (the right-most index of a 'Shape'
-- is the fastest varying). The allowable array element types are members of the
-- 'Elt' class, which roughly consists of:
--
--  * Signed and unsigned integers (8, 16, 32, and 64-bits wide).
--  * Floating point numbers (single and double precision)
--  * 'Char'
--  * 'Bool'
--  * ()
--  * Shapes formed from 'Z' and (':.')
--  * Nested tuples of all of these, currently up to 15-elements wide.
--
-- Note that 'Array' itself is not an allowable element type---there are no
-- nested arrays in Accelerate, regular arrays only!
--
-- If device and host memory are separate, arrays will be transferred to the
-- device when necessary (possibly asynchronously and in parallel with other
-- tasks) and cached on the device if sufficient memory is available. Arrays are
-- made available to embedded language computations via
-- 'Data.Array.Accelerate.use'.
--
-- Section "Getting data in" lists functions for getting data into and out of
-- the 'Array' type.
--
data Array sh e where
  Array :: (Shape sh, Elt e)
        => EltRepr sh                 -- extent of dimensions = shape
        -> ArrayData (EltRepr e)      -- array payload
        -> Array sh e

deriving instance Typeable Array

instance (Eq sh, Eq e) => Eq (Array sh e) where
  arr1@Array{} == arr2@Array{} = shape arr1 == shape arr2 && toList arr1 == toList arr2
  arr1@Array{} /= arr2@Array{} = shape arr1 /= shape arr2 || toList arr1 /= toList arr2

#if __GLASGOW_HASKELL__ >= 710
-- Convert an array to a string, using specialised instances for dimensions
-- zero, one, and two. These are available for ghc-7.10 and later only (earlier
-- versions of ghc would require -XIncoherentInstances in the client module).
--
-- TODO:
--   * Make special formatting optional? It is more difficult to copy/paste the
--     result, for example. Also it does not look good if the matrix row does
--     not fit on a single line.
--   * The AST pretty printer does not use these instances
--
instance Show (Scalar e) where
  show arr@Array{} =
    "Scalar Z " ++ show (toList arr)

instance Show (Vector e) where
  show arr@Array{} =
    "Vector (" ++ showShape (shape arr) ++ ") " ++ show (toList arr)

instance Show (Array DIM2 e) where
  show arr@Array{} =
    "Matrix (" ++ showShape (shape arr) ++ ") " ++ showMat
    where
      Z :. rows :. cols = shape arr
      lengths           = U.generate (rows*cols) (\i -> length (show (arr !! i)))
      widths            = U.generate cols (\c -> U.maximum (U.generate rows (\r -> lengths U.! (r*cols+c))))
      --
      showMat
        | rows * cols == 0 = "[]"
        | otherwise        = "\n  [" ++ ppMat 0 0
      --
      ppMat :: Int -> Int -> String
      ppMat !r !c | c >= cols = ppMat (r+1) 0
      ppMat !r !c             =
        let
            !i    = r*cols+c
            !l    = lengths U.! i
            !w    = widths  U.! c
            !pad  = 1
            cell  = replicate (w-l+pad) ' ' ++ show (arr !! i)
            --
            before
              | r > 0 && c == 0 = "\n   "
              | otherwise       = ""
            --
            after
              | r >= rows-1 && c >= cols-1 = "]"
              | otherwise                  = ',' : ppMat r (c+1)
        in
        before ++ cell ++ after
#endif

-- This is a bit unfortunate, but we need to use an INCOHERENT instance because
-- GHC can't determine that with the above specialisations, a DIM3+ instance
-- covers all remaining possibilities, and lacking a general instance is
-- problematic for operations which want a 'Show (Array sh e)' constraint.
-- Furthermore, those clients are likely to pick this instance, rather than the
-- more specific ones above, which is (perhaps) a little unfortunate.
--
instance {-# INCOHERENT #-} Show (Array sh e) where
  show arr@Array{} =
    "Array (" ++ showShape (shape arr) ++ ") " ++ show (toList arr)

instance Elt e => IsList (Vector e) where
  type Item (Vector e) = e
  toList         = toList
  fromListN n xs = fromList (Z:.n) xs
  fromList xs    = GHC.fromListN (length xs) xs

instance NFData (Array sh e) where
  rnf (Array sh ad) = Repr.size sh `seq` go arrayElt ad `seq` ()
    where
      go :: ArrayEltR e' -> ArrayData e' -> ()
      go ArrayEltRunit         AD_Unit         = ()
      go ArrayEltRint          (AD_Int ua)     = rnf ua
      go ArrayEltRint8         (AD_Int8 ua)    = rnf ua
      go ArrayEltRint16        (AD_Int16 ua)   = rnf ua
      go ArrayEltRint32        (AD_Int32 ua)   = rnf ua
      go ArrayEltRint64        (AD_Int64 ua)   = rnf ua
      go ArrayEltRword         (AD_Word ua)    = rnf ua
      go ArrayEltRword8        (AD_Word8 ua)   = rnf ua
      go ArrayEltRword16       (AD_Word16 ua)  = rnf ua
      go ArrayEltRword32       (AD_Word32 ua)  = rnf ua
      go ArrayEltRword64       (AD_Word64 ua)  = rnf ua
      go ArrayEltRhalf         (AD_Half ua)    = rnf ua
      go ArrayEltRfloat        (AD_Float ua)   = rnf ua
      go ArrayEltRdouble       (AD_Double ua)  = rnf ua
      go ArrayEltRbool         (AD_Bool ua)    = rnf ua
      go ArrayEltRchar         (AD_Char ua)    = rnf ua
      go (ArrayEltRvec r)      (AD_Vec !_ a)   = go r a `seq` ()
      go (ArrayEltRpair r1 r2) (AD_Pair a1 a2) = go r1 a1 `seq` go r2 a2 `seq` ()


-- | Scalar arrays hold a single element
--
type Scalar = Array DIM0

-- | Vectors are one-dimensional arrays
--
type Vector = Array DIM1

-- | Matrices are two-dimensional arrays
--
type Matrix = Array DIM2

-- | Segment descriptor (vector of segment lengths).
--
-- To represent nested one-dimensional arrays, we use a flat array of data
-- values in conjunction with a /segment descriptor/, which stores the lengths
-- of the subarrays.
--
type Segments = Vector

-- Shorthand for common shape types
--
type DIM0 = Z
type DIM1 = DIM0:.Int
type DIM2 = DIM1:.Int
type DIM3 = DIM2:.Int
type DIM4 = DIM3:.Int
type DIM5 = DIM4:.Int
type DIM6 = DIM5:.Int
type DIM7 = DIM6:.Int
type DIM8 = DIM7:.Int
type DIM9 = DIM8:.Int


-- Shape constraints and indexing
-- ------------------------------

-- |Shapes and indices of multi-dimensional arrays
--
class (Elt sh, Elt (Any sh), Repr.Shape (EltRepr sh), FullShape sh ~ sh, CoSliceShape sh ~ sh, SliceShape sh ~ Z)
       => Shape sh where

  -- |Number of dimensions of a /shape/ or /index/ (>= 0).
  rank   :: Int

  -- |Total number of elements in an array of the given /shape/.
  size   :: sh -> Int

  -- |Empty /shape/.
  empty :: sh

  -- |Magic value identifying elements ignored in 'permute'.
  ignore :: sh

  -- |Yield the intersection of two shapes
  intersect :: sh -> sh -> sh

  -- |Yield the union of two shapes
  union :: sh -> sh -> sh

  -- |Map a multi-dimensional index into one in a linear, row-major
  -- representation of the array (first argument is the /shape/, second
  -- argument is the index).
  toIndex   :: sh -> sh -> Int

  -- |Inverse of 'toIndex'.
  fromIndex :: sh -> Int -> sh

  -- |Iterate through the entire shape, applying the function; third argument
  -- combines results and fourth is returned in case of an empty iteration
  -- space; the index space is traversed in row-major order.
  iter  :: sh -> (sh -> a) -> (a -> a -> a) -> a -> a

  -- |Variant of 'iter' without an initial value
  iter1 :: sh -> (sh -> a) -> (a -> a -> a) -> a

  -- |Convert a minpoint-maxpoint index into a /shape/.
  rangeToShape ::  (sh, sh) -> sh

  -- |Convert a /shape/ into a minpoint-maxpoint index.
  shapeToRange ::  sh -> (sh, sh)

  -- |Convert a shape to a list of dimensions.
  shapeToList :: sh -> [Int]

  -- |Convert a list of dimensions into a shape.
  listToShape :: [Int] -> sh

  -- | The slice index for slice specifier 'Any sh'
  sliceAnyIndex  :: Repr.SliceIndex (EltRepr (Any sh)) (EltRepr sh) () (EltRepr sh)

  -- | The slice index for specifying a slice with only the Z component projected
  sliceNoneIndex :: Repr.SliceIndex (EltRepr sh) () (EltRepr sh) (EltRepr sh)

  rank                  = Repr.rank @(EltRepr sh)
  size                  = Repr.size . fromElt
  empty                 = toElt Repr.empty
  -- (#) must be individually defined, as it holds for all instances *except*
  -- the one with the largest arity

  ignore                = toElt Repr.ignore
  intersect sh1 sh2     = toElt (Repr.intersect (fromElt sh1) (fromElt sh2))
  union sh1 sh2         = toElt (Repr.union (fromElt sh1) (fromElt sh2))
  fromIndex sh ix       = toElt (Repr.fromIndex (fromElt sh) ix)
  toIndex sh ix         = Repr.toIndex (fromElt sh) (fromElt ix)

  iter sh f c r         = Repr.iter  (fromElt sh) (f . toElt) c r
  iter1 sh f r          = Repr.iter1 (fromElt sh) (f . toElt) r

  rangeToShape (low, high)
    = toElt (Repr.rangeToShape (fromElt low, fromElt high))
  shapeToRange ix
    = let (low, high) = Repr.shapeToRange (fromElt ix)
      in
      (toElt low, toElt high)

  shapeToList = Repr.shapeToList . fromElt
  listToShape = toElt . Repr.listToShape

instance Shape Z where
  sliceAnyIndex  = Repr.SliceNil
  sliceNoneIndex = Repr.SliceNil

instance Shape sh => Shape (sh:.Int) where
  sliceAnyIndex  = Repr.SliceAll   (sliceAnyIndex  @sh)
  sliceNoneIndex = Repr.SliceFixed (sliceNoneIndex @sh)

-- | Slices, aka generalised indices, as /n/-tuples and mappings of slice
-- indices to slices, co-slices, and slice dimensions
--
class (Elt sl, Shape (SliceShape sl), Shape (CoSliceShape sl), Shape (FullShape sl))
       => Slice sl where
  type SliceShape   sl :: *     -- the projected slice
  type CoSliceShape sl :: *     -- the complement of the slice
  type FullShape    sl :: *     -- the combined dimension
  sliceIndex :: Repr.SliceIndex (EltRepr sl)
                                (EltRepr (SliceShape   sl))
                                (EltRepr (CoSliceShape sl))
                                (EltRepr (FullShape    sl))

instance Slice Z where
  type SliceShape   Z = Z
  type CoSliceShape Z = Z
  type FullShape    Z = Z
  sliceIndex = Repr.SliceNil

instance Slice sl => Slice (sl:.All) where
  type SliceShape   (sl:.All) = SliceShape   sl :. Int
  type CoSliceShape (sl:.All) = CoSliceShape sl
  type FullShape    (sl:.All) = FullShape    sl :. Int
  sliceIndex = Repr.SliceAll (sliceIndex @sl)

instance Slice sl => Slice (sl:.Int) where
  type SliceShape   (sl:.Int) = SliceShape   sl
  type CoSliceShape (sl:.Int) = CoSliceShape sl :. Int
  type FullShape    (sl:.Int) = FullShape    sl :. Int
  sliceIndex = Repr.SliceFixed (sliceIndex @sl)

instance Shape sh => Slice (Any sh) where
  type SliceShape   (Any sh) = sh
  type CoSliceShape (Any sh) = Z
  type FullShape    (Any sh) = sh
  sliceIndex = sliceAnyIndex @sh

-- | Generalised array division, like above but use for splitting an array into
-- many subarrays, as opposed to extracting a single subarray.
--
class (Slice (DivisionSlice sl)) => Division sl where
  type DivisionSlice sl :: *     -- the slice
  slicesIndex :: slix ~ DivisionSlice sl
              => Repr.SliceIndex (EltRepr slix)
                                 (EltRepr (SliceShape   slix))
                                 (EltRepr (CoSliceShape slix))
                                 (EltRepr (FullShape    slix))

instance Division Z where
  type DivisionSlice   Z = Z
  slicesIndex = Repr.SliceNil

instance Division sl => Division (sl:.All) where
  type DivisionSlice  (sl:.All) = DivisionSlice sl :. All
  slicesIndex = Repr.SliceAll (slicesIndex @sl)

instance Division sl => Division (sl:.Split) where
  type DivisionSlice (sl:.Split) = DivisionSlice sl :. Int
  slicesIndex = Repr.SliceFixed (slicesIndex @sl)

instance Shape sh => Division (Any sh) where
  type DivisionSlice (Any sh) = Any sh
  slicesIndex = sliceAnyIndex @sh

instance (Shape sh, Slice sh) => Division (Divide sh) where
  type DivisionSlice (Divide sh) = sh
  slicesIndex = sliceNoneIndex @sh


-- Array operations
-- ----------------

-- | Yield an array's shape
--
shape :: Shape sh => Array sh e -> sh
shape (Array sh _) = toElt sh

-- | Change the shape of an array without altering its contents. The 'size' of
-- the source and result arrays must be identical.
--
reshape :: (Shape sh, Shape sh', Elt e) => sh -> Array sh' e -> Array sh e
reshape sh (Array sh' adata)
  = $boundsCheck "reshape" "shape mismatch" (size sh == Repr.size sh')
  $ Array (fromElt sh) adata

-- | Array indexing
--
infixl 9 !
{-# INLINE (!) #-}
(!) :: Array sh e -> sh -> e
(!) (Array sh adata) ix = toElt (adata `unsafeIndexArrayData` toIndex (toElt sh) ix)

infixl 9 !!
{-# INLINE (!!) #-}
(!!) :: Array sh e -> Int -> e
(!!) (Array _ adata) i = toElt (adata `unsafeIndexArrayData` i)

-- | Create an array from its representation function, applied at each index of
-- the array.
--
{-# INLINEABLE fromFunction #-}
fromFunction :: (Shape sh, Elt e) => sh -> (sh -> e) -> Array sh e
fromFunction sh f = unsafePerformIO $! fromFunctionM sh (return . f)

-- | Create an array using a monadic function applied at each index.
--
-- @since 1.2.0.0
--
{-# INLINEABLE fromFunctionM #-}
fromFunctionM :: (Shape sh, Elt e) => sh -> (sh -> IO e) -> IO (Array sh e)
fromFunctionM sh f = do
  let !n = size sh
  arr <- newArrayData n
  --
  let write !i
        | i >= n    = return ()
        | otherwise = do
            v <- f (fromIndex sh i)
            unsafeWriteArrayData arr i (fromElt v)
            write (i+1)
  --
  write 0
  return $! arr `seq` Array (fromElt sh) arr


-- | Create a vector from the concatenation of the given list of vectors.
--
{-# INLINEABLE concatVectors #-}
concatVectors :: Elt e => [Vector e] -> Vector e
concatVectors vs = adata `seq` Array ((), len) adata
  where
    offsets     = scanl (+) 0 (map (size . shape) vs)
    len         = last offsets
    (adata, _)  = runArrayData $ do
              arr <- newArrayData len
              sequence_ [ unsafeWriteArrayData arr (i + k) (unsafeIndexArrayData ad i)
                        | (Array ((), n) ad, k) <- vs `zip` offsets
                        , i <- [0 .. n - 1] ]
              return (arr, undefined)

-- | Creates a new, uninitialized Accelerate array.
--
{-# INLINEABLE allocateArray #-}
allocateArray :: (Shape sh, Elt e) => sh -> IO (Array sh e)
allocateArray sh = do
  adata  <- newArrayData (size sh)
  return $! Array (fromElt sh) adata


-- | Convert elements of a list into an Accelerate 'Array'.
--
-- This will generate a new multidimensional 'Array' of the specified shape and
-- extent by consuming elements from the list and adding them to the array in
-- row-major order.
--
-- >>> fromList (Z:.10) [0..] :: Vector Int
-- Vector (Z :. 10) [0,1,2,3,4,5,6,7,8,9]
--
-- Note that we pull elements off the list lazily, so infinite lists are
-- accepted:
--
-- >>> fromList (Z:.5:.10) (repeat 0) :: Matrix Float
-- Matrix (Z :. 5 :. 10)
--   [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
--     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
--     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
--     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
--     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
--
-- You can also make use of the @OverloadedLists@ extension to produce
-- one-dimensional vectors from a /finite/ list.
--
-- >>> [0..9] :: Vector Int
-- Vector (Z :. 10) [0,1,2,3,4,5,6,7,8,9]
--
-- Note that this requires first traversing the list to determine its length,
-- and then traversing it a second time to collect the elements into the array,
-- thus forcing the spine of the list to be manifest on the heap.
--
{-# INLINEABLE fromList #-}
fromList :: (Shape sh, Elt e) => sh -> [e] -> Array sh e
fromList sh xs = adata `seq` Array (fromElt sh) adata
  where
    -- Assume the array is in dense row-major order. This is safe because
    -- otherwise backends would not be able to directly memcpy.
    --
    !n          = size sh
    (adata, _)  = runArrayData $ do
                    arr <- newArrayData n
                    let go !i _ | i >= n = return ()
                        go !i (v:vs)     = unsafeWriteArrayData arr i (fromElt v) >> go (i+1) vs
                        go _  []         = error "Data.Array.Accelerate.fromList: not enough input data"
                    --
                    go 0 xs
                    return (arr, undefined)

-- | Convert an accelerated 'Array' to a list in row-major order.
--
{-# INLINEABLE toList #-}
toList :: forall sh e. Array sh e -> [e]
toList (Array sh adata) = go 0
  where
    -- Assume underling array is in row-major order. This is safe because
    -- otherwise backends would not be able to directly memcpy.
    --
    !n                  = Repr.size sh
    go !i | i >= n      = []
          | otherwise   = toElt (adata `unsafeIndexArrayData` i) : go (i+1)

-- | Nicely format a shape as a string
--
showShape :: Shape sh => sh -> String
showShape = foldr (\sh str -> str ++ " :. " ++ show sh) "Z" . shapeToList

-- | Project the shape of a slice from the full shape.
--
sliceShape :: forall slix co sl dim. (Shape sl, Shape dim)
           => Repr.SliceIndex slix (EltRepr sl) co (EltRepr dim)
           -> dim
           -> sl
sliceShape slix = toElt . Repr.sliceShape slix . fromElt

-- | Enumerate all slices within a given bound. The innermost dimension
-- changes most rapidly.
--
-- Example:
--
-- > let slix = sliceIndex @(Z :. Int :. Int :. All)
-- >     sh   = Z :. 2 :. 3 :. 1 :: DIM3
-- > in
-- > enumSlices slix sh :: [ Z :. Int :. Int :. All ]
--
enumSlices :: forall slix co sl dim. (Elt slix, Elt dim)
           => Repr.SliceIndex (EltRepr slix) sl co (EltRepr dim)
           -> dim    -- Bounds
           -> [slix] -- All slices within bounds.
enumSlices slix = map toElt . Repr.enumSlices slix . fromElt


-- Instances
-- ---------

$( runQ $ do
    let
        -- XXX: we might want to do the digItOut trick used by FromIntegral?
        --
        integralTypes :: [Name]
        integralTypes =
          [ ''Int
          , ''Int8
          , ''Int16
          , ''Int32
          , ''Int64
          , ''Word
          , ''Word8
          , ''Word16
          , ''Word32
          , ''Word64
          ]

        floatingTypes :: [Name]
        floatingTypes =
          [ ''Half
          , ''Float
          , ''Double
          ]

        nonNumTypes :: [Name]
        nonNumTypes =
          [ ''Bool
          , ''Char
          ]

        newtypes :: [Name]
        newtypes =
          [ ''CShort
          , ''CUShort
          , ''CInt
          , ''CUInt
          , ''CLong
          , ''CULong
          , ''CLLong
          , ''CULLong
          , ''CFloat
          , ''CDouble
          , ''CChar
          , ''CSChar
          , ''CUChar
          ]

        mkSimple :: Name -> Q [Dec]
        mkSimple name =
          let t = conT name
          in
          [d| instance Elt $t where
                type EltRepr $t = $t
                {-# INLINE eltType #-}
                {-# INLINE fromElt #-}
                {-# INLINE toElt   #-}
                eltType = singletonScalarType
                fromElt = id
                toElt   = id
            |]

        -- XXX: Should we fix this to known "good" vector sizes?
        --
        mkVector :: Name -> Q [Dec]
        mkVector name =
          let t = conT name
          in
          [d| instance KnownNat n => Elt (Vec n $t) where
                type EltRepr (Vec n $t) = Vec n $t
                {-# INLINE eltType #-}
                {-# INLINE fromElt #-}
                {-# INLINE toElt   #-}
                eltType = singletonScalarType
                fromElt = id
                toElt   = id
            |]

        -- ghci> $( stringE . show =<< reify ''CFloat )
        -- TyConI (NewtypeD [] Foreign.C.Types.CFloat [] Nothing (NormalC Foreign.C.Types.CFloat [(Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Types.Float)]) [])
        --
        mkNewtype :: Name -> Q [Dec]
        mkNewtype name = do
          r    <- reify name
          base <- case r of
                    TyConI (NewtypeD _ _ _ _ (NormalC _ [(_, ConT b)]) _) -> return b
                    _                                                     -> error "unexpected case generating newtype Elt instance"
          --
          [d| instance Elt $(conT name) where
                type EltRepr $(conT name) = $(conT base)
                {-# INLINE eltType #-}
                {-# INLINE fromElt #-}
                {-# INLINE toElt   #-}
                eltType = singletonScalarType
                fromElt $(conP (mkName (nameBase name)) [varP (mkName "x")]) = x
                toElt = $(conE (mkName (nameBase name)))
            |]
    --
    ss <- mapM mkSimple ( integralTypes ++ floatingTypes ++      nonNumTypes )
    vs <- mapM mkVector ( integralTypes ++ floatingTypes ++ tail nonNumTypes )  -- not Bool
    ns <- mapM mkNewtype newtypes
    return (concat ss ++ concat vs ++ concat ns)
 )

