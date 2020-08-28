{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
{-# OPTIONS_GHC -fno-warn-inline-rule-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Sugar
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Array.Sugar (
  -- * Tuple representation
  TupR(..),

  -- * Array representation
  Array(..), Scalar, Vector, Matrix, Segments, arrayR,
  Arrays(..), Repr.ArraysR, Repr.ArrayR(..), Repr.arraysRarray, Repr.arraysRtuple2,

  -- * Class of supported surface element types and their mapping to representation types
  Elt(..), TupleType,

  -- * Derived functions
  liftToElt, liftToElt2, sinkFromElt, sinkFromElt2,

  -- * Array shapes
  DIM0, DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7, DIM8, DIM9,

  -- * Array indexing and slicing
  Z(..), (:.)(..), All(..), Split(..), Any(..), Divide(..), Shape(..), Slice(..), Division(..),

  -- * Array shape query, indexing, and conversions
  shape, reshape, (!), (!!), allocateArray, fromFunction, fromFunctionM, fromList, toList, concatVectors,

  -- * Miscellaneous
  showShape, Foreign(..), sliceShape, enumSlices, VecElt,

) where

-- standard library
import Control.DeepSeq
import Data.Kind
import Data.Typeable
import Data.Primitive.Types
import System.IO.Unsafe                                         ( unsafePerformIO )
import Language.Haskell.TH                                      hiding ( Foreign, Type )
import Language.Haskell.TH.Extra
import Prelude                                                  hiding ( (!!) )

import GHC.Exts                                                 ( IsList )
import GHC.Generics
import GHC.TypeLits
import qualified GHC.Exts                                       as GHC

-- friends
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Orphans                            ()
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
  deriving (Show, Eq)

-- | Increase an index rank by one dimension. The ':.' operator is used to
-- construct both values and types.
--
infixl 3 :.
data tail :. head = !tail :. !head
  deriving Eq

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
  showsPrec p (sh :. sz) =
    showsPrec p sh . showString " :. " . showsPrec p sz

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
  deriving (Show, Eq)

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
  deriving (Show, Eq)

-- | Marker for splitting along an entire dimension in division descriptors.
--
-- For example, when used in a division descriptor passed to
-- 'Data.Array.Accelerate.toSeq', a `Split` indicates that the array should be
-- divided along this dimension forming the elements of the output sequence.
--
data Split = Split
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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
class Show a => Elt a where
  -- | Type representation mapping, which explains how to convert a type from
  -- the surface type into the internal representation type consisting only of
  -- simple primitive types, unit '()', and pair '(,)'.
  --
  type EltRepr a :: Type
  type EltRepr a = GEltRepr () (Rep a)
  --
  eltType  :: TupleType (EltRepr a)
  fromElt  :: a -> EltRepr a
  toElt    :: EltRepr a -> a

  {-# INLINE eltType #-}
  default eltType
    :: (GElt (Rep a), EltRepr a ~ GEltRepr () (Rep a))
    => TupleType (EltRepr a)
  eltType = geltType @(Rep a) TupRunit

  {-# INLINE [1] fromElt #-}
  default fromElt
    :: (Generic a, GElt (Rep a), EltRepr a ~ GEltRepr () (Rep a))
    => a
    -> EltRepr a
  fromElt = gfromElt () . from

  {-# INLINE [1] toElt #-}
  default toElt
    :: (Generic a, GElt (Rep a), EltRepr a ~ GEltRepr () (Rep a))
    => EltRepr a
    -> a
  toElt = to . snd . gtoElt @(Rep a) @()


class GElt f where
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
  geltType t        = TupRpair t (eltType @a)
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
-- Which yields the error "couldn't match type 'EltRepr a0' with 'Int16'".
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
-- TLM 2019-03-22: I think this is fixed now
--

instance Elt () where
  type EltRepr () = ()
  {-# INLINE eltType #-}
  {-# INLINE toElt   #-}
  {-# INLINE fromElt #-}
  eltType   = TupRunit
  fromElt   = id
  toElt     = id

instance Elt Z where
  type EltRepr Z = ()
  {-# INLINE eltType     #-}
  {-# INLINE [1] toElt   #-}
  {-# INLINE [1] fromElt #-}
  eltType    = TupRunit
  fromElt Z  = ()
  toElt ()   = Z

instance (Elt t, Elt h) => Elt (t:.h) where
  type EltRepr (t:.h) = (EltRepr t, EltRepr h)
  {-# INLINE eltType     #-}
  {-# INLINE [1] toElt   #-}
  {-# INLINE [1] fromElt #-}
  eltType         = TupRpair (eltType @t) (eltType @h)
  fromElt (t:.h)  = (fromElt t, fromElt h)
  toElt (t, h)    = toElt t :. toElt h

instance Elt All where
  type EltRepr All = ()
  {-# INLINE eltType     #-}
  {-# INLINE [1] toElt   #-}
  {-# INLINE [1] fromElt #-}
  eltType       = TupRunit
  fromElt All   = ()
  toElt ()      = All

type family AnyRepr sh
type instance AnyRepr () = ()
type instance AnyRepr (sh, Int) = (AnyRepr sh, ())

instance Shape sh => Elt (Any sh) where
  type EltRepr (Any sh) = AnyRepr (EltRepr sh)

  {-# INLINE eltType     #-}
  {-# INLINE [1] toElt   #-}
  {-# INLINE [1] fromElt #-}
  eltType       = go $ shapeR @sh
    where
      go :: Repr.ShapeR sh' -> TupleType (AnyRepr sh')
      go Repr.ShapeRz = TupRunit
      go (Repr.ShapeRsnoc shr) = TupRpair (go shr) TupRunit
  fromElt _     = go $ shapeR @sh
    where
      go :: Repr.ShapeR sh' -> AnyRepr sh'
      go Repr.ShapeRz = ()
      go (Repr.ShapeRsnoc shr) = (go shr, ())
  toElt _       = Any


--  Convenience functions
--
singletonScalarType :: IsScalar a => TupleType a
singletonScalarType = TupRsingle scalarType

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

{-# RULES
"fromElt/toElt" forall e. fromElt (toElt e) = e
"toElt/fromElt" forall e. toElt (fromElt e) = e
#-}

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


-- Arrays
-- ------

-- | The 'Arrays' class characterises the types which can appear in collective
-- Accelerate computations of type 'Data.Array.Accelerate.Acc'.
--
-- 'Arrays' consists of nested tuples of individual 'Array's, currently up to
-- 16-elements wide. Accelerate computations can thereby return multiple
-- results.
--
class Arrays a where
  -- | Type representation mapping, which explains how to convert from the
  -- surface type into the internal representation type, which consists only of
  -- 'Array', and '()' and '(,)' as type-level nil and snoc.
  --
  type ArrRepr a :: Type
  type ArrRepr a = GArrRepr () (Rep a)

  arrays   :: Repr.ArraysR (ArrRepr a)
  toArr    :: ArrRepr  a -> a
  fromArr  :: a -> ArrRepr  a

  {-# INLINE arrays #-}
  default arrays
    :: (GArrays (Rep a), ArrRepr a ~ GArrRepr () (Rep a))
    => Repr.ArraysR (ArrRepr a)
  arrays = garrays @(Rep a) TupRunit

  {-# INLINE [1] toArr #-}
  default toArr
    :: (Generic a, GArrays (Rep a), ArrRepr a ~ GArrRepr () (Rep a))
    => ArrRepr a -> a
  toArr = to . snd . gtoArr @(Rep a) @()

  {-# INLINE [1] fromArr #-}
  default fromArr
    :: (Generic a, GArrays (Rep a), ArrRepr a ~ GArrRepr () (Rep a))
    => a -> ArrRepr a
  fromArr = (`gfromArr` ()) . from

  -- flavour :: ArraysFlavour a
  -- default flavour
  --   :: (Generic a, GArrays (Rep a), GArrFlav (Rep a) ~ a, ArrRepr a ~ GArrRepr () (Rep a))
  --   => a -> ArraysFlavour a
  -- flavour _ = gflavour @(Rep a)

arrayR :: forall sh e. (Shape sh, Elt e) => Repr.ArrayR (Repr.Array (EltRepr sh) (EltRepr e))
arrayR = Repr.ArrayR (shapeR @sh) (eltType @e)

class GArrays f where
  type GArrRepr t f
  garrays  :: Repr.ArraysR t -> Repr.ArraysR (GArrRepr t f)
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
  garrays         t = TupRpair t (arrays @a)
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
  {-# INLINE arrays      #-}
  {-# INLINE [1] fromArr #-}
  {-# INLINE [1] toArr   #-}
  arrays  = TupRunit
  fromArr = id
  toArr   = id

instance (Shape sh, Elt e) => Arrays (Array sh e) where
  type ArrRepr (Array sh e) = Repr.Array (EltRepr sh) (EltRepr e)
  {-# INLINE arrays      #-}
  {-# INLINE [1] fromArr #-}
  {-# INLINE [1] toArr   #-}
  arrays  = Repr.arraysRarray (shapeR @sh) (eltType @e)
  fromArr (Array arr) = arr
  toArr   (arr)       = Array arr

{-# RULES
"fromArr/toArr" forall a. fromArr (toArr a) = a
"toArr/fromArr" forall a. toArr (fromArr a) = a
#-}


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
newtype Array sh e = Array (Repr.Array (EltRepr sh) (EltRepr e))

--
-- Note: [Embedded class constraints on Array]
--
-- Previously, we had embedded 'Shape' and 'Elt' constraints on the 'Array'
-- constructor. This was occasionally convenient, however, this has a negative
-- impact on the kind of code which GHC can generate. For example, if we write
-- the function:
--
-- > (!) :: Array sh e -> sh -> e
--
-- Without the 'Shape' and 'Elt' constraints on the type signature, and instead
-- recover those when pattern matching on 'Array', then GHC is unable to
-- specialise functions past this point. In this example, even if 'sh' and 'e'
-- are fixed, GHC would not be able to inline the definitions from 'ArrayElt'
-- which perform the actual data accesses.
--
--   - TLM 2018-09-13
--

deriving instance Typeable Array

instance (Shape sh, Elt e, Eq sh, Eq e) => Eq (Array sh e) where
  arr1 == arr2 = shape arr1 == shape arr2 && toList arr1 == toList arr2
  arr1 /= arr2 = shape arr1 /= shape arr2 || toList arr1 /= toList arr2

-- We perform the rank check at runtime, as we want a generic Show (Array sh e)
-- instance. Alternatives would be to create instances for Show (Array Z e),
-- Show (Array (Z :. Int) e) and so on. This would either require that the
-- instance for general ranks either works only for DIM3+ arrays, or mean
-- that the general case is defined with the INCOHERENT annotation. In the first
-- option, we do not have a general 'Show (Array sh e)' implementation, which
-- is an annoying limitation for users. In the second option, scalars, vectors and
-- matrices may not always be shown with their appropriate format.
--
instance (Shape sh, Elt e) => Show (Array sh e) where
  show (Array arr) = Repr.showArray' (shows . toElt @e) (arrayR @sh @e) arr

instance Elt e => IsList (Vector e) where
  type Item (Vector e) = e
  toList         = toList
  fromListN n xs = fromList (Z:.n) xs
  fromList xs    = GHC.fromListN (length xs) xs

instance (Shape sh, Elt e) => NFData (Array sh e) where
  rnf (Array arr) = Repr.rnfArray (arrayR @sh @e) $ arr

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
class (Elt sh, Elt (Any sh), FullShape sh ~ sh, CoSliceShape sh ~ sh, SliceShape sh ~ Z)
       => Shape sh where

  shapeR :: Repr.ShapeR (EltRepr sh)

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

  -- |Attempt to convert a list of dimensions into a shape
  listToShape' :: [Int] -> Maybe sh

  -- |The slice index for slice specifier 'Any sh'
  sliceAnyIndex  :: Repr.SliceIndex (EltRepr (Any sh)) (EltRepr sh) () (EltRepr sh)

  -- |The slice index for specifying a slice with only the Z component projected
  sliceNoneIndex :: Repr.SliceIndex (EltRepr sh) () (EltRepr sh) (EltRepr sh)

  {-# INLINE rank         #-}
  {-# INLINE size         #-}
  {-# INLINE empty        #-}
  {-# INLINE ignore       #-}
  {-# INLINE intersect    #-}
  {-# INLINE union        #-}
  {-# INLINE fromIndex    #-}
  {-# INLINE toIndex      #-}
  {-# INLINE iter         #-}
  {-# INLINE iter1        #-}
  {-# INLINE rangeToShape #-}
  {-# INLINE shapeToRange #-}
  {-# INLINE shapeToList  #-}
  {-# INLINE listToShape  #-}
  {-# INLINE listToShape' #-}
  rank                  = Repr.rank (shapeR @sh)
  size                  = Repr.size (shapeR @sh) . fromElt
  empty                 = toElt $ Repr.empty $ shapeR @sh
  -- (#) must be individually defined, as it holds for all instances *except*
  -- the one with the largest arity

  ignore                = toElt $ Repr.ignore $ shapeR @sh
  intersect sh1 sh2     = toElt (Repr.intersect (shapeR @sh) (fromElt sh1) (fromElt sh2))
  union sh1 sh2         = toElt (Repr.union (shapeR @sh) (fromElt sh1) (fromElt sh2))
  fromIndex sh ix       = toElt (Repr.fromIndex (shapeR @sh) (fromElt sh) ix)
  toIndex sh ix         = Repr.toIndex (shapeR @sh) (fromElt sh) (fromElt ix)

  iter sh f c r         = Repr.iter  (shapeR @sh) (fromElt sh) (f . toElt) c r
  iter1 sh f r          = Repr.iter1 (shapeR @sh) (fromElt sh) (f . toElt) r

  rangeToShape (low, high)
    = toElt (Repr.rangeToShape (shapeR @sh) (fromElt low, fromElt high))
  shapeToRange ix
    = let (low, high) = Repr.shapeToRange (shapeR @sh) (fromElt ix)
      in
      (toElt low, toElt high)

  shapeToList  = Repr.shapeToList (shapeR @sh) . fromElt
  listToShape  = toElt . Repr.listToShape (shapeR @sh)
  listToShape' = fmap toElt . Repr.listToShape' (shapeR @sh)

instance Shape Z where
  shapeR = Repr.ShapeRz
  sliceAnyIndex  = Repr.SliceNil
  sliceNoneIndex = Repr.SliceNil

instance Shape sh => Shape (sh:.Int) where
  shapeR = Repr.ShapeRsnoc (shapeR @sh)
  sliceAnyIndex  = Repr.SliceAll   (sliceAnyIndex  @sh)
  sliceNoneIndex = Repr.SliceFixed (sliceNoneIndex @sh)

-- | Slices, aka generalised indices, as /n/-tuples and mappings of slice
-- indices to slices, co-slices, and slice dimensions
--
class (Elt sl, Shape (SliceShape sl), Shape (CoSliceShape sl), Shape (FullShape sl))
       => Slice sl where
  type SliceShape   sl :: Type    -- the projected slice
  type CoSliceShape sl :: Type    -- the complement of the slice
  type FullShape    sl :: Type    -- the combined dimension
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
  type DivisionSlice sl :: Type   -- the slice
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
{-# INLINE shape #-}
shape :: Shape sh => Array sh e -> sh
shape (Array arr) = toElt $ Repr.shape arr

-- | Change the shape of an array without altering its contents. The 'size' of
-- the source and result arrays must be identical.
--
{-# INLINE reshape #-}
reshape :: forall sh sh' e. (Shape sh, Shape sh') => sh -> Array sh' e -> Array sh e
reshape sh (Array arr) = Array $ Repr.reshape (shapeR @sh) (fromElt sh) (shapeR @sh') arr

-- | Array indexing
--
infixl 9 !
{-# INLINE [1] (!) #-}
(!) :: forall sh e. (Shape sh, Elt e) => Array sh e -> sh -> e
(!) (Array arr) ix = toElt $ (arrayR @sh @e, arr) Repr.! fromElt ix

infixl 9 !!
{-# INLINE [1] (!!) #-}
(!!) :: forall sh e. Elt e => Array sh e -> Int -> e
(!!) (Array arr) i = toElt $ (eltType @e, arr) Repr.!! i

{-# RULES
"indexArray/DIM0" forall arr.   arr ! Z        = arr !! 0
"indexArray/DIM1" forall arr i. arr ! (Z :. i) = arr !! i
#-}

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
fromFunctionM :: forall sh e. (Shape sh, Elt e) => sh -> (sh -> IO e) -> IO (Array sh e)
fromFunctionM sh f = Array <$> Repr.fromFunctionM (arrayR @sh @e) (fromElt sh) f'
  where
    f' x = do
      y <- f $ toElt x
      return $ fromElt y


-- | Create a vector from the concatenation of the given list of vectors.
--
{-# INLINEABLE concatVectors #-}
concatVectors :: forall e. Elt e => [Vector e] -> Vector e
concatVectors = toArr . Repr.concatVectors (eltType @e) . map fromArr


-- | Creates a new, uninitialized Accelerate array.
--
{-# INLINEABLE allocateArray #-}
allocateArray :: forall sh e. (Shape sh, Elt e) => sh -> IO (Array sh e)
allocateArray sh = Array <$> Repr.allocateArray (arrayR @sh @e) (fromElt sh)


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
fromList :: forall sh e. (Shape sh, Elt e) => sh -> [e] -> Array sh e
fromList sh xs = toArr $ Repr.fromList (arrayR @sh @e) (fromElt sh) $ map fromElt xs

-- | Convert an accelerated 'Array' to a list in row-major order.
--
{-# INLINEABLE toList #-}
toList :: forall sh e. (Shape sh, Elt e) => Array sh e -> [e]
toList = map toElt . Repr.toList (arrayR @sh @e) . fromArr

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


-- Vec
-- ---

class (Elt a, IsSingle a, Prim a, a ~ EltRepr a) => VecElt a

-- XXX: Should we fix this to known "good" vector sizes?
--
instance (KnownNat n, VecElt a) => Elt (Vec n a) where
  type EltRepr (Vec n a) = Vec n a
  {-# INLINE eltType     #-}
  {-# INLINE [1] fromElt #-}
  {-# INLINE [1] toElt   #-}
  eltType = TupRsingle $ VectorScalarType $ VectorType (fromIntegral $ natVal (undefined :: Proxy n)) $ singleType @a
  fromElt = id
  toElt   = id

-- Instances
-- ---------

$(runQ $ do
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
                {-# INLINE eltType     #-}
                {-# INLINE [1] fromElt #-}
                {-# INLINE [1] toElt   #-}
                eltType = singletonScalarType
                fromElt = id
                toElt   = id
            |]

        mkVecElt :: Name -> Q [Dec]
        mkVecElt name =
          let t = conT name
          in
          [d| instance VecElt $t |]

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
                {-# INLINE eltType     #-}
                {-# INLINE [1] fromElt #-}
                {-# INLINE [1] toElt   #-}
                eltType = singletonScalarType
                fromElt $(conP (mkName (nameBase name)) [varP (mkName "x")]) = x
                toElt = $(conE (mkName (nameBase name)))
            |]
    --
    ss <- mapM mkSimple ( integralTypes ++ floatingTypes ++      nonNumTypes )
    vs <- mapM mkVecElt ( integralTypes ++ floatingTypes ++ tail nonNumTypes )  -- not Bool
    ns <- mapM mkNewtype newtypes
    return (concat ss ++ concat vs ++ concat ns)
 )

$(runQ $ do
    let
        mkInstance :: TypeQ -> Int -> Q Dec
        mkInstance cst n =
          let
              xs  = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              ts  = map varT xs
              res = tupT ts
              ctx = mapM (appT cst) ts
          in
          instanceD ctx (appT cst res) []
    --
    es <- mapM (mkInstance [t| Elt    |]) [2..16]
    as <- mapM (mkInstance [t| Arrays |]) [2..16]
    return (es ++ as)
 )

