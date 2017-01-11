{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
#if __GLASGOW_HASKELL__ <= 708
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Sugar
-- Copyright   : [2008..2016] Manuel M T Chakravarty, Gabriele Keller
--               [2008..2009] Sean Lee
--               [2009..2016] Trevor L. McDonell
--               [2013..2016] Robert Clifton-Everest
--               [2014..2014] Frederik M. Madsen
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Array.Sugar (

  -- * Array representation
  Array(..), Scalar, Vector, Segments,
  Arrays(..), ArraysR(..), ArraysFlavour(..), ArrRepr,

  -- * Class of supported surface element types and their mapping to representation types
  Elt(..), EltRepr, EltFlavour(..),

  -- * Derived functions
  liftToElt, liftToElt2, sinkFromElt, sinkFromElt2,

  -- * Array shapes
  DIM0, DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7, DIM8, DIM9,

  -- * Array indexing and slicing
  Z(..), (:.)(..), All(..), Split(..), Any(..), Divide(..), Shape(..), Slice(..), Division(..), AsSlice(..),
  (:<=)(..), (:<=:)(..), ShapeR(..), SliceR(..),

  -- * Array shape query, indexing, and conversions
  shape, (!), allocateArray, fromFunction, fromList, toList, concatVectors,

  -- * Tuples
  TupleR, TupleRepr, tuple,
  AtupleR, atuple,
  Tuple(..), IsTuple, fromTuple, toTuple,
  Atuple(..), IsAtuple, fromAtuple, toAtuple,

  -- * Miscellaneous
  showShape, Foreign(..), sliceShape, enumSlices,

) where

-- standard library
import Control.DeepSeq
import Data.List                                                ( intercalate )
import Data.Typeable
import qualified Data.List                                      as List

import GHC.Exts                                                 ( IsList )
import qualified GHC.Exts                                       as GHC

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Product
import qualified Data.Array.Accelerate.Array.Representation     as Repr


-- Surface types representing array indices and slices
-- ---------------------------------------------------
--
-- Array indices are snoc type lists.  That is, they're backwards --
-- the end-of-list token, `Z`, occurs first.  For example, the type of a
-- rank-2 array index is @Z :. Int :. Int@.
--
-- In Accelerate the rightmost dimension is the /fastest varying/ or innermost.

-- |Rank-0 index
--
data Z = Z
  deriving (Typeable, Show, Eq)

-- |Increase an index rank by one dimension.  The `:.` operator is
--  used to construct both values and types.
--
infixl 3 :.
data tail :. head = tail :. head
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


-- Representation change for array element types
-- ---------------------------------------------

-- | Type representation mapping
--
-- We represent tuples by using '()' and '(,)' as type-level nil and snoc to
-- construct snoc-lists of types, and are flattened all the way down to
-- primitive types.
--
type family EltRepr a :: *
type instance EltRepr ()              = ()
type instance EltRepr Z               = ()
type instance EltRepr (t:.h)          = (EltRepr t, EltRepr h)
type instance EltRepr All             = ()
type instance EltRepr (Any Z)         = ()
type instance EltRepr (Any (sh:.Int)) = (EltRepr (Any sh), ())
type instance EltRepr Int             = Int
type instance EltRepr Int8            = Int8
type instance EltRepr Int16           = Int16
type instance EltRepr Int32           = Int32
type instance EltRepr Int64           = Int64
type instance EltRepr Word            = Word
type instance EltRepr Word8           = Word8
type instance EltRepr Word16          = Word16
type instance EltRepr Word32          = Word32
type instance EltRepr Word64          = Word64
type instance EltRepr CShort          = CShort
type instance EltRepr CUShort         = CUShort
type instance EltRepr CInt            = CInt
type instance EltRepr CUInt           = CUInt
type instance EltRepr CLong           = CLong
type instance EltRepr CULong          = CULong
type instance EltRepr CLLong          = CLLong
type instance EltRepr CULLong         = CULLong
type instance EltRepr Float           = Float
type instance EltRepr Double          = Double
type instance EltRepr CFloat          = CFloat
type instance EltRepr CDouble         = CDouble
type instance EltRepr Bool            = Bool
type instance EltRepr Char            = Char
type instance EltRepr CChar           = CChar
type instance EltRepr CSChar          = CSChar
type instance EltRepr CUChar          = CUChar
type instance EltRepr (a, b)          = TupleRepr (EltRepr a, EltRepr b)
type instance EltRepr (a, b, c)       = TupleRepr (EltRepr a, EltRepr b, EltRepr c)
type instance EltRepr (a, b, c, d)    = TupleRepr (EltRepr a, EltRepr b, EltRepr c, EltRepr d)
type instance EltRepr (a, b, c, d, e) = TupleRepr (EltRepr a, EltRepr b, EltRepr c, EltRepr d, EltRepr e)
type instance EltRepr (a, b, c, d, e, f) = TupleRepr (EltRepr a, EltRepr b, EltRepr c, EltRepr d, EltRepr e, EltRepr f)
type instance EltRepr (a, b, c, d, e, f, g) = TupleRepr (EltRepr a, EltRepr b, EltRepr c, EltRepr d, EltRepr e, EltRepr f, EltRepr g)
type instance EltRepr (a, b, c, d, e, f, g, h) = TupleRepr (EltRepr a, EltRepr b, EltRepr c, EltRepr d, EltRepr e, EltRepr f, EltRepr g, EltRepr h)
type instance EltRepr (a, b, c, d, e, f, g, h, i) = TupleRepr (EltRepr a, EltRepr b, EltRepr c, EltRepr d, EltRepr e, EltRepr f, EltRepr g, EltRepr h, EltRepr i)
type instance EltRepr (a, b, c, d, e, f, g, h, i, j) = TupleRepr (EltRepr a, EltRepr b, EltRepr c, EltRepr d, EltRepr e, EltRepr f, EltRepr g, EltRepr h, EltRepr i, EltRepr j)
type instance EltRepr (a, b, c, d, e, f, g, h, i, j, k) = TupleRepr (EltRepr a, EltRepr b, EltRepr c, EltRepr d, EltRepr e, EltRepr f, EltRepr g, EltRepr h, EltRepr i, EltRepr j, EltRepr k)
type instance EltRepr (a, b, c, d, e, f, g, h, i, j, k, l) = TupleRepr (EltRepr a, EltRepr b, EltRepr c, EltRepr d, EltRepr e, EltRepr f, EltRepr g, EltRepr h, EltRepr i, EltRepr j, EltRepr k, EltRepr l)
type instance EltRepr (a, b, c, d, e, f, g, h, i, j, k, l, m) = TupleRepr (EltRepr a, EltRepr b, EltRepr c, EltRepr d, EltRepr e, EltRepr f, EltRepr g, EltRepr h, EltRepr i, EltRepr j, EltRepr k, EltRepr l, EltRepr m)
type instance EltRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = TupleRepr (EltRepr a, EltRepr b, EltRepr c, EltRepr d, EltRepr e, EltRepr f, EltRepr g, EltRepr h, EltRepr i, EltRepr j, EltRepr k, EltRepr l, EltRepr m, EltRepr n)
type instance EltRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = TupleRepr (EltRepr a, EltRepr b, EltRepr c, EltRepr d, EltRepr e, EltRepr f, EltRepr g, EltRepr h, EltRepr i, EltRepr j, EltRepr k, EltRepr l, EltRepr m, EltRepr n, EltRepr o)

type IsTuple = IsProduct Elt

fromTuple :: IsTuple tup => tup -> TupleRepr tup
fromTuple = fromProd (Proxy :: Proxy Elt)

toTuple :: IsTuple tup => TupleRepr tup -> tup
toTuple = toProd (Proxy :: Proxy Elt)

-- For the purposes of optimisation, we sometimes need to know whether an array
-- elements is scalar or if it is a tuple.
--
data EltFlavour e where
  EltBase  :: Elt e     => EltFlavour e
  EltTuple :: IsTuple t => EltFlavour t


-- Array elements (tuples of scalars)
-- ----------------------------------

-- | The 'Elt' class characterises the allowable array element types, and hence
-- the types which can appear in scalar Accelerate expressions.
--
-- Accelerate arrays consist of simple atomic types as well as nested tuples
-- thereof, stored efficiently in memory as consecutive unpacked elements
-- without pointers. It roughly consists of:
--
--  * Signed and unsigned integers (8, 16, 32, and 64-bits wide)
--  * Floating point numbers (single and double precision)
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
class (Show a, Typeable a, Typeable (EltRepr a), ArrayElt (EltRepr a))
      => Elt a where
  eltType  :: {-dummy-} a -> TupleType (EltRepr a)
  eltFlavour :: {-dummy-} a -> EltFlavour a
  fromElt  :: a -> EltRepr a
  toElt    :: EltRepr a -> a

instance Elt () where
  eltType _    = UnitTuple
  eltFlavour _ = EltTuple
  fromElt      = id
  toElt        = id

instance Elt Z where
  eltType _    = UnitTuple
  eltFlavour _ = EltBase
  fromElt Z    = ()
  toElt ()     = Z

instance (Elt t, Elt h) => Elt (t:.h) where
  eltType (_::(t:.h))   = PairTuple (eltType (undefined :: t)) (eltType (undefined :: h))
  eltFlavour _          = EltTuple
  fromElt (t:.h)        = (fromElt t, fromElt h)
  toElt (t, h)          = toElt t :. toElt h

instance Elt All where
  eltType _     = UnitTuple
  eltFlavour _  = EltBase
  fromElt All   = ()
  toElt ()      = All

instance Elt (Any Z) where
  eltType _     = UnitTuple
  eltFlavour _  = EltBase
  fromElt _     = ()
  toElt _       = Any

instance Shape sh => Elt (Any (sh:.Int)) where
  eltType _     = PairTuple (eltType (undefined::Any sh)) UnitTuple
  fromElt _     = (fromElt (undefined :: Any sh), ())
  eltFlavour _  = EltBase
  toElt _       = Any

instance Elt Int where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt Int8 where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt Int16 where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt Int32 where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt Int64 where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt Word where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt Word8 where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt Word16 where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt Word32 where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt Word64 where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt CShort where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt CUShort where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt CInt where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt CUInt where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt CLong where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt CULong where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt CLLong where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt CULLong where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt Float where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt Double where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt CFloat where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt CDouble where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt Bool where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt Char where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt CChar where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt CSChar where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance Elt CUChar where
  eltType       = singletonScalarType
  eltFlavour _  = EltBase
  fromElt       = id
  toElt         = id

instance (Elt a, Elt b) => Elt (a, b) where
  eltType _             = PairTuple (PairTuple UnitTuple (eltType (undefined::a))) (eltType (undefined::b))
  eltFlavour _          = EltTuple
  fromElt (a,b)         = (((), fromElt a), fromElt b)
  toElt (((),a),b)      = (toElt a, toElt b)

instance (Elt a, Elt b, Elt c) => Elt (a, b, c) where
  eltType _             = PairTuple (eltType (undefined :: (a, b))) (eltType (undefined :: c))
  fromElt (a, b, c)     = (fromElt (a, b), fromElt c)
  eltFlavour _          = EltTuple
  toElt (ab, c)         = let (a, b) = toElt ab in (a, b, toElt c)

instance (Elt a, Elt b, Elt c, Elt d) => Elt (a, b, c, d) where
  eltType _             = PairTuple (eltType (undefined :: (a, b, c))) (eltType (undefined :: d))
  fromElt (a, b, c, d)  = (fromElt (a, b, c), fromElt d)
  eltFlavour _          = EltTuple
  toElt (abc, d)        = let (a, b, c) = toElt abc in (a, b, c, toElt d)

instance (Elt a, Elt b, Elt c, Elt d, Elt e) => Elt (a, b, c, d, e) where
  eltType _               = PairTuple (eltType (undefined :: (a, b, c, d))) (eltType (undefined :: e))
  fromElt (a, b, c, d, e) = (fromElt (a, b, c, d), fromElt e)
  eltFlavour _            = EltTuple
  toElt (abcd, e)         = let (a, b, c, d) = toElt abcd in (a, b, c, d, toElt e)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f) => Elt (a, b, c, d, e, f) where
  eltType _
    = PairTuple (eltType (undefined :: (a, b, c, d, e)))
                (eltType (undefined :: f))
  eltFlavour _ = EltTuple
  fromElt (a, b, c, d, e, f) = (fromElt (a, b, c, d, e), fromElt f)
  toElt (abcde, f) = let (a, b, c, d, e) = toElt abcde in (a, b, c, d, e, toElt f)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
  => Elt (a, b, c, d, e, f, g) where
  eltType _
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f)))
                (eltType (undefined :: g))
  eltFlavour _ = EltTuple
  fromElt (a, b, c, d, e, f, g) = (fromElt (a, b, c, d, e, f), fromElt g)
  toElt (abcdef, g) = let (a, b, c, d, e, f) = toElt abcdef
                      in  (a, b, c, d, e, f, toElt g)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
  => Elt (a, b, c, d, e, f, g, h) where
  eltType _
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f, g)))
                (eltType (undefined :: h))
  eltFlavour _ = EltTuple
  fromElt (a, b, c, d, e, f, g, h) = (fromElt (a, b, c, d, e, f, g), fromElt h)
  toElt (abcdefg, h) = let (a, b, c, d, e, f, g) = toElt abcdefg
                       in  (a, b, c, d, e, f, g, toElt h)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
  => Elt (a, b, c, d, e, f, g, h, i) where
  eltType _
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f, g, h)))
                (eltType (undefined :: i))
  eltFlavour _ = EltTuple
  fromElt (a, b, c, d, e, f, g, h, i) = (fromElt (a, b, c, d, e, f, g, h), fromElt i)
  toElt (abcdefgh, i) = let (a, b, c, d, e, f, g, h) = toElt abcdefgh
                        in  (a, b, c, d, e, f, g, h, toElt i)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j)
  => Elt (a, b, c, d, e, f, g, h, i, j) where
  eltType _
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f, g, h, i)))
                (eltType (undefined :: j))
  eltFlavour _ = EltTuple
  fromElt (a, b, c, d, e, f, g, h, i, j) = (fromElt (a, b, c, d, e, f, g, h, i), fromElt j)
  toElt (abcdefghi, j) = let (a, b, c, d, e, f, g, h, i) = toElt abcdefghi
                         in  (a, b, c, d, e, f, g, h, i, toElt j)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k)
  => Elt (a, b, c, d, e, f, g, h, i, j, k) where
  eltType _
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f, g, h, i, j)))
                (eltType (undefined :: k))
  eltFlavour _ = EltTuple
  fromElt (a, b, c, d, e, f, g, h, i, j, k) = (fromElt (a, b, c, d, e, f, g, h, i, j), fromElt k)
  toElt (abcdefghij, k) = let (a, b, c, d, e, f, g, h, i, j) = toElt abcdefghij
                          in  (a, b, c, d, e, f, g, h, i, j, toElt k)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l)
  => Elt (a, b, c, d, e, f, g, h, i, j, k, l) where
  eltType _
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f, g, h, i, j, k)))
                (eltType (undefined :: l))
  eltFlavour _ = EltTuple
  fromElt (a, b, c, d, e, f, g, h, i, j, k, l) = (fromElt (a, b, c, d, e, f, g, h, i, j, k), fromElt l)
  toElt (abcdefghijk, l) = let (a, b, c, d, e, f, g, h, i, j, k) = toElt abcdefghijk
                           in  (a, b, c, d, e, f, g, h, i, j, k, toElt l)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m)
  => Elt (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  eltType _
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f, g, h, i, j, k, l)))
                (eltType (undefined :: m))
  eltFlavour _ = EltTuple
  fromElt (a, b, c, d, e, f, g, h, i, j, k, l, m) = (fromElt (a, b, c, d, e, f, g, h, i, j, k, l), fromElt m)
  toElt (abcdefghijkl, m) = let (a, b, c, d, e, f, g, h, i, j, k, l) = toElt abcdefghijkl
                            in  (a, b, c, d, e, f, g, h, i, j, k, l, toElt m)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n)
  => Elt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  eltType _
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f, g, h, i, j, k, l, m)))
                (eltType (undefined :: n))
  eltFlavour _ = EltTuple
  fromElt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (fromElt (a, b, c, d, e, f, g, h, i, j, k, l, m), fromElt n)
  toElt (abcdefghijklm, n) = let (a, b, c, d, e, f, g, h, i, j, k, l, m) = toElt abcdefghijklm
                             in  (a, b, c, d, e, f, g, h, i, j, k, l, m, toElt n)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n, Elt o)
  => Elt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  eltType _
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f, g, h, i, j, k, l, m, n)))
                (eltType (undefined :: o))
  eltFlavour _ = EltTuple
  fromElt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (fromElt (a, b, c, d, e, f, g, h, i, j, k, l, m, n), fromElt o)
  toElt (abcdefghijklmn, o) = let (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = toElt abcdefghijklmn
                              in  (a, b, c, d, e, f, g, h, i, j, k, l, m, n, toElt o)


-- |Convenience functions
--

singletonScalarType :: IsScalar a => a -> TupleType a
singletonScalarType _ = SingleTuple scalarType

liftToElt :: (Elt a, Elt b)
          => (EltRepr a -> EltRepr b)
          -> (a -> b)
{-# INLINE liftToElt #-}
liftToElt f = toElt . f . fromElt

liftToElt2 :: (Elt a, Elt b, Elt c)
           => (EltRepr a -> EltRepr b -> EltRepr c)
           -> (a -> b -> c)
{-# INLINE liftToElt2 #-}
liftToElt2 f = \x y -> toElt $ f (fromElt x) (fromElt y)

sinkFromElt :: (Elt a, Elt b)
            => (a -> b)
            -> (EltRepr a -> EltRepr b)
{-# INLINE sinkFromElt #-}
sinkFromElt f = fromElt . f . toElt

sinkFromElt2 :: (Elt a, Elt b, Elt c)
             => (a -> b -> c)
             -> (EltRepr a -> EltRepr b -> EltRepr c)
{-# INLINE sinkFromElt2 #-}
sinkFromElt2 f = \x y -> fromElt $ f (toElt x) (toElt y)

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


-- Surface arrays
-- --------------

-- We represent tuples of arrays in the same way as tuples of scalars; using
-- '()' and '(,)' as type-level nil and snoc. This characterises the domain of
-- results of Accelerate array computations.
--
type family ArrRepr a :: *
type instance ArrRepr ()           = ()
type instance ArrRepr (Array sh e) = Array sh e
type instance ArrRepr (a, b)       = TupleRepr (ArrRepr a, ArrRepr b)
type instance ArrRepr (a, b, c)    = TupleRepr (ArrRepr a, ArrRepr b, ArrRepr c)
type instance ArrRepr (a, b, c, d) = TupleRepr (ArrRepr a, ArrRepr b, ArrRepr c, ArrRepr d)
type instance ArrRepr (a, b, c, d, e) = TupleRepr (ArrRepr a, ArrRepr b, ArrRepr c, ArrRepr d, ArrRepr e)
type instance ArrRepr (a, b, c, d, e, f) = TupleRepr (ArrRepr a, ArrRepr b, ArrRepr c, ArrRepr d, ArrRepr e, ArrRepr f)
type instance ArrRepr (a, b, c, d, e, f, g) = TupleRepr (ArrRepr a, ArrRepr b, ArrRepr c, ArrRepr d, ArrRepr e, ArrRepr f, ArrRepr g)
type instance ArrRepr (a, b, c, d, e, f, g, h) = TupleRepr (ArrRepr a, ArrRepr b, ArrRepr c, ArrRepr d, ArrRepr e, ArrRepr f, ArrRepr g, ArrRepr h)
type instance ArrRepr (a, b, c, d, e, f, g, h, i) = TupleRepr (ArrRepr a, ArrRepr b, ArrRepr c, ArrRepr d, ArrRepr e, ArrRepr f, ArrRepr g, ArrRepr h, ArrRepr i)
type instance ArrRepr (a, b, c, d, e, f, g, h, i, j) = TupleRepr (ArrRepr a, ArrRepr b, ArrRepr c, ArrRepr d, ArrRepr e, ArrRepr f, ArrRepr g, ArrRepr h, ArrRepr i, ArrRepr j)
type instance ArrRepr (a, b, c, d, e, f, g, h, i, j, k) = TupleRepr (ArrRepr a, ArrRepr b, ArrRepr c, ArrRepr d, ArrRepr e, ArrRepr f, ArrRepr g, ArrRepr h, ArrRepr i, ArrRepr j, ArrRepr k)
type instance ArrRepr (a, b, c, d, e, f, g, h, i, j, k, l) = TupleRepr (ArrRepr a, ArrRepr b, ArrRepr c, ArrRepr d, ArrRepr e, ArrRepr f, ArrRepr g, ArrRepr h, ArrRepr i, ArrRepr j, ArrRepr k, ArrRepr l)
type instance ArrRepr (a, b, c, d, e, f, g, h, i, j, k, l, m) = TupleRepr (ArrRepr a, ArrRepr b, ArrRepr c, ArrRepr d, ArrRepr e, ArrRepr f, ArrRepr g, ArrRepr h, ArrRepr i, ArrRepr j, ArrRepr k, ArrRepr l, ArrRepr m)
type instance ArrRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = TupleRepr (ArrRepr a, ArrRepr b, ArrRepr c, ArrRepr d, ArrRepr e, ArrRepr f, ArrRepr g, ArrRepr h, ArrRepr i, ArrRepr j, ArrRepr k, ArrRepr l, ArrRepr m, ArrRepr n)
type instance ArrRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = TupleRepr (ArrRepr a, ArrRepr b, ArrRepr c, ArrRepr d, ArrRepr e, ArrRepr f, ArrRepr g, ArrRepr h, ArrRepr i, ArrRepr j, ArrRepr k, ArrRepr l, ArrRepr m, ArrRepr n, ArrRepr o)

type IsAtuple = IsProduct Arrays

fromAtuple :: IsAtuple tup => tup -> TupleRepr tup
fromAtuple = fromProd (Proxy :: Proxy Arrays)

toAtuple :: IsAtuple tup => TupleRepr tup -> tup
toAtuple = toProd (Proxy :: Proxy Arrays)

-- Array type reification
--
data ArraysR arrs where
  ArraysRunit  ::                                   ArraysR ()
  ArraysRarray :: (Shape sh, Elt e) =>              ArraysR (Array sh e)
  ArraysRpair  :: ArraysR arrs1 -> ArraysR arrs2 -> ArraysR (arrs1, arrs2)

data ArraysFlavour arrs where
  ArraysFunit  ::                                          ArraysFlavour ()
  ArraysFarray :: (Shape sh, Elt e)                       => ArraysFlavour (Array sh e)
  ArraysFtuple :: (IsAtuple arrs, ArrRepr arrs ~ (l,r)) => ArraysFlavour arrs

-- | 'Arrays' consists of nested tuples of individual 'Array's, currently up to
-- 15-elements wide. Accelerate computations can thereby return multiple
-- results.
--
class Typeable a => Arrays a where
  arrays   :: a {- dummy -} -> ArraysR (ArrRepr a)
  flavour  :: a {- dummy -} -> ArraysFlavour a
  --
  toArr    :: ArrRepr  a -> a
  fromArr  :: a -> ArrRepr  a


instance Arrays () where
  arrays  _ = ArraysRunit
  flavour _ = ArraysFunit
  --
  toArr     = id
  fromArr   = id

instance (Shape sh, Elt e) => Arrays (Array sh e) where
  arrays _      = ArraysRarray
  flavour _     = ArraysFarray
  --
  toArr         = id
  fromArr       = id

instance (Arrays a, Arrays b) => Arrays (a, b) where
  arrays  _             = ArraysRpair (ArraysRpair ArraysRunit (arrays (undefined::a))) (arrays (undefined::b))
  flavour _             = ArraysFtuple
  --
  toArr    (((),a), b)  = (toArr a, toArr b)
  fromArr  (a, b)       = (((), fromArr a), fromArr b)

instance (Arrays a, Arrays b, Arrays c) => Arrays (a, b, c) where
  arrays  _             = ArraysRpair (arrays (undefined :: (a, b))) (arrays (undefined::c))
  flavour _             = ArraysFtuple
  --
  toArr    (ab, c)      = let (a, b) = toArr ab in (a, b, toArr c)
  fromArr  (a, b, c)    = (fromArr (a, b), fromArr c)

instance (Arrays a, Arrays b, Arrays c, Arrays d) => Arrays (a, b, c, d) where
  arrays  _             = ArraysRpair (arrays (undefined :: (a, b, c))) (arrays (undefined::d))
  flavour _             = ArraysFtuple
  --
  toArr    (abc, d)     = let (a, b, c) = toArr abc in (a, b, c, toArr d)
  fromArr  (a, b, c, d) = (fromArr (a, b, c), fromArr d)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e) => Arrays (a, b, c, d, e) where
  arrays  _             = ArraysRpair (arrays (undefined :: (a, b, c, d))) (arrays (undefined::e))
  flavour _             = ArraysFtuple
  --
  toArr    (abcd, e)    = let (a, b, c, d) = toArr abcd in (a, b, c, d, toArr e)
  fromArr  (a, b, c, d, e) = (fromArr (a, b, c, d), fromArr e)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f)
  => Arrays (a, b, c, d, e, f) where
  arrays  _             = ArraysRpair (arrays (undefined :: (a, b, c, d, e))) (arrays (undefined::f))
  flavour _             = ArraysFtuple
  --
  toArr    (abcde, f)   = let (a, b, c, d, e) = toArr abcde in (a, b, c, d, e, toArr f)
  fromArr  (a, b, c, d, e, f) = (fromArr (a, b, c, d, e), fromArr f)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g)
  => Arrays (a, b, c, d, e, f, g) where
  arrays  _             = ArraysRpair (arrays (undefined :: (a, b, c, d, e, f))) (arrays (undefined::g))
  flavour _             = ArraysFtuple
  --
  toArr    (abcdef, g)   = let (a, b, c, d, e, f) = toArr abcdef in (a, b, c, d, e, f, toArr g)
  fromArr  (a, b, c, d, e, f, g) = (fromArr (a, b, c, d, e, f), fromArr g)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h)
  => Arrays (a, b, c, d, e, f, g, h) where
  arrays  _             = ArraysRpair (arrays (undefined :: (a, b, c, d, e, f, g))) (arrays (undefined::h))
  flavour _             = ArraysFtuple
  --
  toArr    (abcdefg, h)   = let (a, b, c, d, e, f, g) = toArr abcdefg in (a, b, c, d, e, f, g, toArr h)
  fromArr  (a, b, c, d, e, f, g, h) = (fromArr (a, b, c, d, e, f, g), fromArr h)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i)
  => Arrays (a, b, c, d, e, f, g, h, i) where
  arrays  _             = ArraysRpair (arrays (undefined :: (a, b, c, d, e, f, g, h))) (arrays (undefined::i))
  flavour _             = ArraysFtuple
  --
  toArr    (abcdefgh, i) = let (a, b, c, d, e, f, g, h) = toArr abcdefgh in (a, b, c, d, e, f, g, h, toArr i)
  fromArr  (a, b, c, d, e, f, g, h, i) = (fromArr (a, b, c, d, e, f, g, h), fromArr i)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j)
  => Arrays (a, b, c, d, e, f, g, h, i, j) where
  arrays  _             = ArraysRpair (arrays (undefined :: (a, b, c, d, e, f, g, h, i))) (arrays (undefined::j))
  flavour _             = ArraysFtuple
  --
  toArr    (abcdefghi, j) = let (a, b, c, d, e, f, g, h, i) = toArr abcdefghi in (a, b, c, d, e, f, g, h, i, toArr j)
  fromArr  (a, b, c, d, e, f, g, h, i, j) = (fromArr (a, b, c, d, e, f, g, h, i), fromArr j)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k)
  => Arrays (a, b, c, d, e, f, g, h, i, j, k) where
  arrays  _             = ArraysRpair (arrays (undefined :: (a, b, c, d, e, f, g, h, i, j))) (arrays (undefined::k))
  flavour _             = ArraysFtuple
  --
  toArr    (abcdefghij, k) = let (a, b, c, d, e, f, g, h, i, j) = toArr abcdefghij in (a, b, c, d, e, f, g, h, i, j, toArr k)
  fromArr  (a, b, c, d, e, f, g, h, i, j, k) = (fromArr (a, b, c, d, e, f, g, h, i, j), fromArr k)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l)
  => Arrays (a, b, c, d, e, f, g, h, i, j, k, l) where
  arrays  _             = ArraysRpair (arrays (undefined :: (a, b, c, d, e, f, g, h, i, j, k))) (arrays (undefined::l))
  flavour _             = ArraysFtuple
  --
  toArr    (abcdefghijk, l) = let (a, b, c, d, e, f, g, h, i, j, k) = toArr abcdefghijk in (a, b, c, d, e, f, g, h, i, j, k, toArr l)
  fromArr  (a, b, c, d, e, f, g, h, i, j, k, l) = (fromArr (a, b, c, d, e, f, g, h, i, j, k), fromArr l)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m)
  => Arrays (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  arrays  _             = ArraysRpair (arrays (undefined :: (a, b, c, d, e, f, g, h, i, j, k, l))) (arrays (undefined::m))
  flavour _             = ArraysFtuple
  --
  toArr    (abcdefghijkl, m) = let (a, b, c, d, e, f, g, h, i, j, k, l) = toArr abcdefghijkl in (a, b, c, d, e, f, g, h, i, j, k, l, toArr m)
  fromArr  (a, b, c, d, e, f, g, h, i, j, k, l, m) = (fromArr (a, b, c, d, e, f, g, h, i, j, k, l), fromArr m)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m, Arrays n)
  => Arrays (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  arrays  _             = ArraysRpair (arrays (undefined :: (a, b, c, d, e, f, g, h, i, j, k, l, m))) (arrays (undefined::n))
  flavour _             = ArraysFtuple
  --
  toArr    (abcdefghijklm, n) = let (a, b, c, d, e, f, g, h, i, j, k, l, m) = toArr abcdefghijklm in (a, b, c, d, e, f, g, h, i, j, k, l, m, toArr n)
  fromArr  (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (fromArr (a, b, c, d, e, f, g, h, i, j, k, l, m), fromArr n)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m, Arrays n, Arrays o)
  => Arrays (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  arrays  _             = ArraysRpair (arrays (undefined :: (a, b, c, d, e, f, g, h, i, j, k, l, m, n))) (arrays (undefined::o))
  flavour _             = ArraysFtuple
  --
  toArr    (abcdefghijklmn, o) = let (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = toArr abcdefghijklmn in (a, b, c, d, e, f, g, h, i, j, k, l, m, n, toArr o)
  fromArr  (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (fromArr (a, b, c, d, e, f, g, h, i, j, k, l, m, n), fromArr o)


-- {-# RULES
-- "fromArr/toArr" forall a. fromArr (toArr a) = a
-- "toArr/fromArr" forall a. toArr (fromArr a) = a
-- #-}


-- Tuple representation
-- --------------------

-- |The tuple representation is equivalent to the product representation.
--
type TupleRepr a = ProdRepr a

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
type TupleR  a = ProdR Elt    a
type AtupleR a = ProdR Arrays a

tuple :: IsTuple tup => {- dummy -} tup -> TupleR (TupleRepr tup)
tuple = prod (Proxy :: Proxy Elt)

atuple :: IsAtuple tup => {- dummy -} tup -> AtupleR (TupleRepr tup)
atuple = prod (Proxy :: Proxy Arrays)


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
    "Matrix (" ++ showShape (shape arr) ++ ") " ++ showMat (toMatrix (toList arr))
    where
      Z :. _ :. cols    = shape arr
      toMatrix []       = []
      toMatrix xs       = let (r,rs) = splitAt cols xs
                          in  r : toMatrix rs
      --
      showMat []        = "[]"
      showMat mat       = "\n  " ++ ppMat (map (map show) mat)
      --
      ppRow row         = concatMap (++",") row
      ppMat mat         = "[" ++ init (intercalate "\n   " (map ppRow (ppColumns mat))) ++ "]"
      ppColumns         = List.transpose . map (\col -> pad (width col) col) . List.transpose
        where
          extra = 0
          width = maximum . map length
          pad w = map (\x -> replicate (w - length x + extra) ' ' ++ x)
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
      go ArrayEltRcshort       (AD_CShort ua)  = rnf ua
      go ArrayEltRcushort      (AD_CUShort ua) = rnf ua
      go ArrayEltRcint         (AD_CInt ua)    = rnf ua
      go ArrayEltRcuint        (AD_CUInt ua)   = rnf ua
      go ArrayEltRclong        (AD_CLong ua)   = rnf ua
      go ArrayEltRculong       (AD_CULong ua)  = rnf ua
      go ArrayEltRcllong       (AD_CLLong ua)  = rnf ua
      go ArrayEltRcullong      (AD_CULLong ua) = rnf ua
      go ArrayEltRfloat        (AD_Float ua)   = rnf ua
      go ArrayEltRdouble       (AD_Double ua)  = rnf ua
      go ArrayEltRcfloat       (AD_CFloat ua)  = rnf ua
      go ArrayEltRcdouble      (AD_CDouble ua) = rnf ua
      go ArrayEltRbool         (AD_Bool ua)    = rnf ua
      go ArrayEltRchar         (AD_Char ua)    = rnf ua
      go ArrayEltRcchar        (AD_CChar ua)   = rnf ua
      go ArrayEltRcschar       (AD_CSChar ua)  = rnf ua
      go ArrayEltRcuchar       (AD_CUChar ua)  = rnf ua
      go (ArrayEltRpair r1 r2) (AD_Pair a1 a2) = go r1 a1 `seq` go r2 a2 `seq` ()

-- |Scalars arrays hold a single element
--
type Scalar e = Array DIM0 e

-- |Vectors are one-dimensional arrays
--
type Vector e = Array DIM1 e

-- |Segment descriptor (vector of segment lengths).
--
-- To represent nested one-dimensional arrays, we use a flat array of data
-- values in conjunction with a /segment descriptor/, which stores the lengths
-- of the subarrays.
--
type Segments i = Vector i

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
  rank   :: sh -> Int

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

  -- |Offset a shape by another shape
  offset :: sh -> sh -> sh

  -- |Transpose a shape
  transpose :: sh -> sh

  -- |Map a multi-dimensional index into one in a linear, row-major
  -- representation of the array (first argument is the /shape/, second
  -- argument is the index).
  toIndex   :: sh -> sh -> Int

  -- |Inverse of 'toIndex'.
  fromIndex :: sh -> Int -> sh

  -- |Apply a boundary condition to an index.
  bound  :: sh -> sh -> Boundary a -> Either a sh

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
  sliceAnyIndex :: sh -> Repr.SliceIndex (EltRepr (Any sh)) (EltRepr sh) () (EltRepr sh)

  -- | The slice index for specifying a slice with only the Z component projected
  sliceNoneIndex :: sh -> Repr.SliceIndex (EltRepr sh) () (EltRepr sh) (EltRepr sh)

  asSlice :: proxy sh -> AsSlice sh

  shapeType :: proxy sh -> ShapeR sh

  rank                  = Repr.rank . fromElt
  size                  = Repr.size . fromElt
  empty                 = toElt Repr.empty
  -- (#) must be individually defined, as it holds for all instances *except*
  -- the one with the largest arity

  ignore                = toElt Repr.ignore
  intersect sh1 sh2     = toElt (Repr.intersect (fromElt sh1) (fromElt sh2))
  union sh1 sh2         = toElt (Repr.union (fromElt sh1) (fromElt sh2))
  offset sh1 sh2        = toElt (Repr.offset (fromElt sh1) (fromElt sh2))
  transpose             = toElt . Repr.transpose . fromElt
  fromIndex sh ix       = toElt (Repr.fromIndex (fromElt sh) ix)
  toIndex sh ix         = Repr.toIndex (fromElt sh) (fromElt ix)

  bound sh ix bndy      = case Repr.bound (fromElt sh) (fromElt ix) bndy of
                            Left v    -> Left v
                            Right ix' -> Right $ toElt ix'

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

data AsSlice sh = Slice sh => AsSlice

data ShapeR sh where
  ShapeRnil  :: ShapeR Z
  ShapeRcons :: Shape sh => ShapeR sh -> ShapeR (sh :. Int)

data SliceR sh where
  SliceRnil   :: SliceR Z
  SliceRall   :: (Slice sl) => SliceR sl -> SliceR (sl :. All)
  SliceRfixed :: (Slice sl) => SliceR sl -> SliceR (sl :. Int)
  SliceRany   :: Shape sh => SliceR (Any sh)

instance Shape Z where
  sliceAnyIndex  _ = Repr.SliceNil
  sliceNoneIndex _ = Repr.SliceNil
  asSlice _ = AsSlice
  shapeType _ = ShapeRnil

instance Shape sh => Shape (sh:.Int) where
  sliceAnyIndex  _ = Repr.SliceAll   (sliceAnyIndex  (undefined :: sh))
  sliceNoneIndex _ = Repr.SliceFixed (sliceNoneIndex (undefined :: sh))
  asSlice _ | AsSlice <- asSlice (Proxy :: Proxy sh)
            = AsSlice
  shapeType _ = ShapeRcons (shapeType (Proxy :: Proxy sh))

-- | Slices, aka generalised indices, as /n/-tuples and mappings of slice
-- indices to slices, co-slices, and slice dimensions
--
class (Elt sl, Shape (SliceShape sl), Shape (CoSliceShape sl), Shape (FullShape sl))
       => Slice sl where
  type SliceShape   sl :: *     -- the projected slice
  type CoSliceShape sl :: *     -- the complement of the slice
  type FullShape    sl :: *     -- the combined dimension
  sliceIndex :: sl {- dummy -} -> Repr.SliceIndex (EltRepr sl)
                                    (EltRepr (SliceShape   sl))
                                    (EltRepr (CoSliceShape sl))
                                    (EltRepr (FullShape    sl))
  -- | Increment a slice index into the given shape by 'n'.
  --
  toSlice :: FullShape sl -> Int -> sl

  sliceType :: proxy sl -> SliceR sl

instance Slice Z where
  type SliceShape   Z = Z
  type CoSliceShape Z = Z
  type FullShape    Z = Z
  sliceIndex _ = Repr.SliceNil
  toSlice _ _ = Z
  sliceType _ = SliceRnil

instance Slice sl => Slice (sl:.All) where
  type SliceShape   (sl:.All) = SliceShape   sl :. Int
  type CoSliceShape (sl:.All) = CoSliceShape sl
  type FullShape    (sl:.All) = FullShape    sl :. Int
  sliceIndex _ = Repr.SliceAll (sliceIndex (undefined :: sl))
  toSlice (sh :. _) i = toSlice sh i :. All
  sliceType _ = SliceRall (sliceType (Proxy :: Proxy sl))

instance Slice sl => Slice (sl:.Int) where
  type SliceShape   (sl:.Int) = SliceShape   sl
  type CoSliceShape (sl:.Int) = CoSliceShape sl :. Int
  type FullShape    (sl:.Int) = FullShape    sl :. Int
  sliceIndex _ = Repr.SliceFixed (sliceIndex (undefined :: sl))
  toSlice (sh :. n) i' = toSlice sh (i' `div` n) :. (i' `mod` n)
  sliceType _ = SliceRfixed (sliceType (Proxy :: Proxy sl))

instance Shape sh => Slice (Any sh) where
  type SliceShape   (Any sh) = sh
  type CoSliceShape (Any sh) = Z
  type FullShape    (Any sh) = sh
  sliceIndex _ = sliceAnyIndex (undefined :: sh)
  toSlice _ _ = Any
  sliceType _ = SliceRany

-- Actual tuple instances for slices and shapes
--
instance (Elt sh, Elt i) => IsProduct Elt (sh:.i) where
  type ProdRepr (sh:.i) = (((),sh),i)
  fromProd _ (sh:.i)   = (((),sh),i)
  toProd _ (((),sh),i) = (sh:.i)
  prod _ _             = ProdRsnoc (ProdRsnoc ProdRunit)


-- | Generalised array division, like above but use for splitting an array into
-- many subarrays, as opposed to extracting a single subarray.
--
class (Slice (DivisionSlice sl))
       => Division sl where
  type DivisionSlice sl :: *     -- the slice
  slicesIndex :: slix ~ DivisionSlice sl
              => sl {- dummy -}
              -> Repr.SliceIndex (EltRepr slix)
                                 (EltRepr (SliceShape   slix))
                                 (EltRepr (CoSliceShape slix))
                                 (EltRepr (FullShape    slix))

instance Division Z where
  type DivisionSlice   Z = Z
  slicesIndex _ = Repr.SliceNil

instance Division sl => Division (sl:.All) where
  type DivisionSlice  (sl:.All) = DivisionSlice sl :. All
  slicesIndex _ = Repr.SliceAll (slicesIndex (undefined :: sl))

instance Division sl => Division (sl:.Split) where
  type DivisionSlice (sl:.Split) = DivisionSlice sl :. Int
  slicesIndex _ = Repr.SliceFixed (slicesIndex (undefined :: sl))

instance Shape sh => Division (Any sh) where
  type DivisionSlice (Any sh) = Any sh
  slicesIndex _ = sliceAnyIndex (undefined :: sh)

instance (Shape sh, Slice sh) => Division (Divide sh) where
  type DivisionSlice (Divide sh) = sh
  slicesIndex _ = sliceNoneIndex (undefined :: sh)


-- | Ensures the rank of one shape is less than the rank of another.
--
class sh1 :<= sh2 where
  maximumRank :: sh1 :<=: sh2

infix 2 :<=

instance Z :<= sh where
  maximumRank = RankZ

instance (sh1 :<= sh2) => (sh1:.Int) :<= (sh2:.Int) where
  maximumRank = RankSnoc maximumRank

infix 2 :<=:

data sh1 :<=: sh2 where
  RankZ :: Z :<=: sh
  RankSnoc :: sh1 :<=: sh2 -> sh1:.Int :<=: sh2:.Int

-- Array operations
-- ----------------

-- |Yield an array's shape
--
shape :: Shape sh => Array sh e -> sh
shape (Array sh _) = toElt sh

-- | Array indexing
--
infixl 9 !
(!) :: Array sh e -> sh -> e
{-# INLINE (!) #-}
-- (Array sh adata) ! ix = toElt (adata `indexArrayData` index sh ix)
-- FIXME: using this due to a bug in 6.10.x
(!) (Array sh adata) ix = toElt (adata `unsafeIndexArrayData` toIndex (toElt sh) ix)

-- | Create an array from its representation function, applied at each index of
-- the array.
--
fromFunction :: (Shape sh, Elt e) => sh -> (sh -> e) -> Array sh e
{-# INLINE fromFunction #-}
fromFunction sh f = adata `seq` Array (fromElt sh) adata
  where
    (adata, _) = runArrayData $ do
                   arr <- newArrayData (size sh)
                   let write ix = unsafeWriteArrayData arr (toIndex sh ix)
                                                           (fromElt (f ix))
                   iter sh write (>>) (return ())
                   return (arr, undefined)


-- | Create a vector from the concatenation of the given list of vectors.
--
concatVectors :: Elt e => [Vector e] -> Vector e
{-# INLINE concatVectors #-}
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
allocateArray :: (Shape sh, Elt e) => sh -> IO (Array sh e)
{-# INLINE allocateArray #-}
allocateArray sh = adata `seq` return (Array (fromElt sh) adata)
  where
    (adata, _) = runArrayData $ (,undefined) `fmap` newArrayData (size sh)


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
-- >>> fromList (Z:.5:.10) (repeat 0) :: Array DIM2 Float
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
fromList :: (Shape sh, Elt e) => sh -> [e] -> Array sh e
{-# INLINE fromList #-}
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
toList :: forall sh e. Array sh e -> [e]
{-# INLINE toList #-}
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
-- > let slix = sliceIndex (undefined :: Z :. Int :. Int :. All)
-- >     sh   = Z :. 2 :. 3 :. 1 :: DIM3
-- > in
-- > enumSlices slix sh :: [ Z :. Int :. Int :. All ]
--
enumSlices :: forall slix co sl dim. (Elt slix, Elt dim)
           => Repr.SliceIndex (EltRepr slix) sl co (EltRepr dim)
           -> dim    -- Bounds
           -> [slix] -- All slices within bounds.
enumSlices slix = map toElt . Repr.enumSlices slix . fromElt
