{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Sugar.Array
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Sugar.Array
  where

import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Representation.Type
import qualified Data.Array.Accelerate.Representation.Array         as R

import Control.DeepSeq
import Data.Kind
import Data.Typeable
import Language.Haskell.TH                                          hiding ( Type )
import Language.Haskell.TH.Extra
import System.IO.Unsafe

import GHC.Exts                                                     ( IsList, IsString )
import GHC.Generics
import qualified GHC.Exts                                           as GHC

-- $setup
-- >>> :seti -XOverloadedLists


type Scalar = Array DIM0    -- ^ Scalar arrays hold a single element
type Vector = Array DIM1    -- ^ Vectors are one-dimensional arrays
type Matrix = Array DIM2    -- ^ Matrices are two-dimensional arrays

-- | Segment descriptor (vector of segment lengths)
--
-- To represent nested one-dimensional arrays, we use a flat array of data
-- values in conjunction with a /segment descriptor/, which stores the
-- lengths of the sub-arrays.
--
type Segments = Vector


-- | Dense, regular, multi-dimensional arrays.
--
-- The 'Array' is the core computational unit of Accelerate; all programs
-- in Accelerate take zero or more arrays as input and produce one or more
-- arrays as output. The 'Array' type has two type parameters:
--
--  * /sh/: is the shape of the array, tracking the dimensionality and extent of
--    each dimension of the array; for example, 'DIM1' for one-dimensional
--    'Vector's, 'DIM2' for two-dimensional matrices, and so on.
--
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
--  * Nested tuples of all of these, currently up to 16-elements wide.
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
newtype Array sh e = Array (R.Array (EltR sh) (EltR e))
  deriving Typeable

instance (Shape sh, Elt e, Eq sh, Eq e) => Eq (Array sh e) where
  arr1 == arr2 = shape arr1 == shape arr2 && toList arr1 == toList arr2
  arr1 /= arr2 = shape arr1 /= shape arr2 || toList arr1 /= toList arr2

instance (Shape sh, Elt e, Show e) => Show (Array sh e) where
  show (Array arr) = R.showArray (shows . toElt @e) (arrayR @sh @e) arr

instance Elt e => IsList (Vector e) where
  type Item (Vector e) = e
  toList      = toList
  fromListN n = fromList (Z:.n)
  fromList xs = GHC.fromListN (length xs) xs

instance IsString (Vector Char) where
  fromString s = fromList (Z :. length s) s

instance (Shape sh, Elt e) => NFData (Array sh e) where
  rnf (Array arr) = R.rnfArray (arrayR @sh @e) arr

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

-- | Yield an array's shape
--
shape :: Shape sh => Array sh e -> sh
shape (Array arr) = toElt (R.shape arr)

-- | Change the shape of an array without altering its contents. The 'size' of
-- the source and result arrays must be identical.
--
reshape :: forall sh sh' e. (Shape sh, Shape sh') => sh -> Array sh' e -> Array sh e
reshape sh (Array arr) = Array $ R.reshape (shapeR @sh) (fromElt sh) (shapeR @sh') arr

-- | Return the value of an array at the given multidimensional index
--
infixl 9 !
(!) :: forall sh e. (Shape sh, Elt e) => Array sh e -> sh -> e
(!) (Array arr) ix = toElt $ R.indexArray (arrayR @sh @e) arr (fromElt ix)

-- | Return the value of an array at given the linear (row-major) index
--
infixl 9 !!
(!!) :: forall sh e. Elt e => Array sh e -> Int -> e
(!!) (Array arr) i = toElt $ R.linearIndexArray (eltR @e) arr i

-- | Create an array from its representation function, applied at each
-- index of the array
--
fromFunction :: (Shape sh, Elt e) => sh -> (sh -> e) -> Array sh e
fromFunction sh f = unsafePerformIO $! fromFunctionM sh (return . f)

-- | Create an array using a monadic function applied at each index
--
-- @since 1.2.0.0
--
fromFunctionM :: forall sh e. (Shape sh, Elt e) => sh -> (sh -> IO e) -> IO (Array sh e)
fromFunctionM sh f = Array <$> R.fromFunctionM (arrayR @sh @e) (fromElt sh) f'
  where
    f' x = do
      y <- f (toElt x)
      return (fromElt y)

-- | Create a vector from the concatenation of the given list of vectors
--
concatVectors :: forall e. Elt e => [Vector e] -> Vector e
concatVectors = toArr . R.concatVectors (eltR @e) . map fromArr

-- | Creates a new, uninitialized Accelerate array
--
allocateArray :: forall sh e. (Shape sh, Elt e) => sh -> IO (Array sh e)
allocateArray sh = Array <$> R.allocateArray (arrayR @sh @e) (fromElt sh)

-- | Convert elements of a list into an Accelerate 'Array'
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
fromList :: forall sh e. (Shape sh, Elt e) => sh -> [e] -> Array sh e
fromList sh xs = toArr $ R.fromList (arrayR @sh @e) (fromElt sh) $ map fromElt xs

-- | Convert an accelerated 'Array' to a list in row-major order
--
toList :: forall sh e. (Shape sh, Elt e) => Array sh e -> [e]
toList = map toElt . R.toList (arrayR @sh @e) . fromArr


-- | The 'Arrays' class characterises the types which can appear in collective
-- Accelerate computations of type 'Data.Array.Accelerate.Acc'.
--
-- 'Arrays' consists of nested tuples of individual 'Array's, currently up
-- to 16-elements wide. Accelerate computations can thereby return multiple
-- results.
--
class Arrays a where
  -- | Type representation mapping, which explains how to convert from the
  -- surface type into the internal representation type, which consists
  -- only of 'Array', and '()' and '(,)' as type-level nil and snoc.
  --
  type ArraysR a :: Type
  type ArraysR a = GArraysR () (Rep a)

  arraysR :: R.ArraysR (ArraysR a)
  toArr   :: ArraysR a -> a
  fromArr :: a -> ArraysR a

  default arraysR
    :: (GArrays (Rep a), ArraysR a ~ GArraysR () (Rep a))
    => R.ArraysR (ArraysR a)
  arraysR = garrays @(Rep a) TupRunit

  default toArr
    :: (Generic a, GArrays (Rep a), ArraysR a ~ GArraysR () (Rep a))
    => ArraysR a -> a
  toArr = to . snd . gtoArr @(Rep a) @()

  default fromArr
    :: (Generic a, GArrays (Rep a), ArraysR a ~ GArraysR () (Rep a))
    => a -> ArraysR a
  fromArr = (`gfromArr` ()) . from

arrayR :: forall sh e. (Shape sh, Elt e) => R.ArrayR (R.Array (EltR sh) (EltR e))
arrayR = R.ArrayR (shapeR @sh) (eltR @e)

class GArrays f where
  type GArraysR t f
  garrays  :: R.ArraysR t -> R.ArraysR (GArraysR t f)
  gfromArr :: f a -> t -> GArraysR t f
  gtoArr   :: GArraysR t f -> (t, f a)

instance GArrays U1 where
  type GArraysR t U1 = t
  garrays       =  id
  gfromArr U1   =  id
  gtoArr      t = (t, U1)

instance GArrays a => GArrays (M1 i c a) where
  type GArraysR t (M1 i c a) = GArraysR t a
  garrays         = garrays @a
  gfromArr (M1 x) = gfromArr x
  gtoArr       x  = let (t, x1) = gtoArr x in (t, M1 x1)

instance Arrays a => GArrays (K1 i a) where
  type GArraysR t (K1 i a) = (t, ArraysR a)
  garrays         t = TupRpair t (arraysR @a)
  gfromArr (K1 x) t = (t, fromArr x)
  gtoArr   (t, x)   = (t, K1 (toArr x))

instance (GArrays a, GArrays b) => GArrays (a :*: b) where
  type GArraysR t (a :*: b) = GArraysR (GArraysR t a) b
  garrays            = garrays @b . garrays @a
  gfromArr (a :*: b) = gfromArr b . gfromArr a
  gtoArr t =
    let (t1, b) = gtoArr t
        (t2, a) = gtoArr t1
    in
    (t2, a :*: b)


instance Arrays () where
  type ArraysR () = ()
  arraysR = TupRunit
  fromArr = id
  toArr   = id

instance (Shape sh, Elt e) => Arrays (Array sh e) where
  type ArraysR (Array sh e) = R.Array (EltR sh) (EltR e)
  arraysR = R.arraysRarray (shapeR @sh) (eltR @e)
  fromArr (Array arr) = arr
  toArr               = Array

runQ $ do
  let
      mkTuple :: Int -> Q Dec
      mkTuple n =
        let
            xs  = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
            ts  = map varT xs
            res = tupT ts
            ctx = mapM (appT [t| Arrays |]) ts
        in
        instanceD ctx [t| Arrays $res |] []
  --
  mapM mkTuple [2..16]

