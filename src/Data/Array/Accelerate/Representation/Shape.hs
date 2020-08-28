{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Representation.Shape
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Representation.Shape
  where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Representation.Type

import Language.Haskell.TH
import Prelude                                                      hiding ( zip )

import GHC.Base                                                     ( quotInt, remInt )


-- | Shape and index representations as nested pairs
--
data ShapeR sh where
  ShapeRz    :: ShapeR ()
  ShapeRsnoc :: ShapeR sh -> ShapeR (sh, Int)

-- | Nicely format a shape as a string
--
showShape :: ShapeR sh -> sh -> String
showShape shr = foldr (\sh str -> str ++ " :. " ++ show sh) "Z" . shapeToList shr

-- Synonyms for common shape types
--
type DIM0 = ()
type DIM1 = ((), Int)
type DIM2 = (((), Int), Int)
type DIM3 = ((((), Int), Int), Int)

dim0 :: ShapeR DIM0
dim0 = ShapeRz

dim1 :: ShapeR DIM1
dim1 = ShapeRsnoc dim0

dim2 :: ShapeR DIM2
dim2 = ShapeRsnoc dim1

dim3 :: ShapeR DIM3
dim3 = ShapeRsnoc dim2

-- | Number of dimensions of a /shape/ or /index/ (>= 0)
--
rank :: ShapeR sh -> Int
rank ShapeRz          = 0
rank (ShapeRsnoc shr) = rank shr + 1

-- | Total number of elements in an array of the given shape
--
size :: ShapeR sh -> sh -> Int
size ShapeRz () = 1
size (ShapeRsnoc shr) (sh, sz)
  | sz <= 0   = 0
  | otherwise = size shr sh * sz

-- | The empty shape
--
empty :: ShapeR sh -> sh
empty ShapeRz          = ()
empty (ShapeRsnoc shr) = (empty shr, 0)

-- | Yield the intersection of two shapes
--
intersect :: ShapeR sh -> sh -> sh -> sh
intersect = zip min

-- | Yield the union of two shapes
--
union :: ShapeR sh -> sh -> sh -> sh
union = zip max

zip :: (Int -> Int -> Int) -> ShapeR sh -> sh -> sh -> sh
zip _ ShapeRz          ()      ()      = ()
zip f (ShapeRsnoc shr) (as, a) (bs, b) = (zip f shr as bs, f a b)

eq :: ShapeR sh -> sh -> sh -> Bool
eq ShapeRz          ()      ()        = True
eq (ShapeRsnoc shr) (sh, i) (sh', i') = i == i' && eq shr sh sh'


-- | Map a multi-dimensional index into one in a linear, row-major
-- representation of the array (first argument is the /shape/, second
-- argument is the /index/).
--
toIndex :: HasCallStack => ShapeR sh -> sh -> sh -> Int
toIndex ShapeRz () () = 0
toIndex (ShapeRsnoc shr) (sh, sz) (ix, i)
  = indexCheck i sz
  $ toIndex shr sh ix * sz + i

-- | Inverse of 'toIndex'
--
fromIndex :: HasCallStack => ShapeR sh -> sh -> Int -> sh
fromIndex ShapeRz () _ = ()
fromIndex (ShapeRsnoc shr) (sh, sz) i
  = (fromIndex shr sh (i `quotInt` sz), r)
  -- If we assume that the index is in range, there is no point in computing
  -- the remainder for the highest dimension since i < sz must hold.
  --
  where
    r = case shr of -- Check if rank of shr is 0
      ShapeRz -> indexCheck i sz i
      _       -> i `remInt` sz

-- | Iterate through the entire shape, applying the function in the second
-- argument; third argument combines results and fourth is an initial value
-- that is combined with the results; the index space is traversed in
-- row-major order
--
iter :: ShapeR sh -> sh -> (sh -> a) -> (a -> a -> a) -> a -> a
iter ShapeRz          ()       f _ _ = f ()
iter (ShapeRsnoc shr) (sh, sz) f c r = iter shr sh (\ix -> iter' (ix,0)) c r
  where
    iter' (ix,i) | i >= sz   = r
                 | otherwise = f (ix,i) `c` iter' (ix,i+1)

-- | Variant of 'iter' without an initial value
--
iter1 :: HasCallStack => ShapeR sh -> sh -> (sh -> a) -> (a -> a -> a) -> a
iter1 ShapeRz          ()       f _ = f ()
iter1 (ShapeRsnoc _  ) (_,  0)  _ _ = boundsError "empty iteration space"
iter1 (ShapeRsnoc shr) (sh, sz) f c = iter1 shr sh (\ix -> iter1' (ix,0)) c
  where
    iter1' (ix,i) | i == sz-1 = f (ix,i)
                  | otherwise = f (ix,i) `c` iter1' (ix,i+1)

-- Operations to facilitate conversion with IArray

-- | Convert a minpoint-maxpoint index into a shape
--
rangeToShape :: ShapeR sh -> (sh, sh) -> sh
rangeToShape ShapeRz          ((), ())                 = ()
rangeToShape (ShapeRsnoc shr) ((sh1, sz1), (sh2, sz2)) = (rangeToShape shr (sh1, sh2), sz2 - sz1 + 1)

-- | Converse of 'rangeToShape'
--
shapeToRange :: ShapeR sh -> sh -> (sh, sh)
shapeToRange ShapeRz          ()       = ((), ())
shapeToRange (ShapeRsnoc shr) (sh, sz) = let (low, high) = shapeToRange shr sh in ((low, 0), (high, sz - 1))

-- | Convert a shape or index into its list of dimensions
--
shapeToList :: ShapeR sh -> sh -> [Int]
shapeToList ShapeRz          ()      = []
shapeToList (ShapeRsnoc shr) (sh,sz) = sz : shapeToList shr sh

-- | Convert a list of dimensions into a shape
--
listToShape :: HasCallStack => ShapeR sh -> [Int] -> sh
listToShape shr ds =
  case listToShape' shr ds of
    Just sh -> sh
    Nothing -> error "listToShape: unable to convert list to a shape at the specified type"

-- | Attempt to convert a list of dimensions into a shape
--
listToShape' :: ShapeR sh -> [Int] -> Maybe sh
listToShape' ShapeRz          []     = Just ()
listToShape' (ShapeRsnoc shr) (x:xs) = (, x) <$> listToShape' shr xs
listToShape' _                _      = Nothing

shapeType :: ShapeR sh -> TypeR sh
shapeType ShapeRz          = TupRunit
shapeType (ShapeRsnoc shr) =
  shapeType shr
  `TupRpair`
  TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt)))

rnfShape :: ShapeR sh -> sh -> ()
rnfShape ShapeRz          ()      = ()
rnfShape (ShapeRsnoc shr) (sh, s) = s `seq` rnfShape shr sh

rnfShapeR :: ShapeR sh -> ()
rnfShapeR ShapeRz          = ()
rnfShapeR (ShapeRsnoc shr) = rnfShapeR shr

liftShapeR :: ShapeR sh -> Q (TExp (ShapeR sh))
liftShapeR ShapeRz         = [|| ShapeRz ||]
liftShapeR (ShapeRsnoc sh) = [|| ShapeRsnoc $$(liftShapeR sh) ||]

