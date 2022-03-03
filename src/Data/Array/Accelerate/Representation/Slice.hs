{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Representation.Slice
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Representation.Slice
  where

import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Type

import Language.Haskell.TH.Extra


-- | Class of slice representations (which are nested pairs)
--
class Slice sl where
  type SliceShape    sl      -- the projected slice
  type CoSliceShape  sl      -- the complement of the slice
  type FullShape     sl      -- the combined dimension
  sliceIndex :: SliceIndex sl (SliceShape sl) (CoSliceShape sl) (FullShape sl)

instance Slice () where
  type SliceShape    () = ()
  type CoSliceShape  () = ()
  type FullShape     () = ()
  sliceIndex = SliceNil

instance Slice sl => Slice (sl, ()) where
  type SliceShape   (sl, ()) = (SliceShape  sl, SingletonType Int)
  type CoSliceShape (sl, ()) = CoSliceShape sl
  type FullShape    (sl, ()) = (FullShape   sl, SingletonType Int)
  sliceIndex = SliceAll (sliceIndex @sl)

instance Slice sl => Slice (sl, SingletonType Int) where
  type SliceShape   (sl, SingletonType Int) = SliceShape sl
  type CoSliceShape (sl, SingletonType Int) = (CoSliceShape sl, SingletonType Int)
  type FullShape    (sl, SingletonType Int) = (FullShape    sl, SingletonType Int)
  sliceIndex = SliceFixed (sliceIndex @sl)

-- |Generalised array index, which may index only in a subset of the dimensions
-- of a shape.
--
data SliceIndex ix slice coSlice sliceDim where
  SliceNil   :: SliceIndex () () () ()
  SliceAll   :: SliceIndex ix slice co dim -> SliceIndex (ix, ()) (slice, SingletonType Int) co       (dim, SingletonType Int)
  SliceFixed :: SliceIndex ix slice co dim -> SliceIndex (ix, SingletonType Int) slice      (co, SingletonType Int) (dim, SingletonType Int)

instance Show (SliceIndex ix slice coSlice sliceDim) where
  show SliceNil          = "SliceNil"
  show (SliceAll rest)   = "SliceAll (" ++ show rest ++ ")"
  show (SliceFixed rest) = "SliceFixed (" ++ show rest ++ ")"

-- | Project the shape of a slice from the full shape.
--
sliceShape :: SliceIndex slix sl co dim -> dim -> sl
sliceShape SliceNil          ()      = ()
sliceShape (SliceAll   slix) (sh, n) = (sliceShape slix sh, n)
sliceShape (SliceFixed slix) (sh, _) = sliceShape slix sh

-- | Project the full shape of the slice
--
sliceDomain :: SliceIndex slix sl co dim -> slix -> sl -> dim
sliceDomain SliceNil          ()        ()       = ()
sliceDomain (SliceAll slix)   (slx, ()) (sl, sz) = (sliceDomain slix slx sl, sz)
sliceDomain (SliceFixed slix) (slx, sz) sl       = (sliceDomain slix slx sl, sz)

sliceShapeR :: SliceIndex slix sl co dim -> ShapeR sl
sliceShapeR SliceNil        = ShapeRz
sliceShapeR (SliceAll sl)   = ShapeRsnoc $ sliceShapeR sl
sliceShapeR (SliceFixed sl) = sliceShapeR sl

sliceDomainR :: SliceIndex slix sl co dim -> ShapeR dim
sliceDomainR SliceNil        = ShapeRz
sliceDomainR (SliceAll sl)   = ShapeRsnoc $ sliceDomainR sl
sliceDomainR (SliceFixed sl) = ShapeRsnoc $ sliceDomainR sl

-- | Enumerate all slices within a given bound. The innermost dimension changes
-- most rapidly.
--
-- See 'Data.Array.Accelerate.Sugar.Slice.enumSlices' for an example.
--
enumSlices
    :: forall slix co sl dim.
       SliceIndex slix sl co dim
    -> dim
    -> [slix]
enumSlices SliceNil        ()       = [()]
enumSlices (SliceAll   sl) (sh, _)  = [ (sh', ()) | sh' <- enumSlices sl sh]
enumSlices (SliceFixed sl) (sh, n)  = [ (sh', i)  | sh' <- enumSlices sl sh, i <- [0..n-1]]

-- These functions and the Num, Enum instance make sure we can use the range
-- syntax used above. We might have to provide these instances for all
-- SingletonTypes maybe?
liftSingNumBinary :: (Elt a, EltR a ~ SingletonType a) => (a -> a -> a) -> SingletonType a -> SingletonType a -> SingletonType a
liftSingNumBinary f x y = fromElt $ f (toElt x) (toElt y)

liftSingNumUnary :: (Elt a, EltR a ~ SingletonType a) => (a -> a) -> SingletonType a -> SingletonType a
liftSingNumUnary f x = fromElt $ f (toElt x)

instance Num (SingletonType Int) where
  (+) = liftSingNumBinary @Int (+)
  (*) = liftSingNumBinary @Int (*)
  (-) = liftSingNumBinary @Int (-)
  abs = liftSingNumUnary @Int abs
  signum = liftSingNumUnary @Int abs
  fromInteger = fromElt . fromInteger @Int

instance Enum (SingletonType Int) where
  toEnum = fromElt
  fromEnum = toElt

rnfSliceIndex :: SliceIndex ix slice co sh -> ()
rnfSliceIndex SliceNil        = ()
rnfSliceIndex (SliceAll sh)   = rnfSliceIndex sh
rnfSliceIndex (SliceFixed sh) = rnfSliceIndex sh

liftSliceIndex :: SliceIndex ix slice co sh -> CodeQ (SliceIndex ix slice co sh)
liftSliceIndex SliceNil          = [|| SliceNil ||]
liftSliceIndex (SliceAll rest)   = [|| SliceAll $$(liftSliceIndex rest) ||]
liftSliceIndex (SliceFixed rest) = [|| SliceFixed $$(liftSliceIndex rest) ||]

