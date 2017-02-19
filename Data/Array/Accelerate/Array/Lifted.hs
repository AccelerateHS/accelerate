{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE DeriveDataTypeable    #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.Array.Lifted
-- Copyright   : [2012..2013] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell, Robert Clifton-Everest
-- License     : BSD3
--
-- Maintainer  : Robert Clifton-Everest <robertce@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Flattened array representation
--

module Data.Array.Accelerate.Array.Lifted (

  LiftedType(..), LiftedTupleType(..), avoidedType,

  RegularArray,
  IrregularArray, Segments,

  VectorisedForeign(..), isVectorisedForeign,

  divide, concatRegular, concatIrregular,

) where

import Prelude                                                  hiding ( concat )
import Control.Monad                                            ( zipWithM_, foldM_ )
import Data.Typeable
import System.IO.Unsafe                                         ( unsafePerformIO )

-- friends
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar                        hiding ( Segments )

-- The lifted type relationship
-- ----------------------------
--

-- |Captures the relationship between a type and it's possible lifted
-- alternatives. For example, the type @Array sh e@ has the lifted equivalent
-- types @Array sh e@, @Regular (Array sh e)@, and @Irregular (Array sh e)@.
--
-- In the case of the product type @(a,b)@, the related types are the cartesian
-- product of the related types of @a@ and the related types of @b@.
--
data LiftedType t t' where
  UnitT       ::                      LiftedType ()           ()
  LiftedUnitT ::                      LiftedType ()           (Scalar Int)
  AvoidedT    :: (Shape sh, Elt e) => LiftedType (Array sh e) (Array sh e)
  RegularT    :: (Shape sh, Elt e) => LiftedType (Array sh e) (RegularArray sh e)
  IrregularT  :: (Shape sh, Elt e) => LiftedType (Array sh e) (IrregularArray sh e)
  TupleT      :: (IsProduct Arrays t, IsProduct Arrays t', ArrRepr t ~ (a,b))
              => LiftedTupleType (TupleRepr t) (TupleRepr t')
              -> LiftedType t t'

data LiftedTupleType t t' where
  NilLtup  :: LiftedTupleType () ()
  SnocLtup :: (Arrays a, Arrays a')
           => LiftedTupleType t t'
           -> LiftedType a a'
           -> LiftedTupleType (t,a) (t',a')

deriving instance (Eq (LiftedTupleType t t'))
deriving instance (Eq (LiftedType t t'))
deriving instance (Show (LiftedTupleType t t'))
deriving instance (Show (LiftedType t t'))

-- For any type a, generate a witness that a is a lifted type for a.
--
avoidedType :: forall a. Arrays a
            => LiftedType a a
avoidedType =
  case flavour (undefined :: a) of
    ArraysFunit -> UnitT
    ArraysFarray -> AvoidedT
    ArraysFtuple -> TupleT (tup (prod Proxy (undefined :: a)))
  where
    tup :: ProdR Arrays t -> LiftedTupleType t t
    tup ProdRunit     = NilLtup
    tup (ProdRsnoc t) = SnocLtup (tup t) avoidedType

-- Nested arrays
-- ----------------
--
-- We have two different forms of nested array which we use as sequence chunks.

-- Regular chunks. All subarray are of the same extent and are concatenated
-- together on the outer dimension.
--
type RegularArray sh e = Array (sh:.Int) e

-- Irregular chunks. Subarrays can vary in extent.
--
type IrregularArray sh e = (Segments sh, Vector e)

-- Segment descriptors. The segments descriptors of an irregular chunk capture
-- the offset and extent of each subarray. We split this up into two vectors as
-- opposed to a vector of pairs to aid in fusion.
--
-- We also keep track of the total size of the chunk (in terms of scalar
-- elements) the segment descriptors refer to. If we do not do this, we are
-- forced get the last offset and the last size and add them together. By
-- indexing the offsets and the extents in this way we could force them both to
-- be manifest, even though we may only care about one of their elements.
--
type Segments sh = ( Scalar Int  -- Total size in scalar elements
                   , Vector Int  -- Offsets
                   , Vector sh   -- Extents
                   )


-- Vectorised foreign functions.
--

data VectorisedForeign asm where
  VectorisedForeign :: (Foreign asm, Arrays a, Arrays b)
                    => (forall b' a'. Arrays a' => LiftedType a a' -> LiftedType b b' -> asm (a' -> b'))
                    -> VectorisedForeign (a -> b)
  deriving Typeable

instance Foreign VectorisedForeign where
  strForeign (VectorisedForeign f) = strForeign (f avoidedType avoidedType)

isVectorisedForeign :: (Typeable asm, Foreign f)
                    => f asm
                    -> Maybe (VectorisedForeign asm)
isVectorisedForeign = cast


divide :: LiftedType a a' -> a' -> [a]
divide UnitT       _ = [()]
divide LiftedUnitT a = replicate (a ! Z) ()
divide AvoidedT    a = [a]
divide RegularT    a = divideRegular a
divide IrregularT  a = divideIrregular a
divide (TupleT t)  a = map toAtuple (divideT t (fromAtuple a))
  where
    divideT :: LiftedTupleType t t' -> t' -> [t]
    divideT NilLtup          ()    = repeat ()
    divideT (SnocLtup lt ty) (t,a) = zip (divideT lt t) (divide ty a)

divideRegular :: forall sh e. Shape sh => Array (sh:.Int) e -> [Array sh e]
divideRegular arr@(Array _ adata) = [Array (fromElt sh') (copy (i * size sh') (size sh')) | i <- [0..n-1]]
  where
    sh  = shapeToList (shape arr)
    n   = last sh
    --
    sh' :: sh
    sh' = listToShape (init sh)
    --
    {-# NOINLINE copy #-}
    copy start n = unsafePerformIO $ do
      dst <- newArrayData n
      unsafeCopyArrayData adata dst start 0 n
      touchArrayData adata
      unsafeFreezeArrayData dst
      return dst

divideIrregular :: forall sh e. Shape sh => (Segments sh, Vector e) -> [Array sh e]
divideIrregular (segs, (Array _ adata))
  = [Array (fromElt (shs ! (Z:.i))) (copy (offs ! (Z:.i)) (size (shs ! (Z:.i)))) | i <- [0..n-1]]
  where
    (_, offs, shs) = segs
    n              = size (shape shs)
    --
    {-# NOINLINE copy #-}
    copy start n = unsafePerformIO $ do
      dst <- newArrayData n
      unsafeCopyArrayData adata dst start 0 n
      touchArrayData adata
      unsafeFreezeArrayData dst
      return dst

concatRegular :: forall sh e. (Shape sh, Elt e) => sh -> [Array sh e] -> Array (sh:.Int) e
concatRegular sh arrs = copy
  where
    !n   = length arrs
    sh' :: sh:.Int
    !sh' = listToShape (shapeToList sh ++ [n])
    --
    {-# NOINLINE copy #-}
    copy = unsafePerformIO  $ do
      dst <- newArrayData (size sh * n)
      zipWithM_ (copyIntoArray dst) arrs [0,size sh..]
      dst' <- unsafeFreezeArrayData dst
      return (Array (fromElt sh') dst')

concatIrregular :: forall sh e. (Shape sh, Elt e) => [Array sh e] -> (Segments sh, Vector e)
concatIrregular arrs = (segs, copy)
  where
    !n         = length arrs
    !shapes    = map shape arrs
    !totalSize = sum (map size shapes)
    !segs      = ( fromList Z [totalSize]
                 , fromList (Z:.n) (init (scanl (+) 0 (map size shapes)))
                 , fromList (Z:.n) shapes)
    --
    {-# NOINLINE copy #-}
    copy = unsafePerformIO $ do
      dst <- newArrayData totalSize
      foldM_ (\start arr -> copyIntoArray dst arr start >> return (start + (size (shape arr)))) 0 arrs
      dst' <- unsafeFreezeArrayData dst
      return $! (Array ((),totalSize)) dst'

copyIntoArray :: forall sh e. (Shape sh, Elt e) => ArrayData (EltRepr e) -> Array sh e -> Int -> IO ()
copyIntoArray dst (Array sh src) start = unsafeCopyArrayData src dst 0 start (size (toElt sh :: sh))
