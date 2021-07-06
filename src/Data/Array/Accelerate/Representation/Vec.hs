{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Representation.Vec
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Representation.Vec
  where

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Representation.Type
import Data.Primitive.Vec

import Control.Monad.ST
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Language.Haskell.TH.Extra

import GHC.Base                                         ( Int(..), Int#, (-#) )
import GHC.TypeNats


-- | Declares the size of a SIMD vector and the type of its elements. This
-- data type is used to denote the relation between a vector type (Vec
-- n single) with its tuple representation (tuple). Conversions between
-- those types are exposed through 'pack' and 'unpack'.
--
data VecR (n :: Nat) single tuple where
  VecRnil  :: SingleType s -> VecR 0       s ()
  VecRsucc :: VecR n s t   -> VecR (n + 1) s (t, s)

vecRvector :: KnownNat n => VecR n s tuple -> VectorType (Vec n s)
vecRvector = uncurry VectorType . go
  where
    go :: VecR n s tuple -> (Int, SingleType s)
    go (VecRnil tp)                       = (0,     tp)
    go (VecRsucc vec) | (n, tp) <- go vec = (n + 1, tp)

vecRtuple :: VecR n s tuple -> TypeR tuple
vecRtuple = snd . go
  where
    go :: VecR n s tuple -> (SingleType s, TypeR tuple)
    go (VecRnil tp)                           = (tp, TupRunit)
    go (VecRsucc vec) | (tp, tuple) <- go vec = (tp, TupRpair tuple (TupRsingle (SingleScalarType tp)))

pack :: forall n single tuple. KnownNat n => VecR n single tuple -> tuple -> Vec n single
pack vecR tuple
  | VectorType n single <- vecRvector vecR
  , SingleDict          <- singleDict single
  = runST $ do
      mba <- newByteArray (n * sizeOf (undefined :: single))
      go (n - 1) vecR tuple mba
      ByteArray ba# <- unsafeFreezeByteArray mba
      return $! Vec ba#
  where
    go :: Prim single => Int -> VecR n' single tuple' -> tuple' -> MutableByteArray s -> ST s ()
    go _ (VecRnil _)  ()      _   = return ()
    go i (VecRsucc r) (xs, x) mba = do
      writeByteArray mba i x
      go (i - 1) r xs mba

unpack :: forall n single tuple. KnownNat n => VecR n single tuple -> Vec n single -> tuple
unpack vecR (Vec ba#)
  | VectorType n single <- vecRvector vecR
  , (I# n#)             <- n
  , SingleDict          <- singleDict single
  = go (n# -# 1#) vecR
  where
    go :: Prim single => Int# -> VecR n' single tuple' -> tuple'
    go _  (VecRnil _)  = ()
    go i# (VecRsucc r) = x `seq` xs `seq` (xs, x)
      where
        xs = go (i# -# 1#) r
        x  = indexByteArray# ba# i#

rnfVecR :: VecR n single tuple -> ()
rnfVecR (VecRnil tp)   = rnfSingleType tp
rnfVecR (VecRsucc vec) = rnfVecR vec

liftVecR :: VecR n single tuple -> CodeQ (VecR n single tuple)
liftVecR (VecRnil tp)   = [|| VecRnil $$(liftSingleType tp) ||]
liftVecR (VecRsucc vec) = [|| VecRsucc $$(liftVecR vec) ||]

