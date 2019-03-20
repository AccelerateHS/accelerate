{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Analysis.Hash
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Analysis.Hash (

  -- hashing expressions
  Hash,
  HashOptions(..), defaultHashOptions,
  hashPreOpenAcc, hashPreOpenAccWith,
  hashPreOpenFun, hashPreOpenFunWith,
  hashPreOpenExp, hashPreOpenExpWith,

  -- auxiliary
  EncodeAcc,
  encodePreOpenAcc,
  encodePreOpenExp,
  encodePreOpenFun,
  hashQ,

) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Hash.TH
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Array.Representation                   ( SliceIndex(..) )
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Type

import Crypto.Hash
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra
import Data.ByteString.Short.Internal                               ( ShortByteString(..) )
import Data.Monoid
import System.IO.Unsafe                                             ( unsafePerformIO )
import System.Mem.StableName                                        ( hashStableName, makeStableName )
import Prelude                                                      hiding ( exp )


-- Hashing
-- -------

type Hash = Digest SHA3_256

data HashOptions = HashOptions
  { perfect :: Bool
    -- ^ Should the hash function include _all_ substructure, recursively?
    --
    -- Set to true (the default) if you want a truly unique fingerprint for
    -- the entire expression:
    --
    -- Example:
    --
    -- xs, ys :: Acc (Vector Float)
    -- xs = fill (constant (Z:.10)) 1.0
    -- ys = fill (constant (Z:.20)) 1.0
    --
    -- with perfect=True:
    --
    --   hash xs = 2e1f91aca4c476d13b36f22462e73c15bbdd9fcacb0d4996280f6004058e9732
    --   hash ys = 2fce5c849b6c652192b09aaeafdc8029e57b9f006c1ecd79ccf9114f349aaf9e
    --
    -- However, for a code generating backend the object code used to
    -- evaluate both of these expressions is likely to be identical.
    --
    -- Setting perfect=False results in:
    --
    --   hash xs = hash ys = f97944b0ec64ab8aa989fd60c8b50e7ec3eff759d22d2b340039d837d74dfc3c
    --
    -- Note that to be useful the provided 'EncodeAcc' function must also
    -- understand this option, and the consumer of the hash value must be
    -- agnostic to the elided details.
  }
  deriving Show

defaultHashOptions :: HashOptions
defaultHashOptions = HashOptions True


{-# INLINEABLE hashPreOpenAcc #-}
hashPreOpenAcc :: EncodeAcc acc -> PreOpenAcc acc aenv a -> Hash
hashPreOpenAcc = hashPreOpenAccWith defaultHashOptions

{-# INLINEABLE hashPreOpenFun #-}
hashPreOpenFun :: EncodeAcc acc -> PreOpenFun acc env aenv f -> Hash
hashPreOpenFun = hashPreOpenFunWith defaultHashOptions

{-# INLINEABLE hashPreOpenExp #-}
hashPreOpenExp :: EncodeAcc acc -> PreOpenExp acc env aenv t -> Hash
hashPreOpenExp = hashPreOpenExpWith defaultHashOptions

{-# INLINEABLE hashPreOpenAccWith #-}
hashPreOpenAccWith :: HashOptions -> EncodeAcc acc -> PreOpenAcc acc aenv a -> Hash
hashPreOpenAccWith options encodeAcc
  = hashlazy
  . toLazyByteString
  . encodePreOpenAcc options encodeAcc

{-# INLINEABLE hashPreOpenFunWith #-}
hashPreOpenFunWith :: HashOptions -> EncodeAcc acc -> PreOpenFun acc env aenv f -> Hash
hashPreOpenFunWith options encodeAcc
  = hashlazy
  . toLazyByteString
  . encodePreOpenFun options encodeAcc

{-# INLINEABLE hashPreOpenExpWith #-}
hashPreOpenExpWith :: HashOptions -> EncodeAcc acc -> PreOpenExp acc env aenv t -> Hash
hashPreOpenExpWith options encodeAcc
  = hashlazy
  . toLazyByteString
  . encodePreOpenExp options encodeAcc


-- Array computations
-- ------------------

type EncodeAcc acc = forall aenv a. HashOptions -> acc aenv a -> Builder

{-# INLINEABLE encodePreOpenAcc #-}
encodePreOpenAcc
    :: forall acc aenv arrs.
       HashOptions
    -> EncodeAcc acc
    -> PreOpenAcc acc aenv arrs
    -> Builder
encodePreOpenAcc options encodeAcc pacc =
  let
      travA :: forall aenv' a. Arrays a => acc aenv' a -> Builder
      travA a = encodeArraysType (arrays @a) <> encodeAcc options a

      travAF :: PreOpenAfun acc aenv' f -> Builder
      travAF = encodePreOpenAfun options encodeAcc

      travE :: PreOpenExp acc env' aenv' e -> Builder
      travE = encodePreOpenExp options encodeAcc

      travF :: PreOpenFun acc env' aenv' f -> Builder
      travF = encodePreOpenFun options encodeAcc

      travB :: PreBoundary acc aenv' (Array sh e) -> Builder
      travB = encodePreBoundary options encodeAcc

      nacl :: Arrays arrs => Builder
      nacl = encodeArraysType (arrays @arrs)

      deep :: Builder -> Builder
      deep x | perfect options = x
             | otherwise       = mempty
  in
  case pacc of
    Alet bnd body               -> intHost $(hashQ "Alet")        <> travA bnd <> travA body
    Avar v                      -> intHost $(hashQ "Avar")        <> nacl <> deep (encodeIdx v)
    Atuple t                    -> intHost $(hashQ "Atuple")      <> nacl <> encodeAtuple options encodeAcc t
    Aprj ix a                   -> intHost $(hashQ "Aprj")        <> nacl <> encodeTupleIdx ix <> travA a
    Apply f a                   -> intHost $(hashQ "Apply")       <> nacl <> travAF f <> travA a
    Aforeign _ f a              -> intHost $(hashQ "Aforeign")    <> nacl <> travAF f <> travA a
    Use a                       -> intHost $(hashQ "Use")         <> deep (encodeArrays (arrays @arrs) a)
    Awhile p f a                -> intHost $(hashQ "Awhile")      <> travAF f <> travAF p <> travA a
    Unit e                      -> intHost $(hashQ "Unit")        <> travE e
    Generate e f                -> intHost $(hashQ "Generate")    <> deep (travE e)  <> travF f
    Acond e a1 a2               -> intHost $(hashQ "Acond")       <> deep (travE e)  <> travA a1 <> travA a2
    Reshape sh a                -> intHost $(hashQ "Reshape")     <> deep (travE sh) <> travA a
    Backpermute sh f a          -> intHost $(hashQ "Backpermute") <> deep (travE sh) <> travF f  <> travA a
    Transform sh f1 f2 a        -> intHost $(hashQ "Transform")   <> deep (travE sh) <> travF f1 <> travF f2 <> travA a
    Replicate spec ix a         -> intHost $(hashQ "Replicate")   <> deep (travE ix) <> travA a  <> encodeSliceIndex spec
    Slice spec a ix             -> intHost $(hashQ "Slice")       <> deep (travE ix) <> travA a  <> encodeSliceIndex spec
    Map f a                     -> intHost $(hashQ "Map")         <> travF f  <> travA a
    ZipWith f a1 a2             -> intHost $(hashQ "ZipWith")     <> travF f  <> travA a1 <> travA a2
    Fold f e a                  -> intHost $(hashQ "Fold")        <> travF f  <> travE e  <> travA a
    Fold1 f a                   -> intHost $(hashQ "Fold1")       <> travF f  <> travA a
    FoldSeg f e a s             -> intHost $(hashQ "FoldSeg")     <> travF f  <> travE e  <> travA a  <> travA s
    Fold1Seg f a s              -> intHost $(hashQ "Fold1Seg")    <> travF f  <> travA a  <> travA s
    Scanl f e a                 -> intHost $(hashQ "Scanl")       <> travF f  <> travE e  <> travA a
    Scanl' f e a                -> intHost $(hashQ "Scanl'")      <> travF f  <> travE e  <> travA a
    Scanl1 f a                  -> intHost $(hashQ "Scanl1")      <> travF f  <> travA a
    Scanr f e a                 -> intHost $(hashQ "Scanr")       <> travF f  <> travE e  <> travA a
    Scanr' f e a                -> intHost $(hashQ "Scanr'")      <> travF f  <> travE e  <> travA a
    Scanr1 f a                  -> intHost $(hashQ "Scanr1")      <> travF f  <> travA a
    Permute f1 a1 f2 a2         -> intHost $(hashQ "Permute")     <> travF f1 <> travA a1 <> travF f2 <> travA a2
    Stencil f b a               -> intHost $(hashQ "Stencil")     <> travF f  <> travB b  <> travA a
    Stencil2 f b1 a1 b2 a2      -> intHost $(hashQ "Stencil2")    <> travF f  <> travB b1 <> travA a1 <> travB b2 <> travA a2

{--
{-# INLINEABLE encodePreOpenSeq #-}
encodePreOpenSeq :: forall acc aenv senv arrs. EncodeAcc acc -> PreOpenSeq acc aenv senv arrs -> Int
encodePreOpenSeq encodeAcc s =
  let
      travA :: acc aenv' a -> Builder
      travA = encodeAcc -- XXX: plus type information?

      travE :: PreOpenExp acc env' aenv' e -> Builder
      travE = encodePreOpenExp encodeAcc

      travAF :: PreOpenAfun acc aenv' f -> Builder
      travAF = encodePreOpenAfun encodeAcc

      travF :: PreOpenFun acc env' aenv' f -> Builder
      travF = encodePreOpenFun encodeAcc

      travS :: PreOpenSeq acc aenv senv' arrs' -> Builder
      travS = encodePreOpenSeq encodeAcc

      travV :: forall a. Arrays a => Idx senv' a -> Builder
      travV v = encodeArraysType (arrays @a) <> encodeIdx v

      travP :: Producer acc aenv senv a -> Builder
      travP p =
        case p of
          StreamIn arrs       -> intHost . unsafePerformIO $! hashStableName `fmap` makeStableName arrs
          ToSeq spec _ acc    -> intHost $(hashQ "ToSeq")         <> travA  acc <> stringUtf8 (show spec)
          MapSeq f x          -> intHost $(hashQ "MapSeq")        <> travAF f   <> travV x
          ChunkedMapSeq f x   -> intHost $(hashQ "ChunkedMapSeq") <> travAF f   <> travV x
          ZipWithSeq f x y    -> intHost $(hashQ "ZipWithSeq")    <> travAF f   <> travV x <> travV y
          ScanSeq f e x       -> intHost $(hashQ "ScanSeq")       <> travF  f   <> travE e <> travV x

      travC :: Consumer acc aenv senv' a -> Builder
      travC c =
        case c of
          FoldSeq f e x          -> intHost $(hashQ "FoldSeq")        <> travF  f <> travE e   <> travV x
          FoldSeqFlatten f acc x -> intHost $(hashQ "FoldSeqFlatten") <> travAF f <> travA acc <> travV x
          Stuple t               -> intHost $(hashQ "Stuple")         <> encodeAtuple travC t
  in
  case s of
    Producer p s' -> intHost $(hashQ "Producer")   <> travP p <> travS s'
    Consumer c    -> intHost $(hashQ "Consumer")   <> travC c
    Reify ix      -> intHost $(hashQ "Reify")      <> travV ix
--}

encodeIdx :: Idx env t -> Builder
encodeIdx = intHost . idxToInt

encodeTupleIdx :: TupleIdx tup e -> Builder
encodeTupleIdx = intHost . tupleIdxToInt

encodeArrays :: ArraysR a -> a -> Builder
encodeArrays ArraysRunit         ()       = mempty
encodeArrays (ArraysRpair r1 r2) (a1, a2) = encodeArrays r1 a1 <> encodeArrays r2 a2
encodeArrays ArraysRarray        ad       = intHost . unsafePerformIO $! hashStableName `fmap` makeStableName ad

encodeArraysType :: forall a. ArraysR a -> Builder
encodeArraysType ArraysRunit         = intHost $(hashQ "ArraysRunit")
encodeArraysType (ArraysRpair r1 r2) = intHost $(hashQ "ArraysRpair")  <> encodeArraysType r1 <> encodeArraysType r2
encodeArraysType ArraysRarray        = intHost $(hashQ "ArraysRarray") <> encodeArrayType @a
  where
    encodeArrayType :: forall array sh e. (array ~ Array sh e, Shape sh, Elt e) => Builder
    encodeArrayType = encodeTupleType (eltType @sh) <> encodeTupleType (eltType @e)

encodeAtuple :: HashOptions -> EncodeAcc acc -> Atuple (acc aenv) a -> Builder
encodeAtuple _ _     NilAtup        = intHost $(hashQ "NilAtup")
encodeAtuple o travA (SnocAtup t a) = intHost $(hashQ "SnocAtup") <> encodeAtuple o travA t <> travA o a

encodePreOpenAfun
    :: forall acc aenv f.
       HashOptions
    -> EncodeAcc acc
    -> PreOpenAfun acc aenv f
    -> Builder
encodePreOpenAfun options travA afun =
  let
      travB :: forall aenv' a. Arrays a => acc aenv' a -> Builder
      travB b = encodeArraysType (arrays @a) <> travA options b

      travL :: forall aenv' a b. Arrays a => PreOpenAfun acc (aenv',a) b -> Builder
      travL l = encodeArraysType (arrays @a) <> encodePreOpenAfun options travA l
  in
  case afun of
    Abody b -> intHost $(hashQ "Abody") <> travB b
    Alam  l -> intHost $(hashQ "Alam")  <> travL l


encodePreBoundary
    :: forall acc aenv sh e.
       HashOptions
    -> EncodeAcc acc
    -> PreBoundary acc aenv (Array sh e)
    -> Builder
encodePreBoundary _ _ Wrap          = intHost $(hashQ "Wrap")
encodePreBoundary _ _ Clamp         = intHost $(hashQ "Clamp")
encodePreBoundary _ _ Mirror        = intHost $(hashQ "Mirror")
encodePreBoundary _ _ (Constant c)  = intHost $(hashQ "Constant") <> encodeConst (eltType @e) c
encodePreBoundary o h (Function f)  = intHost $(hashQ "Function") <> encodePreOpenFun o h f

encodeSliceIndex :: SliceIndex slix sl co sh -> Builder
encodeSliceIndex SliceNil         = intHost $(hashQ "SliceNil")
encodeSliceIndex (SliceAll r)     = intHost $(hashQ "SliceAll")   <> encodeSliceIndex r
encodeSliceIndex (SliceFixed r)   = intHost $(hashQ "sliceFixed") <> encodeSliceIndex r


-- Scalar expressions
-- ------------------

{-# INLINEABLE encodePreOpenExp #-}
encodePreOpenExp
    :: forall acc env aenv exp.
       HashOptions
    -> EncodeAcc acc
    -> PreOpenExp acc env aenv exp
    -> Builder
encodePreOpenExp options encodeAcc exp =
  let
      travA :: forall aenv' a. Arrays a => acc aenv' a -> Builder
      travA a = encodeArraysType (arrays @a) <> encodeAcc options a

      travE :: forall env' aenv' e. Elt e => PreOpenExp acc env' aenv' e -> Builder
      travE e =  encodeTupleType (eltType @e) <> encodePreOpenExp options encodeAcc e

      travF :: PreOpenFun acc env' aenv' f -> Builder
      travF = encodePreOpenFun options encodeAcc

      nacl :: Elt exp => Builder
      nacl = encodeTupleType (eltType @exp)
  in
  case exp of
    Let bnd body                -> intHost $(hashQ "Let")         <> travE bnd <> travE body
    Var ix                      -> intHost $(hashQ "Var")         <> nacl <> encodeIdx ix
    Tuple t                     -> intHost $(hashQ "Tuple")       <> nacl <> encodeTuple options encodeAcc t
    Prj i e                     -> intHost $(hashQ "Prj")         <> nacl <> encodeTupleIdx i <> travE e -- XXX: here multiplied nacl by hashTupleIdx
    Const c                     -> intHost $(hashQ "Const")       <> encodeConst (eltType @exp) c
    Undef                       -> intHost $(hashQ "Undef")
    IndexAny                    -> intHost $(hashQ "IndexAny")    <> nacl
    IndexNil                    -> intHost $(hashQ "IndexNil")
    IndexCons sh sz             -> intHost $(hashQ "IndexCons")   <> travE sh <> travE sz
    IndexHead sl                -> intHost $(hashQ "IndexHead")   <> travE sl
    IndexTail sl                -> intHost $(hashQ "IndexTail")   <> travE sl
    IndexSlice spec ix sh       -> intHost $(hashQ "IndexSlice")  <> travE ix <> travE sh <> encodeSliceIndex spec
    IndexFull  spec ix sl       -> intHost $(hashQ "IndexFull")   <> travE ix <> travE sl <> encodeSliceIndex spec
    ToIndex sh i                -> intHost $(hashQ "ToIndex")     <> travE sh <> travE i
    FromIndex sh i              -> intHost $(hashQ "FromIndex")   <> travE sh <> travE i
    Cond c t e                  -> intHost $(hashQ "Cond")        <> travE c  <> travE t  <> travE e
    While p f x                 -> intHost $(hashQ "While")       <> travF p  <> travF f  <> travE x
    PrimApp f x                 -> intHost $(hashQ "PrimApp")     <> encodePrimFun f <> travE x
    PrimConst c                 -> intHost $(hashQ "PrimConst")   <> encodePrimConst c
    Index a ix                  -> intHost $(hashQ "Index")       <> travA a  <> travE ix
    LinearIndex a ix            -> intHost $(hashQ "LinearIndex") <> travA a  <> travE ix
    Shape a                     -> intHost $(hashQ "Shape")       <> travA a
    ShapeSize sh                -> intHost $(hashQ "ShapeSize")   <> travE sh
    Intersect sa sb             -> intHost $(hashQ "Intersect")   <> travE sa <> travE sb
    Union sa sb                 -> intHost $(hashQ "Union")       <> travE sa <> travE sb
    Foreign _ f e               -> intHost $(hashQ "Foreign")     <> travF f  <> travE e
    Coerce e                    -> intHost $(hashQ "Coerce")      <> travE e


{-# INLINEABLE encodePreOpenFun #-}
encodePreOpenFun
    :: forall acc env aenv f.
       HashOptions
    -> EncodeAcc acc
    -> PreOpenFun acc env aenv f
    -> Builder
encodePreOpenFun options travA fun =
  let
      travB :: forall env' aenv' e. Elt e => PreOpenExp acc env' aenv' e -> Builder
      travB b = encodeTupleType (eltType @e) <> encodePreOpenExp options travA b

      travL :: forall env' aenv' a b. Elt a => PreOpenFun acc (env',a) aenv' b -> Builder
      travL l = encodeTupleType (eltType @a) <> encodePreOpenFun options travA l
  in
  case fun of
    Body b -> intHost $(hashQ "Body") <> travB b
    Lam l  -> intHost $(hashQ "Lam")  <> travL l

encodeTuple
    :: HashOptions
    -> EncodeAcc acc
    -> Tuple (PreOpenExp acc env aenv) e
    -> Builder
encodeTuple _ _ NilTup        = intHost $(hashQ "NilTup")
encodeTuple o h (SnocTup t e) = intHost $(hashQ "SnocTup") <> encodeTuple o h t <> encodePreOpenExp o h e


encodeConst :: TupleType t -> t -> Builder
encodeConst TypeRunit         ()    = mempty
encodeConst (TypeRscalar t)   c     = encodeScalarConst t c
encodeConst (TypeRpair ta tb) (a,b) = encodeConst ta a <> encodeConst tb b

encodeScalarConst :: ScalarType t -> t -> Builder
encodeScalarConst (SingleScalarType t) = encodeSingleConst t
encodeScalarConst (VectorScalarType t) = encodeVectorConst t

encodeSingleConst :: SingleType t -> t -> Builder
encodeSingleConst (NumSingleType t)    = encodeNumConst t
encodeSingleConst (NonNumSingleType t) = encodeNonNumConst t

encodeVectorConst :: VectorType (Vec n t) -> Vec n t -> Builder
encodeVectorConst (VectorType n t) (Vec ba#) = intHost $(hashQ "Vec") <> intHost n <> encodeSingleType t <> shortByteString (SBS ba#)

encodeNonNumConst :: NonNumType t -> t -> Builder
encodeNonNumConst TypeBool{} x = intHost $(hashQ "Bool")   <> word8 (fromBool x)
encodeNonNumConst TypeChar{} x = intHost $(hashQ "Char")   <> charUtf8 x

fromBool :: Bool -> Word8
fromBool True  = 1
fromBool False = 0

encodeNumConst :: NumType t -> t -> Builder
encodeNumConst (IntegralNumType t) = encodeIntegralConst t
encodeNumConst (FloatingNumType t) = encodeFloatingConst t

encodeIntegralConst :: IntegralType t -> t -> Builder
encodeIntegralConst TypeInt{}    x = intHost $(hashQ "Int")    <> intHost x
encodeIntegralConst TypeInt8{}   x = intHost $(hashQ "Int8")   <> int8 x
encodeIntegralConst TypeInt16{}  x = intHost $(hashQ "Int16")  <> int16Host x
encodeIntegralConst TypeInt32{}  x = intHost $(hashQ "Int32")  <> int32Host x
encodeIntegralConst TypeInt64{}  x = intHost $(hashQ "Int64")  <> int64Host x
encodeIntegralConst TypeWord{}   x = intHost $(hashQ "Word")   <> wordHost x
encodeIntegralConst TypeWord8{}  x = intHost $(hashQ "Word8")  <> word8 x
encodeIntegralConst TypeWord16{} x = intHost $(hashQ "Word16") <> word16Host x
encodeIntegralConst TypeWord32{} x = intHost $(hashQ "Word32") <> word32Host x
encodeIntegralConst TypeWord64{} x = intHost $(hashQ "Word64") <> word64Host x

encodeFloatingConst :: FloatingType t -> t -> Builder
encodeFloatingConst TypeHalf{}    (Half (CUShort x)) = intHost $(hashQ "Half")    <> word16Host x
encodeFloatingConst TypeFloat{}   x                  = intHost $(hashQ "Float")   <> floatHost x
encodeFloatingConst TypeDouble{}  x                  = intHost $(hashQ "Double")  <> doubleHost x

encodePrimConst :: PrimConst c -> Builder
encodePrimConst (PrimMinBound t)  = intHost $(hashQ "PrimMinBound") <> encodeBoundedType t
encodePrimConst (PrimMaxBound t)  = intHost $(hashQ "PrimMaxBound") <> encodeBoundedType t
encodePrimConst (PrimPi t)        = intHost $(hashQ "PrimPi")       <> encodeFloatingType t

encodePrimFun :: PrimFun f -> Builder
encodePrimFun (PrimAdd a)                = intHost $(hashQ "PrimAdd")                <> encodeNumType a
encodePrimFun (PrimSub a)                = intHost $(hashQ "PrimSub")                <> encodeNumType a
encodePrimFun (PrimMul a)                = intHost $(hashQ "PrimMul")                <> encodeNumType a
encodePrimFun (PrimNeg a)                = intHost $(hashQ "PrimNeg")                <> encodeNumType a
encodePrimFun (PrimAbs a)                = intHost $(hashQ "PrimAbs")                <> encodeNumType a
encodePrimFun (PrimSig a)                = intHost $(hashQ "PrimSig")                <> encodeNumType a
encodePrimFun (PrimQuot a)               = intHost $(hashQ "PrimQuot")               <> encodeIntegralType a
encodePrimFun (PrimRem a)                = intHost $(hashQ "PrimRem")                <> encodeIntegralType a
encodePrimFun (PrimQuotRem a)            = intHost $(hashQ "PrimQuotRem")            <> encodeIntegralType a
encodePrimFun (PrimIDiv a)               = intHost $(hashQ "PrimIDiv")               <> encodeIntegralType a
encodePrimFun (PrimMod a)                = intHost $(hashQ "PrimMod")                <> encodeIntegralType a
encodePrimFun (PrimDivMod a)             = intHost $(hashQ "PrimDivMod")             <> encodeIntegralType a
encodePrimFun (PrimBAnd a)               = intHost $(hashQ "PrimBAnd")               <> encodeIntegralType a
encodePrimFun (PrimBOr a)                = intHost $(hashQ "PrimBOr")                <> encodeIntegralType a
encodePrimFun (PrimBXor a)               = intHost $(hashQ "PrimBXor")               <> encodeIntegralType a
encodePrimFun (PrimBNot a)               = intHost $(hashQ "PrimBNot")               <> encodeIntegralType a
encodePrimFun (PrimBShiftL a)            = intHost $(hashQ "PrimBShiftL")            <> encodeIntegralType a
encodePrimFun (PrimBShiftR a)            = intHost $(hashQ "PrimBShiftR")            <> encodeIntegralType a
encodePrimFun (PrimBRotateL a)           = intHost $(hashQ "PrimBRotateL")           <> encodeIntegralType a
encodePrimFun (PrimBRotateR a)           = intHost $(hashQ "PrimBRotateR")           <> encodeIntegralType a
encodePrimFun (PrimPopCount a)           = intHost $(hashQ "PrimPopCount")           <> encodeIntegralType a
encodePrimFun (PrimCountLeadingZeros a)  = intHost $(hashQ "PrimCountLeadingZeros")  <> encodeIntegralType a
encodePrimFun (PrimCountTrailingZeros a) = intHost $(hashQ "PrimCountTrailingZeros") <> encodeIntegralType a
encodePrimFun (PrimFDiv a)               = intHost $(hashQ "PrimFDiv")               <> encodeFloatingType a
encodePrimFun (PrimRecip a)              = intHost $(hashQ "PrimRecip")              <> encodeFloatingType a
encodePrimFun (PrimSin a)                = intHost $(hashQ "PrimSin")                <> encodeFloatingType a
encodePrimFun (PrimCos a)                = intHost $(hashQ "PrimCos")                <> encodeFloatingType a
encodePrimFun (PrimTan a)                = intHost $(hashQ "PrimTan")                <> encodeFloatingType a
encodePrimFun (PrimAsin a)               = intHost $(hashQ "PrimAsin")               <> encodeFloatingType a
encodePrimFun (PrimAcos a)               = intHost $(hashQ "PrimAcos")               <> encodeFloatingType a
encodePrimFun (PrimAtan a)               = intHost $(hashQ "PrimAtan")               <> encodeFloatingType a
encodePrimFun (PrimSinh a)               = intHost $(hashQ "PrimSinh")               <> encodeFloatingType a
encodePrimFun (PrimCosh a)               = intHost $(hashQ "PrimCosh")               <> encodeFloatingType a
encodePrimFun (PrimTanh a)               = intHost $(hashQ "PrimTanh")               <> encodeFloatingType a
encodePrimFun (PrimAsinh a)              = intHost $(hashQ "PrimAsinh")              <> encodeFloatingType a
encodePrimFun (PrimAcosh a)              = intHost $(hashQ "PrimAcosh")              <> encodeFloatingType a
encodePrimFun (PrimAtanh a)              = intHost $(hashQ "PrimAtanh")              <> encodeFloatingType a
encodePrimFun (PrimExpFloating a)        = intHost $(hashQ "PrimExpFloating")        <> encodeFloatingType a
encodePrimFun (PrimSqrt a)               = intHost $(hashQ "PrimSqrt")               <> encodeFloatingType a
encodePrimFun (PrimLog a)                = intHost $(hashQ "PrimLog")                <> encodeFloatingType a
encodePrimFun (PrimFPow a)               = intHost $(hashQ "PrimFPow")               <> encodeFloatingType a
encodePrimFun (PrimLogBase a)            = intHost $(hashQ "PrimLogBase")            <> encodeFloatingType a
encodePrimFun (PrimAtan2 a)              = intHost $(hashQ "PrimAtan2")              <> encodeFloatingType a
encodePrimFun (PrimTruncate a b)         = intHost $(hashQ "PrimTruncate")           <> encodeFloatingType a <> encodeIntegralType b
encodePrimFun (PrimRound a b)            = intHost $(hashQ "PrimRound")              <> encodeFloatingType a <> encodeIntegralType b
encodePrimFun (PrimFloor a b)            = intHost $(hashQ "PrimFloor")              <> encodeFloatingType a <> encodeIntegralType b
encodePrimFun (PrimCeiling a b)          = intHost $(hashQ "PrimCeiling")            <> encodeFloatingType a <> encodeIntegralType b
encodePrimFun (PrimIsNaN a)              = intHost $(hashQ "PrimIsNaN")              <> encodeFloatingType a
encodePrimFun (PrimIsInfinite a)         = intHost $(hashQ "PrimIsInfinite")         <> encodeFloatingType a
encodePrimFun (PrimLt a)                 = intHost $(hashQ "PrimLt")                 <> encodeSingleType a
encodePrimFun (PrimGt a)                 = intHost $(hashQ "PrimGt")                 <> encodeSingleType a
encodePrimFun (PrimLtEq a)               = intHost $(hashQ "PrimLtEq")               <> encodeSingleType a
encodePrimFun (PrimGtEq a)               = intHost $(hashQ "PrimGtEq")               <> encodeSingleType a
encodePrimFun (PrimEq a)                 = intHost $(hashQ "PrimEq")                 <> encodeSingleType a
encodePrimFun (PrimNEq a)                = intHost $(hashQ "PrimNEq")                <> encodeSingleType a
encodePrimFun (PrimMax a)                = intHost $(hashQ "PrimMax")                <> encodeSingleType a
encodePrimFun (PrimMin a)                = intHost $(hashQ "PrimMin")                <> encodeSingleType a
encodePrimFun (PrimFromIntegral a b)     = intHost $(hashQ "PrimFromIntegral")       <> encodeIntegralType a <> encodeNumType b
encodePrimFun (PrimToFloating a b)       = intHost $(hashQ "PrimToFloating")         <> encodeNumType a      <> encodeFloatingType b
encodePrimFun PrimLAnd                   = intHost $(hashQ "PrimLAnd")
encodePrimFun PrimLOr                    = intHost $(hashQ "PrimLOr")
encodePrimFun PrimLNot                   = intHost $(hashQ "PrimLNot")
encodePrimFun PrimOrd                    = intHost $(hashQ "PrimOrd")
encodePrimFun PrimChr                    = intHost $(hashQ "PrimChr")
encodePrimFun PrimBoolToInt              = intHost $(hashQ "PrimBoolToInt")


encodeTupleType :: TupleType t -> Builder
encodeTupleType TypeRunit       = intHost $(hashQ "TypeRunit")
encodeTupleType (TypeRscalar t) = intHost $(hashQ "TypeRscalar") <> encodeScalarType t
encodeTupleType (TypeRpair a b) = intHost $(hashQ "TypeRpair")   <> encodeTupleType a <> intHost (depthTypeR a)
                                                                 <> encodeTupleType b <> intHost (depthTypeR b)

depthTypeR :: TupleType t -> Int
depthTypeR TypeRunit       = 0
depthTypeR TypeRscalar{}   = 1
depthTypeR (TypeRpair a b) = depthTypeR a + depthTypeR b

encodeScalarType :: ScalarType t -> Builder
encodeScalarType (SingleScalarType t) = intHost $(hashQ "SingleScalarType") <> encodeSingleType t
encodeScalarType (VectorScalarType t) = intHost $(hashQ "VectorScalarType") <> encodeVectorType t

encodeSingleType :: SingleType t -> Builder
encodeSingleType (NumSingleType t)    = intHost $(hashQ "NumSingleType")    <> encodeNumType t
encodeSingleType (NonNumSingleType t) = intHost $(hashQ "NonNumSingleType") <> encodeNonNumType t

encodeVectorType :: VectorType (Vec n t) -> Builder
encodeVectorType (VectorType n t) = intHost $(hashQ "VectorType") <> intHost n <> encodeSingleType t

encodeBoundedType :: BoundedType t -> Builder
encodeBoundedType (IntegralBoundedType t) = intHost $(hashQ "IntegralBoundedType") <> encodeIntegralType t
encodeBoundedType (NonNumBoundedType t)   = intHost $(hashQ "NonNumBoundedType")   <> encodeNonNumType t

encodeNonNumType :: NonNumType t -> Builder
encodeNonNumType TypeBool{} = intHost $(hashQ "Bool")
encodeNonNumType TypeChar{} = intHost $(hashQ "Char")

encodeNumType :: NumType t -> Builder
encodeNumType (IntegralNumType t) = intHost $(hashQ "IntegralNumType") <> encodeIntegralType t
encodeNumType (FloatingNumType t) = intHost $(hashQ "FloatingNumType") <> encodeFloatingType t

encodeIntegralType :: IntegralType t -> Builder
encodeIntegralType TypeInt{}    = intHost $(hashQ "Int")
encodeIntegralType TypeInt8{}   = intHost $(hashQ "Int8")
encodeIntegralType TypeInt16{}  = intHost $(hashQ "Int16")
encodeIntegralType TypeInt32{}  = intHost $(hashQ "Int32")
encodeIntegralType TypeInt64{}  = intHost $(hashQ "Int64")
encodeIntegralType TypeWord{}   = intHost $(hashQ "Word")
encodeIntegralType TypeWord8{}  = intHost $(hashQ "Word8")
encodeIntegralType TypeWord16{} = intHost $(hashQ "Word16")
encodeIntegralType TypeWord32{} = intHost $(hashQ "Word32")
encodeIntegralType TypeWord64{} = intHost $(hashQ "Word64")

encodeFloatingType :: FloatingType t -> Builder
encodeFloatingType TypeHalf{}   = intHost $(hashQ "Half")
encodeFloatingType TypeFloat{}  = intHost $(hashQ "Float")
encodeFloatingType TypeDouble{} = intHost $(hashQ "Double")

