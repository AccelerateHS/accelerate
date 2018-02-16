{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Analysis.Hash
-- Copyright   : [2017] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Analysis.Hash (

  -- hashing expressions
  Hash,
  hashPreOpenAcc,
  hashPreOpenFun,
  hashPreOpenExp,

  -- auxiliary
  EncodeAcc,
  encodePreOpenAcc, encodeOpenAcc,
  encodePreOpenExp, encodeOpenExp,
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
import Data.Bits
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra
import Data.Monoid
import Foreign.C.Types
import System.IO.Unsafe                                             ( unsafePerformIO )
import System.Mem.StableName                                        ( hashStableName, makeStableName )
import Prelude                                                      hiding ( exp )


-- Hashing
-- -------

type Hash = Digest SHA3_256

hashPreOpenAcc :: EncodeAcc acc -> PreOpenAcc acc aenv a -> Hash
hashPreOpenAcc encodeAcc = hashlazy . toLazyByteString . encodePreOpenAcc encodeAcc

hashPreOpenFun :: EncodeAcc acc -> PreOpenFun acc env aenv f -> Hash
hashPreOpenFun encodeAcc = hashlazy . toLazyByteString . encodePreOpenFun encodeAcc

hashPreOpenExp :: EncodeAcc acc -> PreOpenExp acc env aenv t -> Hash
hashPreOpenExp encodeAcc = hashlazy . toLazyByteString . encodePreOpenExp encodeAcc


-- Array computations
-- ------------------

type EncodeAcc acc = forall aenv a. acc aenv a -> Builder

{-# INLINE encodeOpenAcc #-}
encodeOpenAcc :: OpenAcc aenv arrs -> Builder
encodeOpenAcc (OpenAcc pacc) = encodePreOpenAcc encodeOpenAcc pacc

{-# INLINE encodePreOpenAcc #-}
encodePreOpenAcc
    :: forall acc aenv arrs.
       EncodeAcc acc
    -> PreOpenAcc acc aenv arrs
    -> Builder
encodePreOpenAcc encodeAcc pacc =
  let
      {-# INLINE travA #-}
      travA :: forall aenv' a. Arrays a => acc aenv' a -> Builder
      travA a = encodeArraysType (arrays (undefined::a)) <> encodeAcc a

      {-# INLINE travE #-}
      travE :: PreOpenExp acc env' aenv' e -> Builder
      travE = encodePreOpenExp encodeAcc

      {-# INLINE travF #-}
      travF :: PreOpenFun acc env' aenv' f -> Builder
      travF = encodePreOpenFun encodeAcc

      {-# INLINE travB #-}
      travB :: PreBoundary acc aenv' (Array sh e) -> Builder
      travB = encodePreBoundary encodeAcc

      {-# INLINE nacl #-}
      nacl :: Arrays arrs => Builder
      nacl = encodeArraysType (arrays (undefined::arrs))
  in
  case pacc of
    Alet bnd body               -> intHost $(hashQ "Alet")        <> travA bnd <> travA body
    Avar v                      -> intHost $(hashQ "Avar")        <> nacl <> encodeIdx v
    Atuple t                    -> intHost $(hashQ "Atuple")      <> nacl <> encodeAtuple encodeAcc t
    Aprj ix a                   -> intHost $(hashQ "Aprj")        <> nacl <> encodeTupleIdx ix <> travA a
    Apply f a                   -> intHost $(hashQ "Apply")       <> nacl <> encodePreOpenAfun encodeAcc f <> travA a
    Aforeign _ f a              -> intHost $(hashQ "Aforeign")    <> nacl <> encodePreOpenAfun encodeAcc f <> travA a
    Use a                       -> intHost $(hashQ "Use")         <> encodeArrays (arrays (undefined::arrs)) a
    Awhile p f a                -> intHost $(hashQ "Awhile")      <> encodePreOpenAfun encodeAcc f <> encodePreOpenAfun encodeAcc p <> travA a
    Unit e                      -> intHost $(hashQ "Unit")        <> travE e
    Generate e f                -> intHost $(hashQ "Generate")    <> travE e  <> travF f
    Acond e a1 a2               -> intHost $(hashQ "Acond")       <> travE e  <> travA a1 <> travA a2
    Reshape sh a                -> intHost $(hashQ "Reshape")     <> travE sh <> travA a
    Transform sh f1 f2 a        -> intHost $(hashQ "Transform")   <> travE sh <> travF f1 <> travF f2 <> travA a
    Replicate spec ix a         -> intHost $(hashQ "Replicate")   <> travE ix <> travA a  <> encodeSliceIndex spec
    Slice spec a ix             -> intHost $(hashQ "Slice")       <> travE ix <> travA a  <> encodeSliceIndex spec
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
    Backpermute sh f a          -> intHost $(hashQ "Backpermute") <> travF f  <> travE sh <> travA a
    Permute f1 a1 f2 a2         -> intHost $(hashQ "Permute")     <> travF f1 <> travA a1 <> travF f2 <> travA a2
    Stencil f b a               -> intHost $(hashQ "Stencil")     <> travF f  <> travB b  <> travA a
    Stencil2 f b1 a1 b2 a2      -> intHost $(hashQ "Stencil2")    <> travF f  <> travB b1 <> travA a1 <> travB b2 <> travA a2

{--
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
      travV v = encodeArraysType (arrays (undefined::a)) <> encodeIdx v

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

{-# INLINE encodeIdx #-}
encodeIdx :: Idx env t -> Builder
encodeIdx = intHost . idxToInt

{-# INLINE encodeTupleIdx #-}
encodeTupleIdx :: TupleIdx tup e -> Builder
encodeTupleIdx = intHost . tupleIdxToInt

{-# INLINE encodeArrays #-}
encodeArrays :: ArraysR a -> a -> Builder
encodeArrays ArraysRunit         ()       = mempty
encodeArrays (ArraysRpair r1 r2) (a1, a2) = encodeArrays r1 a1 <> encodeArrays r2 a2
encodeArrays ArraysRarray        ad       = intHost . unsafePerformIO $! hashStableName `fmap` makeStableName ad

{-# INLINE encodeArraysType #-}
encodeArraysType :: forall a. ArraysR a -> Builder
encodeArraysType ArraysRunit         = intHost $(hashQ "ArraysRunit")
encodeArraysType (ArraysRpair r1 r2) = intHost $(hashQ "ArraysRpair")  <> encodeArraysType r1 <> encodeArraysType r2
encodeArraysType ArraysRarray        = intHost $(hashQ "ArraysRarray") <> encodeArrayType (undefined::a)
  where
    {-# INLINE encodeArrayType #-}
    encodeArrayType :: forall sh e. (Shape sh, Elt e) => Array sh e -> Builder
    encodeArrayType _ = encodeTupleType (eltType (undefined::sh)) <> encodeTupleType (eltType (undefined::e))

{-# INLINE encodeAtuple #-}
encodeAtuple :: EncodeAcc acc -> Atuple (acc aenv) a -> Builder
encodeAtuple _     NilAtup        = intHost $(hashQ "NilAtup")
encodeAtuple travA (SnocAtup t a) = intHost $(hashQ "SnocAtup") <> encodeAtuple travA t <> travA a

{-# INLINE encodePreOpenAfun #-}
encodePreOpenAfun :: forall acc aenv f. EncodeAcc acc -> PreOpenAfun acc aenv f -> Builder
encodePreOpenAfun travA afun =
  let
      {-# INLINE travB #-}
      travB :: forall aenv' a. Arrays a => acc aenv' a -> Builder
      travB b = encodeArraysType (arrays (undefined::a)) <> travA b

      {-# INLINE travL #-}
      travL :: forall aenv' a b. Arrays a => PreOpenAfun acc (aenv',a) b -> Builder
      travL l = encodeArraysType (arrays (undefined::a)) <> encodePreOpenAfun travA l
  in
  case afun of
    Abody b -> intHost $(hashQ "Abody") <> travB b
    Alam  l -> intHost $(hashQ "Alam")  <> travL l


{-# INLINE encodePreBoundary #-}
encodePreBoundary :: forall acc aenv sh e. EncodeAcc acc -> PreBoundary acc aenv (Array sh e) -> Builder
encodePreBoundary _ Wrap          = intHost $(hashQ "Wrap")
encodePreBoundary _ Clamp         = intHost $(hashQ "Clamp")
encodePreBoundary _ Mirror        = intHost $(hashQ "Mirror")
encodePreBoundary _ (Constant c)  = intHost $(hashQ "Constant") <> encodeConst (eltType (undefined::e)) c
encodePreBoundary h (Function f)  = intHost $(hashQ "Function") <> encodePreOpenFun h f

{-# INLINE encodeSliceIndex #-}
encodeSliceIndex :: SliceIndex slix sl co sh -> Builder
encodeSliceIndex SliceNil         = intHost $(hashQ "SliceNil")
encodeSliceIndex (SliceAll r)     = intHost $(hashQ "SliceAll")   <> encodeSliceIndex r
encodeSliceIndex (SliceFixed r)   = intHost $(hashQ "sliceFixed") <> encodeSliceIndex r


-- Scalar expressions
-- ------------------

{-# INLINE encodeOpenExp #-}
encodeOpenExp :: OpenExp env aenv exp -> Builder
encodeOpenExp = encodePreOpenExp encodeOpenAcc

{-# INLINE encodePreOpenExp #-}
encodePreOpenExp :: forall acc env aenv exp. EncodeAcc acc -> PreOpenExp acc env aenv exp -> Builder
encodePreOpenExp travA exp =
  let
      {-# INLINE travE #-}
      travE :: forall env' aenv' e. Elt e => PreOpenExp acc env' aenv' e -> Builder
      travE e =  encodeTupleType (eltType (undefined::e)) <> encodePreOpenExp travA e

      {-# INLINE travF #-}
      travF :: PreOpenFun acc env' aenv' f -> Builder
      travF = encodePreOpenFun travA

      {-# INLINE nacl #-}
      nacl :: Elt exp => Builder
      nacl = encodeTupleType (eltType (undefined::exp))
  in
  case exp of
    Let bnd body                -> intHost $(hashQ "Let")         <> travE bnd <> travE body
    Var ix                      -> intHost $(hashQ "Var")         <> nacl <> encodeIdx ix
    Tuple t                     -> intHost $(hashQ "Tuple")       <> nacl <> encodeTuple travA t
    Prj i e                     -> intHost $(hashQ "Prj")         <> nacl <> encodeTupleIdx i <> travE e -- XXX: here multiplied nacl by hashTupleIdx
    Const c                     -> intHost $(hashQ "Const")       <> encodeConst (eltType (undefined::exp)) c
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


{-# INLINE encodePreOpenFun #-}
encodePreOpenFun :: forall acc env aenv f. EncodeAcc acc -> PreOpenFun acc env aenv f -> Builder
encodePreOpenFun travA fun =
  let
      travB :: forall env' aenv' e. Elt e => PreOpenExp acc env' aenv' e -> Builder
      travB b = encodeTupleType (eltType (undefined::e)) <> encodePreOpenExp travA b

      travL :: forall env' aenv' a b. Elt a => PreOpenFun acc (env',a) aenv' b -> Builder
      travL l = encodeTupleType (eltType (undefined::a)) <> encodePreOpenFun travA l
  in
  case fun of
    Body b -> intHost $(hashQ "Body") <> travB b
    Lam l  -> intHost $(hashQ "Lam")  <> travL l

{-# INLINE encodeTuple #-}
encodeTuple :: EncodeAcc acc -> Tuple (PreOpenExp acc env aenv) e -> Builder
encodeTuple _ NilTup        = intHost $(hashQ "NilTup")
encodeTuple h (SnocTup t e) = intHost $(hashQ "SnocTup") <> encodeTuple h t <> encodePreOpenExp h e


{-# INLINE encodeConst #-}
encodeConst :: TupleType t -> t -> Builder
encodeConst TypeRunit         ()    = mempty
encodeConst (TypeRscalar t)   c     = encodeScalarConst t c
encodeConst (TypeRpair ta tb) (a,b) = encodeConst ta a <> encodeConst tb b

{-# INLINE encodeScalarConst #-}
encodeScalarConst :: ScalarType t -> t -> Builder
encodeScalarConst (SingleScalarType t) = encodeSingleConst t
encodeScalarConst (VectorScalarType t) = encodeVectorConst t

{-# INLINE encodeSingleConst #-}
encodeSingleConst :: SingleType t -> t -> Builder
encodeSingleConst (NumSingleType t)    = encodeNumConst t
encodeSingleConst (NonNumSingleType t) = encodeNonNumConst t

{-# INLINE encodeVectorConst #-}
encodeVectorConst :: VectorType t -> t -> Builder
encodeVectorConst (Vector2Type t) (V2 a b)     = intHost $(hashQ "V2") <> encodeSingleConst t a <> encodeSingleConst t b
encodeVectorConst (Vector3Type t) (V3 a b c)   = intHost $(hashQ "V3") <> encodeSingleConst t a <> encodeSingleConst t b <> encodeSingleConst t c
encodeVectorConst (Vector4Type t) (V4 a b c d) = intHost $(hashQ "V4") <> encodeSingleConst t a <> encodeSingleConst t b <> encodeSingleConst t c <> encodeSingleConst t d
encodeVectorConst (Vector8Type t) (V8 a b c d e f g h) =
  intHost $(hashQ "V8") <> encodeSingleConst t a <> encodeSingleConst t b <> encodeSingleConst t c <> encodeSingleConst t d
                        <> encodeSingleConst t e <> encodeSingleConst t f <> encodeSingleConst t g <> encodeSingleConst t h
encodeVectorConst (Vector16Type t) (V16 a b c d e f g h i j k l m n o p) =
  intHost $(hashQ "V16") <> encodeSingleConst t a <> encodeSingleConst t b <> encodeSingleConst t c <> encodeSingleConst t d
                         <> encodeSingleConst t e <> encodeSingleConst t f <> encodeSingleConst t g <> encodeSingleConst t h
                         <> encodeSingleConst t i <> encodeSingleConst t j <> encodeSingleConst t k <> encodeSingleConst t l
                         <> encodeSingleConst t m <> encodeSingleConst t n <> encodeSingleConst t o <> encodeSingleConst t p

{-# INLINE encodeNonNumConst #-}
encodeNonNumConst :: NonNumType t -> t -> Builder
encodeNonNumConst TypeBool{}   x          = intHost $(hashQ "Bool")   <> word8 (fromBool x)
encodeNonNumConst TypeChar{}   x          = intHost $(hashQ "Char")   <> charUtf8 x
encodeNonNumConst TypeCSChar{} (CSChar x) = intHost $(hashQ "CSChar") <> int8 x
encodeNonNumConst TypeCUChar{} (CUChar x) = intHost $(hashQ "CUChar") <> word8 x
encodeNonNumConst TypeCChar{}  (CChar  x) = intHost $(hashQ "CChar")  <> $( case isSigned (undefined::CChar) of
                                                                              True  -> [e| int8  |]
                                                                              False -> [e| word8 |] ) x

{-# INLINE fromBool #-}
fromBool :: Bool -> Word8
fromBool True  = 1
fromBool False = 0

{-# INLINE encodeNumConst #-}
encodeNumConst :: NumType t -> t -> Builder
encodeNumConst (IntegralNumType t) = encodeIntegralConst t
encodeNumConst (FloatingNumType t) = encodeFloatingConst t

{-# INLINE encodeIntegralConst #-}
encodeIntegralConst :: IntegralType t -> t -> Builder
encodeIntegralConst TypeInt{}     x           = intHost $(hashQ "Int")     <> intHost x
encodeIntegralConst TypeInt8{}    x           = intHost $(hashQ "Int8")    <> int8 x
encodeIntegralConst TypeInt16{}   x           = intHost $(hashQ "Int16")   <> int16Host x
encodeIntegralConst TypeInt32{}   x           = intHost $(hashQ "Int32")   <> int32Host x
encodeIntegralConst TypeInt64{}   x           = intHost $(hashQ "Int64")   <> int64Host x
encodeIntegralConst TypeWord{}    x           = intHost $(hashQ "Word")    <> wordHost x
encodeIntegralConst TypeWord8{}   x           = intHost $(hashQ "Word8")   <> word8 x
encodeIntegralConst TypeWord16{}  x           = intHost $(hashQ "Word16")  <> word16Host x
encodeIntegralConst TypeWord32{}  x           = intHost $(hashQ "Word32")  <> word32Host x
encodeIntegralConst TypeWord64{}  x           = intHost $(hashQ "Word64")  <> word64Host x
encodeIntegralConst TypeCShort{}  (CShort x)  = intHost $(hashQ "CShort")  <> int16Host x
encodeIntegralConst TypeCUShort{} (CUShort x) = intHost $(hashQ "CUShort") <> word16Host x
encodeIntegralConst TypeCInt{}    (CInt x)    = intHost $(hashQ "CInt")    <> int32Host x
encodeIntegralConst TypeCUInt{}   (CUInt x)   = intHost $(hashQ "CUInt")   <> word32Host x
encodeIntegralConst TypeCLLong{}  (CLLong x)  = intHost $(hashQ "CLLong")  <> int64Host x
encodeIntegralConst TypeCULLong{} (CULLong x) = intHost $(hashQ "CULLong") <> word64Host x
encodeIntegralConst TypeCLong{}   (CLong x)   = intHost $(hashQ "CLong")   <> $( case finiteBitSize (undefined::CLong) of
                                                                                   32 -> [e| int32Host |]
                                                                                   64 -> [e| int64Host |]
                                                                                   _  -> error "I don't know what architecture I am" ) x
encodeIntegralConst TypeCULong{}  (CULong x)  = intHost $(hashQ "CULong")  <> $( case finiteBitSize (undefined::CULong) of
                                                                                   32 -> [e| word32Host |]
                                                                                   64 -> [e| word64Host |]
                                                                                   _  -> error "I don't know what architecture I am" ) x

{-# INLINE encodeFloatingConst #-}
encodeFloatingConst :: FloatingType t -> t -> Builder
encodeFloatingConst TypeHalf{}    (Half (CUShort x)) = intHost $(hashQ "Half")    <> word16Host x
encodeFloatingConst TypeFloat{}   x                  = intHost $(hashQ "Float")   <> floatHost x
encodeFloatingConst TypeDouble{}  x                  = intHost $(hashQ "Double")  <> doubleHost x
encodeFloatingConst TypeCFloat{}  (CFloat x)         = intHost $(hashQ "CFloat")  <> floatHost x
encodeFloatingConst TypeCDouble{} (CDouble x)        = intHost $(hashQ "CDouble") <> doubleHost x

{-# INLINE encodePrimConst #-}
encodePrimConst :: PrimConst c -> Builder
encodePrimConst (PrimMinBound t)  = intHost $(hashQ "PrimMinBound") <> encodeBoundedType t
encodePrimConst (PrimMaxBound t)  = intHost $(hashQ "PrimMaxBound") <> encodeBoundedType t
encodePrimConst (PrimPi t)        = intHost $(hashQ "PrimPi")       <> encodeFloatingType t

{-# INLINE encodePrimFun #-}
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
encodePrimFun (PrimCoerce a b)           = intHost $(hashQ "PrimCoerce")             <> encodeScalarType a   <> encodeScalarType b
encodePrimFun PrimLAnd                   = intHost $(hashQ "PrimLAnd")
encodePrimFun PrimLOr                    = intHost $(hashQ "PrimLOr")
encodePrimFun PrimLNot                   = intHost $(hashQ "PrimLNot")
encodePrimFun PrimOrd                    = intHost $(hashQ "PrimOrd")
encodePrimFun PrimChr                    = intHost $(hashQ "PrimChr")
encodePrimFun PrimBoolToInt              = intHost $(hashQ "PrimBoolToInt")


{-# INLINE encodeTupleType #-}
encodeTupleType :: TupleType t -> Builder
encodeTupleType TypeRunit       = intHost $(hashQ "TypeRunit")
encodeTupleType (TypeRscalar t) = intHost $(hashQ "TypeRscalar") <> encodeScalarType t
encodeTupleType (TypeRpair a b) = intHost $(hashQ "TypeRpair")   <> encodeTupleType a <> intHost (depthTypeR a)
                                                                 <> encodeTupleType b <> intHost (depthTypeR b)

{-# INLINE depthTypeR #-}
depthTypeR :: TupleType t -> Int
depthTypeR TypeRunit       = 0
depthTypeR TypeRscalar{}   = 1
depthTypeR (TypeRpair a b) = depthTypeR a + depthTypeR b

{-# INLINE encodeScalarType #-}
encodeScalarType :: ScalarType t -> Builder
encodeScalarType (SingleScalarType t) = intHost $(hashQ "SingleScalarType") <> encodeSingleType t
encodeScalarType (VectorScalarType t) = intHost $(hashQ "VectorScalarType") <> encodeVectorType t

{-# INLINE encodeSingleType #-}
encodeSingleType :: SingleType t -> Builder
encodeSingleType (NumSingleType t)    = intHost $(hashQ "NumSingleType")    <> encodeNumType t
encodeSingleType (NonNumSingleType t) = intHost $(hashQ "NonNumSingleType") <> encodeNonNumType t

{-# INLINE encodeVectorType #-}
encodeVectorType :: VectorType t -> Builder
encodeVectorType (Vector2Type t)  = intHost $(hashQ "Vector2Type") <> encodeSingleType t
encodeVectorType (Vector3Type t)  = intHost $(hashQ "Vector3Type") <> encodeSingleType t
encodeVectorType (Vector4Type t)  = intHost $(hashQ "Vector4Type") <> encodeSingleType t
encodeVectorType (Vector8Type t)  = intHost $(hashQ "Vector8Type") <> encodeSingleType t
encodeVectorType (Vector16Type t) = intHost $(hashQ "Vector16Type") <> encodeSingleType t

{-# INLINE encodeBoundedType #-}
encodeBoundedType :: BoundedType t -> Builder
encodeBoundedType (IntegralBoundedType t) = intHost $(hashQ "IntegralBoundedType") <> encodeIntegralType t
encodeBoundedType (NonNumBoundedType t)   = intHost $(hashQ "NonNumBoundedType")   <> encodeNonNumType t

{-# INLINE encodeNonNumType #-}
encodeNonNumType :: NonNumType t -> Builder
encodeNonNumType TypeBool{}   = intHost $(hashQ "Bool")
encodeNonNumType TypeChar{}   = intHost $(hashQ "Char")
encodeNonNumType TypeCChar{}  = intHost $(hashQ "CChar")
encodeNonNumType TypeCSChar{} = intHost $(hashQ "CSChar")
encodeNonNumType TypeCUChar{} = intHost $(hashQ "CUChar")

{-# INLINE encodeNumType #-}
encodeNumType :: NumType t -> Builder
encodeNumType (IntegralNumType t) = intHost $(hashQ "IntegralNumType") <> encodeIntegralType t
encodeNumType (FloatingNumType t) = intHost $(hashQ "FloatingNumType") <> encodeFloatingType t

{-# INLINE encodeIntegralType #-}
encodeIntegralType :: IntegralType t -> Builder
encodeIntegralType TypeInt{}     = intHost $(hashQ "Int")
encodeIntegralType TypeInt8{}    = intHost $(hashQ "Int8")
encodeIntegralType TypeInt16{}   = intHost $(hashQ "Int16")
encodeIntegralType TypeInt32{}   = intHost $(hashQ "Int32")
encodeIntegralType TypeInt64{}   = intHost $(hashQ "Int64")
encodeIntegralType TypeWord{}    = intHost $(hashQ "Word")
encodeIntegralType TypeWord8{}   = intHost $(hashQ "Word8")
encodeIntegralType TypeWord16{}  = intHost $(hashQ "Word16")
encodeIntegralType TypeWord32{}  = intHost $(hashQ "Word32")
encodeIntegralType TypeWord64{}  = intHost $(hashQ "Word64")
encodeIntegralType TypeCShort{}  = intHost $(hashQ "CShort")
encodeIntegralType TypeCUShort{} = intHost $(hashQ "CUShort")
encodeIntegralType TypeCInt{}    = intHost $(hashQ "CInt")
encodeIntegralType TypeCUInt{}   = intHost $(hashQ "CUInt")
encodeIntegralType TypeCLong{}   = intHost $(hashQ "CLong")
encodeIntegralType TypeCULong{}  = intHost $(hashQ "CULong")
encodeIntegralType TypeCLLong{}  = intHost $(hashQ "CLLong")
encodeIntegralType TypeCULLong{} = intHost $(hashQ "CULLong")

{-# INLINE encodeFloatingType #-}
encodeFloatingType :: FloatingType t -> Builder
encodeFloatingType TypeHalf{}    = intHost $(hashQ "Half")
encodeFloatingType TypeFloat{}   = intHost $(hashQ "Float")
encodeFloatingType TypeDouble{}  = intHost $(hashQ "Double")
encodeFloatingType TypeCFloat{}  = intHost $(hashQ "CFloat")
encodeFloatingType TypeCDouble{} = intHost $(hashQ "CDouble")

