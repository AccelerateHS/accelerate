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
-- Copyright   : [2017..2020] The Accelerate Team
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
  hashOpenFun, hashOpenExp,

  -- auxiliary
  EncodeAcc,
  encodePreOpenAcc,
  encodeOpenExp,
  encodeOpenFun,
  encodeArraysType,
  hashQ,

) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.Analysis.Hash.TH
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Slice
import Data.Array.Accelerate.Representation.Stencil
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type
import Data.Primitive.Vec
import Crypto.Hash.XKCP

import Foreign.C.Types
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra
import Data.ByteString.Short.Internal                               ( ShortByteString(..) )
import Data.Monoid
import System.IO.Unsafe                                             ( unsafePerformIO )
import System.Mem.StableName                                        ( hashStableName, makeStableName )
import Prelude                                                      hiding ( exp )
import qualified Data.Hashable                                      as Hashable

import GHC.TypeLits


-- Hashing
-- -------

type Hash = SHA3_256

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
hashPreOpenAcc :: HasArraysR acc => EncodeAcc acc -> PreOpenAcc acc aenv a -> Hash
hashPreOpenAcc = hashPreOpenAccWith defaultHashOptions

{-# INLINEABLE hashPreOpenAccWith #-}
hashPreOpenAccWith :: HasArraysR acc => HashOptions -> EncodeAcc acc -> PreOpenAcc acc aenv a -> Hash
hashPreOpenAccWith options encodeAcc
  = hashlazy
  . toLazyByteString
  . encodePreOpenAcc options encodeAcc

{-# INLINEABLE hashOpenFun #-}
hashOpenFun :: OpenFun env aenv f -> Hash
hashOpenFun
  = hashlazy
  . toLazyByteString
  . encodeOpenFun

{-# INLINEABLE hashOpenExp #-}
hashOpenExp :: OpenExp env aenv t -> Hash
hashOpenExp
  = hashlazy
  . toLazyByteString
  . encodeOpenExp


-- Array computations
-- ------------------

type EncodeAcc acc = forall aenv a. HashOptions -> acc aenv a -> Builder

{-# INLINEABLE encodePreOpenAcc #-}
encodePreOpenAcc
    :: forall acc aenv arrs. HasArraysR acc
    => HashOptions
    -> EncodeAcc acc
    -> PreOpenAcc acc aenv arrs
    -> Builder
encodePreOpenAcc options encodeAcc pacc =
  let
      travA :: forall aenv' a. acc aenv' a -> Builder
      travA = encodeAcc options

      travAF :: PreOpenAfun acc aenv' f -> Builder
      travAF = encodePreOpenAfun options encodeAcc

      travE :: OpenExp env' aenv' e -> Builder
      travE = encodeOpenExp

      travF :: OpenFun env' aenv' f -> Builder
      travF = encodeOpenFun

      travD :: Direction -> Builder
      travD LeftToRight = intHost $(hashQ "L")
      travD RightToLeft = intHost $(hashQ "R")

      deep :: Builder -> Builder
      deep | perfect options = id
           | otherwise       = const mempty

      deepE :: forall env' aenv' e. OpenExp env' aenv' e -> Builder
      deepE e
        | perfect options = travE e
        | otherwise       = encodeTypeR $ expType e
  in
  case pacc of
    Alet lhs bnd body               -> intHost $(hashQ "Alet")        <> encodeLeftHandSide encodeArrayType lhs <> travA bnd <> travA body
    Avar (Var repr v)               -> intHost $(hashQ "Avar")        <> encodeArrayType repr <> deep (encodeIdx v)
    Apair a1 a2                     -> intHost $(hashQ "Apair")       <> travA a1 <> travA a2
    Anil                            -> intHost $(hashQ "Anil")
    Atrace (Message _ _ msg) as bs  -> intHost $(hashQ "Atrace")      <> intHost (Hashable.hash msg) <> travA as <> travA bs
    Acoerce _ bR a                  -> intHost $(hashQ "Acoerce")     <> encodeTypeR bR <> travA a
    Apply _ f a                     -> intHost $(hashQ "Apply")       <> travAF f <> travA a
    Aforeign _ _ f a                -> intHost $(hashQ "Aforeign")    <> travAF f <> travA a
    Use repr a                      -> intHost $(hashQ "Use")         <> encodeArrayType repr <> deep (encodeArray a)
    Awhile p f a                    -> intHost $(hashQ "Awhile")      <> travAF f <> travAF p <> travA a
    Unit _ e                        -> intHost $(hashQ "Unit")        <> travE e
    Generate _ e f                  -> intHost $(hashQ "Generate")    <> deepE e <> travF f
    -- We don't need to encode the type of 'e' when perfect is False, as 'e' is an expression of type Bool.
    -- We thus use `deep (travE e)` instead of `deepE e`.
    Acond e a1 a2                   -> intHost $(hashQ "Acond")       <> deep (travE e) <> travA a1 <> travA a2
    Reshape _ sh a                  -> intHost $(hashQ "Reshape")     <> deepE sh <> travA a
    Backpermute _ sh f a            -> intHost $(hashQ "Backpermute") <> deepE sh <> travF f  <> travA a
    Transform _ sh f1 f2 a          -> intHost $(hashQ "Transform")   <> deepE sh <> travF f1 <> travF f2 <> travA a
    Replicate spec ix a             -> intHost $(hashQ "Replicate")   <> deepE ix <> travA a  <> encodeSliceIndex spec
    Slice spec a ix                 -> intHost $(hashQ "Slice")       <> deepE ix <> travA a  <> encodeSliceIndex spec
    Map _ f a                       -> intHost $(hashQ "Map")         <> travF f  <> travA a
    ZipWith _ f a1 a2               -> intHost $(hashQ "ZipWith")     <> travF f  <> travA a1 <> travA a2
    Fold f e a                      -> intHost $(hashQ "Fold")        <> travF f  <> encodeMaybe travE e  <> travA a
    FoldSeg _ f e a s               -> intHost $(hashQ "FoldSeg")     <> travF f  <> encodeMaybe travE e  <> travA a <> travA s
    Scan  d f e a                   -> intHost $(hashQ "Scan")        <> travD d  <> travF f  <> encodeMaybe travE e <> travA a
    Scan' d f e a                   -> intHost $(hashQ "Scan'")       <> travD d  <> travF f  <>             travE e <> travA a
    Permute f1 a1 f2 a2             -> intHost $(hashQ "Permute")     <> travF f1 <> travA a1 <> travF f2 <> travA a2
    Stencil s _ f b a               -> intHost $(hashQ "Stencil")     <> travF f  <> encodeBoundary (stencilEltR s) b   <> travA a
    Stencil2 s1 s2 _ f b1 a1 b2 a2  -> intHost $(hashQ "Stencil2")    <> travF f  <> encodeBoundary (stencilEltR s1) b1 <> travA a1 <> encodeBoundary (stencilEltR s2) b2 <> travA a2

{--
{-# INLINEABLE encodePreOpenSeq #-}
encodePreOpenSeq :: forall acc aenv senv arrs. EncodeAcc acc -> PreOpenSeq acc aenv senv arrs -> Int
encodePreOpenSeq encodeAcc s =
  let
      travA :: acc aenv' a -> Builder
      travA = encodeAcc -- XXX: plus type information?

      travE :: OpenExp env' aenv' e -> Builder
      travE = encodeOpenExp encodeAcc

      travAF :: PreOpenAfun acc aenv' f -> Builder
      travAF = encodePreOpenAfun encodeAcc

      travF :: OpenFun env' aenv' f -> Builder
      travF = encodeOpenFun encodeAcc

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

encodeArray :: Array sh e -> Builder
encodeArray ad = intHost . unsafePerformIO $! hashStableName <$> makeStableName ad

encodeTupR :: (forall b. s b -> Builder) -> TupR s a -> Builder
encodeTupR _ TupRunit         = intHost $(hashQ "TupRunit")
encodeTupR f (TupRpair r1 r2) = intHost $(hashQ "TupRpair")   <> encodeTupR f r1 <> encodeTupR f r2
encodeTupR f (TupRsingle s)   = intHost $(hashQ "TupRsingle") <> f s

encodeLeftHandSide :: (forall b. s b -> Builder) -> LeftHandSide s a env env' -> Builder
encodeLeftHandSide f (LeftHandSideWildcard r) = intHost $(hashQ "LeftHandSideWildcard") <> encodeTupR f r
encodeLeftHandSide f (LeftHandSidePair r1 r2) = intHost $(hashQ "LeftHandSidePair")     <> encodeLeftHandSide f r1 <> encodeLeftHandSide f r2
encodeLeftHandSide f (LeftHandSideSingle s)   = intHost $(hashQ "LeftHandSideArray")    <> f s

encodeArrayType :: ArrayR a -> Builder
encodeArrayType (ArrayR shr tp) = encodeShapeR shr <> encodeTypeR tp

encodeArraysType :: ArraysR arrs -> Builder
encodeArraysType = encodeTupR encodeArrayType

encodeShapeR :: ShapeR sh -> Builder
encodeShapeR = int64Host . rank

encodePreOpenAfun
    :: forall acc aenv f.
       HashOptions
    -> EncodeAcc acc
    -> PreOpenAfun acc aenv f
    -> Builder
encodePreOpenAfun options travA afun =
  let
      travL :: forall aenv1 aenv2 a b. ALeftHandSide a aenv1 aenv2 -> PreOpenAfun acc aenv2 b -> Builder
      travL lhs l = encodeLeftHandSide encodeArrayType lhs <> encodePreOpenAfun options travA l
  in
  case afun of
    Abody b    -> intHost $(hashQ "Abody") <> travA options b
    Alam lhs l -> intHost $(hashQ "Alam")  <> travL lhs  l


encodeBoundary
    :: TypeR e
    -> Boundary aenv (Array sh e)
    -> Builder
encodeBoundary _  Wrap          = intHost $(hashQ "Wrap")
encodeBoundary _  Clamp         = intHost $(hashQ "Clamp")
encodeBoundary _  Mirror        = intHost $(hashQ "Mirror")
encodeBoundary tR (Constant c)  = intHost $(hashQ "Constant") <> encodeConst tR c
encodeBoundary _  (Function f)  = intHost $(hashQ "Function") <> encodeOpenFun f

encodeSliceIndex :: SliceIndex slix sl co sh -> Builder
encodeSliceIndex SliceNil       = intHost $(hashQ "SliceNil")
encodeSliceIndex (SliceAll r)   = intHost $(hashQ "SliceAll")   <> encodeSliceIndex r
encodeSliceIndex (SliceFixed r) = intHost $(hashQ "sliceFixed") <> encodeSliceIndex r


-- Scalar expressions
-- ------------------

{-# INLINEABLE encodeOpenExp #-}
encodeOpenExp
    :: forall env aenv exp.
       OpenExp env aenv exp
    -> Builder
encodeOpenExp exp =
  let
      travE :: forall env' aenv' e. OpenExp env' aenv' e -> Builder
      travE e = encodeOpenExp e

      travF :: OpenFun env' aenv' f -> Builder
      travF = encodeOpenFun
  in
  case exp of
    Let lhs bnd body            -> intHost $(hashQ "Let")         <> encodeLeftHandSide encodeScalarType lhs <> travE bnd <> travE body
    Evar (Var tp ix)            -> intHost $(hashQ "Evar")        <> encodeScalarType tp <> encodeIdx ix
    Nil                         -> intHost $(hashQ "Nil")
    Pair e1 e2                  -> intHost $(hashQ "Pair")        <> travE e1 <> travE e2
    Extract vR iR v i           -> intHost $(hashQ "Extract")     <> encodeScalarType vR <> encodeSingleIntegralType iR <> travE v <> travE i
    Insert vR iR v i x          -> intHost $(hashQ "Insert")      <> encodeScalarType vR <> encodeSingleIntegralType iR <> travE v <> travE i <> travE x
    Shuffle eR iR x y i         -> intHost $(hashQ "Shuffle")     <> encodeScalarType eR <> encodeSingleIntegralType iR <> travE x <> travE y <> travE i
    Select eR m x y             -> intHost $(hashQ "Select")      <> encodeScalarType eR <> travE m <>travE x <> travE y
    Const tp c                  -> intHost $(hashQ "Const")       <> encodeScalarConst tp c
    Undef tp                    -> intHost $(hashQ "Undef")       <> encodeScalarType tp
    IndexSlice spec ix sh       -> intHost $(hashQ "IndexSlice")  <> travE ix <> travE sh <> encodeSliceIndex spec
    IndexFull  spec ix sl       -> intHost $(hashQ "IndexFull")   <> travE ix <> travE sl <> encodeSliceIndex spec
    ToIndex _ sh i              -> intHost $(hashQ "ToIndex")     <> travE sh <> travE i
    FromIndex _ sh i            -> intHost $(hashQ "FromIndex")   <> travE sh <> travE i
    Case eR e rhs def           -> intHost $(hashQ "Case")        <> encodeTagType eR <> travE e <> mconcat [ encodeTagConst eR t <> travE c | (t,c) <- rhs ] <> encodeMaybe travE def
    Cond c t e                  -> intHost $(hashQ "Cond")        <> travE c  <> travE t  <> travE e
    While p f x                 -> intHost $(hashQ "While")       <> travF p  <> travF f  <> travE x
    PrimApp f x                 -> intHost $(hashQ "PrimApp")     <> encodePrimFun f <> travE x
    Index a ix                  -> intHost $(hashQ "Index")       <> encodeArrayVar a <> travE ix
    LinearIndex a ix            -> intHost $(hashQ "LinearIndex") <> encodeArrayVar a <> travE ix
    Shape a                     -> intHost $(hashQ "Shape")       <> encodeArrayVar a
    ShapeSize _ sh              -> intHost $(hashQ "ShapeSize")   <> travE sh
    Foreign _ _ f e             -> intHost $(hashQ "Foreign")     <> travF f  <> travE e
    Bitcast _ tp e              -> intHost $(hashQ "Bitcast")     <> encodeScalarType tp <> travE e

encodeArrayVar :: ArrayVar aenv a -> Builder
encodeArrayVar (Var repr v) = encodeArrayType repr <> encodeIdx v

{-# INLINEABLE encodeOpenFun #-}
encodeOpenFun
    :: OpenFun env aenv f
    -> Builder
encodeOpenFun (Body b)    = intHost $(hashQ "Body") <> encodeOpenExp b
encodeOpenFun (Lam lhs l) = intHost $(hashQ "Lam") <> encodeLeftHandSide encodeScalarType lhs <> encodeOpenFun l


encodeConst :: TypeR t -> t -> Builder
encodeConst TupRunit         ()    = intHost $(hashQ "nil")
encodeConst (TupRsingle t)   c     = encodeScalarConst t c
encodeConst (TupRpair ta tb) (a,b) = intHost $(hashQ "pair") <> encodeConst ta a <> encodeConst tb b

encodeScalarConst :: ScalarType t -> t -> Builder
encodeScalarConst (NumScalarType t) = encodeNumConst t
encodeScalarConst (BitScalarType t) = encodeBitConst t

encodeBitConst :: BitType t -> t -> Builder
encodeBitConst TypeBit      (Bit False) = intHost $(hashQ "Bit") <> int8 0
encodeBitConst TypeBit      (Bit True)  = intHost $(hashQ "Bit") <> int8 1
encodeBitConst (TypeMask n) (Vec ba#)   = intHost $(hashQ "BitMask") <> int8 (fromIntegral (natVal' n)) <> shortByteString (SBS ba#)

encodeNumConst :: NumType t -> t -> Builder
encodeNumConst (IntegralNumType t) = encodeIntegralConst t
encodeNumConst (FloatingNumType t) = encodeFloatingConst t

encodeIntegralConst :: IntegralType t -> t -> Builder
encodeIntegralConst (SingleIntegralType t)   x         = encodeSingleIntegralConst t x
encodeIntegralConst (VectorIntegralType n t) (Vec ba#) = intHost $(hashQ "Vec") <> int8 (fromIntegral (natVal' n)) <> encodeSingleIntegralType t <> shortByteString (SBS ba#)

encodeSingleIntegralConst :: SingleIntegralType t -> t -> Builder
encodeSingleIntegralConst TypeInt8   x              = intHost $(hashQ "Int8")    <> int8 x
encodeSingleIntegralConst TypeInt16  x              = intHost $(hashQ "Int16")   <> int16Host x
encodeSingleIntegralConst TypeInt32  x              = intHost $(hashQ "Int32")   <> int32Host x
encodeSingleIntegralConst TypeInt64  x              = intHost $(hashQ "Int64")   <> int64Host x
encodeSingleIntegralConst TypeInt128 (Int128 x y)   = intHost $(hashQ "Int128")  <> word64Host x <> word64Host y
encodeSingleIntegralConst TypeWord8  x              = intHost $(hashQ "Word8")   <> word8 x
encodeSingleIntegralConst TypeWord16 x              = intHost $(hashQ "Word16")  <> word16Host x
encodeSingleIntegralConst TypeWord32 x              = intHost $(hashQ "Word32")  <> word32Host x
encodeSingleIntegralConst TypeWord64 x              = intHost $(hashQ "Word64")  <> word64Host x
encodeSingleIntegralConst TypeWord128 (Word128 x y) = intHost $(hashQ "Word128") <> word64Host x <> word64Host y

encodeFloatingConst :: FloatingType t -> t -> Builder
encodeFloatingConst (SingleFloatingType t)   x         = encodeSingleFloatingConst t x
encodeFloatingConst (VectorFloatingType n t) (Vec ba#) = intHost $(hashQ "Vec") <> int8 (fromIntegral (natVal' n)) <> encodeSingleFloatingType t <> shortByteString (SBS ba#)

encodeSingleFloatingConst :: SingleFloatingType t -> t -> Builder
encodeSingleFloatingConst TypeFloat16  (Half (CUShort x)) = intHost $(hashQ "Half")     <> word16Host x
encodeSingleFloatingConst TypeFloat32  x                  = intHost $(hashQ "Float")    <> floatHost x
encodeSingleFloatingConst TypeFloat64  x                  = intHost $(hashQ "Double")   <> doubleHost x
encodeSingleFloatingConst TypeFloat128 (Float128 x y)     = intHost $(hashQ "Float128") <> word64Host x <> word64Host y

encodeTagConst :: TagType t -> t -> Builder
encodeTagConst TagBit    (Bit False) = intHost $(hashQ "Bit")   <> int8 0
encodeTagConst TagBit    (Bit True)  = intHost $(hashQ "Bit")   <> int8 1
encodeTagConst TagWord8  x           = intHost $(hashQ "Tag8")  <> word8 x
encodeTagConst TagWord16 x           = intHost $(hashQ "Tag16") <> word16Host x

encodePrimFun :: PrimFun f -> Builder
encodePrimFun (PrimAdd a)                = intHost $(hashQ "PrimAdd")                <> encodeNumType a
encodePrimFun (PrimSub a)                = intHost $(hashQ "PrimSub")                <> encodeNumType a
encodePrimFun (PrimMul a)                = intHost $(hashQ "PrimMul")                <> encodeNumType a
encodePrimFun (PrimNeg a)                = intHost $(hashQ "PrimNeg")                <> encodeNumType a
encodePrimFun (PrimAbs a)                = intHost $(hashQ "PrimAbs")                <> encodeNumType a
encodePrimFun (PrimSig a)                = intHost $(hashQ "PrimSig")                <> encodeNumType a
encodePrimFun (PrimVAdd a)               = intHost $(hashQ "PrimVAdd")               <> encodeNumType a
encodePrimFun (PrimVMul a)               = intHost $(hashQ "PrimVMul")               <> encodeNumType a
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
encodePrimFun (PrimBReverse a)           = intHost $(hashQ "PrimBReverse")           <> encodeIntegralType a
encodePrimFun (PrimBSwap a)              = intHost $(hashQ "PrimBSwap")              <> encodeIntegralType a
encodePrimFun (PrimVBAnd a)              = intHost $(hashQ "PrimVBAnd")              <> encodeIntegralType a
encodePrimFun (PrimVBOr a)               = intHost $(hashQ "PrimVBOr")               <> encodeIntegralType a
encodePrimFun (PrimVBXor a)              = intHost $(hashQ "PrimVBXor")              <> encodeIntegralType a
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
encodePrimFun (PrimLt a)                 = intHost $(hashQ "PrimLt")                 <> encodeScalarType a
encodePrimFun (PrimGt a)                 = intHost $(hashQ "PrimGt")                 <> encodeScalarType a
encodePrimFun (PrimLtEq a)               = intHost $(hashQ "PrimLtEq")               <> encodeScalarType a
encodePrimFun (PrimGtEq a)               = intHost $(hashQ "PrimGtEq")               <> encodeScalarType a
encodePrimFun (PrimEq a)                 = intHost $(hashQ "PrimEq")                 <> encodeScalarType a
encodePrimFun (PrimNEq a)                = intHost $(hashQ "PrimNEq")                <> encodeScalarType a
encodePrimFun (PrimMin a)                = intHost $(hashQ "PrimMin")                <> encodeScalarType a
encodePrimFun (PrimMax a)                = intHost $(hashQ "PrimMax")                <> encodeScalarType a
encodePrimFun (PrimVMin a)               = intHost $(hashQ "PrimVMin")               <> encodeScalarType a
encodePrimFun (PrimVMax a)               = intHost $(hashQ "PrimVMax")               <> encodeScalarType a
encodePrimFun (PrimLAnd a)               = intHost $(hashQ "PrimLAnd")               <> encodeBitType a
encodePrimFun (PrimLOr a)                = intHost $(hashQ "PrimLOr")                <> encodeBitType a
encodePrimFun (PrimLNot a)               = intHost $(hashQ "PrimLNot")               <> encodeBitType a
encodePrimFun (PrimVLAnd a)              = intHost $(hashQ "PrimVLAnd")              <> encodeBitType a
encodePrimFun (PrimVLOr a)               = intHost $(hashQ "PrimVLOr")               <> encodeBitType a
encodePrimFun (PrimFromIntegral a b)     = intHost $(hashQ "PrimFromIntegral")       <> encodeIntegralType a <> encodeNumType b
encodePrimFun (PrimToFloating a b)       = intHost $(hashQ "PrimToFloating")         <> encodeNumType a <> encodeFloatingType b
encodePrimFun (PrimToBool a b)           = intHost $(hashQ "PrimToBool")             <> encodeIntegralType a <> encodeBitType b
encodePrimFun (PrimFromBool a b)         = intHost $(hashQ "PrimFromBool")           <> encodeBitType a <> encodeIntegralType b

encodeTypeR :: TypeR t -> Builder
encodeTypeR TupRunit       = intHost $(hashQ "TupRunit")
encodeTypeR (TupRsingle t) = intHost $(hashQ "TupRsingle") <> encodeScalarType t
encodeTypeR (TupRpair a b) = intHost $(hashQ "TupRpair")   <> encodeTypeR a <> intHost (depthTypeR a)
                                                           <> encodeTypeR b <> intHost (depthTypeR b)

depthTypeR :: TypeR t -> Int
depthTypeR TupRunit       = 0
depthTypeR TupRsingle{}   = 1
depthTypeR (TupRpair a b) = depthTypeR a + depthTypeR b

encodeScalarType :: ScalarType t -> Builder
encodeScalarType (NumScalarType t) = encodeNumType t
encodeScalarType (BitScalarType t) = encodeBitType t

encodeBitType :: BitType t -> Builder
encodeBitType TypeBit      = intHost $(hashQ "Bit")
encodeBitType (TypeMask n) = intHost $(hashQ "BitMask") <> int8 (fromIntegral (natVal' n))

encodeNumType :: NumType t -> Builder
encodeNumType (IntegralNumType t) = intHost $(hashQ "IntegralNumType") <> encodeIntegralType t
encodeNumType (FloatingNumType t) = intHost $(hashQ "FloatingNumType") <> encodeFloatingType t

encodeIntegralType :: IntegralType t -> Builder
encodeIntegralType (SingleIntegralType t)   = encodeSingleIntegralType t
encodeIntegralType (VectorIntegralType n t) = intHost $(hashQ "Vec") <> int8 (fromIntegral (natVal' n)) <> encodeSingleIntegralType t

encodeSingleIntegralType :: SingleIntegralType t -> Builder
encodeSingleIntegralType (TypeInt n)  = intHost $(hashQ "Int")  <> intHost n
encodeSingleIntegralType (TypeWord n) = intHost $(hashQ "Word") <> intHost n

encodeFloatingType :: FloatingType t -> Builder
encodeFloatingType (SingleFloatingType t)   = encodeSingleFloatingType t
encodeFloatingType (VectorFloatingType n t) = intHost $(hashQ "Vec") <> int8 (fromIntegral (natVal' n)) <> encodeSingleFloatingType t

encodeSingleFloatingType :: SingleFloatingType t -> Builder
encodeSingleFloatingType TypeFloat16  = intHost $(hashQ "Half")
encodeSingleFloatingType TypeFloat32  = intHost $(hashQ "Float")
encodeSingleFloatingType TypeFloat64  = intHost $(hashQ "Double")
encodeSingleFloatingType TypeFloat128 = intHost $(hashQ "Float128")

encodeTagType :: TagType t -> Builder
encodeTagType TagBit    = intHost $(hashQ "TagBit")
encodeTagType TagWord8  = intHost $(hashQ "TagWord8")
encodeTagType TagWord16 = intHost $(hashQ "TagWord16")

encodeMaybe :: (a -> Builder) -> Maybe a -> Builder
encodeMaybe _ Nothing  = intHost $(hashQ "Nothing")
encodeMaybe f (Just x) = intHost $(hashQ "Just") <> f x

