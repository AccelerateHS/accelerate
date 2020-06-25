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

      travMaybe :: (a -> Builder) -> Maybe a -> Builder
      travMaybe _ Nothing  = intHost $(hashQ "Nothing")
      travMaybe f (Just x) = intHost $(hashQ "Just") <> f x

      deep :: Builder -> Builder
      deep | perfect options = id
           | otherwise       = const mempty

      deepE :: forall env' aenv' e. OpenExp env' aenv' e -> Builder
      deepE e
        | perfect options = travE e
        | otherwise       = encodeTypeR $ expType e
  in
  case pacc of
    Alet lhs bnd body            -> intHost $(hashQ "Alet")        <> encodeLeftHandSide encodeArrayType lhs <> travA bnd <> travA body
    Avar (Var repr v)            -> intHost $(hashQ "Avar")        <> encodeArrayType repr <> deep (encodeIdx v)
    Apair a1 a2                  -> intHost $(hashQ "Apair")       <> travA a1 <> travA a2
    Anil                         -> intHost $(hashQ "Anil")
    Apply _ f a                  -> intHost $(hashQ "Apply")       <> travAF f <> travA a
    Aforeign _ _ f a             -> intHost $(hashQ "Aforeign")    <> travAF f <> travA a
    Use repr a                   -> intHost $(hashQ "Use")         <> encodeArrayType repr <> deep (encodeArray a)
    Awhile p f a                 -> intHost $(hashQ "Awhile")      <> travAF f <> travAF p <> travA a
    Unit _ e                     -> intHost $(hashQ "Unit")        <> travE e
    Generate _ e f               -> intHost $(hashQ "Generate")    <> deepE e <> travF f
    -- We don't need to encode the type of 'e' when perfect is False, as 'e' is an expression of type Bool.
    -- We thus use `deep (travE e)` instead of `deepE e`.
    Acond e a1 a2                -> intHost $(hashQ "Acond")       <> deep (travE e) <> travA a1 <> travA a2
    Reshape _ sh a               -> intHost $(hashQ "Reshape")     <> deepE sh <> travA a
    Backpermute _ sh f a         -> intHost $(hashQ "Backpermute") <> deepE sh <> travF f  <> travA a
    Transform _ sh f1 f2 a       -> intHost $(hashQ "Transform")   <> deepE sh <> travF f1 <> travF f2 <> travA a
    Replicate spec ix a          -> intHost $(hashQ "Replicate")   <> deepE ix <> travA a  <> encodeSliceIndex spec
    Slice spec a ix              -> intHost $(hashQ "Slice")       <> deepE ix <> travA a  <> encodeSliceIndex spec
    Map _ f a                    -> intHost $(hashQ "Map")         <> travF f  <> travA a
    ZipWith _ f a1 a2            -> intHost $(hashQ "ZipWith")     <> travF f  <> travA a1 <> travA a2
    Fold f e a                   -> intHost $(hashQ "Fold")        <> travF f  <> travMaybe travE e  <> travA a
    FoldSeg _ f e a s            -> intHost $(hashQ "FoldSeg")     <> travF f  <> travMaybe travE e  <> travA a  <> travA s
    Scan  d f e a                -> intHost $(hashQ "Scan")        <> travD d  <> travF f  <> travMaybe travE e  <> travA a
    Scan' d f e a                -> intHost $(hashQ "Scan'")       <> travD d  <> travF f  <>           travE e  <> travA a
    Permute f1 a1 f2 a2          -> intHost $(hashQ "Permute")     <> travF f1 <> travA a1 <> travF f2 <> travA a2
    Stencil s _ f b a            -> intHost $(hashQ "Stencil")     <> travF f  <> encodeBoundary (stencilEltR s) b  <> travA a
    Stencil2 s1 s2 _ f b1 a1 b2 a2 -> intHost $(hashQ "Stencil2")  <> travF f  <> encodeBoundary (stencilEltR s1) b1 <> travA a1 <> encodeBoundary (stencilEltR s2) b2 <> travA a2

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
encodeShapeR = intHost . rank

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
encodeBoundary tp (Constant c)  = intHost $(hashQ "Constant") <> encodeConst tp c
encodeBoundary _  (Function f)  = intHost $(hashQ "Function") <> encodeOpenFun f

encodeSliceIndex :: SliceIndex slix sl co sh -> Builder
encodeSliceIndex SliceNil         = intHost $(hashQ "SliceNil")
encodeSliceIndex (SliceAll r)     = intHost $(hashQ "SliceAll")   <> encodeSliceIndex r
encodeSliceIndex (SliceFixed r)   = intHost $(hashQ "sliceFixed") <> encodeSliceIndex r


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
    VecPack   _ e               -> intHost $(hashQ "VecPack")     <> travE e
    VecUnpack _ e               -> intHost $(hashQ "VecUnpack")   <> travE e
    Const tp c                  -> intHost $(hashQ "Const")       <> encodeScalarConst tp c
    Undef tp                    -> intHost $(hashQ "Undef")       <> encodeScalarType tp
    IndexSlice spec ix sh       -> intHost $(hashQ "IndexSlice")  <> travE ix <> travE sh <> encodeSliceIndex spec
    IndexFull  spec ix sl       -> intHost $(hashQ "IndexFull")   <> travE ix <> travE sl <> encodeSliceIndex spec
    ToIndex _ sh i              -> intHost $(hashQ "ToIndex")     <> travE sh <> travE i
    FromIndex _ sh i            -> intHost $(hashQ "FromIndex")   <> travE sh <> travE i
    Case e rhs                  -> intHost $(hashQ "Case")        <> travE e  <> mconcat [ encodeTag t <> travE c | (t,c) <- rhs ]
    Cond c t e                  -> intHost $(hashQ "Cond")        <> travE c  <> travE t  <> travE e
    While p f x                 -> intHost $(hashQ "While")       <> travF p  <> travF f  <> travE x
    PrimApp f x                 -> intHost $(hashQ "PrimApp")     <> encodePrimFun f <> travE x
    PrimConst c                 -> intHost $(hashQ "PrimConst")   <> encodePrimConst c
    Index a ix                  -> intHost $(hashQ "Index")       <> encodeArrayVar a <> travE ix
    LinearIndex a ix            -> intHost $(hashQ "LinearIndex") <> encodeArrayVar a <> travE ix
    Shape a                     -> intHost $(hashQ "Shape")       <> encodeArrayVar a
    ShapeSize _ sh              -> intHost $(hashQ "ShapeSize")   <> travE sh
    Foreign _ _ f e             -> intHost $(hashQ "Foreign")     <> travF f  <> travE e
    Coerce _ tp e               -> intHost $(hashQ "Coerce")      <> encodeScalarType tp <> travE e

encodeArrayVar :: ArrayVar aenv a -> Builder
encodeArrayVar (Var repr v) = encodeArrayType repr <> encodeIdx v

encodeTag :: TagR t -> Builder
encodeTag TagRunit         = intHost $(hashQ "TagRunit")
encodeTag (TagRsingle t)   = intHost $(hashQ "TagRsingle") <> encodeScalarType t
encodeTag (TagRundef t)    = intHost $(hashQ "TagRundef")  <> encodeScalarType t
encodeTag (TagRtag t a)    = intHost $(hashQ "TagRtag")    <> word8 t <> encodeTag a
encodeTag (TagRpair ta tb) = intHost $(hashQ "TagRpair")   <> encodeTag ta <> encodeTag tb

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
encodeScalarConst (SingleScalarType t) = encodeSingleConst t
encodeScalarConst (VectorScalarType t) = encodeVectorConst t

encodeSingleConst :: SingleType t -> t -> Builder
encodeSingleConst (NumSingleType t) = encodeNumConst t

encodeVectorConst :: VectorType (Vec n t) -> Vec n t -> Builder
encodeVectorConst (VectorType n t) (Vec ba#) = intHost $(hashQ "Vec") <> intHost n <> encodeSingleType t <> shortByteString (SBS ba#)

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
encodeScalarType (SingleScalarType t) = intHost $(hashQ "SingleScalarType") <> encodeSingleType t
encodeScalarType (VectorScalarType t) = intHost $(hashQ "VectorScalarType") <> encodeVectorType t

encodeSingleType :: SingleType t -> Builder
encodeSingleType (NumSingleType t) = intHost $(hashQ "NumSingleType")    <> encodeNumType t

encodeVectorType :: VectorType (Vec n t) -> Builder
encodeVectorType (VectorType n t) = intHost $(hashQ "VectorType") <> intHost n <> encodeSingleType t

encodeBoundedType :: BoundedType t -> Builder
encodeBoundedType (IntegralBoundedType t) = intHost $(hashQ "IntegralBoundedType") <> encodeIntegralType t

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

