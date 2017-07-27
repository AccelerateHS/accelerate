{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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
  HashAcc,
  hashPreOpenAcc, hashOpenAcc,
  hashPreOpenExp, hashOpenExp,
  hashPreOpenFun,

  -- auxiliary
  hashQ,
  commutes,

) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Hash.TH
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Array.Representation                   ( SliceIndex(..) )
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Type

import Data.Hashable
import Foreign.C.Types
import Data.Maybe                                                   ( fromMaybe )
import System.Mem.StableName                                        ( hashStableName, makeStableName )
import System.IO.Unsafe                                             ( unsafePerformIO )
import Prelude                                                      hiding ( exp )


-- Array computations
-- ------------------

type HashAcc acc = forall aenv a. acc aenv a -> Int


hashOpenAcc :: OpenAcc aenv arrs -> Int
hashOpenAcc (OpenAcc pacc) = hashPreOpenAcc hashOpenAcc pacc

hashPreOpenAcc :: forall acc aenv arrs. HashAcc acc -> PreOpenAcc acc aenv arrs -> Int
hashPreOpenAcc hashAcc pacc =
  let
    hashA :: forall aenv' a. Arrays a => Int -> acc aenv' a -> Int
    hashA salt
      = hashWithSalt salt
      . hashWithSalt (hashArraysType (arrays (undefined::a)))
      . hashAcc

    hashE :: Int -> PreOpenExp acc env' aenv' e -> Int
    hashE salt = hashWithSalt salt . hashPreOpenExp hashAcc

    hashF :: Int -> PreOpenFun acc env' aenv' f -> Int
    hashF salt = hashWithSalt salt . hashPreOpenFun hashAcc

    hashB :: Int -> PreBoundary acc aenv' (Array sh e) -> Int
    hashB salt = hashWithSalt salt . hashPreBoundary hashAcc

    -- hashS :: Int -> PreOpenSeq acc aenv senv arrs -> Int
    -- hashS salt = hashWithSalt salt . hashPreOpenSeq hashAcc

    nacl :: Arrays arrs => Int
    nacl = hashArraysType (arrays (undefined::arrs))

  in case pacc of
    Alet bnd body               -> $(hashQ "Alet")        `hashA` bnd `hashA` body
    Avar v                      -> $(hashQ "Avar")        `hashWithSalt` nacl `hashWithSalt` hashIdx v
    Atuple t                    -> $(hashQ "Atuple")      `hashWithSalt` nacl `hashWithSalt` hashAtuple hashAcc t
    Aprj ix a                   -> $(hashQ "Aprj")        `hashWithSalt` nacl `hashWithSalt` hashTupleIdx ix    `hashA` a
    Apply f a                   -> $(hashQ "Apply")       `hashWithSalt` nacl `hashWithSalt` hashAfun hashAcc f `hashA` a
    Aforeign _ f a              -> $(hashQ "Aforeign")    `hashWithSalt` nacl `hashWithSalt` hashAfun hashAcc f `hashA` a
    Use a                       -> $(hashQ "Use")         `hashWithSalt` hashArrays (arrays (undefined::arrs)) a
    Awhile p f a                -> $(hashQ "Awhile")      `hashWithSalt` hashAfun hashAcc f `hashWithSalt` hashAfun hashAcc p `hashA` a
    Unit e                      -> $(hashQ "Unit")        `hashE` e
    Generate e f                -> $(hashQ "Generate")    `hashE` e  `hashF` f
    Acond e a1 a2               -> $(hashQ "Acond")       `hashE` e  `hashA` a1 `hashA` a2
    Reshape sh a                -> $(hashQ "Reshape")     `hashE` sh `hashA` a
    Transform sh f1 f2 a        -> $(hashQ "Transform")   `hashE` sh `hashF` f1 `hashF` f2 `hashA` a
    Replicate spec ix a         -> $(hashQ "Replicate")   `hashE` ix `hashA` a  `hashWithSalt` hashSliceIndex spec
    Slice spec a ix             -> $(hashQ "Slice")       `hashE` ix `hashA` a  `hashWithSalt` hashSliceIndex spec
    Map f a                     -> $(hashQ "Map")         `hashF` f  `hashA` a
    ZipWith f a1 a2             -> $(hashQ "ZipWith")     `hashF` f  `hashA` a1 `hashA` a2
    Fold f e a                  -> $(hashQ "Fold")        `hashF` f  `hashE` e  `hashA` a
    Fold1 f a                   -> $(hashQ "Fold1")       `hashF` f  `hashA` a
    FoldSeg f e a s             -> $(hashQ "FoldSeg")     `hashF` f  `hashE` e  `hashA` a  `hashA` s
    Fold1Seg f a s              -> $(hashQ "Fold1Seg")    `hashF` f  `hashA` a  `hashA` s
    Scanl f e a                 -> $(hashQ "Scanl")       `hashF` f  `hashE` e  `hashA` a
    Scanl' f e a                -> $(hashQ "Scanl'")      `hashF` f  `hashE` e  `hashA` a
    Scanl1 f a                  -> $(hashQ "Scanl1")      `hashF` f  `hashA` a
    Scanr f e a                 -> $(hashQ "Scanr")       `hashF` f  `hashE` e  `hashA` a
    Scanr' f e a                -> $(hashQ "Scanr'")      `hashF` f  `hashE` e  `hashA` a
    Scanr1 f a                  -> $(hashQ "Scanr1")      `hashF` f  `hashA` a
    Backpermute sh f a          -> $(hashQ "Backpermute") `hashF` f  `hashE` sh `hashA` a
    Permute f1 a1 f2 a2         -> $(hashQ "Permute")     `hashF` f1 `hashA` a1 `hashF` f2 `hashA` a2
    Stencil f b a               -> $(hashQ "Stencil")     `hashF` f  `hashB` b  `hashA` a
    Stencil2 f b1 a1 b2 a2      -> $(hashQ "Stencil2")    `hashF` f  `hashB` b1 `hashA` a1 `hashB` b2 `hashA` a2

{--
hashPreOpenSeq :: forall acc aenv senv arrs. HashAcc acc -> PreOpenSeq acc aenv senv arrs -> Int
hashPreOpenSeq hashAcc s =
  let
    hashA :: Int -> acc aenv' a -> Int
    hashA salt = hashWithSalt salt . hashAcc

    hashE :: Int -> PreOpenExp acc env' aenv' e -> Int
    hashE salt = hashWithSalt salt . hashPreOpenExp hashAcc

    hashAF :: Int -> PreOpenAfun acc aenv' f -> Int
    hashAF salt = hashWithSalt salt . hashAfun hashAcc

    hashF :: Int -> PreOpenFun acc env' aenv' f -> Int
    hashF salt = hashWithSalt salt . hashPreOpenFun hashAcc

    hashS :: Int -> PreOpenSeq acc aenv senv' arrs' -> Int
    hashS salt = hashWithSalt salt . hashPreOpenSeq hashAcc

    hashVar :: Int -> Idx senv' a -> Int
    hashVar salt = hashWithSalt salt . idxToInt

    hashP :: Int -> Producer acc aenv senv a -> Int
    hashP salt p =
      case p of
        StreamIn arrs       -> unsafePerformIO $! hashStableName `fmap` makeStableName arrs
        ToSeq spec _ acc    -> hashWithSalt salt "ToSeq"         `hashA`  acc `hashWithSalt` show spec
        MapSeq f x          -> hashWithSalt salt "MapSeq"        `hashAF` f   `hashVar` x
        ChunkedMapSeq f x   -> hashWithSalt salt "ChunkedMapSeq" `hashAF` f   `hashVar` x
        ZipWithSeq f x y    -> hashWithSalt salt "ZipWithSeq"    `hashAF` f   `hashVar` x `hashVar` y
        ScanSeq f e x       -> hashWithSalt salt "ScanSeq"       `hashF`  f   `hashE`   e `hashVar` x

    hashC :: Int -> Consumer acc aenv senv' a -> Int
    hashC salt c =
      case c of
        FoldSeq f e x          -> hashWithSalt salt "FoldSeq"        `hashF`  f `hashE` e   `hashVar` x
        FoldSeqFlatten f acc x -> hashWithSalt salt "FoldSeqFlatten" `hashAF` f `hashA` acc `hashVar` x
        Stuple t               -> hash "Stuple" `hashWithSalt` hashAtuple (hashC salt) t

  in case s of
    Producer   p s' -> hash "Producer"   `hashP` p `hashS` s'
    Consumer   c    -> hash "Consumer"   `hashC` c
    Reify      ix   -> hash "Reify"      `hashVar` ix
--}


hashIdx :: Idx env t -> Int
hashIdx = hash . idxToInt

hashTupleIdx :: TupleIdx tup e -> Int
hashTupleIdx = hash . tupleIdxToInt


hashArrays :: ArraysR a -> a -> Int
hashArrays ArraysRunit         ()       = hash ()
hashArrays (ArraysRpair r1 r2) (a1, a2) = hash (hashArrays r1 a1, hashArrays r2 a2)
hashArrays ArraysRarray        ad       = unsafePerformIO $! hashStableName `fmap` makeStableName ad

hashArraysType :: forall a. ArraysR a -> Int
hashArraysType ArraysRunit         = $(hashQ "ArraysRunit")
hashArraysType (ArraysRpair r1 r2) = $(hashQ "ArraysRpair")  `hashWithSalt` hashArraysType r1 `hashWithSalt` hashArraysType r2
hashArraysType ArraysRarray        = $(hashQ "ArraysRarray") `hashWithSalt` hashArrayType (undefined::a)
  where
    hashArrayType :: forall sh e. (Shape sh, Elt e) => Array sh e -> Int
    hashArrayType _ = hashTupleType (eltType (undefined::sh)) `hashWithSalt` hashTupleType (eltType (undefined::e))

hashAtuple :: HashAcc acc -> Atuple (acc aenv) a -> Int
hashAtuple _ NilAtup            = $(hashQ "NilAtup")
hashAtuple h (SnocAtup t a)     = $(hashQ "SnocAtup") `hashWithSalt` hashAtuple h t `hashWithSalt` h a

hashAfun :: forall acc aenv f. HashAcc acc -> PreOpenAfun acc aenv f -> Int
hashAfun hashAcc afun =
  let
    hashA :: forall aenv' a. Arrays a => Int -> acc aenv' a -> Int
    hashA salt
      = hashWithSalt salt
      . hashWithSalt (hashArraysType (arrays (undefined::a)))
      . hashAcc

    hashL :: forall aenv' a b. Arrays a => Int -> PreOpenAfun acc (aenv',a) b -> Int
    hashL salt
       = hashWithSalt salt
       . hashWithSalt (hashArraysType (arrays (undefined::a)))
       . hashAfun hashAcc

  in case afun of
    Abody b -> $(hashQ "Abody") `hashA` b
    Alam  l -> $(hashQ "Alam")  `hashL` l


hashPreBoundary :: forall acc aenv sh e. HashAcc acc -> PreBoundary acc aenv (Array sh e) -> Int
hashPreBoundary _ Wrap          = $(hashQ "Wrap")
hashPreBoundary _ Clamp         = $(hashQ "Clamp")
hashPreBoundary _ Mirror        = $(hashQ "Mirror")
hashPreBoundary _ (Constant c)  = $(hashQ "Constant") `hashWithSalt` hashConst (eltType (undefined::e)) c
hashPreBoundary h (Function f)  = $(hashQ "Function") `hashWithSalt` hashPreOpenFun h f

hashSliceIndex :: SliceIndex slix sl co sh -> Int
hashSliceIndex SliceNil         = $(hashQ "SliceNil")
hashSliceIndex (SliceAll r)     = $(hashQ "SliceAll")   `hashWithSalt` hashSliceIndex r
hashSliceIndex (SliceFixed r)   = $(hashQ "sliceFixed") `hashWithSalt` hashSliceIndex r


-- Scalar expressions
-- ------------------

hashOpenExp :: OpenExp env aenv exp -> Int
hashOpenExp = hashPreOpenExp hashOpenAcc

hashPreOpenExp :: forall acc env aenv exp. HashAcc acc -> PreOpenExp acc env aenv exp -> Int
hashPreOpenExp hashAcc exp =
  let
    hashE :: forall env' aenv' e. Elt e => Int -> PreOpenExp acc env' aenv' e -> Int
    hashE salt
      = hashWithSalt salt
      . hashWithSalt (hashTupleType (eltType (undefined::e)))
      . hashPreOpenExp hashAcc

    hashA :: Int -> acc aenv' a -> Int
    hashA salt = hashWithSalt salt . hashAcc

    hashF :: Int -> PreOpenFun acc env' aenv' f -> Int
    hashF salt = hashWithSalt salt . hashPreOpenFun hashAcc

    nacl :: Elt exp => Int
    nacl = hashTupleType (eltType (undefined::exp))

  in case exp of
    Let bnd body                -> $(hashQ "Let")         `hashE` bnd `hashE` body
    Const c                     -> $(hashQ "Const")       `hashWithSalt` hashConst (eltType (undefined::exp)) c
    Var ix                      -> $(hashQ "Var")         `hashWithSalt` nacl `hashWithSalt` hashIdx ix
    Tuple t                     -> $(hashQ "Tuple")       `hashWithSalt` nacl `hashWithSalt` hashTuple hashAcc t
    Prj i e                     -> $(hashQ "Prj")         `hashWithSalt` nacl `hashWithSalt` hashTupleIdx i `hashE` e
    IndexAny                    -> $(hashQ "IndexAny")    `hashWithSalt` nacl
    IndexNil                    -> $(hashQ "IndexNil")
    IndexCons sl a              -> $(hashQ "IndexCons")   `hashE` sl `hashE` a
    IndexHead sl                -> $(hashQ "IndexHead")   `hashE` sl
    IndexTail sl                -> $(hashQ "IndexTail")   `hashE` sl
    IndexSlice spec ix sh       -> $(hashQ "IndexSlice")  `hashE` ix `hashE` sh `hashWithSalt` hashSliceIndex spec
    IndexFull  spec ix sl       -> $(hashQ "IndexFull")   `hashE` ix `hashE` sl `hashWithSalt` hashSliceIndex spec
    ToIndex sh i                -> $(hashQ "ToIndex")     `hashE` sh `hashE` i
    FromIndex sh i              -> $(hashQ "FromIndex")   `hashE` sh `hashE` i
    Cond c t e                  -> $(hashQ "Cond")        `hashE` c  `hashE` t  `hashE` e
    While p f x                 -> $(hashQ "While")       `hashF` p  `hashF` f  `hashE` x
    PrimApp f x                 -> $(hashQ "PrimApp")     `hashWithSalt` hashPrimFun f `hashE` fromMaybe x (commutes hashAcc f x)
    PrimConst c                 -> $(hashQ "PrimConst")   `hashWithSalt` hashPrimConst c
    Index a ix                  -> $(hashQ "Index")       `hashA` a  `hashE` ix
    LinearIndex a ix            -> $(hashQ "LinearIndex") `hashA` a  `hashE` ix
    Shape a                     -> $(hashQ "Shape")       `hashA` a
    ShapeSize sh                -> $(hashQ "ShapeSize")   `hashE` sh
    Intersect sa sb             -> $(hashQ "Intersect")   `hashE` sa `hashE` sb
    Union sa sb                 -> $(hashQ "Union")       `hashE` sa `hashE` sb
    Foreign _ f e               -> $(hashQ "Foreign")     `hashF` f  `hashE` e


hashPreOpenFun :: forall acc env aenv f. HashAcc acc -> PreOpenFun acc env aenv f -> Int
hashPreOpenFun hashAcc fun =
  let
    hashE :: forall env' aenv' e. Elt e => Int -> PreOpenExp acc env' aenv' e -> Int
    hashE salt
      = hashWithSalt salt
      . hashWithSalt (hashTupleType (eltType (undefined::e)))
      . hashPreOpenExp hashAcc

    hashL :: forall env' aenv' a b. Elt a => Int -> PreOpenFun acc (env',a) aenv' b -> Int
    hashL salt
      = hashWithSalt salt
      . hashWithSalt (hashTupleType (eltType (undefined::a)))
      . hashPreOpenFun hashAcc

  in case fun of
    Body b -> $(hashQ "Body") `hashE` b
    Lam f  -> $(hashQ "Lam")  `hashL` f


hashTuple :: HashAcc acc -> Tuple (PreOpenExp acc env aenv) e -> Int
hashTuple _ NilTup              = $(hashQ "NilTup")
hashTuple h (SnocTup t e)       = $(hashQ "SnocTup") `hashWithSalt` hashTuple h t `hashWithSalt` hashPreOpenExp h e


hashConst :: TupleType t -> t -> Int
hashConst UnitTuple         ()    = hash ()
hashConst (PairTuple ta tb) (a,b) = hash (hashConst ta a, hashConst tb b)
hashConst (SingleTuple t)   c     = hashScalarConst t c

hashScalarConst :: ScalarType t -> t -> Int
hashScalarConst (NumScalarType t)    = hashNumConst t
hashScalarConst (NonNumScalarType t) = hashNonNumConst t

hashNonNumConst :: NonNumType t -> t -> Int
hashNonNumConst TypeBool{}   x          = $(hashQ "Bool")   `hashWithSalt` x
hashNonNumConst TypeChar{}   x          = $(hashQ "Char")   `hashWithSalt` x
hashNonNumConst TypeCChar{}  (CChar  x) = $(hashQ "CChar")  `hashWithSalt` x
hashNonNumConst TypeCSChar{} (CSChar x) = $(hashQ "CSChar") `hashWithSalt` x
hashNonNumConst TypeCUChar{} (CUChar x) = $(hashQ "CUChar") `hashWithSalt` x

hashNumConst :: NumType t -> t -> Int
hashNumConst (IntegralNumType t) = hashIntegralConst t
hashNumConst (FloatingNumType t) = hashFloatingConst t

hashIntegralConst :: IntegralType t -> t -> Int
hashIntegralConst TypeInt{}     x           = $(hashQ "Int")     `hashWithSalt` x
hashIntegralConst TypeInt8{}    x           = $(hashQ "Int8")    `hashWithSalt` x
hashIntegralConst TypeInt16{}   x           = $(hashQ "Int16")   `hashWithSalt` x
hashIntegralConst TypeInt32{}   x           = $(hashQ "Int32")   `hashWithSalt` x
hashIntegralConst TypeInt64{}   x           = $(hashQ "Int64")   `hashWithSalt` x
hashIntegralConst TypeWord{}    x           = $(hashQ "Word")    `hashWithSalt` x
hashIntegralConst TypeWord8{}   x           = $(hashQ "Word8")   `hashWithSalt` x
hashIntegralConst TypeWord16{}  x           = $(hashQ "Word16")  `hashWithSalt` x
hashIntegralConst TypeWord32{}  x           = $(hashQ "Word32")  `hashWithSalt` x
hashIntegralConst TypeWord64{}  x           = $(hashQ "Word64")  `hashWithSalt` x
hashIntegralConst TypeCShort{}  (CShort x)  = $(hashQ "CShort")  `hashWithSalt` x
hashIntegralConst TypeCUShort{} (CUShort x) = $(hashQ "CUShort") `hashWithSalt` x
hashIntegralConst TypeCInt{}    (CInt x)    = $(hashQ "CInt")    `hashWithSalt` x
hashIntegralConst TypeCUInt{}   (CUInt x)   = $(hashQ "CUInt")   `hashWithSalt` x
hashIntegralConst TypeCLong{}   (CLong x)   = $(hashQ "CLong")   `hashWithSalt` x
hashIntegralConst TypeCULong{}  (CULong x)  = $(hashQ "CULong")  `hashWithSalt` x
hashIntegralConst TypeCLLong{}  (CLLong x)  = $(hashQ "CLLong")  `hashWithSalt` x
hashIntegralConst TypeCULLong{} (CULLong x) = $(hashQ "CULLong") `hashWithSalt` x

hashFloatingConst :: FloatingType t -> t -> Int
hashFloatingConst TypeFloat{}   x           = $(hashQ "Float")   `hashWithSalt` x
hashFloatingConst TypeDouble{}  x           = $(hashQ "Double")  `hashWithSalt` x
hashFloatingConst TypeCFloat{}  (CFloat x)  = $(hashQ "CFloat")  `hashWithSalt` x
hashFloatingConst TypeCDouble{} (CDouble x) = $(hashQ "CDouble") `hashWithSalt` x

hashPrimConst :: PrimConst c -> Int
hashPrimConst (PrimMinBound t)  = $(hashQ "PrimMinBound") `hashWithSalt` hashBoundedType t
hashPrimConst (PrimMaxBound t)  = $(hashQ "PrimMaxBound") `hashWithSalt` hashBoundedType t
hashPrimConst (PrimPi t)        = $(hashQ "PrimPi")       `hashWithSalt` hashFloatingType t


hashPrimFun :: PrimFun f -> Int
hashPrimFun (PrimAdd a)                = $(hashQ "PrimAdd")                `hashWithSalt` hashNumType a
hashPrimFun (PrimSub a)                = $(hashQ "PrimSub")                `hashWithSalt` hashNumType a
hashPrimFun (PrimMul a)                = $(hashQ "PrimMul")                `hashWithSalt` hashNumType a
hashPrimFun (PrimNeg a)                = $(hashQ "PrimNeg")                `hashWithSalt` hashNumType a
hashPrimFun (PrimAbs a)                = $(hashQ "PrimAbs")                `hashWithSalt` hashNumType a
hashPrimFun (PrimSig a)                = $(hashQ "PrimSig")                `hashWithSalt` hashNumType a
hashPrimFun (PrimQuot a)               = $(hashQ "PrimQuot")               `hashWithSalt` hashIntegralType a
hashPrimFun (PrimRem a)                = $(hashQ "PrimRem")                `hashWithSalt` hashIntegralType a
hashPrimFun (PrimQuotRem a)            = $(hashQ "PrimQuotRem")            `hashWithSalt` hashIntegralType a
hashPrimFun (PrimIDiv a)               = $(hashQ "PrimIDiv")               `hashWithSalt` hashIntegralType a
hashPrimFun (PrimMod a)                = $(hashQ "PrimMod")                `hashWithSalt` hashIntegralType a
hashPrimFun (PrimDivMod a)             = $(hashQ "PrimDivMod")             `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBAnd a)               = $(hashQ "PrimBAnd")               `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBOr a)                = $(hashQ "PrimBOr")                `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBXor a)               = $(hashQ "PrimBXor")               `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBNot a)               = $(hashQ "PrimBNot")               `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBShiftL a)            = $(hashQ "PrimBShiftL")            `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBShiftR a)            = $(hashQ "PrimBShiftR")            `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBRotateL a)           = $(hashQ "PrimBRotateL")           `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBRotateR a)           = $(hashQ "PrimBRotateR")           `hashWithSalt` hashIntegralType a
hashPrimFun (PrimPopCount a)           = $(hashQ "PrimPopCount")           `hashWithSalt` hashIntegralType a
hashPrimFun (PrimCountLeadingZeros a)  = $(hashQ "PrimCountLeadingZeros")  `hashWithSalt` hashIntegralType a
hashPrimFun (PrimCountTrailingZeros a) = $(hashQ "PrimCountTrailingZeros") `hashWithSalt` hashIntegralType a
hashPrimFun (PrimFDiv a)               = $(hashQ "PrimFDiv")               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimRecip a)              = $(hashQ "PrimRecip")              `hashWithSalt` hashFloatingType a
hashPrimFun (PrimSin a)                = $(hashQ "PrimSin")                `hashWithSalt` hashFloatingType a
hashPrimFun (PrimCos a)                = $(hashQ "PrimCos")                `hashWithSalt` hashFloatingType a
hashPrimFun (PrimTan a)                = $(hashQ "PrimTan")                `hashWithSalt` hashFloatingType a
hashPrimFun (PrimAsin a)               = $(hashQ "PrimAsin")               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimAcos a)               = $(hashQ "PrimAcos")               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimAtan a)               = $(hashQ "PrimAtan")               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimSinh a)               = $(hashQ "PrimSinh")               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimCosh a)               = $(hashQ "PrimCosh")               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimTanh a)               = $(hashQ "PrimTanh")               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimAsinh a)              = $(hashQ "PrimAsinh")              `hashWithSalt` hashFloatingType a
hashPrimFun (PrimAcosh a)              = $(hashQ "PrimAcosh")              `hashWithSalt` hashFloatingType a
hashPrimFun (PrimAtanh a)              = $(hashQ "PrimAtanh")              `hashWithSalt` hashFloatingType a
hashPrimFun (PrimExpFloating a)        = $(hashQ "PrimExpFloating")        `hashWithSalt` hashFloatingType a
hashPrimFun (PrimSqrt a)               = $(hashQ "PrimSqrt")               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimLog a)                = $(hashQ "PrimLog")                `hashWithSalt` hashFloatingType a
hashPrimFun (PrimFPow a)               = $(hashQ "PrimFPow")               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimLogBase a)            = $(hashQ "PrimLogBase")            `hashWithSalt` hashFloatingType a
hashPrimFun (PrimAtan2 a)              = $(hashQ "PrimAtan2")              `hashWithSalt` hashFloatingType a
hashPrimFun (PrimTruncate a b)         = $(hashQ "PrimTruncate")           `hashWithSalt` hashFloatingType a `hashWithSalt` hashIntegralType b
hashPrimFun (PrimRound a b)            = $(hashQ "PrimRound")              `hashWithSalt` hashFloatingType a `hashWithSalt` hashIntegralType b
hashPrimFun (PrimFloor a b)            = $(hashQ "PrimFloor")              `hashWithSalt` hashFloatingType a `hashWithSalt` hashIntegralType b
hashPrimFun (PrimCeiling a b)          = $(hashQ "PrimCeiling")            `hashWithSalt` hashFloatingType a `hashWithSalt` hashIntegralType b
hashPrimFun (PrimIsNaN a)              = $(hashQ "PrimIsNaN")              `hashWithSalt` hashFloatingType a
hashPrimFun (PrimIsInfinite a)         = $(hashQ "PrimIsInfinite")         `hashWithSalt` hashFloatingType a
hashPrimFun (PrimLt a)                 = $(hashQ "PrimLt")                 `hashWithSalt` hashScalarType a
hashPrimFun (PrimGt a)                 = $(hashQ "PrimGt")                 `hashWithSalt` hashScalarType a
hashPrimFun (PrimLtEq a)               = $(hashQ "PrimLtEq")               `hashWithSalt` hashScalarType a
hashPrimFun (PrimGtEq a)               = $(hashQ "PrimGtEq")               `hashWithSalt` hashScalarType a
hashPrimFun (PrimEq a)                 = $(hashQ "PrimEq")                 `hashWithSalt` hashScalarType a
hashPrimFun (PrimNEq a)                = $(hashQ "PrimNEq")                `hashWithSalt` hashScalarType a
hashPrimFun (PrimMax a)                = $(hashQ "PrimMax")                `hashWithSalt` hashScalarType a
hashPrimFun (PrimMin a)                = $(hashQ "PrimMin")                `hashWithSalt` hashScalarType a
hashPrimFun (PrimFromIntegral a b)     = $(hashQ "PrimFromIntegral")       `hashWithSalt` hashIntegralType a `hashWithSalt` hashNumType b
hashPrimFun (PrimToFloating a b)       = $(hashQ "PrimToFloating")         `hashWithSalt` hashNumType a      `hashWithSalt` hashFloatingType b
hashPrimFun (PrimCoerce a b)           = $(hashQ "PrimCoerce")             `hashWithSalt` hashScalarType a   `hashWithSalt` hashScalarType b
hashPrimFun PrimLAnd                   = $(hashQ "PrimLAnd")
hashPrimFun PrimLOr                    = $(hashQ "PrimLOr")
hashPrimFun PrimLNot                   = $(hashQ "PrimLNot")
hashPrimFun PrimOrd                    = $(hashQ "PrimOrd")
hashPrimFun PrimChr                    = $(hashQ "PrimChr")
hashPrimFun PrimBoolToInt              = $(hashQ "PrimBoolToInt")


-- TLM: We need to include the depth of the branches in the pair case, otherwise
--      we are getting a collision at @hash t == hash (t,(t,t))@.
--
hashTupleType :: TupleType t -> Int
hashTupleType UnitTuple       = $(hashQ "UnitTuple")
hashTupleType (SingleTuple t) = $(hashQ "SingleTuple") `hashWithSalt` hashScalarType t
hashTupleType (PairTuple a b) = $(hashQ "PairTuple")   `hashWithSalt` hashTupleType a `hashWithSalt` depthTupleType a
                                                       `hashWithSalt` hashTupleType b `hashWithSalt` depthTupleType b

depthTupleType :: TupleType t -> Int
depthTupleType UnitTuple       = 0
depthTupleType SingleTuple{}   = 1
depthTupleType (PairTuple a b) = depthTupleType a + depthTupleType b

hashScalarType :: ScalarType t -> Int
hashScalarType (NumScalarType t)    = $(hashQ "NumScalarType")    `hashWithSalt` hashNumType t
hashScalarType (NonNumScalarType t) = $(hashQ "NonNumScalarType") `hashWithSalt` hashNonNumType t

hashBoundedType :: BoundedType t -> Int
hashBoundedType (IntegralBoundedType t) = $(hashQ "IntegralBoundedType") `hashWithSalt` hashIntegralType t
hashBoundedType (NonNumBoundedType t)   = $(hashQ "NonNumBoundedType")   `hashWithSalt` hashNonNumType t

hashNonNumType :: NonNumType t -> Int
hashNonNumType TypeBool{}   = $(hashQ "Bool")
hashNonNumType TypeChar{}   = $(hashQ "Char")
hashNonNumType TypeCChar{}  = $(hashQ "CChar")
hashNonNumType TypeCSChar{} = $(hashQ "CSChar")
hashNonNumType TypeCUChar{} = $(hashQ "CUChar")

hashNumType :: NumType t -> Int
hashNumType (IntegralNumType t) = $(hashQ "IntegralNumType") `hashWithSalt` hashIntegralType t
hashNumType (FloatingNumType t) = $(hashQ "FloatingNumType") `hashWithSalt` hashFloatingType t

hashIntegralType :: IntegralType t -> Int
hashIntegralType TypeInt{}     = $(hashQ "Int")
hashIntegralType TypeInt8{}    = $(hashQ "Int8")
hashIntegralType TypeInt16{}   = $(hashQ "Int16")
hashIntegralType TypeInt32{}   = $(hashQ "Int32")
hashIntegralType TypeInt64{}   = $(hashQ "Int64")
hashIntegralType TypeWord{}    = $(hashQ "Word")
hashIntegralType TypeWord8{}   = $(hashQ "Word8")
hashIntegralType TypeWord16{}  = $(hashQ "Word16")
hashIntegralType TypeWord32{}  = $(hashQ "Word32")
hashIntegralType TypeWord64{}  = $(hashQ "Word64")
hashIntegralType TypeCShort{}  = $(hashQ "CShort")
hashIntegralType TypeCUShort{} = $(hashQ "CUShort")
hashIntegralType TypeCInt{}    = $(hashQ "CInt")
hashIntegralType TypeCUInt{}   = $(hashQ "CUInt")
hashIntegralType TypeCLong{}   = $(hashQ "CLong")
hashIntegralType TypeCULong{}  = $(hashQ "CULong")
hashIntegralType TypeCLLong{}  = $(hashQ "CLLong")
hashIntegralType TypeCULLong{} = $(hashQ "CULLong")

hashFloatingType :: FloatingType t -> Int
hashFloatingType TypeFloat{}   = $(hashQ "Float")
hashFloatingType TypeDouble{}  = $(hashQ "Double")
hashFloatingType TypeCFloat{}  = $(hashQ "CFloat")
hashFloatingType TypeCDouble{} = $(hashQ "CDouble")


-- Auxiliary
-- ---------

-- Discriminate binary functions that commute, and if so return the operands in
-- a stable ordering such that matching recognises expressions modulo
-- commutativity.
--
commutes
    :: forall acc env aenv a r.
       HashAcc acc
    -> PrimFun (a -> r)
    -> PreOpenExp acc env aenv a
    -> Maybe (PreOpenExp acc env aenv a)
commutes h f x = case f of
  PrimAdd{}     -> Just (swizzle x)
  PrimMul{}     -> Just (swizzle x)
  PrimBAnd{}    -> Just (swizzle x)
  PrimBOr{}     -> Just (swizzle x)
  PrimBXor{}    -> Just (swizzle x)
  PrimEq{}      -> Just (swizzle x)
  PrimNEq{}     -> Just (swizzle x)
  PrimMax{}     -> Just (swizzle x)
  PrimMin{}     -> Just (swizzle x)
  PrimLAnd      -> Just (swizzle x)
  PrimLOr       -> Just (swizzle x)
  _             -> Nothing
  where
    swizzle :: PreOpenExp acc env aenv (a',a') -> PreOpenExp acc env aenv (a',a')
    swizzle exp
      | Tuple (NilTup `SnocTup` a `SnocTup` b)  <- exp
      , hashPreOpenExp h a > hashPreOpenExp h b = Tuple (NilTup `SnocTup` b `SnocTup` a)
      --
      | otherwise                               = exp

