{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  commutes,

) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar
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

    hashP :: (Arrays arrs, Hashable a) => a -> Int
    hashP = hashWithSalt (hashArraysType (arrays (undefined::arrs)))

  in case pacc of
    Alet bnd body               -> hashP "Alet"         `hashA` bnd `hashA` body
    Avar v                      -> hashP "Avar"         `hashWithSalt` hashIdx v
    Atuple t                    -> hashP "Atuple"       `hashWithSalt` hashAtuple hashAcc t
    Aprj ix a                   -> hashP "Aprj"         `hashWithSalt` hashTupleIdx ix    `hashA` a
    Apply f a                   -> hashP "Apply"        `hashWithSalt` hashAfun hashAcc f `hashA` a
    Aforeign _ f a              -> hashP "Aforeign"     `hashWithSalt` hashAfun hashAcc f `hashA` a
    Use a                       -> hashP "Use"          `hashWithSalt` hashArrays (arrays (undefined::arrs)) a
    Awhile p f a                -> hashP "Awhile"       `hashWithSalt` hashAfun hashAcc f `hashWithSalt` hashAfun hashAcc p `hashA` a
    Unit e                      -> hashP "Unit"         `hashE` e
    Generate e f                -> hashP "Generate"     `hashE` e  `hashF` f
    Acond e a1 a2               -> hashP "Acond"        `hashE` e  `hashA` a1 `hashA` a2
    Reshape sh a                -> hashP "Reshape"      `hashE` sh `hashA` a
    Transform sh f1 f2 a        -> hashP "Transform"    `hashE` sh `hashF` f1 `hashF` f2 `hashA` a
    Replicate spec ix a         -> hashP "Replicate"    `hashE` ix `hashA` a  `hashWithSalt` show spec
    Slice spec a ix             -> hashP "Slice"        `hashE` ix `hashA` a  `hashWithSalt` show spec
    Map f a                     -> hashP "Map"          `hashF` f  `hashA` a
    ZipWith f a1 a2             -> hashP "ZipWith"      `hashF` f  `hashA` a1 `hashA` a2
    Fold f e a                  -> hashP "Fold"         `hashF` f  `hashE` e  `hashA` a
    Fold1 f a                   -> hashP "Fold1"        `hashF` f  `hashA` a
    FoldSeg f e a s             -> hashP "FoldSeg"      `hashF` f  `hashE` e  `hashA` a  `hashA` s
    Fold1Seg f a s              -> hashP "Fold1Seg"     `hashF` f  `hashA` a  `hashA` s
    Scanl f e a                 -> hashP "Scanl"        `hashF` f  `hashE` e  `hashA` a
    Scanl' f e a                -> hashP "Scanl'"       `hashF` f  `hashE` e  `hashA` a
    Scanl1 f a                  -> hashP "Scanl1"       `hashF` f  `hashA` a
    Scanr f e a                 -> hashP "Scanr"        `hashF` f  `hashE` e  `hashA` a
    Scanr' f e a                -> hashP "Scanr'"       `hashF` f  `hashE` e  `hashA` a
    Scanr1 f a                  -> hashP "Scanr1"       `hashF` f  `hashA` a
    Backpermute sh f a          -> hashP "Backpermute"  `hashF` f  `hashE` sh `hashA` a
    Permute f1 a1 f2 a2         -> hashP "Permute"      `hashF` f1 `hashA` a1 `hashF` f2 `hashA` a2
    Stencil f b a               -> hashP "Stencil"      `hashF` f  `hashB` b  `hashA` a
    Stencil2 f b1 a1 b2 a2      -> hashP "Stencil2"     `hashF` f  `hashB` b1 `hashA` a1 `hashB` b2 `hashA` a2
    -- Collect s                   -> hashP "Seq"          `hashS` s

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
hashArrays (ArraysRpair r1 r2) (a1, a2) = hash ( hashArrays r1 a1, hashArrays r2 a2 )
hashArrays ArraysRarray        ad       = unsafePerformIO $! hashStableName `fmap` makeStableName ad

hashArraysType :: forall a. ArraysR a -> Int
hashArraysType ArraysRunit         = hash "ArraysRunit"
hashArraysType (ArraysRpair r1 r2) = hash "ArraysRpair"  `hashWithSalt` hashArraysType r1 `hashWithSalt` hashArraysType r2
hashArraysType ArraysRarray        = hash "ArraysRarray" `hashWithSalt` hashArrayType (undefined::a)
  where
    hashArrayType :: forall sh e. (Shape sh, Elt e) => Array sh e -> Int
    hashArrayType _ = hashTupleType (eltType (undefined::sh)) `hashWithSalt` hashTupleType (eltType (undefined::e))

hashAtuple :: HashAcc acc -> Atuple (acc aenv) a -> Int
hashAtuple _ NilAtup            = hash "NilAtup"
hashAtuple h (SnocAtup t a)     = hash "SnocAtup" `hashWithSalt` hashAtuple h t `hashWithSalt` h a

hashAfun :: HashAcc acc -> PreOpenAfun acc aenv f -> Int
hashAfun h (Abody b)            = hash "Abody" `hashWithSalt` h b
hashAfun h (Alam f)             = hash "Alam"  `hashWithSalt` hashAfun h f

hashPreBoundary :: forall acc aenv sh e. HashAcc acc -> PreBoundary acc aenv (Array sh e) -> Int
hashPreBoundary _ Wrap          = hash "Wrap"
hashPreBoundary _ Clamp         = hash "Clamp"
hashPreBoundary _ Mirror        = hash "Mirror"
hashPreBoundary _ (Constant c)  = hash "Constant" `hashWithSalt` hashConst (eltType (undefined::e)) c
hashPreBoundary h (Function f)  = hash "Function" `hashWithSalt` hashPreOpenFun h f


-- Scalar expressions
-- ------------------

hashOpenExp :: OpenExp env aenv exp -> Int
hashOpenExp = hashPreOpenExp hashOpenAcc

hashPreOpenExp :: forall acc env aenv exp. HashAcc acc -> PreOpenExp acc env aenv exp -> Int
hashPreOpenExp hashAcc exp =
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

    hashP :: (Elt exp, Hashable a) => a -> Int
    hashP = hashWithSalt (hashTupleType (eltType (undefined::exp)))

  in case exp of
    Let bnd body                -> hashP "Let"          `hashE` bnd `hashE` body
    Var ix                      -> hashP "Var"          `hashWithSalt` hashIdx ix
    Const c                     -> hashP "Const"        `hashWithSalt` hashConst (eltType (undefined::exp)) c
    Tuple t                     -> hashP "Tuple"        `hashWithSalt` hashTuple hashAcc t
    Prj i e                     -> hashP "Prj"          `hashWithSalt` hashTupleIdx i `hashE` e
    IndexAny                    -> hashP "IndexAny"
    IndexNil                    -> hashP "IndexNil"
    IndexCons sl a              -> hashP "IndexCons"    `hashE` sl `hashE` a
    IndexHead sl                -> hashP "IndexHead"    `hashE` sl
    IndexTail sl                -> hashP "IndexTail"    `hashE` sl
    IndexSlice spec ix sh       -> hashP "IndexSlice"   `hashE` ix `hashE` sh `hashWithSalt` show spec
    IndexFull  spec ix sl       -> hashP "IndexFull"    `hashE` ix `hashE` sl `hashWithSalt` show spec
    ToIndex sh i                -> hashP "ToIndex"      `hashE` sh `hashE` i
    FromIndex sh i              -> hashP "FromIndex"    `hashE` sh `hashE` i
    Cond c t e                  -> hashP "Cond"         `hashE` c  `hashE` t  `hashE` e
    While p f x                 -> hashP "While"        `hashF` p  `hashF` f  `hashE` x
    PrimApp f x                 -> hashP "PrimApp"      `hashWithSalt` hashPrimFun f `hashE` fromMaybe x (commutes hashAcc f x)
    PrimConst c                 -> hashP "PrimConst"    `hashWithSalt` hashPrimConst c
    Index a ix                  -> hashP "Index"        `hashA` a  `hashE` ix
    LinearIndex a ix            -> hashP "LinearIndex"  `hashA` a  `hashE` ix
    Shape a                     -> hashP "Shape"        `hashA` a
    ShapeSize sh                -> hashP "ShapeSize"    `hashE` sh
    Intersect sa sb             -> hashP "Intersect"    `hashE` sa `hashE` sb
    Union sa sb                 -> hashP "Union"        `hashE` sa `hashE` sb
    Foreign _ f e               -> hashP "Foreign"      `hashF` f  `hashE` e


hashPreOpenFun :: HashAcc acc -> PreOpenFun acc env aenv f -> Int
hashPreOpenFun h (Body e)       = hash "Body"           `hashWithSalt` hashPreOpenExp h e
hashPreOpenFun h (Lam f)        = hash "Lam"            `hashWithSalt` hashPreOpenFun h f `hashWithSalt` hashArgType f
  where
    hashArgType :: forall acc env aenv a b. Elt a => PreOpenFun acc (env,a) aenv b -> Int
    hashArgType _ = hashTupleType (eltType (undefined::a))

hashTuple :: HashAcc acc -> Tuple (PreOpenExp acc env aenv) e -> Int
hashTuple _ NilTup              = hash "NilTup"
hashTuple h (SnocTup t e)       = hash "SnocTup"        `hashWithSalt` hashTuple h t `hashWithSalt` hashPreOpenExp h e


hashConst :: TupleType t -> t -> Int
hashConst UnitTuple         ()    = hash "()"
hashConst (SingleTuple t)   c     = hashScalarConst t c
hashConst (PairTuple ta tb) (a,b) = hash (hashConst ta a, hashConst tb b)

hashScalarConst :: ScalarType t -> t -> Int
hashScalarConst (NumScalarType t)    = hashNumConst t
hashScalarConst (NonNumScalarType t) = hashNonNumConst t

hashNonNumConst :: NonNumType t -> t -> Int
hashNonNumConst TypeBool{}   x          = hash "Bool"   `hashWithSalt` x
hashNonNumConst TypeChar{}   x          = hash "Char"   `hashWithSalt` x
hashNonNumConst TypeCChar{}  (CChar  x) = hash "CChar"  `hashWithSalt` x
hashNonNumConst TypeCSChar{} (CSChar x) = hash "CSChar" `hashWithSalt` x
hashNonNumConst TypeCUChar{} (CUChar x) = hash "CUChar" `hashWithSalt` x

hashNumConst :: NumType t -> t -> Int
hashNumConst (IntegralNumType t) = hashIntegralConst t
hashNumConst (FloatingNumType t) = hashFloatingConst t

hashIntegralConst :: IntegralType t -> t -> Int
hashIntegralConst TypeInt{}     x           = hash "Int"     `hashWithSalt` x
hashIntegralConst TypeInt8{}    x           = hash "Int8"    `hashWithSalt` x
hashIntegralConst TypeInt16{}   x           = hash "Int16"   `hashWithSalt` x
hashIntegralConst TypeInt32{}   x           = hash "Int32"   `hashWithSalt` x
hashIntegralConst TypeInt64{}   x           = hash "Int64"   `hashWithSalt` x
hashIntegralConst TypeWord{}    x           = hash "Word"    `hashWithSalt` x
hashIntegralConst TypeWord8{}   x           = hash "Word8"   `hashWithSalt` x
hashIntegralConst TypeWord16{}  x           = hash "Word16"  `hashWithSalt` x
hashIntegralConst TypeWord32{}  x           = hash "Word32"  `hashWithSalt` x
hashIntegralConst TypeWord64{}  x           = hash "Word64"  `hashWithSalt` x
hashIntegralConst TypeCShort{}  (CShort x)  = hash "CShort"  `hashWithSalt` x
hashIntegralConst TypeCUShort{} (CUShort x) = hash "CUShort" `hashWithSalt` x
hashIntegralConst TypeCInt{}    (CInt x)    = hash "CInt"    `hashWithSalt` x
hashIntegralConst TypeCUInt{}   (CUInt x)   = hash "CUInt"   `hashWithSalt` x
hashIntegralConst TypeCLong{}   (CLong x)   = hash "CLong"   `hashWithSalt` x
hashIntegralConst TypeCULong{}  (CULong x)  = hash "CULong"  `hashWithSalt` x
hashIntegralConst TypeCLLong{}  (CLLong x)  = hash "CLLong"  `hashWithSalt` x
hashIntegralConst TypeCULLong{} (CULLong x) = hash "CULLong" `hashWithSalt` x

hashFloatingConst :: FloatingType t -> t -> Int
hashFloatingConst TypeFloat{}   x           = hash "Float"   `hashWithSalt` x
hashFloatingConst TypeDouble{}  x           = hash "Double"  `hashWithSalt` x
hashFloatingConst TypeCFloat{}  (CFloat x)  = hash "CFloat"  `hashWithSalt` x
hashFloatingConst TypeCDouble{} (CDouble x) = hash "CDouble" `hashWithSalt` x


hashPrimConst :: PrimConst c -> Int
hashPrimConst (PrimMinBound t)  = hash "PrimMinBound" `hashWithSalt` hashBoundedType t
hashPrimConst (PrimMaxBound t)  = hash "PrimMaxBound" `hashWithSalt` hashBoundedType t
hashPrimConst (PrimPi t)        = hash "PrimPi"       `hashWithSalt` hashFloatingType t


hashPrimFun :: PrimFun f -> Int
hashPrimFun (PrimAdd a)                = hash "PrimAdd"                `hashWithSalt` hashNumType a
hashPrimFun (PrimSub a)                = hash "PrimSub"                `hashWithSalt` hashNumType a
hashPrimFun (PrimMul a)                = hash "PrimMul"                `hashWithSalt` hashNumType a
hashPrimFun (PrimNeg a)                = hash "PrimNeg"                `hashWithSalt` hashNumType a
hashPrimFun (PrimAbs a)                = hash "PrimAbs"                `hashWithSalt` hashNumType a
hashPrimFun (PrimSig a)                = hash "PrimSig"                `hashWithSalt` hashNumType a
hashPrimFun (PrimQuot a)               = hash "PrimQuot"               `hashWithSalt` hashIntegralType a
hashPrimFun (PrimRem a)                = hash "PrimRem"                `hashWithSalt` hashIntegralType a
hashPrimFun (PrimQuotRem a)            = hash "PrimQuotRem"            `hashWithSalt` hashIntegralType a
hashPrimFun (PrimIDiv a)               = hash "PrimIDiv"               `hashWithSalt` hashIntegralType a
hashPrimFun (PrimMod a)                = hash "PrimMod"                `hashWithSalt` hashIntegralType a
hashPrimFun (PrimDivMod a)             = hash "PrimDivMod"             `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBAnd a)               = hash "PrimBAnd"               `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBOr a)                = hash "PrimBOr"                `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBXor a)               = hash "PrimBXor"               `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBNot a)               = hash "PrimBNot"               `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBShiftL a)            = hash "PrimBShiftL"            `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBShiftR a)            = hash "PrimBShiftR"            `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBRotateL a)           = hash "PrimBRotateL"           `hashWithSalt` hashIntegralType a
hashPrimFun (PrimBRotateR a)           = hash "PrimBRotateR"           `hashWithSalt` hashIntegralType a
hashPrimFun (PrimPopCount a)           = hash "PrimPopCount"           `hashWithSalt` hashIntegralType a
hashPrimFun (PrimCountLeadingZeros a)  = hash "PrimCountLeadingZeros"  `hashWithSalt` hashIntegralType a
hashPrimFun (PrimCountTrailingZeros a) = hash "PrimCountTrailingZeros" `hashWithSalt` hashIntegralType a
hashPrimFun (PrimFDiv a)               = hash "PrimFDiv"               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimRecip a)              = hash "PrimRecip"              `hashWithSalt` hashFloatingType a
hashPrimFun (PrimSin a)                = hash "PrimSin"                `hashWithSalt` hashFloatingType a
hashPrimFun (PrimCos a)                = hash "PrimCos"                `hashWithSalt` hashFloatingType a
hashPrimFun (PrimTan a)                = hash "PrimTan"                `hashWithSalt` hashFloatingType a
hashPrimFun (PrimAsin a)               = hash "PrimAsin"               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimAcos a)               = hash "PrimAcos"               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimAtan a)               = hash "PrimAtan"               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimSinh a)               = hash "PrimSinh"               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimCosh a)               = hash "PrimCosh"               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimTanh a)               = hash "PrimTanh"               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimAsinh a)              = hash "PrimAsinh"              `hashWithSalt` hashFloatingType a
hashPrimFun (PrimAcosh a)              = hash "PrimAcosh"              `hashWithSalt` hashFloatingType a
hashPrimFun (PrimAtanh a)              = hash "PrimAtanh"              `hashWithSalt` hashFloatingType a
hashPrimFun (PrimExpFloating a)        = hash "PrimExpFloating"        `hashWithSalt` hashFloatingType a
hashPrimFun (PrimSqrt a)               = hash "PrimSqrt"               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimLog a)                = hash "PrimLog"                `hashWithSalt` hashFloatingType a
hashPrimFun (PrimFPow a)               = hash "PrimFPow"               `hashWithSalt` hashFloatingType a
hashPrimFun (PrimLogBase a)            = hash "PrimLogBase"            `hashWithSalt` hashFloatingType a
hashPrimFun (PrimAtan2 a)              = hash "PrimAtan2"              `hashWithSalt` hashFloatingType a
hashPrimFun (PrimTruncate a b)         = hash "PrimTruncate"           `hashWithSalt` hashFloatingType a `hashWithSalt` hashIntegralType b
hashPrimFun (PrimRound a b)            = hash "PrimRound"              `hashWithSalt` hashFloatingType a `hashWithSalt` hashIntegralType b
hashPrimFun (PrimFloor a b)            = hash "PrimFloor"              `hashWithSalt` hashFloatingType a `hashWithSalt` hashIntegralType b
hashPrimFun (PrimCeiling a b)          = hash "PrimCeiling"            `hashWithSalt` hashFloatingType a `hashWithSalt` hashIntegralType b
hashPrimFun (PrimIsNaN a)              = hash "PrimIsNaN"              `hashWithSalt` hashFloatingType a
hashPrimFun (PrimIsInfinite a)         = hash "PrimIsInfinite"         `hashWithSalt` hashFloatingType a
hashPrimFun (PrimLt a)                 = hash "PrimLt"                 `hashWithSalt` hashScalarType a
hashPrimFun (PrimGt a)                 = hash "PrimGt"                 `hashWithSalt` hashScalarType a
hashPrimFun (PrimLtEq a)               = hash "PrimLtEq"               `hashWithSalt` hashScalarType a
hashPrimFun (PrimGtEq a)               = hash "PrimGtEq"               `hashWithSalt` hashScalarType a
hashPrimFun (PrimEq a)                 = hash "PrimEq"                 `hashWithSalt` hashScalarType a
hashPrimFun (PrimNEq a)                = hash "PrimNEq"                `hashWithSalt` hashScalarType a
hashPrimFun (PrimMax a)                = hash "PrimMax"                `hashWithSalt` hashScalarType a
hashPrimFun (PrimMin a)                = hash "PrimMin"                `hashWithSalt` hashScalarType a
hashPrimFun (PrimFromIntegral a b)     = hash "PrimFromIntegral"       `hashWithSalt` hashIntegralType a `hashWithSalt` hashNumType b
hashPrimFun (PrimToFloating a b)       = hash "PrimToFloating"         `hashWithSalt` hashNumType a      `hashWithSalt` hashFloatingType b
hashPrimFun (PrimCoerce a b)           = hash "PrimCoerce"             `hashWithSalt` hashScalarType a   `hashWithSalt` hashScalarType b
hashPrimFun PrimLAnd                   = hash "PrimLAnd"
hashPrimFun PrimLOr                    = hash "PrimLOr"
hashPrimFun PrimLNot                   = hash "PrimLNot"
hashPrimFun PrimOrd                    = hash "PrimOrd"
hashPrimFun PrimChr                    = hash "PrimChr"
hashPrimFun PrimBoolToInt              = hash "PrimBoolToInt"


-- TLM: We need to include the depth of the branches in the pair case, otherwise
--      we are getting a collision at @hash t == hash (t,(t,t))@.
--
hashTupleType :: TupleType t -> Int
hashTupleType UnitTuple       = hash "UnitTuple"
hashTupleType (SingleTuple t) = hash "SingleTuple" `hashWithSalt` hashScalarType t
hashTupleType (PairTuple a b) = hash "PairTuple"   `hashWithSalt` hashTupleType a `hashWithSalt` depthTupleType a
                                                   `hashWithSalt` hashTupleType b `hashWithSalt` depthTupleType b

depthTupleType :: TupleType t -> Int
depthTupleType UnitTuple       = 0
depthTupleType SingleTuple{}   = 1
depthTupleType (PairTuple a b) = depthTupleType a + depthTupleType b

hashScalarType :: ScalarType t -> Int
hashScalarType (NumScalarType t)    = hash "NumScalarType"    `hashWithSalt` hashNumType t
hashScalarType (NonNumScalarType t) = hash "NonNumScalarType" `hashWithSalt` hashNonNumType t

hashBoundedType :: BoundedType t -> Int
hashBoundedType (IntegralBoundedType t) = hash "IntegralBoundedType" `hashWithSalt` hashIntegralType t
hashBoundedType (NonNumBoundedType t)   = hash "NonNumBoundedType"   `hashWithSalt` hashNonNumType t

hashNonNumType :: NonNumType t -> Int
hashNonNumType TypeBool{}   = hash "Bool"
hashNonNumType TypeChar{}   = hash "Char"
hashNonNumType TypeCChar{}  = hash "CChar"
hashNonNumType TypeCSChar{} = hash "CSChar"
hashNonNumType TypeCUChar{} = hash "CUChar"

hashNumType :: NumType t -> Int
hashNumType (IntegralNumType t) = hash "IntegralNumType" `hashWithSalt` hashIntegralType t
hashNumType (FloatingNumType t) = hash "FloatingNumType" `hashWithSalt` hashFloatingType t

hashIntegralType :: IntegralType t -> Int
hashIntegralType TypeInt{}     = hash "Int"
hashIntegralType TypeInt8{}    = hash "Int8"
hashIntegralType TypeInt16{}   = hash "Int16"
hashIntegralType TypeInt32{}   = hash "Int32"
hashIntegralType TypeInt64{}   = hash "Int64"
hashIntegralType TypeWord{}    = hash "Word"
hashIntegralType TypeWord8{}   = hash "Word8"
hashIntegralType TypeWord16{}  = hash "Word16"
hashIntegralType TypeWord32{}  = hash "Word32"
hashIntegralType TypeWord64{}  = hash "Word64"
hashIntegralType TypeCShort{}  = hash "CShort"
hashIntegralType TypeCUShort{} = hash "CUShort"
hashIntegralType TypeCInt{}    = hash "CInt"
hashIntegralType TypeCUInt{}   = hash "CUInt"
hashIntegralType TypeCLong{}   = hash "CLong"
hashIntegralType TypeCULong{}  = hash "CULong"
hashIntegralType TypeCLLong{}  = hash "CLLong"
hashIntegralType TypeCULLong{} = hash "CULLong"

hashFloatingType :: FloatingType t -> Int
hashFloatingType TypeFloat{}   = hash "Float"
hashFloatingType TypeDouble{}  = hash "Double"
hashFloatingType TypeCFloat{}  = hash "CFloat"
hashFloatingType TypeCDouble{} = hash "CDouble"


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

