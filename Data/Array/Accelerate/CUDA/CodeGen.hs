{-# LANGUAGE CPP, GADTs, PatternGuards, ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.CodeGen
  (
    CUTranslSkel,
    codeGenAcc, codeGenFun, codeGenExp
  )
  where

import Prelude hiding (mod)

import Data.Char
import Language.C
import Control.Applicative
import Control.Monad.State
import Text.PrettyPrint

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Pretty ()
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Analysis.Shape
import Data.Array.Accelerate.Array.Representation
import qualified Data.Array.Accelerate.AST                      as AST
import qualified Data.Array.Accelerate.Array.Sugar              as Sugar
import qualified Foreign.Storable                               as F

import Data.Array.Accelerate.CUDA.CodeGen.Data
import Data.Array.Accelerate.CUDA.CodeGen.Util
import Data.Array.Accelerate.CUDA.CodeGen.Skeleton

#include "accelerate.h"

type CG a = State [CExtDecl] a

-- Array expressions
-- ~~~~~~~~~~~~~~~~~

-- | Generate CUDA device code for an array expression
--
codeGenAcc :: AST.OpenAcc aenv a -> CUTranslSkel
codeGenAcc acc =
  let (CUTranslSkel code skel, fvar) = runState (codeGenAcc' acc) []
      CTranslUnit decl node          = code
  in
  CUTranslSkel (CTranslUnit (fvar ++ decl) node) skel

-- FIXME:
--  The actual code generation workhorse. We run under a state which keeps track
--  of the free array variables encountered so far. This is used to determine
--  which reference to texture read from. Assumes the same traversal behaviour
--  during execution.
--
--  FRAGILE.
--
--  TLM 2010-07-16:
--    Make AST.Acc an instance of Traversable, and use mapAccumL to collect the
--    types of free array variables ??
--
codeGenAcc' :: AST.OpenAcc aenv a -> CG CUTranslSkel
codeGenAcc' op@(AST.Replicate sl e1 a1) = codeGenReplicate sl e1 a1 op <* codeGenExp e1
codeGenAcc' op@(AST.Index sl a1 e1)     = codeGenIndex     sl a1 op e1 <* codeGenExp e1
codeGenAcc' (AST.Fold f1 e1 _)          = mkFold    (codeGenExpType e1) <$> codeGenExp e1   <*> codeGenFun f1
codeGenAcc' (AST.FoldSeg f1 e1 _ s)     = mkFoldSeg (codeGenExpType e1) (codeGenAccType s)  <$> codeGenExp e1 <*> codeGenFun f1
codeGenAcc' (AST.Scanl f1 e1 _)         = mkScanl   (codeGenExpType e1) <$> codeGenExp e1   <*> codeGenFun f1
codeGenAcc' (AST.Scanr f1 e1 _)         = mkScanr   (codeGenExpType e1) <$> codeGenExp e1   <*> codeGenFun f1
codeGenAcc' op@(AST.Map f1 a1)          = mkMap     (codeGenAccType op) (codeGenAccType a1) <$> codeGenFun f1
codeGenAcc' op@(AST.ZipWith f1 a1 a0)
  = mkZipWith (codeGenAccType op) (accDim op)
              (codeGenAccType a1) (accDim a1)
              (codeGenAccType a0) (accDim a0)
              <$> codeGenFun f1

codeGenAcc' op@(AST.Permute f1 _ f2 a1)
  = mkPermute (codeGenAccType a1) (accDim op) (accDim a1)
  <$> codeGenFun f1
  <*> codeGenFun f2

codeGenAcc' op@(AST.Backpermute _ f1 a1)
  = mkBackpermute (codeGenAccType a1) (accDim op) (accDim a1)
  <$> codeGenFun f1

codeGenAcc' x =
  INTERNAL_ERROR(error) "codeGenAcc"
  (unlines ["unsupported array primitive", render . nest 2 $ text (show x)])


-- Embedded expressions
-- ~~~~~~~~~~~~~~~~~~~~

-- Function abstraction
--
codeGenFun :: AST.OpenFun env aenv t -> CG [CExpr]
codeGenFun (AST.Lam  lam)  = codeGenFun lam
codeGenFun (AST.Body body) = codeGenExp body

unit :: a -> [a]
unit x = [x]

-- Implementation of 'IndexScalar' and 'Shape' demonstrate that array
-- computations must be hoisted out of scalar expressions before code generation
-- or execution: kernel functions can not invoke other array computations.
--
-- TLM 2010-06-24: Shape for free array variables??
--
codeGenExp :: forall env aenv t. AST.OpenExp env aenv t -> CG [CExpr]
codeGenExp (AST.Shape _)       = return . unit $ CVar (internalIdent "shape") internalNode
codeGenExp (AST.PrimConst c)   = return . unit $ codeGenPrimConst c
codeGenExp (AST.PrimApp f arg) = unit   . codeGenPrim f <$> codeGenExp arg
codeGenExp (AST.Const c)       = return $ codeGenConst (Sugar.elemType (undefined::t)) c
codeGenExp (AST.Tuple t)       = codeGenTup t
codeGenExp prj@(AST.Prj idx e)
  = reverse
  . take (length $ codeGenTupleType (expType prj))
  . drop (prjToInt idx (expType e))
  . reverse
  <$> codeGenExp e

codeGenExp (AST.Var i)         =
  let var = CVar (internalIdent ('x' : show (idxToInt i))) internalNode
  in case codeGenTupleType (Sugar.elemType (undefined::t)) of
          [_] -> return [var]
          cps -> return . reverse . take (length cps) . flip map (enumFrom 0 :: [Int]) $
            \c -> CMember var (internalIdent ('a':show c)) False internalNode

codeGenExp (AST.Cond p e1 e2) = do
  [a] <- codeGenExp p
  [b] <- codeGenExp e1
  [c] <- codeGenExp e2
  return [CCond a (Just b) c internalNode]

codeGenExp (AST.IndexScalar a1 e1) = do
  n   <- length <$> get
  [i] <- codeGenExp e1
  let ty = codeGenTupleTex (accType a1)
      fv = map (\x -> "tex" ++ show x) [n..]

  modify (++ zipWith globalDecl ty fv)
  return (zipWith (indexArray i) fv ty)

  where
    globalDecl ty name = CDeclExt (CDecl (map CTypeSpec ty) [(Just (CDeclr (Just (internalIdent name)) [] Nothing [] internalNode),Nothing,Nothing)] internalNode)
    indexArray idx name [CDoubleType _] = CCall (CVar (internalIdent "indexDArray") internalNode) [CVar (internalIdent name) internalNode, idx] internalNode
    indexArray idx name _               = CCall (CVar (internalIdent "indexArray")  internalNode) [CVar (internalIdent name) internalNode, idx] internalNode


-- Tuples are defined as snoc-lists, so generate code right-to-left
--
codeGenTup :: Tuple (AST.OpenExp env aenv) t -> CG [CExpr]
codeGenTup NilTup          = return []
codeGenTup (t `SnocTup` e) = (++) <$> codeGenTup t <*> codeGenExp e

-- Convert a typed de Brujin index to the corresponding integer
--
idxToInt :: AST.Idx env t -> Int
idxToInt AST.ZeroIdx       = 0
idxToInt (AST.SuccIdx idx) = 1 + idxToInt idx

-- Convert a tuple index into the corresponding integer. Since the internal
-- representation is flat, be sure to walk over all sub components when indexing
-- past nested tuples.
--
prjToInt :: TupleIdx t e -> TupleType a -> Int
prjToInt ZeroTupIdx     _                 = 0
prjToInt (SuccTupIdx i) (b `PairTuple` a) = length (codeGenTupleType a) + prjToInt i b
prjToInt _ _ =
  INTERNAL_ERROR(error) "prjToInt" "inconsistent valuation"


-- multidimensional array slice and index
--
codeGenIndex
  :: SliceIndex (Sugar.ElemRepr slix)
                (Sugar.ElemRepr sl)
                co
                (Sugar.ElemRepr dim)
  -> AST.OpenAcc aenv (Sugar.Array dim e)
  -> AST.OpenAcc aenv (Sugar.Array sl e)
  -> AST.Exp aenv slix
  -> CG CUTranslSkel
codeGenIndex sl acc acc' slix =
  return . mkIndex ty dimSl dimCo dimIn0 $ restrict sl (dimCo-1,dimSl-1)
  where
    ty     = codeGenAccType acc
    dimCo  = length (codeGenExpType slix)
    dimSl  = accDim acc'
    dimIn0 = accDim acc

    restrict :: SliceIndex slix sl co dim -> (Int,Int) -> [CExpr]
    restrict (SliceNil)            _     = []
    restrict (SliceAll   sliceIdx) (m,n) = mkPrj dimSl "sl" n : restrict sliceIdx (m,n-1)
    restrict (SliceFixed sliceIdx) (m,n) = mkPrj dimCo "co" m : restrict sliceIdx (m-1,n)


-- multidimensional replicate
--
codeGenReplicate
  :: SliceIndex (Sugar.ElemRepr slix)
                (Sugar.ElemRepr sl)
                co
                (Sugar.ElemRepr dim)
  -> AST.Exp aenv slix
  -> AST.OpenAcc aenv (Sugar.Array sl e)
  -> AST.OpenAcc aenv (Sugar.Array dim e)
  -> CG CUTranslSkel
codeGenReplicate sl _slix acc acc' =
  return . mkReplicate ty dimSl dimOut . post $ extend sl (dimOut-1)
  where
    ty     = codeGenAccType acc
    dimSl  = accDim acc
    dimOut = accDim acc'

    extend :: SliceIndex slix sl co dim -> Int -> [CExpr]
    extend (SliceNil)            _ = []
    extend (SliceAll   sliceIdx) n = mkPrj dimOut "dim" n : extend sliceIdx (n-1)
    extend (SliceFixed sliceIdx) n = extend sliceIdx (n-1)

    post [] = [CConst (CIntConst (cInteger 0) internalNode)]
    post xs = xs


mkPrj :: Int -> String -> Int -> CExpr
mkPrj ndim var c
 | ndim <= 1 = CVar (internalIdent var) internalNode
 | otherwise = CMember (CVar (internalIdent var) internalNode) (internalIdent ('a':show c)) False internalNode


-- Types
-- ~~~~~

-- Generate types for the reified elements of an array computation
--
codeGenAccType :: AST.OpenAcc aenv (Sugar.Array dim e) -> [CType]
codeGenAccType =  codeGenTupleType . accType

codeGenExpType :: AST.OpenExp aenv env t -> [CType]
codeGenExpType =  codeGenTupleType . expType


-- Implementation
--
codeGenTupleType :: TupleType a -> [CType]
codeGenTupleType UnitTuple         = []
codeGenTupleType (SingleTuple  ty) = [codeGenScalarType ty]
codeGenTupleType (PairTuple t1 t0) = codeGenTupleType t1 ++ codeGenTupleType t0

codeGenScalarType :: ScalarType a -> CType
codeGenScalarType (NumScalarType    ty) = codeGenNumType ty
codeGenScalarType (NonNumScalarType ty) = codeGenNonNumType ty

codeGenNumType :: NumType a -> CType
codeGenNumType (IntegralNumType ty) = codeGenIntegralType ty
codeGenNumType (FloatingNumType ty) = codeGenFloatingType ty

codeGenIntegralType :: IntegralType a -> CType
codeGenIntegralType (TypeInt8    _) = [CTypeDef (internalIdent "int8_t")   internalNode]
codeGenIntegralType (TypeInt16   _) = [CTypeDef (internalIdent "int16_t")  internalNode]
codeGenIntegralType (TypeInt32   _) = [CTypeDef (internalIdent "int32_t")  internalNode]
codeGenIntegralType (TypeInt64   _) = [CTypeDef (internalIdent "int64_t")  internalNode]
codeGenIntegralType (TypeWord8   _) = [CTypeDef (internalIdent "uint8_t")  internalNode]
codeGenIntegralType (TypeWord16  _) = [CTypeDef (internalIdent "uint16_t") internalNode]
codeGenIntegralType (TypeWord32  _) = [CTypeDef (internalIdent "uint32_t") internalNode]
codeGenIntegralType (TypeWord64  _) = [CTypeDef (internalIdent "uint64_t") internalNode]
codeGenIntegralType (TypeCShort  _) = [CShortType internalNode]
codeGenIntegralType (TypeCUShort _) = [CUnsigType internalNode, CShortType internalNode]
codeGenIntegralType (TypeCInt    _) = [CIntType   internalNode]
codeGenIntegralType (TypeCUInt   _) = [CUnsigType internalNode, CIntType internalNode]
codeGenIntegralType (TypeCLong   _) = [CLongType  internalNode, CIntType internalNode]
codeGenIntegralType (TypeCULong  _) = [CUnsigType internalNode, CLongType internalNode, CIntType internalNode]
codeGenIntegralType (TypeCLLong  _) = [CLongType  internalNode, CLongType internalNode, CIntType internalNode]
codeGenIntegralType (TypeCULLong _) = [CUnsigType internalNode, CLongType internalNode, CLongType internalNode, CIntType internalNode]

codeGenIntegralType (TypeInt     _) =
  case F.sizeOf (undefined::Int) of
       4 -> [CTypeDef (internalIdent "int32_t") internalNode]
       8 -> [CTypeDef (internalIdent "int64_t") internalNode]
       _ -> error "we can never get here"

codeGenIntegralType (TypeWord    _) =
  case F.sizeOf (undefined::Int) of
       4 -> [CTypeDef (internalIdent "uint32_t") internalNode]
       8 -> [CTypeDef (internalIdent "uint64_t") internalNode]
       _ -> error "we can never get here"

codeGenFloatingType :: FloatingType a -> CType
codeGenFloatingType (TypeFloat   _) = [CFloatType  internalNode]
codeGenFloatingType (TypeDouble  _) = [CDoubleType internalNode]
codeGenFloatingType (TypeCFloat  _) = [CFloatType  internalNode]
codeGenFloatingType (TypeCDouble _) = [CDoubleType internalNode]

codeGenNonNumType :: NonNumType a -> CType
codeGenNonNumType (TypeBool   _) = [CUnsigType internalNode, CCharType internalNode]
codeGenNonNumType (TypeChar   _) = [CCharType internalNode]
codeGenNonNumType (TypeCChar  _) = [CCharType internalNode]
codeGenNonNumType (TypeCSChar _) = [CSignedType internalNode, CCharType internalNode]
codeGenNonNumType (TypeCUChar _) = [CUnsigType  internalNode, CCharType internalNode]


-- Texture types
--
codeGenTupleTex :: TupleType a -> [CType]
codeGenTupleTex UnitTuple         = []
codeGenTupleTex (SingleTuple t)   = [codeGenScalarTex t]
codeGenTupleTex (PairTuple t1 t0) = codeGenTupleTex t1 ++ codeGenTupleTex t0

codeGenScalarTex :: ScalarType a -> CType
codeGenScalarTex (NumScalarType    ty) = codeGenNumTex ty
codeGenScalarTex (NonNumScalarType ty) = codeGenNonNumTex ty;

codeGenNumTex :: NumType a -> CType
codeGenNumTex (IntegralNumType ty) = codeGenIntegralTex ty
codeGenNumTex (FloatingNumType ty) = codeGenFloatingTex ty

codeGenIntegralTex :: IntegralType a -> CType
codeGenIntegralTex (TypeInt8    _) = [CTypeDef (internalIdent "TexInt8")    internalNode]
codeGenIntegralTex (TypeInt16   _) = [CTypeDef (internalIdent "TexInt16")   internalNode]
codeGenIntegralTex (TypeInt32   _) = [CTypeDef (internalIdent "TexInt32")   internalNode]
codeGenIntegralTex (TypeInt64   _) = [CTypeDef (internalIdent "TexInt64")   internalNode]
codeGenIntegralTex (TypeWord8   _) = [CTypeDef (internalIdent "TexWord8")   internalNode]
codeGenIntegralTex (TypeWord16  _) = [CTypeDef (internalIdent "TexWord16")  internalNode]
codeGenIntegralTex (TypeWord32  _) = [CTypeDef (internalIdent "TexWord32")  internalNode]
codeGenIntegralTex (TypeWord64  _) = [CTypeDef (internalIdent "TexWord64")  internalNode]
codeGenIntegralTex (TypeCShort  _) = [CTypeDef (internalIdent "TexCShort")  internalNode]
codeGenIntegralTex (TypeCUShort _) = [CTypeDef (internalIdent "TexCUShort") internalNode]
codeGenIntegralTex (TypeCInt    _) = [CTypeDef (internalIdent "TexCInt")    internalNode]
codeGenIntegralTex (TypeCUInt   _) = [CTypeDef (internalIdent "TexCUInt")   internalNode]
codeGenIntegralTex (TypeCLong   _) = [CTypeDef (internalIdent "TexCLong")   internalNode]
codeGenIntegralTex (TypeCULong  _) = [CTypeDef (internalIdent "TexCULong")  internalNode]
codeGenIntegralTex (TypeCLLong  _) = [CTypeDef (internalIdent "TexCLLong")  internalNode]
codeGenIntegralTex (TypeCULLong _) = [CTypeDef (internalIdent "TexCULLong") internalNode]

codeGenIntegralTex (TypeInt     _) =
  case F.sizeOf (undefined::Int) of
       4 -> [CTypeDef (internalIdent "TexInt32") internalNode]
       8 -> [CTypeDef (internalIdent "TexInt64") internalNode]
       _ -> error "we can never get here"

codeGenIntegralTex (TypeWord    _) =
  case F.sizeOf (undefined::Word) of
       4 -> [CTypeDef (internalIdent "TexWord32") internalNode]
       8 -> [CTypeDef (internalIdent "TexWord64") internalNode]
       _ -> error "we can never get here"

codeGenFloatingTex :: FloatingType a -> CType
codeGenFloatingTex (TypeFloat   _) = [CTypeDef (internalIdent "TexFloat")   internalNode]
codeGenFloatingTex (TypeCFloat  _) = [CTypeDef (internalIdent "TexCFloat")  internalNode]
codeGenFloatingTex (TypeDouble  _) = [CTypeDef (internalIdent "TexDouble")  internalNode]
codeGenFloatingTex (TypeCDouble _) = [CTypeDef (internalIdent "TexCDouble") internalNode]

-- TLM 2010-06-29:
--   Bool and Char can be implemented once the array types in
--   Data.Array.Accelerate.[CUDA.]Array.Data are made concrete.
--
codeGenNonNumTex :: NonNumType a -> CType
codeGenNonNumTex (TypeBool   _) = error "codeGenNonNumTex :: Bool"
codeGenNonNumTex (TypeChar   _) = error "codeGenNonNumTex :: Char"
codeGenNonNumTex (TypeCChar  _) = [CTypeDef (internalIdent "TexCChar")  internalNode]
codeGenNonNumTex (TypeCSChar _) = [CTypeDef (internalIdent "TexCSChar") internalNode]
codeGenNonNumTex (TypeCUChar _) = [CTypeDef (internalIdent "TexCUChar") internalNode]


-- Scalar Primitives
-- ~~~~~~~~~~~~~~~~~

codeGenPrimConst :: AST.PrimConst a -> CExpr
codeGenPrimConst (AST.PrimMinBound ty) = codeGenMinBound ty
codeGenPrimConst (AST.PrimMaxBound ty) = codeGenMaxBound ty
codeGenPrimConst (AST.PrimPi       ty) = codeGenPi ty

codeGenPrim :: AST.PrimFun p -> [CExpr] -> CExpr
codeGenPrim (AST.PrimAdd          _) [a,b] = CBinary CAddOp a b internalNode
codeGenPrim (AST.PrimSub          _) [a,b] = CBinary CSubOp a b internalNode
codeGenPrim (AST.PrimMul          _) [a,b] = CBinary CMulOp a b internalNode
codeGenPrim (AST.PrimNeg          _) [a]   = CUnary  CMinOp a   internalNode
codeGenPrim (AST.PrimAbs         ty) [a]   = codeGenAbs ty a
codeGenPrim (AST.PrimSig         ty) [a]   = codeGenSig ty a
codeGenPrim (AST.PrimQuot         _) [a,b] = CBinary CDivOp a b internalNode
codeGenPrim (AST.PrimRem          _) [a,b] = CBinary CRmdOp a b internalNode
codeGenPrim (AST.PrimIDiv         _) [a,b] = CCall (CVar (internalIdent "idiv") internalNode) [a,b] internalNode
codeGenPrim (AST.PrimMod          _) [a,b] = CCall (CVar (internalIdent "mod")  internalNode) [a,b] internalNode
codeGenPrim (AST.PrimBAnd         _) [a,b] = CBinary CAndOp a b internalNode
codeGenPrim (AST.PrimBOr          _) [a,b] = CBinary COrOp  a b internalNode
codeGenPrim (AST.PrimBXor         _) [a,b] = CBinary CXorOp a b internalNode
codeGenPrim (AST.PrimBNot         _) [a]   = CUnary  CCompOp a  internalNode
codeGenPrim (AST.PrimBShiftL      _) [a,b] = CBinary CShlOp a b internalNode
codeGenPrim (AST.PrimBShiftR      _) [a,b] = CBinary CShrOp a b internalNode
codeGenPrim (AST.PrimBRotateL     _) [a,b] = CCall (CVar (internalIdent "rotateL") internalNode) [a,b] internalNode
codeGenPrim (AST.PrimBRotateR     _) [a,b] = CCall (CVar (internalIdent "rotateR") internalNode) [a,b] internalNode
codeGenPrim (AST.PrimFDiv         _) [a,b] = CBinary CDivOp a b internalNode
codeGenPrim (AST.PrimRecip       ty) [a]   = codeGenRecip ty a
codeGenPrim (AST.PrimSin         ty) [a]   = ccall (FloatingNumType ty) "sin"   [a]
codeGenPrim (AST.PrimCos         ty) [a]   = ccall (FloatingNumType ty) "cos"   [a]
codeGenPrim (AST.PrimTan         ty) [a]   = ccall (FloatingNumType ty) "tan"   [a]
codeGenPrim (AST.PrimAsin        ty) [a]   = ccall (FloatingNumType ty) "asin"  [a]
codeGenPrim (AST.PrimAcos        ty) [a]   = ccall (FloatingNumType ty) "acos"  [a]
codeGenPrim (AST.PrimAtan        ty) [a]   = ccall (FloatingNumType ty) "atan"  [a]
codeGenPrim (AST.PrimAsinh       ty) [a]   = ccall (FloatingNumType ty) "asinh" [a]
codeGenPrim (AST.PrimAcosh       ty) [a]   = ccall (FloatingNumType ty) "acosh" [a]
codeGenPrim (AST.PrimAtanh       ty) [a]   = ccall (FloatingNumType ty) "atanh" [a]
codeGenPrim (AST.PrimExpFloating ty) [a]   = ccall (FloatingNumType ty) "exp"   [a]
codeGenPrim (AST.PrimSqrt        ty) [a]   = ccall (FloatingNumType ty) "sqrt"  [a]
codeGenPrim (AST.PrimLog         ty) [a]   = ccall (FloatingNumType ty) "log"   [a]
codeGenPrim (AST.PrimFPow        ty) [a,b] = ccall (FloatingNumType ty) "pow"   [a,b]
codeGenPrim (AST.PrimLogBase     ty) [a,b] = codeGenLogBase ty a b
codeGenPrim (AST.PrimAtan2       ty) [a,b] = ccall (FloatingNumType ty) "atan2" [a,b]
codeGenPrim (AST.PrimLt           _) [a,b] = CBinary CLeOp  a b internalNode
codeGenPrim (AST.PrimGt           _) [a,b] = CBinary CGrOp  a b internalNode
codeGenPrim (AST.PrimLtEq         _) [a,b] = CBinary CLeqOp a b internalNode
codeGenPrim (AST.PrimGtEq         _) [a,b] = CBinary CGeqOp a b internalNode
codeGenPrim (AST.PrimEq           _) [a,b] = CBinary CEqOp  a b internalNode
codeGenPrim (AST.PrimNEq          _) [a,b] = CBinary CNeqOp a b internalNode
codeGenPrim (AST.PrimMax         ty) [a,b] = codeGenMax ty a b
codeGenPrim (AST.PrimMin         ty) [a,b] = codeGenMin ty a b
codeGenPrim AST.PrimLAnd             [a,b] = CBinary CLndOp a b internalNode
codeGenPrim AST.PrimLOr              [a,b] = CBinary CLorOp a b internalNode
codeGenPrim AST.PrimLNot             [a]   = CUnary  CNegOp a   internalNode
codeGenPrim AST.PrimOrd              [a]   = CCast (CDecl [CTypeSpec (CIntType  internalNode)] [] internalNode) a internalNode
codeGenPrim AST.PrimChr              [a]   = CCast (CDecl [CTypeSpec (CCharType internalNode)] [] internalNode) a internalNode
codeGenPrim AST.PrimRoundFloatInt    [a]   = CCall (CVar (internalIdent "lroundf") internalNode) [a] internalNode -- TLM: (int) rintf(x) ??
codeGenPrim AST.PrimTruncFloatInt    [a]   = CCall (CVar (internalIdent "ltruncf") internalNode) [a] internalNode
codeGenPrim AST.PrimIntFloat         [a]   = CCast (CDecl [CTypeSpec (CFloatType internalNode)] [] internalNode) a internalNode -- TLM: __int2float_[rn,rz,ru,rd](a) ??
codeGenPrim AST.PrimBoolToInt        [a]   = CCast (CDecl [CTypeSpec (CIntType   internalNode)] [] internalNode) a internalNode

-- If the argument lists are not the correct length
codeGenPrim _ _ =
  INTERNAL_ERROR(error) "codeGenPrim" "inconsistent valuation"


-- Implementation
--
codeGenConst :: TupleType a -> a -> [CExpr]
codeGenConst UnitTuple           _      = []
codeGenConst (SingleTuple ty)    c      = [codeGenScalar ty c]
codeGenConst (PairTuple ty1 ty0) (cs,c) = codeGenConst ty1 cs ++ codeGenConst ty0 c

-- FIXME:
--  Language-c isn't pretty printing float constants with a trailing 'f', so as
--  per the C spec nvcc considers them to be double constants. This causes
--  warnings on pre-1.3 series devices, and unnecessary runtime conversion and
--  register pressure on later hardware. Work around this with an explicit type
--  cast. This is quite ugly and should be fixed, but appears to work for now.
--
codeGenScalar :: ScalarType a -> a -> CExpr
codeGenScalar (NumScalarType (IntegralNumType ty))
  | IntegralDict <- integralDict ty
  = CConst . flip CIntConst   internalNode . cInteger . fromIntegral
codeGenScalar (NumScalarType (FloatingNumType (TypeFloat _)))
  = flip (CCast (CDecl [CTypeSpec (CFloatType internalNode)] [] internalNode)) internalNode
  . CConst . flip CFloatConst internalNode . cFloat   . fromRational . toRational
codeGenScalar (NumScalarType (FloatingNumType (TypeDouble _)))
  = CConst . flip CFloatConst internalNode . cFloat   . fromRational . toRational
codeGenScalar (NumScalarType (FloatingNumType (TypeCFloat _)))
  = flip (CCast (CDecl [CTypeSpec (CFloatType internalNode)] [] internalNode)) internalNode
  . CConst . flip CFloatConst internalNode . cFloat   . fromRational . toRational
codeGenScalar (NumScalarType (FloatingNumType (TypeCDouble _)))
  = CConst . flip CFloatConst internalNode . cFloat   . fromRational . toRational
codeGenScalar (NonNumScalarType (TypeBool _))   = fromBool
codeGenScalar (NonNumScalarType (TypeChar _))   =
  CConst . flip CCharConst internalNode . cChar
codeGenScalar (NonNumScalarType (TypeCChar _))  =
  CConst . flip CCharConst internalNode . cChar . chr . fromIntegral
codeGenScalar (NonNumScalarType (TypeCUChar _)) =
  CConst . flip CCharConst internalNode . cChar . chr . fromIntegral
codeGenScalar (NonNumScalarType (TypeCSChar _)) =
  CConst . flip CCharConst internalNode . cChar . chr . fromIntegral


codeGenPi :: FloatingType a -> CExpr
codeGenPi ty | FloatingDict <- floatingDict ty
  = codeGenScalar (NumScalarType (FloatingNumType ty)) pi

codeGenMinBound :: BoundedType a -> CExpr
codeGenMinBound (IntegralBoundedType ty)
  | IntegralDict <- integralDict ty
  = codeGenScalar (NumScalarType (IntegralNumType ty)) minBound
codeGenMinBound (NonNumBoundedType   ty)
  | NonNumDict   <- nonNumDict   ty
  = codeGenScalar (NonNumScalarType ty) minBound

codeGenMaxBound :: BoundedType a -> CExpr
codeGenMaxBound (IntegralBoundedType ty)
  | IntegralDict <- integralDict ty
  = codeGenScalar (NumScalarType (IntegralNumType ty)) maxBound
codeGenMaxBound (NonNumBoundedType   ty)
  | NonNumDict   <- nonNumDict   ty
  = codeGenScalar (NonNumScalarType ty) maxBound


codeGenAbs :: NumType a -> CExpr -> CExpr
codeGenAbs ty@(IntegralNumType _) x = ccall ty "abs"  [x]
codeGenAbs ty@(FloatingNumType _) x = ccall ty "fabs" [x]

codeGenSig :: NumType a -> CExpr -> CExpr
codeGenSig ty@(IntegralNumType t) a
  | IntegralDict <- integralDict t
  = CCond (CBinary CGeqOp a (codeGenScalar (NumScalarType ty) 0) internalNode)
          (Just (codeGenScalar (NumScalarType ty) 1))
          (codeGenScalar (NumScalarType ty) 0)
          internalNode
codeGenSig ty@(FloatingNumType t) a
  | FloatingDict <- floatingDict t
  = CCond (CBinary CGeqOp a (codeGenScalar (NumScalarType ty) 0) internalNode)
          (Just (codeGenScalar (NumScalarType ty) 1))
          (codeGenScalar (NumScalarType ty) 0)
          internalNode

codeGenRecip :: FloatingType a -> CExpr -> CExpr
codeGenRecip ty x | FloatingDict <- floatingDict ty
  = CBinary CDivOp (codeGenScalar (NumScalarType (FloatingNumType ty)) 1) x internalNode

codeGenLogBase :: FloatingType a -> CExpr -> CExpr -> CExpr
codeGenLogBase ty x y = let a = ccall (FloatingNumType ty) "log" [x]
                            b = ccall (FloatingNumType ty) "log" [y]
                        in
                        CBinary CDivOp b a internalNode

codeGenMin :: ScalarType a -> CExpr -> CExpr -> CExpr
codeGenMin (NumScalarType ty@(IntegralNumType _)) a b = ccall ty "min"  [a,b]
codeGenMin (NumScalarType ty@(FloatingNumType _)) a b = ccall ty "fmin" [a,b]
codeGenMin (NonNumScalarType _)                   _ _ = undefined

codeGenMax :: ScalarType a -> CExpr -> CExpr -> CExpr
codeGenMax (NumScalarType ty@(IntegralNumType _)) a b = ccall ty "max"  [a,b]
codeGenMax (NumScalarType ty@(FloatingNumType _)) a b = ccall ty "fmax" [a,b]
codeGenMax (NonNumScalarType _)                   _ _ = undefined


-- Helper Functions
-- ~~~~~~~~~~~~~~~~

ccall :: NumType a -> String -> [CExpr] -> CExpr
ccall (IntegralNumType  _) fn args = CCall (CVar (internalIdent fn)                internalNode) args internalNode
ccall (FloatingNumType ty) fn args = CCall (CVar (internalIdent (fn `postfix` ty)) internalNode) args internalNode
  where
    postfix :: String -> FloatingType a -> String
    postfix x (TypeFloat   _) = x ++ "f"
    postfix x (TypeCFloat  _) = x ++ "f"
    postfix x _               = x

