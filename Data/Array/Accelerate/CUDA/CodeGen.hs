{-# LANGUAGE GADTs, PatternGuards #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen
-- Copyright   : [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.CodeGen (codeGenAcc)
  where

import Prelude hiding (mod)

import Data.Char
import Language.C
import Control.Applicative
import Control.Monad.State

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Pretty ()
import Data.Array.Accelerate.Analysis.Type
import qualified Data.Array.Accelerate.AST                      as AST
import qualified Data.Array.Accelerate.Array.Sugar              as Sugar

import Data.Array.Accelerate.CUDA.CodeGen.Util
import Data.Array.Accelerate.CUDA.CodeGen.Skeleton

type CType = [CTypeSpec]
type CG a  = State [CType] a


-- Array expressions
-- ~~~~~~~~~~~~~~~~~

-- | Generate CUDA device code for an array expression
--
codeGenAcc :: AST.OpenAcc aenv a -> (CTranslUnit, [CType])
codeGenAcc acc = runState (codeGenAcc' acc) []

-- FIXME
--  The actual code generation workhorse. We run under a state which keeps track
--  of the types of the free array variables encountered so far. This is used to
--  direct the final stages of code generation, and to determine which reference
--  to texture read from. Assumes the same traversal behaviour during execution.
--
--  FRAGILE.
--
codeGenAcc' :: AST.OpenAcc aenv a -> CG CTranslUnit
codeGenAcc' op@(AST.Map f1 a1)        = mkMap         "map"         (codeGenAccType op) (codeGenAccType a1) <$> codeGenFun f1
codeGenAcc' op@(AST.ZipWith f1 a1 a2) = mkZipWith     "zipWith"     (codeGenAccType op) (codeGenAccType a1) (codeGenAccType a2) <$> codeGenFun f1
codeGenAcc' (AST.Replicate _ e1 a1)   = mkReplicate   "replicate"   (codeGenAccType a1) <$> codeGenExp e1
codeGenAcc' (AST.Index _ a1 e1)       = mkIndex       "index"       (codeGenAccType a1) <$> codeGenExp e1
codeGenAcc' (AST.Fold  f1 e1 _)       = mkFold        "fold"        (codeGenExpType e1) <$> codeGenExp e1 <*> codeGenFun f1
codeGenAcc' (AST.FoldSeg f1 e1 _ _)   = mkFoldSeg     "foldSeg"     (codeGenExpType e1) <$> codeGenExp e1 <*> codeGenFun f1
codeGenAcc' (AST.Scanl f1 e1 _)       = mkScanl       "scan"        (codeGenExpType e1) <$> codeGenExp e1 <*> codeGenFun f1
codeGenAcc' (AST.Scanr f1 e1 _)       = mkScanr       "scan"        (codeGenExpType e1) <$> codeGenExp e1 <*> codeGenFun f1
codeGenAcc' (AST.Permute f1 _ f2 a1)  = mkPermute     "permute"     (codeGenAccType a1) <$> codeGenFun f1 <*> codeGenFun f2
codeGenAcc' (AST.Backpermute _ f1 a1) = mkBackpermute "backpermute" (codeGenAccType a1) <$> codeGenFun f1

codeGenAcc' _ =
  error "codeGenAcc: internal error"


-- Embedded expressions
-- ~~~~~~~~~~~~~~~~~~~~

-- Function abstraction
--
codeGenFun :: AST.OpenFun env aenv t -> CG CExpr
codeGenFun (AST.Lam  lam)  = codeGenFun lam
codeGenFun (AST.Body body) = codeGenExp body


-- Implementation of 'IndexScalar' and 'Shape' demonstrate that array
-- computations must be hoisted out of scalar expressions before code generation
-- or execution: kernel functions can not invoke other array computations.
--
codeGenExp :: forall env aenv t. AST.OpenExp env aenv t -> CG CExpr
codeGenExp (AST.Var i)       = return $ CVar (internalIdent ('x' : show (idxToInt i))) internalNode
codeGenExp (AST.PrimConst c) = return $ codeGenPrimConst c
codeGenExp (AST.Const c)     = return $
  codeGenConst (Sugar.elemType' (undefined::t)) (Sugar.fromElem' (Sugar.toElem c :: t))

codeGenExp (AST.Cond p e1 e2) =
  (\a b c -> CCond a (Just b) c internalNode) <$> codeGenExp p <*> codeGenExp e1 <*> codeGenExp e2

codeGenExp (AST.PrimApp f (AST.Tuple arg))
  | NilTup `SnocTup` x `SnocTup` y <- arg = codeGenPrim f <$> sequence [codeGenExp x, codeGenExp y]
codeGenExp (AST.PrimApp f x)              = codeGenPrim f <$> sequence [codeGenExp x]

codeGenExp (AST.IndexScalar a1 e1) = do
  n <- length <$> get <* modify (++ [codeGenAccType a1])        -- TLM: tuple types?
  a <- codeGenExp e1
  return $ CCall (CVar (internalIdent "tex1Dfetch") internalNode)
                 [CVar (internalIdent ("tex" ++ show n)) internalNode, a] internalNode
  -- TLM 2010-06-07:
  --  * named arrays (environment projection)
  --  * bounds checking

codeGenExp (AST.Tuple _) = error "codeGen AST.Tuple"
codeGenExp (AST.Prj _ _) = error "codeGen AST.Prj"
codeGenExp (AST.Shape _) = error "codeGen AST.Shape"
  -- TLM 2010-06-08:
  --  * generalise `length` kernel parameter to a `shape` type ??


-- Convert a typed de Brujin index to the corresponding integer
--
idxToInt :: AST.Idx env t -> Int
idxToInt AST.ZeroIdx       = 0
idxToInt (AST.SuccIdx idx) = 1 + idxToInt idx

-- Types
-- ~~~~~

-- Generate types for the reified elements of an array computation
--
codeGenAccType :: AST.OpenAcc aenv (Sugar.Array dim e) -> CType
codeGenAccType =  codeGenTupleType . accType

codeGenAccType2 :: AST.OpenAcc aenv (Sugar.Array dim1 e1, Sugar.Array dim2 e2) -> (CType, CType)
codeGenAccType2 (AST.Scanl _ e acc) = (codeGenAccType acc, codeGenExpType e)
codeGenAccType2 (AST.Scanr _ e acc) = (codeGenAccType acc, codeGenExpType e)

codeGenExpType :: AST.OpenExp aenv env t -> CType
codeGenExpType =  codeGenTupleType . expType


-- Implementation
--
codeGenTupleType :: TupleType a -> CType
codeGenTupleType (UnitTuple)              = undefined
codeGenTupleType (SingleTuple         ty) = codeGenScalarType ty
codeGenTupleType (PairTuple UnitTuple ty) = codeGenTupleType  ty
codeGenTupleType (PairTuple _ _)          = undefined

codeGenScalarType :: ScalarType a -> CType
codeGenScalarType (NumScalarType    ty) = codeGenNumType ty
codeGenScalarType (NonNumScalarType ty) = codeGenNonNumType ty

codeGenNumType :: NumType a -> CType
codeGenNumType (IntegralNumType ty) = codeGenIntegralType ty
codeGenNumType (FloatingNumType ty) = codeGenFloatingType ty

codeGenIntegralType :: IntegralType a -> CType
codeGenIntegralType (TypeInt     _) = [CIntType   internalNode]
codeGenIntegralType (TypeInt8    _) = [CCharType  internalNode]
codeGenIntegralType (TypeInt16   _) = [CShortType internalNode]
codeGenIntegralType (TypeInt32   _) = [CIntType   internalNode]
codeGenIntegralType (TypeInt64   _) = [CLongType  internalNode, CLongType internalNode, CIntType internalNode]
codeGenIntegralType (TypeWord    _) = [CUnsigType internalNode, CIntType internalNode]
codeGenIntegralType (TypeWord8   _) = [CUnsigType internalNode, CCharType  internalNode]
codeGenIntegralType (TypeWord16  _) = [CUnsigType internalNode, CShortType internalNode]
codeGenIntegralType (TypeWord32  _) = [CUnsigType internalNode, CIntType   internalNode]
codeGenIntegralType (TypeWord64  _) = [CUnsigType internalNode, CLongType  internalNode, CLongType internalNode, CIntType internalNode]
codeGenIntegralType (TypeCShort  _) = [CShortType internalNode]
codeGenIntegralType (TypeCUShort _) = [CUnsigType internalNode, CShortType internalNode]
codeGenIntegralType (TypeCInt    _) = [CIntType   internalNode]
codeGenIntegralType (TypeCUInt   _) = [CUnsigType internalNode, CIntType internalNode]
codeGenIntegralType (TypeCLong   _) = [CLongType  internalNode, CIntType internalNode]
codeGenIntegralType (TypeCULong  _) = [CUnsigType internalNode, CLongType internalNode, CIntType internalNode]
codeGenIntegralType (TypeCLLong  _) = [CLongType  internalNode, CLongType internalNode, CIntType internalNode]
codeGenIntegralType (TypeCULLong _) = [CUnsigType internalNode, CLongType internalNode, CLongType internalNode, CIntType internalNode]

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
codeGenPrim (AST.PrimQuot        ty) [a,b] = codeGenQuot ty a b
codeGenPrim (AST.PrimRem          _) [a,b] = CBinary CRmdOp a b internalNode
codeGenPrim (AST.PrimIDiv         _) [a,b] = CBinary CDivOp a b internalNode
codeGenPrim (AST.PrimMod         ty) [a,b] = codeGenMod ty a b
codeGenPrim (AST.PrimBAnd         _) [a,b] = CBinary CAndOp a b internalNode
codeGenPrim (AST.PrimBOr          _) [a,b] = CBinary COrOp  a b internalNode
codeGenPrim (AST.PrimBXor         _) [a,b] = CBinary CXorOp a b internalNode
codeGenPrim (AST.PrimBNot         _) [a]   = CUnary  CCompOp a  internalNode
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
  error "Data.Array.Accelerate.CUDA: inconsistent valuation"


-- Implementation
--

-- Need to use an ElemRepr' representation here, so that the SingleTuple
-- type matches the type of the actual constant.
--
codeGenConst :: TupleType a -> a -> CExpr
codeGenConst UnitTuple        _ = undefined             -- void* ??
codeGenConst (SingleTuple ty) c = codeGenScalar ty c
codeGenConst (PairTuple  _ _) _ = undefined


codeGenScalar :: ScalarType a -> a -> CExpr
codeGenScalar (NumScalarType (IntegralNumType ty))
  | IntegralDict <- integralDict ty
  = CConst . flip CIntConst   internalNode . cInteger . fromIntegral
codeGenScalar (NumScalarType (FloatingNumType ty))
  | FloatingDict <- floatingDict ty
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

codeGenQuot :: IntegralType a -> CExpr -> CExpr -> CExpr
codeGenQuot = error "Data.Array.Accelerate.CUDA.CodeGen: PrimQuot"

codeGenMod :: IntegralType a -> CExpr -> CExpr -> CExpr
codeGenMod = error "Data.Array.Accelerate.CUDA.CodeGen: PrimMod"

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

