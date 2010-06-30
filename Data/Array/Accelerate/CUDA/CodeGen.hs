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

module Data.Array.Accelerate.CUDA.CodeGen (codeGenAcc, CUTranslSkel)
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
import qualified Foreign.Storable                               as F

import Data.Array.Accelerate.CUDA.CodeGen.Util
import Data.Array.Accelerate.CUDA.CodeGen.Skeleton


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
type CG a = State [CExtDecl] a

codeGenAcc' :: AST.OpenAcc aenv a -> CG CUTranslSkel
codeGenAcc' op@(AST.Map f1 a1)        = mkMap         (codeGenAccType op) (codeGenAccType a1) <$> codeGenFun f1
codeGenAcc' op@(AST.ZipWith f1 a1 a2) = mkZipWith     (codeGenAccType op) (codeGenAccType a1) (codeGenAccType a2) <$> codeGenFun f1
codeGenAcc' (AST.Replicate _ e1 a1)   = mkReplicate   (codeGenAccType a1) <$> codeGenExp e1
codeGenAcc' (AST.Index _ a1 e1)       = mkIndex       (codeGenAccType a1) <$> codeGenExp e1
codeGenAcc' (AST.Fold  f1 e1 _)       = mkFold        (codeGenExpType e1) <$> codeGenExp e1 <*> codeGenFun f1
codeGenAcc' (AST.FoldSeg f1 e1 _ _)   = mkFoldSeg     (codeGenExpType e1) <$> codeGenExp e1 <*> codeGenFun f1
codeGenAcc' (AST.Scanl f1 e1 _)       = mkScanl       (codeGenExpType e1) <$> codeGenExp e1 <*> codeGenFun f1
codeGenAcc' (AST.Scanr f1 e1 _)       = mkScanr       (codeGenExpType e1) <$> codeGenExp e1 <*> codeGenFun f1
codeGenAcc' (AST.Permute f1 _ f2 a1)  = mkPermute     (codeGenAccType a1) <$> codeGenFun f1 <*> codeGenFun f2
codeGenAcc' (AST.Backpermute _ f1 a1) = mkBackpermute (codeGenAccType a1) <$> codeGenFun f1

codeGenAcc' _ =
  error "codeGenAcc: internal error"


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
codeGenExp (AST.Var i)         = return . unit $ CVar (internalIdent ('x' : show (idxToInt i))) internalNode
codeGenExp (AST.Shape _)       = return . unit $ CVar (internalIdent "shape") internalNode
codeGenExp (AST.PrimConst c)   = return . unit $ codeGenPrimConst c
codeGenExp (AST.PrimApp f arg) = unit   . codeGenPrim f <$> codeGenExp arg
codeGenExp (AST.Const c)       = return $ codeGenConst (Sugar.elemType (undefined::t)) c

codeGenExp (AST.Cond p e1 e2) = do
  [a] <- codeGenExp p
  [b] <- codeGenExp e1
  [c] <- codeGenExp e2
  return [CCond a (Just b) c internalNode]

codeGenExp (AST.IndexScalar a1 e1) = do
  n   <- length <$> get
  [i] <- codeGenExp e1
  let ty = codeGenTupleTex (accType a1)
      fv = zipWith (\_ x -> "tex" ++ show x) ty [n..]

  modify (++ zipWith globalDecl ty fv)
  return . flip map fv $ \a -> CCall (CVar (internalIdent "indexArray") internalNode)
                                     [CVar (internalIdent a) internalNode, i] internalNode

codeGenExp (AST.Tuple t)   = codeGenTup t
codeGenExp (AST.Prj idx e) = do
  [var] <- codeGenExp e
  return [CMember var (internalIdent [enumFrom 'a' !! prjToInt idx]) False internalNode]


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

-- Convert a tuple index into the corresponding integer
--
prjToInt :: TupleIdx t e -> Int
prjToInt ZeroTupIdx       = 0
prjToInt (SuccTupIdx idx) = 1 + prjToInt idx


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
codeGenPrim (AST.PrimQuot        ty) [a,b] = codeGenQuot ty a b
codeGenPrim (AST.PrimRem          _) [a,b] = CBinary CRmdOp a b internalNode
codeGenPrim (AST.PrimIDiv         _) [a,b] = CBinary CDivOp a b internalNode
codeGenPrim (AST.PrimMod         ty) [a,b] = codeGenMod ty a b
codeGenPrim (AST.PrimBAnd         _) [a,b] = CBinary CAndOp a b internalNode
codeGenPrim (AST.PrimBOr          _) [a,b] = CBinary COrOp  a b internalNode
codeGenPrim (AST.PrimBXor         _) [a,b] = CBinary CXorOp a b internalNode
codeGenPrim (AST.PrimBNot         _) [a]   = CUnary  CCompOp a  internalNode
codeGenPrim (AST.PrimBShiftL      _) [a,b] = CBinary CShlOp a b internalNode
codeGenPrim (AST.PrimBShiftR      _) [a,b] = CBinary CShrOp a b internalNode
codeGenPrim (AST.PrimBRotateL     _) [a,b] = codeGenBRotateL a b
codeGenPrim (AST.PrimBRotateR     _) [a,b] = codeGenBRotateR a b
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
codeGenConst :: TupleType a -> a -> [CExpr]
codeGenConst UnitTuple           _      = []
codeGenConst (SingleTuple ty)    c      = [codeGenScalar ty c]
codeGenConst (PairTuple ty1 ty0) (cs,c) = codeGenConst ty1 cs ++ codeGenConst ty0 c


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

-- TLM 2010-06-29:
--   It would be nice we could use something like Language.C.Parser.execParser
--   to suck in the C code directly, instead of storing this long and messy
--   abstract syntax. The problem lies in injecting our `x' and `i' expressions.
--
-- T rotl(T x, int i)
-- {
--   return (i &= 8 * sizeof(x) - 1) == 0 ? x : x << i | x >> 8 * sizeof(x) - i;
-- }
--
codeGenBRotateL :: CExpr -> CExpr -> CExpr
codeGenBRotateL x i =
  CCond (CBinary CEqOp (CAssign CAndAssOp i (CBinary CSubOp (CBinary CMulOp (CConst (CIntConst (cInteger 8) internalNode)) (CSizeofExpr x internalNode) internalNode) (CConst (CIntConst (cInteger 1) internalNode)) internalNode) internalNode) (CConst (CIntConst (cInteger 0) internalNode)) internalNode) (Just x) (CBinary COrOp (CBinary CShlOp x i internalNode) (CBinary CShrOp x (CBinary CSubOp (CBinary CMulOp (CConst (CIntConst (cInteger 8) internalNode)) (CSizeofExpr x internalNode) internalNode) i internalNode) internalNode) internalNode) internalNode

-- T rotr(T x, int i)
-- {
--   return (i &= 8 * sizeof(x) - 1) == 0 ? x : x >> i | x << 8 * sizeof(x) - i;
-- }
--
codeGenBRotateR :: CExpr -> CExpr -> CExpr
codeGenBRotateR x i =
  CCond (CBinary CEqOp (CAssign CAndAssOp i (CBinary CSubOp (CBinary CMulOp (CConst (CIntConst (cInteger 8) internalNode)) (CSizeofExpr x internalNode) internalNode) (CConst (CIntConst (cInteger 1) internalNode)) internalNode) internalNode) (CConst (CIntConst (cInteger 0) internalNode)) internalNode) (Just x) (CBinary COrOp (CBinary CShrOp x i internalNode) (CBinary CShlOp x (CBinary CSubOp (CBinary CMulOp (CConst (CIntConst (cInteger 8) internalNode)) (CSizeofExpr x internalNode) internalNode) i internalNode) internalNode) internalNode) internalNode

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

globalDecl :: CType -> String -> CExtDecl
globalDecl ty name =
  CDeclExt (CDecl (map CTypeSpec ty) [(Just (CDeclr (Just (internalIdent name)) [] Nothing [] internalNode),Nothing,Nothing)] internalNode)


ccall :: NumType a -> String -> [CExpr] -> CExpr
ccall (IntegralNumType  _) fn args = CCall (CVar (internalIdent fn)                internalNode) args internalNode
ccall (FloatingNumType ty) fn args = CCall (CVar (internalIdent (fn `postfix` ty)) internalNode) args internalNode
  where
    postfix :: String -> FloatingType a -> String
    postfix x (TypeFloat   _) = x ++ "f"
    postfix x (TypeCFloat  _) = x ++ "f"
    postfix x _               = x

