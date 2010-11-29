{-# LANGUAGE CPP, GADTs, PatternGuards, ScopedTypeVariables, TemplateHaskell #-}
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
    runCodeGen, codeGenAcc, codeGenFun, codeGenExp
  )
  where

import Prelude hiding (id, (.))
import Control.Category

import Data.Record.Label
import Data.Char
import Language.C
import Control.Applicative                                      hiding (Const)
import Control.Monad.State.Strict
import Text.PrettyPrint

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Pretty ()
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Analysis.Shape
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.AST hiding (arrays)
import qualified Data.Array.Accelerate.Array.Sugar              as Sugar
import qualified Foreign.Storable                               as F

import Data.Array.Accelerate.CUDA.CodeGen.Data
import Data.Array.Accelerate.CUDA.CodeGen.Util
import Data.Array.Accelerate.CUDA.CodeGen.Skeleton

#include "accelerate.h"


-- Array expressions
-- -----------------

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { _arrays :: [CExtDecl]
  , _shapes :: [CExtDecl]
  }

$(mkLabels [''CodeGenState])


-- | Instantiate an array computation with a set of concrete function and type
-- definitions to fix the parameters of an algorithmic skeleton. The generated
-- code can then be pretty-printed to file, and compiled to object code
-- executable on the device.
--
codeGenAcc :: OpenAcc aenv a -> CUTranslSkel
codeGenAcc acc =
  let (CUTranslSkel code defs skel, st) = runCodeGen (codeGen acc)
      (CTranslUnit decl node)           = code
      fvars                             = getL arrays st ++ getL shapes st
  in
  CUTranslSkel (CTranslUnit (fvars ++ decl) node) defs skel

runCodeGen :: CodeGen a -> (a,CodeGenState)
runCodeGen = flip runState (CodeGenState [] [])


-- The code generator, which needs to track any array references from scalar
-- code, to produce the appropriate binding hooks. Such references must be
-- lifted out in depth-first order.
--
codeGen :: OpenAcc aenv a -> CodeGen CUTranslSkel
codeGen a@(Generate _ f)      = mkGenerate (codeGenAccTypeDim a) <$> codeGenFun f
codeGen (Fold f e _)          = mkFold (codeGenExpType e) <$> codeGenExp e <*> codeGenFun f
codeGen (FoldSeg f e _ s)     = mkFoldSeg (codeGenExpType e) (codeGenAccType s) <$> codeGenExp e <*> codeGenFun f
codeGen (Scanl f e _)         = mkScanl  (codeGenExpType e) <$> codeGenExp e <*> codeGenFun f
codeGen (Scanr f e _)         = mkScanr  (codeGenExpType e) <$> codeGenExp e <*> codeGenFun f
codeGen (Scanl' f e _)        = mkScanl' (codeGenExpType e) <$> codeGenExp e <*> codeGenFun f
codeGen (Scanr' f e _)        = mkScanr' (codeGenExpType e) <$> codeGenExp e <*> codeGenFun f
codeGen (Scanl1 f a)          = mkScanl1 (codeGenAccType a) <$> codeGenFun f
codeGen (Scanr1 f a)          = mkScanr1 (codeGenAccType a) <$> codeGenFun f
codeGen b@(Map f a)           = mkMap (codeGenAccType b) (codeGenAccType a) <$> codeGenFun f
codeGen c@(ZipWith f a b)     = mkZipWith (codeGenAccTypeDim c) (codeGenAccTypeDim a) (codeGenAccTypeDim b) <$> codeGenFun f
codeGen b@(Permute f _ g a)   = mkPermute (codeGenAccType a) (accDim b) (accDim a) <$> codeGenFun f <*> codeGenFun g
codeGen b@(Backpermute _ f a) = mkBackpermute (codeGenAccType a) (accDim b) (accDim a) <$> codeGenFun f
codeGen b@(Replicate sl _ a)  = return . mkReplicate (codeGenAccType a) dimSl dimOut . reverse $ extend sl 0
  where
    dimSl  = accDim a
    dimOut = accDim b

    extend :: SliceIndex slix sl co dim -> Int -> [CExpr]
    extend (SliceNil)            _ = []
    extend (SliceAll   sliceIdx) n = mkPrj dimOut "dim" n : extend sliceIdx (n+1)
    extend (SliceFixed sliceIdx) n = extend sliceIdx (n+1)

codeGen b@(Index sl a slix)   = return . mkIndex (codeGenAccType a) dimSl dimCo dimIn0 . reverse $ restrict sl (0,0)
  where
    dimCo  = length (codeGenExpType slix)
    dimSl  = accDim b
    dimIn0 = accDim a

    restrict :: SliceIndex slix sl co dim -> (Int,Int) -> [CExpr]
    restrict (SliceNil)            _     = []
    restrict (SliceAll   sliceIdx) (m,n) = mkPrj dimSl "sl" n : restrict sliceIdx (m,n+1)
    restrict (SliceFixed sliceIdx) (m,n) = mkPrj dimCo "co" m : restrict sliceIdx (m+1,n)

codeGen (Stencil _ _ _)      = error "codeGenAcc: 'stencil' is not supported by the CUDA backend yet"
codeGen (Stencil2 _ _ _ _ _) = error "codeGenAcc: 'stencil2' is not supported by the CUDA backend yet"

-- We should never get here: Use, Let, Let2, Avar, Unit, Reshape
--
codeGen x =
  INTERNAL_ERROR(error) "codeGenAcc"
  (unlines ["unsupported array primitive", render (nest 2 doc)])
  where
    acc = show x
    doc | length acc <= 250 = text acc
        | otherwise         = text (take 250 acc) <+> text "... {truncated}"


mkPrj :: Int -> String -> Int -> CExpr
mkPrj ndim var c
 | ndim <= 1 = cvar var
 | otherwise = CMember (cvar var) (internalIdent ('a':show c)) False internalNode


-- Scalar Expressions
-- ------------------

-- Function abstraction
--
-- Although Accelerate includes lambda abstractions, it does not include a
-- general application form. That is, lambda abstractions of scalar expressions
-- are only introduced as arguments to collective operations, so lambdas are
-- always outermost, and can always be translated into plain C functions.
--
codeGenFun :: OpenFun env aenv t -> CodeGen [CExpr]
codeGenFun (Lam  lam)  = codeGenFun lam
codeGenFun (Body body) = codeGenExp body


-- Embedded scalar computations
--
-- The state is used here to track array expressions that have been hoisted out
-- of the scalar computation; namely, the arguments to 'IndexScalar' and 'Shape'
--
codeGenExp :: forall env aenv t. OpenExp env aenv t -> CodeGen [CExpr]
codeGenExp (PrimConst c)   = return . return $ codeGenPrimConst c
codeGenExp (PrimApp f arg) = return . codeGenPrim f <$> codeGenExp arg
codeGenExp (Const c)       = return $ codeGenConst (Sugar.eltType (undefined::t)) c
codeGenExp (Tuple t)       = codeGenTup t
codeGenExp p@(Prj idx e)
  = reverse
  . take (length $ codeGenTupleType (expType p))
  . drop (prjToInt idx (expType e))
  . reverse
  <$> codeGenExp e

codeGenExp IndexNil         = return []
codeGenExp (IndexCons ix i) =
  let snoc xs x = xs ++ x
  in  snoc <$> codeGenExp ix <*> codeGenExp i

codeGenExp (IndexHead ix)  = return . last <$> codeGenExp ix
codeGenExp (IndexTail ix)  =          init <$> codeGenExp ix

codeGenExp (Var i) =
  let var = cvar ('x' : show (idxToInt i))
  in
  case codeGenTupleType (Sugar.eltType (undefined::t)) of
       [_] -> return [var]
       cps -> return . reverse . take (length cps) . flip map (enumFrom 0 :: [Int]) $
         \c -> CMember var (internalIdent ('a':show c)) False internalNode

codeGenExp (Cond p t e) =
  zipWith . (\[a] b c -> CCond a (Just b) c internalNode) <$> codeGenExp p <*> codeGenExp t <*> codeGenExp e

codeGenExp (Shape a) = do
  sh <- ("sh"++) . show . length <$> getM shapes
  modM shapes (mkShape (accDim a) sh :)
  return [cvar sh]

codeGenExp (Size a) = do
  sh <- codeGenExp (Shape a)
  return [ccall "size" sh]

codeGenExp (IndexScalar a e) = do
  ix <- codeGenExp e
  n  <- length <$> getM arrays
  sh <- ("sh"++) . show . length <$> getM shapes
  let ty = codeGenTupleTex (accType a)
      fv = map (("tex"++) . show) [n..]

  modM arrays (zipWith array ty fv ++)
  modM shapes (mkShape (accDim a) sh :)
  return (zipWith (indexArray sh ix) fv ty)
  where
    array t                 = mkGlobal (map CTypeSpec t)
    indexArray sh ix n t    = ccall (indexer t) [cvar n, ccall "toIndex" [cvar sh, ccall "shape" ix]]
    indexer [CDoubleType _] = "indexDArray"
    indexer _               = "indexArray"


mkShape :: Int -> String -> CExtDecl
mkShape d n = mkGlobal [constant,dimension] n
  where
    constant  = CTypeQual (CAttrQual (CAttr (internalIdent "constant") [] internalNode))
    dimension = CTypeSpec (CTypeDef (internalIdent ("DIM" ++ show d)) internalNode)

mkGlobal :: [CDeclSpec] -> String -> CExtDecl
mkGlobal spec name =
  CDeclExt (CDecl (CStorageSpec (CStatic internalNode) : spec)
           [(Just (CDeclr (Just (internalIdent name)) [] Nothing [] internalNode),Nothing,Nothing)] internalNode)

-- Tuples are defined as snoc-lists, so generate code right-to-left
--
codeGenTup :: Tuple (OpenExp env aenv) t -> CodeGen [CExpr]
codeGenTup NilTup          = return []
codeGenTup (t `SnocTup` e) = (++) <$> codeGenTup t <*> codeGenExp e

-- Convert a typed de Brujin index to the corresponding integer
--
idxToInt :: Idx env t -> Int
idxToInt ZeroIdx       = 0
idxToInt (SuccIdx idx) = 1 + idxToInt idx

-- Convert a tuple index into the corresponding integer. Since the internal
-- representation is flat, be sure to walk over all sub components when indexing
-- past nested tuples.
--
prjToInt :: TupleIdx t e -> TupleType a -> Int
prjToInt ZeroTupIdx     _                 = 0
prjToInt (SuccTupIdx i) (b `PairTuple` a) = length (codeGenTupleType a) + prjToInt i b
prjToInt _ _ =
  INTERNAL_ERROR(error) "prjToInt" "inconsistent valuation"


-- Types
-- -----

-- Generate types for the reified elements of an array computation
--
codeGenAccType :: OpenAcc aenv (Sugar.Array dim e) -> [CType]
codeGenAccType =  codeGenTupleType . accType

codeGenExpType :: OpenExp aenv env t -> [CType]
codeGenExpType =  codeGenTupleType . expType

codeGenAccTypeDim :: OpenAcc aenv (Sugar.Array dim e) -> ([CType],Int)
codeGenAccTypeDim acc = (codeGenAccType acc, accDim acc)


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
-- -----------------

codeGenPrimConst :: PrimConst a -> CExpr
codeGenPrimConst (PrimMinBound ty) = codeGenMinBound ty
codeGenPrimConst (PrimMaxBound ty) = codeGenMaxBound ty
codeGenPrimConst (PrimPi       ty) = codeGenPi ty

codeGenPrim :: PrimFun p -> [CExpr] -> CExpr
codeGenPrim (PrimAdd          _) [a,b] = CBinary CAddOp a b internalNode
codeGenPrim (PrimSub          _) [a,b] = CBinary CSubOp a b internalNode
codeGenPrim (PrimMul          _) [a,b] = CBinary CMulOp a b internalNode
codeGenPrim (PrimNeg          _) [a]   = CUnary  CMinOp a   internalNode
codeGenPrim (PrimAbs         ty) [a]   = codeGenAbs ty a
codeGenPrim (PrimSig         ty) [a]   = codeGenSig ty a
codeGenPrim (PrimQuot         _) [a,b] = CBinary CDivOp a b internalNode
codeGenPrim (PrimRem          _) [a,b] = CBinary CRmdOp a b internalNode
codeGenPrim (PrimIDiv         _) [a,b] = ccall "idiv" [a,b]
codeGenPrim (PrimMod          _) [a,b] = ccall "mod"  [a,b]
codeGenPrim (PrimBAnd         _) [a,b] = CBinary CAndOp a b internalNode
codeGenPrim (PrimBOr          _) [a,b] = CBinary COrOp  a b internalNode
codeGenPrim (PrimBXor         _) [a,b] = CBinary CXorOp a b internalNode
codeGenPrim (PrimBNot         _) [a]   = CUnary  CCompOp a  internalNode
codeGenPrim (PrimBShiftL      _) [a,b] = CBinary CShlOp a b internalNode
codeGenPrim (PrimBShiftR      _) [a,b] = CBinary CShrOp a b internalNode
codeGenPrim (PrimBRotateL     _) [a,b] = ccall "rotateL" [a,b]
codeGenPrim (PrimBRotateR     _) [a,b] = ccall "rotateR" [a,b]
codeGenPrim (PrimFDiv         _) [a,b] = CBinary CDivOp a b internalNode
codeGenPrim (PrimRecip       ty) [a]   = codeGenRecip ty a
codeGenPrim (PrimSin         ty) [a]   = ccall (FloatingNumType ty `postfix` "sin")   [a]
codeGenPrim (PrimCos         ty) [a]   = ccall (FloatingNumType ty `postfix` "cos")   [a]
codeGenPrim (PrimTan         ty) [a]   = ccall (FloatingNumType ty `postfix` "tan")   [a]
codeGenPrim (PrimAsin        ty) [a]   = ccall (FloatingNumType ty `postfix` "asin")  [a]
codeGenPrim (PrimAcos        ty) [a]   = ccall (FloatingNumType ty `postfix` "acos")  [a]
codeGenPrim (PrimAtan        ty) [a]   = ccall (FloatingNumType ty `postfix` "atan")  [a]
codeGenPrim (PrimAsinh       ty) [a]   = ccall (FloatingNumType ty `postfix` "asinh") [a]
codeGenPrim (PrimAcosh       ty) [a]   = ccall (FloatingNumType ty `postfix` "acosh") [a]
codeGenPrim (PrimAtanh       ty) [a]   = ccall (FloatingNumType ty `postfix` "atanh") [a]
codeGenPrim (PrimExpFloating ty) [a]   = ccall (FloatingNumType ty `postfix` "exp")   [a]
codeGenPrim (PrimSqrt        ty) [a]   = ccall (FloatingNumType ty `postfix` "sqrt")  [a]
codeGenPrim (PrimLog         ty) [a]   = ccall (FloatingNumType ty `postfix` "log")   [a]
codeGenPrim (PrimFPow        ty) [a,b] = ccall (FloatingNumType ty `postfix` "pow")   [a,b]
codeGenPrim (PrimLogBase     ty) [a,b] = codeGenLogBase ty a b
codeGenPrim (PrimAtan2       ty) [a,b] = ccall (FloatingNumType ty `postfix` "atan2") [a,b]
codeGenPrim (PrimLt           _) [a,b] = CBinary CLeOp  a b internalNode
codeGenPrim (PrimGt           _) [a,b] = CBinary CGrOp  a b internalNode
codeGenPrim (PrimLtEq         _) [a,b] = CBinary CLeqOp a b internalNode
codeGenPrim (PrimGtEq         _) [a,b] = CBinary CGeqOp a b internalNode
codeGenPrim (PrimEq           _) [a,b] = CBinary CEqOp  a b internalNode
codeGenPrim (PrimNEq          _) [a,b] = CBinary CNeqOp a b internalNode
codeGenPrim (PrimMax         ty) [a,b] = codeGenMax ty a b
codeGenPrim (PrimMin         ty) [a,b] = codeGenMin ty a b
codeGenPrim PrimLAnd             [a,b] = CBinary CLndOp a b internalNode
codeGenPrim PrimLOr              [a,b] = CBinary CLorOp a b internalNode
codeGenPrim PrimLNot             [a]   = CUnary  CNegOp a   internalNode
codeGenPrim PrimOrd              [a]   = CCast (CDecl [CTypeSpec (CIntType  internalNode)] [] internalNode) a internalNode
codeGenPrim PrimChr              [a]   = CCast (CDecl [CTypeSpec (CCharType internalNode)] [] internalNode) a internalNode
codeGenPrim PrimRoundFloatInt    [a]   = ccall "lroundf" [a] -- TLM: (int) rintf(x) ??
codeGenPrim PrimTruncFloatInt    [a]   = ccall "ltruncf" [a]
codeGenPrim PrimIntFloat         [a]   = CCast (CDecl [CTypeSpec (CFloatType internalNode)] [] internalNode) a internalNode -- TLM: __int2float_[rn,rz,ru,rd](a) ??
codeGenPrim PrimBoolToInt        [a]   = CCast (CDecl [CTypeSpec (CIntType   internalNode)] [] internalNode) a internalNode

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
codeGenAbs ty@(IntegralNumType _) x = ccall (ty `postfix` "abs")  [x]
codeGenAbs ty@(FloatingNumType _) x = ccall (ty `postfix` "fabs") [x]

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
codeGenLogBase ty x y = let a = ccall (FloatingNumType ty `postfix` "log") [x]
                            b = ccall (FloatingNumType ty `postfix` "log") [y]
                        in
                        CBinary CDivOp b a internalNode

codeGenMin :: ScalarType a -> CExpr -> CExpr -> CExpr
codeGenMin (NumScalarType ty@(IntegralNumType _)) a b = ccall (ty `postfix` "min")  [a,b]
codeGenMin (NumScalarType ty@(FloatingNumType _)) a b = ccall (ty `postfix` "fmin") [a,b]
codeGenMin (NonNumScalarType _)                   _ _ = undefined

codeGenMax :: ScalarType a -> CExpr -> CExpr -> CExpr
codeGenMax (NumScalarType ty@(IntegralNumType _)) a b = ccall (ty `postfix` "max")  [a,b]
codeGenMax (NumScalarType ty@(FloatingNumType _)) a b = ccall (ty `postfix` "fmax") [a,b]
codeGenMax (NonNumScalarType _)                   _ _ = undefined

-- Auxiliary Functions
-- -------------------

cvar :: String -> CExpr
cvar x = CVar (internalIdent x) internalNode

ccall :: String -> [CExpr] -> CExpr
ccall fn args = CCall (cvar fn) args internalNode

postfix :: NumType a -> String -> String
postfix (FloatingNumType (TypeFloat _))  = (++ "f")
postfix (FloatingNumType (TypeCFloat _)) = (++ "f")
postfix _                                = id

