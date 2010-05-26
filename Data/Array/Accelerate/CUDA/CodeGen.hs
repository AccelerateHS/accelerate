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

import Prelude hiding (id, (.), mod)
import Control.Category

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Pretty ()
import Data.Array.Accelerate.Analysis.Type
import qualified Data.Array.Accelerate.AST              as AST
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar

import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Syntax
import Data.Array.Accelerate.CUDA.CodeGen.Expr (Expr(..))
import qualified Data.Array.Accelerate.CUDA.CodeGen.Skeleton    as Sk

import Foreign.Marshal.Utils (fromBool)


-- Convert a typed de Brujin index to the corresponding integer
--
idxToInt :: AST.Idx env t -> Int
idxToInt AST.ZeroIdx       = 0
idxToInt (AST.SuccIdx idx) = 1 + idxToInt idx


-- |
-- Generate CUDA device code for an array expression
--
-- TLM: extracting resource usage estimates as we go?
--      also, decide if we want to run under with the state token
--
codeGenAcc :: AST.OpenAcc aenv a -> String -> TransUnit
codeGenAcc op@(AST.Map fn xs) name = Sk.map name (Expr out param code Nothing)
  where
    out   = codeGenTupleType (accType op)
    param = [codeGenTupleType (accType xs)]
    code  = codeGenFun fn


{-
codeGenParams :: forall env aenv t. AST.OpenExp env aenv t -> [CUDA.FunParam]
codeGenParams e@(AST.Const c) = case ty of
  Float        -> [CUDA.FArg (read val :: Float)]
  Double       -> [CUDA.VArg (read val :: Double)]
  Char Nothing -> [CUDA.VArg (read val :: Char)]
  Int  Nothing -> [CUDA.IArg (read val :: Int)]
  Bool         -> [CUDA.IArg (fromBool (read val))]
  _            -> error (shows ty " not implemented yet")
  where
    ty  = codeGenTupleType (expType e)
    val = show (Sugar.toElem c :: t)    -- TLM: would be nice if we didn't go via strings

codeGenParams e = error (shows e " could not be marshalled as a kernel parameter")
-}


-- Scalar Functions
-- ~~~~~~~~~~~~~~~~
--
codeGenFun :: AST.OpenFun env aenv t -> [BlkItem]
codeGenFun (AST.Lam  lam)  = codeGenFun lam
codeGenFun (AST.Body body) = [StmtItem . JumpStmt . Return $ Just (codeGenExp body)]


-- Expressions
-- ~~~~~~~~~~~
--
assign :: CUDAExp a => a -> Exp
assign x = Exp [toAssignExp x]

codeGenExp :: forall env aenv t. AST.OpenExp env aenv t -> Exp
codeGenExp (AST.Var i) = assign . Ident $ 'x' : show (idxToInt i)

codeGenExp e@(AST.Const c) = case ty of
  Float        -> assign . FloatConst   $ read val
  Double       -> assign . DoubleConst  $ read val
  Char Nothing -> assign . CharConst    $ read val
  Int  Nothing -> assign . IntegerConst $ read val
  Bool         -> assign . IntegerConst $ fromBool (read val)
  _            -> error (shows ty " not implemented yet")
  where
    ty  = codeGenTupleType (expType e)
    val = show (Sugar.toElem c :: t)    -- TLM: would be nice if we didn't go via strings

codeGenExp (AST.Cond p e1 e2) = assign $ Cond
  (toLgcOrExp . NestedExp $ codeGenExp p)
  (codeGenExp e1)
  (toCondExp  . NestedExp $ codeGenExp e2)

codeGenExp (AST.PrimConst c) = codeGenPrimConst c

codeGenExp (AST.PrimApp f (AST.Tuple (NilTup `SnocTup` x `SnocTup` y))) =
  codeGenPrim f $ map NestedExp [codeGenExp x, codeGenExp y]

codeGenExp e@(AST.PrimApp _ _)     = error (shows e " not supported yet")
codeGenExp e@(AST.Tuple _)         = error (shows e " not supported yet")
codeGenExp e@(AST.Prj _ _)         = error (shows e " not supported yet")
codeGenExp e@(AST.IndexScalar _ _) = error (shows e " not supported yet")
codeGenExp e@(AST.Shape _)         = error (shows e " not supported yet")


-- Types
-- ~~~~~

codeGenTupleType :: TupleType a -> TySpec
codeGenTupleType (UnitTuple)             = undefined
codeGenTupleType (SingleTuple t)         = codeGenScalarType t
codeGenTupleType (PairTuple UnitTuple t) = codeGenTupleType  t
codeGenTupleType (PairTuple _ _)         = undefined

codeGenScalarType :: ScalarType a -> TySpec
codeGenScalarType (NumScalarType t)    = codeGenNumType t
codeGenScalarType (NonNumScalarType t) = codeGenNonNumType t

codeGenNumType :: NumType a -> TySpec
codeGenNumType (IntegralNumType t) = codeGenIntegralType t
codeGenNumType (FloatingNumType t) = codeGenFloatingType t

codeGenIntegralType :: IntegralType a -> TySpec
codeGenIntegralType (TypeInt     _) = Int      Nothing
codeGenIntegralType (TypeInt8    _) = Char     Nothing
codeGenIntegralType (TypeInt16   _) = Short    Nothing
codeGenIntegralType (TypeInt32   _) = Long     Nothing
codeGenIntegralType (TypeInt64   _) = LongLong Nothing
codeGenIntegralType (TypeWord    _) = Int      $ Just Unsigned
codeGenIntegralType (TypeWord8   _) = Char     $ Just Unsigned
codeGenIntegralType (TypeWord16  _) = Short    $ Just Unsigned
codeGenIntegralType (TypeWord32  _) = Long     $ Just Unsigned
codeGenIntegralType (TypeWord64  _) = LongLong $ Just Unsigned
codeGenIntegralType (TypeCShort  _) = Short    Nothing
codeGenIntegralType (TypeCUShort _) = Short    $ Just Unsigned
codeGenIntegralType (TypeCInt    _) = Int      Nothing
codeGenIntegralType (TypeCUInt   _) = Int      $ Just Unsigned
codeGenIntegralType (TypeCLong   _) = Long     Nothing
codeGenIntegralType (TypeCULong  _) = Long     $ Just Unsigned
codeGenIntegralType (TypeCLLong  _) = LongLong Nothing
codeGenIntegralType (TypeCULLong _) = LongLong $ Just Unsigned

codeGenFloatingType :: FloatingType a -> TySpec
codeGenFloatingType (TypeFloat   _) = Float
codeGenFloatingType (TypeDouble  _) = Double
codeGenFloatingType (TypeCFloat  _) = Float
codeGenFloatingType (TypeCDouble _) = Double

codeGenNonNumType :: NonNumType a -> TySpec
codeGenNonNumType (TypeBool   _) = Bool
codeGenNonNumType (TypeChar   _) = Char Nothing
codeGenNonNumType (TypeCChar  _) = Char Nothing
codeGenNonNumType (TypeCSChar _) = Char $ Just Signed
codeGenNonNumType (TypeCUChar _) = Char $ Just Unsigned


-- Scalar Primitives
-- ~~~~~~~~~~~~~~~~~

codeGenPrimConst :: AST.PrimConst a -> Exp
codeGenPrimConst (AST.PrimMinBound ty) = codeGenMinBound ty
codeGenPrimConst (AST.PrimMaxBound ty) = codeGenMaxBound ty
codeGenPrimConst (AST.PrimPi       ty) = codeGenPi ty

codeGenPi :: FloatingType a -> Exp
codeGenPi (TypeFloat   _) = assign (FloatConst  pi)
codeGenPi (TypeDouble  _) = assign (DoubleConst pi)
codeGenPi _               = undefined

codeGenMinBound :: forall a. BoundedType a -> Exp
codeGenMinBound (IntegralBoundedType ty) | IntegralDict <- integralDict ty = assign . IntegerConst $ fromIntegral (minBound :: a)
codeGenMinBound _ = undefined

codeGenMaxBound :: forall a. BoundedType a -> Exp
codeGenMaxBound (IntegralBoundedType ty) | IntegralDict <- integralDict ty = assign . IntegerConst $ fromIntegral (maxBound :: a)
codeGenMaxBound _ = undefined


codeGenPrim :: AST.PrimFun sig -> [PrimaryExp] -> Exp
-- operators from Num
codeGenPrim (AST.PrimAdd         _) [x,y] = assign $ Add  (toAddExp x) (toMulExp y)
codeGenPrim (AST.PrimSub         _) [x,y] = assign $ Sub  (toAddExp x) (toMulExp y)
codeGenPrim (AST.PrimMul         _) [x,y] = assign $ Mul  (toMulExp x) (toCastExp y)
codeGenPrim (AST.PrimNeg         _) [x]   = assign $ Sub  (toAddExp (IntegerConst 0)) (toMulExp x)
codeGenPrim (AST.PrimAbs         _) [x]   = assign $ Cond (toLgcOrExp $ Lt  (toRelExp x) (toShftExp (IntegerConst 0)))
                                                          (assign     $ Sub (toAddExp (IntegerConst 0)) (toMulExp x))
                                                          (toCondExp x)
codeGenPrim (AST.PrimSig         _) [x]   = assign $ Cond (toLgcOrExp $ Lt  (toRelExp x) (toShftExp (IntegerConst 0)))
                                                          (assign (IntegerConst (-1)))
                                                          (Cond (toLgcOrExp $ Gt (toRelExp x) (toShftExp $ IntegerConst 0))
                                                                (Exp [toAssignExp $ IntegerConst 1])
                                                                (toCondExp (IntegerConst 0)))
-- Integral & Bitwise
codeGenPrim (AST.PrimQuot        _) [_,_] = error "AST.PrimQuot"
codeGenPrim (AST.PrimRem         _) [_,_] = error "AST.PrimRem"
codeGenPrim (AST.PrimIDiv        _) [_,_] = error "AST.PrimIDiv"
codeGenPrim (AST.PrimMod         _) [_,_] = error "AST.PrimMod"
codeGenPrim (AST.PrimBAnd        _) [x,y] = assign $ And    (toAndExp x) (toEqExp  y)
codeGenPrim (AST.PrimBOr         _) [x,y] = assign $ Or     (toOrExp  x) (toXorExp y)
codeGenPrim (AST.PrimBXor        _) [x,y] = assign $ Xor    (toXorExp x) (toAndExp y)
codeGenPrim (AST.PrimBNot        _) [x]   = assign $ BitNot (toCastExp x)

-- Fractional, Floating, RealFrac & RealFloat
codeGenPrim (AST.PrimFDiv        _) [_,_] = error "AST.PrimFDiv"
codeGenPrim (AST.PrimRecip       _) [_]   = error "AST.PrimRecip"
codeGenPrim (AST.PrimSin         _) [_]   = error "AST.PrimSin"
codeGenPrim (AST.PrimCos         _) [_]   = error "AST.PrimCos"
codeGenPrim (AST.PrimTan         _) [_]   = error "AST.PrimTan"
codeGenPrim (AST.PrimAsin        _) [_]   = error "AST.PrimAsin"
codeGenPrim (AST.PrimAcos        _) [_]   = error "AST.PrimAcos"
codeGenPrim (AST.PrimAtan        _) [_]   = error "AST.PrimAtan"
codeGenPrim (AST.PrimAsinh       _) [_]   = error "AST.PrimAsinh"
codeGenPrim (AST.PrimAcosh       _) [_]   = error "AST.PrimAcosh"
codeGenPrim (AST.PrimAtanh       _) [_]   = error "AST.PrimAtanh"
codeGenPrim (AST.PrimExpFloating _) [_]   = error "AST.PrimExpFloating"
codeGenPrim (AST.PrimSqrt        _) [_]   = error "AST.PrimSqrt"
codeGenPrim (AST.PrimLog         _) [_]   = error "AST.PrimLog"
codeGenPrim (AST.PrimFPow        _) [_,_] = error "AST.PrimFPow"
codeGenPrim (AST.PrimLogBase     _) [_,_] = error "AST.PrimLogBase"
-- FIXME: add operations from Floating, RealFrac & RealFloat

-- Relational & Equality
codeGenPrim (AST.PrimLt          _) [x,y] = assign $ Lt  (toRelExp x) (toShftExp y)
codeGenPrim (AST.PrimGt          _) [x,y] = assign $ Gt  (toRelExp x) (toShftExp y)
codeGenPrim (AST.PrimLtEq        _) [x,y] = assign $ Le  (toRelExp x) (toShftExp y)
codeGenPrim (AST.PrimGtEq        _) [x,y] = assign $ Ge  (toRelExp x) (toShftExp y)
codeGenPrim (AST.PrimEq          _) [x,y] = assign $ Eq  (toEqExp x)  (toRelExp y)
codeGenPrim (AST.PrimNEq         _) [x,y] = assign $ Neq (toEqExp x)  (toRelExp y)
codeGenPrim (AST.PrimMax         _) [x,y] = assign $ Cond (toLgcOrExp $ Gt (toRelExp x) (toShftExp y))
                                                          (assign x)
                                                          (toCondExp y)
codeGenPrim (AST.PrimMin         _) [x,y] = assign $ Cond (toLgcOrExp $ Lt (toRelExp x) (toShftExp y))
                                                          (assign x)
                                                          (toCondExp y)

-- Logical
codeGenPrim (AST.PrimLAnd         ) [x,y] = assign $ LgcAnd (toLgcAndExp x) (toOrExp y)
codeGenPrim (AST.PrimLOr          ) [x,y] = assign $ LgcOr  (toLgcOrExp  x) (toLgcAndExp y)
codeGenPrim (AST.PrimLNot         ) [x]   = assign $ LgcNot (toCastExp x)

-- Conversions
codeGenPrim (AST.PrimOrd          ) [x]   = assign $ TyCast (TyName [SpecQualTySpec (Int  Nothing)] Nothing) (toCastExp x)
codeGenPrim (AST.PrimChr          ) [x]   = assign $ TyCast (TyName [SpecQualTySpec (Char Nothing)] Nothing) (toCastExp x)
codeGenPrim (AST.PrimIntFloat     ) [x]   = assign $ TyCast (TyName [SpecQualTySpec Float] Nothing) (toCastExp x)
codeGenPrim (AST.PrimBoolToInt    ) [x]   = assign $ TyCast (TyName [SpecQualTySpec (Int Nothing)] Nothing) (toCastExp x)
codeGenPrim (AST.PrimRoundFloatInt) [_]   = error "AST.PrimRoundFloatInt"
codeGenPrim (AST.PrimTruncFloatInt) [_]   = error "AST.PrimTruncFloatInt"

codeGenPrim _ _                           = error "internal error"

