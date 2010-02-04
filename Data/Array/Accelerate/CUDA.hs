{-# LANGUAGE GADTs, BangPatterns, PatternGuards #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA
-- Copyright   : [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module is the CUDA backend for the embedded array language.

module Data.Array.Accelerate.CUDA (

  -- * Generate CUDA code and execute it
  Arrays, run
  
) where

-- standard libraries
import Control.Monad.State
import Data.Bits
import Data.Char            (chr, ord)
import System.Exit          (exitFailure, ExitCode(ExitSuccess))
import System.Posix.Process (
  executeFile, forkProcess, getProcessStatus, ProcessStatus(..))

-- friends
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Sugar (
  Array(..), Scalar, Vector, Segments)
import Data.Array.Accelerate.Array.Delayed
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Smart       as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar

import qualified Data.Array.Accelerate.CUDA.Monad   as CUDA
import qualified Data.Array.Accelerate.CUDA.Scalar  as CUDA
import qualified Data.Array.Accelerate.CUDA.Syntax  as CUDA
import qualified Data.Array.Accelerate.CUDA.Fold    as CUDA
import qualified Data.Array.Accelerate.CUDA.Map     as CUDA
import qualified Data.Array.Accelerate.CUDA.ZipWith as CUDA

-- Program execution
-- -----------------

-- |Characterises the types that may be returned when running an array program.
--
class Delayable as => Arrays as
  
instance Arrays ()  
instance Arrays (Array dim e)
instance (Arrays as1, Arrays as2) => Arrays (as1, as2)

-- |Compile and run a complete embedded array program using the CUDA backend.
--
run :: Arrays a => Sugar.Acc a -> IO a
run acc = do
  (progName, _) <- runStateT (codeGenAcc $ Sugar.convertAcc acc) (CUDA.CGState 0)
  compile progName
  error $ show "Compilation done"

--
-- CUDA code compilation
-- ---------------------

compile :: String -> IO ()
compile progName = do
  let checkStatus status ext = case status of
        Just (Exited ExitSuccess) -> return()
        _ -> putStrLn ("Failed to compile " ++ progName ++ ext ++ ".") >> exitFailure
      cppFlags =
        [ "-W", "-Wall", "-Wimplicit", "-Wswitch", "-Wformat"
        , "-Wchar-subscripts", "-Wparentheses", "-Wmultichar", "-Wtrigraphs"
        , "-Wpointer-arith", "-Wcast-align", "-Wreturn-type"
        , "-Wno-unused-function", "-arch", "i386", "-fno-strict-aliasing"
        , "-DUNIX", "-O2", "-o", progName ++ ".cpp.o", "-c"
        , progName ++ ".cpp"]
      cuFlags =
        [ "-m32", "--compiler-options", "-fno-strict-aliasing", "-DUNIX"
        , "-O2", "-o", progName ++ ".cubin", "-cubin", progName ++ ".cu"]
  cppPid <- forkProcess $ executeFile "g++"  True cppFlags Nothing
  cuPid  <- forkProcess $ executeFile "nvcc" True cuFlags  Nothing
  cppStatus <- getProcessStatus True True cppPid
  cuStatus  <- getProcessStatus True True cuPid
  checkStatus cppStatus ".cpp"
  checkStatus cuStatus  ".cu"

--
-- CUDA code generation
-- --------------------

-- Generate CUDA code for an array expression
--
codeGenAcc :: Delayable a => OpenAcc aenv a -> CUDA.CGIO String
codeGenAcc op@(Map fun xs) = do
  currentState <- get
  let uniqueID = CUDA.uniqueID currentState
      progName = "CUDAMap" ++ show uniqueID
      scalar = CUDA.Scalar
        { CUDA.params =
          [ (codeGenTupleType $ accType xs, "x0")]
        , CUDA.outTy = codeGenTupleType $ accType op
        , CUDA.comp = codeGenFun fun
        , CUDA.identity = Nothing}
  put $ currentState {CUDA.uniqueID = uniqueID + 1}
  liftIO $ CUDA.mapGen progName scalar
  return progName
codeGenAcc op@(ZipWith fun xs ys) = do
  currentState <- get
  let uniqueID = CUDA.uniqueID currentState
      progName = "CUDAZipWith" ++ show uniqueID 
      scalar = CUDA.Scalar
        { CUDA.params =
          [ (codeGenTupleType $ accType xs, "x1")
          , (codeGenTupleType $ accType ys, "x0")]
        , CUDA.outTy = codeGenTupleType $ accType op
        , CUDA.comp = codeGenFun fun
        , CUDA.identity = Nothing}
  put $ currentState {CUDA.uniqueID = uniqueID + 1}
  liftIO $ CUDA.zipWithGen progName scalar
  return progName
codeGenAcc op@(Fold fun left xs) = do
  currentState <- get
  let uniqueID = CUDA.uniqueID currentState
      progName = "CUDAFold" ++ show uniqueID
      scalar = CUDA.Scalar
        { CUDA.params =
          [ (codeGenTupleType $ accType xs, "x0")]
        , CUDA.outTy = codeGenTupleType $ accType op
        , CUDA.comp = codeGenFun fun
        , CUDA.identity = Nothing}
  put $ currentState {CUDA.uniqueID = uniqueID + 1}
  liftIO $ CUDA.foldGen progName scalar (codeGenTupleType $ expType left, "left")
  return progName

-- Scalar function
-- ---------------

codeGenFun :: OpenFun env aenv t -> [CUDA.BlkItem]
codeGenFun f@(Lam lam) = codeGenFun lam
codeGenFun (Body body) =
  [CUDA.StmtItem $ CUDA.JumpStmt $ CUDA.Return $ Just $ codeGenExp body]

-- Expression
-- ----------

codeGenExp :: forall t env aenv . OpenExp env aenv t -> CUDA.Exp
codeGenExp e@(Var idx) =
  let idxToInt :: Idx env' t' -> Int
      idxToInt ZeroIdx        = 0
      idxToInt (SuccIdx idx') = 1 + idxToInt idx'
  in  CUDA.Exp [CUDA.toAssignExp $ CUDA.Ident $ "x" ++ show (idxToInt idx)]
codeGenExp e@(Const c) =
  let valStr = show (Sugar.toElem c :: t)
      getCUDAConst (CUDA.Float)        = CUDA.FloatConst   $ read valStr
      getCUDAConst (CUDA.Double)       = CUDA.DoubleConst  $ read valStr
      getCUDAConst (CUDA.Char Nothing) = CUDA.CharConst    $ read valStr
      getCUDAConst (CUDA.Int  Nothing) | valStr == "True"  = CUDA.IntegerConst 1
                                       | valStr == "False" = CUDA.IntegerConst 0
                                       | otherwise = CUDA.IntegerConst $ read valStr
      getCUDAConst t' = error $ (show t') ++ " not supported yet as a const type"
  in  CUDA.Exp [CUDA.toAssignExp $ getCUDAConst $ codeGenTupleType $ expType e]
codeGenExp e@(Tuple tup) = error $ "the expression, " ++ (show e) ++ ", not supported yet by the code generator."
codeGenExp e@(Prj idx e') = error $ "the expression, " ++ (show e) ++ ", not supported yet by the code generator."
codeGenExp e@(Cond e1 e2 e3) =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.NestedExp $ codeGenExp e1)
    (codeGenExp e2)
    (CUDA.toCondExp $ CUDA.NestedExp $ codeGenExp e3)]
codeGenExp e@(PrimConst c) = codeGenPrimConst c
codeGenExp e@(PrimApp f a@(Tuple (NilTup `SnocTup` e1 `SnocTup` e2))) =
  codeGenPrim f $ map CUDA.NestedExp [codeGenExp e1, codeGenExp e2]
codeGenExp e@(IndexScalar acc ix) = error $ "the expression, " ++ (show e) ++ ", not supported yet by the code generator."
codeGenExp e@(Shape acc) = error $ "the expression, " ++ (show e) ++ ", not supported yet by the code generator."

-- Types
-- -----

codeGenTupleType :: TupleType a -> CUDA.TySpec
codeGenTupleType (UnitTuple)             = error "not implemented yet"
codeGenTupleType (SingleTuple t)         = codeGenScalarType t
codeGenTupleType (PairTuple UnitTuple t) = codeGenTupleType t
codeGenTupleType (PairTuple _         _) = error "not implemented yet"

codeGenScalarType :: ScalarType a -> CUDA.TySpec
codeGenScalarType (NumScalarType t)    = codeGenNumType t
codeGenScalarType (NonNumScalarType t) = codeGenNonNumType t

codeGenNumType :: NumType a -> CUDA.TySpec
codeGenNumType (IntegralNumType t) = codeGenIntegralType t
codeGenNumType (FloatingNumType t) = codeGenFloatingType t

codeGenIntegralType :: IntegralType a -> CUDA.TySpec
codeGenIntegralType (TypeInt     _) = CUDA.Int      Nothing
codeGenIntegralType (TypeInt8    _) = CUDA.Char     Nothing
codeGenIntegralType (TypeInt16   _) = CUDA.Short    Nothing
codeGenIntegralType (TypeInt32   _) = CUDA.Long     Nothing
codeGenIntegralType (TypeInt64   _) = CUDA.LongLong Nothing
codeGenIntegralType (TypeWord    _) = CUDA.Int      $ Just CUDA.Unsigned
codeGenIntegralType (TypeWord8   _) = CUDA.Char     $ Just CUDA.Unsigned
codeGenIntegralType (TypeWord16  _) = CUDA.Short    $ Just CUDA.Unsigned
codeGenIntegralType (TypeWord32  _) = CUDA.Long     $ Just CUDA.Unsigned
codeGenIntegralType (TypeWord64  _) = CUDA.LongLong $ Just CUDA.Unsigned
codeGenIntegralType (TypeCShort  _) = CUDA.Short    Nothing
codeGenIntegralType (TypeCUShort _) = CUDA.Short    $ Just CUDA.Unsigned
codeGenIntegralType (TypeCInt    _) = CUDA.Int      Nothing
codeGenIntegralType (TypeCUInt   _) = CUDA.Int      $ Just CUDA.Unsigned
codeGenIntegralType (TypeCLong   _) = CUDA.Long     Nothing
codeGenIntegralType (TypeCULong  _) = CUDA.Long     $ Just CUDA.Unsigned
codeGenIntegralType (TypeCLLong  _) = CUDA.LongLong Nothing
codeGenIntegralType (TypeCULLong _) = CUDA.LongLong $ Just CUDA.Unsigned

codeGenFloatingType :: FloatingType a -> CUDA.TySpec
codeGenFloatingType (TypeFloat   _) = CUDA.Float
codeGenFloatingType (TypeDouble  _) = CUDA.Double
codeGenFloatingType (TypeCFloat  _) = CUDA.Float
codeGenFloatingType (TypeCDouble _) = CUDA.Double

codeGenNonNumType :: NonNumType a -> CUDA.TySpec
codeGenNonNumType (TypeBool   _) = CUDA.Int  Nothing
codeGenNonNumType (TypeChar   _) = CUDA.Char Nothing
codeGenNonNumType (TypeCChar  _) = CUDA.Char Nothing
codeGenNonNumType (TypeCSChar _) = CUDA.Char $ Just CUDA.Signed
codeGenNonNumType (TypeCUChar _) = CUDA.Char $ Just CUDA.Unsigned

-- Scalar primitives
-- -----------------

codeGenPrimConst :: PrimConst a -> CUDA.Exp
codeGenPrimConst (PrimMinBound ty) = codeGenMinBound ty
codeGenPrimConst (PrimMaxBound ty) = codeGenMaxBound ty
codeGenPrimConst (PrimPi       ty) = codeGenPi ty

codeGenMinBound :: forall a . BoundedType a -> CUDA.Exp
codeGenMinBound (IntegralBoundedType ty)
  | IntegralDict <- integralDict ty =
    CUDA.Exp
      [CUDA.toAssignExp $ CUDA.IntegerConst $ fromIntegral (minBound :: a)]
codeGenMinBound (NonNumBoundedType ty@(TypeBool _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp [CUDA.toAssignExp $ CUDA.IntegerConst 1]
codeGenMinBound (NonNumBoundedType ty@(TypeChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (minBound :: CChar)]
codeGenMinBound (NonNumBoundedType ty@(TypeCChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (minBound :: CChar)]
codeGenMinBound (NonNumBoundedType ty@(TypeCSChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (minBound :: CSChar)]
codeGenMinBound (NonNumBoundedType ty@(TypeCUChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (minBound :: CUChar)]

codeGenMaxBound :: forall a . BoundedType a -> CUDA.Exp
codeGenMaxBound (IntegralBoundedType ty)
  | IntegralDict <- integralDict ty =
    CUDA.Exp
      [CUDA.toAssignExp $ CUDA.IntegerConst $ fromIntegral (maxBound :: a)]
codeGenMaxBound (NonNumBoundedType ty@(TypeBool _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp [CUDA.toAssignExp $ CUDA.IntegerConst 1]
codeGenMaxBound (NonNumBoundedType ty@(TypeChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (maxBound :: CChar)]
codeGenMaxBound (NonNumBoundedType ty@(TypeCChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (maxBound :: CChar)]
codeGenMaxBound (NonNumBoundedType ty@(TypeCSChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (maxBound :: CSChar)]
codeGenMaxBound (NonNumBoundedType ty@(TypeCUChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (maxBound :: CUChar)]

codeGenPi :: FloatingType a -> CUDA.Exp
codeGenPi ty@(TypeFloat _)   | FloatingDict <- floatingDict ty =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.FloatConst pi]
codeGenPi ty@(TypeDouble _)  | FloatingDict <- floatingDict ty =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.DoubleConst pi]
codeGenPi ty@(TypeCFloat _)  | FloatingDict <- floatingDict ty =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.FloatConst pi]
codeGenPi ty@(TypeCDouble _) | FloatingDict <- floatingDict ty =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.DoubleConst pi]

codeGenPrim :: PrimFun sig -> [CUDA.PrimaryExp] -> CUDA.Exp
codeGenPrim (PrimAdd         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Add (CUDA.toAddExp x) (CUDA.toMulExp y)]
codeGenPrim (PrimSub         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Sub (CUDA.toAddExp x) (CUDA.toMulExp y)]
codeGenPrim (PrimMul         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Mul (CUDA.toMulExp x) (CUDA.toCastExp y)]
codeGenPrim (PrimNeg         _) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Sub
    (CUDA.toAddExp $ CUDA.IntegerConst 0) (CUDA.toMulExp x)]
codeGenPrim (PrimAbs         _) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.Lt
      (CUDA.toRelExp x) (CUDA.toShftExp $ CUDA.IntegerConst 0))
    (CUDA.Exp [CUDA.toAssignExp $ CUDA.Sub
      (CUDA.toAddExp $ CUDA.IntegerConst 0) (CUDA.toMulExp x)])
    (CUDA.toCondExp x)]
codeGenPrim (PrimSig         _) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.Lt
      (CUDA.toRelExp x) (CUDA.toShftExp $ CUDA.IntegerConst 0))
    (CUDA.Exp [CUDA.toAssignExp $ CUDA.IntegerConst (-1)])
    (CUDA.Cond
      (CUDA.toLgcOrExp $ CUDA.Gt
        (CUDA.toRelExp x) (CUDA.toShftExp $ CUDA.IntegerConst 0))
      (CUDA.Exp [CUDA.toAssignExp $ CUDA.IntegerConst 1])
      (CUDA.toCondExp $ CUDA.IntegerConst 0))]
codeGenPrim (PrimQuot        _) [x, y] = error "the code generation of PrimQuot not implemented yet."
codeGenPrim (PrimRem         _) [x, y] = error "the code generation of PrimRem not implemented yet."
codeGenPrim (PrimIDiv        _) [x, y] = error "the code generation of PrimIDiv not implemented yet."
codeGenPrim (PrimMod         _) [x, y] = error "the code generation of PrimMod not implemented yet."
codeGenPrim (PrimBAnd        _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.And (CUDA.toAndExp x) (CUDA.toEqExp y)]
codeGenPrim (PrimBOr         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Or (CUDA.toOrExp x) (CUDA.toXorExp y)]
codeGenPrim (PrimBXor        _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Xor (CUDA.toXorExp x) (CUDA.toAndExp y)]
codeGenPrim (PrimBNot        _) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.BitNot (CUDA.toCastExp x)]
codeGenPrim (PrimFDiv        _) [x, y] = error "the code generation of PrimFDiv not implemented yet."
codeGenPrim (PrimRecip       _) [x] = error "the code generation of PrimRecip not implemented yet."
codeGenPrim (PrimSin         _) [x] = error "the code generation of PrimSin not implemented yet."
codeGenPrim (PrimCos         _) [x] = error "the code generation of PrimCos not implemented yet."
codeGenPrim (PrimTan         _) [x] = error "the code generation of PrimTan not implemented yet."
codeGenPrim (PrimAsin        _) [x] = error "the code generation of PrimAsin not implemented yet."
codeGenPrim (PrimAcos        _) [x] = error "the code generation of PrimAcos not implemented yet."
codeGenPrim (PrimAtan        _) [x] = error "the code generation of PrimAtan not implemented yet."
codeGenPrim (PrimAsinh       _) [x] = error "the code generation of PrimAsinh not implemented yet."
codeGenPrim (PrimAcosh       _) [x] = error "the code generation of PrimAcosh not implemented yet."
codeGenPrim (PrimAtanh       _) [x] = error "the code generation of PrimAtanh not implemented yet."
codeGenPrim (PrimExpFloating _) [x] = error "the code generation of PrimExpFloating not implemented yet."
codeGenPrim (PrimSqrt        _) [x] = error "the code generation of PrimSqrt not implemented yet."
codeGenPrim (PrimLog         _) [x] = error "the code generation of PrimLog not implemented yet."
codeGenPrim (PrimFPow        _) [x, y] = error "the code generation of PrimFPow not implemented yet."
codeGenPrim (PrimLogBase     _) [x, y] = error "the code generation of PrimLogBase not implemented yet."
codeGenPrim (PrimLt          _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Lt (CUDA.toRelExp x) (CUDA.toShftExp y)]
codeGenPrim (PrimGt          _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Gt (CUDA.toRelExp x) (CUDA.toShftExp y)]
codeGenPrim (PrimLtEq        _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Le (CUDA.toRelExp x) (CUDA.toShftExp y)]
codeGenPrim (PrimGtEq        _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Ge (CUDA.toRelExp x) (CUDA.toShftExp y)]
codeGenPrim (PrimEq          _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Eq (CUDA.toEqExp x) (CUDA.toRelExp y)]
codeGenPrim (PrimNEq         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Neq (CUDA.toEqExp x) (CUDA.toRelExp y)]
codeGenPrim (PrimMax         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.Gt (CUDA.toRelExp x) (CUDA.toShftExp y))
    (CUDA.Exp [CUDA.toAssignExp x])
    (CUDA.toCondExp y)]
codeGenPrim (PrimMin         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.Lt (CUDA.toRelExp x) (CUDA.toShftExp y))
    (CUDA.Exp [CUDA.toAssignExp x])
    (CUDA.toCondExp y)]
codeGenPrim (PrimLAnd         ) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.LgcAnd
    (CUDA.toLgcAndExp x) (CUDA.toOrExp y)]
codeGenPrim (PrimLOr          ) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.LgcOr
    (CUDA.toLgcOrExp x) (CUDA.toLgcAndExp y)]
codeGenPrim (PrimLNot         ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.LgcNot (CUDA.toCastExp x)]
codeGenPrim (PrimOrd          ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.TyCast
    (CUDA.TyName [CUDA.SpecQualTySpec (CUDA.Int Nothing)] Nothing)
    (CUDA.toCastExp x)]
codeGenPrim (PrimChr          ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.TyCast
    (CUDA.TyName [CUDA.SpecQualTySpec (CUDA.Char Nothing)] Nothing)
    (CUDA.toCastExp x)]
codeGenPrim (PrimRoundFloatInt) [x] = error "the code generation of PrimRoundFloatInt not implemented yet."
codeGenPrim (PrimTruncFloatInt) [x] = error "the code generation of PrimTruncFloatInt not implemented yet."
codeGenPrim (PrimIntFloat     ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.TyCast
    (CUDA.TyName [CUDA.SpecQualTySpec CUDA.Float] Nothing)
    (CUDA.toCastExp x)]
codeGenPrim (PrimBoolToInt    ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.TyCast
    (CUDA.TyName [CUDA.SpecQualTySpec (CUDA.Int Nothing)] Nothing)
    (CUDA.toCastExp x)]
