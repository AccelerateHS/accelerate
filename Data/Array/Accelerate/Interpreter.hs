{-# LANGUAGE GADTs, BangPatterns, PatternGuards #-}

-- |Embedded array processing language: execution by a simple interpreter
--
--  Copyright (c) [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--

module Data.Array.Accelerate.Interpreter (

  -- * Execute an array computation by interpretation
  run

) where

-- standard libraries
import Data.Bits
import Data.Char        (chr, ord)
import Foreign.C.String

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.AST


-- | Array code execution
-- -

-- |Execute a collective operation.
--
run :: Comp a -> a
run = error "Data.Array.Accelerate.Interpreter.run: unimplemented"

-- Valuation for an expression environment
--
data Val env where
  Empty :: Val ()
  Push  :: Val env -> t -> Val (env, t)

-- Projection of a value from a valuation using a de Bruijn index
--
prj :: Idx env t -> Val env -> t
prj ZeroIdx       (Push val v) = v
prj (SuccIdx idx) (Push val _) = prj idx val

-- Execute a close expression
--
runExp :: Exp t -> t
runExp e = runOpenExp e Empty

-- Execute an open expression
--
runOpenExp :: OpenExp env a -> Val env -> a
runOpenExp (Var _ idx)          env = prj idx env
runOpenExp (Const _ c)          _   = c
runOpenExp (Pair e1 e2)         env = (runOpenExp e1 env, runOpenExp e2 env)
runOpenExp (Fst e)              env = let (v1, !_) = runOpenExp e env in v1
runOpenExp (Snd e)              env = let (!_, v2) = runOpenExp e env in v2
runOpenExp (Cond c t e)         env = if runOpenExp c env then runOpenExp t env 
                                                          else runOpenExp e env
runOpenExp (PrimConst c)        _   = runPrimConst c
runOpenExp (PrimApp p arg)      env = runPrim p $ runOpenExp arg env
runOpenExp (IndexScalar arr ix) env = error "???"
runOpenExp (Shape arr)          _   = error "arr doesn't contain the array value"
runPrimConst :: PrimConst a -> a
runPrimConst (PrimMinBound ty) = runMinBound ty
runPrimConst (PrimMaxBound ty) = runMaxBound ty
runPrimConst (PrimPi       ty) = runPi ty

runPrim :: PrimFun p -> p
runPrim (PrimAdd   ty)     = runAdd ty
runPrim (PrimSub   ty)     = runSub ty
runPrim (PrimMul   ty)     = runMul ty
runPrim (PrimNeg   ty)     = runNeg ty
runPrim (PrimAbs   ty)     = runAbs ty
runPrim (PrimSig   ty)     = runSig ty
runPrim (PrimQuot  ty)     = runQuot ty
runPrim (PrimRem   ty)     = runRem ty
runPrim (PrimIDiv  ty)     = runIDiv ty
runPrim (PrimMod   ty)     = runMod ty
runPrim (PrimBAnd  ty)     = runBAnd ty
runPrim (PrimBOr   ty)     = runBOr ty
runPrim (PrimBXor  ty)     = runBXor ty
runPrim (PrimBNot  ty)     = runBNot ty
runPrim (PrimFDiv  ty)     = runFDiv ty
runPrim (PrimRecip ty)     = runRecip ty
runPrim (PrimLt    ty)     = runLt ty
runPrim (PrimGt    ty)     = runGt ty
runPrim (PrimLtEq  ty)     = runLtEq ty
runPrim (PrimGtEq  ty)     = runGtEq ty
runPrim (PrimEq    ty)     = runEq ty
runPrim (PrimNEq   ty)     = runNEq ty
runPrim (PrimMax   ty)     = runMax ty
runPrim (PrimMin   ty)     = runMin ty
runPrim PrimLAnd           = runLAnd
runPrim PrimLOr            = runLOr
runPrim PrimLNot           = runLNot
runPrim PrimOrd            = runOrd
runPrim PrimChr            = runChr
runPrim PrimRoundFloatInt  = runRoundFloatInt
runPrim PrimTruncFloatInt  = runTruncFloatInt
runPrim PrimIntFloat       = runIntFloat


-- Implementation of the primitives
-- --------------------------------

runLAnd :: (Bool, Bool) -> Bool
runLAnd (!x, !y) = x && y

runLOr  :: (Bool, Bool) -> Bool
runLOr (!x, !y) = x || y

runLNot :: Bool -> Bool
runLNot x = not x

runOrd :: Char -> Int
runOrd = ord

runChr :: Int -> Char
runChr =  chr

runRoundFloatInt :: Float -> Int
runRoundFloatInt = round

runTruncFloatInt :: Float -> Int
runTruncFloatInt = truncate

runIntFloat :: Int -> Float
runIntFloat = fromIntegral


-- |Extract methods from reified dictionaries
-- ------------------------------------------

-- |Constant methods of Bounded
-- -

runMinBound :: BoundedType a -> a
runMinBound (IntegralBoundedType ty) 
  | IntegralDict <- integralDict ty = minBound
runMinBound (NonNumBoundedType   ty) 
  | NonNumDict   <- nonNumDict ty   = minBound

runMaxBound :: BoundedType a -> a
runMaxBound (IntegralBoundedType ty) 
  | IntegralDict <- integralDict ty = maxBound
runMaxBound (NonNumBoundedType   ty) 
  | NonNumDict   <- nonNumDict ty   = maxBound

-- |Constant method of floating
-- -

runPi :: FloatingType a -> a
runPi ty | FloatingDict <- floatingDict ty = pi

-- |Methods of Num
-- -

runAdd :: NumType a -> ((a, a) -> a)
runAdd (IntegralNumType ty) | IntegralDict <- integralDict ty = uncurry (+)
runAdd (FloatingNumType ty) | FloatingDict <- floatingDict ty = uncurry (+)

runSub :: NumType a -> ((a, a) -> a)
runSub (IntegralNumType ty) | IntegralDict <- integralDict ty = uncurry (-)
runSub (FloatingNumType ty) | FloatingDict <- floatingDict ty = uncurry (-)

runMul :: NumType a -> ((a, a) -> a)
runMul (IntegralNumType ty) | IntegralDict <- integralDict ty = uncurry (*)
runMul (FloatingNumType ty) | FloatingDict <- floatingDict ty = uncurry (*)

runNeg :: NumType a -> (a -> a)
runNeg (IntegralNumType ty) | IntegralDict <- integralDict ty = negate
runNeg (FloatingNumType ty) | FloatingDict <- floatingDict ty = negate

runAbs :: NumType a -> (a -> a)
runAbs (IntegralNumType ty) | IntegralDict <- integralDict ty = abs
runAbs (FloatingNumType ty) | FloatingDict <- floatingDict ty = abs

runSig :: NumType a -> (a -> a)
runSig (IntegralNumType ty) | IntegralDict <- integralDict ty = signum
runSig (FloatingNumType ty) | FloatingDict <- floatingDict ty = signum

runQuot :: IntegralType a -> ((a, a) -> a)
runQuot ty | IntegralDict <- integralDict ty = uncurry quot

runRem :: IntegralType a -> ((a, a) -> a)
runRem ty | IntegralDict <- integralDict ty = uncurry rem

runIDiv :: IntegralType a -> ((a, a) -> a)
runIDiv ty | IntegralDict <- integralDict ty = uncurry div

runMod :: IntegralType a -> ((a, a) -> a)
runMod ty | IntegralDict <- integralDict ty = uncurry mod

runBAnd :: IntegralType a -> ((a, a) -> a)
runBAnd ty | IntegralDict <- integralDict ty = uncurry (.&.)

runBOr :: IntegralType a -> ((a, a) -> a)
runBOr ty | IntegralDict <- integralDict ty = uncurry (.|.)

runBXor :: IntegralType a -> ((a, a) -> a)
runBXor ty | IntegralDict <- integralDict ty = uncurry xor

runBNot :: IntegralType a -> (a -> a)
runBNot ty | IntegralDict <- integralDict ty = complement

runFDiv :: FloatingType a -> ((a, a) -> a)
runFDiv ty | FloatingDict <- floatingDict ty = uncurry (/)

runRecip :: FloatingType a -> (a -> a)
runRecip ty | FloatingDict <- floatingDict ty = recip

runLt :: ScalarType a -> ((a, a) -> Bool)
runLt (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry (<)
runLt (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry (<)
runLt (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry (<)

runGt :: ScalarType a -> ((a, a) -> Bool)
runGt (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry (>)
runGt (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry (>)
runGt (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry (>)

runLtEq :: ScalarType a -> ((a, a) -> Bool)
runLtEq (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry (<=)
runLtEq (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry (<=)
runLtEq (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry (<=)

runGtEq :: ScalarType a -> ((a, a) -> Bool)
runGtEq (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry (>=)
runGtEq (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry (>=)
runGtEq (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry (>=)

runEq :: ScalarType a -> ((a, a) -> Bool)
runEq (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry (==)
runEq (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry (==)
runEq (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry (==)

runNEq :: ScalarType a -> ((a, a) -> Bool)
runNEq (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry (/=)
runNEq (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry (/=)
runNEq (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry (/=)

runMax :: ScalarType a -> ((a, a) -> a)
runMax (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry max
runMax (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry max
runMax (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry max

runMin :: ScalarType a -> ((a, a) -> a)
runMin (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry min
runMin (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry min
runMin (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry min

