{-# LANGUAGE GADTs, BangPatterns, PatternGuards, RankNTypes #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

-- |Embedded array processing language: execution by a simple interpreter
--
--  Copyright (c) [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--
--  This interpreter is meant to be a reference implementation of the semantics
--  of the embedded array language.  The emphasis is on defining the semantics
--  as clearly as possible, not on performance.

module Data.Array.Accelerate.Interpreter (

  -- * Execute an array computation by interpretation
  run,
  
  -- * Delayed array class
  Delay

) where

-- standard libraries
import Data.Bits
import Data.Char                (chr, ord)

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Sugar (Elem(..), ElemRepr)
import Data.Array.Accelerate.AST


-- Program execution
-- -----------------

-- Run a complete array program
--
run :: Delay a => Acc a -> a
run = force . evalAcc


-- Environments
-- ------------

-- Valuation for an environment
--
data Val env where
  Empty :: Val ()
  Push  :: Val env -> t -> Val (env, t)

-- Projection of a value from a valuation using a de Bruijn index
--
prj :: Idx env t -> Val env -> t
prj ZeroIdx       (Push val v) = v
prj (SuccIdx idx) (Push val _) = prj idx val


-- Delayed arrays
-- --------------

-- Delayed arrays are characterised by the domain of an array and its functional
-- representation
-- 

class Delay a where
  data Delayed a
  delay :: a -> Delayed a
  force :: Delayed a -> a
  
instance Delay () where
  data Delayed () = DelayedUnit
  delay ()          = DelayedUnit
  force DelayedUnit = ()

instance Delay (Array dim e) where
  data Delayed (Array dim e) = (Ix dim, ArrayElem e) => 
                               DelayedArray { shapeDA :: dim
                                            , repfDA  :: (dim -> e)
                                            }
  delay arr@(Array sh _)    = DelayedArray sh (arr!)
  force (DelayedArray sh f) = newArray sh f
  
instance (Delay a1, Delay a2) => Delay (a1, a2) where
  data Delayed (a1, a2) = DelayedPair (Delayed a1) (Delayed a2)
  delay (a1, a2) = DelayedPair (delay a1) (delay a2)
  force (DelayedPair a1 a2) = (force a1, force a2)


-- Array expression evaluation
-- ---------------------------

-- Evaluate an open array expression
--
evalOpenAcc :: Delay a => OpenAcc aenv a -> Val aenv -> Delayed a
evalOpenAcc (Let acc1 acc2) aenv = let !arr1 = force $ evalOpenAcc acc1 aenv
                                   in evalOpenAcc acc2 (aenv `Push` arr1)
evalOpenAcc (Avar idx)      aenv = delay $ prj idx aenv
evalOpenAcc (Use arr)       aenv = delay arr
evalOpenAcc (Unit e)        aenv = unit (evalExp e aenv)
evalOpenAcc (Reshape e acc) aenv = reshape (evalExp e aenv) 
                                           (evalOpenAcc acc aenv)

-- Evaluate a closed array expressions
--
evalAcc :: Delay a => Acc a -> Delayed a
evalAcc acc = evalOpenAcc acc Empty


-- Array primitives
-- ----------------

unit :: ArrayElem e => e -> Delayed (Scalar e)
unit e = DelayedArray {shapeDA = (), repfDA = const e}

reshape :: Ix dim => dim -> Delayed (Array dim' e) -> Delayed (Array dim e)
reshape newShape darr@(DelayedArray {shapeDA = oldShape})
  | size newShape == size oldShape
  = let Array _ adata = force darr
    in 
    delay $ Array newShape adata
  | otherwise 
  = error "Data.Array.Accelerate.Interpreter.reshape: shape mismatch"
  

-- Expression evaluation
-- ---------------------

-- Evaluate an open expression
--
-- NB: The implementation of 'IndexScalar' and 'Shape' demonstrate clearly why
--     array expressions must be hoisted out of scalar expressions before code
--     execution.  If these operations are in the body of a function that
--     gets mapped over an array, the array argument would be forced many times
--     leading to a large amount of wasteful recomputation.
--  
evalOpenExp :: OpenExp env aenv a -> Val env -> Val aenv -> a
evalOpenExp (Var idx)            env _    = prj idx env
evalOpenExp (Const c)            _   _    = c
evalOpenExp (Pair ds dt e1 e2)   env aenv = evalPair ds dt
                                                   (evalOpenExp e1 env aenv) 
         									                         (evalOpenExp e2 env aenv)
evalOpenExp (Fst ds dt e)        env aenv = evalFst ds dt 
                                                    (evalOpenExp e env aenv)
evalOpenExp (Snd ds dt e)        env aenv = evalSnd ds dt 
                                                    (evalOpenExp e env aenv)
evalOpenExp (Cond c t e)         env aenv = if toElem (evalOpenExp c env aenv) 
                                            then evalOpenExp t env aenv
                                            else evalOpenExp e env aenv
evalOpenExp (PrimConst c)        _   _    = fromElem $ evalPrimConst c
evalOpenExp (PrimApp p arg)      env aenv 
  = fromElem $ evalPrim p (toElem (evalOpenExp arg env aenv))
evalOpenExp (IndexScalar acc ix) env aenv 
  = (force $ evalOpenAcc acc aenv) ! (evalOpenExp ix env aenv)
evalOpenExp (Shape acc)          _   aenv 
  = let Array sh _ = force $ evalOpenAcc acc aenv 
    in sh


-- Evaluate a closed expression
--
evalExp :: Exp aenv t -> Val aenv -> t
evalExp e aenv = evalOpenExp e Empty aenv


-- Scalar primitives
-- -----------------

evalPrimConst :: PrimConst a -> a
evalPrimConst (PrimMinBound ty) = evalMinBound ty
evalPrimConst (PrimMaxBound ty) = evalMaxBound ty
evalPrimConst (PrimPi       ty) = evalPi ty

evalPrim :: PrimFun p -> p
evalPrim (PrimAdd   ty)    = evalAdd ty
evalPrim (PrimSub   ty)    = evalSub ty
evalPrim (PrimMul   ty)    = evalMul ty
evalPrim (PrimNeg   ty)    = evalNeg ty
evalPrim (PrimAbs   ty)    = evalAbs ty
evalPrim (PrimSig   ty)    = evalSig ty
evalPrim (PrimQuot  ty)    = evalQuot ty
evalPrim (PrimRem   ty)    = evalRem ty
evalPrim (PrimIDiv  ty)    = evalIDiv ty
evalPrim (PrimMod   ty)    = evalMod ty
evalPrim (PrimBAnd  ty)    = evalBAnd ty
evalPrim (PrimBOr   ty)    = evalBOr ty
evalPrim (PrimBXor  ty)    = evalBXor ty
evalPrim (PrimBNot  ty)    = evalBNot ty
evalPrim (PrimFDiv  ty)    = evalFDiv ty
evalPrim (PrimRecip ty)    = evalRecip ty
evalPrim (PrimLt    ty)    = evalLt ty
evalPrim (PrimGt    ty)    = evalGt ty
evalPrim (PrimLtEq  ty)    = evalLtEq ty
evalPrim (PrimGtEq  ty)    = evalGtEq ty
evalPrim (PrimEq    ty)    = evalEq ty
evalPrim (PrimNEq   ty)    = evalNEq ty
evalPrim (PrimMax   ty)    = evalMax ty
evalPrim (PrimMin   ty)    = evalMin ty
evalPrim PrimLAnd          = evalLAnd
evalPrim PrimLOr           = evalLOr
evalPrim PrimLNot          = evalLNot
evalPrim PrimOrd           = evalOrd
evalPrim PrimChr           = evalChr
evalPrim PrimRoundFloatInt = evalRoundFloatInt
evalPrim PrimTruncFloatInt = evalTruncFloatInt
evalPrim PrimIntFloat      = evalIntFloat


-- Pairing
-- -------

evalPair :: forall s t. (Elem s, Elem t)
        => s {- dummy to fix the type variable -}
        -> t {- dummy to fix the type variable -}
        -> ElemRepr s
        -> ElemRepr t
        -> ElemRepr (s, t)
evalPair _ _ x y = fromElem (toElem x :: s, toElem y :: t)

evalFst :: forall s t. (Elem s, Elem t)
       => s {- dummy to fix the type variable -}
       -> t {- dummy to fix the type variable -}
       -> ElemRepr (s, t)
       -> ElemRepr s
evalFst _ _ xy = let (x, !_) = toElem xy :: (s, t)
                 in fromElem x

evalSnd :: forall s t. (Elem s, Elem t)
       => s {- dummy to fix the type variable -}
       -> t {- dummy to fix the type variable -}
       -> ElemRepr (s, t)
       -> ElemRepr t
evalSnd _ _ xy = let (!_, y) = toElem xy :: (s, t)
                 in fromElem y


-- Implementation of scalar primitives
-- -----------------------------------

evalLAnd :: (Bool, Bool) -> Bool
evalLAnd (!x, !y) = x && y

evalLOr  :: (Bool, Bool) -> Bool
evalLOr (!x, !y) = x || y

evalLNot :: Bool -> Bool
evalLNot x = not x

evalOrd :: Char -> Int
evalOrd = ord

evalChr :: Int -> Char
evalChr =  chr

evalRoundFloatInt :: Float -> Int
evalRoundFloatInt = round

evalTruncFloatInt :: Float -> Int
evalTruncFloatInt = truncate

evalIntFloat :: Int -> Float
evalIntFloat = fromIntegral


-- Extract methods from reified dictionaries
-- 

-- Constant methods of Bounded
-- 

evalMinBound :: BoundedType a -> a
evalMinBound (IntegralBoundedType ty) 
  | IntegralDict <- integralDict ty = minBound
evalMinBound (NonNumBoundedType   ty) 
  | NonNumDict   <- nonNumDict ty   = minBound

evalMaxBound :: BoundedType a -> a
evalMaxBound (IntegralBoundedType ty) 
  | IntegralDict <- integralDict ty = maxBound
evalMaxBound (NonNumBoundedType   ty) 
  | NonNumDict   <- nonNumDict ty   = maxBound

-- Constant method of floating
-- 

evalPi :: FloatingType a -> a
evalPi ty | FloatingDict <- floatingDict ty = pi

-- Methods of Num
-- 

evalAdd :: NumType a -> ((a, a) -> a)
evalAdd (IntegralNumType ty) | IntegralDict <- integralDict ty = uncurry (+)
evalAdd (FloatingNumType ty) | FloatingDict <- floatingDict ty = uncurry (+)

evalSub :: NumType a -> ((a, a) -> a)
evalSub (IntegralNumType ty) | IntegralDict <- integralDict ty = uncurry (-)
evalSub (FloatingNumType ty) | FloatingDict <- floatingDict ty = uncurry (-)

evalMul :: NumType a -> ((a, a) -> a)
evalMul (IntegralNumType ty) | IntegralDict <- integralDict ty = uncurry (*)
evalMul (FloatingNumType ty) | FloatingDict <- floatingDict ty = uncurry (*)

evalNeg :: NumType a -> (a -> a)
evalNeg (IntegralNumType ty) | IntegralDict <- integralDict ty = negate
evalNeg (FloatingNumType ty) | FloatingDict <- floatingDict ty = negate

evalAbs :: NumType a -> (a -> a)
evalAbs (IntegralNumType ty) | IntegralDict <- integralDict ty = abs
evalAbs (FloatingNumType ty) | FloatingDict <- floatingDict ty = abs

evalSig :: NumType a -> (a -> a)
evalSig (IntegralNumType ty) | IntegralDict <- integralDict ty = signum
evalSig (FloatingNumType ty) | FloatingDict <- floatingDict ty = signum

evalQuot :: IntegralType a -> ((a, a) -> a)
evalQuot ty | IntegralDict <- integralDict ty = uncurry quot

evalRem :: IntegralType a -> ((a, a) -> a)
evalRem ty | IntegralDict <- integralDict ty = uncurry rem

evalIDiv :: IntegralType a -> ((a, a) -> a)
evalIDiv ty | IntegralDict <- integralDict ty = uncurry div

evalMod :: IntegralType a -> ((a, a) -> a)
evalMod ty | IntegralDict <- integralDict ty = uncurry mod

evalBAnd :: IntegralType a -> ((a, a) -> a)
evalBAnd ty | IntegralDict <- integralDict ty = uncurry (.&.)

evalBOr :: IntegralType a -> ((a, a) -> a)
evalBOr ty | IntegralDict <- integralDict ty = uncurry (.|.)

evalBXor :: IntegralType a -> ((a, a) -> a)
evalBXor ty | IntegralDict <- integralDict ty = uncurry xor

evalBNot :: IntegralType a -> (a -> a)
evalBNot ty | IntegralDict <- integralDict ty = complement

evalFDiv :: FloatingType a -> ((a, a) -> a)
evalFDiv ty | FloatingDict <- floatingDict ty = uncurry (/)

evalRecip :: FloatingType a -> (a -> a)
evalRecip ty | FloatingDict <- floatingDict ty = recip

evalLt :: ScalarType a -> ((a, a) -> Bool)
evalLt (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry (<)
evalLt (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry (<)
evalLt (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry (<)

evalGt :: ScalarType a -> ((a, a) -> Bool)
evalGt (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry (>)
evalGt (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry (>)
evalGt (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry (>)

evalLtEq :: ScalarType a -> ((a, a) -> Bool)
evalLtEq (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry (<=)
evalLtEq (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry (<=)
evalLtEq (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry (<=)

evalGtEq :: ScalarType a -> ((a, a) -> Bool)
evalGtEq (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry (>=)
evalGtEq (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry (>=)
evalGtEq (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry (>=)

evalEq :: ScalarType a -> ((a, a) -> Bool)
evalEq (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry (==)
evalEq (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry (==)
evalEq (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry (==)

evalNEq :: ScalarType a -> ((a, a) -> Bool)
evalNEq (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry (/=)
evalNEq (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry (/=)
evalNEq (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry (/=)

evalMax :: ScalarType a -> ((a, a) -> a)
evalMax (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry max
evalMax (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry max
evalMax (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry max

evalMin :: ScalarType a -> ((a, a) -> a)
evalMin (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = uncurry min
evalMin (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = uncurry min
evalMin (NonNumScalarType ty) 
  | NonNumDict   <- nonNumDict ty   = uncurry min
