{-# LANGUAGE GADTs, BangPatterns, PatternGuards #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}

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
--  clearly, not on performance.

module Data.Array.Accelerate.Interpreter (

  -- * Interpret an array expression
  run
  
) where

-- standard libraries
import Control.Monad
import Data.Bits
import Data.Char                (chr, ord)

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Delayed
import Data.Array.Accelerate.AST
import qualified Data.Array.Accelerate.Smart       as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar


-- Program execution
-- -----------------

-- Run a complete array program
--
run :: Sugar.Arrays a => Sugar.Acc a -> a
run = Sugar.toArrays . force . evalAcc . Sugar.convertAcc


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
prj ZeroIdx       (Push _   v) = v
prj (SuccIdx idx) (Push val _) = prj idx val
prj _             _            = 
  error "Data.Array.Accelerate.Interpreter: prj: inconsistent valuation"


-- Array expression evaluation
-- ---------------------------

-- Evaluate an open array expression
--
evalOpenAcc :: Delayable a => OpenAcc aenv a -> Val aenv -> Delayed a

evalOpenAcc (Let acc1 acc2) aenv 
  = let !arr1 = force $ evalOpenAcc acc1 aenv
    in evalOpenAcc acc2 (aenv `Push` arr1)

evalOpenAcc (Let2 acc1 acc2) aenv 
  = let (!arr1, !arr2) = force $ evalOpenAcc acc1 aenv
    in evalOpenAcc acc2 (aenv `Push` arr1 `Push` arr2)

evalOpenAcc (Avar idx) aenv = delay $ prj idx aenv

evalOpenAcc (Use arr) _aenv = delay arr

evalOpenAcc (Unit e) aenv = unitOp (evalExp e aenv)

evalOpenAcc (Reshape e acc) aenv 
  = reshapeOp (evalExp e aenv) (evalOpenAcc acc aenv)

evalOpenAcc (Replicate sliceIndex slix acc) aenv
  = replicateOp sliceIndex (evalExp slix aenv) (evalOpenAcc acc aenv)
  
evalOpenAcc (Index sliceIndex acc slix) aenv
  = indexOp sliceIndex (evalOpenAcc acc aenv) (evalExp slix aenv)

evalOpenAcc (Map f acc) aenv = mapOp (evalFun f aenv) (evalOpenAcc acc aenv)

evalOpenAcc (ZipWith f acc1 acc2) aenv
  = zipWithOp (evalFun f aenv) (evalOpenAcc acc1 aenv) (evalOpenAcc acc2 aenv)

evalOpenAcc (Fold f e acc) aenv
  = foldOp (evalFun f aenv) (evalExp e aenv) (evalOpenAcc acc aenv)

evalOpenAcc (Scan f e acc) aenv
  = scanOp (evalFun f aenv) (evalExp e aenv) (evalOpenAcc acc aenv)

evalOpenAcc (Permute f dftAcc p acc) aenv
  = permuteOp (evalFun f aenv) (evalOpenAcc dftAcc aenv) 
              (evalFun p aenv) (evalOpenAcc acc aenv)

evalOpenAcc (Backpermute e p acc) aenv
  = backpermuteOp (evalExp e aenv) (evalFun p aenv) (evalOpenAcc acc aenv)

-- Evaluate a closed array expressions
--
evalAcc :: Delayable a => Acc a -> Delayed a
evalAcc acc = evalOpenAcc acc Empty


-- Array primitives
-- ----------------

unitOp :: ArrayElem e => e -> Delayed (Scalar e)
unitOp e = DelayedArray {shapeDA = (), repfDA = const e}

reshapeOp :: Ix dim => dim -> Delayed (Array dim' e) -> Delayed (Array dim e)
reshapeOp newShape darr@(DelayedArray {shapeDA = oldShape})
  | size newShape == size oldShape
  = let Array _ adata = force darr
    in 
    delay $ Array newShape adata
  | otherwise 
  = error "Data.Array.Accelerate.Interpreter.reshape: shape mismatch"

replicateOp :: Ix dim
            => SliceIndex slix sl co dim 
            -> slix 
            -> Delayed (Array sl e)
            -> Delayed (Array dim e)
replicateOp sliceIndex slix (DelayedArray sh pf)
  = DelayedArray sh' (pf . pf')
  where
    (sh', pf') = extend sliceIndex slix sh
    
    extend :: SliceIndex slix sl co dim
           -> slix 
           -> sl
           -> (dim, dim -> sl)
    extend SliceNil                ()         ()       = ((), const ())
    extend (SliceAll sliceIndex)   (slix, ()) (sl, sz) 
      = let (dim', pf') = extend sliceIndex slix sl
        in
        ((dim', sz), \(ix, i) -> (pf' ix, i))
    extend (SliceFixed sliceIndex) (slix, sz) sl
      = let (dim', pf') = extend sliceIndex slix sl
        in
        ((dim', sz), \(ix, _) -> pf' ix)
    
indexOp :: Ix sl
        => SliceIndex slix sl co dim 
        -> Delayed (Array dim e)
        -> slix 
        -> Delayed (Array sl e)
indexOp sliceIndex (DelayedArray sh pf) slix 
  = DelayedArray sh' (pf . pf')
  where
    (sh', pf') = restrict sliceIndex slix sh

    restrict :: SliceIndex slix sl co dim
             -> slix
             -> dim
             -> (sl, sl -> dim)
    restrict SliceNil () () = ((), const ())
    restrict (SliceAll sliceIndex) (slix, ()) (sh, sz)
      = let (sl', pf') = restrict sliceIndex slix sh
        in
        ((sl', sz), \(ix, i) -> (pf' ix, i))
    restrict (SliceFixed sliceIndex) (slix, i) (sh, sz)
      | i < sz
      = let (sl', pf') = restrict sliceIndex slix sh
        in
        (sl', \ix -> (pf' ix, i))
      | otherwise = error "Index out of bounds"

mapOp :: ArrayElem e' 
      => (e -> e') 
      -> Delayed (Array dim e) 
      -> Delayed (Array dim e')
mapOp f (DelayedArray sh rf) = DelayedArray sh (f . rf)

zipWithOp :: ArrayElem e3
          => (e1 -> e2 -> e3) 
          -> Delayed (Array dim e1) 
          -> Delayed (Array dim e2) 
          -> Delayed (Array dim e3)
zipWithOp f (DelayedArray sh1 rf1) (DelayedArray sh2 rf2) 
  = DelayedArray (sh1 `intersect` sh2) (\ix -> f (rf1 ix) (rf2 ix))

foldOp :: (e -> e -> e)
       -> e
       -> Delayed (Array dim e)
       -> Delayed (Scalar e)
foldOp f e (DelayedArray sh rf)
  = unitOp $ iter sh rf f e

scanOp :: (e -> e -> e)
       -> e
       -> Delayed (Vector e)
       -> Delayed (Vector e, Scalar e)
scanOp f e (DelayedArray sh rf)
  = DelayedPair (delay $ adata `seq` Array sh adata) (unitOp final)
  where
    n = size sh
    --
    (adata, final) = runArrayData $ do
                       arr <- newArrayData n
                       final <- traverse arr 0 e
                       return (arr, final)
    traverse arr i v
      | i >= n    = return v
      | otherwise = do
                      writeArrayData arr i v
                      traverse arr (i + 1) (f v (rf ((), i)))
    
permuteOp :: (e -> e -> e)
          -> Delayed (Array dim' e)
          -> (dim -> dim')
          -> Delayed (Array dim e)
          -> Delayed (Array dim' e)
permuteOp f (DelayedArray dftsSh dftsPf) p (DelayedArray sh pf)
  = delay $ adata `seq` Array dftsSh adata
  where 
    (adata, _) 
      = runArrayData $ do

            -- new array in target dimension
          arr <- newArrayData (size dftsSh)

            -- initialise it with the default values
          let write ix = writeArrayData arr (index dftsSh ix) (dftsPf ix)      
          iter dftsSh write (>>) (return ())

            -- traverse the source dimension and project each element into
            -- the target dimension (where it gets combined with the current
            -- default)
          let update ix = do
                            let target = p ix
                            unless (target == ignore) $ do
                              let i = index dftsSh target
                              e <- readArrayData arr i
                              writeArrayData arr i (pf ix `f` e) 
          iter sh update (>>) (return ())
          
            -- return the updated array
          return (arr, undefined)

backpermuteOp :: Ix dim'
              => dim'
              -> (dim' -> dim)
              -> Delayed (Array dim e)
              -> Delayed (Array dim' e)
backpermuteOp sh' p (DelayedArray _sh rf)
  = DelayedArray sh' (rf . p)


-- Expression evaluation
-- ---------------------

-- Evaluate open function
--
evalOpenFun :: OpenFun env aenv t -> Val env -> Val aenv -> t
evalOpenFun (Body e) env aenv = evalOpenExp e env aenv
evalOpenFun (Lam f)  env aenv = \x -> evalOpenFun f (env `Push` x) aenv

-- Evaluate a closed function
--
evalFun :: Fun aenv t -> Val aenv -> t
evalFun f aenv = evalOpenFun f Empty aenv

-- Evaluate an open expression
--
-- NB: The implementation of 'IndexScalar' and 'Shape' demonstrate clearly why
--     array expressions must be hoisted out of scalar expressions before code
--     execution.  If these operations are in the body of a function that
--     gets mapped over an array, the array argument would be forced many times
--     leading to a large amount of wasteful recomputation.
--  
evalOpenExp :: OpenExp env aenv a -> Val env -> Val aenv -> a

evalOpenExp (Var idx) env _ = prj idx env
  
evalOpenExp (Const c) _ _ = Sugar.fromElem c

evalOpenExp (Pair ds dt e1 e2) env aenv 
  = evalPair ds dt (evalOpenExp e1 env aenv) (evalOpenExp e2 env aenv)

evalOpenExp (Fst ds dt e) env aenv 
  = evalFst ds dt (evalOpenExp e env aenv)

evalOpenExp (Snd ds dt e) env aenv 
  = evalSnd ds dt (evalOpenExp e env aenv)

evalOpenExp (Cond c t e) env aenv 
  = if Sugar.toElem (evalOpenExp c env aenv) 
    then evalOpenExp t env aenv
    else evalOpenExp e env aenv

evalOpenExp (PrimConst c) _ _ = Sugar.fromElem $ evalPrimConst c

evalOpenExp (PrimApp p arg) env aenv 
  = Sugar.fromElem $ evalPrim p (Sugar.toElem (evalOpenExp arg env aenv))

evalOpenExp (IndexScalar acc ix) env aenv 
  = let ix' = evalOpenExp ix env aenv
    in
    case evalOpenAcc acc aenv of
      DelayedArray sh pf -> index sh ix' `seq` pf ix'
                            -- FIXME: This is ugly, but (possibly) needed to
                            --       ensure bounds checking

evalOpenExp (Shape acc) _ aenv 
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
evalPrim PrimBoolToInt     = evalBoolToInt


-- Pairing
-- -------

evalPair :: forall s t. (Sugar.Elem s, Sugar.Elem t)
        => s {- dummy to fix the type variable -}
        -> t {- dummy to fix the type variable -}
        -> Sugar.ElemRepr s
        -> Sugar.ElemRepr t
        -> Sugar.ElemRepr (s, t)
evalPair _ _ x y = Sugar.fromElem (Sugar.toElem x :: s, Sugar.toElem y :: t)

evalFst :: forall s t. (Sugar.Elem s, Sugar.Elem t)
       => s {- dummy to fix the type variable -}
       -> t {- dummy to fix the type variable -}
       -> Sugar.ElemRepr (s, t)
       -> Sugar.ElemRepr s
evalFst _ _ xy = let (x, !_) = Sugar.toElem xy :: (s, t)
                 in Sugar.fromElem x

evalSnd :: forall s t. (Sugar.Elem s, Sugar.Elem t)
       => s {- dummy to fix the type variable -}
       -> t {- dummy to fix the type variable -}
       -> Sugar.ElemRepr (s, t)
       -> Sugar.ElemRepr t
evalSnd _ _ xy = let (!_, y) = Sugar.toElem xy :: (s, t)
                 in Sugar.fromElem y


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

evalBoolToInt :: Bool -> Int
evalBoolToInt = fromEnum


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
