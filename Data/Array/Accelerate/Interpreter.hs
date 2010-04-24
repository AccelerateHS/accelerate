{-# LANGUAGE GADTs, BangPatterns, PatternGuards #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}
-- |
-- Module      : Data.Array.Accelerate.Interpreter
-- Copyright   : [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This interpreter is meant to be a reference implementation of the semantics
-- of the embedded array language.  The emphasis is on defining the semantics
-- clearly, not on performance.

module Data.Array.Accelerate.Interpreter (

  -- * Interpret an array expression
  Arrays, run
  
) where

-- standard libraries
import Control.Monad
import Data.Bits
import Data.Char                (chr, ord)

-- friends
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


-- Program execution
-- -----------------

-- |Characterises the types that may be returned when running an array program.
--
class Delayable as => Arrays as
  
instance Arrays ()  
instance Arrays (Array dim e)
instance (Arrays as1, Arrays as2) => Arrays (as1, as2)

-- |Run a complete embedded array program using the reference interpreter.
--
run :: Arrays a => Sugar.Acc a -> a
run = force . evalAcc . Sugar.convertAcc


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

evalOpenAcc (FoldSeg f e acc1 acc2) aenv
  = foldSegOp (evalFun f aenv) (evalExp e aenv) 
              (evalOpenAcc acc1 aenv) (evalOpenAcc acc2 aenv)

evalOpenAcc (Scan f e acc) aenv
  = scanOp (evalFun f aenv) (evalExp e aenv) (evalOpenAcc acc aenv)

evalOpenAcc (Scanr f e acc) aenv
  = scanrOp (evalFun f aenv) (evalExp e aenv) (evalOpenAcc acc aenv)

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

unitOp :: Sugar.Elem e => e -> Delayed (Scalar e)
unitOp e = DelayedArray {shapeDA = (), repfDA = const (Sugar.fromElem e)}

reshapeOp :: Sugar.Ix dim 
          => dim -> Delayed (Array dim' e) -> Delayed (Array dim e)
reshapeOp newShape darr@(DelayedArray {shapeDA = oldShape})
  | Sugar.size newShape == size oldShape
  = let Array _ adata = force darr
    in 
    delay $ Array (Sugar.fromElem newShape) adata
  | otherwise 
  = error "Data.Array.Accelerate.Interpreter.reshape: shape mismatch"

replicateOp :: (Sugar.Ix dim, Sugar.Elem slix)
            => SliceIndex (Sugar.ElemRepr slix) 
                          (Sugar.ElemRepr sl) 
                          co
                          (Sugar.ElemRepr dim)
            -> slix 
            -> Delayed (Array sl e)
            -> Delayed (Array dim e)
replicateOp sliceIndex slix (DelayedArray sh pf)
  = DelayedArray sh' (pf . pf')
  where
    (sh', pf') = extend sliceIndex (Sugar.fromElem slix) sh
    
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
    
indexOp :: (Sugar.Ix sl, Sugar.Elem slix)
        => SliceIndex (Sugar.ElemRepr slix) 
                      (Sugar.ElemRepr sl) 
                      co
                      (Sugar.ElemRepr dim)
        -> Delayed (Array dim e)
        -> slix 
        -> Delayed (Array sl e)
indexOp sliceIndex (DelayedArray sh pf) slix 
  = DelayedArray sh' (pf . pf')
  where
    (sh', pf') = restrict sliceIndex (Sugar.fromElem slix) sh

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

mapOp :: Sugar.Elem e' 
      => (e -> e') 
      -> Delayed (Array dim e) 
      -> Delayed (Array dim e')
mapOp f (DelayedArray sh rf) = DelayedArray sh (Sugar.sinkFromElem f . rf)

zipWithOp :: Sugar.Elem e3
          => (e1 -> e2 -> e3) 
          -> Delayed (Array dim e1) 
          -> Delayed (Array dim e2) 
          -> Delayed (Array dim e3)
zipWithOp f (DelayedArray sh1 rf1) (DelayedArray sh2 rf2) 
  = DelayedArray (sh1 `intersect` sh2) 
                 (\ix -> (Sugar.sinkFromElem2 f) (rf1 ix) (rf2 ix))

foldOp :: (e -> e -> e)
       -> e
       -> Delayed (Array dim e)
       -> Delayed (Scalar e)
foldOp f e (DelayedArray sh rf)
  = unitOp $ 
      Sugar.toElem (iter sh rf (Sugar.sinkFromElem2 f) (Sugar.fromElem e))

foldSegOp :: forall e.
             (e -> e -> e)
          -> e
          -> Delayed (Vector e)
          -> Delayed Segments
          -> Delayed (Vector e)
foldSegOp f e (DelayedArray _sh rf) seg@(DelayedArray shSeg rfSeg)
  = delay arr
  where
    DelayedPair (DelayedArray _shSeg rfStarts) _ = scanOp (+) 0 seg
    arr = Sugar.newArray (Sugar.toElem shSeg) foldOne
    --
    foldOne :: Sugar.DIM1 -> e
    foldOne i = let
                  start = (Sugar.liftToElem rfStarts) i
                  len   = (Sugar.liftToElem rfSeg) i
              in
              fold e start (start + len)
    --
    fold :: e -> Sugar.DIM1 -> Sugar.DIM1 -> e
    fold v j end
      | j >= end  = v
      | otherwise = fold (f v ((Sugar.liftToElem rf) j)) (j + 1) end

scanOp :: (e -> e -> e)
       -> e
       -> Delayed (Vector e)
       -> Delayed (Vector e, Scalar e)
scanOp f e (DelayedArray sh rf)
  = DelayedPair (delay $ adata `seq` Array sh adata) 
                (unitOp (Sugar.toElem final))
  where
    n  = size sh
    f' = Sugar.sinkFromElem2 f
    --
    (adata, final) = runArrayData $ do
                       arr   <- newArrayData n
                       final <- traverse arr 0 (Sugar.fromElem e)
                       return (arr, final)
    traverse arr i v
      | i >= n    = return v
      | otherwise = do
                      writeArrayData arr i v
                      traverse arr (i + 1) (f' v (rf ((), i)))

scanrOp :: (e -> e -> e)
        -> e
        -> Delayed (Vector e)
        -> Delayed (Vector e, Scalar e)
scanrOp f e (DelayedArray sh rf)
  = DelayedPair (delay $ adata `seq` Array sh adata)
                (unitOp (Sugar.toElem final))
  where
    n  = size sh
    f' = Sugar.sinkFromElem2 f
    --
    (adata, final) = runArrayData $ do
                       arr   <- newArrayData n
                       final <- traverse arr (n-1) (Sugar.fromElem e)
                       return (arr, final)
    traverse arr i v
      | i < 0     = return v
      | otherwise = do
                      writeArrayData arr i v
                      traverse arr (i - 1) (f' v (rf ((), i)))

permuteOp :: (e -> e -> e)
          -> Delayed (Array dim' e)
          -> (dim -> dim')
          -> Delayed (Array dim e)
          -> Delayed (Array dim' e)
permuteOp f (DelayedArray dftsSh dftsPf) p (DelayedArray sh pf)
  = delay $ adata `seq` Array dftsSh adata
  where 
    f' = Sugar.sinkFromElem2 f
    --
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
                            let target = (Sugar.sinkFromElem p) ix
                            unless (target == ignore) $ do
                              let i = index dftsSh target
                              e <- readArrayData arr i
                              writeArrayData arr i (pf ix `f'` e) 
          iter sh update (>>) (return ())
          
            -- return the updated array
          return (arr, undefined)

backpermuteOp :: Sugar.Ix dim'
              => dim'
              -> (dim' -> dim)
              -> Delayed (Array dim e)
              -> Delayed (Array dim' e)
backpermuteOp sh' p (DelayedArray _sh rf)
  = DelayedArray (Sugar.fromElem sh') (rf . Sugar.sinkFromElem p)


-- Expression evaluation
-- ---------------------

-- Evaluate open function
--
evalOpenFun :: OpenFun env aenv t -> Val env -> Val aenv -> t
evalOpenFun (Body e) env aenv = evalOpenExp e env aenv
evalOpenFun (Lam f)  env aenv 
  = \x -> evalOpenFun f (env `Push` Sugar.fromElem x) aenv

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

evalOpenExp (Var idx) env _ = Sugar.toElem $ prj idx env
  
evalOpenExp (Const c) _ _ = Sugar.toElem c

evalOpenExp (Tuple tup) env aenv 
  = toTuple $ evalTuple tup env aenv

evalOpenExp (Prj idx e) env aenv 
  = evalPrj idx (fromTuple $ evalOpenExp e env aenv)

evalOpenExp (Cond c t e) env aenv 
  = if evalOpenExp c env aenv
    then evalOpenExp t env aenv
    else evalOpenExp e env aenv

evalOpenExp (PrimConst c) _ _ = evalPrimConst c

evalOpenExp (PrimApp p arg) env aenv 
  = evalPrim p (evalOpenExp arg env aenv)

evalOpenExp (IndexScalar acc ix) env aenv 
  = case evalOpenAcc acc aenv of
      DelayedArray sh pf -> 
        let ix' = Sugar.fromElem $ evalOpenExp ix env aenv
        in
        index sh ix' `seq` (Sugar.toElem $ pf ix')
                              -- FIXME: This is ugly, but (possibly) needed to
                              --       ensure bounds checking

evalOpenExp (Shape acc) _ aenv 
  = case force $ evalOpenAcc acc aenv of
      Array sh _ -> Sugar.toElem sh

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
evalPrim (PrimAdd         ty)   = evalAdd ty
evalPrim (PrimSub         ty)   = evalSub ty
evalPrim (PrimMul         ty)   = evalMul ty
evalPrim (PrimNeg         ty)   = evalNeg ty
evalPrim (PrimAbs         ty)   = evalAbs ty
evalPrim (PrimSig         ty)   = evalSig ty
evalPrim (PrimQuot        ty)   = evalQuot ty
evalPrim (PrimRem         ty)   = evalRem ty
evalPrim (PrimIDiv        ty)   = evalIDiv ty
evalPrim (PrimMod         ty)   = evalMod ty
evalPrim (PrimBAnd        ty)   = evalBAnd ty
evalPrim (PrimBOr         ty)   = evalBOr ty
evalPrim (PrimBXor        ty)   = evalBXor ty
evalPrim (PrimBNot        ty)   = evalBNot ty
evalPrim (PrimBShift      ty)   = evalBShift ty
evalPrim (PrimBRotate     ty)   = evalBRotate ty
evalPrim (PrimFDiv        ty)   = evalFDiv ty
evalPrim (PrimRecip       ty)   = evalRecip ty
evalPrim (PrimSin         ty)   = evalSin ty
evalPrim (PrimCos         ty)   = evalCos ty
evalPrim (PrimTan         ty)   = evalTan ty
evalPrim (PrimAsin        ty)   = evalAsin ty
evalPrim (PrimAcos        ty)   = evalAcos ty
evalPrim (PrimAtan        ty)   = evalAtan ty
evalPrim (PrimAsinh       ty)   = evalAsinh ty
evalPrim (PrimAcosh       ty)   = evalAcosh ty
evalPrim (PrimAtanh       ty)   = evalAtanh ty
evalPrim (PrimExpFloating ty)   = evalExpFloating ty
evalPrim (PrimSqrt        ty)   = evalSqrt ty
evalPrim (PrimLog         ty)   = evalLog ty
evalPrim (PrimFPow        ty)   = evalFPow ty
evalPrim (PrimLogBase     ty)   = evalLogBase ty
evalPrim (PrimLt          ty)   = evalLt ty
evalPrim (PrimGt          ty)   = evalGt ty
evalPrim (PrimLtEq        ty)   = evalLtEq ty
evalPrim (PrimGtEq        ty)   = evalGtEq ty
evalPrim (PrimEq          ty)   = evalEq ty
evalPrim (PrimNEq         ty)   = evalNEq ty
evalPrim (PrimMax         ty)   = evalMax ty
evalPrim (PrimMin         ty)   = evalMin ty
evalPrim PrimLAnd               = evalLAnd
evalPrim PrimLOr                = evalLOr
evalPrim PrimLNot               = evalLNot
evalPrim PrimOrd                = evalOrd
evalPrim PrimChr                = evalChr
evalPrim PrimRoundFloatInt      = evalRoundFloatInt
evalPrim PrimTruncFloatInt      = evalTruncFloatInt
evalPrim PrimIntFloat           = evalIntFloat
evalPrim PrimBoolToInt          = evalBoolToInt


-- Tuple construction and projection
-- ---------------------------------

evalTuple :: Tuple (OpenExp env aenv) t -> Val env -> Val aenv -> t
evalTuple NilTup            _env _aenv = ()
evalTuple (tup `SnocTup` e) env  aenv  = (evalTuple tup env aenv, 
                                          evalOpenExp e env aenv)

evalPrj :: TupleIdx t e -> t -> e
evalPrj ZeroTupIdx       (!_, v)   = v
evalPrj (SuccTupIdx idx) (tup, !_) = evalPrj idx tup
  -- FIXME: Strictly speaking, we ought to force all components of a tuples;
  --        not only those that we happen to encounter during the recursive
  --        walk.


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

evalSin :: FloatingType a -> (a -> a)
evalSin ty | FloatingDict <- floatingDict ty = sin

evalCos :: FloatingType a -> (a -> a)
evalCos ty | FloatingDict <- floatingDict ty = cos

evalTan :: FloatingType a -> (a -> a)
evalTan ty | FloatingDict <- floatingDict ty = tan

evalAsin :: FloatingType a -> (a -> a)
evalAsin ty | FloatingDict <- floatingDict ty = asin

evalAcos :: FloatingType a -> (a -> a)
evalAcos ty | FloatingDict <- floatingDict ty = acos

evalAtan :: FloatingType a -> (a -> a)
evalAtan ty | FloatingDict <- floatingDict ty = atan

evalAsinh :: FloatingType a -> (a -> a)
evalAsinh ty | FloatingDict <- floatingDict ty = asinh

evalAcosh :: FloatingType a -> (a -> a)
evalAcosh ty | FloatingDict <- floatingDict ty = acosh

evalAtanh :: FloatingType a -> (a -> a)
evalAtanh ty | FloatingDict <- floatingDict ty = atanh

evalExpFloating :: FloatingType a -> (a -> a)
evalExpFloating ty | FloatingDict <- floatingDict ty = exp

evalSqrt :: FloatingType a -> (a -> a)
evalSqrt ty | FloatingDict <- floatingDict ty = sqrt

evalLog :: FloatingType a -> (a -> a)
evalLog ty | FloatingDict <- floatingDict ty = log

evalFPow :: FloatingType a -> ((a, a) -> a)
evalFPow ty | FloatingDict <- floatingDict ty = uncurry (**)

evalLogBase :: FloatingType a -> ((a, a) -> a)
evalLogBase ty | FloatingDict <- floatingDict ty = uncurry logBase


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

evalBShift :: IntegralType a -> ((a, Int) -> a)
evalBShift ty | IntegralDict <- integralDict ty = uncurry shift

evalBRotate :: IntegralType a -> ((a, Int) -> a)
evalBRotate ty | IntegralDict <- integralDict ty = uncurry rotate

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
