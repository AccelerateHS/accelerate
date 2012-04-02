{-# LANGUAGE BangPatterns, GADTs #-}

module Data.Array.Accelerate.SimpleConverter 
--       ()
       where

-- standard libraries
import Control.Monad
import Control.Applicative
-- import Control.Monad.ST                            (ST)
import Data.Bits
import Data.Char                                   (chr, ord)
import Prelude                                     hiding (sum)
import Debug.Trace

import Control.Monad.State.Strict

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation  hiding (sliceIndex)
import Data.Array.Accelerate.Array.Sugar (
  Z(..), (:.)(..), Array(..), Scalar, Vector, Segments)
import Data.Array.Accelerate.Array.Delayed
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Smart       as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar

import qualified Data.Array.Accelerate.SimpleAST as S

-- TEMP:
import qualified Data.Array.Accelerate.Language as Lang
-- import qualified Data.Array.Accelerate as Acc

-- #include "accelerate.h"


-- | Convert the sophisticate Accelerate-internal AST representation
--   into something very simple for external consumption.
convert :: Arrays a => Sugar.Acc a -> S.Exp
convert = runEnvM . evalAcc . Sugar.convertAcc
  -- force . 

-- convertAccFun1 = 

--------------------------------------------------------------------------------

-- dotpAcc :: Vector Float -> Vector Float -> Sugar.Acc (Sugar.Scalar Float)
dotpAcc :: Sugar.Acc (Sugar.Scalar Float)
dotpAcc 
  = let
        xs' = Lang.use $ error "xs: let's not REALLY use this array"
        ys' = Lang.use $ error "ys: let's not REALLY use this array"
--         xs' = error "let's not REALLY use this array"
--         ys' = error "let's not REALLY use this array"
--      xs' = Sugar.use xs
--      ys' = Sugar.use ys
    in
     (Lang.zipWith (*) xs' ys')
--    Lang.fold (+) 0 (Lang.zipWith (*) xs' ys')


{-
run :: Int -> IO (() -> UArray () Float, () -> Acc (Scalar Float))
run n = withSystemRandom $ \gen -> do
  v1  <- randomUArrayR (-1,1) gen n
  v2  <- randomUArrayR (-1,1) gen n
  v1' <- convertUArray v1
  v2' <- convertUArray v2
  --
--  return (run_ref v1 v2, run_acc v1' v2')
    return (dotpAcc v1' v2')
  where
--    {-# NOINLINE run_ref #-}
--    run_ref xs ys () = dotpRef xs ys
--    run_acc xs ys () = dotpAcc xs ys
-}

t1 :: S.Exp
t1 = convert dotpAcc


--------------------------------------------------------------------------------

-- We use a simple state monad for keeping track of the environment
type EnvM = State (SimpleEnv,Counter) 
type SimpleEnv = [S.Var]
type Counter = Int

runEnvM m = evalState m ([],0)

-- Evaluate a sub-branch in an extended environment.
-- Returns the name of the fresh variable as well as the result:
withExtendedEnv :: String -> (EnvM b) -> EnvM (S.Var, b )
withExtendedEnv basename branch = do 
  (env,cnt) <- get 
  let newsym = S.var $ basename ++ show cnt
  put (newsym:env, cnt+1) 
  b <- branch
  -- We keep counter-increments from the sub-branch, but NOT the extended env:
  (_,cnt2) <- get 
  put (env,cnt2)
  return (newsym, b)

-- Look up a de bruijn index in the environment:
envLookup :: Int -> EnvM S.Var
envLookup i = do (env,_) <- get
                 if length env > i
                  then return (env !! i)
                  else error$ "Environment did not contain an element "++show i++" : "++show env

blah a b c = a <$> b <*> c


--------------------------------------------------------------------------------
-- Accelerate Array-level Expressions

evalAcc :: Delayable a => Acc a -> EnvM S.Exp
evalAcc acc = evalOpenAcc acc 

evalOpenAcc :: Delayable a => OpenAcc aenv a -> EnvM S.Exp
evalOpenAcc (OpenAcc acc) = evalPreOpenAcc acc 

evalPreOpenAcc :: Delayable a => PreOpenAcc OpenAcc aenv a -> EnvM S.Exp

-- The environment argument is used to convert de Bruijn indices to vars:
evalPreOpenAcc e = 
  case e of 
    Let acc1 acc2 -> 
       do (v,a2) <- withExtendedEnv "a"$ 
                    evalOpenAcc acc2 
	  a1     <- evalOpenAcc acc1
          return$ S.Let v a1 a2

--    Avar idx -> return$ S.Vr (S.var$ show$ idxToInt idx)
    Avar idx -> S.Vr <$> envLookup (idxToInt idx)

    ZipWith f acc1 acc2 -> S.ZipWith <$> convertFun f
                                     <*> evalOpenAcc acc1
                                     <*> evalOpenAcc acc2

    Let2 acc1 acc2 -> undefined
    PairArrays acc1 acc2 -> undefined

    
    Apply (Alam (Abody funAcc)) acc -> undefined
--   = let !arr = force $ evalOpenAcc acc aenv
--     in evalOpenAcc funAcc (Empty `Push` arr)
    Apply _afun _acc -> error "This case is impossible"

    Acond cond acc1 acc2 -> S.Cond <$> evalExp cond 
                                   <*> evalOpenAcc acc1 
                                   <*> evalOpenAcc acc2
    -- This is a real live array:
    Use arr -> return$ S.Use

    Unit e -> error "unit"
    Generate sh f -> error "generate"
    Reshape e acc -> error "reshape"
    Replicate sliceIndex slix acc -> error "replicate"
    Index sliceIndex acc slix  -> error "Index"
    Map f acc -> error "map"
    Fold     f e acc -> error "fold"
    Fold1    f   acc -> error "fold1"
    FoldSeg  f e acc1 acc2 -> error "foldseg"
    Fold1Seg f   acc1 acc2 -> error "fold1seg"
    Scanl  f e acc -> error "scanl"
    Scanl' f e acc -> error "scanl'"
    Scanl1 f   acc -> error "scanl1"
    Scanr  f e acc -> error "scanr"
    Scanr' f e acc -> error "scanr'"
    Scanr1 f   acc -> error "scanr1"
    Permute f dftAcc p acc -> error "permute"
    Backpermute e p acc -> error "backperm"
    Stencil  sten bndy acc -> error "stencil"
    Stencil2 sten bndy1 acc1 bndy2 acc2 -> error "stencil2"

--    a -> error $ "Unmatch PreOpenAcc: "++ show a

--------------------------------------------------------------------------------
-- Accelerate Scalar Expressions
    
-- Evaluate a closed expression
evalExp :: Exp aenv t -> EnvM S.Exp
evalExp e = evalOpenExp e 

evalOpenExp :: OpenExp env aenv a -> EnvM S.Exp
evalOpenExp e = 
  case e of 
--    Var idx -> return$ S.Vr (S.var$ "TODO__"++ show (idxToInt idx))
    Var idx -> S.Vr <$> envLookup (idxToInt idx)
    PrimApp p arg -> convertPrimApp p arg

    Tuple tup -> convertTuple tup
--    Tuple NilTup -> S.Tuple []
    -- Tuple (SnocTup tup e) -> 
    --   case evalOpenExp tup of 
    --     S.Tuple ls -> S.Tuple (evalOpenExp e : ls)
  
-- evalOpenExp (Const c) _ _ = Sugar.toElt c

-- evalOpenExp (Tuple tup) env aenv 
--   = toTuple $ convertTuple tup env aenv

-- evalOpenExp (Prj idx e) env aenv 
--   = evalPrj idx (fromTuple $ evalOpenExp e env aenv)

-- evalOpenExp IndexNil _env _aenv 
--   = Z

-- evalOpenExp (IndexCons sh i) env aenv 
--   = evalOpenExp sh env aenv :. evalOpenExp i env aenv

-- evalOpenExp (IndexHead ix) env aenv 
--   = case evalOpenExp ix env aenv of _:.h -> h

-- evalOpenExp (IndexTail ix) env aenv 
--   = case evalOpenExp ix env aenv of t:._ -> t

-- evalOpenExp (IndexAny) _ _
--   = Sugar.Any

-- evalOpenExp (Cond c t e) env aenv 
--   = if evalOpenExp c env aenv
--     then evalOpenExp t env aenv
--     else evalOpenExp e env aenv

-- evalOpenExp (PrimConst c) _ _ = evalPrimConst c


-- evalOpenExp (IndexScalar acc ix) env aenv 
--   = case evalOpenAcc acc aenv of
--       DelayedArray sh pf -> 
--         let ix' = Sugar.fromElt $ evalOpenExp ix env aenv
--         in
--         index sh ix' `seq` (Sugar.toElt $ pf ix')
--                               -- FIXME: This is ugly, but (possibly) needed to
--                               --       ensure bounds checking

-- evalOpenExp (Shape acc) _ aenv 
--   = case force $ evalOpenAcc acc aenv of
--       Array sh _ -> Sugar.toElt sh

-- evalOpenExp (Size acc) _ aenv 
--   = case force $ evalOpenAcc acc aenv of
--       Array sh _ -> size sh

    _ -> error$ "unhandled exp "++ show e


-- convertTuple :: Tuple (OpenExp env aenv) t -> [S.Exp]
-- convertTuple NilTup = []
-- convertTuple (tup `SnocTup` e) = 
--     case evalOpenExp e of 
--       S.Lam _ _ -> error "hmm"
--       se -> error$ "FINISHME: This is what we got back for a tuple: "++show se
--   where 
--    sexp = evalOpenExp e

-- convertTuple e = go (evalOpenExp e)
--  where 
--   go (Lam _ _) = undefined


-- Convert a tuple expression to our simpler Tuple representation (containing a list):
-- convertTuple :: Tuple (PreOpenExp acc env aenv) t' -> S.Exp
convertTuple :: Tuple (PreOpenExp OpenAcc env aenv) t' -> EnvM S.Exp
convertTuple NilTup = return$ S.Tuple []
convertTuple (SnocTup tup e) = 
    do e' <- evalOpenExp e
       tup' <- convertTuple tup
       case tup' of 
         S.Tuple ls -> return$ S.Tuple$ ls ++ [e']

convertTupleExp :: PreOpenExp OpenAcc t t1 t2 -> EnvM [S.Exp]
convertTupleExp e = do
  e' <- evalOpenExp e
  case e' of 
    S.Tuple ls -> return ls
    se -> error$ "convertTupleExp: expected a tuple expression, received:\n  "++ show se

--------------------------------------------------------------------------------
-- Accelerate Primitives:    

convertPrimApp :: (Sugar.Elt a, Sugar.Elt b)
               => PrimFun (a -> b) -> PreOpenExp OpenAcc env aenv a
               -> EnvM S.Exp
convertPrimApp p arg = 
  do args' <- convertTupleExp arg
     return$ S.PrimApp (op p) args'
 where 
   op p = 
    case p of 
      PrimAdd ty -> error "add"
      PrimMul ty -> S.NP S.Mul
      _ -> error$ "primapp not handled yet: "++show (PrimApp p arg)

numty nt = 
  case nt of 
    IntegralNumType ty -> undefined
    FloatingNumType ty -> undefined

--------------------------------------------------------------------------------

-- Evaluate open function
convertFun :: OpenFun env aenv t -> EnvM S.Exp
convertFun (Body b) = evalOpenExp b
convertFun (Lam f)  = fmap snd $ 
                 withExtendedEnv "v" $ do
                   v <- envLookup 0
		   S.Lam v <$> convertFun f
