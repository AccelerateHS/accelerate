{-# LANGUAGE BangPatterns, GADTs #-}

module Data.Array.Accelerate.SimpleConverter 
--       ()
       where

-- standard libraries
import Control.Monad
import Control.Monad.ST                            (ST)
import Data.Bits
import Data.Char                                   (chr, ord)
import Prelude                                     hiding (sum)

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
convert = evalAcc . Sugar.convertAcc
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
-- Accelerate Array-level Expressions

evalAcc :: Delayable a => Acc a -> S.Exp
evalAcc acc = evalOpenAcc acc -- Empty

evalOpenAcc :: Delayable a => OpenAcc aenv a -> S.Exp
evalOpenAcc (OpenAcc acc) = evalPreOpenAcc acc 

evalPreOpenAcc :: Delayable a => PreOpenAcc OpenAcc aenv a -> S.Exp

-- The environment argument is used to convert de Bruijn indices to vars:
evalPreOpenAcc e = 
  case e of 
    Let acc1 acc2 -> S.Let undefined (evalOpenAcc acc1)  
                                     (evalOpenAcc acc2)
--    let !arr1 = force $ evalOpenAcc acc1 aenv
--    in evalOpenAcc acc2 (aenv `Push` arr1)

-- TODO: Let2 
    
    Let2 acc1 acc2 -> undefined
    PairArrays acc1 acc2 -> undefined
 --  = DelayedPair (evalOpenAcc acc1 aenv) (evalOpenAcc acc2 aenv)    
    Avar idx -> S.Avar (S.Var "v")
    
    Apply (Alam (Abody funAcc)) acc -> undefined
--   = let !arr = force $ evalOpenAcc acc aenv
--     in evalOpenAcc funAcc (Empty `Push` arr)
    Apply _afun _acc -> error "This case is impossible"

    Acond cond acc1 acc2 -> S.Cond (evalExp cond) 
                                   (evalOpenAcc acc1) 
                                   (evalOpenAcc acc2)
    -- This is a real array:
    Use arr -> S.Use

-- evalPreOpenAcc (Unit e) aenv = unitOp (evalExp e aenv)

-- evalPreOpenAcc (Generate sh f) aenv
--   = generateOp (evalExp sh aenv) (evalFun f aenv)

-- evalPreOpenAcc (Reshape e acc) aenv 
--   = reshapeOp (evalExp e aenv) (evalOpenAcc acc aenv)

-- evalPreOpenAcc (Replicate sliceIndex slix acc) aenv
--   = replicateOp sliceIndex (evalExp slix aenv) (evalOpenAcc acc aenv)
  
-- evalPreOpenAcc (Index sliceIndex acc slix) aenv
--   = indexOp sliceIndex (evalOpenAcc acc aenv) (evalExp slix aenv)

-- evalPreOpenAcc (Map f acc) aenv = mapOp (evalFun f aenv) (evalOpenAcc acc aenv)

    ZipWith f acc1 acc2 -> S.ZipWith (travF f)
                                     (evalOpenAcc acc1) 
                                     (evalOpenAcc acc2)

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


--------------------------------------------------------------------------------
-- Accelerate Scalar Expressions
    

-- evalExp e = evalOpenExp e

-- Evaluate a closed expression
--
-- evalExp :: Exp aenv t -> Val aenv -> t
-- evalExp e aenv = evalOpenExp e Empty aenv
    
evalExp :: Exp aenv t -> S.Exp
evalExp e = evalOpenExp e 

evalOpenExp :: OpenExp env aenv a -> S.Exp               
               -- Val env -> Val aenv -> a
evalOpenExp e = 
  case e of 
    Var idx -> S.Avar (S.Var "yay")    
    PrimApp p arg -> convertPrimApp p arg
  
-- evalOpenExp (Const c) _ _ = Sugar.toElt c

-- evalOpenExp (Tuple tup) env aenv 
--   = toTuple $ evalTuple tup env aenv

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

--------------------------------------------------------------------------------
-- Accelerate Primitives:    

convertPrimApp p arg =  S.PrimApp (op p) []
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
--
-- evalOpenFun :: OpenFun env aenv t -> Val env -> Val aenv -> t
-- evalOpenFun (Body e) env aenv = evalOpenExp e env aenv
-- evalOpenFun (Lam f)  env aenv 
--   = \x -> evalOpenFun f (env `Push` Sugar.fromElt x) aenv

-- -- Evaluate a closed function
-- --
-- evalFun :: Fun aenv t -> Val aenv -> t
-- evalFun f aenv = evalOpenFun f Empty aenv

--     travF :: OpenFun env aenv t
--           -> Ref count
--           -> [AccBinding aenv]
--           -> CIO (PreOpenFun ExecOpenAcc env aenv t, Ref count, [AccBinding aenv])
--     travF (Body b) aenv vars = do
--       (b', env1, var1) <- travE b aenv vars
--       return (Body b', env1, var1)
--     travF (Lam  f) aenv vars = do
--       (f', env1, var1) <- travF f aenv vars
--       return (Lam f', env1, var1)


travF :: OpenFun env aenv t
--          -> Ref count
--          -> [AccBinding aenv]
--          -> CIO (PreOpenFun ExecOpenAcc env aenv t, Ref count, [AccBinding aenv])
          -> S.Exp
--travF (Body b) aenv vars = undefined
-- travF (Body b) = evalPreOpenAcc b
travF (Body b) = evalOpenExp b
travF (Lam f) = S.Lam (S.Var "hi") (travF f)
