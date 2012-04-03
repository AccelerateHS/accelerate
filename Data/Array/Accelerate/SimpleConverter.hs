{-# LANGUAGE BangPatterns, GADTs, ScopedTypeVariables, CPP #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- {-# ANN module "HLint: ignore Eta reduce" #-}

module Data.Array.Accelerate.SimpleConverter 
       (
         convert
         -- TEMP:
         , p1,t1,p2,t2,p3,t3
       )
       where

-- standard libraries
import Control.Monad
import Control.Applicative ((<$>),(<*>))
-- import Control.Monad.ST                            (ST)
import Data.Bits
import Data.Char                                   (chr, ord)
import Prelude                                     hiding (sum)
import Debug.Trace
import Data.Typeable (typeOf)

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

import Data.Array.Accelerate.Analysis.Shape (accDim)

import qualified Data.Array as Arr
import qualified Data.Array.Accelerate.Smart       as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import qualified Data.Array.Accelerate.SimpleAST as S

import qualified Data.Array.Accelerate.Tuple as T
import qualified Data.Vector as V
import qualified Data.Map as M

-- TEMP:
import qualified Data.Array.Accelerate.Language as Lang
-- TEMP:
import qualified Data.Array.Accelerate.Interpreter as Interp

--------------------------------------------------------------------------------
-- TEMPORARY -- Testing:

p0 = Lang.use $ Sugar.fromList (Z :. (10::Int)) [1..10::Int64]
t0 :: S.AExp
t0 = convert p0

p1 :: Sugar.Acc (Sugar.Scalar Float)
p1 = let xs = Lang.generate (Lang.constant (Z :. (10::Int))) (\ (i) -> 3.3 )
         ys = xs
     in  Lang.fold (+) 0 (Lang.zipWith (*) xs ys)

t1 :: S.AExp
t1 = convert p1

p2 :: Sugar.Acc (Sugar.Vector Int32)
p2 = let xs = Lang.replicate (Lang.constant (Z :. (4::Int))) (Lang.unit 4)
     in Lang.map (+ 10) xs
t2 = convert p2

-- p3 :: Sugar.Acc (Sugar.Array Sugar.DIM2 Int32)
p3 :: Sugar.Acc (Sugar.Array Sugar.DIM3 Int32)
p3 = let arr = Lang.generate  (Lang.constant (Z :. (5::Int))) (\_ -> 33)
         xs  = Lang.replicate (Lang.constant$ Z :. (2::Int) :. Sugar.All :. (3::Int)) arr
     in xs 
t3 = convert p3


--------------------------------------------------------------------------------
-- Exposed entrypoints for this module:
--------------------------------------------------------------------------------

-- | Convert the sophisticate Accelerate-internal AST representation
--   into something very simple for external consumption.
convert :: Arrays a => Sugar.Acc a -> S.AExp
convert = runEnvM . convertAcc . Sugar.convertAcc

--------------------------------------------------------------------------------
-- Environments
--------------------------------------------------------------------------------


-- We use a simple state monad for keeping track of the environment
type EnvM = State (SimpleEnv, TypeObsvs, Counter) 
type SimpleEnv = [S.Var]
type TypeObsvs = M.Map S.Var S.Type
type Counter = Int

runEnvM m = evalState m ([], M.empty, 0)

-- Evaluate a sub-branch in an extended environment.
-- Returns the name of the fresh variable as well as the result:
withExtendedEnv :: String -> EnvM b -> EnvM (S.Var, S.Type, b)
withExtendedEnv basename branch = do 
  (env,tyM,cnt) <- get 
  let newsym = S.var $ basename ++ show cnt
  put (newsym:env, tyM, cnt+1) 
  b <- branch
  -- We keep counter-increments from the sub-branch, but NOT the extended env:
  (_,tyM2,cnt2) <- get 
  put (env,tyM2,cnt2)
  case M.lookup newsym tyM2 of
    -- Should this ever happen?:
    Nothing -> trace ("WARNING: Unused variable in Accelerate program!!: "++show newsym)$ 
--               return (newsym, S.TTuple [S.TTuple []], b) -- TEMP - FIXME
               return (newsym, S.TUnknown, b) -- TEMP - FIXME
               -- error$ "Unused variable in Accelerate program!!: "++show newsym
    Just ty -> return (newsym, ty, b)

-- Look up a de bruijn index in the environment:
envLookup :: Int -> EnvM S.Var
envLookup i = do (env,_,_) <- get
                 if length env > i
                  then return (env !! i)
                  else error$ "Environment did not contain an element "++show i++" : "++show env

-- Type /Retrieval/.
--
-- Starting from the internal Accelerate AST representation we don't
-- directly have type information on binders, but we do have type
-- information on variable references.  Thus we collect type
-- observations at variable references and retrieve the types at the
-- binders. 
-- 
-- Observations must be consistent, and they should always be.  But we
-- perform unification just to make sure.
observeType :: S.Var -> S.Type -> EnvM ()
observeType var ty = 
  do (env,tyM,cnt) <- get 
     put (env, M.insert var ty tyM, cnt)


getAccType :: PreOpenAcc OpenAcc aenv a -> S.Type
getAccType = undefined

getExpType :: OpenExp env aenv ans -> S.Type
getExpType = undefined

--------------------------------------------------------------------------------
-- Convert Accelerate Array-level Expressions
--------------------------------------------------------------------------------

convertAcc :: Delayable a => OpenAcc aenv a -> EnvM S.AExp
convertAcc (OpenAcc cacc) = convertPreOpenAcc cacc 
 where 
 convertPreOpenAcc :: forall aenv a . Delayable a => 
		      PreOpenAcc OpenAcc aenv a -> EnvM S.AExp
 convertPreOpenAcc eacc = 
  case eacc of 
    Let acc1 acc2 -> 
       do a1        <- convertAcc acc1
          (v,ty,a2) <- withExtendedEnv "a"$ 
                       convertAcc acc2 
          return$ S.Let v ty a1 a2

    Avar idx -> 
      -- Reify array type information present in dictionary (i.e. type
      -- class overloading):
      do let (ty :: ArraysR a) = arrays
             sty = convertArrayType ty 
         var <- envLookup (idxToInt idx)
         observeType var sty
         return$ S.Vr var

    -- This is real live runtime array data:
    Use arr -> return$ S.Use (show arr)

    Acond cond acc1 acc2 -> S.Cond <$> convertExp cond 
                                   <*> convertAcc acc1 
                                   <*> convertAcc acc2

    Apply (Alam (Abody funAcc)) acc -> 
      do (v,ty,bod) <- withExtendedEnv "a" $ convertAcc funAcc
         S.Apply (S.ALam [(v, ty)] bod) <$> convertAcc acc
    Apply _afun _acc -> error "This case is impossible"

    Let2 acc1 acc2 -> 
       do a1     <- convertAcc acc1
          (v2,ty2,(v1,ty1,a2)) <- withExtendedEnv "a"$ 
				  withExtendedEnv "a"$ 		   
				  convertAcc acc2
          return$ S.LetPair (v1,v2) (ty1,ty2) a1 a2

    PairArrays acc1 acc2 -> S.PairArrays <$> convertAcc acc1 
			                 <*> convertAcc acc2
    Unit e        -> S.Unit <$> convertExp e 

    Map     f acc       -> S.Map     <$> convertFun f 
                                     <*> convertAcc acc
    ZipWith f acc1 acc2 -> S.ZipWith <$> convertFun f
                                     <*> convertAcc acc1
                                     <*> convertAcc acc2
    Fold     f e acc -> S.Fold  <$> convertFun f
                                <*> convertExp e 
                                <*> convertAcc acc
    Fold1    f   acc -> S.Fold1 <$> convertFun f
                                <*> convertAcc acc
    FoldSeg  f e acc1 acc2 -> S.FoldSeg  <$> convertFun f
                                         <*> convertExp e
                                         <*> convertAcc acc1
                                         <*> convertAcc acc2
    Fold1Seg f   acc1 acc2 -> S.Fold1Seg <$> convertFun f
                                         <*> convertAcc acc1
                                         <*> convertAcc acc2
    Scanl  f e acc -> S.Scanl  <$> convertFun f
                               <*> convertExp e 
                               <*> convertAcc acc
    Scanl' f e acc -> S.Scanl' <$> convertFun f
                               <*> convertExp e 
                               <*> convertAcc acc
    Scanl1 f   acc -> S.Scanl1 <$> convertFun f
                               <*> convertAcc acc
    Scanr  f e acc -> S.Scanr  <$> convertFun f
                               <*> convertExp e 
                               <*> convertAcc acc
    Scanr' f e acc -> S.Scanr' <$> convertFun f
                               <*> convertExp e 
                               <*> convertAcc acc
    Scanr1 f   acc -> S.Scanr1 <$> convertFun f
                               <*> convertAcc acc

    Generate sh f -> S.Generate <$> convertExp sh
                                <*> convertFun f

    Replicate sliceIndex slix a -> 
      
      let 
          dimSl  = accDim a
--          dimOut = accDim eacc
          extend :: SliceIndex slix sl co dim -> Int -> [Int]
          extend (SliceNil)            n = []
          extend (SliceAll   sliceIdx) n = dimSl : extend sliceIdx (n+1)
          extend (SliceFixed sliceIdx) n = extend sliceIdx (n+1)
      in S.Replicate (show sliceIndex)
                 --  (show $ extend sliceIndex 0) 
                 <$> convertExp slix 
                 <*> convertAcc a

    Index sliceIndex acc slix -> 
      S.Index (show sliceIndex) <$> convertAcc acc
                                <*> convertExp slix
    
    Reshape e acc -> error "reshape"
    Permute f dftAcc p acc -> error "permute"
    Backpermute e    p acc -> error "backperm"
    Stencil  sten bndy acc -> error "stencil"
    Stencil2 sten bndy1 acc1 bndy2 acc2 -> error "stencil2"


--------------------------------------------------------------------------------
-- Convert Accelerate Scalar Expressions
--------------------------------------------------------------------------------

-- For now I'm leaving it as an index from the right with no length:
convertTupleIdx :: TupleIdx t e -> Int
convertTupleIdx tix = loop tix
 where 
  loop :: TupleIdx t e -> Int
  loop ZeroTupIdx       = 0
  loop (SuccTupIdx idx) = 1 + loop idx

-- Evaluate a closed expression
convertExp :: forall env aenv ans . OpenExp env aenv ans -> EnvM S.Exp
convertExp e = 
  case e of 
    -- Here is where we get to peek at the type of a variable:
    Var idx -> 
      do let ty  = Sugar.eltType ((error"This shouldn't happen (2)")::ans) 
             sty = convertType ty 
         var <- envLookup (idxToInt idx)
         observeType var sty
         return$ S.EVr var
    PrimApp p arg -> convertPrimApp p arg

    Tuple tup -> convertTuple tup

    Const c   -> return$ S.EConst$ 
                 convertConst (Sugar.eltType (undefined::ans)) c

    -- NOTE: The incoming AST indexes tuples FROM THE RIGHT:
    Prj idx e -> 
                 -- If I could get access to the IsTuple dict I could do something here:
                 -- The problem is the type function EltRepr....
                 let n = convertTupleIdx idx in 
--                 S.EPrj n m <$> convertExp e
                 S.ETupProjectFromRight n <$> convertExp e

    -- This would seem to force indices to be LISTS at runtime??
    IndexNil       -> return$ S.EIndex []
    IndexCons esh ei -> do esh' <- convertExp esh
			   ei'  <- convertExp ei
                           return $ case esh' of
                             S.EIndex ls -> S.EIndex (ei' : ls)
			     _           -> S.EIndexConsDynamic ei' esh'
    IndexHead eix   -> do eix' <- convertExp eix
                          return $ case eix' of
                             -- WARNING: This is a potentially unsafe optimization:
                             -- Throwing away expressions:
                             S.EIndex (h:_) -> h 
                             S.EIndex []    -> error "IndexHead of empty index."
			     _              -> S.EIndexHeadDynamic eix'
    IndexTail eix   -> do eix' <- convertExp eix
                          return $ case eix' of
                             -- WARNING: This is a potentially unsafe optimization:
                             -- Throwing away expressions:
                             S.EIndex (_:tl) -> S.EIndex tl
                             S.EIndex []     -> error "IndexTail of empty index."
			     _               -> S.EIndexTailDynamic eix'
    IndexAny       -> return S.EIndexAny

    Cond c t e  -> S.ECond <$> convertExp c 
                           <*> convertExp t
                           <*> convertExp e
    PrimConst c -> return$ S.EConst $ 
                   case c of 
                    PrimMinBound _ -> S.MinBound
		    PrimMaxBound _ -> S.MaxBound
		    PrimPi       _ -> S.Pi

    IndexScalar acc eix -> S.EIndexScalar <$> convertAcc acc
                                          <*> convertExp eix
    Shape acc -> S.EShape <$> convertAcc acc
    Size  acc -> S.ESize  <$> convertAcc acc


-- Convert a tuple expression to our simpler Tuple representation (containing a list):
-- convertTuple :: Tuple (PreOpenExp acc env aenv) t' -> S.AExp
convertTuple :: Tuple (PreOpenExp OpenAcc env aenv) t' -> EnvM S.Exp
convertTuple NilTup = return$ S.ETuple []
convertTuple (SnocTup tup e) = 
    do e' <- convertExp e
       tup' <- convertTuple tup
       case tup' of 
         S.ETuple ls -> return$ S.ETuple$ ls ++ [e']
         se -> error$ "convertTuple: expected a tuple expression, received:\n  "++ show se

convertTupleExp :: PreOpenExp OpenAcc t t1 t2 -> EnvM [S.Exp]
convertTupleExp e = do
  e' <- convertExp e
  case e' of 
    S.ETuple ls -> return ls
    se -> error$ "convertTupleExp: expected a tuple expression, received:\n  "++ show se


--------------------------------------------------------------------------------
-- Convert types
-------------------------------------------------------------------------------

convertType :: TupleType a -> S.Type
convertType ty = 
  case ty of 
    UnitTuple -> S.TTuple []
    PairTuple ty1 ty0  -> 
      let ty0' = convertType ty0 in 
      -- Convert to Haskell-style tuples here (left-first, no unary tuples):
      case convertType ty1 of 
        S.TTuple [] -> ty0'
        S.TTuple ls -> S.TTuple (ty0' : ls)
	oth         -> S.TTuple [ty0', oth]
    SingleTuple scalar -> S.TScalar$ 
     case scalar of 
       NumScalarType (IntegralNumType ty) -> 
	 case ty of 
	   TypeInt   _  -> S.TInt
	   TypeInt8  _  -> S.TInt8 
	   TypeInt16 _  -> S.TInt16  
	   TypeInt32 _  -> S.TInt32 
	   TypeInt64 _  -> S.TInt64 
	   TypeWord   _ -> S.TWord
	   TypeWord8  _ -> S.TWord8 
	   TypeWord16 _ -> S.TWord16 
	   TypeWord32 _ -> S.TWord32 
	   TypeWord64 _ -> S.TWord64 
	   TypeCShort _ -> S.TCShort 
	   TypeCInt   _ -> S.TCInt 
	   TypeCLong  _ -> S.TCLong 
	   TypeCLLong _ -> S.TCLLong 
	   TypeCUShort _ -> S.TCUShort
	   TypeCUInt   _ -> S.TCUInt
	   TypeCULong  _ -> S.TCULong
	   TypeCULLong _ -> S.TCULLong
       NumScalarType (FloatingNumType ty) -> 
	 case ty of 
	   TypeFloat _   -> S.TFloat 
	   TypeDouble _  -> S.TDouble 
	   TypeCFloat _  -> S.TCFloat 
	   TypeCDouble _ -> S.TCDouble 
       NonNumScalarType ty -> 
	 case ty of 
	   TypeBool _   -> S.TBool 
	   TypeChar _   -> S.TChar 
	   TypeCChar _  -> S.TCChar 
	   TypeCSChar _ -> S.TCSChar 
	   TypeCUChar _ -> S.TCUChar 


convertArrayType :: forall arrs . ArraysR arrs -> S.Type
convertArrayType ty = 
  case ty of 
   ArraysRunit  -> S.TTuple []
   -- Again, here we reify information from types (phantom type
   -- parameters) into a concrete data-representation:
   ArraysRarray | (_::ArraysR (Array sh e)) <- ty -> 
     let ety = Sugar.eltType ((error"This shouldn't happen (3)")::e) 
     in S.TArray (convertType ety)
   -- Left to right!
   ArraysRpair t0 t1 -> S.TTuple [convertArrayType t0,
				  convertArrayType t1]

--------------------------------------------------------------------------------
-- Convert constants    
-------------------------------------------------------------------------------

-- convertConst :: Sugar.Elt t => Sugar.EltRepr t -> S.Const
convertConst :: TupleType a -> a -> S.Const
convertConst ty c = 
  case ty of 
    UnitTuple -> S.ConstTup []
    PairTuple ty1 ty0 -> let (c1,c0) = c 
                             c0' = convertConst ty0 c0
                         in 
                         case convertConst ty1 c1 of
                           S.ConstTup ls -> S.ConstTup (c0' : ls)
                           _ -> error "mal constructed tuple"
    SingleTuple scalar -> 
      case scalar of 
        NumScalarType (IntegralNumType ty) -> 
          case ty of 
            TypeInt   _  -> S.I  c
            TypeInt8  _  -> S.I8  c
            TypeInt16 _  -> S.I16 c
            TypeInt32 _  -> S.I32 c
            TypeInt64 _  -> S.I64 c
            TypeWord   _ -> S.W  c
            TypeWord8  _ -> S.W8  c
            TypeWord16 _ -> S.W16 c
            TypeWord32 _ -> S.W32 c
            TypeWord64 _ -> S.W64 c
            TypeCShort _ -> S.CS  c
            TypeCInt   _ -> S.CI  c
            TypeCLong  _ -> S.CL  c
            TypeCLLong _ -> S.CLL c
            TypeCUShort _ -> S.CUS  c
            TypeCUInt   _ -> S.CUI  c
            TypeCULong  _ -> S.CUL  c
            TypeCULLong _ -> S.CULL c
        NumScalarType (FloatingNumType ty) -> 
          case ty of 
            TypeFloat _   -> S.F c    
            TypeDouble _  -> S.D c 
            TypeCFloat _  -> S.CF c    
            TypeCDouble _ -> S.CD c 
        NonNumScalarType ty -> 
          case ty of 
            TypeBool _   -> S.B c
            TypeChar _   -> S.C c
            TypeCChar _  -> S.CC c
            TypeCSChar _ -> S.CSC c 
            TypeCUChar _ -> S.CUC c

--------------------------------------------------------------------------------
-- Convert Accelerate Primitive Applications: 
--------------------------------------------------------------------------------

convertPrimApp :: (Sugar.Elt a, Sugar.Elt b)
               => PrimFun (a -> b) -> PreOpenExp OpenAcc env aenv a
               -> EnvM S.Exp
convertPrimApp p arg = 
  do args' <- convertTupleExp arg
     return$ S.EPrimApp (op p) args'
 where 
   op p = 
    case p of 
      PrimAdd ty -> S.NP S.Add
      PrimMul ty -> S.NP S.Mul
      _ -> error$ "primapp not handled yet: "++show (PrimApp p arg)

numty nt = 
  case nt of 
    IntegralNumType ty -> undefined
    FloatingNumType ty -> undefined

--------------------------------------------------------------------------------
-- Convert Accelerate Functions
--------------------------------------------------------------------------------

-- Convert an open, scalar function:
convertFun :: OpenFun e ae t0 -> EnvM S.Fun
convertFun =  loop [] 
 where 
   loop :: forall env aenv t . 
	   [(S.Var,S.Type)] -> OpenFun env aenv t -> EnvM S.Fun
   loop acc (Body b) = do b' <- convertExp b 
			  return (S.Lam (reverse acc) b')
   -- Here we again dig around in the Haskell types to find the type information we need.
   -- In this case we use quite a few scoped type variables:
   loop acc orig@(Lam f2) | (_:: OpenFun env aenv (arg -> res)) <- orig 
                          = do 
			       let (_:: OpenFun (env, Sugar.EltRepr arg) aenv res) = f2 
				   ety = Sugar.eltType ((error"This shouldn't happen (4)") :: arg)
				   sty = convertType ety
			       (_,_,x) <- withExtendedEnv "v" $ do
					    v <- envLookup 0
					    loop ((v,sty) : acc) f2
			       return x 

-- convertFun (Body b) = convertExp b
-- convertFun (Lam f)  = fmap snd $ 
--                  withExtendedEnv "v" $ do
--                    v <- envLookup 0
--                    S.Lam args bod <- convertFun f 
--                    return$ (S.Lam (v:args) bod)
-- --		   S.Lam v <$> convertFun f
