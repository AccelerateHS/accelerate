{-# LANGUAGE BangPatterns, GADTs, ScopedTypeVariables, CPP #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- {-# ANN module "HLint: ignore Eta reduce" #-}

-- TODO LIST:
--   * Implement Use
--   * Audit treatment of shapes and indices
--   * Audit tuple flattening guarantees
--   * Add type info to some language forms... 


module Data.Array.Accelerate.SimpleConverter 
       ( convert
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
-- import Data.Array.Accelerate.Array.Delayed
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple

import Data.Array.Accelerate.Analysis.Shape (accDim)

import qualified Data.Array as Arr
import qualified Data.Array.Accelerate.Smart       as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import qualified Data.Array.Accelerate.SimpleAST as S

import qualified Data.Array.Accelerate.Tuple as T
import qualified Data.Vector as V

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
type EnvM = State (SimpleEnv, Counter) 
type SimpleEnv = [S.Var]
type Counter = Int

runEnvM m = evalState m ([], 0)

-- Evaluate a sub-branch in an extended environment.
-- Returns the name of the fresh variable as well as the result:
withExtendedEnv :: String -> EnvM b -> EnvM (S.Var, b)
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
--                  then return (S.var "DUMMY")
                  else error$ "Environment did not contain an element "++show i++" : "++show env

getAccType :: forall aenv ans . Arrays ans => OpenAcc aenv ans -> S.Type
getAccType acc = convertArrayType ty 
  where (ty :: ArraysR ans) = arrays

getAccTypePre acc = getAccType (OpenAcc acc)

getExpType :: forall env aenv ans . Sugar.Elt ans => OpenExp env aenv ans -> S.Type
getExpType e = convertType ty 
  where  ty  = Sugar.eltType ((error"This shouldn't happen (0)")::ans) 


--------------------------------------------------------------------------------
-- Convert Accelerate Array-level Expressions
--------------------------------------------------------------------------------

-- convertAcc :: Delayable a => OpenAcc aenv a -> EnvM S.AExp
convertAcc :: OpenAcc aenv a -> EnvM S.AExp
convertAcc (OpenAcc cacc) = convertPreOpenAcc cacc 
 where 
-- convertPreOpenAcc :: forall aenv a . Delayable a => 
 convertPreOpenAcc :: forall aenv a . 
                      PreOpenAcc OpenAcc aenv a -> EnvM S.AExp
 convertPreOpenAcc eacc = 
  case eacc of 
    Alet acc1 acc2 -> 
       do a1     <- convertAcc acc1
          (v,a2) <- withExtendedEnv "a"$ 
                    convertAcc acc2 
          let sty = getAccType acc1
          return$ S.Let v sty a1 a2

    Avar idx -> 
      do var <- envLookup (deBruijnToInt idx)
         return$ S.Vr var

    ------------------------------------------------------------
    -- Array creation:

    -- These should include types.

    -- This is real live runtime array data:
    -- TEMP FIXME -- need to finish the Use case:
    Use arr -> return$ S.Use (show arr)

    Generate sh f -> S.Generate (getAccTypePre eacc)
                                <$> convertExp sh
                                <*> convertFun f

    ------------------------------------------------------------

    Acond cond acc1 acc2 -> S.Cond <$> convertExp cond 
                                   <*> convertAcc acc1 
                                   <*> convertAcc acc2

    Apply (Alam (Abody funAcc)) acc -> 
      do (v,bod) <- withExtendedEnv "a" $ convertAcc funAcc
         let sty = getAccType acc
         S.Apply (S.ALam [(v, sty)] bod) <$> convertAcc acc

    Apply _afun _acc -> error "This case is impossible"

    Alet2 acc1 acc2 -> 
       do a1     <- convertAcc acc1
          (v2,(v1,a2)) <- withExtendedEnv "a"$ 
                                 withExtendedEnv "a"$                    
                          convertAcc acc2
          case getAccType acc2 of 
            S.TTuple [ty1,ty2] -> return$ S.LetPair (v1,v2) (ty1,ty2) a1 a2
            t -> error$ "Type error, expected pair on RHS of Let2, found: "++ show t


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

    Replicate sliceIndex slix a ->       
      let dimSl  = accDim a
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
    Reshape e acc -> S.Reshape <$> convertExp e <*> convertAcc acc
    Permute fn dft pfn acc -> S.Permute <$> convertFun fn 
                                        <*> convertAcc dft
                                        <*> convertFun pfn
                                        <*> convertAcc acc
    Backpermute e pfn acc -> S.Backpermute <$> convertExp e 
                                           <*> convertFun pfn
                                           <*> convertAcc acc 
    Stencil  sten bndy acc -> S.Stencil <$> convertFun sten
                                        <*> return (convertBoundary bndy)
                                        <*> convertAcc acc
    Stencil2 sten bndy1 acc1 bndy2 acc2 -> 
      S.Stencil2 <$> convertFun sten 
                 <*> return (convertBoundary bndy1) <*> convertAcc acc1
                 <*> return (convertBoundary bndy2) <*> convertAcc acc2


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

convertBoundary :: Boundary a -> S.Boundary
convertBoundary = undefined

-- Evaluate a closed expression
convertExp :: forall env aenv ans . OpenExp env aenv ans -> EnvM S.Exp
convertExp e = 
  case e of 
    Let exp1 exp2 -> 
      do e1     <- convertExp exp1
         (v,e2) <- withExtendedEnv "e"$ 
                   convertExp exp2 
         let sty = getExpType exp1
         return$ S.ELet v sty e1 e2
    
    -- Here is where we get to peek at the type of a variable:
    Var idx -> 
      do var <- envLookup (deBruijnToInt idx)
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
    ShapeSize  acc -> S.EShapeSize  <$> convertExp acc


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
    SingleTuple scalar -> 
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
    UnitTuple -> S.Tup []
    PairTuple ty1 ty0 -> let (c1,c0) = c 
                             c0' = convertConst ty0 c0
                         in 
                         case convertConst ty1 c1 of
                           S.Tup [] -> c0' 
                           S.Tup ls -> S.Tup (c0' : ls)
                           singl -> S.Tup [c0', singl]
--                           oth -> error$ "mal constructed tuple on RHS of PairTuple: "++ show oth
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
   loop acc (Body b) = do b'  <- convertExp b 
                          -- It would perhaps be nice to record the output type of function 
                          -- here as well.  But b's type isn't in class Elt.
                          return (S.Lam (reverse acc) b')
   -- Here we again dig around in the Haskell types to find the type information we need.
   -- In this case we use quite a few scoped type variables:
   loop acc orig@(Lam f2) | (_:: OpenFun env aenv (arg -> res)) <- orig 
                          = do 
                               let (_:: OpenFun (env, arg) aenv res) = f2 
                                   ety = Sugar.eltType ((error"This shouldn't happen (4)") :: arg)
                                   sty = convertType ety
                               (_,x) <- withExtendedEnv "v" $ do
                                          v <- envLookup 0
                                          loop ((v,sty) : acc) f2
                               return x 
