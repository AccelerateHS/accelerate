{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Simplify
-- Copyright   : [2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Simplify (

  -- simplify scalar expressions
  simplifyExp,
  simplifyFun,

  -- simplify array computations
  simplifyOpenAcc,

) where

-- standard library
import Prelude                                          hiding ( exp )
import Data.List                                        ( intercalate )
import Data.Maybe                                       ( fromMaybe )
import Data.Typeable

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Pretty                     ()
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Array.Sugar                ( Elt, Shape )
import Data.Array.Accelerate.Tuple

import qualified Debug.Trace                            as Debug


-- An environment that holds let-bound scalar expressions. The second
-- environment variable env' is used to project out the corresponding
-- index when looking up in the environment congruent expressions.
--
data Gamma env env' aenv where
  EmptyEnv :: Gamma env () aenv

  PushEnv  :: Gamma   env env'      aenv
           -> OpenExp env           aenv t
           -> Gamma   env (env', t) aenv

incEnv :: Gamma env env' aenv -> Gamma (env, s) env' aenv
incEnv EmptyEnv        = EmptyEnv
incEnv (PushEnv env e) = incEnv env `PushEnv` weakenE e

lookupEnv :: Gamma   env env' aenv
          -> OpenExp env      aenv t
          -> Maybe  (Idx env' t)
lookupEnv EmptyEnv        _             = Nothing
lookupEnv (PushEnv env e) x
  | Just REFL <- matchOpenExp e x       = Just ZeroIdx
  | otherwise                           = SuccIdx `fmap` lookupEnv env x


-- Currently this takes the form of a pretty weedy CSE optimisation, where we
-- look for expressions of the form:
--
-- > let x = e1 in e2
--
-- and replace all occurrences of e1 in e2 with x. This doesn't do full CSE, but
-- is enough to catch some cases.
--
localCSE
    :: Elt a
    => Gamma   env env aenv
    -> OpenExp env     aenv a
    -> Maybe (OpenExp env aenv a)
localCSE env exp
  | Just ix <- lookupEnv env exp = trace "CSE" (show exp) $ Just (Var ix)
  | otherwise                    = Nothing


-- Recover scalar loops. This looks for the pattern:
--
-- > let x =
-- >   let y = e1
-- >   in e2
-- > in e3
--
-- and if e2 and e3 are congruent, replace this with the value iteration form:
--
-- > iterate[2] e2 e1
--
-- where the expression e2 is repeated twice with an initial value of e1.
-- Similarly, loops can be joined:
--
-- > let x = iterate[n] f e1
-- > in e2
--
-- if the function body of f matches e2, then increase the iteration count.
--
recoverLoops
    :: Elt b
    => Gamma   env env aenv
    -> OpenExp env     aenv a
    -> OpenExp (env,a) aenv b
    -> Maybe (OpenExp env aenv b)
recoverLoops _env bnd body
  | Iterate n f x               <- bnd
  , Just REFL                   <- matchOpenFun f (Lam (Body body))
  = trace "loop join" (show f)
  $ Just $ Iterate (n+1) f x                    -- loop joining

  | Let bnd' body'              <- bnd
  , Just REFL                   <- matchEnvTop body body'
  , Just REFL                   <- matchOpenExp body body'
  = trace "loop intro" (show body)
  $ Just $ Iterate 2 (Lam (Body body)) bnd'     -- loop introduction


  | otherwise
  = Nothing
  where
    matchEnvTop :: (Elt s, Elt t) => OpenExp (env,s) aenv f -> OpenExp (env,t) aenv g -> Maybe (s :=: t)
    matchEnvTop _ _ = gcast REFL


-- Simplify scalar let bindings. This will:
--
--  a) This will check if the binding already exists in the environment, and so
--     replace all occurrences with that existing variable
--
--  b) Check for a specific pattern of let bindings that represent scalar loops
--
simplifyLet
    :: (Elt a, Elt b)
    => Gamma   env env aenv
    -> OpenExp env     aenv a
    -> OpenExp (env,a) aenv b
    -> OpenExp env     aenv b
simplifyLet env bnd body
  | Just x <- recoverLoops env bnd body = x
  | Just x <- lookupEnv env bnd         = inline body (Var x)
  | otherwise                           = Let bnd body


-- Simplify conditional expressions. If the branches are equal, we can avoid the
-- conditional altogether.
--
-- TODO: implement constant folding, then attempt to evaluate the predicate and
--       insert only the appropriate branch (module Algebra).
--
simplifyCond
    :: Elt t
    => Gamma env env aenv
    -> OpenExp env aenv Bool
    -> OpenExp env aenv t       -- then branch
    -> OpenExp env aenv t       -- else branch
    -> OpenExp env aenv t
simplifyCond _env p t e
  | Just REFL <- matchOpenExp t e       = t
  | otherwise                           = Cond p t e


-- Simplify shape intersection. Currently this only compares the terms as given,
-- but it would be possible to be cleverer and get the set of all intersections
-- in the sub-terms.
--
simplifyIntersect
    :: Shape sh
    => OpenExp env aenv sh
    -> OpenExp env aenv sh
    -> OpenExp env aenv sh
simplifyIntersect sh1 sh2
  | Just REFL <- matchOpenExp sh1 sh2   = sh1
  | otherwise                           = Intersect sh1 sh2     -- stable ordering?


-- Walk over the scalar expression, applying simplifications.
--
simplifyOpenExp
    :: forall env aenv t. Elt t
    => Gamma   env env aenv
    -> OpenExp env     aenv t
    -> OpenExp env     aenv t
simplifyOpenExp env = cvt
  where
    cvtA :: OpenAcc aenv a -> OpenAcc aenv a
    cvtA = simplifyOpenAcc

    cvt :: Elt e => OpenExp env aenv e -> OpenExp env aenv e
    cvt exp
      = flip fromMaybe (localCSE env exp)
      $ case exp of
          Let bnd body          -> let bnd'  = cvt bnd
                                       env'  = incEnv env `PushEnv` weakenE bnd'
                                       body' = simplifyOpenExp env' body
                                   in
                                   simplifyLet env bnd' body'
          --
          Var ix                -> Var ix
          Const c               -> Const c
          Tuple tup             -> Tuple (simplifyTuple env tup)
          Prj tup ix            -> Prj tup (cvt ix)
          IndexNil              -> IndexNil
          IndexCons sh sz       -> IndexCons (cvt sh) (cvt sz)
          IndexHead sh          -> IndexHead (cvt sh)
          IndexTail sh          -> IndexTail (cvt sh)
          IndexAny              -> IndexAny
          IndexSlice x ix sh    -> IndexSlice x (cvt ix) (cvt sh)
          IndexFull x ix sl     -> IndexFull x (cvt ix) (cvt sl)
          ToIndex sh ix         -> ToIndex (cvt sh) (cvt ix)
          FromIndex sh ix       -> FromIndex (cvt sh) (cvt ix)
          Cond p t e            -> simplifyCond env (cvt p) (cvt t) (cvt e)
          Iterate n f x         -> Iterate n (simplifyOpenFun env f) (cvt x)
          PrimConst c           -> PrimConst c
          PrimApp f x           -> PrimApp f (cvt x)
          Index a sh            -> Index (cvtA a) (cvt sh)
          LinearIndex a i       -> LinearIndex (cvtA a) (cvt i)
          Shape a               -> Shape (cvtA a)
          ShapeSize sh          -> ShapeSize (cvt sh)
          Intersect s t         -> simplifyIntersect (cvt s) (cvt t)


simplifyTuple
    :: Gamma env env aenv
    -> Tuple (OpenExp env aenv) t
    -> Tuple (OpenExp env aenv) t
simplifyTuple _   NilTup          = NilTup
simplifyTuple env (SnocTup tup e) = simplifyTuple env tup `SnocTup` simplifyOpenExp env e


simplifyOpenFun
    :: Gamma   env env aenv
    -> OpenFun env     aenv t
    -> OpenFun env     aenv t
simplifyOpenFun env (Body e) = Body (simplifyOpenExp env e)
simplifyOpenFun env (Lam  f) = Lam  (simplifyOpenFun (incEnv env `PushEnv` Var ZeroIdx) f)


-- Scalar expressions
-- ------------------

simplifyExp :: Elt t => Exp aenv t -> Exp aenv t
simplifyExp = simplifyOpenExp EmptyEnv . shrinkE

simplifyFun :: Fun aenv t -> Fun aenv t
simplifyFun = simplifyOpenFun EmptyEnv . shrinkFE


-- Array computations
-- ------------------

simplifyOpenAcc :: OpenAcc aenv a -> OpenAcc aenv a
simplifyOpenAcc = cvtA . shrinkOpenAcc
  where
    cvtT :: Atuple (OpenAcc aenv) t -> Atuple (OpenAcc aenv) t
    cvtT atup = case atup of
      NilAtup         -> NilAtup
      SnocAtup t a    -> cvtT t `SnocAtup` cvtA a

    cvtE :: Elt t => Exp aenv t -> Exp aenv t
    cvtE = simplifyExp

    cvtF :: Fun aenv t -> Fun aenv t
    cvtF = simplifyFun

    cvtA :: OpenAcc aenv a -> OpenAcc aenv a
    cvtA (OpenAcc acc) = OpenAcc $ case acc of
      Alet bnd body             -> Alet (cvtA bnd) (cvtA body)
      Avar ix                   -> Avar ix
      Atuple tup                -> Atuple (cvtT tup)
      Aprj tup a                -> Aprj tup (cvtA a)
      Apply f a                 -> Apply (simplifyOpenAfun f) (cvtA a)
      Acond p t e               -> Acond (cvtE p) (cvtA t) (cvtA e)
      Use a                     -> Use a
      Unit e                    -> Unit (cvtE e)
      Reshape e a               -> Reshape (cvtE e) (cvtA a)
      Generate e f              -> Generate (cvtE e) (cvtF f)
      Transform sh ix f a       -> Transform (cvtE sh) (cvtF ix) (cvtF f) (cvtA a)
      Replicate sl slix a       -> Replicate sl (cvtE slix) (cvtA a)
      Slice sl a slix           -> Slice sl (cvtA a) (cvtE slix)
      Map f a                   -> Map (cvtF f) (cvtA a)
      ZipWith f a1 a2           -> ZipWith (cvtF f) (cvtA a1) (cvtA a2)
      Fold f z a                -> Fold (cvtF f) (cvtE z) (cvtA a)
      Fold1 f a                 -> Fold1 (cvtF f) (cvtA a)
      FoldSeg f z a b           -> FoldSeg (cvtF f) (cvtE z) (cvtA a) (cvtA b)
      Fold1Seg f a b            -> Fold1Seg (cvtF f) (cvtA a) (cvtA b)
      Scanl f z a               -> Scanl (cvtF f) (cvtE z) (cvtA a)
      Scanl' f z a              -> Scanl' (cvtF f) (cvtE z) (cvtA a)
      Scanl1 f a                -> Scanl1 (cvtF f) (cvtA a)
      Scanr f z a               -> Scanr (cvtF f) (cvtE z) (cvtA a)
      Scanr' f z a              -> Scanr' (cvtF f) (cvtE z) (cvtA a)
      Scanr1 f a                -> Scanr1 (cvtF f) (cvtA a)
      Permute f1 a1 f2 a2       -> Permute (cvtF f1) (cvtA a1) (cvtF f2) (cvtA a2)
      Backpermute sh f a        -> Backpermute (cvtE sh) (cvtF f) (cvtA a)
      Stencil f b a             -> Stencil (cvtF f) b (cvtA a)
      Stencil2 f b1 a1 b2 a2    -> Stencil2 (cvtF f) b1 (cvtA a1) b2 (cvtA a2)


simplifyOpenAfun :: OpenAfun aenv t -> OpenAfun aenv t
simplifyOpenAfun afun =
  case afun of
    Abody b     -> Abody (simplifyOpenAcc b)
    Alam f      -> Alam (simplifyOpenAfun f)


-- Debugging ===================================================================
--

dump_simplify :: Bool
dump_simplify = False

trace :: String -> String -> a -> a
trace phase str x
  | dump_simplify       = Debug.trace msg x
  | otherwise           = x
  where
    msg = intercalate "\n"
        [ ""
        , ">> " ++ phase ++ ' ' : replicate (80 - 4 - length phase) '-'
        , str
        , replicate 80 '-'
        ]

