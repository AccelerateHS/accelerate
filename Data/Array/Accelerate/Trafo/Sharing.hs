{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Sharing
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--               [2009..2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements HOAS to de Bruijn conversion of array expressions
-- while incorporating sharing information.
--

module Data.Array.Accelerate.Trafo.Sharing (

  -- * HOAS -> de Bruijn conversion
  convertAcc, convertAfun, Afunction, AfunctionR,
  convertExp, convertFun,  Function,  FunctionR

) where

-- standard library
import Control.Applicative                              hiding ( Const )
import Control.Monad.Fix
import Data.List
import Data.Maybe
import Data.Hashable
import Data.Typeable
import qualified Data.HashTable.IO                      as Hash
import qualified Data.IntMap                            as IntMap
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as Set
import System.IO.Unsafe                                 ( unsafePerformIO )
import System.Mem.StableName

-- friends
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Array.Sugar                as Sugar
import Data.Array.Accelerate.Tuple                      hiding ( Tuple )
import Data.Array.Accelerate.AST                        hiding (
  PreOpenAcc(..), OpenAcc(..), Acc, Stencil(..), PreOpenExp(..), OpenExp, PreExp, Exp,
  showPreAccOp, showPreExpOp )
import qualified Data.Array.Accelerate.AST              as AST
import qualified Data.Array.Accelerate.Tuple            as Tuple
import qualified Data.Array.Accelerate.Debug            as Debug

#include "accelerate.h"


-- Configuration
-- -------------

-- Perhaps the configuration should be passed as a reader monad or some such,
-- but that's a little inconvenient.
--
data Config = Config
  {
    recoverAccSharing   :: Bool         -- ^ Recover sharing of array computations ?
  , recoverExpSharing   :: Bool         -- ^ Recover sharing of scalar expressions ?
  , floatOutAcc         :: Bool         -- ^ Always float array computations out of expressions ?
  }


-- Layouts
-- -------

-- A layout of an environment has an entry for each entry of the environment.
-- Each entry in the layout holds the de Bruijn index that refers to the
-- corresponding entry in the environment.
--
data Layout env env' where
  EmptyLayout :: Layout env ()
  PushLayout  :: Typeable t
              => Layout env env' -> Idx env t -> Layout env (env', t)

-- Project the nth index out of an environment layout.
--
-- The first argument provides context information for error messages in the case of failure.
--
prjIdx :: forall t env env'. Typeable t => String -> Int -> Layout env env' -> Idx env t
prjIdx ctxt 0 (PushLayout _ (ix :: Idx env0 t0))
  = flip fromMaybe (gcast ix)
  $ possiblyNestedErr ctxt $
      "Couldn't match expected type `" ++ show (typeOf (undefined::t)) ++
      "' with actual type `" ++ show (typeOf (undefined::t0)) ++ "'" ++
      "\n  Type mismatch"
prjIdx ctxt n (PushLayout l _)  = prjIdx ctxt (n - 1) l
prjIdx ctxt _ EmptyLayout       = possiblyNestedErr ctxt "Environment doesn't contain index"

possiblyNestedErr :: String -> String -> a
possiblyNestedErr ctxt failreason
  = error $ "Fatal error in Sharing.prjIdx:"
      ++ "\n  " ++ failreason ++ " at " ++ ctxt
      ++ "\n  Possible reason: nested data parallelism — array computation that depends on a"
      ++ "\n    scalar variable of type 'Exp a'"

-- Add an entry to a layout, incrementing all indices
--
incLayout :: Layout env env' -> Layout (env, t) env'
incLayout EmptyLayout         = EmptyLayout
incLayout (PushLayout lyt ix) = PushLayout (incLayout lyt) (SuccIdx ix)

sizeLayout :: Layout env env' -> Int
sizeLayout EmptyLayout        = 0
sizeLayout (PushLayout lyt _) = 1 + sizeLayout lyt


-- Conversion from HOAS to de Bruijn computation AST
-- =================================================

-- Array computations
-- ------------------

-- | Convert a closed array expression to de Bruijn form while also incorporating sharing
-- information.
--
convertAcc
    :: Arrays arrs
    => Bool             -- ^ recover sharing of array computations ?
    -> Bool             -- ^ recover sharing of scalar expressions ?
    -> Bool             -- ^ always float array computations out of expressions?
    -> Acc arrs
    -> AST.Acc arrs
convertAcc shareAcc shareExp floatAcc acc
  = let config  = Config shareAcc shareExp (shareAcc && floatAcc)
    in
    convertOpenAcc config 0 [] EmptyLayout acc


-- | Convert a closed function over array computations, while incorporating
-- sharing information.
--
convertAfun :: Afunction f => Bool -> Bool -> Bool -> f -> AST.Afun (AfunctionR f)
convertAfun shareAcc shareExp floatAcc =
  let config = Config shareAcc shareExp (shareAcc && floatAcc)
  in  aconvert config EmptyLayout


-- Convert a HOAS fragment into de Bruijn form, binding variables into the typed
-- environment layout one binder at a time.
--
-- NOTE: Because we convert one binder at a time left-to-right, the bound
--       variables ('vars') will have de Bruijn index _zero_ as the outermost
--       binding, and thus go to the end of the list.
--
class Afunction f where
  type AfunctionR f
  aconvert :: Config -> Layout aenv aenv -> f -> AST.OpenAfun aenv (AfunctionR f)

instance (Arrays a, Afunction r) => Afunction (Acc a -> r) where
  type AfunctionR (Acc a -> r) = a -> AfunctionR r
  --
  aconvert config alyt f
    = let a     = Acc $ Atag (sizeLayout alyt)
          alyt' = incLayout alyt `PushLayout` ZeroIdx
      in
      Alam $ aconvert config alyt' (f a)

instance Arrays b => Afunction (Acc b) where
  type AfunctionR (Acc b) = b
  --
  aconvert config alyt body
    = let lvl    = sizeLayout alyt
          vars   = [lvl-1, lvl-2 .. 0]
      in
      Abody $ convertOpenAcc config lvl vars alyt body


-- | Convert an open array expression to de Bruijn form while also incorporating sharing
-- information.
--
convertOpenAcc
    :: Arrays arrs
    => Config
    -> Level
    -> [Level]
    -> Layout aenv aenv
    -> Acc arrs
    -> AST.OpenAcc aenv arrs
convertOpenAcc config lvl fvs alyt acc
  = let (sharingAcc, initialEnv) = recoverSharingAcc config lvl fvs acc
    in
    convertSharingAcc config alyt initialEnv sharingAcc

-- | Convert an array expression with given array environment layout and sharing information into
-- de Bruijn form while recovering sharing at the same time (by introducing appropriate let
-- bindings).  The latter implements the third phase of sharing recovery.
--
-- The sharing environment 'env' keeps track of all currently bound sharing variables, keeping them
-- in reverse chronological order (outermost variable is at the end of the list).
--
convertSharingAcc
    :: forall aenv arrs. Arrays arrs
    => Config
    -> Layout aenv aenv
    -> [StableSharingAcc]
    -> ScopedAcc arrs
    -> AST.OpenAcc aenv arrs
convertSharingAcc _ alyt aenv (ScopedAcc lams (AvarSharing sa))
  | Just i <- findIndex (matchStableAcc sa) aenv'
  = AST.OpenAcc $ AST.Avar (prjIdx (ctxt ++ "; i = " ++ show i) i alyt)
  | null aenv'
  = error $ "Cyclic definition of a value of type 'Acc' (sa = " ++
            show (hashStableNameHeight sa) ++ ")"
  | otherwise
  = INTERNAL_ERROR(error) "convertSharingAcc" err
  where
    aenv' = lams ++ aenv
    ctxt = "shared 'Acc' tree with stable name " ++ show (hashStableNameHeight sa)
    err  = "inconsistent valuation @ " ++ ctxt ++ ";\n  aenv = " ++ show aenv'

convertSharingAcc config alyt aenv (ScopedAcc lams (AletSharing sa@(StableSharingAcc _ boundAcc) bodyAcc))
  = AST.OpenAcc
  $ let alyt' = incLayout alyt `PushLayout` ZeroIdx
        aenv' = lams ++ aenv
    in
    AST.Alet (convertSharingAcc config alyt aenv' (ScopedAcc [] boundAcc))
             (convertSharingAcc config alyt' (sa:aenv') bodyAcc)

convertSharingAcc config alyt aenv (ScopedAcc lams (AccSharing _ preAcc))
  = AST.OpenAcc
  $ let aenv' = lams ++ aenv

        cvtA :: Arrays a => ScopedAcc a -> AST.OpenAcc aenv a
        cvtA = convertSharingAcc config alyt aenv'

        cvtE :: Elt t => ScopedExp t -> AST.Exp aenv t
        cvtE = convertSharingExp config EmptyLayout alyt [] aenv'

        cvtF1 :: (Elt a, Elt b) => (Exp a -> ScopedExp b) -> AST.Fun aenv (a -> b)
        cvtF1 = convertSharingFun1 config alyt aenv'

        cvtF2 :: (Elt a, Elt b, Elt c) => (Exp a -> Exp b -> ScopedExp c) -> AST.Fun aenv (a -> b -> c)
        cvtF2 = convertSharingFun2 config alyt aenv'

        cvtAfun1 :: (Arrays a, Arrays b) => (Acc a -> ScopedAcc b) -> AST.OpenAfun aenv (a -> b)
        cvtAfun1 = convertSharingAfun1 config alyt aenv'
    in
    case preAcc of

      Atag i
        -> AST.Avar (prjIdx ("de Bruijn conversion tag " ++ show i) i alyt)

      Pipe afun1 afun2 acc
        -> let noStableSharing = StableSharingAcc noStableAccName (undefined :: SharingAcc acc exp ())
               alyt'    = incLayout alyt `PushLayout` ZeroIdx
               boundAcc = cvtAfun1 afun1 `AST.Apply` cvtA acc
               bodyAcc  = convertSharingAfun1 config alyt' (noStableSharing : aenv') afun2
                          `AST.Apply`
                          AST.OpenAcc (AST.Avar AST.ZeroIdx)
           in
           AST.Alet (AST.OpenAcc boundAcc) (AST.OpenAcc bodyAcc)

      Aforeign ff afun acc
        -> let a = recoverAccSharing config
               e = recoverExpSharing config
               f = floatOutAcc config
           in
           AST.Aforeign ff (convertAfun a e f afun) (cvtA acc)

      Acond b acc1 acc2           -> AST.Acond (cvtE b) (cvtA acc1) (cvtA acc2)
      Awhile pred iter init       -> AST.Awhile (cvtAfun1 pred) (cvtAfun1 iter) (cvtA init)
      Atuple arrs                 -> AST.Atuple (convertSharingAtuple config alyt aenv' arrs)
      Aprj ix a                   -> AST.Aprj ix (cvtA a)
      Use array                   -> AST.Use (fromArr array)
      Unit e                      -> AST.Unit (cvtE e)
      Generate sh f               -> AST.Generate (cvtE sh) (cvtF1 f)
      Reshape e acc               -> AST.Reshape (cvtE e) (cvtA acc)
      Replicate ix acc            -> mkReplicate (cvtE ix) (cvtA acc)
      Slice acc ix                -> mkIndex (cvtA acc) (cvtE ix)
      Map f acc                   -> AST.Map (cvtF1 f) (cvtA acc)
      ZipWith f acc1 acc2         -> AST.ZipWith (cvtF2 f) (cvtA acc1) (cvtA acc2)
      Fold f e acc                -> AST.Fold (cvtF2 f) (cvtE e) (cvtA acc)
      Fold1 f acc                 -> AST.Fold1 (cvtF2 f) (cvtA acc)
      FoldSeg f e acc1 acc2       -> AST.FoldSeg (cvtF2 f) (cvtE e) (cvtA acc1) (cvtA acc2)
      Fold1Seg f acc1 acc2        -> AST.Fold1Seg (cvtF2 f) (cvtA acc1) (cvtA acc2)
      Scanl f e acc               -> AST.Scanl (cvtF2 f) (cvtE e) (cvtA acc)
      Scanl' f e acc              -> AST.Scanl' (cvtF2 f) (cvtE e) (cvtA acc)
      Scanl1 f acc                -> AST.Scanl1 (cvtF2 f) (cvtA acc)
      Scanr f e acc               -> AST.Scanr (cvtF2 f) (cvtE e) (cvtA acc)
      Scanr' f e acc              -> AST.Scanr' (cvtF2 f) (cvtE e) (cvtA acc)
      Scanr1 f acc                -> AST.Scanr1 (cvtF2 f) (cvtA acc)
      Permute f dftAcc perm acc   -> AST.Permute (cvtF2 f) (cvtA dftAcc) (cvtF1 perm) (cvtA acc)
      Backpermute newDim perm acc -> AST.Backpermute (cvtE newDim) (cvtF1 perm) (cvtA acc)
      Stencil stencil boundary acc
        -> AST.Stencil (convertSharingStencilFun1 config acc alyt aenv' stencil)
                       (convertBoundary boundary)
                       (cvtA acc)
      Stencil2 stencil bndy1 acc1 bndy2 acc2
        -> AST.Stencil2 (convertSharingStencilFun2 config acc1 acc2 alyt aenv' stencil)
                        (convertBoundary bndy1)
                        (cvtA acc1)
                        (convertBoundary bndy2)
                        (cvtA acc2)

convertSharingAfun1
    :: forall aenv a b. (Arrays a, Arrays b)
    => Config
    -> Layout aenv aenv
    -> [StableSharingAcc]
    -> (Acc a -> ScopedAcc b)
    -> OpenAfun aenv (a -> b)
convertSharingAfun1 config alyt aenv f
  = Alam (Abody (convertSharingAcc config alyt' aenv body))
      where
        alyt' = incLayout alyt `PushLayout` ZeroIdx
        body  = f undefined

convertSharingAtuple
    :: forall aenv a.
       Config
    -> Layout aenv aenv
    -> [StableSharingAcc]
    -> Tuple.Atuple ScopedAcc a
    -> Tuple.Atuple (AST.OpenAcc aenv) a
convertSharingAtuple config alyt aenv = cvt
  where
    cvt :: Tuple.Atuple ScopedAcc a' -> Tuple.Atuple (AST.OpenAcc aenv) a'
    cvt NilAtup         = NilAtup
    cvt (SnocAtup t a)  = cvt t `SnocAtup` convertSharingAcc config alyt aenv a


-- | Convert a boundary condition
--
convertBoundary :: Elt e => Boundary e -> Boundary (EltRepr e)
convertBoundary Clamp        = Clamp
convertBoundary Mirror       = Mirror
convertBoundary Wrap         = Wrap
convertBoundary (Constant e) = Constant (fromElt e)


-- Smart constructors to represent AST forms
--
mkIndex :: forall slix e aenv. (Slice slix, Elt e)
        => AST.OpenAcc                aenv (Array (FullShape  slix) e)
        -> AST.Exp                    aenv slix
        -> AST.PreOpenAcc AST.OpenAcc aenv (Array (SliceShape slix) e)
mkIndex = AST.Slice (sliceIndex slix)
  where
    slix = undefined :: slix

mkReplicate :: forall slix e aenv. (Slice slix, Elt e)
        => AST.Exp                    aenv slix
        -> AST.OpenAcc                aenv (Array (SliceShape slix) e)
        -> AST.PreOpenAcc AST.OpenAcc aenv (Array (FullShape  slix) e)
mkReplicate = AST.Replicate (sliceIndex slix)
  where
    slix = undefined :: slix



-- Scalar functions
-- ----------------

-- | Convert a closed scalar function to de Bruijn form while incorporating
-- sharing information.
--
-- The current design requires all free variables to be bound at the outermost
-- level --- we have no general apply term, and so lambdas are always outermost.
-- In higher-order abstract syntax, this represents an n-ary, polyvariadic
-- function.
--
convertFun :: Function f => Bool -> f -> AST.Fun () (FunctionR f)
convertFun shareExp =
  let config = Config False shareExp False
  in  convert config EmptyLayout


class Function f where
  type FunctionR f
  convert :: Config -> Layout env env -> f -> AST.OpenFun env () (FunctionR f)

instance (Elt a, Function r) => Function (Exp a -> r) where
  type FunctionR (Exp a -> r) = a -> FunctionR r
  --
  convert config lyt f
    = let x     = Exp $ Tag (sizeLayout lyt)
          lyt'  = incLayout lyt `PushLayout` ZeroIdx
      in
      Lam $ convert config lyt' (f x)

instance Elt b => Function (Exp b) where
  type FunctionR (Exp b) = b
  --
  convert config lyt body
    = let lvl    = sizeLayout lyt
          vars   = [lvl-1, lvl-2 .. 0]
      in
      Body $ convertOpenExp config lvl vars lyt body


-- Scalar expressions
-- ------------------

-- | Convert a closed scalar expression to de Bruijn form while incorporating
-- sharing information.
--
convertExp
    :: Elt e
    => Bool             -- ^ recover sharing of scalar expressions ?
    -> Exp e            -- ^ expression to be converted
    -> AST.Exp () e
convertExp shareExp exp
  = let config = Config False shareExp False
    in
    convertOpenExp config 0 [] EmptyLayout exp

convertOpenExp
    :: Elt e
    => Config
    -> Level            -- level of currently bound scalar variables
    -> [Level]          -- tags of bound scalar variables
    -> Layout env env
    -> Exp e
    -> AST.OpenExp env () e
convertOpenExp config lvl fvar lyt exp
  = let (sharingExp, initialEnv) = recoverSharingExp config lvl fvar exp
    in
    convertSharingExp config lyt EmptyLayout initialEnv [] sharingExp


-- | Convert an open expression with given environment layouts and sharing information into
-- de Bruijn form while recovering sharing at the same time (by introducing appropriate let
-- bindings).  The latter implements the third phase of sharing recovery.
--
-- The sharing environments 'env' and 'aenv' keep track of all currently bound sharing variables,
-- keeping them in reverse chronological order (outermost variable is at the end of the list).
--
convertSharingExp
    :: forall t env aenv. Elt t
    => Config
    -> Layout env  env          -- scalar environment
    -> Layout aenv aenv         -- array environment
    -> [StableSharingExp]       -- currently bound sharing variables of expressions
    -> [StableSharingAcc]       -- currently bound sharing variables of array computations
    -> ScopedExp t              -- expression to be converted
    -> AST.OpenExp env aenv t
convertSharingExp config lyt alyt env aenv exp@(ScopedExp lams _) = cvt exp
  where
    -- scalar environment with any lambda bound variables this expression is rooted in
    env' = lams ++ env

    cvt :: Elt t' => ScopedExp t' -> AST.OpenExp env aenv t'
    cvt (ScopedExp _ (VarSharing se))
      | Just i <- findIndex (matchStableExp se) env'
      = AST.Var (prjIdx (ctxt ++ "; i = " ++ show i) i lyt)
      | null env'
      = error $ "Cyclic definition of a value of type 'Exp' (sa = " ++ show (hashStableNameHeight se) ++ ")"
      | otherwise
      = INTERNAL_ERROR(error) "convertSharingExp" err
      where
        ctxt = "shared 'Exp' tree with stable name " ++ show (hashStableNameHeight se)
        err  = "inconsistent valuation @ " ++ ctxt ++ ";\n  env' = " ++ show env'
    cvt (ScopedExp _ (LetSharing se@(StableSharingExp _ boundExp) bodyExp))
      = let lyt' = incLayout lyt `PushLayout` ZeroIdx
        in
        AST.Let (cvt (ScopedExp [] boundExp)) (convertSharingExp config lyt' alyt (se:env') aenv bodyExp)
    cvt (ScopedExp _ (ExpSharing _ pexp))
      = case pexp of
          Tag i                 -> AST.Var (prjIdx ("de Bruijn conversion tag " ++ show i) i lyt)
          Const v               -> AST.Const (fromElt v)
          Tuple tup             -> AST.Tuple (cvtT tup)
          Prj idx e             -> AST.Prj idx (cvt e)
          IndexNil              -> AST.IndexNil
          IndexCons ix i        -> AST.IndexCons (cvt ix) (cvt i)
          IndexHead i           -> AST.IndexHead (cvt i)
          IndexTail ix          -> AST.IndexTail (cvt ix)
          IndexAny              -> AST.IndexAny
          ToIndex sh ix         -> AST.ToIndex (cvt sh) (cvt ix)
          FromIndex sh e        -> AST.FromIndex (cvt sh) (cvt e)
          Cond e1 e2 e3         -> AST.Cond (cvt e1) (cvt e2) (cvt e3)
          While p it i          -> AST.While (cvtFun1 p) (cvtFun1 it) (cvt i)
          PrimConst c           -> AST.PrimConst c
          PrimApp f e           -> cvtPrimFun f (cvt e)
          Index a e             -> AST.Index (cvtA a) (cvt e)
          LinearIndex a i       -> AST.LinearIndex (cvtA a) (cvt i)
          Shape a               -> AST.Shape (cvtA a)
          ShapeSize e           -> AST.ShapeSize (cvt e)
          Intersect sh1 sh2     -> AST.Intersect (cvt sh1) (cvt sh2)
          Foreign ff f e        -> AST.Foreign ff (convertFun (recoverExpSharing config) f) (cvt e)

    cvtA :: Arrays a => ScopedAcc a -> AST.OpenAcc aenv a
    cvtA = convertSharingAcc config alyt aenv

    cvtT :: Tuple.Tuple ScopedExp tup -> Tuple.Tuple (AST.OpenExp env aenv) tup
    cvtT = convertSharingTuple config lyt alyt env' aenv

    cvtFun1 :: (Elt a, Elt b) => (Exp a -> ScopedExp b) -> AST.OpenFun env aenv (a -> b)
    cvtFun1 f = Lam (Body (convertSharingExp config lyt' alyt env' aenv body))
      where
        lyt' = incLayout lyt `PushLayout` ZeroIdx
        body = f undefined

    -- Push primitive function applications down through let bindings so that
    -- they are adjacent to their arguments. It looks a bit nicer this way.
    --
    cvtPrimFun :: (Elt a, Elt r)
               => AST.PrimFun (a -> r) -> AST.OpenExp env' aenv' a -> AST.OpenExp env' aenv' r
    cvtPrimFun f e = case e of
      AST.Let bnd body    -> AST.Let bnd (cvtPrimFun f body)
      x                   -> AST.PrimApp f x

-- | Convert a tuple expression
--
convertSharingTuple
    :: Config
    -> Layout env env
    -> Layout aenv aenv
    -> [StableSharingExp]                 -- currently bound scalar sharing-variables
    -> [StableSharingAcc]                 -- currently bound array sharing-variables
    -> Tuple.Tuple ScopedExp t
    -> Tuple.Tuple (AST.OpenExp env aenv) t
convertSharingTuple config lyt alyt env aenv tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> convertSharingTuple config lyt alyt env aenv t
         `SnocTup` convertSharingExp   config lyt alyt env aenv e

-- | Convert a unary functions
--
convertSharingFun1
    :: forall a b aenv. (Elt a, Elt b)
    => Config
    -> Layout aenv aenv
    -> [StableSharingAcc]       -- currently bound array sharing-variables
    -> (Exp a -> ScopedExp b)
    -> AST.Fun aenv (a -> b)
convertSharingFun1 config alyt aenv f = Lam (Body openF)
  where
    a               = Exp undefined             -- the 'tag' was already embedded in Phase 1
    lyt             = EmptyLayout
                      `PushLayout`
                      (ZeroIdx :: Idx ((), a) a)
    openF           = convertSharingExp config lyt alyt [] aenv (f a)

-- | Convert a binary functions
--
convertSharingFun2
    :: forall a b c aenv. (Elt a, Elt b, Elt c)
    => Config
    -> Layout aenv aenv
    -> [StableSharingAcc]       -- currently bound array sharing-variables
    -> (Exp a -> Exp b -> ScopedExp c)
    -> AST.Fun aenv (a -> b -> c)
convertSharingFun2 config alyt aenv f = Lam (Lam (Body openF))
  where
    a               = Exp undefined
    b               = Exp undefined
    lyt             = EmptyLayout
                      `PushLayout`
                      (SuccIdx ZeroIdx :: Idx (((), a), b) a)
                      `PushLayout`
                      (ZeroIdx         :: Idx (((), a), b) b)
    openF           = convertSharingExp config lyt alyt [] aenv (f a b)

-- | Convert a unary stencil function
--
convertSharingStencilFun1
    :: forall sh a stencil b aenv. (Elt a, Stencil sh a stencil, Elt b)
    => Config
    -> ScopedAcc (Array sh a)          -- just passed to fix the type variables
    -> Layout aenv aenv
    -> [StableSharingAcc]               -- currently bound array sharing-variables
    -> (stencil -> ScopedExp b)
    -> AST.Fun aenv (StencilRepr sh stencil -> b)
convertSharingStencilFun1 config _ alyt aenv stencilFun = Lam (Body openStencilFun)
  where
    stencil = Exp undefined :: Exp (StencilRepr sh stencil)
    lyt     = EmptyLayout
              `PushLayout`
              (ZeroIdx :: Idx ((), StencilRepr sh stencil)
                              (StencilRepr sh stencil))

    body = stencilFun (stencilPrj (undefined::sh) (undefined::a) stencil)
    openStencilFun  = convertSharingExp config lyt alyt [] aenv body

-- | Convert a binary stencil function
--
convertSharingStencilFun2
    :: forall sh a b stencil1 stencil2 c aenv.
       (Elt a, Stencil sh a stencil1,
        Elt b, Stencil sh b stencil2,
        Elt c)
    => Config
    -> ScopedAcc (Array sh a)          -- just passed to fix the type variables
    -> ScopedAcc (Array sh b)          -- just passed to fix the type variables
    -> Layout aenv aenv
    -> [StableSharingAcc]               -- currently bound array sharing-variables
    -> (stencil1 -> stencil2 -> ScopedExp c)
    -> AST.Fun aenv (StencilRepr sh stencil1 -> StencilRepr sh stencil2 -> c)
convertSharingStencilFun2 config _ _ alyt aenv stencilFun = Lam (Lam (Body openStencilFun))
  where
    stencil1 = Exp undefined :: Exp (StencilRepr sh stencil1)
    stencil2 = Exp undefined :: Exp (StencilRepr sh stencil2)
    lyt     = EmptyLayout
              `PushLayout`
              (SuccIdx ZeroIdx :: Idx (((), StencilRepr sh stencil1),
                                            StencilRepr sh stencil2)
                                       (StencilRepr sh stencil1))
              `PushLayout`
              (ZeroIdx         :: Idx (((), StencilRepr sh stencil1),
                                            StencilRepr sh stencil2)
                                       (StencilRepr sh stencil2))

    body = stencilFun (stencilPrj (undefined::sh) (undefined::a) stencil1)
                      (stencilPrj (undefined::sh) (undefined::b) stencil2)
    openStencilFun  = convertSharingExp config lyt alyt [] aenv body


-- Sharing recovery
-- ================

-- Sharing recovery proceeds in two phases:
--
-- /Phase One: build the occurrence map/
--
-- This is a top-down traversal of the AST that computes a map from AST nodes to the number of
-- occurrences of that AST node in the overall Accelerate program.  An occurrences count of two or
-- more indicates sharing.
--
-- IMPORTANT: To avoid unfolding the sharing, we do not descent into subtrees that we have
--   previously encountered.  Hence, the complexity is proportional to the number of nodes in the
--   tree /with/ sharing.  Consequently, the occurrence count is that in the tree with sharing
--   as well.
--
-- During computation of the occurrences, the tree is annotated with stable names on every node
-- using 'AccSharing' constructors and all but the first occurrence of shared subtrees are pruned
-- using 'AvarSharing' constructors (see 'SharingAcc' below).  This phase is impure as it is based
-- on stable names.
--
-- We use a hash table (instead of 'Data.Map') as computing stable names forces us to live in IO
-- anyway.  Once, the computation of occurrence counts is complete, we freeze the hash table into
-- a 'Data.Map'.
--
-- (Implemented by 'makeOccMap*'.)
--
-- /Phase Two: determine scopes and inject sharing information/
--
-- This is a bottom-up traversal that determines the scope for every binding to be introduced
-- to share a subterm.  It uses the occurrence map to determine, for every shared subtree, the
-- lowest AST node at which the binding for that shared subtree can be placed (using a
-- 'AletSharing' constructor)— it's the meet of all the shared subtree occurrences.
--
-- The second phase is also replacing the first occurrence of each shared subtree with a
-- 'AvarSharing' node and floats the shared subtree up to its binding point.
--
--  (Implemented by 'determineScopes*'.)
--
-- /Sharing recovery for expressions/
--
-- We recover sharing for each expression (including function bodies) independently of any other
-- expression — i.e., we cannot share scalar expressions across array computations.  Hence, during
-- Phase One, we mark all scalar expression nodes with a stable name and compute one occurrence map
-- for every scalar expression (including functions) that occurs in an array computation.  These
-- occurrence maps are added to the root of scalar expressions using 'RootExp'.
--
-- NB: We do not need to worry sharing recovery will try to float a shared subexpression past a
--     binder that occurs in that subexpression.  Why?  Otherwise, the binder would already occur
--     out of scope in the original source program.
--
-- /Lambda bound variables/
--
-- During sharing recovery, lambda bound variables appear in the form of 'Atag' and 'Tag' data
-- constructors.  The tag values are determined during Phase One of sharing recovery by computing
-- the /level/ of each variable at its binding occurrence.  The level at the root of the AST is 0
-- and increases by one with each lambda on each path through the AST.

-- Stable names
-- ------------

-- Opaque stable name for AST nodes — used to key the occurrence map.
--
data StableASTName c where
  StableASTName :: (Typeable1 c, Typeable t) => StableName (c t) -> StableASTName c

instance Show (StableASTName c) where
  show (StableASTName sn) = show $ hashStableName sn

instance Eq (StableASTName c) where
  StableASTName sn1 == StableASTName sn2
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

instance Hashable (StableASTName c) where
  hashWithSalt s (StableASTName sn) = hashWithSalt s sn

makeStableAST :: c t -> IO (StableName (c t))
makeStableAST e = e `seq` makeStableName e

-- Stable name for an AST node including the height of the AST representing the array computation.
--
data StableNameHeight t = StableNameHeight (StableName t) Int

instance Eq (StableNameHeight t) where
  (StableNameHeight sn1 _) == (StableNameHeight sn2 _) = sn1 == sn2

higherSNH :: StableNameHeight t1 -> StableNameHeight t2 -> Bool
StableNameHeight _ h1 `higherSNH` StableNameHeight _ h2 = h1 > h2

hashStableNameHeight :: StableNameHeight t -> Int
hashStableNameHeight (StableNameHeight sn _) = hashStableName sn

-- Mutable occurrence map
-- ----------------------

-- Hash table keyed on the stable names of array computations.
--
type HashTable key val = Hash.BasicHashTable key val
type ASTHashTable c v  = HashTable (StableASTName c) v

-- Mutable hashtable version of the occurrence map, which associates each AST node with an
-- occurrence count and the height of the AST.
--
type OccMapHash c = ASTHashTable c (Int, Int)

-- Create a new hash table keyed on AST nodes.
--
newASTHashTable :: IO (ASTHashTable c v)
newASTHashTable = Hash.new

-- Enter one AST node occurrence into an occurrence map.  Returns 'Just h' if this is a repeated
-- occurrence and the height of the repeatedly occurring AST is 'h'.
--
-- If this is the first occurrence, the 'height' *argument* must provide the height of the AST;
-- otherwise, the height will be *extracted* from the occurrence map.  In the latter case, this
-- function yields the AST height.
--
enterOcc :: OccMapHash c -> StableASTName c -> Int -> IO (Maybe Int)
enterOcc occMap sa height
  = do
      entry <- Hash.lookup occMap sa
      case entry of
        Nothing           -> Hash.insert occMap sa (1    , height)  >> return Nothing
        Just (n, heightS) -> Hash.insert occMap sa (n + 1, heightS) >> return (Just heightS)

-- Immutable occurrence map
-- ------------------------

-- Immutable version of the occurrence map (storing the occurrence count only, not the height).  We
-- use the 'StableName' hash to index an 'IntMap' and disambiguate 'StableName's with identical
-- hashes explicitly, storing them in a list in the 'IntMap'.
--
type OccMap c = IntMap.IntMap [(StableASTName c, Int)]

-- Turn a mutable into an immutable occurrence map.
--
freezeOccMap :: OccMapHash c -> IO (OccMap c)
freezeOccMap oc
  = do
      ocl <- Hash.toList oc
      traceChunk "OccMap" (show ocl)

      return . IntMap.fromList
             . map (\kvs -> (key (head kvs), kvs))
             . groupBy sameKey
             . map dropHeight
             $ ocl
  where
    key (StableASTName sn, _) = hashStableName sn
    sameKey kv1 kv2           = key kv1 == key kv2
    dropHeight (k, (cnt, _))  = (k, cnt)

-- Look up the occurrence map keyed by array computations using a stable name.  If the key does
-- not exist in the map, return an occurrence count of '1'.
--
lookupWithASTName :: OccMap c -> StableASTName c -> Int
lookupWithASTName oc sa@(StableASTName sn)
  = fromMaybe 1 $ IntMap.lookup (hashStableName sn) oc >>= Prelude.lookup sa

-- Look up the occurrence map keyed by array computations using a sharing array computation.  If an
-- the key does not exist in the map, return an occurrence count of '1'.
--
lookupWithSharingAcc :: OccMap Acc -> StableSharingAcc -> Int
lookupWithSharingAcc oc (StableSharingAcc (StableNameHeight sn _) _)
  = lookupWithASTName oc (StableASTName sn)

-- Look up the occurrence map keyed by scalar expressions using a sharing expression.  If an
-- the key does not exist in the map, return an occurrence count of '1'.
--
lookupWithSharingExp :: OccMap Exp -> StableSharingExp -> Int
lookupWithSharingExp oc (StableSharingExp (StableNameHeight sn _) _)
  = lookupWithASTName oc (StableASTName sn)

-- Stable 'Acc' nodes
-- ------------------

-- Stable name for 'Acc' nodes including the height of the AST.
--
type StableAccName arrs = StableNameHeight (Acc arrs)

-- Interleave sharing annotations into an array computation AST.  Subtrees can be marked as being
-- represented by variable (binding a shared subtree) using 'AvarSharing' and as being prefixed by
-- a let binding (for a shared subtree) using 'AletSharing'.
--
data SharingAcc acc exp arrs where
  AvarSharing :: Arrays arrs
              => StableAccName arrs                        -> SharingAcc acc exp arrs
  AletSharing :: StableSharingAcc -> acc arrs              -> SharingAcc acc exp arrs
  AccSharing  :: Arrays arrs
              => StableAccName arrs -> PreAcc acc exp arrs -> SharingAcc acc exp arrs

-- Array expression with sharing but shared values have not been scoped; i.e. no let bindings. If
-- the expression is rooted in a function, the list contains the tags of the variables bound by the
-- immediate surrounding lambdas.
data UnscopedAcc t = UnscopedAcc [Int] (SharingAcc UnscopedAcc RootExp t)

-- Array expression with sharing. For expressions rooted in functions the list holds a sorted
-- environment corresponding to the variables bound in the immediate surounding lambdas.
data ScopedAcc t = ScopedAcc [StableSharingAcc] (SharingAcc ScopedAcc ScopedExp t)

-- Stable name for an array computation associated with its sharing-annotated version.
--
data StableSharingAcc where
  StableSharingAcc :: Arrays arrs
                   => StableAccName arrs
                   -> SharingAcc ScopedAcc ScopedExp arrs
                   -> StableSharingAcc

instance Show StableSharingAcc where
  show (StableSharingAcc sn _) = show $ hashStableNameHeight sn

instance Eq StableSharingAcc where
  StableSharingAcc sn1 _ == StableSharingAcc sn2 _
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

higherSSA :: StableSharingAcc -> StableSharingAcc -> Bool
StableSharingAcc sn1 _ `higherSSA` StableSharingAcc sn2 _ = sn1 `higherSNH` sn2

-- Test whether the given stable names matches an array computation with sharing.
--
matchStableAcc :: Typeable arrs => StableAccName arrs -> StableSharingAcc -> Bool
matchStableAcc sn1 (StableSharingAcc sn2 _)
  | Just sn1' <- gcast sn1 = sn1' == sn2
  | otherwise              = False

-- Dummy entry for environments to be used for unused variables.
--
noStableAccName :: StableAccName arrs
noStableAccName = unsafePerformIO $ StableNameHeight <$> makeStableName undefined <*> pure 0

-- Stable 'Exp' nodes
-- ------------------

-- Stable name for 'Exp' nodes including the height of the AST.
--
type StableExpName t = StableNameHeight (Exp t)

-- Interleave sharing annotations into a scalar expressions AST in the same manner as 'SharingAcc'
-- do for array computations.
--
data SharingExp (acc :: * -> *) exp t where
  VarSharing :: Elt t
             => StableExpName t                            -> SharingExp acc exp t
  LetSharing :: StableSharingExp -> exp t                  -> SharingExp acc exp t
  ExpSharing :: Elt t
             => StableExpName t -> PreExp acc exp t -> SharingExp acc exp t

-- Specifies a scalar expression AST with sharing annotations but no scoping; i.e. no LetSharing
-- constructors. If the expression is rooted in a function, the list contains the tags of the
-- variables bound by the immediate surrounding lambdas.
data UnscopedExp t = UnscopedExp [Int] (SharingExp UnscopedAcc UnscopedExp t)

-- Specifies a scalar expression AST with sharing. For expressions rooted in functions the list
-- holds a sorted environment corresponding to the variables bound in the immediate surounding
-- lambdas.
data ScopedExp t = ScopedExp [StableSharingExp] (SharingExp ScopedAcc ScopedExp t)

-- Expressions rooted in 'Acc' computations.
--
-- * When counting occurrences, the root of every expression embedded in an 'Acc' is annotated by
--   an occurrence map for that one expression (excluding any subterms that are rooted in embedded
--   'Acc's.)
--
data RootExp t = RootExp (OccMap Exp) (UnscopedExp t)

-- Stable name for an expression associated with its sharing-annotated version.
--
data StableSharingExp where
  StableSharingExp :: Elt t => StableExpName t -> SharingExp ScopedAcc ScopedExp t -> StableSharingExp

instance Show StableSharingExp where
  show (StableSharingExp sn _) = show $ hashStableNameHeight sn

instance Eq StableSharingExp where
  StableSharingExp sn1 _ == StableSharingExp sn2 _
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

higherSSE :: StableSharingExp -> StableSharingExp -> Bool
StableSharingExp sn1 _ `higherSSE` StableSharingExp sn2 _ = sn1 `higherSNH` sn2

-- Test whether the given stable names matches an expression with sharing.
--
matchStableExp :: Typeable t => StableExpName t -> StableSharingExp -> Bool
matchStableExp sn1 (StableSharingExp sn2 _)
  | Just sn1' <- gcast sn1 = sn1' == sn2
  | otherwise              = False

-- Dummy entry for environments to be used for unused variables.
--
noStableExpName :: StableExpName t
noStableExpName = unsafePerformIO $ StableNameHeight <$> makeStableName undefined <*> pure 0


-- Occurrence counting
-- ===================

-- Compute the 'Acc' occurrence map, marks all nodes (both 'Acc' and 'Exp' nodes) with stable names,
-- and drop repeated occurrences of shared 'Acc' and 'Exp' subtrees (Phase One).
--
-- We compute a single 'Acc' occurrence map for the whole AST, but one 'Exp' occurrence map for each
-- sub-expression rooted in an 'Acc' operation.  This is as we cannot float 'Exp' subtrees across
-- 'Acc' operations, but we can float 'Acc' subtrees out of 'Exp' expressions.
--
-- Note [Traversing functions and side effects]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We need to descent into function bodies to build the 'OccMap' with all occurrences in the
-- function bodies.  Due to the side effects in the construction of the occurrence map and, more
-- importantly, the dependence of the second phase on /global/ occurrence information, we may not
-- delay the body traversals by putting them under a lambda.  Hence, we apply each function, to
-- traverse its body and use a /dummy abstraction/ of the result.
--
-- For example, given a function 'f', we traverse 'f (Tag 0)', which yields a transformed body 'e'.
-- As the result of the traversal of the overall function, we use 'const e'.  Hence, it is crucial
-- that the 'Tag' supplied during the initial traversal is already the one required by the HOAS to
-- de Bruijn conversion in 'convertSharingAcc' — any subsequent application of 'const e' will only
-- yield 'e' with the embedded 'Tag 0' of the original application.  During sharing recovery, we
-- float /all/ free variables ('Atag' and 'Tag') out to construct the initial environment for
-- producing de Bruijn indices, which replaces them by 'AvarSharing' or 'VarSharing' nodes.  Hence,
-- the tag values only serve the purpose of determining the ordering in that initial environment.
-- They are /not/ directly used to compute the de Brujin indices.
--
makeOccMapAcc
    :: Typeable arrs
    => Config
    -> Level
    -> Acc arrs
    -> IO (UnscopedAcc arrs, OccMap Acc)
makeOccMapAcc config lvl acc = do
  traceLine "makeOccMapAcc" "Enter"
  accOccMap             <- newASTHashTable
  (acc', _)             <- makeOccMapSharingAcc config accOccMap lvl acc
  frozenAccOccMap       <- freezeOccMap accOccMap
  traceLine "makeOccMapAcc" "Exit"
  return (acc', frozenAccOccMap)


makeOccMapSharingAcc
    :: Typeable arrs
    => Config
    -> OccMapHash Acc
    -> Level
    -> Acc arrs
    -> IO (UnscopedAcc arrs, Int)
makeOccMapSharingAcc config accOccMap = traverseAcc
  where
    traverseFun1 :: (Elt a, Typeable b) => Level -> (Exp a -> Exp b) -> IO (Exp a -> RootExp b, Int)
    traverseFun1 = makeOccMapFun1 config accOccMap

    traverseFun2 :: (Elt a, Elt b, Typeable c)
                 => Level
                 -> (Exp a -> Exp b -> Exp c)
                 -> IO (Exp a -> Exp b -> RootExp c, Int)
    traverseFun2 = makeOccMapFun2 config accOccMap

    traverseAfun1 :: (Arrays a, Typeable b) => Level -> (Acc a -> Acc b) -> IO (Acc a -> UnscopedAcc b, Int)
    traverseAfun1 = makeOccMapAfun1 config accOccMap

    traverseExp :: Typeable e => Level -> Exp e -> IO (RootExp e, Int)
    traverseExp = makeOccMapExp config accOccMap

    traverseAcc :: forall arrs. Typeable arrs => Level -> Acc arrs -> IO (UnscopedAcc arrs, Int)
    traverseAcc lvl acc@(Acc pacc)
      = mfix $ \ ~(_, height) -> do
          -- Compute stable name and enter it into the occurrence map
          --
          sn                         <- makeStableAST acc
          heightIfRepeatedOccurrence <- enterOcc accOccMap (StableASTName sn) height

          traceLine (showPreAccOp pacc) $ do
            let hash = show (hashStableName sn)
            case heightIfRepeatedOccurrence of
              Just height -> "REPEATED occurrence (sn = " ++ hash ++ "; height = " ++ show height ++ ")"
              Nothing     -> "first occurrence (sn = " ++ hash ++ ")"

          -- Reconstruct the computation in shared form.
          --
          -- In case of a repeated occurrence, the height comes from the occurrence map; otherwise
          -- it is computed by the traversal function passed in 'newAcc'. See also 'enterOcc'.
          --
          -- NB: This function can only be used in the case alternatives below; outside of the
          --     case we cannot discharge the 'Arrays arrs' constraint.
          --
          let reconstruct :: Arrays arrs
                          => IO (PreAcc UnscopedAcc RootExp arrs, Int)
                          -> IO (UnscopedAcc arrs, Int)
              reconstruct newAcc
                = case heightIfRepeatedOccurrence of
                    Just height | recoverAccSharing config
                      -> return (UnscopedAcc [] (AvarSharing (StableNameHeight sn height)), height)
                    _ -> do (acc, height) <- newAcc
                            return (UnscopedAcc [] (AccSharing (StableNameHeight sn height) acc), height)

          case pacc of
            Atag i                      -> reconstruct $ return (Atag i, 0)           -- height is 0!
            Pipe afun1 afun2 acc        -> reconstruct $ do
                                             (afun1', h1) <- traverseAfun1 lvl afun1
                                             (afun2', h2) <- traverseAfun1 lvl afun2
                                             (acc', h3)   <- traverseAcc lvl acc
                                             return (Pipe afun1' afun2' acc'
                                                    , h1 `max` h2 `max` h3 + 1)
            Aforeign ff afun acc        -> reconstruct $ travA (Aforeign ff afun) acc
            Acond e acc1 acc2           -> reconstruct $ do
                                             (e'   , h1) <- traverseExp lvl e
                                             (acc1', h2) <- traverseAcc lvl acc1
                                             (acc2', h3) <- traverseAcc lvl acc2
                                             return (Acond e' acc1' acc2', h1 `max` h2 `max` h3 + 1)
            Awhile pred iter init       -> reconstruct $ do
                                             (pred', h1) <- traverseAfun1 lvl pred
                                             (iter', h2) <- traverseAfun1 lvl iter
                                             (init', h3) <- traverseAcc lvl init
                                             return (Awhile pred' iter' init'
                                                    , h1 `max` h2 `max` h3 + 1)

            Atuple tup                  -> reconstruct $ do
                                             (tup', h) <- travAtup tup
                                             return (Atuple tup', h)
            Aprj ix a                   -> reconstruct $ travA (Aprj ix) a

            Use arr                     -> reconstruct $ return (Use arr, 1)
            Unit e                      -> reconstruct $ do
                                             (e', h) <- traverseExp lvl e
                                             return (Unit e', h + 1)
            Generate e f                -> reconstruct $ do
                                             (e', h1) <- traverseExp lvl e
                                             (f', h2) <- traverseFun1 lvl f
                                             return (Generate e' f', h1 `max` h2 + 1)
            Reshape e acc               -> reconstruct $ travEA Reshape e acc
            Replicate e acc             -> reconstruct $ travEA Replicate e acc
            Slice acc e                 -> reconstruct $ travEA (flip Slice) e acc
            Map f acc                   -> reconstruct $ do
                                             (f'  , h1) <- traverseFun1 lvl f
                                             (acc', h2) <- traverseAcc lvl acc
                                             return (Map f' acc', h1 `max` h2 + 1)
            ZipWith f acc1 acc2         -> reconstruct $ travF2A2 ZipWith f acc1 acc2
            Fold f e acc                -> reconstruct $ travF2EA Fold f e acc
            Fold1 f acc                 -> reconstruct $ travF2A Fold1 f acc
            FoldSeg f e acc1 acc2       -> reconstruct $ do
                                             (f'   , h1) <- traverseFun2 lvl f
                                             (e'   , h2) <- traverseExp lvl e
                                             (acc1', h3) <- traverseAcc lvl acc1
                                             (acc2', h4) <- traverseAcc lvl acc2
                                             return (FoldSeg f' e' acc1' acc2',
                                                     h1 `max` h2 `max` h3 `max` h4 + 1)
            Fold1Seg f acc1 acc2        -> reconstruct $ travF2A2 Fold1Seg f acc1 acc2
            Scanl f e acc               -> reconstruct $ travF2EA Scanl f e acc
            Scanl' f e acc              -> reconstruct $ travF2EA Scanl' f e acc
            Scanl1 f acc                -> reconstruct $ travF2A Scanl1 f acc
            Scanr f e acc               -> reconstruct $ travF2EA Scanr f e acc
            Scanr' f e acc              -> reconstruct $ travF2EA Scanr' f e acc
            Scanr1 f acc                -> reconstruct $ travF2A Scanr1 f acc
            Permute c acc1 p acc2       -> reconstruct $ do
                                             (c'   , h1) <- traverseFun2 lvl c
                                             (p'   , h2) <- traverseFun1 lvl p
                                             (acc1', h3) <- traverseAcc lvl acc1
                                             (acc2', h4) <- traverseAcc lvl acc2
                                             return (Permute c' acc1' p' acc2',
                                                     h1 `max` h2 `max` h3 `max` h4 + 1)
            Backpermute e p acc         -> reconstruct $ do
                                             (e'  , h1) <- traverseExp lvl e
                                             (p'  , h2) <- traverseFun1 lvl p
                                             (acc', h3) <- traverseAcc lvl acc
                                             return (Backpermute e' p' acc', h1 `max` h2 `max` h3 + 1)
            Stencil s bnd acc           -> reconstruct $ do
                                             (s'  , h1) <- makeOccMapStencil1 config accOccMap acc lvl s
                                             (acc', h2) <- traverseAcc lvl acc
                                             return (Stencil s' bnd acc', h1 `max` h2 + 1)
            Stencil2 s bnd1 acc1
                       bnd2 acc2        -> reconstruct $ do
                                             (s'   , h1) <- makeOccMapStencil2 config accOccMap acc1 acc2 lvl s
                                             (acc1', h2) <- traverseAcc lvl acc1
                                             (acc2', h3) <- traverseAcc lvl acc2
                                             return (Stencil2 s' bnd1 acc1' bnd2 acc2',
                                                     h1 `max` h2 `max` h3 + 1)

      where
        travA :: Arrays arrs'
              => (UnscopedAcc arrs' -> PreAcc UnscopedAcc RootExp arrs)
              -> Acc arrs' -> IO (PreAcc UnscopedAcc RootExp arrs, Int)
        travA c acc
          = do
              (acc', h) <- traverseAcc lvl acc
              return (c acc', h + 1)

        travEA :: (Typeable b, Arrays arrs')
               => (RootExp b -> UnscopedAcc arrs' -> PreAcc UnscopedAcc RootExp arrs)
               -> Exp b -> Acc arrs' -> IO (PreAcc UnscopedAcc RootExp arrs, Int)
        travEA c exp acc
          = do
              (exp', h1) <- traverseExp lvl exp
              (acc', h2) <- traverseAcc lvl acc
              return (c exp' acc', h1 `max` h2 + 1)

        travF2A :: (Elt b, Elt c, Typeable d, Arrays arrs')
                => ((Exp b -> Exp c -> RootExp d) -> UnscopedAcc arrs'
                    -> PreAcc UnscopedAcc RootExp arrs)
                -> (Exp b -> Exp c -> Exp d) -> Acc arrs'
                -> IO (PreAcc UnscopedAcc RootExp arrs, Int)
        travF2A c fun acc
          = do
              (fun', h1) <- traverseFun2 lvl fun
              (acc', h2) <- traverseAcc lvl acc
              return (c fun' acc', h1 `max` h2 + 1)

        travF2EA :: (Elt b, Elt c, Typeable d, Typeable e, Arrays arrs')
                 => ((Exp b -> Exp c -> RootExp d) -> RootExp e -> UnscopedAcc arrs' -> PreAcc UnscopedAcc RootExp arrs)
                 -> (Exp b -> Exp c -> Exp d) -> Exp e -> Acc arrs'
                 -> IO (PreAcc UnscopedAcc RootExp arrs, Int)
        travF2EA c fun exp acc
          = do
              (fun', h1) <- traverseFun2 lvl fun
              (exp', h2) <- traverseExp lvl exp
              (acc', h3) <- traverseAcc lvl acc
              return (c fun' exp' acc', h1 `max` h2 `max` h3 + 1)

        travF2A2 :: (Elt b, Elt c, Typeable d, Arrays arrs1, Arrays arrs2)
                 => ((Exp b -> Exp c -> RootExp d) -> UnscopedAcc arrs1 -> UnscopedAcc arrs2 -> PreAcc UnscopedAcc RootExp arrs)
                 -> (Exp b -> Exp c -> Exp d) -> Acc arrs1 -> Acc arrs2
                 -> IO (PreAcc UnscopedAcc RootExp arrs, Int)
        travF2A2 c fun acc1 acc2
          = do
              (fun' , h1) <- traverseFun2 lvl fun
              (acc1', h2) <- traverseAcc lvl acc1
              (acc2', h3) <- traverseAcc lvl acc2
              return (c fun' acc1' acc2', h1 `max` h2 `max` h3 + 1)

        travAtup :: Tuple.Atuple Acc a
                 -> IO (Tuple.Atuple UnscopedAcc a, Int)
        travAtup NilAtup          = return (NilAtup, 1)
        travAtup (SnocAtup tup a) = do
          (tup', h1) <- travAtup tup
          (a',   h2) <- traverseAcc lvl a
          return (SnocAtup tup' a', h1 `max` h2 + 1)

makeOccMapAfun1 :: (Arrays a, Typeable b)
                => Config
                -> OccMapHash Acc
                -> Level
                -> (Acc a -> Acc b)
                -> IO (Acc a -> UnscopedAcc b, Int)
makeOccMapAfun1 config accOccMap lvl f = do
  let x = Acc (Atag lvl)
  --
  (UnscopedAcc [] body, height) <- makeOccMapSharingAcc config accOccMap (lvl+1) (f x)
  return (const (UnscopedAcc [lvl] body), height)


-- Generate occupancy information for scalar functions and expressions. Helper
-- functions wrapping around 'makeOccMapRootExp' with more specific types.
--
-- See Note [Traversing functions and side effects]
--
makeOccMapExp
    :: Typeable e
    => Config
    -> OccMapHash Acc
    -> Level
    -> Exp e
    -> IO (RootExp e, Int)
makeOccMapExp config accOccMap lvl = makeOccMapRootExp config accOccMap lvl []

makeOccMapFun1
    :: (Elt a, Typeable b)
    => Config
    -> OccMapHash Acc
    -> Level
    -> (Exp a -> Exp b)
    -> IO (Exp a -> RootExp b, Int)
makeOccMapFun1 config accOccMap lvl f = do
  let x = Exp (Tag lvl)
  --
  (body, height) <- makeOccMapRootExp config accOccMap (lvl+1) [lvl] (f x)
  return (const body, height)

makeOccMapFun2
    :: (Elt a, Elt b, Typeable c)
    => Config
    -> OccMapHash Acc
    -> Level
    -> (Exp a -> Exp b -> Exp c)
    -> IO (Exp a -> Exp b -> RootExp c, Int)
makeOccMapFun2 config accOccMap lvl f = do
  let x = Exp (Tag (lvl+1))
      y = Exp (Tag lvl)
  --
  (body, height) <- makeOccMapRootExp config accOccMap (lvl+2) [lvl, lvl+1] (f x y)
  return (\_ _ -> body, height)

makeOccMapStencil1
    :: forall sh a b stencil. (Stencil sh a stencil, Typeable b)
    => Config
    -> OccMapHash Acc
    -> Acc (Array sh a)         {- dummy -}
    -> Level
    -> (stencil -> Exp b)
    -> IO (stencil -> RootExp b, Int)
makeOccMapStencil1 config accOccMap _ lvl stencil = do
  let x = Exp (Tag lvl)
      f = stencil . stencilPrj (undefined::sh) (undefined::a)
  --
  (body, height) <- makeOccMapRootExp config accOccMap (lvl+1) [lvl] (f x)
  return (const body, height)

makeOccMapStencil2
    :: forall sh a b c stencil1 stencil2. (Stencil sh a stencil1, Stencil sh b stencil2, Typeable c)
    => Config
    -> OccMapHash Acc
    -> Acc (Array sh a)         {- dummy -}
    -> Acc (Array sh b)         {- dummy -}
    -> Level
    -> (stencil1 -> stencil2 -> Exp c)
    -> IO (stencil1 -> stencil2 -> RootExp c, Int)
makeOccMapStencil2 config accOccMap _ _ lvl stencil = do
  let x         = Exp (Tag (lvl+1))
      y         = Exp (Tag lvl)
      f a b     = stencil (stencilPrj (undefined::sh) (undefined::a) a)
                          (stencilPrj (undefined::sh) (undefined::b) b)
  --
  (body, height) <- makeOccMapRootExp config accOccMap (lvl+2) [lvl, lvl+1] (f x y)
  return (\_ _ -> body, height)


-- Generate sharing information for expressions embedded in Acc computations.
-- Expressions are annotated with:
--
--  1) the tags of free scalar variables (for scalar functions)
--  2) a local occurrence map for that expression.
--
makeOccMapRootExp
    :: Typeable e
    => Config
    -> OccMapHash Acc
    -> Level                            -- The level of currently bound scalar variables
    -> [Int]                            -- The tags of newly introduced free scalar variables in this expression
    -> Exp e
    -> IO (RootExp e, Int)
makeOccMapRootExp config accOccMap lvl fvs exp = do
  traceLine "makeOccMapRootExp" "Enter"
  expOccMap                     <- newASTHashTable
  (UnscopedExp [] exp', height) <- makeOccMapSharingExp config accOccMap expOccMap lvl exp
  frozenExpOccMap               <- freezeOccMap expOccMap
  traceLine "makeOccMapRootExp" "Exit"
  return (RootExp frozenExpOccMap (UnscopedExp fvs exp'), height)


-- Generate sharing information for an open scalar expression.
--
makeOccMapSharingExp
    :: Typeable e
    => Config
    -> OccMapHash Acc
    -> OccMapHash Exp
    -> Level                            -- The level of currently bound variables
    -> Exp e
    -> IO (UnscopedExp e, Int)
makeOccMapSharingExp config accOccMap expOccMap = travE
  where
    travE :: forall a. Typeable a => Level -> Exp a -> IO (UnscopedExp a, Int)
    travE lvl exp@(Exp pexp)
      = mfix $ \ ~(_, height) -> do
          -- Compute stable name and enter it into the occurrence map
          --
          sn                         <- makeStableAST exp
          heightIfRepeatedOccurrence <- enterOcc expOccMap (StableASTName sn) height

          traceLine (showPreExpOp pexp) $ do
            let hash = show (hashStableName sn)
            case heightIfRepeatedOccurrence of
              Just height -> "REPEATED occurrence (sn = " ++ hash ++ "; height = " ++ show height ++ ")"
              Nothing     -> "first occurrence (sn = " ++ hash ++ ")"

          -- Reconstruct the computation in shared form.
          --
          -- In case of a repeated occurrence, the height comes from the occurrence map; otherwise
          -- it is computed by the traversal function passed in 'newExp'.  See also 'enterOcc'.
          --
          -- NB: This function can only be used in the case alternatives below; outside of the
          --     case we cannot discharge the 'Elt a' constraint.
          --
          let reconstruct :: Elt a
                          => IO (PreExp UnscopedAcc UnscopedExp a, Int)
                          -> IO (UnscopedExp a, Int)
              reconstruct newExp
                = case heightIfRepeatedOccurrence of
                    Just height | recoverExpSharing config
                      -> return (UnscopedExp [] (VarSharing (StableNameHeight sn height)), height)
                    _ -> do (exp, height) <- newExp
                            return (UnscopedExp [] (ExpSharing (StableNameHeight sn height) exp), height)

          case pexp of
            Tag i               -> reconstruct $ return (Tag i, 0)      -- height is 0!
            Const c             -> reconstruct $ return (Const c, 1)
            Tuple tup           -> reconstruct $ do
                                     (tup', h) <- travTup tup
                                     return (Tuple tup', h)
            Prj i e             -> reconstruct $ travE1 (Prj i) e
            IndexNil            -> reconstruct $ return (IndexNil, 1)
            IndexCons ix i      -> reconstruct $ travE2 IndexCons ix i
            IndexHead i         -> reconstruct $ travE1 IndexHead i
            IndexTail ix        -> reconstruct $ travE1 IndexTail ix
            IndexAny            -> reconstruct $ return (IndexAny, 1)
            ToIndex sh ix       -> reconstruct $ travE2 ToIndex sh ix
            FromIndex sh e      -> reconstruct $ travE2 FromIndex sh e
            Cond e1 e2 e3       -> reconstruct $ travE3 Cond e1 e2 e3
            While p iter init   -> reconstruct $ do
                                     (p'   , h1) <- traverseFun1 lvl p
                                     (iter', h2) <- traverseFun1 lvl iter
                                     (init', h3) <- travE lvl init
                                     return (While p' iter' init', h1 `max` h2 `max` h3 + 1)
            PrimConst c         -> reconstruct $ return (PrimConst c, 1)
            PrimApp p e         -> reconstruct $ travE1 (PrimApp p) e
            Index a e           -> reconstruct $ travAE Index a e
            LinearIndex a i     -> reconstruct $ travAE LinearIndex a i
            Shape a             -> reconstruct $ travA Shape a
            ShapeSize e         -> reconstruct $ travE1 ShapeSize e
            Intersect sh1 sh2   -> reconstruct $ travE2 Intersect sh1 sh2
            Foreign ff f e      -> reconstruct $ do
                                      (e', h) <- travE lvl e
                                      return  (Foreign ff f e', h+1)

      where
        traverseAcc :: Typeable arrs => Level -> Acc arrs -> IO (UnscopedAcc arrs, Int)
        traverseAcc = makeOccMapSharingAcc config accOccMap

        traverseFun1 :: (Elt a, Typeable b)
                     => Level
                     -> (Exp a -> Exp b)
                     -> IO (Exp a -> UnscopedExp b, Int)
        traverseFun1 lvl f
          = do
              let x = Exp (Tag lvl)
              (UnscopedExp [] body, height) <- travE (lvl+1) (f x)
              return (const (UnscopedExp [lvl] body), height + 1)


        travE1 :: Typeable b => (UnscopedExp b -> PreExp UnscopedAcc UnscopedExp a) -> Exp b
               -> IO (PreExp UnscopedAcc UnscopedExp a, Int)
        travE1 c e
          = do
              (e', h) <- travE lvl e
              return (c e', h + 1)

        travE2 :: (Typeable b, Typeable c)
               => (UnscopedExp b -> UnscopedExp c -> PreExp UnscopedAcc UnscopedExp a)
               -> Exp b -> Exp c
               -> IO (PreExp UnscopedAcc UnscopedExp a, Int)
        travE2 c e1 e2
          = do
              (e1', h1) <- travE lvl e1
              (e2', h2) <- travE lvl e2
              return (c e1' e2', h1 `max` h2 + 1)

        travE3 :: (Typeable b, Typeable c, Typeable d)
               => (UnscopedExp b -> UnscopedExp c -> UnscopedExp d -> PreExp UnscopedAcc UnscopedExp a)
               -> Exp b -> Exp c -> Exp d
               -> IO (PreExp UnscopedAcc UnscopedExp a, Int)
        travE3 c e1 e2 e3
          = do
              (e1', h1) <- travE lvl e1
              (e2', h2) <- travE lvl e2
              (e3', h3) <- travE lvl e3
              return (c e1' e2' e3', h1 `max` h2 `max` h3 + 1)

        travA :: Typeable b => (UnscopedAcc b -> PreExp UnscopedAcc UnscopedExp a) -> Acc b
              -> IO (PreExp UnscopedAcc UnscopedExp a, Int)
        travA c acc
          = do
              (acc', h) <- traverseAcc lvl acc
              return (c acc', h + 1)

        travAE :: (Typeable b, Typeable c)
               => (UnscopedAcc b -> UnscopedExp c -> PreExp UnscopedAcc UnscopedExp a)
               -> Acc b -> Exp c
               -> IO (PreExp UnscopedAcc UnscopedExp a, Int)
        travAE c acc e
          = do
              (acc', h1) <- traverseAcc lvl acc
              (e'  , h2) <- travE lvl e
              return (c acc' e', h1 `max` h2 + 1)

        travTup :: Tuple.Tuple Exp tup -> IO (Tuple.Tuple UnscopedExp tup, Int)
        travTup NilTup          = return (NilTup, 1)
        travTup (SnocTup tup e) = do
                                    (tup', h1) <- travTup tup
                                    (e'  , h2) <- travE lvl e
                                    return (SnocTup tup' e', h1 `max` h2 + 1)


-- Type used to maintain how often each shared subterm, so far, occurred during a bottom-up sweep,
-- as well as the relation between subterms. It is comprised of a list of terms and a graph giving
-- their relation.
--
--   Invariants of the list:
--   - If one shared term 's' is itself a subterm of another shared term 't', then 's' must occur
--     *after* 't' in the list.
--   - No shared term occurs twice.
--   - A term may have a final occurrence count of only 1 iff it is either a free variable ('Atag'
--     or 'Tag') or an array computation lifted out of an expression.
--   - All 'Exp' node counts precede all 'Acc' node counts as we don't share 'Exp' nodes across 'Acc'
--     nodes.
--
-- We determine the subterm property by using the tree height in 'StableNameHeight'.  Trees get
-- smaller towards the end of a 'NodeCounts' list.  The height of free variables ('Atag' or 'Tag')
-- is 0, whereas other leaves have height 1.  This guarantees that all free variables are at the end
-- of the 'NodeCounts' list.
--
-- The graph is represented as a map where a stable name 'a' is mapped to a set of stables names 'b'
-- such that if there exists a edge from 'a' to 'c' that 'c' is contained within 'b'.
--
--  Properties of the graph:
--  - There exists an edge from 'a' to 'b' if the term 'a' names is a subterm of the term named by
--    'b'.
--
-- To ensure the list invariant and the graph properties are preserved over merging node counts from
-- sibling subterms, the function '(+++)' must be used.
--
type NodeCounts = ([NodeCount], Map.HashMap NodeName (Set.HashSet NodeName))

data NodeName where
  NodeName :: Typeable a => StableName a -> NodeName

instance Eq NodeName where
  (NodeName sn1) == (NodeName sn2) | Just sn2' <- gcast sn2 = sn1 == sn2'
                                   | otherwise              = False

instance Hashable NodeName where
  hashWithSalt hash (NodeName sn1) = hash + hashStableName sn1

instance Show NodeName where
  show (NodeName sn) = show (hashStableName sn)

data NodeCount = AccNodeCount StableSharingAcc Int
               | ExpNodeCount StableSharingExp Int
               deriving Show

-- Empty node counts
--
noNodeCounts :: NodeCounts
noNodeCounts = ([], Map.empty)

-- Insert an Acc node into the node counts, assuming that it is a superterm of the all the existing
-- nodes.
--
-- TODO: Perform cycle detection here.
insertAccNode :: StableSharingAcc -> NodeCounts -> NodeCounts
insertAccNode ssa@(StableSharingAcc (StableNameHeight sn _) _) (subterms,g)
  = ([AccNodeCount ssa 1], g') +++ (subterms,g)
  where
    k  = NodeName sn
    hs = map nodeName subterms
    g' = Map.fromList $ (k, Set.empty) : [(h, Set.singleton k) | h <- hs]

-- Insert an Exp node into the node counts, assuming that it is a superterm of the all the existing
-- nodes.
--
-- TODO: Perform cycle detection here.
insertExpNode :: StableSharingExp -> NodeCounts -> NodeCounts
insertExpNode ssa@(StableSharingExp (StableNameHeight sn _) _) (subterms,g)
  = ([ExpNodeCount ssa 1], g') +++ (subterms,g)
  where
    k  = NodeName sn
    hs = map nodeName subterms
    g' = Map.fromList $ (k, Set.empty) : [(h, Set.singleton k) | h <- hs]

-- Remove nodes that aren't in the list from the graph.
--
-- RCE: This is no longer necessary when NDP is supported.
cleanCounts :: NodeCounts -> NodeCounts
cleanCounts (ns, g) = (ns, Map.fromList $ [(h, Set.filter (flip elem hs) (g Map.! h)) | h <- hs ])
  where
    hs = (map nodeName ns)

nodeName :: NodeCount -> NodeName
nodeName (AccNodeCount (StableSharingAcc (StableNameHeight sn _) _) _) = NodeName sn
nodeName (ExpNodeCount (StableSharingExp (StableNameHeight sn _) _) _) = NodeName sn

-- Combine node counts that belong to the same node.
--
-- * We assume that the list invariant —subterms follow their parents— holds for both arguments and
--   guarantee that it still holds for the result.
-- * In the same manner, we assume that all 'Exp' node counts precede 'Acc' node counts and
--   guarantee that this also hold for the result.
--
-- RCE: The list combination should be able to be performed as a more efficient merge.
--
(+++) :: NodeCounts -> NodeCounts -> NodeCounts
(ns1,g1) +++ (ns2,g2) = (foldr insert ns1 ns2, Map.unionWith Set.union g1 g2)
  where
    insert x               []                         = [x]
    insert x@(AccNodeCount sa1 count1) ys@(y@(AccNodeCount sa2 count2) : ys')
      | sa1 == sa2          = AccNodeCount (sa1 `pickNoneAvar` sa2) (count1 + count2) : ys'
      | sa1 `higherSSA` sa2 = x : ys
      | otherwise           = y : insert x ys'
    insert x@(ExpNodeCount se1 count1) ys@(y@(ExpNodeCount se2 count2) : ys')
      | se1 == se2          = ExpNodeCount (se1 `pickNoneVar` se2) (count1 + count2) : ys'
      | se1 `higherSSE` se2 = x : ys
      | otherwise           = y : insert x ys'
    insert x@(AccNodeCount _ _) (y@(ExpNodeCount _ _) : ys')
      = y : insert x ys'
    insert x@(ExpNodeCount _ _) (y@(AccNodeCount _ _) : ys')
      = x : insert y ys'

    (StableSharingAcc _ (AvarSharing _)) `pickNoneAvar` sa2  = sa2
    sa1                                  `pickNoneAvar` _sa2 = sa1

    (StableSharingExp _ (VarSharing _))  `pickNoneVar`  sa2  = sa2
    sa1                                  `pickNoneVar`  _sa2 = sa1

-- Build an initial environment for the tag values given in the first argument for traversing an
-- array expression.  The 'StableSharingAcc's for all tags /actually used/ in the expressions are
-- in the second argument. (Tags are not used if a bound variable has no usage occurrence.)
--
-- Bail out if any tag occurs multiple times as this indicates that the sharing of an argument
-- variable was not preserved and we cannot build an appropriate initial environment (c.f., comments
-- at 'determineScopesAcc'.
--
buildInitialEnvAcc :: [Level] -> [StableSharingAcc] -> [StableSharingAcc]
buildInitialEnvAcc tags sas = map (lookupSA sas) tags
  where
    lookupSA sas tag1
      = case filter hasTag sas of
          []   -> noStableSharing    -- tag is not used in the analysed expression
          [sa] -> sa                 -- tag has a unique occurrence
          sas2 -> INTERNAL_ERROR(error) "buildInitialEnvAcc"
                $ "Encountered duplicate 'ATag's\n  " ++ intercalate ", " (map showSA sas2)
      where
        hasTag (StableSharingAcc _ (AccSharing _ (Atag tag2))) = tag1 == tag2
        hasTag sa
          = INTERNAL_ERROR(error) "buildInitialEnvAcc"
          $ "Encountered a node that is not a plain 'Atag'\n  " ++ showSA sa

        noStableSharing :: StableSharingAcc
        noStableSharing = StableSharingAcc noStableAccName (undefined :: SharingAcc acc exp ())

    showSA (StableSharingAcc _ (AccSharing  sn acc)) = show (hashStableNameHeight sn) ++ ": " ++
                                                       showPreAccOp acc
    showSA (StableSharingAcc _ (AvarSharing sn))     = "AvarSharing " ++ show (hashStableNameHeight sn)
    showSA (StableSharingAcc _ (AletSharing sa _ ))  = "AletSharing " ++ show sa ++ "..."

-- Build an initial environment for the tag values given in the first argument for traversing a
-- scalar expression.  The 'StableSharingExp's for all tags /actually used/ in the expressions are
-- in the second argument. (Tags are not used if a bound variable has no usage occurrence.)
--
-- Bail out if any tag occurs multiple times as this indicates that the sharing of an argument
-- variable was not preserved and we cannot build an appropriate initial environment (c.f., comments
-- at 'determineScopesAcc'.
--
buildInitialEnvExp :: [Level] -> [StableSharingExp] -> [StableSharingExp]
buildInitialEnvExp tags ses = map (lookupSE ses) tags
  where
    lookupSE ses tag1
      = case filter hasTag ses of
          []   -> noStableSharing    -- tag is not used in the analysed expression
          [se] -> se                 -- tag has a unique occurrence
          ses2 -> INTERNAL_ERROR(error) "buildInitialEnvExp"
                    ("Encountered a duplicate 'Tag'\n  " ++ intercalate ", " (map showSE ses2))
      where
        hasTag (StableSharingExp _ (ExpSharing _ (Tag tag2))) = tag1 == tag2
        hasTag se
          = INTERNAL_ERROR(error) "buildInitialEnvExp"
              ("Encountered a node that is not a plain 'Tag'\n  " ++ showSE se)

        noStableSharing :: StableSharingExp
        noStableSharing = StableSharingExp noStableExpName (undefined :: SharingExp acc exp ())

    showSE (StableSharingExp _ (ExpSharing sn exp)) = show (hashStableNameHeight sn) ++ ": " ++
                                                      showPreExpOp exp
    showSE (StableSharingExp _ (VarSharing sn))     = "VarSharing " ++ show (hashStableNameHeight sn)
    showSE (StableSharingExp _ (LetSharing se _ ))  = "LetSharing " ++ show se ++ "..."

-- Determine whether a 'NodeCount' is for an 'Atag' or 'Tag', which represent free variables.
--
isFreeVar :: NodeCount -> Bool
isFreeVar (AccNodeCount (StableSharingAcc _ (AccSharing _ (Atag _))) _) = True
isFreeVar (ExpNodeCount (StableSharingExp _ (ExpSharing _ (Tag  _))) _) = True
isFreeVar _                                                             = False


-- Determine scope of shared subterms
-- ==================================

-- Determine the scopes of all variables representing shared subterms (Phase Two) in a bottom-up
-- sweep.  The first argument determines whether array computations are floated out of expressions
-- irrespective of whether they are shared or not — 'True' implies floating them out.
--
-- In addition to the AST with sharing information, yield the 'StableSharingAcc's for all free
-- variables of 'rootAcc', which are represented by 'Atag' leaves in the tree. They are in order of
-- the tag values — i.e., in the same order that they need to appear in an environment to use the
-- tag for indexing into that environment.
--
-- Precondition: there are only 'AvarSharing' and 'AccSharing' nodes in the argument.
--
determineScopesAcc
    :: Typeable a
    => Config
    -> [Level]
    -> OccMap Acc
    -> UnscopedAcc a
    -> (ScopedAcc a, [StableSharingAcc])
determineScopesAcc config fvs accOccMap rootAcc
  = let (sharingAcc, (counts, _)) = determineScopesSharingAcc config accOccMap rootAcc
        unboundTrees         = filter (not . isFreeVar) counts
    in
    if all isFreeVar counts
       then (sharingAcc, buildInitialEnvAcc fvs [sa | AccNodeCount sa _ <- counts])
       else INTERNAL_ERROR(error) "determineScopesAcc" ("unbound shared subtrees" ++ show unboundTrees)


determineScopesSharingAcc
    :: Config
    -> OccMap Acc
    -> UnscopedAcc a
    -> (ScopedAcc a, NodeCounts)
determineScopesSharingAcc config accOccMap = scopesAcc
  where
    scopesAcc :: forall arrs. UnscopedAcc arrs -> (ScopedAcc arrs, NodeCounts)
    scopesAcc (UnscopedAcc _ (AletSharing _ _))
      = INTERNAL_ERROR(error) "determineScopesSharingAcc: scopesAcc" "unexpected 'AletSharing'"

    scopesAcc (UnscopedAcc _ (AvarSharing sn))
      = (ScopedAcc [] (AvarSharing sn), StableSharingAcc sn (AvarSharing sn) `insertAccNode` noNodeCounts)

    scopesAcc (UnscopedAcc _ (AccSharing sn pacc))
      = case pacc of
          Atag i                  -> reconstruct (Atag i) noNodeCounts
          Pipe afun1 afun2 acc    -> let
                                       (afun1', accCount1) = scopesAfun1 afun1
                                       (afun2', accCount2) = scopesAfun1 afun2
                                       (acc', accCount3)   = scopesAcc acc
                                     in
                                     reconstruct (Pipe afun1' afun2' acc')
                                                 (accCount1 +++ accCount2 +++ accCount3)

          Aforeign ff afun acc    -> let
                                       (acc', accCount) = scopesAcc acc
                                     in
                                     reconstruct (Aforeign ff afun acc') accCount
          Acond e acc1 acc2       -> let
                                       (e'   , accCount1) = scopesExp e
                                       (acc1', accCount2) = scopesAcc acc1
                                       (acc2', accCount3) = scopesAcc acc2
                                     in
                                     reconstruct (Acond e' acc1' acc2')
                                                 (accCount1 +++ accCount2 +++ accCount3)

          Awhile pred iter init   -> let
                                       (pred', accCount1) = scopesAfun1 pred
                                       (iter', accCount2) = scopesAfun1 iter
                                       (init', accCount3) = scopesAcc init
                                     in
                                     reconstruct (Awhile pred' iter' init')
                                                 (accCount1 +++ accCount2 +++ accCount3)

          Atuple tup              -> let (tup', accCount) = travAtup tup
                                     in  reconstruct (Atuple tup') accCount
          Aprj ix a               -> travA (Aprj ix) a

          Use arr                 -> reconstruct (Use arr) noNodeCounts
          Unit e                  -> let
                                       (e', accCount) = scopesExp e
                                     in
                                     reconstruct (Unit e') accCount
          Generate sh f           -> let
                                       (sh', accCount1) = scopesExp sh
                                       (f' , accCount2) = scopesFun1 f
                                     in
                                     reconstruct (Generate sh' f') (accCount1 +++ accCount2)
          Reshape sh acc          -> travEA Reshape sh acc
          Replicate n acc         -> travEA Replicate n acc
          Slice acc i             -> travEA (flip Slice) i acc
          Map f acc               -> let
                                       (f'  , accCount1) = scopesFun1 f
                                       (acc', accCount2) = scopesAcc  acc
                                     in
                                     reconstruct (Map f' acc') (accCount1 +++ accCount2)
          ZipWith f acc1 acc2     -> travF2A2 ZipWith f acc1 acc2
          Fold f z acc            -> travF2EA Fold f z acc
          Fold1 f acc             -> travF2A Fold1 f acc
          FoldSeg f z acc1 acc2   -> let
                                       (f'   , accCount1)  = scopesFun2 f
                                       (z'   , accCount2)  = scopesExp  z
                                       (acc1', accCount3)  = scopesAcc  acc1
                                       (acc2', accCount4)  = scopesAcc  acc2
                                     in
                                     reconstruct (FoldSeg f' z' acc1' acc2')
                                       (accCount1 +++ accCount2 +++ accCount3 +++ accCount4)
          Fold1Seg f acc1 acc2    -> travF2A2 Fold1Seg f acc1 acc2
          Scanl f z acc           -> travF2EA Scanl f z acc
          Scanl' f z acc          -> travF2EA Scanl' f z acc
          Scanl1 f acc            -> travF2A Scanl1 f acc
          Scanr f z acc           -> travF2EA Scanr f z acc
          Scanr' f z acc          -> travF2EA Scanr' f z acc
          Scanr1 f acc            -> travF2A Scanr1 f acc
          Permute fc acc1 fp acc2 -> let
                                       (fc'  , accCount1) = scopesFun2 fc
                                       (acc1', accCount2) = scopesAcc  acc1
                                       (fp'  , accCount3) = scopesFun1 fp
                                       (acc2', accCount4) = scopesAcc  acc2
                                     in
                                     reconstruct (Permute fc' acc1' fp' acc2')
                                       (accCount1 +++ accCount2 +++ accCount3 +++ accCount4)
          Backpermute sh fp acc   -> let
                                       (sh' , accCount1) = scopesExp  sh
                                       (fp' , accCount2) = scopesFun1 fp
                                       (acc', accCount3) = scopesAcc  acc
                                     in
                                     reconstruct (Backpermute sh' fp' acc')
                                       (accCount1 +++ accCount2 +++ accCount3)
          Stencil st bnd acc      -> let
                                       (st' , accCount1) = scopesStencil1 acc st
                                       (acc', accCount2) = scopesAcc      acc
                                     in
                                     reconstruct (Stencil st' bnd acc') (accCount1 +++ accCount2)
          Stencil2 st bnd1 acc1 bnd2 acc2
                                  -> let
                                       (st'  , accCount1) = scopesStencil2 acc1 acc2 st
                                       (acc1', accCount2) = scopesAcc acc1
                                       (acc2', accCount3) = scopesAcc acc2
                                     in
                                     reconstruct (Stencil2 st' bnd1 acc1' bnd2 acc2')
                                       (accCount1 +++ accCount2 +++ accCount3)
      where
        travEA :: Arrays arrs
               => (ScopedExp e -> ScopedAcc arrs' -> PreAcc ScopedAcc ScopedExp arrs)
               -> RootExp e
               -> UnscopedAcc arrs'
               -> (ScopedAcc arrs, NodeCounts)
        travEA c e acc = reconstruct (c e' acc') (accCount1 +++ accCount2)
          where
            (e'  , accCount1) = scopesExp e
            (acc', accCount2) = scopesAcc acc

        travF2A :: (Elt a, Elt b, Arrays arrs)
                => ((Exp a -> Exp b -> ScopedExp c) -> ScopedAcc arrs'
                    -> PreAcc ScopedAcc ScopedExp arrs)
                -> (Exp a -> Exp b -> RootExp c)
                -> UnscopedAcc arrs'
                -> (ScopedAcc arrs, NodeCounts)
        travF2A c f acc = reconstruct (c f' acc') (accCount1 +++ accCount2)
          where
            (f'  , accCount1) = scopesFun2 f
            (acc', accCount2) = scopesAcc  acc

        travF2EA :: (Elt a, Elt b, Arrays arrs)
                 => ((Exp a -> Exp b -> ScopedExp c) -> ScopedExp e
                     -> ScopedAcc arrs' -> PreAcc ScopedAcc ScopedExp arrs)
                 -> (Exp a -> Exp b -> RootExp c)
                 -> RootExp e
                 -> UnscopedAcc arrs'
                 -> (ScopedAcc arrs, NodeCounts)
        travF2EA c f e acc = reconstruct (c f' e' acc') (accCount1 +++ accCount2 +++ accCount3)
          where
            (f'  , accCount1) = scopesFun2 f
            (e'  , accCount2) = scopesExp  e
            (acc', accCount3) = scopesAcc  acc

        travF2A2 :: (Elt a, Elt b, Arrays arrs)
                 => ((Exp a -> Exp b -> ScopedExp c) -> ScopedAcc arrs1
                     -> ScopedAcc arrs2 -> PreAcc ScopedAcc ScopedExp arrs)
                 -> (Exp a -> Exp b -> RootExp c)
                 -> UnscopedAcc arrs1
                 -> UnscopedAcc arrs2
                 -> (ScopedAcc arrs, NodeCounts)
        travF2A2 c f acc1 acc2 = reconstruct (c f' acc1' acc2')
                                             (accCount1 +++ accCount2 +++ accCount3)
          where
            (f'   , accCount1) = scopesFun2 f
            (acc1', accCount2) = scopesAcc  acc1
            (acc2', accCount3) = scopesAcc  acc2

        travAtup ::  Tuple.Atuple UnscopedAcc a
                 -> (Tuple.Atuple ScopedAcc a, NodeCounts)
        travAtup NilAtup          = (NilAtup, noNodeCounts)
        travAtup (SnocAtup tup a) = let (tup', accCountT) = travAtup tup
                                        (a',   accCountA) = scopesAcc a
                                    in
                                    (SnocAtup tup' a', accCountT +++ accCountA)

        travA :: Arrays arrs
              => (ScopedAcc arrs' -> PreAcc ScopedAcc ScopedExp arrs)
              -> UnscopedAcc arrs'
              -> (ScopedAcc arrs, NodeCounts)
        travA c acc = reconstruct (c acc') accCount
          where
            (acc', accCount) = scopesAcc acc

          -- Occurrence count of the currently processed node
        accOccCount = let StableNameHeight sn' _ = sn
                      in
                      lookupWithASTName accOccMap (StableASTName sn')

        -- Reconstruct the current tree node.
        --
        -- * If the current node is being shared ('accOccCount > 1'), replace it by a 'AvarSharing'
        --   node and float the shared subtree out wrapped in a 'NodeCounts' value.
        -- * If the current node is not shared, reconstruct it in place.
        -- * Special case for free variables ('Atag'): Replace the tree by a sharing variable and
        --   float the 'Atag' out in a 'NodeCounts' value.  This is independent of the number of
        --   occurrences.
        --
        -- In either case, any completed 'NodeCounts' are injected as bindings using 'AletSharing'
        -- node.
        --
        reconstruct :: Arrays arrs
                    => PreAcc ScopedAcc ScopedExp arrs -> NodeCounts
                    -> (ScopedAcc arrs, NodeCounts)
        reconstruct newAcc@(Atag _) _subCount
              -- free variable => replace by a sharing variable regardless of the number of
              -- occurrences
          = let thisCount = StableSharingAcc sn (AccSharing sn newAcc) `insertAccNode` noNodeCounts
            in
            tracePure "FREE" (show thisCount)
            (ScopedAcc [] (AvarSharing sn), thisCount)
        reconstruct newAcc subCount
              -- shared subtree => replace by a sharing variable (if 'recoverAccSharing' enabled)
          | accOccCount > 1 && recoverAccSharing config
          = let allCount = (StableSharingAcc sn sharingAcc `insertAccNode` newCount)
            in
            tracePure ("SHARED" ++ completed) (show allCount)
            (ScopedAcc [] (AvarSharing sn), allCount)
              -- neither shared nor free variable => leave it as it is
          | otherwise
          = tracePure ("Normal" ++ completed) (show newCount)
            (ScopedAcc [] sharingAcc, newCount)
          where
              -- Determine the bindings that need to be attached to the current node...
            (newCount, bindHere) = filterCompleted subCount

              -- ...and wrap them in 'AletSharing' constructors
            lets       = foldl (flip (.)) id . map (\x y -> AletSharing x (ScopedAcc [] y)) $ bindHere
            sharingAcc = lets $ AccSharing sn newAcc

              -- trace support
            completed | null bindHere = ""
                      | otherwise     = "(" ++ show (length bindHere) ++ " lets)"

        -- Extract *leading* nodes that have a complete node count (i.e., their node count is equal
        -- to the number of occurrences of that node in the overall expression).
        --
        -- Nodes with a completed node count should be let bound at the currently processed node.
        --
        -- NB: Only extract leading nodes (i.e., the longest run at the *front* of the list that is
        --     complete).  Otherwise, we would let-bind subterms before their parents, which leads
        --     scope errors.
        --
        filterCompleted :: NodeCounts -> (NodeCounts, [StableSharingAcc])
        filterCompleted (ns, graph)
          = let bindable     = map (isBindable bindable (map nodeName ns)) ns
                (bind, rest) = partition fst $ zip bindable ns
            in ((map snd rest, graph), [sa | AccNodeCount sa _ <- map snd bind])
          where
            -- a node is not yet complete while the node count 'n' is below the overall number
            -- of occurrences for that node in the whole program, with the exception that free
            -- variables are never complete
            isCompleted nc@(AccNodeCount sa n) | not . isFreeVar $ nc = lookupWithSharingAcc accOccMap sa == n
            isCompleted _                                             = False

            isBindable :: [Bool] -> [NodeName] -> NodeCount -> Bool
            isBindable bindable nodes nc@(AccNodeCount _ _) =
              let superTerms = Set.toList $ graph Map.! nodeName nc
                  unbound    = mapMaybe (`elemIndex` nodes) superTerms
              in    isCompleted nc
                 && all (bindable !!) unbound
            isBindable _ _ (ExpNodeCount _ _) = False

    scopesExp :: RootExp t -> (ScopedExp t, NodeCounts)
    scopesExp = determineScopesExp config accOccMap

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesAfun1 :: Arrays a1 => (Acc a1 -> UnscopedAcc a2) -> (Acc a1 -> ScopedAcc a2, NodeCounts)
    scopesAfun1 f = (const (ScopedAcc ssa body'), (counts',graph))
      where
        body@(UnscopedAcc fvs _) = f undefined
        ((ScopedAcc [] body'), (counts,graph)) = scopesAcc body
        ssa     = buildInitialEnvAcc fvs [sa | AccNodeCount sa _ <- freeCounts]
        (freeCounts, counts') = partition isBoundHere counts

        isBoundHere (AccNodeCount (StableSharingAcc _ (AccSharing _ (Atag i))) _) = i `elem` fvs
        isBoundHere _                                                             = False

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesFun1 :: Elt e1 => (Exp e1 -> RootExp e2) -> (Exp e1 -> ScopedExp e2, NodeCounts)
    scopesFun1 f = (const body, counts)
      where
        (body, counts) = scopesExp (f undefined)

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesFun2 :: (Elt e1, Elt e2)
               => (Exp e1 -> Exp e2 -> RootExp e3)
               -> (Exp e1 -> Exp e2 -> ScopedExp e3, NodeCounts)
    scopesFun2 f = (\_ _ -> body, counts)
      where
        (body, counts) = scopesExp (f undefined undefined)

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesStencil1 :: forall sh e1 e2 stencil. Stencil sh e1 stencil
                   => UnscopedAcc (Array sh e1){-dummy-}
                   -> (stencil -> RootExp e2)
                   -> (stencil -> ScopedExp e2, NodeCounts)
    scopesStencil1 _ stencilFun = (const body, counts)
      where
        (body, counts) = scopesExp (stencilFun undefined)

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesStencil2 :: forall sh e1 e2 e3 stencil1 stencil2.
                      (Stencil sh e1 stencil1, Stencil sh e2 stencil2)
                   => UnscopedAcc (Array sh e1){-dummy-}
                   -> UnscopedAcc (Array sh e2){-dummy-}
                   -> (stencil1 -> stencil2 -> RootExp e3)
                   -> (stencil1 -> stencil2 -> ScopedExp e3, NodeCounts)
    scopesStencil2 _ _ stencilFun = (\_ _ -> body, counts)
      where
        (body, counts) = scopesExp (stencilFun undefined undefined)


determineScopesExp
    :: Config
    -> OccMap Acc
    -> RootExp t
    -> (ScopedExp t, NodeCounts)          -- Root (closed) expression plus Acc node counts
determineScopesExp config accOccMap (RootExp expOccMap exp@(UnscopedExp fvs _))
  = let
        ((ScopedExp [] expWithScopes), (nodeCounts,graph)) = determineScopesSharingExp config accOccMap expOccMap exp
        (expCounts, accCounts)          = break isAccNodeCount nodeCounts

        isAccNodeCount AccNodeCount{}   = True
        isAccNodeCount _                = False
    in
    (ScopedExp (buildInitialEnvExp fvs [se | ExpNodeCount se _ <- expCounts]) expWithScopes, cleanCounts (accCounts,graph))


determineScopesSharingExp
    :: Config
    -> OccMap Acc
    -> OccMap Exp
    -> UnscopedExp t
    -> (ScopedExp t, NodeCounts)
determineScopesSharingExp config accOccMap expOccMap = scopesExp
  where
    scopesAcc :: UnscopedAcc a -> (ScopedAcc a, NodeCounts)
    scopesAcc = determineScopesSharingAcc config accOccMap

    scopesFun1 :: (Exp a -> UnscopedExp b) -> (Exp a -> ScopedExp b, NodeCounts)
    scopesFun1 f = tracePure ("LAMBDA " ++ (show ssa)) (show counts) (const (ScopedExp ssa body'), (counts',graph))
      where
        body@(UnscopedExp fvs _) = f undefined
        ((ScopedExp [] body'), (counts, graph)) = scopesExp body
        ssa     = buildInitialEnvExp fvs [se | ExpNodeCount se _ <- freeCounts]
        (freeCounts, counts') = partition isBoundHere counts

        isBoundHere (ExpNodeCount (StableSharingExp _ (ExpSharing _ (Tag i))) _) = i `elem` fvs
        isBoundHere _                                                            = False


    scopesExp :: forall t. UnscopedExp t -> (ScopedExp t, NodeCounts)
    scopesExp (UnscopedExp _ (LetSharing _ _))
      = INTERNAL_ERROR(error) "determineScopesSharingExp: scopesExp" "unexpected 'LetSharing'"

    scopesExp (UnscopedExp _ (VarSharing sn))
      = (ScopedExp [] (VarSharing sn), StableSharingExp sn (VarSharing sn) `insertExpNode` noNodeCounts)

    scopesExp (UnscopedExp _ (ExpSharing sn pexp))
      = case pexp of
          Tag i                 -> reconstruct (Tag i) noNodeCounts
          Const c               -> reconstruct (Const c) noNodeCounts
          Tuple tup             -> let (tup', accCount) = travTup tup
                                   in
                                   reconstruct (Tuple tup') accCount
          Prj i e               -> travE1 (Prj i) e
          IndexNil              -> reconstruct IndexNil noNodeCounts
          IndexCons ix i        -> travE2 IndexCons ix i
          IndexHead i           -> travE1 IndexHead i
          IndexTail ix          -> travE1 IndexTail ix
          IndexAny              -> reconstruct IndexAny noNodeCounts
          ToIndex sh ix         -> travE2 ToIndex sh ix
          FromIndex sh e        -> travE2 FromIndex sh e
          Cond e1 e2 e3         -> travE3 Cond e1 e2 e3
          While p it i          -> let
                                     (p' , accCount1) = scopesFun1 p
                                     (it', accCount2) = scopesFun1 it
                                     (i' , accCount3) = scopesExp i
                                   in reconstruct (While p' it' i') (accCount1 +++ accCount2 +++ accCount3)
          PrimConst c           -> reconstruct (PrimConst c) noNodeCounts
          PrimApp p e           -> travE1 (PrimApp p) e
          Index a e             -> travAE Index a e
          LinearIndex a e       -> travAE LinearIndex a e
          Shape a               -> travA Shape a
          ShapeSize e           -> travE1 ShapeSize e
          Intersect sh1 sh2     -> travE2 Intersect sh1 sh2
          Foreign ff f e        -> travE1 (Foreign ff f) e
      where
        travTup :: Tuple.Tuple UnscopedExp tup -> (Tuple.Tuple ScopedExp tup, NodeCounts)
        travTup NilTup          = (NilTup, noNodeCounts)
        travTup (SnocTup tup e) = let
                                    (tup', accCountT) = travTup tup
                                    (e'  , accCountE) = scopesExp e
                                  in
                                  (SnocTup tup' e', accCountT +++ accCountE)

        travE1 :: (ScopedExp a -> PreExp ScopedAcc ScopedExp t) -> UnscopedExp a
               -> (ScopedExp t, NodeCounts)
        travE1 c e = reconstruct (c e') accCount
          where
            (e', accCount) = scopesExp e

        travE2 :: (ScopedExp a -> ScopedExp b -> PreExp ScopedAcc ScopedExp t)
               -> UnscopedExp a
               -> UnscopedExp b
               -> (ScopedExp t, NodeCounts)
        travE2 c e1 e2 = reconstruct (c e1' e2') (accCount1 +++ accCount2)
          where
            (e1', accCount1) = scopesExp e1
            (e2', accCount2) = scopesExp e2

        travE3 :: (ScopedExp a -> ScopedExp b -> ScopedExp c -> PreExp ScopedAcc ScopedExp t)
               -> UnscopedExp a
               -> UnscopedExp b
               -> UnscopedExp c
               -> (ScopedExp t, NodeCounts)
        travE3 c e1 e2 e3 = reconstruct (c e1' e2' e3') (accCount1 +++ accCount2 +++ accCount3)
          where
            (e1', accCount1) = scopesExp e1
            (e2', accCount2) = scopesExp e2
            (e3', accCount3) = scopesExp e3

        travA :: (ScopedAcc a -> PreExp ScopedAcc ScopedExp t) -> UnscopedAcc a
              -> (ScopedExp t, NodeCounts)
        travA c acc = maybeFloatOutAcc c acc' accCount
          where
            (acc', accCount)  = scopesAcc acc

        travAE :: (ScopedAcc a -> ScopedExp b -> PreExp ScopedAcc ScopedExp t)
               -> UnscopedAcc a
               -> UnscopedExp b
               -> (ScopedExp t, NodeCounts)
        travAE c acc e = maybeFloatOutAcc (`c` e') acc' (accCountA +++ accCountE)
          where
            (acc', accCountA) = scopesAcc acc
            (e'  , accCountE) = scopesExp e

        maybeFloatOutAcc :: (ScopedAcc a -> PreExp ScopedAcc ScopedExp t)
                         -> ScopedAcc a
                         -> NodeCounts
                         -> (ScopedExp t, NodeCounts)
        maybeFloatOutAcc c acc@(ScopedAcc _ (AvarSharing _)) accCount        -- nothing to float out
          = reconstruct (c acc) accCount
        maybeFloatOutAcc c acc                 accCount
          | floatOutAcc config = reconstruct (c var) ((stableAcc `insertAccNode` noNodeCounts) +++ accCount)
          | otherwise          = reconstruct (c acc) accCount
          where
             (var, stableAcc) = abstract acc (\(ScopedAcc _ s) -> s)

        abstract :: ScopedAcc a -> (ScopedAcc a -> SharingAcc ScopedAcc ScopedExp a)
                 -> (ScopedAcc a, StableSharingAcc)
        abstract (ScopedAcc _ (AvarSharing _))       _      = INTERNAL_ERROR(error) "sharingAccToVar" "AvarSharing"
        abstract (ScopedAcc ssa (AletSharing sa acc))  lets = abstract acc (lets . (\x -> ScopedAcc ssa (AletSharing sa x)))
        abstract acc@(ScopedAcc ssa (AccSharing sn _)) lets = (ScopedAcc ssa (AvarSharing sn), StableSharingAcc sn (lets acc))

        -- Occurrence count of the currently processed node
        expOccCount = let StableNameHeight sn' _ = sn
                      in
                      lookupWithASTName expOccMap (StableASTName sn')

        -- Reconstruct the current tree node.
        --
        -- * If the current node is being shared ('expOccCount > 1'), replace it by a 'VarSharing'
        --   node and float the shared subtree out wrapped in a 'NodeCounts' value.
        -- * If the current node is not shared, reconstruct it in place.
        -- * Special case for free variables ('Tag'): Replace the tree by a sharing variable and
        --   float the 'Tag' out in a 'NodeCounts' value.  This is independent of the number of
        --   occurrences.
        --
        -- In either case, any completed 'NodeCounts' are injected as bindings using 'LetSharing'
        -- node.
        --
        reconstruct :: PreExp ScopedAcc ScopedExp t -> NodeCounts
                    -> (ScopedExp t, NodeCounts)
        reconstruct newExp@(Tag _) _subCount
              -- free variable => replace by a sharing variable regardless of the number of
              -- occurrences
          = let thisCount = StableSharingExp sn (ExpSharing sn newExp) `insertExpNode` noNodeCounts
            in
            tracePure "FREE" (show thisCount)
            (ScopedExp [] (VarSharing sn), thisCount)
        reconstruct newExp subCount
              -- shared subtree => replace by a sharing variable (if 'recoverExpSharing' enabled)
          | expOccCount > 1 && recoverExpSharing config
          = let allCount = StableSharingExp sn sharingExp `insertExpNode` newCount
            in
            tracePure ("SHARED" ++ completed) (show allCount)
            (ScopedExp [] (VarSharing sn), allCount)
              -- neither shared nor free variable => leave it as it is
          | otherwise
          = tracePure ("Normal" ++ completed) (show newCount)
            (ScopedExp [] sharingExp, newCount)
          where
              -- Determine the bindings that need to be attached to the current node...
            (newCount, bindHere) = filterCompleted subCount

              -- ...and wrap them in 'LetSharing' constructors
            lets       = foldl (flip (.)) id . map (\x y -> LetSharing x (ScopedExp [] y)) $ bindHere
            sharingExp = lets $ ExpSharing sn newExp

              -- trace support
            completed | null bindHere = ""
                      | otherwise     = "(" ++ show (length bindHere) ++ " lets)"

        -- Extract *leading* nodes that have a complete node count (i.e., their node count is equal
        -- to the number of occurrences of that node in the overall expression).
        --
        -- Nodes with a completed node count should be let bound at the currently processed node.
        --
        -- NB: Only extract leading nodes (i.e., the longest run at the *front* of the list that is
        --     complete).  Otherwise, we would let-bind subterms before their parents, which leads
        --     scope errors.
        --
        filterCompleted :: NodeCounts -> (NodeCounts, [StableSharingExp])
        filterCompleted (ns,graph)
          = let bindable       = map (isBindable bindable (map nodeName ns)) ns
                (bind, unbind) = partition fst $ zip bindable ns
            in ((map snd unbind, graph), [se | ExpNodeCount se _ <- map snd bind])
          where
            -- a node is not yet complete while the node count 'n' is below the overall number
            -- of occurrences for that node in the whole program, with the exception that free
            -- variables are never complete
            isCompleted nc@(ExpNodeCount sa n) | not . isFreeVar $ nc = lookupWithSharingExp expOccMap sa == n
            isCompleted _                                             = False

            isBindable :: [Bool] -> [NodeName] -> NodeCount -> Bool
            isBindable bindable nodes nc@(ExpNodeCount _ _) =
              let superTerms = Set.toList $ graph Map.! nodeName nc
                  unbound    = mapMaybe (`elemIndex` nodes) superTerms
              in    isCompleted nc
                 && all (bindable !!) unbound
            isBindable _ _ (AccNodeCount _ _) = False


-- |Recover sharing information and annotate the HOAS AST with variable and let binding
-- annotations.  The first argument determines whether array computations are floated out of
-- expressions irrespective of whether they are shared or not — 'True' implies floating them out.
--
-- Also returns the 'StableSharingAcc's of all 'Atag' leaves in environment order — they represent
-- the free variables of the AST.
--
-- NB: Strictly speaking, this function is not deterministic, as it uses stable pointers to
--     determine the sharing of subterms.  The stable pointer API does not guarantee its
--     completeness; i.e., it may miss some equalities, which implies that we may fail to discover
--     some sharing.  However, sharing does not affect the denotational meaning of an array
--     computation; hence, we do not compromise denotational correctness.
--
--     There is one caveat: We currently rely on the 'Atag' and 'Tag' leaves representing free
--     variables to be shared if any of them is used more than once.  If one is duplicated, the
--     environment for de Bruijn conversion will have a duplicate entry, and hence, be of the wrong
--     size, which is fatal. (The 'buildInitialEnv*' functions will already bail out.)
--
recoverSharingAcc
    :: Typeable a
    => Config
    -> Level            -- The level of currently bound array variables
    -> [Level]          -- The tags of newly introduced free array variables
    -> Acc a
    -> (ScopedAcc a, [StableSharingAcc])
{-# NOINLINE recoverSharingAcc #-}
recoverSharingAcc config alvl avars acc
  = let (acc', occMap)
          = unsafePerformIO             -- to enable stable pointers; this is safe as explained above
          $ makeOccMapAcc config alvl acc
    in
    determineScopesAcc config avars occMap acc'


recoverSharingExp
    :: Typeable e
    => Config
    -> Level            -- The level of currently bound scalar variables
    -> [Level]          -- The tags of newly introduced free scalar variables
    -> Exp e
    -> (ScopedExp e, [StableSharingExp])
{-# NOINLINE recoverSharingExp #-}
recoverSharingExp config lvl fvar exp
  = let
        (rootExp, accOccMap) = unsafePerformIO $ do
          accOccMap       <- newASTHashTable
          (exp', _)       <- makeOccMapRootExp config accOccMap lvl fvar exp
          frozenAccOccMap <- freezeOccMap accOccMap

          return (exp', frozenAccOccMap)

        (ScopedExp sse sharingExp, _) =
          determineScopesExp config accOccMap rootExp
    in
    (ScopedExp [] sharingExp, sse)


-- Debugging
-- ---------

traceLine :: String -> String -> IO ()
traceLine header msg
  = Debug.traceMessage Debug.dump_sharing
  $ header ++ ": " ++ msg

traceChunk :: String -> String -> IO ()
traceChunk header msg
  = Debug.traceMessage Debug.dump_sharing
  $ header ++ "\n      " ++ msg

tracePure :: String -> String -> a -> a
tracePure header msg
  = Debug.tracePure Debug.dump_sharing
  $ header ++ ": " ++ msg

