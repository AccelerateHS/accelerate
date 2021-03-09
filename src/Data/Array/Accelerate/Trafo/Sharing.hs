{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Sharing
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements HOAS to de Bruijn conversion of array expressions
-- while incorporating sharing information.
--

module Data.Array.Accelerate.Trafo.Sharing (

  -- * HOAS to de Bruijn conversion
  convertAcc, convertAccWith,

  Afunction, AfunctionR, ArraysFunctionR, AfunctionRepr(..), afunctionRepr,
  convertAfun, convertAfunWith,

  Function, FunctionR, EltFunctionR, FunctionRepr(..), functionRepr,
  convertExp, convertExpWith,
  convertFun, convertFunWith,

  -- convertSeq

) where

import Data.Array.Accelerate.AST                                    hiding ( PreOpenAcc(..), OpenAcc(..), Acc, OpenExp(..), Exp, Boundary(..), HasArraysR(..), showPreAccOp )
import Data.Array.Accelerate.AST.Environment
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Debug.Internal.Flags                   as Debug
import Data.Array.Accelerate.Debug.Internal.Trace                   as Debug
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Array                   ( Array, ArraysR, ArrayR(..), showArraysR )
import Data.Array.Accelerate.Representation.Shape                   hiding ( zip )
import Data.Array.Accelerate.Representation.Stencil
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Smart                                  as Smart hiding ( StencilR )
import Data.Array.Accelerate.Sugar.Array                            hiding ( Array, ArraysR, (!!) )
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Trafo.Config
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Trafo.Var
import Data.Array.Accelerate.Type
import Data.BitSet                                                  ( (\\), member )
import qualified Data.Array.Accelerate.AST                          as AST
import qualified Data.Array.Accelerate.Representation.Stencil       as R
import qualified Data.Array.Accelerate.Sugar.Array                  as Sugar

import Control.Applicative                                          hiding ( Const )
import Lens.Micro                                                 ( over, mapped, _1, _2 )
import Control.Monad.Fix
import Data.Function                                                ( on )
import Data.Hashable
import Data.List                                                    ( elemIndex, findIndex, groupBy, intercalate, partition )
import Data.Maybe
import Data.Monoid                                                  ( Any(..) )
import System.IO.Unsafe                                             ( unsafePerformIO )
import System.Mem.StableName
import Text.Printf
import qualified Data.HashMap.Strict                                as Map
import qualified Data.HashSet                                       as Set
import qualified Data.HashTable.IO                                  as Hash
import qualified Data.IntMap                                        as IntMap
import Prelude


-- Layouts
-- -------

-- A layout of an environment has an entry for each entry of the environment.
-- Each entry in the layout holds the de Bruijn index that refers to the
-- corresponding entry in the environment.
--
data Layout s env env' where
  EmptyLayout :: Layout s env ()
  PushLayout  :: Layout s env env1
              -> LeftHandSide s t env1 env2
              -> Vars s env t
              -> Layout s env env2

type ELayout = Layout ScalarType
type ArrayLayout = Layout ArrayR


-- Project the nth index out of an environment layout.
--
-- The first argument provides context information for error messages in the
-- case of failure.
--
prjIdx :: forall s t env env1. HasCallStack
       => String
       -> (forall t'. TupR s t' -> ShowS)
       -> (forall u v. TupR s u -> TupR s v -> Maybe (u :~: v))
       -> TupR s t
       -> Int
       -> Layout s env env1
       -> Vars s env t
prjIdx context showTp matchTp tp = go
  where
    go :: forall env'. HasCallStack => Int -> Layout s env env' -> Vars s env t
    go _ EmptyLayout                        = no "environment does not contain index"
    go 0 (PushLayout _ lhs vars)
      | Just Refl <- matchTp tp tp'         = vars
      | otherwise                           = no $ printf "couldn't match expected type `%s' with actual type `%s'"
                                                          (showTp tp  "")
                                                          (showTp tp' "")
      where
        tp' = lhsToTupR lhs
    go n (PushLayout l _ _)                 = go (n-1) l

    no :: HasCallStack => String -> a
    no reason = internalError (printf "%s\nin the context: %s" reason context)

-- Add an entry to a layout, incrementing all indices
--
incLayout :: env1 :> env2 -> Layout s env1 env' -> Layout s env2 env'
incLayout _ EmptyLayout            = EmptyLayout
incLayout k (PushLayout lyt lhs v) = PushLayout (incLayout k lyt) lhs (weakenVars k v)

sizeLayout :: Layout s env env' -> Int
sizeLayout EmptyLayout          = 0
sizeLayout (PushLayout lyt _ _) = 1 + sizeLayout lyt

-- Conversion from HOAS to de Bruijn computation AST
-- =================================================

-- Array computations
-- ------------------

-- | Convert a closed array expression to de Bruijn form while also incorporating sharing
-- information.
--
convertAcc :: HasCallStack => Acc arrs -> AST.Acc (Sugar.ArraysR arrs)
convertAcc = convertAccWith defaultOptions

convertAccWith :: HasCallStack => Config -> Acc arrs -> AST.Acc (Sugar.ArraysR arrs)
convertAccWith config (Acc acc) = convertOpenAcc config EmptyLayout acc


-- | Convert a closed function over array computations, while incorporating
-- sharing information.
--
convertAfun :: HasCallStack => Afunction f => f -> AST.Afun (ArraysFunctionR f)
convertAfun = convertAfunWith defaultOptions

convertAfunWith :: HasCallStack => Afunction f => Config -> f -> AST.Afun (ArraysFunctionR f)
convertAfunWith config = convertOpenAfun config EmptyLayout

data AfunctionRepr f ar areprr where
  AfunctionReprBody
    :: Arrays b => AfunctionRepr (Acc b) b (Sugar.ArraysR b)

  AfunctionReprLam
    :: Arrays a
    => AfunctionRepr b br breprr
    -> AfunctionRepr (Acc a -> b) (a -> br) (Sugar.ArraysR a -> breprr)

-- Convert a HOAS fragment into de Bruijn form, binding variables into the typed
-- environment layout one binder at a time.
--
-- NOTE: Because we convert one binder at a time left-to-right, the bound
--       variables ('vars') will have de Bruijn index _zero_ as the outermost
--       binding, and thus go to the end of the list.
--
class Afunction f where
  type AfunctionR f
  type ArraysFunctionR f
  afunctionRepr   :: HasCallStack => AfunctionRepr f (AfunctionR f) (ArraysFunctionR f)
  convertOpenAfun :: HasCallStack => Config -> ArrayLayout aenv aenv -> f -> AST.OpenAfun aenv (ArraysFunctionR f)

instance (Arrays a, Afunction r) => Afunction (Acc a -> r) where
  type AfunctionR      (Acc a -> r) = a -> AfunctionR r
  type ArraysFunctionR (Acc a -> r) = Sugar.ArraysR a -> ArraysFunctionR r

  afunctionRepr = AfunctionReprLam $ afunctionRepr @r
  convertOpenAfun config alyt f
    | repr <- Sugar.arraysR @a
    , DeclareVars lhs k value <- declareVars repr
    = let
        a     = Acc $ SmartAcc $ Atag repr $ sizeLayout alyt
        alyt' = PushLayout (incLayout k alyt) lhs (value weakenId)
      in
        Alam lhs $ convertOpenAfun config alyt' $ f a

instance Arrays b => Afunction (Acc b) where
  type AfunctionR      (Acc b) = b
  type ArraysFunctionR (Acc b) = Sugar.ArraysR b
  afunctionRepr = AfunctionReprBody
  convertOpenAfun config alyt (Acc body) = Abody $ convertOpenAcc config alyt body

convertSmartAfun1
    :: HasCallStack
    => Config
    -> ArraysR a
    -> (SmartAcc a -> SmartAcc b)
    -> AST.Afun (a -> b)
convertSmartAfun1 config repr f
  | DeclareVars lhs _ value <- declareVars repr
  = let
      a     = SmartAcc $ Atag repr 0
      alyt' = PushLayout EmptyLayout lhs (value weakenId)
    in
      Alam lhs $ Abody $ convertOpenAcc config alyt' $ f a

-- | Convert an open array expression to de Bruijn form while also incorporating sharing
-- information.
--
convertOpenAcc
    :: HasCallStack
    => Config
    -> ArrayLayout aenv aenv
    -> SmartAcc arrs
    -> AST.OpenAcc aenv arrs
convertOpenAcc config alyt acc =
  let lvl                      = sizeLayout alyt
      fvs                      = [lvl-1, lvl-2 .. 0]
      (sharingAcc, initialEnv) = recoverSharingAcc config lvl fvs acc
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
    :: forall aenv arrs. HasCallStack
    => Config
    -> ArrayLayout aenv aenv
    -> [StableSharingAcc]
    -> ScopedAcc arrs
    -> AST.OpenAcc aenv arrs
convertSharingAcc _ alyt aenv (ScopedAcc lams (AvarSharing sa repr))
  | Just i <- findIndex (matchStableAcc sa) aenv'
  = avarsIn AST.OpenAcc
  $ prjIdx (ctxt ++ "; i = " ++ show i) showArraysR matchArraysR repr i alyt
  | null aenv'
  = error $ "Cyclic definition of a value of type 'Acc' (sa = " ++ show (hashStableNameHeight sa) ++ ")"
  | otherwise
  = internalError err
  where
    aenv' = lams ++ aenv
    ctxt = "shared 'Acc' tree with stable name " ++ show (hashStableNameHeight sa)
    err  = "inconsistent valuation @ " ++ ctxt ++ ";\n  aenv = " ++ show aenv'

convertSharingAcc config alyt aenv (ScopedAcc lams (AletSharing sa@(StableSharingAcc (_ :: StableAccName as) boundAcc) bodyAcc))
  = case declareVars $ AST.arraysR bound of
      DeclareVars lhs k value ->
        let
          alyt' = PushLayout (incLayout k alyt) lhs (value weakenId)
        in
          AST.OpenAcc $ AST.Alet
            lhs
            bound
            (convertSharingAcc config alyt' (sa:aenv') bodyAcc)
  where
    aenv' = lams ++ aenv
    bound = convertSharingAcc config alyt aenv' (ScopedAcc [] boundAcc)

convertSharingAcc config alyt aenv (ScopedAcc lams (AccSharing _ preAcc))
  = AST.OpenAcc
  $ let aenv' = lams ++ aenv

        cvtA :: ScopedAcc a -> AST.OpenAcc aenv a
        cvtA = convertSharingAcc config alyt aenv'

        cvtE :: ScopedExp t -> AST.Exp aenv t
        cvtE = convertSharingExp config EmptyLayout alyt [] aenv'

        cvtF1 :: TypeR a -> (SmartExp a -> ScopedExp b) -> AST.Fun aenv (a -> b)
        cvtF1 = convertSharingFun1 config alyt aenv'

        cvtF2 :: TypeR a -> TypeR b -> (SmartExp a -> SmartExp b -> ScopedExp c) -> AST.Fun aenv (a -> b -> c)
        cvtF2 = convertSharingFun2 config alyt aenv'

        cvtAfun1 :: ArraysR a -> (SmartAcc a -> ScopedAcc b) -> AST.OpenAfun aenv (a -> b)
        cvtAfun1 = convertSharingAfun1 config alyt aenv'

        cvtAprj :: forall a b c. PairIdx (a, b) c -> ScopedAcc (a, b) -> AST.OpenAcc aenv c
        cvtAprj ix a = cvtAprj' ix $ cvtA a

        cvtAprj' :: forall a b c aenv1. PairIdx (a, b) c -> AST.OpenAcc aenv1 (a, b) -> AST.OpenAcc aenv1 c
        cvtAprj' PairIdxLeft  (AST.OpenAcc (AST.Apair a _)) = a
        cvtAprj' PairIdxRight (AST.OpenAcc (AST.Apair _ b)) = b
        cvtAprj' ix a = case declareVars $ AST.arraysR a of
          DeclareVars lhs _ value ->
            AST.OpenAcc $ AST.Alet lhs a $ cvtAprj' ix $ avarsIn AST.OpenAcc $ value weakenId
    in
    case preAcc of

      Atag repr i
        -> let AST.OpenAcc a = avarsIn AST.OpenAcc $ prjIdx ("de Bruijn conversion tag " ++ show i) showArraysR matchArraysR repr i alyt
           in  a

      Pipe reprA reprB reprC (afun1 :: SmartAcc as -> ScopedAcc bs) (afun2 :: SmartAcc bs -> ScopedAcc cs) acc
        | DeclareVars lhs k value <- declareVars reprB ->
          let
            noStableSharing = StableSharingAcc noStableAccName (undefined :: SharingAcc acc exp ())
            boundAcc = AST.Apply reprB (cvtAfun1 reprA afun1) (cvtA acc)
            alyt'   = PushLayout (incLayout k alyt) lhs (value weakenId)
            bodyAcc = AST.Apply reprC
                        (convertSharingAfun1 config alyt' (noStableSharing : aenv') reprB afun2)
                        (avarsIn AST.OpenAcc $ value weakenId)
          in AST.Alet lhs (AST.OpenAcc boundAcc) (AST.OpenAcc bodyAcc)

      Aforeign repr ff afun acc
        -> AST.Aforeign repr ff (convertSmartAfun1 config (Smart.arraysR acc) afun) (cvtA acc)

      Acond b acc1 acc2           -> AST.Acond (cvtE b) (cvtA acc1) (cvtA acc2)
      Awhile reprA pred iter init -> AST.Awhile (cvtAfun1 reprA pred) (cvtAfun1 reprA iter) (cvtA init)
      Anil                        -> AST.Anil
      Apair acc1 acc2             -> AST.Apair (cvtA acc1) (cvtA acc2)
      Aprj ix a                   -> let AST.OpenAcc a' = cvtAprj ix a
                                     in a'
      Atrace msg acc1 acc2        -> AST.Atrace msg (cvtA acc1) (cvtA acc2)
      Aerror repr msg acc1        -> AST.Aerror repr msg (cvtA acc1)
      Use repr array              -> AST.Use repr array
      Unit tp e                   -> AST.Unit tp (cvtE e)
      Generate repr@(ArrayR shr _) sh f
                                  -> AST.Generate repr (cvtE sh) (cvtF1 (shapeType shr) f)
      Reshape shr e acc           -> AST.Reshape shr (cvtE e) (cvtA acc)
      Replicate si ix acc         -> AST.Replicate si (cvtE ix) (cvtA acc)
      Slice si acc ix             -> AST.Slice si (cvtA acc) (cvtE ix)
      Map t1 t2 f acc             -> AST.Map t2 (cvtF1 t1 f) (cvtA acc)
      ZipWith t1 t2 t3 f acc1 acc2
                                  -> AST.ZipWith t3 (cvtF2 t1 t2 f) (cvtA acc1) (cvtA acc2)
      Fold tp f e acc             -> AST.Fold (cvtF2 tp tp f) (cvtE <$> e) (cvtA acc)
      FoldSeg i tp f e acc1 acc2  -> AST.FoldSeg i (cvtF2 tp tp f) (cvtE <$> e) (cvtA acc1) (cvtA acc2)
      Scan  d tp f e acc          -> AST.Scan  d (cvtF2 tp tp f) (cvtE <$> e) (cvtA acc)
      Scan' d tp f e acc          -> AST.Scan' d (cvtF2 tp tp f) (cvtE e)     (cvtA acc)
      Permute (ArrayR shr tp) f dftAcc perm acc
                                  -> AST.Permute (cvtF2 tp tp f) (cvtA dftAcc) (cvtF1 (shapeType shr) perm) (cvtA acc)
      Backpermute shr newDim perm acc
                                  -> AST.Backpermute shr (cvtE newDim) (cvtF1 (shapeType shr) perm) (cvtA acc)
      Stencil stencil tp f boundary acc
        -> AST.Stencil stencil
                       tp
                       (convertSharingStencilFun1 config alyt aenv' stencil f)
                       (convertSharingBoundary config alyt aenv' (stencilShapeR stencil) boundary)
                       (cvtA acc)
      Stencil2 stencil1 stencil2 tp f bndy1 acc1 bndy2 acc2
        | shr <- stencilShapeR stencil1
        -> AST.Stencil2 stencil1
                        stencil2
                        tp
                        (convertSharingStencilFun2 config alyt aenv' stencil1 stencil2 f)
                        (convertSharingBoundary config alyt aenv' shr bndy1)
                        (cvtA acc1)
                        (convertSharingBoundary config alyt aenv' shr bndy2)
                        (cvtA acc2)
      -- Collect seq -> AST.Collect (convertSharingSeq config alyt EmptyLayout aenv' [] seq)

{--
-- Sequence expressions
-- --------------------

-- | Convert a closed sequence expression to de Bruijn form while incorporating
-- sharing information.
--
convertSeq
    :: Typeable s
    => Bool             -- ^ recover sharing of array computations ?
    -> Bool             -- ^ recover sharing of scalar expressions ?
    -> Bool             -- ^ recover sharing of sequence computations ?
    -> Bool             -- ^ always float array computations out of expressions?
    -> Seq s            -- ^ computation to be converted
    -> AST.Seq s
convertSeq shareAcc shareExp shareSeq floatAcc seq
  = let config = Config shareAcc shareExp shareSeq floatAcc
        (sharingSeq, initialEnv) = recoverSharingSeq config seq
    in
    convertSharingSeq config EmptyLayout EmptyLayout [] initialEnv sharingSeq

convertSharingSeq
    :: forall aenv senv arrs.
       Config
    -> Layout aenv aenv
    -> Layout senv senv
    -> [StableSharingAcc]
    -> [StableSharingSeq]
    -> ScopedSeq arrs
    -> AST.PreOpenSeq AST.OpenAcc aenv senv arrs
convertSharingSeq _ _ slyt _ senv (ScopedSeq (SvarSharing sn))
  | Just i <- findIndex (matchStableSeq sn) senv
  = AST.Reify $ prjIdx (ctxt ++ "; i = " ++ show i) i slyt
  | null senv
  = error $ "Cyclic definition of a value of type 'Seq' (sa = " ++
            show (hashStableNameHeight sn) ++ ")"
  | otherwise
  = $internalError "convertSharingSeq" err
  where
    ctxt = "shared 'Seq' tree with stable name " ++ show (hashStableNameHeight sn)
    err  = "inconsistent valuation @ " ++ ctxt ++ ";\n  senv = " ++ show senv
convertSharingSeq config alyt slyt aenv senv (ScopedSeq (SletSharing sa@(StableSharingSeq _ (SeqSharing _ boundSeq)) bodySeq))
  = convSeq boundSeq bodySeq
  where
    convSeq :: forall bnd body.
               PreSeq ScopedAcc ScopedSeq ScopedExp bnd
            -> ScopedSeq body
            -> AST.PreOpenSeq AST.OpenAcc aenv senv body
    convSeq bnd body =
      case bnd of
        StreamIn arrs               -> producer $ AST.StreamIn arrs
        ToSeq slix acc              -> producer $ mkToSeq slix (cvtA acc)
        MapSeq afun x               -> producer $ AST.MapSeq (cvtAF1 afun) (asIdx x)
        ZipWithSeq afun x y         -> producer $ AST.ZipWithSeq (cvtAF2 afun) (asIdx x) (asIdx y)
        ScanSeq fun e x             -> producer $ AST.ScanSeq (cvtF2 fun) (cvtE e) (asIdx x)
        _                           -> $internalError "convertSharingSeq:convSeq" "Consumer appears to have been let bound"
      where
        producer :: Arrays a
                 => AST.Producer AST.OpenAcc aenv senv a
                 -> AST.PreOpenSeq AST.OpenAcc aenv senv body
        producer p = AST.Producer p $ convertSharingSeq config alyt slyt' aenv (sa:senv) body
          where
            slyt' = incLayout slyt `PushLayout` ZeroIdx

        asIdx :: (HasCallStack, Arrays a)
              => ScopedSeq [a]
              -> Idx senv a
        asIdx (ScopedSeq (SvarSharing sn))
          | Just i <- findIndex (matchStableSeq sn) senv
          = prjIdx (ctxt ++ "; i = " ++ show i) i slyt
          | null senv
          = error $ "Cyclic definition of a value of type 'Seq' (sa = " ++
                    show (hashStableNameHeight sn) ++ ")"
          | otherwise
          = $internalError "convertSharingSeq" err
          where
            ctxt = "shared 'Seq' tree with stable name " ++ show (hashStableNameHeight sn)
            err  = "inconsistent valuation @ " ++ ctxt ++ ";\n  senv = " ++ show senv
        asIdx _
          = $internalError "convertSharingSeq:asIdx" "Sequence computation not in A-normal form"

        cvtA :: forall a. Arrays a => ScopedAcc a -> AST.OpenAcc aenv a
        cvtA acc = convertSharingAcc config alyt aenv acc

        cvtE :: forall t. Elt t => ScopedExp t -> AST.Exp aenv t
        cvtE = convertSharingExp config EmptyLayout alyt [] aenv

        cvtF2 :: (Elt a, Elt b, Elt c) => (Exp a -> Exp b -> ScopedExp c) -> AST.Fun aenv (a -> b -> c)
        cvtF2 = convertSharingFun2 config alyt aenv

        cvtAF1 :: forall a b. (Arrays a, Arrays b) => (Acc a -> ScopedAcc b) -> OpenAfun aenv (a -> b)
        cvtAF1 afun = convertSharingAfun1 config alyt aenv afun

        cvtAF2 :: forall a b c. (Arrays a, Arrays b, Arrays c) => (Acc a -> Acc b -> ScopedAcc c) -> OpenAfun aenv (a -> b -> c)
        cvtAF2 afun = convertSharingAfun2 config alyt aenv afun

convertSharingSeq _ _ _ _ _ (ScopedSeq (SletSharing _ _))
 = $internalError "convertSharingSeq" "Sequence computation not in A-normal form"

convertSharingSeq config alyt slyt aenv senv s
  = cvtC s
  where
    cvtC :: ScopedSeq a -> AST.PreOpenSeq AST.OpenAcc aenv senv a
    cvtC (ScopedSeq (SeqSharing _ s)) =
      case s of
        FoldSeq fun e x                    -> AST.Consumer $ AST.FoldSeq (cvtF2 fun) (cvtE e) (asIdx x)
        FoldSeqFlatten afun acc x          -> AST.Consumer $ AST.FoldSeqFlatten (cvtAF3 afun) (cvtA acc) (asIdx x)
        Stuple t                           -> AST.Consumer $ AST.Stuple (cvtST t)
        _                                  -> $internalError "convertSharingSeq" "Producer has not been let bound"
    cvtC _ = $internalError "convertSharingSeq" "Unreachable"

    asIdx :: Arrays a
          => ScopedSeq [a]
          -> Idx senv a
    asIdx (ScopedSeq (SvarSharing sn))
      | Just i <- findIndex (matchStableSeq sn) senv
      = prjIdx (ctxt ++ "; i = " ++ show i) i slyt
      | null senv
      = error $ "Cyclic definition of a value of type 'Seq' (sa = " ++
                show (hashStableNameHeight sn) ++ ")"
      | otherwise
      = $internalError "convertSharingSeq" err
      where
        ctxt = "shared 'Seq' tree with stable name " ++ show (hashStableNameHeight sn)
        err  = "inconsistent valuation @ " ++ ctxt ++ ";\n  senv = " ++ show senv
    asIdx _
      = $internalError "convertSharingSeq:asIdx" "Sequence computation not in A-normal form"

    cvtA :: forall a. Arrays a => ScopedAcc a -> AST.OpenAcc aenv a
    cvtA acc = convertSharingAcc config alyt aenv acc

    cvtE :: forall t. Elt t => ScopedExp t -> AST.Exp aenv t
    cvtE = convertSharingExp config EmptyLayout alyt [] aenv

    cvtF2 :: (Elt a, Elt b, Elt c) => (Exp a -> Exp b -> ScopedExp c) -> AST.Fun aenv (a -> b -> c)
    cvtF2 = convertSharingFun2 config alyt aenv

    cvtAF3 :: forall a b c d. (Arrays a, Arrays b, Arrays c, Arrays d) => (Acc a -> Acc b -> Acc c -> ScopedAcc d) -> OpenAfun aenv (a -> b -> c -> d)
    cvtAF3 afun = convertSharingAfun3 config alyt aenv afun

    cvtST :: Atuple ScopedSeq t -> Atuple (AST.Consumer AST.OpenAcc aenv senv) t
    cvtST NilAtup        = NilAtup
    cvtST (SnocAtup t c) | AST.Consumer c' <- cvtC c
                         = SnocAtup (cvtST t) c'
                         | otherwise
                         = $internalError "convertSharingSeq" "Unreachable"
--}

convertSharingAfun1
    :: forall aenv a b. HasCallStack
    => Config
    -> ArrayLayout aenv aenv
    -> [StableSharingAcc]
    -> ArraysR a
    -> (SmartAcc a -> ScopedAcc b)
    -> OpenAfun aenv (a -> b)
convertSharingAfun1 config alyt aenv reprA f
  | DeclareVars lhs k value <- declareVars reprA
  = let
      alyt' = PushLayout (incLayout k alyt) lhs (value weakenId)
      body = f undefined
    in
      Alam lhs (Abody (convertSharingAcc config alyt' aenv body))

-- | Convert a boundary condition
--
convertSharingBoundary
    :: forall aenv sh e. HasCallStack
    => Config
    -> ArrayLayout aenv aenv
    -> [StableSharingAcc]
    -> ShapeR sh
    -> PreBoundary ScopedAcc ScopedExp (Array sh e)
    -> AST.Boundary aenv (Array sh e)
convertSharingBoundary config alyt aenv shr = cvt
  where
    cvt :: PreBoundary ScopedAcc ScopedExp (Array sh e) -> AST.Boundary aenv (Array sh e)
    cvt bndy =
      case bndy of
        Clamp       -> AST.Clamp
        Mirror      -> AST.Mirror
        Wrap        -> AST.Wrap
        Constant v  -> AST.Constant v
        Function f  -> AST.Function $ convertSharingFun1 config alyt aenv (shapeType shr) f


-- mkToSeq :: forall slsix slix e aenv senv. (Division slsix, DivisionSlice slsix ~ slix, Elt e, Elt slix, Slice slix)
--         => slsix
--         -> AST.OpenAcc              aenv (Array (FullShape  slix) e)
--         -> AST.Producer AST.OpenAcc aenv senv (Array (SliceShape slix) e)
-- mkToSeq _ = AST.ToSeq (sliceIndex slix) (Proxy :: Proxy slix)
--   where
--     slix = undefined :: slix


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
convertFun :: (HasCallStack, Function f) => f -> AST.Fun () (EltFunctionR f)
convertFun
  = convertFunWith
  $ defaultOptions { options = options defaultOptions \\ [seq_sharing, acc_sharing] }

convertFunWith :: (HasCallStack, Function f) => Config -> f -> AST.Fun () (EltFunctionR f)
convertFunWith config = convertOpenFun config EmptyLayout

data FunctionRepr f r reprr where
  FunctionReprBody
    :: Elt b => FunctionRepr (Exp b) b (EltR b)

  FunctionReprLam
    :: Elt a
    => FunctionRepr b br breprr
    -> FunctionRepr (Exp a -> b) (a -> br) (EltR a -> breprr)

class Function f where
  type FunctionR f
  type EltFunctionR f

  functionRepr   :: HasCallStack => FunctionRepr f (FunctionR f) (EltFunctionR f)
  convertOpenFun :: HasCallStack => Config -> ELayout env env -> f -> AST.OpenFun env () (EltFunctionR f)

instance (Elt a, Function r) => Function (Exp a -> r) where
  type FunctionR (Exp a -> r) = a -> FunctionR r
  type EltFunctionR (Exp a -> r) = EltR a -> EltFunctionR r

  functionRepr = FunctionReprLam $ functionRepr @r
  convertOpenFun config lyt f
    | tp <- eltR @a
    , DeclareVars lhs k value <- declareVars tp
    = let
        e    = Exp $ SmartExp $ Tag tp $ sizeLayout lyt
        lyt' = PushLayout (incLayout k lyt) lhs (value weakenId)
      in
        Lam lhs $ convertOpenFun config lyt' $ f e

instance Elt b => Function (Exp b) where
  type FunctionR (Exp b) = b
  type EltFunctionR (Exp b) = EltR b

  functionRepr = FunctionReprBody
  convertOpenFun config lyt (Exp body) = Body $ convertOpenExp config lyt body

convertSmartFun
    :: HasCallStack
    => Config
    -> TypeR a
    -> (SmartExp a -> SmartExp b)
    -> AST.Fun () (a -> b)
convertSmartFun config tp f
  | DeclareVars lhs _ value <- declareVars tp
  = let
      e    = SmartExp $ Tag tp 0
      lyt' = PushLayout EmptyLayout lhs (value weakenId)
    in
      Lam lhs $ Body $ convertOpenExp config lyt' $ f e

-- Scalar expressions
-- ------------------

-- | Convert a closed scalar expression to de Bruijn form while incorporating
-- sharing information.
--
convertExp
    :: HasCallStack
    => Exp e
    -> AST.Exp () (EltR e)
convertExp
  = convertExpWith
  $ defaultOptions { options = options defaultOptions \\ [seq_sharing, acc_sharing] }

convertExpWith
      :: HasCallStack
      => Config
      -> Exp e
      -> AST.Exp () (EltR e)
convertExpWith config (Exp e) = convertOpenExp config EmptyLayout e

convertOpenExp
    :: HasCallStack
    => Config
    -> ELayout env env
    -> SmartExp e
    -> AST.OpenExp env () e
convertOpenExp config lyt exp =
  let lvl                      = sizeLayout lyt
      fvs                      = [lvl-1, lvl-2 .. 0]
      (sharingExp, initialEnv) = recoverSharingExp config lvl fvs exp
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
    :: forall t env aenv. HasCallStack
    => Config
    -> ELayout env env          -- scalar environment
    -> ArrayLayout aenv aenv    -- array environment
    -> [StableSharingExp]       -- currently bound sharing variables of expressions
    -> [StableSharingAcc]       -- currently bound sharing variables of array computations
    -> ScopedExp t              -- expression to be converted
    -> AST.OpenExp env aenv t
convertSharingExp config lyt alyt env aenv exp@(ScopedExp lams _) = cvt exp
  where
    -- scalar environment with any lambda bound variables this expression is rooted in
    env' = lams ++ env

    cvt :: HasCallStack => ScopedExp t' -> AST.OpenExp env aenv t'
    cvt (ScopedExp _ (VarSharing se tp))
      | Just i <- findIndex (matchStableExp se) env' = expVars (prjIdx (ctx i) shows matchTypeR tp i lyt)
      | otherwise                                    = internalError msg
      where
        ctx i = printf "shared 'Exp' tree with stable name %d; i=%d" (hashStableNameHeight se) i
        msg   = unlines
          [ if null env'
               then printf "cyclic definition of a value of type 'Exp' (sa=%d)" (hashStableNameHeight se)
               else printf "inconsistent valuation at shared 'Exp' tree (sa=%d; env=%s)" (hashStableNameHeight se) (show env')
          , ""
          , "Note that this error usually arises due to the presence of nested data"
          , "parallelism; when a parallel computation attempts to initiate new parallel"
          , "work _which depends on_ a scalar variable given by the first computation."
          , ""
          , "For example, suppose we wish to sum the columns of a two-dimensional array."
          , "You might think to do this in the following (incorrect) way: by constructing"
          , "a vector using 'generate' where at each index we 'slice' out the"
          , "corresponding column of the matrix and 'sum' it:"
          , ""
          , "> sum_columns_ndp :: Num a => Acc (Matrix a) -> Acc (Vector a)"
          , "> sum_columns_ndp mat ="
          , ">   let I2 rows cols = shape mat"
          , ">   in  generate (I1 cols)"
          , ">                (\\(I1 col) -> the $ sum (slice mat (lift (Z :. All :. col))))"
          , ""
          , "However, since both 'generate' and 'slice' are data-parallel operators, and"
          , "moreover that 'slice' _depends on_ the argument 'col' given to it by the"
          , "'generate' function, this operation requires nested parallelism and is thus"
          , "not (at this time) permitted. The clue that this definition is invalid is"
          , "that in order to create a program which will be accepted by the type checker,"
          , "we had to use the function 'the' to retrieve the result of the parallel"
          , "'sum', effectively concealing that this is a collective operation in order to"
          , "match the type expected by 'generate'."
          , ""
          , "To solve this particular example, we can make use of the fact that (most)"
          , "collective operations in Accelerate are _rank polymorphic_. The 'sum'"
          , "operation reduces along the innermost dimension of an array of arbitrary"
          , "rank, reducing the dimensionality of the array by one. To reduce the array"
          , "column-wise then, we first need to simply 'transpose' the array:"
          , ""
          , "> sum_columns :: Num a => Acc (Matrix a) -> Acc (Vector a)"
          , "> sum_columns = sum . transpose"
          , ""
          , "If you feel like this is not the cause of your error, or you would like some"
          , "advice locating the problem and perhaps with a workaround, feel free to"
          , "submit an issue at the above URL."
          ]

    cvt (ScopedExp _ (LetSharing se@(StableSharingExp _ boundExp) bodyExp))
      | DeclareVars lhs k value <- declareVars $ typeR boundExp
      = let
          lyt' = PushLayout (incLayout k lyt) lhs (value weakenId)
        in
          AST.Let lhs (cvt (ScopedExp [] boundExp)) (convertSharingExp config lyt' alyt (se:env') aenv bodyExp)
    cvt (ScopedExp _ (ExpSharing _ pexp))
      = case pexp of
          Tag tp i              -> expVars $ prjIdx ("de Bruijn conversion tag " ++ show i) shows matchTypeR tp i lyt
          Match _ e             -> cvt e  -- XXX: this should probably be an error
          Const tp v            -> AST.Const tp v
          Undef tp              -> AST.Undef tp
          Prj idx e             -> cvtPrj idx (cvt e)
          Nil                   -> AST.Nil
          Pair e1 e2            -> AST.Pair (cvt e1) (cvt e2)
          VecPack   vec e       -> AST.VecPack   vec (cvt e)
          VecUnpack vec e       -> AST.VecUnpack vec (cvt e)
          ToIndex shr sh ix     -> AST.ToIndex shr (cvt sh) (cvt ix)
          FromIndex shr sh e    -> AST.FromIndex shr (cvt sh) (cvt e)
          Case e rhs            -> cvtCase (cvt e) (over (mapped . _2) cvt rhs)
          Cond e1 e2 e3         -> AST.Cond (cvt e1) (cvt e2) (cvt e3)
          While tp p it i       -> AST.While (cvtFun1 tp p) (cvtFun1 tp it) (cvt i)
          PrimConst c           -> AST.PrimConst c
          PrimApp f e           -> cvtPrimFun f (cvt e)
          Index _ a e           -> AST.Index (cvtAvar a) (cvt e)
          LinearIndex _ a i     -> AST.LinearIndex (cvtAvar a) (cvt i)
          Shape _ a             -> AST.Shape (cvtAvar a)
          ShapeSize shr e       -> AST.ShapeSize shr (cvt e)
          Foreign repr ff f e   -> AST.Foreign repr ff (convertSmartFun config (typeR e) f) (cvt e)
          Coerce t1 t2 e        -> AST.Coerce t1 t2 (cvt e)

    cvtPrj :: forall a b c env1 aenv1. PairIdx (a, b) c -> AST.OpenExp env1 aenv1 (a, b) -> AST.OpenExp env1 aenv1 c
    cvtPrj PairIdxLeft  (AST.Pair a _) = a
    cvtPrj PairIdxRight (AST.Pair _ b) = b
    cvtPrj ix a
      | DeclareVars lhs _ value <- declareVars $ AST.expType a
      = AST.Let lhs a (cvtPrj ix (expVars (value weakenId)))

    cvtA :: HasCallStack => ScopedAcc a -> AST.OpenAcc aenv a
    cvtA = convertSharingAcc config alyt aenv

    cvtAvar :: HasCallStack => ScopedAcc a -> AST.ArrayVar aenv a
    cvtAvar a = case cvtA a of
      AST.OpenAcc (AST.Avar var) -> var
      _                          -> internalError "Expected array computation in expression to be floated out"

    cvtFun1 :: HasCallStack => TypeR a -> (SmartExp a -> ScopedExp b) -> AST.OpenFun env aenv (a -> b)
    cvtFun1 tp f
      | DeclareVars lhs k value <- declareVars tp
      = let
          lyt' = PushLayout (incLayout k lyt) lhs (value weakenId)
          body = f undefined
        in
          Lam lhs $ Body $ convertSharingExp config lyt' alyt env' aenv body

    -- Push primitive function applications down through let bindings so that
    -- they are adjacent to their arguments. It looks a bit nicer this way.
    --
    cvtPrimFun :: HasCallStack => AST.PrimFun (a -> r) -> AST.OpenExp env' aenv' a -> AST.OpenExp env' aenv' r
    cvtPrimFun f e = case e of
      AST.Let lhs bnd body -> AST.Let lhs bnd (cvtPrimFun f body)
      x                    -> AST.PrimApp f x

    -- Convert the flat list of equations into nested case statement
    -- directly on the tag variables.
    --
    cvtCase :: HasCallStack => AST.OpenExp env' aenv' a -> [(TagR a, AST.OpenExp env' aenv' b)] -> AST.OpenExp env' aenv' b
    cvtCase s es
      | AST.Pair{} <- s
      = nested s es
      | DeclareVars lhs _ value <- declareVars (AST.expType s)
      = AST.Let lhs s $ nested (expVars (value weakenId)) (over (mapped . _2) (weakenE (weakenWithLHS lhs)) es)
      where
        nested :: HasCallStack => AST.OpenExp env' aenv' a -> [(TagR a, AST.OpenExp env' aenv' b)] -> AST.OpenExp env' aenv' b
        nested _ [(_,r)] = r
        nested s rs      =
          let groups = groupBy (eqT `on` fst) rs
              tags   = map (firstT . fst . head) groups
              e      = prjT (fst (head rs)) s
              rhs    = map (nested s . map (over _1 ignore)) groups
          in
          AST.Case e (zip tags rhs) Nothing

        -- Extract the variable representing this particular tag from the
        -- scrutinee. This is safe because we let-bind the argument first.
        prjT :: TagR a -> AST.OpenExp env' aenv' a -> AST.OpenExp env' aenv' TAG
        prjT = fromJust $$ go
          where
            go :: TagR a -> AST.OpenExp env' aenv' a -> Maybe (AST.OpenExp env' aenv' TAG)
            go TagRtag{}        (AST.Pair l _) = Just l
            go (TagRpair ta tb) (AST.Pair l r) =
              case go ta l of
                Just t  -> Just t
                Nothing -> go tb r
            go _ _ = Nothing

        -- Equality up to the first constructor tag encountered
        eqT :: TagR a -> TagR a -> Bool
        eqT a b = snd $ go a b
          where
            go :: TagR a -> TagR a -> (Any, Bool)
            go TagRunit          TagRunit          = no True
            go TagRsingle{}      TagRsingle{}      = no True
            go TagRundef{}       TagRundef{}       = no True
            go (TagRtag v1 _)    (TagRtag v2 _)    = yes (v1 == v2)
            go (TagRpair a1 b1)  (TagRpair a2 b2)  =
              let (Any r, s) = go a1 a2
               in case r of
                    True  -> yes s
                    False -> go b1 b2
            go _ _ = no False

        firstT :: TagR a -> TAG
        firstT = fromJust . go
          where
            go :: TagR a -> Maybe TAG
            go (TagRtag v _)  = Just v
            go (TagRpair a b) =
              case go a of
                Just t  -> Just t
                Nothing -> go b
            go _ = Nothing

        -- Replace the first constructor tag encountered with a regular
        -- scalar tag, so that that tag will be ignored in the recursive
        -- case.
        ignore = snd . go
          where
            go :: TagR a -> (Any, TagR a)
            go TagRunit         = no  $ TagRunit
            go (TagRsingle t)   = no  $ TagRsingle t
            go (TagRundef t)    = no  $ TagRundef t
            go (TagRtag _ a)    = yes $ TagRpair (TagRundef scalarType) a
            go (TagRpair a1 a2) =
              let (Any r, a1') = go a1
               in case r of
                    True  -> yes $ TagRpair a1' a2
                    False -> TagRpair a1' <$> go a2

        yes :: x -> (Any, x)
        yes e = (Any True, e)

        no :: x -> (Any, x)
        no = pure


-- | Convert a unary functions
--
convertSharingFun1
    :: HasCallStack
    => Config
    -> ArrayLayout aenv aenv
    -> [StableSharingAcc]       -- currently bound array sharing-variables
    -> TypeR a
    -> (SmartExp a -> ScopedExp b)
    -> AST.Fun aenv (a -> b)
convertSharingFun1 config alyt aenv tp f
  | DeclareVars lhs _ value <- declareVars tp
  = let
      a               = SmartExp undefined             -- the 'tag' was already embedded in Phase 1
      lyt             = PushLayout EmptyLayout lhs (value weakenId)
      openF           = convertSharingExp config lyt alyt [] aenv (f a)
    in
      Lam lhs (Body openF)

-- | Convert a binary functions
--
convertSharingFun2
    :: HasCallStack
    => Config
    -> ArrayLayout aenv aenv
    -> [StableSharingAcc]       -- currently bound array sharing-variables
    -> TypeR a
    -> TypeR b
    -> (SmartExp a -> SmartExp b -> ScopedExp c)
    -> AST.Fun aenv (a -> b -> c)
convertSharingFun2 config alyt aenv ta tb f
  | DeclareVars lhs1 _  value1 <- declareVars ta
  , DeclareVars lhs2 k2 value2 <- declareVars tb
  = let
      a               = SmartExp undefined
      b               = SmartExp undefined
      lyt1            = PushLayout EmptyLayout lhs1 (value1 k2)
      lyt2            = PushLayout lyt1        lhs2 (value2 weakenId)
      openF           = convertSharingExp config lyt2 alyt [] aenv (f a b)
    in
      Lam lhs1 $ Lam lhs2 $ Body openF

-- | Convert a unary stencil function
--
convertSharingStencilFun1
    :: HasCallStack
    => Config
    -> ArrayLayout aenv aenv
    -> [StableSharingAcc]               -- currently bound array sharing-variables
    -> R.StencilR sh a stencil
    -> (SmartExp stencil -> ScopedExp b)
    -> AST.Fun aenv (stencil -> b)
convertSharingStencilFun1 config alyt aenv sR1 stencil =
  convertSharingFun1 config alyt aenv (R.stencilR sR1) stencil

-- | Convert a binary stencil function
--
convertSharingStencilFun2
    :: HasCallStack
    => Config
    -> ArrayLayout aenv aenv
    -> [StableSharingAcc]               -- currently bound array sharing-variables
    -> R.StencilR sh a stencil1
    -> R.StencilR sh b stencil2
    -> (SmartExp stencil1 -> SmartExp stencil2 -> ScopedExp c)
    -> AST.Fun aenv (stencil1 -> stencil2 -> c)
convertSharingStencilFun2 config alyt aenv sR1 sR2 stencil =
  convertSharingFun2 config alyt aenv (R.stencilR sR1) (R.stencilR sR2) stencil


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
  StableASTName :: StableName (c t) -> StableASTName c

instance Show (StableASTName c) where
  show (StableASTName sn) = show $ hashStableName sn

instance Eq (StableASTName c) where
  StableASTName sn1 == StableASTName sn2 = eqStableName sn1 sn2

instance Hashable (StableASTName c) where
  hashWithSalt s (StableASTName sn) = hashWithSalt s sn

makeStableAST :: c t -> IO (StableName (c t))
makeStableAST e = e `seq` makeStableName e

-- Stable name for an AST node including the height of the AST representing the array computation.
--
data StableNameHeight t = StableNameHeight (StableName t) Int

instance Eq (StableNameHeight t) where
  (StableNameHeight sn1 _) == (StableNameHeight sn2 _) = eqStableName sn1 sn2

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
  = Hash.mutate occMap sa
  $ \case
      Nothing           -> (Just (1,   height),  Nothing)
      Just (n, heightS) -> (Just (n+1, heightS), Just heightS)


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
lookupWithSharingAcc :: OccMap SmartAcc -> StableSharingAcc -> Int
lookupWithSharingAcc oc (StableSharingAcc (StableNameHeight sn _) _)
  = lookupWithASTName oc (StableASTName sn)

-- Look up the occurrence map keyed by scalar expressions using a sharing expression.  If an
-- the key does not exist in the map, return an occurrence count of '1'.
--
lookupWithSharingExp :: OccMap SmartExp -> StableSharingExp -> Int
lookupWithSharingExp oc (StableSharingExp (StableNameHeight sn _) _)
  = lookupWithASTName oc (StableASTName sn)


-- Stable 'SmartAcc' nodes
-- ------------------

-- Stable name for 'SmartAcc' nodes including the height of the AST.
--
type StableAccName t = StableNameHeight (SmartAcc t)

-- Interleave sharing annotations into an array computation AST.  Subtrees can be marked as being
-- represented by variable (binding a shared subtree) using 'AvarSharing' and as being prefixed by
-- a let binding (for a shared subtree) using 'AletSharing'.
--
data SharingAcc acc exp arrs where
  AvarSharing :: StableAccName arrs -> ArraysR arrs             -> SharingAcc acc exp arrs
  AletSharing :: StableSharingAcc -> acc arrs                   -> SharingAcc acc exp arrs
  AccSharing  :: StableAccName arrs -> PreSmartAcc acc exp arrs -> SharingAcc acc exp arrs

instance HasArraysR acc => HasArraysR (SharingAcc acc exp) where
  arraysR (AvarSharing _ repr) = repr
  arraysR (AletSharing _ acc)  = Smart.arraysR acc
  arraysR (AccSharing  _ acc)  = Smart.arraysR acc


-- Array expression with sharing but shared values have not been scoped; i.e. no let bindings. If
-- the expression is rooted in a function, the list contains the tags of the variables bound by the
-- immediate surrounding lambdas.
data UnscopedAcc t = UnscopedAcc [Int] (SharingAcc UnscopedAcc RootExp t)

instance HasArraysR UnscopedAcc where
  arraysR (UnscopedAcc _ acc) = Smart.arraysR acc


-- Array expression with sharing. For expressions rooted in functions the list holds a sorted
-- environment corresponding to the variables bound in the immediate surounding lambdas.
data ScopedAcc t = ScopedAcc [StableSharingAcc] (SharingAcc ScopedAcc ScopedExp t)

instance HasArraysR ScopedAcc where
  arraysR (ScopedAcc _ acc) = Smart.arraysR acc


-- Stable name for an array computation associated with its sharing-annotated version.
--
data StableSharingAcc where
  StableSharingAcc :: StableAccName arrs
                   -> SharingAcc ScopedAcc ScopedExp arrs
                   -> StableSharingAcc

instance Show StableSharingAcc where
  show (StableSharingAcc sn _) = show $ hashStableNameHeight sn

instance Eq StableSharingAcc where
  StableSharingAcc (StableNameHeight sn1 _) _ == StableSharingAcc (StableNameHeight sn2 _) _
    = eqStableName sn1 sn2

higherSSA :: StableSharingAcc -> StableSharingAcc -> Bool
StableSharingAcc sn1 _ `higherSSA` StableSharingAcc sn2 _ = sn1 `higherSNH` sn2

-- Test whether the given stable names matches an array computation with sharing.
--
matchStableAcc :: StableAccName arrs -> StableSharingAcc -> Bool
matchStableAcc (StableNameHeight sn1 _) (StableSharingAcc (StableNameHeight sn2 _) _)
  = eqStableName sn1 sn2

-- Dummy entry for environments to be used for unused variables.
--
{-# NOINLINE noStableAccName #-}
noStableAccName :: StableAccName arrs
noStableAccName = unsafePerformIO $ StableNameHeight <$> makeStableName undefined <*> pure 0

-- Stable 'Exp' nodes
-- ------------------

-- Stable name for 'Exp' nodes including the height of the AST.
--
type StableExpName t = StableNameHeight (SmartExp t)

-- Interleave sharing annotations into a scalar expressions AST in the same manner as 'SharingAcc'
-- do for array computations.
--
data SharingExp acc exp t where
  VarSharing :: StableExpName t -> TypeR t               -> SharingExp acc exp t
  LetSharing :: StableSharingExp -> exp t                -> SharingExp acc exp t
  ExpSharing :: StableExpName t -> PreSmartExp acc exp t -> SharingExp acc exp t

instance HasTypeR exp => HasTypeR (SharingExp acc exp) where
  typeR (VarSharing _ tp)  = tp
  typeR (LetSharing _ exp) = Smart.typeR exp
  typeR (ExpSharing _ exp) = Smart.typeR exp

-- Specifies a scalar expression AST with sharing annotations but no scoping; i.e. no LetSharing
-- constructors. If the expression is rooted in a function, the list contains the tags of the
-- variables bound by the immediate surrounding lambdas.
data UnscopedExp t = UnscopedExp [Int] (SharingExp UnscopedAcc UnscopedExp t)

instance HasTypeR UnscopedExp where
  typeR (UnscopedExp _ exp) = Smart.typeR exp

-- Specifies a scalar expression AST with sharing. For expressions rooted in functions the list
-- holds a sorted environment corresponding to the variables bound in the immediate surounding
-- lambdas.
data ScopedExp t = ScopedExp [StableSharingExp] (SharingExp ScopedAcc ScopedExp t)

instance HasTypeR ScopedExp where
  typeR (ScopedExp _ exp) = Smart.typeR exp

-- Expressions rooted in 'SmartAcc' computations.
--
-- * When counting occurrences, the root of every expression embedded in an 'SmartAcc' is annotated by
--   an occurrence map for that one expression (excluding any subterms that are rooted in embedded
--   'SmartAcc's.)
--
data RootExp t = RootExp (OccMap SmartExp) (UnscopedExp t)

-- Stable name for an expression associated with its sharing-annotated version.
--
data StableSharingExp where
  StableSharingExp :: StableExpName t -> SharingExp ScopedAcc ScopedExp t -> StableSharingExp

instance Show StableSharingExp where
  show (StableSharingExp sn _) = show $ hashStableNameHeight sn

instance Eq StableSharingExp where
  StableSharingExp (StableNameHeight sn1 _) _ == StableSharingExp (StableNameHeight sn2 _) _ =
    eqStableName sn1 sn2

higherSSE :: StableSharingExp -> StableSharingExp -> Bool
StableSharingExp sn1 _ `higherSSE` StableSharingExp sn2 _ = sn1 `higherSNH` sn2

-- Test whether the given stable names matches an expression with sharing.
--
matchStableExp :: StableExpName t -> StableSharingExp -> Bool
matchStableExp (StableNameHeight sn1 _) (StableSharingExp (StableNameHeight sn2 _) _) = eqStableName sn1 sn2

-- Dummy entry for environments to be used for unused variables.
--
{-# NOINLINE noStableExpName #-}
noStableExpName :: StableExpName t
noStableExpName = unsafePerformIO $ StableNameHeight <$> makeStableName undefined <*> pure 0


{--
-- Stable 'Seq' nodes
-- ------------------

-- Stable name for 'Seq' nodes including the height of the AST.
--
type StableSeqName arrs = StableNameHeight (Seq arrs)

-- Interleave sharing annotations into an sequence computation AST in the same manner as SharingAcc
-- and SharingExp
--
data SharingSeq acc seq exp arrs where
  SvarSharing :: (Typeable arrs, Arrays arrs)
              => StableSeqName [arrs]                       -> SharingSeq acc seq exp [arrs]
  SletSharing :: StableSharingSeq -> seq t                  -> SharingSeq acc seq exp t
  SeqSharing  :: Typeable arrs
              => StableSeqName arrs -> PreSeq acc seq exp arrs -> SharingSeq acc seq exp arrs

-- Array expression with sharing but shared values have not been scoped; i.e. no let bindings. If
-- the expression is rooted in a function, the list contains the tags of the variables bound by the
-- immediate surrounding lambdas.
data UnscopedSeq t = UnscopedSeq (SharingSeq UnscopedAcc UnscopedSeq RootExp t)

-- Array expression with sharing. For expressions rooted in functions the list holds a sorted
-- environment corresponding to the variables bound in the immediate surounding lambdas.
data ScopedSeq t = ScopedSeq (SharingSeq ScopedAcc ScopedSeq ScopedExp t)

-- Sequences rooted in 'Acc' computations.
--
-- * When counting occurrences, the root of every sequence embedded in an 'Acc' is annotated by
--   an occurrence map for that one expression (excluding any subterms that are rooted in embedded
--   'Acc's.)
--
data RootSeq t = RootSeq (OccMap Seq) (UnscopedSeq t)

-- Stable name for an array computation associated with its sharing-annotated version.
--
data StableSharingSeq where
  StableSharingSeq :: Typeable arrs
                   => StableSeqName arrs
                   -> SharingSeq ScopedAcc ScopedSeq ScopedExp arrs
                   -> StableSharingSeq

instance Show StableSharingSeq where
  show (StableSharingSeq sn _) = show $ hashStableNameHeight sn

instance Eq StableSharingSeq where
  StableSharingSeq sn1 _ == StableSharingSeq sn2 _
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

higherSSS :: StableSharingSeq -> StableSharingSeq -> Bool
StableSharingSeq sn1 _ `higherSSS` StableSharingSeq sn2 _ = sn1 `higherSNH` sn2

-- Test whether the given stable names matches an array computation with sharing.
--
matchStableSeq :: Typeable arrs => StableSeqName arrs -> StableSharingSeq -> Bool
matchStableSeq sn1 (StableSharingSeq sn2 _)
  | Just sn1' <- gcast sn1 = sn1' == sn2
  | otherwise              = False
--}


-- Occurrence counting
-- ===================

-- Compute the 'SmartAcc' occurrence map, marks all nodes (both 'Seq' and 'Exp' nodes) with stable names,
-- and drop repeated occurrences of shared 'SmartAcc' and 'Exp' subtrees (Phase One).
--
-- We compute a single 'SmartAcc' occurrence map for the whole AST, but one 'Exp' occurrence map for each
-- sub-expression rooted in an 'SmartAcc' operation.  This is as we cannot float 'Exp' subtrees across
-- 'SmartAcc' operations, but we can float 'SmartAcc' subtrees out of 'Exp' expressions.
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
    :: HasCallStack
    => Config
    -> Level
    -> SmartAcc arrs
    -> IO (UnscopedAcc arrs, OccMap SmartAcc)
makeOccMapAcc config lvl acc = do
  traceLine "makeOccMapAcc" "Enter"
  accOccMap             <- newASTHashTable
  (acc', _)             <- makeOccMapSharingAcc config accOccMap lvl acc
  frozenAccOccMap       <- freezeOccMap accOccMap
  traceLine "makeOccMapAcc" "Exit"
  return (acc', frozenAccOccMap)


makeOccMapSharingAcc
    :: HasCallStack
    => Config
    -> OccMapHash SmartAcc
    -> Level
    -> SmartAcc arrs
    -> IO (UnscopedAcc arrs, Int)
makeOccMapSharingAcc config accOccMap = traverseAcc
  where
    traverseFun1
        :: HasCallStack
        => Level
        -> TypeR a
        -> (SmartExp a -> SmartExp b)
        -> IO (SmartExp a -> RootExp b, Int)
    traverseFun1 = makeOccMapFun1 config accOccMap

    traverseFun2
        :: HasCallStack
        => Level
        -> TypeR a
        -> TypeR b
        -> (SmartExp a -> SmartExp b -> SmartExp c)
        -> IO (SmartExp a -> SmartExp b -> RootExp c, Int)
    traverseFun2 = makeOccMapFun2 config accOccMap

    traverseAfun1
        :: HasCallStack
        => Level
        -> ArraysR a
        -> (SmartAcc a -> SmartAcc b)
        -> IO (SmartAcc a -> UnscopedAcc b, Int)
    traverseAfun1 = makeOccMapAfun1 config accOccMap

    traverseExp
      :: HasCallStack
      => Level
      -> SmartExp e
      -> IO (RootExp e, Int)
    traverseExp = makeOccMapExp config accOccMap

    traverseBoundary
        :: HasCallStack
        => Level
        -> ShapeR sh
        -> PreBoundary SmartAcc SmartExp (Array sh e)
        -> IO (PreBoundary UnscopedAcc RootExp (Array sh e), Int)
    traverseBoundary lvl shr bndy =
      case bndy of
        Clamp      -> return (Clamp, 0)
        Mirror     -> return (Mirror, 0)
        Wrap       -> return (Wrap, 0)
        Constant v -> return (Constant v, 0)
        Function f -> do
          (f', h) <- traverseFun1 lvl (shapeType shr) f
          return (Function f', h)

    -- traverseSeq :: forall arrs. Typeable arrs
    --             => Level -> Seq arrs
    --             -> IO (RootSeq arrs, Int)
    -- traverseSeq = makeOccMapRootSeq config accOccMap

    traverseAcc
        :: forall arrs. HasCallStack
        => Level
        -> SmartAcc arrs
        -> IO (UnscopedAcc arrs, Int)
    traverseAcc lvl acc@(SmartAcc pacc)
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
          let reconstruct :: IO (PreSmartAcc UnscopedAcc RootExp arrs, Int)
                          -> IO (UnscopedAcc arrs, Int)
              reconstruct newAcc
                = case heightIfRepeatedOccurrence of
                    Just height | acc_sharing `member` options config
                      -> return (UnscopedAcc [] (AvarSharing (StableNameHeight sn height) (Smart.arraysR pacc)), height)
                    _ -> do (acc, height) <- newAcc
                            return (UnscopedAcc [] (AccSharing (StableNameHeight sn height) acc), height)

          reconstruct $ case pacc of
            Atag repr i                 -> return (Atag repr i, 0)           -- height is 0!
            Pipe repr1 repr2 repr3 afun1 afun2 acc
                                        -> do
                                             (afun1', h1) <- traverseAfun1 lvl repr1 afun1
                                             (afun2', h2) <- traverseAfun1 lvl repr2 afun2
                                             (acc', h3)   <- traverseAcc lvl acc
                                             return (Pipe repr1 repr2 repr3 afun1' afun2' acc'
                                                    , h1 `max` h2 `max` h3 + 1)
            Aforeign repr ff afun acc   -> travA (Aforeign repr ff afun) acc
            Acond e acc1 acc2           -> do
                                             (e'   , h1) <- traverseExp lvl e
                                             (acc1', h2) <- traverseAcc lvl acc1
                                             (acc2', h3) <- traverseAcc lvl acc2
                                             return (Acond e' acc1' acc2', h1 `max` h2 `max` h3 + 1)
            Awhile repr pred iter init  -> do
                                             (pred', h1) <- traverseAfun1 lvl repr pred
                                             (iter', h2) <- traverseAfun1 lvl repr iter
                                             (init', h3) <- traverseAcc lvl init
                                             return (Awhile repr pred' iter' init'
                                                    , h1 `max` h2 `max` h3 + 1)

            Anil                        -> return (Anil, 0)
            Apair acc1 acc2             -> do
                                             (a', h1) <- traverseAcc lvl acc1
                                             (b', h2) <- traverseAcc lvl acc2
                                             return (Apair a' b', h1 `max` h2 + 1)
            Aprj ix a                   -> travA (Aprj ix) a

            Atrace msg acc1 acc2        -> do
                                             (a', h1) <- traverseAcc lvl acc1
                                             (b', h2) <- traverseAcc lvl acc2
                                             return (Atrace msg a' b', h1 `max` h2 + 1)
            Aerror repr msg acc1        -> do
                                             (a', h1) <- traverseAcc lvl acc1
                                             return (Aerror repr msg a', h1 + 1)
            Use repr arr                -> return (Use repr arr, 1)
            Unit tp e                   -> do
                                             (e', h) <- traverseExp lvl e
                                             return (Unit tp e', h + 1)
            Generate repr@(ArrayR shr _) e f
                                        -> do
                                             (e', h1) <- traverseExp lvl e
                                             (f', h2) <- traverseFun1 lvl (shapeType shr) f
                                             return (Generate repr e' f', h1 `max` h2 + 1)
            Reshape shr e acc           -> travEA (Reshape shr) e acc
            Replicate si e acc          -> travEA (Replicate si) e acc
            Slice si acc e              -> travEA (flip $ Slice si) e acc
            Map t1 t2 f acc             -> do
                                             (f'  , h1) <- traverseFun1 lvl t1 f
                                             (acc', h2) <- traverseAcc lvl acc
                                             return (Map t1 t2 f' acc', h1 `max` h2 + 1)
            ZipWith t1 t2 t3 f acc1 acc2
                                        -> travF2A2 (ZipWith t1 t2 t3) t1 t2 f acc1 acc2
            Fold tp f e acc             -> travF2MEA (Fold tp) tp tp f e acc
            FoldSeg i tp f e acc1 acc2  -> do
                                             (f'   , h1) <- traverseFun2 lvl tp tp f
                                             (e'   , h2) <- travME e
                                             (acc1', h3) <- traverseAcc lvl acc1
                                             (acc2', h4) <- traverseAcc lvl acc2
                                             return (FoldSeg i tp f' e' acc1' acc2',
                                                     h1 `max` h2 `max` h3 `max` h4 + 1)
            Scan  d tp f e acc          -> travF2MEA (Scan  d tp) tp tp f e acc
            Scan' d tp f e acc          -> travF2EA (Scan' d tp) tp tp f e acc
            Permute repr@(ArrayR shr tp) c acc1 p acc2
                                        -> do
                                             (c'   , h1) <- traverseFun2 lvl tp tp c
                                             (p'   , h2) <- traverseFun1 lvl (shapeType shr) p
                                             (acc1', h3) <- traverseAcc lvl acc1
                                             (acc2', h4) <- traverseAcc lvl acc2
                                             return (Permute repr c' acc1' p' acc2',
                                                     h1 `max` h2 `max` h3 `max` h4 + 1)
            Backpermute shr e p acc     -> do
                                             (e'  , h1) <- traverseExp lvl e
                                             (p'  , h2) <- traverseFun1 lvl (shapeType shr) p
                                             (acc', h3) <- traverseAcc lvl acc
                                             return (Backpermute shr e' p' acc', h1 `max` h2 `max` h3 + 1)
            Stencil s tp f bnd acc      -> do
                                             (f'  , h1) <- makeOccMapStencil1 config accOccMap s lvl f
                                             (bnd', h2) <- traverseBoundary lvl (stencilShapeR s) bnd
                                             (acc', h3) <- traverseAcc lvl acc
                                             return (Stencil s tp f' bnd' acc', h1 `max` h2 `max` h3 + 1)
            Stencil2 s1 s2 tp f bnd1 acc1
                              bnd2 acc2 -> do
                                             let shr = stencilShapeR s1
                                             (f'   , h1) <- makeOccMapStencil2 config accOccMap s1 s2 lvl f
                                             (bnd1', h2) <- traverseBoundary lvl shr bnd1
                                             (acc1', h3) <- traverseAcc lvl acc1
                                             (bnd2', h4) <- traverseBoundary lvl shr bnd2
                                             (acc2', h5) <- traverseAcc lvl acc2
                                             return (Stencil2 s1 s2 tp f' bnd1' acc1' bnd2' acc2',
                                                     h1 `max` h2 `max` h3 `max` h4 `max` h5 + 1)
            -- Collect s                   -> do
            --                                  (s', h) <- traverseSeq lvl s
            --                                  return (Collect s', h + 1)


      where
        travA :: HasCallStack
              => (UnscopedAcc arrs' -> PreSmartAcc UnscopedAcc RootExp arrs)
              -> SmartAcc arrs'
              -> IO (PreSmartAcc UnscopedAcc RootExp arrs, Int)
        travA c acc
          = do
              (acc', h) <- traverseAcc lvl acc
              return (c acc', h + 1)

        travEA :: HasCallStack
               => (RootExp b -> UnscopedAcc arrs' -> PreSmartAcc UnscopedAcc RootExp arrs)
               -> SmartExp b
               -> SmartAcc arrs'
               -> IO (PreSmartAcc UnscopedAcc RootExp arrs, Int)
        travEA c exp acc
          = do
              (exp', h1) <- traverseExp lvl exp
              (acc', h2) <- traverseAcc lvl acc
              return (c exp' acc', h1 `max` h2 + 1)

        travF2EA
            :: HasCallStack
            => ((SmartExp b -> SmartExp c -> RootExp d) -> RootExp e -> UnscopedAcc arrs' -> PreSmartAcc UnscopedAcc RootExp arrs)
            -> TypeR b
            -> TypeR c
            -> (SmartExp b -> SmartExp c -> SmartExp d)
            -> SmartExp e
            -> SmartAcc arrs'
            -> IO (PreSmartAcc UnscopedAcc RootExp arrs, Int)
        travF2EA c t1 t2 fun exp acc
          = do
              (fun', h1) <- traverseFun2 lvl t1 t2 fun
              (exp', h2) <- traverseExp lvl exp
              (acc', h3) <- traverseAcc lvl acc
              return (c fun' exp' acc', h1 `max` h2 `max` h3 + 1)

        travF2MEA
            :: HasCallStack
            => ((SmartExp b -> SmartExp c -> RootExp d) -> Maybe (RootExp e) -> UnscopedAcc arrs' -> PreSmartAcc UnscopedAcc RootExp arrs)
            -> TypeR b
            -> TypeR c
            -> (SmartExp b -> SmartExp c -> SmartExp d)
            -> Maybe (SmartExp e)
            -> SmartAcc arrs'
            -> IO (PreSmartAcc UnscopedAcc RootExp arrs, Int)
        travF2MEA c t1 t2 fun exp acc
          = do
              (fun', h1) <- traverseFun2 lvl t1 t2 fun
              (exp', h2) <- travME exp
              (acc', h3) <- traverseAcc lvl acc
              return (c fun' exp' acc', h1 `max` h2 `max` h3 + 1)

        travME :: HasCallStack => Maybe (SmartExp t) -> IO (Maybe (RootExp t), Int)
        travME Nothing  = return (Nothing, 0)
        travME (Just e) = do
          (e', c) <- traverseExp lvl e
          return (Just e', c)

        travF2A2
            :: HasCallStack
            => ((SmartExp b -> SmartExp c -> RootExp d) -> UnscopedAcc arrs1 -> UnscopedAcc arrs2 -> PreSmartAcc UnscopedAcc RootExp arrs)
            -> TypeR b
            -> TypeR c
            -> (SmartExp b -> SmartExp c -> SmartExp d)
            -> SmartAcc arrs1
            -> SmartAcc arrs2
            -> IO (PreSmartAcc UnscopedAcc RootExp arrs, Int)
        travF2A2 c t1 t2 fun acc1 acc2
          = do
              (fun' , h1) <- traverseFun2 lvl t1 t2 fun
              (acc1', h2) <- traverseAcc lvl acc1
              (acc2', h3) <- traverseAcc lvl acc2
              return (c fun' acc1' acc2', h1 `max` h2 `max` h3 + 1)

makeOccMapAfun1
    :: HasCallStack
    => Config
    -> OccMapHash SmartAcc
    -> Level
    -> ArraysR a
    -> (SmartAcc a -> SmartAcc b)
    -> IO (SmartAcc a -> UnscopedAcc b, Int)
makeOccMapAfun1 config accOccMap lvl repr f = do
  let x = SmartAcc (Atag repr lvl)
  --
  (UnscopedAcc [] body, height) <- makeOccMapSharingAcc config accOccMap (lvl+1) (f x)
  return (const (UnscopedAcc [lvl] body), height)

{--
makeOccMapAfun2 :: (Arrays a, Arrays b, Typeable c)
                => Config
                -> OccMapHash Acc
                -> Level
                -> (Acc a -> Acc b -> Acc c)
                -> IO (Acc a -> Acc b -> UnscopedAcc c, Int)
makeOccMapAfun2 config accOccMap lvl f = do
  let x = Acc (Atag (lvl + 1))
      y = Acc (Atag (lvl + 0))
  --
  (UnscopedAcc [] body, height) <- makeOccMapSharingAcc config accOccMap (lvl+2) (f x y)
  return (\ _ _ -> (UnscopedAcc [lvl, lvl+1] body), height)

makeOccMapAfun3 :: (Arrays a, Arrays b, Arrays c, Typeable d)
                => Config
                -> OccMapHash Acc
                -> Level
                -> (Acc a -> Acc b -> Acc c -> Acc d)
                -> IO (Acc a -> Acc b -> Acc c -> UnscopedAcc d, Int)
makeOccMapAfun3 config accOccMap lvl f = do
  let x = Acc (Atag (lvl + 2))
      y = Acc (Atag (lvl + 1))
      z = Acc (Atag (lvl + 0))
  --
  (UnscopedAcc [] body, height) <- makeOccMapSharingAcc config accOccMap (lvl+3) (f x y z)
  return (\ _ _ _ -> (UnscopedAcc [lvl, lvl+1, lvl+2] body), height)
--}

-- Generate occupancy information for scalar functions and expressions. Helper
-- functions wrapping around 'makeOccMapRootExp' with more specific types.
--
-- See Note [Traversing functions and side effects]
--
makeOccMapExp
    :: HasCallStack
    => Config
    -> OccMapHash SmartAcc
    -> Level
    -> SmartExp e
    -> IO (RootExp e, Int)
makeOccMapExp config accOccMap lvl = makeOccMapRootExp config accOccMap lvl []

makeOccMapFun1
    :: HasCallStack
    => Config
    -> OccMapHash SmartAcc
    -> Level
    -> TypeR a
    -> (SmartExp a -> SmartExp b)
    -> IO (SmartExp a -> RootExp b, Int)
makeOccMapFun1 config accOccMap lvl tp f = do
  let x = SmartExp (Tag tp lvl)
  --
  (body, height) <- makeOccMapRootExp config accOccMap (lvl+1) [lvl] (f x)
  return (const body, height)

makeOccMapFun2
    :: HasCallStack
    => Config
    -> OccMapHash SmartAcc
    -> Level
    -> TypeR a
    -> TypeR b
    -> (SmartExp a -> SmartExp b -> SmartExp c)
    -> IO (SmartExp a -> SmartExp b -> RootExp c, Int)
makeOccMapFun2 config accOccMap lvl t1 t2 f = do
  let x = SmartExp (Tag t1 (lvl+1))
      y = SmartExp (Tag t2 lvl)
  --
  (body, height) <- makeOccMapRootExp config accOccMap (lvl+2) [lvl, lvl+1] (f x y)
  return (\_ _ -> body, height)

makeOccMapStencil1
    :: forall sh a b stencil. HasCallStack
    => Config
    -> OccMapHash SmartAcc
    -> R.StencilR sh a stencil
    -> Level
    -> (SmartExp stencil -> SmartExp b)
    -> IO (SmartExp stencil -> RootExp b, Int)
makeOccMapStencil1 config accOccMap s lvl stencil = do
  let x = SmartExp (Tag (R.stencilR s) lvl)
  --
  (body, height) <- makeOccMapRootExp config accOccMap (lvl+1) [lvl] (stencil x)
  return (const body, height)

makeOccMapStencil2
    :: forall sh a b c stencil1 stencil2. HasCallStack
    => Config
    -> OccMapHash SmartAcc
    -> R.StencilR sh a stencil1
    -> R.StencilR sh b stencil2
    -> Level
    -> (SmartExp stencil1 -> SmartExp stencil2 -> SmartExp c)
    -> IO (SmartExp stencil1 -> SmartExp stencil2 -> RootExp c, Int)
makeOccMapStencil2 config accOccMap sR1 sR2 lvl stencil = do
  let x = SmartExp (Tag (R.stencilR sR1) (lvl+1))
      y = SmartExp (Tag (R.stencilR sR2) lvl)
  --
  (body, height) <- makeOccMapRootExp config accOccMap (lvl+2) [lvl, lvl+1] (stencil x y)
  return (\_ _ -> body, height)


-- Generate sharing information for expressions embedded in Acc computations.
-- Expressions are annotated with:
--
--  1) the tags of free scalar variables (for scalar functions)
--  2) a local occurrence map for that expression.
--
makeOccMapRootExp
    :: HasCallStack
    => Config
    -> OccMapHash SmartAcc
    -> Level                            -- The level of currently bound scalar variables
    -> [Int]                            -- The tags of newly introduced free scalar variables in this expression
    -> SmartExp e
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
    :: HasCallStack
    => Config
    -> OccMapHash SmartAcc
    -> OccMapHash SmartExp
    -> Level                            -- The level of currently bound variables
    -> SmartExp e
    -> IO (UnscopedExp e, Int)
makeOccMapSharingExp config accOccMap expOccMap = travE
  where
    travE :: forall a. HasCallStack => Level -> SmartExp a -> IO (UnscopedExp a, Int)
    travE lvl exp@(SmartExp pexp)
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
          let reconstruct :: IO (PreSmartExp UnscopedAcc UnscopedExp a, Int)
                          -> IO (UnscopedExp a, Int)
              reconstruct newExp
                = case heightIfRepeatedOccurrence of
                    Just height | exp_sharing `member` options config
                      -> return (UnscopedExp [] (VarSharing (StableNameHeight sn height) (typeR pexp)), height)
                    _ -> do (exp, height) <- newExp
                            return (UnscopedExp [] (ExpSharing (StableNameHeight sn height) exp), height)

          reconstruct $ case pexp of
            Tag tp i            -> return (Tag tp i, 0)      -- height is 0!
            Const tp c          -> return (Const tp c, 1)
            Undef tp            -> return (Undef tp, 1)
            Nil                 -> return (Nil, 1)
            Pair e1 e2          -> travE2 Pair e1 e2
            Prj i e             -> travE1 (Prj i) e
            VecPack   vec e     -> travE1 (VecPack   vec) e
            VecUnpack vec e     -> travE1 (VecUnpack vec) e
            ToIndex shr sh ix   -> travE2 (ToIndex shr) sh ix
            FromIndex shr sh e  -> travE2 (FromIndex shr) sh e
            Match t e           -> travE1 (Match t) e
            Case e rhs          -> do
                                     (e',   h1) <- travE lvl e
                                     (rhs', h2) <- unzip <$> sequence [ travE1 (t,) c | (t,c) <- rhs ]
                                     return (Case e' rhs', h1 `max` maximum h2 + 1)
            Cond e1 e2 e3       -> travE3 Cond e1 e2 e3
            While t p iter init -> do
                                     (p'   , h1) <- traverseFun1 lvl t p
                                     (iter', h2) <- traverseFun1 lvl t iter
                                     (init', h3) <- travE lvl init
                                     return (While t p' iter' init', h1 `max` h2 `max` h3 + 1)
            PrimConst c         -> return (PrimConst c, 1)
            PrimApp p e         -> travE1 (PrimApp p) e
            Index tp a e        -> travAE (Index tp) a e
            LinearIndex tp a i  -> travAE (LinearIndex tp) a i
            Shape shr a         -> travA (Shape shr) a
            ShapeSize shr e     -> travE1 (ShapeSize shr) e
            Foreign tp ff f e   -> do
                                      (e', h) <- travE lvl e
                                      return  (Foreign tp ff f e', h+1)
            Coerce t1 t2 e      -> travE1 (Coerce t1 t2) e

      where
        traverseAcc :: HasCallStack => Level -> SmartAcc arrs -> IO (UnscopedAcc arrs, Int)
        traverseAcc = makeOccMapSharingAcc config accOccMap

        traverseFun1
            :: HasCallStack
            => Level
            -> TypeR a
            -> (SmartExp a -> SmartExp b)
            -> IO (SmartExp a -> UnscopedExp b, Int)
        traverseFun1 lvl tp f
          = do
              let x = SmartExp (Tag tp lvl)
              (UnscopedExp [] body, height) <- travE (lvl+1) (f x)
              return (const (UnscopedExp [lvl] body), height + 1)


        travE1 :: HasCallStack => (UnscopedExp b -> r) -> SmartExp b -> IO (r, Int)
        travE1 c e
          = do
              (e', h) <- travE lvl e
              return (c e', h + 1)

        travE2 :: HasCallStack
               => (UnscopedExp b -> UnscopedExp c -> r)
               -> SmartExp b
               -> SmartExp c
               -> IO (r, Int)
        travE2 c e1 e2
          = do
              (e1', h1) <- travE lvl e1
              (e2', h2) <- travE lvl e2
              return (c e1' e2', h1 `max` h2 + 1)

        travE3 :: HasCallStack
               => (UnscopedExp b -> UnscopedExp c -> UnscopedExp d -> r)
               -> SmartExp b
               -> SmartExp c
               -> SmartExp d
               -> IO (r, Int)
        travE3 c e1 e2 e3
          = do
              (e1', h1) <- travE lvl e1
              (e2', h2) <- travE lvl e2
              (e3', h3) <- travE lvl e3
              return (c e1' e2' e3', h1 `max` h2 `max` h3 + 1)

        travA :: HasCallStack => (UnscopedAcc b -> r) -> SmartAcc b -> IO (r, Int)
        travA c acc
          = do
              (acc', h) <- traverseAcc lvl acc
              return (c acc', h + 1)

        travAE :: HasCallStack
               => (UnscopedAcc b -> UnscopedExp c -> r)
               -> SmartAcc b
               -> SmartExp c
               -> IO (r, Int)
        travAE c acc e
          = do
              (acc', h1) <- traverseAcc lvl acc
              (e'  , h2) <- travE lvl e
              return (c acc' e', h1 `max` h2 + 1)

{--
makeOccMapRootSeq
    :: Typeable arrs
    => Config
    -> OccMapHash Acc
    -> Level
    -> Seq arrs
    -> IO (RootSeq arrs, Int)
makeOccMapRootSeq config accOccMap lvl seq = do
  traceLine "makeOccMapRootSeq" "Enter"
  seqOccMap       <- newASTHashTable
  (seq', height)  <- makeOccMapSharingSeq config accOccMap seqOccMap lvl seq
  frozenSeqOccMap <- freezeOccMap seqOccMap
  traceLine "makeOccMapRootSeq" "Exit"
  return (RootSeq frozenSeqOccMap seq', height)

-- Generate sharing information for an open sequence expression.
--
makeOccMapSharingSeq
    :: Typeable e
    => Config
    -> OccMapHash Acc
    -> OccMapHash Seq
    -> Level                            -- The level of currently bound variables
    -> Seq e
    -> IO (UnscopedSeq e, Int)
makeOccMapSharingSeq config accOccMap seqOccMap = traverseSeq
  where
    traverseAcc :: Typeable arrs => Level -> Acc arrs -> IO (UnscopedAcc arrs, Int)
    traverseAcc = makeOccMapSharingAcc config accOccMap

    traverseAfun1 :: (Arrays a, Typeable b) => Level -> (Acc a -> Acc b) -> IO (Acc a -> UnscopedAcc b, Int)
    traverseAfun1 = makeOccMapAfun1 config accOccMap

    traverseAfun2 :: (Arrays a, Arrays b, Typeable c) => Level -> (Acc a -> Acc b -> Acc c) -> IO (Acc a -> Acc b -> UnscopedAcc c, Int)
    traverseAfun2 = makeOccMapAfun2 config accOccMap

    traverseAfun3 :: (Arrays a, Arrays b, Arrays c, Typeable d) => Level -> (Acc a -> Acc b -> Acc c -> Acc d) -> IO (Acc a -> Acc b -> Acc c -> UnscopedAcc d, Int)
    traverseAfun3 = makeOccMapAfun3 config accOccMap

    traverseExp :: Typeable e => Level -> Exp e -> IO (RootExp e, Int)
    traverseExp = makeOccMapExp config accOccMap

    traverseFun2 :: (Elt a, Elt b, Typeable c)
                 => Level
                 -> (Exp a -> Exp b -> Exp c)
                 -> IO (Exp a -> Exp b -> RootExp c, Int)
    traverseFun2 = makeOccMapFun2 config accOccMap

    traverseTup :: Level -> Atuple Seq tup -> IO (Atuple UnscopedSeq tup, Int)
    traverseTup _   NilAtup          = return (NilAtup, 1)
    traverseTup lvl (SnocAtup tup s) = do
                                        (tup', h1) <- traverseTup lvl tup
                                        (s'  , h2) <- traverseSeq lvl s
                                        return (SnocAtup tup' s', h1 `max` h2 + 1)

    traverseSeq :: forall arrs. Typeable arrs => Level -> Seq arrs -> IO (UnscopedSeq arrs, Int)
    traverseSeq lvl acc@(Seq seq)
      = mfix $ \ ~(_, height) -> do
          -- Compute stable name and enter it into the occurrence map
          --
          sn                         <- makeStableAST acc
          heightIfRepeatedOccurrence <- enterOcc seqOccMap (StableASTName sn) height

          traceLine (showPreSeqOp seq) $ do
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
          let producer :: (arrs ~ [a], Arrays a)
                       => IO (PreSeq UnscopedAcc UnscopedSeq RootExp arrs, Int)
                       -> IO (UnscopedSeq arrs, Int)
              producer newSeq
                = case heightIfRepeatedOccurrence of
                    Just height | recoverSeqSharing config
                      -> return (UnscopedSeq (SvarSharing (StableNameHeight sn height)), height)
                    _ -> do (seq, height) <- newSeq
                            return (UnscopedSeq (SeqSharing (StableNameHeight sn height) seq), height)

          let consumer :: IO (PreSeq UnscopedAcc UnscopedSeq RootExp arrs, Int)
                       -> IO (UnscopedSeq arrs, Int)
              consumer newSeq
                = do (seq, height) <- newSeq
                     return (UnscopedSeq (SeqSharing (StableNameHeight sn height) seq), height)

          case seq of
            StreamIn arrs -> producer $ return (StreamIn arrs, 1)
            ToSeq sl acc -> producer $ do
              (acc', h1) <- traverseAcc lvl acc
              return (ToSeq sl acc', h1 + 1)
            MapSeq afun s -> producer $ do
              (afun', h1) <- traverseAfun1 lvl afun
              (s'   , h2) <- traverseSeq lvl s
              return (MapSeq afun' s', h1 `max` h2 + 1)
            ZipWithSeq afun s1 s2 -> producer $ do
              (afun', h1) <- traverseAfun2 lvl afun
              (s1'  , h2) <- traverseSeq lvl s1
              (s2'  , h3) <- traverseSeq lvl s2
              return (ZipWithSeq afun' s1' s2', h1 `max` h2 `max` h3 + 1)
            ScanSeq fun e s -> producer $ do
              (fun', h1) <- traverseFun2 lvl fun
              (e',  h2) <- traverseExp lvl e
              (s'   , h3) <- traverseSeq lvl s
              return (ScanSeq fun' e' s', h1 `max` h2 `max` h3 + 1)
            FoldSeq fun e s -> consumer $ do
              (fun', h1) <- traverseFun2 lvl fun
              (e'  , h2) <- traverseExp lvl e
              (s'  , h3) <- traverseSeq lvl s
              return (FoldSeq fun' e' s', h1 `max` h2 `max` h3 + 1)
            FoldSeqFlatten afun acc s -> consumer $ do
              (afun', h1) <- traverseAfun3 lvl afun
              (acc',  h2) <- traverseAcc lvl acc
              (s'   , h3) <- traverseSeq lvl s
              return (FoldSeqFlatten afun' acc' s', h1 `max` h2 `max` h3 + 1)
            Stuple t -> consumer $ do
              (t', h1) <- traverseTup lvl t
              return (Stuple t', h1 + 1)
--}


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
--   - All 'Exp' node counts precede all 'SmartAcc' node counts as we don't share 'Exp' nodes across 'SmartAcc'
--     nodes. Similarly, all 'Seq' nodes precede 'SmartAcc' nodes and 'Exp' nodes precede 'Seq' nodes.
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
  NodeName :: StableName a -> NodeName

instance Eq NodeName where
  (NodeName sn1) == (NodeName sn2) = eqStableName sn1 sn2

instance Hashable NodeName where
  hashWithSalt hash (NodeName sn1) = hash + hashStableName sn1

instance Show NodeName where
  show (NodeName sn) = show (hashStableName sn)

data NodeCount = AccNodeCount StableSharingAcc Int
               | ExpNodeCount StableSharingExp Int
               -- SeqNodeCount StableSharingSeq Int
               deriving Show

-- Empty node counts
--
noNodeCounts :: NodeCounts
noNodeCounts = ([], Map.empty)

-- Insert an Acc node into the node counts, assuming that it is a superterm of the all the existing
-- nodes.
--
-- TODO: Perform cycle detection here.
--
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
--
insertExpNode :: StableSharingExp -> NodeCounts -> NodeCounts
insertExpNode ssa@(StableSharingExp (StableNameHeight sn _) _) (subterms,g)
  = ([ExpNodeCount ssa 1], g') +++ (subterms,g)
  where
    k  = NodeName sn
    hs = map nodeName subterms
    g' = Map.fromList $ (k, Set.empty) : [(h, Set.singleton k) | h <- hs]

{--
-- Insert an Seq node into the node counts, assuming that it is a superterm of the all the existing
-- nodes.
--
-- TODO: Perform cycle detection here.
--
insertSeqNode :: StableSharingSeq -> NodeCounts -> NodeCounts
insertSeqNode ssa@(StableSharingSeq (StableNameHeight sn _) _) (subterms,g)
  = ([SeqNodeCount ssa 1], g') +++ (subterms,g)
  where
    k  = NodeName sn
    hs = map nodeName subterms
    g' = Map.fromList $ (k, Set.empty) : [(h, Set.singleton k) | h <- hs]
--}

-- Remove nodes that aren't in the list from the graph.
--
-- RCE: This is no longer necessary when NDP is supported.
--
cleanCounts :: NodeCounts -> NodeCounts
cleanCounts (ns, g) = (ns, Map.fromList [(h, Set.filter (flip elem hs) (g Map.! h)) | h <- hs ])
  where
    hs = map nodeName ns

nodeName :: NodeCount -> NodeName
nodeName (AccNodeCount (StableSharingAcc (StableNameHeight sn _) _) _) = NodeName sn
nodeName (ExpNodeCount (StableSharingExp (StableNameHeight sn _) _) _) = NodeName sn
-- nodeName (SeqNodeCount (StableSharingSeq (StableNameHeight sn _) _) _) = NodeName sn


-- Combine node counts that belong to the same node.
--
-- * We assume that the list invariant —subterms follow their parents— holds for both arguments and
--   guarantee that it still holds for the result.
--
-- * In the same manner, we assume that all 'Exp' node counts precede 'SmartAcc' node counts and
--   guarantee that this also hold for the result.
--
(+++) :: NodeCounts -> NodeCounts -> NodeCounts
(ns1, g1) +++ (ns2, g2) = (cleanup $ merge ns1 ns2, Map.unionWith Set.union g1 g2)
  where
    merge [] x = x
    merge x [] = x
    merge (x@(AccNodeCount sa1 count1):xs) (y@(AccNodeCount sa2 count2):ys)
     | sa1 == sa2          = AccNodeCount (sa1 `pickNoneAvar` sa2) (count1 + count2) : merge xs ys
     | sa1 `higherSSA` sa2 = x : merge xs (y:ys)
     | otherwise           = y : merge (x:xs) ys
    merge (x@(ExpNodeCount se1 count1):xs) (y@(ExpNodeCount se2 count2):ys)
     | se1 == se2          = ExpNodeCount (se1 `pickNoneVar` se2) (count1 + count2) : merge xs ys
     | se1 `higherSSE` se2 = x : merge xs (y:ys)
     | otherwise           = y : merge (x:xs) ys
    merge (x@(AccNodeCount _ _):xs) (y@(ExpNodeCount _ _):ys) = y : merge (x:xs) ys
    merge (x@(ExpNodeCount _ _):xs) (y@(AccNodeCount _ _):ys) = x : merge xs (y:ys)

    (StableSharingAcc _ (AvarSharing _ _)) `pickNoneAvar` sa2  = sa2
    sa1                                    `pickNoneAvar` _sa2 = sa1

    (StableSharingExp _ (VarSharing _ _))  `pickNoneVar`  sa2  = sa2
    sa1                                    `pickNoneVar`  _sa2 = sa1

    -- As the StableSharingAccs do not pose a strict ordering, this cleanup
    -- step is needed. In this step, all pairs of AccNodes and ExpNodes
    -- that are of the same height are compared against each other. Without
    -- this step, duplicates may arise.
    --
    -- Note that while (+++) is morally symmetric, replacing `merge [x] y'
    -- with `merge y [x]' inside of `cleanup' won't check all required
    -- possibilities.
    --
    cleanup = concatMap (foldr (\x y -> merge [x] y) []) . groupBy sameHeight
    sameHeight (AccNodeCount sa1 _) (AccNodeCount sa2 _) = not (sa1 `higherSSA` sa2) && not (sa2 `higherSSA` sa1)
    sameHeight (ExpNodeCount se1 _) (ExpNodeCount se2 _) = not (se1 `higherSSE` se2) && not (se2 `higherSSE` se1)
    sameHeight _ _ = False


-- Build an initial environment for the tag values given in the first argument for traversing an
-- array expression.  The 'StableSharingAcc's for all tags /actually used/ in the expressions are
-- in the second argument. (Tags are not used if a bound variable has no usage occurrence.)
--
-- Bail out if any tag occurs multiple times as this indicates that the sharing of an argument
-- variable was not preserved and we cannot build an appropriate initial environment (c.f., comments
-- at 'determineScopesAcc'.
--
buildInitialEnvAcc
    :: HasCallStack
    => [Level]
    -> [StableSharingAcc]
    -> [StableSharingAcc]
buildInitialEnvAcc tags sas = map (lookupSA sas) tags
  where
    lookupSA sas tag1
      = case filter hasTag sas of
          []   -> noStableSharing    -- tag is not used in the analysed expression
          [sa] -> sa                 -- tag has a unique occurrence
          sas2 -> internalError ("Encountered duplicate 'ATag's\n  " ++ intercalate ", " (map showSA sas2))
      where
        hasTag (StableSharingAcc _ (AccSharing _ (Atag _ tag2))) = tag1 == tag2
        hasTag sa
          = internalError ("Encountered a node that is not a plain 'Atag'\n  " ++ showSA sa)

        noStableSharing :: StableSharingAcc
        noStableSharing = StableSharingAcc noStableAccName (undefined :: SharingAcc acc exp ())

    showSA (StableSharingAcc _ (AccSharing  sn acc)) = show (hashStableNameHeight sn) ++ ": " ++
                                                       showPreAccOp acc
    showSA (StableSharingAcc _ (AvarSharing sn _))   = "AvarSharing " ++ show (hashStableNameHeight sn)
    showSA (StableSharingAcc _ (AletSharing sa _))   = "AletSharing " ++ show sa ++ "..."

-- Build an initial environment for the tag values given in the first argument for traversing a
-- scalar expression.  The 'StableSharingExp's for all tags /actually used/ in the expressions are
-- in the second argument. (Tags are not used if a bound variable has no usage occurrence.)
--
-- Bail out if any tag occurs multiple times as this indicates that the sharing of an argument
-- variable was not preserved and we cannot build an appropriate initial environment (c.f., comments
-- at 'determineScopesAcc'.
--
buildInitialEnvExp
    :: HasCallStack
    => [Level]
    -> [StableSharingExp]
    -> [StableSharingExp]
buildInitialEnvExp tags ses = map (lookupSE ses) tags
  where
    lookupSE ses tag1
      = case filter hasTag ses of
          []   -> noStableSharing    -- tag is not used in the analysed expression
          [se] -> se                 -- tag has a unique occurrence
          ses2 -> internalError ("Encountered a duplicate 'Tag'\n  " ++ intercalate ", " (map showSE ses2))
      where
        hasTag (StableSharingExp _ (ExpSharing _ (Tag _ tag2))) = tag1 == tag2
        hasTag se
          = internalError ("Encountered a node that is not a plain 'Tag'\n  " ++ showSE se)

        noStableSharing :: StableSharingExp
        noStableSharing = StableSharingExp noStableExpName (undefined :: SharingExp acc exp ())

    showSE (StableSharingExp _ (ExpSharing sn exp)) = show (hashStableNameHeight sn) ++ ": " ++
                                                      showPreExpOp exp
    showSE (StableSharingExp _ (VarSharing sn _ ))  = "VarSharing " ++ show (hashStableNameHeight sn)
    showSE (StableSharingExp _ (LetSharing se _ ))  = "LetSharing " ++ show se ++ "..."

-- Determine whether a 'NodeCount' is for an 'Atag' or 'Tag', which represent free variables.
--
isFreeVar :: NodeCount -> Bool
isFreeVar (AccNodeCount (StableSharingAcc _ (AccSharing _ (Atag _ _))) _) = True
isFreeVar (ExpNodeCount (StableSharingExp _ (ExpSharing _ (Tag  _ _))) _) = True
isFreeVar _                                                               = False


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
    :: HasCallStack
    => Config
    -> [Level]
    -> OccMap SmartAcc
    -> UnscopedAcc a
    -> (ScopedAcc a, [StableSharingAcc])
determineScopesAcc config fvs accOccMap rootAcc
  = let (sharingAcc, (counts, _)) = determineScopesSharingAcc config accOccMap rootAcc
        unboundTrees              = filter (not . isFreeVar) counts
    in
    if all isFreeVar counts
       then (sharingAcc, buildInitialEnvAcc fvs [sa | AccNodeCount sa _ <- counts])
       else internalError ("unbound shared subtrees" ++ show unboundTrees)


determineScopesSharingAcc
    :: HasCallStack
    => Config
    -> OccMap SmartAcc
    -> UnscopedAcc a
    -> (ScopedAcc a, NodeCounts)
determineScopesSharingAcc config accOccMap = scopesAcc
  where
    scopesAcc :: forall arrs. HasCallStack => UnscopedAcc arrs -> (ScopedAcc arrs, NodeCounts)
    scopesAcc (UnscopedAcc _ (AletSharing _ _))
      = internalError "unexpected 'AletSharing'"

    scopesAcc (UnscopedAcc _ (AvarSharing sn tp))
      = (ScopedAcc [] (AvarSharing sn tp), StableSharingAcc sn (AvarSharing sn tp) `insertAccNode` noNodeCounts)

    scopesAcc (UnscopedAcc _ (AccSharing sn pacc))
      = case pacc of
          Atag tp i               -> reconstruct (Atag tp i) noNodeCounts
          Pipe repr1 repr2 repr3 afun1 afun2 acc
                                  -> let
                                       (afun1', accCount1) = scopesAfun1 afun1
                                       (afun2', accCount2) = scopesAfun1 afun2
                                       (acc', accCount3)   = scopesAcc acc
                                     in
                                     reconstruct (Pipe repr1 repr2 repr3 afun1' afun2' acc')
                                                 (accCount1 +++ accCount2 +++ accCount3)

          Aforeign r ff afun acc  -> let
                                       (acc', accCount) = scopesAcc acc
                                     in
                                     reconstruct (Aforeign r ff afun acc') accCount
          Acond e acc1 acc2       -> let
                                       (e'   , accCount1) = scopesExp e
                                       (acc1', accCount2) = scopesAcc acc1
                                       (acc2', accCount3) = scopesAcc acc2
                                     in
                                     reconstruct (Acond e' acc1' acc2')
                                                 (accCount1 +++ accCount2 +++ accCount3)

          Awhile repr pred iter init
                                  -> let
                                       (pred', accCount1) = scopesAfun1 pred
                                       (iter', accCount2) = scopesAfun1 iter
                                       (init', accCount3) = scopesAcc init
                                     in
                                     reconstruct (Awhile repr pred' iter' init')
                                                 (accCount1 +++ accCount2 +++ accCount3)

          Anil                    -> reconstruct Anil noNodeCounts
          Apair a1 a2             -> let
                                       (a1', accCount1) = scopesAcc a1
                                       (a2', accCount2) = scopesAcc a2
                                     in
                                       reconstruct (Apair a1' a2') (accCount1 +++ accCount2)
          Aprj ix a               -> travA (Aprj ix) a

          Atrace msg a1 a2        -> let
                                       (a1', accCount1) = scopesAcc a1
                                       (a2', accCount2) = scopesAcc a2
                                     in
                                       reconstruct (Atrace msg a1' a2') (accCount1 +++ accCount2)
          Aerror repr msg a1      -> let
                                       (a1', accCount1) = scopesAcc a1
                                     in
                                       reconstruct (Aerror repr msg a1') accCount1
          Use repr arr            -> reconstruct (Use repr arr) noNodeCounts
          Unit tp e               -> let
                                       (e', accCount) = scopesExp e
                                     in
                                     reconstruct (Unit tp e') accCount
          Generate repr sh f      -> let
                                       (sh', accCount1) = scopesExp sh
                                       (f' , accCount2) = scopesFun1 f
                                     in
                                     reconstruct (Generate repr sh' f') (accCount1 +++ accCount2)
          Reshape shr sh acc      -> travEA (Reshape shr) sh acc
          Replicate si n acc      -> travEA (Replicate si) n acc
          Slice si acc i          -> travEA (flip $ Slice si) i acc
          Map t1 t2 f acc         -> let
                                       (f'  , accCount1) = scopesFun1 f
                                       (acc', accCount2) = scopesAcc  acc
                                     in
                                     reconstruct (Map t1 t2 f' acc') (accCount1 +++ accCount2)
          ZipWith t1 t2 t3 f acc1 acc2
                                  -> travF2A2 (ZipWith t1 t2 t3) f acc1 acc2
          Fold tp f z acc         -> travF2MEA (Fold tp) f z acc
          FoldSeg i tp f z acc1 acc2 -> let
                                       (f'   , accCount1)  = scopesFun2 f
                                       (z'   , accCount2)  = travME z
                                       (acc1', accCount3)  = scopesAcc  acc1
                                       (acc2', accCount4)  = scopesAcc  acc2
                                     in
                                     reconstruct (FoldSeg i tp f' z' acc1' acc2')
                                       (accCount1 +++ accCount2 +++ accCount3 +++ accCount4)
          Scan d tp f z acc       -> travF2MEA (Scan d tp) f z acc
          Scan' d tp f z acc      -> travF2EA (Scan' d tp) f z acc
          Permute repr fc acc1 fp acc2
                                  -> let
                                       (fc'  , accCount1) = scopesFun2 fc
                                       (acc1', accCount2) = scopesAcc  acc1
                                       (fp'  , accCount3) = scopesFun1 fp
                                       (acc2', accCount4) = scopesAcc  acc2
                                     in
                                     reconstruct (Permute repr fc' acc1' fp' acc2')
                                       (accCount1 +++ accCount2 +++ accCount3 +++ accCount4)
          Backpermute shr sh fp acc
                                  -> let
                                       (sh' , accCount1) = scopesExp  sh
                                       (fp' , accCount2) = scopesFun1 fp
                                       (acc', accCount3) = scopesAcc  acc
                                     in
                                     reconstruct (Backpermute shr sh' fp' acc')
                                       (accCount1 +++ accCount2 +++ accCount3)
          Stencil sr tp st bnd acc      -> let
                                       (st' , accCount1) = scopesStencil1 acc st
                                       (bnd', accCount2) = scopesBoundary bnd
                                       (acc', accCount3) = scopesAcc acc
                                     in
                                     reconstruct (Stencil sr tp st' bnd' acc') (accCount1 +++ accCount2 +++ accCount3)
          Stencil2 s1 s2 tp st bnd1 acc1 bnd2 acc2
                                  -> let
                                       (st'  , accCount1) = scopesStencil2 acc1 acc2 st
                                       (bnd1', accCount2) = scopesBoundary bnd1
                                       (acc1', accCount3) = scopesAcc acc1
                                       (bnd2', accCount4) = scopesBoundary bnd2
                                       (acc2', accCount5) = scopesAcc acc2
                                     in
                                     reconstruct (Stencil2 s1 s2 tp st' bnd1' acc1' bnd2' acc2')
                                       (accCount1 +++ accCount2 +++ accCount3 +++ accCount4 +++ accCount5)
          -- Collect seq             -> let
          --                              (seq', accCount1) = scopesSeq seq
          --                            in
          --                            reconstruct (Collect seq') accCount1

      where
        travEA :: HasCallStack
               => (ScopedExp e -> ScopedAcc arrs' -> PreSmartAcc ScopedAcc ScopedExp arrs)
               -> RootExp e
               -> UnscopedAcc arrs'
               -> (ScopedAcc arrs, NodeCounts)
        travEA c e acc = reconstruct (c e' acc') (accCount1 +++ accCount2)
          where
            (e'  , accCount1) = scopesExp e
            (acc', accCount2) = scopesAcc acc

        travF2EA
            :: HasCallStack
            => ((SmartExp a -> SmartExp b -> ScopedExp c) -> ScopedExp e -> ScopedAcc arrs' -> PreSmartAcc ScopedAcc ScopedExp arrs)
            -> (SmartExp a -> SmartExp b -> RootExp c)
            -> RootExp e
            -> UnscopedAcc arrs'
            -> (ScopedAcc arrs, NodeCounts)
        travF2EA c f e acc = reconstruct (c f' e' acc') (accCount1 +++ accCount2 +++ accCount3)
          where
            (f'  , accCount1) = scopesFun2 f
            (e'  , accCount2) = scopesExp  e
            (acc', accCount3) = scopesAcc  acc

        travF2MEA
            :: HasCallStack
            => ((SmartExp a -> SmartExp b -> ScopedExp c) -> Maybe (ScopedExp e) -> ScopedAcc arrs' -> PreSmartAcc ScopedAcc ScopedExp arrs)
            -> (SmartExp a -> SmartExp b -> RootExp c)
            -> Maybe (RootExp e)
            -> UnscopedAcc arrs'
            -> (ScopedAcc arrs, NodeCounts)
        travF2MEA c f e acc = reconstruct (c f' e' acc') (accCount1 +++ accCount2 +++ accCount3)
          where
            (f'  , accCount1) = scopesFun2 f
            (e'  , accCount2) = travME e
            (acc', accCount3) = scopesAcc  acc

        travME :: HasCallStack => Maybe (RootExp e) -> (Maybe (ScopedExp e), NodeCounts)
        travME Nothing  = (Nothing, noNodeCounts)
        travME (Just e) = (Just e', c)
          where (e', c) = scopesExp e

        travF2A2
            :: HasCallStack
            => ((SmartExp a -> SmartExp b -> ScopedExp c) -> ScopedAcc arrs1 -> ScopedAcc arrs2 -> PreSmartAcc ScopedAcc ScopedExp arrs)
            -> (SmartExp a -> SmartExp b -> RootExp c)
            -> UnscopedAcc arrs1
            -> UnscopedAcc arrs2
            -> (ScopedAcc arrs, NodeCounts)
        travF2A2 c f acc1 acc2 = reconstruct (c f' acc1' acc2')
                                             (accCount1 +++ accCount2 +++ accCount3)
          where
            (f'   , accCount1) = scopesFun2 f
            (acc1', accCount2) = scopesAcc  acc1
            (acc2', accCount3) = scopesAcc  acc2

        travA :: HasCallStack
              => (ScopedAcc arrs' -> PreSmartAcc ScopedAcc ScopedExp arrs)
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
        reconstruct
            :: HasCallStack
            => PreSmartAcc ScopedAcc ScopedExp arrs
            -> NodeCounts
            -> (ScopedAcc arrs, NodeCounts)
        reconstruct newAcc@(Atag tp _) _subCount
              -- free variable => replace by a sharing variable regardless of the number of
              -- occurrences
          = let thisCount = StableSharingAcc sn (AccSharing sn newAcc) `insertAccNode` noNodeCounts
            in
            tracePure "FREE" (show thisCount)
            (ScopedAcc [] (AvarSharing sn tp), thisCount)
        reconstruct newAcc subCount
              -- shared subtree => replace by a sharing variable (if 'recoverAccSharing' enabled)
          | accOccCount > 1 && acc_sharing `member` options config
          = let allCount = (StableSharingAcc sn sharingAcc `insertAccNode` newCount)
            in
            tracePure ("SHARED" ++ completed) (show allCount)
            (ScopedAcc [] (AvarSharing sn $ Smart.arraysR newAcc), allCount)
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
            -- isBindable _ _ (SeqNodeCount _ _) = False

    -- scopesSeq :: forall arrs. RootSeq arrs -> (ScopedSeq arrs, NodeCounts)
    -- scopesSeq = determineScopesSeq config accOccMap

    scopesExp
        :: HasCallStack
        => RootExp t
        -> (ScopedExp t, NodeCounts)
    scopesExp = determineScopesExp config accOccMap

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesAfun1
        :: HasCallStack
        => (SmartAcc a1 -> UnscopedAcc a2)
        -> (SmartAcc a1 -> ScopedAcc a2, NodeCounts)
    scopesAfun1 f = (const (ScopedAcc ssa body'), (counts', graph))
      where
        body@(UnscopedAcc fvs _)             = f undefined
        (ScopedAcc [] body', (counts,graph)) = scopesAcc body
        (freeCounts, counts')                = partition isBoundHere counts
        ssa                                  = buildInitialEnvAcc fvs [sa | AccNodeCount sa _ <- freeCounts]

        isBoundHere (AccNodeCount (StableSharingAcc _ (AccSharing _ (Atag _ i))) _) = i `elem` fvs
        isBoundHere _                                                               = False

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesFun1
        :: HasCallStack
        => (SmartExp e1 -> RootExp e2)
        -> (SmartExp e1 -> ScopedExp e2, NodeCounts)
    scopesFun1 f = (const body, counts)
      where
        (body, counts) = scopesExp (f undefined)

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesFun2
        :: HasCallStack
        => (SmartExp e1 -> SmartExp e2 -> RootExp e3)
        -> (SmartExp e1 -> SmartExp e2 -> ScopedExp e3, NodeCounts)
    scopesFun2 f = (\_ _ -> body, counts)
      where
        (body, counts) = scopesExp (f undefined undefined)

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesStencil1
        :: forall sh e1 e2 stencil. HasCallStack
        => UnscopedAcc (Array sh e1){-dummy-}
        -> (stencil -> RootExp e2)
        -> (stencil -> ScopedExp e2, NodeCounts)
    scopesStencil1 _ stencilFun = (const body, counts)
      where
        (body, counts) = scopesExp (stencilFun undefined)

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesStencil2
        :: forall sh e1 e2 e3 stencil1 stencil2. HasCallStack
        => UnscopedAcc (Array sh e1){-dummy-}
        -> UnscopedAcc (Array sh e2){-dummy-}
        -> (stencil1 -> stencil2 -> RootExp e3)
        -> (stencil1 -> stencil2 -> ScopedExp e3, NodeCounts)
    scopesStencil2 _ _ stencilFun = (\_ _ -> body, counts)
      where
        (body, counts) = scopesExp (stencilFun undefined undefined)

    scopesBoundary
        :: HasCallStack
        => PreBoundary UnscopedAcc RootExp t
        -> (PreBoundary ScopedAcc ScopedExp t, NodeCounts)
    scopesBoundary bndy =
      case bndy of
        Clamp      -> (Clamp, noNodeCounts)
        Mirror     -> (Mirror, noNodeCounts)
        Wrap       -> (Wrap, noNodeCounts)
        Constant v -> (Constant v, noNodeCounts)
        Function f -> let (body, counts) = scopesFun1 f
                      in  (Function body, counts)


determineScopesExp
    :: HasCallStack
    => Config
    -> OccMap SmartAcc
    -> RootExp t
    -> (ScopedExp t, NodeCounts)          -- Root (closed) expression plus Acc node counts
determineScopesExp config accOccMap (RootExp expOccMap exp@(UnscopedExp fvs _))
  = let
        (ScopedExp [] expWithScopes, (nodeCounts,graph)) = determineScopesSharingExp config accOccMap expOccMap exp
        (expCounts, accCounts)                           = partition isExpNodeCount nodeCounts

        isExpNodeCount ExpNodeCount{} = True
        isExpNodeCount _              = False
    in
    (ScopedExp (buildInitialEnvExp fvs [se | ExpNodeCount se _ <- expCounts]) expWithScopes, cleanCounts (accCounts,graph))


determineScopesSharingExp
    :: HasCallStack
    => Config
    -> OccMap SmartAcc
    -> OccMap SmartExp
    -> UnscopedExp t
    -> (ScopedExp t, NodeCounts)
determineScopesSharingExp config accOccMap expOccMap = scopesExp
  where
    scopesAcc
        :: HasCallStack
        => UnscopedAcc a
        -> (ScopedAcc a, NodeCounts)
    scopesAcc = determineScopesSharingAcc config accOccMap

    scopesFun1
        :: HasCallStack
        => (SmartExp a -> UnscopedExp b)
        -> (SmartExp a -> ScopedExp b, NodeCounts)
    scopesFun1 f = tracePure ("LAMBDA " ++ show ssa) (show counts) (const (ScopedExp ssa body'), (counts',graph))
      where
        body@(UnscopedExp fvs _)              = f undefined
        (ScopedExp [] body', (counts, graph)) = scopesExp body
        (freeCounts, counts')                 = partition isBoundHere counts
        ssa                                   = buildInitialEnvExp fvs [se | ExpNodeCount se _ <- freeCounts]

        isBoundHere (ExpNodeCount (StableSharingExp _ (ExpSharing _ (Tag _ i))) _) = i `elem` fvs
        isBoundHere _                                                              = False

    scopesExp
        :: forall t. HasCallStack
        => UnscopedExp t
        -> (ScopedExp t, NodeCounts)
    scopesExp (UnscopedExp _ (LetSharing _ _))
      = internalError "unexpected 'LetSharing'"

    scopesExp (UnscopedExp _ (VarSharing sn tp))
      = (ScopedExp [] (VarSharing sn tp), StableSharingExp sn (VarSharing sn tp) `insertExpNode` noNodeCounts)

    scopesExp (UnscopedExp _ (ExpSharing sn pexp))
      = case pexp of
          Tag tp i              -> reconstruct (Tag tp i) noNodeCounts
          Const tp c            -> reconstruct (Const tp c) noNodeCounts
          Undef tp              -> reconstruct (Undef tp) noNodeCounts
          Pair e1 e2            -> travE2 Pair e1 e2
          Nil                   -> reconstruct Nil noNodeCounts
          Prj i e               -> travE1 (Prj i) e
          VecPack   vec e       -> travE1 (VecPack   vec) e
          VecUnpack vec e       -> travE1 (VecUnpack vec) e
          ToIndex shr sh ix     -> travE2 (ToIndex shr) sh ix
          FromIndex shr sh e    -> travE2 (FromIndex shr) sh e
          Match t e             -> travE1 (Match t) e
          Case e rhs            -> let (e',   accCount1) = scopesExp e
                                       (rhs', accCount2) = unzip [ ((t,c'), counts)| (t,c) <- rhs, let (c', counts) = scopesExp c ]
                                    in reconstruct (Case e' rhs') (foldr (+++) accCount1 accCount2)
          Cond e1 e2 e3         -> travE3 Cond e1 e2 e3
          While tp p it i       -> let (p' , accCount1) = scopesFun1 p
                                       (it', accCount2) = scopesFun1 it
                                       (i' , accCount3) = scopesExp i
                                    in reconstruct (While tp p' it' i') (accCount1 +++ accCount2 +++ accCount3)
          PrimConst c           -> reconstruct (PrimConst c) noNodeCounts
          PrimApp p e           -> travE1 (PrimApp p) e
          Index tp a e          -> travAE (Index tp) a e
          LinearIndex tp a e    -> travAE (LinearIndex tp) a e
          Shape shr a           -> travA (Shape shr) a
          ShapeSize shr e       -> travE1 (ShapeSize shr) e
          Foreign tp ff f e     -> travE1 (Foreign tp ff f) e
          Coerce t1 t2 e        -> travE1 (Coerce t1 t2) e
      where
        travE1 :: HasCallStack
               => (ScopedExp a -> PreSmartExp ScopedAcc ScopedExp t)
               -> UnscopedExp a
               -> (ScopedExp t, NodeCounts)
        travE1 c e = reconstruct (c e') accCount
          where
            (e', accCount) = scopesExp e

        travE2 :: HasCallStack
               => (ScopedExp a -> ScopedExp b -> PreSmartExp ScopedAcc ScopedExp t)
               -> UnscopedExp a
               -> UnscopedExp b
               -> (ScopedExp t, NodeCounts)
        travE2 c e1 e2 = reconstruct (c e1' e2') (accCount1 +++ accCount2)
          where
            (e1', accCount1) = scopesExp e1
            (e2', accCount2) = scopesExp e2

        travE3 :: HasCallStack
               => (ScopedExp a -> ScopedExp b -> ScopedExp c -> PreSmartExp ScopedAcc ScopedExp t)
               -> UnscopedExp a
               -> UnscopedExp b
               -> UnscopedExp c
               -> (ScopedExp t, NodeCounts)
        travE3 c e1 e2 e3 = reconstruct (c e1' e2' e3') (accCount1 +++ accCount2 +++ accCount3)
          where
            (e1', accCount1) = scopesExp e1
            (e2', accCount2) = scopesExp e2
            (e3', accCount3) = scopesExp e3

        travA :: HasCallStack
              => (ScopedAcc a -> PreSmartExp ScopedAcc ScopedExp t) -> UnscopedAcc a
              -> (ScopedExp t, NodeCounts)
        travA c acc = floatOutAcc c acc' accCount
          where
            (acc', accCount)  = scopesAcc acc

        travAE :: HasCallStack
               => (ScopedAcc a -> ScopedExp b -> PreSmartExp ScopedAcc ScopedExp t)
               -> UnscopedAcc a
               -> UnscopedExp b
               -> (ScopedExp t, NodeCounts)
        travAE c acc e = floatOutAcc (`c` e') acc' (accCountA +++ accCountE)
          where
            (acc', accCountA) = scopesAcc acc
            (e'  , accCountE) = scopesExp e

        floatOutAcc
            :: HasCallStack
            => (ScopedAcc a -> PreSmartExp ScopedAcc ScopedExp t)
            -> ScopedAcc a
            -> NodeCounts
            -> (ScopedExp t, NodeCounts)
        floatOutAcc c acc@(ScopedAcc _ (AvarSharing _ _)) accCount        -- nothing to float out
          = reconstruct (c acc) accCount
        floatOutAcc c acc accCount
          = reconstruct (c var) ((stableAcc `insertAccNode` noNodeCounts) +++ accCount)
          where
             (var, stableAcc) = abstract acc (\(ScopedAcc _ s) -> s)

        abstract
            :: HasCallStack
            => ScopedAcc a
            -> (ScopedAcc a -> SharingAcc ScopedAcc ScopedExp a)
            -> (ScopedAcc a, StableSharingAcc)
        abstract (ScopedAcc _   (AvarSharing _ _))     _    = internalError "AvarSharing"
        abstract (ScopedAcc ssa (AletSharing sa acc))  lets = abstract acc (lets . ScopedAcc ssa . AletSharing sa)
        abstract acc@(ScopedAcc ssa (AccSharing sn a)) lets = (ScopedAcc ssa (AvarSharing sn $ Smart.arraysR a), StableSharingAcc sn (lets acc))

        -- Occurrence count of the currently processed node
        expOccCount = let StableNameHeight sn' _ = sn
                       in lookupWithASTName expOccMap (StableASTName sn')

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
        reconstruct
            :: HasCallStack
            => PreSmartExp ScopedAcc ScopedExp t
            -> NodeCounts
            -> (ScopedExp t, NodeCounts)
        reconstruct newExp@(Tag tp _) _subCount
              -- free variable => replace by a sharing variable regardless of the number of
              -- occurrences
          = let thisCount = StableSharingExp sn (ExpSharing sn newExp) `insertExpNode` noNodeCounts
            in
            tracePure "FREE" (show thisCount)
            (ScopedExp [] (VarSharing sn tp), thisCount)
        reconstruct newExp subCount
              -- shared subtree => replace by a sharing variable (if 'recoverExpSharing' enabled)
          | expOccCount > 1 && exp_sharing `member` options config
          = let allCount = StableSharingExp sn sharingExp `insertExpNode` newCount
            in
            tracePure ("SHARED" ++ completed) (show allCount)
            (ScopedExp [] (VarSharing sn $ typeR newExp), allCount)
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
        filterCompleted :: HasCallStack => NodeCounts -> (NodeCounts, [StableSharingExp])
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
            -- isBindable _ _ (SeqNodeCount _ _) = False

{--
determineScopesSeq
    :: Config
    -> OccMap Acc
    -> RootSeq t
    -> (ScopedSeq t, NodeCounts)          -- Root (closed) expression plus Acc node counts
determineScopesSeq config accOccMap (RootSeq seqOccMap seq)
  = let
        (ScopedSeq seqWithScopes, (nodeCounts,graph)) = determineScopesSharingSeq config accOccMap seqOccMap seq
        binds      = [s | SeqNodeCount s _ <- nodeCounts]
        lets       = foldl (flip (.)) id . map (\x y -> SletSharing x (ScopedSeq y)) $ binds
        sharingSeq = lets seqWithScopes
        newCounts  = filter (not . isSeqCount) nodeCounts
        isSeqCount SeqNodeCount{} = True
        isSeqCount _              = False
    in
    (ScopedSeq sharingSeq, cleanCounts (newCounts,graph))

determineScopesSharingSeq
  :: Config
  -> OccMap Acc
  -> OccMap Seq
  -> UnscopedSeq t
  -> (ScopedSeq t, NodeCounts)
determineScopesSharingSeq config accOccMap _seqOccMap = scopesSeq
  where
    scopesAcc :: UnscopedAcc a -> (ScopedAcc a, NodeCounts)
    scopesAcc = determineScopesSharingAcc config accOccMap

    scopesExp :: RootExp t -> (ScopedExp t, NodeCounts)
    scopesExp = determineScopesExp config accOccMap

    scopesFun2 :: (Elt e1, Elt e2)
               => (Exp e1 -> Exp e2 -> RootExp e3)
               -> (Exp e1 -> Exp e2 -> ScopedExp e3, NodeCounts)
    scopesFun2 f = (\_ _ -> body, counts)
      where
        (body, counts) = scopesExp (f undefined undefined)

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

    scopesAfun2 :: (Arrays a1, Arrays a2) => (Acc a1 -> Acc a2 -> UnscopedAcc a3) -> (Acc a1 -> Acc a2 -> ScopedAcc a3, NodeCounts)
    scopesAfun2 f = (\ _ _ -> (ScopedAcc ssa body'), (counts',graph))
      where
        body@(UnscopedAcc fvs _) = f undefined undefined
        ((ScopedAcc [] body'), (counts,graph)) = scopesAcc body
        ssa     = buildInitialEnvAcc fvs [sa | AccNodeCount sa _ <- freeCounts]
        (freeCounts, counts') = partition isBoundHere counts

        isBoundHere (AccNodeCount (StableSharingAcc _ (AccSharing _ (Atag i))) _) = i `elem` fvs
        isBoundHere _                                                             = False

    scopesAfun3 :: (Arrays a1, Arrays a2, Arrays a3) => (Acc a1 -> Acc a2 -> Acc a3 -> UnscopedAcc a4) -> (Acc a1 -> Acc a2 -> Acc a3 -> ScopedAcc a4, NodeCounts)
    scopesAfun3 f = (\ _ _ _ -> (ScopedAcc ssa body'), (counts',graph))
      where
        body@(UnscopedAcc fvs _) = f undefined undefined undefined
        ((ScopedAcc [] body'), (counts,graph)) = scopesAcc body
        ssa     = buildInitialEnvAcc fvs [sa | AccNodeCount sa _ <- freeCounts]
        (freeCounts, counts') = partition isBoundHere counts

        isBoundHere (AccNodeCount (StableSharingAcc _ (AccSharing _ (Atag i))) _) = i `elem` fvs
        isBoundHere _                                                             = False

    scopesTup :: Atuple UnscopedSeq tup -> (Atuple ScopedSeq tup, NodeCounts)
    scopesTup NilAtup          = (NilAtup, noNodeCounts)
    scopesTup (SnocAtup tup s) = let
                                   (tup', accCountT) = scopesTup tup
                                   (s'  , accCountS) = scopesSeq s
                                 in
                                 (SnocAtup tup' s', accCountT +++ accCountS)

    scopesSeq :: forall t. UnscopedSeq t -> (ScopedSeq t, NodeCounts)
    scopesSeq (UnscopedSeq (SletSharing _ _))
      = $internalError "determineScopesSharingSeq: scopesSeq" "unexpected 'LetSharing'"
    scopesSeq (UnscopedSeq (SvarSharing sn))
      = (ScopedSeq (SvarSharing sn), StableSharingSeq sn (SvarSharing sn) `insertSeqNode` noNodeCounts)

    scopesSeq (UnscopedSeq (SeqSharing sn s)) =
      case s of
        StreamIn arrs -> producer (StreamIn arrs) noNodeCounts
        ToSeq sl acc   -> let
                            (acc', accCount1) = scopesAcc acc
                          in producer (ToSeq sl acc') accCount1
        MapSeq     afun s'  -> let
                                 (afun', accCount1) = scopesAfun1 afun
                                 (s''  , accCount2) = scopesSeq s'
                               in producer (MapSeq afun' s'') (accCount1 +++ accCount2)
        ZipWithSeq afun s1 s2 -> let
                                   (afun', accCount1) = scopesAfun2 afun
                                   (s1'  , accCount2) = scopesSeq s1
                                   (s2'  , accCount3) = scopesSeq s2
                                 in producer (ZipWithSeq afun' s1' s2') (accCount1 +++ accCount2 +++ accCount3)
        ScanSeq fun e s' -> let
                              (fun', accCount1) = scopesFun2 fun
                              (e'  , accCount2) = scopesExp e
                              (s'' , accCount3) = scopesSeq s'
                            in producer (ScanSeq fun' e' s'') (accCount1 +++ accCount2 +++ accCount3)
        FoldSeq fun e s' -> let
                              (fun', accCount1) = scopesFun2 fun
                              (e'  , accCount2) = scopesExp e
                              (s'' , accCount3) = scopesSeq s'
                            in consumer (FoldSeq fun' e' s'') (accCount1 +++ accCount2 +++ accCount3)
        FoldSeqFlatten afun acc s' ->
                               let
                                 (afun', accCount1) = scopesAfun3 afun
                                 (acc' , accCount2) = scopesAcc acc
                                 (s''  , accCount3) = scopesSeq s'
                               in consumer (FoldSeqFlatten afun' acc' s'') (accCount1 +++ accCount2 +++ accCount3)
        Stuple tup          -> let
                                 (tup', accCount1) = scopesTup tup
                               in consumer (Stuple tup') accCount1
      where
        -- All producers must be replaced by sharing variables
        --
        producer :: (t ~ [a], Arrays a)
                 => PreSeq ScopedAcc ScopedSeq ScopedExp t
                 -> NodeCounts
                 -> (ScopedSeq t, NodeCounts)
        producer newSeq subCount
          = let allCount = StableSharingSeq sn (SeqSharing sn newSeq) `insertSeqNode` subCount
            in
            tracePure "Producer" (show allCount)
            (ScopedSeq (SvarSharing sn), allCount)

        -- Consumers cannot be shared.
        --
        consumer :: PreSeq ScopedAcc ScopedSeq ScopedExp t
                 -> NodeCounts
                 -> (ScopedSeq t, NodeCounts)
        consumer newSeq subCount
          = tracePure "Consumer" (show subCount)
            (ScopedSeq (SeqSharing sn newSeq), subCount)
--}

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
{-# NOINLINE recoverSharingAcc #-}
recoverSharingAcc
    :: HasCallStack
    => Config
    -> Level            -- The level of currently bound array variables
    -> [Level]          -- The tags of newly introduced free array variables
    -> SmartAcc a
    -> (ScopedAcc a, [StableSharingAcc])
recoverSharingAcc config alvl avars acc
  = let (acc', occMap)
          = unsafePerformIO             -- to enable stable pointers; this is safe as explained above
          $ makeOccMapAcc config alvl acc
    in
    determineScopesAcc config avars occMap acc'


{-# NOINLINE recoverSharingExp #-}
recoverSharingExp
    :: HasCallStack
    => Config
    -> Level            -- The level of currently bound scalar variables
    -> [Level]          -- The tags of newly introduced free scalar variables
    -> SmartExp e
    -> (ScopedExp e, [StableSharingExp])
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


{--
{-# NOINLINE recoverSharingSeq #-}
recoverSharingSeq
    :: Config
    -> Seq e
    -> (ScopedSeq e, [StableSharingSeq])
recoverSharingSeq config seq
  = let
        (rootSeq, accOccMap) = unsafePerformIO $ do
          accOccMap       <- newASTHashTable
          (seq', _)       <- makeOccMapRootSeq config accOccMap 0 seq
          frozenAccOccMap <- freezeOccMap accOccMap

          return (seq', frozenAccOccMap)

        (ScopedSeq sharingSeq, (ns, _)) =
          determineScopesSeq config accOccMap rootSeq
    in
    (ScopedSeq sharingSeq, [a | SeqNodeCount a _ <- ns])
--}


-- Debugging
-- ---------

traceLine :: String -> String -> IO ()
traceLine header msg
  = Debug.traceIO Debug.dump_sharing
  $ header ++ ": " ++ msg

traceChunk :: String -> String -> IO ()
traceChunk header msg
  = Debug.traceIO Debug.dump_sharing
  $ header ++ "\n      " ++ msg

tracePure :: String -> String -> a -> a
tracePure header msg
  = Debug.trace Debug.dump_sharing
  $ header ++ ": " ++ msg

