{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Simplify
-- Copyright   : [2012..2014] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Simplify (

  Simplify(..),

) where

-- standard library
import Data.Label
import Data.List                                        ( nubBy )
import Data.Maybe
import Data.Monoid                                      hiding ( All )
import Data.Typeable
import Text.Printf
import Control.Applicative                              hiding ( Const )
import Prelude                                          hiding ( exp, iterate )

-- friends
import Data.Array.Accelerate.AST                        hiding ( prj )
import Data.Array.Accelerate.Analysis.Shape
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Trafo.Algebra
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Trafo.Shrink
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Analysis.Shape
import Data.Array.Accelerate.Array.Representation       ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                ( Elt(..), Shape(..), ShapeR(..), SliceR(..), Slice(..), AsSlice(..), toElt, fromElt, (:.)(..), EltRepr
                                                        , Tuple(..), IsTuple, fromTuple, TupleRepr, shapeToList, transpose )
import qualified Data.Array.Accelerate.Debug            as Stats


class Simplify f where
  simplify :: f -> f

instance Kit acc => Simplify (PreFun acc aenv f) where
  simplify = simplifyFun

instance (Kit acc, Elt e) => Simplify (PreExp acc aenv e) where
  simplify = simplifyExp


-- Scalar optimisations
-- ====================

-- Common subexpression elimination finds computations that are performed at
-- least twice on a given execution path and eliminates the second and later
-- occurrences, replacing them with uses of saved values. This implements a
-- simplified version of that idea, where we look for the expressions of the
-- form:
--
--   let x = e1 in e2
--
-- and replace all occurrences of e1 in e2 with x. This is not full redundancy
-- elimination, but good enough to catch some cases, and in particular those
-- likely to be introduced by scalar composition of terms in the fusion process.
--
-- While it may seem that common subexpression elimination is always worthwhile,
-- as it reduces the number of arithmetic operations performed, this is not
-- necessarily advantageous. The simplest case in which it may not be desirable
-- is if it causes a register to be occupied for a long time in order to hold
-- the shared expression's value, which hence reduces the number of registers
-- available for other uses. Even worse is if the value has to be spilled to
-- memory because there are insufficient registers available. We sidestep this
-- tricky and target-dependent issue by, for now, simply ignoring it.
--
localCSE :: (Kit acc, Elt a)
         => Gamma      acc env env aenv
         -> PreOpenExp acc env     aenv a
         -> PreOpenExp acc (env,a) aenv b
         -> Maybe (PreOpenExp acc env aenv b)
localCSE env bnd body
  | Just ix <- lookupExp env bnd = Stats.ruleFired "CSE" . Just $ inline body (Var ix)
  | otherwise                    = Nothing

-- Common subexpression elimination, which attempts to match the given
-- expression against something already bound in the environment. This can occur
-- due to simplification, in which case we replace the entire subterm with x.
--
-- > let x = e in .. e ..
--
globalCSE :: (Kit acc, Elt t)
          => Gamma      acc env env aenv
          -> PreOpenExp acc env     aenv t
          -> Maybe (PreOpenExp acc env aenv t)
globalCSE env exp
  | Just ix <- lookupExp env exp = Stats.ruleFired "CSE" . Just $ Var ix
  | otherwise                    = Nothing


{--
-- Compared to regular Haskell, the scalar expression language of Accelerate is
-- rather limited in order to meet the restrictions of what can be efficiently
-- implemented on specialised hardware, such as GPUs. For example, to avoid
-- excessive SIMD divergence, we do not support any form of recursion or
-- iteration in scalar expressions. This harmonises well with the stratified
-- design of the Accelerate language: collective array operations comprise many
-- scalar computations that are executed in parallel, so for simplicity of
-- scheduling these operations we would like some assurance that each scalar
-- computation takes approximately the same time to execute as all others.
--
-- However, some computations are naturally expressed in terms of iteration. For
-- some problems, we can instead use generative techniques to implement the
-- program by defining a single step of a recurrence relation as an Accelerate
-- collective operation and using standard Haskell to unroll the loop a _fixed_
-- number of times.
--
-- However, this is outrageously slow because the intermediate values are
-- written to memory at the end of every iteration. Luckily the fusion process
-- will eliminate this intermediate memory traffic by combining the 'n'
-- collective operations into a single operation with 'n' instances of the loop
-- body. However, doing this we uncover an embarrassing secret: C compilers do
-- not compile C code, they compile _idiomatic_ C code.
--
-- This process recovers the iteration structure that was lost in the process of
-- fusing the collective operations. This allows a backend to generate explicit
-- loops in its target language.
--
recoverLoops
    :: (Kit acc, Elt b)
    => Gamma      acc env env aenv
    -> PreOpenExp acc env     aenv a
    -> PreOpenExp acc (env,a) aenv b
    -> Maybe (PreOpenExp acc env aenv b)
recoverLoops _ bnd e3
  -- To introduce scaler loops, we look for expressions of the form:
  --
  --   let x =
  --     let y = e1 in e2
  --   in e3
  --
  -- and if e2 and e3 are congruent, replace with:
  --
  --   iterate[2] (\y -> e2) e1
  --
  | Let e1 e2           <- bnd
  , Just Refl           <- matchEnvTop e2 e3
  , Just Refl           <- match e2 e3
  = Stats.ruleFired "loop recovery/intro" . Just
  $ Iterate (constant 2) e2 e1

  -- To merge expressions into a loop body, look for the pattern:
  --
  --   let x = iterate[n] f e1
  --   in e3
  --
  -- and if e3 matches the loop body, replace the let binding with the bare
  -- iteration with the trip count increased by one.
  --
  | Iterate n f e1      <- bnd
  , Just Refl           <- match f e3
  = Stats.ruleFired "loop recovery/merge" . Just
  $ Iterate (constant 1 `plus` n) f e1

  | otherwise
  = Nothing

  where
    plus :: PreOpenExp acc env aenv Int -> PreOpenExp acc env aenv Int -> PreOpenExp acc env aenv Int
    plus x y = PrimApp (PrimAdd numType) $ Tuple $ NilTup `SnocTup` x `SnocTup` y

    constant :: Int -> PreOpenExp acc env aenv Int
    constant i = Const ((),i)

    matchEnvTop :: (Elt s, Elt t)
                => PreOpenExp acc (env,s) aenv f
                -> PreOpenExp acc (env,t) aenv g
                -> Maybe (s :=: t)
    matchEnvTop _ _ = gcast Refl
--}


-- Walk a scalar expression applying simplifications to terms bottom-up.
--
-- TODO: Look for particular patterns of expressions that can be replaced by
--       something equivalent and simpler. In particular, indexing operations
--       introduced by the fusion transformation. This would benefit from a
--       rewrite rule schema.
--
simplifyOpenExp
    :: forall acc env aenv e. (Kit acc, Elt e)
    => Gamma acc env env aenv
    -> PreOpenExp acc env aenv e
    -> (Bool, PreOpenExp acc env aenv e)
simplifyOpenExp env = first getAny . cvtE
  where
    cvtE :: Elt t => PreOpenExp acc env aenv t -> (Any, PreOpenExp acc env aenv t)
    cvtE IndexNil = pure IndexNil
    cvtE _   | Just e <- gcast IndexNil    = yes e -- If it's Z, don't bother traversing
    cvtE exp | not (zeroCost exp)
             , Just e <- globalCSE env exp = yes e
    cvtE exp = case exp of
      Let bnd body
        -- Just reduct <- recoverLoops env (snd bnd') (snd body') -> yes . snd $ cvtE reduct
        | Just reduct <- localCSE env (snd bnd') (snd body') -> yes . snd $ cvtE reduct
        | zeroCost (snd bnd')                                -> yes $ inline (snd body') (snd bnd')
        | otherwise                                          -> Let <$> bnd' <*> body'
        where
          bnd'  = cvtE bnd
          env'  = PushExp env (snd bnd')
          body' = cvtE' (incExp env') body

      Var ix                    -> pure $ Var ix
      Const c                   -> pure $ Const c
      Tuple tup                 -> Tuple <$> cvtT tup
      Prj ix t                  -> prj env ix (cvtE t)
      IndexNil                  -> pure IndexNil
      IndexAny                  -> pure IndexAny
      IndexCons sh sz           -> indexCons (cvtE sh) (cvtE sz)
      IndexHead sh              -> indexHead (cvtE sh)
      IndexTail sh              -> indexTail (cvtE sh)
      IndexTrans sh             -> indexTrans (cvtE sh)
      IndexSlice x ix sh        -> indexSlice x ix (cvtE sh)
      IndexFull x ix sl         -> indexFull x (cvtE ix) (cvtE sl)
      ToIndex sh ix             -> toIndex (cvtE sh) (cvtE ix)
      FromIndex sh ix           -> fromIndex (cvtE sh) (cvtE ix)
      ToSlice x sh i            -> toSlice x (cvtE sh) (cvtE i)
      Cond p t e                -> cond (cvtE p) (cvtE t) (cvtE e)
      PrimConst c               -> pure $ PrimConst c
      PrimApp f x               -> (u<>v, fx)
        where
          (u, x') = cvtE x
          (v, fx) = evalPrimApp env f x'
      Index a sh                -> Index a <$> cvtE sh
      LinearIndex a i           -> LinearIndex a <$> cvtE i
      Shape a                   -> pure $ Shape a
      ShapeSize sh              -> shapeSize (cvtE sh)
      Intersect s t             -> cvtE s `intersect` cvtE t
      Union s t                 -> cvtE s `union` cvtE t
      Foreign ff f e            -> Foreign ff <$> first Any (simplifyOpenFun EmptyExp f) <*> cvtE e
      While p f x               -> While <$> cvtF env p <*> cvtF env f <*> cvtE x

    cvtT :: Tuple (PreOpenExp acc env aenv) t -> (Any, Tuple (PreOpenExp acc env aenv) t)
    cvtT NilTup        = pure NilTup
    cvtT (SnocTup t e) = SnocTup <$> cvtT t <*> cvtE e

    cvtE' :: Elt e' => Gamma acc env' env' aenv -> PreOpenExp acc env' aenv e' -> (Any, PreOpenExp acc env' aenv e')
    cvtE' env' = first Any . simplifyOpenExp env'

    cvtF :: Gamma acc env' env' aenv -> PreOpenFun acc env' aenv f -> (Any, PreOpenFun acc env' aenv f)
    cvtF env' = first Any . simplifyOpenFun env'

    -- Return the minimal set of unique shapes to intersect. This is a bit
    -- inefficient, but the number of shapes is expected to be small so should
    -- be fine in practice.
    --
    intersect :: Shape t
              => (Any, PreOpenExp acc env aenv t)
              -> (Any, PreOpenExp acc env aenv t)
              -> (Any, PreOpenExp acc env aenv t)
    intersect (c1, sh1) (c2, sh2)
      | Nothing <- match sh sh' = Stats.ruleFired "intersect" (yes sh')
      | otherwise               = (c1 <> c2, sh')
      where
        sh      = Intersect sh1 sh2
        sh'     = foldl1 Intersect
                $ nubBy (\x y -> isJust (match x y))
                $ leaves sh1 ++ leaves sh2

        leaves :: Shape t => PreOpenExp acc env aenv t -> [PreOpenExp acc env aenv t]
        leaves (Intersect x y)  = leaves x ++ leaves y
        leaves rest             = [rest]

    -- Return the minimal set of unique shapes to take the union of. This is a bit
    -- inefficient, but the number of shapes is expected to be small so should
    -- be fine in practice.
    --
    union :: Shape t
          => (Any, PreOpenExp acc env aenv t)
          -> (Any, PreOpenExp acc env aenv t)
          -> (Any, PreOpenExp acc env aenv t)
    union (c1, sh1) (c2, sh2)
      | Nothing <- match sh sh' = Stats.ruleFired "union" (yes sh')
      | otherwise               = (c1 <> c2, sh')
      where
        sh      = Union sh1 sh2
        sh'     = foldl1 Union
                $ nubBy (\x y -> isJust (match x y))
                $ leaves sh1 ++ leaves sh2

        leaves :: Shape t => PreOpenExp acc env aenv t -> [PreOpenExp acc env aenv t]
        leaves (Union x y)  = leaves x ++ leaves y
        leaves rest         = [rest]


    -- Simplify conditional expressions, in particular by eliminating branches
    -- when the predicate is a known constant.
    --
    cond :: forall t. Elt t
         => (Any, PreOpenExp acc env aenv Bool)
         -> (Any, PreOpenExp acc env aenv t)
         -> (Any, PreOpenExp acc env aenv t)
         -> (Any, PreOpenExp acc env aenv t)
    cond p@(_,p') t@(_,t') e@(_,e')
      | Const True  <- p'        = Stats.knownBranch "True"      (yes t')
      | Const False <- p'        = Stats.knownBranch "False"     (yes e')
      | Just Refl <- match t' e' = Stats.knownBranch "redundant" (yes e')
      | otherwise                = Cond <$> p <*> t <*> e

    -- If we are projecting elements from a tuple structure or tuple of constant
    -- valued tuple, pick out the appropriate component directly.
    --
    -- Follow variable bindings, but only if they result in a simplification.
    --
    prj :: forall env' s t. (Elt s, Elt t, IsTuple t)
        => Gamma acc env' env' aenv
        -> TupleIdx (TupleRepr t) s
        -> (Any, PreOpenExp acc env' aenv t)
        -> (Any, PreOpenExp acc env' aenv s)
    prj env' ix top@(_,e) = case e of
      Tuple t                      -> Stats.inline "prj/Tuple"     . yes $ prjT ix t
      IndexCons sl i               -> Stats.inline "prj/IndexCons" . yes $ prjT ix (SnocTup (SnocTup NilTup sl) i)
      Const c                      -> Stats.inline "prj/Const"     . yes $ prjC ix (fromTuple (toElt c :: t))
      Var v   | Just x <- prjV v   -> Stats.inline "prj/Var"       . yes $ x
      Let a b | Just x <- prjL a b -> Stats.inline "prj/Let"       . yes $ x
      _                            -> Prj ix <$> top
      where
        prjT :: TupleIdx tup s -> Tuple (PreOpenExp acc env' aenv) tup -> PreOpenExp acc env' aenv s
        prjT ZeroTupIdx       (SnocTup _ v) = v
        prjT (SuccTupIdx idx) (SnocTup t _) = prjT idx t
#if __GLASGOW_HASKELL__ < 800
        prjT _                _             = error "DO MORE OF WHAT MAKES YOU HAPPY"
#endif

        prjC :: TupleIdx tup s -> tup -> PreOpenExp acc env' aenv s
        prjC ZeroTupIdx       (_,   v) = Const (fromElt v)
        prjC (SuccTupIdx idx) (tup, _) = prjC idx tup

        prjV :: Idx env' t -> Maybe (PreOpenExp acc env' aenv s)
        prjV var
          | e'      <- prjExp var env'
          , Nothing <- match e e'
          = case e' of
              -- Don't push through nested let-bindings; this leads to code explosion
              Let _ _                                    -> Nothing
              _ | (Any True, x) <- prj env' ix (pure e') -> Just x
              _                                          -> Nothing
          | otherwise
          = Nothing

        prjL :: Elt a
             => PreOpenExp acc env'     aenv a
             -> PreOpenExp acc (env',a) aenv t
             -> Maybe (PreOpenExp acc env' aenv s)
        prjL a b
          | (Any True, c) <- prj (incExp $ PushExp env' a) ix (pure b) = Just (Let a c)
        prjL _ _                                                       = Nothing


    -- Shape manipulations
    --
    indexCons :: (Slice sl, Elt sz)
              => (Any, PreOpenExp acc env aenv sl)
              -> (Any, PreOpenExp acc env aenv sz)
              -> (Any, PreOpenExp acc env aenv (sl :. sz))
    indexCons (_,IndexNil) (_,Const c)
      | Just c'         <- cast c       -- EltRepr Z ~ EltRepr ()
      = Stats.ruleFired "Z:.const" $ yes (Const c')
    indexCons (_,IndexNil) (_,IndexHead sz')
      | 1               <- expDim sz'   -- no type information that this is a 1D shape, hence gcast next
      , Just sh'        <- gcast sz'
      = Stats.ruleFired "Z:.indexHead" $ yes sh'
    indexCons (_,IndexTail sl') (_,IndexHead sz')
      | Just Refl       <- match sl' sz'
      = Stats.ruleFired "indexTail:.indexHead" $ yes sl'
    indexCons sl sz
      = IndexCons <$> sl <*> sz

    indexHead :: forall sl sz. (Slice sl, Elt sz) => (Any, PreOpenExp acc env aenv (sl :. sz)) -> (Any, PreOpenExp acc env aenv sz)
    indexHead (_, Const c)
      | _ :. sz <- toElt c :: sl :. sz  = Stats.ruleFired "indexHead/const"     $ yes (Const (fromElt sz))
    indexHead (_, IndexCons _ sz)       = Stats.ruleFired "indexHead/indexCons" $ yes sz
    indexHead sh                        = IndexHead <$> sh

    indexTail :: forall sl sz. (Slice sl, Elt sz) => (Any, PreOpenExp acc env aenv (sl :. sz)) -> (Any, PreOpenExp acc env aenv sl)
    indexTail (_, Const c)
      | sl :. _ <- toElt c :: sl :. sz  = Stats.ruleFired "indexTail/const"     $ yes (Const (fromElt sl))
    indexTail (_, IndexCons sl _)       = Stats.ruleFired "indexTail/indexCons" $ yes sl
    indexTail sh                        = IndexTail <$> sh

    indexTrans :: forall sh. Shape sh => (Any, PreOpenExp acc env aenv sh) -> (Any, PreOpenExp acc env aenv sh)
    indexTrans (_, Const c)                = Stats.ruleFired "indexTrans/const"      $ yes (Const (fromElt (transpose (toElt c :: sh))))
    indexTrans (_, IndexTrans sh)          = Stats.ruleFired "indexTrans/indexTrans" $ yes sh
    indexTrans (_, sh)
      | ShapeRcons ShapeRnil <- shapeType sh
      = Stats.ruleFired "indexTrans/dim1" $ yes sh
    indexTrans sh                          = IndexTrans <$> sh

    indexFull :: forall slix sl sh co. (Shape sl, Shape sh, Elt slix)
              => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
              -> (Any, PreOpenExp acc env aenv slix)
              -> (Any, PreOpenExp acc env aenv sl)
              -> (Any, PreOpenExp acc env aenv sh)
    indexFull _ (_, IndexNil) _
      | Just sh <- gcast IndexNil         -- sh ~ Z
      = Stats.ruleFired "indexFull/Z" $ yes sh
    indexFull (SliceAll x) (_, IndexCons slix (Const ())) (_, IndexCons sl i)
      | ShapeRcons sr <- shapeType (Proxy :: Proxy sh)
      , ShapeRcons _  <- shapeType (Proxy :: Proxy sl)
      , AsSlice       <- asSlice sr
      , Just i'       <- gcast i
      = Stats.ruleFired "indexFull/All" $ yes (IndexCons (IndexFull x slix sl) i')
    indexFull (SliceFixed x) (_, IndexCons slix i) (_, sl)
      | ShapeRcons sr <- shapeType (Proxy :: Proxy sh)
      , AsSlice       <- asSlice sr
      , Just i'       <- gcast i
      = Stats.ruleFired "indexFull/Fixed" $ yes (IndexCons (IndexFull x slix sl) i')
    indexFull slix (_,ToSlice _ sh i) _
      | allFixed slix
      , Just sh' <- gcast (FromIndex sh i)
      = Stats.ruleFired "indexFull/toSlice" $ yes sh'
    indexFull x slix sl
      = IndexFull x <$> slix <*> sl


    indexSlice :: forall proxy slix sl sh co. (Shape sl, Shape sh, Slice slix)
               => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
               -> proxy slix
               -> (Any, PreOpenExp acc env aenv sh)
               -> (Any, PreOpenExp acc env aenv sl)
    indexSlice SliceNil _ _
      | Just sh <- gcast IndexNil         -- sh ~ Z
      = Stats.ruleFired "indexSlice/nil" $ yes sh
    indexSlice (SliceFixed x) p (_, sh)
      | SliceRfixed sr <- sliceType p
      , ShapeRcons sh' <- shapeType sh
      , AsSlice        <- asSlice sh'
      = Stats.ruleFired "indexSlice/fixed" $ yes (IndexSlice x sr (IndexTail sh))
    indexSlice x _ (_, sh)
      | Just Refl <- allAlls x
      , Just sh' <- gcast sh
      = Stats.ruleFired "indexSlice/alls" $ yes sh'
    indexSlice (SliceAll x) p (_, IndexCons sh i)
      | SliceRall sr   <- sliceType p
      , ShapeRcons _   <- shapeType (Proxy :: Proxy sh)
      , ShapeRcons sl' <- shapeType (Proxy :: Proxy sl)
      , AsSlice        <- asSlice sl'
      = Stats.ruleFired "indexSlice/all" $ yes (IndexCons (IndexSlice x sr sh) i)
    indexSlice x p sh
      = IndexSlice x p <$> sh

    allAlls :: SliceIndex slix sl co sh -> Maybe (sl :~: sh)
    allAlls SliceNil       = Just Refl
    allAlls (SliceFixed _) = Nothing
    allAlls (SliceAll sl)  | Just Refl <- allAlls sl
                           = Just Refl
                           | otherwise
                           = Nothing

    shapeSize :: forall sh. Shape sh => (Any, PreOpenExp acc env aenv sh) -> (Any, PreOpenExp acc env aenv Int)
    shapeSize (_, Const c)  = Stats.ruleFired "shapeSize/const" $ yes (Const (product (shapeToList (toElt c :: sh))))
    shapeSize (_, sh)
      | ShapeRnil <- shapeType sh
      = Stats.ruleFired "shapeSize/Z"     $ yes (Const 1)
    shapeSize (_, IndexCons IndexNil i)
      | ShapeRcons _ <- shapeType (Proxy :: Proxy sh)
      = Stats.ruleFired "shapeSize/index1" $ yes i
    shapeSize (_, IndexCons sh (Const n))
      | ShapeRcons _ <- shapeType (Proxy :: Proxy sh)
      , n == 1
      = Stats.ruleFired "shapeSize/indexCons" $ yes (ShapeSize sh)
    shapeSize sh            = ShapeSize <$> sh

    toIndex :: forall sh. Shape sh => (Any, PreOpenExp acc env aenv sh) -> (Any, PreOpenExp acc env aenv sh) -> (Any, PreOpenExp acc env aenv Int)
    toIndex  (_,sh) (_,FromIndex sh' ix)
      | sameSize sh sh' = Stats.ruleFired "toIndex/fromIndex" $ yes ix
    toIndex _ (_,IndexNil)        = Stats.ruleFired "toIndex/Z"         $ yes (Const 0)
    toIndex _ (_,ix)
      | ShapeRcons ShapeRnil <- shapeType ix
      = Stats.ruleFired "toIndex/dim1" $ yes (ShapeSize ix)
    toIndex sh ix                 = ToIndex <$> sh <*> ix

    fromIndex :: forall sh. Shape sh => (Any, PreOpenExp acc env aenv sh) -> (Any, PreOpenExp acc env aenv Int) -> (Any, PreOpenExp acc env aenv sh)
    fromIndex  (_,IndexNil) _
      = Stats.ruleFired "fromIndex/Z" $ yes IndexNil
    fromIndex  (_,sh) (_,ToIndex sh' ix)
      | Just Refl <- match sh sh' = Stats.ruleFired "fromIndex/toIndex" $ yes ix
    fromIndex _ (_,i)
      | ShapeRcons ShapeRnil <- shapeType (Proxy :: Proxy sh)
      = Stats.ruleFired "fromIndex/dim1" $ yes (IndexCons IndexNil i)
    fromIndex sh ix               = FromIndex <$> sh <*> ix

    toSlice :: forall slix sl co. Slice slix
            => SliceIndex (EltRepr slix) sl co (EltRepr (FullShape slix))
            -> (Any, PreOpenExp acc env aenv (FullShape slix))
            -> (Any, PreOpenExp acc env aenv Int)
            -> (Any, PreOpenExp acc env aenv slix)
    toSlice SliceNil _ _
      | SliceRnil <- sliceType (Proxy :: Proxy slix)
      = Stats.ruleFired "toSlice/z" $ yes IndexNil
    toSlice _ _ _
      | SliceRany <- sliceType (Proxy :: Proxy slix)
      = Stats.ruleFired "toSlice/Any" $ yes IndexAny
    toSlice x sh i
      = ToSlice x <$> sh <*> i

    sameSize :: forall sh sh'. (Shape sh, Shape sh') => PreOpenExp acc env aenv sh -> PreOpenExp acc env aenv sh' -> Bool
    sameSize (Const sh) (Const sh')
      | size (toElt sh :: sh) == size (toElt sh' :: sh')
      = True
    sameSize (IndexTrans sh) sh'
      = sameSize sh sh'
    sameSize sh (IndexTrans sh')
      = sameSize sh sh'
    sameSize (IndexCons sh (Const i)) sh'
      | ShapeRcons _ <- shapeType (Proxy :: Proxy sh)
      , i == 1 = sameSize sh sh'
    sameSize sh (IndexCons sh' (Const i))
      | ShapeRcons _ <- shapeType (Proxy :: Proxy sh')
      , i == 1 = sameSize sh sh'
    sameSize (IndexCons sh i) (IndexCons sh' i')
      | ShapeRcons _ <- shapeType (Proxy :: Proxy sh)
      , ShapeRcons _ <- shapeType (Proxy :: Proxy sh')
      , Just Refl <- match i i'
      = sameSize sh sh'
    sameSize sh sh'
      | Just Refl <- match sh sh'
      = True
    sameSize _ _ = False

    -- Sometimes sharing recovery leaves us with simple terms let bound that
    -- don't really need to be. Leaving them let bound can stop other
    -- optimisation passes, like array access reduction, from working.
    --
    -- RCE: At least for the case of array access reduction, this wouldn't be
    -- necessary if we had a more general form of CSE. However, given that we
    -- don't, this is a reasonable compromise as it works in a lot of cases
    -- where it needs to and has no impact on those where it doesn't.
    --
    zeroCost :: PreOpenExp acc env aenv a -> Bool
    zeroCost IndexNil         = True
    zeroCost (Var _)          = True
    zeroCost (Const _)        = True
    zeroCost (IndexTail e)    = zeroCost e
    zeroCost (IndexHead e)    = zeroCost e
    zeroCost (Prj _ e)        = zeroCost e
    zeroCost (IndexCons sh i) = zeroCost sh && zeroCost i
    zeroCost _                = False

    allFixed :: SliceIndex slix sl co sh -> Bool
    allFixed SliceNil          = True
    allFixed (SliceFixed slix) = allFixed slix
    allFixed (SliceAll _)      = False

    first :: (a -> a') -> (a,b) -> (a',b)
    first f (x,y) = (f x, y)

    yes :: x -> (Any, x)
    yes x = (Any True, x)


-- Simplification for open functions
--
simplifyOpenFun
    :: Kit acc
    => Gamma acc env env aenv
    -> PreOpenFun acc env aenv f
    -> (Bool, PreOpenFun acc env aenv f)
simplifyOpenFun env (Body e) = Body <$> simplifyOpenExp env  e
simplifyOpenFun env (Lam f)  = Lam  <$> simplifyOpenFun env' f
  where
    env' = incExp env `PushExp` Var ZeroIdx


-- Simplify closed expressions and functions. The process is applied
-- repeatedly until no more changes are made.
--
simplifyExp :: (Elt t, Kit acc) => PreExp acc aenv t -> PreExp acc aenv t
simplifyExp = iterate summariseOpenExp (simplifyOpenExp EmptyExp)

simplifyFun :: Kit acc => PreFun acc aenv f -> PreFun acc aenv f
simplifyFun = iterate summariseOpenFun (simplifyOpenFun EmptyExp)


-- NOTE: [Simplifier iterations]
--
-- Run the simplification pass _before_ the shrinking step. There are cases
-- where it is better to run shrinking first, and then simplification would
-- complete in a single step, but the converse is also true. However, as
-- shrinking can remove some structure of the let bindings, which might be
-- useful for the transformations (e.g. loop recovery) we want to maintain this
-- information for at least the first pass.
--
-- We always apply the simplification step once. Following this, we iterate
-- shrinking and simplification until the expression no longer changes. Both
-- shrink and simplify return a boolean indicating whether any work was done; we
-- stop as soon as either returns false.
--
-- With internal checks on, we also issue a warning if the iteration limit is
-- reached, but it was still possible to make changes to the expression.
--
{-# SPECIALISE iterate :: (Exp aenv t -> Stats) -> (Exp aenv t -> (Bool, Exp aenv t)) -> Exp aenv t -> Exp aenv t #-}
{-# SPECIALISE iterate :: (Fun aenv t -> Stats) -> (Fun aenv t -> (Bool, Fun aenv t)) -> Fun aenv t -> Fun aenv t #-}

iterate
    :: forall f a. (Match f, Shrink (f a))
    => (f a -> Stats)
    -> (f a -> (Bool, f a))
    -> f a
    -> f a
iterate summarise f = fix 1 . setup
  where
    -- The maximum number of simplifier iterations. To be conservative and avoid
    -- excessive run times, we (should) set this value very low.
    --
    -- TODO: make this tunable via debug flags.
    --
    lIMIT       = 25

    simplify'   = Stats.simplifierDone . f
    setup x     = Stats.trace Stats.dump_simpl_iterations (msg 0 "init" x)
                $ snd (trace 1 "simplify" (simplify' x))

    fix :: Int -> f a -> f a
    fix i x0
      | i > lIMIT       = $internalWarning "simplify" "iteration limit reached" (not (x0 ==^ f x0)) x0
      | not shrunk      = x1
      | not simplified  = x2
      | otherwise       = fix (i+1) x2
      where
        (shrunk,     x1) = trace i "shrink"   $ shrink' x0
        (simplified, x2) = trace i "simplify" $ simplify' x1

    -- debugging support
    --
    u ==^ (_,v)         = isJust (match u v)

    trace i s v@(changed,x)
      | changed         = Stats.trace Stats.dump_simpl_iterations (msg i s x) v
      | otherwise       = v

    msg :: Int -> String -> f a -> String
    msg i s x = printf "simpl-iters/%-8s [%d]: %s" s i (ppr x)

    ppr :: f a -> String
    ppr = show . summarise


-- Debugging support
-- -----------------

data Stats = Stats
  { _terms    :: {-# UNPACK #-} !Int
  , _types    :: {-# UNPACK #-} !Int
  , _binders  :: {-# UNPACK #-} !Int
  , _vars     :: {-# UNPACK #-} !Int
  , _ops      :: {-# UNPACK #-} !Int
  }

instance Show Stats where
  show (Stats a b c d e) =
    printf "terms = %d, types = %d, lets = %d, vars = %d, primops = %d" a b c d e

-- Rather than using the TH deriving mechanism, otherwise the summarise*
-- functions will not be in scope for the above.
--
terms, types, binders, vars, ops :: Stats :-> Int
terms   = lens _terms   (\f Stats{..} -> Stats { _terms   = f _terms, ..})
types   = lens _types   (\f Stats{..} -> Stats { _types   = f _types, ..})
binders = lens _binders (\f Stats{..} -> Stats { _binders = f _binders, ..})
vars    = lens _vars    (\f Stats{..} -> Stats { _vars    = f _vars, ..})
ops     = lens _ops     (\f Stats{..} -> Stats { _ops     = f _ops, ..})

infixl 1 &
(&) :: a -> (a -> b) -> b
(&) x f = f x

infixr 4 +~
(+~) :: Num a => f :-> a -> a -> f -> f
(+~) l c s = modify l (+c) s

infixl 6 +++
(+++) :: Stats -> Stats -> Stats
Stats a1 b1 c1 d1 e1 +++ Stats a2 b2 c2 d2 e2 = Stats (a1+a2) (b1+b2) (c1+c2) (d1+d2) (e1+e2)

summariseOpenFun :: PreOpenFun acc env aenv f -> Stats
summariseOpenFun (Body e) = summariseOpenExp e & terms +~ 1
summariseOpenFun (Lam f)  = summariseOpenFun f & terms +~ 1 & binders +~ 1

summariseOpenExp :: PreOpenExp acc env aenv t -> Stats
summariseOpenExp = modify terms (+1) . goE
  where
    zero = Stats 0 0 0 0 0

    travE :: PreOpenExp acc env aenv t -> Stats
    travE = summariseOpenExp

    travF :: PreOpenFun acc env aenv t -> Stats
    travF = summariseOpenFun

    travA :: acc aenv a -> Stats
    travA _ = zero & vars +~ 1  -- assume an array index, else we should have failed elsewhere

    travT :: Tuple (PreOpenExp acc env aenv) t -> Stats
    travT NilTup        = zero & terms +~ 1
    travT (SnocTup t e) = travT t +++ travE e & terms +~ 1

    travTix :: TupleIdx t e -> Stats
    travTix ZeroTupIdx     = zero & terms +~ 1
    travTix (SuccTupIdx t) = travTix t & terms +~ 1

    travC :: PrimConst c -> Stats
    travC (PrimMinBound t) = travBoundedType t & terms +~ 1
    travC (PrimMaxBound t) = travBoundedType t & terms +~ 1
    travC (PrimPi t)       = travFloatingType t & terms +~ 1

    travNonNumType :: NonNumType t -> Stats
    travNonNumType _ = zero & types +~ 1

    travIntegralType :: IntegralType t -> Stats
    travIntegralType _ = zero & types +~ 1

    travFloatingType :: FloatingType t -> Stats
    travFloatingType _ = zero & types +~ 1

    travNumType :: NumType t -> Stats
    travNumType (IntegralNumType t) = travIntegralType t & types +~ 1
    travNumType (FloatingNumType t) = travFloatingType t & types +~ 1

    travBoundedType :: BoundedType t -> Stats
    travBoundedType (IntegralBoundedType t) = travIntegralType t & types +~ 1
    travBoundedType (NonNumBoundedType t)   = travNonNumType t & types +~ 1

    travScalarType :: ScalarType t -> Stats
    travScalarType (NumScalarType t)    = travNumType t & types +~ 1
    travScalarType (NonNumScalarType t) = travNonNumType t & types +~ 1

    -- The scrutinee has already been counted
    goE :: PreOpenExp acc env aenv t -> Stats
    goE exp =
      case exp of
        Let bnd body          -> travE bnd +++ travE body & binders +~ 1
        Var{}                 -> zero & vars +~ 1
        Foreign _ _ x         -> travE x & terms +~ 1   -- +1 for asm, ignore fallback impls.
        Const{}               -> zero
        Tuple tup             -> travT tup & terms +~ 1
        Prj ix e              -> travTix ix +++ travE e
        IndexNil              -> zero
        IndexCons sh sz       -> travE sh +++ travE sz
        IndexHead sh          -> travE sh
        IndexTail sh          -> travE sh
        IndexAny              -> zero
        IndexSlice _ _ sh     -> travE sh & terms +~ 1 -- +1 for sliceIndex
        IndexFull _ _ sl      -> travE sl & terms +~ 1 -- +1 for sliceIndex
        ToIndex sh ix         -> travE sh +++ travE ix
        FromIndex sh ix       -> travE sh +++ travE ix
        IndexTrans ix         -> travE ix
        ToSlice _ n ix        -> travE n +++ travE ix
        Cond p t e            -> travE p +++ travE t +++ travE e
        While p f x           -> travF p +++ travF f +++ travE x
        PrimConst c           -> travC c
        Index a ix            -> travA a +++ travE ix
        LinearIndex a ix      -> travA a +++ travE ix
        Shape a               -> travA a
        ShapeSize sh          -> travE sh
        Intersect sh1 sh2     -> travE sh1 +++ travE sh2
        Union sh1 sh2         -> travE sh1 +++ travE sh2
        PrimApp f x           -> travPrimFun f +++ travE x

    travPrimFun :: PrimFun f -> Stats
    travPrimFun = modify ops (+1) . goF
      where
        goF :: PrimFun f -> Stats
        goF fun =
          case fun of
            PrimAdd                t -> travNumType t
            PrimSub                t -> travNumType t
            PrimMul                t -> travNumType t
            PrimNeg                t -> travNumType t
            PrimAbs                t -> travNumType t
            PrimSig                t -> travNumType t
            PrimQuot               t -> travIntegralType t
            PrimRem                t -> travIntegralType t
            PrimQuotRem            t -> travIntegralType t
            PrimIDiv               t -> travIntegralType t
            PrimMod                t -> travIntegralType t
            PrimDivMod             t -> travIntegralType t
            PrimBAnd               t -> travIntegralType t
            PrimBOr                t -> travIntegralType t
            PrimBXor               t -> travIntegralType t
            PrimBNot               t -> travIntegralType t
            PrimBShiftL            t -> travIntegralType t
            PrimBShiftR            t -> travIntegralType t
            PrimBRotateL           t -> travIntegralType t
            PrimBRotateR           t -> travIntegralType t
            PrimPopCount           t -> travIntegralType t
            PrimCountLeadingZeros  t -> travIntegralType t
            PrimCountTrailingZeros t -> travIntegralType t
            PrimFDiv               t -> travFloatingType t
            PrimRecip              t -> travFloatingType t
            PrimSin                t -> travFloatingType t
            PrimCos                t -> travFloatingType t
            PrimTan                t -> travFloatingType t
            PrimAsin               t -> travFloatingType t
            PrimAcos               t -> travFloatingType t
            PrimAtan               t -> travFloatingType t
            PrimSinh               t -> travFloatingType t
            PrimCosh               t -> travFloatingType t
            PrimTanh               t -> travFloatingType t
            PrimAsinh              t -> travFloatingType t
            PrimAcosh              t -> travFloatingType t
            PrimAtanh              t -> travFloatingType t
            PrimExpFloating        t -> travFloatingType t
            PrimSqrt               t -> travFloatingType t
            PrimLog                t -> travFloatingType t
            PrimFPow               t -> travFloatingType t
            PrimLogBase            t -> travFloatingType t
            PrimTruncate         f i -> travFloatingType f +++ travIntegralType i
            PrimRound            f i -> travFloatingType f +++ travIntegralType i
            PrimFloor            f i -> travFloatingType f +++ travIntegralType i
            PrimCeiling          f i -> travFloatingType f +++ travIntegralType i
            PrimIsNaN              t -> travFloatingType t
            PrimAtan2              t -> travFloatingType t
            PrimLt                 t -> travScalarType t
            PrimGt                 t -> travScalarType t
            PrimLtEq               t -> travScalarType t
            PrimGtEq               t -> travScalarType t
            PrimEq                 t -> travScalarType t
            PrimNEq                t -> travScalarType t
            PrimMax                t -> travScalarType t
            PrimMin                t -> travScalarType t
            PrimLAnd                 -> zero
            PrimLOr                  -> zero
            PrimLNot                 -> zero
            PrimOrd                  -> zero
            PrimChr                  -> zero
            PrimBoolToInt            -> zero
            PrimFromIntegral     i n -> travIntegralType i +++ travNumType n
            PrimToFloating       n f -> travNumType n +++ travFloatingType f
            PrimCoerce           a b -> travScalarType a +++ travScalarType b

