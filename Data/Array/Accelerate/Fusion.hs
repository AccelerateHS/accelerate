{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Fusion
-- Copyright   : [2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements HOAS to de Bruijn conversion of array expressions
-- while incorporating sharing observation and array fusion.
--

module Data.Array.Accelerate.Fusion (

  -- * HOAS -> de Bruijn conversion
  convertAcc, convertAccFun1,

) where

-- standard library
import Data.List                                        ( findIndex )
import Prelude                                          hiding ( exp )

-- friends
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Substitution
-- import Data.Array.Accelerate.Array.Representation       ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                ( Array, Arrays, Shape, Elt, EltRepr )
import Data.Array.Accelerate.Sharing                    hiding ( convertAcc, convertAccFun1 )
import Data.Array.Accelerate.Tuple                      hiding ( Tuple )
import Data.Array.Accelerate.AST                        hiding (
  PreOpenAcc(..), OpenAcc(..), Acc, Stencil(..), PreOpenExp(..), OpenExp, PreExp, Exp )
import qualified Data.Array.Accelerate.AST              as AST
import qualified Data.Array.Accelerate.Tuple            as Tuple
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar

#include "accelerate.h"


-- Conversion from HOAS to de Bruijn computation AST
-- =================================================

-- Array computations
-- ------------------

-- | Convert a closed array expression to de Bruijn form while also
-- incorporating sharing observation and array fusion.
--
convertAcc :: Arrays arrs => Acc arrs -> AST.Acc arrs
convertAcc = convertOpenAcc 0 [] EmptyLayout

-- | Convert a unary function over array computations
--
convertAccFun1 :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> AST.Afun (a -> b)
convertAccFun1 f = Alam (Abody openF)
  where
    lvl         = 0
    a           = Atag lvl
    alyt        = EmptyLayout `PushLayout` ZeroIdx
    openF       = convertOpenAcc (lvl+1) [lvl] alyt (f (Acc a))

-- | Convert an open array expression to de Bruijn form while also
-- incorporating sharing observation and array fusion.
--
convertOpenAcc
    :: Arrays arrs
    => Level
    -> [Level]
    -> Layout aenv aenv
    -> Acc arrs
    -> AST.OpenAcc aenv arrs
convertOpenAcc lvl fvs alyt acc =
  let (sharingAcc, initialEnv)  = recoverSharingAcc True lvl fvs acc
  in
  force $ convertFuseSharingAcc alyt initialEnv sharingAcc


-- | Convert an array expression with given array environment layout and sharing
-- information into de Bruijn form while recovering sharing and fusing adjacent
-- array operations. This implements the third phase of sharing recovery.
--
-- The sharing environment 'env' keeps track of all currently bound sharing
-- variables, keeping them in reverse chronological order (outermost variable is
-- at the end of the list).
--
convertFuseSharingAcc
    :: forall aenv arrs. Arrays arrs
    => Layout aenv aenv
    -> [StableSharingAcc]
    -> SharingAcc arrs
    -> DelayedAcc aenv arrs
convertFuseSharingAcc alyt aenv = cvt
  where
    cvt :: Arrays arrs' => SharingAcc arrs' -> DelayedAcc aenv arrs'
    cvt (AvarSharing sa)
      | Just i <- findIndex (matchStableAcc sa) aenv
      = Done    $ AST.Avar (prjIdx (context ++ "; i = " ++ show i) i alyt)
      | null aenv
      = error userErr
      | otherwise
      = INTERNAL_ERROR(error) "convertFuseSharingAcc" intErr
      where
        context     = "shared 'Acc' tree with stable name " ++ show (hashStableNameHeight sa)
        intErr      = "inconsistent valuation @ " ++ context ++ ";\n  aenv = " ++ show aenv
        userErr     = "Cyclic definition of a value of type 'Acc' (sa = " ++ show (hashStableNameHeight sa) ++ ")"

    cvt (AletSharing sa@(StableSharingAcc _ boundAcc) bodyAcc)
      = let alyt'   = incLayout alyt `PushLayout` ZeroIdx
        in
        Done $ AST.Alet (force $ convertFuseSharingAcc alyt      aenv  boundAcc)
                        (force $ convertFuseSharingAcc alyt' (sa:aenv) bodyAcc)

    cvt (AccSharing _ preAcc)
      = let cvtE :: Elt e => RootExp e -> AST.Exp aenv e
            cvtE = convertFuseExp alyt aenv

            cvtF1 :: (Elt a, Elt b) => (Exp a -> RootExp b) -> AST.Fun aenv (a -> b)
            cvtF1 = convertFuseFun1 alyt aenv

            cvtF2 :: (Elt a, Elt b, Elt c) => (Exp a -> Exp b -> RootExp c) -> AST.Fun aenv (a -> b -> c)
            cvtF2 = convertFuseFun2 alyt aenv
        --
        in case preAcc of
          -- Forms that introduce environment manipulations and control flow. These
          -- stable points of the expression we generally don't want to fuse past.
          --
          Atag i
            -> Done $ AST.Avar (prjIdx ("de Bruijn conversion tag " ++ show i) i alyt)

          Pipe afun1 afun2 acc
            -> let boundAcc = convertAccFun1 afun1 `AST.Apply` force (cvt acc)
                   bodyAcc  = convertAccFun1 afun2 `AST.Apply` AST.OpenAcc (AST.Avar ZeroIdx)
               in
               Done $ AST.Alet (AST.OpenAcc boundAcc) (AST.OpenAcc bodyAcc)

          Acond p acc1 acc2
            -> Done $ AST.Acond (cvtE p) (force (cvt acc1)) (force (cvt acc2))

          Atuple arrs
            -> Done $ AST.Atuple (convertFuseSharingAtuple alyt aenv arrs)

          Aprj ix arrs
            -> Done $ AST.Aprj ix (force (cvt arrs))

          -- Manifest arrays
          --
          Use array
            -> Done $ AST.Use (Sugar.fromArr array)

          Unit e
            -> Done $ AST.Unit (cvtE e)
--            -> Yield BaseEnv (AST.Const ()) (AST.Lam (AST.Body (weakenE $ cvtE e)))

          -- Index space transformations
          --
          Reshape sl acc
            -> let sh'  = cvtE sl
               in case cvt acc of
                 Done a'
                   -> Done $ AST.Reshape sh' (AST.OpenAcc a')

                 -- TLM: There was a runtime check to ensure the old and new
                 -- shapes contained exactly the same number of elements. This
                 -- has been lost!
                 --
                 Step env sh ix f a
                   -> Step env (sinkE env sh')
                               (ix `compose` fromIndex sh `compose` toIndex (sinkE env sh')) f a

                 Yield env sh f
                  -> Yield env (sinkE env sh')
                               (f  `compose` fromIndex sh `compose` toIndex (sinkE env sh'))

          Replicate _slix _a
            -> error "Replicate"

          Index _a _slix
            -> error "Index"

          -- Computation forms
          --
          Generate sh f
            -> Yield BaseEnv (cvtE sh) (cvtF1 f)

          Map f a
            -> delay (cvtF1 f) (cvt a)

          ZipWith f a b
            -> delay2 (cvtF2 f) (cvt a) (cvt b)

          Fold f z a
            -> Done $ AST.Fold (cvtF2 f) (cvtE z) (force (cvt a))

          Fold1 f a
            -> Done $ AST.Fold1 (cvtF2 f) (force (cvt a))

          FoldSeg f z a s
            -> Done $ AST.FoldSeg (cvtF2 f) (cvtE z) (force (cvt a)) (force (cvt s))

          Fold1Seg f a s
            -> Done $ AST.Fold1Seg (cvtF2 f) (force (cvt a)) (force (cvt s))

          Scanl f z a
            -> Done $ AST.Scanl (cvtF2 f) (cvtE z) (force (cvt a))

          Scanl' f z a
            -> Done $ AST.Scanl' (cvtF2 f) (cvtE z) (force (cvt a))

          Scanl1 f a
            -> Done $ AST.Scanl1 (cvtF2 f) (force (cvt a))

          Scanr f z a
            -> Done $ AST.Scanr (cvtF2 f) (cvtE z) (force (cvt a))

          Scanr' f z a
            -> Done $ AST.Scanr' (cvtF2 f) (cvtE z) (force (cvt a))

          Scanr1 f a
            -> Done $ AST.Scanr1 (cvtF2 f) (force (cvt a))

          Permute f d ix a
            -> Done $ AST.Permute (cvtF2 f) (force (cvt d)) (cvtF1 ix) (force (cvt a))

          Backpermute sl p acc
            -> let sh'  = cvtE  sl
                   ix'  = cvtF1 p
               in case cvt acc of
                 Step env _ ix f a      -> Step env (sinkE env sh') (ix `compose` sinkF env ix') f a
                 Yield env _ f          -> Yield env (sinkE env sh') (f `compose` sinkF env ix')
                 Done a
                   | AST.Avar _ <- a    -> Step BaseEnv sh' ix' identity a
                   | otherwise          -> Step (BaseEnv `PushEnv` AST.OpenAcc a)
                                                (weakenEA sh')
                                                (weakenFA ix')
                                                identity
                                                (AST.Avar ZeroIdx)

          Stencil f b a
            -> Done $ AST.Stencil (convertFuseStencilFun a alyt aenv f)
                                  (convertBoundary b) (force (cvt a))

          Stencil2 f b1 a1 b0 a0
            -> Done $ AST.Stencil2 (convertFuseStencilFun2 a1 a0 alyt aenv f)
                                   (convertBoundary b1) (force (cvt a1))
                                   (convertBoundary b0) (force (cvt a0))

-- mkReplicate
--     :: (Shape sh, Elt slix)
--     => SliceIndex (EltRepr slix)
--                   (EltRepr sl)
--                   co
--                   (EltRepr sh)
--     -> AST.Exp aenv slix
--     -> AST.Exp aenv sl
--     -> (AST.Exp aenv sh, AST.Fun aenv (sh -> sl))
-- mkReplicate _ slix sl = undefined
--   where
--     extend :: SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
--            -> AST.Exp aenv slix
--            -> AST.Exp aenv sl
--            -> (AST.Exp aenv sh, AST.Fun aenv (sh -> sl))
--     extend SliceNil             AST.IndexNil    AST.IndexNil    = (AST.IndexNil, AST.Lam (AST.Body AST.IndexNil))
--
--     extend (SliceAll sliceIdx)  x               y
--       = let slx         = AST.IndexTail x
--             sl          = AST.IndexTail y
--             sz          = AST.IndexHead y
--             (dim', f')  = extend sliceIdx slx sl
--         in
--         ( AST.IndexCons dim' sz
--         , undefined )


-- | Convert a boundary condition
--
convertBoundary :: Elt e => Boundary e -> Boundary (EltRepr e)
convertBoundary Clamp        = Clamp
convertBoundary Mirror       = Mirror
convertBoundary Wrap         = Wrap
convertBoundary (Constant e) = Constant (Sugar.fromElt e)


-- | Convert an array tuple expression
--
convertFuseSharingAtuple
    :: forall aenv a.
       Layout aenv aenv
    -> [StableSharingAcc]
    -> Tuple.Atuple SharingAcc         a
    -> Tuple.Atuple (AST.OpenAcc aenv) a
convertFuseSharingAtuple alyt aenv = cvt
  where
    cvt :: Tuple.Atuple SharingAcc a' -> Tuple.Atuple (AST.OpenAcc aenv) a'
    cvt NilAtup         = NilAtup
    cvt (SnocAtup t a)  = cvt t `SnocAtup` force (convertFuseSharingAcc alyt aenv a)


-- Scalar expressions
-- ------------------

-- | Convert an open expression with given environment layouts and sharing
-- information into de Bruijn form while recovering sharing by introducing
-- appropriate let bindings. The latter implements the third stage of sharing
-- recovery.
--
-- The sharing environments 'env' and 'aenv' keep track of all currently bound
-- sharing variables in reverse chronological order (outermost binding is at the
-- end of the list).
--
convertFuseSharingExp
    :: forall env aenv t. Elt t
    => Layout env  env
    -> Layout aenv aenv
    -> [StableSharingExp]
    -> [StableSharingAcc]
    -> SharingExp t
    -> AST.OpenExp env aenv t
convertFuseSharingExp lyt alyt env aenv = cvt
  where
    cvt :: Elt t' => SharingExp t' -> AST.OpenExp env aenv t'
    cvt (VarSharing se)
      | Just i <- findIndex (matchStableExp se) env
      = AST.Var (prjIdx (context ++ "; i = " ++ show i) i lyt)
      | null env
      = error userErr
      | otherwise
      = INTERNAL_ERROR(error) "convertFuseSharingExp" intErr
      where
        context     = "shared 'Exp' tree with stable name " ++ show (hashStableNameHeight se)
        intErr      = "inconsistent valuation @ " ++ context ++ ";\n  env = " ++ show env
        userErr     = "Cyclic definition of a value of type 'Exp' (sa = " ++ show (hashStableNameHeight se) ++ ")"

    cvt (LetSharing se@(StableSharingExp _ boundExp) bodyExp)
      = let lyt'    = incLayout lyt `PushLayout` ZeroIdx
        in
        AST.Let (convertFuseSharingExp lyt  alyt     env  aenv boundExp)
                (convertFuseSharingExp lyt' alyt (se:env) aenv bodyExp)

    cvt (ExpSharing _ preExp)
      = let cvtA :: Arrays arrs => SharingAcc arrs -> AST.OpenAcc aenv arrs
            cvtA = force . convertFuseSharingAcc alyt aenv

        in case preExp of
          Tag i           -> AST.Var (prjIdx ("de Bruijn conversion tag " ++ show i) i lyt)
          Const v         -> AST.Const (Sugar.fromElt v)
          Tuple tup       -> AST.Tuple (convertFuseTuple lyt alyt env aenv tup)
          Prj idx e       -> AST.Prj idx (cvt e)
          IndexNil        -> AST.IndexNil
          IndexCons ix i  -> AST.IndexCons (cvt ix) (cvt i)
          IndexHead i     -> AST.IndexHead (cvt i)
          IndexTail ix    -> AST.IndexTail (cvt ix)
          IndexAny        -> AST.IndexAny
          Cond e1 e2 e3   -> AST.Cond (cvt e1) (cvt e2) (cvt e3)
          PrimConst c     -> AST.PrimConst c
          PrimApp p e     -> AST.PrimApp p (cvt e)
          IndexScalar a e -> AST.IndexScalar (cvtA a) (cvt e)
          Shape a         -> AST.Shape (cvtA a)
          ShapeSize e     -> AST.ShapeSize (cvt e)


convertFuseTuple
    :: forall env aenv t.
       Layout env  env
    -> Layout aenv aenv
    -> [StableSharingExp]
    -> [StableSharingAcc]
    -> Tuple.Tuple SharingExp             t
    -> Tuple.Tuple (AST.OpenExp env aenv) t
convertFuseTuple lyt alyt env aenv = cvt
  where
    cvt :: Tuple.Tuple SharingExp t' -> Tuple.Tuple (AST.OpenExp env aenv) t'
    cvt NilTup          = NilTup
    cvt (SnocTup t e)   = cvt t `SnocTup` convertFuseSharingExp lyt alyt env aenv e


convertFuseExp
    :: Elt t
    => Layout aenv aenv
    -> [StableSharingAcc]
    -> RootExp t
    -> AST.Exp aenv t
convertFuseExp alyt aenv rootExp
  | EnvExp env exp <- rootExp   = convertFuseSharingExp EmptyLayout alyt env aenv exp
  | otherwise                   = INTERNAL_ERROR(error) "convertFuseExp" "not an 'EnvExp'"


convertFuseFun1
    :: (Elt a, Elt b)
    => Layout aenv aenv
    -> [StableSharingAcc]
    -> (Exp a -> RootExp b)
    -> AST.Fun aenv (a -> b)
convertFuseFun1 alyt aenv f = Lam (Body openF)
  where
    a                   = Exp undefined
    lyt                 = EmptyLayout `PushLayout` ZeroIdx
    EnvExp env body     = f a
    openF               = convertFuseSharingExp lyt alyt env aenv body

convertFuseFun2
    :: (Elt a, Elt b, Elt c)
    => Layout aenv aenv
    -> [StableSharingAcc]
    -> (Exp a -> Exp b -> RootExp c)
    -> AST.Fun aenv (a -> b -> c)
convertFuseFun2 alyt aenv f = Lam (Lam (Body openF))
  where
    a                   = Exp undefined
    b                   = Exp undefined
    lyt                 = EmptyLayout `PushLayout` SuccIdx ZeroIdx
                                      `PushLayout` ZeroIdx
    EnvExp env body     = f a b
    openF               = convertFuseSharingExp lyt alyt env aenv body

convertFuseStencilFun
    :: forall a b sh stencil aenv. (Elt a, Elt b, Stencil sh a stencil)
    => SharingAcc (Array sh a)          {- dummy -}
    -> Layout aenv aenv
    -> [StableSharingAcc]
    -> (stencil -> RootExp b)
    -> AST.Fun aenv (StencilRepr sh stencil -> b)
convertFuseStencilFun _ alyt aenv stencilFun = Lam (Body openStencilF)
  where
    stencil             = Exp undefined
    lyt                 = EmptyLayout `PushLayout` ZeroIdx
    EnvExp env body     = stencilFun (stencilPrj (undefined::sh) (undefined::a) stencil)
    openStencilF        = convertFuseSharingExp lyt alyt env aenv body

convertFuseStencilFun2
    :: forall a b c sh stencil1 stencil2 aenv.
       (Elt a, Elt b, Elt c, Stencil sh a stencil1, Stencil sh b stencil2)
    => SharingAcc (Array sh a)          {- dummy -}
    -> SharingAcc (Array sh b)          {- dummy -}
    -> Layout aenv aenv
    -> [StableSharingAcc]
    -> (stencil1 -> stencil2 -> RootExp c)
    -> AST.Fun aenv (StencilRepr sh stencil1 -> StencilRepr sh stencil2 -> c)
convertFuseStencilFun2 _ _ alyt aenv stencilFun = Lam (Lam (Body openStencilF))
  where
    stencil1            = Exp undefined
    stencil2            = Exp undefined
    lyt                 = EmptyLayout `PushLayout` SuccIdx ZeroIdx
                                      `PushLayout` ZeroIdx
    EnvExp env body     = stencilFun (stencilPrj (undefined::sh) (undefined::a) stencil1)
                                     (stencilPrj (undefined::sh) (undefined::b) stencil2)
    openStencilF        = convertFuseSharingExp lyt alyt env aenv body


-- Array Fusion
-- ============


data DelayedAcc aenv a where
  Done  :: AST.PreOpenAcc AST.OpenAcc aenv a
        -> DelayedAcc                 aenv a

  Step  :: (Elt a, Elt b, Shape sh, Shape sh')
        => Extend aenv aenv'
        -> AST.PreExp     AST.OpenAcc aenv' sh'
        -> AST.PreFun     AST.OpenAcc aenv' (sh' -> sh)
        -> AST.PreFun     AST.OpenAcc aenv' (a   -> b)
        -> AST.PreOpenAcc AST.OpenAcc aenv' (Array sh  a)
        -> DelayedAcc                 aenv  (Array sh' b)

  Yield :: (Shape sh, Elt a)
        => Extend aenv aenv'
        -> AST.PreExp AST.OpenAcc aenv' sh
        -> AST.PreFun AST.OpenAcc aenv' (sh -> a)
        -> DelayedAcc             aenv  (Array sh a)


-- Fusion combinators
-- ------------------

identity :: Elt a => OpenFun env aenv (a -> a)
identity
  = AST.Lam
  $ AST.Body
  $ AST.Var ZeroIdx

toIndex :: Shape sh => AST.OpenExp env aenv sh -> AST.OpenFun env aenv (sh -> Int)
toIndex sh
  = AST.Lam
  $ AST.Body
  $ AST.ToIndex (weakenE sh) (AST.Var ZeroIdx)

fromIndex :: Shape sh => AST.OpenExp env aenv sh -> AST.OpenFun env aenv (Int -> sh)
fromIndex sh
  = AST.Lam
  $ AST.Body
  $ AST.FromIndex (weakenE sh) (AST.Var ZeroIdx)


-- "force" a delayed array representation to produce a real AST node.
--
force :: forall aenv a. DelayedAcc aenv a -> AST.OpenAcc aenv a
force delayed = AST.OpenAcc $ case delayed of
  Done a                                -> a
  Yield env sh f                        -> bind env $ AST.Generate sh f
  Step env sh ix f a
   | Lam (Body (AST.Var ZeroIdx)) <- ix -> bind env $ AST.Map f             (AST.OpenAcc a)
   | Lam (Body (AST.Var ZeroIdx)) <- f  -> bind env $ AST.Backpermute sh ix (AST.OpenAcc a)
   | otherwise                          -> bind env $ AST.Transform sh ix f (AST.OpenAcc a)


-- Combine a unary value function of to a delayed array to produce another
-- delayed array. There is some extraneous work to not introduce extra array
-- variables for things already let-bound.
--
delay :: (Shape sh, Elt a, Elt b)
      => AST.PreFun AST.OpenAcc aenv (a -> b)
      -> DelayedAcc             aenv (Array sh a)
      -> DelayedAcc             aenv (Array sh b)
delay f acc = case acc of
  Step env sh ix g a    -> Step env sh ix (sinkF env f `compose` g) a
  Yield env sh g        -> Yield env sh (sinkF env f `compose` g)
  Done a
    | AST.Avar _ <- a   -> Step BaseEnv (AST.Shape (AST.OpenAcc a)) identity f a
    | otherwise         -> Step (BaseEnv `PushEnv` AST.OpenAcc a)
                                (AST.Shape (AST.OpenAcc (AST.Avar ZeroIdx)))
                                identity
                                (weakenFA f)
                                (AST.Avar ZeroIdx)


-- Combine a binary value function and two delayed arrays to produce another
-- delayed array.
--
delay2 :: forall sh a b c aenv. (Shape sh, Elt a, Elt b, Elt c)
       => AST.PreFun AST.OpenAcc aenv (a -> b -> c)
       -> DelayedAcc             aenv (Array sh a)
       -> DelayedAcc             aenv (Array sh b)
       -> DelayedAcc             aenv (Array sh c)
delay2 fn as bs
  | AST.Lam (AST.Lam (AST.Body combine)) <- fn
  = let
        index acc
          = AST.IndexScalar acc (AST.Var ZeroIdx)

        -- build the generator function
        --
        generate f x1 x0
          = let open :: Elt z => Idx ((env,x),y) z -> AST.PreOpenExp acc (((env,w),x),y) aenv' z
                open ZeroIdx                = AST.Var ZeroIdx
                open (SuccIdx ZeroIdx)      = AST.Var (SuccIdx ZeroIdx)
                open (SuccIdx (SuccIdx ix)) = AST.Var (SuccIdx (SuccIdx (SuccIdx ix)))
            in
            AST.Let x1 $ AST.Let (weakenE x0)       -- as 'x0' is now under a binder
                       $ rebuildE open f            -- add space for the indexing environment variable

        -- now combine the second delayed array, under the already-extended
        -- environment of the first.
        --
        inner :: Extend aenv aenv'
              -> AST.Exp aenv' sh
              -> AST.OpenExp ((),sh) aenv' a
              -> DelayedAcc             aenv (Array sh c)
        inner aenv sh1 g1 = case bs of
          Done a
            | AST.Avar _ <- a
            -> let sh'          = sh1 `AST.Intersect` AST.Shape v0
                   v0           = AST.OpenAcc (sinkA aenv a)
               in
               Yield aenv sh' (AST.Lam (AST.Body (generate (sinkE aenv combine) g1 (index v0))))

            | otherwise
            -> let aenv'        = aenv `PushEnv` AST.OpenAcc (sinkA aenv a)
                   sh'          = weakenEA sh1 `AST.Intersect` AST.Shape v0
                   v0           = AST.OpenAcc (AST.Avar ZeroIdx)
               in
               Yield aenv' sh' (AST.Lam (AST.Body (generate (sinkE aenv' combine) (weakenEA g1) (index v0))))

          -- This is difficult, because we need to combine two differently
          -- extend environments:
          --
          -- > Extend env env' `merge` Extend env env'' --> Extend env ???
          --
          Step _ _ _ _ _ -> error "delay2: inner/step"
          Yield _ _ _    -> error "delay2: inner/yield"
    --
    in case as of
      Done a
        | AST.Avar _ <- a
        -> inner BaseEnv (AST.Shape (AST.OpenAcc a)) (index (AST.OpenAcc a))

        | otherwise
        -> let aenv     = BaseEnv `PushEnv` AST.OpenAcc a
               v0       = AST.OpenAcc (AST.Avar ZeroIdx)
           in
           inner aenv (AST.Shape v0) (index v0)

      Step aenv sh ix' f' a
        | AST.Lam (AST.Body ix) <- ix'
        , AST.Lam (AST.Body f)  <- f'
        -> case a of
             AST.Avar _ -> inner aenv sh (f `substitute` index (AST.OpenAcc a) `substitute` ix)
             _          -> let aenv'    = aenv `PushEnv` AST.OpenAcc a
                               v0       = AST.OpenAcc (AST.Avar ZeroIdx)
                           in
                           inner aenv' (weakenEA sh)
                                       (weakenEA f `substitute` index v0 `substitute` weakenEA ix)
        | otherwise
        -> error "impossible evaluation"

      Yield aenv sh f'
        | AST.Lam (AST.Body f) <- f'    -> inner aenv sh f
        | otherwise                     -> error "impossible evaluation"

  | otherwise = error "impossible evaluation"


-- Environment manipulation
-- ------------------------

-- NOTE: [Extend]
--
-- As part of the fusion transformation we often need to lift out array valued
-- inputs to be let-bound at a higher point. This is used by the delayed
-- representations to witness how the array environment is extended in the
-- presence of fused operators.
--
data Extend aenv aenv' where
  BaseEnv ::                                             Extend aenv aenv
  PushEnv :: Arrays a
          => Extend aenv aenv' -> AST.OpenAcc aenv' a -> Extend aenv (aenv', a)

-- Bind the extended environment so that the fused operators in the inner
-- environment (aenv') can be applied in the outer (aenv).
--
bind :: Arrays a
     => Extend aenv aenv'
     -> AST.PreOpenAcc AST.OpenAcc aenv' a
     -> AST.PreOpenAcc AST.OpenAcc aenv  a
bind BaseEnv         = id
bind (PushEnv env a) = bind env . AST.Alet a . AST.OpenAcc


-- Extend array environments
--
sinkA :: Arrays a
      => Extend aenv aenv'
      -> AST.PreOpenAcc AST.OpenAcc aenv  a
      -> AST.PreOpenAcc AST.OpenAcc aenv' a
sinkA BaseEnv       = id
sinkA (PushEnv e _) = weakenA . sinkA e

sinkE :: Extend aenv aenv'
      -> AST.OpenExp env aenv  e
      -> AST.OpenExp env aenv' e
sinkE BaseEnv       = id
sinkE (PushEnv e _) = weakenEA . sinkE e

sinkF :: Extend env env'
      -> AST.PreFun AST.OpenAcc env  f
      -> AST.PreFun AST.OpenAcc env' f
sinkF BaseEnv       = id
sinkF (PushEnv e _) = weakenFA . sinkF e


-- Increase the scope of scalar or array environments.
-- SEE: [Weakening]
--
weakenA :: Arrays t
    => AST.PreOpenAcc AST.OpenAcc aenv      t
    -> AST.PreOpenAcc AST.OpenAcc (aenv, s) t
weakenA = rebuildA rebuildOpenAcc (weakenAcc rebuildOpenAcc . IA)

weakenE
    :: Elt t
    => AST.OpenExp env      aenv t
    -> AST.OpenExp (env, s) aenv t
weakenE = rebuildE (weakenExp . IE)

weakenEA
    :: AST.OpenExp env aenv     t
    -> AST.OpenExp env (aenv,s) t
weakenEA = rebuildEA rebuildOpenAcc (weakenAcc rebuildOpenAcc . IA)

weakenFA
    :: AST.PreOpenFun AST.OpenAcc env aenv t
    -> AST.PreOpenFun AST.OpenAcc env (aenv,s) t
weakenFA = rebuildFA rebuildOpenAcc (weakenAcc rebuildOpenAcc . IA)

