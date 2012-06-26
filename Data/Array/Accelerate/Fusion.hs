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
  convertAcc, convertAccFun1

) where

-- standard library
import Prelude                                          hiding ( exp )

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Substitution
import Data.Array.Accelerate.Array.Sugar                ( Array, Arrays, Shape, Elt )
import Data.Array.Accelerate.Tuple                      hiding ( Tuple )
import qualified Data.Array.Accelerate.Tuple            as Tuple
import qualified Data.Array.Accelerate.Smart            as Smart
import qualified Data.Array.Accelerate.Sharing          as Smart
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar


-- | Convert a closed array expression to de Bruijn form while also
-- incorporating sharing observation and array fusion.
--
convertAcc :: Arrays arrs => Smart.Acc arrs -> Acc arrs
convertAcc = fuseOpenAcc . Smart.convertAcc

-- | Convert a unary function over array computations
--
convertAccFun1 :: (Arrays a, Arrays b) => (Smart.Acc a -> Smart.Acc b) -> Afun (a -> b)
convertAccFun1 = fuseOpenAfun . Smart.convertAccFun1


-- Array fusion of a de Bruijn computation AST
-- ===========================================

-- Array computations
-- ------------------

fuseOpenAfun :: OpenAfun aenv t -> OpenAfun aenv t
fuseOpenAfun (Alam  f) = Alam  (fuseOpenAfun f)
fuseOpenAfun (Abody a) = Abody (fuseOpenAcc a)


fuseOpenAcc :: OpenAcc aenv a -> OpenAcc aenv a
fuseOpenAcc = force . delayOpenAcc


-- TLM: we may be able to mash an Extend in here for zipwith?
--
delayOpenAcc
    :: OpenAcc    aenv arrs
    -> DelayedAcc aenv arrs
delayOpenAcc (OpenAcc pacc) =
  let cvt :: OpenAcc aenv a -> DelayedAcc aenv a
      cvt = delayOpenAcc

      cvtE :: OpenExp env aenv t -> OpenExp env aenv t
      cvtE = fuseOpenExp

      cvtF :: OpenFun env aenv t -> OpenFun env aenv t
      cvtF = fuseOpenFun
  --
  in case pacc of
    -- Forms that introduce environment manipulations and control flow. These
    -- stable points of the expression we generally don't want to fuse past.
    --
    Alet bndAcc bodyAcc
      -> Done $ Alet (fuseOpenAcc bndAcc) (fuseOpenAcc bodyAcc)

    Avar ix
      -> Done $ Avar ix

    Atuple arrs
      -> Done $ Atuple (fuseAtuple arrs)

    Aprj ix arrs
      -> Done $ Aprj ix (fuseOpenAcc arrs)

    Apply f a
      -> Done $ Apply (fuseOpenAfun f) (fuseOpenAcc a)

    Acond p acc1 acc2
      -> Done $ Acond (cvtE p) (fuseOpenAcc acc1) (fuseOpenAcc acc2)

    -- Array injection
    --
    Use arrs
      -> Done $ Use arrs

    Unit e
      -> Done $ Unit e
--        -> Yield BaseEnv (Const ()) (Lam (Body (weakenE (cvtE e))))

    -- Index space transforms
    --
    Reshape sl acc
      -> let sh'        = cvtE sl
         in case cvt acc of
           -- TLM: there was a runtime check to ensure the old and new shapes
           -- contained the same number of elements: this has been lost!
           --
           Done a
            -> Done $ Reshape sh' (OpenAcc a)

           Step env sh ix f a
            -> Step env (sinkE env sh')
                        (ix `compose` fromIndex sh `compose` toIndex (sinkE env sh')) f a

           Yield env sh f
            -> Yield env (sinkE env sh')
                         (f `compose` fromIndex sh `compose` toIndex (sinkE env sh'))

    Replicate _slix _sh _a
      -> error "delay: Replicate"

    Index _slix _a _sh
      -> error "delay: Index"

    Backpermute sl p acc
      -> backpermuteD (cvtE sl) (cvtF p) (cvt acc)

    -- Producers
    --
    Generate sh f
      -> Yield BaseEnv (cvtE sh) (cvtF f)

    Transform sl p f acc
      -> backpermuteD (cvtE sl) (cvtF p)
       $ mapD (cvtF f) (cvt acc)

    Map f a
      -> mapD (cvtF f) (cvt a)

    ZipWith f a b
      -> zipWithD (cvtF f) (cvt a) (cvt b)

    -- Consumers
    --
    Fold f z a
      -> Done $ Fold (cvtF f) (cvtE z) (force (cvt a))

    Fold1 f a
      -> Done $ Fold1 (cvtF f) (force (cvt a))

    FoldSeg f z a s
      -> Done $ FoldSeg (cvtF f) (cvtE z) (force (cvt a)) (force (cvt s))

    Fold1Seg f a s
      -> Done $ Fold1Seg (cvtF f) (force (cvt a)) (force (cvt s))

    Scanl f z a
      -> Done $ Scanl (cvtF f) (cvtE z) (force (cvt a))

    Scanl' f z a
      -> Done $ Scanl' (cvtF f) (cvtE z) (force (cvt a))

    Scanl1 f a
      -> Done $ Scanl1 (cvtF f) (force (cvt a))

    Scanr f z a
      -> Done $ Scanr (cvtF f) (cvtE z) (force (cvt a))

    Scanr' f z a
      -> Done $ Scanr' (cvtF f) (cvtE z) (force (cvt a))

    Scanr1 f a
      -> Done $ Scanr1 (cvtF f) (force (cvt a))

    Permute f d ix a
      -> Done $ Permute (cvtF f) (force (cvt d)) (cvtF ix) (force (cvt a))

    Stencil f b a
      -> Done $ Stencil (cvtF f) b (force (cvt a))

    Stencil2 f b1 a1 b0 a0
      -> Done $ Stencil2 (cvtF f) b1 (force (cvt a1))
                                  b0 (force (cvt a0))


fuseAtuple
    :: Tuple.Atuple (OpenAcc aenv) a
    -> Tuple.Atuple (OpenAcc aenv) a
fuseAtuple NilAtup          = NilAtup
fuseAtuple (SnocAtup tup a) = fuseAtuple tup `SnocAtup` fuseOpenAcc a


-- Scalar expressions
-- ------------------

fuseOpenExp
    :: OpenExp env aenv t
    -> OpenExp env aenv t
fuseOpenExp = cvt
  where
    cvtA :: OpenAcc aenv a -> OpenAcc aenv a
    cvtA = fuseOpenAcc

    cvt :: OpenExp env aenv t -> OpenExp env aenv t
    cvt exp = case exp of
      Let bnd body      -> Let (cvt bnd) (cvt body)
      Var ix            -> Var ix
      Const c           -> Const c
      Tuple tup         -> Tuple (fuseTuple tup)
      Prj tup ix        -> Prj tup (cvt ix)
      IndexNil          -> IndexNil
      IndexCons sh sz   -> IndexCons (cvt sh) (cvt sz)
      IndexHead sh      -> IndexHead (cvt sh)
      IndexTail sh      -> IndexTail (cvt sh)
      IndexAny          -> IndexAny
      ToIndex sh ix     -> ToIndex (cvt sh) (cvt ix)
      FromIndex sh ix   -> FromIndex (cvt sh) (cvt ix)
      Cond p t e        -> Cond (cvt p) (cvt t) (cvt e)
      PrimConst c       -> PrimConst c
      PrimApp f x       -> PrimApp f (cvt x)
      IndexScalar a sh  -> IndexScalar (cvtA a) (cvt sh)
      Shape a           -> Shape (cvtA a)
      ShapeSize sh      -> ShapeSize (cvt sh)
      Intersect s t     -> Intersect (cvt s) (cvt t)


fuseOpenFun
    :: OpenFun env aenv t
    -> OpenFun env aenv t
fuseOpenFun (Lam f)  = Lam  (fuseOpenFun f)
fuseOpenFun (Body b) = Body (fuseOpenExp b)


fuseTuple
    :: Tuple.Tuple (OpenExp env aenv) t
    -> Tuple.Tuple (OpenExp env aenv) t
fuseTuple NilTup          = NilTup
fuseTuple (SnocTup tup e) = fuseTuple tup `SnocTup` fuseOpenExp e



-- Array Fusion
-- ============

data DelayedAcc aenv a where            -- expose aenv' ?
  Done  :: PreOpenAcc OpenAcc aenv a
        -> DelayedAcc         aenv a

  Step  :: (Elt a, Elt b, Shape sh, Shape sh')
        => Extend aenv aenv'
        -> PreExp     OpenAcc aenv' sh'
        -> PreFun     OpenAcc aenv' (sh' -> sh)
        -> PreFun     OpenAcc aenv' (a   -> b)
        -> PreOpenAcc OpenAcc aenv' (Array sh  a)
        -> DelayedAcc         aenv  (Array sh' b)

  Yield :: (Shape sh, Elt a)
        => Extend aenv aenv'
        -> PreExp OpenAcc aenv' sh
        -> PreFun OpenAcc aenv' (sh -> a)
        -> DelayedAcc     aenv  (Array sh a)


-- Fusion combinators
-- ------------------

identity :: Elt a => OpenFun env aenv (a -> a)
identity = Lam . Body $ Var ZeroIdx

toIndex :: Shape sh => OpenExp env aenv sh -> OpenFun env aenv (sh -> Int)
toIndex sh = Lam . Body $ ToIndex (weakenE sh) (Var ZeroIdx)

fromIndex :: Shape sh => OpenExp env aenv sh -> OpenFun env aenv (Int -> sh)
fromIndex sh = Lam . Body $ FromIndex (weakenE sh) (Var ZeroIdx)


-- "force" a delayed array representation to produce a real AST node.
--
force :: DelayedAcc aenv a -> OpenAcc aenv a
force delayed = OpenAcc $ case delayed of
  Done a                                -> a
  Yield env sh f                        -> bind env $ Generate sh f
  Step env sh ix f a
   | Lam (Body (Var ZeroIdx)) <- ix     -> bind env $ Map f             (OpenAcc a)
   | Lam (Body (Var ZeroIdx)) <- f      -> bind env $ Backpermute sh ix (OpenAcc a)
   | otherwise                          -> bind env $ Transform sh ix f (OpenAcc a)


-- Apply an index space transform that specifies where elements in the
-- destination array read their data from in the source array.
--
backpermuteD
    :: (Shape sh, Shape sh', Elt e)
    => PreExp OpenAcc aenv sh'
    -> PreFun OpenAcc aenv (sh' -> sh)
    -> DelayedAcc     aenv (Array sh  e)
    -> DelayedAcc     aenv (Array sh' e)
backpermuteD sh' p acc = case acc of
  Step env _ ix f a     -> Step env (sinkE env sh') (ix `compose` sinkF env p) f a
  Yield env _ f         -> Yield env (sinkE env sh') (f `compose` sinkF env p)
  Done a                -> Step BaseEnv sh' p identity a


-- Combine a unary value function to a delayed array to produce another delayed
-- array. There is some extraneous work to not introduce extra array variables
-- for things already let-bound.
--
mapD :: (Shape sh, Elt a, Elt b)
     => PreFun OpenAcc aenv (a -> b)
     -> DelayedAcc     aenv (Array sh a)
     -> DelayedAcc     aenv (Array sh b)
mapD f acc = case acc of
  Step env sh ix g a    -> Step env sh ix (sinkF env f `compose` g) a
  Yield env sh g        -> Yield env sh (sinkF env f `compose` g)
  Done a
    | Avar _ <- a       -> Step BaseEnv (Shape (OpenAcc a)) identity f a
    | otherwise         -> Step (BaseEnv `PushEnv` OpenAcc a)
                                (Shape (OpenAcc (Avar ZeroIdx)))
                                identity
                                (weakenFA f)
                                (Avar ZeroIdx)


-- Combine a binary value function and two delayed arrays to produce another
-- delayed array.
--
zipWithD
    :: forall sh a b c aenv. (Shape sh, Elt a, Elt b, Elt c)
    => PreFun OpenAcc aenv (a -> b -> c)
    -> DelayedAcc     aenv (Array sh a)
    -> DelayedAcc     aenv (Array sh b)
    -> DelayedAcc     aenv (Array sh c)
zipWithD fn as bs
  | Lam (Lam (Body combine)) <- fn
  = let
        index acc
          = IndexScalar acc (Var ZeroIdx)

        -- build the generator function
        --
        generate f x1 x0
          = let open :: Elt z => Idx ((env,x),y) z -> PreOpenExp acc (((env,w),x),y) aenv' z
                open ZeroIdx                = Var ZeroIdx
                open (SuccIdx ZeroIdx)      = Var (SuccIdx ZeroIdx)
                open (SuccIdx (SuccIdx ix)) = Var (SuccIdx (SuccIdx (SuccIdx ix)))
            in
            Let x1 $ Let (weakenE x0)           -- as 'x0' is now under a binder
                   $ rebuildE open f            -- add space for the indexing environment variable

        -- now combine the second delayed array, under the already-extended
        -- environment of the first.
        --
        inner :: Extend aenv aenv'
              -> Exp aenv' sh
              -> OpenExp ((),sh) aenv' a
              -> DelayedAcc             aenv (Array sh c)
        inner aenv sh1 g1 = case bs of
          Done a
            | Avar _ <- a
            -> let sh'          = sh1 `Intersect` Shape v0
                   v0           = OpenAcc (sinkA aenv a)
               in
               Yield aenv sh' (Lam (Body (generate (sinkE aenv combine) g1 (index v0))))

            | otherwise
            -> let aenv'        = aenv `PushEnv` OpenAcc (sinkA aenv a)
                   sh'          = weakenEA sh1 `Intersect` Shape v0
                   v0           = OpenAcc (Avar ZeroIdx)
               in
               Yield aenv' sh' (Lam (Body (generate (sinkE aenv' combine) (weakenEA g1) (index v0))))

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
        | Avar _ <- a
        -> inner BaseEnv (Shape (OpenAcc a)) (index (OpenAcc a))

        | otherwise
        -> let aenv     = BaseEnv `PushEnv` OpenAcc a
               v0       = OpenAcc (Avar ZeroIdx)
           in
           inner aenv (Shape v0) (index v0)

      Step aenv sh ix' f' a
        | Lam (Body ix) <- ix'
        , Lam (Body f)  <- f'
        -> case a of
             Avar _ -> inner aenv sh (f `substitute` index (OpenAcc a) `substitute` ix)
             _          -> let aenv'    = aenv `PushEnv` OpenAcc a
                               v0       = OpenAcc (Avar ZeroIdx)
                           in
                           inner aenv' (weakenEA sh)
                                       (weakenEA f `substitute` index v0 `substitute` weakenEA ix)
        | otherwise
        -> error "impossible evaluation"

      Yield aenv sh f'
        | Lam (Body f) <- f'    -> inner aenv sh f
        | otherwise             -> error "impossible evaluation"

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
  BaseEnv ::                                         Extend aenv aenv
  PushEnv :: Arrays a
          => Extend aenv aenv' -> OpenAcc aenv' a -> Extend aenv (aenv', a)

-- Bind the extended environment so that the fused operators in the inner
-- environment (aenv') can be applied in the outer (aenv).
--
bind :: Arrays a
     => Extend aenv aenv'
     -> PreOpenAcc OpenAcc aenv' a
     -> PreOpenAcc OpenAcc aenv  a
bind BaseEnv         = id
bind (PushEnv env a) = bind env . Alet a . OpenAcc


-- Extend array environments
--
sinkA :: Arrays a
      => Extend aenv aenv'
      -> PreOpenAcc OpenAcc aenv  a
      -> PreOpenAcc OpenAcc aenv' a
sinkA BaseEnv       = id
sinkA (PushEnv e _) = weakenA . sinkA e

sinkE :: Extend aenv aenv'
      -> OpenExp env aenv  e
      -> OpenExp env aenv' e
sinkE BaseEnv       = id
sinkE (PushEnv e _) = weakenEA . sinkE e

sinkF :: Extend env env'
      -> PreFun OpenAcc env  f
      -> PreFun OpenAcc env' f
sinkF BaseEnv       = id
sinkF (PushEnv e _) = weakenFA . sinkF e


-- Increase the scope of scalar or array environments.
-- SEE: [Weakening]
--
weakenA :: Arrays t
    => PreOpenAcc OpenAcc aenv      t
    -> PreOpenAcc OpenAcc (aenv, s) t
weakenA = rebuildA rebuildOpenAcc (weakenAcc rebuildOpenAcc . IA)

weakenE
    :: Elt t
    => OpenExp env      aenv t
    -> OpenExp (env, s) aenv t
weakenE = rebuildE (weakenExp . IE)

weakenEA
    :: OpenExp env aenv     t
    -> OpenExp env (aenv,s) t
weakenEA = rebuildEA rebuildOpenAcc (weakenAcc rebuildOpenAcc . IA)

weakenFA
    :: PreOpenFun OpenAcc env aenv t
    -> PreOpenFun OpenAcc env (aenv,s) t
weakenFA = rebuildFA rebuildOpenAcc (weakenAcc rebuildOpenAcc . IA)

