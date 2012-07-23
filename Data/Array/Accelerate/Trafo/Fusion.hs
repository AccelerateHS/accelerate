{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Fusion
-- Copyright   : [2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Fusion (

  -- * Fuse array computations
  fuseAcc, fuseAccFun1

) where

-- standard library
import Prelude                                          hiding ( exp )

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo.Simplify
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Array.Sugar                ( Array, Arrays, Shape, Elt )
import Data.Array.Accelerate.Tuple                      hiding ( Tuple )
import qualified Data.Array.Accelerate.Tuple            as Tuple


-- | Apply the array fusion transformation to a de Bruijn AST
--
fuseAcc :: Acc arrs -> Acc arrs
fuseAcc = fuseOpenAcc

-- | Fuse a unary function over array computations
--
fuseAccFun1 :: (Arrays a, Arrays b) => Afun (a -> b) -> Afun (a -> b)
fuseAccFun1 = fuseOpenAfun


-- Array fusion of a de Bruijn computation AST
-- ===========================================

-- Array computations
-- ------------------

fuseOpenAfun :: OpenAfun aenv t -> OpenAfun aenv t
fuseOpenAfun (Alam  f) = Alam  (fuseOpenAfun f)
fuseOpenAfun (Abody a) = Abody (fuseOpenAcc a)


fuseOpenAcc :: OpenAcc aenv a -> OpenAcc aenv a
fuseOpenAcc = force . delayOpenAcc


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
    --
    -- Forms that introduce environment manipulations and control flow. These
    -- stable points of the expression we generally don't want to fuse past.
    --
    -- As an exception, let bound nodes of manifest arrays are allowed to float
    -- upwards.
    --
    Alet bndAcc bodyAcc
      -> case bndAcc of
           OpenAcc bnd@(Use _)  -> float bnd (cvt bodyAcc)
           OpenAcc bnd@(Unit _) -> float bnd (cvt bodyAcc)
           _                    -> done $ Alet (fuseOpenAcc bndAcc) (fuseOpenAcc bodyAcc)

    Avar ix
      -> done $ Avar ix

    Atuple arrs
      -> done $ Atuple (fuseAtuple arrs)

    Aprj ix arrs
      -> done $ Aprj ix (fuseOpenAcc arrs)

    Apply f a
      -> done $ Apply (fuseOpenAfun f) (fuseOpenAcc a)

    Acond p acc1 acc2
      -> done $ Acond (cvtE p) (fuseOpenAcc acc1) (fuseOpenAcc acc2)

    -- Array injection
    --
    Use arrs
      -> done $ Use arrs

    Unit e
      -> done $ Unit e

    -- Index space transforms
    --
    Reshape sl acc
      -> let sh'        = cvtE sl
         in case cvt acc of
           -- TLM: there was a runtime check to ensure the old and new shapes
           -- contained the same number of elements: this has been lost!
           --
           Done env a
            -> Done env $ Reshape (sinkE env sh') (OpenAcc a)

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

    ZipWith f acc1 acc2
      -> zipWithD (cvtF f) acc1 acc2

    -- Consumers
    --
    Fold f z a
      -> done $ Fold (cvtF f) (cvtE z) (force (cvt a))

    Fold1 f a
      -> done $ Fold1 (cvtF f) (force (cvt a))

    FoldSeg f z a s
      -> done $ FoldSeg (cvtF f) (cvtE z) (force (cvt a)) (force (cvt s))

    Fold1Seg f a s
      -> done $ Fold1Seg (cvtF f) (force (cvt a)) (force (cvt s))

    Scanl f z a
      -> done $ Scanl (cvtF f) (cvtE z) (force (cvt a))

    Scanl' f z a
      -> done $ Scanl' (cvtF f) (cvtE z) (force (cvt a))

    Scanl1 f a
      -> done $ Scanl1 (cvtF f) (force (cvt a))

    Scanr f z a
      -> done $ Scanr (cvtF f) (cvtE z) (force (cvt a))

    Scanr' f z a
      -> done $ Scanr' (cvtF f) (cvtE z) (force (cvt a))

    Scanr1 f a
      -> done $ Scanr1 (cvtF f) (force (cvt a))

    Permute f d ix a
      -> done $ Permute (cvtF f) (force (cvt d)) (cvtF ix) (force (cvt a))

    Stencil f b a
      -> done $ Stencil (cvtF f) b (force (cvt a))

    Stencil2 f b1 a1 b0 a0
      -> done $ Stencil2 (cvtF f) b1 (force (cvt a1))
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

data DelayedAcc aenv a where
  Done  :: Arrays a
        => Extend aenv aenv'
        -> PreOpenAcc OpenAcc aenv' a
        -> DelayedAcc         aenv  a

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

done :: Arrays a => PreOpenAcc OpenAcc aenv a -> DelayedAcc aenv a
done = Done BaseEnv

identity :: Elt a => OpenFun env aenv (a -> a)
identity = Lam . Body $ Var ZeroIdx

toIndex :: Shape sh => OpenExp env aenv sh -> OpenFun env aenv (sh -> Int)
toIndex sh = Lam . Body $ ToIndex (weakenE sh) (Var ZeroIdx)

fromIndex :: Shape sh => OpenExp env aenv sh -> OpenFun env aenv (Int -> sh)
fromIndex sh = Lam . Body $ FromIndex (weakenE sh) (Var ZeroIdx)

intersect :: Shape sh => OpenExp env aenv sh -> OpenExp env aenv sh -> OpenExp env aenv sh
intersect sh1 sh2
  | Just REFL <- matchOpenExp sh1 sh2   = sh1
  | Intersect sa sb <- sh1              = sh2 `intersect` sa `intersect` sb
  | Intersect sa sb <- sh2              = sh1 `intersect` sa `intersect` sb
  | otherwise                           = Intersect sh1 sh2


-- "force" a delayed array representation to produce a real AST node.
--
force :: DelayedAcc aenv a -> OpenAcc aenv a
force delayed = OpenAcc $ case delayed of
  Done env a                            -> bind env a
  Yield env sh f                        -> bind env $ Generate (simplifyExp sh) (simplifyFun f)
  Step env sh ix f a
   | Lam (Body (Var ZeroIdx)) <- ix     -> bind env $ Map f'               (OpenAcc a)
   | Lam (Body (Var ZeroIdx)) <- f      -> bind env $ Backpermute sh' ix'  (OpenAcc a)
   | otherwise                          -> bind env $ Transform sh' ix' f' (OpenAcc a)
   where
     f'  = simplifyFun f
     ix' = simplifyFun ix
     sh' = simplifyExp sh

-- let floating
--
float :: Arrays a
      => PreOpenAcc OpenAcc aenv      a
      -> DelayedAcc         (aenv, a) b
      -> DelayedAcc         aenv      b
float a delayed =
  let bnd = BaseEnv `PushEnv` a
  in case delayed of
    Done env b            -> Done  (cat bnd env) b
    Step env sh ix f b    -> Step  (cat bnd env) sh ix f b
    Yield env sh f        -> Yield (cat bnd env) sh f

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
  Done env a            -> Step env (sinkE env sh') (sinkF env p) identity a


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
  Done env a
    | Avar _ <- a       -> Step env (Shape (OpenAcc a)) identity (sinkF env f) a
    | otherwise         -> Step (env `PushEnv` a)
                                (Shape (OpenAcc (Avar ZeroIdx)))
                                identity
                                (weakenFA (sinkF env f))
                                (Avar ZeroIdx)


-- Combine a binary value function and two arrays to produce a delayed array.
-- The trick is that we need to delay one array and then the other, so that the
-- extended environments are built atop each other.
--
zipWithD
    :: forall sh a b c aenv. (Shape sh, Elt a, Elt b, Elt c)
    => Fun        aenv (a -> b -> c)
    -> OpenAcc    aenv (Array sh a)
    -> OpenAcc    aenv (Array sh b)
    -> DelayedAcc aenv (Array sh c)
zipWithD f acc1 acc2 = case delayOpenAcc acc1 of
  Done env a1
    | Avar _ <- a1      -> inner env (shape a1) (index a1)
    | otherwise         -> inner (env `PushEnv` a1) (shape (Avar ZeroIdx)) (index (Avar ZeroIdx))

  Step env1 sh1 ix1 g1 a1
    | Avar _ <- a1      -> inner env1 sh1 (g1 `compose` index a1 `compose` ix1)
    | otherwise         -> inner (env1 `PushEnv` a1)
                                 (weakenEA sh1)
                                 (weakenFA g1 `compose` index (Avar ZeroIdx) `compose` weakenFA ix1)

  Yield env1 sh1 g1     -> inner env1 sh1 g1
  --
  where
    shape :: (Shape dim, Elt e) => PreOpenAcc OpenAcc aenv' (Array dim e) -> Exp aenv' dim
    shape = Shape . OpenAcc

    index :: (Shape dim, Elt e) => PreOpenAcc OpenAcc aenv' (Array dim e) -> Fun aenv' (dim -> e)
    index a = Lam . Body $ IndexScalar (OpenAcc a) (Var ZeroIdx)

    inner :: Extend aenv aenv'
          -> Exp        aenv' sh
          -> Fun        aenv' (sh -> a)
          -> DelayedAcc aenv  (Array sh c)
    inner env1 sh1 g1 = case delayOpenAcc (sinkA env1 acc2) of
      Done env2 a2
        | Avar _ <- a2  ->
            let env = cat env1 env2
            in  Yield env (sinkE env2 sh1 `intersect` shape a2)
                          (generate (sinkF env f) (sinkF env2 g1) (index a2))

        | otherwise     ->
            let env = cat env1 env2 `PushEnv` a2
            in  Yield env (weakenEA (sinkE env2 sh1) `intersect` shape (Avar ZeroIdx))
                          (generate (sinkF env f) (weakenFA (sinkF env2 g1)) (index (Avar ZeroIdx)))

      Step env2 sh2 ix2 g2 a2
        | Avar _ <- a2  ->
            let env = cat env1 env2
            in  Yield env (sinkE env2 sh1 `intersect` sh2)
                          (generate (sinkF env f) (sinkF env2 g1) (g2 `compose` index a2 `compose` ix2))
        | otherwise     ->
            let env = cat env1 env2 `PushEnv` a2
            in  Yield env (weakenEA (sinkE env2 sh1 `intersect` sh2))
                          (generate (sinkF env f) (weakenFA (sinkF env2 g1)) (weakenFA g2 `compose` index (Avar ZeroIdx) `compose` weakenFA ix2))

      Yield env2 sh2 g2
        -> let env = cat env1 env2
           in  Yield env (sinkE env2 sh1 `intersect` sh2)
                         (generate (sinkF env f) (sinkF env2 g1) g2)


-- Substitution
-- ------------

substitute2
    :: (Elt a, Elt b, Elt c)
    => OpenExp ((env, a), b) aenv c
    -> OpenExp (env, x)      aenv a
    -> OpenExp (env, x)      aenv b
    -> OpenExp (env, x)      aenv c
substitute2 f a b
  = Let a
  $ Let (weakenE b)             -- as 'b' has been pushed under a binder
  $ rebuildE split2 f           -- add space for the index environment variable
  where
    split2 :: Elt c => Idx ((env,a),b) c -> PreOpenExp acc (((env,x),a),b) aenv c
    split2 ZeroIdx                = Var ZeroIdx
    split2 (SuccIdx ZeroIdx)      = Var (SuccIdx ZeroIdx)
    split2 (SuccIdx (SuccIdx ix)) = Var (SuccIdx (SuccIdx (SuccIdx ix)))

generate
    :: (Elt a, Elt b, Elt c, Shape sh)
    => OpenFun env aenv (a -> b -> c)
    -> OpenFun env aenv (sh -> a)
    -> OpenFun env aenv (sh -> b)
    -> OpenFun env aenv (sh -> c)
generate (Lam (Lam (Body f))) (Lam (Body a)) (Lam (Body b)) = Lam . Body $ substitute2 f a b
generate _                    _              _              = error "generate: impossible evaluation"


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
  BaseEnv :: Extend aenv aenv

  PushEnv :: Arrays a
          => Extend aenv aenv'
          -> PreOpenAcc OpenAcc aenv' a
          -> Extend aenv (aenv', a)

-- Bind the extended environment so that the fused operators in the inner
-- environment (aenv') can be applied in the outer (aenv).
--
bind :: Arrays a
     => Extend aenv aenv'
     -> PreOpenAcc OpenAcc aenv' a
     -> PreOpenAcc OpenAcc aenv  a
bind BaseEnv         = id
bind (PushEnv env a) = bind env . Alet (OpenAcc a) . OpenAcc


-- Extend array environments
--
sinkA :: Extend aenv aenv'
      -> OpenAcc aenv  a
      -> OpenAcc aenv' a
sinkA BaseEnv       = id
sinkA (PushEnv e _) = weakenA . sinkA e

sinkE :: Extend aenv aenv'
      -> OpenExp env aenv  e
      -> OpenExp env aenv' e
sinkE BaseEnv       = id
sinkE (PushEnv e _) = weakenEA . sinkE e

sinkF :: Extend aenv aenv'
      -> OpenFun env aenv  f
      -> OpenFun env aenv' f
sinkF BaseEnv       = id
sinkF (PushEnv e _) = weakenFA . sinkF e


-- Concatenate two environments
--
cat :: Extend env env' -> Extend env' env'' -> Extend env env''
cat x BaseEnv           = x
cat x (PushEnv e a)     = cat x e `PushEnv` a

