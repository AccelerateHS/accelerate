{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
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
  fuseAcc, fuseAfun,
  Embedded(..), embedOpenAcc,

) where

-- standard library
import Prelude                                          hiding ( exp, until )
import Data.Maybe

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo.Common
import Data.Array.Accelerate.Trafo.Simplify
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Array.Representation       ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                ( Arrays, Array, Elt, EltRepr, Shape )
import Data.Array.Accelerate.Tuple


-- | Apply the array fusion transformation to a de Bruijn AST
--
fuseAcc :: Arrays arrs => Acc arrs -> Acc arrs
fuseAcc = until matchOpenAcc fuseOpenAcc

-- | Fuse a unary function over array computations
--
fuseAfun :: Afun f -> Afun f
fuseAfun = until matchOpenAfun fuseOpenAfun


-- | Convert an array computation into an embeddable delayed representation.
--   TLM: make this nicer-er.
--
data Embedded aenv sh e
  = (Shape sh, Elt e) =>
    DelayedArray { extent      :: Exp aenv sh
                 , index       :: Fun aenv (sh  -> e)
                 , linearIndex :: Fun aenv (Int -> e)
                 }

embedOpenAcc :: (Shape sh, Elt e) => OpenAcc aenv (Array sh e) -> Maybe (Embedded aenv sh e)
embedOpenAcc (OpenAcc pacc)
  | Generate sh f       <- pacc
  = Just $ DelayedArray sh f (f `compose` fromIndex sh)

  | Avar v              <- pacc
  = Just $ DelayedArray (arrayShape v) (indexArray v) (linearIndexArray v)

  | Map f a             <- pacc
  , OpenAcc (Avar v)    <- a
  = Just $ DelayedArray (arrayShape v)
                        (f `compose` indexArray v)
                        (f `compose` linearIndexArray v)

  | Backpermute sh ix a <- pacc
  , OpenAcc (Avar v)    <- a
  = Just $ DelayedArray sh (indexArray v `compose` ix)
                           (indexArray v `compose` ix `compose` fromIndex sh)

  | Transform sh ix f a <- pacc
  , OpenAcc (Avar v)    <- a
  = Just $ DelayedArray sh (f `compose` indexArray v `compose` ix)
                           (f `compose` indexArray v `compose` ix `compose` fromIndex sh)

  | otherwise
  = Nothing


-- Array fusion of a de Bruijn computation AST
-- ===========================================

-- Array computations
-- ------------------

fuseOpenAfun :: OpenAfun aenv t -> OpenAfun aenv t
fuseOpenAfun (Alam  f) = Alam  (fuseOpenAfun f)
fuseOpenAfun (Abody a) = Abody (fuseOpenAcc a)


fuseOpenAcc :: Arrays a => OpenAcc aenv a -> OpenAcc aenv a
fuseOpenAcc = force . delayOpenAcc


delayOpenAcc
    :: Arrays arrs
    => OpenAcc    aenv arrs
    -> DelayedAcc aenv arrs
delayOpenAcc (OpenAcc pacc) =
  let cvtA :: Arrays a => OpenAcc aenv a -> DelayedAcc aenv a
      cvtA = delayOpenAcc

      cvtE :: OpenExp env aenv t -> OpenExp env aenv t
      cvtE = fuseOpenExp

      cvtF :: OpenFun env aenv t -> OpenFun env aenv t
      cvtF = fuseOpenFun

      delayA :: Arrays a => OpenAcc aenv a -> DelayedAcc aenv a
      delayA = delayOpenAcc . until matchOpenAcc fuseOpenAcc

      a0 :: Arrays a => OpenAcc (aenv, a) a
      a0 = OpenAcc (Avar ZeroIdx)

      -- Move bindings around so that consumers are next to producers. We need
      -- to run (delay . fuse) so that the producer Acc is optimised in the
      -- presence of its let bindings before being integrated (cf. aletD)
      --
      consumeFA
          :: (Arrays arrs, Arrays arrs')
          => (forall aenv'. Fun aenv' c -> OpenAcc aenv' arrs -> PreOpenAcc OpenAcc aenv' arrs')
          -> Fun        aenv c
          -> OpenAcc    aenv arrs
          -> DelayedAcc aenv arrs'
      consumeFA op c arr =
        case delayA arr of
          Yield env sh f        -> Done env (op (sinkF env c) (force $ Yield BaseEnv sh f))
          Step env sh ix f v    -> Done env (op (sinkF env c) (force $ Step  BaseEnv sh ix f v))
          Done env a            -> let env' = env `PushEnv` a in Done env' (op (sinkF env' c) a0)

      consumeFEA
          :: (Arrays arrs, Arrays arrs')
          => (forall aenv'. Fun aenv' c -> Exp aenv' z -> OpenAcc aenv' arrs -> PreOpenAcc OpenAcc aenv' arrs')
          -> Fun        aenv c
          -> Exp        aenv z
          -> OpenAcc    aenv arrs
          -> DelayedAcc aenv arrs'
      consumeFEA op c z arr =
        case delayA arr of
          Yield env sh f        -> Done env (op (sinkF env c) (sinkE env z) (force $ Yield BaseEnv sh f))
          Step env sh ix f v    -> Done env (op (sinkF env c) (sinkE env z) (force $ Step  BaseEnv sh ix f v))
          Done env a            -> let env' = env `PushEnv` a in Done env' (op (sinkF env' c) (sinkE env' z) a0)

      consumeFA2
          :: forall arrs' aenv c as bs. (Arrays as, Arrays bs, Arrays arrs')
          => (forall aenv'. Fun aenv' c -> OpenAcc aenv' as -> OpenAcc aenv' bs -> PreOpenAcc OpenAcc aenv' arrs')
          -> Fun        aenv c
          -> OpenAcc    aenv as
          -> OpenAcc    aenv bs
          -> DelayedAcc aenv arrs'
      consumeFA2 op c arr1 arr2 =
        case delayA arr1 of
          Done env a1           -> inner (env `PushEnv` a1) a0
          Step env sh ix f v    -> inner env (force $ Step  BaseEnv sh ix f v)
          Yield env sh f        -> inner env (force $ Yield BaseEnv sh f)
        where
          inner :: Extend aenv aenv' -> OpenAcc aenv' as -> DelayedAcc aenv arrs'
          inner env1 a1 =
            case delayA (sinkA env1 arr2) of
              Yield env2 sh f           -> let env' = join env1 env2 in Done env' (op (sinkF env' c) (sinkA env2 a1) (force $ Yield BaseEnv sh f))
              Step env2 sh ix f v       -> let env' = join env1 env2 in Done env' (op (sinkF env' c) (sinkA env2 a1) (force $ Step  BaseEnv sh ix f v))
              Done env2 a2              -> let env' = join env1 env2 `PushEnv` a2
                                           in  Done env' (op (sinkF env' c) (sinkA (env2 `PushEnv` a2) a1) a0)

      consumeFEA2
          :: forall arrs' aenv c z as bs. (Arrays as, Arrays bs, Arrays arrs')
          => (forall aenv'. Fun aenv' c -> Exp aenv' z -> OpenAcc aenv' as -> OpenAcc aenv' bs -> PreOpenAcc OpenAcc aenv' arrs')
          -> Fun        aenv c
          -> Exp        aenv z
          -> OpenAcc    aenv as
          -> OpenAcc    aenv bs
          -> DelayedAcc aenv arrs'
      consumeFEA2 op c z arr1 arr2 =
        case delayA arr1 of
          Done env a1           -> inner (env `PushEnv` a1) a0
          Step env sh ix f v    -> inner env (force $ Step  BaseEnv sh ix f v)
          Yield env sh f        -> inner env (force $ Yield BaseEnv sh f)
        where
          inner :: Extend aenv aenv' -> OpenAcc aenv' as -> DelayedAcc aenv arrs'
          inner env1 a1 =
            case delayA (sinkA env1 arr2) of
              Yield env2 sh f           -> let env' = join env1 env2 in Done env' (op (sinkF env' c) (sinkE env' z) (sinkA env2 a1) (force $ Yield BaseEnv sh f))
              Step env2 sh ix f v       -> let env' = join env1 env2 in Done env' (op (sinkF env' c) (sinkE env' z) (sinkA env2 a1) (force $ Step  BaseEnv sh ix f v))
              Done env2 a2              -> let env' = join env1 env2 `PushEnv` a2
                                           in  Done env' (op (sinkF env' c) (sinkE env' z) (sinkA (env2 `PushEnv` a2) a1) a0)

      consumeF2A2
          :: forall arrs' aenv c p as bs. (Arrays as, Arrays bs, Arrays arrs')
          => (forall aenv'. Fun aenv' c -> Fun aenv' p -> OpenAcc aenv' as -> OpenAcc aenv' bs -> PreOpenAcc OpenAcc aenv' arrs')
          -> Fun        aenv c
          -> Fun        aenv p
          -> OpenAcc    aenv as
          -> OpenAcc    aenv bs
          -> DelayedAcc aenv arrs'
      consumeF2A2 op c p arr1 arr2 =
        case delayA arr1 of
          Done env a1           -> inner (env `PushEnv` a1) a0
          Step env sh ix f v    -> inner env (force $ Step  BaseEnv sh ix f v)
          Yield env sh f        -> inner env (force $ Yield BaseEnv sh f)
        where
          inner :: Extend aenv aenv' -> OpenAcc aenv' as -> DelayedAcc aenv arrs'
          inner env1 a1 =
            case delayA (sinkA env1 arr2) of
              Yield env2 sh f           -> let env' = join env1 env2 in Done env' (op (sinkF env' c) (sinkF env' p) (sinkA env2 a1) (force $ Yield BaseEnv sh f))
              Step env2 sh ix f v       -> let env' = join env1 env2 in Done env' (op (sinkF env' c) (sinkF env' p) (sinkA env2 a1) (force $ Step  BaseEnv sh ix f v))
              Done env2 a2              -> let env' = join env1 env2 `PushEnv` a2
                                           in  Done env' (op (sinkF env' c) (sinkF env' p) (sinkA (env2 `PushEnv` a2) a1) a0)
  --
  in case pacc of
    -- Forms that introduce environment manipulations and control flow. We
    -- generally don't want to fuse past these points in the expression.
    --
    Alet bnd body       -> aletD bnd body
    Avar ix             -> done $ Avar ix
    Atuple arrs         -> done $ Atuple (fuseAtuple arrs)
    Aprj ix arrs        -> done $ Aprj ix (fuseOpenAcc arrs)
    Apply f a           -> done $ Apply (fuseOpenAfun f) (fuseOpenAcc a)
    Acond p acc1 acc2   -> done $ Acond (cvtE p) (fuseOpenAcc acc1) (fuseOpenAcc acc2)
    Foreign ff afun acc -> done $ Foreign ff (fuseOpenAfun afun) (fuseOpenAcc acc)

    -- Array injection
    --
    Use arrs            -> done $ Use arrs
    Unit e              -> done $ Unit e

    -- Index space transforms
    --
    Reshape sl acc      -> reshapeD (cvtE sl) (cvtA acc)
    Replicate slix sh a -> replicateD slix (cvtE sh) (cvtA a)
    Slice slix a sh     -> sliceD slix (cvtA a) (cvtE sh)
    Backpermute sl p a  -> backpermuteD (cvtE sl) (cvtF p) (cvtA a)

    -- Producers
    --
    Generate sh f       -> Yield BaseEnv (cvtE sh) (cvtF f)
    Map f a             -> mapD (cvtF f) (cvtA a)
    ZipWith f a1 a2     -> zipWithD (cvtF f) a1 a2
    Transform sl p f a  -> backpermuteD (cvtE sl) (cvtF p) $ mapD (cvtF f) (cvtA a)

    -- Consumers
    --
    Fold f z a          -> consumeFEA Fold (cvtF f) (cvtE z) a
    Fold1 f a           -> consumeFA Fold1 (cvtF f) a
    FoldSeg f z a s     -> consumeFEA2 FoldSeg (cvtF f) (cvtE z) a s
    Fold1Seg f a s      -> consumeFA2 Fold1Seg (cvtF f) a s
    Scanl1 f a          -> consumeFA Scanl1 (cvtF f) a
    Scanl f z a         -> consumeFEA Scanl (cvtF f) (cvtE z) a
    Scanl' f z a        -> consumeFEA Scanl' (cvtF f) (cvtE z) a
    Scanr1 f a          -> consumeFA Scanr1 (cvtF f) a
    Scanr f z a         -> consumeFEA Scanr (cvtF f) (cvtE z) a
    Scanr' f z a        -> consumeFEA Scanr' (cvtF f) (cvtE z) a
    Permute f d p a     -> consumeF2A2 (\f' p' d' a' -> Permute f' d' p' a') (cvtF f) (cvtF p) d a
    Stencil f x a       -> consumeFA (\f' a' -> Stencil f' x a') (cvtF f) a
    Stencil2 f x a y b  -> consumeFA2 (\f' a' b' -> Stencil2 f' x a' y b') (cvtF f) a b


fuseAtuple
    :: Atuple (OpenAcc aenv) a
    -> Atuple (OpenAcc aenv) a
fuseAtuple NilAtup          = NilAtup
fuseAtuple (SnocAtup tup a) = fuseAtuple tup `SnocAtup` fuseOpenAcc a


-- Scalar expressions
-- ------------------

fuseOpenExp
    :: OpenExp env aenv t
    -> OpenExp env aenv t
fuseOpenExp = cvt
  where
    cvtA :: Arrays a => OpenAcc aenv a -> OpenAcc aenv a
    cvtA = fuseOpenAcc

    cvt :: OpenExp env aenv t -> OpenExp env aenv t
    cvt exp = case exp of
      Let bnd body              -> Let (cvt bnd) (cvt body)
      Var ix                    -> Var ix
      Const c                   -> Const c
      Tuple tup                 -> Tuple (fuseTuple tup)
      Prj tup ix                -> Prj tup (cvt ix)
      IndexNil                  -> IndexNil
      IndexCons sh sz           -> IndexCons (cvt sh) (cvt sz)
      IndexHead sh              -> IndexHead (cvt sh)
      IndexTail sh              -> IndexTail (cvt sh)
      IndexAny                  -> IndexAny
      IndexSlice x ix sh        -> IndexSlice x (cvt ix) (cvt sh)
      IndexFull x ix sl         -> IndexFull x (cvt ix) (cvt sl)
      ToIndex sh ix             -> ToIndex (cvt sh) (cvt ix)
      FromIndex sh ix           -> FromIndex (cvt sh) (cvt ix)
      Cond p t e                -> Cond (cvt p) (cvt t) (cvt e)
      Iterate n f x             -> Iterate (cvt n) (cvt f) (cvt x)
      PrimConst c               -> PrimConst c
      PrimApp f x               -> PrimApp f (cvt x)
      Index a sh                -> Index (cvtA a) (cvt sh)
      LinearIndex a i           -> LinearIndex (cvtA a) (cvt i)
      Shape a                   -> Shape (cvtA a)
      ShapeSize sh              -> ShapeSize (cvt sh)
      Intersect s t             -> Intersect (cvt s) (cvt t)


fuseOpenFun
    :: OpenFun env aenv t
    -> OpenFun env aenv t
fuseOpenFun (Lam f)  = Lam  (fuseOpenFun f)
fuseOpenFun (Body b) = Body (fuseOpenExp b)


fuseTuple
    :: Tuple (OpenExp env aenv) t
    -> Tuple (OpenExp env aenv) t
fuseTuple NilTup          = NilTup
fuseTuple (SnocTup tup e) = fuseTuple tup `SnocTup` fuseOpenExp e



-- Array Fusion
-- ============

data DelayedAcc aenv a where
  Done  :: Arrays a
        => Extend aenv aenv'
        -> PreOpenAcc OpenAcc aenv' a   -- a sub-term
        -> DelayedAcc         aenv  a

  Step  :: (Elt a, Elt b, Shape sh, Shape sh')
        => Extend aenv aenv'
        -> PreExp OpenAcc aenv' sh'
        -> PreFun OpenAcc aenv' (sh' -> sh)
        -> PreFun OpenAcc aenv' (a   -> b)
        -> Idx            aenv' (Array sh  a)
        -> DelayedAcc     aenv  (Array sh' b)

  Yield :: (Shape sh, Elt a)
        => Extend aenv aenv'
        -> PreExp OpenAcc aenv' sh
        -> PreFun OpenAcc aenv' (sh -> a)
        -> DelayedAcc     aenv  (Array sh a)


-- Fusion combinators
-- ------------------

done :: Arrays a => PreOpenAcc OpenAcc aenv a -> DelayedAcc aenv a
done a = Done (BaseEnv `PushEnv` a) (Avar ZeroIdx)

identity :: Elt a => OpenFun env aenv (a -> a)
identity = Lam . Body $ Var ZeroIdx

toIndex :: Shape sh => OpenExp env aenv sh -> OpenFun env aenv (sh -> Int)
toIndex sh = Lam . Body $ ToIndex (weakenE sh) (Var ZeroIdx)

fromIndex :: Shape sh => OpenExp env aenv sh -> OpenFun env aenv (Int -> sh)
fromIndex sh = Lam . Body $ FromIndex (weakenE sh) (Var ZeroIdx)

arrayShape :: (Shape sh, Elt e) => Idx aenv (Array sh e) -> Exp aenv sh
arrayShape = Shape . OpenAcc . Avar

indexArray :: (Shape sh, Elt e) => Idx aenv (Array sh e) -> Fun aenv (sh -> e)
indexArray v = Lam . Body $ Index (OpenAcc (Avar v)) (Var ZeroIdx)

linearIndexArray :: (Shape sh, Elt e) => Idx aenv (Array sh e) -> Fun aenv (Int -> e)
linearIndexArray v = Lam . Body $ LinearIndex (OpenAcc (Avar v)) (Var ZeroIdx)

reindex :: (Shape sh, Shape sh')
        => OpenExp env aenv sh'
        -> OpenExp env aenv sh
        -> OpenFun env aenv (sh -> sh')
reindex sh' sh
  | Just REFL <- matchOpenExp sh sh'    = identity
  | otherwise                           = fromIndex sh' `compose` toIndex sh


-- "force" a delayed array representation to produce a real AST node.
--
force :: Arrays a => DelayedAcc aenv a -> OpenAcc aenv a
force delayed
  = simplifyOpenAcc EmptyAcc . OpenAcc  -- TLM: hax
  $ case delayed of
      Done env a                                -> bind env a
      Yield env sh f                            -> bind env $ Generate sh f
      Step env sh ix f v
        | Just REFL <- s
        , Lam (Body (Var ZeroIdx)) <- ix
        , Lam (Body (Var ZeroIdx)) <- f         -> bind env $ Avar v
        | Just REFL <- s
        , Lam (Body (Var ZeroIdx)) <- ix        -> bind env $ Map f a
        | Lam (Body (Var ZeroIdx)) <- f         -> bind env $ Backpermute sh ix a
        | otherwise                             -> bind env $ Transform sh ix f a
        where
          a = OpenAcc (Avar v)
          s = matchOpenExp sh (Shape a)


-- Reshape a delayed array.
--
-- TLM: there was a runtime check to ensure the old and new shapes contained the
--      same number of elements: this has been lost!
--
reshapeD
    :: (Shape sh, Shape sh', Elt e)
    => Exp        aenv sh'
    -> DelayedAcc aenv (Array sh  e)
    -> DelayedAcc aenv (Array sh' e)
reshapeD sl acc = case acc of
  Step env sh ix f v    -> let sl' = sinkE env sl in Step env sl' (ix `compose` reindex sh sl') f v
  Yield env sh f        -> let sl' = sinkE env sl in Yield env sl' (f `compose` reindex sh sl')
  Done env a            ->
    let env'    = env `PushEnv` a
        sl'     = sinkE env' sl
        ix      = reindex (Shape (OpenAcc (Avar ZeroIdx))) sl'
    in  Step env' sl' ix identity ZeroIdx


-- Apply an index space transform that specifies where elements in the
-- destination array read their data from in the source array.
--
backpermuteD
    :: (Shape sh, Shape sh', Elt e)
    => Exp        aenv sh'
    -> Fun        aenv (sh' -> sh)
    -> DelayedAcc aenv (Array sh  e)
    -> DelayedAcc aenv (Array sh' e)
backpermuteD sh' p acc = case acc of
  Step env _ ix f a     -> Step env (sinkE env sh') (ix `compose` sinkF env p) f a
  Yield env _ f         -> Yield env (sinkE env sh') (f `compose` sinkF env p)
  Done env a            ->
    let env' = env `PushEnv` a
    in  Step env' (sinkE env' sh') (sinkF env' p) identity ZeroIdx


-- Replicate as a backwards permutation
--
replicateD
    :: forall slix sl co sh aenv e. (Shape sh, Shape sl, Elt slix, Elt e)
    => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
    -> Exp        aenv slix
    -> DelayedAcc aenv (Array sl e)
    -> DelayedAcc aenv (Array sh e)
replicateD sliceIndex slix acc = case acc of
  Step env sl pf f a    -> Step env (fullshape env sl) (pf `compose` extend env) f a
  Yield env sl f        -> Yield env (fullshape env sl) (f `compose` extend env)
  Done env a            ->
    let env' = env `PushEnv` a
        a0   = OpenAcc (Avar ZeroIdx)
    in  Step env' (fullshape env' (Shape a0)) (extend env') identity ZeroIdx
  --
  where
    fullshape :: Extend aenv aenv' -> Exp aenv' sl -> Exp aenv' sh
    fullshape env = IndexFull sliceIndex (sinkE env slix)

    extend :: Extend aenv aenv' -> Fun aenv' (sh -> sl)
    extend env = Lam (Body (IndexSlice sliceIndex (weakenE (sinkE env slix)) (Var ZeroIdx)))


-- Dimensional slice as a backwards permutation
--
sliceD
    :: forall slix sl co sh aenv e. (Shape sh, Shape sl, Elt slix, Elt e)
    => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
    -> DelayedAcc aenv (Array sh e)
    -> Exp        aenv slix
    -> DelayedAcc aenv (Array sl e)
sliceD sliceIndex acc slix = case acc of
  Step env sl pf f a    -> Step env (sliceshape env sl) (pf `compose` restrict env) f a
  Yield env sl f        -> Yield env (sliceshape env sl) (f `compose` restrict env)
  Done env a            ->
    let env' = env `PushEnv` a
        a0   = OpenAcc (Avar ZeroIdx)
    in  Step env' (sliceshape env' (Shape a0)) (restrict env') identity ZeroIdx
  --
  where
    sliceshape :: Extend aenv aenv' -> Exp aenv' sh -> Exp aenv' sl
    sliceshape env = IndexSlice sliceIndex (sinkE env slix)

    restrict :: Extend aenv aenv' -> Fun aenv' (sl -> sh)
    restrict env = Lam (Body (IndexFull sliceIndex (weakenE (sinkE env slix)) (Var ZeroIdx)))


-- Combine a unary value function to a delayed array to produce another delayed
-- array. There is some extraneous work to not introduce extra array variables
-- for things already let-bound.
--
mapD :: (Shape sh, Elt a, Elt b)
     => Fun        aenv (a -> b)
     -> DelayedAcc aenv (Array sh a)
     -> DelayedAcc     aenv (Array sh b)
mapD f acc = case acc of
  Step env sh ix g a    -> Step env sh ix (sinkF env f `compose` g) a
  Yield env sh g        -> Yield env sh (sinkF env f `compose` g)
  Done env a            -> Step (env `PushEnv` a)
                                (arrayShape ZeroIdx)
                                identity
                                (weakenFA (sinkF env f))
                                ZeroIdx


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
  Done env a1                   -> inner (env `PushEnv` a1) (arrayShape ZeroIdx) (indexArray ZeroIdx)
  Step env1 sh1 ix1 g1 a1       -> inner env1 sh1 (g1 `compose` indexArray a1 `compose` ix1)
  Yield env1 sh1 g1             -> inner env1 sh1 g1
  where
    inner :: Extend aenv aenv'
          -> Exp        aenv' sh
          -> Fun        aenv' (sh -> a)
          -> DelayedAcc aenv  (Array sh c)
    inner env1 sh1 g1 = case delayOpenAcc (sinkA env1 acc2) of
      Done env2 a2 -> let env = join env1 env2 `PushEnv` a2
                      in  Yield env (weakenEA (sinkE env2 sh1) `Intersect` arrayShape ZeroIdx)
                                    (generate (sinkF env f)
                                              (weakenFA (sinkF env2 g1))
                                              (indexArray ZeroIdx))

      Step env2 sh2 ix2 g2 a2
        -> let env = join env1 env2
           in  Yield env (sinkE env2 sh1 `Intersect` sh2)
                         (generate (sinkF env f) (sinkF env2 g1) (g2 `compose` indexArray a2 `compose` ix2))

      Yield env2 sh2 g2
        -> let env = join env1 env2
           in  Yield env (sinkE env2 sh1 `Intersect` sh2)
                         (generate (sinkF env f) (sinkF env2 g1) g2)

    substitute2
        :: OpenExp ((env, a), b) aenv' c
        -> OpenExp (env, dim)    aenv' a
        -> OpenExp (env, dim)    aenv' b
        -> OpenExp (env, dim)    aenv' c
    substitute2 gen a b
      = Let a
      $ Let (weakenE b)             -- as 'b' has been pushed under a binder
      $ rebuildE split2 gen         -- add space for the index environment variable
      where
        split2 :: Elt t => Idx ((env,a),b) t -> PreOpenExp acc (((env,dim),a),b) aenv' t
        split2 ZeroIdx                = Var ZeroIdx
        split2 (SuccIdx ZeroIdx)      = Var (SuccIdx ZeroIdx)
        split2 (SuccIdx (SuccIdx ix)) = Var (SuccIdx (SuccIdx (SuccIdx ix)))

    generate
        :: OpenFun env aenv' (a -> b -> c)
        -> OpenFun env aenv' (dim -> a)
        -> OpenFun env aenv' (dim -> b)
        -> OpenFun env aenv' (dim -> c)
    generate (Lam (Lam (Body e))) (Lam (Body a)) (Lam (Body b)) = Lam . Body $ substitute2 e a b
    generate _                    _              _              = error "generate: impossible evaluation"



-- Sometimes it is necessary to fuse past array bindings. See comments below.
--
aletD :: (Arrays a, Arrays b)
      => OpenAcc    aenv      a
      -> OpenAcc    (aenv, a) b
      -> DelayedAcc aenv      b
aletD bndAcc bodyAcc =
  case delayOpenAcc bndAcc of
    -- If the binding is marked as "done" (i.e. needs to be computed now), add
    -- it to the extended environment of the delayed body and continue. Since
    -- manifest arrays also fall into this category, this elegantly handles
    -- let-floating.
    --
    Done env1 a
     -> case delayOpenAcc (sinkA1 env1 bodyAcc) of
          Done env2 b                   -> Done  (env1 `join` a `cons` env2) b
          Step env2 sh2 ix2 f2 b        -> Step  (env1 `join` a `cons` env2) sh2 ix2 f2 b
          Yield env2 sh2 f2             -> Yield (env1 `join` a `cons` env2) sh2 f2

    -- If instead the binding is still in a delayed state, we might be able to
    -- fuse it directly into the body. For example, functions such as reverse
    -- and transpose:
    --
    --   reverse xs = backpermute (shape xs) (\i -> length xs - i - 1) xs
    --
    -- These generate a let binding for the input array because it is required
    -- for both its data and shape information. However, if the data is only
    -- used once within the body, we can still fuse the two together because we
    -- can generate the shape directly.
    --
    Step env1 sh1 ix1 f1 a1
     -> let OpenAcc bnd = force $ Step BaseEnv sh1 ix1 f1 a1
        in case delayOpenAcc (sinkA1 env1 bodyAcc) of
          Done env2 b
           -> into (env1 `join` bnd `cons` env2) env2 sh1 (f1 `compose` indexArray a1 `compose` ix1) b

          Step env2 sh2 ix2 f2 a2
           -> fromMaybe (Step (env1 `join` bnd `cons` env2) sh2 ix2 f2 a2)
                        (yield env1 env2 sh1 sh2 (f1 `compose` indexArray a1 `compose` ix1)
                                                 (f2 `compose` indexArray a2 `compose` ix2))

          Yield env2 sh2 f2
           -> fromMaybe (Yield (env1 `join` bnd `cons` env2) sh2 f2)
                        (yield env1 env2 sh1 sh2 (f1 `compose` indexArray a1 `compose` ix1) f2)

    Yield env1 sh1 f1
     -> let OpenAcc bnd = force $ Yield BaseEnv sh1 f1
        in case delayOpenAcc (sinkA1 env1 bodyAcc) of
          Done env2 b
           -> into (env1 `join` bnd `cons` env2) env2 sh1 f1 b

          Step env2 sh2 ix2 f2 a2
           -> fromMaybe (Step (env1 `join` bnd `cons` env2) sh2 ix2 f2 a2)
                        (yield env1 env2 sh1 sh2 f1 (f2 `compose` indexArray a2 `compose` ix2))

          Yield env2 sh2 f2
           -> fromMaybe (Yield (env1 `join` bnd `cons` env2) sh2 f2)
                        (yield env1 env2 sh1 sh2 f1 f2)

  where
    -- When does the cost of re-computation out weight global memory access? For
    -- the moment only do the substitution on a single use of the bound array,
    -- but it is likely advantageous to be far more aggressive here.
    --
    lIMIT = 1

    -- Eliminating a let binding pushes the binding subject into the body as a
    -- scalar shape and generator function, producing a delayed Yield node.
    --
    yield :: (Shape sh, Shape sh', Elt e, Elt e')
          => Extend aenv                aenv'
          -> Extend (aenv', Array sh e) aenv''
          -> Exp aenv'  sh
          -> Exp aenv'' sh'
          -> Fun aenv'  (sh -> e)
          -> Fun aenv'' (sh' -> e')
          -> Maybe (DelayedAcc aenv (Array sh' e'))
    yield env1 env2 sh1 sh2 f1 f2
      | usesOfEA a0 sh2 + usesOfFA a0 f2 + usesOfAX a0 env2 <= lIMIT
      = Just $ Yield (env1 `join` env2') (replaceE sh1' f1' a0 sh2) (replaceF sh1' f1' a0 f2)

      | otherwise
      = Nothing
      where
        -- If we do the merge, 'bnd' becomes dead code and will be later
        -- eliminated by the shrinking step.
        --
        OpenAcc bnd     = force $ Yield BaseEnv sh1 f1
        a0              = sink env2 ZeroIdx
        env2'           = bnd `cons` env2
        sh1'            = sinkE env2' sh1
        f1'             = sinkF env2' f1

    -- If the body is forward permutation, we might be able to fuse into the
    -- shape and index transformation. See radix sort for an example.
    --
    into :: (Shape sh, Elt e, Arrays a)
         => Extend aenv                aenv''
         -> Extend (aenv', Array sh e) aenv''
         -> Exp aenv' sh
         -> Fun aenv' (sh -> e)
         -> PreOpenAcc OpenAcc aenv'' a
         -> DelayedAcc         aenv   a
    into env env2 sh1 f1 body
      | Permute c2 d2 ix2 s2 <- body
      , usesOfFA a0 c2 + usesOfFA a0 ix2 + usesOfAX a0 env2 + usesOf a0 d2 + usesOf a0 s2 <= lIMIT
      = Done env $ Permute (replaceF sh1' f1' a0 c2) d2 (replaceF sh1' f1' a0 ix2) s2

      | otherwise
      = Done env body
      where
        a0      = sink  env2 ZeroIdx
        sh1'    = sinkE env2 $ weakenEA sh1
        f1'     = sinkF env2 $ weakenFA f1

    -- Count the number of uses of an array variable. This is specialised from
    -- the procedure for shrinking in that we ignore uses that occur as part of
    -- a Shape.
    --
    usesOfAX :: Idx aenv' a -> Extend (aenv, a) aenv' -> Int
    usesOfAX _             BaseEnv         = 0
    usesOfAX (SuccIdx idx) (PushEnv env a) = usesOfPA idx a + usesOfAX idx env
    usesOfAX _             _               = error "usesOfAExt: inconsistent valuation"

    usesOf :: Idx aenv s -> OpenAcc aenv t -> Int
    usesOf idx (OpenAcc acc) = usesOfPA idx acc

    usesOfPA :: Idx aenv s -> PreOpenAcc OpenAcc aenv t -> Int
    usesOfPA idx acc =
      case acc of
        Alet bnd body       -> usesOf idx bnd + usesOf (SuccIdx idx) body
        Avar idx'
          | Just REFL <- matchIdx idx idx'  -> 1
          | otherwise                       -> 0
        Atuple tup          -> usesOfATA idx tup
        Aprj _ a            -> usesOf idx a
        Apply _ a           -> usesOf idx a
        Acond p t e         -> usesOfEA idx p + usesOf idx t + usesOf idx e
        Use _               -> 0
        Unit e              -> usesOfEA idx e
        Reshape e a         -> usesOfEA idx e + usesOf idx a
        Generate e f        -> usesOfEA idx e + usesOfFA idx f
        Transform sh ix f a -> usesOfEA idx sh + usesOfFA idx ix + usesOfFA idx f + usesOf idx a
        Replicate _ slix a  -> usesOfEA idx slix + usesOf idx a
        Slice _ a slix      -> usesOfEA idx slix + usesOf idx a
        Map f a             -> usesOfFA idx f + usesOf idx a
        ZipWith f a1 a2     -> usesOfFA idx f + usesOf idx a1 + usesOf idx a2
        Fold f z a          -> usesOfFA idx f + usesOfEA idx z + usesOf idx a
        Fold1 f a           -> usesOfFA idx f + usesOf idx a
        FoldSeg f z a s     -> usesOfFA idx f + usesOfEA idx z + usesOf idx a + usesOf idx s
        Fold1Seg f a s      -> usesOfFA idx f + usesOf idx a + usesOf idx s
        Scanl f z a         -> usesOfFA idx f + usesOfEA idx z + usesOf idx a
        Scanl' f z a        -> usesOfFA idx f + usesOfEA idx z + usesOf idx a
        Scanl1 f a          -> usesOfFA idx f + usesOf idx a
        Scanr f z a         -> usesOfFA idx f + usesOfEA idx z + usesOf idx a
        Scanr' f z a        -> usesOfFA idx f + usesOfEA idx z + usesOf idx a
        Scanr1 f a          -> usesOfFA idx f + usesOf idx a
        Permute f1 a1 f2 a2 -> usesOfFA idx f1 + usesOf idx a1 + usesOfFA idx f2 + usesOf idx a2
        Backpermute sh f a  -> usesOfEA idx sh + usesOfFA idx f + usesOf idx a
        Stencil f _ a       -> usesOfFA idx f + usesOf idx a
        Stencil2 f _ a1 _ a2-> usesOfFA idx f + usesOf idx a1 + usesOf idx a2
        Foreign _ _ a       -> usesOf idx a

    usesOfATA :: Idx aenv s -> Atuple (OpenAcc aenv) t -> Int
    usesOfATA idx atup =
      case atup of
        NilAtup      -> 0
        SnocAtup t a -> usesOfATA idx t + usesOf idx a

    usesOfEA :: Idx aenv a -> OpenExp env aenv t -> Int
    usesOfEA idx exp =
      case exp of
        Let bnd body        -> usesOfEA idx bnd + usesOfEA idx body
        Var _               -> 0
        Const _             -> 0
        Tuple t             -> usesOfTA idx t
        Prj _ e             -> usesOfEA idx e
        IndexNil            -> 0
        IndexCons sl sz     -> usesOfEA idx sl + usesOfEA idx sz
        IndexHead sh        -> usesOfEA idx sh
        IndexTail sh        -> usesOfEA idx sh
        IndexSlice _ ix sh  -> usesOfEA idx ix + usesOfEA idx sh
        IndexFull _ ix sl   -> usesOfEA idx ix + usesOfEA idx sl
        IndexAny            -> 0
        ToIndex sh ix       -> usesOfEA idx sh + usesOfEA idx ix
        FromIndex sh i      -> usesOfEA idx sh + usesOfEA idx i
        Cond p t e          -> usesOfEA idx p  + usesOfEA idx t  + usesOfEA idx e
        Iterate n f x       -> usesOfEA idx n  + usesOfEA idx f  + usesOfEA idx x
        PrimConst _         -> 0
        PrimApp _ x         -> usesOfEA idx x
        ShapeSize sh        -> usesOfEA idx sh
        Intersect sh sz     -> usesOfEA idx sh + usesOfEA idx sz
        --
        -- Special case: Because we are looking to fuse two array computations
        -- together, it is not necessary to consider the contribution of Shape since
        -- this would be replaced with a simple scalar expression.
        --
        Index a sh          -> usesOf idx a + usesOfEA idx sh
        LinearIndex a i     -> usesOf idx a + usesOfEA idx i
        Shape _             -> 0

    usesOfTA :: Idx aenv a -> Tuple (OpenExp env aenv) t -> Int
    usesOfTA idx tup =
      case tup of
        NilTup      -> 0
        SnocTup t e -> usesOfTA idx t + usesOfEA idx e

    usesOfFA :: Idx aenv a -> OpenFun env aenv f -> Int
    usesOfFA idx fun =
      case fun of
        Body e      -> usesOfEA idx e
        Lam f       -> usesOfFA idx f

    -- Substitute shape and array indexing with scalar functions at the given
    -- array index.
    --
    replaceF :: (Shape sh, Elt e)
             => OpenExp env' aenv sh
             -> OpenFun env' aenv (sh -> e)
             -> Idx          aenv (Array sh e)
             -> OpenFun env' aenv f
             -> OpenFun env' aenv f
    replaceF sh ix idx fun =
      case fun of
        Body e      -> Body (replaceE sh ix idx e)
        Lam f       -> Lam  (replaceF (weakenE sh) (weakenFE ix) idx f)

    replaceE
        :: forall sh e t env aenv. (Shape sh, Elt e)
        => OpenExp env aenv sh
        -> OpenFun env aenv (sh -> e)
        -> Idx         aenv (Array sh e)
        -> OpenExp env aenv t
        -> OpenExp env aenv t
    replaceE sh_ ix_ idx exp =
      let travE :: OpenExp env aenv t' -> OpenExp env aenv t'
          travE = replaceE sh_ ix_ idx

          travT :: Tuple (OpenExp env aenv) t' -> Tuple (OpenExp env aenv) t'
          travT NilTup        = NilTup
          travT (SnocTup t e) = travT t `SnocTup` travE e

      in case exp of
        Let bnd body        -> Let (travE bnd) (replaceE (weakenE sh_) (weakenFE ix_) idx body)
        Var i               -> Var i
        Const c             -> Const c
        Tuple t             -> Tuple (travT t)
        Prj ix e            -> Prj ix (travE e)
        IndexNil            -> IndexNil
        IndexCons sl sz     -> IndexCons (travE sl) (travE sz)
        IndexHead sh        -> IndexHead (travE sh)
        IndexTail sz        -> IndexTail (travE sz)
        IndexAny            -> IndexAny
        IndexSlice x ix sh  -> IndexSlice x (travE ix) (travE sh)
        IndexFull x ix sl   -> IndexFull x (travE ix) (travE sl)
        ToIndex sh ix       -> ToIndex (travE sh) (travE ix)
        FromIndex sh i      -> FromIndex (travE sh) (travE i)
        Cond p t e          -> Cond (travE p) (travE t) (travE e)
        Iterate n f x       -> Iterate (travE n) (replaceE (weakenE sh_) (weakenFE ix_) idx f) (travE x)
        PrimConst c         -> PrimConst c
        PrimApp g x         -> PrimApp g (travE x)
        ShapeSize sh        -> ShapeSize (travE sh)
        Intersect sh sl     -> Intersect (travE sh) (travE sl)
        Shape (OpenAcc a)
          | Avar idx'       <- a
          , Just REFL       <- matchIdx idx idx'
          -> sh_

          | otherwise
          -> exp
        --
        Index (OpenAcc a) sh
          | Avar idx'       <- a
          , Just REFL       <- matchIdx idx idx'
          , Lam (Body f)    <- ix_
          -> Let sh f

          | otherwise
          -> Index (OpenAcc a) (travE sh)
        --
        LinearIndex (OpenAcc a) i
          | Avar idx'       <- a
          , Just REFL       <- matchIdx idx idx'
          , Lam (Body f)    <- ix_
          -> Let (Let i (FromIndex (weakenE sh_) (Var ZeroIdx))) f

          | otherwise
          -> LinearIndex (OpenAcc a) (travE i)


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
  BaseEnv ::                                                    Extend aenv aenv
  PushEnv :: Arrays a
          => Extend aenv aenv' -> PreOpenAcc OpenAcc aenv' a -> Extend aenv (aenv', a)


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
sink :: Extend env env'
     -> Idx env  t
     -> Idx env' t
sink BaseEnv       = id
sink (PushEnv e _) = SuccIdx . sink e

sinkA :: Extend aenv aenv'
      -> OpenAcc aenv  a
      -> OpenAcc aenv' a
sinkA env = weakenByA (sink env)

sinkE :: Extend aenv aenv'
      -> OpenExp env aenv  e
      -> OpenExp env aenv' e
sinkE env = weakenByEA (sink env)

sinkF :: Extend aenv aenv'
      -> OpenFun env aenv  f
      -> OpenFun env aenv' f
sinkF env = weakenByFA (sink env)


sink1 :: Extend env env'
      -> Idx (env, a) t
      -> Idx (env',a) t
sink1 BaseEnv       = id
sink1 (PushEnv e _) = split1 . sink1 e
  where
    split1 :: Idx (env, a) t -> Idx ((env, s), a) t
    split1 ZeroIdx       = ZeroIdx
    split1 (SuccIdx idx) = SuccIdx (SuccIdx idx)

sinkA1 :: Extend aenv aenv'
       -> OpenAcc (aenv,  a) b
       -> OpenAcc (aenv', a) b
sinkA1 env = weakenByA (sink1 env)


-- Manipulating environments
--
infixr `cons`
infixr `join`

cons :: Arrays a => PreOpenAcc OpenAcc aenv a -> Extend (aenv,a) aenv' -> Extend aenv aenv'
cons a = join (BaseEnv `PushEnv` a)

join :: Extend env env' -> Extend env' env'' -> Extend env env''
join x BaseEnv        = x
join x (PushEnv xs a) = join x xs `PushEnv` a

