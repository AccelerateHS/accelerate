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
import Data.Maybe

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo.Simplify
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Array.Representation       ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                ( Arrays, Array, Elt, EltRepr, Shape )
import Data.Array.Accelerate.Tuple


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
    -- Forms that introduce environment manipulations and control flow. These
    -- stable points of the expression we generally don't want to fuse past.
    --
    Alet bnd body       -> aletD bnd body
    Avar ix             -> done $ Avar ix
    Atuple arrs         -> done $ Atuple (fuseAtuple arrs)
    Aprj ix arrs        -> done $ Aprj ix (fuseOpenAcc arrs)
    Apply f a           -> done $ Apply (fuseOpenAfun f) (fuseOpenAcc a)
    Acond p acc1 acc2   -> done $ Acond (cvtE p) (fuseOpenAcc acc1) (fuseOpenAcc acc2)

    -- Array injection
    --
    Use arrs            -> done $ Use arrs
    Unit e              -> done $ Unit e

    -- Index space transforms
    --
    Reshape sl acc
      -> let sh' = cvtE sl
         in case cvt acc of
           -- TLM: there was a runtime check to ensure the old and new shapes
           -- contained the same number of elements: this has been lost!
           --
           Done env a           -> Done env $ Reshape (sinkE env sh') (OpenAcc a)
           Step env sh ix f a   -> let shx = sinkE env sh' in Step env shx (ix `compose` reindex sh shx) f a
           Yield env sh f       -> let shx = sinkE env sh' in Yield env shx (f `compose` reindex sh shx)

    Replicate slix sh a -> replicateD slix (cvtE sh) (cvt a)
    Index slix a sh     -> indexD slix (cvt a) (cvtE sh)
    Backpermute sl p a  -> backpermuteD (cvtE sl) (cvtF p) (cvt a)

    -- Producers
    --
    Generate sh f       -> Yield BaseEnv (cvtE sh) (cvtF f)
    Map f a             -> mapD (cvtF f) (cvt a)
    ZipWith f a1 a2     -> zipWithD (cvtF f) a1 a2
    Transform sl p f a  -> backpermuteD (cvtE sl) (cvtF p) $ mapD (cvtF f) (cvt a)

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
    cvtA :: OpenAcc aenv a -> OpenAcc aenv a
    cvtA = fuseOpenAcc

    cvtF :: OpenFun env aenv t -> OpenFun env aenv t
    cvtF = fuseOpenFun

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
      Iterate n f x             -> Iterate n (cvtF f) (cvt x)
      PrimConst c               -> PrimConst c
      PrimApp f x               -> PrimApp f (cvt x)
      IndexScalar a sh          -> IndexScalar (cvtA a) (cvt sh)
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

reindex :: (Shape sh, Shape sh')
        => OpenExp env aenv sh'
        -> OpenExp env aenv sh
        -> OpenFun env aenv (sh -> sh')
reindex sh' sh
  | Just REFL <- matchOpenExp sh sh'
  = Lam . Body $ Var ZeroIdx

  | otherwise
  = fromIndex sh' `compose` toIndex sh


-- "force" a delayed array representation to produce a real AST node.
--
force :: DelayedAcc aenv a -> OpenAcc aenv a
force delayed
  = simplifyOpenAcc . OpenAcc
  $ case delayed of
      Done env a                                -> bind env a
      Yield env sh f                            -> bind env $ Generate sh f
      Step env sh ix f a
        | Just REFL <- matchOpenExp sh (Shape a')
        , Lam (Body (Var ZeroIdx)) <- ix        -> bind env $ Map f a'
        | Lam (Body (Var ZeroIdx)) <- f         -> bind env $ Backpermute sh ix a'
        | otherwise                             -> bind env $ Transform sh ix f a'
        where
          a'    = OpenAcc a


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
  Done env a            -> Step env (sinkE env sh') (sinkF env p) identity a


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
        a0   = Avar ZeroIdx
    in  Step env' (fullshape env' (Shape (OpenAcc a0))) (extend env') identity a0
  --
  where
    fullshape :: Extend aenv aenv' -> Exp aenv' sl -> Exp aenv' sh
    fullshape env sl = IndexFull sliceIndex (sinkE env slix) sl

    extend :: Extend aenv aenv' -> Fun aenv' (sh -> sl)
    extend env = Lam (Body (IndexSlice sliceIndex (weakenE (sinkE env slix)) (Var ZeroIdx)))


-- Dimensional slice as a backwards permutation
--
indexD
    :: forall slix sl co sh aenv e. (Shape sh, Shape sl, Elt slix, Elt e)
    => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
    -> DelayedAcc aenv (Array sh e)
    -> Exp        aenv slix
    -> DelayedAcc aenv (Array sl e)
indexD sliceIndex acc slix = case acc of
  Step env sl pf f a    -> Step env (sliceshape env sl) (pf `compose` restrict env) f a
  Yield env sl f        -> Yield env (sliceshape env sl) (f `compose` restrict env)
  Done env a            ->
    let env' = env `PushEnv` a
        a0   = Avar ZeroIdx
    in  Step env' (sliceshape env' (Shape (OpenAcc a0))) (restrict env') identity a0
  --
  where
    sliceshape :: Extend aenv aenv' -> Exp aenv' sh -> Exp aenv' sl
    sliceshape env sh = IndexSlice sliceIndex (sinkE env slix) sh

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
    -> inner (env `PushEnv` a1) (shape (Avar ZeroIdx)) (index (Avar ZeroIdx))

  Step env1 sh1 ix1 g1 a1
    -> inner (env1 `PushEnv` a1)
             (weakenEA sh1)
             (weakenFA g1 `compose` index (Avar ZeroIdx) `compose` weakenFA ix1)

  Yield env1 sh1 g1
    -> inner env1 sh1 g1
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
        -> let env = join env1 env2 `PushEnv` a2
           in  Yield env (weakenEA (sinkE env2 sh1) `Intersect` shape (Avar ZeroIdx))
                         (generate (sinkF env f)
                                   (weakenFA (sinkF env2 g1))
                                   (index (Avar ZeroIdx)))

      Step env2 sh2 ix2 g2 a2
        -> let env = join env1 env2 `PushEnv` a2
           in  Yield env (weakenEA (sinkE env2 sh1 `Intersect` sh2))
                         (generate (sinkF env f)
                                   (weakenFA (sinkF env2 g1))
                                   (weakenFA g2 `compose` index (Avar ZeroIdx) `compose` weakenFA ix2))

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
           Done env2 b                  -> Done  (env1 `join` a `cons` env2) b
           Step env2 sh2 ix2 f2 b       -> Step  (env1 `join` a `cons` env2) sh2 ix2 f2 b
           Yield env2 sh2 f2            -> Yield (env1 `join` a `cons` env2) sh2 f2

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
             -> Done (env1 `join` bnd `cons` env2) b

           Step env2 sh2 ix2 f2 a2
             -> fromMaybe (Step (env1 `join` bnd `cons` env2) sh2 ix2 f2 a2)
                          (inner env1 env2 sh1 sh2 (f1 `compose` index a1 `compose` ix1)
                                                   (f2 `compose` index a2 `compose` ix2))

           Yield env2 sh2 f2
             -> fromMaybe (Yield (env1 `join` bnd `cons` env2) sh2 f2)
                          (inner env1 env2 sh1 sh2 (f1 `compose` index a1 `compose` ix1) f2)

    Yield env1 sh1 f1
      -> let OpenAcc bnd = force $ Yield BaseEnv sh1 f1
         in case delayOpenAcc (sinkA1 env1 bodyAcc) of
           Done env2 b
             -> Done (env1 `join` bnd `cons` env2) b

           Step env2 sh2 ix2 f2 b
             -> fromMaybe (Step (env1 `join` bnd `cons` env2) sh2 ix2 f2 b)
                          (inner env1 env2 sh1 sh2 f1 (f2 `compose` index b `compose` ix2))

           Yield env2 sh2 f2
             -> fromMaybe (Yield (env1 `join` bnd `cons` env2) sh2 f2)
                          (inner env1 env2 sh1 sh2 f1 f2)

  where
    index :: (Shape sh, Elt e) => PreOpenAcc OpenAcc aenv (Array sh e) -> Fun aenv (sh -> e)
    index a = Lam . Body $ IndexScalar (OpenAcc a) (Var ZeroIdx)

    inner :: (Shape sh, Shape sh', Elt e, Elt e')
          => Extend aenv                aenv'
          -> Extend (aenv', Array sh e) aenv''
          -> Exp aenv'  sh
          -> Exp aenv'' sh'
          -> Fun aenv'  (sh  -> e)
          -> Fun aenv'' (sh' -> e')
          -> Maybe (DelayedAcc aenv (Array sh' e'))
    inner env1 env2 sh1 sh2 f1 f2
      | usesOfAX a0 env2 + usesOfEA a0 sh2 + usesOfFA a0 f2 <= lIMIT
      = Just $ Yield (env1 `join` env2') (replaceE sh1' f1' a0 sh2) (replaceF sh1' f1' a0 f2)

      | otherwise
      = Nothing
      where
        -- When does the cost of re-computation out weight global memory access? For
        -- the moment only do the substitution on a single use of the bound array,
        -- but it is likely advantageous to be far more aggressive here.
        --
        lIMIT :: Int
        lIMIT = 1

        OpenAcc bnd     = force $ Yield BaseEnv sh1 f1          -- will be eliminated by shrinking
        a0              = sink env2 ZeroIdx
        env2'           = bnd `cons` env2
        sh1'            = sinkE env2' sh1
        f1'             = sinkF env2' f1


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
        Index _ a slix      -> usesOfEA idx slix + usesOf idx a
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
        Iterate _ f x       -> usesOfFA idx f  + usesOfEA idx x
        PrimConst _         -> 0
        PrimApp _ x         -> usesOfEA idx x
        ShapeSize sh        -> usesOfEA idx sh
        Intersect sh sz     -> usesOfEA idx sh + usesOfEA idx sz
        --
        -- Special case: Because we are looking to fuse two array computations
        -- together, it is not necessary to consider the contribution of Shape since
        -- this would be replaced with a simple scalar expression.
        --
        IndexScalar a sh    -> usesOf idx a + usesOfEA idx sh
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
        Iterate n f x       -> Iterate n (replaceF sh_ ix_ idx f) (travE x)
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
        IndexScalar (OpenAcc a) sh
          | Avar idx'       <- a
          , Just REFL       <- matchIdx idx idx'
          , Lam (Body f)    <- ix_
          -> Let sh f

          | otherwise
          -> IndexScalar (OpenAcc a) (travE sh)


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
    split1 ZeroIdx      = ZeroIdx
    split1 (SuccIdx ix) = SuccIdx (SuccIdx ix)

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



{--
-- We use a type family to represent how to concatenate environments, so we can
-- separate the base environment (aenv) from just the part that extends it
-- (aenv'). In this way, we can manipulate both the head and tail of the
-- extended environment, and are not limited to simply putting more things onto
-- the end.
--
type family Cat env env' :: *
type instance Cat env ()       = env
type instance Cat env (env',s) = (Cat env env', s)


cat :: Extend env env' -> Extend env env'' -> Extend env (Cat env' env'')
cat x BaseEnv       = x
cat x (PushEnv e a) = cat x e `PushEnv` split x e a
  where
    split :: Extend env env'
          -> Extend env env''
          -> PreOpenAcc OpenAcc (Cat env env'')            a
          -> PreOpenAcc OpenAcc (Cat env (Cat env' env'')) a
    split env1 env2 = rebuildA rebuildOpenAcc (Avar . open env1 env2)

    open :: Extend env env'
         -> Extend env env''
         -> Idx (Cat env env'')            t
         -> Idx (Cat env (Cat env' env'')) t
    open e BaseEnv       ix           = sink e ix
    open _ (PushEnv _ _) ZeroIdx      = ZeroIdx
    open e (PushEnv n _) (SuccIdx ix) = SuccIdx (open e n ix)
--}

