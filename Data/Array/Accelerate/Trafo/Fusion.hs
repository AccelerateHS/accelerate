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
import Data.Array.Accelerate.Array.Representation       ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                ( Arrays, Array, Elt, EltRepr, Shape )
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
    -- As always, there are exceptions:
    --
    --  1) let bound nodes of manifest arrays are allowed to float
    --
    --  2) generate-in-generate forms might be fused together, if the bound
    --     generator is only used once within the body.
    --
    Alet bndAcc bodyAcc ->
      let OpenAcc bnd   = fuseOpenAcc bndAcc
          OpenAcc body  = fuseOpenAcc bodyAcc
      in
      case bnd of
        Use _                   -> float bnd (cvt $ OpenAcc body)
        Unit _                  -> float bnd (cvt $ OpenAcc body)
        Generate sh1 f1
          | Generate sh2 f2 <- body
          , Just acc        <- fuseGenInGen sh1 f1 sh2 f2
                                -> acc
        _                       -> done $ Alet (OpenAcc bnd) (OpenAcc body)

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

reindex :: (Shape sh, Shape sh')
        => OpenExp env aenv sh'
        -> OpenExp env aenv sh
        -> OpenFun env aenv (sh -> sh')
reindex sh' sh
  | Just REFL <- matchOpenExp sh sh'
  = Lam . Body $ Var ZeroIdx

  | otherwise
  = fromIndex sh' `compose` toIndex sh


intersect :: Shape sh => OpenExp env aenv sh -> OpenExp env aenv sh -> OpenExp env aenv sh
intersect sh1 sh2
  | Just REFL <- matchOpenExp sh1 sh2   = sh1
  | otherwise                           = Intersect sh1 sh2


-- "force" a delayed array representation to produce a real AST node.
--
force :: DelayedAcc aenv a -> OpenAcc aenv a
force delayed = OpenAcc $ case delayed of
  Done env a                                    -> bind env a
  Yield env sh f                                -> bind env $ Generate (simplifyExp sh) (simplifyFun f)
  Step env sh ix f a
   | Lam (Body (Var ZeroIdx)) <- ix
   , Just REFL <- matchOpenExp sh (Shape a')    -> bind env $ Map f' a'
   | Lam (Body (Var ZeroIdx)) <- f              -> bind env $ Backpermute sh' ix' a'
   | otherwise                                  -> bind env $ Transform sh' ix' f' a'
   where
     a'  = OpenAcc a
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
  Done env a
    | Avar _ <- a       -> Step env (fullshape env (Shape (OpenAcc a))) (extend env) identity a
    | otherwise         ->
        let env' = env `PushEnv` a
            a0   = Avar ZeroIdx
        in  Step env' (fullshape env' (Shape (OpenAcc a0))) (extend env') identity a0

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
  Done env a
    | Avar _ <- a       -> Step env (sliceshape env (Shape (OpenAcc a))) (restrict env) identity a
    | otherwise         ->
        let env' = env `PushEnv` a
            a0   = Avar ZeroIdx
        in  Step env' (sliceshape env' (Shape (OpenAcc a0))) (restrict env') identity a0

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
                          (generate (sinkF env f)
                                    (weakenFA (sinkF env2 g1))
                                    (index (Avar ZeroIdx)))

      Step env2 sh2 ix2 g2 a2
        | Avar _ <- a2  ->
            let env = cat env1 env2
            in  Yield env (sinkE env2 sh1 `intersect` sh2)
                          (generate (sinkF env f)
                                    (sinkF env2 g1)
                                    (g2 `compose` index a2 `compose` ix2))
        | otherwise     ->
            let env = cat env1 env2 `PushEnv` a2
            in  Yield env (weakenEA (sinkE env2 sh1 `intersect` sh2))
                          (generate (sinkF env f)
                                    (weakenFA (sinkF env2 g1))
                                    (weakenFA g2 `compose` index (Avar ZeroIdx) `compose` weakenFA ix2))

      Yield env2 sh2 g2
        -> let env = cat env1 env2
           in  Yield env (sinkE env2 sh1 `intersect` sh2)
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



-- Combine two generator functions. We need to do a couple of things here:
--
--  1) Count the number of occurrences of a0 in the body computation. If there
--     are "too many", don't do the substitution.
--
--  2) Replace occurrences of indexing into the bound array with the generator
--     function for that array.
--
--  3) Erase the (now unused) top array variable, shrinking the environment.
--
--
-- TLM: need to generalise this to all producer-type functions, where the
--      binding is used only once in the body.
--
fuseGenInGen
    :: (Shape sh, Shape sh', Elt a, Elt b)
    => Exp aenv sh
    -> Fun aenv (sh -> a)
    -> Exp (aenv, Array sh a) sh'
    -> Fun (aenv, Array sh a) (sh' -> b)
    -> Maybe (DelayedAcc aenv (Array sh' b))
fuseGenInGen sh1 f1 sh2 f2
  | countEA v0 sh2 <= lIMIT && countFA v0 f2 <= lIMIT
  , countEA v0 sh' == 0     && countFA v0 f' == 0
  = Just $ Yield BaseEnv (rebuildEA rebuildOpenAcc eraseTop sh')
                         (rebuildFA rebuildOpenAcc eraseTop f')

  | otherwise
  = Nothing
  where
    -- When does the cost of re-computation out weight global memory access? For
    -- the moment only do the substitution on a single use of the bound array,
    -- but it is likely advantageous to be far more aggressive here.
    --
    lIMIT       = 1

    -- The new shape and generator functions, before the top variable is erased
    --
    v0          = OpenAcc (Avar ZeroIdx)
    sh'         = replaceE (Shape v0) (weakenEA sh1) sh2
    f'          = replaceIF f1 f2

    eraseTop :: Arrays t => Idx (aenv, s) t -> PreOpenAcc OpenAcc aenv t
    eraseTop ZeroIdx      = error "IT'S CLOBBERIN' TIME!"
    eraseTop (SuccIdx ix) = Avar ix

    -- Replace the indexing function with a value generator
    --
    replaceIF
        :: (Shape sh, Elt a)
        => OpenFun env aenv (sh -> a)
        -> OpenFun env (aenv, Array sh a) t
        -> OpenFun env (aenv, Array sh a) t
    replaceIF gen fun = case fun of
      Lam f         -> Lam (replaceIF (weakenFE gen) f)
      Body e        -> Body (replaceIE gen e)
      where
        replaceIT :: (Shape sh, Elt a)
                  => OpenFun env aenv (sh -> a)
                  -> Tuple.Tuple (OpenExp env (aenv, Array sh a)) t
                  -> Tuple.Tuple (OpenExp env (aenv, Array sh a)) t
        replaceIT ix tup = case tup of
          NilTup            -> NilTup
          SnocTup t e       -> replaceIT ix t `SnocTup` replaceIE ix e

        replaceIE :: (Shape sh, Elt a)
                  => OpenFun env aenv (sh -> a)
                  -> OpenExp env (aenv, Array sh a) e
                  -> OpenExp env (aenv, Array sh a) e
        replaceIE ix exp = case exp of
          Let bnd body          -> Let (replaceIE ix bnd) (replaceIE (weakenFE ix) body)
          Var i                 -> Var i
          Const c               -> Const c
          Tuple t               -> Tuple (replaceIT ix t)
          Prj i e               -> Prj i (replaceIE ix e)
          IndexNil              -> IndexNil
          IndexCons sl sz       -> IndexCons (replaceIE ix sl) (replaceIE ix sz)
          IndexHead sh          -> IndexHead (replaceIE ix sh)
          IndexTail sh          -> IndexTail (replaceIE ix sh)
          IndexAny              -> IndexAny
          IndexSlice x slix sh  -> IndexSlice x (replaceIE ix slix) (replaceIE ix sh)
          IndexFull x slix sl   -> IndexFull x (replaceIE ix slix) (replaceIE ix sl)
          ToIndex sh i          -> ToIndex (replaceIE ix sh) (replaceIE ix i)
          FromIndex sh i        -> FromIndex (replaceIE ix sh) (replaceIE ix i)
          Cond p t e            -> Cond (replaceIE ix p) (replaceIE ix t) (replaceIE ix e)
          Iterate n r x         -> Iterate n (replaceIF ix r) (replaceIE ix x)
          PrimConst c           -> PrimConst c
          PrimApp p x           -> PrimApp p (replaceIE ix x)
          Shape acc             -> Shape acc
          ShapeSize sh          -> ShapeSize (replaceIE ix sh)
          Intersect sh sl       -> Intersect (replaceIE ix sh) (replaceIE ix sl)
          IndexScalar acc sh
            | Just REFL         <- matchOpenAcc acc (OpenAcc (Avar ZeroIdx))
            , Lam (Body f)      <- ix
            -> case sh of
                 Var _  -> inline (weakenEA f) sh
                 _      -> Let sh (weakenEA f)
            --
            | otherwise -> IndexScalar acc (replaceIE ix sh)

    -- Replace all occurrences of the first term with the second, in the given
    -- expression
    --
    replaceE :: OpenExp env aenv a
             -> OpenExp env aenv a
             -> OpenExp env aenv b
             -> OpenExp env aenv b
    replaceE a b exp
      | Just REFL <- matchOpenExp exp a = b
      | otherwise =  case exp of
          Let bnd body          -> Let (replaceE a b bnd) (replaceE (weakenE a) (weakenE b) body)
          Var ix                -> Var ix
          Const c               -> Const c
          Tuple t               -> Tuple (replaceT a b t)
          Prj ix e              -> Prj ix (replaceE a b e)
          IndexNil              -> IndexNil
          IndexCons sl sz       -> IndexCons (replaceE a b sl) (replaceE a b sz)
          IndexHead sh          -> IndexHead (replaceE a b sh)
          IndexTail sh          -> IndexTail (replaceE a b sh)
          IndexAny              -> IndexAny
          IndexSlice x slix sh  -> IndexSlice x (replaceE a b slix) (replaceE a b sh)
          IndexFull x slix sl   -> IndexFull x (replaceE a b slix) (replaceE a b sl)
          ToIndex sh ix         -> ToIndex (replaceE a b sh) (replaceE a b ix)
          FromIndex sh i        -> FromIndex (replaceE a b sh) (replaceE a b i)
          Cond p t e            -> Cond (replaceE a b p) (replaceE a b t) (replaceE a b e)
          Iterate n f x         -> Iterate n (replaceF a b f) (replaceE a b x)
          PrimConst c           -> PrimConst c
          PrimApp f x           -> PrimApp f (replaceE a b x)
          IndexScalar acc sh    -> IndexScalar acc (replaceE a b sh)
          Shape acc             -> Shape acc
          ShapeSize sh          -> ShapeSize (replaceE a b sh)
          Intersect sh sl       -> Intersect (replaceE a b sh) (replaceE a b sl)
      where
        replaceT :: OpenExp env aenv a
                 -> OpenExp env aenv a
                 -> Tuple.Tuple (OpenExp env aenv) t
                 -> Tuple.Tuple (OpenExp env aenv) t
        replaceT a' b' tup = case tup of
          NilTup        -> NilTup
          SnocTup t e   -> replaceT a' b' t `SnocTup` replaceE a' b' e

        replaceF :: OpenExp env' aenv a
                 -> OpenExp env' aenv a
                 -> OpenFun env' aenv f
                 -> OpenFun env' aenv f
        replaceF a' b' fun = case fun of
          Body e        -> Body (replaceE a' b' e)
          Lam f         -> Lam (replaceF (weakenE a') (weakenE b') f)


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


-- Concatenate two environments
--
cat :: Extend env env' -> Extend env' env'' -> Extend env env''
cat x BaseEnv           = x
cat x (PushEnv e a)     = cat x e `PushEnv` a

