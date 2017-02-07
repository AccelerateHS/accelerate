{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Rewrite
-- Copyright   : [2012..2014] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Rewrite
  where

import Prelude                                          hiding ( seq )
#if __GLASGOW_HASKELL__ <= 708
import Data.Functor                                     ( (<$>) )
#endif

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Trafo.Base                 ( Extend(..), StreamSeq(..) )
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Array.Sugar                ( Array, Arrays, Segments, Shape, ShapeR(..), shapeType, AsSlice(..), asSlice
                                                        , Elt, fromElt, Tuple(..), Atuple(..) )


-- Convert segment length arrays passed to segmented operations into offset
-- index style. This is achieved by wrapping the segmented array argument in a
-- left prefix-sum, so you must only ever apply this once.
--
convertSegments :: OpenAcc aenv a -> OpenAcc aenv a
convertSegments = cvtA
  where
    cvtT :: Atuple (OpenAcc aenv) t -> Atuple (OpenAcc aenv) t
    cvtT atup = case atup of
      NilAtup      -> NilAtup
      SnocAtup t a -> cvtT t `SnocAtup` cvtA a

    cvtAfun :: OpenAfun aenv t -> OpenAfun aenv t
    cvtAfun = convertSegmentsAfun

    cvtE :: Exp aenv t -> Exp aenv t
    cvtE = id

    cvtF :: Fun aenv t -> Fun aenv t
    cvtF = id

    a0 :: Arrays a => OpenAcc (aenv, a) a
    a0 = OpenAcc (Avar ZeroIdx)

    segments :: (Elt i, IsIntegral i) => OpenAcc aenv (Segments i) -> OpenAcc aenv (Segments i)
    segments s = OpenAcc $ Scanl plus zero (cvtA s)

    zero :: forall aenv i. (Elt i, IsIntegral i) => PreOpenExp OpenAcc () aenv i
    zero = Const (fromElt (0::i))

    plus :: (Elt i, IsIntegral i) => PreOpenFun OpenAcc () aenv (i -> i -> i)
    plus = Lam (Lam (Body (PrimAdd numType
                          `PrimApp`
                          Tuple (NilTup `SnocTup` Var (SuccIdx ZeroIdx)
                                        `SnocTup` Var ZeroIdx))))

    cvtA :: OpenAcc aenv a -> OpenAcc aenv a
    cvtA (OpenAcc pacc) = OpenAcc $ case pacc of
      Alet bnd body             -> Alet (cvtA bnd) (cvtA body)
      Avar ix                   -> Avar ix
      Atuple tup                -> Atuple (cvtT tup)
      Aprj tup a                -> Aprj tup (cvtA a)
      Apply f a                 -> Apply (cvtAfun f) (cvtA a)
      Aforeign ff afun acc      -> Aforeign ff (cvtAfun afun) (cvtA acc)
      Acond p t e               -> Acond (cvtE p) (cvtA t) (cvtA e)
      Awhile p f a              -> Awhile (cvtAfun p) (cvtAfun f) (cvtA a)
      Use a                     -> Use a
      Subarray ix sh arr        -> Subarray (cvtE ix) (cvtE sh) arr
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
      Collect min max i s       -> Collect (cvtE min) (cvtE <$> max) (cvtE <$> i) (convertSegmentsSeq s)

      -- Things we are interested in, whoo!
      FoldSeg f z a s           -> Alet (segments s) (OpenAcc (FoldSeg (cvtF f') (cvtE z') (cvtA a') a0))
        where f' = weaken SuccIdx f
              z' = weaken SuccIdx z
              a' = weaken SuccIdx a

      Fold1Seg f a s            -> Alet (segments s) (OpenAcc (Fold1Seg (cvtF f') (cvtA a') a0))
        where f' = weaken SuccIdx f
              a' = weaken SuccIdx a


convertSegmentsAfun :: OpenAfun aenv t -> OpenAfun aenv t
convertSegmentsAfun afun =
  case afun of
    Abody b     -> Abody (convertSegments b)
    Alam f      -> Alam  (convertSegmentsAfun f)

convertSegmentsStreamSeq :: StreamSeq index OpenAcc a -> StreamSeq index OpenAcc a
convertSegmentsStreamSeq (StreamSeq binds seq) = StreamSeq (cvtExtend binds) (convertSegmentsSeq seq)
  where
    cvtExtend :: Extend OpenAcc aenv aenv' -> Extend OpenAcc aenv aenv'
    cvtExtend BaseEnv = BaseEnv
    cvtExtend (PushEnv env a) = cvtExtend env `PushEnv` convertSegments a

convertSegmentsSeq :: PreOpenSeq index OpenAcc aenv a -> PreOpenSeq index OpenAcc aenv a
convertSegmentsSeq seq =
  case seq of
    Producer p s -> Producer (cvtP p) (convertSegmentsSeq s)
    Consumer c   -> Consumer (cvtC c)
    Reify ty a   -> Reify ty (cvtA a)
  where
    cvtP :: Producer index OpenAcc aenv a -> Producer index OpenAcc aenv a
    cvtP p =
      case p of
        Pull src            -> Pull src
        Subarrays sh arr    -> Subarrays (cvtE sh) arr
        FromSegs s n vs     -> FromSegs (cvtA s) (cvtE n) (cvtA vs)
        Produce l f         -> Produce (cvtE <$> l) (cvtAfun f)
        -- MapBatch f c c' a x -> MapBatch (cvtAfun f) (cvtAfun c) (cvtAfun c') (cvtA a) (cvtA x)
        ProduceAccum l f a  -> ProduceAccum (cvtE <$> l) (cvtAfun f) (cvtA a)

    cvtC :: Consumer index OpenAcc aenv a -> Consumer index OpenAcc aenv a
    cvtC c =
      case c of
        FoldBatch f a x -> FoldBatch (cvtAfun f) (cvtA a) (cvtA x)
        Last a d        -> Last (cvtA a) (cvtA d)
        Elements x      -> Elements (cvtA x)
        Tabulate x      -> Tabulate (cvtA x)
        Stuple t        -> Stuple (cvtCT t)

    cvtCT :: Atuple (PreOpenSeq index OpenAcc aenv) t -> Atuple (PreOpenSeq index OpenAcc aenv) t
    cvtCT NilAtup        = NilAtup
    cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (convertSegmentsSeq c)

    cvtE :: Exp aenv t -> Exp aenv t
    cvtE = id

    cvtA :: OpenAcc aenv t -> OpenAcc aenv t
    cvtA = convertSegments

    cvtAfun :: OpenAfun aenv t -> OpenAfun aenv t
    cvtAfun = convertSegmentsAfun


-- Convert all occurences of 'Subarray' to the corresponding index
-- transformation over a "used" array. This is useful for backends that do not
-- have a remote memory.
--
convertSubarray :: OpenAcc aenv a -> OpenAcc aenv a
convertSubarray = cvtA
  where
    cvtT :: Atuple (OpenAcc aenv) t -> Atuple (OpenAcc aenv) t
    cvtT atup = case atup of
      NilAtup      -> NilAtup
      SnocAtup t a -> cvtT t `SnocAtup` cvtA a

    cvtAfun :: OpenAfun aenv t -> OpenAfun aenv t
    cvtAfun = convertSubarrayAfun

    cvtE :: Exp aenv t -> Exp aenv t
    cvtE = id

    cvtF :: Fun aenv t -> Fun aenv t
    cvtF = id

    subarray :: (Shape sh, Elt e) => Exp aenv sh -> Exp aenv sh -> Array sh e -> PreOpenAcc OpenAcc aenv (Array sh e)
    subarray ix sh arr = Alet (OpenAcc (Use arr)) (OpenAcc (Backpermute sh' f (OpenAcc (Avar ZeroIdx))))
      where
        sh' = weaken SuccIdx sh
        ix' = weakenE SuccIdx (weaken SuccIdx ix)
        f = Lam . Body $ ix' `offset` Var ZeroIdx

    offset :: Shape sh => OpenExp env aenv sh -> OpenExp env aenv sh -> OpenExp env aenv sh
    offset ix1 ix2 = Let ix1 (Let (weakenE SuccIdx ix2) (offset' (shapeType ix1) (Var (SuccIdx ZeroIdx)) (Var ZeroIdx)))
      where
        offset' :: ShapeR sh -> OpenExp env aenv sh -> OpenExp env aenv sh -> OpenExp env aenv sh
        offset' ShapeRnil       _   _   = IndexNil
        offset' (ShapeRcons sr) ix1 ix2 | AsSlice <- asSlice sr
                                        = IndexCons (offset' sr (IndexTail ix1) (IndexTail ix2))
                                                    (IndexHead ix1 `plus` IndexHead ix2)

    plus :: (Elt i, IsIntegral i) => OpenExp env aenv i -> OpenExp env aenv i -> OpenExp env aenv i
    plus x y = PrimAdd numType
               `PrimApp`
               Tuple (NilTup `SnocTup` x
                             `SnocTup` y)

    cvtA :: OpenAcc aenv a -> OpenAcc aenv a
    cvtA (OpenAcc pacc) = OpenAcc $ case pacc of
      Alet bnd body             -> Alet (cvtA bnd) (cvtA body)
      Avar ix                   -> Avar ix
      Atuple tup                -> Atuple (cvtT tup)
      Aprj tup a                -> Aprj tup (cvtA a)
      Apply f a                 -> Apply (cvtAfun f) (cvtA a)
      Aforeign ff afun acc      -> Aforeign ff (cvtAfun afun) (cvtA acc)
      Acond p t e               -> Acond (cvtE p) (cvtA t) (cvtA e)
      Awhile p f a              -> Awhile (cvtAfun p) (cvtAfun f) (cvtA a)
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
      Collect min max i s       -> Collect (cvtE min) (cvtE <$> max) (cvtE <$> i) (convertSubarraySeq s)
      FoldSeg f z a s           -> FoldSeg (cvtF f) (cvtE z) (cvtA a) (cvtA s)
      Fold1Seg f a s            -> Fold1Seg (cvtF f) (cvtA a) (cvtA s)

      -- What we want to change
      Subarray ix sh arr        -> subarray (cvtE ix) (cvtE sh) arr

convertSubarrayAfun :: OpenAfun aenv t -> OpenAfun aenv t
convertSubarrayAfun afun =
  case afun of
    Abody b     -> Abody (convertSubarray b)
    Alam f      -> Alam  (convertSubarrayAfun f)

convertSubarrayStreamSeq :: StreamSeq index OpenAcc a -> StreamSeq index OpenAcc a
convertSubarrayStreamSeq (StreamSeq binds seq) = StreamSeq (cvtExtend binds) (convertSubarraySeq seq)
  where
    cvtExtend :: Extend OpenAcc aenv aenv' -> Extend OpenAcc aenv aenv'
    cvtExtend BaseEnv = BaseEnv
    cvtExtend (PushEnv env a) = cvtExtend env `PushEnv` convertSubarray a

convertSubarraySeq :: PreOpenSeq index OpenAcc aenv a -> PreOpenSeq index OpenAcc aenv a
convertSubarraySeq seq =
  case seq of
    Producer p s -> Producer (cvtP p) (convertSubarraySeq s)
    Consumer c   -> Consumer (cvtC c)
    Reify ty a   -> Reify ty (cvtA a)
  where
    cvtP :: Producer index OpenAcc aenv a -> Producer index OpenAcc aenv a
    cvtP p =
      case p of
        Pull src            -> Pull src
        Subarrays sh arr    -> Subarrays (cvtE sh) arr
        FromSegs s n vs     -> FromSegs (cvtA s) (cvtE n) (cvtA vs)
        Produce l f         -> Produce (cvtE <$> l) (cvtAfun f)
        -- MapBatch f c c' a x -> MapBatch (cvtAfun f) (cvtAfun c) (cvtAfun c') (cvtA a) (cvtA x)
        ProduceAccum l f a  -> ProduceAccum (cvtE <$> l) (cvtAfun f) (cvtA a)

    cvtC :: Consumer index OpenAcc aenv a -> Consumer index OpenAcc aenv a
    cvtC c =
      case c of
        FoldBatch f a x -> FoldBatch (cvtAfun f) (cvtA a) (cvtA x)
        Last a d        -> Last (cvtA a) (cvtA d)
        Elements x      -> Elements (cvtA x)
        Tabulate x      -> Tabulate (cvtA x)
        Stuple t        -> Stuple (cvtCT t)

    cvtCT :: Atuple (PreOpenSeq index OpenAcc aenv) t -> Atuple (PreOpenSeq index OpenAcc aenv) t
    cvtCT NilAtup        = NilAtup
    cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (convertSubarraySeq c)

    cvtE :: Exp aenv t -> Exp aenv t
    cvtE = id

    cvtA :: OpenAcc aenv t -> OpenAcc aenv t
    cvtA = convertSubarray

    cvtAfun :: OpenAfun aenv t -> OpenAfun aenv t
    cvtAfun = convertSubarrayAfun
