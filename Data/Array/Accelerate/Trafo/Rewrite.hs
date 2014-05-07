{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Rewrite
-- Copyright   : [2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Rewrite
  where

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Array.Sugar                ( Arrays, Segments, Elt, fromElt )


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

    cvtE :: Elt t => Exp aenv t -> Exp aenv t
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

      -- Things we are interested in, whoo!
      FoldSeg f z a s           -> Alet (segments s) (OpenAcc (FoldSeg (cvtF f') (cvtE z') (cvtA a') a0))
        where f' = weakenFA rebuildOpenAcc SuccIdx f
              z' = weakenEA rebuildOpenAcc SuccIdx z
              a' = rebuildOpenAcc (Avar . SuccIdx) a

      Fold1Seg f a s            -> Alet (segments s) (OpenAcc (Fold1Seg (cvtF f') (cvtA a') a0))
        where f' = weakenFA rebuildOpenAcc SuccIdx f
              a' = rebuildOpenAcc (Avar . SuccIdx) a


convertSegmentsAfun :: OpenAfun aenv t -> OpenAfun aenv t
convertSegmentsAfun afun =
  case afun of
    Abody b     -> Abody (convertSegments b)
    Alam f      -> Alam (convertSegmentsAfun f)

