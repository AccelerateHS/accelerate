{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Fission
-- Copyright   : [2015] Trevor L. McDonell, Michael Vollmer
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements fissioning as a term rewriting of the AST.
--

module Data.Array.Accelerate.Trafo.Fission (

  convertAcc

) where

import Prelude                                                  hiding ( map, concat )
import Data.Typeable

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Type


-- | Apply the fission transformation to a closed de Bruijn AST
--
convertAcc :: Arrays arrs => DelayedAcc arrs -> DelayedAcc arrs
convertAcc = convertOpenAcc


-- | Apply the fissioning transformation to an AST.
--
convertOpenAcc
    :: forall aenv arrs. Arrays arrs
    => DelayedOpenAcc aenv arrs
    -> DelayedOpenAcc aenv arrs
convertOpenAcc Delayed{}
  = $internalError "convertOpenAcc" "unexpected delayed array"

convertOpenAcc (Manifest pacc)
  = Manifest
  $ case pacc of
      Use a             -> Use a
      Map f a           -> map (cvtF f) (cvtA a)

      ZipWith{}         -> fusionError
      Slice{}           -> fusionError
      Replicate{}       -> fusionError
  where
    prim :: String
    prim        = showPreAccOp pacc
    fusionError = $internalError "convertOpenAcc" $ "unexpected fusible materials: " ++ prim

    cvtF :: PreOpenFun acc env aenv f -> PreOpenFun acc env aenv f
    cvtF = id

    cvtA :: Arrays a => DelayedOpenAcc aenv a -> DelayedOpenAcc aenv a
    cvtA = convertOpenAcc

    -- The fission rules
    -- -----------------

    map :: forall aenv sh a b. (Shape sh, Elt a, Elt b)
        => DelayedFun aenv (a -> b)
        ->            DelayedOpenAcc aenv (Array sh a)
        -> PreOpenAcc DelayedOpenAcc aenv (Array sh b)
    map f a
      | Just REFL <- matchArrayShape a (undefined::DIM1) = splitjoin a
      | Just REFL <- matchArrayShape a (undefined::DIM2) = splitjoin a
      | otherwise                                        = Map f a
      where
        splitjoin
            :: (Shape sh', Slice sh')
            =>            DelayedOpenAcc aenv (Array (sh' :. Int) a)
            -> PreOpenAcc DelayedOpenAcc aenv (Array (sh' :. Int) b)
        splitjoin a'
          = let a1 = splitArray 2 0 a'
                a2 = splitArray 2 1 a'
            in
            Alet (inject                  $ Map f a1) . inject $
            Alet (inject . weaken SuccIdx $ Map f a2) . inject $
              concatArray (inject (Avar (SuccIdx ZeroIdx)))
                          (inject (Avar ZeroIdx))


-- Concatenate two arrays, as in (++).
--
concatArray
    :: (Slice sh, Shape sh, Elt e)
    =>            DelayedOpenAcc aenv (Array (sh :. Int) e)
    ->            DelayedOpenAcc aenv (Array (sh :. Int) e)
    -> PreOpenAcc DelayedOpenAcc aenv (Array (sh :. Int) e)
concatArray xs ys
  = error "concatArray: finish me"


-- Return chunk 'm' of an array that was split into 'n' equal pieces.
--
splitArray
  :: (Slice sh, Shape sh, Elt e)
  => Int
  -> Int
  -> DelayedOpenAcc aenv (Array (sh :. Int) e)
  -> DelayedOpenAcc aenv (Array (sh :. Int) e)
splitArray n m delayed@Delayed{..}
  = let sh' = withSplitPts n m extentD
            $ error "splitArray: finish me"
    in
    Delayed{ extentD = sh', .. }

splitArray n m (Manifest pacc)
  = error "splitArray: finish me"


-- When splitting an array 'acc' into 'k' pieces, put into the environment as
-- the last two bound variables the split indices for the start and end of chunk
-- 'i'. Assumes that 'i < k'.
--
withSplitPts
    :: forall acc env aenv sh t. (Slice sh, Shape sh, Elt t)
    => Int
    -> Int
    -> PreOpenExp acc env aenv (sh :. Int)
    -> PreOpenExp acc (((((((env, Int), (Int, Int)), Int), Int), Int), Int), Int) aenv t
    -> PreOpenExp acc env aenv t
withSplitPts k' i' sh cont
  = Let (IndexHead sh)
  $ Let (PrimQuotRem int `app` tup2 (v z) k)                                                            -- (chunk, leftover)
  $ Let (Prj (SuccTupIdx ZeroTupIdx) (v z))                                                             -- chunk
  $ Let (Prj ZeroTupIdx (v (s z)))                                                                      -- leftover
  $ Let (PrimAdd num `app` tup2 (v (s z)) (constant 1))                                                 -- chunk + 1
  $ Let (Cond (PrimLt scalarType `app` tup2 i (v (s z)))                                                -- if i <* leftover
              (PrimMul num `app` tup2 i (v z))                                                          --   then start = i * (chunk + 1)
              (PrimAdd num `app` tup2 (PrimMul num `app` tup2 i (v (s (s z)))) (v (s z))))              --   else start = i * chunk + leftover
  $ Let (Cond (PrimLt scalarType `app` tup2 i1 (v (s (s z))))                                           -- if i+1 <* leftover
              (PrimAdd num `app` tup2 (v z) (v (s (s (s z)))))                                          --   then end = start + chunk
              (PrimAdd num `app` tup2 (PrimMul num `app` tup2 i1 (v (s (s (s z))))) (v (s (s z)))))     --   else end = (i+1) * chunk + leftover
  $ cont
  where
    k           = constant k'
    i           = constant i'
    i1          = constant (i'+1)

    z           = ZeroIdx
    s           = SuccIdx
    num         = numType
    int         = integralType
    app f x     = f `PrimApp` x
    tup2 x y    = Tuple (NilTup `SnocTup` x `SnocTup` y)

    constant :: Elt e => e -> PreOpenExp acc env' aenv' e
    constant    = Const . fromElt

    v :: Elt e => Idx env' e -> PreOpenExp acc env' aenv' e
    v           = Var


-- Producet a type witness for the shape of a given array. This is used so that
-- we can apply split/concat operations to arrays, which is only valid for
-- non-singleton arrays.
--
matchArrayShape
    :: forall acc aenv sh sh' e. (Shape sh, Shape sh')
    => {- dummy -} acc aenv (Array sh e)
    -> {- dummy -} sh'
    -> Maybe (sh :=: sh')
matchArrayShape _ _
  | Just REFL <- matchTupleType (eltType (undefined::sh)) (eltType (undefined::sh'))
  = gcast REFL

  | otherwise
  = Nothing

