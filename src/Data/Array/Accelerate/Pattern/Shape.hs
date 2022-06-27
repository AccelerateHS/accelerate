{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern.Shape
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern.Shape (

  pattern Z, Z,
  pattern (:.), (:.),
  pattern All, All,
  pattern Any, Any,

  pattern I0, pattern I1, pattern I2, pattern I3, pattern I4,
  pattern I5, pattern I6, pattern I7, pattern I8, pattern I9,

) where

import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape                            ( (:.), Z, All, Any )
import qualified Data.Array.Accelerate.Sugar.Shape                  as Sugar

import Language.Haskell.TH.Extra                                    hiding ( Exp, Match )


pattern Z :: IsShapeZ z => z
pattern Z <- (matchZ -> True)
  where Z = buildZ
{-# COMPLETE Z :: Z   #-}
{-# COMPLETE Z :: Exp #-}

pattern All :: IsShapeAll all => all
pattern All <- (matchAll -> True)
  where All = buildAll
{-# COMPLETE All :: All #-}
{-# COMPLETE All :: Exp #-}

pattern Any :: IsShapeAny any => any
pattern Any <- (matchAny -> True)
  where Any = buildAny
{-# COMPLETE Any :: Any #-}
{-# COMPLETE Any :: Exp #-}

infixl 3 :.
pattern (:.) :: IsShapeSnoc t h s => t -> h -> s
pattern t :. h <- (matchSnoc -> (t Sugar.:. h))
  where t :. h = buildSnoc t h
{-# COMPLETE (:.) :: (:.) #-}
{-# COMPLETE (:.) :: Exp  #-}


class IsShapeZ z where
  matchZ :: z -> Bool
  buildZ :: z

instance IsShapeZ Z where
  matchZ _ = True
  buildZ   = Sugar.Z

instance IsShapeZ (Exp Z) where
  matchZ _ = True
  buildZ   = constant Sugar.Z

class IsShapeAll all where
  matchAll :: all -> Bool
  buildAll :: all

instance IsShapeAll All where
  matchAll _ = True
  buildAll   = Sugar.All

instance IsShapeAll (Exp All) where
  matchAll _ = True
  buildAll   = constant Sugar.All

class IsShapeAny any where
  matchAny :: any -> Bool
  buildAny :: any

instance IsShapeAny (Any sh) where
  matchAny _ = True
  buildAny   = Sugar.Any

instance Elt (Any sh) => IsShapeAny (Exp (Any sh)) where
  matchAny _ = True
  buildAny   = constant Sugar.Any

class IsShapeSnoc t h s | s -> h t where
  matchSnoc :: s -> (t :. h)
  buildSnoc :: t -> h -> s

instance IsShapeSnoc (Exp t) (Exp h) (Exp (t :. h)) where
  buildSnoc (Exp a) (Exp b) = Exp $ SmartExp $ Pair a b
  matchSnoc (Exp t)         = Exp (SmartExp $ Prj PairIdxLeft t) Sugar.:. Exp (SmartExp $ Prj PairIdxRight t)

instance IsShapeSnoc t h (t :. h) where
  buildSnoc = (Sugar.:.)
  matchSnoc = id


-- Generate patterns for constructing and destructing indices of a given
-- dimensionality:
--
-- > let ix = Ix 2 3    -- :: Exp DIM2
-- > let I2 y x = ix    -- y :: Exp Int, x :: Exp Int
--
runQ $
  let
      mkI :: Int -> Q [Dec]
      mkI n =
        let xs      = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
            ts      = map varT xs
            name    = mkName ('I':show n)
            ix      = mkName ":."
            cst     = tupT (map (\t -> [t| Elt $t |]) ts)
            dim     = foldl (\h t -> [t| $h :. $t |]) [t| Z |] ts
            sig     = foldr (\t r -> [t| Exp $t -> $r |]) [t| Exp $dim |] ts
        in
        sequence
          [ patSynSigD name [t| $cst => $sig |]
          , patSynD    name (prefixPatSyn xs) implBidir (foldl (\ps p -> infixP ps ix (varP p)) [p| Z |] xs)
          , pragCompleteD [name] Nothing
          ]
  in
  concat <$> mapM mkI [0..9]

