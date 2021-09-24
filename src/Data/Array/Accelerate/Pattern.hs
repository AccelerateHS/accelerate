{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern (

  pattern Pattern,
  pattern T2,  pattern T3,  pattern T4,  pattern T5,  pattern T6,
  pattern T7,  pattern T8,  pattern T9,  pattern T10, pattern T11,
  pattern T12, pattern T13, pattern T14, pattern T15, pattern T16,

  pattern Z_, pattern Ix, pattern (::.), pattern All_, pattern Any_,
  pattern I0, pattern I1, pattern I2, pattern I3, pattern I4,
  pattern I5, pattern I6, pattern I7, pattern I8, pattern I9,

  pattern V2, pattern V3, pattern V4, pattern V8, pattern V16,

  -- Used internally in other pattern synonyms
  sourceMapPattern,

) where

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Vec
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type
import Data.Primitive.Vec

import Language.Haskell.TH.Extra                                    hiding ( Exp, Match )


-- | A pattern synonym for working with (product) data types. You can declare
-- your own pattern synonyms based off of this.
--
pattern Pattern :: forall b a context. (HasCallStack, IsPattern context a b) => b -> context a
pattern Pattern vars <- (sourceMapPattern (matcher @context) -> vars)
  where Pattern = sourceMapPattern (builder @context)

class IsPattern context a b where
  builder :: SourceMapped => b -> context a
  matcher :: SourceMapped => context a -> b


pattern Vector :: forall b a context. IsVector context a b => b -> context a
pattern Vector vars <- (sourceMapPattern (vunpack @context) -> vars)
  where Vector = sourceMapPattern (vpack @context)

class IsVector context a b where
  vpack   :: SourceMapped => b -> context a
  vunpack :: SourceMapped => context a -> b

-- | Pattern synonyms for indices, which may be more convenient to use than
-- 'Data.Array.Accelerate.Lift.lift' and
-- 'Data.Array.Accelerate.Lift.unlift'.
--
pattern Z_ :: Exp DIM0
pattern Z_ = Pattern Z
{-# COMPLETE Z_ #-}

infixl 3 ::.
pattern (::.) :: (Elt a, Elt b) => Exp a -> Exp b -> Exp (a :. b)
pattern a ::. b = Pattern (a :. b)
{-# COMPLETE (::.) #-}

infixl 3 `Ix`
pattern Ix :: (Elt a, Elt b) => Exp a -> Exp b -> Exp (a :. b)
pattern a `Ix` b = a ::. b
{-# COMPLETE Ix #-}

pattern All_ :: Exp All
pattern All_ <- (sourceMapPattern (const True) -> True)
  where All_ = sourceMapPattern $ constant All
{-# COMPLETE All_ #-}

pattern Any_ :: (Shape sh, Elt (Any sh)) => Exp (Any sh)
pattern Any_ <- (sourceMapPattern (const True) -> True)
  where Any_ = sourceMapPattern $ constant Any
{-# COMPLETE Any_ #-}

-- IsPattern instances for Shape nil and cons
--
instance IsPattern Exp Z Z where
  builder _ = constant Z
  matcher _ = Z

instance (Elt a, Elt b) => IsPattern Exp (a :. b) (Exp a :. Exp b) where
  builder (Exp a :. Exp b) = Exp $ SmartExp $ Pair mkAnn a b
  matcher (Exp t)          = Exp (SmartExp $ Prj mkAnn PairIdxLeft t) :. Exp (SmartExp $ Prj mkAnn PairIdxRight t)


-- IsPattern instances for up to 16-tuples (Acc and Exp). TH takes care of
-- the (unremarkable) boilerplate for us.
--
runQ $ do
    let
        -- Generate instance declarations for IsPattern of the form:
        -- instance (Arrays x, ArraysR x ~ (((), ArraysR a), ArraysR b), Arrays a, Arrays b,) => IsPattern Acc x (Acc a, Acc b)
        mkAccPattern :: Int -> Q [Dec]
        mkAccPattern n = do
          a <- newName "a"
          let
              -- Type variables for the elements
              xs       = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              -- Last argument to `IsPattern`, eg (Acc a, Acc b) in the example
              b        = tupT (map (\t -> [t| Acc $(varT t)|]) xs)
              -- Representation as snoc-list of pairs, eg (((), ArraysR a), ArraysR b)
              snoc     = foldl (\sn t -> [t| ($sn, ArraysR $(varT t)) |]) [t| () |] xs
              -- Constraints for the type class, consisting of Arrays constraints on all type variables,
              -- and an equality constraint on the representation type of `a` and the snoc representation `snoc`.
              context  = tupT
                       $ [t| Arrays $(varT a) |]
                       : [t| ArraysR $(varT a) ~ $snoc |]
                       : map (\t -> [t| Arrays $(varT t)|]) xs
              --
              get x 0 = [| Acc (SmartAcc (Aprj mkAnn PairIdxRight $x)) |]
              get x i = get  [| SmartAcc (Aprj mkAnn PairIdxLeft $x) |] (i-1)
          --
          _x <- newName "_x"
          [d| instance $context => IsPattern Acc $(varT a) $b where
                builder $(tupP (map (\x -> [p| Acc $(varP x)|]) xs)) =
                  Acc $(foldl (\vs v -> [| SmartAcc (Apair mkAnn $vs $(varE v)) |]) [| SmartAcc (Anil mkAnn) |] xs)
                matcher (Acc $(varP _x)) =
                  $(tupE (map (get (varE _x)) [(n-1), (n-2) .. 0]))
            |]

        -- Generate instance declarations for IsPattern of the form:
        -- instance (Elt x, EltR x ~ (((), EltR a), EltR b), Elt a, Elt b,) => IsPattern Exp x (Exp a, Exp b)
        mkExpPattern :: Int -> Q [Dec]
        mkExpPattern n = do
          a <- newName "a"
          let
              -- Type variables for the elements
              xs       = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              -- Variables for sub-pattern matches
              ms       = [ mkName ('m' : show i) | i <- [0 .. n-1] ]
              tags     = foldl (\ts t -> [p| $ts `TagRpair` $(varP t) |]) [p| TagRunit |] ms
              -- Last argument to `IsPattern`, eg (Exp, a, Exp b) in the example
              b        = tupT (map (\t -> [t| Exp $(varT t)|]) xs)
              -- Representation as snoc-list of pairs, eg (((), EltR a), EltR b)
              snoc     = foldl (\sn t -> [t| ($sn, EltR $(varT t)) |]) [t| () |] xs
              -- Constraints for the type class, consisting of Elt constraints on all type variables,
              -- and an equality constraint on the representation type of `a` and the snoc representation `snoc`.
              context  = tupT
                       $ [t| Elt $(varT a) |]
                       : [t| EltR $(varT a) ~ $snoc |]
                       : map (\t -> [t| Elt $(varT t)|]) xs
              --
              get x 0 =     [| SmartExp (Prj mkAnn PairIdxRight $x) |]
              get x i = get [| SmartExp (Prj mkAnn PairIdxLeft $x)  |] (i-1)
          --
          _x <- newName "_x"
          _y <- newName "_y"
          [d| instance $context => IsPattern Exp $(varT a) $b where
                builder $(tupP (map (\x -> [p| Exp $(varP x)|]) xs)) =
                  let _unmatch :: SmartExp a -> SmartExp a
                      _unmatch (SmartExp (Match _ $(varP _y))) = $(varE _y)
                      _unmatch x = x
                  in
                  Exp $(foldl (\vs v -> [| SmartExp (Pair mkAnn $vs (_unmatch $(varE v))) |]) [| SmartExp (Nil mkAnn) |] xs)
                matcher (Exp $(varP _x)) =
                  case $(varE _x) of
                    SmartExp (Match $tags $(varP _y))
                      -> $(tupE [[| Exp (SmartExp (Match $(varE m) $(get (varE _x) i))) |] | m <- ms | i <- [(n-1), (n-2) .. 0]])
                    _ -> $(tupE [[| Exp $(get (varE _x) i) |] | i <- [(n-1), (n-2) .. 0]])
            |]

        -- Generate instance declarations for IsVector of the form:
        -- instance (Elt v, EltR v ~ Vec 2 a, Elt a) => IsVector Exp v (Exp a, Exp a)
        mkVecPattern :: Int -> Q [Dec]
        mkVecPattern n = do
          a <- newName "a"
          v <- newName "v"
          let
              -- Last argument to `IsVector`, eg (Exp, a, Exp a) in the example
              tup      = tupT (replicate n ([t| Exp $(varT a)|]))
              -- Representation as a vector, eg (Vec 2 a)
              vec      = [t| Vec $(litT (numTyLit (fromIntegral n))) $(varT a) |]
              -- Constraints for the type class, consisting of Elt constraints on all type variables,
              -- and an equality constraint on the representation type of `a` and the vector representation `vec`.
              context  = [t| (Elt $(varT v), VecElt $(varT a), EltR $(varT v) ~ $vec) |]
              --
              vecR     = foldr appE ([| VecRnil |] `appE` (varE 'singleType `appTypeE` varT a)) (replicate n [| VecRsucc |])
              tR       = tupT (replicate n (varT a))
          --
          [d| instance $context => IsVector Exp $(varT v) $tup where
                vpack x = case builder x :: Exp $tR of
                            Exp x' -> Exp (SmartExp (VecPack mkAnn $vecR x'))
                vunpack (Exp x) = matcher (Exp (SmartExp (VecUnpack mkAnn $vecR x)) :: Exp $tR)
            |]
    --
    es <- mapM mkExpPattern [0..16]
    as <- mapM mkAccPattern [0..16]
    vs <- mapM mkVecPattern [2,3,4,8,16]
    return $ concat (es ++ as ++ vs)


-- | Specialised pattern synonyms for tuples, which may be more convenient to
-- use than 'Data.Array.Accelerate.Lift.lift' and
-- 'Data.Array.Accelerate.Lift.unlift'. For example, to construct a pair:
--
-- > let a = 4        :: Exp Int
-- > let b = 2        :: Exp Float
-- > let c = T2 a b   -- :: Exp (Int, Float); equivalent to 'lift (a,b)'
--
-- Similarly they can be used to destruct values:
--
-- > let T2 x y = c   -- x :: Exp Int, y :: Exp Float; equivalent to 'let (x,y) = unlift c'
--
-- These pattern synonyms can be used for both 'Exp' and 'Acc' terms.
--
-- Similarly, we have patterns for constructing and destructing indices of
-- a given dimensionality:
--
-- > let ix = Ix 2 3    -- :: Exp DIM2
-- > let I2 y x = ix    -- y :: Exp Int, x :: Exp Int
--
runQ $ do
    let
        mkT :: Int -> Q [Dec]
        mkT n =
          let xs    = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              ts    = map varT xs
              name  = mkName ('T':show n)
              con   = varT (mkName "con")
              ty1   = tupT ts
              ty2   = tupT (map (con `appT`) ts)
              sig   = foldr (\t r -> [t| $con $t -> $r |]) (appT con ty1) ts
          in
          sequence
            [ patSynSigD name [t| IsPattern $con $ty1 $ty2 => $sig |]
            , patSynD    name (prefixPatSyn xs) implBidir [p| Pattern $(tupP (map varP xs)) |]
            , pragCompleteD [name] (Just ''Acc)
            , pragCompleteD [name] (Just ''Exp)
            ]

        mkI :: Int -> Q [Dec]
        mkI n =
          let xs      = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              ts      = map varT xs
              name    = mkName ('I':show n)
              ix      = mkName "Ix"
              cst     = tupT (map (\t -> [t| Elt $t |]) ts)
              dim     = foldl (\h t -> [t| $h :. $t |]) [t| Z |] ts
              sig     = foldr (\t r -> [t| Exp $t -> $r |]) [t| Exp $dim |] ts
          in
          sequence
            [ patSynSigD name [t| $cst => $sig |]
            , patSynD    name (prefixPatSyn xs) implBidir (foldl (\ps p -> infixP ps ix (varP p)) [p| Z_ |] xs)
            , pragCompleteD [name] Nothing
            ]

        mkV :: Int -> Q [Dec]
        mkV n =
          let xs    = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              ts    = map varT xs
              name  = mkName ('V':show n)
              con   = varT (mkName "con")
              ty1   = varT (mkName "vec")
              ty2   = tupT (map (con `appT`) ts)
              sig   = foldr (\t r -> [t| $con $t -> $r |]) (appT con ty1) ts
          in
          sequence
            [ patSynSigD name [t| IsVector $con $ty1 $ty2 => $sig |]
            , patSynD    name (prefixPatSyn xs) implBidir [p| Vector $(tupP (map varP xs)) |]
            , pragCompleteD [name] (Just ''Exp)
            ]
    --
    ts <- mapM mkT [2..16]
    is <- mapM mkI [0..9]
    vs <- mapM mkV [2,3,4,8,16]
    return $ concat (ts ++ is ++ vs)

