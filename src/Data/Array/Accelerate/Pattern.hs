{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
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

) where

import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt

import Language.Haskell.TH.Extra                                    hiding ( Exp, Match )


-- | A pattern synonym for working with (product) data types. You can declare
-- your own pattern synonyms based off of this.
--
pattern Pattern :: forall b a context. IsPattern context a b => b -> context a
pattern Pattern vars <- (matcher @context -> vars)
  where Pattern = builder @context
{-# COMPLETE Pattern :: Exp #-}
{-# COMPLETE Pattern :: Acc #-}

class IsPattern context a b where
  builder :: b -> context a
  matcher :: context a -> b


-- IsPattern instances for up to 16-tuples (Acc and Exp). TH takes care of
-- the (unremarkable) boilerplate for us.
--
runQ $ do
    let
        -- Generate instance declarations for IsPattern of the form:
        -- instance (Arrays x, ArraysR x ~ (((), ArraysR a), ArraysR b), Arrays a, Arrays b,) => IsPattern Acc x (Acc a, Acc b)
        mkAccPattern :: Int -> Q [Dec]
        mkAccPattern n = do
          a  <- newName "a"
          _x <- newName "_x"
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
              get x 0 = [| Acc (SmartAcc (Aprj PairIdxRight $x)) |]
              get x i = get  [| SmartAcc (Aprj PairIdxLeft $x) |] (i-1)
          --
          [d| instance $context => IsPattern Acc $(varT a) $b where
                builder $(tupP (map (\x -> [p| Acc $(varP x)|]) xs)) =
                  Acc $(foldl (\vs v -> [| SmartAcc ($vs `Apair` $(varE v)) |]) [| SmartAcc Anil |] xs)
                matcher (Acc $(varP _x)) =
                  $(tupE (map (get (varE _x)) [(n-1), (n-2) .. 0]))
            |]

        -- Generate instance declarations for IsPattern of the form:
        -- instance (Elt x, EltR x ~ (((), EltR a), EltR b), Elt a, Elt b,) => IsPattern Exp x (Exp a, Exp b)
        mkExpPattern :: Int -> Q [Dec]
        mkExpPattern n = do
          a  <- newName "a"
          _x <- newName "_x"
          _y <- newName "_y"
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
              get x 0 =     [| SmartExp (Prj PairIdxRight $x) |]
              get x i = get [| SmartExp (Prj PairIdxLeft $x)  |] (i-1)
          --
          [d| instance $context => IsPattern Exp $(varT a) $b where
                builder $(tupP (map (\x -> [p| Exp $(varP x)|]) xs)) =
                  let _unmatch :: SmartExp a -> SmartExp a
                      _unmatch (SmartExp (Match _ $(varP _y))) = $(varE _y)
                      _unmatch x = x
                  in
                  Exp $(foldl (\vs v -> [| SmartExp ($vs `Pair` _unmatch $(varE v)) |]) [| SmartExp Nil |] xs)
                matcher (Exp $(varP _x)) =
                  case $(varE _x) of
                    SmartExp (Match $tags $(varP _y))
                      -> $(tupE [[| Exp (SmartExp (Match $(varE m) $(get (varE _x) i))) |] | m <- ms | i <- [(n-1), (n-2) .. 0]])
                    _ -> $(tupE [[| Exp $(get (varE _x) i) |] | i <- [(n-1), (n-2) .. 0]])
            |]
    --
    es <- mapM mkExpPattern [0..16]
    as <- mapM mkAccPattern [0..16]
    return $ concat (es ++ as)


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
    --
    concat <$> mapM mkT [2..16]

