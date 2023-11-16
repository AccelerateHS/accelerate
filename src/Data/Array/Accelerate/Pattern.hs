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
{-# LANGUAGE TypeOperators         #-}
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

