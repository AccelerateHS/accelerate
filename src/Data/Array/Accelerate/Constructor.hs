{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
-- |
-- Module      : Data.Array.Accelerate.Constructor
-- Copyright   : [2018..2018] Joshua Meredith, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Constructor (

  pattern MkT,

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Smart

import Language.Haskell.TH                                          hiding (Exp)


-- | This pattern synonym can be used as an alternative to 'lift' and 'unlift'
-- for creating and accessing data types isomorphic to simple product (tuple)
-- types.
--
-- For example, let's say we have regular Haskell data type representing a point
-- in two-dimensional space:
--
-- > data Point = Point_ Float Float
-- >   deriving (Show, Generic, Elt, IsTuple)
--
-- Note that we derive instances for the 'Elt' class, so that this data type can
-- be used within Accelerate scalar expressions, and 'IsTuple', as this is
-- a product type (contains multiple values).
--
-- In order to access the individual fields of the data constructor from within
-- an Accelerate expression, we define the following pattern synonym:
--
-- > pattern Point :: Exp Float -> Exp Float -> Exp Point
-- > pattern Point x y = MkT (x,y)
--
-- In essence, the 'MkT' pattern is really telling GHC how to treat our @Point@
-- type as a regular pair for use in Accelerate code. The pattern can then be
-- used on both the left and right hand side of an expression:
--
-- > addPoint :: Exp Point -> Exp Point -> Exp Point
-- > addPoint (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)
--
-- Similarly, we can define pattern synonyms for values in 'Acc'. We can also
-- use record syntax to generate field accessors, if we desire:
--
-- > data SparseVector a = SparseVector_ (Vector Int) (Vector a)
-- >   deriving (Show, Generic, Arrays, IsAtuple)
-- >
-- > pattern SparseVector :: Elt a => Acc (Vector Int) -> Acc (Vector a) -> Acc (SparseVector a)
-- > pattern SparseVector { indices, values } = MkT (indices, values)
--
pattern MkT :: forall b a context. MkData context a b => b -> context a
pattern MkT vars <- (destruct @context -> vars)
  where MkT = construct @context

class MkData con a t where
  construct :: t -> con a
  destruct  :: con a -> t

-- MkData instances for up to 16-tuples (Acc and Exp). The instances are
-- unremarkable, TH just takes care of the boilerplate for us.
--
$(runQ $ do
    let
        mkData :: Name -> TypeQ -> ExpQ -> ExpQ -> ExpQ -> ExpQ -> Int -> Q [Dec]
        mkData con cst tup prj nil snoc n =
          let
              xs      = [ mkName ('x' : show i) | i <- [0 .. n-1]]
              b       = foldl (\ts t -> appT ts (appT (conT con) (varT t))) (tupleT n) xs
              repr    = foldl (\ts t -> [t| ($ts, $(varT t)) |]) [t| () |] xs
              context = foldl (\ts t -> appT ts (appT cst (varT t))) (tupleT n) xs
              --
              tix 0   = [| ZeroTupIdx |]
              tix i   = [| SuccTupIdx $(tix (i-1)) |]
              get x i = [| $(conE con) ($prj $(tix i) $x) |]
          in
          [d| instance
                ( IsProduct $cst a
                , ProdRepr a ~ $repr
                , $cst a
                , $context
                ) => MkData $(conT con) a $b where
                  construct $(tupP (map varP xs)) = $(conE con) ($tup $(foldl (\vs v -> appE (appE snoc vs) (varE v)) nil xs))
                  destruct x = $(tupE (map (get [|x|]) [(n-1), (n-2) .. 0]))
            |]

        mkAccData = mkData (mkName "Acc") [t| Arrays |] [| Atuple |] [| Aprj |] [| NilAtup |] [| SnocAtup |]
        mkExpData = mkData (mkName "Exp") [t| Elt    |] [| Tuple  |] [| Prj  |] [| NilTup  |] [| SnocTup  |]
    --
    as <- mapM mkAccData [1..16]
    es <- mapM mkExpData [1..16]
    return (concat as ++ concat es)

 )

