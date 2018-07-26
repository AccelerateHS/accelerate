{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
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
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lift
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Smart


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
pattern MkT :: forall a b c. AsTuple a b c => a -> c b
pattern MkT vars <- (unlift @c . coerce -> vars)
  where MkT = coerce . lift @c


type AsTuple a b c =
  ( Unlift c a
  , ProdRepr (Plain a) ~ ProdRepr b
  , Coerce (c (Plain a)) (c b), Coerce (c b) (c (Plain a))
  )

class Coerce a b where
  coerce :: a -> b

instance (Elt b, IsTuple b, ProdRepr a ~ ProdRepr b) => Coerce (Exp a) (Exp b) where
  coerce (Exp (Tuple t)) = Exp (Tuple t)
  coerce _               = $internalError "coerce" "expected a tuple"

instance (Arrays b, IsAtuple b, ProdRepr a ~ ProdRepr b) => Coerce (Acc a) (Acc b) where
  coerce (Acc (Atuple t)) = Acc (Atuple t)
  coerce _                = $internalError "coerce" "expected a tuple"

