{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
-- |
-- Module      : Data.Array.Accelerate.Constructor
-- Copyright   : [2018..2018] Joshua Meredith
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Constructing terms of custom data types with pattern synonyms.
--

module Data.Array.Accelerate.Constructor (

  pattern Constructor,


) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lift
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Smart

import Data.Constraint


type family FlattenTuple con a where
  FlattenTuple con () = con ()
  FlattenTuple con ((), a) = con a
  FlattenTuple con (((), a), b) = (con a, con b)
  FlattenTuple con ((((), a), b), c) = (con a, con b, con c)
  FlattenTuple con (((((), a), b), c), d) = (con a, con b, con c, con d)
  FlattenTuple con ((((((), a), b), c), d), e) = (con a, con b, con c, con d, con e)
  FlattenTuple con (((((((), a), b), c), d), e), f) = (con a, con b, con c, con d, con e, con f)
  FlattenTuple con ((((((((), a), b), c), d), e), f), g) = (con a, con b, con c, con d, con e, con f, con g)
  FlattenTuple con (((((((((), a), b), c), d), e), f), g), h) = (con a, con b, con c, con d, con e, con f, con g, con h)
  FlattenTuple con ((((((((((), a), b), c), d), e), f), g), h), i) = (con a, con b, con c, con d, con e, con f, con g, con h, con i)
  FlattenTuple con (((((((((((), a), b), c), d), e), f), g), h), i), j) = (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j)
  FlattenTuple con ((((((((((((), a), b), c), d), e), f), g), h), i), j), k) = (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k)
  FlattenTuple con (((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l) = (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k, con l)
  FlattenTuple con ((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m) = (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k, con l, con m)
  FlattenTuple con (((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n) = (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k, con l, con m, con n)
  FlattenTuple con ((((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n), o) = (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k, con l, con m, con n, con o)

type TupleOf con a = FlattenTuple con (ProdRepr a)
type Constructable con a = (Castable con (Plain (TupleOf con a)) a, Castable con a (Plain (TupleOf con a)), Unlift con (TupleOf con a))

construct :: forall con a. Constructable con a => TupleOf con a -> con a
construct = cast @con . lift

deconstruct :: forall con a. Constructable con a => con a -> TupleOf con a
deconstruct = unlift . cast @con


class Castable con a b where
  type Cst (con :: * -> *) :: * -> Constraint
  cast :: con a -> con b

instance (Elt b, IsProduct Elt a, IsProduct Elt b, ProdRepr a ~ ProdRepr b) => Castable Exp a b where
  type Cst Exp = Elt
  cast (Exp (Tuple x)) = Exp (Tuple x)
  cast _               = $internalError "cast (exp)" "can only cast products"

instance (Arrays b, IsProduct Arrays a, IsProduct Arrays b, ProdRepr a ~ ProdRepr b) => Castable Acc a b where
  type Cst Acc = Arrays
  cast (Acc (Atuple x)) = Acc (Atuple x)
  cast _                = $internalError "cast (acc)" "can only cast products"


-- | A pattern synonym for constucting data into an `Exp` or `Acc` context. For
-- example, to define a custom data type representing coordinates, first define
-- the type as a normal Haskell ADT and derive instances for @Show@, @Generic@,
-- @Elt@, and @IsProduct Elt@.
--
-- > {-# LANGUAGE DeriveGeneric, DeriveAnyClass, PatternSynonyms #-}
-- > import GHC.Generics
-- > data Coord = Coord' Int Int
-- >   deriving (Show, Generic, Elt, IsProduct Elt)
--
-- Now, we can write a less polymorphic synonym to @Constructor@:
--
-- > pattern Coord :: Exp Int -> Exp Int -> Exp Coord
-- > pattern Coord x y = Constructor (x, y)
--
-- and use the pattern in the LHS and RHS of expressions:
--
-- > add1toY :: Exp Coord -> Exp Coord
-- > add1toY (Coord x y) = Coord x (y + 1)
--
-- We can similarly define custom data types containing arrays to represent
-- world data for our computation:
--
-- > data Computation = Computation' (Array DIM1 Float) (Array DIM2 Float)
-- >   deriving (Show, Generic, Arrays, IsProduct Arrays)
-- > pattern Computation { info, grid } = Constructor' (info, grid)
--
-- In this case, we have defined the pattern synonym with record syntax, giving
-- us the option to access the fields with normal record accessors:
--
-- > filteredInfo :: (Exp Float -> Exp Bool) -> Acc Computation -> Acc (Array DIM1 Float)
-- > filteredInfo pred = filter pred . info
--
pattern Constructor :: forall con a. Constructable con a => TupleOf con a -> con a
pattern Constructor vars <- (deconstruct @con -> vars)
  where Constructor = construct @con







