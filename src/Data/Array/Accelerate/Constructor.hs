{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Data.Array.Accelerate.Constructor (

  cast,

  construct, deconstruct,

  pattern Constructor, ConstructorFor,

  TupleOf, Castable, Constructable

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lift
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Smart


type family FlattenTuple a where
  FlattenTuple () = Exp ()
  FlattenTuple ((), a) = Exp a
  FlattenTuple (((), a), b) = (Exp a, Exp b)
  FlattenTuple ((((), a), b), c) = (Exp a, Exp b, Exp c)
  FlattenTuple (((((), a), b), c), d) = (Exp a, Exp b, Exp c, Exp d)
  FlattenTuple ((((((), a), b), c), d), e) = (Exp a, Exp b, Exp c, Exp d, Exp e)
  FlattenTuple (((((((), a), b), c), d), e), f) = (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f)
  FlattenTuple ((((((((), a), b), c), d), e), f), g) = (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g)
  FlattenTuple (((((((((), a), b), c), d), e), f), g), h) = (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h)
  FlattenTuple ((((((((((), a), b), c), d), e), f), g), h), i) = (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i)
  FlattenTuple (((((((((((), a), b), c), d), e), f), g), h), i), j) = (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j)
  FlattenTuple ((((((((((((), a), b), c), d), e), f), g), h), i), j), k) = (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k)
  FlattenTuple (((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l) = (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l)
  FlattenTuple ((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), o) = (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l, Exp o)
  FlattenTuple (((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), o), p) = (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l, Exp o, Exp p)

type TupleOf a = FlattenTuple (ProdRepr a)
type Castable a b = (Elt b, IsTuple a, IsTuple b, ProdRepr a ~ ProdRepr b)
type Constructable a = (Castable (Plain (TupleOf a)) a, Castable a (Plain (TupleOf a)), Lift Exp (TupleOf a), Unlift Exp (TupleOf a))

construct   :: Constructable a => TupleOf a -> Exp     a
construct = cast . lift

deconstruct :: Constructable a => Exp     a -> TupleOf a
deconstruct = unlift . cast

cast :: Castable a b => Exp a -> Exp b
cast (Exp (Tuple x)) = Exp . Tuple $ x
cast _               = $internalError "cast" "only products may be casted"

pattern Constructor :: Constructable a => TupleOf a -> Exp a
pattern Constructor vars <- (deconstruct -> vars)
  where Constructor = construct

type family ConstructorFor a where
  ConstructorFor a = EltFunction (Exp a) (EltRepr a)

type family EltFunction acc a where
  EltFunction acc  ()     = acc
  EltFunction acc (a , b) = EltFunction (Exp b -> acc) a
