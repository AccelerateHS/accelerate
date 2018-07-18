{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Data.Array.Accelerate.Constructor (

  construct, deconstruct,

  pattern Constructor,

  pattern E1 , pattern E2 , pattern E3 , pattern E4 , pattern E5 ,
  pattern E6 , pattern E7 , pattern E8 , pattern E9 , pattern E10,
  pattern E11, pattern E12, pattern E13, pattern E14, pattern E15,

  pattern A1 , pattern A2 , pattern A3 , pattern A4 , pattern A5 ,
  pattern A6 , pattern A7 , pattern A8 , pattern A9 , pattern A10,
  pattern A11, pattern A12, pattern A13, pattern A14, pattern A15

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


pattern Constructor :: forall con a. Constructable con a => TupleOf con a -> con a
pattern Constructor vars <- (deconstruct @con -> vars)
  where Constructor = construct @con


pattern C1
  :: forall con t a.
  ( Constructable con t
  , TupleOf con t ~ con a
  ) => con a -> con t
pattern C1 a = Constructor a

pattern C2
  :: forall con t a b.
  ( Constructable con t
  , TupleOf con t ~ TupleOf con (a, b)
  ) => con a -> con b -> con t
pattern C2 a b = Constructor (a, b)

pattern C3
  :: forall con t a b c.
  ( Constructable con t
  , TupleOf con t ~ TupleOf con (a, b, c)
  ) => con a -> con b -> con c -> con t
pattern C3 a b c = Constructor (a, b, c)

pattern C4
  :: forall con t a b c d.
  ( Constructable con t
  , TupleOf con t ~ TupleOf con (a, b, c, d)
  ) => con a -> con b -> con c -> con d -> con t
pattern C4 a b c d = Constructor (a, b, c, d)

pattern C5
  :: forall con t a b c d e.
  ( Constructable con t
  , TupleOf con t ~ TupleOf con (a, b, c, d, e)
  ) => con a -> con b -> con c -> con d -> con e -> con t
pattern C5 a b c d e = Constructor (a, b, c, d, e)

pattern C6
  :: forall con t a b c d e f.
  ( Constructable con t
  , TupleOf con t ~ TupleOf con (a, b, c, d, e, f)
  ) => con a -> con b -> con c -> con d -> con e -> con f -> con t
pattern C6 a b c d e f = Constructor (a, b, c, d, e, f)

pattern C7
  :: forall con t a b c d e f g.
  ( Constructable con t
  , TupleOf con t ~ TupleOf con (a, b, c, d, e, f, g)
  ) => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con t
pattern C7 a b c d e f g = Constructor (a, b, c, d, e, f, g)

pattern C8
  :: forall con t a b c d e f g h.
  ( Constructable con t
  , TupleOf con t ~ TupleOf con (a, b, c, d, e, f, g, h)
  ) => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con t
pattern C8 a b c d e f g h = Constructor (a, b, c, d, e, f, g, h)

pattern C9
  :: forall con t a b c d e f g h i.
  ( Constructable con t
  , TupleOf con t ~ TupleOf con (a, b, c, d, e, f, g, h, i)
  ) => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con t
pattern C9 a b c d e f g h i = Constructor (a, b, c, d, e, f, g, h, i)

pattern C10
  :: forall con t a b c d e f g h i j.
  ( Constructable con t
  , TupleOf con t ~ TupleOf con (a, b, c, d, e, f, g, h, i, j)
  ) => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con t
pattern C10 a b c d e f g h i j = Constructor (a, b, c, d, e, f, g, h, i, j)

pattern C11
  :: forall con t a b c d e f g h i j k.
  ( Constructable con t
  , TupleOf con t ~ TupleOf con (a, b, c, d, e, f, g, h, i, j, k)
  ) => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con t
pattern C11 a b c d e f g h i j k = Constructor (a, b, c, d, e, f, g, h, i, j, k)

pattern C12
  :: forall con t a b c d e f g h i j k l.
  ( Constructable con t
  , TupleOf con t ~ TupleOf con (a, b, c, d, e, f, g, h, i, j, k, l)
  ) => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con l -> con t
pattern C12 a b c d e f g h i j k l = Constructor (a, b, c, d, e, f, g, h, i, j, k, l)

pattern C13
  :: forall con t a b c d e f g h i j k l m.
  ( Constructable con t
  , TupleOf con t ~ TupleOf con (a, b, c, d, e, f, g, h, i, j, k, l, m)
  ) => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con l -> con m -> con t
pattern C13 a b c d e f g h i j k l m = Constructor (a, b, c, d, e, f, g, h, i, j, k, l, m)

pattern C14
  :: forall con t a b c d e f g h i j k l m n.
  ( Constructable con t
  , TupleOf con t ~ TupleOf con (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  ) => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con l -> con m -> con n -> con t
pattern C14 a b c d e f g h i j k l m n = Constructor (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

pattern C15
  :: forall con t a b c d e f g h i j k l m n o.
  ( Constructable con t
  , TupleOf con t ~ TupleOf con (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  ) => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con l -> con m -> con n -> con o -> con t
pattern C15 a b c d e f g h i j k l m n o = Constructor (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

pattern E1  a                             = C1  a                             :: Exp t
pattern E2  a b                           = C2  a b                           :: Exp (a, b)
pattern E3  a b c                         = C3  a b c                         :: Exp (a, b, c)
pattern E4  a b c d                       = C4  a b c d                       :: Exp (a, b, c, d)
pattern E5  a b c d e                     = C5  a b c d e                     :: Exp (a, b, c, d, e)
pattern E6  a b c d e f                   = C6  a b c d e f                   :: Exp (a, b, c, d, e, f)
pattern E7  a b c d e f g                 = C7  a b c d e f g                 :: Exp (a, b, c, d, e, f, g)
pattern E8  a b c d e f g h               = C8  a b c d e f g h               :: Exp (a, b, c, d, e, f, g, h)
pattern E9  a b c d e f g h i             = C9  a b c d e f g h i             :: Exp (a, b, c, d, e, f, g, h, i)
pattern E10 a b c d e f g h i j           = C10 a b c d e f g h i j           :: Exp (a, b, c, d, e, f, g, h, i, j)
pattern E11 a b c d e f g h i j k         = C11 a b c d e f g h i j k         :: Exp (a, b, c, d, e, f, g, h, i, j, k)
pattern E12 a b c d e f g h i j k l       = C12 a b c d e f g h i j k l       :: Exp (a, b, c, d, e, f, g, h, i, j, k, l)
pattern E13 a b c d e f g h i j k l m     = C13 a b c d e f g h i j k l m     :: Exp (a, b, c, d, e, f, g, h, i, j, k, l, m)
pattern E14 a b c d e f g h i j k l m n   = C14 a b c d e f g h i j k l m n   :: Exp (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
pattern E15 a b c d e f g h i j k l m n o = C15 a b c d e f g h i j k l m n o :: Exp (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

pattern A1  a                             = C1  a                             :: Acc t
pattern A2  a b                           = C2  a b                           :: Acc (a, b)
pattern A3  a b c                         = C3  a b c                         :: Acc (a, b, c)
pattern A4  a b c d                       = C4  a b c d                       :: Acc (a, b, c, d)
pattern A5  a b c d e                     = C5  a b c d e                     :: Acc (a, b, c, d, e)
pattern A6  a b c d e f                   = C6  a b c d e f                   :: Acc (a, b, c, d, e, f)
pattern A7  a b c d e f g                 = C7  a b c d e f g                 :: Acc (a, b, c, d, e, f, g)
pattern A8  a b c d e f g h               = C8  a b c d e f g h               :: Acc (a, b, c, d, e, f, g, h)
pattern A9  a b c d e f g h i             = C9  a b c d e f g h i             :: Acc (a, b, c, d, e, f, g, h, i)
pattern A10 a b c d e f g h i j           = C10 a b c d e f g h i j           :: Acc (a, b, c, d, e, f, g, h, i, j)
pattern A11 a b c d e f g h i j k         = C11 a b c d e f g h i j k         :: Acc (a, b, c, d, e, f, g, h, i, j, k)
pattern A12 a b c d e f g h i j k l       = C12 a b c d e f g h i j k l       :: Acc (a, b, c, d, e, f, g, h, i, j, k, l)
pattern A13 a b c d e f g h i j k l m     = C13 a b c d e f g h i j k l m     :: Acc (a, b, c, d, e, f, g, h, i, j, k, l, m)
pattern A14 a b c d e f g h i j k l m n   = C14 a b c d e f g h i j k l m n   :: Acc (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
pattern A15 a b c d e f g h i j k l m n o = C15 a b c d e f g h i j k l m n o :: Acc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
