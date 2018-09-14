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
-- Module      : Data.Array.Accelerate.Pattern
-- Copyright   : [2018..2018] Joshua Meredith, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern (

  pattern Pattern,
  pattern T2,  pattern T3,  pattern T4,  pattern T5,  pattern T6,
  pattern T7,  pattern T8,  pattern T9,  pattern T10, pattern T11,
  pattern T12, pattern T13, pattern T14, pattern T15, pattern T16,

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Smart

import Language.Haskell.TH                                          hiding ( Exp )


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
-- > pattern Point x y = Pattern (x,y)
--
-- In essence, the 'Pattern' pattern is really telling GHC how to treat our @Point@
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
-- > pattern SparseVector { indices, values } = Pattern (indices, values)
--
pattern Pattern :: forall b a context. IsPattern context a b => b -> context a
pattern Pattern vars <- (destruct @context -> vars)
  where Pattern = construct @context

class IsPattern con a t where
  construct :: t -> con a
  destruct  :: con a -> t

-- | Specialised pattern synonyms for tuples, which may be more convenient to
-- use than 'lift' and 'unlift'. For example, to construct a pair:
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
pattern T2 :: IsPattern con (a,b) (con a, con b) => con a -> con b -> con (a, b)
pattern T2 a b = Pattern (a, b)

pattern T3 :: IsPattern con (a,b,c) (con a, con b, con c) => con a -> con b -> con c -> con (a, b, c)
pattern T3 a b c = Pattern (a, b, c)

pattern T4
    :: IsPattern con (a,b,c,d) (con a, con b, con c, con d)
    => con a -> con b -> con c -> con d
    -> con (a, b, c, d)
pattern T4 a b c d = Pattern (a, b, c, d)

pattern T5
    :: IsPattern con (a,b,c,d,e) (con a, con b, con c, con d, con e)
    => con a -> con b -> con c -> con d -> con e
    -> con (a, b, c, d, e)
pattern T5 a b c d e = Pattern (a, b, c, d, e)

pattern T6
    :: IsPattern con (a,b,c,d,e,f) (con a, con b, con c, con d, con e, con f)
    => con a -> con b -> con c -> con d -> con e -> con f
    -> con (a, b, c, d, e, f)
pattern T6 a b c d e f = Pattern (a, b, c, d, e, f)

pattern T7
    :: IsPattern con (a,b,c,d,e,f,g) (con a, con b, con c, con d, con e, con f, con g)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g
    -> con (a, b, c, d, e, f, g)
pattern T7 a b c d e f g = Pattern (a, b, c, d, e, f, g)

pattern T8
    :: IsPattern con (a,b,c,d,e,f,g,h) (con a, con b, con c, con d, con e, con f, con g, con h)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h
    -> con (a, b, c, d, e, f, g, h)
pattern T8 a b c d e f g h = Pattern (a, b, c, d, e, f, g, h)

pattern T9
    :: IsPattern con (a,b,c,d,e,f,g,h,i) (con a, con b, con c, con d, con e, con f, con g, con h, con i)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i
    -> con (a, b, c, d, e, f, g, h, i)
pattern T9 a b c d e f g h i = Pattern (a, b, c, d, e, f, g, h, i)

pattern T10
    :: IsPattern con (a,b,c,d,e,f,g,h,i,j) (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j
    -> con (a, b, c, d, e, f, g, h, i, j)
pattern T10 a b c d e f g h i j = Pattern (a, b, c, d, e, f, g, h, i, j)

pattern T11
    :: IsPattern con (a,b,c,d,e,f,g,h,i,j,k) (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k
    -> con (a, b, c, d, e, f, g, h, i, j, k)
pattern T11 a b c d e f g h i j k = Pattern (a, b, c, d, e, f, g, h, i, j, k)

pattern T12
    :: IsPattern con (a,b,c,d,e,f,g,h,i,j,k,l) (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k, con l)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con l
    -> con (a, b, c, d, e, f, g, h, i, j, k, l)
pattern T12 a b c d e f g h i j k l = Pattern (a, b, c, d, e, f, g, h, i, j, k, l)

pattern T13
    :: IsPattern con (a,b,c,d,e,f,g,h,i,j,k,l,m) (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k, con l, con m)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con l -> con m
    -> con (a, b, c, d, e, f, g, h, i, j, k, l, m)
pattern T13 a b c d e f g h i j k l m = Pattern (a, b, c, d, e, f, g, h, i, j, k, l, m)

pattern T14
    :: IsPattern con (a,b,c,d,e,f,g,h,i,j,k,l,m,n) (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k, con l, con m, con n)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con l -> con m -> con n
    -> con (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
pattern T14 a b c d e f g h i j k l m n = Pattern (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

pattern T15
    :: IsPattern con (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k, con l, con m, con n, con o)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con l -> con m -> con n -> con o
    -> con (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
pattern T15 a b c d e f g h i j k l m n o = Pattern (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

pattern T16
    :: IsPattern con (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k, con l, con m, con n, con o, con p)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con l -> con m -> con n -> con o -> con p
    -> con (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
pattern T16 a b c d e f g h i j k l m n o p = Pattern (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

-- IsPattern instances for up to 16-tuples (Acc and Exp). TH takes care of the
-- (unremarkable) boilerplate for us, but since the implementation is a little
-- tricky it is debatable whether or not this is a good idea...
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
                ) => IsPattern $(conT con) a $b where
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

