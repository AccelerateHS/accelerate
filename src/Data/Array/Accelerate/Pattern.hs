{-# LANGUAGE TypeOperators         #-}
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
-- Copyright   : [2018..2019] The Accelerate Team
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

  pattern Z_, pattern Ix, pattern (::.),
  pattern I0, pattern I1, pattern I2, pattern I3, pattern I4,
  pattern I5, pattern I6, pattern I7, pattern I8, pattern I9,

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Smart

import Language.Haskell.TH                                          hiding ( Exp )


-- | A pattern synonym for working with (product) data types. You can declare
-- your own pattern synonyms based off of this.
--
pattern Pattern :: forall b a context. IsPattern context a b => b -> context a
pattern Pattern vars <- (destruct @context -> vars)
  where Pattern = construct @context

class IsPattern con a t where
  construct :: t -> con a
  destruct  :: con a -> t


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

pattern Ix :: (Elt a, Elt b) => Exp a -> Exp b -> Exp (a :. b)
pattern a `Ix` b = a ::. b
{-# COMPLETE Ix #-}

pattern I0 :: Exp DIM0
pattern I0 = Z_
{-# COMPLETE I0 #-}

pattern I1 :: Elt a => Exp a -> Exp (Z :. a)
pattern I1 a = Z_ `Ix` a
{-# COMPLETE I1 #-}

pattern I2 :: (Elt a, Elt b) => Exp a -> Exp b -> Exp (Z :. a :. b)
pattern I2 a b = Z_ `Ix` a `Ix` b
{-# COMPLETE I2 #-}

pattern I3
    :: (Elt a, Elt b, Elt c)
    => Exp a -> Exp b -> Exp c
    -> Exp (Z :. a :. b :. c)
pattern I3 a b c = Z_ `Ix` a `Ix` b `Ix` c
{-# COMPLETE I3 #-}

pattern I4
    :: (Elt a, Elt b, Elt c, Elt d)
    => Exp a -> Exp b -> Exp c -> Exp d
    -> Exp (Z :. a :. b :. c :. d)
pattern I4 a b c d = Z_ `Ix` a `Ix` b `Ix` c `Ix` d
{-# COMPLETE I4 #-}

pattern I5
    :: (Elt a, Elt b, Elt c, Elt d, Elt e)
    => Exp a -> Exp b -> Exp c -> Exp d -> Exp e
    -> Exp (Z :. a :. b :. c :. d :. e)
pattern I5 a b c d e = Z_ `Ix` a `Ix` b `Ix` c `Ix` d `Ix` e
{-# COMPLETE I5 #-}

pattern I6
    :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
    => Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f
    -> Exp (Z :. a :. b :. c :. d :. e :. f)
pattern I6 a b c d e f = Z_ `Ix` a `Ix` b `Ix` c `Ix` d `Ix` e `Ix` f
{-# COMPLETE I6 #-}

pattern I7
    :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
    => Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g
    -> Exp (Z :. a :. b :. c :. d :. e :. f :. g)
pattern I7 a b c d e f g = Z_ `Ix` a `Ix` b `Ix` c `Ix` d `Ix` e `Ix` f `Ix` g
{-# COMPLETE I7 #-}

pattern I8
    :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
    => Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h
    -> Exp (Z :. a :. b :. c :. d :. e :. f :. g :. h)
pattern I8 a b c d e f g h = Z_ `Ix` a `Ix` b `Ix` c `Ix` d `Ix` e `Ix` f `Ix` g `Ix` h
{-# COMPLETE I8 #-}

pattern I9
    :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
    => Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i
    -> Exp (Z :. a :. b :. c :. d :. e :. f :. g :. h :. i)
pattern I9 a b c d e f g h i = Z_ `Ix` a `Ix` b `Ix` c `Ix` d `Ix` e `Ix` f `Ix` g `Ix` h `Ix` i
{-# COMPLETE I9 #-}


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
pattern T2 :: IsPattern con (a,b) (con a, con b) => con a -> con b -> con (a, b)
pattern T2 a b = Pattern (a, b)
{-# COMPLETE T2 :: Exp #-}
{-# COMPLETE T2 :: Acc #-}

pattern T3 :: IsPattern con (a,b,c) (con a, con b, con c) => con a -> con b -> con c -> con (a, b, c)
pattern T3 a b c = Pattern (a, b, c)
{-# COMPLETE T3 :: Exp #-}
{-# COMPLETE T3 :: Acc #-}

pattern T4
    :: IsPattern con (a,b,c,d) (con a, con b, con c, con d)
    => con a -> con b -> con c -> con d
    -> con (a, b, c, d)
pattern T4 a b c d = Pattern (a, b, c, d)
{-# COMPLETE T4 :: Exp #-}
{-# COMPLETE T4 :: Acc #-}

pattern T5
    :: IsPattern con (a,b,c,d,e) (con a, con b, con c, con d, con e)
    => con a -> con b -> con c -> con d -> con e
    -> con (a, b, c, d, e)
pattern T5 a b c d e = Pattern (a, b, c, d, e)
{-# COMPLETE T5 :: Exp #-}
{-# COMPLETE T5 :: Acc #-}

pattern T6
    :: IsPattern con (a,b,c,d,e,f) (con a, con b, con c, con d, con e, con f)
    => con a -> con b -> con c -> con d -> con e -> con f
    -> con (a, b, c, d, e, f)
pattern T6 a b c d e f = Pattern (a, b, c, d, e, f)
{-# COMPLETE T6 :: Exp #-}
{-# COMPLETE T6 :: Acc #-}

pattern T7
    :: IsPattern con (a,b,c,d,e,f,g) (con a, con b, con c, con d, con e, con f, con g)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g
    -> con (a, b, c, d, e, f, g)
pattern T7 a b c d e f g = Pattern (a, b, c, d, e, f, g)
{-# COMPLETE T7 :: Exp #-}
{-# COMPLETE T7 :: Acc #-}

pattern T8
    :: IsPattern con (a,b,c,d,e,f,g,h) (con a, con b, con c, con d, con e, con f, con g, con h)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h
    -> con (a, b, c, d, e, f, g, h)
pattern T8 a b c d e f g h = Pattern (a, b, c, d, e, f, g, h)
{-# COMPLETE T8 :: Exp #-}
{-# COMPLETE T8 :: Acc #-}

pattern T9
    :: IsPattern con (a,b,c,d,e,f,g,h,i) (con a, con b, con c, con d, con e, con f, con g, con h, con i)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i
    -> con (a, b, c, d, e, f, g, h, i)
pattern T9 a b c d e f g h i = Pattern (a, b, c, d, e, f, g, h, i)
{-# COMPLETE T9 :: Exp #-}
{-# COMPLETE T9 :: Acc #-}

pattern T10
    :: IsPattern con (a,b,c,d,e,f,g,h,i,j) (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j
    -> con (a, b, c, d, e, f, g, h, i, j)
pattern T10 a b c d e f g h i j = Pattern (a, b, c, d, e, f, g, h, i, j)
{-# COMPLETE T10 :: Exp #-}
{-# COMPLETE T10 :: Acc #-}

pattern T11
    :: IsPattern con (a,b,c,d,e,f,g,h,i,j,k) (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k
    -> con (a, b, c, d, e, f, g, h, i, j, k)
pattern T11 a b c d e f g h i j k = Pattern (a, b, c, d, e, f, g, h, i, j, k)
{-# COMPLETE T11 :: Exp #-}
{-# COMPLETE T11 :: Acc #-}

pattern T12
    :: IsPattern con (a,b,c,d,e,f,g,h,i,j,k,l) (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k, con l)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con l
    -> con (a, b, c, d, e, f, g, h, i, j, k, l)
pattern T12 a b c d e f g h i j k l = Pattern (a, b, c, d, e, f, g, h, i, j, k, l)
{-# COMPLETE T12 :: Exp #-}
{-# COMPLETE T12 :: Acc #-}

pattern T13
    :: IsPattern con (a,b,c,d,e,f,g,h,i,j,k,l,m) (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k, con l, con m)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con l -> con m
    -> con (a, b, c, d, e, f, g, h, i, j, k, l, m)
pattern T13 a b c d e f g h i j k l m = Pattern (a, b, c, d, e, f, g, h, i, j, k, l, m)
{-# COMPLETE T13 :: Exp #-}
{-# COMPLETE T13 :: Acc #-}

pattern T14
    :: IsPattern con (a,b,c,d,e,f,g,h,i,j,k,l,m,n) (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k, con l, con m, con n)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con l -> con m -> con n
    -> con (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
pattern T14 a b c d e f g h i j k l m n = Pattern (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
{-# COMPLETE T14 :: Exp #-}
{-# COMPLETE T14 :: Acc #-}

pattern T15
    :: IsPattern con (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k, con l, con m, con n, con o)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con l -> con m -> con n -> con o
    -> con (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
pattern T15 a b c d e f g h i j k l m n o = Pattern (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
{-# COMPLETE T15 :: Exp #-}
{-# COMPLETE T15 :: Acc #-}

pattern T16
    :: IsPattern con (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) (con a, con b, con c, con d, con e, con f, con g, con h, con i, con j, con k, con l, con m, con n, con o, con p)
    => con a -> con b -> con c -> con d -> con e -> con f -> con g -> con h -> con i -> con j -> con k -> con l -> con m -> con n -> con o -> con p
    -> con (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
pattern T16 a b c d e f g h i j k l m n o p = Pattern (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
{-# COMPLETE T16 :: Exp #-}
{-# COMPLETE T16 :: Acc #-}

-- IsPattern instances for Shape nil and cons
--
instance IsPattern Exp Z Z where
  construct _ = Exp IndexNil
  destruct _  = Z

instance (Elt a, Elt b) => IsPattern Exp (a :. b) (Exp a :. Exp b) where
  construct (a :. b) = Exp (a `IndexCons` b)
  destruct t         = Exp (IndexTail t) :. Exp (IndexHead t)

-- IsPattern instances for up to 16-tuples (Acc and Exp). TH takes care of the
-- (unremarkable) boilerplate for us, but since the implementation is a little
-- tricky it is debatable whether or not this is a good idea...
--
$(runQ $ do
    let
        mkIsPattern :: Name -> TypeQ -> ExpQ -> ExpQ -> ExpQ -> ExpQ -> Int -> Q [Dec]
        mkIsPattern con cst tup prj nil snoc n =
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
                  destruct _x = $(tupE (map (get [|_x|]) [(n-1), (n-2) .. 0]))
            |]

        mkAccPattern = mkIsPattern (mkName "Acc") [t| Arrays |] [| Atuple |] [| Aprj |] [| NilAtup |] [| SnocAtup |]
        mkExpPattern = mkIsPattern (mkName "Exp") [t| Elt    |] [| Tuple  |] [| Prj  |] [| NilTup  |] [| SnocTup  |]
    --
    --
    as <- mapM mkAccPattern [0..16]
    es <- mapM mkExpPattern [0..16]
    return (concat as ++ concat es)
 )

