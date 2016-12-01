{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Eq
-- Copyright   : [2016] Manuel M T Chakravarty, Gabriele Keller
--               [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Eq (

  Eq(..),
  (&&),
  (||),
  not,

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Text.Printf
import Prelude                                                      ( String, error)
import qualified Prelude                                            as P


infix 4 ==
infix 4 /=

-- | Conjunction: True if both arguments are true. This is a short-circuit
-- operator, so the second argument will be evaluated only if the first is true.
--
infixr 3 &&
(&&) :: Exp Bool -> Exp Bool -> Exp Bool
(&&) = mkLAnd

-- | Disjunction: True if either argument is true. This is a short-circuit
-- operator, so the second argument will be evaluated only if the first is
-- false.
--
infixr 2 ||
(||) :: Exp Bool -> Exp Bool -> Exp Bool
(||) = mkLOr

-- | Logical negation
--
not :: Exp Bool -> Exp Bool
not = mkLNot


-- | The 'Eq' class defines equality '==' and inequality '/=' for scalar
-- Accelerate expressions.
--
-- For convenience, we include 'Elt' as a superclass.
--
class Elt a => Eq a where
  (==) :: Exp a -> Exp a -> Exp Bool
  (/=) :: Exp a -> Exp a -> Exp Bool
  {-# MINIMAL (==) | (/=) #-}
  x == y = mkLNot (x /= y)
  x /= y = mkLNot (x == y)


instance Eq () where
  _ == _ = constant True   -- force arguments?
  _ /= _ = constant False  -- force arguments?

instance Eq Int where
  (==) = mkEq
  (/=) = mkNEq

instance Eq Int8 where
  (==) = mkEq
  (/=) = mkNEq

instance Eq Int16 where
  (==) = mkEq
  (/=) = mkNEq

instance Eq Int32 where
  (==) = mkEq
  (/=) = mkNEq

instance Eq Int64 where
  (==) = mkEq
  (/=) = mkNEq

instance Eq Word where
  (==) = mkEq
  (/=) = mkNEq

instance Eq Word8 where
  (==) = mkEq
  (/=) = mkNEq

instance Eq Word16 where
  (==) = mkEq
  (/=) = mkNEq

instance Eq Word32 where
  (==) = mkEq
  (/=) = mkNEq

instance Eq Word64 where
  (==) = mkEq
  (/=) = mkNEq

instance Eq CInt where
  (==) = mkEq
  (/=) = mkNEq

instance Eq CUInt where
  (==) = mkEq
  (/=) = mkNEq

instance Eq CLong where
  (==) = mkEq
  (/=) = mkNEq

instance Eq CULong where
  (==) = mkEq
  (/=) = mkNEq

instance Eq CLLong where
  (==) = mkEq
  (/=) = mkNEq

instance Eq CULLong where
  (==) = mkEq
  (/=) = mkNEq

instance Eq CShort where
  (==) = mkEq
  (/=) = mkNEq

instance Eq CUShort where
  (==) = mkEq
  (/=) = mkNEq

instance Eq Bool where
  (==) = mkEq
  (/=) = mkNEq

instance Eq Char where
  (==) = mkEq
  (/=) = mkNEq

instance Eq CChar where
  (==) = mkEq
  (/=) = mkNEq

instance Eq CUChar where
  (==) = mkEq
  (/=) = mkNEq

instance Eq CSChar where
  (==) = mkEq
  (/=) = mkNEq

instance Eq Float where
  (==) = mkEq
  (/=) = mkNEq

instance Eq Double where
  (==) = mkEq
  (/=) = mkNEq

instance Eq CFloat where
  (==) = mkEq
  (/=) = mkNEq

instance Eq CDouble where
  (==) = mkEq
  (/=) = mkNEq

instance (Eq a, Eq b) => Eq (a, b) where
  x == y = let (a1,b1) = untup2 x
               (a2,b2) = untup2 y
           in a1 == a2 && b1 == b2
  x /= y = let (a1,b1) = untup2 x
               (a2,b2) = untup2 y
           in a1 /= a2 || b1 /= b2

instance (Eq a, Eq b, Eq c) => Eq (a, b, c) where
  x == y = let (a1,b1,c1) = untup3 x
               (a2,b2,c2) = untup3 y
           in a1 == a2 && b1 == b2 && c1 == c2
  x /= y = let (a1,b1,c1) = untup3 x
               (a2,b2,c2) = untup3 y
           in a1 /= a2 || b1 /= b2 || c1 /= c2

instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d) where
  x == y = let (a1,b1,c1,d1) = untup4 x
               (a2,b2,c2,d2) = untup4 y
           in a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2
  x /= y = let (a1,b1,c1,d1) = untup4 x
               (a2,b2,c2,d2) = untup4 y
           in a1 /= a2 || b1 /= b2 || c1 /= c2 || d1 /= d2

instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a, b, c, d, e) where
  x == y = let (a1,b1,c1,d1,e1) = untup5 x
               (a2,b2,c2,d2,e2) = untup5 y
           in a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2
  x /= y = let (a1,b1,c1,d1,e1) = untup5 x
               (a2,b2,c2,d2,e2) = untup5 y
           in a1 /= a2 || b1 /= b2 || c1 /= c2 || d1 /= d2 || e1 /= e2

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq (a, b, c, d, e, f) where
  x == y = let (a1,b1,c1,d1,e1,f1) = untup6 x
               (a2,b2,c2,d2,e2,f2) = untup6 y
           in a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2
  x /= y = let (a1,b1,c1,d1,e1,f1) = untup6 x
               (a2,b2,c2,d2,e2,f2) = untup6 y
           in a1 /= a2 || b1 /= b2 || c1 /= c2 || d1 /= d2 || e1 /= e2 || f1 /= f2

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq (a, b, c, d, e, f, g) where
  x == y = let (a1,b1,c1,d1,e1,f1,g1) = untup7 x
               (a2,b2,c2,d2,e2,f2,g2) = untup7 y
           in a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2
  x /= y = let (a1,b1,c1,d1,e1,f1,g1) = untup7 x
               (a2,b2,c2,d2,e2,f2,g2) = untup7 y
           in a1 /= a2 || b1 /= b2 || c1 /= c2 || d1 /= d2 || e1 /= e2 || f1 /= f2 || g1 /= g2

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => Eq (a, b, c, d, e, f, g, h) where
  x == y = let (a1,b1,c1,d1,e1,f1,g1,h1) = untup8 x
               (a2,b2,c2,d2,e2,f2,g2,h2) = untup8 y
           in a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2
  x /= y = let (a1,b1,c1,d1,e1,f1,g1,h1) = untup8 x
               (a2,b2,c2,d2,e2,f2,g2,h2) = untup8 y
           in a1 /= a2 || b1 /= b2 || c1 /= c2 || d1 /= d2 || e1 /= e2 || f1 /= f2 || g1 /= g2 || h1 /= h2

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => Eq (a, b, c, d, e, f, g, h, i) where
  x == y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1) = untup9 x
               (a2,b2,c2,d2,e2,f2,g2,h2,i2) = untup9 y
           in a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 && i1 == i2
  x /= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1) = untup9 x
               (a2,b2,c2,d2,e2,f2,g2,h2,i2) = untup9 y
           in a1 /= a2 || b1 /= b2 || c1 /= c2 || d1 /= d2 || e1 /= e2 || f1 /= f2 || g1 /= g2 || h1 /= h2 || i1 /= i2

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) => Eq (a, b, c, d, e, f, g, h, i, j) where
  x == y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1) = untup10 x
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2) = untup10 y
           in a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 && i1 == i2 && j1 == j2
  x /= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1) = untup10 x
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2) = untup10 y
           in a1 /= a2 || b1 /= b2 || c1 /= c2 || d1 /= d2 || e1 /= e2 || f1 /= f2 || g1 /= g2 || h1 /= h2 || i1 /= i2 || j1 /= j2

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) => Eq (a, b, c, d, e, f, g, h, i, j, k) where
  x == y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1) = untup11 x
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2) = untup11 y
           in a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 && i1 == i2 && j1 == j2 && k1 == k2
  x /= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1) = untup11 x
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2) = untup11 y
           in a1 /= a2 || b1 /= b2 || c1 /= c2 || d1 /= d2 || e1 /= e2 || f1 /= f2 || g1 /= g2 || h1 /= h2 || i1 /= i2 || j1 /= j2 || k1 /= k2

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l) => Eq (a, b, c, d, e, f, g, h, i, j, k, l) where
  x == y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1) = untup12 x
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2) = untup12 y
           in a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 && i1 == i2 && j1 == j2 && k1 == k2 && l1 == l2
  x /= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1) = untup12 x
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2) = untup12 y
           in a1 /= a2 || b1 /= b2 || c1 /= c2 || d1 /= d2 || e1 /= e2 || f1 /= f2 || g1 /= g2 || h1 /= h2 || i1 /= i2 || j1 /= j2 || k1 /= k2 || l1 /= l2

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m) => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  x == y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1) = untup13 x
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2) = untup13 y
           in a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 && i1 == i2 && j1 == j2 && k1 == k2 && l1 == l2 && m1 == m2
  x /= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1) = untup13 x
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2) = untup13 y
           in a1 /= a2 || b1 /= b2 || c1 /= c2 || d1 /= d2 || e1 /= e2 || f1 /= f2 || g1 /= g2 || h1 /= h2 || i1 /= i2 || j1 /= j2 || k1 /= k2 || l1 /= l2 || m1 /= m2

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n) => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  x == y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1) = untup14 x
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2) = untup14 y
           in a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 && i1 == i2 && j1 == j2 && k1 == k2 && l1 == l2 && m1 == m2 && n1 == n2
  x /= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1) = untup14 x
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2) = untup14 y
           in a1 /= a2 || b1 /= b2 || c1 /= c2 || d1 /= d2 || e1 /= e2 || f1 /= f2 || g1 /= g2 || h1 /= h2 || i1 /= i2 || j1 /= j2 || k1 /= k2 || l1 /= l2 || m1 /= m2 || n1 /= n2

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o) => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  x == y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1) = untup15 x
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2) = untup15 y
           in a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 && i1 == i2 && j1 == j2 && k1 == k2 && l1 == l2 && m1 == m2 && n1 == n2 && o1 == o2
  x /= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1) = untup15 x
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2) = untup15 y
           in a1 /= a2 || b1 /= b2 || c1 /= c2 || d1 /= d2 || e1 /= e2 || f1 /= f2 || g1 /= g2 || h1 /= h2 || i1 /= i2 || j1 /= j2 || k1 /= k2 || l1 /= l2 || m1 /= m2 || n1 /= n2 || o1 /= o2


-- Instances of 'Prelude.Eq' don't make sense with the standard signatures as
-- the return type is fixed to 'Bool'. This instance is provided to provide
-- a useful error message.
--
instance P.Eq (Exp a) where
  (==) = preludeError "Eq.==" "(==)"
  (/=) = preludeError "Eq./=" "(/=)"

preludeError :: String -> String -> a
preludeError x y = error (printf "Prelude.%s applied to EDSL types: use Data.Array.Accelerate.%s instead" x y)

