{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Ord
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Ord (

  Ord(..),
  Ordering(..),

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Eq

import Text.Printf
import Prelude                                                      ( ($), (.), Ordering(..), String, error, unlines )
import qualified Prelude                                            as P


infix 4 <
infix 4 >
infix 4 <=
infix 4 >=

-- | The 'Ord' class for totally ordered datatypes
--
class Eq a => Ord a where
  {-# MINIMAL (<=) | compare #-}
  (<)     :: Exp a -> Exp a -> Exp Bool
  (>)     :: Exp a -> Exp a -> Exp Bool
  (<=)    :: Exp a -> Exp a -> Exp Bool
  (>=)    :: Exp a -> Exp a -> Exp Bool
  min     :: Exp a -> Exp a -> Exp a
  max     :: Exp a -> Exp a -> Exp a
  compare :: Exp a -> Exp a -> Exp Ordering

  x <  y = if compare x y == constant LT then constant True  else constant False
  x <= y = if compare x y == constant GT then constant False else constant True
  x >  y = if compare x y == constant GT then constant True  else constant False
  x >= y = if compare x y == constant LT then constant False else constant True

  min x y = if x <= y then x else y
  max x y = if x <= y then y else x

  compare x y =
    if x == y then constant EQ else
    if x <= y then constant LT
              else constant GT

-- Local redefinition for use with RebindableSyntax (pulled forward from Prelude.hs)
--
ifThenElse :: Elt a => Exp Bool -> Exp a -> Exp a -> Exp a
ifThenElse = Exp $$$ Cond

instance Ord () where
  (<)     _ _ = constant False
  (>)     _ _ = constant False
  (>=)    _ _ = constant True
  (<=)    _ _ = constant True
  min     _ _ = constant ()
  max     _ _ = constant ()
  compare _ _ = constant EQ

instance Ord Int where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord Int8 where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord Int16 where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord Int32 where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord Int64 where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord Word where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord Word8 where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord Word16 where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord Word32 where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord Word64 where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord CInt where
  (<)  = liftB mkLt
  (>)  = liftB mkGt
  (<=) = liftB mkLtEq
  (>=) = liftB mkGtEq
  min  = lift2 mkMin
  max  = lift2 mkMax

instance Ord CUInt where
  (<)  = liftB mkLt
  (>)  = liftB mkGt
  (<=) = liftB mkLtEq
  (>=) = liftB mkGtEq
  min  = lift2 mkMin
  max  = lift2 mkMax

instance Ord CLong where
  (<)  = liftB mkLt
  (>)  = liftB mkGt
  (<=) = liftB mkLtEq
  (>=) = liftB mkGtEq
  min  = lift2 mkMin
  max  = lift2 mkMax

instance Ord CULong where
  (<)  = liftB mkLt
  (>)  = liftB mkGt
  (<=) = liftB mkLtEq
  (>=) = liftB mkGtEq
  min  = lift2 mkMin
  max  = lift2 mkMax

instance Ord CLLong where
  (<)  = liftB mkLt
  (>)  = liftB mkGt
  (<=) = liftB mkLtEq
  (>=) = liftB mkGtEq
  min  = lift2 mkMin
  max  = lift2 mkMax

instance Ord CULLong where
  (<)  = liftB mkLt
  (>)  = liftB mkGt
  (<=) = liftB mkLtEq
  (>=) = liftB mkGtEq
  min  = lift2 mkMin
  max  = lift2 mkMax

instance Ord CShort where
  (<)  = liftB mkLt
  (>)  = liftB mkGt
  (<=) = liftB mkLtEq
  (>=) = liftB mkGtEq
  min  = lift2 mkMin
  max  = lift2 mkMax

instance Ord CUShort where
  (<)  = liftB mkLt
  (>)  = liftB mkGt
  (<=) = liftB mkLtEq
  (>=) = liftB mkGtEq
  min  = lift2 mkMin
  max  = lift2 mkMax

instance Ord Bool where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord Char where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord CChar where
  (<)  = liftB mkLt
  (>)  = liftB mkGt
  (<=) = liftB mkLtEq
  (>=) = liftB mkGtEq
  min  = lift2 mkMin
  max  = lift2 mkMax

instance Ord CUChar where
  (<)  = liftB mkLt
  (>)  = liftB mkGt
  (<=) = liftB mkLtEq
  (>=) = liftB mkGtEq
  min  = lift2 mkMin
  max  = lift2 mkMax

instance Ord CSChar where
  (<)  = liftB mkLt
  (>)  = liftB mkGt
  (<=) = liftB mkLtEq
  (>=) = liftB mkGtEq
  min  = lift2 mkMin
  max  = lift2 mkMax

instance Ord Half where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord Float where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord Double where
  (<)  = mkLt
  (>)  = mkGt
  (<=) = mkLtEq
  (>=) = mkGtEq
  min  = mkMin
  max  = mkMax

instance Ord CFloat where
  (<)  = liftB mkLt
  (>)  = liftB mkGt
  (<=) = liftB mkLtEq
  (>=) = liftB mkGtEq
  min  = lift2 mkMin
  max  = lift2 mkMax

instance Ord CDouble where
  (<)  = liftB mkLt
  (>)  = liftB mkGt
  (<=) = liftB mkLtEq
  (>=) = liftB mkGtEq
  min  = lift2 mkMin
  max  = lift2 mkMax

instance (Ord a, Ord b) => Ord (a, b) where
  x <= y = let (a1,b1) = untup2 x
               (a2,b2) = untup2 y
           in a1 < a2 || (a1 == a2 && b1 <= b2)
  x >= y = let (a1,b1) = untup2 x
               (a2,b2) = untup2 y
           in a1 > a2 || (a1 == a2 && b1 >= b2)
  x < y  = let (a1,b1) = untup2 x
               (a2,b2) = untup2 y
           in a1 < a2 || (a1 == a2 && b1 < b2)
  x > y  = let (a1,b1) = untup2 x
               (a2,b2) = untup2 y
           in a1 > a2 || (a1 == a2 && b1 > b2)

instance (Ord a, Ord b, Ord c) => Ord (a, b, c) where
  x <= y = let (a1,b1,c1) = untup3 x; x' = tup2 (b1,c1)
               (a2,b2,c2) = untup3 y; y' = tup2 (b2,c2)
           in a1 < a2 || (a1 == a2 && x' <= y')
  x >= y = let (a1,b1,c1) = untup3 x; x' = tup2 (b1,c1)
               (a2,b2,c2) = untup3 y; y' = tup2 (b2,c2)
           in a1 > a2 || (a1 == a2 && x' >= y')
  x < y  = let (a1,b1,c1) = untup3 x; x' = tup2 (b1,c1)
               (a2,b2,c2) = untup3 y; y' = tup2 (b2,c2)
           in a1 < a2 || (a1 == a2 && x' < y')
  x > y  = let (a1,b1,c1) = untup3 x; x' = tup2 (b1,c1)
               (a2,b2,c2) = untup3 y; y' = tup2 (b2,c2)
           in a1 > a2 || (a1 == a2 && x' > y')

instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d) where
  x <= y = let (a1,b1,c1,d1) = untup4 x; x' = tup3 (b1,c1,d1)
               (a2,b2,c2,d2) = untup4 y; y' = tup3 (b2,c2,d2)
           in a1 < a2 || (a1 == a2 && x' <= y')
  x >= y = let (a1,b1,c1,d1) = untup4 x; x' = tup3 (b1,c1,d1)
               (a2,b2,c2,d2) = untup4 y; y' = tup3 (b2,c2,d2)
           in a1 > a2 || (a1 == a2 && x' >= y')
  x < y  = let (a1,b1,c1,d1) = untup4 x; x' = tup3 (b1,c1,d1)
               (a2,b2,c2,d2) = untup4 y; y' = tup3 (b2,c2,d2)
           in a1 < a2 || (a1 == a2 && x' < y')
  x > y  = let (a1,b1,c1,d1) = untup4 x; x' = tup3 (b1,c1,d1)
               (a2,b2,c2,d2) = untup4 y; y' = tup3 (b2,c2,d2)
           in a1 > a2 || (a1 == a2 && x' > y')

instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e) where
  x <= y = let (a1,b1,c1,d1,e1) = untup5 x; x' = tup4 (b1,c1,d1,e1)
               (a2,b2,c2,d2,e2) = untup5 y; y' = tup4 (b2,c2,d2,e2)
           in a1 < a2 || (a1 == a2 && x' <= y')
  x >= y = let (a1,b1,c1,d1,e1) = untup5 x; x' = tup4 (b1,c1,d1,e1)
               (a2,b2,c2,d2,e2) = untup5 y; y' = tup4 (b2,c2,d2,e2)
           in a1 > a2 || (a1 == a2 && x' >= y')
  x < y  = let (a1,b1,c1,d1,e1) = untup5 x; x' = tup4 (b1,c1,d1,e1)
               (a2,b2,c2,d2,e2) = untup5 y; y' = tup4 (b2,c2,d2,e2)
           in a1 < a2 || (a1 == a2 && x' < y')
  x > y  = let (a1,b1,c1,d1,e1) = untup5 x; x' = tup4 (b1,c1,d1,e1)
               (a2,b2,c2,d2,e2) = untup5 y; y' = tup4 (b2,c2,d2,e2)
           in a1 > a2 || (a1 == a2 && x' > y')

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f) => Ord (a, b, c, d, e, f) where
  x <= y = let (a1,b1,c1,d1,e1,f1) = untup6 x; x' = tup5 (b1,c1,d1,e1,f1)
               (a2,b2,c2,d2,e2,f2) = untup6 y; y' = tup5 (b2,c2,d2,e2,f2)
           in a1 < a2 || (a1 == a2 && x' <= y')
  x >= y = let (a1,b1,c1,d1,e1,f1) = untup6 x; x' = tup5 (b1,c1,d1,e1,f1)
               (a2,b2,c2,d2,e2,f2) = untup6 y; y' = tup5 (b2,c2,d2,e2,f2)
           in a1 > a2 || (a1 == a2 && x' >= y')
  x < y  = let (a1,b1,c1,d1,e1,f1) = untup6 x; x' = tup5 (b1,c1,d1,e1,f1)
               (a2,b2,c2,d2,e2,f2) = untup6 y; y' = tup5 (b2,c2,d2,e2,f2)
           in a1 < a2 || (a1 == a2 && x' < y')
  x > y  = let (a1,b1,c1,d1,e1,f1) = untup6 x; x' = tup5 (b1,c1,d1,e1,f1)
               (a2,b2,c2,d2,e2,f2) = untup6 y; y' = tup5 (b2,c2,d2,e2,f2)
           in a1 > a2 || (a1 == a2 && x' > y')

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g) => Ord (a, b, c, d, e, f, g) where
  x <= y = let (a1,b1,c1,d1,e1,f1,g1) = untup7 x; x' = tup6 (b1,c1,d1,e1,f1,g1)
               (a2,b2,c2,d2,e2,f2,g2) = untup7 y; y' = tup6 (b2,c2,d2,e2,f2,g2)
           in a1 < a2 || (a1 == a2 && x' <= y')
  x >= y = let (a1,b1,c1,d1,e1,f1,g1) = untup7 x; x' = tup6 (b1,c1,d1,e1,f1,g1)
               (a2,b2,c2,d2,e2,f2,g2) = untup7 y; y' = tup6 (b2,c2,d2,e2,f2,g2)
           in a1 > a2 || (a1 == a2 && x' >= y')
  x < y  = let (a1,b1,c1,d1,e1,f1,g1) = untup7 x; x' = tup6 (b1,c1,d1,e1,f1,g1)
               (a2,b2,c2,d2,e2,f2,g2) = untup7 y; y' = tup6 (b2,c2,d2,e2,f2,g2)
           in a1 < a2 || (a1 == a2 && x' < y')
  x > y  = let (a1,b1,c1,d1,e1,f1,g1) = untup7 x; x' = tup6 (b1,c1,d1,e1,f1,g1)
               (a2,b2,c2,d2,e2,f2,g2) = untup7 y; y' = tup6 (b2,c2,d2,e2,f2,g2)
           in a1 > a2 || (a1 == a2 && x' > y')

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h) => Ord (a, b, c, d, e, f, g, h) where
  x <= y = let (a1,b1,c1,d1,e1,f1,g1,h1) = untup8 x; x' = tup7 (b1,c1,d1,e1,f1,g1,h1)
               (a2,b2,c2,d2,e2,f2,g2,h2) = untup8 y; y' = tup7 (b2,c2,d2,e2,f2,g2,h2)
           in a1 < a2 || (a1 == a2 && x' <= y')
  x >= y = let (a1,b1,c1,d1,e1,f1,g1,h1) = untup8 x; x' = tup7 (b1,c1,d1,e1,f1,g1,h1)
               (a2,b2,c2,d2,e2,f2,g2,h2) = untup8 y; y' = tup7 (b2,c2,d2,e2,f2,g2,h2)
           in a1 > a2 || (a1 == a2 && x' >= y')
  x < y  = let (a1,b1,c1,d1,e1,f1,g1,h1) = untup8 x; x' = tup7 (b1,c1,d1,e1,f1,g1,h1)
               (a2,b2,c2,d2,e2,f2,g2,h2) = untup8 y; y' = tup7 (b2,c2,d2,e2,f2,g2,h2)
           in a1 < a2 || (a1 == a2 && x' < y')
  x > y  = let (a1,b1,c1,d1,e1,f1,g1,h1) = untup8 x; x' = tup7 (b1,c1,d1,e1,f1,g1,h1)
               (a2,b2,c2,d2,e2,f2,g2,h2) = untup8 y; y' = tup7 (b2,c2,d2,e2,f2,g2,h2)
           in a1 > a2 || (a1 == a2 && x' > y')

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i) => Ord (a, b, c, d, e, f, g, h, i) where
  x <= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1) = untup9 x; x' = tup8 (b1,c1,d1,e1,f1,g1,h1,i1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2) = untup9 y; y' = tup8 (b2,c2,d2,e2,f2,g2,h2,i2)
           in a1 < a2 || (a1 == a2 && x' <= y')
  x >= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1) = untup9 x; x' = tup8 (b1,c1,d1,e1,f1,g1,h1,i1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2) = untup9 y; y' = tup8 (b2,c2,d2,e2,f2,g2,h2,i2)
           in a1 > a2 || (a1 == a2 && x' >= y')
  x < y  = let (a1,b1,c1,d1,e1,f1,g1,h1,i1) = untup9 x; x' = tup8 (b1,c1,d1,e1,f1,g1,h1,i1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2) = untup9 y; y' = tup8 (b2,c2,d2,e2,f2,g2,h2,i2)
           in a1 < a2 || (a1 == a2 && x' < y')
  x > y  = let (a1,b1,c1,d1,e1,f1,g1,h1,i1) = untup9 x; x' = tup8 (b1,c1,d1,e1,f1,g1,h1,i1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2) = untup9 y; y' = tup8 (b2,c2,d2,e2,f2,g2,h2,i2)
           in a1 > a2 || (a1 == a2 && x' > y')

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j) => Ord (a, b, c, d, e, f, g, h, i, j) where
  x <= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1) = untup10 x; x' = tup9 (b1,c1,d1,e1,f1,g1,h1,i1,j1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2) = untup10 y; y' = tup9 (b2,c2,d2,e2,f2,g2,h2,i2,j2)
           in a1 < a2 || (a1 == a2 && x' <= y')
  x >= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1) = untup10 x; x' = tup9 (b1,c1,d1,e1,f1,g1,h1,i1,j1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2) = untup10 y; y' = tup9 (b2,c2,d2,e2,f2,g2,h2,i2,j2)
           in a1 > a2 || (a1 == a2 && x' >= y')
  x < y  = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1) = untup10 x; x' = tup9 (b1,c1,d1,e1,f1,g1,h1,i1,j1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2) = untup10 y; y' = tup9 (b2,c2,d2,e2,f2,g2,h2,i2,j2)
           in a1 < a2 || (a1 == a2 && x' < y')
  x > y  = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1) = untup10 x; x' = tup9 (b1,c1,d1,e1,f1,g1,h1,i1,j1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2) = untup10 y; y' = tup9 (b2,c2,d2,e2,f2,g2,h2,i2,j2)
           in a1 > a2 || (a1 == a2 && x' > y')

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k) => Ord (a, b, c, d, e, f, g, h, i, j, k) where
  x <= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1) = untup11 x; x' = tup10 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2) = untup11 y; y' = tup10 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2)
           in a1 < a2 || (a1 == a2 && x' <= y')
  x >= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1) = untup11 x; x' = tup10 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2) = untup11 y; y' = tup10 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2)
           in a1 > a2 || (a1 == a2 && x' >= y')
  x < y  = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1) = untup11 x; x' = tup10 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2) = untup11 y; y' = tup10 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2)
           in a1 < a2 || (a1 == a2 && x' < y')
  x > y  = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1) = untup11 x; x' = tup10 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2) = untup11 y; y' = tup10 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2)
           in a1 > a2 || (a1 == a2 && x' > y')

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l) => Ord (a, b, c, d, e, f, g, h, i, j, k, l) where
  x <= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1) = untup12 x; x' = tup11 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2) = untup12 y; y' = tup11 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2)
           in a1 < a2 || (a1 == a2 && x' <= y')
  x >= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1) = untup12 x; x' = tup11 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2) = untup12 y; y' = tup11 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2)
           in a1 > a2 || (a1 == a2 && x' >= y')
  x < y  = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1) = untup12 x; x' = tup11 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2) = untup12 y; y' = tup11 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2)
           in a1 < a2 || (a1 == a2 && x' < y')
  x > y  = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1) = untup12 x; x' = tup11 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2) = untup12 y; y' = tup11 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2)
           in a1 > a2 || (a1 == a2 && x' > y')

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m) => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  x <= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1) = untup13 x; x' = tup12 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2) = untup13 y; y' = tup12 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2)
           in a1 < a2 || (a1 == a2 && x' <= y')
  x >= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1) = untup13 x; x' = tup12 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2) = untup13 y; y' = tup12 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2)
           in a1 > a2 || (a1 == a2 && x' >= y')
  x < y  = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1) = untup13 x; x' = tup12 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2) = untup13 y; y' = tup12 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2)
           in a1 < a2 || (a1 == a2 && x' < y')
  x > y  = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1) = untup13 x; x' = tup12 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2) = untup13 y; y' = tup12 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2)
           in a1 > a2 || (a1 == a2 && x' > y')

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n) => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  x <= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1) = untup14 x; x' = tup13 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2) = untup14 y; y' = tup13 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2)
           in a1 < a2 || (a1 == a2 && x' <= y')
  x >= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1) = untup14 x; x' = tup13 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2) = untup14 y; y' = tup13 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2)
           in a1 > a2 || (a1 == a2 && x' >= y')
  x < y  = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1) = untup14 x; x' = tup13 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2) = untup14 y; y' = tup13 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2)
           in a1 < a2 || (a1 == a2 && x' < y')
  x > y  = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1) = untup14 x; x' = tup13 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2) = untup14 y; y' = tup13 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2)
           in a1 > a2 || (a1 == a2 && x' > y')

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o) => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  x <= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1) = untup15 x; x' = tup14 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2) = untup15 y; y' = tup14 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2)
           in a1 < a2 || (a1 == a2 && x' <= y')
  x >= y = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1) = untup15 x; x' = tup14 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2) = untup15 y; y' = tup14 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2)
           in a1 > a2 || (a1 == a2 && x' >= y')
  x < y  = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1) = untup15 x; x' = tup14 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2) = untup15 y; y' = tup14 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2)
           in a1 < a2 || (a1 == a2 && x' < y')
  x > y  = let (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1) = untup15 x; x' = tup14 (b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1)
               (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2) = untup15 y; y' = tup14 (b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2)
           in a1 > a2 || (a1 == a2 && x' > y')


instance Elt Ordering where
  type EltRepr Ordering = Int8
  eltType = TypeRscalar scalarType
  fromElt = P.fromIntegral . P.fromEnum
  toElt   = P.toEnum . P.fromIntegral

instance Eq Ordering where
  x == y = mkBitcast x == (mkBitcast y :: Exp Int8)
  x /= y = mkBitcast x /= (mkBitcast y :: Exp Int8)

instance Ord Ordering where
  x < y   = mkBitcast x < (mkBitcast y :: Exp Int8)
  x > y   = mkBitcast x > (mkBitcast y :: Exp Int8)
  x <= y  = mkBitcast x <= (mkBitcast y :: Exp Int8)
  x >= y  = mkBitcast x >= (mkBitcast y :: Exp Int8)
  min x y = mkBitcast $ min (mkBitcast x) (mkBitcast y :: Exp Int8)
  max x y = mkBitcast $ max (mkBitcast x) (mkBitcast y :: Exp Int8)


-- Instances of 'Prelude.Ord' (mostly) don't make sense with the standard
-- signatures as the return type is fixed to 'Bool'. This instance is provided
-- to provide a useful error message.
--
-- Note that 'min' and 'max' are implementable, so we do hook those into the
-- accelerate instances defined here. This allows us to use operations such as
-- 'Prelude.minimum' and 'Prelude.maximum'.
--
instance Ord a => P.Ord (Exp a) where
  (<)     = preludeError "Ord.(<)"  "(<)"
  (<=)    = preludeError "Ord.(<=)" "(<=)"
  (>)     = preludeError "Ord.(>)"  "(>)"
  (>=)    = preludeError "Ord.(>=)" "(>=)"
  min     = min
  max     = max

preludeError :: String -> String -> a
preludeError x y
  = error
  $ unlines [ printf "Prelude.%s applied to EDSL types: use Data.Array.Accelerate.%s instead" x y
            , ""
            , "These Prelude.Ord instances are present only to fulfil superclass"
            , "constraints for subsequent classes in the standard Haskell numeric"
            , "hierarchy."
            ]

lift2 :: (Elt a, Elt b, IsScalar b, b ~ EltRepr a)
      => (Exp b -> Exp b -> Exp b)
      -> Exp a
      -> Exp a
      -> Exp a
lift2 f x y = mkUnsafeCoerce (f (mkUnsafeCoerce x) (mkUnsafeCoerce y))

liftB :: (Elt a, Elt b, IsScalar b, b ~ EltRepr a)
      => (Exp b -> Exp b -> Exp Bool)
      -> Exp a
      -> Exp a
      -> Exp Bool
liftB f x y = f (mkUnsafeCoerce x) (mkUnsafeCoerce y)

