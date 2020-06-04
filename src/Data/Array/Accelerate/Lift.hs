{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances  #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE UndecidableInstances  #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.Lift
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lifting and lowering surface expressions through constructors.
--

module Data.Array.Accelerate.Lift (

  -- * Lifting and unlifting
  Lift(..), Unlift(..),

  lift1, lift2, lift3,
  ilift1, ilift2, ilift3,

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Language.Haskell.TH                                          hiding ( Exp )
import Language.Haskell.TH.Extra


-- |Lift a unary function into 'Exp'.
--
lift1 :: (Unlift Exp a, Lift Exp b)
      => (a -> b)
      -> Exp (Plain a)
      -> Exp (Plain b)
lift1 f = lift . f . unlift

-- |Lift a binary function into 'Exp'.
--
lift2 :: (Unlift Exp a, Unlift Exp b, Lift Exp c)
      => (a -> b -> c)
      -> Exp (Plain a)
      -> Exp (Plain b)
      -> Exp (Plain c)
lift2 f x y = lift $ f (unlift x) (unlift y)

-- |Lift a ternary function into 'Exp'.
--
lift3 :: (Unlift Exp a, Unlift Exp b, Unlift Exp c, Lift Exp d)
      => (a -> b -> c -> d)
      -> Exp (Plain a)
      -> Exp (Plain b)
      -> Exp (Plain c)
      -> Exp (Plain d)
lift3 f x y z = lift $ f (unlift x) (unlift y) (unlift z)

-- |Lift a unary function to a computation over rank-1 indices.
--
ilift1 :: (Exp Int -> Exp Int) -> Exp DIM1 -> Exp DIM1
ilift1 f = lift1 (\(Z:.i) -> Z :. f i)

-- |Lift a binary function to a computation over rank-1 indices.
--
ilift2 :: (Exp Int -> Exp Int -> Exp Int) -> Exp DIM1 -> Exp DIM1 -> Exp DIM1
ilift2 f = lift2 (\(Z:.i) (Z:.j) -> Z :. f i j)

-- |Lift a ternary function to a computation over rank-1 indices.
--
ilift3 :: (Exp Int -> Exp Int -> Exp Int -> Exp Int) -> Exp DIM1 -> Exp DIM1 -> Exp DIM1 -> Exp DIM1
ilift3 f = lift3 (\(Z:.i) (Z:.j) (Z:.k) -> Z :. f i j k)


-- | The class of types @e@ which can be lifted into @c@.
--
class Lift c e where
  -- | An associated-type (i.e. a type-level function) that strips all
  --   instances of surface type constructors @c@ from the input type @e@.
  --
  --   For example, the tuple types @(Exp Int, Int)@ and @(Int, Exp
  --   Int)@ have the same \"Plain\" representation.  That is, the
  --   following type equality holds:
  --
  --    @Plain (Exp Int, Int) ~ (Int,Int) ~ Plain (Int, Exp Int)@
  --
  type Plain e

  -- | Lift the given value into a surface type 'c' --- either 'Exp' for scalar
  -- expressions or 'Acc' for array computations. The value may already contain
  -- subexpressions in 'c'.
  --
  lift :: e -> c (Plain e)

-- | A limited subset of types which can be lifted, can also be unlifted.
class Lift c e => Unlift c e where

  -- | Unlift the outermost constructor through the surface type. This is only
  -- possible if the constructor is fully determined by its type - i.e., it is a
  -- singleton.
  --
  unlift :: c (Plain e) -> e


-- identity instances

instance Lift Exp (Exp e) where
  type Plain (Exp e) = e
  lift = id

instance Unlift Exp (Exp e) where
  unlift = id

instance Lift Acc (Acc a) where
  type Plain (Acc a) = a
  lift = id

instance Unlift Acc (Acc a) where
  unlift = id

-- instance Lift Seq (Seq a) where
--   type Plain (Seq a) = a
--   lift = id

-- instance Unlift Seq (Seq a) where
--   unlift = id


-- instances for indices

instance Lift Exp () where
  type Plain () = ()
  lift _ = Exp $ SmartExp Nil

instance Unlift Exp () where
  unlift _ = ()

instance Lift Exp Z where
  type Plain Z = Z
  lift _ = Exp $ SmartExp Nil

instance Unlift Exp Z where
  unlift _ = Z

instance (Elt (Plain ix), Lift Exp ix) => Lift Exp (ix :. Int) where
  type Plain (ix :. Int) = Plain ix :. Int
  lift (ix:.i) = Exp $ SmartExp $ Pair (unExp $ lift ix) (unExp $ expConst i)

instance (Elt (Plain ix), Lift Exp ix) => Lift Exp (ix :. All) where
  type Plain (ix :. All) = Plain ix :. All
  lift (ix:.i) = Exp $ SmartExp $ Pair (unExp $ lift ix) (unExp $ constant i)

instance (Elt e, Elt (Plain ix), Lift Exp ix) => Lift Exp (ix :. Exp e) where
  type Plain (ix :. Exp e) = Plain ix :. e
  lift (ix :. Exp i) = Exp $ SmartExp $ Pair (unExp $ lift ix) i

instance {-# OVERLAPPABLE #-} (Elt e, Elt (Plain ix), Unlift Exp ix) => Unlift Exp (ix :. Exp e) where
  unlift (Exp e) = unlift (Exp $ SmartExp $ Prj PairIdxLeft e) :. Exp (SmartExp $ Prj PairIdxRight e)

instance {-# OVERLAPPABLE #-} (Elt e, Elt ix) => Unlift Exp (Exp ix :. Exp e) where
  unlift (Exp e) = (Exp $ SmartExp $ Prj PairIdxLeft e) :. Exp (SmartExp $ Prj PairIdxRight e)

instance (Shape sh, Elt (Any sh)) => Lift Exp (Any sh) where
  type Plain (Any sh) = Any sh
  lift Any = constant Any

-- instances for numeric types

{-# INLINE expConst #-}
expConst :: forall e. Elt e => IsScalar (EltRepr e) => e -> Exp e
expConst = Exp . SmartExp . Const (scalarType @(EltRepr e)) . fromElt

instance Lift Exp Int where
  type Plain Int = Int
  lift = expConst

instance Lift Exp Int8 where
  type Plain Int8 = Int8
  lift = expConst

instance Lift Exp Int16 where
  type Plain Int16 = Int16
  lift = expConst

instance Lift Exp Int32 where
  type Plain Int32 = Int32
  lift = expConst

instance Lift Exp Int64 where
  type Plain Int64 = Int64
  lift = expConst

instance Lift Exp Word where
  type Plain Word = Word
  lift = expConst

instance Lift Exp Word8 where
  type Plain Word8 = Word8
  lift = expConst

instance Lift Exp Word16 where
  type Plain Word16 = Word16
  lift = expConst

instance Lift Exp Word32 where
  type Plain Word32 = Word32
  lift = expConst

instance Lift Exp Word64 where
  type Plain Word64 = Word64
  lift = expConst

instance Lift Exp CShort where
  type Plain CShort = CShort
  lift = expConst

instance Lift Exp CUShort where
  type Plain CUShort = CUShort
  lift = expConst

instance Lift Exp CInt where
  type Plain CInt = CInt
  lift = expConst

instance Lift Exp CUInt where
  type Plain CUInt = CUInt
  lift = expConst

instance Lift Exp CLong where
  type Plain CLong = CLong
  lift = expConst

instance Lift Exp CULong where
  type Plain CULong = CULong
  lift = expConst

instance Lift Exp CLLong where
  type Plain CLLong = CLLong
  lift = expConst

instance Lift Exp CULLong where
  type Plain CULLong = CULLong
  lift = expConst

instance Lift Exp Half where
  type Plain Half = Half
  lift = expConst

instance Lift Exp Float where
  type Plain Float = Float
  lift = expConst

instance Lift Exp Double where
  type Plain Double = Double
  lift = expConst

instance Lift Exp CFloat where
  type Plain CFloat = CFloat
  lift = expConst

instance Lift Exp CDouble where
  type Plain CDouble = CDouble
  lift = expConst

instance Lift Exp Bool where
  type Plain Bool = Bool
  lift = expConst

instance Lift Exp Char where
  type Plain Char = Char
  lift = expConst

instance Lift Exp CChar where
  type Plain CChar = CChar
  lift = expConst

instance Lift Exp CSChar where
  type Plain CSChar = CSChar
  lift = expConst

instance Lift Exp CUChar where
  type Plain CUChar = CUChar
  lift = expConst

-- Instances for tuples

instance (Lift Exp a, Lift Exp b, Elt (Plain a), Elt (Plain b)) => Lift Exp (a, b) where
  type Plain (a, b) = (Plain a, Plain b)
  lift (a, b) = tup2 (lift a, lift b)

instance (Elt a, Elt b) => Unlift Exp (Exp a, Exp b) where
  unlift = untup2

instance (Lift Exp a, Lift Exp b, Lift Exp c,
          Elt (Plain a), Elt (Plain b), Elt (Plain c))
  => Lift Exp (a, b, c) where
  type Plain (a, b, c) = (Plain a, Plain b, Plain c)
  lift (a, b, c) = tup3 (lift a, lift b, lift c)

instance (Elt a, Elt b, Elt c) => Unlift Exp (Exp a, Exp b, Exp c) where
  unlift = untup3

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d))
  => Lift Exp (a, b, c, d) where
  type Plain (a, b, c, d) = (Plain a, Plain b, Plain c, Plain d)
  lift (a, b, c, d) = tup4 (lift a, lift b, lift c, lift d)

instance (Elt a, Elt b, Elt c, Elt d) => Unlift Exp (Exp a, Exp b, Exp c, Exp d) where
  unlift = untup4

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e))
  => Lift Exp (a, b, c, d, e) where
  type Plain (a, b, c, d, e) = (Plain a, Plain b, Plain c, Plain d, Plain e)
  lift (a, b, c, d, e) = tup5 (lift a, lift b, lift c, lift d, lift e)

instance (Elt a, Elt b, Elt c, Elt d, Elt e)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e) where
  unlift = untup5

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e, Lift Exp f,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f))
  => Lift Exp (a, b, c, d, e, f) where
  type Plain (a, b, c, d, e, f) = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f)
  lift (a, b, c, d, e, f) = tup6 (lift a, lift b, lift c, lift d, lift e, lift f)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) where
  unlift = untup6

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e, Lift Exp f, Lift Exp g,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f),
          Elt (Plain g))
  => Lift Exp (a, b, c, d, e, f, g) where
  type Plain (a, b, c, d, e, f, g) = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g)
  lift (a, b, c, d, e, f, g) = tup7 (lift a, lift b, lift c, lift d, lift e, lift f, lift g)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) where
  unlift = untup7

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e, Lift Exp f, Lift Exp g, Lift Exp h,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f),
          Elt (Plain g), Elt (Plain h))
  => Lift Exp (a, b, c, d, e, f, g, h) where
  type Plain (a, b, c, d, e, f, g, h)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h)
  lift (a, b, c, d, e, f, g, h)
    = tup8 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) where
  unlift = untup8

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e,
          Lift Exp f, Lift Exp g, Lift Exp h, Lift Exp i,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e),
          Elt (Plain f), Elt (Plain g), Elt (Plain h), Elt (Plain i))
  => Lift Exp (a, b, c, d, e, f, g, h, i) where
  type Plain (a, b, c, d, e, f, g, h, i)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i)
  lift (a, b, c, d, e, f, g, h, i)
    = tup9 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) where
  unlift = untup9

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e,
          Lift Exp f, Lift Exp g, Lift Exp h, Lift Exp i, Lift Exp j,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e),
          Elt (Plain f), Elt (Plain g), Elt (Plain h), Elt (Plain i), Elt (Plain j))
  => Lift Exp (a, b, c, d, e, f, g, h, i, j) where
  type Plain (a, b, c, d, e, f, g, h, i, j)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j)
  lift (a, b, c, d, e, f, g, h, i, j)
    = tup10 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j) where
  unlift = untup10

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e,
          Lift Exp f, Lift Exp g, Lift Exp h, Lift Exp i, Lift Exp j, Lift Exp k,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e),
          Elt (Plain f), Elt (Plain g), Elt (Plain h), Elt (Plain i), Elt (Plain j), Elt (Plain k))
  => Lift Exp (a, b, c, d, e, f, g, h, i, j, k) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k)
  lift (a, b, c, d, e, f, g, h, i, j, k)
    = tup11 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k) where
  unlift = untup11

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e, Lift Exp f,
          Lift Exp g, Lift Exp h, Lift Exp i, Lift Exp j, Lift Exp k, Lift Exp l,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f),
          Elt (Plain g), Elt (Plain h), Elt (Plain i), Elt (Plain j), Elt (Plain k), Elt (Plain l))
  => Lift Exp (a, b, c, d, e, f, g, h, i, j, k, l) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k, l)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k, Plain l)
  lift (a, b, c, d, e, f, g, h, i, j, k, l)
    = tup12 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k, lift l)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l) where
  unlift = untup12

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e, Lift Exp f,
          Lift Exp g, Lift Exp h, Lift Exp i, Lift Exp j, Lift Exp k, Lift Exp l, Lift Exp m,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f),
          Elt (Plain g), Elt (Plain h), Elt (Plain i), Elt (Plain j), Elt (Plain k), Elt (Plain l), Elt (Plain m))
  => Lift Exp (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k, l, m)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k, Plain l, Plain m)
  lift (a, b, c, d, e, f, g, h, i, j, k, l, m)
    = tup13 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k, lift l, lift m)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l, Exp m) where
  unlift = untup13

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e, Lift Exp f, Lift Exp g,
          Lift Exp h, Lift Exp i, Lift Exp j, Lift Exp k, Lift Exp l, Lift Exp m, Lift Exp n,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f), Elt (Plain g),
          Elt (Plain h), Elt (Plain i), Elt (Plain j), Elt (Plain k), Elt (Plain l), Elt (Plain m), Elt (Plain n))
  => Lift Exp (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k, Plain l, Plain m, Plain n)
  lift (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    = tup14 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k, lift l, lift m, lift n)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l, Exp m, Exp n) where
  unlift = untup14

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e, Lift Exp f, Lift Exp g,
          Lift Exp h, Lift Exp i, Lift Exp j, Lift Exp k, Lift Exp l, Lift Exp m, Lift Exp n, Lift Exp o,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f), Elt (Plain g),
          Elt (Plain h), Elt (Plain i), Elt (Plain j), Elt (Plain k), Elt (Plain l), Elt (Plain m), Elt (Plain n), Elt (Plain o))
  => Lift Exp (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k, Plain l, Plain m, Plain n, Plain o)
  lift (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    = tup15 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k, lift l, lift m, lift n, lift o)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n, Elt o)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l, Exp m, Exp n, Exp o) where
  unlift = untup15

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e, Lift Exp f, Lift Exp g, Lift Exp h,
          Lift Exp i, Lift Exp j, Lift Exp k, Lift Exp l, Lift Exp m, Lift Exp n, Lift Exp o, Lift Exp p,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f), Elt (Plain g), Elt (Plain h),
          Elt (Plain i), Elt (Plain j), Elt (Plain k), Elt (Plain l), Elt (Plain m), Elt (Plain n), Elt (Plain o), Elt (Plain p))
  => Lift Exp (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k, Plain l, Plain m, Plain n, Plain o, Plain p)
  lift (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    = tup16 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k, lift l, lift m, lift n, lift o, lift p)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n, Elt o, Elt p)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l, Exp m, Exp n, Exp o, Exp p) where
  unlift = untup16


instance Lift Acc () where
  type Plain () = ()
  lift _ = Acc (SmartAcc Anil)

instance (Shape sh, Elt e) => Lift Acc (Array sh e) where
  type Plain (Array sh e) = Array sh e
  lift (Array arr) = Acc $ SmartAcc $ Use (arrayR @sh @e) arr

-- Lift and Unlift instances for tuples
--
$(runQ $ do
    let
        mkInstances :: Name -> TypeQ -> ExpQ -> ExpQ -> ExpQ -> ExpQ -> Int -> Q [Dec]
        mkInstances con cst smart prj nil pair n = do
          let
              xs      = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              ts      = map varT xs
              res1    = tupT ts
              res2    = tupT (map (conT con `appT`) ts)
              plain   = tupT (map (\t -> [t| Plain $t |]) ts)
              ctx1    = tupT (map (\t -> [t| Lift $(conT con) $t |]) ts)
              ctx2    = tupT (map (\t -> [t| $cst (Plain $t) |]) ts)
              ctx3    = tupT (map (appT cst) ts)
              --
              get x 0 = [| $(conE con) ($smart ($prj PairIdxRight $x)) |]
              get x i = get [| $smart ($prj PairIdxLeft $x) |] (i-1)
          --
          _x <- newName "_x"
          [d| instance ($ctx1, $ctx2) => Lift $(conT con) $res1 where
                type Plain $res1 = $plain
                lift $(tupP (map varP xs)) =
                  $(conE con)
                  $(foldl (\vs v -> do _v <- newName "_v"
                                       [| let $(conP con [varP _v]) = lift $(varE v)
                                           in $smart ($pair $vs $(varE _v)) |]) [| $smart $nil |] xs)

              instance $ctx3 => Unlift $(conT con) $res2 where
                unlift $(conP con [varP _x]) =
                  $(tupE (map (get (varE _x)) [(n-1), (n-2) .. 0]))
            |]

        mkAccInstances = mkInstances (mkName "Acc") [t| Arrays |] [| SmartAcc |] [| Aprj |] [| Anil |] [| Apair |]
    --
    as <- mapM mkAccInstances [2..16]
    return $ concat as
 )

