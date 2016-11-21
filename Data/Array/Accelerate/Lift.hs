{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances  #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.Lift
-- Copyright   : [2016] Manuel M T Chakravarty, Gabriele Keller
--               [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
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
class Lift c e where
  -- | An associated-type (i.e. a type-level function) that strips all
  --   instances of surface type constructors @c@ from the input type @e@.
  --
  --   For example, the tuple types @(Exp Int, Int)@ and @(Int, Exp
  --   Int)@ have the same \"Plain\" representation.  That is, the
  --   following type equality holds:
  --
  --    @Plain (Exp Int, Int) ~ (Int,Int) ~ Plain (Int, Exp Int)@
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

instance Lift Seq (Seq a) where
  type Plain (Seq a) = a
  lift = id

instance Unlift Seq (Seq a) where
  unlift = id


-- instances for indices

instance Lift Exp () where
  type Plain () = ()
  lift _ = Exp $ Tuple NilTup

instance Unlift Exp () where
  unlift _ = ()

instance Lift Exp Z where
  type Plain Z = Z
  lift _ = Exp $ IndexNil

instance Unlift Exp Z where
  unlift _ = Z

instance (Slice (Plain ix), Lift Exp ix) => Lift Exp (ix :. Int) where
  type Plain (ix :. Int) = Plain ix :. Int
  lift (ix:.i) = Exp $ IndexCons (lift ix) (Exp $ Const i)

instance (Slice (Plain ix), Lift Exp ix) => Lift Exp (ix :. All) where
  type Plain (ix :. All) = Plain ix :. All
  lift (ix:.i) = Exp $ IndexCons (lift ix) (Exp $ Const i)

instance (Elt e, Slice (Plain ix), Lift Exp ix) => Lift Exp (ix :. Exp e) where
  type Plain (ix :. Exp e) = Plain ix :. e
  lift (ix:.i) = Exp $ IndexCons (lift ix) i

instance {-# OVERLAPPABLE #-} (Elt e, Slice (Plain ix), Unlift Exp ix) => Unlift Exp (ix :. Exp e) where
  unlift e = unlift (Exp $ IndexTail e) :. Exp (IndexHead e)

instance {-# OVERLAPPABLE #-} (Elt e, Slice ix) => Unlift Exp (Exp ix :. Exp e) where
  unlift e = (Exp $ IndexTail e) :. Exp (IndexHead e)

instance Shape sh => Lift Exp (Any sh) where
 type Plain (Any sh) = Any sh
 lift Any = Exp $ IndexAny

-- instances for numeric types

instance Lift Exp Int where
  type Plain Int = Int
  lift = Exp . Const

instance Lift Exp Int8 where
  type Plain Int8 = Int8
  lift = Exp . Const

instance Lift Exp Int16 where
  type Plain Int16 = Int16
  lift = Exp . Const

instance Lift Exp Int32 where
  type Plain Int32 = Int32
  lift = Exp . Const

instance Lift Exp Int64 where
  type Plain Int64 = Int64
  lift = Exp . Const

instance Lift Exp Word where
  type Plain Word = Word
  lift = Exp . Const

instance Lift Exp Word8 where
  type Plain Word8 = Word8
  lift = Exp . Const

instance Lift Exp Word16 where
  type Plain Word16 = Word16
  lift = Exp . Const

instance Lift Exp Word32 where
  type Plain Word32 = Word32
  lift = Exp . Const

instance Lift Exp Word64 where
  type Plain Word64 = Word64
  lift = Exp . Const

instance Lift Exp CShort where
  type Plain CShort = CShort
  lift = Exp . Const

instance Lift Exp CUShort where
  type Plain CUShort = CUShort
  lift = Exp . Const

instance Lift Exp CInt where
  type Plain CInt = CInt
  lift = Exp . Const

instance Lift Exp CUInt where
  type Plain CUInt = CUInt
  lift = Exp . Const

instance Lift Exp CLong where
  type Plain CLong = CLong
  lift = Exp . Const

instance Lift Exp CULong where
  type Plain CULong = CULong
  lift = Exp . Const

instance Lift Exp CLLong where
  type Plain CLLong = CLLong
  lift = Exp . Const

instance Lift Exp CULLong where
  type Plain CULLong = CULLong
  lift = Exp . Const

instance Lift Exp Float where
  type Plain Float = Float
  lift = Exp . Const

instance Lift Exp Double where
  type Plain Double = Double
  lift = Exp . Const

instance Lift Exp CFloat where
  type Plain CFloat = CFloat
  lift = Exp . Const

instance Lift Exp CDouble where
  type Plain CDouble = CDouble
  lift = Exp . Const

instance Lift Exp Bool where
  type Plain Bool = Bool
  lift = Exp . Const

instance Lift Exp Char where
  type Plain Char = Char
  lift = Exp . Const

instance Lift Exp CChar where
  type Plain CChar = CChar
  lift = Exp . Const

instance Lift Exp CSChar where
  type Plain CSChar = CSChar
  lift = Exp . Const

instance Lift Exp CUChar where
  type Plain CUChar = CUChar
  lift = Exp . Const

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



-- Instances for Arrays class

--instance Lift Acc () where
--  type Plain () = ()
--  lift _ = Acc (Atuple NilAtup)

instance (Shape sh, Elt e) => Lift Acc (Array sh e) where
  type Plain (Array sh e) = Array sh e
  lift = Acc . Use

instance (Lift Acc a, Lift Acc b, Arrays (Plain a), Arrays (Plain b)) => Lift Acc (a, b) where
  type Plain (a, b) = (Plain a, Plain b)
  lift (a, b) = atup2 (lift a, lift b)

instance (Arrays a, Arrays b) => Unlift Acc (Acc a, Acc b) where
  unlift = unatup2

instance (Lift Acc a, Lift Acc b, Lift Acc c,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c))
  => Lift Acc (a, b, c) where
  type Plain (a, b, c) = (Plain a, Plain b, Plain c)
  lift (a, b, c) = atup3 (lift a, lift b, lift c)

instance (Arrays a, Arrays b, Arrays c) => Unlift Acc (Acc a, Acc b, Acc c) where
  unlift = unatup3

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d))
  => Lift Acc (a, b, c, d) where
  type Plain (a, b, c, d) = (Plain a, Plain b, Plain c, Plain d)
  lift (a, b, c, d) = atup4 (lift a, lift b, lift c, lift d)

instance (Arrays a, Arrays b, Arrays c, Arrays d) => Unlift Acc (Acc a, Acc b, Acc c, Acc d) where
  unlift = unatup4

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e))
  => Lift Acc (a, b, c, d, e) where
  type Plain (a, b, c, d, e) = (Plain a, Plain b, Plain c, Plain d, Plain e)
  lift (a, b, c, d, e) = atup5 (lift a, lift b, lift c, lift d, lift e)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e) where
  unlift = unatup5

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e, Lift Acc f,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f))
  => Lift Acc (a, b, c, d, e, f) where
  type Plain (a, b, c, d, e, f) = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f)
  lift (a, b, c, d, e, f) = atup6 (lift a, lift b, lift c, lift d, lift e, lift f)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f) where
  unlift = unatup6

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e, Lift Acc f, Lift Acc g,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f),
          Arrays (Plain g))
  => Lift Acc (a, b, c, d, e, f, g) where
  type Plain (a, b, c, d, e, f, g) = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g)
  lift (a, b, c, d, e, f, g) = atup7 (lift a, lift b, lift c, lift d, lift e, lift f, lift g)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g) where
  unlift = unatup7

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e, Lift Acc f, Lift Acc g, Lift Acc h,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f),
          Arrays (Plain g), Arrays (Plain h))
  => Lift Acc (a, b, c, d, e, f, g, h) where
  type Plain (a, b, c, d, e, f, g, h)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h)
  lift (a, b, c, d, e, f, g, h)
    = atup8 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h) where
  unlift = unatup8

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e,
          Lift Acc f, Lift Acc g, Lift Acc h, Lift Acc i,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e),
          Arrays (Plain f), Arrays (Plain g), Arrays (Plain h), Arrays (Plain i))
  => Lift Acc (a, b, c, d, e, f, g, h, i) where
  type Plain (a, b, c, d, e, f, g, h, i)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i)
  lift (a, b, c, d, e, f, g, h, i)
    = atup9 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i) where
  unlift = unatup9

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e,
          Lift Acc f, Lift Acc g, Lift Acc h, Lift Acc i, Lift Acc j,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e),
          Arrays (Plain f), Arrays (Plain g), Arrays (Plain h), Arrays (Plain i), Arrays (Plain j))
  => Lift Acc (a, b, c, d, e, f, g, h, i, j) where
  type Plain (a, b, c, d, e, f, g, h, i, j)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j)
  lift (a, b, c, d, e, f, g, h, i, j)
    = atup10 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j) where
  unlift = unatup10

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e,
          Lift Acc f, Lift Acc g, Lift Acc h, Lift Acc i, Lift Acc j, Lift Acc k,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e),
          Arrays (Plain f), Arrays (Plain g), Arrays (Plain h), Arrays (Plain i), Arrays (Plain j), Arrays (Plain k))
  => Lift Acc (a, b, c, d, e, f, g, h, i, j, k) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k)
  lift (a, b, c, d, e, f, g, h, i, j, k)
    = atup11 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k) where
  unlift = unatup11

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e, Lift Acc f,
          Lift Acc g, Lift Acc h, Lift Acc i, Lift Acc j, Lift Acc k, Lift Acc l,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f),
          Arrays (Plain g), Arrays (Plain h), Arrays (Plain i), Arrays (Plain j), Arrays (Plain k), Arrays (Plain l))
  => Lift Acc (a, b, c, d, e, f, g, h, i, j, k, l) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k, l)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k, Plain l)
  lift (a, b, c, d, e, f, g, h, i, j, k, l)
    = atup12 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k, lift l)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k, Acc l) where
  unlift = unatup12

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e, Lift Acc f,
          Lift Acc g, Lift Acc h, Lift Acc i, Lift Acc j, Lift Acc k, Lift Acc l, Lift Acc m,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f),
          Arrays (Plain g), Arrays (Plain h), Arrays (Plain i), Arrays (Plain j), Arrays (Plain k), Arrays (Plain l), Arrays (Plain m))
  => Lift Acc (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k, l, m)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k, Plain l, Plain m)
  lift (a, b, c, d, e, f, g, h, i, j, k, l, m)
    = atup13 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k, lift l, lift m)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k, Acc l, Acc m) where
  unlift = unatup13

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e, Lift Acc f, Lift Acc g,
          Lift Acc h, Lift Acc i, Lift Acc j, Lift Acc k, Lift Acc l, Lift Acc m, Lift Acc n,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f), Arrays (Plain g),
          Arrays (Plain h), Arrays (Plain i), Arrays (Plain j), Arrays (Plain k), Arrays (Plain l), Arrays (Plain m), Arrays (Plain n))
  => Lift Acc (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k, Plain l, Plain m, Plain n)
  lift (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    = atup14 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k, lift l, lift m, lift n)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m, Arrays n)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k, Acc l, Acc m, Acc n) where
  unlift = unatup14

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e, Lift Acc f, Lift Acc g,
          Lift Acc h, Lift Acc i, Lift Acc j, Lift Acc k, Lift Acc l, Lift Acc m, Lift Acc n, Lift Acc o,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f), Arrays (Plain g),
          Arrays (Plain h), Arrays (Plain i), Arrays (Plain j), Arrays (Plain k), Arrays (Plain l), Arrays (Plain m), Arrays (Plain n), Arrays (Plain o))
  => Lift Acc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k, Plain l, Plain m, Plain n, Plain o)
  lift (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    = atup15 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k, lift l, lift m, lift n, lift o)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m, Arrays n, Arrays o)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k, Acc l, Acc m, Acc n, Acc o) where
  unlift = unatup15


-- Instances for Seq

instance Arrays a => Lift Seq (Acc a) where
  type Plain (Acc a) = a
  lift a = Seq (AsSeq a)

instance (Lift Seq a, Lift Seq b, Arrays (Plain a), Arrays (Plain b)) => Lift Seq (a, b) where
  type Plain (a, b) = (Plain a, Plain b)
  lift (a, b) = stup2 (lift a, lift b)

instance (Lift Seq a, Lift Seq b, Lift Seq c,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c))
  => Lift Seq (a, b, c) where
  type Plain (a, b, c) = (Plain a, Plain b, Plain c)
  lift (a, b, c) = stup3 (lift a, lift b, lift c)

instance (Lift Seq a, Lift Seq b, Lift Seq c, Lift Seq d,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d))
  => Lift Seq (a, b, c, d) where
  type Plain (a, b, c, d) = (Plain a, Plain b, Plain c, Plain d)
  lift (a, b, c, d) = stup4 (lift a, lift b, lift c, lift d)

instance (Lift Seq a, Lift Seq b, Lift Seq c, Lift Seq d, Lift Seq e,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e))
  => Lift Seq (a, b, c, d, e) where
  type Plain (a, b, c, d, e) = (Plain a, Plain b, Plain c, Plain d, Plain e)
  lift (a, b, c, d, e) = stup5 (lift a, lift b, lift c, lift d, lift e)

instance (Lift Seq a, Lift Seq b, Lift Seq c, Lift Seq d, Lift Seq e, Lift Seq f,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f))
  => Lift Seq (a, b, c, d, e, f) where
  type Plain (a, b, c, d, e, f) = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f)
  lift (a, b, c, d, e, f) = stup6 (lift a, lift b, lift c, lift d, lift e, lift f)

instance (Lift Seq a, Lift Seq b, Lift Seq c, Lift Seq d, Lift Seq e, Lift Seq f, Lift Seq g,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f),
          Arrays (Plain g))
  => Lift Seq (a, b, c, d, e, f, g) where
  type Plain (a, b, c, d, e, f, g) = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g)
  lift (a, b, c, d, e, f, g) = stup7 (lift a, lift b, lift c, lift d, lift e, lift f, lift g)

instance (Lift Seq a, Lift Seq b, Lift Seq c, Lift Seq d, Lift Seq e, Lift Seq f, Lift Seq g, Lift Seq h,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f),
          Arrays (Plain g), Arrays (Plain h))
  => Lift Seq (a, b, c, d, e, f, g, h) where
  type Plain (a, b, c, d, e, f, g, h)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h)
  lift (a, b, c, d, e, f, g, h)
    = stup8 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h)

instance (Lift Seq a, Lift Seq b, Lift Seq c, Lift Seq d, Lift Seq e,
          Lift Seq f, Lift Seq g, Lift Seq h, Lift Seq i,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e),
          Arrays (Plain f), Arrays (Plain g), Arrays (Plain h), Arrays (Plain i))
  => Lift Seq (a, b, c, d, e, f, g, h, i) where
  type Plain (a, b, c, d, e, f, g, h, i)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i)
  lift (a, b, c, d, e, f, g, h, i)
    = stup9 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i)

instance (Lift Seq a, Lift Seq b, Lift Seq c, Lift Seq d, Lift Seq e,
          Lift Seq f, Lift Seq g, Lift Seq h, Lift Seq i, Lift Seq j,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e),
          Arrays (Plain f), Arrays (Plain g), Arrays (Plain h), Arrays (Plain i), Arrays (Plain j))
  => Lift Seq (a, b, c, d, e, f, g, h, i, j) where
  type Plain (a, b, c, d, e, f, g, h, i, j)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j)
  lift (a, b, c, d, e, f, g, h, i, j)
    = stup10 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j)

instance (Lift Seq a, Lift Seq b, Lift Seq c, Lift Seq d, Lift Seq e,
          Lift Seq f, Lift Seq g, Lift Seq h, Lift Seq i, Lift Seq j, Lift Seq k,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e),
          Arrays (Plain f), Arrays (Plain g), Arrays (Plain h), Arrays (Plain i), Arrays (Plain j), Arrays (Plain k))
  => Lift Seq (a, b, c, d, e, f, g, h, i, j, k) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k)
  lift (a, b, c, d, e, f, g, h, i, j, k)
    = stup11 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k)

instance (Lift Seq a, Lift Seq b, Lift Seq c, Lift Seq d, Lift Seq e, Lift Seq f,
          Lift Seq g, Lift Seq h, Lift Seq i, Lift Seq j, Lift Seq k, Lift Seq l,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f),
          Arrays (Plain g), Arrays (Plain h), Arrays (Plain i), Arrays (Plain j), Arrays (Plain k), Arrays (Plain l))
  => Lift Seq (a, b, c, d, e, f, g, h, i, j, k, l) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k, l)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k, Plain l)
  lift (a, b, c, d, e, f, g, h, i, j, k, l)
    = stup12 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k, lift l)

instance (Lift Seq a, Lift Seq b, Lift Seq c, Lift Seq d, Lift Seq e, Lift Seq f,
          Lift Seq g, Lift Seq h, Lift Seq i, Lift Seq j, Lift Seq k, Lift Seq l, Lift Seq m,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f),
          Arrays (Plain g), Arrays (Plain h), Arrays (Plain i), Arrays (Plain j), Arrays (Plain k), Arrays (Plain l), Arrays (Plain m))
  => Lift Seq (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k, l, m)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k, Plain l, Plain m)
  lift (a, b, c, d, e, f, g, h, i, j, k, l, m)
    = stup13 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k, lift l, lift m)

instance (Lift Seq a, Lift Seq b, Lift Seq c, Lift Seq d, Lift Seq e, Lift Seq f, Lift Seq g,
          Lift Seq h, Lift Seq i, Lift Seq j, Lift Seq k, Lift Seq l, Lift Seq m, Lift Seq n,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f), Arrays (Plain g),
          Arrays (Plain h), Arrays (Plain i), Arrays (Plain j), Arrays (Plain k), Arrays (Plain l), Arrays (Plain m), Arrays (Plain n))
  => Lift Seq (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k, Plain l, Plain m, Plain n)
  lift (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    = stup14 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k, lift l, lift m, lift n)

instance (Lift Seq a, Lift Seq b, Lift Seq c, Lift Seq d, Lift Seq e, Lift Seq f, Lift Seq g,
          Lift Seq h, Lift Seq i, Lift Seq j, Lift Seq k, Lift Seq l, Lift Seq m, Lift Seq n, Lift Seq o,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f), Arrays (Plain g),
          Arrays (Plain h), Arrays (Plain i), Arrays (Plain j), Arrays (Plain k), Arrays (Plain l), Arrays (Plain m), Arrays (Plain n), Arrays (Plain o))
  => Lift Seq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  type Plain (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i, Plain j, Plain k, Plain l, Plain m, Plain n, Plain o)
  lift (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    = stup15 (lift a, lift b, lift c, lift d, lift e, lift f, lift g, lift h, lift i, lift j, lift k, lift l, lift m, lift n, lift o)
