{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE UndecidableInstances  #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.Lift
-- Copyright   : [2016..2020] The Accelerate Team
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

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Type

import Language.Haskell.TH.Extra                                    hiding ( Exp )


-- | Lift a unary function into 'Exp'.
--
lift1 :: (HasCallStack, Unlift Exp a, Lift Exp b)
      => (a -> b)
      -> Exp (Plain a)
      -> Exp (Plain b)
lift1 f = sourceMap $ lift . f . unlift

-- | Lift a binary function into 'Exp'.
--
lift2 :: (HasCallStack, Unlift Exp a, Unlift Exp b, Lift Exp c)
      => (a -> b -> c)
      -> Exp (Plain a)
      -> Exp (Plain b)
      -> Exp (Plain c)
lift2 f x y = sourceMap $ lift $ f (unlift x) (unlift y)

-- | Lift a ternary function into 'Exp'.
--
lift3 :: (HasCallStack, Unlift Exp a, Unlift Exp b, Unlift Exp c, Lift Exp d)
      => (a -> b -> c -> d)
      -> Exp (Plain a)
      -> Exp (Plain b)
      -> Exp (Plain c)
      -> Exp (Plain d)
lift3 f x y z = sourceMap $ lift $ f (unlift x) (unlift y) (unlift z)

-- | Lift a unary function to a computation over rank-1 indices.
--
ilift1 :: HasCallStack => (Exp Int -> Exp Int) -> Exp DIM1 -> Exp DIM1
ilift1 f = sourceMap $ lift1 (\(Z:.i) -> Z :. f i)

-- | Lift a binary function to a computation over rank-1 indices.
--
ilift2 :: HasCallStack => (Exp Int -> Exp Int -> Exp Int) -> Exp DIM1 -> Exp DIM1 -> Exp DIM1
ilift2 f = sourceMap $ lift2 (\(Z:.i) (Z:.j) -> Z :. f i j)

-- | Lift a ternary function to a computation over rank-1 indices.
--
ilift3 :: HasCallStack => (Exp Int -> Exp Int -> Exp Int -> Exp Int) -> Exp DIM1 -> Exp DIM1 -> Exp DIM1 -> Exp DIM1
ilift3 f = sourceMap $ lift3 (\(Z:.i) (Z:.j) (Z:.k) -> Z :. f i j k)


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
  lift :: HasCallStack => e -> c (Plain e)

-- | A limited subset of types which can be lifted, can also be unlifted.
class Lift c e => Unlift c e where

  -- | Unlift the outermost constructor through the surface type. This is only
  -- possible if the constructor is fully determined by its type - i.e., it is a
  -- singleton.
  --
  unlift :: HasCallStack => c (Plain e) -> e


-- Identity instances
-- ------------------

instance Lift Exp (Exp e) where
  type Plain (Exp e) = e
  lift = sourceMap id

instance Unlift Exp (Exp e) where
  unlift = sourceMap id

instance Lift Acc (Acc a) where
  type Plain (Acc a) = a
  lift = sourceMap id

instance Unlift Acc (Acc a) where
  unlift = sourceMap id

-- instance Lift Seq (Seq a) where
--   type Plain (Seq a) = a
--   lift = id

-- instance Unlift Seq (Seq a) where
--   unlift = id


-- Instances for indices
-- ---------------------

instance Lift Exp Z where
  type Plain Z = Z
  lift _ = sourceMap Z_

instance Unlift Exp Z where
  unlift _ = Z

instance (Elt (Plain ix), Lift Exp ix) => Lift Exp (ix :. Int) where
  type Plain (ix :. Int) = Plain ix :. Int
  lift = sourceMap $ \(ix :. i) -> lift ix ::. lift i

instance (Elt (Plain ix), Lift Exp ix) => Lift Exp (ix :. All) where
  type Plain (ix :. All) = Plain ix :. All
  lift = sourceMap $ \(ix :. i) -> lift ix ::. constant i

instance (Elt e, Elt (Plain ix), Lift Exp ix) => Lift Exp (ix :. Exp e) where
  type Plain (ix :. Exp e) = Plain ix :. e
  lift = sourceMap $ \(ix :. i) -> lift ix ::. i

instance {-# OVERLAPPABLE #-} (Elt e, Elt (Plain ix), Unlift Exp ix) => Unlift Exp (ix :. Exp e) where
  unlift = sourceMap $ \(ix ::. i) -> unlift ix :. i

instance {-# OVERLAPPABLE #-} (Elt e, Elt ix) => Unlift Exp (Exp ix :. Exp e) where
  unlift = sourceMap $ \(ix ::. i) -> ix :. i

instance (Shape sh, Elt (Any sh)) => Lift Exp (Any sh) where
  type Plain (Any sh) = Any sh
  lift Any = sourceMap $ constant Any

-- Instances for numeric types
-- ---------------------------

{-# INLINE expConst #-}
expConst :: forall e. (SourceMapped, Elt e, IsScalar (EltR e)) => e -> Exp e
expConst = Exp . SmartExp . Const mkAnn (scalarType @(EltR e)) . fromElt

instance Lift Exp Int where
  type Plain Int = Int
  lift = sourceMap expConst

instance Lift Exp Int8 where
  type Plain Int8 = Int8
  lift = sourceMap expConst

instance Lift Exp Int16 where
  type Plain Int16 = Int16
  lift = sourceMap expConst

instance Lift Exp Int32 where
  type Plain Int32 = Int32
  lift = sourceMap expConst

instance Lift Exp Int64 where
  type Plain Int64 = Int64
  lift = sourceMap expConst

instance Lift Exp Word where
  type Plain Word = Word
  lift = sourceMap expConst

instance Lift Exp Word8 where
  type Plain Word8 = Word8
  lift = sourceMap expConst

instance Lift Exp Word16 where
  type Plain Word16 = Word16
  lift = sourceMap expConst

instance Lift Exp Word32 where
  type Plain Word32 = Word32
  lift = sourceMap expConst

instance Lift Exp Word64 where
  type Plain Word64 = Word64
  lift = sourceMap expConst

instance Lift Exp CShort where
  type Plain CShort = CShort
  lift = sourceMap expConst

instance Lift Exp CUShort where
  type Plain CUShort = CUShort
  lift = sourceMap expConst

instance Lift Exp CInt where
  type Plain CInt = CInt
  lift = sourceMap expConst

instance Lift Exp CUInt where
  type Plain CUInt = CUInt
  lift = sourceMap expConst

instance Lift Exp CLong where
  type Plain CLong = CLong
  lift = sourceMap expConst

instance Lift Exp CULong where
  type Plain CULong = CULong
  lift = sourceMap expConst

instance Lift Exp CLLong where
  type Plain CLLong = CLLong
  lift = sourceMap expConst

instance Lift Exp CULLong where
  type Plain CULLong = CULLong
  lift = sourceMap expConst

instance Lift Exp Half where
  type Plain Half = Half
  lift = sourceMap expConst

instance Lift Exp Float where
  type Plain Float = Float
  lift = sourceMap expConst

instance Lift Exp Double where
  type Plain Double = Double
  lift = sourceMap expConst

instance Lift Exp CFloat where
  type Plain CFloat = CFloat
  lift = sourceMap expConst

instance Lift Exp CDouble where
  type Plain CDouble = CDouble
  lift = sourceMap expConst

instance Lift Exp Bool where
  type Plain Bool = Bool
  lift True  = sourceMap $ Exp . SmartExp $ Pair mkAnn (SmartExp (Const mkAnn scalarType 1)) (SmartExp (Nil mkAnn))
  lift False = sourceMap $ Exp . SmartExp $ Pair mkAnn (SmartExp (Const mkAnn scalarType 0)) (SmartExp (Nil mkAnn))

instance Lift Exp Char where
  type Plain Char = Char
  lift = sourceMap expConst

instance Lift Exp CChar where
  type Plain CChar = CChar
  lift = sourceMap expConst

instance Lift Exp CSChar where
  type Plain CSChar = CSChar
  lift = sourceMap expConst

instance Lift Exp CUChar where
  type Plain CUChar = CUChar
  lift = sourceMap expConst

-- Instances for tuples
-- --------------------

instance Lift Exp () where
  type Plain () = ()
  lift _ = sourceMap $ Exp (SmartExp (Nil mkAnn))

instance Unlift Exp () where
  unlift _ = ()

instance Lift Acc () where
  type Plain () = ()
  lift _ = sourceMap $ Acc (SmartAcc (Anil mkAnn))

instance Unlift Acc () where
  unlift _ = ()

instance (Shape sh, Elt e) => Lift Acc (Array sh e) where
  type Plain (Array sh e) = Array sh e
  lift (Array arr) = sourceMap $ Acc $ SmartAcc $ Use mkAnn (arrayR @sh @e) arr

-- Lift and Unlift instances for tuples
--
runQ $ do
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
                  sourceMap $
                  $(conE con)
                  $(foldl (\vs v -> do _v <- newName "_v"
                                       [| let $(conP con [varP _v]) = lift $(varE v)
                                           in $smart ($pair $vs $(varE _v)) |]) [| $smart $nil |] xs)

              instance $ctx3 => Unlift $(conT con) $res2 where
                unlift $(conP con [varP _x]) =
                  sourceMap $
                  $(tupE (map (get (varE _x)) [(n-1), (n-2) .. 0]))
            |]

        mkAccInstances = mkInstances (mkName "Acc") [t| Arrays |] [| SmartAcc |] [| Aprj mkAnn |] [| Anil mkAnn |] [| Apair mkAnn |]
        mkExpInstances = mkInstances (mkName "Exp") [t| Elt    |] [| SmartExp |] [| Prj mkAnn  |] [| Nil mkAnn  |] [| Pair mkAnn  |]
    --
    as <- mapM mkAccInstances [2..16]
    es <- mapM mkExpInstances [2..16]
    return $ concat (as ++ es)

