{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.VOrd
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.VOrd (

  VOrd(..),

) where

import Data.Array.Accelerate.AST                                    ( PrimFun(..) )
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Classes.VEq
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type

import qualified Data.Primitive.Vec                                 as Prim
import qualified Data.Primitive.Bit                                 as Prim

import Language.Haskell.TH.Extra                                    hiding ( Type, Exp )

import Prelude                                                      hiding ( Ord(..), Ordering(..), (<*) )
import qualified Prelude                                            as P


infix 4 <*
infix 4 >*
infix 4 <=*
infix 4 >=*

-- | The 'VOrd' class defines lane-wise comparisons for totally ordered
-- data types.
--
-- @since 1.4.0.0
--
class VEq n a => VOrd n a where
  {-# MINIMAL (<=*) | vcompare #-}
  (<*)     :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  (>*)     :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  (<=*)    :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  (>=*)    :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  vmin     :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n a)
  vmax     :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n a)
  vminimum :: Exp (Vec n a) -> Exp a
  vmaximum :: Exp (Vec n a) -> Exp a
  vcompare :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Ordering)

  x <*  y  = select (vcompare x y ==* vlt) vtrue vfalse
  x <=* y  = select (vcompare x y ==* vgt) vfalse vtrue
  x >*  y  = select (vcompare x y ==* vgt) vtrue vfalse
  x >=* y  = select (vcompare x y ==* vlt) vfalse vtrue

  vmin x y = select (x <=* y) x y
  vmax x y = select (x <=* y) y x

  default vminimum :: Ord a => Exp (Vec n a) -> Exp a
  default vmaximum :: Ord a => Exp (Vec n a) -> Exp a
  vminimum x = P.minimum (unpack x)
  vmaximum x = P.maximum (unpack x)

  vcompare x y
    = select (x ==* y) veq
    $ select (x <=* y) vlt vgt

vlt, veq, vgt :: KnownNat n => Exp (Vec n Ordering)
vlt = constant (Vec (let (tag,()) = fromElt P.LT in Prim.splat tag, ()))
veq = constant (Vec (let (tag,()) = fromElt P.EQ in Prim.splat tag, ()))
vgt = constant (Vec (let (tag,()) = fromElt P.GT in Prim.splat tag, ()))

vtrue, vfalse :: KnownNat n => Exp (Vec n Bool)
vtrue  = constant (Vec (Prim.unMask Prim.ones))
vfalse = constant (Vec (Prim.unMask Prim.zeros))

runQ $ do
  let
      integralTypes :: [Name]
      integralTypes =
        [ ''Int
        , ''Int8
        , ''Int16
        , ''Int32
        , ''Int64
        , ''Int128
        , ''Word
        , ''Word8
        , ''Word16
        , ''Word32
        , ''Word64
        , ''Word128
        ]

      floatingTypes :: [Name]
      floatingTypes =
        [ ''Half
        , ''Float
        , ''Double
        , ''Float128
        ]

      nonNumTypes :: [Name]
      nonNumTypes =
        [ ''Char
        ]

      numTypes :: [Name]
      numTypes = integralTypes ++ floatingTypes

      mkPrim :: Name -> Q [Dec]
      mkPrim name =
        [d| instance KnownNat n => VOrd n $(conT name) where
              (<*)     = mkPrimBinary $ PrimLt scalarType
              (>*)     = mkPrimBinary $ PrimGt scalarType
              (<=*)    = mkPrimBinary $ PrimLtEq scalarType
              (>=*)    = mkPrimBinary $ PrimGtEq scalarType
              vmin     = mkPrimBinary $ PrimMin scalarType
              vmax     = mkPrimBinary $ PrimMax scalarType
              vminimum = mkPrimUnary $ PrimVMin scalarType
              vmaximum = mkPrimUnary $ PrimVMax scalarType
          |]

      mkTup :: Word8 -> Q Dec
      mkTup n = do
        w <- newName "w"
        x <- newName "x"
        y <- newName "y"
        let
            xs  = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
            ts  = map varT xs
            res = tupT ts
            ctx = (++) <$> mapM (appT [t| Ord |]) ts
                       <*> mapM (appT [t| SIMD $(varT w) |]) ts
            cmp f = [| pack (zipWith $f (unpack $(varE x)) (unpack $(varE y))) |]
        --
        instanceD ctx [t| VOrd $(varT w) $res |]
          [ funD (mkName "<*")  [ clause [varP x, varP y] (normalB (cmp [| (<) |]))  [] ]
          , funD (mkName ">*")  [ clause [varP x, varP y] (normalB (cmp [| (>) |]))  [] ]
          , funD (mkName "<=*") [ clause [varP x, varP y] (normalB (cmp [| (<=) |])) [] ]
          , funD (mkName ">=*") [ clause [varP x, varP y] (normalB (cmp [| (>=) |])) [] ]
          ]
  --
  ps <- concat <$> mapM mkPrim (numTypes ++ nonNumTypes)
  ts <- mapM mkTup [2..16]
  return (ps ++ ts)

instance KnownNat n => VOrd n () where
  (<*)     _ _ = vfalse
  (>*)     _ _ = vfalse
  (<=*)    _ _ = vtrue
  (>=*)    _ _ = vtrue
  vcompare _ _ = veq

instance KnownNat n => VOrd n Z where
  (<*)     _ _ = vfalse
  (>*)     _ _ = vfalse
  (<=*)    _ _ = vtrue
  (>=*)    _ _ = vtrue
  vcompare _ _ = veq

instance KnownNat n => VOrd n Ordering where
  x <*  y = mkCoerce x <*  (mkCoerce y :: Exp (Vec n TAG))
  x >*  y = mkCoerce x >*  (mkCoerce y :: Exp (Vec n TAG))
  x <=* y = mkCoerce x <=* (mkCoerce y :: Exp (Vec n TAG))
  x >=* y = mkCoerce x >=* (mkCoerce y :: Exp (Vec n TAG))

instance (Ord sh, VOrd n sh) => VOrd n (sh :. Int) where
  x <* y  = pack (zipWith (<)  (unpack x) (unpack y))
  x >* y  = pack (zipWith (>)  (unpack x) (unpack y))
  x <=* y = pack (zipWith (<=) (unpack x) (unpack y))
  x >=* y = pack (zipWith (>=) (unpack x) (unpack y))

