{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.VEq
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.VEq (

  VEq(..),
  (&&*),
  (||*),
  vnot,
  vand,
  vor,

) where

import Data.Array.Accelerate.AST                                    ( PrimFun(..) )
import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type

import qualified Data.Primitive.Bit                                 as Prim

import Language.Haskell.TH.Extra                                    hiding ( Type, Exp )

import Prelude                                                      hiding ( Eq(..) )


-- | Vectorised conjunction: Element-wise returns true if both arguments in
-- the corresponding lane are True. This is a strict vectorised version of
-- '(Data.Array.Accelerate.&&)' that always evaluates both arguments.
--
-- @since 1.4.0.0
--
infixr 3 &&*
(&&*) :: KnownNat n => Exp (Vec n Bool) -> Exp (Vec n Bool) -> Exp (Vec n Bool)
(&&*) = mkPrimBinary $ PrimLAnd bitType

-- | Vectorised disjunction: Element-wise returns true if either argument
-- in the corresponding lane is true. This is a strict vectorised version
-- of '(Data.Array.Accelerate.||)' that always evaluates both arguments.
--
-- @since 1.4.0.0
--
infixr 2 ||*
(||*) :: KnownNat n => Exp (Vec n Bool) -> Exp (Vec n Bool) -> Exp (Vec n Bool)
(||*) = mkPrimBinary $ PrimLOr bitType

-- | Vectorised logical negation
--
-- @since 1.4.0.0
--
vnot :: KnownNat n => Exp (Vec n Bool) -> Exp (Vec n Bool)
vnot = mkPrimUnary $ PrimLNot bitType

-- | Return 'True' if all lanes of the vector are 'True'
--
-- @since 1.4.0.0
--
vand :: KnownNat n => Exp (Vec n Bool) -> Exp Bool
vand = mkPrimUnary $ PrimVLAnd bitType

-- | Return 'True' if any lane of the vector is 'True'
--
-- @since 1.4.0.0
--
vor :: KnownNat n => Exp (Vec n Bool) -> Exp Bool
vor = mkPrimUnary $ PrimVLOr bitType


infix 4 ==*
infix 4 /=*

-- | The 'VEq' class defines lane-wise equality '(==*)' and inequality
-- '(/=*)' for Accelerate vector expressions.
--
class SIMD n a => VEq n a where
  (==*) :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  (/=*) :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  {-# MINIMAL (==*) | (/=*) #-}
  x ==* y = vnot (x /=* y)
  x /=* y = vnot (x ==* y)

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
        [d| instance KnownNat n => VEq n $(conT name) where
              (==*) = mkPrimBinary $ PrimEq  scalarType
              (/=*) = mkPrimBinary $ PrimNEq scalarType
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
            ctx = (++) <$> mapM (appT [t| Eq |]) ts
                       <*> mapM (appT [t| SIMD $(varT w) |]) ts

            cmp f = [| pack (zipWith $f (unpack $(varE x)) (unpack $(varE y))) |]
        --
        instanceD ctx [t| VEq $(varT w) $res |]
          [ funD (mkName "==*") [ clause [varP x, varP y] (normalB (cmp [| (==) |])) [] ]
          , funD (mkName "/=*") [ clause [varP x, varP y] (normalB (cmp [| (/=) |])) [] ]
          ]
  --
  ps <- concat <$> mapM mkPrim (numTypes ++ nonNumTypes)
  ts <- mapM mkTup [2..16]
  return (ps ++ ts)

vtrue, vfalse :: KnownNat n => Exp (Vec n Bool)
vtrue  = constant (Vec (Prim.unMask Prim.ones))
vfalse = constant (Vec (Prim.unMask Prim.zeros))

instance KnownNat n => VEq n () where
  _ ==* _ = vtrue
  _ /=* _ = vfalse

instance KnownNat n => VEq n Z where
  _ ==* _ = vtrue
  _ /=* _ = vfalse

instance KnownNat n => VEq n Bool where
  (==*) = mkPrimBinary $ PrimEq  scalarType
  (/=*) = mkPrimBinary $ PrimNEq scalarType

{--
  (==*) =
    let n = natVal' (proxy# :: Proxy# n)
        --
        cmp :: forall t. (Elt t, IsIntegral (EltR t))
            => Exp (Vec n Bool)
            -> Exp (Vec n Bool)
            -> Exp (Vec n Bool)
        cmp x y =
          let x' = mkPrimUnary (PrimFromBool bitType integralType) x :: Exp t
              y' = mkPrimUnary (PrimFromBool bitType integralType) y
           in
           mkPrimUnary (PrimToBool integralType bitType) (mkBAnd x' y')
    in
    if n <= 8   then cmp @Word8   else
    if n <= 16  then cmp @Word16  else
    if n <= 32  then cmp @Word32  else
    if n <= 64  then cmp @Word64  else
    if n <= 128 then cmp @Word128 else
      internalError "Can not handle Vec types with more than 128 lanes"

  (/=*) =
    let n = natVal' (proxy# :: Proxy# n)
        --
        cmp :: forall t. (Elt t, IsIntegral (EltR t))
            => Exp (Vec n Bool)
            -> Exp (Vec n Bool)
            -> Exp (Vec n Bool)
        cmp x y =
          let x' = mkPrimUnary (PrimFromBool bitType integralType) x :: Exp t
              y' = mkPrimUnary (PrimFromBool bitType integralType) y
           in
           mkPrimUnary (PrimToBool integralType bitType) (mkBXor x' y')
    in
    if n <= 8   then cmp @Word8   else
    if n <= 16  then cmp @Word16  else
    if n <= 32  then cmp @Word32  else
    if n <= 64  then cmp @Word64  else
    if n <= 128 then cmp @Word128 else
      internalError "Can not handle SIMD vector types with more than 128 lanes"
--}

instance (Eq sh, SIMD n sh) => VEq n (sh :. Int) where
  x ==* y = pack (zipWith (==) (unpack x) (unpack y))
  x /=* y = pack (zipWith (/=) (unpack x) (unpack y))

instance KnownNat n => VEq n Ordering where
  x ==* y = mkCoerce x ==* (mkCoerce y :: Exp (Vec n TAG))
  x /=* y = mkCoerce x /=* (mkCoerce y :: Exp (Vec n TAG))

