{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Bounded
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Bounded (

  Bounded,
  P.minBound, P.maxBound,

) where

import Data.Array.Accelerate.Pattern.Tuple
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type
import qualified Data.Primitive.Vec                                 as Prim

import Language.Haskell.TH.Extra                                    hiding ( Exp )
import Prelude                                                      hiding ( Bounded )
import qualified Prelude                                            as P


-- | Name the upper and lower limits of a type. Types which are not totally
-- ordered may still have upper and lower bounds.
--
type Bounded a = (Elt a, P.Bounded (Exp a))


instance P.Bounded (Exp ()) where
  minBound = constant ()
  maxBound = constant ()

instance P.Bounded (Exp Bool) where
  minBound = constant P.minBound
  maxBound = constant P.maxBound

instance P.Bounded (Exp Char) where
  minBound = constant P.minBound
  maxBound = constant P.maxBound

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

      mkBounded :: Name -> Q [Dec]
      mkBounded a =
        [d| instance P.Bounded (Exp $(conT a)) where
              minBound = constant P.minBound
              maxBound = constant P.maxBound

            instance KnownNat n => P.Bounded (Exp (Vec n $(conT a))) where
              minBound = constant (Vec (Prim.splat minBound))
              maxBound = constant (Vec (Prim.splat maxBound))
          |]

      mkTuple :: Int -> Q [Dec]
      mkTuple n =
        let
            xs      = [ mkName ('x':show i) | i <- [0 .. n-1] ]
            cst     = tupT (map (\x -> [t| Bounded $(varT x) |]) xs)
            res     = tupT (map varT xs)
            app x   = appsE (conE (mkName ('T':show n)) : P.replicate n x)
        in
        [d| instance $cst => P.Bounded (Exp $res) where
              minBound = $(app [| P.minBound |])
              maxBound = $(app [| P.maxBound |])
          |]
  --
  as <- mapM mkBounded integralTypes
  ts <- mapM mkTuple [2..16]
  return $ concat (as ++ ts)

