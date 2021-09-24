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

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Type

import Prelude                                                      ( ($), (<$>), Num(..), Char, Bool, show, concat, map, mapM )
import Language.Haskell.TH.Extra                                    hiding ( Exp )
import qualified Prelude                                            as P


-- | Name the upper and lower limits of a type. Types which are not totally
-- ordered may still have upper and lower bounds.
--
type Bounded a = (Elt a, P.Bounded (Exp a))


instance P.Bounded (Exp ()) where
  minBound = sourceMapRuntime $ constant ()
  maxBound = sourceMapRuntime $ constant ()

instance P.Bounded (Exp Int) where
  minBound = sourceMapRuntime mkMinBound
  maxBound = sourceMapRuntime mkMaxBound

instance P.Bounded (Exp Int8) where
  minBound = sourceMapRuntime mkMinBound
  maxBound = sourceMapRuntime mkMaxBound

instance P.Bounded (Exp Int16) where
  minBound = sourceMapRuntime mkMinBound
  maxBound = sourceMapRuntime mkMaxBound

instance P.Bounded (Exp Int32) where
  minBound = sourceMapRuntime mkMinBound
  maxBound = sourceMapRuntime mkMaxBound

instance P.Bounded (Exp Int64) where
  minBound = sourceMapRuntime mkMinBound
  maxBound = sourceMapRuntime mkMaxBound

instance P.Bounded (Exp Word) where
  minBound = sourceMapRuntime mkMinBound
  maxBound = sourceMapRuntime mkMaxBound

instance P.Bounded (Exp Word8) where
  minBound = sourceMapRuntime mkMinBound
  maxBound = sourceMapRuntime mkMaxBound

instance P.Bounded (Exp Word16) where
  minBound = sourceMapRuntime mkMinBound
  maxBound = sourceMapRuntime mkMaxBound

instance P.Bounded (Exp Word32) where
  minBound = sourceMapRuntime mkMinBound
  maxBound = sourceMapRuntime mkMaxBound

instance P.Bounded (Exp Word64) where
  minBound = sourceMapRuntime mkMinBound
  maxBound = sourceMapRuntime mkMaxBound

instance P.Bounded (Exp CShort) where
  minBound = sourceMapRuntime $ mkBitcast (mkMinBound @Int16)
  maxBound = sourceMapRuntime $ mkBitcast (mkMaxBound @Int16)

instance P.Bounded (Exp CUShort) where
  minBound = sourceMapRuntime $ mkBitcast (mkMinBound @Word16)
  maxBound = sourceMapRuntime $ mkBitcast (mkMaxBound @Word16)

instance P.Bounded (Exp CInt) where
  minBound = sourceMapRuntime $ mkBitcast (mkMinBound @Int32)
  maxBound = sourceMapRuntime $ mkBitcast (mkMaxBound @Int32)

instance P.Bounded (Exp CUInt) where
  minBound = sourceMapRuntime $ mkBitcast (mkMinBound @Word32)
  maxBound = sourceMapRuntime $ mkBitcast (mkMaxBound @Word32)

instance P.Bounded (Exp CLong) where
  minBound = sourceMapRuntime $ mkBitcast (mkMinBound @HTYPE_CLONG)
  maxBound = sourceMapRuntime $ mkBitcast (mkMaxBound @HTYPE_CLONG)

instance P.Bounded (Exp CULong) where
  minBound = sourceMapRuntime $ mkBitcast (mkMinBound @HTYPE_CULONG)
  maxBound = sourceMapRuntime $ mkBitcast (mkMaxBound @HTYPE_CULONG)

instance P.Bounded (Exp CLLong) where
  minBound = sourceMapRuntime $ mkBitcast (mkMinBound @Int64)
  maxBound = sourceMapRuntime $ mkBitcast (mkMaxBound @Int64)

instance P.Bounded (Exp CULLong) where
  minBound = sourceMapRuntime $ mkBitcast (mkMinBound @Word64)
  maxBound = sourceMapRuntime $ mkBitcast (mkMaxBound @Word64)

instance P.Bounded (Exp Bool) where
  minBound = sourceMapRuntime $ constant P.minBound
  maxBound = sourceMapRuntime $ constant P.maxBound

instance P.Bounded (Exp Char) where
  minBound = sourceMapRuntime mkMinBound
  maxBound = sourceMapRuntime mkMaxBound

instance P.Bounded (Exp CChar) where
  minBound = sourceMapRuntime $ mkBitcast (mkMinBound @HTYPE_CCHAR)
  maxBound = sourceMapRuntime $ mkBitcast (mkMaxBound @HTYPE_CCHAR)

instance P.Bounded (Exp CSChar) where
  minBound = sourceMapRuntime $ mkBitcast (mkMinBound @Int8)
  maxBound = sourceMapRuntime $ mkBitcast (mkMaxBound @Int8)

instance P.Bounded (Exp CUChar) where
  minBound = sourceMapRuntime $ mkBitcast (mkMinBound @Word8)
  maxBound = sourceMapRuntime $ mkBitcast (mkMaxBound @Word8)

$(runQ $ do
    let
        mkInstance :: Int -> Q [Dec]
        mkInstance n =
          let
              xs      = [ mkName ('x':show i) | i <- [0 .. n-1] ]
              cst     = tupT (map (\x -> [t| Bounded $(varT x) |]) xs)
              res     = tupT (map varT xs)
              app x   = appsE (conE (mkName ('T':show n)) : P.replicate n x)
          in
          [d| instance $cst => P.Bounded (Exp $res) where
                minBound = sourceMapRuntime $ $(app [| P.minBound |])
                maxBound = sourceMapRuntime $ $(app [| P.maxBound |])
            |]
    --
    concat <$> mapM mkInstance [2..16]
 )

