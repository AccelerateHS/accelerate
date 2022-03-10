{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_HADDOCK hide #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
-- This is needed to derive POSable for tuples of size more then 4
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}
-- |
-- Module      : Data.Array.Accelerate.Representation.POS
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Sugar.POS
  where

-- import Data.Array.Accelerate.Type

import Data.Bits
import Data.Char
import Data.Kind
import Language.Haskell.TH.Extra                                    hiding ( Type )

import GHC.Generics
import GHC.TypeLits

import Data.Type.POSable.POSable as POSable
import Data.Type.POSable.Representation
import Data.Type.POSable.Instances
import Data.Type.POSable.TH

import Data.Int
import Data.Word
import Numeric.Half
import Foreign.C.Types

import Data.Array.Accelerate.Type

runQ $ do
    let
        -- XXX: we might want to do the digItOut trick used by FromIntegral?
        --
        integralTypes :: [Name]
        integralTypes =
          [ ''Int
          , ''Int8
          , ''Int16
          , ''Int32
          , ''Int64
          , ''Word
          , ''Word8
          , ''Word16
          , ''Word32
          , ''Word64
          ]
  
        floatingTypes :: [Name]
        floatingTypes =
          [ ''Half
          , ''Float
          , ''Double
          ]
  
        newtypes :: [Name]
        newtypes =
          [ ''CShort
          , ''CUShort
          , ''CInt
          , ''CUInt
          , ''CLong
          , ''CULong
          , ''CLLong
          , ''CULLong
          , ''CFloat
          , ''CDouble
          , ''CChar
          , ''CSChar
          , ''CUChar
          ]
  
        mkSimple :: Name -> Name -> Q [Dec]
        mkSimple typ name =
          let t = conT name
              tt = conT typ
              tr = pure $ ConE $ mkName ("Type" ++ nameBase name)
          in
          [d|
              instance GroundType $t where
                type TypeRep $t = $tt $t
  
                mkTypeRep = $tr
            |]
  
        mkTuple :: Int -> Q Dec
        mkTuple n =
          let
              xs  = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              ts  = map varT xs
              res = tupT ts
              ctx = mapM (appT [t| POSable |]) ts
          in
          instanceD ctx [t| POSable $res |] []
  
        mkNewtype :: Name -> Q [Dec]
        mkNewtype name = do
          r    <- reify name
          base <- case r of
                    TyConI (NewtypeD _ _ _ _ (NormalC _ [(_, ConT b)]) _) -> return b
                    _                                                     -> error "unexpected case generating newtype Elt instance"
          --
          mkPOSableGroundType name
    --
    si <- mapM (mkSimple ''IntegralType) integralTypes
    sf <- mapM (mkSimple ''FloatingType) floatingTypes
    ns <- mapM mkPOSableGroundType (floatingTypes ++ integralTypes)
    -- ns <- mapM mkNewtype newtypes
    -- ts <- mapM mkTuple [2..16]
    -- vs <- sequence [ mkVecElt t n | t <- integralTypes ++ floatingTypes, n <- [2,3,4,8,16] ]
    return (concat si ++ concat sf ++ concat ns)
  