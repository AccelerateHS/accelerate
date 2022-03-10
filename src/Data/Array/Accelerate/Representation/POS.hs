{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
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

module Data.Array.Accelerate.Representation.POS (
  POSable(..), POS, POST, mkPOS, mkPOST, fromPOS, Product(..), Sum(..),
  GroundType(..), Finite, ProductType(..), SumType(..), POSable.Generic, type (++),
  mkPOSableGroundType)
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

-- import Data.Array.Accelerate.Representation.Type

type POS a = (Finite (Choices a), Product (Fields a))

-- type family EltR (cs :: Nat) (fs :: f (g a)) = (r :: Type) where
--   EltR 1 x = FlattenProduct x
--   EltR n x = (Finite n, FlattenProduct x)

-- type family FlattenProduct (xss :: f (g a)) :: Type where
--   FlattenProduct '[] = ()
--   FlattenProduct '[ '[x]] = x
--   FlattenProduct (x ': xs) = (FlattenSum x, FlattenProduct xs)

-- type family FlattenSum (xss :: f a) :: Type where
--   FlattenSum '[] = ()
--   FlattenSum (x ': xs) = (x, FlattenSum xs)

-- mkEltR :: (POSable a) => a -> EltR (Choices a) (Fields a)
-- mkEltR x = undefined
--   where
--     cs = choices x
--     fs = fields x

-- -- productToTupR :: Product a -> TypeR (FlattenProduct a)
-- -- productToTupR Nil = TupRunit
-- -- productToTupR (Cons x xs) = TupRpair x (productToTupR xs)

mkPOS :: (POSable a) => a -> POS a
mkPOS x = (choices x, fields x)

fromPOS :: (POSable a) => POS a -> a
fromPOS (cs, fs) = fromPOSable cs fs

type POST a = (Finite (Choices a), ProductType (Fields a))

mkPOST :: forall a . (POSable a) => POST a
mkPOST = (0, emptyFields @a)


type family Snoc2List x = xs | xs -> x where
  Snoc2List () = '[]
  Snoc2List (xs, x) = (x ': Snoc2List xs)
