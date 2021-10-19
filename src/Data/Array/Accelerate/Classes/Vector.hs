{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Vector
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Data.Array.Accelerate.Classes.Vector where

import GHC.TypeLits
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Smart
import Data.Primitive.Vec

class Vectoring a b c | a -> b where
    indexAt :: a -> c -> b

instance (VecElt a, KnownNat n) => Vectoring (Exp (Vec n a)) (Exp a) (Exp Int) where
    indexAt = mkVectorIndex
    

