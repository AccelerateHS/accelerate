{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
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

import Data.Kind
import GHC.TypeLits
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Smart
import Data.Primitive.Vec

instance (VecElt a, KnownNat n) => Vectoring (Exp (Vec n a)) (Exp a) where
    type IndexType (Exp (Vec n a)) = Exp Int
    vecIndex = mkVectorIndex
    vecEmpty = mkVectorCreate
    

