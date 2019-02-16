{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-orphans         #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Real
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Real (

  Real,

) where

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord

import Prelude                                                      hiding ( Real, Num, Ord )
import qualified Prelude                                            as P


type Real a = (Num a, Ord a, P.Real (Exp a))

-- Instances of 'Real' don't make sense in Accelerate at the moment. These are
-- only provided to fulfil superclass constraints; e.g. Integral.
--
-- We won't need `toRational' until we support rational numbers in AP
-- computations.
--
instance (Num a, Ord a) => P.Real (Exp a) where
  toRational
    = error
    $ unlines [ "Prelude.toRational is not supported for Accelerate types"
              , ""
              , "These Prelude.Real instances are present only to fulfil superclass"
              , "constraints for subsequent classes in the standard Haskell numeric hierarchy."
              ]

