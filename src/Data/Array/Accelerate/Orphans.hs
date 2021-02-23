{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Orphans
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Orphans ()
  where

import Data.Orphans ()    -- orphan instances for 8-tuples and beyond
import Data.Primitive.Types
import Data.Hashable
import Data.Ratio
import Foreign.C.Types
import GHC.Generics
import GHC.Real
import GHC.Stack
import Numeric.Half


-- base
--
deriving instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p)
  => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

deriving instance Generic (a, b, c, d, e, f, g, h)
deriving instance Generic (a, b, c, d, e, f, g, h, i)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

deriving instance Generic (Ratio a)

deriving instance Generic SrcLoc

-- primitive
--
deriving instance Prim Half

-- hashable
--
instance Hashable SrcLoc
