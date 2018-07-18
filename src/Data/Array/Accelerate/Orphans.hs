{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}

module Data.Array.Accelerate.Orphans (
) where

import GHC.Generics

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



