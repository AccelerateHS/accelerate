{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RankNTypes      #-}
module Data.Array.Accelerate.Uncurrency (
    id,
) where

import Prelude                                            hiding ( id )
import GHC.Exts                                           ( TYPE )


-- | Use in place of e.g. '($)' in order to reduce the dependency on volatile
-- currencies.
id :: forall r a (b :: TYPE r). (a -> b) -> a -> b
f `id` x =  f x
{-# INLINE id #-}

infixr 0 `id`
