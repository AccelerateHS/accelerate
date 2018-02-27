{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Unsafe
-- Copyright   : [2009..2018] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Operations which may be unsafe. Use with care.
--
-- @since 1.2.0.0
--

module Data.Array.Accelerate.Unsafe (

  -- ** Unsafe operations
  undef, coerce,

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type


-- | The function 'coerce' allows you to convert a value between any two types
-- whose underlying representations have the same bit size.
--
-- This is equivalent to 'Data.Array.Accelerate.Language.bitcast', but does not
-- include the type-level equality check of this requirement.
--
-- @since 1.2.0.0
--
coerce :: (Elt a, Elt b, IsScalar (EltRepr a), IsScalar (EltRepr b)) => Exp a -> Exp b
coerce = mkUnsafeCoerce

