{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-orphans         #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Enum
-- Copyright   : [2016] Manuel M T Chakravarty, Gabriele Keller
--               [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Enum (

  Enum,

) where

import Data.Array.Accelerate.Smart
import Text.Printf

import Prelude                                                      ( String, error )
import qualified Prelude                                            as P


-- | Operations over sequentially ordered types
--
type Enum a = P.Enum (Exp a)


-- Instances of 'Enum' don't make sense in Accelerate at the moment. These are
-- only provided to fulfil superclass constraints; e.g. Integral.
--
instance P.Enum (Exp a) where
  toEnum   = preludeError "toEnum"
  fromEnum = preludeError "fromEnum"

preludeError :: String -> a
preludeError x = error (printf "Prelude.%s not supported Accelerate types" x)

-- instance Enum (Exp Int8)
-- instance Enum (Exp Int16)
-- instance Enum (Exp Int32)
-- instance Enum (Exp Int64)
-- instance Enum (Exp Word)
-- instance Enum (Exp Word8)
-- instance Enum (Exp Word16)
-- instance Enum (Exp Word32)
-- instance Enum (Exp Word64)
-- instance Enum (Exp CInt)
-- instance Enum (Exp CUInt)
-- instance Enum (Exp CLong)
-- instance Enum (Exp CULong)
-- instance Enum (Exp CLLong)
-- instance Enum (Exp CULLong)
-- instance Enum (Exp CShort)
-- instance Enum (Exp CUShort)
-- instance Enum (Exp Bool)
-- instance Enum (Exp Char)
-- instance Enum (Exp CChar)
-- instance Enum (Exp CUChar)
-- instance Enum (Exp CSChar)
-- instance Enum (Exp Float)
-- instance Enum (Exp Double)
-- instance Enum (Exp CFloat)
-- instance Enum (Exp CDouble)

