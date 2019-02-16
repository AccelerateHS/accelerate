{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Enum
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Enum (

  Enum,
  succ, pred,

) where

import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type
import Text.Printf

import Prelude                                                      ( ($), String, error, unlines, succ, pred )
import qualified Prelude                                            as P


-- | Operations over sequentially ordered types
--
type Enum a = P.Enum (Exp a)


instance P.Enum (Exp Int) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp Int8) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp Int16) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp Int32) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp Int64) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp Word) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp Word8) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp Word16) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp Word32) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp Word64) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp CInt) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp CUInt) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp CLong) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp CULong) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp CLLong) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp CULLong) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp CShort) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp CUShort) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp Half) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp Float) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp Double) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp CFloat) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

instance P.Enum (Exp CDouble) where
  succ      = defaultSucc
  pred      = defaultPred
  toEnum    = defaultToEnum
  fromEnum  = defaultFromEnum

defaultSucc :: Num a => Exp a -> Exp a
defaultSucc x = x + 1

defaultPred :: Num a => Exp a -> Exp a
defaultPred x = x - 1

defaultToEnum :: Int -> a
defaultToEnum = preludeError "toEnum"

defaultFromEnum :: a -> Int
defaultFromEnum = preludeError "fromEnum"

preludeError :: String -> a
preludeError x
  = error
  $ unlines [ printf "Prelude.%s is not supported for Accelerate types" x
            , ""
            , "These Prelude.Enum instances are present only to fulfil superclass"
            , "constraints for subsequent classes in the standard Haskell numeric hierarchy."
            ]

