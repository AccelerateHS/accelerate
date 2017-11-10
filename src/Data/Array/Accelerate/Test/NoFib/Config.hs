{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Config
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Config
  where

import Data.Bits
import Data.Proxy
import Data.Typeable

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Options


nofibIngredient :: Ingredient
nofibIngredient =
  includingOptions
    [ Option (Proxy::Proxy TestDouble)
    , Option (Proxy::Proxy TestFloat)
    , Option (Proxy::Proxy TestInt8)
    , Option (Proxy::Proxy TestInt16)
    , Option (Proxy::Proxy TestInt32)
    , Option (Proxy::Proxy TestInt64)
    , Option (Proxy::Proxy TestWord8)
    , Option (Proxy::Proxy TestWord16)
    , Option (Proxy::Proxy TestWord32)
    , Option (Proxy::Proxy TestWord64)
    ]

newtype TestAll    = TestAll    Bool deriving (Eq, Show, Typeable)
newtype TestDouble = TestDouble Bool deriving (Eq, Show, Typeable)
newtype TestFloat  = TestFloat  Bool deriving (Eq, Show, Typeable)
newtype TestInt8   = TestInt8   Bool deriving (Eq, Show, Typeable)
newtype TestInt16  = TestInt16  Bool deriving (Eq, Show, Typeable)
newtype TestInt32  = TestInt32  Bool deriving (Eq, Show, Typeable)
newtype TestInt64  = TestInt64  Bool deriving (Eq, Show, Typeable)
newtype TestWord8  = TestWord8  Bool deriving (Eq, Show, Typeable)
newtype TestWord16 = TestWord16 Bool deriving (Eq, Show, Typeable)
newtype TestWord32 = TestWord32 Bool deriving (Eq, Show, Typeable)
newtype TestWord64 = TestWord64 Bool deriving (Eq, Show, Typeable)

instance IsOption TestAll where
  defaultValue = TestAll False
  parseValue = fmap TestAll . safeRead
  optionName = return "all-types"
  optionHelp = return "Enable tests on all primitive types"
  optionCLParser = flagCLParser Nothing (TestAll True)

instance IsOption TestDouble where
  defaultValue = TestDouble True
  parseValue = fmap TestDouble . safeRead
  optionName = return "double"
  optionHelp = return "Enable double-precision tests"
  optionCLParser = flagCLParser Nothing (TestDouble True)

instance IsOption TestFloat where
  defaultValue = TestFloat False
  parseValue = fmap TestFloat . safeRead
  optionName = return "float"
  optionHelp = return "Enable single-precision tests"
  optionCLParser = flagCLParser Nothing (TestFloat True)

instance IsOption TestInt8 where
  defaultValue = TestInt8 False
  parseValue = fmap TestInt8 . safeRead
  optionName = return "int8"
  optionHelp = return "Enable 8-bit signed integer tests"
  optionCLParser = flagCLParser Nothing (TestInt8 True)

instance IsOption TestInt16 where
  defaultValue = TestInt16 False
  parseValue = fmap TestInt16 . safeRead
  optionName = return "int16"
  optionHelp = return "Enable 16-bit signed integer tests"
  optionCLParser = flagCLParser Nothing (TestInt16 True)

instance IsOption TestInt32 where
  defaultValue = TestInt32 $( [e| finiteBitSize (undefined::Int) == 32 |] )
  parseValue = fmap TestInt32 . safeRead
  optionName = return "int32"
  optionHelp = return "Enable 32-bit signed integer tests"
  optionCLParser = flagCLParser Nothing (TestInt32 True)

instance IsOption TestInt64 where
  defaultValue = TestInt64 $( [e| finiteBitSize (undefined::Int) == 64 |] )
  parseValue = fmap TestInt64 . safeRead
  optionName = return "int64"
  optionHelp = return "Enable 64-bit signed integer tests"
  optionCLParser = flagCLParser Nothing (TestInt64 True)

instance IsOption TestWord8 where
  defaultValue = TestWord8 False
  parseValue = fmap TestWord8 . safeRead
  optionName = return "word8"
  optionHelp = return "Enable 8-bit unsigned integer tests"
  optionCLParser = flagCLParser Nothing (TestWord8 True)

instance IsOption TestWord16 where
  defaultValue = TestWord16 False
  parseValue = fmap TestWord16 . safeRead
  optionName = return "word16"
  optionHelp = return "Enable 16-bit unsigned integer tests"
  optionCLParser = flagCLParser Nothing (TestWord16 True)

instance IsOption TestWord32 where
  defaultValue = TestWord32 False
  parseValue = fmap TestWord32 . safeRead
  optionName = return "word32"
  optionHelp = return "Enable 32-bit unsigned integer tests"
  optionCLParser = flagCLParser Nothing (TestWord32 True)

instance IsOption TestWord64 where
  defaultValue = TestWord64 False
  parseValue = fmap TestWord64 . safeRead
  optionName = return "word64"
  optionHelp = return "Enable 64-bit unsigned integer tests"
  optionCLParser = flagCLParser Nothing (TestWord64 True)


class IsOption a => TestConfig a where
  at :: Proxy a -> TestTree -> TestTree

instance TestConfig TestFloat where
  at _ t = askOption $ \(TestFloat v)  -> if v then t else testGroup "Float" []

instance TestConfig TestDouble where
  at _ t = askOption $ \(TestDouble v) -> if v then t else testGroup "Double" []

instance TestConfig TestInt8 where
  at _ t = askOption $ \(TestInt8 v)   -> if v then t else testGroup "Int8" []

instance TestConfig TestInt16 where
  at _ t = askOption $ \(TestInt16 v)  -> if v then t else testGroup "Int16" []

instance TestConfig TestInt32 where
  at _ t = askOption $ \(TestInt32 v)  -> if v then t else testGroup "Int32" []

instance TestConfig TestInt64 where
  at _ t = askOption $ \(TestInt64 v)  -> if v then t else testGroup "Int64" []

instance TestConfig TestWord8 where
  at _ t = askOption $ \(TestWord8 v)  -> if v then t else testGroup "Word8" []

instance TestConfig TestWord16 where
  at _ t = askOption $ \(TestWord16 v) -> if v then t else testGroup "Word16" []

instance TestConfig TestWord32 where
  at _ t = askOption $ \(TestWord32 v) -> if v then t else testGroup "Word32" []

instance TestConfig TestWord64 where
  at _ t = askOption $ \(TestWord64 v) -> if v then t else testGroup "Word64" []

