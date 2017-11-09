{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
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

import Data.Maybe
import Data.Bits
import Lens.Micro
import Lens.Micro.TH
import System.Console.GetOpt


type a :-> b = Getting b a b


-- Which test types should be enabled?
--
data Config
  = Config
  { _configDouble       :: Bool
  , _configFloat        :: Bool
  , _configInt64        :: Bool
  , _configInt32        :: Bool
  , _configInt16        :: Bool
  , _configInt8         :: Bool
  , _configWord64       :: Bool
  , _configWord32       :: Bool
  , _configWord16       :: Bool
  , _configWord8        :: Bool
  }

makeLenses ''Config

defaults :: Config
defaults = Config
  {
    _configDouble       = True
  , _configFloat        = False
  , _configInt64        = $( [e| finiteBitSize (undefined::Int) == 64 |] )
  , _configInt32        = $( [e| finiteBitSize (undefined::Int) == 32 |] )
  , _configInt16        = False
  , _configInt8         = False
  , _configWord64       = False
  , _configWord32       = False
  , _configWord16       = False
  , _configWord8        = False
  }

extensive :: Config
extensive = Config
  {
    _configDouble       = True
  , _configFloat        = True
  , _configInt64        = True
  , _configInt32        = True
  , _configInt16        = True
  , _configInt8         = True
  , _configWord64       = True
  , _configWord32       = True
  , _configWord16       = True
  , _configWord8        = True
  }


options :: [OptDescr (Config -> Config)]
options =
  [ Option  [] ["all"]
            (NoArg (const extensive))
            "enable tests on all primitive types"

  , Option  [] ["double"]
            (OptArg (set configDouble . read . fromMaybe "True") "BOOL")
            (describe configDouble "enable double-precision tests")

  , Option  [] ["float"]
            (OptArg (set configFloat . read . fromMaybe "True") "BOOL")
            (describe configFloat "enable single-precision tests")

  , Option  [] ["int64"]
            (OptArg (set configInt64 . read . fromMaybe "True") "BOOL")
            (describe configInt64 "enable 64-bit integer tests")

  , Option  [] ["int32"]
            (OptArg (set configInt32 . read . fromMaybe "True") "BOOL")
            (describe configInt32 "enable 32-bit integer tests")

  , Option  [] ["int16"]
            (OptArg (set configInt16 . read . fromMaybe "True") "BOOL")
            (describe configInt16 "enable 16-bit integer tests")

  , Option  [] ["int8"]
            (OptArg (set configInt8 . read . fromMaybe "True") "BOOL")
            (describe configInt8 "enable 8-bit integer tests")

  , Option  [] ["word64"]
            (OptArg (set configWord64 . read . fromMaybe "True") "BOOL")
            (describe configWord64 "enable 64-bit unsigned integer tests")

  , Option  [] ["word32"]
            (OptArg (set configWord32 . read . fromMaybe "True") "BOOL")
            (describe configWord32 "enable 32-bit unsigned integer tests")

  , Option  [] ["word16"]
            (OptArg (set configWord16 . read . fromMaybe "True") "BOOL")
            (describe configWord16 "enable 16-bit unsigned integer tests")

  , Option  [] ["word8"]
            (OptArg (set configWord8 . read . fromMaybe "True") "BOOL")
            (describe configWord8 "enable 8-bit unsigned integer tests")
  ]
  where
    describe f msg
      = msg ++ " (" ++ show (defaults ^. f) ++ ")"

