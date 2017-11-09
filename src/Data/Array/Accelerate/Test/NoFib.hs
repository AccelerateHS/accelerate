{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib (

  nofib,
  nofibIngredient,

  module Data.Array.Accelerate.Test.NoFib.Prelude,

) where

import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.NoFib.Prelude

import Test.Tasty
import System.Environment


nofib :: RunN -> IO ()
nofib runN = do
  me <- getProgName
  defaultMainWithIngredients (nofibIngredient : defaultIngredients) $
    testGroup me
      [ test_prelude runN
      ]

