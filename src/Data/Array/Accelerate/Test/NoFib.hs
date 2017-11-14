{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK hide #-}
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

  module Data.Array.Accelerate.Test.NoFib.Sharing,
  module Data.Array.Accelerate.Test.NoFib.Prelude,
  module Data.Array.Accelerate.Test.NoFib.Imaginary,
  module Data.Array.Accelerate.Test.NoFib.Spectral,

) where

import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.NoFib.Sharing
import Data.Array.Accelerate.Test.NoFib.Prelude
import Data.Array.Accelerate.Test.NoFib.Imaginary
import Data.Array.Accelerate.Test.NoFib.Spectral

import Test.Tasty
import System.Environment


nofib :: RunN -> IO ()
nofib runN = do
  me <- getProgName
  defaultMainWithIngredients (nofibIngredient : defaultIngredients) $
    testGroup me
      [ test_sharing
      , test_prelude runN
      , test_imaginary runN
      , test_spectral runN
      ]

