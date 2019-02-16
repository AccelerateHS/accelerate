{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib
-- Copyright   : [2009..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib (

  nofib,
  nofibIngredient,

#ifndef ACCELERATE_DISABLE_NOFIB
  module Data.Array.Accelerate.Test.NoFib.Sharing,
  module Data.Array.Accelerate.Test.NoFib.Prelude,
  module Data.Array.Accelerate.Test.NoFib.Imaginary,
  module Data.Array.Accelerate.Test.NoFib.Spectral,
  module Data.Array.Accelerate.Test.NoFib.Issues,
#endif

) where

import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
#ifndef ACCELERATE_DISABLE_NOFIB
import Data.Array.Accelerate.Test.NoFib.Sharing
import Data.Array.Accelerate.Test.NoFib.Prelude
import Data.Array.Accelerate.Test.NoFib.Imaginary
import Data.Array.Accelerate.Test.NoFib.Spectral
import Data.Array.Accelerate.Test.NoFib.Issues

import Test.Tasty
import System.Environment
#endif


nofib :: RunN -> IO ()
#ifdef ACCELERATE_DISABLE_NOFIB
nofib _    = error $ unlines [ "Data.Array.Accelerate: the nofib test-suite has been disabled."
                             , "Reinstall package 'accelerate' with '-fnofib' to enable it."
                             ]
#else
nofib runN = do
  me <- getProgName
  defaultMainWithIngredients (nofibIngredient : defaultIngredients) $
    testGroup me
      [ test_sharing
      , test_prelude runN
      , test_imaginary runN
      , test_spectral runN
      , test_issues runN
      ]
#endif

