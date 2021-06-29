{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Text.Format.Extra
-- Copyright   : [2009..2021] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Text.Format.Extra
  where

import Data.List                                                    ( intersperse )
import Data.Text.Lazy.Builder
import Prelude                                                      hiding ( unlines )


-- | Insert the builder @b@ between the builders in @bs@ and concatenate
-- the result.
--
-- > intercalate "\n" ["Hello", "World", "!"]
-- "Hello\nWorld\n!"
--
intercalate :: Builder -> [Builder] -> Builder
intercalate b bs = foldr (<>) mempty (intersperse b bs)

-- | Join builders after appending a terminating newline to each
--
-- > unlines ["Hello, "World", "!"]
-- "Hello\nWorld\n!\n"
--
unlines :: [Builder] -> Builder
unlines []     = mempty
unlines (l:ls) = l <> "\n" <> unlines ls

