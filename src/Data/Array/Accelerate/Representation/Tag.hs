{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Representation.Tag
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Representation.Tag (TAG, TagR(..))
  where

import Data.Array.Accelerate.Type ( TAG )



-- | This structure both witnesses the layout of our representation types
-- (as TupR does) and represents a complete path of pattern matching
-- through this type. It indicates which fields of the structure represent
-- the union tags (TagRtag) or store undefined values (TagRundef).
--
-- The function 'eltTags' produces all valid paths through the type. For
-- example the type '(Bool,Bool)' produces the following:
--
--   ghci> putStrLn . unlines . map show $ eltTags @(Bool,Bool)
--   (((),(0#,())),(0#,()))     -- (False, False)
--   (((),(0#,())),(1#,()))     -- (False, True)
--   (((),(1#,())),(0#,()))     -- (True, False)
--   (((),(1#,())),(1#,()))     -- (True, True)
--
data TagR a = TagR TAG TAG
  deriving Show
