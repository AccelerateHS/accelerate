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

module Data.Array.Accelerate.Representation.Tag
  where

import Data.Array.Accelerate.Type

import Language.Haskell.TH.Extra


-- | The type of the runtime value used to distinguish constructor
-- alternatives in a sum type.
--
type TAG = Word8

-- | This structure both witnesses the layout of our representation types
-- (as TupR does) and represents a complete path of pattern matching
-- through this type. It indicates which fields of the structure represent
-- the union tags (TagRtag) or store undefined values (TagRundef).
--
-- The function 'tagsR' produces all valid paths through the type. For
-- example the type '(Bool,Bool)' produces the following:
--
--   ghci> putStr . unlines . map show $ tagsR @(Bool,Bool)
--   (((),(0#,())),(0#,()))     -- (False, False)
--   (((),(0#,())),(1#,()))     -- (False, True)
--   (((),(1#,())),(0#,()))     -- (True, False)
--   (((),(1#,())),(1#,()))     -- (True, True)
--
data TagR a where
  TagRunit   :: TagR ()
  TagRsingle :: ScalarType a -> TagR a
  TagRundef  :: ScalarType a -> TagR a
  TagRtag    :: TAG -> TagR a -> TagR (TAG, a)
  TagRpair   :: TagR a -> TagR b -> TagR (a, b)

instance Show (TagR a) where
  show TagRunit         = "()"
  show TagRsingle{}     = "."
  show TagRundef{}      = "undef"
  show (TagRtag v t)    = "(" ++ show v ++ "#," ++ show t ++ ")"
  show (TagRpair ta tb) = "(" ++ show ta ++ "," ++ show tb ++ ")"

rnfTag :: TagR a -> ()
rnfTag TagRunit         = ()
rnfTag (TagRsingle t)   = rnfScalarType t
rnfTag (TagRundef t)    = rnfScalarType t
rnfTag (TagRtag v t)    = v `seq` rnfTag t
rnfTag (TagRpair ta tb) = rnfTag ta `seq` rnfTag tb

liftTag :: TagR a -> CodeQ (TagR a)
liftTag TagRunit         = [|| TagRunit ||]
liftTag (TagRsingle t)   = [|| TagRsingle $$(liftScalarType t) ||]
liftTag (TagRundef t)    = [|| TagRundef $$(liftScalarType t) ||]
liftTag (TagRtag v t)    = [|| TagRtag v $$(liftTag t) ||]
liftTag (TagRpair ta tb) = [|| TagRpair $$(liftTag ta) $$(liftTag tb) ||]

