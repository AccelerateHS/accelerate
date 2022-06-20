{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
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

data TagType t where
  TagBit    :: TagType Bit
  TagWord8  :: TagType Word8
  TagWord16 :: TagType Word16

-- | This structure both witnesses the layout of our representation types
-- (as TupR does) and represents a complete path of pattern matching
-- through this type. It indicates which fields of the structure represent
-- the union tags (TagRcon or TagRenum) or store undefined values (TagRundef).
--
-- The function 'tagsR' produces all valid paths through the type. For
-- example the type '(Bool,Bool)' produces the following:
--
--   ghci> putStrLn . unlines . map show $ tagsR @(Bool,Bool)
--   (((),0#),0#)      -- (False, False)
--   (((),0#),1#)      -- (False, True)
--   (((),1#),0#)      -- (True, False)
--   (((),1#),1#)      -- (True, True)
--
data TagR a where
  TagRunit   :: TagR ()
  TagRsingle :: ScalarType a -> TagR a
  TagRundef  :: ScalarType a -> TagR a
  TagRpair   :: TagR a -> TagR b -> TagR (a, b)
  TagRcon    :: TagType t -> t -> TagR a -> TagR (t, a)   -- data constructors
  TagRenum   :: TagType t -> t -> TagR t                  -- enumerations

instance Show (TagR a) where
  show TagRunit         = "()"
  show TagRsingle{}     = "."
  show TagRundef{}      = "undef"
  show (TagRpair a b)   = "(" ++ show a ++ "," ++ show b ++ ")"
  show (TagRcon tR t e) = "(" ++ showTag tR t ++ "#," ++ show e ++ ")"
  show (TagRenum tR t)  = showTag tR t ++ "#"

showTag :: TagType t -> t -> String
showTag TagBit    = show
showTag TagWord8  = show
showTag TagWord16 = show

rnfTag :: TagR a -> ()
rnfTag TagRunit          = ()
rnfTag (TagRsingle e)    = rnfScalarType e
rnfTag (TagRundef e)     = rnfScalarType e
rnfTag (TagRpair a b)    = rnfTag a `seq` rnfTag b
rnfTag (TagRenum tR t)   = rnfTagType tR `seq` t `seq` ()
rnfTag (TagRcon tR t e)  = rnfTagType tR `seq` t `seq` rnfTag e

rnfTagType :: TagType t -> ()
rnfTagType TagBit    = ()
rnfTagType TagWord8  = ()
rnfTagType TagWord16 = ()

liftTagR :: TagR a -> CodeQ (TagR a)
liftTagR TagRunit         = [|| TagRunit ||]
liftTagR (TagRsingle e)   = [|| TagRsingle $$(liftScalarType e) ||]
liftTagR (TagRundef e)    = [|| TagRundef $$(liftScalarType e) ||]
liftTagR (TagRpair a b)   = [|| TagRpair $$(liftTagR a) $$(liftTagR b) ||]
liftTagR (TagRcon tR t e) = [|| TagRcon $$(liftTagType tR) $$(liftTag tR t) $$(liftTagR e) ||]
liftTagR (TagRenum tR t)  = [|| TagRenum $$(liftTagType tR) $$(liftTag tR t) ||]

liftTag :: TagType t -> t -> CodeQ t
liftTag TagBit    (Bit x) = [|| Bit x ||]
liftTag TagWord8  x       = [|| x ||]
liftTag TagWord16 x       = [|| x ||]

liftTagType :: TagType t -> CodeQ (TagType t)
liftTagType TagBit    = [|| TagBit    ||]
liftTagType TagWord8  = [|| TagWord8  ||]
liftTagType TagWord16 = [|| TagWord16 ||]

