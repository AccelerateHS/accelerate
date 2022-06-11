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
import Data.Primitive.Bit

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
-- The function 'eltTags' produces all valid paths through the type. For
-- example the type '(Bool,Bool)' produces the following:
--
--   ghci> putStrLn . unlines . map show $ tagsR @(Bool,Bool)
--   (((),(0#,())),(0#,()))     -- (False, False)
--   (((),(0#,())),(1#,()))     -- (False, True)
--   (((),(1#,())),(0#,()))     -- (True, False)
--   (((),(1#,())),(1#,()))     -- (True, True)
--
data TagR a where
  TagRunit   :: TagR ()
  TagRsingle :: ScalarType a -> TagR a
  TagRundef  :: ScalarType a -> TagR a
  TagRpair   :: TagR a -> TagR b -> TagR (a, b)
  TagRtag    :: SingleIntegralType t -> t -> TagR a -> TagR (t, a)
  TagRbit    :: BitType t -> t -> TagR t

instance Show (TagR a) where
  show TagRunit         = "()"
  show TagRsingle{}     = "."
  show TagRundef{}      = "undef"
  show (TagRpair ta tb) = "(" ++ show ta ++ "," ++ show tb ++ ")"
  show (TagRtag tR t e) = "(" ++ integral tR t ++ "#," ++ show e ++ ")"
    where
      integral :: SingleIntegralType t -> t -> String
      integral TypeInt8    = show
      integral TypeInt16   = show
      integral TypeInt32   = show
      integral TypeInt64   = show
      integral TypeInt128  = show
      integral TypeWord8   = show
      integral TypeWord16  = show
      integral TypeWord32  = show
      integral TypeWord64  = show
      integral TypeWord128 = show
  show (TagRbit tR t)   = bit tR t
    where
      bit :: BitType t -> t -> String
      bit TypeBit    x = shows x "#"
      bit TypeMask{} x = shows (BitMask x) "#"

rnfTag :: TagR a -> ()
rnfTag TagRunit         = ()
rnfTag (TagRsingle e)   = rnfScalarType e
rnfTag (TagRundef e)    = rnfScalarType e
rnfTag (TagRpair ta tb) = rnfTag ta `seq` rnfTag tb
rnfTag (TagRtag tR t e) = rnfSingleIntegralType tR `seq` t `seq` rnfTag e
rnfTag (TagRbit tR t)   = rnfBitType tR `seq` t `seq` ()

liftTag :: TagR a -> CodeQ (TagR a)
liftTag TagRunit         = [|| TagRunit ||]
liftTag (TagRsingle e)   = [|| TagRsingle $$(liftScalarType e) ||]
liftTag (TagRundef e)    = [|| TagRundef $$(liftScalarType e) ||]
liftTag (TagRpair ta tb) = [|| TagRpair $$(liftTag ta) $$(liftTag tb) ||]
liftTag (TagRtag tR t e) = [|| TagRtag $$(liftSingleIntegralType tR) $$(liftSingleIntegral tR t) $$(liftTag e) ||]
liftTag (TagRbit tR t)   = [|| TagRbit $$(liftBitType tR) $$(liftBit tR t) ||]

