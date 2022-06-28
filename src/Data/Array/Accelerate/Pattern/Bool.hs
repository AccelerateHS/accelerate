{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern.Bool
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern.Bool (

  Bool, pattern True, pattern False,

) where

import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Bool                                                    ( Bool )
import Prelude                                                      hiding ( Bool(..) )
import qualified Prelude                                            as P

import GHC.Stack


{-# COMPLETE False, True #-}
pattern False :: (HasCallStack, IsFalse r) => r
pattern False <- (matchFalse -> Just ())
  where False = buildFalse

pattern True :: (HasCallStack, IsTrue r) => r
pattern True <- (matchTrue -> Just ())
  where True = buildTrue

class IsFalse r where
  buildFalse :: r
  matchFalse :: r -> Maybe ()

instance IsFalse Bool where
  buildFalse         = P.False
  matchFalse P.False = Just ()
  matchFalse _       = Nothing

instance IsFalse (Exp Bool) where
  buildFalse = _buildFalse
  matchFalse = _matchFalse

class IsTrue r where
  buildTrue :: r
  matchTrue :: r -> Maybe ()

instance IsTrue Bool where
  buildTrue        = P.True
  matchTrue P.True = Just ()
  matchTrue _      = Nothing

instance IsTrue (Exp Bool) where
  buildTrue = _buildTrue
  matchTrue = _matchTrue



_buildFalse :: Exp Bool
_buildFalse = mkExp $ Const scalarType 0

_matchFalse :: HasCallStack => Exp Bool -> Maybe ()
_matchFalse (Exp e) =
  case e of
    SmartExp (Match (TagRenum TagBit 0) _) -> Just ()
    SmartExp Match{}                       -> Nothing
    _ -> error $ unlines
           [ "Embedded pattern synonym used outside 'match' context."
           , ""
           , "To use case statements in the embedded language the case statement must"
           , "be applied as an n-ary function to the 'match' operator. For single"
           , "argument case statements this can be done inline using LambdaCase, for"
           , "example:"
           , ""
           , "> x & match \\case"
           , ">   False_ -> ..."
           , ">   _      -> ..."
           ]

_buildTrue :: Exp Bool
_buildTrue = mkExp $ Const scalarType 1

_matchTrue :: HasCallStack => Exp Bool -> Maybe ()
_matchTrue (Exp e) =
  case e of
    SmartExp (Match (TagRenum TagBit 1) _) -> Just ()
    SmartExp Match{}                       -> Nothing
    _ -> error $ unlines
           [ "Embedded pattern synonym used outside 'match' context."
           , ""
           , "To use case statements in the embedded language the case statement must"
           , "be applied as an n-ary function to the 'match' operator. For single"
           , "argument case statements this can be done inline using LambdaCase, for"
           , "example:"
           , ""
           , "> x & match \\case"
           , ">   True_ -> ..."
           , ">   _     -> ..."
           ]

