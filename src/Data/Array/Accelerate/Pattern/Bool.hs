{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
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

  Bool, pattern True_, pattern False_,

) where

import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import GHC.Stack


{-# COMPLETE False_, True_ #-}
pattern False_ :: HasCallStack => Exp Bool
pattern False_ <- (matchFalse -> Just ())
  where False_ = buildFalse

pattern True_ :: HasCallStack => Exp Bool
pattern True_ <- (matchTrue -> Just ())
  where True_ = buildTrue


buildFalse :: Exp Bool
buildFalse = mkExp $ Const scalarType 0

matchFalse :: HasCallStack => Exp Bool -> Maybe ()
matchFalse (Exp e) =
  case e of
    SmartExp (Match (TagRbit TypeBit 0) _) -> Just ()
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

buildTrue :: Exp Bool
buildTrue = mkExp $ Const scalarType 1

matchTrue :: HasCallStack => Exp Bool -> Maybe ()
matchTrue (Exp e) =
  case e of
    SmartExp (Match (TagRbit TypeBit 1) _) -> Just ()
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

