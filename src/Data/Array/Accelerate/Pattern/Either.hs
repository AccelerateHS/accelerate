{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE ViewPatterns           #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern.Either
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern.Either (

  Either, pattern Left, pattern Right,

) where

import Data.Array.Accelerate.Pattern.TH
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Smart

import Data.Either                                                  ( Either )
import Language.Haskell.TH.Extra                                    hiding ( Exp )
import Prelude                                                      hiding ( Either(..) )
import qualified Data.List                                          as P
import qualified Prelude                                            as P


runQ $ do
  let it SigD{} = True
      it FunD{} = True
      it _      = False

      find _   []     = error "could not find specified function"
      find pat (d:ds) =
        case d of
          SigD n _ | pat `P.isPrefixOf` nameBase n -> varE n
          _                                        -> find pat ds

  decs <- filter it <$> mkPattern ''Either
  rest <- [d| {-# COMPLETE Left, Right :: Exp    #-}
              {-# COMPLETE Left, Right :: Either #-}
              pattern Left :: IsLeft a b r => a -> r
              pattern Left x <- (matchLeft -> P.Just x)
                where Left = buildLeft

              class IsLeft a b r | r -> a b where
                buildLeft :: a -> r
                matchLeft :: r -> Maybe a

              instance IsLeft a b (Either a b) where
                buildLeft            = P.Left
                matchLeft (P.Left a) = P.Just a
                matchLeft _          = P.Nothing

              instance (Elt a, Elt b) => IsLeft (Exp a) (Exp b) (Exp (Either a b)) where
                buildLeft = $(find "_buildLeft" decs)
                matchLeft = $(find "_matchLeft" decs)

              pattern Right :: IsRight a b r => b -> r
              pattern Right x <- (matchRight -> P.Just x)
                where Right = buildRight

              class IsRight a b r | r -> a b where
                buildRight :: b -> r
                matchRight :: r -> Maybe b

              instance IsRight a b (Either a b) where
                buildRight             = P.Right
                matchRight (P.Right b) = P.Just b
                matchRight _           = P.Nothing

              instance (Elt a, Elt b) => IsRight (Exp a) (Exp b) (Exp (Either a b)) where
                buildRight = $(find "_buildRight" decs)
                matchRight = $(find "_matchRight" decs)
            |]
  return (decs ++ rest)

