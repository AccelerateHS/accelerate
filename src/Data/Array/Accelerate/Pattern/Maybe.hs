{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE ViewPatterns           #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern.Maybe
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern.Maybe (

  Maybe, pattern Nothing, pattern Just,

) where

import Data.Array.Accelerate.Pattern.TH
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Smart

import Data.Maybe                                                   ( Maybe )
import Language.Haskell.TH.Extra                                    hiding ( Exp )
import Prelude                                                      hiding ( Maybe(..) )
import qualified Data.List                                          as P
import qualified Prelude                                            as P

import GHC.Stack


-- TODO: We should make this a feature of the mkPattern machinery
--
runQ $ do
  let it SigD{} = True
      it FunD{} = True
      it _      = False

      find _   []     = error "could not find specified function"
      find pat (d:ds) =
        case d of
          SigD n _ | pat `P.isPrefixOf` nameBase n -> varE n
          _                                        -> find pat ds

  decs <- filter it <$> mkPattern ''Maybe
  rest <- [d| {-# COMPLETE Nothing, Just :: Exp   #-}
              {-# COMPLETE Nothing, Just :: Maybe #-}
              pattern Nothing :: (HasCallStack, IsNothing r) => r
              pattern Nothing <- (matchNothing -> P.Just ())
                where Nothing = buildNothing

              pattern Just :: (HasCallStack, IsJust a r) => a -> r
              pattern Just x <- (matchJust -> P.Just x)
                where Just = buildJust

              class IsNothing r where
                buildNothing :: r
                matchNothing :: r -> Maybe ()

              instance IsNothing (Maybe a) where
                buildNothing = P.Nothing
                matchNothing P.Nothing = P.Just ()
                matchNothing _         = P.Nothing

              instance Elt a => IsNothing (Exp (Maybe a)) where
                buildNothing = $(find "_buildNothing" decs)
                matchNothing = $(find "_matchNothing" decs)

              class IsJust a r | r -> a where
                buildJust :: a -> r
                matchJust :: r -> Maybe a

              instance IsJust a (Maybe a) where
                buildJust = P.Just
                matchJust = P.id

              instance Elt a => IsJust (Exp a) (Exp (Maybe a)) where
                buildJust = $(find "_buildJust" decs)
                matchJust = $(find "_matchJust" decs)
            |]
  return (decs ++ rest)

