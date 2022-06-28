{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern.Ordering
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern.Ordering (

  Ordering, pattern LT, pattern EQ, pattern GT,

) where

import Data.Array.Accelerate.Pattern.TH
import Data.Array.Accelerate.Smart

import Data.Ord                                                     ( Ordering )
import Language.Haskell.TH.Extra                                    hiding ( Exp )
import Prelude                                                      hiding ( Ordering(..) )
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

  decs <- filter it <$> mkPattern ''Ordering
  rest <- [d| {-# COMPLETE LT, EQ, GT :: Exp      #-}
              {-# COMPLETE LT, EQ, GT :: Ordering #-}
              pattern LT :: IsLT a => a
              pattern LT <- (matchLT -> Just ())
                where LT = buildLT

              class IsLT a where
                buildLT :: a
                matchLT :: a -> Maybe ()

              instance IsLT Ordering where
                buildLT      = P.LT
                matchLT P.LT = Just ()
                matchLT _    = Nothing

              instance IsLT (Exp Ordering) where
                buildLT = $(find "_buildLT" decs)
                matchLT = $(find "_matchLT" decs)

              pattern EQ :: IsEQ a => a
              pattern EQ <- (matchEQ -> Just ())
                where EQ = buildEQ

              class IsEQ a where
                buildEQ :: a
                matchEQ :: a -> Maybe ()

              instance IsEQ Ordering where
                buildEQ      = P.EQ
                matchEQ P.EQ = Just ()
                matchEQ _    = Nothing

              instance IsEQ (Exp Ordering) where
                buildEQ = $(find "_buildEQ" decs)
                matchEQ = $(find "_matchEQ" decs)

              pattern GT :: IsGT a => a
              pattern GT <- (matchGT -> Just ())
                where GT = buildGT

              class IsGT a where
                buildGT :: a
                matchGT :: a -> Maybe ()

              instance IsGT Ordering where
                buildGT      = P.GT
                matchGT P.GT = Just ()
                matchGT _    = Nothing

              instance IsGT (Exp Ordering) where
                buildGT = $(find "_buildGT" decs)
                matchGT = $(find "_matchGT" decs)

            |]
  return (decs ++ rest)

