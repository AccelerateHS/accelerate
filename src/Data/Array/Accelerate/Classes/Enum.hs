{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Enum
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Enum (

  Enum,
  succ, pred,

) where

import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type

import Control.Monad
import Language.Haskell.TH                                          hiding ( Exp )
import Text.Printf
import Prelude                                                      hiding ( Num, Enum )
import qualified Prelude                                            as P


-- | Operations over sequentially ordered types
--
type Enum a = P.Enum (Exp a)

defaultSucc :: Num a => Exp a -> Exp a
defaultSucc x = x + 1

defaultPred :: Num a => Exp a -> Exp a
defaultPred x = x - 1

defaultToEnum :: Int -> a
defaultToEnum = preludeError "toEnum"

defaultFromEnum :: a -> Int
defaultFromEnum = preludeError "fromEnum"

preludeError :: String -> a
preludeError x
  = error
  $ unlines [ printf "Prelude.%s is not supported for Accelerate types" x , ""
            , "These Prelude.Enum instances are present only to fulfil superclass"
            , "constraints for subsequent classes in the standard Haskell numeric hierarchy."
            ]

runQ $
  let
      integralTypes :: [Name]
      integralTypes =
        [ ''Int
        , ''Int8
        , ''Int16
        , ''Int32
        , ''Int64
        , ''Int128
        , ''Word
        , ''Word8
        , ''Word16
        , ''Word32
        , ''Word64
        , ''Word128
        ]

      floatingTypes :: [Name]
      floatingTypes =
        [ ''Half
        , ''Float
        , ''Double
        , ''Float128
        ]

      numTypes :: [Name]
      numTypes = integralTypes ++ floatingTypes

      mkEnum :: Name -> Q [Dec]
      mkEnum a =
        [d| instance P.Enum (Exp $(conT a)) where
              succ      = defaultSucc
              pred      = defaultPred
              toEnum    = defaultToEnum
              fromEnum  = defaultFromEnum

            instance KnownNat n => P.Enum (Exp (Vec n $(conT a))) where
              succ      = defaultSucc
              pred      = defaultPred
              toEnum    = defaultToEnum
              fromEnum  = defaultFromEnum
          |]
  in
  concat <$> mapM mkEnum numTypes

