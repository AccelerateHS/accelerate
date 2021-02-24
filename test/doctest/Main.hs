{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RebindableSyntax #-}
-- |
-- Module      : Main
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Main where

-- import Build_doctests                           ( flags, pkgs, module_sources )
-- import Data.Foldable                            ( traverse_ )
-- import Test.DocTest

-- main :: IO ()
-- main = do
--   traverse_ putStrLn args
--   doctest args
--   where
--     args = flags ++ pkgs ++ module_sources

import Data.Array.Accelerate.Control.Monad.Trans
import Prelude ( IO, print )

main :: IO ()
main = print bar

-- App a ~ Exp Int -> Exp (Maybe (Either Bool (a, Int)))
type App = StateT Int (ExceptT Bool (MaybeT Exp))

bar :: Exp Int -> Exp (Maybe (Either Bool (Int, Int)))
bar = runMaybeT . runExceptT . runStateT foo

foo :: App Int
foo = do
  modify (+1)
  _ <- get
  i <- lift $ throwE False_ `catchE` match \case
                                    False_ -> except $ Right_ 5
                                    True_  -> except $ Left_ False_
  return i