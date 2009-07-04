-- |Embedded array processing language: Typeable utilities
--
--  Copyright (c) [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--

module Data.Array.Accelerate.Typeable (
  cast1
) where

-- standard libraries
import Data.Maybe
import Data.Typeable

-- friends
import Data.Array.Accelerate.AST (Idx)


instance Typeable2 Idx where
  typeOf2 _ = mkTyCon "Data.Array.Accelerate.AST.Idx" `mkTyConApp` []

cast1 :: (Typeable1 t, Typeable1 t') => t a -> Maybe (t' a)
cast1 = fromJust . gcast1 . Just

