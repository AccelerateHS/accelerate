-- |Embedded array processing language: debugging support (internal)
--
--  Copyright (c) 2009 Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--
--  This module provides functionality that is useful for developers of the
--  library.  It is not meant for library users.

module Data.Array.Accelerate.Debug (

  dumpAcc, dumpExp

) where

-- friends
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Pretty ()

dumpAcc :: Acc as -> String
dumpAcc = show . convertAcc

dumpExp :: Exp a -> String
dumpExp = show . convertClosedExp
