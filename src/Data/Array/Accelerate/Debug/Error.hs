{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Error
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Functions for checking properties or invariants
-- of a program.
--
-- @since 1.4.0.0
--

module Data.Array.Accelerate.Debug.Error (

  -- * Throwing errors
  -- $errors
  --
  aerror, aerrorArray, aerrorExp

) where

import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Array                            as S
import Data.Array.Accelerate.Sugar.Elt
import qualified Data.Array.Accelerate.Representation.Array         as R
import qualified Data.Array.Accelerate.Representation.Shape         as R


-- $errors
--
-- The 'aerror', 'aerrorArray', and 'aerrorExp' functions abort the execution
-- of the array program and print errors to an output stream. They are intended
-- for stopping the program when the program is in some invalid state, which
-- was expected to be unreachable.
--
-- Besides printing a given error message, it can also print the contents of an
-- array (with 'aerrorArray') or print some scalar value ('aerrorExp').
--

-- | Stops execution of the array computation and outputs the error message to
-- the console.
--
aerror :: forall a. Arrays a => String -> Acc a
aerror message
  = Acc
  $ SmartAcc
  $ Aerror (S.arraysR @a)
           (Message (\_ -> "") (Just [|| \_ -> "" ||]) message)
           (SmartAcc Anil :: SmartAcc ())

-- | Outputs the trace message and the array(s) from the second argument to
-- the console, before the 'Acc' computation proceeds with the result of
-- the third argument.
--
aerrorArray :: forall a b. (Arrays a, Arrays b, Show a) => String -> Acc a -> Acc b
aerrorArray message (Acc inspect)
  = Acc
  $ SmartAcc
  $ Aerror (S.arraysR @b)
           (Message (show . toArr @a)
           (Just [|| show . toArr @a ||]) message) inspect

-- | Outputs the trace message and a scalar value to the console, before
-- the 'Acc' computation proceeds with the result of the third argument.
--
aerrorExp :: forall e a. (Elt e, Show e, Arrays a) => String -> Exp e -> Acc a
aerrorExp message value =
  let Acc inspect = unit value
   in Acc
    $ SmartAcc
    $ Aerror (S.arraysR @a)
             (Message (\a -> show (toElt @e (R.indexArray (R.ArrayR R.dim0 (eltR @e)) a ())))
             (Just [|| \a -> show (toElt @e (R.indexArray (R.ArrayR R.dim0 (eltR @e)) a ())) ||]) message) inspect

