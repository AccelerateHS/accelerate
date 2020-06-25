{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Error
-- Copyright   : [2009..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Error (

  internalError,   boundsError,   unsafeError,
  internalCheck,   boundsCheck,   unsafeCheck,   indexCheck,
  internalWarning, boundsWarning, unsafeWarning,

) where

import Debug.Trace
import Text.Printf
import Prelude                                          hiding ( error )

import GHC.Stack

data Check = Bounds | Unsafe | Internal


-- | Issue an internal error message
--
internalError :: HasCallStack => String -> a
internalError = withFrozenCallStack $ error Internal

boundsError :: HasCallStack => String -> a
boundsError = withFrozenCallStack $ error Bounds

unsafeError :: HasCallStack => String -> a
unsafeError = withFrozenCallStack $ error Unsafe


-- | Throw an error if the condition evaluates to False, otherwise evaluate the
-- result.
--
--   $internalCheck :: String -> String -> Bool -> a -> a
--
internalCheck :: HasCallStack => String -> Bool -> a -> a
internalCheck = withFrozenCallStack $ check Internal

boundsCheck :: HasCallStack => String -> Bool -> a -> a
boundsCheck = withFrozenCallStack $ check Bounds

unsafeCheck :: HasCallStack => String -> Bool -> a -> a
unsafeCheck = withFrozenCallStack $ check Unsafe


-- | Throw an error if the index is not in range, otherwise evaluate the result.
--
indexCheck :: HasCallStack => Int -> Int -> a -> a
indexCheck i n =
  boundsCheck (printf "index out of bounds: i=%d, n=%d" i n) (i >= 0 && i < n)

-- | Print a warning message if the condition evaluates to False.
--
--   $internalWarning :: String -> String -> Bool -> a -> a
--
internalWarning :: HasCallStack => String -> Bool -> a -> a
internalWarning = withFrozenCallStack $ warning Internal

boundsWarning :: HasCallStack => String -> Bool -> a -> a
boundsWarning = withFrozenCallStack $ warning Bounds

unsafeWarning :: HasCallStack => String -> Bool -> a -> a
unsafeWarning = withFrozenCallStack $ warning Unsafe


error :: HasCallStack => Check -> String -> a
error kind msg = errorWithoutStackTrace (format kind msg)

check :: HasCallStack => Check -> String -> Bool -> a -> a
check kind msg cond k =
  case not (doChecks kind) || cond of
    True  -> k
    False -> errorWithoutStackTrace (format kind msg)

warning :: HasCallStack => Check -> String -> Bool -> a -> a
warning kind msg cond k =
  case not (doChecks kind) || cond of
    True  -> k
    False -> trace (format kind msg) k

format :: HasCallStack => Check -> String -> String
format kind msg = unlines [ header, msg, "", prettyCallStack callStack ]
  where
    header
      = unlines
      $ case kind of
          Internal -> [""
                      ,"*** Internal error in package accelerate ***"
                      ,"*** Please submit a bug report at https://github.com/AccelerateHS/accelerate/issues"]
          _        -> []


-- CPP malarky
-- -----------

{-# INLINE doChecks #-}
doChecks :: Check -> Bool
doChecks Bounds   = doBoundsChecks
doChecks Unsafe   = doUnsafeChecks
doChecks Internal = doInternalChecks

doBoundsChecks :: Bool
#ifdef ACCELERATE_BOUNDS_CHECKS
doBoundsChecks = True
#else
doBoundsChecks = False
#endif

doUnsafeChecks :: Bool
#ifdef ACCELERATE_UNSAFE_CHECKS
doUnsafeChecks = True
#else
doUnsafeChecks = False
#endif

doInternalChecks :: Bool
#ifdef ACCELERATE_INTERNAL_CHECKS
doInternalChecks = True
#else
doInternalChecks = False
#endif

