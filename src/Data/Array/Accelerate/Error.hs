{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Error
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Error (

  HasCallStack,
  internalError,   boundsError,   unsafeError,
  internalCheck,   boundsCheck,   unsafeCheck,   indexCheck,
  internalWarning, boundsWarning, unsafeWarning,

) where

import Debug.Trace
import Data.List                                          ( intercalate )
import Text.Printf
import Prelude                                            hiding ( error, id )

import GHC.Stack

import Data.Array.Accelerate.Uncurrency

data Check = Bounds | Unsafe | Internal


-- | Issue an internal error message
--
internalError :: HasCallStack => String -> a
internalError = withFrozenCallStack `id` error Internal

boundsError :: HasCallStack => String -> a
boundsError = withFrozenCallStack `id` error Bounds

unsafeError :: HasCallStack => String -> a
unsafeError = withFrozenCallStack `id` error Unsafe


-- | Throw an error if the condition evaluates to False, otherwise evaluate the
-- result.
--
--   $internalCheck :: String -> String -> Bool -> a -> a
--
internalCheck :: HasCallStack => String -> Bool -> a -> a
internalCheck = withFrozenCallStack `id` check Internal

boundsCheck :: HasCallStack => String -> Bool -> a -> a
boundsCheck = withFrozenCallStack `id` check Bounds

unsafeCheck :: HasCallStack => String -> Bool -> a -> a
unsafeCheck = withFrozenCallStack `id` check Unsafe


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
internalWarning = withFrozenCallStack `id` warning Internal

boundsWarning :: HasCallStack => String -> Bool -> a -> a
boundsWarning = withFrozenCallStack `id` warning Bounds

unsafeWarning :: HasCallStack => String -> Bool -> a -> a
unsafeWarning = withFrozenCallStack `id` warning Unsafe


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
format kind msg = intercalate "\n" [ header, msg, ppCallStack callStack ]
  where
    header
      = intercalate "\n"
      `id` case kind of
          Internal -> [""
                      ,"*** Internal error in package accelerate ***"
                      ,"*** Please submit a bug report at https://github.com/AccelerateHS/accelerate/issues"
                      ,""]
          _        -> []

ppCallStack :: CallStack -> String
ppCallStack = intercalate "\n" . ppLines
  where
    ppLines cs =
      case getCallStack cs of
        [] -> []
        st -> ""
            : "CallStack (from HasCallStack):"
            : map (("  " ++) . ppCallSite) st

    ppCallSite (f, loc) = f ++ ": " ++ ppSrcLoc loc

    ppSrcLoc SrcLoc{..} =
      foldr (++) ""
        [ srcLocModule, ":"
        , show srcLocStartLine, ":"
        , show srcLocStartCol
        ]

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

