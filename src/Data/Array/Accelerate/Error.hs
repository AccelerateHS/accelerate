{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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

import Data.Text.Format                                   ( Only(..), build )
import Data.Text.Format.Extra
import Data.Text.Lazy.Builder
import Debug.Trace
import Prelude                                            hiding ( error )

import GHC.Stack

data Check = Bounds | Unsafe | Internal


-- | Issue an internal error message
--
internalError :: HasCallStack => Builder -> a
internalError = withFrozenCallStack $ error Internal

boundsError :: HasCallStack => Builder -> a
boundsError = withFrozenCallStack $ error Bounds

unsafeError :: HasCallStack => Builder -> a
unsafeError = withFrozenCallStack $ error Unsafe


-- | Throw an error if the condition evaluates to False, otherwise evaluate the
-- result.
--
internalCheck :: HasCallStack => Builder -> Bool -> a -> a
internalCheck = withFrozenCallStack $ check Internal

boundsCheck :: HasCallStack => Builder -> Bool -> a -> a
boundsCheck = withFrozenCallStack $ check Bounds

unsafeCheck :: HasCallStack => Builder -> Bool -> a -> a
unsafeCheck = withFrozenCallStack $ check Unsafe


-- | Throw an error if the index is not in range, otherwise evaluate the result.
--
indexCheck :: HasCallStack => Int -> Int -> a -> a
indexCheck i n =
  boundsCheck (build "index out of bounds: i={}, n={}" (i, n)) (i >= 0 && i < n)

-- | Print a warning message if the condition evaluates to False.
--
internalWarning :: HasCallStack => Builder -> Bool -> a -> a
internalWarning = withFrozenCallStack $ warning Internal

boundsWarning :: HasCallStack => Builder -> Bool -> a -> a
boundsWarning = withFrozenCallStack $ warning Bounds

unsafeWarning :: HasCallStack => Builder -> Bool -> a -> a
unsafeWarning = withFrozenCallStack $ warning Unsafe


error :: HasCallStack => Check -> Builder -> a
error kind msg = errorWithoutStackTrace (format kind msg)

check :: HasCallStack => Check -> Builder -> Bool -> a -> a
check kind msg cond k =
  case not (doChecks kind) || cond of
    True  -> k
    False -> errorWithoutStackTrace (format kind msg)

warning :: HasCallStack => Check -> Builder -> Bool -> a -> a
warning kind msg cond k =
  case not (doChecks kind) || cond of
    True  -> k
    False -> trace (format kind msg) k

format :: HasCallStack => Check -> Builder -> String
format kind msg = show $ intercalate "\n" [ header, msg, ppCallStack callStack ]
  where
    header
      = intercalate "\n"
      $ case kind of
          Internal -> [""
                      ,"*** Internal error in package accelerate ***"
                      ,"*** Please submit a bug report at https://github.com/AccelerateHS/accelerate/issues"
                      ,""]
          _        -> []

ppCallStack :: CallStack -> Builder
ppCallStack = intercalate "\n" . ppLines
  where
    ppLines cs =
      case getCallStack cs of
        [] -> []
        st -> ""
            : "CallStack (from HasCallStack):"
            : map (("  " <>) . ppCallSite) st

    ppCallSite (f, loc) = fromString f <> ": " <> ppSrcLoc loc

    ppSrcLoc SrcLoc{..} =
      foldr (<>) ""
        [ fromString srcLocModule, ":"
        , build "{}" (Only srcLocStartLine), ":"
        , build "{}" (Only srcLocStartCol)
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

