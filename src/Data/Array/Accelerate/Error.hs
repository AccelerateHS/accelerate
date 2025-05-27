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

import Data.Text.Lazy.Builder
import Debug.Trace
import Formatting
import Prelude                                                      hiding ( error )

import GHC.Stack

data Check = Bounds | Unsafe | Internal


-- | Issue an internal error message
--
internalError :: HasCallStack => Format r a -> a
internalError = withFrozenCallStack $ error Internal

boundsError :: HasCallStack => Format r a -> a
boundsError = withFrozenCallStack $ error Bounds

unsafeError :: HasCallStack => Format r a -> a
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
  boundsCheck (bformat ("index out of bounds: i=" % int % ", n=" % int) i n) (i >= 0 && i < n)

-- | Print a warning message if the condition evaluates to False.
--
internalWarning :: HasCallStack => Builder -> Bool -> a -> a
internalWarning = withFrozenCallStack $ warning Internal

boundsWarning :: HasCallStack => Builder -> Bool -> a -> a
boundsWarning = withFrozenCallStack $ warning Bounds

unsafeWarning :: HasCallStack => Builder -> Bool -> a -> a
unsafeWarning = withFrozenCallStack $ warning Unsafe


error :: HasCallStack => Check -> Format r a -> a
error kind fmt = runFormat fmt $ \msg -> errorWithoutStackTrace (decorated kind msg)

check :: HasCallStack => Check -> Builder -> Bool -> a -> a
check kind msg cond k =
  case not (doChecks kind) || cond of
    True  -> k
    False -> errorWithoutStackTrace (decorated kind msg)

warning :: HasCallStack => Check -> Builder -> Bool -> a -> a
warning kind msg cond k =
  case not (doChecks kind) || cond of
    True  -> k
    False -> trace (decorated kind msg) k

decorated :: HasCallStack => Check -> Builder -> String
decorated kind msg = formatToString (intercalated "\n" builder) [ header, msg, ppCallStack callStack ]
  where
    header =
      case kind of
        Internal -> bformat (intercalated "\n" builder)
                      [""
                      ,"*** Internal error in package accelerate ***"
                      ,"*** Please submit a bug report at https://github.com/AccelerateHS/accelerate/issues"
                      ,""
                      ]
        _        -> mempty

ppCallStack :: CallStack -> Builder
ppCallStack = ppLines
  where
    ppLines cs =
      case getCallStack cs of
        [] -> mempty
        st -> bformat ("CallStack (from HasCallStack):\n" % indentedLines 2 (later ppCallSite)) st

    ppCallSite (fun, loc) =
      bformat (string % ": " % build)
        fun
        (ppSrcLoc loc)

    ppSrcLoc SrcLoc{..} =
      bformat (string % ":" % int % ":" % int)
        srcLocModule
        srcLocStartLine
        srcLocStartCol


-- CPP malarky
-- -----------

{-# INLINE doChecks #-}
doChecks :: Check -> Bool
doChecks Bounds   = doBoundsChecks
doChecks Unsafe   = internalError "If you want to do unsafe checks, re-enable the unsafe-checks cabal flag. It is currently disabled because there are no unsafe operation checks."
doChecks Internal = doInternalChecks

doBoundsChecks :: Bool
#ifdef ACCELERATE_BOUNDS_CHECKS
doBoundsChecks = True
#else
doBoundsChecks = False
#endif

-- doUnsafeChecks :: Bool
-- #ifdef ACCELERATE_UNSAFE_CHECKS
-- doUnsafeChecks = True
-- #else
-- doUnsafeChecks = False
-- #endif

doInternalChecks :: Bool
#ifdef ACCELERATE_INTERNAL_CHECKS
doInternalChecks = True
#else
doInternalChecks = False
#endif

