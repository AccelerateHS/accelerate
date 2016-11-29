{-# LANGUAGE CPP             #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Error
-- Copyright   : [2009..2014] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Error (

  internalError,   boundsError,   unsafeError,
  internalCheck,   boundsCheck,   unsafeCheck,   indexCheck,
  internalWarning, boundsWarning, unsafeWarning,

) where

import Data.List
import Debug.Trace
import Language.Haskell.TH                              hiding ( Unsafe )

data Check = Bounds | Unsafe | Internal


-- | Issue an internal error message
--
--   $internalError :: String -> String -> a
--
internalError :: Q Exp
internalError = appE errorQ [| Internal |]

boundsError :: Q Exp
boundsError = appE errorQ [| Bounds |]

unsafeError :: Q Exp
unsafeError = appE errorQ [| Unsafe |]


-- | Throw an error if the condition evaluates to False, otherwise evaluate the
-- result.
--
--   $internalCheck :: String -> String -> Bool -> a -> a
--
internalCheck :: Q Exp
internalCheck = appE checkQ [| Internal |]

boundsCheck :: Q Exp
boundsCheck = appE checkQ [| Bounds |]

unsafeCheck :: Q Exp
unsafeCheck = appE checkQ [| Unsafe |]


-- | Throw an error if the index is not in range, otherwise evaluate the result.
--
--   $boundsCheck :: String -> Int -> Int -> a -> a
--
indexCheck :: Q Exp
indexCheck = withLocation
  [| \format fn i n x ->
        case not (doChecks Bounds) || (i >= 0 && i < n) of
           True  -> x
           False -> error (format Bounds (call fn ("index out of bounds: " ++ show (i,n)))) x |]


-- | Print a warning message if the condition evaluates to False.
--
--   $internalWarning :: String -> String -> Bool -> a -> a
--
internalWarning :: Q Exp
internalWarning = appE warningQ [| Internal |]

boundsWarning :: Q Exp
boundsWarning = appE warningQ [| Bounds |]

unsafeWarning :: Q Exp
unsafeWarning = appE warningQ [| Unsafe |]


-- Template Haskell implementation
-- -------------------------------

call :: String -> String -> String
call f m = concat ["(", f, "): ", m]

errorQ :: Q Exp
errorQ = withLocation
  [| \format kind fn msg -> error (format kind (call fn msg)) |]

checkQ :: Q Exp
checkQ = withLocation
  [| \format kind fn msg cond x ->
        case not (doChecks kind) || cond of
          True  -> x
          False -> error (format kind (call fn msg)) |]

warningQ :: Q Exp
warningQ = withLocation
  [| \format kind fn msg cond x ->
        case not (doChecks kind) || cond of
          True  -> x
          False -> trace (format kind (call fn msg)) x |]

withLocation :: Q Exp -> Q Exp
withLocation f =
  appE f (locatedMessage =<< location)

locatedMessage :: Loc -> Q Exp
locatedMessage loc =
  [| \kind msg -> message kind ($(litE (stringL (formatLoc loc))) ++ msg) |]

formatLoc :: Loc -> String
formatLoc loc =
  let   file            = loc_filename loc
        (line,col)      = loc_start loc
  in
  intercalate ":" [file, show line, show col, " "]

message :: Check -> String -> String
message kind msg = unlines header ++ msg
  where
    header =
      case kind of
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

