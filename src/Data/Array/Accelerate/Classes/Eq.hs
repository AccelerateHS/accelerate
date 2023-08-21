{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Eq
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Eq (

  Bool, pattern True, pattern False,
  Eq(..),
  (&&), (&&!),
  (||), (||!),
  not,

) where

import Data.Array.Accelerate.AST                                    ( PrimFun(..) )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Pattern.Bool
import Data.Array.Accelerate.Pattern.Tuple
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type
import {-# SOURCE #-} Data.Array.Accelerate.Classes.VEq

import Text.Printf
import Data.Char
import Prelude                                                      ( ($), String, Num(..), Ordering(..), show, error, return, concat, map, zipWith, foldr1, mapM )
import Language.Haskell.TH.Extra                                    hiding ( Exp )
import qualified Prelude                                            as P


infix 4 ==
infix 4 /=

-- | Conjunction: True if both arguments are true. This is a short-circuit
-- operator, so the second argument will be evaluated only if the first is true.
--
infixr 3 &&
(&&) :: Exp Bool -> Exp Bool -> Exp Bool
(&&) (Exp x) (Exp y) =
  mkExp $ Cond x y (SmartExp $ Const (scalarType @PrimBool) 0)

-- | Conjunction: True if both arguments are true. This is a strict version of
-- '(&&)': it will always evaluate both arguments, even when the first is false.
--
-- @since 1.3.0.0
--
infixr 3 &&!
(&&!) :: Exp Bool -> Exp Bool -> Exp Bool
(&&!) = mkPrimBinary $ PrimLAnd bitType

-- | Disjunction: True if either argument is true. This is a short-circuit
-- operator, so the second argument will be evaluated only if the first is
-- false.
--
infixr 2 ||
(||) :: Exp Bool -> Exp Bool -> Exp Bool
(||) (Exp x) (Exp y) =
  mkExp $ Cond x (SmartExp $ Const (scalarType @PrimBool) 1) y


-- | Disjunction: True if either argument is true. This is a strict version of
-- '(||)': it will always evaluate both arguments, even when the first is true.
--
-- @since 1.3.0.0
--
infixr 2 ||!
(||!) :: Exp Bool -> Exp Bool -> Exp Bool
(||!) = mkPrimBinary $ PrimLOr bitType

-- | Logical negation
--
not :: Exp Bool -> Exp Bool
not = mkPrimUnary $ PrimLNot bitType


-- | The 'Eq' class defines equality '(==)' and inequality '(/=)' for
-- Accelerate expressions.
--
-- Vector types behave analogously to tuple types. For testing equality
-- lane-wise on each element of a vector, see the class
-- 'Data.Array.Accelerate.VEq'.
--
class Elt a => Eq a where
  (==) :: Exp a -> Exp a -> Exp Bool
  (/=) :: Exp a -> Exp a -> Exp Bool
  {-# MINIMAL (==) | (/=) #-}
  x == y = not (x /= y)
  x /= y = not (x == y)


instance Eq () where
  _ == _ = True
  _ /= _ = False

instance Eq Z where
  _ == _ = True
  _ /= _ = False

-- Instances of 'Prelude.Eq' don't make sense with the standard signatures as
-- the return type is fixed to 'Bool'. This instance is provided to provide
-- a useful error message.
--
instance P.Eq (Exp a) where
  (==) = preludeError "Eq.(==)" "(==)"
  (/=) = preludeError "Eq.(/=)" "(/=)"

preludeError :: HasCallStack => String -> String -> a
preludeError x y
  = error
  $ P.unlines [ printf "Prelude.%s applied to EDSL types: use Data.Array.Accelerate.%s instead" x y
              , ""
              , "These Prelude.Eq instances are present only to fulfil superclass"
              , "constraints for subsequent classes in the standard Haskell numeric"
              , "hierarchy."
              ]

runQ $ do
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

      nonNumTypes :: [Name]
      nonNumTypes =
        [ ''Char
        ]

      mkPrim :: Name -> Q [Dec]
      mkPrim t =
        [d| instance Eq $(conT t) where
              (==) = mkPrimBinary $ PrimEq  scalarType
              (/=) = mkPrimBinary $ PrimNEq scalarType
          |]

      mkTup :: Int -> Q [Dec]
      mkTup n =
        let
            xs      = [ mkName ('x':show i) | i <- [0 .. n-1] ]
            ys      = [ mkName ('y':show i) | i <- [0 .. n-1] ]
            cst     = tupT (map (\x -> [t| Eq $(varT x) |]) xs)
            res     = tupT (map varT xs)
            pat vs  = conP (mkName ('T':show n)) (map varP vs)
        in
        [d| instance ($cst) => Eq $res where
              $(pat xs) == $(pat ys) = $(foldr1 (\vs v -> [| $vs && $v |]) (zipWith (\x y -> [| $x == $y |]) (map varE xs) (map varE ys)))
              $(pat xs) /= $(pat ys) = $(foldr1 (\vs v -> [| $vs || $v |]) (zipWith (\x y -> [| $x /= $y |]) (map varE xs) (map varE ys)))
          |]

  is <- mapM mkPrim integralTypes
  fs <- mapM mkPrim floatingTypes
  ns <- mapM mkPrim nonNumTypes
  ts <- mapM mkTup [2..16]
  return $ concat (concat [is,fs,ns,ts])

instance Eq sh => Eq (sh :. Int) where
  x == y = indexHead x == indexHead y && indexTail x == indexTail y
  x /= y = indexHead x /= indexHead y || indexTail x /= indexTail y

instance Eq Bool where
  (==) = mkPrimBinary $ PrimEq  scalarType
  (/=) = mkPrimBinary $ PrimNEq scalarType

instance Eq Ordering where
  x == y = mkCoerce x == (mkCoerce y :: Exp TAG)
  x /= y = mkCoerce x /= (mkCoerce y :: Exp TAG)

instance VEq n a => Eq (Vec n a) where
  x == y = vand (x ==* y)
  x /= y = vor  (x /=* y)

