{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
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

  Bool(..), pattern True_, pattern False_,
  Eq(..),
  (&&), (&&!),
  (||), (||!),
  not,

) where

import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Pattern.Bool
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Type

import Data.Bool                                                    ( Bool(..) )
import Data.Char                                                    ( Char )
import Text.Printf
import Prelude                                                      ( ($), String, Num(..), show, error, return, concat, map, zipWith, foldr1, mapM )
import Language.Haskell.TH                                          hiding ( Exp )
import Language.Haskell.TH.Extra
import qualified Prelude                                            as P


infix 4 ==
infix 4 /=

-- | Conjunction: True if both arguments are true. This is a short-circuit
-- operator, so the second argument will be evaluated only if the first is true.
--
infixr 3 &&
(&&) :: Exp Bool -> Exp Bool -> Exp Bool
(&&) (Exp x) (Exp y) =
  mkExp $ SmartExp (Cond (SmartExp $ Prj PairIdxLeft x)
                         (SmartExp $ Prj PairIdxLeft y)
                         (SmartExp $ Const scalarTypeWord8 0))
          `Pair` SmartExp Nil

-- | Conjunction: True if both arguments are true. This is a strict version of
-- '(&&)': it will always evaluate both arguments, even when the first is false.
--
-- @since 1.3.0.0
--
infixr 3 &&!
(&&!) :: Exp Bool -> Exp Bool -> Exp Bool
(&&!) = mkLAnd

-- | Disjunction: True if either argument is true. This is a short-circuit
-- operator, so the second argument will be evaluated only if the first is
-- false.
--
infixr 2 ||
(||) :: Exp Bool -> Exp Bool -> Exp Bool
(||) (Exp x) (Exp y) =
  mkExp $ SmartExp (Cond (SmartExp $ Prj PairIdxLeft x)
                         (SmartExp $ Const scalarTypeWord8 1)
                         (SmartExp $ Prj PairIdxLeft y))
          `Pair` SmartExp Nil


-- | Disjunction: True if either argument is true. This is a strict version of
-- '(||)': it will always evaluate both arguments, even when the first is true.
--
-- @since 1.3.0.0
--
infixr 2 ||!
(||!) :: Exp Bool -> Exp Bool -> Exp Bool
(||!) = mkLOr

-- | Logical negation
--
not :: Exp Bool -> Exp Bool
not = mkLNot


-- | The 'Eq' class defines equality '==' and inequality '/=' for scalar
-- Accelerate expressions.
--
-- For convenience, we include 'Elt' as a superclass.
--
class Elt a => Eq a where
  (==) :: Exp a -> Exp a -> Exp Bool
  (/=) :: Exp a -> Exp a -> Exp Bool
  {-# MINIMAL (==) | (/=) #-}
  x == y = mkLNot (x /= y)
  x /= y = mkLNot (x == y)


instance Eq () where
  _ == _ = True_
  _ /= _ = False_

instance Eq Z where
  _ == _ = True_
  _ /= _ = False_

instance Eq sh => Eq (sh :. Int) where
  x == y = indexHead x == indexHead y && indexTail x == indexTail y
  x /= y = indexHead x /= indexHead y || indexTail x /= indexTail y

instance Eq Bool where
  x == y = mkCoerce x == (mkCoerce y :: Exp PrimBool)
  x /= y = mkCoerce x /= (mkCoerce y :: Exp PrimBool)

-- Instances of 'Prelude.Eq' don't make sense with the standard signatures as
-- the return type is fixed to 'Bool'. This instance is provided to provide
-- a useful error message.
--
instance P.Eq (Exp a) where
  (==) = preludeError "Eq.(==)" "(==)"
  (/=) = preludeError "Eq.(/=)" "(/=)"

preludeError :: String -> String -> a
preludeError x y = error (printf "Prelude.%s applied to EDSL types: use Data.Array.Accelerate.%s instead" x y)

$(runQ $ do
    let
        integralTypes :: [Name]
        integralTypes =
          [ ''Int
          , ''Int8
          , ''Int16
          , ''Int32
          , ''Int64
          , ''Word
          , ''Word8
          , ''Word16
          , ''Word32
          , ''Word64
          ]

        floatingTypes :: [Name]
        floatingTypes =
          [ ''Half
          , ''Float
          , ''Double
          ]

        nonNumTypes :: [Name]
        nonNumTypes =
          [ ''Char
          ]

        cTypes :: [Name]
        cTypes =
          [ ''CInt
          , ''CUInt
          , ''CLong
          , ''CULong
          , ''CLLong
          , ''CULLong
          , ''CShort
          , ''CUShort
          , ''CChar
          , ''CUChar
          , ''CSChar
          , ''CFloat
          , ''CDouble
          ]

        mkPrim :: Name -> Q [Dec]
        mkPrim t =
          [d| instance Eq $(conT t) where
                (==) = mkEq
                (/=) = mkNEq
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
    cs <- mapM mkPrim cTypes
    ts <- mapM mkTup [2..16]
    return $ concat (concat [is,fs,ns,cs,ts])
 )

