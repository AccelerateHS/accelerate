{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans -freduction-depth=100 #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Ord
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Ord (

  Ord(..),
  Ordering(..), pattern LT_, pattern EQ_, pattern GT_,

) where

import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Eq

import Text.Printf
import Prelude                                                      ( ($), (.), (>>=), Ordering(..), Num(..), Maybe(..), String, show, error, unlines, return, concat, map, mapM )
import Language.Haskell.TH                                          hiding ( Exp )
import Language.Haskell.TH.Extra
import qualified Prelude                                            as P


infix 4 <
infix 4 >
infix 4 <=
infix 4 >=

pattern LT_ :: Exp Ordering
pattern LT_ = Exp (Const LT)

pattern EQ_ :: Exp Ordering
pattern EQ_ = Exp (Const EQ)

pattern GT_ :: Exp Ordering
pattern GT_ = Exp (Const GT)
{-# COMPLETE LT_, EQ_, GT_ #-}

-- | The 'Ord' class for totally ordered datatypes
--
class Eq a => Ord a where
  {-# MINIMAL (<=) | compare #-}
  (<)     :: Exp a -> Exp a -> Exp Bool
  (>)     :: Exp a -> Exp a -> Exp Bool
  (<=)    :: Exp a -> Exp a -> Exp Bool
  (>=)    :: Exp a -> Exp a -> Exp Bool
  min     :: Exp a -> Exp a -> Exp a
  max     :: Exp a -> Exp a -> Exp a
  compare :: Exp a -> Exp a -> Exp Ordering

  x <  y = if compare x y == constant LT then constant True  else constant False
  x <= y = if compare x y == constant GT then constant False else constant True
  x >  y = if compare x y == constant GT then constant True  else constant False
  x >= y = if compare x y == constant LT then constant False else constant True

  min x y = if x <= y then x else y
  max x y = if x <= y then y else x

  compare x y =
    if x == y then constant EQ else
    if x <= y then constant LT
              else constant GT

-- Local redefinition for use with RebindableSyntax (pulled forward from Prelude.hs)
--
ifThenElse :: Elt a => Exp Bool -> Exp a -> Exp a -> Exp a
ifThenElse (Exp c) (Exp x) (Exp y) = Exp $ SmartExp $ Cond c x y

instance Ord () where
  (<)     _ _ = constant False
  (>)     _ _ = constant False
  (>=)    _ _ = constant True
  (<=)    _ _ = constant True
  min     _ _ = constant ()
  max     _ _ = constant ()
  compare _ _ = constant EQ

instance Ord Z where
  (<)  _ _ = constant False
  (>)  _ _ = constant False
  (<=) _ _ = constant True
  (>=) _ _ = constant True
  min  _ _ = constant Z
  max  _ _ = constant Z

instance Ord sh => Ord (sh :. Int) where
  x <= y = indexHead x <= indexHead y && indexTail x <= indexTail y
  x >= y = indexHead x >= indexHead y && indexTail x >= indexTail y
  x < y  = indexHead x < indexHead y
        && case matchTupleType (eltType @sh) (eltType @Z) of
             Just Refl -> constant True
             Nothing   -> indexTail x < indexTail y
  x > y  = indexHead x > indexHead y
        && case matchTupleType (eltType @sh) (eltType @Z) of
             Just Refl -> constant True
             Nothing   -> indexTail x > indexTail y

instance Elt Ordering where
  type EltRepr Ordering = Int8
  eltType = TupRsingle scalarType
  fromElt = P.fromIntegral . P.fromEnum
  toElt   = P.toEnum . P.fromIntegral

instance Eq Ordering where
  x == y = mkBitcast x == (mkBitcast y :: Exp Int8)
  x /= y = mkBitcast x /= (mkBitcast y :: Exp Int8)

instance Ord Ordering where
  x < y   = mkBitcast x < (mkBitcast y :: Exp Int8)
  x > y   = mkBitcast x > (mkBitcast y :: Exp Int8)
  x <= y  = mkBitcast x <= (mkBitcast y :: Exp Int8)
  x >= y  = mkBitcast x >= (mkBitcast y :: Exp Int8)
  min x y = mkBitcast $ min (mkBitcast x) (mkBitcast y :: Exp Int8)
  max x y = mkBitcast $ max (mkBitcast x) (mkBitcast y :: Exp Int8)


-- Instances of 'Prelude.Ord' (mostly) don't make sense with the standard
-- signatures as the return type is fixed to 'Bool'. This instance is provided
-- to provide a useful error message.
--
-- Note that 'min' and 'max' are implementable, so we do hook those into the
-- accelerate instances defined here. This allows us to use operations such as
-- 'Prelude.minimum' and 'Prelude.maximum'.
--
instance Ord a => P.Ord (Exp a) where
  (<)     = preludeError "Ord.(<)"  "(<)"
  (<=)    = preludeError "Ord.(<=)" "(<=)"
  (>)     = preludeError "Ord.(>)"  "(>)"
  (>=)    = preludeError "Ord.(>=)" "(>=)"
  min     = min
  max     = max

preludeError :: String -> String -> a
preludeError x y
  = error
  $ unlines [ printf "Prelude.%s applied to EDSL types: use Data.Array.Accelerate.%s instead" x y
            , ""
            , "These Prelude.Ord instances are present only to fulfil superclass"
            , "constraints for subsequent classes in the standard Haskell numeric"
            , "hierarchy."
            ]

-- To support 16-tuples, we must set the maximum recursion depth of the type
-- checker higher. The default is 51, which appears to be a problem for
-- 16-tuples (15-tuples do work). Hence we set a compiler flag at the top
-- of this file: -freduction-depth=100
--

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
          [ ''Bool
          , ''Char
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
          [d| instance Ord $(conT t) where
                (<)  = mkLt
                (>)  = mkGt
                (<=) = mkLtEq
                (>=) = mkGtEq
                min  = mkMin
                max  = mkMax
            |]

        mkLt' :: [ExpQ] -> [ExpQ] -> ExpQ
        mkLt' [x] [y]       = [| $x < $y |]
        mkLt' (x:xs) (y:ys) = [| $x < $y || ( $x == $y && $(mkLt' xs ys) ) |]
        mkLt' _      _      = error "mkLt'"

        mkGt' :: [ExpQ] -> [ExpQ] -> ExpQ
        mkGt' [x]    [y]    = [| $x > $y |]
        mkGt' (x:xs) (y:ys) = [| $x > $y || ( $x == $y && $(mkGt' xs ys) ) |]
        mkGt' _      _      = error "mkGt'"

        mkLtEq' :: [ExpQ] -> [ExpQ] -> ExpQ
        mkLtEq' [x] [y]       = [| $x < $y |]
        mkLtEq' (x:xs) (y:ys) = [| $x < $y || ( $x == $y && $(mkLtEq' xs ys) ) |]
        mkLtEq' _      _      = error "mkLtEq'"

        mkGtEq' :: [ExpQ] -> [ExpQ] -> ExpQ
        mkGtEq' [x]    [y]    = [| $x > $y |]
        mkGtEq' (x:xs) (y:ys) = [| $x > $y || ( $x == $y && $(mkGtEq' xs ys) ) |]
        mkGtEq' _      _      = error "mkGtEq'"

        mkTup :: Int -> Q [Dec]
        mkTup n =
          let
              xs      = [ mkName ('x':show i) | i <- [0 .. n-1] ]
              ys      = [ mkName ('y':show i) | i <- [0 .. n-1] ]
              cst     = tupT (map (\x -> [t| Ord $(varT x) |]) xs)
              res     = tupT (map varT xs)
              pat vs  = conP (mkName ('T':show n)) (map varP vs)
          in
          [d| instance $cst => Ord $res where
                $(pat xs) <  $(pat ys) = $( mkLt' (map varE xs) (map varE ys) )
                $(pat xs) >  $(pat ys) = $( mkGt' (map varE xs) (map varE ys) )
                $(pat xs) >= $(pat ys) = $( mkGtEq' (map varE xs) (map varE ys) )
                $(pat xs) <= $(pat ys) = $( mkLtEq' (map varE xs) (map varE ys) )
            |]

    is <- mapM mkPrim integralTypes
    fs <- mapM mkPrim floatingTypes
    ns <- mapM mkPrim nonNumTypes
    cs <- mapM mkPrim cTypes
    ts <- mapM mkTup [2..16]
    return $ concat (concat [is,fs,ns,cs,ts])
 )

