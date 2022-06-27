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
-- Module      : Data.Array.Accelerate.Classes.Ord
-- Copyright   : [2016..2020] The Accelerate Team
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

import Data.Array.Accelerate.AST                                    ( PrimFun(..), BitOrMask )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Pattern.Ordering
import Data.Array.Accelerate.Pattern.Tuple
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type
import {-# SOURCE #-} Data.Array.Accelerate.Classes.VOrd

import Data.Bits
import Data.Char
import Language.Haskell.TH.Extra                                    hiding ( Exp )
import Prelude                                                      ( ($), Num(..), Ordering(..), Maybe(..), String, show, error, unlines, return, concat, map, mapM )
import Text.Printf
import qualified Prelude                                            as P

import GHC.Exts
import GHC.TypeLits


infix 4 <
infix 4 >
infix 4 <=
infix 4 >=

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

  x <  y = cond (compare x y == LT_) True_  False_
  x <= y = cond (compare x y == GT_) False_ True_
  x >  y = cond (compare x y == GT_) True_  False_
  x >= y = cond (compare x y == LT_) False_ True_

  min x y = cond (x <= y) x y
  max x y = cond (x <= y) y x

  compare x y
    = cond (x == y) EQ_
    $ cond (x <= y) LT_
         {- else -} GT_

-- Local redefinition to prevent cyclic imports
--
cond :: Elt a => Exp Bool -> Exp a -> Exp a -> Exp a
cond (Exp c) (Exp x) (Exp y) = Exp $ SmartExp $ Cond (mkCoerce' c) x y

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

preludeError :: HasCallStack => String -> String -> a
preludeError x y
  = error
  $ unlines [ printf "Prelude.%s applied to EDSL types: use Data.Array.Accelerate.%s instead" x y
            , ""
            , "These Prelude.Ord instances are present only to fulfil superclass"
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
      mkLtEq' [x] [y]       = [| $x <= $y |]
      mkLtEq' (x:xs) (y:ys) = [| $x < $y || ( $x == $y && $(mkLtEq' xs ys) ) |]
      mkLtEq' _      _      = error "mkLtEq'"

      mkGtEq' :: [ExpQ] -> [ExpQ] -> ExpQ
      mkGtEq' [x]    [y]    = [| $x >= $y |]
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
  ts <- mapM mkTup [2..16]
  return $ concat (concat [is,fs,ns,ts])


instance Ord () where
  (<)     _ _ = constant False
  (>)     _ _ = constant False
  (>=)    _ _ = constant True
  (<=)    _ _ = constant True
  min     _ _ = constant ()
  max     _ _ = constant ()
  compare _ _ = constant EQ

instance Ord Z where
  (<)     _ _ = constant False
  (>)     _ _ = constant False
  (<=)    _ _ = constant True
  (>=)    _ _ = constant True
  min     _ _ = constant Z
  max     _ _ = constant Z
  compare _ _ = constant EQ

instance Ord sh => Ord (sh :. Int) where
  x <= y = indexHead x <= indexHead y && indexTail x <= indexTail y
  x >= y = indexHead x >= indexHead y && indexTail x >= indexTail y
  x < y  = indexHead x < indexHead y
        && case matchTypeR (eltR @sh) (eltR @Z) of
             Just Refl -> constant True
             Nothing   -> indexTail x < indexTail y
  x > y  = indexHead x > indexHead y
        && case matchTypeR (eltR @sh) (eltR @Z) of
             Just Refl -> constant True
             Nothing   -> indexTail x > indexTail y

instance Ord Ordering where
  x < y   = mkCoerce x < (mkCoerce y :: Exp TAG)
  x > y   = mkCoerce x > (mkCoerce y :: Exp TAG)
  x <= y  = mkCoerce x <= (mkCoerce y :: Exp TAG)
  x >= y  = mkCoerce x >= (mkCoerce y :: Exp TAG)
  min x y = mkCoerce $ min (mkCoerce x) (mkCoerce y :: Exp TAG)
  max x y = mkCoerce $ max (mkCoerce x) (mkCoerce y :: Exp TAG)

instance VOrd n a => Ord (Vec n a) where
  (<)  = vcmp (<*)
  (>)  = vcmp (>*)
  (<=) = vcmp (<=*)
  (>=) = vcmp (>=*)

vcmp :: forall n a. KnownNat n
     => (Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool))
     -> (Exp (Vec n a) -> Exp (Vec n a) -> Exp Bool)
vcmp op x y =
  let n :: Int
      n = fromInteger $ natVal' (proxy# :: Proxy# n)
      v = op x y
      --
      cmp :: forall t. (Elt t, Num t, Bits t, IsScalar (EltR t), IsIntegral (EltR t), BitOrMask (EltR t) ~ Bit)
          => Exp (Vec n Bool)
          -> Exp Bool
      cmp u =
        let u' = mkPrimUnary (PrimFromBool bitType integralType) u :: Exp t
         in mkEq (constant ((1 `unsafeShiftL` n) - 1)) u'
  in
  if n P.<= 8   then cmp @Word8   v else
  if n P.<= 16  then cmp @Word16  v else
  if n P.<= 32  then cmp @Word32  v else
  if n P.<= 64  then cmp @Word64  v else
  if n P.<= 128 then cmp @Word128 v else
    internalError "Can not handle Vec types with more than 128 lanes"

