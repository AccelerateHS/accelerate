{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Num
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Num (

  Num,
  (P.+), (P.-), (P.*), P.negate, P.abs, P.signum, P.fromInteger,

) where

import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Prelude                                                      ( (.) )
import qualified Prelude                                            as P


-- Note: [Haskell/Accelerate numeric hierarchy]
--
-- Should we replace 'Prelude.Num' with our own version, as we did with 'Ord'
-- and 'Eq'? That might require clients to enable RebindableSyntax in order to
-- get the correct 'fromInteger' (or miss out on special magic and need to add
-- 'constant' instead).
--
-- I think that we should, because otherwise we require FlexibleContexts and
-- constraints are going to be inconsistent, e.g.:
--
-- f :: (P.Num (Exp a), A.Ord a) => ...
--
-- A light-weight alternative is the following constraint kind:
--
-- UPDATE TLM 2018-01-12: I attempted separating the two class hierarchies, and
-- while in principle it works, has very poor ergonomics in modules which use
-- both Accelerate and standard Haskell types. RebindableSyntax only helps for
-- Accelerate-only modules; for mixed-mode files, we need to use every operation
-- qualified, which is a pain. On the other hand, type inference appears to be
-- much, _much_ better.
--


-- | Conversion from an 'Integer'.
--
-- An integer literal represents the application of the function 'fromInteger'
-- to the appropriate value of type 'Integer'. We export this specialised
-- version where the return type is fixed to an 'Exp' term in order to improve
-- type checking in Accelerate modules when @RebindableSyntax@ is enabled.
--
-- fromInteger :: Num a => Integer -> Exp a
-- fromInteger = P.fromInteger


-- | Basic numeric class
--
type Num a = (Elt a, P.Num (Exp a))


instance P.Num (Exp Int) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp Int8) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp Int16) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp Int32) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp Int64) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp Word) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp Word8) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp Word16) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp Word32) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp Word64) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp CInt) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp CUInt) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp CLong) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp CULong) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp CLLong) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp CULLong) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp CShort) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp CUShort) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp Half) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp Float) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp Double) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp CFloat) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance P.Num (Exp CDouble) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger
