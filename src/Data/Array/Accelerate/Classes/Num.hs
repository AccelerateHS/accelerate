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

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Prelude                                                      ( ($), (.) )
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
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp Int8) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp Int16) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp Int32) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp Int64) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp Word) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp Word8) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp Word16) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp Word32) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp Word64) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp CInt) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp CUInt) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp CLong) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp CULong) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp CLLong) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp CULLong) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp CShort) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp CUShort) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp Half) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp Float) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp Double) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp CFloat) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger

instance P.Num (Exp CDouble) where
  (+)         = withExecutionStackAsCallStack mkAdd
  (-)         = withExecutionStackAsCallStack mkSub
  (*)         = withExecutionStackAsCallStack mkMul
  negate      = withExecutionStackAsCallStack mkNeg
  abs         = withExecutionStackAsCallStack mkAbs
  signum      = withExecutionStackAsCallStack mkSig
  fromInteger = withExecutionStackAsCallStack $ constant . P.fromInteger
