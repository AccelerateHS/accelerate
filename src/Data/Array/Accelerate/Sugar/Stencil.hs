{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Sugar.Stencil
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Sugar.Stencil
  where

import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Representation.Type
import qualified Data.Array.Accelerate.Representation.Stencil       as R
import Data.Array.Accelerate.Representation.Stencil                 hiding ( StencilR, stencilR )

import Data.Kind
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Smart
import qualified Data.Array.Accelerate.Sugar.Shape as Sugar
import Data.Array.Accelerate.AST.Idx

-- Stencil reification
-- -------------------
--
-- In the AST representation, we turn the stencil type from nested tuples
-- of Accelerate expressions into an Accelerate expression whose type is
-- a tuple nested in the same manner. This enables us to represent the
-- stencil function as a unary function (which also only needs one de
-- Bruijn index). The various positions in the stencil are accessed via
-- tuple indices (i.e., projections).
--
class Stencil sh e stencil where
  type StencilR sh stencil :: Type

  stencilR   :: R.StencilR (EltR sh) (EltR e) (StencilR sh stencil)
  stencilPrj :: SmartExp (StencilR sh stencil) -> stencil

-- DIM1
instance Elt e => Stencil Sugar.DIM1 e (Unary (Exp e)) where
  type StencilR Sugar.DIM1 (Unary (Exp e)) = ((), EltR e)
  stencilR = StencilRunit1 @(EltR e) $ eltR @e
  stencilPrj s = Unary (Exp $ prj0 s)

instance Elt e => Stencil Sugar.DIM1 e (Exp e, Exp e, Exp e) where
  type StencilR Sugar.DIM1 (Exp e, Exp e, Exp e)
    = EltR (e, e, e)
  stencilR = StencilRunit3 @(EltR e) $ eltR @e
  stencilPrj s = (Exp $ prj2 s,
                  Exp $ prj1 s,
                  Exp $ prj0 s)

instance Elt e => Stencil Sugar.DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilR Sugar.DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e)
    = EltR (e, e, e, e, e)
  stencilR = StencilRunit5 $ eltR @e
  stencilPrj s = (Exp $ prj4 s,
                  Exp $ prj3 s,
                  Exp $ prj2 s,
                  Exp $ prj1 s,
                  Exp $ prj0 s)

instance Elt e => Stencil Sugar.DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilR Sugar.DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
    = EltR (e, e, e, e, e, e, e)
  stencilR = StencilRunit7 $ eltR @e
  stencilPrj s = (Exp $ prj6 s,
                  Exp $ prj5 s,
                  Exp $ prj4 s,
                  Exp $ prj3 s,
                  Exp $ prj2 s,
                  Exp $ prj1 s,
                  Exp $ prj0 s)

instance Elt e => Stencil Sugar.DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
  where
  type StencilR Sugar.DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
    = EltR (e, e, e, e, e, e, e, e, e)
  stencilR = StencilRunit9 $ eltR @e
  stencilPrj s = (Exp $ prj8 s,
                  Exp $ prj7 s,
                  Exp $ prj6 s,
                  Exp $ prj5 s,
                  Exp $ prj4 s,
                  Exp $ prj3 s,
                  Exp $ prj2 s,
                  Exp $ prj1 s,
                  Exp $ prj0 s)

-- DIM(n+1)
instance Stencil (sh:.Int) a row => Stencil (sh:.Int:.Int) a (Unary row) where
  type StencilR (sh:.Int:.Int) (Unary row) = Tup1 (StencilR (sh:.Int) row)
  stencilR = StencilRtup1 (stencilR @(sh:.Int) @a @row)
  stencilPrj s = Unary (stencilPrj @(sh:.Int) @a $ prj0 s)

instance (Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row0) => Stencil (sh:.Int:.Int) a (row2, row1, row0) where
  type StencilR (sh:.Int:.Int) (row2, row1, row0)
    = Tup3 (StencilR (sh:.Int) row2) (StencilR (sh:.Int) row1) (StencilR (sh:.Int) row0)
  stencilR = StencilRtup3 (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = (stencilPrj @(sh:.Int) @a $ prj2 s,
                  stencilPrj @(sh:.Int) @a $ prj1 s,
                  stencilPrj @(sh:.Int) @a $ prj0 s)

instance (Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row0) => Stencil (sh:.Int:.Int) a (row4, row3, row2, row1, row0) where
  type StencilR (sh:.Int:.Int) (row4, row3, row2, row1, row0)
    = Tup5 (StencilR (sh:.Int) row4) (StencilR (sh:.Int) row3) (StencilR (sh:.Int) row2)
       (StencilR (sh:.Int) row1) (StencilR (sh:.Int) row0)
  stencilR = StencilRtup5 (stencilR @(sh:.Int) @a @row4) (stencilR @(sh:.Int) @a @row3)
                  (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = (stencilPrj @(sh:.Int) @a $ prj4 s,
                  stencilPrj @(sh:.Int) @a $ prj3 s,
                  stencilPrj @(sh:.Int) @a $ prj2 s,
                  stencilPrj @(sh:.Int) @a $ prj1 s,
                  stencilPrj @(sh:.Int) @a $ prj0 s)

instance (Stencil (sh:.Int) a row6,
          Stencil (sh:.Int) a row5,
          Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row0)
  => Stencil (sh:.Int:.Int) a (row6, row5, row4, row3, row2, row1, row0) where
  type StencilR (sh:.Int:.Int) (row6, row5, row4, row3, row2, row1, row0)
    = Tup7 (StencilR (sh:.Int) row6) (StencilR (sh:.Int) row5) (StencilR (sh:.Int) row4)
       (StencilR (sh:.Int) row3) (StencilR (sh:.Int) row2) (StencilR (sh:.Int) row1)
       (StencilR (sh:.Int) row0)
  stencilR = StencilRtup7 (stencilR @(sh:.Int) @a @row6)
                  (stencilR @(sh:.Int) @a @row5) (stencilR @(sh:.Int) @a @row4) (stencilR @(sh:.Int) @a @row3)
                  (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = (stencilPrj @(sh:.Int) @a $ prj6 s,
                  stencilPrj @(sh:.Int) @a $ prj5 s,
                  stencilPrj @(sh:.Int) @a $ prj4 s,
                  stencilPrj @(sh:.Int) @a $ prj3 s,
                  stencilPrj @(sh:.Int) @a $ prj2 s,
                  stencilPrj @(sh:.Int) @a $ prj1 s,
                  stencilPrj @(sh:.Int) @a $ prj0 s)

instance (Stencil (sh:.Int) a row8,
          Stencil (sh:.Int) a row7,
          Stencil (sh:.Int) a row6,
          Stencil (sh:.Int) a row5,
          Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row0)
  => Stencil (sh:.Int:.Int) a (row8, row7, row6, row5, row4, row3, row2, row1, row0) where
  type StencilR (sh:.Int:.Int) (row8, row7, row6, row5, row4, row3, row2, row1, row0)
    = Tup9 (StencilR (sh:.Int) row8) (StencilR (sh:.Int) row7) (StencilR (sh:.Int) row6)
       (StencilR (sh:.Int) row5) (StencilR (sh:.Int) row4) (StencilR (sh:.Int) row3)
       (StencilR (sh:.Int) row2) (StencilR (sh:.Int) row1) (StencilR (sh:.Int) row0)
  stencilR = StencilRtup9
                  (stencilR @(sh:.Int) @a @row8) (stencilR @(sh:.Int) @a @row7) (stencilR @(sh:.Int) @a @row6)
                  (stencilR @(sh:.Int) @a @row5) (stencilR @(sh:.Int) @a @row4) (stencilR @(sh:.Int) @a @row3)
                  (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = (stencilPrj @(sh:.Int) @a $ prj8 s,
                  stencilPrj @(sh:.Int) @a $ prj7 s,
                  stencilPrj @(sh:.Int) @a $ prj6 s,
                  stencilPrj @(sh:.Int) @a $ prj5 s,
                  stencilPrj @(sh:.Int) @a $ prj4 s,
                  stencilPrj @(sh:.Int) @a $ prj3 s,
                  stencilPrj @(sh:.Int) @a $ prj2 s,
                  stencilPrj @(sh:.Int) @a $ prj1 s,
                  stencilPrj @(sh:.Int) @a $ prj0 s)

-- Smart constructors for stencils
-- -------------------------------




prjTail :: SmartExp (t, a) -> SmartExp t
prjTail = SmartExp . Prj PairIdxLeft

prj0 :: SmartExp (t, a) -> SmartExp a
prj0 = SmartExp . Prj PairIdxRight

prj1 :: SmartExp ((t, a), s0) -> SmartExp a
prj1 = prj0 . prjTail

prj2 :: SmartExp (((t, a), s1), s0) -> SmartExp a
prj2 = prj1 . prjTail

prj3 :: SmartExp ((((t, a), s2), s1), s0) -> SmartExp a
prj3 = prj2 . prjTail

prj4 :: SmartExp (((((t, a), s3), s2), s1), s0) -> SmartExp a
prj4 = prj3 . prjTail

prj5 :: SmartExp ((((((t, a), s4), s3), s2), s1), s0) -> SmartExp a
prj5 = prj4 . prjTail

prj6 :: SmartExp (((((((t, a), s5), s4), s3), s2), s1), s0) -> SmartExp a
prj6 = prj5 . prjTail

prj7 :: SmartExp ((((((((t, a), s6), s5), s4), s3), s2), s1), s0) -> SmartExp a
prj7 = prj6 . prjTail

prj8 :: SmartExp (((((((((t, a), s7), s6), s5), s4), s3), s2), s1), s0) -> SmartExp a
prj8 = prj7 . prjTail

