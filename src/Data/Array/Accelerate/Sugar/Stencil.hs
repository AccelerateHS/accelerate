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

import Data.Kind

-- Reification of the stencil type from nested tuples of Accelerate
-- expressions in the surface language. This enables us to represent the
-- stencil function as a unary function.
--
class Stencil sh e stencil where
  type StencilR sh stencil :: Type
  stencilR :: R.StencilR (EltR sh) (EltR e) (StencilR sh stencil)

instance Elt e => Stencil DIM1 e (exp e, exp e, exp e) where
  type StencilR DIM1 (exp e, exp e, exp e) = EltR (e, e, e)
  stencilR = R.StencilRunit3 $ eltR @e

instance Elt e => Stencil DIM1 e (exp e, exp e, exp e, exp e, exp e) where
  type StencilR DIM1 (exp e, exp e, exp e, exp e, exp e) =
    EltR (e, e, e, e, e)
  stencilR = R.StencilRunit5 $ eltR @e

instance Elt e => Stencil DIM1 e (exp e, exp e, exp e, exp e, exp e, exp e, exp e) where
  type StencilR DIM1 (exp e, exp e, exp e, exp e, exp e, exp e, exp e) =
    EltR (e, e, e, e, e, e, e)
  stencilR = R.StencilRunit7 $ eltR @e

instance Elt e => Stencil DIM1 e (exp e, exp e, exp e, exp e, exp e, exp e, exp e, exp e, exp e) where
  type StencilR DIM1 (exp e, exp e, exp e, exp e, exp e, exp e, exp e, exp e, exp e) =
    EltR (e, e, e, e, e, e, e, e, e)
  stencilR = R.StencilRunit9 $ eltR @e

instance ( Stencil (sh:.Int) a row2
         , Stencil (sh:.Int) a row1
         , Stencil (sh:.Int) a row0
         )
      => Stencil (sh:.Int:.Int) a (row2, row1, row0) where
  type StencilR (sh:.Int:.Int) (row2, row1, row0) =
    Tup3 (StencilR (sh:.Int) row2) (StencilR (sh:.Int) row1) (StencilR (sh:.Int) row0)
  stencilR = R.StencilRtup3 (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)

instance ( Stencil (sh:.Int) a row4
         , Stencil (sh:.Int) a row3
         , Stencil (sh:.Int) a row2
         , Stencil (sh:.Int) a row1
         , Stencil (sh:.Int) a row0
         )
      => Stencil (sh:.Int:.Int) a (row4, row3, row2, row1, row0) where
  type StencilR (sh:.Int:.Int) (row4, row3, row2, row1, row0) =
    Tup5 (StencilR (sh:.Int) row4) (StencilR (sh:.Int) row3) (StencilR (sh:.Int) row2)
         (StencilR (sh:.Int) row1) (StencilR (sh:.Int) row0)
  stencilR = R.StencilRtup5
                  (stencilR @(sh:.Int) @a @row4) (stencilR @(sh:.Int) @a @row3) (stencilR @(sh:.Int) @a @row2)
                  (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)

instance ( Stencil (sh:.Int) a row6
         , Stencil (sh:.Int) a row5
         , Stencil (sh:.Int) a row4
         , Stencil (sh:.Int) a row3
         , Stencil (sh:.Int) a row2
         , Stencil (sh:.Int) a row1
         , Stencil (sh:.Int) a row0
         )
      => Stencil (sh:.Int:.Int) a (row6, row5, row4, row3, row2, row1, row0) where
  type StencilR (sh:.Int:.Int) (row6, row5, row4, row3, row2, row1, row0) =
    Tup7 (StencilR (sh:.Int) row6) (StencilR (sh:.Int) row5) (StencilR (sh:.Int) row4)
         (StencilR (sh:.Int) row3) (StencilR (sh:.Int) row2) (StencilR (sh:.Int) row1)
         (StencilR (sh:.Int) row0)
  stencilR = R.StencilRtup7
                  (stencilR @(sh:.Int) @a @row6) (stencilR @(sh:.Int) @a @row5) (stencilR @(sh:.Int) @a @row4)
                  (stencilR @(sh:.Int) @a @row3) (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1)
                  (stencilR @(sh:.Int) @a @row0)

instance ( Stencil (sh:.Int) a row8
         , Stencil (sh:.Int) a row7
         , Stencil (sh:.Int) a row6
         , Stencil (sh:.Int) a row5
         , Stencil (sh:.Int) a row4
         , Stencil (sh:.Int) a row3
         , Stencil (sh:.Int) a row2
         , Stencil (sh:.Int) a row1
         , Stencil (sh:.Int) a row0
         )
      => Stencil (sh:.Int:.Int) a (row8, row7, row6, row5, row4, row3, row2, row1, row0) where
  type StencilR (sh:.Int:.Int) (row8, row7, row6, row5, row4, row3, row2, row1, row0) =
    Tup9 (StencilR (sh:.Int) row8) (StencilR (sh:.Int) row7) (StencilR (sh:.Int) row6)
         (StencilR (sh:.Int) row5) (StencilR (sh:.Int) row4) (StencilR (sh:.Int) row3)
         (StencilR (sh:.Int) row2) (StencilR (sh:.Int) row1) (StencilR (sh:.Int) row0)
  stencilR = R.StencilRtup9
                  (stencilR @(sh:.Int) @a @row8) (stencilR @(sh:.Int) @a @row7) (stencilR @(sh:.Int) @a @row6)
                  (stencilR @(sh:.Int) @a @row5) (stencilR @(sh:.Int) @a @row4) (stencilR @(sh:.Int) @a @row3)
                  (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)

