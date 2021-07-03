{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Representation.Stencil
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Representation.Stencil (

  -- ** Stencil patterns
  StencilR(..),
  stencilArrayR,
  stencilR, stencilEltR, stencilShapeR, stencilHalo,
  rnfStencilR,
  liftStencilR,

) where

import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type

import Language.Haskell.TH.Extra


-- | GADT reifying the 'Stencil' class
--
data StencilR sh e pat where
  StencilRunit3 :: TypeR e -> StencilR DIM1 e (Tup3 e e e)
  StencilRunit5 :: TypeR e -> StencilR DIM1 e (Tup5 e e e e e)
  StencilRunit7 :: TypeR e -> StencilR DIM1 e (Tup7 e e e e e e e)
  StencilRunit9 :: TypeR e -> StencilR DIM1 e (Tup9 e e e e e e e e e)

  StencilRtup3  :: StencilR sh e pat1
                -> StencilR sh e pat2
                -> StencilR sh e pat3
                -> StencilR (sh, Int) e (Tup3 pat1 pat2 pat3)

  StencilRtup5  :: StencilR sh e pat1
                -> StencilR sh e pat2
                -> StencilR sh e pat3
                -> StencilR sh e pat4
                -> StencilR sh e pat5
                -> StencilR (sh, Int) e (Tup5 pat1 pat2 pat3 pat4 pat5)

  StencilRtup7  :: StencilR sh e pat1
                -> StencilR sh e pat2
                -> StencilR sh e pat3
                -> StencilR sh e pat4
                -> StencilR sh e pat5
                -> StencilR sh e pat6
                -> StencilR sh e pat7
                -> StencilR (sh, Int) e (Tup7 pat1 pat2 pat3 pat4 pat5 pat6 pat7)

  StencilRtup9  :: StencilR sh e pat1
                -> StencilR sh e pat2
                -> StencilR sh e pat3
                -> StencilR sh e pat4
                -> StencilR sh e pat5
                -> StencilR sh e pat6
                -> StencilR sh e pat7
                -> StencilR sh e pat8
                -> StencilR sh e pat9
                -> StencilR (sh, Int) e (Tup9 pat1 pat2 pat3 pat4 pat5 pat6 pat7 pat8 pat9)

stencilEltR :: StencilR sh e pat -> TypeR e
stencilEltR (StencilRunit3 t) = t
stencilEltR (StencilRunit5 t) = t
stencilEltR (StencilRunit7 t) = t
stencilEltR (StencilRunit9 t) = t
stencilEltR (StencilRtup3 sR _ _) = stencilEltR sR
stencilEltR (StencilRtup5 sR _ _ _ _) = stencilEltR sR
stencilEltR (StencilRtup7 sR _ _ _ _ _ _) = stencilEltR sR
stencilEltR (StencilRtup9 sR _ _ _ _ _ _ _ _) = stencilEltR sR

stencilShapeR :: StencilR sh e pat -> ShapeR sh
stencilShapeR (StencilRunit3 _) = ShapeRsnoc ShapeRz
stencilShapeR (StencilRunit5 _) = ShapeRsnoc ShapeRz
stencilShapeR (StencilRunit7 _) = ShapeRsnoc ShapeRz
stencilShapeR (StencilRunit9 _) = ShapeRsnoc ShapeRz
stencilShapeR (StencilRtup3 sR _ _) = ShapeRsnoc $ stencilShapeR sR
stencilShapeR (StencilRtup5 sR _ _ _ _) = ShapeRsnoc $ stencilShapeR sR
stencilShapeR (StencilRtup7 sR _ _ _ _ _ _) = ShapeRsnoc $ stencilShapeR sR
stencilShapeR (StencilRtup9 sR _ _ _ _ _ _ _ _) = ShapeRsnoc $ stencilShapeR sR

stencilR :: StencilR sh e pat -> TypeR pat
stencilR (StencilRunit3 t) = tupR3 t t t
stencilR (StencilRunit5 t) = tupR5 t t t t t
stencilR (StencilRunit7 t) = tupR7 t t t t t t t
stencilR (StencilRunit9 t) = tupR9 t t t t t t t t t
stencilR (StencilRtup3 s1 s2 s3) = tupR3 (stencilR s1) (stencilR s2) (stencilR s3)
stencilR (StencilRtup5 s1 s2 s3 s4 s5) = tupR5 (stencilR s1) (stencilR s2) (stencilR s3) (stencilR s4) (stencilR s5)
stencilR (StencilRtup7 s1 s2 s3 s4 s5 s6 s7) = tupR7 (stencilR s1) (stencilR s2) (stencilR s3) (stencilR s4) (stencilR s5) (stencilR s6) (stencilR s7)
stencilR (StencilRtup9 s1 s2 s3 s4 s5 s6 s7 s8 s9) = tupR9 (stencilR s1) (stencilR s2) (stencilR s3) (stencilR s4) (stencilR s5) (stencilR s6) (stencilR s7) (stencilR s8) (stencilR s9)

stencilArrayR :: StencilR sh e pat -> ArrayR (Array sh e)
stencilArrayR sR = ArrayR (stencilShapeR sR) (stencilEltR sR)

stencilHalo :: StencilR sh e stencil -> (ShapeR sh, sh)
stencilHalo = go'
  where
    go' :: StencilR sh e stencil -> (ShapeR sh, sh)
    go' StencilRunit3{} = (dim1, ((), 1))
    go' StencilRunit5{} = (dim1, ((), 2))
    go' StencilRunit7{} = (dim1, ((), 3))
    go' StencilRunit9{} = (dim1, ((), 4))
    --
    go' (StencilRtup3 a b c            ) = (ShapeRsnoc shR, cons shR 1 $ foldl1 (union shR) [a', go b, go c])
      where (shR, a') = go' a
    go' (StencilRtup5 a b c d e        ) = (ShapeRsnoc shR, cons shR 2 $ foldl1 (union shR) [a', go b, go c, go d, go e])
      where (shR, a') = go' a
    go' (StencilRtup7 a b c d e f g    ) = (ShapeRsnoc shR, cons shR 3 $ foldl1 (union shR) [a', go b, go c, go d, go e, go f, go g])
      where (shR, a') = go' a
    go' (StencilRtup9 a b c d e f g h i) = (ShapeRsnoc shR, cons shR 4 $ foldl1 (union shR) [a', go b, go c, go d, go e, go f, go g, go h, go i])
      where (shR, a') = go' a

    go :: StencilR sh e stencil -> sh
    go = snd . go'

    cons :: ShapeR sh -> Int -> sh -> (sh, Int)
    cons ShapeRz          ix ()       = ((), ix)
    cons (ShapeRsnoc shr) ix (sh, sz) = (cons shr ix sh, sz)

tupR3 :: TupR s t1 -> TupR s t2 -> TupR s t3 -> TupR s (Tup3 t1 t2 t3)
tupR3 t1 t2 t3 = TupRunit `TupRpair` t1 `TupRpair` t2 `TupRpair` t3

tupR5 :: TupR s t1 -> TupR s t2 -> TupR s t3 -> TupR s t4 -> TupR s t5 -> TupR s (Tup5 t1 t2 t3 t4 t5)
tupR5 t1 t2 t3 t4 t5 = TupRunit `TupRpair` t1 `TupRpair` t2 `TupRpair` t3 `TupRpair` t4 `TupRpair` t5

tupR7 :: TupR s t1 -> TupR s t2 -> TupR s t3 -> TupR s t4 -> TupR s t5 -> TupR s t6 -> TupR s t7 -> TupR s (Tup7 t1 t2 t3 t4 t5 t6 t7)
tupR7 t1 t2 t3 t4 t5 t6 t7 = TupRunit `TupRpair` t1 `TupRpair` t2 `TupRpair` t3 `TupRpair` t4 `TupRpair` t5 `TupRpair` t6 `TupRpair` t7

tupR9 :: TupR s t1 -> TupR s t2 -> TupR s t3 -> TupR s t4 -> TupR s t5 -> TupR s t6 -> TupR s t7 -> TupR s t8 -> TupR s t9 -> TupR s (Tup9 t1 t2 t3 t4 t5 t6 t7 t8 t9)
tupR9 t1 t2 t3 t4 t5 t6 t7 t8 t9 = TupRunit `TupRpair` t1 `TupRpair` t2 `TupRpair` t3 `TupRpair` t4 `TupRpair` t5 `TupRpair` t6 `TupRpair` t7 `TupRpair` t8 `TupRpair` t9

rnfStencilR :: StencilR sh e pat -> ()
rnfStencilR (StencilRunit3 t) = rnfTypeR t
rnfStencilR (StencilRunit5 t) = rnfTypeR t
rnfStencilR (StencilRunit7 t) = rnfTypeR t
rnfStencilR (StencilRunit9 t) = rnfTypeR t
rnfStencilR (StencilRtup3 s1 s2 s3) = rnfStencilR s1 `seq` rnfStencilR s2 `seq` rnfStencilR s3
rnfStencilR (StencilRtup5 s1 s2 s3 s4 s5) = rnfStencilR s1 `seq` rnfStencilR s2 `seq` rnfStencilR s3 `seq` rnfStencilR s4 `seq` rnfStencilR s5
rnfStencilR (StencilRtup7 s1 s2 s3 s4 s5 s6 s7) = rnfStencilR s1 `seq` rnfStencilR s2 `seq` rnfStencilR s3 `seq` rnfStencilR s4 `seq` rnfStencilR s5 `seq` rnfStencilR s6 `seq` rnfStencilR s7
rnfStencilR (StencilRtup9 s1 s2 s3 s4 s5 s6 s7 s8 s9) = rnfStencilR s1 `seq` rnfStencilR s2 `seq` rnfStencilR s3 `seq` rnfStencilR s4 `seq` rnfStencilR s5 `seq` rnfStencilR s6 `seq` rnfStencilR s7 `seq` rnfStencilR s8 `seq` rnfStencilR s9

liftStencilR :: StencilR sh e pat -> CodeQ (StencilR sh e pat)
liftStencilR (StencilRunit3 tp) = [|| StencilRunit3 $$(liftTypeR tp) ||]
liftStencilR (StencilRunit5 tp) = [|| StencilRunit5 $$(liftTypeR tp) ||]
liftStencilR (StencilRunit7 tp) = [|| StencilRunit7 $$(liftTypeR tp) ||]
liftStencilR (StencilRunit9 tp) = [|| StencilRunit9 $$(liftTypeR tp) ||]
liftStencilR (StencilRtup3 s1 s2 s3) = [|| StencilRtup3 $$(liftStencilR s1) $$(liftStencilR s2) $$(liftStencilR s3) ||]
liftStencilR (StencilRtup5 s1 s2 s3 s4 s5) = [|| StencilRtup5 $$(liftStencilR s1) $$(liftStencilR s2) $$(liftStencilR s3) $$(liftStencilR s4) $$(liftStencilR s5) ||]
liftStencilR (StencilRtup7 s1 s2 s3 s4 s5 s6 s7) = [|| StencilRtup7 $$(liftStencilR s1) $$(liftStencilR s2) $$(liftStencilR s3) $$(liftStencilR s4) $$(liftStencilR s5) $$(liftStencilR s6) $$(liftStencilR s7) ||]
liftStencilR (StencilRtup9 s1 s2 s3 s4 s5 s6 s7 s8 s9) = [|| StencilRtup9 $$(liftStencilR s1) $$(liftStencilR s2) $$(liftStencilR s3) $$(liftStencilR s4) $$(liftStencilR s5) $$(liftStencilR s6) $$(liftStencilR s7) $$(liftStencilR s8) $$(liftStencilR s9) ||]

