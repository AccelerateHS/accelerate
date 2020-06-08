{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
#if __GLASGOW_HASKELL__ <= 800
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.Pattern
-- Copyright   : [2018..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern (

  pattern Pattern,
  pattern T2,  pattern T3,  pattern T4,  pattern T5,  pattern T6,
  pattern T7,  pattern T8,  pattern T9,  pattern T10, pattern T11,
  pattern T12, pattern T13, pattern T14, pattern T15, pattern T16,

  pattern Z_, pattern Ix, pattern (::.),
  pattern I0, pattern I1, pattern I2, pattern I3, pattern I4,
  pattern I5, pattern I6, pattern I7, pattern I8, pattern I9,

  pattern V2, pattern V3, pattern V4, pattern V8, pattern V16,

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Array.Representation                   ( VecR(..) )
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Language.Haskell.TH                                          hiding ( Exp )
import Language.Haskell.TH.Extra


-- | A pattern synonym for working with (product) data types. You can declare
-- your own pattern synonyms based off of this.
--
pattern Pattern :: forall b a context. IsPattern context a b => b -> context a
pattern Pattern vars <- (destruct @context -> vars)
  where Pattern = construct @context

class IsPattern con a t where
  construct :: t -> con a
  destruct  :: con a -> t


-- | Pattern synonyms for indices, which may be more convenient to use than
-- 'Data.Array.Accelerate.Lift.lift' and
-- 'Data.Array.Accelerate.Lift.unlift'.
--
pattern Z_ :: Exp DIM0
pattern Z_ = Pattern Z
{-# COMPLETE Z_ #-}

infixl 3 ::.
pattern (::.) :: (Elt a, Elt b) => Exp a -> Exp b -> Exp (a :. b)
pattern a ::. b = Pattern (a :. b)
{-# COMPLETE (::.) #-}

pattern Ix :: (Elt a, Elt b) => Exp a -> Exp b -> Exp (a :. b)
pattern a `Ix` b = a ::. b
{-# COMPLETE Ix #-}

-- IsPattern instances for Shape nil and cons
--
instance IsPattern Exp Z Z where
  construct _ = constant Z
  destruct _  = Z

instance (Elt a, Elt b) => IsPattern Exp (a :. b) (Exp a :. Exp b) where
  construct (Exp a :. Exp b) = Exp $ SmartExp $ Pair a b
  destruct (Exp t)           = Exp (SmartExp $ Prj PairIdxLeft t) :. Exp (SmartExp $ Prj PairIdxRight t)

-- Newtype wrapper to distinguish between T and V patterns
--
newtype VecPattern a = VecPattern a

-- IsPattern instances for up to 16-tuples (Acc and Exp). TH takes care of the
-- (unremarkable) boilerplate for us, but since the implementation is a little
-- tricky it is debatable whether or not this is a good idea...
--
$(runQ $ do
    let
        -- Generate instance declarations for IsPattern of the form:
        -- instance (Elt x, EltRepr x ~ (((), EltRepr a), EltRepr b), Elt a, Elt b,) => IsPattern Exp x (Exp a, Exp b)
        mkIsPattern :: Name -> TypeQ -> TypeQ -> ExpQ -> ExpQ -> ExpQ -> ExpQ -> Int -> Q [Dec]
        mkIsPattern con cst repr smart prj nil pair n = do
          a <- newName "a"
          let
              -- Type variables for the elements
              xs       = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              -- Last argument to `IsPattern`, eg (Exp, a, Exp b) in the example
              b        = foldl (\ts t -> appT ts (appT (conT con) (varT t))) (tupleT n) xs
              -- Representation as snoc-list of pairs, eg (((), EltRepr a), EltRepr b)
              snoc     = foldl (\sn t -> [t| ($sn, $(appT repr $ varT t)) |]) [t| () |] xs
              -- Constraints for the type class, consisting of Elt constraints on all type variables,
              -- and an equality constraint on the representation type of `a` and the snoc representation `snoc`.
              contexts = appT cst [t| $(varT a) |]
                       : [t| $repr $(varT a) ~ $snoc |]
                       : map (\t -> appT cst (varT t)) xs
              -- Store all constraints in a tuple
              context  = foldl (\ts t -> appT ts t) (tupleT $ length contexts) contexts
              --
              get x 0 = [| $(conE con) ($smart ($prj PairIdxRight $x)) |]
              get x i = get [| $smart ($prj PairIdxLeft $x) |] (i-1)
          --
          _x <- newName "_x"
          [d| instance $context => IsPattern $(conT con) $(varT a) $b where
                construct $(tupP (map (conP con . return . varP) xs)) =
                  $(conE con) $(foldl (\vs v -> appE smart (appE (appE pair vs) (varE v))) (appE smart nil) xs)
                destruct $(conP con [varP _x]) =
                  $(tupE (map (get (varE _x)) [(n-1), (n-2) .. 0]))
            |]

        mkVecPattern :: Int -> Q [Dec]
        mkVecPattern n = do
          a <- newName "a"
          let
              v = foldr appE [| VecRnil (singleType @(EltRepr $(varT a))) |] (replicate n [| VecRsucc |])
              r = tupT (replicate n [t| Exp $(varT a) |])
              t = tupT (replicate n (varT a))
          --
          [d| instance VecElt $(varT a) => IsPattern Exp (Vec $(litT (numTyLit (fromIntegral n))) $(varT a)) (VecPattern $r) where
                construct (VecPattern x) =
                  case construct x :: Exp $t of
                    Exp x' -> Exp (SmartExp (VecPack $v x'))
                destruct (Exp x) = VecPattern (destruct (Exp (SmartExp (VecUnpack $v x)) :: Exp $t))
            |]

        mkExpPattern = mkIsPattern (mkName "Exp") [t| Elt    |] [t| EltRepr |] [| SmartExp |] [| Prj  |] [| Nil  |] [| Pair  |]
        mkAccPattern = mkIsPattern (mkName "Acc") [t| Arrays |] [t| ArrRepr |] [| SmartAcc |] [| Aprj |] [| Anil |] [| Apair |]
    --
    es <- mapM mkExpPattern [0..16]
    as <- mapM mkAccPattern [0..16]
    vs <- mapM mkVecPattern [2,3,4,8,16]
    return $ concat (es ++ as ++ vs)
 )

-- | Specialised pattern synonyms for tuples, which may be more convenient to
-- use than 'Data.Array.Accelerate.Lift.lift' and
-- 'Data.Array.Accelerate.Lift.unlift'. For example, to construct a pair:
--
-- > let a = 4        :: Exp Int
-- > let b = 2        :: Exp Float
-- > let c = T2 a b   -- :: Exp (Int, Float); equivalent to 'lift (a,b)'
--
-- Similarly they can be used to destruct values:
--
-- > let T2 x y = c   -- x :: Exp Int, y :: Exp Float; equivalent to 'let (x,y) = unlift c'
--
-- These pattern synonyms can be used for both 'Exp' and 'Acc' terms.
--
-- Similarly, we have patterns for constructing and destructing indices of
-- a given dimensionality:
--
-- > let ix = Ix 2 3    -- :: Exp DIM2
-- > let I2 y x = ix    -- y :: Exp Int, x :: Exp Int
--
$(runQ $ do
    let
        mkT :: Int -> Q [Dec]
        mkT n =
          let xs    = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              ts    = map varT xs
              name  = mkName ('T':show n)
              con   = varT (mkName "con")
              ty1   = tupT ts
              ty2   = tupT (map (con `appT`) ts)
              sig   = foldr (\t r -> [t| $con $t -> $r |]) (appT con ty1) ts
          in
          sequence
            [ patSynSigD name [t| IsPattern $con $ty1 $ty2 => $sig |]
            , patSynD    name (prefixPatSyn xs) implBidir [p| Pattern $(tupP (map varP xs)) |]
            , pragCompleteD [name] (Just ''Acc)
            , pragCompleteD [name] (Just ''Exp)
            ]

        mkI :: Int -> Q [Dec]
        mkI n =
          let xs      = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              ts      = map varT xs
              name    = mkName ('I':show n)
              ix      = mkName "Ix"
              cst     = tupT (map (\t -> [t| Elt $t |]) ts)
              dim     = foldl (\h t -> [t| $h :. $t |]) [t| Z |] ts
              sig     = foldr (\t r -> [t| Exp $t -> $r |]) [t| Exp $dim |] ts
          in
          sequence
            [ patSynSigD name [t| $cst => $sig |]
            , patSynD    name (prefixPatSyn xs) implBidir (foldl (\ps p -> infixP ps ix (varP p)) [p| Z_ |] xs)
            , pragCompleteD [name] Nothing
            ]

        mkV :: Int -> Q [Dec]
        mkV n = do
          a <- newName "a"
          let xs    = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              ts    = replicate n (varT a)
              name  = mkName ('V':show n)
              sig   = foldr (\t r -> [t| Exp $t -> $r |]) [t| Exp (Vec $(litT (numTyLit (fromIntegral n))) $(varT a)) |] ts
          --
          sequence
            [ patSynSigD name [t| VecElt $(varT a) => $sig |]
            , patSynD    name (prefixPatSyn xs) implBidir [p| Pattern (VecPattern $(tupP (map varP xs))) |]
            , pragCompleteD [name] Nothing
            ]
    --
    ts <- mapM mkT [2..16]
    is <- mapM mkI [0..9]
    vs <- mapM mkV [2,3,4,8,16]
    return $ concat (ts ++ is ++ vs)
 )

