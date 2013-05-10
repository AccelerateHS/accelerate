{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Algebra
-- Copyright   : [2012..2013] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Algebraic simplifications of scalar expressions, including constant folding
-- and using algebraic properties of particular operator-operand combinations.
--

module Data.Array.Accelerate.Trafo.Algebra (

  evalPrimApp

) where

import Prelude                                          hiding ( exp )
import Data.Maybe                                       ( fromMaybe )
import Data.Bits
import Data.Char
import Text.PrettyPrint
import qualified Prelude                                as P

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Pretty.Print               ( prettyPrim )
import Data.Array.Accelerate.Array.Sugar                ( Elt, toElt, fromElt )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo.Base

import qualified Data.Array.Accelerate.Debug            as Stats


-- Propagate constant expressions, which are either constant valued expressions
-- or constant let bindings. Be careful not to follow self-cycles.
--
propagate
    :: forall acc env aenv exp. Kit acc
    => Gamma acc env env aenv
    -> PreOpenExp acc env aenv exp
    -> Maybe exp
propagate env = cvtE
  where
    cvtE :: PreOpenExp acc env aenv e -> Maybe e
    cvtE exp = case exp of
      Const c                                   -> Just (toElt c)
      PrimConst c                               -> Just (evalPrimConst c)
      Prj ix (Var v) | Tuple t <- prjExp v env  -> cvtT ix t
      Prj ix e       | Just c  <- cvtE e        -> cvtP ix (fromTuple c)
      Var ix
        | e             <- prjExp ix env
        , Nothing       <- match exp e  -> cvtE e
      --
      _                                 -> Nothing

    cvtP :: TupleIdx t e -> t -> Maybe e
    cvtP ZeroTupIdx       (_, v)   = Just v
    cvtP (SuccTupIdx idx) (tup, _) = cvtP idx tup

    cvtT :: TupleIdx t e -> Tuple (PreOpenExp acc env aenv) t -> Maybe e
    cvtT ZeroTupIdx       (SnocTup _   e) = cvtE e
    cvtT (SuccTupIdx idx) (SnocTup tup _) = cvtT idx tup
    cvtT _                _               = error "hey what's the head angle on that thing?"


-- Attempt to evaluate primitive function applications
--
evalPrimApp
    :: forall acc env aenv a r. (Kit acc, Elt a, Elt r)
    => Gamma acc env env aenv
    -> PrimFun (a -> r)
    -> PreOpenExp acc env aenv a
    -> PreOpenExp acc env aenv r
evalPrimApp env f x
  -- First attempt to move constant values towards the left
  | Just r      <- commutes f x env     = evalPrimApp env f r
--  | Just r      <- associates f x       = r

  -- Now attempt to evaluate any expressions
  | otherwise
  = fromMaybe (PrimApp f x)
  $ case f of
      PrimAdd ty                -> evalAdd ty x env
      PrimSub ty                -> evalSub ty x env
      PrimMul ty                -> evalMul ty x env
      PrimNeg ty                -> evalNeg ty x env
      PrimAbs ty                -> evalAbs ty x env
      PrimSig ty                -> evalSig ty x env
      PrimQuot ty               -> evalQuot ty x env
      PrimRem ty                -> evalRem ty x env
      PrimIDiv ty               -> evalIDiv ty x env
      PrimMod ty                -> evalMod ty x env
      PrimBAnd ty               -> evalBAnd ty x env
      PrimBOr ty                -> evalBOr ty x env
      PrimBXor ty               -> evalBXor ty x env
      PrimBNot ty               -> evalBNot ty x env
      PrimBShiftL ty            -> evalBShiftL ty x env
      PrimBShiftR ty            -> evalBShiftR ty x env
      PrimBRotateL ty           -> evalBRotateL ty x env
      PrimBRotateR ty           -> evalBRotateR ty x env
      PrimFDiv ty               -> evalFDiv ty x env
      PrimRecip ty              -> evalRecip ty x env
      PrimSin ty                -> evalSin ty x env
      PrimCos ty                -> evalCos ty x env
      PrimTan ty                -> evalTan ty x env
      PrimAsin ty               -> evalAsin ty x env
      PrimAcos ty               -> evalAcos ty x env
      PrimAtan ty               -> evalAtan ty x env
      PrimAsinh ty              -> evalAsinh ty x env
      PrimAcosh ty              -> evalAcosh ty x env
      PrimAtanh ty              -> evalAtanh ty x env
      PrimExpFloating ty        -> evalExpFloating ty x env
      PrimSqrt ty               -> evalSqrt ty x env
      PrimLog ty                -> evalLog ty x env
      PrimFPow ty               -> evalFPow ty x env
      PrimLogBase ty            -> evalLogBase ty x env
      PrimAtan2 ty              -> evalAtan2 ty x env
      PrimTruncate ta tb        -> evalTruncate ta tb x env
      PrimRound ta tb           -> evalRound ta tb x env
      PrimFloor ta tb           -> evalFloor ta tb x env
      PrimCeiling ta tb         -> evalCeiling ta tb x env
      PrimLt ty                 -> evalLt ty x env
      PrimGt ty                 -> evalGt ty x env
      PrimLtEq ty               -> evalLtEq ty x env
      PrimGtEq ty               -> evalGtEq ty x env
      PrimEq ty                 -> evalEq ty x env
      PrimNEq ty                -> evalNEq ty x env
      PrimMax ty                -> evalMax ty x env
      PrimMin ty                -> evalMin ty x env
      PrimLAnd                  -> evalLAnd x env
      PrimLOr                   -> evalLOr x env
      PrimLNot                  -> evalLNot x env
      PrimOrd                   -> evalOrd x env
      PrimChr                   -> evalChr x env
      PrimBoolToInt             -> evalBoolToInt x env
      PrimFromIntegral ta tb    -> evalFromIntegral ta tb x env


-- Discriminate binary functions that commute, and if so return the operands in
-- a stable ordering. If only one of the arguments is a constant, this is placed
-- to the left of the operator. Returning Nothing indicates no change is made.
--
commutes
    :: forall acc env aenv a r. (Kit acc, Elt a, Elt r)
    => PrimFun (a -> r)
    -> PreOpenExp acc env aenv a
    -> Gamma acc env env aenv
    -> Maybe (PreOpenExp acc env aenv a)
commutes f x env = case f of
  PrimAdd _     -> swizzle x
  PrimMul _     -> swizzle x
  PrimBAnd _    -> swizzle x
  PrimBOr _     -> swizzle x
  PrimBXor _    -> swizzle x
  PrimEq _      -> swizzle x
  PrimNEq _     -> swizzle x
  PrimMax _     -> swizzle x
  PrimMin _     -> swizzle x
  PrimLAnd      -> swizzle x
  PrimLOr       -> swizzle x
  _             -> Nothing
  where
    swizzle :: PreOpenExp acc env aenv (b,b) -> Maybe (PreOpenExp acc env aenv (b,b))
    swizzle (Tuple (NilTup `SnocTup` a `SnocTup` b))
      | Nothing         <- propagate env a
      , Just _          <- propagate env b
      = Stats.ruleFired (pprFun "commutes" f)
      $ Just $ Tuple (NilTup `SnocTup` b `SnocTup` a)

--    TLM: changing the ordering here when neither term can be reduced can be
--         disadvantageous: for example in (x &&* y), the user might have put a
--         simpler condition first that is designed to fail fast.
--
--      | Nothing         <- propagate env a
--      , Nothing         <- propagate env b
--      , hashOpenExp a > hashOpenExp b
--      = Just $ Tuple (NilTup `SnocTup` b `SnocTup` a)

    swizzle _
      = Nothing


{--
-- Determine if successive applications of a binary operator will associate, and
-- if so move them to the left. That is:
--
--   a + (b + c)  -->  (a + b) + c
--
-- Returning Nothing indicates no change is made.
--
-- TLM: we might get into trouble here, as we've lost track of where the user
--      has explicitly put parenthesis.
--
-- TLM: BROKEN!! does not correctly change the sign of expressions when flipping
--      (-x+y) or (-y+x).
--
associates
    :: (Elt a, Elt r)
    => PrimFun (a -> r)
    -> PreOpenExp acc env aenv a
    -> Maybe (PreOpenExp acc env aenv r)
associates fun exp = case fun of
  PrimAdd _     -> swizzle fun exp [PrimAdd ty, PrimSub ty]
  PrimSub _     -> swizzle fun exp [PrimAdd ty, PrimSub ty]
  PrimLAnd      -> swizzle fun exp [fun]
  PrimLOr       -> swizzle fun exp [fun]
  _             -> swizzle fun exp [fun]
  where
    -- TODO: check the list of ops is complete (and correct)
    ty  = undefined
    ops = [ PrimMul ty, PrimFDiv ty, PrimAdd ty, PrimSub ty, PrimBAnd ty, PrimBOr ty, PrimBXor ty ]

    swizzle :: (Elt a, Elt r) => PrimFun (a -> r) -> PreOpenExp acc env aenv a -> [PrimFun (a -> r)] -> Maybe (PreOpenExp acc env aenv r)
    swizzle f x lvl
      | Just REFL       <- matches f ops
      , Just (a,bc)     <- untup2 x
      , PrimApp g y     <- bc
      , Just REFL       <- matches g lvl
      , Just (b,c)      <- untup2 y
      = Stats.ruleFired (pprFun "associates" f)
      $ Just $ PrimApp g (tup2 (PrimApp f (tup2 (a,b)), c))

    swizzle _ _ _
      = Nothing

    matches :: (Elt s, Elt t) => PrimFun (s -> a) -> [PrimFun (t -> a)] -> Maybe (s :=: t)
    matches _ []        = Nothing
    matches f (x:xs)
      | Just REFL       <- matchPrimFun' f x
      = Just REFL

      | otherwise
      = matches f xs
--}


-- Helper functions
-- ----------------

type a :-> b = forall acc env aenv. Kit acc => PreOpenExp acc env aenv a -> Gamma acc env env aenv -> Maybe (PreOpenExp acc env aenv b)

eval1 :: Elt b => (a -> b) -> a :-> b
eval1 f x env
  | Just a <- propagate env x   = Stats.substitution "constant fold" . Just $ Const (fromElt (f a))
  | otherwise                   = Nothing

eval2 :: Elt c => (a -> b -> c) -> (a,b) :-> c
eval2 f (untup2 -> Just (x,y)) env
  | Just a <- propagate env x
  , Just b <- propagate env y
  = Stats.substitution "constant fold"
  $ Just $ Const (fromElt (f a b))

eval2 _ _ _
  = Nothing

-- tup2 :: (Elt a, Elt b) => (PreOpenExp acc env aenv a, PreOpenExp acc env aenv b) -> PreOpenExp acc env aenv (a, b)
-- tup2 (a,b) = Tuple (NilTup `SnocTup` a `SnocTup` b)

untup2 :: PreOpenExp acc env aenv (a, b) -> Maybe (PreOpenExp acc env aenv a, PreOpenExp acc env aenv b)
untup2 exp
  | Tuple (NilTup `SnocTup` a `SnocTup` b) <- exp   = Just (a, b)
  | otherwise                                       = Nothing


pprFun :: String -> PrimFun f -> String
pprFun rule f = show $ text rule <+> snd (prettyPrim f)


-- Methods of Num
-- --------------

evalAdd :: Elt a => NumType a -> (a,a) :-> a
evalAdd (IntegralNumType ty) | IntegralDict <- integralDict ty = evalAdd'
evalAdd (FloatingNumType ty) | FloatingDict <- floatingDict ty = evalAdd'

evalAdd' :: (Elt a, Eq a, Num a) => (a,a) :-> a
evalAdd' (untup2 -> Just (x,y)) env
  | Just a      <- propagate env x
  , a == 0
  = Stats.ruleFired "x+0" $ Just y

evalAdd' arg env
  = eval2 (+) arg env


evalSub :: Elt a => NumType a -> (a,a) :-> a
evalSub ty@(IntegralNumType ty') | IntegralDict <- integralDict ty' = evalSub' ty
evalSub ty@(FloatingNumType ty') | FloatingDict <- floatingDict ty' = evalSub' ty

evalSub' :: forall a. (Elt a, Eq a, Num a) => NumType a -> (a,a) :-> a
evalSub' ty (untup2 -> Just (x,y)) env
  | Just b      <- propagate env y
  , b == 0
  = Stats.ruleFired "x-0" $ Just x

  | Nothing     <- propagate env x
  , Just b      <- propagate env y
  = Stats.ruleFired "-y+x"
  $ Just $ evalPrimApp env (PrimAdd ty) (Tuple $ NilTup `SnocTup` Const (fromElt (-b)) `SnocTup` x)

  | Just REFL   <- match x y
  = Stats.ruleFired "x-x"
  $ Just $ Const (fromElt (0::a))

evalSub' _ arg env
  = eval2 (-) arg env


evalMul :: Elt a => NumType a -> (a,a) :-> a
evalMul (IntegralNumType ty) | IntegralDict <- integralDict ty = evalMul'
evalMul (FloatingNumType ty) | FloatingDict <- floatingDict ty = evalMul'

evalMul' :: (Elt a, Eq a, Num a) => (a,a) :-> a
evalMul' (untup2 -> Just (x,y)) env
  | Just a      <- propagate env x
  , Nothing     <- propagate env y
  = case a of
      0         -> Stats.ruleFired "x*0" $ Just x
      1         -> Stats.ruleFired "x*1" $ Just y
      _         -> Nothing

evalMul' arg env
  = eval2 (*) arg env

evalNeg :: Elt a => NumType a -> a :-> a
evalNeg (IntegralNumType ty) | IntegralDict <- integralDict ty = eval1 negate
evalNeg (FloatingNumType ty) | FloatingDict <- floatingDict ty = eval1 negate

evalAbs :: Elt a => NumType a -> a :-> a
evalAbs (IntegralNumType ty) | IntegralDict <- integralDict ty = eval1 abs
evalAbs (FloatingNumType ty) | FloatingDict <- floatingDict ty = eval1 abs

evalSig :: Elt a => NumType a -> a :-> a
evalSig (IntegralNumType ty) | IntegralDict <- integralDict ty = eval1 signum
evalSig (FloatingNumType ty) | FloatingDict <- floatingDict ty = eval1 signum


-- Methods of Integral & Bits
-- --------------------------

evalQuot :: Elt a => IntegralType a -> (a,a) :-> a
evalQuot ty | IntegralDict <- integralDict ty = eval2 quot

evalRem :: Elt a => IntegralType a -> (a,a) :-> a
evalRem ty | IntegralDict <- integralDict ty = eval2 rem

evalIDiv :: Elt a => IntegralType a -> (a,a) :-> a
evalIDiv ty | IntegralDict <- integralDict ty = evalIDiv'

evalIDiv' :: (Elt a, Integral a, Eq a) => (a,a) :-> a
evalIDiv' (untup2 -> Just (x,y)) env
  | Just 1      <- propagate env y
  = Stats.ruleFired "x`div`1" $ Just x

evalIDiv' arg env
  = eval2 div arg env

evalMod :: Elt a => IntegralType a -> (a,a) :-> a
evalMod ty | IntegralDict <- integralDict ty = eval2 mod

evalBAnd :: Elt a => IntegralType a -> (a,a) :-> a
evalBAnd ty | IntegralDict <- integralDict ty = eval2 (.&.)

evalBOr :: Elt a => IntegralType a -> (a,a) :-> a
evalBOr ty | IntegralDict <- integralDict ty = eval2 (.|.)

evalBXor :: Elt a => IntegralType a -> (a,a) :-> a
evalBXor ty | IntegralDict <- integralDict ty = eval2 xor

evalBNot :: Elt a => IntegralType a -> a :-> a
evalBNot ty | IntegralDict <- integralDict ty = eval1 complement

evalBShiftL :: Elt a => IntegralType a -> (a,Int) :-> a
evalBShiftL _ (untup2 -> Just (x,i)) env
  | Just 0 <- propagate env i
  = Stats.ruleFired "x `shiftL` 0" $ Just x

evalBShiftL ty arg env
  | IntegralDict <- integralDict ty = eval2 shiftL arg env

evalBShiftR :: Elt a => IntegralType a -> (a,Int) :-> a
evalBShiftR _ (untup2 -> Just (x,i)) env
  | Just 0 <- propagate env i
  = Stats.ruleFired "x `shiftR` 0" $ Just x

evalBShiftR ty arg env
  | IntegralDict <- integralDict ty = eval2 shiftR arg env

evalBRotateL :: Elt a => IntegralType a -> (a,Int) :-> a
evalBRotateL _ (untup2 -> Just (x,i)) env
  | Just 0 <- propagate env i
  = Stats.ruleFired "x `rotateL` 0" $ Just x
evalBRotateL ty arg env
  | IntegralDict <- integralDict ty = eval2 rotateL arg env

evalBRotateR :: Elt a => IntegralType a -> (a,Int) :-> a
evalBRotateR _ (untup2 -> Just (x,i)) env
  | Just 0 <- propagate env i
  = Stats.ruleFired "x `rotateR` 0" $ Just x
evalBRotateR ty arg env
  | IntegralDict <- integralDict ty = eval2 rotateR arg env


-- Methods of Fractional & Floating
-- --------------------------------

evalFDiv :: Elt a => FloatingType a -> (a,a) :-> a
evalFDiv ty | FloatingDict <- floatingDict ty = evalFDiv'

evalFDiv' :: (Elt a, Fractional a, Eq a) => (a,a) :-> a
evalFDiv' (untup2 -> Just (x,y)) env
  | Just 1      <- propagate env y
  = Stats.ruleFired "x/1" $ Just x

evalFDiv' arg env
  = eval2 (/) arg env


evalRecip :: Elt a => FloatingType a -> a :-> a
evalRecip ty | FloatingDict <- floatingDict ty = eval1 recip

evalSin :: Elt a => FloatingType a -> a :-> a
evalSin ty | FloatingDict <- floatingDict ty = eval1 sin

evalCos :: Elt a => FloatingType a -> a :-> a
evalCos ty | FloatingDict <- floatingDict ty = eval1 cos

evalTan :: Elt a => FloatingType a -> a :-> a
evalTan ty | FloatingDict <- floatingDict ty = eval1 tan

evalAsin :: Elt a => FloatingType a -> a :-> a
evalAsin ty | FloatingDict <- floatingDict ty = eval1 asin

evalAcos :: Elt a => FloatingType a -> a :-> a
evalAcos ty | FloatingDict <- floatingDict ty = eval1 acos

evalAtan :: Elt a => FloatingType a -> a :-> a
evalAtan ty | FloatingDict <- floatingDict ty = eval1 atan

evalAsinh :: Elt a => FloatingType a -> a :-> a
evalAsinh ty | FloatingDict <- floatingDict ty = eval1 asinh

evalAcosh :: Elt a => FloatingType a -> a :-> a
evalAcosh ty | FloatingDict <- floatingDict ty = eval1 acosh

evalAtanh :: Elt a => FloatingType a -> a :-> a
evalAtanh ty | FloatingDict <- floatingDict ty = eval1 atanh

evalExpFloating :: Elt a => FloatingType a -> a :-> a
evalExpFloating ty | FloatingDict <- floatingDict ty = eval1 P.exp

evalSqrt :: Elt a => FloatingType a -> a :-> a
evalSqrt ty | FloatingDict <- floatingDict ty = eval1 sqrt

evalLog :: Elt a => FloatingType a -> a :-> a
evalLog ty | FloatingDict <- floatingDict ty = eval1 log

evalFPow :: Elt a => FloatingType a -> (a,a) :-> a
evalFPow ty | FloatingDict <- floatingDict ty = eval2 (**)

evalLogBase :: Elt a => FloatingType a -> (a,a) :-> a
evalLogBase ty | FloatingDict <- floatingDict ty = eval2 logBase

evalAtan2 :: Elt a => FloatingType a -> (a,a) :-> a
evalAtan2 ty | FloatingDict <- floatingDict ty = eval2 atan2

evalTruncate :: (Elt a, Elt b) => FloatingType a -> IntegralType b -> a :-> b
evalTruncate ta tb
  | FloatingDict <- floatingDict ta
  , IntegralDict <- integralDict tb = eval1 truncate

evalRound :: (Elt a, Elt b) => FloatingType a -> IntegralType b -> a :-> b
evalRound ta tb
  | FloatingDict <- floatingDict ta
  , IntegralDict <- integralDict tb = eval1 round

evalFloor :: (Elt a, Elt b) => FloatingType a -> IntegralType b -> a :-> b
evalFloor ta tb
  | FloatingDict <- floatingDict ta
  , IntegralDict <- integralDict tb = eval1 floor

evalCeiling :: (Elt a, Elt b) => FloatingType a -> IntegralType b -> a :-> b
evalCeiling ta tb
  | FloatingDict <- floatingDict ta
  , IntegralDict <- integralDict tb = eval1 ceiling


-- Relational & Equality
-- ---------------------

evalLt :: ScalarType a -> (a,a) :-> Bool
evalLt (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = eval2 (<)
evalLt (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = eval2 (<)
evalLt (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = eval2 (<)

evalGt :: ScalarType a -> (a,a) :-> Bool
evalGt (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = eval2 (>)
evalGt (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = eval2 (>)
evalGt (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = eval2 (>)

evalLtEq :: ScalarType a -> (a,a) :-> Bool
evalLtEq (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = eval2 (<=)
evalLtEq (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = eval2 (<=)
evalLtEq (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = eval2 (<=)

evalGtEq :: ScalarType a -> (a,a) :-> Bool
evalGtEq (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = eval2 (>=)
evalGtEq (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = eval2 (>=)
evalGtEq (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = eval2 (>=)

evalEq :: ScalarType a -> (a,a) :-> Bool
evalEq (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = eval2 (==)
evalEq (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = eval2 (==)
evalEq (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = eval2 (==)

evalNEq :: ScalarType a -> (a,a) :-> Bool
evalNEq (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = eval2 (/=)
evalNEq (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = eval2 (/=)
evalNEq (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = eval2 (/=)

evalMax :: Elt a => ScalarType a -> (a,a) :-> a
evalMax (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = eval2 max
evalMax (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = eval2 max
evalMax (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = eval2 max

evalMin :: Elt a => ScalarType a -> (a,a) :-> a
evalMin (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = eval2 min
evalMin (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = eval2 min
evalMin (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = eval2 min


-- Logical operators
-- -----------------

evalLAnd :: (Bool,Bool) :-> Bool
evalLAnd (untup2 -> Just (x,y)) env
  | Just a      <- propagate env x
  = Just $ if a then Stats.ruleFired "True &&" y
                else Stats.ruleFired "False &&" $ Const (fromElt False)

evalLAnd _ _
  = Nothing

evalLOr  :: (Bool,Bool) :-> Bool
evalLOr (untup2 -> Just (x,y)) env
  | Just a      <- propagate env x
  = Just $ if a then Stats.ruleFired "True ||" $ Const (fromElt True)
                else Stats.ruleFired "False ||" y

evalLOr _ _
  = Nothing

evalLNot :: Bool :-> Bool
evalLNot = eval1 not

evalOrd :: Char :-> Int
evalOrd = eval1 ord

evalChr :: Int :-> Char
evalChr = eval1 chr

evalBoolToInt :: Bool :-> Int
evalBoolToInt = eval1 fromEnum

evalFromIntegral :: Elt b => IntegralType a -> NumType b -> a :-> b
evalFromIntegral ta (IntegralNumType tb)
  | IntegralDict <- integralDict ta
  , IntegralDict <- integralDict tb = eval1 fromIntegral

evalFromIntegral ta (FloatingNumType tb)
  | IntegralDict <- integralDict ta
  , FloatingDict <- floatingDict tb = eval1 fromIntegral


-- Scalar primitives
-- -----------------

evalPrimConst :: PrimConst a -> a
evalPrimConst (PrimMinBound ty) = evalMinBound ty
evalPrimConst (PrimMaxBound ty) = evalMaxBound ty
evalPrimConst (PrimPi       ty) = evalPi ty

evalMinBound :: BoundedType a -> a
evalMinBound (IntegralBoundedType ty) | IntegralDict <- integralDict ty = minBound
evalMinBound (NonNumBoundedType   ty) | NonNumDict   <- nonNumDict ty   = minBound

evalMaxBound :: BoundedType a -> a
evalMaxBound (IntegralBoundedType ty) | IntegralDict <- integralDict ty = maxBound
evalMaxBound (NonNumBoundedType   ty) | NonNumDict   <- nonNumDict ty   = maxBound

evalPi :: FloatingType a -> a
evalPi ty | FloatingDict <- floatingDict ty = pi

