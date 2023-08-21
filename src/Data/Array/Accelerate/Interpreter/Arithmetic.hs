{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Interpreter.Arithmetic
-- Copyright   : [2008..2022] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Interpreter.Arithmetic (

  add, sub, mul, negate, abs, signum, vadd, vmul,
  quot, rem, quotRem, div, mod, divMod,
  band, bor, xor, complement, shiftL, shiftR, rotateL, rotateR, popCount, countLeadingZeros, countTrailingZeros, bitreverse, byteswap, vband, vbor, vbxor,
  fdiv, recip, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh, exp, sqrt, log, pow, logBase,
  truncate, round, floor, ceiling,
  atan2, isNaN, isInfinite,
  lt, gt, lte, gte, eq, neq, min, max, vmin, vmax,
  land, lor, lnot, vland, vlor,
  fromIntegral, toFloating, toBool, fromBool,

) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type

import Data.Primitive.Bit                                           ( BitMask(..) )
import Data.Primitive.Vec                                           ( Vec )
import qualified Data.Primitive.Bit                                 as Bit
import qualified Data.Primitive.Vec                                 as Vec

import Data.Primitive.Types

import Data.Bits                                                    ( (.&.), (.|.) )
import Data.Bool
import Data.Maybe
import Data.Type.Equality
import Formatting
import Prelude                                                      ( ($), (.) )
import qualified Data.Bits                                          as P
import qualified Prelude                                            as P

import GHC.Int
import GHC.Word
import GHC.Exts
import GHC.TypeLits
import GHC.TypeLits.Extra


-- Operators from Num
-- ------------------

add :: NumType a -> ((a, a) -> a)
add = num2 (P.+)

sub :: NumType a -> ((a, a) -> a)
sub = num2 (P.-)

mul :: NumType a -> ((a, a) -> a)
mul = num2 (P.*)

negate :: NumType a -> (a -> a)
negate = num1 P.negate

abs :: NumType a -> (a -> a)
abs = num1 P.abs

signum :: NumType a -> (a -> a)
signum = num1 P.signum

vadd :: NumType (Vec n a) -> (Vec n a -> a)
vadd = vnum2 (P.+)

vmul :: NumType (Vec n a) -> (Vec n a -> a)
vmul = vnum2 (P.*)

num1 :: (forall t. P.Num t => t -> t) -> NumType a -> (a -> a)
num1 f = \case
  IntegralNumType t -> integral t
  FloatingNumType t -> floating t
  where
    integral :: IntegralType t -> (t -> t)
    integral (SingleIntegralType   t) | IntegralDict <- integralDict t = f
    integral (VectorIntegralType _ t) | IntegralDict <- integralDict t = map f
    --
    floating :: FloatingType t -> (t -> t)
    floating (SingleFloatingType   t) | FloatingDict <- floatingDict t = f
    floating (VectorFloatingType _ t) | FloatingDict <- floatingDict t = map f

num2 :: (forall t. P.Num t => t -> t -> t) -> NumType a -> ((a, a) -> a)
num2 f = \case
  IntegralNumType t -> integral t
  FloatingNumType t -> floating t
  where
    integral :: IntegralType t -> ((t, t) -> t)
    integral (SingleIntegralType   t) | IntegralDict <- integralDict t = P.uncurry f
    integral (VectorIntegralType _ t) | IntegralDict <- integralDict t = P.uncurry (zipWith f)
    --
    floating :: FloatingType t -> ((t, t) -> t)
    floating (SingleFloatingType   t) | FloatingDict <- floatingDict t = P.uncurry f
    floating (VectorFloatingType _ t) | FloatingDict <- floatingDict t = P.uncurry (zipWith f)

vnum2 :: (forall t. P.Num t => t -> t -> t) -> NumType (Vec n a) -> (Vec n a -> a)
vnum2 f = \case
  IntegralNumType t -> integral t
  FloatingNumType t -> floating t
  where
    integral :: IntegralType (Vec n t) -> (Vec n t -> t)
    integral = \case
      SingleIntegralType   t                                  -> case t of
      VectorIntegralType _ t | IntegralDict <- integralDict t -> P.foldl1 f . Vec.toList

    floating :: FloatingType (Vec n t) -> (Vec n t -> t)
    floating = \case
      SingleFloatingType   t                                  -> case t of
      VectorFloatingType _ t | FloatingDict <- floatingDict t -> P.foldl1 f . Vec.toList

-- Operators from Integral
-- -----------------------

quot :: IntegralType a -> ((a, a) -> a)
quot = int2 P.quot

rem :: IntegralType a -> ((a, a) -> a)
rem = int2 P.rem

quotRem :: IntegralType a -> ((a, a) -> (a, a))
quotRem = int2' P.quotRem

div :: IntegralType a -> ((a, a) -> a)
div = int2 P.div

mod :: IntegralType a -> ((a, a) -> a)
mod = int2 P.mod

divMod :: IntegralType a -> ((a, a) -> (a, a))
divMod = int2' P.divMod

int2 :: (forall t. P.Integral t => t -> t -> t) -> IntegralType a -> ((a, a) -> a)
int2 f (SingleIntegralType   t) | IntegralDict <- integralDict t = P.uncurry f
int2 f (VectorIntegralType _ t) | IntegralDict <- integralDict t = P.uncurry (zipWith f)

int2' :: (forall t. P.Integral t => t -> t -> (t, t)) -> IntegralType a -> ((a, a) -> (a, a))
int2' f (SingleIntegralType   t) | IntegralDict <- integralDict t = P.uncurry f
int2' f (VectorIntegralType _ t) | IntegralDict <- integralDict t = P.uncurry (zipWith' f)


-- Operators from Bits & FiniteBits
-- --------------------------------

band :: IntegralType a -> ((a, a) -> a)
band = bits2 (.&.)

bor :: IntegralType a -> ((a, a) -> a)
bor = bits2 (.|.)

xor :: IntegralType a -> ((a, a) -> a)
xor = bits2 P.xor

complement :: IntegralType a -> (a -> a)
complement = bits1 P.complement

shiftL :: IntegralType a -> ((a, a) -> a)
shiftL = bits2 (\x i -> x `P.shiftL` P.fromIntegral i)

shiftR :: IntegralType a -> ((a, a) -> a)
shiftR = bits2 (\x i -> x `P.shiftR` P.fromIntegral i)

rotateL :: IntegralType a -> ((a, a) -> a)
rotateL = bits2 (\x i -> x `P.rotateL` P.fromIntegral i)

rotateR :: IntegralType a -> ((a, a) -> a)
rotateR = bits2 (\x i -> x `P.rotateR` P.fromIntegral i)

popCount :: IntegralType a -> (a -> a)
popCount = bits1 (P.fromIntegral . P.popCount)

countLeadingZeros :: IntegralType a -> (a -> a)
countLeadingZeros = bits1 (P.fromIntegral . P.countLeadingZeros)

countTrailingZeros :: IntegralType a -> (a -> a)
countTrailingZeros = bits1 (P.fromIntegral . P.countTrailingZeros)

bitreverse :: IntegralType a -> (a -> a)
bitreverse = \case
  VectorIntegralType _ t | IntegralDict <- integralDict t -> Vec.fromList . P.map (bitreverse (SingleIntegralType t)) . Vec.toList
  SingleIntegralType   t                                  -> single t
  where
    single :: SingleIntegralType t -> t -> t
    single TypeInt8    (I8#  i#)     = I8#  (word8ToInt8#   (wordToWord8#  (bitReverse8#  (word8ToWord#  (int8ToWord8# i#)))))
    single TypeInt16   (I16# i#)     = I16# (word16ToInt16# (wordToWord16# (bitReverse16# (word16ToWord# (int16ToWord16# i#)))))
    single TypeInt32   (I32# i#)     = I32# (word32ToInt32# (wordToWord32# (bitReverse32# (word32ToWord# (int32ToWord32# i#)))))
    single TypeInt64   (I64# i#)     = I64# (word64ToInt64# (bitReverse64# (int64ToWord64# i#)))
    single TypeInt128  (Int128 h l)  = Int128 (bitreverse integralType l) (bitreverse integralType h)
    single TypeWord8   (W8#  w#)     = W8#  (wordToWord8#  (bitReverse8#  (word8ToWord#  w#)))
    single TypeWord16  (W16# w#)     = W16# (wordToWord16# (bitReverse16# (word16ToWord# w#)))
    single TypeWord32  (W32# w#)     = W32# (wordToWord32# (bitReverse32# (word32ToWord# w#)))
    single TypeWord64  (W64# w#)     = W64# (bitReverse64# w#)
    single TypeWord128 (Word128 h l) = Word128 (bitreverse integralType l) (bitreverse integralType h)

byteswap :: IntegralType a -> (a -> a)
byteswap = \case
  VectorIntegralType _ t | IntegralDict <- integralDict t -> Vec.fromList . P.map (byteswap (SingleIntegralType t)) . Vec.toList
  SingleIntegralType   t                                  -> single t
  where
    single :: SingleIntegralType t -> t -> t
    single TypeInt8    x             = x
    single TypeInt16   (I16# i#)     = I16# (word16ToInt16# (wordToWord16# (byteSwap16# (word16ToWord# (int16ToWord16# i#)))))
    single TypeInt32   (I32# i#)     = I32# (word32ToInt32# (wordToWord32# (byteSwap32# (word32ToWord# (int32ToWord32# i#)))))
    single TypeInt64   (I64# i#)     = I64# (word64ToInt64# (byteSwap64# (int64ToWord64# i#)))
    single TypeInt128  (Int128 h l)  = Int128 (byteswap integralType l) (byteswap integralType h)
    single TypeWord8   x             = x
    single TypeWord16  (W16# w#)     = W16# (wordToWord16# (byteSwap16# (word16ToWord# w#)))
    single TypeWord32  (W32# w#)     = W32# (wordToWord32# (byteSwap32# (word32ToWord# w#)))
    single TypeWord64  (W64# w#)     = W64# (byteSwap64# w#)
    single TypeWord128 (Word128 h l) = Word128 (byteswap integralType l) (byteswap integralType h)

vband :: IntegralType (Vec n a) -> (Vec n a -> a)
vband = vbits2 (.&.)

vbor :: IntegralType (Vec n a) -> (Vec n a -> a)
vbor = vbits2 (.|.)

vbxor :: IntegralType (Vec n a) -> (Vec n a -> a)
vbxor = vbits2 P.xor

bits1 :: (forall t. (P.Integral t, P.FiniteBits t) => t -> t) -> IntegralType a -> (a -> a)
bits1 f (SingleIntegralType   t) | IntegralDict <- integralDict t = f
bits1 f (VectorIntegralType _ t) | IntegralDict <- integralDict t = map f

bits2 :: (forall t. (P.Integral t, P.FiniteBits t) => t -> t -> t) -> IntegralType a -> ((a, a) -> a)
bits2 f (SingleIntegralType   t) | IntegralDict <- integralDict t = P.uncurry f
bits2 f (VectorIntegralType _ t) | IntegralDict <- integralDict t = P.uncurry (zipWith f)

vbits2 :: (forall t. P.FiniteBits t => t -> t -> t) -> IntegralType (Vec n a) -> (Vec n a -> a)
vbits2 _ (SingleIntegralType   t)                                  = case t of
vbits2 f (VectorIntegralType _ t) | IntegralDict <- integralDict t = P.foldl1 f . Vec.toList


-- Operators from Fractional and Floating
-- --------------------------------------

fdiv :: FloatingType a -> ((a, a) -> a)
fdiv = float2 (P./)

recip :: FloatingType a -> (a -> a)
recip = float1 P.recip

sin :: FloatingType a -> (a -> a)
sin = float1 P.sin

cos :: FloatingType a -> (a -> a)
cos = float1 P.cos

tan :: FloatingType a -> (a -> a)
tan = float1 P.tan

asin :: FloatingType a -> (a -> a)
asin = float1 P.asin

acos :: FloatingType a -> (a -> a)
acos = float1 P.acos

atan :: FloatingType a -> (a -> a)
atan = float1 P.atan

sinh :: FloatingType a -> (a -> a)
sinh = float1 P.sinh

cosh :: FloatingType a -> (a -> a)
cosh = float1 P.cosh

tanh :: FloatingType a -> (a -> a)
tanh = float1 P.tanh

asinh :: FloatingType a -> (a -> a)
asinh = float1 P.asinh

acosh :: FloatingType a -> (a -> a)
acosh = float1 P.acosh

atanh :: FloatingType a -> (a -> a)
atanh = float1 P.atanh

exp :: FloatingType a -> (a -> a)
exp = float1 P.exp

sqrt :: FloatingType a -> (a -> a)
sqrt = float1 P.sqrt

log :: FloatingType a -> (a -> a)
log = float1 P.log

pow :: FloatingType a -> ((a, a) -> a)
pow = float2 (P.**)

logBase :: FloatingType a -> ((a, a) -> a)
logBase = float2 P.logBase


float1 :: (forall t. P.RealFloat t => t -> t) -> FloatingType a -> (a -> a)
float1 f (SingleFloatingType   t) | FloatingDict <- floatingDict t = f
float1 f (VectorFloatingType _ t) | FloatingDict <- floatingDict t = map f

float2 :: (forall t. P.RealFloat t => t -> t -> t) -> FloatingType a -> ((a, a) -> a)
float2 f (SingleFloatingType   t) | FloatingDict <- floatingDict t = P.uncurry f
float2 f (VectorFloatingType _ t) | FloatingDict <- floatingDict t = P.uncurry (zipWith f)


-- Operators from RealFrac
-- -----------------------

truncate :: FloatingType a -> IntegralType b -> (a -> b)
truncate (SingleFloatingType a) (SingleIntegralType b)
  | FloatingDict <- floatingDict a
  , IntegralDict <- integralDict b
  = P.truncate
truncate (VectorFloatingType n a) (VectorIntegralType m b)
  | Just Refl    <- sameNat' n m
  , FloatingDict <- floatingDict a
  , IntegralDict <- integralDict b
  = map P.truncate
truncate a b
  = internalError ("truncate: cannot reconcile `" % formatFloatingType % "' with `" % formatIntegralType % "'") a b

round :: FloatingType a -> IntegralType b -> (a -> b)
round (SingleFloatingType a) (SingleIntegralType b)
  | FloatingDict <- floatingDict a
  , IntegralDict <- integralDict b
  = P.round
round (VectorFloatingType n a) (VectorIntegralType m b)
  | Just Refl    <- sameNat' n m
  , FloatingDict <- floatingDict a
  , IntegralDict <- integralDict b
  = map P.round
round a b
  = internalError ("round: cannot reconcile `" % formatFloatingType % "' with `" % formatIntegralType % "'") a b

floor :: FloatingType a -> IntegralType b -> (a -> b)
floor (SingleFloatingType a) (SingleIntegralType b)
  | FloatingDict <- floatingDict a
  , IntegralDict <- integralDict b
  = P.floor
floor (VectorFloatingType n a) (VectorIntegralType m b)
  | Just Refl    <- sameNat' n m
  , FloatingDict <- floatingDict a
  , IntegralDict <- integralDict b
  = map P.floor
floor a b
  = internalError ("floor: cannot reconcile `" % formatFloatingType % "' with `" % formatIntegralType % "'") a b

ceiling :: FloatingType a -> IntegralType b -> (a -> b)
ceiling (SingleFloatingType a) (SingleIntegralType b)
  | FloatingDict <- floatingDict a
  , IntegralDict <- integralDict b
  = P.ceiling
ceiling (VectorFloatingType n a) (VectorIntegralType m b)
  | Just Refl    <- sameNat' n m
  , FloatingDict <- floatingDict a
  , IntegralDict <- integralDict b
  = map P.ceiling
ceiling a b
  = internalError ("ceiling: cannot reconcile `" % formatFloatingType % "' with `" % formatIntegralType % "'") a b


-- Operators from RealFloat
-- ------------------------

atan2 :: FloatingType a -> ((a, a) -> a)
atan2 = float2 P.atan2

isNaN :: FloatingType a -> (a -> BitOrMask a)
isNaN = \case
  SingleFloatingType t   | FloatingDict <- floatingDict t -> isNaN'
  VectorFloatingType _ t | FloatingDict <- floatingDict t -> unMask . map isNaN'
  where
    isNaN' x = Bit (P.isNaN x)

isInfinite :: FloatingType a -> (a -> BitOrMask a)
isInfinite = \case
  SingleFloatingType t   | FloatingDict <- floatingDict t -> isInfinite'
  VectorFloatingType _ t | FloatingDict <- floatingDict t -> unMask . map isInfinite'
  where
    isInfinite' x = Bit (P.isInfinite x)


-- Operators from Eq & Ord
-- -----------------------

lt :: ScalarType a -> ((a, a) -> BitOrMask a)
lt = cmp (P.<)

gt :: ScalarType a -> ((a, a) -> BitOrMask a)
gt = cmp (P.>)

lte :: ScalarType a -> ((a, a) -> BitOrMask a)
lte = cmp (P.<=)

gte :: ScalarType a -> ((a, a) -> BitOrMask a)
gte = cmp (P.>=)

eq :: ScalarType a -> ((a, a) -> BitOrMask a)
eq = cmp (P.==)

neq :: ScalarType a -> ((a, a) -> BitOrMask a)
neq = cmp (P./=)

cmp :: (forall t. P.Ord t => (t -> t -> Bool)) -> ScalarType a -> ((a, a) -> BitOrMask a)
cmp f = \case
  NumScalarType t -> num t
  BitScalarType t -> bit t
  where
    bit :: BitType t -> ((t, t) -> BitOrMask t)
    bit TypeBit    = Bit . P.uncurry f
    bit TypeMask{} = \(x,y) -> unMask (zipWith (Bit $$ f) (BitMask x) (BitMask y))

    num :: NumType t -> ((t, t) -> BitOrMask t)
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType t -> ((t, t) -> BitOrMask t)
    integral (SingleIntegralType   t) | IntegralDict <- integralDict t = P.uncurry (Bit $$ f)
    integral (VectorIntegralType _ t) | IntegralDict <- integralDict t = P.uncurry (unMask $$ zipWith (Bit $$ f))

    floating :: FloatingType t -> ((t, t) -> BitOrMask t)
    floating (SingleFloatingType   t) | FloatingDict <- floatingDict t = P.uncurry (Bit $$ f)
    floating (VectorFloatingType _ t) | FloatingDict <- floatingDict t = P.uncurry (unMask $$ zipWith (Bit $$ f))

min :: ScalarType a -> ((a, a) -> a)
min = ord2 P.min

max :: ScalarType a -> ((a, a) -> a)
max = ord2 P.max

vmin :: ScalarType (Vec n a) -> (Vec n a -> a)
vmin = vord2 P.min

vmax :: ScalarType (Vec n a) -> (Vec n a -> a)
vmax = vord2 P.max

ord2 :: (forall t. P.Ord t => t -> t -> t) -> ScalarType a -> ((a, a) -> a)
ord2 f = \case
  NumScalarType t -> num t
  BitScalarType t -> bit t
  where
    bit :: BitType t -> ((t, t) -> t)
    bit TypeBit    = P.uncurry f
    bit TypeMask{} = \(x,y) -> unMask (zipWith f (BitMask x) (BitMask y))

    num :: NumType t -> ((t, t) -> t)
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType t -> ((t, t) -> t)
    integral (SingleIntegralType   t) | IntegralDict <- integralDict t = P.uncurry f
    integral (VectorIntegralType _ t) | IntegralDict <- integralDict t = P.uncurry (zipWith f)

    floating :: FloatingType t -> ((t, t) -> t)
    floating (SingleFloatingType   t) | FloatingDict <- floatingDict t = P.uncurry f
    floating (VectorFloatingType _ t) | FloatingDict <- floatingDict t = P.uncurry (zipWith f)


vord2 :: (forall t. P.Ord t => t -> t -> t) -> ScalarType (Vec n a) -> (Vec n a -> a)
vord2 f = \case
  NumScalarType t -> num t
  BitScalarType t -> bit t
  where
    bit :: BitType (Vec n t) -> (Vec n t -> t)
    bit TypeMask{} = P.foldl1 f . Bit.toList . BitMask

    num :: NumType (Vec n t) -> (Vec n t -> t)
    num = \case
      IntegralNumType t -> integral t
      FloatingNumType t -> floating t

    integral :: IntegralType (Vec n t) -> (Vec n t -> t)
    integral = \case
      SingleIntegralType   t                                  -> case t of
      VectorIntegralType _ t | IntegralDict <- integralDict t -> P.foldl1 f . Vec.toList

    floating :: FloatingType (Vec n t) -> (Vec n t -> t)
    floating = \case
      SingleFloatingType   t                                  -> case t of
      VectorFloatingType _ t | FloatingDict <- floatingDict t -> P.foldl1 f . Vec.toList

-- Logical operators
-- -----------------

land :: BitType a -> ((a, a) -> a)
land = \case
  TypeBit    -> P.uncurry land'
  TypeMask{} -> \(x,y) -> unMask (zipWith land' (BitMask x) (BitMask y))
  where
    land' (Bit x) (Bit y) = Bit (x && y)

lor :: BitType a -> ((a, a) -> a)
lor = \case
  TypeBit    -> P.uncurry lor'
  TypeMask{} -> \(x,y) -> unMask (zipWith lor' (BitMask x) (BitMask y))
  where
    lor' (Bit x) (Bit y) = Bit (x || y)

lnot :: BitType a -> (a -> a)
lnot = \case
  TypeBit    -> not'
  TypeMask{} -> unMask . map not' . BitMask
  where
    not' (Bit x) = Bit (not x)

vland :: BitType (Vec n a) -> (Vec n a -> a)
vland TypeMask{} x = Bit $ P.all unBit (toList (BitMask x))

vlor :: BitType (Vec n a) -> (Vec n a -> a)
vlor TypeMask{} x = Bit $ P.any unBit (toList (BitMask x))


-- Conversion
-- ----------

fromIntegral :: forall a b. IntegralType a -> NumType b -> (a -> b)
fromIntegral (SingleIntegralType a) (IntegralNumType (SingleIntegralType b))
  | IntegralDict <- integralDict a
  , IntegralDict <- integralDict b
  = P.fromIntegral
fromIntegral (SingleIntegralType a) (FloatingNumType (SingleFloatingType b))
  | IntegralDict <- integralDict a
  , FloatingDict <- floatingDict b
  = P.fromIntegral
fromIntegral (VectorIntegralType n a) (IntegralNumType (VectorIntegralType m b))
  | Just Refl    <- sameNat' n m
  , IntegralDict <- integralDict a
  , IntegralDict <- integralDict b
  = map P.fromIntegral
fromIntegral (VectorIntegralType n a) (FloatingNumType (VectorFloatingType m b))
  | Just Refl    <- sameNat' n m
  , IntegralDict <- integralDict a
  , FloatingDict <- floatingDict b
  = map P.fromIntegral
fromIntegral a b
  = internalError ("fromIntegral: cannot reconcile `" % formatIntegralType % "' with `" % formatNumType % "'") a b


toFloating :: forall a b. NumType a -> FloatingType b -> (a -> b)
toFloating (IntegralNumType (SingleIntegralType a)) (SingleFloatingType b)
  | IntegralDict <- integralDict a
  , FloatingDict <- floatingDict b
  = P.realToFrac
toFloating (FloatingNumType (SingleFloatingType a)) (SingleFloatingType b)
  | FloatingDict <- floatingDict a
  , FloatingDict <- floatingDict b
  = P.realToFrac
toFloating (IntegralNumType (VectorIntegralType n a)) (VectorFloatingType m b)
  | Just Refl    <- sameNat' n m
  , IntegralDict <- integralDict a
  , FloatingDict <- floatingDict b
  = map P.realToFrac
toFloating (FloatingNumType (VectorFloatingType n a)) (VectorFloatingType m b)
  | Just Refl    <- sameNat' n m
  , FloatingDict <- floatingDict a
  , FloatingDict <- floatingDict b
  = map P.realToFrac
toFloating a b
  = internalError ("toFloating: cannot reconcile `" % formatNumType % "' with `" % formatFloatingType % "'") a b


toBool :: IntegralType a -> BitType b -> (a -> b)
toBool iR bR =
  case iR of
    SingleIntegralType t | IntegralDict <- integralDict t ->
      case bR of
        TypeBit    -> P.fromIntegral
        TypeMask n -> \x -> let m    = P.finiteBitSize x
                                bits = P.map (Bit . P.testBit x) [0 .. m P.- 1] P.++ P.repeat (Bit False)
                            in
                            unMask $ fromList (P.reverse (P.take (P.fromIntegral (natVal' n)) bits))
    --
    VectorIntegralType _ t | IntegralDict <- integralDict t ->
        case bR of
          TypeBit    -> \x -> P.fromIntegral (Vec.extract x 0)  -- XXX: first or last lane?
          TypeMask _ -> unMask . map P.fromIntegral

fromBool :: forall a b. BitType a -> IntegralType b -> (a -> b)
fromBool bR iR =
  case bR of
    TypeBit ->
      case iR of
        SingleIntegralType   t | IntegralDict <- integralDict t -> P.fromIntegral
        VectorIntegralType _ t | IntegralDict <- integralDict t -> \x ->
          if unBit x
             then Vec.insert (Vec.splat 0) 0 1  -- XXX: first or last lane?
             else Vec.splat 0
    --
    TypeMask _ ->
      case iR of
        SingleIntegralType t | IntegralDict <- integralDict t -> \x ->
          let bits = toList (BitMask x)
              m    = P.finiteBitSize (P.undefined :: b)
              --
              go !_ !w []              = w
              go !i !w (Bit True : bs) = go (i P.+ 1) (P.setBit w i) bs
              go !i !w (Bit False: bs) = go (i P.+ 1) w              bs
          in
          go 0 0 (P.reverse (P.take m bits))
        --
        VectorIntegralType _ t | IntegralDict <- integralDict t -> map P.fromIntegral . BitMask


-- Vector element-wise operations
-- ------------------------------

-- XXX: These implementations lose the type safety that the length of the
-- underlying Vec or BitMask is preserved

map :: (IsList a, IsList b) => (Item a -> Item b) -> a -> b
map f xs = fromList $ P.map f (toList xs)

zipWith :: (IsList a, IsList b, IsList c) => (Item a -> Item b -> Item c) -> a -> b -> c
zipWith f xs ys = fromList $ P.zipWith f (toList xs) (toList ys)

zipWith'
    :: (IsList a, IsList b, IsList c, IsList d)
    => (Item a -> Item b -> (Item c, Item d))
    -> a
    -> b
    -> (c, d)
zipWith' f xs ys =
  let (us, vs) = P.unzip $ P.zipWith f (toList xs) (toList ys)
   in (fromList us, fromList vs)


-- Utilities
-- ---------

data IntegralDict t where
  IntegralDict :: (P.Integral t, P.FiniteBits t, Prim t, BitOrMask t ~ Bit) => IntegralDict t

data FloatingDict t where
  FloatingDict :: (P.RealFloat t, Prim t, BitOrMask t ~ Bit) => FloatingDict t

{-# INLINE integralDict #-}
integralDict :: SingleIntegralType t -> IntegralDict t
integralDict TypeInt8    = IntegralDict
integralDict TypeInt16   = IntegralDict
integralDict TypeInt32   = IntegralDict
integralDict TypeInt64   = IntegralDict
integralDict TypeInt128  = IntegralDict
integralDict TypeWord8   = IntegralDict
integralDict TypeWord16  = IntegralDict
integralDict TypeWord32  = IntegralDict
integralDict TypeWord64  = IntegralDict
integralDict TypeWord128 = IntegralDict

{-# INLINE floatingDict #-}
floatingDict :: SingleFloatingType t -> FloatingDict t
floatingDict TypeFloat16  = FloatingDict
floatingDict TypeFloat32  = FloatingDict
floatingDict TypeFloat64  = FloatingDict
floatingDict TypeFloat128 = FloatingDict

infixr 0 $$
($$) :: (b -> a) -> (c -> d -> b) -> c -> d -> a
(f $$ g) x y = f (g x y)

