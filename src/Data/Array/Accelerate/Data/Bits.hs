{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Bits
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Bitwise operations for signed and unsigned integer expressions.
--

module Data.Array.Accelerate.Data.Bits (

  Bits(..),
  FiniteBits(..),

) where

import Data.Array.Accelerate.AST                                    ( BitOrMask )
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.FromIntegral
import Data.Array.Accelerate.Classes.Integral                       ( )
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Classes.VEq
import Data.Array.Accelerate.Classes.VOrd

import Data.Kind
import Language.Haskell.TH                                          hiding ( Exp, Type )
import Prelude                                                      ( ($), (<$>), undefined, otherwise, concat, mapM, toInteger )
import qualified Prelude                                            as P
import qualified Data.Bits                                          as B

import GHC.Exts
import GHC.TypeLits

infixl 8 `shiftL`, `shiftR`, `rotateL`, `rotateR`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.


-- | The 'Bits' class defines bitwise operations over integral scalar expression
-- types. As usual, bits are numbered from zero, with zero being the least
-- significant bit.
--
class Eq a => Bits a where
  type Bools a :: Type
  {-# MINIMAL (.&.), (.|.), xor, complement, shiftL, shiftR, rotateL, rotateR, isSigned, bit, testBit, popCount #-}

  -- | Bitwise "and"
  (.&.)         :: Exp a -> Exp a -> Exp a

  -- | Bitwise "or"
  (.|.)         :: Exp a -> Exp a -> Exp a

  -- | Bitwise "xor"
  xor           :: Exp a -> Exp a -> Exp a

  -- | Reverse all bits in the argument
  complement    :: Exp a -> Exp a

  -- | The value with all bits unset
  zeroBits      :: Exp a
  default zeroBits :: Num a => Exp a
  zeroBits = clearBit (bit 0) 0

  -- | @bit /i/@ is a value with the @/i/@th bit set and all other bits clear.
  bit           :: Exp a -> Exp a

  -- | @x \`setBit\` i@ is the same as @x .|. bit i@
  setBit        :: Exp a -> Exp a -> Exp a
  setBit x i = x .|. bit i

  -- | @x \`clearBit\` i@ is the same as @x .&. complement (bit i)@
  clearBit      :: Exp a -> Exp a -> Exp a
  clearBit x i = x .&. complement (bit i)

  -- | @x \`complementBit\` i@ is the same as @x \`xor\` bit i@
  complementBit :: Exp a -> Exp a -> Exp a
  complementBit x i = x `xor` bit i

  -- | Return 'True' if the @n@th bit of the argument is 1
  testBit       :: Exp a -> Exp a -> Exp (Bools a)

  -- | Return 'True' if the argument is a signed type.
  isSigned      :: Exp a -> Exp Bool

  -- | Shift the argument left by the specified number of bits (which must be
  -- non-negative)
  shiftL        :: Exp a -> Exp a -> Exp a

  -- | Shift the argument left by the specified number of bits. The result is
  -- undefined for negative shift amounts and shift amounts greater or equal to
  -- the 'finiteBitSize'.
  unsafeShiftL  :: Exp a -> Exp a -> Exp a
  unsafeShiftL = shiftL

  -- | Shift the first argument right by the specified number of bits (which
  -- must be non-negative).
  --
  -- Right shifts perform sign extension on signed number types; i.e. they fill
  -- the top bits with 1 if @x@ is negative and with 0 otherwise.
  shiftR        :: Exp a -> Exp a -> Exp a

  -- | Shift the first argument right by the specified number of bits. The
  -- result is undefined for negative shift amounts and shift amounts greater or
  -- equal to the 'finiteBitSize'.
  unsafeShiftR  :: Exp a -> Exp a -> Exp a
  unsafeShiftR = shiftR

  -- | Rotate the argument left by the specified number of bits (which must be
  -- non-negative).
  rotateL       :: Exp a -> Exp a -> Exp a

  -- | Rotate the argument right by the specified number of bits (which must be non-negative).
  rotateR       :: Exp a -> Exp a -> Exp a

  -- | Return the number of set bits in the argument. This number is known as
  -- the population count or the Hamming weight.
  popCount      :: Exp a -> Exp a


class Bits a => FiniteBits a where
  -- | Return the number of bits in the type of the argument.
  finiteBitSize :: Exp a -> Exp Int

  -- | Count the number of zero bits preceding the most significant set bit.
  -- This can be used to compute a base-2 logarithm via:
  --
  -- > logBase2 x = finiteBitSize x - 1 - countLeadingZeros x
  --
  countLeadingZeros :: Exp a -> Exp a

  -- | Count the number of zero bits following the least significant set bit.
  -- The related
  -- <http://en.wikipedia.org/wiki/Find_first_set find-first-set operation> can
  -- be expressed in terms of this as:
  --
  -- > findFirstSet x = 1 + countTrailingZeros x
  --
  countTrailingZeros :: Exp a -> Exp a


-- Instances for Bits
-- ------------------

instance Bits Bool where
  type Bools Bool = Bool
  (.&.)        = (&&)
  (.|.)        = (||)
  xor          = (/=)
  complement   = not
  zeroBits     = False
  testBit      = (&&)
  bit x        = x
  isSigned     = isSignedDefault
  shiftL x i   = cond i False x
  shiftR x i   = cond i False x
  rotateL x _  = x
  rotateR x _  = x
  popCount x   = x

instance FiniteBits Bool where
  finiteBitSize _      = fromInteger (toInteger (B.finiteBitSize (undefined::Bool)))
  countLeadingZeros  x = x
  countTrailingZeros x = x


-- Default implementations
-- -----------------------

bitDefault :: (Num t, Bits t) => Exp t -> Exp t
bitDefault x = 1 `shiftL` x

testBitDefault :: (Num t, Bits t) => Exp t -> Exp t -> Exp Bool
testBitDefault x i = (x .&. bit i) /= 0

-- shiftDefault :: (FiniteBits t, IsIntegral (EltR t), B.Bits t) => Exp t -> Exp t -> Exp t
-- shiftDefault x i
--   = cond (i >= 0) (shiftLDefault x i)
--                   (shiftRDefault x (-i))

shiftLDefault :: forall t. (B.FiniteBits t, Num t, Ord t, FromIntegral Int t, IsIntegral (EltR t)) => Exp t -> Exp t -> Exp t
shiftLDefault x i
  = cond (i >= P.fromIntegral (B.finiteBitSize (undefined::t))) 0
  $ mkBShiftL x i

shiftRDefault :: forall t. (B.Bits t, B.FiniteBits t, Num t, Ord t, FromIntegral Int t, IsScalar (EltR t), IsIntegral (EltR t), BitOrMask (EltR t) ~ Bit) => Exp t -> Exp t -> Exp t
shiftRDefault
  | B.isSigned (undefined::t) = shiftRADefault
  | otherwise                 = shiftRLDefault

-- Shift the argument right (signed)
shiftRADefault :: forall t. (B.FiniteBits t, Num t, Ord t, FromIntegral Int t, IsScalar (EltR t), IsIntegral (EltR t), BitOrMask (EltR t) ~ Bit) => Exp t -> Exp t -> Exp t
shiftRADefault x i
  = cond (i >= P.fromIntegral (B.finiteBitSize (undefined::t))) (cond (mkLt x 0) (-1) 0)
  $ mkBShiftR x i

-- Shift the argument right (unsigned)
shiftRLDefault :: forall t. (B.FiniteBits t, Num t, Ord t, FromIntegral Int t, IsIntegral (EltR t)) => Exp t -> Exp t -> Exp t
shiftRLDefault x i
  = cond (i >= P.fromIntegral (B.finiteBitSize (undefined::t))) 0
  $ mkBShiftR x i

-- rotateDefault :: forall t. (FiniteBits t, IsIntegral (EltR t)) => Exp t -> Exp t -> Exp t
-- rotateDefault x i
--   = cond (i < 0) (mkBRotateR x (-i))
--   $ cond (i > 0) (mkBRotateL x i)
--   $ x

{--
-- Rotation can be implemented in terms of two shifts, but care is needed
-- for negative values. This suggested implementation assumes
-- 2's-complement arithmetic.
--
-- This is disabled because (at least) LLVM-9 generates incorrect code on
-- the Turing architecture for negative shift amounts of 64-bit values.
--
rotateDefault :: forall t. (FiniteBits t, IsIntegral (EltR t)) => Exp t -> Exp Int -> Exp t
rotateDefault =
  case integralType :: IntegralType (EltR t) of
    TypeInt{}     -> rotateDefault' (undefined::Word)
    TypeInt8{}    -> rotateDefault' (undefined::Word8)
    TypeInt16{}   -> rotateDefault' (undefined::Word16)
    TypeInt32{}   -> rotateDefault' (undefined::Word32)
    TypeInt64{}   -> rotateDefault' (undefined::Word64)
    TypeWord{}    -> rotateDefault' (undefined::Word)
    TypeWord8{}   -> rotateDefault' (undefined::Word8)
    TypeWord16{}  -> rotateDefault' (undefined::Word16)
    TypeWord32{}  -> rotateDefault' (undefined::Word32)
    TypeWord64{}  -> rotateDefault' (undefined::Word64)

rotateDefault'
    :: forall i w. (Elt w, FiniteBits i, IsIntegral (EltR i), IsIntegral (EltR w), IsIntegral (EltR i), IsIntegral (EltR w), BitSizeEq (EltR i) (EltR w), BitSizeEq (EltR w) (EltR i))
    => w {- dummy -}
    -> Exp i
    -> Exp Int
    -> Exp i
rotateDefault' _ x i
  = cond (i' == 0) x
  $ w2i ((x' `mkBShiftL` i') `mkBOr` (x' `mkBShiftR` (wsib - i')))
  where
    w2i  = mkBitcast :: Exp w -> Exp i
    i2w  = mkBitcast :: Exp i -> Exp w
    --
    x'   = i2w x
    i'   = i `mkBAnd` (wsib - 1)
    wsib = finiteBitSize x
--}

-- rotateLDefault :: (Num t, Eq t, IsIntegral (EltR t)) => Exp t -> Exp t -> Exp t
-- rotateLDefault x i
--   = cond (i == 0) x
--   $ mkBRotateL x i

-- rotateRDefault :: (Num t, Eq t, IsIntegral (EltR t)) => Exp t -> Exp t -> Exp t
-- rotateRDefault x i
--   = cond (i == 0) x
--   $ mkBRotateR x i

isSignedDefault :: forall b. B.Bits b => Exp b -> Exp Bool
isSignedDefault _ = constant (B.isSigned (undefined::b))

{--
_popCountDefault :: forall a. (B.FiniteBits a, IsScalar a, Bits a, Num a) => Exp a -> Exp Int
_popCountDefault =
  $( [e| case B.finiteBitSize (undefined::a) of
           8  -> popCnt8  . mkUnsafeCoerce
           16 -> popCnt16 . mkUnsafeCoerce
           32 -> popCnt32 . mkUnsafeCoerce
           64 -> popCnt64 . mkUnsafeCoerce
           _  -> popCountKernighan |] )

-- http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetKernighan
popCountKernighan :: (Bits a, Num a) => Exp a -> Exp Int
popCountKernighan x = r
  where
    (r,_) = untup2
          $ while (\(untup2 -> (_,v)) -> v /= 0)
                  (\(untup2 -> (c,v)) -> tup2 (c+1, v .&. (v-1)))
                  (tup2 (0,x))

-- http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
popCnt8 :: Exp Word8 -> Exp Int
popCnt8 v1 = mkFromIntegral c
  where
    v2 = v1 - ((v1 `unsafeShiftR` 1) .&. 0x55)
    v3 = (v2 .&. 0x33) + ((v2 `unsafeShiftR` 2) .&. 0x33)
    v4 = (v3 + (v3 `unsafeShiftR` 4)) .&. 0x0F
    c  = v4 * 0x01

popCnt16 :: Exp Word16 -> Exp Int
popCnt16 v1 = mkFromIntegral c
  where
    v2 = v1 - ((v1 `unsafeShiftR` 1) .&. 0x5555)
    v3 = (v2 .&. 0x3333) + ((v2 `unsafeShiftR` 2) .&. 0x3333)
    v4 = (v3 + (v3 `unsafeShiftR` 4)) .&. 0x0F0F
    c  = (v4 * 0x0101) `unsafeShiftR` 8

popCnt32 :: Exp Word32 -> Exp Int
popCnt32 v1 = mkFromIntegral c
  where
    v2 = v1 - ((v1 `unsafeShiftR` 1) .&. 0x55555555)
    v3 = (v2 .&. 0x33333333) + ((v2 `unsafeShiftR` 2) .&. 0x33333333)
    v4 = (v3 + (v3 `unsafeShiftR` 4)) .&. 0x0F0F0F0F
    c  = (v4 * 0x01010101) `unsafeShiftR` 24

popCnt64 :: Exp Word64 -> Exp Int
popCnt64 v1 = mkFromIntegral c
  where
    v2 = v1 - ((v1 `unsafeShiftR` 1) .&. 0x5555555555555555)
    v3 = (v2 .&. 0x3333333333333333) + ((v2 `unsafeShiftR` 2) .&. 0x3333333333333333)
    v4 = (v3 + (v3 `unsafeShiftR` 4)) .&. 0X0F0F0F0F0F0F0F0F
    c  = (v4 * 0x0101010101010101) `unsafeShiftR` 56
--}

runQ $
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

      thBits :: Name -> Q [Dec]
      thBits a =
        [d| instance Bits $(conT a) where
              type Bools $(conT a) = Bool
              (.&.)        = mkBAnd
              (.|.)        = mkBOr
              xor          = mkBXor
              complement   = mkBNot
              bit          = bitDefault
              testBit      = testBitDefault
              shiftL       = shiftLDefault
              shiftR       = shiftRDefault
              unsafeShiftL = mkBShiftL
              unsafeShiftR = mkBShiftR
              rotateL      = mkBRotateL
              rotateR      = mkBRotateR
              isSigned     = isSignedDefault
              popCount     = mkPopCount

            instance KnownNat n => Bits (Vec n $(conT a)) where
              type Bools (Vec n $(conT a)) = (Vec n Bool)
              (.&.)        = mkBAnd
              (.|.)        = mkBOr
              xor          = mkBXor
              complement   = mkBNot
              bit          = bitDefault
              testBit x i  = (x .&. bit i) /=* 0
              shiftL x i   = select (i >=* P.fromIntegral (B.finiteBitSize (undefined :: $(conT a)))) 0 (mkBShiftL x i)
              shiftR x i
                | B.isSigned (undefined :: $(conT a)) = select (i >=* P.fromIntegral (B.finiteBitSize (undefined :: $(conT a)))) (select (x <* 0) (P.fromInteger (-1)) 0) (mkBShiftR x i)
                | otherwise                           = select (i >=* P.fromIntegral (B.finiteBitSize (undefined :: $(conT a)))) 0                                        (mkBShiftR x i)
              unsafeShiftL = mkBShiftL
              unsafeShiftR = mkBShiftR
              rotateL      = mkBRotateL
              rotateR      = mkBRotateR
              isSigned _   = constant (B.isSigned (undefined :: $(conT a)))
              popCount     = mkPopCount

            instance FiniteBits $(conT a) where
              finiteBitSize _    = fromInteger (toInteger (B.finiteBitSize (undefined :: $(conT a))))
              countLeadingZeros  = mkCountLeadingZeros
              countTrailingZeros = mkCountTrailingZeros

            instance KnownNat n => FiniteBits (Vec n $(conT a)) where
              finiteBitSize _    = fromInteger (natVal' (proxy# :: Proxy# n) * toInteger (B.finiteBitSize (undefined :: $(conT a))))
              countLeadingZeros  = mkCountLeadingZeros
              countTrailingZeros = mkCountTrailingZeros
          |]
  in
  concat <$> mapM thBits integralTypes

