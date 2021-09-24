{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
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

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Classes.Integral                       ()

import Prelude                                                      ( (.), ($), undefined, otherwise )
import qualified Data.Bits                                          as B


infixl 8 `shift`, `rotate`, `shiftL`, `shiftR`, `rotateL`, `rotateR`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.


-- | The 'Bits' class defines bitwise operations over integral scalar expression
-- types. As usual, bits are numbered from zero, with zero being the least
-- significant bit.
--
class Eq a => Bits a where
  {-# MINIMAL (.&.), (.|.), xor, complement,
              (shift | (shiftL, shiftR)),
              (rotate | (rotateL, rotateR)),
              isSigned, testBit, bit, popCount #-}

  -- | Bitwise "and"
  (.&.)         :: HasCallStack => Exp a -> Exp a -> Exp a

  -- | Bitwise "or"
  (.|.)         :: HasCallStack => Exp a -> Exp a -> Exp a

  -- | Bitwise "xor"
  xor           :: HasCallStack => Exp a -> Exp a -> Exp a

  -- | Reverse all bits in the argument
  complement    :: HasCallStack => Exp a -> Exp a

  -- | @'shift' x i@ shifts @x@ left by @i@ bits if @i@ is positive, or right by
  -- @-i@ bits otherwise. Right shifts perform sign extension on signed number
  -- types; i.e. they fill the top bits with 1 if the @x@ is negative and with
  -- 0 otherwise.
  shift         :: HasCallStack => Exp a -> Exp Int -> Exp a
  shift x i
    = sourceMap
    $ cond (i < 0) (x `shiftR` (-i))
    $ cond (i > 0) (x `shiftL` i)
    $ x

  -- | @'rotate' x i@ rotates @x@ left by @i@ bits if @i@ is positive, or right
  -- by @-i@ bits otherwise.
  rotate        :: HasCallStack => Exp a -> Exp Int -> Exp a
  rotate x i
    = sourceMap
    $ cond (i < 0) (x `rotateR` (-i))
    $ cond (i > 0) (x `rotateL` i)
    $ x

  -- | The value with all bits unset
  zeroBits      :: HasCallStack => Exp a
  zeroBits = sourceMap $ clearBit (bit 0) 0

  -- | @bit /i/@ is a value with the @/i/@th bit set and all other bits clear.
  bit           :: HasCallStack => Exp Int -> Exp a

  -- | @x \`setBit\` i@ is the same as @x .|. bit i@
  setBit        :: HasCallStack => Exp a -> Exp Int -> Exp a
  setBit x i = sourceMap $ x .|. bit i

  -- | @x \`clearBit\` i@ is the same as @x .&. complement (bit i)@
  clearBit      :: HasCallStack => Exp a -> Exp Int -> Exp a
  clearBit x i = sourceMap $ x .&. complement (bit i)

  -- | @x \`complementBit\` i@ is the same as @x \`xor\` bit i@
  complementBit :: HasCallStack => Exp a -> Exp Int -> Exp a
  complementBit x i = sourceMap $ x `xor` bit i

  -- | Return 'True' if the @n@th bit of the argument is 1
  testBit       :: HasCallStack => Exp a -> Exp Int -> Exp Bool

  -- | Return 'True' if the argument is a signed type.
  isSigned      :: HasCallStack => Exp a -> Exp Bool

  -- | Shift the argument left by the specified number of bits (which must be
  -- non-negative).
  shiftL        :: HasCallStack => Exp a -> Exp Int -> Exp a
  shiftL x i = sourceMap $ x `shift` i

  -- | Shift the argument left by the specified number of bits. The result is
  -- undefined for negative shift amounts and shift amounts greater or equal to
  -- the 'finiteBitSize'.
  unsafeShiftL  :: HasCallStack => Exp a -> Exp Int -> Exp a
  unsafeShiftL = sourceMap shiftL

  -- | Shift the first argument right by the specified number of bits (which
  -- must be non-negative).
  --
  -- Right shifts perform sign extension on signed number types; i.e. they fill
  -- the top bits with 1 if @x@ is negative and with 0 otherwise.
  shiftR        :: HasCallStack => Exp a -> Exp Int -> Exp a
  shiftR x i = sourceMap $ x `shift` (-i)

  -- | Shift the first argument right by the specified number of bits. The
  -- result is undefined for negative shift amounts and shift amounts greater or
  -- equal to the 'finiteBitSize'.
  unsafeShiftR  :: HasCallStack => Exp a -> Exp Int -> Exp a
  unsafeShiftR = sourceMap shiftR

  -- | Rotate the argument left by the specified number of bits (which must be
  -- non-negative).
  rotateL       :: HasCallStack => Exp a -> Exp Int -> Exp a
  rotateL x i = sourceMap $ x `rotate` i

  -- | Rotate the argument right by the specified number of bits (which must be non-negative).
  rotateR       :: HasCallStack => Exp a -> Exp Int -> Exp a
  rotateR x i = sourceMap $ x `rotate` (-i)

  -- | Return the number of set bits in the argument. This number is known as
  -- the population count or the Hamming weight.
  popCount      :: HasCallStack => Exp a -> Exp Int


class Bits b => FiniteBits b where
  -- | Return the number of bits in the type of the argument.
  finiteBitSize :: HasCallStack => Exp b -> Exp Int

  -- | Count the number of zero bits preceding the most significant set bit.
  -- This can be used to compute a base-2 logarithm via:
  --
  -- > logBase2 x = finiteBitSize x - 1 - countLeadingZeros x
  --
  countLeadingZeros :: HasCallStack => Exp b -> Exp Int

  -- | Count the number of zero bits following the least significant set bit.
  -- The related
  -- <http://en.wikipedia.org/wiki/Find_first_set find-first-set operation> can
  -- be expressed in terms of this as:
  --
  -- > findFirstSet x = 1 + countTrailingZeros x
  --
  countTrailingZeros :: HasCallStack => Exp b -> Exp Int


-- Instances for Bits
-- ------------------

instance Bits Bool where
  (.&.)        = sourceMap (&&)
  (.|.)        = sourceMap (||)
  xor          = sourceMap (/=)
  complement   = sourceMap not
  shift x i    = sourceMap $ cond (i == 0) x (constant False)
  testBit x i  = sourceMap $ cond (i == 0) x (constant False)
  rotate x _   = sourceMap x
  bit i        = sourceMap (i == 0)
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap boolToInt

instance Bits Int where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap bitDefault
  testBit      = sourceMap testBitDefault
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap mkPopCount

instance Bits Int8 where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap bitDefault
  testBit      = sourceMap testBitDefault
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap mkPopCount

instance Bits Int16 where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap bitDefault
  testBit      = sourceMap testBitDefault
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap mkPopCount

instance Bits Int32 where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap bitDefault
  testBit      = sourceMap testBitDefault
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap mkPopCount

instance Bits Int64 where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap bitDefault
  testBit      = sourceMap testBitDefault
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap mkPopCount

instance Bits Word where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap bitDefault
  testBit      = sourceMap testBitDefault
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap mkPopCount

instance Bits Word8 where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap bitDefault
  testBit      = sourceMap testBitDefault
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap mkPopCount

instance Bits Word16 where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap bitDefault
  testBit      = sourceMap testBitDefault
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap mkPopCount

instance Bits Word32 where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap bitDefault
  testBit      = sourceMap testBitDefault
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap mkPopCount

instance Bits Word64 where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap bitDefault
  testBit      = sourceMap testBitDefault
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap mkPopCount

instance Bits CInt where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap $ mkBitcast . bitDefault @Int32
  testBit b    = sourceMap $ testBitDefault (mkBitcast @Int32 b)
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap $ mkPopCount . mkBitcast @Int32

instance Bits CUInt where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap $ mkBitcast . bitDefault @Word32
  testBit b    = sourceMap $ testBitDefault (mkBitcast @Word32 b)
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap $ mkPopCount . mkBitcast @Word32

instance Bits CLong where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap $ mkBitcast . bitDefault @HTYPE_CLONG
  testBit b    = sourceMap $ testBitDefault (mkBitcast @HTYPE_CLONG b)
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap $ mkPopCount . mkBitcast @HTYPE_CLONG

instance Bits CULong where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap $ mkBitcast . bitDefault @HTYPE_CULONG
  testBit b    = sourceMap $ testBitDefault (mkBitcast @HTYPE_CULONG b)
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap $ mkPopCount . mkBitcast @HTYPE_CULONG

instance Bits CLLong where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap $ mkBitcast . bitDefault @Int64
  testBit b    = sourceMap $ testBitDefault (mkBitcast @Int64 b)
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap $ mkPopCount . mkBitcast @Int64

instance Bits CULLong where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap $ mkBitcast . bitDefault @Word64
  testBit b    = sourceMap $ testBitDefault (mkBitcast @Word64 b)
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap $ mkPopCount . mkBitcast @Word64

instance Bits CShort where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap $ mkBitcast . bitDefault @Int16
  testBit b    = sourceMap $ testBitDefault (mkBitcast @Int16 b)
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap $ mkPopCount . mkBitcast @Int16

instance Bits CUShort where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap $ mkBitcast . bitDefault @Word16
  testBit b    = sourceMap $ testBitDefault (mkBitcast @Word16 b)
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap $ mkPopCount . mkBitcast @Word16

instance Bits CChar where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap $ mkBitcast . bitDefault @HTYPE_CCHAR
  testBit b    = sourceMap $ testBitDefault (mkBitcast @HTYPE_CCHAR b)
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap $ mkPopCount . mkBitcast @HTYPE_CCHAR

instance Bits CSChar where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap $ mkBitcast . bitDefault @Int8
  testBit b    = sourceMap $ testBitDefault (mkBitcast @Int8 b)
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap $ mkPopCount . mkBitcast @Int8

instance Bits CUChar where
  (.&.)        = sourceMap mkBAnd
  (.|.)        = sourceMap mkBOr
  xor          = sourceMap mkBXor
  complement   = sourceMap mkBNot
  bit          = sourceMap $ mkBitcast . bitDefault @Word8
  testBit b    = sourceMap $ testBitDefault (mkBitcast @Word8 b)
  shift        = sourceMap shiftDefault
  shiftL       = sourceMap shiftLDefault
  shiftR       = sourceMap shiftRDefault
  unsafeShiftL = sourceMap mkBShiftL
  unsafeShiftR = sourceMap mkBShiftR
  rotate       = sourceMap rotateDefault
  rotateL      = sourceMap rotateLDefault
  rotateR      = sourceMap rotateRDefault
  isSigned     = sourceMap isSignedDefault
  popCount     = sourceMap $ mkPopCount . mkBitcast @Word8



-- Instances for FiniteBits
-- ------------------------

instance FiniteBits Bool where
  finiteBitSize _      = sourceMap $ constInt 8 -- stored as Word8 {- (B.finiteBitSize (undefined::Bool)) -}
  countLeadingZeros  x = sourceMap $ cond x 0 1
  countTrailingZeros x = sourceMap $ cond x 0 1

instance FiniteBits Int where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::Int))
  countLeadingZeros  = sourceMap mkCountLeadingZeros
  countTrailingZeros = sourceMap mkCountTrailingZeros

instance FiniteBits Int8 where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::Int8))
  countLeadingZeros  = sourceMap mkCountLeadingZeros
  countTrailingZeros = sourceMap mkCountTrailingZeros

instance FiniteBits Int16 where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::Int16))
  countLeadingZeros  = sourceMap mkCountLeadingZeros
  countTrailingZeros = sourceMap mkCountTrailingZeros

instance FiniteBits Int32 where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::Int32))
  countLeadingZeros  = sourceMap mkCountLeadingZeros
  countTrailingZeros = sourceMap mkCountTrailingZeros

instance FiniteBits Int64 where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::Int64))
  countLeadingZeros  = sourceMap mkCountLeadingZeros
  countTrailingZeros = sourceMap mkCountTrailingZeros

instance FiniteBits Word where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::Word))
  countLeadingZeros  = sourceMap mkCountLeadingZeros
  countTrailingZeros = sourceMap mkCountTrailingZeros

instance FiniteBits Word8 where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::Word8))
  countLeadingZeros  = sourceMap mkCountLeadingZeros
  countTrailingZeros = sourceMap mkCountTrailingZeros

instance FiniteBits Word16 where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::Word16))
  countLeadingZeros  = sourceMap mkCountLeadingZeros
  countTrailingZeros = sourceMap mkCountTrailingZeros

instance FiniteBits Word32 where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::Word32))
  countLeadingZeros  = sourceMap mkCountLeadingZeros
  countTrailingZeros = sourceMap mkCountTrailingZeros

instance FiniteBits Word64 where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::Word64))
  countLeadingZeros  = sourceMap mkCountLeadingZeros
  countTrailingZeros = sourceMap mkCountTrailingZeros

instance FiniteBits CInt where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::CInt))
  countLeadingZeros  = sourceMap $ mkCountLeadingZeros  . mkBitcast @Int32
  countTrailingZeros = sourceMap $ mkCountTrailingZeros . mkBitcast @Int32

instance FiniteBits CUInt where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::CUInt))
  countLeadingZeros  = sourceMap $ mkCountLeadingZeros  . mkBitcast @Word32
  countTrailingZeros = sourceMap $ mkCountTrailingZeros . mkBitcast @Word32

instance FiniteBits CLong where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::CLong))
  countLeadingZeros  = sourceMap $ mkCountLeadingZeros  . mkBitcast @HTYPE_CLONG
  countTrailingZeros = sourceMap $ mkCountTrailingZeros . mkBitcast @HTYPE_CLONG

instance FiniteBits CULong where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::CULong))
  countLeadingZeros  = sourceMap $ mkCountLeadingZeros  . mkBitcast @HTYPE_CULONG
  countTrailingZeros = sourceMap $ mkCountTrailingZeros . mkBitcast @HTYPE_CULONG

instance FiniteBits CLLong where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::CLLong))
  countLeadingZeros  = sourceMap $ mkCountLeadingZeros  . mkBitcast @Int64
  countTrailingZeros = sourceMap $ mkCountTrailingZeros . mkBitcast @Int64

instance FiniteBits CULLong where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::CULLong))
  countLeadingZeros  = sourceMap $ mkCountLeadingZeros  . mkBitcast @Word64
  countTrailingZeros = sourceMap $ mkCountTrailingZeros . mkBitcast @Word64

instance FiniteBits CShort where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::CShort))
  countLeadingZeros  = sourceMap $ mkCountLeadingZeros  . mkBitcast @Int16
  countTrailingZeros = sourceMap $ mkCountTrailingZeros . mkBitcast @Int16

instance FiniteBits CUShort where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::CUShort))
  countLeadingZeros  = sourceMap $ mkCountLeadingZeros  . mkBitcast @Word16
  countTrailingZeros = sourceMap $ mkCountTrailingZeros . mkBitcast @Word16

instance FiniteBits CChar where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::CChar))
  countLeadingZeros  = sourceMap $ mkCountLeadingZeros  . mkBitcast @HTYPE_CCHAR
  countTrailingZeros = sourceMap $ mkCountTrailingZeros . mkBitcast @HTYPE_CCHAR

instance FiniteBits CSChar where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::CSChar))
  countLeadingZeros  = sourceMap $ mkCountLeadingZeros  . mkBitcast @Int8
  countTrailingZeros = sourceMap $ mkCountTrailingZeros . mkBitcast @Int8

instance FiniteBits CUChar where
  finiteBitSize _    = sourceMap $ constInt (B.finiteBitSize (undefined::CUChar))
  countLeadingZeros  = sourceMap $ mkCountLeadingZeros  . mkBitcast @Word8
  countTrailingZeros = sourceMap $ mkCountTrailingZeros . mkBitcast @Word8


-- Default implementations
-- -----------------------
bitDefault :: (SourceMapped, IsIntegral (EltR t), Bits t) => Exp Int -> Exp t
bitDefault x = constInt 1 `shiftL` x

testBitDefault :: (SourceMapped, IsIntegral (EltR t), Bits t) => Exp t -> Exp Int -> Exp Bool
testBitDefault x i = (x .&. bit i) /= constInt 0

shiftDefault :: (SourceMapped, FiniteBits t, IsIntegral (EltR t), B.Bits t) => Exp t -> Exp Int -> Exp t
shiftDefault x i
  = cond (i >= 0) (shiftLDefault x i)
                  (shiftRDefault x (-i))

shiftLDefault :: (SourceMapped, FiniteBits t, IsIntegral (EltR t)) => Exp t -> Exp Int -> Exp t
shiftLDefault x i
  = cond (i >= finiteBitSize x) (constInt 0)
  $ mkBShiftL x i

shiftRDefault :: forall t. (SourceMapped, B.Bits t, FiniteBits t, IsIntegral (EltR t)) => Exp t -> Exp Int -> Exp t
shiftRDefault
  | B.isSigned (undefined::t) = shiftRADefault
  | otherwise                 = shiftRLDefault

-- Shift the argument right (signed)
shiftRADefault :: (SourceMapped, FiniteBits t, IsIntegral (EltR t)) => Exp t -> Exp Int -> Exp t
shiftRADefault x i
  = cond (i >= finiteBitSize x) (cond (mkLt x (constInt 0)) (constInt (-1)) (constInt 0))
  $ mkBShiftR x i

-- Shift the argument right (unsigned)
shiftRLDefault :: (SourceMapped, FiniteBits t, IsIntegral (EltR t)) => Exp t -> Exp Int -> Exp t
shiftRLDefault x i
  = cond (i >= finiteBitSize x) (constInt 0)
  $ mkBShiftR x i

rotateDefault :: forall t. (SourceMapped, FiniteBits t, IsIntegral (EltR t)) => Exp t -> Exp Int -> Exp t
rotateDefault x i
  = cond (i < 0) (mkBRotateR x (-i))
  $ cond (i > 0) (mkBRotateL x i)
  $ x

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

rotateLDefault :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp Int -> Exp t
rotateLDefault x i
  = cond (i == 0) x
  $ mkBRotateL x i

rotateRDefault :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp Int -> Exp t
rotateRDefault x i
  = cond (i == 0) x
  $ mkBRotateR x i

isSignedDefault :: forall b. (SourceMapped, B.Bits b) => Exp b -> Exp Bool
isSignedDefault _ = constant (B.isSigned (undefined::b))

constInt :: (SourceMapped, IsIntegral (EltR e)) => EltR e -> Exp e
constInt = mkExp . Const mkAnn (SingleScalarType (NumSingleType (IntegralNumType integralType)))

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

