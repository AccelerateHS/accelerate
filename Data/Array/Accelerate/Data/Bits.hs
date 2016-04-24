{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Bits
-- Copyright   : [2016] Manuel M T Chakravarty, Gabriele Keller
--               [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Bitwise operations for signed and unsigned integer expressions.
--

module Data.Array.Accelerate.Data.Bits (

  Bits(..),
  FiniteBits(..),

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Integral                       ()

import Prelude                                                      ( ($), undefined )
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
  (.&.)         :: Exp a -> Exp a -> Exp a

  -- | Bitwise "or"
  (.|.)         :: Exp a -> Exp a -> Exp a

  -- | Bitwise "xor"
  xor           :: Exp a -> Exp a -> Exp a

  -- | Reverse all bits in the argument
  complement    :: Exp a -> Exp a

  -- | @'shift' x i@ shifts @x@ left by @i@ bits if @i@ is positive, or right by
  -- @-i@ bits otherwise. Right shifts perform sign extension on signed number
  -- types; i.e. they fill the top bits with 1 if the @x@ is negative and with
  -- 0 otherwise.
  shift         :: Exp a -> Exp Int -> Exp a
  shift x i
    = cond (i <* 0) (x `shiftR` (-i))
    $ cond (i >* 0) (x `shiftL` i)
    $ x

  -- | @'rotate' x i@ rotates @x@ left by @i@ bits if @i@ is positive, or right
  -- by @-i@ bits otherwise.
  rotate        :: Exp a -> Exp Int -> Exp a
  rotate x i
    = cond (i <* 0) (x `rotateR` (-i))
    $ cond (i >* 0) (x `rotateL` i)
    $ x

  -- | The value with all bits unset
  zeroBits      :: Exp a
  zeroBits = clearBit (bit 0) 0

  -- | @bit /i/@ is a value with the @/i/@th bit set and all other bits clear.
  bit           :: Exp Int -> Exp a

  -- | @x \`setBit\` i@ is the same as @x .|. bit i@
  setBit        :: Exp a -> Exp Int -> Exp a
  setBit x i = x .|. bit i

  -- | @x \`clearBit\` i@ is the same as @x .&. complement (bit i)@
  clearBit      :: Exp a -> Exp Int -> Exp a
  clearBit x i = x .&. complement (bit i)

  -- | @x \`complementBit\` i@ is the same as @x \`xor\` bit i@
  complementBit :: Exp a -> Exp Int -> Exp a
  complementBit x i = x `xor` bit i

  -- | Return 'True' if the @n@th bit of the argument is 1
  testBit       :: Exp a -> Exp Int -> Exp Bool

  -- | Return 'True' if the argument is a signed type.
  isSigned      :: Exp a -> Exp Bool

  -- | Shift the argument left by the specified number of bits (which must be
  -- non-negative).
  shiftL        :: Exp a -> Exp Int -> Exp a
  shiftL x i = x `shift` i

  -- | Shift the first argument right by the specified number of bits. The
  -- result is undefined for negative shift amounts and shift amounts greater or
  -- equal to the 'bitSize'.
  --
  -- Right shifts perform sign extension on signed number types; i.e. they fill
  -- the top bits with 1 if the @x@ is negative and with 0 otherwise.
  shiftR        :: Exp a -> Exp Int -> Exp a
  shiftR x i = x `shift` (-i)

  -- | Rotate the argument left by the specified number of bits (which must be
  -- non-negative).
  rotateL       :: Exp a -> Exp Int -> Exp a
  rotateL x i = x `rotate` i

  -- | Rotate the argument right by the specified number of bits (which must be non-negative).
  rotateR       :: Exp a -> Exp Int -> Exp a
  rotateR x i = x `rotate` (-i)

  -- | Return the number of set bits in the argument. This number is known as
  -- the population count or the Hamming weight.
  popCount      :: Exp a -> Exp Int


class Bits b => FiniteBits b where
  -- | Return the number of bits in the type of the argument.
  finiteBitSize :: Exp b -> Exp Int


-- Instances for Bits
-- ------------------

instance Bits Bool where
  (.&.)       = (&&*)
  (.|.)       = (||*)
  xor         = (/=*)
  complement  = not
  shift x i   = cond (i ==* 0) x (constant False)
  testBit x i = cond (i ==* 0) x (constant False)
  rotate x _  = x
  bit i       = i ==* 0
  isSigned    = isSignedDefault
  popCount x  = cond x 1 0

instance Bits Int where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits Int8 where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits Int16 where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits Int32 where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits Int64 where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits Word where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits Word8 where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits Word16 where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits Word32 where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits Word64 where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits CInt where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits CUInt where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits CLong where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits CULong where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits CLLong where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits CULLong where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits CShort where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

instance Bits CUShort where
  (.&.)       = mkBAnd
  (.|.)       = mkBOr
  xor         = mkBXor
  complement  = mkBNot
  bit         = bitDefault
  testBit     = testBitDefault
  shift       = shiftDefault
  shiftL      = shiftLDefault
  shiftR      = shiftRDefault
  rotate      = rotateDefault
  rotateL     = rotateLDefault
  rotateR     = rotateRDefault
  isSigned    = isSignedDefault
  popCount    = popCountDefault

-- instance Bits CChar where
--   (.&.)       = mkBAnd
--   (.|.)       = mkBOr
--   xor         = mkBXor
--   complement  = mkBNot
--   bit         = bitDefault
--   testBit     = testBitDefault
--   shift       = shiftDefault
--   shiftL      = shiftLDefault
--   shiftR      = shiftRDefault
--   rotate      = rotateDefault
--   rotateL     = rotateLDefault
--   rotateR     = rotateRDefault
--   isSigned    = isSignedDefault
--   popCount    = popCountDefault

-- instance Bits CUChar where
--   (.&.)       = mkBAnd
--   (.|.)       = mkBOr
--   xor         = mkBXor
--   complement  = mkBNot
--   bit         = bitDefault
--   testBit     = testBitDefault
--   shift       = shiftDefault
--   shiftL      = shiftLDefault
--   shiftR      = shiftRDefault
--   rotate      = rotateDefault
--   rotateL     = rotateLDefault
--   rotateR     = rotateRDefault
--   isSigned    = isSignedDefault
--   popCount    = popCountDefault

-- instance Bits CSChar where
--   (.&.)       = mkBAnd
--   (.|.)       = mkBOr
--   xor         = mkBXor
--   complement  = mkBNot
--   bit         = bitDefault
--   testBit     = testBitDefault
--   shift       = shiftDefault
--   shiftL      = shiftLDefault
--   shiftR      = shiftRDefault
--   rotate      = rotateDefault
--   rotateL     = rotateLDefault
--   rotateR     = rotateRDefault
--   isSigned    = isSignedDefault
--   popCount    = popCountDefault


-- Instances for FiniteBits
-- ------------------------

instance FiniteBits Bool where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::Bool))

instance FiniteBits Int where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::Int))

instance FiniteBits Int8 where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::Int8))

instance FiniteBits Int16 where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::Int16))

instance FiniteBits Int32 where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::Int32))

instance FiniteBits Int64 where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::Int64))

instance FiniteBits Word where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::Word))

instance FiniteBits Word8 where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::Word8))

instance FiniteBits Word16 where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::Word16))

instance FiniteBits Word32 where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::Word32))

instance FiniteBits Word64 where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::Word64))

instance FiniteBits CInt where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::CInt))

instance FiniteBits CUInt where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::CUInt))

instance FiniteBits CLong where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::CLong))

instance FiniteBits CULong where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::CULong))

instance FiniteBits CLLong where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::CLLong))

instance FiniteBits CULLong where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::CULLong))

instance FiniteBits CShort where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::CShort))

instance FiniteBits CUShort where
  finiteBitSize _ = constant (B.finiteBitSize (undefined::CUShort))

-- instance FiniteBits CChar
-- instance FiniteBits CUChar
-- instance FiniteBits CSChar


-- Default implementations
-- -----------------------

bitDefault :: (Elt t, IsIntegral t, Bits t) => Exp Int -> Exp t
bitDefault x = constant 1 `shiftL` x

testBitDefault :: (Elt t, IsIntegral t, Bits t) => Exp t -> Exp Int -> Exp Bool
testBitDefault x i = (x .&. bit i) /=* constant 0

shiftDefault :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
shiftDefault x i
  = cond (i ==* 0) x
  $ cond (i <*  0) (x `mkBShiftR` (-1))
                   (x `mkBShiftL` i)

shiftLDefault :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
shiftLDefault x i
  = cond (i ==* 0) x
  $ mkBShiftL x i

shiftRDefault :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
shiftRDefault x i
  = cond (i ==* 0) x
  $ mkBShiftR x i

rotateDefault :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
rotateDefault x i
  = cond (i ==* 0) x
  $ cond (i <*  0) (x `mkBRotateR` (-1))
                   (x `mkBRotateL` i)

rotateLDefault :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
rotateLDefault x i
  = cond (i ==* 0) x
  $ mkBRotateL x i

rotateRDefault :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
rotateRDefault x i
  = cond (i ==* 0) x
  $ mkBRotateR x i

isSignedDefault :: forall b. B.Bits b => Exp b -> Exp Bool
isSignedDefault _ = constant (B.isSigned (undefined::b))

popCountDefault :: (Bits a, Num a) => Exp a -> Exp Int
popCountDefault = $internalError "Bits.popCount" "Not implemented yet"

