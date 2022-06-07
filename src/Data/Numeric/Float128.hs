{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Numeric.Float128
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- IEEE 128-bit floating point type (quadruple precision), consisting of
-- a 15-bit signed exponent and 113-bit mantissa (compared to 11-bit and
-- 53-bit respectively for IEEE double precision).
--
-- Partly stolen from the (unmaintained) float128 package (BSD3 license).
-- We required the Float128 data type even if operations on that type are
-- not implemented, and link against the methods from the quadmath library
-- (as this is what LLVM will generate).
--

module Data.Numeric.Float128 (

  Float128(..),

) where

import Numeric
import Data.Bits
import Data.Primitive.Types
import Data.Ratio
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import GHC.Base
import GHC.Int
import GHC.Integer.Logarithms
import GHC.Word

#if defined(FLOAT128_ENABLE) && !defined(__GHCIDE__)
import Language.Haskell.TH.Syntax
#endif


-- | A 128-bit floating point number
--
data Float128 = Float128 !Word64 !Word64
  deriving Eq

instance Show Float128 where
  showsPrec p x = showParen (x < 0 && p > 6) (showFloat x)

instance Read Float128 where
  readsPrec _ = readSigned readFloat

instance Ord Float128 where
  (<)  = cmp c_ltq
  (>)  = cmp c_gtq
  (<=) = cmp c_leq
  (>=) = cmp c_geq
  min  = call2 c_fminq
  max  = call2 c_fmaxq

instance Num Float128 where
  (+)    = call2 c_addq
  (-)    = call2 c_subq
  (*)    = call2 c_mulq
  negate = call1 c_negateq
  abs    = call1 c_absq
  signum = call1 c_signumq
  fromInteger z = encodeFloat z 0

instance Fractional Float128 where
  (/)   = call2 c_divq
  recip = call1 c_recipq
  fromRational q = -- FIXME accuracy?
    let a = fromInteger (numerator q) / fromInteger (denominator q)
        b = fromInteger (numerator r) / fromInteger (denominator r)
        r = q - toRational a
     in a + b

instance Floating Float128 where
  pi = unsafePerformIO $ alloca $ \pr -> do
         c_piq pr
         peek pr
  exp   = call1 c_expq
  log   = call1 c_logq
  sqrt  = call1 c_sqrtq
  (**)  = call2 c_powq
  sin   = call1 c_sinq
  cos   = call1 c_cosq
  tan   = call1 c_tanq
  asin  = call1 c_asinq
  acos  = call1 c_acosq
  atan  = call1 c_atanq
  sinh  = call1 c_sinhq
  cosh  = call1 c_coshq
  tanh  = call1 c_tanhq
  asinh = call1 c_asinhq
  acosh = call1 c_acoshq
  atanh = call1 c_atanhq
  log1p = call1 c_log1pq
  expm1 = call1 c_expm1q

instance Real Float128 where
  toRational l =
    case decodeFloat l of
      (m, e)
        | e >= 0    -> m `shiftL` e % 1
        | otherwise -> m % bit (negate e)

instance RealFrac Float128 where
  properFraction l
    | l >= 0    = let n' = floor' l
                      n  = fromInteger (toInteger' n')
                      f  = l - n'
                   in (n, f)
    | l < 0     = let n' = ceil' l
                      n  = fromInteger (toInteger' n')
                      f  = l - n'
                   in (n, f)
    | otherwise = (0, l) -- NaN

  truncate = fromInteger . toInteger' . trunc'
  round    = fromInteger . toInteger' . round'
  floor    = fromInteger . toInteger' . floor'
  ceiling  = fromInteger . toInteger' . ceil'

toInteger' :: Float128 -> Integer
toInteger' l =
  case decodeFloat l of
    (m, e)
      | e >= 0    -> m `shiftL` e
      | otherwise -> m `shiftR` negate e

round', trunc', floor', ceil' :: Float128 -> Float128
round' = call1 c_roundq
trunc' = call1 c_truncq
floor' = call1 c_floorq
ceil'  = call1 c_ceilq

instance RealFloat Float128 where
  isIEEE _       = True
  floatRadix _   = 2
  floatDigits _  = 113             -- quadmath.h:FLT128_MANT_DIG
  floatRange _   = (-16381,16384)  -- quadmath.h:FLT128_MIN_EXP, FLT128_MAX_EXP
  isNaN          = tst c_isnanq
  isInfinite     = tst c_isinfq
  isNegativeZero = tst c_isnegzeroq
  isDenormalized = tst c_isdenormq
  atan2          = call2 c_atan2q

  decodeFloat l@(Float128 msw lsw)
    | isNaN l      = (0, 0)
    | isInfinite l = (0, 0)
    | l == 0       = (0, 0)
    | isDenormalized l =
        case decodeFloat (scaleFloat 128 l) of
          (m, e) -> (m, e - 128)
    | otherwise =
        let s  = shiftR msw 48 `testBit` 15
            m0 = shiftL (0x1000000000000 .|. toInteger msw .&. 0xFFFFffffFFFF) 64 .|. toInteger lsw
            e0 = shiftR msw 48 .&. (bit 15 - 1)
            m  = if s then negate m0 else m0
            e  = fromIntegral e0 - 16383 - 112 -- FIXME verify
         in (m, e)

  encodeFloat m e
    | m == 0          = Float128 0 0
    | m <  0          = negate (encodeFloat (negate m) e)
    | b >= bit 15 - 1 = Float128 (shiftL (bit 15 - 1) 48) 0                 -- infinity
    | b <= 0          = scaleFloat (b - 128) (encodeFloat m (e - b + 128))  -- denormal
    | otherwise       = Float128 msw lsw                                    -- normal
    where
      l = I# (integerLog2# m)
      t = l - 112 -- FIXME verify
      m' | t >= 0    = m `shiftR`        t
         | otherwise = m `shiftL` negate t
      -- FIXME: verify that m' `testBit` 112 == True
      lsw = fromInteger (m' .&. 0xFFFFffffFFFFffff)
      msw = fromInteger (shiftR m' 64 .&. 0xFFFFffffFFFF) .|. shiftL (fromIntegral b) 48
      b   = e + t + 16383 + 112 -- FIXME verify

  exponent l@(Float128 msw _)
    | isNaN l      = 0
    | isInfinite l = 0
    | l == 0       = 0
    | isDenormalized l = snd (decodeFloat l) + 113
    | otherwise        = let e0 = shiftR msw 48 .&. (bit 15 - 1)
                          in fromIntegral e0 - 16383 - 112 + 113

  significand l = unsafePerformIO $
    with l $ \lp ->
    alloca $ \ep -> do
      c_frexpq lp lp ep
      peek lp

  scaleFloat e l = unsafePerformIO $
    with l $ \lp -> do
      c_ldexpq lp lp (fromIntegral e)
      peek lp

instance Storable Float128 where
  {-# INLINE sizeOf    #-}
  {-# INLINE alignment #-}
  {-# INLINE peek      #-}
  {-# INLINE poke      #-}
  sizeOf _    = 16
  alignment _ = 16
  peek        = peek128
  peekElemOff = peekElemOff128
  poke        = poke128
  pokeElemOff = pokeElemOff128

instance Prim Float128 where
  {-# INLINE sizeOf#         #-}
  {-# INLINE alignment#      #-}
  {-# INLINE indexByteArray# #-}
  {-# INLINE readByteArray#  #-}
  {-# INLINE writeByteArray# #-}
  {-# INLINE setByteArray#   #-}
  {-# INLINE indexOffAddr#   #-}
  {-# INLINE readOffAddr#    #-}
  {-# INLINE writeOffAddr#   #-}
  {-# INLINE setOffAddr#     #-}
  sizeOf# _       = 16#
  alignment# _    = 16#
  indexByteArray# = indexByteArray128#
  readByteArray#  = readByteArray128#
  writeByteArray# = writeByteArray128#
  setByteArray#   = setByteArray128#
  indexOffAddr#   = indexOffAddr128#
  readOffAddr#    = readOffAddr128#
  writeOffAddr#   = writeOffAddr128#
  setOffAddr#     = setOffAddr128#

{-# INLINE peek128 #-}
peek128 :: Ptr Float128 -> IO Float128
peek128 ptr = Float128 <$> peekElemOff (castPtr ptr) index1
                       <*> peekElemOff (castPtr ptr) index0

{-# INLINE peekElemOff128 #-}
peekElemOff128 :: Ptr Float128 -> Int -> IO Float128
peekElemOff128 ptr i =
  let i2 = 2 * i
   in Float128 <$> peekElemOff (castPtr ptr) (i2 + index1)
               <*> peekElemOff (castPtr ptr) (i2 + index0)

{-# INLINE poke128 #-}
poke128 :: Ptr Float128 -> Float128 -> IO ()
poke128 ptr (Float128 a b) = do
  pokeElemOff (castPtr ptr) index1 a
  pokeElemOff (castPtr ptr) index0 b

{-# INLINE pokeElemOff128 #-}
pokeElemOff128 :: Ptr Float128 -> Int -> Float128 -> IO ()
pokeElemOff128 ptr i (Float128 a1 a0) =
  let i2 = 2 * i
   in do pokeElemOff (castPtr ptr) (i2 + index0) a0
         pokeElemOff (castPtr ptr) (i2 + index1) a1

{-# INLINE indexByteArray128# #-}
indexByteArray128# :: ByteArray# -> Int# -> Float128
indexByteArray128# arr# i# =
  let i2# = 2# *# i#
      x   = indexByteArray# arr# (i2# +# unInt index1)
      y   = indexByteArray# arr# (i2# +# unInt index0)
   in Float128 x y

{-# INLINE readByteArray128# #-}
readByteArray128# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Float128 #)
readByteArray128# arr# i# s0 =
  let i2# = 2# *# i#
   in case readByteArray# arr# (i2# +# unInt index1) s0 of { (# s1, x #) ->
      case readByteArray# arr# (i2# +# unInt index0) s1 of { (# s2, y #) ->
        (# s2, Float128 x y #)
      }}

{-# INLINE writeByteArray128# #-}
writeByteArray128# :: MutableByteArray# s -> Int# -> Float128 -> State# s -> State# s
writeByteArray128# arr# i# (Float128 a b) s0 =
  let i2# = 2# *# i#
   in case writeByteArray# arr# (i2# +# unInt index1) a s0 of { s1 ->
      case writeByteArray# arr# (i2# +# unInt index0) b s1 of { s2 ->
        s2
      }}

{-# INLINE setByteArray128# #-}
setByteArray128# :: MutableByteArray# s -> Int# -> Int# -> Float128 -> State# s -> State# s
setByteArray128# = defaultSetByteArray#

{-# INLINE indexOffAddr128# #-}
indexOffAddr128# :: Addr# -> Int# -> Float128
indexOffAddr128# addr# i# =
  let i2# = 2# *# i#
      x   = indexOffAddr# addr# (i2# +# unInt index1)
      y   = indexOffAddr# addr# (i2# +# unInt index0)
   in Float128 x y

{-# INLINE readOffAddr128# #-}
readOffAddr128# :: Addr# -> Int# -> State# s -> (# State# s, Float128 #)
readOffAddr128# addr# i# s0 =
  let i2# = 2# *# i#
   in case readOffAddr# addr# (i2# +# unInt index1) s0 of { (# s1, x #) ->
      case readOffAddr# addr# (i2# +# unInt index0) s1 of { (# s2, y #) ->
        (# s2, Float128 x y #)
      }}

{-# INLINE writeOffAddr128# #-}
writeOffAddr128# :: Addr# -> Int# -> Float128 -> State# s -> State# s
writeOffAddr128# addr# i# (Float128 a b) s0 =
  let i2# = 2# *# i#
   in case writeOffAddr# addr# (i2# +# unInt index1) a s0 of { s1 ->
      case writeOffAddr# addr# (i2# +# unInt index0) b s1 of { s2 ->
        s2
      }}

{-# INLINE setOffAddr128# #-}
setOffAddr128# :: Addr# -> Int# -> Int# -> Float128 -> State# s -> State# s
setOffAddr128# = defaultSetOffAddr#

{-# INLINE unInt #-}
unInt :: Int -> Int#
unInt (I# i#) = i#

-- Use these indices to get the peek/poke ordering endian correct.
{-# INLINE index0 #-}
{-# INLINE index1 #-}
index0, index1 :: Int
#if WORDS_BIGENDIAN
index0 = 1
index1 = 0
#else
index0 = 0
index1 = 1
#endif

type F1  = Ptr Float128 -> Ptr Float128 -> IO ()
type F2  = Ptr Float128 -> Ptr Float128 -> Ptr Float128 -> IO ()
type CMP = Ptr Float128 -> Ptr Float128 -> IO Int32
type TST = Ptr Float128 -> IO Int32

{-# INLINE call1 #-}
call1 :: F1 -> Float128 -> Float128
call1 f x = unsafePerformIO $
  alloca $ \pr ->
  with x $ \px -> do
    f pr px
    peek pr

{-# INLINE call2 #-}
call2 :: F2 -> Float128 -> Float128 -> Float128
call2 f x y = unsafePerformIO $
  alloca $ \pr ->
  with x $ \px -> do
  with y $ \py -> do
    f pr px py
    peek pr

{-# INLINE cmp #-}
cmp :: CMP -> Float128 -> Float128 -> Bool
cmp f x y = unsafePerformIO $
  with x $ \px ->
  with y $ \py ->
    toBool <$> f px py

{-# INLINE tst #-}
tst :: TST -> Float128 -> Bool
tst f x = unsafePerformIO $
  with x $ \px ->
    toBool <$> f px


-- SEE: [HLS and GHC IDE]
--
#if defined(FLOAT128_ENABLE) && !defined(__GHCIDE__)

foreign import ccall unsafe "_addq" c_addq :: F2
foreign import ccall unsafe "_subq" c_subq :: F2
foreign import ccall unsafe "_mulq" c_mulq :: F2
foreign import ccall unsafe "_absq" c_absq :: F1
foreign import ccall unsafe "_negateq" c_negateq :: F1
foreign import ccall unsafe "_signumq" c_signumq :: F1

foreign import ccall unsafe "_divq" c_divq :: F2
foreign import ccall unsafe "_recipq" c_recipq :: F1

foreign import ccall unsafe "_piq" c_piq :: Ptr Float128 -> IO ()
foreign import ccall unsafe "_expq" c_expq :: F1
foreign import ccall unsafe "_logq" c_logq :: F1
foreign import ccall unsafe "_sqrtq" c_sqrtq :: F1
foreign import ccall unsafe "_powq" c_powq :: F2
foreign import ccall unsafe "_sinq" c_sinq :: F1
foreign import ccall unsafe "_cosq" c_cosq :: F1
foreign import ccall unsafe "_tanq" c_tanq :: F1
foreign import ccall unsafe "_asinq" c_asinq :: F1
foreign import ccall unsafe "_acosq" c_acosq :: F1
foreign import ccall unsafe "_atanq" c_atanq :: F1
foreign import ccall unsafe "_sinhq" c_sinhq :: F1
foreign import ccall unsafe "_coshq" c_coshq :: F1
foreign import ccall unsafe "_tanhq" c_tanhq :: F1
foreign import ccall unsafe "_asinhq" c_asinhq :: F1
foreign import ccall unsafe "_acoshq" c_acoshq :: F1
foreign import ccall unsafe "_atanhq" c_atanhq :: F1
foreign import ccall unsafe "_log1pq" c_log1pq :: F1
foreign import ccall unsafe "_expm1q" c_expm1q :: F1

foreign import ccall unsafe "_roundq" c_roundq :: F1
foreign import ccall unsafe "_truncq" c_truncq :: F1
foreign import ccall unsafe "_floorq" c_floorq :: F1
foreign import ccall unsafe "_ceilq" c_ceilq :: F1

foreign import ccall unsafe "_isnanq" c_isnanq :: TST
foreign import ccall unsafe "_isinfq" c_isinfq :: TST
foreign import ccall unsafe "_isnegzeroq" c_isnegzeroq :: TST
foreign import ccall unsafe "_isdenormq" c_isdenormq :: TST
foreign import ccall unsafe "_frexpq" c_frexpq :: Ptr Float128 -> Ptr Float128 -> Ptr Int32 -> IO ()
foreign import ccall unsafe "_ldexpq" c_ldexpq :: Ptr Float128 -> Ptr Float128 -> Int32 -> IO ()
foreign import ccall unsafe "_atan2q" c_atan2q :: F2

foreign import ccall unsafe "_ltq" c_ltq :: CMP
foreign import ccall unsafe "_ltq" c_gtq :: CMP
foreign import ccall unsafe "_ltq" c_leq :: CMP
foreign import ccall unsafe "_ltq" c_geq :: CMP
foreign import ccall unsafe "_fminq" c_fminq :: F2
foreign import ccall unsafe "_fmaxq" c_fmaxq :: F2

-- foreign import ccall unsafe "_readq" c_readq :: Ptr Float128 -> Ptr CChar -> IO ()
-- foreign import ccall unsafe "_showq" c_showq :: Ptr CChar -> CSize -> Ptr Float128 -> IO ()

-- SEE: [linking to .c files]
--
runQ $ do
  addForeignFilePath LangC "cbits/float128.c"
  return []

#else

c_addq :: F2
c_addq = not_enabled

c_subq :: F2
c_subq = not_enabled

c_mulq :: F2
c_mulq = not_enabled

c_absq :: F1
c_absq = not_enabled

c_negateq :: F1
c_negateq = not_enabled

c_signumq :: F1
c_signumq = not_enabled

c_divq :: F2
c_divq = not_enabled

c_recipq :: F1
c_recipq = not_enabled

c_piq :: Ptr Float128 -> IO ()
c_piq = not_enabled

c_expq :: F1
c_expq = not_enabled

c_logq :: F1
c_logq = not_enabled

c_sqrtq :: F1
c_sqrtq = not_enabled

c_powq :: F2
c_powq = not_enabled

c_sinq :: F1
c_sinq = not_enabled

c_cosq :: F1
c_cosq = not_enabled

c_tanq :: F1
c_tanq = not_enabled

c_asinq :: F1
c_asinq = not_enabled

c_acosq :: F1
c_acosq = not_enabled

c_atanq :: F1
c_atanq = not_enabled

c_sinhq :: F1
c_sinhq = not_enabled

c_coshq :: F1
c_coshq = not_enabled

c_tanhq :: F1
c_tanhq = not_enabled

c_asinhq :: F1
c_asinhq = not_enabled

c_acoshq :: F1
c_acoshq = not_enabled

c_atanhq :: F1
c_atanhq = not_enabled

c_log1pq :: F1
c_log1pq = not_enabled

c_expm1q :: F1
c_expm1q = not_enabled

c_roundq :: F1
c_roundq = not_enabled

c_truncq :: F1
c_truncq = not_enabled

c_floorq :: F1
c_floorq = not_enabled

c_ceilq :: F1
c_ceilq = not_enabled

c_isnanq :: TST
c_isnanq = not_enabled

c_isinfq :: TST
c_isinfq = not_enabled

c_isnegzeroq :: TST
c_isnegzeroq = not_enabled

c_isdenormq :: TST
c_isdenormq = not_enabled

c_frexpq :: Ptr Float128 -> Ptr Float128 -> Ptr Int32 -> IO ()
c_frexpq = not_enabled

c_ldexpq :: Ptr Float128 -> Ptr Float128 -> Int32 -> IO ()
c_ldexpq = not_enabled

c_atan2q :: F2
c_atan2q = not_enabled

c_ltq :: CMP
c_ltq = not_enabled

c_gtq :: CMP
c_gtq = not_enabled

c_leq :: CMP
c_leq = not_enabled

c_geq :: CMP
c_geq = not_enabled

c_fminq :: F2
c_fminq = not_enabled

c_fmaxq :: F2
c_fmaxq = not_enabled

-- c_readq :: Ptr Float128 -> Ptr CChar -> IO ()
-- c_readq = not_enabled
--
-- c_showq :: Ptr CChar -> CSize -> Ptr Float128 -> IO ()
-- c_showq = not_enabled

not_enabled :: a
not_enabled = error $
  unlines [ "128-bit floating point numbers are not enabled."
          , "Reinstall package 'accelerate' with '-ffloat128' to enable them."
          ]
#endif

