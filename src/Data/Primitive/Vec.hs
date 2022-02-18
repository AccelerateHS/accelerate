{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Primitive.Vec
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Primitive.Vec (

  -- * SIMD vector types
  Vec(..),
  Vec2, pattern Vec2,
  Vec3, pattern Vec3,
  Vec4, pattern Vec4,
  Vec8, pattern Vec8,
  Vec16, pattern Vec16,

  listOfVec,
  liftVec,

) where

import Control.Monad.ST
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Language.Haskell.TH.Extra
import Prettyprinter

import GHC.Base                                                     ( isTrue# )
import GHC.Int
import GHC.Prim
import GHC.TypeLits
import GHC.Word


-- Note: [Representing SIMD vector types]
--
-- A simple polymorphic representation of SIMD types such as the following:
--
-- > data Vec2 a = Vec2 !a !a
--
-- is not able to unpack the values into the constructor, meaning that
-- 'Vec2' is storing pointers to (strict) values on the heap, which is
-- a very inefficient representation.
--
-- We might try defining a data family instead so that we can get efficient
-- unboxed representations, and even make use of the unlifted SIMD types GHC
-- knows about:
--
-- > data family Vec2 a :: *
-- > data instance Vec2 Float    = Vec2_Float Float# Float#   -- reasonable
-- > data instance Vec2 Double   = Vec2_Double DoubleX2#      -- built in!
--
-- However, this runs into the problem that GHC stores all values as word sized
-- entities:
--
-- > data instance Vec2 Int      = Vec2_Int Int# Int#
-- > data instance Vec2 Int8     = Vec2_Int8 Int8# Int8#      -- Int8# does not exist; requires a full Int#
--
-- which, again, is very memory inefficient.
--
-- So, as a last resort, we'll just use a ByteArray# to ensure an efficient
-- packed representation.
--
-- One inefficiency of this approach is that the byte array does track its size,
-- which redundant for our use case (derivable from type level information).
--
data Vec (n :: Nat) a = Vec ByteArray#

type role Vec nominal representational

instance (Show a, Prim a, KnownNat n) => Show (Vec n a) where
  show = vec . listOfVec
    where
      vec :: [a] -> String
      vec = show
          . group . encloseSep (flatAlt "< " "<") (flatAlt " >" ">") ", "
          . map viaShow

listOfVec :: forall a n. (Prim a, KnownNat n) => Vec n a -> [a]
listOfVec (Vec ba#) = go 0#
  where
    go :: Int# -> [a]
    go i# | isTrue# (i# <# n#)  = indexByteArray# ba# i# : go (i# +# 1#)
          | otherwise           = []

    !(I# n#)  = fromIntegral (natVal' (proxy# :: Proxy# n))

instance Eq (Vec n a) where
  Vec ba1# == Vec ba2# = ByteArray ba1# == ByteArray ba2#

-- Type synonyms for common SIMD vector types
--
-- Note that non-power-of-two sized SIMD vectors are a bit dubious, and
-- special care must be taken in the code generator. For example, LLVM will
-- treat a Vec3 with alignment of _4_, meaning that reads and writes will
-- be (without further action) incorrect.
--
type Vec2 a  = Vec 2 a
type Vec3 a  = Vec 3 a
type Vec4 a  = Vec 4 a
type Vec8 a  = Vec 8 a
type Vec16 a = Vec 16 a

pattern Vec2 :: Prim a => a -> a -> Vec2 a
pattern Vec2 a b <- (unpackVec2 -> (a,b))
  where Vec2 = packVec2
{-# COMPLETE Vec2 #-}

pattern Vec3 :: Prim a => a -> a -> a -> Vec3 a
pattern Vec3 a b c <- (unpackVec3 -> (a,b,c))
  where Vec3 = packVec3
{-# COMPLETE Vec3 #-}

pattern Vec4 :: Prim a => a -> a -> a -> a -> Vec4 a
pattern Vec4 a b c d <- (unpackVec4 -> (a,b,c,d))
  where Vec4 = packVec4
{-# COMPLETE Vec4 #-}

pattern Vec8 :: Prim a => a -> a -> a -> a -> a -> a -> a -> a -> Vec8 a
pattern Vec8 a b c d e f g h <- (unpackVec8 -> (a,b,c,d,e,f,g,h))
  where Vec8 = packVec8
{-# COMPLETE Vec8 #-}

pattern Vec16 :: Prim a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Vec16 a
pattern Vec16 a b c d e f g h i j k l m n o p <- (unpackVec16 -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p))
  where Vec16 = packVec16
{-# COMPLETE Vec16 #-}

unpackVec2 :: Prim a => Vec2 a -> (a,a)
unpackVec2 (Vec ba#) =
  ( indexByteArray# ba# 0#
  , indexByteArray# ba# 1#
  )

unpackVec3 :: Prim a => Vec3 a -> (a,a,a)
unpackVec3 (Vec ba#) =
  ( indexByteArray# ba# 0#
  , indexByteArray# ba# 1#
  , indexByteArray# ba# 2#
  )

unpackVec4 :: Prim a => Vec4 a -> (a,a,a,a)
unpackVec4 (Vec ba#) =
  ( indexByteArray# ba# 0#
  , indexByteArray# ba# 1#
  , indexByteArray# ba# 2#
  , indexByteArray# ba# 3#
  )

unpackVec8 :: Prim a => Vec8 a -> (a,a,a,a,a,a,a,a)
unpackVec8 (Vec ba#) =
  ( indexByteArray# ba# 0#
  , indexByteArray# ba# 1#
  , indexByteArray# ba# 2#
  , indexByteArray# ba# 3#
  , indexByteArray# ba# 4#
  , indexByteArray# ba# 5#
  , indexByteArray# ba# 6#
  , indexByteArray# ba# 7#
  )

unpackVec16 :: Prim a => Vec16 a -> (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
unpackVec16 (Vec ba#) =
  ( indexByteArray# ba# 0#
  , indexByteArray# ba# 1#
  , indexByteArray# ba# 2#
  , indexByteArray# ba# 3#
  , indexByteArray# ba# 4#
  , indexByteArray# ba# 5#
  , indexByteArray# ba# 6#
  , indexByteArray# ba# 7#
  , indexByteArray# ba# 8#
  , indexByteArray# ba# 9#
  , indexByteArray# ba# 10#
  , indexByteArray# ba# 11#
  , indexByteArray# ba# 12#
  , indexByteArray# ba# 13#
  , indexByteArray# ba# 14#
  , indexByteArray# ba# 15#
  )

packVec2 :: Prim a => a -> a -> Vec2 a
packVec2 a b = runST $ do
  mba <- newByteArray (2 * sizeOf a)
  writeByteArray mba 0 a
  writeByteArray mba 1 b
  ByteArray ba# <- unsafeFreezeByteArray mba
  return $! Vec ba#

packVec3 :: Prim a => a -> a -> a -> Vec3 a
packVec3 a b c = runST $ do
  mba <- newByteArray (3 * sizeOf a)
  writeByteArray mba 0 a
  writeByteArray mba 1 b
  writeByteArray mba 2 c
  ByteArray ba# <- unsafeFreezeByteArray mba
  return $! Vec ba#

packVec4 :: Prim a => a -> a -> a -> a -> Vec4 a
packVec4 a b c d = runST $ do
  mba <- newByteArray (4 * sizeOf a)
  writeByteArray mba 0 a
  writeByteArray mba 1 b
  writeByteArray mba 2 c
  writeByteArray mba 3 d
  ByteArray ba# <- unsafeFreezeByteArray mba
  return $! Vec ba#

packVec8 :: Prim a => a -> a -> a -> a -> a -> a -> a -> a -> Vec8 a
packVec8 a b c d e f g h = runST $ do
  mba <- newByteArray (8 * sizeOf a)
  writeByteArray mba 0 a
  writeByteArray mba 1 b
  writeByteArray mba 2 c
  writeByteArray mba 3 d
  writeByteArray mba 4 e
  writeByteArray mba 5 f
  writeByteArray mba 6 g
  writeByteArray mba 7 h
  ByteArray ba# <- unsafeFreezeByteArray mba
  return $! Vec ba#

packVec16 :: Prim a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Vec16 a
packVec16 a b c d e f g h i j k l m n o p = runST $ do
  mba <- newByteArray (16 * sizeOf a)
  writeByteArray mba 0 a
  writeByteArray mba 1 b
  writeByteArray mba 2 c
  writeByteArray mba 3 d
  writeByteArray mba 4 e
  writeByteArray mba 5 f
  writeByteArray mba 6 g
  writeByteArray mba 7 h
  writeByteArray mba 8 i
  writeByteArray mba 9 j
  writeByteArray mba 10 k
  writeByteArray mba 11 l
  writeByteArray mba 12 m
  writeByteArray mba 13 n
  writeByteArray mba 14 o
  writeByteArray mba 15 p
  ByteArray ba# <- unsafeFreezeByteArray mba
  return $! Vec ba#

-- O(n) at runtime to copy from the Addr# to the ByteArray#. We should be able
-- to do this without copying, but I don't think the definition of ByteArray# is
-- exported (or it is deeply magical).
--
liftVec :: Vec n a -> CodeQ (Vec n a)
liftVec (Vec ba#)
  = unsafeCodeCoerce
    [| runST $ \s ->
         case newByteArray# $(liftInt# n#) s                                             of { (# s1, mba# #) ->
         case copyAddrToByteArray# $(litE (StringPrimL bytes)) mba# 0# $(liftInt# n#) s1 of { s2             ->
         case unsafeFreezeByteArray# mba# s2                                             of { (# s3, ba'# #) ->
           (# s3, Vec ba'# #)
        }}}
     |]
  where
      bytes :: [Word8]
      bytes = go 0#
        where
          go i# | isTrue# (i# <# n#) = W8# (indexWord8Array# ba# i#) : go (i# +# 1#)
                | otherwise          = []

      n# = sizeofByteArray# ba#

      -- XXX: Typed TH does not support unlifted types
      --
      liftInt# :: Int# -> ExpQ
      liftInt# i# = litE (IntPrimL (toInteger (I# i#)))

