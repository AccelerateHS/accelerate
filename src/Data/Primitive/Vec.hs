{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Primitive.Vec
-- Copyright   : [2008..2022] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Primitive.Vec (

  -- * SIMD vector types
  Vec(..), KnownNat,
  V2, pattern V2,
  V3, pattern V3,
  V4, pattern V4,
  V8, pattern V8,
  V16, pattern V16,

  toList, fromList,
  extract, insert, splat,
  liftVec,

) where

import Data.Array.Accelerate.Error

import Control.Monad.ST
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Language.Haskell.TH.Extra
import Prettyprinter
import Prelude

import qualified Foreign.Storable                                   as Foreign

import GHC.Base                                                     ( isTrue# )
import GHC.Int
import GHC.Prim
import GHC.TypeLits
import GHC.Types                                                    ( IO(..) )
import GHC.Word
import qualified GHC.Exts                                           as GHC

#if __GLASGOW_HASKELL__ < 808
import GHC.Ptr
#endif


-- Note: [Representing SIMD vector types]
--
-- A simple polymorphic representation of SIMD types such as the following:
--
-- > data V2 a = V2 !a !a
--
-- is not able to unpack the values into the constructor, meaning that
-- 'V2' is storing pointers to (strict) values on the heap, which is
-- a very inefficient representation.
--
-- We might try defining a data family instead so that we can get efficient
-- unboxed representations, and even make use of the unlifted SIMD types GHC
-- knows about:
--
-- > data family V2 a :: *
-- > data instance V2 Float    = V2_Float Float# Float#   -- reasonable
-- > data instance V2 Double   = V2_Double DoubleX2#      -- built in!
--
-- However, this runs into the problem that GHC stores all values as word sized
-- entities:
--
-- > data instance V2 Int      = V2_Int Int# Int#
-- > data instance V2 Int8     = V2_Int8 Int8# Int8#      -- Int8# does not exist; requires a full Int#
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
  show = vec . toList
    where
      vec :: [a] -> String
      vec = show
          . group . encloseSep (flatAlt "< " "<") (flatAlt " >" ">") ", "
          . map viaShow

instance (Prim a, KnownNat n) => GHC.IsList (Vec n a) where
  type Item (Vec n a) = a
  {-# INLINE toList   #-}
  {-# INLINE fromList #-}
  toList   = toList
  fromList = fromList

instance (Foreign.Storable a, KnownNat n) => Foreign.Storable (Vec n a) where
  {-# INLINE sizeOf    #-}
  {-# INLINE alignment #-}
  {-# INLINE peek      #-}
  {-# INLINE poke      #-}

  sizeOf _    = fromInteger (natVal' (proxy# :: Proxy# n)) * Foreign.sizeOf (undefined :: a)
  alignment _ = Foreign.alignment (undefined :: a)

  peek (Ptr addr#) =
    IO $ \s0 ->
      case Foreign.sizeOf (undefined :: Vec n a)        of { I# bytes#      ->
      case newAlignedPinnedByteArray# bytes# 16# s0     of { (# s1, mba# #) ->
      case copyAddrToByteArray# addr# mba# 0# bytes# s1 of { s2             ->
      case unsafeFreezeByteArray# mba# s2               of { (# s3, ba# #)  ->
        (# s3, Vec ba# #)
      }}}}

  poke (Ptr addr#) (Vec ba#) =
    IO $ \s0 ->
      case Foreign.sizeOf (undefined :: Vec n a)       of { I# bytes# ->
      case copyByteArrayToAddr# ba# 0# addr# bytes# s0 of {
        s1 -> (# s1, () #)
    }}

{-# INLINE toList #-}
toList :: forall a n. (Prim a, KnownNat n) => Vec n a -> [a]
toList (Vec ba#) = go 0#
  where
    go :: Int# -> [a]
    go i# | isTrue# (i# <# n#)  = indexByteArray# ba# i# : go (i# +# 1#)
          | otherwise           = []
    --
    !(I# n#)  = fromInteger (natVal' (proxy# :: Proxy# n))

{-# INLINE fromList #-}
fromList :: forall a n. (Prim a, KnownNat n) => [a] -> Vec n a
fromList xs =
  case byteArrayFromListN (fromInteger (natVal' (proxy# :: Proxy# n))) xs of
    ByteArray ba# -> Vec ba#

instance Eq (Vec n a) where
  Vec ba1# == Vec ba2# = ByteArray ba1# == ByteArray ba2#

-- | Extract an element from a vector at the given index
--
{-# INLINE extract #-}
extract :: forall n a. (Prim a, KnownNat n) => Vec n a -> Int -> a
extract (Vec ba#) i@(I# i#) =
  let n = fromInteger (natVal' (proxy# :: Proxy# n))
   in boundsCheck "out of range" (i >= 0 && i < n) $ indexByteArray# ba# i#

-- | Returns a new vector where the element at the specified index has been
-- replaced with the supplied value.
--
{-# INLINE insert #-}
insert :: forall n a. (Prim a, KnownNat n) => Vec n a -> Int -> a -> Vec n a
insert (Vec ba#) i a =
  let n     = fromInteger (natVal' (proxy# :: Proxy# n))
      bytes = n * sizeOf (undefined :: a)
   in boundsCheck "out of range" (i >= 0 && i < n)
    $ runST $ do
        mba <- newByteArray bytes
        copyByteArray mba 0 (ByteArray ba#) 0 bytes
        writeByteArray mba i a
        ByteArray ba'# <- unsafeFreezeByteArray mba
        return $! Vec ba'#

-- | Fill all lanes of a vector with the same value
--
{-# INLINE splat #-}
splat :: forall n a. (Prim a, KnownNat n) => a -> Vec n a
splat x = runST $ do
  let n = fromInteger (natVal' (proxy# :: Proxy# n))
  mba <- newByteArray (n * sizeOf (undefined :: a))
  setByteArray mba 0 n x
  ByteArray ba# <- unsafeFreezeByteArray mba
  return $! Vec ba#

-- Type synonyms for common SIMD vector types
--
-- Note that non-power-of-two sized SIMD vectors are a bit dubious, and
-- special care must be taken in the code generator. For example, LLVM will
-- treat a V3 with alignment of _4_, meaning that reads and writes will
-- be (without further action) incorrect.
--
type V2 a  = Vec 2 a
type V3 a  = Vec 3 a
type V4 a  = Vec 4 a
type V8 a  = Vec 8 a
type V16 a = Vec 16 a

pattern V2 :: Prim a => a -> a -> V2 a
pattern V2 a b <- (unpackVec2 -> (a,b))
  where V2 = packVec2
{-# COMPLETE V2 #-}

pattern V3 :: Prim a => a -> a -> a -> V3 a
pattern V3 a b c <- (unpackVec3 -> (a,b,c))
  where V3 = packVec3
{-# COMPLETE V3 #-}

pattern V4 :: Prim a => a -> a -> a -> a -> V4 a
pattern V4 a b c d <- (unpackVec4 -> (a,b,c,d))
  where V4 = packVec4
{-# COMPLETE V4 #-}

pattern V8 :: Prim a => a -> a -> a -> a -> a -> a -> a -> a -> V8 a
pattern V8 a b c d e f g h <- (unpackVec8 -> (a,b,c,d,e,f,g,h))
  where V8 = packVec8
{-# COMPLETE V8 #-}

pattern V16 :: Prim a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> V16 a
pattern V16 a b c d e f g h i j k l m n o p <- (unpackVec16 -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p))
  where V16 = packVec16
{-# COMPLETE V16 #-}

unpackVec2 :: Prim a => V2 a -> (a,a)
unpackVec2 (Vec ba#) =
  ( indexByteArray# ba# 0#
  , indexByteArray# ba# 1#
  )

unpackVec3 :: Prim a => V3 a -> (a,a,a)
unpackVec3 (Vec ba#) =
  ( indexByteArray# ba# 0#
  , indexByteArray# ba# 1#
  , indexByteArray# ba# 2#
  )

unpackVec4 :: Prim a => V4 a -> (a,a,a,a)
unpackVec4 (Vec ba#) =
  ( indexByteArray# ba# 0#
  , indexByteArray# ba# 1#
  , indexByteArray# ba# 2#
  , indexByteArray# ba# 3#
  )

unpackVec8 :: Prim a => V8 a -> (a,a,a,a,a,a,a,a)
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

unpackVec16 :: Prim a => V16 a -> (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
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

packVec2 :: Prim a => a -> a -> V2 a
packVec2 a b = runST $ do
  mba <- newByteArray (2 * sizeOf a)
  writeByteArray mba 0 a
  writeByteArray mba 1 b
  ByteArray ba# <- unsafeFreezeByteArray mba
  return $! Vec ba#

packVec3 :: Prim a => a -> a -> a -> V3 a
packVec3 a b c = runST $ do
  mba <- newByteArray (3 * sizeOf a)
  writeByteArray mba 0 a
  writeByteArray mba 1 b
  writeByteArray mba 2 c
  ByteArray ba# <- unsafeFreezeByteArray mba
  return $! Vec ba#

packVec4 :: Prim a => a -> a -> a -> a -> V4 a
packVec4 a b c d = runST $ do
  mba <- newByteArray (4 * sizeOf a)
  writeByteArray mba 0 a
  writeByteArray mba 1 b
  writeByteArray mba 2 c
  writeByteArray mba 3 d
  ByteArray ba# <- unsafeFreezeByteArray mba
  return $! Vec ba#

packVec8 :: Prim a => a -> a -> a -> a -> a -> a -> a -> a -> V8 a
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

packVec16 :: Prim a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> V16 a
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

