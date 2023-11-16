{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Representation.Array
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Representation.Array
  where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.Representation.Shape                   hiding ( zip )
import Data.Array.Accelerate.Representation.Type

import Data.List                                                    ( intersperse )
import Data.Maybe                                                   ( isJust )
import Formatting
import Language.Haskell.TH.Extra
import System.IO.Unsafe
import Text.Show                                                    ( showListWith )
import Prelude                                                      hiding ( (!!) )
import qualified Data.Vector.Unboxed                                as U


-- | Array data type, where the type arguments regard the representation
-- types of the shape and elements.
--
data Array sh e where
  Array :: sh                         -- extent of dimensions = shape
        -> ArrayData e                -- array payload
        -> Array sh e

-- | Segment descriptor (vector of segment lengths).
--
-- To represent nested one-dimensional arrays, we use a flat array of data
-- values in conjunction with a /segment descriptor/, which stores the lengths
-- of the subarrays.
--
type Segments = Vector

type Scalar = Array DIM0    -- ^ A singleton array with one element
type Vector = Array DIM1    -- ^ A one-dimensional array
type Matrix = Array DIM2    -- ^ A two-dimensional array

-- | Type witnesses shape and data layout of an array
--
data ArrayR a where
  ArrayR :: { arrayRshape :: ShapeR sh
            , arrayRtype  :: TypeR e
            }
         -> ArrayR (Array sh e)

type ArraysR = TupR ArrayR

instance Show (ArrayR a) where
  show (ArrayR shR eR) = "Array DIM" ++ show (rank shR) ++ " " ++ show eR

formatArrayR :: Format r (ArrayR a -> r)
formatArrayR = later $ \case
  ArrayR shR eR -> bformat ("Array DIM" % int % " " % formatTypeR) (rank shR) eR

formatArraysR :: Format r (TupR ArrayR e -> r)
formatArraysR = later $ \case
  TupRunit         -> "()"
  TupRsingle aR    -> bformat formatArrayR aR
  TupRpair aR1 aR2 -> bformat (parenthesised (formatArraysR % "," % formatArraysR)) aR1 aR2

showArraysR :: ArraysR a -> ShowS
showArraysR = shows

arraysRarray :: ShapeR sh -> TypeR e -> ArraysR (Array sh e)
arraysRarray shR eR = TupRsingle (ArrayR shR eR)

arraysRpair :: ArrayR a -> ArrayR b -> ArraysR (((), a), b)
arraysRpair a b = TupRunit `TupRpair` TupRsingle a `TupRpair` TupRsingle b

-- | Creates a new, uninitialized Accelerate array.
--
allocateArray :: ArrayR (Array sh e) -> sh -> IO (Array sh e)
allocateArray (ArrayR shR eR) sh = do
  adata  <- newArrayData eR (fromIntegral (size shR sh))
  return $! Array sh adata

-- | Create an array from its representation function, applied at each
-- index of the array.
--
fromFunction :: ArrayR (Array sh e) -> sh -> (sh -> e) -> Array sh e
fromFunction repr sh f = unsafePerformIO $! fromFunctionM repr sh (return . f)

-- | Create an array using a monadic function applied at each index.
--
-- @since 1.2.0.0
--
fromFunctionM :: ArrayR (Array sh e) -> sh -> (sh -> IO e) -> IO (Array sh e)
fromFunctionM (ArrayR shR eR) sh f = do
  let !n = size shR sh
  arr <- newArrayData eR (fromIntegral n)
  --
  let write !i
        | i >= n    = return ()
        | otherwise = do
            v <- f (fromIndex shR sh i)
            writeArrayData eR arr (fromIntegral i) v
            write (i+1)
  --
  write 0
  return $! arr `seq` Array sh arr


-- | Convert a list into an Accelerate 'Array' in dense row-major order.
--
fromList :: forall sh e. ArrayR (Array sh e) -> sh -> [e] -> Array sh e
fromList (ArrayR shR eR) sh xs = adata `seq` Array sh adata
  where
    -- Assume the array is in dense row-major order. This is safe because
    -- otherwise backends would not be able to directly memcpy.
    --
    !n    = fromIntegral (size shR sh)
    (adata, _) = runArrayData @e $ do
                  arr <- newArrayData eR n
                  let go !i _ | i >= n = return ()
                      go !i (v:vs)     = writeArrayData eR arr i v >> go (i+1) vs
                      go _  []         = error "Data.Array.Accelerate.fromList: not enough input data"
                  --
                  go 0 xs
                  return (arr, undefined)


-- | Convert an accelerated 'Array' to a list in row-major order.
--
toList :: ArrayR (Array sh e) -> Array sh e -> [e]
toList (ArrayR shR eR) (Array sh adata) = go 0
  where
    -- Assume underling array is in row-major order. This is safe because
    -- otherwise backends would not be able to directly memcpy.
    --
    !n                  = fromIntegral (size shR sh)
    go !i | i >= n      = []
          | otherwise   = indexArrayData eR adata i : go (i+1)

-- concatVectors :: forall e. TypeR e -> [Vector e] -> Vector e
-- concatVectors tR vs = adata `seq` Array ((), len) adata
--   where
--     offsets     = scanl (+) 0 (map (size dim1 . shape) vs)
--     len         = last offsets
--     (adata, _)  = runArrayData @e $ do
--       arr <- newArrayData tR len
--       sequence_ [ writeArrayData tR arr (i + k) (indexArrayData tR ad i)
--                 | (Array ((), n) ad, k) <- vs `zip` offsets
--                 , i <- [0 .. n - 1] ]
--       return (arr, undefined)

shape :: Array sh e -> sh
shape (Array sh _) = sh

reshape :: HasCallStack => ShapeR sh -> sh -> ShapeR sh' -> Array sh' e -> Array sh e
reshape shR sh shR' (Array sh' adata)
  = boundsCheck "shape mismatch" (size shR sh == size shR' sh')
  $ Array sh adata

(!) :: (ArrayR (Array sh e), Array sh e) -> sh -> e
(!) = uncurry indexArray

(!!) :: (TypeR e, Array sh e) -> INT -> e
(!!) = uncurry linearIndexArray

indexArray :: ArrayR (Array sh e) -> Array sh e -> sh -> e
indexArray (ArrayR shR adR) (Array sh adata) ix = indexArrayData adR adata (fromIntegral (toIndex shR sh ix))

linearIndexArray :: TypeR e -> Array sh e -> INT -> e
linearIndexArray adR (Array _ adata) = indexArrayData adR adata . fromIntegral

showArray :: (e -> ShowS) -> ArrayR (Array sh e) -> Array sh e -> String
showArray f arrR@(ArrayR shR _) arr@(Array sh _) = case shR of
  ShapeRz                         -> "Scalar Z "                       ++ xs
  ShapeRsnoc ShapeRz              -> "Vector (" ++ shapeString ++ ") " ++ xs
  ShapeRsnoc (ShapeRsnoc ShapeRz) -> "Matrix (" ++ shapeString ++ ") " ++ showMatrix f arrR arr
  _                               -> "Array ("  ++ shapeString ++ ") " ++ xs
  where
    shapeString = showShape shR sh
    xs          = showListWith f (toList arrR arr) ""

showArrayShort :: Int -> (e -> ShowS) -> ArrayR (Array sh e) -> Array sh e -> String
showArrayShort n f arrR arr = '[' : go 0 (toList arrR arr)
  where
    go _ []       = "]"
    go i (x:xs)
      | i >= n    = " ..]"
      | otherwise = ',' : f x (go (i+1) xs)

-- TODO: Make special formatting optional? It is more difficult to
-- copy/paste the result, for example. Also it does not look good if the
-- matrix row does not fit on a single line.
--
showMatrix :: (e -> ShowS) -> ArrayR (Array DIM2 e) -> Array DIM2 e -> String
showMatrix f (ArrayR _ arrR) arr@(Array sh _)
  | rows * cols == 0 = "[]"
  | otherwise        = "\n  [" ++ ppMat 0 0
    where
      (((), ri), ci) = sh
      rows           = fromIntegral ri
      cols           = fromIntegral ci
      lengths        = U.generate (rows*cols) (\i -> length (f (linearIndexArray arrR arr (fromIntegral i)) ""))
      widths         = U.generate cols (\c -> U.maximum (U.generate rows (\r -> lengths U.! (r*cols+c))))
      --
      ppMat :: Int -> Int -> String
      ppMat !r !c | c >= cols = ppMat (r+1) 0
      ppMat !r !c             =
        let
            !i    = r*cols+c
            !l    = lengths U.! i
            !w    = widths  U.! c
            !pad  = 1
            cell  = replicate (w-l+pad) ' ' ++ f (linearIndexArray arrR arr (fromIntegral i)) ""
            --
            before
              | r > 0 && c == 0 = "\n   "
              | otherwise       = ""
            --
            after
              | r >= rows-1 && c >= cols-1 = "]"
              | otherwise                  = ',' : ppMat r (c+1)
        in
        before ++ cell ++ after

showArrays :: ArraysR arrs -> arrs -> String
showArrays repr arrs = showsArrays repr arrs ""

showsArrays :: ArraysR arrs -> arrs -> ShowS
showsArrays repr arrs = go 0 repr arrs
  where
    go :: Int -> ArraysR a -> a -> ShowS
    go _     TupRunit                ()
      = showString "()"
    go level repr' arrs'
      | Just tuple <- extractTuple repr' arrs'
      = let
          constructor = 'T' : show (length tuple)
          level' = level + length constructor + 1
          content = intersperse (('\n' :) . indent level') $ map ($ level') tuple
        in
          showString constructor . (' ' :) . foldr (flip (.)) id content
    go level (TupRpair repr1 repr2)  (arrs1, arrs2)
      = showString "( " . go (level + 2) repr1 arrs1 . showString "\n"
      . indent level . showString ", " . go (level + 1) repr2 arrs2 . showString "\n"
      . indent level . showString ")"
    go level (TupRsingle r@ArrayR{}) arr
      = showString $ indents level $ showArray (showsElt $ arrayRtype r) r arr

    indent :: Int -> ShowS
    indent 0 str = str
    indent n str = ' ' : indent (n - 1) str

    indents :: Int -> String -> String
    indents _     []          = []
    indents level ('\n' : xs) = '\n' : indent level (indents level xs)
    indents level (x    : xs) = x : indents level xs

    -- Tries to extract the representation of a tuple.
    -- Tuples are represented as a snoc-list built with
    -- pairs and nil.
    -- The tuple is returned a list of pretty-printed
    -- elements, in reverse order.
    extractTuple :: ArraysR a -> a -> Maybe [Int -> ShowS]
    extractTuple TupRunit        ()      = Just []
    extractTuple (TupRpair rs r) (as, a) = (current :) <$> extractTuple rs as
      where
        current level
          -- Avoid duplicate parentheses for () and pairs which don't form a tuple
          | needsParens r a = showString "( " . go (level + 2) r a . showString " )"
          | otherwise       = go level r a
    extractTuple _               _       = Nothing

    needsParens :: ArraysR a -> a -> Bool
    needsParens TupRunit _ = False
    needsParens repr'@(TupRpair _ _) as = isJust $ extractTuple repr' as
    needsParens _ _ = True

reduceRank :: ArrayR (Array (sh, INT) e) -> ArrayR (Array sh e)
reduceRank (ArrayR (ShapeRsnoc shR) aeR) = ArrayR shR aeR

rnfArray :: ArrayR a -> a -> ()
rnfArray (ArrayR shR adR) (Array sh ad) = rnfShape shR sh `seq` rnfArrayData adR ad

rnfArrayR :: ArrayR arr -> ()
rnfArrayR (ArrayR shR tR) = rnfShapeR shR `seq` rnfTupR rnfScalarType tR

rnfArraysR :: ArraysR arrs -> arrs -> ()
rnfArraysR TupRunit           ()      = ()
rnfArraysR (TupRsingle arrR)  arr     = rnfArray arrR arr
rnfArraysR (TupRpair aR1 aR2) (a1,a2) = rnfArraysR aR1 a1 `seq` rnfArraysR aR2 a2

liftArrayR :: ArrayR a -> CodeQ (ArrayR a)
liftArrayR (ArrayR shR tR) = [|| ArrayR $$(liftShapeR shR) $$(liftTypeR tR) ||]

liftArraysR :: ArraysR arrs -> CodeQ (ArraysR arrs)
liftArraysR TupRunit          = [|| TupRunit ||]
liftArraysR (TupRsingle repr) = [|| TupRsingle $$(liftArrayR repr) ||]
liftArraysR (TupRpair a b)    = [|| TupRpair $$(liftArraysR a) $$(liftArraysR b) ||]

liftArray :: forall sh e. ArrayR (Array sh e) -> Array sh e -> CodeQ (Array sh e)
liftArray (ArrayR shR adR) (Array sh adata) =
  [|| Array $$(liftElt (shapeType shR) sh) $$(liftArrayData sz adR adata) ||] `at` [t| Array $(liftTypeQ (shapeType shR)) $(liftTypeQ adR) |]
  where
    sz = fromIntegral (size shR sh)

    at :: CodeQ t -> Q Type -> CodeQ t
    at e t = unsafeCodeCoerce $ sigE (unTypeCode e) t

