{-
  Copyright (C) 2010 by IPwn Studios
  Released under GNU General Public License v3
-}

{-# LANGUAGE ScopedTypeVariables #-}
module RasterizeAcc where

import Control.Arrow
import Control.Parallel
import Control.Parallel.Strategies
import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter
import Data.Bits ((.&.))
import Prelude   hiding (replicate, zip, unzip, map, filter, max, min, not,
    zipWith, round, uncurry, scanl, fst, snd, tail, even)
import qualified Prelude
import Debug.Trace


type Area = ((Int, Int), (Int, Int))
type Value = ((Int, Int), Float)
type Insertion = ((Int, Int), Float)
type Facet = (Int, Int, Int)

aseq :: (NFData a, Elem a, Ix dim) => String -> Acc (Array dim a) -> Acc (Array dim a)
aseq descr = id -- use . trace descr . hseq . run

aseq1 :: (NFData a, Elem a, Ix dim, Show a) => String -> Acc (Array dim a) -> Acc (Array dim a)
aseq1 descr aarr =
    let arr = run aarr
    in  use $ {-trace (descr ++ " " ++ show (arrayShape arr) ++ " " ++ show arr)-} arr

hseq :: (NFData a, Elem a, Ix dim) => Array dim a -> Array dim a
hseq arr =
    let l = toList arr `using` seqList rwhnf
    in  l `seq` fromList (arrayShape arr) l

rasterize :: Exp Area
          -> Acc (Vector Value)
          -> Acc (Vector Facet) 
          -> Acc (Array DIM2 Float)
rasterize area values facets =
    let -- Triangle vertices sorted by X co-ordinate
        tris :: Acc (Vector (Value, Value, Value))
        tris = flip map facets $ \tri ->
            let (iA, iB, iC) = untuple tri
            in  sort3Tuple lessThanX $ tuple (values ! iA, values ! iB, values ! iC)

        noOfTris = shape tris :: Exp Int

        -- Vector of (triangle index, (start column, end column))
        columnRanges :: Acc (Vector (Int, Int))
        columnRanges = aseq1 "columnRanges" $ map rangeOfColumns tris

        -- Vector of (triangle index, integer x co-ordinate of column)
        columnIndices :: Acc (Vector (Int, Int))
        columnIndices = aseq1 "columnIndices" $ flatten columnRanges

        columns :: Acc (Vector ((Int, Float, Float), (Int, Float, Float)))
        columns = aseq1 "columns" $ flip map columnIndices $ \pair ->
            let (triIx, xInt) = untuple pair
            in  rowRange (tris ! triIx) xInt

        pointRanges :: Acc (Vector (Int, Int))
        pointRanges = aseq1 "pointRanges" $ flip map columns $ \col ->
            let ((yStart, _, _), (yEnd, _, _)) = (untuple *** untuple) $ untuple col
                    :: ((Exp Int, Exp Float, Exp Float), (Exp Int, Exp Float, Exp Float))
            in  tuple (yStart, yEnd)

        -- Vector of (column index, integer y co-ordinate of column)
        pointIndices :: Acc (Vector (Int, Int))
        pointIndices = aseq1 "pointIndices" $ flatten pointRanges

        insertions :: Acc (Vector Insertion)
        insertions = aseq1 "insertions" $ flip map pointIndices $ \pi ->
            let (colIx, yInt) = untuple pi
                (triIx, xInt) = untuple $ columnIndices ! colIx
                    :: (Exp Int, Exp Int)
                ((_, yBot, vBot), (_, yTop, vTop)) =
                    (untuple *** untuple) $ untuple $ columns ! colIx
                    :: ((Exp Int, Exp Float, Exp Float), (Exp Int, Exp Float, Exp Float))
                -- Displace odd columns because the terrain mesh is hexagonal
                hexify yInt = even xInt ? (intToFloat yInt, intToFloat yInt + 0.5)
                v = interpolate (yBot, vBot) (yTop, vTop) (hexify yInt)
            in  tuple (tuple (xInt, yInt), v)
    in  toArray area insertions
  where
    ((x0,y0),(x1,y1)) = (untuple *** untuple) $ untuple area  :: ((Exp Int, Exp Int), (Exp Int, Exp Int))

    -- Give the start and end of the Y value of the specified row in both
    -- floating point and integer co-ordinates.  Output is a pair of
    -- (integer y, float y, value)
    rowRange :: Exp (Value, Value, Value) -> Exp Int -> Exp ((Int, Float, Float), (Int, Float, Float))
    rowRange tri xInt =
        let (a, b, c) = untuple tri
            ((axInt, ayInt), vA) = (untuple *** id) (untuple a) :: ((Exp Int, Exp Int), Exp Float)
            ((bxInt, byInt), vB) = (untuple *** id) (untuple b) :: ((Exp Int, Exp Int), Exp Float)
            ((cxInt, cyInt), vC) = (untuple *** id) (untuple c) :: ((Exp Int, Exp Int), Exp Float)
            ax = intToFloat axInt
            ay = intToFloat ayInt
            bx = intToFloat bxInt
            by = intToFloat byInt
            cx = intToFloat cxInt
            cy = intToFloat cyInt
            x = intToFloat xInt
            yAC = interpolate (ax, ay) (cx, cy) x :: Exp Float
            vAC = interpolate (ax, vA) (cx, vC) x :: Exp Float
            (yOther, vOther) = untuple (
                    x <* bx ? (tuple (interpolate (ax,ay) (bx,by) x, interpolate (ax,vA) (bx,vB) x),
                               tuple (interpolate (bx,by) (cx,cy) x, interpolate (bx,vB) (cx,vC) x))
                ) :: (Exp Float, Exp Float)
            -- Order AC and Other so that y is increasing
            (yBot, vBot, yTop, vTop) = untuple (
                    yAC <=* yOther ? (tuple (yAC, vAC, yOther, vOther),
                                      tuple (yOther, vOther, yAC, vAC))
                ) :: (Exp Float, Exp Float, Exp Float, Exp Float)
            yStart = max y0 (roundFloatToInt yBot)
            yEnd   = min (y1+1) (roundFloatToInt yTop)
        in  tuple (tuple (yStart, yBot, vBot), tuple (yEnd, yTop, vTop))

    rangeOfColumns :: Exp (Value, Value, Value) -> Exp (Int, Int)
    rangeOfColumns tri =
        let (a, _, c) = untuple tri :: (Exp Value, Exp Value, Exp Value)
            (xyA, _) = untuple a    :: (Exp (Int, Int), Exp Float)
            (xA, _)  = untuple xyA  :: (Exp Int, Exp Int)
            (xyC, _) = untuple c    :: (Exp (Int, Int), Exp Float)
            (xC, _)  = untuple xyC  :: (Exp Int, Exp Int)
        in  tuple (x0 `max` xA, (x1 + 1) `min` xC)

-- | Flatten a list of ranges with an exclusive upper value into a list of
-- indices into the concatenated ranges in the form (input ix, value in range). e.g.
--
-- > [(5,8),(10,15),(3,4)]
--
-- > [(0,5),(0,6),(0,7),(1,10),(1,11),(1,12),(1,13),(1,14),(2,3)]
flatten :: Acc (Vector (Int, Int))
        -> Acc (Vector (Int, Int))
flatten ranges0 =
    let -- ensure the ranges are not decreasing
        ranges = flip map ranges0 $ \ab ->
            let (a, b) = untuple ab
            in  tuple (a, a `max` b)
        noOfRanges = shape ranges :: Exp Int

        rangeWidths :: Acc (Vector Int)
        rangeWidths = aseq1 "f.rangeWidths" $ map (uncurry $ flip (-)) ranges

        -- The start position of each column in a yet-to-be-created array of columns
        (columnOffsets, noOfCols) = second (! constant ()) $ scanl (+) 0 $ rangeWidths
            :: (Acc (Vector Int), Exp Int)

        -- Input indices for each of the output elements, e.g. for the example
        -- above, [0,0,0,1,1,1,1,1,2]
        rangeIndices :: Acc (Vector Int)
        rangeIndices = aseq1 "f.rangeIndices" $
            let ones = replicate noOfRanges (unit 1) :: Acc (Vector Int)
                zeroes = replicate (noOfCols + 1) (unit 0) :: Acc (Vector Int)
                steps = permute (+) zeroes (columnOffsets !) ones
            in  tail $ Prelude.fst $ scanl (+) (-1) steps

        columnIndices :: Acc (Vector Int)
        columnIndices = aseq1 "f.columnIndices" $
            --   [10 - 8 + 1, 3 - 15 + 1] 
            --   [3, -11]
            -- 5 
            let ones = use $ run $ replicate (noOfCols + 1) (unit 1) :: Acc (Vector Int)
                -- ([5,10,3], [8,15,4]) 
                (rangeStarts, rangeEnds) = unzip ranges
                -- [3, -11]
                deltas = use $ run $ map (1+) $ zipWith (-) (tail rangeStarts) rangeEnds
                deltas_m1 = map (\x -> x - 1) deltas
                -- [1, 1, 1, 3, 1, 1, 1, 1,-11, 1]
                changes = use $ run $ permute (+) ones (\ix -> columnOffsets ! (ix + 1)) deltas_m1
            --   [5, 6, 7,10,11,12,13, 14, 3]
            in  tail $ Prelude.fst $ scanl (+) ((rangeStarts ! 0) - 1) changes

    in  zip rangeIndices columnIndices

tail :: Elem a => Acc (Vector a) -> Acc (Vector a)
tail xs = backpermute (shape xs - 1) (1+) xs

even :: Exp Int -> Exp Bool
even x = (x .&. 1) ==* 0

interpolate :: (Exp Float, Exp Float) -> (Exp Float, Exp Float) -> Exp Float -> Exp Float
interpolate (t0, x0) (t1, x1) t = (t - t0) * (x1 - x0) / (t1 - t0) + x0

-- | Give an array of values [0..length xs-1], e.g. [8,2,3,4] -> [0,1,2,3]
indicesOf :: Elem a => Acc (Vector a) -> Acc (Vector Int)
indicesOf = Prelude.fst . scanl (+) 0 . map (const 1)

lessThanX :: Exp Value -> Exp Value -> Exp Bool
lessThanX a b =
    let (xyA, _) = untuple a   :: (Exp (Int, Int), Exp Float)
        (xA, _)  = untuple xyA :: (Exp Int, Exp Int)
        (xyB, _) = untuple b   :: (Exp (Int, Int), Exp Float)
        (xB, _)  = untuple xyB :: (Exp Int, Exp Int)
    in  xA <* xB

sort3Tuple :: forall a . (Elem a) =>
              (Exp a -> Exp a -> Exp Bool)   -- ^ Less-than function
           -> Exp (a, a, a)                  -- ^ Tuple
           -> Exp (a, a, a)
sort3Tuple lessThan = shuttle1 . untuple
  where
    -- Shuttle sort
    shuttle1 :: (Exp a, Exp a, Exp a) -> Exp (a, a, a)
    shuttle1 (a, b, c) = a `lessThan` b ? (shuttle2 (a, b, c), shuttle2 (b, a, c))

    shuttle2 :: (Exp a, Exp a, Exp a) -> Exp (a, a, a)
    shuttle2 (a, b, c) = b `lessThan` c ? (tuple (a, b, c), shuttle3 (a, c, b))

    shuttle3 :: (Exp a, Exp a, Exp a) -> Exp (a, a, a)
    shuttle3 (a, b, c) = a `lessThan` b ? (tuple (a, b, c), tuple (b, a, c))

prop_sort3Tuple :: (Int, Int, Int) -> Bool
prop_sort3Tuple triple =
    let (a, b, c) = head $ toList $ run (map (sort3Tuple (<*)) $ unit $ constant triple)
    in  a <= b && b <= c

toArray :: Exp Area
        -> Acc (Vector Insertion)
        -> Acc (Array DIM2 Float)
toArray area insertions =
    let (bl, tr) = untuple area
        (x0, y0) = untuple bl
        (x1, y1) = untuple tr
        w = x1 - x0 + 1
        h = y1 - y0 + 1 
        z = zeroArray (tuple (h, w))
        (ixs, values) = unzip insertions
        perm :: Exp Int -> Exp (Int, Int)
        perm ix =
            let (x, y) = untuple (ixs ! ix)
            in
                x >=* x0 &&* x <=* x1 &&*
                y >=* y0 &&* y <=* y1
                    ? (tuple (y - y0, x - x0), ignore)
    in  permute const z perm values

zeroArray :: Exp (Int, Int) -> Acc (Array DIM2 Float)
zeroArray dim = replicate dim (unit 0)

