{-
  Copyright (C) 2010 by IPwn Studios
  Released under GNU General Public License v3
-}
import qualified RasterizeAcc

import Control.Applicative
import Control.Arrow
import Data.Array (Array)
import Data.Array.IArray as A
import qualified Data.Array.Accelerate as Acc
import qualified Data.Array.Accelerate.Interpreter as Acc
import Data.List
import Data.Ord
import Debug.Trace
import System.IO

main :: IO ()
main = do
    test rasterizeH "rasterize-test1.txt"
    test rasterizeH "rasterize-test2.txt"
    test rasterizeH "rasterize-test3.txt"
    test rasterizeH "rasterize-test4.txt"
    test rasterizeAI "rasterize-test1.txt"
    test rasterizeAI "rasterize-test2.txt"
    test rasterizeAI "rasterize-test3.txt"
    test rasterizeAI "rasterize-test4.txt"
  where
    rasterizeH  = (rasterize, "Haskell")
    rasterizeAI = (rasterizeAI_, "Accelerate interpreted")
    rasterizeAI_ :: Area -> [Value] -> [Facet] -> [Float]
    rasterizeAI_ area@((x0,y0),(x1,y1)) values facets =
        let acc = RasterizeAcc.rasterize 
                    (Acc.tuple (Acc.tuple (Acc.constant x0, Acc.constant y0),
                                Acc.tuple (Acc.constant x1, Acc.constant y1)))
                    (Acc.use $ Acc.fromList (length values) values)
                    (Acc.use $ Acc.fromList (length facets) facets)
            out = Acc.toList $ Acc.run acc
        in  out    
      where
        v2acc ((x, y), v) = Acc.tuple (Acc.tuple (Acc.constant x, Acc.constant y), Acc.constant v)
        f2acc (a, b, c)   = Acc.tuple (Acc.constant a, Acc.constant b, Acc.constant c)

test :: (Area -> [Insertion] -> [Facet] -> [Float], String) -> FilePath -> IO ()
test (rast, descr) fn = do
    (values, facets, area, sb) <- withFile fn ReadMode $ \h -> do
        values <- read <$> hGetLine h 
        facets <- read <$> hGetLine h
        area   <- read <$> hGetLine h
        sb     <- read <$> hGetLine h
        return (values, facets, area, sb)
    let is = rast area values facets
    if sb `similarTo` is
        then putStrLn $ fn ++ " (" ++ descr ++ ") - pass"
        else putStrLn $ fn ++ " (" ++ descr ++ ") - fail - "++
            show (length sb)++" vs. "++show (length is)++" got "++show (zip is sb) 

similarTo :: [Float] -> [Float] -> Bool
similarTo xs ys = and (zipWith near xs ys) && length xs == length ys
  where
    near x y = abs (x - y) < 0.001

type Area = ((Int, Int), (Int, Int))
type Value = ((Int, Int), Float)
type Insertion = ((Int, Int), Float)
type Facet = (Int, Int, Int)

-- | Rasterize the specified facets, clipping to area.
-- Any gaps are filled with 0 values.
rasterize :: Area -> [Value] -> [Facet] -> [Float]
rasterize area@((x0,y0),(x1,y1)) values facets = toArray $ concatMap tri facets
  where
    tri :: (Int,Int,Int) -> [((Int, Int), Float)]
    tri (iA,iB,iC) =
        -- Sort by increasing X
        let [(aInt@(axInt, _), vA),
             (bInt,            vB),
             (cInt@(cxInt, _), vC)] = sortBy (comparing xCoord) $
                                                    map (values !!) [iA,iB,iC]
        -- Scan across
            a@(ax, _) = fromIntegral *** fromIntegral $ aInt :: (Float, Float)
            b@(bx, _) = fromIntegral *** fromIntegral $ bInt :: (Float, Float)
            c@(cx, _) = fromIntegral *** fromIntegral $ cInt :: (Float, Float)
        in  flip concatMap [(max x0 axInt)..(min x1 (cxInt-1))] $ \xInt ->
                let x = fromIntegral xInt :: Float
                    (yAC, vAC) = (interpolate a c x, interpolate (ax,vA) (cx,vC) x)
                    (yOther, vOther) = if x < bx
                                  then (interpolate a b x, interpolate (ax,vA) (bx,vB) x)
                                  else (interpolate b c x, interpolate (bx,vB) (cx,vC) x)
                    -- Order AC and Other so that y is increasing
                    (yBot, vBot, yTop, vTop) = if yAC <= yOther
                                  then (yAC, vAC, yOther, vOther)
                                  else (yOther, vOther, yAC, vAC)
                    yStart = max y0 (round yBot)
                    yEnd   = min y1 (round yTop - 1)
                    -- Displace odd columns because the terrain mesh is hexagonal
                    hexify y | even xInt = fromIntegral y
                    hexify y             = fromIntegral y + 0.5
                in  flip map [yStart .. yEnd] $ \yInt ->
                        ((xInt, yInt), interpolate (yBot, vBot) (yTop, vTop) (hexify yInt))
      where
        xCoord ((x, _), _) = x

    interpolate :: (Float, Float) -> (Float, Float) -> Float -> Float
    interpolate (t0, x0) (t1, x1) t = (t - t0) * (x1 - x0) / (t1 - t0) + x0

    toArray :: [Insertion] -> [Float]
    toArray output =
        let output' = A.accumArray (flip const) 0 area output :: Array (Int, Int) Float
        in  A.elems output'

