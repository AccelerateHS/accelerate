{-# LANGUAGE FlexibleContexts #-}

module SVM (listToKernel2D, imageConvolveKernel) where

import Prelude hiding (replicate, scanl, zip, map, filter, max, min, not, zipWith)
import Data.Array.Accelerate


listToKernel2D :: Elem a => [a] -> Array DIM1 a
listToKernel2D list = fromList (length list) list

imageConvolveKernel :: (Elem a, IsNum a) => (Exp Int, Exp Int) -> (Exp Int, Exp Int) -> Acc (Array DIM1 a) -> Acc (Array DIM1 a) -> Acc (Array DIM1 a)
imageConvolveKernel (kernelLength , kernelWidth) (inputLength , inputWidth) kernel input
   = let
        interDim = inputLength * inputWidth * kernelLength * kernelWidth
        --input = reshape (inputLength * inputWidth) input'
        --kernel = reshape (kernelLength * kernelWidth) kernel'

        inputPatch =  backpermute interDim (patchPermute (kernelLength , kernelWidth) (inputLength , inputWidth)) input
        kernelPatch = backpermute interDim (\ix -> mod ix (kernelLength * kernelWidth)) kernel

        products = zipWith (*) inputPatch kernelPatch
        def = map (const ((kernelLength * kernelWidth))) input
   in foldSeg (+) (0) products def

   --in permute (+) def (patchPermute (kernelLength , kernelWidth) (inputLength , inputWidth)) products

patchPermute :: (Exp Int, Exp Int)  -> (Exp Int, Exp Int) -> Exp Int -> Exp Int
patchPermute (patchRows, patchCols) (inputRows, inputCols) ix
    = let
        patchPixels = patchRows * patchCols :: Exp Int
        patchIx     = mod ix patchPixels :: Exp Int
        pixel       = div (ix - patchIx) patchPixels :: Exp Int
        pixelCol    = mod pixel inputCols :: Exp Int
        pixelRow    = div (pixel - pixelCol) inputCols :: Exp Int
        patchCol    = mod patchIx patchCols :: Exp Int
        patchRow    = div (patchIx - patchCol) patchCols :: Exp Int
        targetRow   = pixelRow - ((patchRows - 1) `div` 2) + patchRow :: Exp Int
        targetCol   = pixelCol - ((patchCols - 1) `div` 2) + patchCol :: Exp Int
     in ((targetRow <* 0) ||* (targetCol <* 0) ||* (targetRow >=* inputRows - 1) ||* (targetCol >=* inputCols - 1)) ?
                                             ((pixelRow * inputCols + pixelCol), (targetRow * inputCols + targetCol))


