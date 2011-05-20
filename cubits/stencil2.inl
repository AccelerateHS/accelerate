/* -----------------------------------------------------------------------------
 *
 * Kernel      : Stencil2
 * Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * Apply the function to each element of the array that takes a neighborhood of
 * elements from two input arrays. Each thread processes multiple elements,
 * striding the array by the grid size. To improve performance, both input
 * arrays are bound as texture references so that reads are cached.
 *
 * ---------------------------------------------------------------------------*/

extern "C"
__global__ void
stencil2
(
    ArrOut        d_out,
    const DimOut  shOut,
    const DimIn1  shIn1,
    const DimIn0  shIn0
)
{
    const Ix shapeSize = size(shOut);
    const Ix gridSize  = __umul24(blockDim.x, gridDim.x);

    for (Ix i = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; i < shapeSize; i += gridSize)
    {
        DimOut ix = fromIndex(shOut, i);
        set(d_out, i, apply(gather1(shIn1, ix), gather0(shIn0, ix)));
    }
}

