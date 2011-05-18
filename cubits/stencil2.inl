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

    for (Ix ix = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; ix < shapeSize; ix += gridSize)
    {
        Ix ix_ = fromIndex(shOut, ix);
        set(d_out, ix, apply(gather1(shOut, shIn1, ix_), gather0(shOut, shIn0, ix_)));
    }
}

