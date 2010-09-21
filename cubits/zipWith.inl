/* -----------------------------------------------------------------------------
 *
 * Kernel      : ZipWith
 * Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * Combine two arrays using the given binary operator. Each thread processes
 * multiple elements, striding the array by the grid size.
 *
 * ---------------------------------------------------------------------------*/

extern "C"
__global__ void
zipWith
(
    ArrOut              d_out,
    const ArrIn1        d_in1,
    const ArrIn0        d_in0,
    const DimOut        shOut,
    const DimIn1        shIn1,
    const DimIn0        shIn0
)
{
    const Ix shapeSize = size(shOut);
    const Ix gridSize  = __umul24(blockDim.x, gridDim.x);

    for (Ix ix = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; ix < shapeSize; ix += gridSize)
    {
        Ix i1 = toIndex(shIn1, fromIndex(shOut, ix));
        Ix i0 = toIndex(shIn0, fromIndex(shOut, ix));

        set(d_out, ix, apply(get1(d_in1, i1), get0(d_in0, i0)));
    }
}

