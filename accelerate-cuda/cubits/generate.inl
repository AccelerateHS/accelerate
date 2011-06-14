/* -----------------------------------------------------------------------------
 *
 * Kernel      : Generate
 * Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * Apply the function to each element of the array. Each thread processes
 * multiple elements, striding the array by the grid size.
 *
 * ---------------------------------------------------------------------------*/

extern "C"
__global__ void
generate
(
    ArrOut              d_out,
    const DimOut        shOut
)
{
    int       idx;
    const int n        = size(shOut);
    const int gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < n; idx += gridSize)
    {
        set(d_out, idx, apply(fromIndex(shOut, idx)));
    }
}

