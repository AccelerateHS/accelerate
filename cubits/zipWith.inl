/* -----------------------------------------------------------------------------
 *
 * Module    : ZipWith
 * Copyright : (c) 2010 Trevor L. McDonell
 * License   : BSD
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
    const Ix            shape
)
{
    Ix       idx;
    const Ix gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < shape; idx += gridSize)
    {
        set(d_out, idx, apply(get1(d_in1, idx), get0(d_in0, idx), shape));
    }
}

