/* -----------------------------------------------------------------------------
 *
 * Module    : Backpermute
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * Backwards permutation (gather) an array according to the permutation
 * function. The input `shape' is that of the output array.
 *
 *   bpermute :: [a] -> [Int] -> [a]
 *   bpermute v is = [ v!i | i <- is ]
 *
 * ---------------------------------------------------------------------------*/

extern "C"
__global__ void
backpermute
(
    ArrOut              d_out,
    const ArrIn0        d_in0,
    const Ix            shape
)
{
    Ix       idx;
    const Ix gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < shape; idx += gridSize)
    {
        set(d_out, idx, get0(d_in0, project(idx)));
    }
}

