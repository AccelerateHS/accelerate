/* -----------------------------------------------------------------------------
 *
 * Module    : Backpermute
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * Permute an array according to the permutation indices. This handles both
 * forward (scatter) and backward (gather) permutation, where:
 *
 *   bpermute :: [a] -> [Int] -> [a]
 *   bpermute v is = [ v!i | i <- is ]
 *
 * In this case, `length' specifies the number of elements in the `indices' and
 * `out' arrays.
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

