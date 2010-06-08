/* -----------------------------------------------------------------------------
 *
 * Module    : Backpermute
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

typedef unsigned int T;
typedef unsigned int Ix;

/*
 * Index projection from destination -> source
 */
__device__ Ix
project(const Ix x0)
{
    return x0;
}


/*
 * Permute an array according to the permutation indices. This handles both
 * forward (scatter) and backward (gather) permutation, where:
 *
 *   bpermute :: [a] -> [Int] -> [a]
 *   bpermute v is = [ v!i | i <- is ]
 *
 * In this case, `length' specifies the number of elements in the `indices' and
 * `out' arrays.
 */
__global__ void
backpermute
(
    T                   *d_out,
    const T             *d_in0,
    const unsigned int  length
)
{
    Ix                 src;
    unsigned int       idx;
    const unsigned int gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < length; idx += gridSize)
    {
        src        = project(idx);
        d_out[idx] = d_in0[src];
    }
}

// vim:filetype=cuda.c

