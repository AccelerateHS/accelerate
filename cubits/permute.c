/* -----------------------------------------------------------------------------
 *
 * Module    : Permute
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

typedef int             T;
typedef unsigned int    Ix;

const Ix ignore = (Ix) -1;

/*
 * Index projection source -> destination
 */
__device__ Ix
project(const Ix idx)
{
    return idx;
}

/*
 * Combination function
 */
__device__ static T
apply(const T x, const T y)
{
    return x + y;
}


/*
 * Forward permutation, characterised by a function that determines for each
 * element in the source array where it should go in the target. The output
 * array should be initialised with a default value, as the permutation may be
 * between arrays of different sizes and some positions may never be touched.
 *
 * Elements from the source array are dropped for which the permutation function
 * yields the magic index `ignore`.
 */
__global__ void
permute
(
    T                   *d_out,
    const T             *d_in0,
    const unsigned int  length
)
{
    Ix                 dst;
    unsigned int       idx;
    const unsigned int gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < length; idx += gridSize)
    {
        dst = project(idx);

        if (dst != ignore)
        {
           d_out[dst] = apply(d_out[dst], d_in0[idx]);
        }
    }
}

// vim:filetype=cuda.c

