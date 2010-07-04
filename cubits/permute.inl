/* -----------------------------------------------------------------------------
 *
 * Module    : Permute
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * Forward permutation, characterised by a function that determines for each
 * element in the source array where it should go in the target. The output
 * array should be initialised with a default value, as the permutation may be
 * between arrays of different sizes and some positions may never be touched.
 *
 * Elements from the source array are dropped for which the permutation function
 * yields the magic index `ignore`.
 *
 * ---------------------------------------------------------------------------*/

extern "C"
__global__ void
permute
(
    ArrOut              d_out,
    const ArrIn0        d_in0,
    const Ix            shape
)
{
    Ix       dst;
    Ix       idx;
    const Ix gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < shape; idx += gridSize)
    {
        dst = project(idx);

        if (dst != ignore)
        {
            set(d_out, dst, apply(get0(d_in0, idx), get0(d_out, dst)));
        }
    }
}

