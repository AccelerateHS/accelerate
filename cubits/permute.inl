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
    const DimOut        shOut,
    const DimIn0        shIn0
)
{
    Ix shapeSize      = size(shIn0);
    const Ix gridSize = __umul24(blockDim.x, gridDim.x);

    for (Ix ix = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; ix < shapeSize; ix += gridSize)
    {
        DimIn0 src = fromIndex(shIn0, ix);
        DimOut dst = project(src);

        if (!ignore(dst))
        {
            Ix j = toIndex(shOut, dst);
            set(d_out, j, apply(get0(d_in0, ix), get0(d_out, j)));
        }
    }
}

