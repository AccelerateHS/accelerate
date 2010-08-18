/* -----------------------------------------------------------------------------
 *
 * Module    : Slice
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

extern "C"
__global__ void
slice
(
    ArrOut              d_out,
    const ArrIn0        d_in0,
    const Slice         slice,
    const CoSlice       slix,
    const SliceDim      sliceDim
)
{
    Ix shapeSize      = size(slice);
    const Ix gridSize = __umul24(blockDim.x, gridDim.x);

    for (Ix ix = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; ix < shapeSize; ix += gridSize)
    {
        Slice    dst = fromIndex(slice, ix);
        SliceDim src = sliceIndex(dst, slix);

        set(d_out, ix, get0(d_in0, toIndex(sliceDim, src)));
    }
}

