/* -----------------------------------------------------------------------------
 *
 * Kernel      : Fold
 * Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * Reduce the innermost dimension of a multidimensional array to a single value
 * with a binary associative function
 *
 * ---------------------------------------------------------------------------*/

#include "reduce.inl"


extern "C"
__global__ void
fold
(
    ArrOut              d_out,
    const ArrIn0        d_in0,
    const DimOut        shOut,
    const DimIn0        shIn0
)
{
    extern volatile __shared__ void* s_ptr[];
    ArrOut s_data = partition(s_ptr, blockDim.x);

    const Ix tid          = threadIdx.x;
    const Ix num_elements = indexHead(shIn0);
    const Ix num_segments = size(shOut);

    /*
     * Each block of threads reduces elements along a projection through an
     * innermost dimension to a single value.
     */
    for (Ix ix = blockIdx.x; ix < num_segments; ix += gridDim.x)
    {
        Ix     i     = tid;
        DimOut ixOut = fromIndex(shOut, ix);
        TyOut  sum;

        /*
         * Reduce multiple elements per thread
         */
        if (i < num_elements)
        {
            sum = get0(d_in0, toIndex(shIn0, indexCons(ixOut, i)));

            for (i += blockDim.x; i < num_elements; i += blockDim.x)
                sum = apply(sum, get0(d_in0, toIndex(shIn0, indexCons(ixOut, i))));
        }

        /*
         * Each thread puts its local sum into shared memory, then cooperatively
         * reduce the shared array to a single value.
         */
        set(s_data, tid, sum);
        __syncthreads();

        sum = reduce_block_n(s_data, sum, min(num_elements, blockDim.x));

        if (tid == 0)
        {
#ifndef INCLUSIVE
            sum = num_elements > 0 ? apply(sum, identity()) : identity();
#endif
            set(d_out, ix, sum);
        }
    }
}

