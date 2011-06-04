/* -----------------------------------------------------------------------------
 *
 * Kernel      : Fold
 * Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
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

    const Ix num_elements = indexHead(shIn0);
    const Ix num_segments = size(shOut);

    const Ix num_vectors  = blockDim.x / warpSize * gridDim.x;
    const Ix thread_id    = blockDim.x * blockIdx.x + threadIdx.x;
    const Ix vector_id    = thread_id / warpSize;
    const Ix thread_lane  = threadIdx.x & (warpSize - 1);

    /*
     * Each warp reduces elements along a projection through an innermost
     * dimension to a single value
     */
    for (Ix seg = vector_id; seg < num_segments; seg += num_vectors)
    {
        const Ix    start = seg   * num_elements;
        const Ix    end   = start + num_elements;
              TyOut sum;

        if (num_elements > warpSize)
        {
            /*
             * Ensure aligned access to global memory, and that each thread
             * initialises its local sum.
             */
            Ix i = start - (start & (warpSize - 1)) + thread_lane;
            if (i >= start)
                sum = get0(d_in0, i);

            if (i + warpSize < end)
            {
                TyOut tmp = get0(d_in0, i + warpSize);

                if (i >= start) sum = apply(sum, tmp);
                else            sum = tmp;
            }

            /*
             * Now, iterate along the inner-most dimension collecting a local sum
             */
            for (i += 2 * warpSize; i < end; i += warpSize)
                sum = apply(sum, get0(d_in0, i));
        }
        else if (start + thread_lane < end)
        {
            sum = get0(d_in0, start + thread_lane);
        }

        /*
         * Each thread puts its local sum into shared memory, then cooperatively
         * reduce the shared array to a single value.
         */
        set(s_data, threadIdx.x, sum);
        sum = reduce_warp_n(s_data, sum, min(num_elements, warpSize));

        /*
         * Finally, the first thread writes the result for this segment
         */
        if (thread_lane == 0)
        {
#ifndef INCLUSIVE
            sum = num_elements > 0 ? apply(sum, identity()) : identity();
#endif
            set(d_out, seg, sum);
        }
    }
}

