/* -----------------------------------------------------------------------------
 *
 * Module    : FoldSeg
 * Copyright : (c) [2009..2010] Trevor L. McDonell, Rami G. Mukhtar
 * License   : BSD
 *
 * Reduction an array to a single value for each segment, using a binary
 * associative function
 *
 * ---------------------------------------------------------------------------*/

#define WARP_SIZE       32


/*
 * Cooperatively reduce an array to a single value. The computation requires an
 * extra half-warp worth of elements of shared memory per block, to let threads
 * index beyond the input data without using any branch instructions.
 */
static __inline__ __device__
TyOut reduce_warp(ArrOut s_data, TyOut sum)
{
    set(s_data, threadIdx.x, sum);
    sum = apply(sum, get0(s_data, threadIdx.x + 16)); set(s_data, threadIdx.x, sum);
    sum = apply(sum, get0(s_data, threadIdx.x +  8)); set(s_data, threadIdx.x, sum);
    sum = apply(sum, get0(s_data, threadIdx.x +  4)); set(s_data, threadIdx.x, sum);
    sum = apply(sum, get0(s_data, threadIdx.x +  2)); set(s_data, threadIdx.x, sum);
    sum = apply(sum, get0(s_data, threadIdx.x +  1));

    return sum;
}


/*
 * Each segment of the vector is assigned to a warp, which computes the
 * reduction of the i-th section, in parallel.
 *
 * This division of work implies that the data arrays are accessed in a
 * contiguous manner (if not necessarily aligned). For devices of compute
 * capability 1.2 and later, these accesses will be coalesced. A single
 * transaction will be issued if all of the addresses for a half-warp happen to
 * fall within a single 128-byte boundary. Extra transactions will be made to
 * cover any spill. The same applies for 2.x devices, except that all widths are
 * doubled since transactions occur on a per-warp basis.
 *
 * Since an entire 32-thread warp is assigned for each segment, many threads
 * will remain idle when the segments are very small. This code relies on
 * implicit synchronisation among threads in a warp.
 *
 * The offset array contains the starting index for each segment in the input
 * array. The i-th warp reduces values in the input array at indices
 * [d_offset[i], d_offset[i+1]).
 */
extern "C"
__global__ void
fold_segmented
(
    ArrOut              d_out,
    const ArrIn0        d_in0,
    const Int*          d_offset,
    const Ix            num_segments,
    const Ix            length                  // of input array d_in0
)
{
    const Ix vectors_per_block = blockDim.x / WARP_SIZE;
    const Ix num_vectors       = vectors_per_block * gridDim.x;
    const Ix thread_id         = blockDim.x * blockIdx.x + threadIdx.x;
    const Ix vector_id         = thread_id / WARP_SIZE;
    const Ix thread_lane       = threadIdx.x & (WARP_SIZE - 1);
    const Ix vector_lane       = threadIdx.x / WARP_SIZE;

    /*
     * Manually partition (dynamically-allocated) shared memory
     */
    extern volatile __shared__ Ix s_ptrs[][2];
    ArrOut s_data = partition((void*) &s_ptrs[vectors_per_block][2], blockDim.x);

    for (Ix seg = vector_id; seg < num_segments; seg += num_vectors)
    {
        /*
         * Use two threads to fetch the indices of the start and end of this
         * segment. This results in single coalesced global read, instead of two
         * separate transactions. If we are the last segment, the final index is
         * taken from the overall array length.
         */
        if (seg < num_segments - 1)
        {
            if (thread_lane < 2)
                s_ptrs[vector_lane][thread_lane] = d_offset[seg + thread_lane];
        }
        else
        {
            if (thread_lane == 0)
                s_ptrs[vector_lane][0] = d_offset[seg];

            s_ptrs[vector_lane][1] = length;
        }

        const Ix start = s_ptrs[vector_lane][0];
        const Ix end   = s_ptrs[vector_lane][1];

        /*
         * Have each thread read in all values for this segment, accumulating a
         * local sum. This is then reduced cooperatively in shared memory.
         */
        TyOut sum = identity();
        for (Ix i = start + thread_lane; i < end; i += WARP_SIZE)
            sum   = apply(sum, get0(d_in0, i));

        sum = reduce_warp(s_data, sum);

        /*
         * Finally, the first thread writes the result for this segment
         */
        if (thread_lane == 0)
            set(d_out, seg, sum);
    }
}

