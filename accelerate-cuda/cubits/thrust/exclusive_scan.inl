/*
 *  Copyright 2008-2010 NVIDIA Corporation
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */


extern "C"
__global__ void
inclusive_scan
(
    ArrOut              d_out,
    const ArrIn0        d_in0,
    ArrIn0              d_block_results,
    const Ix            N,
    const Ix            interval_size
)
{
    extern __shared__ TyOut sdata[];
    scan_intervals(sdata, d_out, d_in0, d_block_results, N, interval_size);
}


/*
 * Haskell-style scans increase the array length by one, so the indices of where
 * to update results changes slightly. Additionally, the final reduction value
 * is written to d_out, rather than updating the input partial block sums d_sum.
 */
extern "C"
__global__ void
exclusive_update
(
    ArrOut              d_out,
    ArrIn0              d_in0,
    ArrIn0              d_sum,
    const Ix            N,
    const Ix            interval_size
)
{
    extern __shared__ TyOut sdata[];

    const int interval_begin = interval_size * blockIdx.x;
    const int interval_end   = min(interval_begin + interval_size, N);

    // value to add to this segment
    TyOut carry = identity();

#if REVERSE
    if (blockIdx.x != gridDim.x - 1)
    {
      TyOut tmp = get0(d_in0, blockIdx.x + 1);
      carry     = apply(carry, tmp);
    }
#else
    if (blockIdx.x != 0)
    {
      TyOut tmp = get0(d_in0, blockIdx.x - 1);
      carry     = apply(carry, tmp);
    }
#endif

    // advance result iterator
    int output = REVERSE ? interval_end - threadIdx.x - 1 : interval_begin + threadIdx.x;
    TyOut val = carry;

    for (int base = interval_begin; base < interval_end; base += blockDim.x)
    {
        const int i = base + threadIdx.x;

        if (i < interval_end)
        {
            TyOut tmp          = get0(d_out, output);
            sdata[threadIdx.x] = apply(carry, tmp);
        }
        __syncthreads();

        if (threadIdx.x != 0)
            val = sdata[threadIdx.x - 1];

        if (i < interval_end)
        {
#if REVERSE && HASKELL_STYLE
            set(d_out, output + 1, val);
#else
            set(d_out, output, val);
#endif
        }

        if (threadIdx.x == 0)
            val = sdata[blockDim.x - 1];

        __syncthreads();

        // update output iterator
#if REVERSE
        output -= blockDim.x;
#else
        output += blockDim.x;
#endif
    }

    // Use a single thread to set the overall scan result.
    if (blockIdx.x == 0 && threadIdx.x == 0)
    {
        TyOut sum = apply(identity(), get0(d_sum, 0));

#if HASKELL_STYLE
        set(d_out, REVERSE ? 0 : N, sum);
#else
        set(d_sum, 0, sum);
#endif
    }
}

