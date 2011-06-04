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


extern "C"
__global__ void
inclusive_update
(
    ArrOut              d_out,
    ArrIn0              d_in0,
    const Ix            N,
    const Ix            interval_size
)
{
    const Ix interval_begin = interval_size * blockIdx.x;
    const Ix interval_end   = min(interval_begin + interval_size, N);
    TyOut sum;

    // ignore first block and get value to add to this segment
#if REVERSE
    if (blockIdx.x == gridDim.x - 1)
      return;

    sum = get0(d_in0, blockIdx.x + 1);
#else
    if (blockIdx.x == 0)
      return;

    sum = get0(d_in0, blockIdx.x - 1);
#endif

    // advance result iterator
    for(Ix base = interval_begin; base < interval_end; base += blockDim.x)
    {
        const Ix i = base + threadIdx.x;

        if (i < interval_end)
        {
            TyOut tmp = get0(d_out, i);
            set(d_out, i, apply(sum, tmp));
        }
        __syncthreads();
    }
}

