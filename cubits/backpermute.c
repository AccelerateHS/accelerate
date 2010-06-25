/* -----------------------------------------------------------------------------
 *
 * Module    : Backpermute
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

typedef unsigned int T;
typedef unsigned int Ix;

typedef int  TyOut;
typedef int  TyIn0;

typedef int* ArrOut;
typedef int* ArrIn0;

static __inline__ __device__ void
set(ArrOut d_out, const Ix idx, const TyOut val)
{
}

static __inline__ __device__ TyIn0
get0(const ArrIn0 d_in0, const Ix idx)
{
}

/*
 * Index projection from destination -> source
 */
static __inline__ __device__ Ix
project(const Ix idx, const Ix shape)
{
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
    ArrOut              d_out,
    const ArrIn0        d_in0,
    const Ix            shape
)
{
    Ix       idx;
    const Ix gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < shape; idx += gridSize)
    {
        set(d_out, idx, get0(d_in0, project(idx, shape)));
    }
}

// vim:filetype=cuda.c

