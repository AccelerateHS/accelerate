/* -----------------------------------------------------------------------------
 *
 * Module    : ZipWith
 * Copyright : (c) 2010 Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

typedef unsigned int TyOut;
typedef unsigned int TyIn0;
typedef unsigned int TyIn1;

typedef TyOut* ArrOut;
typedef TyIn0* ArrIn0;
typedef TyIn1* ArrIn1;

static __inline__ __device__ void
set(ArrOut d_out, const Ix idx, const TyOut val)
{
}

static __inline__ __device__ TyIn0
get0(const ArrIn0 d_in0, const Ix idx)
{
}

static __inline__ __device__ TyIn1
get1(const ArrIn1 d_in1, const Ix idx)
{
}

static __inline__ __device__ TyOut
apply(const TyIn0 in1, const TyIn1 in0, const Ix shape)
{
}


/*
 * Combine two arrays using the given binary operator. Each thread processes
 * multiple elements, striding the array by the grid size.
 */
__global__ void
zipWith
(
    ArrOut              d_out,
    const ArrIn1        d_in1,
    const ArrIn0        d_in0,
    const Ix            shape
)
{
    Ix       idx;
    const Ix gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < shape; idx += gridSize)
    {
        set(d_out, idx, apply(get1(d_in1, idx), get0(d_in0, idx), shape));
    }
}


// vim:filetype=cuda.c

