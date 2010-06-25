/* -----------------------------------------------------------------------------
 *
 * Module    : Map
 * Copyright : (c) 2010 Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

typedef unsigned int TyOut;
typedef unsigned int TyIn0;

typedef TyOut*  ArrOut;
typedef TyIn0*  ArrIn0;

static __inline__ __device__ void
set(ArrOut d_out, const Ix idx, const TyOut val)
{
}

static __inline__ __device__ TyIn0
get0(const ArrIn0 d_in0, const Ix idx)
{
}

static __inline__ __device__ TyOut
apply(const TyIn0 in0, const Ix shape)
{
}


/*
 * Apply the function to each element of the array. Each thread processes
 * multiple elements, striding the array by the grid size.
 */
__global__ void
map
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
        set(d_out, idx, apply(get0(d_in0, idx), shape));
    }
}


// vim:filetype=cuda.c

