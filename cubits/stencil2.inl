/* -----------------------------------------------------------------------------
 *
 * Kernel      : Stencil2
 * Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * Apply the function to each element of the array that takes a neighborhood of
 * elements from two input arrays as its input. Each thread processes multiple
 * elements, striding the array by the grid size. To improve performance, both
 * input arrays are bound as texture references so that reads are cached.
 *
 * ---------------------------------------------------------------------------*/


#include <stencil.inl>


#if !defined(BOUNDARY_CLAMP_1) && !defined(BOUNDARY_MIRROR_1) && !defined(BOUNDARY_WRAP_1)

/*
 * Bounds check handling for Constant.
 */
static __inline__ __device__ TyIn1 get1_for_constant_bounds(const DIM1 sh, const DIM1 ix)
{
    if (ix < 0)
        return boundary_const1();
    else if (ix >= sh)
        return boundary_const1();
    else
        return tex_get1(ix);
}

static __inline__ __device__ TyIn1 get1_for_constant_bounds(const DIM2 sh, const DIM2 ix)
{
    if (ix.a1 < 0 || ix.a0 < 0)
        return boundary_const1();
    else if (ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const1();
    else
        return tex_get1(toIndex(sh, ix));
}

static __inline__ __device__ TyIn1 get1_for_constant_bounds(const DIM3 sh, const DIM3 ix)
{
    if (ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const1();
    else if (ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const1();
    else
        return tex_get1(toIndex(sh, ix));
}

static __inline__ __device__ TyIn1 get1_for_constant_bounds(const DIM4 sh, const DIM4 ix)
{
    if (ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const1();
    else if (ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const1();
    else
        return tex_get1(toIndex(sh, ix));
}

static __inline__ __device__ TyIn1 get1_for_constant_bounds(const DIM5 sh, const DIM5 ix)
{
    if (ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const1();
    else if (ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const1();
    else
        return tex_get1(toIndex(sh, ix));
}

static __inline__ __device__ TyIn1 get1_for_constant_bounds(const DIM6 sh, const DIM6 ix)
{
    if (ix.a5 < 0 || ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const1();
    else if (ix.a5 >= sh.a5 || ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const1();
    else
        return tex_get1(toIndex(sh, ix));
}

static __inline__ __device__ TyIn1 get1_for_constant_bounds(const DIM7 sh, const DIM7 ix)
{
    if (ix.a6 < 0 || ix.a5 < 0 || ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const1();
    else if (ix.a6 >= sh.a6 || ix.a5 >= sh.a5 || ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const1();
    else
        return tex_get1(toIndex(sh, ix));
}

static __inline__ __device__ TyIn1 get1_for_constant_bounds(const DIM8 sh, const DIM8 ix)
{
    if (ix.a7 < 0 || ix.a6 < 0 || ix.a5 < 0 || ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const1();
    else if (ix.a7 >= sh.a7 || ix.a6 >= sh.a6 || ix.a5 >= sh.a5 || ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const1();
    else
        return tex_get1(toIndex(sh, ix));
}

static __inline__ __device__ TyIn1 get1_for_constant_bounds(const DIM9 sh, const DIM9 ix)
{
    if (ix.a8 < 0 || ix.a7 < 0 || ix.a6 < 0 || ix.a5 < 0 || ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const1();
    else if (ix.a8 >= sh.a8 || ix.a7 >= sh.a7 || ix.a6 >= sh.a6 || ix.a5 >= sh.a5 || ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const1();
    else
        return tex_get1(toIndex(sh, ix));
}

#endif


/*
 * Getter function that handles indexing outside the array boundary (array 1).
 */
static inline __attribute__((device)) TyIn1 get1_for_stencil(DimIn1 sh, DimIn1 ix)
{
    #if defined(BOUNDARY_CLAMP_1) || defined(BOUNDARY_MIRROR_1) || defined(BOUNDARY_WRAP_1)
        return tex_get1(toIndex(sh, project_for_bounds(sh, ix)));
    #else
        return get1_for_constant_bounds(sh, ix);
    #endif
}


/*
 * The kernel.
 */
extern "C"
__global__ void
stencil2
(
    ArrOut        d_out,
    const DimOut  d_out_shape,
    const DimIn0  d_in0_shape,
    const DimIn0  d_in1_shape
)
{
    Ix       idx;
    const Ix gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < size(d_out_shape); idx += gridSize)
    {
        set(d_out, idx, gather_and_apply(d_in0_shape, fromIndex(d_in0_shape, idx), d_in1_shape, fromIndex(d_in1_shape, idx)));
    }
}

