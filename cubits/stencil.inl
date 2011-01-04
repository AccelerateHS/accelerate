/* -----------------------------------------------------------------------------
 *
 * Kernel      : Stencil
 * Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * Apply the function to each element of the array that takes a neighborhood of
 * elements as its input. Each thread processes multiple elements, striding the
 * array by the grid size.
 *
 * To improve performance, the input array is bound as a texture reference so
 * that reads are cached. Note that the input array is still passed in to the
 * kernel as an argument, and also passed around through various functions, but
 * never actually used. This is simply an artefact of reusing generated functions
 * such as 'gather' for optimised version of specific kernel operations (e.g. 1D
 * and 2D stencil operations).
 *
 * ---------------------------------------------------------------------------*/


#if defined(BOUNDARY_CLAMP) || defined(BOUNDARY_MIRROR) || defined(BOUNDARY_WRAP)

/*
 * Bounds check handling for Clamp, Mirror and Wrap. Peforms an index projection.
 */
static __inline__ __device__ DIM1 project_for_bounds(DIM1 sh, DIM1 ix)
{
    if (ix < 0)
    {
        #if defined(BOUNDARY_CLAMP)
            return 0;
        #elif defined(BOUNDARY_MIRROR)
            return 0 - ix - 1;
        #elif defined(BOUNDARY_WRAP)
            return sh - ix;
        #else
            #error "project_for_bounds - only support CLAMP, MIRROR and WRAP."
        #endif
    }
    else if (ix >= sh)
    {
        #if defined(BOUNDARY_CLAMP)
            return sh - 1;
        #elif defined(BOUNDARY_MIRROR)
            return (sh - 1) - (ix - sh);
        #elif defined(BOUNDARY_WRAP)
            return ix - sh;
        #else
            #error "project_for_bounds - only support CLAMP, MIRROR and WRAP."
        #endif
    }
    else
    {
        return ix;
    }
}

static __inline__ __device__ DIM2 project_for_bounds(DIM2 sh, DIM2 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

static __inline__ __device__ DIM3 project_for_bounds(DIM3 sh, DIM3 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

static __inline__ __device__ DIM4 project_for_bounds(DIM4 sh, DIM4 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

static __inline__ __device__ DIM5 project_for_bounds(DIM5 sh, DIM5 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

static __inline__ __device__ DIM6 project_for_bounds(DIM6 sh, DIM6 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

static __inline__ __device__ DIM7 project_for_bounds(DIM7 sh, DIM7 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

static __inline__ __device__ DIM8 project_for_bounds(DIM8 sh, DIM8 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

static __inline__ __device__ DIM9 project_for_bounds(DIM9 sh, DIM9 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

#else

/*
 * Bounds check handling for Constant.
 */
static __inline__ __device__ TyIn0 get0_for_constant_bounds(ArrIn0 d_in0, DIM1 sh, DIM1 ix)
{
    if (ix < 0)
        return boundary_const();
    else if (ix >= sh)
        return boundary_const();
    else
        return tex_get0(ix);
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(ArrIn0 d_in0, DIM2 sh, DIM2 ix)
{
    if (ix.a1 < 0 || ix.a0 < 0)
        return boundary_const();
    else if (ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const();
    else
        return tex_get0(toIndex(sh, ix));
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(ArrIn0 d_in0, DIM3 sh, DIM3 ix)
{
    if (ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const();
    else if (ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const();
    else
        return tex_get0(toIndex(sh, ix));
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(ArrIn0 d_in0, DIM4 sh, DIM4 ix)
{
    if (ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const();
    else if (ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const();
    else
        return tex_get0(toIndex(sh, ix));
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(ArrIn0 d_in0, DIM5 sh, DIM5 ix)
{
    if (ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const();
    else if (ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const();
    else
        return tex_get0(toIndex(sh, ix));
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(ArrIn0 d_in0, DIM6 sh, DIM6 ix)
{
    if (ix.a5 < 0 || ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const();
    else if (ix.a5 >= sh.a5 || ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const();
    else
        return tex_get0(toIndex(sh, ix));
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(ArrIn0 d_in0, DIM7 sh, DIM7 ix)
{
    if (ix.a6 < 0 || ix.a5 < 0 || ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const();
    else if (ix.a6 >= sh.a6 || ix.a5 >= sh.a5 || ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const();
    else
        return tex_get0(toIndex(sh, ix));
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(ArrIn0 d_in0, DIM8 sh, DIM8 ix)
{
    if (ix.a7 < 0 || ix.a6 < 0 || ix.a5 < 0 || ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const();
    else if (ix.a7 >= sh.a7 || ix.a6 >= sh.a6 || ix.a5 >= sh.a5 || ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const();
    else
        return tex_get0(toIndex(sh, ix));
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(ArrIn0 d_in0, DIM9 sh, DIM9 ix)
{
    if (ix.a8 < 0 || ix.a7 < 0 || ix.a6 < 0 || ix.a5 < 0 || ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const();
    else if (ix.a8 >= sh.a8 || ix.a7 >= sh.a7 || ix.a6 >= sh.a6 || ix.a5 >= sh.a5 || ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const();
    else
        return tex_get0(toIndex(sh, ix));
}

#endif


/*
 * Getter function that handles indexing outside the array boundary.
 */
static inline __attribute__((device)) TyIn0 get0_for_stencil(ArrIn0 d_in0, ArrDimIn0 sh, ArrDimIn0 ix)
{
    #if defined(BOUNDARY_CLAMP) || defined(BOUNDARY_MIRROR) || defined(BOUNDARY_WRAP)
        return tex_get0(toIndex(sh, project_for_bounds(sh, ix)));
    #else
        return get0_for_constant_bounds(d_in0, sh, ix);
    #endif
}


/*
 * The kernel.
 */
extern "C"
__global__ void
stencil
(
    ArrOut              d_out,
    const ArrIn0        d_in0,
    const ArrDimIn0     d_in0_shape
)
{
    Ix       idx;
    const Ix gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < size(d_in0_shape); idx += gridSize)
    {
        set(d_out, idx, apply(gather0(d_in0, fromIndex(d_in0_shape, idx), d_in0_shape)));
    }
}


