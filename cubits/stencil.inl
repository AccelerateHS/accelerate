/* -----------------------------------------------------------------------------
 *
 * Kernel      : Stencil utilities
 * Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * Utility functions for stencil kernels that handle array access that are out
 * of bounds.
 *
 * ---------------------------------------------------------------------------*/


#if defined(BOUNDARY_CLAMP_0) || defined(BOUNDARY_MIRROR_0) || defined(BOUNDARY_WRAP_0) || \
    defined(BOUNDARY_CLAMP_1) || defined(BOUNDARY_MIRROR_1) || defined(BOUNDARY_WRAP_1)

/*
 * Bounds check handling for Clamp, Mirror and Wrap. Peforms an index projection.
 */
static __inline__ __device__ DIM1 project_for_bounds(const DIM1 sh, const DIM1 ix)
{
    if (ix < 0)
    {
        #if defined(BOUNDARY_CLAMP_0) || defined(BOUNDARY_CLAMP_1)
            return 0;
        #elif defined(BOUNDARY_MIRROR_0) || defined(BOUNDARY_MIRROR_1)
            return 0 - ix - 1;
        #elif defined(BOUNDARY_WRAP_0) || defined(BOUNDARY_WRAP_1)
            return sh - ix;
        #else
            #error "project_for_bounds - only support CLAMP, MIRROR and WRAP."
        #endif
    }
    else if (ix >= sh)
    {
        #if defined(BOUNDARY_CLAMP_0) || defined(BOUNDARY_CLAMP_1)
            return sh - 1;
        #elif defined(BOUNDARY_MIRROR_0) || defined(BOUNDARY_MIRROR_1)
            return (sh - 1) - (ix - sh);
        #elif defined(BOUNDARY_WRAP_0) || defined(BOUNDARY_WRAP_1)
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

static __inline__ __device__ DIM2 project_for_bounds(const DIM2 sh, const DIM2 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

static __inline__ __device__ DIM3 project_for_bounds(const DIM3 sh, const DIM3 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

static __inline__ __device__ DIM4 project_for_bounds(const DIM4 sh, const DIM4 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

static __inline__ __device__ DIM5 project_for_bounds(const DIM5 sh, const DIM5 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

static __inline__ __device__ DIM6 project_for_bounds(const DIM6 sh, const DIM6 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

static __inline__ __device__ DIM7 project_for_bounds(const DIM7 sh, const DIM7 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

static __inline__ __device__ DIM8 project_for_bounds(const DIM8 sh, const DIM8 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

static __inline__ __device__ DIM9 project_for_bounds(const DIM9 sh, const DIM9 ix)
{
    return indexCons(project_for_bounds(indexTail(sh), indexTail(ix)),
                     project_for_bounds(indexHead(sh), indexHead(ix)));
}

#endif

#if !defined(BOUNDARY_CLAMP_0) && !defined(BOUNDARY_MIRROR_0) && !defined(BOUNDARY_WRAP_0)
/*
 * Bounds check handling for Constant.
 */
static __inline__ __device__ TyIn0 get0_for_constant_bounds(const DIM1 sh, const DIM1 ix)
{
    if (ix < 0)
        return boundary_const0();
    else if (ix >= sh)
        return boundary_const0();
    else
        return tex_get0(ix);
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(const DIM2 sh, const DIM2 ix)
{
    if (ix.a1 < 0 || ix.a0 < 0)
        return boundary_const0();
    else if (ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const0();
    else
        return tex_get0(toIndex(sh, ix));
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(const DIM3 sh, const DIM3 ix)
{
    if (ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const0();
    else if (ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const0();
    else
        return tex_get0(toIndex(sh, ix));
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(const DIM4 sh, const DIM4 ix)
{
    if (ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const0();
    else if (ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const0();
    else
        return tex_get0(toIndex(sh, ix));
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(const DIM5 sh, const DIM5 ix)
{
    if (ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const0();
    else if (ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const0();
    else
        return tex_get0(toIndex(sh, ix));
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(const DIM6 sh, const DIM6 ix)
{
    if (ix.a5 < 0 || ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const0();
    else if (ix.a5 >= sh.a5 || ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const0();
    else
        return tex_get0(toIndex(sh, ix));
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(const DIM7 sh, const DIM7 ix)
{
    if (ix.a6 < 0 || ix.a5 < 0 || ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const0();
    else if (ix.a6 >= sh.a6 || ix.a5 >= sh.a5 || ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const0();
    else
        return tex_get0(toIndex(sh, ix));
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(const DIM8 sh, const DIM8 ix)
{
    if (ix.a7 < 0 || ix.a6 < 0 || ix.a5 < 0 || ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const0();
    else if (ix.a7 >= sh.a7 || ix.a6 >= sh.a6 || ix.a5 >= sh.a5 || ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const0();
    else
        return tex_get0(toIndex(sh, ix));
}

static __inline__ __device__ TyIn0 get0_for_constant_bounds(const DIM9 sh, const DIM9 ix)
{
    if (ix.a8 < 0 || ix.a7 < 0 || ix.a6 < 0 || ix.a5 < 0 || ix.a4 < 0 || ix.a3 < 0 || ix.a2 < 0 || ix.a1 < 0 || ix.a0 < 0)
        return boundary_const0();
    else if (ix.a8 >= sh.a8 || ix.a7 >= sh.a7 || ix.a6 >= sh.a6 || ix.a5 >= sh.a5 || ix.a4 >= sh.a4 || ix.a3 >= sh.a3 || ix.a2 >= sh.a2 || ix.a1 >= sh.a1 || ix.a0 >= sh.a0)
        return boundary_const0();
    else
        return tex_get0(toIndex(sh, ix));
}

#endif


/*
 * Getter function that handles indexing outside the array boundary (array 0).
 */
static inline __attribute__((device)) TyIn0 get0_for_stencil(DimIn0 sh, DimIn0 ix)
{
    #if defined(BOUNDARY_CLAMP_0) || defined(BOUNDARY_MIRROR_0) || defined(BOUNDARY_WRAP_0)
        return tex_get0(toIndex(sh, project_for_bounds(sh, ix)));
    #else
        return get0_for_constant_bounds(sh, ix);
    #endif
}


