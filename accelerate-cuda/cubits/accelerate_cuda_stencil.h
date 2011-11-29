/* -----------------------------------------------------------------------------
 *
 * Module      : Stencil
 * Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * ---------------------------------------------------------------------------*/

#ifndef __ACCELERATE_CUDA_STENCIL_H__
#define __ACCELERATE_CUDA_STENCIL_H__

#include <accelerate_cuda_shape.h>

#ifdef __cplusplus

/*
 * Test if an index lies within the boundaries of a shape
 */
template <typename Shape>
static __inline__ __device__ int inRange(const Shape sh, const Shape ix)
{
    return inRange(indexHead(sh), indexHead(ix)) && inRange(indexTail(sh), indexTail(ix));
}

template <>
static __inline__ __device__ int inRange(const DIM1 sz, const DIM1 i)
{
    return i >= 0 && i < sz;
}

template <>
static __inline__ __device__ int inRange(const DIM0 sz, const DIM0 i)
{
    return i == 0;
}


/*
 * Boundary condition handlers
 */
template <typename Shape>
static __inline__ __device__ Shape clamp(const Shape sh, const Shape ix)
{
    return indexCons( clamp(indexTail(sh), indexTail(ix))
                    , clamp(indexHead(sh), indexHead(ix)) );
}

template <>
static __inline__ __device__ DIM1 clamp(const DIM1 sz, const DIM1 i)
{
    // CUDA-4.0 does not include 64-bit min/max functions for Fermi (?)
    return max(0, min((int) i, (int) sz-1));
}


template <typename Shape>
static __inline__ __device__ Shape mirror(const Shape sh, const Shape ix)
{
    return indexCons( mirror(indexTail(sh), indexTail(ix))
                    , mirror(indexHead(sh), indexHead(ix)) );
}

template <>
static __inline__ __device__ DIM1 mirror(const DIM1 sz, const DIM1 i)
{
    if      (i <  0)  return -i;
    else if (i >= sz) return sz - (i-sz+2);
    else              return i;
}


template <typename Shape>
static __inline__ __device__ Shape wrap(const Shape sh, const Shape ix)
{
    return indexCons( wrap(indexTail(sh), indexTail(ix))
                    , wrap(indexHead(sh), indexHead(ix)) );
}

template <>
static __inline__ __device__ DIM1 wrap(const DIM1 sz, const DIM1 i)
{
    if      (i <  0)  return sz+i;
    else if (i >= sz) return i-sz;
    else              return i;
}

#endif

#endif
