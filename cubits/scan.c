/* -----------------------------------------------------------------------------
 *
 * Module    : Scan
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * The CUDPP parallel implementation of scan, hoisted out of C++ template-land
 *
 * ---------------------------------------------------------------------------*/

typedef int     T;
typedef int4    T4;

#define IsBackward              0       // backward scan
#define IsExclusive             1       // exclusive scan
#define IsMultiRow              0       // multi-row scan
#define IsMultiBlock            1       // write results to the d_blockSums array (multi-block scan)
#define IsFullBlock             0       // all blocks process CTA_SIZE * SCAN_ELEMENTS_PER_THREAD

#define MaxLevel                4       // Allows warp scan to be performed on partial warps

#define ElementsPerThread       8       // Number of elements processed by each thread

#include "cudpp/scan_cta.cu"
#include "cudpp/scan_kernel.cu"
#include "cudpp/vector_kernel.cu"

