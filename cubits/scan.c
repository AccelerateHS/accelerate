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

#define IS_BACKWARD             isBackward      // backward scan
#define IS_EXCLUSIVE            isExclusive     // exclusive scan
#define IS_MULTIROW             isMultiRow      // multi-row scan
#define IS_MULTIBLOCK           isMultiBlock    // write results to the d_blockSums array (multi-block scan)
#define IS_FULLBLOCK            isFullBlock     // all blocks process CTA_SIZE * SCAN_ELEMENTS_PER_THREAD

#define MAX_LEVEL               4               // Allows warp scan to be performed on partial warps

#define ELEMENTS_PER_THREAD     8               // Number of elements processed by each thread

#include "cudpp/globals.h"
#include "cudpp/scan_cta.cu"
#include "cudpp/scan_kernel.cu"
#include "cudpp/vector_kernel.cu"

