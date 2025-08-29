/*
 * Module      : Data.Array.Accelerate.Debug.Flags
 * Copyright   : [2017..2020] The Accelerate Team
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
 * Stability   : experimental
 * Portability : non-portable (GHC extensions)
 */

#ifndef __ACCELERATE_FLAGS_H__
#define __ACCELERATE_FLAGS_H__

#include <stdint.h>

/* NOTE: [layout of command line options bitfield]
 */
typedef union {
  uint32_t bitfield;

  struct {
    uint32_t seq_sharing            : 1;
    uint32_t acc_sharing            : 1;
    uint32_t exp_sharing            : 1;
    uint32_t fusion                 : 1;
    uint32_t inplace                : 1;
    uint32_t fast_math              : 1;
    uint32_t fast_permute_const     : 1;
    uint32_t force_recomp           : 1;

    uint32_t debug                  : 1;
    uint32_t verbose                : 1;
    uint32_t dump_phases            : 1;
    uint32_t dump_sharing           : 1;
    uint32_t dump_fusion            : 1;
    uint32_t dump_simpl_stats       : 1;
    uint32_t dump_simpl_iterations  : 1;
    uint32_t dump_vectorisation     : 1;
    uint32_t dump_dot               : 1;
    uint32_t dump_simpl_dot         : 1;
    uint32_t dump_gc                : 1;
    uint32_t dump_gc_stats          : 1;
    uint32_t dump_cc                : 1;
    uint32_t dump_ld                : 1;
    uint32_t dump_asm               : 1;
    uint32_t dump_exec              : 1;
    uint32_t dump_sched             : 1;
  };
} __flags_t;

#endif // __ACCELERATE_FLAGS_H__

