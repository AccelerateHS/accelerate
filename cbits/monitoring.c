/*
 * Module      : Data.Array.Accelerate.Debug.Monitoring
 * Copyright   : [2016..2019] The Accelerate Team
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
 * Stability   : experimental
 * Portability : non-portable (GHC extensions)
 *
 * Support for runtime system monitoring
 *
 * This is a hack to work around <https://github.com/haskell/cabal/issues/4937>
 */

#include <stdint.h>
#include <stdio.h>


/* These monitoring counters are globals which will be accessed from the
 * Haskell side.
 */
int64_t __active_ns_llvm_native             = 0;
int64_t __active_ns_llvm_ptx                = 0;

int64_t __current_bytes_remote              = 0;
int64_t __current_bytes_nursery             = 0;

int64_t __total_bytes_allocated_local       = 0;
int64_t __total_bytes_allocated_remote      = 0;
int64_t __total_bytes_copied_to_remote      = 0;
int64_t __total_bytes_copied_from_remote    = 0;
int64_t __total_bytes_evicted_from_remote   = 0;
int64_t __num_remote_gcs                    = 0;
int64_t __num_evictions                     = 0;

extern int32_t __dump_gc;
extern int32_t __dump_gc_stats;

#if defined(ACCELERATE_DEBUG)

/* cbits/clock.c */
double clock_gettime_elapsed_seconds(void);

/*
 * Format a large number, using comma separators.
 */
static char* format_int64(char *buffer, int64_t x)
{
  char *s = buffer;

  if (x < 0)
  {
    *s++  = '-';
    x     = -x;
  }

  if (x < 1000)
  {
    sprintf(s, "%lld", x);
  }
  else if (x < 1000000)
  {
    sprintf(s, "%lld,%03lld", x/1000, x%1000);
  }
  else if (x < 1000000000)
  {
    sprintf(s, "%lld,%03lld,%03lld"
             ,  x/1000000
             , (x/1000)%1000
             ,  x%1000);
  }
  else if (x < 1000000000000)
  {
    sprintf(s, "%lld,%03lld,%03lld,%03lld"
             ,  x/1000000000
             , (x/1000000)%1000
             , (x/1000)%1000
             ,  x%1000);
  }
  else if (x < 1000000000000000)
  {
    sprintf(s, "%lld,%03lld,%03lld,%03lld,%03lld"
             ,  x/1000000000000
             , (x/1000000000)%1000
             , (x/1000000)%1000
             , (x/1000)%1000
             ,  x%1000);
  }
  else if (x < 1000000000000000000)
  {
    sprintf(s, "%lld,%03lld,%03lld,%03lld,%03lld,%03lld"
             ,  x/1000000000000000
             , (x/1000000000000)%1000
             , (x/1000000000)%1000
             , (x/1000000)%1000
             , (x/1000)%1000
             ,  x%1000);
  }
  else
  {
    sprintf(s, "%lld,%03lld,%03lld,%03lld,%03lld,%03lld,%03lld"
             ,  x/1000000000000000000
             , (x/1000000000000000)%1000
             , (x/1000000000000)%1000
             , (x/1000000000)%1000
             , (x/1000000)%1000
             , (x/1000)%1000
             ,  x%1000);
  }

  return buffer;
}

/*
 * This function runs after main(), and is used to print final GC and memory
 * statistics (if enabled). This is similar to the +RTS -s option.
 */
__attribute__((destructor)) void dump_gc_stats(void)
{
  if (__dump_gc_stats) {
    /*
     * int64 ranges from -9223372036854775807..9223372036854775807, so we need a
     * buffer size of at least 27 characters (including the terminating \0) to
     * format any numbers with commas.
     */
    char buffer[96];
    double timestamp = clock_gettime_elapsed_seconds();

    fprintf(stderr, "\n");
    fprintf(stderr, "[%8.3f] gc: %s bytes allocated locally\n", timestamp, format_int64(buffer, __total_bytes_allocated_local));
    fprintf(stderr, "[%8.3f] gc: %s bytes allocated on the remote device\n", timestamp, format_int64(buffer, __total_bytes_allocated_remote));
    fprintf(stderr, "[%8.3f] gc: %s bytes copied to the remote device\n", timestamp, format_int64(buffer, __total_bytes_copied_to_remote));
    fprintf(stderr, "[%8.3f] gc: %s bytes copied from the remote device\n", timestamp, format_int64(buffer, __total_bytes_copied_from_remote));
    fprintf(stderr, "[%8.3f] gc: %s bytes evicted from the remote (%s evictions, %s GCs)\n", timestamp, format_int64(&buffer[0], __total_bytes_evicted_from_remote), format_int64(&buffer[32], __num_evictions), format_int64(&buffer[64], __num_remote_gcs));
  }
}

#endif /* ACCELERATE_DEBUG */

