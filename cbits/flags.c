/*
 * Module      : Data.Array.Accelerate.Debug.Flags
 * Copyright   : [2017] Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 * Portability : non-portable (GHC extensions)
 *
 * Option parsing for debug flags. This is a translation of the module
 * Data.Array.Accelerate.Debug.Flags into C, so that we can implement it at
 * program initialisation.
 *
 * This processes flags between +ACC ... -ACC on the command line. The
 * corresponding fields are removed from the command line. Note that we can't at
 * this stage update the number of command line arguments, so the entries are
 * simply set to NULL.
 */

#include <getopt.h>
#include <libgen.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* These globals will be accessed from the Haskell side to implement the
 * corresponding behaviour.
 */
int32_t __acc_sharing               = 1;
int32_t __exp_sharing               = 1;
int32_t __fusion                    = 1;
int32_t __simplify                  = 1;
int32_t __unfolding_use_threshold   = 1;
int32_t __fast_math                 = 1;
int32_t __flush_cache               = 0;
int32_t __force_recomp              = 0;

int32_t __verbose                   = 0;
int32_t __dump_phases               = 0;
int32_t __dump_simpl_stats          = 0;
int32_t __dump_simpl_iterations     = 0;
int32_t __dump_vectorisation        = 0;
int32_t __dump_dot                  = 0;
int32_t __dump_simpl_dot            = 0;
int32_t __dump_gc                   = 0;
int32_t __dump_gc_stats             = 0;
int32_t __dump_cc                   = 0;
int32_t __dump_ld                   = 0;
int32_t __dump_asm                  = 0;
int32_t __dump_exec                 = 0;
int32_t __dump_sched                = 0;

static const char*         shortopts  = "";
static const struct option longopts[] =
  { { "dverbose",                     no_argument,       &__verbose,               1    }
  , { "ddump-phases",                 no_argument,       &__dump_phases,           1    }
  , { "ddump-simpl-stats",            no_argument,       &__dump_simpl_stats,      1    }
  , { "ddump-simpl-iterations",       no_argument,       &__dump_simpl_iterations, 1    }
  , { "ddump-vectorisation",          no_argument,       &__dump_vectorisation,    1    }
  , { "ddump-dot",                    no_argument,       &__dump_dot,              1    }
  , { "ddump-simpl-dot",              no_argument,       &__dump_simpl_dot,        1    }
  , { "ddump-gc",                     no_argument,       &__dump_gc,               1    }
  , { "ddump-gc-stats",               no_argument,       &__dump_gc_stats,         1    }
  , { "ddump-cc",                     no_argument,       &__dump_cc,               1    }
  , { "ddump-ld",                     no_argument,       &__dump_ld,               1    }
  , { "ddump-asm",                    no_argument,       &__dump_asm,              1    }
  , { "ddump-exec",                   no_argument,       &__dump_exec,             1    }
  , { "ddump-sched",                  no_argument,       &__dump_sched,            1    }

  , { "facc-sharing",                 no_argument,       &__acc_sharing,           1    }
  , { "fexp-sharing",                 no_argument,       &__exp_sharing,           1    }
  , { "ffusion",                      no_argument,       &__fusion,                1    }
  , { "fsimplify",                    no_argument,       &__simplify,              1    }
  , { "fflush-cache",                 no_argument,       &__flush_cache,           1    }
  , { "fforce-recomp",                no_argument,       &__force_recomp,          1    }
  , { "ffast-math",                   no_argument,       &__fast_math,             1    }

  , { "fno-acc-sharing",              no_argument,       &__acc_sharing,           0    }
  , { "fno-exp-sharing",              no_argument,       &__exp_sharing,           0    }
  , { "fno-fusion",                   no_argument,       &__fusion,                0    }
  , { "fno-simplify",                 no_argument,       &__simplify,              0    }
  , { "fno-flush-cache",              no_argument,       &__flush_cache,           0    }
  , { "fno-force-recomp",             no_argument,       &__force_recomp,          0    }
  , { "fno-fast-math",                no_argument,       &__fast_math,             0    }

  , { "funfolding-use-threshold=INT", required_argument, NULL,                     1000 }

  /* required sentinel */
  , { NULL, 0, NULL, 0 }
  };


/* This function will be run before main() and process the command line options
 * before the Haskell environment is initialised. This prevents the flags from
 * interfering with the regular Haskell program (in the same way as the RTS
 * options).
 */
__attribute__((constructor)) void process_options(int argc, char *argv[])
{
  const struct option* opt;
  char* this;
  int   did_show_banner;
  int   end;
  int   prefix;
  int   result;
  int   start;
  int   longindex;

  /* Determine the subset of the options which we should process. Note that
   * getopt() implicitly skips the first element of the input vector (as this is
   * usually the name of the program) so when calling getopt() we point to the
   * +ACC entry, rather than the first proper option.
   */
  for (start = 1; start < argc; ++start) {
    if (0 == strncmp("+ACC", argv[start], 4)) {
      break;
    }
  }

  for (end = start+1; end < argc; ++end) {
    if (0 == strncmp("-ACC", argv[end], 4)) {
      break;
    }
  }

  while (-1 != (result = getopt_long_only(end-start, &argv[start], shortopts, longopts, &longindex)))
  {
    switch(result)
    {
    /* the option flag was set */
    case 0:
      break;

    /* attempt to decode the argument to flags which require them */
    case 1000:
      if (1 != sscanf(optarg, "%d", &__unfolding_use_threshold)) {
        fprintf(stderr, "%s: option `-%s' requires an integer argument, but got: %s\n"
                      , basename(argv[0])
                      , longopts[longindex].name
                      , optarg
                      );
      }
      break;

    /* option was ambiguous or was missing a required argument
     *
     * TLM: longindex is not being updated correctly on my system for the case
     *      of an ambiguous argument, which makes it tricker to directly test
     *      whether the option we got here due to a missing argument or
     *      ambiguous option.
     */
    case ':':
    case '?':
      opt             = longopts;
      this            = argv[start+optind-1];
      did_show_banner = 0;

      /* drop the leading '-' from the input command line argument */
      while (*this) {
        if ('-' == *this) {
          ++this;
        } else {
          break;
        }
      }
      prefix = strlen(this);

      /* display any options which are a prefix match for the ambiguous option */
      while (opt->name) {
        if (0 == strncmp(opt->name, this, prefix)) {
          /* only here can we determine if this was a missing argument case */
          if (opt->has_arg == required_argument)
            break;

          /* only show the banner if there are possible matches */
          if (0 == did_show_banner) {
            did_show_banner = 1;
            fprintf(stderr, "Did you mean one of these?\n");
          }
          fprintf(stderr, "    -%s\n", opt->name);
        }
        ++opt;
      }
      break;

    default:
      fprintf(stderr, "failed to process command line options (%d)\n", result);
      abort();
    }
  }

  /* After stripping out the options flags, shuffle the remaining options to the
   * front of the vector. This still leaves some empty fields at the end of the
   * argument vector (as we can not update argc), but that is probably better
   * than having them in the middle..?
   */
  if (start < argc) {
    int remaining = argc-end;

    if (end < argc) {
      memmove(&argv[start], &argv[end+1], remaining * sizeof(char*));
    }
    memset(&argv[start+remaining], 0, (end-start) * sizeof(char*));
  }
}


/*
 * This function runs after main(), and is used to print final GC and memory
 * statistics (if enabled). This is similar to the +RTS -s option.
 */
__attribute__((destructor)) void dump_gc_stats(void)
{
}

