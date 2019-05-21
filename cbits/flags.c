/*
 * Module      : Data.Array.Accelerate.Debug.Flags
 * Copyright   : [2017..2019] The Accelerate Team
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
 * Stability   : experimental
 * Portability : non-portable (GHC extensions)
 *
 * Option parsing for debug flags. This is a translation of the module
 * Data.Array.Accelerate.Debug.Flags into C, so that we can implement it at
 * program initialisation.
 *
 * This processes flags between +ACC ... -ACC on the command line. The
 * corresponding fields are removed from the command line. Note that we can't at
 * this stage update the number of command line arguments, but with some tricks
 * they can be mostly deleted.
 */

#include <ctype.h>
#include <getopt.h>
#include <libgen.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "flags.h"
#include "HsFFI.h"


/* These globals will be accessed from the Haskell side to implement the
 * corresponding behaviour.
 */

__flags_t __cmd_line_flags          = { 0b111111 };
HsInt     __unfolding_use_threshold = 1;


#if defined(ACCELERATE_DEBUG)

enum {
  OPT_ENABLE = 1,
  OPT_DISABLE,
  OPT_UNFOLDING_USE_THRESHOLD
};

/* NOTE: [adding new command line options]
 *
 * When adding new options, make sure the offset value in the OPT_DISABLE branch
 * is updated, and that the flags are kept in order.
 */
static const char*         shortopts  = "";
static const struct option longopts[] =
  { { "fseq-sharing",                 no_argument,       NULL, OPT_ENABLE                  }
  , { "facc-sharing",                 no_argument,       NULL, OPT_ENABLE                  }
  , { "fexp-sharing",                 no_argument,       NULL, OPT_ENABLE                  }
  , { "ffusion",                      no_argument,       NULL, OPT_ENABLE                  }
  , { "fsimplify",                    no_argument,       NULL, OPT_ENABLE                  }
  , { "ffast-math",                   no_argument,       NULL, OPT_ENABLE                  }
  , { "fflush-cache",                 no_argument,       NULL, OPT_ENABLE                  }
  , { "fforce-recomp",                no_argument,       NULL, OPT_ENABLE                  }

  , { "ddebug",                       no_argument,       NULL, OPT_ENABLE                  }
  , { "dverbose",                     no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-phases",                 no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-sharing",                no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-fusion",                 no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-simpl-stats",            no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-simpl-iterations",       no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-vectorisation",          no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-dot",                    no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-simpl-dot",              no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-gc",                     no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-gc-stats",               no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-cc",                     no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-ld",                     no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-asm",                    no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-exec",                   no_argument,       NULL, OPT_ENABLE                  }
  , { "ddump-sched",                  no_argument,       NULL, OPT_ENABLE                  }

  , { "fno-seq-sharing",              no_argument,       NULL, OPT_DISABLE                 }
  , { "fno-acc-sharing",              no_argument,       NULL, OPT_DISABLE                 }
  , { "fno-exp-sharing",              no_argument,       NULL, OPT_DISABLE                 }
  , { "fno-fusion",                   no_argument,       NULL, OPT_DISABLE                 }
  , { "fno-simplify",                 no_argument,       NULL, OPT_DISABLE                 }
  , { "fno-fast-math",                no_argument,       NULL, OPT_DISABLE                 }
  , { "fno-flush-cache",              no_argument,       NULL, OPT_DISABLE                 }
  , { "fno-force-recomp",             no_argument,       NULL, OPT_DISABLE                 }

  , { "funfolding-use-threshold=INT", required_argument, NULL, OPT_UNFOLDING_USE_THRESHOLD }

  /* required sentinel */
  , { NULL, 0, NULL, 0 }
  };

#endif /* ACCELERATE_DEBUG */


/* Parse the given vector of command line arguments and set the corresponding
 * flags. The vector should contain no non-option arguments (aside from the name
 * of the program as the first entry, which is required for getopt()).
 */
static void parse_options(int argc, char *argv[])
{
#if defined(ACCELERATE_DEBUG)

  const struct option* opt;
  char* this;
  int   did_show_banner;
  int   prefix;
  int   result;
  int   longindex;

  while (-1 != (result = getopt_long_only(argc, argv, shortopts, longopts, &longindex)))
  {
    switch(result)
    {
    /* the option flag was set */
    case 0:
      break;

    case OPT_ENABLE:
      __cmd_line_flags.bitfield |= 1 << longindex;
      break;

    case OPT_DISABLE:
      __cmd_line_flags.bitfield &= ~(1 << (longindex - 25));  // SEE: [adding new command line options]
      break;

    /* attempt to decode the argument to flags which require them */
    case OPT_UNFOLDING_USE_THRESHOLD:
      if (1 != sscanf(optarg, "%lld", &__unfolding_use_threshold)) {
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
     *      whether we got here due to a missing argument or ambiguous option.
     */
    case ':':
    case '?':
      opt             = longopts;
      this            = argv[optind-1];
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

#else

  fprintf(stderr, "Data.Array.Accelerate: Debugging options are disabled.\n");
  fprintf(stderr, "Reinstall package 'accelerate' with '-fdebug' to enable them.\n");

#endif
}


/* This function will be run automatically before main() to process options sent
 * to the Accelerate runtime system.
 *
 * This processes both command line flags as well as those specified via the
 * environment variable "ACCELERATE_FLAGS" (with precedence to the former).
 *
 * The input 'argv' vector is mutated to remove the entries processed by this
 * module. This prevents the flags from interfering with the regular Haskell
 * program (in the same way as the RTS options). Note however that since we can
 * not update the 'argc' length of the vector, the removed entries are simply
 * set to NULL (and moved to the end of the vector).
 */
__attribute__((constructor)) void process_options(int argc, char *argv[])
{
  int i;

  /* Find the command line options which need to be processed. These will be
   * between +ACC ... [-ACC] (similar to the Haskell RTS options).
   *
   * Note that this only recognises a single +ACC ... -ACC group. Should we be
   * able to handle multiple (disjoint) groups of flags? To do this properly we
   * probably want to collect the arguments (from both sources) into a linked
   * list. This would not be particularly difficult, just tedious... \:
   */
  int cl_start;
  int cl_end;
  int num_cl_options = 0;

  for (cl_start = 1; cl_start < argc; ++cl_start) {
    if (0 == strncmp("+ACC", argv[cl_start], 4)) {
      break;
    }
  }

  for (cl_end = cl_start+1; cl_end < argc; ++cl_end) {
    if (0 == strncmp("-ACC", argv[cl_end], 4)) {
      break;
    }
  }
  num_cl_options = cl_end-cl_start-1;

  /* Gather options from the ACCELERATE_FLAGS environment variable. Note that we
   * must not modify this variable, otherwise subsequent invocations of getenv()
   * will get the modified version.
   */
  char *env            = getenv("ACCELERATE_FLAGS");
  int  num_env_options = 0;

  if (NULL != env) {
    /* copy the environment string, as we will mutate it during tokenisation */
    char *p = env = strdup(env);

    /* first count how many tokens there are, so that we can allocate memory for
     * the combined options vector
     */
    while (*p) {
      while (*p && isspace(*p)) ++p;

      if (*p) {
        ++num_env_options;
        while (*p && !isspace(*p)) ++p;
      }
    }
  }

  /* Create the combined options vector containing both the environment and
   * command line options for parsing. The command line options are placed at
   * the end, so that they may override environment options.
   */
  int    argc2 = num_cl_options + num_env_options + 1;
  char** argv2 = NULL;

  if (argc2 > 1) {
    char*  p = env;
    char** r = argv2 = malloc(argc2 * sizeof(char*));

    /* program name */
    *r++ = argv[0];

    /* environment variables */
    if (p) {
      while (*p) {
        while (*p && isspace(*p)) ++p;

        if (*p) {
          *r++ = p;
          while (*p && !isspace(*p)) ++p;

          if (isspace(*p)) {
            *p++ = '\0';
          }
        }
      }
    }

    /* command line flags */
    for (i = cl_start+1; i < cl_end; ++i)
      *r++ = argv[i];

    /* finally process command lines */
    parse_options(argc2, argv2);
  }

  /* Remove the Accelerate options from the command line arguments which will be
   * passed to main(). We can't do this in a sensible fashion by updating argc,
   * but we can pull a small sleight-of-hand by rewriting them to -RTS, so that
   * they will be deleted by the GHC RTS when it is initialised.
   *
   * In this method, we can also updated them in place, without permuting the
   * order of the options to place the (now unused) Accelerate flags at the end
   * of the vector. This does create a slight change in behaviour though, where
   * the application will become more lenient to the user not (correctly)
   * closing the RTS group, for example:
   *
   * > ./foo +RTS -... +ACC -... -ACC
   *
   * is rewritten to:
   *
   * > ./foo +RTS -... -RTS -... -RTS
   *
   * Previously, since the RTS group was not terminated correctly the GHC RTS
   * would complain that the trailing Accelerate options (+ACC -...) were
   * unknown RTS flags.
   */
  for (i = cl_start; i < cl_end+1 && i < argc; ++i) {
    if (strlen(argv[i]) >= 4) {
      strcpy(argv[i], "-RTS");
    } else {
      argv[i][0] = '\0';
    }
  }

  /* cleanup */
  if (argv2) free(argv2);
  if (env)   free(env);
}

