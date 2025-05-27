/*
 * Module      : Data.Array.Accelerate.Debug.Flags
 * Copyright   : [2017..2020] The Accelerate Team
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
#include <inttypes.h>
#include <libgen.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "flags.h"
#include "getopt.h"


/* SEE: [layout of command line options bitfield]
 * There are 7 default-enabled options, followed by 1 default-disabled option,
 * followed by 17 debug options.
 * Note the bit trick: ((1 << n) - 1) is a number with the lowest n bits set. */
static const uint32_t def_enabled_opts_bitfield = { (1<<7) - 1 };
#ifndef ACCELERATE_DEBUG
static const uint32_t debug_opts_bitfield = { ((1<<17) - 1) << (7+1) };
#endif
static const int disable_opts_offset = (7+1+17);

/* This global is accessed from the Haskell side. */
__flags_t __cmd_line_flags            = { def_enabled_opts_bitfield };

enum {
  OPT_ENABLE = 1,
  OPT_DISABLE,
};

/* NOTE: [layout of command line options bitfield]
 *
 * HERE BE DRAGONS.
 *
 * When adding, removing, reordering, or changing options in ANY way, be aware
 * of the following:
 * - Various code relies on the fact (by bit hacks) that these options come in
 *   this order: -f enablers, -d enablers, -f disablers.
 * - The -f enablers and -f disablers lists must be exactly the same, including
 *   the order.
 * - The order of the options in __flags_t in flags.h must also be the same.
 * - Data.Array.Accelerate.Debug.Internal.Flags contains 2 blocks of code
 *   hard-coding offsets into this options list.
 * - Some metrics about this options list used in the bit hacks are at the top
 *   of this file (def_enabled_opts_bitfield etc.).
 */
static const char*         shortopts  = "";
static const struct option longopts[] =
  { { "fseq-sharing",                   no_argument,       NULL, OPT_ENABLE                    }
  , { "facc-sharing",                   no_argument,       NULL, OPT_ENABLE                    }
  , { "fexp-sharing",                   no_argument,       NULL, OPT_ENABLE                    }
  , { "ffusion",                        no_argument,       NULL, OPT_ENABLE                    }
  , { "finplace",                       no_argument,       NULL, OPT_ENABLE                    }
  , { "ffast-math",                     no_argument,       NULL, OPT_ENABLE                    }
  , { "ffast-permute-const",            no_argument,       NULL, OPT_ENABLE                    }
  , { "fforce-recomp",                  no_argument,       NULL, OPT_ENABLE                    }

  , { "ddebug",                         no_argument,       NULL, OPT_ENABLE                    }
  , { "dverbose",                       no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-phases",                   no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-sharing",                  no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-fusion",                   no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-simpl-stats",              no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-simpl-iterations",         no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-vectorisation",            no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-dot",                      no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-simpl-dot",                no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-gc",                       no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-gc-stats",                 no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-cc",                       no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-ld",                       no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-asm",                      no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-exec",                     no_argument,       NULL, OPT_ENABLE                    }
  , { "ddump-sched",                    no_argument,       NULL, OPT_ENABLE                    }

  , { "fno-seq-sharing",                no_argument,       NULL, OPT_DISABLE                   }
  , { "fno-acc-sharing",                no_argument,       NULL, OPT_DISABLE                   }
  , { "fno-exp-sharing",                no_argument,       NULL, OPT_DISABLE                   }
  , { "fno-fusion",                     no_argument,       NULL, OPT_DISABLE                   }
  , { "fno-inplace",                    no_argument,       NULL, OPT_DISABLE                   }
  , { "fno-fast-math",                  no_argument,       NULL, OPT_DISABLE                   }
  , { "fno-fast-permute-const",         no_argument,       NULL, OPT_DISABLE                   }
  , { "fno-force-recomp",               no_argument,       NULL, OPT_DISABLE                   }

  /* There were options that took arguments here before; see the git blame of
   * this comment for how that looked. */

  /* required sentinel */
  , { NULL, 0, NULL, 0 }
  };


/* Parse the given vector of command line arguments and set the corresponding
 * flags. The vector should contain no non-option arguments (aside from the name
 * of the program as the first entry, which is required for getopt()).
 */
static void parse_options(int argc, char *argv[])
{
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
      __cmd_line_flags.bitfield &= ~(1 << (longindex - disable_opts_offset));
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
#if !defined(ACCELERATE_DEBUG)
  if (__cmd_line_flags.bitfield & debug_opts_bitfield) {
    fprintf(stderr, "Data.Array.Accelerate: Debugging options are disabled.\n");
    fprintf(stderr, "Reinstall package 'accelerate' with '-fdebug' to enable them.\n");
  }
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
 * not update the 'argc' length of the vector, the removed entries are replaced
 * with "-RTS" (see the comment at the end of the function).
 */
static void process_options(int argc, char *argv[])
{
  /* Find the command line options which need to be processed. These will be
   * between +ACC ... [-ACC] (similar to the Haskell RTS options).
   *
   * First we collect the total number of command-line options. We also
   * already store what occurs where in the argument list, so that we only have
   * to do the complicated parsing once.
   *
   * Note that this function may well be called twice; this probably has
   * something to do with runtime loading of binaries in e.g.
   * accelerate-llvm-native (but I'm not sure). If so, we have already parsed
   * out +ACC stuff the first time round, and the GHC RTS has already removed
   * the +RTS flags including the -RTS drop-ins that we replaced the +ACC
   * arguments with. It does that by reordering arguments so that the non-RTS
   * ones come first, and by replacing the first not-an-argument-anymore with
   * NULL.
   *
   * Long story short, if we encounter a NULL, we have encountered what is,
   * according to the GHC RTS, de-facto the end of the argument list. So we
   * update argc and exit the loop.
   */
  typedef enum {
    PROC_OPT_OTHER,   /* some non-accelerate argument */
    PROC_OPT_MARKER,  /* +ACC or -ACC (not +/-RTS!) */
    PROC_OPT_OPT,     /* an option for accelerate */
  } cl_option_t;
  cl_option_t *cl_option_type = malloc(argc * sizeof(cl_option_t));
  if (argc > 0) cl_option_type[0] = PROC_OPT_OTHER;

  int num_cl_options = 0;  /* the number of PROC_OPT_OPT */

  {
    bool in_rts = false;
    bool in_acc = false;
    for (int i = 1; i < argc; ++i) {
      if (NULL == argv[i]) {  /* see above */
        argc = i;
        break;
      }

      /* the default, overriden in the case analysis below */
      cl_option_type[i] = PROC_OPT_OTHER;

      if (0 == strncmp("+RTS", argv[i], 4)) {
        if (in_acc) {
          fprintf(stderr,
            "accelerate: error: a '+RTS' option found inside a '+ACC' block. Close the '+ACC'\n"
            "block using '-ACC' before opening a '+RTS' block. Continuing, assuming a '-ACC'.\n"
          );
          in_acc = false;
        }
        in_rts = true;  /* let's not error on +RTS +RTS */

      } else if (0 == strncmp("-RTS", argv[i], 4)) {
        if (in_acc) {
          fprintf(stderr,
            "accelerate: error: a '-RTS' option found inside a '+ACC' block. Close the '+ACC'\n"
            "block using '-ACC' before opening a '+RTS' block. Continuing, assuming a '-ACC'.\n"
          );
          in_acc = false;
        }
        in_rts = false;

      } else if (0 == strncmp("+ACC", argv[i], 4)) {
        if (in_rts) {
          fprintf(stderr,
            "accelerate: error: a '+ACC' option found inside a '+RTS' block. Close the '+RTS'\n"
            "block using '-RTS' before opening a '+ACC' block.\n"
          );
        } else {
          in_acc = true;
          cl_option_type[i] = PROC_OPT_MARKER;
        }

      } else if (0 == strncmp("-ACC", argv[i], 4)) {
        /* inside +RTS, just leave them alone; the GHC RTS will error for us */
        if (!in_rts) {
          cl_option_type[i] = PROC_OPT_MARKER;
          in_acc = false;
        }

      } else {
        /* a normal argument */
        if (in_acc) {
          cl_option_type[i] = PROC_OPT_OPT;
          ++num_cl_options;
        }
      }
    }
  }

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
  int    argc2 = 1 + num_env_options + num_cl_options;
  char** argv2 = NULL;

  if (argc2 > 1) {
    char** r = argv2 = malloc(argc2 * sizeof(char*));

    /* program name */
    *r++ = argv[0];

    /* environment variables */
    if (env) {
      char* p = env;
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
    for (int i = 1; i < argc; ++i) {
      if (cl_option_type[i] == PROC_OPT_OPT) {
        *r++ = argv[i];
      }
    }

    /* finally process command lines */
    parse_options(argc2, argv2);
  }

  /* Remove the Accelerate options from the command line arguments which will be
   * passed to main(). We can't do this in a sensible fashion by updating argc,
   * but we can pull a small sleight-of-hand by rewriting them to -RTS, so that
   * they will be deleted by the GHC RTS when it is initialised.
   *
   * In this method, we can also update them in place, without permuting the
   * order of the options to place the (now unused) Accelerate flags at the end
   * of the vector.
   *
   * Note that we do not have to worry about a +RTS +ACC situation where this
   * replacement would change semantics, because we did not parse +ACC arguments
   * inside a +RTS block above.
   */
  for (int i = 1; i < argc; ++i) {
    /* Replace markers _and_ accelerate options. */
    if (cl_option_type[i] == PROC_OPT_MARKER ||
          cl_option_type[i] == PROC_OPT_OPT) {
      if (strlen(argv[i]) >= 4) {
        strcpy(argv[i], "-RTS");
      } else {
        argv[i] = malloc(5);  /* 4 + the zero byte */
        strcpy(argv[i], "-RTS");
      }
    }
  }

  /* cleanup */
  if (cl_option_type) free(cl_option_type);
  if (argv2) free(argv2);
  if (env) free(env);
}

/* On Windows, the GHC RTS uses GetCommandLineW() to get the actual command line
 * using the Windows API; the memory that this function reads from cannot easily
 * be modified. Supposedly one can locate the PEB and modify the string
 * in-place, but that is too much hackery. So we'll just disable +ACC parsing on
 * Windows. */
#ifndef _WIN32

/* On MacOS, we use a constructor attribute, because .init_array seems to be a
 * Linux-only thing. */
#if defined(__APPLE__) && defined(__MACH__)
__attribute__((constructor))
static void process_options_constructor(int argc, char *argv[]) {
  process_options(argc, argv);
}
#else
/* On Linux(/BSD? Do we even support that?), register process_options() as a
 * constructor function in the new style by putting a reference to it in the
 * .init_array section. The advantage of this approach over simply using
 * __attribute__((constructor)) is that this way, the function will predictably
 * be called with the same arguments as main(). A simple constructor might
 * _accidentally_ be called with the same arguments as main(), but it isn't
 * defined to be, and sometimes will not be. (In particular, this failed with
 * clang on Windows, which is a bad reason to do this on Linux, but whatever.)
 * Source: https://stackoverflow.com/a/37358751 */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
/* Add 'used' so that the variable is not optimised away. */
__attribute__((section(".init_array"), used))
  static void *process_options_ctor_entry = &process_options;
#pragma GCC diagnostic pop
#endif /* APPLE */
#endif /* WIN32 */
