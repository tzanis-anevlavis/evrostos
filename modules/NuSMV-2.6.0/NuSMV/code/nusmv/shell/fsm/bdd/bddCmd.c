/* ---------------------------------------------------------------------------

 This file is part of the ``mc'' package of NuSMV version 2.
  Copyright (C) 2003 by FBK-irst.

  NuSMV version 2 is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  NuSMV version 2 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA.

  For more information on NuSMV see <http://nusmv.fbk.eu>
  or email to <nusmv-users@fbk.eu>.
  Please report bugs to <nusmv-users@fbk.eu>.

  To contact the NuSMV development board, email to <nusmv@fbk.eu>.

-----------------------------------------------------------------------------*/

/*!
  \author Marco Roveri
  \brief Bdd FSM commands

  This file contains all the shell commands to dela with
  computation and printing of reachable states, fair states and fair
  transitions.

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/shell/cmd/cmd.h"
#include "nusmv/shell/fsm/bdd/bddCmd.h"

#include "nusmv/core/fsm/bdd/bdd.h"
#include "nusmv/core/compile/compile.h"

#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/prop/propPkg.h"
#include "nusmv/core/parser/parser.h"
#include "nusmv/core/utils/ErrorMgr.h"

#include <math.h> /* for log10 */


int CommandCheckFsm(NuSMVEnv_ptr env, int argc, char** argv);
int CommandComputeReachable(NuSMVEnv_ptr env, int argc, char** argv);
int CommandPrintReachableStates(NuSMVEnv_ptr env, int argc, char** argv);
int CommandPrintFairStates(NuSMVEnv_ptr env, int argc, char** argv);
int CommandPrintFairTransitions(NuSMVEnv_ptr env, int argc, char** argv);
int CommandPrintFairStateInputPairs(NuSMVEnv_ptr env, int argc, char** argv);
int CommandDumpFsm(NuSMVEnv_ptr env, int argc, char** argv);

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
extern cmp_struct_ptr cmps;

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static int UsageCheckFsm(const NuSMVEnv_ptr env);
static int UsageComputeReachable(const NuSMVEnv_ptr env);
static int UsagePrintReachableStates(const NuSMVEnv_ptr env);
static int UsagePrintFairStates(const NuSMVEnv_ptr env);
static int UsagePrintFairTransitions(const NuSMVEnv_ptr env);
static int UsagePrintFairStateInputPairs(const NuSMVEnv_ptr env);
static int UsageDumpFsm(const NuSMVEnv_ptr env);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Bdd_Init(NuSMVEnv_ptr env)
{
  Cmd_CommandAdd(env, "check_fsm", CommandCheckFsm, 0, false);
  Cmd_CommandAdd(env, "print_reachable_states", CommandPrintReachableStates,
                 0, false);
  Cmd_CommandAdd(env, "compute_reachable", CommandComputeReachable, 0, false);
  Cmd_CommandAdd(env, "print_fair_states", CommandPrintFairStates, 0, false);
  Cmd_CommandAdd(env, "print_fair_transitions", CommandPrintFairTransitions,
                 0, false);
  Cmd_CommandAdd(env, "_print_fair_state_input_pairs",
                 CommandPrintFairStateInputPairs, 0, false);
  Cmd_CommandAdd(env, "dump_fsm", CommandDumpFsm, 0, false);
}

void Bdd_End(NuSMVEnv_ptr env)
{
  boolean status = true;

  status = status && Cmd_CommandRemove(env, "check_fsm");
  status = status && Cmd_CommandRemove(env, "print_reachable_states");
  status = status && Cmd_CommandRemove(env, "compute_reachable");
  status = status && Cmd_CommandRemove(env, "print_fair_states");
  status = status && Cmd_CommandRemove(env, "print_fair_transitions");
  status = status && Cmd_CommandRemove(env, "dump_fsm");

  nusmv_assert(status);
}

/*!
  \command{check_fsm} Checks the transition relation for totality.

  \command_args{[-h] [-m | -o output-file]}


  Checks if the transition relation is total. If the transition
  relation is not total then a potential deadlock state is shown out.
  <p>
  Command options:<p>
  <dl>
    <dt> <tt>-m</tt>
       <dd> Pipes the output generated by the command to the program
            specified by the <tt>PAGER</tt> shell variable if
            defined, else through the UNIX command "more".
    <dt> <tt>-o output-file</tt>
       <dd> Writes the output generated by the command to the file
       <tt>output-file</tt>.
  </dl>
  At the beginning reachable states are computed in order to guarantee
  that deadlock states are actually reachable.
*/

int CommandCheckFsm(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int c;
  int useMore = 0;
  char * dbgFileName = NIL(char);
#if NUSMV_HAVE_GETENV
  char * pager;
#endif
  FILE * old_outstream = NIL(FILE);

  util_getopt_reset();
  while ((c = util_getopt(argc,argv,"hmo:")) != EOF) {
    switch (c) {
    case 'h': return UsageCheckFsm(env);
    case 'o':
      if (useMore == 1) return UsageCheckFsm(env);
      dbgFileName = util_strsav(util_optarg);
      StreamMgr_print_output(streams,  "Output to file: %s\n", dbgFileName);
      break;
    case 'm':
      if (dbgFileName != NIL(char)) return UsageCheckFsm(env);
      useMore = 1;
      break;
    default:  return UsageCheckFsm(env);
    }
  }

  if (Compile_check_if_model_was_built(env, errstream, true)) return 1;

  if (argc != util_optind) return UsageCheckFsm(env);

  if (useMore) {
#if NUSMV_HAVE_POPEN
    old_outstream = outstream;
#if NUSMV_HAVE_GETENV
    pager = getenv("PAGER");
    if (pager == NULL) {
      outstream = popen("more", "w");
      if (outstream == NULL) {
        StreamMgr_print_error(streams,  "Unable to open pipe with \"more\".\n");
        outstream = old_outstream;
        return 1;
      }
    }
    else {
      outstream = popen(pager, "w");
      if (outstream == NULL) {
        StreamMgr_print_error(streams,  "Unable to open pipe with \"%s\".\n", pager);
        outstream = old_outstream;
        return 1;
      }
    }
#else /* NUSMV_HAVE_GETENV */
    outstream = popen("more", "w");
    if (outstream == NULL) {
      StreamMgr_print_error(streams,  "Unable to open pipe with \"more\".\n");
      outstream = old_outstream;
      return 1;
    }
#endif /* NUSMV_HAVE_GETENV */

#else /* NUSMV_HAVE_POPEN */
    StreamMgr_print_error(streams,  "Pipe is not supported\n");
    return 1;
#endif /* NUSMV_HAVE_POPEN */
  }
  if (dbgFileName != NIL(char)) {
    old_outstream = outstream;
    outstream = fopen(dbgFileName, "w");
    if (outstream == NULL) {
      StreamMgr_print_error(streams,  "Unable to open file \"%s\".\n", dbgFileName);
      outstream = old_outstream;
      return 1;
    }
  }

  BddFsm_check_machine(BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM)));
  set_forward_search(opts);

#if NUSMV_HAVE_POPEN
  if (useMore) {
    pclose(outstream);
    outstream = old_outstream;
  }
#endif

  if (dbgFileName != NIL(char)) {
    fflush(outstream);
    fclose(outstream);
    outstream = old_outstream;
  }
  return 0;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageCheckFsm(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: check_fsm [-h] [-m | -o file]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -m \t\tPipes output through the program specified by\n");
  StreamMgr_print_error(streams,  "      \t\tthe \"PAGER\" environment variable if defined,\n");
  StreamMgr_print_error(streams,  "      \t\telse through the UNIX command \"more\".\n");
  StreamMgr_print_error(streams,  "   -o file\tWrites the generated output to \"file\".\n");
  return 1;
}

/*!
  \command{compute_reachable} Computes the set of reachable states

  \command_args{[-h] [-k number]}

  The set of reachable states is used to simplify
  image and preimage computations. This can result in improved
  performances for models with sparse state spaces.
  <p>
  Command Options:<p>
  <dl>
    <dt> <tt>-k number</tt>
    <dd> Provides an explicit bound to perform at most "number"
    steps.
    <dt> <tt>-t number</tt> <dd> Provides a fail cut-off maximum
    CPU time to halt the computation. This option can be used to limit
    execution time.
  </dl>
*/

int CommandComputeReachable(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  int c, k, t;
  boolean used_k, used_t, completed;
  BddFsm_ptr fsm;
  int diameter = 0;

  used_k = false;
  used_t = false;

  util_getopt_reset();
  while ((c = util_getopt(argc,argv,"t:k:h")) != EOF) {
    switch (c) {
    case 'h': return UsageComputeReachable(env);
    case 'k':
      {
        int res;

        if(used_k) {
          StreamMgr_print_error(streams,  "You cannot specify -k more than once!\n");
        }
        used_k = true;

        res = sscanf(util_optarg, "%d", &k);
        if (res <= 0) {
          StreamMgr_print_error(streams,  "You must specify a valid integer number as k!\n");
          return 1;
        }
        if (k <= 0) {
          StreamMgr_print_error(streams,  "You must specify a positive number as k!\n");
          return 1;
        }

        break;
      }
    case 't':
      {
        int res;

        if(used_t) {
          StreamMgr_print_error(streams,  "You cannot specify -t more than once!\n");
        }
        used_t = true;

        res = sscanf(util_optarg, "%d", &t);
        if (res <= 0) {
          StreamMgr_print_error(streams,  "You must specify a valid integer number as time!\n");
          return 1;
        }
        if (t <= 0) {
          StreamMgr_print_error(streams,  "You must specify a positive number as time!\n");
          return 1;
        }

        break;
      }
    default:  return UsageComputeReachable(env);
    }
  }
  if (argc != util_optind) return UsageComputeReachable(env);

  if (Compile_check_if_model_was_built(env, errstream, true)) return 1;

  fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));

  if(!used_t) {
    t = -1; /* No limit */
  }

  if(!used_k) {
    k = -1; /* No limit */
  }

  completed = BddFsm_compute_reachable(fsm, k, t, &diameter);

  if (completed) {
    StreamMgr_print_error(streams,
            "The computation of reachable states has been completed.\n");
    StreamMgr_print_error(streams,
            "The diameter of the FSM is %d.\n",
            diameter);
  }
  else {
    StreamMgr_print_error(streams,
            "The computation of reachable states has not been completed yet.\n");
    StreamMgr_print_error(streams,
            "The number of performed steps is %d.\n",
            diameter);
  }
  return 0;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageComputeReachable(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: compute_reachable [-h] [-k number] [-t time ]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -k \t\tLimit the forward search to number steps forward starting from the last reached frontier.\n");
  StreamMgr_print_error(streams,  "   -t \t\tLimit the forward search to time seconds (The limit can be exceeded for the duration of the last cycle).\n");
  return 1;
}

/*!
  \command{print_reachable_states} Prints out information about reachable states

  \command_args{[-h] [-v] [-f] [-d] [-o filename] }

  Prints the number of reachable states of the
  given model. In verbose mode, prints also the list of all reachable
  states.  The reachable states are computed if needed.<p>

  Command Options:
  <dl>
  <dt> <tt>-v</tt>
  <dd> Verbosely prints the list of reachable states.
  <dt> <tt>-f</tt>
  <dd> Print the list of reachable states as a formula.
  <dt> <tt>-d</tt>
  <dd> Prints the list of reachable states with defines (Requires -v).
  <dt> <tt>-o filename </tt>
  <dd> Prints the result on the specified filename instead of on standard output
  </dl>

*/

int CommandPrintReachableStates(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  BddFsm_ptr bdd_fsm = NULL;

  int c;
  boolean verbose = false;
  boolean formula = false;
  boolean print_defines = false;
  char* filename;
  int retval = 0;

  filename = (char*) NULL;

  /* Parse the command line */
  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hfvdo:")) != EOF) {
    switch (c) {
    case 'h': return UsagePrintReachableStates(env);
    case 'f':
      formula = true;
      break;
    case 'v':
      verbose = true;
      break;
    case 'd':
      print_defines = true;
      break;
    case 'o':
      if ((char*)NULL != filename) {
        FREE(filename);
        return UsagePrintReachableStates(env);
      }

      filename = util_strsav(util_optarg);
      break;
    default:
      if ((char*)NULL != filename) FREE(filename);
      return UsagePrintReachableStates(env);
    }
  }

  if (verbose && formula) {
    if ((char*)NULL != filename) FREE(filename);
    return UsagePrintReachableStates(env);
  }

  if (print_defines && formula) {
    StreamMgr_print_error(streams,  "-f and -d are non combinable!\n");
    if ((char*)NULL != filename) FREE(filename);
    return UsagePrintReachableStates(env);
  }

  if(print_defines && !(verbose)) {
    StreamMgr_print_error(streams, "-d requires -v option!\n");
    if ((char*)NULL != filename) FREE(filename);
    return UsagePrintReachableStates(env);
  }

  if (Compile_check_if_model_was_built(env, errstream, true))
    return 1;

  {  /* Printing */
    OStream_ptr stream = StreamMgr_get_output_ostream(streams);

    CATCH(errmgr) {
      if ((char*)NULL != filename) {
        stream = OStream_create_file(filename, false);

        if (OSTREAM(NULL) == stream) {
          StreamMgr_print_error(streams,  "Unable to open specified file.\n");
          FREE(filename);
          return 1;
        }
      }

      bdd_fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));
      retval = BddFsm_print_reachable_states(bdd_fsm, env, stream, verbose,
                                             print_defines, formula);

      if ((char*)NULL != filename) {
        OStream_destroy(stream);
        FREE(filename);
      }
    }
    FAIL(errmgr) {
      if ((char*)NULL != filename) {
        OStream_destroy(stream);
        FREE(filename);
      }

      retval = 1;
    }
  }  /* printing */

  return retval;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsagePrintReachableStates(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,
          "usage: print_reachable_states [-h] [-v] [-d] [-f] [-o filename]\n");

  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -v \t\tPrints the list of reachable states.\n");
  StreamMgr_print_error(streams,  "   -d \t\tPrints the list of reachable states with ");
  StreamMgr_print_error(streams,  "defines (Requires -v).\n");
  StreamMgr_print_error(streams,  "   -f \t\tPrints the formula representing the ");
  StreamMgr_print_error(streams,  "reachable states.\n");
  StreamMgr_print_error(streams,  "   -o filename\tPrints the result on the specified ");
  StreamMgr_print_error(streams,  "filename instead of on standard output\n");

  return 1;
}


/*!
  \command{print_fair_states} Prints out information about fair states

  \command_args{[-h] [-v]}

  This command provides information about the fair
  states of the current model.number of fair states. In verbose mode,
  prints also the list of fair states.<p>

  Command Options:
  <dl>
    <dt> <tt>-v</tt>
    <dd> Verbosely prints the list of fair states.
  </dl>
*/

int CommandPrintFairStates(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OStream_ptr outstream = StreamMgr_get_output_ostream(streams);
  FILE* errstream = StreamMgr_get_error_stream(streams);

  BddFsm_ptr bdd_fsm = NULL;
  int c;
  boolean verbose = false;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hv")) != EOF) {
    switch (c) {
    case 'h': return UsagePrintFairStates(env);
    case 'v':
      verbose = true;
      break;
    default:
      return UsagePrintFairStates(env);
    }
  }

  if (Compile_check_if_model_was_built(env, errstream, true)) return 1;
  bdd_fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));

  return BddFsm_print_fair_states(bdd_fsm, env, outstream, verbose);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsagePrintFairStates(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: print_fair_states [-h] [-v]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -v \t\tPrints the list of fair states.\n");
  return 1;
}


/*!
  \command{print_fair_transitions} Prints the number of fair transitions,
  and list transitions in verbose mode.

  \command_args{[-h] [-v [-f format] [-o fname]]}

  Prints the number of fair transitions. In verbose mode, prints
  also the list of fair transitions.<p>

    Command Options:
    <dl>
    <dt> <tt>-v</tt>
    <dd> Verbosely prints the list of fair transitions.
    <dt> <tt>-f</tt>
    <dd> Use given format when printing the list of fair transitions.
    <dt> <tt>-o fname</tt>
    <dd> Writes to given filename (default: stderr).
  </dl>
*/

int CommandPrintFairTransitions(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  OStream_ptr outstream = NULL;
  FILE* errstream = StreamMgr_get_error_stream(streams);

  BddFsm_ptr bdd_fsm = NULL;
  int c;
  boolean verbose = false;
  enum BddFsmTransPrinterFormat format = BDD_FSM_TRANS_PRINTER_SMV;
  int res;

  /* Parse the command line */
  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "ho:vf:")) != EOF) {
    switch (c) {
      case 'h':
        goto cmd_help;

      case 'o':
        outstream = OStream_create_file(util_optarg, false);
        break;

      case 'v':
        verbose = true;
        break;

      case 'f':
        verbose = true;
        format = BddFsm_trans_printer_format_from_string(util_optarg);
        if (BDD_FSM_TRANS_PRINTER_INVALID == format) {
          size_t num;
          int i;
          enum BddFsmTransPrinterFormat* formats;

          StreamMgr_print_error(streams,  "Invalid format. Valid formats are: ");
          formats = BddFsm_trans_printer_get_avail_formats(&num);
          for (i=0; i<num; ++i) {
            StreamMgr_print_error(streams, "%s%s",
                                  BddFsm_trans_printer_format_to_string(formats[i]),
                                  i + 1 < num ? ", " : "");
          }

          StreamMgr_print_error(streams, "\n");
          FREE(formats);
          goto cmd_fail;
        }
        break;

      default:
        return UsagePrintFairTransitions(env);
    }
  }

  if (Compile_check_if_model_was_built(env, errstream, true))
    return 1;

  bdd_fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));
  res = BddFsm_print_fair_transitions(
      bdd_fsm, env,
      verbose? format : BDD_FSM_TRANS_PRINTER_SILENT,
      outstream ? outstream : StreamMgr_get_output_ostream(streams));

cmd_exit:
  if (NULL != outstream) {
    OStream_destroy(outstream);
  }
  return res;

cmd_help:
  res = UsagePrintFairTransitions(env);
  goto cmd_exit;

cmd_fail:
  res = 1;
  goto cmd_exit;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsagePrintFairTransitions(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(
      streams,  "usage: print_fair_transitions [-h] [-v [-f format] [-o fname]]\n");
  StreamMgr_print_error(
      streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(
      streams,  "   -v \t\tPrints the list of fair transitions.\n");
  StreamMgr_print_error(
      streams,  "   -f format\tUse given format when printing the list "
      "of fair transitions.\n");
  StreamMgr_print_error(
      streams,  "      \t\tValid formats are: ");
  {
    size_t num;
    int i;
    enum BddFsmTransPrinterFormat* formats;

    formats = BddFsm_trans_printer_get_avail_formats(&num);
    for (i=0; i<num; ++i) {
      StreamMgr_print_error(streams, "%s%s",
                            BddFsm_trans_printer_format_to_string(formats[i]),
                            i + 1 < num ? ", " : "");
    }

    StreamMgr_print_error(streams, "\n");
    FREE(formats);
  }
  StreamMgr_print_error(
      streams,  "   -o fname\tWrites to given filename (default: stderr).\n");

  return 1;
}


/*!
  \command{_print_fair_state_input_pairs} Prints the number of fair
  state/input pairs

  \command_args{[-h] [-v]}

  Prints the number of fair state/input pairs. In
  verbose mode, prints also the list of fair pairs.<p>

    Command Options:
    <dl>
    <dt> <tt>-v</tt>
    <dd> Verbosely prints the list of fair state/input pairs.
  </dl>
*/

int CommandPrintFairStateInputPairs(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OStream_ptr outstream = StreamMgr_get_output_ostream(streams);
  FILE* errstream = StreamMgr_get_error_stream(streams);

  BddFsm_ptr bdd_fsm = NULL;
  int c;
  boolean verbose = false;

  /* Parse the command line */
  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hv")) != EOF) {
    switch (c) {
    case 'h': return UsagePrintFairStateInputPairs(env);
    case 'v':
      verbose = true;
      break;
    default:
      return UsagePrintFairStateInputPairs(env);
    }
  }

  if (Compile_check_if_model_was_built(env, errstream, true)) return 1;

  bdd_fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));
  return BddFsm_print_fair_state_input_pairs(bdd_fsm, env,
                                             outstream, verbose);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsagePrintFairStateInputPairs(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: _print_fair_state_input_pairs [-h] [-v]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -v \t\tPrints the list of fair state/input pairs.\n");
  return 1;
}


/*!
  \command{dump_fsm} Dumps (in DOT format) selected parts of the bdd
  fsm, with optional expression

  \command_args{[-h] -o filename [-i] [-I] [-t] [-f] [-r] [-e expression]}

  Dumps selected parts of the bdd fsm, with
  optional expression, in DOT format. At least one among options
  [iIte] must be specified.

    Command Options:

    <dt> <tt>-o filename</tt>
    <dd> Dumps to the specified file name.

    <dt> <tt>-i</tt>
    <dd> Dumps the initial states of the FSM, among with other
    selected outputs.

    <dt> <tt>-I</tt>
    <dd> Dumps the invariant states of the FSM, among with other
    selected outputs.

    <dt> <tt>-t</tt>
    <dd> Dumps the (monolithic) transition relation of the FSM, among with other
    selected outputs.

    <dt> <tt>-F</tt>
    <dd> Dumps the (monolithic) fair states of the FSM, among with other
    selected outputs.

    <dt> <tt>-r</tt>
    <dd> Dumps the (monolithic) reachable states of the FSM, among with other
    selected outputs.

    <dt> <tt>-e</tt>
    <dd> Dumps the specified expression, among with other
    selected outputs (see also command dump_expr).
  </dl>
*/

int CommandDumpFsm(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  BddFsm_ptr bdd_fsm = NULL;

  FILE* errstream = StreamMgr_get_error_stream(streams);
  int c;
  int res = 0;
  boolean init = false;
  boolean invar = false;
  boolean trans = false;
  boolean fair = false;
  boolean reachable = false;
  char* str_constr = (char*) NULL;
  char* fname = (char*) NULL;
  FILE* outfile = (FILE*) NULL;
  node_ptr parsed_expr = NULL;
  node_ptr node_expr = NULL;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "ho:e:iItFr")) != EOF) {
    switch (c) {
    case 'h':
      res = UsageDumpFsm(env);
      goto dump_fsm_quit;

    case 'i': init = true; break;
    case 'I': invar = true; break;
    case 't': trans = true; break;
    case 'F': fair = true; break;
    case 'r': reachable = true; break;

    case 'e':
      if ((char*) NULL != str_constr) FREE(str_constr);
      str_constr = util_strsav(util_optarg);
      break;

    case 'o':
      if ((char*) NULL != fname) FREE(fname);
      fname = util_strsav(util_optarg);
      break;

    default:
      res = 1;
      goto dump_fsm_quit;
    }
  }

  /* preconditions */
  if (Compile_check_if_model_was_built(env, errstream, true)) {
    res = 1;
    goto dump_fsm_quit;
  }

  /* checks and processes arguments */
  if ((char*) NULL == fname) {
    StreamMgr_print_error(streams,  "Output file must be specified\n");
    res = 1;
    goto dump_fsm_quit;
  }

  if ((((char*) NULL != str_constr) + init + trans + invar +
       fair + reachable) == 0) {
    StreamMgr_print_error(streams,  "At least one option in 'eiItFr' must be specified.\n");
    res = 1;
    goto dump_fsm_quit;
  }

  outfile = fopen(fname, "w");
  if ((FILE*) NULL == outfile) {
    StreamMgr_print_error(streams,  "Problems opening output file '%s'.\n", fname);
    res = 1;
    goto dump_fsm_quit;
  }

  if ((char*) NULL != str_constr) {
    if (Parser_ReadNextExprFromString(env, str_constr, &parsed_expr)) {
      res = 1;
      goto dump_fsm_quit;
    }

    /* [MD] This should be a function */
    {
      BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
      SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(bdd_enc));
      TypeChecker_ptr tc = SymbTable_get_type_checker(st);
      SymbType_ptr tp = NULL;

      node_expr = Compile_FlattenSexp(st, car(parsed_expr), Nil);

      tp = TypeChecker_get_expression_type(tc, node_expr, Nil);
      if (SymbType_is_error(tp)) {
        StreamMgr_print_error(streams,  "Type of expression is not correct.\n");

        res = 1;
        goto dump_fsm_quit;
      }
      if (SymbType_is_real(tp) ||
          SymbType_is_continuous(tp) ||
          SymbType_is_statement(tp)) {
        StreamMgr_print_error(streams,  "Type of expression is not supported.\n");

        res = 1;
        goto dump_fsm_quit;
      }
    }
  }

  bdd_fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));
  res = BddFsm_dump_fsm(bdd_fsm, env, node_expr, str_constr, init, invar, trans, fair, reachable,
                        outfile);
 dump_fsm_quit:
  if ((char*) NULL != str_constr) { FREE(str_constr); }
  if ((char*) NULL != fname) { FREE(fname); }
  if ((FILE*) NULL != outfile) {
    fclose(outfile);
  }
  return res;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageDumpFsm(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: dump_fsm [-h] -o <fname> [-i][-I][-t][-F][-r][-e <expr>]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -o fname \tDumps to the specified file.\n");
  StreamMgr_print_error(streams,  "   -e expr \tDumps the specified expression.\n");
  StreamMgr_print_error(streams,  "   -i \t\tDumps the initial states.\n");
  StreamMgr_print_error(streams,  "   -I \t\tDumps the invariant states.\n");
  StreamMgr_print_error(streams,  "   -t \t\tDumps the transition relation.\n");
  StreamMgr_print_error(streams,  "   -F \t\tDumps the fair states.\n");
  StreamMgr_print_error(streams,  "   -r \t\tDumps the reachable states.\n");
  return 1;
}
