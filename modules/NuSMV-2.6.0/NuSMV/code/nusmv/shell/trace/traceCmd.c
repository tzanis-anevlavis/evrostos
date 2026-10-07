/* ---------------------------------------------------------------------------


  This file is part of the ``trace'' package of NuSMV version 2.
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
  \author Ashutosh Trivedi, Marco Pensallorto
  \brief Trace Commands

  This file contains commands related to traces.

*/

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/shell/cmd/cmd.h"
#include "nusmv/shell/trace/traceCmd.h"

#include "nusmv/core/compile/compile.h"

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/trace/pkg_trace.h"

#include "nusmv/core/prop/propPkg.h"
#include "nusmv/core/trace/TraceMgr.h"
#include "nusmv/core/trace/TraceOpt.h"
#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/exec/traceExec.h"
#include "nusmv/core/enc/enc.h"

#include "nusmv/core/utils/ucmd.h"
#include "nusmv/core/utils/ustring.h"

#include "nusmv/shell/bmc/bmcCmd.h"

#include "nusmv/core/trace/loaders/TraceLoader.h"
#include "nusmv/core/trace/loaders/TraceXmlLoader.h"

#include "nusmv/core/trace/exec/BaseTraceExecutor.h"
#include "nusmv/core/trace/exec/CompleteTraceExecutor.h"
#include "nusmv/core/trace/exec/PartialTraceExecutor.h"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define IS_PARTIAL_EXECUTOR true

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static int UsageShowTraces(const NuSMVEnv_ptr env);
static int UsageShowPlugins(const NuSMVEnv_ptr env);
static int UsageReadTrace(const NuSMVEnv_ptr env);
static int UsageExecuteTraces(NuSMVEnv_ptr env);
static int UsageExecutePartialTraces(NuSMVEnv_ptr env);

static inline
void trace_cmd_print_registered_executors(TraceMgr_ptr const trace_mgr,
                                          const boolean is_partial_executor);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/
void traceCmd_init(NuSMVEnv_ptr env)
{
  Cmd_CommandAdd(env, "show_traces", CommandShowTraces, 0, true);
  Cmd_CommandAdd(env, "show_plugins", CommandShowPlugins, 0, true);

  Cmd_CommandAdd(env, "read_trace", CommandReadTrace, 0, true);
  Cmd_CommandAdd(env, "execute_traces", CommandExecuteTraces, 0, true);
  Cmd_CommandAdd(env, "execute_partial_traces", CommandExecutePartialTraces, 0, true);
}

int CommandShowTraces(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  TraceMgr_ptr const tm =
    TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
  int c, res = 0; /* success */

  boolean all = false;
  boolean number = false;

  boolean use_more = false;
  char* dbgFileName = NIL(char);
  FILE* output_stream = NIL(FILE);

  int traceno = TraceMgr_get_size(tm);
  int trace = traceno;
  int from_state = 0;
  int to_state = 0;

  int plugin_index = TraceMgr_get_default_plugin(tm);

  /* Create a new instance of options, starting from the environment
     settings */
  TraceOpt_ptr trace_opt = TraceOpt_create_from_env(env);

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hvAatmp:o:d")) != EOF) {
    switch (c) {
    case 'h':
      res = UsageShowTraces(env);
      goto leave;

    case 'v':

      /* verbose makes sense for explainers only */
      if (0 == plugin_index) {
        plugin_index += 1;
      }
      else {
        StreamMgr_print_error(streams,  "Warning: -v option ignored.\n");
      }
      break;

    case 'a':
      all = true;
      break;

    case 't':
      number = true;
      break;

    case 'd':
      TraceOpt_set_show_defines(trace_opt, false);
      break;

    case 'A':
      TraceOpt_set_obfuscate(trace_opt, true);
      break;

    case 'p':
      {
        char* err_occ[1];
        plugin_index = strtol(util_optarg, err_occ, 10);
        if ((strncmp(err_occ[0], "", 1) != 0) ||
            (plugin_index < 0) || \
            (TraceMgr_get_plugin_size(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)))<=plugin_index)) {

          StreamMgr_print_error(streams,
                  "Error: \"%s\" is not a valid trace plugin value " \
                  "for \"show_traces -p\" command line option.\n",
                  util_optarg);

          res = UsageShowTraces(env); goto leave;
        }
      }
      break;

    case 'o':
      if (use_more == 1) { res = UsageShowTraces(env); goto leave; }
      dbgFileName = util_strsav(util_optarg);
      break;

    case 'm':
      if (dbgFileName != NIL(char)) { res = UsageShowTraces(env); goto leave; }
      use_more = 1;
      break;

    default:
      res = UsageShowTraces(env);
      goto leave;
    } /* Switch */
  } /* While */

  if (traceno == 0) {
    StreamMgr_print_error(streams,  "There are no traces currently available.\n");
    return 0;
  }

  if ((util_optind == 0) && (argc > 2)) {
    res = UsageShowTraces(env);
    goto leave;
  }

  /* Parsing of the trace number and state number/slices to be printed */
  if (all == false) {
    if (argc != util_optind) {
      res = traceCmd_parse_slice(env, argv[util_optind], &trace,
                                  &from_state, &to_state);
      if (trace < 1 || traceno < trace) {
        StreamMgr_print_error(streams,
                "Invalid trace number"
                " (valid values are 1-%d).\n", traceno);
        res = 1;
      }
    }
    if (0 != res) goto leave;
  }
  else if (argc != util_optind) { res = UsageShowTraces(env); goto leave; }

  if (use_more) {
    output_stream = CmdOpenPipe(env, use_more);
    TraceOpt_set_output_stream(trace_opt, output_stream);
    if (NIL(FILE) == output_stream) {
      res = 1; goto leave;
    }
  }

  if (NIL(char) != dbgFileName) {
    output_stream = CmdOpenFile(env, dbgFileName);
    TraceOpt_set_output_stream(trace_opt, output_stream);
    if (NIL(FILE) == output_stream) {
      res = 1; goto leave;
    }
  }

  if (number == true) {
    StreamMgr_print_error(streams,  (traceno == 1) ?
            "There is %d trace currently available.\n" :
            "There are %d traces currently available.\n", traceno);
  }
  else {
    res = TraceMgr_show_traces(tm, plugin_index, all, trace, trace_opt,
                               traceno, from_state, to_state);
  }

 /* command cleanup */
  leave:
  StreamMgr_reset_indent_size(streams);
  TraceOpt_destroy(trace_opt);

  /* handle file errors gracefully */
  if (use_more && (NIL(FILE) != output_stream)) {
    CmdClosePipe(output_stream);
  }
  if (NIL(char) != dbgFileName) {
    if (NIL(FILE) != output_stream) {
      CmdCloseFile(output_stream);
    }
    FREE(dbgFileName); dbgFileName = (char*)NULL;
  }

  return res;
}

/*!
  \brief UsageShowTraces


*/
static int UsageShowTraces(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,
          "usage: show_traces [-h] [-v] [-t] [-A] [-m | -o output-file] " \
          "[-p plugin-no] [-a | trace_number[.from_state[:[to_state]]] \n");

  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,
          "  -v \t\tVerbosely prints traces content (unchanged vars also).\n");

  StreamMgr_print_error(streams,  "  -a \t\tPrints all the currently stored traces.\n");
  StreamMgr_print_error(streams,
          "  -t \t\tPrints only the total number of currently stored traces.\n");

  StreamMgr_print_error(streams,
          "  -A \t\tPrints the trace in anonimized form.\n");

  StreamMgr_print_error(streams,
          "  -d \t\tDisables the printing of DEFINEs .\n");

  StreamMgr_print_error(streams,
       "  -m \t\tPipes output through the program specified by the \"PAGER\"\n");

  StreamMgr_print_error(streams,
          "     \t\tenvironment variable if defined, else through " \
          "the UNIX command \"more\".\n");

  StreamMgr_print_error(streams,
         "  -p plugin-no\tUses the specified trace plugin to explain the trace.\n");

  StreamMgr_print_error(streams,
          "  -o output-file\tWrites the generated output to \"file\".\n");

  StreamMgr_print_error(streams,
          "  trace_number\tThe number of the trace to be printed.\n");

  StreamMgr_print_error(streams,
          "  from_state\tLeft end of the trace slice to be printed.\n");

  StreamMgr_print_error(streams,
          "  to_state\tRight end of the trace slice to printed.\n"
          "  \t\t(if omitted, last step is assumed by default.)\n");
  StreamMgr_print_error(streams,
          "You can control the behavior of this command even through the variables:\n"
          "traces_regex, traces_show_defines, traces_show_defines_with_next, traces_hiding_prefix\n");

  return 1;
}

int CommandShowPlugins(NuSMVEnv_ptr env, int argc, char** argv)
{
  TraceMgr_ptr const trace_mgr =
    TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  int c;
  boolean showAll = false;
  int dp = -1;
  int res = 1;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hn:a")) != EOF) {
    switch (c) {
    case 'h': return UsageShowPlugins(env);
      break;

    case 'n':
      {
        char *err_occ[1];

        if (showAll) return UsageShowPlugins(env);
        dp = strtol(util_optarg, err_occ, 10);
        if (strncmp(err_occ[0], "", 1) != 0) {
          StreamMgr_print_error(streams,
                  "Error: \"%s\" is not a valid value for" \
                  "\"-show_plugins\" command line option.\n",
                  err_occ[0]);

          return UsageShowPlugins(env);
        }
      }
      break;

    case 'a':
      if (dp >= 0) return UsageShowPlugins(env);
      showAll = true;
      break;

    default: return UsageShowPlugins(env);
    }
  }

  res = TraceMgr_show_plugins(trace_mgr, showAll, dp);

  return res;
}

/*!
  \brief UsageShowPlugins


*/
static int UsageShowPlugins(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: show_plugins [-h]  [-n  plugin_index | -a]\n");
  StreamMgr_print_error(streams,  "  -h                Prints the command usage.\n");
  StreamMgr_print_error(streams,  "  -a                Shows all registered plugins.\n");
  StreamMgr_print_error(streams,
          "  -n plugin_index   Shows only the description of the specified " \
          "plugin_index.\n");

  return 1;
}

int CommandReadTrace(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  int res = 0;
  int c;
  /* Does filename need to be freed? */
  char* filename = NIL(char);
  boolean halt_if_undef = true;
  boolean halt_if_wrong_section = true;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "husi:")) != EOF) {
    switch (c) {
    case 'h':
      res = UsageReadTrace(env);
      goto leave;

    case 'i':
      filename = util_optarg;
      StreamMgr_print_error(streams,  "Warning: option -i is deprecated feature.\n");
      break;

    case 'u':
      halt_if_undef = false;
      break;

    case 's':
      halt_if_wrong_section = false;
      break;

    default:
      UsageReadTrace(env);
      res = 1;
      goto leave;
    }
  }

  /* -i did not provide a file name. the last argument must be a file name */
  if (NIL(char) == filename) {
    if (argc == util_optind) {
      StreamMgr_print_error(streams,  "Error: Input XML file has to be provided.\n");
      res = 1; goto leave;
    }
    if (argc != util_optind + 1) {
      StreamMgr_print_error(streams,
              "Error: Only one input XML file can be specified.\n");
      res = 1; goto leave;
    }
    filename = argv[util_optind];

    if(strcmp(filename, "") == 0) {
      StreamMgr_print_error(streams,  "Error: Input XML file name is empty.\n");
      res = 1; goto leave;
    }
  }
  else {
    /* check that there is no unprocessed options */
    if (argc > util_optind) {
      StreamMgr_print_error(streams,
              "Error: unknown option is provided to command : %s\n",
              argv[util_optind]);
      res = 1; goto leave;
    }
  }

  /* pre-conditions */
  if (Compile_check_if_flat_model_was_built(env, errstream, false)) {
    res = 1; goto leave;
  }

#if NUSMV_HAVE_LIBXML2
  {
    Trace_ptr trace = TRACE(NULL);
    SexpFsm_ptr sexp_fsm =
      SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));

    SEXP_FSM_CHECK_INSTANCE(sexp_fsm);

    trace = TracePkg_read_trace(env, sexp_fsm, filename, halt_if_undef,
                                halt_if_wrong_section);

    if (TRACE(NULL) == trace) {
      StreamMgr_print_error(streams,
              "Unable to load trace from XML File \"%s\".\n", filename);
      res = 1;
    }
    else {
      StreamMgr_print_error(streams,  "Trace is stored at %d index \n",
              TraceMgr_register_trace(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), trace) + 1);
      res = 0;
    }
  } /* load trace */

#else
  {
    StreamMgr_print_error(streams,  "LIBXML2 library is not available on this system.\n");
    StreamMgr_print_error(streams,  "Try to recompile %s with the LIBXML2 library.\n",
                          NuSMVCore_get_tool_name());
  }
#endif

 /* command cleanup */
 leave:
  return res;
}

/*!
  \brief UsageReadTrace


*/
static int UsageReadTrace(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: read_trace [-u] [-s] (-h | file_name | -i file_name)\n"
          "  -h \t Prints the command usage.\n"
          "  -u \t Output a warning instead of an error for every "
          "undefined symbol.\n"
          "  -s \t Output a warning instead of an error for every "
          "symbol placed in \n \t inappropriate trace section.\n"
          "  -i file_name  Option -i is deprecated.\n"
          "  file-name     Trace is read from a given XML file.\n"
          );

  return 1;
}

int CommandExecuteTraces(NuSMVEnv_ptr env, int argc, char** argv)
{
  TraceMgr_ptr const trace_mgr =
    TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  int c, res = 0;

  boolean use_more = false;
  boolean all = false;

  int traceno = TraceMgr_get_size(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)));
  int trace = traceno;

  char* dbgFileName = NIL(char);
  char* engineName = NIL(char);
  char* err_occ[2];

  /* executor params */
  int verbosity = 0;
  FILE* output_stream = NIL(FILE);

  BaseTraceExecutor_ptr executor = NULL;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hvamo:e:")) != EOF) {
    switch (c) {
    case 'h':
      res = UsageExecuteTraces(env);
      goto leave;

    case 'v':
      verbosity ++ ;
      break;

    case 'a':
      all = true;
      break;

    case 'o':
      if (0 != use_more) {
        UsageExecuteTraces(env);
        res = 1; goto leave;
      }
      dbgFileName = util_strsav(util_optarg);
      break;

    case 'm':
      if (NIL(char) != dbgFileName) {
        UsageShowTraces(env);
        res = 1; goto leave;
      }
      use_more = 1;
      break;

    case 'e':
      engineName = util_strsav(util_optarg);
      executor =
        BASE_TRACE_EXECUTOR(TraceMgr_get_complete_trace_executor(trace_mgr,
                                                                 engineName));
      if (NULL == executor) {
        StreamMgr_print_error(streams,  "Error: \"%s\" is not a valid executor\n",
                engineName);
        res = UsageExecuteTraces(env); goto leave;
      }
      break;

    default:
        res = UsageExecuteTraces(env);
        goto leave;
    } /* switch */
  } /* loop */

  /* [MD] Here instead of this, a proper check about the state of the system
     is needed */
  if (NULL != executor ||
      NULL != TraceMgr_get_default_partial_trace_executor(trace_mgr)) {
    if (use_more) {
      output_stream = CmdOpenPipe(env, use_more);

      if (NIL(FILE) == output_stream) {
        res = 1; goto leave;
      }
    }

    if (dbgFileName != NIL(char)) {
      output_stream = CmdOpenFile(env, dbgFileName);

      if (NIL(FILE) == output_stream) {
        res = 1; goto leave;
      }
    }

    if (traceno == 0) {
      StreamMgr_print_error(streams,  "There are no traces currently available.\n");
      goto leave;
    }

    if ((util_optind == 0) && (argc > 2)) {
      UsageExecuteTraces(env);
      res = 1; goto leave;
    }

    /* Parsing of the trace number to be executed */
    if (all == false) {
      if (argc != util_optind) {
        err_occ[0] = "";
        trace = strtol(argv[util_optind], err_occ, 10);

        if  ((strncmp(err_occ[0], "", 1) != 0)) {
          StreamMgr_print_error(streams,
                  "Error: \"%s\" is not a valid value (must be a positive " \
                  "integer).\n",
                  err_occ[0]);
          res = 1; goto leave;
        }
        if ( (trace > traceno) || (trace == 0) ) {
          StreamMgr_print_error(streams,
                  "Error: \"%d\" is not a valid trace number. Acceptable range is"
                  " 1..%d.\n", trace, traceno);
          res = 1; goto leave;
        }
      }
    }
    else if (argc != util_optind) {
      UsageExecuteTraces(env);
      res = 1; goto leave;
    }
  } /* if (COMPLETE_TRACE_EXECUTOR(NULL) != executor) */
  else {
    StreamMgr_print_error(streams,  "No valid executor found. "
            "Model must be built in order to perform trace re-execution.\n");
    res = 1; goto leave;
  }

  res = TracePkg_execute_traces(env, trace_mgr, output_stream, engineName,
                                verbosity, trace);


 /* command cleanup */
 leave:
  /* handle file errors gracefully */
  if (use_more && (NIL(FILE) != output_stream)) {
    CmdClosePipe(output_stream);
  }
  if (NIL(char) != dbgFileName) {
    if (NIL(FILE) != output_stream) {
      CmdCloseFile(output_stream);
    }
    FREE(dbgFileName);
  }

  if (NIL(char) != engineName) {
    FREE(engineName);
  }

  return res;
}

/*!
  \brief UsageExecuteTraces


*/
static int UsageExecuteTraces(NuSMVEnv_ptr env)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  TraceMgr_ptr tm = TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));

  StreamMgr_print_error(streams,
          "usage: execute_traces [-h] [-v] [-m | -o file] " \
          "[-e engine] [-a | trace_number]\n");

  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,
          "  -v \t\tVerbosely prints execution steps.\n");

  StreamMgr_print_error(streams,  "  -a \t\tExecutes all the currently stored traces.\n");
  StreamMgr_print_error(streams,
          "  -m \t\tPipes output through the program specified by the \"PAGER\"\n"
          "     \t\tenvironment variable if defined, else through the UNIX\n"
          "     \t\tcommand \"more\".\n");
  StreamMgr_print_error(streams,
          "  -o file\tWrites the generated output to \"file\". This option \n"
          "     \t\tis incompatible with -m.\n");

  StreamMgr_print_error(streams,
          "  -e executor\tSelects an executor to perform trace re-execution.\n");

  trace_cmd_print_registered_executors(tm, ! IS_PARTIAL_EXECUTOR);

  StreamMgr_print_error(streams,
          "  trace_number\tThe number of the trace to be executed.\n");

  return 1;
}

int CommandExecutePartialTraces(NuSMVEnv_ptr env, int argc, char** argv)
{
  TraceMgr_ptr const trace_mgr =
    TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  int c, res = 0;

  int traceno = TraceMgr_get_size(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)));
  int trace = traceno;
  int first_trace;
  int last_trace;

  boolean all = false;

  boolean use_more = false;
  char* dbgFileName = NIL(char);
  char* engineName = NIL(char);

  /* executor params */
  boolean restart = false;
  int verbosity = 0;
  FILE* output_stream = NIL(FILE);

  PartialTraceExecutor_ptr executor = NULL;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hvarmo:e:")) != EOF) {
    switch (c) {
    case 'h':
      res = UsageExecutePartialTraces(env);
      goto leave;

    case 'v':
      verbosity ++ ;
      break;

    case 'a':
      all = true;
      break;

    case 'r':
      restart = true;
      break;

    case 'o':
      if (use_more == 1) {
        UsageExecutePartialTraces(env);
        res = 1; goto leave;
      }
      dbgFileName = util_strsav(util_optarg);
      break;

    case 'm':
      if (dbgFileName != NIL(char)) {
        UsageExecutePartialTraces(env);
        res = 1;
        goto leave;
      }
      use_more = 1;
      break;

    case 'e':
      engineName = util_strsav(util_optarg);
      if (restart) {
        char* tmp = ALLOC(char, strlen(engineName) + 2);
        strcpy(tmp, engineName); strcat(tmp, "_r");
        FREE(engineName); engineName = tmp;
        StreamMgr_print_error(streams,  "Warning: Option -r is deprecated\n");
      }

      executor = TraceMgr_get_partial_trace_executor(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                                                         engineName);
      if (PARTIAL_TRACE_EXECUTOR(NULL) == executor) {
        StreamMgr_print_error(streams,  "Error: \"%s\" is not a valid executor\n",
                engineName);

        res = UsageExecutePartialTraces(env); goto leave;
      }
      break;

    default:
        res = UsageExecutePartialTraces(env);
        goto leave;
    } /* switch */
  } /* loop */


  if (PARTIAL_TRACE_EXECUTOR(NULL) !=  executor ||
      NULL != TraceMgr_get_default_complete_trace_executor(trace_mgr)) {
    if (use_more) {
      output_stream = CmdOpenPipe(env, use_more);

      if (NIL(FILE) == output_stream) {
        res = 1; goto leave;
      }
    }

    if (dbgFileName != NIL(char)) {
      output_stream = CmdOpenFile(env, dbgFileName);

      if (NIL(FILE) == output_stream) {
        res = 1; goto leave;
      }
    }

    if (traceno == 0) {
      StreamMgr_print_error(streams,  "There are no traces currently available.\n");
      goto leave;
    }

    if ((util_optind == 0) && (argc > 2)) {
      UsageExecutePartialTraces(env);
      res = 1;
      goto leave;
    }

    /* Parsing of the trace number to be executed */
    if (all == false) {
      if (argc != util_optind) {
        char* err_occ[2];

        err_occ[0] = "";
        trace = strtol(argv[util_optind], err_occ, 10);

        if  ((strncmp(err_occ[0], "", 1) != 0)) {
          StreamMgr_print_error(streams,
                  "Error: \"%s\" is not a valid value "
                  "(must be a positive integer).\n", err_occ[0]);
          res = 1; goto leave;
        }
        if ( (trace > traceno) || (trace == 0) ) {
          StreamMgr_print_error(streams,
                  "Error: \"%d\" is not a valid trace number. "
                  "Acceptable range is 1..%d.\n", trace, traceno);
          res = 1; goto leave;
        }
      }
    }
    else if (argc != util_optind) {
      UsageExecutePartialTraces(env);
      res = 1; goto leave;
    }

    res = TracePkg_execute_partial_traces(env, trace_mgr, output_stream,
                                          engineName, verbosity, trace);
  }
  else { /*  no valid executor found */
    StreamMgr_print_error(streams,  "No valid executor found. "
            "Model must be built in order to perform trace re-execution.\n");
    res = 1; goto leave;
  }

 leave:
  /* handle file errors gracefully */
  if (use_more && (NIL(FILE) != output_stream)) {
    CmdClosePipe(output_stream);
  }
  if (NIL(char) != dbgFileName) {
    if (NIL(FILE) != output_stream) {
      CmdCloseFile(output_stream);
    }
    FREE(dbgFileName);
  }

  if (NIL(char) != engineName) {
    FREE(engineName);
  }

  return res;
}

/*!
  \brief UsageExecutePartialTrace


*/
static int UsageExecutePartialTraces(NuSMVEnv_ptr env)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  TraceMgr_ptr tm = TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));

  StreamMgr_print_error(streams,
          "usage: execute_partial_traces [-h] [-v] [-r] [-m | -o file] " \
          "[-e engine] [-a | trace_number]\n");

  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,
          "  -v \t\tVerbosely prints execution steps.\n");

  StreamMgr_print_error(streams,
          "  -a \t\tExecutes all the currently stored traces.\n");

  StreamMgr_print_error(streams,
          "  -r \t\tPerforms restart on complete states (deprecated).\n");

  StreamMgr_print_error(streams,
          "  -m \t\tPipes output through the program specified by the \"PAGER\"\n"
          "     \t\tenvironment variable if defined, else through the UNIX\n"
          "     \t\tcommand \"more\".\n");

   StreamMgr_print_error(streams,
          "  -o file\tWrites the generated output to \"file\". This option \n"
          "     \t\tis incompatible with -m.\n");

  StreamMgr_print_error(streams,
          "  -e executor\tSelects an executor to perform trace re-execution.\n");

  trace_cmd_print_registered_executors(tm, IS_PARTIAL_EXECUTOR);


  StreamMgr_print_error(streams,
          "  trace_number\tThe number of the trace to be executed.\n");

  return 1;
}

int traceCmd_parse_slice(const NuSMVEnv_ptr env, const char* s,
                         int* trace, int* from, int* to)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  char* endptr;
  int parse_res;

  const char TRACE_SEP = '.';
  const char STATE_SEP= ':';

  const char* trace_no_err_msg = \
    "Error: \"%s\" is not a valid trace number (must be a positive "    \
    " integer).\n";

  const char* state_no_err_msg = \
    "Error: \"%s\" is not a valid state number (must be an integer).\n" ;

  (*from) = 0; (*to) = 0;

  /* TODO[AMi] This code can be factorized to similar code in TraceLabel.c */
  parse_res = util_str2int_incr(s, &endptr, trace);
  if (0 == parse_res) {
    if (TRACE_SEP == *endptr) {
      s = ++ endptr;
      parse_res = util_str2int_incr(s, &endptr, from);
      if (0 == parse_res) {
        if (STATE_SEP == *endptr) {
          s = ++ endptr;
          parse_res = util_str2int_incr(s, &endptr, to);

          if (0 == parse_res)  {
            /* as user-friendly feature if no rhs end is given, pick
               last state. This is not allowed if junk is found */
            if ((0 == *to) && (0 == *endptr)) { (*to) = -1; /* last */ }

          } else { StreamMgr_print_error(streams,  state_no_err_msg, s); }
        }
      } else { StreamMgr_print_error(streams,  state_no_err_msg, s); }
    }
  } else { StreamMgr_print_error(streams,  trace_no_err_msg, s); }

  return parse_res;
} /* trace_cmd_parse_slice */

/*!
  \brief Print the registered executors

  Print the registered executors
*/
static inline void
trace_cmd_print_registered_executors(TraceMgr_ptr const trace_mgr,
                                     const boolean is_partial_executor)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(trace_mgr));
  StreamMgr_ptr const streams =
   STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  array_t* registered_executors;
  string_ptr tmp;
  int i;

  if (is_partial_executor) {
    registered_executors = TraceMgr_get_partial_trace_executor_ids(trace_mgr);
  }
  else {
    registered_executors = TraceMgr_get_complete_trace_executor_ids(trace_mgr);
  }

  if (0 < array_n(registered_executors)) {
    StreamMgr_print_error(streams,
                          "\t\t(must be one of the following registered executors)\n");

    arrayForEachItem(string_ptr, registered_executors, i, tmp) {
      const char* id = UStringMgr_get_string_text(tmp);
      const char* exec_desc;

      if (is_partial_executor) {
          exec_desc = TraceMgr_get_partial_trace_executor_desc(trace_mgr, id);
      }
      else {
        exec_desc = TraceMgr_get_complete_trace_executor_desc(trace_mgr, id);
      }

      StreamMgr_print_error(streams,  "\t\t%s - %s\n", id, exec_desc);
    }

    StreamMgr_print_error(streams,  "\n");
  }
  else {
    const char* exec_type;

    exec_type = (is_partial_executor) ? "partial" : "complete";
    StreamMgr_print_error(streams,
                          "\t\t(no %s trace executors registered yet.)\n",
                          exec_type);
  }
  array_free(registered_executors);
}
