/* ---------------------------------------------------------------------------


  This file is part of the ``mc'' package of NuSMV version 2.
  Copyright (C) 1998-2001 by CMU and FBK-irst.

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
  \brief Model checking commands.

  This file contains all the shell command to deal with
  model checking and for counterexample navigation.

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/shell/cmd/cmd.h"
#include "nusmv/shell/bmc/bmcCmd.h"
#include "nusmv/shell/bmc/sbmc/sbmcCmd.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/mc/mc.h"
#include "nusmv/shell/mc/mcCmd.h"
#include "nusmv/core/bmc/bmcPkg.c"

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h" /* for CATCH(errmgr) */
#include "nusmv/core/prop/Prop.h"
#include "nusmv/core/prop/PropDb.h"
#include "nusmv/core/prop/propPkg.h"
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/TraceMgr.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/utils/ucmd.h" /* for util_str2int */
#include "nusmv/core/bmc/bmcUtils.h" /* for Bmc_Utils_ConvertLoopFromString */
#include "nusmv/core/bmc/bmcBmc.h"
#include "nusmv/core/fsm/bdd/bdd.h" /* to check preconditions for EL_fwd */

int CommandCheckCtlSpec(NuSMVEnv_ptr env, int argc, char** argv);
int CommandCheckInvar(NuSMVEnv_ptr env, int argc, char** argv);
int CommandCheckCompute(NuSMVEnv_ptr env, int argc, char** argv);
int CommandCheckPslSpec(NuSMVEnv_ptr env, int argc, char** argv);
int CommandLanguageEmptiness(NuSMVEnv_ptr env, int argc, char** argv);

int CommandCompute(NuSMVEnv_ptr env, int argc, char** argv);
/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static int UsageCheckCtlSpec(const NuSMVEnv_ptr env);
static int UsageCheckInvar(const NuSMVEnv_ptr env);
static int UsageCheckCompute(const NuSMVEnv_ptr env);
static int UsageCheckPslSpec(const NuSMVEnv_ptr env);
static int UsageLanguageEmptiness(const NuSMVEnv_ptr env);

static int UsageCompute(const NuSMVEnv_ptr env);

static int mc_cmd_check_compute(NuSMVEnv_ptr env, int argc, char **argv,
                                int (*usage_fun)(const NuSMVEnv_ptr));

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Mc_Init(NuSMVEnv_ptr env)
{
  Cmd_CommandAdd(env, "check_ctlspec", CommandCheckCtlSpec, 0, true);
  Cmd_CommandAdd(env, "check_invar", CommandCheckInvar, 0, true);
  Cmd_CommandAdd(env, "check_compute", CommandCheckCompute, 0, true);
  Cmd_CommandAdd(env, "check_pslspec", CommandCheckPslSpec, 0, true);
  Cmd_CommandAdd(env, "_language_emptyness", CommandLanguageEmptiness, 0, true);

  /* This one is deprecated */
  Cmd_CommandAdd(env, "compute", CommandCompute, 0, true);
}

void Mc_End(NuSMVEnv_ptr env)
{
  boolean status = true;

  status = status && Cmd_CommandRemove(env, "check_ctlspec");
  status = status && Cmd_CommandRemove(env, "check_invar");
  status = status && Cmd_CommandRemove(env, "check_compute");
  status = status && Cmd_CommandRemove(env, "check_pslspec");
  status = status && Cmd_CommandRemove(env, "_language_emptyness");

  /* This one is deprecated */
  status = status && Cmd_CommandRemove(env, "compute");

  if (!status) {
    ErrorMgr_ptr err_mgr = NuSMVEnv_get_value(env, ENV_ERROR_MANAGER);
    ErrorMgr_internal_error(err_mgr,
                            "Error while deinitializing commands in MC\n");
  }
}

/*!
  \command{check_ctlspec} Performs fair CTL model checking.

  \command_args{[-h] [-m | -o output-file] [-n number | -p "ctl-expr [IN context]" | -P "name"]}

  Performs fair CTL model checking.<p>

  A <tt>ctl-expr</tt> to be checked can be specified at command line
  using option <tt>-p</tt>. Alternatively, option <tt>-n</tt> or
  <tt>-P</tt> can be used for checking a particular formula in the
  property database. If neither <tt>-n</tt> nor <tt>-p</tt> are used,
  all the SPEC formulas in the database are checked.<p>

  Command options:<p>
  <dl>
    <dt> <tt>-m</tt>
       <dd> Pipes the output generated by the command in processing
           <tt>SPEC</tt>s to the program specified by the
           <tt>PAGER</tt> shell variable if defined, else
           through the <tt>UNIX</tt> command "more".
    <dt> <tt>-o output-file</tt>
       <dd> Writes the output generated by the command in processing
           <tt>SPEC</tt>s to the file <tt>output-file</tt>.
    <dt> <tt>-p "ctl-expr [IN context]"</tt>
       <dd> A CTL formula to be checked. <tt>context</tt> is the module
       instance name which the variables in <tt>ctl-expr</tt> must
       be evaluated in.
    <dt> <tt>-n number</tt>
       <dd> Checks the CTL property with index <tt>number</tt> in the property
            database.
    <dt> <tt>-P name</tt>
       <dd> Checks the CTL property with name <tt>name</tt> in the property
            database.
  </dl><p>

  If the <tt>ag_only_search</tt> environment variable has been set, and
  the set of reachable states has already been computed, then a
  specialized algorithm to check AG formulas is used instead of the
  standard model checking algorithms.
*/

int CommandCheckCtlSpec(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* old_outstream = outstream;

  int c;
  int prop_no = -1;
  char* formula = NIL(char);
  char* formula_name = NIL(char);
  int status = 0;
  int useMore = 0;
  char* dbgFileName = NIL(char);
  PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  util_getopt_reset();
  while ((c = util_getopt(argc,argv,"hmo:n:p:P:")) != EOF) {
    switch (c) {
    case 'h': return UsageCheckCtlSpec(env);
    case 'n':
      {
        if (formula != NIL(char)) return UsageCheckCtlSpec(env);
        if (prop_no != -1) return UsageCheckCtlSpec(env);
        if (formula_name != NIL(char)) return UsageCheckCtlSpec(env);

        prop_no = PropDb_get_prop_index_from_string(prop_db,
                                                    util_optarg);
        if (prop_no == -1) return 1;

        break;
      }
    case 'P':
      {
        if (formula != NIL(char)) return UsageCheckCtlSpec(env);
        if (prop_no != -1) return UsageCheckCtlSpec(env);
        if (formula_name != NIL(char)) return UsageCheckCtlSpec(env);

        formula_name = util_strsav(util_optarg);

        prop_no = PropDb_prop_parse_name(prop_db,
                                         formula_name);

        if (prop_no == -1) {
          StreamMgr_print_error(streams,  "No property named \"%s\"\n", formula_name);
          FREE(formula_name);
          return 1;
        }
        FREE(formula_name);
        break;
      }
    case 'p':
      {
        if (prop_no != -1) return UsageCheckCtlSpec(env);
        if (formula != NIL(char)) return UsageCheckCtlSpec(env);
        if (formula_name != NIL(char)) return UsageCheckCtlSpec(env);

        formula = util_strsav(util_optarg);
        break;
      }
    case 'o':
      if (useMore == 1) return UsageCheckCtlSpec(env);
      dbgFileName = util_strsav(util_optarg);
      StreamMgr_print_output(streams,  "Output to file: %s\n", dbgFileName);
      break;
    case 'm':
      if (dbgFileName != NIL(char)) return UsageCheckCtlSpec(env);
      useMore = 1;
      break;
    default:  return UsageCheckCtlSpec(env);
    }
  }
  if (argc != util_optind) return UsageCheckCtlSpec(env);

  /* pre-conditions */
  if (Compile_check_if_model_was_built(env, errstream, false)) return 1;

  if (useMore || (char*)NULL != dbgFileName) {
    if (OUTCOME_SUCCESS !=
        Cmd_Misc_open_pipe_or_file(env, dbgFileName, &outstream)) {
      status = 1; goto check_ctlspec_exit;
    }
  }

  if (formula != NIL(char)) {
    SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
    prop_no = PropDb_prop_parse_and_add(prop_db, st,
                                        formula, Prop_Ctl, Nil);
    if (prop_no == -1) { status = 1; goto check_ctlspec_exit; }
    CATCH(errmgr) {
      PropDb_verify_prop_at_index(prop_db, prop_no);
    }
    FAIL(errmgr) {
      status = 1;
    }
  }
  else if (prop_no != -1) {
    if (Prop_check_type(PropDb_get_prop_at_index(prop_db,
                                                 prop_no),
                        Prop_Ctl) != 0) {
      status = 1;
    }
    else {
      CATCH(errmgr) {
        PropDb_verify_prop_at_index(prop_db, prop_no);
      }
      FAIL(errmgr) {
        status = 1;
      }
    }
  }
  else {
    CATCH(errmgr) {
      if (opt_use_coi_size_sorting(opts)) {
        FlatHierarchy_ptr hierarchy =
          FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));
        PropDb_ordered_verify_all_type(prop_db, hierarchy, Prop_Ctl);
      }
      else PropDb_verify_all_type(prop_db, Prop_Ctl);
    }
    FAIL(errmgr) {
      status = 1;
    }
  }

check_ctlspec_exit:
  if (useMore) {
    FILE* reset_stream;

    CmdClosePipe(outstream);
    reset_stream = StreamMgr_reset_output_stream(streams);
    StreamMgr_set_output_stream(streams, old_outstream);

    nusmv_assert(reset_stream == outstream);

    outstream = (FILE*)NULL;
  }

  if ((char*)NULL != dbgFileName) {
    /* this closes the file stream as well  */
    StreamMgr_set_output_stream(streams, old_outstream);

    outstream = (FILE*)NULL;
  }

  return status;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageCheckCtlSpec(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: check_ctlspec [-h] [-m | -o file] [-n number | -p \"ctl-expr\" | -P \"name\"]\n");
  StreamMgr_print_error(streams,  "   -h \t\t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -m \t\t\tPipes output through the program specified\n");
  StreamMgr_print_error(streams,  "      \t\t\tby the \"PAGER\" environment variable if defined,\n");
  StreamMgr_print_error(streams,  "      \t\t\telse through the UNIX command \"more\".\n");
  StreamMgr_print_error(streams,  "   -o file\t\tWrites the generated output to \"file\".\n");
  StreamMgr_print_error(streams,  "   -n number\t\tChecks only the SPEC with the given index number.\n");
  StreamMgr_print_error(streams,  "   -p \"ctl-expr\"\tChecks only the given CTL formula.\n");
  StreamMgr_print_error(streams,  "   -P \"name\"\t\tChecks only the SPEC with the given name\n");
  return 1;
}

/*!
  \command{check_invar} Performs model checking of invariants

  \command_args{[-h] [-m | -o output-file] [-s "strategy"]
  [-e "heuristic"] [-t number] [-k number] [-j "heuristic"]
  [-n number | -p "invar-expr [IN context]" | -P "name"]}

  Performs invariant checking on the given
  model. An invariant is a set of states. Checking the invariant is
  the process of determining that all states reachable from the
  initial states lie in the invariant.

  Invariants to be verified can be provided as simple formulas
  (without any temporal operators) in the input file via the
  <tt>INVARSPEC</tt> keyword or directly at command line, using the option
  <tt>-p</tt>.<p>

  Option <tt>-n</tt> can be used for checking a particular invariant
  of the model. If neither <tt>-n</tt> nor <tt>-p</tt> are
  used, all the invariants are checked.<p>

  During checking of invariant all the fairness conditions associated
  with the model are ignored.<p>

  If an invariant does not hold, a proof of failure is demonstrated.
  This consists of a path starting from an initial state to a state
  lying outside the invariant. This path has the property that it is the
  shortest path leading to a state outside the invariant.<p>

  Command options:<p>
  <dl>
    <dt> <tt>-m</tt>
       <dd> Pipes the output generated by the program in processing
           <tt>INVARSPEC</tt>s to the program specified by the
           <tt>PAGER</tt> shell variable if defined, else
           through the UNIX command "more".
    <dt> <tt>-o output-file</tt>
       <dd> Writes the output generated by the command in processing
           <tt>INVARSPEC</tt>s to the file <tt>output-file</tt>.
    <dt> <tt>-s <i>strategy</i></tt>
    <dd> Force the analysis strategy.
    <dt> <tt>-e <i>heuristic</i></tt>
    <dd> Force the search heuristic for the forward-backward strategy.
    <dt> <tt>-t <i>number</i></tt>
    <dd> When using the mixed BDD and BMC approach specify the heuristic threshold.
    <dt> <tt>-k <i>number</i></tt>
    <dd> When using the mixed BDD and BMC approach specify the BMC max k.
    <dt> <tt>-j <i>heuristic</i></tt>
    <dd> Force the switch heuristic for the BDD-BMC strategy.
    <dt> <tt>-p "invar-expr [IN context]"</tt>
       <dd> The command line specified invariant formula to be verified.
       <tt>context</tt> is the module instance name which the variables
       in <tt>invar-expr</tt> must be evaluated in.
    <dt> <tt>-P name</tt>
       <dd> Checks the INVARSPEC with name <tt>name</tt> in the property
            database.
  </dl>
*/

int CommandCheckInvar(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* old_outstream = outstream;
  int c;
  int prop_no = -1;
  char* formula = NIL(char);
  char* formula_name = NIL(char);
  int status = 0;
  int useMore = 0;
  char* dbgFileName = NIL(char);
  McCheckInvarOpts options;

  boolean used_s, used_e, used_j, used_k, used_t;

  PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  Prop_ptr prop = NULL;

  McCheckInvarOpts_init(&options, env);

  /* 'Used' variables are initially false */
  used_s = false;
  used_e = false;
  used_j = false;
  used_k = false;
  used_t = false;

  util_getopt_reset();
  while ((c = util_getopt(argc,argv,"hbt:k:j:e:s:mo:n:p:P:")) != EOF) {
    switch (c) {
    case 'h': return(UsageCheckInvar(env));
    case 'n':
      {
        if (formula != NIL(char)) return(UsageCheckInvar(env));
        if (prop_no != -1) return(UsageCheckInvar(env));
        if (formula_name != NIL(char)) return UsageCheckInvar(env);

        prop_no = PropDb_get_prop_index_from_string(prop_db,
                                                    util_optarg);
        if (prop_no == -1) return 1;

        break;
      }
    case 'P':
      {
        if (formula != NIL(char)) return UsageCheckInvar(env);
        if (prop_no != -1) return UsageCheckInvar(env);
        if (formula_name != NIL(char)) return UsageCheckInvar(env);

        formula_name = util_strsav(util_optarg);

        prop_no = PropDb_prop_parse_name(prop_db,
                                         formula_name);

        if (prop_no == -1) {
          StreamMgr_print_error(streams,  "No property named \"%s\"\n", formula_name);
          FREE(formula_name);
          return 1;
        }
        FREE(formula_name);
        break;
      }
    case 'p':
      {
        if (prop_no != -1) return(UsageCheckInvar(env));
        if (formula != NIL(char)) return(UsageCheckInvar(env));
        if (formula_name != NIL(char)) return UsageCheckInvar(env);
        formula = util_strsav(util_optarg);
        break;
      }
    case 'o':
      if (useMore == 1) return(UsageCheckInvar(env));
      dbgFileName = util_strsav(util_optarg);
      StreamMgr_print_output(streams,  "Output to file: %s\n", dbgFileName);
      break;
    case 'm':
      if (dbgFileName != NIL(char)) return(UsageCheckInvar(env));
      useMore = 1;
      break;
    case 's':
      if(used_s) {
        StreamMgr_print_error(streams,  "You cannot specify -s more than once!\n");
      }
      used_s = true;

      if (0 == strcmp("forward", util_optarg)) {
        options.strategy = FORWARD;
      }
      else if (0 == strcmp("backward", util_optarg)) {
        options.strategy = BACKWARD;
      }
      else if (0 == strcmp("forward-backward", util_optarg)) {
        options.strategy = FORWARD_BACKWARD;
      }
#if NUSMV_HAVE_SAT_SOLVER
      else if (0 == strcmp("bdd-bmc", util_optarg)) {
        options.strategy = BDD_BMC;
      }
      else {
        StreamMgr_print_error(streams,  "Wrong strategy specified; you must choose between {'forward', 'backward', 'forward-backward', 'bdd-bmc'}.\n");
        return 1;
      }
#else
      else {
        StreamMgr_print_error(streams,  "Wrong strategy specified; you must choose between {'forward', 'backward', 'forward-backward'}.\n");
        return 1;
      }
#endif
      break;

    case 'e':
      if(used_e) {
        StreamMgr_print_error(streams,  "You cannot specify -e more than once!\n");
      }
      used_e = true;

      if (0 == strcmp("zigzag", util_optarg)) {
        options.fb_heuristic = ZIGZAG_HEURISTIC;
      }
      else if (0 == strcmp("smallest", util_optarg)) {
        options.fb_heuristic = SMALLEST_BDD_HEURISTIC;
      }
      else {
        StreamMgr_print_error(streams,  "Wrong heuristic specified; you must choose between {'zigzag', 'smallest'}.\n");
        return 1;
      }
      break;

    case 'j':
      if(used_j) {
        StreamMgr_print_error(streams,  "You cannot specify -j more than once!\n");
      }
      used_j = true;

      if (0 == strcmp("steps", util_optarg)) {
        options.bdd2bmc_heuristic = STEPS_HEURISTIC;
      }
      else if (0 == strcmp("size", util_optarg)) {
        options.bdd2bmc_heuristic = SIZE_HEURISTIC;
      }
      else {
        StreamMgr_print_error(streams,  "Wrong heuristic specified; you must choose between {'steps', 'size'}.\n");
        return 1;
      }
      break;

    case 't':
      {
        int res;

        if(used_t) {
          StreamMgr_print_error(streams,  "You cannot specify -t more than once!\n");
        }
        used_t = true;

        res = sscanf(util_optarg, "%d", &(options.threshold));
        if (res <= 0) {
          StreamMgr_print_error(streams,  "You must specify a valid number as threshold\n");
          return 1;
        }
        break;
      }
    case 'k':
      {
        int res;

        if(used_k) {
          StreamMgr_print_error(streams,  "You cannot specify -k more than once!\n");
        }
        used_k = true;

        res = sscanf(util_optarg, "%d", &(options.bmc_length));
        if (res <= 0) {
          StreamMgr_print_error(streams,  "You must specify a valid integer number as BMC k\n");
          return 1;
        }
        break;
      }
    default:  return(UsageCheckInvar(env));
    }
  }
  if (argc != util_optind) return(UsageCheckInvar(env));

  /* pre-conditions */
  if (Compile_check_if_model_was_built(env, errstream, false)) return 1;

  if ((BDD_BMC == options.strategy) && \
      (Bmc_check_if_model_was_built(env, errstream, false))) return 1;

  if (used_e) {
    if ((FORWARD_BACKWARD != options.strategy) &&
      (BDD_BMC != options.strategy)) {
    StreamMgr_print_error(streams,  "Unable to use an heuristic without forward-backward or bdd-bmc strategy.\n");
      status = 1;
      goto check_invar_exit;
    }
  }

  if (used_j) {
    if (used_s && BDD_BMC != options.strategy) {
      StreamMgr_print_error(streams,  "Unable to use an heuristic for bdd-bmc while not in bdd-bmc strategy!\n");
      status = 1;
      goto check_invar_exit;
    }
  }

  if (used_t) {
    if (used_s && BDD_BMC != options.strategy) {
        StreamMgr_print_error(streams,  "Unable to use an heuristic threshold for bdd-bmc while not in bdd-bmc strategy!\n");
      status = 1;
      goto check_invar_exit;
    }
  }

 if (used_k) {
   if (used_s && BDD_BMC != options.strategy) {
      StreamMgr_print_error(streams,  "Unable to set BMC k while not in bdd-bmc strategy!\n");
      status = 1;
      goto check_invar_exit;
    }
  }

  if (useMore || (char*)NULL != dbgFileName) {
    if (OUTCOME_SUCCESS !=
        Cmd_Misc_open_pipe_or_file(env, dbgFileName, &outstream)) {
      status = 1; goto check_invar_exit;
    }
  }

  if (formula != NIL(char)) {
    SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
    prop_no = PropDb_prop_parse_and_add(prop_db, st,
                                        formula, Prop_Invar, Nil);
    if (prop_no == -1) { status = 1; goto check_invar_exit; }

    prop = PropDb_get_prop_at_index(prop_db, prop_no);
  }
  else if (prop_no != -1) {
    prop = PropDb_get_prop_at_index(prop_db, prop_no);

    if (Prop_check_type(prop, Prop_Invar) != 0) {
      status = 1;
    }
  }
  else { /* nothing to do */ }

  status = Mc_check_invar(env, prop, &options);

 check_invar_exit:   /* clean exit */
  if (useMore) {
    FILE* reset_stream;

    CmdClosePipe(outstream);
    reset_stream = StreamMgr_reset_output_stream(streams);
    StreamMgr_set_output_stream(streams, old_outstream);

    nusmv_assert(reset_stream == outstream);

    outstream = (FILE*)NULL;
  }

  if ((char*)NULL != dbgFileName) {
    /* this closes the file stream as well  */
    StreamMgr_set_output_stream(streams, old_outstream);

    outstream = (FILE*)NULL;
  }

  return status;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageCheckInvar(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
#if NUSMV_HAVE_SAT_SOLVER
  StreamMgr_print_error(streams,  "usage: check_invar [-h] [-m| -o file] [-s strategy] [-e heuristic] [-t number] [-k number] [-j heuristic] [-n number | -p \"invar-expr\" | -P \"name\"]\n");
#else
  StreamMgr_print_error(streams,  "usage: check_invar [-h] [-m| -o file] [-s strategy] [-e heuristic] [-n number | -p \"invar-expr\" | -P \"name\"]\n");
#endif
  StreamMgr_print_error(streams,  "   -h \t\t\tPrints the command usage.\n");
#if NUSMV_HAVE_SAT_SOLVER
  StreamMgr_print_error(streams,  "   -s \t\t\tForce the analysis strategy (Overrides options). Possible values are {'forward', 'backward', 'forward-backward', 'bdd-bmc'}\n");
#else
  StreamMgr_print_error(streams,  "   -s \t\t\tForce the analysis strategy (Overrides options). Possible values are {'forward', 'backward', 'forward-backward'}\n");
#endif
  StreamMgr_print_error(streams,  "   -e \t\t\tForce the heuristic in case of forward-backward strategy (Overrides options). Possible values are {'zigzag', 'smallest'}\n");
#if NUSMV_HAVE_SAT_SOLVER
  StreamMgr_print_error(streams,  "   -t \t\t\tWhen using the mixed BDD and BMC approach specify the heuristic threshold\n");
  StreamMgr_print_error(streams,  "   -k \t\t\tWhen using the mixed BDD and BMC approach specify the BMC max k\n");
  StreamMgr_print_error(streams,  "   -j \t\t\tSpecify the heuristic used to switch from BDD to BMC. Possible values are {'steps', 'size'}\n");
#endif
  StreamMgr_print_error(streams,  "   -m \t\t\tPipes output through the program specified by\n");
  StreamMgr_print_error(streams,  "      \t\t\tthe \"PAGER\" shell variable if defined,\n");
  StreamMgr_print_error(streams,  "      \t\t\telse through the UNIX command \"more\".\n");
  StreamMgr_print_error(streams,  "   -o file\t\tWrites the generated output to \"file\".\n");
  StreamMgr_print_error(streams,  "   -n number\t\tchecks only the INVARSPEC with the given index number.\n");
  StreamMgr_print_error(streams,  "   -p \"invar-expr\"\tchecks only the given invariant formula.\n");
  StreamMgr_print_error(streams,  "   -P \"name\"\t\tchecks only the INVARSPEC with the given name.\n");
  return 1;
}

/*!
  \command{check_compute} Performs computation of quantitative characteristics

  \command_args{[-h] [-m | -o output-file] [-n number |
  -p "compute-expr [IN context]" | -P "name"]}

  This command deals with the computation of
  quantitative characteristics of real time systems. It is able to
  compute the length of the shortest (longest) path from two given set
  of states.<p>

  <center><code>MAX [ alpha , beta ]</code></center><br>
  <center><code>MIN [ alpha , beta ]</code></center><p>

  Properties of the above form can be specified in the input file via
  the keyword <code>COMPUTE</code> or directly at command line,
  using option <tt>-p</tt>.<p>

  Option <tt>-n</tt> can be used for computing a particular expression
  in the model. If neither <tt>-n</tt> nor <tt>-p</tt> nor <tt>-P</tt>
  are used, all the COMPUTE specifications are computed.<p>

  Command options:<p>
  <dl>
    <dt> <tt>-m</tt>
       <dd> Pipes the output generated by the command in processing
           <tt>COMPUTE</tt>s to the program specified by the
           <tt>PAGER</tt> shell variable if defined, else
           through the UNIX command "more".
    <dt> <tt>-o output-file</tt>
       <dd> Writes the output generated by the command in processing
           <tt>COMPUTE</tt>s to the file <tt>output-file</tt>.
    <dt> <tt>-p "compute-expr [IN context]"</tt>
       <dd> A COMPUTE formula to be checked. <tt>context</tt> is the module
       instance name which the variables in <tt>compute-expr</tt> must
       be evaluated in.
    <dt> <tt>-n number</tt>
       <dd> Computes only the property with index <tt>number</tt>
    <dt> <tt>-P name</tt>
       <dd> Computes only the property named <tt>name</tt>
  </dl>
*/

int CommandCheckCompute(NuSMVEnv_ptr env, int argc, char** argv)
{
  return mc_cmd_check_compute(env, argc, argv, UsageCheckCompute);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageCheckCompute(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: check_compute [-h] [-m | -o file] [-n number | -p \"compute-expr\" | -P \"name\"]\n");
  StreamMgr_print_error(streams,  "   -h \t\t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -m \t\t\tPipes output through the program specified by\n");
  StreamMgr_print_error(streams,  "      \t\t\tthe \"PAGER\" shell variable if defined,\n");
  StreamMgr_print_error(streams,  "      \t\t\telse through the UNIX command \"more\".\n");
  StreamMgr_print_error(streams,  "   -o file\t\tWrites the generated output to \"file\".\n");
  StreamMgr_print_error(streams,  "   -n number\t\tConsiders only the compute expression with the given index number.\n");
  StreamMgr_print_error(streams,  "   -p \"compute-expr\"\tComputes the given expression.\n");
  StreamMgr_print_error(streams,  "   -P name\t\tConsiders only the compute expression with the given name.\n");
  return 1;
}

/*!
  \command{compute} Performs computation of quantitative characteristics

  \command_args{}

  This command is deprecated. It has been substituted by the command check_compute.

  \sa check_compute
*/
int CommandCompute(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  int res = mc_cmd_check_compute(env, argc, argv, UsageCompute);

  StreamMgr_print_error(streams,
          "\nWarning: command 'compute' is deprecated, use "    \
          "'check_compute' instead.\n");
  return res;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageCompute(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: compute [-h] [-m | -o file] [-n number | -p \"compute-expr\" | -P \"name\"]\n");
  StreamMgr_print_error(streams,  "   -h \t\t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -m \t\t\tPipes output through the program specified by\n");
  StreamMgr_print_error(streams,  "      \t\t\tthe \"PAGER\" shell variable if defined,\n");
  StreamMgr_print_error(streams,  "      \t\t\telse through the UNIX command \"more\".\n");
  StreamMgr_print_error(streams,  "   -o file\t\tWrites the generated output to \"file\".\n");
  StreamMgr_print_error(streams,  "   -n number\t\tConsiders only the compute expression with the given index number.\n");
  StreamMgr_print_error(streams,  "   -p \"compute-expr\"\tComputes the given expression.\n");
  StreamMgr_print_error(streams,  "   -P name\t\tConsiders only the compute expression with the given name.\n");
  return 1;
}

/*!
  \command{check_pslspec} Performs fair PSL model checking.

  \command_args{[-h] [-m | -o output-file] [-n number | -p "psl-expr [IN context]" | -P "name"]}

  Performs fair PSL model checking.<p>

  A <tt>psl-expr</tt> to be checked can be specified at command line
  using option <tt>-p</tt>. Alternatively, option <tt>-n</tt> can be used
  for checking a particular formula in the property database. If
  neither <tt>-n</tt> nor <tt>-p</tt> are used, all the PSLSPEC formulas in
  the database are checked.<p>

  Command options:<p>
  <dl>
    <dt> <tt>-m</tt>
       <dd> Pipes the output generated by the command in processing
           <tt>SPEC</tt>s to the program specified by the
           <tt>PAGER</tt> shell variable if defined, else
           through the <tt>UNIX</tt> command "more".
    <dt> <tt>-o output-file</tt>
       <dd> Writes the output generated by the command in processing
           <tt>PSLSPEC</tt>s to the file <tt>output-file</tt>.
    <dt> <tt>-p "psl-expr [IN context]"</tt>
       <dd> A PSL formula to be checked. <tt>context</tt> is the module
       instance name which the variables in <tt>ctl-expr</tt> must
       be evaluated in.
    <dt> <tt>-n number</tt>
       <dd> Checks the PSL property with index <tt>number</tt> in the property
            database.
    <dt> <tt>-P name</tt>
       <dd> Checks the PSL property named <tt>name</tt> in the property
            database.
  </dl><p>


*/

int CommandCheckPslSpec(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  PropDb_ptr const prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  FILE* outstream = StreamMgr_get_output_stream(streams);
  int c;

  int prop_no = -1;
  char* formula = NIL(char);
  char* formula_name = NIL(char);
  int status = 0;
  int useMore = 0;
  char* dbgFileName = NIL(char);
  FILE* old_outstream = NIL(FILE);

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hmo:n:p:P:")) != EOF) {
    switch (c) {
    case 'h': return UsageCheckPslSpec(env);
    case 'n':
    {
      if (formula != NIL(char)) return UsageCheckPslSpec(env);
      if (prop_no != -1) return UsageCheckPslSpec(env);
      if (formula_name != NIL(char)) return UsageCheckPslSpec(env);

      prop_no = PropDb_get_prop_index_from_string(prop_db,
                                                  util_optarg);
      if (prop_no == -1) return 1;

      break;
    }
    case 'P':
      {
        if (formula != NIL(char)) return UsageCheckPslSpec(env);
        if (prop_no != -1) return UsageCheckPslSpec(env);
        if (formula_name != NIL(char)) return UsageCheckPslSpec(env);

        formula_name = util_strsav(util_optarg);

        prop_no = PropDb_prop_parse_name(prop_db,
                                         formula_name);

        if (prop_no == -1) {
          StreamMgr_print_error(streams,  "No property named \"%s\"\n", formula_name);
          FREE(formula_name);
          return 1;
        }
        FREE(formula_name);
        break;
      }

    case 'p':
    {
      if (prop_no != -1) return UsageCheckPslSpec(env);
      if (formula != NIL(char)) return UsageCheckPslSpec(env);
      if (formula_name != NIL(char)) return UsageCheckPslSpec(env);

      formula = util_strsav(util_optarg);
      break;
    }
    case 'o':
      if (useMore == 1) return UsageCheckPslSpec(env);
      dbgFileName = util_strsav(util_optarg);
      StreamMgr_print_output(streams,  "Output to file: %s\n", dbgFileName);
      break;

    case 'm':
      if (dbgFileName != NIL(char)) return UsageCheckPslSpec(env);
      useMore = 1;
      break;
    default:  return UsageCheckPslSpec(env);
    }
  }
  if (argc != util_optind) return UsageCheckPslSpec(env);

  /* ---------------------------------------------------------------------- */

  /* Checking compilation status */
  if (Compile_check_if_encoding_was_built(env, errstream)) return 1;

  /* Model construction is delayed until property is being checked,
     according to the specific technique that is being used. */

  if (useMore) {
    old_outstream = outstream;
    outstream = CmdOpenPipe(env, useMore);
    if (outstream==(FILE*) NULL) {outstream=old_outstream; return 1;}
    /* StreamMgr_set_output_stream(streams, outstream); */
  }

  if (dbgFileName != NIL(char)) {
    old_outstream = outstream;
    outstream = CmdOpenFile(env, dbgFileName);
    if (outstream==(FILE*) NULL) {outstream = old_outstream; return 1;}
    /* StreamMgr_set_output_stream(streams, outstream); */
  }

  if (formula != NIL(char)) {
    SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
    prop_no = PropDb_prop_parse_and_add(prop_db, st,
                                        formula, Prop_Psl, Nil);
    if (prop_no == -1) { status = 1; goto check_psl_exit; }
  }

  status = Mc_check_psl_spec(env, prop_no);

check_psl_exit:
  if (useMore) {
    CmdClosePipe(outstream);
    /* StreamMgr_set_output_stream(streams, old_outstream); */
    outstream = old_outstream;
  }

  if (dbgFileName != NIL(char)) {
    CmdCloseFile(outstream);
    /* StreamMgr_set_output_stream(streams, old_outstream); */
    outstream = old_outstream;
  }

  return status;
}

/*!
  \brief -h option output

  -h option output
*/
static int UsageCheckPslSpec(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: check_pslspec [-h] [-m | -o file] [-n number | -p \"psl-expr\" | -P \"name\"]\n");
  StreamMgr_print_error(streams,  "   -h \t\t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -m \t\t\tPipes output through the program specified\n");
  StreamMgr_print_error(streams,  "      \t\t\tby the \"PAGER\" environment variable if defined,\n");
  StreamMgr_print_error(streams,  "      \t\t\telse through the UNIX command \"more\".\n");
  StreamMgr_print_error(streams,  "   -o file\t\tWrites the generated output to \"file\".\n");
  StreamMgr_print_error(streams,  "   -n number\t\tChecks only the PSLSPEC with the given index number.\n");
  StreamMgr_print_error(streams,  "   -p \"psl-expr\"\tChecks only the given PSL formula.\n");
  StreamMgr_print_error(streams,  "   -P \"name\"\t\tChecks only the PSLSPEC with the given name.\n");

  return 1;
}


/*!
  \command{language_emptiness} Checks for language emptiness.

  \command_args{[-h] [-v] [-a]}

  Checks for the language emptiness. <br>

  If <tt>-a</tt> is given the check is performed by verifying whether
  all initial states are included in the set of fair states. If it is
  the case from all initial states there exists a fair path and thus
  the language is not empty. On the other hand, if no <tt>-a</tt> is
  specified, the check is performed by verifying whether there exists
  at least one inital state that is also a fair state. In this case
  there is an initial state from which it starts a fair path and thus
  the lnaguage is not empty.

  if <tt>-v</tt> is specified, then some information on the set of
  initial states is printed out too.
*/

int CommandLanguageEmptiness(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  int c;
  boolean allinit = false;
  boolean verbose = false;
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hva")) != EOF) {
    switch (c) {
    case 'h': return UsageLanguageEmptiness(env);
    case 'v':
      verbose = true;
      break;
    case 'a':
      allinit = true;
      break;
    default:
      return UsageLanguageEmptiness(env);
    }
  }

  if (Compile_check_if_model_was_built(env, errstream, true)) return 1;

  /* [VS] I guess a check that there is no compassion would be in
     order. It that is true, corresponding checks should be added to
     the lower level functions, too. */
  if (get_oreg_justice_emptiness_bdd_algorithm(opts) ==
      BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD) {
    if (allinit) {
      /* VS: See comment in mcLE.c why allinit is not implemented. */
      StreamMgr_print_error(streams,
              "Forward Emerson-Lei cannot be used to check whether all "\
              "initial states are fair.\n");
      return 1;
    }
  }

  StreamMgr_print_output(streams,
          "######################################################################\n");
  Mc_CheckLanguageEmptiness(env,
              BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM)),
              allinit, verbose);
  StreamMgr_print_output(streams,
          "######################################################################\n");

  return 0;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageLanguageEmptiness(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: language_emptyness [-h] [-v] [-a]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -v \t\tPrints some information on the list of initial fair states.\n");
  StreamMgr_print_error(streams,  "   -a \t\tChecks all initial states for being fair states.\n");
  StreamMgr_print_error(streams,  "      \t\tOtherwise checks for the existence of at least a fair initial state.\n");
  return 1;
}

/*!
  \brief helper function of commands check_compute and compute

  helper function of commands check_compute and compute

  \sa CommandCheckCompute CommandCompute
*/
static int mc_cmd_check_compute(NuSMVEnv_ptr env, int argc, char **argv,
                                int (*usage_fun)(const NuSMVEnv_ptr))
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* old_outstream = outstream;
  int c;
  int prop_no = -1;
  char * formula = NIL(char);
  char * formula_name = NIL(char);
  int status = 0;
  int useMore = 0;
  char * dbgFileName = NIL(char);
  PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  util_getopt_reset();
  while ((c = util_getopt(argc,argv,"hmo:n:p:P:")) != EOF) {
    switch (c) {
    case 'h': return usage_fun(env);
    case 'n':
      {
        if (formula != NIL(char)) return usage_fun(env);
        if (prop_no != -1) return usage_fun(env);
        if (formula_name != NIL(char)) return usage_fun(env);

        prop_no = PropDb_get_prop_index_from_string(prop_db, util_optarg);
        if (prop_no == -1) return 1;

        break;
      }
    case 'P':
      {
        if (formula != NIL(char)) return usage_fun(env);
        if (prop_no != -1) return usage_fun(env);
        if (formula_name != NIL(char)) return usage_fun(env);

        formula_name = util_strsav(util_optarg);

        prop_no = PropDb_prop_parse_name(prop_db,
                                         formula_name);

        if (prop_no == -1) {
          StreamMgr_print_error(streams,  "No property named \"%s\"\n", formula_name);
          FREE(formula_name);
          return 1;
        }
        FREE(formula_name);
        break;
      }
    case 'p':
      {
        if (prop_no != -1) return usage_fun(env);
        if (formula != NIL(char)) return usage_fun(env);
        if (formula_name != NIL(char)) return usage_fun(env);

        formula = util_strsav(util_optarg);
        break;
      }
    case 'o':
      if (useMore == 1) return usage_fun(env);
      dbgFileName = util_strsav(util_optarg);
      StreamMgr_print_output(streams,  "Output to file: %s\n", dbgFileName);
      break;
    case 'm':
      if (dbgFileName != NIL(char)) return usage_fun(env);
      useMore = 1;
      break;

    default: return usage_fun(env);
    }
  }
  if (argc != util_optind) return usage_fun(env);

  /* pre-conditions */
  if (Compile_check_if_model_was_built(env, errstream, false)) return 1;

  if (useMore || (char*)NULL != dbgFileName) {
    if (OUTCOME_SUCCESS !=
        Cmd_Misc_open_pipe_or_file(env, dbgFileName, &outstream)) {
      status = 1; goto check_compute_exit;
    }
  }

  if (formula != NIL(char)) {
    SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
    prop_no = PropDb_prop_parse_and_add(prop_db, st,
                                        formula, Prop_Compute, Nil);
    if (prop_no == -1) { status = 1; goto check_compute_exit; }
    CATCH(errmgr) {
      PropDb_verify_prop_at_index(prop_db, prop_no);
    }
    FAIL(errmgr) {
      status = 1;
    }
  }
  else if (prop_no != -1) {
    Prop_ptr prop = PropDb_get_prop_at_index(prop_db, prop_no);
    if (Prop_check_type(prop, Prop_Compute) != 0) {
      status = 1;
    }
    else {
      CATCH(errmgr) {
        PropDb_verify_prop_at_index(prop_db, prop_no);
      }
      FAIL(errmgr) {
        status = 1;
      }
    }
  }
  else {
    CATCH(errmgr) {
      if (opt_use_coi_size_sorting(opts)) {
        FlatHierarchy_ptr hierarchy =
          FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));

        PropDb_ordered_verify_all_type(prop_db, hierarchy, Prop_Compute);
      }
      else PropDb_verify_all_type(prop_db, Prop_Compute);
    }
    FAIL(errmgr) {
      status = 1;
    }
  }

check_compute_exit:   /* clean exit */
  if (useMore) {
    FILE* reset_stream;

    CmdClosePipe(outstream);
    reset_stream = StreamMgr_reset_output_stream(streams);
    StreamMgr_set_output_stream(streams, old_outstream);

    nusmv_assert(reset_stream == outstream);

    outstream = (FILE*)NULL;
  }

  if ((char*)NULL != dbgFileName) {
    /* this closes the file stream as well  */
    StreamMgr_set_output_stream(streams, old_outstream);

    outstream = (FILE*)NULL;
  }

  return status;

}
