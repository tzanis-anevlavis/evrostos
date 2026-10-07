/* ---------------------------------------------------------------------------


  This file is part of the ``simulate'' package of NuSMV version 2.
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
  \author Andrea Morichetti
  \brief Model Checker Simulator Commands

  This file contains commands to be used for the simulation feature.

*/

#include "nusmv/shell/cmd/cmd.h"
#include "nusmv/shell/simulate/simulateCmd.h"

#include "nusmv/core/simulate/SimulateState.h"
#include "nusmv/core/simulate/simulate.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/error.h" /* for CATCH(errmgr) */
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/prop/Prop.h"
#include "nusmv/core/prop/PropDb.h"
#include "nusmv/core/prop/propPkg.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/parser.h"
#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/mc/mc.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/utils/ucmd.h"
#include "nusmv/core/compile/compile.h"

/* Prototypes of command functions */
int CommandSimulate(NuSMVEnv_ptr env, int argc, char** argv);
int CommandPickState(NuSMVEnv_ptr env, int argc, char** argv);
int CommandGotoState(NuSMVEnv_ptr env, int argc, char** argv);
int CommandPrintCurrentState(NuSMVEnv_ptr env, int argc, char** argv);

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static int UsageSimulate(const NuSMVEnv_ptr env);
static int UsagePickState(const NuSMVEnv_ptr env);
static int UsageGotoState(const NuSMVEnv_ptr env);
static int UsagePrintCurrentState(const NuSMVEnv_ptr env);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Simulate_Cmd_init(NuSMVEnv_ptr env)
{
  Cmd_CommandAdd(env, "simulate", CommandSimulate, 0, true);
  Cmd_CommandAdd(env, "pick_state", CommandPickState, 0, true);
  Cmd_CommandAdd(env, "goto_state", CommandGotoState, 0, true);
  Cmd_CommandAdd(env, "print_current_state", CommandPrintCurrentState, 0, true);
}

void Simulate_Cmd_quit(NuSMVEnv_ptr env)
{
  boolean status = true;

  status = status && Cmd_CommandRemove(env, "simulate");
  status = status && Cmd_CommandRemove(env, "pick_state");
  status = status && Cmd_CommandRemove(env, "goto_state");
  status = status && Cmd_CommandRemove(env, "print_current_state");

  nusmv_assert(status);
}

/*!
  \command{pick_state} Picks a state from the set of initial states

  \command_args{[-h] [-v] [-r | -i [-a]] [-c "constraints" | -s trace.state]}



  Chooses an element from the set of initial states, and makes it the
  <tt>current state</tt> (replacing the old one). The chosen state is
  stored as the first state of a new trace ready to be lengthened by
  <tt>steps</tt> states by the <tt>simulate</tt> command. The state can be
  chosen according to different policies which can be specified via command
  line options. By default the state is chosen in a deterministic way.
  <p>
  Command Options:<p>
  <dl>
    <dt> <tt>-v</tt>
       <dd> Verbosely prints out chosen state (all state variables, otherwise
       it prints out only the label <tt>t.1</tt> of the state chosen, where
       <tt>t</tt> is the number of the new trace, that is the number of
       traces so far generated plus one).
    <dt> <tt>-r</tt>
       <dd> Randomly picks a state from the set of initial states.
    <dt> <tt>-i</tt>
       <dd> Enables the user to interactively pick up an initial state. The
       user is requested to choose a state from a list of possible items
       (every item in the list doesn't show state variables unchanged with
       respect to a previous item). If the number of possible states is too
       high, then the user has to specify some further constraints as
       "simple expression".
    <dt> <tt>-a</tt>
       <dd> Displays all state variables (changed and unchanged with respect
       to a previous item) in an interactive picking. This option
       works only if the <tt>-i</tt> options has been specified.
    <dt> <tt>-c "constraints"</tt>
       <dd> Uses <tt>constraints</tt> to restrict the set of initial states
       in which the state has to be picked.
    <dt> <tt>-s trace.state</tt>
       <dd> Picks state from trace.state label. A new simulation trace will
       be created by copying prefix of the source trace up to specified state.
  </dl>

  \sa goto_state simulate
*/

int CommandPickState(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  TraceMgr_ptr const gtm =
    TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
  DDMgr_ptr const dd_mgr = DD_MGR(NuSMVEnv_get_value(env, ENV_DD_MGR));
  BddEnc_ptr bdd_enc = NULL;
  NodeMgr_ptr const nodemgr =
     NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  FILE* errstream = StreamMgr_get_error_stream(streams);
  int res = 1;
  int c = 0;
  boolean verbose = false;
  int display_all = 0;
  char *strConstr = NIL(char);
  char *strLabel = NIL(char);
  Simulation_Mode mode = Deterministic;
  short int usedMode = 0;
  TraceLabel label = TRACE_LABEL_INVALID;
  bdd_ptr bdd_constraints = NULL;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hriavc:s:")) != EOF) {
    switch(c){
    case 'h': return UsagePickState(env);
    case 'r':
      if (++usedMode > 1) goto simulate_cmd_pick_state_usage;
      mode = Random;
      break;
    case 'i':
      if (++usedMode > 1) goto simulate_cmd_pick_state_usage;
      mode = Interactive;
      break;
    case 'a':
      display_all = 1;
      break;
    case 'v':
      verbose = true;
      break;
    case 'c':
      strConstr = util_strsav(util_optarg);
      break;
    case 's':
      strLabel = util_strsav(util_optarg);
      break;
    default:
      goto simulate_cmd_pick_state_usage;
    }
  }

  if ((mode != Interactive) && (display_all == 1)) {
    goto simulate_cmd_pick_state_usage;
  }

  if (argc != util_optind) goto simulate_cmd_pick_state_usage;

  /* pre-conditions */
  if (Compile_check_if_model_was_built(env, errstream, true)) {
    goto simulate_cmd_pick_state_free;
  }

  if (strLabel != (char*) NULL && strConstr != (char*) NULL) {
    StreamMgr_print_error(streams,
                          "Options -c and -s cannot be used at the same time\n");
    res = 1;
    goto simulate_cmd_pick_state_free;
  }

  if (strLabel != (char*) NULL) {
    label = TraceLabel_create_from_string(nodemgr, strLabel);

    if (label == TRACE_LABEL_INVALID ||   \
        !TraceMgr_is_label_valid(gtm, label)) {

      StreamMgr_print_error(streams,  "Label \"%s\" is invalid\n", strLabel);

      res = 1;
      goto simulate_cmd_pick_state_free;
    }
  }

  if (strConstr != NIL(char)) {
    bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
    bdd_constraints =
      simulate_get_constraints_from_string(env, strConstr, bdd_enc,
                                           false, /* no nexts */
                                           false  /* only states*/);

    if (bdd_constraints == (bdd_ptr) NULL) {
      res = 1;
      goto simulate_cmd_pick_state_free;
    }
  } /* end of constraints processing */

  res = Simulate_pick_state(env, label, mode, display_all,
                            verbose, bdd_constraints);

  goto simulate_cmd_pick_state_free;

 simulate_cmd_pick_state_usage:
  res = UsagePickState(env);

 simulate_cmd_pick_state_free:
  if (NIL(char) != strLabel) { FREE(strLabel); }
  if (NIL(char) != strConstr) { FREE(strConstr); }
  if (NULL != bdd_constraints) bdd_free(dd_mgr, bdd_constraints);

  return res;
}

static int UsagePickState (const NuSMVEnv_ptr env) {
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: pick_state [-h] [-v] [-r | -i [-a]] [-c \"constr\" | -s trace.state]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -v \t\tVerbosely prints picked state.\n");
  StreamMgr_print_error(streams,  "  -r \t\tRandomly picks a state from the set of the initial states\n");
  StreamMgr_print_error(streams,  "     \t\t(otherwise choice is deterministic).\n");
  StreamMgr_print_error(streams,  "  -i \t\tLets the user interactively pick a state from\n");
  StreamMgr_print_error(streams,  "     \t\tthe set of initial ones.\n");
  StreamMgr_print_error(streams,  "  -a \t\tDisplays all the state variables (changed and\n");
  StreamMgr_print_error(streams,  "   \t\tunchanged) in an interactive session.\n");
  StreamMgr_print_error(streams,  "   \t\tIt works only together with -i option.\n");
  StreamMgr_print_error(streams,  "  -c \"constr\"   Sets constraints for the initial set of states.\n");
  StreamMgr_print_error(streams,  "  -s state\tPicks state from trace.state label.\n");
  return 1;
}

/*!
  \command{simulate} Performs a simulation from the current selected state

  \command_args{[-h] [-p | -v] [-r | -i [-a]]
  [[-c "constraints"] | [-t "constraints"] ] [-k steps]
  }


  Generates a sequence of at most <tt>steps</tt> states (representing a
  possible execution of the model), starting from the <em>current state</em>.
  The current state must be set via the <em>pick_state</em> or
  <em>goto_state</em> commands.<p>
  It is possible to run the simulation in three ways (according to different
  command line policies):
  deterministic (the default mode), random and interactive.<p>
  The resulting sequence is stored in a trace indexed with an integer number
  taking into account the total number of traces stored in the system. There is
  a different behavior in the way traces are built, according to how
  <em>current state</em> is set: <em>current state</em> is always put at
  the beginning of a new trace (so it will contain at most <it>steps + 1</it>
  states) except when it is the last state of an existent old trace.
  In this case the old trace is lengthened by at most <it>steps</it> states.
  <p>
  Command Options:<p>
  <dl>
    <dt> <tt>-p</tt>
       <dd> Prints current generated trace (only those variables whose value
       changed from the previous state).
    <dt> <tt>-v</tt>
       <dd> Verbosely prints current generated trace (changed and unchanged
       state variables).
    <dt> <tt>-r</tt>
       <dd> Picks a state from a set of possible future states in a random way.
    <dt> <tt>-i</tt>
       <dd> Enables the user to interactively choose every state of the trace,
       step by step. If the number of possible states is too high, then
       the user has to specify some constraints as simple expression.
       These constraints are used only for a single simulation step and
       are <em>forgotten</em> in the following ones. They are to be intended
       in an opposite way with respect to those constraints eventually entered
       with the pick_state command, or during an interactive simulation
       session (when the number of future states to be displayed is too high),
       that are <em>local</em> only to a single step of the simulation and
       are <em>forgotten</em> in the next one.
    <dt> <tt>-a</tt>
       <dd> Displays all the state variables (changed and unchanged) during
       every step of an interactive session. This option works only if the
       <tt>-i</tt> option has been specified.
    <dt> <tt>-c "constraints"</tt>
       <dd> Performs a simulation in which computation is restricted
       to states satisfying those <tt>constraints</tt>. The desired
       sequence of states could not exist if such constraints were too
       strong or it may happen that at some point of the simulation a
       future state satisfying those constraints doesn't exist: in
       that case a trace with a number of states less than
       <tt>steps</tt> trace is obtained. The expression cannot contain
       next operators, and is automatically shifted by one state in
       order to constraint only the next steps
    <dt> <tt>-t "constraints"</tt>
       <dd> Performs a simulation in which computation is restricted
       to states satisfying those <tt>constraints</tt>. The desired
       sequence of states could not exist if such constraints were too
       strong or it may happen that at some point of the simulation a
       future state satisfying those constraints doesn't exist: in
       that case a trace with a number of states less than
       <tt>steps</tt> trace is obtained.  The expression can contain
       next operators, and is NOT automatically shifted by one state
       as done with option -c
    <dt> <tt>-k steps</tt>
       <dd> Maximum length of the path according to the constraints.
       The length of a trace could contain less than <tt>steps</tt> states:
       this is the case in which simulation stops in an intermediate
       step because it may not exist any future state satisfying those
       constraints.
    </dl>

  \sa pick_state goto_state
*/

int CommandSimulate(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  BddEnc_ptr enc;
  DDMgr_ptr dd;
  bdd_ptr bdd_constraints = (bdd_ptr) NULL;
  boolean isconstraint = false;
  boolean printrace = false;
  int display_all = 0;
  int c = 0;
  boolean only_changes = 1;
  boolean time_shift = false;
  int steps = get_default_simulation_steps(opts);
  Simulation_Mode mode = Deterministic;
  boolean k_specified = false;
  /* the string of constraint to parsificate */
  char* strConstr = NIL(char);
  int res = 0;

  util_getopt_reset();
  while((c = util_getopt(argc,argv,"t:c:hpvriak:")) != EOF){
    switch(c){
    case 'h': return UsageSimulate(env);
    case 'p':
      if (printrace == true) return UsageSimulate(env);
      printrace = true;
      only_changes = true;
      break;
    case 'v':
      if (printrace == true) return UsageSimulate(env);
      printrace = true;
      only_changes = false;
      break;
    case 'r':
      if (mode == Interactive) return UsageSimulate(env);
      mode = Random;
      break;
    case 'i':
      if (mode == Random) return UsageSimulate(env);
      mode = Interactive;
      break;
    case 'a':
      display_all = 1;
      break;
    case 'c':
      if (NIL(char) != strConstr) return UsageSimulate(env);
      strConstr = util_strsav(util_optarg);
      isconstraint = true;
      time_shift = true;
      break;
    case 't':
      if (NIL(char) != strConstr) return UsageSimulate(env);
      strConstr = util_strsav(util_optarg);
      isconstraint = true;
      time_shift = false;
      break;

    case 'k':
      {
        char* strNumber;

        if (k_specified) {
          StreamMgr_print_error(streams,
                  "Option -k cannot be specified more than once.\n");
          return 1;
        }

        strNumber = util_strsav(util_optarg);

        if (util_str2int(strNumber, &steps) != 0) {
          ErrorMgr_error_invalid_number(errmgr, strNumber);
          FREE(strNumber);
          return 1;
        }

        if (steps < 0) {
           ErrorMgr_error_invalid_number(errmgr, strNumber);
           FREE(strNumber);
          return 1;
        }

        FREE(strNumber);
        k_specified = true;
        break;
      }

    default:
      return UsageSimulate(env);
    }
  }

  if ((mode != Interactive) && (display_all == 1)) return UsageSimulate(env);

  if (argc == util_optind + 1) {
    char* strNumber;

    StreamMgr_print_error(streams,  "*** Warning: Parameter \"steps\" is deprecated. "
            "Use option \"-k\" instead\n");

    if (k_specified) {
      StreamMgr_print_error(streams,  "Error: Parameter \"steps\" conflicts with option -k\n");
      return 1;
    }

    strNumber = util_strsav(argv[util_optind]);

    if (util_str2int(strNumber, &steps) != 0) {
      ErrorMgr_error_invalid_number(errmgr, strNumber);
      FREE(strNumber);
      return 1;
    }

    if (steps < 0) {
      ErrorMgr_error_invalid_number(errmgr, strNumber);
      FREE(strNumber);
      return 1;
    }

    FREE(strNumber);
    k_specified = true;
  }
  else if (argc != util_optind) {
    return UsageSimulate(env);
  }

  /* pre-conditions */
  if (Compile_check_if_model_was_built(env, errstream, true)) return 1;

  if (! NuSMVEnv_has_value(env, ENV_SIMULATE_STATE)) {
    StreamMgr_print_error(streams,
            "No current state set. Use the \"pick_state\" command.\n");
    return 1;
  }

  enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  dd = BddEnc_get_dd_manager(enc);

  if (isconstraint) {
    bdd_constraints =
      simulate_get_constraints_from_string(env, strConstr, enc,
                                           !time_shift, true /* inputs */);
    if (bdd_constraints == (bdd_ptr) NULL) res = 1;
  }
  else bdd_constraints = bdd_true(dd);

  if (0 == res) {
    res = Simulate_simulate(env, time_shift, mode,
                            steps, display_all, printrace, only_changes,
                            bdd_constraints);
  }

  FREE(strConstr);
  if ((bdd_ptr)NULL != bdd_constraints) bdd_free(dd, bdd_constraints);

  return res;

} /* Command Simulate */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageSimulate(const NuSMVEnv_ptr env){
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,
          "usage: simulate [-h] [-p | -v] [-r | -i [-a]] [[-c \"constr\"] "
          "| [-t \"constr\"]] [-k steps]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -p \t\tPrints current generated trace (only changed variables).\n");
  StreamMgr_print_error(streams,  "  -v \t\tVerbosely prints current generated trace (all variables).\n");
  StreamMgr_print_error(streams,  "  -r \t\tSets picking mode to random (default is deterministic).\n");
  StreamMgr_print_error(streams,  "  -i \t\tEnters simulation's interactive mode.\n");
  StreamMgr_print_error(streams,  "  -a \t\tDisplays all the state variables (changed and unchanged)\n");
  StreamMgr_print_error(streams,  "     \t\tin every step of an interactive session.\n");
  StreamMgr_print_error(streams,  "     \t\tIt works only together with -i option.\n");
  StreamMgr_print_error(streams,  "  -c \"constr\"\tSets constraint (simple expression) for the next steps.\n");
  StreamMgr_print_error(streams,  "  -t \"constr\"\tSets constraint (next expression) for the next steps.\n");
  StreamMgr_print_error(streams,  "  -k <length> \tSpecifies the simulation length\n"
          "\t\tto be used when generating the simulated problem.\n");
  return 1;
}

/*!
  \command{goto_state} Goes to a given state of a trace

  \command_args{[-h] state}

  Makes <tt>state</tt> the <em>current
  state</em>. This command is used to navigate alongs traces
  produced by NuSMV. During the navigation, there is a <em>current
  state</em>, and the <em>current trace</em> is the trace the
  <em>current state</em> belongs to.
    Command options:<p>
    <dl>
      <dt><tt>state: </tt>
      <dd> The state of a trace (trace.state) to be picked.
    </dl>

*/

int CommandGotoState(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  int c;
  int status = 0;
  TraceLabel label = NULL;

  util_getopt_reset();

  while ((c = util_getopt(argc,argv,"h")) != EOF) {
    switch (c) {
    case 'h': return UsageGotoState(env);
    default:  return UsageGotoState(env);
    }
  }
  if (argc == 1) return UsageGotoState(env);

  /* pre-conditions */
  if (Compile_check_if_model_was_built(env, errstream, true)) return 1;

  argv += util_optind-1;
  argc -= util_optind-1;
  label = TraceLabel_create_from_string(nodemgr, argv[1]);

  if (label != TRACE_LABEL_INVALID) {
    if (TraceMgr_is_label_valid(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                                label)) {

      status = Simulate_goto_state(env, label);

    }
    else {
      StreamMgr_print_error(streams,  "The label %d.%d is invalid.\n",
                            TraceLabel_get_trace(label) + 1,
                            TraceLabel_get_state(label) + 1);

      /* [MD] My opinion is that here should be an error status, but
         discussion is needed */
      status = 0;
    }
  }
  else {
    StreamMgr_print_error(streams,  "Parsing error: expected "                  \
                          "\"goto_state <trace_number>.<state_number>\".\n");
    status = 1;
  }

  return status;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageGotoState(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: goto_state [-h] state\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   state \tSets current state to \"state\".\n");
  return 1;
}

/*!
  \command{print_current_state} Prints out the current state

  \command_args{[-h] [-v]}

  Prints the name of the <em>current state</em> if
  defined.<p>

  Command options:<p>
  <dl>
    <dt> <tt>-v</tt>
       <dd> Prints the value of all the state variables of the <em>current
       state</em>.
  </dl>

*/

int CommandPrintCurrentState(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  int c;
  int Verbosely = 1;
  SexpFsm_ptr scalar_fsm;
  OStream_ptr outstream = StreamMgr_get_output_ostream(streams);

  util_getopt_reset();
  while ((c = util_getopt(argc,argv,"hv")) != EOF) {
    switch (c) {
    case 'h': return UsagePrintCurrentState(env);
    case 'v': {
      Verbosely = 0;
      break;
    }
    default:  return UsagePrintCurrentState(env);
    }
  }

  if (argc != util_optind) return UsagePrintCurrentState(env);

  if (NuSMVEnv_has_value(env, ENV_SIMULATE_STATE)) {
    return Simulate_print_current_state(env, Verbosely);
  }
  else {
    if (TraceMgr_get_current_trace_number(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR))) >= 0) {
      StreamMgr_print_output(streams,  "The current state has not yet been defined.\n");
      StreamMgr_print_output(streams,
              "Use \"goto_state\" to define the current state.\n");
    }
    else {
      StreamMgr_print_output(streams,
              "There is no trace actually stored in the system.\n");
      StreamMgr_print_output(streams,
              "Use \"pick_state\" to define the current state.\n");
    }
    return 1;
  }

  return 0;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsagePrintCurrentState(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: print_current_state [-h] [-v]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,
          "   -v \t\tPrints the value of each state variable in the current state.\n");
  return 1;
}
