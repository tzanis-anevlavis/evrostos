/* ---------------------------------------------------------------------------


  This file is part of the ``bmc'' package of NuSMV version 2.
  Copyright (C) 2000-2001 by FBK-irst and University of Trento.

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
  \author Roberto Cavada
  \brief Bmc.Cmd module

  This module contains all the bmc commands implementation.
  Options parsing and checking is performed here, than the high-level Bmc
  layer is called

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/shell/cmd/cmd.h"
#include "nusmv/shell/bmc/bmcCmd.h"

#include "nusmv/core/bmc/bmc.h"
#include "nusmv/core/bmc/bmcBmc.h"
#include "nusmv/core/bmc/bmcUtils.h"
#include "nusmv/core/bmc/bmcSimulate.h"
#include "nusmv/core/bmc/bmcTest.h"
#include "nusmv/shell/bmc/sbmc/sbmcCmd.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/prop/propPkg.h"
#include "nusmv/core/simulate/simulate.h"
#include "nusmv/core/trace/exec/SATCompleteTraceExecutor.h"
#include "nusmv/core/trace/exec/SATPartialTraceExecutor.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/wff/w2w/w2w.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BMC_USAGE 2

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static int UsageBmcSetup(const NuSMVEnv_ptr env);
static int UsageBmcSimulate(const NuSMVEnv_ptr env);
static int UsageBmcIncSimulate(const NuSMVEnv_ptr env);
static int UsageBmcGenLtlSpec(const NuSMVEnv_ptr env);
static int UsageBmcGenLtlSpecOnePb(const NuSMVEnv_ptr env);
static int UsageBmcCheckLtlSpec(const NuSMVEnv_ptr env);
static int UsageBmcCheckLtlSpecOnePb(const NuSMVEnv_ptr env);
static int UsageBmcGenInvar(const NuSMVEnv_ptr env);
static int UsageBmcCheckInvar(const NuSMVEnv_ptr env);
static int UsageBmcSimulateCheckFeasibleConstraints(const NuSMVEnv_ptr env);
static int UsageBmcPickState(const NuSMVEnv_ptr env);
static int UsageCheckPslSpecBmc(const NuSMVEnv_ptr env);
static int UsageBmcTestTableau(const NuSMVEnv_ptr env);

static int bmc_CommandCheckPslSpecBmc(NuSMVEnv_ptr env, int argc, char** argv);

#if NUSMV_HAVE_INCREMENTAL_SAT
static int UsageBmcCheckLtlSpecInc(const NuSMVEnv_ptr env);
static int UsageBmcCheckInvarInc(const NuSMVEnv_ptr env);
static int UsageCheckPslSpecBmcInc(const NuSMVEnv_ptr env);
static int bmc_CommandCheckPslSpecBmcInc(NuSMVEnv_ptr env, int argc, char** argv);
#endif

static inline int
bmc_cmd_gen_solve_ltl_selected_or_all_props(const NuSMVEnv_ptr env,
                                        const Prop_ptr ltlprop,
                                        const int k, const int relative_loop,
                                        const char* fname,
                                        const boolean has_to_iter_over_k,
                                        const boolean has_to_solve);

static inline int
bmc_cmd_gen_solve_ltl_inc_selected_or_all_props(const NuSMVEnv_ptr env,
                                                const Prop_ptr ltlprop,
                                                const int k,
                                                const int relative_loop,
                                                const boolean must_inc_length);

static inline int
bmc_cmd_gen_solve_invar_selected_or_all_props(const NuSMVEnv_ptr env,
                                              const Prop_ptr invarprop,
                                              const char* fname,
                                              const boolean has_to_solve);

static inline int
bmc_cmd_gen_solve_invar_een_selected_or_all_props(const NuSMVEnv_ptr env,
                                                  const Prop_ptr invarprop,
                                                  const int max_k,
                                                  const char* fname,
                                                  const boolean use_extra_step);

static inline int
bmc_cmd_gen_solve_invar_dual_selected_or_all_props(NuSMVEnv_ptr const env,
Prop_ptr const invarprop,
const int max_k,
bmc_invar_closure_strategy closure_strategy);

static inline int
bmc_cmd_gen_solve_invar_fals_selected_or_all_props(NuSMVEnv_ptr const env,
                                                   Prop_ptr const invarprop,
                                                   const int max_k, int step_k);

static inline int
bmc_cmd_gen_solve_invar_zigzag_selected_or_all_props(NuSMVEnv_ptr const env,
                                                     Prop_ptr const invarprop,
                                                     const int max_k);

static inline int
bmc_cmd_gen_solve_psl_selected_or_all_props(NuSMVEnv_ptr const env,
                                            const int prop_no,
                                            const boolean bmc_dump,
                                            const boolean inc_sat,
                                            const boolean single_bmc_prob,
                                            const int k,
                                            const int l);

/**AutomaticEnd***************************************************************/
/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Bmc_AddCmd(NuSMVEnv_ptr env)
{
  Cmd_CommandAdd(env, "bmc_setup", Bmc_CommandBmcSetup, 0, false);
  Cmd_CommandAdd(env, "bmc_simulate", Bmc_CommandBmcSimulate, 0, true);
  Cmd_CommandAdd(env, "bmc_inc_simulate", Bmc_CommandBmcIncSimulate, 0, true);
  Cmd_CommandAdd(env, "bmc_pick_state", Bmc_CommandBmcPickState, 0, true);
  Cmd_CommandAdd(env, "bmc_simulate_check_feasible_constraints",
                 Bmc_CommandBmcSimulateCheckFeasibleConstraints, 0, true);
  Cmd_CommandAdd(env, "gen_ltlspec_bmc",
                 Bmc_CommandGenLtlSpecBmc, 0, true);
  Cmd_CommandAdd(env, "gen_ltlspec_bmc_onepb",
                 Bmc_CommandGenLtlSpecBmcOnePb, 0, true);
  Cmd_CommandAdd(env, "check_ltlspec_bmc", Bmc_CommandCheckLtlSpecBmc, 0, true);
  Cmd_CommandAdd(env, "check_ltlspec_bmc_onepb",
                 Bmc_CommandCheckLtlSpecBmcOnePb, 0, true);

  Cmd_CommandAdd(env, "gen_invar_bmc",     Bmc_CommandGenInvarBmc, 0, true);
  Cmd_CommandAdd(env, "check_invar_bmc",   Bmc_CommandCheckInvarBmc, 0, true);
  Cmd_CommandAdd(env, "check_pslspec_bmc", bmc_CommandCheckPslSpecBmc, 0,
                 true);

#if NUSMV_HAVE_INCREMENTAL_SAT
  Cmd_CommandAdd(env, "check_ltlspec_bmc_inc", Bmc_CommandCheckLtlSpecBmcInc,
                 0, true);
  Cmd_CommandAdd(env, "check_invar_bmc_inc",   Bmc_CommandCheckInvarBmcInc,
                 0, true);
  Cmd_CommandAdd(env, "check_pslspec_bmc_inc", bmc_CommandCheckPslSpecBmcInc, 0,
                 true);
#endif

#if NUSMV_HAVE_BMC_PROFILER_LIBRARY
  Cmd_CommandAdd(env, "bmc_profile", Bmc_CommandProfile, 0, true);
#endif
#if NUSMV_HAVE_WATCHDOG_LIBRARY
  Cmd_CommandAdd(env, "bmc_watchdog", Bmc_CommandWatchdog, 0, true);
#endif

  Cmd_CommandAdd(env, "_bmc_test_tableau", Bmc_TestTableau, 0, true);

  SBmc_AddCmd(env);
}

void Bmc_Cmd_quit(NuSMVEnv_ptr env)
{
  boolean status = true;

  status = status && Cmd_CommandRemove(env, "bmc_setup");
  status = status && Cmd_CommandRemove(env, "bmc_simulate");
  status = status && Cmd_CommandRemove(env, "bmc_inc_simulate");
  status = status && Cmd_CommandRemove(env, "bmc_pick_state");
  status = status &&
    Cmd_CommandRemove(env, "bmc_simulate_check_feasible_constraints");
  status = status && Cmd_CommandRemove(env, "gen_ltlspec_bmc");
  status = status && Cmd_CommandRemove(env, "gen_ltlspec_bmc_onepb");
  status = status && Cmd_CommandRemove(env, "check_ltlspec_bmc");
  status = status && Cmd_CommandRemove(env, "check_ltlspec_bmc_onepb");
  status = status && Cmd_CommandRemove(env, "gen_invar_bmc");
  status = status && Cmd_CommandRemove(env, "check_invar_bmc");
  status = status && Cmd_CommandRemove(env, "check_pslspec_bmc");
  status = status && Cmd_CommandRemove(env, "_bmc_test_tableau");

#if NUSMV_HAVE_INCREMENTAL_SAT
  status = status && Cmd_CommandRemove(env, "check_ltlspec_bmc_inc");
  status = status && Cmd_CommandRemove(env, "check_invar_bmc_inc");
  status = status && Cmd_CommandRemove(env, "check_pslspec_bmc_inc");
#endif

  nusmv_assert(status);

  Sbmc_Cmd_quit(env);
}

Outcome Bmc_Cmd_compute_rel_loop(NuSMVEnv_ptr const env,
                                 int* const l, /* rel_loop */
                                 const char* str_loop,
                                 const int k)
{
  ErrorMgr_ptr const errmgr =
   ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (str_loop != (char*)NULL) {
    Outcome res;
    int rel_loop;

    rel_loop = Bmc_Utils_ConvertLoopFromString(str_loop, &res);

    if (res != OUTCOME_SUCCESS) {
      ErrorMgr_error_invalid_number(errmgr, str_loop);

      return OUTCOME_GENERIC_ERROR;
    }

    if (Bmc_Utils_Check_k_l(k, Bmc_Utils_RelLoop2AbsLoop(rel_loop, k))
        != OUTCOME_SUCCESS) {
      ErrorMgr_error_bmc_invalid_k_l(errmgr, k, rel_loop);

      return OUTCOME_GENERIC_ERROR;
    }

    *l = rel_loop;
  }
  else {
    /* A default value for loop */
    *l = Bmc_Utils_ConvertLoopFromString(get_bmc_pb_loop(opts), NULL);
  }

  return OUTCOME_SUCCESS;
}

int Bmc_CommandBmcSetup(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  /* processes the command options */
  int c;
  boolean forced = false;
  OptsHandler_ptr opts = NuSMVEnv_get_value(env, ENV_OPTS_HANDLER);

  util_getopt_reset();
  while ((c = util_getopt((int)argc, (char**)argv, "hf")) != EOF) {
    switch (c) {
    case 'h': return UsageBmcSetup(env);
    case 'f': forced = true; break;
    default: return UsageBmcSetup(env);
    }
  }

  if (Compile_check_if_bool_model_was_built(env, errstream, forced)) return 1;

  if (cmp_struct_get_bmc_setup(cmps) && !forced) {
    StreamMgr_print_error(streams,  "A call to bmc_setup has already been done.\n");
    return 1;
  }

  Bmc_Pkg_bmc_setup(env, forced);

  return 0;
}


/*!
  \brief Usage string for Bmc_CommandBmcSetup



  \sa Bmc_CommandBmcSetup
*/

static int UsageBmcSetup (const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: bmc_setup [-h][-f]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -f \t\tForces the BMC model to be built.\n");
  return 1;
}

int Bmc_CommandBmcSimulate(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  BddEnc_ptr bdd_enc;
  BeEnc_ptr be_enc;
  Be_Manager_ptr be_mgr;
  be_ptr be_constraints = (be_ptr) NULL;
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

  util_getopt_reset();
  while((c = util_getopt(argc,argv,"t:c:hpvrk:")) != EOF){
    switch(c){
    case 'h': return UsageBmcSimulate(env);
    case 'p':
      if (printrace == true) return UsageBmcSimulate(env);
      printrace = true;
      only_changes = true;
      break;
    case 'v':
      if (printrace == true) return UsageBmcSimulate(env);
      printrace = true;
      only_changes = false;
      break;
    case 'r':
      if (mode == Interactive) return UsageBmcSimulate(env);
      mode = Random;
      break;

      /* NOTE: Uncomment this if adding support for interactive
         mode. remember to add "ai" to the util_getopt format
         parameter */
    /* case 'i': */
    /*   if (mode == Random) return UsageBmcSimulate(env); */
    /*   mode = Interactive; */
    /*   break; */
    /* case 'a': */
    /*   display_all = 1; */
    /*   break; */
    case 'c':
      if (NIL(char) != strConstr) return UsageBmcSimulate(env);
      strConstr = util_strsav(util_optarg);
      isconstraint = true;
      time_shift = true;
      break;
    case 't':
      if (NIL(char) != strConstr) return UsageBmcSimulate(env);
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
      return UsageBmcSimulate(env);
    }
  }

  if ((mode != Interactive) && (display_all == 1)) return UsageBmcSimulate(env);

  if (argc != util_optind) {
    return UsageBmcSimulate(env);
  }

  /* pre-conditions */
  if (Bmc_check_if_model_was_built(env, errstream, true)) return 1;

  if (bmc_simulate_get_curr_sim_trace(env) == TRACE(NULL)) {
    StreamMgr_print_error(streams,
            "No current state set. Use the \"bmc_pick_state\" command.\n");
    return 1;
  }

  bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  be_enc = BE_ENC(NuSMVEnv_get_value(env, ENV_BE_ENCODER));
  be_mgr = BeEnc_get_be_manager(be_enc);

  if (isconstraint) {
    if (time_shift) {
      be_constraints = Bmc_Utils_simple_costraint_from_string(be_enc,
                                                              bdd_enc,
                                                              strConstr,
                                                              (Expr_ptr*)NULL);
    }
    else {
      be_constraints = Bmc_Utils_next_costraint_from_string(be_enc,
                                                            bdd_enc,
                                                            strConstr,
                                                            (Expr_ptr*)NULL);
    }

    FREE(strConstr);
  }
  else {
    be_constraints = Be_Truth(be_mgr);
  }

  (void)Bmc_Simulate(env, BE_FSM(NuSMVEnv_get_value(env, ENV_BE_FSM)),
                     bdd_enc, be_constraints, time_shift,
                     steps, printrace, only_changes, mode == Random);

  return 0;
}


/*!
  \brief Usage string for UsageBmcSimulate



  \se None

  \sa Bmc_CommandBmcSimulate
*/

static int UsageBmcSimulate (const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  /* StreamMgr_print_error(streams,  */
  /*         "usage: bmc_simulate [-h] [-p | -v] [-r | -i [-a]] [[-c \"constr\"] " */
  /*         "| [-t \"constr\"]] [-k steps]\n"); */
  StreamMgr_print_error(streams,
          "usage: bmc_simulate [-h] [-p | -v] [-r] [[-c \"constr\"] "
          "| [-t \"constr\"]] [-k steps]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -p \t\tPrints current generated trace (only changed variables).\n");
  StreamMgr_print_error(streams,  "  -v \t\tVerbosely prints current generated trace (all variables).\n");
  StreamMgr_print_error(streams,  "  -r \t\tSets picking mode to random (default is deterministic).\n");
  /* StreamMgr_print_error(streams,  "  -i \t\tEnters simulation's interactive mode.\n"); */
  /* StreamMgr_print_error(streams,  "  -a \t\tDisplays all the state variables (changed and unchanged)\n"); */
  /* StreamMgr_print_error(streams,  "     \t\tin every step of an interactive session.\n"); */
  /* StreamMgr_print_error(streams,  "     \t\tIt works only together with -i option.\n"); */
  StreamMgr_print_error(streams,  "  -c \"constr\"\tSets constraint (simple expression) for the next steps.\n");
  StreamMgr_print_error(streams,  "  -t \"constr\"\tSets constraint (next expression) for the next steps.\n");
  StreamMgr_print_error(streams,  "  -k <length> \tSpecifies the simulation length\n"
          "\t\tto be used when generating the simulated problem.\n");
  return 1;
}

int Bmc_CommandBmcIncSimulate(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  BddEnc_ptr bdd_enc;
  BeEnc_ptr be_enc;
  Be_Manager_ptr be_mgr;
  be_ptr be_constraints = (be_ptr) NULL;
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

  util_getopt_reset();

  while((c = util_getopt(argc,argv,"t:c:hpvrk:ia")) != EOF){
    switch(c){
    case 'h': return UsageBmcIncSimulate(env);
    case 'p':
      if (printrace == true) return UsageBmcIncSimulate(env);
      printrace = true;
      only_changes = true;
      break;
    case 'v':
      if (printrace == true) return UsageBmcIncSimulate(env);
      printrace = true;
      only_changes = false;
      break;
    case 'r':
      if (mode == Interactive) return UsageBmcIncSimulate(env);
      mode = Random;
      break;

    case 'i':
      if (mode == Random) return UsageBmcIncSimulate(env);
      mode = Interactive;
      break;
    case 'a':
      display_all = 1;
      break;
    case 'c':
      if (NIL(char) != strConstr) return UsageBmcIncSimulate(env);
      strConstr = util_strsav(util_optarg);
      isconstraint = true;
      time_shift = true;
      break;
    case 't':
      if (NIL(char) != strConstr) return UsageBmcIncSimulate(env);
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
      return UsageBmcIncSimulate(env);
    }
  }

  if ((mode != Interactive) && (display_all == 1)) return UsageBmcIncSimulate(env);

  if (argc != util_optind) {
    return UsageBmcIncSimulate(env);
  }

  /* pre-conditions */
  if (Bmc_check_if_model_was_built(env, errstream, true)) return 1;

  if (bmc_simulate_get_curr_sim_trace(env) == TRACE(NULL)) {
    StreamMgr_print_error(streams,
            "No current state set. Use the \"bmc_pick_state\" command.\n");
    return 1;
  }

  bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  be_enc = BE_ENC(NuSMVEnv_get_value(env, ENV_BE_ENCODER));
  be_mgr = BeEnc_get_be_manager(be_enc);

  if (isconstraint) {
    if (time_shift) {
      be_constraints = Bmc_Utils_simple_costraint_from_string(be_enc,
                                                              bdd_enc,
                                                              strConstr,
                                                              (Expr_ptr*)NULL);
    }
    else {
      be_constraints = Bmc_Utils_next_costraint_from_string(be_enc,
                                                            bdd_enc,
                                                            strConstr,
                                                            (Expr_ptr*)NULL);
    }

    FREE(strConstr);
  }
  else {
    be_constraints = Be_Truth(be_mgr);
  }

  (void)Bmc_StepWiseSimulation(env, BE_FSM(NuSMVEnv_get_value(env, ENV_BE_FSM)),
                               bdd_enc, TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                               steps, be_constraints, time_shift,
                               printrace, only_changes, mode, display_all);

  return 0;
}

/*!
  \brief Usage string for UsageBmcIncSimulate



  \se None

  \sa Bmc_CommandBmcIncSimulate
*/

static int UsageBmcIncSimulate (const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,
          "usage: bmc_inc_simulate [-h] [-p | -v] [-r | -i [-a]] [[-c \"constr\"] "
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

int Bmc_CommandBmcPickState(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  NodeMgr_ptr const nodemgr =
     NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  TraceMgr_ptr const gtm =
    TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
  ErrorMgr_ptr const errmgr =
   ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  FILE* errstream = StreamMgr_get_error_stream(streams);
  int retval = 0;
  int c = 0;
  char* str_constr = (char*) NULL;
  char* str_label = (char*) NULL;
  boolean verbose = false;
  int tr_number = -1;
  Simulation_Mode mode = Deterministic;
  short int usedMode = 0;
  int display_all = 0;
  TraceLabel label = NULL;
  be_ptr be_constr = NULL;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hc:s:vrai")) != EOF) {
    switch (c) {
    case 'h':
      retval = UsageBmcPickState(env);
      goto CommandBmcPickState_exit;

    case 'c': str_constr = util_strsav(util_optarg); break;
    case 's': str_label = util_strsav(util_optarg); break;
    case 'v': verbose = true; break;
    case 'r':
      if (++usedMode > 1) {
        retval = UsageBmcPickState(env);
        goto CommandBmcPickState_exit;
      }

      mode = Random;
      break;

    case 'i':
      if (++usedMode > 1) {
        retval = UsageBmcPickState(env);
        goto CommandBmcPickState_exit;
      }

      mode = Interactive;
      break;
    case 'a':
      display_all = 1;
      break;

    default:
      retval = UsageBmcPickState(env);
      goto CommandBmcPickState_exit;
    }
  }

  if ((mode != Interactive) && (display_all == 1)) {
    retval = UsageBmcPickState(env);
    goto CommandBmcPickState_exit;
  }

  if (argc != util_optind) {
    retval = UsageBmcPickState(env);
    goto CommandBmcPickState_exit;
  }

  /* conditions */
  if (Bmc_check_if_model_was_built(env, errstream, true)) {
    retval = 1;
    goto CommandBmcPickState_exit;
  }

  if (str_label != (char*) NULL) {
    if (str_constr != (char*) NULL) {
      StreamMgr_print_error(streams,
                            "Options -c and -s cannot be used at the same time\n");

      retval = UsageBmcPickState(env); goto CommandBmcPickState_exit;
    }

    label = TraceLabel_create_from_string(nodemgr, str_label);
    if (label == TRACE_LABEL_INVALID ||   \
        !TraceMgr_is_label_valid(gtm, label)) {

      StreamMgr_print_error(streams,  "Label \"%s\" is invalid\n", str_label);

      retval = 1; goto CommandBmcPickState_exit;
    }
  }
  else {
    BeEnc_ptr be_enc = BE_ENC(NuSMVEnv_get_value(env, ENV_BE_ENCODER));
    Be_Manager_ptr be_mgr = BeEnc_get_be_manager(be_enc);
    BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));

    if (str_constr != (char*) NULL) {
      CATCH(errmgr) {
        be_constr = Bmc_Utils_simple_costraint_from_string(be_enc, bdd_enc,
                                                           str_constr,
                                                           (Expr_ptr*) NULL);
      }
      FAIL(errmgr) {
        StreamMgr_print_error(streams,  "In constraint: %s\n", str_constr);
        StreamMgr_print_error(streams,
                              "Parsing error: expected a simple expression.\n");

        retval = 1; goto CommandBmcPickState_exit;
      }
    }
    else {
      be_constr = Be_Truth(be_mgr);
    }
  }

  retval = Bmc_Simulate_bmc_pick_state(env, label, be_constr, tr_number,
                                       mode, display_all, verbose);

CommandBmcPickState_exit:
  if ((char*) NULL != str_constr) FREE(str_constr);
  if ((char*) NULL != str_label) FREE(str_label);
  return retval;
}

/*!
  \brief Usage string for UsageBmcPickState



  \se None

  \sa CommandBmcPickState
*/
static int UsageBmcPickState(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: bmc_pick_state [-h] [-v] [-r | -i [-a]] [-c \"constr\" | -s trace.state]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -v \t\tVerbosely prints picked state.\n");
  StreamMgr_print_error(streams,  "  -r \t\tRandomly picks a state from the set of the initial states\n");
  StreamMgr_print_error(streams,  "     \t\t(otherwise choice is deterministic).\n");
  StreamMgr_print_error(streams,  "  -c \"constr\"   Sets constraints for the initial set of states.\n");
  StreamMgr_print_error(streams,  "  -s state\tPicks state from trace.state label.\n");
  StreamMgr_print_error(streams,  "  -i \t\tEnters simulation's interactive mode.\n");
  StreamMgr_print_error(streams,  "  -a \t\tDisplays all the state variables (changed and unchanged)\n");
  StreamMgr_print_error(streams,  "     \t\tin the interactive session.\n");
  StreamMgr_print_error(streams,  "     \t\tIt works only together with -i option.\n");
  return 1;
}

int Bmc_CommandBmcSimulateCheckFeasibleConstraints(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  Olist_ptr str_constraints = Olist_create();
  Olist_ptr expr_constraints = Olist_create();
  Olist_ptr be_constraints = Olist_create();

  boolean human_readable = true; /* -q not used */
  int c = 0;
  int retval = 0;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hc:q")) != EOF) {
    switch (c) {
    case 'h':
      retval = UsageBmcSimulateCheckFeasibleConstraints(env);
      goto BmcSimulateCheckFeasibleConstraints_exit;
      break;

    case 'c': {
      /* now stores the constraint, later they will be compiled and checked */
      char* str_constr = util_strsav(util_optarg);
      Olist_append(str_constraints, str_constr);
      break;
    }

    case 'q':
      human_readable = false;
      break;

    default:
      retval = UsageBmcSimulateCheckFeasibleConstraints(env);
      goto BmcSimulateCheckFeasibleConstraints_exit;
    }
  }

  /* pre-conditions */
  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    retval = 1;
    goto BmcSimulateCheckFeasibleConstraints_exit;
  }
  if (bmc_simulate_get_curr_sim_trace(env) == TRACE(NULL)) {
    StreamMgr_print_error(streams,  "A starting state has to be chosen. " \
            "Use command 'bmc_pick_state'\n");
    retval = 1;
    goto BmcSimulateCheckFeasibleConstraints_exit;
  }

  retval =
    Bmc_Simulate_bmc_simulate_check_feasible_constraints(env, str_constraints,
                                                    be_constraints,
                                                    expr_constraints,
                                                    human_readable);

 BmcSimulateCheckFeasibleConstraints_exit:
  { /* frees any pending string in the constraints list */
    Oiter iter;
    OLIST_FOREACH(str_constraints, iter) {
      char* str_constr = (char*) Oiter_element(iter);
      FREE(str_constr);
    }
    Olist_destroy(str_constraints);
  }
  Olist_destroy(be_constraints);
  Olist_destroy(expr_constraints);

    return retval;
}

/*!
  \brief Usage string for
                      UsageBmcSimulateCheckFeasibleConstraints



  \se None

  \sa Bmc_CommandSimulateCheckFeasibleConstraints
*/
static int UsageBmcSimulateCheckFeasibleConstraints(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,
          "usage: bmc_simulate_check_feasible_constraints [-h] [-q] [-c \"next_expr\"] [-c ...]\n");
  StreamMgr_print_error(streams,
          "  -h \t\t Prints the command usage.\n");
  StreamMgr_print_error(streams,
          "  -q \t\t Prints the output in compact form.\n");
  StreamMgr_print_error(streams,
          "  -c next_expr\t Specify one constraint whose feasability has to be checked\n"\
          "\t\t (can be used multiple times, order is important to read the result)\n");
  return 1;
}

int Bmc_CommandGenLtlSpecBmc(NuSMVEnv_ptr env, int argc, char** argv)
{
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  Prop_ptr ltlprop = PROP(NULL);   /* The property being processed */
  Outcome opt_handling_res;
  int k = get_bmc_pb_length(opts);
  char* fname = (char*) NULL;
  int relative_loop = Bmc_Utils_ConvertLoopFromString(get_bmc_pb_loop(opts),
                                                      NULL);
  int retval = 0;

  /* ----------------------------------------------------------------------- */
  /* Options handling: */
  opt_handling_res = Bmc_cmd_options_handling(env, argc, argv,
                                              Prop_Ltl, &ltlprop,
                                              &k, &relative_loop,
                                              NULL, NULL, &fname, NULL,
                                              NULL);

  if (opt_handling_res == OUTCOME_SUCCESS_REQUIRED_HELP) {
    if (fname != (char*) NULL) FREE(fname);
    return UsageBmcGenLtlSpec(env);
  }

  if (opt_handling_res != OUTCOME_SUCCESS) {
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }

  /* makes sure bmc has been set up */
  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }
  /* ----------------------------------------------------------------------- */

  if (fname == (char*) NULL) {
    fname = util_strsav(get_bmc_dimacs_filename(opts));
  }

  retval =
    bmc_cmd_gen_solve_ltl_selected_or_all_props(env, ltlprop, k, relative_loop,
                                           fname, true, false);

  FREE(fname);

  return retval;
}

/*!
  \brief Usage string for command gen_ltlspec_bmc



  \se None

  \sa Bmc_CommandGenLtlSpecBmc
*/
static int UsageBmcGenLtlSpec(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "\nusage: gen_ltlspec_bmc [-h | -n idx | -p \"formula\" | -P \"name\"] [-k max_length] [-l loopback]\n\t\t\t [-o filename]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -n idx\tChecks the LTL property specified with <idx>.\n");
  StreamMgr_print_error(streams,  "  -P \"name\"\tChecks the LTL property specified with <name>.\n");
  StreamMgr_print_error(streams,  "  -p \"formula\"\tChecks the specified LTL property.\n");
  StreamMgr_print_error(streams,  "\t\tIf no property is specified, checks all LTL properties.\n");
  StreamMgr_print_error(streams,  "  -k max_length\tChecks the property using <max_length> value instead of using the\n\t\tvariable <bmc_length> value.\n");
  StreamMgr_print_error(streams,  "  -l loopback\tChecks the property using <loopback> value instead of using the\n\t\tvariable <bmc_loopback> value.\n");
  StreamMgr_print_error(streams,  "  -o filename\tUses <filename> as dimacs file instead of using the\n\t\t\"bmc_dimacs_filename\" variable. <filename> may contain patterns.\n\n");

  return 1;
}

int Bmc_CommandGenLtlSpecBmcOnePb(NuSMVEnv_ptr env, int argc, char** argv)
{
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  Prop_ptr ltlprop = PROP(NULL);   /* The property being processed */
  Outcome opt_handling_res;
  int k = get_bmc_pb_length(opts);
  char* fname = (char*) NULL;
  int relative_loop =
    Bmc_Utils_ConvertLoopFromString(get_bmc_pb_loop(opts), NULL);

  /* ----------------------------------------------------------------------- */
  /* Options handling: */
  opt_handling_res = Bmc_cmd_options_handling(env, argc, argv,
                                              Prop_Ltl, &ltlprop,
                                              &k, &relative_loop,
                                              NULL, NULL, &fname, NULL,
                                              NULL);

  if (opt_handling_res == OUTCOME_SUCCESS_REQUIRED_HELP) {
    if (fname != (char*) NULL) FREE(fname);
    return UsageBmcGenLtlSpecOnePb(env);
  }

  if (opt_handling_res != OUTCOME_SUCCESS) {
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }

  /* makes sure bmc has been set up */
  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }

  /* ----------------------------------------------------------------------- */

  if (fname == (char*) NULL) {
    fname = util_strsav(get_bmc_dimacs_filename(opts));
  }

  /* WARNING [MD] ignored error value */
  (void)bmc_cmd_gen_solve_ltl_selected_or_all_props(env, ltlprop, k, relative_loop,
                                            fname, false, false);

  FREE(fname);
  return 0;
}

/*!
  \brief Usage string for command gen_ltlspec_bmc_onepb



  \se None

  \sa Bmc_CommandGenLtlSpecBmcOnePb
*/
static int UsageBmcGenLtlSpecOnePb(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "\nusage: gen_ltlspec_bmc_onepb [-h | -n idx | -p \"formula\" | -P \"name\"] [-k length] [-l loopback]\n\t\t\t [-o filename]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -n idx\tChecks the LTL property specified with <idx>.\n");
  StreamMgr_print_error(streams,  "  -P \"name\"\tChecks the LTL property specified with <name>.\n");
  StreamMgr_print_error(streams,  "  -p \"formula\"\tChecks the specified LTL property.\n");
  StreamMgr_print_error(streams,  "\t\tIf no property is specified, checks all LTL properties.\n");
  StreamMgr_print_error(streams,  "  -k length\tChecks the property using <length> value instead of using the\n\t\tvariable <bmc_length> value.\n");
  StreamMgr_print_error(streams,  "  -l loopback\tChecks the property using <loopback> value instead of using the\n\t\tvariable <bmc_loopback> value.\n");
  StreamMgr_print_error(streams,  "  -o filename\tUses <filename> as dimacs file instead of \"bmc_dimacs_filename\"\n\t\tvariable. <filename> may contain patterns.\n\n");

  return 1;
}

int Bmc_CommandCheckLtlSpecBmc(NuSMVEnv_ptr env, int argc, char** argv)
{
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  Prop_ptr ltlprop = PROP(NULL);   /* The property being processed */
  Outcome opt_handling_res;
  int k = get_bmc_pb_length(opts);
  char* fname = (char*) NULL;
  int relative_loop =
    Bmc_Utils_ConvertLoopFromString(get_bmc_pb_loop(opts), NULL);

  /* ----------------------------------------------------------------------- */
  /* Options handling: */
  opt_handling_res = Bmc_cmd_options_handling(env, argc, argv,
                                              Prop_Ltl, &ltlprop,
                                              &k, &relative_loop,
                                              NULL, NULL, &fname, NULL,
                                              NULL);

  if (opt_handling_res == OUTCOME_SUCCESS_REQUIRED_HELP) {
    if (fname != (char*) NULL) FREE(fname);
    return UsageBmcCheckLtlSpec(env);
  }

  if (opt_handling_res != OUTCOME_SUCCESS) {
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }

  /* makes sure bmc has been set up */
  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }
  /* ----------------------------------------------------------------------- */

  /* WARNING [MD] ignored error value */
  (void)bmc_cmd_gen_solve_ltl_selected_or_all_props(env, ltlprop, k, relative_loop,
                                            fname, true, true);

  FREE(fname);
  return 0;
}

/*!
  \brief Usage string for command check_ltlspec_bmc



  \se None

  \sa Bmc_CommandCheckLtlSpecBmc
*/
static int UsageBmcCheckLtlSpec(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "\nusage: check_ltlspec_bmc [-h | -n idx | -p \"formula\" | -P \"name\"] [-k max_length] [-l loopback]\n\t\t\t [-o <filename>]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -n idx\tChecks the LTL property specified with <idx>.\n");
  StreamMgr_print_error(streams,  "  -P \"name\"\tChecks the LTL property specified with <name>.\n");
  StreamMgr_print_error(streams,  "  -p \"formula\"\tChecks the specified LTL property.\n");
  StreamMgr_print_error(streams,  "\t\tIf no property is specified, checks all LTL properties.\n");
  StreamMgr_print_error(streams,  "  -k max_length\tChecks the property using <max_length> value instead of using the\n\t\tvariable <bmc_length> value.\n");
  StreamMgr_print_error(streams,  "  -l loopback\tChecks the property using <loopback> value instead of using the\n\t\tvariable <bmc_loopback> value.\n");
  StreamMgr_print_error(streams,  "  -o filename\tGenerates dimacs output file too. <filename> may contain patterns.\n\n");

  return 1;
}

int Bmc_CommandCheckLtlSpecBmcOnePb(NuSMVEnv_ptr env, int argc, char** argv)
{
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
const StreamMgr_ptr streams =
  STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  Prop_ptr ltlprop = PROP(NULL);   /* The property being processed */
  Outcome opt_handling_res;
  int k = get_bmc_pb_length(opts);
  char* fname = (char*) NULL;
  int relative_loop =
    Bmc_Utils_ConvertLoopFromString(get_bmc_pb_loop(opts), NULL);
  int retval = 0;

  /* ----------------------------------------------------------------------- */
  /* Options handling: */
  opt_handling_res = Bmc_cmd_options_handling(env, argc, argv,
                                              Prop_Ltl, &ltlprop,
                                              &k, &relative_loop,
                                              NULL, NULL, &fname, NULL,
                                              NULL);

  if (opt_handling_res == OUTCOME_SUCCESS_REQUIRED_HELP) {
    if (fname != (char*) NULL) FREE(fname);
    return UsageBmcCheckLtlSpecOnePb(env);
  }

  if (opt_handling_res != OUTCOME_SUCCESS) {
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }

  /* makes sure bmc has been set up */
  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }

  /* ----------------------------------------------------------------------- */

  retval =
    bmc_cmd_gen_solve_ltl_selected_or_all_props(env, ltlprop, k, relative_loop,
                                            fname, false, true);

  FREE(fname);
  return retval;
}

/*!
  \brief Usage string for command check_ltlspec_bmc_onepb



  \se None

  \sa Bmc_CommandCheckLtlSpecBmcOnePb
*/
static int UsageBmcCheckLtlSpecOnePb(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "\nusage: check_ltlspec_bmc_onepb [-h | -n idx | -p \"formula\" | -P \"name\"] [-k length] [-l loopback]\n\t\t\t [-o filename]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -n idx\tChecks the LTL property specified with <idx>.\n");
  StreamMgr_print_error(streams,  "  -p \"formula\"\tChecks the specified LTL property.\n");
  StreamMgr_print_error(streams,  "  -P \"name\"\tChecks the LTL property specified with <name>.\n");
  StreamMgr_print_error(streams,  "\t\tIf no property is specified, checks all LTL properties.\n");
  StreamMgr_print_error(streams,  "  -k length\tChecks the property using <length> value instead of using the\n\t\tvariable <bmc_length> value.\n");
  StreamMgr_print_error(streams,  "  -l loopback\tChecks the property using <loopback> value instead of using the\n\t\tvariable <bmc_loopback> value.\n");
  StreamMgr_print_error(streams,  "  -o filename\tGenerates dimacs output file too. <filename> may contain patterns.\n\n");

  return 1;
}

#if NUSMV_HAVE_INCREMENTAL_SAT

int Bmc_CommandCheckLtlSpecBmcInc(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  Prop_ptr ltlprop = PROP(NULL);   /* The property being processed */
  Outcome opt_handling_res;
  int k = get_bmc_pb_length(opts);
  int relative_loop =
    Bmc_Utils_ConvertLoopFromString(get_bmc_pb_loop(opts), NULL);
  int retval;

  /* ----------------------------------------------------------------------- */
  /* Options handling: */
  opt_handling_res = Bmc_cmd_options_handling(env, argc, argv,
                                              Prop_Ltl, &ltlprop,
                                              &k, &relative_loop,
                                              NULL, NULL, NULL, NULL,
                                              NULL);

  if (opt_handling_res == OUTCOME_SUCCESS_REQUIRED_HELP) {
    return UsageBmcCheckLtlSpecInc(env);
  }
  if (opt_handling_res != OUTCOME_SUCCESS)  return 1;

  /* makes sure bmc has been set up */
  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    return 1;
  }

  /* ----------------------------------------------------------------------- */
  retval =
    bmc_cmd_gen_solve_ltl_inc_selected_or_all_props(env, ltlprop, k,
                                                    relative_loop, true);

    return retval;
}
#endif

#if NUSMV_HAVE_INCREMENTAL_SAT

/*!
  \brief Usage string for command check_ltlspec_bmc_inc

  The function is compiled only if there is at least
  one incremental SAT solver.

  \se None

  \sa Bmc_CommandCheckLtlSpecBmc
*/
static int UsageBmcCheckLtlSpecInc(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "\nusage: check_ltlspec_bmc_inc [-h | -n idx | -p \"formula\" | -P \"name\"] [-k max_length] [-l loopback]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -n idx\tChecks the LTL property specified with <idx> \n"
                        "        \t(using incremental algorithms).\n");
  StreamMgr_print_error(streams,  "  -P \"name\"\tChecks the LTL property specified with <name> \n"
                        "        \t(using incremental algorithms).\n");
  StreamMgr_print_error(streams,  "  -p \"formula\"\tChecks the specified LTL property (using incremental algorithms).\n");
  StreamMgr_print_error(streams,  "\t\tIf no property is specified, checks all LTL properties (using \n"
                        "\t\tincremental algorithms).\n");
  StreamMgr_print_error(streams,  "  -k max_length\tChecks the property using <max_length> value instead of using \n\t\tthe variable <bmc_length> value.\n");
  StreamMgr_print_error(streams,  "  -l loopback\tChecks the property using <loopback> value instead of using the\n\t\tvariable <bmc_loopback> value.\n\n");
  return 1;
}
#endif

int Bmc_CommandGenInvarBmc(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  Prop_ptr invarprop = PROP(NULL);   /* The property being processed */
  Outcome opt_handling_res;
  char* fname = (char*) NULL;
  char* algorithm_name = (char*) NULL;
  int retval;

  /* ----------------------------------------------------------------------- */
  /* Options handling: */
  opt_handling_res = Bmc_cmd_options_handling(env, argc, argv,
                                              Prop_Invar, &invarprop,
                                              NULL, NULL,
                                              &algorithm_name, NULL,
                                              &fname, NULL,
                                              NULL);

  if (opt_handling_res == OUTCOME_SUCCESS_REQUIRED_HELP) {
    if (algorithm_name != (char*) NULL) FREE(algorithm_name);
    if (fname != (char*) NULL) FREE(fname);
    return UsageBmcGenInvar(env);
  }

  if (opt_handling_res != OUTCOME_SUCCESS) {
    if (algorithm_name != (char*) NULL) FREE(algorithm_name);
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }

  /* make sure bmc has been set up */
  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    if (algorithm_name != (char*) NULL) FREE(algorithm_name);
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }
  /* ----------------------------------------------------------------------- */

  if (fname == (char*) NULL) {
    fname = util_strsav(get_bmc_invar_dimacs_filename(opts));
  }

  /* Checks algorithms: */
  if (algorithm_name == (char*) NULL) {
    algorithm_name = util_strsav((char*) get_bmc_invar_alg(opts));
  }

  if (strcasecmp(algorithm_name, BMC_INVAR_ALG_CLASSIC) != 0) {
    StreamMgr_print_error(streams,
             "Generation of invariants are allowed only with "
             "'" BMC_INVAR_ALG_CLASSIC "'"
             " algorithm.\n");
    FREE(algorithm_name);
    FREE(fname);
    return 1;
  }
  /* ----------------------------------------------------------------------- */

  FREE(algorithm_name); algorithm_name = NULL;

  retval =
    bmc_cmd_gen_solve_invar_selected_or_all_props(env, invarprop, fname,
                                                  ! BMC_HAS_TO_SOLVE);

  FREE(fname); fname = NULL;

  return retval;
}

/*!
  \brief Usage string for command gen_invar_bmc



  \se None

  \sa Bmc_CommandGenInvarBmc
*/
static int UsageBmcGenInvar(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "\nusage: gen_invar_bmc [-h | -n idx | -p \"formula\" | -P \"name\"] [-o filename]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");

  StreamMgr_print_error(streams,  "  -n idx\tChecks the INVAR property specified with <idx>.\n");
  StreamMgr_print_error(streams,  "  -p \"formula\"\tChecks the specified INVAR propositional property.\n");
  StreamMgr_print_error(streams,  "  -P \"name\"\tChecks the INVAR property specified with <name>.\n");
  StreamMgr_print_error(streams,  "\t\tIf no property is specified, checks all INVAR properties.\n");
  StreamMgr_print_error(streams,  "  -o filename\tUses <filename> as dimacs file instead of using the\n\t\t\"bmc_invar_dimacs_filename\" variable. <filename> may contain patterns.\n\n");

  return 1;
}

int Bmc_CommandCheckInvarBmc(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  Prop_ptr invarprop = PROP(NULL);   /* The property being processed */
  Outcome opt_handling_res;
  char* fname = (char*) NULL;
  char* algorithm_name = (char*) NULL;
  int max_k = -1;
  boolean use_classic_alg = true;
  int res = 0;
  boolean use_extra_step = false;


  /* ----------------------------------------------------------------------- */
  /* Options handling: */
  opt_handling_res = Bmc_cmd_options_handling(env, argc, argv,
                                              Prop_Invar, &invarprop,
                                              &max_k, NULL,
                                              &algorithm_name,
                                              NULL, &fname,
                                              &use_extra_step,
                                              NULL);

  if (opt_handling_res == OUTCOME_SUCCESS_REQUIRED_HELP) {
    if (algorithm_name != (char*) NULL) FREE(algorithm_name);
    if (fname != (char*) NULL) FREE(fname);
    return UsageBmcCheckInvar(env);
  }

  if (opt_handling_res != OUTCOME_SUCCESS) {
    if (algorithm_name != (char*) NULL) FREE(algorithm_name);
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }

  /* makes sure bmc has been set up */
  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    if (algorithm_name != (char*) NULL) FREE(algorithm_name);
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }
  /* ----------------------------------------------------------------------- */

  /* Checks algorithms: */
  if (algorithm_name == (char*) NULL) {
    algorithm_name = util_strsav((char*) get_bmc_invar_alg(opts));
  }

  if ((strcasecmp(algorithm_name, BMC_INVAR_ALG_CLASSIC) != 0) &&
      (strcasecmp(algorithm_name, BMC_INVAR_ALG_EEN_SORENSSON) != 0)) {
    StreamMgr_print_error(streams,
             "'%s' is an invalid algorithm name.\n"
             "Valid names are "
             "'" BMC_INVAR_ALG_CLASSIC "'"
             " and "
             "'" BMC_INVAR_ALG_EEN_SORENSSON "'.\n", algorithm_name);
    FREE(algorithm_name);
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }

  /* ----------------------------------------------------------------------- */

  /* choses the algorithm: */
  use_classic_alg = (strcasecmp(algorithm_name, BMC_INVAR_ALG_CLASSIC) == 0);
  FREE(algorithm_name);

  /* checks length: */
  if (use_classic_alg && max_k != -1) {
    StreamMgr_print_error(streams,
             "Option -k can be used only when '"
             BMC_INVAR_ALG_EEN_SORENSSON "' algorithm is selected.\n");
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }

  /* checks extra step: */
  if (use_classic_alg && use_extra_step) {
    StreamMgr_print_error(streams,
             "Option -e can be used only when '"
             BMC_INVAR_ALG_EEN_SORENSSON "' algorithm is selected.\n");
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }

  /* if not specified, selects length from bmc_pb_length */
  if (max_k == -1) {
    max_k = get_bmc_pb_length(opts);
  }

  if (use_classic_alg) {
    res =
      bmc_cmd_gen_solve_invar_selected_or_all_props(env, invarprop,
                                                    fname, BMC_HAS_TO_SOLVE);
  }
  else {
    res =
      bmc_cmd_gen_solve_invar_een_selected_or_all_props(env, invarprop, max_k,
                                                        fname, use_extra_step);
  }

  FREE(fname); fname = NULL;

  return res;
}

/*!
  \brief Usage string for command check_invar_bmc



  \se None

  \sa Bmc_CommandCheckInvarBmc
*/
static int UsageBmcCheckInvar(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "\nusage: check_invar_bmc [-h | -n idx | -p \"formula\" | -P \"name\"] [-k max_len] [-s] [-a alg] [-o filename]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -n idx\tChecks the INVAR property specified with <idx>.\n");
  StreamMgr_print_error(streams,  "  -p \"formula\"\tChecks the specified INVAR propositional property.\n");
  StreamMgr_print_error(streams,  "  -P name\tChecks the INVAR property specified with <name>.\n");
  StreamMgr_print_error(streams,  "\t\tIf no property is specified, checks all INVAR properties.\n");
  StreamMgr_print_error(streams,  "  -k max_len\tUpper bound for the search.\n"
          "\t\tUse only when " BMC_INVAR_ALG_EEN_SORENSSON " algorithm is selected.\n"
          "\t\tIf not specified, variable bmc_length is taken.\n");
  StreamMgr_print_error(streams,  "  -e \t\tPerforms an extra step for finding a proof\n"
          "\t\tCan be used only with the " BMC_INVAR_ALG_EEN_SORENSSON " algorithm\n");
  StreamMgr_print_error(streams,  "  -a alg\tUses the specified algorithm. \n");
  StreamMgr_print_error(streams,  "\t\tValid values are: "
          BMC_INVAR_ALG_CLASSIC ", " BMC_INVAR_ALG_EEN_SORENSSON
          "\n\t\tDefault value is taken from variable bmc_invar_alg.\n");
  StreamMgr_print_error(streams,  "  -o filename\tGenerates dimacs output file too. <filename> may contain patterns.\n\n");

  return 1;
}


#if NUSMV_HAVE_INCREMENTAL_SAT

int Bmc_CommandCheckInvarBmcInc(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  Prop_ptr invarprop = PROP(NULL);   /* The property being processed */
  Outcome opt_handling_res;
  char* algorithm_name = (char*) NULL;
  char* closure_strategy_name = (char*) NULL;

  bmc_invar_algorithm algorithm = ALG_UNDEFINED;
  bmc_invar_closure_strategy closure_strategy = BMC_INVAR_BACKWARD_CLOSURE;

  int res = 0;
  int max_k = get_bmc_pb_length(opts);
  int step_k = 1;


  /* ----------------------------------------------------------------------- */
  /* Options handling: */
  opt_handling_res = Bmc_cmd_options_handling(env, argc, argv,
                                              Prop_Invar, &invarprop,
                                              &max_k, NULL,
                                              &algorithm_name,
                                              &closure_strategy_name,
                                              NULL, NULL,
                                              &step_k);

  if (opt_handling_res == OUTCOME_SUCCESS_REQUIRED_HELP) {
    if (algorithm_name != (char*) NULL) FREE(algorithm_name);
    return UsageBmcCheckInvarInc(env);
  }

  if (opt_handling_res != OUTCOME_SUCCESS) {
    if (algorithm_name != (char*) NULL) FREE(algorithm_name);
    return 1;
  }

  /* makes sure bmc has been set up */
  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    if (algorithm_name != (char*) NULL) FREE(algorithm_name);
    return 1;
  }

  /* ----------------------------------------------------------------------- */

  /* Checks algorithms: */
  if (algorithm_name == (char*) NULL) {
    algorithm_name = util_strsav((char*) get_bmc_inc_invar_alg(opts));
  }

  if ((strcasecmp(algorithm_name, BMC_INC_INVAR_ALG_DUAL) != 0) &&
      (strcasecmp(algorithm_name, BMC_INC_INVAR_ALG_FALSIFICATION) != 0) &&
      (strcasecmp(algorithm_name, BMC_INC_INVAR_ALG_ZIGZAG) != 0)) {
    StreamMgr_print_error(streams,
             "'%s' is an invalid algorithm name.\n"
             "Valid names are "
             "'" BMC_INC_INVAR_ALG_DUAL "'"
             ", "
             "'" BMC_INVAR_ALG_FALSIFICATION "'"
             " and "
             "'" BMC_INC_INVAR_ALG_ZIGZAG "'.\n", algorithm_name);
    FREE(algorithm_name);
    return 1;
  }

  /* Checks closure strategy */
  if ((char *) NULL != closure_strategy_name) {

    if (strcasecmp(algorithm_name, BMC_INC_INVAR_ALG_DUAL) != 0) {
      StreamMgr_print_error(streams,
              "Closure strategy can be specified for "
              "'" BMC_INC_INVAR_ALG_DUAL "'"
              " only.\n");
      FREE(closure_strategy_name);
      return 1;
    }

    if ((strcasecmp(closure_strategy_name, BMC_INVAR_BACKWARD) != 0) &&
        (strcasecmp(closure_strategy_name, BMC_INVAR_FORWARD)) != 0) {
    StreamMgr_print_error(streams,
             "'%s' is an invalid closure strategy name.\n"
             "Valid names are "
             "'" BMC_INVAR_BACKWARD "'"
             " and "
             "'" BMC_INVAR_FORWARD "'.\n", closure_strategy_name);
    FREE(closure_strategy_name);
    return 1;
    }
  }

 /* ----------------------------------------------------------------------- */

 /* Algorithm selection */
  if (!strcasecmp(algorithm_name, BMC_INC_INVAR_ALG_DUAL)) {
    algorithm = ALG_DUAL;
  }
  else if (!strcasecmp(algorithm_name, BMC_INC_INVAR_ALG_FALSIFICATION)) {
    algorithm = ALG_FALSIFICATION;
  }
  else if (!strcasecmp(algorithm_name, BMC_INC_INVAR_ALG_ZIGZAG)) {
    algorithm = ALG_ZIGZAG;
  }
  else {
    ErrorMgr_internal_error(errmgr, "%s:%d:%s unexpected algorithm specified (%s)",
                   __FILE__, __LINE__, __func__, algorithm_name);
  }
  FREE(algorithm_name);

  /* Closure strategy selection */
  if ((char*) NULL != closure_strategy_name) {
    if (!strcasecmp(closure_strategy_name, BMC_INVAR_BACKWARD)) {
      closure_strategy = BMC_INVAR_BACKWARD_CLOSURE;
    }
    else if (!strcasecmp(closure_strategy_name, BMC_INVAR_FORWARD)) {
      closure_strategy = BMC_INVAR_FORWARD_CLOSURE;
    }
    else {
      ErrorMgr_internal_error(errmgr, "%s:%d:%s unexpected closure strategy specified (%s)",
                     __FILE__, __LINE__, __func__, closure_strategy_name);
    }
    FREE(closure_strategy_name);
  }

  switch (algorithm) {
  case ALG_DUAL:
    res =
      bmc_cmd_gen_solve_invar_dual_selected_or_all_props(env, invarprop, max_k,
                                                         closure_strategy);
    break;

  case ALG_FALSIFICATION:
    res =
      bmc_cmd_gen_solve_invar_fals_selected_or_all_props(env, invarprop, max_k,
                                                         step_k);
    break;

  case ALG_ZIGZAG:
    res =
      bmc_cmd_gen_solve_invar_zigzag_selected_or_all_props(env, invarprop,
                                                           max_k);
    break;

  default: error_unreachable_code();
  }

  return res;
}
#endif


#if NUSMV_HAVE_INCREMENTAL_SAT

/*!
  \brief Usage string for command check_invar_bmc_inc

  The function is compiled only if there is at least
  one incremental SAT solver

  \se None

  \sa Bmc_CommandCheckInvarBmcInc
*/
static int UsageBmcCheckInvarInc(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,
          "\nusage: check_invar_bmc_inc [-h | -n idx | -p \"formula\" | "
          "-P \"name\"] [-k max_len] [-K step_size] [-a alg] [-s strategy]\n");
  StreamMgr_print_error(streams,
          "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,
          "  -n idx\tChecks the INVAR property specified with <idx>"
          "\n\t\t(using incremental algorithms).\n");
  StreamMgr_print_error(streams,
          "  -P name\tChecks the INVAR property specified with <name>"
          "\n\t\t(using incremental algorithms).\n");
  StreamMgr_print_error(streams,
          "  -p \"formula\"\tChecks the specified INVAR propositional property"
          "\n\t\t(using incremental algorithms).\n");
  StreamMgr_print_error(streams,
          "\t\tIf no property is specified, checks all INVAR properties"
          "\n\t\t(using incremental algorithms).\n");

  StreamMgr_print_error(streams,
          "  -k max_len\tUpper bound for the search."
          "\n\t\tIf not specified, variable bmc_length is taken.\n");

  StreamMgr_print_error(streams,
          "  -K step_size\tOnly for falsification: increment the search of <step_size>"
          "\n\t\tunits at a time. Must be greater than zero (1 by default).\n");

  StreamMgr_print_error(streams,  "  -a alg\tUses the specified algorithm.");

  StreamMgr_print_error(streams,  "\n\t\tValid values are: "
          BMC_INC_INVAR_ALG_DUAL ", "
          BMC_INC_INVAR_ALG_ZIGZAG ", "
          BMC_INC_INVAR_ALG_FALSIFICATION
          "\n\t\tDefault value is taken from variable bmc_inc_invar_alg.\n");

  StreamMgr_print_error(streams,
          "  -s strategy\tUses the specified strategy for closure."
          "\n\t\t(currently this applies only to "
          BMC_INC_INVAR_ALG_DUAL ").\n");

  StreamMgr_print_error(streams,  "\t\tValid names are "
          "'" BMC_INVAR_BACKWARD "'" " and " "'" BMC_INVAR_FORWARD "'.\n");

  StreamMgr_print_error(streams,
          "\t\tDefault value is '" BMC_INVAR_BACKWARD "'.\n");

  return 1;
}
#endif

/*!
  \command{check_pslspec_bmc} Performs fair PSL model checking.

  \command_args{[-h] [-m | -o output-file] [-n number | -p "psl-expr [IN context]" | -P "name"]
  [-g] [-1] [-k bmc_length] [-l loopback]\}

  Performs fair PSL model checking using BMC.<p>

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

    <dt> <tt>-g</tt> While solving a problem, dumps it as a DIMACS
    file whose name depends on the content of the system variable
    "bmc_dimacs_filename". This feature is not allowed when the option
    -i is used as well.

    <dt> <tt>-1</tt> Generates and solves a single problem instead of
    iterating through 0 and bmc_length.

    <dt> <tt>-k <i>bmc_length</i></tt>
       <dd> <i>bmc_length</i> is the maximum problem bound must be reached if
       the option -1 is not specified. If -1 is specified, <i>bmc_length</i> is
       the exact length of the problem to be generated.
       Only natural number are valid values for this option. If no value
       is given the environment variable <i>bmc_length</i> is considered
       instead.
    <dt> <tt>-l <i>loopback</i></tt>
       <dd> <i>loopback</i> value may be: <BR>
       - a natural number in (0, <i>bmc_length-1</i>). Positive sign ('+') can
       be also used as prefix of the number. Any invalid combination of length
       and loopback will be skipped during the generation/solving process.<BR>
       - a negative number in (-1, -<i>bmc_length</i>). In this case
       <i>loopback</i> is considered a value relative to <i>bmc_length</i>.
       Any invalid combination of length and loopback will be skipped
       during the generation/solving process.<BR>
       - the symbol 'X', which means "no loopback" <BR>
       - the symbol '*', which means "all possible loopback from zero to
       <i>bmc_length-1</i>"

  </dl><p>


*/
static int bmc_CommandCheckPslSpecBmc(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  PropDb_ptr const prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  FILE* errstream = StreamMgr_get_error_stream(streams);
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* old_outstream = outstream;
  int c;

  int k = -1;
  int l = 0;
  char* str_loop = (char*) NULL;
  boolean l_specified = false;
  boolean bmc_dump = false;
  boolean single_bmc_prob = false;

  int prop_no = -1;
  char* formula = NIL(char);
  char* formula_name = NIL(char);
  int status = 0;
  boolean useMore = false;
  char* dbgFileName = NIL(char);

  /* It is possible to extend sbmc_cmd_options_handling in order to handle the
     extra options of this function g m */
  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "h1gmo:n:p:P:k:l:")) != EOF) {
    switch (c) {
    case 'h': {
      status = BMC_USAGE; goto label_clean_and_exit;
    }
    case 'n':
      {
        if (formula != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (prop_no != -1) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula_name != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }

        prop_no = PropDb_get_prop_index_from_string(prop_db,
                                                    util_optarg);
        if (prop_no == -1) {
          status = 1; goto label_clean_and_exit;
        }

        break;
      }
    case 'P':
      {
        if (formula != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (prop_no != -1) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula_name != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }

        formula_name = util_strsav(util_optarg);

        prop_no = PropDb_prop_parse_name(prop_db,
                                         formula_name);

        if (prop_no == -1) {
          StreamMgr_print_error(streams,  "No property named \"%s\"\n", formula_name);

          status = 1; goto label_clean_and_exit;
        }

        break;
      }

    case 'g':
      bmc_dump = true;
      break;

    case '1':
      single_bmc_prob = true;
      break;

    case 'k':
      {
        char* str_k;

        /* check if a value has already been specified: */
        if (k != -1) {
          StreamMgr_print_error(streams,
                                "Option -k cannot be specified more than once.\n");
          status = 1; goto label_clean_and_exit;
        }

        str_k = util_strsav(util_optarg);

        if ((util_str2int(str_k, &k) != 0) || k<0) {
          ErrorMgr_error_invalid_number(errmgr, str_k);
          FREE(str_k);
          status = 1; goto label_clean_and_exit;
        }

        FREE(str_k);
        break;
      }

    case 'l':
      /* check if a value has already been specified: */
      if (l_specified) {
        StreamMgr_print_error(streams,
                              "Option -l cannot be specified more than once.\n");
        status = 1; goto label_clean_and_exit;
      }

      str_loop = util_strsav(util_optarg);
      l_specified = true;
      /* checking of loopback value is delayed after command line
         processing to allow any -k option evaluation before (see
         the cheking code below) */
      break;

    case 'p':
      {
        if (prop_no != -1) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula_name != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }

        formula = util_strsav(util_optarg);
        break;
      }
    case 'o':
      if (useMore) {
        status = BMC_USAGE; goto label_clean_and_exit;
      }
      dbgFileName = util_strsav(util_optarg);
      StreamMgr_print_output(streams,  "Output to file: %s\n", dbgFileName);
      break;

    case 'm':
      if (dbgFileName != NIL(char)) {
        status = BMC_USAGE; goto label_clean_and_exit;
      }
      useMore = true;
      break;
    default:
      status = BMC_USAGE; goto label_clean_and_exit;
    }
  }
  if (argc != util_optind) {
    status = BMC_USAGE; goto label_clean_and_exit;
  }

  /* ---------------------------------------------------------------------- */
  if (k == -1) k = get_bmc_pb_length(opts); /* default for k */

  if (OUTCOME_SUCCESS != Bmc_Cmd_compute_rel_loop(env, &l, str_loop, k)) {
    status = 1; goto label_clean_and_exit;
  }

  /* Checking compilation status */
  if (Compile_check_if_encoding_was_built(env, errstream)) {
    status = 1; goto label_clean_and_exit;
  }

  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    status = 1; goto label_clean_and_exit;
  }

  if (formula != NIL(char)) {
    SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
    prop_no = PropDb_prop_parse_and_add(prop_db, st,
                                        formula, Prop_Psl, Nil);
    if (prop_no == -1) { status = 1; goto label_clean_and_exit; }
  }

  if (useMore || (char*)NULL != dbgFileName) {
    if (OUTCOME_SUCCESS !=
        Cmd_Misc_open_pipe_or_file(env, dbgFileName, &outstream)) {
      status = 1; goto label_clean_and_exit;
    }
  }

  if (0 == status) {
    status =
      bmc_cmd_gen_solve_psl_selected_or_all_props(env, prop_no, bmc_dump,
                                                  ! IS_INC_SAT,
                                                  single_bmc_prob,
                                                  k, l);
  }

 label_clean_and_exit:
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

  FREE(str_loop); str_loop = (char*)NULL;
  FREE(formula_name); formula_name = (char*)NULL;

  if (BMC_USAGE == status) return UsageCheckPslSpecBmc(env);
  else return status;
}

/*!
  \command{check_pslspec_bmc_inc} Performs fair PSL model checking using incremental BMC.

  \command_args{[-h] [-m | -o output-file] [-n number | -p "psl-expr [IN context]" | -P "name"]
  [-1] [-k bmc_length] [-l loopback]\}

  Performs fair PSL model checking using incremental BMC.<p>

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

    <dt> <tt>-1</tt> Generates and solves a single problem instead of
    iterating through 0 and bmc_length.

    <dt> <tt>-k <i>bmc_length</i></tt>
       <dd> <i>bmc_length</i> is the maximum problem bound must be reached if
       the option -1 is not specified. If -1 is specified, <i>bmc_length</i> is
       the exact length of the problem to be generated.
       Only natural number are valid values for this option. If no value
       is given the environment variable <i>bmc_length</i> is considered
       instead.
    <dt> <tt>-l <i>loopback</i></tt>
       <dd> <i>loopback</i> value may be: <BR>
       - a natural number in (0, <i>bmc_length-1</i>). Positive sign ('+') can
       be also used as prefix of the number. Any invalid combination of length
       and loopback will be skipped during the generation/solving process.<BR>
       - a negative number in (-1, -<i>bmc_length</i>). In this case
       <i>loopback</i> is considered a value relative to <i>bmc_length</i>.
       Any invalid combination of length and loopback will be skipped
       during the generation/solving process.<BR>
       - the symbol 'X', which means "no loopback" <BR>
       - the symbol '*', which means "all possible loopback from zero to
       <i>bmc_length-1</i>"

  </dl><p>


*/

#if NUSMV_HAVE_INCREMENTAL_SAT

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int bmc_CommandCheckPslSpecBmcInc(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  PropDb_ptr const prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  FILE* errstream = StreamMgr_get_error_stream(streams);
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* old_outstream = outstream;
  int c;

  int k = -1;
  int l = 0;
  char* str_loop = (char*) NULL;
  boolean l_specified = false;
  boolean single_bmc_prob = false;

  int prop_no = -1;
  char* formula = NIL(char);
  char* formula_name = NIL(char);
  int status = 0;
  boolean useMore = false;
  char* dbgFileName = NIL(char);

  /* It is possible to extend sbmc_cmd_options_handling in order to handle the
     extra options of this function g m */
  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "h1gmo:n:p:P:k:l:")) != EOF) {
    switch (c) {
    case 'h': {
      status = BMC_USAGE; goto label_clean_and_exit;
    }
    case 'n':
      {
        if (formula != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (prop_no != -1) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula_name != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }

        prop_no = PropDb_get_prop_index_from_string(prop_db,
                                                    util_optarg);
        if (prop_no == -1) {
          status = 1; goto label_clean_and_exit;
        }

        break;
      }
    case 'P':
      {
        if (formula != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (prop_no != -1) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula_name != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }

        formula_name = util_strsav(util_optarg);

        prop_no = PropDb_prop_parse_name(prop_db,
                                         formula_name);

        if (prop_no == -1) {
          StreamMgr_print_error(streams,  "No property named \"%s\"\n", formula_name);

          status = 1; goto label_clean_and_exit;
        }

        break;
      }

    case '1':
      single_bmc_prob = true;
      break;

    case 'k':
      {
        char* str_k;

        /* check if a value has already been specified: */
        if (k != -1) {
          StreamMgr_print_error(streams,
                                "Option -k cannot be specified more than once.\n");
          status = 1; goto label_clean_and_exit;
        }

        str_k = util_strsav(util_optarg);

        if ((util_str2int(str_k, &k) != 0) || k<0) {
          ErrorMgr_error_invalid_number(errmgr, str_k);
          FREE(str_k);
          status = 1; goto label_clean_and_exit;
        }

        FREE(str_k);
        break;
      }

    case 'l':
      /* check if a value has already been specified: */
      if (l_specified) {
        StreamMgr_print_error(streams,
                              "Option -l cannot be specified more than once.\n");
        status = 1; goto label_clean_and_exit;
      }

      str_loop = util_strsav(util_optarg);
      l_specified = true;
      /* checking of loopback value is delayed after command line
         processing to allow any -k option evaluation before (see
         the cheking code below) */
      break;

    case 'p':
      {
        if (prop_no != -1) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula_name != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }

        formula = util_strsav(util_optarg);
        break;
      }
    case 'o':
      if (useMore) {
        status = BMC_USAGE; goto label_clean_and_exit;
      }
      dbgFileName = util_strsav(util_optarg);
      StreamMgr_print_output(streams,  "Output to file: %s\n", dbgFileName);
      break;

    case 'm':
      if (dbgFileName != NIL(char)) {
        status = BMC_USAGE; goto label_clean_and_exit;
      }
      useMore = true;
      break;
    default:
      status = BMC_USAGE; goto label_clean_and_exit;
    }
  }
  if (argc != util_optind) {
    status = BMC_USAGE; goto label_clean_and_exit;
  }

  /* ---------------------------------------------------------------------- */
  if (k == -1) k = get_bmc_pb_length(opts); /* default for k */

  if (OUTCOME_SUCCESS != Bmc_Cmd_compute_rel_loop(env, &l, str_loop, k)) {
    status = 1; goto label_clean_and_exit;
  }

  /* Checking compilation status */
  if (Compile_check_if_encoding_was_built(env, errstream)) {
    status = 1; goto label_clean_and_exit;
  }

  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    status = 1; goto label_clean_and_exit;
  }

  if (formula != NIL(char)) {
    SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
    prop_no = PropDb_prop_parse_and_add(prop_db, st,
                                        formula, Prop_Psl, Nil);
    if (prop_no == -1) { status = 1; goto label_clean_and_exit; }
  }

  if (OUTCOME_SUCCESS != Cmd_Misc_open_pipe_or_file(env, dbgFileName, &outstream)) {
    status = 1; goto label_clean_and_exit;
  }

  if (0 == status) {
    status =
      bmc_cmd_gen_solve_psl_selected_or_all_props(env, prop_no, ! IS_BMC_DUMP,
                                                  IS_INC_SAT,
                                                  single_bmc_prob,
                                                  k, l);
  }

 label_clean_and_exit:
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

  FREE(str_loop); str_loop = (char*)NULL;
  FREE(formula_name); formula_name = (char*)NULL;

  if (BMC_USAGE == status) return UsageCheckPslSpecBmcInc(env);
  else return status;
}
#endif /* NUSMV_HAVE_INCREMENTAL_SAT */


#if NUSMV_HAVE_BMC_PROFILER_LIBRARY
int Bmc_CommandProfile(NuSMVEnv_ptr env, int argc, char ** argv)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  int c;
  boolean enable;
  boolean disable;
  char* out_file_name;

  enable = false;
  disable = false;
  out_file_name = (char*) NULL;

  util_getopt_reset();
  while ((c = util_getopt((int) argc, (char**) argv, "hde:")) != EOF) {
    switch (c) {
    case 'h': goto CommandProfileUsage;
    case 'd':
      if (enable) goto CommandProfileUsage;
      disable = true;
      break;
    case 'e':
      if (disable) goto CommandProfileUsage;
      enable = true;
      out_file_name = util_strsav(util_optarg);
      break;
    default: goto CommandProfileUsage;
    }
  }

  /* makes sure bmc has been set up */
  if (Bmc_check_if_model_was_built(env,
                                   StreamMgr_get_error_stream(streams),
                                   false)) {
    if (out_file_name != (char*) NULL) FREE(out_file_name);
    return 1;
  }

  nusmv_assert(! (disable && enable));

  if (enable) {
    if (BMC_PROFILER_IS_ENABLED(env)) {
      StreamMgr_print_error(streams, "The BMC profiler is already enabled.\n");
      FREE(out_file_name);
      return 1;
    }
    else {
      BMC_PROFILER_ENABLE(env, out_file_name);
      FREE(out_file_name);
    }
  }
  else if (disable) {
    if (! BMC_PROFILER_IS_ENABLED(env)) {
      StreamMgr_print_error(streams, "The BMC profiler is already disabled.\n");
      return 1;
    }
    else {
      BMC_PROFILER_DISABLE(env);
    }
  }

  /* Always print the library status to give a feedback to the user. */
  if (! BMC_PROFILER_IS_ENABLED(env)) {
    StreamMgr_print_output(streams, "The BMC profiler is disabled.\n");
  }
  else {
    nusmv_assert((char*) NULL != BMC_PROFILER_GET_OUT_FILE(env));
    StreamMgr_print_output(streams, "The BMC profiler is enabled.\n"    \
                           "The profiler output file is: %s\n",
                           BMC_PROFILER_GET_OUT_FILE(env));
  }
  return 0;

  /* usage */
 CommandProfileUsage:
  StreamMgr_print_error(streams,  "usage: bmc_profile [-h][-e <file> | -d]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -e <filename>\tEnable the profiling of BMC " \
                        "calls in <file>.\n");
  StreamMgr_print_error(streams,  "  -d \t\tDisable the profiling of BMC calls.\n");
  return 1;
}
#endif

#if NUSMV_HAVE_WATCHDOG_LIBRARY
int Bmc_CommandWatchdog(NuSMVEnv_ptr env, int argc, char ** argv)
{
  int c;
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env,
                                                        ENV_STREAM_MANAGER));
  long long int timeout;
  boolean enable, watchdog_opt_set;
  watchdogOption watchdog_option;

  util_getopt_reset();
  enable = false;
  watchdog_opt_set = false;
  timeout = -1;
  while ((c = util_getopt((int) argc, (char**) argv, "he:t:")) != EOF) {
    switch (c) {
    case 'h': goto CommandWatchdogUsage;
    case 'e':
      {
        char* str_number = (char*) NULL;
        char* err_occ[2];
        str_number = util_strsav(util_optarg);

        err_occ[0] = "";
        timeout = strtoll(str_number, err_occ, 10);
        if ((strcmp(err_occ[0], "") != 0)) {
          StreamMgr_print_error(streams,
                                "Error: \"%s\" is not a valid value "   \
                                "(must be integer).\n", str_number);
          goto CommandWatchdogUsage;
        }
        else if (timeout < 0) {
          StreamMgr_print_error(streams,
                                "Error: The timeout value (\"%s\") "\
                                "is negative!\n", str_number);
          FREE(str_number);
          goto CommandWatchdogUsage;
        }
        FREE(str_number);

        if (timeout < 0) {
          StreamMgr_print_error(streams,
                                "Error: \"%d\" should be non-negative!\n",
                                timeout);
          goto CommandWatchdogUsage;
        }
        enable = true;
      }
      break;
    case 't':
      {
        char* str_opt = util_optarg;
        if (0 == strcmp(str_opt, "periodic")) {
          watchdog_option = TIMER_PERIODIC;
        }
        else if (0 == strcmp(str_opt, "one_shot")) {
          watchdog_option = TIMER_ONE_SHOT;
        }
        else {
          goto CommandWatchdogUsage;
        }
        watchdog_opt_set = true;
      }
      break;
    default: goto CommandWatchdogUsage;
    }
  }

  if (enable) {
    if (! watchdog_opt_set) {
      StreamMgr_print_error(streams, "Error: the type of the watchdog was " \
                            "not set!\n");
      goto CommandWatchdogUsage;
    }

    /* to be set: action, options */
    WATCHDOG_INIT(env, BMC_WATCHDOG_NAME, timeout, watchdog_action,
                  watchdog_option);
  }

  /* print the watchdog status */
  {
    int status;
    unsigned long long int period;
    period = WATCHDOG_GET_STATUS(env, BMC_WATCHDOG_NAME, &status);

    if (period < 0) {
      StreamMgr_print_output(streams, "The watchdog is not set\n");
    }
    else {
      StreamMgr_print_output(streams, "The watchdog is %s and is set to "
                             "%llu.\n",
                             status ? "enabled" : "disabled",
                             period);
    }
  }

  return 0;

  /* usage */
CommandWatchdogUsage:
  StreamMgr_print_error(streams,  "usage: bmc_watchdog [-h] [-e <timeout> "\
                        "-t periodic|one_shot]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -e <timeout>\tEnable the watchdog on the " \
                        "execution of BMC algorithms with the specified " \
                        "timeout.\n");
  StreamMgr_print_error(streams,  "  -t periodic|one_shot\t"
                        "Option for the watchdog.\n");
  return 1;
}
#endif

int Bmc_TestTableau(NuSMVEnv_ptr env, int argc, char ** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  node_ptr wff=NULL;
  int k, l, max_depth, max_conns;
  boolean usePastOperators = false;
  boolean crossComparison = false;
  SymbTable_ptr symb_table = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  int res = 0;
  GenWffOperator wff_operator = GWO_None;

  max_depth = -1;  max_conns = 1; /* default values */

  k = get_bmc_pb_length(opts);

  l = Bmc_Utils_ConvertLoopFromString(get_bmc_pb_loop(opts), NULL);
  l = Bmc_Utils_RelLoop2AbsLoop(l, k);

  if (Bmc_Utils_IsAllLoopbacks(l)) {
    /* not implemented yet */
    StreamMgr_print_error(streams,  "Error: the case 'all loops' is not implemented yet.\nPlease set the variable 'bmc_loopback' to another value.\n\n");
    return 1;
  }

  if (cmp_struct_get_bmc_setup(cmps) == 0) {
    StreamMgr_print_error(streams,  "Please call bmc_setup before use this command.\n");
    return 1;
  }

  /* process command options */
  {
    int c;
    char* strNumber = NIL(char);
    char* szOperator= NIL(char);
    int prop_no;
    Prop_ptr ltlprop = PROP(NULL); /* The property being processed */
    node_ptr ltlspec;

    util_getopt_reset();
    while((c = util_getopt(argc, argv, "hn:d:c:po:x")) != EOF) {
      switch (c) {
      case 'n':
        {
          char* err_occ[2];
          strNumber = util_strsav(util_optarg);
          err_occ[0] = "";
          prop_no = strtol(strNumber, err_occ, 10);
          if ((strcmp(err_occ[0], "") != 0)) {
            StreamMgr_print_output(streams,
                    "Error: \"%s\" is not a valid value (must be integer).\n"
                    , strNumber);
            return 1;
          }
        }

        if (prop_no >= PropDb_get_size(PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB))) || prop_no < 0) {
          StreamMgr_print_output(streams,
                  "Error: \"%s\" is not a valid value, must be in the range [0,%d].\n",
                  strNumber, PropDb_get_size(PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB)))-1);
          return 1;
        }

        ltlprop = PropDb_get_prop_at_index(PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB)), prop_no);

        if (Prop_get_type(ltlprop) != Prop_Ltl) {
          StreamMgr_print_error(streams,
                  "Error: property %d is not of type LTL\n", prop_no);
          return 1;
        }

        /* here prop is ok */
        ltlspec = Prop_get_expr_core(ltlprop);
        ltlspec = Compile_FlattenSexpExpandDefine(symb_table, ltlspec, Nil);
        wff = Wff2Nnf(env, Compile_detexpr2bexpr(
            BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER)),
            ltlspec));
        break;


      case 'd': /* for max_depth */
        {
          char* err_occ[2];
          strNumber = util_strsav(util_optarg);

          err_occ[0] = "";
          max_depth = strtol(strNumber, err_occ, 10);
          if ((strcmp(err_occ[0], "") != 0)) {
            StreamMgr_print_output(streams,
                    "Error: \"%s\" is not a valid value (must be integer).\n",
                    strNumber);
            return 1;
          }
        }
        break;

      case 'c': /* for max_conns */
        {
          char* err_occ[2];
          strNumber = util_strsav(util_optarg);

          err_occ[0] = "";
          max_conns = strtol(strNumber, err_occ, 10);
          if ((strcmp(err_occ[0], "") != 0)) {
            StreamMgr_print_output(streams,
                    "Error: \"%s\" is not a valid value (must be integer).\n",
                    strNumber);
            return 1;
          }
        }
        break;

      case 'p': /* for past operators */
        usePastOperators = true;
        break;

      case 'x':
        crossComparison = true;
        break;


      case 'o': /* operator specification */
        szOperator = util_strsav(util_optarg);
        if (strcmp(szOperator, "G")==0) wff_operator = GWO_Globally;
        else if (strcmp(szOperator, "F")==0) wff_operator = GWO_Future;
        else if (strcmp(szOperator, "U")==0) wff_operator = GWO_Until;
        else if (strcmp(szOperator, "R")==0) wff_operator = GWO_Releases;
        else if (strcmp(szOperator, "H")==0) wff_operator = GWO_Historically;
        else if (strcmp(szOperator, "O")==0) wff_operator = GWO_Once;
        else if (strcmp(szOperator, "S")==0) wff_operator = GWO_Since;
        else if (strcmp(szOperator, "T")==0) wff_operator = GWO_Triggered;

        if(!usePastOperators && (wff_operator == GWO_Historically ||
                                 wff_operator == GWO_Once  ||
                                 wff_operator == GWO_Since ||
                                 wff_operator == GWO_Triggered ) ) {
          StreamMgr_print_output(streams,
                  "Error: operator \"%s\" is not allowed, unless you turn on the \"p\" option.\n",
                  szOperator);
          return 1;
        }

        if(wff_operator == GWO_None) {
          StreamMgr_print_output(streams,
                  "Error: operator \"%s\" is not valid. Use G|F|X|U|R|Y|G|H|S|T\n",
                  szOperator);
          return 1;
        }

        break;

      case 'h':
      default:
        return UsageBmcTestTableau(env);
      } /* switch */
    } /* while */

    if (argc>8) return UsageBmcTestTableau(env);
  }

  res = Bmc_Test_test_tableau(env, wff, wff_operator, max_depth, max_conns,
                              usePastOperators, crossComparison, k, l);

  return res;
}

/*!
  \brief Usage string for Bmc_TestTableau



  \se None
*/
static int UsageBmcTestTableau(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,
"usage: _bmc_test_tableau [-h] | [-n <property_index>] | \n \
                         [[ -d <max_depth>] [-c <max_conns>] [-o <operator>] [-p]]\n");
  StreamMgr_print_error(streams,  "  -h \t\t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -n <prop_idx> \tTest tableau of property represented by the given index\n");
  StreamMgr_print_error(streams,  "  -d <max_depth>\tGenerates a random wff with the given max depth (default value is -1)\n");
  StreamMgr_print_error(streams,  "  -c <max_conns>\tGenerates a random wff with the given max number of connectives\n\t\t\t(default value is 1).\n");
  StreamMgr_print_error(streams,  "  -p \t\t\tGenerate future and past operators\n\t\t\t(only future operators are generated by default).\n");
  StreamMgr_print_error(streams,  "  -o <operator> \tGenerates a random wff based on the specified operator, which will \n\t\t\tbe placed at top level. Valid values are G | F | U | R | H | O | S | T\n");
  StreamMgr_print_error(streams,  "  -x \t\t\tGenerate two smv files with the same set of random formulas (not tautologies)\n\t\t\t with and without loop condition, respectively.\n");
  return 1;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

Outcome
Bmc_cmd_options_handling(NuSMVEnv_ptr env,
                         int argc, char** argv,
                         Prop_Type prop_type,
                         /* output parameters: */
                         Prop_ptr* res_prop,
                         int* res_k,
                         int* res_l,
                         char** res_a,
                         char** res_s,
                         char** res_o,
                         boolean* res_e,
                         int *res_step_k)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  SymbTable_ptr symb_table = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
  PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  int c;
  int prop_idx;
  char* formula_name = (char*) NULL;
  char* str_formula = (char*) NULL;
  char* str_loop = (char*) NULL;

  boolean k_specified = false;
  boolean l_specified = false;
  boolean e_specified = false;
  boolean step_k_specified = false;

  /* If one or more options are added here, the size of this array
     must be changed. At the momemt eight options are supported.  */
  int opt_string_size = 10*2+1;
  char opt_string[opt_string_size];
  /* ossd :=> opt_string_size_debug */
  int ossd = 0;

  /* ---------------------------------------------------------------------- */
  /* Fills up the string to pass to util_getopt, depending on which
     options are actually required */
  ossd += 1;
  nusmv_assert(ossd < opt_string_size);
  strcpy(opt_string, "h");  /* h is always needed */

  if (res_prop != (Prop_ptr*) NULL) {
    *res_prop = (Prop_ptr) NULL;
    ossd += 6;
    nusmv_assert(ossd < opt_string_size);
    strcat(opt_string, "n:p:P:");
  }
  if (res_k != (int*) NULL) {
    ossd += 2;
    nusmv_assert(ossd < opt_string_size);
    strcat(opt_string, "k:");
  }
  if (res_l != (int*) NULL) {
    ossd += 2;
    nusmv_assert(ossd < opt_string_size);
    strcat(opt_string, "l:");
  }
  if (res_a != (char**) NULL) {
    *res_a = (char*) NULL;
    ossd += 2;
    nusmv_assert(ossd < opt_string_size);
    strcat(opt_string, "a:");
  }
  if (res_s != (char**) NULL) {
    *res_s = (char*) NULL;
    ossd += 2;
    nusmv_assert(ossd < opt_string_size);
    strcat(opt_string, "s:");
  }
  if (res_o != (char**) NULL) {
    *res_o = (char*) NULL;
    ossd += 2;
    nusmv_assert(ossd < opt_string_size);
    strcat(opt_string, "o:");
  }
  if (res_e != (boolean*)NULL) {
    *res_e = false;
    ossd += 1;
    nusmv_assert(ossd < opt_string_size);
    strcat(opt_string, "e");
  }
  if (res_step_k != (int*) NULL) {
    ossd += 2;
    nusmv_assert(ossd < opt_string_size);
    strcat(opt_string, "K:");
  }

  nusmv_assert(ossd < opt_string_size);

  util_getopt_reset();
  while ((c = util_getopt((int)argc, (char**) argv, opt_string)) != EOF) {
    switch (c) {

    case 'h':
      return OUTCOME_SUCCESS_REQUIRED_HELP;

    case 'e':
      nusmv_assert(res_e != (boolean*) NULL);

      /* check if a value has already been specified: */
      if (e_specified) {
        StreamMgr_print_error(streams,
                "Option -e cannot be specified more than once.\n");
        return OUTCOME_GENERIC_ERROR;
      }
      *res_e = true;

      e_specified = true;
      break;


    case 'n':
      {
        char* str_prop_idx = (char*) NULL;

        nusmv_assert(res_prop != (Prop_ptr*) NULL);

        /* check if a formula has already been specified: */
        if ((*res_prop != PROP(NULL)) || (str_formula != (char*) NULL) || (formula_name != (char*) NULL)) {
          ErrorMgr_error_property_already_specified(errmgr);
          return OUTCOME_GENERIC_ERROR;
        }

        str_prop_idx = util_strsav(util_optarg);

        /* check if property idx is ok */
        prop_idx = PropDb_get_prop_index_from_string(
                                    prop_db, str_prop_idx);
        FREE(str_prop_idx);

        if (prop_idx == -1) {
          /* error messages have already been shown */
          return OUTCOME_GENERIC_ERROR;
        }

        /* here property idx is ok */
        *res_prop = PropDb_get_prop_at_index(prop_db,
                                             prop_idx);
        if ( Prop_check_type(*res_prop, prop_type) != 0 ) {
          /* specified property's type is not what the caller expected */
          return OUTCOME_GENERIC_ERROR;
        }

        break;
     } /* case 'n' */

    case 'P':
      {
        nusmv_assert(res_prop != (Prop_ptr*) NULL);

        /* check if a formula has already been specified: */
        if ((*res_prop != PROP(NULL)) || (str_formula != (char*) NULL) || (formula_name != (char*) NULL)) {
          ErrorMgr_error_property_already_specified(errmgr);
          return OUTCOME_GENERIC_ERROR;
        }

        formula_name = util_strsav(util_optarg);

        prop_idx = PropDb_prop_parse_name(prop_db,
                                          formula_name);

        if (prop_idx == -1) {
          StreamMgr_print_error(streams,  "No property named \"%s\"\n", formula_name);
          FREE(formula_name);
          /* error messages have already been shown */
          return OUTCOME_GENERIC_ERROR;
        }

        FREE(formula_name);

        /* here property idx is ok */
        *res_prop = PropDb_get_prop_at_index(prop_db,
                                             prop_idx);
        if ( Prop_check_type(*res_prop, prop_type) != 0 ) {
          /* specified property's type is not what the caller expected */
          return OUTCOME_GENERIC_ERROR;
        }

        break;
      } /* case 'P' */

    case 'p':
      nusmv_assert(res_prop != (Prop_ptr*) NULL);

      /* check if a formula has already been specified: */
      if ((*res_prop != PROP(NULL)) || (str_formula != (char*) NULL) || (formula_name != (char*) NULL)) {
        ErrorMgr_error_property_already_specified(errmgr);
        return OUTCOME_GENERIC_ERROR;
      }

      str_formula = util_strsav(util_optarg);
      break;

    case 'k':
      {
        char* str_k;
        int k;

        nusmv_assert(res_k != (int*) NULL);

        /* check if a value has already been specified: */
        if (k_specified) {
          StreamMgr_print_error(streams,
                  "Option -k cannot be specified more than once.\n");
          return OUTCOME_GENERIC_ERROR;
        }

        str_k = util_strsav(util_optarg);

        if (util_str2int(str_k, &k) != 0) {
          ErrorMgr_error_invalid_number(errmgr, str_k);
          FREE(str_k);
          return OUTCOME_GENERIC_ERROR;
        }

        if (k < 0) {
          ErrorMgr_error_invalid_number(errmgr, str_k);
          FREE(str_k);
          return OUTCOME_GENERIC_ERROR;
        }

        FREE(str_k);
        *res_k = k;
        k_specified = true;
        break;
      }

    case 'l':
      nusmv_assert(res_l != (int*) NULL);

      /* check if a value has already been specified: */
      if (l_specified) {
        StreamMgr_print_error(streams,
                "Option -l cannot be specified more than once.\n");
        return OUTCOME_GENERIC_ERROR;
      }


      str_loop = util_strsav(util_optarg);
      l_specified = true;
      /* checking of loopback value is delayed after command line
         processing to allow any -k option evaluation before (see the
         cheking code below) */
      break;

    case 'a':
      nusmv_assert(res_a != (char**) NULL);

      if (*res_a != (char*) NULL) {
        StreamMgr_print_error(streams,  "Algorithm can be specified only once.\n\n");
        return OUTCOME_GENERIC_ERROR;
      }

      *res_a = util_strsav(util_optarg);
      break;

    case 's':
      nusmv_assert(res_s != (char**) NULL);

      if (*res_s != (char*) NULL) {
        StreamMgr_print_error(streams,
                "Closure strategy can be specified only once.\n\n");
        return OUTCOME_GENERIC_ERROR;
      }

      *res_s = util_strsav(util_optarg);
      break;

    case 'o':
      nusmv_assert(res_o != (char**) NULL);

      *res_o = util_strsav(util_optarg);
      break;

    case 'K':
      {
        char* str_k;
        int k;

        nusmv_assert(res_step_k != (int*) NULL);

        /* check if a value has already been specified: */
        if (step_k_specified) {
          StreamMgr_print_error(streams,
                  "Option -K cannot be specified more than once.\n");
          return OUTCOME_GENERIC_ERROR;
        }

        str_k = util_strsav(util_optarg);

        if (util_str2int(str_k, &k) != 0) {
          ErrorMgr_error_invalid_number(errmgr, str_k);
          FREE(str_k);
          return OUTCOME_GENERIC_ERROR;
        }

        if (k <= 0) {
          ErrorMgr_error_invalid_number(errmgr, str_k);
          FREE(str_k);
          return OUTCOME_GENERIC_ERROR;
        }

        FREE(str_k);
        *res_step_k = k;
        step_k_specified = true;
        break;
      }

    default:  return OUTCOME_GENERIC_ERROR;

    } /* switch case */
  } /* end of cmd line processing */

  /* checks if there are unexpected options: */
  if (argc != util_optind) {
    StreamMgr_print_error(streams,  "You specified one or more invalid options.\n\n");
    return OUTCOME_GENERIC_ERROR;
  }


  /* Checking of k,l constrains: */
  if (str_loop != (char*) NULL) {
    Outcome res;
    int rel_loop;

    rel_loop = Bmc_Utils_ConvertLoopFromString(str_loop, &res);

    if (res != OUTCOME_SUCCESS) {
      ErrorMgr_error_invalid_number(errmgr, str_loop);
      FREE(str_loop);
      return OUTCOME_GENERIC_ERROR;
    }
    FREE(str_loop);

    if (Bmc_Utils_Check_k_l(*res_k,
                            Bmc_Utils_RelLoop2AbsLoop(rel_loop, *res_k))
        != OUTCOME_SUCCESS) {
      ErrorMgr_error_bmc_invalid_k_l(errmgr, *res_k, rel_loop);
      return OUTCOME_GENERIC_ERROR;
    }

    *res_l = rel_loop;
  } /* k,l consistency check */


  /* Formula checking and commitment: */
  if (str_formula != (char*) NULL) {
    int idx;

    /* make sure bmc has been set up */
    if (Bmc_check_if_model_was_built(env, errstream, false)) {
      FREE(str_formula);
      return OUTCOME_GENERIC_ERROR;
    }

    idx = PropDb_prop_parse_and_add(prop_db,
                                    symb_table,
                                    str_formula, prop_type, Nil);
    if (idx == -1) {
      FREE(str_formula);
      return OUTCOME_GENERIC_ERROR;
    }

    /* index is ok */
    nusmv_assert(*res_prop == PROP(NULL));
    *res_prop = PropDb_get_prop_at_index(prop_db,
                                         idx);

    FREE(str_formula);
  } /* formula checking and commit */

  return OUTCOME_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Call Bmc_GenSolveLtl on a selected property or over all
  the properties

  Call Bmc_GenSolveLtl on a selected property or over all
  the properties
*/
static inline int bmc_cmd_gen_solve_ltl_selected_or_all_props(const NuSMVEnv_ptr env,
                                            const Prop_ptr ltlprop,
                                            const int k, const int relative_loop,
                                            const char* fname,
                                            const boolean has_to_iter_over_k,
                                            const boolean has_to_solve)
{
  const Bmc_DumpType dump_type =
    (fname != (char*)NULL) ? BMC_DUMP_DIMACS : BMC_DUMP_NONE;

  if (ltlprop == PROP(NULL)) {
    const PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
    lsList props;
    lsGen  iterator;
    Prop_ptr prop;

    props = PropDb_prepare_prop_list(prop_db, Prop_Ltl);

    lsForEachItem(props, iterator, prop) {
      if (Bmc_GenSolveLtl(env, prop, k, relative_loop,
                          has_to_iter_over_k,
                          has_to_solve,
                          dump_type, fname) != 0) {
        return 1;
      }
    }

    lsDestroy(props, NULL);
  }
  else {
    if (Bmc_GenSolveLtl(env, ltlprop, k, relative_loop,
                        has_to_iter_over_k,
                        has_to_solve,
                        dump_type, fname) != 0) {
      return 1;
    }
  }

  return 0;
}

/*!
  \brief Call Bmc_GenSolveLtlInc on a selected property or over all
  the properties

  Call Bmc_GenSolveLtlInc on a selected property or over all
  the properties
*/
static inline int bmc_cmd_gen_solve_ltl_inc_selected_or_all_props(const NuSMVEnv_ptr env,
                                                    const Prop_ptr ltlprop,
                                                    const int k,
                                                    const int relative_loop,
                                                    const boolean must_inc_length)
{
  if (ltlprop == PROP(NULL)) {
    PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
    lsList props;
    lsGen  iterator;
    Prop_ptr prop;

    props = PropDb_prepare_prop_list(prop_db, Prop_Ltl);

    lsForEachItem(props, iterator, prop) {
      if (Bmc_GenSolveLtlInc(env, prop, k, relative_loop, must_inc_length) != 0) {
        return 1;
      }
    }

    lsDestroy(props, NULL);
  }
  else {
    if (Bmc_GenSolveLtlInc(env, ltlprop, k, relative_loop, must_inc_length) != 0) {
      return 1;
    }
  }

  return 0;
}

/*!
  \brief Call Bmc_GenSolveInvar on a selected property or over all
  the properties

  Call Bmc_GenSolveInvar on a selected property or over all
  the properties
*/
static inline int bmc_cmd_gen_solve_invar_selected_or_all_props(const NuSMVEnv_ptr env,
                                                  const Prop_ptr invarprop,
                                                  const char* fname,
                                                  const boolean has_to_solve)
{
  const Bmc_DumpType dump_type =
    (fname != (char*)NULL) ? BMC_DUMP_DIMACS : BMC_DUMP_NONE;

  if (invarprop == PROP(NULL)) {
    PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
    lsList props;
    lsGen  iterator;
    Prop_ptr prop;

    props = PropDb_prepare_prop_list(prop_db, Prop_Invar);

    lsForEachItem(props, iterator, prop) {
      if (Bmc_GenSolveInvar(env, prop,
                            has_to_solve,
                            dump_type, fname) != 0) {

        return 1;
      }
    }

    lsDestroy(props, NULL);
  }
  else {
    if (Bmc_GenSolveInvar(env, invarprop,
                          has_to_solve,
                          dump_type, fname) != 0) {

      return 1;
    }
  }

  return 0;
}

/*!
  \brief Call Bmc_GenSolveInvar_EenSorensson on a selected
  property or over all the properties

  Call Bmc_GenSolveInvar_EenSorensson on a selected
  property or over all the properties
*/
static inline int bmc_cmd_gen_solve_invar_een_selected_or_all_props(const NuSMVEnv_ptr env,
                                                             const Prop_ptr invarprop,
                                                             const int max_k,
                                                             const char* fname,
                                                             const boolean use_extra_step)
{
  const Bmc_DumpType dump_type =
    (fname != (char*)NULL) ? BMC_DUMP_DIMACS : BMC_DUMP_NONE;
  int res = 0;

  if (invarprop == PROP(NULL)) {
    PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
    lsList props;
    lsGen  iterator;
    Prop_ptr prop;

    props = PropDb_prepare_prop_list(prop_db, Prop_Invar);

    lsForEachItem(props, iterator, prop) {
      res =
        Bmc_GenSolveInvar_EenSorensson(env, prop, max_k,
                                       dump_type, fname, use_extra_step);

      if (res != 0) {
        break;
      }
    }

    lsDestroy(props, NULL);
  }
  else {
    res = Bmc_GenSolveInvar_EenSorensson(env, invarprop, max_k,
                                         dump_type,
                                         fname, use_extra_step);
  }

  return res;
}

/*!
  \brief Call Bmc_GenSolveInvarDual on a selected property or over
  all the properties

  Call Bmc_GenSolveInvarDual on a selected property or over
  all the properties
*/
static inline int
bmc_cmd_gen_solve_invar_dual_selected_or_all_props(NuSMVEnv_ptr const env,
                                                   Prop_ptr const invarprop,
                                                   const int max_k,
                                                   bmc_invar_closure_strategy closure_strategy)
{
  int res = 0;

  if (invarprop == PROP(NULL)) {
    PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
    lsList props;
    lsGen  iterator;
    Prop_ptr prop;

    props = PropDb_prepare_prop_list(prop_db, Prop_Invar);

    lsForEachItem(props, iterator, prop) {
      res = Bmc_GenSolveInvarDual(env, prop, max_k, closure_strategy);

      if (res != 0) {
        break;
      }
    }

    lsDestroy(props, NULL);
  }
  else {
    res = Bmc_GenSolveInvarDual(env, invarprop, max_k, closure_strategy);
  }

  return res;
}

/*!
  \brief Call Bmc_GenSolveInvarFalsification on a selected
  property or over all the properties

  Call Bmc_GenSolveInvarFalsification on a selected
  property or over all the properties
*/
static inline int
bmc_cmd_gen_solve_invar_fals_selected_or_all_props(NuSMVEnv_ptr const env,
                                                   Prop_ptr const invarprop,
                                                   const int max_k, int step_k)
{
  int res = 0;

  if (invarprop == PROP(NULL)) {
    PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
    lsList props;
    lsGen  iterator;
    Prop_ptr prop;

    props = PropDb_prepare_prop_list(prop_db, Prop_Invar);

    lsForEachItem(props, iterator, prop) {
      res = Bmc_GenSolveInvarFalsification(env, prop, max_k, step_k);

      if (res != 0) {
        break;
      }
    }

    lsDestroy(props, NULL);
  }
  else {
    res = Bmc_GenSolveInvarFalsification(env, invarprop, max_k, step_k);
  }

  return res;
}

/*!
  \brief Call Bmc_GenSolveInvarZigzag on a selected property or
  over all the properties

  Call Bmc_GenSolveInvarZigzag on a selected property or
  over all the properties
*/
static inline int
bmc_cmd_gen_solve_invar_zigzag_selected_or_all_props(NuSMVEnv_ptr const env,
                                                     Prop_ptr const invarprop,
                                                     const int max_k)
{
  int res = 0;

  if (invarprop == PROP(NULL)) {
    PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
    lsList props;
    lsGen  iterator;
    Prop_ptr prop;

    props = PropDb_prepare_prop_list(prop_db, Prop_Invar);

    lsForEachItem(props, iterator, prop) {
      res = Bmc_GenSolveInvarZigzag(env, prop, max_k);

      if (res != 0) {
        break;
      }
    }

    lsDestroy(props, NULL);
  }
  else {
    res = Bmc_GenSolveInvarZigzag(env, invarprop, max_k);
  }

  return res;
}

/*!
  \brief Call the sbmc check function over the selected psl
  property or over all the psl properties

  Call the sbmc check function over the selected psl
  property or over all the psl properties. PSL properties not convertible to
  LTL will be ignored.
*/
static inline int
bmc_cmd_gen_solve_psl_selected_or_all_props(NuSMVEnv_ptr const env,
                                            const int prop_no,
                                            const boolean bmc_dump,
                                            const boolean inc_sat,
                                            const boolean single_bmc_prob,
                                            const int k,
                                            const int l)
{
  const PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  int status = 0;

  if (prop_no != -1) {
    if (Prop_check_type(PropDb_get_prop_at_index(prop_db,
                                                 prop_no),
                        Prop_Psl) != 0) {
      status = 1;
    }
    else {
      status =
        Bmc_Gen_check_psl_property(env,
                                   PropDb_get_prop_at_index(prop_db, prop_no),
                                   bmc_dump, inc_sat,
                                   single_bmc_prob, k, l);
    }
  }
  else {
    lsList props;
    lsGen  iterator;
    Prop_ptr prop;

    props = PropDb_prepare_prop_list(prop_db, Prop_Psl);

    lsForEachItem(props, iterator, prop) {
      if (Prop_is_psl_ltl(prop)) {
        status =
          Bmc_Gen_check_psl_property(env, prop, bmc_dump, inc_sat,
                                   single_bmc_prob, k, l);
      }

      if (0 != status) break;
    }

    lsDestroy(props, NULL);
  }

  return status;
}

/*!
  \brief Usage function

  the output of -h option
*/
static int UsageCheckPslSpecBmc(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  StreamMgr_print_error(streams, "usage: check_pslspec_bmc [-h] [-m | -o file] [-n number | -p \"psl-expr\" | -P \"name\"]\n");
  StreamMgr_print_error(streams, "                         [-g] [-1] \n");
  StreamMgr_print_error(streams, "                         [-k bmc_length] [-l loopback]\n");
  StreamMgr_print_error(streams, "   -h \t\t\tPrints the command usage.\n");
  StreamMgr_print_error(streams, "   -m \t\t\tPipes output through the program specified\n");
  StreamMgr_print_error(streams, "      \t\t\tby the \"PAGER\" environment variable if defined,\n");
  StreamMgr_print_error(streams, "      \t\t\telse through the UNIX command \"more\".\n");
  StreamMgr_print_error(streams, "   -o file\t\tWrites the generated output to \"file\".\n");
  StreamMgr_print_error(streams, "   -n number\t\tChecks only the PSLSPEC with the given index number.\n");
  StreamMgr_print_error(streams, "   -p \"psl-expr\"\tChecks only the given PSL formula.\n");
  StreamMgr_print_error(streams, "   -P \"name\"\t\tChecks only the PSLSPEC with the given name.\n");
  StreamMgr_print_error(streams, "   -g \t\t\tDumps generated problems in DIMACS format.\n");
  StreamMgr_print_error(streams, "   -1 \t\t\tGenerates and solves single problems.\n");
  StreamMgr_print_error(streams, "   -k bmc_length\tChecks the property using <bmc_length> value instead \n\t\t\tof using the variable <bmc_length> value.\n");
  StreamMgr_print_error(streams, "   -l loopback\t\tChecks the property using <loopback> value\n\t\t\tinstead of using the variable <bmc_loopback> value.\n");

  return 1;
}

/*!
  \brief Usage function

  the output of -h option
*/

#if NUSMV_HAVE_INCREMENTAL_SAT

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageCheckPslSpecBmcInc(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  StreamMgr_print_error(streams, "usage: check_pslspec_bmc_inc [-h] [-m | -o file] [-n number | -p \"psl-expr\" | -P \"name\"]\n");
  StreamMgr_print_error(streams, "                             [-1] \n");
  StreamMgr_print_error(streams, "                             [-k bmc_length] [-l loopback]\n");
  StreamMgr_print_error(streams, "   -h \t\t\tPrints the command usage.\n");
  StreamMgr_print_error(streams, "   -m \t\t\tPipes output through the program specified\n");
  StreamMgr_print_error(streams, "      \t\t\tby the \"PAGER\" environment variable if defined,\n");
  StreamMgr_print_error(streams, "      \t\t\telse through the UNIX command \"more\".\n");
  StreamMgr_print_error(streams, "   -o file\t\tWrites the generated output to \"file\".\n");
  StreamMgr_print_error(streams, "   -n number\t\tChecks only the PSLSPEC with the given index number.\n");
  StreamMgr_print_error(streams, "   -p \"psl-expr\"\tChecks only the given PSL formula.\n");
  StreamMgr_print_error(streams, "   -P \"name\"\t\tChecks only the PSLSPEC with the given name.\n");
  StreamMgr_print_error(streams, "   -1 \t\t\tGenerates and solves single problems.\n");
  StreamMgr_print_error(streams, "   -k bmc_length\tChecks the property using <bmc_length> value instead \n\t\t\tof using the variable <bmc_length> value.\n");
  StreamMgr_print_error(streams, "   -l loopback\t\tChecks the property using <loopback> value\n\t\t\tinstead of using the variable <bmc_loopback> value.\n");

  return 1;
}
#endif
