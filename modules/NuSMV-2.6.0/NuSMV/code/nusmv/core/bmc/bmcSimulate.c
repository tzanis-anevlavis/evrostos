/* ---------------------------------------------------------------------------



  This file is part of the ``bmc'' package of NuSMV version 2.
  Copyright (C) 2010 FBK-irst.

  NuSMV version 2 is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2 of
  the License, or (at your option) any later version.

  NuSMV version 2 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
  USA.

  For more information on NuSMV see <http://nusmv.fbk.eu>
  or email to <nusmv-users@fbk.eu>.
  Please report bugs to <nusmv-users@fbk.eu>.

  To contact the NuSMV development board, email to <nusmv@fbk.eu>.

-----------------------------------------------------------------------------*/

/*!
  \author Marco Roveri
  \brief Incremental SAT Based simulation

  Incremental SAT Based simulation

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/bmc/bmcModel.h"

#include "cudd/util.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/bmc/bmcInt.h"
#include "nusmv/core/bmc/bmcUtils.h"
#include "nusmv/core/bmc/bmcConv.h"
#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/simulate/simulate.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/sat/sat.h"
#include "nusmv/core/parser/parser.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/parser.h"
#include "nusmv/core/bmc/bmcSimulate.h"
#include "nusmv/core/utils/Olist.h"

#include "nusmv/core/prop/propPkg.h"
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BMC_PICK_TRACE_DESCRIPTION "BMC Simulation"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define READ_BUF_SIZE 8

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_BMC_SIM_TRACE "ebmcsimtr"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_BMC_SIM_TRACE_IDX "ebmcsimtridx"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_BMC_HAS_TRACE "ebmcht"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define DEF_SIM_TRACE_IDX_OFFSET 2

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
static void bmc_simulate_enable_random_mode(SatSolver_ptr solver);

static void bmc_simulate_print_state(const NuSMVEnv_ptr env,
                                     Trace_ptr trace, TraceIter step,
                                     int state_num, boolean show_inputs,
                                     hash_ptr shown_assignments);

static void bmc_simulate_trace_step_print(const NuSMVEnv_ptr env,
                                          const Trace_ptr trace,
                                          const TraceIter step,
                                          TraceIteratorType it_type,
                                          hash_ptr shown_assignments);

static Trace_ptr bmc_simulate_interactive_step(SatSolver_ptr solver,
                                               BeEnc_ptr be_enc,
                                               BddEnc_ptr bdd_enc,
                                               NodeList_ptr symbols,
                                               boolean in_simulation,
                                               boolean display_all);

static int bmc_simulate_ask_for_state(StreamMgr_ptr streams, int max_choice);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

int Bmc_Simulate(NuSMVEnv_ptr env,
                 const BeFsm_ptr be_fsm, BddEnc_ptr bdd_enc,
                 be_ptr be_constraints, boolean time_shift,
                 const int k, const boolean print_trace,
                 const boolean changes_only,
                 const Simulation_Mode mode)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const Be_CnfAlgorithm cnf_alg = get_rbc2cnf_algorithm(opts);

  be_ptr init, prob; /* The problem in BE format */
  SatSolver_ptr solver;
  SatSolverResult sat_res;
  BeEnc_ptr be_enc;
  Be_Manager_ptr be_mgr;
  Be_Cnf_ptr cnf;

  Trace_ptr trace = bmc_simulate_get_curr_sim_trace(env);
  int tr_num = bmc_simulate_get_curr_sim_trace_index(env);

  TRACE_CHECK_INSTANCE(trace); /* curr trace was picked */

  be_enc = BeFsm_get_be_encoding(be_fsm);
  be_mgr = BeEnc_get_be_manager(be_enc);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Simulation of length %d (no loopback)\n", k);
  }

  solver = Sat_CreateNonIncSolver(env, get_sat_solver(opts));
  if (solver == SAT_SOLVER(NULL)) {
    StreamMgr_print_error(streams,
            "Non-incremental sat solver '%s' is not available.\n",
            get_sat_solver(opts));
    return 1;
  }

  switch (mode) {
  case Random:
    bmc_simulate_enable_random_mode(SAT_SOLVER(solver));
    break;
  case Interactive:
    ErrorMgr_internal_error(errmgr, "%s: Interactive mode not supported yet", __func__);
    break;
  case Deterministic:
    /* Do nothing */
    break;
  default:
    ErrorMgr_internal_error(errmgr, "%s: Invalid mode given", __func__);
  }

  /* starting state taken from the last node of the current sim trace */
  init = BeEnc_untimed_expr_to_timed(be_enc,
               TraceUtils_fetch_as_be(trace, Trace_last_iter(trace),
                                      TRACE_ITER_SF_VARS, be_enc, bdd_enc), 0);

  prob = Be_And(be_mgr, Bmc_Model_GetPathNoInit(be_fsm, k), init);

  /* Use constraints only if actually necessary */
  if (!Be_IsTrue(be_mgr, be_constraints)) {
    int i;
    for (i = 0; i <= k; ++i) {
      be_ptr be_timed = be_constraints;

      if (time_shift) {
        be_timed = BeEnc_shift_curr_to_next(be_enc, be_timed);
      }

      be_timed = BeEnc_untimed_expr_to_timed(be_enc, be_timed, i);

      prob = Be_And(be_mgr, prob, be_timed);
    }
  }

  prob = Bmc_Utils_apply_inlining(be_mgr, prob);
  cnf = Be_ConvertToCnf(be_mgr, prob, 1, cnf_alg);

  SatSolver_add(solver, cnf, SatSolver_get_permanent_group(solver));
  SatSolver_set_polarity(solver, cnf, 1,
                         SatSolver_get_permanent_group(solver));
  sat_res = SatSolver_solve_all_groups(solver);

  /* Processes the result: */
  switch (sat_res) {

  case SAT_SOLVER_UNSATISFIABLE_PROBLEM:
    StreamMgr_print_output(streams,
            "The model deadlocks before requested length %d!\n", k);
    break;

  case SAT_SOLVER_SATISFIABLE_PROBLEM:
    {
      BoolSexpFsm_ptr bsexp_fsm; /* needed for trace language */
      Trace_ptr extension;
      bsexp_fsm = 
        BOOL_SEXP_FSM(NuSMVEnv_get_value(env, ENV_BOOL_FSM));

      BOOL_SEXP_FSM_CHECK_INSTANCE(bsexp_fsm);

      extension = \
        Bmc_create_trace_from_cnf_model(be_enc,
             SexpFsm_get_symbols_list(SEXP_FSM(bsexp_fsm)), NIL(char),
                                      TRACE_TYPE_UNSPECIFIED,
                                      SatSolver_get_model(solver), k);

      trace = Trace_concat(trace, &extension);
      nusmv_assert(TRACE(NULL) == extension);

      if (print_trace) {
        TraceMgr_execute_plugin(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), TRACE_OPT(NULL),
                                    (changes_only) ? 0 : 1, tr_num);
      }

      break;
    } /* SAT */

  case SAT_SOLVER_INTERNAL_ERROR:
    ErrorMgr_internal_error(errmgr, "Sorry, solver answered with a fatal Internal "
                   "Failure during problem solving.\n");

  case SAT_SOLVER_TIMEOUT:
  case SAT_SOLVER_MEMOUT:
    ErrorMgr_internal_error(errmgr, "Sorry, solver ran out of resources and aborted "
                   "the execution.\n");

  default:
    ErrorMgr_internal_error(errmgr, " Bmc_Simulate: Unexpected value in sat result");
  } /* switch */

  /* cleanup */
  SatSolver_destroy(solver);
  Be_Cnf_Delete(cnf);

  return 0;
}

int Bmc_StepWiseSimulation(NuSMVEnv_ptr env,
                           BeFsm_ptr be_fsm,
                           BddEnc_ptr bdd_enc,
                           TraceMgr_ptr trace_manager,
                           int target_steps,
                           be_ptr constraints,
                           boolean time_shift,
                           boolean print_trace,
                           boolean changes_only,
                           Simulation_Mode mode,
                           boolean display_all)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
#if NUSMV_HAVE_INCREMENTAL_SAT
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const Be_CnfAlgorithm cnf_alg = get_rbc2cnf_algorithm(opts);

  int steps;
  boolean no_deadlock;
  BeEnc_ptr be_enc = BE_ENC(NULL);
  Be_Manager_ptr be_mgr = (Be_Manager_ptr)NULL;
  SatIncSolver_ptr solver = SAT_INC_SOLVER(NULL);
  SatSolverGroup satGrp = (SatSolverGroup)-1;
  SatSolverResult satResult = SAT_SOLVER_UNAVAILABLE;
  Trace_ptr trace = bmc_simulate_get_curr_sim_trace(env);
  int tr_num = bmc_simulate_get_curr_sim_trace_index(env);
  be_ptr be_trans = (be_ptr)NULL;
  long time_statistics = util_cpu_time();

  TRACE_CHECK_INSTANCE(trace); /* a trace was picked */

  if (target_steps <= 0) return -1;

  /* Create the SAT solver instance */
  solver  = Sat_CreateIncSolver(env, get_sat_solver(opts));
  if (SAT_INC_SOLVER(NULL) == solver) {
    StreamMgr_print_error(streams,
            "Incremental sat solver '%s' is not available.\n",
            get_sat_solver(opts));
    return -1;
  }

  switch (mode) {
  case Random:
    bmc_simulate_enable_random_mode(SAT_SOLVER(solver));
    break;
  case Interactive:
    /* Do nothing */
    break;
  case Deterministic:
    /* Do nothing */
    break;
  default:
    ErrorMgr_internal_error(errmgr, "%s: Invalid mode given", __func__);
  }

  no_deadlock = true;

  be_enc = BeFsm_get_be_encoding(be_fsm);
  be_mgr = BeEnc_get_be_manager(be_enc);

  /* 1. Add the transition relation into the solver permanently */
  be_trans = BeFsm_get_invar(be_fsm);
  be_trans = Be_And(be_mgr,
                    be_trans,
                    BeEnc_shift_curr_to_next(be_enc, be_trans));
  be_trans = Be_And(be_mgr, BeFsm_get_trans(be_fsm), be_trans);

  /* We force the constraints that can be over starting states, or
     over next states, or over the input. If the constraint is over
     the current state variables, then it might be the case the chosen
     next state does not satisfy the constraint. */
  if (time_shift) {
    constraints = BeEnc_shift_curr_to_next(be_enc, constraints);
  }

  be_trans = Be_And(be_mgr, be_trans, constraints);

  /* Necessary to re-use the routines for extracting the model */
  be_trans = BeEnc_untimed_expr_to_timed(be_enc, be_trans, 0);
  Bmc_Utils_add_be_into_inc_solver_positively(
      SAT_INC_SOLVER(solver),
      SatSolver_get_permanent_group(SAT_SOLVER(solver)),
      be_trans, be_enc, cnf_alg);

  /* 2. Iteration adding last computed state as src state and
        extracting the pair (input,next_state), adding the pair to the
        computed trace, and then iterating from the so far computed
        next_state */
  {
    for (steps = 0; ((steps < target_steps) && no_deadlock) ; steps++) {
      be_ptr be_src_state = (be_ptr)NULL;

      if (opt_verbose_level_gt(opts, 1)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "Performing simulation step %d ...", steps+1);
      }

      /* 2.0. Extraction from the so far compute trace the last
         state. */
      be_src_state = BeEnc_untimed_expr_to_timed(be_enc,
                         TraceUtils_fetch_as_be(trace, Trace_last_iter(trace),
                                        TRACE_ITER_SF_VARS, be_enc, bdd_enc), 0);

      /* 2.2 Create the group to store the last state and, add the
           last state to the solver */
      satGrp = SatIncSolver_create_group(solver);
      Bmc_Utils_add_be_into_inc_solver_positively(
          SAT_INC_SOLVER(solver),
          satGrp, be_src_state, be_enc, cnf_alg);

      /* 2.3. Call the solver on the instantiated problem */
      satResult = SatSolver_solve_all_groups(SAT_SOLVER(solver));

      switch (satResult) {
      case SAT_SOLVER_SATISFIABLE_PROBLEM:
        if (Interactive == mode) {
          Trace_ptr iTrace =
            bmc_simulate_interactive_step(SAT_SOLVER(solver), be_enc,
                                          bdd_enc, Trace_get_symbols(trace),
                                          true, display_all);
          Trace_concat(trace, &iTrace);
        }
        else {
          /* Append current computed state to the trace */
          bmc_trace_utils_append_input_state(trace, be_enc,
                                   SatSolver_get_model(SAT_SOLVER(solver)));
        }

        break;

      case SAT_SOLVER_UNSATISFIABLE_PROBLEM:
        StreamMgr_print_error(streams,
            "The model reached a deadlock state: iteration %d.\n", steps);
        if (!Be_IsTrue(be_mgr, constraints)) {
          StreamMgr_print_error(streams,
            "This might be due to the constraints that are too strong.\n");
        }
        no_deadlock = false;
        break;

      default:
        StreamMgr_print_error(streams,
            "At iteration %d, the solver returned an unexpected value: %d\n",
                steps, satResult);
        no_deadlock = false;
        break;
      }

      /* 2.4. Remove and destroy the group containing the last
              computed state */
      SatIncSolver_destroy_group(solver, satGrp);

      if (opt_verbose_level_gt(opts, 1)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "... done\n");
      }

      if (opt_verbose_level_gt(opts, 0)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger,
                " -- simulation of step %d has finished in %2.1f seconds\n",
                steps, (util_cpu_time() - time_statistics) / 1000.0);
        time_statistics = util_cpu_time();
      }
    } /* (steps < target_steps) && no_deadlock) */

  } /* iteration block */

  /* 3. We clean the memory and we return */
  SatIncSolver_destroy(solver);

  if (no_deadlock && print_trace) {
    TraceMgr_execute_plugin(trace_manager, TRACE_OPT(NULL),
                                (changes_only) ? 0 : 1, tr_num);
  }

  return steps;

#else
  StreamMgr_print_error(streams,
          "%s: Relies on Incremental solving. "
          "No incremental SAT solver is available now\n", __func__);
  return -1;
#endif
}

Olist_ptr
Bmc_simulate_check_feasible_constraints(NuSMVEnv_ptr env,
                                        BeFsm_ptr be_fsm,
                                        BddEnc_ptr bdd_enc,
                                        Olist_ptr constraints,
                                        be_ptr from_state)
{
  Olist_ptr res = Olist_create();
#if NUSMV_HAVE_INCREMENTAL_SAT
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const Be_CnfAlgorithm cnf_alg = get_rbc2cnf_algorithm(opts);

  BeEnc_ptr be_enc = BeFsm_get_be_encoding(be_fsm);
  Be_Manager_ptr be_mgr = BeEnc_get_be_manager(be_enc);

  SatIncSolver_ptr solver;
  be_ptr be_init, be_trans, be_prob;

  /* Create the SAT solver instance */
  solver = Sat_CreateIncSolver(env, get_sat_solver(opts));
  if (SAT_INC_SOLVER(NULL) == solver) {
    ErrorMgr_internal_error(errmgr, "Incremental sat solver '%s' is not available.\n",
                   get_sat_solver(opts));
  }

  be_init = (from_state == (be_ptr) NULL) ? BeFsm_get_init(be_fsm) : from_state;
  be_init = Be_And(be_mgr, be_init, BeFsm_get_invar(be_fsm));

  be_trans = Be_And(be_mgr,
                    BeFsm_get_trans(be_fsm),
                    BeEnc_shift_curr_to_next(be_enc, BeFsm_get_invar(be_fsm)));

  be_prob = Be_And(be_mgr, be_init, be_trans);
  be_prob = BeEnc_untimed_expr_to_timed(be_enc, be_prob, 0);

  /* pushes the transition relation */
  Bmc_Utils_add_be_into_inc_solver_positively(
      SAT_INC_SOLVER(solver),
      SatSolver_get_permanent_group(SAT_SOLVER(solver)),
      be_prob, be_enc, cnf_alg);

  { /* loops over the constraints list */
    Oiter iter;
    OLIST_FOREACH(constraints, iter) {
      be_ptr be_constr = (be_ptr) Oiter_element(iter);
      boolean constr_truth;
      SatSolverGroup satGrp;
      SatSolverResult sat_res;

      be_constr = BeEnc_untimed_expr_to_timed(be_enc, be_constr, 0);

      /* pushes the constraint */
      satGrp = SatIncSolver_create_group(solver);
      Bmc_Utils_add_be_into_inc_solver_positively(
          SAT_INC_SOLVER(solver),
          satGrp, be_constr, be_enc, cnf_alg);

      /* solves */
      sat_res = SatSolver_solve_all_groups(SAT_SOLVER(solver));
      switch (sat_res) {
      case SAT_SOLVER_SATISFIABLE_PROBLEM:
        constr_truth = true; break;

      case SAT_SOLVER_UNSATISFIABLE_PROBLEM:
        constr_truth = false; break;

      default:
        error_unreachable_code(); /* no other options handled! */
      }

      Olist_append(res, (void*) constr_truth);
      SatIncSolver_destroy_group(solver, satGrp);
    } /* constr loop */
  }

  /* clean up */
  SatIncSolver_destroy(solver);

  /* there is a result for each input */
  nusmv_assert(Olist_get_size(constraints) == Olist_get_size(res));

#else
  {
    const StreamMgr_ptr streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
    StreamMgr_print_error(streams,  "%s: Relies on Incremental solving. " \
                          "No incremental SAT solver is available now\n",
                          __func__);
  }
#endif

  return res;
}

int Bmc_pick_state_from_constr(NuSMVEnv_ptr env,
                               BeFsm_ptr fsm, BddEnc_ptr bdd_enc,
                               be_ptr constr, Simulation_Mode mode,
                               boolean display_all)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  const Be_CnfAlgorithm cnf_alg = get_rbc2cnf_algorithm(opts);

  TraceMgr_ptr tm = TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
  BeEnc_ptr be_enc = BeFsm_get_be_encoding(fsm);
  Be_Manager_ptr be_mgr = BeEnc_get_be_manager(be_enc);
  SatSolver_ptr solver = \
    Sat_CreateNonIncSolver(env, get_sat_solver(opts));
  SatSolverResult sat_res;
  int tr_num = -1;

  be_ptr init, invar, prob;

  if (solver == SAT_SOLVER(NULL)) {
    ErrorMgr_internal_error(errmgr, "Non-incremental sat solver '%s' is not available.",
                   get_sat_solver(opts));
  }

  switch (mode) {
  case Random:
    bmc_simulate_enable_random_mode(SAT_SOLVER(solver));
    break;
  case Interactive:
    /* Do nothing */
    break;
  case Deterministic:
    /* Do nothing */
    break;
  default:
    ErrorMgr_internal_error(errmgr, "%s: Invalid mode given", __func__);
  }

  init = BeFsm_get_init(fsm);
  invar = BeFsm_get_invar(fsm);
  prob = Be_And(be_mgr, Be_And(be_mgr, init, Be_And(be_mgr, invar, constr)),
                constr);
  prob = BeEnc_untimed_expr_to_timed(be_enc, prob, 0);

  Bmc_Utils_add_be_into_non_inc_solver_positively(
      SAT_SOLVER(solver),
      prob, be_enc, cnf_alg);

  sat_res = SatSolver_solve_all_groups(solver);

  /* Processes the result: */
  switch (sat_res) {
  case SAT_SOLVER_SATISFIABLE_PROBLEM: {

    /* extracts the trace */
    Trace_ptr trace;

    /* this was set to false in previous implementation */
    boolean has_constraints = false;

    { /* Build language for the FSM */
      BoolSexpFsm_ptr bsexp_fsm; /* needed for trace language */
      NodeList_ptr symbols;
      bsexp_fsm =  \
          BOOL_SEXP_FSM(NuSMVEnv_get_value(env, ENV_BOOL_FSM));
      BOOL_SEXP_FSM_CHECK_INSTANCE(bsexp_fsm);
      symbols = SexpFsm_get_symbols_list(SEXP_FSM(bsexp_fsm));

      if (Interactive == mode) {
        trace = bmc_simulate_interactive_step(solver, be_enc, bdd_enc,
                                              symbols, false, display_all);
      }
      else {
        trace =                                 \
          Bmc_create_trace_from_cnf_model(be_enc,
                                          symbols,
                                          BMC_PICK_TRACE_DESCRIPTION,
                                          TRACE_TYPE_SIMULATION,
                                          SatSolver_get_model(solver),
                                          has_constraints ? 1 : 0);
      }
    }

    /* Registering the trace in the trace manager */
    tr_num = TraceMgr_register_trace(tm, trace);
    TraceMgr_set_current_trace_number(tm, tr_num);
    bmc_simulate_set_curr_sim_trace(env, trace, tr_num);
    break;
  }

  case SAT_SOLVER_UNSATISFIABLE_PROBLEM:
    /* deadlock in the initial state */
    bmc_simulate_set_curr_sim_trace(env, NULL, -1);
    tr_num = -1;
    break;

  default:
    ErrorMgr_internal_error(errmgr, "%s: Unexpected value in sat result", __func__);
  }

  return tr_num;
}

int
Bmc_Simulate_bmc_simulate_check_feasible_constraints(const NuSMVEnv_ptr env,
                                                const Olist_ptr str_constraints,
                                                const Olist_ptr be_constraints,
                                                const Olist_ptr expr_constraints,
                                                const boolean human_readable)
{
  const BeEnc_ptr be_enc = BE_ENC(NuSMVEnv_get_value(env, ENV_BE_ENCODER));
  const BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const StreamMgr_ptr streams =
   STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  int retval = 0;

  /* converts the string constraints to be constraints */
  while (!Olist_is_empty(str_constraints)) {
    char* str_constr = (char*) Olist_delete_first(str_constraints);

    CATCH(errmgr) {
      Expr_ptr constr;
      be_ptr be_constr =
        Bmc_Utils_next_costraint_from_string(be_enc, bdd_enc, str_constr,
                                             &constr);
      Olist_append(be_constraints, be_constr);
      Olist_append(expr_constraints, constr);
    }
    FAIL(errmgr) {
      StreamMgr_print_error(streams,  "In constraint: %s\n", str_constr);
      StreamMgr_print_error(streams,
                            "Parsing error: expected a next expression.\n");
      FREE(str_constr);

      return 1;
    }
    FREE(str_constr);
  }

  { /* does the actual work, and presents the result */
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    Trace_ptr curr_trace = bmc_simulate_get_curr_sim_trace(env);

    be_ptr curr_state = \
      TraceUtils_fetch_as_be(curr_trace, Trace_last_iter(curr_trace),
                             TRACE_ITER_SF_VARS, be_enc, bdd_enc);

    Olist_ptr res = Bmc_simulate_check_feasible_constraints(env,
                                                            BE_FSM(NuSMVEnv_get_value(env, ENV_BE_FSM)),
                                                            bdd_enc, be_constraints,
                                                            curr_state);
    {
      Oiter iter_c;
      Oiter iter_r;
      int idx = 0;

      /* prints out the result */
      for (iter_c=Olist_first(expr_constraints),
             iter_r=Olist_first(res), idx=0;
           !Oiter_is_end(iter_c) && !Oiter_is_end(iter_r);
           iter_c=Oiter_next(iter_c), iter_r=Oiter_next(iter_r), ++idx) {

        Expr_ptr constr = (Expr_ptr) Oiter_element(iter_c);
        boolean truth = (boolean) Oiter_element(iter_r);

        if (human_readable) {
          /* header */
          if (idx == 0) StreamMgr_print_output(streams,  "#num\ttruth\tconstraint\n");
          StreamMgr_print_output(streams,  "%d\t%d\t", idx, truth);
          StreamMgr_nprint_output(streams, wffprint, "%N", constr);
          StreamMgr_print_output(streams,  "\n");
        }
        else {
          /* not human-readable output format */
          StreamMgr_print_output(streams,  "%d", truth);
        }
      }

      StreamMgr_print_output(streams,  "\n");
    }

    Olist_destroy(res);
  }

  return retval;
}

int Bmc_Simulate_bmc_pick_state(const NuSMVEnv_ptr env,
                                TraceLabel label,
                                be_ptr be_constr,
                                int tr_number,
                                const Simulation_Mode mode,
                                const int display_all,
                                const boolean verbose)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const TraceMgr_ptr gtm = TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (label != NULL) {
    /* constructs a new trace from given label */
    Trace_ptr from_trace = \
      TraceMgr_get_trace_at_index(gtm, TraceLabel_get_trace(label));
    TraceIter iter = TraceMgr_get_iterator_from_label(gtm, label);

    Trace_ptr new_trace  = TRACE(NULL);

    /* create new simulation trace from previous one */
    new_trace = Trace_copy(from_trace, iter, false);

    Trace_set_desc(new_trace, "BMC Simulation");
    Trace_set_type(new_trace, TRACE_TYPE_SIMULATION);

    tr_number = TraceMgr_register_trace(gtm, new_trace);
    TraceMgr_set_current_trace_number(gtm, tr_number);
    bmc_simulate_set_curr_sim_trace(env, new_trace, tr_number);
  }
  else { /* creates the trace from given constraint */
    BeEnc_ptr be_enc = BE_ENC(NuSMVEnv_get_value(env, ENV_BE_ENCODER));
    Be_Manager_ptr be_mgr = BeEnc_get_be_manager(be_enc);
    BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));

    BeFsm_ptr be_fsm = BE_FSM(NuSMVEnv_get_value(env, ENV_BE_FSM));

    nusmv_assert(NULL != be_constr);

    tr_number = Bmc_pick_state_from_constr(env, be_fsm, bdd_enc,
                                           be_constr, mode, display_all);
  }

  /* results presentation */
  if (tr_number != -1) {
    if (verbose) {
      TraceMgr_execute_plugin(gtm, TRACE_OPT(NULL),
                              TRACE_MGR_DEFAULT_PLUGIN,
                              TRACE_MGR_LAST_TRACE);
    }
  }
  else {
    if ((be_ptr) NULL == be_constr) {
      StreamMgr_print_error(streams,  "No trace: initial state is inconsistent\n");
    }
    else {
      StreamMgr_print_error(streams,
                            "No trace: constraint and initial state are inconsistent\n");
    }
    return 1;
  }

  return 0;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief Internal function used during the simulation to set the
  current simulation trace


*/

void bmc_simulate_set_curr_sim_trace(const NuSMVEnv_ptr env,
                                     Trace_ptr trace, int idx)
{
  /* THis is because we are going to use an offset for putting the
     index into the environment */
  nusmv_assert(idx >= -1);

  NuSMVEnv_set_flag(env, ENV_BMC_HAS_TRACE, TRACE(NULL) != trace);
  NuSMVEnv_set_or_replace_value(env, ENV_BMC_SIM_TRACE, trace);
  NuSMVEnv_set_or_replace_value(env, ENV_BMC_SIM_TRACE_IDX,
                                NODE_FROM_INT(idx + DEF_SIM_TRACE_IDX_OFFSET));
}

Trace_ptr bmc_simulate_get_curr_sim_trace(const NuSMVEnv_ptr env)
{
  if (NuSMVEnv_get_flag(env, ENV_BMC_HAS_TRACE)) {
    return TRACE(NuSMVEnv_get_value(env, ENV_BMC_SIM_TRACE));
  }

  return TRACE(NULL);
}

int bmc_simulate_get_curr_sim_trace_index(const NuSMVEnv_ptr env)
{
  return NODE_TO_INT(NuSMVEnv_get_value(env, ENV_BMC_SIM_TRACE_IDX)) -
    DEF_SIM_TRACE_IDX_OFFSET;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Enables random mode in the given sat solver

  Enables random mode in the given sat solver.
                 Seed used in random
*/
static void bmc_simulate_enable_random_mode(SatSolver_ptr solver)
{
  utils_random_set_seed();
  SatSolver_set_random_mode(solver, utils_random());
}

/*!
  \brief Performs a step of interactive simulation

  Performs a step of interactive simulation.

                       Finds all alternative assignments of the
                       current model in the given sat solver. For this
                       reason, the function must be called only after
                       a call to SatSolver_solve_all_groups that
                       returned SAT. "in_simulation" determines
                       whether the interactive step should be done for
                       extending a trace (i.e. simulate) or when
                       creating a new one (i.e. pick_state). Returns a
                       trace that represents the new choosen state,
                       which can be used as starting point for
                       pick_state and can be concatenated to the
                       previously existing one when simulating
*/
static Trace_ptr bmc_simulate_interactive_step(SatSolver_ptr solver,
                                               BeEnc_ptr be_enc,
                                               BddEnc_ptr bdd_enc,
                                               NodeList_ptr symbols,
                                               boolean in_simulation,
                                               boolean display_all)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(solver));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  const Be_CnfAlgorithm cnf_alg = get_rbc2cnf_algorithm(opts);

  const Be_Manager_ptr be_mgr = BeEnc_get_be_manager(be_enc);
  const int max_states = opt_shown_states_level(opts);
  const SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(be_enc));

  Trace_ptr result = TRACE(NULL);

  Slist_ptr curr_model = SatSolver_get_model(solver);
  Trace_ptr curr_trace = Trace_create(st, BMC_PICK_TRACE_DESCRIPTION,
                                      TRACE_TYPE_SIMULATION, symbols, true);
  Trace_ptr* traces;
  Slist_ptr groups = Slist_create();
  Siter siter;
  int i = 1, j = 0, choice = 0;
  boolean found_all_states = false;

  traces = ALLOC(Trace_ptr, max_states);
  nusmv_assert(NULL != traces);

  /* Fill the first temporary trace */
  Bmc_fill_trace_from_cnf_model(be_enc, curr_model,
                                (in_simulation ? 1 : 0), curr_trace);
  /* Append the current set of assignments */
  traces[0] = curr_trace;

  /* Now lookup for other states */
  while ((i < max_states) && (!found_all_states)) {
    SatSolverGroup iSatGrp = (SatSolverGroup)-1;
    SatSolverResult iSatResult = SAT_SOLVER_UNAVAILABLE;
    be_ptr be_tr;
    be_ptr be_ass;

    /* Retrieve the variables assignments from the very latest
       trace: Those will be negated and added as a group to
       the sat solver */
    be_tr = Be_Not(be_mgr,
                   TraceUtils_fetch_as_be(curr_trace,
                                          (in_simulation ?
                                           Trace_last_iter(curr_trace) :
                                           Trace_first_iter(curr_trace)),
                                          TRACE_ITER_SF_VARS, be_enc, bdd_enc));
    be_ass = BeEnc_untimed_expr_to_timed(be_enc, be_tr, (in_simulation ? 1 : 0));

    if (in_simulation) {
      iSatGrp = SatIncSolver_create_group(SAT_INC_SOLVER(solver));
      Bmc_Utils_add_be_into_inc_solver_positively(
          SAT_INC_SOLVER(solver),
          iSatGrp, be_ass, be_enc, cnf_alg);
      Slist_push(groups, (void*)iSatGrp);
    }
    else {
      Bmc_Utils_add_be_into_non_inc_solver_positively(
          solver,
          be_ass, be_enc, cnf_alg);
    }

    iSatResult = SatSolver_solve_all_groups(solver);

    /* Processes the result: */
    switch (iSatResult) {

    case SAT_SOLVER_UNSATISFIABLE_PROBLEM:
      /* no more possible states.. Quit the loop  */
      found_all_states = true;
      break;

    case SAT_SOLVER_SATISFIABLE_PROBLEM:
      {
        curr_model = SatSolver_get_model(solver);
        curr_trace = Trace_create(st, BMC_PICK_TRACE_DESCRIPTION,
                                  TRACE_TYPE_SIMULATION, symbols, true);
        /* Build the trace upon what we just found */
        Bmc_fill_trace_from_cnf_model(be_enc, curr_model,
                                      (in_simulation ? 1 : 0), curr_trace);

        /* Append the current set of assignments */
        traces[i] = curr_trace;

        ++i; /* Found one state, count it */
        break;
      } /* SAT */

    case SAT_SOLVER_INTERNAL_ERROR:
      ErrorMgr_internal_error(errmgr, "Sorry, solver answered with a fatal Internal "
                     "Failure during problem solving.\n");

    case SAT_SOLVER_TIMEOUT:
    case SAT_SOLVER_MEMOUT:
      ErrorMgr_internal_error(errmgr, "Sorry, solver ran out of resources and aborted "
                     "the execution.\n");

    default:
      ErrorMgr_internal_error(errmgr, " Bmc_Simulate: Unexpected value in sat result");
    } /* switch */
  }

  { /* Start printing found states */
    hash_ptr shown_assignments = (hash_ptr)NULL;

    if (!display_all) {
      shown_assignments = new_assoc();
    }

    /* Show found states */
    StreamMgr_print_output(streams,
            "\n***************  AVAILABLE STATES  *************\n");
    for (j = 0; j < i; ++j) {
      if (in_simulation) {
        bmc_simulate_print_state(env, traces[j], Trace_last_iter(traces[j]),
                                 j, in_simulation, shown_assignments);
      }
      else {
        bmc_simulate_print_state(env, traces[j], Trace_first_iter(traces[j]),
                                 j, in_simulation, shown_assignments);
      }
    }

    if (!display_all) {
      free_assoc(shown_assignments);
    }
  } /* End printing found states */

  /* Let the user choose */
  choice = bmc_simulate_ask_for_state(streams, j - 1);

  /* Pick the "trace" (i.e. the state) choosed by the user */
  result = traces[choice];

  /* Free Memory */
  for (j = 0; j < i; ++j) {
    /* The one left will be returned, shall be destroyed by the
       caller. */
    if (j != choice) { Trace_destroy(traces[j]); }
  }
  FREE(traces);

  SLIST_FOREACH(groups, siter) {
    SatSolverGroup g = (SatSolverGroup)Siter_element(siter);

    /* Just because we expect an incremental solver only if simulating
       (bmc_inc_simulate). Pick state uses a non incremental sat
       solver. */
    nusmv_assert(in_simulation);
    SatIncSolver_destroy_group(SAT_INC_SOLVER(solver), g);
  }
  Slist_destroy(groups);

  nusmv_assert(TRACE(NULL) != result);
  return result;
}

/*!
  \brief Aux function for interactive simulation.
                       Prints the given set of assignments

  Prints all variable assignments of trace "trace", in
                       step "step"
*/
static void bmc_simulate_print_state(const NuSMVEnv_ptr env,
                                     Trace_ptr trace, TraceIter step,
                                     int state_num, boolean show_inputs,
                                     hash_ptr shown_assignments)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  NodeList_ptr inputs = Trace_get_i_vars(trace);
  boolean has_inputs = NodeList_get_length(inputs) > 0;

  StreamMgr_print_output(streams,  "\n================= State =================\n");
  StreamMgr_print_output(streams,  "%d) -------------------------\n", state_num);

  bmc_simulate_trace_step_print(env,
                                trace, step, TRACE_ITER_SF_VARS,
                                shown_assignments);

  if (has_inputs && show_inputs) {
    StreamMgr_print_output(streams,  "\nThis state is reachable through:\n");
    bmc_simulate_trace_step_print(env,
                                  trace, step, TRACE_ITER_I_VARS,
                                  shown_assignments);
  }

  StreamMgr_print_output(streams,  "\n");
}

/*!
  \brief Aux function for interactive simulation.
                       Prints the given set of assignments

  Prints all variable assignments of trace "trace", in
                       step "step" and of type "it_type"
*/
static void bmc_simulate_trace_step_print(const NuSMVEnv_ptr env,
                                          const Trace_ptr trace,
                                          const TraceIter step,
                                          TraceIteratorType it_type,
                                          hash_ptr shown_assignments)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  TraceStepIter iter;
  node_ptr symb, val;

  TRACE_STEP_FOREACH(trace, step, it_type, iter, symb, val) {
    if ((hash_ptr)NULL != shown_assignments) {
      if (val == find_assoc(shown_assignments, symb)) continue;

      insert_assoc(shown_assignments, symb, val);
    }

    StreamMgr_print_output(streams,  "   ");
    StreamMgr_nprint_output(streams, wffprint, "%N", symb);
    StreamMgr_print_output(streams,  " = ");
    StreamMgr_nprint_output(streams, wffprint, "%N", val);
    StreamMgr_print_output(streams,  "\n");
  }
}

/*!
  \brief Aux function for interactive simulation.
                       Asks the user for a number from 0 to max_choice.

  Asks the user for a number from 0 to max_choice. Input
                       is taken from the stream manager input
                       stream. Returns the selected number
*/
static int bmc_simulate_ask_for_state(StreamMgr_ptr streams, int max_choice)
{
  FILE* instream = StreamMgr_get_input_stream(streams);
  int choice = 0;
  int i;

  if (max_choice > 0) {
    char line[READ_BUF_SIZE];

    for (i = 0; i < READ_BUF_SIZE; i++) line[i] = '\0';

    StreamMgr_print_output(streams,
            "\nChoose a state from the above (0-%d): ", max_choice);

    while (NIL(char) != (fgets(line, READ_BUF_SIZE, instream)) ||
           (line[0] != '\n') ) {
      if ((sscanf(line, "%d", &choice) != 1) || (choice < 0) ||
          (choice > max_choice)) {
        StreamMgr_print_output(streams,
                "Choose a state from the above (0-%d): ", max_choice);
        continue;
      }
      else break;
    }
  }
  else {
    /* there is only one possible choice here: */
    StreamMgr_print_output(streams,
            "\nThere's only one available state. Press Return to Proceed.");
    while ((choice = getc(instream)) != '\n') ; /* consumes chars */
    choice = 0;
  }

  StreamMgr_print_output(streams,  "\nChosen state is: %d\n", choice);

  return choice;
}
