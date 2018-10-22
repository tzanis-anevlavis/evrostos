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
  \author Andrea Morichetti, Roberto Cavada
  \brief Simulator routines

  This file contains functions for image computation, for state
        picking in a set (according to three different policies), for states display
        and constraints insertion.

*/

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/simulate/simulateInt.h"
#include "nusmv/core/simulate/simulateTransSet.h"
#include "nusmv/core/simulate/SimulateState.h"

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/error.h" /* for CATCH(errmgr) */
#include "nusmv/core/utils/utils_io.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/parser.h"

#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/TraceMgr.h"
#include "nusmv/core/trace/TraceLabel.h"
#include "nusmv/core/trace/pkg_trace.h"

/* WARNING [MD] This is wrong. The function has to be moved in mc/mc.h */
#include "nusmv/core/mc/mcInt.h" /* for Mc_trace_step_put_state_from_bdd */
#include "nusmv/core/mc/mc.h"

#include "nusmv/core/enc/enc.h"
#include "nusmv/core/prop/propPkg.h" /* for PropDb_master_get_scalar_sexp_fsm */
#include "nusmv/core/compile/symb_table/SymbTable.h"

#if NUSMV_HAVE_SIGNAL_H
# include <signal.h>
#endif

/**Variable********************************************************************

  Synopsis    [Stack and context for non-local goto inside simulator]

  Description [Stack and context for non-local goto inside simulator]

******************************************************************************/
/* Doesn't create reentrancy problems */
#if defined SOLARIS && SOLARIS
static sigjmp_buf simulate_jmp_buff;
#else
static jmp_buf simulate_jmp_buff;
#endif

#if NUSMV_HAVE_SIGNAL_H
static  void (*saved_simulate_sigterm)(int);
#endif

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static boolean
list_contains_next_var(const SymbTable_ptr st, const NodeList_ptr vars);

static bdd_ptr simulate_accumulate_constraints(NuSMVEnv_ptr env, BddEnc_ptr enc, bdd_ptr bdd,
           int max_size);
static bdd_ptr simulate_request_constraints(NuSMVEnv_ptr env, BddEnc_ptr enc);

static void simulate_choose_next(NuSMVEnv_ptr env,
                                 BddFsm_ptr fsm,
                                 bdd_ptr from_state,
                                 bdd_ptr next_state_set,
                                 Simulation_Mode mode,
                                 int display_all,
                                 bdd_ptr* which_input,
                                 bdd_ptr* which_state);

static void
simulate_extend_print_curr_trace(NuSMVEnv_ptr env,
                                 BddEnc_ptr enc, node_ptr fragment,
                                 boolean printyesno,
                                 boolean only_changes,
                                 NodeList_ptr symbols);

#if NUSMV_HAVE_SIGNAL_H
static void simulate_sigterm(int);
#endif

node_ptr Simulate_MultipleSteps(NuSMVEnv_ptr env, BddFsm_ptr fsm, bdd_ptr constraint,
                                boolean time_shift, Simulation_Mode mode,
                                int n, int display_all)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  SimulateState_ptr const current_state =
    SIMULATE_STATE(NuSMVEnv_get_value(env, ENV_SIMULATE_STATE));

  BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);
  DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
  bdd_ptr next_constraint;
  bdd_ptr current_state_bdd;
  bdd_ptr input;
  bdd_ptr state;
  bdd_ptr mask;
  node_ptr result = Nil;
  int i = 1;

  current_state_bdd = SimulateState_get_bdd(current_state);
  /* we append the current state in first position of the trace */
  result = cons(nodemgr, (node_ptr) current_state_bdd, result);
  mask = BddEnc_get_state_frozen_vars_mask_bdd(enc);
  while (i <= n) {
    bdd_ptr next_constr_set = (bdd_ptr)NULL;

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      switch (mode) {
      case Interactive:
        Logger_log(logger,
                "********  Interactive mode Simulation step %d  *********\n",i);
        break;
      case Random:
        Logger_log(logger,
                "**********  Random mode Simulation step %d  **********\n",i);
        break;
      case Deterministic:
        Logger_log(logger,
                "*******  Deterministic mode Simulation step %d  *******\n",i);
        break;
      }
    }

    /* if time shifting is enabled, perform current -> next time shift
       on state vars, leave constraint as it is otherwise */
    if (time_shift) {
      next_constraint = BddEnc_state_var_to_next_state_var(enc, constraint);
    }
    else {
      next_constraint = bdd_dup(constraint);
    }
    next_constr_set = BddFsm_get_constrained_forward_image(fsm, current_state_bdd, next_constraint);
    bdd_free(dd, next_constraint);

    bdd_and_accumulate(dd, &next_constr_set, mask);

    if (bdd_is_false(dd, next_constr_set)) {
      StreamMgr_print_error(streams,  "No future state exists");
      StreamMgr_print_error(streams,  (llength(result) == 1 ? ": trace not built.\n" : "."));
      StreamMgr_print_error(streams,  "Simulation stopped at step %d.\n", i);
      bdd_free(dd, next_constr_set);
      result = reverse(result);
      /* We don't free the current_state_bdd variable because the
         list "result" must contain referenced states */
      return(result);
    }

    Simulate_ChooseOneStateInput(env, fsm,
                                 current_state_bdd, next_constr_set,
                                 mode, display_all,
                                 &input, &state);

    if (state == (bdd_ptr) NULL || bdd_is_false(dd, state)) {
      StreamMgr_print_error(streams,
        "\nCan't find a future state. Simulation stopped at step %d.\n", i);
      if (state != (bdd_ptr) NULL) { bdd_free(dd, state); }
      bdd_free(dd, next_constr_set);
      result = reverse(result);
      return result;
    }

    if (input != (bdd_ptr) NULL) {
      result = cons(nodemgr, (node_ptr) input, result);
    }
    else {
      /* there are no inputs */
      result = cons(nodemgr, (node_ptr) bdd_true(dd), result);
    }

    result = cons(nodemgr, (node_ptr) state, result);

    /* we don't free the current_state_bdd variable because the final list "result"
       must contain referenced states: they will be freed after their insertion
       in the traces_hash table */
    i++;
    bdd_free(dd, next_constr_set);
    current_state_bdd = state;
  }

  result = reverse(result);
  return result;
}

bdd_ptr Simulate_ChooseOneState(NuSMVEnv_ptr env, BddFsm_ptr fsm,
                                bdd_ptr next_set, Simulation_Mode mode,
                                int display_all)
{
  bdd_ptr state;
  bdd_ptr dummy_input;

  simulate_choose_next(env, fsm, NULL, next_set, mode, display_all,
                       &dummy_input, &state);

  nusmv_assert(dummy_input == (bdd_ptr) NULL);
  return state;
}

void Simulate_ChooseOneStateInput(NuSMVEnv_ptr env, BddFsm_ptr fsm,
                                  bdd_ptr from_state, bdd_ptr next_set,
                                  Simulation_Mode mode,
                                  int display_all,
                                  bdd_ptr* input, bdd_ptr* state)
{
  simulate_choose_next(env, fsm, from_state, next_set,
                       mode, display_all,
                       input, state);
}

int Simulate_CmdPickOneState(NuSMVEnv_ptr env, BddFsm_ptr fsm, Simulation_Mode mode,
                             int display_all, bdd_ptr bdd_constraints)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);
  DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
  bdd_ptr initial_set;
  bdd_ptr chosen = (bdd_ptr) NULL;
  int i;
  int trace_id = -1;

  {
    bdd_ptr init_bdd = BddFsm_get_init(fsm);
    bdd_ptr invar_bdd = BddFsm_get_state_constraints(fsm);
    bdd_ptr tmp;

    tmp = bdd_and(dd, init_bdd, invar_bdd);
    initial_set = BddEnc_apply_state_frozen_vars_mask_bdd(enc, tmp);

    bdd_free(dd, tmp);
    bdd_free(dd, init_bdd);
    bdd_free(dd, invar_bdd);
  }

  if (NULL != bdd_constraints) {
    bdd_and_accumulate(dd, &initial_set, bdd_constraints);
  }

  i = BddEnc_count_states_of_bdd(enc, initial_set);

  if (i == 0) {
    StreamMgr_print_error(streams,  "The set of initial states is EMPTY. "
      "No state can be chosen.\n");
    bdd_free(dd, initial_set);
    return -1;
  }

  chosen = Simulate_ChooseOneState(env, fsm, initial_set, mode, display_all);
  bdd_free(dd, initial_set);

  if ((chosen == (bdd_ptr) NULL) || bdd_is_false(dd, chosen)) {
    StreamMgr_print_error(streams,  "Chosen state is the EMPTY set. "
      "No state has been chosen.\n");
    if (chosen != (bdd_ptr) NULL) bdd_free(dd, chosen);
    return -1;
  }
  else {
    SexpFsm_ptr sexp_fsm = \
        SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));

    TraceLabel label;
    Trace_ptr trace;

    SEXP_FSM_CHECK_INSTANCE(sexp_fsm);

    /* Now the new label we get would be the label of the next trace that
     * is going to be registered. */
    label = TraceLabel_create(nodemgr, TraceMgr_get_size(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR))), 0);;
    (void)SimulateState_set_in_env(env, chosen, label);

    /* picked state is set as the 'current_state' and as the first state
       of a new trace */
    trace = Trace_create(SexpFsm_get_symb_table(sexp_fsm),
                         "Simulation Trace", TRACE_TYPE_SIMULATION,
                         SexpFsm_get_symbols_list(sexp_fsm), false);

    Mc_trace_step_put_state_from_bdd(trace, Trace_first_iter(trace),
                                     enc, chosen);

    trace_id = TraceMgr_register_trace(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), trace);
    TraceMgr_set_current_trace_number(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), trace_id);

    bdd_free(dd, chosen);
  }

  return trace_id;
}

int Simulate_pick_state(const NuSMVEnv_ptr env,
                        TraceLabel label,
                        const Simulation_Mode mode,
                        const int display_all,
                        const boolean verbose,
                        bdd_ptr bdd_constraints)
{
  const NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const TraceMgr_ptr gtm = TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  int tr_number = -1;

  if (label != TRACE_LABEL_INVALID) {
    /* constructs a new trace from given label */
    Trace_ptr from_trace = \
      TraceMgr_get_trace_at_index(gtm, TraceLabel_get_trace(label));
    TraceIter iter = TraceMgr_get_iterator_from_label(gtm, label);

    Trace_ptr new_trace  = TRACE(NULL);
    bdd_ptr state;
    TraceLabel new_label;
    BddEnc_ptr enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));

    /* create new simulation trace from previous one */
    new_trace = Trace_copy(from_trace, iter, false);

    Trace_set_desc(new_trace, "Simulation Trace");
    Trace_set_type(new_trace, TRACE_TYPE_SIMULATION);

    tr_number = TraceMgr_register_trace(gtm, new_trace);
    TraceMgr_set_current_trace_number(gtm, tr_number);

    /* Now the new label we get would be the label of the next
     * trace that is going to be registered. */
    new_label = TraceLabel_create(nodemgr,
                                  TraceMgr_get_size(gtm),
                                  TraceMgr_get_abs_index_from_label(gtm, label));

    state = TraceUtils_fetch_as_bdd(from_trace, iter,
                                    TRACE_ITER_SF_VARS, enc);

    (void)SimulateState_set_in_env(env, state, new_label);
  }
  else {
    BddFsm_ptr bdd_fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));
    tr_number = Simulate_CmdPickOneState(env, bdd_fsm, mode,
                                         display_all, bdd_constraints);
  }

  /* results presentation */
  /* this should be a public function not called here */
  {
    if (tr_number != -1) {
      if (verbose) {
        TraceMgr_execute_plugin(gtm, TRACE_OPT(NULL),
                                TRACE_MGR_DEFAULT_PLUGIN,
                                TRACE_MGR_LAST_TRACE);
      }

      return 0;
    }
    else {
      if (NULL == bdd_constraints) {
        StreamMgr_print_error(streams,  "No trace: initial state is inconsistent\n");
      }
      else {
        StreamMgr_print_error(streams,
                              "No trace: constraint and initial state are inconsistent\n");
      }
      return 1;
    }
  }

  return 0;
}

int Simulate_simulate(const NuSMVEnv_ptr env,
                      const boolean time_shift,
                      const Simulation_Mode mode,
                      const int steps,
                      const int display_all,
                      const boolean printrace,
                      const boolean only_changes,
                      const bdd_ptr bdd_constraints)
{
  /* needed for trace lanugage */
  const SexpFsm_ptr const sexp_fsm =
    SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));
  const BddFsm_ptr const fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));
  const StreamMgr_ptr const streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const BddEnc_ptr const bdd_enc =
    BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  const NodeMgr_ptr nodemgr =
     NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const DDMgr_ptr dd = BddEnc_get_dd_manager(bdd_enc);
  node_ptr current_trace = Nil;
  TraceLabel curr_lbl;
  SimulateState_ptr const current_state =
    SIMULATE_STATE(NuSMVEnv_get_value(env, ENV_SIMULATE_STATE));

  curr_lbl = SimulateState_get_trace_label(current_state);
  nusmv_assert(curr_lbl != TRACE_LABEL_INVALID);

  StreamMgr_print_output(streams,  "********  Simulation Starting From State %d.%d "
                         "  ********\n",
                         TraceLabel_get_trace(curr_lbl) + 1,
                         TraceLabel_get_state(curr_lbl) + 1);

  /* Important: the simulation ALWAYS starts from the current selected state */
  current_trace = Simulate_MultipleSteps(env, fsm, bdd_constraints, time_shift,
                                         mode, steps, display_all);
  if (current_trace == Nil) {
    return 1;
  }

  /* extends and prints the current simulation trace */
  simulate_extend_print_curr_trace(env, bdd_enc, current_trace, printrace,
                                   only_changes,
                                   SexpFsm_get_symbols_list(sexp_fsm));

  /* Update the current state. */
  {
    int trace_id;
    Trace_ptr curr_trace;
    BddStates curr_state;
    TraceLabel new_label;

    trace_id = TraceMgr_get_current_trace_number(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)));

    curr_trace = TraceMgr_get_trace_at_index(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                                             trace_id);

    new_label = TraceLabel_create(nodemgr, trace_id, Trace_get_length(curr_trace));

    curr_state = TraceUtils_fetch_as_bdd(curr_trace,
                                         Trace_last_iter(curr_trace),
                                         TRACE_ITER_SF_VARS, bdd_enc);

    SimulateState_set_all(current_state, curr_state, new_label);

    bdd_free(dd, curr_state);
  }

  walk_dd(dd, bdd_free, current_trace);

  return 0;
}

TraceLabel Simulate_get_new_trace_no_from_label(NuSMVEnv_ptr env,
                                                TraceMgr_ptr gtm,
                                                const char* str_label,
                                                int* out_tr_number)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  int from_trace_no;
  Trace_ptr from_trace;
  TraceLabel label = TraceLabel_create_from_string(nodemgr, str_label);

  if (label == TRACE_LABEL_INVALID || !TraceMgr_is_label_valid(gtm, label)) {
    StreamMgr_print_error(streams,  "Label \"%s\" is invalid\n", str_label);
    *out_tr_number = -1;
    return NULL;
  }

  /* trace */
  from_trace_no = TraceLabel_get_trace(label);
  from_trace = TraceMgr_get_trace_at_index(gtm, from_trace_no);

  { /* constructs a new trace from given label */
    TraceIter iter = TraceMgr_get_iterator_from_label(gtm, label);
    Trace_ptr new_trace = Trace_copy(from_trace, iter, false);

    Trace_set_desc(new_trace, "Simulation Trace");
    Trace_set_type(new_trace, TRACE_TYPE_SIMULATION);

    *out_tr_number = TraceMgr_register_trace(gtm, new_trace);
    TraceMgr_set_current_trace_number(gtm, *out_tr_number);
  }

  return label;
}

int Simulate_goto_state(NuSMVEnv_ptr env,
                        TraceLabel label)
{
  StreamMgr_ptr const streams =
   STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  NodeMgr_ptr const nodemgr =
     NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  OStream_ptr outstream = StreamMgr_get_output_ostream(streams);
  Trace_ptr from_trace, new_trace;
  TraceIter iter;
  bdd_ptr state;
  int new_trace_id;
  node_ptr new_label;
  int from_trace_no = TraceLabel_get_trace(label);
  BddEnc_ptr enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  SexpFsm_ptr scalar_fsm = SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));
  int retval = 0;

  from_trace = TraceMgr_get_trace_at_index(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                                           from_trace_no);
  iter = TraceMgr_get_iterator_from_label(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                                          label);

  state = TraceUtils_fetch_as_bdd(from_trace, iter,
                                  TRACE_ITER_SF_VARS, enc);

  /* create new trace copying from given one up to given iter */
  new_trace = Trace_copy(from_trace, iter, false);
  Trace_set_desc(new_trace, "Simulation Trace");
  Trace_set_type(new_trace, TRACE_TYPE_SIMULATION);

  /* Now the new label we get would be the label of the next
   * trace that is going to be registered. */
  new_label = TraceLabel_create(nodemgr,
                                TraceMgr_get_size(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR))),
                                TraceMgr_get_abs_index_from_label(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), label));

  new_trace_id = TraceMgr_register_trace(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                                         new_trace);
  TraceMgr_set_current_trace_number(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                                    new_trace_id);
  (void)SimulateState_set_in_env(env, state, new_label);

  StreamMgr_print_output(streams,  "The current state for new trace is:\n");

  StreamMgr_print_output(streams,  "-> State %d.%d <-\n",
                         TraceLabel_get_trace(new_label)+1,
                         TraceLabel_get_state(new_label)+1);

  BddEnc_print_bdd_begin(enc, SexpFsm_get_vars_list(scalar_fsm), true);
  StreamMgr_set_indent_size(streams, 2);
  BddEnc_print_bdd(enc, state, (VPFBEFNNV) NULL, outstream, NULL);
  StreamMgr_reset_indent_size(streams);
  BddEnc_print_bdd_end(enc);

  return retval;
}

int Simulate_print_current_state(NuSMVEnv_ptr env,
                                 boolean Verbosely)
{
  StreamMgr_ptr const streams =
   STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  SexpFsm_ptr const sexp_fsm = SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));

  int retval = 0;

  BddEnc_ptr enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  SimulateState_ptr const current_state =
    SIMULATE_STATE(NuSMVEnv_get_value(env, ENV_SIMULATE_STATE));
  TraceLabel const current_state_label = SimulateState_get_trace_label(current_state);

  StreamMgr_print_output(streams,  "Current state is %d.%d\n",
                         TraceLabel_get_trace(current_state_label) + 1,
                         TraceLabel_get_state(current_state_label) + 1);

  if (Verbosely == 0) {
    bdd_ptr const current_state_bdd = SimulateState_get_bdd(current_state);
    OStream_ptr outstream = StreamMgr_get_output_ostream(streams);

    BddEnc_print_bdd_begin(enc, SexpFsm_get_vars_list(sexp_fsm), false);
    BddEnc_print_bdd(enc, current_state_bdd, (VPFBEFNNV) NULL, outstream, NULL);
    BddEnc_print_bdd_end(enc);
  }

  return retval;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

bdd_ptr simulate_get_constraints_from_string(NuSMVEnv_ptr env,
                                             const char* constr_str,
                                             BddEnc_ptr enc,
                                             boolean allow_nexts,
                                             boolean allow_inputs)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  node_ptr parsed = Nil;
  char* old_input_file;

  bdd_ptr res = (bdd_ptr) NULL;

  SymbTable_ptr st =
    BaseEnc_get_symb_table(BASE_ENC(enc));

  TypeChecker_ptr tc =
    BaseEnc_get_type_checker(BASE_ENC(enc));

  Set_t vars = NULL;

  /* String has to be duplicated, since the internal one is returned,
     and overwriting it will result in it's freeing */
  old_input_file = util_strsav(get_input_file(opts));
  set_input_file(opts, "<command-line>");

  if ((Parser_ReadNextExprFromString(env, constr_str, &parsed) == 0) &&
      (parsed != Nil) && (node_get_type(parsed) == NEXTWFF)) {

    node_ptr constraints = car(parsed);

    boolean is_wff =
      TypeChecker_is_expression_wellformed(tc, constraints, Nil);

    if (is_wff) {

      /* keep NEXT nodes */
      vars = Formula_GetDependenciesByType(st, constraints, Nil,
                                           VFT_CNIF, true);

      /* verify wrt input that are not allowed in constraints: */
      CATCH(errmgr) {
        const NodeList_ptr vars_list = Set_Set2List(vars);

        if (!allow_inputs &&
            SymbTable_list_contains_input_var(st, vars_list)) {

          /* input vars are not allowed */
          StreamMgr_print_error(streams,
                  "Parsing error: constraints cannot contain "
                  "input variables.\n");
          goto exit_error_constraint;
        }

        /* if allow_next is false, formula can not contain
           NEXTed(vars) */
        if (!allow_nexts &&
            list_contains_next_var(st, vars_list)) {

          StreamMgr_print_error(streams,
                  "Parsing error: constraints must be "
                  "\"simple expressions\".\n");
          goto exit_error_constraint;
        }

        /* sexp -> bdd */
        res = BddEnc_expr_to_bdd(enc, constraints, Nil);
      } /* CATCH(errmgr) */

      FAIL(errmgr) {
        StreamMgr_print_error(streams,
                "Parsing error: constraints must be "
                "\"simple expressions\".\n");
        res = (bdd_ptr) NULL;

        ErrorMgr_error_type_system_violation(errmgr);
      }
    }
  } /* parsed NEXTWFF ok */
  else {
    StreamMgr_print_error(streams,
            "Parsing error: constraints must be "
            "\"simple expressions\".\n");
  }

 exit_error_constraint:
  set_input_file(opts, old_input_file);
  FREE(old_input_file);

  if ((Set_t) NULL != vars) { Set_ReleaseSet(vars); }

  return res;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static boolean
list_contains_next_var(const SymbTable_ptr st, const NodeList_ptr vars)
{
  ListIter_ptr iter;
  boolean res = false;

  UNUSED_PARAM(st);

  NODE_LIST_FOREACH(vars, iter) {
    node_ptr dep = NodeList_get_elem_at(vars, iter);

    if (NEXT == node_get_type(dep)) {
      res = true;
      break;
    }
  }

  return res;
}

/*!
  \brief 

  from_state can be NULL from the initial set of states.
  At the end which_input will contained the chosen input (if any, NULL
  otherwise) and which_state will contain the chosen state
*/
static void simulate_choose_next(NuSMVEnv_ptr env, BddFsm_ptr fsm,
                                 bdd_ptr from_state, bdd_ptr next_state_set,
                                 Simulation_Mode mode,
                                 int display_all,
                                 bdd_ptr* which_input, bdd_ptr* which_state)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  FILE* instream = StreamMgr_get_input_stream(streams);

  BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);
  DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
  int i;

  *which_state = NULL;
  *which_input = NULL;

  if (mode == Interactive) {
    SimulateTransSet_ptr trans_set = SIMULATE_TRANS_SET(NULL);

    /* to detect ctrl+c interrupt */
#if NUSMV_HAVE_SIGNAL_H
    saved_simulate_sigterm = signal(SIGINT, simulate_sigterm);
#endif
    if (SETJMP(simulate_jmp_buff, 1) == 0) {
      double states_count;
      int max_choice;
      int choice = 0;
      bdd_ptr constraints;
      bdd_ptr constrained_next_set;

      /* if required, this will ask for further constraints: */
      constraints = simulate_accumulate_constraints(env, enc, next_state_set,
               opt_shown_states_level(opts));

      bdd_and_accumulate(dd, &constraints, next_state_set);
      constrained_next_set = BddEnc_apply_state_frozen_vars_mask_bdd(enc, constraints);
      bdd_free(dd, constraints);

      states_count = BddEnc_count_states_of_bdd(enc, constrained_next_set);
      trans_set = SimulateTransSet_create(fsm, enc, from_state,
            constrained_next_set, states_count);
      bdd_free(dd, constrained_next_set);

      max_choice = SimulateTransSet_print(trans_set, (display_all == 0),
                                          StreamMgr_get_output_ostream(streams));
      if (max_choice > 0) {
        char line[CHOICE_LENGTH];

        for (i=0; i<CHOICE_LENGTH; i++) line[i] = '\0';
        StreamMgr_print_output(streams,
                "\nChoose a state from the above (0-%d): ", max_choice);

        while (NIL(char) != (fgets(line, CHOICE_LENGTH, instream)) ||
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
      SimulateTransSet_get_state_input_at(trans_set, choice,
            which_state, which_input);
    } /* setjump */

    /* construction might be failed, so the instance is checked before
       being destroyed: */
    if (trans_set != SIMULATE_TRANS_SET(NULL)) {
      SimulateTransSet_destroy(trans_set);
    }

  } /* if mode is interactive */

  else { /* random and deterministic modes: */
    bdd_ptr next_state;
    bdd_ptr input = (bdd_ptr) NULL;

    nusmv_assert( (mode == Random) || (mode == Deterministic) );

    if (mode == Random) {
      next_state = BddEnc_pick_one_state_rand(enc, next_state_set);

      if (from_state != (bdd_ptr) NULL) {
        bdd_ptr inputs, masked_inputs;

        inputs = BddFsm_states_to_states_get_inputs(fsm, from_state, next_state);
        masked_inputs = BddEnc_apply_input_vars_mask_bdd(enc, inputs);
        bdd_free(dd, inputs);

        input = BddEnc_pick_one_input_rand(enc, masked_inputs);
        bdd_free(dd, masked_inputs);
      }
    }
    else {  /* deterministic */
      next_state = BddEnc_pick_one_state(enc, next_state_set);

      if (from_state != (bdd_ptr) NULL) {
        bdd_ptr inputs, masked_inputs;

        inputs = BddFsm_states_to_states_get_inputs(fsm, from_state, next_state);
        masked_inputs = BddEnc_apply_input_vars_mask_bdd(enc, inputs);
        bdd_free(dd, inputs);

        input = BddEnc_pick_one_input(enc, masked_inputs);
        bdd_free(dd, masked_inputs);
      }
    }

    *which_state = next_state;
    *which_input = input;
  }
}


#if NUSMV_HAVE_SIGNAL_H

/*!
  \brief Signal handler

  SIGINT signal handler inside the simulator.
*/
static void simulate_sigterm(int sig) {
  UNUSED_PARAM(sig);

  signal(SIGINT, saved_simulate_sigterm);
  LONGJMP(simulate_jmp_buff, 1);
}
#endif

/*!
  \brief required

  There are 4 condition to be verified in order to accept
        new further constraints:

        1) entered expression must be a non-zero set;

  2) entered expression must be consistent with the accumulated
        constraints (i.e. the product (further /\ accumulated) must be
        non-zero;

  3) if (further /\ accumulated) is non-zero, it also must be
        non-zero the product (further /\ accumulated) /\ next_set of
        states

  4) cardinality of the set obtained from the last product must
     be <= shown_states

        

  \se required

  \sa optional
*/
static bdd_ptr simulate_accumulate_constraints(NuSMVEnv_ptr env, BddEnc_ptr enc,
                                               bdd_ptr bdd, int max_size)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  DDMgr_ptr dd;
  double size;
  double old_size = -1;
  bdd_ptr result;
  bdd_ptr masked_bdd;

  dd = BddEnc_get_dd_manager(enc);
  result = bdd_true(dd);

  masked_bdd = BddEnc_apply_state_frozen_vars_mask_bdd(enc, bdd);
  size = BddEnc_count_states_of_bdd(enc, masked_bdd);
  bdd_free(dd, masked_bdd);

  while (size > max_size) {
    bdd_ptr local_constr, constraints, constrained_bdd;

    if (old_size != size) {
      StreamMgr_print_output(streams,
        "Too many (%.0g) future states to visualize. "
        "Please specify further constraints: \n", size);
      old_size = size;
    }

    local_constr = simulate_request_constraints(env, enc);
    if (local_constr != (bdd_ptr) NULL) {
      constraints = bdd_and(dd, local_constr, result);
      bdd_free(dd, local_constr);
    }

    /* Incompatible constrains: */
    if (bdd_is_false(dd, constraints)) {
      StreamMgr_print_error(streams,
        "Entered expression is incompatible with old constraints."
        " Try again.\n");
      bdd_free(dd, constraints);
      continue;
    }

    /* too strong constraints: */
    constrained_bdd = bdd_and(dd, bdd, constraints);
    masked_bdd = BddEnc_apply_state_frozen_vars_mask_bdd(enc, constrained_bdd);
    bdd_free(dd, constrained_bdd);
    constrained_bdd = masked_bdd;

    if (bdd_is_false(dd, constrained_bdd)) {
      StreamMgr_print_error(streams,
        "Set of future states is EMPTY: constraints too strong?"
        " Try again.\n");
      bdd_free(dd, constrained_bdd);
      bdd_free(dd, constraints);
      continue;
    }

    /* accumulates contraints for result */
    bdd_free(dd, result);
    result = bdd_dup(constraints);

    /* recalculates loop conditions and cleans up */

    size = BddEnc_count_states_of_bdd(enc, constrained_bdd);
    bdd_free(dd, constrained_bdd);
    bdd_free(dd, constraints);
  }

  return result;
}

/*!
  \brief required

  optional

  \se required

  \sa optional
*/
static bdd_ptr simulate_request_constraints(NuSMVEnv_ptr env, BddEnc_ptr enc)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* instream = StreamMgr_get_input_stream(streams);
  char* simulation_buffer = NULL;
  size_t simulation_buffer_size = 0;

  DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
  bdd_ptr result = (bdd_ptr) NULL;

  /* if this is a first use of a buffer => allocate some memory */
  simulation_buffer_size = 50; /* 50 bytes at first */
  simulation_buffer = ALLOC(char, simulation_buffer_size);

  /* loops until it receives a valid constraint */
  while (true) {
    /* Continues if receives a "good" string */
    size_t read_bytes = 0;
    char* str = simulation_buffer;
    str[0] = '\0';

    StreamMgr_print_output(streams,  "> ");

    /* read one line, i.e. until new-line or end of file is received */
    while (NIL(char) != fgets(str, simulation_buffer_size - read_bytes,
                              instream)) {
      size_t new_read_bytes = strlen(str);
      if (new_read_bytes > 0 && str[new_read_bytes-1] == '\n') break;
      /* allocate new buffer if required */
      read_bytes += new_read_bytes;
      if (read_bytes == simulation_buffer_size - 1) {
        simulation_buffer_size *= 2;
        simulation_buffer = REALLOC(char, simulation_buffer,
                                    simulation_buffer_size);
      }
      str = simulation_buffer + read_bytes;
    } /* end of while(fgets) */

    result = simulate_get_constraints_from_string(env, simulation_buffer, enc,
                                                  false, /* no NEXTs */
                                                  false /*only states*/);
    if (result == (bdd_ptr) NULL) { /* error */
      StreamMgr_print_error(streams,  "Try again\n");
      continue;
    }
    if ( bdd_is_false(dd, result) ) {
      StreamMgr_print_error(streams,
              "Entered constraints are equivalent to the empty set. Try again.\n");
      bdd_free(dd, result);
      continue;
    }
    else break; /* given constraint is ok */

  } /* end of loop */

  FREE(simulation_buffer);

  return result;
}

/*!
  \brief Extends current simulation trace and prints it

  Extends current simulation trace by creating a new
                trace for simulation fragment and concatenating it to
                existing one.

  \sa Trace_concat
*/
static void simulate_extend_print_curr_trace(NuSMVEnv_ptr env,
                                             BddEnc_ptr enc,
                                             node_ptr fragment,
                                             boolean printyesno,
                                             boolean only_changes,
                                             NodeList_ptr symbols)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  const TraceMgr_ptr tm =
    TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));

  Trace_ptr trace;
  Trace_ptr extension;

  unsigned prev_length;

  trace = \
    TraceMgr_get_trace_at_index(tm, TraceMgr_get_current_trace_number(tm));

  prev_length = Trace_get_length(trace);

  /*  extend simulation trace */
  extension = \
    Mc_create_trace_from_bdd_state_input_list(enc, symbols, NIL(char),
                                              TRACE_TYPE_UNSPECIFIED,
                                              fragment);

  /* extend existing simulation trace */
  trace = Trace_concat(trace, &extension);
  nusmv_assert(TRACE(NULL) == extension);

  if (opt_verbose_level_gt(opts, 1) && printyesno) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));

    Logger_log(logger,
            "#####################################################\n"
            "######         Print Of Current Trace         #######\n"
            "#####################################################\n");
  }

  if (printyesno) {
    TracePlugin_ptr plugin;

    /* only the TraceExplainer plugin can be used here: */
    if (only_changes) {
      plugin = TraceMgr_get_plugin_at_index(tm, 0);
    }
    else {
      plugin = TraceMgr_get_plugin_at_index(tm, 1);
    }

    {
      TraceOpt_ptr trace_opt = \
        TraceOpt_create_from_env(env);

      TraceOpt_set_from_here(trace_opt, prev_length);
      (void)TracePlugin_action(plugin, trace, trace_opt);

      TraceOpt_destroy(trace_opt);
    }
  } /* if printyesno */
}
