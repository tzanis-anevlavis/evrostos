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
  \brief Dedicated algorithms for the verification of
   invariants on-the-fly wrt reachability analysis.

  Dedicated algorithms for the verification of
   invariants on-the-fly wrt reachability analysis.

*/


#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/portability.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/mc/mc.h"
#include "nusmv/core/mc/mcInt.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/prop/propPkg.h"

#include "nusmv/core/bmc/bmc.h"
#include "nusmv/core/wff/wff.h"
#include "nusmv/core/wff/w2w/w2w.h"
#include "nusmv/core/prop/propProp.h"


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

extern bdd_ptr actions_bdd;
extern node_ptr all_symbols;

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* The result type of an heuristic in forward-backward analysis */
typedef enum {FORWARD_STEP, BACKWARD_STEP} Step_Direction;

/* Signature of heuristics */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef Step_Direction (*heuristic_type)(DDMgr_ptr dd,
                                         bdd_ptr reachable_frontier,
                                         bdd_ptr bad_frontier,
                                         bdd_ptr reachable_states,
                                         bdd_ptr bad_states,
                                         int turn);

/* Signature of heuristics */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef boolean (*stopping_heuristic_type)(DDMgr_ptr dd,
                                           bdd_ptr reachable_frontier,
                                           bdd_ptr bad_frontier,
                                           bdd_ptr reachable_states,
                                           bdd_ptr bad_states,
                                           int turn);

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void print_result(NuSMVEnv_ptr env, Prop_ptr p);

static int check_invariant(NuSMVEnv_ptr env,
                           BddFsm_ptr fsm,
                           Prop_ptr inv_prop,
                           Check_Strategy strategy,
                           NodeList_ptr symbols,
                           Trace_ptr* trace);

static Step_Direction forward_heuristic(DDMgr_ptr dd,
                                        bdd_ptr reachable_frontier,
                                        bdd_ptr bad_frontier,
                                        bdd_ptr reachable_states,
                                        bdd_ptr bad_states,
                                        int turn);

static Step_Direction backward_heuristic(DDMgr_ptr dd,
                                         bdd_ptr reachable_frontier,
                                         bdd_ptr bad_frontier,
                                         bdd_ptr reachable_states,
                                         bdd_ptr bad_states,
                                         int turn);

static Step_Direction forward_backward_heuristic(DDMgr_ptr dd,
                                                 bdd_ptr reachable_frontier,
                                                 bdd_ptr bad_frontier,
                                                 bdd_ptr reachable_states,
                                                 bdd_ptr bad_states,
                                                 int turn);

static boolean stopping_heuristic(DDMgr_ptr dd,
                                  bdd_ptr reachable_frontier,
                                  bdd_ptr bad_frontier,
                                  bdd_ptr reachable_states,
                                  bdd_ptr bad_states,
                                  int turn);

static boolean never_stopping_heuristic(DDMgr_ptr dd,
                                        bdd_ptr reachable_frontier,
                                        bdd_ptr bad_frontier,
                                        bdd_ptr reachable_states,
                                        bdd_ptr bad_states,
                                        int turn);

static Trace_ptr complete_bmc_trace_with_bdd(Trace_ptr* trace,
                                             NodeList_ptr symbols,
                                             BddEnc_ptr bdd_enc,
                                             BddFsm_ptr bdd_fsm,
                                             node_ptr f_list,
                                             node_ptr b_list);

static Trace_ptr compute_path_fb(BddFsm_ptr fsm,
                                 bdd_ptr target_states,
                                 node_ptr f_list,
                                 node_ptr b_list,
                                 NodeList_ptr symbols);

static Trace_ptr compute_and_complete_path(BddFsm_ptr fsm,
                                           bdd_ptr start_fw_state,
                                           bdd_ptr start_bw_state,
                                           node_ptr f_list,
                                           node_ptr b_list,
                                           NodeList_ptr symbols,
                                           Trace_ptr* middle_trace);

static int
check_invariant_forward_backward_with_break(NuSMVEnv_ptr env,
                                            BddFsm_ptr fsm,
                                            Prop_ptr inv_prop,
                                            heuristic_type heuristic,
                                            stopping_heuristic_type stopping_h,
                                            NodeList_ptr symbols,
                                            Trace_ptr* output_trace);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Mc_CheckInvar(NuSMVEnv_ptr env, Prop_ptr prop)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  Mc_CheckInvar_With_Strategy(env, prop, opt_check_invar_strategy(opts),
                              (Trace_ptr*)NULL, false);
}

void Mc_CheckInvarSilently(NuSMVEnv_ptr env, Prop_ptr prop, Trace_ptr* trace)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  boolean old_ce;

  if ((Trace_ptr*)NULL == trace) {
    /* Disable counter examples */
    old_ce = opt_counter_examples(opts);
    unset_counter_examples(opts);
  }

  Mc_CheckInvar_With_Strategy(env, prop, opt_check_invar_strategy(
                                              opts),
                              trace, true);

  if (((Trace_ptr*)NULL == trace) && old_ce) {
    set_counter_examples(opts);
  }
}

void Mc_CheckInvar_With_Strategy(NuSMVEnv_ptr env,
                                 Prop_ptr prop,
                                 Check_Strategy strategy,
                                 Trace_ptr* output_trace,
                                 boolean silent)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  /* needed for trace lanugage */
  NodeList_ptr symbols;
  SexpFsm_ptr sexp_fsm;

  if (opt_cone_of_influence(opts)) {
    /* This will be computed in the same way later, so this is NOT as
       doing the same thing twice. In this case, we use the generated
       scalar sexp fsm to retrieve the set of symbols of COI */
    (void)Prop_compute_ground_bdd_fsm(env, prop);
    sexp_fsm = Prop_get_scalar_sexp_fsm(prop);
    SEXP_FSM_CHECK_INSTANCE(sexp_fsm);
  }
  else {
    sexp_fsm = Prop_get_scalar_sexp_fsm(prop);
    if (SEXP_FSM(NULL) == sexp_fsm) {
      sexp_fsm = SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));
      SEXP_FSM_CHECK_INSTANCE(sexp_fsm);
    }
  }

  symbols = SexpFsm_get_symbols_list(sexp_fsm);

  Mc_CheckInvar_With_Strategy_And_Symbols(env, prop, strategy, output_trace, silent,
                                          symbols);
} /* Mc_CheckInvar_With_Strategy */

void Mc_CheckInvar_With_Strategy_And_Symbols(NuSMVEnv_ptr env,
                                             Prop_ptr prop,
                                             Check_Strategy strategy,
                                             Trace_ptr* output_trace,
                                             boolean silent,
                                             NodeList_ptr symbols)
{
  const OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  SymbTable_ptr symb_table = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
  FsmBuilder_ptr builder = FSM_BUILDER(NuSMVEnv_get_value(env, ENV_FSM_BUILDER));
  BddFsm_ptr fsm = BDD_FSM(NULL);
  Trace_ptr trace = TRACE(NULL);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "evaluating ");
    print_invar(Logger_get_ostream(logger), prop, get_prop_print_method(opts));
    Logger_log(logger, "\n");
  }

  fsm = Prop_compute_ground_bdd_fsm(env, prop);
  BDD_FSM_CHECK_INSTANCE(fsm);

  /* ---------- ALL REACHABLE STATES OF THE FSM ARE AVAILABLE ---------- */
  /* If we have all reachable states, but no onions, it is possible
     to check if an invariant is true or false very simply. */
  if (BddFsm_has_cached_reachable_states(fsm) &&
      (!BddFsm_reachable_states_computed(fsm))) {
    BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);
    DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
    BddStates reachable = BddFsm_get_reachable_states(fsm);
    bdd_ptr prop_bdd = BddEnc_expr_to_bdd(enc, Prop_get_expr_core(prop), Nil);
    boolean prop_holds;

    if (Prop_needs_rewriting(symb_table, prop)) {
      bdd_ptr not_prop = bdd_not(dd, prop_bdd);
      bdd_ptr tmp = BddTrans_get_backward_image_state(BddFsm_get_trans(fsm),
                                                      not_prop);
      bdd_free(dd, not_prop);

      prop_holds = !bdd_intersected(dd, reachable, tmp);
      bdd_free(dd, tmp);
    }
    else {
      prop_holds = bdd_entailed(dd, reachable, prop_bdd);
    }

    bdd_free(dd, reachable);
    bdd_free(dd, prop_bdd);

    if (prop_holds) {
      Prop_set_status(prop, Prop_True);
    }
    else {
      /* [AMa] It should be possible to construct the counter-example
      for the given property, having the DAG structure of guided
      reachability, built when applying the scenario.
      This has to be discussed */
      Prop_set_status(prop, Prop_False);
    }

    if (!silent) {
      print_result(env, prop);
    }

    return;
  }
  /* ---------- -------------------- ----------*/

  {
    check_invariant(env, Prop_get_bdd_fsm(prop), prop, strategy,
                    symbols, &trace);

    /* Store the trace if needed or return it to upper level if requested */
    if (opt_counter_examples(opts) && TRACE(NULL) != trace) {
      if ((Trace_ptr*)NULL != output_trace) {
        *output_trace = trace;
      }
      else {
        /* Set the trace in the property */
        int tr;
        TraceMgr_ptr tm = TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));

        tr = TraceMgr_register_trace(tm, trace);
        Prop_set_trace(prop, tr+1);
      }
    }

    if (!silent) {
      print_result(env, prop);
    }
  }
}

/*!
  \brief Generates a counterexample from a path forward and a
   path backward


*/

static Trace_ptr compute_path_fb (BddFsm_ptr fsm,
                                  bdd_ptr target_states,
                                  node_ptr f_list,
                                  node_ptr b_list,
                                  NodeList_ptr symbols)
{
  bdd_ptr bad_state;
  BddEnc_ptr enc;
  DDMgr_ptr dd;
  Trace_ptr res;
  Trace_ptr empty = TRACE(NULL);

  /* Get needed encoders */
  enc = BddFsm_get_bdd_encoding(fsm);
  dd = BddEnc_get_dd_manager(enc);

  /* Choose a target state: it is in common between forward and
     backward paths */
  bad_state = BddEnc_pick_one_state(enc, target_states);

  /* Use the ''compute_and_complete_path'' without asking any
     completions */
  res = compute_and_complete_path(fsm,
                                  bad_state,
                                  bad_state,
                                  f_list,
                                  b_list,
                                  symbols,
                                  &empty);

  /* Cleanup */
  bdd_free(dd, bad_state);

  return res;
}


/*!
  \brief Generates a counterexample from a path forward and a
   path backward completing the two parts with the specified middle trace if
   needed


*/

static Trace_ptr
compute_and_complete_path (BddFsm_ptr fsm, bdd_ptr start_fw_state,
                           bdd_ptr start_bw_state, node_ptr f_list,
                           node_ptr b_list, NodeList_ptr symbols,
                           Trace_ptr* middle_trace)
{
  node_ptr fpath, bpath;
  bdd_ptr state;
  BddEnc_ptr enc;
  DDMgr_ptr dd;
  Trace_ptr ftrace, btrace;
  NuSMVEnv_ptr env;
  ErrorMgr_ptr errmgr;
  NodeMgr_ptr nodemgr;

  /* Get the encoder */
  enc = BddFsm_get_bdd_encoding(fsm);
  dd = BddEnc_get_dd_manager(enc);
  env = EnvObject_get_environment(ENV_OBJECT(enc));
  errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  /*********************
   *   BACKWARD PATH   *
   *********************/

  /* Backward path starts with ''start_bw_state'' */
  state = bdd_dup(start_bw_state);

  if (Nil != b_list) {
    bpath = cons(nodemgr, (node_ptr) bdd_dup(start_bw_state), Nil);
    /* Skip the first element of b_list*/
    b_list = cdr(b_list);
  }
  else {
    bpath = Nil;
  }

  /* for every backward step */
  while(Nil != b_list) {
    bdd_ptr intersection, image;
    bdd_ptr inputs, input, state_next;

    /* The element must be a valid reachable step */
    if (car(b_list) == Nil) ErrorMgr_internal_error(errmgr, "b_list == Nil");

    /* Get the forward image of the current state and compute its intersection
       with the reachable step in the ''b_list'' list */
    image = BddFsm_get_forward_image(fsm, state);
    intersection = bdd_and(dd, image, (bdd_ptr) car(b_list));

    /* The intersection MUST exist */
    if (bdd_is_false(dd, intersection)) {
      bdd_free(dd, image);
      bdd_free(dd, state);
      bdd_free(dd, intersection);
      ErrorMgr_internal_error(errmgr, "Intersection == emptyset");
    }

    /* Pick one stete in the nexts */
    state_next = BddEnc_pick_one_state(enc, intersection);

    /* appends (input, state) pair to the path */
    inputs = BddFsm_states_to_states_get_inputs(fsm, state, state_next);
    input = BddEnc_pick_one_input(enc, inputs);

    /* Append input to the path */
    bpath = cons(nodemgr, (node_ptr) bdd_dup(input), bpath);

    /* Free inputs and current state */
    bdd_free(dd, inputs);
    bdd_free(dd, state);
    bdd_free(dd, input);

    /* Append next state to the path */
    bpath = cons(nodemgr, (node_ptr) bdd_dup(state_next), bpath);

    /* Set state to the next for the successive cycle */
    state = state_next;

    /* Cleanup */
    bdd_free(dd, image);
    bdd_free(dd, intersection);

    b_list = cdr(b_list);
  }

  /* Reverse path: currently bpath contains the backward trace, but in reverse
     order, so we have to reverse the whole list */
  bpath = reverse(bpath);

  /* Free state bdd */
  bdd_free(dd, state);

  /*********************
   *    FORWARD PATH   *
   *********************/

  /* get the start state */
  fpath = cons(nodemgr, (node_ptr) bdd_dup(start_fw_state), Nil);

  /* Forward path start  */
  state = bdd_dup(start_fw_state);

  /* Skip the first element */
  if (Nil != f_list) {
    f_list = cdr(f_list);
  }

  /* for every forward step */
  while(Nil != f_list) {
    bdd_ptr intersection, image;
    bdd_ptr inputs, input, state_prev;

    /* The element must be a valid reachable step */
    if (car(f_list) == Nil) ErrorMgr_internal_error(errmgr, "f_list == Nil");

    /* Get the backward image of the current state and compute its
       intersection with the reachable step in the ''f_list'' list */
    image = BddFsm_get_backward_image(fsm, state);
    intersection = bdd_and(dd, image, (bdd_ptr) car(f_list));

    /* The intersection MUST exist */
    if (bdd_is_false(dd, intersection)) {
      bdd_free(dd, image);
      bdd_free(dd, state);
      bdd_free(dd, intersection);
      ErrorMgr_internal_error(errmgr, "Intersection == emptyset");
    }

    /* Pich a state in the valid backward image */
    state_prev = BddEnc_pick_one_state(enc, intersection);

    /* appends (input, state) pair */
    inputs = BddFsm_states_to_states_get_inputs(fsm, state_prev, state);
    input = BddEnc_pick_one_input(enc, inputs);

    /* Append input to the path */
    fpath = cons(nodemgr, (node_ptr) bdd_dup(input), fpath);

    /* Free inputs and current state */
    bdd_free(dd, inputs);
    bdd_free(dd, state);
    bdd_free(dd, input);

    /* Append previous state to the path */
    fpath = cons(nodemgr, (node_ptr) bdd_dup(state_prev), fpath);

    /* Set state for the next cycle */
    state = state_prev;

    /* Cleanup */
    bdd_free(dd, image);
    bdd_free(dd, intersection);

    f_list = cdr(f_list);
  }

  /* Free state bdd */
  bdd_free(dd, state);

  /*********************
   *       MERGE       *
   *********************/

  /* Create the traces from the forward and backward paths */
  ftrace = Mc_create_trace_from_bdd_state_input_list(enc, symbols,
              "AG alpha Counterexample", TRACE_TYPE_CNTEXAMPLE, fpath);

  btrace = Mc_create_trace_from_bdd_state_input_list(enc, symbols,
              "AG alpha Counterexample", TRACE_TYPE_CNTEXAMPLE, bpath);

  /* concatenation is destructive on rhs argument (see Trace_concat) */
  if (TRACE(NULL) != ftrace) {
    (void)Trace_concat(ftrace, middle_trace);
    nusmv_assert(TRACE(NULL) == *middle_trace);
  } else ftrace = *middle_trace;

  if (TRACE(NULL) != ftrace) {
    Trace_concat(ftrace, &btrace);
    nusmv_assert(TRACE(NULL) == btrace);
  } else ftrace = btrace;

  /* Cleanup */
  walk_dd(dd, bdd_free, fpath);
  walk_dd(dd, bdd_free, bpath);
  free_list(nodemgr, fpath);
  free_list(nodemgr, bpath);

  return ftrace;
}

/*!
  \brief Performs on the fly verification of the
   invariant during reachability analysis.

  During the computation of reachable states it
   checks invariants. If the invariant is not satisfied, then an
   execution trace leading to a state not satisfing the invariant is
   printed out.

   NOTE: returns 0 if the property is false, 1 if it is true, 2 if BMC
   was not able to solve the problem

   If opt_counter_examples is setted and trace is not null, then a
   trace is stored (and must be released by caller) in trace
   parameter location.

   The result of model checking is stored in the given property.

*/
static int
check_invariant_forward_backward_with_break(NuSMVEnv_ptr env,
                                            BddFsm_ptr fsm,
                                            Prop_ptr inv_prop,
                                            heuristic_type heuristic,
                                            stopping_heuristic_type stopping_h,
                                            NodeList_ptr symbols,
                                            Trace_ptr* output_trace)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  int result, turn;
  bdd_ptr last_reachable_states, last_bad_states;
  int backwardStep, forwardStep;
  node_ptr forward_reachable_list, backward_reachable_list;
  bdd_ptr invar_bdd, init_bdd;
  bdd_ptr target_states, bad_frontier, reachable_frontier, tmp;
  Expr_ptr inv_expr = NULL;
  BddEnc_ptr enc = NULL;
  DDMgr_ptr dd = NULL;

  int diameter;
  boolean completed;
  BddStates* layers;

  bdd_ptr buggy;
  Prop_ptr inputprop = inv_prop;
  Prop_Rewriter_ptr rewriter = NULL;
  BeFsm_ptr model_fsm = NULL;
  BoolSexpFsm_ptr bool_fsm = NULL;

  FsmType fsm_type = 0;

  enc = BddFsm_get_bdd_encoding(fsm);

  /* if we could go bmc, we must ask the rewriter to prepare also the be fsm */
  if (stopping_h == stopping_heuristic) {
    model_fsm = Prop_compute_ground_be_fsm(env, inv_prop);

    fsm_type = FSM_TYPE_BDD | FSM_TYPE_BE;
  }
  else fsm_type = FSM_TYPE_BDD;

  rewriter = Prop_Rewriter_create(env, inv_prop,
                                  WFF_REWRITE_METHOD_DEADLOCK_FREE,
                                  WFF_REWRITER_REWRITE_INPUT_NEXT,
                                  fsm_type, enc);
  inv_prop = Prop_Rewriter_rewrite(rewriter);

  fsm = Prop_get_bdd_fsm(inv_prop);
  model_fsm = Prop_get_be_fsm(inv_prop);
  bool_fsm = Prop_get_bool_sexp_fsm(inv_prop);

  inv_expr = Prop_get_expr_core(inv_prop);
  dd = BddEnc_get_dd_manager(enc);

  result = 0;
  backwardStep = 0;
  forwardStep = 0;

  /* Initilaize lists */
  forward_reachable_list = Nil;
  backward_reachable_list = Nil;

  /* the invariants for this FSM */
  invar_bdd = BddFsm_get_state_constraints(fsm);

  /* the initial state */
  init_bdd = BddFsm_get_init(fsm);

  /* Retrive the chached reachable states if any */
  completed = BddFsm_get_cached_reachable_states(fsm, &layers, &diameter);
  if (diameter > 0) {
    /* We have at least one state cached */

    /* Build the list of layers */
    int i;
    for (i=0; i< diameter; i++) {
      forward_reachable_list = cons(nodemgr, (node_ptr) bdd_dup(layers[i]),
                                    forward_reachable_list);
    }

    /* Update nstep variable, because the possible analysis will not
       start from the beginning but from the cached size */
    forwardStep = diameter - 1;

    /* Reachable states calculated right now is exactly the last layer */
    last_reachable_states = bdd_dup(layers[diameter - 1]);

    if (diameter > 1) {
      /*
        Compute the last frontier as difference between the last and the
        second last layer
      */
      bdd_ptr neg;
      neg = bdd_not(dd, layers[diameter - 2]);
      reachable_frontier = bdd_and(dd, last_reachable_states, neg);
      bdd_free(dd, neg);
    }
    else {
      /* The initial reachable frontier is the init state itself */
      reachable_frontier = bdd_dup(last_reachable_states);
    }

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,"Cached information found\n");
      Logger_log(logger,
              "Size of current forward frontier states: %d bdd nodes\n",
              bdd_size(dd, reachable_frontier));
      Logger_log(logger,"Cached diameter: %d\n", diameter);
    }
  }
  else {
    /* We must start the reachability analysis from the beginning */
    forwardStep = 0;

    /* reset diameter */
    diameter = 0;

    /* the first frontier is the init states intersected the invar conditions */
    last_reachable_states = bdd_and(dd, init_bdd, invar_bdd);

    /* The initial reachable frontier is the init state itself */
    reachable_frontier = bdd_dup(last_reachable_states);

    /* Add the initial layer in the layers list */
    forward_reachable_list = cons(nodemgr, (node_ptr) bdd_dup(last_reachable_states),
                                  Nil);

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,"No cached information found.\n");
      Logger_log(logger,
              "Size of initial forward frontier states: %d bdd nodes\n",
              bdd_size(dd, reachable_frontier));
    }
  }

  /* Compute the bad states negating the property */
  tmp = BddEnc_expr_to_bdd(enc, inv_expr, Nil);
  last_bad_states = bdd_not(dd, tmp);
  bdd_free(dd, tmp);
  /* The bad states have to be intersected with the invariants */
  bdd_and_accumulate(dd, &last_bad_states, invar_bdd);

  /* Append reachable states to the layers list */
  backward_reachable_list = cons(nodemgr, (node_ptr) bdd_dup(last_bad_states),
                                 backward_reachable_list);

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,"Size of bad states: %d bdd nodes\n",
            bdd_size(dd, last_bad_states));
  }

  /* The initial bad frontier is the bad set itself */
  bad_frontier = bdd_dup(last_bad_states);

  /* Target states of this level are goal_states intersected the last frontier*/
  target_states = bdd_and(dd, last_bad_states, last_reachable_states);

  /* check if there is a bug in cached states or in initial state */
  buggy = bdd_and(dd, last_reachable_states, last_bad_states);
  if (bdd_isnot_false(dd, buggy)) {
    /* There is a bug, so no further analysis is required */
    int pos, upper, lower, i;
    boolean state_buggy, previous_state_safe;
    bdd_ptr buggy_first, init_state;

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,"Bug found in initial state or in cached states\n");
    }

    /* Check if the bug is in the initial state */
    init_state = bdd_and(dd, init_bdd, invar_bdd);
    buggy_first = bdd_and(dd, init_state, last_bad_states);
    if(bdd_isnot_false(dd, buggy_first)) {
      node_ptr path;

      if (opt_verbose_level_gt(opts, 1)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger,
                "Bug found in initial state, terminating analysis.\n");
      }

      /* The path is composed only by the initial state */
      path = cons(nodemgr, (node_ptr)bdd_dup(init_state), Nil);

      /* Get the counter example if needed */
      if (opt_counter_examples(opts)) {
        *output_trace = compute_path_fb(fsm, buggy_first, path, Nil, symbols);
      }

      /* Update property fields */
      Prop_set_status(inv_prop, Prop_False);
      result = 0;

      /* If the bug was in initial state and we are not using cache,
         then cache the result */
      if (diameter <= 0) {
        /* Save the progress for further reachability analysis
           NOTE: this function frees path list */
        BddFsm_update_cached_reachable_states(fsm, path, 1, false);
      }
      else {
        /* Free the path list */
        walk_dd(dd, bdd_free, path);
        free_list(nodemgr, path);
      }
    }
    else {
      /* We have a bug in a state different from the first one, so
         we must find the first frontier which intersects the buggy
         area */

      if (opt_verbose_level_gt(opts, 1)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger,"The bug is not in the initial state.\n");
      }

      /* Get the counter example if needed */
      if (opt_counter_examples(opts)) {
        if (opt_verbose_level_gt(opts, 1)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger,
                  "Starting the binary search to fin the shortest counterexample.\n");
        }

        /*
           Find the first buggy frontier with binary search:
              INVARIANTS:
                layers[upper] is always unsafe.
                layers[lower] is always safe.
                (layers[0] is safe because otherwise we would be catched by
                 the previous check)
         */
        upper = diameter - 1;
        lower = 0;
        do {

          if (2 <= (upper - lower)) {
            pos = ((upper + lower) / 2);
          }
          else {
            /* adjacency case: bug is in upper */
            nusmv_assert( 1 == (upper - lower) ) ;
            pos = upper;
          }

          bdd_free(dd, target_states);
          target_states = bdd_and(dd, layers[pos], last_bad_states);

          state_buggy = bdd_isnot_false(dd, target_states);

          tmp = bdd_and(dd, layers[pos-1], last_bad_states);
          previous_state_safe = bdd_is_false(dd, tmp);
          bdd_free(dd, tmp);

          if (state_buggy) {
            upper = pos;
          }
          else {
            lower = pos;
          }
        } while(!(state_buggy && previous_state_safe));
        /* This condition is always reached because a bug must exist */

        if (opt_verbose_level_gt(opts, 1)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger,
                  "Found that state %d is safe and state %d is buggy.\n",
                  pos-1,
                  pos);
        }

        /* Reduce the layers list length to pos where the pos-th
           frontier is the first buggy one */
        for (i=pos+1; i<diameter; i++) {
          node_ptr val = car(forward_reachable_list);
          node_ptr nodetmp;

          /* Free the stored bdd (was duplicated in insertion) */
          bdd_free(dd, (bdd_ptr) val);

          nodetmp = forward_reachable_list;
          forward_reachable_list = cdr(forward_reachable_list);
          free_node(nodemgr, nodetmp);
        }

        /* Generate counter example */
        *output_trace = compute_path_fb(fsm, target_states,
                                        forward_reachable_list, Nil, symbols);
      }

      /* Update property fields */
      Prop_set_status(inv_prop, Prop_False);
      result = 0;
    }

    /* Cleanup */
    bdd_free(dd, buggy_first);
    bdd_free(dd, init_state);

    /* Free the list */
    walk_dd(dd, bdd_free, forward_reachable_list);
    free_list(nodemgr, forward_reachable_list);
  }
  else {
    if (completed) {
      /* We have the complete reachable states so we can skip reachability
         analysis */

      /* In the complete analysis there is no intersection, so there
         is no bugs */

      if (opt_verbose_level_gt(opts, 1)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger,
                "Cached analysis is complete, and there is no intersection with buggy states.\n");
      }

      /* Update property */
      Prop_set_status(inv_prop, Prop_True);
      result = 1;

      /* Free the list */
      walk_dd(dd, bdd_free, forward_reachable_list);
      free_list(nodemgr, forward_reachable_list);
    }
    else {
      /* The analysis was not completed and the bug is not in cached
         states so we have to resume */

      if (opt_verbose_level_gt(opts, 1)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger,
                "Starting forward-backward reachability analysis.\n");
      }

      /*
        Terminate if there are no more reachable states
        or there are no more bad_states
        or we found a bug (target_states != 0)
      */
      turn = 0;
      while (bdd_isnot_false(dd, bad_frontier) &&
             bdd_isnot_false(dd, reachable_frontier) &&
             bdd_is_false(dd, target_states)) {
        if (opt_verbose_level_gt(opts, 1)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, "Begin of turn %d.\n", turn);
        }

        /*Ask heuristic if it is time to switch to BMC... */
        if (stopping_h(dd,
                       bad_frontier,
                       reachable_frontier,
                       last_reachable_states,
                       last_bad_states,
                       turn)) {
          /* The answer is NO: we have to continue the BDD search */

          /* Ask heuristic function for the direction to follow */
          if (FORWARD_STEP == heuristic(dd,
                                        bad_frontier,
                                        reachable_frontier,
                                        last_reachable_states,
                                        last_bad_states,
                                        turn)) {
            bdd_ptr image, f_prev_reachable, not_f_prev_reachable;

            f_prev_reachable = bdd_dup(last_reachable_states);

            /* Generate forward image (NOTE: the generated image is
               already intersected with invars) */
            image = BddFsm_get_forward_image(fsm, reachable_frontier);

            /* Increment reachable states */
            bdd_or_accumulate(dd, &last_reachable_states, image);

            /* Cleanup image */
            bdd_free(dd, image);

            /* Get the complement of reachable states in previous cycle */
            not_f_prev_reachable = bdd_not(dd, f_prev_reachable);

            /* Update the current frontier */
            bdd_free(dd, reachable_frontier);
            reachable_frontier = bdd_and(dd,
                                         last_reachable_states,
                                         not_f_prev_reachable);

            /* release the negation */
            bdd_free(dd, not_f_prev_reachable);

            /* Append reachable states to the layers list */
            forward_reachable_list =cons(nodemgr, (node_ptr)bdd_dup(last_reachable_states),
                                         forward_reachable_list);

            /* Free the targets since they are no more significative
               (the frontier changed) */
            bdd_free(dd, target_states);

            /* Create the new targets */
            target_states = bdd_and(dd, last_bad_states, reachable_frontier);

            /* Free old reachables before new cycle */
            bdd_free(dd, f_prev_reachable);

            /* Increment the forward step number */
            forwardStep++;

            if (opt_verbose_level_gt(opts, 1)) {
              Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
              Logger_log(logger, "Performed forward step.\n");
              Logger_log(logger, "New diameter: %d.\n", forwardStep);
              Logger_log(logger,
                      "New frontier size: %d.\n",
                      bdd_size(dd, reachable_frontier));
            }
          }
          else {
            bdd_ptr image, f_prev_bad, not_f_prev_bad;

            f_prev_bad = bdd_dup(last_bad_states);

            /* Generate backward image (NOTE: the generated image is
               already intersected with invars) */
            image = BddFsm_get_backward_image(fsm, bad_frontier);

            /* Increment reachable states */
            bdd_or_accumulate(dd, &last_bad_states, image);

            /* Cleanup image */
            bdd_free(dd, image);

            /* Get the complement of bad states in previous cycle */
            not_f_prev_bad = bdd_not(dd, f_prev_bad);

            /* Update the current frontier */
            bdd_free(dd, bad_frontier);
            bad_frontier = bdd_and(dd, last_bad_states, not_f_prev_bad);

            /* release the negation */
            bdd_free(dd, not_f_prev_bad);

            /* Append reachable states to the layers list */
            backward_reachable_list = cons(nodemgr, (node_ptr) bdd_dup(last_bad_states),
                                           backward_reachable_list);

            /* Free the targets since they are no more significative
               (the frontier changed) */
            bdd_free(dd, target_states);

            /* Create the new targets */
            target_states = bdd_and(dd, last_bad_states, last_reachable_states);

            /* Free old reachables before new cycle */
            bdd_free(dd, f_prev_bad);

            /* Increment the step number */
            backwardStep++;

            if (opt_verbose_level_gt(opts, 1)) {
              Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
              Logger_log(logger, "Performed backward step.\n");
              Logger_log(logger, "New bug diameter: %d.\n", backwardStep);
              Logger_log(logger,
                      "New bug frontier size: %d.\n",
                      bdd_size(dd, bad_frontier));
            }
          }
        }
        else {
          /* Heuristic decided to continue with BMC so we have to
             convert everything in BE and build a BMC problem with
             the current frontiers. */
          BeFsm_ptr new_fsm;
          BeEnc_ptr be_enc;
          BddEnc_ptr bdd_enc;
          Be_Manager_ptr manager;
          node_ptr reachables, bads, binvarspec;
          be_ptr be_reachables, be_bads, be_invars, be_init;
          bdd_ptr bdd_not_bad_frontier;

          Bmc_result res;
          Trace_ptr trace = TRACE(NULL);
          int max_k;
          char* algorithm_name;

          /* Obtain the k value from options */
          max_k = get_bmc_pb_length(opts);

          /* Get bdd encoder */
          bdd_enc = BddFsm_get_bdd_encoding(fsm);

          /* Get the be encoder */
          be_enc = BeFsm_get_be_encoding(model_fsm);
          manager = BeEnc_get_be_manager(be_enc);

          if (forwardStep > 1) {
            /* We have explored some space forward so we will exclude it from
               BMC search */
            reachables = BddEnc_bdd_to_expr(bdd_enc,
                                            (bdd_ptr)car(cdr(forward_reachable_list)));
          }
          else {
            /* No space explored, so nothing to exclude */
            reachables = ExprMgr_false(exprs);
          }

          if (backwardStep > 1) {
            /* We have explored some space backward so we will exclude it from
               BMC search */
            bads = BddEnc_bdd_to_expr(bdd_enc,
                                      bdd_dup((bdd_ptr) car(cdr(backward_reachable_list))));
          }
          else {
            /* No space explored, so nothing to exclude */
            bads = ExprMgr_false(exprs);
          }

          /* Transform explored space in Be */
          be_reachables = Bmc_Conv_Bexp2Be(be_enc, reachables);
          be_bads = Bmc_Conv_Bexp2Be(be_enc, bads);

          /* Build invars [invars = model_invars && !explored_space] */
          be_invars = Be_And(manager,
                             Be_Not(manager, be_reachables),
                             Be_Not(manager, be_bads));
          be_invars = Be_And(manager, be_invars, BeFsm_get_invar(model_fsm));

          /* Calculate new inits */
          be_init = Bmc_Conv_Bexp2Be(be_enc,
                                     BddEnc_bdd_to_expr(bdd_enc, reachable_frontier));

          /* Accumulate invars in INIT state */
          be_init = Be_And(manager, be_invars, be_init);

          /* Build new FSM: Since this inherits from the BoolSexpFsm,
             it contains the input and state mask constraints. */
          new_fsm = BeFsm_create(be_enc,
                                 be_init,
                                 be_invars,
                                 BeFsm_get_trans(model_fsm),
                                 BeFsm_get_fairness_list(model_fsm));

          /* Compute booleanized, negated and NNFed formula */
          bdd_not_bad_frontier = bdd_not(dd, bad_frontier);
          binvarspec = Wff2Nnf(env, BddEnc_bdd_to_expr(bdd_enc,
                                                  bdd_not_bad_frontier));
          bdd_free(dd, bdd_not_bad_frontier);


          /* BMC algorithm call: the algorithm to be used is decided
             by the ''bmc_invar_alg'' system variable */
          algorithm_name = util_strsav((char*) get_bmc_invar_alg(opts));

          if (strcasecmp(algorithm_name, BMC_INVAR_ALG_CLASSIC) == 0) {
            res = Bmc_induction_algorithm(env,
                                          new_fsm,
                                          binvarspec,
                                          &trace,
                                          symbols);
          }
          else if (strcasecmp(algorithm_name, BMC_INVAR_ALG_EEN_SORENSSON) == 0) {
            res = Bmc_een_sorensson_algorithm_without_dump(env,
                                                           new_fsm,
                                                           bool_fsm,
                                                           binvarspec,
                                                           max_k,
                                                           false,
                                                           &trace);
          }
          else {
            StreamMgr_print_error(streams,
                    "'%s' is an invalid algorithm for BDD-BMC approach.\n",
                    algorithm_name);
            StreamMgr_print_error(streams,  "Valid names are '%s' and '%s'.\n",
                    BMC_INVAR_ALG_CLASSIC, BMC_INVAR_ALG_EEN_SORENSSON);
            res = BMC_ERROR;
          }
          FREE(algorithm_name);

          /* The newly built FSM is now useless */
          BeFsm_destroy(new_fsm);

          /* Report results */
          switch (res) {
          case BMC_TRUE:
            Prop_set_status(inv_prop, Prop_True);
            result = 1;
            break;

          case BMC_UNKNOWN:
            {
              int bmc_tr;
              if (opt_counter_examples(opts) && \
                  TRACE(NULL) != trace) { /* register trace (if applicable) */
                bmc_tr = TraceMgr_register_trace(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                                                     trace);
                Prop_set_trace(inv_prop, bmc_tr+1);
              }

              Prop_set_status(inv_prop, Prop_Unchecked);
              result = 2;
              break;
            }

          case BMC_FALSE:
            {
              if (opt_counter_examples(opts)) {

                /* 'trace' is used as the middle fragment, thus we can
                   make no assumptions on it. (i.e. it may or may not
                   be NULL, depending on other fragments. */
                *output_trace = complete_bmc_trace_with_bdd(&trace, symbols,
                                                           bdd_enc,
                                                           fsm,
                                                           forward_reachable_list,
                                                           backward_reachable_list);
              }

              Prop_set_status(inv_prop, Prop_False);
              result = 0;
              break;
            }

          case BMC_ERROR:
            ErrorMgr_rpterr(errmgr, "Error in BMC check!");

          default:
            ErrorMgr_rpterr(errmgr, "Unexpected result in BMC check!");
          }

          /* Update cache if possible */
          if (forwardStep > diameter) {
            /* Cache the analysis
               NOTE: this function frees forward_reachable_list */
            BddFsm_update_cached_reachable_states(fsm,
                                                  forward_reachable_list,
                                                  forwardStep+1,
                                                  false);
          }
          else {
            /* Free the forward list */
            walk_dd(dd, bdd_free, forward_reachable_list);
            free_list(nodemgr, forward_reachable_list);
          }

          /* Free the backward list */
          walk_dd(dd, bdd_free, backward_reachable_list);
          free_list(nodemgr, backward_reachable_list);

          /* Free used bdds */
          bdd_free(dd, buggy);
          bdd_free(dd, target_states);
          bdd_free(dd, bad_frontier);
          bdd_free(dd, reachable_frontier);
          bdd_free(dd, last_reachable_states);
          bdd_free(dd, last_bad_states);
          bdd_free(dd, invar_bdd);
          bdd_free(dd, init_bdd);

          Prop_Rewriter_update_original_property(rewriter);
          Prop_Rewriter_destroy(rewriter); rewriter = NULL;

          return result;
        }

        turn ++;
      }/* end while */

      /*
        If the exit condition was caused by target_states
        reachability then the property is false
      */
      if (bdd_isnot_false(dd, target_states)) {
        /* Set the status */
        Prop_set_status(inv_prop, Prop_False);

        /* Compute the counter example */
        if (opt_counter_examples(opts)) {
          *output_trace = compute_path_fb(fsm,
                                         target_states,
                                         forward_reachable_list,
                                          backward_reachable_list,
                                          symbols);
        }

        /* The property is false */
        result = 0;
      } else {
        /* The property is true because we completed the reachability
           analysis and we did not found a bug */
        Prop_set_status(inv_prop, Prop_True);
        result = 1;
      }

      /* We added new states, so we have to cache them */
      if (forwardStep > diameter) {
        if (bdd_is_false(dd, reachable_frontier)) {
          /* We completed the forward analysis */
          /* Cache the analysis
             NOTE: this function frees plan_reach_list */
          BddFsm_update_cached_reachable_states(fsm,
                                                forward_reachable_list,
                                                forwardStep+1,
                                                true);
        }
        else {
          /* We did not complete the forward analysis but we have new states*/
          /* Cache the analysis
             NOTE: this function frees plan_reach_list */
          BddFsm_update_cached_reachable_states(fsm,
                                                forward_reachable_list,
                                                forwardStep+1,
                                                false);
        }
      }
      else {
        /* We have not added any new state */

        /* Free the forward list */
        walk_dd(dd, bdd_free, forward_reachable_list);
        free_list(nodemgr, forward_reachable_list);
      }
    }
  }

  /* Free the backward list */
  walk_dd(dd, bdd_free, backward_reachable_list);
  free_list(nodemgr, backward_reachable_list);

  /* Free used bdds */
  bdd_free(dd, buggy);
  bdd_free(dd, target_states);
  bdd_free(dd, bad_frontier);
  bdd_free(dd, reachable_frontier);
  bdd_free(dd, last_reachable_states);
  bdd_free(dd, last_bad_states);
  bdd_free(dd, invar_bdd);
  bdd_free(dd, init_bdd);

  Prop_Rewriter_update_original_property(rewriter);
  Prop_Rewriter_destroy(rewriter); rewriter = NULL;

  return result;
}

void print_invar(OStream_ptr file, Prop_ptr p, Prop_PrintFmt fmt)
{
  OStream_printf(file, "invariant ");
  Prop_print(p, file, fmt);
}

/*!
  \brief Prints the result of the check if the check was performed,
   does nothing otherwise

  Print an invariant specification check result
*/
static void print_result(NuSMVEnv_ptr env, Prop_ptr p)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  TraceMgr_ptr tm = TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  StreamMgr_print_output(streams,  "-- ");
  print_invar(StreamMgr_get_output_ostream(streams),
              p, get_prop_print_method(opts));

  if (Prop_False == Prop_get_status(p)) {
    StreamMgr_print_output(streams,  " is false\n");

    if (opt_counter_examples(opts)) {
      if (Prop_get_trace(p) > 0) {
        StreamMgr_print_output(streams,
                "-- as demonstrated by the following execution sequence\n");

        TraceMgr_execute_plugin(tm, TRACE_OPT(NULL),
                                    TRACE_MGR_DEFAULT_PLUGIN,
                                    Prop_get_trace(p)-1);
      }
      else {
        StreamMgr_print_output(streams,
                "-- Cannot build counter-example. Layers informations are not available\n");
      }
    }
  }
  else if(Prop_True == Prop_get_status(p)) {
    StreamMgr_print_output(streams,  " is true\n");
  }
  else if((Prop_Unchecked == Prop_get_status(p)) &&
          (Prop_get_trace(p) > 0)) {
    StreamMgr_print_output(streams,  "-- cannot prove the ");

    print_invar(StreamMgr_get_output_ostream(streams),
                p, get_prop_print_method(opts));

    StreamMgr_print_output(streams,  " is true or false : the induction fails\n");

    if (opt_counter_examples(opts)) {
      /* Print the trace using default plugin */
      StreamMgr_print_output(streams,
              "-- as demonstrated by the following execution sequence\n");

      TraceMgr_execute_plugin(tm, TRACE_OPT(NULL),
                                  TRACE_MGR_DEFAULT_PLUGIN,
                                  Prop_get_trace(p)-1);
    }
  }
  else {
    ErrorMgr_rpterr(errmgr, "Unknown property state!");
  }
}

/*!
  \brief Check the given invariant with the specified technology

  If opt_counter_examples is setted and trace is not
   null, then a trace is stored (and must be released by caller) in
   trace parameter location.

   The result of model checking is stored in the given property.
*/
static int check_invariant(NuSMVEnv_ptr env,
                           BddFsm_ptr fsm,
                           Prop_ptr inv_prop,
                           Check_Strategy strategy,
                           NodeList_ptr symbols,
                           Trace_ptr* trace) {

  switch (strategy) {
  case FORWARD:
    return check_invariant_forward_backward_with_break(env, fsm,
                                                       inv_prop,
                                                       forward_heuristic,
                                                       never_stopping_heuristic,
                                                       symbols, trace);

  case BACKWARD:
    return check_invariant_forward_backward_with_break(env, fsm,
                                                       inv_prop,
                                                       backward_heuristic,
                                                       never_stopping_heuristic,
                                                       symbols, trace);

  case FORWARD_BACKWARD:
    return check_invariant_forward_backward_with_break(env, fsm,
                                                       inv_prop,
                                                       forward_backward_heuristic,
                                                       never_stopping_heuristic,
                                                       symbols, trace);
  case BDD_BMC:
    return check_invariant_forward_backward_with_break(env, fsm,
                                                       inv_prop,
                                                       forward_backward_heuristic,
                                                       stopping_heuristic,
                                                       symbols, trace);
  default:
    error_unreachable_code();
  }
}


/*!
  \brief Constant function to perform forward analysis


*/

static Step_Direction forward_heuristic (DDMgr_ptr dd,
                                         bdd_ptr reachable_frontier,
                                         bdd_ptr bad_frontier,
                                         bdd_ptr reachable_states,
                                         bdd_ptr bad_states,
                                         int turn) {
  return FORWARD_STEP;
}


/*!
  \brief Constant function to perform backward analysis


*/

static Step_Direction backward_heuristic (DDMgr_ptr dd,
                                          bdd_ptr reachable_frontier,
                                          bdd_ptr bad_frontier,
                                          bdd_ptr reachable_states,
                                          bdd_ptr bad_states,
                                          int turn) {
  return BACKWARD_STEP;
}


/*!
  \brief Constant function to perform backward, forward and
   FB analysis


*/

static boolean never_stopping_heuristic (DDMgr_ptr dd,
                                         bdd_ptr reachable_frontier,
                                         bdd_ptr bad_frontier,
                                         bdd_ptr reachable_states,
                                         bdd_ptr bad_states,
                                         int turn) {
  return true;
}


/*!
  \brief Heuristic function used to decide the sept to perform
   in forward-backward analysis


*/

static Step_Direction forward_backward_heuristic (DDMgr_ptr dd,
                                                  bdd_ptr reachable_frontier,
                                                  bdd_ptr bad_frontier,
                                                  bdd_ptr reachable_states,
                                                  bdd_ptr bad_states,
                                                  int turn)
{
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  switch (opt_check_invar_fb_heuristic(opts)) {
  case ZIGZAG_HEURISTIC:
    if (turn % 2) {
      return BACKWARD_STEP;
    }
    else {
      return FORWARD_STEP;
    }

  case SMALLEST_BDD_HEURISTIC:
    if (bdd_size(dd, reachable_states) <= bdd_size(dd, bad_states)) {
      return BACKWARD_STEP;
    }
    else {
      return FORWARD_STEP;
    }

  default:
    error_unreachable_code();
  }

  return false; /* suppress warning */
}

/*!
  \brief Heuristic function used to decide whether to stop BDD
   analysis to pass to BMC.

  True means continue with BDD false means swap to BMC
*/
static boolean stopping_heuristic(DDMgr_ptr dd,
                                  bdd_ptr reachable_frontier,
                                  bdd_ptr bad_frontier,
                                  bdd_ptr reachable_states,
                                  bdd_ptr bad_states,
                                  int turn)
{
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  switch (opt_check_invar_bddbmc_heuristic(opts)) {
  case STEPS_HEURISTIC:
    return turn < opt_check_invar_bddbmc_heuristic_threshold(opts);

  case SIZE_HEURISTIC:
    return (bdd_size(dd, reachable_states) + bdd_size(dd,bad_states) <
            opt_check_invar_bddbmc_heuristic_threshold(opts));

  default:
    error_unreachable_code();
  }

  return false; /* suppress warning */
}

/*!
  \brief Completes a partial BMC tace in BDD-BMC analysis

  The free of the returned trace is demanded to the caller
*/
static Trace_ptr complete_bmc_trace_with_bdd(Trace_ptr* trace,
                                             NodeList_ptr symbols,
                                             BddEnc_ptr bdd_enc,
                                             BddFsm_ptr bdd_fsm,
                                             node_ptr f_list,
                                             node_ptr b_list) {
  DDMgr_ptr dd;
  Trace_ptr res = TRACE(NULL);

  bdd_ptr begin_bdd ;
  bdd_ptr end_bdd;

  dd = BddEnc_get_dd_manager(bdd_enc);

  begin_bdd = TraceUtils_fetch_as_bdd(*trace, Trace_first_iter(*trace),
                                      TRACE_ITER_SF_VARS, bdd_enc);

  end_bdd = TraceUtils_fetch_as_bdd(*trace, Trace_last_iter(*trace),
                                    TRACE_ITER_SF_VARS, bdd_enc);

  res = compute_and_complete_path(bdd_fsm, begin_bdd, end_bdd,
                                  f_list, b_list, symbols, trace);
  bdd_free(dd, begin_bdd);
  bdd_free(dd, end_bdd);

  return res;
}
