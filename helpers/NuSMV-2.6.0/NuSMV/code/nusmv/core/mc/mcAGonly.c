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
  \brief This file contains the code to deal with AG formulas in a
  special way.

  This file contains the code to deal with AG formulas
  only, using special purpose algorithms. This functionality is invoked
  with the -AG option and works only in conjunction with the -f
  (forward search) option.

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/mc/mc.h"
#include "nusmv/core/mc/mcInt.h"

#include "nusmv/core/node/node.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/TraceMgr.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/prop/propPkg.h"
/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static boolean check_AG_only(BddFsm_ptr fsm, BddEnc_ptr enc, Prop_ptr prop,
                             Expr_ptr spec, node_ptr context,
                             NodeList_ptr symbols,
                             Trace_ptr* out_trace);

static boolean is_AG_only_formula(node_ptr n);
static boolean is_AG_only_formula_recur(node_ptr n, int* ag_count);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Mc_CheckAGOnlySpec(NuSMVEnv_ptr env, Prop_ptr prop)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  BddFsm_ptr fsm = BDD_FSM(NULL);
  Expr_ptr spec = Prop_get_expr_core(prop);
  Trace_ptr trace = TRACE(NULL);
  BddEnc_ptr enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "evaluating ");
    print_spec(Logger_get_ostream(logger), prop, get_prop_print_method(opts));
    Logger_log(logger, "\n");
  }

  fsm = Prop_compute_ground_bdd_fsm(env, prop);
  BDD_FSM_CHECK_INSTANCE(fsm);

  if (is_AG_only_formula(spec)) {
    SexpFsm_ptr sexp_fsm; /* needed for trace language */

    sexp_fsm = Prop_get_scalar_sexp_fsm(prop);
    if (SEXP_FSM(NULL) == sexp_fsm) {
      sexp_fsm = SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));
      SEXP_FSM_CHECK_INSTANCE(sexp_fsm);
    }

    StreamMgr_print_output(streams,  "-- ");
    print_spec(StreamMgr_get_output_ostream(streams),
               prop, get_prop_print_method(opts));

    if (check_AG_only(fsm, enc, prop, spec, Nil,
                      SexpFsm_get_symbols_list(sexp_fsm), &trace)) {

      /* property is true */
      StreamMgr_print_output(streams,  "is true\n");
      Prop_set_status(prop, Prop_True);
    }
    else { /* property is false */
      StreamMgr_print_output(streams,  "is false\n");
      Prop_set_status(prop, Prop_False);

      if (TRACE(NULL) != trace) {
        /* Print the trace using default plugin */
        StreamMgr_print_output(streams, 
            "-- as demonstrated by the following execution sequence\n");

        TraceMgr_register_trace(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), trace);
        TraceMgr_execute_plugin(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), TRACE_OPT(NULL),
                                    TRACE_MGR_DEFAULT_PLUGIN,
                                    TRACE_MGR_LAST_TRACE);

        Prop_set_trace(prop, Trace_get_id(trace));

      }
    }
  }
  else {
    ErrorMgr_warning_non_ag_only_spec(errmgr, prop);
    return;
  }

} /* Mc_CheckAGOnlySpec */

node_ptr make_AG_counterexample(BddFsm_ptr fsm, BddStates target_states)
{
  BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);
  DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr counterexample = Nil;
  bdd_ptr state, dist;
  bdd_ptr tgt = bdd_dup(target_states);
  int distance;
  int i;

  distance = BddFsm_get_minimum_distance_of_states(fsm, target_states);

  /* returns an empty list if any of given target states is not reachable */
  if (distance == -1) return Nil;

  /* pushes the one state from target states (all reachable) at the end: */
  dist = BddFsm_get_reachable_states_at_distance(fsm, distance);
  bdd_and_accumulate(dd, &tgt, dist);
  bdd_free(dd, dist);

  state = BddEnc_pick_one_state(enc, tgt);
  bdd_free(dd, tgt);

  counterexample = cons(nodemgr, (node_ptr) state, counterexample);

  for (i = distance-1; i >= 0 ; --i) {
    BddStates pre_image;
    BddStates reachables;
    BddInputs inputs;
    bdd_ptr input;

    pre_image = BddFsm_get_backward_image(fsm, state);
    reachables = BddFsm_get_reachable_states_at_distance(fsm, i);

    bdd_and_accumulate(dd, &pre_image, reachables);
    bdd_free(dd, reachables);

    /* transitions from the reachable pre image to the current state at i+1 */
    inputs = BddFsm_states_to_states_get_inputs(fsm, pre_image, state);
    input = BddEnc_pick_one_input(enc, inputs);
    nusmv_assert(!bdd_is_false(dd, input));
    bdd_free(dd, inputs);
    counterexample = cons(nodemgr, (node_ptr) input, counterexample);

    state = BddEnc_pick_one_state(enc, pre_image);
    bdd_free(dd, pre_image);
    nusmv_assert(!bdd_is_false(dd, state));
    counterexample = cons(nodemgr, (node_ptr) state, counterexample);
  }

  return counterexample;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief This function checks for SPEC of the form AG alpha in
               "context".

  The implicit assumption is that "spec" must be an AG
               formula (i.e. it must contain only conjunctions and
               AG's).  No attempt is done to normalize the formula
               (e.g. push negations). The AG mode relies on the
               previous computation and storage of the reachable state
               space (<tt>reachable_states_layers</tt>), they are used
               in counterexample computation.

               Returns true iff the property is true.

  \se *out_trace contains the counterexample trace (where
               applicable)

  \sa check_ctlspec
*/
static boolean check_AG_only(BddFsm_ptr fsm, BddEnc_ptr enc, Prop_ptr prop,
                             Expr_ptr spec, node_ptr context,
                             NodeList_ptr symbols, Trace_ptr* out_trace)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  boolean res = false;

  if (spec == Nil) return false;

  switch (node_get_type(spec)) {

  case CONTEXT:
    res = check_AG_only(fsm, enc, prop, cdr(spec),
                        car(spec), symbols, out_trace);
    break;

  case AND:
    res = check_AG_only(fsm, enc, prop, car(spec),
                        context, symbols, out_trace);
    if (res) { /* lazy mc */
      res = check_AG_only(fsm, enc, prop, cdr(spec),
                          context, symbols, out_trace);
    }
    break;

  case AG:
    {
      bdd_ptr tmp_1, tmp_2, acc;
      bdd_ptr invar_bdd, reachable_bdd;
      DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
      bdd_ptr s0 = eval_ctl_spec(fsm, enc, car(spec), context);

      invar_bdd = BddFsm_get_state_constraints(fsm);
      reachable_bdd = BddFsm_get_reachable_states(fsm);

      tmp_1 = bdd_not(dd, s0);
      tmp_2 = bdd_and(dd, invar_bdd, tmp_1);
      acc = bdd_and(dd, reachable_bdd, tmp_2);

      bdd_free(dd, s0);
      bdd_free(dd, tmp_2);
      bdd_free(dd, reachable_bdd);
      bdd_free(dd, tmp_1);
      bdd_free(dd, invar_bdd);

      if (bdd_is_false(dd, acc)) {
        bdd_free(dd, acc);
        res = true;
      }
      else {
        res = false;

        if (opt_counter_examples(opts)) {
          /* build counter-example trace */
          node_ptr path = make_AG_counterexample(fsm, acc);

          nusmv_assert(NIL(Trace_ptr) != out_trace);
          (*out_trace) = Mc_create_trace_from_bdd_state_input_list(enc, symbols,
                         "AG Only counterexample", TRACE_TYPE_CNTEXAMPLE, path);

          /* free the list "path" */
          walk_dd(dd, bdd_free, path);
          free_list(nodemgr, path);

          bdd_free(dd, acc);
        }
      }

      break;
    } /* case AG */

  default:
    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));

      Logger_log(logger, "*** WARNING - ");
      print_spec(Logger_get_ostream(logger), prop, get_prop_print_method(opts));
      Logger_log(logger, "skipped: it is not an AG-only formula\n");
    }
  } /* switch */

  return res;
}

/*!
  \brief Checks if the formulas is of type AGOnly.

  returns true , if the formula is AGOnly formula.

  \sa is_AG_only_formula_recur
*/
static boolean is_AG_only_formula(node_ptr n)
{
  int ag_count = 0;
  return is_AG_only_formula_recur(n, &ag_count);
}

/*!
  \brief Recursive function that helps is_AG_only_formula.

  

  \sa is_AG_only_formula
*/
static boolean is_AG_only_formula_recur(node_ptr n, int* ag_count)
{
  if (n == Nil) return true;

  switch (node_get_type(n)) {

  case CONTEXT:
      return is_AG_only_formula_recur(cdr(n), ag_count);

  case NOT:
      return is_AG_only_formula_recur(car(n), ag_count);

  case AND:
  case OR:
  case XOR:
  case XNOR:
  case IMPLIES:
  case IFF:
    return ((is_AG_only_formula_recur(car(n), ag_count)) &&
            (is_AG_only_formula_recur(cdr(n), ag_count)));

  case EX:   /* Non-AG formula */
  case AX:
  case EF:
  case AF:
  case EG:
  case EU:
  case AU:
  case EBU:
  case ABU:
  case EBF:
  case ABF:
  case EBG:
  case ABG:
    return false;

  case AG:
    *ag_count += 1;
    if(*ag_count > 1) return false; /* More than one AG */
    return is_AG_only_formula_recur(car(n), ag_count);

  default:
    { /* for all the other cases, we can safely assume it to be AG Only formula. */
      return true;
    }
  }
}

