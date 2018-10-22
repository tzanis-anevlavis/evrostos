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
  \brief Fair CTL model checking routines.

  Fair CTL model checking routines.

*/

#include "nusmv/core/mc/mcInt.h"
#include "nusmv/core/mc/mc.h"

#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/utils_io.h"

#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/TraceMgr.h"

#include "nusmv/core/bmc/bmc.h"
#include "nusmv/core/bmc/sbmc/sbmcGen.h"

#include "nusmv/core/fsm/bdd/FairnessList.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/prop/propPkg.h"

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/



/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static BddStatesInputs
Mc_get_fair_si_subset(BddFsm_ptr fsm,
                      BddStatesInputs si);

static BddStatesInputs
Mc_fair_si_iteration(BddFsm_ptr fsm,
                     bdd_ptr states,
                     bdd_ptr subspace);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Mc_CheckCTLSpec(NuSMVEnv_ptr env, Prop_ptr prop)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  node_ptr exp;
  Trace_ptr trace;
  bdd_ptr s0, tmp_1, tmp_2;
  BddFsm_ptr fsm;
  BddEnc_ptr enc;
  DDMgr_ptr dd;
  Expr_ptr spec  = Prop_get_expr_core(prop);
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "evaluating ");
    print_spec(Logger_get_ostream(logger), prop, get_prop_print_method(opts));
    Logger_log(logger, "\n");
  }

  fsm = Prop_compute_ground_bdd_fsm(env, prop);
  enc = BddFsm_get_bdd_encoding(fsm);
  dd = BddEnc_get_dd_manager(enc);

  /* Evaluates the spec */
  s0 = eval_ctl_spec(fsm, enc, spec, Nil);

  tmp_1 = bdd_not(dd, s0);
  tmp_2 = BddFsm_get_state_constraints(fsm);
  bdd_and_accumulate(dd, &tmp_2 , tmp_1);
  bdd_free(dd, tmp_1);
  tmp_1 = BddFsm_get_fair_states(fsm);
  if (bdd_is_false(dd, tmp_1)) {
    ErrorMgr_warning_fsm_fairness_empty(errmgr);
  }
  bdd_and_accumulate(dd, &tmp_2 , tmp_1);
  bdd_free(dd, tmp_1);
  bdd_free(dd, s0);

  s0 = BddFsm_get_init(fsm);
  bdd_and_accumulate(dd, &s0, tmp_2);
  bdd_free(dd, tmp_2);

  /* Prints out the result, if not true explain. */
  StreamMgr_print_output(streams,  "-- ");
  print_spec(StreamMgr_get_output_ostream(streams),
             prop, get_prop_print_method(opts));

  if (bdd_is_false(dd, s0)) {
    StreamMgr_print_output(streams,  "is true\n");
    Prop_set_status(prop, Prop_True);
  }
  else {
    StreamMgr_print_output(streams,  "is false\n");
    Prop_set_status(prop, Prop_False);

    if (opt_counter_examples(opts)) {
      char* trace_title = NULL;
      char* trace_title_postfix = " Counterexample";

      tmp_1 = BddEnc_pick_one_state(enc, s0);
      bdd_free(dd, s0);
      s0 = bdd_dup(tmp_1);
      bdd_free(dd, tmp_1);

      exp = reverse(explain(fsm, enc, cons(nodemgr, (node_ptr) bdd_dup(s0), Nil),
                            spec, Nil));

      if (exp == Nil) {
        /* The counterexample consists of one initial state */
        exp = cons(nodemgr, (node_ptr) bdd_dup(s0), Nil);
      }

      /* The trace title depends on the property type. For example it
       is in the form "LTL Counterexample" */
      trace_title = ALLOC(char,
                          strlen(Prop_get_type_as_string(prop)) +
                          strlen(trace_title_postfix) + 1);
      nusmv_assert(trace_title != (char*) NULL);
      strcpy(trace_title, Prop_get_type_as_string(prop));
      strcat(trace_title, trace_title_postfix);

      {
        SexpFsm_ptr sexp_fsm; /* needed for trace lanugage */
        sexp_fsm = Prop_get_scalar_sexp_fsm(prop);
        if (SEXP_FSM(NULL) == sexp_fsm) {
          sexp_fsm = \
            SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));
          SEXP_FSM_CHECK_INSTANCE(sexp_fsm);
        }

        trace = \
          Mc_create_trace_from_bdd_state_input_list(enc,
               SexpFsm_get_symbols_list(sexp_fsm), trace_title,
                                                   TRACE_TYPE_CNTEXAMPLE, exp);
      }

      FREE(trace_title);

      StreamMgr_print_output(streams, 
              "-- as demonstrated by the following execution sequence\n");

      TraceMgr_register_trace(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), trace);
      TraceMgr_execute_plugin(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), TRACE_OPT(NULL),
                                  TRACE_MGR_DEFAULT_PLUGIN,
                                  TRACE_MGR_LAST_TRACE);

      Prop_set_trace(prop, Trace_get_id(trace));

      walk_dd(dd, bdd_free, exp);
      free_list(nodemgr, exp);
    }
  }

  bdd_free(dd, s0);
} /* Mc_CheckCTLSpec */

void Mc_CheckCompute(NuSMVEnv_ptr env, Prop_ptr prop)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  FILE* outstream = StreamMgr_get_output_stream(streams);

  int s0;
  Expr_ptr  spec = Prop_get_expr_core(prop);
  BddFsm_ptr fsm = BDD_FSM(NULL);
  BddEnc_ptr enc;
  DDMgr_ptr dd;

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "evaluating ");
    print_spec(Logger_get_ostream(logger), prop, get_prop_print_method(opts));
    Logger_log(logger, "\n");
  }

  fsm = Prop_compute_ground_bdd_fsm(env, prop);
  BDD_FSM_CHECK_INSTANCE(fsm);

  enc = BddFsm_get_bdd_encoding(fsm);
  dd = BddEnc_get_dd_manager(enc);

  {
    /*
       We force computation of reachable states, as the following
       calls will be performed more efficiently since they are cached.
    */
    bdd_ptr r = BddFsm_get_reachable_states(fsm);
    bdd_free(dd, r);
  }

  s0 = eval_compute(fsm, enc, spec, Nil);

  StreamMgr_print_output(streams,  "-- ");
  print_compute(StreamMgr_get_output_ostream(streams),
                prop, get_prop_print_method(opts));

  if (s0 == -1) {
    StreamMgr_print_output(streams,  "is infinity\n");
    Prop_set_number_infinite(prop);
    Prop_set_status(prop, Prop_Number);
  }
  else if (s0 == -2) {
    StreamMgr_print_output(streams,  "is undefined\n");
    Prop_set_number_undefined(prop);
    Prop_set_status(prop, Prop_Number);
  }
  else {
    StreamMgr_print_output(streams,  "is %d\n", s0);
    Prop_set_number(prop, s0);
    Prop_set_status(prop, Prop_Number);
  }

  fflush(outstream);
  fflush(errstream);
}

BddStates ex(BddFsm_ptr fsm, BddStates g)
{
  DDMgr_ptr dd = BddEnc_get_dd_manager(BddFsm_get_bdd_encoding(fsm));
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  bdd_ptr result;
  bdd_ptr tmp = bdd_dup(g);

  {
    /*
       The explicit restriction to fair states is required (it affects
       the result from a logical point of view.)
    */
    bdd_ptr fair_states_bdd = BddFsm_get_fair_states(fsm);

    bdd_and_accumulate(dd, &tmp, fair_states_bdd);
    bdd_free(dd, fair_states_bdd);
  }

  if (opt_use_reachable_states(opts)) {
    bdd_ptr reachable_states_bdd =  BddFsm_get_reachable_states(fsm);
    bdd_and_accumulate(dd, &tmp, reachable_states_bdd);
    bdd_free(dd, reachable_states_bdd);
  }

  result = BddFsm_get_backward_image(fsm, tmp);
  bdd_free(dd, tmp);

  if (opt_use_reachable_states(opts)) {
    bdd_ptr reachable_states_bdd =  BddFsm_get_reachable_states(fsm);
    bdd_and_accumulate(dd, &result, reachable_states_bdd);
    bdd_free(dd, reachable_states_bdd);
  }

  return(result);
}

BddStates eu(BddFsm_ptr fsm, BddStates f, BddStates g)
{
  BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);
  DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  bdd_ptr new, oldY;
  bdd_ptr Y = bdd_dup(g);
  int n = 1;

  /* The following simplification may be useful for efficiency since g
     may be unreachable (but they are not fundamental for correctness
     similar simplifications are applied in ex). */

  {
    bdd_ptr fair_states_bdd = BddFsm_get_fair_states(fsm);

    bdd_and_accumulate(dd, &Y, fair_states_bdd);
    bdd_free(dd, fair_states_bdd);
  }

  if (opt_use_reachable_states(opts)) {
      bdd_ptr reachable_states_bdd = BddFsm_get_reachable_states(fsm);
      bdd_and_accumulate(dd, &Y, reachable_states_bdd);
      bdd_free(dd, reachable_states_bdd);
  }

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_nlog(logger, wffprint,
                "eu: computing fixed point approximations for %N ...\n",
                ErrorMgr_get_the_node(errmgr));
  }

  oldY = bdd_dup(Y);
  new = bdd_dup(Y);
  while(bdd_isnot_false(dd, new)) {
    bdd_ptr tmp_1, tmp_2;

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      double states = BddEnc_count_states_of_bdd(enc, Y);
      int size = bdd_size(dd, Y);

      Logger_log(logger, "size of Y%d = %g states, %d BDD nodes\n",
                 n++, states, size);

    }
    bdd_free(dd, oldY);
    oldY = bdd_dup(Y);

    tmp_1 = ex(fsm, new);

    tmp_2 = bdd_and(dd, f, tmp_1);

    bdd_free(dd, tmp_1);
    bdd_or_accumulate(dd, &Y, tmp_2);

    bdd_free(dd, tmp_2);
    tmp_1 = bdd_not(dd, oldY);

    bdd_free(dd, new);
    new = bdd_and(dd, Y, tmp_1);

    bdd_free(dd, tmp_1);
  }
  bdd_free(dd, new);
  bdd_free(dd, oldY);

  return(Y);
}

BddStates eg(BddFsm_ptr fsm, BddStates g)
{
  DDMgr_ptr dd = BddEnc_get_dd_manager(BddFsm_get_bdd_encoding(fsm));
  bdd_ptr fair_transitions;
  bdd_ptr fair_transitions_g;
  bdd_ptr res_si;
  bdd_ptr res;

  /* Lazy evaluation for the case 'EG True' */
  if (bdd_is_true(dd, g)) return BddFsm_get_fair_states(fsm);

  fair_transitions = BddFsm_get_fair_states_inputs(fsm);
  fair_transitions_g = bdd_and(dd, fair_transitions, g);

  res_si = eg_si(fsm, fair_transitions_g);

  res = BddFsm_states_inputs_to_states(fsm, res_si);

  bdd_free(dd, res_si);
  bdd_free(dd, fair_transitions_g);
  bdd_free(dd, fair_transitions);

  return(res);
}

BddStates ef(BddFsm_ptr fsm, BddStates g)
{
  DDMgr_ptr dd = BddEnc_get_dd_manager(BddFsm_get_bdd_encoding(fsm));
  bdd_ptr result, one;

  one = bdd_true(dd);
  result = eu(fsm, one, g);
  bdd_free(dd, one);

  return(result);
}

BddStates au(BddFsm_ptr fsm, BddStates f, BddStates g)
{
  DDMgr_ptr dd = BddEnc_get_dd_manager(BddFsm_get_bdd_encoding(fsm));
  bdd_ptr result, tmp_1, tmp_2, tmp_3, tmp_4;

  tmp_1 = bdd_not(dd, f);
  tmp_2 = bdd_not(dd, g);
  tmp_3 = eg(fsm, tmp_2);
  tmp_4 = bdd_and(dd, tmp_1, tmp_2);
  bdd_free(dd, tmp_1);
  tmp_1 = eu(fsm, tmp_2, tmp_4);
  bdd_free(dd, tmp_2);
  tmp_2 = bdd_or(dd, tmp_1, tmp_3);
  result = bdd_not(dd, tmp_2);

  bdd_free(dd, tmp_2);
  bdd_free(dd, tmp_1);
  bdd_free(dd, tmp_4);
  bdd_free(dd, tmp_3);

  return(result);
}

BddStatesInputs ex_si(BddFsm_ptr fsm, BddStatesInputs si)
{
  DDMgr_ptr dd = BddEnc_get_dd_manager(BddFsm_get_bdd_encoding(fsm));
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  BddStates states;
  BddStatesInputs si_preimage;

  /* Eliminate input variables */
  states = BddFsm_states_inputs_to_states(fsm, si);

  /* Perform weak preimage */
  si_preimage = BddFsm_get_weak_backward_image(fsm, states);

  if (opt_use_reachable_states(opts)) {
    bdd_ptr reachable_states_bdd = BddFsm_get_reachable_states(fsm);

    bdd_and_accumulate(dd, &si_preimage, reachable_states_bdd);
    bdd_free(dd, reachable_states_bdd);
  }

  bdd_free(dd, states);

  return si_preimage;
}

BddStatesInputs eu_si(BddFsm_ptr fsm, bdd_ptr f, bdd_ptr g)
{
  int i = 0;
  BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);
  DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));


  bdd_ptr oldY;
  bdd_ptr resY;
  bdd_ptr newY;
  bdd_ptr rg = bdd_dup(g);

  if (opt_use_reachable_states(opts)) {
    bdd_ptr reachable_states_bdd = BddFsm_get_reachable_states(fsm);

    bdd_and_accumulate(dd, &rg, reachable_states_bdd);
    bdd_free(dd, reachable_states_bdd);
  }

  oldY = bdd_dup(rg);
  resY = bdd_dup(rg);
  newY = bdd_dup(rg);

  bdd_free(dd, rg);

  while (bdd_isnot_false(dd, newY)) {
    bdd_ptr tmp_1, tmp_2;

    if (opt_verbose_level_gt(opts, 5)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,
              "    size of Y%d = %g <states>x<inputs>, %d BDD nodes\n",
              i++, BddEnc_count_states_inputs_of_bdd(enc, resY),
              bdd_size(dd, resY) );
    }

    bdd_free(dd, oldY);

    oldY = bdd_dup(resY);
    tmp_1 = ex_si(fsm, newY);
    tmp_2 = bdd_and(dd, tmp_1, f);
    bdd_free(dd, tmp_1);

    bdd_or_accumulate(dd, &resY, tmp_2);
    bdd_free(dd, tmp_2);

    tmp_1 = bdd_not(dd, oldY);
    bdd_free(dd, newY);

    newY = bdd_and(dd, resY, tmp_1);
    bdd_free(dd, tmp_1);
  }

  bdd_free(dd, newY);
  bdd_free(dd, oldY);

  return BDD_STATES_INPUTS( resY );
}

bdd_ptr eg_si(BddFsm_ptr fsm, bdd_ptr g_si)
{
  DDMgr_ptr dd = BddEnc_get_dd_manager(BddFsm_get_bdd_encoding(fsm));
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  bdd_ptr applicable_states_inputs;
  bdd_ptr fair_states_inputs;

  applicable_states_inputs =
    BddFsm_get_states_inputs_constraints(fsm, BDD_FSM_DIR_BWD);
  bdd_and_accumulate(dd, &applicable_states_inputs, g_si);

  if (opt_use_reachable_states(opts)) {
    bdd_ptr reachable_states_bdd = BddFsm_get_reachable_states(fsm);

    bdd_and_accumulate(dd, &applicable_states_inputs, reachable_states_bdd);
    bdd_free(dd, reachable_states_bdd);
  }

  fair_states_inputs = Mc_get_fair_si_subset(fsm, applicable_states_inputs);

  bdd_free(dd, applicable_states_inputs);

  return fair_states_inputs;
}

BddStates ebu(BddFsm_ptr fsm, BddStates f, BddStates g, int inf, int sup)
{
  BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);
  DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  int i;
  bdd_ptr Y, oldY, tmp_1, tmp_2;
  int n = 1;

  if (inf > sup || inf < 0) return(bdd_false(dd));

  Y = bdd_dup(g);

  {
    bdd_ptr fair_states_bdd = BddFsm_get_fair_states(fsm);

    bdd_and_accumulate(dd, &Y, fair_states_bdd);
    bdd_free(dd, fair_states_bdd);
  }

  if (opt_use_reachable_states(opts)) {
    bdd_ptr reachable_states_bdd = BddFsm_get_reachable_states(fsm);

    bdd_and_accumulate(dd, &Y, reachable_states_bdd);
    bdd_free(dd, reachable_states_bdd);
  }

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_nlog(logger, wffprint,
                "ebu: computing fixed point approximations for  %N ...\n",
                ErrorMgr_get_the_node(errmgr));
  }

  /* compute Y = g | (f & ex(Y)) for states within the bound */
  for (i = sup; i > inf; i--) {
    /* There are more states within the bounds */
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "size of Y%d = %g states, %d BDD nodes\n",
              n++, BddEnc_count_states_of_bdd(enc, Y),
              bdd_size(dd, Y));
    }
    oldY = Y;
    tmp_1 = ex(fsm, Y);
    tmp_2 = bdd_and(dd, f, tmp_1);
    bdd_or_accumulate(dd, &Y, tmp_2);
    bdd_free(dd, tmp_1);
    bdd_free(dd, tmp_2);

    if (Y == oldY) {
      /* fixpoint found. collect garbage, and goto next phase */
      break;
    }
  }

  /* compute Y = f & ex(Y) for states before the bound */
  for (i = inf; i > 0; i--) {
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "size of Y%d = %g states, %d BDD nodes\n",
              n++, BddEnc_count_states_of_bdd(enc, Y),
              bdd_size(dd, Y));
    }
    oldY = bdd_dup(Y);
    tmp_1 = ex(fsm, Y);
    bdd_free(dd, Y);
    Y = bdd_and(dd, f, tmp_1);
    bdd_free(dd, tmp_1);
    bdd_free(dd, oldY);
    if (Y == oldY) {
      /* fixpoint found. collect garbage, and finish */
      break;
    }
  }
  return(Y);
}

BddStates ebf(BddFsm_ptr fsm, BddStates g, int inf, int sup)
{
  DDMgr_ptr dd = BddEnc_get_dd_manager(BddFsm_get_bdd_encoding(fsm));
  bdd_ptr one, result;

  one = bdd_true(dd);
  result = ebu(fsm, one, g, inf, sup);
  bdd_free(dd, one);
  return(result);
}

BddStates ebg(BddFsm_ptr fsm, BddStates g, int inf, int sup)
{
  BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);
  DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  int i;
  bdd_ptr Y, oldY, tmp_1;
  int n = 1;

  if (inf > sup || inf < 0) return bdd_true(dd);

  Y = bdd_dup(g);

  /* Limitation to fair states should be imposed. */
  {
    bdd_ptr fair_states_bdd = BddFsm_get_fair_states(fsm);

    bdd_and_accumulate(dd, &Y, fair_states_bdd);
    bdd_free(dd, fair_states_bdd);
  }

  if (opt_use_reachable_states(opts)) {
    bdd_ptr reachable_states_bdd = BddFsm_get_reachable_states(fsm);

    bdd_and_accumulate(dd, &Y, reachable_states_bdd);
    bdd_free(dd, reachable_states_bdd);
  }

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_nlog(logger, wffprint,
                "ebg: computing fixed point approximations for %N ...\n",
                ErrorMgr_get_the_node(errmgr));
  }

  /* compute Y = g & ex(Y) for states within the bound */
  for (i = sup; i > inf; i--) {
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "size of Y%d = %g states, %d BDD nodes\n",
                 n++, BddEnc_count_states_of_bdd(enc, Y),
                 bdd_size(dd, Y));
    }
    oldY = bdd_dup(Y);
    tmp_1 = ex(fsm, Y);
    bdd_and_accumulate(dd, &Y, tmp_1);
    bdd_free(dd, tmp_1);
    if (Y == oldY) {
      bdd_free(dd, oldY);
      /* fixpoint found. goto next phase */
      break;
    }
    bdd_free(dd, oldY);
  }
  /* compute Y = ex(Y) for states before the bound */
  for (i = inf; i > 0; i--) {
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "size of Y%d = %g states, %d BDD nodes\n",
              n++, BddEnc_count_states_of_bdd(enc, Y),
              bdd_size(dd, Y));
    }
    oldY = Y;
    tmp_1 = ex(fsm, Y);
    bdd_free(dd, Y);
    Y = tmp_1;
    if (Y == oldY) {
      break; /* fixpoint found. */
    }
  }
  return Y;
}

BddStates abu(BddFsm_ptr fsm, BddStates f, BddStates g, int inf, int sup)
{
  BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);
  DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  int i;
  bdd_ptr Y, oldY, tmp_1, tmp_2;
  int n = 1;

  if (inf > sup || inf < 0) return(bdd_false(dd));

  Y = bdd_dup(g);

  {
    bdd_ptr fair_states_bdd = BddFsm_get_fair_states(fsm);

    bdd_and_accumulate(dd, &Y, fair_states_bdd);
    bdd_free(dd, fair_states_bdd);
  }

  if (opt_use_reachable_states(opts)) {
    bdd_ptr reachable_states_bdd = BddFsm_get_reachable_states(fsm);

    bdd_and_accumulate(dd, &Y, reachable_states_bdd);
    bdd_free(dd, reachable_states_bdd);
  }

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_nlog(logger, wffprint,
                "abu: computing fixed point approximations for %N ...\n",
                ErrorMgr_get_the_node(errmgr));
  }
  /* compute Y = g | (f & ax(Y)) for states within the bound */
  for (i = sup; i > inf; i--) {
    if (opt_verbose_level_gt(opts, 1)){
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "size of Y%d = %g states, %d BDD nodes\n",
              n++, BddEnc_count_states_of_bdd(enc, Y),
              bdd_size(dd, Y));
    }
    oldY = Y;
    tmp_1 = bdd_not(dd, Y);
    tmp_2 = ex(fsm, tmp_1);
    bdd_free(dd, tmp_1);
    tmp_1 = bdd_not(dd, tmp_2);
    bdd_free(dd, tmp_2);
    tmp_2 = bdd_and(dd, f, tmp_1);
    bdd_or_accumulate(dd, &Y, tmp_2);
    bdd_free(dd, tmp_1);
    bdd_free(dd, tmp_2);
    if (Y == oldY) {
      break; /* fixpoint found. goto next phase */
    }
  }
  /* compute Y = f & ax(Y) for states before the bound */
  for (i = inf; i > 0; i--) {
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "size of Y%d = %g states, %d BDD nodes\n",
              n++, BddEnc_count_states_of_bdd(enc, Y),
              bdd_size(dd, Y));
    }
    oldY = bdd_dup(Y);
    tmp_1 = bdd_not(dd, Y);
    tmp_2 = ex(fsm, tmp_1);
    bdd_free(dd, tmp_1);
    tmp_1 = bdd_not(dd, tmp_2);
    bdd_free(dd, tmp_2);
    bdd_free(dd, Y);
    Y = bdd_and(dd, f, tmp_1);
    bdd_free(dd, oldY);
    bdd_free(dd,tmp_1);

    if (Y == oldY) {
      break; /* fixpoint found. finish */
    }
  }
  return(Y);
}

int minu(BddFsm_ptr fsm, bdd_ptr arg_f, bdd_ptr arg_g)
{
  BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);
  DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  int i;
  int n = 1;
  bdd_ptr R, Rp, tmp_1;
  bdd_ptr f = bdd_dup(arg_f);
  bdd_ptr g = bdd_dup(arg_g);
  bdd_ptr invar_bdd = BddFsm_get_state_constraints(fsm);
  bdd_ptr fair_states_bdd = BddFsm_get_fair_states(fsm);
  bdd_ptr reachable_states_bdd = BddFsm_get_reachable_states(fsm);

  R = (bdd_ptr)NULL;

  /* We restrict f and g to the seat of fair states */
  bdd_and_accumulate(dd, &g, fair_states_bdd);
  bdd_and_accumulate(dd, &f, fair_states_bdd);

  /* We restrict to reachable states */
  bdd_and_accumulate(dd, &f, reachable_states_bdd);
  bdd_and_accumulate(dd, &g, reachable_states_bdd);

  bdd_free(dd, reachable_states_bdd);

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_nlog(logger, wffprint,
                "minu: computing fixed point approximations for %N ...\n",
                ErrorMgr_get_the_node(errmgr));
  }
  i = 0;

  Rp = bdd_and(dd, f, invar_bdd); /* starts searching from f */

  do {
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "size of Rp%d = %g states, %d BDD nodes\n",
              n++, BddEnc_count_states_of_bdd(enc, Rp),
              bdd_size(dd, Rp));
    }

    tmp_1 = bdd_and(dd, Rp, g);

    if (bdd_isnot_false(dd, tmp_1)) {
      /* If current frontier intersects g return minimum */
      bdd_free(dd, tmp_1);
      bdd_free(dd, f);
      bdd_free(dd, g);
      bdd_free(dd, Rp);
      bdd_free(dd, invar_bdd);
      bdd_free(dd, fair_states_bdd);
      if (R != (bdd_ptr)NULL) bdd_free(dd, R);

      return(i);
    }

    bdd_free(dd, tmp_1);

    if (R != (bdd_ptr)NULL) bdd_free(dd, R);

    R = Rp;

    /* go forward */
    tmp_1 = BddFsm_get_forward_image(fsm, R);

    /* We restrict the image to the set of fair states */
    bdd_and_accumulate(dd, &tmp_1, fair_states_bdd);

    Rp = bdd_or(dd, R, tmp_1);

    bdd_free(dd, tmp_1);

    i++;

  } while ( Rp != R );
  /* could not find g anywhere. A fixpoint has been found. g will not be
     ever found, so return infinity. */
  bdd_free(dd, f);
  bdd_free(dd, g);
  bdd_free(dd, Rp);
  bdd_free(dd, R);
  bdd_free(dd, invar_bdd);
  bdd_free(dd, fair_states_bdd);

  return(-1);
}

int maxu(BddFsm_ptr fsm, bdd_ptr f, bdd_ptr g)
{
  BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);
  DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));


  int i;
  int n = 1;
  bdd_ptr R, Rp;
  bdd_ptr notg, tmp_1;
  bdd_ptr invar_bdd, fair_states_bdd, reachable_states_bdd;

  invar_bdd = BddFsm_get_state_constraints(fsm);
  fair_states_bdd = BddFsm_get_fair_states(fsm);
  reachable_states_bdd = BddFsm_get_reachable_states(fsm);

  { /* checks if f is empty */
    bdd_ptr tmp = bdd_and(dd, f, invar_bdd);
    bdd_and_accumulate(dd, &tmp, reachable_states_bdd);
    if (!bdd_is_false(dd, fair_states_bdd)) {
      bdd_and_accumulate(dd, &tmp, fair_states_bdd);
    }
    else {
      StreamMgr_print_error(streams,  "Warning: fair states are empty. "\
              "Check FSM totality with check_fsm.\n");
    }

    if (bdd_is_false(dd, tmp)) {
      StreamMgr_print_error(streams,  "Warning: in COMPUTE initial state is empty\n");
      bdd_free(dd, tmp);
      bdd_free(dd, reachable_states_bdd);
      bdd_free(dd, fair_states_bdd);
      bdd_free(dd, invar_bdd);
      return -2; /* undefined, as f is empty or not reachable/fair */
    }
    bdd_free(dd, tmp);
  }

  { /* checks if g is empty */
    bdd_ptr tmp = bdd_and(dd, g, invar_bdd);
    bdd_and_accumulate(dd, &tmp, reachable_states_bdd);
    if (!bdd_is_false(dd, fair_states_bdd)) {
      bdd_and_accumulate(dd, &tmp, fair_states_bdd);
    }

    if (bdd_is_false(dd, tmp)) {
      StreamMgr_print_error(streams,  "Warning: in COMPUTE final state is empty\n");
      bdd_free(dd, tmp);
      bdd_free(dd, reachable_states_bdd);
      bdd_free(dd, fair_states_bdd);
      bdd_free(dd, invar_bdd);
      return -2; /* undefined, as g is empty or not reachable/fair */
    }

    bdd_free(dd, tmp);
  }

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_nlog(logger, wffprint,
                "maxu: computing fixed point approximations for %N ...\n",
                ErrorMgr_get_the_node(errmgr));
  }

  tmp_1 = bdd_not(dd, g);
  notg = bdd_and(dd, tmp_1, invar_bdd);

  /* We restrict to fair states */
  bdd_and_accumulate(dd, &notg, fair_states_bdd);

  bdd_free(dd, tmp_1);
  bdd_free(dd, invar_bdd);

  i = 0;
  R = bdd_true(dd);
  Rp = bdd_dup(notg); /* starts from !g */


  /* We restrict to reachable states */
  {
    bdd_ptr reachable_states_bdd = BddFsm_get_reachable_states(fsm);
    bdd_and_accumulate(dd, &Rp, reachable_states_bdd);
    bdd_free(dd, reachable_states_bdd);
  }

  /* We restrict to fair states */
  bdd_and_accumulate(dd, &Rp, fair_states_bdd);

  do {
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "size of Rp%d = %g states, %d BDD nodes\n",
              n++, BddEnc_count_states_of_bdd(enc, Rp),
              bdd_size(dd, Rp));
    }

    tmp_1 = bdd_and(dd, Rp, f);

    if (bdd_is_false(dd, tmp_1)) {
      /* !g does not intersect f anymore. The maximum length of a path
         completely in !g is i. This is the maximum. */
      bdd_free(dd, tmp_1);
      bdd_free(dd, R);
      bdd_free(dd, Rp);
      bdd_free(dd, notg);
      bdd_free(dd, fair_states_bdd);
      bdd_free(dd, reachable_states_bdd);

      return(i);
    }

    bdd_free(dd, tmp_1);
    bdd_free(dd, R);

    R = Rp;

    tmp_1 = BddFsm_get_backward_image(fsm, R);

    /* We restrict to reachable states */
    bdd_and_accumulate(dd, &tmp_1, reachable_states_bdd);

    /* We restrict to fir states */
    bdd_and_accumulate(dd, &tmp_1, fair_states_bdd);

    Rp = bdd_and(dd, tmp_1, notg);

    bdd_free(dd, tmp_1);

    i++;

  } while (R != Rp);

  /* a fixpoint has been found in which !g & f holds, so return infinity */
  bdd_free(dd, R);
  bdd_free(dd, Rp);
  bdd_free(dd, notg);
  bdd_free(dd, fair_states_bdd);
  bdd_free(dd, reachable_states_bdd);

  return -1;
}

void print_spec(OStream_ptr file, Prop_ptr prop, Prop_PrintFmt fmt)
{
  OStream_printf(file, "specification ");
  Prop_print(prop, file, fmt);
  OStream_printf(file, " ");
}

void print_compute(OStream_ptr file, Prop_ptr p, Prop_PrintFmt fmt)
{
  OStream_printf(file, "the result of ");
  Prop_print(p, file, fmt);
}

int Mc_check_psl_property(NuSMVEnv_ptr env, Prop_ptr prop)
{
  int status = 0;
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  nusmv_assert(prop != PROP(NULL));
  nusmv_assert(Prop_get_type(prop) == Prop_Psl);

  if (!cmp_struct_get_build_model(cmps) && !opt_cone_of_influence(opts)) {
    StreamMgr_print_error(streams, 
            "The current partition method %s has not yet be computed.\n",
            TransType_to_string(get_partition_method(opts)));
    StreamMgr_print_error(streams, 
            "Use \t \"build_model -f -m %s\"\nto build the transition " \
            "relation.\n",
            TransType_to_string(get_partition_method(opts)));
    return 1;
  }

  /* Performs mc with bdd technique */
  CATCH(errmgr) { Prop_verify(prop); }
  FAIL(errmgr) { status = 1; }

  return status;
}

int Mc_check_psl_spec(const NuSMVEnv_ptr env, const int prop_no)
{
  const PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  int status = 0;

  if (prop_no != -1) {
    /* checks a single property */
    if (Prop_check_type(PropDb_get_prop_at_index(prop_db,
                                                 prop_no),
                        Prop_Psl) != 0) {
      status = 1;
    }
    else {
      status =
        Mc_check_psl_property(env, PropDb_get_prop_at_index(prop_db, prop_no));
    }
  }
  else {
    lsList props;
    lsGen  iterator;
    Prop_ptr prop;

    props = PropDb_prepare_prop_list(prop_db, Prop_Psl);

    lsForEachItem(props, iterator, prop) {
      if (Prop_is_psl_ltl(prop)) status = Mc_check_psl_property(env, prop);

      if (status != 0) break;
    }

    lsDestroy(props, NULL);
  }

  return status;
}

int Mc_check_invar(NuSMVEnv_ptr env,
                   Prop_ptr prop,
                   McCheckInvarOpts* options)
{
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  PropDb_ptr const prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  ErrorMgr_ptr const errmgr =
   ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  McCheckInvarOpts old_options;

  int retval = 0;

  /* Save current option values for restoring */
  McCheckInvarOpts_init(&old_options, env);

  nusmv_assert(McCheckInvarOpts_is_valid(options));

  set_check_invar_strategy(opts, options->strategy);
  set_check_invar_fb_heuristic(opts, options->fb_heuristic);
  set_check_invar_bddbmc_heuristic(opts, options->bdd2bmc_heuristic);
  set_check_invar_bddbmc_heuristic_threshold(opts, options->threshold);
#if NUSMV_HAVE_SAT_SOLVER
    set_bmc_pb_length(opts, options->bmc_length);
#endif

  CATCH(errmgr) {
    if (NULL != prop) {
      Prop_verify(prop);
    }
    else {
      if (opt_use_coi_size_sorting(opts)) {
        FlatHierarchy_ptr hierarchy =
          FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));
        PropDb_ordered_verify_all_type(prop_db, hierarchy, Prop_Invar);
      }
      else PropDb_verify_all_type(prop_db, Prop_Invar);
    }

    retval = 0;
  }
  FAIL(errmgr) {
    retval = 1;
  }

  /* Restore options */
  set_check_invar_strategy(opts, old_options.strategy);
  set_check_invar_fb_heuristic(opts, old_options.fb_heuristic);
  set_check_invar_bddbmc_heuristic(opts, old_options.bdd2bmc_heuristic);
  set_check_invar_bddbmc_heuristic_threshold(opts, old_options.threshold);
#if NUSMV_HAVE_SAT_SOLVER
  set_bmc_pb_length(opts, old_options.bmc_length);
#endif

  return retval;
}

/*!
  \brief Initialize self to the environment values

  
*/

void McCheckInvarOpts_init(McCheckInvarOpts* self,
                           NuSMVEnv_ptr env)
{
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  self->strategy = opt_check_invar_strategy(opts);
  self->fb_heuristic = opt_check_invar_fb_heuristic(opts);
  self->bdd2bmc_heuristic = opt_check_invar_bddbmc_heuristic(opts);
  self->threshold = opt_check_invar_bddbmc_heuristic_threshold(opts);
#if NUSMV_HAVE_SAT_SOLVER
  self->bmc_length = get_bmc_pb_length(opts);
#else
  self->bmc_length = MC_CHECK_INVAR_OPTS_INVALID;
#endif
}

/*!
  \brief Initialaze self to invalid values

  
*/

void McCheckInvarOpts_init_invalid(McCheckInvarOpts* self)
{
  self->strategy = MC_CHECK_INVAR_OPTS_INVALID;
  self->fb_heuristic = MC_CHECK_INVAR_OPTS_INVALID;
  self->bdd2bmc_heuristic = MC_CHECK_INVAR_OPTS_INVALID;
  self->threshold = MC_CHECK_INVAR_OPTS_INVALID;
  self->bmc_length = MC_CHECK_INVAR_OPTS_INVALID;
}

/*!
  \brief Checks if all the values of self make sense

  
*/

boolean McCheckInvarOpts_is_valid(McCheckInvarOpts* self)
{
  return
    self->strategy != MC_CHECK_INVAR_OPTS_INVALID &&
    self->fb_heuristic != MC_CHECK_INVAR_OPTS_INVALID &&
    self->bdd2bmc_heuristic != MC_CHECK_INVAR_OPTS_INVALID &&
    self->threshold != MC_CHECK_INVAR_OPTS_INVALID
#if NUSMV_HAVE_SAT_SOLVER
    && self->bmc_length != MC_CHECK_INVAR_OPTS_INVALID
#endif
    ;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief  

  Perform one iteration over the list of fairness
  conditions (order is statically determined). Compute states that are
  backward reachable from each of the fairness conditions.

  MAP( ApplicableStatesInputs ) over Fairness constraints

  (Q /\ ex_si ( Z /\ AND_i eu_si(Z, (Z/\ StatesInputFC_i))))

  
*/

static bdd_ptr Mc_fair_si_iteration(BddFsm_ptr fsm,
                                    BddStatesInputs states,
                                    BddStatesInputs subspace)
{
  bdd_ptr res;
  FairnessListIterator_ptr iter;
  bdd_ptr partial_result;
  BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);
  DDMgr_ptr dd_manager = BddEnc_get_dd_manager(enc);

  res = bdd_true(dd_manager);
  partial_result = bdd_dup(states);

  iter = FairnessList_begin( FAIRNESS_LIST( BddFsm_get_justice(fsm) ) );
  while ( ! FairnessListIterator_is_end(iter) ) {
    bdd_ptr fc_si;
    bdd_ptr constrained_fc_si;
    bdd_ptr temp;

    /* Extract next fairness constraint */
    fc_si = JusticeList_get_p(BddFsm_get_justice(fsm), iter);

    /* Constrain it to current set */
    constrained_fc_si = bdd_and(dd_manager, states, fc_si);

    /* Collect states-input that can reach constrained_fc_si without leaving subspace */
    temp = eu_si(fsm, subspace, constrained_fc_si);

    bdd_free(dd_manager, constrained_fc_si);
    bdd_free(dd_manager, fc_si);

    bdd_and_accumulate(dd_manager, &partial_result, temp);
    bdd_free(dd_manager, temp);

    iter = FairnessListIterator_next(iter);
  }

  /* Compute preimage */
  res = ex_si(fsm, partial_result);
  bdd_free(dd_manager, partial_result);

  return res;
}

/*!
  \brief 

  Returns the set of state-input pairs in si that are
  fair, i.e. beginning of a fair path.

  \sa corresponding routines in BddFsm
*/
static BddStatesInputs Mc_get_fair_si_subset(BddFsm_ptr fsm,
                                             BddStatesInputs si)
{
  int i = 0;
  BddStatesInputs res;
  BddStatesInputs old;
  BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);
  DDMgr_ptr dd_manager = BddEnc_get_dd_manager(enc);
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  BDD_FSM_CHECK_INSTANCE(fsm);

  res = BDD_STATES_INPUTS(bdd_true(dd_manager));
  old = BDD_STATES_INPUTS(bdd_false(dd_manager));

  /* GFP computation */
  while (res != old) {
    BddStatesInputs new;

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "  size of res%d = %g <states>x<input>, %d BDD nodes\n",
              i++, BddEnc_count_states_inputs_of_bdd(enc, res),
              bdd_size(dd_manager, res));
    }

    bdd_free(dd_manager, old);
    old = bdd_dup(res);

    /* One iteration over fairness conditions */
    new = Mc_fair_si_iteration(fsm, res, si);

    bdd_and_accumulate(dd_manager, &res, (bdd_ptr) new);
    bdd_and_accumulate(dd_manager, &res, (bdd_ptr) si);

    bdd_free(dd_manager, (bdd_ptr) new);
  }
  bdd_free(dd_manager, old);

  return res;
}
