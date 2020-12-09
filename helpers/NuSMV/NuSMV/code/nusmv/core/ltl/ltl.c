/* ---------------------------------------------------------------------------


  This file is part of the ``ltl'' package of NuSMV version 2.
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
  \brief Routines to perform reduction of LTL model checking to
  CTL model checking.

  Here we perform the reduction of LTL model checking to
  CTL model checking. The technique adopted has been taken from [1].
  <ol>
    <li>
       O. Grumberg E. Clarke and K. Hamaguchi. "Another Look at LTL
       Model Checking".  <em>Formal Methods in System Design</em>,
       10(1):57--71, February 1997.
    </li>
  </ol>
*/


#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/ltl/ltl.h"
#include "nusmv/core/ltl/ltlInt.h"
#include "nusmv/core/ltl/ltl2smv/ltl2smv.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/parser.h"
#include "nusmv/core/prop/propPkg.h"
#include "nusmv/core/prop/Prop.h"
#include "nusmv/core/wff/ExprMgr.h" /* for Expr_ptr */
#include "nusmv/core/fsm/bdd/FairnessList.h"
#include "nusmv/core/fsm/bdd/bdd.h" /* to check preconditions for EL_fwd */
#include "nusmv/core/mc/mc.h"
#include "nusmv/core/mc/mcInt.h" /* for Mc_create_trace_from_bdd_state_input_list */
#include "nusmv/core/compile/compile.h" /* to check for presence of compassion */

#include "nusmv/core/utils/error.h" /* for CATCH(errmgr) */
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/TraceMgr.h"
#include "nusmv/core/utils/EnvObject_private.h"

#include "nusmv/core/enc/enc.h"
#include "nusmv/core/opt/opt.h"

#include "nusmv/core/hrc/HrcNode.h"
#include "nusmv/core/prop/propProp.h"

// J-Edit:
// #include <iostream>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/stat.h>
// End of J-edit.

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_LTL_SPEC_COUNTER "ltl_spec_counter"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef struct Ltl_StructCheckLtlSpec_TAG Ltl_StructCheckLtlSpec;

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

struct Ltl_StructCheckLtlSpec_TAG
{
  INHERITS_FROM(EnvObject);

  Prop_ptr prop; /* The property to verify */
  BddFsm_ptr fsm; /* The FSM representing the product model and
                     tableau */
  BddEnc_ptr bdd_enc; /* The BDD encoder */
  DDMgr_ptr dd;  /* The BDD package manager */
  SymbTable_ptr symb_table; /* The Symbol Table */
  SymbLayer_ptr tableau_layer; /* The layer where tableau variables
                                  will be added */
  bdd_ptr s0; /* The BDD representing the result of the verification */
  node_ptr spec_formula;
  Ltl_StructCheckLtlSpec_oreg2smv oreg2smv; /* The tableau constructor
                                               to use. This one may
                                               generate additional
                                               LTL, that will be
                                               removed by ltl2smv */
  Ltl_StructCheckLtlSpec_ltl2smv ltl2smv;   /* The tableau constructor
                                               to use. This is used to
                                               remove additional LTL
                                               properties left by
                                               oreg2smv */
  boolean negate_formula; /* flag to keep track wether the formula has
                             to be negated or not */
  boolean removed_layer; /* Flag to inform wether the layer has been
                            removed or not */
  boolean do_rewriting; /* Enables the rewriting to remove input from
                           properties */
  Prop_Rewriter_ptr rewriter;
};

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static BddFsm_ptr
ltlPropAddTableau(Ltl_StructCheckLtlSpec_ptr, FlatHierarchy_ptr);
static void
ltl_structcheckltlspec_remove_layer(Ltl_StructCheckLtlSpec_ptr);
static void ltl_structcheckltlspec_finalize(Object_ptr object, void* dummy);
static void ltl_structcheckltlspec_deinit(Ltl_StructCheckLtlSpec_ptr);
static void ltl_structcheckltlspec_init(Ltl_StructCheckLtlSpec_ptr,
                                        NuSMVEnv_ptr env,
                                        Prop_ptr prop);
static void ltl_structcheckltlspec_prepare(Ltl_StructCheckLtlSpec_ptr);
static int
ltl_structcheckltlspec_build_tableau_and_prop_fsm(Ltl_StructCheckLtlSpec_ptr self);
static void
ltl_structcheckltlspec_check_compassion(Ltl_StructCheckLtlSpec_ptr self);
static void
ltl_structcheckltlspec_check_el_bwd(Ltl_StructCheckLtlSpec_ptr self);
static void
ltl_structcheckltlspec_check_el_fwd(Ltl_StructCheckLtlSpec_ptr self);
static bdd_ptr ltl_clean_bdd(Ltl_StructCheckLtlSpec_ptr, bdd_ptr);

static void compute_loopback_information(Ltl_StructCheckLtlSpec_ptr self,
                                         node_ptr exp, array_t * loops);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Ltl_CheckLtlSpec(NuSMVEnv_ptr env, Prop_ptr prop)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  BddELFwdSavedOptions_ptr elfwd_saved_options = (BddELFwdSavedOptions_ptr) NULL;
  FlatHierarchy_ptr hierarchy = FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));
  Ltl_StructCheckLtlSpec_ptr cls;
  /* WARNING: [MR]: Here we should keep the info from the property and
     if not there from the env. This indeed may cause problems since
     the FSM associated tot he property may have only Justice and not
     Compassion constraints. */
  /* save settings */
  if ((Nil == FlatHierarchy_get_compassion(hierarchy)) &&
      (get_oreg_justice_emptiness_bdd_algorithm(opts) ==
       BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD)) {
    elfwd_saved_options = Bdd_elfwd_check_set_and_save_options(env, BDD_ELFWD_OPT_ALL);
  }

  /* construction */
  cls = Ltl_StructCheckLtlSpec_create(env, prop);

  /* setup options */
  /* These are now default options.. */
  /* Ltl_StructCheckLtlSpec_set_oreg2smv(cls, ltl2smv); */
  /* Ltl_StructCheckLtlSpec_set_ltl2smv(cls, NULL); */
  /* Ltl_StructCheckLtlSpec_set_negate_formula(cls, true); */
  /* Ltl_StructCheckLtlSpec_set_do_rewriting(cls, true); */

  /* action */
  Ltl_StructCheckLtlSpec_build(cls);
  Ltl_StructCheckLtlSpec_check(cls);

  Ltl_StructCheckLtlSpec_print_result(cls);

  if (bdd_isnot_false(cls->dd, cls->s0) &&
      opt_counter_examples(opts)) {

    SexpFsm_ptr sexp_fsm; /* needed for trace lanugage */
    sexp_fsm = Prop_get_scalar_sexp_fsm(prop);
    /* The scalar fsm is set within the property by the
       Ltl_StructCheckLtlSpec_build procedure. It must exist. */
    SEXP_FSM_CHECK_INSTANCE(sexp_fsm);

    Ltl_StructCheckLtlSpec_explain(cls, SexpFsm_get_symbols_list(sexp_fsm));
  }

  /* cleanup */
  Ltl_StructCheckLtlSpec_destroy(cls);

  /* restore settings */
  if (elfwd_saved_options != (BddELFwdSavedOptions_ptr) NULL) {
    Bdd_elfwd_restore_options(env, BDD_ELFWD_OPT_ALL, elfwd_saved_options);
  }
}

void print_ltlspec(OStream_ptr stream, Prop_ptr prop, Prop_PrintFmt fmt)
{
  OStream_printf(stream, "LTL specification ");
  Prop_print(prop, stream, fmt);
}

Ltl_StructCheckLtlSpec_ptr Ltl_StructCheckLtlSpec_create(NuSMVEnv_ptr env,
                                                         Prop_ptr prop)
{
  Ltl_StructCheckLtlSpec_ptr res;

  res = ALLOC(Ltl_StructCheckLtlSpec, 1);
  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(res);

  ltl_structcheckltlspec_init(res, env, prop);

  return res;
}

void Ltl_StructCheckLtlSpec_destroy(Ltl_StructCheckLtlSpec_ptr self)
{
  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

void Ltl_StructCheckLtlSpec_set_oreg2smv(Ltl_StructCheckLtlSpec_ptr self,
                                        Ltl_StructCheckLtlSpec_oreg2smv oreg2smv)
{
  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);

  self->oreg2smv = oreg2smv;
}

void Ltl_StructCheckLtlSpec_set_ltl2smv(Ltl_StructCheckLtlSpec_ptr self,
                                       Ltl_StructCheckLtlSpec_ltl2smv ltl2smv)
{
  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);

  self->ltl2smv = ltl2smv;
}

void Ltl_StructCheckLtlSpec_set_negate_formula(Ltl_StructCheckLtlSpec_ptr self,
                                               boolean negate_formula)
{
  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);

  self->negate_formula = negate_formula;
}

void Ltl_StructCheckLtlSpec_set_do_rewriting(Ltl_StructCheckLtlSpec_ptr self,
                                            boolean do_rewriting)
{
  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);

  self->do_rewriting = do_rewriting;
}

bdd_ptr Ltl_StructCheckLtlSpec_get_s0(Ltl_StructCheckLtlSpec_ptr self)
{
  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);

  return self->s0;
}

bdd_ptr Ltl_StructCheckLtlSpec_get_clean_s0(Ltl_StructCheckLtlSpec_ptr self)
{
  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);

  return ltl_clean_bdd(self, self->s0);
}

void Ltl_StructCheckLtlSpec_build(Ltl_StructCheckLtlSpec_ptr self)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int res = 0; /* suppress warning*/

  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);
  nusmv_assert(self->prop != NULL);
  nusmv_assert(self->oreg2smv != NULL);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "evaluating ");
    print_ltlspec(Logger_get_ostream(logger),
                  self->prop, get_prop_print_method(opts));
    Logger_log(logger, "\n");
  }

  CATCH(errmgr) {
    res = ltl_structcheckltlspec_build_tableau_and_prop_fsm(self);
  }
  FAIL(errmgr) {
    ltl_structcheckltlspec_remove_layer(self);
    StreamMgr_print_error(streams,  "An error occured during tableau construction.\n");
    ErrorMgr_nusmv_exit(errmgr, 1);
  }

  if (res == 1) {
    ltl_structcheckltlspec_remove_layer(self);
    StreamMgr_print_error(streams,
            "Ltl_CheckLtlSpec: Problems in Tableau generation.\n");
    ErrorMgr_nusmv_exit(errmgr, 1);
  }
}

void Ltl_StructCheckLtlSpec_check(Ltl_StructCheckLtlSpec_ptr self)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  boolean full_fairness;

  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);
  BDD_FSM_CHECK_INSTANCE(self->fsm);

  /* If the compassion list is not empty, then activate the full
     fairness algorithm. */
  full_fairness =
    ! FairnessList_is_empty(FAIRNESS_LIST(BddFsm_get_compassion(self->fsm)));

  if (full_fairness) {
    ltl_structcheckltlspec_check_compassion(self);
  } else {
    /* Check fairness states to be empty or not */
    if (opt_verbose_level_gt(opts, 0)) {
       bdd_ptr fair =  BddFsm_get_fair_states(self->fsm);

       if (bdd_is_false(self->dd, fair)) {
         ErrorMgr_warning_fsm_fairness_empty(errmgr);
       }

       bdd_free(self->dd, fair);
    }

    /* [VS] Making the switch here is probably ok for algorithms
       that vary only the fixed point computation. For things like
       l2s it probably has to be moved up to Ltl_CheckLtlSpec (and
       others). But then algorithms so much different might have
       dedicated NuSMV shell commands, so keeping it here for
       now. */
    switch(get_oreg_justice_emptiness_bdd_algorithm(opts)) {
    case BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_BWD:
      ltl_structcheckltlspec_check_el_bwd(self);
      break;
    case BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD:
      ltl_structcheckltlspec_check_el_fwd(self);
      break;
    default:
      error_unreachable_code();
      break;
    }
  }

  if (bdd_is_false(self->dd, self->s0)) {
    Prop_set_status(self->prop, Prop_True);
  }
  else {
    Prop_set_status(self->prop, Prop_False);
  }

  if (self->do_rewriting) {
    Prop_Rewriter_update_original_property(self->rewriter);
    self->prop = Prop_Rewriter_get_original_property(self->rewriter);
  }
}

void Ltl_StructCheckLtlSpec_print_result(Ltl_StructCheckLtlSpec_ptr self)
{
  // J-edit: Communicate result of LTL model-checking:
  FILE *fptrJ;

  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(EnvObject_get_environment(ENV_OBJECT(self)), ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(EnvObject_get_environment(ENV_OBJECT(self)), ENV_OPTS_HANDLER));

  /* Prints out the result, if not true explain. */
  StreamMgr_print_output(streams,  "-- ");
  print_spec(StreamMgr_get_output_ostream(streams),
             self->prop, get_prop_print_method(opts));

  if (Prop_get_status(self->prop) == Prop_True){
    StreamMgr_print_output(streams,  "is true\n");
      //Open a file for writing
      fptrJ = fopen("bitvalue.txt", "w");
      if (fptrJ == NULL){
          printf("Cannot open file in NuSMV\n");
          exit(0);
      }
      // Write truth value = 1
      fputc('1', fptrJ);
  }
  else {
    StreamMgr_print_output(streams,  "is false\n");
      //Open a file for writing
      fptrJ = fopen("bitvalue.txt", "w");
      if (fptrJ == NULL){
          printf("Cannot open file in NuSMV\n");
          exit(0);
      }
      //Write truth value = 1
      fputc('0', fptrJ);
  }
  fclose(fptrJ);

  StreamMgr_flush_streams(streams);
  // End of J-Edit
}

Trace_ptr
Ltl_StructCheckLtlSpec_build_counter_example(Ltl_StructCheckLtlSpec_ptr self,
                                             NodeList_ptr symbols)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(EnvObject_get_environment(ENV_OBJECT(self)), ENV_OPTS_HANDLER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(EnvObject_get_environment(ENV_OBJECT(self)), ENV_NODE_MGR));

  boolean full_fairness;
  node_ptr exp;
  Trace_ptr trace;
  char* trace_title = NULL;
  char* trace_title_postfix = " Counterexample";
  bdd_ptr tmp;
  array_t * loops = array_alloc(unsigned int, 0);

  nusmv_assert((array_t *)NULL != loops);

  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);

  full_fairness =
    ! FairnessList_is_empty(FAIRNESS_LIST(BddFsm_get_compassion(self->fsm)));

  nusmv_assert(opt_counter_examples(opts));
  nusmv_assert(bdd_isnot_false(self->dd, self->s0));
  /* Counterexample construction for forward Emerson-Lei not yet
     implemented. */
  nusmv_assert(full_fairness ||
   !(get_oreg_justice_emptiness_bdd_algorithm(opts) ==
   BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD));

  tmp = BddEnc_pick_one_state(self->bdd_enc, self->s0);
  bdd_free(self->dd, self->s0);
  self->s0 = tmp;

  if (full_fairness) {
    exp = witness(self->fsm, self->bdd_enc, self->s0);
  }
  else {
    bdd_ref(self->s0); /* to put s0 in the list */
    exp = reverse(explain(self->fsm, self->bdd_enc, cons(nodemgr, (node_ptr)self->s0, Nil),
                          self->spec_formula, Nil));
  }
  if (exp == Nil) {
    /* The counterexample consists of one initial state */
    exp = cons(nodemgr, (node_ptr)self->s0, Nil);
  }

  /* Computes loopback information and stores it into the array loops
     if any */
  compute_loopback_information(self, exp, loops);

  /* removes all the tableau variables from the result before
     building the resulting trace. This will make simulation
     working, but may show unexistent loops in the shown trace */

  {
    node_ptr iter = exp;

    while (iter != Nil) {
      bdd_ptr sit;
      bdd_ptr si;

      nusmv_assert(node_get_type(iter) == CONS);
      sit = (bdd_ptr) car(iter);
      si = ltl_clean_bdd(self, sit);
      bdd_free(self->dd, sit);
      node_bdd_setcar(iter, si);

      iter = cdr(iter);
    }
  }

  /* The trace title depends on the property type. For example it
     is in the form "LTL Counterexample" */
  trace_title = ALLOC(char, strlen(Prop_get_type_as_string(self->prop)) +
                      strlen(trace_title_postfix) + 1);
  nusmv_assert(trace_title != (char*) NULL);
  strcpy(trace_title, Prop_get_type_as_string(self->prop));
  strcat(trace_title, trace_title_postfix);

  trace = Mc_create_trace_from_bdd_state_input_list(self->bdd_enc, symbols,
                                        trace_title, TRACE_TYPE_CNTEXAMPLE, exp);

  {
    unsigned int l;

    Trace_freeze(trace); /* BDD traces are always frozen */

    for(l = 0; l < array_n(loops); l++) {
      unsigned int loopback = array_fetch(int, loops, l);
      Trace_step_force_loopback(trace, Trace_ith_iter(trace, loopback));
    }
  }

  array_free(loops);

  FREE(trace_title);

  walk_dd(self->dd, bdd_free, exp);
  free_list(nodemgr, exp);

  return trace;
}

void Ltl_StructCheckLtlSpec_explain(Ltl_StructCheckLtlSpec_ptr self,
                                    NodeList_ptr symbols)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(EnvObject_get_environment(ENV_OBJECT(self)), ENV_STREAM_MANAGER));
  Trace_ptr trace;
  TraceMgr_ptr tm;

  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);

  tm = TRACE_MGR(NuSMVEnv_get_value(EnvObject_get_environment(ENV_OBJECT(self)),
                                        ENV_TRACE_MGR));

  trace = Ltl_StructCheckLtlSpec_build_counter_example(self, symbols);

  StreamMgr_print_output(streams,
          "-- as demonstrated by the following execution sequence\n");

  (void)TraceMgr_register_trace(tm, trace);

  /* TODO[MD] here the error status is not considered */
  (void)TraceMgr_execute_plugin(tm, TRACE_OPT(NULL),
                                TRACE_MGR_DEFAULT_PLUGIN,
                                TRACE_MGR_LAST_TRACE);

  Prop_set_trace(self->prop, Trace_get_id(trace));
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Main routine to add the tableau to the FSM

  The bdd fsm into the property will change
*/
static BddFsm_ptr ltlPropAddTableau(Ltl_StructCheckLtlSpec_ptr self,
                                    FlatHierarchy_ptr hierarchy)
{
  FsmBuilder_ptr builder = FSM_BUILDER(NuSMVEnv_get_value(EnvObject_get_environment(ENV_OBJECT(self)),
                                                          ENV_FSM_BUILDER));
  SexpFsm_ptr tableau_sexp_fsm;
  BddFsm_ptr prop_bdd_fsm = BDD_FSM(NULL);
  BddFsm_ptr tableau_bdd_fsm = BDD_FSM(NULL);
  TransType  trans_type;
  BoolEnc_ptr bool_enc;

  bool_enc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(self->bdd_enc));

  /*
   * After introducing all new variables, commit tableau_layer.
   */
  BaseEnc_commit_layer(BASE_ENC(bool_enc),
                       SymbLayer_get_name(self->tableau_layer));
  BaseEnc_commit_layer(BASE_ENC(self->bdd_enc),
                       SymbLayer_get_name(self->tableau_layer));

  prop_bdd_fsm = Prop_get_bdd_fsm(self->prop);
  BDD_FSM_CHECK_INSTANCE(prop_bdd_fsm);

  { /* Forces all the variable in the model to be declared also in
       the tableau hierarchy.  Notice that thus the set of
       variables of the tableau hiearchy results a superset of the
       needed set, however this approximation helps performances,
       as calculate the exact dependencies of the LTL formula may
       cost much more then approximating */
    FlatHierarchy_ptr fh_model =
      SexpFsm_get_hierarchy(Prop_get_scalar_sexp_fsm(self->prop));
    Set_t vars = FlatHierarchy_get_vars(fh_model);
    Set_Iterator_t iter;
    SET_FOREACH(vars, iter) {
      FlatHierarchy_add_var(hierarchy, Set_GetMember(vars, iter));
    }
  }

  /* Creation of the corresponding FSMs: */
  tableau_sexp_fsm = SexpFsm_create(hierarchy,
                                    FlatHierarchy_get_vars(hierarchy));

  trans_type =
    GenericTrans_get_type( GENERIC_TRANS(BddFsm_get_trans(prop_bdd_fsm)) );

  tableau_bdd_fsm = FsmBuilder_create_bdd_fsm(builder, self->bdd_enc,
                                              tableau_sexp_fsm, trans_type);

  /* Carries out the reversed synchronous product. This is correct, because
     we are only interested in determining if M x T |= EG True */
  BddFsm_apply_synchronous_product(tableau_bdd_fsm, prop_bdd_fsm);

  SexpFsm_destroy(tableau_sexp_fsm);

  return tableau_bdd_fsm;
}

void Ltl_spec_to_hierarchy(NuSMVEnv_ptr env, Expr_ptr spec, node_ptr context,
                           node_ptr (*what2smv)(NuSMVEnv_ptr _env,
                                                unsigned int id,
                                                node_ptr expr),
                           SymbLayer_ptr layer,
                           FlatHierarchy_ptr outfh)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  unsigned int ltl_spec_counter = 0;

  node_ptr module;
  char* module_name;
  FlatHierarchy_ptr modfh;
  int c;
  size_t module_name_len = strlen(LTL_MODULE_BASE_NAME)+7;
  SymbTable_ptr st = FlatHierarchy_get_symb_table(outfh);

  nusmv_assert(! Wff_Rewrite_is_rewriting_needed(st, spec, context));

  if (NuSMVEnv_has_value(env, ENV_LTL_SPEC_COUNTER)) {
    /* ALways put offset for avoiding clash between 0 and NULL */
    ltl_spec_counter = NODE_TO_INT(NuSMVEnv_get_value(env, ENV_LTL_SPEC_COUNTER)) - 2;
  }

  if (ExprMgr_is_true(exprs, spec)) return; /* nothing to be done */

  module_name = ALLOC(char, module_name_len);
  if (module_name == (char*) NULL) {
    ErrorMgr_internal_error(errmgr, "Unable to allocate module name.");
  }

  c = snprintf(module_name, module_name_len, "%s%u", LTL_MODULE_BASE_NAME, ltl_spec_counter);
  SNPRINTF_CHECK(c, module_name_len);

  /* Remember the offset of 2, and increment of 1 */
  NuSMVEnv_set_or_replace_value(env, ENV_LTL_SPEC_COUNTER,
                                NODE_FROM_INT(ltl_spec_counter + 3));

  /* constructs the module */
  module = what2smv(env, ltl_spec_counter, spec);
  /* TODO[MR]: We can consider memoizing of modules" */

  /* we insert the definition of the current module in the
     module_hash in order to make it available for the
     Compile_FlattenHierarchy routines. */
  CompileFlatten_hash_module(env, module);
  /* TODO[MR]: Here we can consider free the new module added so far, as
     to free memory */

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Flattening the generated tableau....");
  }

  /* call Compile_FlattenTableau with the name of the generated
     tableau, and as root name the actual property context. In this
     way local variables of the tableau and local variables of the
     formula will be contextualized to the right module. */
  modfh = Compile_FlattenHierarchy(env,
                                   st, layer,
                                   sym_intern(env, module_name),
                                   context,
                                   Nil, /* no actual */
                                   false /*do not create process vars*/,
                                   true /* carries out calc of vars constr now */,
                                   false /* expand_bounded_arrays */,
                                   HRC_NODE(NULL) /* hrc structure must not be constructed */);
  FREE(module_name);

  FlatHierarchy_mergeinto(outfh, modfh);
  FlatHierarchy_destroy(modfh);
}

/*!
  \brief Creates the tableau

  Creates the tableau for a LTL property.  The FSM of the
  property contains the tableau. Returns 1 if an error is encountered
  during the tableau generation, 0 otherwise

  \se The bdd fsm into the prop will change
*/
static int
ltl_structcheckltlspec_build_tableau_and_prop_fsm(Ltl_StructCheckLtlSpec_ptr self)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ExprMgr_ptr exprs =
    EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  FlatHierarchy_ptr hierarchy;
  Expr_ptr spec;
  Expr_ptr ltl_formula;
  node_ptr context;
  Prop_Rewriter_ptr rewriter = NULL;
  int retval = -1;
  const SymbTable_ptr st = Prop_get_symb_table(self->prop);
  hierarchy = FlatHierarchy_create(st);

  /* performs input rewriting if required */
  if (self->do_rewriting) {
    rewriter = self->rewriter;
    self->prop = Prop_Rewriter_rewrite(rewriter);
    spec = Prop_get_expr_core(self->prop);
    spec = Compile_FlattenSexpExpandDefine(st, spec, Nil);
  }
  else {
  /* We consider the parsed expression.
     oreg2smv should take care of the case in which the spec is not LTL*/
    spec = Prop_get_expr_core(self->prop);
  }

  /* finds the context if any */
  if (node_get_type(spec) == CONTEXT) {
    context     = car(spec);
    ltl_formula = cdr(spec);
  }
  else {
    context = Nil;
    ltl_formula = spec;
  }

  /* the formula has to be negated */
  if (self->negate_formula) {
    ltl_formula = ExprMgr_not(exprs, ltl_formula);
  }

  /* time to construct the tableau of the (negated) formula */
  Ltl_spec_to_hierarchy(EnvObject_get_environment(ENV_OBJECT(self)),
                        ltl_formula, context,
                        self->oreg2smv,
                        self->tableau_layer,
                        hierarchy);

  /* handle potentially remaining LTL part (e.g. for PSL) */
  if (Nil != FlatHierarchy_get_ltlspec(hierarchy)) {
    Expr_ptr conj_ltlspecs;
    node_ptr iter;

    /* LTL part */
    nusmv_assert(false == self->do_rewriting);
    nusmv_assert(NULL != self->ltl2smv);

    /* calculates the conjuction of all LTLs */
    conj_ltlspecs = ExprMgr_true(exprs);
    for (iter=FlatHierarchy_get_ltlspec(hierarchy); Nil != iter;
         iter = cdr(iter)) {
      node_ptr ctxexpr;
      node_ptr ctx;
      node_ptr expr;

      nusmv_assert(CONS == node_get_type(iter));
      ctxexpr = car(iter);

      /* [AM] Added support for named properties: Tree has now a new
       * node of type LTLSPEC before the CONTEXT node. Property name
       * can be found in the right part of the LTLSPEC node, while the
       * old CONTEXT node is on it's left part
       */
      nusmv_assert(Nil != ctxexpr);
      nusmv_assert(LTLSPEC == node_get_type(ctxexpr));
      ctxexpr = car(ctxexpr);

      nusmv_assert(Nil != ctxexpr);
      nusmv_assert(CONTEXT == node_get_type(ctxexpr));
      ctx = car(ctxexpr);
      nusmv_assert(Nil == ctx);
      expr = cdr(ctxexpr);
      conj_ltlspecs = ExprMgr_and(exprs, conj_ltlspecs, expr);
    }

    /* the ltlspecs are no longer needed and must be removed */
    FlatHierarchy_set_ltlspec(hierarchy, Nil);

    /* time to append (and merge) the tableau of the negated LTLs */
    conj_ltlspecs = ExprMgr_not(exprs, conj_ltlspecs);
    Ltl_spec_to_hierarchy(EnvObject_get_environment(ENV_OBJECT(self)),
                          conj_ltlspecs, Nil /* nil context */,
                          self->ltl2smv,
                          self->tableau_layer,
                          hierarchy);
  }

  /* ------------------------------------------------------------ */
  /* Some checks on the generated hierarchy */
  /* Check if we are using an old version of ltl2smv */
  if (FlatHierarchy_get_spec(hierarchy) != Nil) {
    ErrorMgr_internal_error(errmgr, "Error: CTL specification in tableau construction"
                   " (check version of ltl2smv)\n");
  }
  nusmv_assert(Nil == FlatHierarchy_get_ltlspec(hierarchy));
  nusmv_assert(Nil == FlatHierarchy_get_invarspec(hierarchy));
  nusmv_assert(Nil == FlatHierarchy_get_pslspec(hierarchy));
  nusmv_assert(Nil == FlatHierarchy_get_compute(hierarchy));
  /* ------------------------------------------------------------ */


  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, ".... done\n");
    Logger_log(logger, "Creating LTL tableau variables...\n");
  }

  /* The error trapping mechanism is enough in this block. All the
     other errors even external to this block are trapped and the
     else of the CATCH(errmgr) is executed. */
  CATCH(errmgr) {
    self->fsm = ltlPropAddTableau(self, hierarchy);
    retval = 0;
  }
  FAIL(errmgr) {
    retval = 1;
  }

  /* cleaning */
  FlatHierarchy_destroy(hierarchy); hierarchy = NULL;

  return retval;
}

/*!
  \brief Perform the check to see wether the property holds or
  not using an algorithm for strong fairness

  Assumes the Ltl_StructcCheckLtlSpec structure being
  initialized before with Ltl_StructCheckLtlSpec_build.
*/
static void
ltl_structcheckltlspec_check_compassion(Ltl_StructCheckLtlSpec_ptr self)
{
  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);
  nusmv_assert(!FairnessList_is_empty(FAIRNESS_LIST(BddFsm_get_compassion(self->fsm))));

  self->s0 = feasible(self->fsm, self->bdd_enc);
}

/*!
  \brief Perform the check to see wether the property holds or
  not using the backward Emerson-Lei algorithm

  Assumes the Ltl_StructcCheckLtlSpec structure being
  initialized before with Ltl_StructCheckLtlSpec_build.
*/
static void
ltl_structcheckltlspec_check_el_bwd(Ltl_StructCheckLtlSpec_ptr self)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  bdd_ptr tmp;

  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);
  nusmv_assert(FairnessList_is_empty(FAIRNESS_LIST(BddFsm_get_compassion(self->fsm))));

  self->spec_formula =
    find_node(nodemgr, NOT,
              find_node(nodemgr, EG,
                        find_node(nodemgr, TRUEEXP,Nil,Nil), Nil), Nil);

  if (opt_verbose_level_gt(opts, 2)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Prop_ptr phi = Prop_create_partial(EnvObject_get_environment(ENV_OBJECT(self)), self->spec_formula, Prop_Ctl);
    Logger_log(logger, "Checking CTL ");
    print_spec(Logger_get_ostream(logger), phi, get_prop_print_method(opts));
    Logger_log(logger, " generated from the tableau.\n");
    Prop_destroy(phi);
  }

  /* Verification of the property: */
  CATCH(errmgr) {
   self->s0 = eval_ctl_spec(self->fsm,
                            self->bdd_enc,
                            self->spec_formula,
                            Nil);
  }
  FAIL(errmgr) {
    ltl_structcheckltlspec_remove_layer(self);
    StreamMgr_print_error(streams,
            "ltl_checkltlspec_el_bwd: Problems in Tableau verification.\n");
    ErrorMgr_nusmv_exit(errmgr, 1);
    return;
  }

  /* Negate the result */
  tmp = bdd_not(self->dd, self->s0);
  bdd_free(self->dd, self->s0);
  self->s0 = tmp;

  /* Intersect with init, invar and fair states */
  {
    bdd_ptr init  = BddFsm_get_init(self->fsm);
    bdd_ptr invar = BddFsm_get_state_constraints(self->fsm);
    bdd_ptr fair =  BddFsm_get_fair_states(self->fsm);

    bdd_and_accumulate(self->dd, &(self->s0), init);
    bdd_and_accumulate(self->dd, &(self->s0), invar);
    bdd_and_accumulate(self->dd, &(self->s0), fair);
    bdd_free(self->dd, fair);
    bdd_free(self->dd, invar);
    bdd_free(self->dd, init);
  }
}

/*!
  \brief Perform the check to see wether the property holds or
  not using the forward Emerson-Lei algorithm

  Assumes the Ltl_StructcCheckLtlSpec structure being
  initialized before with Ltl_StructCheckLtlSpec_build.
*/
static void
ltl_structcheckltlspec_check_el_fwd(Ltl_StructCheckLtlSpec_ptr self)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);

  nusmv_assert(FairnessList_is_empty(FAIRNESS_LIST(BddFsm_get_compassion(self->fsm))));
  nusmv_assert(Bdd_elfwd_check_options(EnvObject_get_environment(ENV_OBJECT(self)),
                                       BDD_ELFWD_OPT_FORWARD_SEARCH |
                                       BDD_ELFWD_OPT_LTL_TABLEAU_FORWARD_SEARCH |
                                       BDD_ELFWD_OPT_USE_REACHABLE_STATES,
                                       false));
  {
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(EnvObject_get_environment(ENV_OBJECT(self)), ENV_ERROR_MANAGER));

    /* Verification of the property: */
    CATCH(errmgr) {
      /* The result below is the set of states that can be reached from
         a rechable cycle through all Buechi fairness
         constraints. Hence, if self->s0 is not empty, then the language
         of the transition system is not empty. */
      self->s0 = BddFsm_get_revfair_states(self->fsm);
    }
    FAIL(errmgr) {
      ltl_structcheckltlspec_remove_layer(self);
      StreamMgr_print_error(streams,
              "ltl_checkltlspec_el_fwd: Problems in Tableau verification.\n");
      ErrorMgr_nusmv_exit(errmgr, 1);
      return;
    }
  }
}

/*!
  \brief Private service that removes the given layer from
  the symbol table, and from both the boolean and bdd encodings.


*/
static void ltl_structcheckltlspec_remove_layer(Ltl_StructCheckLtlSpec_ptr self)
{
  BoolEnc_ptr bool_enc;

  LTL_STRUCTCHECKLTLSPEC_CHECK_INSTANCE(self);

  nusmv_assert(!self->removed_layer);

  bool_enc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(self->bdd_enc));

  if (BaseEnc_layer_occurs(BASE_ENC(self->bdd_enc),
                           SymbLayer_get_name(self->tableau_layer))) {
    BaseEnc_remove_layer(BASE_ENC(self->bdd_enc),
                         SymbLayer_get_name(self->tableau_layer));
  }

  if (BaseEnc_layer_occurs(BASE_ENC(bool_enc),
                           SymbLayer_get_name(self->tableau_layer))) {
    BaseEnc_remove_layer(BASE_ENC(bool_enc),
                         SymbLayer_get_name(self->tableau_layer));
  }

  /* remove tableau layer from symbol table */
  if (SymbTable_layer_class_exists(self->symb_table, ARTIFACTS_LAYERS_CLASS)) {
    SymbTable_layer_remove_from_class(self->symb_table,
                                      SymbLayer_get_name(self->tableau_layer),
                                      ARTIFACTS_LAYERS_CLASS);
  }

  SymbTable_remove_layer(self->symb_table, self->tableau_layer);

  self->removed_layer = true;
}

/*!
  \brief Quantify out tableau variables

  Quantify out tableau variables

  \se required

  \sa optional
*/
static bdd_ptr ltl_clean_bdd(Ltl_StructCheckLtlSpec_ptr self, bdd_ptr bdd)
{
  BddVarSet_ptr tableau_cube;
  bdd_ptr res;

  tableau_cube = BddEnc_get_layer_vars_cube(self->bdd_enc,
                                            self->tableau_layer,
                                            VFT_CNIF);

  res = bdd_forsome(self->dd, bdd, tableau_cube);

  return res;
}

/*!
  \brief required

  optional

  \se required

  \sa optional
*/
static void ltl_structcheckltlspec_init(Ltl_StructCheckLtlSpec_ptr self,
                                        NuSMVEnv_ptr env,
                                        Prop_ptr prop)
{

  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  env_object_init(ENV_OBJECT(self), env);

  self->prop = prop;
  self->fsm = BDD_FSM(NULL);
  self->bdd_enc = BDD_ENC(NULL);
  self->dd = (DDMgr_ptr )NULL;

  self->symb_table = SYMB_TABLE(NULL);
  self->tableau_layer = SYMB_LAYER(NULL);

  self->removed_layer = false;
  self->spec_formula = Nil;

  if (opt_ltl2smv_single_justice(opts)) {
    self->oreg2smv = ltl2smv_single_justice;
  }
  else {
    self->oreg2smv = ltl2smv;
  }
  self->ltl2smv = NULL;
  self->negate_formula = true;
  self->do_rewriting = true;

  ltl_structcheckltlspec_prepare(self);

  self->rewriter = Prop_Rewriter_create(env, prop, WFF_REWRITE_METHOD_STANDARD,
                                        WFF_REWRITER_REWRITE_INPUT_NEXT,
                                        FSM_TYPE_BDD, self->bdd_enc);

  OVERRIDE(Object, finalize) = ltl_structcheckltlspec_finalize;
}

/*!
  \brief required

  optional

  \se required

  \sa optional
*/
static void ltl_structcheckltlspec_finalize(Object_ptr object, void* dummy)
{
  Ltl_StructCheckLtlSpec_ptr self = LTL_STRUCTCHECKLTLSPEC(object);

  ltl_structcheckltlspec_deinit(self);

  FREE(self);
}

/*!
  \brief required

  optional

  \se required

  \sa optional
*/
static void ltl_structcheckltlspec_deinit(Ltl_StructCheckLtlSpec_ptr self)
{
  if (!self->removed_layer) {
    ltl_structcheckltlspec_remove_layer(self);
  }

  if (BDD_FSM(NULL) != self->fsm) {
    BddFsm_destroy(self->fsm);
  }
  bdd_free(self->dd, self->s0);

  Prop_Rewriter_destroy(self->rewriter); self->rewriter = NULL;
}

/*!
  \brief Support function for the init function

  Support function for the init function

  \sa ltl_structcheckltlspec_init
*/
static void ltl_structcheckltlspec_prepare(Ltl_StructCheckLtlSpec_ptr self)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(EnvObject_get_environment(ENV_OBJECT(self)), ENV_OPTS_HANDLER));

  BddFsm_ptr bdd_fsm = BDD_FSM(NULL);
  SexpFsm_ptr sexp_fsm = SEXP_FSM(NULL);

  /* Prepare here all structures needed for the LTL MC.*/

  /* Prepare property's FSMs, if needed. Prop_compute_ground_bdd_fsm
     takes care of this */
  bdd_fsm = Prop_compute_ground_bdd_fsm(EnvObject_get_environment(ENV_OBJECT(self)), self->prop);
  sexp_fsm = Prop_get_scalar_sexp_fsm(self->prop);

  SEXP_FSM_CHECK_INSTANCE(sexp_fsm);
  BDD_FSM_CHECK_INSTANCE(bdd_fsm);

  /* Prepare the ST and the BDD_ENC for later uses */
  self->bdd_enc = BddFsm_get_bdd_encoding(bdd_fsm);
  BDD_ENC_CHECK_INSTANCE(self->bdd_enc);

  self->dd = BddEnc_get_dd_manager(self->bdd_enc);
  nusmv_assert((DDMgr_ptr )NULL != self->dd);

  self->symb_table = BaseEnc_get_symb_table(BASE_ENC(self->bdd_enc));
  SYMB_TABLE_CHECK_INSTANCE(self->symb_table);

  /* Now that we have a symbol table, prepare the tableau layer */
  nusmv_assert(SYMB_LAYER(NULL) == self->tableau_layer);
  self->tableau_layer = SymbTable_create_layer(self->symb_table,
                                               NULL /* temp name */,
                                               SYMB_LAYER_POS_BOTTOM);

  /* The tableau layer must be added to ARTIFACTS class in order to strip
   * the tableau symbols from resulting ctx trace (if any). */
  SymbTable_layer_add_to_class(self->symb_table,
                               SymbLayer_get_name(self->tableau_layer),
                               ARTIFACTS_LAYERS_CLASS);

  /* Calculate reachable states from the model fsm, if COI is not
     enabled and reachable states are required. This will disable
     calculation of reachable states in the tableau fsm. If option
     ltl_tableau_forward_search is enabled, reachables will be
     re-calculated when needed */
  if ((!opt_cone_of_influence(opts)) &&
      opt_use_reachable_states(opts) &&
      !opt_ltl_tableau_forward_search(opts)) {
    BddStates states = BddFsm_get_reachable_states(bdd_fsm);
    bdd_free(self->dd, states);
  }
}

/*!
  \brief Support function for computing the loop back information.

  Support function for computing the loop back information.  The exp
  is S (I,S)* where S is the initial state. The loop back information
  are constructed by returning the "step" of the states that are equal
  to the last state (if different from the initial state).  i.e. for a
  trace S_0 I_1 S_1 ... S_k ... S_n S_final , the constructed array
  contains the index in the set {k | S_k = S_final}.
*/
static void compute_loopback_information(Ltl_StructCheckLtlSpec_ptr self,
                                         node_ptr exp, array_t * loops)
{
  unsigned int step = 1;
  node_ptr iter;
  node_ptr last = Nil;

  /* We compute the last state of the counterexample */
  /* Ignore initial state */
  nusmv_assert(Nil != exp);
  iter = cdr(exp);
  while (Nil != iter) {
    /* We consume the input */
    iter = cdr(iter);
    nusmv_assert(Nil != iter);
    last = car(iter);
    iter = cdr(iter);
  }
  /* If the counterexample is not composed of the only initial
     state */
  if (Nil != last) {
    bdd_ptr bdd_l = (bdd_ptr)last;
    /* If the initial state is equal to the last state, then add loop
       back to intiial state */
    if (bdd_l == (bdd_ptr)car(exp)) {
      array_insert_last(int, loops, step);
    }
    iter = cdr(exp);
    while (Nil != iter) {
      step++;
      /* We consume the input */
      iter = cdr(iter);
      /* It is not the last state of the sequence (I,S), and it is
         equal to the last state, then add the loopback to the current
         step */
      if ((bdd_l == (bdd_ptr)car(iter)) && (Nil != cdr(iter))) {
        array_insert_last(int, loops, step);
      }
      iter = cdr(iter);
    }
  }
}
