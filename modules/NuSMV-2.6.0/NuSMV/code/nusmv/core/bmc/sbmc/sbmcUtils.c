/* ---------------------------------------------------------------------------


  This file is part of the ``bmc.sbmc'' package of NuSMV version 2.
  Copyright (C) 2006 by Tommi Junttila, Timo Latvala.

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
  \author Tommi Junttila, Timo Latvala, Marco Roveri
  \brief Utilities function for SBMC package

  Utilities function for SBMC package

*/



#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include <stdlib.h>
#include <stdio.h>

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/bmc/sbmc/sbmcUtils.h"
#include "nusmv/core/bmc/sbmc/sbmcStructs.h"

#include "nusmv/core/bmc/bmcInt.h"
#include "nusmv/core/wff/wff.h"
#include "nusmv/core/wff/w2w/w2w.h"
#include "nusmv/core/bmc/bmcCheck.h"
#include "nusmv/core/bmc/bmcUtils.h"

#include "nusmv/core/utils/list.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/parser/symbols.h" /* for tokens */
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/compile/compile.h" /* for sym_intern */
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/prop/Prop.h"

#include "nusmv/core/trace/pkg_trace.h"

#include "nusmv/core/fsm/sexp/BoolSexpFsm.h"
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SBMC_LOOPVAR_LAYER_NAME "SBMC LOOP var"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SBMC_HAS_LOOP_VAR "esbmchlv"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SBMC_LOOP_VAR "esbmclv"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SBMC_UNIQUE_ID "esbmcuid"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SBMC_LOOP_VAR_ADDED "esbmclovaad"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

struct sbmc_MetaSolver_TAG
{
  BeEnc_ptr        be_enc;
  boolean          using_volatile_group;
  SatIncSolver_ptr solver;
  SatSolverGroup   permanent_group;
  SatSolverGroup   volatile_group;
};

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/**Variable********************************************************************

  Synopsis    [Counter to create unique ids.]

  Description [Counter to create unique ids for fresh variables,
  similarly to what happen in LTL2SMV]

  SeeAlso     []

******************************************************************************/

int sbmc_get_unique_id(const NuSMVEnv_ptr env)
{
  /* We use an offset of 2 for avoiding NULL problems within the
     environment */
  if (!NuSMVEnv_has_value(env, ENV_SBMC_UNIQUE_ID)) {
    NuSMVEnv_set_value(env, ENV_SBMC_UNIQUE_ID, NODE_FROM_INT(1)); /* 1 because -1 + 2*/
  }

  return PTR_TO_INT(NuSMVEnv_get_value(env, ENV_SBMC_UNIQUE_ID)) - 2;
}

void sbmc_reset_unique_id(const NuSMVEnv_ptr env)
{
  NuSMVEnv_set_or_replace_value(env, ENV_SBMC_UNIQUE_ID, NODE_FROM_INT(1)); /* 1 because -1 + 2*/
}

void sbmc_increment_unique_id(const NuSMVEnv_ptr env)
{
  if (!NuSMVEnv_has_value(env, ENV_SBMC_UNIQUE_ID)) {
    NuSMVEnv_set_value(env, ENV_SBMC_UNIQUE_ID, NODE_FROM_INT(2)); /* 2 because 0 + 2*/
  }
  else {
    int currval = PTR_TO_INT(NuSMVEnv_get_value(env, ENV_SBMC_UNIQUE_ID));
    NuSMVEnv_set_or_replace_value(env, ENV_SBMC_UNIQUE_ID, NODE_FROM_INT(currval + 1));
  }
}

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define METASOLVERCHECK( ms ) \
  nusmv_assert((sbmc_MetaSolver *)NULL != ms); \
  nusmv_assert((BeEnc_ptr)NULL != ms->be_enc); \
  nusmv_assert((SatIncSolver_ptr)NULL != ms->solver);


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/
/*
 * Auxiliary node printing routine
 */

void sbmc_print_node(const NuSMVEnv_ptr env,
                     FILE * out, const char * prefix, node_ptr node,
                     const char * postfix)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  if ((char *)NULL != prefix) fprintf(out, "%s", prefix);
  print_node(wffprint, out, node);
  if ((char *)NULL != postfix) fprintf(out, "%s", postfix);
}

void sbmc_print_node_list(const NuSMVEnv_ptr env, FILE *out, lsList l)
{
  node_ptr node;
  lsGen iterator;
  const char *sep = "";

  lsForEachItem(l, iterator, node) {
    sbmc_print_node(env, out, sep, node, "");
    sep = ",";
  }
}

node_ptr sbmc_add_new_state_variable(const NuSMVEnv_ptr env,
                                     SymbLayer_ptr layer, const char *name)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  SymbType_ptr symbolicType;
  node_ptr node;
  char * uname;
  size_t uname_size = 0;

  /* The 20 is for keeping track of the possible unique id */
  uname_size = strlen(name) + 10;

  uname = ALLOC(char, uname_size);

  if (snprintf(uname, uname_size, "%d_%s", sbmc_get_unique_id(env), name) < 0) {
    FREE(uname);
    ErrorMgr_internal_error(errmgr, "%s:%d: Unable to create unique string", __FILE__, __LINE__);
  }

  /* Create the internal name of the new variable. It is part of the
     main module */
  node = find_node(nodemgr, DOT, Nil, sym_intern(env, uname));
  nusmv_assert((node_ptr)NULL != node);

  /* uname no longer needed */
  FREE(uname);

  if (!SymbLayer_can_declare_var(layer, node))
    ErrorMgr_error_redefining(errmgr, node);

  symbolicType = SymbType_create(env, SYMB_TYPE_BOOLEAN, Nil);
  SymbLayer_declare_state_var(layer, node, symbolicType);
  return node;
}

lsList sbmc_find_formula_vars(const NuSMVEnv_ptr env, node_ptr ltlspec)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  hash_ptr visit_cache = (hash_ptr)NULL;
  lsList   unprocessed_nodes = (lsList)0;
  lsList   formula_vars = (lsList)0;

  /* Debug output */
  if (opt_verbose_level_ge(opts, 5)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_nlog(logger, wffprint, "Finding vars occurring in %N\n", ltlspec);
  }

  visit_cache = sbmc_set_create();
  formula_vars = lsCreate();
  unprocessed_nodes = lsCreate();
  lsNewBegin(unprocessed_nodes, (lsGeneric) ltlspec, LS_NH);

  while(lsLength(unprocessed_nodes) > 0) {
    node_ptr node, lsf, rsf;
    int has_unprocessed_children;

    /* Get node */
    if ((lsFirstItem(unprocessed_nodes, (lsGeneric*)&node, LS_NH) != LS_OK) ||
        ((node_ptr)NULL == node))
      ErrorMgr_internal_error(errmgr, sbmc_SNH_text, __FILE__, __LINE__);

    /* Already visited? */
    if (sbmc_set_is_in(visit_cache, node)) {
      if (lsDelBegin(unprocessed_nodes, (lsGeneric*)&node) != LS_OK)
        ErrorMgr_internal_error(errmgr, sbmc_SNH_text,__FILE__,__LINE__);
      continue;
    }

    /* Traverse children and build info */
    lsf = car(node);
    rsf = cdr(node);
    has_unprocessed_children = 0;
    switch(node_get_type(node)) {
    case ATOM:
    case BIT:
    case DOT:
    case ARRAY:
      lsNewBegin(formula_vars, (lsGeneric)node, LS_NH);
      break;

    case TRUEEXP:
    case FALSEEXP:
      break;

    case AND:
    case OR:
    case XOR:
    case XNOR:
    case IMPLIES:
    case IFF:
    case UNTIL:
    case RELEASES:
    case TRIGGERED:
    case SINCE:
      if (!sbmc_set_is_in(visit_cache, lsf)) {
        lsNewBegin(unprocessed_nodes, (lsGeneric)lsf, LS_NH);
        has_unprocessed_children = 1;
      }
      if (!sbmc_set_is_in(visit_cache, rsf)) {
        lsNewBegin(unprocessed_nodes, (lsGeneric)rsf, LS_NH);
        has_unprocessed_children = 1;
      }
      break;

    case NOT:
    case OP_GLOBAL:
    case OP_FUTURE:
    case OP_NEXT:
    case OP_HISTORICAL:
    case OP_ONCE:
    case OP_PREC:
    case OP_NOTPRECNOT:
      if (!sbmc_set_is_in(visit_cache, lsf)) {
        lsNewBegin(unprocessed_nodes, (lsGeneric)lsf, LS_NH);
        has_unprocessed_children = 1;
      }
      break;

    default:
      StreamMgr_nprint_error(streams, wffprint, "%N", node);
      ErrorMgr_internal_error(errmgr, "%s:%d: Something not implemented",
                     __FILE__, __LINE__);
      break;
    }
    if (has_unprocessed_children)
      continue;

    if (lsDelBegin(unprocessed_nodes, (lsGeneric*)&node) != LS_OK)
      ErrorMgr_internal_error(errmgr, sbmc_SNH_text, __FILE__, __LINE__);

    sbmc_set_insert(visit_cache, node);
  }

  lsDestroy(unprocessed_nodes, NULL);
  sbmc_set_destroy(visit_cache);

  return formula_vars;
}

void sbmc_print_varmap(const NuSMVEnv_ptr env, FILE *out,
                       node_ptr node, sbmc_node_info *info)
{
  const MasterPrinter_ptr wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  nusmv_assert((node_ptr)NULL != node);
  nusmv_assert((sbmc_node_info *)NULL != info);

  if (opt_verbose_level_ge(opts, 2)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    unsigned int d;
    array_t * trans_vars = sbmc_node_info_get_trans_vars(info);

    for (d = 0; d <= sbmc_node_info_get_past_depth(info); d++) {
      Logger_nlog(logger, wffprint, "[[%N]]^%u = %N;\n",
                  node, d, array_fetch(node_ptr, trans_vars, d));
    }
  }
}

void sbmc_print_Gvarmap(const NuSMVEnv_ptr env,
                        FILE *out, node_ptr var, node_ptr formula)
{
  const MasterPrinter_ptr wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  nusmv_assert((node_ptr)NULL != var);
  nusmv_assert((node_ptr)NULL != formula);

  if (opt_verbose_level_ge(opts, 2)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_nlog(logger, wffprint, "<<G %N>> = %N;\n", formula, var);
  }
}

void sbmc_print_Fvarmap(const NuSMVEnv_ptr env, FILE *out,
                        node_ptr var, node_ptr formula)
{
  const MasterPrinter_ptr wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  nusmv_assert((node_ptr)NULL != var);
  nusmv_assert((node_ptr)NULL != formula);

  if (opt_verbose_level_ge(opts, 2)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_nlog(logger, wffprint, "<<F %N>> = %N;\n", formula, var);
  }
}

node_ptr sbmc_1_fresh_state_var(const NuSMVEnv_ptr env,
                                SymbLayer_ptr layer, unsigned int *index)
{
  char new_var_name[16];
  node_ptr new_var = 0;
  int chars;

  nusmv_assert(index != (unsigned int*) NULL);

  chars = snprintf(new_var_name, 16, "#LTL_t%u", *index);
  SNPRINTF_CHECK(chars, 16);

  *index = *index + 1;
  new_var = sbmc_add_new_state_variable(env, layer, new_var_name);
  return new_var;
}

array_t * sbmc_n_fresh_state_vars(const NuSMVEnv_ptr env,
                                  SymbLayer_ptr layer,
                                  const unsigned int n,
                                  unsigned int *index)
{
  array_t* array;
  unsigned int i;

  nusmv_assert(n > 0);

  nusmv_assert(index != (unsigned int*) NULL);

  array = array_alloc(node_ptr, n);
  nusmv_assert((array_t *)NULL != array);

  for (i = 0; i < n; i++) {
    array_insert(node_ptr, array, i, sbmc_1_fresh_state_var(env, layer, index));
  }
  return array;
}

void sbmc_allocate_trans_vars(const NuSMVEnv_ptr env,
                              sbmc_node_info *info,
                              SymbLayer_ptr layer,
                              lsList state_vars_formula_pd0,
                              lsList state_vars_formula_pdx,
                              unsigned int* new_var_index)
{
  unsigned int d;
  unsigned int pd;
  array_t * array;

  nusmv_assert(info);

  array = sbmc_node_info_get_trans_vars(info);
  nusmv_assert((array_t *)NULL == array);

  pd = sbmc_node_info_get_past_depth(info);
  array = sbmc_n_fresh_state_vars(env, layer, pd + 1, new_var_index);
  sbmc_node_info_set_past_trans_vars(info, array);

  /* Update state_vars_formula_pd0 and state_vars_formula_pdx */
  lsNewBegin(state_vars_formula_pd0,
             (lsGeneric)array_fetch(node_ptr, array, 0),
             LS_NH);
  for (d = 1; d <= pd; d++)
    lsNewBegin(state_vars_formula_pdx,
               (lsGeneric)array_fetch(node_ptr, array, d),
               LS_NH);
}

node_ptr sbmc_make_boolean_formula(BddEnc_ptr bdd_enc, Prop_ptr ltlprop)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(bdd_enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  node_ptr fltlspec = (node_ptr)NULL;

  fltlspec= Wff_make_not(nodemgr, Compile_detexpr2bexpr(bdd_enc,
                                               Prop_get_expr_core(ltlprop)));

  /*
   * Add fairness constraints to the formula
   */
  {
    BoolSexpFsm_ptr sexp_fsm = Prop_get_bool_sexp_fsm(ltlprop);
    node_ptr j_list;
    node_ptr justice_list;
    node_ptr compassion_list;

    BOOL_SEXP_FSM_CHECK_INSTANCE(sexp_fsm);

    j_list = SexpFsm_get_justice(SEXP_FSM(sexp_fsm));
    justice_list = Bmc_CheckFairnessListForPropositionalFormulae(env, j_list);
    j_list = justice_list;

    while(!is_list_empty(justice_list)) {
      fltlspec = Wff_make_and(nodemgr, fltlspec, Wff_make_globally(nodemgr, Wff_make_eventually(nodemgr, car(justice_list))));
      justice_list = cdr(justice_list);
    }

    compassion_list = SexpFsm_get_compassion(SEXP_FSM(sexp_fsm));
    /* Here we should add /\_i ((GF p_i) -> (GF q_i)) /\ fltlspec */
    if (!is_list_empty(compassion_list))
      ErrorMgr_internal_error(errmgr, "%s:%d: Compassion not handled", __FILE__, __LINE__);

    free_list(nodemgr, j_list);
  }

  return Wff2Nnf(env, fltlspec);
}

void sbmc_find_relevant_vars(state_vars_struct *svs,
                             BeFsm_ptr be_fsm,
                             node_ptr bltlspec)
{
  ErrorMgr_ptr errmgr;
  lsGen  iterator;
  node_ptr node = (node_ptr)NULL;
  BeEnc_ptr be_enc = (BeEnc_ptr)NULL;
  SymbTable_ptr st = (SymbTable_ptr)NULL;
  lsList f_vars = (lsList)NULL;
  NuSMVEnv_ptr env;

  nusmv_assert((state_vars_struct *)NULL != svs);

  /* Get be encoding */
  be_enc = BeFsm_get_be_encoding(be_fsm);
  nusmv_assert((BeEnc_ptr)NULL != be_enc);

  env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  /* Get symbol table */
  st = BaseEnc_get_symb_table(BASE_ENC(be_enc));
  nusmv_assert((SymbTable_ptr)NULL != st);

  /* Clean lists if not empty */
  if (lsLength(sbmc_state_vars_get_formula_state_vars(svs)) > 0) {
    lsDestroy(sbmc_state_vars_get_formula_state_vars(svs), NULL);
    sbmc_state_vars_set_formula_state_vars(svs, lsCreate());
  }
  if (lsLength(sbmc_state_vars_get_formula_input_vars(svs)) > 0) {
    lsDestroy(sbmc_state_vars_get_formula_input_vars(svs), NULL);
    sbmc_state_vars_set_formula_input_vars(svs, lsCreate());
  }
  if (lsLength(sbmc_state_vars_get_simple_path_system_vars(svs)) > 0) {
    lsDestroy(sbmc_state_vars_get_simple_path_system_vars(svs), NULL);
    sbmc_state_vars_set_simple_path_system_vars(svs, lsCreate());
  }

  /* Get all the variables occurring in the formula */
  f_vars = sbmc_find_formula_vars(env, bltlspec);
  nusmv_assert((lsList)NULL != f_vars);

  /* Classify the variables to state and input ones */
  lsForEachItem(f_vars, iterator, node) {
    if (SymbTable_is_symbol_state_var(st, node))
      lsNewEnd(sbmc_state_vars_get_formula_state_vars(svs),
               (lsGeneric)node, LS_NH);
    else if (SymbTable_is_symbol_input_var(st, node))
      lsNewEnd(sbmc_state_vars_get_formula_input_vars(svs),
               (lsGeneric)node, LS_NH);
    else if (SymbTable_is_symbol_frozen_var(st, node)) {
      /* frozen vars are ignored since they are not used in state equality expr*/
    }
    else {
      ErrorMgr_internal_error(errmgr, "%s:%d: Unknown variable type (nor state nor input nor frozen)",
                     __FILE__, __LINE__);
    }
  }
  /* Release list */
  lsDestroy(f_vars, NULL);
  f_vars = (lsList)NULL;

  /* Build simple_path_system_vars */
  {
    lsList l, spsv;
    hash_ptr union_set = sbmc_set_create();

    spsv = sbmc_state_vars_get_simple_path_system_vars(svs);

    nusmv_assert((lsList)NULL != spsv);

    l = sbmc_state_vars_get_trans_state_vars(svs);
    lsForEachItem(l, iterator, node) {
      if (!sbmc_set_is_in(union_set, node)) {
        sbmc_set_insert(union_set, node);
        lsNewEnd(spsv, (lsGeneric)node, LS_NH);
      }
    }
    l = sbmc_state_vars_get_formula_state_vars(svs);
    lsForEachItem(l, iterator, node) {
      if (!sbmc_set_is_in(union_set, node)) {
        sbmc_set_insert(union_set, node);
        lsNewEnd(spsv, (lsGeneric)node, LS_NH);
      }
    }
    l = sbmc_state_vars_get_formula_input_vars(svs);
    lsForEachItem(l, iterator, node) {
      if (!sbmc_set_is_in(union_set, node)) {
        sbmc_set_insert(union_set, node);
        lsNewEnd(spsv, (lsGeneric)node, LS_NH);
      }
    }

    sbmc_set_destroy(union_set);
  }
}

Trace_ptr Sbmc_Utils_generate_and_print_cntexample(BeEnc_ptr be_enc,
                                                   TraceMgr_ptr tm,
                                                   sbmc_MetaSolver * solver,
                                                   node_ptr l_var,
                                                   const int k,
                                                   const char * trace_name,
                                                   NodeList_ptr symbols)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  Trace_ptr trace = \
    Sbmc_Utils_generate_cntexample(be_enc, solver, l_var, k,
                                   trace_name, symbols);

  /* Print the trace using default plugin */
  StreamMgr_print_output(streams,
          "-- as demonstrated by the following execution sequence\n");

  TraceMgr_register_trace(tm, trace);
  TraceMgr_execute_plugin(tm, TRACE_OPT(NULL),
                              TRACE_MGR_DEFAULT_PLUGIN,
                              TRACE_MGR_LAST_TRACE);

  return trace;
}

Trace_ptr Sbmc_Utils_generate_cntexample(BeEnc_ptr be_enc,
                                         sbmc_MetaSolver * solver,
                                         node_ptr l_var, const int k,
                                         const char * trace_name,
                                         NodeList_ptr symbols)
{
  const SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(be_enc));

  Trace_ptr trace = Trace_create(st, trace_name, TRACE_TYPE_CNTEXAMPLE,
                                 symbols, false);

  return Sbmc_Utils_fill_cntexample(be_enc, solver, l_var, k, trace);
}

Trace_ptr Sbmc_Utils_fill_cntexample(BeEnc_ptr be_enc,
                                     sbmc_MetaSolver * solver,
                                     node_ptr l_var, const int k,
                                     Trace_ptr res)
{
  /* local refs */
  const BoolEnc_ptr bool_enc = \
    BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(be_enc));

  const Be_Manager_ptr be_mgr = BeEnc_get_be_manager(be_enc);
  const SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(be_enc));
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  Siter genLiteral;
  Slist_ptr cnf_model;

  hash_ptr tvar_2_bval = new_assoc();
  hash_ptr time_2_step = new_assoc();

  nusmv_ptrint cnfLiteral;
  nusmv_ptrint beLiteral;

  int i, loopback, lv_index;

  nusmv_assert(Trace_is_empty(res));

  /* phase 0: setup trace iterators for all times. These iters are
     purposedly time shifted. */
  insert_assoc(time_2_step,
               NODE_FROM_INT(sbmc_real_k(0)), (node_ptr)(Trace_first_iter(res)));

  for (i = 1; i <= k; ++ i) {
    TraceIter step = Trace_append_step(res);
    insert_assoc(time_2_step, NODE_FROM_INT(sbmc_real_k(i)), (node_ptr)(step));
  }

  /* take note of loopvar index */
  loopback = -1; nusmv_assert((be_ptr) NULL != l_var);
  lv_index = BeEnc_name_to_index(be_enc, l_var);

  /* phase 1: we consider only the cnf variables corresponding to BE
     variables in the range [sbmc_real_k(0),sbmc_real_k(k)].

     Thus we ignore the cnf variables that are not corresponding to
     the encoding of the:
     - model variables;
     - encoding variables (sub formulas, loop variables, ...)
  */

  /* The satisfying assignment produced by the SAT solver. */
  /* it is a list of cnf variable index, positive or negative */
  cnf_model = sbmc_MS_get_model(solver);
  SLIST_FOREACH(cnf_model, genLiteral) {
    int index, uv_index;
    int vtime = -1;
    node_ptr var;

    cnfLiteral = (nusmv_ptrint) Siter_element(genLiteral);
    beLiteral = (nusmv_ptrint) Be_CnfLiteral2BeLiteral(be_mgr, cnfLiteral);

    /* if there is no corresponding rbc variable skip this */
    if (0 == beLiteral) continue;

    index = Be_BeLiteral2BeIndex(be_mgr, (int) beLiteral);
    if (!BeEnc_is_index_untimed(be_enc, index)) {
      vtime = BeEnc_index_to_time(be_enc, index);
    }

    /* either frozenvars or timed vars in the valid range are allowed */
    if (BeEnc_is_index_frozen_var(be_enc, index) || \
        (sbmc_real_k(0) <= vtime && vtime <= sbmc_real_k(k))) {

      uv_index = BeEnc_index_to_untimed_index(be_enc, index);
      var = BeEnc_index_to_name(be_enc, uv_index);

      /* needed to adapt to new trace timing format, input is stored
         in the next step. However, input on the last step have no
         semantics. */
      if (SymbTable_is_symbol_input_var(st, var)) {
        ++ vtime;
        if (k < sbmc_model_k(vtime)) { continue; }
      }

      /* loop var needs special handling */
      if ((lv_index == uv_index) && (0 < beLiteral)) {
          nusmv_assert(-1 == loopback);
          loopback = sbmc_model_k(vtime);
          continue;
      }

      /* if it's a bit get/create a BitValues structure for
         the scalar variable which this bit belongs to */
      if (BoolEnc_is_var_bit(bool_enc, var)) {
        BitValues_ptr bv;
        node_ptr key, scalar_var;

        scalar_var = BoolEnc_get_scalar_var_from_bit(bool_enc, var);
        key = find_node(nodemgr, ATTIME, scalar_var, NODE_FROM_INT(vtime));

        bv = BIT_VALUES(find_assoc(tvar_2_bval, key));
        if (BIT_VALUES(NULL) == bv) {
          bv = BitValues_create(bool_enc, scalar_var);
          insert_assoc(tvar_2_bval, key, (node_ptr)(bv));
        }

        /* set the bit value */
        BitValues_set(bv, BoolEnc_get_index_from_bit(bool_enc, var),
                      (beLiteral >= 0) ? BIT_VALUE_TRUE : BIT_VALUE_FALSE);

      }
      else { /* boolean variables do not require any further processing */

        TraceIter timed_step = (-1 != vtime) /* frozenvars */
          ? TRACE_ITER(find_assoc(time_2_step, NODE_FROM_INT(vtime)))
          : Trace_first_iter(res) ;

        nusmv_assert(TRACE_END_ITER != timed_step);
        Trace_step_put_value(res, timed_step, var, beLiteral >= 0
                             ? ExprMgr_true(exprs) : ExprMgr_false(exprs));
      }
    }
  } /* SLIST_FOREACH (phase 1) */

  { /* phase 2: iterate over elements of the hash table (i.e. scalar
       vars) and populate the trace accordingly. */
    assoc_iter aiter;
    node_ptr ts_var;
    BitValues_ptr bitValues;

    ASSOC_FOREACH(tvar_2_bval, aiter, &ts_var, &bitValues) {
      int vtime = NODE_TO_INT(cdr(ts_var)); /* its time */
      node_ptr value = BoolEnc_get_value_from_var_bits(bool_enc, bitValues);

      TraceIter timed_step = (-1 != vtime) /* frozenvars */
        ? TRACE_ITER(find_assoc(time_2_step, NODE_FROM_INT(vtime)))
        : Trace_first_iter(res);

      nusmv_assert(TRACE_END_ITER != timed_step);
      Trace_step_put_value(res, timed_step, car(ts_var), value);

      BitValues_destroy(bitValues);
    }
  } /* phase 2 */

  /* phase 3: some assignments may be missing, complete the trace */
  bmc_trace_utils_complete_trace(res, bool_enc);

  /* phase 4: freeze trace and add loopback information */
  Trace_freeze(res); /* sbmc traces are always frozen */
  if (loopback != -1) {
    /* NuSMV prints traces so that first state is numbered 1 */
    StreamMgr_print_output(streams,
            "   the loop starts at state %d (that is redundantly printed also as"
            " state %d)\n", loopback, k+1);

    Trace_step_force_loopback(res, Trace_ith_iter(res, loopback));
  }
  else {
    StreamMgr_print_output(streams,  "   the execution has no loop\n");
  }

  /* cleanup */
  free_assoc(tvar_2_bval);
  free_assoc(time_2_step);

  return res;
} /* Sbmc_Utils_generate_cntexample */

/*
 * Routines for the state indexing scheme
 * State 0 is the L state and 1 is the E state
 * The first real state is state 2.
 */

int sbmc_L_state(void)
{
  return 0;
}

int sbmc_E_state(void)
{
  return 1;
}

int sbmc_real_k(int k)
{
  return k+2;
}

unsigned int sbmc_model_k(int k)
{
  nusmv_assert(k >= 2);
  return k-2;
}

char* sbmc_real_k_string(const unsigned int k_real)
{
  char *str = ALLOC(char, 32);
  int c = 0;

  if (k_real == sbmc_L_state()) c = snprintf(str, 32, "L");
  else if (k_real == sbmc_E_state()) c = snprintf(str, 32, "E");
  else c = snprintf(str, 32, "%u", sbmc_model_k(k_real));

  SNPRINTF_CHECK(c, 32);

  return str;
}

sbmc_MetaSolver * sbmc_MS_create(BeEnc_ptr be_enc)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  sbmc_MetaSolver* ms = ALLOC(sbmc_MetaSolver, 1);

  nusmv_assert((sbmc_MetaSolver *)NULL != ms);

  ms->be_enc = be_enc;
  ms->using_volatile_group = false;
  ms->solver = SAT_INC_SOLVER(NULL);
  ms->solver = Sat_CreateIncSolver(env, get_sat_solver(opts));

  if (ms->solver == SAT_INC_SOLVER(NULL)) {
    StreamMgr_print_error(streams,  "Incremental sat solver '%s' is not available.\n",
            get_sat_solver(opts));
    FREE(ms);
    return (sbmc_MetaSolver *)NULL;
  }
  ms->permanent_group = SatSolver_get_permanent_group(SAT_SOLVER(ms->solver));

  return ms;
}

void sbmc_MS_destroy(sbmc_MetaSolver *ms)
{
  METASOLVERCHECK(ms);
  SatIncSolver_destroy(ms->solver);
  ms->solver = SAT_INC_SOLVER(NULL);
  FREE(ms);
}

void sbmc_MS_create_volatile_group(sbmc_MetaSolver *ms)
{
  METASOLVERCHECK(ms);
  nusmv_assert(!ms->using_volatile_group);
  ms->volatile_group = SatIncSolver_create_group(ms->solver);
}

void sbmc_MS_destroy_volatile_group(sbmc_MetaSolver *ms)
{
  METASOLVERCHECK(ms);
  SatIncSolver_destroy_group(ms->solver, ms->volatile_group);
  ms->using_volatile_group = false;
}

void sbmc_MS_switch_to_permanent_group(sbmc_MetaSolver *ms)
{
  METASOLVERCHECK(ms);
  ms->using_volatile_group = false;
}

void sbmc_MS_switch_to_volatile_group(sbmc_MetaSolver *ms)
{
  METASOLVERCHECK(ms);
  ms->using_volatile_group = true;
}

void sbmc_MS_goto_permanent_group(sbmc_MetaSolver *ms)
{
  METASOLVERCHECK(ms);
  nusmv_assert(ms->using_volatile_group);
  SatIncSolver_destroy_group(ms->solver, ms->volatile_group);
  ms->using_volatile_group = false;
}

void sbmc_MS_goto_volatile_group(sbmc_MetaSolver *ms)
{
  METASOLVERCHECK(ms);
  nusmv_assert(!ms->using_volatile_group);
  ms->volatile_group = SatIncSolver_create_group(ms->solver);
  ms->using_volatile_group = true;
}

void sbmc_MS_force_true(sbmc_MetaSolver *ms, be_ptr be_constraint,
                        Be_CnfAlgorithm cnf_alg)
{
  Be_Manager_ptr be_mgr;
  Be_Cnf_ptr cnf;
  be_ptr inconstr;
  SatSolver_ptr solver;

  METASOLVERCHECK(ms);

  be_mgr = BeEnc_get_be_manager(ms->be_enc);
  solver = SAT_SOLVER(ms->solver);

  /* We force inclusion of the conjunct set to guarantee soundness */
  inconstr = Bmc_Utils_apply_inlining4inc(be_mgr, be_constraint);

  cnf = Be_ConvertToCnf(be_mgr, inconstr, 1, cnf_alg);

  if (ms->using_volatile_group) {
    SatSolver_add(solver, cnf, ms->volatile_group);
    SatSolver_set_polarity(solver, cnf, 1, ms->volatile_group);
  }
  else {
    SatSolver_add(solver, cnf, ms->permanent_group);
    SatSolver_set_polarity(solver, cnf, 1, ms->permanent_group);
  }
  Be_Cnf_Delete(cnf);
}

void sbmc_MS_force_constraint_list(sbmc_MetaSolver *ms, lsList constraints,
                                   Be_CnfAlgorithm cnf_alg)
{
  lsGen  iterator;
  be_ptr be_constraint;

  lsForEachItem(constraints, iterator, be_constraint) {
    sbmc_MS_force_true(ms, be_constraint, cnf_alg);
  }
}

SatSolverResult sbmc_MS_solve(sbmc_MetaSolver *ms)
{
  METASOLVERCHECK(ms);
  return SatSolver_solve_all_groups(SAT_SOLVER(ms->solver));
}

SatSolverResult sbmc_MS_solve_assume(sbmc_MetaSolver *ms, Slist_ptr assumptions)
{
  METASOLVERCHECK(ms);
  return SatSolver_solve_all_groups_assume(SAT_SOLVER(ms->solver), assumptions);
}

SatSolver_ptr sbmc_MS_get_solver(sbmc_MetaSolver *ms)
{
  METASOLVERCHECK(ms);
  return SAT_SOLVER(ms->solver);
}

Slist_ptr sbmc_MS_get_conflicts(sbmc_MetaSolver *ms)
{
  METASOLVERCHECK(ms);
  return SatSolver_get_conflicts(SAT_SOLVER(ms->solver));
}

Slist_ptr sbmc_MS_get_model(sbmc_MetaSolver *ms)
{
  METASOLVERCHECK(ms);
  return SatSolver_get_model(SAT_SOLVER(ms->solver));
}

void sbmc_add_loop_variable(BddEnc_ptr bdd_enc, BeFsm_ptr be_fsm)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(bdd_enc));
  BeEnc_ptr be_enc;
  BoolEnc_ptr bool_enc;
  SymbTable_ptr symb_table;
  SymbLayer_ptr ltl_layer;
  node_ptr loop_var_name;

  /* Get be encoding */
  be_enc = BeFsm_get_be_encoding(be_fsm);
  nusmv_assert((BeEnc_ptr)NULL != be_enc);

  /* Get Boolean encoding */
  bool_enc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(be_enc));
  nusmv_assert((BoolEnc_ptr)NULL != bool_enc);

  /* Get the symbol table */
  symb_table = BaseEnc_get_symb_table(BASE_ENC(be_enc));
  nusmv_assert((SymbTable_ptr)NULL != symb_table);

  /* checks if previous removal failed due to an interruption by the user */
  if (NuSMVEnv_get_flag(env, ENV_SBMC_LOOP_VAR_ADDED)) sbmc_remove_loop_variable(bdd_enc, be_fsm);

  /* Add a new layer for translation variables */
  ltl_layer = SymbTable_create_layer(symb_table, SBMC_LOOPVAR_LAYER_NAME,
                                     SYMB_LAYER_POS_BOTTOM);
  nusmv_assert((SymbLayer_ptr)NULL != ltl_layer);

  /* We increment to counter for the creation of unique ids among
     different calls */
  sbmc_increment_unique_id(env);

  nusmv_assert((node_ptr)NULL == sbmc_loop_var_name_get(env));
  loop_var_name = sbmc_add_new_state_variable(env, ltl_layer, "#SBMC_LTL_l");
  sbmc_loop_var_name_set(env, loop_var_name);

  /*
   * After introducing all new variables, commit ltl_layer
   */
  BaseEnc_commit_layer(BASE_ENC(bool_enc), SymbLayer_get_name(ltl_layer));
  BaseEnc_commit_layer(BASE_ENC(be_enc), SymbLayer_get_name(ltl_layer));
  BaseEnc_commit_layer(BASE_ENC(bdd_enc), SymbLayer_get_name(ltl_layer));

  NuSMVEnv_set_flag(env, ENV_SBMC_LOOP_VAR_ADDED, true);
}

void sbmc_remove_loop_variable(BddEnc_ptr bdd_enc, BeFsm_ptr be_fsm)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(bdd_enc));
  BeEnc_ptr be_enc;
  BoolEnc_ptr bool_enc;
  SymbTable_ptr symb_table;
  SymbLayer_ptr ltl_layer;

  /* Get be encoding */
  be_enc = BeFsm_get_be_encoding(be_fsm);
  nusmv_assert((BeEnc_ptr)NULL != be_enc);

  /* Get Boolean encoding */
  bool_enc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(be_enc));
  nusmv_assert((BoolEnc_ptr)NULL != bool_enc);

  /* Get the symbol table */
  symb_table = BaseEnc_get_symb_table(BASE_ENC(be_enc));
  nusmv_assert((SymbTable_ptr)NULL != symb_table);

  /* Retrieve the new layer for translation variables */
  ltl_layer = SymbTable_get_layer(symb_table, SBMC_LOOPVAR_LAYER_NAME);
  nusmv_assert((SymbLayer_ptr)NULL != ltl_layer);
  /*
   * Remove extra encoding layers
   */
  BaseEnc_remove_layer(BASE_ENC(bdd_enc), SBMC_LOOPVAR_LAYER_NAME);
  BaseEnc_remove_layer(BASE_ENC(be_enc), SBMC_LOOPVAR_LAYER_NAME);
  BaseEnc_remove_layer(BASE_ENC(bool_enc), SBMC_LOOPVAR_LAYER_NAME);
  SymbTable_remove_layer(symb_table, ltl_layer);

  sbmc_loop_var_name_set(env, (node_ptr)NULL);

  NuSMVEnv_set_flag(env, ENV_SBMC_LOOP_VAR_ADDED, false);
}

void sbmc_loop_var_name_set(const NuSMVEnv_ptr env, node_ptr n)
{
  if (Nil == n) {
    NuSMVEnv_set_flag(env, ENV_SBMC_HAS_LOOP_VAR, false);
  }
  else {
    NuSMVEnv_set_flag(env, ENV_SBMC_HAS_LOOP_VAR, true);
    NuSMVEnv_set_or_replace_value(env, ENV_SBMC_LOOP_VAR, n);
  }
}

node_ptr sbmc_loop_var_name_get(const NuSMVEnv_ptr env)
{
  if (NuSMVEnv_get_flag(env, ENV_SBMC_HAS_LOOP_VAR)) {
    return NODE_PTR(NuSMVEnv_get_value(env, ENV_SBMC_LOOP_VAR));
  }
  return Nil;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
