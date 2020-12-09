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
  \brief Utilities for the bmc package

  \todo: Missing description

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/bmc/bmcInt.h"
#include "nusmv/core/bmc/bmcUtils.h"
#include "nusmv/core/bmc/bmcConv.h"

#include "nusmv/core/parser/parser.h"
#include "nusmv/core/parser/symbols.h"

#include <limits.h>
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BMC_NO_LOOP   -(INT_MAX-1) /* must be negative! */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BMC_ALL_LOOPS BMC_NO_LOOP+1

/* ---------------------------------------------------------------------- */
/* You can define your own symbols for these: */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BMC_ALL_LOOPS_USERSIDE_SYMBOL "*"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BMC_NO_LOOP_USERSIDE_SYMBOL   "X"
/* ---------------------------------------------------------------------- */

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

static be_ptr
bmc_utils_costraint_from_string(BeEnc_ptr be_enc,
                                BddEnc_ptr bdd_enc,
                                const char* str,
                                boolean accept_next_expr,
                                Expr_ptr* node_expr);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

int Bmc_Utils_ConvertLoopFromString(const char* strValue, Outcome* result)
{
  Outcome res = OUTCOME_SUCCESS;
  int l = 0;

  if (strValue == NIL(char)) {
    res = OUTCOME_GENERIC_ERROR;
  }
  else if (Bmc_Utils_IsAllLoopbacksString(strValue))  {
    l = Bmc_Utils_GetAllLoopbacks();
  }
  else if (Bmc_Utils_IsNoLoopbackString(strValue))  {
    l = Bmc_Utils_GetNoLoopback();
  }
  else if (util_str2int(strValue, &l) == 0) {
    /* User could have supplied a private integer value which
       corresponds to a reserved value:: */
    if (Bmc_Utils_IsAllLoopbacks(l) || Bmc_Utils_IsNoLoopback(l)) {
      res = OUTCOME_GENERIC_ERROR;
    }
  }
  else res = OUTCOME_GENERIC_ERROR; /* bad string value */

  /* This implements the auto-check (to simplify coding): */
  if (result == NULL)  {nusmv_assert(res == OUTCOME_SUCCESS);}
  else  *result = res;

  return l;
}

void Bmc_Utils_ConvertLoopFromInteger(const int iLoopback, char* szLoopback,
                                      const int _bufsize)
{
  int iCheck; /* for buffer operations checking only */
  int bufsize = _bufsize-2; /* to store terminator */

  nusmv_assert(bufsize > 0); /* one character+terminator at least! */
  szLoopback[bufsize+1] = '\0'; /* put terminator */

  if (Bmc_Utils_IsAllLoopbacks(iLoopback)) {
    iCheck = snprintf(szLoopback, bufsize, "%s",
          BMC_ALL_LOOPS_USERSIDE_SYMBOL);
    SNPRINTF_CHECK(iCheck, bufsize);
  }
  else if (Bmc_Utils_IsNoLoopback(iLoopback)) {
    iCheck = snprintf(szLoopback, bufsize, "%s", BMC_NO_LOOP_USERSIDE_SYMBOL);
    SNPRINTF_CHECK(iCheck, bufsize);
  }
  else {
    /* value is ok, convert to string: */
    iCheck = snprintf(szLoopback, bufsize, "%d", iLoopback);
    SNPRINTF_CHECK(iCheck, bufsize);
  }
}

boolean Bmc_Utils_IsNoLoopback(const int l)
{
  return (l == BMC_NO_LOOP)? true : false;
}

boolean Bmc_Utils_IsNoLoopbackString(const char* str)
{
  return (strcmp(str, BMC_NO_LOOP_USERSIDE_SYMBOL) == 0)? true : false;
}

boolean Bmc_Utils_IsSingleLoopback(const int l)
{
  return (Bmc_Utils_IsNoLoopback(l) == false)
    && (Bmc_Utils_IsAllLoopbacks(l) == false);
}

boolean Bmc_Utils_IsAllLoopbacks(const int l)
{
  return (l == BMC_ALL_LOOPS)? true : false;
}

boolean Bmc_Utils_IsAllLoopbacksString(const char* str)
{
  return (strcmp(str, BMC_ALL_LOOPS_USERSIDE_SYMBOL) == 0)? true : false;
}

int Bmc_Utils_GetNoLoopback() { return BMC_NO_LOOP; }

int Bmc_Utils_GetAllLoopbacks() { return BMC_ALL_LOOPS; }

const char* Bmc_Utils_GetAllLoopbacksString()
{
  return BMC_ALL_LOOPS_USERSIDE_SYMBOL;
}

int Bmc_Utils_RelLoop2AbsLoop(const int upov_loop, const int k)
{
  if ((Bmc_Utils_IsNoLoopback(upov_loop))
      || (Bmc_Utils_IsAllLoopbacks(upov_loop))
      || (upov_loop >=0)) {
    return upov_loop;
  }
  else return k + upov_loop;
}

Outcome Bmc_Utils_Check_k_l(const int k, const int l)
{
  Outcome ret = OUTCOME_GENERIC_ERROR;

  if ( (k>=0) &&                    /* k has to be non-negative in all cases */
       ( Bmc_Utils_IsNoLoopback(l)  /* the no-loop case */
         ||
         Bmc_Utils_IsAllLoopbacks(l) /* the all-loops case */
         ||
         ( (l>=0) && (l<k) )  /* the single-loop case with the new semantics */
        )
      )
    ret = OUTCOME_SUCCESS;

  return ret;
}

int Bmc_Utils_GetSuccTime(const int time, const int k, const int l)
{
  nusmv_assert((time < k) || (time==k && Bmc_Utils_IsNoLoopback(l)) );

  if (Bmc_Utils_IsNoLoopback(l))
    if (time<k)
      return (time + 1);
    else
      return l;
  else
    if (time < k-1)
      return (time + 1);
    else
      return l;
}

void Bmc_Utils_add_be_into_inc_solver_positively(SatIncSolver_ptr solver,
                                                 SatSolverGroup group,
                                                 be_ptr prob,
                                                 BeEnc_ptr be_enc,
                                                 Be_CnfAlgorithm cnf_alg)
{
  Be_Cnf_ptr cnf;
  Be_Manager_ptr be_mgr;
  be_ptr inprob;
  int polarity = 1;

  /* get the be manager for applying inlining */
  be_mgr = BeEnc_get_be_manager(be_enc);

  /* We force inclusion of the conjunct set to guarantee soundness */
  inprob = Bmc_Utils_apply_inlining4inc(be_mgr, prob);

  /* Convert the problem into CNF */
  cnf = Be_ConvertToCnf(be_mgr, inprob, polarity, cnf_alg);

  /* Add the problem into the solver */
  SatSolver_add(SAT_SOLVER(solver), cnf, group);

  /* Force the added group to be considered positively */
  SatSolver_set_polarity(SAT_SOLVER(solver), cnf, polarity, group);

  /* The CNF is no longer needed */
  Be_Cnf_Delete(cnf);
}

void Bmc_Utils_add_be_into_non_inc_solver_positively(SatSolver_ptr solver,
                                                     be_ptr prob,
                                                     BeEnc_ptr be_enc,
                                                     Be_CnfAlgorithm cnf_alg)
{
  Be_Cnf_ptr cnf;
  Be_Manager_ptr be_mgr;
  be_ptr inprob;
  int polarity = 1;

  /* get the be manager for applying inlining */
  be_mgr = BeEnc_get_be_manager(be_enc);

  /* We force inclusion of the conjunct set to guarantee soundness */
  inprob = Bmc_Utils_apply_inlining(be_mgr, prob);

  /* Convert the problem into CNF */
  cnf = Be_ConvertToCnf(be_mgr, inprob, polarity, cnf_alg);

  /* Add the problem into the solver */
  SatSolver_add(solver, cnf, SatSolver_get_permanent_group(solver));

  /* Force the added group to be considered positively */
  SatSolver_set_polarity(solver, cnf, polarity,
                         SatSolver_get_permanent_group(solver));

  /* The CNF is no longer needed */
  Be_Cnf_Delete(cnf);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void Bmc_Utils_ExpandMacrosInFilename(const char* filename_to_be_expanded,
                                      const SubstString* table_ptr,
                                      const size_t table_len,
                                      char* filename_expanded,
                                      size_t buf_len)
{
  int i;
  /* copy the source string into the destination one: */
  strncpy(filename_expanded, filename_to_be_expanded, buf_len);

  for(i=0; i < table_len; ++i) {
      apply_string_macro_expansion(table_ptr + i, filename_expanded, buf_len);
    } /* for each symbol template */
}

Trace_ptr Bmc_Utils_generate_and_print_cntexample(BeEnc_ptr be_enc,
                                                  TraceMgr_ptr tm,
                                                  SatSolver_ptr solver,
                                                  be_ptr be_prob,
                                                  const int k,
                                                  const char* trace_name,
                                                  NodeList_ptr symbols)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  Trace_ptr trace = \
    Bmc_Utils_generate_cntexample(be_enc, solver, be_prob,
                                  k, trace_name, symbols);

  /* Print the trace using default plugin */
  StreamMgr_print_output(streams,
          "-- as demonstrated by the following execution sequence\n");

  TraceMgr_register_trace(tm, trace);
  TraceMgr_execute_plugin(tm, TRACE_OPT(NULL),
                              TRACE_MGR_DEFAULT_PLUGIN,
                              TRACE_MGR_LAST_TRACE);
  return trace;
}

Trace_ptr Bmc_Utils_generate_cntexample(BeEnc_ptr be_enc,
                                        SatSolver_ptr solver,
                                        be_ptr be_prob,
                                        const int k,
                                        const char* trace_name,
                                        NodeList_ptr symbols)
{
  return Bmc_create_trace_from_cnf_model(be_enc, symbols, trace_name,
             TRACE_TYPE_CNTEXAMPLE, SatSolver_get_model(solver), k);
}

Trace_ptr Bmc_Utils_fill_cntexample(BeEnc_ptr be_enc,
                                    SatSolver_ptr solver,
                                    const int k, Trace_ptr trace)
{
  return Bmc_fill_trace_from_cnf_model(be_enc, SatSolver_get_model(solver),
                                       k, trace);
}

lsList Bmc_Utils_get_vars_list_for_uniqueness(BeEnc_ptr be_enc,
                                              Prop_ptr invarprop)
{
  SexpFsm_ptr bool_sexp_fsm;

  bool_sexp_fsm = SEXP_FSM(Prop_get_bool_sexp_fsm(invarprop));

  return Bmc_Utils_get_vars_list_for_uniqueness_fsm(be_enc, bool_sexp_fsm);
}

lsList Bmc_Utils_get_vars_list_for_uniqueness_fsm(BeEnc_ptr be_enc,
                                                  SexpFsm_ptr bool_sexp_fsm)
{
  SymbTable_ptr st;
  lsList crnt_state_be_vars;

  st = BaseEnc_get_symb_table(BASE_ENC(be_enc));
  crnt_state_be_vars = lsCreate();

  SEXP_FSM_CHECK_INSTANCE(bool_sexp_fsm);
  /* if coi was performed the list will not contain unnecessary vars */

  {
    NodeList_ptr vars = SexpFsm_get_vars_list(bool_sexp_fsm);
    ListIter_ptr iter;
    NODE_LIST_FOREACH(vars, iter) {
      node_ptr sexp_var = NodeList_get_elem_at(vars, iter);

      if (SymbTable_is_symbol_state_var(st, sexp_var)) {
        be_ptr be_var;
        lsStatus status;

        if (SymbTable_is_symbol_bool_var(st, sexp_var)) {
          be_var = BeEnc_name_to_untimed(be_enc, sexp_var);
          status = lsNewEnd(crnt_state_be_vars, (lsGeneric) be_var, 0);
          nusmv_assert(LS_OK == status);
        }
        else {
          /* scalar var, retrieves the list of bits that make its encoding */
          NodeList_ptr bits;
          ListIter_ptr bits_iter;
          bits = BoolEnc_get_var_bits(
            BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(be_enc)), sexp_var);
          NODE_LIST_FOREACH(bits, bits_iter) {
            node_ptr bit = NodeList_get_elem_at(bits, bits_iter);
            be_var = BeEnc_name_to_untimed(be_enc, bit);
            status = lsNewEnd(crnt_state_be_vars, (lsGeneric) be_var, 0);
            nusmv_assert(LS_OK == status);
          }
          NodeList_destroy(bits);
        }
      }
    } /* loop */
  }

  return crnt_state_be_vars;
}

be_ptr Bmc_Utils_apply_inlining(Be_Manager_ptr be_mgr, be_ptr f)
{
  const NuSMVEnv_ptr env = Be_Manager_GetEnvironment(be_mgr);
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (!opt_rbc_inlining(opts)) return f;

  return Be_apply_inlining(be_mgr, f,
                           (!opt_rbc_inlining_lazy(opts) &&
                            opt_counter_examples(opts)));
}

be_ptr Bmc_Utils_apply_inlining4inc(Be_Manager_ptr be_mgr, be_ptr f)
{
  const NuSMVEnv_ptr env = Be_Manager_GetEnvironment(be_mgr);
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (!opt_rbc_inlining(opts)) return f;

  return Be_apply_inlining(be_mgr, f, true);
}

be_ptr Bmc_Utils_simple_costraint_from_string(BeEnc_ptr be_enc,
                                              BddEnc_ptr bdd_enc,
                                              const char* str,
                                              Expr_ptr* node_expr)
{
  return bmc_utils_costraint_from_string(be_enc, bdd_enc, str,
                                         false, node_expr);
}

be_ptr Bmc_Utils_next_costraint_from_string(BeEnc_ptr be_enc,
                                            BddEnc_ptr bdd_enc,
                                            const char* str,
                                            Expr_ptr* node_expr)
{
  return bmc_utils_costraint_from_string(be_enc, bdd_enc, str,
                                         true, node_expr);
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Reads an expression and builds the corresponding BE
               formula. If accept_next_expr is true, then a next
               expression is parsed, otherwise a simple expression is
               parsed.

  Reads a either simple or next expression and builds the
               corresponding BE formula. Exceptions are raised if the
               expression cannot be parsed or has type
               errors. Internal service.

  \se None
*/
static be_ptr
bmc_utils_costraint_from_string(BeEnc_ptr be_enc,
                                BddEnc_ptr bdd_enc,
                                const char* str,
                                boolean accept_next_expr,
                                Expr_ptr* node_expr)
{
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  node_ptr parsed_expr = Nil;
  be_ptr result = (be_ptr) NULL;

  int (*parse_fun)(NuSMVEnv_ptr, const char*, node_ptr*) = accept_next_expr ? \
    Parser_ReadNextExprFromString : Parser_ReadSimpExprFromString;
  int node_type = accept_next_expr ? NEXTWFF : SIMPWFF;

  if (0 == parse_fun(env, str, &parsed_expr) &&
      Nil != parsed_expr &&
      node_type == node_get_type(parsed_expr)) {
    node_ptr bool_constraints;
    node_ptr constraints = car(parsed_expr);

    CATCH(errmgr) {
      if (!TypeChecker_is_expression_wellformed(
                    BaseEnc_get_type_checker(BASE_ENC(bdd_enc)),
                    constraints, Nil)) {
        ErrorMgr_error_type_system_violation(errmgr);
      }

      /* here constraints are intended in context Nil */
      bool_constraints = Compile_detexpr2bexpr(bdd_enc, constraints);

      result = Bmc_Conv_Bexp2Be(be_enc, bool_constraints);
      if ((Expr_ptr*) NULL != node_expr) *node_expr = constraints;
    }
    FAIL(errmgr) {
      result = (be_ptr) NULL;
      if ((Expr_ptr*) NULL != node_expr) *node_expr = (Expr_ptr) NULL;
    }
  }

  if ((be_ptr) NULL == result) {
    ErrorMgr_rpterr(errmgr, "Conversion from expression to BE (aka RBC) failed.");
  }

  return result;
}
