/* ---------------------------------------------------------------------------


  This file is part of the ``trace'' package of NuSMV version 2.
  Copyright (C) 2010 by FBK.

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
  \author Marco Pensallorto
  \brief This module contains defines evaluation code

  \todo: Missing description

*/

#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/Trace_private.h"

#include "nusmv/core/node/node.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/enc/bdd/BddEnc.h" /* For BDD Encoder */
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/bmc/bmcConv.h"
#include "nusmv/core/compile/symb_table/SymbTable.h"

#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/trace/pkg_traceInt.h"

#include "nusmv/core/trace/eval/BaseEvaluator.h"
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


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

static node_ptr
trace_make_failure(const NuSMVEnv_ptr env, const char* tmpl, node_ptr symbol);

static hash_ptr
trace_eval_make_environment(Trace_ptr trace, TraceIter step);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

boolean trace_step_check_defines(const Trace_ptr trace, const TraceIter step,
                                 NodeList_ptr failures)
{
  SymbTable_ptr st = Trace_get_symb_table(trace);
  NuSMVEnv_ptr nusmv_env = EnvObject_get_environment(ENV_OBJECT(st));
  TraceSymbolsIter sym_iter;

  node_ptr sym;
  node_ptr val;
  boolean res = true; /* no error */
  TraceMgr_ptr tm = TRACE_MGR(NuSMVEnv_get_value(nusmv_env, ENV_TRACE_MGR));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(nusmv_env, ENV_WFF_PRINTER));

  const BaseEvaluator_ptr evaluator = TraceMgr_get_evaluator(tm);

  BASE_EVALUATOR_CHECK_INSTANCE(evaluator);
  nusmv_assert(TRACE_END_ITER != step);
  NODE_LIST_CHECK_INSTANCE(failures);
  nusmv_assert(0 == NodeList_get_length(failures));

  {  /* To evaluate a set of expressions, we first set context for
        evaluation. Then, in two distinct phases state and
        transitional defines are evaluated. 'State' belong to current
        step, 'transitional' belong to next */
    hash_ptr env = trace_eval_make_environment(trace, step);
    const SymbTable_ptr st = Trace_get_symb_table(trace);

    BaseEvaluator_set_context(evaluator, st, env);

    /* 1. state defines */
    TRACE_SYMBOLS_FOREACH(trace, TRACE_ITER_S_DEFINES, sym_iter, sym) {
      if (Nil != trace_step_get_value(trace, step, sym)) {

        val = BaseEvaluator_evaluate(evaluator,
                                     SymbTable_get_define_flatten_body(st, sym));

        if (FAILURE != node_get_type(val)) {
          node_ptr exp_val = trace_step_get_value(trace, step, sym);
          if (exp_val != val) {
            SymbCategory cat = trace_section_to_category(sym_iter.section);

            const char* fail_tmpl = \
              "Value mismatch for symbol %s (%s) calculated: %s, expected: %s";

            const char* cat_repr = trace_symb_category_to_string(cat);

            char *symb_repr = sprint_node(wffprint, sym);
            char* calc_repr = sprint_node(wffprint, val);
            char* expd_repr = sprint_node(wffprint, exp_val);

            char *fail_repr = ALLOC(char, 1 +            \
                                    strlen(fail_tmpl) +  \
                                    strlen(symb_repr) +  \
                                    strlen(cat_repr)  +  \
                                    strlen(calc_repr) +  \
                                    strlen(expd_repr));

            sprintf(fail_repr,
                    fail_tmpl, symb_repr, cat_repr, calc_repr, expd_repr);

            NodeList_append(failures, trace_make_failure(nusmv_env, fail_repr, Nil));

            FREE(symb_repr);
            FREE(calc_repr);
            FREE(expd_repr);
            FREE(fail_repr);

            res = false;
          }
        }
      }
    }

    { /* 2. transitional defines (exist only if there's next state) */
      TraceIter next = trace_iter_get_next(step);

      if (TRACE_END_ITER != next) {
        TRACE_SYMBOLS_FOREACH(trace, TRACE_ITER_TRANSITIONAL, sym_iter, sym) {
          if (Nil != trace_step_get_value(trace, next, sym)) {

            /* this is a bit tricky: evaluation takes place in 'step'
               but being transitional, the resulting value belongs to
               next and must be checked accordingly. */
            val = BaseEvaluator_evaluate(evaluator,
                                         SymbTable_get_define_flatten_body(st,
                                                                           sym));

            if (FAILURE != node_get_type(val)) {
              node_ptr exp_val = trace_step_get_value(trace, next, sym);
              if (exp_val != val) {
                SymbCategory cat = trace_section_to_category(sym_iter.section);

                const char* fail_tmpl = \
                  "Value mismatch for symbol %s (%s) calculated: %s, "
                  "expected: %s";

                const char* cat_repr = trace_symb_category_to_string(cat);

                char *symb_repr = sprint_node(wffprint, sym);
                char* calc_repr = sprint_node(wffprint, val);
                char* expd_repr = sprint_node(wffprint, exp_val);

                char *fail_repr = ALLOC(char, 1 +            \
                                        strlen(fail_tmpl) +  \
                                        strlen(symb_repr) +  \
                                        strlen(cat_repr)  +  \
                                        strlen(calc_repr) +  \
                                        strlen(expd_repr));

                sprintf(fail_repr,
                        fail_tmpl, symb_repr, cat_repr, calc_repr, expd_repr);

                NodeList_append(failures, trace_make_failure(nusmv_env, fail_repr, Nil));

                /* cleanup */
                FREE(symb_repr);
                FREE(calc_repr);
                FREE(expd_repr);
                FREE(fail_repr);

                res = false;
              }
            }
          }
        }
      }
    }

    /* destroy the environment */
    free_assoc(env);
  }

  return res;
} /* trace_step_check_defines */

void trace_step_evaluate_defines(Trace_ptr trace, const TraceIter step)
{
  TraceSymbolsIter sym_iter;
  node_ptr sym;
  node_ptr val;
  NuSMVEnv_ptr nenv = EnvObject_get_environment(ENV_OBJECT(Trace_get_symb_table(trace)));
  TraceMgr_ptr tm = TRACE_MGR(NuSMVEnv_get_value(nenv, ENV_TRACE_MGR));

  const BaseEvaluator_ptr evaluator = TraceMgr_get_evaluator(tm);

  BASE_EVALUATOR_CHECK_INSTANCE(evaluator);
  nusmv_assert(TRACE_END_ITER != step);

  {  /* To evaluate a set of expressions, we first set context for
        evaluation. Then, in two distinct phases state and
        transitional defines are evaluated. 'State' belong to current
        step, 'transitional' belong to next */
    hash_ptr env = trace_eval_make_environment(trace, step);
    const SymbTable_ptr st = Trace_get_symb_table(trace);

    BaseEvaluator_set_context(evaluator, st, env);

    /* 1 . state defines */
    TRACE_SYMBOLS_FOREACH(trace, TRACE_ITER_S_DEFINES, sym_iter, sym) {
      if (Nil == trace_step_get_value(trace, step, sym)) {

        val = BaseEvaluator_evaluate(evaluator,
                                     SymbTable_get_define_flatten_body(st, sym));

        if (FAILURE != node_get_type(val)) {
          trace_step_put_value(trace, step, sym, val);
        }
      }
    } /* foreach state define */

    { /* 2. transitional defines (exist only if there's next state) */
      TraceIter next = trace_iter_get_next(step);
      if (TRACE_END_ITER != next) {
        TRACE_SYMBOLS_FOREACH(trace, TRACE_ITER_TRANSITIONAL, sym_iter, sym) {
          if (Nil == trace_step_get_value(trace, next, sym)) {

            val = BaseEvaluator_evaluate(evaluator,
                                         SymbTable_get_define_flatten_body(st,
                                                                           sym));
            if (FAILURE != node_get_type(val)) {
              trace_step_put_value(trace, next, sym, val);
            }
          }
        }
      }
    } /* foreach transitional define */

    /* destroy the environment */
    free_assoc(env);
  }

} /* trace_step_evaluate_defines */

void Trace_Eval_evaluate_defines(Trace_ptr trace)
{
  TraceIter step;

  TRACE_FOREACH(trace, step) {
    trace_step_evaluate_defines(trace, step);
  }
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Private service of trace_evaluate_expr_recur

  Private service of trace_evaluate_expr_recur

  \sa Private service of trace_evaluate_expr_recur
*/
static node_ptr
trace_make_failure(const NuSMVEnv_ptr env, const char* tmpl, node_ptr symbol)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  char *symb_str = sprint_node(wffprint, symbol);
  char *buf = ALLOC(char, 1 + strlen(tmpl) + strlen(symb_str));
  node_ptr res;

  sprintf(buf, tmpl, symb_str);
  res = ErrorMgr_failure_make(errmgr, buf, FAILURE_UNSPECIFIED, -1);

  FREE(buf);
  FREE(symb_str);

  return res;
}

/*!
  \brief Private service of trace_step_evaluate_defines and
  trace_step_check_defines

  This function builds a local environment for constant
  expressions evaluation

  \se none
*/
static hash_ptr trace_eval_make_environment(Trace_ptr trace, TraceIter step)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(trace));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  hash_ptr res = new_assoc(); /* empty env */
  TraceIter next_step = TraceIter_get_next(step); /* the next step */
  TraceSymbolsIter sym_iter;
  node_ptr sym;

  /* 1. frozen vars always have the same value for x and x' */
  TRACE_SYMBOLS_FOREACH(trace, TRACE_ITER_F_VARS, sym_iter, sym) {
    insert_assoc(res, sym,
                 trace_step_get_value(trace, TRACE_END_ITER, sym));
    insert_assoc(res, find_node(nodemgr, NEXT, sym, Nil),
                 trace_step_get_value(trace, TRACE_END_ITER, sym));
  }

  /* 2. state vars for x and x' (if any) */
  TRACE_SYMBOLS_FOREACH(trace, TRACE_ITER_S_VARS, sym_iter, sym) {
    insert_assoc(res, sym, trace_step_get_value(trace, step, sym));
    if (TRACE_END_ITER != next_step) {
      insert_assoc(res, find_node(nodemgr, NEXT, sym, Nil),
                   trace_step_get_value(trace, next_step, sym));
    }
  }

  /* 3. input vars for i (value is in next step) */
  TRACE_SYMBOLS_FOREACH(trace, TRACE_ITER_I_VARS, sym_iter, sym) {
    if (TRACE_END_ITER != next_step) {
      insert_assoc(res, sym,
                   trace_step_get_value(trace, next_step, sym));
    }
  }

  return res;
}
