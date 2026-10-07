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
  \brief Automated tests suite for the trace package

  This module contains self-testing code for the trace package

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/trace/pkg_traceInt.h"
#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/trace/TraceMgr.h"
#include "nusmv/core/trace/TraceOpt.h"
#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/exec/traceExec.h"

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/ucmd.h"

#include "nusmv/core/enc/enc.h"
#include "nusmv/core/enc/bool/BoolEnc.h"

#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/prop/propPkg.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/cinit/cinit.h"

/******************************************************************************/
/* NOT COMPILED UNLESS TRACE_DEBUG IS DEFINED */
/* The ifdef is here to avoid ISO C forbids an empty translation unit
   [-pedantic] */
/******************************************************************************/
#ifndef NDEBUG

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef int (*trace_pkg_test_ptr)(NuSMVEnv_ptr env, FILE* out, FILE* err);

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
static int trace_test_setup(NuSMVEnv_ptr env, FILE* out, FILE* err);
static int trace_test_cleanup(NuSMVEnv_ptr env, FILE* out, FILE* err);

/* create a trace, perform a few basic tests on metadata */
static int trace_test_creation(NuSMVEnv_ptr env, FILE* out, FILE* err);

/* create a trace, perform language queries */
static int trace_test_language(NuSMVEnv_ptr env, FILE* out, FILE* err);

/* create a trace, perform queries on assignments */
static int trace_test_population(NuSMVEnv_ptr env, FILE* out, FILE* err);

/* perform trace copy */
static int trace_test_copy(NuSMVEnv_ptr env, FILE* out, FILE* err);

/* perform trace concatenation */
static int trace_test_concat(NuSMVEnv_ptr env, FILE* out, FILE* err);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/
/*!
  \brief Self-test for the trace package

  This function assumes "go" to have been called. WARNING:
  a RESET of the system is performed
*/

/* [MD] If you wanto to use this test, either write a command for it, or make
   sure it is called after go. */

/* prototype to avoid warnings */
int TracePkg_test_package (NuSMVEnv_ptr env, FILE* out, FILE* err);

int TracePkg_test_package (NuSMVEnv_ptr env, FILE* out, FILE* err)
{
  int passed = 0, failed = 0;
  int i, res;
  trace_pkg_test_ptr tests[] = {
    trace_test_creation,
    trace_test_language,
    trace_test_population,
    trace_test_copy,
    trace_test_concat,
  };

  for (i = 0; i < sizeof(tests) / sizeof(tests[0]); ++ i) {
    trace_pkg_test_ptr f = tests[i];
    nusmv_assert( (trace_pkg_test_ptr)(NULL) !=  f);

    fprintf(out, "\n***** TEST %d: *****\n", i);

    /* run test */
    res = trace_test_setup(env, out, err) ||                      \
      (*f)(env, out, err) ||                                      \
      trace_test_cleanup(env, out, err);

    if (0 == res) {
      fprintf(out, "PASSED\n\n");
      ++ passed;
    }
    else {
      fprintf(out, "FAILED\n\n");
      ++ failed;
    }
  }

  fprintf(out, "%d tests performed, %d passed, %d failed.\n",
          (unsigned)(sizeof(tests) / sizeof(tests[0])), passed, failed);

  return ! (0 == failed);
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int trace_test_setup(NuSMVEnv_ptr env, FILE* out, FILE* err)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  /* here "go" was called directly before. Removed to avoid shell dependencies */
  int res = 0;

  if (0 == res) {
    SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
    BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
    BoolEnc_ptr benc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(bdd_enc));
    SymbTableIter iter;

    fprintf(out, "symbols: ");

    SymbTable_gen_iter(st, &iter, STT_VAR | STT_DEFINE);
    while (!SymbTable_iter_is_end(st, &iter)) {
      node_ptr symb = SymbTable_iter_get_symbol(st, &iter);

      if (BoolEnc_is_var_bit(benc, symb)) continue;
      print_node(wffprint, out, symb); fprintf(out, " ");
      SymbTable_iter_next(st, &iter);
    }
  }
  else {
    fprintf(err, "%s:%d:%s: test setup failed\n",
            __FILE__, __LINE__, __func__);
  }

  return res;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int trace_test_cleanup(NuSMVEnv_ptr env, FILE* out, FILE* err)
{
  NuSMVCore_reset(env);

  return 0;
}

static int trace_test_creation (NuSMVEnv_ptr env, FILE* out, FILE* err)
{
  const char* desc = "My test trace #1";
  SexpFsm_ptr sexp_fsm;
  SymbTable_ptr st;
  int i;

  sexp_fsm =
      SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));
  SEXP_FSM_CHECK_INSTANCE(sexp_fsm);

  st = SexpFsm_get_symb_table(sexp_fsm);

  fprintf(out, "\n## Trace creation ##\n");

  /* try it a few times ... */
  for (i = 0; i < 10; ++ i ) {
      Trace_ptr trace =
          Trace_create(st, desc, TRACE_TYPE_SIMULATION,
                       SexpFsm_get_symbols_list(sexp_fsm), true);

    /* a newly created traces has 0 length */
    if (0 != Trace_get_length(trace)) return 1;

    /* retrieve TRACE TYPE */
    if (TRACE_TYPE_SIMULATION != Trace_get_type(trace)) return 1;

    /* ... and description */
    if (0 != strcmp(Trace_get_desc(trace), desc)) return 1;

    /* a newly created trace is unregistered */
    if (Trace_is_registered(trace)) return 1;

    /* destroy it */
    Trace_destroy(trace);
  }

  return  0;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int trace_test_language(NuSMVEnv_ptr env, FILE* out, FILE* err)
{
  Trace_ptr trace;
  SexpFsm_ptr sexp_fsm;
  SymbTable_ptr st;

  fprintf(out, "\n## Trace language ##\n");

  sexp_fsm = SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));
  SEXP_FSM_CHECK_INSTANCE(sexp_fsm);
  st = SexpFsm_get_symb_table(sexp_fsm);

  trace = Trace_create(st, NIL(char), TRACE_TYPE_SIMULATION,
                       SexpFsm_get_symbols_list(sexp_fsm), true);

  { /* test fsm symbols */
    NodeList_ptr symbs = SexpFsm_get_symbols_list(sexp_fsm);
    ListIter_ptr liter;

    NODE_LIST_FOREACH(symbs, liter) {
      node_ptr symb = NodeList_get_elem_at(symbs, liter);

      if (!Trace_symbol_in_language(trace, symb)) {
        return 1;
      }
    }
  }

  /* destroy it */
  Trace_destroy(trace);

  return 0;
}

static int trace_test_population (NuSMVEnv_ptr env, FILE* out, FILE* err)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  SexpFsm_ptr sexp_fsm = SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));
  SymbTable_ptr st;
  Trace_ptr trace;
  int i;

  SEXP_FSM_CHECK_INSTANCE(sexp_fsm);
  st = SexpFsm_get_symb_table(sexp_fsm);

  fprintf(out, "\n## Trace population ##\n");

  trace = Trace_create(st, NIL(char), TRACE_TYPE_SIMULATION,
                       SexpFsm_get_symbols_list(sexp_fsm), true);

  /* make up a few steps... */
  for (i = 0; i < 100; ++ i) {
    Trace_append_step(trace);
  }

  if (100 != Trace_get_length(trace)) {
    return 1;
  }

  { /* populate the trace with default values */
    TraceIter step;
    const TraceIter first = Trace_first_iter(trace);
    BoolEnc_ptr bool_enc = BOOL_ENC(NuSMVEnv_get_value(env, ENV_BOOL_ENCODER));
    i = 0;
    TRACE_FOREACH(trace, step) {
      TraceSymbolsIter sym_iter;
      node_ptr var;

      ++ i;
      TRACE_SYMBOLS_FOREACH(trace, TRACE_ITER_ALL_VARS, sym_iter, var) {
        node_ptr val = Trace_step_get_value(trace, step, var);
        if (Nil == val) { /* not assigned */
          BitValues_ptr bv  = BIT_VALUES(NULL);

          if (!SymbType_is_boolean(SymbTable_get_var_type(st, var))) {

            if (BoolEnc_is_var_bit(bool_enc, var)) {
              var = BoolEnc_get_scalar_var_from_bit(bool_enc, var);
              bv = BitValues_create(bool_enc, var);
            }
            else bv = BitValues_create(bool_enc, var);
          }

          /* no inputs on first step */
          if (!SymbTable_is_symbol_input_var(st, var) || (first != step)) {
            int tmp = Trace_step_put_value(trace, step, var,
                                 BIT_VALUES(NULL) != bv
                                 ? BoolEnc_get_value_from_var_bits(bool_enc,
                                                                   bv)
                                 : ExprMgr_false(exprs));

            if (!tmp) {
              StreamMgr_print_error(streams,  "halt\n");
              return 1;
            }
          }

          if (BIT_VALUES(NULL) != bv) { FREE(bv); }
        }
      }
    }
  }

  /* destroy it */
  Trace_destroy(trace);

  return 0;
}

static int trace_test_copy (NuSMVEnv_ptr env, FILE* out, FILE* err)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  SexpFsm_ptr sexp_fsm = \
    SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));
  SymbTable_ptr st;
  Trace_ptr trace;
  int i;

  SEXP_FSM_CHECK_INSTANCE(sexp_fsm);
  st = SexpFsm_get_symb_table(sexp_fsm);

  fprintf(out, "\n## Trace copy ##\n");

  trace = Trace_create(st, NIL(char), TRACE_TYPE_SIMULATION,
                       SexpFsm_get_symbols_list(sexp_fsm), true);

  /* make up a few steps... */
  for (i = 0; i < 10; ++ i) {
    Trace_append_step(trace);
  }

  if (10 != Trace_get_length(trace)) {
    return 1;
  }

  { /* populate the trace with default values */
    TraceIter step;
    const TraceIter first = Trace_first_iter(trace);
    BoolEnc_ptr bool_enc = BOOL_ENC(NuSMVEnv_get_value(env, ENV_BOOL_ENCODER));
    i = 0;
    TRACE_FOREACH(trace, step) {
      TraceSymbolsIter sym_iter;
      node_ptr var;

      ++ i;
      TRACE_SYMBOLS_FOREACH(trace, TRACE_ITER_ALL_VARS, sym_iter, var) {
        node_ptr val = Trace_step_get_value(trace, step, var);
        if (Nil == val) { /* not assigned */
          BitValues_ptr bv  = BIT_VALUES(NULL);
          if (!SymbType_is_boolean(SymbTable_get_var_type(st, var))) {

            if (BoolEnc_is_var_bit(bool_enc, var)) {
              var = BoolEnc_get_scalar_var_from_bit(bool_enc, var);
              bv = BitValues_create(bool_enc, var);
            }
            else bv = BitValues_create(bool_enc, var);
          }

          /* no inputs on first step */
          if (!SymbTable_is_symbol_input_var(st, var) || (first != step)) {
            Trace_step_put_value(trace, step, var,
                                 BIT_VALUES(NULL) != bv
                                 ? BoolEnc_get_value_from_var_bits(bool_enc,
                                                                   bv)
                                 : ExprMgr_false(exprs));
          }

          if (BIT_VALUES(NULL) != bv) { FREE(bv); }
        }
      }
    }
  }

  { /* make a full copy and compare the two traces */
    Trace_ptr copy = Trace_copy(trace, TRACE_END_ITER, true);
    if (!Trace_equals(trace, copy)) return 1;
    Trace_destroy(copy);
  }

  { /* do it again with frozen traces */
    Trace_ptr copy;
    Trace_freeze(trace);
    Trace_step_force_loopback(trace, Trace_ith_iter(trace, 3));

    copy = Trace_copy(trace, TRACE_END_ITER, true);
    if (!Trace_equals(trace, copy))
      return 1;

    Trace_destroy(copy);
  }

  Trace_destroy(trace);

  return 0;
}

static int trace_test_concat (NuSMVEnv_ptr env, FILE* out, FILE* err)
{
  fprintf(out, "\n## Trace concat ##\n");
  return 0;
}
#endif
