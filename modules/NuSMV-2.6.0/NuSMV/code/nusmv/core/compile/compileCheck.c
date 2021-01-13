/* ---------------------------------------------------------------------------


  This file is part of the ``compile'' package of NuSMV version 2.
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
  \brief Performs semantic checks on the model.

  The routines to perform some the semantic checks.<br>
  The performed checks are:
  <ul>
  <li>undefined symbols</li>
  <li>multiple definition of symbols</li>
  <li>circular definition of symbols</li>
  <li>assignment to input variables</li>
  </ul>

*/

#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"

#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/utils/Triple.h"
#include "nusmv/core/utils/Pair.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"

#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"

#include "nusmv/core/parser/symbols.h"

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CHECK_NEXT_HAS_NO_NEXT 1

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CHECK_NEXT_HAS_NEXT 2

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CHECK_NEXT_EVALUATING 3

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void compileCheckInitForInputVars(SymbTable_ptr, node_ptr);
static void compileCheckInvarForInputVars(SymbTable_ptr, node_ptr);
static void compileCheckTransForInputVars(Pair_ptr data, node_ptr);
static void compileCheckInvarSpecForInputVars(SymbTable_ptr symb_table,
                                              Pair_ptr data, node_ptr);
static void compileCheckLtlSpecForInputVars(SymbTable_ptr symb_table,
                                            Pair_ptr data, node_ptr);
static void compileCheckAssignForInputVars(Pair_ptr data, node_ptr,
                                           FlatHierarchy_ptr hierarchy);
static void compileCheckNoNextInputs(Pair_ptr data,
                                     node_ptr expr, node_ptr ctx);


static void check_circular_assign(Triple_ptr, node_ptr,
                                  node_ptr, boolean, boolean, boolean);
static void check_circ(Triple_ptr data, node_ptr n,
                       node_ptr context, boolean, boolean);

static boolean check_next(const Triple_ptr data,
                          node_ptr n,
                          node_ptr context,
                          boolean is_next);

static void check_case(const NuSMVEnv_ptr env,
                       node_ptr expr);

static void check_assign(Triple_ptr data, node_ptr n,
                         node_ptr context, int mode);

static void check_assign_both(Triple_ptr data, node_ptr v,
                              int node_type, int lineno);
static void error_circular_assign(Triple_ptr data, node_ptr n);
static void error_nested_next(Triple_ptr data, node_ptr s);
static void error_unexpected_next(Triple_ptr data, node_ptr s);
static void compile_check_print_io_atom_stack_assign(Triple_ptr data,
                                                     FILE*);
static hash_ptr compile_check_get_handled_hash(SymbTable_ptr, char*);
static void insert_assign_hash(hash_ptr assign_hash, node_ptr x, node_ptr y);
static node_ptr lookup_assign_hash(hash_ptr assign_hash, node_ptr x);
static void insert_check_next_hash(hash_ptr check_next_hash,
                                   node_ptr k, node_ptr v);
static void clear_check_next_hash(hash_ptr check_next_hash);
static node_ptr lookup_check_next_hash(hash_ptr check_next_hash,
                                 node_ptr k);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Compile_CheckAssigns(const SymbTable_ptr symb_table, node_ptr procs)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  node_ptr procs_list = procs;

  /* Initialization of the hashes */
  hash_ptr global_assign_hash = new_assoc();
  hash_ptr assign_hash = new_assoc();
  Triple data;

  Triple_init(&data, symb_table, global_assign_hash, assign_hash);

  while (procs_list) { /* Loops over processes */
    node_ptr context = car(car(procs_list));
    node_ptr assign_expr = cdr(car(procs_list));

    /* Checks for multiple assignments: */
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_nlog(logger, wffprint,
                  "checking for multiple assignments in process %N...\n", context);
    }

    check_assign(&data, assign_expr, Nil, 0);

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Done\n");
    }

    /* Checks for circular assignments: */
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_nlog(logger, wffprint,
                  "checking for circular assignments in process %N...\n", context);
    }

    check_assign(&data, assign_expr, Nil, 1);

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Done\n");
    }

    clear_assoc(assign_hash);

    procs_list = cdr(procs_list);
  }

  {
    SymbTableIter iter;

    /* checks state variables */
    SYMB_TABLE_FOREACH(symb_table, iter, STT_STATE_VAR | STT_FROZEN_VAR) {
      node_ptr v = SymbTable_iter_get_symbol(symb_table, &iter);
      int lineno = NODE_TO_INT(find_assoc(global_assign_hash, v));

      if (lineno != 0) {
        check_assign_both(&data, v, NEXT, lineno);
        check_assign_both(&data, v, SMALLINIT, lineno);
      }
    }
  }

  free_assoc(global_assign_hash);
  free_assoc(assign_hash);
}

void compileCheckForInputVars(SymbTable_ptr symb_table,
                              FlatHierarchy_ptr hierarchy)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  Pair data;
  /* The hash table to record which expressions already checked
   for correct use of input vars */
  hash_ptr check_inputs_hash;

  check_inputs_hash =
    compile_check_get_handled_hash(symb_table, ST_CHECK_INPUTS_HASH);

  Pair_init(&data, symb_table, check_inputs_hash);

  if (SymbTable_get_input_vars_num(symb_table) > 0) {
    node_ptr trans_expr = NULL;
    node_ptr init_expr = NULL;
    node_ptr invar_expr = NULL;
    node_ptr assign_expr = NULL;
    node_ptr invarspec = NULL;
    node_ptr ltlspec = NULL;

    trans_expr = FlatHierarchy_get_trans(hierarchy);
    init_expr = FlatHierarchy_get_init(hierarchy);
    invar_expr = FlatHierarchy_get_invar(hierarchy);
    assign_expr = FlatHierarchy_get_assign(hierarchy);
    invarspec = FlatHierarchy_get_invarspec(hierarchy);
    ltlspec = FlatHierarchy_get_ltlspec(hierarchy);

    if (opt_verbose_level_gt(opts, 2)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Check for input vars\n");
    }

    compileCheckInitForInputVars(symb_table, init_expr);
    compileCheckInvarForInputVars(symb_table, invar_expr);

    compileCheckTransForInputVars(&data, trans_expr);
    compileCheckAssignForInputVars(&data, assign_expr, hierarchy);

    compileCheckInvarSpecForInputVars(symb_table, &data, invarspec);
    compileCheckLtlSpecForInputVars(symb_table, &data, ltlspec);
  }
  else {
    if (opt_verbose_level_gt(opts, 2)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "No check for input vars\n");
    }
  }
}

void Compile_check_case(const SymbTable_ptr st, node_ptr expr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  check_case(env, expr);
}

void Compile_check_next(const SymbTable_ptr st,
                        node_ptr expr, node_ptr context,
                        boolean is_one_next_allowed)
{
  hash_ptr check_next_hash;
  Triple t;

  extern int nusmv_yylineno;
  int cur_lineno = nusmv_yylineno;

  check_next_hash =
    compile_check_get_handled_hash(st, ST_CHECK_NEXT_HASH);

  Triple_init(&t, st, NODE_FROM_INT(is_one_next_allowed), check_next_hash);

  if (expr == Nil) return;
  nusmv_yylineno = node_get_lineno(expr);
  check_next(&t, expr, context, false);
  nusmv_yylineno = cur_lineno;
}

void Compile_check_input_next(const SymbTable_ptr st,
                              node_ptr expr, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  /* The hash table to record which expressions already checked
     for correct use of input vars */
  hash_ptr check_inputs_hash =
    compile_check_get_handled_hash(st, ST_CHECK_INPUTS_HASH);

  Pair data;

  Pair_init(&data, st, check_inputs_hash);


  CATCH(errmgr) {
    compileCheckNoNextInputs(&data, expr, context);
  }
  FAIL(errmgr) {
    ErrorMgr_rpterr(errmgr, NULL); /* rethrow */
  }
}

/*!
  \brief Call SymbTable_get_handled_hash_ptr with proper arguments


*/

hash_ptr compile_check_get_handled_hash(SymbTable_ptr symb_table,
                                        char* hash_str)
{
  if (! strcmp(ST_CHECK_INPUTS_HASH, hash_str)) {
    return
      SymbTable_get_handled_hash_ptr(symb_table,
                                     ST_CHECK_INPUTS_HASH,
                                     (ST_PFICPCP)NULL,
                                     (ST_PFICPI)NULL,
                                     (ST_PFSR)NULL,
                                     (SymbTableTriggerFun)NULL,
                                     SymbTable_clear_handled_remove_action_hash,
                                     (SymbTableTriggerFun)NULL
                                     );
  }
  else if (! strcmp(ST_CHECK_NEXT_HASH, hash_str)) {
    return
      SymbTable_get_handled_hash_ptr(symb_table,
                                     ST_CHECK_NEXT_HASH,
                                     (ST_PFICPCP)NULL,
                                     (ST_PFICPI)NULL,
                                     (ST_PFSR)NULL,
                                     (SymbTableTriggerFun)NULL,
                                     SymbTable_clear_handled_remove_action_hash,
                                     (SymbTableTriggerFun)NULL
                                     );
  }
  else error_unreachable_code();
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Checks flattened init statement for input variables

  If the flattened init statement contains input
  variables then this function will print out an error message.
*/
static void compileCheckInitForInputVars(SymbTable_ptr symb_table,
                                         node_ptr init)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  Set_t deps = Formula_GetDependencies(symb_table, init, Nil);

  if (SymbTable_list_contains_input_var(symb_table, Set_Set2List(deps))) {
    ErrorMgr_error_init_exp_contains_input_vars(errmgr, init);
  }
  Set_ReleaseSet(deps);
}

/*!
  \brief Checks flattened invar statement for input variables

  If the flattened invar statement contains input
  variables then this function will print out an error message.
*/
static void compileCheckInvarForInputVars(SymbTable_ptr symb_table,
                                          node_ptr invar)
{
  Set_t deps = Formula_GetDependencies(symb_table, invar, Nil);

  if (SymbTable_list_contains_input_var(symb_table, Set_Set2List(deps))) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    ErrorMgr_error_invar_exp_contains_input_vars(errmgr, invar);
  }
  Set_ReleaseSet(deps);
}

/*!
  \brief Checks flattened trans statement for input variables

  If the flattened trans statement contains input
  variables within next() statements then this function will print out an
  error message.
*/
static void compileCheckTransForInputVars(Pair_ptr data,
                                          node_ptr trans)
{
  if (trans != Nil) {
    compileCheckNoNextInputs(data, trans, Nil);
  }
}

/*!
  \brief Checks the invarspecs for input variables within next


*/
static void compileCheckInvarSpecForInputVars(SymbTable_ptr symb_table,
                                              Pair_ptr data,
                                              node_ptr list)
{
  UNUSED_PARAM(symb_table);

  if (list != Nil) {
    node_ptr iter = NULL;
    node_ptr expr = NULL;

    nusmv_assert(CONS == node_get_type(list));

    NODE_CONS_LIST_FOREACH(expr, iter) {
      expr = Node_conslist_get(iter);

      nusmv_assert(CONTEXT == node_get_type(expr));

      compileCheckNoNextInputs(data, cdr(expr), car(expr));
    }
  }
}

/*!
  \brief Checks the ltlspecs for input variables within next


*/
static void compileCheckLtlSpecForInputVars(SymbTable_ptr symb_table,
                                            Pair_ptr data,
                                            node_ptr list)
{
  UNUSED_PARAM(symb_table);

  if (list != Nil) {
    node_ptr iter = NULL;
    node_ptr expr = NULL;

    nusmv_assert(CONS == node_get_type(list));

    NODE_CONS_LIST_FOREACH(expr, iter) {
      expr = Node_conslist_get(iter);

      nusmv_assert(CONTEXT == node_get_type(expr));

      compileCheckNoNextInputs(data, cdr(expr), car(expr));
    }
  }
}

/*!
  \brief Checks flattened assign statement for input variables

  If the flattened assign statement contains input
  variables then this function will print out an error message. Note that
  input variables are allowed in some parts of an assign statement. They're
  not allowed anywhere in an init section and cannot be contained within a
  next statement inside a next declaration.
*/
static void compileCheckAssignForInputVars(Pair_ptr data,
                                           node_ptr assign,
                                           FlatHierarchy_ptr hierarchy)
{
  SymbTable_ptr symb_table = SYMB_TABLE(Pair_get_first(data));
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (assign == Nil) return;

  switch (node_get_type(assign)) {
  case CONS:
  case AND:
    compileCheckAssignForInputVars(data, car(assign), hierarchy);
    compileCheckAssignForInputVars(data, cdr(assign), hierarchy);
    break;

  case DOT:
  case ARRAY: /* process name => skip it */

    /* [RC] confirm it is unclear
       First shot of this was in 9f647e57c63691e53fdcd6ab19d7bf8588226b7d
-  case DOT:
-    {
-      node_ptr lhsa = assign;
-      node_ptr stored = lookup_assign_db_hash(lhsa);
-
-      if (stored != (node_ptr) NULL) {
-        if (car(stored) != Nil) {
-          NodeList_ptr var_list =
-           Compile_get_expression_dependencies(symb_table, car(stored));
-
-         if (SymbTable_list_contains_input_var(symb_table, var_list)) {
-           error_assign_exp_contains_input_vars(assign);
-         }
-         NodeList_destroy(var_list);
-        }
-      }
-    }
+
+  case DOT: // process name => skip it
    */

    break;

  case EQDEF: {
    node_ptr stored;
    node_ptr name = car(assign);
    nusmv_yylineno = node_get_lineno(assign);

    switch (node_get_type(name)) {
    case ARRAY:
      if (SymbTable_is_symbol_input_var(symb_table, name)) {
        ErrorMgr_error_assign_exp_contains_input_vars(errmgr, name);
      }
      break;

    case NEXT:
      /* We don't care about presence of input vars in next assign,
         but we check for the presence of references to next of input
         variables. Defines are taken into account by expanding them
         before performing this check. */
      name = find_atom(nodemgr, name);
      stored = FlatHierarchy_lookup_assign(hierarchy, name);

      if (Nil != stored) {
        /* checks that the right value does not contain next(inputs) */
        compileCheckNoNextInputs(data, stored, Nil);
      }

      break;

    case DOT: /* only resolved identifiers can be here */
    case SMALLINIT:
      if (SMALLINIT == node_get_type(name)) name = find_atom(nodemgr, name);
      /* For normal assignments and init assignments we verify the rhs
         does not contain input variables. In this respect we have to
         look at the flattened assign, since from flattened symbols we
         can see whether they are input or state variables. */
      stored = FlatHierarchy_lookup_assign(hierarchy, name);

      if (Nil != stored) {
        Set_t deps = Formula_GetDependencies(symb_table, stored, Nil);
        if (SymbTable_list_contains_input_var(symb_table, Set_Set2List(deps))) {
          ErrorMgr_error_assign_exp_contains_input_vars(errmgr, name);
        }
        Set_ReleaseSet(deps);
      }
      break;

    default:
      StreamMgr_print_error(streams,
              "compileCheckAssignForInputVars: unrecognised token (%d)\n",
              node_get_type(name));
      ErrorMgr_internal_error(errmgr, "");
    } /* internal (EQDEF) switch */

    break;
  }

  default:
    StreamMgr_print_error(streams,  "compileCheckAssignForInputVars: unknown token (%d)\n",
            node_get_type(assign));
    ErrorMgr_internal_error(errmgr, "");

  } /* switch */
}

/*!
  \brief Checks expression for input variables in next statements

  It outputs an error message (and rises an exception)
  iff the expression contains a next statement which itself has an
  input variable in it.
*/
static void compileCheckNoNextInputs(Pair_ptr data,
                                     node_ptr expr, node_ptr ctx)
{
  const SymbTable_ptr symb_table = SYMB_TABLE(Pair_get_first(data));
  hash_ptr check_inputs_hash = (hash_ptr)Pair_get_second(data);

  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  node_ptr expr_to_remember = Nil; /* expression which will be memoized */

  if (expr == Nil) return;

  switch (node_get_type(expr)) {
  case FAILURE:
  case NUMBER:
  case TRUEEXP:
  case FALSEEXP:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case UWCONST:
  case SWCONST:
    return;

  case ATOM:
  case BIT:
  case DOT:
  case ARRAY:
    {
      ResolveSymbol_ptr rs;
      node_ptr resName = NULL;
      node_ptr flat_complex_expr = NULL;
      node_ptr new_body = NULL;
      node_ptr new_ctx = NULL;

      rs = SymbTable_resolve_symbol(symb_table, expr, ctx);
      resName = ResolveSymbol_get_resolved_name(rs);


      /* dotted constants or a variable, ar an array */
      if (ResolveSymbol_is_constant(rs) ||
          ResolveSymbol_is_var(rs) ||
          ResolveSymbol_is_function(rs) ||
          ResolveSymbol_is_array(rs)) {
        return;
      }

      /* this identifier is a complex expression and may have
         been processed already */
      expr_to_remember = resName;
      if (find_assoc(check_inputs_hash, expr_to_remember)) return;

      /* is this a define ? -> recur into flattened body */
      if (ResolveSymbol_is_define(rs)) {
        flat_complex_expr = SymbTable_get_define_flatten_body(symb_table, resName);
      }
      /* is this a array define ? -> recur into flattened body */
      else if (ResolveSymbol_is_array_def(rs)) {
        flat_complex_expr = SymbTable_get_array_define_flatten_body(symb_table, resName);
      }
      /* or a parameter ? */
      else if (ResolveSymbol_is_parameter(rs)) {
        flat_complex_expr = SymbTable_get_flatten_actual_parameter(symb_table, resName);
      }
      /* unknown kind of identifier */
      else if (ResolveSymbol_is_error(rs)) {
        /* undefined because complex array index, just have a look into it. We
           also set expr_to_remember, avoiding the flattening */
        if (ARRAY == node_get_type(expr) &&
            ResolveSymbol_is_undefined(rs)) {
          flat_complex_expr = cdr(expr);
          expr_to_remember = expr;
        }
        else ResolveSymbol_throw_error(rs, env);
      }
      else {
        ErrorMgr_internal_error(errmgr, "impossible code in function compileCheckNoNextInputs");
      }

      /* here we have to pass Nil context, since flat_complex_expr is
         flattened */
      nusmv_assert(expr != flat_complex_expr);

      compileCheckNoNextInputs(data, flat_complex_expr, Nil);
    }
    break;

  case CONTEXT:
    expr_to_remember = expr;
    compileCheckNoNextInputs(data, cdr(expr), car(expr));
    break;

  case NEXT:
    {  /* this expr may have been processed already */
      Set_t deps;
      boolean res;

      expr_to_remember = find_node(nodemgr, CONTEXT, ctx, expr);
      if (find_assoc(check_inputs_hash, expr_to_remember))
          return;

      deps = Formula_GetDependencies(symb_table, expr, ctx);
      res = SymbTable_list_contains_input_var(symb_table,
                                              Set_Set2List(deps));
      Set_ReleaseSet(deps);

      if (res) {
        extern int nusmv_yylineno;
        nusmv_yylineno = node_get_lineno(expr);
        ErrorMgr_error_next_exp_contains_input_vars(errmgr, expr);
        /* this code does not return */
      }
    }
    break;

  default:
    /* this expr may have been processed already */
    expr_to_remember = find_node(nodemgr, CONTEXT, ctx, expr);
    if (find_assoc(check_inputs_hash, expr_to_remember)) return;

    compileCheckNoNextInputs(data, car(expr), ctx);
    compileCheckNoNextInputs(data, cdr(expr), ctx);
    break;
  }

  /* remember the expression already processed.  Note: here only
     complex expressions are memorized. The simple expressions such as
     numbers, constants and vars are not in hash (hopefully decreasing such
     a ways the size of the hash). */
  insert_assoc(check_inputs_hash, expr_to_remember, NODE_PTR(1));

  nusmv_assert(expr_to_remember != Nil); /* the expr has to be a real expression */
  return;
}

/*!
  \brief Checks for circular definitions.

  This function checks for circular definition of
  any kind. This function is able to detect circularity of the
  following kinds:
  <ul>
     <li><code>next(x) := alpha(next(x))<code></li>
     <li><code>next(x) := next(alpha(x))<code></li<
     <li>any combination of the two above.</li>
     <li><code>x := alpha(x)</code>
  </ul>
  where <code>alpha(x)</code> (<code>alpha(next(x))</code>) is a
  formula in which the variable <code>x</code> (<code>next(x)</code>)
  occurs. Notice that <code>next(alpha(x))</code> can be rewritten in
  term of <code>next(x)</code>, since the next operator distributes
  along all kind of operators.<br>

  Here we check also the case where we have next(x), and x is a symbol
  declared as DEFINE whose body contain a next(v).  These kind of
  formulas cannot be checked at parsing time, since, it would require
  to knowledge of part of the model that might be possibly parsed
  later. And removing next from the body of DEFINE is a too
  restrictive choice.
*/
static void check_circ(Triple_ptr data, node_ptr n,
                       node_ptr context,
                       boolean is_next, boolean lhs_is_next)
{
  const SymbTable_ptr symb_table = SYMB_TABLE(Triple_get_first(data));
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));

  if (n == Nil) return;

  switch (node_get_type(n)) {

  case FAILURE:
  case NUMBER:
  case TRUEEXP:
  case FALSEEXP:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case UWCONST:
  case SWCONST:
    return;

  case BIT:
  case DOT:
  case ATOM: {
    ResolveSymbol_ptr rs;
    node_ptr name;

    rs = SymbTable_resolve_symbol(symb_table, n, context);
    name = ResolveSymbol_get_resolved_name(rs);

    if (ResolveSymbol_is_error(rs)) ResolveSymbol_throw_error(rs, env);

    if (ResolveSymbol_is_parameter(rs)) {
      node_ptr par = SymbTable_get_actual_parameter(symb_table, name);
      node_ptr ctx = SymbTable_get_actual_parameter_context(symb_table, name);
      check_circ(data, par, ctx, is_next, lhs_is_next);
      return;
    }

    if (ResolveSymbol_is_constant(rs)) return;

    check_circular_assign(data, name, context, is_next, false,
                          lhs_is_next);
    return;
  } /* end of case ATOM */

  case ARRAY:
    {
      ResolveSymbol_ptr rs;
      node_ptr t;

      rs = SymbTable_resolve_symbol(symb_table, n, context);
      t = ResolveSymbol_get_resolved_name(rs);

      if (ResolveSymbol_is_undefined(rs)) {
        /* this is not identifier but array expression */
        check_circ(data, car(n), context, is_next, lhs_is_next);
        check_circ(data, cdr(n), context, is_next, lhs_is_next);
      }
      else {
        /* this is identifier-with-brackets */
        check_circular_assign(data, t, context, is_next, false,
                              lhs_is_next);
      }
      return;
    }

  case CONTEXT:
    check_circ(data, cdr(n), car(n), is_next, lhs_is_next);
    return;

  case NEXT:
    /* handling of hidden next not easy to detect syntactically */
    if (is_next) { error_nested_next(data, n); }
    if (!lhs_is_next) { error_unexpected_next(data, n); }

    check_circ(data, car(n), context, true, lhs_is_next);
    return;

  default:
    check_circ(data, car(n), context, is_next, lhs_is_next);
    check_circ(data, cdr(n), context, is_next, lhs_is_next);
  }

}

/*!
  \brief Performs circular assignment checking

  Checks for circular assignments in the model. If
  there are any, then an error is generated. NEXT operator, if any,
  must be stripped away from given expression 'n', and in that case is_next
  must be set to true. Parameter is_lhs is true at the first call (done
  with the first left-hand-side value (the assigned value)
*/
static void check_circular_assign(Triple_ptr data, node_ptr n,
                                  node_ptr context, boolean is_next,
                                  boolean is_lhs, boolean lhs_is_next)
{
  const SymbTable_ptr symb_table = SYMB_TABLE(Triple_get_first(data));
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  hash_ptr assign_hash = (hash_ptr)Triple_get_third(data);

  node_ptr t;
  node_ptr next_n;
  boolean is_rhs_next;

  if ((n != Nil) && (is_next) && (node_get_type(n) == NUMBER)) return;

  next_n = find_node(nodemgr, NEXT, n, Nil);
  if (is_next) {
    t = lookup_assign_hash(assign_hash, next_n);
  }
  else {
    /* check if it is a next assignment or a normal assignment */
    t = lookup_assign_hash(assign_hash, n);
    if (t == Nil) {
      /* check if it is an init assignment */
      t = lookup_assign_hash(assign_hash,
                             find_node(nodemgr, SMALLINIT, n, Nil));
    }
  }

  if (t == CLOSED_NODE) return;
  if (t == FAILURE_NODE) { error_circular_assign(data, n); }

  if (t == Nil) {
    /* it might be a define: */
    if (SymbTable_is_symbol_define(symb_table, n)) {
      /* switch to define ctx and body, and continue: */
      context = SymbTable_get_define_context(symb_table, n);
      t = SymbTable_get_define_body(symb_table, n);
      is_rhs_next = false; /* this actually is a don't care */
    }
    else return;
  }
  else {
    is_rhs_next = (node_get_type(t) == NEXT);
    if (!is_lhs && is_next && is_rhs_next) { error_nested_next(data, n); }

    is_lhs = true; /* we found an assignment: restart the search */
  }

  if (t == Nil) {
    if (SymbTable_is_symbol_constant(symb_table, n)) return;
    else ErrorMgr_error_undefined(errmgr, n);
  }

  /* [AT] unclear how arrays are processed, element by element or as a
     whole?
     Single elements of array may depend on other elements of the same
     array.  Probably, if array is in dependency then every element
     has to be added to dependency as well.
   */

  insert_assign_hash(assign_hash, is_next ? next_n : n, FAILURE_NODE);
  ErrorMgr_io_atom_push(errmgr, is_next? next_n : n);

  /* if this is the first time this function is called, rhs decides if
     there is NEXT operator, otherwise keeps the current mode */
  check_circ(data, t, context, is_lhs? is_rhs_next : is_next,
             lhs_is_next);

  ErrorMgr_io_atom_pop(errmgr);
  insert_assign_hash(assign_hash, is_next ? next_n : n, CLOSED_NODE);
}

/*!
  \brief Checks for multiple or circular assignments.

  This function detects either multiple or
  circular assignments in "context" regarding to "mode".
  If mode is equal to 0 (zero) then it checks for multiple assignments
  or symbols redefinition. Otherwise it performs checks for circular
  assignments.
*/
static void check_assign(Triple_ptr data, node_ptr n,
                         node_ptr context, int mode)
{
  const SymbTable_ptr symb_table = SYMB_TABLE(Triple_get_first(data));
  hash_ptr global_assign_hash = (hash_ptr)Triple_get_second(data);
  hash_ptr assign_hash = (hash_ptr)Triple_get_third(data);

  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (n == Nil) return;
  nusmv_yylineno = node_get_lineno(n);

  switch (node_get_type(n)) {
  case AND:
    check_assign(data, car(n), context, mode);
    check_assign(data, cdr(n), context, mode);
    break;

  case CONTEXT:
    check_assign(data, cdr(n), car(n), mode);
    break;

  case EQDEF:
    {
      node_ptr t1, t2;
      ResolveSymbol_ptr rs;

      if ((node_get_type(car(n)) == SMALLINIT) ||
          (node_get_type(car(n)) == NEXT)) {
        rs = SymbTable_resolve_symbol(symb_table, car(car(n)), context);

        t1 = ResolveSymbol_get_resolved_name(rs);
        t2 = find_node(nodemgr, node_get_type(car(n)), t1, Nil);
      }
      else {
        rs = SymbTable_resolve_symbol(symb_table, car(n), context);

        t1 = t2 = ResolveSymbol_get_resolved_name(rs);
      }

      if (mode == 0) {
        /* Checking for multiple assignments */
        if (! SymbTable_is_symbol_declared(symb_table, t1)) { ErrorMgr_error_undefined(errmgr, t1); }
        if (SymbTable_is_symbol_input_var(symb_table, t1)) {
          ErrorMgr_error_assign_input_var(errmgr, car(n));
        }
        if (SymbTable_is_symbol_frozen_var(symb_table, t1) &&
            SMALLINIT != node_get_type(car(n))) {
          ErrorMgr_error_assign_frozen_var(errmgr, car(n));
        }
        if (! SymbTable_is_symbol_state_frozen_var(symb_table, t1)) {
          /* How it can be not state or frozen variable ?*/
          ErrorMgr_error_redefining(errmgr, t1);
        }

        if (lookup_assign_hash(assign_hash, t2)) {
          ErrorMgr_error_multiple_assignment(errmgr, t2);
        }

        insert_assign_hash(assign_hash, t2,
                           find_node(nodemgr, CONTEXT, context, cdr(n)));
        insert_assoc(global_assign_hash, t2, NODE_FROM_INT(nusmv_yylineno));
      }
      else { /* Checking for circular assignments */
        if (node_get_type(t2) == NEXT) {
          check_circular_assign(data, car(t2), context, true, true, true);
        }
        else {
          check_circular_assign(data, t2, context, false, true, false);
        }
      }
      break;
    }

  default:
    ErrorMgr_internal_error(errmgr, "check_assign: type = %d", node_get_type(n));
  }
}

/*!
  \brief Given a variable, it checks if there are
  multiple assignments to it.

  Checks if there exists in the model an
  assignments of type <tt>node_type</tt> for variable <tt>v</tt>. If
  such an assignment exists, then an error is generated.
*/
static void check_assign_both(Triple_ptr data, node_ptr v,
                              int node_type, int lineno)
{
  SymbTable_ptr symb_table = SYMB_TABLE(Triple_get_first(data));
  hash_ptr global_assign_hash = (hash_ptr)Triple_get_second(data);

  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  node_ptr v1 = find_node(nodemgr, node_type, v, Nil);
  int lineno2 = NODE_TO_INT(find_assoc(global_assign_hash, v1));

  if (lineno2) ErrorMgr_error_assign_both(errmgr, v, v1, lineno, lineno2);
}

/*!
  \brief


*/
static void compile_check_print_io_atom_stack_assign(Triple_ptr data, FILE * fd)
{
  SymbTable_ptr symb_table = SYMB_TABLE(Triple_get_first(data));
  hash_ptr global_assign_hash = (hash_ptr)Triple_get_second(data);

  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  while(!ErrorMgr_io_atom_is_empty(errmgr)){
    node_ptr s = ErrorMgr_io_atom_head(errmgr);

    ErrorMgr_io_atom_pop(errmgr);
    fprintf(fd, "in definition of ");
    print_node(wffprint, fd, s);
    {
      int lineno = NODE_TO_INT(find_assoc(global_assign_hash, s));

      if (lineno) fprintf(fd," at line %d", lineno);
      fprintf(fd, "\n");
    }
  }
}

/*!
  \brief


*/
static void error_circular_assign(Triple_ptr data, node_ptr s)
{
  SymbTable_ptr symb_table = SYMB_TABLE(Triple_get_first(data));
  hash_ptr assign_hash = (hash_ptr)Triple_get_third(data);

  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));

  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  ErrorMgr_start_parsing_err(errmgr);
  StreamMgr_print_error(streams,  "recursively defined: ");
  StreamMgr_nprint_error(streams, wffprint, "%N", s);
  StreamMgr_print_error(streams, "\n");
  compile_check_print_io_atom_stack_assign(data, errstream);
  clear_assoc(assign_hash);
  ErrorMgr_finish_parsing_err(errmgr);
}

/*!
  \brief


*/
static void error_nested_next(Triple_ptr data, node_ptr s)
{
  SymbTable_ptr symb_table = SYMB_TABLE(Triple_get_first(data));
  hash_ptr assign_hash = (hash_ptr)Triple_get_third(data);

  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));

  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  ErrorMgr_start_parsing_err(errmgr);
  StreamMgr_print_error(streams,  "nested NEXT operators: ");
  StreamMgr_nprint_error(streams, wffprint, "%N", s);
  StreamMgr_print_error(streams, "\n");
  compile_check_print_io_atom_stack_assign(data, errstream);
  clear_assoc(assign_hash);
  ErrorMgr_finish_parsing_err(errmgr);
}

/*!
  \brief


*/
static void error_unexpected_next(Triple_ptr data, node_ptr s)
{
  SymbTable_ptr symb_table = SYMB_TABLE(Triple_get_first(data));
  hash_ptr assign_hash = (hash_ptr)Triple_get_third(data);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));

  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  ErrorMgr_start_parsing_err(errmgr);
  StreamMgr_print_error(streams,  "found unexpected next operator: ");
  StreamMgr_nprint_error(streams, wffprint, "%N", s);
  StreamMgr_print_error(streams, "\n");
  compile_check_print_io_atom_stack_assign(data, errstream);
  clear_assoc(assign_hash);
  ErrorMgr_finish_parsing_err(errmgr);
}

/*!
  \brief


*/
static void check_case(const NuSMVEnv_ptr env, node_ptr expr)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (Nil == expr) return;

  switch (node_get_type(expr)) {

  case ATOM:
  case DOT:
  case TRUEEXP:
  case FALSEEXP:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case UWCONST:
  case SWCONST:
    return;

  case FAILURE: ErrorMgr_internal_error(errmgr, "%s:%d:%s %s", __FILE__, __LINE__, __func__,
                               "unexpected FAILURE node");

  case IFTHENELSE:
  case CASE:
      if (node_get_type(cdr(expr)) == FAILURE) {
        /* checks that the last condition is 1 */
        nusmv_assert((node_get_type(car(expr)) == COLON));
        if (!(((node_get_type(car(car(expr))) == NUMBER) &&
               (node_get_int(car(car(expr))) == 1))
              ||
              (node_get_type(car(car(expr))) == TRUEEXP)))
          ErrorMgr_warning_case_not_exhaustive(errmgr, cdr(expr));
      }
      check_case(env, cdr(car(expr)));

  default :
    if (Nil != cdr(expr)) check_case(env, cdr(expr));
    if (Nil != car(expr)) check_case(env, car(expr));
  }
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static boolean check_next(const Triple_ptr data,
                          node_ptr n,
                          node_ptr context,
                          boolean is_next)
{
  hash_ptr check_next_hash = (hash_ptr)Triple_get_third(data);

  boolean result = false;

  if (Nil == n) return result;

  if (node_is_leaf(n)) return result;

  switch (node_get_type(n)) {

    /* Array is treated as a normal expression: we are just checking
       for next operators!! */
  case DOT:
  case BIT:
  case ATOM:
    {
      SymbTable_ptr symb_table = SYMB_TABLE(Triple_get_first(data));
      const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

      ResolveSymbol_ptr rs;
      node_ptr res;
      node_ptr name;

      rs = SymbTable_resolve_symbol(symb_table, n, context);
      name = ResolveSymbol_get_resolved_name(rs);

      res = lookup_check_next_hash(check_next_hash, name);

      if (res != Nil) {
        int has_next = NODE_TO_INT(res);
        /* If the expression contains next and we are already in a
           next operator, or next are not allowed */
        if (CHECK_NEXT_HAS_NEXT == has_next) {
          boolean is_next_allowed = NODE_TO_INT(Triple_get_second(data));

          if (is_next) {
            clear_check_next_hash(check_next_hash);
            ErrorMgr_rpterr(errmgr, "Nested next operator.\n");
          }
          if (!is_next_allowed) {
            clear_check_next_hash(check_next_hash);
            ErrorMgr_rpterr(errmgr, "Unexpected next operator.\n");
          }

          /* return TRUE, expression contains a next. */
          return true;
        }
        if (CHECK_NEXT_EVALUATING == has_next) {
          clear_check_next_hash(check_next_hash);
          nusmv_yylineno = node_get_lineno(n);
          ErrorMgr_error_circular(errmgr, n);
        }
        return false;
      }

      if (ResolveSymbol_is_ambiguous(rs)) {
        clear_check_next_hash(check_next_hash);
        ErrorMgr_error_ambiguous(errmgr, name);
      }

      if (ResolveSymbol_is_constant(rs)) { return false; }

      /* while evaluating the node, mark it to find circular
         dependencies */
      insert_check_next_hash(check_next_hash, name,
                             NODE_FROM_INT(CHECK_NEXT_EVALUATING));

      if (ResolveSymbol_is_defined(rs)) {
        if (ResolveSymbol_is_define(rs)) {
          node_ptr ctx, body;
          body = SymbTable_get_define_body(symb_table, name);
          ctx = SymbTable_get_define_context(symb_table, name);
          result = check_next(data, body, ctx, is_next);
        }
        else if (ResolveSymbol_is_parameter(rs)) {
          node_ptr ctx, body;
          body = SymbTable_get_actual_parameter(symb_table, name);
          ctx = SymbTable_get_actual_parameter_context(symb_table, name);
          result = check_next(data, body, ctx, is_next);
        }
        else if (ResolveSymbol_is_array_def(rs)) {
          node_ptr ctx, body;
          body = SymbTable_get_array_define_body(symb_table, name);
          ctx = SymbTable_get_array_define_context(symb_table, name);
          result = check_next(data, body, ctx, is_next);
        }
        /* If the assertion fails, it may be that a new kind of symbol
           has been added to the symbol table. add the proper case in
           the condition below: */
        else if (ResolveSymbol_is_var(rs) ||
                 ResolveSymbol_is_function(rs) ||
                 ResolveSymbol_is_array(rs) ||
                 ResolveSymbol_is_constant(rs)) {
          /* variables/constant/function cannot have "next" */
        }
        else {
          error_unreachable_code_msg("Compile_check_next: Unsupported symbol found.\n");
        }
      }
      else {
        /* The identifier is not declared. We do not rise an assertion
           because an identifier introduced by PSL forall operator is
           not declared anywhere. We assume that such an identifier is
           met.

           If in future PSL forall identifier is declared then
           here "error_unreachable_code();" should be added.
        */
      }

      /* stores the result in the cache */
      insert_check_next_hash(check_next_hash, name, NODE_FROM_INT(result ? \
                                                                  CHECK_NEXT_HAS_NEXT : CHECK_NEXT_HAS_NO_NEXT));
    }
    break;

  case CONTEXT:
    result = check_next(data, cdr(n), car(n), is_next);
    break;

  case NEXT:
    {
      boolean is_next_allowed = NODE_TO_INT(Triple_get_second(data));
      SymbTable_ptr symb_table = SYMB_TABLE(Triple_get_first(data));
      const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));

      /* handling of hidden next not easy to detect syntactically */
      if (is_next) {
        const ErrorMgr_ptr errmgr =
          ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

        clear_check_next_hash(check_next_hash);
        ErrorMgr_rpterr(errmgr, "Nested next operator.\n");
      }
      if (!is_next_allowed) {
        const ErrorMgr_ptr errmgr =
          ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

        clear_check_next_hash(check_next_hash);
        ErrorMgr_rpterr(errmgr, "Unexpected next operator.\n");
      }

      check_next(data, car(n), context, true);
      result = true;
      break;
    }

  default:
    result = check_next(data, car(n), context, is_next);
    result |= check_next(data, cdr(n), context, is_next);
    break;
  } /* switch(node_get_type(n)) */

  return result;
}

/*!
  \brief Inserting function for the assign_hash

  Inserting function for the assign_hash
*/
static void insert_assign_hash(hash_ptr assign_hash, node_ptr x, node_ptr y)
{
  insert_assoc(assign_hash, x, y);
}

/*!
  \brief Lookup function for the assign_hash

  Lookup function for the assign_hash
*/
static node_ptr lookup_assign_hash(hash_ptr assign_hash, node_ptr x)
{
  return(find_assoc(assign_hash, x));
}

/*!
  \brief Inserting function for the check_next_hash

  Inserting function for the check_next_hash
*/
static void insert_check_next_hash(hash_ptr check_next_hash,
                                   node_ptr k, node_ptr v)
{
  insert_assoc(check_next_hash, k, v);
}

/*!
  \brief Lookup function for the check_next_hash

  Lookup function for the check_next_hash
*/
static node_ptr lookup_check_next_hash(hash_ptr check_next_hash,
                                       node_ptr k)
{
  return find_assoc(check_next_hash, k);
}

/*!
  \brief Clear the check next hash

  Clear the check next hash
*/
static void clear_check_next_hash(hash_ptr check_next_hash)
{
  clear_assoc(check_next_hash);
}
