/* ---------------------------------------------------------------------------


  This file is part of the ``compile.dependency'' package of NuSMV version 2.
  Copyright (C) 2013 by FBK-irst.

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
  \author Sergio Mover
  \brief Implementation of class 'FormulaDependency'

  \todo: Missing description

*/


#include "nusmv/core/compile/dependency/FormulaDependency.h"
#include "nusmv/core/compile/dependency/FormulaDependency_private.h"
#include "nusmv/core/compile/dependency/DependencyBase.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/wff/ExprMgr.h"
#include "nusmv/core/parser/symbols.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'FormulaDependency_private.h' for class 'FormulaDependency' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief Name used to lookup the hash used to store dependencies.

  Name used to lookup the hash that associates to each
  formula the corresponding set of dependencies.

  (node_ptr)formula --> (Set_t)dependencies
  The formula is fully traversed and symbols are resolved.
*/
#define FORMULA_DEPENDENCY_HASH "fdh"


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void formula_dependency_finalize(Object_ptr object, void* dummy);

static assoc_retval formula_dependency_hash_free(char *key, char *data,
                                                 char * arg);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

FormulaDependency_ptr FormulaDependency_create(const NuSMVEnv_ptr env)
{
  FormulaDependency_ptr self = ALLOC(FormulaDependency, 1);
  FORMULA_DEPENDENCY_CHECK_INSTANCE(self);

  formula_dependency_init(self, env);
  return self;
}

void FormulaDependency_destroy(FormulaDependency_ptr self)
{
  FORMULA_DEPENDENCY_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

Set_t FormulaDependency_get_dependencies(FormulaDependency_ptr self,
                                         SymbTable_ptr symb_table,
                                         node_ptr formula,
                                         node_ptr context)
{
  return FormulaDependency_get_dependencies_by_type(self,
                                                    symb_table,
                                                    formula,
                                                    context, VFT_CNIF, false);
}

Set_t FormulaDependency_get_dependencies_by_type(FormulaDependency_ptr self,
                                                 SymbTable_ptr symb_table,
                                                 node_ptr formula, node_ptr context,
                                                 SymbFilterType filter,
                                                 boolean preserve_time)
{
  Set_t result;
  int temp;
  extern int nusmv_yylineno;
  hash_ptr dependencies_hash;

  if (formula == Nil)
      return Set_MakeEmpty();

  temp = nusmv_yylineno;
  nusmv_yylineno = node_get_lineno(formula);
  dependencies_hash = formula_dependency_get_hash(self, symb_table);

  result = formula_dependency_get_dependencies(self,
                                               symb_table,
                                               formula, context, filter,
                                               preserve_time,
                                               EXPR_UNTIMED_CURRENT,
                                               dependencies_hash);
  nusmv_yylineno = temp;

  return result;
}

Set_t FormulaDependency_formulae_get_dependencies(FormulaDependency_ptr self,
                                                  SymbTable_ptr symb_table,
                                                  node_ptr formula,
                                                  node_ptr justice,
                                                  node_ptr compassion)
{
  Set_t result1, result2, result3;

  result1 = FormulaDependency_get_dependencies(self, symb_table, formula, Nil);
  result2 = FormulaDependency_get_dependencies(self, symb_table, justice, Nil);
  result3 = FormulaDependency_get_dependencies(self, symb_table, compassion, Nil);

  result1 = Set_Union(result1, result2);
  result1 = Set_Union(result1, result3);
  Set_ReleaseSet(result3);
  Set_ReleaseSet(result2);
  return result1;
}


Set_t FormulaDependency_formulae_get_dependencies_by_type(FormulaDependency_ptr self,
                                                          SymbTable_ptr symb_table,
                                                          node_ptr formula,
                                                          node_ptr justice,
                                                          node_ptr compassion,
                                                          SymbFilterType filter,
                                                          boolean preserve_time)
{
  Set_t result1, result2, result3;

  result1 = FormulaDependency_get_dependencies_by_type(self, symb_table, formula, Nil, filter, preserve_time);
  result2 = FormulaDependency_get_dependencies_by_type(self, symb_table, justice, Nil, filter, preserve_time);
  result3 = FormulaDependency_get_dependencies_by_type(self, symb_table, compassion, Nil, filter, preserve_time);

  result1 = Set_Union(result1, result2);
  result1 = Set_Union(result1, result3);
  Set_ReleaseSet(result3);
  Set_ReleaseSet(result2);
  return result1;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void formula_dependency_init(FormulaDependency_ptr self, const NuSMVEnv_ptr env)
{
  /* base class initialization */
  master_node_walker_init(MASTER_NODE_WALKER(self), env);

  /* members initialization */

  /* [SM] TODO - instantiate a new hash in the env symbol table */


  /* virtual methods settings */
  OVERRIDE(Object, finalize) = formula_dependency_finalize;
}

void formula_dependency_deinit(FormulaDependency_ptr self)
{
  /* members deinitialization */
  /* /\* [SM] DEBUG */
  /*    Deinit the entries in the hash - this because the content of the */
  /*    hash is not freed when the symbol table is freed (see bug */
  /*    3995) *\/ */
  /* { */
  /*   hash_ptr dependencies_hash = formula_dependency_get_hash(self); */

  /*   clear_assoc_and_free_entries(dependencies_hash, */
  /*                                formula_dependency_hash_free); */
  /* } */

  /* base class deinitialization */
  master_node_walker_deinit(MASTER_NODE_WALKER(self));
}

Set_t formula_dependency_get_dependencies(FormulaDependency_ptr self,
                                          SymbTable_ptr symb_table,
                                          node_ptr formula, node_ptr context,
                                          SymbFilterType filter,
                                          boolean preserve_time, int time,
                                          hash_ptr dependencies_hash)
{
  ListIter_ptr iter;

  /* TODO[SM] For memoized results we pay the price of looking up the walker
     responsible of the node type */
  iter = NodeList_get_first_iter(MASTER_NODE_WALKER(self)->walkers);
  while (! ListIter_is_end(iter)) {
    DependencyBase_ptr pr =
      DEPENDENCY_BASE(NodeList_get_elem_at(MASTER_NODE_WALKER(self)->walkers,
                                           iter));

    if (NodeWalker_can_handle(NODE_WALKER(pr), formula)) {
      {
        Set_t result;
        Tuple5 key;

        /* Check if dependencies for formula have been computed before */
        formula_dependency_mk_hash_key(formula, context, filter, preserve_time,
                                       time, &key);
        result = formula_dependency_lookup_hash(dependencies_hash, NODE_PTR(&key));

        if (result == EMPTY_DEP_SET) {
          return Set_MakeEmpty();
        }

        if (result != (Set_t)NULL) {
          return Set_Copy(result);
        }
      }

      return DependencyBase_get_dependencies(pr, symb_table,
                                             formula, context,
                                             filter,
                                             preserve_time, time,
                                             dependencies_hash);
    }

    iter = ListIter_get_next(iter);
  }

  return Set_MakeEmpty();
}

Set_t formula_dependency_get_definition_dependencies(FormulaDependency_ptr self,
                                                     SymbTable_ptr symb_table,
                                                     node_ptr formula,
                                                     SymbFilterType filter,
                                                     boolean preserve_time, int time,
                                                     hash_ptr dependencies_hash)
{
  const NuSMVEnv_ptr env = ENV_OBJECT_GET_ENV(self);
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  Set_t result;
  if (SymbTable_is_symbol_var(symb_table, formula) ||
      SymbTable_is_symbol_parameter(symb_table, formula)) {
    if (((filter & VFT_INPUT) && SymbTable_is_symbol_input_var(symb_table,
                                                               formula)) ||
        ((filter & VFT_CURRENT) && SymbTable_is_symbol_state_var(symb_table,
                                                                 formula)) ||
        ((filter & VFT_FROZEN) && SymbTable_is_symbol_frozen_var(symb_table,
                                                                 formula)) ||
        SymbTable_is_symbol_parameter(symb_table, formula)) {
      if (preserve_time) {
        if ( EXPR_UNTIMED_NEXT == time &&
             !SymbTable_is_symbol_input_var(symb_table, formula) &&
             !SymbTable_is_symbol_parameter(symb_table, formula)) {
          formula = ExprMgr_next(exprs, formula, symb_table);
        }
        else if (time >= 0) {
          formula = ExprMgr_attime(exprs, formula, time, symb_table);
        }
      }

      /* TODO [AT]: should not we add the all elements of array if at
         least one element is if one element of array is in? */
      return Set_MakeSingleton((Set_Element_t) formula);
    }
    /* a variable filtered out */
    return Set_MakeEmpty();
  } /* end of outer if branch */

  if (SymbTable_is_symbol_function(symb_table, formula)) {
    if (filter & VFT_FUNCTION) {
      return Set_MakeSingleton((Set_Element_t) formula);
    }
    else return Set_MakeEmpty();
  }

  if (SymbTable_is_symbol_define(symb_table, formula)) {
    Tuple5 key;

    formula_dependency_mk_hash_key(formula, Nil, filter, preserve_time, time, &key);

    result = formula_dependency_lookup_hash(dependencies_hash, NODE_PTR(&key));
    if (result == BUILDING_DEP_SET) {
      ErrorMgr_error_circular(errmgr, formula); }

    if (result == EMPTY_DEP_SET) {
      nusmv_assert(!(filter & VFT_DEFINE));

      return Set_MakeEmpty();
    }

    if (result == (Set_t) NULL) {
      /* We mark the formula as open and we start looking for the body
         dependencies. */
      node_ptr nformula;
      formula_dependency_insert_hash(dependencies_hash, NODE_PTR(&key),
                                     BUILDING_DEP_SET);
      ErrorMgr_io_atom_push(errmgr, formula);
      nformula = SymbTable_get_define_body(symb_table, formula);
      result =
        formula_dependency_get_dependencies(self, symb_table, nformula,
                                            SymbTable_get_define_context(symb_table,
                                                                         formula),
                                            filter, preserve_time, time,
                                            dependencies_hash);
      ErrorMgr_io_atom_pop(errmgr);

      /* We add define to the result set if defines are include in filter */
      if (filter & VFT_DEFINE) {
        result = Set_AddMember(result, formula);
      }

      /* We mark the formula as closed, storing the computed
         dependencies for further use. */
      if (Set_IsEmpty(result)) {
        formula_dependency_close_define_hash(dependencies_hash, NODE_PTR(&key),
                                             EMPTY_DEP_SET);
      }
      else formula_dependency_close_define_hash(dependencies_hash, NODE_PTR(&key),
                                                result);
    }
    else {
      result = Set_Copy(result);
    }
  }
  else if (SymbTable_is_symbol_array_define(symb_table, formula)) {
    /* Recursively compute the dependencies for this define array */
    node_ptr nformula = SymbTable_get_array_define_body(symb_table, formula);
    result =
      formula_dependency_get_dependencies(self, symb_table, nformula,
                                          SymbTable_get_array_define_context(symb_table,
                                                                             formula),
                                          filter, preserve_time, time,
                                          dependencies_hash);

    /* We add the define array to the result set if defines are include in
       filter */
    if (filter & VFT_DEFINE) {
      result = Set_AddMember(result, formula);
    }
  }
  else if (SymbTable_is_symbol_variable_array(symb_table, formula)) {
    /* Array dependencies are all it's elements */

    SymbType_ptr type;
    int low, high;
    int i;

    type = SymbTable_get_variable_array_type(symb_table, formula);
    low = SymbType_get_array_lower_bound(type);
    high = SymbType_get_array_upper_bound(type);

    result = Set_MakeEmpty();

    for (i = low; i <= high; ++i) {
      node_ptr index = find_node(nodemgr, NUMBER, NODE_FROM_INT(i), Nil);
      node_ptr arr_var = find_node(nodemgr, ARRAY, formula, index);
      Set_t ret;

      ret = formula_dependency_get_definition_dependencies(self, symb_table,
                                                           arr_var, filter,
                                                           preserve_time, time,
                                                           dependencies_hash);

      result = Set_Union(result, ret);
    }

  }
  else if (SymbTable_is_symbol_constant(symb_table, formula) &&
           (ATOM == node_get_type(formula) ||
            DOT == node_get_type(formula) ||
            CONTEXT == node_get_type(formula))) {
    result = (VFT_CONSTANTS & filter) ?
      Set_MakeSingleton(formula) : Set_MakeEmpty();
  }
  else {
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    StreamMgr_print_error(streams,  "Undefined symbol \"");
    StreamMgr_nprint_error(streams, wffprint, "%N", formula);
    StreamMgr_print_error(streams,  "\"\n");
    ErrorMgr_nusmv_exit(errmgr, 1);
  }

  return result;
}

hash_ptr formula_dependency_get_hash(FormulaDependency_ptr self,
                                     SymbTable_ptr symb_table)

{
  return SymbTable_get_handled_hash_ptr(
                           symb_table,
                           FORMULA_DEPENDENCY_HASH,
                           (ST_PFICPCP) Tuple5_compare,
                           (ST_PFICPI) Tuple5_hash,
                           formula_dependency_hash_free,
                           (SymbTableTriggerFun) NULL,
                           SymbTable_clear_handled_remove_action_hash,
                           (SymbTableTriggerFun) NULL);

}

void formula_dependency_insert_hash(hash_ptr dependencies_hash,
                                    node_ptr key, Set_t value)
{
  nusmv_assert(dependencies_hash != (hash_ptr) NULL);
  nusmv_assert((Set_t) NULL == formula_dependency_lookup_hash(dependencies_hash,
                                                              key));

  if (IS_VALID_SET(value)) {
    value = Set_Freeze(value);
    value = Set_Copy(value);
  }

  key =
    NODE_PTR(Tuple5_create(Tuple5_get_first(TUPLE_5(key)),
                           Tuple5_get_second(TUPLE_5(key)),
                           Tuple5_get_third(TUPLE_5(key)),
                           Tuple5_get_forth(TUPLE_5(key)),
                           Tuple5_get_fifth(TUPLE_5(key))));

  Tuple5_freeze(TUPLE_5(key));

  insert_assoc(dependencies_hash, key, NODE_PTR(value));
}

void formula_dependency_close_define_hash(hash_ptr dependencies_hash,
                                          node_ptr key, Set_t value)
{
  nusmv_assert(dependencies_hash != (hash_ptr)NULL);
  nusmv_assert(formula_dependency_lookup_hash(dependencies_hash, key) ==
               BUILDING_DEP_SET);

  if (IS_VALID_SET(value)) {
    value = Set_Freeze(value);
    value = Set_Copy(value);
  }

  insert_assoc(dependencies_hash, key, NODE_PTR(value));
}

Set_t formula_dependency_lookup_hash(hash_ptr dependencies_hash, node_ptr key)
{
  nusmv_assert(dependencies_hash != (hash_ptr) NULL);

  return (Set_t) find_assoc(dependencies_hash, key);
}

void formula_dependency_mk_hash_key(node_ptr e, node_ptr c, SymbFilterType filter,
                                    boolean preserve_time, int time,
                                    Tuple5_ptr key)
{
  Tuple5_init(key,
              (void*)e,
              (void*)c,
              PTR_FROM_INT(void*, filter),
              PTR_FROM_INT(void*, preserve_time),
              PTR_FROM_INT(void*, time));

  Tuple5_freeze(key);
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The FormulaDependency class virtual finalizer

  Called by the class destructor
*/
static void formula_dependency_finalize(Object_ptr object, void* dummy)
{
  FormulaDependency_ptr self = FORMULA_DEPENDENCY(object);

  formula_dependency_deinit(self);
  FREE(self);
}

/*!
  \brief Free hash function for dependencies_hash

  This function has to be passed to
   SymbTable_get_handled_hash_ptr.

  \sa compile_cone_clear_st_handled_hash_free,
   SymbTable_get_handled_hash_ptr
*/
static assoc_retval formula_dependency_hash_free(char *key, char *data, char * arg)
{
  Set_t element;

  /* free key */
  Tuple5_destroy((Tuple5_ptr) key);

  /* free value */
  element = (Set_t)data;

  if (element != (Set_t)NULL && element != BUILDING_DEP_SET &&
      element != EMPTY_DEP_SET) {

    Set_ReleaseSet(element);
  }

  return ASSOC_DELETE;
}

/**AutomaticEnd***************************************************************/
