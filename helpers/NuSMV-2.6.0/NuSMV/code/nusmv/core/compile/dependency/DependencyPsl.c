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
  \author Sergio Mover , split in Psl Rade Rudic
  \brief Implementation of class 'DependencyPsl'

  \todo: Missing description

*/


#include "nusmv/core/compile/dependency/DependencyPsl.h"
#include "nusmv/core/compile/dependency/DependencyPsl_private.h"

#include "nusmv/core/compile/dependency/FormulaDependency.h"
#include "nusmv/core/compile/dependency/FormulaDependency_private.h"

#include "nusmv/core/parser/symbols.h"

#include "nusmv/core/parser/psl/psl_symbols.h"
#include "nusmv/core/parser/psl/pslNode.h"


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'DependencyPsl_private.h' for class 'DependencyPsl' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void dependency_psl_finalize(Object_ptr object, void* dummy);

static Set_t
dependency_psl_get_dependencies(DependencyBase_ptr self,
                                 SymbTable_ptr symb_table,
                                 node_ptr formula, node_ptr context,
                                 SymbFilterType filter,
                                 boolean preserve_time, int time,
                                 hash_ptr dependencies_hash);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

DependencyPsl_ptr DependencyPsl_create(const NuSMVEnv_ptr env,
                                         const char* name)
{
  DependencyPsl_ptr self = ALLOC(DependencyPsl, 1);
  DEPENDENCY_PSL_CHECK_INSTANCE(self);

  dependency_psl_init(self, env, name,
                      NUSMV_PSL_SYMBOL_FIRST,
                      NUSMV_PSL_SYMBOL_LAST - NUSMV_PSL_SYMBOL_FIRST);
  return self;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void dependency_psl_init(DependencyPsl_ptr self,
                          const NuSMVEnv_ptr env,
                          const char* name,
                          int low, size_t num)
{
  /* base class initialization */
  dependency_base_init(DEPENDENCY_BASE(self), env, name, low, num, true);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = dependency_psl_finalize;
  OVERRIDE(DependencyBase, get_dependencies) =
    dependency_psl_get_dependencies;
}

void dependency_psl_deinit(DependencyPsl_ptr self)
{
  /* members deinitialization */


  /* base class deinitialization */
  dependency_base_deinit(DEPENDENCY_BASE(self));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The DependencyPsl class virtual finalizer

  Called by the class destructor
*/
static void dependency_psl_finalize(Object_ptr object, void* dummy)
{
  DependencyPsl_ptr self = DEPENDENCY_PSL(object);

  dependency_psl_deinit(self);
  FREE(self);
}

/*!
  \brief Get the dependencies of formula


*/
static Set_t
dependency_psl_get_dependencies(DependencyBase_ptr self,
                                 SymbTable_ptr symb_table,
                                 node_ptr formula, node_ptr context,
                                 SymbFilterType filter,
                                 boolean preserve_time, int time,
                                 hash_ptr dependencies_hash)
{
  const NuSMVEnv_ptr env = ENV_OBJECT_GET_ENV(self);
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs =
    EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  Set_t result;
  Tuple5 key;
  boolean is_to_be_inserted;
  PslNode_ptr formula_psl;

  is_to_be_inserted = true;

  /* 0 for filter means no variables are looked for. Do not create
     a special constant in SymbFilterType, otherwise it will be
     visible outside (which may be not good). */
  if (formula == Nil || filter == 0)
    return Set_MakeEmpty();

  /* [SM] Memoization is checked before calling the get_dependencies
     function.  This avoids to check memoization in each implemented
     psl.
   */
  formula_dependency_mk_hash_key(formula, context, filter,
                                 preserve_time, time, &key);

  formula_psl = PslNode_convert_from_node_ptr(formula);

  switch (psl_node_get_op(formula_psl)) {
  /* "start PSL:" */
  case PSL_INF:
  case PSL_RANGE:
    result = Set_MakeEmpty();
    break;

  case PSL_WSELECT:
    {
      PslNode_ptr right = psl_node_get_right(formula);

      Set_t object   = _THROW(symb_table, psl_node_get_left(formula),
                              context, filter,
                              preserve_time, time, dependencies_hash);
      Set_t high_bit = _THROW(symb_table, psl_node_get_left(right),
                              context, filter,
                              preserve_time, time, dependencies_hash);
      Set_t low_bit  = _THROW(symb_table, psl_node_get_right(right),
                              context, filter,
                              preserve_time, time, dependencies_hash);

      result = Set_Union(Set_Union(object, high_bit), low_bit);
      Set_ReleaseSet(low_bit);
      Set_ReleaseSet(high_bit);
      break;
    }

  case PSL_SERE:
  case PSL_SERECOMPOUND:
    result = _THROW(symb_table, psl_node_get_left(formula), context, filter,
                    preserve_time, time, dependencies_hash);
    break;

  case PSL_CONCATENATION:
    {
      /* TODO [MR]: to be fixed it might be wrong */
      Set_t right = _THROW(symb_table, psl_node_get_right(formula), context,
                           filter, preserve_time, time, dependencies_hash);

      result = _THROW(symb_table, psl_node_get_left(formula), context,
                      filter, preserve_time, time, dependencies_hash);
      result = Set_Union(result, right);
      Set_ReleaseSet(right);
      break;
    }

  case PSL_SERECONCAT:
  case PSL_SEREFUSION:
    {
      Set_t right = _THROW(symb_table, psl_node_get_right(formula),
                           context, filter,
                           preserve_time, time, dependencies_hash);
      result = _THROW(symb_table, psl_node_get_left(formula), context, filter,
                      preserve_time, time, dependencies_hash);
      result = Set_Union(result, right);
      Set_ReleaseSet(right);
      break;
    }

  case PSL_SEREREPEATED:
    result = _THROW(symb_table, psl_node_sere_repeated_get_expr(formula),
                    context, filter,
                    preserve_time, time, dependencies_hash);
    break;

  case PSL_REPLPROP:
    /* TODO [MR]: to be checked */
    result = _THROW(symb_table, psl_node_repl_prop_get_property(formula),
                    context, filter, preserve_time, time,
                    dependencies_hash);
    break;

  case PSL_PIPEMINUSGT:
  case PSL_PIPEEQGT:
  case PSL_DIAMONDMINUSGT:
    {
      Set_t con = _THROW(symb_table,
                         psl_node_suffix_implication_get_consequence(formula),
                         context, filter, preserve_time, time,
                         dependencies_hash);

      result = _THROW(symb_table,
                      psl_node_suffix_implication_get_premise(formula),
                      context, filter, preserve_time, time,
                      dependencies_hash);
      result = Set_Union(result, con);
      Set_ReleaseSet(con);
      break;
    }

  case PSL_ALWAYS:
  case PSL_NEVER:
  case PSL_EVENTUALLYBANG:
    result = _THROW(symb_table, psl_node_get_left(formula), context, filter,
                    preserve_time, time, dependencies_hash);
    break;

  case PSL_WITHINBANG:
  case PSL_WITHIN:
  case PSL_WITHINBANG_:
  case PSL_WITHIN_:
    {
      Set_t n2 = _THROW(symb_table,
                        psl_node_get_right(psl_node_get_left(formula)),
                        context,
                        filter, preserve_time, time, dependencies_hash);
      Set_t n3 = _THROW(symb_table, psl_node_get_right(formula),
                        context, filter,
                        preserve_time, time, dependencies_hash);

      result = _THROW(symb_table,
                      psl_node_get_left(psl_node_get_left(formula)),
                      context,
                      filter, preserve_time, time, dependencies_hash);

      result = Set_Union(Set_Union(result, n2), n3);
      Set_ReleaseSet(n3);
      Set_ReleaseSet(n2);
      break;
    }

  case PSL_NEXT_EVENT_ABANG:
  case PSL_NEXT_EVENT_A:
  case PSL_NEXT_EVENT_EBANG:
  case PSL_NEXT_EVENT_E:
  case PSL_NEXT_EVENTBANG:
  case PSL_NEXT_EVENT:
  case PSL_NEXT_ABANG:
  case PSL_NEXT_EBANG:
  case PSL_NEXT_A:
  case PSL_NEXT_E:
  case PSL_NEXTBANG:
  case PSL_NEXT:
  case PSL_X:
  case PSL_XBANG:
    {
      Set_t n2 = _THROW(symb_table,
                        psl_node_extended_next_get_when(formula),
                        context, filter,
                        preserve_time, time, dependencies_hash);
      Set_t n3 = _THROW(symb_table,
                        psl_node_extended_next_get_condition(formula),
                        context, filter,
                        preserve_time, time, dependencies_hash);

      result = _THROW(symb_table, psl_node_extended_next_get_expr(formula),
                      context, filter,
                      preserve_time, time, dependencies_hash);

      result = Set_Union(Set_Union(result, n2), n3);
      Set_ReleaseSet(n3);
      Set_ReleaseSet(n2);
      break;
    }

  case PSL_BEFOREBANG:
  case PSL_BEFORE:
  case PSL_BEFOREBANG_:
  case PSL_BEFORE_:
  case PSL_UNTILBANG:
  case PSL_UNTIL:
  case PSL_UNTILBANG_:
  case PSL_UNTIL_:
  case PSL_ABORT:
  case PSL_W:
  case PSL_OR:
  case PSL_CARET:
  case PSL_TILDE:
  case PSL_EQEQ:
  case PSL_PIPEPIPE:
  case PSL_AMPERSANDAMPERSAND:
  case PSL_WHILENOTBANG:
  case PSL_WHILENOT:
  case PSL_WHILENOTBANG_:
  case PSL_WHILENOT_:
    {
      Set_t right = _THROW(symb_table, psl_node_get_right(formula),
                           context, filter,
                           preserve_time, time, dependencies_hash);

      result = _THROW(symb_table, psl_node_get_left(formula), context, filter,
                      preserve_time, time, dependencies_hash);
      result = Set_Union(result, right);
      Set_ReleaseSet(right);
      break;
    }

  case PSL_ITE:
    {
      Set_t then_arg  = _THROW(symb_table, psl_node_get_ite_then(formula),
                               context, filter,
                               preserve_time, time, dependencies_hash);
      Set_t else_arg  = _THROW(symb_table, psl_node_get_ite_else(formula),
                               context, filter,
                               preserve_time, time, dependencies_hash);

      result = _THROW(symb_table, psl_node_get_ite_cond(formula),
                      context, filter,
                      preserve_time, time, dependencies_hash);
      result = Set_Union(Set_Union(result, then_arg), else_arg);
      Set_ReleaseSet(else_arg);
      Set_ReleaseSet(then_arg);
      break;
    }
    /* "MR: end PSL:" */
  default:
    StreamMgr_print_error(streams,
                          "\ndependency_psl_get_dependencies: " \
                          "Reached undefined connective (%d)\n",
                          node_get_type(formula_psl));
    ErrorMgr_nusmv_exit(errmgr, 1);
  }

  /* inserts the result in the hash table */
  _INSERT_IN_HASH(dependencies_hash, key, result, is_to_be_inserted);

  return result;
}
