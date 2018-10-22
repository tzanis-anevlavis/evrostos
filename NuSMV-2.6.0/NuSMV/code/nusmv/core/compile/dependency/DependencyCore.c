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
  \brief Implementation of class 'DependencyCore'

  \todo: Missing description

*/


#include "nusmv/core/compile/dependency/DependencyCore.h"
#include "nusmv/core/compile/dependency/DependencyCore_private.h"

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
/* See 'DependencyCore_private.h' for class 'DependencyCore' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void dependency_core_finalize(Object_ptr object, void* dummy);

static Set_t
dependency_core_get_dependencies(DependencyBase_ptr self,
                                 SymbTable_ptr symb_table,
                                 node_ptr formula, node_ptr context,
                                 SymbFilterType filter,
                                 boolean preserve_time, int time,
                                 hash_ptr dependencies_hash);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

DependencyCore_ptr DependencyCore_create(const NuSMVEnv_ptr env,
                                         const char* name)
{
  DependencyCore_ptr self = ALLOC(DependencyCore, 1);
  DEPENDENCY_CORE_CHECK_INSTANCE(self);

  dependency_core_init(self, env, name,
                       NUSMV_CORE_SYMBOL_FIRST,
                       NUSMV_CORE_SYMBOL_LAST - NUSMV_CORE_SYMBOL_FIRST);
  return self;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void dependency_core_init(DependencyCore_ptr self,
                          const NuSMVEnv_ptr env,
                          const char* name,
                          int low, size_t num)
{
  /* base class initialization */
  dependency_base_init(DEPENDENCY_BASE(self), env, name, low, num, true);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = dependency_core_finalize;
  OVERRIDE(DependencyBase, get_dependencies) =
    dependency_core_get_dependencies;
}

void dependency_core_deinit(DependencyCore_ptr self)
{
  /* members deinitialization */


  /* base class deinitialization */
  dependency_base_deinit(DEPENDENCY_BASE(self));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The DependencyCore class virtual finalizer

  Called by the class destructor
*/
static void dependency_core_finalize(Object_ptr object, void* dummy)
{
  DependencyCore_ptr self = DEPENDENCY_CORE(object);

  dependency_core_deinit(self);
  FREE(self);
}

/*!
  \brief Get the dependencies of formula


*/
static Set_t
dependency_core_get_dependencies(DependencyBase_ptr self,
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

  is_to_be_inserted = true;

  /* 0 for filter means no variables are looked for. Do not create
     a special constant in SymbFilterType, otherwise it will be
     visible outside (which may be not good). */
  if (formula == Nil || filter == 0)
    return Set_MakeEmpty();

  /* [SM] Memoization is checked before calling the get_dependencies
     function.  This avoids to check memoization in each implemented
     core.
   */
  formula_dependency_mk_hash_key(formula, context, filter,
                                 preserve_time, time, &key);

  switch (node_get_type(formula)) {
  case CONTEXT:
    result = _THROW(symb_table, cdr(formula), car(formula), filter,
                    preserve_time, time, dependencies_hash);
    break;

  case BIT:
    /* ignore bits, consider only scalar vars */
    result = _THROW(symb_table, car(formula), context, filter,
                    preserve_time, time, dependencies_hash);
    break;

  case ATOM:
    {
      ResolveSymbol_ptr rs;
      node_ptr name;

      rs = SymbTable_resolve_symbol(symb_table, formula, context);

      name = ResolveSymbol_get_resolved_name(rs);

      if (ResolveSymbol_is_error(rs)) {
        ResolveSymbol_throw_error(rs, env);
      }

      if (ResolveSymbol_is_parameter(rs)) {
        /* a formal parameter */
        node_ptr param;
        param = SymbTable_get_flatten_actual_parameter(symb_table, name);
        result = _THROW(symb_table, param, context, filter,
                        preserve_time, time, dependencies_hash);
      }
      else if (ResolveSymbol_is_constant(rs)) {
        result = formula_dependency_get_definition_dependencies(
                        FORMULA_DEPENDENCY(NODE_WALKER(self)->master),
                        symb_table,
                        name, filter,
                        preserve_time, time,
                        dependencies_hash);
      }
      else { /* it should be a defined symbol, running, or a variable */

        result =
          formula_dependency_get_definition_dependencies(
                        FORMULA_DEPENDENCY(NODE_WALKER(self)->master),
                        symb_table,
                        name, filter,
                        preserve_time, time,
                        dependencies_hash);

        /* It is possible
           formula_dependency_get_definition_dependencies inserted a
           value on the same key*/
        if (name == formula && SymbTable_is_symbol_define(symb_table, name)) {
          nusmv_assert((Set_t)NULL !=
                       formula_dependency_lookup_hash(dependencies_hash,
                                                      NODE_PTR(&key)));
          is_to_be_inserted = false;
        }
      }
      break;
    }

    /* a variable of word type */
  case SIGNED_WORD:
  case UNSIGNED_WORD:
    /* ignore width (a constant), consider only bits */
    result = _THROW(symb_table, car(formula), context, filter,
                    preserve_time, time, dependencies_hash);
    break;

    /* no dependencies */
  case FAILURE:
  case TRUEEXP:
  case FALSEEXP:
  case NUMBER:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case UWCONST:
  case SWCONST:
  case TWODOTS: /* Sets */
    result = Set_MakeEmpty();
    break;

  case WSIZEOF:
  case CAST_TOINT:
    result = _THROW(symb_table, car(formula), context, filter,
                    preserve_time, time, dependencies_hash);
    break;

    /* unary operation */
  case NEXT:   /* next(alpha), with alpha possibly complex, thus ... */
    /* nested next are checked by type checker */
    if(filter & VFT_NEXT) {
      /* the next variables become the current variables from this frame on */
      filter = (filter & (~VFT_NEXT)) | VFT_CURRENT;
    }
    else if (filter & VFT_CURRENT) {
      filter = filter & (~VFT_CURRENT);
    }

    /* input and frozen variables are searched for independently of
       next-operator */
    result = _THROW(symb_table, car(formula), context, filter, preserve_time,
                    EXPR_UNTIMED_NEXT, dependencies_hash);
    break;

  case ATTIME: /* the variable without time information are returned */
    {
      int time2 = ExprMgr_attime_get_time(exprs, formula);
      result = _THROW(symb_table, car(formula), context, filter,
                      preserve_time, time2,
                      dependencies_hash);
      break;
    }
  case NOT:    /* Unary boolean connectives */
  case UMINUS:
  case EX:    /* CTL unary Temporal Operators */
  case SMALLINIT:  /* used for init(expr) */
  case AX:
  case EF:
  case AF:
  case EG:
  case AG:
  case EBF:    /* CTL unary bounded Temporal Operators */
  case ABF:
  case EBG:
  case ABG:
  case EBU:    /* CTL binary bounded Temporal Operators */
  case ABU:
  case OP_NEXT:    /* LTL unary Temporal Operators */
  case OP_PREC:
  case OP_NOTPRECNOT:
  case OP_FUTURE:
  case OP_ONCE:
  case OP_GLOBAL:
  case OP_HISTORICAL:
  case CAST_BOOL: /* Casts */
  case CAST_WORD1:
  case CAST_SIGNED:
  case CAST_UNSIGNED:
  case FLOOR:
    result = _THROW(symb_table, car(formula), context, filter,
                    preserve_time, time, dependencies_hash);
    break;

  case WRESIZE: /* For this it is important only the expression on the lhs */
    result = _THROW(symb_table, car(formula), context, filter,
                    preserve_time, time, dependencies_hash);
    break;

    /* binary operation */
  case EXTEND:
  case EQDEF: /* assignment */
  case CONS:
  case UNION:
  case SETIN:
  case COLON:
  case PLUS:    /* Numerical Operations */
  case MINUS:
  case TIMES:
  case DIVIDE:
  case MOD:
  case LSHIFT:  /* Binary shifts and rotates */
  case RSHIFT:
  case LROTATE:
  case RROTATE:
  case CONCATENATION: /* concatenation */
  case EQUAL:   /* Comparison Operations */
  case NOTEQUAL:
  case LT:
  case GT:
  case LE:
  case GE:
  case AND:    /* Binary boolean connectives */
  case OR:
  case XOR:
  case XNOR:
  case IMPLIES:
  case IFF:
  case EU:     /* CTL binary  Temporal Operators */
  case AU:
  case UNTIL:    /* LTL binary Temporal Operators */
  case RELEASES:
  case SINCE:
  case TRIGGERED:
  case MAXU:    /* MIN MAX operators */
  case MINU:
    {
      Set_t right = _THROW(symb_table, cdr(formula), context, filter,
                           preserve_time, time, dependencies_hash);

      result = _THROW(symb_table, car(formula), context, filter,
                      preserve_time, time, dependencies_hash);
      result = Set_Union(result, right);
      Set_ReleaseSet(right);
      break;
    }

    /* 3-arity operations */
  case CASE:
  case IFTHENELSE:
    {
      Set_t then_arg  = _THROW(symb_table, cdr(car(formula)), context, filter,
                               preserve_time, time, dependencies_hash);

      Set_t else_arg  = _THROW(symb_table, cdr(formula), context, filter,
                               preserve_time, time, dependencies_hash);

      result = _THROW(symb_table, car(car(formula)),context, filter,
                      preserve_time, time, dependencies_hash);

      result = Set_Union(Set_Union(result, then_arg), else_arg);
      Set_ReleaseSet(else_arg);
      Set_ReleaseSet(then_arg);
      break;
    }

  case NFUNCTION:
    {
      Set_t fname;

      fname =  _THROW(symb_table, car(formula), context, filter,
                      preserve_time, time, dependencies_hash);
      result = _THROW(symb_table, cdr(formula), context, filter,
                      preserve_time, time, dependencies_hash);
      result = Set_Union(result, fname);
      
      Set_ReleaseSet(fname);

      break;
    }

  case BIT_SELECTION:
    {
      Set_t high_bit = _THROW(symb_table, car(cdr(formula)), context, filter,
                              preserve_time, time, dependencies_hash);
      Set_t low_bit = _THROW(symb_table, cdr(cdr(formula)), context, filter,
                             preserve_time, time, dependencies_hash);

      result = _THROW(symb_table, car(formula), context, filter,
                      preserve_time, time, dependencies_hash);

      result = Set_Union(Set_Union(result, high_bit), low_bit);
      Set_ReleaseSet(low_bit);
      Set_ReleaseSet(high_bit);
      break;
    }

  case COUNT:
    {
      node_ptr list = car(formula);

      result = Set_MakeEmpty();

      while (Nil != list) {
        Set_t tmp = _THROW(symb_table, car(list), context, filter,
                           preserve_time, time, dependencies_hash);
        result = Set_Union(result, tmp);
        Set_ReleaseSet(tmp);

        list = cdr(list);
      }

      break;
    }
    /* Operations on WORDARRAYs */
  case WAWRITE:
    {
      Set_t location = _THROW(symb_table, car(cdr(formula)), context, filter,
                              preserve_time, time, dependencies_hash);
      Set_t address = _THROW(symb_table, cdr(cdr(formula)), context, filter,
                             preserve_time, time, dependencies_hash);

      result = _THROW(symb_table, car(formula), context, filter,
                      preserve_time, time, dependencies_hash);
      result = Set_Union(Set_Union(result, location), address);
      Set_ReleaseSet(address);
      Set_ReleaseSet(location);
      break;
    }

  case WAREAD:
  case CAST_TO_UNSIGNED_WORD:
    {
      Set_t location = _THROW(symb_table, car(formula), context, filter,
                              preserve_time, time, dependencies_hash);

      result = _THROW(symb_table, cdr(formula), context, filter,
                      preserve_time, time, dependencies_hash);
      result = Set_Union(result, location);
      Set_ReleaseSet(location);
      break;
    }

  case CONST_ARRAY:
    {
      result = _THROW(symb_table, cdr(formula), context, filter,
                      preserve_time, time, dependencies_hash);
      break;
    }

    /* [MD] I think this can be safely merged with the ATOM case to avoid code
       duplication */
    /* name cases */
  case DOT:
    {
      ResolveSymbol_ptr rs;
      node_ptr name;

      rs = SymbTable_resolve_symbol(symb_table, formula, context);
      name = ResolveSymbol_get_resolved_name(rs);

      if (ResolveSymbol_is_constant(rs)) {
        result =
          formula_dependency_get_definition_dependencies(
                            FORMULA_DEPENDENCY(NODE_WALKER(self)->master),
                            symb_table,
                            name, filter,
                            preserve_time, time,
                            dependencies_hash);
      }
      else {
        result =
          formula_dependency_get_definition_dependencies(
                           FORMULA_DEPENDENCY(NODE_WALKER(self)->master),
                           symb_table,
                           name, filter,
                           preserve_time, time,
                           dependencies_hash);

        /* It is possible
           formula_dependency_get_definition_dependencies inserted a
           value on the same key */
        if (name == formula && SymbTable_is_symbol_define(symb_table, name)) {
          nusmv_assert((Set_t)NULL !=
                       formula_dependency_lookup_hash(dependencies_hash,
                                                      NODE_PTR(&key)));
          is_to_be_inserted = false;
        }
      }

      break;
    }

  case ARRAY:
    {
      ResolveSymbol_ptr rs;
      node_ptr name;

      rs = SymbTable_resolve_symbol(symb_table, formula, context);
      name = ResolveSymbol_get_resolved_name(rs);

      if (ResolveSymbol_is_defined(rs)) {
        /* this array is identifier-with-brackets */
        result =
          formula_dependency_get_definition_dependencies(
                        FORMULA_DEPENDENCY(NODE_WALKER(self)->master),
                        symb_table,
                        name, filter,
                        preserve_time, time,
                        dependencies_hash);

        /* It is possible
           formula_dependency_get_definition_dependencies inserted a
           value on the same key */
        if (name == formula && SymbTable_is_symbol_define(symb_table, name)) {
          nusmv_assert((Set_t)NULL !=
                       formula_dependency_lookup_hash(dependencies_hash,
                                                      NODE_PTR(&key)));
          is_to_be_inserted = false;
        }

      }
      else {
        /* this array is an expression => process the children */
        Set_t tmp;

        result = _THROW(symb_table, car(formula), context, filter,
                        preserve_time, time, dependencies_hash);

        tmp = _THROW(symb_table, cdr(formula), context, filter,
                     preserve_time, time, dependencies_hash);

        result = Set_Union(result, tmp);
        Set_ReleaseSet(tmp);
      }
      break;
    }

  case ARRAY_DEF:
    {
      node_ptr iter;

      result = Set_MakeEmpty();

      for (iter = car(formula); iter != Nil; iter = cdr(iter)) {
        Set_t tmp;
        nusmv_assert(CONS == node_get_type(iter));
        tmp = _THROW(symb_table, car(iter), context, filter,
                     preserve_time, time, dependencies_hash);
        result = Set_Union(result, tmp);
        Set_ReleaseSet(tmp);
      }
      break;
    }

  default:
    StreamMgr_print_error(streams,
                          "\ndependency_core_get_dependencies: " \
                          "Reached undefined connective (%d)\n",
                          node_get_type(formula));
    ErrorMgr_nusmv_exit(errmgr, 1);
  }

  /* inserts the result in the hash table */
  _INSERT_IN_HASH(dependencies_hash, key, result, is_to_be_inserted);

  return result;
}
