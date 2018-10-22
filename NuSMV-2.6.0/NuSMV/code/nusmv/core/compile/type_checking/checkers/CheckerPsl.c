/* ---------------------------------------------------------------------------


  This file is part of the ``compile.type_checking.checkers'' package of NuSMV version 2.
  Copyright (C) 2004 by FBK-irst.

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
  \brief Implementaion of class 'CheckerPsl'

  \todo: Missing description

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/compile/type_checking/checkers/CheckerPsl.h"
#include "nusmv/core/compile/type_checking/checkers/CheckerPsl_private.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/compile/symb_table/symb_table.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/psl/psl_symbols.h"
#include "nusmv/core/parser/psl/pslNode.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/error.h"
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'CheckerPsl_private.h' for class 'CheckerPsl' definition. */

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

static void checker_psl_finalize(Object_ptr object, void* dummy);

static SymbType_ptr checker_psl_check_expr(CheckerBase_ptr self,
                                           node_ptr e, node_ptr ctx);

static boolean
checker_psl_viol_handler(CheckerBase_ptr self,
                         TypeSystemViolation violation,
                         node_ptr expression);

static void print_operator(FILE* output_stream, PslNode_ptr expr);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

CheckerPsl_ptr CheckerPsl_create(const NuSMVEnv_ptr env)
{
  CheckerPsl_ptr self = ALLOC(CheckerPsl, 1);
  CHECKER_PSL_CHECK_INSTANCE(self);

  checker_psl_init(self, env);
  return self;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void checker_psl_init(CheckerPsl_ptr self, const NuSMVEnv_ptr env)
{
  /* base class initialization */
  checker_base_init(CHECKER_BASE(self), env, "PSL Type Checker",
                    NUSMV_PSL_SYMBOL_FIRST,
                    NUSMV_PSL_SYMBOL_LAST - NUSMV_PSL_SYMBOL_FIRST);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = checker_psl_finalize;
  OVERRIDE(CheckerBase, check_expr) = checker_psl_check_expr;
  OVERRIDE(CheckerBase, viol_handler) = checker_psl_viol_handler;
}

void checker_psl_deinit(CheckerPsl_ptr self)
{
  /* members deinitialization */

  /* base class initialization */
  checker_base_deinit(CHECKER_BASE(self));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The CheckerPsl class virtual finalizer

  Called by the class destructor
*/
static void checker_psl_finalize(Object_ptr object, void* dummy)
{
  CheckerPsl_ptr self = CHECKER_PSL(object);

  checker_psl_deinit(self);
  FREE(self);
}

/*!
  \brief 

  
*/
static SymbType_ptr checker_psl_check_expr(CheckerBase_ptr self,
                                           node_ptr e, node_ptr ctx)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  /* converts and operates on PslNodes, not node_ptr: */
  PslNode_ptr expr = PslNode_convert_from_node_ptr(e);
  PslNode_ptr context = PslNode_convert_from_node_ptr(ctx);

  PslNode_ptr ctx_expr;

  /* wrap expr into the context. This is required by
     the facilities which remembers the type of expressions
     and by the violation handler. */
  if (context != PSL_NULL) {
    ctx_expr = PslNode_new_context(nodemgr,
                                   psl_node_context_to_main_context(nodemgr, context),
                                   expr);
  }
  else ctx_expr = expr;

  { /* checks memoizing */
    SymbType_ptr tmp = _GET_TYPE(ctx_expr);
    if (nullType != tmp) return tmp;
  }

  switch (psl_node_get_op(expr)) {
  case PSL_INF: return _SET_TYPE(ctx_expr, SymbTablePkg_integer_set_type(env));

  case PSL_SERE:
  case PSL_SERECOMPOUND:
    {
      SymbType_ptr type = _THROW(psl_node_get_left(expr), context);

      if (SymbType_is_error(type)) {
        /* earlier error */
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      if (SymbType_is_boolean(type)) return _SET_TYPE(ctx_expr, type);

      /* there is violation */
      if (_VIOLATION(SymbType_is_back_comp(type) ?
                     TC_VIOLATION_TYPE_BACK_COMP : TC_VIOLATION_TYPE_MANDATORY,
                     ctx_expr)) {
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }


      /* keeps the current type */
      return _SET_TYPE(ctx_expr, type);
    }

    /* concatenation and multiple concatenation */
  case PSL_CONCATENATION:
    {
      PslNode_ptr iter;
      SymbType_ptr left = SYMB_TYPE(NULL);
      SymbType_ptr right = SYMB_TYPE(NULL);
      boolean is_ok = true;

      if (psl_node_get_right(expr) != PSL_NULL) {
        /* multiple concatenation */
        right = _THROW(psl_node_get_right(expr), context);
        if (SymbType_is_error(right)) {
          return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
        }

        if (!SymbType_is_boolean(right)) {
          if (_VIOLATION(SymbType_is_back_comp(right) ?
                         TC_VIOLATION_TYPE_BACK_COMP :
                         TC_VIOLATION_TYPE_MANDATORY,
                         ctx_expr)) {
            return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
          }
        }
      }

      /* checks all the list's elements */
      iter = psl_node_get_left(expr);
      while (is_ok && iter != PSL_NULL) {
        left = _THROW(psl_node_get_left(iter), context);
        if (SymbType_is_error(left)) {
          return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
        }

        is_ok = SymbType_is_boolean(left);
        iter = psl_node_get_right(iter);
      }

      if (!is_ok && _VIOLATION(SymbType_is_back_comp(left) ?
                               TC_VIOLATION_TYPE_BACK_COMP :
                               TC_VIOLATION_TYPE_MANDATORY,
                               ctx_expr)) {
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      /* this is not an error after all -> return boolean type */
      return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
    }

  case PSL_SERECONCAT:
  case PSL_SEREFUSION:
    {
      SymbType_ptr type1 = _THROW(psl_node_get_left(expr), context);
      SymbType_ptr type2 = _THROW(psl_node_get_right(expr), context);


      if (SymbType_is_error(type1) || SymbType_is_error(type2)) {
        /* earlier error */
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      if (SymbType_is_boolean(type1) && SymbType_is_boolean(type2)) {
        return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
      }

      /* there is violation */
      if (_VIOLATION(SymbType_is_back_comp(type1) &&
                     SymbType_is_back_comp(type2)?
                     TC_VIOLATION_TYPE_BACK_COMP : TC_VIOLATION_TYPE_MANDATORY,
                     ctx_expr)) {
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      /* this is not an error after all -> return boolean type */
      return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
    }


  case PSL_SEREREPEATED:
    {
      PslNode_ptr sere, count;
      boolean is_ok = true;
      SymbType_ptr tcount = SYMB_TYPE(NULL);
      SymbType_ptr tsere;

      nusmv_assert(psl_node_get_left(expr) != PSL_NULL);
      sere = psl_node_sere_repeated_get_expr(expr);
      count = psl_node_sere_repeated_get_count(expr);

      /* checks the count at first: */
      if (count != PSL_NULL) {
        tcount = _THROW(count, context);

        if (SymbType_is_error(tcount)) return _SET_TYPE(ctx_expr, tcount);
        if (tcount != SymbTablePkg_integer_type(env) &&
            tcount != SymbTablePkg_boolean_type(env)) {
          is_ok = false;
        }
      }

      /* checks the sere now: */
      if (sere != PSL_NULL && is_ok) {
        tsere = _THROW(sere, context);
        if (SymbType_is_error(tsere)) {
          return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
        }

        if (SymbType_is_boolean(tsere)) {
          /* ok */
          return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
        }
      }
      else tsere = SymbTablePkg_error_type(env);

      /* is this a type error ? */
      if (!is_ok && _VIOLATION(SymbType_is_back_comp(tcount) &&
                               SymbType_is_back_comp(tsere) ?
                               TC_VIOLATION_TYPE_BACK_COMP :
                               TC_VIOLATION_TYPE_MANDATORY,
                               ctx_expr)) {
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      /* this is not an error after all -> return boolean type */
      return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
    }


  case PSL_REPLPROP:
    {
      SymbTable_ptr symb_table =
        TypeChecker_get_symb_table(TYPE_CHECKER(NODE_WALKER(self)->master));

      PslNode_ptr repl = psl_node_repl_prop_get_replicator(expr);
      PslNode_ptr prop = psl_node_repl_prop_get_property(expr);
      PslNode_ptr value_set = psl_node_get_replicator_value_set(repl);
      PslNode_ptr evalue_set =
        psl_node_get_replicator_normalized_value_set(env, repl);
      PslNode_ptr id = psl_node_get_replicator_id(repl);
      boolean first_loop = true;
      ResolveSymbol_ptr rs;
      node_ptr id_ctx;

      rs = SymbTable_resolve_symbol(symb_table,
                            PslNode_convert_to_node_ptr(id), ctx);
      id_ctx = ResolveSymbol_get_resolved_name(rs);

      /* Prepares the forall value set */
      if (!psl_node_is_boolean_type(value_set)) {
        /* not boolean checks the value_set content */
        SymbType_ptr tvs = SymbType_create(env, SYMB_TYPE_ENUM, evalue_set);
        if (!TypeChecker_is_type_wellformed(
                                    TYPE_CHECKER(NODE_WALKER(self)->master),
                                    tvs, id_ctx)) {
          return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
        }
      }

      /* loops over the set of possible values of the ID, and defines
         a new define for each value. Then checks the replicated
         property */
      while (evalue_set != PSL_NULL) {
        SymbType_ptr prop_type;
        boolean is_ok = true;

        /* creates a new temporary layer to contain the id as input var */
        SymbLayer_ptr layer =
          SymbTable_create_layer(symb_table, NULL, SYMB_LAYER_POS_DEFAULT);

        /* checks the forall identifier: */
        if (!SymbLayer_can_declare_define(layer, id_ctx)) {
          _VIOLATION(TC_VIOLATION_AMBIGUOUS_IDENTIFIER, id);
          return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
        }

        /* declares a new temporary define to represent the id */
        SymbLayer_declare_define(layer, id_ctx, ctx,
                         PslNode_convert_to_node_ptr(
                             psl_node_cons_get_element(evalue_set)));


        /* now checks the replicated property: */
        prop_type = _THROW(prop, context);
        if (SymbType_is_error(prop_type)) {
          return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
        }

        is_ok = SymbType_is_boolean(prop_type);

        /* handle violation */
        if (!is_ok && _VIOLATION(SymbType_is_back_comp(prop_type) ?
                                 TC_VIOLATION_TYPE_BACK_COMP :
                                 TC_VIOLATION_TYPE_MANDATORY,
                                 ctx_expr)) {
          return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
        }

        /* gets rid of the temporary layer: */
        SymbTable_remove_layer(symb_table, layer);

        /* Disables memoizing, to force checking of the property event
           if already checked, as the property that is going to be
           checked at next iteration is grammatically identically to
           the one that is being checked, but contain different value
           for the forall ID */
        if (first_loop) {
          type_checker_enable_memoizing(
                TYPE_CHECKER(NODE_WALKER(self)->master), false);
          first_loop = false;
        }

        evalue_set = psl_node_cons_get_next(evalue_set); /* iterates on */
      } /* loop over forall range */

      /* re-enables memoizing. The property has already been memoized
         during the first loop iteration */
      type_checker_enable_memoizing(TYPE_CHECKER(NODE_WALKER(self)->master),
                                    true);

      /* this is not an error after all -> return boolean type */
      return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
    }

    case PSL_WSELECT:
      {
        SymbTable_ptr symb_table =
          TypeChecker_get_symb_table(TYPE_CHECKER(NODE_WALKER(self)->master));

        PslNode_ptr left = psl_node_get_left(expr);
        PslNode_ptr right = psl_node_get_right(expr);
        /* get the operand' type */
        SymbType_ptr type = _THROW(left, context);

        node_ptr hbit, lbit;
        int width;
        int highBound;
        int lowBound;

        if (SymbType_is_error(type)) { /* earlier error */
          return _SET_TYPE(ctx_expr, type);
        }

        hbit = PslNode_convert_to_node_ptr(psl_node_get_left(right));
        lbit = PslNode_convert_to_node_ptr(psl_node_get_right(right));

        hbit = CompileFlatten_resolve_number(symb_table, hbit, context);
        lbit = CompileFlatten_resolve_number(symb_table, lbit, context);

        nusmv_assert(COLON == psl_node_get_op(right));

        /* Non constant expressions for range */
        if ((Nil == hbit || Nil == lbit) ||
            (NUMBER != node_get_type(hbit) || NUMBER != node_get_type(lbit))) {
          if (_VIOLATION(TC_VIOLATION_UNCONSTANT_EXPRESSION, ctx_expr)) {
            return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
          }
          else { /* return arbitrary Word type */
            return _SET_TYPE(ctx_expr, SymbTablePkg_unsigned_word_type(env, 1));
          }
        }

        /* check the first operand type */
        if (!SymbType_is_word(type)) { /* ERROR */
          if (_VIOLATION(TC_VIOLATION_TYPE_MANDATORY, ctx_expr)) {
            return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
          }
          else { /* return arbitrary Word type */
            return _SET_TYPE(ctx_expr, SymbTablePkg_unsigned_word_type(env, 1));
          }
        }
        width = SymbType_get_word_width(type);
        highBound = NODE_TO_INT(car(hbit));
        lowBound  = NODE_TO_INT(car(lbit));

        /* checks the bit width */
        if (width <= highBound || highBound < lowBound || lowBound < 0) {
          /* ERROR */
          if (_VIOLATION(TC_VIOLATION_OUT_OF_WORD_WIDTH, ctx_expr)) {
            return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
          }
          else { /* give some realistic bit specifiers */
            highBound = 0;
            lowBound = 0;
          }
        }
        /* everything is OK */
        return _SET_TYPE(ctx_expr,
                         SymbTablePkg_unsigned_word_type(env, highBound - lowBound + 1));
      }

  case PSL_PIPEMINUSGT:
  case PSL_PIPEEQGT:
    {
      PslNode_ptr pre = psl_node_suffix_implication_get_premise(expr);
      PslNode_ptr con = psl_node_suffix_implication_get_consequence(expr);
      SymbType_ptr type1 = _THROW(pre, context);
      SymbType_ptr type2 = _THROW(con, context);

      if (SymbType_is_error(type1) || SymbType_is_error(type2)) {
        /* earlier error */
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      if (SymbType_is_boolean(type1) && SymbType_is_boolean(type2)) {
        return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
      }

      /* there is violation */
      if (_VIOLATION(SymbType_is_back_comp(type1) &&
                     SymbType_is_back_comp(type2)?
                     TC_VIOLATION_TYPE_BACK_COMP : TC_VIOLATION_TYPE_MANDATORY,
                     ctx_expr)) {
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      /* this is not an error after all -> return boolean type */
      return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
    }


  case PSL_ALWAYS:
  case PSL_NEVER:
  case PSL_EVENTUALLYBANG:
    {
      SymbType_ptr type = _THROW(psl_node_get_left(expr), context);

      if (SymbType_is_error(type)) {
        /* earlier error */
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      if (SymbType_is_boolean(type)) {
        return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
      }

      /* there is violation */
      if (_VIOLATION(SymbType_is_back_comp(type) ?
                     TC_VIOLATION_TYPE_BACK_COMP : TC_VIOLATION_TYPE_MANDATORY,
                     ctx_expr)) {
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      /* this is not an error after all -> return boolean type */
      return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
    }


  case PSL_WITHINBANG:
  case PSL_WITHIN:
  case PSL_WITHINBANG_:
  case PSL_WITHIN_:
    {
      PslNode_ptr n1 = psl_node_get_left(psl_node_get_left(expr));
      PslNode_ptr n2 = psl_node_get_right(psl_node_get_left(expr));
      PslNode_ptr n3 = psl_node_get_right(expr);

      SymbType_ptr t1 = _THROW(n1, context);
      SymbType_ptr t2 = _THROW(n2, context);
      SymbType_ptr t3 = _THROW(n3, context);

      if (SymbType_is_error(t1) || SymbType_is_error(t2) ||
          SymbType_is_error(t3) ) {
        /* earlier error */
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      if (SymbType_is_boolean(t1) && SymbType_is_boolean(t2) &&
          SymbType_is_boolean(t3)) {
        return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
      }

      /* there is violation */
      if (_VIOLATION(SymbType_is_back_comp(t1) &&
                     SymbType_is_back_comp(t2) && SymbType_is_back_comp(t3) ?
                     TC_VIOLATION_TYPE_BACK_COMP : TC_VIOLATION_TYPE_MANDATORY,
                     ctx_expr)) {
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      /* this is not an error after all -> return boolean type */
      return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
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
      PslNode_ptr n1 = psl_node_extended_next_get_expr(expr);
      PslNode_ptr n2 = psl_node_extended_next_get_when(expr);
      PslNode_ptr n3 = psl_node_extended_next_get_condition(expr);

      SymbType_ptr t1 = _THROW(n1, context);

      SymbType_ptr t2 =
        (n2 != PSL_NULL) ? _THROW(n2, context) : SYMB_TYPE(NULL);

      SymbType_ptr t3 =
        (n3 != PSL_NULL) ? _THROW(n3, context) : SYMB_TYPE(NULL);

      if (SymbType_is_error(t1) ||
          (t2 != SYMB_TYPE(NULL) && SymbType_is_error(t2)) ||
          (t3 != SYMB_TYPE(NULL) && SymbType_is_error(t3))) {
        /* earlier error */
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      if (SymbType_is_boolean(t1) &&
          (t3 == SYMB_TYPE(NULL) || SymbType_is_boolean(t3))) {
        switch (psl_node_get_op(expr)) {
        case PSL_NEXTBANG:
        case PSL_NEXT:
        case PSL_X:
        case PSL_XBANG:
          /* number */
          if ((t2 == SYMB_TYPE(NULL)) ||
              ((SymbType_get_tag(t2) == SYMB_TYPE_INTEGER ||
                SymbType_is_pure_int_enum(t2) ||
                SymbType_is_boolean(t2)) &&
               psl_node_number_get_value(n2) >= 0)) {
            return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
          }
          break;

        case PSL_NEXT_ABANG:
        case PSL_NEXT_EBANG:
        case PSL_NEXT_A:
        case PSL_NEXT_E:
          /* finite range */
          if (t2 == SYMB_TYPE(NULL)) {
            return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
          }

          if (SymbType_get_tag(t2) == SYMB_TYPE_SET_INT) {
            PslNode_ptr low = psl_node_range_get_low(n2);
            PslNode_ptr high = psl_node_range_get_high(n2);
            if (!psl_node_is_infinite(high) &&
                psl_node_number_get_value(low) >= 0 &&
                psl_node_number_get_value(low) <= psl_node_number_get_value(high)) {
              return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
            }
          }
          break;


        case PSL_NEXT_EVENT_ABANG:
        case PSL_NEXT_EVENT_A:
        case PSL_NEXT_EVENT_EBANG:
        case PSL_NEXT_EVENT_E:
          /* finite positive range */
          if (t2 == SYMB_TYPE(NULL)) {
            return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
          }

          if (SymbType_get_tag(t2) == SYMB_TYPE_SET_INT) {
            PslNode_ptr low = psl_node_range_get_low(n2);
            PslNode_ptr high = psl_node_range_get_high(n2);
            if (!psl_node_is_infinite(high) &&
                psl_node_number_get_value(low) > 0 &&
                psl_node_number_get_value(low) <= psl_node_number_get_value(high)) {
              return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
            }
          }
          break;

        case PSL_NEXT_EVENTBANG:
        case PSL_NEXT_EVENT:
          /* positive number */
          if ((t2 == SYMB_TYPE(NULL)) ||
              ((SymbType_get_tag(t2) == SYMB_TYPE_INTEGER ||
                SymbType_is_pure_int_enum(t2) ||
                SymbType_is_boolean(t2)) &&
               psl_node_number_get_value(n2) > 0)) {
            return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
          }
          break;

        default:
          error_unreachable_code(); /* no other cases */
        }
      }

      /* there is violation */
      if (_VIOLATION(SymbType_is_back_comp(t1) &&
                     (t2 == SYMB_TYPE(NULL) || SymbType_is_back_comp(t2)) &&
                     (t3 == SYMB_TYPE(NULL) || SymbType_is_back_comp(t3)) ?
                     TC_VIOLATION_TYPE_BACK_COMP : TC_VIOLATION_TYPE_MANDATORY,
                     ctx_expr)) {
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      /* this is not an error after all -> return boolean type */
      return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
    }


  case PSL_RANGE:
    /* colon is used only for index ranges */
    return _SET_TYPE(ctx_expr, SymbTablePkg_integer_set_type(env));


    /* binary operators */
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
      SymbType_ptr type1 = _THROW(psl_node_get_left(expr), context);
      SymbType_ptr type2 = _THROW(psl_node_get_right(expr), context);


      if (SymbType_is_error(type1) || SymbType_is_error(type2)) {
        /* earlier error */
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      if (SymbType_is_boolean(type1) && SymbType_is_boolean(type2)) {
        return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
      }

      /* there is violation */
      if (_VIOLATION(SymbType_is_back_comp(type1) &&
                     SymbType_is_back_comp(type2)?
                     TC_VIOLATION_TYPE_BACK_COMP : TC_VIOLATION_TYPE_MANDATORY,
                     ctx_expr)) {
        return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
      }

      /* this is not an error after all -> return boolean type */
      return _SET_TYPE(ctx_expr, SymbTablePkg_boolean_type(env));
    }


  case PSL_ITE:
  {
    /* get the operands' type */
    SymbType_ptr returnType;
    SymbType_ptr condType = _THROW(psl_node_get_ite_cond(expr), context);
    SymbType_ptr thenType = _THROW(psl_node_get_ite_then(expr), context);
    SymbType_ptr elseType = _THROW(psl_node_get_ite_else(expr), context);

    if (SymbType_is_error(condType) ||
        SymbType_is_error(thenType) || SymbType_is_error(elseType)) {
      /* earlier error */
      return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
    }

   /* condition should be boolean */
    if ( (!SymbType_is_boolean(condType)) &&
         _VIOLATION(SymbType_is_back_comp(condType) ?
                    TC_VIOLATION_TYPE_BACK_COMP :
                    TC_VIOLATION_TYPE_MANDATORY,
                    expr) ) {
      return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
    }

    /* both ITE expressions should be convertable to a common type.
       If one of the expressions is of a set-type then before implicit
       convertion the other expression is converted to a corresponding
       set-type.
    */
    {
      SymbType_ptr tmp1 = SymbType_is_set(elseType) ?
        SymbType_make_set_type(thenType) : thenType;

      SymbType_ptr tmp2 = SymbType_is_set(thenType) ?
        SymbType_make_set_type(elseType) : elseType;

      if (nullType != tmp1 && nullType != tmp2) {
        returnType = SymbType_get_minimal_common(tmp1, tmp2);
      }
      else returnType = nullType;
    }

    /* we do not care which type exactly is obtained, since only
       correct type could pass the above code
    */
    if (nullType != returnType) {
      return _SET_TYPE(ctx_expr, returnType);
    }

    /* is this a type error ? */
    if (_VIOLATION(SymbType_is_back_comp(thenType) &&
                   SymbType_is_back_comp(elseType) ?
                   TC_VIOLATION_TYPE_BACK_COMP :
                   TC_VIOLATION_TYPE_MANDATORY,
                   expr)) {
      return _SET_TYPE(ctx_expr, SymbTablePkg_error_type(env));
    }

    /* this is not an error after all -> return one of the types  */
    return _SET_TYPE(ctx_expr, thenType);
  }

  default:
    ErrorMgr_internal_error(errmgr, "checker_psl_check_expr: not supported type");
  }

  return nullType;
}

/*!
  \brief The type core violation handler.

  The violation handler is implemented as
  a virtual method, which is invoked by the checker when an expression
  being checked violates the type system.
  See the violation handler TypeCheckingViolationHandler_ptr
  for more explanations.

  The below function is the default violation handler, and a
  user can potentially define its own violation handler, by deriving
  a new class from this class and by overriding this virtual method.

  This violation handler outputs an error and warning message to
  errstream. A warning is output if the detected violation is
  TC_VIOLATION_TYPE_BACK_COMP and the system variable
  "type_checking_backward_compatibility" is true. Also the
  TC_VIOLATION_TYPE_WARNING violation outputs a warning. Only in this
  case the false value is returned, indicating that this is NOT an
  error. Otherwise the true value is returned, indicating that this is
  an error.

  Also, if the system variable "type_check_warning_on" is false,
  warning messages are not output.

  NB: if the expression is checked in some context (context is not null) then
  before providing the expression to this function the expression should be
  wrapped into context, i.e. with find_node(nodemgr, CONEXT, context, expr)

  \sa TypeSystemViolation
*/
static boolean
checker_psl_viol_handler(CheckerBase_ptr self,
                         TypeSystemViolation violation, node_ptr expression)
{
  /* In the output message, the information about the expression
     location are output. So, make sure that the input file name and
     line number are correctly set!
  */
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  boolean isError = true; /* is this error or warning */

  /* get rid of the context the expression may be wrapped in */
  PslNode_ptr context = PSL_NULL;
  PslNode_ptr expr = PslNode_convert_from_node_ptr(expression);

  if (node_get_type(expression) == CONTEXT) {
    context = PslNode_convert_from_node_ptr(car(expression));
    expr = PslNode_convert_from_node_ptr(cdr(expression));
  }

  /* checks the given violation */
  nusmv_assert(TypeSystemViolation_is_valid(violation));

  /* only violation TC_VIOLATION_TYPE_BACK_COMP and the variable
     type_checking_backward_compatibility being true, may make a
     warning from an error.
     TC_VIOLATION_TYPE_WARNING always forces a warning
  */
  if (  TC_VIOLATION_TYPE_WARNING == violation
     || ( TC_VIOLATION_TYPE_BACK_COMP == violation
         && opt_backward_comp(opts))) {
    isError = false;
  }

  if (!isError && !opt_type_checking_warning_on(opts)) {
    /* this is a warning and warning messages are not allowed.
     So, do nothing, just return false (this is not an error)
    */
    return false;
  }

  _PRINT_ERROR_MSG(expr, isError);

  switch (violation) {
  case TC_VIOLATION_AMBIGUOUS_IDENTIFIER:
    StreamMgr_print_error(streams,   "identifier '");
    StreamMgr_nprint_error(streams, wffprint, "%N", PslNode_convert_to_node_ptr(expr));
    StreamMgr_print_error(streams,  "' is ambiguous\n");
    break;

  case TC_VIOLATION_UNCONSTANT_EXPRESSION:
    StreamMgr_print_error(streams,  "Expected constant expression in '");
    StreamMgr_nprint_error(streams, wffprint, "%N", PslNode_convert_to_node_ptr(expr));
    StreamMgr_print_error(streams,  "'\n");
    break;

  case TC_VIOLATION_TYPE_MANDATORY:
  case TC_VIOLATION_TYPE_BACK_COMP:
  case TC_VIOLATION_TYPE_WARNING:
    if (isError) StreamMgr_print_error(streams,  "illegal ");
    else         StreamMgr_print_error(streams,  "potentially incorrect ");

    switch (psl_node_get_op(expr)) {

    case PSL_SERE:
    case PSL_SERECOMPOUND:
      StreamMgr_print_error(streams,  "sere type of {");
      print_node(wffprint, stderr, PslNode_convert_to_node_ptr(psl_node_get_left(expr)));
      StreamMgr_print_error(streams,  "} : ");
      checker_base_print_type(self, errstream,
                      PslNode_convert_to_node_ptr(psl_node_get_left(expr)),
                      PslNode_convert_to_node_ptr(context));
      break;

    case PSL_SERECONCAT:
      StreamMgr_print_error(streams,  "operand types of sere concatenation: ");
      checker_base_print_type(self, errstream,
                      PslNode_convert_to_node_ptr(psl_node_get_left(expr)),
                      PslNode_convert_to_node_ptr(context));
      StreamMgr_print_error(streams,  " ");
      print_operator(errstream, expr);
      StreamMgr_print_error(streams,  " ");
      checker_base_print_type(self, errstream,
                      PslNode_convert_to_node_ptr(psl_node_get_right(expr)),
                      PslNode_convert_to_node_ptr(context));
      break;

    case PSL_SEREFUSION:
      StreamMgr_print_error(streams,  "operand types of sere fusion: ");
      checker_base_print_type(self, errstream,
                      PslNode_convert_to_node_ptr(psl_node_get_left(expr)),
                      PslNode_convert_to_node_ptr(context));
      StreamMgr_print_error(streams,  " ");
      print_operator(errstream, expr);
      StreamMgr_print_error(streams,  " ");
      checker_base_print_type(self, errstream,
                      PslNode_convert_to_node_ptr(psl_node_get_right(expr)),
                      PslNode_convert_to_node_ptr(context));
      break;


    case PSL_SEREREPEATED:
      {
        PslNode_ptr sere = psl_node_sere_repeated_get_expr(expr);
        PslNode_ptr count = psl_node_sere_repeated_get_count(expr);

        StreamMgr_print_error(streams,  "operand types of sere repeated: ");
        if (sere != PSL_NULL) {
          checker_base_print_type(self, errstream,
                                  PslNode_convert_to_node_ptr(sere),
                                  PslNode_convert_to_node_ptr(context));
        }
        print_operator(errstream, psl_node_get_left(expr));
        StreamMgr_print_error(streams,  " ");

        if (count != PSL_NULL) {
          checker_base_print_type(self, errstream,
                                  PslNode_convert_to_node_ptr(count),
                                  PslNode_convert_to_node_ptr(context));
        }
        StreamMgr_print_error(streams,  " ]");
        break;
      }

    case PSL_REPLPROP:
      {
        PslNode_ptr prop = psl_node_repl_prop_get_property(expr);

        StreamMgr_print_error(streams,  "operand type of ");
        print_operator(errstream, expr);
        StreamMgr_print_error(streams,  " property: ");
        checker_base_print_type(self, errstream,
                                PslNode_convert_to_node_ptr(prop),
                                PslNode_convert_to_node_ptr(context));
        break;
      }

      /* suffix implication */
    case PSL_PIPEMINUSGT:
    case PSL_PIPEEQGT:
      {
        PslNode_ptr pre = psl_node_suffix_implication_get_premise(expr);
        PslNode_ptr con = psl_node_suffix_implication_get_consequence(expr);

        StreamMgr_print_error(streams,  "operand types of suffix implication: ");
        checker_base_print_type(self, errstream,
                                PslNode_convert_to_node_ptr(pre),
                                PslNode_convert_to_node_ptr(context));

        StreamMgr_print_error(streams,  " ");
        print_operator(errstream, expr);
        StreamMgr_print_error(streams,  " ");

        checker_base_print_type(self, errstream,
                                PslNode_convert_to_node_ptr(con),
                                PslNode_convert_to_node_ptr(context));
        break;
      }

      /* unary operators: */
    case PSL_ALWAYS:
    case PSL_NEVER:
    case PSL_EVENTUALLYBANG:
        StreamMgr_print_error(streams,  "operand types of \"");
        print_operator(errstream, expr);
        StreamMgr_print_error(streams,  "\" : ");
        checker_base_print_type(self, errstream,
                                PslNode_convert_to_node_ptr(psl_node_get_left(expr)),
                                PslNode_convert_to_node_ptr(context));
        break;

      /* within operators */
    case PSL_WITHINBANG:
    case PSL_WITHIN:
    case PSL_WITHINBANG_:
    case PSL_WITHIN_:
      {
        PslNode_ptr n1 = psl_node_get_left(psl_node_get_left(expr));
        PslNode_ptr n2 = psl_node_get_right(psl_node_get_left(expr));
        PslNode_ptr n3 = psl_node_get_right(expr);

        StreamMgr_print_error(streams,  "operand types of \"");
        print_operator(errstream, expr);
        StreamMgr_print_error(streams,  "\" : (");
        checker_base_print_type(self, errstream,
                                PslNode_convert_to_node_ptr(n1),
                                PslNode_convert_to_node_ptr(context));
        StreamMgr_print_error(streams,  ", ");
        checker_base_print_type(self, errstream,
                                PslNode_convert_to_node_ptr(n2),
                                PslNode_convert_to_node_ptr(context));
        StreamMgr_print_error(streams,  ") ");
        checker_base_print_type(self, errstream,
                                PslNode_convert_to_node_ptr(n3),
                                PslNode_convert_to_node_ptr(context));
        break;
      }

      /* next operators */
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
        PslNode_ptr n1 = psl_node_extended_next_get_expr(expr);
        PslNode_ptr n2 = psl_node_extended_next_get_when(expr);
        PslNode_ptr n3 = psl_node_extended_next_get_condition(expr);

        StreamMgr_print_error(streams,  "operand types of \"");
        print_operator(errstream, expr);
        StreamMgr_print_error(streams,  "\" : ");

        if (n3 != PSL_NULL) {
          StreamMgr_print_error(streams,  " (");
          checker_base_print_type(self, errstream,
                                  PslNode_convert_to_node_ptr(n3),
                                  PslNode_convert_to_node_ptr(context));
          StreamMgr_print_error(streams,  ")");
        }

        if (n2 != PSL_NULL) {
          StreamMgr_print_error(streams,  " [");
          checker_base_print_type(self, errstream,
                                  PslNode_convert_to_node_ptr(n2),
                                  PslNode_convert_to_node_ptr(context));
          StreamMgr_print_error(streams,  "]");
        }

        nusmv_assert(n1 != PSL_NULL); /* n1 must occur here */
        StreamMgr_print_error(streams,  " (");
        checker_base_print_type(self, errstream,
                                PslNode_convert_to_node_ptr(n1),
                                PslNode_convert_to_node_ptr(context));
        StreamMgr_print_error(streams,  ")");
        break;
      }

      /* Binary operators */
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
      StreamMgr_print_error(streams,  "operand types of \"");
      print_operator(errstream, expr);
      StreamMgr_print_error(streams, "\" : ");
      checker_base_print_type(self, errstream,
                      PslNode_convert_to_node_ptr(psl_node_get_left(expr)),
                      PslNode_convert_to_node_ptr(context));
      StreamMgr_print_error(streams, " and ");
      checker_base_print_type(self, errstream,
                      PslNode_convert_to_node_ptr(psl_node_get_right(expr)),
                      PslNode_convert_to_node_ptr(context));
      break;

    case PSL_ITE:
      StreamMgr_print_error(streams,  "operand types of \"");
      print_operator(errstream, expr);
      StreamMgr_print_error(streams, "\" : ");
      checker_base_print_type(self, errstream,
                      PslNode_convert_to_node_ptr(psl_node_get_ite_cond(expr)),
                      PslNode_convert_to_node_ptr(context));
      StreamMgr_print_error(streams, " ? ");
      checker_base_print_type(self, errstream,
                      PslNode_convert_to_node_ptr(psl_node_get_ite_then(expr)),
                      PslNode_convert_to_node_ptr(context));
      StreamMgr_print_error(streams, " : ");
      checker_base_print_type(self, errstream,
                      PslNode_convert_to_node_ptr(psl_node_get_ite_else(expr)),
                      PslNode_convert_to_node_ptr(context));
      break;


    default: /* unknown kind of an expression */
      error_unreachable_code();
    } /* switch (node_get_type(expr)) */

    StreamMgr_print_error(streams, "\n");
    break;

  default:
    error_unreachable_code(); /* unknown kind of error */

  } /* switch (errorKind) */

  return isError;
}



/**Static function*************************************************************

  Synopsis           [Just prints an expression's operator to output_stream]

  Description        [This function is the almost the same as
  print_sexp, except this function does not print the children of the node.
  The expr must be a correct expression.
  The function is used in printing of an error messages only.]

  SideEffects        []

  SeeAlso            [print_sexp]
******************************************************************************/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void print_operator(FILE* output_stream, PslNode_ptr expr)
{
  nusmv_assert(expr != PSL_NULL);
  switch (psl_node_get_op(expr)) {

  case PSL_INF: fprintf(output_stream, "inf"); return;

  case PSL_SERECONCAT: fprintf(output_stream, ";"); return;
  case PSL_SEREFUSION: fprintf(output_stream, ":"); return;

  case PSL_LBSPLAT: fprintf(output_stream, "[*"); return;
  case PSL_LBPLUSRB: fprintf(output_stream, "[+"); return;
  case PSL_LBEQ: fprintf(output_stream, "[="); return;
  case PSL_LBMINUSGT: fprintf(output_stream, "[->"); return;

  case PSL_REPLPROP: fprintf(output_stream, "forall"); return;

  case PSL_PIPEMINUSGT: fprintf(output_stream, "|->"); return;
  case PSL_PIPEEQGT: fprintf(output_stream, "|=>"); return;

  case PSL_ALWAYS: fprintf(output_stream, "always"); return;
  case PSL_NEVER: fprintf(output_stream, "never"); return;
  case PSL_EVENTUALLYBANG: fprintf(output_stream, "eventually!"); return;

  case PSL_WITHINBANG: fprintf(output_stream, "within!"); return;
  case PSL_WITHIN: fprintf(output_stream, "within"); return;
  case PSL_WITHINBANG_: fprintf(output_stream, "within!_"); return;
  case PSL_WITHIN_: fprintf(output_stream, "within_"); return;

  case PSL_NEXT_EVENT_ABANG: fprintf(output_stream, "next_event_a!"); return;
  case PSL_NEXT_EVENT_A: fprintf(output_stream, "next_event_a"); return;
  case PSL_NEXT_EVENT_EBANG: fprintf(output_stream, "next_event_e!"); return;
  case PSL_NEXT_EVENT_E: fprintf(output_stream, "next_event_e"); return;
  case PSL_NEXT_EVENTBANG: fprintf(output_stream, "next_event!"); return;
  case PSL_NEXT_EVENT: fprintf(output_stream, "next_event"); return;
  case PSL_NEXT_ABANG: fprintf(output_stream, "next_a!"); return;
  case PSL_NEXT_EBANG: fprintf(output_stream, "next_e!"); return;
  case PSL_NEXT_A: fprintf(output_stream, "next_a"); return;
  case PSL_NEXT_E: fprintf(output_stream, "next_e"); return;
  case PSL_NEXTBANG: fprintf(output_stream, "next!"); return;
  case PSL_NEXT: fprintf(output_stream, "next"); return;
  case PSL_X: fprintf(output_stream, "X"); return;
  case PSL_XBANG: fprintf(output_stream, "X!"); return;

  case PSL_BEFOREBANG: fprintf(output_stream, "before!"); return;
  case PSL_BEFORE: fprintf(output_stream, "before"); return;
  case PSL_BEFOREBANG_: fprintf(output_stream, "before!_"); return;
  case PSL_BEFORE_: fprintf(output_stream, "before_"); return;
  case PSL_UNTILBANG: fprintf(output_stream, "until!"); return;
  case PSL_UNTIL: fprintf(output_stream, "until"); return;
  case PSL_UNTILBANG_: fprintf(output_stream,"until!_"); return;
  case PSL_UNTIL_: fprintf(output_stream,"until_"); return;
  case PSL_ABORT: fprintf(output_stream,"abort"); return;
  case PSL_W: fprintf(output_stream,"W"); return;
  case PSL_OR: fprintf(output_stream,"or"); return;
  case PSL_CARET: fprintf(output_stream,"^"); return;
  case PSL_TILDE: fprintf(output_stream,"~"); return;
  case PSL_EQEQ: fprintf(output_stream,"=="); return;
  case PSL_PIPEPIPE: fprintf(output_stream,"||"); return;
  case PSL_AMPERSANDAMPERSAND: fprintf(output_stream,"&&"); return;
  case PSL_WHILENOTBANG: fprintf(output_stream,"whilenot!"); return;
  case PSL_WHILENOT: fprintf(output_stream,"whilenot"); return;
  case PSL_WHILENOTBANG_: fprintf(output_stream,"whilenot!_"); return;
  case PSL_WHILENOT_: fprintf(output_stream,"whilenot_"); return;

  case PSL_ITE: fprintf(output_stream,"ITE"); return;

    /* these are for sere compound */
  case AND: fprintf(output_stream,"&"); return;
  case OR: fprintf(output_stream,"|"); return;

  default:
    error_unreachable_code_msg("\n%d\n", psl_node_get_op(expr));
  }

}




/**AutomaticEnd***************************************************************/

