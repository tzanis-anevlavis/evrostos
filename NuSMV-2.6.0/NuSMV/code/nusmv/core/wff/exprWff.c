/* ---------------------------------------------------------------------------


  This file is part of the ``wff'' package of NuSMV version 2.
  Copyright (C) 2000-2011 by FBK-irst and University of Trento.

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
  \author Alessandro Cimatti, Lorenzo Delana, Alessandro Mariotti
  \brief Well Formed Formula manipulation routines

  \todo: Missing description

*/


#if HAVE_CONFIG_H
#  include "nusmv-config.h"
#endif

#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"

#include "nusmv/core/wff/wff.h"

#include "nusmv/core/parser/symbols.h" /* for constants */
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/wff/lr/MasterLogicRecognizer.h"

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

static node_ptr expr_wff_make_binary(NodeMgr_ptr nodemgr,
                                     int type, node_ptr arg1,
                                     node_ptr arg2);

static node_ptr expr_wff_make_unary(NodeMgr_ptr nodemgr,
                                    int type, node_ptr arg);

static node_ptr expr_wff_make_const(NodeMgr_ptr nodemgr,
                                    int type);


/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

node_ptr Wff_make_truth(NodeMgr_ptr nodemgr)
{
  return expr_wff_make_const(nodemgr, TRUEEXP);
}

node_ptr Wff_make_falsity(NodeMgr_ptr nodemgr)
{
  return expr_wff_make_const(nodemgr, FALSEEXP);
}

node_ptr Wff_make_not(NodeMgr_ptr nodemgr, node_ptr arg)
{
  return expr_wff_make_unary(nodemgr, NOT, arg);
}

node_ptr Wff_make_and(NodeMgr_ptr nodemgr, node_ptr arg1, node_ptr arg2)
{
  node_ptr falsity;
  node_ptr truth;

  falsity = Wff_make_falsity(nodemgr);
  if ((arg1 == falsity) || (arg2 == falsity)) return falsity;

  truth = Wff_make_truth(nodemgr);
  if (arg1 == truth) return arg2;
  if (arg2 == truth) return arg1;

  return expr_wff_make_binary(nodemgr, AND, arg1, arg2);
}

node_ptr Wff_make_or(NodeMgr_ptr nodemgr, node_ptr arg1, node_ptr arg2)
{
  node_ptr falsity;
  node_ptr truth;

  truth = Wff_make_truth(nodemgr);
  if ((arg1 == truth) || (arg2 == truth)) return truth;

  falsity = Wff_make_falsity(nodemgr);
  if (arg1 == falsity) return arg2;
  if (arg2 == falsity) return arg1;

  return expr_wff_make_binary(nodemgr, OR, arg1, arg2);
}

node_ptr Wff_make_implies(NodeMgr_ptr nodemgr, node_ptr arg1, node_ptr arg2)
{
  return expr_wff_make_binary(nodemgr, IMPLIES, arg1, arg2);
}

node_ptr Wff_make_iff(NodeMgr_ptr nodemgr, node_ptr arg1, node_ptr arg2)
{
  return expr_wff_make_binary(nodemgr, IFF, arg1, arg2);
}

node_ptr Wff_make_next(NodeMgr_ptr nodemgr, node_ptr arg)
{
  return expr_wff_make_unary(nodemgr, NEXT, arg);
}

node_ptr Wff_make_opnext_times(NodeMgr_ptr nodemgr, node_ptr arg, int x)
{
  nusmv_assert(x >= 0);

  if (x == 0)
    return arg;
  else
    return Wff_make_opnext(nodemgr, Wff_make_opnext_times(nodemgr, arg, x - 1));
}

node_ptr Wff_make_opnext(NodeMgr_ptr nodemgr, node_ptr arg)
{
  return expr_wff_make_unary(nodemgr, OP_NEXT, arg);
}

node_ptr Wff_make_opprec(NodeMgr_ptr nodemgr, node_ptr arg)
{
  return expr_wff_make_unary(nodemgr, OP_PREC, arg);
}

node_ptr Wff_make_opnotprecnot(NodeMgr_ptr nodemgr, node_ptr arg)
{
  return expr_wff_make_unary(nodemgr, OP_NOTPRECNOT, arg);
}

node_ptr Wff_make_globally(NodeMgr_ptr nodemgr, node_ptr arg)
{
  return expr_wff_make_unary(nodemgr, OP_GLOBAL, arg);
}

node_ptr Wff_make_historically(NodeMgr_ptr nodemgr, node_ptr arg)
{
  return expr_wff_make_unary(nodemgr, OP_HISTORICAL, arg);
}

node_ptr Wff_make_eventually(NodeMgr_ptr nodemgr, node_ptr arg)
{
  return expr_wff_make_unary(nodemgr, OP_FUTURE, arg);
}

node_ptr Wff_make_once(NodeMgr_ptr nodemgr, node_ptr arg)
{
  return expr_wff_make_unary(nodemgr, OP_ONCE, arg);
}

node_ptr Wff_make_until(NodeMgr_ptr nodemgr, node_ptr arg1, node_ptr arg2)
{
  return expr_wff_make_binary(nodemgr, UNTIL, arg1, arg2);
}

node_ptr Wff_make_since(NodeMgr_ptr nodemgr, node_ptr arg1, node_ptr arg2)
{
  return expr_wff_make_binary(nodemgr, SINCE, arg1, arg2);
}

node_ptr Wff_make_releases(NodeMgr_ptr nodemgr, node_ptr arg1, node_ptr arg2)
{
  return expr_wff_make_binary(nodemgr, RELEASES, arg1, arg2);
}

node_ptr Wff_make_triggered(NodeMgr_ptr nodemgr, node_ptr arg1, node_ptr arg2)
{
  return expr_wff_make_binary(nodemgr, TRIGGERED, arg1, arg2);
}

int Wff_get_depth(const NuSMVEnv_ptr env, node_ptr ltl_wff)
{
  int depth = -1;
  int d1,d2;

  switch (node_get_type(ltl_wff)) {
  case TRUEEXP:                           /* TRUEEXP   */
  case FALSEEXP:                          /* FALSEEXP  */
    depth = 0;
    break;

  case NOT:                             /* NOT       */
    depth = Wff_get_depth(env, car(ltl_wff));
    break;

  case AND:                             /* AND       */
  case OR:                              /* OR        */
  case IFF:                             /* IFF        */
    d1 = Wff_get_depth(env, car(ltl_wff));
    d2 = Wff_get_depth(env, cdr(ltl_wff));
    depth = max(d1,d2);
    break;

  case OP_NEXT:                         /* OP_NEXT   */
  case OP_PREC:                         /* OP_PREC   */
  case OP_NOTPRECNOT:                   /* OP_NOTPRECNOT */
  case OP_GLOBAL:                       /* OP_GLOBAL */
  case OP_HISTORICAL:                   /* OP_HISTORICAL */
  case OP_FUTURE:                       /* OP_FUTURE */
  case OP_ONCE:                         /* OP_ONCE */
    depth = 1 + Wff_get_depth(env, car(ltl_wff));
    break;

  case UNTIL:                           /* UNTIL     */
  case SINCE:                           /* SINCE     */
  case RELEASES:                        /* RELEASES  */
  case TRIGGERED:                       /* TRIGGERED */
    d1 = Wff_get_depth(env, car(ltl_wff));
    d2 = Wff_get_depth(env, cdr(ltl_wff));
    depth = 1 + max(d1,d2);
    break;

  case IMPLIES:
    {
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
      ErrorMgr_internal_error(errmgr, "implies should have been nnf-ef away!\n");
      break;
    }

  case BIT:
  case DOT:
  case ARRAY:
    depth = 0;
    break;

    /* in pure bool bmc these nodes these nodes are not present,
       whereas the mathsat add-on calls this function while building
       the tableau for LTL specifications. In the latter case the
       formula is no longer guaranteed to be pure boolean.*/
  case CAST_BOOL:
  case EQUAL:
  case NOTEQUAL:
  case LT:
  case GT:
  case LE:
  case GE:
  case SETIN:
    depth = 0;
    break;

    /* these nodes are expected to be unreachable as already handled
       by previous cases */
  case ATOM:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
    {
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
      ErrorMgr_internal_error(errmgr, "Unexpected leaf, node type %d\n",
                              node_get_type(ltl_wff));
    }
    break;

  case EX:
  case AX:
  case EF:
  case AF:
  case EG:
  case AG:
  case EBF:
  case EBG:
  case ABF:
  case ABG:
    {
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
      ErrorMgr_internal_error(errmgr, "Unexpected CTL operator, node type %d\n",
                              node_get_type(ltl_wff));
    }
    break;

  default:
    {
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
      /* no other cases are currently allowed */
      ErrorMgr_internal_error(errmgr, "Unexpected node, node type %d\n",
                              node_get_type(ltl_wff));
    }
  }

  return depth;
}

boolean Wff_is_propositional(SymbTable_ptr symb_table,
                             node_ptr wff,
                             node_ptr context,
                             boolean is_next_allowed)
{
  NuSMVEnv_ptr env = NULL;
  MasterLogicRecognizer_ptr master_recogn = NULL;

  TypeChecker_ptr type_checker = NULL;
  SymbType_ptr symb_type = NULL;
  LogicType logic = EXP_NONE;
  boolean retval = true;

  SYMB_TABLE_CHECK_INSTANCE(symb_table);
  nusmv_assert(retval);

  env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  master_recogn = MASTER_LOGIC_RECOGNIZER(NuSMVEnv_get_value(env, ENV_MASTER_LOGIC_RECOGNIZER));

  type_checker = SymbTable_get_type_checker(symb_table);

  if (TypeChecker_is_expression_type_checked(type_checker, wff, context)) {
    symb_type = TypeChecker_get_expression_type(type_checker, wff, context);
    if (! SymbType_is_boolean(symb_type)) retval = false;
  }

  if (retval) {
    logic = MasterLogicRecognizer_recognize(master_recogn, wff, context);
    switch(logic) {
    case EXP_CTL:
    case EXP_LTL:
      retval = false; break;

    case EXP_NEXT:
      if (! is_next_allowed) retval = false;
      break;

    case EXP_SIMPLE:
      break;

    default:
      error_unreachable_code();
    }
  }

  if (retval) {
    if (! TypeChecker_is_expression_type_checked(type_checker, wff, context)) {
      symb_type = TypeChecker_get_expression_type(type_checker, wff, context);
      if (! SymbType_is_boolean(symb_type)) retval = false;
    }
  }

  return retval;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/



/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Makes a <b>binary</b> WFF

  

  \se node hash may change
*/
static node_ptr expr_wff_make_binary(NodeMgr_ptr nodemgr,
                                     int type, node_ptr arg1, node_ptr arg2)
{
  return find_node(nodemgr, type, arg1, arg2);
}

/*!
  \brief Makes a <b>unary</b> WFF

  

  \se node hash may change
*/
static node_ptr expr_wff_make_unary(NodeMgr_ptr nodemgr,
                                    int type, node_ptr arg)
{
  return find_node(nodemgr, type, arg, Nil);
}

/*!
  \brief Makes a <b>constant</b> WFF

  

  \se node hash may change
*/
static node_ptr expr_wff_make_const(NodeMgr_ptr nodemgr, int type)
{
  return find_node(nodemgr, type, Nil, Nil);
}
