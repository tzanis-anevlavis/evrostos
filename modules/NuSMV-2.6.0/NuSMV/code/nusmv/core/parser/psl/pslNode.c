/* ---------------------------------------------------------------------------


This file is part of the ``parser.psl'' package of NuSMV version 2.
Copyright (C) 2005 by FBK-irst.

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
  \author Roberto Cavada, Marco Roveri
  \brief Implementation of the PslNode structure

  \todo: Missing description

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/parser/psl/pslNode.h"
#include "nusmv/core/parser/psl/pslExpr.h"
#include "nusmv/core/parser/psl/psl_symbols.h"
#include "nusmv/core/parser/psl/pslInt.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/utils/ustring.h"
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

static boolean psl_node_is_equal(PslNode_ptr e, PslNode_ptr f);
static boolean psl_node_is_star_free(PslNode_ptr expr);
static boolean psl_node_is_unbound_star_free(const NuSMVEnv_ptr env,
                                             PslNode_ptr expr);
static boolean psl_node_is_emptystar_free(PslNode_ptr expr);
static boolean psl_node_is_handled_next(const NuSMVEnv_ptr env,
                                        PslNode_ptr expr);
static boolean psl_node_is_handled_fl_op(PslOp op);
static boolean psl_node_is_fl_op(PslOp op);
static boolean psl_node_is_obe_op(PslOp op);

static boolean
psl_node_is_handled_sere(const NuSMVEnv_ptr env, PslNode_ptr e, boolean toplevel);

static boolean
psl_node_is_propositional(const PslNode_ptr expr,
                          boolean accept_next);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

PslNode_ptr PslNode_convert_from_node_ptr(node_ptr expr)
{
  if (expr == Nil) return PSL_NULL;
  return (PslNode_ptr) expr;
}

node_ptr PslNode_convert_to_node_ptr(PslNode_ptr expr)
{
  if (expr == PSL_NULL) return Nil;
  return (node_ptr) expr;
}

PslNode_ptr PslNode_new_context(NodeMgr_ptr nodemgr,
                                PslNode_ptr ctx, PslNode_ptr node)
{
  return psl_new_node(nodemgr, CONTEXT, ctx, node);
}

boolean PslNode_is_handled_psl(const NuSMVEnv_ptr env, PslNode_ptr e)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  PslOp op;

  if (e == PSL_NULL) return true;

  op = psl_node_get_op(e);

  /* no sere */
  if (PslNode_is_propositional(e)) return true;

  /* no obe expressions */
  if (psl_node_is_obe_op(op)) return false;

  /* weak suffix operator */
  if (psl_node_is_suffix_implication_weak(e)) {
    StreamMgr_print_error(streams,  "In PSL expression '");
    StreamMgr_nprint_error(streams, wffprint, "%N", e);
    StreamMgr_print_error(streams,  "'\nWeak suffix implications are not currently supported.\n");
    return false;
  }

  /* operators we do not deal with */
  if (!psl_node_is_handled_fl_op(op)) {
    StreamMgr_print_error(streams,  "In PSL expression '");
    StreamMgr_nprint_error(streams, wffprint, "%N", e);
    StreamMgr_print_error(streams,  "'\nPSL operator not currently supported.\n");
    return false;
  }

  if (psl_node_is_extended_next(e) && !psl_node_is_handled_next(env, e)) {
    StreamMgr_print_error(streams,  "In PSL expression '");
    StreamMgr_nprint_error(streams, wffprint, "%N", e);
    StreamMgr_print_error(streams,  "'\nPSL next expression contains "\
            "an unsupported feature.\n");
    return false;
  }

  if (psl_node_is_suffix_implication(e)) {
    PslNode_ptr pre = psl_node_suffix_implication_get_premise(e);
    PslNode_ptr con = psl_node_suffix_implication_get_consequence(e);

    if (!psl_node_is_unbound_star_free(env, pre)) {
      StreamMgr_print_error(streams,  "In PSL expression '");
      StreamMgr_nprint_error(streams, wffprint, "%N", e);
      StreamMgr_print_error(streams,  "'\nPremise in suffix implication "\
              "contains an unsupported feature.\n");
      return false;
    }

    if (!PslNode_is_handled_psl(env, pre) || !PslNode_is_handled_psl(env, con)) {
      return false;
    }

    return true;
  }

  if (op==PSL_SERE || op==PSL_SERECONCAT || op==PSL_SEREFUSION ||
      op==PSL_SERECOMPOUND || op==PSL_SEREREPEATED) {
    if (psl_node_is_handled_sere(env, e, true)) return true;

    StreamMgr_print_error(streams,  "In PSL expression '");
    StreamMgr_nprint_error(streams, wffprint, "%N", e);
    StreamMgr_print_error(streams,  "'\nPSL SERE expression contains an "\
            "unsupported feature.\n");
    return false;
  }

  /* applies to binary and unary FL operators */
  return PslNode_is_handled_psl(env, psl_node_get_left(e)) &&
    PslNode_is_handled_psl(env, psl_node_get_right(e));
}

boolean PslNode_is_propositional(const PslNode_ptr expr)
{
  return psl_node_is_propositional(expr, false);
}

boolean PslNode_is_trans_propositional(const PslNode_ptr expr)
{
  return psl_node_is_propositional(expr, true);
}

boolean PslNode_propositional_contains_next(const PslNode_ptr expr)
{
  boolean result = false;

  if (expr == PSL_NULL) return false;

  if (psl_node_is_leaf(expr) || psl_node_is_id(expr)) return false;

  switch (psl_node_get_op(expr)) {

  case NEXT:
    result = true;
    break;

  case CONTEXT:
    result = PslNode_propositional_contains_next(psl_node_get_right(expr));
    break;

  case MINUS:
  case PLUS:
  case NOT:
  case PSL_TILDE:
    result = PslNode_propositional_contains_next(psl_node_get_left(expr));
    break;

  case UNION:
  case SETIN:
  case TIMES:
  case DIVIDE:
  case MOD:
  case PSL_EQEQ:
  case EQUAL:
  case NOTEQUAL:
  case LT:
  case LE:
  case GT:
  case GE:
  case AND:
  case OR:
  case PSL_OR:
  case XOR:
  case PSL_CARET:
  case IFF:
  case IMPLIES:
    result = PslNode_propositional_contains_next(psl_node_get_left(expr)) ||
      PslNode_propositional_contains_next(psl_node_get_right(expr));
    break;

  case CASE:
    result = PslNode_propositional_contains_next(psl_node_get_case_cond(expr)) ||
      PslNode_propositional_contains_next(psl_node_get_case_then(expr)) ||
      PslNode_propositional_contains_next(psl_node_get_case_next(expr));
    break;

  case PSL_ITE:
    result = PslNode_propositional_contains_next(psl_node_get_ite_cond(expr)) ||
      PslNode_propositional_contains_next(psl_node_get_ite_then(expr)) ||
      PslNode_propositional_contains_next(psl_node_get_ite_else(expr));
    break;

  case PSL_REPLPROP:
    result = PslNode_propositional_contains_next(psl_node_repl_prop_get_property(expr));
    break;

  default:
    result = false;
  }

  return result;
}

boolean PslNode_is_obe(const PslNode_ptr expr)
{
  boolean result = false;

  if (expr == PSL_NULL) return true;

  /* no FL expressions */
  if (psl_node_is_fl_op(psl_node_get_op(expr))) return false;

  switch (psl_node_get_op(expr)) {
  case CONTEXT:
    result = PslNode_is_obe(psl_node_get_right(expr));
    break;

    /* id */
  case DOT:
  case ATOM:
  case ARRAY:

    /* boolean */
  case TRUEEXP:
  case FALSEEXP:

  case FAILURE:

    /* numbers */
  case NUMBER:
    /* primary */
  case MINUS:
  case PLUS:
    /* binary operators */
  case UNION:
  case SETIN:
  case TIMES:
  case DIVIDE:
  case MOD:
  case PSL_EQEQ:
  case EQUAL:
  case NOTEQUAL:
  case LT:
  case LE:
  case GT:
  case GE:
    result = true;
    break;
    /* boolean binary operators */
  case NOT:
  case PSL_TILDE:
    result = PslNode_is_obe(psl_node_get_left(expr));
    break;
  case AND:
  case OR:
  case PSL_OR:
  case PSL_CARET:
  case XOR:
  case IFF:
  case IMPLIES:
    result = PslNode_is_obe(psl_node_get_left(expr)) &&
      PslNode_is_obe(psl_node_get_right(expr));
    break;
    /* next operators */
  case AX:
  case EX:
    /* globally operators */
  case AG:
  case EG:
    /* eventually operators */
  case AF:
  case EF:
    /* Until operators */
    result = PslNode_is_obe(psl_node_get_left(expr));
    break;
    /* until* operators */
  case EU:
  case AU:
    result = PslNode_is_obe(psl_node_get_left(expr)) &&
      PslNode_is_obe(psl_node_get_right(expr));
    break;

    /* case: */
  case CASE:
    result = PslNode_is_obe(psl_node_get_case_cond(expr)) &&
      PslNode_is_obe(psl_node_get_case_then(expr)) &&
      PslNode_is_obe(psl_node_get_case_next(expr));
    break;

  case PSL_ITE:
    result = PslNode_is_obe(psl_node_get_ite_cond(expr)) &&
      PslNode_is_obe(psl_node_get_ite_then(expr)) &&
      PslNode_is_obe(psl_node_get_ite_else(expr));
    break;

  case PSL_REPLPROP:
    result = PslNode_is_obe(psl_node_repl_prop_get_property(expr));
    break;

  default:
    result = false;
  }

  return result;
}

boolean PslNode_is_ltl(const PslNode_ptr expr)
{
  boolean result = false;

  if (expr == PSL_NULL) return true;

  /* no obe expressions */
  if (psl_node_is_obe_op(psl_node_get_op(expr))) return false;

  switch (psl_node_get_op(expr)) {

  case CONTEXT:
    result = PslNode_is_ltl(psl_node_get_right(expr));
    break;

    /* id */
  case DOT:
  case ATOM:
  case ARRAY:

    /* boolean */
  case TRUEEXP:
  case FALSEEXP:

  case FAILURE:

    /* numbers */
  case NUMBER:
    /* primary */
  case MINUS:
  case PLUS:
    /* binary operators */
  case UNION:
  case SETIN:
  case TIMES:
  case DIVIDE:
  case MOD:
  case PSL_EQEQ:
  case EQUAL:
  case NOTEQUAL:
  case LT:
  case LE:
  case GT:
  case GE:
    result = true;
    break;

  case NOT:
  case PSL_TILDE:
    result = PslNode_is_ltl(psl_node_get_left(expr));
    break;
    /* boolean binary operators */
  case AND:
  case OR:
  case PSL_OR:
  case PSL_CARET:
  case XOR:
  case IFF:
  case IMPLIES:
    result = PslNode_is_ltl(psl_node_get_left(expr)) &&
      PslNode_is_ltl(psl_node_get_right(expr));
    break;

    /* case: */
  case CASE:
    result = PslNode_is_ltl(psl_node_get_case_cond(expr)) &&
      PslNode_is_ltl(psl_node_get_case_then(expr)) &&
      PslNode_is_ltl(psl_node_get_case_next(expr));
    break;

  case PSL_ITE:
    result = PslNode_is_ltl(psl_node_get_ite_cond(expr)) &&
      PslNode_is_ltl(psl_node_get_ite_then(expr)) &&
      PslNode_is_ltl(psl_node_get_ite_else(expr));
    break;

    /* next* operators */
  case OP_NEXT:
  case PSL_X:
  case PSL_XBANG:
  case PSL_NEXT:
  case PSL_NEXTBANG:
  case PSL_NEXT_E:
  case PSL_NEXT_EBANG:
  case PSL_NEXT_A:
  case PSL_NEXT_ABANG:
  case PSL_NEXT_EVENT:
  case PSL_NEXT_EVENTBANG:
  case PSL_NEXT_EVENT_E:
  case PSL_NEXT_EVENT_EBANG:
  case PSL_NEXT_EVENT_A:
  case PSL_NEXT_EVENT_ABANG:
    {
      PslNode_ptr l = psl_node_get_left(expr);
      PslNode_ptr r = psl_node_get_right(expr);

      result = PslNode_is_ltl(l);

      if (result && (r != PSL_NULL)) {

        nusmv_assert(psl_node_get_op(r) == COLON);

        /* checks the boolean expression if there is any */
        if (psl_node_get_right(r) != PSL_NULL) {
          result = PslNode_is_ltl(psl_node_get_right(r));
        }

        /* Here there is either an expression or a range */
        if (result && (psl_node_get_left(r) != PSL_NULL)) {
          switch (psl_node_get_op(psl_node_get_left(r))) {
          case PSL_RANGE:
            break;
          default:
            result = result && PslNode_is_ltl(psl_node_get_left(r));
            break;
          }
        }
      }
    }
    break; /* end of next operators case */

    /* eventually operators */
  case OP_FUTURE:
    /* never */
  case PSL_NEVER:
    /* globally */
  case OP_GLOBAL:
  case PSL_ALWAYS:
    /* eventually */
  case PSL_EVENTUALLYBANG:
    result = PslNode_is_ltl(psl_node_get_left(expr));
    break;
    /* until* operators */
  case UNTIL:
  case PSL_W:
  case PSL_UNTILBANG:
  case PSL_UNTIL:
  case PSL_UNTIL_:
  case PSL_UNTILBANG_:
  case PSL_BEFORE:
  case PSL_BEFOREBANG:
  case PSL_BEFORE_:
  case PSL_BEFOREBANG_:
    result = PslNode_is_ltl(psl_node_get_left(expr)) &&
      PslNode_is_ltl(psl_node_get_right(expr));
    break;
  case PSL_REPLPROP:
    result = PslNode_is_ltl(psl_node_repl_prop_get_property(expr));
    break;

  default:
    result = false;
  }

  return result;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

PslNode_ptr psl_new_node(NodeMgr_ptr nodemgr, PslOp op,
                         PslNode_ptr left, PslNode_ptr right)
{
  return (PslNode_ptr) NodeMgr_find_node(nodemgr, (int) op,
                                             (node_ptr) left,
                                             (node_ptr) right);
}

PslOp psl_node_get_op(PslNode_ptr n)
{ return (PslOp) node_get_type(((node_ptr) n)); }

PslNode_ptr psl_node_get_left(PslNode_ptr n)
{ return (PslNode_ptr) car((node_ptr) n); }

PslNode_ptr psl_node_get_right(PslNode_ptr n)
{ return (PslNode_ptr) cdr((node_ptr) n); }

void psl_node_set_left(PslNode_ptr n, PslNode_ptr l)
{ setcar((node_ptr) n, (node_ptr) l); }

void psl_node_set_right (PslNode_ptr n, PslNode_ptr r)
{ setcdr((node_ptr) n, (node_ptr) r); }

PslNode_ptr psl_node_make_true(NodeMgr_ptr nodemgr)
{ return psl_new_node(nodemgr, TRUEEXP, PSL_NULL, PSL_NULL); }

boolean psl_node_is_true(PslNode_ptr self)
{ return psl_node_get_op(self)==TRUEEXP; }

PslNode_ptr psl_node_make_false(NodeMgr_ptr nodemgr)
{ return psl_new_node(nodemgr, FALSEEXP, PSL_NULL, PSL_NULL); }

boolean psl_node_is_false(PslNode_ptr self)
{ return psl_node_get_op(self)==FALSEEXP; }

boolean psl_node_is_sere(PslNode_ptr expr)
{
  PslOp op;
  if (expr == PSL_NULL) return false;

  op = psl_node_get_op(expr);

  return (op == PSL_SERE || op == PSL_SERECONCAT ||
          op == PSL_SEREFUSION || op == PSL_SEREREPEATED ||
          op == PSL_SERECOMPOUND);
}

boolean psl_node_is_serebrackets(PslNode_ptr expr)
{
  PslOp op;
  if (expr == PSL_NULL) return false;

  op = psl_node_get_op(expr);

  return (op == PSL_SERE);
}

PslNode_ptr psl_node_sere_star_get_count(const PslNode_ptr e)
{
  nusmv_assert(psl_node_sere_is_star(e));

  if (psl_node_get_right(e) == PSL_NULL) return PSL_EMPTYSTAR;
  else {
    PslNode_ptr nc = psl_node_get_right(e);
    return nc;
  }
}

boolean psl_node_is_handled_star(const NuSMVEnv_ptr env, PslNode_ptr expr, boolean toplevel)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  PslNode_ptr repeated_expr;

  /* precond: expr must be a repeated sere */
  nusmv_assert(psl_node_sere_is_repeated(expr));

  /* [* count] are handled if count is a number */
  if (psl_node_sere_is_star_count(expr)) {
    PslNode_ptr count = psl_node_sere_star_get_count(expr);
    if (psl_node_is_number(count)) {
      /* star count might be applied to unbound stars */
      return (psl_node_number_get_value(count) > 0) &&
        psl_node_is_handled_sere(env, psl_node_sere_repeated_get_expr(expr),
                                 toplevel);
    }
    StreamMgr_print_error(streams,  "In expression ");
    StreamMgr_nprint_error(streams, wffprint, "%N", expr);
    ErrorMgr_error_expected_number(errmgr);
  }

  /* r[= count] and r[->] are not handled */
  if (psl_node_sere_is_stareq(expr) || psl_node_sere_is_starminusgt(expr)) {
    return false;
  }

  /* toplevel stars are not handled, only plus are handled at toplevel */
  if (toplevel && psl_node_sere_is_star(expr)) return false;

  /* here we are sure expr is a repeated sere, and not a toplevel *,
     a [*Range], a [= count], [->] */

  repeated_expr = psl_node_sere_repeated_get_expr(expr);

  /* from here on we are sure we are not toplevel and we handle only
     [*] and [+] that are stand alone or applied to propositionals */
  return ((repeated_expr == PSL_NULL) ||
          PslNode_is_propositional(repeated_expr));
}

boolean psl_node_sere_is_propositional(PslNode_ptr e)
{
  if (e==NULL) return false;
  if (psl_node_get_op(e)!=PSL_SERE) return false;
  if (PslNode_is_propositional(psl_node_get_left(e))) return true;
  return psl_node_sere_is_propositional(psl_node_get_left(e));
}

boolean psl_node_sere_is_repeated(PslNode_ptr e)
{
  return (e != PSL_NULL) && (psl_node_get_op(e)==PSL_SEREREPEATED);
}

boolean psl_node_sere_is_star(PslNode_ptr e)
{
  return psl_node_sere_is_repeated(e) &&
    (psl_node_get_op(psl_node_get_left(e))==PSL_LBSPLAT);
}

boolean psl_node_sere_is_stareq(PslNode_ptr e)
{
  return psl_node_sere_is_repeated(e) &&
    psl_node_get_op(psl_node_get_left(e))==PSL_LBEQ;
}

boolean psl_node_sere_is_starminusgt(PslNode_ptr e)
{
  return psl_node_sere_is_repeated(e) &&
    psl_node_get_op(psl_node_get_left(e))==PSL_LBMINUSGT;
}

boolean psl_node_sere_is_standalone_star(PslNode_ptr e)
{
  return psl_node_sere_is_star(e) &&
    psl_node_sere_repeated_get_expr(e) == PSL_NULL;
}

boolean psl_node_sere_is_plus(PslNode_ptr e)
{
  return psl_node_sere_is_repeated(e) &&
    (psl_node_get_op(psl_node_get_left(e))==PSL_LBPLUSRB);
}

boolean psl_node_sere_is_standalone_plus(PslNode_ptr e)
{
  return psl_node_sere_is_plus(e) &&
    (psl_node_sere_repeated_get_expr(e) == PSL_NULL);
}

boolean psl_node_sere_is_star_count(PslNode_ptr e)
{
  if (psl_node_sere_is_star(e)) {
    PslNode_ptr count = psl_node_sere_star_get_count(e);
    return (count != PSL_EMPTYSTAR);
  }
  return false;
}

boolean psl_node_sere_is_star_count_zero(PslNode_ptr e)
{
  return (e != PSL_NULL) && (psl_node_sere_is_star_count(e)) &&
    (psl_node_is_number(psl_node_sere_star_get_count(e))) &&
    (0 == psl_node_number_get_value(psl_node_sere_star_get_count(e)));
}

PslNode_ptr psl_node_sere_propositional_get_expr(PslNode_ptr e)
{
  PslNode_ptr expr;

  nusmv_assert(psl_node_sere_is_propositional(e));

  expr = psl_node_get_left(e);

  if (expr == PSL_NULL) return PSL_NULL;

  /* getting rid of { } */
  while (psl_node_get_op(expr)==PSL_SERE) expr = psl_node_get_left(expr);

  /* here expr it is not a SERE */
  return expr;
}

PslNode_ptr psl_node_sere_compound_get_left(PslNode_ptr e)
{
  PslNode_ptr left;

  nusmv_assert(psl_node_is_sere_compound_binary(e));
  nusmv_assert(psl_node_get_left(e) != PSL_NULL);

  left = psl_node_get_left(psl_node_get_left(e));

  return left;
}

PslNode_ptr psl_node_sere_compound_get_right(PslNode_ptr e)
{
  PslNode_ptr right;

  nusmv_assert(psl_node_is_sere_compound_binary(e));
  nusmv_assert(psl_node_get_left(e) != PSL_NULL);

  right = psl_node_get_right(psl_node_get_left(e));

  return right;
}

PslNode_ptr psl_node_sere_repeated_get_expr(PslNode_ptr e)
{
  PslNode_ptr expr;

  nusmv_assert(psl_node_sere_is_repeated(e));

  expr = psl_node_get_left(psl_node_get_left(e));

  if (expr == PSL_NULL) return PSL_NULL;

  /* getting rid of { } */
  while (psl_node_get_op(expr)==PSL_SERE) expr = psl_node_get_left(expr);

  /* here expr it is not a SERE */
  return expr;
}

PslNode_ptr psl_node_sere_repeated_get_count(PslNode_ptr e)
{
  PslNode_ptr cnt = PSL_EMPTYSTAR;

  nusmv_assert(psl_node_sere_is_repeated(e));

  if (PSL_NULL != psl_node_get_right(e)) {
    cnt = psl_node_get_right(e);
  }

  return cnt;
}

PslOp psl_node_sere_repeated_get_op(PslNode_ptr e)
{
  nusmv_assert(psl_node_sere_is_repeated(e));

  return psl_node_get_op(psl_node_get_left(e));
}

PslNode_ptr
psl_node_make_sere_propositional(NodeMgr_ptr nodemgr, PslNode_ptr expr)
{
  return psl_new_node(nodemgr, PSL_SERE, expr, PSL_NULL);
}

PslNode_ptr
psl_node_make_sere_concat(NodeMgr_ptr nodemgr, PslNode_ptr seq1, PslNode_ptr seq2)
{
  return psl_new_node(nodemgr, PSL_SERECONCAT, seq1, seq2) ;
}

PslNode_ptr
psl_node_make_sere_star(NodeMgr_ptr nodemgr, PslNode_ptr seq)
{
  return psl_new_node(nodemgr, PSL_SEREREPEATED,
                      psl_new_node(nodemgr, PSL_LBSPLAT, seq, PSL_NULL), PSL_NULL);
}

PslNode_ptr psl_node_sere_star_get_starred(PslNode_ptr e)
{
  nusmv_assert(psl_node_sere_is_star(e));
  return psl_node_get_left(psl_node_get_left(e));
}

PslNode_ptr
psl_node_make_sere_2ampersand(NodeMgr_ptr nodemgr,
                              PslNode_ptr seq1, PslNode_ptr seq2)
{
  return psl_node_make_sere_compound(nodemgr, seq1, PSL_AMPERSANDAMPERSAND, seq2);
}

PslNode_ptr
psl_node_make_sere_compound(NodeMgr_ptr nodemgr, PslNode_ptr seq1,
                            PslOp op, PslNode_ptr seq2)
{
  return psl_new_node(nodemgr, PSL_SERECOMPOUND,
                      psl_new_node(nodemgr, op, seq1, seq2),
                      PSL_NULL);
}

boolean psl_node_is_sere_compound_binary(PslNode_ptr e)
{
  if (e == PSL_NULL) return false;
  return (psl_node_get_op(e) == PSL_SERECOMPOUND);
}

boolean psl_node_is_suffix_implication(PslNode_ptr expr)
{
  PslOp op;

  if (expr == PSL_NULL) return false;

  op = psl_node_get_op(expr);
  return (op == PSL_PIPEMINUSGT || op == PSL_PIPEEQGT);
}

boolean psl_node_is_suffix_implication_weak(PslNode_ptr expr)
{
  return psl_node_is_suffix_implication(expr) &&
    (psl_node_get_right(expr) == PSL_NULL);
}

boolean psl_node_is_suffix_implication_strong(PslNode_ptr expr)
{
  return psl_node_is_suffix_implication(expr) &&
    (psl_node_get_right(expr) != PSL_NULL) &&
    (psl_node_get_op(psl_node_get_right(expr)) == NOT);
}

PslNode_ptr
psl_node_suffix_implication_get_premise(PslNode_ptr e)
{
  nusmv_assert(psl_node_is_suffix_implication(e));
  return psl_node_get_left(psl_node_get_left(e));
}

PslNode_ptr
psl_node_suffix_implication_get_consequence(PslNode_ptr e)
{
  nusmv_assert(psl_node_is_suffix_implication(e));
  return psl_node_get_right(psl_node_get_left(e));
}

boolean psl_node_sere_is_concat_holes_free(PslNode_ptr e)
{
  if (e == PSL_NULL) return false;

  if (psl_node_get_op(e)==PSL_SERE && !psl_node_is_sere(psl_node_get_left(e))) {
    return (psl_node_get_op(psl_node_get_left(e)) != PSL_UNTILBANG &&
            psl_node_get_op(psl_node_get_left(e)) != PSL_XBANG &&
            psl_node_get_op(psl_node_get_left(e)) != PSL_EVENTUALLYBANG);
  }

  if (psl_node_get_op(e)==PSL_SERE) {
    return psl_node_sere_is_concat_fusion_holes_free(psl_node_get_left(e));
  }

  if (psl_node_get_op(e)==PSL_SERECONCAT) {
    return psl_node_sere_is_concat_holes_free(psl_node_get_left(e)) &&
      psl_node_sere_is_concat_holes_free(psl_node_get_right(e));
  }

  return false;
}

boolean psl_node_sere_is_concat_fusion(PslNode_ptr e)
{
  if (e == PSL_NULL) return false;

  if (psl_node_get_op(e)==PSL_SERE &&
      !psl_node_is_sere(psl_node_get_left(e))) {
    return true;
  }

  if (psl_node_get_op(e)==PSL_SERE ) {
    return psl_node_sere_is_concat_fusion(psl_node_get_left(e));
  }

  if (psl_node_get_op(e)==PSL_SERECONCAT ||
      psl_node_get_op(e)==PSL_SEREFUSION) {
    return psl_node_sere_is_concat_fusion(psl_node_get_left(e)) &&
      psl_node_sere_is_concat_fusion(psl_node_get_right(e));
  }

  return false;
}

boolean psl_node_sere_is_concat(PslNode_ptr e)
{
  if (e == PSL_NULL) return false;

  if (psl_node_get_op(e)==PSL_SERE &&
      psl_node_get_left(e)!=NULL &&
      psl_node_get_op(psl_node_get_left(e))==PSL_SERECONCAT) {
    return true;
  }

  if (psl_node_get_op(e)==PSL_SERECONCAT) {
    return true;
  }

  return false;
}

PslNode_ptr psl_node_sere_concat_get_left(PslNode_ptr e)
{
  if (e == PSL_NULL) return false;

  if (psl_node_get_op(e)==PSL_SERE &&
      psl_node_get_left(e)!=NULL &&
      psl_node_get_op(psl_node_get_left(e))==PSL_SERECONCAT) {
    return psl_node_get_left(psl_node_get_left(e));
  } else
    return psl_node_get_left(e);
}

PslNode_ptr psl_node_sere_concat_get_right(PslNode_ptr e)
{
  if (e == PSL_NULL) return false;

  if (psl_node_get_op(e)==PSL_SERE &&
      psl_node_get_left(e)!=NULL &&
      psl_node_get_op(psl_node_get_left(e))==PSL_SERECONCAT) {
    return psl_node_get_right(psl_node_get_left(e));
  } else
    return psl_node_get_right(e);
}

PslNode_ptr psl_node_sere_concat_get_leftmost(PslNode_ptr e)
{
  if (psl_node_sere_is_concat(e))
    return psl_node_sere_concat_get_leftmost(psl_node_sere_concat_get_left(e));
  else
    return e;
}

PslNode_ptr psl_node_sere_concat_get_rightmost(PslNode_ptr e)
{
  if (psl_node_sere_is_concat(e))
    return
      psl_node_sere_concat_get_rightmost(psl_node_sere_concat_get_right(e));
  else
    return e;
}

PslNode_ptr psl_node_sere_concat_cut_leftmost(NodeMgr_ptr nodemgr,
                                              PslNode_ptr e)
{
  PslNode_ptr left;
  PslNode_ptr right;
  nusmv_assert(psl_node_sere_is_concat(e));
  left = psl_node_sere_concat_get_left(e);
  right = psl_node_sere_concat_get_right(e);
  if (psl_node_sere_is_concat(left))
    return psl_node_make_sere_concat(nodemgr,
                                     psl_node_sere_concat_cut_leftmost(nodemgr, left),
                                     right);
  else
    return right;
}

PslNode_ptr psl_node_sere_fusion_get_left(PslNode_ptr e)
{
  if (e == PSL_NULL) return false;

  if (psl_node_get_op(e)==PSL_SERE &&
      psl_node_get_left(e)!=NULL &&
      psl_node_get_op(psl_node_get_left(e))==PSL_SEREFUSION) {
    return psl_node_get_left(psl_node_get_left(e));
  } else
    return psl_node_get_left(e);
}

PslNode_ptr psl_node_sere_fusion_get_right(PslNode_ptr e)
{
  if (e == PSL_NULL) return false;

  if (psl_node_get_op(e)==PSL_SERE &&
      psl_node_get_left(e)!=NULL &&
      psl_node_get_op(psl_node_get_left(e))==PSL_SEREFUSION) {
    return psl_node_get_right(psl_node_get_left(e));
  } else
    return psl_node_get_right(e);
}

boolean psl_node_sere_is_fusion(PslNode_ptr e)
{
  if (e == PSL_NULL) return false;

  if (psl_node_get_op(e)==PSL_SERE &&
      psl_node_get_left(e)!=NULL &&
      psl_node_get_op(psl_node_get_left(e))==PSL_SEREFUSION) {
    return true;
  }

  if (psl_node_get_op(e)==PSL_SEREFUSION) {
    return true;
  }

  return false;
}

boolean psl_node_sere_is_or(PslNode_ptr e)
{
  if (e == PSL_NULL) return false;
  if (psl_node_get_left(e) == PSL_NULL) return false;
  if (psl_node_get_op(e) == PSL_SERE) {
    return psl_node_sere_is_or(psl_node_get_left(e));
  }
  return ((psl_node_get_op(e) == PSL_SERECOMPOUND) &&
          (psl_node_get_op(psl_node_get_left(e)) == OR));
}

boolean psl_node_sere_is_concat_fusion_holes_free(PslNode_ptr e)
{
  if (e == PSL_NULL) return false;

  if (psl_node_get_op(e)==PSL_SERE &&
      !psl_node_is_sere(psl_node_get_left(e))) {
    return (psl_node_get_op(psl_node_get_left(e)) != PSL_UNTILBANG &&
            psl_node_get_op(psl_node_get_left(e)) != PSL_XBANG &&
            psl_node_get_op(psl_node_get_left(e)) != PSL_EVENTUALLYBANG);
  }

  if (psl_node_get_op(e)==PSL_SERE ) {
    return psl_node_sere_is_concat_fusion_holes_free(psl_node_get_left(e));
  }

  if (psl_node_get_op(e)==PSL_SERECONCAT ||
      psl_node_get_op(e)==PSL_SEREFUSION) {
    return psl_node_sere_is_concat_fusion_holes_free(psl_node_get_left(e)) &&
      psl_node_sere_is_concat_fusion_holes_free(psl_node_get_right(e));
  }

  return false;
}

PslNode_ptr psl_node_prune(NodeMgr_ptr nodemgr,
                           PslNode_ptr tree, PslNode_ptr branch)
{
  PslOp op;

  if (tree == PSL_NULL) return PSL_NULL;

  op = psl_node_get_op(tree);

  /* base case */
  if (psl_node_is_equal(tree, branch)) return PSL_NULL;

  /* atomic tree which is not equal to the branch to be pruned */
  if (psl_node_sere_is_propositional(tree)) return tree;

  if (psl_node_is_equal(psl_node_get_left(tree), branch)) {
    return psl_node_get_right(tree);
  }

  if (psl_node_is_equal(psl_node_get_right(tree), branch)) {
    return psl_node_get_left(tree);
  }

  return psl_new_node(nodemgr, op,
                      psl_node_prune(nodemgr, psl_node_get_left(tree), branch),
                      psl_node_prune(nodemgr, psl_node_get_right(tree), branch));
}

boolean psl_node_is_propstar(PslNode_ptr e)
{
  PslNode_ptr expr;

  while (psl_node_get_op(e) == PSL_SERE) e = psl_node_get_left(e);

  if (!psl_node_sere_is_star(e)) return false;

  expr = psl_node_sere_repeated_get_expr(e);

  return (expr != PSL_NULL) &&
    PslNode_is_propositional(expr);
}

boolean psl_node_sere_is_2ampersand(PslNode_ptr e)
{
  return psl_node_is_sere_compound_binary(e) &&
    psl_node_get_op(psl_node_get_left(e)) == PSL_AMPERSANDAMPERSAND;
}

PslNode_ptr psl_node_make_cons(NodeMgr_ptr nodemgr,
                               PslNode_ptr elem, PslNode_ptr next)
{
  return psl_new_node(nodemgr, CONS, elem, next);
}

PslNode_ptr psl_node_make_cons_new(NodeMgr_ptr nodemgr,
                                   PslNode_ptr elem, PslNode_ptr next)
{
  return (PslNode_ptr) NodeMgr_new_node(nodemgr, CONS, (node_ptr) elem, (node_ptr) next);
}

boolean psl_node_is_cons(PslNode_ptr e)
{
  nusmv_assert(e!= PSL_NULL);
  if (psl_node_get_op(e) == CONS) return true;
  return false;
}

PslNode_ptr psl_node_cons_get_element(PslNode_ptr e)
{
  nusmv_assert(psl_node_is_cons(e));
  return psl_node_get_left(e);
}

PslNode_ptr psl_node_cons_get_next(PslNode_ptr e)
{
  nusmv_assert(psl_node_is_cons(e));
  return psl_node_get_right(e);
}

PslNode_ptr psl_node_cons_reverse(PslNode_ptr e)
{
  PslNode_ptr y;

  if (e == PSL_NULL) return PSL_NULL;
  nusmv_assert(psl_node_is_cons(e));

  y = PSL_NULL;
  while (e != PSL_NULL) {
    PslNode_ptr z = psl_node_get_right(e);
    psl_node_set_right(e, y);
    y = e;
    e = z;
  }

  return y;
}

boolean psl_node_is_ite(PslNode_ptr _ite)
{
  if (_ite == PSL_NULL) return false;
  return (psl_node_get_op(_ite) == PSL_ITE) &&
    (psl_node_get_op(psl_node_get_left(_ite)) == COLON);
}

PslNode_ptr psl_node_get_ite_cond(PslNode_ptr _ite)
{
  nusmv_assert(psl_node_is_ite(_ite));
  return psl_node_get_left(psl_node_get_left(_ite));
}

PslNode_ptr psl_node_get_ite_then(PslNode_ptr _ite)
{
  nusmv_assert(psl_node_is_ite(_ite));
  return psl_node_get_right(psl_node_get_left(_ite));
}

PslNode_ptr psl_node_get_ite_else(PslNode_ptr _ite)
{
  nusmv_assert(psl_node_is_ite(_ite));
  return psl_node_get_right(_ite);
}

boolean psl_node_is_case(PslNode_ptr _case)
{
  if (_case == PSL_NULL) return false;
  if (psl_node_get_op(_case) != CASE) return false;
  if (psl_node_get_op(psl_node_get_left(_case)) == COLON) return true;
  else return false;
}

PslNode_ptr psl_node_get_case_cond(PslNode_ptr _case)
{
  nusmv_assert(psl_node_is_case(_case));
  return psl_node_get_left(psl_node_get_left(_case));
}

PslNode_ptr psl_node_get_case_then(PslNode_ptr _case)
{
  nusmv_assert(psl_node_is_case(_case));
  return psl_node_get_right(psl_node_get_left(_case));
}

PslNode_ptr psl_node_get_case_next(PslNode_ptr _case)
{
  nusmv_assert(psl_node_is_case(_case));
  return psl_node_get_right(_case);
}

boolean psl_node_is_range(PslNode_ptr expr)
{
  nusmv_assert(expr != PSL_NULL);
  if (psl_node_get_op(expr) == PSL_RANGE) return true;
  return false;
}

PslNode_ptr psl_node_range_get_low(PslNode_ptr expr)
{
  nusmv_assert(psl_node_is_range(expr) == true);
  return psl_node_get_left(expr);
}

PslNode_ptr psl_node_range_get_high(PslNode_ptr expr)
{
  nusmv_assert(psl_node_is_range(expr) == true);
  return psl_node_get_right(expr);
}

PslNode_ptr psl_node_make_number(NodeMgr_ptr nodemgr,
                                 int value)
{
  return psl_new_node(nodemgr, NUMBER, PSLNODE_FROM_INT(value), PSL_NULL);
}

PslNode_ptr psl_node_make_case(NodeMgr_ptr nodemgr,
                               PslNode_ptr _cond,
                               PslNode_ptr _then, PslNode_ptr _next)
{
  return psl_new_node(nodemgr, CASE,
                      psl_new_node(nodemgr, COLON, _cond, _then), _next) ;
}

PslNode_ptr psl_node_make_failure(NodeMgr_ptr nodemgr,
                                  const char* msg, FailureKind kind)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(nodemgr));
  UStringMgr_ptr strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));

  return psl_new_node(nodemgr, FAILURE,
                      psl_new_node(nodemgr, COLON,
                                   (PslNode_ptr) UStringMgr_find_string(strings, (char*) msg),
                                   (PslNode_ptr) kind),
                      (PslNode_ptr) 0 /* fake line number */);
}

boolean psl_node_is_number(PslNode_ptr e)
{
  nusmv_assert(e != PSL_NULL);
  return (psl_node_get_op(e) == NUMBER);
}

boolean psl_node_is_word_number(PslNode_ptr e)
{
  nusmv_assert(e != PSL_NULL);
  return (psl_node_get_op(e) == NUMBER_SIGNED_WORD ||
          psl_node_get_op(e) == NUMBER_UNSIGNED_WORD);
}

int psl_node_number_get_value(PslNode_ptr e)
{
  /* PslNode_ptr n; */
  nusmv_assert(psl_node_is_number(e));

  /* NOTE: Please notice that the below code has been commented. This
     is because the PSL node is already a number and should not need
     to be resolved (see the above assertions). However, if you found
     a bug and found that the source of it is here, give a try
     uncommenting the below commented code (Will need a
     SymbolTable) */

  /* n = PslNode_convert_from_node_ptr( */
  /*           CompileFlatten_resolve_number(Compile_get_global_symb_table(), */
  /*                         PslNode_convert_to_node_ptr(e), Nil)); */

  /* nusmv_assert(psl_node_get_op(n) == NUMBER); */

  return PSLNODE_TO_INT(psl_node_get_left(e));
}

PslNode_ptr psl_node_make_extended_next(NodeMgr_ptr nodemgr,
                                        PslOp op, PslNode_ptr expr,
                                        PslNode_ptr when,
                                        PslNode_ptr condition)
{
  return psl_new_node(nodemgr, op, expr, psl_new_node(nodemgr, COLON, when, condition));
}

boolean psl_node_is_extended_next(PslNode_ptr e)
{
  PslOp op = psl_node_get_op(e);

  return (op == PSL_X ||
          op == PSL_XBANG ||
          op == PSL_NEXT ||
          op == PSL_NEXTBANG ||
          op == PSL_NEXT_E ||
          op == PSL_NEXT_EBANG ||
          op == PSL_NEXT_A ||
          op == PSL_NEXT_ABANG ||
          op == PSL_NEXT_EVENT ||
          op == PSL_NEXT_EVENTBANG ||
          op == PSL_NEXT_EVENT_E ||
          op == PSL_NEXT_EVENT_EBANG ||
          op == PSL_NEXT_EVENT_A ||
          op == PSL_NEXT_EVENT_ABANG);
}

PslNode_ptr psl_node_extended_next_get_expr(PslNode_ptr next)
{
  nusmv_assert(psl_node_is_extended_next(next));

  return psl_node_get_left(next);
}

PslNode_ptr psl_node_extended_next_get_when(PslNode_ptr next)
{
  PslNode_ptr r;

  nusmv_assert(psl_node_is_extended_next(next));

  r = psl_node_get_right(next);
  if (r == PSL_NULL) return PSL_NULL;

  nusmv_assert(psl_node_get_op(r) == COLON);
  return psl_node_get_left(r);
}

PslNode_ptr psl_node_extended_next_get_condition(PslNode_ptr next)
{
  PslNode_ptr r;

  nusmv_assert(psl_node_is_extended_next(next));

  r = psl_node_get_right(next);
  if (r == PSL_NULL) return PSL_NULL;

  nusmv_assert(psl_node_get_op(r) == COLON);
  return psl_node_get_right(r);
}

boolean psl_node_is_boolean_type(PslNode_ptr expr)
{
  return (expr != PSL_NULL) && (psl_node_get_op(expr) == BOOLEAN);
}

boolean psl_node_is_infinite(PslNode_ptr expr)
{
  return (expr != PSL_NULL) && (psl_node_get_op(expr) == PSL_INF);
}

boolean psl_node_is_id(PslNode_ptr expr)
{
  nusmv_assert(expr != PSL_NULL);

  return (psl_node_get_op(expr) == DOT ||
          psl_node_get_op(expr) == ATOM ||
          psl_node_get_op(expr) == ARRAY);
}

boolean psl_node_is_id_equal(PslNode_ptr _id1, PslNode_ptr _id2)
{
  boolean res = false;
  nusmv_assert(psl_node_is_id(_id1) && psl_node_is_id(_id2));

  if (psl_node_get_op(_id1) != psl_node_get_op(_id2)) return false;
  switch (psl_node_get_op(_id1)) {
  case NUMBER:
    res = psl_node_is_num_equal(_id1, _id2);
    break;

  case ATOM:
    res = (psl_node_get_left(_id1) == psl_node_get_left(_id2));
    break;

  case ARRAY:
    res = psl_node_is_id_equal(psl_node_get_left(_id1), psl_node_get_left(_id2))
      &&
      psl_node_is_id_equal(psl_node_get_right(_id1), psl_node_get_right(_id2));
    break;

  case DOT:
    res = psl_node_is_id_equal(psl_node_get_left(_id1),
                               psl_node_get_left(_id2)) &&
      ((psl_node_get_right(_id1) == PSL_NULL &&
        psl_node_get_right(_id2) == PSL_NULL) ||
       psl_node_is_id_equal(psl_node_get_right(_id1), psl_node_get_right(_id2)));
    break;

  default:
    error_unreachable_code_msg("psl_node_is_id_equal: operator type not supported \"%d\"\n",
                               psl_node_get_op(_id1));
  }

  return res;
}

boolean psl_node_is_num_equal(PslNode_ptr _id1, PslNode_ptr _id2)
{
  nusmv_assert(psl_node_get_op(_id1) == NUMBER &&
               psl_node_get_op(_id2) == NUMBER);
  return psl_node_get_left(_id1) == psl_node_get_left(_id2);
}

boolean psl_node_is_leaf(PslNode_ptr expr)
{
  PslOp op;
  if (expr == PSL_NULL) return true;
  op = psl_node_get_op(expr);

  return (NUMBER == op ||
          NUMBER_FRAC == op || NUMBER_REAL == op || NUMBER_EXP == op ||
          ATOM == op ||
          TRUEEXP == op || FALSEEXP == op ||
          NUMBER_SIGNED_WORD == op || NUMBER_UNSIGNED_WORD == op ||
          FAILURE == op);
}

boolean psl_node_is_repl_prop(PslNode_ptr _prop)
{
  return (psl_node_get_op(_prop) == PSL_REPLPROP);
}

PslNode_ptr psl_node_repl_prop_get_replicator(PslNode_ptr _prop)
{
  nusmv_assert(psl_node_is_repl_prop(_prop));
  return psl_node_get_left(_prop);
}

PslNode_ptr psl_node_repl_prop_get_property(PslNode_ptr _prop)
{
  nusmv_assert(psl_node_is_repl_prop(_prop));
  return psl_node_get_right(_prop);
}

boolean psl_node_is_replicator(PslNode_ptr _repl)
{
  if (_repl == PSL_NULL) return false;
  if (psl_node_get_op(_repl) != PSL_FORALL &&
      psl_node_get_op(_repl) != PSL_FORANY) return false;
  if (psl_node_get_op(psl_node_get_right(_repl)) != SETIN) return false;
  return true;
}

PslNode_ptr psl_node_get_replicator_id(PslNode_ptr _repl)
{
  nusmv_assert(psl_node_is_replicator(_repl));
  return psl_node_get_left(_repl);
}

PslNode_ptr psl_node_get_replicator_range(PslNode_ptr _repl)
{
  nusmv_assert(psl_node_is_replicator(_repl));
  return psl_node_get_left(psl_node_get_right(_repl));
}

PslNode_ptr psl_node_get_replicator_value_set(PslNode_ptr _repl)
{
  nusmv_assert(psl_node_is_replicator(_repl));
  return psl_node_get_right(psl_node_get_right(_repl));
}

PslOp psl_node_get_replicator_join_op(PslNode_ptr _repl)
{
  nusmv_assert(psl_node_is_replicator(_repl));
  switch (psl_node_get_op(_repl)) {
  case PSL_FORALL: return AND;
  case PSL_FORANY: return OR;
  default: error_unreachable_code(); /* no other possible cases */
  }
  return -1;
}

PslNode_ptr psl_node_get_replicator_normalized_value_set(const NuSMVEnv_ptr env,
                                                         PslNode_ptr rep)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  PslNode_ptr range, vset;
  PslNode_ptr result;

  nusmv_assert(psl_node_is_replicator(rep));

  range = psl_node_get_replicator_range(rep);
  vset = psl_node_get_replicator_value_set(rep);

  if (range != PSL_NULL) {
    StreamMgr_print_error(streams,  "psl_node_get_replicator_normalized_value_set: " \
            "Replicator range not yet handled.\n");
    ErrorMgr_error_psl_not_supported_feature(errmgr);
  }

  result = PSL_NULL;

  if (psl_node_is_boolean_type(vset) == true) {
    result = psl_node_make_cons_new(nodemgr, psl_node_make_false(nodemgr),
                                    psl_node_make_cons_new(nodemgr, psl_node_make_true(nodemgr),
                                                           result));
  }
  else {
    nusmv_assert(psl_node_is_cons(vset));

    for (; vset != PSL_NULL; vset = psl_node_cons_get_next(vset)) {
      PslNode_ptr v = psl_node_cons_get_element(vset);

      switch (psl_node_get_op(v)) {
      case PSL_RANGE:
        {
          int i, inf, sup;

          if (!psl_node_is_number(psl_node_range_get_low(v))) {
            ErrorMgr_error_psl_not_supported_feature_next_number(errmgr);
          }
          inf = psl_node_number_get_value(psl_node_range_get_low(v));

          if (!psl_node_is_number(psl_node_range_get_high(v))) {
            ErrorMgr_error_psl_not_supported_feature_next_number(errmgr);
          }
          sup = psl_node_number_get_value(psl_node_range_get_high(v));

          if (inf > sup) {
            StreamMgr_print_error(streams,  "Error in: ");
            StreamMgr_nprint_error(streams, wffprint, "%N", v);
            StreamMgr_print_error(streams,  "\n");

            ErrorMgr_error_invalid_numeric_value(errmgr, sup,
                        "Range with high bound greater than low bound.");
          }

          for (i = inf; i <= sup; i++) {
            result = psl_node_make_cons_new(nodemgr, psl_node_make_number(nodemgr, i), result);
          }
          break;
        }
      case BOOLEAN:
        result = psl_node_make_cons_new(nodemgr, psl_node_make_false(nodemgr),
                                    psl_node_make_cons_new(nodemgr, psl_node_make_true(nodemgr),
                                                       result));
        break;
      case NUMBER:
        result = psl_node_make_cons_new(nodemgr, v, result);
        break;

      case ATOM:
      case DOT:
        result = psl_node_make_cons_new(nodemgr, v, result);
        break;

      default:
        StreamMgr_print_error(streams, 
                "psl_node_get_replicator_normalized_value_set: expression not " \
                "supported \"%d\"\n",
                psl_node_get_op(v));
        error_unreachable_code();
        break;
      }
    }
        result = psl_node_cons_reverse(result);  /* We must reverse the list */
  }

  return result;
}

PslNode_ptr psl_node_context_to_main_context(NodeMgr_ptr nodemgr,
                                             PslNode_ptr context)
{
  /* assumes psl_new_node invoke find_node */

  if (context == PSL_NULL) return PSL_NULL;

  switch (psl_node_get_op(context)) {
  case ATOM:
    return psl_new_node(nodemgr, DOT, PSL_NULL, context);

  case DOT:
    return psl_new_node(nodemgr, DOT,
                        psl_node_context_to_main_context(nodemgr, psl_node_get_left(context)),
                        psl_node_get_right(context));

  case ARRAY:
    return psl_new_node(nodemgr, ARRAY,
                        psl_node_context_to_main_context(nodemgr, psl_node_get_left(context)),
                        psl_node_get_right(context));

  default:
    error_unreachable_code_msg("psl_node_context_to_mainc_ontext: undefined token \"%d\"\n",
                               psl_node_get_op(context));
  }
}



/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief 

  To compare structures like {{a}} and {{{{a}}}} and check
   whether the innermost {a}'s are actually the same node pointer

  \se None
*/
static boolean psl_node_is_equal(PslNode_ptr e, PslNode_ptr f)
{
  if (e==f) return true;

  if (e==PSL_NULL || f==PSL_NULL) return false;

  if (PslNode_is_propositional(e) && PslNode_is_propositional(f))
    return false;

  if ((psl_node_get_op(e)==PSL_SERE) &&
      !PslNode_is_propositional(psl_node_get_left(e))) {
    return psl_node_is_equal(psl_node_get_left(e), f);
  }

  if ((psl_node_get_op(f)==PSL_SERE) &&
      !PslNode_is_propositional(psl_node_get_left(f))) {
    return psl_node_is_equal(e, psl_node_get_left(f));
  }

  return false;
}

/*!
  \brief Returns true if the given sere is star-free

  

  \se None
*/
static boolean psl_node_is_star_free(PslNode_ptr expr)
{
  if (expr == PSL_NULL) return true;

  /* leaf? */
  if (psl_node_is_leaf(expr) || psl_node_is_id(expr)) {
    return true;
  }
  if (psl_node_sere_is_repeated(expr)) return false;

  return psl_node_is_star_free(psl_node_get_left(expr)) &&
    psl_node_is_star_free(psl_node_get_right(expr));
}

/*!
  \brief Returns true if the given sere doesn't contain any unbound
                    star

  

  \se None
*/
static boolean psl_node_is_unbound_star_free(const NuSMVEnv_ptr env,
                                             PslNode_ptr expr)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  if (expr == PSL_NULL) return true;

  /* leaf? */
  if (psl_node_is_leaf(expr) || psl_node_is_id(expr)) return true;

  /* repeated sere? */
  if (psl_node_sere_is_repeated(expr)) {
    if (psl_node_sere_is_star_count(expr)) {
      /* only star-count with a number as counter are accepted */
      PslNode_ptr count = psl_node_sere_star_get_count(expr);
      if (!psl_node_is_number(count)) {
        StreamMgr_print_error(streams,  "In expression ");
        StreamMgr_nprint_error(streams, wffprint, "%N", expr);
        ErrorMgr_error_expected_number(errmgr);
      }
    }
    else return false;
  }

  return psl_node_is_unbound_star_free(env, psl_node_get_left(expr)) &&
    psl_node_is_unbound_star_free(env, psl_node_get_right(expr));
}

/*!
  \brief Returns true if the given expression is empty star-free

  

  \se None
*/
static boolean psl_node_is_emptystar_free(PslNode_ptr expr)
{
  PslOp op;

  if (expr == PSL_NULL) return true;

  op = psl_node_get_op(expr);

  /* leaf? */
  if (psl_node_is_leaf(expr) || psl_node_is_id(expr)) return true;

  if (psl_node_sere_is_plus(expr)) return true;

  if (psl_node_sere_is_star_count(expr)) return true;

  /* here expr is not a star count */
  if (psl_node_sere_is_star(expr)) return false;

  if (op==PSL_SERECONCAT) {
    return psl_node_is_emptystar_free(psl_node_get_left(expr)) ||
      psl_node_is_emptystar_free(psl_node_get_right(expr));
  };

  return psl_node_is_emptystar_free(psl_node_get_left(expr)) &&
    psl_node_is_emptystar_free(psl_node_get_right(expr));
}

/*!
  \brief Private service of PslNode_is_handled_psl

  

  \se None

  \sa PslNode_is_handled_psl
*/
static boolean psl_node_is_handled_sere(const NuSMVEnv_ptr env,
                                        PslNode_ptr e, boolean toplevel)
{
  PslOp op;

  if (e == PSL_NULL) return false;
  if (PslNode_is_propositional(e)) return true;

  op = psl_node_get_op(e);

  if (op==PSL_SERE) {
    return psl_node_is_handled_sere(env, psl_node_get_left(e), toplevel);
  }

  if (op==PSL_SERECONCAT) {

    if (toplevel) {
      return (psl_node_is_emptystar_free(psl_node_get_left(e)) ||
              psl_node_is_emptystar_free(psl_node_get_right(e))) &&
        psl_node_is_handled_sere(env, psl_node_get_left(e), false) &&
        psl_node_is_handled_sere(env, psl_node_get_right(e), false);
    }

    return psl_node_is_handled_sere(env, psl_node_get_left(e), false) &&
      psl_node_is_handled_sere(env, psl_node_get_right(e), false);
  }

  if (op==PSL_SEREFUSION) {
    return psl_node_is_handled_sere(env, psl_node_get_left(e), false) &&
      psl_node_is_handled_sere(env, psl_node_get_right(e), false) &&
      psl_node_is_emptystar_free(psl_node_get_left(e)) &&
      psl_node_is_emptystar_free(psl_node_get_right(e));
  }

  if (op==PSL_SERECOMPOUND) {
    op = psl_node_get_op(psl_node_get_left(e));

    if (op==AND) {
      /* we rule out starts in arguments of & because: {a;b[*]} &
        {c;d;e} would be translated into {a AND c};{bU(b AND e) AND d}
        forcing the sequence of bs to end with an e which belongs to
        {c;d;e} */
      return psl_node_is_star_free(psl_node_get_left(psl_node_get_left(e))) &&
        psl_node_is_star_free(psl_node_get_right(psl_node_get_left(e))) &&
        psl_node_is_handled_sere(env, psl_node_get_left(psl_node_get_left(e)), toplevel) &&
        psl_node_is_handled_sere(env, psl_node_get_right(psl_node_get_left(e)), toplevel);
    }

    if (op==PSL_AMPERSANDAMPERSAND) {
      return psl_node_is_star_free(psl_node_get_left(psl_node_get_left(e))) &&
        psl_node_is_star_free(psl_node_get_right(psl_node_get_left(e))) &&
        psl_node_is_handled_sere(env, psl_node_get_left(psl_node_get_left(e)), toplevel) &&
        psl_node_is_handled_sere(env, psl_node_get_right(psl_node_get_left(e)), toplevel);
    }

    if (op==OR) {

      if (toplevel) {
        return psl_node_is_emptystar_free(psl_node_get_left(psl_node_get_left(e))) &&
          psl_node_is_emptystar_free(psl_node_get_right(psl_node_get_left(e))) &&
          psl_node_is_handled_sere(env, psl_node_get_left(psl_node_get_left(e)), toplevel) &&
          psl_node_is_handled_sere(env, psl_node_get_right(psl_node_get_left(e)), toplevel);
      }

      return psl_node_is_handled_sere(env, psl_node_get_left(psl_node_get_left(e)), toplevel) &&
        psl_node_is_handled_sere(env, psl_node_get_right(psl_node_get_left(e)), toplevel);

    }

    return psl_node_is_emptystar_free(psl_node_get_left(psl_node_get_left(e))) &&
      psl_node_is_emptystar_free(psl_node_get_right(psl_node_get_left(e))) &&
      psl_node_is_handled_sere(env, psl_node_get_left(psl_node_get_left(e)), toplevel) &&
      psl_node_is_handled_sere(env, psl_node_get_right(psl_node_get_left(e)), toplevel);
  }

  if (op==PSL_SEREREPEATED) return psl_node_is_handled_star(env, e, toplevel);

  return false;
}

/*!
  \brief Private service of PslNode_is_handled_psl

  

  \se None

  \sa PslNode_is_handled_psl
*/
static boolean psl_node_is_handled_next(const NuSMVEnv_ptr env, PslNode_ptr next)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  PslNode_ptr expr;
  PslNode_ptr condition;
  PslNode_ptr when;

  nusmv_assert(psl_node_is_extended_next(next));

  /* gets the components of the next-expression */
  expr = psl_node_extended_next_get_expr(next);
  condition = psl_node_extended_next_get_condition(next);
  when = psl_node_extended_next_get_when(next);

  /* check that the expression is handled psl
     expr cannot be NULL */
  if ((expr == PSL_NULL) || !PslNode_is_handled_psl(env, expr)) {
    return false;
  }

  /* check that the boolean condition is handled psl
     condition may be NULL */
  if ((condition != PSL_NULL) && !PslNode_is_propositional(condition)) {
    return false;
  }

  /* check that the "when" part of the expression is numeric when may
     be NULL */

  /* Developers' note: this section is delayed until convertion is
     performed, because later forall might resolve ranges and counters
     in 'when' to be pure numbers. */
  if (when != PSL_NULL) {
    if (!psl_node_is_number(when) &&
        (psl_node_is_range(when) &&
         (!psl_node_is_number(psl_node_range_get_low(when)) ||
          !psl_node_is_number(psl_node_range_get_high(when))))) {
      StreamMgr_print_error(streams,  "In expression ");
      StreamMgr_nprint_error(streams, wffprint, "%N", when);
      ErrorMgr_error_expected_number(errmgr);
    }
  }

  /* here we have: "expr" != NULL and handled, and "when" and
     "condition" NULL or handled */
  return true;
}

/*!
  \brief Private service of PslNode_is_handled_psl

  

  \se None

  \sa PslNode_is_handled_psl
*/
static boolean psl_node_is_handled_fl_op(PslOp op)
{
  return (op != PSL_WHILENOTBANG && op != PSL_WHILENOT &&
          op != PSL_WHILENOTBANG_ && op != PSL_WHILENOT_ &&

          op != PSL_WITHINBANG && op != PSL_WITHIN &&
          op != PSL_WITHINBANG_ && op != PSL_WITHIN_ &&

          op != PSL_ABORT);
}

/*!
  \brief Private service of PslNode_is_handled_psl

  

  \se None

  \sa PslNode_is_handled_psl
*/
static boolean psl_node_is_fl_op(PslOp op)
{
  return (op == OP_NEXT ||
          op == PSL_X ||
          op == PSL_XBANG ||
          op == PSL_NEXT ||
          op == PSL_NEXTBANG ||
          op == PSL_NEXT_E ||
          op == PSL_NEXT_EBANG ||
          op == PSL_NEXT_A ||
          op == PSL_NEXT_ABANG ||
          op == PSL_NEXT_EVENT ||
          op == PSL_NEXT_EVENTBANG ||
          op == PSL_NEXT_EVENT_E ||
          op == PSL_NEXT_EVENT_EBANG ||
          op == PSL_NEXT_EVENT_A ||
          op == PSL_NEXT_EVENT_ABANG ||
          op == OP_FUTURE ||
          op == PSL_NEVER ||
          op == OP_GLOBAL ||
          op == PSL_ALWAYS ||
          op == PSL_EVENTUALLYBANG ||
          op == PSL_W ||
          op == PSL_UNTIL ||
          op == UNTIL ||
          op == PSL_UNTILBANG ||
          op == PSL_UNTIL_ ||
          op == PSL_UNTILBANG_ ||
          op == PSL_BEFORE ||
          op == PSL_BEFOREBANG ||
          op == PSL_BEFORE_ ||
          op == PSL_BEFOREBANG_ ||
          op == PSL_WHILENOTBANG ||
          op == PSL_WHILENOT ||
          op == PSL_WHILENOTBANG_ ||
          op == PSL_WHILENOT_ ||
          op == PSL_ABORT);
}

/*!
  \brief Private service of PslNode_is_handled_psl

  

  \se None

  \sa PslNode_is_handled_psl
*/
static boolean psl_node_is_obe_op(PslOp op)
{
  return (op == AX ||
          op == EX ||
          op == AG ||
          op == EG ||
          op == AF ||
          op == EF ||
          op == EU ||
          op == AU);
}

/*!
  \brief Checks for a formula being a propositional formula

  Checks for a formula being a propositional formula

  \se None
*/
static boolean psl_node_is_propositional(const PslNode_ptr expr,
                                         boolean accept_next)
{
  boolean result = false;

  if (expr == PSL_NULL) return true;

  if (psl_node_is_leaf(expr) || psl_node_is_id(expr)) return true;

  switch (psl_node_get_op(expr)) {

  case CONTEXT:
    result = PslNode_is_propositional(psl_node_get_right(expr));
    break;

    /* primary */
  case MINUS:
  case PLUS:

    /* binary operators */
  case UNION:
  case SETIN:
  case TIMES:
  case DIVIDE:
  case MOD:
  case PSL_EQEQ:
  case EQUAL:
  case NOTEQUAL:
  case LT:
  case LE:
  case GT:
  case GE:
    result = true;
    break;

  case NOT:
  case PSL_TILDE:
    result = PslNode_is_propositional(psl_node_get_left(expr));
    break;

    /* boolean binary operators */
  case AND:
  case OR:
  case PSL_OR:
  case XOR:
  case PSL_CARET:
  case IFF:
  case IMPLIES:
    result = PslNode_is_propositional(psl_node_get_left(expr)) &&
      PslNode_is_propositional(psl_node_get_right(expr));
    break;

  case CASE:
    result = PslNode_is_propositional(psl_node_get_case_cond(expr)) &&
      PslNode_is_propositional(psl_node_get_case_then(expr)) &&
      PslNode_is_propositional(psl_node_get_case_next(expr));
    break;

  case PSL_ITE:
    result = PslNode_is_propositional(psl_node_get_ite_cond(expr)) &&
      PslNode_is_propositional(psl_node_get_ite_then(expr)) &&
      PslNode_is_propositional(psl_node_get_ite_else(expr));
    break;

  case PSL_REPLPROP:
    result = PslNode_is_propositional(psl_node_repl_prop_get_property(expr));
    break;

  case NEXT:
    result = accept_next;
    break;

  default:
    result = false;
  }

  return result;
}
