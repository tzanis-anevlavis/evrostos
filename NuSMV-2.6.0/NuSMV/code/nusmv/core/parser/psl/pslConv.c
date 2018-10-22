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
  \author Fabio Barbon, Roberto Cavada, Simone Semprini
  \brief Algorithms and conversions on PslNode structure

  \todo: Missing description

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/parser/psl/pslNode.h"
#include "nusmv/core/parser/psl/pslExpr.h"
#include "nusmv/core/parser/psl/pslInt.h"
#include "nusmv/core/parser/psl/psl_symbols.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/NodeList.h"
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

/*!
  \brief Define to optimize the convertion of next

  
*/
#define PSL_CONV_DISTRIB_NEXT

/*!
  \brief Enable for debugging of fix point

  
*/
#define PSL_VERBOSE_TRANSLATE 0

/*!
  \brief This was implemented for the sake of readability

  This is used by the function that converts the operators
*/
#define PSL_OP_CONV3(err, tok, psl, smv)                                 \
   if (op == ((type == TOK2PSL)? tok :                                   \
              (type == TOK2SMV)? tok :                                   \
              (type == PSL2PSL)? psl : psl)) {                           \
      switch (type) {                                                    \
      case PSL2PSL: return psl;                                          \
      case TOK2PSL: return psl;                                          \
      case TOK2SMV:                                                      \
         if (smv == -1) ErrorMgr_internal_error(err, "PSL_OP_CONV: unknown token.\n"); \
         return smv;                                                     \
      case PSL2TOK: return tok;                                          \
      case PSL2SMV: return smv;                                          \
      default:                                                           \
        ErrorMgr_internal_error(err, "PSL_OP_CONV: invalid conversion type.\n");       \
      }                                                                  \
   }

/*!
  \brief This was implemented for the sake of readability

  This is used by the function that converts the operators, as
  a shortcut for PSL_OP_CONV3(tok, X, X) 
*/
#define PSL_OP_CONV2(err, tok, sym)     PSL_OP_CONV3(err, tok, sym, sym)




/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static PslNode_ptr
psl_node_pslobe2ctl(const NuSMVEnv_ptr env,
                    PslNode_ptr expr, PslOpConvType type,
                    NodeList_ptr replicator_id_stack);

static PslNode_ptr
psl_node_pslltl2ltl(const NuSMVEnv_ptr env,
                    PslNode_ptr expr, PslOpConvType type,
                    NodeList_ptr replicator_id_stack);

static PslNode_ptr
psl_node_expand_next_event(const NuSMVEnv_ptr env,
                           PslOp op, PslNode_ptr f, PslNode_ptr b,
                           PslOpConvType type);

static PslNode_ptr
psl_node_subst_id(const NuSMVEnv_ptr env,
                  PslNode_ptr expr, PslNode_ptr id, PslNode_ptr v,
                  boolean is_top_level);

static PslNode_ptr
psl_node_expand_replicator(const NuSMVEnv_ptr env,
                           PslNode_ptr rep, PslNode_ptr wff,
                           PslOp op);

static PslNode_ptr psl_node_sere_remove_disj(const NuSMVEnv_ptr env,
                                             PslNode_ptr e);

static PslNode_ptr
psl_node_insert_inside_holes(const NuSMVEnv_ptr env,
                             PslNode_ptr e, PslNode_ptr to_be_inserted,
                             boolean* inserted);

static PslNode_ptr
psl_node_sere_concat_fusion2ltl(const NuSMVEnv_ptr env,
                                PslNode_ptr e, PslNode_ptr phi);

static PslNode_ptr psl_node_sere_translate(const NuSMVEnv_ptr env,
                                           PslNode_ptr e);

static boolean psl_node_sere_is_disj(PslNode_ptr e);

static PslNode_ptr
psl_node_sere_distrib_disj(const NuSMVEnv_ptr env,
                           PslNode_ptr e, boolean *modified);

static PslNode_ptr psl_node_sere_remove_star_count(const NuSMVEnv_ptr env,
                                                   PslNode_ptr e);

static PslNode_ptr
psl_node_sere_remove_trailing_star(const NuSMVEnv_ptr env,
                                   PslNode_ptr e, boolean* modified);

static boolean psl_node_sere_is_ampersand(PslNode_ptr e);
static PslNode_ptr psl_node_sere_get_leftmost(PslNode_ptr e);
static PslNode_ptr psl_node_sere_get_rightmost(PslNode_ptr e);

static PslNode_ptr
psl_node_sere_remove_plus(const NuSMVEnv_ptr env,
                          PslNode_ptr e, boolean toplevel);

static PslNode_ptr psl_node_sere_remove_trailing_plus(const NuSMVEnv_ptr env,
                                                      PslNode_ptr e);
static PslNode_ptr psl_node_remove_suffix_implication(const NuSMVEnv_ptr env,
                                                      PslNode_ptr e);

static PslNode_ptr
psl_node_sere_remove_star(const NuSMVEnv_ptr env, PslNode_ptr e, boolean toplevel,
                          boolean* modified);

static PslNode_ptr
psl_node_sere_remove_ampersand(const NuSMVEnv_ptr env, PslNode_ptr e, boolean* modified);

static PslNode_ptr
psl_node_sere_remove_2ampersand(const NuSMVEnv_ptr env,
                                PslNode_ptr e, boolean *modified);

static PslNode_ptr
psl_node_sere_remove_fusion(const NuSMVEnv_ptr env, PslNode_ptr e, boolean *modified);

static PslNode_ptr
psl_node_remove_forall_replicators(const NuSMVEnv_ptr env,
                                   PslNode_ptr expr,
                                   NodeList_ptr replicator_id_stack);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

node_ptr PslNode_convert_psl_to_core(const NuSMVEnv_ptr env, PslNode_ptr expr)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  PslNode_ptr res;

  /* removal of forall */
  res = PslNode_remove_forall_replicators(env, expr);

  if (PslNode_is_handled_psl(env, res)) {
    /* either smooth LTL or SERE: */
    if (!PslNode_is_ltl(res)) {
      /* here it is a SERE: must be converted to LTL */
      res = PslNode_remove_sere(env, res);
    }

    /* converts to SMV ltl */
    res = PslNode_pslltl2ltl(env, res, PSL2SMV);
  }
  else {
    /* here the property may be either OBE or unmanageable */
    if (PslNode_is_obe(res)) {
      res = PslNode_pslobe2ctl(env, res, PSL2SMV);
    }
    else ErrorMgr_error_psl_not_supported_feature(errmgr);
  }

  return PslNode_convert_to_node_ptr(res);
}




/*!
  \brief Converts an id to a different id type, for example a PSL id
to a SMV id

  

  \se None
*/

PslNode_ptr PslNode_convert_id(const NuSMVEnv_ptr env, PslNode_ptr id, PslOpConvType type)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  PslNode_ptr result;
  PslOp op, op_psl;

  if (id == PSL_NULL) return PSL_NULL;

  op = psl_node_get_op(id);

  /* Cases are limited to normalized parse-tree compatible node, so if token is
     coming, it is converted at first */
  if ((type == TOK2PSL) || (type == TOK2SMV)) op_psl = psl_conv_op(env, type, op);
  else op_psl = op;

  switch (op_psl) {
    /* leaves: */
  case ATOM:
  case NUMBER:
    result = psl_new_node(nodemgr, psl_conv_op(env, type, op),
                          psl_node_get_left(id),
                          psl_node_get_right(id));
    break;

  case ARRAY:
  case DOT:
    result = psl_new_node(nodemgr, psl_conv_op(env, type, op),
                          PslNode_convert_id(env, psl_node_get_left(id), type),
                          PslNode_convert_id(env, psl_node_get_right(id), type));
    break;

  default:
    StreamMgr_print_error(streams, 
                          "PslNode_convert_id: operator type not supported \"%d\"\n",
                          op_psl);
    ErrorMgr_internal_error(errmgr, "Invalid op");
    result = (PslNode_ptr) NULL;
  }

  return result;
}


/*!
  \brief Takes a PSL OBE expression and builds the corresponding
CTL expression 

  Takes a PSL OBE expression and builds the corresponding
CTL expression.

  \se required

  \sa optional
*/

PslNode_ptr PslNode_pslobe2ctl(const NuSMVEnv_ptr env,
                               PslNode_ptr expr, PslOpConvType type)
{
  NodeList_ptr repl_stack = NodeList_create();
  PslNode_ptr res = psl_node_pslobe2ctl(env, expr, type, repl_stack);

  NodeList_destroy(repl_stack);
  return res;
}

/*!
  \brief Private service for high level function PslNode_pslobe2ctl

  Private service for high level function PslNode_pslobe2ctl

  \se required

  \sa PslNode_pslobe2ctl
*/
static PslNode_ptr psl_node_pslobe2ctl(const NuSMVEnv_ptr env,
                                       PslNode_ptr expr, PslOpConvType type,
                                       NodeList_ptr replicator_id_stack)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  PslNode_ptr result;
  PslOp op;

  if (expr == PSL_NULL) return PSL_NULL;

  op = psl_node_get_op(expr);

  if (psl_node_is_leaf(expr)) {
    return psl_new_node(nodemgr, op, psl_node_get_left(expr), psl_node_get_right(expr));
  }

  switch (op) {
  case CONTEXT:
  case CASE:
  case COLON:

    /* id */
  case ARRAY:
  case DOT:

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

  case NOT:
  case PSL_TILDE:

  case AND:
  case OR:
  case PSL_OR:
  case XOR:
  case PSL_CARET:
  case IFF:
  case IMPLIES:

    /* next operators */
  case AX:
  case EX:
    /* globally operators */
  case AG:
  case EG:
    /* eventually operators */
  case AF:
  case EF:
    /* AU, EU */
  case EU:
  case AU:
    result = psl_new_node(nodemgr, op,
                          psl_node_pslobe2ctl(env, psl_node_get_left(expr), type,
                                           replicator_id_stack),
                          psl_node_pslobe2ctl(env, psl_node_get_right(expr), type,
                                           replicator_id_stack));
    break;

  case PSL_ITE:
    {
      PslNode_ptr _cond = psl_node_get_ite_cond(expr);
      PslNode_ptr _then = psl_node_get_ite_then(expr);
      PslNode_ptr _else = psl_node_get_ite_else(expr);

      PslNode_ptr case_else =
        psl_node_make_case(nodemgr, psl_node_make_true(nodemgr),
                           _else,
                           psl_node_make_failure(nodemgr, "Impossible failure",
                                                 FAILURE_UNSPECIFIED));

      result = psl_node_make_case(nodemgr, _cond, _then, case_else);
      break;
    }

  case PSL_REPLPROP:
    {
      PslNode_ptr rep = psl_node_repl_prop_get_replicator(expr);
      PslNode_ptr wff = psl_node_repl_prop_get_property(expr);
      PslNode_ptr id = psl_node_get_replicator_id(rep);
      PslOp rop = psl_node_get_replicator_join_op(rep);

      /* checks if the forall ID has been already used by an outer forall */
      if (NodeList_belongs_to(replicator_id_stack,
                              PslNode_convert_to_node_ptr(id))) {
        ErrorMgr_error_psl_repeated_replicator_id(errmgr);
      }
      NodeList_prepend(replicator_id_stack, PslNode_convert_to_node_ptr(id));

      result = psl_node_expand_replicator(env, rep, wff, rop);
      result = psl_node_pslobe2ctl(env, result, type, replicator_id_stack);

      /* finally pops the forall ID from the stack: */
      {
        node_ptr el = NodeList_remove_elem_at(replicator_id_stack,
                                NodeList_get_first_iter(replicator_id_stack));
        free_node(nodemgr, el);
      }
      break;
    }

  default:
    StreamMgr_print_error(streams, 
            "psl_node_pslobe2ctl: operator type not supported \"%d\"\n", op);
    error_unreachable_code();
  }

  return result;
}

/*!
  \brief Takes a PSL expression and expands all forall constructs
contained in the expression

  Takes a PSL expression and expands all forall constructs
contained in the expression. Visits the syntax tree of the expressions
and whenever it finds a forall construct it expands the expression in
its scope.

  \se required

  \sa optional
*/

PslNode_ptr PslNode_remove_forall_replicators(const NuSMVEnv_ptr env,
                                              PslNode_ptr expr)
{
  NodeList_ptr repl_stack = NodeList_create();
  PslNode_ptr res = psl_node_remove_forall_replicators(env, expr, repl_stack);

  NodeList_destroy(repl_stack);
  return res;
}

/*!
  \brief Private service for high level function
          PslNode_remove_forall_replicators

  Private service for high level function
             PslNode_remove_forall_replicators. In removing nested
             forall it takes into accaount possible clashes on the
             names of the bounded variables.

  \se required

  \sa PslNode_remove_forall_replicators
*/
static PslNode_ptr
psl_node_remove_forall_replicators(const NuSMVEnv_ptr env,
                                   PslNode_ptr expr,
                                   NodeList_ptr replicator_id_stack)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  PslNode_ptr result;
  PslOp op;

  if (expr == PSL_NULL) return PSL_NULL;
  if (psl_node_is_leaf(expr)) return expr;

  op = psl_node_get_op(expr);

  switch (op) {
  case PSL_REPLPROP:
    {
      PslNode_ptr rep = psl_node_repl_prop_get_replicator(expr);
      PslNode_ptr wff = psl_node_repl_prop_get_property(expr);
      PslNode_ptr id = psl_node_get_replicator_id(rep);
      PslOp rop = psl_node_get_replicator_join_op(rep);

      /* checks if the forall ID has been already used by an outer forall */
      if (NodeList_belongs_to(replicator_id_stack,
                              PslNode_convert_to_node_ptr(id))) {
        ErrorMgr_error_psl_repeated_replicator_id(errmgr);
      }
      NodeList_prepend(replicator_id_stack, PslNode_convert_to_node_ptr(id));

      result = psl_node_expand_replicator(env, rep, wff, rop);
      result = psl_node_remove_forall_replicators(env, result, replicator_id_stack);

      /* finally pops the forall ID from the stack: */
      {
        node_ptr el = NodeList_remove_elem_at(replicator_id_stack,
                              NodeList_get_first_iter(replicator_id_stack));
        free_node(nodemgr, el);
      }
      break;
    }
  default:
    result = psl_new_node(nodemgr, psl_node_get_op(expr),
                          psl_node_remove_forall_replicators(env, psl_node_get_left(expr),
                                                             replicator_id_stack),
                          psl_node_remove_forall_replicators(env, psl_node_get_right(expr),
                                                             replicator_id_stack));
    break;
  }
  return result;
}

/*!
  \brief Takes a PSL LTL expression and builds the
corresponding LTL expression 

  Takes a PSL LTL expression and builds the corresponding
LTL expression. This ignores SERE that can be easily mapped to the
corresponding LTL expression.  The parameter replicator_id_stack is
used to prevent ID duplication of nested forall (replicators).

  \se required

  \sa optional
*/

PslNode_ptr PslNode_pslltl2ltl(const NuSMVEnv_ptr env, PslNode_ptr expr, PslOpConvType type)
{
  NodeList_ptr repl_stack = NodeList_create();
  PslNode_ptr res = psl_node_pslltl2ltl(env, expr, type, repl_stack);

  NodeList_destroy(repl_stack);
  return res;
}

/*!
  \brief Takes a PSL LTL expression and builds the
corresponding LTL expression 

  Takes a PSL LTL expression and builds the corresponding
LTL expression. This ignores SERE that can be easily mapped to the
corresponding LTL expression.  The parameter replicator_id_stack is
used to prevent ID duplication of nested forall (replicators).
type can be PSL2SMV or PSL2PSL

  \se required

  \sa optional
*/
static PslNode_ptr psl_node_pslltl2ltl(const NuSMVEnv_ptr env,
                                       PslNode_ptr expr, PslOpConvType type,
                                       NodeList_ptr replicator_id_stack)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  PslNode_ptr result;
  PslOp op;

  if (expr == PSL_NULL) return PSL_NULL;

  op = psl_node_get_op(expr);
  if (psl_node_is_leaf(expr)) {
    return psl_new_node(nodemgr, op, psl_node_get_left(expr), psl_node_get_right(expr));
  }

  switch (op) {
  case CONTEXT:
  case CASE:
  case COLON:

    /* id */
  case ARRAY:
  case DOT:

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

  case NOT:
  case PSL_TILDE:

  case AND:
  case OR:
  case PSL_OR:
  case XOR:
  case PSL_CARET:
  case IFF:
  case IMPLIES:

    /* Word operations */
  case UWCONST:
  case SWCONST:
  case XNOR:
  case BIT_SELECTION:
  case LSHIFT:
  case RSHIFT:
  case CONCATENATION:
  case EXTEND:
  case WRESIZE:
  case WSIZEOF:
  case CAST_TOINT:
  case CAST_WORD1:
  case CAST_BOOL:
  case CAST_SIGNED:
  case CAST_UNSIGNED:
  case UMINUS:

    /* unary temporal ops */
  case PSL_EVENTUALLYBANG:
  case OP_FUTURE:
  case PSL_ALWAYS:
  case OP_GLOBAL:

    /* binary temporal ops */
  case UNTIL:
    result = psl_new_node(nodemgr, psl_conv_op(env, type, op),
                          psl_node_pslltl2ltl(env, psl_node_get_left(expr), type,
                                              replicator_id_stack),
                          psl_node_pslltl2ltl(env, psl_node_get_right(expr), type,
                                              replicator_id_stack));
    break;

  case PSL_ITE:
    {
      PslNode_ptr _cond = psl_node_get_ite_cond(expr);
      PslNode_ptr _then = psl_node_get_ite_then(expr);
      PslNode_ptr _else = psl_node_get_ite_else(expr);

      PslNode_ptr case_else =
        psl_node_make_case(nodemgr, psl_node_make_true(nodemgr),
                           _else,
                           psl_node_make_failure(nodemgr, "Impossible failure",
                                                 FAILURE_UNSPECIFIED));

      result = psl_node_make_case(nodemgr, _cond, _then, case_else);
      break;
    }

    /* next* operators */
  case OP_NEXT:
  case PSL_X:
  case PSL_XBANG:
  case PSL_NEXT:
  case PSL_NEXTBANG:
    {
      PslNode_ptr l = psl_node_pslltl2ltl(env, psl_node_get_left(expr), type,
                                          replicator_id_stack);
      PslNode_ptr r = psl_node_get_right(expr);

      /* Developer's note: we might think to use specific top level
         ops for extended next operators, instead of reusing NEXT and X */
      if (r != PSL_NULL) {
        /* Extended next (event) operator */
        int lim;
        PslNode_ptr lim_expr;

        nusmv_assert(psl_node_get_op(r) == COLON);
        nusmv_assert(psl_node_get_right(r) == PSL_NULL);

        lim_expr = psl_node_get_left(r);
        if (!psl_node_is_number(lim_expr)) {
          StreamMgr_print_error(streams,  "In PSL expression '");
          StreamMgr_nprint_error(streams, wffprint, "%N", expr);
          StreamMgr_print_error(streams,  "'\n");
          ErrorMgr_error_psl_not_supported_feature_next_number(errmgr);
        }
        lim = psl_node_number_get_value(lim_expr);

        /* inf must be greater or equal to 0 */
        nusmv_assert(lim >=0 );
        for (; lim > 0; --lim) {
          l = psl_new_node(nodemgr, psl_conv_op(env, type, op), l, PSL_NULL);
        }
      }
      else {
        /* when is not specified */
        l = psl_new_node(nodemgr, psl_conv_op(env, type, op), l, PSL_NULL);
      }

      /* At the moment we do not distinguish among weak and strong next */
      result = l;
    }
    break;


  case PSL_WSELECT:
    {
      PslNode_ptr l = psl_node_get_left(expr);
      PslNode_ptr r = psl_node_get_right(expr);

      result = psl_new_node(nodemgr, BIT_SELECTION, l, r);
      break;
    }

  case PSL_NEXT_E:
  case PSL_NEXT_EBANG:
  case PSL_NEXT_A:
  case PSL_NEXT_ABANG:
    {
      PslNode_ptr l = psl_node_get_left(expr);
      PslNode_ptr r = psl_node_get_right(expr);

      /* we do not distinguish among weak and strong next */
      nusmv_assert(r != PSL_NULL);
      nusmv_assert(psl_node_get_op(r) == COLON);
      nusmv_assert(psl_node_get_right(r) == PSL_NULL);

      result = psl_node_pslltl2ltl(env, l, type, replicator_id_stack);

      {
        PslNode_ptr rr;
        int inf, sup;
        PslNode_ptr lim_expr = psl_node_get_left(r);

        nusmv_assert(psl_node_get_op(lim_expr) == PSL_RANGE);

        if (!psl_node_is_number(psl_node_get_left(lim_expr))) {
          StreamMgr_print_error(streams,  "In PSL expression '");
          StreamMgr_nprint_error(streams, wffprint, "%N", expr);
          StreamMgr_print_error(streams,  "'\n");
          ErrorMgr_error_psl_not_supported_feature_next_number(errmgr);
        }
        inf = psl_node_number_get_value(psl_node_get_left(lim_expr));

        if (!psl_node_is_number(psl_node_get_right(lim_expr))) {
          StreamMgr_print_error(streams,  "In PSL expression '");
          StreamMgr_nprint_error(streams, wffprint, "%N", expr);
          StreamMgr_print_error(streams,  "'\n");
          ErrorMgr_error_psl_not_supported_feature_next_number(errmgr);
        }
        sup = psl_node_number_get_value(psl_node_get_right(lim_expr));

        if (inf > sup) {
          StreamMgr_print_error(streams,  "Error in: ");
          StreamMgr_nprint_error(streams, wffprint, "%N", expr);
          StreamMgr_print_error(streams,  "\n");

          ErrorMgr_error_invalid_numeric_value(errmgr, sup, "Next operators expect"\
                                      " ranges with high bound greater than"\
                                      " low bound.");
        }

        rr = result;

#ifdef PSL_CONV_DISTRIB_NEXT
        /*
           X^{inf}(phi & X (phi & .. X(phi & X phi)....))
           \----------------- sup - inf -------/
        */
        {
          int k;

          for (k = (sup - inf); k > 0; --k) {
            if ((op == PSL_NEXT_E) || (op == PSL_NEXT_EBANG)) {
              /* result' = result | rr */
              rr = psl_new_node(nodemgr, psl_conv_op(env, type, OR), result,
                                psl_new_node(nodemgr, psl_conv_op(env, type, PSL_XBANG),
                                             rr, PSL_NULL));
            }
            else {
              rr = psl_new_node(nodemgr, psl_conv_op(env,type, AND), result,
                                psl_new_node(nodemgr, psl_conv_op(env, type, PSL_XBANG),
                                             rr, PSL_NULL));
            }
          }
          for (k = inf; k > 0; --k) {
            rr = psl_new_node(nodemgr, psl_conv_op(env, type, PSL_XBANG), rr, PSL_NULL);
          }
        }
        result = rr;
#else
        /* X^{inf}(phi) & X^{inf+1}(phi) & ... & X^{sup}(phi) */

        for (i = inf; i > 0; --i) {
          result = psl_new_node(nodemgr, psl_conv_op(env, type, PSL_XBANG),
                                result, PSL_NULL);
        }

        rr = result;

        for (i = inf + 1 ; i <= sup; ++i) {
          rr = psl_new_node(nodemgr, psl_conv_op(env, type, PSL_XBANG), rr, PSL_NULL);

          if ((op == PSL_NEXT_E) || (op == PSL_NEXT_EBANG)) {
            /* result' = result | rr */
            result = psl_new_node(nodemgr, psl_conv_op(env, type, OR), result, rr);
          }
          else {
            /* result' = result & rr */
            result = psl_new_node(nodemgr, psl_conv_op(env, type, AND), result, rr);
          }
        }
#endif
      }
    }
    break;

  case PSL_NEXT_EVENT:
  case PSL_NEXT_EVENTBANG:
    {
      PslNode_ptr b;
      PslNode_ptr l = psl_node_get_left(expr);
      PslNode_ptr r = psl_node_get_right(expr);

      nusmv_assert(r != PSL_NULL);
      nusmv_assert(psl_node_get_op(r) == COLON);

      b = psl_node_pslltl2ltl(env, psl_node_get_right(r), type,
                              replicator_id_stack);
      nusmv_assert(b != PSL_NULL);

      /* !b {U|W} (b & l) */
      result = psl_node_expand_next_event(env, op,
                                          psl_node_pslltl2ltl(env,l, type, replicator_id_stack),
                                          b, type);

      {
        PslNode_ptr lim_expr = psl_node_get_left(r);

        if (lim_expr != PSL_NULL) {
          /* the next event is iterated */
          int lim;

          if (!psl_node_is_number(lim_expr)) {
            StreamMgr_print_error(streams,  "In PSL expression '");
            StreamMgr_nprint_error(streams, wffprint, "%N", expr);
            StreamMgr_print_error(streams,  "'\n");
            ErrorMgr_error_psl_not_supported_feature_next_number(errmgr);
          }
          lim  = psl_node_number_get_value(lim_expr);
          if (lim <= 0) {
            StreamMgr_print_error(streams,  "Error in: ");
            StreamMgr_nprint_error(streams, wffprint, "%N", expr);
            StreamMgr_print_error(streams,  "\n");

            ErrorMgr_error_invalid_numeric_value(errmgr, lim, "Next event operators expect"\
                                        " a positive integer.");
          }

          for (; lim > 1; --lim) {
            /* We assume strong next */
            result = psl_new_node(nodemgr, psl_conv_op(env, type, PSL_XBANG),
                                  result, PSL_NULL);
            result = psl_node_expand_next_event(env, op, result, b, type);
          }
        }
      }
    }
    break;

  case PSL_NEXT_EVENT_E:
  case PSL_NEXT_EVENT_EBANG:
  case PSL_NEXT_EVENT_A:
  case PSL_NEXT_EVENT_ABANG:
    {
      PslNode_ptr b;
      PslNode_ptr l = psl_node_get_left(expr);
      PslNode_ptr r = psl_node_get_right(expr);

      nusmv_assert(r != PSL_NULL);
      nusmv_assert(psl_node_get_op(r) == COLON);

      b = psl_node_pslltl2ltl(env, psl_node_get_right(r), type, replicator_id_stack);
      nusmv_assert(b != PSL_NULL);

      {
        int i, inf, sup;
        PslNode_ptr rr;
        PslNode_ptr lim_expr = psl_node_get_left(r);

        /* !b {U|W} (b & l) */
        rr = psl_node_expand_next_event(env, op,
                                        psl_node_pslltl2ltl(env,l, type, replicator_id_stack),
                                        b, type);

        if (!psl_node_is_number(psl_node_get_left(lim_expr))) {
          StreamMgr_print_error(streams,  "In PSL expression '");
          StreamMgr_nprint_error(streams, wffprint, "%N", expr);
          StreamMgr_print_error(streams,  "'\n");
          ErrorMgr_error_psl_not_supported_feature_next_number(errmgr);
        }
        inf = psl_node_number_get_value(psl_node_get_left(lim_expr));

        if (!psl_node_is_number(psl_node_get_right(lim_expr))) {
          StreamMgr_print_error(streams,  "In PSL expression '");
          StreamMgr_nprint_error(streams, wffprint, "%N", expr);
          StreamMgr_print_error(streams,  "'\n");
          ErrorMgr_error_psl_not_supported_feature_next_number(errmgr);
        }
        sup = psl_node_number_get_value(psl_node_get_right(lim_expr));

        /* inf, sup must be greater than 0 */
        if (inf <= 0) {
          StreamMgr_print_error(streams,  "Error in: ");
          StreamMgr_nprint_error(streams, wffprint, "%N", expr);
          StreamMgr_print_error(streams,  "\n");

          ErrorMgr_error_invalid_numeric_value(errmgr, inf, "Next event operators expect"\
                                      " a positive range.");
        }

        if (sup <= 0) {
          StreamMgr_print_error(streams,  "Error in: ");
          StreamMgr_nprint_error(streams, wffprint, "%N", expr);
          StreamMgr_print_error(streams,  "\n");

          ErrorMgr_error_invalid_numeric_value(errmgr, sup, "Next event operators expect"\
                                      " a positive range.");
        }

        if (inf > sup) {
          StreamMgr_print_error(streams,  "Error in: ");
          StreamMgr_nprint_error(streams, wffprint, "%N", expr);
          StreamMgr_print_error(streams,  "\n");

          ErrorMgr_error_invalid_numeric_value(errmgr, sup, "Next event operators expect"\
                                      " ranges with high bound greater than"\
                                      " low bound.");
        }

        for (i = 2; i <= inf; ++i) {
          rr = psl_new_node(nodemgr, psl_conv_op(env, type, PSL_XBANG), rr, PSL_NULL);
          rr = psl_node_expand_next_event(env, op, rr, b, type);
        }

        result = rr;

        for (i = inf + 1; i <= sup; ++i) {
          rr = psl_new_node(nodemgr, psl_conv_op(env, type, PSL_XBANG), rr, PSL_NULL);
          rr = psl_node_expand_next_event(env, op, rr, b, type);

          if (op == PSL_NEXT_EVENT_A || op == PSL_NEXT_EVENT_ABANG) {
            result = psl_new_node(nodemgr, psl_conv_op(env, type, AND), result, rr);
          }
          else result = psl_new_node(nodemgr, psl_conv_op(env, type, OR), result, rr);
        }
      }
      break;
    }

    /* never */
  case PSL_NEVER:
    {
      /* globally */
      PslNode_ptr not_l;
      PslNode_ptr l = psl_node_get_left(expr);
      PslNode_ptr r = psl_node_get_right(expr);

      nusmv_assert(r == PSL_NULL);

      l = psl_node_pslltl2ltl(env, l, type, replicator_id_stack);
      not_l = psl_new_node(nodemgr, psl_conv_op(env, type, NOT), l, PSL_NULL);
      result = psl_new_node(nodemgr, psl_conv_op(env, type, PSL_ALWAYS), not_l, PSL_NULL);
      break;
    }

    /* weak until operators */
  case PSL_W:
  case PSL_UNTIL:
    {
      PslNode_ptr f1 = psl_node_get_left(expr);
      PslNode_ptr f2 = psl_node_get_right(expr);

      f1 = psl_node_pslltl2ltl(env,f1, type, replicator_id_stack);
      f2 = psl_node_pslltl2ltl(env, f2, type, replicator_id_stack);

      result = psl_new_node(nodemgr, psl_conv_op(env, type, UNTIL), f1, f2);
      result = psl_new_node(nodemgr, psl_conv_op(env, type, OR), result,
                            psl_new_node(nodemgr, psl_conv_op(env, type, PSL_ALWAYS),
                                         f1, PSL_NULL));
      break;
    }

  case PSL_UNTILBANG:
    {
      PslNode_ptr f1 = psl_node_get_left(expr);
      PslNode_ptr f2 = psl_node_get_right(expr);

      f1 = psl_node_pslltl2ltl(env, f1, type, replicator_id_stack);
      f2 = psl_node_pslltl2ltl(env, f2, type, replicator_id_stack);

      result = psl_new_node(nodemgr, psl_conv_op(env, type, UNTIL), f1, f2);
      break;
    }

  case PSL_UNTILBANG_:
    {
      PslNode_ptr f1 = psl_node_get_left(expr);
      PslNode_ptr f2 = psl_node_get_right(expr);

      f1 = psl_node_pslltl2ltl(env, f1, type, replicator_id_stack);
      f2 = psl_node_pslltl2ltl(env, f2, type, replicator_id_stack);

      f2 = psl_new_node(nodemgr, psl_conv_op(env, type, AND), f1, f2);
      result = psl_new_node(nodemgr, psl_conv_op(env, type, UNTIL), f1, f2);
      break;
    }

  case PSL_UNTIL_:
    {
      PslNode_ptr f1 = psl_node_get_left(expr);
      PslNode_ptr f2 = psl_node_get_right(expr);

      f1 = psl_node_pslltl2ltl(env, f1, type, replicator_id_stack);
      f2 = psl_node_pslltl2ltl(env, f2, type, replicator_id_stack);

      f2 = psl_new_node(nodemgr, psl_conv_op(env, type, AND), f1, f2);
      result = psl_new_node(nodemgr, psl_conv_op(env, type, UNTIL), f1, f2);
      result = psl_new_node(nodemgr, psl_conv_op(env, type, OR), result,
                            psl_new_node(nodemgr, psl_conv_op(env, type, PSL_ALWAYS),
                                         f1, PSL_NULL));
      break;
    }

  case PSL_BEFOREBANG:
    {
      PslNode_ptr f1 = psl_node_get_left(expr);
      PslNode_ptr f2 = psl_node_get_right(expr);

      f1 = psl_node_pslltl2ltl(env, f1, type, replicator_id_stack);
      f2 = psl_node_pslltl2ltl(env, f2, type, replicator_id_stack);

      f2 = psl_new_node(nodemgr, psl_conv_op(env, type, NOT), f2, PSL_NULL);
      f1 = psl_new_node(nodemgr, psl_conv_op(env, type, AND), f1, f2);
      result = psl_new_node(nodemgr, psl_conv_op(env, type, UNTIL), f2, f1);
      break;
    }

  case PSL_BEFORE:
    {
      PslNode_ptr f1 = psl_node_get_left(expr);
      PslNode_ptr f2 = psl_node_get_right(expr);

      f1 = psl_node_pslltl2ltl(env, f1, type, replicator_id_stack);
      f2 = psl_node_pslltl2ltl(env, f2, type, replicator_id_stack);

      f2 = psl_new_node(nodemgr, psl_conv_op(env, type, NOT), f2, PSL_NULL);
      f1 = psl_new_node(nodemgr, psl_conv_op(env, type, AND), f1, f2);
      result = psl_new_node(nodemgr, psl_conv_op(env, type, UNTIL), f2, f1);
      result = psl_new_node(nodemgr, psl_conv_op(env, type, OR), result,
                            psl_new_node(nodemgr, psl_conv_op(env, type, PSL_ALWAYS),
                                         f2, PSL_NULL));
      break;
    }

  case PSL_BEFOREBANG_:
    {
      PslNode_ptr f1 = psl_node_get_left(expr);
      PslNode_ptr f2 = psl_node_get_right(expr);

      f1 = psl_node_pslltl2ltl(env, f1, type, replicator_id_stack);
      f2 = psl_node_pslltl2ltl(env, f2, type, replicator_id_stack);

      f2 = psl_new_node(nodemgr, psl_conv_op(env, type, NOT), f2, PSL_NULL);
      result = psl_new_node(nodemgr, psl_conv_op(env, type, UNTIL), f2, f1);
      break;
    }

  case PSL_BEFORE_:
    {
      PslNode_ptr f1 = psl_node_get_left(expr);
      PslNode_ptr f2 = psl_node_get_right(expr);

      f1 = psl_node_pslltl2ltl(env, f1, type, replicator_id_stack);
      f2 = psl_node_pslltl2ltl(env, f2, type, replicator_id_stack);

      f2 = psl_new_node(nodemgr, psl_conv_op(env, type, NOT), f2, PSL_NULL);
      result = psl_new_node(nodemgr, psl_conv_op(env, type, UNTIL), f2, f1);
      result = psl_new_node(nodemgr, psl_conv_op(env, type, OR), result,
                            psl_new_node(nodemgr, psl_conv_op(env, type, PSL_ALWAYS),
                                         f2, PSL_NULL));
      break;
    }

  case PSL_REPLPROP:
    {
      PslNode_ptr rep = psl_node_repl_prop_get_replicator(expr);
      PslNode_ptr wff = psl_node_repl_prop_get_property(expr);
      PslNode_ptr id = psl_node_get_replicator_id(rep);
      PslOp rop = psl_node_get_replicator_join_op(rep);

      /* checks if the forall ID has been already used by an outer forall */
      if (NodeList_belongs_to(replicator_id_stack,
                              PslNode_convert_to_node_ptr(id))) {
        ErrorMgr_error_psl_repeated_replicator_id(errmgr);
      }
      NodeList_prepend(replicator_id_stack, PslNode_convert_to_node_ptr(id));

      result = psl_node_expand_replicator(env, rep, wff, rop);
      result = psl_node_pslltl2ltl(env, result, type, replicator_id_stack);

      /* finally pops the forall ID from the stack: */
      {
        node_ptr el = NodeList_remove_elem_at(replicator_id_stack,
                              NodeList_get_first_iter(replicator_id_stack));
        free_node(nodemgr, el);
      }
      break;
    }

  default:
    StreamMgr_print_error(streams, 
            "psl_node_pslltl2ltl: operator type not supported \"%d\"\n",
            op);
    error_unreachable_code();
  }

  return result;
}


/*!
  \brief Converts the given expression (possibly containing sere)
into an equivalent LTL formula

  

  \se required

  \sa optional
*/

PslNode_ptr PslNode_remove_sere(const NuSMVEnv_ptr env, PslNode_ptr e)
{
  PslNode_ptr r = psl_node_sere_translate(env, e);
  /* Here e is a disjunction of concat/fusion */
  PslNode_ptr m = psl_node_sere_remove_disj(env, r);
  return m;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief Converts the given operator into either a PSL operator, or
a SMV operator, depending on the value of 'type'

  

  \se required

  \sa PSL_OP_CONV
*/

#include "nusmv/core/parser/psl/psl_grammar.h" /* token are used only here */
PslOp psl_conv_op(const NuSMVEnv_ptr env, PslOpConvType type, PslOp op)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  ErrorMgr_ptr errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  /* This is table is organized as either:
        TOKEN, SYMBOL
     or:
        TOKEN, PSL_SYMBOL, SMV_SYMBOL            */
  PSL_OP_CONV2(errmgr, TKCOMMA, COMMA);
  PSL_OP_CONV2(errmgr, TKBOOLEAN, BOOLEAN);
  PSL_OP_CONV2(errmgr, TKCONTEXT, CONTEXT);
  PSL_OP_CONV2(errmgr, TKIDENTIFIER, DOT);
  PSL_OP_CONV3(errmgr, TKDOTDOT, PSL_RANGE, TWODOTS);
  PSL_OP_CONV2(errmgr, TKATOM, ATOM);
  PSL_OP_CONV2(errmgr, TKARRAY, ARRAY);
  PSL_OP_CONV2(errmgr, TKTRUE, TRUEEXP);
  PSL_OP_CONV2(errmgr, TKFALSE, FALSEEXP);
  PSL_OP_CONV2(errmgr, TKNUMBER, NUMBER);
  PSL_OP_CONV2(errmgr, TKREALNUMBER, NUMBER_REAL);

  /* WORD SUPPORT */
  PSL_OP_CONV2(errmgr, TKSIGNEDWORDNUMBER, NUMBER_SIGNED_WORD);
  PSL_OP_CONV2(errmgr, TKUNSIGNEDWORDNUMBER, NUMBER_UNSIGNED_WORD);
  PSL_OP_CONV2(errmgr, TKBOOL, CAST_BOOL);
  PSL_OP_CONV2(errmgr, TKWORD1, CAST_WORD1);
  PSL_OP_CONV2(errmgr, TKWRESIZE, WRESIZE);
  PSL_OP_CONV2(errmgr, TKUWCONST, UWCONST);
  PSL_OP_CONV2(errmgr, TKSWCONST, SWCONST);
  PSL_OP_CONV2(errmgr, TKWSIZEOF, WSIZEOF);
  PSL_OP_CONV2(errmgr, TKWTOINT, CAST_TOINT);
  PSL_OP_CONV2(errmgr, TKSIGNED, CAST_SIGNED);
  PSL_OP_CONV2(errmgr, TKUNSIGNED, CAST_UNSIGNED);
  PSL_OP_CONV2(errmgr, TKEXTEND, EXTEND);
  PSL_OP_CONV2(errmgr, TKBITSELECTION, BIT_SELECTION);
  PSL_OP_CONV2(errmgr, TKWSELECT, PSL_WSELECT);
  PSL_OP_CONV2(errmgr, TKXNOR, XNOR);
  PSL_OP_CONV2(errmgr, TKCONCATENATION, CONCATENATION);
  PSL_OP_CONV2(errmgr, TKLTLT, LSHIFT);
  PSL_OP_CONV2(errmgr, TKGTGT, RSHIFT);
  PSL_OP_CONV2(errmgr, TKUMINUS, UMINUS);

  PSL_OP_CONV2(errmgr, TKMINUS, MINUS);
  PSL_OP_CONV2(errmgr, TKPLUS, PLUS);
  PSL_OP_CONV2(errmgr, TKUNION, UNION);
  PSL_OP_CONV2(errmgr, TKIN, SETIN);
  PSL_OP_CONV2(errmgr, TKSPLAT, TIMES);
  PSL_OP_CONV2(errmgr, TKSLASH, DIVIDE);
  PSL_OP_CONV2(errmgr, TKPERCENT, MOD);
  PSL_OP_CONV2(errmgr, TKEQ, EQUAL);
  PSL_OP_CONV2(errmgr, TKEQEQ, EQUAL);
  PSL_OP_CONV2(errmgr, TKBANGEQ, NOTEQUAL);
  PSL_OP_CONV2(errmgr, TKLT, LT);
  PSL_OP_CONV2(errmgr, TKLE, LE);
  PSL_OP_CONV2(errmgr, TKGT, GT);
  PSL_OP_CONV2(errmgr, TKGE, GE);

  PSL_OP_CONV2(errmgr, TKCONS, CONS);

  PSL_OP_CONV2(errmgr, TKCASE, CASE);
  PSL_OP_CONV2(errmgr, TKCOLON, COLON);

  PSL_OP_CONV2(errmgr, TKBANG, NOT);
  PSL_OP_CONV3(errmgr, TKTILDE, PSL_TILDE, NOT);
  PSL_OP_CONV2(errmgr, TKAMPERSAND, AND);
  PSL_OP_CONV2(errmgr, TKPIPE, OR);
  PSL_OP_CONV3(errmgr, TKCARET, PSL_CARET, XOR);
  PSL_OP_CONV2(errmgr, TKXOR, XOR);

  PSL_OP_CONV2(errmgr, TKLTMINUSGT, IFF);
  PSL_OP_CONV2(errmgr, TKMINUSGT, IMPLIES);

  PSL_OP_CONV3(errmgr, TKX, PSL_X, OP_NEXT);
  PSL_OP_CONV3(errmgr, TKXBANG, PSL_XBANG, OP_NEXT);
  PSL_OP_CONV3(errmgr, TKNEXT, PSL_NEXT, OP_NEXT);
  PSL_OP_CONV3(errmgr, TKNEXTBANG, PSL_NEXTBANG, OP_NEXT);

  PSL_OP_CONV3(errmgr, TKEVENTUALLYBANG, PSL_EVENTUALLYBANG, OP_FUTURE);
  PSL_OP_CONV2(errmgr, TKF, OP_FUTURE);

  PSL_OP_CONV2(errmgr, TKG, OP_GLOBAL);
  PSL_OP_CONV3(errmgr, TKALWAYS, PSL_ALWAYS, OP_GLOBAL);

  PSL_OP_CONV2(errmgr, TKU, UNTIL);
  PSL_OP_CONV3(errmgr, TKUNTIL, PSL_UNTIL, UNTIL);
  PSL_OP_CONV3(errmgr, TKUNTILBANG, PSL_UNTILBANG, UNTIL);

  PSL_OP_CONV2(errmgr, TKFAILURE, FAILURE);

  /* CTL operators */
  PSL_OP_CONV2(errmgr, TKAX, AX);
  PSL_OP_CONV2(errmgr, TKEX, EX);
  PSL_OP_CONV2(errmgr, TKAG, AG);
  PSL_OP_CONV2(errmgr, TKEG, EG);
  PSL_OP_CONV2(errmgr, TKAF, AF);
  PSL_OP_CONV2(errmgr, TKEF, EF);

  PSL_OP_CONV2(errmgr, TKE, EU);
  PSL_OP_CONV2(errmgr, TKA, AU);

  PSL_OP_CONV3(errmgr, TKOR, PSL_OR, OR);

  /* Specific of psl */
  PSL_OP_CONV3(errmgr, TKSERE, PSL_SERE, -1);
  PSL_OP_CONV3(errmgr, TKSERECONCAT, PSL_SERECONCAT, -1);
  PSL_OP_CONV3(errmgr, TKSEREFUSION, PSL_SEREFUSION, -1);
  PSL_OP_CONV3(errmgr, TKSERECOMPOUND, PSL_SERECOMPOUND, -1);
  PSL_OP_CONV3(errmgr, TKSEREREPEATED, PSL_SEREREPEATED, -1);
  PSL_OP_CONV3(errmgr, TKCONCATENATION, PSL_CONCATENATION, -1);
  PSL_OP_CONV3(errmgr, TKREPLPROP, PSL_REPLPROP, -1);
  PSL_OP_CONV3(errmgr, TKINF, PSL_INF, -1);
  PSL_OP_CONV3(errmgr, TKFORALL, PSL_FORALL, -1);
  PSL_OP_CONV3(errmgr, TKFORANY, PSL_FORANY, -1);
  PSL_OP_CONV3(errmgr, TKPIPEMINUSGT, PSL_PIPEMINUSGT, -1);
  PSL_OP_CONV3(errmgr, TKPIPEEQGT, PSL_PIPEEQGT, -1);
  PSL_OP_CONV3(errmgr, TKNEVER, PSL_NEVER, -1);
  PSL_OP_CONV3(errmgr, TKWITHINBANG, PSL_WITHINBANG, -1);
  PSL_OP_CONV3(errmgr, TKWITHIN, PSL_WITHIN, -1);
  PSL_OP_CONV3(errmgr, TKWITHINBANG_, PSL_WITHINBANG_, -1);
  PSL_OP_CONV3(errmgr, TKWITHIN_, PSL_WITHIN_, -1);
  PSL_OP_CONV3(errmgr, TKWHILENOTBANG, PSL_WHILENOTBANG, -1);
  PSL_OP_CONV3(errmgr, TKWHILENOT, PSL_WHILENOT, -1);
  PSL_OP_CONV3(errmgr, TKWHILENOTBANG_, PSL_WHILENOTBANG_, -1);
  PSL_OP_CONV3(errmgr, TKWHILENOT_, PSL_WHILENOT_, -1);
  PSL_OP_CONV3(errmgr, TKNEXT_EVENT_ABANG, PSL_NEXT_EVENT_ABANG, -1);
  PSL_OP_CONV3(errmgr, TKNEXT_EVENT_A, PSL_NEXT_EVENT_A, -1);
  PSL_OP_CONV3(errmgr, TKNEXT_EVENT_EBANG, PSL_NEXT_EVENT_EBANG, -1);
  PSL_OP_CONV3(errmgr, TKNEXT_EVENT_E, PSL_NEXT_EVENT_E, -1);
  PSL_OP_CONV3(errmgr, TKNEXT_EVENTBANG, PSL_NEXT_EVENTBANG, -1);
  PSL_OP_CONV3(errmgr, TKNEXT_EVENT, PSL_NEXT_EVENT, -1);
  PSL_OP_CONV3(errmgr, TKNEXT_ABANG, PSL_NEXT_ABANG, -1);
  PSL_OP_CONV3(errmgr, TKNEXT_EBANG, PSL_NEXT_EBANG, -1);
  PSL_OP_CONV3(errmgr, TKNEXT_A, PSL_NEXT_A, -1);
  PSL_OP_CONV3(errmgr, TKNEXT_E, PSL_NEXT_E, -1);
  PSL_OP_CONV3(errmgr, TKBEFOREBANG, PSL_BEFOREBANG, -1);
  PSL_OP_CONV3(errmgr, TKBEFORE, PSL_BEFORE, -1);
  PSL_OP_CONV3(errmgr, TKBEFOREBANG_, PSL_BEFOREBANG_, -1);
  PSL_OP_CONV3(errmgr, TKBEFORE_, PSL_BEFORE_, -1);
  PSL_OP_CONV3(errmgr, TKUNTILBANG_, PSL_UNTILBANG_, -1);
  PSL_OP_CONV3(errmgr, TKUNTIL_, PSL_UNTIL_, -1);
  PSL_OP_CONV3(errmgr, TKABORT, PSL_ABORT, -1);
  PSL_OP_CONV3(errmgr, TKW, PSL_W, -1);
  PSL_OP_CONV3(errmgr, TKPIPEPIPE, PSL_PIPEPIPE, -1);
  PSL_OP_CONV3(errmgr, TKAMPERSANDAMPERSAND, PSL_AMPERSANDAMPERSAND, -1);
  PSL_OP_CONV3(errmgr, TKCARET, PSL_CARET, -1);
  PSL_OP_CONV3(errmgr, TKLBSPLAT, PSL_LBSPLAT, -1);
  PSL_OP_CONV3(errmgr, TKLBEQ, PSL_LBEQ, -1);
  PSL_OP_CONV3(errmgr, TKLBMINUSGT, PSL_LBMINUSGT, -1);
  PSL_OP_CONV3(errmgr, TKLBPLUSRB, PSL_LBPLUSRB, -1);
  PSL_OP_CONV3(errmgr, TKITE, PSL_ITE, -1);

  StreamMgr_print_error(streams,  "psl_conv_op: operator type not supported \"%d\"\n", op);
  ErrorMgr_internal_error(errmgr, "Invalid operator.");
  return 0; /* return something to suppress C warnings */
}



/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief During the conversion to LTL, this function is invoked
when the expansion of next_event family is required.

  

  \se required

  \sa optional
*/
static PslNode_ptr
psl_node_expand_next_event(const NuSMVEnv_ptr env, PslOp op, PslNode_ptr f, PslNode_ptr b,
                           PslOpConvType type)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslNode_ptr result, notb;

  nusmv_assert((op == PSL_NEXT_EVENT) || (op == PSL_NEXT_EVENTBANG) ||
               (op == PSL_NEXT_EVENT_E) || (op == PSL_NEXT_EVENT_EBANG) ||
               (op == PSL_NEXT_EVENT_A) || (op == PSL_NEXT_EVENT_ABANG));

  notb = psl_new_node(nodemgr, psl_conv_op(env, type, NOT), b, PSL_NULL);
  result = psl_new_node(nodemgr, psl_conv_op(env, type, AND), b, f);

  if ((op == PSL_NEXT_EVENT) ||
      (op == PSL_NEXT_EVENT_E) ||
      (op == PSL_NEXT_EVENT_A)) {
    /* result = psl_new_node(nodemgr, PSL_W, notb, result); */
    result = psl_new_node(nodemgr, psl_conv_op(env, type, OR),
                          psl_new_node(nodemgr, psl_conv_op(env, type, UNTIL), notb, result),
                          psl_new_node(nodemgr, psl_conv_op(env, type, PSL_ALWAYS),
                                       notb, PSL_NULL));
  }
  else {
    result = psl_new_node(nodemgr, psl_conv_op(env, type, UNTIL), notb, result);
  }
  return result;
}

/*!
  \brief This is used to rename IDs occurring in the tree, when
the replicator 'foreach' statement is resolved

  

  \se required

  \sa optional
*/
static PslNode_ptr
psl_node_subst_id(const NuSMVEnv_ptr env,
                  PslNode_ptr expr, PslNode_ptr id, PslNode_ptr v,
                  boolean is_top_level)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
if (expr == PSL_NULL) return expr;

  /* Does not substitute inner replicators */
  if (psl_node_is_replicator(expr)) return expr;

  if (psl_node_is_id(expr)) {
    if (psl_node_is_id_equal(expr, id) && is_top_level) return v; /* substitute */
    else {
      switch (psl_node_get_op(expr)) {
      case ARRAY:
        /* Developers' note: second operand allows to write expressions like:
           forall i in {0,1,2}: forall arr in {a,b,c}: arr[i];
           ... will be expanded in:
           ((((c[2] & b[2]) & a[2]) & ((c[1] & b[1]) & a[1])) & ((c[0] & b[0]) & a[0]))
        */
        expr = psl_new_node(nodemgr, psl_node_get_op(expr),
                            psl_node_subst_id(env, psl_node_get_left(expr), id, v, true),
                            psl_node_subst_id(env, psl_node_get_right(expr), id, v, true));
        break;

      case DOT:
        expr = psl_new_node(nodemgr, psl_node_get_op(expr),
                            psl_node_subst_id(env, psl_node_get_left(expr), id, v, false),
                            psl_node_subst_id(env, psl_node_get_right(expr), id, v, false));
        break;

      }
      return expr;
    }
  }

  if (psl_node_is_leaf(expr)) return expr;
  return psl_new_node(nodemgr, psl_node_get_op(expr),
                      psl_node_subst_id(env, psl_node_get_left(expr), id, v, true),
                      psl_node_subst_id(env, psl_node_get_right(expr), id, v, true));
}

/*!
  \brief Expansion of a replicator 'forall' statement

  Wff must not have been converted to smv yet when this
   function is called. Each replicated expression wff will be joined
   with the others by using the passed operator op. The result still
   contains only psl tokens.

  \se required

  \sa optional
*/
static PslNode_ptr
psl_node_expand_replicator(const NuSMVEnv_ptr env,
                           PslNode_ptr rep, PslNode_ptr wff, PslOp op)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
PslNode_ptr result;
  PslNode_ptr id;
  PslNode_ptr erange;

  nusmv_assert(psl_node_is_replicator(rep));

  id = psl_node_get_replicator_id(rep);
  erange = psl_node_get_replicator_normalized_value_set(env, rep);

  result = PSL_NULL;

  {
    PslNode_ptr r;

    for (r = erange ; r != PSL_NULL; r = psl_node_cons_get_next(r)) {
      PslNode_ptr rwff;
      PslNode_ptr rt;
      PslNode_ptr v = psl_node_cons_get_element(r);

      rt = r;
      rwff = psl_node_subst_id(env, wff, id, v, true);
      if (result == PSL_NULL) result = rwff;
      else result = psl_new_node(nodemgr, op, result, rwff);

      free_node(nodemgr, rt);
    }
  }
  return result;
}

/*!
  \brief Removes the disjunction among SERE, by distributing it

   This function assumes that expression is a disjunction
of concat/fusion

  \se required

  \sa optional
*/
static PslNode_ptr psl_node_sere_remove_disj(const NuSMVEnv_ptr env,
                                             PslNode_ptr e)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  if (e == PSL_NULL) return PSL_NULL;

  /* absorb sere parenteses */
  if (psl_node_get_op(e)==PSL_SERE) {
    return psl_node_sere_remove_disj(env, psl_node_get_left(e));
  }

  if (psl_node_is_id(e) || psl_node_is_leaf(e) ||
      PslNode_is_propositional(e)) {
    return e;
  }

  if (psl_node_sere_is_concat_fusion(e)) {
    return psl_node_sere_concat_fusion2ltl(env, e, PSL_NULL);
  }

  /* if the top level operator is a | */
  if (psl_node_get_op(e)==PSL_SERECOMPOUND && psl_node_get_left(e) &&
      psl_node_get_op(psl_node_get_left(e))==OR) {
    PslNode_ptr l, r;
    e = psl_node_get_left(e); /* gets r1 | r2 */
    l = psl_node_sere_remove_disj(env, psl_node_get_left(e)); /* gets r1 */
    r = psl_node_sere_remove_disj(env, psl_node_get_right(e)); /* gets r2 */
    return psl_new_node(nodemgr, OR /* here is logical or */, l, r);
  }

  return psl_new_node(nodemgr, psl_node_get_op(e),
                      psl_node_sere_remove_disj(env, psl_node_get_left(e)),
                      psl_node_sere_remove_disj(env, psl_node_get_right(e)));
}

/*!
  \brief Service due to way concat_fusion expansion is implemented

  

  \se required

  \sa optional
*/
static PslNode_ptr
psl_node_insert_inside_holes(const NuSMVEnv_ptr env,
                             PslNode_ptr e, PslNode_ptr to_be_inserted,
                             boolean* inserted)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  if (e == PSL_NULL) return PSL_NULL;

  if (to_be_inserted == PSL_NULL) return e;

  if (psl_node_is_id(e) || psl_node_is_leaf(e) ||
      PslNode_is_propositional(e)) {
    return e;
  }

  /* visits e and insert to_be_inserted into the right argument of
     each hole contained in e */

  /* holes inside until */
  if (psl_node_get_op(e) == PSL_UNTILBANG) {
    *inserted = true;
    return psl_new_node(nodemgr, PSL_UNTILBANG,
                        psl_node_get_left(e),
                        psl_new_node(nodemgr, AND,
                                     psl_node_get_right(e),
                                     to_be_inserted));
  }

  /* holes inside X of XF */

  if (psl_node_get_op(e) == PSL_EVENTUALLYBANG &&
      psl_node_get_left(e) == PSL_NULL) {
    /* inside F */
    *inserted = true;
    return psl_new_node(nodemgr, PSL_EVENTUALLYBANG, to_be_inserted, PSL_NULL);

  }

  {
    PslNode_ptr l = psl_node_insert_inside_holes(env, psl_node_get_left(e),
                                                 to_be_inserted,
                                                 inserted);
    PslNode_ptr r = psl_node_insert_inside_holes(env, psl_node_get_right(e),
                                                 to_be_inserted,
                                                 inserted);

    return psl_new_node(nodemgr, psl_node_get_op(e), l, r);
  }
}

/*!
  \brief Resolves concat/fusion and converts it to an equivalent LTL
expression

  This function assumes that expression is a concat/fusion

  \se required

  \sa optional
*/
static PslNode_ptr
psl_node_sere_concat_fusion2ltl(const NuSMVEnv_ptr env,
                                PslNode_ptr e, PslNode_ptr phi)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  if (e == PSL_NULL) return PSL_NULL;

  /* recursion on atomic  */
  if (psl_node_get_op(e)==PSL_SERE) {
    return psl_node_sere_concat_fusion2ltl(env, psl_node_get_left(e), phi);
  }

  /* rec rule: f({r1; r2})=f(r1, next! f(r2)) */
  if ((psl_node_get_op(e) == PSL_SERECONCAT) ||
      (psl_node_get_op(e) == PSL_SEREFUSION)) {
    PslNode_ptr r1 = psl_node_get_left(e);
    PslNode_ptr r2 = psl_node_get_right(e);
    PslNode_ptr next_phi = (PslNode_ptr) NULL;

    if (psl_node_get_op(e) == PSL_SERECONCAT) {
      next_phi = psl_new_node(nodemgr, PSL_XBANG,
                              psl_node_sere_concat_fusion2ltl(env, r2, phi),
                              PSL_NULL);
    }

    if (psl_node_get_op(e) == PSL_SEREFUSION) {
      next_phi = psl_node_sere_concat_fusion2ltl(env, r2, phi);
    }

    return psl_node_sere_concat_fusion2ltl(env, r1, next_phi);
  }

  /* atomic */
  {
    PslNode_ptr rec;
    boolean inserted = false;
    rec = psl_node_insert_inside_holes(env, e, phi, &inserted);
    if (inserted) phi = PSL_NULL;
    rec = psl_node_sere_remove_disj(env, rec);
    return (phi != PSL_NULL) ? psl_new_node(nodemgr, AND, rec, phi) : rec;
  }
}

/*!
  \brief High-level service of exported function PslNode_remove_sere

  

  \se required

  \sa optional
*/
static PslNode_ptr psl_node_sere_translate(const NuSMVEnv_ptr env,
                                           PslNode_ptr e)
{
  PslNode_ptr m=e;
  boolean mod = false;
  boolean mod1 = false;

  /* no need for fixpoint here (every possible star count is expanded
     one shoot). */
  m = psl_node_sere_remove_star_count(env, m);
#if PSL_VERBOSE_TRANSLATE
  StreamMgr_print_output(streams,  "multiplication<");
  StreamMgr_nprint_output(streams, wffprint, "%N", m);
  StreamMgr_print_output(streams,  ">\n");
#endif

  /* a first call to remove_star is needed to remove possible trailing
     starts and allow the treatmeant of possible trailing pluses, like
     in {a;[+];[*]} */
  /* here mod1 is passed as a dummy parameter and has no consequencese
     on the following code */
  m = psl_node_sere_remove_star(env, m, true, &mod1);
#if PSL_VERBOSE_TRANSLATE
  StreamMgr_print_output(streams,  "rem[*]<");
  StreamMgr_nprint_output(streams, wffprint, "%N", m);
  StreamMgr_print_output(streams,  ">\n");
#endif

  m = psl_node_sere_remove_plus(env, m, true);
#if PSL_VERBOSE_TRANSLATE
  StreamMgr_print_output(streams,  "rem[+]<");
  StreamMgr_nprint_output(streams, wffprint, "%N", m);
  StreamMgr_print_output(streams,  ">\n");
#endif

  m = psl_node_remove_suffix_implication(env, m);
#if PSL_VERBOSE_TRANSLATE
  StreamMgr_print_output(streams,  "rem[si]<");
  StreamMgr_nprint_output(streams, wffprint, "%N", m);
  StreamMgr_print_output(streams,  ">\n");
#endif

  do {
    mod = false;

    m = psl_node_sere_remove_star(env, m, true, &mod1); mod |= mod1;
#if PSL_VERBOSE_TRANSLATE
    StreamMgr_print_output(streams,  "rem[*]<");
    StreamMgr_nprint_output(streams, wffprint, "%N", m);
    StreamMgr_print_output(streams,  ">\n");
#endif

    m = psl_node_sere_remove_ampersand(env, m, &mod1); mod |= mod1;
#if PSL_VERBOSE_TRANSLATE
    if (mod1) {
      StreamMgr_print_output(streams,  "rem&<");
      StreamMgr_nprint_output(streams, wffprint, "%N", m);
      StreamMgr_print_output(streams,  ">\n");
    }
#endif

    m = psl_node_sere_remove_2ampersand(env, m, &mod1); mod=mod || mod1;
#if PSL_VERBOSE_TRANSLATE
    if (mod1) {
      StreamMgr_print_output(streams,  "rem&&<");
      StreamMgr_nprint_output(streams, wffprint, "%N", m);
      StreamMgr_print_output(streams,  ">\n");
    }
#endif

    m = psl_node_sere_remove_fusion(env, m, &mod1); mod=mod || mod1;
#if PSL_VERBOSE_TRANSLATE
    if (mod1) {
      StreamMgr_print_output(streams,  "rem:<");
      StreamMgr_nprint_output(streams, wffprint, "%N", m);
      StreamMgr_print_output(streams,  ">\n");
    }
#endif

    m = psl_node_sere_distrib_disj(env, m, &mod1); mod=mod || mod1;
#if PSL_VERBOSE_TRANSLATE
    if (mod1) {
      StreamMgr_print_output(streams,  "dist|<");
      StreamMgr_nprint_output(streams, wffprint, "%N", m);
      StreamMgr_print_output(streams,  ">\n");
    }
#endif

#if PSL_VERBOSE_TRANSLATE
    if (mod) {
      StreamMgr_print_output(streams,  "MOD: <");
      StreamMgr_nprint_output(streams, wffprint, "%N", m);
      StreamMgr_print_output(streams,  ">\n");
    }
#endif
  } while (mod);

  return m;
}

/*!
  \brief Returns true if the given expression is a disjunction of SEREs.

  

  \se required

  \sa optional
*/
static boolean psl_node_sere_is_disj(PslNode_ptr e)
{
  if (e == PSL_NULL) return false;
  if (psl_node_get_left(e) == PSL_NULL) return false;
  if (psl_node_get_op(e) == PSL_SERE) {
    return psl_node_sere_is_disj(psl_node_get_left(e));
  }
  return ((psl_node_get_op(e) == PSL_SERECOMPOUND) &&
          (psl_node_get_op(psl_node_get_left(e)) == OR));
}

/*!
  \brief Distributes the disjunction among SEREs

  

  \se required

  \sa optional
*/
static PslNode_ptr psl_node_sere_distrib_disj(const NuSMVEnv_ptr env,
                                              PslNode_ptr e, boolean *modified)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslOp op;
  PslOp real_op;

  *modified=false;

  if (e == PSL_NULL) return PSL_NULL;

  if (psl_node_is_leaf(e) || psl_node_is_id(e)) return e;

  op = psl_node_get_op(e);
  real_op = (op==PSL_SERECOMPOUND) ? psl_node_get_op(psl_node_get_left(e)) : op;

  /* distributive rule (lookahead on parse-tree is used ) */
  if (real_op==PSL_SERECONCAT || real_op==PSL_SEREFUSION ||
      real_op==AND || real_op==PSL_AMPERSANDAMPERSAND) {
    PslNode_ptr l, r, disj;

    if (op==PSL_SERECOMPOUND) {
      l = psl_node_get_left(psl_node_get_left(e));
      r = psl_node_get_right(psl_node_get_left(e));
    }
    else {
      l = psl_node_get_left(e);
      r = psl_node_get_right(e);
    }

    if (psl_node_sere_is_disj(l)) {
      /* left node is a disjunction */
      PslNode_ptr r1, r2, r3, r1_op_r3, r2_op_r3;

      *modified=true;
      while (psl_node_get_op(l)==PSL_SERE) l = psl_node_get_left(l);

      disj = psl_node_get_left(l); /* to remove PSL_SERECOMPOUND */
      nusmv_assert(psl_node_get_op(disj)==OR);

      r1 = psl_node_get_left(disj);
      r2 = psl_node_get_right(disj);
      r3 = r;

      if (real_op==PSL_SERECONCAT || real_op==PSL_SEREFUSION) {
        r1_op_r3 = psl_new_node(nodemgr, real_op, r1, r3);
        r2_op_r3 = psl_new_node(nodemgr, real_op, r2, r3);
      }
      else {
        r1_op_r3 = psl_node_make_sere_compound(nodemgr, r1, real_op, r3);
        r2_op_r3 = psl_node_make_sere_compound(nodemgr, r2, real_op, r3);
      }

      return psl_node_make_sere_compound(nodemgr, r1_op_r3, OR, r2_op_r3);
    }
    else if (psl_node_sere_is_disj(r)) {
      /* right node is a disjunction (no matter if both are) */
      PslNode_ptr r1, r2, r3, r3_op_r1, r3_op_r2;

      *modified=true;

      while (psl_node_get_op(r)==PSL_SERE) r = psl_node_get_left(r);

      disj = psl_node_get_left(r); /* to remove PSL_SERECOMPOUND */
      nusmv_assert(psl_node_get_op(disj)==OR);

      r1 = psl_node_get_left(disj);
      r2 = psl_node_get_right(disj);
      r3 = l;

      if (real_op==PSL_SERECONCAT || real_op==PSL_SEREFUSION) {
        r3_op_r1 = psl_new_node(nodemgr, real_op, r3, r1);
        r3_op_r2 = psl_new_node(nodemgr, real_op, r3, r2);
      }
      else {
        r3_op_r1 = psl_node_make_sere_compound(nodemgr, r3, real_op, r1);
        r3_op_r2 = psl_node_make_sere_compound(nodemgr, r3, real_op, r2);
      }

      return psl_node_make_sere_compound(nodemgr, r3_op_r1, OR, r3_op_r2);
    }
  }

  /* fallback rule */
  {
    boolean lm, rm;
    PslNode_ptr l=psl_node_sere_distrib_disj(env, psl_node_get_left(e), &lm);
    PslNode_ptr r=psl_node_sere_distrib_disj(env, psl_node_get_right(e), &rm);
    *modified = (lm||rm);
    return psl_new_node(nodemgr, psl_node_get_op(e), l, r);
  }
}

/*!
  \brief Resolves starred SEREs

  

  \se required

  \sa optional
*/
static PslNode_ptr psl_node_sere_remove_star_count(const NuSMVEnv_ptr env,
                                                   PslNode_ptr e)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  if (e == PSL_NULL) return PSL_NULL;

  if (psl_node_is_leaf(e) || psl_node_is_id(e)) return e;

  if (psl_node_sere_is_propositional(e)) return e;

  if (psl_node_sere_is_repeated(e) && !psl_node_sere_is_star_count(e)) return e;

  if (psl_node_get_op(e)==PSL_SERE) {
    return psl_node_sere_remove_star_count(env, psl_node_get_left(e));
  }

  if (psl_node_sere_is_star_count(e)) {
    /* assumes the counter is a number */
    PslNode_ptr count_range = psl_node_sere_star_get_count(e);
    int count;

    nusmv_assert(psl_node_is_number(count_range));
    count = psl_node_number_get_value(count_range);
    if (count>0) {
      /* a [* count] possibly applied to a sere */
      PslNode_ptr mul = psl_node_sere_remove_star_count(env, psl_node_get_left(psl_node_get_left(e)));
      if (mul ==  PSL_NULL) {
        /* a [* count] stand-alone, i.e. not applied to a sere, can be
           treated as if applied to the sere {True} */
        mul = psl_new_node(nodemgr, PSL_SERE, psl_node_make_true(nodemgr), PSL_NULL);
      }
      {
      PslNode_ptr acc = mul;
      for (count--; count>0; count--) acc = psl_new_node(nodemgr, PSL_SERECONCAT, mul, acc);

      return acc;
      }
    } /* else empty star or plus on propositionals */
  }

  {
    PslNode_ptr l=psl_node_sere_remove_star_count(env, psl_node_get_left(e));
    PslNode_ptr r=psl_node_sere_remove_star_count(env, psl_node_get_right(e));
    return psl_new_node(nodemgr, psl_node_get_op(e), l, r);
  }
}

/*!
  \brief Returns true if the given SERE is in the form {a} & {b}

  

  \se required

  \sa optional
*/
static boolean psl_node_sere_is_ampersand(PslNode_ptr e)
{
  return psl_node_is_sere_compound_binary(e) &&
    (psl_node_get_left(e) != PSL_NULL) &&
    (psl_node_get_op(psl_node_get_left(e))==AND);
}

/*!
  \brief Returns the leftmost element of e that is not a SERE

  

  \se required

  \sa optional
*/
static PslNode_ptr psl_node_sere_get_leftmost(PslNode_ptr e)
{
  if (psl_node_get_op(e)==PSL_SERE && ! psl_node_is_sere(psl_node_get_left(e))) {
    return e;
  }

  nusmv_assert(psl_node_sere_is_concat_fusion(e));
  nusmv_assert(psl_node_get_left(e) != PSL_NULL);
  return psl_node_sere_get_leftmost(psl_node_get_left(e));
}

/*!
  \brief Returns the rightmost element of e that is not a SERE

  

  \se required

  \sa optional
*/
static PslNode_ptr psl_node_sere_get_rightmost(PslNode_ptr e)
{
  if (psl_node_get_op(e)==PSL_SERE) {
    if (!psl_node_is_sere(psl_node_get_left(e))) return e;
    else return psl_node_sere_get_rightmost(psl_node_get_left(e));
  }

  nusmv_assert(psl_node_sere_is_concat_fusion(e));
  nusmv_assert(psl_node_get_right(e) != PSL_NULL);
  return psl_node_sere_get_rightmost(psl_node_get_right(e));
}

/*!
  \brief Resolve SERE \[+\]

  

  \se required

  \sa optional
*/
static PslNode_ptr psl_node_sere_remove_plus(const NuSMVEnv_ptr env,
                                             PslNode_ptr e, boolean toplevel)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  boolean toplevel_l = toplevel;
  boolean toplevel_r = toplevel;

  if (e == PSL_NULL) return PSL_NULL;
  if (psl_node_is_leaf(e) || psl_node_is_id(e)) return e;

  /* toplevel trailing plus in a concatenations is simplified
     e.g. r;[+] --> r;TRUE */
  if (psl_node_get_op(e)==PSL_SERECONCAT && toplevel) {
    e = psl_node_sere_remove_trailing_plus(env, e);
  }

  if (toplevel && psl_node_sere_is_standalone_plus(e)) {
    return psl_new_node(nodemgr, PSL_SERE, psl_node_make_true(nodemgr), PSL_NULL);
  }

  if (psl_node_sere_is_plus(e)) {
    PslNode_ptr expr;

    /* since the possible toplevel trailing plus has been already simplified,
       here we are NOT toplevel */
    /* {r1;[+]} --> {r1;{F}} */
    if(psl_node_sere_is_standalone_plus(e)) {
      return psl_new_node(nodemgr, PSL_SERE,
                          psl_new_node(nodemgr, PSL_EVENTUALLYBANG, PSL_NULL, PSL_NULL),
                          PSL_NULL);
    }

    expr = psl_node_sere_repeated_get_expr(e);

    return psl_new_node(nodemgr, PSL_SERE,
                        psl_new_node(nodemgr, PSL_UNTILBANG, expr, expr),
                        PSL_NULL);
  }

  if (psl_node_get_op(e)==PSL_SERECONCAT || psl_node_get_op(e)==PSL_SEREFUSION) {
    toplevel_l = false;
  }

/*   if (psl_node_get_op(e)==PSL_SERECONCAT || psl_node_get_op(e)==PSL_SEREFUSION) { */
/*     toplevel = false; */
/*   } */

  {
    PslNode_ptr l = psl_node_sere_remove_plus(env, psl_node_get_left(e), toplevel_l);
    PslNode_ptr r = psl_node_sere_remove_plus(env, psl_node_get_right(e), toplevel_r);

    return psl_new_node(nodemgr, psl_node_get_op(e), l, r);
  }
}

/*!
  \brief Resolves suffix implication

  

  \se required

  \sa optional
*/
static PslNode_ptr psl_node_remove_suffix_implication(const NuSMVEnv_ptr env,
                                                      PslNode_ptr e)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  if (e == PSL_NULL) return PSL_NULL;

  if (psl_node_is_leaf(e) || psl_node_is_id(e) ||
      psl_node_sere_is_propositional(e)) {
    return e;
  }

  if (psl_node_is_suffix_implication(e)) {
    PslNode_ptr pre, con;
    PslNode_ptr npre, sere_con;
    PslOp op = psl_node_get_op(e);

    pre = psl_node_suffix_implication_get_premise(e);
    con = psl_node_suffix_implication_get_consequence(e);
    npre = psl_new_node(nodemgr, NOT, pre, PSL_NULL);

    /* makes 'con' a sere if needed */
    if (!psl_node_is_sere(con)) {
      sere_con = psl_new_node(nodemgr, PSL_SERE, con, PSL_NULL);
    }
    else sere_con = con;

    /* resolves possible nested suffix implications within con: */
    sere_con = psl_node_remove_suffix_implication(env, sere_con);

    if (op == PSL_PIPEMINUSGT) {
      sere_con = psl_new_node(nodemgr, PSL_SEREFUSION, pre, sere_con);
    }
    else {
      nusmv_assert(op == PSL_PIPEEQGT);
      sere_con = psl_new_node(nodemgr, PSL_SERECONCAT, pre, sere_con);
    }

    return psl_new_node(nodemgr, OR, npre, sere_con);
  }
  else {
    PslNode_ptr l, r;
    l = psl_node_remove_suffix_implication(env, psl_node_get_left(e));
    r = psl_node_remove_suffix_implication(env, psl_node_get_right(e));
    return psl_new_node(nodemgr, psl_node_get_op(e), l, r);
  }
}

/*!
  \brief Resolves starred SEREs

  

  \se required

  \sa optional
*/
static PslNode_ptr psl_node_sere_remove_star(const NuSMVEnv_ptr env,
                                             PslNode_ptr e, boolean toplevel,
                                             boolean* modified)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  *modified = false;

  if (e == PSL_NULL) return PSL_NULL;
  if (psl_node_is_leaf(e) || psl_node_is_id(e)) return e;

  /* toplevel trailing stars in a concatenations are simplified
     e.g. r;[*];[*] --> r */
  if (toplevel) {
    e = psl_node_sere_remove_trailing_star(env, e, modified);
    if (e == PSL_NULL) return PSL_NULL;
  }

  if (psl_node_get_op(e)==PSL_SERECONCAT) {

    if (psl_node_sere_is_standalone_star(psl_node_get_right(e))) {
      PslNode_ptr l;

      /* since toplevel trailing stars have been already simplified,
         here we are NOT toplevel */
      /* {r1;[*]} --> {r1 | {r1;{F}}} */
      nusmv_assert(!toplevel);

      l = psl_node_sere_remove_star(env, psl_node_get_left(e), false, modified);

      /* regardless the effects of the recursive call on tail, the
         expression has been modified */
      *modified = true;
      return psl_new_node(nodemgr, PSL_SERECOMPOUND,
                psl_new_node(nodemgr, OR, l,
                   psl_new_node(nodemgr, PSL_SERECONCAT, l,
                      psl_new_node(nodemgr, PSL_SERE,
                         psl_new_node(nodemgr, PSL_EVENTUALLYBANG, PSL_NULL, PSL_NULL),
                            PSL_NULL))),
                          PSL_NULL);
    }

    if (psl_node_is_propstar(psl_node_get_right(e))) {
      PslNode_ptr l;
      PslNode_ptr p_star;
      PslNode_ptr p;

      /* {r1;b[*]} --> {r1 | {r1;{bUb}}} */

      l = psl_node_sere_remove_star(env, psl_node_get_left(e), false, modified);
      p_star = psl_node_get_right(e);
      while (psl_node_get_op(p_star)==PSL_SERE) p_star = psl_node_get_left(p_star);
      /* gets the expression that is argument of [*] */
      p = psl_node_sere_repeated_get_expr(p_star);

      /* regardless the effects of the recursive call on tail, the
         expression has been modified */
      *modified = true;
      return psl_new_node(nodemgr, PSL_SERECOMPOUND,
                psl_new_node(nodemgr, OR, l,
                   psl_new_node(nodemgr, PSL_SERECONCAT, l,
                                psl_new_node(nodemgr, PSL_SERE,
                                             psl_new_node(nodemgr, PSL_UNTILBANG, p, p),
                                             PSL_NULL))),
                          PSL_NULL);
    }

    if (psl_node_sere_is_standalone_star(psl_node_get_left(e))) {
      PslNode_ptr r;

      /* {[*];r1} --> {r1 | {{F};r1}} */

      r = psl_node_sere_remove_star(env, psl_node_get_right(e), false, modified);

      /* regardless the effects of the recursive call on tail, the
         expression has been modified */
      *modified = true;
      return psl_new_node(nodemgr, PSL_SERECOMPOUND,
                psl_new_node(nodemgr, OR, r,
                   psl_new_node(nodemgr, PSL_SERECONCAT,
                      psl_new_node(nodemgr, PSL_SERE,
                         psl_new_node(nodemgr, PSL_EVENTUALLYBANG, PSL_NULL, PSL_NULL),
                            PSL_NULL), r)),
                          PSL_NULL);

    }

    if (psl_node_is_propstar(psl_node_get_left(e))) {
      PslNode_ptr r;
      PslNode_ptr p_star;
      PslNode_ptr p;

      /* b[*];r2 --> r2 | {bUb};r2 */

      r = psl_node_sere_remove_star(env, psl_node_get_right(e), false, modified);
      p_star = psl_node_get_left(e);
      while (psl_node_get_op(p_star)==PSL_SERE) p_star = psl_node_get_left(p_star);
      /* gets the expression that is argument of [*] */
      p = psl_node_sere_repeated_get_expr(p_star);

      /* regardless the effects of the recursive call on tail, the
         expression has been modified */
      *modified = true;
      return psl_new_node(nodemgr, PSL_SERECOMPOUND,
                psl_new_node(nodemgr, OR, r,
                   psl_new_node(nodemgr, PSL_SERECONCAT,
                                psl_new_node(nodemgr, PSL_SERE,
                                             psl_new_node(nodemgr, PSL_UNTILBANG, p, p),
                                             PSL_NULL), r)),
                          PSL_NULL);
    }

  }

  /* either not PSL_SERECONCAT, or PSL_SERECONCAT but no star on left and right */

  /* fallback rule */
  {

    PslNode_ptr l;
    PslNode_ptr r;
    boolean toplevel_l = toplevel;
    boolean toplevel_r = toplevel;
    boolean rec_modified;

    if (psl_node_get_op(e)==PSL_SERECONCAT || psl_node_get_op(e)==PSL_SEREFUSION) {
      toplevel_l = false;
    }


    if (psl_node_is_sere_compound_binary(e)) {

      /* gets the operands */
      l = psl_node_get_left(psl_node_get_left(e));
      r = psl_node_get_right(psl_node_get_left(e));

      /* recursive calls on operands */
      l = psl_node_sere_remove_star(env, l, toplevel_l, &rec_modified);
      *modified |= rec_modified;

      r = psl_node_sere_remove_star(env, r, toplevel_r, &rec_modified);
      *modified |= rec_modified;

      if (l == PSL_NULL) return r;
      if (r == PSL_NULL) return l;

      return psl_node_make_sere_compound(nodemgr, l,
                psl_node_get_op(psl_node_get_left(e)), r);
    }

    l = psl_node_sere_remove_star(env, psl_node_get_left(e), toplevel_l,
                                  &rec_modified);
    *modified |= rec_modified;

    r = psl_node_sere_remove_star(env, psl_node_get_right(e), toplevel_r,
                                  &rec_modified);
    *modified |= rec_modified;

    return psl_new_node(nodemgr, psl_node_get_op(e), l, r);
  }
}

/*!
  \brief Resolves trailing standalone stars

  

  \se required

  \sa optional
*/
static PslNode_ptr psl_node_sere_remove_trailing_star(const NuSMVEnv_ptr env,
                                                      PslNode_ptr e,
                                                      boolean* modified)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslNode_ptr head;
  PslNode_ptr tail;
  PslNode_ptr tail_rec;

  if (e == PSL_NULL) return PSL_NULL;
  if (psl_node_is_leaf(e) || psl_node_is_id(e)) return e;

  if (psl_node_sere_is_star(e)) {
    *modified = true;
    return PSL_NULL;
  }

  if (!(psl_node_get_op(e) == PSL_SERECONCAT)) return e;

  head = psl_node_get_left(e);
  tail = psl_node_get_right(e);
  tail_rec = psl_node_sere_remove_trailing_star(env, tail, modified);

  if (tail_rec == PSL_NULL) {
    return psl_node_sere_remove_trailing_star(env, head, modified);
  }

  return psl_new_node(nodemgr, PSL_SERECONCAT, head, tail_rec);
}

/*!
  \brief Resolves the last trailing standalone plus

  

  \se required

  \sa optional
*/
static PslNode_ptr psl_node_sere_remove_trailing_plus(const NuSMVEnv_ptr env,
                                                      PslNode_ptr e)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslNode_ptr head;
  PslNode_ptr tail;
  PslNode_ptr tail_rec;

  if (e == PSL_NULL) return PSL_NULL;
  if (psl_node_is_leaf(e) || psl_node_is_id(e)) return e;

  if (psl_node_sere_is_standalone_plus(e)) {
    return psl_new_node(nodemgr, PSL_SERE, psl_node_make_true(nodemgr), PSL_NULL);
  }

  if (!(psl_node_get_op(e) == PSL_SERECONCAT)) return e;

  head = psl_node_get_left(e);
  tail = psl_node_get_right(e);
  tail_rec = psl_node_sere_remove_trailing_plus(env, tail);

  return psl_new_node(nodemgr, PSL_SERECONCAT, head, tail_rec);
}

/*!
  \brief Resolves {a}&{a}

  

  \se required

  \sa optional
*/
static PslNode_ptr
psl_node_sere_remove_ampersand(const NuSMVEnv_ptr env,
                               PslNode_ptr e, boolean* modified)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  *modified=false;

  if (e == PSL_NULL) return PSL_NULL;
  if (psl_node_is_leaf(e) || psl_node_is_id(e)) return e;
  if (psl_node_sere_is_propositional(e)) return e;

  /* recursion step */
  if (psl_node_sere_is_ampersand(e)) {

    /* found an ampersand */
    PslNode_ptr exp = psl_node_get_left(e); /* gets r1 & r2 */
    PslNode_ptr l=psl_node_get_left(exp); /* gets r1 */
    PslNode_ptr r=psl_node_get_right(exp); /* gets r2 */
    boolean lb=psl_node_sere_is_propositional(l);
    boolean rb=psl_node_sere_is_propositional(r);
    boolean lc=psl_node_sere_is_concat_holes_free(l);
    boolean rc=psl_node_sere_is_concat_holes_free(r);
    PslNode_ptr head_l;
    PslNode_ptr head_r;
    PslNode_ptr heads;

    /* if at least one ampersand argument is boolean (base
       expression), apply base rule: merge&(a, {b; r})={(a\and b); r} */

    /* gets the first element of both operands */
    if ((lb || lc) && (rb || rc)) {
      head_l = psl_node_sere_get_leftmost(l);
      head_r = psl_node_sere_get_leftmost(r);
      heads = psl_new_node(nodemgr, PSL_SERE,
                           psl_new_node(nodemgr, AND /* here is logical "and" */,
                                        psl_node_get_left(head_l), /* extracts proposition from atomic sere */
                                        psl_node_get_left(head_r) /* extracts proposition from atomic sere */
                                        ),
                           PSL_NULL);

      if (lb && rb) {
        *modified = true;
        return heads;
      }

      if ((lc && !lb) && (rc && !rb)) {
        /* are both concatenations, we need to keep the tails; since
           they are bot concatenations we are guaranteed the tails ar
           not null */
        PslNode_ptr tails_rec;
        PslNode_ptr tails = psl_new_node(nodemgr, PSL_SERECOMPOUND,
                                         psl_new_node(nodemgr, AND,
                                                      psl_node_prune(nodemgr, l, head_l),
                                                      psl_node_prune(nodemgr, r, head_r)),
                                         PSL_NULL);

        tails_rec = psl_node_sere_remove_ampersand(env, tails, modified);
        /* regardless the effects of the recursive call on tail, the
         *expression has been modified */
        *modified = true;
        return psl_new_node(nodemgr, PSL_SERECONCAT, heads, tails_rec);
      }

      /* here one is propositional and the other is "non atomic" concatenation */

      if (rc && !rb) { /* iff is_propositional(l) */
        *modified = true;
        return psl_new_node(nodemgr, PSL_SERECONCAT, heads, psl_node_prune(nodemgr, r, head_r));
      }

      if (lc && !lb) { /* iff is_propositional(r) */
        *modified = true;
        return psl_new_node(nodemgr, PSL_SERECONCAT, heads, psl_node_prune(nodemgr, l, head_l));
      }
    }
  }

  /* fallback rec rule */
  /* either the top level is not a &, or the two arguments are not concatenations */
  {
    boolean lm, rm;

    if (psl_node_sere_is_ampersand(e)) {
      /* special handling of & */
      PslNode_ptr exp = psl_node_get_left(e); /* gets r1 & r2 */
      PslNode_ptr lexp=psl_node_get_left(exp); /* gets r1 */
      PslNode_ptr rexp=psl_node_get_right(exp); /* gets r2 */
      PslNode_ptr lrec = psl_node_sere_remove_ampersand(env, lexp, &lm);
      PslNode_ptr rrec = psl_node_sere_remove_ampersand(env, rexp, &rm);

      if (lm||rm) {
        boolean m;
        PslNode_ptr rec = psl_node_sere_remove_ampersand(env, 
                                 psl_new_node(nodemgr, PSL_SERECOMPOUND,
                                              psl_new_node(nodemgr, AND, lrec, rrec),
                                              PSL_NULL),
                                 &m);
        *modified = m;
        return rec;
      }
      else return e;

    }
    else {
      PslNode_ptr l = psl_node_sere_remove_ampersand(env, psl_node_get_left(e), &lm);
      PslNode_ptr r = psl_node_sere_remove_ampersand(env, psl_node_get_right(e), &rm);

      *modified=(lm || rm);
      return psl_new_node(nodemgr, psl_node_get_op(e), l, r);
    }
  }
}

/*!
  \brief Resolves {a} && {a}

  

  \se required

  \sa optional
*/
static PslNode_ptr psl_node_sere_remove_2ampersand(const NuSMVEnv_ptr env,
                                                   PslNode_ptr e, boolean *modified)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
*modified=false;

  if (e == PSL_NULL) return PSL_NULL;

  if (psl_node_is_leaf(e) || psl_node_is_id(e)) return e;

  if (psl_node_get_op(e)==PSL_SERE &&
      PslNode_is_propositional(psl_node_get_left(e))) {
    return e;
  }

  /* recursion step */
  if (psl_node_sere_is_2ampersand(e)) {

    /* found an ampersand */
    PslNode_ptr exp = psl_node_get_left(e); /* gets r1 && r2 */
    PslNode_ptr l=psl_node_get_left(exp); /* gets r1 */
    PslNode_ptr r=psl_node_get_right(exp); /* gets r2 */
    boolean lb=psl_node_sere_is_propositional(l);
    boolean rb=psl_node_sere_is_propositional(r);
    boolean lc=psl_node_sere_is_concat_holes_free(l);
    boolean rc=psl_node_sere_is_concat_holes_free(r);

    /* it could be the case that one of the two arguments is FALSEEXP
       (as a result of a recursion) */
    if ((psl_node_get_op(l) == FALSEEXP) || (psl_node_get_op(r) == FALSEEXP)) {
      error_unreachable_code();
    }

    /* if both the two 2ampersand argument are boolean (base expression),
       apply base rule: merge&&(a, {b; r})=merge&&({a; r}, b)=false ,
       merge&&(a, b) = {a\and b} */
    if (lb && rb) {
      PslNode_ptr head_l = psl_node_sere_get_leftmost(l);
      PslNode_ptr head_r = psl_node_sere_get_leftmost(r);
      PslNode_ptr heads =
        psl_new_node(nodemgr, PSL_SERE,
                     psl_new_node(nodemgr, AND, /* here is logical "and" */
                                  psl_node_get_left(head_l), /* extracts proposition from atomic sere */
                                  psl_node_get_left(head_r) /* extracts proposition from atomic sere */
                                  ),
                     PSL_NULL);
      *modified=true;
      return heads;
    }

    /* if one of the two arguments is propositional and the other
       is a concatenation there is no use in proceeding */
    if ((lb && rc) || (lc && rb)) {
      *modified=true;
      return psl_new_node(nodemgr, PSL_SERE, psl_node_make_false(nodemgr), PSL_NULL);
    }

    /* this function assumes both arguments are concatenations */
    if (lc && rc) {
      /* if both ampersand arguments are non atomic concatenations ("; ") , apply
         recursive rule: merge&&({a; r_1}, {b; r_2})={(a\and b); merge&&(r_1, r_2)*/

      PslNode_ptr head_l = psl_node_sere_get_leftmost(l);
      PslNode_ptr head_r = psl_node_sere_get_leftmost(r);
      PslNode_ptr heads =
        psl_new_node(nodemgr, PSL_SERE,
                     psl_new_node(nodemgr, AND, /* here is logical "and" */
                                  psl_node_get_left(head_l), /* extracts proposition from atomic sere */
                                  psl_node_get_left(head_r) /* extracts proposition from atomic sere */
                                  ),
                     PSL_NULL);

      /* since arguments are bot concatenations we are guaranteed the tails ar not null */
      PslNode_ptr tails =
        psl_new_node(nodemgr, PSL_SERECOMPOUND,
                     psl_new_node(nodemgr, PSL_AMPERSANDAMPERSAND,
                                  psl_node_prune(nodemgr, l, head_l),
                                  psl_node_prune(nodemgr, r, head_r)),
                     PSL_NULL);

      *modified=true;
      return psl_new_node(nodemgr, PSL_SERECONCAT,
                          heads,
                          psl_node_sere_remove_2ampersand(env, tails, modified));


    }
  }

  /* fallback rec rule */
  /* either the top level is not a &&, or the two arguments are not concatenations */
  {
    boolean lm, rm;

    if (psl_node_sere_is_2ampersand(e)) {
      /* special handling of && */
      PslNode_ptr exp = psl_node_get_left(e); /* gets r1 && r2 */
      PslNode_ptr lexp=psl_node_get_left(exp); /* gets r1 */
      PslNode_ptr rexp=psl_node_get_right(exp); /* gets r2 */
      PslNode_ptr lrec = psl_node_sere_remove_2ampersand(env, lexp, &lm);
      PslNode_ptr rrec = psl_node_sere_remove_2ampersand(env, rexp, &rm);

      if (lm || rm) {
        boolean m;
        PslNode_ptr rec = psl_node_sere_remove_2ampersand(env, 
                                  psl_new_node(nodemgr, PSL_SERECOMPOUND,
                                               psl_new_node(nodemgr, PSL_AMPERSANDAMPERSAND, lrec, rrec),
                                               PSL_NULL),
                                  &m);
        *modified = *modified || m;
        return rec;
      }
      else return e;
    }
    else {
      PslNode_ptr l = psl_node_sere_remove_2ampersand(env, psl_node_get_left(e), &lm);
      PslNode_ptr r = psl_node_sere_remove_2ampersand(env, psl_node_get_right(e), &rm);

      *modified = (lm || rm);
      return psl_new_node(nodemgr, psl_node_get_op(e), l, r);
    }
  }
}

/*!
  \brief Resolves {a}:{a}

  

  \se required

  \sa optional
*/
static PslNode_ptr
psl_node_sere_remove_fusion(const NuSMVEnv_ptr env,
                            PslNode_ptr e, boolean *modified)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslNode_ptr l;
  PslNode_ptr r;

  *modified=false;

  if (e == PSL_NULL) return PSL_NULL;

  if (psl_node_is_leaf(e) || psl_node_is_id(e)) return e;

  if (psl_node_sere_is_propositional(e)) return e;

  /* if the top level operator is a : and the arguments are
     concatenation_or_fusion, then merges the arguments by conjoyning
     the righmost element of the left operand and the left most
     element of the right operand */
  l = psl_node_get_left(e);
  r = psl_node_get_right(e);
  if (psl_node_get_op(e)==PSL_SEREFUSION &&
      psl_node_sere_is_concat_fusion_holes_free(l) &&
      psl_node_sere_is_concat_fusion_holes_free(r)) {
    boolean lb = psl_node_sere_is_propositional(l);
    boolean rb = psl_node_sere_is_propositional(r);

    /* if both arguments are propositional, returns their conjunction */
    if (lb && rb) {
      *modified=true;
      return psl_new_node(nodemgr, PSL_SERE,
                          psl_new_node(nodemgr, AND, /* here is logical "and" */
                                       psl_node_get_left(psl_node_sere_get_leftmost(l)),
                                       psl_node_get_left(psl_node_sere_get_leftmost(r))),
                          PSL_NULL);

    }

    /* if only one argument is propositional, recurs on the other
       argument and then joins the first argument to the left most
       element of the result of the recursion */
    if (lb) {
      /* left is propositional, iff the other is not, i.e. is
         concat_or_fusion */
      boolean m;

      /* proposition a: */
      PslNode_ptr a = psl_node_sere_get_leftmost(l);
      PslNode_ptr rec = psl_node_sere_remove_fusion(env, r, &m);
      PslNode_ptr b = psl_node_sere_get_leftmost(rec); /* r head */
      PslNode_ptr head =
        psl_new_node(nodemgr, PSL_SERE,
                     psl_new_node(nodemgr, AND, /* here is logical "and" */
                                  psl_node_get_left(a), psl_node_get_left(b)),
                     PSL_NULL);

      PslNode_ptr tail = psl_node_prune(nodemgr, rec, b);

      *modified=true;

      /* recurring on one argument could make it propositional (e.g. {a:b} -> {a&b}),
         and its tail could be null */
      if (tail == PSL_NULL) return head;
      else return psl_new_node(nodemgr, PSL_SERECONCAT, head, tail);
    }

    if (rb) {
      /* right is propositional, iff the other is not, i.e. is
         concat_or_fusion */
      boolean m;

      /* proposition a: */
      PslNode_ptr a = psl_node_sere_get_leftmost(r);
      PslNode_ptr rec = psl_node_sere_remove_fusion(env, l, &m);
      PslNode_ptr b = psl_node_sere_get_rightmost(rec); /* r tail */
      PslNode_ptr tail =
        psl_new_node(nodemgr, PSL_SERE,
                     psl_new_node(nodemgr, AND, /* here is logical "and" */
                                  psl_node_get_left(b), psl_node_get_left(a)),
                     PSL_NULL);

      PslNode_ptr head = psl_node_prune(nodemgr, rec, b);

      *modified=true;

      /* recurring on one argument could make it propositional (e.g. {a:b} -> {a&b}),
         and its tail could be null */
      if (head == PSL_NULL) return tail;
      else return psl_new_node(nodemgr, PSL_SERECONCAT, head, tail);
    }

    /* both arguments are concat_or_fusion */
    {
      boolean m;

      /* recursive calls : the results do NOT contains : */
      PslNode_ptr l_rec = psl_node_sere_remove_fusion(env, l, &m);
      PslNode_ptr r_rec = psl_node_sere_remove_fusion(env, r, &m);

      /* the two elements to be merged */
      PslNode_ptr l_tail = psl_node_sere_get_rightmost(l_rec);
      PslNode_ptr r_head = psl_node_sere_get_leftmost(r_rec);

      /* the rest of the arguments */
      PslNode_ptr l_rest = psl_node_prune(nodemgr, l_rec, l_tail);
      PslNode_ptr r_rest = psl_node_prune(nodemgr, r_rec, r_head);

      PslNode_ptr merge_point =
        psl_new_node(nodemgr, PSL_SERE,
                     psl_new_node(nodemgr, AND, /* here is logical "and" */
                                  psl_node_get_left(l_tail),
                                  psl_node_get_left(r_head)),
                     PSL_NULL);
      if ((l_rest == PSL_NULL) && (r_rest == PSL_NULL)) {
        *modified=true;
        return merge_point;
      }

      if (l_rest == PSL_NULL) {
        *modified=true;
        return psl_new_node(nodemgr, PSL_SERECONCAT,
                            merge_point,
                            r_rest);
      }

      if (r_rest == PSL_NULL) {
        *modified=true;
        return psl_new_node(nodemgr, PSL_SERECONCAT,
                            l_rest,
                            merge_point);
      }

      *modified=m;
      return psl_new_node(nodemgr, PSL_SERECONCAT,
                          l_rest,
                          psl_new_node(nodemgr, PSL_SERECONCAT,
                                       merge_point,
                                       r_rest));
    }
  }

  /* fall back rule: if the top level operator is not a :, then
     continue on its arguments */
  {
    boolean lm, rm;

    PslNode_ptr l = psl_node_sere_remove_fusion(env, psl_node_get_left(e), &lm);
    PslNode_ptr r = psl_node_sere_remove_fusion(env, psl_node_get_right(e), &rm);
    PslNode_ptr result;

    *modified = lm || rm;
    result = psl_new_node(nodemgr, psl_node_get_op(e), l, r);

    return result;
  }
}

