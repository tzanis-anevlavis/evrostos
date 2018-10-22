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
  \brief Implementation of the PSL parser interface

  \todo: Missing description

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include <stdio.h>

#include "nusmv/core/parser/psl/pslExpr.h"
#include "nusmv/core/parser/psl/pslInt.h"
#include "nusmv/core/parser/psl/psl_grammar.h"

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/WordNumberMgr.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/
#define TOK2SYM(env, tok)  psl_conv_op(env, TOK2PSL, tok)

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
static void psl_expr_print_klass(FILE* file, SyntaxClass type);
static int psl_expr_is_boolean(const PslExpr psl);

static int
psl_expr_check_klass(const PslExpr psl, SyntaxClass expected);

static int psl_expr_base_num_to_val(char* base_num);

static void
psl_expr_require_klass(const NuSMVEnv_ptr env, const PslExpr psl, SyntaxClass expected);

static void psl_expr_is_valid_flproperty(const NuSMVEnv_ptr env,
                                         PslOp op_id,
                                         const PslExpr* left,
                                         const PslExpr* right);


/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

/* ---------- Creates: -------------------------------------------------

   OP_ID
   /  \
   right  Nil
   ---------------------------------------------------------------------- */
void psl_expr_make_unary_op(const NuSMVEnv_ptr env,
                            PslExpr* res,
                            const PslExpr* right,
                            const PslOp op_id,
                            const SyntaxClass right_req_klass,
                            const SyntaxClass res_klass)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  psl_expr_require_klass(env, *right, right_req_klass);

  res->klass = res_klass;

  if (op_id == TKMINUS) {
    res->psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKUMINUS), right->psl_node, PSL_NULL);
    return;
  }
  if (op_id == TKPLUS) {
    res->psl_node = right->psl_node;
    return;
  }

  /* takes the action here */
  res->psl_node = psl_new_node(nodemgr, TOK2SYM(env, op_id), right->psl_node, PSL_NULL);
}


/* ---------- Creates: -------------------------------------------------

   OP_ID
   /  \
   left   rigth
   ---------------------------------------------------------------------- */
void psl_expr_make_binary_op(const NuSMVEnv_ptr env,
                             PslExpr* res,
                             const PslExpr* left,
                             const PslOp op_id,
                             const PslExpr* right,
                             const SyntaxClass ops_req_klass,
                             const SyntaxClass res_klass)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  psl_expr_require_klass(env, *left, ops_req_klass);

  psl_expr_require_klass(env, *right, ops_req_klass);

  psl_expr_is_valid_flproperty(env, op_id, left, right);

  /* takes the action here */
  res->klass = res_klass;
  res->psl_node = psl_new_node(nodemgr, TOK2SYM(env, op_id),
                               left->psl_node, right->psl_node);
}

/* ---------- Creates: -------------------------------------------------

   OP_ID
   /  \
   left   rigth
   ---------------------------------------------------------------------- */
void psl_expr_make_binary_mixed_op(const NuSMVEnv_ptr env,
                                   PslExpr* res,
                                   const PslExpr* left,
                                   const PslOp op_id,
                                   const PslExpr* right,
                                   const SyntaxClass left_req_klass,
                                   const SyntaxClass right_req_klass,
                                   const SyntaxClass res_klass)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  psl_expr_require_klass(env, *left, left_req_klass);

  psl_expr_require_klass(env, *right, right_req_klass);

  psl_expr_is_valid_flproperty(env, op_id, left, right);

  /* takes the action here */
  res->klass = res_klass;
  res->psl_node = psl_new_node(nodemgr, TOK2SYM(env, op_id),
                               left->psl_node, right->psl_node);
}


/* ---------- Creates: -------------------------------------------------

   NEXT_OP_ID
   /     \
   fl_property   TKCOLON
   /   \
   when  bool_expr

   ... where any 'when' and 'bool_expr' can be NULL (see the grammar)
   ---------------------------------------------------------------------- */
void psl_expr_make_extended_next_op(const NuSMVEnv_ptr env,
                                    PslOp op_id,
                                    const PslExpr* fl_property,
                                    const PslExpr* when,
                                    const PslExpr* bool_expr,
                                    PslExpr* res)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr empty = psl_expr_make_empty();

  if (bool_expr != NULL)
    psl_expr_require_klass(env, *bool_expr, SC_BOOL_EXPR);

  if (when == NULL) when = &empty;
  if (bool_expr == NULL) bool_expr = &empty;

  res->klass = fl_property->klass;
  res->psl_node = psl_new_node(nodemgr, TOK2SYM(env, op_id), fl_property->psl_node,
                               psl_new_node(nodemgr, TOK2SYM(env, TKCOLON),
                                            when->psl_node,
                                            bool_expr->psl_node));
}


/* ---------- Creates: -------------------------------------------------
   op_id
   /    \
   id     TKIN
   /  \
   range   value_set

   ... where op_id can be TKFORALL|TKFORANY and range might be empty,
   and value_set can be either a list of values/ranges or the boolean
   type.
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_replicator(const NuSMVEnv_ptr env,
                                 PslOp op_id,
                                 PslExpr id, PslExpr range,
                                 PslExpr value_set)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;

  nusmv_assert(op_id == TKFORALL || op_id == TKFORANY);

  res.klass = SC_REPLICATOR;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, op_id), id.psl_node,
                              psl_new_node(nodemgr, TOK2SYM(env, TKIN), range.psl_node,
                                           value_set.psl_node));
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKREPLPROP
   /  \
   replicator   property
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_replicated_property(const NuSMVEnv_ptr env,
                                          PslExpr replicator, PslExpr expr)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;


  psl_expr_require_klass(env, replicator, SC_REPLICATOR);
  psl_expr_require_klass(env, expr, SC_PROPERTY);

  res.klass = expr.klass;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKREPLPROP), replicator.psl_node,
                              expr.psl_node);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKATOM
   /    \
   string   Nil
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_atom(const NuSMVEnv_ptr env,const char* str)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));


  UStringMgr_ptr strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
  PslExpr res;

  res.klass = SC_IDENTIFIER;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKATOM),
                              (PslNode_ptr) UStringMgr_find_string(strings, (char*) str), NULL);
  return res;
}

/* ---------- Creates: -------------------------------------------------
   TKIDENTIFIER
   /    \
   left   right

   ... where left can be TKATOM or TKARRAY,
   and right is in { TKIDENTIFIER, TKATOM, TKARRAY, TKNUMBER, Nil }
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_id(const NuSMVEnv_ptr env,PslExpr left, PslExpr right)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;

  res.klass = SC_IDENTIFIER;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKIDENTIFIER),
                              left.psl_node,
                              right.psl_node);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKARRAY
   /    \
   TKIDENTIFIER   TKNUMBER
   or TKATOM       or TKATOM
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_id_array(const NuSMVEnv_ptr env,PslExpr id, PslExpr idx)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;



  psl_expr_require_klass(env, id, SC_IDENTIFIER);
  psl_expr_require_klass(env, idx, SC_NUM_WORD_EXPR);

  res.klass = SC_IDENTIFIER;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKARRAY),
                              id.psl_node, idx.psl_node);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKCONTEXT
   /    \
   ctx     node
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_context(const NuSMVEnv_ptr env,PslExpr ctx, PslExpr node)
{
  PslExpr res;
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  psl_expr_require_klass(env, ctx, SC_IDENTIFIER);

  res.klass = SC_IDENTIFIER;
  res.psl_node = PslNode_new_context(nodemgr,
                                     psl_node_context_to_main_context(nodemgr, ctx.psl_node), node.psl_node);

  return res;
}


/* ---------- Creates: -------------------------------------------------
   Nil
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_empty()
{
  PslExpr res;
  res.klass = SC_NONE;
  res.psl_node = PSL_NULL;
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKTRUE
   /  \
   Nil   Nil
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_true(const NuSMVEnv_ptr env)
{
  PslExpr res;
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  res.klass = SC_BOOL_EXPR;
  res.psl_node = psl_node_make_true(nodemgr);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKFALSE
   /  \
   Nil   Nil
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_false(const NuSMVEnv_ptr env)
{
  PslExpr res;
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  res.klass = SC_BOOL_EXPR;
  res.psl_node = psl_node_make_false(nodemgr);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKINF
   /  \
   Nil   Nil
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_inf(const NuSMVEnv_ptr env)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;
  res.klass = SC_NUM_EXPR;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKINF), PSL_NULL, PSL_NULL);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKBOOLEAN
   /  \
   Nil   Nil
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_boolean_type(const NuSMVEnv_ptr env)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;
  res.klass = SC_NONE;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKBOOLEAN), PSL_NULL, PSL_NULL);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKTRUE              TKFALSE
   /  \       or       /   \
   Nil   Nil           Nil    Nil

   depending on the value of val
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_boolean_value(const NuSMVEnv_ptr env,int val)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;
  PslOp op;
  if (val == 0) op = TKFALSE;
  else op = TKTRUE;

  res.klass = SC_BOOL_EXPR;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, op), PSL_NULL, PSL_NULL);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKFAILURE
   /  \
   msg   kind
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_failure(const NuSMVEnv_ptr env,const char* msg, FailureKind kind)
{
  PslExpr res;
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  res.klass = SC_BOOL_EXPR;
  res.psl_node = psl_node_make_failure(nodemgr, msg, kind);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKNUMBER
   /   \
   val    Nil
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_number(const NuSMVEnv_ptr env,int val)
{
  PslExpr res;
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  res.klass = SC_NUM_EXPR;
  res.psl_node = psl_node_make_number(nodemgr, val);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKNUMBER
   /   \
   val    Nil
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_base_number(const NuSMVEnv_ptr env,char* base_num)
{
  return psl_expr_make_number(env, psl_expr_base_num_to_val(base_num));
}


/* ---------- Creates: -------------------------------------------------
   TKREALNUMBER
   /   \
   fval    Nil
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_real_number(const NuSMVEnv_ptr env,char* fval)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  UStringMgr_ptr strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));

  PslExpr res;
  res.klass = SC_NUM_EXPR;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKREALNUMBER),
                              (PslNode_ptr) UStringMgr_find_string(strings, fval), PSL_NULL);
  return res;
}

/* ---------- Creates: -------------------------------------------------
   TKWORDNUMBER
   /   \
   fval    Nil
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_word_number(const NuSMVEnv_ptr env,char* fval)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

  char* error;
  WordNumber_ptr number =
    WordNumberMgr_parsed_string_to_word_number(words, fval, &error);
  PslExpr res;
  int sign;

  if (WORD_NUMBER(NULL) == number) {
    ErrorMgr_rpterr(errmgr, "%s", error);
  }

  res.klass = SC_WORD_EXPR;

  sign = ('s' == fval[1] ? TKSIGNEDWORDNUMBER : TKUNSIGNEDWORDNUMBER);

  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, sign),
                              (PslNode_ptr) number, PSL_NULL);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKDOTDOT
   /  \
   low   high
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_range(const NuSMVEnv_ptr env,PslExpr low, PslExpr high)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;


  psl_expr_require_klass(env, low, SC_NUM_EXPR);
  psl_expr_require_klass(env, high, SC_NUM_EXPR);

  res.klass = SC_RANGE;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKDOTDOT), low.psl_node, high.psl_node);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKCASE
   /     \
   TKCOLON    \
   /  \      \
   cond  _then  _list

   ... where _list is either a similar structure or a FAILURE node
   as a terminator.

   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_case(const NuSMVEnv_ptr env,PslExpr cond, PslExpr _then, PslExpr _list)
{
  PslExpr res;
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  psl_expr_require_klass(env, cond, SC_BOOL_EXPR);
  psl_expr_require_klass(env, _then, SC_NUM_BOOL_WORD_EXPR);
  res.klass = _then.klass;
  res.psl_node = psl_node_make_case(nodemgr, cond.psl_node, _then.psl_node,
                                    _list.psl_node);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKITE
   /    \
   TKCOLON   \
   /  \      \
   cond  _then  _else

   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_ite(const NuSMVEnv_ptr env,PslExpr cond, PslExpr _then, PslExpr _else)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;



  psl_expr_require_klass(env, cond, SC_BOOL_EXPR);
  psl_expr_require_klass(env, _then, SC_NUM_BOOL_WORD_EXPR);
  psl_expr_require_klass(env, _else, SC_NUM_BOOL_WORD_EXPR);

  res.klass = _then.klass;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKITE),
                              psl_new_node(nodemgr, TOK2SYM(env, TKCOLON), cond.psl_node,
                                           _then.psl_node),
                              _else.psl_node);

  return res;
}



/* ---------- Creates: -------------------------------------------------
   op
   /    \
   TKCOLON Nil
   /   \
   seq   expr

   ... where op must be in {TKPIPEMINUSGT, TKPIPEEQGT}
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_suffix_implication_weak(const NuSMVEnv_ptr env,PslExpr seq, PslOp op,
                                              PslExpr expr)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  PslExpr res;

  nusmv_assert(op == TKPIPEMINUSGT || op == TKPIPEEQGT);



  psl_expr_require_klass(env, seq, SC_SEQUENCE);
  psl_expr_require_klass(env, expr, SC_FL_PROPERTY);

  res.klass = SC_FL_PROPERTY;

  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, op),
                              psl_new_node(nodemgr, TOK2SYM(env, TKCOLON), seq.psl_node,
                                           expr.psl_node),
                              PSL_NULL);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   op
   /  \
   TKCOLON  TKBANG
   / \
   seq   expr

   ... where op must be in {TKPIPEMINUSGT, TKPIPEEQGT}
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_suffix_implication_strong(const NuSMVEnv_ptr env,PslExpr seq, PslOp op,
                                                PslExpr expr)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;

  nusmv_assert(op == TKPIPEMINUSGT || op == TKPIPEEQGT);


  psl_expr_require_klass(env, seq, SC_SEQUENCE);
  psl_expr_require_klass(env, expr, SC_FL_PROPERTY);

  res.klass =  SC_FL_PROPERTY;

  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, op),
                              psl_new_node(nodemgr, TOK2SYM(env, TKCOLON), seq.psl_node,
                                           expr.psl_node),
                              psl_new_node(nodemgr, TOK2SYM(env, TKBANG),
                                           PSL_NULL, PSL_NULL));
  return res;
}


/* ---------- Creates: -------------------------------------------------
   op
   /  \
   TKCOMMA  seq
   /  \
   begin  end

   ... where op must be in {TKWITHINBANG, TKWITHIN, TKWITHINBANG_, TKWITHIN_}
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_within(const NuSMVEnv_ptr env,PslOp op, PslExpr begin, PslExpr end,
                             PslExpr seq)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;

  nusmv_assert(op == TKWITHINBANG || op == TKWITHIN ||
               op == TKWITHINBANG_ || op == TKWITHIN_);


  psl_expr_require_klass(env, begin, SC_SEQUENCE);
  psl_expr_require_klass(env, end, SC_BOOL_EXPR);
  psl_expr_require_klass(env, seq, SC_SEQUENCE);

  res.klass = SC_FL_PROPERTY;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, op),
                              psl_new_node(nodemgr, TOK2SYM(env, TKCOMMA), begin.psl_node,
                                           end.psl_node),
                              seq.psl_node);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   op
   /  \
   expr   seq

   ... where op must be in
   {TKWHILENOTBANG, TKWHILENOT, TKWHILENOTBANG_, TKWHILENOT_}
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_whilenot(const NuSMVEnv_ptr env,PslOp op, PslExpr expr, PslExpr seq)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;

  nusmv_assert(op == TKWHILENOTBANG || op == TKWHILENOT ||
               op == TKWHILENOTBANG_ || op == TKWHILENOT_);


  psl_expr_require_klass(env, expr, SC_BOOL_EXPR);
  psl_expr_require_klass(env, seq, SC_SEQUENCE);

  res.klass = SC_FL_PROPERTY;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, op), expr.psl_node, seq.psl_node);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKABORT
   /   \
   fl_prop   cond
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_abort(const NuSMVEnv_ptr env,PslExpr fl_prop, PslExpr cond)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;


  psl_expr_require_klass(env, fl_prop, SC_FL_PROPERTY);
  psl_expr_require_klass(env, cond, SC_BOOL_EXPR);

  res.klass = SC_FL_PROPERTY;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKABORT),
                              fl_prop.psl_node, cond.psl_node);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKSERE
   /  \
   expr   Nil
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_sere(const NuSMVEnv_ptr env,PslExpr expr)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;


  psl_expr_require_klass(env, expr, SC_SEQUENCE);

  res.klass = SC_SEQUENCE;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKSERE), expr.psl_node, PSL_NULL);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKSERECONCAT
   /      \
   seq1     seq2
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_sere_concat(const NuSMVEnv_ptr env,PslExpr seq1, PslExpr seq2)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;


  psl_expr_require_klass(env, seq1, SC_SEQUENCE);
  psl_expr_require_klass(env, seq2, SC_SEQUENCE);

  res.klass = SC_SEQUENCE;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKSERECONCAT), seq1.psl_node,
                              seq2.psl_node);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKSEREFUSION
   /      \
   seq1     seq2
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_sere_fusion(const NuSMVEnv_ptr env,PslExpr seq1, PslExpr seq2)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;


  psl_expr_require_klass(env, seq1, SC_SEQUENCE);
  psl_expr_require_klass(env, seq2, SC_SEQUENCE);

  res.klass = SC_SEQUENCE;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKSEREFUSION),
                              seq1.psl_node, seq2.psl_node);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKSERECOMPOUND
   /    \
   op      Nil
   /  \
   seq1   seq2

   ... where op must be in {TKAMPERSANDAMPERSAND, TKAMPERSAND, TKPIPE}
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_sere_compound_binary_op(const NuSMVEnv_ptr env,PslExpr seq1, PslOp op,
                                              PslExpr seq2)
{
  PslExpr res;
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  nusmv_assert(op==TKAMPERSANDAMPERSAND || op==TKAMPERSAND || op==TKPIPE);

  psl_expr_require_klass(env, seq1, SC_SEQUENCE);
  psl_expr_require_klass(env, seq2, SC_SEQUENCE);

  res.klass = SC_SEQUENCE;
  res.psl_node = psl_node_make_sere_compound(nodemgr, seq1.psl_node, TOK2SYM(env, op),
                                             seq2.psl_node);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKSEREREPEATED
   /         \
   op         count
   /  \
   sere  Nil

   ... where op must be in {TKLBSPLAT, TKLBPLUSRB, TKLBEQ, TKLBMINUSGT}.

   count can be Nil.
   sere can be Nil.

   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_repeated_sere(const NuSMVEnv_ptr env,PslOp op, PslExpr sere, PslExpr count)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;

  nusmv_assert(op==TKLBSPLAT || op==TKLBPLUSRB ||
               op==TKLBEQ || op==TKLBMINUSGT);

  if (sere.klass != SC_NONE)
    psl_expr_require_klass(env, sere, SC_SEQUENCE);
  if (count.klass != SC_NONE) psl_expr_require_klass(env, count, SC_NUM_RANGE);

  res.klass = SC_SEQUENCE;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKSEREREPEATED),
                              psl_new_node(nodemgr, TOK2SYM(env, op), sere.psl_node,
                                           PSL_NULL),
                              count.psl_node);
  return res;

}


/* ---------- Creates: -------------------------------------------------
   TKCONS
   /    \
   a        b

   The resulting type is SC_LIST
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_cons(const NuSMVEnv_ptr env, PslExpr a, PslExpr b)
{
  PslExpr res;
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  res.klass = SC_LIST;
  res.psl_node = psl_node_make_cons(nodemgr, a.psl_node, b.psl_node);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKCONS
   /    \
   a        b

   The resulting type is SC_LIST. A new node is created.
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_cons_new(const NuSMVEnv_ptr env,PslExpr a, PslExpr b)
{
  PslExpr res;
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  res.klass = SC_LIST;
  res.psl_node = psl_node_make_cons_new(nodemgr, a.psl_node, b.psl_node);
  return res;
}

/* ---------- Creates: -------------------------------------------------
   TKCONCATENATION
   /         \
   expr_list     Nil

   ... where expr_list is a list in the form
   CONS(expr1, CONS(expr2, ... , CONS(exprN, Nil)...))
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_concatenation(const NuSMVEnv_ptr env,PslExpr expr_list)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;


  psl_expr_require_klass(env, expr_list, SC_LIST);
  res.klass = SC_LIST;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKCONCATENATION),
                              expr_list.psl_node, PSL_NULL);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKCONCATENATION
   /           \
   expr_list    expr

   ... where expr_list is a list in the form
   CONS(expr1, CONS(expr2, ... , CONS(exprN, Nil)...))
   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_multiple_concatenation(const NuSMVEnv_ptr env,PslExpr expr,
                                             PslExpr expr_list)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;

  psl_expr_require_klass(env, expr_list, SC_LIST);
  res.klass = SC_LIST;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKCONCATENATION),
                              expr_list.psl_node, expr.psl_node);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   op
   /  \
   expr   Nil

   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_obe_unary(const NuSMVEnv_ptr env,PslOp op, PslExpr expr)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;


  psl_expr_require_klass(env, expr, SC_OBE_PROPERTY);

  res.klass = SC_OBE_PROPERTY;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, op), expr.psl_node, PSL_NULL);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   op
   /  \
   left   right

   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_obe_binary(const NuSMVEnv_ptr env,PslExpr left, PslOp op, PslExpr right)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;


  psl_expr_require_klass(env, left, SC_OBE_PROPERTY);
  psl_expr_require_klass(env, right, SC_OBE_PROPERTY);

  res.klass = SC_OBE_PROPERTY;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, op), left.psl_node, right.psl_node);
  return res;
}


/* ---------- Creates: -------------------------------------------------
   TKBITSELECTION
   /         \
   word_expr   TKCOLON
   /  \
   left   right

   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_bit_selection(const NuSMVEnv_ptr env,PslExpr word_expr,
                                    PslExpr left,  PslExpr right)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;

  psl_expr_require_klass(env, word_expr, SC_WORD_EXPR);
  psl_expr_require_klass(env, left, SC_NUM_WORD_EXPR);
  psl_expr_require_klass(env, right, SC_NUM_WORD_EXPR);

  res.klass = SC_WORD_EXPR;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKWSELECT), word_expr.psl_node,
                              psl_new_node(nodemgr, TOK2SYM(env, TKCOLON),
                                           left.psl_node,
                                           right.psl_node));
  return res;
}

/* ---------- Creates: -------------------------------------------------
   TKCONCATENATION
   /  \
   left   right

   ---------------------------------------------------------------------- */
PslExpr psl_expr_make_word_concatenation(const NuSMVEnv_ptr env, PslExpr left,  PslExpr right)
{
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  PslExpr res;

  psl_expr_require_klass(env, left, SC_WORD_EXPR);
  psl_expr_require_klass(env, right, SC_NUM_WORD_EXPR);

  res.klass = SC_WORD_EXPR;
  res.psl_node = psl_new_node(nodemgr, TOK2SYM(env, TKCONCATENATION),
                              left.psl_node,
                              right.psl_node);
  return res;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief required

  optional

  \se required

  \sa optional
*/
static void psl_expr_print_klass(FILE* file, SyntaxClass type)
{
  char* name;

  switch (type) {
  case SC_NUM_EXPR: name = "Numeric"; break;
  case SC_BOOL_EXPR: name = "Boolean"; break;
  case SC_IDENTIFIER: name = "Identifier"; break;
  case SC_NUM_BOOL_WORD_EXPR: name = "Numeric or Boolean or Word"; break;
  case SC_NUM_BOOL_EXPR: name = "Numeric or Boolean"; break;
  case SC_NUM_WORD_EXPR: name = "Numeric or Word"; break;
  case SC_BOOL_WORD_EXPR: name = "Boolean or Word"; break;
  case SC_WORD_EXPR: name = "Word"; break;
  case SC_PROPERTY: name = "Property"; break;
  case SC_FL_PROPERTY: name = "FL Property"; break;
  case SC_OBE_PROPERTY: name = "OBE Property"; break;
  case SC_SEQUENCE: name = "Sequence"; break;
  case SC_REPLICATOR: name = "Replicator"; break;
  case SC_RANGE: name = "Range"; break;
  case SC_NONE: name = "None"; break;
  case SC_LIST: name = "List"; break;
  case SC_NUM_RANGE: name = "Numeric or Range"; break;
  default: name = "Unknown";
  }

  fprintf(file, "%s", name);
}

/*!
  \brief Returns 1 if the given node is boolean compatible type, 0
   otherwise

  optional

  \sa optional
*/
static int psl_expr_is_boolean(const PslExpr psl)
{
  if ((psl.klass == SC_BOOL_EXPR)     ||
      (psl.klass == SC_IDENTIFIER)    ||
      (psl.klass == SC_NUM_BOOL_WORD_EXPR) ||
      (psl.klass == SC_NUM_BOOL_EXPR) ||
      (psl.klass == SC_BOOL_WORD_EXPR) ||
      (psl.klass == SC_PROPERTY)      ||
      (psl.klass == SC_OBE_PROPERTY)  ||
      (psl.klass == SC_FL_PROPERTY)) return 1;

  if (psl_node_is_number(psl.psl_node)) {
    /* checks whether it is a "boolean" constant number */
    if ( (psl_node_number_get_value(psl.psl_node) == 0) ||
         (psl_node_number_get_value(psl.psl_node) == 1) ) return 1;
  }

  return 0;
}

/*!
  \brief returns 0 if the given psl expr is not compatible with the
   given klass

  optional

  \sa optional
*/
static int psl_expr_check_klass(const PslExpr psl, SyntaxClass expected)
{
  int res = (expected == psl.klass);

  switch (expected) {
  case SC_NUM_EXPR:
    res = res || (psl.klass == SC_IDENTIFIER);
    break;

  case SC_BOOL_EXPR:
    res = res || psl_expr_is_boolean(psl);
    break;

  case SC_WORD_EXPR:
    res = (res || (psl.klass == SC_IDENTIFIER) ||
           (psl.klass == SC_NUM_WORD_EXPR));
    break;

  case SC_NUM_BOOL_EXPR:
    res = (res || (psl.klass == SC_NUM_EXPR) ||
           psl_expr_is_boolean(psl) || (psl.klass == SC_BOOL_WORD_EXPR));
    break;

  case SC_NUM_WORD_EXPR:
    res = (res || (psl.klass == SC_NUM_EXPR) ||
           (psl.klass == SC_WORD_EXPR) || (psl.klass == SC_IDENTIFIER) ||
           (psl.klass == SC_BOOL_WORD_EXPR));
    break;

  case SC_NUM_BOOL_WORD_EXPR:
    res = (res || (psl.klass == SC_NUM_EXPR) || psl_expr_is_boolean(psl) ||
           (psl.klass == SC_WORD_EXPR) || (psl.klass == SC_NUM_BOOL_EXPR) ||
           (psl.klass == SC_NUM_WORD_EXPR));
    break;

  case SC_BOOL_WORD_EXPR:
    res = (res || (psl.klass == SC_WORD_EXPR) ||
           psl_expr_is_boolean(psl) || (psl.klass == SC_NUM_WORD_EXPR));
    break;

  case SC_IDENTIFIER:
    break;

  case SC_FL_PROPERTY:
    res = (res || (psl.klass == SC_IDENTIFIER) ||
           (psl.klass == SC_BOOL_EXPR) || (psl.klass == SC_BOOL_WORD_EXPR) ||
           (psl.klass == SC_SEQUENCE));
    break;

  case SC_OBE_PROPERTY:
    res = res || (psl.klass == SC_IDENTIFIER) || (psl.klass == SC_BOOL_EXPR) ||
      (psl.klass == SC_BOOL_WORD_EXPR);
    break;

  case SC_PROPERTY:
    res = res || (psl.klass == SC_IDENTIFIER) || (psl.klass == SC_BOOL_EXPR)
      || (psl.klass == SC_FL_PROPERTY) || (psl.klass == SC_OBE_PROPERTY)  ||
      (psl.klass == SC_BOOL_WORD_EXPR);
    break;

  case SC_REPLICATOR:
    break;

  case SC_SEQUENCE:
    res = (res ||
           (psl_expr_is_boolean(psl) &&
            PslNode_is_propositional(psl.psl_node)));
    break;

  case SC_LIST:
    break;

  case SC_RANGE:
    break;

  case SC_NUM_RANGE:
    res = (res || (psl.klass == SC_IDENTIFIER) ||
           (psl.klass == SC_NUM_EXPR) || (psl.klass == SC_RANGE));
    break;

  default:
    res = 0;
  }

  return res;
}

/*!
  \brief Converts from base to number: TO BE IMPLEMENTED

  optional

  \sa optional
*/
static int psl_expr_base_num_to_val(char* base_num)
{
  UNUSED_PARAM(base_num);

  error_unreachable_code(); /* Cannot be called (not implemented) */
  return 0;
}

/*!
  \brief Checks that given expression is compatible with the
   given required syntactic class

  optional

  \sa optional
*/
static void psl_expr_require_klass(const NuSMVEnv_ptr env, const PslExpr psl, SyntaxClass expected)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (psl_expr_check_klass(psl, expected) == 0) {
    StreamMgr_print_error(streams,  "\nType error: expected '");
    psl_expr_print_klass(stderr, expected);
    StreamMgr_print_error(streams,  "', got '");
    psl_expr_print_klass(stderr, psl.klass);
    StreamMgr_print_error(streams,  "' instead.\n");
    psl_yyerror("wrong type.");
    ErrorMgr_nusmv_exit(errmgr, 1);
  }
}

/*!
  \brief Checks that there is no violation of types for
   operators '=', '!=' and '==', that cannot be
   used with FL_PROPERTY expressions


*/
static void psl_expr_is_valid_flproperty(const NuSMVEnv_ptr env,
                                         PslOp op_id,
                                         const PslExpr* left,
                                         const PslExpr* right)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (((TKEQ == op_id) || (TKEQEQ == op_id) || (TKBANGEQ == op_id)) &&
      ((SC_FL_PROPERTY == left->klass) || (SC_FL_PROPERTY == right->klass))) {
    StreamMgr_print_error(streams,  "\nType error: unexpected expression type '");
    psl_expr_print_klass(stderr, SC_FL_PROPERTY);
    StreamMgr_print_error(streams,  "', used with operator '=' or '!=' or '=='.");
    ErrorMgr_nusmv_exit(errmgr, 1);
  }
}
