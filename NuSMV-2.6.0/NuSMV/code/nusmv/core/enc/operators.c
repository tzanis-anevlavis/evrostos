/* ---------------------------------------------------------------------------


  This file is part of the ``enc'' package of NuSMV version 2.
  Copyright (C) 2003 by FBK-irst.

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
  \author Marco Roveri, Roberto Cavada
  \brief These operators are used by dd package

  Functions like add_plus, add_equal, etc., call these operators

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/Pair.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/enc/operators.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/utils/WordNumberMgr.h"

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
typedef int (*INTPFII)(int, int);


/*---------------------------------------------------------------------------*/
/* macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
extern int nusmv_yylineno;

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static node_ptr
node_word_full_adder(node_ptr ai, node_ptr bi, node_ptr carry_in,
                     node_ptr* carry_out, const NuSMVEnv_ptr env);

static node_ptr node_word_unsigned_divide_reminder(node_ptr a, node_ptr b,
                                                   node_ptr* reminder,
                                                   const NuSMVEnv_ptr env);
static node_ptr
node_word_signed_divide_reminder_simple(node_ptr a, node_ptr b,
                                        node_ptr* reminder,
                                        const NuSMVEnv_ptr env);
static node_ptr
node_word_signed_divide_reminder_hardware(node_ptr a, node_ptr b,
                                          node_ptr* reminder,
                                          const NuSMVEnv_ptr env);

static node_ptr node_word_signed_op(node_ptr a, node_ptr b,
                                    node_ptr (*op)(node_ptr, node_ptr, const NuSMVEnv_ptr),
                                    const NuSMVEnv_ptr env);

static boolean _is_bool(const node_ptr a);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

node_ptr node_and(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if (_is_bool(n1) && _is_bool(n2)) {
    return ExprMgr_and(exprs, n1, n2);
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, "&", n1, n2);

  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_or(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if (_is_bool(n1) && _is_bool(n2)) {
    return ExprMgr_or(exprs, n1, n2);
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, "|", n1, n2);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_not(node_ptr n, node_ptr this_node_is_not_used, const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (node_get_type(n) == FAILURE) return n; /* error in previous expr */

  if (_is_bool(n)) {
    return ExprMgr_not(exprs, n);
  }
  else ErrorMgr_error_not_proper_number(errmgr, "!", n);
  return (node_ptr) NULL;/* return something to suppress warnings */
}

node_ptr node_iff(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if (_is_bool(n1) && _is_bool(n2)) {
    return ExprMgr_iff(exprs, n1, n2);
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, "<->", n1, n2);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_xor(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if (_is_bool(n1) && _is_bool(n2)) {
    return ExprMgr_xor(exprs, n1, n2);
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, "xor", n1, n2);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_implies(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if (_is_bool(n1) && _is_bool(n2)) {
    return ExprMgr_implies(exprs, n1, n2);
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, "implies", n1, n2);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_equal(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if(n1 == n2) return(ExprMgr_true(exprs));

  return(ExprMgr_false(exprs));
}

node_ptr node_not_equal(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if(n1 == n2) return(ExprMgr_false(exprs));

  return(ExprMgr_true(exprs));
}

node_ptr node_lt(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if (node_get_type(n1) == NUMBER && node_get_type(n2) == NUMBER) {
    return (NODE_TO_INT(car(n1)) < NODE_TO_INT(car(n2))) ?
      ExprMgr_true(exprs) : ExprMgr_false(exprs);
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, "<", n1, n2);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_gt(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if (node_get_type(n1) == NUMBER && node_get_type(n2) == NUMBER) {
    return NODE_TO_INT(car(n1)) > NODE_TO_INT(car(n2)) ?
      ExprMgr_true(exprs) : ExprMgr_false(exprs);
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, ">", n1, n2);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_le(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if (node_get_type(n1) == NUMBER && node_get_type(n2) == NUMBER) {
    return (NODE_TO_INT(car(n1)) <= NODE_TO_INT(car(n2))) ?
      ExprMgr_true(exprs) : ExprMgr_false(exprs);
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, "<=", n1, n2);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_ge(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if (node_get_type(n1) == NUMBER && node_get_type(n2) == NUMBER) {
    return NODE_TO_INT(car(n1)) >= NODE_TO_INT(car(n2)) ?
      ExprMgr_true(exprs) : ExprMgr_false(exprs);
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, ">=", n1, n2);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_unary_minus(node_ptr n, node_ptr this_node_is_not_used, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (node_get_type(n) == FAILURE) return n; /* error in previous expr */

  if (node_get_type(n) == NUMBER) {
    return find_node(nodemgr, NUMBER, NODE_FROM_INT(-NODE_TO_INT(car(n))), Nil);
  }
  else ErrorMgr_error_not_proper_number(errmgr, "- (unary)", n);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_plus(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if (node_get_type(n1) == NUMBER && node_get_type(n2) == NUMBER) {
    return find_node(nodemgr, NUMBER,
             NODE_FROM_INT(NODE_TO_INT(car(n1)) + NODE_TO_INT(car(n2))), Nil);
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, "+", n1, n2);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_minus(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if (node_get_type(n1) == NUMBER && node_get_type(n2) == NUMBER) {
    return  find_node(nodemgr, NUMBER,
              NODE_FROM_INT(NODE_TO_INT(car(n1)) - NODE_TO_INT(car(n2))), Nil);
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, "-", n1, n2);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_times(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if (node_get_type(n1) == NUMBER && node_get_type(n2) == NUMBER) {
    return find_node(nodemgr, NUMBER,
             NODE_FROM_INT(NODE_TO_INT(car(n1)) * NODE_TO_INT(car(n2))), Nil);
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, "*", n1, n2);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_divide(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  /* check the second operand for being zero */
  if (node_get_type(n2) == NUMBER && 0 == NODE_TO_INT(car(n2))) {
    return ErrorMgr_failure_make(errmgr, "Division by zero", FAILURE_DIV_BY_ZERO, nusmv_yylineno);
  }

  if (node_get_type(n1) == NUMBER && node_get_type(n2) == NUMBER) {
    /* here is the check whether usual ANSI semantics of division is used */
    if (opt_use_ansi_c_div_op(opts)) {
      return find_node(nodemgr, NUMBER,
              NODE_FROM_INT(NODE_TO_INT(car(n1)) / NODE_TO_INT(car(n2))), Nil);
    }
    /* the semantics of the division is from old version of NuSMV */
    else {
      int a = NODE_TO_INT(car(n1));
      int b = NODE_TO_INT(car(n2));
      int r = a % b;
      int result = a/b - (r < 0);/*IF r < 0 THEN a/b - 1 ELSE a/b */
      return find_node(nodemgr, NUMBER, NODE_FROM_INT(result), Nil);
    }
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, "/", n1, n2);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_mod(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  /* check the second operand for being zero */
  if (node_get_type(n2) == NUMBER && 0 == NODE_TO_INT(car(n2))) {
    return ErrorMgr_failure_make(errmgr, "Division by zero", FAILURE_DIV_BY_ZERO, nusmv_yylineno);
  }

  if (node_get_type(n1) == NUMBER && node_get_type(n2) == NUMBER) {
    /* here is the check whether usual ANSI semantics of division is used */
    if (opt_use_ansi_c_div_op(opts)) {
      return find_node(nodemgr, NUMBER,
         NODE_FROM_INT(NODE_TO_INT(car(n1)) % NODE_TO_INT(car(n2))), Nil);
    }
    /* the semantics of the division is from old version of NuSMV */
    else {
      int a = NODE_TO_INT(car(n1));
      int b = NODE_TO_INT(car(n2));
      int r = a % b;
      if (r < 0) r += b;
      return find_node(nodemgr, NUMBER, NODE_FROM_INT(r), Nil);
    }
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, "mod", n1, n2);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_bit_range(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (node_get_type(n1) == FAILURE) return n1; /* error in previous expr */
  if (node_get_type(n2) == FAILURE) return n2; /* error in previous expr */

  if (node_get_type(n1) == NUMBER && node_get_type(n2) == NUMBER) {
    return find_node(nodemgr, RANGE, n1, n2);
  }
  else ErrorMgr_error_not_proper_numbers(errmgr, "bit-selection-range", n1, n2);
  return (node_ptr)NULL;/* return something to suppress warnings */
}

node_ptr node_union(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  node_ptr tmp;

  if(n1 == Nil) return(n2);
  if(n2 == Nil) return(n1);

  if (node_get_type(n1) == FAILURE) return n1; /* error in operand */
  if (node_get_type(n2) == FAILURE) return n2; /* error in operand */

  /* convert singleton elements to lists */
  if(node_get_type(n1) != CONS) n1 = find_node(nodemgr, CONS, n1, Nil);
  if(node_get_type(n2) != CONS) n2 = find_node(nodemgr, CONS, n2, Nil);

  /* create a list merged from the given lists and
     with all elements ordered (less-comparison)
  */
  tmp = Nil;
  while (n1 != Nil && n2 != Nil) {
    if (car(n1) == car(n2)) {
      tmp = cons(nodemgr, car(n1), tmp);
      n1 = cdr(n1);
      n2 = cdr(n2);
    }
    else if (car(n1) < car(n2)) {/* < is used because the list will be reversed */
      tmp = cons(nodemgr, car(n1), tmp);
      n1 = cdr(n1);
    }
    else { /*car(n2) > car(n1) */
      tmp = cons(nodemgr, car(n2), tmp);
      n2 = cdr(n2);
    }
  }
  if (Nil == n1) n1 = n2; /* remaining elements (they were in n1 or n2) */

  /* reverse the obtained list and apply find_node. The result will be in n1 */
  while (Nil != tmp) {
    n1 = find_node(nodemgr, CONS, car(tmp), n1);

    n2 =  cdr(tmp);
    free_node(nodemgr, tmp);
    tmp = n2;
  }
  return n1;
}

node_ptr node_setin(node_ptr n1, node_ptr n2, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  node_ptr iter1;
  node_ptr iter2;

  if (node_get_type(n1) == FAILURE) return n1; /* error in operand */
  if (node_get_type(n2) == FAILURE) return n2; /* error in operand */

  /* convert singleton elements to lists */
  if (CONS != node_get_type(n1)) n1 = find_node(nodemgr, CONS, n1, Nil);
  if (CONS != node_get_type(n2)) n2 = find_node(nodemgr, CONS, n2, Nil);

  /* check that every element of n1 is equal to some element of n2 */
  for (iter1 = n1; iter1 != Nil; iter1 = cdr(iter1)) {
    for (iter2 = n2; iter2 != Nil; iter2 = cdr(iter2)) {
      if (car(iter1) == car(iter2)) break; /* there is equality */
    }
    /* one of the elements of n1 is not equal to any elements of n2 */
    if (iter2 == Nil) return ExprMgr_false(exprs);
  }
  return ExprMgr_true(exprs);
}


/* ---------------------------------------------------------------------- */
/*   WORDS releated (encoded as node_ptr)                                 */
/* ---------------------------------------------------------------------- */

node_ptr node_word_create(node_ptr bitval, size_t w, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr width = find_node(nodemgr, NUMBER, NODE_FROM_INT(w), Nil);
  node_ptr enc = Nil;
  for (;w>0; --w) { enc = find_node(nodemgr, CONS, bitval, enc); }

  return find_node(nodemgr, UNSIGNED_WORD, enc, width);
}

node_ptr node_word_create_from_list(node_ptr l, size_t w,
                                    const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  nusmv_assert(node_get_type(l) == CONS);
  nusmv_assert(llength(l) == w);

  return find_node(nodemgr, UNSIGNED_WORD, l, find_node(nodemgr, NUMBER, NODE_FROM_INT(w), Nil));
}

node_ptr node_word_create_from_wordnumber(WordNumber_ptr wn, const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr bits;
  int w, i;

  w = WordNumber_get_width(wn);
  bits = Nil;
  for (i=0; i<w; ++i) {
    bits = find_node(nodemgr, CONS,
                     WordNumber_get_bit(wn, i)
                     ? ExprMgr_true(exprs) : ExprMgr_false(exprs),
                     bits);
  }

  return node_word_create_from_list(bits, w, env);
}

node_ptr node_word_create_from_integer(unsigned long long value,
                                       size_t width,
                                       const NuSMVEnv_ptr env)
{
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

  WordNumber_ptr wn = WordNumberMgr_integer_to_word_number(words, value, width);

  return node_word_create_from_wordnumber(wn, env);
}

node_ptr node_word_create_from_array(array_t* arr, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr res = Nil;
  node_ptr bit;
  unsigned int i;

  arrayForEachItem(node_ptr, arr, i, bit) { res = find_node(nodemgr, CONS, bit, res); }
  return node_word_create_from_list(res, array_n(arr), env);
}

size_t node_word_get_width(node_ptr w)
{
  return node_get_int(cdr(w));
}

array_t* node_word_to_array(node_ptr w)
{
  int wid, i;
  array_t* res;
  node_ptr iter;

  _CHECK_WORD(w);

  wid = node_get_int(cdr(w));
  res = array_alloc(node_ptr, wid);
  for (i=wid-1, iter=car(w); i>=0; --i, iter=cdr(iter)) {
    array_insert(node_ptr, res, i, car(iter));
  }

  return res;
}


/*!
  \brief Private helpers for node_word_apply_unary
*/

static node_ptr _node_word_apply_unary_aux(node_ptr e, void* arg)
{
  Pair_ptr pair = PAIR(arg);

  const NuSMVEnv_ptr env = NUSMV_ENV(Pair_get_first(pair));
  int apply_op = PTR_TO_INT(Pair_get_second(pair));

  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  /* this optimizes the result when possible */
  switch (apply_op) {
  case NOT: return ExprMgr_not(exprs, e);
  default: return find_node(nodemgr, apply_op, e, Nil);
  }
}

/*!
  \brief Private helper for node_word_apply_attime

  Helper for node_word_apply_attime

  \se None
*/

static node_ptr _node_word_apply_attime_aux(node_ptr e, void* arg)
{
  const Pair_ptr pair = PAIR(arg);
  const NuSMVEnv_ptr env = NUSMV_ENV(Pair_get_first(pair));
  const int time = PTR_TO_INT(Pair_get_second(pair));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  return ExprMgr_attime(exprs, e, time, SYMB_TABLE(NULL));
}

static node_ptr _node_word_apply_binary_aux(node_ptr e1, node_ptr e2,
                                            int op,
                                            const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  /* this optimizes the result when possible */
  switch (op) {
  case AND: return ExprMgr_and(exprs, e1, e2);
  case OR: return ExprMgr_or(exprs, e1, e2);
  case XOR: return ExprMgr_xor(exprs, e1, e2);
  case XNOR: return ExprMgr_xnor(exprs, e1, e2);
  case IMPLIES: return ExprMgr_implies(exprs, e1, e2);
  case IFF: return ExprMgr_iff(exprs, e1, e2);
  default: return find_node(nodemgr, op, e1, e2);
  }
}

node_ptr map2_param(node_ptr (*fun)(node_ptr, node_ptr, int, const NuSMVEnv_ptr),
                    node_ptr l1, node_ptr l2, int op, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr res = Nil;

  while (l1 != Nil) {
    node_ptr t;
    nusmv_assert(l2 != Nil);

    t = (node_ptr)(*fun)(car(l1), car(l2), op, env);
    res = cons(nodemgr, t, res);

    l1 = cdr(l1); l2 = cdr(l2);
  }

  nusmv_assert(l2 == Nil);

  return reverse(res);
}

node_ptr node_word_apply_unary(node_ptr wenc, int op, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr res = Nil;
  Pair pair;

  Pair_init(&pair, env, VOIDPTR_FROM_INT(op));

  _CHECK_WORD(wenc);

  /* here "map" creates new nodes which are not freed. A possible
     solution is to create an alternative to "map", which would use
     find_node */
  res = map_param(nodemgr, _node_word_apply_unary_aux, car(wenc), &pair);

  return find_node(nodemgr, UNSIGNED_WORD, res, cdr(wenc));
}

node_ptr node_word_apply_attime(node_ptr wenc, int time, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr res;
  Pair pair;

  Pair_init(&pair, env, VOIDPTR_FROM_INT(time));

  _CHECK_WORD(wenc);

  res = map_param(nodemgr, _node_word_apply_attime_aux, car(wenc), &pair);

  return find_node(nodemgr, UNSIGNED_WORD, res, cdr(wenc));
}

node_ptr node_word_apply_binary(node_ptr wenc1, node_ptr wenc2, int op,
                                const NuSMVEnv_ptr env)
{
  node_ptr res;
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  _CHECK_WORDS(wenc1, wenc2);

  res = map2_param(_node_word_apply_binary_aux, car(wenc1), car(wenc2), op, env);
  return find_node(nodemgr, UNSIGNED_WORD, res, cdr(wenc1));
}

node_ptr node_word_make_conjuction(node_ptr w, const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  node_ptr res = ExprMgr_true(exprs);
  for (w=car(w); w != Nil; w=cdr(w)) res = ExprMgr_and(exprs, car(w), res);
  return res;
}

node_ptr node_word_make_disjunction(node_ptr w, const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  node_ptr res = ExprMgr_false(exprs);
  for (w=car(w); w != Nil; w=cdr(w)) res = ExprMgr_or(exprs, car(w), res);
  return res;
}

node_ptr node_word_cast_bool(node_ptr w, const NuSMVEnv_ptr env)
{
  _CHECK_WORD(w);
  nusmv_assert(node_get_int(cdr(w)) == 1);
  return car(car(w));
}

node_ptr node_word_not(node_ptr w, const NuSMVEnv_ptr env)
{
  return node_word_apply_unary(w, NOT, env);
}

node_ptr node_word_and(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  return node_word_apply_binary(a, b, AND, env);
}

node_ptr node_word_or(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{ return node_word_apply_binary(a, b, OR, env); }

node_ptr node_word_xor(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{ return node_word_apply_binary(a, b, XOR, env); }

node_ptr node_word_xnor(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{ return node_word_apply_binary(a, b, XNOR, env); }

node_ptr node_word_implies(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{ return node_word_apply_binary(a, b, IMPLIES, env); }

node_ptr node_word_iff(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{ return node_word_apply_binary(a, b, IFF, env); }

node_ptr node_word_equal(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{ return node_word_make_conjuction(node_word_iff(a, b, env), env); }

node_ptr node_word_notequal(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{ return node_word_make_disjunction(node_word_xor(a, b, env), env); }

node_ptr node_word_concat(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  size_t w;

  _CHECK_WORD(a);
  _CHECK_WORD(b);

  w = node_get_int(cdr(a)) + node_get_int(cdr(b));
  return node_word_create_from_list(append_ns(nodemgr, car(a), car(b)), w, env);
}

node_ptr node_word_selection(node_ptr word, node_ptr range, const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

  int width, high, low, i;
  node_ptr res, iter, tmp;

  _CHECK_WORD(word);
  nusmv_assert(node_get_type(range) == COLON &&
               node_get_type(car(range)) == NUMBER &&
               node_get_type(cdr(range)) == NUMBER);

  high = node_get_int(car(range));
  low = node_get_int(cdr(range));

  /* constant? */
  if (node_get_type(word) == NUMBER_UNSIGNED_WORD ||
      node_get_type(word) == NUMBER_SIGNED_WORD) {
    WordNumber_ptr w = WORD_NUMBER(car(word));
    width = WordNumber_get_width(w);
    nusmv_assert(high >= low && low >= 0 && high < width);
    w = WordNumberMgr_bit_select(words, w, high, low);
    return find_node(nodemgr, NUMBER_UNSIGNED_WORD, (node_ptr) w, Nil);
  }

  /* Non constant word, gets rid of higher bits */
  width = node_get_int(cdr(word));
  nusmv_assert(high >= low && low >= 0 && high < width);

  iter = car(word);
  for (i=width-1; i > high; --i) iter = cdr(iter);

  /* Takes only bits until low. */
  tmp = cons(nodemgr, car(iter), Nil); /* at least one bit exists */
  for (--i, iter = cdr(iter); i>=low; --i, iter = cdr(iter)) {
    tmp = cons(nodemgr, car(iter), tmp);
  }
  /* reverse the list and "find_node" it */
  for (res = Nil, iter = tmp; iter; iter = cdr(iter)) {
    res = find_node(nodemgr, CONS, car(iter), res);
  }
  free_list(nodemgr, tmp);

  return node_word_create_from_list(res, high-low+1, env);
}

node_ptr node_word_extend(node_ptr exp, node_ptr times, boolean isSigned,
                          const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  int width;
  int delta;
  node_ptr list;
  node_ptr bit;

  _CHECK_WORD(exp);
  nusmv_assert(NUMBER == node_get_type(times));

  width = node_get_int(cdr(exp));
  delta = node_get_int(times);
  list = car(exp);
  bit = isSigned ? car(list) : ExprMgr_false(exprs);

  nusmv_assert(delta >= 0);

  for (; delta > 0 ; --delta) {
    list = find_node(nodemgr, CONS, bit, list);
  }

  return node_word_create_from_list(list, width + node_get_int(times), env);
}

node_ptr node_word_adder(node_ptr a, node_ptr b, node_ptr carry_in,
                         node_ptr* carry_out, const NuSMVEnv_ptr env)
{
  node_ptr res;
  node_ptr width;
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  _CHECK_WORDS(a, b);

  width = cdr(a);
  res = Nil;
  for (a=reverse_ns(nodemgr, car(a)),b=reverse_ns(nodemgr, car(b));
       a!=Nil && b != Nil; a=cdr(a), b=cdr(b)) {
    node_ptr bit_carry;
    node_ptr bit = node_word_full_adder(car(a), car(b), carry_in, &bit_carry, env);
    res = find_node(nodemgr, CONS, bit, res);
    carry_in = bit_carry;
  }

  *carry_out = carry_in;
  return find_node(nodemgr, UNSIGNED_WORD, res, width);
}

node_ptr node_word_plus(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  node_ptr carry_out;
  return node_word_adder(a, b, ExprMgr_false(exprs), &carry_out, env);
}

node_ptr node_word_minus(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  /* a-b ==  a+(not b)+1 */
  node_ptr carry_out;
  return node_word_adder(a, node_word_not(b, env), ExprMgr_true(exprs), &carry_out, env);
}

node_ptr node_word_uminus(node_ptr a, const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  /* -a ==  (not a)+1 */
  node_ptr carry_out;
  node_ptr zero, zenc, wzero;
  int w;

  _CHECK_WORD(a);

  /* creates 0b0 */
  zero = ExprMgr_false(exprs);
  zenc = Nil;
  for (w = node_get_int(cdr(a)); w > 0; --w) zenc = find_node(nodemgr, CONS, zero, zenc);
  wzero = find_node(nodemgr, UNSIGNED_WORD, zenc, cdr(a));

  return node_word_adder(node_word_not(a, env), wzero, ExprMgr_true(exprs), &carry_out, env);
}

node_ptr node_word_times(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  /* A*B = ((B & A[0])<<0) +...+ ((B & A[i])<<i) +...+ ((B & A[N-1])<<N-1) */
  array_t *va, *vb, *vab;
  int w, i;

  _CHECK_WORDS(a,b);

  w = node_get_int(cdr(a));

  va = node_word_to_array(a);
  vb = node_word_to_array(b);

  vab = array_alloc(node_ptr, w);
  /* prepares (a[0] & b) */
  for (i=0; i<w; ++i) {
    node_ptr bit_a = array_fetch(node_ptr, va, 0);
    node_ptr bit_b = array_fetch(node_ptr, vb, i);
    array_insert(node_ptr, vab, i, ExprMgr_and(exprs, bit_a, bit_b));
  }

  for (i=1; i<w; ++i) {
    node_ptr cin = ExprMgr_false(exprs);
    int k;

    for (k=0; k < w-i; ++k) {
      node_ptr sum, ctmp;
      node_ptr bit_a = array_fetch(node_ptr, va, i);
      node_ptr bit_b = array_fetch(node_ptr, vb, k);
      sum = node_word_full_adder(array_fetch(node_ptr, vab, i+k),
                                 ExprMgr_and(exprs, bit_a, bit_b), cin, &ctmp, env);
      array_insert(node_ptr, vab, i+k, sum);
      cin = ctmp;
    }
  }

  array_free(vb);
  array_free(va);

  { /* creates the WORD from the bit array */
    node_ptr res = node_word_create_from_array(vab, env);
    array_free(vab);
    return res;
  }
}

node_ptr node_word_unsigned_divide(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  node_ptr rem;
  return node_word_unsigned_divide_reminder(a, b, &rem, env);
}

node_ptr node_word_unsigned_mod(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  node_ptr rem;
  node_word_unsigned_divide_reminder(a, b, &rem, env);
  return rem;
}

node_ptr node_word_signed_divide(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  node_ptr rem;
#if 1
  return node_word_signed_divide_reminder_simple(a, b, &rem, env);
#else
  return node_word_signed_divide_reminder_hardware(a, b, &rem, env);
#endif

}

node_ptr node_word_signed_mod(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  node_ptr rem;
#if 1
  node_word_signed_divide_reminder_simple(a, b, &rem, env);
#else
  node_word_signed_divide_reminder_hardware(a, b, &rem, env);
#endif
  return rem;
}

node_ptr node_word_unsigned_less(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  /* "carry bit of" ((not A) + B) */
  node_ptr carry_out;
  (void)node_word_adder(node_word_not(a, env), b, ExprMgr_false(exprs), &carry_out, env);
  return carry_out;
}

node_ptr node_word_unsigned_less_equal(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  /* "carry bit of" ((not A) + (B+1)) */
  node_ptr carry_out;
  (void)node_word_adder(node_word_not(a, env), b, ExprMgr_true(exprs), &carry_out, env);
  return carry_out;
}

node_ptr node_word_unsigned_greater(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{ return node_word_unsigned_less(b, a, env); }

node_ptr node_word_unsigned_greater_equal(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{ return node_word_unsigned_less_equal(b, a, env); }

node_ptr node_word_signed_less(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  /* A <s B
     === (A[msb] & !B[msb]) | ((A[msb] = B[msb]) & (A <u B)) */
  return node_word_signed_op(a, b, node_word_unsigned_less, env);
}

node_ptr node_word_signed_less_equal(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  /* A <=s B
     === (A[msb] & !B[msb]) | ((A[msb] = B[msb]) & (A <=u B)) */
  return node_word_signed_op(a, b, node_word_unsigned_less_equal, env);
}

node_ptr node_word_signed_greater(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  /* A >s B
     === B <s A */
  return node_word_signed_less(b, a, env);
}

node_ptr node_word_signed_greater_equal(node_ptr a, node_ptr b, const NuSMVEnv_ptr env)
{
  /* A >=s B
     === B <=s A */
  return node_word_signed_less_equal(b, a, env);
}

/*!
  \brief A private service for predicates


*/
static node_ptr node_word_signed_op(node_ptr a, node_ptr b,
                                    node_ptr (*op)(node_ptr, node_ptr, const NuSMVEnv_ptr),
                                    const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  node_ptr msb_a;
  node_ptr msb_b;

  node_ptr opres;
  node_ptr tmp1, tmp2;

  _CHECK_WORDS(a, b);

  msb_a = car(car(a));
  msb_b = car(car(b));

  opres = op(a, b, env);

  /* tmp1 := ((A[msb] = B[msb]) & (A <u B)) */
  tmp1 = ExprMgr_and(exprs, ExprMgr_iff(exprs, msb_a, msb_b), opres);

  /* tmp2 := (A[msb] & !B[msb]) */
  tmp2 = ExprMgr_and(exprs, msb_a, ExprMgr_not(exprs, msb_b));

  /* result: tmp1 | tmp2 */
  return ExprMgr_or(exprs, tmp1, tmp2);
}

/*!
  \brief Implements a full adder circuit

  implements a full adder circuit
*/
static node_ptr
node_word_full_adder(node_ptr ai, node_ptr bi, node_ptr carry_in,
                     node_ptr* carry_out, const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  node_ptr tmp = ExprMgr_xor(exprs, ai, bi);

  /* curry_out = (arg1 and arg2) OR ((arg1 XOR arg2) AND carry_in) */
  *carry_out = ExprMgr_or(exprs, ExprMgr_and(exprs, ai, bi),
                       ExprMgr_and(exprs, tmp, carry_in));

  /* sum = arg1 XOR arg2 XOR carry_in */
  return ExprMgr_xor(exprs, tmp, carry_in);
}

/*!
  \brief Implements a (unsigned) divide-with-reminder circuit

  Quotient is directly returned and remainder is return
  in 'reminder'
*/
static node_ptr node_word_unsigned_divide_reminder(node_ptr a, node_ptr b,
                                                   node_ptr* reminder,
                                                   const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  int width, i;
  array_t* va, *vb, *vquot, *vrem;

  _CHECK_WORDS(a,b);

  width = node_get_int(cdr(a));


  /* creates quotient and remainder */
  vquot = array_alloc(node_ptr, width);
  vrem = node_word_to_array(node_word_create(ExprMgr_false(exprs), width, env));
  va = node_word_to_array(a);
  vb = node_word_to_array(b);

  /* calculates the division operation */
  for (i=width-1; i>=0; --i) {
    node_ptr bit, iter;
    int k;

    /* shifts remainder left by 1 bit and add the i-th bit from 'a' */
    for (k = width-1; k > 0; --k) {
      bit = array_fetch(node_ptr, vrem, k-1);
      array_insert(node_ptr, vrem, k, bit);
    }
    bit = array_fetch(node_ptr, va, i);
    array_insert(node_ptr, vrem, 0, bit);

    /* calculates (rem >= b), i.e. that subtraction can be performed,
       and the subtraction itself, i.e. (rem - b)
    */
    {
      node_ptr rem = node_word_create_from_array(vrem, env);
      node_ptr is_dividable = node_word_unsigned_greater_equal(rem, b, env);
      node_ptr substruction = node_word_minus(rem, b, env);

      /* set the quotient's bit */
      array_insert(node_ptr, vquot, i, is_dividable);

      /* sets remainder to ITE(rem>=b, rem-b, rem) */
      for (k=width-1, iter = car(substruction);
           k>=0;
           --k, iter = cdr(iter)) {
        node_ptr bit = ExprMgr_ite(exprs, is_dividable, car(iter),
                                   array_fetch(node_ptr, vrem, k),
                                   SYMB_TABLE(NULL));
        array_insert(node_ptr, vrem, k, bit);
      }
    }
  } /* for i */

  array_free(vb);
  array_free(va);

  /* Now guards every bit of quotient and remainder by the condition
     that the divisor is not equal to zero */
  {
    /* prepares a check for 'b' not to be zero, to be inserted in the
       resulting encoding */
    node_ptr b_nz = node_word_make_disjunction(b, env);

    node_ptr dbz = ErrorMgr_failure_make(errmgr, "Division by zero",
                                FAILURE_DIV_BY_ZERO,
                                node_get_lineno(ErrorMgr_get_the_node(errmgr)));

    for (i=0; i<width; ++i) {
      node_ptr ite;

      /* quotient */
      ite = ExprMgr_ite(exprs, b_nz,
                     array_fetch(node_ptr, vquot, i),
                     dbz,
                     SYMB_TABLE(NULL));
      array_insert(node_ptr, vquot, i, ite);

      /* reminder */
      ite = ExprMgr_ite(exprs, b_nz,
                     array_fetch(node_ptr, vrem, i),
                     dbz,
                     SYMB_TABLE(NULL));
      array_insert(node_ptr, vrem, i, ite);
    }
  }

  {
    node_ptr rem = node_word_create_from_array(vrem, env);
    node_ptr quot = node_word_create_from_array(vquot, env);
    array_free(vrem);
    array_free(vquot);

    *reminder = rem;

    return quot;
  }
}

/*!
  \brief Implements a (signed) divide-with-reminder circuit

  Quotient is directly returned and remainder is return
  in 'reminder'.
  This is simple impelementation of signed division.
  See also node_word_signed_divide_reminder_hardware.

  \se node_word_signed_divide_reminder_hardware
*/
static node_ptr node_word_signed_divide_reminder_simple(node_ptr a, node_ptr b,
                                                        node_ptr* reminder,
                                                        const NuSMVEnv_ptr env)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr quot, rem;
  node_ptr iter, iter_neg;
  node_ptr list;
  int width;

  node_ptr sign_a;
  node_ptr sign_b;

  node_ptr positive_a;
  node_ptr positive_b;

  _CHECK_WORDS(a,b);

  width = node_get_int(cdr(a));

  /* signs of a and b */
  sign_a = car(car(a));
  sign_b = car(car(b));

  /* now obtain for sure not-negative a and b */
  positive_a = node_word_uminus(a, env);
  positive_b = node_word_uminus(b, env);

  list = Nil;
  for (iter = car(a), iter_neg = car(positive_a);
       iter != Nil;
       iter = cdr(iter), iter_neg = cdr(iter_neg)) {
    node_ptr bit = ExprMgr_ite(exprs, sign_a,
                            car(iter_neg),
                            car(iter),
                            SYMB_TABLE(NULL));
    list = cons(nodemgr, bit, list);
  }
  list = reverse(list);
  positive_a = new_node(nodemgr, UNSIGNED_WORD, list, cdr(a));


  list = Nil;
  for (iter = car(b), iter_neg = car(positive_b);
       iter != Nil;
       iter = cdr(iter), iter_neg = cdr(iter_neg)) {
    node_ptr bit = ExprMgr_ite(exprs, sign_b,
                            car(iter_neg),
                            car(iter),
                            SYMB_TABLE(NULL));
    list = cons(nodemgr, bit, list);
  }
  list = reverse(list);
  positive_b = new_node(nodemgr, UNSIGNED_WORD, list, cdr(b));

  /* perform unsigned division */
  quot = node_word_unsigned_divide_reminder(positive_a, positive_b, &rem, env);

  free_list(nodemgr, car(positive_a));
  free_node(nodemgr, positive_a);
  free_list(nodemgr, car(positive_b));
  free_node(nodemgr, positive_b);

  /* negate the remainder if the dividend was negative */
  {
    node_ptr negated_rem = node_word_uminus(rem, env);

    array_t* arr_rem = node_word_to_array(rem);
    array_t* arr_neg_rem = node_word_to_array(negated_rem);
    int i;
    for (i=0; i<width; ++i) {
      node_ptr bit = array_fetch(node_ptr, arr_neg_rem, i);
      bit = ExprMgr_ite(exprs, sign_a,
                     bit,
                     array_fetch(node_ptr, arr_rem, i),
                     SYMB_TABLE(NULL));
      array_insert(node_ptr, arr_rem, i, bit);
    }
    rem = node_word_create_from_array(arr_rem, env);
    array_free(arr_rem);
    array_free(arr_neg_rem);
  }

  {
    node_ptr negated_quot = node_word_uminus(quot, env);

    array_t* arr_quot = node_word_to_array(quot);
    array_t* arr_neg_quot = node_word_to_array(negated_quot);
    node_ptr diff_sign = ExprMgr_xor(exprs, sign_a, sign_b);
    int i;
    for (i=0; i<width; ++i) {
      node_ptr bit = array_fetch(node_ptr, arr_neg_quot, i);
      bit = ExprMgr_ite(exprs, diff_sign,
                     bit,
                     array_fetch(node_ptr, arr_quot, i),
                     SYMB_TABLE(NULL));
      array_insert(node_ptr, arr_quot, i, bit);
    }
    quot = node_word_create_from_array(arr_quot, env);
    array_free(arr_quot);
    array_free(arr_neg_quot);
  }

  *reminder = rem;
  return quot;
}

/*!
  \brief Implements a (signed) divide-with-reminder circuit

  Quotient is directly returned and remainder is return
  in 'reminder'.
  This is alternative to node_word_signed_divide_reminder_simple.
  This function should be impelemented similar to
  add_array_signed_division_remainder_hardware.
  Then node_word_signed_divide_reminder_simple and
  node_word_signed_divide_reminder_hardware should be compared which
  generated exps are better. Then the worse function should be removed.

  \se node_word_signed_divide_reminder_hardware
*/
static node_ptr node_word_signed_divide_reminder_hardware(node_ptr a, node_ptr b,
                                                          node_ptr* reminder,
                                                          const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  ErrorMgr_rpterr(errmgr, "node_word_signed_divide_reminder_hardware is not impelemnted yet.");
  return Nil;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static boolean _is_bool(const node_ptr a)
{
  return (TRUEEXP == node_get_type(a) || FALSEEXP == node_get_type(a));
}
