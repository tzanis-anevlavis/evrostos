/* ---------------------------------------------------------------------------


  This file is part of the ``mc'' package of NuSMV version 2.
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
  \brief CTL to BDD compiler

  In this file there is the code to compile CTL formulas
  into BDD and the code to call the model checking algorithms.

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/mc/mc.h"
#include "nusmv/core/mc/mcInt.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/wff/wff.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef bdd_ptr (*BDDPFDB)(DDMgr_ptr , bdd_ptr);
typedef bdd_ptr (*BDDPFFB)(BddFsm_ptr, bdd_ptr);
typedef bdd_ptr (*BDDPFDBB)(DDMgr_ptr , bdd_ptr, bdd_ptr);
typedef bdd_ptr (*BDDPFFBB)(BddFsm_ptr, bdd_ptr, bdd_ptr);
typedef bdd_ptr (*BDDPFDBII)(DDMgr_ptr , bdd_ptr, int, int);
typedef bdd_ptr (*BDDPFFBII)(BddFsm_ptr, bdd_ptr, int, int);
typedef bdd_ptr (*BDDPFDBBII)(DDMgr_ptr , bdd_ptr, bdd_ptr, int, int);
typedef bdd_ptr (*BDDPFFBBII)(BddFsm_ptr, bdd_ptr, bdd_ptr, int, int);

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static bdd_ptr eval_ctl_spec_recur(BddFsm_ptr, BddEnc_ptr enc,
                                   node_ptr, node_ptr);

static int eval_compute_recur(BddFsm_ptr, BddEnc_ptr enc,
                              node_ptr, node_ptr);

static bdd_ptr unary_bdd_op(BddFsm_ptr, BddEnc_ptr, BDDPFDB, node_ptr,
                            int, int, node_ptr);
static bdd_ptr binary_bdd_op(BddFsm_ptr, BddEnc_ptr, BDDPFDBB, node_ptr,
                             int, int, int, node_ptr);

static bdd_ptr unary_mod_bdd_op(BddFsm_ptr, BddEnc_ptr, BDDPFFB,
                                node_ptr, int, int, node_ptr);
static bdd_ptr binary_mod_bdd_op(BddFsm_ptr, BddEnc_ptr, BDDPFFBB,
                                 node_ptr, int, int, int, node_ptr);
static bdd_ptr binary_mod_bdd_op_ns(BddFsm_ptr, BddEnc_ptr, BDDPFFBB,
                                    node_ptr, int, int, int, node_ptr);
static bdd_ptr ternary_mod_bdd_op(BddFsm_ptr, BddEnc_ptr, BDDPFFBII,
                                  node_ptr, int, int, node_ptr);
static bdd_ptr quad_mod_bdd_op(BddFsm_ptr, BddEnc_ptr, BDDPFFBBII,
                               node_ptr, int, int, int, node_ptr);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

bdd_ptr eval_ctl_spec(BddFsm_ptr fsm, BddEnc_ptr enc,
                      node_ptr n, node_ptr context)
{
  bdd_ptr res;
  int temp = nusmv_yylineno;
  DDMgr_ptr dd_manager = BddEnc_get_dd_manager(enc);

  if (n == Nil) return(bdd_true(dd_manager));
  nusmv_yylineno = node_get_lineno(n);
  res = eval_ctl_spec_recur(fsm, enc, n, context);
  nusmv_yylineno = temp;
  return(res);
}

node_ptr eval_formula_list(BddFsm_ptr fsm, BddEnc_ptr enc,
                           node_ptr nodes, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  if (nodes == Nil) return(Nil);
  if (node_get_type(nodes) == CONS) {
    return(find_node(nodemgr, CONS, eval_formula_list(fsm, enc, car(nodes), context),
                           eval_formula_list(fsm, enc, cdr(nodes), context)));
  }
  return(find_node(nodemgr, BDD, (node_ptr) eval_ctl_spec(fsm, enc, nodes, context),
                   Nil));
}

int eval_compute(BddFsm_ptr fsm, BddEnc_ptr enc, node_ptr n, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  int res;
  int temp = nusmv_yylineno;

  if (n == Nil) ErrorMgr_internal_error(errmgr, "eval_compute: n = NIL\n");
  nusmv_yylineno = node_get_lineno(n);
  res = eval_compute_recur(fsm, enc, n, context);
  nusmv_yylineno = temp;
  return(res);
}

void free_formula_list(DDMgr_ptr dd, node_ptr formula_list){
  node_ptr fl = formula_list;

  while(fl != Nil) {
    node_ptr s = car(fl);

    fl = cdr(fl);
    bdd_free(dd, (bdd_ptr)car(s));
  }
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Recursive step of <code>eval_ctl_spec</code>.

  Performs the recursive step of
  <code>eval_ctl_spec</code>.

  \sa eval_ctl_spec
*/
static bdd_ptr eval_ctl_spec_recur(BddFsm_ptr fsm, BddEnc_ptr enc, node_ptr n,
                                   node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (n == Nil) { return bdd_true(BddEnc_get_dd_manager(enc)); }

  {
    SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(enc));
    if (Wff_is_propositional(st, n, context, false)) {
      bdd_ptr res_bdd = BddEnc_expr_to_bdd(enc, n, context);

      if (res_bdd == NULL) {
        ErrorMgr_rpterr(errmgr, "eval_ctl_spec: res = NULL after a call to \"eval\".");
        ErrorMgr_nusmv_exit(errmgr, 1);
      }
      return res_bdd;
    }
  }

  switch (node_get_type(n)) {
  case CONTEXT: return(eval_ctl_spec(fsm, enc, cdr(n),car(n)));
  case AND:     return(binary_bdd_op(fsm, enc, bdd_and, n, 1, 1, 1, context));
  case OR:      return(binary_bdd_op(fsm, enc, bdd_or, n, 1, 1, 1, context));
  case XOR:     return(binary_bdd_op(fsm, enc, bdd_xor, n, 1, 1, 1, context));
  case XNOR:    return(binary_bdd_op(fsm, enc, bdd_xor, n, 1, 1, -1, context));
  case NOT:     return(unary_bdd_op(fsm, enc, bdd_not, n, 1, 1, context));
  case IMPLIES: return(binary_bdd_op(fsm, enc, bdd_or, n, 1, -1, 1, context));
  case IFF:     return(binary_bdd_op(fsm, enc, bdd_xor, n, -1, 1, 1, context));

  case EX:      return(unary_mod_bdd_op(fsm, enc, ex, n,  1,  1, context));
  case AX:      return(unary_mod_bdd_op(fsm, enc, ex, n, -1, -1, context));
  case EF:      return(unary_mod_bdd_op(fsm, enc, ef, n,  1,  1, context));
  case AG:      return(unary_mod_bdd_op(fsm, enc, ef, n, -1, -1, context));
  case AF:      return(unary_mod_bdd_op(fsm, enc, eg, n, -1, -1, context));
  case EG:      return(unary_mod_bdd_op(fsm, enc, eg, n,  1,  1, context));
  case EU:      return(binary_mod_bdd_op(fsm, enc, eu, n, 1, 1, 1, context));
  case AU:      return(binary_mod_bdd_op(fsm, enc, au, n, 1, 1, 1, context));
  case EBU:     return(quad_mod_bdd_op(fsm, enc, ebu, n, 1, 1, 1, context));
  case ABU:     return(quad_mod_bdd_op(fsm, enc, abu, n, 1, 1, 1, context));
  case EBF:     return(ternary_mod_bdd_op(fsm, enc, ebf, n, 1, 1, context));
  case ABF:     return(ternary_mod_bdd_op(fsm, enc, ebg, n, -1, -1, context));
  case EBG:     return(ternary_mod_bdd_op(fsm, enc, ebg, n, 1, 1, context));
  case ABG:     return(ternary_mod_bdd_op(fsm, enc, ebf, n, -1, -1, context));
  default:
    {
      bdd_ptr res_bdd = BddEnc_expr_to_bdd(enc, n, context);

      if (res_bdd == NULL) {
        ErrorMgr_rpterr(errmgr, "eval_ctl_spec: res = NULL after a call to \"eval\".");
        ErrorMgr_nusmv_exit(errmgr, 1);
      }
      return res_bdd;
    }
  } /* switch */
}

/*!
  \brief Recursive step of <code>eval_compute</code>.

  Performs the recursive step of <code>eval_compute</code>.

  \sa eval_compute
*/
static int eval_compute_recur(BddFsm_ptr fsm, BddEnc_ptr enc,
                              node_ptr n, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  int res;

  if (n == Nil) ErrorMgr_internal_error(errmgr, "eval_compute_recur: n = NIL\n");

  switch (node_get_type(n)) {
  case CONTEXT:
    res = eval_compute_recur(fsm, enc, cdr(n),car(n)); break;

  case MINU:
    res = PTR_TO_INT(binary_mod_bdd_op_ns(fsm, enc,
                          (BDDPFFBB)minu, n, 1, 1, 1, context));
    break;

  case MAXU:
    res = PTR_TO_INT(binary_mod_bdd_op_ns(fsm, enc,
                          (BDDPFFBB)maxu, n, 1, 1, 1, context));
    break;

  default:
    res = 0;
    ErrorMgr_internal_error(errmgr, "eval_compute: type = %d\n", node_get_type(n));
  }

  return res;
}

/*!
  \brief Applies unary operation.

  Takes in input the expression <code>n</code> and a
  unary operation <code>op</code>. Evaluates <code>n</n> and applies to this
  partial result the unary operator <code>op</code>. The sign of the
  partial result and of the result depends respectively from the flag
  <code>argflag</code> and <code>resflag</code>.

  \sa binary_bdd_op, ternary_bdd_op, quaternary_bdd_op
*/
static bdd_ptr unary_bdd_op(BddFsm_ptr fsm, BddEnc_ptr enc, BDDPFDB op,
                            node_ptr n, int resflag, int argflag,
                            node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  bdd_ptr tmp_1, tmp_2, res;
  bdd_ptr arg = eval_ctl_spec(fsm, enc, car(n), context);
  DDMgr_ptr dd = BddEnc_get_dd_manager(enc);

  ErrorMgr_set_the_node(errmgr, n);

  /* compute and ref argument of operation according its sign */
  tmp_1 = BddEnc_eval_sign_bdd(enc, arg, argflag);

  /* apply and ref the result of the application of "op" to previous arg. */
  tmp_2 = op(dd, tmp_1);

  /* compute and ref the result according to sign of the result */
  res = BddEnc_eval_sign_bdd(enc, tmp_2, resflag);

  /* free temporary results */
  bdd_free(dd, tmp_1);
  bdd_free(dd, tmp_2);
  bdd_free(dd, arg);

  return res;
}

/*!
  \brief Applies binary operation.

  Takes in input the expression <code>n</code> and a
  binary operation <code>op</code>. Extracts from <code>n</n> the operands
  and evaluates them. The binary operator <code>op</code> is then applied
  to these partial results. The sign of the partial results and of the
  result depends respectively from the flags <code>argflag1</code>,
  <code>argflag2</code> and <code>resflag</code>.

  \sa unary_bdd_op, ternary_bdd_op, quaternary_bdd_op
*/
static bdd_ptr binary_bdd_op(BddFsm_ptr fsm, BddEnc_ptr enc, BDDPFDBB op,
                             node_ptr n, int resflag, int argflag1,
                             int argflag2, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  bdd_ptr tmp_1, tmp_2, tmp_3, res;
  bdd_ptr arg1 = eval_ctl_spec(fsm, enc, car(n), context);
  bdd_ptr arg2 = eval_ctl_spec(fsm, enc, cdr(n), context);

  DDMgr_ptr dd = BddEnc_get_dd_manager(enc);
  ErrorMgr_set_the_node(errmgr, n);

  tmp_1 = BddEnc_eval_sign_bdd(enc, arg1, argflag1);
  tmp_2 = BddEnc_eval_sign_bdd(enc, arg2, argflag2);
  tmp_3 = op(dd, tmp_1, tmp_2);
  res = BddEnc_eval_sign_bdd(enc, tmp_3, resflag);

  bdd_free(dd, tmp_1);
  bdd_free(dd, tmp_2);
  bdd_free(dd, tmp_3);
  bdd_free(dd, arg1);
  bdd_free(dd, arg2);

  return res;
}

/*!
  \brief Applies unary operation.

  Takes in input the expression <code>n</code> and a
  unary operation <code>op</code>. Evaluates <code>n</n> and applies to this
  partial result the unary operator <code>op</code>. The sign of the
  partial result and of the result depends respectively from the flag
  <code>argflag</code> and <code>resflag</code>.

  \sa binary_bdd_op, ternary_bdd_op, quaternary_bdd_op
*/
static bdd_ptr unary_mod_bdd_op(BddFsm_ptr fsm, BddEnc_ptr enc, BDDPFFB op,
                                node_ptr n, int resflag, int argflag,
                                node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  bdd_ptr tmp_1, tmp_2, res;
  bdd_ptr arg;
  DDMgr_ptr dd;

  BDD_FSM_CHECK_INSTANCE(fsm);

  arg = eval_ctl_spec(fsm, enc, car(n), context);
  dd = BddEnc_get_dd_manager(enc);
  ErrorMgr_set_the_node(errmgr, n);

  /* compute and ref argument of operation according its sign */
  tmp_1 = BddEnc_eval_sign_bdd(enc, arg, argflag);

  /* apply and ref the result of the application of "op" to previous arg. */
  tmp_2 = op(fsm, tmp_1);

  /* compute and ref the result according to sign of the result */
  res = BddEnc_eval_sign_bdd(enc, tmp_2, resflag);

  /* free temporary results */
  bdd_free(dd, tmp_1);
  bdd_free(dd, tmp_2);
  bdd_free(dd, arg);

  return res;
}

/*!
  \brief Applies binary operation.

  Takes in input the expression <code>n</code> and a
  binary operation <code>op</code>. Extracts from <code>n</n> the operands
  and evaluates them. The binary operator <code>op</code> is then applied
  to these partial results. The sign of the partial results and of the
  result depends respectively from the flags <code>argflag1</code>,
  <code>argflag2</code> and <code>resflag</code>.

  \sa unary_bdd_op, ternary_bdd_op, quaternary_bdd_op
*/
static bdd_ptr binary_mod_bdd_op(BddFsm_ptr fsm, BddEnc_ptr enc, BDDPFFBB op,
                                 node_ptr n, int resflag, int argflag1,
                                 int argflag2, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  bdd_ptr tmp_1, tmp_2, tmp_3, res;
  bdd_ptr arg1;
  bdd_ptr arg2;
  DDMgr_ptr dd;

  BDD_FSM_CHECK_INSTANCE(fsm);

  arg1 = eval_ctl_spec(fsm, enc, car(n), context);
  arg2 = eval_ctl_spec(fsm, enc, cdr(n), context);
  dd = BddEnc_get_dd_manager(enc);

  ErrorMgr_set_the_node(errmgr, n);

  tmp_1 = BddEnc_eval_sign_bdd(enc, arg1, argflag1);
  tmp_2 = BddEnc_eval_sign_bdd(enc, arg2, argflag2);
  tmp_3 = op(fsm, tmp_1, tmp_2);
  res = BddEnc_eval_sign_bdd(enc, tmp_3, resflag);

  bdd_free(dd, tmp_1);
  bdd_free(dd, tmp_2);
  bdd_free(dd, tmp_3);
  bdd_free(dd, arg1);
  bdd_free(dd, arg2);

  return(res);
}

/*!
  \brief Applies binary operation.

  Takes in input the expression <code>n</code> and a
  binary operation <code>op</code>. Extracts from <code>n</n> the operands
  and evaluates them. The binary operator <code>op</code> is then applied
  to these partial results. The sign of the partial results and of the
  result depends respectively from the flags <code>argflag1</code>,
  <code>argflag2</code> and <code>resflag</code>.<br>
  The only difference between this and "binary_mod_bdd_op" is that the
  result of the application of the operation passed as argument is not
  referenced. This is used for example in the "minu" and "maxu" operations.

  \sa unary_bdd_op, ternary_bdd_op, quaternary_bdd_op
*/
static bdd_ptr binary_mod_bdd_op_ns(BddFsm_ptr fsm, BddEnc_ptr enc, BDDPFFBB op,
                                    node_ptr n, int resflag, int argflag1,
                                    int argflag2, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  bdd_ptr tmp_1, tmp_2, res;
  bdd_ptr arg1;
  bdd_ptr arg2;
  DDMgr_ptr dd;

  BDD_FSM_CHECK_INSTANCE(fsm);

  arg1 = eval_ctl_spec(fsm, enc, car(n), context);
  arg2 = eval_ctl_spec(fsm, enc, cdr(n), context);
  dd = BddEnc_get_dd_manager(enc);
  ErrorMgr_set_the_node(errmgr, n);

  tmp_1 = BddEnc_eval_sign_bdd(enc, arg1, argflag1);
  tmp_2 = BddEnc_eval_sign_bdd(enc, arg2, argflag2);
  res = op(fsm, tmp_1, tmp_2);

  bdd_free(dd, tmp_1);
  bdd_free(dd, tmp_2);
  bdd_free(dd, arg1);
  bdd_free(dd, arg2);

  return res;
}

/*!
  \brief Applies ternary operation.

  Takes in input the expression <code>n</code> and a
  ternary operation <code>op</code>. Extracts from <code>n</n> the operands
  and evaluates them.<br>
  The second and third arguments have to evaluate to numbers. And
  <code>op</code> is a function that takes as input an BDD an two integers.
  The ternary operator <code>op</code> is then applied to these partial
  results. The sign of the partial result and of the result depends
  respectively from the flags <code>argflag</code> and <code>resflag</code>.

  \sa unary_bdd_op, binary_bdd_op, quaternary_bdd_op
*/
static bdd_ptr ternary_mod_bdd_op(BddFsm_ptr fsm, BddEnc_ptr enc, BDDPFFBII op,
                                  node_ptr n, int resflag, int argflag,
                                  node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  bdd_ptr tmp_1, tmp_2, res;
  bdd_ptr arg1;
  int arg2;
  int arg3;
  DDMgr_ptr dd;

  BDD_FSM_CHECK_INSTANCE(fsm);

  arg1 = eval_ctl_spec(fsm, enc, car(n), context);
  arg2 = BddEnc_eval_num(enc, car(cdr(n)), context);
  arg3 = BddEnc_eval_num(enc, cdr(cdr(n)), context);
  dd = BddEnc_get_dd_manager(enc);
  ErrorMgr_set_the_node(errmgr, n);

  tmp_1 = BddEnc_eval_sign_bdd(enc, arg1, argflag);
  tmp_2 = op(fsm, tmp_1, arg2, arg3);
  res = BddEnc_eval_sign_bdd(enc, tmp_2, resflag);

  bdd_free(dd, tmp_1);
  bdd_free(dd, tmp_2);
  bdd_free(dd, arg1);

  return res;
}

/*!
  \brief Applies quaternary operation.

  Takes in input the expression <code>n</code> and a
  quaternary operation <code>op</code>. Extracts from <code>n</n> the operands
  and evaluates them.<br>
  The third and fourth arguments have to evaluate to numbers. And
  <code>op</code> is a function that takes as input two BDD and two integers.
  The quaternary operator <code>op</code> is then applied to these partial
  results. The sign of the partial result and of the result depends
  respectively from the flags <code>argflag1</code>, <code>argflag2</code> and
  <code>resflag</code>.

  \sa unary_bdd_op, binary_bdd_op, ternary_bdd_op
*/
static bdd_ptr quad_mod_bdd_op(BddFsm_ptr fsm, BddEnc_ptr enc, BDDPFFBBII op,
                               node_ptr n, int resflag, int argflag1,
                               int argflag2, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  bdd_ptr tmp_1, tmp_2, tmp_3, res;
  bdd_ptr arg1;
  bdd_ptr arg2;
  int arg3;
  int arg4;
  DDMgr_ptr dd;

  BDD_FSM_CHECK_INSTANCE(fsm);

  arg1 = eval_ctl_spec(fsm, enc, car(car(n)), context);
  arg2 = eval_ctl_spec(fsm, enc, cdr(car(n)), context);
  arg3 = BddEnc_eval_num(enc, car(cdr(n)), context);
  arg4 = BddEnc_eval_num(enc, cdr(cdr(n)), context);
  dd = BddEnc_get_dd_manager(enc);

  ErrorMgr_set_the_node(errmgr, n);

  tmp_1 = BddEnc_eval_sign_bdd(enc, arg1, argflag1);
  tmp_2 = BddEnc_eval_sign_bdd(enc, arg2, argflag1);
  tmp_3 = op(fsm, tmp_1, tmp_2, arg3, arg4);
  res = BddEnc_eval_sign_bdd(enc, tmp_3, resflag);

  bdd_free(dd, tmp_1);
  bdd_free(dd, tmp_2);
  bdd_free(dd, tmp_3);
  bdd_free(dd, arg1);
  bdd_free(dd, arg2);

  return res;
}


