/* ---------------------------------------------------------------------------


  This file is part of the ``eval'' package of NuSMV version 2.
  Copyright (C) 2010 by FBK-irst.

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
  \author Marco Pensallorto, Marco Roveri
  \brief Implementation of class 'BaseEvaluator'

  \todo: Missing description

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/trace/eval/BaseEvaluator.h"
#include "nusmv/core/trace/eval/BaseEvaluator_private.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/parser/symbols.h"
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
/* See 'BaseEvaluator_private.h' for class 'BaseEvaluator' definition. */

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

static void base_evaluator_finalize(Object_ptr object, void* dummy);

static Expr_ptr base_evaluator_eval_recur(BaseEvaluator_ptr self,
                                          const Expr_ptr const_expr,
                                          boolean in_next);

static Expr_ptr base_evaluator_resolve_expr(BaseEvaluator_ptr self,
                                            const Expr_ptr const_expr);

static node_ptr
base_evaluator_make_failure(const NuSMVEnv_ptr env,
                            const char* tmpl, node_ptr symbol);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

BaseEvaluator_ptr BaseEvaluator_create(void)
{
  BaseEvaluator_ptr self = ALLOC(BaseEvaluator, 1);
  BASE_EVALUATOR_CHECK_INSTANCE(self);

  base_evaluator_init(self);
  return self;
}

void BaseEvaluator_destroy(BaseEvaluator_ptr self)
{
  BASE_EVALUATOR_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

void BaseEvaluator_set_context(BaseEvaluator_ptr self,
                               const SymbTable_ptr st, const hash_ptr env)
{
  BASE_EVALUATOR_CHECK_INSTANCE(self);

  if ((hash_ptr)(NULL) != self->cache) {
    free_assoc(self->cache);
  }
  self->cache = new_assoc();

  self->st = st;
  self->env = env;
}

Expr_ptr BaseEvaluator_evaluate(BaseEvaluator_ptr self,
                                const Expr_ptr const_expr)
{
  BASE_EVALUATOR_CHECK_INSTANCE(self);
  SYMB_TABLE_CHECK_INSTANCE(self->st);
  nusmv_assert((hash_ptr)(NULL) != self->env);

  return base_evaluator_eval_recur(self, const_expr, false);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void base_evaluator_init(BaseEvaluator_ptr self)
{
  /* base class initialization */
  object_init(OBJECT(self));

  /* members initialization */
  self->cache = (hash_ptr)(NULL);

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = base_evaluator_finalize;
  OVERRIDE(BaseEvaluator, resolve) = base_evaluator_resolve_expr;
}

void base_evaluator_deinit(BaseEvaluator_ptr self)
{
  /* members deinitialization */
  if ((hash_ptr)(NULL) != self->cache) { free_assoc(self->cache); }

  /* base class deinitialization */
  object_deinit(OBJECT(self));
}

Expr_ptr base_evaluator_resolve(const BaseEvaluator_ptr self,
                                const Expr_ptr const_expr)
{
  return (*self->resolve)(self, const_expr);
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief This function is a private service of BaseEvaluator_eval

  This function is a private service of BaseEvaluator_eval
*/
static Expr_ptr base_evaluator_eval_recur(BaseEvaluator_ptr self,
                                          const Expr_ptr expr,
                                          boolean in_next)
{
  const NuSMVEnv_ptr nenv = EnvObject_get_environment(ENV_OBJECT(self->st));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(nenv, ENV_NODE_MGR));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(nenv, ENV_EXPR_MANAGER));

  Expr_ptr res;
  const SymbTable_ptr st =  self->st;
  const hash_ptr env = self->env;
  node_ptr key;

  /* corner cases: they can all be handled together */
  if ((Nil == expr)  || FAILURE == node_get_type(expr) ||
      SymbTable_is_symbol_constant(st, expr) ||
      node_is_leaf(expr)) {

    return expr;
  }

  /* key for result memoization */
  key = in_next ? find_node(nodemgr, NEXT, expr, Nil) : expr;

  /* if a memoized result is available, return */
  res = find_assoc(self->cache, key);
  if (Nil != res)
    return res;

  switch (node_get_type(expr)) {

  case INTARRAY:
  case WORDARRAY:
  case ARRAY:
  case ATOM:
  case DOT:
    {
      res = find_assoc(env, key);
      if (Nil == res) {
        const char* fail_msg = "Uknown symbol: '%s'";
        res = base_evaluator_make_failure(nenv, fail_msg, key);
      }
      break;
    } /* array, atom, dot */

    /** unary ops */
  case NOT:
  case UMINUS:
  case FLOOR:
    {
      Expr_ptr tmp;
      node_ptr left_expr = car(expr);

      /* handle failure conditions */
      left_expr = base_evaluator_eval_recur(self, left_expr, in_next);
      if (FAILURE == node_get_type(left_expr)) return left_expr;

      tmp = find_node(nodemgr, node_get_type(expr), left_expr, Nil);
      res = find_assoc(self->cache, tmp);
      if (Nil == res) { res = base_evaluator_resolve(self, tmp); }
      break;
    }

  case NEXT:
    {
      nusmv_assert(!in_next); /* no nested NEXTs allowed */
      return base_evaluator_eval_recur(self, car(expr), true);
    }

    /** binary ops */
  case LT: case GT: case LE: case GE:
  case NOTEQUAL: case EQUAL: case EQDEF:
  case CONS: case AND: case OR: case XOR:
  case XNOR: case IFF: case IMPLIES: case PLUS:
  case MINUS: case TIMES: case DIVIDE: case MOD:
  case LSHIFT: case RSHIFT: case UNION: case SETIN:
  case TWODOTS:

  case COLON: /* probably not needed, but kept for safety */
    /** word bin ops */
  case CONCATENATION: case EXTEND: case LROTATE: case RROTATE:
    {
      Expr_ptr tmp;
      node_ptr left_expr = car(expr);
      node_ptr right_expr = cdr(expr);

      /* handle failure conditions */
      if (Nil != left_expr) {
        left_expr = base_evaluator_eval_recur(self, left_expr, in_next);
        if (FAILURE == node_get_type(left_expr)) return left_expr;
      }

      if (Nil != right_expr) {
        right_expr = base_evaluator_eval_recur(self, right_expr, in_next);
        if (FAILURE == node_get_type(right_expr)) return right_expr;
      }

      tmp = find_node(nodemgr, node_get_type(expr), left_expr, right_expr);
      res = find_assoc(self->cache, tmp);

      if (SETIN == node_get_type(expr)) {
        /* evaluate SETIN predicates semantically, by building set representation
           for left and right and verifying that left is contained into right */
        Set_t left = Set_MakeFromUnion(nodemgr, left_expr);
        Set_t right = Set_MakeFromUnion(nodemgr, right_expr);

        res = Set_Contains(right, left) ? ExprMgr_true(exprs) : ExprMgr_false(exprs);
        Set_ReleaseSet(left);
        Set_ReleaseSet(right);
      }

      if (Nil == res) { res = base_evaluator_resolve(self, tmp); }
      break;
     } /* binary ops */

  case BIT_SELECTION:
    {
      Expr_ptr m, M, tmp;
      Expr_ptr w = car(expr);
      Expr_ptr bits = cdr(expr);

      nusmv_assert(COLON == node_get_type(bits));

      m = car(bits);
      M = cdr(bits);

      /* To avoid recursing on the COLON of the BIT selection */
      if (Nil != m) {
        m = base_evaluator_eval_recur(self, car(bits), in_next);
        if (FAILURE == node_get_type(m)) return m;
      }
      if (Nil != M) {
        M = base_evaluator_eval_recur(self, cdr(bits), in_next);
        if (FAILURE == node_get_type(M)) return M;
      }
      if (Nil != w) {
        w = base_evaluator_eval_recur(self, w, in_next);
        if (FAILURE == node_get_type(w)) return w;
      }
      tmp = find_node(nodemgr, node_get_type(expr), w, find_node(nodemgr, COLON, m, M));

      res = find_assoc(self->cache, tmp);

      if (Nil == res) { res = base_evaluator_resolve(self, tmp); }
      break;
    }

  case WRESIZE:
  case CAST_TO_UNSIGNED_WORD:
    {
      Expr_ptr tmp;
      Expr_ptr wexpr = car(expr);
      Expr_ptr wsize = cdr(expr);

      /* handle failure conditions */
      if (Nil != wexpr) {
        wexpr = base_evaluator_eval_recur(self, wexpr, in_next);
        if (FAILURE == node_get_type(wexpr)) return wexpr;
      }
      if (Nil != wsize) {
        wsize = base_evaluator_eval_recur(self, wsize, in_next);
        if (FAILURE == node_get_type(wsize)) return wsize;
      }

      tmp = find_node(nodemgr, node_get_type(expr), wexpr, wsize);
      res = find_assoc(self->cache, tmp);

      if (Nil == res) { res = base_evaluator_resolve(self, tmp); }
      break;
    }

  case CAST_SIGNED:
  case CAST_UNSIGNED:
  case CAST_WORD1:
    {
      Expr_ptr tmp;
      Expr_ptr e = car(expr);

      /* handle failure conditions */
      if (Nil != e) {
        e = base_evaluator_eval_recur(self, e, in_next);
        if (FAILURE == node_get_type(e)) return e;
      }

      tmp = find_node(nodemgr, node_get_type(expr), e, Nil);
      res = find_assoc(self->cache, tmp);

      if (Nil == res) { res = base_evaluator_resolve(self, tmp); }
      break;
    }

  case CAST_BOOL:
    {
      Expr_ptr tmp;
      Expr_ptr w = car(expr);

      /* handle failure conditions */
      if (Nil != w) {
        w = base_evaluator_eval_recur(self, w, in_next);
        if (FAILURE == node_get_type(w)) return w;
      }

      tmp = find_node(nodemgr, node_get_type(expr), w, Nil);
      res = find_assoc(self->cache, tmp);

      if (Nil == res) { res = base_evaluator_resolve(self, tmp); }
      break;
    }

    /** ternary ops */
  case IFTHENELSE:
  case CASE:
    {
      Expr_ptr cond_expr = caar(expr);
      Expr_ptr cond_value = base_evaluator_eval_recur(self, cond_expr, in_next);
      if (FAILURE == node_get_type(cond_value)) {
        return cond_value;
      }

      /* condition is a predicate */
      nusmv_assert (ExprMgr_is_true(exprs, cond_value) || ExprMgr_is_false(exprs, cond_value));
      if (ExprMgr_is_true(exprs, cond_value)) {
        res = base_evaluator_eval_recur(self, cdar(expr), in_next);
      }
      else {
        res = base_evaluator_eval_recur(self, cdr(expr), in_next);
      }
      break;
    }

  case FAILURE:
    return expr;

  case CONST_ARRAY:
    {
      Expr_ptr value = base_evaluator_eval_recur(self, cdr(expr), in_next);
      nusmv_assert(Nil != value);
      if (FAILURE == node_get_type(value)) return value;
      res = find_node(nodemgr, CONST_ARRAY, car(expr), value);
      break;
    }

  case WAREAD:
    {
      Expr_ptr array_expr = car(expr);
      Expr_ptr index_expr = cdr(expr);

      nusmv_assert(Nil != index_expr);
      nusmv_assert(Nil != array_expr);

      /* The index shall evaluate to a constant value, either integer
         or unsigned word */
      index_expr = base_evaluator_eval_recur(self, index_expr, in_next);

      nusmv_assert(Nil != index_expr);
      if (FAILURE == node_get_type(index_expr)) return index_expr;

      nusmv_assert((INTEGER == node_get_type(index_expr)) ||
                   (NUMBER_UNSIGNED_WORD == node_get_type(index_expr)));

      array_expr = base_evaluator_eval_recur(self, array_expr, in_next);

      if (FAILURE == node_get_type(array_expr)) return array_expr;

      nusmv_assert((WAWRITE == node_get_type(array_expr)) ||
                   (CONST_ARRAY == node_get_type(array_expr)));

      {
        TypeChecker_ptr tc = SymbTable_get_type_checker(st);
        SymbType_ptr at = TypeChecker_get_expression_type(tc, array_expr, Nil);

        nusmv_assert(SymbType_is_wordarray(at) || SymbType_is_intarray(at));

        if (SymbType_is_wordarray(at)) {
          int size = SymbType_get_wordarray_awidth(at);
          Expr_ptr s_expr = ExprMgr_word_max_value(exprs, size, NUMBER_UNSIGNED_WORD);
          Expr_ptr i_expr = (NUMBER != node_get_type(index_expr)) ? index_expr : Nil;
            if (ExprMgr_is_true(exprs, ExprMgr_gt(exprs, i_expr, s_expr))) {
            const ErrorMgr_ptr errmgr =
              ERROR_MGR(NuSMVEnv_get_value(nenv, ENV_ERROR_MANAGER));
            const MasterPrinter_ptr wffprint =
              MASTER_PRINTER(NuSMVEnv_get_value(nenv, ENV_WFF_PRINTER));
            const char* fail_msg = "Out of bound read for a word array: (size, index) = '(%s, %s)'\n";
            char *s1 = sprint_node(wffprint, s_expr);
            char *s2 = sprint_node(wffprint, i_expr);
            char *buf = ALLOC(char, 20 + strlen(fail_msg) + strlen(s1) + strlen(s2));

            sprintf(buf, fail_msg, s1, s2);
            res = ErrorMgr_failure_make(errmgr, buf, FAILURE_UNSPECIFIED, -1);

            FREE(buf); FREE(s1); FREE(s2);
            nusmv_assert(false);
            break;
          }
        }
        else {
          /* There is no bound, thus it is ok to proceed */
          nusmv_assert(SymbType_is_intarray(at));
        }
      }

      {
        boolean found = false;
        while (!found) {

          nusmv_assert(Nil != array_expr);

          if (CONST_ARRAY == node_get_type(array_expr)) {
            /* READ(CONSTARRAY(type, value), index_expr) -> value */
            Expr_ptr value = cdr(array_expr);
            res = value;
            found = true;
          }
          else if (WAWRITE == node_get_type(array_expr)) {
            /* READ(WRITE(array_expr, index_expr, value), index_expr) -> value */
            /* READ(WRITE(array_expr, diff_index_expr, value), index_expr) ->
               READ(array_expr, index_expr) */
            Expr_ptr wr_array = car(array_expr);
            Expr_ptr wr_index = car(cdr(array_expr));
            Expr_ptr wr_value = cdr(cdr(array_expr));

            if (ExprMgr_is_true(exprs,
                                ExprMgr_equal(exprs, index_expr, wr_index, st))) {
              /* READ(WRITE(array_expr, index_expr, value), index_expr) -> value */
              res = wr_value;
              found = true;
            }
            else {
              /* READ(WRITE(array_expr, diff_index_expr, value), index_expr) ->
                 READ(array_expr, index_expr) */
              array_expr = wr_array;
            }
          }
          else {
            const char* fail_msg = "Uknown expression: '%s'";
            res = base_evaluator_make_failure(nenv, fail_msg, array_expr);
            found = true;
          }
        }
      }
      nusmv_assert(res != Nil);
      break;
    }

  case WAWRITE:
    {
      Expr_ptr tmp;
      Expr_ptr array_expr = car(expr);
      Expr_ptr index_expr = cadr(expr);
      Expr_ptr value_expr = cddr(expr);

       /* handle failure conditions */
      if (Nil != array_expr) {
        array_expr = base_evaluator_eval_recur(self, array_expr, in_next);
        if (FAILURE == node_get_type(array_expr)) return array_expr;
      }
      if (Nil != index_expr) {
        index_expr = base_evaluator_eval_recur(self, index_expr, in_next);
        if (FAILURE == node_get_type(index_expr)) return index_expr;
      }
      if (Nil != value_expr) {
        value_expr = base_evaluator_eval_recur(self, value_expr, in_next);
        if (FAILURE == node_get_type(value_expr)) return value_expr;
      }
      tmp = find_node(nodemgr, WAWRITE, array_expr,
                      new_node(nodemgr, WAWRITE, index_expr, value_expr));
      res = find_assoc(self->cache, tmp);

      if (Nil == res) { res = base_evaluator_resolve(self, tmp); }
      break;
    }

  case NFUNCTION:
    {

      node_ptr fun_name = car(expr);
      node_ptr params = cdr(expr);
      node_ptr params_resolved = Nil;


      /* Recusive parameters evaluation */
      NodeList_ptr args_list = NodeList_create();
      node_ptr tmp;

      while(Nil != params){
        tmp = base_evaluator_eval_recur(self, car(params), in_next);
        NodeList_append(args_list, tmp);
        params = cdr(params);
      }

      { /* Regeneration of conslist parameters */
        ListIter_ptr iter;
        node_ptr arg;

        NODE_LIST_FOREACH(args_list, iter){
          arg = NodeList_get_elem_at(args_list, iter);
          params_resolved = find_node(nodemgr, CONS, arg, params_resolved);
        }
      }

      NodeList_destroy(args_list);

      res = find_node(nodemgr, node_get_type(expr), fun_name, params_resolved);
      break;
    }


  default:
    {
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(nenv, ENV_ERROR_MANAGER));

      ErrorMgr_internal_error(errmgr, "%s:%d:%s Unsupported node type (%d)",
                              __FILE__, __LINE__, __func__,
                              node_get_type(expr));
    }
  } /* of switch */

  /* memoize results */
  insert_assoc(self->cache, key, res);
  return res;
}

/*!
  \brief The BaseEvaluator class virtual finalizer

  Called by the class destructor
*/
static void base_evaluator_finalize(Object_ptr object, void* dummy)
{
  BaseEvaluator_ptr self = BASE_EVALUATOR(object);

  base_evaluator_deinit(self);
  FREE(self);
}


/*!
  \brief

*/

static Expr_ptr base_evaluator_resolve_expr(BaseEvaluator_ptr self,
                                            const Expr_ptr const_expr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  nusmv_assert(Nil != const_expr);
  return ExprMgr_resolve(exprs, self->st, node_get_type(const_expr),
                      car(const_expr), cdr(const_expr));
}

/*!
  \brief This function is a private service of BaseEvaluator_eval

  This function is a private service of BaseEvaluator_eval
*/
static node_ptr
base_evaluator_make_failure(const NuSMVEnv_ptr env,
                            const char* tmpl, node_ptr symbol)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  char *symb_str = sprint_node(wffprint, symbol);
  char *buf = ALLOC(char, 1 + strlen(tmpl) + strlen(symb_str));
  node_ptr res;

  sprintf(buf, tmpl, symb_str);
  res = ErrorMgr_failure_make(errmgr, buf, FAILURE_UNSPECIFIED, -1);

  FREE(buf);
  FREE(symb_str);

  return res;
}

/**AutomaticEnd***************************************************************/
