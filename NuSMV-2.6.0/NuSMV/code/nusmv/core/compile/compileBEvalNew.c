/* ---------------------------------------------------------------------------


  This file is part of the ``compile'' package of NuSMV version 2.
  Copyright (C) 2000-2014 by FBK-irst.

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
  \author Alessandro Cimatti and Marco Roveri

  \brief Convertion of a scalar expression into a boolean expression.

  This file provides routines to convert a scalar expressions into a
  corresponding boolean expression.

*/

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/enc/operators.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/utils_io.h" /* for indent_node */
#include "nusmv/core/utils/range.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/WordNumberMgr.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"

#define ENABLE_PN_BEFORE_CONVERSION 1

#if ENABLE_PN_BEFORE_CONVERSION
#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#endif

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/
#define _IS_LEAF(a)                  \
  ((NULL != a) &&                    \
   ((NUMBER   == node_get_type(a)) || \
    (FALSEEXP == node_get_type(a)) || \
    (TRUEEXP  == node_get_type(a)) || \
    (ATOM     == node_get_type(a)) || \
    (DOT      == node_get_type(a)) || \
    (CONS     == node_get_type(a)) || \
    (FAILURE  == node_get_type(a)) ))

#define _IS_ITE(a)                      \
  (((IFTHENELSE == node_get_type(a)) || \
    (CASE == node_get_type(a))) &&      \
   (COLON == node_get_type(car(a))))
#define _ITE_GET_COND(a) car(car(a))
#define _ITE_GET_THEN(a) cdr(car(a))
#define _ITE_GET_ELSE(a) cdr(a)

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/** This structure is used only when calling compile_is_booleanizable_aux */
typedef struct CompileIsBooleanizableAuxParams_TAG {
  SymbTable_ptr st;
  NodeMgr_ptr nodemgr;
  ErrorMgr_ptr errmgr;
  boolean word_unbooleanizable;
  hash_ptr cache;
} CompileIsBooleanizableAuxParams;


/** This pointer to function is used to refer to thefunctions used by
    _apply to evaluate the expressions into boolean ones. */
typedef Expr_ptr (*EPFMEE)(ExprMgr_ptr, Expr_ptr, Expr_ptr);

/** This is a structure used by _apply to store information used
    within the recursion to evaluate expressions. */
typedef struct BStructInfo_TAG {
  ExprMgr_ptr expm;
  SymbTable_ptr st;
  NodeMgr_ptr nodemgr;
  hash_ptr apply_hash;
} BStructInfo;

typedef struct BStructInfo_TAG * BStructInfo_ptr;

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static node_ptr
expr2bexpr_recur(BddEnc_ptr enc, SymbLayer_ptr,
                 node_ptr, node_ptr, boolean, hash_ptr);
static node_ptr
scalar_atom2bexpr(BddEnc_ptr, SymbLayer_ptr,
                  node_ptr, node_ptr, boolean);

static node_ptr
expr2bexpr_ite(BddEnc_ptr enc, SymbLayer_ptr det_layer,
               node_ptr expr, node_ptr context, boolean in_next,
               hash_ptr expr2bexpr);

static node_ptr
expr2bexpr_word_ite_aux(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                        node_ptr expr, node_ptr context,
                        boolean in_next, hash_ptr expr2bexpr);

static boolean
compile_is_booleanizable_aux(
                   node_ptr expr,
                   node_ptr context,
                   const CompileIsBooleanizableAuxParams* params);

static node_ptr make_key(NodeMgr_ptr nodemgr, node_ptr expr, boolean a,
                         boolean b);


static void
expr2bexpr_hash_insert_entry(hash_ptr hash, NodeMgr_ptr nodemgr,
                             node_ptr expr, node_ptr ctx, node_ptr bexpr,
                             boolean a, boolean b);

static node_ptr
expr2bexpr_hash_lookup_entry(hash_ptr hash, NodeMgr_ptr nodemgr,
                             node_ptr expr, node_ptr ctx, boolean a,
                             boolean b);

static hash_ptr compile_beval_get_handled_hash(SymbTable_ptr, char*);

static node_ptr expr2bexpr_shift_left(BddEnc_ptr enc,
                                      SymbLayer_ptr det_layer,
                                      node_ptr context,
                                      boolean in_next,
                                      node_ptr a,
                                      node_ptr b,
                                      node_ptr def_case,
                                      int numWidth,
                                      hash_ptr expr2bexpr);

static node_ptr expr2bexpr_shift_right(BddEnc_ptr enc,
                                       SymbLayer_ptr det_layer,
                                       node_ptr context,
                                       boolean in_next,
                                       node_ptr a,
                                       node_ptr b,
                                       node_ptr def_case,
                                       int numWidth,
                                       hash_ptr expr2bexpr);

static node_ptr expr2bexpr_shift(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                                 node_ptr expr, node_ptr context,
                                 boolean in_next, hash_ptr expr2bexpr);

static node_ptr
expr2bexpr_recur_binary(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                        node_ptr expr, node_ptr context,
                        boolean in_next, hash_ptr expr2bexpr);

static node_ptr
expr2bexpr_recur_unary(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                       node_ptr expr, node_ptr context,
                       boolean in_next, hash_ptr expr2bexpr);

static node_ptr expr2bexpr_rotate(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                                  node_ptr expr, node_ptr context,
                                  boolean in_next, hash_ptr expr2bexpr);

static node_ptr
expr2bexpr_get_shift_def_value(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                               node_ptr context, boolean in_next,
                               node_ptr a, node_ptr b, int numWidth,
                               node_ptr defaultBit, hash_ptr expr2bexpr);

static void
init_compile_is_booleanizable_params(CompileIsBooleanizableAuxParams* self,
                                     SymbTable_ptr st,
                                     boolean word_unbooleanizable,
                                     hash_ptr cache);


static Expr_ptr expr2bexpr_cast_word1(BddEnc_ptr enc, SymbLayer_ptr det_layer, node_ptr expr, node_ptr context, boolean in_next, hash_ptr expr2bexpr);
static Expr_ptr expr2bexpr_extend(BddEnc_ptr enc, SymbLayer_ptr det_layer, node_ptr expr, node_ptr context, boolean in_next, hash_ptr expr2bexpr);
static Expr_ptr expr2bexpr_bitselect(BddEnc_ptr enc, SymbLayer_ptr det_layer, node_ptr expr, node_ptr context, boolean in_next, hash_ptr expr2bexpr);
static Expr_ptr expr2bexpr_resize(BddEnc_ptr enc, SymbLayer_ptr det_layer, node_ptr expr, node_ptr context, boolean in_next, hash_ptr expr2bexpr);
static Expr_ptr expr2bexpr_eqdef(BddEnc_ptr enc, SymbLayer_ptr det_layer, node_ptr expr, node_ptr context, boolean in_next, hash_ptr expr2bexpr);
static Expr_ptr expr2bexpr_leaf(BddEnc_ptr enc, SymbLayer_ptr det_layer, node_ptr expr, node_ptr context, boolean in_next, hash_ptr expr2bexpr);
static node_ptr _mk_key(BStructInfo_ptr aux, EPFMEE fn, Expr_ptr a, Expr_ptr b);
static Expr_ptr _cache_lookup(BStructInfo_ptr aux, node_ptr key);
static void _cache_insert(BStructInfo_ptr aux, node_ptr key, Expr_ptr res);

static Expr_ptr _apply(BStructInfo_ptr aux, Expr_ptr a, Expr_ptr b, EPFMEE fn);

static Expr_ptr expr_plus_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b);
static Expr_ptr expr_times_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b);
static Expr_ptr expr_divide_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b);
static Expr_ptr expr_minus_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b);
static Expr_ptr expr_mod_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b);
static Expr_ptr expr_equal_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b);
static Expr_ptr expr_notequal_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b);
static Expr_ptr expr_le_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b);
static Expr_ptr expr_ge_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b);
static Expr_ptr expr_lt_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b);
static Expr_ptr expr_gt_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b);
static Expr_ptr expr_eqdef_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b);
static Expr_ptr expr_union_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b);
static Expr_ptr expr_setin_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

Expr_ptr Compile_expr2bexpr(BddEnc_ptr enc,
                            SymbLayer_ptr det_layer,
                            Expr_ptr expr)
{
  node_ptr res;
  int temp;
  SymbTable_ptr symb_table;
  hash_ptr expr2bexpr;

  if (expr == EXPR(NULL)) return expr;

  temp = nusmv_yylineno;

  symb_table = BaseEnc_get_symb_table(BASE_ENC(enc));

  expr2bexpr = compile_beval_get_handled_hash(symb_table,
                                              ST_BEVAL_EXPR2BEXPR_HASH);

  nusmv_yylineno = node_get_lineno( NODE_PTR(expr) );

#if ENABLE_PN_BEFORE_CONVERSION
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
    const OptsHandler_ptr opts =
      OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

    if (opt_boolconv_uses_prednorm(opts)) {
      PredicateNormaliser_ptr pn;
      pn = PredicateNormaliser_create(symb_table);
      expr = PredicateNormaliser_normalise_expr(pn, expr);
      PredicateNormaliser_destroy(pn);
    }
  }
#endif

  res = expr2bexpr_recur(enc, det_layer, expr, Nil, false, expr2bexpr);
  nusmv_yylineno = temp;
  return EXPR(res);
}


Expr_ptr Compile_detexpr2bexpr(BddEnc_ptr enc, Expr_ptr expr)
{
  node_ptr res;
  int temp = nusmv_yylineno;
  SymbTable_ptr symb_table;
  hash_ptr expr2bexpr;

  if (expr == EXPR(NULL)) return expr;

  nusmv_yylineno = node_get_lineno(expr);

  symb_table = BaseEnc_get_symb_table(BASE_ENC(enc));

  expr2bexpr = compile_beval_get_handled_hash(symb_table,
                                              ST_BEVAL_EXPR2BEXPR_HASH);

  res = expr2bexpr_recur(enc, SYMB_LAYER(NULL), expr, Nil, false, expr2bexpr);

  nusmv_yylineno = temp;
  return EXPR(res);
}


Expr_ptr Compile_detexpr2bexpr_list(BddEnc_ptr enc, Expr_ptr expr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr list, res, iter;
  boolean is_next = false;

  SymbTable_ptr symb_table;
  hash_ptr expr2bexpr;

  symb_table = BaseEnc_get_symb_table(BASE_ENC(enc));

  expr2bexpr = compile_beval_get_handled_hash(symb_table,
                                              ST_BEVAL_EXPR2BEXPR_HASH);

  /* get rid of top level next */
  if (expr != Nil && NEXT == node_get_type(expr)) {
    is_next = true;
    expr = car(expr);
  }

  /* NOTE: to keep the order, at first, the original list is reversed
     and then it is processed. During booleanization the order is reversed
     again, i.e. in the returned expressions the original order is restored.
  */
  list = Nil;
  while (expr != Nil &&
         (node_get_type(expr) == AND || node_get_type(expr) == CONS)) {
    list = cons(nodemgr, car(expr), list);
    expr = cdr(expr);
  }

  /* process the last element */
  res = expr2bexpr_recur(enc, SYMB_LAYER(NULL), expr, Nil, is_next, expr2bexpr);

  /* process the rest of the list in the loop */
  for (iter = list; Nil != iter; iter = cdr(iter)) {
    node_ptr elem;

    /* lazy evaluation : avoid computation of other elements if possible */
    if (res != Nil && ExprMgr_is_false(exprs, res)) {
      break; /* fully avoid computation of the rest */
    }

    elem = expr2bexpr_recur(enc, SYMB_LAYER(NULL), car(iter), Nil, is_next,
                            expr2bexpr);
    /* merge new element with existing results */
    res = ExprMgr_and(exprs, elem, res);
  }

  /* free the reversed list */
  free_list(nodemgr, list);


  return EXPR(res);
}


boolean Compile_is_expr_booleanizable(const SymbTable_ptr st,
                                      node_ptr expr,
                                      boolean word_unbooleanizable,
                                      hash_ptr cache)
{
  CompileIsBooleanizableAuxParams params;
  hash_ptr to_use;
  boolean res;

  if ((hash_ptr)NULL == cache) {
    to_use = new_assoc();
  }
  else {
    to_use = cache;
  }

  init_compile_is_booleanizable_params(&params,
                                       st, word_unbooleanizable, to_use);

  res = compile_is_booleanizable_aux(expr, Nil /*context*/, &params);

  if ((hash_ptr)NULL == cache) {
    free_assoc(to_use);
  }

  return res;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Creates the encoding of the left-shifting circuit for
  words. numWidth is the width of b or -1 if b is not a word
  (it can be a number)
*/
static node_ptr expr2bexpr_shift_left(BddEnc_ptr enc,
                                      SymbLayer_ptr det_layer,
                                      node_ptr context,
                                      boolean in_next,
                                      node_ptr a,
                                      node_ptr b,
                                      node_ptr def_case,
                                      int numWidth,
                                      hash_ptr expr2bexpr)
{
  /* creates shift circuit */
  array_t* va;
  node_ptr res;
  unsigned long long maxShift;
  int width, i;
  SymbTable_ptr symb_table = BaseEnc_get_symb_table(BASE_ENC(enc));
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  width = node_get_int(cdr(a));
  va = node_word_to_array(a);
  if (numWidth > 0) maxShift = (2ULL << (numWidth-1)) - 1;
  else maxShift = ~0U; /* value for sure bigger than any possible width */

  res = Nil;
  for (i=0; i<width; ++i) {
    node_ptr bit = def_case;
    int k;

    for (k=MIN(i,maxShift); k >= 0; --k) {
      node_ptr beqk;

      if (numWidth > 0) {/* b is a word */
        node_ptr wk = node_word_create_from_integer(k, numWidth, env);
        beqk = node_word_equal(b, wk, env);
      }
      else {/* b is not a word */
        beqk = expr2bexpr_recur(enc,
                                det_layer,
                                find_node(nodemgr, EQUAL,
                                          b,
                                          find_node(nodemgr, NUMBER,
                                                    NODE_FROM_INT(k),
                                                    Nil)),
                                context, in_next, expr2bexpr);
      }

      bit = ExprMgr_ite(exprs, beqk, array_fetch(node_ptr, va, i-k), bit,
                        symb_table);
    }

    /* MR: cons vs find_node(CONS, ) */
    res = cons(nodemgr, bit, res); /* msb at the top */
  }
  array_free(va);

  return node_word_create_from_list(res, width, env);
}


/*!
  \brief Creates the encoding of the unsigned right-shifting circuit
  for words

  \se numWidth is the width of b or -1 if b is not a word
  (it can be a number)
*/
static node_ptr expr2bexpr_shift_right(BddEnc_ptr enc,
                                       SymbLayer_ptr det_layer,
                                       node_ptr context,
                                       boolean in_next,
                                       node_ptr a,
                                       node_ptr b,
                                       node_ptr def_case,
                                       int numWidth,
                                       hash_ptr expr2bexpr)
{
  /* creates shift circuit */
  array_t* va;
  node_ptr res;
  unsigned long long maxShift;
  int width, i;
  SymbTable_ptr symb_table = BaseEnc_get_symb_table(BASE_ENC(enc));
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  width = node_get_int(cdr(a));
  va = node_word_to_array(a);
  if (numWidth > 0) maxShift = (2ULL << (numWidth-1)) - 1;
  else maxShift = ~0U; /* value for sure bigger than any possible width */

  res = Nil;
  for (i = 0; i < width; ++i) {
    node_ptr bit = def_case;
    int k;

    for (k = MIN(maxShift, width - i - 1); k >= 0; --k) {
      node_ptr beqk;

      if (numWidth > 0) { /* b is a word */
        node_ptr wk = node_word_create_from_integer(k, numWidth, env);
        beqk = node_word_equal(b, wk, env);
      }
      else { /* b is not a word */
        beqk = expr2bexpr_recur(enc,
                                det_layer,
                                find_node(nodemgr, EQUAL,
                                          b,
                                          find_node(nodemgr, NUMBER,
                                                    NODE_FROM_INT(k),
                                                    Nil)),
                                context,
                                in_next,
                                expr2bexpr);
      }

      bit = ExprMgr_ite(exprs, beqk, array_fetch(node_ptr, va, i+k), bit,
                        symb_table);
    } /* for k */

    /* MR: cons vs find_node(CONS, ) */
    res = cons(nodemgr, bit, res); /* msb at the top */
  } /* for i */

  array_free(va);

  return node_word_create_from_list(res, width, env);
}


/*!
  \brief Private service for shifting operations.

  It creates a default error case.  numWidth is the width of b or -1
  if b is not a word.  defaultBit is a default value of a
  bit. Typically it is 0 and the highest bit of a for right signed
  shift.  */
static node_ptr
expr2bexpr_get_shift_def_value(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                               node_ptr context, boolean in_next,
                               node_ptr a, node_ptr b, int numWidth,
                               node_ptr defaultBit, hash_ptr expr2bexpr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  SymbTable_ptr symb_table = BaseEnc_get_symb_table(BASE_ENC(enc));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  node_ptr error, res;
  int width;

  width = node_get_int(cdr(a));

  error =
    ErrorMgr_failure_make(errmgr,
                          "Right operand of shift operator is out of range",
                          FAILURE_UNSPECIFIED,
                          node_get_lineno(ErrorMgr_get_the_node(errmgr)));

  if (numWidth > 0) { /* b is a word */
    /* right operand can be only *unsigned* word => do not compare with 0 */
    if ((width >= (2ULL<<(numWidth-1)) - 1)) { /* width >= (2^numWidth - 1). */
      /* 'number' cannot represent values > width. Thus
         a check that number<=width is not required. */
      res = defaultBit;
    }
    else {
      node_ptr cond = node_word_unsigned_less_equal(b,
                                  node_word_create_from_integer(width,
                                                                numWidth, env),
                                                    env);
      res = ExprMgr_ite(exprs, cond, defaultBit, error, symb_table);
    }
  }
  else { /* b is not a word */
    node_ptr bge0, blew, cond;
    bge0 = expr2bexpr_recur(enc, det_layer,
                            find_node(nodemgr, GE, b,
                                      find_node(nodemgr, NUMBER,
                                                NODE_FROM_INT(0), Nil)),
                            context, in_next, expr2bexpr);
    blew = expr2bexpr_recur(enc, det_layer,
                            find_node(nodemgr, LE, b,
                                      find_node(nodemgr, NUMBER,
                                                NODE_FROM_INT(width), Nil)),
                            context, in_next, expr2bexpr);
    cond = ExprMgr_and(exprs, bge0, blew);
    res = ExprMgr_ite(exprs, cond, defaultBit, error, symb_table);
  }

  return res;
}


/*!
  \brief High-level function for shifting operations

  This function is called directly by the booleanizer
*/
static node_ptr expr2bexpr_shift(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                                 node_ptr expr, node_ptr context,
                                 boolean in_next, hash_ptr expr2bexpr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

  TypeChecker_ptr tc;
  SymbType_ptr ta, tb;
  node_ptr a, b;
  /* TODO[MD] Here width is computed, but never used */
  int width, numWidth;
  int etype = node_get_type(expr);

  nusmv_assert(etype == LSHIFT || etype == RSHIFT);

  tc = BaseEnc_get_type_checker(BASE_ENC(enc));
  ta = TypeChecker_get_expression_type(tc, car(expr), context);

  if (ta == SYMB_TYPE(NULL) || SymbType_is_error(ta)) {
    ErrorMgr_internal_error(errmgr,
                            "expr2bexpr_shift: operand has invalid type");
  }

  nusmv_assert(SymbType_is_word(ta));
  a = expr2bexpr_recur(enc, det_layer, car(expr), context, in_next, expr2bexpr);
  _CHECK_WORD(a);

  if (node_get_type(a) == NUMBER_UNSIGNED_WORD ||
      node_get_type(a) == NUMBER_SIGNED_WORD) {
    width = WordNumber_get_width(WORD_NUMBER(car(a)));
  }
  else width = node_get_int(cdr(a));
  UNUSED_VAR(width);

  tb = TypeChecker_get_expression_type(tc, cdr(expr), context);
  if (SymbType_is_unsigned_word(tb)) {
    b = expr2bexpr_recur(enc, det_layer, cdr(expr), context, in_next,
                         expr2bexpr);
    _CHECK_WORD(b);
    if (node_get_type(b) == NUMBER_UNSIGNED_WORD) {
      /* b is a constant word: converts it to integer */
      b = find_node(nodemgr, NUMBER,
                    NODE_FROM_INT(
          (int) WordNumber_get_unsigned_value(WORD_NUMBER(car(b)))), Nil);
      numWidth = -1;
    }
    else numWidth = node_get_int(cdr(b));
  }
  else {
    /* only unsigned word or int allowed */
    nusmv_assert(SymbType_get_tag(tb) == SYMB_TYPE_INTEGER
                 || SymbType_is_boolean(tb));
    b = cdr(expr);
    numWidth = -1;
  }

  /* checks if constant word and constant shift value */
  if (node_get_type(a) == NUMBER_UNSIGNED_WORD ||
      node_get_type(a) == NUMBER_SIGNED_WORD) {
    int bits;
    switch (node_get_type(b)) {
    case NUMBER: bits = node_get_int(b); break;
    case NUMBER_UNSIGNED_WORD:
      bits = WordNumber_get_unsigned_value(WORD_NUMBER(car(b))); break;
    case NUMBER_SIGNED_WORD:
      bits = WordNumber_get_signed_value(WORD_NUMBER(car(b))); break;
    case TRUEEXP: bits = 1; break;
    case FALSEEXP: bits = 0; break;
    default: bits = -1;
    }

    if (bits == 0) return a;
    if (bits > 0) {
      WordNumber_ptr ws;

      /* checks width */
      if (bits > WordNumber_get_width(WORD_NUMBER(car(a)))) {
        ErrorMgr_error_wrong_word_operand(errmgr,
                                     "Right operand of shift is out of range",
                                     expr);
      }

      if (etype == LSHIFT) {
        ws = WordNumberMgr_left_shift(words, WORD_NUMBER(car(a)), bits);
      }
      else {
        ws = (node_get_type(a) == NUMBER_UNSIGNED_WORD) ?
          WordNumberMgr_unsigned_right_shift(words, WORD_NUMBER(car(a)), bits)
          : WordNumberMgr_signed_right_shift(words, WORD_NUMBER(car(a)), bits);
      }
      return find_node(nodemgr, node_get_type(a), (node_ptr) ws, Nil);
    }

    /* fallback to default code */
    a = node_word_create_from_wordnumber(WORD_NUMBER(car(a)), env);
  }  /* else bits=-1 and fallback to default code */

  ErrorMgr_set_the_node(errmgr, expr); /* set node for error reporting */

  { /* construct the default value.  For right shift of signed
       words the default value is the highest bit of a.  For all
       other case default value is 0.  */
    node_ptr def_case;
    node_ptr res;

    def_case = car(car(a)); /* highest bit of a */
    def_case =
      expr2bexpr_get_shift_def_value(enc, det_layer, context, in_next, a, b,
                                     numWidth,
                                     etype == RSHIFT &&
                                     SymbType_is_signed_word(ta)
                                     ? def_case : find_node(nodemgr, FALSEEXP,
                                                            Nil, Nil),
                                     expr2bexpr);

    if (etype == LSHIFT) {
      res = expr2bexpr_shift_left(enc, det_layer, context, in_next,
                                  a, b, def_case, numWidth, expr2bexpr);
    }
    else {
      res = expr2bexpr_shift_right(enc, det_layer, context, in_next,
                                   a, b, def_case, numWidth, expr2bexpr);
    }
    return res;
  }
}


/*!
  \brief High-level function for rotating words

  This function is called directly by the booleanizer
*/
static node_ptr expr2bexpr_rotate(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                                  node_ptr expr, node_ptr context,
                                  boolean in_next, hash_ptr expr2bexpr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  TypeChecker_ptr tc;
  SymbType_ptr ta, tb;
  node_ptr a, b;
  int width, numWidth;
  int etype = node_get_type(expr);
  SymbTable_ptr symb_table = BaseEnc_get_symb_table(BASE_ENC(enc));

  nusmv_assert(etype == LROTATE || etype == RROTATE);

  tc = BaseEnc_get_type_checker(BASE_ENC(enc));
  ta = TypeChecker_get_expression_type(tc, car(expr), context);

  if (ta == SYMB_TYPE(NULL) || SymbType_is_error(ta)) {
    ErrorMgr_internal_error(errmgr,
                            "expr2bexpr_rotate: operand has invalid type");
  }

  nusmv_assert(SymbType_is_word(ta));
  a = expr2bexpr_recur(enc, det_layer, car(expr), context, in_next, expr2bexpr);
  _CHECK_WORD(a);

  if (node_get_type(a) == NUMBER_UNSIGNED_WORD ||
      node_get_type(a) == NUMBER_SIGNED_WORD) {
    width = WordNumber_get_width(WORD_NUMBER(car(a)));
  }
  else width = node_get_int(cdr(a));

  tb = TypeChecker_get_expression_type(tc, cdr(expr), context);
  if (SymbType_is_word(tb)) {
    b = expr2bexpr_recur(enc, det_layer, cdr(expr), context, in_next,
                         expr2bexpr);
    _CHECK_WORD(b);

    if (node_get_type(b) == NUMBER_UNSIGNED_WORD) {
      /* b is a constant word: converts it to integer */
      b = find_node(nodemgr, NUMBER,
                    NODE_FROM_INT(
            (int) WordNumber_get_unsigned_value(WORD_NUMBER(car(b)))), Nil);
      numWidth = -1;
    }
    else if (node_get_type(b) == NUMBER_SIGNED_WORD) {
      /* b is a constant word: converts it to integer */
      b = find_node(nodemgr, NUMBER,
                    NODE_FROM_INT(
            (int) WordNumber_get_signed_value(WORD_NUMBER(car(b)))), Nil);
      numWidth = -1;
    }
    else numWidth = node_get_int(cdr(b));
  }
  else { /* an integer value */
    b = cdr(expr);
    numWidth = -1;
  }

  /* checks if constant word and constant shift value */
  if (node_get_type(a) == NUMBER_UNSIGNED_WORD ||
      node_get_type(a) == NUMBER_SIGNED_WORD) {
    int bits;
    switch (node_get_type(b)) {
    case NUMBER: bits = node_get_int(b); break;
    case NUMBER_UNSIGNED_WORD:
      bits = WordNumber_get_unsigned_value(WORD_NUMBER(car(b))); break;
    case NUMBER_SIGNED_WORD:
      bits = WordNumber_get_signed_value(WORD_NUMBER(car(b))); break;
    case TRUEEXP: bits = 1; break;
    case FALSEEXP: bits = 0; break;
    default: bits = -1;
    }

    if (bits == 0) return a;
    if (bits > 0) {
      WordNumber_ptr ws;
      if (etype == LROTATE) {
        ws = WordNumberMgr_left_rotate(words, WORD_NUMBER(car(a)), bits);
      }
      else ws =WordNumberMgr_right_rotate(words, WORD_NUMBER(car(a)), bits);
      return find_node(nodemgr, node_get_type(a), (node_ptr) ws, Nil);
    }

    /* fallback to default code */
    a = node_word_create_from_wordnumber(WORD_NUMBER(car(a)), env);
  }  /* else bits=-1 and fallback to default code */

  {
    array_t* va = node_word_to_array(a);
    node_ptr err_case;
    node_ptr res;
    unsigned long long maxRotate;
    int i;

    err_case = ErrorMgr_failure_make(errmgr,
                                     "Right operand of rotate operation is out of range",
                                     FAILURE_UNSPECIFIED,
                                     (int) node_get_lineno(expr));

    if (numWidth > 0) maxRotate = MIN(((2ULL << (numWidth-1)) - 1), width);
    else maxRotate = width;

    res = Nil;
    /* performs the rotation */
    for (i = 0; i < width; ++i) {
      node_ptr bit = err_case;
      int k;

      for (k = maxRotate; k >= 0; --k) {
        node_ptr beqk;
        int p;

        if (numWidth > 0) {
          node_ptr wk = node_word_create_from_integer(k, numWidth, env);
          beqk = node_word_equal(b, wk, env);
        }
        else { /* b is not a word */
          beqk = expr2bexpr_recur(enc, det_layer,
                                  find_node(nodemgr, EQUAL, b,
                                            find_node(nodemgr, NUMBER,
                                                      NODE_FROM_INT(k), Nil)),
                                  context, in_next, expr2bexpr);
        }

        if (etype == LROTATE) {
          p = (i >= k) ? i-k : i-k+width;
        }
        else { /* RROTATE */
          p = ((i+k) % width);
        }
        bit = ExprMgr_ite(exprs, beqk, array_fetch(node_ptr, va, p), bit,
                          symb_table);
      } /* for k */

      /* MR: cons vs find_node(CONS, ) */
      res = cons(nodemgr, bit, res); /* msb at the top */
    } /* for i */

    array_free(va);

    return node_word_create_from_list(res, width, env);
  }
}


/*!
  \brief This function booleanizes an unary expression.

  In order to booleanize the unary expression, at first process the
  argument. Then for words apply a corresponding unary word function,
  for all other type just create exp of the same kind with find_node.

  \se None
*/
static node_ptr expr2bexpr_recur_unary(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                                       node_ptr expr, node_ptr context,
                                       boolean in_next, hash_ptr expr2bexpr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  TypeChecker_ptr tc = BaseEnc_get_type_checker(BASE_ENC(enc));
  SymbType_ptr ta;

  ta = TypeChecker_get_expression_type(tc, car(expr), context);

  if (ta == SYMB_TYPE(NULL) || SymbType_is_error(ta)) {
    ErrorMgr_internal_error(errmgr,
                            "expr2bexpr_recur_unary: operand has invalid type");
  }

  /* if word, applies the given operation to its encoding */
  if (SymbType_is_word(ta)) {
    node_ptr a = expr2bexpr_recur(enc, det_layer, car(expr), context, in_next,
                                  expr2bexpr);
    node_ptr (*op)(node_ptr, const NuSMVEnv_ptr);
    _CHECK_WORD(a);

    if (node_get_type(a) == NUMBER_UNSIGNED_WORD ||
        node_get_type(a) == NUMBER_SIGNED_WORD) {
      /* constant */
      WordNumber_ptr wn = WORD_NUMBER(car(a));
      switch (node_get_type(expr)) {
      case CAST_BOOL:
        nusmv_assert(WordNumber_get_width(wn) == 1);
        if (WordNumber_get_unsigned_value(wn) != 0) {
          return find_node(nodemgr, TRUEEXP, Nil, Nil);
        }
        else return find_node(nodemgr, FALSEEXP, Nil, Nil);

      case NOT:
        wn = WordNumberMgr_not(words, wn);
        return find_node(nodemgr, node_get_type(a), (node_ptr) wn, Nil);

      case UMINUS:
        wn = WordNumberMgr_unary_minus(words, wn);
        return find_node(nodemgr, node_get_type(a), (node_ptr) wn, Nil);

      default:
        ErrorMgr_internal_error(errmgr,
                                "expr2bexpr_recur_unary: illegal expression");
      }
    }

    /* not a constant */
    switch (node_get_type(expr)) {
    case CAST_BOOL: op = node_word_cast_bool; break;
    case NOT:       op = node_word_not; break;
    case UMINUS:    op = node_word_uminus; break;
    default:
      ErrorMgr_internal_error(errmgr,
                              "expr2bexpr_recur_unary: illegal expression");
    }

    ErrorMgr_set_the_node(errmgr, expr);
    return op(a, env);
  }

  {  /* not a leaf, applies the standard encoding */
    node_ptr v = expr2bexpr_recur(enc, det_layer, car(expr), context, in_next,
                                  expr2bexpr);
    if (node_get_type(expr) == NOT) return ExprMgr_not(exprs, v);
    return find_node(nodemgr, node_get_type(expr), v, Nil);
  }
}


/*!
  \brief This function booleanizes a binary expression.

  For words: at first convert to boolean the arguments and then apply
  the corresponding word function.  For all other types if the kind of
  an expression is arithmetic or relational converts the expression
  down to a boolean formula by executing the given
  operations. Otherwise it processes the arguments and creates a new
  expression of the same kind with find_node.

  \se None
*/
static node_ptr
expr2bexpr_recur_binary(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                        node_ptr expr, node_ptr context,
                        boolean in_next, hash_ptr expr2bexpr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  /* TODO: */
  TypeChecker_ptr tc = BaseEnc_get_type_checker(BASE_ENC(enc));
  SymbType_ptr t_left, t_right;
  node_ptr left = car(expr);
  node_ptr right = cdr(expr);
  node_ptr res = NODE_PTR(-1); /* for sure incorrect value to catch bugs */

  t_left = TypeChecker_get_expression_type(tc, left, context);
  t_right = TypeChecker_get_expression_type(tc, right, context);

  if (t_left == SYMB_TYPE(NULL) || t_right == SYMB_TYPE(NULL) ||
      SymbType_is_error(t_left) || SymbType_is_error(t_right)) {
    ErrorMgr_internal_error(errmgr,
                        "expr2bexpr_recur_binary: operands have invalid types");
  }

  /* ------------------------------------------------------------- */
  /* if words, apply the given operation to their encodings. */
  if (SymbType_is_word(t_left) || SymbType_is_word(t_right)) {
    node_ptr a;
    node_ptr b;

    nusmv_assert(UNION != node_get_type(expr));

    /* if one operand is word and other is boolean => convert bool to word
       (it is required by below word functions) */
    if (SymbType_is_boolean(t_right)) {
      right = find_node(nodemgr, CAST_WORD1, right, Nil);
      t_right = t_left;
    }
    else if (SymbType_is_boolean(t_left)) {
      left = find_node(nodemgr, CAST_WORD1, left, Nil);
      t_left = t_right;
    }

    a = expr2bexpr_recur(enc, det_layer, left, context, in_next, expr2bexpr);
    b = expr2bexpr_recur(enc, det_layer, right, context, in_next, expr2bexpr);

    _CHECK_WORD(a);
    _CHECK_WORD(b);

    if ((node_get_type(a) == NUMBER_UNSIGNED_WORD ||
         node_get_type(a) == NUMBER_SIGNED_WORD) &&
        (node_get_type(b) == NUMBER_UNSIGNED_WORD ||
         node_get_type(b) == NUMBER_SIGNED_WORD)) {
      /* constant */
      boolean (*bop)(WordNumber_ptr, WordNumber_ptr) = NULL;
      WordNumber_ptr (*wop)
        (WordNumberMgr_ptr, WordNumber_ptr, WordNumber_ptr) = NULL;
      int wtype = node_get_type(a);

      switch (node_get_type(expr)) {
        /* boolean operations */
      case EQDEF: bop = WordNumber_equal; break;
      case EQUAL: bop = WordNumber_equal; break;
      case NOTEQUAL: bop = WordNumber_not_equal; break;
      case LT: bop = SymbType_is_unsigned_word(t_left)
          ? WordNumber_unsigned_less : WordNumber_signed_less;
        break;
      case LE: bop = SymbType_is_unsigned_word(t_left) ?
          WordNumber_unsigned_less_or_equal
          : WordNumber_signed_less_or_equal;
        break;
      case GT: bop = SymbType_is_unsigned_word(t_left)
          ? WordNumber_unsigned_greater : WordNumber_signed_greater;
        break;
      case GE: bop = SymbType_is_unsigned_word(t_left) ?
          WordNumber_unsigned_greater_or_equal
          : WordNumber_signed_greater_or_equal;
        break;

        /* logical and arithmetical */
      case CONCATENATION:
        wop = WordNumberMgr_concatenate; wtype = NUMBER_UNSIGNED_WORD;
        break;

      case AND: wop = WordNumberMgr_and; break;
      case OR: wop = WordNumberMgr_or; break;
      case XOR: wop = WordNumberMgr_xor; break;
      case XNOR: wop = WordNumberMgr_xnor; break;
      case IMPLIES: wop = WordNumberMgr_implies; break;
      case IFF: wop = WordNumberMgr_iff; break;
      case PLUS: wop = WordNumberMgr_plus; break;
      case MINUS: wop = WordNumberMgr_minus; break;
      case TIMES: wop = WordNumberMgr_times; break;
      case DIVIDE: wop = SymbType_is_unsigned_word(t_left)
          ? WordNumberMgr_unsigned_divide : WordNumberMgr_signed_divide;
        break;
      case MOD: wop = SymbType_is_unsigned_word(t_left)
          ? WordNumberMgr_unsigned_mod : WordNumberMgr_signed_mod;
        break;

      default:
        ErrorMgr_internal_error(errmgr,
                            "expr2bexpr_recur_binary: illegal word expression");
      }

      /* not both wop and bop */
      if (NULL != bop) {
        nusmv_assert(NULL == wop);
        return (bop(WORD_NUMBER(car(a)), WORD_NUMBER(car(b)))) ?
          find_node(nodemgr, TRUEEXP, Nil, Nil) : find_node(nodemgr, FALSEEXP,
                                                            Nil, Nil);
      }
      nusmv_assert(wop != NULL);
      /* according to the grammar, the result keeps the sign of
         the left operand */
      return find_node(nodemgr, wtype,
                       (node_ptr) wop(words, WORD_NUMBER(car(a)),
                                             WORD_NUMBER(car(b))),
                       Nil);
    }
    else {  /* not both constant word numbers */
      NPFNNE op;

      /* encodes the possible constant word (only one is admitted here) */
      if (node_get_type(a) == NUMBER_UNSIGNED_WORD ||
          node_get_type(a) == NUMBER_SIGNED_WORD) {
        a = node_word_create_from_wordnumber(WORD_NUMBER(car(a)), env);
      }
      else if (node_get_type(b) == NUMBER_UNSIGNED_WORD ||
               node_get_type(b) == NUMBER_SIGNED_WORD) {
        b = node_word_create_from_wordnumber(WORD_NUMBER(car(b)), env);
      }

      switch (node_get_type(expr)) {
      case EQDEF: op = node_word_equal; break;
      case CONCATENATION: op = node_word_concat; break;
      case AND: op = node_word_and; break;
      case OR: op = node_word_or; break;
      case XOR: op = node_word_xor; break;
      case XNOR: op = node_word_xnor; break;
      case IMPLIES: op = node_word_implies; break;
      case IFF: op = node_word_iff; break;
      case EQUAL: op = node_word_equal; break;
      case NOTEQUAL: op = node_word_notequal; break;
      case PLUS: op = node_word_plus; break;
      case MINUS: op = node_word_minus; break;
      case LT: op = SymbType_is_unsigned_word(t_left)
          ? node_word_unsigned_less : node_word_signed_less;
        break;
      case LE: op = SymbType_is_unsigned_word(t_left)
          ? node_word_unsigned_less_equal : node_word_signed_less_equal;
        break;
      case GT: op = SymbType_is_unsigned_word(t_left)
          ? node_word_unsigned_greater : node_word_signed_greater;
        break;
      case GE: op = SymbType_is_unsigned_word(t_left)
          ? node_word_unsigned_greater_equal : node_word_signed_greater_equal;
        break;

      case TIMES: op = node_word_times; break;
      case DIVIDE: op =  SymbType_is_unsigned_word(t_left)
          ? node_word_unsigned_divide : node_word_signed_divide;
        break;
      case MOD: op =  SymbType_is_unsigned_word(t_left)
          ? node_word_unsigned_mod : node_word_signed_mod; break;
      default: ErrorMgr_internal_error(errmgr, "expr2bexpr_recur_binary: illegal expression");
      }

      ErrorMgr_set_the_node(errmgr, expr);
      return op(a, b, env);
    }
  }

  /* ------------------------------------------------------------- */
  /* if both operands are boolean, process them more efficiently */
  if (SymbType_is_boolean(t_left) &&
      SymbType_is_boolean(t_right)) {

    switch (node_get_type(expr)) {
      /* Arithmetic operations which can be booleanized without BDD */
    case LT:
      res =
        ExprMgr_and(exprs,
                    ExprMgr_not(exprs,
                                expr2bexpr_recur(enc, det_layer, left, context,
                                                 in_next, expr2bexpr)),
                    expr2bexpr_recur(enc, det_layer,
                                     right, context, in_next, expr2bexpr));
      break;

    case GT:
      res = ExprMgr_and(exprs, expr2bexpr_recur(enc, det_layer,
                                                left, context, in_next, expr2bexpr),
                        ExprMgr_not(exprs, expr2bexpr_recur(enc, det_layer,
                                                            right, context, in_next,
                                                            expr2bexpr)));
      break;

    case LE:
      res = ExprMgr_or(exprs,
                       ExprMgr_not(exprs,
                                   expr2bexpr_recur(enc, det_layer,
                                                    left, context, in_next,
                                                    expr2bexpr)),
                       expr2bexpr_recur(enc, det_layer,
                                        right, context, in_next, expr2bexpr));
      break;

    case GE:
      res = ExprMgr_or(exprs, expr2bexpr_recur(enc, det_layer,
                                               left, context, in_next, expr2bexpr),
                       ExprMgr_not(exprs, expr2bexpr_recur(enc, det_layer,
                                                           right, context, in_next,
                                                           expr2bexpr)));
      break;

      /* Equality/disequality */
    case EQDEF:
    case SETIN:
    case EQUAL:
      res = ExprMgr_iff(exprs, expr2bexpr_recur(enc, det_layer,
                                                left, context, in_next, expr2bexpr),
                        expr2bexpr_recur(enc, det_layer,
                                         right, context, in_next, expr2bexpr));
      break;

    case NOTEQUAL: /* !(a=b) is smaller than (a!=b) */
      res = ExprMgr_not(exprs,
                        ExprMgr_iff(exprs,
                                    expr2bexpr_recur(enc, det_layer,
                                                     left, context, in_next,
                                                     expr2bexpr),
                                    expr2bexpr_recur(enc, det_layer,
                                                     right, context, in_next,
                                                     expr2bexpr)));
      break;

      /* pure local operations (impossible for scalars) */
    case AND: {
      node_ptr a = expr2bexpr_recur(enc, det_layer, left, context, in_next,
                                    expr2bexpr);
      if (ExprMgr_is_false(exprs, a)) return a; /* fully avoid computation of the right */
      res = ExprMgr_and(exprs, a, expr2bexpr_recur(enc, det_layer, right,
                                                   context, in_next,
                                                   expr2bexpr));
      break;
    }

    case OR: {
      node_ptr a = expr2bexpr_recur(enc, det_layer, left, context, in_next,
                                    expr2bexpr);
      if (ExprMgr_is_true(exprs, a)) return a;
      res = ExprMgr_or(exprs, a, expr2bexpr_recur(enc, det_layer, right,
                                                  context, in_next,
                                                  expr2bexpr));
      break;
    }

    case XOR:
      res = ExprMgr_xor(exprs, expr2bexpr_recur(enc, det_layer, left, context,
                                                in_next, expr2bexpr),
                        expr2bexpr_recur(enc, det_layer, right, context, in_next,
                                         expr2bexpr));
      break;

    case XNOR:
      res = ExprMgr_xnor(exprs, expr2bexpr_recur(enc, det_layer, left, context,
                                                 in_next, expr2bexpr),
                         expr2bexpr_recur(enc, det_layer, right, context, in_next,
                                          expr2bexpr));
      break;

    case IMPLIES: {
      node_ptr a = expr2bexpr_recur(enc, det_layer, left, context, in_next,
                                    expr2bexpr);
      if (ExprMgr_is_false(exprs, a)) return ExprMgr_true(exprs);
      res = ExprMgr_implies(exprs, a, expr2bexpr_recur(enc, det_layer, right,
                                                       context, in_next,
                                                       expr2bexpr));
      break;
    }

    case IFF:
      res = ExprMgr_iff(exprs, expr2bexpr_recur(enc, det_layer, left, context,
                                                in_next, expr2bexpr),
                        expr2bexpr_recur(enc, det_layer, right, context, in_next,
                                         expr2bexpr));
      break;

      /* word operations applied to booleans */
    case CONCATENATION: {
      /* concatenation of two booleans. Tries to fix this by casting to
         word1 and retries */
      node_ptr retry = find_node(nodemgr, node_get_type(expr),
                                 find_node(nodemgr, CAST_WORD1, left, Nil),
                                 find_node(nodemgr, CAST_WORD1, right, Nil));

      res = expr2bexpr_recur(enc, det_layer, retry, context, in_next,
                             expr2bexpr);
      break;
    }

      /* boolean expressions which do not require special dealing or
         simplification */
    case CONS:
      res = find_node(nodemgr, node_get_type(expr),
                      expr2bexpr_recur(enc, det_layer, left, context, in_next,
                                       expr2bexpr),
                      expr2bexpr_recur(enc, det_layer, right, context, in_next,
                                       expr2bexpr));
      break;

      /* arithmetic operations which cannot be easily booleanized =>
         use BDD then */
    case PLUS:
    case MINUS:
    case DIVIDE:
    case MOD:
    case TIMES:
      res = scalar_atom2bexpr(enc, det_layer, expr, context, in_next);
      break;
    case UNION: {
      node_ptr tres = scalar_atom2bexpr(enc, det_layer, expr, context, in_next);
      if ((CONS == node_get_type(tres)) && (SYMB_LAYER(NULL) != det_layer)) {
        /* makes the leaf deterministic. Calculates the
           boolean encoding of the non-deterministic set */
        Set_t det_vars = Set_MakeEmpty();
        Set_Iterator_t det_vars_iter;
        node_ptr det_enc;

        nusmv_assert(llength(tres) > 1); /* singleton are not allowed here */

        det_enc = BoolEnc_get_values_bool_encoding(
                          BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(enc)),
                          tres, /* this is the non-deterministic range */
                          &det_vars /* the set of determinization bits */);

        /* now declares the determinization symbols into the
           determinization layer */
        SET_FOREACH(det_vars, det_vars_iter) {
          node_ptr bit = (node_ptr) Set_GetMember(det_vars, det_vars_iter);
          SymbLayer_declare_state_var(det_layer, bit,
                                      SymbType_create(env, SYMB_TYPE_BOOLEAN, Nil));
        }

        Set_ReleaseSet(det_vars);

        res = det_enc;
      }
      else {
        res = tres;
      }
      break;
    }
    default:
      /* all other expressions are impossible. they cannot have boolean
         operands or it has to be processed as part of the outer
         (parent) expression */
      ErrorMgr_internal_error(errmgr, "Unexpected operator while booleanizing an expression");
    }
    return res;
  }

  /* not words, not pure boolean => they are scalar, i.e.
     pass through BDD evaluation is required. */

  switch (node_get_type(expr)) {

  case NOTEQUAL: /* !(a=b) is smaller than (a!=b) */
    res = expr2bexpr_recur(enc, det_layer,
                           find_node(nodemgr, EQUAL, left, right),
                           context, in_next, expr2bexpr);
    res = find_node(nodemgr, NOT, res, Nil);
    break;

  case EQDEF:
  case EQUAL:
  case LT:
  case GT:
  case LE:
  case GE:
  case PLUS:
  case MINUS:
  case TIMES:
  case DIVIDE:
  case MOD:
  case UNION:
  case SETIN:
    res = scalar_atom2bexpr(enc, det_layer, expr, context, in_next);
    break;

  case AND:
  case OR:
  case XOR:
  case XNOR:
  case IMPLIES:
  case IFF:
  case CONCATENATION:
    ErrorMgr_internal_error(errmgr,
                 "Only booleans can have above operators, but here are scalars");

  default:
    ErrorMgr_internal_error(errmgr, "Unexpected operators");

  }

  return res;
}


/*!
  \brief Creates an encoding for CASE/IFTHENELSE node.

  This is a private sesrvice called by expr2bexpr_recur. If
  CASE/IFTHENELSE evaluates to a word, a WORD encoding is created.

  \se None

  \sa expr2bexpr_word_ite_aux
*/
static node_ptr expr2bexpr_ite(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                               node_ptr expr, node_ptr context,
                               boolean in_next, hash_ptr expr2bexpr)
{
  node_ptr res;
  node_ptr c;
  SymbTable_ptr symb_table = BaseEnc_get_symb_table(BASE_ENC(enc));
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  nusmv_assert(node_get_type(expr) == CASE ||
               node_get_type(expr) == IFTHENELSE);

  if (SymbType_is_word(TypeChecker_get_expression_type(
                  BaseEnc_get_type_checker(BASE_ENC(enc)), expr, context))) {
    res = expr2bexpr_word_ite_aux(enc, det_layer, expr, context, in_next,
                                  expr2bexpr);
  }
  else {
    c = expr2bexpr_recur(enc, det_layer, car(car(expr)),
                         context, in_next, expr2bexpr);
    if (ExprMgr_is_true(exprs, c)) {
      res = expr2bexpr_recur(enc, det_layer, cdr(car(expr)), context, in_next,
                             expr2bexpr);
    }
    else if (ExprMgr_is_false(exprs, c)) {
      res = expr2bexpr_recur(enc, det_layer, cdr(expr), context, in_next,
                             expr2bexpr);
    }
    /* TODO[MD] The comment below is wrong, may hide a bug */
    /* Indeed the if that checks for a word is closed some lines above, not
       here. Probably, the comment is just misplaced. */
    else { /* not a word, fallback case */
      res = ExprMgr_ite(exprs, c, expr2bexpr_recur(enc, det_layer,
                                                   cdr(car(expr)),
                                         context, in_next, expr2bexpr),
                        expr2bexpr_recur(enc, det_layer, cdr(expr),
                                         context, in_next, expr2bexpr),
                        symb_table);
    }
  }
  return res;
}

/*!
  \brief Service of expr2bexpr_word_ite, that creates the word encoding

  Creates the resulting WORD encoding as:
 <textarea>
                     WORD
                  /        \
                 /          NUMBER(size)
                /
             _____ CONS ________________
            /                           \
       _   ITE                         CONS
      |  /  |  \                      /    \
      |c1 t1.0  ITE                 ITE     ...
  bit0|        /  |  \            _ ...
  |          c2  t2.0 ITE       |
  .           ...      |        |
  .                    .    bit1|
  |_                   .
  |_
</textarea>
Encoding complexity is N*C (N=word width, C=num of cases)
*/
static node_ptr
expr2bexpr_word_ite_aux(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                        node_ptr expr, node_ptr context, boolean in_next,
                        hash_ptr expr2bexpr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  SymbTable_ptr symb_table = BaseEnc_get_symb_table(BASE_ENC(enc));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  node_ptr res;
  node_ptr bcase, iter, failure;
  int size = -1;

  /* extracts and booleanizes the case */
  bcase = Nil; /* a list of pairs <bcond, bthen as array> */
  failure = Nil; /* to hold possible FAILURE node */
  iter = expr;
  while (true) {
    node_ptr bcond = expr2bexpr_recur(enc, det_layer, car(car(iter)),
                                      context, in_next, expr2bexpr);

    /* optimization: if the condition is FALSE then skip the current expression */
    if (!ExprMgr_is_false(exprs, bcond)) {
      node_ptr bthen = expr2bexpr_recur(enc, det_layer, cdr(car(iter)),
                                        context, in_next, expr2bexpr);
      _CHECK_WORD(bthen);

      if ((node_get_type(bthen) == NUMBER_UNSIGNED_WORD ||
           node_get_type(bthen) == NUMBER_SIGNED_WORD)) {
        /* encodes the constant word as a normal word */
        bthen = node_word_create_from_wordnumber(WORD_NUMBER(car(bthen)), env);
      }

      /* all then nodes have the same width: */
      if (size == -1) size = node_word_get_width(bthen);
      else { nusmv_assert(node_word_get_width(bthen) == size); }

      /* optimization : if condition is TRUE do not process anything else */
      if (ExprMgr_is_true(exprs, bcond)) { failure = bthen; break; }

      /* MR: cons vs find_node(CONS, ) */
      bcase = cons(nodemgr, find_node(nodemgr, COLON, bcond, (node_ptr) node_word_to_array(bthen)),
                   bcase);
    }

    iter = cdr(iter);

    /* terminating conditions */
    if (node_get_type(iter) == FAILURE) { failure = iter; break; }
    else if (node_get_type(iter) != CASE && node_get_type(iter) != IFTHENELSE) {
      /* non-standard last case, i.e. a normal word expression */
      failure = expr2bexpr_recur(enc, det_layer, iter, context, in_next,
                                 expr2bexpr);
      _CHECK_WORD(failure); /* must be a proper word expr */

      if ((node_get_type(failure) == NUMBER_UNSIGNED_WORD ||
           node_get_type(failure) == NUMBER_SIGNED_WORD)) {
        /* encodes the constant word as a normal word */
        failure = node_word_create_from_wordnumber(WORD_NUMBER(car(failure)), env);
      }
      /* all conditions were FALSE => only now the size is known */
      if (-1 == size) size = node_word_get_width(failure);
      nusmv_assert(node_word_get_width(failure) == size); /* of proper size */
      break;
    }
  }

  /* all conditions were simplified to FALSE, there must be default exp */
  if (Nil == bcase && FAILURE == node_get_type(failure)) {
    ErrorMgr_rpterr_node(errmgr, expr, "Error: in conditional expression all conditions "
                "are false and there is no default case :  ");
  }

  /* get a reversed list of word bits for default expression.
     We do not free previous value of failure because it may or may not
     be find_noded and may potentially be already cached somewhere (in type
     checker or evaluator) */
  if (FAILURE != node_get_type(failure)) failure = reverse_ns(nodemgr, car(failure));

  { /* creates the encoding */

    int i;
    node_ptr wbits = Nil;
    node_ptr failure_iter = failure; /* used if failure is a list of bits */

    for (i=0; i < size; ++i) {
      node_ptr cases;
      /* create a failure value:
         'failure' may be a FAILURE node or a list of bits
      */
      if (FAILURE == node_get_type(failure)) cases = failure;
      else {
        nusmv_assert(CONS == node_get_type(failure_iter)); /* a list of bits */
        cases = car(failure_iter);
        failure_iter = cdr(failure_iter);/* a bit for next iteration */
      }

      for (iter=bcase; iter!=Nil; iter=cdr(iter)) {
        nusmv_assert(node_get_type(car(iter)) == COLON);
        node_ptr bcond = car(car(iter));
        node_ptr biti = array_fetch(node_ptr, (array_t*) cdr(car(iter)), i);

        cases = ExprMgr_ite(exprs, bcond, biti, cases, symb_table);
      }
      wbits = cons(nodemgr, cases, wbits);
    }
    /* MR: shall wbits be freed? */
    res = node_word_create_from_list(wbits, size, env);
  }

  /* frees the bcase structure and the failure list of bits */
  for (iter=bcase; iter!=Nil; iter=cdr(iter)) {
    array_free((array_t*) cdr(car(iter)));
  }
  /* these lists were not cached anywhere (for example, during evaluation) */
  free_list(nodemgr, bcase);
  if (CONS == node_get_type(failure)) free_list(nodemgr, failure);

  return res;
}


/*!
  \brief Converts a generic expression into a boolean expression.

 Takes an expression intended to evaluate to boolean, maps through
 booleans down to the atomic scalar propositions, builds the
 corresponding boolean function, and returns the resulting boolean
 expression.

 The conversion of atomic scalar proposition is currently performed by
 generating the corresponding ADD, and then printing it in terms of
 binary variables.

 The introduction of determinization variables is allowed only if
 parameter "det_layer" is not NULL; Those variables will be declared
 within the given layer

 The input expression may be normal (not flattened), flattened or
 expanded. Parameter 'context' is used if the expression is not flattened.

  \se None

  \sa Compile_expr2bexpr, detexpr2bexpr
*/
static node_ptr expr2bexpr_recur(BddEnc_ptr enc,
                                 SymbLayer_ptr det_layer,
                                 node_ptr expr,
                                 node_ptr context,
                                 boolean in_next,
                                 hash_ptr expr2bexpr)
{
  node_ptr res;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  if (expr == Nil) return Nil;

  res = expr2bexpr_hash_lookup_entry(expr2bexpr, nodemgr, expr, context, in_next,
                                     det_layer != SYMB_LAYER(NULL));
  if (res != (node_ptr) NULL) return res;

  {
    const StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
    const ErrorMgr_ptr errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
    const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

    switch (node_get_type(expr)) {

    case FAILURE:
    case TRUEEXP:
    case FALSEEXP:
      res = expr;
      break;

    case NUMBER_FRAC:
    case NUMBER_REAL:
    case NUMBER_EXP:
      ErrorMgr_rpterr(errmgr, "Real constant cannot be casted to boolean");
      break;

    case NUMBER:
      /* This is needed to ensure uniqueness of the numbers, to allow
         for proper checking of eauality within the _apply function */
      res = find_atom(nodemgr, expr);
      break;
    case NUMBER_UNSIGNED_WORD:
    case NUMBER_SIGNED_WORD:
      res = expr;
      break;

    case TWODOTS: {
      SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(enc));
      int dim1, dim2, i;
      node_ptr t = Nil;
      node_ptr ndim1 = CompileFlatten_resolve_number(st, car(expr), context);
      node_ptr ndim2 = CompileFlatten_resolve_number(st, cdr(expr), context);
      nusmv_assert(NUMBER == node_get_type(ndim1));
      nusmv_assert(NUMBER == node_get_type(ndim2));
      dim1 = NODE_TO_INT(car(ndim1));
      dim2 = NODE_TO_INT(car(ndim2));
      for (i = dim2; i >= dim1; --i) {
        t = find_node(nodemgr, CONS, find_node(nodemgr, NUMBER, NODE_FROM_INT(i), Nil), t);
      }

      if (t == Nil) { ErrorMgr_rpterr(errmgr, "empty range: %d..%d", dim1, dim2); }
      res = t; // res = expr;
      break;
    }
    case UWCONST:
    case SWCONST:
      /* here we rely on the flattener, which is in charge of getting
         rid of SWCONST and UWCONST */
      res = Compile_FlattenSexp(BaseEnc_get_symb_table(BASE_ENC(enc)), expr, context);
      break;

    case BIT:
    case ARRAY:
    case DOT:
    case ATOM:
      res = expr2bexpr_leaf(enc, det_layer, expr, context, in_next, expr2bexpr);
      break;

    case WSIZEOF:
    case CAST_TOINT:
    case CAST_BOOL: {
      /* Here we rely on the flattener, which is in charge of getting
         rid of CAST_BOOL, toint, sizeof */
      node_ptr tmp = Compile_FlattenSexp(BaseEnc_get_symb_table(BASE_ENC(enc)),
                                         expr, context);
      res = expr2bexpr_recur(enc, det_layer, tmp, Nil, in_next, expr2bexpr);
      break;
    }

    case CAST_WORD1:
      res = expr2bexpr_cast_word1(enc, det_layer, expr, context, in_next, expr2bexpr);
      break;

    case CAST_SIGNED:
      res = expr2bexpr_recur(enc, det_layer, car(expr), context, false,
                             expr2bexpr);
      _CHECK_WORD(res);
      if (node_get_type(res) == NUMBER_UNSIGNED_WORD) {
        res = find_node(nodemgr, NUMBER_SIGNED_WORD, car(res), cdr(res));
      }
      break;

    case CAST_UNSIGNED:
      res = expr2bexpr_recur(enc, det_layer, car(expr), context, false,
                             expr2bexpr);
      _CHECK_WORD(res);
      if (node_get_type(res) == NUMBER_SIGNED_WORD) {
        res = find_node(nodemgr, NUMBER_UNSIGNED_WORD, car(res), cdr(res));
      }
      break;

    case WRESIZE:
      res = expr2bexpr_resize(enc, det_layer, expr, context, in_next, expr2bexpr);
      break;

    case EXTEND:
      res = expr2bexpr_extend(enc, det_layer, expr, context, in_next, expr2bexpr);
      break;

    case COUNT:
      res = Compile_FlattenSexp(BaseEnc_get_symb_table(BASE_ENC(enc)),
                                expr, context);
      res = expr2bexpr_recur(enc, det_layer, res, context, in_next, expr2bexpr);
      break;

    case NEXT:
      /* Operator NEXT is not applied to the obtained exp because
         NEXT is passed as "true" in the last parameter in the fun below).*/
      nusmv_assert(!in_next);
      res = expr2bexpr_recur(enc, det_layer, car(expr), context, true,
                             expr2bexpr);
      break;

    case LSHIFT:
    case RSHIFT:
      res = expr2bexpr_shift(enc, det_layer, expr, context, in_next, expr2bexpr);
      break;

    case LROTATE:
    case RROTATE:
      res = expr2bexpr_rotate(enc, det_layer, expr, context, in_next, expr2bexpr);
      break;

    case BIT_SELECTION:
      res = expr2bexpr_bitselect(enc, det_layer, expr, context, in_next, expr2bexpr);
      break;

    case EQDEF:
      res = expr2bexpr_eqdef(enc, det_layer, expr, context, in_next, expr2bexpr);
      break;

    case CASE:
    case IFTHENELSE:
      res = expr2bexpr_ite(enc, det_layer, expr, context, in_next, expr2bexpr);
      break;

      /* -- unary operations which can be processed the standard way -- */
    case NOT:
    case UMINUS:
      res = expr2bexpr_recur_unary(enc, det_layer, expr, context, in_next,
                                   expr2bexpr);
      break;

      /* -- binary operations which can be processed the standard way -- */
    case UNION:
    case SETIN:
    case CONCATENATION:
    case AND:
    case OR:
    case XOR:
    case XNOR:
    case IMPLIES:
    case IFF:
    case EQUAL:
    case NOTEQUAL:
    case PLUS:
    case MINUS:
    case LT:
    case LE:
    case GT:
    case GE:
    case TIMES:
    case DIVIDE:
    case MOD:
      res = expr2bexpr_recur_binary(enc, det_layer, expr, context, in_next,
                                    expr2bexpr);
      break;

      /* -- end of standard processing -- */

    case EX:  /* UNARY CTL/LTL OPERATORS */
    case AX:
    case EG:
    case AG:
    case EF:
    case AF:
    case OP_NEXT:
    case OP_PREC:
    case OP_NOTPRECNOT:
    case OP_FUTURE:
    case OP_ONCE:
    case OP_GLOBAL:
    case OP_HISTORICAL:

    case EBF: /* BOUNDED TEMPORAL OPERATORS (cdr is range, no recursion needed) */
    case ABF:
    case EBG:
    case ABG:
    case EBU:
    case ABU:
      res = find_node(nodemgr, node_get_type(expr),
                      expr2bexpr_recur(enc, det_layer, car(expr), context,
                                       in_next, expr2bexpr),
                      cdr(expr)); /* second operand (if any) is passed as it is */
      break;

      /* -- binary boolean expressions which do not require any special care */
    case CONS:
    case EU: /* BINARY CTL/LTL OPERATORS */
    case AU:
    case MINU:
    case MAXU:
    case UNTIL:
    case RELEASES:
    case SINCE:
    case TRIGGERED:
      res = find_node(nodemgr, node_get_type(expr),
                      expr2bexpr_recur(enc, det_layer, car(expr), context,
                                       in_next, expr2bexpr),
                      expr2bexpr_recur(enc, det_layer, cdr(expr), context,
                                       in_next, expr2bexpr));
      break;

    case SPEC:
    case LTLSPEC:
    case PSLSPEC:
    case INVARSPEC:
    case COMPUTE:
      res = expr2bexpr_recur(enc, det_layer, cdr(car(expr)), car(car(expr)),
                             in_next, expr2bexpr);
      break;

    case CONTEXT:
      res = expr2bexpr_recur(enc, det_layer, cdr(expr), car(expr), in_next,
                             expr2bexpr);
      break;

    default:
      ErrorMgr_internal_error(errmgr, "expr2bexpr_recur: type = %d\n", node_get_type(expr));
      res = (node_ptr) NULL;
    }

    /* updates the results hash  */
    if (res != (node_ptr) NULL) {
      expr2bexpr_hash_insert_entry(expr2bexpr, nodemgr, expr, context, res, in_next,
                                   det_layer != SYMB_LAYER(NULL));
    }

  }
  return res;

}


/*!
  \brief Converts a scalar expression into the corresponding
  (boolean) expression.

  Takes a scalar expression and converts it into a corresponding
  boolean expression.

  The introduction of determinization variables is allowed only if the
  layer <tt>det_layer</tt> is not NULL

  \se New boolean variables might be introduced.
*/
static node_ptr scalar_atom2bexpr(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                                  node_ptr expr, node_ptr context,
                                  boolean in_next)
#if 0
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  node_ptr res;
  int temp = nusmv_yylineno;
  add_ptr bool_add = BddEnc_expr_to_add(enc, (in_next) ?
                                        find_node(nodemgr, NEXT, expr, Nil) : expr,
                                        context);

  nusmv_yylineno = node_get_lineno(expr);
  res = BddEnc_add_to_expr(enc, bool_add, det_layer);
  add_free(BddEnc_get_dd_manager(enc), bool_add);
  nusmv_yylineno = temp;

#if 0
  printf("======================================================================\n");
  printf("expr = ");
  debug_print_node(EnvObject_get_environment(ENV_OBJECT(enc)), expr);
  printf("\n context = ");
  debug_print_node(EnvObject_get_environment(ENV_OBJECT(enc)), context);
  printf("\n res = ");
  debug_print_node(EnvObject_get_environment(ENV_OBJECT(enc)), res);
  printf("\n");
  printf("======================================================================\n");
#endif

  return res;
}
#else
{
  Expr_ptr res = EXPR(NULL);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  EPFMEE op;
  switch(node_get_type(expr)) {
  case PLUS:
    op = expr_plus_;
    break;
  case TIMES:
    op = expr_times_;
    break;
  case DIVIDE:
    op = expr_divide_;
    break;
  case MINUS:
    op = expr_minus_;
    break;
  case UMINUS:
    op = expr_minus_;
    break;
  case MOD:
    op = expr_mod_;
    break;
  case LT:
    op = expr_lt_;
    break;
  case LE:
    op = expr_le_;
    break;
  case GT:
    op = expr_gt_;
    break;
  case GE:
    op = expr_ge_;
    break;
  case EQUAL:
    op = expr_equal_;
    break;
  case NOTEQUAL:
    op = expr_notequal_;
    break;
  case EQDEF:
    op = expr_eqdef_;
    break;
  case UNION:
    op = expr_union_;
    break;
  case SETIN:
    op = expr_setin_;
    break;
  default:
    {
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
      ErrorMgr_internal_error(errmgr, "scalar_atom2bexpr: type = %d\n", node_get_type(expr));
      res = (node_ptr) NULL;
      break;
    }
  }

  {
    Expr_ptr left, right, r_left, r_right;
    const ExprMgr_ptr exprmgr = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
    const SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(enc));
    hash_ptr expr2bexpr = compile_beval_get_handled_hash(st,
                                                         ST_BEVAL_EXPR2BEXPR_HASH);

    BStructInfo bst;
    bst.expm = exprmgr;
    bst.st = st;
    bst.nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
#warning TODO: Consider to use an ST Handled Hash to perform this memoization    
    bst.apply_hash = new_assoc();

    if (UMINUS != node_get_type(expr)) {
      left = car(expr);
      right = cdr(expr);
    }
    else {
      left = ExprMgr_number(exprmgr, 0);
      right = car(expr);
    }

    r_left = expr2bexpr_recur(enc, det_layer, left, context, in_next, expr2bexpr);
    r_right = expr2bexpr_recur(enc, det_layer, right, context, in_next, expr2bexpr);

    res = _apply(&bst, r_left, r_right, op);

#warning TODO: if handled hash is used, the two lines have to be removed
    clear_assoc(bst.apply_hash);
    free_assoc(bst.apply_hash);
  }

#if 0
  printf("======================================================================\n");
  printf("expr = ");
  debug_print_node(EnvObject_get_environment(ENV_OBJECT(enc)), expr);
  printf("\n context = ");
  debug_print_node(EnvObject_get_environment(ENV_OBJECT(enc)), context);
  printf("\n res = ");
  debug_print_node(EnvObject_get_environment(ENV_OBJECT(enc)), res);
  printf("\n");
  printf("======================================================================\n");
#endif
  return res;
}
#endif


/*!
  \brief true if expression is booleanizable

  Private service of compile_is_booleanizable.
                       To represent 'true' in cache we use the constant 2 for
                       'false' we use 1 to avoid representation problems wrt Nil

  \se cache can be updated
*/
static boolean
compile_is_booleanizable_aux(node_ptr expr,
                             node_ptr context,
                             const CompileIsBooleanizableAuxParams* params)
{
  SymbTable_ptr st = params->st;
  node_ptr key;
  node_ptr tmp;
  boolean res;

  if (Nil == expr) {
    return true;
  }
  key = find_node(params->nodemgr, CONTEXT, context, expr);

  tmp = find_assoc(params->cache, key);
  if (Nil != tmp) {
    /* 2 means true */
    return (NODE_TO_INT(tmp) == 2);
  }

  if (CONTEXT == node_get_type(expr)) {
    return compile_is_booleanizable_aux(car(expr), cdr(expr), params);
  }

  res = true;
  switch (node_get_type(expr)) {
  case NUMBER:
  case NUMBER_SIGNED_WORD:
  case NUMBER_UNSIGNED_WORD:
  case TRUEEXP:
  case FALSEEXP:
  case FAILURE:
    res = true;
    break;

  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
    res = false;
    break;

  case NFUNCTION:
    res = false;
    break;

  case BIT:
    res = true;
    break;

  case ATOM:
  case DOT:
  case ARRAY:
    {
      ResolveSymbol_ptr rs = SymbTable_resolve_symbol(st, expr, context);
      node_ptr resolved = ResolveSymbol_get_resolved_name(rs);

      if (ResolveSymbol_is_var(rs)) {
        SymbType_ptr t = SymbTable_get_var_type(st, resolved);

        /* Real, integers, strings.. */
        res = ! (SymbType_is_infinite_precision(t));

        if (res && SymbType_is_word(t)) {
          res = !params->word_unbooleanizable;
        }
      }
      else if (ResolveSymbol_is_define(rs)) {
        res = compile_is_booleanizable_aux(
                     SymbTable_get_define_body(st, resolved),
                     SymbTable_get_define_context(st, resolved),
                     params);
      }
      else if (ResolveSymbol_is_parameter(rs)) {
        res = compile_is_booleanizable_aux(
                   SymbTable_get_actual_parameter(st, resolved),
                   SymbTable_get_actual_parameter_context(st, resolved),
                   params);
      }
      else if (ResolveSymbol_is_function(rs)) {
        res = false;
      }
      else if (ResolveSymbol_is_constant(rs)) {
        TypeChecker_ptr tc = SymbTable_get_type_checker(st);
        SymbType_ptr ty = TypeChecker_get_expression_type(tc, resolved, Nil);
        /* Only real constants are unbooleanizable */
        nusmv_assert(! SymbType_is_continuous(ty));

        res = !SymbType_is_real(ty);
      }
      else {
        ErrorMgr_rpterr(params->errmgr, "Unexpected symbol in Compile_is_expr_booleanizable.");
        error_unreachable_code();
      }
      break;
    }
  default:
    {
      /* Lazy evaluation */
      res = compile_is_booleanizable_aux(car(expr), context, params);
      if (res) {
        /* Check the cdr iff car is booleanizable */
        res = compile_is_booleanizable_aux(cdr(expr), context, params);
      }
    }
  }

  if (res) {
    /* 2 means true */
    insert_assoc(params->cache, key, NODE_FROM_INT(2));
  }
  else {
    /* 1 means false */
    insert_assoc(params->cache, key, NODE_FROM_INT(1));
  }

  return res;
}


/*!
  \brief Computes the key for expr2bexpr
*/
static node_ptr make_key(NodeMgr_ptr nodemgr, node_ptr expr, boolean a,
                         boolean b)
{
  int j = 0;

  j += a? 1 : 0;
  j += b? 2 : 0;

  return find_node(nodemgr, CONTEXT, expr, NODE_FROM_INT(j));
}


/*!
  \brief Insert an association in expr2bespr
*/
static void
expr2bexpr_hash_insert_entry(hash_ptr expr2bexpr, NodeMgr_ptr nodemgr,
                             node_ptr expr,
                             node_ptr ctx, node_ptr bexpr, boolean a, boolean b)
{
  node_ptr cexp;

  nusmv_assert(expr2bexpr != (hash_ptr)NULL);

  if (ctx == Nil) cexp = expr;
  else cexp = find_node(nodemgr, CONTEXT, ctx, expr);

  insert_assoc(expr2bexpr, make_key(nodemgr, cexp, a, b), bexpr);
}


/*!
  \brief Lookups an association in expr2bexpr
*/
static node_ptr
expr2bexpr_hash_lookup_entry(hash_ptr expr2bexpr, NodeMgr_ptr nodemgr,
                             node_ptr expr,
                             node_ptr ctx, boolean a, boolean b)
{
  node_ptr cexp;

  nusmv_assert(expr2bexpr != (hash_ptr) NULL);

  if (ctx == Nil) cexp = expr;
  else cexp = find_node(nodemgr, CONTEXT, ctx, expr);

  return find_assoc(expr2bexpr, make_key(nodemgr, cexp, a, b));
}

/*!
  \brief Call SymbTable_get_handled_hash_ptr with proper arguments
*/
static hash_ptr compile_beval_get_handled_hash(SymbTable_ptr symb_table,
                                               char* hash_str)
{
  if (! strcmp(ST_BEVAL_EXPR2BEXPR_HASH, hash_str)) {
    return
      SymbTable_get_handled_hash_ptr(
                            symb_table,
                            ST_BEVAL_EXPR2BEXPR_HASH,
                            (ST_PFICPCP)NULL,
                            (ST_PFICPI)NULL,
                            (ST_PFSR)NULL,
                            (SymbTableTriggerFun)NULL,
                            SymbTable_clear_handled_remove_action_hash,
                            (SymbTableTriggerFun)NULL);
  }
  else error_unreachable_code();
}


/*!
  \brief Initializes structure passed to compile_is_booleanizable_aux
*/
static void
init_compile_is_booleanizable_params(CompileIsBooleanizableAuxParams* self,
                                     SymbTable_ptr st,
                                     boolean word_unbooleanizable,
                                     hash_ptr cache)
{
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  self->st = st;
  self->nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  self->errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  self->word_unbooleanizable = word_unbooleanizable;
  self->cache = cache;
}


/*!
  \brief Auxiliary function to handle the cast of a boolean expression to a word of size 1.
*/
static Expr_ptr expr2bexpr_cast_word1(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                                      node_ptr expr, node_ptr context,
                                      boolean in_next, hash_ptr expr2bexpr) {

  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const TypeChecker_ptr tc = BaseEnc_get_type_checker(BASE_ENC(enc));
  node_ptr a = car(expr);
  Expr_ptr res = EXPR(NULL);

  nusmv_assert(SymbType_is_boolean(TypeChecker_get_expression_type(tc, a, context)));

  a = expr2bexpr_recur(enc, det_layer, a, context, in_next, expr2bexpr);

  if (node_get_type(a) == TRUEEXP) {
    res = find_node(nodemgr, NUMBER_UNSIGNED_WORD,
                    (node_ptr) WordNumberMgr_integer_to_word_number(words, 1,1), Nil);
  }
  else if (node_get_type(a) == FALSEEXP) {
    res = find_node(nodemgr, NUMBER_UNSIGNED_WORD,
                    (node_ptr) WordNumberMgr_integer_to_word_number(words, 0,1), Nil);
  }
  else res = node_word_create(a, 1, env);

  return res;
}

/*!
  \brief Auxiliary function to handle the extend operation over words.
*/
static Expr_ptr expr2bexpr_extend(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                                  node_ptr expr, node_ptr context,
                                  boolean in_next, hash_ptr expr2bexpr) {
  Expr_ptr res = EXPR(NULL);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  node_ptr delta = CompileFlatten_resolve_number(BaseEnc_get_symb_table(BASE_ENC(enc)),
                                                 cdr(expr),
                                                 context);
  nusmv_assert(Nil != delta && NUMBER == node_get_type(delta));

  SymbType_ptr ta = \
    TypeChecker_get_expression_type(BaseEnc_get_type_checker(BASE_ENC(enc)),
                                    car(expr), context);
  nusmv_assert(SymbType_is_word(ta));
  node_ptr a = expr2bexpr_recur(enc, det_layer, car(expr), context, in_next,
                       expr2bexpr);
  if (node_get_type(a) == NUMBER_UNSIGNED_WORD) {
    WordNumber_ptr wn = WORD_NUMBER(car(a));
    int times = node_get_int(delta);
    res = find_node(nodemgr, NUMBER_UNSIGNED_WORD,
                    (node_ptr)WordNumberMgr_unsigned_extend(words, wn, times), Nil);
  }
  else if (node_get_type(a) == NUMBER_SIGNED_WORD) {
    WordNumber_ptr wn = WORD_NUMBER(car(a));
    int times = node_get_int(delta);
    res = find_node(nodemgr, NUMBER_SIGNED_WORD,
                    (node_ptr)WordNumberMgr_signed_extend(words, wn, times), Nil);
  }
  else {
    /* not a constant */
    res = node_word_extend(a, delta, SymbType_is_signed_word(ta), env);
  }

  return res;
}

/*!
  \brief Auxiliary function to handle the bitselection operation among two words.
*/
static Expr_ptr expr2bexpr_bitselect(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                                  node_ptr expr, node_ptr context,
                                  boolean in_next, hash_ptr expr2bexpr)
{
  node_ptr a, hbit, lbit;
  Expr_ptr res = EXPR(NULL);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(enc));
  const SymbType_ptr ta = \
    TypeChecker_get_expression_type(BaseEnc_get_type_checker(BASE_ENC(enc)),
                                    car(expr), context);
  nusmv_assert(SymbType_is_word(ta));

  a = expr2bexpr_recur(enc, det_layer, car(expr), context, in_next, expr2bexpr);

  /* Resolve constant expressions. node_word_selection will assert
     that the range is numeric. */
  hbit = CompileFlatten_resolve_number(st, car(cdr(expr)), context);
  lbit = CompileFlatten_resolve_number(st, cdr(cdr(expr)), context);

  nusmv_assert(Nil != hbit && Nil != lbit &&
               NUMBER == node_get_type(hbit) &&
               NUMBER == node_get_type(lbit));

  /* this handles both constant and non-constant words */
  res = node_word_selection(a, find_node(nodemgr, COLON, hbit, lbit), env);

  return res;
}

/*!
  \brief Auxiliary function to handle the resize operation among two words.
*/
static Expr_ptr expr2bexpr_resize(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                                  node_ptr expr, node_ptr context,
                                  boolean in_next, hash_ptr expr2bexpr)
{
  /* see note 0003136, issue #0001787 for full d+efinition of the
     semantics of the word resize operator */
  nusmv_assert(WRESIZE == node_get_type(expr));

  int m, n;
  Expr_ptr res = EXPR(NULL);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  SymbType_ptr orig_type = \
    TypeChecker_get_expression_type(BaseEnc_get_type_checker(BASE_ENC(enc)),
                                    car(expr), context);
  node_ptr _size = \
    CompileFlatten_resolve_number(BaseEnc_get_symb_table(BASE_ENC(enc)),
                                  cdr(expr), context);

  nusmv_assert(NULL != _size  && NUMBER == node_get_type(_size));
  m = SymbType_get_word_width(orig_type);
  n = node_get_int(_size);
  nusmv_assert(0 < n);

  if (n == m) {
    res = expr2bexpr_recur(enc, det_layer, car(expr), context, in_next,
                           expr2bexpr);
  }
  else if (n < m) {
    if (SymbType_is_signed_word(orig_type)) {
      node_ptr msb_sel = \
        find_node(nodemgr, COLON,
                  find_node(nodemgr, NUMBER, NODE_FROM_INT(m-1), Nil),
                  find_node(nodemgr, NUMBER, NODE_FROM_INT(m-1), Nil));

      node_ptr rightmost_sel = \
        find_node(nodemgr, COLON,
                  find_node(nodemgr, NUMBER, NODE_FROM_INT(n-2), Nil),
                  find_node(nodemgr, NUMBER, NODE_FROM_INT(0), Nil));

      node_ptr nexpr =  \
        find_node(nodemgr, CONCATENATION,
                  find_node(nodemgr, BIT_SELECTION, car(expr), msb_sel),
                  find_node(nodemgr, BIT_SELECTION, car(expr), rightmost_sel));

      node_ptr cnexpr = \
        find_node(nodemgr, CAST_SIGNED, nexpr, Nil);

      res = expr2bexpr_recur(enc, det_layer, cnexpr, context, in_next
                             , expr2bexpr);
    }/* signed words */
    else {
      node_ptr rightmost_sel = \
        find_node(nodemgr, COLON,
                  find_node(nodemgr, NUMBER, NODE_FROM_INT(n-1), Nil),
                  find_node(nodemgr, NUMBER, NODE_FROM_INT(0), Nil));

      node_ptr nexpr = find_node(nodemgr, BIT_SELECTION, car(expr), rightmost_sel);

      res = expr2bexpr_recur(enc, det_layer, nexpr, context, in_next,
                             expr2bexpr);
    }/* unsigned */
  }
  else {
    /* word extension (handles both signed and unsigned) */
    node_ptr nexpr = find_node(nodemgr, EXTEND, car(expr),
                               find_node(nodemgr, NUMBER, NODE_FROM_INT(n-m), Nil));

    res = expr2bexpr_recur(enc, det_layer, nexpr, context, in_next,
                           expr2bexpr);
  }

  return res;
}


/*!
  \brief Auxiliary function to handle the eqdef operation.
*/
static Expr_ptr expr2bexpr_eqdef(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                                 node_ptr expr, node_ptr context,
                                 boolean in_next, hash_ptr expr2bexpr)
{
  Expr_ptr res = EXPR(NULL);
  Expr_ptr var_name;
  node_ptr lhs = car(expr);
  node_ptr rhs = cdr(expr);

  nusmv_assert(!in_next);

  switch (node_get_type(lhs)) {
  case SMALLINIT:
  case NEXT:
    var_name = car(lhs);
    break;
  default:
    var_name = lhs;
  }

  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const TypeChecker_ptr tc = BaseEnc_get_type_checker(BASE_ENC(enc));
  const SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(enc));
  const SymbType_ptr lhs_type = TypeChecker_get_expression_type(tc, lhs, context);
  const SymbType_ptr rhs_type = TypeChecker_get_expression_type(tc, rhs, context);

  { /* Resolve the symbol */
    ResolveSymbol_ptr rs;
    rs = SymbTable_resolve_symbol(st, var_name, context);
    var_name = ResolveSymbol_get_resolved_name(rs);

    nusmv_assert(!ResolveSymbol_is_ambiguous(rs));
  }

  nusmv_assert(SymbTable_is_symbol_declared(st, var_name));

  if (!SymbTable_is_symbol_var(st, var_name)) {
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
    ErrorMgr_rpterr(errmgr, "Unexpected data structure, variable was expected");
  }

  if (SymbType_is_boolean(lhs_type) &&
      (SymbType_is_boolean(rhs_type) ||
       SYMB_TYPE_SET_BOOL == SymbType_get_tag(rhs_type))) {
    /* assignment is fully boolean (it is not bool := int) */

    if (SYMB_LAYER(NULL) == det_layer) {
      if (!SymbType_is_boolean(rhs_type)) {
        const StreamMgr_ptr streams =
          STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
        const ErrorMgr_ptr errmgr =
          ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
        const MasterPrinter_ptr wffprint =
          MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

        StreamMgr_print_error(streams,  "Attempting to booleanize the formula: '");
        StreamMgr_nprint_error(streams, wffprint, "%N", expr);
        StreamMgr_print_error(streams,  "'");
        ErrorMgr_internal_error(errmgr, "Unexpected non-deterministic formula:" \
                                " determinization layer not specified");
      }
    }

    /* boolean variable, rhs will be determinized */
    if (NEXT == node_get_type(lhs)) {
      var_name = find_node(NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR)),
                           NEXT, var_name, Nil);
    }

    res = expr2bexpr_recur(enc, det_layer, rhs, context, in_next, expr2bexpr);
    res = ExprMgr_iff(EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER)),
                      var_name, res);
  }
  else { /* scalar variable */
    node_ptr fixed_expr;

    /* take the original unflatten LHS and get rid of SMALLINIT */
    if (SMALLINIT == node_get_type(lhs)) lhs = car(lhs);
    fixed_expr = find_node(NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR)),
                           EQDEF, lhs, rhs);

    res = expr2bexpr_recur_binary(enc, det_layer, fixed_expr, context,
                                  in_next, expr2bexpr);
  }

  return res;
}


/*!
  \brief Auxiliary function to handle the leaf expressions, namely
  constants, variables, defines, parameters.
*/
static Expr_ptr expr2bexpr_leaf(BddEnc_ptr enc, SymbLayer_ptr det_layer,
                                node_ptr expr, node_ptr context,
                                boolean in_next, hash_ptr expr2bexpr)
{
  Expr_ptr name, res;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(enc));
  const NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  ResolveSymbol_ptr rs = SymbTable_resolve_symbol(st, expr, context);

  switch (node_get_type(expr)) {
  case BIT: {
    name = ResolveSymbol_get_resolved_name(rs);

    nusmv_assert(ResolveSymbol_is_var(rs));

    if (in_next) res = find_node(nodemgr, NEXT, name, Nil);
    else res = name;
    break;
  }

  case ARRAY: {
    if (ResolveSymbol_is_undefined(rs)) {
      /* ARRAY may be identifier-with-brackets and may be an
         expression.  Here array expression is detected => flatten
         it and then process.  See description of
         flattener_core_flatten for details */
      node_ptr tmp = Compile_FlattenSexp(st, expr, context);
      nusmv_assert(tmp != expr); /* loop in recursion is impossible */
      res = expr2bexpr_recur(enc, det_layer, tmp, Nil, in_next, expr2bexpr);
      break;
    }
    else {
      /* array is actually identifier => process it with ATOM and DOT */
    }
    /* NO BREAK HERE:
       fall trhough to handle it as with the body of DOT and ATOM */
  }
  case DOT:
  case ATOM: {
    name = ResolveSymbol_get_resolved_name(rs);

    /* We assume expression is well typed and there are no ambiguous terms. */
    nusmv_assert(!ResolveSymbol_is_ambiguous(rs));

    /* parameter */
    if (ResolveSymbol_is_parameter(rs)) {
      /* process the value of the parameter */
      node_ptr param = SymbTable_get_flatten_actual_parameter(st, name);
      res = expr2bexpr_recur(enc, det_layer, param, Nil, in_next, expr2bexpr);
      break;
    }

    /* define */
    if (ResolveSymbol_is_define(rs)) {
      /* define (rhs must be boolean, recur to check) */
      node_ptr body = SymbTable_get_define_flatten_body(st, name);
      if (body == (node_ptr) NULL) {
        const ErrorMgr_ptr errmgr =
          ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
        ErrorMgr_error_undefined(errmgr, name);
      }

      res = expr2bexpr_recur(enc, det_layer, body, Nil, in_next,
                             expr2bexpr);
    }
    /* constant */
    else if (ResolveSymbol_is_constant(rs)) {
      res = name;
    }
    /* variable */
    else if (ResolveSymbol_is_var(rs)) {
      SymbType_ptr vtype = SymbTable_get_var_type(st, name);
      /* variable, must be boolean or word */
      if (SymbType_is_boolean(vtype)) {
        const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
        if (in_next) res = find_node(nodemgr, NEXT, name, Nil);
        else res = name;
        res = find_node(nodemgr, IFTHENELSE,
                        find_node(nodemgr, COLON, res, ExprMgr_true(exprs)),
                        ExprMgr_false(exprs));
      }
      else if (SymbType_is_word(vtype)) {
        BoolEnc_ptr benc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(enc));

        res = BoolEnc_get_var_encoding(benc, name);
        _CHECK_WORD(res);
        if (in_next) res = node_word_apply_unary(res, NEXT, env);
      }
      else {
        BoolEnc_ptr benc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(enc));

        res = BoolEnc_get_var_encoding(benc, name);
        if (in_next) {
          /* We move next on the leaves */
          res = expr2bexpr_recur(enc, det_layer, res, context, in_next,
                                 expr2bexpr);
        }
#warning TODO: here the case for real/int/function are missing
      }
    }
    else {
      const StreamMgr_ptr streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
      const MasterPrinter_ptr sexpprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_SEXP_PRINTER));
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
      /* unknow identifier. This code should be impossible since the
         type checker already checked the expr for being correct
      */
      StreamMgr_nprint_error(streams, sexpprint, "%N", expr);
      ErrorMgr_rpterr(errmgr, "Unexpected data structure");
    }

    break;
  }
  default:
    {
      const StreamMgr_ptr streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
      const MasterPrinter_ptr sexpprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_SEXP_PRINTER));
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
      /* unknow identifier. This code should be impossible since the
         type checker already checked the expr for being correct
      */
      StreamMgr_nprint_error(streams, sexpprint, "%N", expr);
      ErrorMgr_rpterr(errmgr, "Unexpected data structure");
      break;
    }
  }
  return res;
}

static Expr_ptr expr_plus_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b) {
  return node_plus(a, b, EnvObject_get_environment(ENV_OBJECT(self)));
}

static Expr_ptr expr_times_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b) {
  return node_times(a, b, EnvObject_get_environment(ENV_OBJECT(self)));
}

static Expr_ptr expr_divide_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b) {
  return node_divide(a, b, EnvObject_get_environment(ENV_OBJECT(self)));
}

static Expr_ptr expr_minus_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b) {
  return node_minus(a, b, EnvObject_get_environment(ENV_OBJECT(self)));
}

static Expr_ptr expr_mod_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b) {
  return node_mod(a, b, EnvObject_get_environment(ENV_OBJECT(self)));
}

static Expr_ptr expr_equal_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b) {
  return node_equal(a, b, EnvObject_get_environment(ENV_OBJECT(self)));
}

static Expr_ptr expr_notequal_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b) {
  return node_not_equal(a, b, EnvObject_get_environment(ENV_OBJECT(self)));
}

static Expr_ptr expr_le_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b) {
  return node_le(a, b, EnvObject_get_environment(ENV_OBJECT(self)));
}

static Expr_ptr expr_ge_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b) {
  return node_ge(a, b, EnvObject_get_environment(ENV_OBJECT(self)));
}

static Expr_ptr expr_lt_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b) {
  return node_lt(a, b, EnvObject_get_environment(ENV_OBJECT(self)));
}

static Expr_ptr expr_gt_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b) {
  return node_gt(a, b, EnvObject_get_environment(ENV_OBJECT(self)));
}

static Expr_ptr expr_eqdef_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b) {
  return node_setin(a, b, EnvObject_get_environment(ENV_OBJECT(self)));
}

static Expr_ptr expr_union_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b) {
  return node_union(a, b, EnvObject_get_environment(ENV_OBJECT(self)));
}

static Expr_ptr expr_setin_(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b) {
  return node_setin(a, b, EnvObject_get_environment(ENV_OBJECT(self)));
}

static node_ptr _mk_key(BStructInfo_ptr aux, EPFMEE fn, Expr_ptr a, Expr_ptr b) {
  return find_node(aux->nodemgr, CONTEXT, (node_ptr)fn,
                   find_node(aux->nodemgr, CONTEXT, a, b));
}

static Expr_ptr _cache_lookup(BStructInfo_ptr aux, node_ptr key) {
  return find_assoc(aux->apply_hash, key);
}

static void _cache_insert(BStructInfo_ptr aux, node_ptr key, Expr_ptr res) {
  insert_assoc(aux->apply_hash, key, res);
}

/*!
  \brief Auxiliary function to apply an operation among two expression.

  This function applies the given function to the two argument
  expressions. The arguments are assumed to be ITE structures.

*/
static Expr_ptr _apply(BStructInfo_ptr aux, Expr_ptr a, Expr_ptr b, EPFMEE fn) {
  Expr_ptr res;
  if (_IS_LEAF(a) && _IS_LEAF(b)) {
    res = (*fn)(aux->expm, a, b);
    return res;
  }

  node_ptr key = _mk_key(aux, fn, a, b);
  res = _cache_lookup(aux, key);
  if (EXPR(NULL) != res) return res;

  if (_IS_LEAF(a)) {
    nusmv_assert(_IS_ITE(b));
    Expr_ptr cond = _ITE_GET_COND(b);
    Expr_ptr res_t = _apply(aux, a, _ITE_GET_THEN(b), fn);
    Expr_ptr res_e = _apply(aux, a, _ITE_GET_ELSE(b), fn);
    res = ExprMgr_ite(aux->expm, cond, res_t, res_e, aux->st);
  }
  else if (_IS_LEAF(b)) {
    nusmv_assert(_IS_ITE(a));
    Expr_ptr cond = _ITE_GET_COND(a);
    Expr_ptr res_t = _apply(aux, _ITE_GET_THEN(a), b, fn);
    Expr_ptr res_e = _apply(aux, _ITE_GET_ELSE(a), b, fn);
    res = ExprMgr_ite(aux->expm, cond, res_t, res_e, aux->st);
  }
  else if (!_IS_LEAF(a) && !_IS_LEAF(b)) {
    nusmv_assert(_IS_ITE(a));
    nusmv_assert(_IS_ITE(b));
    Expr_ptr cond = _ITE_GET_COND(a);
    Expr_ptr res_t = _apply(aux, _ITE_GET_THEN(a), b, fn);
    Expr_ptr res_e = _apply(aux, _ITE_GET_ELSE(a), b, fn);
    res = ExprMgr_ite(aux->expm, cond, res_t, res_e, aux->st);
  }
  else {
    nusmv_assert(false);
  }

  _cache_insert(aux, key, res);

  return res;
}

