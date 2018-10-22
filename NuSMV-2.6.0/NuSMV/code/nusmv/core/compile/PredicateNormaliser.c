/* ---------------------------------------------------------------------------


   This file is part of the ``compile'' package of NuSMV version 2.
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
  \author Andrei Tchaltsev
  \brief A Predicate-Normaliser class

  See PredicateNormaliser.h for more info

*/



#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/compile/PredicateNormaliser.h"
#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/WordNumberMgr.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/EnvObject_private.h"

/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct PredicateNormaliser_TAG
{
  INHERITS_FROM(EnvObject);

  hash_ptr expr2normalisedPredicate; /* hash of
                                        expr -> predicate-normalised expr */
  TypeChecker_ptr checker; /* type-checker is used to get type info
                              of processed expressions and type check
                              generated expressions */
  SymbTable_ptr st;  /* the symbol table */
} PredicateNormaliser;


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief A switcher is used force the generated
   expressions to have linearised ITE only , or allow to have nested ITE.

  If this macro can be :
   0 -- indicates that all created ITE should be linear, i.e. there
   should not be nested (not-boolean) ITEs.
   1 -- indicates that (not-boolean) ITE can be nested.

   See pred_norm_push_ite_up for more examples.

  \sa pred_norm_push_ite_up
*/
#define ALLOW_NESTED_CASES 1

/*
  \brief The name of the hash used for the predicate normalizer within
  the Symbol Table
 */
#define PREDICATE_NORMALIZER_HASH "___predicate_normalizer_hash___"

/*
  \brief Forces the predicate normalizer to use an hash handled by the
  symbol table instead of creating a new hash each time the predicate
  normalizer is isntantiated.
  */
#define PN_USES_ST_HANDLED_HASH 1

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void pred_norm_init(PredicateNormaliser_ptr self,
                           SymbTable_ptr st);
static void pred_norm_deinit(PredicateNormaliser_ptr self);

static void pred_norm_finalize(Object_ptr object, void* dummy);

static node_ptr pred_norm_normalise(PredicateNormaliser_ptr self,
                                    node_ptr expr,
                                    node_ptr context,
                                    boolean expand_defines);
static node_ptr pred_norm_find_node(PredicateNormaliser_ptr self,
                                    int kind, node_ptr op1, node_ptr op2);
static boolean pred_norm_is_true_bool_exp(PredicateNormaliser_ptr self,
                                          node_ptr expr, node_ptr context);

static node_ptr pred_norm_bool2int(PredicateNormaliser_ptr self, node_ptr expr);
static node_ptr pred_norm_bool2word1(PredicateNormaliser_ptr self, node_ptr expr);

static node_ptr pred_norm_push_ite_up(PredicateNormaliser_ptr self,
                                      int op, node_ptr op1, node_ptr op2);
static node_ptr pred_norm_push_ite_up_conditioned(PredicateNormaliser_ptr self,
                                                  node_ptr condition, int op,
                                                  node_ptr exp1, node_ptr exp2,
                                                  node_ptr tail);
static node_ptr pred_norm_normalise_ite(PredicateNormaliser_ptr self,
                                        node_ptr cond,
                                        node_ptr then,
                                        node_ptr tail);
static void
pred_norm_get_predicates_only(const PredicateNormaliser_ptr self,
                              Set_t* preds, node_ptr expr,
                              hash_ptr already_processed);

static hash_ptr
predicate_normalizer_get_handled_hash(PredicateNormaliser_ptr self);

static assoc_retval
predicate_normalizer_hash_free(char *key, char *data, char * arg);

static node_ptr
pred_normalizer_make_key(const NodeMgr_ptr nodemgr, boolean expand_define,
                         node_ptr context, node_ptr expr);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

PredicateNormaliser_ptr PredicateNormaliser_create(SymbTable_ptr st)
{
  PredicateNormaliser_ptr self = ALLOC(PredicateNormaliser, 1);

  PREDICATE_NORMALISER_CHECK_INSTANCE(self);

  pred_norm_init(self, st);
  return self;
}

void PredicateNormaliser_destroy(PredicateNormaliser_ptr self)
{
  PREDICATE_NORMALISER_CHECK_INSTANCE(self);
  Object_destroy(OBJECT(self), NULL);
}

node_ptr
PredicateNormaliser_normalise_expr(PredicateNormaliser_ptr self, node_ptr expr)
{
  int lineno_tmp;
  node_ptr result;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  PREDICATE_NORMALISER_CHECK_INSTANCE(self);

  /* Nil returned as it is. Probably, only CONS-exprs may have Nil inside */
  if (Nil == expr) return Nil;

  /* new node will be created with for sure error line number */
  lineno_tmp = nusmv_yylineno;
  nusmv_yylineno = -1;

  /* sometimes top-level expressions are connected together with CONS
     or AND node. Such pseudo-expressions do not respect general
     rules, e.g.  they may have Nil as left or right child, which is
     incorrect wrt type checking.

     Here such expressions are processed separately. They are not type
     checked or memoized.

     Note: that normal expressions with AND or CONS at the top will be
     processed correctly, but just not memoized (some efficiency may
     be lost).

     NOTE: the right solution would be to use a special connector
     for high level expressions, not generic AND or CONS.
  */
  if (AND == node_get_type(expr) || CONS == node_get_type(expr)) {
    node_ptr left = PredicateNormaliser_normalise_expr(self, car(expr));
    node_ptr right = PredicateNormaliser_normalise_expr(self, cdr(expr));
    result = ExprMgr_resolve(exprs, self->st, node_get_type(expr), left, right);
  }
  else {
    /* this is a usual expression */
    result = pred_norm_normalise(self, expr, Nil, true);
  }

  nusmv_yylineno = lineno_tmp; /* restore line number */
  return result;
}

node_ptr
PredicateNormaliser_normalise_expr_no_expand(PredicateNormaliser_ptr self, node_ptr expr)
{
  int lineno_tmp;
  node_ptr result;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  PREDICATE_NORMALISER_CHECK_INSTANCE(self);

  /* Nil returned as it is. Probably, only CONS-exprs may have Nil inside */
  if (Nil == expr) return Nil;

  /* new node will be created with for sure error line number */
  lineno_tmp = nusmv_yylineno;
  nusmv_yylineno = -1;

  /* sometimes top-level expressions are connected together with CONS
     or AND node. Such pseudo-expressions do not respect general
     rules, e.g.  they may have Nil as left or right child, which is
     incorrect wrt type checking.

     Here such expressions are processed separately. They are not type
     checked or memoized.

     Note: that normal expressions with AND or CONS at the top will be
     processed correctly, but just not memoized (some efficiency may
     be lost).

     NOTE: the right solution would be to use a special connector
     for high level expressions, not generic AND or CONS.
  */
  if (AND == node_get_type(expr) || CONS == node_get_type(expr)) {
    node_ptr left = PredicateNormaliser_normalise_expr_no_expand(self, car(expr));
    node_ptr right = PredicateNormaliser_normalise_expr_no_expand(self, cdr(expr));
    result = ExprMgr_resolve(exprs, self->st, node_get_type(expr), left, right);
  }
  else {
    /* this is a usual expression */
    result = pred_norm_normalise(self, expr, Nil, false);
  }

  nusmv_yylineno = lineno_tmp; /* restore line number */
  return result;
}

node_ptr
PredicateNormaliser_normalise_specification(PredicateNormaliser_ptr self,
                                            node_ptr expr)
{
  node_ptr result;
  int lineno_tmp;
  boolean isTypeOK;

  PREDICATE_NORMALISER_CHECK_INSTANCE(self);

  /* new node will be created with for sure error line number */
  lineno_tmp = nusmv_yylineno;
  nusmv_yylineno = -1;

  result = pred_norm_normalise(self, expr, Nil, true);

  /* perform type checking .
     The type of the property is of no importance since
     original expressions is already type checked and the normalisation
     keeps this correctness (therefore, no error messages can be).
     Actually, the type checking is performed just to remember
     the type of generated expressions.
  */
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    isTypeOK = TypeChecker_is_specification_wellformed(self->checker,
                                                       find_node(nodemgr, SPEC, result, Nil));
  }
  nusmv_assert(isTypeOK);/* the type should be always OK */

  nusmv_yylineno = lineno_tmp; /* restore line number */
  return result;
}

void
PredicateNormaliser_get_predicates_only(const PredicateNormaliser_ptr self,
                                        Set_t* preds, node_ptr expr)
{
  PREDICATE_NORMALISER_CHECK_INSTANCE(self);


  if (Nil == expr) return;

  /* Process the high level expressions.
     See comment inside PredicateNormaliser_normalise_expr about AND and CONS
     used to connect high level expressions.

     Note for developers: AND and CONS used to connect high level expressions
     are never type checked because they may be incorrect wrt type rules.
     I.e. do not type check them here!

     Note: even if a normal expression is met here there is no problem
     because only boolean expressions can be met here and only their
     operands have to be processes. the expression itself is of no
     importance.
  */
  if (CONS == node_get_type(expr) || AND == node_get_type(expr)) {
    PredicateNormaliser_get_predicates_only(self, preds, car(expr));
    PredicateNormaliser_get_predicates_only(self, preds, cdr(expr));
  }
  else {
    hash_ptr a_hash;
    { /* debugging code : only boolean (or statement) expressions can be met here */
      SymbType_ptr type = TypeChecker_get_expression_type(self->checker, expr, Nil);
      nusmv_assert(SymbType_is_boolean(type) ||
                   SYMB_TYPE_STATEMENT == SymbType_get_tag(type));
    }

    a_hash = new_assoc();

    pred_norm_get_predicates_only(self, preds, expr, a_hash);

    free_assoc(a_hash);
  }
  return;
}

void
PredicateNormaliser_print_predicates_only(const PredicateNormaliser_ptr self,
                                          FILE* stream, node_ptr expr)
{
  Set_t preds;
  Set_Iterator_t iter;

  PREDICATE_NORMALISER_CHECK_INSTANCE(self);

  preds = Set_MakeEmpty();
  PredicateNormaliser_get_predicates_only(self, &preds, expr);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    SET_FOREACH(preds, iter) {
      node_ptr pred = Set_GetMember(preds, iter);
      print_node(wffprint, stream, pred);
      fprintf(stream, "\n\n");
    }
  }

  Set_ReleaseSet(preds);
  return;
}


/*---------------------------------------------------------------------------*/
/* Static function definitions                                               */
/*---------------------------------------------------------------------------*/

/*!
  \brief initialiser of an object of this class


*/
static void pred_norm_init(PredicateNormaliser_ptr self,
                           SymbTable_ptr st)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));

  env_object_init(ENV_OBJECT(self), env);
  /* This is needed since the predicate_normalizer_get_handled_hash
     requires the ST in the structure */
  self->st = st;

#if PN_USES_ST_HANDLED_HASH
  self->expr2normalisedPredicate = predicate_normalizer_get_handled_hash(self);
#else
  self->expr2normalisedPredicate = new_assoc();
#endif

  self->checker = SymbTable_get_type_checker(st);

  OVERRIDE(Object, finalize) = pred_norm_finalize;
}

/*!
  \brief finalizer of this class


*/
static void pred_norm_finalize(Object_ptr object, void* dummy)
{
  PredicateNormaliser_ptr self = PREDICATE_NORMALISER(object);

  pred_norm_deinit(self);

  FREE(self);
}

/*!
  \brief de-initialiser of an object of this class


*/
static void pred_norm_deinit(PredicateNormaliser_ptr self)
{
  nusmv_assert(TYPE_CHECKER(NULL) != self->checker);
  /* check this is initialised object */
  nusmv_assert((hash_ptr)NULL != self->expr2normalisedPredicate);

#if PN_USES_ST_HANDLED_HASH
  /* The hash is handled in the symbol table, thus there is no need to
     destroy it here, it will be destroyed together with the symbol
     table. */
#else
  clear_assoc(self->expr2normalisedPredicate);
  free_assoc(self->expr2normalisedPredicate);
#endif

  self->expr2normalisedPredicate = (hash_ptr)NULL;
  self->st = SYMB_TABLE(NULL);
  self->checker = TYPE_CHECKER(NULL);

  env_object_deinit(ENV_OBJECT(self));
}


/*!
  \brief Performs the predicate-normalisation of an expression

  See PredicateNormaliser_normalise_expr
   for more info on predicate-normalisation.
   This function is the main part of normalisation.

   NB for developers: the main idea behind this function is that
   if a not-boolean operation is applied (to one or two not-boolean operands),
   then ITE from operands must be pushed up to the root of the expression.
   For example,
   "case a : 3; true : 5; esac + 400"
   is converted to
   "case a : 3 + 400; true : 5 + 400; esac"
   i.e. after convertion not-boolean operand will not have ITE, and
   therefore will not have boolean subexpression.

   Boolean expressions are not changed since boolean expressions
   may have any subexpression.


  \sa PredicateNormaliser_normalise_expr
*/

static node_ptr pred_norm_normalise(PredicateNormaliser_ptr self,
                                    node_ptr expr,
                                    node_ptr context,
                                    boolean expand_defines)
{
  SymbType_ptr type;
  node_ptr key, result;
  int node_type;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

#ifndef NDEBUG
  Logger_ptr const logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
#endif

  nusmv_assert(Nil != expr);

  /* already processed */
  key = pred_normalizer_make_key(nodemgr, expand_defines, context, expr);
  result = find_assoc(self->expr2normalisedPredicate, key);
  if (Nil != result) return result;

#ifndef NDEBUG
  Logger_vnlog_trace(logger, wffprint, opts, "\nINPUT\n%N",
                     expr);
#endif

  node_type = node_get_type(expr);
  type = TypeChecker_get_expression_type(self->checker, expr, context);

  /* process every kind of an expression */
  switch (node_type) {
  case CONTEXT:
    {
      node_ptr new_ctx = CompileFlatten_concat_contexts(env, context, car(expr));
      result = pred_norm_normalise(self, cdr(expr), new_ctx, expand_defines);
    }
    break;

    /* list of simple constants that do not need normaliasation */
  case FAILURE:
  case FALSEEXP:
  case TRUEEXP:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case TWODOTS:
    result = ExprMgr_resolve(exprs, self->st, node_type, car(expr), cdr(expr));
    break;

  case UWCONST:
  case SWCONST:
    result = ExprMgr_resolve(exprs, self->st, node_type,
               pred_norm_normalise(self, car(expr), context, expand_defines),
               pred_norm_normalise(self, cdr(expr), context, expand_defines));
    break;

  case ARRAY: {
    ResolveSymbol_ptr rs;

    rs = SymbTable_resolve_symbol(self->st, expr, context);

    if (ResolveSymbol_is_undefined(rs)) {
      /* Array may be an identifier-with-brackets and may be
         expression.  Here an array-expression is detected =>
         expression is to be flattened at first to resolve array
         identifiers-with-brackets (see description of
         flattener_core_flatten for details) and then general
         predicate normalizer is to be invoked */
      node_ptr tmp = Compile_FlattenSexp(self->st, expr, context);
      nusmv_assert(tmp != expr); /* loop in recursion is impossible */
      result = pred_norm_normalise(self, tmp, Nil, expand_defines);
      break;
    }
    else {
      /* array is actually identifier => process it with other identifiers */
    }
    /* NO BREAK HERE */
  }

  case DOT:
  case ATOM:
  case BIT: {
    /* The expression is a symbol.  It can be a variable, a define, a
       constant or a parameter or a function.  The expression may not
       have been flattened as well as not flattened.

       Note, that NO ERRORS CAN BE HERE, since all the error
       situations have been checked during type-checking of the
       original expression.
    */

    /* First, try to resolve the symbol */
    ResolveSymbol_ptr rs;
    node_ptr resolvedName;

    rs = SymbTable_resolve_symbol(self->st, expr, context);
    resolvedName = ResolveSymbol_get_resolved_name(rs);

     /* Check whether this is a variable */
    if (ResolveSymbol_is_var(rs)) {
      result = resolvedName;
    }

    /* check whether is a define */
    else if (ResolveSymbol_is_define(rs)) {
      boolean body_is_boolean =  SymbType_is_boolean(type);

      result = resolvedName;
      if (expand_defines) {
        node_ptr def = SymbTable_get_define_flatten_body(self->st,
                                                         resolvedName);

        /* Whether we expand or not the defines, we return the flattened
           symbol or the recursion on the body of the define */
        /* the context is Nil because expr is already flattened */
        result = pred_norm_normalise(self, def, Nil, expand_defines);
        body_is_boolean =
          SymbType_is_boolean(TypeChecker_get_expression_type(self->checker,
                                                              def, Nil));
      }

      /* Special case: array define may be declared with Integer (or
         higher) subtype and at the same time has a boolean element.
         In this case the boolean element has to be casted to integer.
      */
      if (ARRAY == node_type && !SymbType_is_boolean(type) && body_is_boolean) {
        /* boolean can be casted to Int, Int-Symb or their Sets only
           thus conversion to integer is enough*/
        nusmv_assert(SymbType_is_integer(type) ||
                     SymbType_is_int_symbolic_enum(type) ||
                     SYMB_TYPE_SET_INT == SymbType_get_tag(type) ||
                     SYMB_TYPE_SET_INT_SYMB == SymbType_get_tag(type));
        if (pred_norm_is_true_bool_exp(self, result, Nil)) {
          result = pred_norm_bool2int(self, result);
        }
      }
    }
    /* check whether this symbol is a constant. The ResolveSymbol
       takes care of simple/complex constants */
    else if (ResolveSymbol_is_constant(rs)) {
      result = resolvedName;
    }
    else if (ResolveSymbol_is_function(rs)) {
      result = resolvedName;
    }
    /* check whether this symbol is a parameter */
    else {
      node_ptr param = Nil;
      node_ptr new_ctx;

      if (!ResolveSymbol_is_parameter(rs)) {
        /* it must be a parameter, since the expr is already
           type-checked, but being a parameter is the last
           possibility */
        ErrorMgr_internal_error(errmgr, "Symbol is unexpectedly undefined: %s",
                       sprint_node(wffprint, resolvedName));
      }
      param = SymbTable_get_actual_parameter(self->st,
                                             resolvedName);

      new_ctx = SymbTable_get_actual_parameter_context(self->st,
                                                       resolvedName);

      result = pred_norm_normalise(self, param, new_ctx, expand_defines);
    }

    break;
  } /* ATOM */

    /* unary expression
       or binary expressions those right child has not to be normalized */
  case EX: case AX: case EF: case AF: case EG: case AG:
  case OP_NEXT: case OP_PREC: case OP_NOTPRECNOT: case OP_GLOBAL:
  case OP_HISTORICAL: case OP_FUTURE: case OP_ONCE:

  case EBF: case ABF: case EBG: case ABG: /* ignore the number..number part */
  case ABU: case EBU: /* ignore the number..number part */

    nusmv_assert(SymbType_is_boolean(type)); /* only boolean can be here */
    /* [MRAT]: here the code does fall through going to the remaining part
       [MRAT]: in paticular the to the code that handle NOT UMINUS... Not all
       [MRAT]: of the above operators is unary!

       [RC]: ok as stated above, the number..number parts are ignored.
    */
  case UMINUS:
  case FLOOR:
    /* "next" and "init" are here as normal unary operation because
        EQDEF is normal binary operation */
  case NEXT:
  case SMALLINIT:
  case NOT:
    /* [AI] adding unary operator typeof*/
  case TYPEOF: {
    /* note that NOT and UMINUS may be boolean and/or words */
    node_ptr op1 = pred_norm_normalise(self, car(expr), context, expand_defines);
    /* do not normalise right operand if any */
    node_ptr op2 = find_atom(nodemgr, cdr(expr));

    if (SymbType_is_boolean(type)) { /* this is boolean => just return */
      result = ExprMgr_resolve(exprs, self->st, node_type, op1, op2);
    }
    else { /* this is not a boolean => push all ITE up.
              note that temporal operators cannot be here as they are boolean */
      /* if operand of UMINUS is boolean => convert it into integer */
      if (UMINUS == node_type &&
          pred_norm_is_true_bool_exp(self, op1, context)) {
        op1 = pred_norm_bool2int(self, op1);
      }

      result = pred_norm_push_ite_up(self, node_type, op1, op2);
    }

    break;
  }

    /* CONS is like AND */
  case CONS: {
    node_ptr op1 =
      pred_norm_normalise(self, car(expr), context, expand_defines);
    node_ptr op2 = Nil == cdr(expr)
      ? ExprMgr_true(exprs)
      : pred_norm_normalise(self, cdr(expr), context, expand_defines);

    if (SymbType_is_boolean(type)) { /* this is boolean => just return */
      result = pred_norm_find_node(self, AND, op1, op2);
    }
    else { /* this is not a boolean => push all ITE up */
      result = pred_norm_push_ite_up(self, AND, op1, op2);
    }
    break;
  }

    /* binary expression */
  case AND: case OR: case XOR: case XNOR: case IFF: case IMPLIES:
  case AU: case EU:
  case UNTIL: case SINCE: {
    node_ptr op1 =
      pred_norm_normalise(self, car(expr), context, expand_defines);
    node_ptr op2 = Nil == cdr(expr)
      ? Nil
      : pred_norm_normalise(self, cdr(expr), context, expand_defines);

    if (SymbType_is_boolean(type)) { /* this is boolean => just return */
      result = pred_norm_find_node(self, node_type, op1, op2);
    }
    else { /* this is not a boolean => push all ITE up.
            note that temporal operators cannot be here as they are boolean. */
      result = pred_norm_push_ite_up(self, node_type, op1, op2);
    }

    break;
  }

    /* relational operators */
  case EQDEF: case SETIN:
  case EQUAL: case NOTEQUAL: case LT: case GT: case LE: case GE: {
    SymbType_ptr type1 = TypeChecker_get_expression_type(self->checker,
                                                         car(expr), context);
    SymbType_ptr type2 = TypeChecker_get_expression_type(self->checker,
                                                         cdr(expr), context);
    node_ptr op1 = pred_norm_normalise(self, car(expr), context, expand_defines);
    node_ptr op2 = pred_norm_normalise(self, cdr(expr), context, expand_defines);

    /* some type conversion may be required */
    /* both operands are boolean (or bool set for EQDEF and SETIN) */
    if ((SymbType_is_boolean(type1) ||
         SYMB_TYPE_SET_BOOL == SymbType_get_tag(type1)) &&
        (SymbType_is_boolean(type2) ||
         SYMB_TYPE_SET_BOOL == SymbType_get_tag(type2))) {
      /* do nothing; just return */
      result = pred_norm_find_node(self, node_type, op1, op2);
    }
    /* [AI] check for unbounded/bounded arrays do nothing */
    else if ((SymbType_is_intarray(type1) && SymbType_is_intarray(type2)) ||
             (SymbType_is_wordarray(type1) && SymbType_is_wordarray(type2))) {
      result = pred_norm_find_node(self, node_type, op1, op2);
    }
    /* left is boolean and the right is word[1] => convert word1 to bool */
    else if (SymbType_is_boolean(type1) && SymbType_is_word_1(type2)) {
      result = pred_norm_find_node(self, node_type, op1,
                                   ExprMgr_resolve(exprs, self->st, CAST_BOOL, op2, Nil));
    }
    /* left is word[1] and the right is boolean => convert word1 to bool */
    else if (SymbType_is_word_1(type1) && SymbType_is_boolean(type2)) {
      result = pred_norm_find_node(self, node_type,
                                   ExprMgr_resolve(exprs, self->st, CAST_BOOL, op1, Nil),
                                   op2);
    }
    /* both operands are scalar => convert "true boolean" operand (if
       there is one) to 1, and then push all ITE up from operands.
       For example, (5 mod 2) = 4 should not be converted as it is
       purely not-boolean, but (5 = 3) > 4 should be converted to
       "ITE(5=3, 1, 0) > 4" then normalisation shoud push the ITE up.
       NB: Originally "(5 mod 2)", "0", "1" are all boolean.
    */
    else {
      /* special case: assignment bool := int-expr.
         The assignment has to be changed to "in".
         Otherwise, at the end we will get, e.g., 1 := exp which
         is illegal as LHS is not var */
      if (EQDEF == node_type  &&
          SymbType_is_boolean(type1) &&
          (SymbType_is_integer(type2)
           || SYMB_TYPE_SET_INT == SymbType_get_tag(type2))) {
        node_type = SETIN;
      }

      if (pred_norm_is_true_bool_exp(self, op1, context)) {
        op1 = pred_norm_bool2int(self, op1);
      }

      if (pred_norm_is_true_bool_exp(self, op2, context)) {
        op2 = pred_norm_bool2int(self, op2);
      }
      result = pred_norm_push_ite_up(self, node_type, op1, op2);
    }

    break;
  }

    /* these exprs are always scalar, so only
       "true boolean" operand should be converted to integer.
       The pushing the ITE up from operands is always performed.
       (even though "exp mod 2" is a boolean expression,
       there is no need to deal with it as with boolean).
    */
  case TIMES: case DIVIDE: case PLUS :case MINUS: case MOD:
  case LSHIFT: case RSHIFT: /*case LROTATE: case RROTATE: */
  case CAST_UNSIGNED:
  case CAST_SIGNED:
  case EXTEND:
  case WAREAD: /* case WAWRITE:
                  [AI] separating write as it has three operands
               */
  case CAST_TO_UNSIGNED_WORD:
  case CONST_ARRAY: { /* casts, extend and these two WordArray
                                  operators cannot have boolean operands
                                  (type checked already). It is OK.
                                  WAWRITE has actually three-operands
                                  but the second and third are dealt recursively
                               */
    node_ptr op1 = pred_norm_normalise(self, car(expr), context, expand_defines);
    node_ptr op2 = Nil == cdr(expr)
      ? Nil
      : pred_norm_normalise(self, cdr(expr), context, expand_defines);

    /* below code is required to change, e.g "(a!=b)+5=6"
       into "((a!=b)?1:0) +5 = 6" and then obtain "(a!=b) ? 1+5=6 : 0+5=0" */
    if (pred_norm_is_true_bool_exp(self, op1, context)) {
      op1 = pred_norm_bool2int(self, op1);
    }

    if (op2 != Nil && pred_norm_is_true_bool_exp(self, op2, context)) {
      op2 = pred_norm_bool2int(self, op2);
    }

    result = pred_norm_push_ite_up(self, node_type, op1, op2);
    break;
  }

  case WAWRITE: {
    node_ptr op1 = pred_norm_normalise(self, car(expr), context, expand_defines);
    node_ptr op2 = pred_norm_normalise(self, car(cdr(expr)), context, expand_defines);
    node_ptr op3 = pred_norm_normalise(self, cdr(cdr(expr)), context, expand_defines);

    /* below code is required to change, e.g "(a!=b)+5=6"
       into "((a!=b)?1:0) +5 = 6" and then obtain "(a!=b) ? 1+5=6 : 0+5=0" */
    if (pred_norm_is_true_bool_exp(self, op1, context)) {
      op1 = pred_norm_bool2int(self, op1);
    }

    if (pred_norm_is_true_bool_exp(self, op2, context)) {
      op2 = pred_norm_bool2int(self, op2);
    }

    if (pred_norm_is_true_bool_exp(self, op3, context)) {
      op3 = pred_norm_bool2int(self, op3);
    }
    result = pred_norm_push_ite_up(self, node_type, op1,
                                   pred_norm_push_ite_up(self, node_type, op2, op3));
    break;
  }

  case WORDARRAY_TYPE:
  case INTARRAY_TYPE: {
    /* [AI] this case is added as Mathsat return model for arrays in terms of
       CONSTARRAY(array integer of ..., value). As it is not easy to replace
       the first argument with TYPEOF node, since TYPEOF takes a variable id
       as parameter. This case happens when we do simulation.
       We handle that case here for normalization for array integer ... */
    result = expr;
    break;
  }

    /* COLON cannot go as a independent operation */
  case COLON:
    error_unreachable_code();

  case BIT_SELECTION: {
    node_ptr op1 = pred_norm_normalise(self, car(expr), context, expand_defines);
    node_ptr op2;

    /* We must expand the bits to ensure they resolve to numbers */
    node_ptr hbit = pred_norm_normalise(self, car(cdr(expr)), context, true);
    node_ptr lbit = pred_norm_normalise(self, cdr(cdr(expr)), context, true);

    /*just consistency check*/
    nusmv_assert(COLON == node_get_type(cdr(expr)));
    nusmv_assert(Nil != hbit && NUMBER == node_get_type(hbit));
    nusmv_assert(Nil != lbit && NUMBER == node_get_type(lbit));

    op2 = find_node(nodemgr, COLON, hbit, lbit);

    result = pred_norm_push_ite_up(self, node_type, op1, op2);
    break;
  }

  case WSIZEOF: {
    /* here we rely on the flattener to resolve the WSIZEOF to NUMBER */
    node_ptr num = Compile_FlattenSexp(self->st, expr, context);
    nusmv_assert(NUMBER == node_get_type(num));
    result = pred_norm_normalise(self, num, Nil, false);
    break;
  }

  case WRESIZE: {
    node_ptr op1 = pred_norm_normalise(self, car(expr), context, expand_defines);
    node_ptr op2 = pred_norm_normalise(self, cdr(expr), context, expand_defines);

    result = pred_norm_push_ite_up(self, node_type, op1, op2);
    break;
  }

  case CAST_TOINT: {
    node_ptr op1 = pred_norm_normalise(self, car(expr), context, expand_defines);
    nusmv_assert(Nil == cdr(expr)); /* indeed no right child */

    result = pred_norm_push_ite_up(self, node_type, op1, Nil);
    break;
  }

  case COUNT: {
    node_ptr list = car(expr);
    node_ptr new_list = Nil, find_list = Nil;

    while (Nil != list) {
      node_ptr elem = pred_norm_normalise(self, car(list), context, expand_defines);
      new_list = cons(nodemgr, elem, new_list);
      list = cdr(list);
    }

    while (Nil != new_list) {
      node_ptr tmp = car(new_list);

      find_list = find_node(nodemgr, CONS, find_atom(nodemgr, tmp), find_list);

      tmp = new_list;
      new_list = cdr(new_list);
      free_node(nodemgr, tmp);
    }

    result = pred_norm_push_ite_up(self, node_type, find_list, Nil);
    break;
  }

    /* concatenation requires two word arguments (i.e. scalar).  the
       only required thing is to convert bool to word1 (independent if
       it is a true boolean or one of 0, 1, mod2.
    */
  case CONCATENATION: {
    SymbType_ptr type1 = TypeChecker_get_expression_type(self->checker,
                                                         car(expr), context);
    SymbType_ptr type2 = TypeChecker_get_expression_type(self->checker,
                                                         cdr(expr), context);

    node_ptr op1 = pred_norm_normalise(self, car(expr), context, expand_defines);
    node_ptr op2 = pred_norm_normalise(self, cdr(expr), context, expand_defines);

    if (SymbType_is_boolean(type1)) {
      op1 = pred_norm_bool2word1(self, op1);
    }
    if (SymbType_is_boolean(type2)) {
      op2 = pred_norm_bool2word1(self, op2);
    }

    result = pred_norm_push_ite_up(self, node_type, op1, op2);

    break;
  }
    /* pushes down bool cast if operand is a case */
  case CAST_BOOL: {
    node_ptr op1 = pred_norm_normalise(self, car(expr), context, expand_defines);
    nusmv_assert(cdr(expr) == Nil);

    result = pred_norm_push_ite_up(self, node_type, op1, Nil);
    break;
  }

  case CAST_WORD1:
    result = pred_norm_normalise(self, car(expr), context, expand_defines);
    result = pred_norm_bool2word1(self, result);
    break;

    /* UNION's operands are sets.
       For boolean sets we do not know if it will be used as boolean
       or as integer.
       For example, A := (B=C) union 1.
       If A is boolean nothing to be done, but if A is int then
       the expression should become B=C ? A := 1 union 1 : A := 0 union 1.
       But his info is not available at time union is processed.

       Thus to be save the normalization, cast bool2int and ITE-push-up
       is performed always.

       Unfortunately, bool2int cast cannot be applied to union, and
       thus it has to be applied in advance to every boolean operand,
       even if potentially not required.

       NB: if this code is to be changed then change also the same
       part in PredicateExtractor.c.
    */
  case UNION: {
    node_ptr op1 = pred_norm_normalise(self, car(expr), context, expand_defines);
    node_ptr op2 = pred_norm_normalise(self, cdr(expr), context, expand_defines);

     /* see comment above */
    if (pred_norm_is_true_bool_exp(self, op1, context)) {
      op1 = pred_norm_bool2int(self, op1);
    }
    if (pred_norm_is_true_bool_exp(self, op2, context)) {
      op2 = pred_norm_bool2int(self, op2);
    }

    result = pred_norm_push_ite_up(self, UNION, op1, op2);
    break;
  }

  case IFTHENELSE:
  case CASE: {
    node_ptr cond, then, tail;
    nusmv_assert(COLON == node_get_type(car(expr)));
    cond = pred_norm_normalise(self, car(car(expr)), context, expand_defines);
    then = pred_norm_normalise(self, cdr(car(expr)), context, expand_defines);
    tail = pred_norm_normalise(self, cdr(expr), context, expand_defines);

    if (SymbType_is_boolean(type)) {
      /* this is boolean ITE  => do nothing */
      result = ExprMgr_resolve(exprs, self->st, CASE,
                            ExprMgr_resolve(exprs, self->st, COLON, cond, then), tail);
    }
    else result = pred_norm_normalise_ite(self, cond, then, tail);

    break;
  }

  case ATTIME: {
    node_ptr op1 = pred_norm_normalise(self, car(expr), context, expand_defines);
    /* do not normalise right operand if any */
    node_ptr op2 = find_atom(nodemgr, cdr(expr));

    if (SymbType_is_boolean(type)) { /* this is boolean => just return */
      result = ExprMgr_resolve(exprs, self->st, node_type, op1, op2);
    }
    else { /* this is not a boolean => push all ITE up */
      result = pred_norm_push_ite_up(self, node_type, op1, op2);
    }

    break;
  }

  case NFUNCTION:
    {
      /* For functions, we normalise actual parameters */
      node_ptr params = cdr(expr);
      node_ptr new_params = Nil;
      node_ptr tmp = Nil;
      node_ptr p;

      while (Nil != params) {
        p = pred_norm_normalise(self, car(params), context, expand_defines);
        tmp = cons(nodemgr, p, tmp);
        params = cdr(params);
      }

      /* Reverse the new parameter list, and create nodes using find_node */
      while (Nil != tmp) {
        node_ptr _tmp;

        new_params = find_node(nodemgr, CONS, car(tmp), new_params);

        _tmp = tmp;
        tmp = cdr(tmp);
        free_node(nodemgr, _tmp);
      }

      result = ExprMgr_function(exprs, car(expr), new_params);
    }
    break;

  default:
    /* TODO[MD] This is wrong. Should be ifdef. See issue 3009. Two other cases
       around. */
#ifndef NDEBUG
    result = ExprMgr_resolve(exprs, self->st, node_type,
                          pred_norm_normalise(self, car(expr), context, expand_defines),
                          pred_norm_normalise(self, cdr(expr), context, expand_defines));
    break;
#else
    {
      const MasterPrinter_ptr sexpprinter =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_SEXP_PRINTER));
      const StreamMgr_ptr streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

      StreamMgr_nprint_error(streams, sexpprinter, "%N", expr);
      StreamMgr_print_error(streams,  "unknown token = %d\n", node_type);
      error_unreachable_code(); /* unknown kind of an expression */
    }
#endif
  } /* switch */

  /* remember the processed expression */
  /* the expression cannot be processed twice */
  nusmv_assert(Nil == find_assoc(self->expr2normalisedPredicate, key));
  insert_assoc(self->expr2normalisedPredicate, key, result);

#ifndef NDEBUG
  Logger_vnlog_trace(logger, wffprint, opts, "\nOUTPUT\n%N",
                     result);
#endif

  return result;
}


/*!
  \brief Create a new node with the help of find_node function

  This function is the same as find_node, except that if
   the operator is commutative then operands are ordered before applying
   find_node. This may result in a greater sharing.

   THIS BEHAVIOUR IS CURRENTLY DISABLED!

   Note that it is necessary to use find_node during construction
   of the expression, because later subexpressions with not-boolean operands
   will be assigned a new boolean var. Since find_node is used,
   the same var will be assigned to the same subexpression (because pointer
   will be the same).
*/

static node_ptr pred_norm_find_node(PredicateNormaliser_ptr self,
                                    int kind, node_ptr op1, node_ptr op2)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  switch (kind) {
    /* commutative operations */
  case AND: case OR: case XOR: case XNOR: case IFF:
  case EQUAL: case NOTEQUAL:  case TIMES: case PLUS:
  case UNION:
    /* order the operands (somehow) */
    if (op1 > op2) {
      node_ptr tmp = op1;
      op1 = op2;
      op2 = tmp;
    }
    break;

    /* noncommutative operations */
  case FALSEEXP: case TRUEEXP: case FAILURE:
  case DOT:  case ATOM:  case SELF:  case ARRAY:  case BIT_SELECTION:
  case NEXT: case SMALLINIT: case NOT: case UMINUS: case IMPLIES:
  case COLON: case CASE:  case CAST_WORD1:  case CAST_BOOL:
  case EQDEF:  /* I think, EQDEF is not commutative */
  case LT: case GT: case LE: case GE:
  case DIVIDE: case MINUS: case LSHIFT: case RSHIFT: case MOD:
  case NUMBER: case NUMBER_UNSIGNED_WORD: case NUMBER_SIGNED_WORD:
  case UWCONST: case SWCONST:
  case NUMBER_FRAC: case NUMBER_REAL:
  case NUMBER_EXP:
  case TWODOTS:  case CONCATENATION:
  case EX: case AX: case EF: case AF: case EG: case AG:
  case ABU: case EBU: case EBF: case ABF: case EBG: case ABG:
  case AU: case EU:
  case OP_NEXT: case OP_PREC: case OP_NOTPRECNOT: case OP_GLOBAL:
  case OP_HISTORICAL: case OP_FUTURE: case OP_ONCE:
  case UNTIL: case SINCE: case CONTEXT:
  case WAREAD: case WAWRITE: case ATTIME:
  case SETIN: /* AG */
  case EXTEND: case WRESIZE:
  case FLOOR: case COUNT:
  case CAST_UNSIGNED: case CAST_SIGNED: case CAST_TOINT:
  case WSIZEOF: case NFUNCTION:
  case CONST_ARRAY: /* AI */
  case TYPEOF:
  case CAST_TO_UNSIGNED_WORD:
    break;

  default:
#if 0 /* def NDEBUG */
    break;
#else
    {
      const MasterPrinter_ptr sexpprinter =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_SEXP_PRINTER));
      const StreamMgr_ptr streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
      const NodeMgr_ptr nodemgr =
        NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

      StreamMgr_nprint_error(streams, sexpprinter, "%N", new_node(nodemgr, kind, op1, op2));
      error_unreachable_code(); /* unknown kind of an expression */
    }
#endif
  } /* switch */

  return ExprMgr_resolve(exprs, self->st, kind, op1, op2);
}

/*!
  \brief Returns true is the given expression is a boolean
   expression

  This function returns true if the expression
   is "really boolean", i.e. has a boolean types and
   NOT constants "0" and "1" and NOT mod-operation.
   (Currently, "0", "1" and "expr mod 2" is officially a boolean expression).

   This function is used to detect cases when a boolean expression
   must be converted to an integer one. And this function decreases the number
   of such conversions.
*/

static boolean pred_norm_is_true_bool_exp(PredicateNormaliser_ptr self,
                                          node_ptr expr,
                                          node_ptr context)
{
  /* application of "next" or "init" does not influence on whether
     the expressions is truly boolean or not */
  node_ptr unnexted
    = (node_get_type(expr) == NEXT || node_get_type(expr) == INIT)
    ? car(expr) : expr;

  SymbType_ptr type = TypeChecker_get_expression_type(self->checker, expr,
                                                      context);

  return SymbType_is_boolean(type)
    && NUMBER != node_get_type(unnexted)
    && MOD != node_get_type(unnexted);
  /* note that check for number being 0/1 or mod's second operand being 2
     is not done as check for boolean type is enough */
}


/*!
  \brief Casts a boolean expression to an integer expression

  the function takes an boolean expression and
   converts its to integer by the following way:
   if "expr" is original expressions, then the result is
   "case expr: 1; 0; esac"
*/

static node_ptr pred_norm_bool2int(PredicateNormaliser_ptr self,
                                   node_ptr expr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  return
    /* optimization: TRUE becomes 1 and FALSE become 0 */
    (TRUEEXP == node_get_type(expr)) ? ExprMgr_true(exprs) :
    ((FALSEEXP == node_get_type(expr)) ? ExprMgr_false(exprs) :
     /* normal conversion */
     ExprMgr_resolve(exprs, self->st, CASE,
                  ExprMgr_resolve(exprs, self->st, COLON, expr, ExprMgr_true(exprs)), ExprMgr_false(exprs))
    );
}


/*!
  \brief Cast a boolean expression to a Word[1] expression

   the function takes an boolean expression and
   converts its to Word[1] by the following way:
   if "expr" is an original expressions then the result is
   "case expr: 0b1_1; 0b1_0; esac"

*/

static node_ptr pred_norm_bool2word1(PredicateNormaliser_ptr self,
                                     node_ptr expr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));

  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (ExprMgr_is_true(exprs, expr) || ExprMgr_is_number(exprs, expr, 1)) {
    return ExprMgr_resolve(exprs, self->st, NUMBER_UNSIGNED_WORD,
                        (node_ptr) WordNumberMgr_integer_to_word_number(words, 1,1), Nil);
  }
  if (ExprMgr_is_false(exprs, expr) || ExprMgr_is_number(exprs, expr, 0)) {
    return ExprMgr_resolve(exprs, self->st, NUMBER_UNSIGNED_WORD,
                        (node_ptr) WordNumberMgr_integer_to_word_number(words, 0,1), Nil);
  }

  return ExprMgr_resolve(exprs, self->st, CASE,
                      ExprMgr_resolve(exprs, self->st, COLON, expr,
                                   ExprMgr_resolve(exprs, self->st, NUMBER_UNSIGNED_WORD,
                                                (node_ptr)WordNumberMgr_integer_to_word_number(words, 1,1),
                                                Nil)),
                      ExprMgr_resolve(exprs, self->st, NUMBER_UNSIGNED_WORD,
                                   (node_ptr)WordNumberMgr_integer_to_word_number(words, 0,1),
                                   Nil));
}



/*!
  \brief Applies an operators to two (or one) not-boolean operands
   The returned value is predicate-normalised

  The function takes an operator and its two operands and
   return an expression of applying the operator to operands.
   The operands are assumed to be not-boolean.
   The distinguishing feature of the function is that
   if operands are ITE expresssions, then ITE will be pushed up from
   operands to the root of the result expression. For example,
   if input operator is "+",
   the left operator is "case a1 : e1; a2 : e2; 1 : e3; esac"
   the right opeator is "case b1 : s1; b2 : s2; 1 : s3; esac"
   then the result will be (if nested ITE are not allowed, see
   ALLOW_NESTED_CASES)
   "case
   a1 & b1 : e1 + s1;
   a1 & b2 : e1 + s2;
   a1      : e1 + s3;
   a2 & b1 : e2 + s1;
   a2 & b2 : e2 + s2;
   a2      : e2 + s3;
   b1      : e3 + s1;
   b2      : e3 + s2;
   1       : e3 + s3;
   esac"
   another possible result can be (if nested ITE are allowed):
   "case
   a1 : case b1 : e1 + s1;
             b2 : e1 + s2;
                   1  : e1 + s3;
        esac;
   a2 : case b1 : e2 + s1;
             b2 : e2 + s2;
             1  : e2 + s3;
        esac;
   b1 : e3 + s1;
   b2 : e3 + s2;
   1  : e3 + s3;
   esac"

   Macro ALLOW_NESTED_CASES switches these two behaviours.

   If the given operator is unary then the op2 is Nil.

   NB: If the operator is unary, the second operand will be Nil.

   NB: if the operator is NEXT or SMALLINIT, then it, of course, applied to
   the conditions of ITE also (not only to the second and third operators).

*/

static node_ptr pred_norm_push_ite_up(PredicateNormaliser_ptr self,
                                      int op, node_ptr op1, node_ptr op2)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  /* Note :
     NEXT and SMALLINIT are special operators since they
     have to be applied not only to the second and third operands of
     ITE but also to the first.
  */

  if (CASE == node_get_type(op1)) {
    /* push_ite_up(ITE(c, t, e) + op2) =
       ITE(c, puch_ite_up(t + op2), push_ite_up(e + op2))
       and normalise, since  op2 - may be a list of ITE.
    */
    node_ptr cond = car(car(op1));
    /* apply the operation on op2 and op1's tail (without first CASE).
       Of op1's tail is FAILURE simply return this tail
    */
    node_ptr tail = (FAILURE == node_get_type(cdr(op1))) ? cdr(op1)
      : pred_norm_push_ite_up(self, op, cdr(op1), op2);

    /* RC: This code adds an hard to track down implicit requirement

     * The problem here is that when the grammar gets changed (e.g. a
     * new node is added) the code below might required that new node
     * is added among the handled cases. This is an implicit
     * requirement that is very hard to track down. For example when
     * ATTIME was added, a bug was found, as the new node was not
     * added here. */

    if (NEXT == op || SMALLINIT == op || ATTIME == op) {
      cond = ExprMgr_resolve(exprs, self->st, op, cond, op2);
    }

    /* apply the operation on op2 and the first CASE of op1.
       Here the problem is that op2 can be a CASE and since
       CASEs may not be nestable, it may be necessary to push all CASE form
       op2 to the CASE list of op1
    */
    return pred_norm_push_ite_up_conditioned(self, cond,
                                             op, cdr(car(op1)), op2,
                                             tail);
  }
  else if (Nil != op2 && CASE == node_get_type(op2)) {
    /* push_ite_up(op1 + ITE(c, t, e)) =
       ITE(c, op1 + t, push_ite_up(op1 + e))

       op1 - cannot be an ITE.
       Even if nested ITE are not allowed
       nor futher normalisation required since t then cannot be a list of ITE.
       NB: If op2's tail is FAILURE then keep it as it is.
    */
    node_ptr cond = car(car(op2));
    node_ptr first_expr = pred_norm_push_ite_up(self, op, op1,
                                                cdr(car(op2)));
    node_ptr second_expr
      = FAILURE == node_get_type(cdr(op2)) ? cdr(op2)
      : pred_norm_push_ite_up(self, op, op1, cdr(op2));

    return ExprMgr_resolve(exprs, self->st, CASE, ExprMgr_resolve(exprs, self->st, COLON, cond, first_expr),  second_expr);
  }
  /* both operands are not ITE */
  else {
    return pred_norm_find_node(self, op, op1, op2);
  }
}

/*!
  \brief This function takes condition (cond),
   operator (op), three expressions (exp1, exp2 and tail) and creates
   an expression ITE(cond, op(exp1, exp2), tail) and then pushes all
   ITE from exp1 and exp2 up (if required).

  The function creates a new expression with the semantics
   ITE(condition, exp1 op exp2, tail)
   'condition' is the condition of ITE,
   op - binary operation,
   exp1, exp2 - expression,
   tail - is the else-expression of created ITE.

   This function is used to normalise not-boolean expressions (exp1 and exp2).

   If nested ITE are not allowed, then exp1 cannot be an ITE expression, but
   exp2 can be. In this case the return value is
   a linearised (one-dimensional) ITE expression (i.e. only
   else-expression of ITE can contain another ITE). To obtain such
   form, given 'condition' will be conjuncted with all conditions in
   exp2, and exp1 will be applied to all expressions in exp2 and the
   final expression in the list of ITE will be 'tail'.

   Otherwise, nested ITE are allowed and no normalisation is required.

   If the given operator is unary then the op2 is Nil.
*/

static node_ptr
pred_norm_push_ite_up_conditioned(PredicateNormaliser_ptr self,
                                  node_ptr condition,
                                  int op,
                                  node_ptr exp1,
                                  node_ptr exp2,
                                  node_ptr tail)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

#if 0 == ALLOW_NESTED_CASES  /* nested ITE are NOT allowed! */

  nusmv_assert(CASE != node_get_type(exp1));
  nusmv_assert(FAILURE != node_get_type(exp1));

  if (Nil != exp2 && CASE == node_get_type(exp2)) {
    /* exp2 is a list of ITE */
    node_ptr tmp = pred_norm_find_node(self, COLON,
                                       pred_norm_find_node(self, AND,
                                                           condition,
                                                           car(car(exp2))),
                                       pred_norm_find_node(self, op, exp1,
                                                           cdr(car(exp2))));
    node_ptr new_tail = FAILURE == node_get_type(cdr(exp2)) ? cdr(exp2)
      : pred_norm_push_ite_up_conditioned(self, condition, op,
                                          exp1, cdr(exp2), tail);

    return ExprMgr_resolve(exprs, self->st, CASE, tmp, new_tail);
  }
  /* exp2 is not ITE expr, simple apply the operator */
  else {
    node_ptr tmp = ExprMgr_resolve(exprs, self->st, COLON, condition,
                                pred_norm_find_node(self, op, exp1, exp2));
    return pred_norm_find_node(self, CASE, tmp, tail);
  }

#elif 1 == ALLOW_NESTED_CASES   /* nested ITE are allowed */
  /* [AT] Optimization to be implemented:
     let b be a boolean var, define d1 := b + b, d2 := d1 + d1; d3 := d2 + d2; d4 := d3+d3;
     Having now INIT d4; cannot be simplified because the size of CASE is huge.

     Solution is to detect if conditions in left case-exp and right
     case-exp are the same and not build full expression in that case.
   */
  node_ptr tmp = pred_norm_push_ite_up(self, op, exp1, exp2);
  return ExprMgr_resolve(exprs, self->st, CASE, ExprMgr_resolve(exprs, self->st, COLON, condition, tmp), tail);

#else
#error ALLOW_NESTED_CASES is incorrectly defined!
#endif
}


/*!
  \brief The function take three expressions (cond, e1, e2) and
   creates ITE(cond, e1, e2). Normalisation is performed if required.

  normalises ITE expression, i.e.
   the function creates "ITE(cond, then, tail)"  and normalised it.

   The 'then' and 'tail' expressions are assumed to be not-boolean.

   If nested ITE are not allowed and 'then' is an ITE-expression
   then normalisation is performed, i.e. all ITE are moved up from 'then'.
   For example,
   ITE(c1, ITE(c2, t2, e2), e1)
   will become
   ITE(c1&c2, t2, ITE(c1, e2, e1)),
   and the result will be ITE list (without nested ITE). Note that in
   this case t2 cannot be ITE expression(has been already
   normalised)).
*/

static node_ptr
pred_norm_normalise_ite(PredicateNormaliser_ptr self,
                        node_ptr cond, node_ptr then, node_ptr tail)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
#if 0 == ALLOW_NESTED_CASES  /* nested ITE are NOT allowed */

  if (CASE == node_get_type(then)) {
    /* 'then' is ITE expression => modify it.*/
    node_ptr new_tail = (FAILURE == node_get_type(cdr(then))) ? cdr(then)
      : pred_norm_normalise_ite(self, cond, cdr(then), tail);

    return ExprMgr_resolve(exprs, self->st, CASE,
                        ExprMgr_resolve(exprs, self->st, COLON,
                                     pred_norm_find_node(self, AND, cond, car(car(then))),
                                     cdr(car(then))),
                        new_tail);
  }
  /* 'then' is not ITE expression => just create the expression*/
  else return ExprMgr_resolve(exprs, self->st, CASE, ExprMgr_resolve(exprs, self->st, COLON, cond, then), tail);


#elif 1 == ALLOW_NESTED_CASES /* nested ITE are allowed */
  /* just create the expression */
  node_ptr tmp = ExprMgr_resolve(exprs, self->st, COLON, cond, then);
  return ExprMgr_resolve(exprs, self->st, CASE, tmp, tail);

#else
#error ALLOW_NESTED_CASES is incorrectly defined!
#endif
}

/*!
  \brief This is implementation of PredicateNormaliser_get_predicates_only

  already_processed is a hash table of already processed
   expressions. Since every predicate has to be collected only once
   there is no need to process the same exp twice.
*/
static void
pred_norm_get_predicates_only(const PredicateNormaliser_ptr self,
                              Set_t* preds, node_ptr expr,
                              hash_ptr already_processed)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (find_assoc(already_processed, expr)) return;

  /* process every kind of an expression */
  switch (node_get_type(expr)) {
  case TRUEEXP:
  case FALSEEXP:
  case FAILURE:
    return; /* do not hash simple expression, it is waste of time and memory */

    /* No numbers can be met */
  case NUMBER:
    error_unreachable_code();
    /* nusmv_assert((node_get_int(expr) >> 1) == 0); */
    return; /* do not hash simple expression, it is waste of time and memory */

  case DOT:
  case ATOM:
  case ARRAY: {
    SymbType_ptr type = TypeChecker_get_expression_type(self->checker, expr, Nil);
    nusmv_assert(SymbType_is_boolean(type)); /* only bool var can be met */
    break;
  }

    /* unary expressions whose operands are boolean */
  case NEXT: case SMALLINIT:
  case NOT:
  case EX: case AX: case EF: case AF: case EG:  case AG:
  case EBF: case ABF: case EBG: case ABG: /* ignore the number..number part */
  case ABU: case EBU: /* ignore the number..number part */
  case OP_NEXT: case OP_PREC: case OP_NOTPRECNOT: case OP_GLOBAL:
  case OP_HISTORICAL: case OP_FUTURE: case OP_ONCE:
    pred_norm_get_predicates_only(self, preds, car(expr), already_processed);
    break;

    /* binary expressions whose operands are boolean */
  case AND: case OR: case XOR: case XNOR: case IFF: case IMPLIES:
  case AU: case EU: case UNTIL: case SINCE: case CONS:
    pred_norm_get_predicates_only(self, preds, car(expr), already_processed);
    pred_norm_get_predicates_only(self, preds, cdr(expr), already_processed);
    break;

    /* operand of bool-cast is always word[1] => add it */
  case CAST_BOOL:  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const WordNumberMgr_ptr words =
      WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));
    SymbType_ptr type;

    /* here the expression word[1] is casted to boolean.
       added to the set of predicates "expr == 0d1_1" */
    expr = car(expr);
    type = TypeChecker_get_expression_type(self->checker, expr, Nil);
    nusmv_assert(SymbType_is_word_1(type));

    expr = ExprMgr_resolve(exprs, self->st, EQUAL, expr,
                        find_node(nodemgr, NUMBER_UNSIGNED_WORD,
                                  (node_ptr)WordNumberMgr_integer_to_word_number(words, 1,1), Nil));
    *preds = Set_AddMember(*preds, (Set_Element_t) expr);
    break;
  }

    /* three operands expression which do not change the type */
  case CASE:
    pred_norm_get_predicates_only(self, preds, car(car(expr)), already_processed);
    pred_norm_get_predicates_only(self, preds, cdr(car(expr)), already_processed);
    if (FAILURE != node_get_type(cdr(expr))) {
      pred_norm_get_predicates_only(self, preds, cdr(expr), already_processed);
    }
    break;

    /* binary expression whose operands may not be boolean */
  case EQDEF: case SETIN:
  case EQUAL: case NOTEQUAL: case LT: case GT: case LE: case GE: {
    SymbType_ptr type1 = TypeChecker_get_expression_type(self->checker,
                                                         car(expr), Nil);
    SymbType_ptr type2 = TypeChecker_get_expression_type(self->checker,
                                                         cdr(expr), Nil);

    if (SymbType_is_boolean(type1) &&
        (SymbType_is_boolean(type2) ||
         SYMB_TYPE_SET_BOOL == SymbType_get_tag(type2))) {
      pred_norm_get_predicates_only(self, preds, car(expr), already_processed);
      pred_norm_get_predicates_only(self, preds, cdr(expr), already_processed);
    }
    else {
      /* the expression is not boolean.
         after normalisation both operands must be boolean or not boolean only together.
         Let's check it here. */
      nusmv_assert(!pred_norm_is_true_bool_exp(self, car(expr), Nil) &&
                   !pred_norm_is_true_bool_exp(self, cdr(expr), Nil));

      /* here the expression is added to the set of predicates (if it was not added before) */
      if (!Set_IsMember(*preds, (Set_Element_t)expr)) {
        *preds = Set_AddMember(*preds, (Set_Element_t) expr);
      }
    }
    break;
  }

  case UNION:
    /* only boolean (set)  expresssions may be here */
    nusmv_assert(SYMB_TYPE_SET_BOOL ==
                 SymbType_get_tag(TypeChecker_get_expression_type(self->checker,
                                                                  expr, Nil)));
    pred_norm_get_predicates_only(self, preds, car(expr), already_processed);
    pred_norm_get_predicates_only(self, preds, cdr(expr), already_processed);
    break;

   /* Below are expressions which can never be met at this point
      because only boolean one can be here and only expanded expressions. */
  case CONTEXT:  /* after normalisation the expr should be expanded */
    /* these exps should have beem met earlier*/
  case NUMBER_UNSIGNED_WORD: case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:  case NUMBER_REAL:  case NUMBER_EXP:
  case UWCONST: case SWCONST:
  case TWODOTS:
  case COUNT:

  case WSIZEOF: case WRESIZE: case CAST_TOINT:
    /* not- boolean expression cannot be met here */
  case TIMES: case DIVIDE: case PLUS: case MINUS: case MOD:
  case LSHIFT: case RSHIFT: /*case LROTATE: case RROTATE: */
  case BIT_SELECTION:
  case WAREAD: case WAWRITE:
  case CONST_ARRAY:
  case TYPEOF:
  case CONCATENATION:
  case COLON:
  case CAST_WORD1: /* normalisation have already converted CAST_WORD1 to ITE*/
  case CAST_SIGNED:   case CAST_UNSIGNED:   case EXTEND:
  case CAST_TO_UNSIGNED_WORD:
    error_unreachable_code();
    break;

  default:
    {
      const MasterPrinter_ptr sexpprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_SEXP_PRINTER));

      StreamMgr_nprint_error(streams, sexpprint, "%N", expr);

      ErrorMgr_internal_error(errmgr, "unknown kind of an expression (see above)");
    }
  } /* switch */


  insert_assoc(already_processed, expr, NODE_PTR(1));
  return;
}


static hash_ptr
predicate_normalizer_get_handled_hash(PredicateNormaliser_ptr self) {
  hash_ptr result = (hash_ptr)NULL;

  PREDICATE_NORMALISER_CHECK_INSTANCE(self);

  result =
    SymbTable_get_handled_hash_ptr(self->st,
                                   PREDICATE_NORMALIZER_HASH,
                                   (ST_PFICPCP)NULL,
                                   (ST_PFICPI)NULL,
                                   predicate_normalizer_hash_free,
                                   (SymbTableTriggerFun)NULL,
                                   SymbTable_clear_handled_remove_action_hash,
                                   (SymbTableTriggerFun)NULL
                                   );
  return result;
}

static assoc_retval predicate_normalizer_hash_free(char *key, char *data, char * arg)
{
  return ASSOC_DELETE;
}

static node_ptr
pred_normalizer_make_key(const NodeMgr_ptr nodemgr, boolean expand_define,
                         node_ptr context, node_ptr expr)
{
  return find_node(nodemgr, CONTEXT + (expand_define ? 1 : 0), context, expr);
}
