/* ---------------------------------------------------------------------------


  This file is part of the ``ltl2smv'' package of NuSMV version 2.
  Copyright (C) 1998-2005 by CMU and FBK-irst.

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
  \author Marco Roveri, Andrei Tchaltsev
  \brief Functions performing conversion of LTL formula to
  SMV module

  see the header of file ltl2smv.h

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/ltl/ltl2smv/ltl2smv.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/wff/ExprMgr.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"

#include <stdarg.h>
/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* MACRO declaration                                                         */
/*---------------------------------------------------------------------------*/
/* Set or unset the debugging mode (value 1 and 0 respectively).*/
/* #define OUTPUT_DEBUGGING 0 */

/*
   The NO_DEFINE_FOR_OR_and_NOT_OPT define controls whether in the
   tableau construction it is created or not a DEFINE for each OR and
   NOT occurring in the rewritten formula. Originally, a DEFINE was
   created for each OR and NOT occurring in the rewritten
   formula. Now, by default this creation is disabled. The reason
   being that on large LTL properties having disabled such creation
   results in a drammatic reduction of the size of the generated
   internal representation of the MODULE corresponding to the tableau,
   and as a consequence also the size of the generated file, and in
   its subsequent handling by the flattener. To re-enable the old
   behavior comment the following define.
*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NO_DEFINE_FOR_OR_and_NOT_OPT

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define PRE_PREFIX "LTL_"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define PREFIXNAME "_SPECF_"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_LTL2SMV_DEFINE_DECL "l2sDefDecl"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_LTL2SMV_VAR_DECL "l2sVarDecl"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_LTL2SMV_TRANS_DECL "l2sTraDecl"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_LTL2SMV_JUSTICE_DECL "l2sJusDecl"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_LTL2SMV_INIT_DECL "l2sIniDecl"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_LTL2SMV_EXPR_TO_NAME "l2sE2N"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_LTL2SMV_UNIQUE_POS_NUMBER "l2sUPN"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_LTL2SMV_SPECIFICATION_NUMBER "l2sSN"

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void initialise_transformation(const NuSMVEnv_ptr env,
                                      unsigned int specificationNumber);
static void deinitialise_transformation(const NuSMVEnv_ptr env);

static node_ptr normalise_formula(const NuSMVEnv_ptr env, node_ptr t);
static node_ptr perform_memory_sharing(const NuSMVEnv_ptr env, node_ptr t);
static node_ptr transform_ltl_expression(const NuSMVEnv_ptr env,
                                         node_ptr t,
                                         boolean polarity,
                                         const Ltl2SmvPrefixes* prefixes);
static node_ptr generate_smv_module(const NuSMVEnv_ptr env,
                                    node_ptr t,
                                    boolean single_justice,
                                    const Ltl2SmvPrefixes* prefixes);

static node_ptr expr_to_name(const NuSMVEnv_ptr env,
                             node_ptr node,
                             boolean always,
                             const Ltl2SmvPrefixes* prefixes);
static void add_to_list(NodeList_ptr list, node_ptr node);
static string_ptr generate_string(UStringMgr_ptr strings, const char *format, ...);
static node_ptr expand_case_body(const NuSMVEnv_ptr env, node_ptr);
static node_ptr generate_expr_name(NuSMVEnv_ptr env,
                                   const char * pre_prefix,
                                   const char * prefix_name);
#ifdef OUTPUT_DEBUGGING
static void ltl2smv_print_module(NuSMVEnv_ptr env, FILE* ostream, node_ptr module);
#endif

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

node_ptr ltl2smv(NuSMVEnv_ptr env, unsigned int uniqueId,
                 node_ptr in_ltl_expr)
{ 
  return ltl2smv_core(env, uniqueId, in_ltl_expr, false, NULL);
}

node_ptr ltl2smv_single_justice(NuSMVEnv_ptr env, unsigned int uniqueId,
                                node_ptr in_ltl_expr)
{
  return ltl2smv_core(env, uniqueId, in_ltl_expr, true, NULL);
}

node_ptr ltl2smv_core(NuSMVEnv_ptr env,
                      unsigned int uniqueId,
                      node_ptr in_ltl_expr,
                      boolean single_justice,
                      const Ltl2SmvPrefixes* prefixes)
{
  MasterPrinter_ptr const wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  StreamMgr_ptr const streams =
   STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  {
    /* the line number is set to LTL expression's line number.  */
    extern int nusmv_yylineno;
    nusmv_yylineno = node_get_lineno(in_ltl_expr);
  }

  /* DEBUGGING */
#ifdef OUTPUT_DEBUGGING
  StreamMgr_print_output(streams,  "-- original LTL expression : ");
  StreamMgr_nprint_output(streams, wffprint, "%N", in_ltl_expr);
  StreamMgr_print_output(streams,  "\n");
#endif

  in_ltl_expr = normalise_formula(env, in_ltl_expr);

  /* DEBUGGING */
#ifdef OUTPUT_DEBUGGING
  StreamMgr_print_output(streams,  "-- normalised LTL expression : ");
  StreamMgr_nprint_output(streams, wffprint, "%N", in_ltl_expr);
  StreamMgr_print_output(streams,  "\n");
#endif

  initialise_transformation(env, uniqueId);

  in_ltl_expr = transform_ltl_expression(env, in_ltl_expr, false, prefixes);
  in_ltl_expr = generate_smv_module(env, in_ltl_expr, single_justice, prefixes);

  deinitialise_transformation(env);

#ifdef OUTPUT_DEBUGGING
  ltl2smv_print_module(env, stdout, in_ltl_expr);
#endif

  return in_ltl_expr;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Initialises the conversion performed by this package

  The specificationNumber is the same operand provided to ltl2smv

  \se initialises all the data structures required for conversion

  \sa ltl2smv
*/
static void initialise_transformation(const NuSMVEnv_ptr env,
                                      unsigned int specificationNumber)
{
  /* the body of the module being created during transformation.
     See transform_ltl_expression for more info
  */
  NodeList_ptr define_declarations;
  NodeList_ptr trans_declarations;
  NodeList_ptr var_declarations;
  NodeList_ptr justice_declarations;
  NodeList_ptr init_declarations;

  hash_ptr expr_to_name_hash = NULL;


  expr_to_name_hash = new_assoc();

  define_declarations = NodeList_create();
  trans_declarations = NodeList_create();
  init_declarations = NodeList_create();
  justice_declarations = NodeList_create();
  var_declarations = NodeList_create();

  NuSMVEnv_set_value(env, ENV_LTL2SMV_DEFINE_DECL, define_declarations);
  NuSMVEnv_set_value(env, ENV_LTL2SMV_TRANS_DECL, trans_declarations);
  NuSMVEnv_set_value(env, ENV_LTL2SMV_INIT_DECL, init_declarations);
  NuSMVEnv_set_value(env, ENV_LTL2SMV_JUSTICE_DECL, justice_declarations);
  NuSMVEnv_set_value(env, ENV_LTL2SMV_VAR_DECL, var_declarations);

  NuSMVEnv_set_value(env, ENV_LTL2SMV_EXPR_TO_NAME, expr_to_name_hash);

  /* Here 1 is added since 0 may clash with NULL value. */
  NuSMVEnv_set_or_replace_value(env, ENV_LTL2SMV_UNIQUE_POS_NUMBER,
                                PTR_FROM_INT(void*, 1));

  NuSMVEnv_set_or_replace_value(env, ENV_LTL2SMV_SPECIFICATION_NUMBER,
                                PTR_FROM_INT(void*, specificationNumber + 2));
}

/*!
  \brief deinitialises the package

  The only required thing is to free the hash table: exp -> name
*/
static void deinitialise_transformation(const NuSMVEnv_ptr env)
{
  NodeList_ptr define_declarations =
    NODE_LIST(NuSMVEnv_remove_value(env, ENV_LTL2SMV_DEFINE_DECL));
  NodeList_ptr trans_declarations =
    NODE_LIST(NuSMVEnv_remove_value(env, ENV_LTL2SMV_TRANS_DECL));
  NodeList_ptr var_declarations =
    NODE_LIST(NuSMVEnv_remove_value(env, ENV_LTL2SMV_VAR_DECL));
  NodeList_ptr justice_declarations =
    NODE_LIST(NuSMVEnv_remove_value(env, ENV_LTL2SMV_JUSTICE_DECL));
  NodeList_ptr init_declarations =
    NODE_LIST(NuSMVEnv_remove_value(env, ENV_LTL2SMV_INIT_DECL));

  hash_ptr expr_to_name_hash =
    (hash_ptr)NuSMVEnv_remove_value(env, ENV_LTL2SMV_EXPR_TO_NAME);

  NodeList_destroy(define_declarations);
  NodeList_destroy(trans_declarations);
  NodeList_destroy(var_declarations);
  NodeList_destroy(justice_declarations);
  NodeList_destroy(init_declarations);

  free_assoc(expr_to_name_hash);
}

/*!
  \brief Normalises the formula

  Normalisations will create a new formula with
  the following transformations:
   "a & b"   => "!(!a | !b)"
   "a -> b"  => "!a | b"
   "a <-> b"  => "!(!a | !b) | !(a | b)"
   "a xnor b" => "!(!a | !b) | !(a | b)"
   "a xor b"  => "!(!a | b) | !(!a | b)"
   "Z a" => "! Y !a"
   "F a" => "1 U a"
   "G a" => "!(1 U !a)"
   "a V b" => "!(!a U !b)"
   "O a" => "1 S a"
   "H a" => "!(1 S !a)"
   "a T b" => "!(!a S !b)"

   If two consecutive NOT are met => both removed.

   The expression is also memory-shared, i.e. find_atom
   of find_node is invoked on every(!) expressions, including
   on the leaf-nodes (i.e. nodes not participating in the conversion directly).
   
*/
static node_ptr normalise_formula(const NuSMVEnv_ptr env, node_ptr t)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ExprMgr_ptr exprs =
    EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  SymbTable_ptr const symb_table =
    SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));

  node_ptr left = Nil, right = Nil;
  node_ptr or, or1, or2, tmp;
  node_ptr retval = Nil;

#ifdef OUTPUT_DEBUGGING
  {
    StreamMgr_ptr const streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
    MasterPrinter_ptr const wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    StreamMgr_inc_indent_size(streams);
    StreamMgr_nprint_output(streams, wffprint,
                            "%s: Input formula:\n%N\n",
                            __func__,
                            t);
  }
#endif

  if (Nil == t) {
    retval =  Nil;
  }
  else {
    /* proceed with every kind of expression separately*/
    switch (node_get_type(t)) {
    case NOT: /* ! */
      left = normalise_formula(env, car(t));
      retval = ExprMgr_not(exprs, left);
      break;

    case OP_NEXT: /* X */
    case OP_PREC: /* Y */
      left = normalise_formula(env, car(t));
      retval = find_node(nodemgr, node_get_type(t), left, Nil);
      break;

    case OR: /* | */
      left = normalise_formula(env, car(t));
      right = normalise_formula(env, cdr(t));
      retval = ExprMgr_or(exprs, left, right);
      break;

    case UNTIL: /* U */
    case SINCE: /* S */
      left = normalise_formula(env, car(t));
      right = normalise_formula(env, cdr(t));
      retval = find_node(nodemgr, node_get_type(t), left, right);
      break;

    case AND: /* & */
      left = normalise_formula(env, car(t));
      right = normalise_formula(env, cdr(t));
      or  = ExprMgr_or(exprs, ExprMgr_not(exprs, left), ExprMgr_not(exprs, right));
      retval = ExprMgr_not(exprs, or);
      break;

    case IMPLIES: /* ->  */
      left = normalise_formula(env, car(t));
      right = normalise_formula(env, cdr(t));
      retval = ExprMgr_or(exprs, ExprMgr_not(exprs, left), right);
      break;

    case IFF: /* <-> */
    case XNOR: /* xnor */
      /* a <-> b  =>  (!(!a | !b) | !(a | b)) */
      left = normalise_formula(env, car(t));
      right = normalise_formula(env, cdr(t));
      or1 = ExprMgr_or(exprs, ExprMgr_not(exprs, left), ExprMgr_not(exprs, right));
      or2 = ExprMgr_or(exprs, left, right);
      retval = ExprMgr_or(exprs, ExprMgr_not(exprs, or1), ExprMgr_not(exprs, or2));
      break;

    case XOR: /* xor */
      /* a xor b   =>   (!(!a | b) | !(a | !b)) */
      left = normalise_formula(env, car(t));
      right = normalise_formula(env, cdr(t));
      or1 = ExprMgr_or(exprs, ExprMgr_not(exprs, left), right);
      or2 = ExprMgr_or(exprs, left, ExprMgr_not(exprs, right));
      retval = ExprMgr_or(exprs, ExprMgr_not(exprs, or1), ExprMgr_not(exprs, or2));
      break;

    case OP_NOTPRECNOT: /* Z  */
      left = normalise_formula(env, car(t));
      tmp = find_node(nodemgr, OP_PREC, ExprMgr_not(exprs, left), Nil);
      retval = ExprMgr_not(exprs, tmp);
      break;

    case OP_FUTURE: /* F */
      left = normalise_formula(env,car(t));
      retval = find_node(nodemgr, UNTIL, ExprMgr_true(exprs), left);
      break;

    case OP_GLOBAL: /* G  */
      left = normalise_formula(env, car(t));
      tmp = find_node(nodemgr, UNTIL, ExprMgr_true(exprs), ExprMgr_not(exprs, left));
      retval = ExprMgr_not(exprs, tmp);
      break;

    case OP_ONCE: /* O */
      left = normalise_formula(env, car(t));
      retval = find_node(nodemgr, SINCE, ExprMgr_true(exprs), left);
      break;

    case OP_HISTORICAL: /* H */
      left = normalise_formula(env, car(t));
      tmp = find_node(nodemgr, SINCE, ExprMgr_true(exprs), ExprMgr_not(exprs, left));
      retval = ExprMgr_not(exprs, tmp);
      break;

    case TRIGGERED: /* T */
    case RELEASES: /* V */
      error_unreachable_code(); /* T and V were transformed by the parser */

      /* Leafs, i.e. all the usual kinds of expressions.
         They all considered as an atomic expressions and not
         participate in LTL -> Module transformation.
         Just perform memory-sharing.
      */
      /* In previous version of this function the leafs were expected to
         be already shared, but now they are obligitory shared here
         (the sharing makes the return module potentially smaller).
      */

      /* Expressions: a simplification can occur, this could be not a leaf
         anymore, we need to normalize, see issues 3681, 3694, 3907 */
    case IFTHENELSE:
    case CASE:
    case UWCONST: case SWCONST:
    case TWODOTS:   case SELF:
    case BIT_SELECTION:  case CONCATENATION: case EXTEND:
    case WSIZEOF: case WRESIZE:  case CAST_TOINT:
    case CAST_BOOL:  case CAST_WORD1:  case CAST_SIGNED:  case CAST_UNSIGNED:
    case TIMES: case DIVIDE: case PLUS :case MINUS: case MOD: case UMINUS:
    case LSHIFT: case RSHIFT: case LROTATE: case RROTATE:
    case UNION: case SETIN:
    case EQUAL: case NOTEQUAL: case LT: case GT: case LE: case GE:
    case FLOOR: case COUNT:
    case NFUNCTION:
    case WAREAD: case WAWRITE:
        retval =  perform_memory_sharing(env, t);

        if (retval != t) {
          retval = normalise_formula(env, retval);
        }

      break;

      /* leaves... */
    case NUMBER:  case NUMBER_UNSIGNED_WORD:  case NUMBER_SIGNED_WORD:
    case FAILURE: case FALSEEXP:  case TRUEEXP:
    case NUMBER_FRAC:  case NUMBER_REAL: case NUMBER_EXP:
      /* ... and symbols */
    case ATOM: case DOT:  case ARRAY:
      /* array constants */
    case CONST_ARRAY: case TYPEOF:
      retval =  perform_memory_sharing(env, t);
      break;

    default:
      error_unreachable_code(); /*  unknown operator */
      retval =  Nil;
      break;
    }
  }

#ifdef OUTPUT_DEBUGGING
  {
    StreamMgr_ptr const streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
    MasterPrinter_ptr const wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    StreamMgr_nprint_output(streams, wffprint,
                            "%s: Output formula:\n%N\n",
                            __func__,
                            retval);
    StreamMgr_dec_indent_size(streams);
  }
#endif

  return retval;
}

/*!
  \brief Make all sub-expressions of the expression
  to share as much memory as possible, i.e. the same sub-expressions
  will have the same pointer.

  
*/
static node_ptr perform_memory_sharing(const NuSMVEnv_ptr env, node_ptr t)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ExprMgr_ptr exprs =
    EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
  Logger_ptr const logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
  MasterPrinter_ptr const wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  node_ptr retval = Nil;

  if (t == (node_ptr) NULL) return retval;

#ifdef OUTPUT_DEBUGGING
  Logger_inc_indent_size(logger);
  Logger_vnlog_error(logger, wffprint, opts, "Input: %N\n", t);
#endif

  switch (node_get_type(t)) {
    /* 0-arity */
  case FAILURE: case FALSEEXP: case TRUEEXP:
  case NUMBER: case NUMBER_UNSIGNED_WORD: case NUMBER_SIGNED_WORD:
  case UWCONST: case SWCONST:
  case NUMBER_FRAC: case NUMBER_REAL:
  case NUMBER_EXP:
  case ATOM: case SELF:
    retval = find_atom(nodemgr, t);
    break;
    /* not, unary minus */
  case NOT:
  case UMINUS:
    if (node_get_type(t) == node_get_type(car(t))) {
      /* remove duplicated NOT and UMINUS */
      retval = perform_memory_sharing(env, car(car(t)));
      break;
    }
    /* else behave as a usual unary operator */
    /* 1-arity */
  case CAST_BOOL:  case CAST_WORD1: case CAST_SIGNED: case CAST_UNSIGNED:
  case FLOOR: case COUNT:
    retval = ExprMgr_resolve(exprs, st, node_get_type(t),
                             perform_memory_sharing(env, car(t)), cdr(t));
    break;
    /* 1 or 2 arity */
  case DOT:
    if (Nil == car(t)) {
      retval =  find_node(nodemgr, node_get_type(t),
                          Nil,
                          perform_memory_sharing(env, cdr(t)));
      break;
    }
    else { /* skip to 2-arity expressions */ }
    /* 2-arity */
  case OR: case AND: case XOR: case XNOR: case IFF: case IMPLIES:
  case TWODOTS: case ARRAY: case CONS:
  case BIT_SELECTION:  case CONCATENATION: case EXTEND:
  case WSIZEOF: case WRESIZE: case CAST_TOINT:
  case CASE: /* CASE is no longer a part of LTL formula - just a usual exp */
  case IFTHENELSE:
  case COLON:
  case TIMES: case DIVIDE: case PLUS :case MINUS: case MOD:
  case LSHIFT: case RSHIFT: case LROTATE: case RROTATE:
  case UNION: case SETIN:
  case EQUAL: case NOTEQUAL: case LT: case GT: case LE: case GE:
    retval = ExprMgr_resolve(exprs, st, node_get_type(t),
                             perform_memory_sharing(env, car(t)),
                             perform_memory_sharing(env, cdr(t)));
    break;
    /* Function expression */
  case NFUNCTION:
    retval = ExprMgr_resolve(exprs, st, node_get_type(t),
                             perform_memory_sharing(env, car(t)),
                             perform_memory_sharing(env, cdr(t)));
    break;

  case WAREAD: case WAWRITE: case CONST_ARRAY: case TYPEOF:
    retval = ExprMgr_resolve(exprs, st, node_get_type(t),
                             perform_memory_sharing(env, car(t)),
                             perform_memory_sharing(env, cdr(t)));
    break;

  default:
    {
      StreamMgr_ptr const streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
      MasterPrinter_ptr const wffprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
      ErrorMgr_ptr const errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

      StreamMgr_nprint_error(streams, wffprint,
                           "Unexpected expression found during %s:%N\n",
                           __func__,
                           t);
      ErrorMgr_nusmv_exit(errmgr, 1);
    }
  }

#ifdef OUTPUT_DEBUGGING
  Logger_vnlog_error(logger, wffprint, opts, "Output: %N\n", retval);
  Logger_dec_indent_size(logger);
#endif

  return retval;
}

/*!
  \brief The main conversion function.

  It fills in the lists : var_declarations,
  define_declarations, trans_declarations, init_declarations,
  justice_declarations. These lists are list of nodes.
  var_declarations: a list of ATOM (future VAR declarations)
  define_declarations: a list of DEFINES body, i.e. EQDEF (see syntax
  of DEFINE) trans_declarations: a list of TRANS expressions (but not
  TRANS itself) init_declarations: a list of INIT expressions (but not
  INIT itself) justice_declarations: a list of JUSTICE expressions(but
  not JUSTICE itself)

  NB: The elements of the lists use memory-sharing so
  pointers can be compare to check their uniqueness.
  (But the lists themselves do not use memory-sharing)

  Returns the name of the input expression (see expr_to_name
  for more details).

  NB: The memory of expression is expected to be shared. Then the
  same name will be used for the same sub-expression.
  
*/
static node_ptr transform_ltl_expression(const NuSMVEnv_ptr env,
                                         node_ptr t,
                                         boolean polarity,
                                         const Ltl2SmvPrefixes* prefixes)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ExprMgr_ptr exprs =
    EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  NodeList_ptr define_declarations =
    NODE_LIST(NuSMVEnv_get_value(env, ENV_LTL2SMV_DEFINE_DECL));
  NodeList_ptr trans_declarations =
    NODE_LIST(NuSMVEnv_get_value(env, ENV_LTL2SMV_TRANS_DECL));
  NodeList_ptr var_declarations =
    NODE_LIST(NuSMVEnv_get_value(env, ENV_LTL2SMV_VAR_DECL));
  NodeList_ptr justice_declarations =
    NODE_LIST(NuSMVEnv_get_value(env, ENV_LTL2SMV_JUSTICE_DECL));
  NodeList_ptr init_declarations =
    NODE_LIST(NuSMVEnv_get_value(env, ENV_LTL2SMV_INIT_DECL));

  node_ptr name;
  node_ptr nameLeft;
  node_ptr nameRight;
  node_ptr result;
  node_ptr nameXY; /* an additional X or Y expression */

  switch(node_get_type(t)) {
  case OR:
#ifndef NO_DEFINE_FOR_OR_and_NOT_OPT
    name = expr_to_name(env, t, false, prefixes);
    nameLeft = transform_ltl_expression(env, car(t), polarity, prefixes);
    nameRight = transform_ltl_expression(env, cdr(t), polarity, prefixes);
    /* generate a new DEFINE */
    result = find_node(nodemgr, EQDEF, name, ExprMgr_or(exprs, nameLeft, nameRight));
    add_to_list(define_declarations, result);
    return name;
#else
    nameLeft = transform_ltl_expression(env, car(t), polarity, prefixes);
    nameRight = transform_ltl_expression(env, cdr(t), polarity, prefixes);
    return ExprMgr_or(exprs, nameLeft, nameRight);
#endif

  case NOT:
#ifndef NO_DEFINE_FOR_OR_and_NOT_OPT
    name = expr_to_name(env, t, false, prefixes);
    nameLeft = transform_ltl_expression(env, car(t), !polarity, prefixes);
    /* generate a new DEFINE */
    result = find_node(nodemgr, EQDEF, name, ExprMgr_not(exprs, nameLeft));
    add_to_list(define_declarations, result);
    return name;
#else
    nameLeft = transform_ltl_expression(env, car(t), !polarity, prefixes);
    return ExprMgr_not(exprs, nameLeft);
#endif

  case OP_NEXT:  /* X */
    name = expr_to_name(env, t, false, prefixes);
    /* add to VAR */
    add_to_list(var_declarations, name);

    nameLeft = transform_ltl_expression(env, car(t), polarity, prefixes);

    /* generate a new TRANS */
    result = ExprMgr_equal(exprs, ExprMgr_next(exprs, nameLeft, SYMB_TABLE(NULL)),
                        name,
                        SYMB_TABLE(NULL));
    add_to_list(trans_declarations, result);
    return name;

  case OP_PREC: /* Y */
    name = expr_to_name(env, t, false, prefixes);
    /* add to VAR */
    add_to_list(var_declarations, name);

    nameLeft = transform_ltl_expression(env, car(t), polarity, prefixes);

    /* generate a new TRANS */
    /* Note that the TRANS is similar to X's TRANS, but the NEXT operator
       is applied to a different term */
    result = ExprMgr_equal(exprs, nameLeft,
                        ExprMgr_next(exprs, name, SYMB_TABLE(NULL)),
                        SYMB_TABLE(NULL));
    add_to_list(trans_declarations, result);

    /* generate a new INIT */
    result = ExprMgr_equal(exprs, name, ExprMgr_false(exprs), SYMB_TABLE(NULL));

    add_to_list(init_declarations, result);
    return name ;

  case UNTIL:
    name = expr_to_name(env, t, false, prefixes);
    /* generate X (expr) and add to VAR */
    nameXY = expr_to_name(env, find_node(nodemgr, OP_NEXT, t, Nil), false,
                          prefixes);
    add_to_list(var_declarations, nameXY);

    nameLeft = transform_ltl_expression(env, car(t), polarity, prefixes);
    nameRight = transform_ltl_expression(env, cdr(t), polarity, prefixes);


    /* generate a new DEFINE */
    result = find_node(nodemgr, EQDEF, name, ExprMgr_or(exprs, nameRight,
                                            ExprMgr_and(exprs, nameLeft, nameXY)));
    add_to_list(define_declarations, result);
    /* generate a new TRANS */
    result = ExprMgr_equal(exprs, ExprMgr_next(exprs, name, SYMB_TABLE(NULL)),
                        nameXY,
                        SYMB_TABLE(NULL));
    add_to_list(trans_declarations, result);

    /* if the occurrence of the formula is positive, generate a new JUSTICE */
    if (polarity == false) {
      result = ExprMgr_or(exprs, ExprMgr_not(exprs, name), nameRight);
      add_to_list(justice_declarations, result);
    }
    return name;

  case SINCE:
    name = expr_to_name(env, t, false, prefixes);
    /* generate Y (expr) and add to VAR */
    nameXY = expr_to_name(env, find_node(nodemgr, OP_PREC, t, Nil), false, prefixes);
    add_to_list(var_declarations, nameXY);

    nameLeft = transform_ltl_expression(env, car(t), polarity, prefixes);
    nameRight = transform_ltl_expression(env, cdr(t), polarity, prefixes);

    /* generate a new DEFINE */
    result = find_node(nodemgr, EQDEF, name, ExprMgr_or(exprs, nameRight,
                                            ExprMgr_and(exprs, nameLeft, nameXY)));
    add_to_list(define_declarations, result);
    /* generate a new TRANS */
    result = ExprMgr_equal(exprs, name,
                        ExprMgr_next(exprs, nameXY, SYMB_TABLE(NULL)),
                        SYMB_TABLE(NULL));
    add_to_list(trans_declarations, result);
    /* generate a new INIT */
    result = ExprMgr_equal(exprs, nameXY, ExprMgr_false(exprs), SYMB_TABLE(NULL));
    add_to_list(init_declarations, result);
    return name;

    /* Leafs. Skip them */
  case FAILURE: case FALSEEXP:  case TRUEEXP:
  case NUMBER:  case NUMBER_UNSIGNED_WORD: case NUMBER_SIGNED_WORD:
  case UWCONST: case SWCONST:
  case NUMBER_FRAC:  case NUMBER_REAL:
  case NUMBER_EXP:
  case TWODOTS:  case ATOM:  case SELF: case DOT:  case ARRAY:
  case BIT_SELECTION:  case CONCATENATION: case EXTEND:
  case WSIZEOF: case WRESIZE: case CAST_TOINT:
  case CASE: /* CASE is no longer a part of LTL formula - just a usual exp */
  case IFTHENELSE: case COUNT:
  case CAST_BOOL:  case CAST_WORD1: case CAST_SIGNED: case CAST_UNSIGNED:
  case TIMES: case DIVIDE: case PLUS :case MINUS: case MOD:
  case LSHIFT: case RSHIFT: case LROTATE: case RROTATE:
  case UNION: case SETIN:
  case EQUAL: case NOTEQUAL: case LT: case GT: case LE: case GE:
    return t;
  case NFUNCTION:
    return t;
  default:
    error_unreachable_code(); /* unknown expression. This is for debugging only */
  }
  return Nil;
}

/*!
  \brief Returns a name of the expression or
  the expression itself

  If the expression is an LTL leaf node (i.e. not
  a node participating in the transformation) then the node itself is
  returned.
  If the node is a node participating in the transformation then
  the ATOM node with a unique name is returned.

  NB: The memory sharing for expression will make the whole algorithm
  more efficient because the same name will be returned
  for the same subexpression
*/
static node_ptr expr_to_name(const NuSMVEnv_ptr env,
                             node_ptr node,
                             boolean always,
                             const Ltl2SmvPrefixes* prefixes)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  hash_ptr expr_to_name_hash =
    (hash_ptr)NuSMVEnv_get_value(env, ENV_LTL2SMV_EXPR_TO_NAME);

  node_ptr result = find_assoc(expr_to_name_hash, node);

  char* pre_prefix;
  char* prefix_name;

  pre_prefix = ((NULL == prefixes || NULL == prefixes->pre_prefix)? 
                PRE_PREFIX : prefixes->pre_prefix);

  prefix_name = ((NULL == prefixes || NULL == prefixes->prefix_name)? 
                 PREFIXNAME : prefixes->prefix_name);

  if (Nil != result) 
      return result; /* the association already exists */

  /* no association has been created =>  create a new association */
  switch (node_get_type(node)) {
    /* operators participating in the conversion => create a name */
  case NOT: /* ! */
  case OR: /* | */
  case OP_NEXT: /* X */
  case OP_PREC: /* Y */
  case UNTIL: /* U */
  case SINCE: /* S */{
    result = generate_expr_name(env, pre_prefix, prefix_name);
    break;
  }

    /* Leafs. just return the input node  */
  case FAILURE: case FALSEEXP:  case TRUEEXP:
  case NUMBER:  case NUMBER_UNSIGNED_WORD:  case NUMBER_SIGNED_WORD:
  case UWCONST: case SWCONST:
  case NUMBER_FRAC:  case NUMBER_REAL:
  case NUMBER_EXP:
  case TWODOTS:  case ATOM: case SELF: case DOT:  case ARRAY:
  case BIT_SELECTION:  case CONCATENATION: case EXTEND:
  case WSIZEOF: case WRESIZE: case CAST_TOINT:
  case CASE: /* CASE is no longer a part of LTL formula - just a usual exp */
  case IFTHENELSE:
  case CAST_BOOL:  case CAST_WORD1:  case CAST_SIGNED:  case CAST_UNSIGNED:
  case TIMES: case DIVIDE: case PLUS :case MINUS: case MOD:
  case LSHIFT: case RSHIFT: case LROTATE: case RROTATE:
  case UNION: case SETIN:
  case EQUAL: case NOTEQUAL: case LT: case GT: case LE: case GE:
  case NFUNCTION:
    if (always) {
      result = generate_expr_name(env, pre_prefix, prefix_name);
    }
    else {
      result = node;
    }
    break;
  default: error_unreachable_code(); /* unknown expression: just for debugging */
  }
  /* remember the node and its name */
  insert_assoc(expr_to_name_hash, node, result);

  return result;
}

/*!
  \brief Add a node to a list. If the node is already in the list,
  nothing happens

*/
static void add_to_list(NodeList_ptr list, node_ptr node)
{

  if (!NodeList_belongs_to(list, node)) {
    NodeList_append(list, node);
  }
}

/*!
  \brief Behaves similar to standard sprintf, but will allocates
  the required memory by itself

  Warning:
  1. memory belongs to the function. Do not modify the returned
     string. Consecutive invocations damage the previously returned
     strings.

  2. The limit of generated strings is set to 100 bytes. Do not try
  to generate a bigger string
*/
static string_ptr generate_string(UStringMgr_ptr strings,
                                  const char* format, ...)
{
  char buffer[100]; /* 100 bytes should be enough */

  int len;
  va_list ap;

  va_start(ap, format);
  len = vsnprintf(buffer, 100, format, ap); /* 100 - buffer length */
  va_end(ap);

  nusmv_assert(len >= 0); /* an error in the vsnprintf */
  nusmv_assert(len < 100); /* buffer overflow */
  return UStringMgr_find_string(strings, buffer);

#if 0
  /* TODO[AMa] Untested and with different behavior */
  !!! HERE is the implementation of the string with arbitrary length. !!!
  !!! Apparently this implementation is less efficient !!!!

  /* the buffer is not deallocated every. This is correct */
  static char* buffer = NULL;
  static int bufferSize = 0;

  int len;
  char* returnString;

  do {
    /* try to print with the standard vsnprintf */
    va_list ap;
    va_start(ap, format);
    len = vsnprintf(buffer, bufferSize, format, ap);
    va_end(ap);

    nusmv_assert(len >= 0); /* an error in the vsnprintf */

    /* buffer is too small */
    if (len == bufferSize) {
      bufferSize = 100 + bufferSize * 2; /* increase the buffer */
      buffer = REALLOC(char, buffer, bufferSize);
      len = bufferSize;
    }
  } while(len == bufferSize); /* buffer was increased. Try one more time */

  /* allocate the memory which will be returned */
  returnString = ALLOC(char, len + 1);
  strncpy(returnString, buffer, len + 1);
  return returnString;
#endif
}

/*!
  \brief After invocation of transform_ltl_expression, this
  function generates SMV modules from the obtained lists
  of DEFINEs, INITs, etc.

  The parameter whole_expression_name
  is the name (see expr_to_name) of the whole LTL expression.

  All expressions in the module are memory-shared, but
  the module itself is not. The invoker may modify or delete
  all the declarations (and lists insided VAR and DEFINE), but
  should not modify the expressions (including EQDEF in DEFINEs).
*/
static node_ptr generate_smv_module(const NuSMVEnv_ptr env,
                                    node_ptr whole_expression_name,
                                    boolean single_justice,
                                    const Ltl2SmvPrefixes* prefixes)
{
  const UStringMgr_ptr strings =
    USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  NodeList_ptr define_declarations =
    NODE_LIST(NuSMVEnv_get_value(env, ENV_LTL2SMV_DEFINE_DECL));
  NodeList_ptr trans_declarations =
    NODE_LIST(NuSMVEnv_get_value(env, ENV_LTL2SMV_TRANS_DECL));
  NodeList_ptr var_declarations =
    NODE_LIST(NuSMVEnv_get_value(env, ENV_LTL2SMV_VAR_DECL));
  NodeList_ptr justice_declarations =
    NODE_LIST(NuSMVEnv_get_value(env, ENV_LTL2SMV_JUSTICE_DECL));
  NodeList_ptr init_declarations =
    NODE_LIST(NuSMVEnv_get_value(env, ENV_LTL2SMV_INIT_DECL));

  /* We use an offset of 2 for avoiding clash with 0 and NULL */
  unsigned int specificationNumber =
    (unsigned int)(nusmv_ptrint)NuSMVEnv_get_value(env, ENV_LTL2SMV_SPECIFICATION_NUMBER) - 2;

  node_ptr all_declr;
  ListIter_ptr iter;
  node_ptr tmp;

  char* pre_prefix;
  char* prefix_name;
  char* ltl_module_base_name;
  
  pre_prefix = ((NULL == prefixes || NULL == prefixes->pre_prefix)?
                PRE_PREFIX : prefixes->pre_prefix);

  prefix_name = ((NULL == prefixes || NULL == prefixes->prefix_name)?
                 PREFIXNAME : prefixes->prefix_name);

  ltl_module_base_name = 
      ((NULL == prefixes || NULL == prefixes->ltl_module_base_name)?
        LTL_MODULE_BASE_NAME : prefixes->ltl_module_base_name);

  /* add the INIT with the name of the whole expression */
  all_declr = cons(nodemgr, new_node(nodemgr, INIT, whole_expression_name, Nil), Nil);

  if (single_justice) {
    const ExprMgr_ptr exprs =
      EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
    node_ptr one_fairness_expr = ExprMgr_true(exprs);
    string_ptr name = generate_string(strings, "%s%u%s_one_fairness",
                                      pre_prefix, specificationNumber,
                                      prefix_name);
    node_ptr one_fairness_name = find_node(nodemgr, ATOM, (node_ptr)name, Nil);

    NODE_LIST_FOREACH(justice_declarations, iter) {
      node_ptr val = NodeList_get_elem_at(justice_declarations, iter);
      node_ptr fairness_name = expr_to_name(env, val, true, prefixes);
      all_declr =
        cons(nodemgr,
             new_node(nodemgr, VAR,
                      cons(nodemgr,
                           new_node(nodemgr, COLON, fairness_name,
                                    find_node(nodemgr, BOOLEAN, Nil, Nil)),
                           Nil),
                      Nil),
             all_declr);
      all_declr =
        cons(nodemgr,
             new_node(nodemgr, INIT,
                      new_node(nodemgr, NOT, fairness_name, Nil), Nil),
             all_declr);
      all_declr =
        cons(nodemgr,
             new_node(nodemgr, TRANS,
                      ExprMgr_implies(exprs,
                                      ExprMgr_not(exprs,
                                                  one_fairness_name),
                                      ExprMgr_iff(exprs,
                                                  ExprMgr_or(exprs,
                                                             fairness_name,
                                                             val),
                                                  find_node(nodemgr,
                                                            NEXT,
                                                            fairness_name,
                                                            Nil))),
                      Nil),
             all_declr);
      all_declr =
        cons(nodemgr,
             new_node(nodemgr, TRANS,
                      ExprMgr_implies(exprs,
                                      one_fairness_name,
                                      ExprMgr_iff(exprs,
                                                  val,
                                                  find_node(nodemgr,
                                                            NEXT,
                                                            fairness_name,
                                                            Nil))),
                      Nil),
             all_declr);
      one_fairness_expr = ExprMgr_and(exprs, one_fairness_expr, fairness_name);
    }
    all_declr =
      cons(nodemgr,
           new_node(nodemgr, DEFINE, cons(nodemgr,
                                          new_node(nodemgr,
                                                   EQDEF,
                                                   one_fairness_name,
                                                   one_fairness_expr), Nil),
                    Nil),
           all_declr);
    all_declr =
      cons(nodemgr,
           new_node(nodemgr, JUSTICE, one_fairness_name, Nil),
           all_declr);
  }
  else {
    /* add the JUSTICEs to the list of all declarations */
    tmp = Nil;
    NODE_LIST_FOREACH(justice_declarations, iter) {
      node_ptr val = NodeList_get_elem_at(justice_declarations, iter);
      tmp = cons(nodemgr, new_node(nodemgr, JUSTICE, val, Nil), tmp);
    }
    all_declr = append(all_declr, tmp);
  }

  /* add the INITs to the list of all declarations */
  tmp = Nil;
  NODE_LIST_FOREACH(init_declarations, iter) {
    node_ptr val = NodeList_get_elem_at(init_declarations, iter);
    tmp = cons(nodemgr, new_node(nodemgr, INIT, val, Nil), tmp);
  }
  all_declr = append(all_declr, tmp);

  /* add the TRANSs to the list of all declarations */
  tmp = Nil;
  NODE_LIST_FOREACH(trans_declarations, iter) {
    node_ptr val = NodeList_get_elem_at(trans_declarations, iter);
    tmp = cons(nodemgr, new_node(nodemgr, TRANS, val, Nil), tmp);
  }
  all_declr = append(all_declr, tmp);

  /* add the DEFINESs to the list of all declarations */
  {
/*     node_ptr shared_defines = Nil; /\* defines with shared memory *\/ */
/*     iter = define_declarations; */
/*     while (Nil != iter) { */
/*       shared_defines = find_node(nodemgr, CONS, car(iter), shared_defines); */
/*       iter = cdr(iter); */
/*     } */
/*     all_declr = find_node(nodemgr, CONS, */
/*                           find_node(nodemgr, DEFINE, shared_defines, Nil), all_declr); */
    tmp = Nil;
    NODE_LIST_FOREACH(define_declarations, iter) {
      node_ptr val = NodeList_get_elem_at(define_declarations, iter);
      tmp = cons(nodemgr, val, tmp);
    }
    all_declr = cons(nodemgr, new_node(nodemgr, DEFINE, tmp, Nil), all_declr);
  }

  /* add the VARs to the list of all declarations */
  {
    node_ptr new_vars = Nil;/* all the variable declaration */
    node_ptr boolean_type = find_node(nodemgr, BOOLEAN, Nil, Nil);
    NODE_LIST_FOREACH(var_declarations, iter) {
      node_ptr val = NodeList_get_elem_at(var_declarations, iter);
      new_vars = cons(nodemgr, new_node(nodemgr, COLON, val, boolean_type), new_vars);
    }
    all_declr = cons(nodemgr, new_node(nodemgr, VAR, new_vars, Nil), all_declr);
  }

  /* create the module name */
  {
    string_ptr name = generate_string(strings, "%s%u", ltl_module_base_name,
                                      specificationNumber);
    tmp = find_node(nodemgr, ATOM, (node_ptr)name, Nil);
  }

  return new_node(nodemgr, MODULE, new_node(nodemgr, MODTYPE, tmp, Nil), all_declr);
}

/*!
  \brief Expands the body of a case

  Given a case expression, of the form
  <textarea>
  case
     c1 : e1;
     c2 : e2;
     ...
  esac;
  </textarea>
  it returns <tt> (c1 and e1) or (!c1 and ((c2 and e2) or (!c2 and (....)))) </tt>

*/
static node_ptr expand_case_body(const NuSMVEnv_ptr env, node_ptr expr)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  nusmv_assert(Nil != expr);

  if ((CASE == node_get_type(expr)) ||
      (IFTHENELSE == node_get_type(expr))) {
    node_ptr c, t, e;

    nusmv_assert((COLON == node_get_type(car(expr))));

    c = car(car(expr));
    t = cdr(car(expr));
    e = expand_case_body(env, cdr(expr));

    return ExprMgr_or(exprs, ExprMgr_and(exprs, c, t), ExprMgr_and(exprs, ExprMgr_not(exprs, c), e));
  }

  return expr;
}

/*!
  \brief Generates unique name for the ATOM

  Function expr_to_name returns an ATOM for the node that
  participate in the transformation.

  This function generates an unique name for that ATOM.

*/
static node_ptr generate_expr_name(NuSMVEnv_ptr env,
                                   const char * pre_prefix,
                                   const char * prefix_name)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const UStringMgr_ptr strings =
    USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
  const SymbTable_ptr symb_table =
    SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));

  /* Here 1 is removed since 0 may clash with NULL value. */
  unsigned int uniquePositiveNumber =
    (unsigned int)(nusmv_ptrint)NuSMVEnv_get_value(env, ENV_LTL2SMV_UNIQUE_POS_NUMBER) - 1;

  /* We use an offset of 2 for avoiding clash with 0 and NULL */
  unsigned int specificationNumber =
    (unsigned int)(nusmv_ptrint)NuSMVEnv_get_value(env, ENV_LTL2SMV_SPECIFICATION_NUMBER) - 2;

  string_ptr str = NULL;
  node_ptr result = NULL;
  ResolveSymbol_ptr rs = NULL;

  str = generate_string(strings, "%s%u%s%d", pre_prefix, specificationNumber,
                                     prefix_name, uniquePositiveNumber++);
  result = new_node(nodemgr, ATOM, (node_ptr)str, Nil);
  rs = SymbTable_resolve_symbol(symb_table, result, Nil);

  while(ResolveSymbol_is_defined(rs)) {
    free_node(nodemgr, result);
    str = generate_string(strings, "%s%u%s%d", pre_prefix, specificationNumber,
                          prefix_name, uniquePositiveNumber++);
    result = new_node(nodemgr, ATOM, (node_ptr)str, Nil);
    rs = SymbTable_resolve_symbol(symb_table, result, Nil);
  }

  NuSMVEnv_set_or_replace_value(env, ENV_LTL2SMV_UNIQUE_POS_NUMBER,
                                PTR_FROM_INT(void*, uniquePositiveNumber + 1));

  return result;
}

#ifdef OUTPUT_DEBUGGING

/*!
  \brief Prints the tableau in SMV format

  Prints the tableau in SMV format

  \se None
*/
static void ltl2smv_print_module(NuSMVEnv_ptr env, FILE* ostream, node_ptr module)
{
  MasterPrinter_ptr const wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  node_ptr iter;

  nusmv_assert(Nil != module);
  nusmv_assert(MODULE == node_get_type(module));
  /* print the name */
  nusmv_assert(MODTYPE == node_get_type(car(module)));
  nusmv_assert(ATOM == node_get_type(car(car(module))));
  nusmv_assert(Nil == cdr(car(module)));

  fprintf(ostream, "MODULE %s\n", UStringMgr_get_string_text((string_ptr)car(car(car(module)))));

  iter = cdr(module);
  while (Nil != iter) {
    nusmv_assert(CONS == node_get_type(iter));
    switch (node_get_type(car(iter))) {

    case VAR: { /* variable declarations */
      node_ptr var;
      var = car(car(iter));
      if ( Nil != var) {
        fprintf(ostream, "VAR\n");
        while (Nil != var) { /* iterate over variable declarations */

          nusmv_assert(CONS == node_get_type(var));
          nusmv_assert(COLON == node_get_type(car(var)));
          nusmv_assert(ATOM == node_get_type(car(car(var))));
          nusmv_assert(BOOLEAN == node_get_type(cdr(car(var))));

          fprintf(ostream, "   %s : boolean;\n",
 UStringMgr_get_string_text((string_ptr)car(car(car(var)))));

          var = cdr(var);
        }
      }
      break;
    }

    case DEFINE: { /* define declarations */
      node_ptr def;
      def = car(car(iter));
      if ( Nil != def) {
        fprintf(ostream, "DEFINE\n");
        while (Nil != def) { /* iterate over define declarations */

          nusmv_assert(CONS == node_get_type(def));
          nusmv_assert(EQDEF == node_get_type(car(def)));

          fprintf(ostream, "   ");
          print_node(wffprint, ostream, car(def));
          fprintf(ostream, ";\n");

          def = cdr(def);
        }
      } /* if */
      break;
    }

    case INIT: /* INIT  declarations */
      fprintf(ostream, "INIT\n   ");
      print_node(wffprint, ostream, car(car(iter)));
      fprintf(ostream, "\n");
      break;

    case TRANS: /* TRANS  declarations */
      fprintf(ostream, "TRANS\n   ");
      print_node(wffprint, ostream, car(car(iter)));
      fprintf(ostream, "\n");
      break;

    case JUSTICE: /* JUSTICE  declarations */
      fprintf(ostream, "JUSTICE\n   ");
      print_node(wffprint, ostream, car(car(iter)));
      fprintf(ostream, "\n");
      break;
    default: error_unreachable_code(); /* unexpected node */
    } /*switch */

    iter = cdr(iter);
  } /* while */

  return;
}
#endif
