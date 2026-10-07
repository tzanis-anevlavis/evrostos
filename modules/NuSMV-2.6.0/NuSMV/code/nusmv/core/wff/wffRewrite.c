/* ---------------------------------------------------------------------------


  This file is part of the ``wff'' package.
  %COPYRIGHT%

-----------------------------------------------------------------------------*/

/*!
  \author Michele Dorigatti, Marco Roveri
  \brief This module rewrites a formula in such a way it can be processed
  by all the algorithms in the system

  \todo: Missing description

*/


#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/wff/wffRewrite.h"
#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/wff/wff.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/
/*!
  \brief Encapsulate the information needed for the rewriting


*/

typedef struct WffRewriter_TAG
{
  NuSMVEnv_ptr env;
  WffRewriteMethod method;
  WffRewriterExpectedProperty eproptype;
  SymbLayer_ptr layer;
  FlatHierarchy_ptr outfh;
  short int spec_type;
  NodeList_ptr new_var_exprs;
  boolean initialize_monitor_to_true;
} WffRewriter;

typedef enum {
  WFF_REWRITER_PROPOSITIONAL,
  WFF_REWRITER_PURE_INVAR,
  WFF_REWRITER_PROP_IMP_INVAR,
  WFF_REWRITER_INVAR_IMP_INVAR,
  WFF_REWRITER_NONE } WffRewriter_InvariantKind;

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define WFF_MONITOR_PREFIX "__WFF_MONITOR_"


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static Pair_ptr _wff_rewrite_extract_next_input_predicates(WffRewriter* rewriter,
                                                           node_ptr spec);

static WffRewriteFormulaKind _wff_rewrite_input(WffRewriter* rewriter,
                                                node_ptr* spec);

static node_ptr _wff_rewrite_create_substitution(WffRewriter* rewriter,
                                                 node_ptr spec);

static void _wff_rewrite_fill_layer_and_hierarchy(WffRewriter* rewriter);


static boolean _wff_rewrite_is_rewriting_needed(SymbTable_ptr st,
                                                node_ptr wff,
                                                node_ptr context,
                                                WffRewriterExpectedProperty eproptype);

static Pair_ptr _wff_rewrite_generalized_property(const NuSMVEnv_ptr env,
                                                  const WffRewriteMethod method,
                                                  const WffRewriterExpectedProperty eproptype,
                                                  SymbLayer_ptr layer,
                                                  FlatHierarchy_ptr outfh,
                                                  const node_ptr spec,
                                                  const short int spec_type,
                                                  const boolean initialize_monitor_to_true);


static Pair_ptr _wff_rewrite_ltl2invar(const NuSMVEnv_ptr env,
                                       const WffRewriteMethod method,
                                       const WffRewriterExpectedProperty eproptype,
                                       SymbLayer_ptr layer,
                                       FlatHierarchy_ptr outfh,
                                       const node_ptr spec,
                                       const short int spec_type,
                                       const boolean initialize_monitor_to_true,
                                       const boolean ltl2invar_negate_property);

static WffRewriter_InvariantKind _wff_invariant_kind(const SymbTable_ptr st,
                                                     node_ptr spec, node_ptr context,
                                                     WffRewriter_InvariantKind outer);


static Pair_ptr _wff_invariant_get_members(const SymbTable_ptr st,
                                           node_ptr spec, node_ptr context);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

Pair_ptr Wff_Rewrite_rewrite_formula(const NuSMVEnv_ptr env,
                                     const WffRewriteMethod method,
                                     const WffRewriterExpectedProperty eproptype,
                                     SymbLayer_ptr layer,
                                     FlatHierarchy_ptr outfh,
                                     const node_ptr spec,
                                     const short int spec_type)
{
  return Wff_Rewrite_rewrite_formula_generic(env, method, eproptype, layer,
                                             outfh, spec, spec_type, true, false);
}

Pair_ptr Wff_Rewrite_rewrite_formula_generic(const NuSMVEnv_ptr env,
                                             const WffRewriteMethod method,
                                             const WffRewriterExpectedProperty eproptype,
                                             SymbLayer_ptr layer,
                                             FlatHierarchy_ptr outfh,
                                             const node_ptr spec,
                                             const short int spec_type,
                                             const boolean initialize_monitor_to_true,
                                             const boolean ltl2invar_negate_property)
{
  Pair_ptr retval = NULL;
  SymbTable_ptr st = NULL;

  FLAT_HIERARCHY_CHECK_INSTANCE(outfh);
  SYMB_LAYER_CHECK_INSTANCE(layer);

  switch(spec_type) {
  case SPEC: break;
  case LTLSPEC: break;
  case INVARSPEC: break;
  case PSLSPEC: break;
  case COMPUTE: break;
  default:
    error_unreachable_code_msg("Unrecognized specification type");
  }

  if (Nil == spec) return PAIR(NULL);

  st = FlatHierarchy_get_symb_table(outfh);

  /* Maybe there is nothing to do */
  if (_wff_rewrite_is_rewriting_needed(st, spec, Nil, eproptype)) {
    switch(eproptype) {
    case WFF_REWRITER_REWRITE_INPUT_NEXT:
      retval = _wff_rewrite_generalized_property(env, method, eproptype, layer, outfh, spec,
                                                 spec_type, initialize_monitor_to_true);
      break;
    case WFF_REWRITER_LTL_2_INVAR:
      retval = _wff_rewrite_ltl2invar(env, method, eproptype, layer, outfh, spec,
                                      spec_type, initialize_monitor_to_true,
                                      ltl2invar_negate_property);
      break;
    default:
      error_unreachable_code_msg("Unrecognized rewriting of expected input property");
    }
  }
  else {
    /* No rewriting is needed */
    NodeMgr_ptr const nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    retval = Pair_create(new_node(nodemgr, node_get_type(spec), car(spec), cdr(spec)),
                         VOIDPTR_FROM_INT(spec_type));
  }

  nusmv_assert(!Wff_Rewrite_is_rewriting_needed(st,
                                                NODE_PTR(Pair_get_first(retval)),
                                                Nil));
  // nusmv_assert(! _wff_rewrite_is_rewriting_needed(st, retval, Nil, eproptype));

  return retval;
}

boolean Wff_Rewrite_is_rewriting_needed(SymbTable_ptr st,
                                        node_ptr wff,
                                        node_ptr context)
{
  Set_t cone = NULL;
  boolean result = false;

  wff = Compile_FlattenSexpExpandDefine(st, wff, context);

  cone = Formula_GetDependenciesByType(st,
                                       wff,
                                       Nil,
                                       VFT_NEXT | VFT_INPUT,
                                       true);

  /* If there are next or input then return true */
  result = !Set_IsEmpty(cone);
  Set_ReleaseSet(cone);

  return result;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
/*!
  \brief Rewrites the formula and collects the map monitor ->
  predicate

  next/input predicates are substituted and collected
*/

static Pair_ptr _wff_rewrite_extract_next_input_predicates(WffRewriter* rewriter,
                                                           node_ptr spec)
{
  NodeMgr_ptr const nodemgr =
     NODE_MGR(NuSMVEnv_get_value(rewriter->env, ENV_NODE_MGR));
  WffRewriteFormulaKind formula_kind = WFF_REWRITE_FORMULA_KIND_FIRST;
  NodeList_ptr new_var_exprs = NodeList_create();
  Pair_ptr retval = NULL;
  SymbTable_ptr symb_table = NULL;

  symb_table = FlatHierarchy_get_symb_table(rewriter->outfh);
  spec = Compile_FlattenSexpExpandDefine(symb_table, spec, Nil);
  rewriter->new_var_exprs = new_var_exprs;

  if (INVARSPEC == rewriter->spec_type) {
    node_ptr formula_to_free = Nil;
    /* convert temporary to ltl to process it with the same function */
    spec = new_node(nodemgr, OP_GLOBAL, spec, Nil);
    formula_kind = _wff_rewrite_input(rewriter, &spec);
    formula_to_free = spec;
    spec = car(spec);
    free_node(nodemgr, formula_to_free);
  }
  else {
    nusmv_assert(LTLSPEC == rewriter->spec_type);

    formula_kind = _wff_rewrite_input(rewriter, &spec);
  }

  /* It is still possible to have next or input vars if they were at top
     level. Example:
  LTLSPEC next(a) */

  /* the bitwise operator is wanted. We have do something if the formula has
     still input vars and/or next operators */
  if (WFF_REWRITE_FORMULA_KIND_INPUT_NEXT & formula_kind) {
    spec = _wff_rewrite_create_substitution(rewriter, spec);
  }

  retval = Pair_create(spec, new_var_exprs);
  rewriter->new_var_exprs = new_var_exprs;

  return retval;
}

/*!
  \brief Recursively walk over the expressions and returns
  the kind of the expression, i.e. if it is temporal or with input vars or next
  operators

  A copy of the provided expression is created and
  returned in the same pointer "expr". The copy may be exact or
  already rewritten (to remove inputs or next operator in temporal
  expressions).

  "new_var_exprs" is a list of Pairs of a new state var
  introduced during rewriting and an expression associated with that
  state variable.

  Precondition: the expression have to be correctly typed.

  NOTE FOR DEVELOPERS: This function creates new expression using the
  same approach as flattener_core_flatten, i.e. consts and ids are
  find_atom-ed and operations are new_node-ed. Both functions should be
  changed synchronously.

  \sa PropRewriteFormulaKind
*/

static WffRewriteFormulaKind _wff_rewrite_input(WffRewriter* rewriter,
                                                node_ptr* expr)
{
  NodeMgr_ptr const nodemgr = NODE_MGR(NuSMVEnv_get_value(rewriter->env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(rewriter->env, ENV_ERROR_MANAGER));
  OptsHandler_ptr const opts =
    OPTS_HANDLER(NuSMVEnv_get_value(rewriter->env, ENV_OPTS_HANDLER));
  Logger_ptr const logger = LOGGER(NuSMVEnv_get_value(rewriter->env, ENV_LOGGER));
  MasterPrinter_ptr const wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(rewriter->env, ENV_WFF_PRINTER));
  SymbTable_ptr symb_table = NULL;

  WffRewriteFormulaKind retval = WFF_REWRITE_FORMULA_KIND_FIRST;
  WffRewriteFormulaKind kind1 = WFF_REWRITE_FORMULA_KIND_FIRST;
  WffRewriteFormulaKind kind2 = WFF_REWRITE_FORMULA_KIND_FIRST;
  node_ptr expr1 = Nil;
  node_ptr expr2 = Nil;

  symb_table = FlatHierarchy_get_symb_table(rewriter->outfh);

#ifdef DEBUG_WFF_REWRITE
  {
    Logger_ptr const logger = LOGGER(NuSMVEnv_get_value(rewriter->env, ENV_LOGGER));
    MasterPrinter_ptr const sexpprint =
      MASTER_PRINTER(NuSMVEnv_get_value(rewriter->env, ENV_SEXP_PRINTER));
    OptsHandler_ptr const opts =
      OPTS_HANDLER(NuSMVEnv_get_value(rewriter->env, ENV_OPTS_HANDLER));

    Logger_inc_indent_size(logger);
    Logger_vnlog_error(logger, sexpprint, opts, "Input: \n%N\n", *expr);
  }
#endif

  if (Nil == *expr) {
    retval = WFF_REWRITE_FORMULA_KIND_STATE;
  }
  else {
    switch (node_get_type(*expr)) {
      /* --- constants ---
         the expression is already find_atom => no need to create a copy */
    case FAILURE:  case TRUEEXP:  case FALSEEXP:
    case NUMBER:  case NUMBER_UNSIGNED_WORD:  case NUMBER_SIGNED_WORD:
    case UWCONST: case SWCONST:
    case NUMBER_FRAC:  case NUMBER_REAL:  case NUMBER_EXP:
      retval =  WFF_REWRITE_FORMULA_KIND_STATE;
      break;

      /* In this case we do not need to extend the language */
    case WSIZEOF: case CAST_TOINT:
      retval=  WFF_REWRITE_FORMULA_KIND_STATE;
      break;

      /* can contain an identifier or a next */
    case CONTEXT:
      expr2 = cdr(*expr);
      kind2 = _wff_rewrite_input(rewriter, &expr2);

      *expr = new_node(nodemgr, node_get_type(*expr), car(*expr), expr2);
      retval = kind2;
      break;

      /* --- identifier ---
         since the expression is already flattened there is not need
         to resolve the identifier, find_atom it or create a copy. */
    case ATOM:  case DOT:  case ARRAY:
      {
        if (SymbTable_is_symbol_input_var(symb_table, *expr)) {
          retval = WFF_REWRITE_FORMULA_KIND_INPUT;
          break;
        }

        if (SymbTable_is_symbol_state_var(symb_table, *expr) ||
            SymbTable_is_symbol_constant(symb_table, *expr) ||
            SymbTable_is_symbol_frozen_var(symb_table, *expr)) {
          retval = WFF_REWRITE_FORMULA_KIND_STATE;
          break;
        }

        Logger_vnlog_error(logger, wffprint, opts, "Internal error: Unknown identifier %N is met during PROP REWRITE\n",
                           *expr);
        ErrorMgr_nusmv_exit(errmgr, 1);
      }

    case NFUNCTION:
      /* No need to look at the name of the nfunction */
      expr2 = cdr(*expr);
      kind2 = _wff_rewrite_input(rewriter, &expr2);

      nusmv_assert(NULL != car(*expr));

      *expr = new_node(nodemgr, node_get_type(*expr), car(*expr), expr2);
      retval = kind2;
      break;

      /* --- next --- */
    case NEXT:
      *expr = new_node(nodemgr, node_get_type(*expr), car(*expr), Nil);
      retval = WFF_REWRITE_FORMULA_KIND_NEXT;
      break;

      /* --- unary non-temporal operator --- */
    case NOT:
    case UMINUS:
    case FLOOR:
      expr1 = car(*expr);
      kind1 = _wff_rewrite_input(rewriter, &expr1);
      nusmv_assert(Nil == cdr(*expr));

      *expr = new_node(nodemgr, node_get_type(*expr), expr1, Nil);
      retval =  kind1;
      break;

      /* --- binary non-temporal operators ---
         if one operand has input and other has a temporal op =>
         rewrite operand with input var and return temporal kind.
         If kinds are different "input" and "temporal" kinds wins
         "state".*/
    case TWODOTS: /* This is dealt as a binary operator */
    case AND: case OR: case IMPLIES: case IFF: case XOR: case XNOR:
      {
        expr1 = car(*expr);
        expr2 = cdr(*expr);
        kind1 = _wff_rewrite_input(rewriter, &expr1);
        kind2 = _wff_rewrite_input(rewriter, &expr2);

        /* if kind1 is input, next or both */
        if ((WFF_REWRITE_FORMULA_KIND_INPUT_NEXT & kind1)
            && WFF_REWRITE_FORMULA_KIND_TEMP == kind2) {
          expr1 = _wff_rewrite_create_substitution(rewriter, expr1);
          kind1 = WFF_REWRITE_FORMULA_KIND_STATE;
        }
        else if ((WFF_REWRITE_FORMULA_KIND_INPUT_NEXT & kind2)
                 && WFF_REWRITE_FORMULA_KIND_TEMP == kind1) {
          expr2 = _wff_rewrite_create_substitution(rewriter, expr2);
          kind2 = WFF_REWRITE_FORMULA_KIND_STATE;
        }

        *expr = new_node(nodemgr, node_get_type(*expr), expr1, expr2);

        if (kind1 == kind2) return kind1;
        else if (WFF_REWRITE_FORMULA_KIND_TEMP == kind1 ||
                 WFF_REWRITE_FORMULA_KIND_TEMP == kind2) {
          return WFF_REWRITE_FORMULA_KIND_TEMP;
        }
        else if ((WFF_REWRITE_FORMULA_KIND_INPUT_NEXT & kind1) ||
                 (WFF_REWRITE_FORMULA_KIND_INPUT_NEXT & kind2)) {
          return (kind1 | kind2) & WFF_REWRITE_FORMULA_KIND_INPUT_NEXT;
        }
        else error_unreachable_code();
      }

    case WRESIZE:
      expr1 = car(*expr);
      expr2 = cdr(*expr);
      kind2 = _wff_rewrite_input(rewriter, &expr1);
      nusmv_assert(WFF_REWRITE_FORMULA_KIND_TEMP != kind2);

      *expr = new_node(nodemgr, node_get_type(*expr), expr1, expr2);

      retval = kind2;
      break;

      /* --- binary non-temporal operators ---
         it is exactly as previous case but the operands cannot have temporal
         operators. It is written as a special case only for debugging purposes.*/
    case CONS:
    case WAREAD: case WAWRITE:
    case CASE: case COLON:
    case EQUAL: case NOTEQUAL:
    case LT: case GT: case LE: case GE:
    case PLUS: case MINUS: case TIMES: case MOD: case DIVIDE:
    case UNION: case SETIN:
    case LSHIFT: case RSHIFT:
    case BIT: case CONCATENATION: case BIT_SELECTION:  case EXTEND:
    case CAST_BOOL:  case CAST_WORD1:  case CAST_SIGNED: case CAST_UNSIGNED:
    case IFTHENELSE:
      expr1 = car(*expr);
      expr2 = cdr(*expr);
      kind1 = _wff_rewrite_input(rewriter, &expr1);
      kind2 = _wff_rewrite_input(rewriter, &expr2);
      nusmv_assert(WFF_REWRITE_FORMULA_KIND_TEMP != kind1 &&
                   WFF_REWRITE_FORMULA_KIND_TEMP != kind2);

      *expr = new_node(nodemgr, node_get_type(*expr), expr1, expr2);

      if ((WFF_REWRITE_FORMULA_KIND_INPUT_NEXT & kind1) ||
          (WFF_REWRITE_FORMULA_KIND_INPUT_NEXT & kind2)) {
        retval = (kind1 | kind2) & WFF_REWRITE_FORMULA_KIND_INPUT_NEXT;
        break;
      }
      else {
        retval = WFF_REWRITE_FORMULA_KIND_STATE;
        break;
      }

      /*  -- unary temporal operators ---
          if operand has inputs then rewrite it. */
    case OP_NEXT: case OP_PREC: case OP_NOTPRECNOT: case OP_FUTURE:
    case OP_ONCE: case OP_GLOBAL: case OP_HISTORICAL:
      expr1 = car(*expr);
      kind1 = _wff_rewrite_input(rewriter, &expr1);
      nusmv_assert(Nil == cdr(*expr));

      if (WFF_REWRITE_FORMULA_KIND_INPUT_NEXT & kind1) {
        expr1 = _wff_rewrite_create_substitution(rewriter, expr1);
      }

      *expr = new_node(nodemgr, node_get_type(*expr), expr1, Nil);

      retval =  WFF_REWRITE_FORMULA_KIND_TEMP;
      break;

      /* --- binary temporal operators ---
         If any operand has inputs then rewrite it.*/
    case UNTIL: case SINCE: case RELEASES: case TRIGGERED:
      expr1 = car(*expr);
      expr2 = cdr(*expr);
      kind1 = _wff_rewrite_input(rewriter, &expr1);
      kind2 = _wff_rewrite_input(rewriter, &expr2);

      if (WFF_REWRITE_FORMULA_KIND_INPUT_NEXT & kind1) {
        expr1 = _wff_rewrite_create_substitution(rewriter, expr1);
      }
      if (WFF_REWRITE_FORMULA_KIND_INPUT_NEXT & kind2) {
        expr2 = _wff_rewrite_create_substitution(rewriter, expr2);
      }
      *expr = new_node(nodemgr, node_get_type(*expr), expr1, expr2);

      retval =  WFF_REWRITE_FORMULA_KIND_TEMP;
      break;

    default:
      {
        StreamMgr_ptr const streams =
          STREAM_MGR(NuSMVEnv_get_value(rewriter->env, ENV_STREAM_MANAGER));

        StreamMgr_print_error(streams, "Error: %s:%d:%s: unexpected operator of type %d\n",
                              __FILE__, __LINE__, __func__, node_get_type(*expr));
        ErrorMgr_nusmv_exit(errmgr, 1);
      }
    } /* switch */
  }

#ifdef DEBUG_WFF_REWRITE
  {
    Logger_ptr const logger = LOGGER(NuSMVEnv_get_value(rewriter->env, ENV_LOGGER));
    MasterPrinter_ptr const sexpprint =
      MASTER_PRINTER(NuSMVEnv_get_value(rewriter->env, ENV_SEXP_PRINTER));
    OptsHandler_ptr const opts =
      OPTS_HANDLER(NuSMVEnv_get_value(rewriter->env, ENV_OPTS_HANDLER));

    Logger_vnlog_error(logger, sexpprint, opts, "Output: \n%N\n", *expr);
    Logger_dec_indent_size(logger);
  }
#endif

  return retval;
}

/*!
  \brief Creates a new state variable and add a pair <var id, expr>
  to the list "new_var_exprs"

  The purpose of the function is to create a substitution
  for the given expression in an LTL formula.

  The function returns:
  if rewrite_type is
      WFF_REWRITE_METHOD_STANDARD : new identifiers.
      WFF_REWRITE_METHOD_DEADLOCK_FREE: "X new_identifier"

  [AT] Optimization
  The input expression comes from flattener_core_flatten
  (e.g.  through Compile_FlattenSexpExpandDefine). That function, currently,
  does not applies find_node to the operation node.  If, in future, it does
  then it will be possible to check here if a given expression has already
  associated var and do not create a new one
*/

static node_ptr _wff_rewrite_create_substitution(WffRewriter* rewriter,
                                                 node_ptr spec)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(rewriter->env, ENV_NODE_MGR));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(rewriter->env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(rewriter->env, ENV_WFF_PRINTER));

  node_ptr new_var;
  node_ptr result = Nil;
  SymbTable_ptr symb_table = NULL;

  symb_table = FlatHierarchy_get_symb_table(rewriter->outfh);

  /* declare a new variable */
  new_var = SymbTable_get_fresh_symbol_name(symb_table, WFF_MONITOR_PREFIX);

  if (opt_verbose_level_gt(opts, 2)) { /* debug info */
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(rewriter->env, ENV_LOGGER));
    Logger_nlog(logger, wffprint, "%s: Associating %N to %N\n", __func__,
                new_var, spec);
  }

  NodeList_append(rewriter->new_var_exprs, NODE_PTR(Pair_create(new_var, spec)));

  switch(rewriter->method) {
  case WFF_REWRITE_METHOD_STANDARD:
    result = new_var;
    break;

  case WFF_REWRITE_METHOD_DEADLOCK_FREE:
    if (INVARSPEC == rewriter->spec_type) {
      result = new_var;
    }
    else {
      result = new_node(nodemgr, OP_NEXT, new_var, Nil);
    }

    break;

  default: error_unreachable_code();
  }

  return result;
}

/*!
  \brief Given the map monitor -> predicate, builds the
  corresponding flat hierarchy

  Given the map monitor -> predicate, builds the
  corresponding flat hierarchy
*/

static void _wff_rewrite_fill_layer_and_hierarchy(WffRewriter* rewriter)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(rewriter->env, ENV_EXPR_MANAGER));
  SymbTable_ptr symb_table = NULL;
  ListIter_ptr iter;

  symb_table = FlatHierarchy_get_symb_table(rewriter->outfh);

  /* declare the new variables and create hierarchy expressions */
  NODE_LIST_FOREACH(rewriter->new_var_exprs, iter) {
    SymbType_ptr bool_type;
    Pair_ptr tmp = PAIR(NodeList_get_elem_at(rewriter->new_var_exprs, iter));
    node_ptr var = NODE_PTR(Pair_get_first(tmp));
    node_ptr expr = NODE_PTR(Pair_get_second(tmp));

    FlatHierarchy_add_var(rewriter->outfh, var);
    bool_type = SymbType_create(rewriter->env, SYMB_TYPE_BOOLEAN, Nil);
    SymbLayer_declare_state_var(rewriter->layer, var, bool_type);

    switch(rewriter->method) {
    case WFF_REWRITE_METHOD_STANDARD:
      FlatHierarchy_add_trans(rewriter->outfh,
                              ExprMgr_iff(exprs, var, expr));
      break;

    case WFF_REWRITE_METHOD_DEADLOCK_FREE:
      if (rewriter->initialize_monitor_to_true) {
        FlatHierarchy_add_init(rewriter->outfh, var);
      }
      else {
        FlatHierarchy_add_init(rewriter->outfh, ExprMgr_not(exprs, var));
      }
      FlatHierarchy_add_trans(rewriter->outfh,
                              ExprMgr_iff(exprs,
                                          ExprMgr_next(exprs, var, symb_table),
                                          expr));
      break;

    default: error_unreachable_code();
    }
  }
}


static Pair_ptr _wff_rewrite_generalized_property(const NuSMVEnv_ptr env,
                                                  const WffRewriteMethod method,
                                                  const WffRewriterExpectedProperty eproptype,
                                                  SymbLayer_ptr layer,
                                                  FlatHierarchy_ptr outfh,
                                                  node_ptr const spec,
                                                  const short int spec_type,
                                                  const boolean initialize_monitor_to_true)
{
  NodeList_ptr new_var_exprs = NULL;
  Pair_ptr retval = NULL;
  WffRewriter rewriter;
  SymbTable_ptr symb_table = FlatHierarchy_get_symb_table(outfh);
  Pair_ptr pair = NULL;

  rewriter.env = env;
  rewriter.outfh = outfh;
  rewriter.layer = layer;
  rewriter.method = method;
  rewriter.eproptype = eproptype;
  rewriter.spec_type = spec_type;
  rewriter.new_var_exprs = NULL;
  rewriter.initialize_monitor_to_true = initialize_monitor_to_true;

  nusmv_assert(SymbTable_has_layer(symb_table, SymbLayer_get_name(layer)));

  pair = _wff_rewrite_extract_next_input_predicates(&rewriter, spec);
  rewriter.new_var_exprs = NODE_LIST(Pair_get_second(pair));

  _wff_rewrite_fill_layer_and_hierarchy(&rewriter);

  {
    ListIter_ptr iter;

    /* cleaning */
    NODE_LIST_FOREACH(rewriter.new_var_exprs, iter) {
      Pair_ptr elem = PAIR(NodeList_get_elem_at(rewriter.new_var_exprs, iter));
      Pair_destroy(elem);
    }
  }

  NodeList_destroy(rewriter.new_var_exprs); rewriter.new_var_exprs = NULL;

  retval = Pair_create(Pair_get_first(pair), VOIDPTR_FROM_INT(spec_type));

  Pair_destroy(pair); pair = NULL;

  return retval;
}


static boolean _wff_rewrite_is_rewriting_needed(SymbTable_ptr st,
                                                node_ptr wff,
                                                node_ptr context,
                                                WffRewriterExpectedProperty eproptype)
{
  boolean result = false;

  switch(eproptype) {
  case WFF_REWRITER_REWRITE_INPUT_NEXT:
    {
      Set_t cone = NULL;

      wff = Compile_FlattenSexpExpandDefine(st, wff, context);

      cone = Formula_GetDependenciesByType(st,
                                           wff,
                                           Nil,
                                           VFT_NEXT | VFT_INPUT,
                                           true);

      /* If there are next or input then return true */
      result = !Set_IsEmpty(cone);
      Set_ReleaseSet(cone);
    }
    break;
  case WFF_REWRITER_LTL_2_INVAR:
    result = true;
    break;
  default:
    error_unreachable_code_msg("Unrecognized rewriting of expected input property");
    break;
  }

  return result;
}


static Pair_ptr
_wff_rewrite_ltl2invar(const NuSMVEnv_ptr env,
                       const WffRewriteMethod method,
                       const WffRewriterExpectedProperty eproptype,
                       SymbLayer_ptr layer,
                       FlatHierarchy_ptr outfh,
                       const node_ptr spec,
                       const short int spec_type,
                       const boolean initialize_monitor_to_true,
                       const boolean ltl2invar_negate_property)
{

  if (INVARSPEC == spec_type) {
    return _wff_rewrite_generalized_property(env, method, eproptype, layer, outfh,
                                             spec, spec_type, initialize_monitor_to_true);
  }
  else {
    WffRewriter_InvariantKind kind;
    SymbTable_ptr st = FlatHierarchy_get_symb_table(outfh);
    Pair_ptr retval = NULL;

    nusmv_assert(LTLSPEC == spec_type);

    kind = _wff_invariant_kind(st, spec, Nil, WFF_REWRITER_NONE);

    if (WFF_REWRITER_NONE == kind || WFF_REWRITER_PROPOSITIONAL == kind) {
      /* No rewriting is needed */
      const NodeMgr_ptr nodemgr =
        NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

      return Pair_create(new_node(nodemgr, node_get_type(spec),
                                  car(spec), cdr(spec)), VOIDPTR_FROM_INT(spec_type));
    }
    /* It is needed to perform some transformation */
    {
      node_ptr flatten_spec = Compile_FlattenSexpExpandDefine(st, spec, Nil);
      Pair_ptr prop_pair = _wff_invariant_get_members(st, flatten_spec, Nil);

      if (WFF_REWRITER_PURE_INVAR == kind) {
        const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
        node_ptr invariant = NODE_PTR(Pair_get_second(prop_pair));

        if (ltl2invar_negate_property) {
          invariant = ExprMgr_not(exprs, invariant);
        }
        nusmv_assert(Nil == NODE_PTR(Pair_get_first(prop_pair)));
        retval = _wff_rewrite_generalized_property(env, method, eproptype, layer,
                                                   outfh, invariant, INVARSPEC,
                                                   initialize_monitor_to_true);
      }
      else if (WFF_REWRITER_PROP_IMP_INVAR == kind) {
        const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
        node_ptr premises = NODE_PTR(Pair_get_first(prop_pair));
        node_ptr invariant = NODE_PTR(Pair_get_second(prop_pair));
        FlatHierarchy_ptr premises_fh = FlatHierarchy_create(st);

        if (ltl2invar_negate_property) {
          invariant = ExprMgr_not(exprs, invariant);
        }

        if (Wff_Rewrite_is_rewriting_needed(st, premises, Nil)) {
          /* The property contains input/next thus it must be added as
             a TRANS, but conditioned to be happening only once.
             VAR _monitor_ : boolean;
             INIT _monitor_;
             TRANS !next(_monitor_);
             TRANS _monitor_ -> premises;
          /* declare a new variable */
          const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
          node_ptr new_var = SymbTable_get_fresh_symbol_name(st, WFF_MONITOR_PREFIX);
          SymbType_ptr bool_type = SymbType_create(env, SYMB_TYPE_BOOLEAN, Nil);

          FlatHierarchy_add_var(premises_fh, new_var);
          SymbLayer_declare_state_var(layer, new_var, bool_type);
          FlatHierarchy_add_init(premises_fh, new_var);
          FlatHierarchy_add_trans(premises_fh, ExprMgr_and(exprs,
            ExprMgr_not(exprs, ExprMgr_next(exprs, new_var, st)),
            ExprMgr_implies(exprs, new_var, premises)));
        }
        else {
          /* The property does NOT contains input/next thus it must be added as an INIT */
          FlatHierarchy_add_init(premises_fh, premises);
        }
        if (Wff_Rewrite_is_rewriting_needed(st, invariant, Nil)) {
          retval = _wff_rewrite_generalized_property(env, method,
                                                     WFF_REWRITER_REWRITE_INPUT_NEXT,
                                                     layer, outfh, invariant,
                                                     INVARSPEC,
                                                     initialize_monitor_to_true);
        }
        else {
          const NodeMgr_ptr nodemgr =
            NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
          retval = Pair_create(invariant, VOIDPTR_FROM_INT(INVARSPEC));
        }
        FlatHierarchy_mergeinto(outfh, premises_fh);
        FlatHierarchy_destroy(premises_fh);
      }
      else if (WFF_REWRITER_INVAR_IMP_INVAR == kind) {
        const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
        node_ptr premises = NODE_PTR(Pair_get_first(prop_pair));
        node_ptr invariant = NODE_PTR(Pair_get_second(prop_pair));
        FlatHierarchy_ptr premises_fh = FlatHierarchy_create(st);
        Set_t premises_cone = Formula_GetDependenciesByType(st,
                                                            premises,
                                                            Nil,
                                                            VFT_NEXT | VFT_INPUT,
                                                            true);

        if (ltl2invar_negate_property) {
          invariant = ExprMgr_not(exprs, invariant);
        }

        if (!Set_IsEmpty(premises_cone)) {
          /* The property contains input/next thus it must be added as TRANS */
          FlatHierarchy_add_trans(premises_fh, premises);
        }
        else {
          /* The property does NOT contains input/next thus it must be
             added as an INVAR, this ensures it holds initially and
             within the transition relation. */
          FlatHierarchy_add_invar(premises_fh, premises);
        }
        if (Wff_Rewrite_is_rewriting_needed(st, invariant, Nil)) {
          retval = _wff_rewrite_generalized_property(env, method,
                                                     WFF_REWRITER_REWRITE_INPUT_NEXT,
                                                     layer, outfh, invariant,
                                                     INVARSPEC,
                                                     initialize_monitor_to_true);
        }
        else {
          retval = Pair_create(invariant, VOIDPTR_FROM_INT(INVARSPEC));
        }
        FlatHierarchy_mergeinto(outfh, premises_fh);
        FlatHierarchy_destroy(premises_fh);
      }
      else {
        error_unreachable_code_msg("Unexpected expression kind");
      }
      Pair_destroy(prop_pair);
      return retval;
    }
  }

  nusmv_assert(false);
  return PAIR(NULL);
}




static WffRewriter_InvariantKind _wff_invariant_kind(const SymbTable_ptr st,
                                                     node_ptr spec, node_ptr context,
                                                     WffRewriter_InvariantKind outer)
{
  if (Wff_is_propositional(st, spec, context, true))
    return WFF_REWRITER_PROPOSITIONAL;

  switch (node_get_type(spec)) {
    case CONTEXT:
      return _wff_invariant_kind(st,  cdr(spec), car(spec), outer);
      break;

    case OP_GLOBAL:
      if (WFF_REWRITER_NONE == outer || WFF_REWRITER_PURE_INVAR == outer) {
        WffRewriter_InvariantKind result =
            _wff_invariant_kind(st,  car(spec), context,
                                WFF_REWRITER_PURE_INVAR);
        if (WFF_REWRITER_PROPOSITIONAL == result ||
            WFF_REWRITER_PURE_INVAR == result) {
          return WFF_REWRITER_PURE_INVAR;
        }
      }
      return WFF_REWRITER_NONE;
      break;

    case AND:
      if (WFF_REWRITER_NONE == outer || WFF_REWRITER_PURE_INVAR == outer) {
        WffRewriter_InvariantKind left;
        left = _wff_invariant_kind(st, car(spec), context, outer);

        if (WFF_REWRITER_PROPOSITIONAL == left ||
            WFF_REWRITER_PURE_INVAR == left) {
          WffRewriter_InvariantKind right;
          right = _wff_invariant_kind(st,  cdr(spec), context, outer);

          if (WFF_REWRITER_PROPOSITIONAL == left &&
              WFF_REWRITER_PROPOSITIONAL == right &&
              WFF_REWRITER_PURE_INVAR == outer)
            return WFF_REWRITER_PURE_INVAR;

          if (WFF_REWRITER_PROPOSITIONAL == left &&
              WFF_REWRITER_PURE_INVAR == right &&
              WFF_REWRITER_PURE_INVAR == outer)
            return WFF_REWRITER_PURE_INVAR;

          if (WFF_REWRITER_PURE_INVAR == left &&
              WFF_REWRITER_PROPOSITIONAL == right &&
              WFF_REWRITER_PURE_INVAR == outer)
            return WFF_REWRITER_PURE_INVAR;

          if (WFF_REWRITER_PURE_INVAR == left &&
              WFF_REWRITER_PURE_INVAR == right)
            return WFF_REWRITER_PURE_INVAR;
        }
      }

      return WFF_REWRITER_NONE;
      break;

    case IMPLIES:
      if (WFF_REWRITER_NONE == outer) {
        WffRewriter_InvariantKind left, right;
        left = _wff_invariant_kind(st,  car(spec), context, outer);

        if (WFF_REWRITER_PROPOSITIONAL == left ||
            WFF_REWRITER_PURE_INVAR == left) {
          right = _wff_invariant_kind(st,  cdr(spec), context, outer);

          if (WFF_REWRITER_PROPOSITIONAL == left &&
              WFF_REWRITER_PROPOSITIONAL == right)
            return WFF_REWRITER_PROPOSITIONAL;

          if (WFF_REWRITER_PROPOSITIONAL == left &&
              WFF_REWRITER_PURE_INVAR == right)
            return WFF_REWRITER_PROP_IMP_INVAR;

          /* We disregard (G p) -> q */
          if (WFF_REWRITER_PURE_INVAR == left &&
              WFF_REWRITER_PROPOSITIONAL == right)
            return WFF_REWRITER_NONE;

          if (WFF_REWRITER_PURE_INVAR == left &&
              WFF_REWRITER_PURE_INVAR == right)
            return WFF_REWRITER_INVAR_IMP_INVAR;
        }
      }

      return WFF_REWRITER_NONE;
      break;

    case OR:
      /* handles implication equivalences:
         (!p | G q), (!G p | G q), (G q | !p), (G q | !G p)

         (![G] p | q) is disregarded as done for implication.
         (This was done to fix issue 5178)
      */
      if (WFF_REWRITER_NONE == outer) {
        WffRewriter_InvariantKind left, right;

        if (NOT == node_get_type(car(spec))) {
          left = _wff_invariant_kind(st,  car(car(spec)), context, outer);
          if (WFF_REWRITER_PROPOSITIONAL == left ||
              WFF_REWRITER_PURE_INVAR == left) {
            right = _wff_invariant_kind(st,  cdr(spec), context, outer);

            if (WFF_REWRITER_PROPOSITIONAL == left &&
                WFF_REWRITER_PROPOSITIONAL == right)
              return WFF_REWRITER_PROPOSITIONAL;

            if (WFF_REWRITER_PROPOSITIONAL == left &&
                WFF_REWRITER_PURE_INVAR == right)
              return WFF_REWRITER_PROP_IMP_INVAR;

            /* We disregard !(G p) | q */
            if (WFF_REWRITER_PURE_INVAR == left &&
                WFF_REWRITER_PROPOSITIONAL == right)
              return WFF_REWRITER_NONE;

            if (WFF_REWRITER_PURE_INVAR == left &&
                WFF_REWRITER_PURE_INVAR == right)
              return WFF_REWRITER_INVAR_IMP_INVAR;
          }
        }
        else if (NOT == node_get_type(cdr(spec))) {
          right = _wff_invariant_kind(st,  car(cdr(spec)), context, outer);
          if (WFF_REWRITER_PROPOSITIONAL == right ||
              WFF_REWRITER_PURE_INVAR == right) {
            left = _wff_invariant_kind(st,  car(spec), context, outer);

            if (WFF_REWRITER_PROPOSITIONAL == right &&
                WFF_REWRITER_PROPOSITIONAL == left)
              return WFF_REWRITER_PROPOSITIONAL;

            if (WFF_REWRITER_PROPOSITIONAL == right &&
                WFF_REWRITER_PURE_INVAR == left)
              return WFF_REWRITER_PROP_IMP_INVAR;

            /* We disregard q | !(G p) */
            if (WFF_REWRITER_PURE_INVAR == right &&
                WFF_REWRITER_PROPOSITIONAL == left)
              return WFF_REWRITER_NONE;

            if (WFF_REWRITER_PURE_INVAR == right &&
                WFF_REWRITER_PURE_INVAR == left)
              return WFF_REWRITER_INVAR_IMP_INVAR;
          }
        }
      }
      return WFF_REWRITER_NONE;
      break;

    default:
      return WFF_REWRITER_NONE;
      break;
  }

  return WFF_REWRITER_NONE;
}


static Pair_ptr _wff_invariant_get_members(const SymbTable_ptr st,
                                           node_ptr spec, node_ptr context)
{
  if (Wff_is_propositional(st, spec, context, true)) {
    return Pair_create(Nil, spec);
  }

  switch (node_get_type(spec)) {
    case CONTEXT:
      error_unreachable_code_msg("Expression expected to be already flattened");
      return NULL;

    case OP_GLOBAL:
      return _wff_invariant_get_members(st, car(spec), context);

    case AND:
      {
        const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
        const NodeMgr_ptr nodemgr =
          NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
        Pair_ptr left = _wff_invariant_get_members(st, car(spec), context);
        Pair_ptr right = _wff_invariant_get_members(st, cdr(spec), context);
        Pair_ptr result = Pair_create(Nil,
                                      new_node(nodemgr, AND,
                                               NODE_PTR(Pair_get_second(left)),
                                               NODE_PTR(Pair_get_second(right))));
        Pair_destroy(left); Pair_destroy(right);
        return result;
      }

    case IMPLIES:
      {
        Pair_ptr left = _wff_invariant_get_members(st, car(spec), context);
        Pair_ptr right = _wff_invariant_get_members(st, cdr(spec), context);
        Pair_ptr result = Pair_create(Pair_get_second(left),
                                      Pair_get_second(right));
        Pair_destroy(left);
        Pair_destroy(right);
        return result;
      }

    case OR:
      {  /* this was added to fix issue 5178 */
        Pair_ptr left, right, result;

        /* If we are here, we can assume we have an implication-like
           disjunction in the forms:
           (!p | G q), (!G p | G q), (G q | !p), (G q | !G p) */
        if (NOT == node_get_type(car(spec))) {
          left = _wff_invariant_get_members(st, car(car(spec)), context);
          right = _wff_invariant_get_members(st, cdr(spec), context);
        }
        else {
          nusmv_assert(NOT == node_get_type(cdr(spec)));
          /* here cdr for left and car for right are used on purpose */
          left = _wff_invariant_get_members(st, car(cdr(spec)), context);
          right = _wff_invariant_get_members(st, car(spec), context);
        }

        result = Pair_create(Pair_get_second(left),
                             Pair_get_second(right));

        Pair_destroy(left);
        Pair_destroy(right);
        return result;
      }

    default:
      error_unreachable_code_msg("Unexpected expression structure");
      break;
  }

  error_unreachable_code_msg("Unexpected expression structure");
}
