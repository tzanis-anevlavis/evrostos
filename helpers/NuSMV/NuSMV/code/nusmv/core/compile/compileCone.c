/* ---------------------------------------------------------------------------


  This file is part of the ``compile'' package of NuSMV version 2.
  Copyright (C) 2000-2001 by FBK-irst.

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
  \author Marco Roveri and Marco Pistore
  \brief Computes the cone of influence of the model variables.

  This file contains the functions needed for computing
  the cone of influence (COI) of a given formula. The COI of all the
  variables in the model is pre-computed and cached the first time
  a cone of influence is required (function <code>initCoi</code>.
  Functions are also provided that compute the dependency variables
  for a formula, namely those variables that appear in the formula
  or in one of the definitions the formula depends on.

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/compile/compileInt.h"

#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/compile/dependency/FormulaDependency.h"

#include "nusmv/core/set/set.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/enc/enc.h"

/* TODO[MR]: to be removed once splitted in core/psl/... */
#include "nusmv/core/parser/psl/psl_symbols.h"
#include "nusmv/core/parser/psl/pslNode.h"

#include "nusmv/core/compile/symb_table/ResolveSymbol.h"
#include "nusmv/core/utils/Tuple5.h"


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/
/*!
  \brief Indicates that the dependency computation is ongoing.

  The value used during the building of dependencies of
   defined symbols to keep track that compuation is ongoing to discover
   circular definitions.
*/
#define BUILDING_DEP_SET (Set_t)-10

/*!
  \brief Indicates that the dependency is empty


*/
#define EMPTY_DEP_SET (Set_t)-11

/*!
  \brief Indicates that no dependency has been yet computed.


*/
#define NO_DEP_SET (Set_t)-12

/*!
  \brief True if the set is not NULL or equal the fake sets used as
                 placeholder


*/
#define IS_VALID_SET(set)                          \
  (EMPTY_DEP_SET != set &&                         \
   BUILDING_DEP_SET != set &&                      \
   NO_DEP_SET != set &&                            \
   (Set_t)NULL != set)

/*!
  \brief Indicates that the COI computation should be verbose.

  Indicates that the COI computation should be verbose.
*/
#define COI_VERBOSE (opt_verbose_level_gt(opts, 2))

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static Set_t
formulaGetDependenciesRecur(const SymbTable_ptr,
                            node_ptr, node_ptr,
                            SymbFilterType, boolean, int, hash_ptr);


static Set_t
formulaGetDefinitionDependencies(const SymbTable_ptr, node_ptr,
                                 SymbFilterType,
                                 boolean preserve_time, int time,
                                 hash_ptr);

static void
coiInit(const SymbTable_ptr symb_table, FlatHierarchy_ptr hierarchy);


static Set_t
formulaGetConstantsRecur(const SymbTable_ptr symb_table,
                         node_ptr formula, node_ptr context,
                         hash_ptr consts_hash);

#if 0
/* Disabled because no longer used */
static Set_t
get_array_cells(const SymbTable_ptr symb_table, node_ptr arr);

static void resolve_range(SymbTable_ptr st,
                          node_ptr range, node_ptr context,
                          int* low, int* high);
#endif

static Set_t
_coi_get_var_coi0(SymbTable_ptr st,
                  FlatHierarchy_ptr hierarchy,
                  node_ptr var,
                  boolean* nonassign,
                  boolean use_cache,
                  hash_ptr);

static Set_t
computeCoiVar(SymbTable_ptr st, FlatHierarchy_ptr fh, node_ptr var);

static assoc_retval coi0_hash_free(char *key, char *data, char * arg);

static assoc_retval consts_hash_free(char *key, char *data, char * arg);

static void mk_hash_key(node_ptr e, node_ptr c, SymbFilterType filter,
                        boolean preserve_time, int time, Tuple5_ptr key);

static assoc_retval dependencies_hash_free(char *key, char *data,
                                           char * arg);

static assoc_retval coi_hash_free(char *key, char *data, char * arg);

static hash_ptr compile_cone_get_handled_hash(SymbTable_ptr, char*);

static void insert_coi_hash(hash_ptr, node_ptr, Set_t);
static Set_t lookup_coi_hash(hash_ptr, node_ptr);

static void insert_coi0_hash(hash_ptr, node_ptr, Set_t);
static Set_t lookup_coi0_hash(hash_ptr, node_ptr);

static void insert_dependencies_hash(hash_ptr, node_ptr, Set_t);
static void close_define_dependencies_hash(hash_ptr, node_ptr, Set_t);
static Set_t lookup_dependencies_hash(hash_ptr, node_ptr);

static void insert_consts_hash(hash_ptr, node_ptr key, Set_t value);
static Set_t lookup_consts_hash(hash_ptr, node_ptr key);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

Set_t Formula_GetDependencies(const SymbTable_ptr symb_table,
                              node_ptr formula, node_ptr context)
{
  FormulaDependency_ptr dep =
    FORMULA_DEPENDENCY(NuSMVEnv_get_value(ENV_OBJECT_GET_ENV(symb_table),
                                          ENV_DEPENDENCY));

  return FormulaDependency_get_dependencies_by_type(dep,
                                                    symb_table,
                                                    formula,
                                                    context,
                                                    /* [MD] I believe it should
                                                       be: */
                                                    /* VFT_CNIF | VFT_FUNCTION, */
                                                    VFT_CNIF,
                                                    false);
}

Set_t
Formula_GetDependenciesByType(const SymbTable_ptr symb_table,
                              node_ptr formula, node_ptr context,
                              SymbFilterType filter,
                              boolean preserve_time)
{
  FormulaDependency_ptr dep =
    FORMULA_DEPENDENCY(NuSMVEnv_get_value(ENV_OBJECT_GET_ENV(symb_table),
                                          ENV_DEPENDENCY));


  return FormulaDependency_get_dependencies_by_type(dep,
                                                    symb_table,
                                                    formula,
                                                    context,
                                                    filter,
                                                    preserve_time);
}

Set_t Formulae_GetDependencies(const SymbTable_ptr symb_table,
                               node_ptr formula,
                               node_ptr justice, node_ptr compassion)
{
  FormulaDependency_ptr dep =
    FORMULA_DEPENDENCY(NuSMVEnv_get_value(ENV_OBJECT_GET_ENV(symb_table),
                                          ENV_DEPENDENCY));

  return
    FormulaDependency_formulae_get_dependencies(dep, symb_table,
                                                formula, justice, compassion);
}


Set_t Formulae_GetDependenciesByType(const SymbTable_ptr symb_table,
                                     node_ptr formula,
                                     node_ptr justice, node_ptr compassion,
                                     SymbFilterType filter, boolean preserve_time)
{
  FormulaDependency_ptr dep =
    FORMULA_DEPENDENCY(NuSMVEnv_get_value(ENV_OBJECT_GET_ENV(symb_table),
                                          ENV_DEPENDENCY));

  return
    FormulaDependency_formulae_get_dependencies_by_type(dep, symb_table,
                                                        formula, justice,
                                                        compassion, filter,
                                                        preserve_time);
}

Set_t Formula_GetConstants(const SymbTable_ptr symb_table,
                           node_ptr formula, node_ptr context)
{
  hash_ptr consts_hash =
    compile_cone_get_handled_hash(symb_table, ST_CONE_CONSTS_HASH);

  return formulaGetConstantsRecur(symb_table, formula, context, consts_hash);
}

Set_t ComputeCOIFixpoint(const SymbTable_ptr symb_table,
                         const FlatHierarchy_ptr hierarchy,
                         const Expr_ptr expression,
                         const int steps,
                         boolean* reached_fixpoint)
{
  Set_t deps;
  Set_t symbols_left;
  int i = 0;

  deps = Formula_GetDependenciesByType(symb_table, expression, Nil,
                                       VFT_CNIF | VFT_FUNCTION, false);

  if (steps == -1) {
    return ComputeCOI(symb_table, hierarchy, deps);
  }

  symbols_left = Set_Copy(deps);

  while ((i < steps) && (!Set_IsEmpty(symbols_left))) {
    Set_Iterator_t iter;
    Set_t tmp = Set_MakeEmpty();

    SET_FOREACH(symbols_left, iter) {
      Set_t base;
      node_ptr var = Set_GetMember(symbols_left, iter);
      boolean nonassign = false;

      /* Here we do no enable memoizing for cone at depth 0. The
         reason being that we are not guaranteed the hierarchy is the
         mainFlatHierarchy. If it is not, than possible momoized
         values may be wrong for this hierarchy */
      /* Until memoizing is not enabled there is no point to retrieve the
         hash */
      base = _coi_get_var_coi0(symb_table, hierarchy, var, &nonassign, false,
                               (hash_ptr)NULL);

      tmp = Set_Union(tmp, base);
      Set_ReleaseSet(base);
    }

    /* Remove from the freshly found variables the variables we
       already know  */
    Set_ReleaseSet(symbols_left);
    symbols_left = Set_Copy(tmp);
    symbols_left = Set_Difference(symbols_left, deps);
    deps = Set_Union(deps, tmp);
    Set_ReleaseSet(tmp);

    ++i;
  }

  if ((boolean*)NULL != reached_fixpoint) {
    if (Set_IsEmpty(symbols_left)) *reached_fixpoint = true;
    else *reached_fixpoint = false;
  }

  Set_ReleaseSet(symbols_left);

  return deps;
}

Set_t ComputeCOI(const SymbTable_ptr symb_table,
                 const FlatHierarchy_ptr hierarchy, Set_t base)
{
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  Set_t coi = Set_Copy(base);
  Set_Iterator_t iter;

  if (! cmp_struct_get_coi(cmps)) {
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Initializing Cone Of Influence...\n");
    }

    coiInit(symb_table, hierarchy);
    cmp_struct_set_coi(cmps);

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "COI initialized.\n");
    }
  }

  SET_FOREACH(base, iter) {
    node_ptr var = Set_GetMember(base, iter);
    Set_t varcoi = computeCoiVar(symb_table, hierarchy, var);
    coi = Set_Union(coi, varcoi);
  }

  return coi;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Compute the dependencies of an atom

  This function computes the dependencies of an atom. If
   the atom corresponds to a variable then the singleton with the
   variable is returned. If the atom corresponds to a "running"
   condition the singleton with variable PROCESS_SELECTOR_VAR_NAME is
   returned. Otherwise if the atom corresponds to a defined symbol the
   dependency set corresponding to the body of the definition is
   computed and returned. filter specifies what variables we are
   interested to, as in Formula_GetDependenciesByType, and
   is_inside_next is supposed to be true if the atom is inside a Next,
   false otherwise. Returned set must be disposed by the caller

  \se The <tt>define_dep_hash</tt> is modified in
   order to memoize previously computed dependencies of defined symbols.

  \sa Formula_GetDependencies
*/
static Set_t
formulaGetDefinitionDependencies(const SymbTable_ptr symb_table,
                                 node_ptr formula, SymbFilterType filter,
                                 boolean preserve_time, int time,
                                 hash_ptr dependencies_hash)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));


  Set_t result;
  if (SymbTable_is_symbol_var(symb_table, formula)) {
    if (((filter & VFT_INPUT) && SymbTable_is_symbol_input_var(symb_table,
                                                               formula)) ||
        ((filter & VFT_CURRENT) && SymbTable_is_symbol_state_var(symb_table,
                                                                 formula)) ||
        ((filter & VFT_FROZEN) && SymbTable_is_symbol_frozen_var(symb_table,
                                                                 formula))) {
      if (preserve_time) {
        if ( EXPR_UNTIMED_NEXT == time &&
             !SymbTable_is_symbol_input_var(symb_table, formula) ) {
          formula = ExprMgr_next(exprs, formula, symb_table);
        }
        else if (time >= 0) {
          formula = ExprMgr_attime(exprs, formula, time, symb_table);
        }
      }
      return Set_MakeSingleton((Set_Element_t) formula);
    }
    /* a variable filtered out */
    return Set_MakeEmpty();
  } /* end of outer if branch */

  if (SymbTable_is_symbol_define(symb_table, formula)) {
    Tuple5 key;

    mk_hash_key(formula, Nil, filter, preserve_time, time, &key);

    result = lookup_dependencies_hash(dependencies_hash, NODE_PTR(&key));
    if (result == BUILDING_DEP_SET) {
      ErrorMgr_error_circular(errmgr, formula); }

    if (result == EMPTY_DEP_SET) {
      nusmv_assert(!(filter & VFT_DEFINE));

      return Set_MakeEmpty();
    }

    if (result == (Set_t) NULL) {
      /* We mark the formula as open and we start looking for the body
         dependencies. */
      node_ptr nformula;
      insert_dependencies_hash(dependencies_hash, NODE_PTR(&key),
                               BUILDING_DEP_SET);
      ErrorMgr_io_atom_push(errmgr, formula);
      nformula = SymbTable_get_define_body(symb_table, formula);
      result =
        formulaGetDependenciesRecur(symb_table, nformula,
                                    SymbTable_get_define_context(symb_table,
                                                                 formula),
                                    filter, preserve_time, time,
                                    dependencies_hash);
      ErrorMgr_io_atom_pop(errmgr);

      /* We add define to the result set if defines are include in filter */
      if (filter & VFT_DEFINE) {
        result = Set_AddMember(result, formula);
      }

      /* We mark the formula as closed, storing the computed
         dependencies for further use. */
      if (Set_IsEmpty(result)) {
        close_define_dependencies_hash(dependencies_hash, NODE_PTR(&key),
                                  EMPTY_DEP_SET);
      }
      else close_define_dependencies_hash(dependencies_hash, NODE_PTR(&key),
                                          result);
    }
    else {
      result = Set_Copy(result);
    }
  }
  else if (SymbTable_is_symbol_array_define(symb_table, formula)) {
    /* Recursively compute the dependencies for this define array */
    node_ptr nformula = SymbTable_get_array_define_body(symb_table, formula);
    result =
      formulaGetDependenciesRecur(symb_table, nformula,
                                  SymbTable_get_array_define_context(symb_table,
                                                                     formula),
                                  filter, preserve_time, time,
                                  dependencies_hash);

    /* We add the define array to the result set if defines are include in
       filter */
    if (filter & VFT_DEFINE) {
      result = Set_AddMember(result, formula);
    }
  }
  else if (SymbTable_is_symbol_variable_array(symb_table, formula)) {
    /* Array dependencies are all it's elements */

    SymbType_ptr type;
    int low, high;
    int i;

    type = SymbTable_get_variable_array_type(symb_table, formula);
    low = SymbType_get_array_lower_bound(type);
    high = SymbType_get_array_upper_bound(type);

    result = Set_MakeEmpty();

    for (i = low; i <= high; ++i) {
      node_ptr index = find_node(nodemgr, NUMBER, NODE_FROM_INT(i), Nil);
      node_ptr arr_var = find_node(nodemgr, ARRAY, formula, index);
      Set_t ret;

      ret = formulaGetDefinitionDependencies(symb_table, arr_var, filter,
                                             preserve_time, time,
                                             dependencies_hash);

      result = Set_Union(result, ret);
    }

  }
  else {
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    StreamMgr_print_error(streams,  "Undefined symbol \"");
    StreamMgr_nprint_error(streams, wffprint, "%N", formula);
    StreamMgr_print_error(streams,  "\"\n");
    ErrorMgr_nusmv_exit(errmgr, 1);
  }

  return result;
}

/*!
  \brief Recursive call to Formula_GetDependenciesByType

  Recursive call to Formula_GetDependenciesByType.
   Returned set must be released by the caller.

  \sa formulaGetDefinitionDependencies
   Formula_GetDependenciesByType
*/
static Set_t
formulaGetDependenciesRecur(const SymbTable_ptr symb_table,
                            node_ptr formula, node_ptr context,
                            SymbFilterType filter,
                            boolean preserve_time, int time,
                            hash_ptr dependencies_hash)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs =
    EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  Set_t result;
  Tuple5 key;
  boolean is_to_be_inserted;

  is_to_be_inserted = true;

  /* 0 for filter means no variables are looked for. Do not create
     a special constant in SymbFilterType, otherwise it will be
     visible outside (which may be not good). */
  if (formula == Nil || filter == 0) return Set_MakeEmpty();

  mk_hash_key(formula, context, filter, preserve_time, time, &key);

  result = lookup_dependencies_hash(dependencies_hash, NODE_PTR(&key));

  if (result == EMPTY_DEP_SET) {
    return Set_MakeEmpty();
  }

  if (result != (Set_t)NULL) {
    return Set_Copy(result);
  }

  switch (node_get_type(formula)) {
  case CONTEXT:
    result = formulaGetDependenciesRecur(symb_table, cdr(formula),
                                         car(formula), filter,
                                         preserve_time, time,
                                         dependencies_hash);
    break;

  case BIT:
    /* ignore bits, consider only scalar vars */
    result = formulaGetDependenciesRecur(symb_table, car(formula), context,
                                         filter, preserve_time, time,
                                         dependencies_hash);
    break;

  case ATOM:
    {
      ResolveSymbol_ptr rs;
      node_ptr name;

      rs = SymbTable_resolve_symbol(symb_table, formula, context);

      name = ResolveSymbol_get_resolved_name(rs);

      if (ResolveSymbol_is_error(rs)) {
        ResolveSymbol_throw_error(rs, env);
      }

      if (ResolveSymbol_is_parameter(rs)) {
        /* a formal parameter */
        node_ptr param;
        param = SymbTable_get_flatten_actual_parameter(symb_table, name);
        result = formulaGetDependenciesRecur(symb_table, param, context,
                                             filter, preserve_time, time,
                                             dependencies_hash);
      }
      else if (ResolveSymbol_is_constant(rs)) result = Set_MakeEmpty();
      else { /* it should be a defined symbol, running, or a variable */
        result = formulaGetDefinitionDependencies(symb_table,
                                                  name, filter,
                                                  preserve_time, time,
                                                  dependencies_hash);

        /* It is possible formulaGetDefinitionDependencies inserted a value on
           the same key */
        if (name == formula && SymbTable_is_symbol_define(symb_table, name)) {
          nusmv_assert((Set_t)NULL !=
                       lookup_dependencies_hash(dependencies_hash,
                                                NODE_PTR(&key)));
          is_to_be_inserted = false;
        }
      }
      break;
    }

    /* a variable of word type */
  case SIGNED_WORD:
  case UNSIGNED_WORD:
    /* ignore width (a constant), consider only bits */
    result = formulaGetDependenciesRecur(symb_table, car(formula),
                                         context, filter, preserve_time, time,
                                         dependencies_hash);
    break;

    /* no dependencies */
  case FAILURE:
  case TRUEEXP:
  case FALSEEXP:
  case NUMBER:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case UWCONST:
  case SWCONST:
  case TWODOTS: /* Sets */
  case WSIZEOF: /* It is not important the expression inside, it is
                   like a constant */
  case CAST_TOINT: /* It is not important the expression inside, it is like
                      a constant */
    result = Set_MakeEmpty();
    break;

    /* unary operation */
  case NEXT:   /* next(alpha), with alpha possibly complex, thus ... */
    /* nested next are checked by type checker */
    if(filter & VFT_NEXT) {
      /* the next variables become the current variables from this frame on */
      filter = (filter & (~VFT_NEXT)) | VFT_CURRENT;
    }
    else if (filter & VFT_CURRENT) {
      filter = filter & (~VFT_CURRENT);
    }

    /* input and frozen variables are searched for independently of
       next-operator */
    result = formulaGetDependenciesRecur(symb_table, car(formula), context,
                                         filter, preserve_time,
                                         EXPR_UNTIMED_NEXT, dependencies_hash);
    break;

  case ATTIME: /* the variable without time information are returned */
    {
      int time2 = ExprMgr_attime_get_time(exprs, formula);
      result = formulaGetDependenciesRecur(symb_table, car(formula),
                                           context, filter,
                                           preserve_time, time2,
                                           dependencies_hash);
      break;
    }

  case NOT:    /* Unary boolean connectives */
  case UMINUS:
  case EX:    /* CTL unary Temporal Operators */
  case SMALLINIT:  /* used for init(expr) */
  case AX:
  case EF:
  case AF:
  case EG:
  case AG:
  case EBF:    /* CTL unary bounded Temporal Operators */
  case ABF:
  case EBG:
  case ABG:
  case EBU:    /* CTL binary bounded Temporal Operators */
  case ABU:
  case OP_NEXT:    /* LTL unary Temporal Operators */
  case OP_PREC:
  case OP_NOTPRECNOT:
  case OP_FUTURE:
  case OP_ONCE:
  case OP_GLOBAL:
  case OP_HISTORICAL:
  case CAST_BOOL: /* Casts */
  case CAST_WORD1:
  case CAST_SIGNED:
  case CAST_UNSIGNED:
  case FLOOR:
    result = formulaGetDependenciesRecur(symb_table, car(formula), context,
                                         filter, preserve_time, time,
                                         dependencies_hash);
    break;

  case WRESIZE: /* For this it is important only the expression on the lhs */
    result = formulaGetDependenciesRecur(symb_table, car(formula), context,
                                         filter, preserve_time, time,
                                         dependencies_hash);
    break;

    /* binary operation */
  case EXTEND:
  case EQDEF: /* assignment */
  case CONS:
  case UNION:
  case SETIN:
  case COLON:
  case PLUS:    /* Numerical Operations */
  case MINUS:
  case TIMES:
  case DIVIDE:
  case MOD:
  case LSHIFT:  /* Binary shifts and rotates */
  case RSHIFT:
  case LROTATE:
  case RROTATE:
  case CONCATENATION: /* concatenation */
  case EQUAL:   /* Comparison Operations */
  case NOTEQUAL:
  case LT:
  case GT:
  case LE:
  case GE:
  case AND:    /* Binary boolean connectives */
  case OR:
  case XOR:
  case XNOR:
  case IMPLIES:
  case IFF:
  case EU:     /* CTL binary  Temporal Operators */
  case AU:
  case UNTIL:    /* LTL binary Temporal Operators */
  case RELEASES:
  case SINCE:
  case TRIGGERED:
  case MAXU:    /* MIN MAX operators */
  case MINU:
    {
      Set_t right = formulaGetDependenciesRecur(symb_table,
                                                cdr(formula), context,
                                                filter, preserve_time, time,
                                                dependencies_hash);

      result = formulaGetDependenciesRecur(symb_table,
                                           car(formula), context,
                                           filter, preserve_time, time,
                                           dependencies_hash);
      result = Set_Union(result, right);
      Set_ReleaseSet(right);
      break;
    }

    /* 3-arity operations */
  case CASE:
  case IFTHENELSE:
    {
      Set_t then_arg  = formulaGetDependenciesRecur(symb_table,
                                                    cdr(car(formula)), context,
                                                    filter, preserve_time,
                                                    time, dependencies_hash);
      Set_t else_arg  = formulaGetDependenciesRecur(symb_table,
                                                    cdr(formula), context,
                                                    filter, preserve_time,
                                                    time, dependencies_hash);

      result = formulaGetDependenciesRecur(symb_table,
                                           car(car(formula)),context,
                                           filter, preserve_time, time,
                                           dependencies_hash);

      result = Set_Union(Set_Union(result, then_arg), else_arg);
      Set_ReleaseSet(else_arg);
      Set_ReleaseSet(then_arg);
      break;
    }

  case NFUNCTION:
    result = formulaGetDependenciesRecur(symb_table, cdr(formula), context,
                                         filter, preserve_time, time,
                                         dependencies_hash);
    break;

  case BIT_SELECTION:
    {
      Set_t high_bit = formulaGetDependenciesRecur(symb_table,
                                                   car(cdr(formula)), context,
                                                   filter, preserve_time, time,
                                                   dependencies_hash);
      Set_t low_bit = formulaGetDependenciesRecur(symb_table,
                                                  cdr(cdr(formula)), context,
                                                  filter, preserve_time, time,
                                                  dependencies_hash);

      result = formulaGetDependenciesRecur(symb_table,
                                           car(formula), context,
                                           filter, preserve_time, time,
                                           dependencies_hash);

      result = Set_Union(Set_Union(result, high_bit), low_bit);
      Set_ReleaseSet(low_bit);
      Set_ReleaseSet(high_bit);
      break;
    }

  case COUNT:
    {
      node_ptr list = car(formula);

      result = Set_MakeEmpty();

      while (Nil != list) {
        Set_t tmp = formulaGetDependenciesRecur(symb_table, car(list), context,
                                                filter, preserve_time, time,
                                                dependencies_hash);
        result = Set_Union(result, tmp);
        Set_ReleaseSet(tmp);

        list = cdr(list);
      }

      break;
    }
    /* Operations on WORDARRAYs */
  case WAWRITE:
    {
      Set_t location = formulaGetDependenciesRecur(symb_table,
                                                   car(cdr(formula)), context,
                                                   filter, preserve_time, time,
                                                   dependencies_hash);
      Set_t address = formulaGetDependenciesRecur(symb_table,
                                                  cdr(cdr(formula)), context,
                                                  filter, preserve_time, time,
                                                  dependencies_hash);

      result = formulaGetDependenciesRecur(symb_table,
                                           car(formula), context,
                                           filter, preserve_time, time,
                                           dependencies_hash);
      result = Set_Union(Set_Union(result, location), address);
      Set_ReleaseSet(address);
      Set_ReleaseSet(location);
      break;
    }

  case WAREAD:
    {
      Set_t location = formulaGetDependenciesRecur(symb_table,
                                                   cdr(formula), context,
                                                   filter, preserve_time, time,
                                                   dependencies_hash);

      result = formulaGetDependenciesRecur(symb_table,
                                           car(formula), context,
                                           filter, preserve_time, time,
                                           dependencies_hash);
      result = Set_Union(result, location);
      Set_ReleaseSet(location);
      break;
    }


    /* name cases */
  case DOT:
    {
      ResolveSymbol_ptr rs;
      node_ptr name;

      rs = SymbTable_resolve_symbol(symb_table, formula, context);
      name = ResolveSymbol_get_resolved_name(rs);

      if (ResolveSymbol_is_constant(rs)) {
        result = Set_MakeEmpty();
      }
      else {
        result = formulaGetDefinitionDependencies(symb_table, name, filter,
                                                  preserve_time, time,
                                                  dependencies_hash);

        /* It is possible formulaGetDefinitionDependencies inserted a value on
           the same key */
        if (name == formula && SymbTable_is_symbol_define(symb_table, name)) {
          nusmv_assert((Set_t)NULL !=
                       lookup_dependencies_hash(dependencies_hash,
                                                NODE_PTR(&key)));
          is_to_be_inserted = false;
        }
      }

      break;
    }

  case ARRAY:
    {
      ResolveSymbol_ptr rs;
      node_ptr name;

      rs = SymbTable_resolve_symbol(symb_table, formula, context);
      name = ResolveSymbol_get_resolved_name(rs);

      if (ResolveSymbol_is_defined(rs)) {
        /* this array is identifier-with-brackets */
        result = formulaGetDefinitionDependencies(symb_table, name, filter,
                                                  preserve_time, time,
                                                  dependencies_hash);

        /* It is possible formulaGetDefinitionDependencies inserted a value on
           the same key */
        if (name == formula && SymbTable_is_symbol_define(symb_table, name)) {
          nusmv_assert((Set_t)NULL !=
                       lookup_dependencies_hash(dependencies_hash,
                                                NODE_PTR(&key)));
          is_to_be_inserted = false;
        }

      }
      else {  /* this array is an expression => process the children */
        Set_t tmp;
        result = formulaGetDependenciesRecur(symb_table,
                                             car(formula), context,
                                             filter, preserve_time, time,
                                             dependencies_hash);
        tmp = formulaGetDependenciesRecur(symb_table,
                                          cdr(formula), context,
                                          filter, preserve_time, time,
                                          dependencies_hash);
        result = Set_Union(result, tmp);
        Set_ReleaseSet(tmp);
      }
      break;
    }


  case ARRAY_DEF:
    {
      node_ptr iter;

      result = Set_MakeEmpty();

      for (iter = car(formula); iter != Nil; iter = cdr(iter)) {
        Set_t tmp;
        nusmv_assert(CONS == node_get_type(iter));
        tmp = formulaGetDependenciesRecur(symb_table,
                                          car(iter), context,
                                          filter, preserve_time, time,
                                          dependencies_hash);
        result = Set_Union(result, tmp);
        Set_ReleaseSet(tmp);
      }
      break;
    }

    /* "MR: start PSL:" */
  case PSL_INF:
  case PSL_RANGE:
    result = Set_MakeEmpty();
    break;

  case PSL_WSELECT:
    {
      PslNode_ptr right = psl_node_get_right(formula);

      Set_t object   = formulaGetDependenciesRecur(symb_table,
                                                   psl_node_get_left(formula),
                                                   context, filter,
                                                   preserve_time, time,
                                                   dependencies_hash);
      Set_t high_bit = formulaGetDependenciesRecur(symb_table,
                                                   psl_node_get_left(right),
                                                   context, filter,
                                                   preserve_time, time,
                                                   dependencies_hash);
      Set_t low_bit  = formulaGetDependenciesRecur(symb_table,
                                                   psl_node_get_right(right),
                                                   context, filter,
                                                   preserve_time, time,
                                                   dependencies_hash);

      result = Set_Union(Set_Union(object, high_bit), low_bit);
      Set_ReleaseSet(low_bit);
      Set_ReleaseSet(high_bit);
      break;
    }

  case PSL_SERE:
  case PSL_SERECOMPOUND:
    result = formulaGetDependenciesRecur(symb_table,
                                         psl_node_get_left(formula), context,
                                         filter, preserve_time, time,
                                         dependencies_hash);
    break;

  case PSL_CONCATENATION:
    {
      Set_t right = formulaGetDependenciesRecur(symb_table,
                                                psl_node_get_right(formula),
                                                context,
                                                filter, preserve_time, time,
                                                dependencies_hash);

      result = formulaGetDependenciesRecur(symb_table,
                                           psl_node_get_left(formula),
                                           context,
                                           filter, preserve_time, time,
                                           dependencies_hash);
      result = Set_Union(result, right);
      Set_ReleaseSet(right);
      break;
    }

  case PSL_SERECONCAT:
  case PSL_SEREFUSION:
    {
      Set_t right = formulaGetDependenciesRecur(symb_table,
                                                psl_node_get_right(formula),
                                                context,
                                                filter, preserve_time, time,
                                                dependencies_hash);
      result = formulaGetDependenciesRecur(symb_table,
                                           psl_node_get_left(formula),
                                           context,
                                           filter, preserve_time, time,
                                           dependencies_hash);
      result = Set_Union(result, right);
      Set_ReleaseSet(right);
      break;
    }

  case PSL_SEREREPEATED:
    result = formulaGetDependenciesRecur(symb_table,
                                         psl_node_sere_repeated_get_expr(formula),
                                         context, filter, preserve_time, time,
                                         dependencies_hash);
    break;

  case PSL_REPLPROP:
    result = formulaGetDependenciesRecur(symb_table,
                                         psl_node_repl_prop_get_property(formula),
                                         context, filter, preserve_time, time,
                                         dependencies_hash);
    break;

  case PSL_PIPEMINUSGT:
  case PSL_PIPEEQGT:
  case PSL_DIAMONDMINUSGT:
    {
      Set_t con = formulaGetDependenciesRecur(symb_table,
                                              psl_node_suffix_implication_get_consequence(formula),
                                              context,
                                              filter, preserve_time, time,
                                              dependencies_hash);

      result = formulaGetDependenciesRecur(symb_table,
                                           psl_node_suffix_implication_get_premise(formula),
                                           context,
                                           filter, preserve_time, time,
                                           dependencies_hash);
      result = Set_Union(result, con);
      Set_ReleaseSet(con);
      break;
    }

  case PSL_ALWAYS:
  case PSL_NEVER:
  case PSL_EVENTUALLYBANG:
    result = formulaGetDependenciesRecur(symb_table,
                                         psl_node_get_left(formula),
                                         context,
                                         filter, preserve_time, time,
                                         dependencies_hash);
    break;

  case PSL_WITHINBANG:
  case PSL_WITHIN:
  case PSL_WITHINBANG_:
  case PSL_WITHIN_:
    {
      Set_t n2 = formulaGetDependenciesRecur(symb_table,
                                             psl_node_get_right(psl_node_get_left(formula)),
                                             context,
                                             filter, preserve_time, time,
                                             dependencies_hash);
      Set_t n3 = formulaGetDependenciesRecur(symb_table,
                                             psl_node_get_right(formula),
                                             context,
                                             filter, preserve_time, time,
                                             dependencies_hash);

      result = formulaGetDependenciesRecur(symb_table,
                                           psl_node_get_left(psl_node_get_left(formula)),
                                           context,
                                           filter, preserve_time, time,
                                           dependencies_hash);

      result = Set_Union(Set_Union(result, n2), n3);
      Set_ReleaseSet(n3);
      Set_ReleaseSet(n2);
      break;
    }

  case PSL_NEXT_EVENT_ABANG:
  case PSL_NEXT_EVENT_A:
  case PSL_NEXT_EVENT_EBANG:
  case PSL_NEXT_EVENT_E:
  case PSL_NEXT_EVENTBANG:
  case PSL_NEXT_EVENT:
  case PSL_NEXT_ABANG:
  case PSL_NEXT_EBANG:
  case PSL_NEXT_A:
  case PSL_NEXT_E:
  case PSL_NEXTBANG:
  case PSL_NEXT:
  case PSL_X:
  case PSL_XBANG:
    {
      Set_t n2 = formulaGetDependenciesRecur(symb_table,
                                             psl_node_extended_next_get_when(formula),
                                             context,
                                             filter, preserve_time, time,
                                             dependencies_hash);
      Set_t n3 = formulaGetDependenciesRecur(symb_table,
                                             psl_node_extended_next_get_condition(formula),
                                             context,
                                             filter, preserve_time, time,
                                             dependencies_hash);

      result = formulaGetDependenciesRecur(symb_table,
                                           psl_node_extended_next_get_expr(formula),
                                           context,
                                           filter, preserve_time, time,
                                           dependencies_hash);

      result = Set_Union(Set_Union(result, n2), n3);
      Set_ReleaseSet(n3);
      Set_ReleaseSet(n2);
      break;
    }

  case PSL_BEFOREBANG:
  case PSL_BEFORE:
  case PSL_BEFOREBANG_:
  case PSL_BEFORE_:
  case PSL_UNTILBANG:
  case PSL_UNTIL:
  case PSL_UNTILBANG_:
  case PSL_UNTIL_:
  case PSL_ABORT:
  case PSL_W:
  case PSL_OR:
  case PSL_CARET:
  case PSL_TILDE:
  case PSL_EQEQ:
  case PSL_PIPEPIPE:
  case PSL_AMPERSANDAMPERSAND:
  case PSL_WHILENOTBANG:
  case PSL_WHILENOT:
  case PSL_WHILENOTBANG_:
  case PSL_WHILENOT_:
    {
      Set_t right = formulaGetDependenciesRecur(symb_table,
                                                psl_node_get_right(formula),
                                                context,
                                                filter, preserve_time, time,
                                                dependencies_hash);

      result = formulaGetDependenciesRecur(symb_table,
                                           psl_node_get_left(formula),
                                           context,
                                           filter, preserve_time, time,
                                           dependencies_hash);
      result = Set_Union(result, right);
      Set_ReleaseSet(right);
      break;
    }

  case PSL_ITE:
    {
      Set_t then_arg  = formulaGetDependenciesRecur(symb_table,
                                                    psl_node_get_ite_then(formula),
                                                    context, filter,
                                                    preserve_time, time,
                                                    dependencies_hash);
      Set_t else_arg  = formulaGetDependenciesRecur(symb_table,
                                                    psl_node_get_ite_else(formula),
                                                    context, filter,
                                                    preserve_time, time,
                                                    dependencies_hash);

      result =
        formulaGetDependenciesRecur(symb_table, psl_node_get_ite_cond(formula),
                                    context, filter, preserve_time, time,
                                    dependencies_hash);
      result = Set_Union(Set_Union(result, then_arg), else_arg);
      Set_ReleaseSet(else_arg);
      Set_ReleaseSet(then_arg);
      break;
    }
    /* "MR: end PSL:" */
  default:
    StreamMgr_print_error(streams,
                          "\nFormula_GetDependencies: Reached undefined connective (%d)\n",
                          node_get_type(formula));
    ErrorMgr_nusmv_exit(errmgr, 1);
  }

  if (is_to_be_inserted) {
    /* this assures absence of leaks */
    nusmv_assert((Set_t)NULL ==
                 lookup_dependencies_hash(dependencies_hash,
                                          NODE_PTR(&key)));
    if (Set_IsEmpty(result)) {
      insert_dependencies_hash(dependencies_hash, NODE_PTR(&key),
                               EMPTY_DEP_SET);
    }
    else insert_dependencies_hash(dependencies_hash, NODE_PTR(&key),
                                  result);
  }

  return result;
}

/*!
  \brief Pre-compute the COI of the variables

  Computes the COI of all the variables occurring within
   the symbol table

  \sa ComputeCOI
*/
static void coiInit(const SymbTable_ptr symb_table, FlatHierarchy_ptr hierarchy)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  SymbTableIter iter;

  hash_ptr coi_hash =
    compile_cone_get_handled_hash(symb_table, ST_CONE_COI_HASH);

  hash_ptr coi0_hash =
    compile_cone_get_handled_hash(symb_table, ST_CONE_COI0_HASH);

  if (COI_VERBOSE) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,"*** INIT COI ***\n");
  }

  SYMB_TABLE_FOREACH(symb_table, iter, STT_VAR) {
    node_ptr var = SymbTable_iter_get_symbol(symb_table, &iter);
    boolean nonassign = false;
    Set_t base;

    /* We guarantee the set is initialized with something different
       from NULL */
    insert_coi0_hash(coi0_hash, var, NO_DEP_SET);

    base = _coi_get_var_coi0(symb_table, hierarchy, var, &nonassign, true,
                             coi0_hash);

    /* We associate to the var no set */
    insert_coi_hash(coi_hash, var, NO_DEP_SET);

    if (COI_VERBOSE) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      const MasterPrinter_ptr wffprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

      Logger_nlog(logger, wffprint, "Variable  %N\n", var);

      if (nonassign) {
        Logger_log(logger,"  Has non-assign constraints\n");
      }

      Logger_log(logger,"  Initial coi: ");
      Set_PrintSet(wffprint, Logger_get_stream(logger), base, NULL, NULL);
      Logger_log(logger,"\n");
    }

    Set_ReleaseSet(base);
  } /* vars iteration */
}

/*!
  \brief Returns the set of constants occurring into the given
   formula

  Returns the set of constants occurring into the given
   formula

  \se required

  \sa optional
*/
static Set_t
formulaGetConstantsRecur(const SymbTable_ptr symb_table,
                         node_ptr formula, node_ptr context,
                         hash_ptr consts_hash)
{
  const NuSMVEnv_ptr env =
    EnvObject_get_environment(ENV_OBJECT(symb_table));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  node_ptr key = find_node(nodemgr, CONTEXT, context, formula);
  Set_t result;
  if (formula == Nil) return Set_MakeEmpty();

  result = lookup_consts_hash(consts_hash, key);
  if (result == EMPTY_DEP_SET) return Set_MakeEmpty();
  if (result != (Set_t) NULL) return Set_Copy(result);

  switch (node_get_type(formula)) {
  case CONTEXT:
    result = formulaGetConstantsRecur(symb_table, cdr(formula), car(formula),
                                      consts_hash);
    break;

  case BIT:
    /* ignore bits, consider only scalar vars */
    return Set_MakeEmpty();

    /* name cases */
  case DOT:
  case ARRAY:
  case ATOM:
    {
      ResolveSymbol_ptr rs;
      node_ptr name;

      rs = SymbTable_resolve_symbol(symb_table, formula, context);

      name = ResolveSymbol_get_resolved_name(rs);

      if (ResolveSymbol_is_error(rs)) ResolveSymbol_throw_error(rs, env);

      if (ResolveSymbol_is_constant(rs)) {
        result = Set_MakeSingleton((Set_Element_t) formula);
      }
      else if (ResolveSymbol_is_parameter(rs)) {
        node_ptr param;
        param = SymbTable_get_flatten_actual_parameter(symb_table, name);
        result = formulaGetConstantsRecur(symb_table, param, context,
                                          consts_hash);
      }
      /* it should be a defined symbol, running, or a variable */
      else if (ResolveSymbol_is_var(rs)) {
        result = Set_MakeEmpty();
      }
      else if (ResolveSymbol_is_define(rs)) {
        result =
          formulaGetConstantsRecur(symb_table,
                                   SymbTable_get_define_body(symb_table,
                                                             formula),
                                   SymbTable_get_define_context(symb_table,
                                                                formula),
                                   consts_hash);
      }
      else {
        /* only vars are remaining and they can be ignored */
        nusmv_assert(SymbTable_is_symbol_var(symb_table, name) ||
                     SymbTable_is_symbol_variable_array(symb_table, name) ||
                     SymbTable_is_symbol_function(symb_table, name));

        result = Set_MakeEmpty();
      }

      break;
    }

    /* a word var */
  case SIGNED_WORD:
  case UNSIGNED_WORD:
  case FAILURE:
  case TWODOTS: /* Sets */
    return Set_MakeEmpty();

  case TRUEEXP:
  case FALSEEXP:
  case NUMBER:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case UWCONST:
  case SWCONST:
    result = Set_MakeSingleton((Set_Element_t) formula);
    break;


  case ATTIME:   /* unary operation */
  case NEXT:   /* unary operation */
  case NOT:    /* Unary boolean connectives */
  case UMINUS:
  case EX:    /* CTL unary Temporal Operators */
  case SMALLINIT:  /* used for init(expr) */
  case AX:
  case EF:
  case AF:
  case EG:
  case AG:
  case EBF:    /* CTL unary bounded Temporal Operators */
  case ABF:
  case EBG:
  case ABG:
  case EBU:    /* CTL binary bounded Temporal Operators */
  case ABU:
  case OP_NEXT:    /* LTL unary Temporal Operators */
  case OP_PREC:
  case OP_NOTPRECNOT:
  case OP_FUTURE:
  case OP_ONCE:
  case OP_GLOBAL:
  case OP_HISTORICAL:
  case CAST_BOOL: /* Casts */
  case CAST_WORD1:
  case CAST_SIGNED:
  case CAST_UNSIGNED:
  case WSIZEOF: /* We extract possible constant in the body */
  case CAST_TOINT:  /* We extract possible constant in the body */
  case FLOOR:
    result = formulaGetConstantsRecur(symb_table, car(formula), context,
                                      consts_hash);
    break;

    /* binary operation */
  case EXTEND:
  case WRESIZE:
  case EQDEF: /* assignment */
  case CONS:
  case UNION:
  case SETIN:
  case COLON:
  case PLUS:    /* Numerical Operations */
  case MINUS:
  case TIMES:
  case DIVIDE:
  case MOD:
  case LSHIFT:  /* Binary shifts and rotates */
  case RSHIFT:
  case LROTATE:
  case RROTATE:
  case CONCATENATION: /* concatenation */
  case EQUAL:   /* Comparison Operations */
  case NOTEQUAL:
  case LT:
  case GT:
  case LE:
  case GE:
  case AND:    /* Binary boolean connectives */
  case OR:
  case XOR:
  case XNOR:
  case IMPLIES:
  case IFF:
  case EU:     /* CTL binary  Temporal Operators */
  case AU:
  case UNTIL:    /* LTL binary Temporal Operators */
  case RELEASES:
  case SINCE:
  case TRIGGERED:
  case MAXU:    /* MIN MAX operators */
  case MINU:
    {
      Set_t right = formulaGetConstantsRecur(symb_table, cdr(formula), context,
                                             consts_hash);
      result = formulaGetConstantsRecur(symb_table, car(formula), context,
                                        consts_hash);
      result = Set_Union(result, right);
      Set_ReleaseSet(right);
      break;
    }

  case NFUNCTION:
    result = formulaGetConstantsRecur(symb_table, cdr(formula), context,
                                      consts_hash);
    break;

    /* 3-arity operations */
  case CASE:
  case IFTHENELSE:
    {
      Set_t condition = formulaGetConstantsRecur(symb_table, car(car(formula)),
                                                 context, consts_hash);
      Set_t then_arg  = formulaGetConstantsRecur(symb_table, cdr(car(formula)),
                                                 context, consts_hash);
      Set_t else_arg  = formulaGetConstantsRecur(symb_table, cdr(formula),
                                                 context, consts_hash);

      result = Set_Union(Set_Union(condition, then_arg), else_arg);
      Set_ReleaseSet(else_arg);
      Set_ReleaseSet(then_arg);
      break;
    }

  case BIT_SELECTION:
    {
      Set_t object   = formulaGetConstantsRecur(symb_table, car(formula),
                                                context, consts_hash);
      Set_t high_bit = formulaGetConstantsRecur(symb_table, car(cdr(formula)),
                                                context, consts_hash);
      Set_t low_bit  = formulaGetConstantsRecur(symb_table, cdr(cdr(formula)),
                                                context, consts_hash);
      result = Set_Union(Set_Union(object, high_bit), low_bit);
      Set_ReleaseSet(low_bit);
      Set_ReleaseSet(high_bit);
      break;
    }

  case COUNT:
    {
      node_ptr list = car(formula);

      result = Set_MakeEmpty();

      while (Nil != list) {
        Set_t tmp = formulaGetConstantsRecur(symb_table, car(list), context,
                                             consts_hash);

        result = Set_Union(result, tmp);
        Set_ReleaseSet(tmp);

        list = cdr(list);
      }

      break;
    }

    /* Operations on WORDARRAYs */
  case WAWRITE:
    {
      Set_t memory   = formulaGetConstantsRecur(symb_table, car(formula),
                                                context, consts_hash);
      Set_t location = formulaGetConstantsRecur(symb_table, car(cdr(formula)),
                                                context, consts_hash);
      Set_t address  = formulaGetConstantsRecur(symb_table, cdr(cdr(formula)),
                                                context, consts_hash);
      result = Set_Union(Set_Union(memory, location), address);
      Set_ReleaseSet(address);
      Set_ReleaseSet(location);
      break;
    }

  case WAREAD:
    {
      Set_t memory = formulaGetConstantsRecur(symb_table,
                                              car(formula), context,
                                              consts_hash);
      Set_t location = formulaGetConstantsRecur(symb_table,
                                                cdr(formula), context,
                                                consts_hash);
      result = Set_Union(memory, location);
      Set_ReleaseSet(location);
      break;
    }
    /* "MR: start PSL:" */
  case PSL_INF:
  case PSL_RANGE:
    result = Set_MakeEmpty();
    break;

  case PSL_WSELECT:
    {
      PslNode_ptr right = psl_node_get_right(formula);

      Set_t object   = formulaGetConstantsRecur(symb_table,
                                                psl_node_get_left(formula),
                                                context, consts_hash);
      Set_t high_bit = formulaGetConstantsRecur(symb_table,
                                                psl_node_get_left(right),
                                                context, consts_hash);
      Set_t low_bit  = formulaGetConstantsRecur(symb_table,
                                                psl_node_get_right(right),
                                                context, consts_hash);

      result = Set_Union(Set_Union(object, high_bit), low_bit);
      Set_ReleaseSet(low_bit);
      Set_ReleaseSet(high_bit);
      break;
    }

  case PSL_SERE:
  case PSL_SERECOMPOUND:
    result = formulaGetConstantsRecur(symb_table, psl_node_get_left(formula),
                                      context, consts_hash);
    break;

  case PSL_SERECONCAT:
  case PSL_SEREFUSION:
  case PSL_CONCATENATION:
    {
      Set_t left = formulaGetConstantsRecur(symb_table,
                                            psl_node_get_left(formula), context,
                                            consts_hash);
      Set_t right = formulaGetConstantsRecur(symb_table,
                                             psl_node_get_right(formula),
                                             context, consts_hash);
      result = Set_Union(left, right);
      Set_ReleaseSet(right);
      break;
    }

  case PSL_SEREREPEATED:
    result = formulaGetConstantsRecur(symb_table,
                                      psl_node_sere_repeated_get_expr(formula),
                                      context, consts_hash);
    break;

  case PSL_REPLPROP:
    result = formulaGetConstantsRecur(symb_table,
                                      psl_node_repl_prop_get_property(formula),
                                      context, consts_hash);
    break;

  case PSL_PIPEMINUSGT:
  case PSL_PIPEEQGT:
  case PSL_DIAMONDMINUSGT:
    {
      Set_t pre = formulaGetConstantsRecur(symb_table,
                                           psl_node_suffix_implication_get_premise(formula),
                                           context, consts_hash);
      Set_t con = formulaGetConstantsRecur(symb_table,
                                           psl_node_suffix_implication_get_consequence(formula),
                                           context, consts_hash);
      result = Set_Union(pre, con);
      Set_ReleaseSet(con);
      break;
    }

  case PSL_ALWAYS:
  case PSL_NEVER:
  case PSL_EVENTUALLYBANG:
    result = formulaGetConstantsRecur(symb_table,
                                      psl_node_get_left(formula), context,
                                      consts_hash);
    break;

  case PSL_WITHINBANG:
  case PSL_WITHIN:
  case PSL_WITHINBANG_:
  case PSL_WITHIN_:
    {
      Set_t n1 = formulaGetConstantsRecur(symb_table,
                                          psl_node_get_left(psl_node_get_left(formula)), context,
                                          consts_hash);
      Set_t n2 = formulaGetConstantsRecur(symb_table,
                                          psl_node_get_right(psl_node_get_left(formula)), context,
                                          consts_hash);
      Set_t n3 = formulaGetConstantsRecur(symb_table,
                                          psl_node_get_right(formula), context, consts_hash);

      result = Set_Union(Set_Union(n1, n2), n3);
      Set_ReleaseSet(n3);
      Set_ReleaseSet(n2);
      break;
    }

  case PSL_NEXT_EVENT_ABANG:
  case PSL_NEXT_EVENT_A:
  case PSL_NEXT_EVENT_EBANG:
  case PSL_NEXT_EVENT_E:
  case PSL_NEXT_EVENTBANG:
  case PSL_NEXT_EVENT:
  case PSL_NEXT_ABANG:
  case PSL_NEXT_EBANG:
  case PSL_NEXT_A:
  case PSL_NEXT_E:
  case PSL_NEXTBANG:
  case PSL_NEXT:
  case PSL_X:
  case PSL_XBANG:
    {
      Set_t n1 = formulaGetConstantsRecur(symb_table,
                                          psl_node_extended_next_get_expr(formula), context,
                                          consts_hash);
      Set_t n2 = formulaGetConstantsRecur(symb_table,
                                          psl_node_extended_next_get_when(formula), context,
                                          consts_hash);
      Set_t n3 = formulaGetConstantsRecur(symb_table,
                                          psl_node_extended_next_get_condition(formula), context,
                                          consts_hash);

      result = Set_Union(Set_Union(n1, n2), n3);
      Set_ReleaseSet(n3);
      Set_ReleaseSet(n2);
      break;
    }

  case PSL_BEFOREBANG:
  case PSL_BEFORE:
  case PSL_BEFOREBANG_:
  case PSL_BEFORE_:
  case PSL_UNTILBANG:
  case PSL_UNTIL:
  case PSL_UNTILBANG_:
  case PSL_UNTIL_:
  case PSL_ABORT:
  case PSL_W:
  case PSL_OR:
  case PSL_CARET:
  case PSL_TILDE:
  case PSL_EQEQ:
  case PSL_PIPEPIPE:
  case PSL_AMPERSANDAMPERSAND:
  case PSL_WHILENOTBANG:
  case PSL_WHILENOT:
  case PSL_WHILENOTBANG_:
  case PSL_WHILENOT_:
    {
      Set_t left = formulaGetConstantsRecur(symb_table,
                                            psl_node_get_left(formula),
                                            context, consts_hash);
      Set_t right = formulaGetConstantsRecur(symb_table,
                                             psl_node_get_right(formula),
                                             context, consts_hash);
      result = Set_Union(left, right);
      Set_ReleaseSet(right);
      break;
    }

  case PSL_ITE:
    {
      Set_t condition = formulaGetConstantsRecur(symb_table,
                                                 psl_node_get_ite_cond(formula),
                                                 context, consts_hash);
      Set_t then_arg  = formulaGetConstantsRecur(symb_table,
                                                 psl_node_get_ite_then(formula),
                                                 context, consts_hash);
      Set_t else_arg  = formulaGetConstantsRecur(symb_table,
                                                 psl_node_get_ite_else(formula),
                                                 context, consts_hash);

      result = Set_Union(Set_Union(condition, then_arg), else_arg);
      Set_ReleaseSet(else_arg);
      Set_ReleaseSet(then_arg);
      break;
    }

    /* "MR: end PSL:" */
  default:
    {
      StreamMgr_print_error(streams,
                            "\nformulaGetConstantsRecur: Reached undefined connective (%d)\n",
                            node_get_type(formula));
      ErrorMgr_nusmv_exit(errmgr, 1);
    }
  }

  if (!Set_IsEmpty(result)) {
    insert_consts_hash(consts_hash, key, result);
  }

  return result;
}

/*!
  \brief Computes the complete cone for a given variable.

  optional

  \se required

  \sa optional
*/
static Set_t
computeCoiVar(SymbTable_ptr st, FlatHierarchy_ptr fh, node_ptr var)
{
  Set_t result;

  hash_ptr coi_hash =
    compile_cone_get_handled_hash(st, ST_CONE_COI_HASH);
  hash_ptr coi0_hash;

  result = lookup_coi_hash(coi_hash, var);

  if (NO_DEP_SET != result)
      return result;

  result = Set_MakeSingleton(var);

  coi0_hash =
    compile_cone_get_handled_hash(st, ST_CONE_COI0_HASH);

  {
    Set_t vars_left = Set_Copy(result);
    Set_Iterator_t iter;
    Set_t new_vars, vdeps;
    boolean nonassign = false;
    node_ptr v;

    while (!Set_IsEmpty(vars_left)) {

      new_vars = Set_MakeEmpty();

      SET_FOREACH(vars_left, iter) {
        v = NODE_PTR(Set_GetMember(vars_left, iter));
        vdeps = _coi_get_var_coi0(st, fh, v, &nonassign, true, coi0_hash);
        new_vars = Set_Union(new_vars, vdeps);
        Set_ReleaseSet(vdeps);
      }
      /* Put in vars_left the new fresh variables */
      Set_ReleaseSet(vars_left);
      vars_left = Set_Copy(new_vars);
      vars_left = Set_Difference(vars_left, result);
      result = Set_Union(result, new_vars);
      Set_ReleaseSet(new_vars);
    }

    Set_ReleaseSet(vars_left);
  }

  insert_coi_hash(coi_hash, var, result);

  return result;
}

/*!
  \brief Given a variable it returns the cone at depth 0.

  Given a variable it returns the cone at depth
   0. If use_cache is true, then the result is memoized on the
   cache. When use_cache is true, it is assumed the hierarchy to be the
   mainFlatHierarchy. An assertion enforces this condition.

   TODO[MR2MR]: Find a better name


  \se required

  \sa optional
*/
static Set_t _coi_get_var_coi0(SymbTable_ptr st,
                               FlatHierarchy_ptr hierarchy,
                               node_ptr var,
                               boolean* nonassign,
                               boolean use_cache,
                               hash_ptr coi0_hash)
{
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  FlatHierarchy_ptr mhierarchy =
    FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));
  Set_t result = NO_DEP_SET;

  if (use_cache) {
    nusmv_assert((hierarchy == mhierarchy));
    result = lookup_coi0_hash(coi0_hash, var);
  }

  if (NO_DEP_SET != result) return Set_Copy(result);

  {
    node_ptr invar_var = var;
    node_ptr init_var = find_node(nodemgr, SMALLINIT, var, Nil);
    node_ptr next_var = find_node(nodemgr, NEXT, var, Nil);

    node_ptr invar_a= FlatHierarchy_lookup_assign(hierarchy, invar_var);
    node_ptr invar_c= FlatHierarchy_lookup_constrains(hierarchy, invar_var);
    node_ptr init_a = FlatHierarchy_lookup_assign(hierarchy, init_var);
    node_ptr init_c = FlatHierarchy_lookup_constrains(hierarchy, init_var);
    node_ptr next_a = FlatHierarchy_lookup_assign(hierarchy, next_var);
    node_ptr next_c = FlatHierarchy_lookup_constrains(hierarchy, next_var);

    result = Set_MakeEmpty();

    /* Process normal assignments and INIT constraints */
    if (Nil != invar_a) {
      Set_t deps = Formula_GetDependenciesByType(st, invar_a, Nil, VFT_CNIF | VFT_FUNCTION, false);

      result = Set_Union(result, deps);
      Set_ReleaseSet(deps);
    }
    if (Nil != invar_c) {
      Set_t deps = Formula_GetDependenciesByType(st, invar_c, Nil, VFT_CNIF | VFT_FUNCTION, false);

      *nonassign = true;
      result = Set_Union(result, deps);
      Set_ReleaseSet(deps);
    }

    /* Process init assignments and INIT constraints */
    if (Nil != init_a) {
      Set_t deps = Formula_GetDependenciesByType(st, init_a, Nil, VFT_CNIF | VFT_FUNCTION, false);
      result = Set_Union(result, deps);
      Set_ReleaseSet(deps);
    }
    if (Nil != init_c) {
      Set_t deps = Formula_GetDependenciesByType(st, init_c, Nil, VFT_CNIF | VFT_FUNCTION, false);

      *nonassign = true;
      result = Set_Union(result, deps);
      Set_ReleaseSet(deps);
    }

    /* Process next assignments and TRANS constraints */
    if (Nil != next_a) {
      Set_t deps = Formula_GetDependenciesByType(st, next_a, Nil, VFT_CNIF | VFT_FUNCTION, false);
      result = Set_Union(result, deps);
      Set_ReleaseSet(deps);
    }
    if (Nil != next_c) {
      Set_t deps = Formula_GetDependenciesByType(st, next_c, Nil, VFT_CNIF | VFT_FUNCTION, false);

      *nonassign = true;
      result = Set_Union(result, deps);
      Set_ReleaseSet(deps);
    }
  }

  if (use_cache) {
    insert_coi0_hash(coi0_hash, var, result);
  }

  return result;
}

/*!
  \brief Free hash function for coi_hash

  This function has to be passed to
   SymbTable_get_handled_hash_ptr.

  \sa SymbTable_get_handled_hash_ptr
*/
static assoc_retval coi_hash_free(char *key, char *data, char * arg)
{
  Set_t element = (Set_t) data;

  /* free key */


  /* free value */
  if (((Set_t)NULL != element) && (NO_DEP_SET != element)) {
    Set_ReleaseSet(element);
  }

  return ASSOC_DELETE;
}

/*!
  \brief Free hash function for dependencies_hash

  This function has to be passed to
   SymbTable_get_handled_hash_ptr.

  \sa SymbTable_get_handled_hash_ptr
*/
static assoc_retval dependencies_hash_free(char *key, char *data, char * arg)
{
  Set_t element;

  /* free key */
  Tuple5_destroy((Tuple5_ptr)key);

  /* free value */
  element = (Set_t)data;

  if (element != (Set_t)NULL && element != BUILDING_DEP_SET &&
      element != EMPTY_DEP_SET) {
    Set_ReleaseSet(element);
  }

  return ASSOC_DELETE;
}

/*!
  \brief Free hash function for consts_hash

  This function has to be passed to
   SymbTable_get_handled_hash_ptr.

  \sa SymbTable_get_handled_hash_ptr
*/
static assoc_retval consts_hash_free(char *key, char *data, char * arg)
{
  Set_t element = (Set_t) data;

  if (element != (Set_t)NULL && element != EMPTY_DEP_SET) {
    Set_ReleaseSet(element);
  }
  return ASSOC_DELETE;
}

/*!
  \brief Free hash function for coi0_hash

  This function has to be passed to
   SymbTable_get_handled_hash_ptr.

  \sa SymbTable_get_handled_hash_ptr
*/
static assoc_retval coi0_hash_free(char *key, char *data, char * arg)
{
  Set_t element = (Set_t) data;

  if (((Set_t)NULL != element) && (NO_DEP_SET != element)) {
    Set_ReleaseSet(element);
  }
  return ASSOC_DELETE;
}

/*!
  \brief Make the hash key used by dependencies_hash

  The Tuple5 has to freed by the caller
*/
static void mk_hash_key(node_ptr e, node_ptr c, SymbFilterType filter,
                        boolean preserve_time, int time, Tuple5_ptr key)
{
  Tuple5_init(key,
              (void*)e,
              (void*)c,
              PTR_FROM_INT(void*, filter),
              PTR_FROM_INT(void*, preserve_time),
              PTR_FROM_INT(void*, time));

  Tuple5_freeze(key);
}

/*!
  \brief Insertion function for coi_hash


*/
static void insert_coi_hash(hash_ptr coi_hash, node_ptr key, Set_t value) {
  nusmv_assert(coi_hash != (hash_ptr)NULL);
  if (value != BUILDING_DEP_SET && value != NO_DEP_SET && value != NULL) {
    value = Set_Freeze(value);
    value = Set_Copy(value);
  }
  insert_assoc(coi_hash, key, (node_ptr)value);
}

/*!
  \brief Lookup function for coi_hash


*/
static Set_t lookup_coi_hash(hash_ptr coi_hash, node_ptr key) {
  nusmv_assert(coi_hash != (hash_ptr)NULL);
  return (Set_t) find_assoc(coi_hash, key);
}

/*!
  \brief Insertion function for consts hash


*/
static void insert_consts_hash(hash_ptr consts_hash, node_ptr key, Set_t value)
{
  nusmv_assert(consts_hash != (hash_ptr)NULL);
  if (value != NULL && value != EMPTY_DEP_SET) {
    value = Set_Copy(Set_Freeze(value));
  }
  insert_assoc(consts_hash, key, (node_ptr) value);
}

/*!
  \brief Lookup function for consts hash


*/
static Set_t lookup_consts_hash(hash_ptr consts_hash, node_ptr key)
{
  nusmv_assert(consts_hash != (hash_ptr)NULL);
  return (Set_t) find_assoc(consts_hash, key);
}

/*!
  \brief Insertion function for coi0_hash


*/
static void insert_coi0_hash(hash_ptr coi0_hash, node_ptr key, Set_t value) {
  nusmv_assert(coi0_hash != (hash_ptr)NULL);
  if (value != BUILDING_DEP_SET && value != NO_DEP_SET && value != NULL) {
    value = Set_Freeze(value);
    value = Set_Copy(value);
  }
  insert_assoc(coi0_hash, key, (node_ptr)value);
}

/*!
  \brief Lookup function for coi0_hash


*/
static Set_t lookup_coi0_hash(hash_ptr coi0_hash, node_ptr key) {
  nusmv_assert(coi0_hash != (hash_ptr)NULL);
  return (Set_t) find_assoc(coi0_hash, key);
}

/*!
  \brief Insertion function for dependencies_hash

  Take a Tuple5_ptr set with Tuple5_init() (memorized on
   the stack), allocates it and insert it in the hash table.
   Assumes the key not be already in the table.
*/
static void insert_dependencies_hash(hash_ptr dependencies_hash,
                                     node_ptr key, Set_t value) {
  nusmv_assert(dependencies_hash != (hash_ptr)NULL);
  nusmv_assert(lookup_dependencies_hash(dependencies_hash, key) == (Set_t)NULL);

  if (IS_VALID_SET(value)) {
    value = Set_Freeze(value);
    value = Set_Copy(value);
  }

  key =
    NODE_PTR(Tuple5_create(Tuple5_get_first(TUPLE_5(key)),
                           Tuple5_get_second(TUPLE_5(key)),
                           Tuple5_get_third(TUPLE_5(key)),
                           Tuple5_get_forth(TUPLE_5(key)),
                           Tuple5_get_fifth(TUPLE_5(key))));

  Tuple5_freeze(TUPLE_5(key));

  insert_assoc(dependencies_hash, key, NODE_PTR(value));
}

/*!
  \brief Insertion function for dependencies_hash

  To be called after the dependencies of the define
   represented by key has been full computed. Replace the placeholder
   BUILDING_DEP_SET with the result.
   Assumes the key to be already in the table. Take a Tuple5_ptr set with
   Tuple5_init() (memorized on the stack) and use it to replace the associated
   valued in the hash.
*/
static void close_define_dependencies_hash(hash_ptr dependencies_hash,
                                           node_ptr key, Set_t value) {
  nusmv_assert(dependencies_hash != (hash_ptr)NULL);
  nusmv_assert(lookup_dependencies_hash(dependencies_hash, key) ==
               BUILDING_DEP_SET);

  if (IS_VALID_SET(value)) {
    value = Set_Freeze(value);
    value = Set_Copy(value);
  }

  insert_assoc(dependencies_hash, key, NODE_PTR(value));
}

/*!
  \brief Lookup function for dependencies_hash


*/
static Set_t lookup_dependencies_hash(hash_ptr dependencies_hash,
                                      node_ptr key) {
  nusmv_assert(dependencies_hash != (hash_ptr)NULL);
  return (Set_t) find_assoc(dependencies_hash, key);
}

/*!
  \brief Call SymbTable_get_handled_hash_ptr with proper
                       arguments


*/
static hash_ptr compile_cone_get_handled_hash(SymbTable_ptr symb_table,
                                              char* hash_str) {

  if (! strcmp(ST_CONE_COI_HASH, hash_str)) {
    return
      SymbTable_get_handled_hash_ptr(symb_table,
                                     ST_CONE_COI_HASH,
                                     (ST_PFICPCP)NULL,
                                     (ST_PFICPI)NULL,
                                     coi_hash_free,
                                     (SymbTableTriggerFun)NULL,
                                     // Temporary bugfix for issue: 4168 (passing NULL) start
                                     (SymbTableTriggerFun)NULL,
                                     // SymbTable_clear_handled_remove_action_hash,
                                     // Temporary bugfix for issue: 4168 (passing NULL) end
                                     (SymbTableTriggerFun)NULL
                                     );
  }
  else if (! strcmp(ST_CONE_DEPENDENCIES_HASH, hash_str)) {
    return
      SymbTable_get_handled_hash_ptr(symb_table,
                                     ST_CONE_DEPENDENCIES_HASH,
                                     (ST_PFICPCP)Tuple5_compare,
                                     (ST_PFICPI)Tuple5_hash,
                                     dependencies_hash_free,
                                     (SymbTableTriggerFun)NULL,
                                     SymbTable_clear_handled_remove_action_hash,
                                     (SymbTableTriggerFun)NULL
                                     );
  }
  else if (! strcmp(ST_CONE_CONSTS_HASH,  hash_str)) {
    return
      SymbTable_get_handled_hash_ptr(symb_table,
                                     ST_CONE_CONSTS_HASH,
                                     (ST_PFICPCP)NULL,
                                     (ST_PFICPI)NULL,
                                     consts_hash_free,
                                     (SymbTableTriggerFun)NULL,
                                     SymbTable_clear_handled_remove_action_hash,
                                     (SymbTableTriggerFun)NULL
                                     );
  }
  else if (! strcmp(ST_CONE_COI0_HASH, hash_str)) {
    return
      SymbTable_get_handled_hash_ptr(symb_table,
                                     ST_CONE_COI0_HASH,
                                     (ST_PFICPCP)NULL,
                                     (ST_PFICPI)NULL,
                                     coi0_hash_free,
                                     (SymbTableTriggerFun)NULL,
                                     // Temporary bugfix for issue: 4168 (passing NULL)
                                     (SymbTableTriggerFun)NULL,
                                     // SymbTable_clear_handled_remove_action_hash,
                                     // Temporary bugfix for issue: 4168 (passing NULL) end
                                     (SymbTableTriggerFun)NULL
                                     );
  }
  else error_unreachable_code();
}
