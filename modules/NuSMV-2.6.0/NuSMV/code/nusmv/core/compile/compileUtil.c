/* ---------------------------------------------------------------------------


   This file is part of the ``compile'' package of NuSMV version 2.
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
  \brief Routines for model computation.

  This file contains the code for the compilation of the
   flattened hierarchy into BDD:
   <ul>
   <li> Creation of the boolean variables.</li>
   <li> Creation of the BDD representing the inertia of the system when
   there are processes. In fact when a process is running the
   other processes are stopped, and their state variables don't
   change.</li>
   <li> Creation of the BDD representing what does not change in the
   system, i.e. the set of invariance. These are introduced in the
   model by the keyword "<tt>INVAR</tt>" or by the <em>normal
   assignments</em> (i.e. "<tt>ASSIGN x : = y & z;</tt>"). These
   states are not stored in the transition relation, they are
   stored in an ad hoc variable.
   <li> Creation of the BDD representing the set of initial states.
   <li> Creation of the BDD representing the transition relation.
   Various ways of representing the transition relation are offered
   the users.
   <ul>
   <li> <em>Monolithic</em>: the monolithic transition relation is
   computed.</li>
   <li> <em>Conjunctive Partioned (Threshold)</em>: the transition
   relation is stored as an implicitly conjoined list of
   transition relation. This kind of partitioning can be
   used only if the model considered is a synchronous one.</li>
   <li> <em>Conjunctive Partioned IWLS95</em>: as the above, but the
   heuristic proposed in \[1\] is used to order partition clusters. </li>
   </ul>
   <li> Computes the fairness constraints. I.e. each fairness constraint
   (which can be a CTL formula) is evaluated and the resulting BDD
   is stored in the list <tt>fairness_constraints_bdd</tt> to be
   then used in the model checking phase.
   </ul>
   \[1\] R. K. Ranjan and A. Aziz and B. Plessier and C. Pixley and R. K. Brayton,
   "Efficient BDD Algorithms for FSM Synthesis and Verification,
   IEEE/ACM Proceedings International Workshop on Logic Synthesis,
   Lake Tahoe (NV), May 1995.</li>


*/

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"
#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/compile/compileUtil.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/WordNumberMgr.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/ustring.h"

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
typedef enum {
  State_Instantiation_Mode,
  Input_Instantiation_Mode
} Instantiation_Vars_Mode_Type;



/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static node_ptr compile_pop_distrib_ops_recurse(NodeMgr_ptr nodemgr,
                                                node_ptr prop);

static node_ptr compile_remove_ltl_bop_recurse(NodeMgr_ptr nodemgr,
                                               node_ptr prop);
/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

node_ptr sym_intern_from_ustring(const NuSMVEnv_ptr env,
                                 const string_ptr _string)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  return NodeMgr_find_node(nodemgr, ATOM, (node_ptr) _string, Nil);
}


node_ptr sym_intern(const NuSMVEnv_ptr env, const char* s)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  UStringMgr_ptr strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));

  return find_node(nodemgr, ATOM,
                   (node_ptr) UStringMgr_find_string(strings, s), Nil);
}


node_ptr Compile_Util_symbol_from_expr(NuSMVEnv_ptr const env,
                                       node_ptr const expr,
                                       const char* prefix,
                                       const char* suffix)
{
  NodeMgr_ptr const nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  MasterPrinter_ptr const wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  char* result_name;
  char* expr_string;
  size_t result_size;
  node_ptr result_var;
  boolean add_quotes = false;

  NUSMV_ENV_CHECK_INSTANCE(env);
  nusmv_assert(NULL != expr);
  nusmv_assert((char*)NULL != prefix);
  nusmv_assert((char*)NULL != suffix);

  expr_string = sprint_node(wffprint, expr);

  /* if expr is an identifier between quotes, and the user provides a prefix or
     a suffix, we must remove the quotes and add them after having concatenated
     the affixes */
  if ((0 != strcmp("", prefix) || 0 != strcmp("", suffix)) &&
      '"' == expr_string[0]) {
    size_t expr_string_len;
    char* tmp;

    /* remove the quotes */
    expr_string_len = strlen(expr_string);
    tmp = ALLOC(char, expr_string_len - 1);
    tmp = strncpy(tmp, expr_string + 1, expr_string_len - 2);
    tmp[expr_string_len - 2] = '\0';
    FREE(expr_string);
    expr_string = tmp;
    add_quotes = true;
  }

  result_size = strlen(prefix) + strlen(suffix) + strlen(expr_string) + 1;
  result_name = ALLOC(char, result_size);

  {
    int c = snprintf(result_name, result_size, "%s%s%s",
                     prefix, expr_string, suffix);
    SNPRINTF_CHECK(c, result_size);
  }

  result_var = sym_intern(env, result_name);

  FREE(result_name);

  /* If we removed the quotes, we must add them again */
  if (add_quotes) {
    result_var = Compile_Util_symbol_from_expr(env, result_var, "\"", "\"");
  }

  nusmv_assert(NULL != result_var);

  return result_var;
}

boolean sym_names_are_equal(const NuSMVEnv_ptr env,
                            node_ptr name1, node_ptr name2)
{
  if (name1 == name2) return true;
  if ((Nil == name1 && Nil != name2) || (Nil != name1 && Nil == name2)) {
    return false;
  }

  if (node_get_type(name1) != node_get_type(name2)) return false;

  switch (node_get_type(name1)) {
  case ATOM:
    return (car(name1) == car(name2) ||
            (0 == strcmp(UStringMgr_get_string_text((string_ptr) car(name1)),
                         UStringMgr_get_string_text((string_ptr) car(name2))))
            );

  case DOT:
    return (sym_names_are_equal(env, car(name1), car(name2)) &
            sym_names_are_equal(env, cdr(name1), cdr(name2)));

  default:
    ErrorMgr_internal_error(
                            ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER)),
                            "Invalid case %d in sym_names_are_equal", node_get_type(name1));
  }

  error_unreachable_code();
}


/*!
  \brief Private macros for the sake of readability of the function
   Compile_pop_global


*/

/* G a, AG a, H a */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define IS_AND_DISTRIB_OP(prop)                 \
  ((OP_GLOBAL == node_get_type(prop)) ||        \
   (AG == node_get_type(prop)) ||               \
   (OP_HISTORICAL == node_get_type(prop)))

/* F a, EF a, O a */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define IS_OR_DISTRIB_OP(prop)                  \
  ((OP_FUTURE == node_get_type(prop)) ||        \
   (EF == node_get_type(prop))        ||        \
   (OP_ONCE == node_get_type(prop)))

/*
 * G (a) & G (b) ------> G (a & b)
 * AG (a) & AG (b) ------> AG (a & b)
 * H (a) & H (b) ------> H (a & b)
 */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ARE_AND_DISTRIB_OPS(prop1, prop2)               \
  (IS_AND_DISTRIB_OP(prop1) &&                          \
   IS_AND_DISTRIB_OP(prop2) &&                          \
   node_get_type(prop1) == node_get_type(prop2))

/*
 * F (a) | F (b) ------> F (a | b)
 * EF (a) | EF (b) ------> EF (a | b)
 * O (a) | O (b) ------> O (a | b)
 */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ARE_OR_DISTRIB_OPS(prop1, prop2)                \
  (IS_OR_DISTRIB_OP(prop1) &&                           \
   IS_OR_DISTRIB_OP(prop2) &&                           \
   node_get_type(prop1) == node_get_type(prop2))

node_ptr Compile_pop_distrib_ops(const NuSMVEnv_ptr env, node_ptr prop) {
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr result;

  result = compile_pop_distrib_ops_recurse(nodemgr, prop);

  if (opt_verbose_level_gt(opts, 6)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    if (prop == result) {
      Logger_log(logger, "-- No simplification occured\n");
    }
    else {
      Logger_nlog(logger, wffprint,
                  "-- The simplified formula is: \"%N\"\n", result);
    }
  }
  return result;
}


node_ptr Compile_remove_ltl_bop(const NuSMVEnv_ptr env, node_ptr prop) {
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr result;

  result = compile_remove_ltl_bop_recurse(nodemgr, prop);

  if (opt_verbose_level_gt(opts, 6)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    if (prop == result) {
      Logger_log(logger, "-- No bounded operators have been rewritten\n");
    }
    else {
      Logger_nlog(logger, wffprint,
                  "-- The rewritten formula is: \"%N\"\n", result);
    }
  }
  return result;
}


Set_t
Compile_make_sorted_vars_list_from_order(const SymbTable_ptr st,
                                         const NodeList_ptr vars,
                                         const NodeList_ptr vars_order)
{
  Set_t res;
  ListIter_ptr iter;

  res = Set_MakeEmpty();

  /* pushes all the names in vars_order that belong to the intersection
     of vars and vars_order */
  NODE_LIST_FOREACH(vars_order, iter) {
    ResolveSymbol_ptr rs;
    node_ptr name;

    rs = SymbTable_resolve_symbol(st,
                                  NodeList_get_elem_at(vars_order, iter), Nil);
    name = ResolveSymbol_get_resolved_name(rs);

    if (NodeList_belongs_to(vars, name)) {
      res = Set_AddMember(res, (Set_Element_t) name);
    }
  }

  /* pushes all the remaining symbols at the end of the resulting list */
  NODE_LIST_FOREACH(vars, iter) {
    ResolveSymbol_ptr rs;
    node_ptr name;

    rs = SymbTable_resolve_symbol(st,
                                  NodeList_get_elem_at(vars, iter), Nil);
    name = ResolveSymbol_get_resolved_name(rs);

    res = Set_AddMember(res, name);
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
  \brief


*/
static node_ptr compile_pop_distrib_ops_recurse(NodeMgr_ptr nodemgr,
                                                node_ptr prop)
{
  if ((node_ptr) NULL == prop) return (node_ptr) NULL;

  /* Base cases */
  switch (node_get_type(prop)) {
  case FAILURE:
  case TRUEEXP:
  case FALSEEXP:
  case SELF:
  case BOOLEAN:
  case ATOM:
  case DOT:
  case ARRAY:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case UWCONST: case SWCONST:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case BIT:
    return prop;

  case CONTEXT:
    return find_node(nodemgr, CONTEXT, car(prop), compile_pop_distrib_ops_recurse(nodemgr, cdr(prop)));

  default: break;
  }

  /* 1) <OP> <OP> a :-> <OP> a */
  if (ARE_AND_DISTRIB_OPS(prop, car(prop)) ||
      ARE_OR_DISTRIB_OPS(prop, car(prop))) {
    return compile_pop_distrib_ops_recurse(nodemgr, car(prop));
  }

  /* 2) (<OP> a) * (<OP> b) :-> <OP> (a * b); */
  if (AND == node_get_type(prop) ||
      OR == node_get_type(prop)) {
    node_ptr l = compile_pop_distrib_ops_recurse(nodemgr, car(prop));
    node_ptr r = compile_pop_distrib_ops_recurse(nodemgr, cdr(prop));

    if ((ARE_AND_DISTRIB_OPS(l, r) && AND == node_get_type(prop)) ||
        (ARE_OR_DISTRIB_OPS(l, r) && OR == node_get_type(prop))) {
      return compile_pop_distrib_ops_recurse(nodemgr, find_node(nodemgr, node_get_type(l),
                                                                find_node(nodemgr, node_get_type(prop), car(l), car(r)), Nil));
    }
    else return find_node(nodemgr, node_get_type(prop), l, r);
  }

  /* 3) (<OP> (a * <OP> b))   :-> <OP> (a * b);
     4) (<OP> (<OP> a * b))   :-> <OP> (a * b);
     5) (<OP> (<OP> a * <OP> b)) :-> <OP> (a * b); */

  if (IS_AND_DISTRIB_OP(prop) || IS_OR_DISTRIB_OP(prop)) {
    int op = node_get_type(car(prop));
    if (AND == op || OR == op) {
      node_ptr l = compile_pop_distrib_ops_recurse(nodemgr, car(car(prop)));
      node_ptr r = compile_pop_distrib_ops_recurse(nodemgr, cdr(car(prop)));

      if ( ((AND == op) && ARE_AND_DISTRIB_OPS(prop, l) &&
            ARE_AND_DISTRIB_OPS(prop, r))
           ||
           ((OR == op) && ARE_OR_DISTRIB_OPS(prop, l) &&
            ARE_OR_DISTRIB_OPS(prop, r)) ) { /* 5 */

        return compile_pop_distrib_ops_recurse(nodemgr, find_node(nodemgr, node_get_type(prop),
                                                                  find_node(nodemgr, op, car(l), car(r)), Nil));
      }
      else if ( ((AND == op) && ARE_AND_DISTRIB_OPS(prop, l))
                ||
                ((OR == op) && ARE_OR_DISTRIB_OPS(prop, l)) ) { /* 4 */
        return compile_pop_distrib_ops_recurse(nodemgr, find_node(nodemgr, node_get_type(prop),
                                                                  find_node(nodemgr, op, car(l), r), Nil));
      }
      else if ( ((AND == op) && ARE_AND_DISTRIB_OPS(prop, r))
                ||
                ((OR == op) && ARE_OR_DISTRIB_OPS(prop, r)) ) { /* 3 */
        return compile_pop_distrib_ops_recurse(nodemgr, find_node(nodemgr, node_get_type(prop),
                                                                  find_node(nodemgr, op, l, car(r)), Nil));
      }
      return find_node(nodemgr, node_get_type(prop), find_node(nodemgr, op, l, r), Nil);
    }
  }

  /* fall back case */
  return find_node(nodemgr, node_get_type(prop),
                   compile_pop_distrib_ops_recurse(nodemgr, car(prop)),
                   compile_pop_distrib_ops_recurse(nodemgr, cdr(prop)));
}



/*!
  \brief


*/
static node_ptr compile_remove_ltl_bop_recurse(NodeMgr_ptr nodemgr,
                                               node_ptr prop)
{
  if ((node_ptr) NULL == prop) return (node_ptr) NULL;

  /* Base cases */
  switch (node_get_type(prop)) {
  case FAILURE:
  case TRUEEXP:
  case FALSEEXP:
  case SELF:
  case BOOLEAN:
  case ATOM:
  case DOT:
  case ARRAY:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case UWCONST: case SWCONST:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case BIT:
    return prop;

  case CONTEXT:
    return find_node(nodemgr, CONTEXT, car(prop), compile_remove_ltl_bop_recurse(nodemgr, cdr(prop)));
  case OP_GLOBAL:
  case OP_FUTURE:
  case OP_ONCE:
  case OP_HISTORICAL:
    /* The temporal operator has a bound to be expanded */
    if (Nil != cdr(prop)) {
      node_ptr condition = compile_remove_ltl_bop_recurse(nodemgr, car(prop));
      node_ptr nt1 = car(cdr(prop));
      node_ptr nt2 = cdr(cdr(prop));
      node_ptr result = condition;
      int OP = -1, TOP = -1;
      int t1, t2, j;

      nusmv_assert(TWODOTS == node_get_type(cdr(prop)));

      t1 = node_get_int(nt1);
      t2 = node_get_int(nt2);

      nusmv_assert(t1 >= 0);
      nusmv_assert(t2 >= 0);
      nusmv_assert(t1 <= t2);

      OP = ((OP_FUTURE == node_get_type(prop)) ||
            (OP_ONCE == node_get_type(prop))) ? OR : AND;
      TOP = ((OP_FUTURE == node_get_type(prop)) ||
             (OP_GLOBAL == node_get_type(prop))) ? OP_NEXT : OP_PREC;

      for (j = t2 - t1; j > 0; j--) {
        /* C OP TOP acc */
        result = find_node(nodemgr, TOP, result, Nil);
        result = find_node(nodemgr, OP, condition, result);
      }
      /* TOP^{t1} (body) */
      for (j = t1; j > 0; j--) {
        result = find_node(nodemgr, TOP, result, Nil);
      }
      return result;
    }
    break;
  default:
    break;
  }

  /* fall back case */
  return find_node(nodemgr, node_get_type(prop),
                   compile_remove_ltl_bop_recurse(nodemgr, car(prop)),
                   compile_remove_ltl_bop_recurse(nodemgr, cdr(prop)));
}
