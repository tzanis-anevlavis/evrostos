/* ---------------------------------------------------------------------------


  This file is part of the ``fsm.sexp'' package of NuSMV version 2.
  Copyright (C) 2009 by FBK-irst.

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
  \brief The SexpInliner implementation

  \todo: Missing description

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/wff/ExprMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/sexp/SexpInliner.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/compile/PredicateNormaliser.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/EnvObject.h"
#include "nusmv/core/utils/EnvObject_private.h"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define IN_MOD
#ifdef IN_MOD
  #include "nusmv/core/utils/Olist.h"
#endif

/*---------------------------------------------------------------------------*/
/* Constants declarations                                                    */
/*---------------------------------------------------------------------------*/

/* Uncomment the following line to enable debugging of the
   substitutions */
/* #derfine _DEBUG_SUBST */

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct SexpInliner_TAG {

  INHERITS_FROM(EnvObject);

  SymbTable_ptr st;
  hash_ptr var2expr;     /* substitution hash var -> expr (forced) */
  hash_ptr var2invar;    /* substitution hash var -> invar (forced) */
  Set_t invars;          /* the set of forced invariants */
  size_t fixpoint_limit; /* limit for fixpoint computation in subst */
  Set_t blacklist;       /* name of symbols not to be learn nor substituted */

  hash_ptr hash_timed2leaves; /* hash used in memoization when
                                 bringing timed nodes to leaves */
  hash_ptr hash_subst; /* hash used in memoization of substitution */

  hash_ptr hash_extract_equals_invars; /* hash to memoize the results of
                                          sexp_inliner_extract_equals_invars */
  hash_ptr hash_is_expr_deterministic; /* hash to memoize the results of
                                          sexp_inliner_is_expr_deterministic */
} SexpInliner;

typedef struct InlineRes_TAG {
  NuSMVEnv_ptr env;
  Expr_ptr orig;
  Expr_ptr inlined;
  Set_t equivs;
  Set_t invars;

} InlineRes;


struct SexpInlinerExtractEqualsInfo {
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;
  NodeMgr_ptr nodemgr;
  ErrorMgr_ptr errmgr;
  ExprMgr_ptr exprmgr;
  Logger_ptr logger;
  MasterPrinter_ptr wffprint;

  Set_t equals;
  Set_t invars;
  Set_t vars;
};

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

#define SEXP_INLINER_EXPAND_DEFINES 1


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void sexp_inliner_init(SexpInliner_ptr self,
                              SymbTable_ptr st,
                              const size_t fixpoint_limit);

static void sexp_inliner_deinit(SexpInliner_ptr self);

static void sexp_inliner_finalize(Object_ptr o, void* dummy);

static void sexp_inliner_copy(const SexpInliner_ptr self,
                              SexpInliner_ptr copy);

static node_ptr
sexp_inliner_move_time_to_leaves(const SexpInliner_ptr self,
                                 node_ptr expr, int time);

static node_ptr
sexp_inliner_extract_equals_invars(const SexpInliner_ptr self,
                                   struct SexpInlinerExtractEqualsInfo* info,
                                   node_ptr expr,
                                   boolean is_neg);

static node_ptr
sexp_try_acquiring_equality(const SexpInliner_ptr self,
                            struct SexpInlinerExtractEqualsInfo* info,
                            node_ptr expr,
                            boolean is_neg);

static array_t*
sexp_inliner_extract_candidate_equals(const SexpInliner_ptr self,
                                      const Set_t equals,
                                      const Set_t imp_vars,
                                      const hash_ptr var2invar,
                                      Set_t* rem_equals);

static hash_ptr
sexp_inliner_remove_loops(const SexpInliner_ptr self,
                          array_t* good_equals,
                          hash_ptr hash_invars,
                          Set_t* good, Set_t* rem);

static node_ptr
sexp_inliner_substitute(SexpInliner_ptr self, node_ptr expr,
                        hash_ptr var2expr, hash_ptr var2invar,
                        boolean* changed);

static boolean
sexp_inliner_force_equivalence(SexpInliner_ptr self,
                               node_ptr var, Expr_ptr expr);

static boolean
sexp_inliner_force_invariant(SexpInliner_ptr self,
                             node_ptr var, Expr_ptr expr);

static boolean
sexp_inliner_expr_is_var(const SexpInliner_ptr self, node_ptr expr);

static InlineRes_ptr inline_res_create(const NuSMVEnv_ptr env, Expr_ptr orig);
static void inline_res_deinit(InlineRes_ptr self);

static int sexp_inliner_expr_ptr_compare(const void * c1, const void * c2);

static void sexp_inliner_free_equalities_array(NodeMgr_ptr nodemgr,
                                               array_t* arr);

static boolean
sexp_inliner_is_expr_deterministic(const SexpInliner_ptr self,
                                   node_ptr expr);

static boolean sexp_inliner_is_expr_timed(node_ptr expr);

static Expr_ptr sexp_inliner_assign_to_setin(SexpInliner_ptr self,
                                             Expr_ptr assign);


static void
SexpInlinerExtractEqualsInfo_init(struct SexpInlinerExtractEqualsInfo* info,
                                  const NuSMVEnv_ptr env);

static void
SexpInlinerExtractEqualsInfo_quit(struct SexpInlinerExtractEqualsInfo* info);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

SexpInliner_ptr SexpInliner_create(SymbTable_ptr st,
                                   const size_t fixpoint_limit)
{
  SexpInliner_ptr self;

  /* allocation: */
  self = ALLOC(SexpInliner, 1);
  SEXP_INLINER_CHECK_INSTANCE(self);

  /* initialization: */
  sexp_inliner_init(self, st, fixpoint_limit);
  return self;
}

SexpInliner_ptr SexpInliner_copy(const SexpInliner_ptr self)
{
  SexpInliner_ptr copy;

  SEXP_INLINER_CHECK_INSTANCE(self);

  copy = ALLOC(SexpInliner, 1);
  SEXP_INLINER_CHECK_INSTANCE(copy);

  sexp_inliner_copy(self, copy);
  return copy;
}

void SexpInliner_destroy(SexpInliner_ptr self)
{
  SEXP_INLINER_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

void SexpInliner_clear_equivalences(SexpInliner_ptr self)
{
  SEXP_INLINER_CHECK_INSTANCE(self);
  clear_assoc(self->var2expr);
  clear_assoc(self->hash_subst);
}

void SexpInliner_clear_invariants(SexpInliner_ptr self)
{
  SEXP_INLINER_CHECK_INSTANCE(self);
  clear_assoc(self->var2invar);

  Set_ReleaseSet(self->invars);
  self->invars = Set_MakeEmpty();
  clear_assoc(self->hash_subst);
}

void SexpInliner_clear_blacklist(SexpInliner_ptr self)
{
  SEXP_INLINER_CHECK_INSTANCE(self);

  Set_ReleaseSet(self->blacklist);
  self->blacklist = Set_MakeEmpty();
  clear_assoc(self->hash_subst);
}

SymbTable_ptr SexpInliner_get_symb_table(const SexpInliner_ptr self)
{
  SEXP_INLINER_CHECK_INSTANCE(self);
  return self->st;
}

hash_ptr SexpInliner_get_var2expr_hash(const SexpInliner_ptr self)
{
  SEXP_INLINER_CHECK_INSTANCE(self);
  return self->var2expr;
}

hash_ptr SexpInliner_get_var2invar_hash(const SexpInliner_ptr self)
{
  SEXP_INLINER_CHECK_INSTANCE(self);
  return self->var2invar;
}

boolean SexpInliner_force_equivalence(SexpInliner_ptr self,
                                      node_ptr var, Expr_ptr expr)
{
  node_ptr expr2;
  boolean res;

  SEXP_INLINER_CHECK_INSTANCE(self);

  expr2 = sexp_inliner_move_time_to_leaves(self, expr, EXPR_UNTIMED_CURRENT);
  res = sexp_inliner_force_equivalence(self, var, expr2);
  if (res) clear_assoc(self->hash_subst);
  return res;
}

boolean SexpInliner_force_equivalences(SexpInliner_ptr self, Set_t equivs)
{
  boolean res = false;
  Set_Iterator_t iter;

  SEXP_INLINER_CHECK_INSTANCE(self);

  SET_FOREACH(equivs, iter) {
    node_ptr eq = Set_GetMember(equivs, iter);
    nusmv_assert(EQUAL == node_get_type(eq) || IFF == node_get_type(eq) ||
                 EQDEF == node_get_type(eq));

    res |= SexpInliner_force_equivalence(self, car(eq), cdr(eq));
  }

  return res;
}

boolean SexpInliner_force_invariant(SexpInliner_ptr self,
                                    node_ptr var, Expr_ptr expr)
{
  boolean res;
  node_ptr expr2;
  SEXP_INLINER_CHECK_INSTANCE(self);

  nusmv_assert(SymbTable_is_symbol_var(self->st, var));

  expr2 = sexp_inliner_move_time_to_leaves(self, expr, EXPR_UNTIMED_CURRENT);
  res = sexp_inliner_force_invariant(self, var, expr2);
  if (res) clear_assoc(self->hash_subst);
  return res;
}

boolean SexpInliner_force_invariants(SexpInliner_ptr self, Set_t invars)
{
  boolean res = false;
  Set_Iterator_t iter;

  SEXP_INLINER_CHECK_INSTANCE(self);

  SET_FOREACH(invars, iter) {
    node_ptr invar = Set_GetMember(invars, iter);
    nusmv_assert( EQUAL == node_get_type(invar) ||
                  IFF == node_get_type(invar) ||
                  /* an invariant assignment */
                  (EQDEF == node_get_type(invar) &&
                   SMALLINIT != node_get_type(car(invar)) &&
                   NEXT != node_get_type(car(invar))) );

    res |= SexpInliner_force_invariant(self, car(invar), cdr(invar));
  }

  return res;
}

void SexpInliner_blacklist_name(SexpInliner_ptr self, node_ptr var)
{
  SEXP_INLINER_CHECK_INSTANCE(self);
  nusmv_assert(SymbTable_is_symbol_var(self->st, var));

  self->blacklist = Set_AddMember(self->blacklist, (Set_Element_t) var);
  clear_assoc(self->hash_subst);
}

InlineRes_ptr SexpInliner_inline(SexpInliner_ptr self, Expr_ptr expr,
                                 boolean* changed)
{
  NuSMVEnv_ptr env;
  struct SexpInlinerExtractEqualsInfo info;

  size_t counter;
  node_ptr psi;
  Set_t kept_equals;
  boolean _changed; /* local changed */

  SEXP_INLINER_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  SexpInlinerExtractEqualsInfo_init(&info, env);

  if (opt_verbose_level_gt(info.opts, 5)) {
    Logger_log(info.logger, "SexpInliner: Applying inlining...\n");
  }

  /* 1. moves timed nodes to the leaves */
  psi = sexp_inliner_move_time_to_leaves(self, expr, EXPR_UNTIMED_CURRENT);

  /* these two are accumulated out of psi */
  kept_equals = Set_MakeEmpty();

  /* enters the fixpoint loop */
  counter = 0;
  do {
    array_t* good_equals;
    hash_ptr hash_equals, hash_invars;
    Set_t rem_equals;

    /* 10. extracts equals and set of involved vars incrementally */
    psi = sexp_inliner_extract_equals_invars(self, &info, psi, false);

    if (opt_verbose_level_gt(info.opts, 7)) {
      Logger_nlog(info.logger, info.wffprint,
                  "\nSexpInliner: before inlining "
                  "(before re-introduction):\n%N", psi);
    }

    /* this is the set of equivalences to be re-introduced in psi later */
    rem_equals = Set_MakeEmpty();

    { /* 12. extract hash out of auto-extracted invars set */
      Set_Iterator_t invars_iter;
      hash_invars = new_assoc();
      SET_FOREACH(info.invars, invars_iter) {
        node_ptr invar = (node_ptr)
            Set_GetMember(info.invars, invars_iter);
        /* extracted invars are always assign */
        nusmv_assert(EQDEF == node_get_type(invar));

        /* multiple invariants for the same var */
        if ((node_ptr) NULL == find_assoc(hash_invars, car(invar))) {
          /* there is no existing extracted invariant about this
             variable (this is always true with well-formed
             expressions coming from the compiler, but not true in
             general with hand-made formulae) */
          insert_assoc(hash_invars, car(invar), cdr(invar));
        }
        else {
          /* a multiple invariant assignment for the same
             variable. This is not a well-formed expression, but it
             may happen in hand-made expressions. In this case the
             expression is kept in psi, transformed to get inlined
             later. */
          rem_equals = Set_AddMember(
              rem_equals, sexp_inliner_assign_to_setin(self, invar));
        }
      }
    }

    /* 14. extracts unique equals sorted array and remaining set */
    good_equals = sexp_inliner_extract_candidate_equals(self,
                                                        info.equals,
                                                        info.vars,
                                                        hash_invars,
                                                        &rem_equals);

    /* 16. removes loops from good_equals, obtaining kep_equals and
       possibly growing rem_equals with equals causing loops */
    hash_equals = sexp_inliner_remove_loops(self, good_equals, hash_invars,
                                            &kept_equals, &rem_equals);

    /* prints out the remaining set */
    if (opt_verbose_level_gt(info.opts, 6)) {
      Set_Iterator_t iter;
      Logger_log(info.logger, "\nSexpInliner: re-introduced equals are:\n");
      SET_FOREACH(rem_equals, iter) {
        Logger_nlog(info.logger, info.wffprint, "%N",
                    (node_ptr) Set_GetMember(rem_equals, iter));
        Logger_log(info.logger, "\n");
      }
    }

    { /* accumulates the remaining equalitites and conjuncts it to
         psi, as they may reveal new equalities. For example if
         rem_equals contained v1 <-> (v2 -> v3 = t2) and for some
         reason this got simplified to v3 = t2 */
      Set_Iterator_t iter;
      SET_FOREACH(rem_equals, iter) {
        psi = ExprMgr_and(info.exprmgr, psi,
                          (node_ptr) Set_GetMember(rem_equals, iter));
      }
    }

    if (opt_verbose_level_gt(info.opts, 7)) {
      Logger_nlog(info.logger, info.wffprint,
                  "\nSexpInliner: before inlining "
                  "(after re-introduction):\n%N", psi);
    }

    /* 20. substitutes the assignments into the expression */
    _changed = false;
    psi = sexp_inliner_substitute(self, psi, hash_equals, hash_invars,
                                  &_changed);

    /* marks if changed */
    if (_changed && (boolean*) NULL != changed)
      *changed = _changed;

    /* 22. gets rid of all no-longer useful things */
    Set_ReleaseSet(rem_equals);
    free_assoc(hash_invars);
    free_assoc(hash_equals);
    sexp_inliner_free_equalities_array(info.nodemgr, good_equals);
    array_free(good_equals);

    /* 24. increments the counter if needed */
    if (self->fixpoint_limit > 0)
      counter += 1;

  /* Handling of the end of fixpoint loop. */
  } while (_changed && counter <= self->fixpoint_limit);

  if (opt_verbose_level_gt(info.opts, 7)) {
    Set_Iterator_t iter;

    Logger_nlog(info.logger, info.wffprint,
                "\nSexpInliner: Inlined expression was:\n%N"
                "\nSexpInliner: After inlining is:\n%N"
                "\nSexpInliner: Kept equalities are:\n", expr, psi);
    SET_FOREACH(kept_equals, iter) {
      Logger_nlog(info.logger, info.wffprint, "%N\n",
                  (node_ptr) Set_GetMember(kept_equals, iter));
    }

    Logger_log(info.logger, "\nSexpInliner: Extracted invariants are:\n");
    SET_FOREACH(info.invars, iter) {
      Logger_nlog(info.logger, info.wffprint, "%N\n",
                 (node_ptr) Set_GetMember(info.invars, iter));
    }
  }

  { /* constructs the result */
    InlineRes_ptr res = inline_res_create(env, expr);

    /* the inlined expression */
    res->inlined = psi;

    /* retrieves the equivalences */
    res->equivs = Set_Union(res->equivs, kept_equals);

    /* retrieves the invariants */
    res->invars = Set_Union(Set_Union(res->invars, info.invars),
                            self->invars);

    Set_ReleaseSet(kept_equals);
    SexpInlinerExtractEqualsInfo_quit(&info);

    return res;
  }
}

Expr_ptr SexpInliner_inline_no_learning(SexpInliner_ptr self, Expr_ptr expr,
                                        boolean* changed)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));


  size_t counter;
  Expr_ptr psi;
  hash_ptr hash_equals;
  boolean _changed;

  SEXP_INLINER_CHECK_INSTANCE(self);

  if (opt_verbose_level_gt(opts, 5)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "\nSexpInliner: Applying inlining (no learning)...\n");
  }

  /* 0. resets change variable */
  if ((boolean*) NULL != changed)
    *changed = false;

  /* 1. moves timed nodes to the leaves */
  psi = sexp_inliner_move_time_to_leaves(self, expr, EXPR_UNTIMED_CURRENT);

  /* this is a dummy, will be not used by the inliner, as only
     forced equivalences and invariants will be used */
  hash_equals = new_assoc();

  /* 3. enters the fixpoint loop */
  counter = 0;
  do {
    /* this controls the exit from the loop */
    _changed = false;

    /* performs the smooth inlining */
    psi = sexp_inliner_substitute(self, psi,
                                  hash_equals,
                                  (hash_ptr) NULL /* no extracted invars */,
                                  &_changed);

    /* marks if changed */
    if (_changed && (boolean*) NULL != changed)
      *changed = _changed;

    /* increments the counter if needed */
    if (self->fixpoint_limit > 0)
      counter += 1;

    if (opt_verbose_level_gt(opts, 6)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "SexpInliner: Done inlining iteration...\n");
    }

  /* Handling of the end of fixpoint loop. */
  } while (_changed && counter <= self->fixpoint_limit);

  free_assoc(hash_equals);

  if (opt_verbose_level_gt(opts, 7)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_nlog(logger, wffprint,
                "\nSexpInliner: Inlined expression was:\n%N"
                "\nSexpInliner: After inlining is:\n%N\n", expr, psi);
  }

  return psi;
}


void InlineRes_destroy(InlineRes_ptr self)
{
  INLINE_RES_CHECK_INSTANCE(self);
  inline_res_deinit(self);
  FREE(self);
}

Expr_ptr InlineRes_get_result(const InlineRes_ptr self)
{
  const ExprMgr_ptr exprs =
      EXPR_MGR(NuSMVEnv_get_value(self->env, ENV_EXPR_MANAGER));

  INLINE_RES_CHECK_INSTANCE(self);
  return ExprMgr_and(exprs, InlineRes_get_inlined_expr(self),
                  ExprMgr_and(exprs, InlineRes_get_equivalences_expr(self),
                           InlineRes_get_invariant_expr(self)));
}

Expr_ptr InlineRes_get_result_unique(const InlineRes_ptr self)
{
  array_t* arr;
  node_ptr iter;
  Expr_ptr sorted_expr;
  ExprMgr_ptr exprs;

  INLINE_RES_CHECK_INSTANCE(self);

  exprs = EXPR_MGR(NuSMVEnv_get_value(self->env, ENV_EXPR_MANAGER));

  arr = array_alloc(Expr_ptr, 8);

  /* accumulates equivalences */
  for (iter=InlineRes_get_equivalences_expr(self);
       (node_ptr) NULL != iter; iter=cdr(iter)) {
    if (AND != node_get_type(iter)) {
      array_insert_last(Expr_ptr, arr, iter);
      break;
    }
    else array_insert_last(Expr_ptr, arr, car(iter));
  }

  /* accumulates invariants */
  for (iter=InlineRes_get_invariant_expr(self);
       (node_ptr) NULL != iter; iter=cdr(iter)) {
    if (AND != node_get_type(iter)) {
      array_insert_last(Expr_ptr, arr, iter);
      break;
    }
    else array_insert_last(Expr_ptr, arr, car(iter));
  }

  sorted_expr = ExprMgr_true(exprs);

  { /* sorts and produces a unique expression */
    Expr_ptr conj;
    int j;
    array_sort(arr, sexp_inliner_expr_ptr_compare);
    arrayForEachItem(Expr_ptr, arr, j, conj) {
      sorted_expr = ExprMgr_and_nil(exprs, sorted_expr, conj);
    }
  }
  array_free(arr);

  return ExprMgr_and(exprs, InlineRes_get_inlined_expr(self), sorted_expr);
}

Expr_ptr InlineRes_get_original_expr(const InlineRes_ptr self)
{
  ExprMgr_ptr exprs;

  INLINE_RES_CHECK_INSTANCE(self);

  exprs = EXPR_MGR(NuSMVEnv_get_value(self->env, ENV_EXPR_MANAGER));

  return ((node_ptr) NULL != self->orig) ? self->orig : ExprMgr_true(exprs);
}

Expr_ptr
InlineRes_get_inlined_expr(const InlineRes_ptr self)
{
  ExprMgr_ptr exprs;

  INLINE_RES_CHECK_INSTANCE(self);

  exprs = EXPR_MGR(NuSMVEnv_get_value(self->env, ENV_EXPR_MANAGER));

  return ((node_ptr) NULL != self->inlined) ?
      self->inlined : ExprMgr_true(exprs);
}

Expr_ptr InlineRes_get_equivalences_expr(const InlineRes_ptr self)
{
  Expr_ptr res;
  Set_Iterator_t iter;
  ExprMgr_ptr exprs;

  INLINE_RES_CHECK_INSTANCE(self);

  exprs = EXPR_MGR(NuSMVEnv_get_value(self->env, ENV_EXPR_MANAGER));

  res = ExprMgr_true(exprs);
  SET_FOREACH(self->equivs, iter) {
    res = ExprMgr_and_nil(exprs, res,
                          (Expr_ptr) Set_GetMember(self->equivs, iter));
  }
  return res;
}

Set_t InlineRes_get_equivalences(const InlineRes_ptr self)
{
  INLINE_RES_CHECK_INSTANCE(self);
  return self->equivs;
}

Expr_ptr InlineRes_get_invariant_expr(const InlineRes_ptr self)
{
  Expr_ptr res;
  Set_Iterator_t iter;
  ExprMgr_ptr exprs;

  INLINE_RES_CHECK_INSTANCE(self);

  exprs = EXPR_MGR(NuSMVEnv_get_value(self->env, ENV_EXPR_MANAGER));

  res = ExprMgr_true(exprs);
  SET_FOREACH(self->invars, iter) {
    res = ExprMgr_and_nil(exprs, res,
                          (Expr_ptr) Set_GetMember(self->invars, iter));
  }
  return res;
}

Set_t InlineRes_get_invariants(const InlineRes_ptr self)
{
  INLINE_RES_CHECK_INSTANCE(self);
  return self->invars;
}


/*---------------------------------------------------------------------------*/
/* Static function definitions                                               */
/*---------------------------------------------------------------------------*/

/*!
  \brief Initializes either the boolean or scalar sexp fsm

  hierarchy is copied into an independent FlatHierarchy
  instance. If the new sexp must be based only on a set of variables, the
  hierarchy must be empty
*/
static void sexp_inliner_init(SexpInliner_ptr self,
                              SymbTable_ptr st, const size_t fixpoint_limit)
{
  env_object_init(ENV_OBJECT(self), EnvObject_get_environment(ENV_OBJECT(st)));

  /* inits some private members */
  self->st = st;
  self->var2expr = new_assoc();
  self->var2invar = new_assoc();
  self->invars = Set_MakeEmpty();
  self->fixpoint_limit = fixpoint_limit;
  self->blacklist = Set_MakeEmpty();
  self->hash_timed2leaves = new_assoc();
  self->hash_subst = new_assoc();
  self->hash_extract_equals_invars = new_assoc();
  self->hash_is_expr_deterministic = new_assoc();

  OVERRIDE(Object, finalize) = sexp_inliner_finalize;
}

/*!
  \brief SexpInliner class virtual finalizer


*/
static void sexp_inliner_finalize(Object_ptr object, void* dummy)
{
  SexpInliner_ptr self = SEXP_INLINER(object);

  sexp_inliner_deinit(self);

  FREE(self);
}

/*!
  \brief Class deinitializer


*/
static void sexp_inliner_deinit(SexpInliner_ptr self)
{
  free_assoc(self->hash_is_expr_deterministic);
  self->hash_is_expr_deterministic = (hash_ptr) NULL;

  free_assoc(self->hash_extract_equals_invars);
  self->hash_extract_equals_invars = (hash_ptr) NULL;

  free_assoc(self->hash_subst);
  self->hash_subst = (hash_ptr) NULL;

  free_assoc(self->hash_timed2leaves);
  self->hash_timed2leaves = (hash_ptr) NULL;

  if ((hash_ptr) NULL != self->var2expr) {
    free_assoc(self->var2expr);
    self->var2expr = (hash_ptr) NULL;
  }

  if ((hash_ptr) NULL != self->var2invar) {
    free_assoc(self->var2invar);
    self->var2invar = (hash_ptr) NULL;
  }

  Set_ReleaseSet(self->invars);
  self->invars = (Set_t) NULL;

  Set_ReleaseSet(self->blacklist);
  self->blacklist = (Set_t) NULL;
}

/*!
  \brief Copies members from self to copy


*/
static void sexp_inliner_copy(const SexpInliner_ptr self, SexpInliner_ptr copy)
{
  /* inits some private members */
  copy->fixpoint_limit = self->fixpoint_limit;
  copy->st = self->st;

  if (self->var2expr != (hash_ptr) NULL) {
    copy->var2expr = copy_assoc(self->var2expr);
  } else copy->var2expr = (hash_ptr) NULL;

  if (self->var2invar != (hash_ptr) NULL) {
    copy->var2invar = copy_assoc(self->var2invar);
  } else copy->var2invar = (hash_ptr) NULL;

  copy->invars = Set_Copy(self->invars);
  copy->blacklist = Set_Copy(self->blacklist);
}

/*!
  \brief Brings all time-related nodes (ATTIME and NEXT) down to the
                 leaves, while expanding defines and formal parameters

  internal self->hash_timed2leaves is used for memoization

  \se self->hash_timed2leaves is changed
*/
static node_ptr sexp_inliner_move_time_to_leaves(const SexpInliner_ptr self,
                                                 node_ptr expr, int time)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  SymbTable_ptr symb_table = SexpInliner_get_symb_table(self);
  const ExprMgr_ptr exprs =
      EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  node_ptr key;
  node_ptr res;

  if ((node_ptr) NULL == expr) return expr;

  /* checks memoization */
  key = find_node(nodemgr, COLON, expr, NODE_FROM_INT(time));
  res = find_assoc(self->hash_timed2leaves, key);
  if (res != (node_ptr) NULL) return res;

  switch (node_get_type(expr)) {
    /* leaves */
  case FAILURE:
  case TRUEEXP:
  case FALSEEXP:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case UWCONST: case SWCONST:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
    res = expr;
    break;

  case CONTEXT:
    ErrorMgr_internal_error(errmgr, "SexpInliner::mode_time_to_leaves: "
                            "CONTEXT in expression assumed "
                            "to be already flattened");

  case BIT:
  case DOT:
  case ATOM:
  case ARRAY: {
    ResolveSymbol_ptr rs;
    node_ptr symb;

    rs = SymbTable_resolve_symbol(self->st, expr, Nil);
    symb = ResolveSymbol_get_resolved_name(rs);

    /* defines are expanded */
    if (ResolveSymbol_is_define(rs)) {
      /* define (rhs must be boolean, recur to check) */
#if SEXP_INLINER_EXPAND_DEFINES
      /*   [MR]: We may think in more deep if we want the DEFINES to
               be expanded or if we can think of expanding later code
               to handle also expressions containing DEFINEs. In this
               way the expression may grow alot. */
      node_ptr body = SymbTable_get_define_flatten_body(self->st, symb);
      res = sexp_inliner_move_time_to_leaves(self, body, time);
#else
      res = sexp_inliner_move_time_to_leaves(self, symb, time);
#endif
    }

    /* is it a formal param? */
    else if (ResolveSymbol_is_parameter(rs)) {
      node_ptr actual = SymbTable_get_flatten_actual_parameter(self->st, symb);
      res = sexp_inliner_move_time_to_leaves(self, actual, time);
    }

    /* is it a constant? */
    else if (ResolveSymbol_is_constant(rs))
      res = symb;

    /* otherwise keep as it is and attach time information to it */
    else if (time == EXPR_UNTIMED_CURRENT)
      res = symb;

    else if (time == EXPR_UNTIMED_NEXT)
      res = ExprMgr_next(exprs, symb, symb_table);

    else {
      /* here it is timed */
      nusmv_assert(time >= 0);
      res = ExprMgr_attime(exprs, symb, time, symb_table);
    }

    break;
  }

  case NEXT:
    nusmv_assert(time != EXPR_UNTIMED_NEXT); /* no nested next */
    if (time == EXPR_UNTIMED_CURRENT) {
      res = sexp_inliner_move_time_to_leaves(self, car(expr),
                                             EXPR_UNTIMED_NEXT);
    }
    else {
      /* here it is timed */
      nusmv_assert(time >= 0);
      res = sexp_inliner_move_time_to_leaves(self, car(expr), time+1);
    }
    break;

  case ATTIME:
    nusmv_assert(EXPR_UNTIMED_NEXT != time); /* not in NEXT */
    /* here ATTIME is considered absolute */
    res = sexp_inliner_move_time_to_leaves(
        self,
        ExprMgr_attime_get_untimed(exprs, expr),
        ExprMgr_attime_get_time(exprs, expr));
    break;

    /* These are special cases: The cdr part of the subformula is a
       range. If the range is passed to Expr_resolve, and min == max
       (e.g. 18..18), then it is simplified to "18", which then leads
       to a non well-formed formula. */
  case EBF:
  case ABF:
  case EBG:
  case ABG:
  case EBU:
  case ABU:
    nusmv_assert(Nil == cdr(expr) || TWODOTS == node_get_type(cdr(expr)));

    res = ExprMgr_resolve(
        exprs, self->st, node_get_type(expr),
        sexp_inliner_move_time_to_leaves(self, car(expr), time),
        cdr(expr));
    break;

  default: /* all other uninteresting cases */
    res = ExprMgr_resolve(
        exprs, self->st, node_get_type(expr),
        sexp_inliner_move_time_to_leaves(self, car(expr), time),
        sexp_inliner_move_time_to_leaves(self, cdr(expr), time));
  }

  insert_assoc(self->hash_timed2leaves, key, res); /* memoizes the result */
  return res;
}


/*!
  \brief Extract all equals (EQUAL and IFF) in the form v = t.

  Unicity of v is not guaranteed. Returns new expr where all
  extracted equals have been substituted.

  This function collect all equalities which HAVE TO be satisfied to
  make the top-expression hold. For example, for (a=b ^ c=d) both
  equalities are returned but for (a=b | c=d) the returned set is
  empty.


  SideEffects   [The results of the function call is memoized in self]

*****************************************************************************

  \se The results of the function call is memoized in self
*/
/* know if it should be reset after every iteration or not? */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static node_ptr
sexp_inliner_extract_equals_invars(const SexpInliner_ptr self,
                                   struct SexpInlinerExtractEqualsInfo* info,
                                   node_ptr expr,
                                   boolean is_neg)
{
  node_ptr key, res;

  if (Nil == expr)
    return (node_ptr) NULL;

  /* checks memoization */
  key = expr;
  if (is_neg)
    key = find_node(info->nodemgr, NOT, key, Nil);

  res = find_assoc(self->hash_extract_equals_invars, key);
  if (res != Nil)
    return res;

  switch (node_get_type(expr)) {
  case CONTEXT:
    ErrorMgr_internal_error(info->errmgr,
                            "SexpInliner::auto_learn_equals: "
                            "CONTEXT in expression "
                            " assumed to be already flattened");

  case NOT:
    res = ExprMgr_not(
        info->exprmgr,
        sexp_inliner_extract_equals_invars(self, info, car(expr), !is_neg));
    break;

  case AND:
    if (!is_neg) { /* negated AND becomes OR which is useless for us */
      res = ExprMgr_and(
          info->exprmgr,
          sexp_inliner_extract_equals_invars(self, info, car(expr), is_neg),
          sexp_inliner_extract_equals_invars(self, info, cdr(expr), is_neg));
    }
    /* otherwise just resolve */
    else res = ExprMgr_resolve(info->exprmgr, self->st, node_get_type(expr),
                               car(expr), cdr(expr));
    break;

  case OR:
    if (is_neg) { /* negated OR becomes AND */
      res = ExprMgr_or(
          info->exprmgr,
          sexp_inliner_extract_equals_invars(self, info, car(expr), is_neg),
          sexp_inliner_extract_equals_invars(self, info, cdr(expr), is_neg));
    }
    /* otherwise just resolve */
    else res = ExprMgr_resolve(info->exprmgr, self->st, node_get_type(expr),
                               car(expr), cdr(expr));
    break;

    /*   a iff b = !a xor b, or alternatively a xor !b. Are there other
         operators that it would be worth to consider? */
  case IMPLIES:
    if (is_neg) {
      res = ExprMgr_implies(
          info->exprmgr,
          sexp_inliner_extract_equals_invars(self, info, car(expr), false),
          sexp_inliner_extract_equals_invars(self, info, cdr(expr), is_neg));
    }
    /* otherwise just resolve */
    else res = ExprMgr_resolve(info->exprmgr, self->st, node_get_type(expr),
                               car(expr), cdr(expr));
    break;

  case NOTEQUAL:
    if (is_neg) {
      res = sexp_try_acquiring_equality(self, info, expr, is_neg);
    }
    /* otherwise just resolve */
    else res = ExprMgr_resolve(info->exprmgr, self->st, node_get_type(expr),
                               car(expr), cdr(expr));
    break;

  case IFF:
  case EQUAL:
    if (!is_neg) { /* process equalities only with positive polarity */
      res = sexp_try_acquiring_equality(self, info, expr, is_neg);
    }
    /* otherwise just resolve */
    else res = ExprMgr_resolve(info->exprmgr, self->st, node_get_type(expr),
                               car(expr), cdr(expr));
    break;

  case EQDEF:
    if (!is_neg) {
      if (NEXT != node_get_type(car(expr)) &&
          SMALLINIT != node_get_type(car(expr))) {
        const OptsHandler_ptr opts =
          OPTS_HANDLER(NuSMVEnv_get_value(ENV_OBJECT(self)->environment,
                                          ENV_OPTS_HANDLER));

        /* this is an invariant */
        node_ptr hit;

        /* [MR??]: assignment to next variables and to to initial values
           [MR??]: of variables. Is there any reason why for instance we
           [MR??]: do not want to inline initial expression or next?
           [MR??]: init(x) := y + 1; init(y) := z + 2 the y can be inlined in
           [MR??]: the first expression (similarly for next(x)) thus possibly
           [MR??]: simplifying a lot expressions, and possibly reduce the cone
           [MR??]: of variables. */
        /* checks if the variable has a forced invariant already: in
           this case keep the formula */
        hit = find_assoc(self->var2invar, car(expr));
        if ((node_ptr) NULL != hit) {
          res = sexp_inliner_assign_to_setin(self, expr);
          break;
        }

        /* checks if variable has a forced equivalence already: in
           this case keep the formula */
        hit = find_assoc(self->var2expr, car(expr));
        if ((node_ptr) NULL != hit) {
          res = sexp_inliner_assign_to_setin(self, expr);
          break;
        }

        if (!sexp_inliner_is_expr_deterministic(self, cdr(expr))) {
          res = sexp_inliner_assign_to_setin(self, expr);
          break;
        }

        /* acquire the invariant */
        if (opt_verbose_level_gt(opts, 6)) {
          Logger_nlog(info->logger, info->wffprint,
                      "SexpInliner: acquiring invar '%N'\n",
                      expr);
        }

        info->invars = Set_AddMember(info->invars, (Set_Element_t) expr);
        res = ExprMgr_true(info->exprmgr);
        break;
      }
      /* not an invariant, handles it as a normal equality */
      res = sexp_try_acquiring_equality(self, info, expr, is_neg);
    }
    /* otherwise just resolve */
    else res = ExprMgr_resolve(info->exprmgr, self->st, node_get_type(expr),
                               car(expr), cdr(expr));
    break;

    /* I add this to simplify timed expressions during quantifier elim using
       strutural elim. */
  case ATTIME:
  case NEXT:
    nusmv_assert((DOT == node_get_type(car(expr))) ||
                 (ATOM == node_get_type(car(expr))) ||
                 (BIT == node_get_type(car(expr))) ||
                 (ARRAY == node_get_type(car(expr))));
    /* a variable here must be boolean, and we can acquire
       next(v) <-> neg ? F:T */
    if (SymbTable_is_symbol_var(self->st, car(expr))) {
      nusmv_assert(SymbTable_is_symbol_bool_var(self->st, car(expr)));
      /* [MR] Here we cannot use Expr_iff since it will simplify the
         expressions into expr or !expr thus breaking the expected input. */
      res = sexp_try_acquiring_equality(
          self, info,
          (is_neg ?
           find_node(info->nodemgr, IFF, expr, ExprMgr_false(info->exprmgr)) :
           find_node(info->nodemgr, IFF, expr, ExprMgr_true(info->exprmgr))),
          is_neg);
    }
    /* otherwise just resolve */
    else res = ExprMgr_resolve(info->exprmgr, self->st, node_get_type(expr),
                               car(expr), cdr(expr));
    break;

  case DOT:
  case ATOM:
  case BIT:
  case ARRAY:
    /* a variable here must be boolean, and we can acquire v <-> neg ? F:T */
    if (SymbTable_is_symbol_var(self->st, expr)) {
      nusmv_assert(SymbTable_is_symbol_bool_var(self->st, expr));
      /* [MR] Here we cannot use Expr_iff since it will simplify the
         expressions into expr or !expr thus breaking the expected input. */
      res = sexp_try_acquiring_equality(
          self, info,
          (is_neg ?
           find_node(info->nodemgr, IFF, expr, ExprMgr_false(info->exprmgr)) :
           find_node(info->nodemgr, IFF, expr, ExprMgr_true(info->exprmgr))),
          is_neg);
    }
    /* otherwise just resolve */
    else res = ExprMgr_resolve(info->exprmgr, self->st, node_get_type(expr),
                               car(expr), cdr(expr));
    break;

  default:
    /* for all other expressions just resolve */
    res = ExprMgr_resolve(info->exprmgr, self->st, node_get_type(expr),
                          car(expr), cdr(expr));
  }

  nusmv_assert(Nil != res); /* the exp has to be processed already */

  /* memoizes the result */
  insert_assoc(self->hash_extract_equals_invars, key, res);

  return res;
}


/*!
  \brief privately used by sexp_inliner_extract_candidate_equals

  Callback to fill in the good_equals array
*/

static enum st_retval
sexp_inliner_fill_good_equals(char* key, char* data, char* arg)
{
  nusmv_assert(COLON == node_get_type((node_ptr) data));
  array_insert_last(node_ptr, (array_t*) arg, (node_ptr) data);
  return ST_CONTINUE;
}


/*!
  \brief privately used by sexp_inliner_extract_candidate_equals

  Callback to sort the good_equals array
*/

static int sexp_inliner_sort_good_equals(const void* obj1, const void* obj2)
{
  register node_ptr n1 = (*(node_ptr*) obj1);
  register node_ptr n2 = (*(node_ptr*) obj2);

  nusmv_assert(COLON == node_get_type(n1) && COLON == node_get_type(n2));

  return (PTR_TO_INT(cdr(n1)) - PTR_TO_INT(cdr(n2)));
}

/*!
  \brief used privately for free the content of the array
                 returned by sexp_inliner_extract_candidate_equals


*/
static void sexp_inliner_free_equalities_array(NodeMgr_ptr nodemgr,
                                               array_t* arr)
{
  int k;
  node_ptr col;

  arrayForEachItem(node_ptr, arr, k, col) {
    if ((node_ptr) NULL != col) {
      nusmv_assert(COLON == node_get_type(col));
      free_node(nodemgr, col);
    }
  }
}


/*!
  \brief used for debugging purposes


*/

static void sexp_inliner_print_equality_array(const NuSMVEnv_ptr env,
                                              array_t* arr, FILE* _file)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  int k;
  node_ptr col;

  fprintf(_file, "The ordered equalities array is:\n");
  fprintf(_file, "pos:deps:equality\n");

  arrayForEachItem(node_ptr, arr, k, col) {
    if ((node_ptr) NULL != col) {
      int deps;
      nusmv_assert(COLON == node_get_type(col));
      deps = PTR_TO_INT(cdr(col));
      fprintf(_file, "%d:%d:", k, deps);
      print_node(wffprint, _file, car(col));
    }
    else fprintf(_file, "%d: : REMOVED", k);

    fprintf(_file, "\n");
  }
  fprintf(_file, "\n");
}

/*!
  \brief Removes duplicates

  splits equals into good_equals and rem_equals
  sets. Returned array must be freed by the caller
*/
static array_t*
sexp_inliner_extract_candidate_equals(const SexpInliner_ptr self,
                                      const Set_t equals,
                                      const Set_t imp_vars,
                                      const hash_ptr var2invar,
                                      Set_t* rem_equals)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  array_t* good_equals;

#ifdef IN_MOD
  Olist_ptr vlist = Olist_create();
  Oiter vit;
#endif

  /* this hash associates 'var' -> COLON('var op expr', deps_num),
     where op can be IFF or EQUAL. It used to keep the best var */
  hash_ptr hash = new_assoc();
  Set_Iterator_t iter;

  SET_FOREACH(equals, iter) {
    node_ptr equal = Set_GetMember(equals, iter);
    node_ptr var, expr, prev_expr;
    Set_t deps;
    Set_t ideps;

    nusmv_assert(EQUAL == node_get_type(equal) ||
                 IFF == node_get_type(equal) ||
                 EQDEF == node_get_type(equal));

    var = car(equal);
    expr = cdr(equal);

    /* skip init(var) := ... */
    if (EQDEF == node_get_type(equal) &&
        SMALLINIT == node_get_type(var)) continue;

    /* check if there is already an extracted invariant about the
       variable, and re-introduce the equivalence in rem_equivs */
    if ((node_ptr) NULL != find_assoc(var2invar, var)) {
      *rem_equals = Set_AddMember(*rem_equals, (Set_Element_t) equal);
      continue;
    }

    /* Dependencies wrt imp_vars only */
    deps = Formula_GetDependenciesByType(self->st, expr, Nil,
                                         VFT_CNIF, true /* preserve time */);
    ideps = Set_Copy(deps);
    ideps = Set_Intersection(ideps, imp_vars);

    if (Set_IsMember(ideps, var)) {
      /* here there is a self-loop. Inserts into remaining */
      *rem_equals = Set_AddMember(*rem_equals, (Set_Element_t) equal);
    }
    else { /* no self-loop, compares with the best candidate found so far */
      int expr_deps = Set_GiveCardinality(deps);

      prev_expr = find_assoc(hash, var);
      if ((node_ptr) NULL != prev_expr) {
        int prev_deps;
        nusmv_assert(COLON == node_get_type(prev_expr));
        prev_deps = PTR_TO_INT(cdr(prev_expr));

        if (prev_deps > expr_deps) {
          /* prev_expr is substituted by expr */
          insert_assoc(hash, var,
                       new_node(nodemgr, COLON, equal,
                                PTR_FROM_INT(node_ptr, expr_deps)));
          *rem_equals = Set_AddMember(*rem_equals,
                                      (Set_Element_t) car(prev_expr));
          /* We must free the substitued expression */
          FREE(prev_expr);
        }
        else { /* keeps prev_deps */
          *rem_equals = Set_AddMember(*rem_equals, (Set_Element_t) equal);
        }
      }
      else { /* never found var before */
        insert_assoc(hash, var,
                     new_node(nodemgr, COLON, equal,
                              PTR_FROM_INT(node_ptr, expr_deps)));
#ifdef IN_MOD
        Olist_append(vlist, var);
#endif
      }
    }
    /* This needs to be released, Formula_GetDependenciesByType
       returns a copy of the set, and it is the responsibility of
       the caller to free it. */
    Set_ReleaseSet(deps);
    Set_ReleaseSet(ideps);
  } /* foreach equals loop */

  /* constructs, fills in and sort good_equals array. Sorting is
     performed looking at the cardinality associated with each
     equal within the hash */
  good_equals = array_alloc(node_ptr,
       Set_GiveCardinality(equals) - Set_GiveCardinality(*rem_equals));

#ifdef IN_MOD
  OLIST_FOREACH(vlist, vit) {
    node_ptr v = (node_ptr) Oiter_element(vit);
    node_ptr d = find_assoc(hash, v);
    nusmv_assert(Nil != d);
    nusmv_assert(COLON == node_get_type(d));
    array_insert_last(node_ptr, good_equals, d);
  }
#else
  st_foreach(hash, sexp_inliner_fill_good_equals, (char*) good_equals);
#endif

  array_sort(good_equals, sexp_inliner_sort_good_equals);

  if (opt_verbose_level_gt(opts, 6)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "In sexp_inliner_extract_candidate_equals:\n");
    sexp_inliner_print_equality_array(env, good_equals, errstream);
  }

  /* nodes within the hash are not freed (even if they were built
     with new_node), as they have been copied inside the returned
     array, and they will be freed later by
     sexp_inliner_free_equalities_array */
  free_assoc(hash);

#ifdef IN_MOD
  Olist_destroy(vlist);
#endif

  return good_equals;
}


/*!
  \brief Removes loops from good_equals
*/
static hash_ptr
sexp_inliner_remove_loops(const SexpInliner_ptr self,
                          array_t* good_equals, hash_ptr hash_invars,
                          Set_t* good, Set_t* rem)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  hash_ptr hash_equals = new_assoc();
  int i;

  for (i=0; i < array_n(good_equals); ++i) {
    node_ptr coli = array_fetch(node_ptr, good_equals, i);
    node_ptr vi, ti;
    int j;

    if ((node_ptr) NULL != coli) {
      nusmv_assert(COLON == node_get_type(coli));
      vi = car(car(coli));
      ti = cdr(car(coli));

      insert_assoc(hash_equals, vi, ti);

      for (j=i+1; j < array_n(good_equals); ++j) {
        /* this loop tries to inline the following equal, or drops
           the following equals if loops are detected */

        node_ptr colj = array_fetch(node_ptr, good_equals, j);
        if ((node_ptr) NULL != colj) {
          node_ptr vj, tj, tj_inlined;
          Set_t deps;

          nusmv_assert(COLON == node_get_type(colj));
          vj = car(car(colj));
          tj = cdr(car(colj));

          /* here we inline even if vi is not in tj, as there are
             also user-specified invariants and equalities which
             may be introducing loops */
          tj_inlined = sexp_inliner_substitute(self, tj,
                                               hash_equals, hash_invars,
                                               (boolean*) NULL);

          /* now checks if a loop has been introduced */
          deps = Formula_GetDependenciesByType(self->st, tj_inlined, Nil,
                                               VFT_CNIF,
                                               true /*preserve time*/);
          if (Set_IsMember(deps, vj)) {
            /* detected a loop: move to remaining set, clears the
               corresponding entry within the array */
            *rem = Set_AddMember(*rem, (Set_Element_t) car(colj));
            free_node(nodemgr, colj);
            array_insert(node_ptr, good_equals, j, (node_ptr) NULL);
          }
          else { /* inlines tj */
            node_ptr new_colj =
              new_node(
                  nodemgr, COLON,
                  find_node(nodemgr, node_get_type(car(colj)),
                            vj, tj_inlined),
                  cdr(colj));

            /* removes the previous node, which will be substituted
               with one containing the inlined expression */
            free_node(nodemgr, colj);

            /* substitutes colj with new_colj within the ordered array */
            array_insert(node_ptr, good_equals, j, new_colj);
          }

          /* dependencies are no longer needed */
          Set_ReleaseSet(deps);
        }
      } /* end of inner loop */
    }
  } /* end of outer loop */

  if (opt_verbose_level_gt(opts, 7)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Removing loops: after first traversal:\n");
    sexp_inliner_print_equality_array(env, good_equals, errstream);
  }

  /* here hash_equals does not contain loops, but expression may be
     furtherly simplified, so we drops it and rebuild it by
     traversing the array bottom-up */
  clear_assoc(hash_equals);
  /* traverses the array from bottom-up to re-construct the hash */
  for (i=array_n(good_equals)-1; i>=0; --i) {
    node_ptr col = array_fetch(node_ptr, good_equals, i);

    /* if the entry was not removed */
    if ((node_ptr) NULL != col) {
      node_ptr v, t, e;
      nusmv_assert(COLON == node_get_type(col));

      e = car(col);
      v = car(e); t = cdr(e);

      /* last entry is surely already simplified */
      if (i < array_n(good_equals)-1) {
        boolean tchanged = false;

        t = sexp_inliner_substitute(self, t, hash_equals, hash_invars,
                                    &tchanged);
        if (tchanged)
          e = find_node(nodemgr, node_get_type(e), v, t);
      }

      /* adds the equality to the good set, and updates hash_equals */
      *good = Set_AddMember(*good, (Set_Element_t) e);
      insert_assoc(hash_equals, v, t);
    }
  }

  if (opt_verbose_level_gt(opts, 7)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Removing loops, after re-traversing bottom-up:\n");
    sexp_inliner_print_equality_array(env, good_equals, errstream);
  }

  /* here hash_equals is fully simplified */
  return hash_equals;
}

/*!
  \brief If given equal concerns a variable (like in
                      'expr = var') the equality is added to the given set
                      (after possible syntactic manipolation) and
                      the True expression is returned. Otherwise
                      the given equal expression is returned.

  This is called during the traversal of the
                      expression when auto-extracting equalities.
                      WARNING! The equal expression is assumed to
                      be already flattened.
*/
static node_ptr
sexp_try_acquiring_equality(const SexpInliner_ptr self,
                            struct SexpInlinerExtractEqualsInfo* info,
                            node_ptr equal, boolean is_neg)
{
  node_ptr left, right;
  node_ptr var = Nil;
  node_ptr expr = Nil;
  node_ptr res;
  int type;
  SymbTable_ptr symb_table = self->st;
  TypeChecker_ptr type_checker = SymbTable_get_type_checker(symb_table);

  /* normalizes type wrt to the essential value */
  if (is_neg && NOTEQUAL == node_get_type(equal))
    type = EQUAL;
  else
    type = node_get_type(equal);

  nusmv_assert(EQUAL == type || IFF == type || EQDEF == type);

  left = sexp_inliner_expr_is_var(self, car(equal)) ? car(equal) : Nil;
  right = sexp_inliner_expr_is_var(self, cdr(equal)) ? cdr(equal) : Nil;

  if (Nil != left && Nil == right) {
    var = left;
    expr = cdr(equal);
  }
  else if (Nil == left && Nil != right) {
    var = right;
    expr = car(equal);
  }
  else if (Nil != left && Nil != right && left != right) {
    /* take one randomly (this can be improved) */
    var = left;
    expr = right;
  }

  /*     [MR??]: an equality among constants and thus we do not need to even proceed and
         [MR??]: we can simplify the expression if possible w.r.t. the values
         [MR??]: either to TRUE, FALSE or leave as it is if we cannot perfrm the test. */
  if ((Nil != var) && (SMALLINIT == node_get_type(var))) {
    var = car(var);
  }

  /* Do not acquire equalities between integer vars and real expressions.
     If the real expression is a variable, acquire the swapped equality. */
  if (Nil != var) {
    SymbType_ptr var_type;

    /* Here I must use this function because var can be a var inside a NEXT */
    var_type  = TypeChecker_get_expression_type(type_checker,
                                                var,
                                                Nil);

    if (SymbType_is_integer(var_type) || SymbType_is_pure_int_enum(var_type)) {
      SymbType_ptr expr_type;

      expr_type = TypeChecker_get_expression_type(type_checker,
                                                  expr,
                                                  Nil);

      if (SymbType_is_real(expr_type) || SymbType_is_continuous(expr_type)) {
        if (Nil != left && Nil != right && left != right) {
          node_ptr tmp;

          tmp = var; var = expr; expr = tmp;
        }
        else {
          var = Nil;
          expr = Nil;
        }
      }
    }
  }


  /* the equality is acquired only if not in black list, and if the
     expression is found syntactically deterministic */
  if (Nil != var && Nil != expr &&
      !Set_IsMember(self->blacklist, (Set_Element_t) var) &&
      sexp_inliner_is_expr_deterministic(self, expr)) {
    node_ptr hit;

    /* checks if the variable has a forced invariant already: in
       this case keep the formula */
    hit = find_assoc(self->var2invar, var);
    if ((node_ptr) NULL != hit) return equal;

    /* checks if variable has a forced equivalence already: in
       this case keep the formula */
    hit = find_assoc(self->var2expr, var);
    if ((node_ptr) NULL != hit) return equal;

    /* here the equivalence can be acquired */
    info->vars = Set_AddMember(info->vars, (Set_Element_t) var);

    info->equals = Set_AddMember(
        info->equals, (Set_Element_t) find_node(info->nodemgr, type, var, expr));

    if (opt_verbose_level_gt(info->opts, 6)) {
      Logger_nlog(info->logger, info->wffprint,
                  "SexpInliner: acquiring equality '%N'\n",
                  find_node(info->nodemgr, type, var, expr));
    }

    res = is_neg ? ExprMgr_false(info->exprmgr) : ExprMgr_true(info->exprmgr);
  }
  else res = equal; /* not a variable equality */

  return res;
}

/*!
  \brief Returns true if given flattened expression is a
                    variable (potentially timed).

  expr is assumed to have next and timed nodes moved
                    to the leaves.
*/
static boolean
sexp_inliner_expr_is_var(const SexpInliner_ptr self, node_ptr expr)
{
  node_ptr symb;

  if (NEXT == node_get_type(expr) || SMALLINIT == node_get_type(expr) ||
      ATTIME == node_get_type(expr)) symb = car(expr);
  else symb = expr;

  if (BIT == node_get_type(symb)    ||
      DOT == node_get_type(symb)    ||
      ATOM == node_get_type(symb)   ||
      ARRAY == node_get_type(symb)) {
    return SymbTable_is_symbol_var(self->st, symb);
  }
  return false;
}

/*!
  \brief Traverses the structure of the expression, substituting
  (in top-level conjuctions) all found equivalences. Invariants are
  substituted within the whole expression. changed is set to true
  when applying the inlining, otherwise it keeps its values.

  The inlined expression is returned. var2invar (can be NULL) and
  is used to resolve invariants extracted from ASSIGNs.
  internal , self->hash_subst is used for memoization

  \se changed and self->hash_subst are modified
*/
static node_ptr
sexp_inliner_substitute(SexpInliner_ptr self, node_ptr expr,
                        hash_ptr var2expr, hash_ptr var2invar,
                        boolean* changed)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  node_ptr res;
  SymbTable_ptr symb_table = SexpInliner_get_symb_table(self);
  const ExprMgr_ptr exprs =
      EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if ((node_ptr) NULL == expr) return (node_ptr) NULL;

  /* memoization */
  res = find_assoc(self->hash_subst, expr);
  if (res != (node_ptr) NULL) {
    node_ptr res_in = car(res);

    nusmv_assert(COLON == node_get_type(res));
    if ((boolean*) NULL != changed)
      *changed = false;

    if (res_in != expr) {
      res_in = sexp_inliner_substitute(self, res_in, var2expr, var2invar,
                                       changed);
    }

    if ((boolean*) NULL == changed || *changed) {
      insert_assoc(self->hash_subst, expr,
                   find_node(nodemgr, COLON, res_in,
                             ((boolean*) NULL != changed) ?
                             PTR_FROM_INT(node_ptr, (int) *changed) :
                             PTR_FROM_INT(node_ptr, false)));
    }

    return res_in;
  }

  switch (node_get_type(expr)) {
    /* leaves */
  case FAILURE:
  case TRUEEXP:
  case FALSEEXP:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case UWCONST: case SWCONST:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
    return expr;

  case BIT:
  case ATOM:
  case DOT:
  case ARRAY: {
    node_ptr hit;

    /* tries with automatic equivalences */
    hit = find_assoc(var2expr, expr);
    if ((node_ptr) NULL != hit) {
#ifdef _DEBUG_SUBST
      StreamMgr_print_output(streams,  "Substituting v2e \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", expr);
      StreamMgr_print_output(streams,  "\" with \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", hit);
      fprintf(outstream, "\n");
#endif
      if ((boolean*) NULL != changed && hit != expr)
        *changed = true;

      return hit;
    }

    /* tries with user-provided equivalences */
    hit = find_assoc(self->var2expr, expr);
    if ((node_ptr) NULL != hit) {
#ifdef _DEBUG_SUBST
      StreamMgr_print_output(streams,  "Substituting iv2e \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", expr);
      StreamMgr_print_output(streams,  "\" with \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", hit);
      fprintf(outstream, "\n");
#endif
      if ((boolean*) NULL != changed && hit != expr)
        *changed = true;

      return hit;
    }

    /* tries with invariants (forced and auto-extracted) */
    if ((hash_ptr) NULL != var2invar) {
      hit = find_assoc(var2invar, expr);
      if ((node_ptr) NULL != hit) {
#ifdef _DEBUG_SUBST
        StreamMgr_print_output(streams,  "Substituting v2i \"");
        StreamMgr_nprint_output(streams, wffprint, "%N", expr);
        StreamMgr_print_output(streams,  "\" with \"");
        StreamMgr_nprint_output(streams, wffprint, "%N", hit);
        fprintf(outstream, "\n");
#endif
        if ((boolean*) NULL != changed && hit != expr)
          *changed = true;

        return hit;
      }
    }
    hit = find_assoc(self->var2invar, expr);
    if ((node_ptr) NULL != hit) {
#ifdef _DEBUG_SUBST
      StreamMgr_print_output(streams,  "Substituting iv2i \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", expr);
      StreamMgr_print_output(streams,  "\" with \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", hit);
      fprintf(outstream, "\n");
#endif
      if ((boolean*) NULL != changed && hit != expr)
        *changed = true;

      return hit;
    }
    return expr;
  }

  case CONTEXT:
    ErrorMgr_internal_error(
        errmgr,
        "SexpInliner::substitute: CONTEXT in expression assumed"
        " to be already flattened");

  case NEXT: { /* next was moved to leaves */
    node_ptr name = car(expr);
    node_ptr nname = ExprMgr_next(exprs, name, symb_table);
    node_ptr hit;

    nusmv_assert((DOT == node_get_type(name)) ||
                 (ATOM == node_get_type(name)) ||
                 (BIT == node_get_type(name)) ||
                 (ARRAY == node_get_type(name)));

    /* tries with automatic equivalences */
    hit = find_assoc(var2expr, nname);
    if ((node_ptr) NULL != hit) {
#ifdef _DEBUG_SUBST
      StreamMgr_print_output(streams,  "Substituting v2e \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", nname);
      StreamMgr_print_output(streams,  "\" with \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", hit);
      fprintf(outstream, "\n");
#endif
      if ((boolean*) NULL != changed && hit != nname)
        *changed = true;

      return hit;
    }

    /* tries with user-provided equivalences */
    hit = find_assoc(self->var2expr, nname);
    if ((node_ptr) NULL != hit) {
#ifdef _DEBUG_SUBST
      StreamMgr_print_output(streams,  "Substituting iv2e \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", nname);
      StreamMgr_print_output(streams,  "\" with \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", hit);
      fprintf(outstream, "\n");
#endif
      if ((boolean*) NULL != changed && hit != nname)
        *changed = true;

      return hit;
    }

    /* sees if there is an invariant about this variable (first
       auto-extracted, then user-provided) */
    if ((hash_ptr) NULL != var2invar) {
      hit = find_assoc(var2invar, name);
      if ((node_ptr) NULL != hit) {
#ifdef _DEBUG_SUBST
        StreamMgr_print_output(streams,  "Substituting v2i \"");
        StreamMgr_nprint_output(streams, wffprint, "%N", nname);
        StreamMgr_print_output(streams,  "\" with \"");
        StreamMgr_nprint_output(streams, wffprint, "%N", hit);
        fprintf(outstream, "\n");
#endif
        if ((boolean*) NULL != changed && hit != name)
          *changed = true;

        hit = ExprMgr_next(exprs, hit, symb_table);
        hit = sexp_inliner_move_time_to_leaves(self, hit, EXPR_UNTIMED_CURRENT);
        return hit;
      }
    }

    hit = find_assoc(self->var2invar, name);
    if ((node_ptr) NULL != hit) {
#ifdef _DEBUG_SUBST
      StreamMgr_print_output(streams,  "Substituting iv2i \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", nname);
      StreamMgr_print_output(streams,  "\" with \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", hit);
      fprintf(outstream, "\n");
#endif
      if ((boolean*) NULL != changed && hit != name)
        *changed = true;

      /* We move next to leaves */
      hit = ExprMgr_next(exprs, hit, symb_table);
      hit = sexp_inliner_move_time_to_leaves(self, hit, EXPR_UNTIMED_CURRENT);
      return hit;
    }
    /* no hit in the invariants */
    return nname;
  }

  case ATTIME: { /* attime was moved to leaves */
    node_ptr name = ExprMgr_attime_get_untimed(exprs, expr);
    node_ptr hit;

    nusmv_assert((DOT == node_get_type(name)) ||
                 (ATOM == node_get_type(name)) ||
                 (BIT == node_get_type(name)) ||
                 (ARRAY == node_get_type(name)));

    /* tries with automatic equivalences */
    hit = find_assoc(var2expr, expr);
    if ((node_ptr) NULL != hit) {
#ifdef _DEBUG_SUBST
      StreamMgr_print_output(streams,  "Substituting v2e \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", expr);
      StreamMgr_print_output(streams,  "\" with \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", hit);
      fprintf(outstream, "\n");
#endif
      if ((boolean*) NULL != changed && hit != expr)
        *changed = true;

      return hit;
    }

    /* tries with user-provided equivalences */
    hit = find_assoc(self->var2expr, expr);
    if ((node_ptr) NULL != hit) {
#ifdef _DEBUG_SUBST
      StreamMgr_print_output(streams,  "Substituting iv2e \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", expr);
      StreamMgr_print_output(streams,  "\" with \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", hit);
      fprintf(outstream, "\n");
#endif
      if ((boolean*) NULL != changed && hit != expr)
        *changed = true;

      return hit;
    }

    /* sees if there is an invariant about this variable (first
       auto-extracted, then user-provided) */
    if ((hash_ptr) NULL != var2invar) {
      hit = find_assoc(var2invar, name);
      if ((node_ptr) NULL != hit) {
#ifdef _DEBUG_SUBST
        StreamMgr_print_output(streams,  "Substituting v2i \"");
        StreamMgr_nprint_output(streams, wffprint, "%N", expr);
        StreamMgr_print_output(streams,  "\" with \"");
        StreamMgr_nprint_output(streams, wffprint, "%N", hit);
        fprintf(outstream, "\n");
#endif
        if ((boolean*) NULL != changed && hit != name)
          *changed = true;

        error_unreachable_code();
      /* [MRRC]: should be returned assuring at-time is moved to leaves */
        return hit;
      }
    }

    hit = find_assoc(self->var2invar, name);
    if ((node_ptr) NULL != hit) {
#ifdef _DEBUG_SUBST
      StreamMgr_print_output(streams,  "Substituting iv2i \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", expr);
      StreamMgr_print_output(streams,  "\" with \"");
      StreamMgr_nprint_output(streams, wffprint, "%N", hit);
      StreamMgr_print_output(streams,  "\n");
#endif
      if ((boolean*) NULL != changed && hit != name)
        *changed = true;

      error_unreachable_code();
      /* [MRRC]: should be returned assuring at-time is moved to leaves */
      return ExprMgr_attime(exprs, hit, ExprMgr_attime_get_time(exprs, expr),
                            symb_table);
    }
    /* no hit in the invariants */
    return expr;
  }

  case AND:
    res = ExprMgr_and(
        exprs,
        sexp_inliner_substitute(self, car(expr), var2expr,
                                var2invar, changed),
        sexp_inliner_substitute(self, cdr(expr), var2expr,
                                var2invar, changed));
    break;

  case EQDEF:
    /* substitutes only left side */
    res = ExprMgr_resolve(exprs, self->st, node_get_type(expr), car(expr),
                       sexp_inliner_substitute(self, cdr(expr), var2expr,
                                               var2invar, changed));
    break;

    /* These nodes need special treatment when used with
       Expr_resolve, since recursively enter into their cdr may
       break the formula. (Ranges with min = max are resolved as
       number by Expr_resolve). See issue 2194. */
  case EBF:
  case ABF:
  case EBG:
  case ABG:
  case EBU:
  case ABU:
    nusmv_assert(Nil == cdr(expr) || TWODOTS == node_get_type(cdr(expr)));

    res = ExprMgr_resolve(exprs, self->st, node_get_type(expr),
                       sexp_inliner_substitute(self, car(expr), var2expr,
                                               var2invar, changed),
                       cdr(expr));
    break;

  default:
    res = ExprMgr_resolve(exprs, self->st, node_get_type(expr),
                       sexp_inliner_substitute(self, car(expr), var2expr,
                                               var2invar, changed),
                       sexp_inliner_substitute(self, cdr(expr), var2expr,
                                               var2invar, changed));
  }

  /* memoizes the result */
  insert_assoc(self->hash_subst, expr,
               find_node(nodemgr, COLON, res,
                         ((boolean*) NULL != changed) ?
                         PTR_FROM_INT(node_ptr, (int) *changed) :
                          PTR_FROM_INT(node_ptr, false)));
  return res;
}

/*!
  \brief Here expr has next and attime moved to the leaves

  If the expression's cone contains the variable, the equivalence is
  not created. If the given expression is syntactically
  non-deterministic (see sexp_inliner_is_expr_deterministic about this
  over-approximation) the equivalence is not created. The equivalence
  is learnt independently on the blacklist. This is called only when
  the user forces an equivalence to exist, and not when extracting
  equivalences automatically.

  Returns true if the equivalences was successfully created, or false
  otherwise.
*/
static boolean
sexp_inliner_force_equivalence(SexpInliner_ptr self,
                               node_ptr var, Expr_ptr expr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  boolean res = false;

  if (sexp_inliner_is_expr_deterministic(self, expr)) {
    Set_t deps = Formula_GetDependenciesByType(self->st, expr, Nil,
                                               VFT_CNIF,
                                               true /* preserve time */);
    if (!Set_IsMember(deps, var)) {
      insert_assoc(self->var2expr, var, expr);
      res = true;

      if (opt_verbose_level_gt(opts, 6)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "SexpInliner: accepted user-provided equivalence '");
        Logger_nlog(logger, wffprint, "%N",  var);
        Logger_log(logger, " ==> ");
        Logger_nlog(logger, wffprint, "%N",  expr);
        Logger_log(logger, "'\n");
      }
    }
    Set_ReleaseSet(deps);
  }

  return res;
}

/*!
  \brief Here expr has next and attime moved to the leaves

  If the expression's cone contains the variable, the equivalence is
  not created.  If the given expression is syntactically
  non-deterministic (see sexp_inliner_is_expr_deterministic about this
  over-approximation) the equivalence is not created.

 Returns true if the equivalences was successfully created, or false
 otherwise.
*/
static boolean
sexp_inliner_force_invariant(SexpInliner_ptr self, node_ptr var, Expr_ptr expr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ExprMgr_ptr exprs =
      EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  boolean res = false;
  SymbTable_ptr symb_table = SexpInliner_get_symb_table(self);

  if (sexp_inliner_is_expr_deterministic(self, expr)) {
    Set_t deps =
        Formula_GetDependenciesByType(self->st, expr, Nil,
                                      VFT_CNIF, true /*preserve time*/);
    if (!Set_IsMember(deps, var)) {
      Expr_ptr old_invar = find_assoc(self->var2invar, var);

      if (expr != old_invar) {
        expr = ExprMgr_and_nil(exprs, old_invar, expr);
        insert_assoc(self->var2invar, var, expr);
        self->invars = Set_AddMember(self->invars,
                                     (Set_Element_t)ExprMgr_equal(exprs, var,
                                                               expr,
                                                               symb_table));
        res = true;

        if (opt_verbose_level_gt(opts, 6)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, "SexpInliner: accepted invariant '");
          Logger_nlog(logger, wffprint, "%N",  var);
          Logger_log(logger, " ==> ");
          Logger_nlog(logger, wffprint, "%N",  expr);
          Logger_log(logger, "'\n");
        }
      }
    }
    Set_ReleaseSet(deps);
  }

  return res;
}

/*!
  \brief Private constructor


*/
static InlineRes_ptr inline_res_create(const NuSMVEnv_ptr env, Expr_ptr orig)
{
  const ExprMgr_ptr exprs =
      EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  InlineRes_ptr self = ALLOC(InlineRes, 1);
  INLINE_RES_CHECK_INSTANCE(self);

  self->env = env;
  self->orig = orig;
  self->inlined = ExprMgr_true(exprs);
  self->equivs = Set_MakeEmpty();
  self->invars = Set_MakeEmpty();

  return self;
}

/*!
  \brief Class deinitializer


*/
static void inline_res_deinit(InlineRes_ptr self)
{
  Set_ReleaseSet(self->equivs);
  self->equivs = (Set_t) NULL;

  Set_ReleaseSet(self->invars);
  self->invars = (Set_t) NULL;
}

/*!
  \brief Private comparison


*/
static int sexp_inliner_expr_ptr_compare(const void * c1, const void * c2)
{
  Expr_ptr a = *((Expr_ptr *) c1); Expr_ptr b = *((Expr_ptr *) c2);
  if (a < b) return -1;
  if (a == b) return 0;
  return 1;
}


/*!
  \brief Detects if the given expression is deterministic.

  If TWODOTS or UNION node is found in the expression,
                 then the expression (with an over-approximation)
                 is considered as non-deterministic.  WARNING:
                 defines are not expanded, and cardinality is not
                 considered for sets (even singletons are
                 considered as sets, so the expression should be
                 simplified before calling this function).

  \se The results of the function call is memoized in self
*/

/* know if it should be reset after every iteration or not? */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static boolean
sexp_inliner_is_expr_deterministic(const SexpInliner_ptr self, node_ptr expr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  node_ptr res;
  boolean is_deterministic;

  if (Nil == expr) return true;

  /* checks memoization */
  res = find_assoc(self->hash_is_expr_deterministic, expr);
  if (res != Nil) { /* this expression has been already checked */
    /* here two values are possible : 1 - exp is deterministic, 2 --
       exp is not deterministic */
    switch (PTR_TO_INT(res)) {
    case 1: return true;
    case 2: return false;
    default: ErrorMgr_internal_error(errmgr, "impossible code");
    }
  }

  if (expr == Nil) return true;

  switch (node_get_type(expr)) {
    /* leaves */
  case FAILURE:
  case TRUEEXP:
  case FALSEEXP:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case UWCONST: case SWCONST:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case BIT:
  case ATOM:
  case DOT:
  case ARRAY:
    return true; /* do not remember constants */

  case TWODOTS:
  case UNION:
    return false; /* do not remember constants */

  case CASE:
  case IFTHENELSE:
    /* ignores conditions, focuses only on values */
    is_deterministic
      = (sexp_inliner_is_expr_deterministic(self, cdr(car(expr))) /*then*/
         &&
         sexp_inliner_is_expr_deterministic(self, cdr(expr)) /*else*/);

  default: /* all other cases */
    is_deterministic
      = (sexp_inliner_is_expr_deterministic(self, car(expr)) /*left*/
         &&
         sexp_inliner_is_expr_deterministic(self, cdr(expr)) /*right*/);
  } /* switch */

  /* remember the result : see at the beginning of fun why numbers are
   * 1 & 2. */
  if (is_deterministic) res = NODE_PTR(1);
  else res = NODE_PTR(2);
  insert_assoc(self->hash_is_expr_deterministic, expr, res);

  return is_deterministic;
}

/*!
  \brief Detects if the given expression contains ATTIME nodes.

  Expression is assumed to have DEFINEs expanded
*/
static boolean sexp_inliner_is_expr_timed(node_ptr expr)
{
  if (expr == Nil) return false;

  switch (node_get_type(expr)) {
    /* leaves */
  case FAILURE:
  case TRUEEXP:
  case FALSEEXP:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case UWCONST: case SWCONST:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case BIT:
  case ATOM:
  case DOT:
  case ARRAY:
    return false;

  case ATTIME: return true;

  default: /* all other cases */
    return (sexp_inliner_is_expr_timed(car(expr)) ||
            sexp_inliner_is_expr_timed(cdr(expr)));
  } /* switch */
}

/*!
  \brief Converts the given assign to an equivalent SETIN expression

  WARNING: init(x) := e is converted to "x in e"
*/
static Expr_ptr sexp_inliner_assign_to_setin(const SexpInliner_ptr self,
                                             Expr_ptr assign)
{
  SymbTable_ptr symb_table = SexpInliner_get_symb_table(self);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs =
      EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  nusmv_assert(EQDEF == node_get_type(assign));

  if (SMALLINIT == node_get_type(car(assign))) {
    /* gets rid of init */
    return ExprMgr_setin(exprs, caar(assign), cdr(assign), symb_table);
  }

  return ExprMgr_setin(exprs, car(assign), cdr(assign), symb_table);
}


static void SexpInlinerExtractEqualsInfo_init(
    struct SexpInlinerExtractEqualsInfo* info,
    const NuSMVEnv_ptr env)
{
  info->env = env;
  info->opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  info->nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  info->errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  info->exprmgr = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  info->logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
  info->wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  info->equals = Set_MakeEmpty();
  info->invars = Set_MakeEmpty();
  info->vars = Set_MakeEmpty();
}


static void
SexpInlinerExtractEqualsInfo_quit(struct SexpInlinerExtractEqualsInfo* info)
{
  Set_ReleaseSet(info->vars);
  Set_ReleaseSet(info->invars);
  Set_ReleaseSet(info->equals);
}
