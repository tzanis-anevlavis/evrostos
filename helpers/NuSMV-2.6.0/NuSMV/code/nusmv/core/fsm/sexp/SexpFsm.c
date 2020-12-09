/* ---------------------------------------------------------------------------


  This file is part of the ``fsm.sexp'' package of NuSMV version 2.
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
  \author Roberto Cavada
  \brief The SexpFsm implementation

  A SexpFsm instance represents a scalar FSM, but it
               is used also as base class for boolean FSMs which
               are instances of derived class BoolSexpFsm

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/fsm/sexp/SexpFsm.h"
#include "nusmv/core/fsm/sexp/SexpFsm_private.h"

#include "nusmv/core/fsm/sexp/sexpInt.h"

/* there are still some variables to be accessed there: */
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/compile/PredicateNormaliser.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"
#include "nusmv/core/parser/symbols.h"

#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/utils.h"

#include "nusmv/core/sexp/SexpInliner.h"
/*---------------------------------------------------------------------------*/
/* Constants declarations                                                    */
/*---------------------------------------------------------------------------*/

/*!
  \brief Set to 1 (needs recompilation) to force auto-check of the
             SexpFsm

  Use only in debugging mode, as self-checking can be expensive
*/
#define SEXP_FSM__ENABLE_SELF_CHECK 0

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief A fsm for a single variable. It is represented as a triple
  of Expr_ptr

  Private structure, internally used
*/
typedef node_ptr VarFsm_ptr;

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define VAR_FSM(x)  ((VarFsm_ptr) x)


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

#define _PRINT(txt)                             \
  StreamMgr_print_error(streams,  "%s", txt);             \
  fflush(errstream)


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void sexp_fsm_hash_var_fsm_init(SexpFsm_ptr self,
                                       hash_ptr simp_hash);

static void sexp_fsm_const_var_fsm_init(SexpFsm_ptr self,
                                        hash_ptr simp_hash);

static void sexp_fsm_finalize(Object_ptr object, void* dummy);

static Object_ptr sexp_fsm_copy(const Object_ptr object);

static Expr_ptr sexp_fsm_simplify_expr(SexpFsm_ptr self,
                                       hash_ptr hash, Expr_ptr expr,
                                       const int group);

static hash_ptr simplifier_hash_create(void);
static void simplifier_hash_destroy(hash_ptr hash);
static void simplifier_hash_add_expr(hash_ptr hash,
                                     Expr_ptr expr, const int group);
static boolean simplifier_hash_query_expr(hash_ptr hash, Expr_ptr expr,
                                          const int group);

static void sexp_fsm_hash_var_fsm_destroy(SexpFsm_ptr self);
static assoc_retval
sexp_fsm_callback_var_fsm_free(char *key, char *data, char * arg);
static VarFsm_ptr
sexp_fsm_hash_var_fsm_lookup_var(SexpFsm_ptr self, node_ptr var);
static void
sexp_fsm_hash_var_fsm_insert_var(SexpFsm_ptr self,
                                 node_ptr var, VarFsm_ptr varfsm);

static VarFsm_ptr var_fsm_create(SexpFsm_ptr self,
                                 Expr_ptr init, Expr_ptr invar,
                                 Expr_ptr next);

static void var_fsm_destroy(SexpFsm_ptr self, VarFsm_ptr vf);
static Expr_ptr var_fsm_get_init(SexpFsm_ptr self, VarFsm_ptr vf);
static Expr_ptr var_fsm_get_invar(SexpFsm_ptr self, VarFsm_ptr vf);
static Expr_ptr var_fsm_get_next(SexpFsm_ptr self, VarFsm_ptr vf);
static Expr_ptr var_fsm_get_input(SexpFsm_ptr self, VarFsm_ptr vf);
static VarFsm_ptr var_fsm_synchronous_product(SexpFsm_ptr self,
                                              VarFsm_ptr fsm1,
                                              VarFsm_ptr fsm2);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

SexpFsm_ptr SexpFsm_create(const FlatHierarchy_ptr hierarchy, const Set_t vars)
{
  SexpFsm_ptr self;

  /* allocation: */
  self = ALLOC(SexpFsm, 1);
  SEXP_FSM_CHECK_INSTANCE(self);

  /* initialization: */
  sexp_fsm_init(self, hierarchy, vars);

#if SEXP_FSM__ENABLE_SELF_CHECK
  SexpFsm_self_check(self);
#endif

  return self;
}

VIRTUAL SexpFsm_ptr SexpFsm_copy(const SexpFsm_ptr self)
{
  return SEXP_FSM(Object_copy(OBJECT(self)));
}

SexpFsm_ptr
SexpFsm_create_predicate_normalised_copy(const SexpFsm_ptr self,
                                         PredicateNormaliser_ptr normaliser)
{
  SexpFsm_ptr copy;

  SEXP_FSM_CHECK_INSTANCE(self);

  copy = SexpFsm_copy(self);

  FlatHierarchy_set_init(copy->hierarchy,
                         PredicateNormaliser_normalise_expr(normaliser,
                                 FlatHierarchy_get_init(copy->hierarchy)));

  FlatHierarchy_set_invar(copy->hierarchy,
                         PredicateNormaliser_normalise_expr(normaliser,
                                 FlatHierarchy_get_invar(copy->hierarchy)));

  FlatHierarchy_set_trans(copy->hierarchy,
                          PredicateNormaliser_normalise_expr(normaliser,
                                 FlatHierarchy_get_trans(copy->hierarchy)));

  FlatHierarchy_set_input(copy->hierarchy,
                          PredicateNormaliser_normalise_expr(normaliser,
                                 FlatHierarchy_get_input(copy->hierarchy)));

  FlatHierarchy_set_justice(copy->hierarchy,
                            PredicateNormaliser_normalise_expr(normaliser,
                                 FlatHierarchy_get_justice(copy->hierarchy)));

  FlatHierarchy_set_compassion(copy->hierarchy,
                    PredicateNormaliser_normalise_expr(normaliser,
                         FlatHierarchy_get_compassion(copy->hierarchy)));

#if SEXP_FSM__ENABLE_SELF_CHECK
  SexpFsm_self_check(copy);
#endif

  return copy;
}

VIRTUAL void SexpFsm_destroy(SexpFsm_ptr self)
{
  SEXP_FSM_CHECK_INSTANCE(self);
  Object_destroy(OBJECT(self), NULL);
}

boolean SexpFsm_is_boolean(const SexpFsm_ptr self)
{
  SEXP_FSM_CHECK_INSTANCE(self);
  return self->is_boolean;
}

SymbTable_ptr SexpFsm_get_symb_table(const SexpFsm_ptr self)
{
  SEXP_FSM_CHECK_INSTANCE(self);
  return self->st;
}

FlatHierarchy_ptr SexpFsm_get_hierarchy(const SexpFsm_ptr self)
{
  SEXP_FSM_CHECK_INSTANCE(self);
  return self->hierarchy;
}

Expr_ptr SexpFsm_get_init(const SexpFsm_ptr self)
{
  SEXP_FSM_CHECK_INSTANCE(self);
  return FlatHierarchy_get_init(self->hierarchy);
}

Expr_ptr SexpFsm_get_invar(const SexpFsm_ptr self)
{
  SEXP_FSM_CHECK_INSTANCE(self);
  return FlatHierarchy_get_invar(self->hierarchy);
}

Expr_ptr SexpFsm_get_trans(const SexpFsm_ptr self)
{
  SEXP_FSM_CHECK_INSTANCE(self);
  return FlatHierarchy_get_trans(self->hierarchy);
}

Expr_ptr SexpFsm_get_input(const SexpFsm_ptr self)
{
  SEXP_FSM_CHECK_INSTANCE(self);
  /* Currently no constraints over input are allowed, thus we return
     true to inidicate this. */
  return FlatHierarchy_get_input(self->hierarchy);
}

Expr_ptr SexpFsm_get_var_init(const SexpFsm_ptr self, node_ptr var_name)
{
  VarFsm_ptr var_fsm;
  SEXP_FSM_CHECK_INSTANCE(self);

  var_fsm = find_assoc(self->hash_var_fsm, var_name);
  return var_fsm_get_init(self, var_fsm);
}

Expr_ptr SexpFsm_get_var_invar(const SexpFsm_ptr self, node_ptr var_name)
{
  VarFsm_ptr var_fsm;

  SEXP_FSM_CHECK_INSTANCE(self);

  var_fsm = find_assoc(self->hash_var_fsm, var_name);
  return var_fsm_get_invar(self, var_fsm);
}

Expr_ptr SexpFsm_get_var_trans(const SexpFsm_ptr self, node_ptr var_name)
{
  VarFsm_ptr var_fsm;

  SEXP_FSM_CHECK_INSTANCE(self);

  var_fsm = find_assoc(self->hash_var_fsm, var_name);
  return var_fsm_get_next(self, var_fsm);
}

Expr_ptr SexpFsm_get_var_input(const SexpFsm_ptr self, node_ptr var_name)
{
  VarFsm_ptr var_fsm;

  SEXP_FSM_CHECK_INSTANCE(self);

  var_fsm = find_assoc(self->hash_var_fsm, var_name);
  return var_fsm_get_input(self, var_fsm);
}

node_ptr SexpFsm_get_justice(const SexpFsm_ptr self)
{
  SEXP_FSM_CHECK_INSTANCE(self);
  return FlatHierarchy_get_justice(self->hierarchy);
}

node_ptr SexpFsm_get_compassion(const SexpFsm_ptr self)
{
  SEXP_FSM_CHECK_INSTANCE(self);
  return FlatHierarchy_get_compassion(self->hierarchy);
}

NodeList_ptr SexpFsm_get_vars_list(const SexpFsm_ptr self)
{
  SEXP_FSM_CHECK_INSTANCE(self);
  return Set_Set2List(self->vars_set);
}

NodeList_ptr SexpFsm_get_symbols_list(const SexpFsm_ptr self)
{
  SEXP_FSM_CHECK_INSTANCE(self);

  if (NODE_LIST(NULL) == self->symbols) {
    SymbTableIter iter;

    self->symbols = NodeList_create();
    NodeList_concat(self->symbols, Set_Set2List(self->vars_set));

    SYMB_TABLE_FOREACH(self->st, iter, STT_DEFINE) {
      node_ptr def = SymbTable_iter_get_symbol(self->st, &iter);

      nusmv_assert(SymbTable_is_symbol_define(self->st, def));
      NodeList_append(self->symbols, def);
    }
  }

  return self->symbols;
}

Set_t SexpFsm_get_vars(const SexpFsm_ptr self)
{
  SEXP_FSM_CHECK_INSTANCE(self);
  return self->vars_set;
}

void SexpFsm_apply_synchronous_product(SexpFsm_ptr self, SexpFsm_ptr other)
{
  Set_Iterator_t iter;
  node_ptr var;
  VarFsm_ptr fsm_self;
  VarFsm_ptr fsm_other;
  VarFsm_ptr fsm_prod;

  SEXP_FSM_CHECK_INSTANCE(self);
  SEXP_FSM_CHECK_INSTANCE(other);
  nusmv_assert(*(self->family_counter) > 0);

  /* concatenate vars_sets */
  self->vars_set = Set_Union(self->vars_set, other->vars_set);

  /* destroy memoized symbols list */
  if (NODE_LIST(NULL) != self->symbols) {
    NodeList_destroy(self->symbols);
    self->symbols = NODE_LIST(NULL);
  }

  /* Merge const_var_fsm */
  fsm_self = self->const_var_fsm;
  self->const_var_fsm = var_fsm_synchronous_product(self,
                                                    self->const_var_fsm,
                                                    other->const_var_fsm);
  var_fsm_destroy(self, fsm_self);

  /* merge hash_var_fsm
   *
   * [VS] note that the structure of var_fsms (first constrains, then
   * assigns) is not preserved; MR said that's ok */

  SET_FOREACH(self->vars_set, iter) {
    var = Set_GetMember(self->vars_set, iter);

    fsm_self = sexp_fsm_hash_var_fsm_lookup_var(self, var);
    fsm_other = sexp_fsm_hash_var_fsm_lookup_var(other, var);

    fsm_prod = var_fsm_synchronous_product(self, fsm_self, fsm_other);

    if ((Nil != fsm_self) && (*(self->family_counter) == 1)) {
      var_fsm_destroy(self, fsm_self);
    }
    sexp_fsm_hash_var_fsm_insert_var(self, var, fsm_prod);
  }

  /* merge hierarchy */
  FlatHierarchy_mergeinto(self->hierarchy, other->hierarchy);

  /* family_counter: we're not a copy anymore - if we're not the only
     instance, get a fresh family_counter and decrease old one */
  if (*(self->family_counter) > 1) {
    *(self->family_counter) -= 1;
    self->family_counter = ALLOC(int, 1);
    nusmv_assert(self->family_counter != (int*) NULL);
    *(self->family_counter) = 1;
  }
}

boolean SexpFsm_is_syntactically_universal(SexpFsm_ptr self)
{
  NuSMVEnv_ptr env;
  ExprMgr_ptr exprs;
  Expr_ptr init, invar, trans, input, justice, compassion;

  SEXP_FSM_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->st));
  exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  init = SexpFsm_get_init(self);
  if (Nil != init && !ExprMgr_is_true(exprs, init))
    return false;

  invar = SexpFsm_get_invar(self);
  if (Nil != invar && !ExprMgr_is_true(exprs, invar))
    return false;

  trans = SexpFsm_get_trans(self);
  if (Nil != trans && !ExprMgr_is_true(exprs, trans))
    return false;

  input = SexpFsm_get_input(self);
  if (Nil != input && !ExprMgr_is_true(exprs, input))
    return false;

  justice = SexpFsm_get_justice(self);
  if (Nil != justice)
    return false;

  compassion = SexpFsm_get_compassion(self);
  if (Nil != compassion)
    return false;

  return true;
}

void SexpFsm_self_check(const SexpFsm_ptr self)
{
  FlatHierarchy_self_check(self->hierarchy);

  if (!Set_Contains(self->vars_set, FlatHierarchy_get_vars(self->hierarchy))) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
    ErrorMgr_internal_error(errmgr, "SexpFsm failed self-check.");
  }
}


/*---------------------------------------------------------------------------*/
/* Static function definitions                                               */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void sexp_fsm_init(SexpFsm_ptr self,
                   const FlatHierarchy_ptr hierarchy, const Set_t vars_set)
{
  const NuSMVEnv_ptr env = FlatHierarchy_get_environment(hierarchy);
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  /* -------------------------------------------------------------------- */
  /* 0. Initialization                                                    */
  /* -------------------------------------------------------------------- */

  /* base class initialization */
  object_init(OBJECT(self));

  /* inits some private members */
  self->st = FlatHierarchy_get_symb_table(hierarchy);
  self->hierarchy = FlatHierarchy_copy(hierarchy);
  self->vars_set = Set_Copy(vars_set);
  self->symbols = NODE_LIST(NULL);

  self->inlining = opt_symb_inlining(opts);
  self->is_boolean = false;

  self->hash_var_fsm = new_assoc();
  self->const_var_fsm = VAR_FSM(NULL);

  self->family_counter = ALLOC(int, 1);
  nusmv_assert(self->family_counter != (int*) NULL);
  *(self->family_counter) = 1; /* this is adam for this family */

  /* -------------------------------------------------------------------- */
  /* 1. Simplification                                                    */
  /* -------------------------------------------------------------------- */
  {
    hash_ptr hash = simplifier_hash_create();
    Set_Iterator_t iter;

    /* sets up and simplifies the single variable FSMs */
    sexp_fsm_hash_var_fsm_init(self, hash);

    /* Sets up and simplifies the constants FSM */
    sexp_fsm_const_var_fsm_init(self, hash);

    /* init */
    FlatHierarchy_set_init(self->hierarchy,
                           sexp_fsm_simplify_expr(self, hash,
                                                  FlatHierarchy_get_init(self->hierarchy), INIT));

    /* invar */
    FlatHierarchy_set_invar(self->hierarchy,
                            sexp_fsm_simplify_expr(self, hash,
                                                   FlatHierarchy_get_invar(self->hierarchy), INVAR));

    /* trans */
    FlatHierarchy_set_trans(self->hierarchy,
                            sexp_fsm_simplify_expr(self, hash,
                                                   FlatHierarchy_get_trans(self->hierarchy), TRANS));

    /* now integrates information coming from the variables FSMs
       (assign and constraints) and the constant FSM */
    /* [MR]: Here we do side effect on the hierarchy stored within the SexpFSM
       as to provide */
    /* [MR]: Sexp_get_init/invar/trans to return the whole FSM with exprs and assings */

    /* inits */
    FlatHierarchy_set_init(self->hierarchy,
                           ExprMgr_and_nil(exprs, FlatHierarchy_get_init(self->hierarchy),
                                           var_fsm_get_init(self, self->const_var_fsm)));
    /* invars */
    FlatHierarchy_set_invar(self->hierarchy,
                            ExprMgr_and_nil(exprs, FlatHierarchy_get_invar(self->hierarchy),
                                            var_fsm_get_invar(self, self->const_var_fsm)));
    /* next */
    FlatHierarchy_set_trans(self->hierarchy,
                            ExprMgr_and_nil(exprs, FlatHierarchy_get_trans(self->hierarchy),
                                            var_fsm_get_next(self, self->const_var_fsm)));

    SET_FOREACH(self->vars_set, iter) {
      node_ptr var = Set_GetMember(self->vars_set, iter);
      VarFsm_ptr varfsm = sexp_fsm_hash_var_fsm_lookup_var(self, var);

      if (varfsm != VAR_FSM(NULL)) {
        Expr_ptr tmp;

        /* inits */
        tmp = var_fsm_get_init(self, varfsm);
        FlatHierarchy_set_init(self->hierarchy,
                ExprMgr_and_nil(exprs, FlatHierarchy_get_init(self->hierarchy),
                             tmp));
        /* invars */
        tmp = var_fsm_get_invar(self, varfsm);
        FlatHierarchy_set_invar(self->hierarchy,
                      ExprMgr_and_nil(exprs, FlatHierarchy_get_invar(self->hierarchy),
                                   tmp));
        /* next */
        tmp = var_fsm_get_next(self, varfsm);
        FlatHierarchy_set_trans(self->hierarchy,
                      ExprMgr_and_nil(exprs, FlatHierarchy_get_trans(self->hierarchy),
                                   tmp));
      }
    } /* loop over vars */

    simplifier_hash_destroy(hash);
  }

  /* -------------------------------------------------------------------- */
  /* 2. Inlining                                                          */
  /* -------------------------------------------------------------------- */
  if (self->inlining) {
    SexpInliner_ptr inliner;
    node_ptr invar, trans, init;
    InlineRes_ptr invar_res, init_res, trans_res;

    /* Create a fixpoint inliner */
    inliner = SexpInliner_create(self->st, 0);

    /* Get the non-inlined expressions */
    invar = FlatHierarchy_get_invar(self->hierarchy);
    init = FlatHierarchy_get_init(self->hierarchy);
    trans = FlatHierarchy_get_trans(self->hierarchy);

    /* Inline invariant */
    invar_res = SexpInliner_inline(inliner, invar, NULL);

    /* Inline init without clearing the inliner since invariants
       equivalences still hold */
    init_res = SexpInliner_inline(inliner, init, NULL);

    /* Reset inliner */
    SexpInliner_clear_equivalences(inliner);
    SexpInliner_clear_invariants(inliner);

    /* Re-learn invariant equivalences */
    /* TODO[MD] Probably it should be better to check the ret val */
    (void)SexpInliner_force_equivalences(inliner,
                                   InlineRes_get_equivalences(invar_res));
    (void)SexpInliner_force_invariants(inliner, InlineRes_get_invariants(invar_res));

    /* Inline trans (with re-learned invariants) */
    trans_res = SexpInliner_inline(inliner, trans, NULL);

    /* Set inlined expressions */
    FlatHierarchy_set_invar(self->hierarchy, InlineRes_get_result(invar_res));
    FlatHierarchy_set_init(self->hierarchy, InlineRes_get_result(init_res));
    FlatHierarchy_set_trans(self->hierarchy, InlineRes_get_result(trans_res));

    /* Cleanup */
    InlineRes_destroy(invar_res);
    InlineRes_destroy(init_res);
    InlineRes_destroy(trans_res);
    SexpInliner_destroy(inliner);
  }

  /* -------------------------------------------------------------------- */
  /* 3. Other initializations                                             */
  /* -------------------------------------------------------------------- */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = sexp_fsm_finalize;
  OVERRIDE(Object, copy) = sexp_fsm_copy;
}

void sexp_fsm_deinit(SexpFsm_ptr self)
{
  nusmv_assert(*(self->family_counter) > 0);
  *(self->family_counter) -= 1; /* self de-ref */

  if (VAR_FSM(NULL) != self->const_var_fsm) {
    var_fsm_destroy(self, self->const_var_fsm);
  }

  sexp_fsm_hash_var_fsm_destroy(self);
  FlatHierarchy_destroy(self->hierarchy);
  Set_ReleaseSet(self->vars_set);

  if (*(self->family_counter) == 0) {
    FREE(self->family_counter);
    self->family_counter = (int*) NULL;
  }

  if (NODE_LIST(NULL) != self->symbols) {
    NodeList_destroy(self->symbols);
    self->symbols = NODE_LIST(NULL);
  }
}

void sexp_fsm_copy_aux(const SexpFsm_ptr self, SexpFsm_ptr copy)
{
  /* copies the base class: */
  object_copy_aux(OBJECT(self), OBJECT(copy));

  /* copies private members */
  copy->st = self->st;
  copy->vars_set   = Set_Copy(self->vars_set);
  copy->symbols    = NODE_LIST(NULL);
  copy->hierarchy = FlatHierarchy_copy(self->hierarchy);

  /* This could be a bug, see issue 4479 */
  copy->hash_var_fsm = copy_assoc(self->hash_var_fsm);
  copy->const_var_fsm = var_fsm_create(self,
                                       var_fsm_get_init(self, self->const_var_fsm),
                                       var_fsm_get_invar(self, self->const_var_fsm),
                                       var_fsm_get_next(self, self->const_var_fsm));

  copy->inlining = self->inlining;
  copy->is_boolean = self->is_boolean;

  /* increments family members */
  copy->family_counter = self->family_counter;
  *(self->family_counter) += 1;

  /* copies local virtual methods */
}



/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief This is called by the virtual copy constructor


*/
static Object_ptr sexp_fsm_copy(const Object_ptr object)
{
  SexpFsm_ptr self = SEXP_FSM(object);
  SexpFsm_ptr copy;

  SEXP_FSM_CHECK_INSTANCE(self);

  copy = ALLOC(SexpFsm, 1);
  SEXP_FSM_CHECK_INSTANCE(copy);

  sexp_fsm_copy_aux(self, copy);
  return OBJECT(copy);
}

/*!
  \brief The SexpFsm class virtual finalizer

  Called by the class destructor
*/
static void sexp_fsm_finalize(Object_ptr object, void* dummy)
{
  SexpFsm_ptr self = SEXP_FSM(object);

  sexp_fsm_deinit(self);
  FREE(self);
}

/*!
  \brief Initializes the const_var_fsm field

  Formulae are simplified through
                 sexp_fsm_simplify_expr. For this reason a
                 simplification hash is required as input
*/
static void sexp_fsm_const_var_fsm_init(SexpFsm_ptr self, hash_ptr simp_hash)
{
  NuSMVEnv_ptr env =
      EnvObject_get_environment(ENV_OBJECT(self->st));
  ExprMgr_ptr exprs =
      EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  node_ptr invar_const =
    FlatHierarchy_lookup_constant_constrains(self->hierarchy, INVAR);
  node_ptr trans_const =
    FlatHierarchy_lookup_constant_constrains(self->hierarchy, TRANS);
  node_ptr init_const =
    FlatHierarchy_lookup_constant_constrains(self->hierarchy, INIT);

  if (invar_const == Nil)
    invar_const = ExprMgr_true(exprs);
  if (trans_const == Nil)
    trans_const = ExprMgr_true(exprs);
  if (init_const == Nil)
    init_const = ExprMgr_true(exprs);

  invar_const = sexp_fsm_simplify_expr(self, simp_hash, invar_const, INVAR);
  init_const = sexp_fsm_simplify_expr(self, simp_hash, init_const, INIT);
  trans_const = sexp_fsm_simplify_expr(self, simp_hash, trans_const, TRANS);

  self->const_var_fsm =
      var_fsm_create(self, init_const, invar_const, trans_const);
}

/*!
  \brief Initializes the vars fsm hash

  Formulae are simplified through
                 sexp_fsm_simplify_expr. For this reason a
                 simplification hash is required as input
*/
static void sexp_fsm_hash_var_fsm_init(SexpFsm_ptr self, hash_ptr simp_hash)
{
  Set_Iterator_t iter;
  SymbTable_ptr symb_table = SexpFsm_get_symb_table(self);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  SET_FOREACH(self->vars_set, iter) {
    int saved_yylineno = nusmv_yylineno;

    VarFsm_ptr var_fsm;

    node_ptr var_name = Set_GetMember(self->vars_set, iter);
    node_ptr var_name_i = find_node(nodemgr, SMALLINIT, var_name, Nil);
    node_ptr var_name_n = ExprMgr_next(exprs, var_name, symb_table);

    node_ptr init_a =
      FlatHierarchy_lookup_assign(self->hierarchy, var_name_i);

    node_ptr invar_a =
      FlatHierarchy_lookup_assign(self->hierarchy, var_name);

    node_ptr next_a =
      FlatHierarchy_lookup_assign(self->hierarchy, var_name_n);

    node_ptr init_c =
      FlatHierarchy_lookup_constrains(self->hierarchy, var_name_i);

    node_ptr invar_c =
      FlatHierarchy_lookup_constrains(self->hierarchy, var_name);

    node_ptr next_c =
      FlatHierarchy_lookup_constrains(self->hierarchy, var_name_n);

    Expr_ptr init_sexp  = ExprMgr_true(exprs);
    Expr_ptr invar_sexp = ExprMgr_true(exprs);
    Expr_ptr trans_sexp = ExprMgr_true(exprs);

    /* add all the constrains */
    if (Nil != init_c) init_sexp = ExprMgr_and(exprs, init_sexp, EXPR(init_c));
    if (Nil != invar_c) invar_sexp = ExprMgr_and(exprs, invar_sexp, EXPR(invar_c));
    if (Nil != next_c)  trans_sexp = ExprMgr_and(exprs, trans_sexp, EXPR(next_c));

    /* add all assignments */
    if (Nil != init_a) {
      nusmv_yylineno = init_a->lineno;
      init_sexp = ExprMgr_and(exprs, init_sexp,
                           EXPR(find_node(nodemgr, EQDEF, var_name_i, init_a)));
    }

    if (Nil != invar_a) {
      nusmv_yylineno = invar_a->lineno;
      invar_sexp = ExprMgr_and(exprs, invar_sexp,
                            EXPR(new_node(nodemgr, EQDEF, var_name, invar_a)));
    }

    if (Nil != next_a) {
      nusmv_yylineno = next_a->lineno;
      trans_sexp = ExprMgr_and(exprs, trans_sexp,
                            EXPR(new_node(nodemgr, EQDEF, var_name_n, next_a)));
    }

    /* simplification */
    init_sexp = sexp_fsm_simplify_expr(self, simp_hash, init_sexp, INIT);
    invar_sexp = sexp_fsm_simplify_expr(self, simp_hash, invar_sexp, INVAR);
    trans_sexp = sexp_fsm_simplify_expr(self, simp_hash, trans_sexp, TRANS);

    /* inserts the var fsm inside the hash table */
    var_fsm = var_fsm_create(self,
                             init_sexp, invar_sexp, trans_sexp);
    sexp_fsm_hash_var_fsm_insert_var(self, var_name, var_fsm);

    nusmv_yylineno = saved_yylineno;
  } /* loop */
}

/*!
  \brief removes duplicates from expression containing AND nodes

  group identifies INVAR, TRANS or INIT group.
*/
static Expr_ptr
sexp_fsm_simplify_expr(SexpFsm_ptr self, hash_ptr hash, Expr_ptr expr,
                       const int group)
{
  Expr_ptr result;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if ((expr == EXPR(NULL)) || simplifier_hash_query_expr(hash, expr, group)) {
    result = ExprMgr_true(exprs);
  }
  else {
    switch (node_get_type(NODE_PTR(expr))) {
    case AND:
      {
        Expr_ptr left = sexp_fsm_simplify_expr(self, hash, car(NODE_PTR(expr)),
                                               group);
        Expr_ptr right = sexp_fsm_simplify_expr(self, hash, cdr(NODE_PTR(expr)),
                                                group);
        result = ExprMgr_and(exprs, left, right);
        break;
      }

    default:
      result = expr;
    } /* switch */

    simplifier_hash_add_expr(hash, expr, group);
  }

  return result;
}

/*!
  \brief This is used when creating cluster list from vars list


*/
static hash_ptr simplifier_hash_create()
{
  hash_ptr result;

  result = st_init_table(st_ptrcmp, st_ptrhash);
  nusmv_assert(result != ((hash_ptr) NULL));

  return result;
}

/*!
  \brief Call after sexp_fsm_cluster_hash_create


*/
static void simplifier_hash_destroy(hash_ptr hash)
{
  nusmv_assert(hash != (hash_ptr) NULL);
  st_free_table(hash);
}

/*!
  \brief To insert a new node in the hash

  group is INIT, INVAR or TRANS

  \se The hash can change
*/
static void
simplifier_hash_add_expr(hash_ptr hash, Expr_ptr expr, const int group)
{
  int res;

  res = st_add_direct(hash, (char*) expr, PTR_FROM_INT(char*, group));
  nusmv_assert(res != ST_OUT_OF_MEM);
}

/*!
  \brief Queries for an element in the hash, returns True if
  found


*/
static boolean
simplifier_hash_query_expr(hash_ptr hash, Expr_ptr expr,
                           const int group)
{
  nusmv_ptrint hashed_group;
  boolean result;

  result = st_lookup(hash, (char*) expr, (char**) &hashed_group);

  /* groups are checked consecutively, i.e. at first, *all* INIT expressions
     are checked, then *all* INVAR, and then *all* TRANS. So hash_group
     will not interfere with each other
  */
  return (result && ((int) hashed_group == group));
}

/*!
  \brief Call to destroy the var fsm hash

  Private method, used internally
*/
static void sexp_fsm_hash_var_fsm_destroy(SexpFsm_ptr self)
{
  nusmv_assert(self->hash_var_fsm != (hash_ptr) NULL);

  if (*(self->family_counter) == 0) {
    clear_assoc_and_free_entries_arg(self->hash_var_fsm,
                                     sexp_fsm_callback_var_fsm_free,
                                     (char*)self);
  }
  free_assoc(self->hash_var_fsm);
}

/*!
  \brief Private callback that destroys a single variable fsm
  contained into the var fsm hash


*/
static assoc_retval sexp_fsm_callback_var_fsm_free(char *key,
                                                   char *data, char * arg)
{
  VarFsm_ptr varfsm = VAR_FSM(data);
  SexpFsm_ptr self = SEXP_FSM(arg);

  var_fsm_destroy(self, varfsm);
  return ASSOC_DELETE;
}

/*!
  \brief Given a variable name, returns the corresponding variable
  fsm, or NULL if not found


*/
static VarFsm_ptr
sexp_fsm_hash_var_fsm_lookup_var(SexpFsm_ptr self, node_ptr var)
{
  nusmv_assert(self->hash_var_fsm != (hash_ptr) NULL);

  return VAR_FSM( find_assoc(self->hash_var_fsm, var) );
}

/*!
  \brief Adds a var fsm to the internal hash. Private.


*/
static void
sexp_fsm_hash_var_fsm_insert_var(SexpFsm_ptr self,
                                 node_ptr var, VarFsm_ptr varfsm)
{
  nusmv_assert(self->hash_var_fsm != (hash_ptr) NULL);

  insert_assoc(self->hash_var_fsm, var, varfsm);
}

/*!
  \brief Creates a var fsm


*/
static VarFsm_ptr var_fsm_create(SexpFsm_ptr self,
                                 Expr_ptr init,
                                 Expr_ptr invar,
                                 Expr_ptr next)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  return VAR_FSM( cons(nodemgr, NODE_PTR(init),
                       cons(nodemgr, NODE_PTR(invar), NODE_PTR(next))) );
}

/*!
  \brief It does not destroy the init, trans and invar nodes.
  It destroys only the support nodes


*/
static void var_fsm_destroy(SexpFsm_ptr self,
                            VarFsm_ptr vfsm)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr node = NODE_PTR(vfsm);

  free_node(nodemgr, cdr(node));
  free_node(nodemgr, node);
}

/*!
  \brief


*/
static Expr_ptr var_fsm_get_init(SexpFsm_ptr self, VarFsm_ptr vf)
{
  return EXPR( car(NODE_PTR(vf)) );
}

/*!
  \brief


*/
static Expr_ptr var_fsm_get_invar(SexpFsm_ptr self, VarFsm_ptr vf)
{
  return EXPR( car(cdr(NODE_PTR(vf))) );
}

/*!
  \brief


*/
static Expr_ptr var_fsm_get_next(SexpFsm_ptr self, VarFsm_ptr vf)
{
  return EXPR( cdr(cdr(NODE_PTR(vf))) );
}

/*!
  \brief


*/
static Expr_ptr var_fsm_get_input(SexpFsm_ptr self, VarFsm_ptr vf)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  /* Currently no constraints over input are allowed, thus we return
     true to inidicate this. */
  return ExprMgr_true(exprs);
}

/*!
  \brief Returns new var fsm that is synchronous product of var
  fsms.

  Any argument can be Nil. When both are Nil the product
  has all arguments true.
*/
static VarFsm_ptr var_fsm_synchronous_product(SexpFsm_ptr self,
                                              VarFsm_ptr fsm1,
                                              VarFsm_ptr fsm2)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  VarFsm_ptr prod;
  node_ptr prod_init;
  node_ptr prod_invar;
  node_ptr prod_next;

  if (Nil == fsm1 && Nil == fsm2) {
    prod_init = ExprMgr_true(exprs);
    prod_invar = ExprMgr_true(exprs);
    prod_next = ExprMgr_true(exprs);
  } else if (Nil == fsm1) {
    prod_init = var_fsm_get_init(self, fsm2);
    prod_invar = var_fsm_get_invar(self, fsm2);
    prod_next = var_fsm_get_next(self, fsm2);
  } else if (Nil == fsm2) {
    prod_init = var_fsm_get_init(self, fsm1);
    prod_invar = var_fsm_get_invar(self, fsm1);
    prod_next = var_fsm_get_next(self, fsm1);
  } else {
    prod_init = NODE_PTR(ExprMgr_and_nil(exprs, EXPR(var_fsm_get_init(self, fsm1)),
                                      EXPR(var_fsm_get_init(self, fsm2))));
    prod_invar = NODE_PTR(ExprMgr_and_nil(exprs, EXPR(var_fsm_get_invar(self, fsm1)),
                                       EXPR(var_fsm_get_invar(self, fsm2))));
    prod_next = NODE_PTR(ExprMgr_and_nil(exprs, EXPR(var_fsm_get_next(self, fsm1)),
                                      EXPR(var_fsm_get_next(self, fsm2))));
  }

  prod = var_fsm_create(self,
                        prod_init,
                        prod_invar,
                        prod_next);
  nusmv_assert(NULL != prod);

  return prod;
}
