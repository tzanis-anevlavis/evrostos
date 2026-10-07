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
  \author Roberto Cavada
  \brief Implementation of class 'BoolSexpFsm'

  This module defines a class representing a boolean
               sexp-based FSM. The class BoolSexpFsm derives from
               SexpFsm.

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/fsm/sexp/BoolSexpFsm.h"
#include "nusmv/core/fsm/sexp/BoolSexpFsm_private.h"

#include "nusmv/core/fsm/sexp/sexpInt.h"

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
/* See 'BoolSexpFsm_private.h' for class 'BoolSexpFsm' definition. */

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

static void bool_sexp_fsm_finalize(Object_ptr object, void* dummy);

static Object_ptr bool_sexp_fsm_copy(const Object_ptr object);

static Expr_ptr
bool_sexp_fsm_booleanize_expr(const BoolSexpFsm_ptr self,
                              Expr_ptr expr);

static void
bool_sexp_fsm_build_input_state_mask(BoolSexpFsm_ptr self,
                                     Expr_ptr *input,
                                     Expr_ptr *state);
static boolean
bool_sexp_fsm_set_contains_infinite_variables(const SymbTable_ptr st,
                                              const Set_t vars);
/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

BoolSexpFsm_ptr BoolSexpFsm_create(const FlatHierarchy_ptr hierarchy,
                                   const Set_t vars_set,
                                   BddEnc_ptr benc,
                                   SymbLayer_ptr det_layer)
{
  BoolSexpFsm_ptr self = ALLOC(BoolSexpFsm, 1);

  BOOL_SEXP_FSM_CHECK_INSTANCE(self);

  bool_sexp_fsm_init(self, hierarchy, vars_set, benc, det_layer);
  return self;
}

BoolSexpFsm_ptr
BoolSexpFsm_create_from_scalar_fsm(const SexpFsm_ptr scalar_fsm,
                                   BddEnc_ptr benc, SymbLayer_ptr det_layer)
{
  BoolSexpFsm_ptr self;

  if (SexpFsm_is_boolean(scalar_fsm)) {
    /* the instance is already a BoolSexpFsm */
    return BOOL_SEXP_FSM(SexpFsm_copy(scalar_fsm));
  }

  self = ALLOC(BoolSexpFsm, 1);
  BOOL_SEXP_FSM_CHECK_INSTANCE(self);

  bool_sexp_fsm_init(self, scalar_fsm->hierarchy, scalar_fsm->vars_set,
                     benc, det_layer);
  return self;
}

BoolSexpFsm_ptr BoolSexpFsm_copy(BoolSexpFsm_ptr self)
{
  BOOL_SEXP_FSM_CHECK_INSTANCE(self);
  return BOOL_SEXP_FSM(Object_copy(OBJECT(self)));
}

VIRTUAL void BoolSexpFsm_destroy(BoolSexpFsm_ptr self)
{
  BOOL_SEXP_FSM_CHECK_INSTANCE(self);
  Object_destroy(OBJECT(self), NULL);
}

BoolEnc_ptr BoolSexpFsm_get_bool_enc(const BoolSexpFsm_ptr self)
{
  BOOL_SEXP_FSM_CHECK_INSTANCE(self);
  return BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(self->enc));
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void bool_sexp_fsm_init(BoolSexpFsm_ptr self,
                        const FlatHierarchy_ptr hierarchy,
                        const Set_t vars_set,
                        BddEnc_ptr enc, SymbLayer_ptr det_layer)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  FlatHierarchy_ptr fh;
  int curr_verbosity;
  Expr_ptr inputs_mask = ExprMgr_true(exprs);
  Expr_ptr states_mask = ExprMgr_true(exprs);
  SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(enc));

  if (bool_sexp_fsm_set_contains_infinite_variables(st, vars_set)) {
    ErrorMgr_rpterr(errmgr, "Impossible to build a boolean FSM"
           " with infinite precision variables");
  }

  if (SymbTable_get_functions_num(st) > 0) {
    ErrorMgr_rpterr(errmgr, "Impossible to build a boolean FSM"
           " with uninterpreted functions");
  }

  /* base class initialization. Here sele is made a copy of the
     scalar fsm is done. Later on the copy will be booleanized. */
  sexp_fsm_init(SEXP_FSM(self), hierarchy, vars_set);

  /* local members */
  self->enc = enc;
  self->det_layer = det_layer;

  /* here we get the hierarchy instead of using the one passed as
     formal parameter, as we need to use the local copy within the
     base class */
  fh = SexpFsm_get_hierarchy(SEXP_FSM(self));

  /* marks the sexp fsm to be a boolean fsm */
  SEXP_FSM(self)->is_boolean = true;

  /* We set the verbose level to 0 and then we restore the original
     value. This because booleanization uses eval */
  curr_verbosity = get_verbose_level(opts);
  set_verbose_level(opts, 0);

  /*
     input and state mask for the FSM are computed and added to the
     model: input mask is added to the transition relation, while
     state mask is added to the invar.
  */
  bool_sexp_fsm_build_input_state_mask(self, &inputs_mask, &states_mask);

  /* here the flat hierarchy gets booleanized */

  /* init */
  FlatHierarchy_set_init(fh, bool_sexp_fsm_booleanize_expr(self,
                                         FlatHierarchy_get_init(fh)));

  /* invar */
  FlatHierarchy_set_invar(fh,
                          ExprMgr_and(exprs, states_mask,
                                   bool_sexp_fsm_booleanize_expr(self,
                                         FlatHierarchy_get_invar(fh))));

  /* trans */
  FlatHierarchy_set_trans(fh,
                          ExprMgr_and(exprs, inputs_mask,
                                   bool_sexp_fsm_booleanize_expr(self,
                                         FlatHierarchy_get_trans(fh))));

  /* justice */
  FlatHierarchy_set_justice(fh,
                            bool_sexp_fsm_booleanize_expr(self,
                                         FlatHierarchy_get_justice(fh)));

  /* compassion */
  FlatHierarchy_set_compassion(fh,
                               bool_sexp_fsm_booleanize_expr(self,
                                         FlatHierarchy_get_compassion(fh)));

  /* restores the verbosity level */
  set_verbose_level(opts, curr_verbosity);

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = bool_sexp_fsm_finalize;
  OVERRIDE(Object, copy)     = bool_sexp_fsm_copy;
}

void bool_sexp_fsm_deinit(BoolSexpFsm_ptr self)
{
  /* members deinitialization */

  /* base class deinitialization */
  sexp_fsm_deinit(SEXP_FSM(self));
}

void bool_sexp_fsm_copy_aux(const BoolSexpFsm_ptr self, BoolSexpFsm_ptr copy)
{
  /* copies the base class: */
  sexp_fsm_copy_aux(SEXP_FSM(self), SEXP_FSM(copy));

  /* copies private members */
  copy->enc = self->enc;
  copy->det_layer = self->det_layer;

  /* copies local virtual methods */
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief This is called by the virtual copy constructor

  
*/
static Object_ptr bool_sexp_fsm_copy(const Object_ptr object)
{
  BoolSexpFsm_ptr self = BOOL_SEXP_FSM(object);
  BoolSexpFsm_ptr copy;

  BOOL_SEXP_FSM_CHECK_INSTANCE(self);

  copy = ALLOC(BoolSexpFsm, 1);
  BOOL_SEXP_FSM_CHECK_INSTANCE(copy);

  bool_sexp_fsm_copy_aux(self, copy);
  return OBJECT(copy);
}

/*!
  \brief The BoolSexpFsm class virtual finalizer

  Called by the class destructor
*/
static void bool_sexp_fsm_finalize(Object_ptr object, void* dummy)
{
  BoolSexpFsm_ptr self = BOOL_SEXP_FSM(object);

  bool_sexp_fsm_deinit(self);
  FREE(self);
}

/*!
  \brief Booleanizes the given expression, keeping each top level
  part of a possible conjuction

  If the fsm is not boolean, the input expression is
  returned
*/
static Expr_ptr bool_sexp_fsm_booleanize_expr(BoolSexpFsm_ptr self,
                                              Expr_ptr expr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->enc));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  Expr_ptr result;

  if (expr == NODE_PTR(NULL)) return NODE_PTR(NULL);

  switch (node_get_type(NODE_PTR(expr))) {
  case AND:
    {
      Expr_ptr left  = bool_sexp_fsm_booleanize_expr(self, car(NODE_PTR(expr)));
      Expr_ptr right = bool_sexp_fsm_booleanize_expr(self, cdr(NODE_PTR(expr)));
      result = ExprMgr_and(exprs, left, right);
      break;
    }

  default:
    result = EXPR(Compile_expr2bexpr(self->enc, self->det_layer,
                                     NODE_PTR(expr)));
  } /* switch */

  return result;
}

/*!
  \brief Computes the mask for the variables of the FSM.

  For the variables in the vars_set of the FSM computes the
  mask and accumulate them into input or state depending on the kind
  of the variable being respectively input or state.
*/
static void bool_sexp_fsm_build_input_state_mask(BoolSexpFsm_ptr self,
                                                 Expr_ptr *input,
                                                 Expr_ptr *state)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->enc));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  Set_t vars = SexpFsm_get_vars(SEXP_FSM(self));
  node_ptr var, mask;
  Set_Iterator_t iter;
  SymbTable_ptr st = SexpFsm_get_symb_table(SEXP_FSM(self));
  BoolEnc_ptr bool_enc = BoolSexpFsm_get_bool_enc(self);

  SET_FOREACH(vars, iter) {
    var = Set_GetMember(vars, iter);

    if (SymbTable_is_symbol_state_var(st, var)) {
      mask = BoolEnc_get_var_mask(bool_enc, var);
      *state = ExprMgr_and(exprs, *state, mask);
    }
    else if (SymbTable_is_symbol_input_var(st, var)) {
      mask = BoolEnc_get_var_mask(bool_enc, var);
      *input = ExprMgr_and(exprs, *input, mask);
    }
  }
#if defined BOOL_FSM_DEBUG_MASK && BOOL_FSM_DEBUG_MASK
  StreamMgr_print_error(streams,  "Input mask is: ");
  StreamMgr_nprint_error(streams, wffprint, "%N", *input);
  StreamMgr_print_error(streams,  "\n");
  StreamMgr_print_error(streams,  "State mask is: ");
  StreamMgr_nprint_error(streams, wffprint, "%N", *state);
  StreamMgr_print_error(streams,  "\n");
#endif
}

/*!
  \brief Checks if the given set of variables contains at least
                       one infinite precision variable

  Checks if the given set of variables contains at least
                       one infinite precision variable
*/
static boolean
bool_sexp_fsm_set_contains_infinite_variables(const SymbTable_ptr st,
                                              const Set_t vars)
{
  Set_Iterator_t iter;

  SET_FOREACH(vars, iter) {
    node_ptr var = Set_GetMember(vars, iter);
    SymbType_ptr type;

    nusmv_assert(SymbTable_is_symbol_var(st, var));

    type = SymbTable_get_var_type(st, var);

    if (SymbType_is_infinite_precision(type) ||
        SymbType_is_wordarray(type) ||
        SymbType_is_intarray(type)) {
      return true;
    }
  }

  return false;
}

/**AutomaticEnd***************************************************************/

