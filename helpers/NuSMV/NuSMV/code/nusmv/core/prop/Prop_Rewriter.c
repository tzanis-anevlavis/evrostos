/* ---------------------------------------------------------------------------


  This file is part of the ``prop'' package of NuSMV version 2.
  Copyright (C) 2013 by FBK-irst.

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
  \author Michele Dorigatti, Andrea Micheli, Marco Roveri, Andrei Tchaltsev
  \brief Implementation of class 'Prop_Rewriter'

  This class rewrites a property in such a way it can be processed
  by all the algorithms in the system.
  At the moment are handled:
  * input variables
  * next operator

  This is done by copying the original property and modifying both the formula and
  the fsms. In the formula, the predicates containing something not processable
  directly by the model checking algorithms are substited by a fresh monitor
  variable. Then, in the fsms, the transition relation is enriched with the
  condition:

  monitor <-> predicate

  This is done with two slightly different methods (see enum
  WffRewriteMethod).

  The typical use pattern is the following:

  Prop_Rewriter_ptr rewriter;

  ******************************************************************************
  * here we create the rewriter and rewrite the property
  ******************************************************************************
  rewriter = Prop_Rewriter_create(env, prop, WFF_REWRITE_METHOD_SOME_METHOD,
                                  FSM_TYPE_SOME_FSM, bdd_enc);
  prop = Prop_Rewriter_rewrite(rewriter);

  ******************************************************************************
  * here is executed the algorithm
  ******************************************************************************
  ...

  ******************************************************************************
  * here the original property is updated with the results of model checking. This
  * call have to be done, otherwise the destructor will abort
  ******************************************************************************
  Prop_Rewriter_update_original_property(rewriter);
  Prop_Rewriter_destroy(rewriter); rewriter = NULL

  The original property is referenced in self and can always be accessed.

  At the moment only invar and ltl properties are processed, self will return a
  copy of the original property in the other cases.

*/


#include "nusmv/core/utils/defs.h"
#include "nusmv/core/prop/Prop_Rewriter.h"
#include "nusmv/core/prop/Prop_Rewriter_private.h"

#include "nusmv/core/prop/propInt.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/compile/FlatHierarchy.h"
#include "nusmv/core/compile/compile.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'Prop_Rewriter_private.h' for class 'Prop_Rewriter' definition. */


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/


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

static void prop_rewriter_finalize(Object_ptr object, void* dummy);
static void pr_free_node(NuSMVEnv_ptr env,
                         NodeMgr_ptr nodemgr,
                         node_ptr expr);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

Prop_Rewriter_ptr Prop_Rewriter_create(NuSMVEnv_ptr env,
                                       Prop_ptr prop,
                                       WffRewriteMethod method,
                                       WffRewriterExpectedProperty expprop,
                                       FsmType fsm_type,
                                       BddEnc_ptr bddenc)
{
  Prop_Rewriter_ptr self = ALLOC(Prop_Rewriter, 1);
  PROP_REWRITER_CHECK_INSTANCE(self);

  prop_rewriter_init(self, env, prop, method, expprop, fsm_type, bddenc, NULL);
  return self;
}

void Prop_Rewriter_destroy(Prop_Rewriter_ptr self)
{
  PROP_REWRITER_CHECK_INSTANCE(self);

  {
    /* This is needed since self is FREEd by Object_destroy, and this
       will possibly result in reading of corrupted memory */
    SymbTable_ptr symb_table = self->symb_table;
    const Prop_ptr original = self->original;
    WffRewriterExpectedProperty expprop = self->expprop;
    const char * layer_name =
      (NULL != self->layer) ? SymbLayer_get_name(self->layer) : NULL;

    Object_destroy(OBJECT(self), NULL);

    if (NULL != layer_name &&
        SymbTable_has_layer(symb_table, layer_name) &&
        /* If input property did not need rewriting, we never created the layer!
           Another rewriter created it! */
        ((Prop_needs_rewriting(symb_table, original) ||
          (WFF_REWRITER_LTL_2_INVAR == expprop )))) {
      SymbLayer_ptr layer = SymbTable_get_layer(symb_table, layer_name);

      SymbTable_remove_layer(symb_table, layer);
      /* There is no need to destroy the layer since it is done by
         SymbTable_remove_layer */
    }
  }
}

Prop_ptr Prop_Rewriter_get_original_property(Prop_Rewriter_ptr self)
{
  PROP_REWRITER_CHECK_INSTANCE(self);

  return self->original;
}

Prop_ptr Prop_Rewriter_rewrite(Prop_Rewriter_ptr self)
{
  PROP_REWRITER_CHECK_INSTANCE(self);

  return self->rewrite(self);
}

void Prop_Rewriter_update_original_property(Prop_Rewriter_ptr self)
{
  PROP_REWRITER_CHECK_INSTANCE(self);
  nusmv_assert(NULL != self->rewritten);

  Prop_set_trace(self->original, Prop_get_trace(self->rewritten));
  Prop_set_status(self->original, Prop_get_status(self->rewritten));
  self->is_status_consistent = true;
}

void Prop_Rewriter_make_monitor_vars_visible(Prop_Rewriter_ptr self) {
  PROP_REWRITER_CHECK_INSTANCE(self);
  self->monitor_visible_in_traces = true;
}

void Prop_Rewriter_make_monitor_vars_invisible(Prop_Rewriter_ptr self) {
  PROP_REWRITER_CHECK_INSTANCE(self);
  self->monitor_visible_in_traces = false;
}

void Prop_Rewriter_initialize_monitor_vars_to_true(Prop_Rewriter_ptr self) {
  PROP_REWRITER_CHECK_INSTANCE(self);
  self->monitor_variable_initialized_to_true = true;
}

void Prop_Rewriter_initialize_monitor_vars_to_false(Prop_Rewriter_ptr self) {
  PROP_REWRITER_CHECK_INSTANCE(self);
  self->monitor_variable_initialized_to_true = false;
}

void Prop_Rewriter_ltl2invar_negate_property_to_true(Prop_Rewriter_ptr self) {
  PROP_REWRITER_CHECK_INSTANCE(self);
  self->ltl2invar_negate_property = true;
}

void Prop_Rewriter_ltl2invar_negate_property_to_false(Prop_Rewriter_ptr self) {
  PROP_REWRITER_CHECK_INSTANCE(self);
  self->ltl2invar_negate_property = false;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

Prop_ptr prop_rewriter_rewrite(Prop_Rewriter_ptr self)
{
  NuSMVEnv_ptr const env = EnvObject_env(ENV_OBJECT(self));
  Logger_ptr const logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
  MasterPrinter_ptr const wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  Prop_ptr retval = NULL;
  Prop_Type proptype = Prop_NoType;

  PROP_CHECK_INSTANCE(self->original);

  proptype = Prop_get_type(self->original);

  Logger_vnlog_debug(logger, wffprint, opts, "Input property\n%N\n",
                     Prop_get_expr(self->original));

  /* We only process ltl or invar */
  if (Prop_Ltl != proptype && Prop_Invar != proptype) {
    retval = Prop_copy(self->original);
    Prop_set_index(retval, -1);
  }
  /* Maybe there is nothing to do */
  else if (! Prop_needs_rewriting(self->symb_table, self->original) &&
           WFF_REWRITER_REWRITE_INPUT_NEXT == self->expprop) {
    /* If the rewriting has only to take into account input/next
       avoiding conversion from LTL to invar, then simply copy the
       property */
    retval = Prop_copy(self->original);
    Prop_set_index(retval, -1);
  }
  else {
    SymbLayer_ptr layer = NULL;

    FsmBuilder_ptr fsm_builder = NULL;
    FlatHierarchy_ptr outfh = NULL;

    ListIter_ptr iter;
    Pair_ptr rewritten_formula = PAIR(NULL);
    Prop_Type new_prop_type = Prop_NoType;
    /* we use a unique name by purpose so to trigger an assertion if a
       developer forgets to call Prop_Rewriter_destroy */
    if (NULL == self->layer) {
      layer = SymbTable_create_layer(self->symb_table,
                                     NULL,
                                     SYMB_LAYER_POS_BOTTOM);
      self->layer = layer;
      {
        const char * layer_name = SymbLayer_get_name(layer);

        nusmv_assert(NULL != layer_name);

        /* Set the layer in artifacts or in model depending whether the
           monitor variables have to be visible or not in traces. By
           default they are not visible. */
        if (self->monitor_visible_in_traces) {
          SymbTable_layer_add_to_class(self->symb_table, layer_name,
                                       MODEL_LAYERS_CLASS);
        }
        else {
          SymbTable_layer_add_to_class(self->symb_table, layer_name,
                                       ARTIFACTS_LAYERS_CLASS);
        }
      }
    }
    else layer = self->layer;

    {
      node_ptr formula = NULL;
      short int spec_type = 0;

      outfh = FlatHierarchy_create(self->symb_table);
      formula = Prop_get_expr_core(self->original);
      spec_type = PropType_to_node_type(Prop_get_type(self->original));

      rewritten_formula =
        Wff_Rewrite_rewrite_formula_generic(env, self->method,
                                            self->expprop,
                                            layer,
                                            outfh, formula, spec_type,
                                            self->monitor_variable_initialized_to_true,
                                            self->ltl2invar_negate_property);
    }

    nusmv_assert((INVARSPEC == PTR_TO_INT(Pair_get_second(rewritten_formula)) ||
                  LTLSPEC == PTR_TO_INT(Pair_get_second(rewritten_formula))));
    new_prop_type = (INVARSPEC == PTR_TO_INT(Pair_get_second(rewritten_formula))) ? \
      Prop_Invar : Prop_Ltl;
    retval = Prop_create_partial(env, NODE_PTR(Pair_get_first(rewritten_formula)),
                                 new_prop_type);

    Pair_destroy(rewritten_formula);

    /* Commit the needed encoders */
    {
      const char* layer_name = SymbLayer_get_name(layer);
      BddEnc_ptr bddenc = NULL;
      BoolEnc_ptr boolenc = NULL;

      bddenc = self->bddenc;
      BDD_ENC_CHECK_INSTANCE(bddenc);
      boolenc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(bddenc));
      BOOL_ENC_CHECK_INSTANCE(boolenc);

      BaseEnc_commit_layer(BASE_ENC(boolenc), layer_name);
      BaseEnc_commit_layer(BASE_ENC(bddenc), layer_name);

      if (FSM_TYPE_BE & self->fsm_type) {
        BeFsm_ptr befsm = Prop_get_be_fsm(self->original);
        BeEnc_ptr beenc = BeFsm_get_be_encoding(befsm);
        BE_ENC_CHECK_INSTANCE(beenc);

        BaseEnc_commit_layer(BASE_ENC(beenc), layer_name);
      }
    }

    /* Build the needed fsm */
    {
      SexpFsm_ptr orig_sexpfsm = NULL;

      SexpFsm_ptr sexpfsm = NULL;
      BoolSexpFsm_ptr boolfsm = NULL;
      BddFsm_ptr bddfsm = NULL;
      BeFsm_ptr befsm = NULL;

      BddEnc_ptr bddenc = NULL;
      FsmBuilder_ptr fsm_builder = NULL;

      bddenc = self->bddenc;
      orig_sexpfsm = Prop_get_scalar_sexp_fsm(self->original);

      /* ONE - Build the required fsms ****************************************/
      if (NULL == boolfsm &&
          (FSM_TYPE_BOOL_SEXP & self->fsm_type)) {
        if (NULL == sexpfsm) {
          sexpfsm = SexpFsm_create(outfh, FlatHierarchy_get_vars(outfh));
        }

        SexpFsm_apply_synchronous_product(sexpfsm, orig_sexpfsm);

        boolfsm = BoolSexpFsm_create_from_scalar_fsm(sexpfsm, bddenc, layer);
      }

      if (NULL == bddfsm &&
          (FSM_TYPE_BDD & self->fsm_type)) {
        if (NULL == sexpfsm) {
          sexpfsm = SexpFsm_create(outfh, FlatHierarchy_get_vars(outfh));
        }

        fsm_builder = FSM_BUILDER(NuSMVEnv_get_value(env, ENV_FSM_BUILDER));

        SexpFsm_apply_synchronous_product(sexpfsm, orig_sexpfsm);

        bddfsm = FsmBuilder_create_bdd_fsm(fsm_builder, bddenc, sexpfsm,
                                           get_partition_method(opts));
      }

      if (NULL == sexpfsm &&
          (FSM_TYPE_SEXP & self->fsm_type)) {
        sexpfsm = SexpFsm_create(outfh, FlatHierarchy_get_vars(outfh));

        SexpFsm_apply_synchronous_product(sexpfsm, orig_sexpfsm);
      }

      if (NULL == befsm &&
          (FSM_TYPE_BE & self->fsm_type)) {
        BeFsm_ptr orig_befsm = NULL;
        BeEnc_ptr beenc = NULL;

        orig_befsm = Prop_get_be_fsm(self->original);
        beenc = BE_ENC(BeFsm_get_be_encoding(orig_befsm));
        fsm_builder = FSM_BUILDER(NuSMVEnv_get_value(env, ENV_FSM_BUILDER));

        if (NULL != boolfsm) {
          befsm = BeFsm_create_from_sexp_fsm(beenc, boolfsm);
        }
        else {
          boolfsm =
            FsmBuilder_create_boolean_sexp_fsm(fsm_builder,
                                               outfh,
                                               FlatHierarchy_get_vars(outfh),
                                               bddenc,
                                               layer);
          befsm = BeFsm_create_from_sexp_fsm(beenc, boolfsm);
          BeFsm_apply_synchronous_product(befsm, orig_befsm);

          /* Partial bug fix : in the old code the boolsexpfsm set in the
             rewritten property was the original one, not the sync product.
             So, missing a BoolSexpFsm_apply_synchronous_product, if we have
             the scalar fsm we set the right fsm, otherwise we set the wrong
             one (it should not create problems) */
          if (NULL != orig_sexpfsm) {
            SexpFsm_ptr sexpfsm = NULL;

            if (NULL == sexpfsm) {
              sexpfsm = SexpFsm_create(outfh, FlatHierarchy_get_vars(outfh));
              SexpFsm_apply_synchronous_product(sexpfsm, orig_sexpfsm);
            }

            boolfsm = BoolSexpFsm_create_from_scalar_fsm(sexpfsm, bddenc, layer);
          }
        }
      } /* end of be fsm building */

      /* TWO - Set the built one in the rewritten prop and destroy the local
         copy *****************************************************************/
      {
        if (NULL != boolfsm) {
          Prop_set_bool_sexp_fsm(retval, boolfsm);
          BoolSexpFsm_destroy(boolfsm); boolfsm = NULL;
        }

        if (NULL != sexpfsm) {
          Prop_set_scalar_sexp_fsm(retval, sexpfsm);
          SexpFsm_destroy(sexpfsm); sexpfsm = NULL;
        }

        if (NULL != bddfsm) {
          Prop_set_bdd_fsm(retval, bddfsm);
          BddFsm_destroy(bddfsm); bddfsm = NULL;
        }

        if (NULL != befsm) {
          Prop_set_be_fsm(retval, befsm);
          BeFsm_destroy(befsm); befsm = NULL;
        }
      }
    }

    {
      Set_t cone = Prop_get_cone(self->original);

      if (NULL != cone) {
        cone = FlatHierarchy_get_vars(outfh);
        Prop_set_cone(retval, cone);
      }
    }

    PROP_CHECK_INSTANCE(retval);

    Logger_vnlog_debug(logger, wffprint, opts, "Output property\n%N\n",
                       Prop_get_expr(retval));
    if (NULL != Prop_get_scalar_sexp_fsm(retval)
        && opt_verbose_level_ge(opts, 5)) {
      array_t* layers = array_alloc(char*, 2);
      if (NULL != self->layer) {
        const char * layer_name = SymbLayer_get_name(self->layer);

        if (SymbTable_has_layer(self->symb_table, layer_name)) {
          (void)array_insert_last(char*, layers, (char*)layer_name);
        }
      }
      if (SymbTable_has_layer(self->symb_table, SymbLayer_get_name(layer))) {
        (void)array_insert_last(char*, layers, (char*)SymbLayer_get_name(layer));
      }

      Compile_WriteFlattenFsm(env, Logger_get_stream(logger), self->symb_table,
                              layers, "MODULE main",
                              SexpFsm_get_hierarchy(Prop_get_scalar_sexp_fsm(retval)),
                              false);

      array_free(layers); layers = NULL;
    }
  }

  /* Now the formula MUST be input vars and next operator free */
  nusmv_assert(! Prop_needs_rewriting(self->symb_table, retval));

  self->rewritten = retval;

  return retval;
}

void prop_rewriter_init(Prop_Rewriter_ptr self,
                        NuSMVEnv_ptr env,
                        Prop_ptr original,
                        WffRewriteMethod method,
                        WffRewriterExpectedProperty expprop,
                        FsmType fsm_type,
                        BddEnc_ptr bddenc,
                        SymbLayer_ptr layer)
{
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env,
                                                         ENV_OPTS_HANDLER));

  /* base class initialization */
  env_object_init(ENV_OBJECT(self), env);

  /* here all needed unset fsms are taken from the environment and
     copied into the original property */
  if ((FSM_TYPE_SEXP |
       FSM_TYPE_BOOL_SEXP |
       FSM_TYPE_BDD |
       FSM_TYPE_BE) & fsm_type) {
    SexpFsm_ptr fsm = Prop_get_scalar_sexp_fsm(original);
    if (NULL == fsm) {
      if (opt_cone_of_influence(opts)) {
        Prop_apply_coi_for_scalar(env, original);
        fsm = Prop_get_scalar_sexp_fsm(original);
      }
      else if (NuSMVEnv_has_value(env, ENV_SEXP_FSM)) {
        fsm = SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));
        Prop_set_scalar_sexp_fsm(original, fsm);
      }
    }
  }

  if ((FSM_TYPE_BOOL_SEXP |
       FSM_TYPE_BE) & fsm_type) {
    BoolSexpFsm_ptr fsm = Prop_get_bool_sexp_fsm(original);
    if (NULL == fsm) {
      if (opt_cone_of_influence(opts)) {
        Prop_apply_coi_for_bmc(env, original);
        fsm = Prop_get_bool_sexp_fsm(original);
      }
      else if (NuSMVEnv_has_value(env, ENV_BOOL_FSM)) {
        fsm = BOOL_SEXP_FSM(NuSMVEnv_get_value(env, ENV_BOOL_FSM));
        Prop_set_bool_sexp_fsm(original, fsm);
      }
    }
  }

  if (FSM_TYPE_BDD & fsm_type) {
    BddFsm_ptr fsm = Prop_get_bdd_fsm(original);
    if (NULL == fsm) {
      if (opt_cone_of_influence(opts)) {
        Prop_apply_coi_for_bdd(env, original);
        fsm =  Prop_get_bdd_fsm(original);
      }
      else {
        fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));
        Prop_set_bdd_fsm(original, fsm);
      }
    }
    nusmv_assert(NULL != Prop_get_scalar_sexp_fsm(original));
  }

  if  (FSM_TYPE_BE & fsm_type) {
    BeFsm_ptr fsm = Prop_get_be_fsm(original);
    if (NULL == fsm) {
      fsm = BE_FSM(NuSMVEnv_get_value(env, ENV_BE_FSM));
      Prop_set_be_fsm(original, fsm);
    }

    nusmv_assert(NULL != Prop_get_bool_sexp_fsm(original) ||
                 NULL != Prop_get_scalar_sexp_fsm(original));
  }

  /* members initialization */
  self->original = original;
  self->method = method;
  self->expprop = expprop;
  self->fsm_type = fsm_type;
  self->bddenc = bddenc;

  /* By default the monitor variable are not visible in traces */
  self->monitor_visible_in_traces = false;
  /* By default the monitor variable are initialized to TRUE if method
     is WFF_REWRITE_METHOD_DEADLOCK_FREE*/
  self->monitor_variable_initialized_to_true = true;

  /* By default, when the LTL is converted into a property, it is not
     negated. */
  self->ltl2invar_negate_property = false;

  if (NULL != Prop_get_bdd_fsm(original)) {
    self->symb_table =
      BaseEnc_get_symb_table(BASE_ENC(
                      BddFsm_get_bdd_encoding(Prop_get_bdd_fsm(original))));
    nusmv_assert(bddenc ==
                 BddFsm_get_bdd_encoding(Prop_get_bdd_fsm(original)));
  }
  else if (NULL != Prop_get_be_fsm(original)) {
    self->symb_table =
      BaseEnc_get_symb_table(BASE_ENC(
                      BeFsm_get_be_encoding(Prop_get_be_fsm(original))));
  }
  else if (NULL != Prop_get_scalar_sexp_fsm(original)) {
    self->symb_table =
      SexpFsm_get_symb_table(Prop_get_scalar_sexp_fsm(original));
  }
  else {
    error_unreachable_code_msg("At least one FSM built is assumed\n");
  }

  self->is_status_consistent = false;
  self->layer = layer;

  /* owned */
  self->rewritten = NULL;

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = prop_rewriter_finalize;
  OVERRIDE(Prop_Rewriter, rewrite) = prop_rewriter_rewrite;
}


void prop_rewriter_deinit(Prop_Rewriter_ptr self)
{
  const char* layer_name =
    (NULL != self->layer) ? SymbLayer_get_name(self->layer) : NULL;

  if (! self->is_status_consistent) {
    error_unreachable_code_msg("Error: You have to call "
                               "Prop_Rewriter_update_original_property "
                               "before destroying the Property Rewriter\n");
  }

  if (Prop_needs_rewriting(self->symb_table, self->original) ||
      (WFF_REWRITER_LTL_2_INVAR == self->expprop )) {
    BddEnc_ptr bddenc = NULL;
    BoolEnc_ptr boolenc = NULL;
    BeEnc_ptr beenc = NULL;

    nusmv_assert(NULL != layer_name);

    bddenc = self->bddenc;
    boolenc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(bddenc));

    if (FSM_TYPE_BE & self->fsm_type) {
      BeFsm_ptr fsm = NULL;

      fsm = Prop_get_be_fsm(self->original);
      beenc = BeFsm_get_be_encoding(fsm);

      if (BaseEnc_layer_occurs(BASE_ENC(beenc), layer_name)) {
        BaseEnc_remove_layer(BASE_ENC(beenc), layer_name);
      }
    }

    if (BaseEnc_layer_occurs(BASE_ENC(bddenc), layer_name)) {
      BaseEnc_remove_layer(BASE_ENC(bddenc), layer_name);
    }

    if (BaseEnc_layer_occurs(BASE_ENC(boolenc), layer_name)) {
      BaseEnc_remove_layer(BASE_ENC(boolenc), layer_name);
    }
  }

  /* references */
  self->original = NULL;
  self->method = -1;
  self->is_status_consistent = false;
  self->layer = NULL;
  self->fsm_type = 0;
  self->bddenc = NULL;

  /* owned */
  Prop_destroy(self->rewritten); self->rewritten = NULL;

  /* base class deinitialization */
  env_object_deinit(ENV_OBJECT(self));
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The Prop_Rewriter class virtual finalizer

  Called by the class destructor
*/
static void prop_rewriter_finalize(Object_ptr object, void* dummy)
{
  Prop_Rewriter_ptr self = PROP_REWRITER(object);

  UNUSED_PARAM(dummy);

  prop_rewriter_deinit(self);
  FREE(self);
}

/*!
\brief Frees an expression created in the CompileFlatten style

The formula is traversed and all the nodes are freed, but the ones that are
believed to have been created with find_node, according to the Compiler style:
constants and symbols.
*/
static void pr_free_node(NuSMVEnv_ptr env,
                         NodeMgr_ptr nodemgr,
                         node_ptr expr)
{
  if (NULL != expr) {
    switch(node_get_type(expr)) {
      /* --- constants ---
         the expression is find_atom */
    case FAILURE:  case TRUEEXP:  case FALSEEXP:
    case NUMBER:  case NUMBER_UNSIGNED_WORD:  case NUMBER_SIGNED_WORD:
    case UWCONST: case SWCONST:
    case NUMBER_FRAC:  case NUMBER_REAL:  case NUMBER_EXP:
      break;

    case WSIZEOF: case CAST_TOINT:
      break;

    case CONTEXT:
      pr_free_node(env, nodemgr, cdr(expr));
      free_node(nodemgr, cdr(expr));
      break;

      /* --- identifier ---
         since the expression is already flattened there is not need
         to resolve the identifier, find_atom it or create a copy. */
    case ATOM:  case DOT:  case ARRAY:
      break;

    case NFUNCTION:
      /* No need to look at the name of the nfunction */
      pr_free_node(env, nodemgr, cdr(expr));
      free_node(nodemgr, cdr(expr));
      break;

      /* --- next --- */
    case NEXT:
      pr_free_node(env, nodemgr, car(expr));
      free_node(nodemgr, car(expr));
      break;

      /* --- unary non-temporal operator --- */
    case NOT:
    case UMINUS:
    case FLOOR:
      pr_free_node(env, nodemgr, car(expr));
      free_node(nodemgr, car(expr));
      break;

      /* --- binary non-temporal operators ---
         if one operand has input and other has a temporal op =>
         rewrite operand with input var and return temporal kind.
         If kinds are different "input" and "temporal" kinds wins
         "state".*/
    case TWODOTS: /* This is dealt as a binary operator */
    case AND: case OR: case IMPLIES: case IFF: case XOR: case XNOR:
      pr_free_node(env, nodemgr, car(expr));
      free_node(nodemgr, car(expr));

      pr_free_node(env, nodemgr, cdr(expr));
      free_node(nodemgr, cdr(expr));
      break;

    case WRESIZE:
      pr_free_node(env, nodemgr, car(expr));
      free_node(nodemgr, car(expr));
      break;

      /* --- binary non-temporal operators ---
         it is exactly as previous case but the operands cannot have temporal
         operators. It is written as a special case only for debugging purposes.*/
    case CONS:
    case CASE: case COLON:
    case EQUAL: case NOTEQUAL:
    case LT: case GT: case LE: case GE:
    case PLUS: case MINUS: case TIMES: case MOD: case DIVIDE:
    case UNION: case SETIN:
    case LSHIFT: case RSHIFT:
    case BIT: case CONCATENATION: case BIT_SELECTION:  case EXTEND:
    case CAST_BOOL:  case CAST_WORD1:  case CAST_SIGNED: case CAST_UNSIGNED:
    case IFTHENELSE:
      pr_free_node(env, nodemgr, car(expr));
      free_node(nodemgr, car(expr));

      pr_free_node(env, nodemgr, cdr(expr));
      free_node(nodemgr, cdr(expr));
      break;

      /*  -- unary temporal operators ---
          if operand has inputs then rewrite it. */
    case OP_NEXT: case OP_PREC: case OP_NOTPRECNOT: case OP_FUTURE:
    case OP_ONCE: case OP_GLOBAL: case OP_HISTORICAL:
      pr_free_node(env, nodemgr, car(expr));
      free_node(nodemgr, car(expr));
      break;

      /* --- binary temporal operators ---
         If any operand has inputs then rewrite it.*/
    case UNTIL: case SINCE: case RELEASES: case TRIGGERED:
      pr_free_node(env, nodemgr, car(expr));
      free_node(nodemgr, car(expr));

      pr_free_node(env, nodemgr, cdr(expr));
      free_node(nodemgr, cdr(expr));
      break;

    default:
      {
        StreamMgr_ptr const streams =
          STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
        ErrorMgr_ptr const errmgr =
          ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

        StreamMgr_print_error(streams, "Error: %s:%d:%s: unexpected operator of type %d\n",
                              __FILE__, __LINE__, __func__, node_get_type(expr));
        ErrorMgr_nusmv_exit(errmgr, 1);
      }
    }
  }
}


/**AutomaticEnd***************************************************************/
