/* ---------------------------------------------------------------------------

   This file is part of the ``fsm.bdd'' package of NuSMV version 2.
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
  \author Roberto Cavada, Marco Benedetti
  \brief Defines the public interface for the class BddFsm

  A BddFsm is a Finite State Machine (FSM) whose building blocks
   (the set of initial state, the transition relation, the set of
   constraints on inputs and so on) are represented by means of
   BDD data structures, and whose capabilities are based on
   operations upon and between BDDs as well.

   Note: a state is an assignment to state and frozen variables.
   an input is an assigment to input variables.

*/


#include "nusmv/core/fsm/bdd/BddFsm.h"
#include "nusmv/core/fsm/bdd/BddFsm_private.h"
#include "nusmv/core/fsm/bdd/bddInt.h"
#include "nusmv/core/fsm/bdd/BddFsm.h"
#include "nusmv/core/fsm/bdd/FairnessList.h"

#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/utils/error.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/parser/parser.h"
#include "nusmv/core/parser/symbols.h"

/* libraries */
#include <math.h>


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/* ---------------------------------------------------------------------- */
/*                     Static functions prototypes                        */
/* ---------------------------------------------------------------------- */

static void bdd_fsm_init(BddFsm_ptr self, BddEnc_ptr encoding,
                         BddStates init, BddInvarStates invar_states,
                         BddInvarInputs invar_inputs,
                         BddTrans_ptr trans,
                         JusticeList_ptr justice,
                         CompassionList_ptr compassion);

static void bdd_fsm_copy(const BddFsm_ptr self, BddFsm_ptr copy);

static void bdd_fsm_deinit(BddFsm_ptr self);

static void bdd_fsm_compute_reachable_states(BddFsm_ptr self);

static BddStatesInputs
bdd_fsm_get_legal_state_input(BddFsm_ptr self);

/* The new code for fairness */
static BddStatesInputs
bdd_fsm_get_fair_or_revfair_states_inputs(BddFsm_ptr self,
                                          BddFsm_dir dir);

static BddStatesInputs
bdd_fsm_get_fair_or_revfair_states_inputs_in_subspace(
    const BddFsm_ptr self, BddStatesInputs subspace, BddFsm_dir dir);

static BddStatesInputs
bdd_fsm_compute_EL_SI_subset(const BddFsm_ptr self,
                             BddStatesInputs subspace,
                             BddFsm_dir dir);

static BddStatesInputs
bdd_fsm_compute_EL_SI_subset_aux(const BddFsm_ptr self,
                                 BddStatesInputs states,
                                 BddStatesInputs subspace,
                                 BddFsm_dir dir);


static void bdd_fsm_check_init_state_invar_emptiness(const BddFsm_ptr self);
static void bdd_fsm_check_fairness_emptiness(const BddFsm_ptr self);


/* ---------------------------------------------------------------------- */
/*                          public methods                                */
/* ---------------------------------------------------------------------- */

BddFsm_ptr BddFsm_create(BddEnc_ptr encoding,
                         BddStates init,
                         BddInvarStates invar_states,
                         BddInvarInputs invar_inputs,
                         BddTrans_ptr trans,
                         JusticeList_ptr justice,
                         CompassionList_ptr compassion)
{
  BddFsm_ptr self = ALLOC( BddFsm, 1 );
  BDD_FSM_CHECK_INSTANCE(self);

  bdd_fsm_init(self, encoding, init, invar_states, invar_inputs,
               trans, justice, compassion);

  return self;
}


void BddFsm_destroy(BddFsm_ptr self)
{
  BDD_FSM_CHECK_INSTANCE(self);

  bdd_fsm_deinit(self);
  FREE(self);
}


BddFsm_ptr BddFsm_copy(const BddFsm_ptr self)
{
  BddFsm_ptr copy;

  BDD_FSM_CHECK_INSTANCE(self);

  copy = ALLOC( BddFsm, 1 );
  BDD_FSM_CHECK_INSTANCE(copy);

  bdd_fsm_copy(self, copy);

  return copy;
}


void BddFsm_copy_cache(BddFsm_ptr self, const BddFsm_ptr other,
                       boolean keep_family)
{
  BDD_FSM_CHECK_INSTANCE(self);

  BddFsmCache_destroy(self->cache);
  if (keep_family) self->cache = BddFsmCache_soft_copy(other->cache);
  else self->cache = BddFsmCache_hard_copy(other->cache);
}


JusticeList_ptr BddFsm_get_justice(const BddFsm_ptr self)
{
  BDD_FSM_CHECK_INSTANCE(self);
  return self->justice;
}


CompassionList_ptr BddFsm_get_compassion(const BddFsm_ptr self)
{
  BDD_FSM_CHECK_INSTANCE(self);
  return self->compassion;
}


BddStates BddFsm_get_init(const BddFsm_ptr self)
{
  bdd_ptr res;

  BDD_FSM_CHECK_INSTANCE(self);

  res = bdd_dup((bdd_ptr) self->init);
  return BDD_STATES(res);
}


BddInvarStates BddFsm_get_state_constraints(const BddFsm_ptr self)
{
  bdd_ptr res;

  BDD_FSM_CHECK_INSTANCE(self);

  res = bdd_dup( (bdd_ptr) self->invar_states );
  return BDD_INVAR_STATES(res);
}


BddInvarInputs BddFsm_get_input_constraints(const BddFsm_ptr self)
{
  bdd_ptr res;

  BDD_FSM_CHECK_INSTANCE(self);

  res = bdd_dup( (bdd_ptr) self->invar_inputs );
  return BDD_INVAR_INPUTS(res);
}


BddTrans_ptr BddFsm_get_trans(const BddFsm_ptr self)
{
  BDD_FSM_CHECK_INSTANCE(self);

  return self->trans;
}


BddEnc_ptr BddFsm_get_bdd_encoding(const BddFsm_ptr self)
{
  BDD_FSM_CHECK_INSTANCE(self);
  return self->enc;
}


boolean BddFsm_get_cached_reachable_states(const BddFsm_ptr self,
                                           BddStates** layers,
                                           int* size)
{
  BDD_FSM_CHECK_INSTANCE(self);

  *layers = CACHE_GET(reachable.layers);
  *size = CACHE_GET(reachable.diameter);

  return CACHE_GET(reachable.computed);
}


void BddFsm_set_reachable_states(const BddFsm_ptr self,
                                 BddStates reachable)
{
  BDD_FSM_CHECK_INSTANCE(self);
  BddFsmCache_set_reachable_states(self->cache, reachable);
}


boolean BddFsm_has_cached_reachable_states(const BddFsm_ptr self)
{
  BDD_FSM_CHECK_INSTANCE(self);
  return !CACHE_IS_EQUAL(reachable.reachable_states, (bdd_ptr)NULL);
}


void BddFsm_update_cached_reachable_states(const BddFsm_ptr self,
                                           node_ptr layers_list,
                                           int size,
                                           boolean completed)
{
  BDD_FSM_CHECK_INSTANCE(self);

  BddFsmCache_set_reachables(self->cache, layers_list, size, completed);
}


boolean BddFsm_reachable_states_computed(BddFsm_ptr self)
{
  BDD_FSM_CHECK_INSTANCE(self);

  if ( CACHE_IS_EQUAL(reachable.computed, false) ) return false;
  else return true;
}


BddStates BddFsm_get_reachable_states(BddFsm_ptr self)
{
  BddStates res;

  BDD_FSM_CHECK_INSTANCE(self);

  /* If we already have the complete set of reachable states, return
     it. */
  if (BddFsm_has_cached_reachable_states(self)) {
    res = CACHE_GET(reachable.reachable_states);
  }
  /* Otherwise compute them, if necessary */
  else {

    if ( CACHE_IS_EQUAL(reachable.computed, false) ) {
      bdd_fsm_compute_reachable_states(self);
    }

    if (CACHE_GET(reachable.diameter) > 0) {
      res = CACHE_GET(reachable.layers[CACHE_GET(reachable.diameter) - 1]);
    }
    else {
      res = bdd_false(self->dd);
    }

  }

  bdd_ref((bdd_ptr) res);

  return res;
}


void BddFsm_copy_reachable_states(BddFsm_ptr self, BddFsm_ptr other,
                                  boolean keep_family,
                                  boolean force_calculation)
{
  BDD_FSM_CHECK_INSTANCE(self);

  /* computes reachables in other if needed */
  if (!other->cache->reachable.computed && force_calculation) {
    bdd_fsm_compute_reachable_states(other);
  }

  if (!keep_family && *(self->cache->family_counter) > 1) {
    /* the cache is shared and must be copied to detach it */
    BddFsmCache_ptr new_cache = BddFsmCache_hard_copy(self->cache);
    BddFsmCache_destroy(self->cache);
    self->cache = new_cache;
  }

  BddFsmCache_copy_reachables(self->cache, other->cache);
}


BddStates BddFsm_get_reachable_states_at_distance(BddFsm_ptr self,
                                                  int distance)
{
  BddStates res;

  BDD_FSM_CHECK_INSTANCE(self);

  res = BDD_STATES(NULL);

  if (distance >= 0) {
    int diameter;

    if (CACHE_IS_EQUAL(reachable.computed, false)) {
      bdd_fsm_compute_reachable_states(self);
    }

    diameter = CACHE_GET(reachable.diameter);

    /* checks distance */
    nusmv_assert(distance < diameter);

    res = CACHE_GET_BDD(reachable.layers[distance]);
    /* Compute the distance-th frontier if we are not looking for the
       initial state */
    if (distance >= 1) {
      bdd_ptr x, neg;

      x = CACHE_GET_BDD(reachable.layers[distance - 1]);
      neg = bdd_not(self->dd, x);

      bdd_and_accumulate(self->dd, &res, neg);

      bdd_free(self->dd, x);
      bdd_free(self->dd, neg);
    }
  }

  /* checks if assigned: */
  if (res == BDD_STATES(NULL)) {
    res = BDD_STATES(bdd_false(self->dd));
  }

  return res;
}


bdd_ptr BddFsm_get_monolithic_trans_bdd(BddFsm_ptr self)
{
  bdd_ptr res;

  BDD_FSM_CHECK_INSTANCE(self);

  if ( CACHE_IS_EQUAL(monolithic_trans, (bdd_ptr) NULL) ) {
    res = BddTrans_get_monolithic_bdd(self->trans);

    CACHE_SET_BDD(monolithic_trans, res);
    bdd_free(self->dd, res);
  }

  res = CACHE_GET_BDD(monolithic_trans);
  return res;
}


int BddFsm_get_distance_of_states(BddFsm_ptr self, BddStates states)
{
  bdd_ptr constr_states;
  int i;
  int diameter;
  int result = -1;

  BDD_FSM_CHECK_INSTANCE(self);

  if (CACHE_IS_EQUAL(reachable.computed, false)) {
    bdd_fsm_compute_reachable_states(self);
  }

  /* applies state constraints: */
  constr_states = bdd_and(self->dd,
                          (bdd_ptr) states,
                          (bdd_ptr) self->invar_states);

  diameter = CACHE_GET(reachable.diameter);

  for (i=0; i<diameter; ++i) {
    bdd_ptr Ri = (bdd_ptr) BddFsm_get_reachable_states_at_distance(self, i);
    int entailed = bdd_entailed(self->dd, constr_states, Ri);

    if (entailed == 1) {
      bdd_free(self->dd, Ri);
      result = i;
      break;
    }

    bdd_free(self->dd, Ri);
  }

  bdd_free(self->dd, constr_states);
  return result;
}


int BddFsm_get_minimum_distance_of_states(BddFsm_ptr self, BddStates states)
{
  bdd_ptr constr_states;
  int i;
  int diameter;
  int result = -1;

  BDD_FSM_CHECK_INSTANCE(self);

  if (CACHE_IS_EQUAL(reachable.computed, false)) {
    bdd_fsm_compute_reachable_states(self);
  }

  /* applies state constraints: */
  constr_states = bdd_and(self->dd, (bdd_ptr) states,
                          (bdd_ptr) self->invar_states);

  diameter = CACHE_GET(reachable.diameter);

  for (i=0; (-1 == result) && i<diameter; ++i) {
    bdd_ptr Ri  = (bdd_ptr) BddFsm_get_reachable_states_at_distance(self, i);

    bdd_and_accumulate(self->dd, &Ri, constr_states);
    if (bdd_isnot_false(self->dd, Ri)) { result = i; }

    bdd_free(self->dd, Ri);
  }

  bdd_free(self->dd, constr_states);
  return result;
}


int BddFsm_get_diameter(BddFsm_ptr self)
{
  BDD_FSM_CHECK_INSTANCE(self);

  if (CACHE_IS_EQUAL(reachable.computed, false)) {
    bdd_fsm_compute_reachable_states(self);
  }

  return CACHE_GET(reachable.diameter);
}


BddStates BddFsm_get_not_successor_states(BddFsm_ptr self)
{
  BddStates res;

  BDD_FSM_CHECK_INSTANCE(self);

  if ( CACHE_IS_EQUAL(not_successor_states, BDD_STATES(NULL)) ) {
    bdd_ptr all_states = bdd_true(self->dd);
    bdd_ptr succ       = BddFsm_get_backward_image(self, all_states);
    bdd_ptr not_succ   = bdd_not(self->dd, succ);
    bdd_ptr no_succ_constr = bdd_and(self->dd, not_succ, self->invar_states);

    bdd_free(self->dd, not_succ);
    bdd_free(self->dd, succ);
    bdd_free(self->dd, all_states);

    CACHE_SET_BDD(not_successor_states, no_succ_constr);
    bdd_free(self->dd, no_succ_constr);
  }

  res = CACHE_GET_BDD(not_successor_states);
  return res;
}


BddStates BddFsm_get_deadlock_states(BddFsm_ptr self)
{
  BddStates res;

  BDD_FSM_CHECK_INSTANCE(self);

  if ( CACHE_IS_EQUAL(deadlock_states, BDD_STATES(NULL)) ) {
    BddStates no_succ = BddFsm_get_not_successor_states(self);
    BddStates reachable = BddFsm_get_reachable_states(self);

    bdd_ptr deadlock = bdd_and(self->dd, reachable, no_succ);
    bdd_free(self->dd, reachable);
    bdd_free(self->dd, no_succ);

    CACHE_SET_BDD(deadlock_states, BDD_STATES(deadlock));
    bdd_free(self->dd, deadlock);
  }

  res = CACHE_GET_BDD(deadlock_states);
  return res;
}


boolean BddFsm_is_total(BddFsm_ptr self)
{
  BddStates no_succ;
  boolean res;

  BDD_FSM_CHECK_INSTANCE(self);

  no_succ = BddFsm_get_not_successor_states(self);

  res = bdd_is_false(self->dd, (bdd_ptr) no_succ);
  bdd_free(self->dd, no_succ);

  return res;
}


boolean BddFsm_is_deadlock_free(BddFsm_ptr self)
{
  BddStates deadlock;
  boolean res;

  BDD_FSM_CHECK_INSTANCE(self);

  deadlock = BddFsm_get_deadlock_states(self);

  res = bdd_is_false(self->dd, (bdd_ptr) deadlock);
  bdd_free(self->dd, deadlock);

  return res;
}


BddStates BddFsm_get_forward_image(const BddFsm_ptr self, BddStates states)
{
  BddStatesInputs one = bdd_true(self->dd);
  BddStates res;

  BDD_FSM_CHECK_INSTANCE(self);

  res = BddFsm_get_constrained_forward_image(self, states, one);
  bdd_free(self->dd, one);

  return res;
}


BddStates
BddFsm_get_constrained_forward_image(const BddFsm_ptr self,
                                     BddStates states,
                                     BddStatesInputsNexts constraints)
{
  BddStates res;
  bdd_ptr constr_trans;
  bdd_ptr tmp;

  BDD_FSM_CHECK_INSTANCE(self);

  /* ------------------------------------------------------------ */
  /* Apply invariant contraints: */
  constr_trans = bdd_and(self->dd, states, self->invar_states);
  bdd_and_accumulate(self->dd, &constr_trans, self->invar_inputs);
  bdd_and_accumulate(self->dd, &constr_trans, constraints);
  /* ------------------------------------------------------------ */

  tmp = BddTrans_get_forward_image_state(self->trans, constr_trans);
  bdd_free(self->dd, constr_trans);

  res = BDD_STATES( BddEnc_next_state_var_to_state_var(self->enc, tmp) );
  bdd_free(self->dd, tmp);

  bdd_and_accumulate(self->dd, (bdd_ptr*) &res, self->invar_states);
  return res;
}


BddStates
BddFsm_get_sins_constrained_forward_image(const BddFsm_ptr self,
                                          BddStates states,
                                          BddStatesInputsNexts constraints)
{
  BddStates res;
  bdd_ptr constr_trans;
  bdd_ptr tmp;

  BDD_FSM_CHECK_INSTANCE(self);

  /* ------------------------------------------------------------ */
  /* Apply invariant contraints: */
  constr_trans = bdd_and(self->dd, states, self->invar_states);
  bdd_and_accumulate(self->dd, &constr_trans, self->invar_inputs);
  bdd_and_accumulate(self->dd, &constr_trans, constraints);
  /* ------------------------------------------------------------ */

  tmp = BddTrans_get_forward_image_state(self->trans, constr_trans);
  bdd_free(self->dd, constr_trans);

  res = BDD_STATES( BddEnc_next_state_var_to_state_var(self->enc, tmp) );
  bdd_free(self->dd, tmp);

  bdd_and_accumulate(self->dd, (bdd_ptr*) &res, self->invar_states);
  return res;
}


BddStatesInputs
BddFsm_get_forward_image_states_inputs(const BddFsm_ptr self,
                                       BddStatesInputs si)
{
  BddStatesInputs one = bdd_true(self->dd);
  BddStatesInputs res;

  BDD_FSM_CHECK_INSTANCE(self);

  res = BddFsm_get_constrained_forward_image_states_inputs(self, si, one);
  bdd_free(self->dd, one);

  return res;
}


BddStatesInputs
BddFsm_get_constrained_forward_image_states_inputs(
                                    const BddFsm_ptr self,
                                    BddStatesInputs si,
                                    BddStatesInputsNexts constraints)
{
  BddStatesInputs res;
  bdd_ptr constr_trans;
  bdd_ptr tmp;

  BDD_FSM_CHECK_INSTANCE(self);

  /* ------------------------------------------------------------ */
  /* Apply invariant contraints: */
  constr_trans = bdd_and(self->dd, si, self->invar_states);
  bdd_and_accumulate(self->dd, &constr_trans, self->invar_inputs);
  bdd_and_accumulate(self->dd, &constr_trans, constraints);
  /* ------------------------------------------------------------ */

  tmp = BddTrans_get_forward_image_state(self->trans, constr_trans);
  bdd_free(self->dd, constr_trans);

  res = BDD_STATES_INPUTS( BddEnc_next_state_var_to_state_var(self->enc, tmp) );
  bdd_free(self->dd, tmp);

  bdd_and_accumulate(self->dd, (bdd_ptr*) &res, self->invar_states);
  bdd_and_accumulate(self->dd, (bdd_ptr*) &res, self->invar_inputs);
  return res;
}


BddStates BddFsm_get_backward_image(const BddFsm_ptr self, BddStates states)
{
  BddStates res;
  BddStatesInputs one;

  BDD_FSM_CHECK_INSTANCE(self);

  one = bdd_true(self->dd);

  res = BddFsm_get_constrained_backward_image(self, states, one);
  bdd_free(self->dd, one);

  return res;
}


BddStates
BddFsm_get_constrained_backward_image(const BddFsm_ptr self,
                                      BddStates states,
                                      BddStatesInputsNexts constraints)
{
  bdd_ptr constr_trans;
  bdd_ptr tmp, result;

  BDD_FSM_CHECK_INSTANCE(self);

  tmp = bdd_and(self->dd, states, self->invar_states);
  constr_trans = BddEnc_state_var_to_next_state_var(self->enc, tmp);
  bdd_free(self->dd, tmp);

  bdd_and_accumulate(self->dd, &constr_trans, self->invar_inputs);
  bdd_and_accumulate(self->dd, &constr_trans, constraints);

  result = BddTrans_get_backward_image_state(self->trans, constr_trans);

  bdd_and_accumulate(self->dd, &result, self->invar_states);

  bdd_free(self->dd, constr_trans);

  return BDD_STATES(result);
}


BddStatesInputs BddFsm_get_k_backward_image(const BddFsm_ptr self,
                                            BddStates states,
                                            int k)
{
  bdd_ptr tmp, tmp1, result;

  BDD_FSM_CHECK_INSTANCE(self);

  tmp = bdd_and(self->dd, states, self->invar_states);

  /* We need to apply the mask, otherwise the count is not correct! */
  tmp1 = BddEnc_apply_state_frozen_vars_mask_bdd(self->enc, tmp);
  bdd_free(self->dd, tmp);

  tmp = BddEnc_state_var_to_next_state_var(self->enc, tmp1);
  bdd_free(self->dd, tmp1);

  result = BddTrans_get_k_backward_image_state_input(self->trans, tmp, k);
  bdd_free(self->dd, tmp);

  bdd_and_accumulate(self->dd, &result, self->invar_inputs);
  bdd_and_accumulate(self->dd, &result, self->invar_states);

  return BDD_STATES(result);
}


BddStatesInputs BddFsm_get_weak_backward_image(const BddFsm_ptr self,
                                               BddStates states)
{
  bdd_ptr constr_trans;
  bdd_ptr tmp, result;

  BDD_FSM_CHECK_INSTANCE(self);

  tmp = bdd_and(self->dd, states, self->invar_states);
  constr_trans = BddEnc_state_var_to_next_state_var(self->enc, tmp);
  bdd_free(self->dd, tmp);

  bdd_and_accumulate(self->dd, &constr_trans, self->invar_inputs);

  result = BddTrans_get_backward_image_state_input(self->trans, constr_trans);
  bdd_free(self->dd, constr_trans);

  bdd_and_accumulate(self->dd, &result, self->invar_states);

  return BDD_STATES(result);
}


BddStatesInputs BddFsm_get_strong_backward_image(const BddFsm_ptr self,
                                                 BddStates states)
{
  bdd_ptr not_states;
  bdd_ptr tmp, result;

  BDD_FSM_CHECK_INSTANCE(self);

  not_states = bdd_not(self->dd, states);
  /* there is no need to add state or input invariants because
     there are added in BddFsm_get_weak_backward_image
  */
  tmp = BddFsm_get_weak_backward_image(self, not_states);
  bdd_free(self->dd, not_states);

  /* Here tmp is the set of state/input transitions that can be
     actually made and that lead outside the set defined by the
     input parameter 'states'
  */

  result = bdd_not(self->dd, tmp);
  bdd_free(self->dd, tmp);

  /* result is the set of state/input transitions that either
     originate from 'non-real states' or - in case there is at least
     one successor - are such that all the successors lay inside the
     set defined by the input parameter 'states' or or one of state,
     input or all successors belong to sets not satisfying invariants.
  */

  /* Obtain all the legal transitions, i.e. such state/input
     which have have at least one successor  and all state/input/successor
     satisty the invariants.
  */
  tmp = (bdd_ptr) bdd_fsm_get_legal_state_input(self);

  bdd_and_accumulate(self->dd, &result, tmp);
  bdd_free(self->dd, tmp);

  /* At this point, result is the set of state/input transitions that
     have at least one successor and are such that all
     the successors lay inside the set defined by the
     input parameter 'states' */

  return BDD_STATES_INPUTS(result);
}


BddStatesInputs BddFsm_get_fair_states_inputs(BddFsm_ptr self)
{
  BDD_FSM_CHECK_INSTANCE(self);
  return bdd_fsm_get_fair_or_revfair_states_inputs(self, BDD_FSM_DIR_BWD);
}


BddStatesInputs BddFsm_get_revfair_states_inputs(BddFsm_ptr self)
{
  BDD_FSM_CHECK_INSTANCE(self);
  return bdd_fsm_get_fair_or_revfair_states_inputs(self, BDD_FSM_DIR_FWD);
}


BddStates BddFsm_get_fair_states(BddFsm_ptr self)
{
  BddStates res;

  BDD_FSM_CHECK_INSTANCE(self);

  if ( CACHE_IS_EQUAL(fair_states, BDD_STATES(NULL)) ) {
    BddStatesInputs si = BddFsm_get_fair_states_inputs(self);
    BddStates fs = BddFsm_states_inputs_to_states(self, si);

    CACHE_SET_BDD(fair_states, fs);

    bdd_free(self->dd, fs);
  }

  res = CACHE_GET_BDD(fair_states);
  return res;
}


BddStates BddFsm_get_revfair_states(BddFsm_ptr self)
{
  BddStates res;

  BDD_FSM_CHECK_INSTANCE(self);

  if ( CACHE_IS_EQUAL(revfair_states, BDD_STATES(NULL)) ) {
    BddStatesInputs si = BddFsm_get_revfair_states_inputs(self);
    BddStates fs = BddFsm_states_inputs_to_states(self, si);

    CACHE_SET_BDD(revfair_states, fs);

    bdd_free(self->dd, fs);
  }

  res = CACHE_GET_BDD(revfair_states);
  return res;
}


BddInputs BddFsm_states_to_states_get_inputs(const BddFsm_ptr self,
                                             BddStates cur_states,
                                             BddStates next_states)
{
  BddStates bwd_image_si;
  BddInputs inputs;

  BDD_FSM_CHECK_INSTANCE(self);

  bwd_image_si = BddFsm_get_weak_backward_image(self, next_states);
  bdd_and_accumulate(self->dd, &bwd_image_si, cur_states);

  inputs = BddFsm_states_inputs_to_inputs(self, bwd_image_si);

  bdd_free(self->dd, bwd_image_si);
  return inputs;
}


boolean BddFsm_is_fair_states(const BddFsm_ptr self, BddStates states)
{
  BddStates fair_states;
  boolean res;

  BDD_FSM_CHECK_INSTANCE(self);

  fair_states = BddFsm_get_fair_states(self);

  res = (bdd_entailed(self->dd, states, fair_states) == 1);

  bdd_free(self->dd, fair_states);
  return res;
}


BddStatesInputs BddFsm_get_states_inputs_constraints(const BddFsm_ptr self,
                                                     BddFsm_dir dir)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->enc));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  BddStatesInputs result;
  BddStates all_states;

  BDD_FSM_CHECK_INSTANCE(self);

  if (opt_use_reachable_states(opts)) {
    all_states = BddFsm_get_reachable_states(self);
  }
  else all_states = bdd_true(self->dd);

  /* it would be a better idea to cache it */
  if (dir == BDD_FSM_DIR_BWD) {
    result = BddTrans_get_backward_image_state_input(self->trans, all_states);
  }
  else {
    /* TODO[VS] make symmetric? */
    result = BddFsm_get_forward_image_states_inputs(self, all_states);
  }
  bdd_free(self->dd, all_states);
  return result;
}


BddStates BddFsm_states_inputs_to_states(const BddFsm_ptr self,
                                         BddStatesInputs si)
{
  BddStates states;
  bdd_ptr input_vars_cube;

  BDD_FSM_CHECK_INSTANCE(self);

  input_vars_cube = BddEnc_get_input_vars_cube(self->enc);

  states = bdd_forsome(self->dd, si, input_vars_cube);

  bdd_free(self->dd, input_vars_cube);

  return states;
}


boolean BddFsm_expand_cached_reachable_states(BddFsm_ptr self,
                                              int k,
                                              int max_seconds)
{
  bdd_ptr reachable_states_bdd;
  bdd_ptr from_lower_bound;   /* the frontier */
  bdd_ptr invars;
  node_ptr reachable_states_layers;

  int diameter;
  boolean completed;
  BddStates* layers;
  boolean result;

  long start_time;
  long limit_time;
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;
  NodeMgr_ptr nodemgr;

  BDD_FSM_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->enc));
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  start_time = util_cpu_time();

  /* Transform max_seconds in milliseconds */
  limit_time = max_seconds * 1000;

  /* Initialize the layers list */
  reachable_states_layers = Nil;

  /* Get the cache */
  completed = BddFsm_get_cached_reachable_states(self, &layers, &diameter);

  /* Reload cache if any */
  if (diameter > 0) {
    int i;

    if (completed) {
      return true; /* already ready */
    }
    else {
      /* The cached analysis is not complete, so we have to resume the
         last state */

      /* Regen the list */
      for (i=0; i<diameter; i++) {
        reachable_states_layers =
          cons(nodemgr, (node_ptr) bdd_dup(layers[i]), reachable_states_layers);
      }

      /* Last layer contains the last reachable set */
      reachable_states_bdd = bdd_dup(layers[diameter - 1]);

      /* Get the last frontier */
      if (diameter > 1) {
        /* Compute the last frontier*/
        bdd_ptr tmp;

        tmp = bdd_not(self->dd, layers[diameter - 2]);
        from_lower_bound = bdd_and(self->dd,
                                   reachable_states_bdd,
                                   tmp);
        bdd_free(self->dd, tmp);
      }
      else {
        /* We computed only the init state, so the frontier is the
           init itself */
        from_lower_bound = bdd_dup(layers[0]);
      }
    }
  }
  else {
    /* No cache, we hawe to start from scratch */

    /* Initial state = inits && invars */
    reachable_states_bdd = BddFsm_get_init(self);
    invars = BddFsm_get_state_constraints(self);
    bdd_and_accumulate(self->dd, &reachable_states_bdd, invars);
    bdd_free(self->dd, invars);

    /* The initial frontier is the initial reachables */
    from_lower_bound = bdd_dup(reachable_states_bdd);

    if (bdd_isnot_false(self->dd, reachable_states_bdd)) {
      reachable_states_layers =
        cons(nodemgr, (node_ptr) bdd_dup(reachable_states_bdd), reachable_states_layers);

      diameter = 1;
    }
    else {
      /* If the initial region is empty then diameter is 0 */
      reachable_states_layers = Nil;
      diameter = 0;
    }
  }

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "\ncomputing reachable state space\n");
  }

  /* Real analysis: the cycle terminates when fixpoint is reached so
     no new states can be visited */
  while ((bdd_isnot_false(self->dd, from_lower_bound)) &&
         (0 != k) &&
         ((-1 == max_seconds) ||
          (util_cpu_time()-start_time) < limit_time)) {
    bdd_ptr from_upper_bound, img, not_from_upper_bound;

    /* Decrease the remaining steps if k is not < 0*/
    if (k>0) k--;

    /* Save old reachables */
    from_upper_bound = bdd_dup(reachable_states_bdd);

    if (opt_verbose_level_gt(opts, 2)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,
              "  iteration %d: BDD size = %d, frontier size = %d, states = %g\n",
              diameter, bdd_size(self->dd, reachable_states_bdd),
              bdd_size(self->dd, from_lower_bound),
              BddEnc_count_states_of_bdd(self->enc, reachable_states_bdd));
    }

    /* Get the forward image */
    img = BddFsm_get_forward_image(self, BDD_STATES(from_lower_bound));

    /* Now the reachable states are the old ones union the forward
       image */
    bdd_or_accumulate(self->dd, &reachable_states_bdd, img);
    bdd_free(self->dd, (bdd_ptr) img);

    if (opt_verbose_level_gt(opts, 2)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "  forward step done, size = %d\n",
              bdd_size(self->dd, reachable_states_bdd));
    }

    /* Now, update the frontier */

    /* Negate the old reachables ( = get the complementar set ) */
    not_from_upper_bound = bdd_not(self->dd, from_upper_bound);

    /* Free the old frontier */
    bdd_free(self->dd, from_lower_bound);

    /* New frontier is the differnece between old reachables and new
       ones so we do the intersection between the complementar set of
       old reachables with the new reachables. See also issue 4475 */
    from_lower_bound = bdd_and(self->dd,
                                 reachable_states_bdd,
                                 not_from_upper_bound);

    /* Free old reachables negation */
    bdd_free(self->dd, not_from_upper_bound);

    if (opt_verbose_level_gt(opts, 2)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "  new frontier computed, size = %d\n",
              bdd_size(self->dd, from_lower_bound));
    }

    /* Free old reachables */
    bdd_free(self->dd, from_upper_bound);

    /* increment the diameter */
    ++diameter;

    /* Update the reachable list */
    reachable_states_layers =
      cons(nodemgr, (node_ptr) bdd_dup(reachable_states_bdd), reachable_states_layers);

  } /* while loop */

  result = bdd_is_false(self->dd, from_lower_bound);

  if (result) {
    /*
       Cache the computed layers.
       BddFsm_update_cached_reachables is responsible of the free of the list
       reachable_states_layers
    */

    if (Nil != reachable_states_layers) {
      node_ptr tmp;

      tmp = reachable_states_layers;
      reachable_states_layers = cdr(reachable_states_layers);
      bdd_free(self->dd, (bdd_ptr) car(tmp));
      free_node(nodemgr, tmp);
      diameter --;
    }

    BddFsm_update_cached_reachable_states(self, reachable_states_layers,
                                          diameter, true);
  }
  else {
    /*
       Cache the partial computed layers.
       BddFsm_update_cached_reachables is responsible of the free of the list
       reachable_states_layers
    */
    BddFsm_update_cached_reachable_states(self, reachable_states_layers,
                                          diameter, false);
  }

  /* Free the last reachable states 'set' */
  bdd_free(self->dd, reachable_states_bdd);

  /* Free the last frontier */
  bdd_free(self->dd, from_lower_bound);

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "done\n");
  }

  /* True if fixpoint, false otherwise */
  return result;
}


BddStates BddFsm_states_inputs_to_inputs(const BddFsm_ptr self,
                                         BddStatesInputs si)
{
  BddStates input;
  bdd_ptr vars_cube;

  BDD_FSM_CHECK_INSTANCE(self);

  vars_cube = BddEnc_get_state_frozen_vars_cube(self->enc);
  input = bdd_forsome(self->dd, si, vars_cube);
  bdd_free(self->dd, vars_cube);

  return input;
}


void BddFsm_check_machine(const BddFsm_ptr self)
{
  StreamMgr_ptr streams;
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  BDD_FSM_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->enc));
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Checking totality and deadlock states.\n");
  }

  bdd_fsm_check_init_state_invar_emptiness(self);
  bdd_fsm_check_fairness_emptiness(self);

  if (! BddFsm_is_total(self)) {
    OStream_ptr outstream = StreamMgr_get_output_ostream(streams);
    bdd_ptr noSuccStates = BddFsm_get_not_successor_states(self);
    bdd_ptr ds = BddEnc_pick_one_state(self->enc, noSuccStates);
    NodeList_ptr vars;
    bdd_free(self->dd, noSuccStates);

    StreamMgr_print_output(streams,  "\n##########################################################\n");
    StreamMgr_print_output(streams,  "The transition relation is not total. A state without\n");
    StreamMgr_print_output(streams,  "successors is:\n");

    vars = SymbTable_get_layers_sf_i_vars(self->symb_table,
                                          SymbTable_get_class_layer_names(self->symb_table, (const char*) NULL));
    BddEnc_print_bdd_begin(self->enc, vars, false);
    BddEnc_print_bdd(self->enc, ds, (VPFBEFNNV) NULL, outstream, NULL);

    if (CACHE_IS_EQUAL(reachable.computed, true) ||
        opt_use_reachable_states(opts)) {
      /* here the reachable states calculation has been done or requested. */
      if (! BddFsm_is_deadlock_free(self)) {

        bdd_ptr deadlockStates = BddFsm_get_deadlock_states(self);
        bdd_ptr ds = BddEnc_pick_one_state(self->enc, deadlockStates);
        bdd_free(self->dd, deadlockStates);

        StreamMgr_print_output(streams,  "The transition relation is not deadlock-free.\n");
        StreamMgr_print_output(streams,  "A deadlock state is:\n");
        BddEnc_print_bdd(self->enc, ds, (VPFBEFNNV) NULL, outstream, NULL);
      }
      else {
        StreamMgr_print_output(streams,  "However, all the states without successors are\n");
        StreamMgr_print_output(streams,  "non-reachable, so the machine is deadlock-free.\n");
      }
    }
    else {
      /* reachables states should be calculated */
      StreamMgr_print_output(streams,  "NOTE: No-successor states could be non-reachable, so\n");
      StreamMgr_print_output(streams,  "      the machine could still be deadlock-free.\n");
      StreamMgr_print_output(streams,  "      Reachable states have to be computed to check this.\n");
    }

    BddEnc_print_bdd_end(self->enc);
    NodeList_destroy(vars);

    StreamMgr_print_output(streams,  "##########################################################\n");
    bdd_free(self->dd, ds);
  }
  else {
    StreamMgr_print_output(streams,  "\n##########################################################\n");
    StreamMgr_print_output(streams,  "The transition relation is total: No deadlock state exists\n");
    StreamMgr_print_output(streams,  "##########################################################\n");
  }
}


void BddFsm_apply_synchronous_product_custom_varsets(BddFsm_ptr self,
                                                     const BddFsm_ptr other,
                                                     bdd_ptr state_vars_cube,
                                                     bdd_ptr input_vars_cube,
                                                     bdd_ptr next_vars_cube)
{
  BddFsmCache_ptr new_cache;

  BDD_FSM_CHECK_INSTANCE(self);

  /* check for the same dd manager */
  nusmv_assert(self->dd == other->dd);

  /* check for the same dd manager, in the future we will probably
     relax this constraint  */
  nusmv_assert(self->enc == other->enc);

  /* init */
  bdd_and_accumulate(self->dd, &(self->init), other->init);

  /* invars */
  bdd_and_accumulate(self->dd, &(self->invar_states), other->invar_states);

  /* trans */
  BddTrans_apply_synchronous_product(self->trans, other->trans);

  /* fairness constraints */
  JusticeList_apply_synchronous_product(self->justice, other->justice);
  CompassionList_apply_synchronous_product(self->compassion, other->compassion);

  /* cache substitution */
  new_cache = BddFsmCache_hard_copy(self->cache);
  BddFsmCache_reset_not_reusable_fields_after_product(new_cache);
  BddFsmCache_destroy(self->cache);
  self->cache = new_cache;
}


void BddFsm_apply_synchronous_product(BddFsm_ptr self, const BddFsm_ptr other)
{
  bdd_ptr input_vars_cube;
  bdd_ptr state_vars_cube;
  bdd_ptr next_vars_cube;

  BDD_FSM_CHECK_INSTANCE(self);

  input_vars_cube = BddEnc_get_input_vars_cube(self->enc);
  state_vars_cube = BddEnc_get_state_vars_cube(self->enc);
  next_vars_cube = BddEnc_get_next_state_vars_cube(self->enc);

  BddFsm_apply_synchronous_product_custom_varsets(self,
                                                  other,
                                                  state_vars_cube,
                                                  input_vars_cube,
                                                  next_vars_cube);

  bdd_free(self->dd, (bdd_ptr) next_vars_cube);
  bdd_free(self->dd, (bdd_ptr) state_vars_cube);
  bdd_free(self->dd, (bdd_ptr) input_vars_cube);
}


boolean BddFsm_compute_reachable(BddFsm_ptr self, int k, int t, int* diameter)
{
  BddStates* layers = NULL;

  BDD_FSM_CHECK_INSTANCE(self);

  BddFsm_expand_cached_reachable_states(self, k, t);

  return BddFsm_get_cached_reachable_states(self, &layers, diameter);
}


double BddFsm_count_transitions(const BddFsm_ptr self,
                                BddStatesInputs bdd)
{
  double num = 0;
  size_t array_size;
  bdd_ptr* array;

  BDD_FSM_CHECK_INSTANCE(self);

  {  /* computes all pairs */
    int res;
    bdd_ptr mask_bdd =
        BddEnc_apply_state_frozen_vars_mask_bdd(self->enc, bdd);

    array_size = BddEnc_count_states_inputs_of_bdd(self->enc, mask_bdd);
    nusmv_assert(array_size >= 0);
    array = ALLOC(bdd_ptr, array_size);
    nusmv_assert(NULL != array);

    res = BddEnc_pick_all_terms_states_inputs(
        self->enc, mask_bdd, array, array_size);
    nusmv_assert(!res);

    bdd_free(self->dd, mask_bdd);
  }

  {  /* loop over the pairs, accumulate the number of transitions */
    BddStates one = bdd_true(self->dd);
    size_t j;

    for (j=0; j<array_size; ++j) {
      bdd_ptr image =
          BddFsm_get_constrained_forward_image(self, one, array[j]);
      bdd_ptr mask_image =
          BddEnc_apply_state_frozen_vars_mask_bdd(self->enc, image);

      num += BddEnc_count_states_of_bdd(self->enc, mask_image);

      bdd_free(self->dd, mask_image);
      bdd_free(self->dd, image);
    }

    bdd_free(self->dd, one);
  }

  FREE(array);

  return num;
}


int BddFsm_dump_fsm(BddFsm_ptr self,
                    const NuSMVEnv_ptr env, node_ptr node_expr,
                    char* str_constr,
                    boolean init,
                    boolean invar, boolean trans, boolean fair,
                    boolean reachable, FILE* outfile)
{
  const BddFsm_ptr fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));
  const BddEnc_ptr bdd_enc = BddFsm_get_bdd_encoding(fsm);
  const DDMgr_ptr dd = BddEnc_get_dd_manager(bdd_enc);
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  AddArray_ptr aar_expr = (AddArray_ptr) NULL;
  AddArray_ptr addarray;
  const char** labels;
  int res = 0;

  if (NULL != node_expr) {
    aar_expr = BddEnc_expr_to_addarray(bdd_enc, node_expr, Nil);
  }

  /* here we construct the input for BddEnc_dump_addarray_dot */
  {
    struct {
      boolean enabled;
      const char* label;
      bdd_ptr bdd;
    } info[] = {
      { init, "Init", BddFsm_get_init(fsm) },
      { invar, "Invar", BddFsm_get_state_constraints(fsm) },
      { trans, "Trans", BddFsm_get_monolithic_trans_bdd(fsm) },
      { fair, "Fair", BddFsm_get_fair_states(fsm) },
      { reachable, "Reachables", BddFsm_get_reachable_states(fsm) },
    };
    int i, idx;
    int entries = init + invar + trans + fair + reachable;
    if ((AddArray_ptr) NULL != aar_expr) {
      entries += AddArray_get_size(aar_expr);
    }

    nusmv_assert(entries > 0);
    addarray = AddArray_create(entries);
    labels = ALLOC(const char*, entries);
    nusmv_assert((const char**) NULL != labels);

    for (idx=0, i=0; i<sizeof(info)/sizeof(info[0]) && idx<entries; ++i) {
      if (info[i].enabled) {
        labels[idx] = util_strsav(info[i].label);
        AddArray_set_n(addarray, idx, bdd_to_add(dd, info[i].bdd));
        ++idx;
      }
    }

    /* adds all labels and adds coming possibly from the given expression */
    if ((AddArray_ptr) NULL != aar_expr) {
      const char* oname_fmt = "%s[%0*d]";
      const int digits = (int) log10(AddArray_get_size(aar_expr));
      const int oname_len = (strlen(str_constr) + strlen(oname_fmt) + digits + 1);

      /* keeps going from last reached idx */
      for (i=idx; i<entries; ++i) {
        char* oname = ALLOC(char, oname_len);
        int c;
        nusmv_assert((char*) NULL != oname);
        c = snprintf(oname, oname_len, oname_fmt, str_constr, digits, i-idx);
        SNPRINTF_CHECK(c, oname_len);

        labels[i] = oname;
        AddArray_set_n(addarray, i, add_dup(AddArray_get_n(aar_expr, i-idx)));
      }
    }

    res = BddEnc_dump_addarray_dot(bdd_enc, addarray, labels, outfile);

    /* cleanup */
    for (i=0; i<entries; ++i) { FREE(labels[i]); }
    FREE(labels);
    for (i=0; i<sizeof(info)/sizeof(info[0]); ++i) {
      if ((bdd_ptr) NULL != info[i].bdd) { bdd_free(dd, info[i].bdd); }
    }

    AddArray_destroy(dd, addarray);
    if ((AddArray_ptr) NULL != aar_expr) { AddArray_destroy(dd, aar_expr); }
  }

  return res;
}


/* ---------------------------------------------------------------------- */
/*                         Static functions                               */
/* ---------------------------------------------------------------------- */

/*!
  \brief Private initializer
*/
static void bdd_fsm_init(BddFsm_ptr self,
                         BddEnc_ptr encoding,
                         BddStates init,
                         BddInvarStates invar_states,
                         BddInvarInputs invar_inputs,
                         BddTrans_ptr trans,
                         JusticeList_ptr justice,
                         CompassionList_ptr compassion)
{
  self->enc = encoding;
  self->dd = BddEnc_get_dd_manager(encoding);
  self->symb_table = BaseEnc_get_symb_table(BASE_ENC(encoding));

  /* Here no check for infinite precision variables is done, since the
     BddEnc should not be able to convert expressions containing such
     variables. With this assumption, it is impossible that BDD init,
     invar and trans are built, and consequently, this code should not
     be reached. */

  nusmv_assert(init != NULL);

  self->init = BDD_STATES( bdd_dup((bdd_ptr) init) );
  self->invar_states = BDD_INVAR_STATES( bdd_dup((bdd_ptr) invar_states) );
  self->invar_inputs = BDD_INVAR_INPUTS( bdd_dup((bdd_ptr) invar_inputs) );
  self->trans = trans;
  self->justice = justice;
  self->compassion = compassion;

  self->cache = BddFsmCache_create(self->dd);

  /* check inits and invars for emptiness */
  bdd_fsm_check_init_state_invar_emptiness(self);
}

/*!
  \brief private copy constructor


*/
static void bdd_fsm_copy(const BddFsm_ptr self, BddFsm_ptr copy)
{
  copy->dd = self->dd;
  copy->enc = self->enc;
  copy->symb_table = self->symb_table;

  copy->init = BDD_STATES( bdd_dup((bdd_ptr) self->init ) );
  copy->invar_states =
    BDD_INVAR_STATES( bdd_dup((bdd_ptr) self->invar_states ) );
  copy->invar_inputs  =
    BDD_INVAR_INPUTS( bdd_dup((bdd_ptr) self->invar_inputs ) );

  copy->trans = BDD_TRANS( Object_copy(OBJECT(self->trans)) );
  copy->justice = JUSTICE_LIST( Object_copy(OBJECT(self->justice)));
  copy->compassion = COMPASSION_LIST( Object_copy(OBJECT(self->compassion)));

  copy->cache = BddFsmCache_soft_copy(self->cache);
}

/*!
  \brief private member called by the destructor


*/
static void bdd_fsm_deinit(BddFsm_ptr self)
{
  bdd_free(self->dd, (bdd_ptr) self->init);
  bdd_free(self->dd, (bdd_ptr) self->invar_states);
  bdd_free(self->dd, (bdd_ptr) self->invar_inputs);

  Object_destroy(OBJECT(self->trans), NULL);
  Object_destroy(OBJECT(self->justice), NULL);
  Object_destroy(OBJECT(self->compassion), NULL);

  BddFsmCache_destroy(self->cache);
}

/*!
  \brief Computes the set of reachable states of this machine



  \se Changes the internal cache
*/
static void bdd_fsm_compute_reachable_states(BddFsm_ptr self)
{
  boolean res;

  /* Expand cacked reachable states until fixpoint without time limitations */
  res = BddFsm_expand_cached_reachable_states(self, -1, -1);

  /* Assert that we completed the reachability analysis */
  nusmv_assert(res);
}

/*!
  \brief Returns the set of states and inputs,
   for which a legal transition can be made.

  A legal transition is a transition which satisfy the
   transition relation, and the state, input and next-state satisfy the
   invariants.  So the image S(X, F, I) is computed as follows:
   S(X,F,I) = StateConstr(X,F) & InputConstr(i) & StateConstr(X',F) &
   Tr(X,F,I,X') for some X'
   X - state variables, I - input variables, F - frozen variables.

   Used for planning in strong backward image computation.
   Could update the cache.

   Note: a state is represented by state and frozen variables,
   but frozen variable are never abstracted away.

   Returned bdd is referenced.


  \se Cache can change
*/
static BddStatesInputs bdd_fsm_get_legal_state_input(BddFsm_ptr self)
{
  BddStatesInputs res;

  if ( CACHE_IS_EQUAL(legal_state_input, BDD_STATES_INPUTS(NULL)) ) {
    bdd_ptr one = bdd_true(self->dd);
    /* Here we use weak-backward-image because
       it automatically applies invariant contrains on state, input, next-state.
    */
    bdd_ptr tmp = BddFsm_get_weak_backward_image(self, one);

    CACHE_SET_BDD(legal_state_input, tmp);
    bdd_free(self->dd, one);
    bdd_free(self->dd, tmp);
  }

  res = CACHE_GET_BDD(legal_state_input);
  return res;
}

/*!
  \brief Computes the preimage (if dir = BDD_FSM_DIR_BWD) or the
   postimage (otherwise) of a set of states-inputs pairs.

  Preimage:

   Quantifies away the inputs, and computes the (states-inputs)
   preimage of the resulting set of states.

   Postimage:

   Computes the (states-inputs) postimage of si.

*/

static BddStatesInputs bdd_fsm_EXorEY_SI(const BddFsm_ptr self,
                                         BddStatesInputs si,
                                         BddFsm_dir dir)
{
  BddStatesInputs si_image;
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  BDD_FSM_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->enc));
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));


  if (dir == BDD_FSM_DIR_BWD) {
    BddStates states;

    states = BddFsm_states_inputs_to_states(self, si);
    si_image = BddFsm_get_weak_backward_image(self, states);

    bdd_free(self->dd, states);
  }
  else {
    si_image = BddFsm_get_forward_image_states_inputs(self, si);
  }

  if (opt_use_reachable_states(opts)) {
    bdd_ptr reachable_states_bdd = BddFsm_get_reachable_states(self);

    bdd_and_accumulate(self->dd, &si_image, reachable_states_bdd);
    bdd_free(self->dd, reachable_states_bdd);
  }

  return si_image;
}


/*!
  \brief Computes the set of state-input pairs that satisfy E(f U g)
   (if dir = BDD_FSM_DIR_BWD) or E(f S g) (otherwise),
   with f and g sets of state-input pairs.


*/

static BddStatesInputs bdd_fsm_EUorES_SI(const BddFsm_ptr self,
                                         BddStatesInputs f, BddStatesInputs g,
                                         BddFsm_dir dir)
{
  int i;
  BddStatesInputs resY;
  BddStatesInputs newY;
  BddStatesInputs rg = bdd_dup(g);
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  BDD_FSM_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->enc));
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (opt_use_reachable_states(opts)) {
    bdd_ptr reachable_states_bdd = BddFsm_get_reachable_states(self);

    bdd_and_accumulate(self->dd, &rg, reachable_states_bdd);
    bdd_free(self->dd, reachable_states_bdd);
  }

  resY = bdd_dup(rg);
  newY = bdd_dup(rg);
  bdd_free(self->dd, rg);
  i = 0;

  while (bdd_isnot_false(self->dd, newY)) {
    BddStatesInputs tmp_1, tmp_2;
    BddStatesInputs oldNotY;

    if (opt_verbose_level_gt(opts, 5)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,
              "    size of Y%d = %g <states>x<inputs>, %d BDD nodes\n",
              i, BddEnc_count_states_inputs_of_bdd(self->enc, resY),
              bdd_size(self->dd, resY));
    }

    oldNotY = bdd_not(self->dd, resY);

    tmp_1 = bdd_fsm_EXorEY_SI(self, newY, dir);
    tmp_2 = bdd_and(self->dd, f, tmp_1);
    bdd_free(self->dd, tmp_1);

    bdd_or_accumulate(self->dd, &resY, tmp_2);
    bdd_free(self->dd, tmp_2);

    bdd_free(self->dd, newY);

    /* see issue 4475 */
    newY = bdd_and(self->dd, resY, oldNotY);
    bdd_free(self->dd, oldNotY);

    i++;
  }

  bdd_free(self->dd, newY);

  /* We do not free resY since it is rersposibility of the caller to
     free it. Functions always return a referenced bdd. */
  return BDD_STATES_INPUTS( resY );
}

/*!
  \brief Executes the Emerson-Lei algorithm

  Executes the Emerson-Lei algorithm in the set of states
   given by subspace in the direction given by dir
*/
static BddStatesInputs bdd_fsm_compute_EL_SI_subset(const BddFsm_ptr self,
                                                    BddStatesInputs subspace,
                                                    BddFsm_dir dir)
{
  BddStatesInputs res;
  BddStatesInputs old;
  int i = 0;
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  BDD_FSM_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->enc));
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  res = bdd_true(self->dd);
  old = bdd_false(self->dd);

  /* GFP computation */
  while (res != old) {
    BddStatesInputs _new;

    if (opt_verbose_level_gt(opts, 5)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "  size of res%d = %g <states>x<input>, %d BDD nodes\n",
              i++, BddEnc_count_states_inputs_of_bdd(self->enc, res),
              bdd_size(self->dd, res));
    }

    bdd_free(self->dd, old);
    old = bdd_dup(res);

    /* See issue 4476 */
    _new = bdd_fsm_compute_EL_SI_subset_aux(self, BDD_STATES_INPUTS(res), subspace, dir);

    /* TODO[AT] is not it true that "new" is always a subset of "res" and only
       "subspace" is required? */
    bdd_and_accumulate(self->dd, &res, (bdd_ptr) _new);
    bdd_and_accumulate(self->dd, &res, (bdd_ptr) subspace);

    bdd_free(self->dd, (bdd_ptr) _new);
  }
  bdd_free(self->dd, old);

  return BDD_STATES_INPUTS(res);
}

/*!
  \brief Executes the inner fixed point of the Emerson-Lei algorithm

  Executes the inner fixed point of the Emerson-Lei
   algorithm. Direction is given by dir, fair states are restricted to
   states, backward/forward exploration (other than the last, "strict"
   image) is restricted to subspace.

   TODO[AT] why last state is not restricted by subspace? This is always done
   in bdd_fsm_compute_EL_SI_subset! See issue 4477.

*/
static BddStatesInputs bdd_fsm_compute_EL_SI_subset_aux(const BddFsm_ptr self,
                                                        BddStatesInputs states,
                                                        BddStatesInputs subspace,
                                                        BddFsm_dir dir)
{
  BddStatesInputs res;
  FairnessListIterator_ptr iter;
  BddStatesInputs partial_result;
  int i;

  res = bdd_true(self->dd);
  partial_result = bdd_dup(states);
  i = 0;

  /* Accumulates justice constraints: */
  iter = FairnessList_begin( FAIRNESS_LIST(self->justice) );
  while ( ! FairnessListIterator_is_end(iter) ) {
    /* See issue 4478 */
    BddStatesInputs p;
    BddStatesInputs constrained_state;
    BddStatesInputs temp;

    p = JusticeList_get_p(self->justice, iter);
    constrained_state = bdd_and(self->dd, states, p);
    temp = bdd_fsm_EUorES_SI(self, subspace, constrained_state, dir);

    bdd_free(self->dd, constrained_state);
    bdd_free(self->dd, p);

    bdd_and_accumulate(self->dd, &partial_result, temp);
    bdd_free(self->dd, temp);

    iter = FairnessListIterator_next(iter);
    i++;
  } /* outer while loop */

  res = bdd_fsm_EXorEY_SI(self, partial_result, dir);
  bdd_free(self->dd, partial_result);

  return res;
}

/*!
  \brief Computes the set of (reverse) fair states in subspace

  Computes the set of fair states (if dir =
   BDD_FSM_DIR_BWD) or reverse fair states (otherwise) by calling the
   Emerson-Lei algorithm.
*/
static BddStatesInputs
bdd_fsm_get_fair_or_revfair_states_inputs_in_subspace(const BddFsm_ptr self,
                                                      BddStatesInputs subspace,
                                                      BddFsm_dir dir)
{
  BddStatesInputs fair_or_revfair_states_inputs;

  fair_or_revfair_states_inputs =
    bdd_fsm_compute_EL_SI_subset(self, subspace, dir);

  return fair_or_revfair_states_inputs;
}

/*!
  \brief Computes the set of (reverse) fair states

  Computes the set of fair states (if dir =
   BDD_FSM_DIR_BWD) or reverse fair states (otherwise) by calling the
   Emerson-Lei algorithm.

  \se Cache might change

  \sa bdd_fsm_get_fair_or_revfair_states_inputs_in_subspace
*/
static BddStatesInputs
bdd_fsm_get_fair_or_revfair_states_inputs(BddFsm_ptr self, BddFsm_dir dir)
{
  BddStatesInputs res;
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  BDD_FSM_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->enc));
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if ( (dir == BDD_FSM_DIR_BWD &&
        CACHE_IS_EQUAL(fair_states_inputs, BDD_STATES(NULL))) ||
       (dir == BDD_FSM_DIR_FWD &&
        CACHE_IS_EQUAL(revfair_states_inputs, BDD_STATES(NULL))) ) {
    BddStatesInputs si;
    BddStates fair_or_revfair_si;

    /* Computation is restricted to those states that have a legal
       successor (if dir = BDD_FSM_DIR_BWD) or predecessor
       (otherwise). This potentially reduces the number of states to
       be considered. */
    /* TODO[AT] Is it true that this predecessor/successor are useful? (are
       there benchmarks?) */
    si = BddFsm_get_states_inputs_constraints(self, dir);

    if (opt_use_reachable_states(opts)) {
      bdd_ptr reachable_states_bdd = BddFsm_get_reachable_states(self);

      bdd_and_accumulate(self->dd, &si, reachable_states_bdd);
      bdd_free(self->dd, reachable_states_bdd);
    }

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      if (dir == BDD_FSM_DIR_BWD) {
        Logger_log(logger, "Computing the set of fair <state>x<input> pairs\n");
      }
      else {
        Logger_log(logger, "Computing the set of reverse fair <state>x<input> pairs\n");
      }
    }

    fair_or_revfair_si =
      bdd_fsm_get_fair_or_revfair_states_inputs_in_subspace(self, si, dir);

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,"done\n");
    }

    if (dir == BDD_FSM_DIR_BWD) {
      CACHE_SET_BDD(fair_states_inputs, fair_or_revfair_si);
    }
    else {
      CACHE_SET_BDD(revfair_states_inputs, fair_or_revfair_si);
    }

    bdd_free(self->dd, fair_or_revfair_si);
    bdd_free(self->dd, si);

  }

  if (dir == BDD_FSM_DIR_BWD) {
    res = CACHE_GET_BDD(fair_states_inputs);
  }
  else {
    res = CACHE_GET_BDD(revfair_states_inputs);
  }

  return res;
}

/*!
  \brief Check inits for emptiness, and prints a warning if needed


*/
static void bdd_fsm_check_init_state_invar_emptiness(const BddFsm_ptr self)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  /* checks for emptiness of inits: */
  if (bdd_is_false(self->dd, self->init)) {
    ErrorMgr_warning_fsm_init_empty(errmgr);
  }
  if (bdd_is_false(self->dd, self->invar_states)) {
    ErrorMgr_warning_fsm_invar_empty(errmgr);
  }
}

/*!
  \brief Checks fair states for emptiness, as well as fot the
   intersaction of fair states and inits. Prints a warning if needed


*/
static void bdd_fsm_check_fairness_emptiness(const BddFsm_ptr self)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  bdd_ptr fair;

  fair = BddFsm_get_fair_states_inputs(self);

  if (bdd_is_false(self->dd, fair)) {
    ErrorMgr_warning_fsm_fairness_empty(errmgr);
  }
  else if (bdd_isnot_false(self->dd, self->init)) {
    bdd_ptr fair_init = bdd_and(self->dd, self->init, fair);

    if (bdd_is_false(self->dd, fair_init)) {
      ErrorMgr_warning_fsm_init_and_fairness_empty(errmgr);
    }
    bdd_free(self->dd, fair_init);
  }

  bdd_free(self->dd, fair);
}
