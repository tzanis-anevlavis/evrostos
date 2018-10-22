/* ---------------------------------------------------------------------------


  This file is part of the ``bdd_fsm'' package of NuSMV version 2. 
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
  \brief Private interface for package bdd_fsm

  \todo: Missing description

*/



#ifndef __NUSMV_CORE_FSM_BDD_BDD_INT_H__
#define __NUSMV_CORE_FSM_BDD_BDD_INT_H__

#include "nusmv/core/dd/dd.h"
#include "nusmv/core/opt/opt.h"

/* members are public from within the bdd fsm */
typedef struct BddFsmCache_TAG 
{
  /* for sharing families (i.e. compatible instances): */
  unsigned int* family_counter;
  
  /* dd manager */
  DDMgr_ptr dd;
  
  /* cached values */
  BddStates  fair_states;
  BddStatesInputs fair_states_inputs;
  BddStates  revfair_states;
  BddStatesInputs revfair_states_inputs;

  /* interface to this structure is private */
  struct BddFsmReachable_TAG
  {
    boolean computed;
    BddStates* layers;   /* array of bdds */
    int diameter;
    BddStates reachable_states; /* Used to hold the bdd representing the
                                   whole set of reachable states of the
                                   BddFsm.  These may be computed for
                                   example by Guided Reachability */
  } reachable;

  BddStates successor_states;
  BddStates not_successor_states;
  BddStates deadlock_states;
  BddStatesInputs legal_state_input; 
  BddStatesInputs monolithic_trans;

} BddFsmCache;

/*!
  \struct BddFsmCache
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef struct BddFsmCache_TAG* BddFsmCache_ptr;

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BDD_FSM_CACHE(x) \
         ( (BddFsmCache_ptr) x )

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BDD_FSM_CACHE_CHECK_INSTANCE(x) \
         ( nusmv_assert(BDD_FSM_CACHE(x) != BDD_FSM_CACHE(NULL)) )

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CACHE_SET(member, value) \
         (self->cache->member = value)

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CACHE_GET(member) \
         (self->cache->member)

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CACHE_SET_BDD(member, value) \
         (self->cache->member = bdd_dup(value))

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CACHE_GET_BDD(member) \
         (bdd_dup(self->cache->member))

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CACHE_IS_EQUAL(member, value) \
         (self->cache->member == value)

/*!
  \methodof BddFsmCache
  \brief Class contructor

  
*/
BddFsmCache_ptr BddFsmCache_create(DDMgr_ptr dd);

/*!
  \methodof BddFsmCache
  \brief Class destructor

  
*/
void BddFsmCache_destroy(BddFsmCache_ptr self);

/*!
  \methodof BddFsmCache
  \brief Class copy constructor

  Hardly copy the instance, by creating a new, separate family

  \sa BddFsmCache_soft_copy
*/
BddFsmCache_ptr 
BddFsmCache_hard_copy(const BddFsmCache_ptr self);

/*!
  \methodof BddFsmCache
  \brief Family soft copier

  Returns the same instance, but the family counter is increased,
  in order to handle sharing of the instance. The destructor will actually
  destroy this instance only when the last family member will be destroyed

  \sa BddFsmCache_hard_copy
*/
BddFsmCache_ptr 
BddFsmCache_soft_copy(const BddFsmCache_ptr self);

/*!
  \methodof BddFsmCache
  \brief Copies reachable states information within other into self

  This method is used when copying reachable states
  information between to FSMs
*/
void 
BddFsmCache_copy_reachables(BddFsmCache_ptr self, 
                            const BddFsmCache_ptr other);

/*!
  \methodof BddFsmCache
  \brief Fills cache structure with reachable states information

  Fills cache structure with reachable states
                information.  The given BDD is supposed to represent
                the whole set of reachable states of the Bdd FSM. It
                should NOT contain other informations (such as onion
                rings ecc)
*/
void BddFsmCache_set_reachable_states(BddFsmCache_ptr self,
                                             BddStates reachable);

/*!
  \methodof BddFsmCache
  \brief Fills cache structure with reachable states information

  Given list layers_list must be reversed, from last
  layer to the layer corresponding to initial state.  Given list
  layers_list will be destroyed.

  \se given list layers_list will be destroyed, cache
  changes
*/
void BddFsmCache_set_reachables(BddFsmCache_ptr self, 
                                       node_ptr   layers_list, 
                                       const int  diameter,
                                       boolean completed);

/*!
  \methodof BddFsmCache
  \brief Resets any field in the cache that must be recalculated

  This is called when syncronous product is carried out.
  In particular LTL BDD-based model checking, after having applied the
  product between the original fsm and the fsm coming from the tableau
  contruction, needs to disable the cache sharing and to recalculate
  fair states and other fields, since the fsm changed.
  All fsm's fields that need to be recalculated after the syncronous
  product must be reset by this method.

  \sa BddFsm_apply_synchronous_product
*/
void 
BddFsmCache_reset_not_reusable_fields_after_product(BddFsmCache_ptr self);

#endif /* __NUSMV_CORE_FSM_BDD_BDD_INT_H__ */
