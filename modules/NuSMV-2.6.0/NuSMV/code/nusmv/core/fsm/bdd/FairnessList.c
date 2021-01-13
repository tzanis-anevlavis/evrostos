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
  \author Roberto Cavada
  \brief Declares the interface for the classes that contains fairness
  conditions

  Exported classes are:
  - FairnessList (pure, base class)
  - JusticeList
  - CompassionList
  

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/fsm/bdd/FairnessList.h"

#include "nusmv/core/fsm/bdd/bddInt.h"
#include "nusmv/core/utils/object_private.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/parser/symbols.h"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define FAIRNESS_LIST_ITERATOR(x) \
       ((FairnessListIterator_ptr) x)

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define END_ITERATOR \
       FAIRNESS_LIST_ITERATOR(Nil)


typedef struct FairnessList_TAG
{
  INHERITS_FROM(Object);

  FairnessListIterator_ptr first;
  DDMgr_ptr dd;

  /* ---------------------------------------------------------------------- */
  /*     Virtuals                                                           */
  /* ---------------------------------------------------------------------- */

} FairnessList;


typedef struct JusticeList_TAG
{
  INHERITS_FROM(FairnessList);

} JusticeList;


typedef struct CompassionList_TAG
{
  INHERITS_FROM(FairnessList);

} CompassionList;

/* ---------------------------------------------------------------------- */


static void fairness_list_finalize(Object_ptr object, void* dummy);

static void fairness_list_init(FairnessList_ptr self,
                               DDMgr_ptr dd_manager);

static void fairness_list_deinit(FairnessList_ptr self);

static void fairness_list_deinit_aux(FairnessList_ptr self,
                                     FairnessListIterator_ptr iter);

static void fairness_list_finalize(Object_ptr object, void* dummy);

static Object_ptr fairness_list_copy(const Object_ptr self);

static node_ptr
fairness_list_copy_aux(const FairnessList_ptr self,
                       const FairnessListIterator_ptr iter);

FairnessList_ptr FairnessList_create(DDMgr_ptr dd_manager)
{
  FairnessList_ptr self = ALLOC(FairnessList, 1);
  FAIRNESS_LIST_CHECK_INSTANCE(self);

  fairness_list_init(self, dd_manager);

  return self;
}

FairnessListIterator_ptr FairnessList_begin(const FairnessList_ptr self)
{
  FAIRNESS_LIST_CHECK_INSTANCE(self);
  return self->first;
}


boolean FairnessList_is_empty(const FairnessList_ptr self)
{
  FAIRNESS_LIST_CHECK_INSTANCE(self);
  return self->first == END_ITERATOR;
}

boolean FairnessListIterator_is_end(const FairnessListIterator_ptr self)
{
  return (self == END_ITERATOR);
}

FairnessListIterator_ptr
FairnessListIterator_next(const FairnessListIterator_ptr self)
{
  FairnessListIterator_ptr res = END_ITERATOR;

  if (self != END_ITERATOR) {
    res = FAIRNESS_LIST_ITERATOR( cdr(NODE_PTR(self)) );
  }

  return res;
}

JusticeList_ptr JusticeList_create(DDMgr_ptr dd_manager)
{
  JusticeList_ptr self = ALLOC(JusticeList, 1);
  JUSTICE_LIST_CHECK_INSTANCE(self);

  fairness_list_init(FAIRNESS_LIST(self), dd_manager);

  return self;
}

BddStates JusticeList_get_p(const JusticeList_ptr self,
                            const FairnessListIterator_ptr iter)
{
  BddStates res;
  node_ptr bdd;

  JUSTICE_LIST_CHECK_INSTANCE(self);
  nusmv_assert(iter != END_ITERATOR);

  bdd = car((node_ptr) iter);
  nusmv_assert(node_get_type(bdd) == BDD);
  res = BDD_STATES(car(bdd));
  bdd_ref((bdd_ptr) res);

  return res;
}

void JusticeList_append_p(JusticeList_ptr self, BddStates p)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(FAIRNESS_LIST(self)->dd));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  node_ptr _new;

  JUSTICE_LIST_CHECK_INSTANCE(self);

  bdd_ref((bdd_ptr) p);

  _new = new_node(nodemgr, BDD, (node_ptr) p, Nil);
  FAIRNESS_LIST(self)->first = cons(nodemgr, (node_ptr) _new,
                                    FAIRNESS_LIST(self)->first);
}

void JusticeList_apply_synchronous_product(JusticeList_ptr self,
                                           const JusticeList_ptr other)
{
  FairnessListIterator_ptr iter;

  JUSTICE_LIST_CHECK_INSTANCE(self);

  iter = FairnessList_begin(FAIRNESS_LIST(other));
  while (! FairnessListIterator_is_end(iter)) {
    BddStates p = JusticeList_get_p(other, iter);

    JusticeList_append_p(self, p);
    bdd_free( FAIRNESS_LIST(self)->dd, p );

    iter = FairnessListIterator_next(iter);
  }
}

CompassionList_ptr CompassionList_create(DDMgr_ptr dd_manager)
{
  CompassionList_ptr self = ALLOC(CompassionList, 1);
  COMPASSION_LIST_CHECK_INSTANCE(self);

  fairness_list_init(FAIRNESS_LIST(self), dd_manager);

  return self;
}

BddStates CompassionList_get_p(const CompassionList_ptr self,
                               const FairnessListIterator_ptr iter)
{
  BddStates res;
  node_ptr couple;
  node_ptr bdd;

  COMPASSION_LIST_CHECK_INSTANCE(self);
  nusmv_assert(iter != END_ITERATOR);

  couple = car((node_ptr) iter);
  nusmv_assert(node_get_type(couple) == CONS);

  bdd = car(couple);
  nusmv_assert(node_get_type(bdd) == BDD);

  res = BDD_STATES(car(bdd));
  bdd_ref((bdd_ptr) res);

  return res;
}

BddStates CompassionList_get_q(const CompassionList_ptr self,
                               const FairnessListIterator_ptr iter)
{
  BddStates res;
  node_ptr couple;
  node_ptr bdd;

  COMPASSION_LIST_CHECK_INSTANCE(self);
  nusmv_assert(iter != END_ITERATOR);

  couple = car((node_ptr) iter);
  nusmv_assert(node_get_type(couple) == CONS);

  bdd = cdr(couple);
  nusmv_assert(node_get_type(bdd) == BDD);

  res = BDD_STATES(car(bdd));
  bdd_ref((bdd_ptr) res);

  return res;
}

void CompassionList_append_p_q(CompassionList_ptr self,
                               BddStates p, BddStates q)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(FAIRNESS_LIST(self)->dd));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  node_ptr bdd_l, bdd_r;
  node_ptr couple;

  COMPASSION_LIST_CHECK_INSTANCE(self);

  bdd_l = new_node(nodemgr, BDD, (node_ptr) p, Nil);
  bdd_r = new_node(nodemgr, BDD, (node_ptr) q, Nil);

  couple = cons(nodemgr, bdd_l, bdd_r);
  FAIRNESS_LIST(self)->first = cons(nodemgr, couple, FAIRNESS_LIST(self)->first);

  bdd_ref((bdd_ptr) p);
  bdd_ref((bdd_ptr) q);
}

void CompassionList_apply_synchronous_product(CompassionList_ptr self,
                                              const CompassionList_ptr other)
{
  FairnessListIterator_ptr iter;

  COMPASSION_LIST_CHECK_INSTANCE(self);

  iter = FairnessList_begin(FAIRNESS_LIST(other));
  while (! FairnessListIterator_is_end(iter)) {
    BddStates p = CompassionList_get_p(other, iter);
    BddStates q = CompassionList_get_q(other, iter);


    CompassionList_append_p_q(self, p, q);
    bdd_free( FAIRNESS_LIST(self)->dd, p );
    bdd_free( FAIRNESS_LIST(self)->dd, q );

    iter = FairnessListIterator_next(iter);
  }

}



/* ---------------------------------------------------------------------- */
/* Static members                                                         */
/* ---------------------------------------------------------------------- */

/*!
  \brief 

  
*/
static void fairness_list_init(FairnessList_ptr self,
                               DDMgr_ptr dd_manager)
{
  /* initializes the base class */
  object_init(OBJECT(self));

  self->first = END_ITERATOR;
  self->dd   = dd_manager;

  OVERRIDE(Object, finalize) = fairness_list_finalize;
  OVERRIDE(Object, copy) = fairness_list_copy;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void fairness_list_deinit(FairnessList_ptr self)
{
  object_deinit(OBJECT(self));

  /* deletes the list and any contained BDDs */
  fairness_list_deinit_aux(self, self->first);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void fairness_list_deinit_aux(FairnessList_ptr self,
                                     FairnessListIterator_ptr iter)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->dd));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  if( ! FairnessListIterator_is_end(iter) ) {
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    switch(node_get_type(NODE_PTR(iter))) {
    case CONS:
      fairness_list_deinit_aux( self,
                                FAIRNESS_LIST_ITERATOR(car(NODE_PTR(iter))) );
      fairness_list_deinit_aux( self,
                                FAIRNESS_LIST_ITERATOR(cdr(NODE_PTR(iter))) );
      break;

    case BDD:
      bdd_free(self->dd, (bdd_ptr) car(NODE_PTR(iter)));
      break;

    default:
      ErrorMgr_internal_error(errmgr, "fairness_list_deinit_aux: unexpected  %d-type node.",
                     node_get_type(NODE_PTR(iter)));
    }

    free_node(nodemgr, NODE_PTR(iter));
  }
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void fairness_list_finalize(Object_ptr object, void* dummy)
{
  FairnessList_ptr self = FAIRNESS_LIST(object);

  fairness_list_deinit(self);
  FREE(self);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static Object_ptr fairness_list_copy(const Object_ptr object)
{
  FairnessList_ptr self = FAIRNESS_LIST(object);

  FairnessList_ptr copy = ALLOC(FairnessList, 1);
  FAIRNESS_LIST_CHECK_INSTANCE(copy);

  /* copies base class instances: */
  object_copy_aux(OBJECT(self), OBJECT(copy));

  /* copies meembers: */
  copy->dd = self->dd;

  /* copies the list of bdds */
  copy->first = fairness_list_copy_aux(self, self->first);

  return OBJECT(copy);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static node_ptr fairness_list_copy_aux(const FairnessList_ptr self,
                                       const FairnessListIterator_ptr iter)
{
  node_ptr res;

  if( ! FairnessListIterator_is_end(iter)) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->dd));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    node_ptr iter2 = NODE_PTR(iter);
    FairnessListIterator_ptr left;
    FairnessListIterator_ptr right;

    switch (node_get_type(iter2)) {

    case CONS:
      left  = FAIRNESS_LIST_ITERATOR(car(iter2));
      right = FAIRNESS_LIST_ITERATOR(cdr(iter2));
      res = cons(nodemgr,  fairness_list_copy_aux(self, left),
                  fairness_list_copy_aux(self, right) );
      break;

    case BDD:
      res = new_node(nodemgr, BDD,
                     NODE_PTR(bdd_dup((bdd_ptr) car(iter2)) ),
                     Nil);
      break;

    default:
      ErrorMgr_internal_error(errmgr, "fairness_list_copy_aux: unexpected  %d-type node.",
                     node_get_type(iter2));
    }

  }
  else {
    res = Nil;
  }

  return res;
}
