/* ---------------------------------------------------------------------------


   This file is part of the ``fsm.be'' package of NuSMV version 2.
   Copyright (C) 2005 by FBK-irst.

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
  \brief Implementation of class BeFsm

  \todo: Missing description

*/


#include "nusmv/core/fsm/be/BeFsm.h"

#include "nusmv/core/bmc/bmcCheck.h"
#include "nusmv/core/bmc/bmcConv.h"

#include "nusmv/core/utils/error.h"
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct BeFsm_TAG {
  BeEnc_ptr be_enc;
  be_ptr init;
  be_ptr invar;
  be_ptr trans;
  node_ptr fairness_list;

} BeFsm;


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

static void be_fsm_init(BeFsm_ptr self,
                        BeEnc_ptr be_enc, const be_ptr init,
                        const be_ptr invar, const be_ptr trans,
                        const node_ptr list_of_be_fairness);

static void be_fsm_deinit(BeFsm_ptr self);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

BeFsm_ptr BeFsm_create_from_sexp_fsm(BeEnc_ptr be_enc,
                                     const BoolSexpFsm_ptr bfsm)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  BeFsm_ptr self;
  SexpFsm_ptr _bfsm = SEXP_FSM(bfsm);
  node_ptr list_of_valid_fairness;

  nusmv_assert(SexpFsm_is_boolean(_bfsm));

  list_of_valid_fairness =
    Bmc_CheckFairnessListForPropositionalFormulae(env, SexpFsm_get_justice(_bfsm));

  self = BeFsm_create(be_enc,
                      Bmc_Conv_Bexp2Be(be_enc, SexpFsm_get_init(_bfsm)),
                      Bmc_Conv_Bexp2Be(be_enc, SexpFsm_get_invar(_bfsm)),
                      Bmc_Conv_Bexp2Be(be_enc, SexpFsm_get_trans(_bfsm)),
                      Bmc_Conv_BexpList2BeList(be_enc, list_of_valid_fairness));

  free_list(nodemgr, list_of_valid_fairness);
  return self;
}

BeFsm_ptr BeFsm_create(BeEnc_ptr be_enc,
                       const be_ptr init,
                       const be_ptr invar,
                       const be_ptr trans,
                       const node_ptr list_of_be_fairness)
{
  BeFsm_ptr self = ALLOC(BeFsm, 1);
  BE_FSM_CHECK_INSTANCE(self);

  be_fsm_init(self, be_enc, init, invar, trans, list_of_be_fairness);

  return self;
}

void BeFsm_destroy(BeFsm_ptr self)
{
  BE_FSM_CHECK_INSTANCE(self);

  be_fsm_deinit(self);
  FREE(self);
}

BeFsm_ptr BeFsm_copy(BeFsm_ptr self)
{
  BeFsm_ptr copy;
  BE_FSM_CHECK_INSTANCE(self);

  /* Necessary since the master in BE is built only after be_setup */
  copy = BeFsm_create(self->be_enc,
                      BeFsm_get_init(self),
                      BeFsm_get_invar(self),
                      BeFsm_get_trans(self),
                      BeFsm_get_fairness_list(self));
  return copy;
}

BeEnc_ptr BeFsm_get_be_encoding(const BeFsm_ptr self)
{
  BE_FSM_CHECK_INSTANCE(self);
  return self->be_enc;
}

be_ptr BeFsm_get_init(const BeFsm_ptr self)
{
  BE_FSM_CHECK_INSTANCE(self);
  return self->init;
}

be_ptr BeFsm_get_invar(const BeFsm_ptr self)
{
  BE_FSM_CHECK_INSTANCE(self);
  return self->invar;
}

be_ptr BeFsm_get_trans(const BeFsm_ptr self)
{
  BE_FSM_CHECK_INSTANCE(self);
  return self->trans;
}

node_ptr BeFsm_get_fairness_list(const BeFsm_ptr self)
{
  BE_FSM_CHECK_INSTANCE(self);
  return self->fairness_list;
}

void BeFsm_apply_synchronous_product(BeFsm_ptr self, const BeFsm_ptr other) {
  node_ptr list;
  Be_Manager_ptr manager;
  NodeMgr_ptr nodemgr;
  NuSMVEnv_ptr env;

  BE_FSM_CHECK_INSTANCE(self);

  manager = BeEnc_get_be_manager(self->be_enc);
  env = EnvObject_get_environment(ENV_OBJECT(self->be_enc));
  nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  list = other->fairness_list;

  while (Nil != list) {
    self->fairness_list = cons(nodemgr, car(list), self->fairness_list);

    list = cdr(list);
  }

  self->init = Be_And(manager, self->init, other->init);
  self->trans = Be_And(manager, self->trans, other->trans);
  self->invar = Be_And(manager, self->invar, other->invar);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Private service to initialize the internal members

  

  \se self will change internally
*/
static void be_fsm_init(BeFsm_ptr self,
                        BeEnc_ptr be_enc, const be_ptr init,
                        const be_ptr invar, const be_ptr trans,
                        const node_ptr list_of_be_fairness)
{
  self->be_enc = be_enc;
  self->init = init;
  self->invar = invar;
  self->trans = trans;
  self->fairness_list = list_of_be_fairness;
}

/*!
  \brief Private service to deinitialize the internal members

  
*/
static void be_fsm_deinit(BeFsm_ptr self)
{

}


