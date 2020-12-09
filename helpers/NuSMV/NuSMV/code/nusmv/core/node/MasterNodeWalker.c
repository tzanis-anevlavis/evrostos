/* ---------------------------------------------------------------------------


  This file is part of the ``node'' package of NuSMV version 2.
  Copyright (C) 2006 by FBK-irst.

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
  \brief Implementaion of class 'MasterNodeWalker'

  \todo: Missing description

*/



#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/MasterNodeWalker.h"
#include "nusmv/core/node/MasterNodeWalker_private.h"

#include "nusmv/core/node/NodeWalker_private.h"
#include "nusmv/core/node/nodeInt.h"

#include "nusmv/core/utils/utils.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
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
static void master_node_walker_finalize(Object_ptr object, void* dummy);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

MasterNodeWalker_ptr MasterNodeWalker_create(const NuSMVEnv_ptr env)
{
  MasterNodeWalker_ptr self = ALLOC(MasterNodeWalker, 1);
  MASTER_NODE_WALKER_CHECK_INSTANCE(self);

  master_node_walker_init(self, env);
  return self;
}

void MasterNodeWalker_destroy(MasterNodeWalker_ptr self)
{
  MASTER_NODE_WALKER_CHECK_INSTANCE(self);
  Object_destroy(OBJECT(self), NULL);
}

boolean MasterNodeWalker_register_walker(MasterNodeWalker_ptr self,
                                         NodeWalker_ptr walker)
{
  ListIter_ptr iter;

  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  MASTER_NODE_WALKER_CHECK_INSTANCE(self);

  if (opt_verbose_level_gt(opts, 3)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "MasterNodeWalker: registering walker '%s'...",
      NodeWalker_get_name(walker));
  }

  iter = NodeList_get_first_iter(self->walkers);
  while (!ListIter_is_end(iter)) {
    NodeWalker_ptr pr =
      NODE_WALKER(NodeList_get_elem_at(self->walkers, iter));

    if (pr == walker) return false; /* already registered */

    if (NodeWalker_collides(walker, pr)) {
      ErrorMgr_rpterr(errmgr, "The walker '%s' partition collides with the " \
       "registered walker '%s'\n",
       NodeWalker_get_name(walker),
       NodeWalker_get_name(pr));
    }

    iter = ListIter_get_next(iter);
  }

  /* ok, not found and valid partition: appends and sets it up */
  NodeList_append(self->walkers, (node_ptr) walker);
  node_walker_set_master(walker, self);

  if (opt_verbose_level_gt(opts, 3)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, " ok\n");
  }
  return true;
}

NodeWalker_ptr MasterNodeWalker_unregister_walker(MasterNodeWalker_ptr self,
                                                  const char* name)
{
  ListIter_ptr iter;

  MASTER_NODE_WALKER_CHECK_INSTANCE(self);

  iter = NodeList_get_first_iter(self->walkers);
  while (!ListIter_is_end(iter)) {
    NodeWalker_ptr pr = NODE_WALKER(
          NodeList_get_elem_at(self->walkers, iter));

    if (strcmp(NodeWalker_get_name(pr), name) == 0) {
      NodeList_remove_elem_at(self->walkers, iter); /* unregistration */
      node_walker_set_master(pr, MASTER_NODE_WALKER(NULL));
      return pr;
    }
    iter = ListIter_get_next(iter);
  }

  return NODE_WALKER(NULL); /* not found */
}

NodeWalker_ptr
MasterNodeWalker_get_walker(MasterNodeWalker_ptr self, const char* name)
{
  ListIter_ptr iter;

  MASTER_NODE_WALKER_CHECK_INSTANCE(self);

  iter = NodeList_get_first_iter(self->walkers);
  while (!ListIter_is_end(iter)) {
    NodeWalker_ptr pr = NODE_WALKER(
          NodeList_get_elem_at(self->walkers, iter));

    if (strcmp(NodeWalker_get_name(pr), name) == 0) return pr;
    iter = ListIter_get_next(iter);
  }

  return NODE_WALKER(NULL); /* not found */
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void master_node_walker_init(MasterNodeWalker_ptr self, const NuSMVEnv_ptr env)
{
  /* base class initialization */
  env_object_init(ENV_OBJECT(self), env);

  /* members initialization */
  self->walkers = NodeList_create();

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = master_node_walker_finalize;
}

void master_node_walker_deinit(MasterNodeWalker_ptr self)
{
  /* members deinitialization */
  ListIter_ptr iter = NodeList_get_first_iter(self->walkers);
  while (!ListIter_is_end(iter)) {
    NodeWalker_ptr w = NODE_WALKER(NodeList_get_elem_at(self->walkers, iter));

    /* Prepare the iterator ready to point the next iterator, since
       this element will be removed from the self->walkers list by the
       node_walker_deinit function, which unregisters itself from this
       master walker */
    iter = ListIter_get_next(iter);

    NodeWalker_destroy(w);
  }

  NodeList_destroy(self->walkers);

  /* base class initialization */
  env_object_deinit(ENV_OBJECT(self));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The MasterNodeWalker class virtual finalizer

  Called by the class destructor
*/
static void master_node_walker_finalize(Object_ptr object, void* dummy)
{
  MasterNodeWalker_ptr self = MASTER_NODE_WALKER(object);

  master_node_walker_deinit(self);
  FREE(self);
}


/**AutomaticEnd***************************************************************/

