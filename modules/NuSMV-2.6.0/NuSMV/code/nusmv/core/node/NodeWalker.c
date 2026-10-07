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
  \brief Implementaion of class 'NodeWalker'

  \todo: Missing description

*/


#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeWalker.h"
#include "nusmv/core/node/NodeWalker_private.h"
#include "nusmv/core/node/MasterNodeWalker_private.h"
#include "nusmv/core/node/nodeInt.h"

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/defs.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'NodeWalker_private.h' for class 'NodeWalker' definition. */

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

static void node_walker_finalize(Object_ptr object, void* dummy);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

NodeWalker_ptr
NodeWalker_create(const NuSMVEnv_ptr env, const char* name,
                  int low, size_t num, boolean can_handle_null)
{
  NodeWalker_ptr self = ALLOC(NodeWalker, 1);
  NODE_WALKER_CHECK_INSTANCE(self);

  node_walker_init(self, env, name, low, num, can_handle_null);
  return self;
}

void NodeWalker_destroy(NodeWalker_ptr self)
{
  NODE_WALKER_CHECK_INSTANCE(self);
  Object_destroy(OBJECT(self), NULL);
}

boolean NodeWalker_can_handle(const NodeWalker_ptr self, node_ptr n)
{
  NODE_WALKER_CHECK_INSTANCE(self);

  if (n != (node_ptr) NULL) {
    int i = node_get_type(n);
    return (i >= self->low) && (i < (self->low + self->num));
  }

  /* for NULL case ask the specific walker */
  return node_walker_can_handle_null_node(self);
}

const char* NodeWalker_get_name(const NodeWalker_ptr self)
{
  NODE_WALKER_CHECK_INSTANCE(self);
  return (const char*) self->name;
}

boolean NodeWalker_collides(const NodeWalker_ptr self,
                            const NodeWalker_ptr other)
{
  int l1, l2, h1, h2;

  NODE_WALKER_CHECK_INSTANCE(self);

  l1 = self->low; l2 = other->low;
  h1 = l1 + self->num - 1; h2 = l2 + other->num - 1;

  return !((l2 > h1) || (l1 > h2) || (h1 < l2) || (h2 < l1));
}


int NodeWalker_add_node_transformation(NodeWalker_ptr self,
                                       const NodeTransformation* nt)
{
  NODE_WALKER_CHECK_INSTANCE(self);
  if (NULL == self->node_transformations) {
    self->node_transformations = array_alloc(NodeTransformation, 1);
    nusmv_assert(NULL != self->node_transformations);
  }

  array_insert_last(NodeTransformation, self->node_transformations, *nt);
  return array_n(self->node_transformations) - 1;
}


void NodeWalker_remove_node_transformation(NodeWalker_ptr self,
                                           int transf_handle)
{
  int len;

  NODE_WALKER_CHECK_INSTANCE(self);
  nusmv_assert(NULL != self->node_transformations);

  len = array_n(self->node_transformations);
  nusmv_assert(transf_handle < len);

  if (len == 1) {
    array_free(self->node_transformations);
    self->node_transformations = NULL;
  }
  else {
    /* we remove the element by copying the array up to the given index */
    array_t* _new = array_alloc(NodeTransformation, len - 1);
    int idx;
    nusmv_assert(NULL != _new);

    for (idx=0; idx < len; ++idx) {
      if (idx != transf_handle) {
        NodeTransformation* ntp =
          array_fetch_p(NodeTransformation, self->node_transformations, idx);
        array_insert_last(NodeTransformation, _new, *ntp);
      }
    }

    array_free(self->node_transformations);
    self->node_transformations = _new;
  }
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void node_walker_init(NodeWalker_ptr self, const NuSMVEnv_ptr env,
                      const char* name, int low, size_t num,
                      boolean can_handle_null)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  /* base class initialization */
  env_object_init(ENV_OBJECT(self), env);

  /* members initialization */
  if (name != (const char*) NULL) {
    self->name = ALLOC(char, strlen(name) + 1);
    nusmv_assert(self->name != (char*) NULL);
    strcpy(self->name, name);
  }
  else self->name = (char*) NULL;

  self->master = MASTER_NODE_WALKER(NULL);
  self->low = low;
  self->num = num;
  self->can_handle_null = can_handle_null;

  /* members initialization */
  self->node_transformations = NULL;

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = node_walker_finalize;

  if (opt_verbose_level_gt(opts, 8)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
            "Created walker '%s' to handle nodes from %d to %" PRIuPTR,
            name, low, low+num-1);
    if (can_handle_null) Logger_log(logger, " (and NULL nodes)\n");
    else Logger_log(logger, "\n");
  }
}

void node_walker_deinit(NodeWalker_ptr self)
{
  /* members deinitialization */
  if (self->master != MASTER_NODE_WALKER(NULL)) {
    MasterNodeWalker_unregister_walker(self->master, self->name);
  }

  /* members deinitialization */
  if (NULL != self->node_transformations) {
    array_free(self->node_transformations);
    self->node_transformations = NULL;
  }

  if (self->name != (char*) NULL) FREE(self->name);

  /* base class de-initialization */
  env_object_deinit(ENV_OBJECT(self));
}

void node_walker_set_master(NodeWalker_ptr self, MasterNodeWalker_ptr master)
{
  if (self->master == master) return; /* not the same master */

  if ((master != MASTER_NODE_WALKER(NULL)) && /* not unregistering */
      (self->master != MASTER_NODE_WALKER(NULL))) {
    /* unregister from the previous master, but only if it is not
       unregistering */
    MasterNodeWalker_unregister_walker(self->master, self->name);
  }

  self->master = master;
}

boolean node_walker_can_handle_null_node(const NodeWalker_ptr self)
{ return self->can_handle_null; }


node_ptr node_walker_run_transformation_chain(const NodeWalker_ptr self,
                                              node_ptr node)
{
  if (NULL != self->node_transformations) {
    NodeTransformation* ntp;
    int nt_idx;
    arrayForEachItemP(NodeTransformation, self->node_transformations,
                      nt_idx, ntp) {
      node = ntp->func(self, node, ntp->arg);
    }
  }

  return node;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The NodeWalker class virtual finalizer

  Called by the class destructor
*/
static void node_walker_finalize(Object_ptr object, void* dummy)
{
  NodeWalker_ptr self = NODE_WALKER(object);

  node_walker_deinit(self);
  FREE(self);
}


/**AutomaticEnd***************************************************************/
