/* ---------------------------------------------------------------------------


  This file is part of the ``node.normalizers'' package of NuSMV version 2.
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
  \author Mariotti Alessandro
  \brief Implementaion of class 'NormalizerBase'

  \todo: Missing description

*/


#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/normalizers/NormalizerBase.h"
#include "nusmv/core/node/normalizers/NormalizerBase_private.h"

#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#include "nusmv/core/node/normalizers/MasterNormalizer_private.h"

#include "nusmv/core/node/MasterNodeWalker.h"
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
/* See 'NormalizerBase_private.h' for class 'NormalizerBase' definition. */

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

static void normalizer_base_finalize(Object_ptr object, void* dummy);

static node_ptr
normalizer_base_normalize_node(NormalizerBase_ptr self, node_ptr n);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

NormalizerBase_ptr
NormalizerBase_create(const NuSMVEnv_ptr env, const char* name, int low, size_t num)
{
  NormalizerBase_ptr self = ALLOC(NormalizerBase, 1);
  NORMALIZER_BASE_CHECK_INSTANCE(self);

  normalizer_base_init(self, env, name, low, num, true);
  return self;
}

VIRTUAL node_ptr
NormalizerBase_normalize_node(NormalizerBase_ptr self, node_ptr n)
{
  node_ptr res;
  MasterNormalizer_ptr master;
  NORMALIZER_BASE_CHECK_INSTANCE(self);

  master = MASTER_NORMALIZER(NODE_WALKER(self)->master);

  /* Lookup in the cache, return cached data if found */
  res = MasterNormalizer_lookup_cache(master, n);
  if (Nil != res) return res;

  res = self->normalize_node(self, n);

  MasterNormalizer_insert_cache(master, n, res);

  return res;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void normalizer_base_init(NormalizerBase_ptr self, const NuSMVEnv_ptr env,
                          const char* name,
                          int low, size_t num, boolean can_handle_null)
{
  /* base class initialization */
  node_walker_init(NODE_WALKER(self), env, name, low, num, can_handle_null);
  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = normalizer_base_finalize;
  OVERRIDE(NormalizerBase, normalize_node) = normalizer_base_normalize_node;
}

void normalizer_base_deinit(NormalizerBase_ptr self)
{
  /* members deinitialization */

  /* base class initialization */
  node_walker_deinit(NODE_WALKER(self));
}

node_ptr normalizer_base_throw_normalize_node(NormalizerBase_ptr self, node_ptr n)
{
  if (NodeWalker_can_handle(NODE_WALKER(self), n)) {
    /* checks if self can handle the node without need of re-throw
       to the master */
    return NormalizerBase_normalize_node(self, n);
  }
  return master_normalizer_normalize_node(
           MASTER_NORMALIZER(NODE_WALKER(self)->master), n);
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The NormalizerBase class virtual finalizer

  Called by the class destructor
*/
static void normalizer_base_finalize(Object_ptr object, void* dummy)
{
  NormalizerBase_ptr self = NORMALIZER_BASE(object);

  normalizer_base_deinit(self);
  FREE(self);
}

/*!
  \brief Virtual menthod that prints the given node

  This is a pure virtual method, to be implemented by derived
  class, and cannot be called
*/
static node_ptr
normalizer_base_normalize_node(NormalizerBase_ptr self, node_ptr n)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  ErrorMgr_internal_error(errmgr, "NormalizerBase: Pure virtual method normalize_node "  \
                 "not implemented\n");
  return Nil;
}

/**AutomaticEnd***************************************************************/
