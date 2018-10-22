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
  \author Alessandro Mariotti
  \brief Implementaion of class 'MasterNormalizer', derived from
  MasterNodeWalker

  \todo: Missing description

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#include "nusmv/core/node/normalizers/MasterNormalizer_private.h"

#include "nusmv/core/node/MasterNodeWalker_private.h"

#include "nusmv/core/node/normalizers/NormalizerBase.h"
#include "nusmv/core/node/normalizers/NormalizerBase_private.h"

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

typedef struct MasterNormalizer_TAG
{
  /* this MUST stay on the top */
  INHERITS_FROM(MasterNodeWalker);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */

  hash_ptr cache;

} MasterNormalizer;

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

static void master_normalizer_init(MasterNormalizer_ptr self,
                                   const NuSMVEnv_ptr env);
static void master_normalizer_deinit(MasterNormalizer_ptr self);
static void master_normalizer_finalize(Object_ptr object, void* dummy);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

MasterNormalizer_ptr MasterNormalizer_create(const NuSMVEnv_ptr env)
{
  MasterNormalizer_ptr self = ALLOC(MasterNormalizer, 1);
  MASTER_NORMALIZER_CHECK_INSTANCE(self);

  master_normalizer_init(self, env);
  return self;
}

node_ptr MasterNormalizer_normalize_node(MasterNormalizer_ptr self, node_ptr n)
{
  node_ptr res;
  MASTER_NORMALIZER_CHECK_INSTANCE(self);
  res = master_normalizer_normalize_node(self, n);

  /* Clear the memoization cache at each call. See issue #1960 for
     further details.  A possible scenario that demonstrates that the
     cache has to be cleared is the following:

       - A node n is created with new_node.

       - MasterNormalizer_normalize_node(normalizer, n) is called and memoization is updated with
         the pointer to n

       - n is released with free_node

       - a new node n' is created with new_node and n' has the same
         address of n, even if it is different.

       - MasterNormalizer_normalize_node(normalizer, n') is called, but using the memoization we
         obtain a wrong result.
  */

  clear_assoc(self->cache);

  return res;
}

node_ptr MasterNormalizer_lookup_cache(MasterNormalizer_ptr self, node_ptr n)
{
  MASTER_NORMALIZER_CHECK_INSTANCE(self);
  return find_assoc(self->cache, n);
}

void MasterNormalizer_insert_cache(MasterNormalizer_ptr self, node_ptr n,
                                   node_ptr norm)
{
  MASTER_NORMALIZER_CHECK_INSTANCE(self);
  insert_assoc(self->cache, n, norm);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

node_ptr master_normalizer_normalize_node(MasterNormalizer_ptr self,
                                          node_ptr n)
{
  ListIter_ptr iter;
  iter = NodeList_get_first_iter(MASTER_NODE_WALKER(self)->walkers);
  while (!ListIter_is_end(iter)) {
    NormalizerBase_ptr pr =
      NORMALIZER_BASE(NodeList_get_elem_at(MASTER_NODE_WALKER(self)->walkers,
                                           iter));

    if (NodeWalker_can_handle(NODE_WALKER(pr), n)) {

      return NormalizerBase_normalize_node(pr, n);
    }

    iter = ListIter_get_next(iter);
  }

  return Nil;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The MasterNormalizer class private initializer

  The MasterNormalizer class private initializer

  \sa MasterNormalizer_create
*/
static void master_normalizer_init(MasterNormalizer_ptr self,
                                   const NuSMVEnv_ptr env)
{
  /* base class initialization */
  master_node_walker_init(MASTER_NODE_WALKER(self), env);

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = master_normalizer_finalize;

  self->cache = new_assoc();
}

/*!
  \brief The MasterNormalizer class private deinitializer

  The MasterNormalizer class private deinitializer

  \sa Object_destroy
*/
static void master_normalizer_deinit(MasterNormalizer_ptr self)
{
  /* base class deinitialization */
  master_node_walker_deinit(MASTER_NODE_WALKER(self));

  free_assoc(self->cache);
}

/*!
  \brief The NormalizerBase class virtual finalizer

  Called by the class destructor
*/
static void master_normalizer_finalize(Object_ptr object, void* dummy)
{
  MasterNormalizer_ptr self = MASTER_NORMALIZER(object);

  master_normalizer_deinit(self);
  FREE(self);
}

/**AutomaticEnd***************************************************************/
