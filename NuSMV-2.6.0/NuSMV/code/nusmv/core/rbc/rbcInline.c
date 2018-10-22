/* ---------------------------------------------------------------------------


  This file is part of the ``rbc'' package of NuSMV version 2.
  Copyright (C) 2007 by FBK-irst.

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
  \brief Implementaion of RBC inlining

  \todo: Missing description

*/


#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/rbc/rbcInt.h"
#include "nusmv/core/rbc/InlineResult.h"

#include "nusmv/core/dag/dag.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/parser/symbols.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/
/* Size of the LRU cache for inlining, value seems to be fitting well
 from a few experiments */
#define RBC_INLINE_CACHE_THREASHOLD   \
  1031

/* Use to enable/disable caching of inline functions */
#define RBC_ENABLE_INLINING_CACHE \
  1

/*---------------------------------------------------------------------------*/
/* Stucture declarations                                                     */
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
/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void
rbc_inlining_cache_add_result(Rbc_Manager_t*, Rbc_t* f, InlineResult_ptr res);

/*---------------------------------------------------------------------------*/
/* Definition of external functions                                          */
/*---------------------------------------------------------------------------*/

InlineResult_ptr RbcInline_apply_inlining(Rbc_Manager_t* rbcm, Rbc_t* f)
{
  const NuSMVEnv_ptr env = Rbc_ManagerGetEnvironment(rbcm);
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  InlineResult_ptr ir;
#if RBC_ENABLE_INLINING_CACHE
  ir = rbc_inlining_cache_lookup_result(rbcm, f);
  if (NULL != ir) {
    return InlineResult_ref(ir);
  }
#endif

  if (opt_verbose_level_gt(opts, 2)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Rbc: starting inlining ... \n");
  }

  ir = InlineResult_create(rbcm, f);

#if RBC_ENABLE_INLINING_CACHE
  /* ir gets referenced by this function */
  rbc_inlining_cache_add_result(rbcm, f, ir); /* caches result */
#endif

  if (opt_verbose_level_gt(opts, 2)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "RBC: end of inlining\n");
  }

  return ir;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief Inline caching private service for content destruction
*/
static void _destroy_cache_entry(void* key, void* _elem, void* arg)
{
  if (NULL != _elem) {
    InlineResult_destroy(INLINE_RESULT(_elem));
  }
}

/*!
  \brief Inline caching initialization
*/
void rbc_inlining_cache_init(Rbc_Manager_t* rbcm)
{
  nusmv_assert((Rbc_Manager_t*) NULL != rbcm);
  nusmv_assert(NULL == rbcm->inlining_cache);

  rbcm->inlining_cache = LRUCache_create(
      RBC_INLINE_CACHE_THREASHOLD,
      OAHash_pointer_eq_fun,
      OAHash_pointer_hash_fun,
      _destroy_cache_entry, NULL);
}

/*!
  \brief Inline caching deinitialization
*/
void rbc_inlining_cache_quit(Rbc_Manager_t* rbcm)
{
  nusmv_assert((Rbc_Manager_t*) NULL != rbcm);

  LRUCache_destroy(rbcm->inlining_cache);
  rbcm->inlining_cache = NULL;
}

InlineResult_ptr
rbc_inlining_cache_lookup_result(Rbc_Manager_t* rbcm, Rbc_t* f)
{
  nusmv_assert((Rbc_Manager_t*) NULL != rbcm);
  return INLINE_RESULT(LRUCache_lookup(rbcm->inlining_cache, f));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Inline caching private service to add an entry

  The given InlineResult gets referenced before being stored into the
  cache, caller keeps its ownership
*/
static void
rbc_inlining_cache_add_result(Rbc_Manager_t* rbcm, Rbc_t* f,
                              InlineResult_ptr res)
{
  InlineResult_ptr old;
  nusmv_assert((Rbc_Manager_t*) NULL != rbcm);

  old = rbc_inlining_cache_lookup_result(rbcm, f);
  if (res == old) {
    /* alread cached */
    return;
  }

  if (NULL != old) {
    /* clear the old association */
    InlineResult_destroy(old);
  }

  /* since we cache, the stored InlineResult gets referenced to
   * protect against multiple destruction */
  LRUCache_insert(rbcm->inlining_cache, f, InlineResult_ref(res));
}
