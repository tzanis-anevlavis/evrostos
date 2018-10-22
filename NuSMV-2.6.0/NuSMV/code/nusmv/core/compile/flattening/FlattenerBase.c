/* ---------------------------------------------------------------------------


  This file is part of the ``compile.flattening'' package of NuSMV version 2.
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
  \author Sergio Mover
  \brief Implementation of class 'FlattenerBase'

  Base (abstract) class that defines a flattener for a
  contiguous interval of nodes.

*/


#include "nusmv/core/compile/flattening/FlattenerBase.h"
#include "nusmv/core/compile/flattening/FlattenerBase_private.h"

#include "nusmv/core/compile/flattening/MasterCompileFlattener.h"
#include "nusmv/core/compile/flattening/MasterCompileFlattener_private.h"

#include "nusmv/core/node/MasterNodeWalker.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/ErrorMgr.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'FlattenerBase_private.h' for class 'FlattenerBase' definition. */

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

static void flattener_base_finalize(Object_ptr object, void* dummy);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

FlattenerBase_ptr FlattenerBase_create(const NuSMVEnv_ptr env, const char* name,
                                       int low, size_t num)
{
  FlattenerBase_ptr self = ALLOC(FlattenerBase, 1);
  FLATTENER_BASE_CHECK_INSTANCE(self);

  flattener_base_init(self, env, name, low, num, false);
  return self;
}

VIRTUAL node_ptr FlattenerBase_flatten(FlattenerBase_ptr self,
                                       SymbTable_ptr symb_table,
                                       hash_ptr def_hash,
                                       node_ptr sexp,
                                       node_ptr context,
                                       MasterCompileFlattener_def_mode mode)
{
  FLATTENER_BASE_CHECK_INSTANCE(self);

  return self->flatten(self, symb_table, def_hash, sexp, context, mode);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void flattener_base_init(FlattenerBase_ptr self, const NuSMVEnv_ptr env,
                         const char* name, int low, size_t num,
                         boolean can_handle_null)
{
  /* base class initialization */
  node_walker_init(NODE_WALKER(self), env, name, low, num, can_handle_null);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = flattener_base_finalize;
  OVERRIDE(FlattenerBase, flatten) = flattener_base_flatten;
}

void flattener_base_deinit(FlattenerBase_ptr self)
{
  /* members deinitialization */

  /* base class deinitialization */
  node_walker_deinit(NODE_WALKER(self));
}

node_ptr flattener_base_flatten(FlattenerBase_ptr self,
                                SymbTable_ptr symb_table,
                                hash_ptr def_hash,
                                node_ptr sexp,
                                node_ptr context,
                                MasterCompileFlattener_def_mode mode)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  ErrorMgr_internal_error(errmgr, "FlattenerBase: Pure virtual method flatten " \
                 "not implemented\n");
  return 0;
}

node_ptr flattener_base_throw_flatten(FlattenerBase_ptr self,
                                      SymbTable_ptr symb_table,
                                      hash_ptr def_hash,
                                      node_ptr sexp,
                                      node_ptr context,
                                      MasterCompileFlattener_def_mode mode)
{
  if (NodeWalker_can_handle(NODE_WALKER(self), sexp)) {
    /* checks if self can handle the node without need of re-throw
       to the master */
    return FlattenerBase_flatten(self, symb_table, def_hash, sexp, context, mode);
  }
  return master_compile_flattener_flatten(MASTER_COMPILE_FLATTENER(NODE_WALKER(self)->master),
                                          symb_table,
                                          def_hash,
                                          sexp,
                                          context,
                                          mode);
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The FlattenerBase class virtual finalizer

  Called by the class destructor
*/
static void flattener_base_finalize(Object_ptr object, void* dummy)
{
  FlattenerBase_ptr self = FLATTENER_BASE(object);

  flattener_base_deinit(self);
  FREE(self);
}



/**AutomaticEnd***************************************************************/

