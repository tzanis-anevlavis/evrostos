/* ---------------------------------------------------------------------------


  This file is part of the ``compile.dependency'' package of NuSMV version 2.
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
  \brief Implementation of class 'DependencyBase'

  Base (abstract) class that defines a dependency for a
  contiguous interval of nodes.

*/


#include "nusmv/core/compile/dependency/DependencyBase.h"
#include "nusmv/core/compile/dependency/DependencyBase_private.h"

#include "nusmv/core/compile/dependency/FormulaDependency.h"
#include "nusmv/core/compile/dependency/FormulaDependency_private.h"

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
/* See 'DependencyBase_private.h' for class 'DependencyBase' definition. */

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

static void dependency_base_finalize(Object_ptr object, void* dummy);

static Set_t
dependency_base_get_dependencies(DependencyBase_ptr self,
                                 SymbTable_ptr symb_table,
                                 node_ptr formula, node_ptr context,
                                 SymbFilterType filter,
                                 boolean preserve_time, int time,
                                 hash_ptr dependencies_hash);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

DependencyBase_ptr DependencyBase_create(const NuSMVEnv_ptr env, const char* name,
                                         int low, size_t num)
{
  DependencyBase_ptr self = ALLOC(DependencyBase, 1);
  DEPENDENCY_BASE_CHECK_INSTANCE(self);

  dependency_base_init(self, env, name, low, num, false);
  return self;
}

VIRTUAL Set_t DependencyBase_get_dependencies(DependencyBase_ptr self,
                                              SymbTable_ptr symb_table,
                                              node_ptr formula, node_ptr context,
                                              SymbFilterType filter,
                                              boolean preserve_time, int time,
                                              hash_ptr dependencies_hash)
{
  DEPENDENCY_BASE_CHECK_INSTANCE(self);

  return self->get_dependencies(self, symb_table, formula, context, filter,
                                preserve_time, time, dependencies_hash);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void dependency_base_init(DependencyBase_ptr self, const NuSMVEnv_ptr env,
                          const char* name, int low, size_t num,
                          boolean can_handle_null)
{
  /* base class initialization */
  node_walker_init(NODE_WALKER(self), env, name, low, num, can_handle_null);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = dependency_base_finalize;
  OVERRIDE(DependencyBase, get_dependencies) = dependency_base_get_dependencies;
}

void dependency_base_deinit(DependencyBase_ptr self)
{
  /* members deinitialization */

  /* base class deinitialization */
  node_walker_deinit(NODE_WALKER(self));
}

Set_t dependency_base_throw_get_dependencies(DependencyBase_ptr self,
                                             SymbTable_ptr symb_table,
                                             node_ptr formula, node_ptr context,
                                             SymbFilterType filter,
                                             boolean preserve_time, int time,
                                             hash_ptr dependencies_hash)
{
  if (NodeWalker_can_handle(NODE_WALKER(self), formula)) {
    {
      Set_t result;
      Tuple5 key;

      /* Check if dependencies for formula have been computed before */
      formula_dependency_mk_hash_key(formula, context, filter, preserve_time, time,
                                     &key);
      result = formula_dependency_lookup_hash(dependencies_hash, NODE_PTR(&key));

      if (result == EMPTY_DEP_SET) {
        return Set_MakeEmpty();
      }

      if (result != (Set_t)NULL) {
        return Set_Copy(result);
      }
    }

    /* checks if self can handle the node without need of re-throw
       to the master */
    return DependencyBase_get_dependencies(self, symb_table,
                                           formula, context, filter,
                                           preserve_time, time,
                                           dependencies_hash);
  }

  return formula_dependency_get_dependencies(FORMULA_DEPENDENCY(NODE_WALKER(self)->master),
                                             symb_table,
                                             formula, context, filter,
                                             preserve_time, time,
                                             dependencies_hash);
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The DependencyBase class virtual finalizer

  Called by the class destructor
*/
static void dependency_base_finalize(Object_ptr object, void* dummy)
{
  DependencyBase_ptr self = DEPENDENCY_BASE(object);

  dependency_base_deinit(self);
  FREE(self);
}

/*!
  \brief Get the dependencies of formula

  
*/
static Set_t dependency_base_get_dependencies(DependencyBase_ptr self,
                                              SymbTable_ptr symb_table,
                                              node_ptr formula, node_ptr context,
                                              SymbFilterType filter,
                                              boolean preserve_time, int time,
                                              hash_ptr dependencies_hash)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  ErrorMgr_internal_error(errmgr, "DependencyBase: Pure virtual method " \
                          "get_dependencies not implemented\n");
  return SET_T(NULL);
}

/**AutomaticEnd***************************************************************/

