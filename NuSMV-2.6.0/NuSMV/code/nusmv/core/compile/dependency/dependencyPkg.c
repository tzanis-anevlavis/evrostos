/* ---------------------------------------------------------------------------



  This file is part of the ``hycomp.dependency'' package of NuSMV version 2.
  Copyright (C) 2013 by FBK.

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
  \brief Package function of the compile.dependency package

  \todo: Missing description

*/


#include "nusmv/core/compile/dependency/dependencyPkg.h"
#include "nusmv/core/compile/dependency/FormulaDependency.h"
#include "nusmv/core/compile/dependency/DependencyBase.h"
#include "nusmv/core/compile/dependency/DependencyCore.h"
#include "nusmv/core/compile/dependency/DependencyPsl.h"

#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/cinit/NuSMVEnv.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/opt/opt.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


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

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief Initialization of the dependency package.

  Initialization of the dependency package.

  It associates a formula dependency to the global symbol table and
  the global environment.
*/

void DependencyPkg_init(NuSMVEnv_ptr env)
{
  FormulaDependency_ptr dependency;
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (opt_verbose_level_gt(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
               "Instantiating the FormulaDependency instance "
               "within the given environment...\n");
  }

  dependency = FormulaDependency_create(env);
  {
    /* add core handlers */
    DependencyBase_ptr core_dep;
    DependencyBase_ptr psl_dep;
    core_dep = DEPENDENCY_BASE(DependencyCore_create(env, "Dependency core"));
    MasterNodeWalker_register_walker(MASTER_NODE_WALKER(dependency),
                                     NODE_WALKER(core_dep));

    psl_dep = DEPENDENCY_BASE(DependencyPsl_create(env, "Dependency psl"));
    MasterNodeWalker_register_walker(MASTER_NODE_WALKER(dependency),
                                     NODE_WALKER(psl_dep));

  }

  NuSMVEnv_set_value(env, ENV_DEPENDENCY, dependency);
}

/*!
  \brief Deinitialization of the dependency package

  
*/

void DependencyPkg_quit(NuSMVEnv_ptr env)
{
  FormulaDependency_ptr dependency =
    FORMULA_DEPENDENCY(NuSMVEnv_remove_value(env, ENV_DEPENDENCY));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (opt_verbose_level_gt(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
               "Clearing the formula dependency instance in "
               "the given environment...\n");
  }

  MasterNodeWalker_destroy(MASTER_NODE_WALKER(dependency));
}



/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/



