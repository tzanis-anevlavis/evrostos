/* ---------------------------------------------------------------------------



  This file is part of the ``compile.flattening'' package of NuSMV version 2.
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
  \brief Package function of the compile.flattening package

  \todo: Missing description

*/


#include "nusmv/core/compile/flattening/flatteningPkg.h"
#include "nusmv/core/compile/flattening/MasterCompileFlattener.h"
#include "nusmv/core/compile/flattening/FlattenerCore.h"

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
  \brief Initialization of the flattening package.

  Initialization of the flattening package.

  It associates a flattener to the global environment.
*/

void FlatteningPkg_init(NuSMVEnv_ptr env)
{
  MasterCompileFlattener_ptr flattener;
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (opt_verbose_level_gt(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
               "Instantiating the Flattener instance "
               "within the given environment...\n");
  }

  flattener = MasterCompileFlattener_create(env);
  {
    /* add core flatteners */
    FlattenerBase_ptr core_flattener;
    core_flattener = FLATTENER_BASE(FlattenerCore_create(env, "Flattener core"));
    MasterNodeWalker_register_walker(MASTER_NODE_WALKER(flattener),
                                     NODE_WALKER(core_flattener));

  }

  NuSMVEnv_set_value(env, ENV_FLATTENER, flattener);
}

/*!
  \brief Deinitialization of the flattening package

  
*/

void FlatteningPkg_quit(NuSMVEnv_ptr env)
{
  MasterCompileFlattener_ptr flattener =
    MASTER_COMPILE_FLATTENER(NuSMVEnv_remove_value(env, ENV_FLATTENER));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (opt_verbose_level_gt(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
               "Clearing the Flattener instance in "
               "the given environment...\n");
  }

  MasterCompileFlattener_destroy(flattener);
}



/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/



