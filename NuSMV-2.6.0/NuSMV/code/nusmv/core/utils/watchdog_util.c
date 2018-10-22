/* ---------------------------------------------------------------------------

  This file is part of the ``utils'' package of NuSMV version 2.
  Copyright (C) 1998-2001 by CMU and FBK-irst.

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

#include "nusmv/core/utils/watchdog_util.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/utils/defs.h"

/*!
  \author Sergio Mover
  \brief The file contains the functions used to wrap the call to a
  watchdog library.
*/

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


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
void watchdog_action(void* inputenv)
{
  NuSMVEnv_ptr env = NUSMV_ENV(inputenv);
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env,
                                                        ENV_STREAM_MANAGER));
  ErrorMgr_ptr errmgr = ERROR_MGR(NuSMVEnv_get_value(env,
                                                      ENV_ERROR_MANAGER));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env,
                                                         ENV_OPTS_HANDLER));

  StreamMgr_print_output(streams, "Watchdog Timer: reached timeout\n");
  if (! opt_batch(opts)) {
    StreamMgr_print_error(streams,
                          "Warning: %s status may not be consistent."   \
                          "Use 'reset' command if needed.\n",
                          get_pgm_name(opts));
  }
  ErrorMgr_long_jmp(errmgr);
}
