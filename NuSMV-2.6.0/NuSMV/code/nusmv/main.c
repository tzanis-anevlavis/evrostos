/* ---------------------------------------------------------------------------


  This file is part of the ``sm'' package of NuSMV version 2.
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

/*!
  \author Adapted to NuSMV by Marco Roveri
  \brief Main NuSMV routine. Parses command line at invocation of NuSMV.

  \todo: Missing description

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/cinit/cinit.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/shell/cinit/cinit.h"
#include "nusmv/core/cinit/cinitInt.h"
#include "nusmv/addons_core/addonsCore.h"
#include "nusmv/addons_core/compass/compassCmd.h"

/*---------------------------------------------------------------------------*/
/* Macro definitions                                                         */
/*---------------------------------------------------------------------------*/
#ifndef  NUSMV_PACKAGE_BUGREPORT

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define  NUSMV_PACKAGE_BUGREPORT "nusmv-users@list.fbk.eu"
#endif


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void main_init_custom_data(void);
static void main_init_custom_cmd_options(void);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief required

  optional

  \se required

  \sa optional
*/

int main(int  argc, char ** argv)
{
  int status;
  boolean requires_shutdown = true;
  NuSMVEnv_ptr env;

  FP_V_E iq_fns[][2] = {
    /* these are for the core */
    {AddonsCore_Init, AddonsCore_Quit},
#if NUSMV_HAVE_INTERACTIVE_SHELL
    /* these are for the interactive shell */
    {CInit_init_cmd, CInit_quit_cmd},
    {Compass_init_cmd, Compass_Cmd_quit},
#endif
  };

  /* Initializes data such as tool name, tool version, email.. */
  NuSMVCore_init_data();

  /* Customize the library data. */
  main_init_custom_data();

  env = NuSMVEnv_create();

  /* Initializes all packages, having the list of init/quit mfunctions */
  NuSMVCore_init(env, iq_fns, sizeof(iq_fns)/sizeof(iq_fns[0]));

  /* Adds the command line options of NuSMV */
  NuSMVCore_init_cmd_options(env);

  /* Add [or remove] custom command line options */
  main_init_custom_cmd_options();

  /* Finally, call the main function */
  requires_shutdown = NuSMVCore_main(env, argc, argv, &status);

  if (requires_shutdown) {
    NuSMVCore_quit(env);
  }

  NuSMVEnv_destroy(env);

  return status;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void main_init_custom_data(void)
{
  /* Empty stub */

  /* Example: */
  /* NuSMVCore_set_tool_name("esmc"); */
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void main_init_custom_cmd_options(void)
{
  /* Empty stub */

  /* Example: */
  /* NuSMVCore_add_env_command_line_option(...) */
}
