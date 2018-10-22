/* ---------------------------------------------------------------------------


  This file is part of the ``utils'' package.
  Copyright (C) 2012 by FBK.

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
  \author Michele Dorigatti
  \brief Shell interface for the utils package

  This file contains the interface of the utils package
  with the interactive shell.

*/

#include "nusmv/shell/cmd/cmd.h"
#include "nusmv/shell/utils/utilsCmd.h"

#include "nusmv/core/utils/StreamMgr.h"

#ifndef NDEBUG
#include "nusmv/core/utils/Sset.h"
#include "nusmv/core/utils/NodeList.h"
#endif

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

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
#ifndef NDEBUG
static int UsageUtilsTestSset(const NuSMVEnv_ptr env);
static int UsageUtilsTestNodeList(const NuSMVEnv_ptr env);
static int CommandUtilsTestNodeList(NuSMVEnv_ptr env, int argc, char** argv);
#endif

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Utils_init_cmd(NuSMVEnv_ptr env)
{
#ifndef NDEBUG
  Cmd_CommandAdd(env, "_utils_test_sset", CommandUtilsTestSset, 0, false);
  Cmd_CommandAdd(env, "_utils_test_nodelist", CommandUtilsTestNodeList, 0, false);
#endif
}

void Utils_quit_cmd(NuSMVEnv_ptr env)
{
#ifndef NDEBUG
  Cmd_CommandRemove(env, "_utils_test_sset");
  Cmd_CommandRemove(env, "_utils_test_nodelist");
#endif
}

/*!
  \command{optional} optional

  \command_args{optional}

  optional
*/

#ifndef NDEBUG
int CommandUtilsTestSset(NuSMVEnv_ptr env, int argc, char** argv)
{
  int c = 0;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "h")) != EOF) {
    switch (c) {
    case 'h': return UsageUtilsTestSset(env);
    default:
      break;
    }
  }

  Sset_test(env);

  return 0;
}
#endif

/*!
  \command{optional} optional

  \command_args{optional}

  optional
*/

#ifndef NDEBUG
static int CommandUtilsTestNodeList(NuSMVEnv_ptr env, int argc, char** argv)
{
  int c = 0;
  int retval = 1;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "h")) != EOF) {
    switch (c) {
    case 'h': return UsageUtilsTestNodeList(env);
    default:
      break;
    }
  }

  retval = NodeList_test(env);

  return retval;
}
#endif

int Utils_check_non_option_args(NuSMVEnv_ptr env,
                                int argc,
                                unsigned int expected_args,
                                int (*usage)(void* arg),
                                void* arg)
{
  StreamMgr_ptr const streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  nusmv_assert(NULL == usage || NULL != arg);

  if (argc != (util_optind + expected_args)) {
    StreamMgr_print_error(streams,
                          "Error: wrong number of non option arguments; "
                          "given %d but expected %d.\n",
                          argc - util_optind,
                          expected_args);

    if (NULL != usage) return (*usage)(arg);
    else return 1;
  }

  return 0;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
#ifndef NDEBUG

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageUtilsTestSset(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: _utils_test_sset [-h]\n");

  return 1;
}
#endif

#ifndef NDEBUG

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageUtilsTestNodeList(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: _utils_test_nodelist [-h]\n");

  return 1;
}
#endif
