/* ---------------------------------------------------------------------------


  This file is part of the ``cinit'' package of NuSMV version 2.
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
  \brief Interface of the cinit package with the shell.

  Interface of the cinit package with the shell.

*/

#include "nusmv/shell/cmd/cmd.h"
#include "nusmv/shell/cinit/cinitCmd.h"

#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/cinit/cinit.h"

#include "nusmv/core/utils/error.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/compile/compile.h"

int CommandCmdReset(NuSMVEnv_ptr env, int argc, char** argv);
int CommandPrintUsage(NuSMVEnv_ptr env, int argc, char** argv);
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
static int UsageCmdReset(const NuSMVEnv_ptr env);
static int UsagePrintUsage(const NuSMVEnv_ptr env);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/
void cinit_AddCmd(NuSMVEnv_ptr env)
{
  Cmd_CommandAdd(env, "reset", CommandCmdReset, 0, false);
  Cmd_CommandAdd(env, "print_usage", CommandPrintUsage, 0, true);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \command{reset} Resets the whole system.

  \command_args{[-h]}

  Resets the whole system, in order to read in
  another model and to perform verification on it.
  <p>
  Command options:<p>
  <dl>
    <dt> -h
       <dd> Prints the command usage.
  </dl>
*/

int CommandCmdReset(NuSMVEnv_ptr env, int argc, char** argv)
{
  int c;

  util_getopt_reset();
  while((c = util_getopt(argc,argv,"h")) != EOF){
    switch(c){
    case 'h': return(UsageCmdReset(env));
    default:  return(UsageCmdReset(env));
    }
  }
  if (argc != util_optind) return(UsageCmdReset(env));

  NuSMVCore_reset(env);

  return 0;
}

/*!
  \brief


*/
static int UsageCmdReset(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: reset [-h]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  return(1);
}


/*!
  \command{print_usage} Prints processor and BDD statistics.

  \command_args{[-h]}

  Prints a formatted dump of processor-specific
  usage statistics, and BDD usage statistics. For Berkeley Unix, this
  includes all of the information in the <tt>getrusage()</tt> structure.
  <p>
  Command options:<p>
  <dl>
    <dt> -h
       <dd> Prints the command usage.
  </dl>

*/

int CommandPrintUsage(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  OStream_ptr outstream = StreamMgr_get_output_ostream(streams);
  int c;
  int retval = 0;

  util_getopt_reset();
  while((c = util_getopt(argc,argv,"h")) != EOF){
    switch(c) {
    case 'h': return(UsagePrintUsage(env));
    default:  return(UsagePrintUsage(env));
    }
  }

  retval = Compile_print_usage(env, outstream);

  return retval;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsagePrintUsage(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: print_usage [-h]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  return(1);
}
