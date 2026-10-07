/* ---------------------------------------------------------------------------


  This file is part of the ``'' package.
  %COPYRIGHT%


-----------------------------------------------------------------------------*/

/*!
  \author Michele Dorigatti
  \brief \todo: Missing synopsis

  \todo: Missing description

*/

#if HAVE_CONFIG_H
#include "nusmv-config.h"
#endif

#include "nusmv/shell/cmd/cmdCmd.h"
#include "nusmv/shell/cinit/cinit.h"
#include "nusmv/shell/parser/parserCmd.h"
#include "nusmv/shell/enc/encCmd.h"
#include "nusmv/shell/hrc/hrcCmd.h"
#include "nusmv/shell/simulate/simulateCmd.h"
#include "nusmv/shell/prop/propCmd.h"
#include "nusmv/shell/compile/compileCmd.h"
#include "nusmv/shell/mc/mcCmd.h"
#include "nusmv/shell/cinit/cinitCmd.h"
#include "nusmv/shell/bmc/bmcCmd.h"
#include "nusmv/shell/bmc/sbmc/sbmcCmd.h"
#include "nusmv/shell/utils/utilsCmd.h"
#include "nusmv/shell/trace/traceCmd.h"
#include "nusmv/shell/opt/optCmd.h"
#include "nusmv/shell/dd/ddCmd.h"
#include "nusmv/shell/ltl/ltlCmd.h"
#include "nusmv/shell/fsm/bdd/bddCmd.h"
#include "nusmv/shell/opt/optCmd.h"

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

void CInit_init_cmd(NuSMVEnv_ptr env)
{
  Cmd_init_cmd(env);
  Parser_Cmd_init(env);
  Enc_add_commands(env);
  Hrc_init_cmd(env);
  Simulate_Cmd_init(env);
  PropPkg_init_cmd(env);
  Compile_init_cmd(env);
  Mc_Init(env);
  cinit_AddCmd(env);

#if NUSMV_HAVE_SAT_SOLVER
  Bmc_AddCmd(env);
#endif

  Utils_init_cmd(env);
  traceCmd_init(env);
  Opt_Cmd_init(env);
  dd_AddCmd(env);

  Bdd_Init(env);
  Ltl_Init(env);

  Opt_Cmd_init(env);
}

void CInit_quit_cmd(NuSMVEnv_ptr env)
{
  Cmd_quit_cmd(env);
  Utils_quit_cmd(env);
  Hrc_quit_cmd(env);
  PropPkg_quit_cmd(env);
  Simulate_Cmd_quit(env);

#if NUSMV_HAVE_SAT_SOLVER
  Bmc_Cmd_quit(env);
#endif

  Bdd_End(env); /* does nothing */
  Mc_End(env); /* does nothing */
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
