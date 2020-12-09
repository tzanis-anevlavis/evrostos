/* ---------------------------------------------------------------------------


  This file is part of the ``bmc'' package of NuSMV version 2.
  Copyright (C) 2004 Timo Latvala <timo.latvala@tkk.fi>

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

  For more information of NuSMV see <http://nusmv.fbk.eu>
  or email to <nusmv-users@fbk.eu>.
  Please report bugs to <nusmv-users@fbk.eu>.

  To contact the NuSMV development board, email to <nusmv@fbk.eu>. 

-----------------------------------------------------------------------------*/

/*!
  \author Timo Latvala, Marco Roveri
  \brief Bmc.Gen module

  This module contains all the problems generation functions

*/


#include "nusmv/core/bmc/sbmc/sbmcGen.h"
#include "nusmv/core/bmc/sbmc/sbmcTableau.h"

#include "nusmv/core/bmc/bmcInt.h"
#include "nusmv/core/bmc/bmcModel.h"
#include "nusmv/core/bmc/bmcUtils.h"

#include "nusmv/core/utils/ErrorMgr.h"

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

be_ptr Bmc_Gen_SBMCProblem(const BeFsm_ptr be_fsm,
                          const node_ptr ltl_wff,
                          const int k, const int l)
{
  Be_Manager_ptr be_mgr = BeEnc_get_be_manager(BeFsm_get_be_encoding(be_fsm));
  be_ptr res = NULL;
  be_ptr path_k = Bmc_Model_GetPathWithInit(be_fsm, k);

  if (Bmc_Utils_IsAllLoopbacks(l)) {
    /* Generates the problem with all possible loopbacks: */
    be_ptr tableau_loops = NULL;

    tableau_loops = Bmc_SBMCTableau_GetAllLoops(be_fsm, ltl_wff, k, l);
    res = Be_And( be_mgr, path_k, tableau_loops);
  }
  else if (Bmc_Utils_IsNoLoopback(l)) {
    /* Generates the problem with no loopback: */
    be_ptr tableau = Bmc_SBMCTableau_GetNoLoop(be_fsm, ltl_wff, k);
    res =  Be_And(be_mgr, path_k, tableau);
  }
  else {
    /* one loopback: */
    be_ptr tableau_loopback = NULL;

    nusmv_assert(Bmc_Utils_IsSingleLoopback(l)); /* no other choices */

    tableau_loopback = Bmc_SBMCTableau_GetSingleLoop(be_fsm, ltl_wff, k, l);
    res = Be_And(be_mgr, path_k, tableau_loopback);
  }

  return res;
}

int Sbmc_Gen_check_psl_property(NuSMVEnv_ptr env,
                                Prop_ptr prop,
                                boolean dump_prob,
                                boolean inc_sat,
                                boolean do_completeness_check,
                                boolean do_virtual_unrolling,
                                boolean is_single_prob,
                                int k,
                                int rel_loop)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  nusmv_assert(prop != PROP(NULL));
  nusmv_assert(Prop_get_type(prop) == Prop_Psl);

  if (!Prop_is_psl_ltl(prop)) {
    StreamMgr_print_error(streams,  "SBMC can be used only with Psl/ltl properies.\n");
    return 1;
  }

  if (inc_sat) {
#if NUSMV_HAVE_INCREMENTAL_SAT
    return Bmc_GenSolveLtlInc(env, prop, k, rel_loop, !is_single_prob);
#else
    {
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
      ErrorMgr_internal_error(errmgr,
                              "Sbmc_Gen_check_psl_property: Inc SAT Solving "\
                              "requested when not supported.\n");
    }
#endif
  }

  if (is_single_prob && inc_sat) {
    StreamMgr_print_error(streams,
            "Error: single problem generation (option -1) with incremental "\
            "solvers is an unsupported feature of SBMC.\n");
    return 1;
  }

  if (dump_prob && inc_sat) {
    StreamMgr_print_error(streams,
            "Error: problem cannot be dumped when incremental sat solving is used.\n");
    return 1;
  }

  if (inc_sat) {
    if (Sbmc_zigzag_incr(env, prop, k, do_virtual_unrolling,
                         do_completeness_check) != 0)
      return 1;
  }
  else {
    if (Bmc_SBMCGenSolveLtl(env, prop, k, rel_loop,
                            ! is_single_prob,
                            BMC_HAS_TO_SOLVE,
                            (dump_prob) ? BMC_DUMP_DIMACS : BMC_DUMP_NONE,
                            get_bmc_dimacs_filename(opts)) != 0) {
      return 1;
    }
  }

  return 0;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

