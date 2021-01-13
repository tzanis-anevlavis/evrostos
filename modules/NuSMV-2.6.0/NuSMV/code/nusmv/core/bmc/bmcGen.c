/* ---------------------------------------------------------------------------


  This file is part of the ``bmc'' package of NuSMV version 2.
  Copyright (C) 2000-2001 by FBK-irst and University of Trento.

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
  \author Roberto Cavada
  \brief Bmc.Gen module

  This module contains all the problems generation functions

*/


#include "nusmv/core/bmc/bmcGen.h"
#include "nusmv/core/bmc/bmcInt.h"
#include "nusmv/core/bmc/bmcModel.h"
#include "nusmv/core/bmc/bmcTableau.h"
#include "nusmv/core/bmc/bmcUtils.h"
#include "nusmv/core/bmc/bmcConv.h"
#include "nusmv/core/bmc/bmcDump.h"
#include "nusmv/core/bmc/bmcBmc.h"

#include "nusmv/core/wff/wff.h"
#include "nusmv/core/wff/w2w/w2w.h"

#include "nusmv/core/prop/PropDb.h"

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

be_ptr Bmc_Gen_InvarProblem(const BeFsm_ptr be_fsm, const node_ptr wff)
{
  Be_Manager_ptr be_mgr = BeEnc_get_be_manager(BeFsm_get_be_encoding(be_fsm));
  be_ptr base = Bmc_Gen_InvarBaseStep(be_fsm, wff);
  be_ptr induct = Bmc_Gen_InvarInductStep(be_fsm, wff);

  return Be_Not(be_mgr, Be_And(be_mgr, base, induct));
}

be_ptr Bmc_Gen_LtlProblem(const BeFsm_ptr be_fsm,
                          const node_ptr ltl_wff,
                          const int k, const int l)
{
  Be_Manager_ptr mgr = BeEnc_get_be_manager(BeFsm_get_be_encoding(be_fsm));
  be_ptr path_k = Bmc_Model_GetPathWithInit(be_fsm, k);
  be_ptr tableau = Bmc_Tableau_GetLtlTableau(be_fsm, ltl_wff, k, l);
  return Be_And(mgr, tableau, path_k);
}

be_ptr Bmc_Gen_InvarBaseStep(const BeFsm_ptr be_fsm, const node_ptr wff)
{
  BeEnc_ptr be_enc = BeFsm_get_be_encoding(be_fsm);
  Be_Manager_ptr be_mgr = BeEnc_get_be_manager(be_enc);

  be_ptr P_0 = BeEnc_untimed_expr_to_timed(be_enc,
                                           Bmc_Conv_Bexp2Be(be_enc, wff), 0);

  return Be_Implies( be_mgr, Be_And(be_mgr,
                                    Bmc_Model_GetInit0(be_fsm),
                                    Bmc_Model_GetInvarAtTime(be_fsm, 0)),
                     P_0 );
}

be_ptr Bmc_Gen_InvarInductStep(const BeFsm_ptr be_fsm,
                               const node_ptr wff)
{
  BeEnc_ptr be_enc = BeFsm_get_be_encoding(be_fsm);
  Be_Manager_ptr be_mgr = BeEnc_get_be_manager(be_enc);

  be_ptr P = Bmc_Conv_Bexp2Be(be_enc, wff);

  be_ptr trans_01_invar_01 = Bmc_Model_GetPathNoInit(be_fsm, 1);

  be_ptr trans_01_invar_01_P0 =
    Be_And(be_mgr,
           trans_01_invar_01,
           BeEnc_untimed_expr_to_timed(be_enc, P, 0));

  return Be_Implies(be_mgr, trans_01_invar_01_P0,
                    BeEnc_untimed_expr_to_timed(be_enc, P, 1));
}

be_ptr Bmc_Gen_UnrollingFragment(BeFsm_ptr be_fsm, const int i)
{
  BeEnc_ptr be_enc = BeFsm_get_be_encoding(be_fsm);
  Be_Manager_ptr be_mgr = BeEnc_get_be_manager(be_enc);
  nusmv_assert(0<=i);

  /* Init[0] & Invar[0] */
  if (0 == i) {
    return Bmc_Model_GetInit0(be_fsm);
  }

  /* Invar[i-1] & Trans[i-1] & Invar[i] */
  return Be_And(be_mgr,
                Bmc_Model_GetInvarAtTime(be_fsm, i -1),
                Be_And(be_mgr,
                       Bmc_Model_GetTransAtTime(be_fsm, i-1),
                       Bmc_Model_GetInvarAtTime(be_fsm, i)));
}

int Bmc_Gen_check_psl_property(NuSMVEnv_ptr env,
                               Prop_ptr prop,
                               boolean dump_prob,
                               boolean inc_sat,
                               boolean single_prob,
                               int k, int rel_loop)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  nusmv_assert(prop != PROP(NULL));
  nusmv_assert(Prop_get_type(prop) == Prop_Psl);

  /* checks the property is LTL compatible */
  if (!Prop_is_psl_ltl(prop)) {
    StreamMgr_print_error(streams,  "BMC can be used only with Psl/ltl properties.\n");
    return 1;
  }

  if (inc_sat) {
#if NUSMV_HAVE_INCREMENTAL_SAT
    return Bmc_GenSolveLtlInc(env, prop, k, rel_loop, !single_prob);
#else
    {
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

      ErrorMgr_internal_error(errmgr,
                              "Bmc_Gen_check_psl_property: Inc SAT Solving "\
                              "requested when not supported.\n");
    }
#endif
  }

  return Bmc_GenSolveLtl(env, prop, k, rel_loop,
                         !single_prob, /* incrementally */
                         BMC_HAS_TO_SOLVE,
                         dump_prob ? BMC_DUMP_DIMACS : BMC_DUMP_NONE,
                         get_bmc_dimacs_filename(opts));
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

