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
  \brief Bmc.Pkg module

  This module contains all the bmc package handling functions

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/be/be.h"
#include "nusmv/core/bmc/bmcBmc.h"
#include "nusmv/core/bmc/bmcDump.h"
#include "nusmv/core/bmc/bmcInt.h"
#include "nusmv/core/bmc/bmcPkg.h"
#include "nusmv/core/bmc/bmcSimulate.h"
#include "nusmv/core/bmc/sbmc/sbmcPkg.h"
#include "nusmv/core/cinit/NuSMVEnv.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/fsm/be/BeFsm.h"
#include "nusmv/core/prop/Prop.h"
#include "nusmv/core/prop/PropDb.h"
#include "nusmv/core/sat/sat.h"
#include "nusmv/core/trace/exec/SATCompleteTraceExecutor.h"
#include "nusmv/core/trace/exec/SATPartialTraceExecutor.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/bmc_profiler.h"

#ifdef BENCHMARKING
#include <time.h>
clock_t start_time;
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


/**AutomaticEnd***************************************************************/



/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Bmc_Init(NuSMVEnv_ptr env)
{
  BeEnc_ptr be_enc;
  OptsHandler_ptr opt = NuSMVEnv_get_value(env, ENV_OPTS_HANDLER);

  /* does not initialize the package if previously initialized */
  if (cmp_struct_get_bmc_init(cmps)) return;

#ifdef BENCHMARKING
  StreamMgr_print_output(streams, ":START:benchmarking Bmc_Init\n");
  start_time = clock();
#endif

  if (opt_verbose_level_gt(opt, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Initializing the BMC package... \n");
  }

  /* Initializes the be generic interface layer: */
  Be_Init();

  /* builds the variables manager: */
  Enc_init_be_encoding(env);
  be_enc = BE_ENC(NuSMVEnv_get_value(env, ENV_BE_ENCODER));

  { /* commits all default layers */
    const char* name; int i;
    arrayForEachItem(const char*,
                     SymbTable_get_class_layer_names(
                                                     BaseEnc_get_symb_table(BASE_ENC(be_enc)), NULL),
                     i, name) {
      BaseEnc_commit_layer(BASE_ENC(be_enc), name);
    }
  }

  /* the determinization layer will be committed by the command
     bmc_setup (see Bmc_Pkg_build_master_be_fsm) */

#ifdef BENCHMARKING
  StreamMgr_print_output(streams, ":UTIME = %.4f secs.\n",((double)(clock()-start_time))/CLOCKS_PER_SEC);
  StreamMgr_print_output(streams, ":STOP:benchmarking Bmc_Init\n");
#endif

  /* Initialize the SBMC sub-package */
  SBmc_Init(env);

  cmp_struct_set_bmc_init(cmps);

  bmc_simulate_set_curr_sim_trace(env, TRACE(NULL), -1);

  /* initialize the data structure used by the bmc profiler */
#if NUSMV_HAVE_BMC_PROFILER_LIBRARY
  BMC_PROFILER_INIT_ENV(env);
#endif
}

void Bmc_Quit(NuSMVEnv_ptr env)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Quitting the BMC package... \n");
  }

  /* Destroy the BE FSM if any */
  if (NuSMVEnv_has_value(env, ENV_BE_FSM)) {
    BeFsm_ptr be_fsm = BE_FSM(NuSMVEnv_remove_value(env, ENV_BE_FSM));
    BeFsm_destroy(be_fsm);
  }

  /* destroy the data structure used by the bmc profiler.
     Note that, for some reason I don't know, the Bmc_Init function
     may not be called in a NuSMV run while the Bmc_Quit is always
     called (see cinitInit.c, CInit_init and CInit_end).
   */
#if NUSMV_HAVE_BMC_PROFILER_LIBRARY
  if (NuSMVEnv_has_value(env, ENV_BMC_PROFILER)) {
    BMC_PROFILER_DEINIT_ENV(env);
  }
#endif

  bmc_simulate_set_curr_sim_trace(env, TRACE(NULL), -1);

  /* Shuts down the Be layer: */
  Be_Quit();

  /* shuts down bmc data */
  Bmc_QuitData();

  /* Quits the SBMC sub-package */
  SBmc_Quit(env);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Done \n");
  }

  cmp_struct_unset_bmc_init(cmps);
}

void Bmc_QuitData()
{
  /* Resets the _bmc_test_tableau command private data: */
  Bmc_TestReset();
}

void Bmc_Pkg_build_master_be_fsm(const NuSMVEnv_ptr env)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  BeEnc_ptr be_enc;
  BeFsm_ptr fsm_be;

  /* builds the master bmc fsm: */
  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Building the BMC FSM... \n");
  }

  be_enc = BE_ENC(NuSMVEnv_get_value(env, ENV_BE_ENCODER));

  if (SymbTable_get_layer(BaseEnc_get_symb_table(BASE_ENC(be_enc)),
                          INLINING_LAYER_NAME) != SYMB_LAYER(NULL)) {
    /* commits the determ layer if not previulsy committed */
    if (!BaseEnc_layer_occurs(BASE_ENC(be_enc), DETERM_LAYER_NAME)) {
      BaseEnc_commit_layer(BASE_ENC(be_enc), DETERM_LAYER_NAME);
    }

    /* commits the inlining layer if not previulsy committed */
    if (!BaseEnc_layer_occurs(BASE_ENC(be_enc), INLINING_LAYER_NAME)) {
      BaseEnc_commit_layer(BASE_ENC(be_enc), INLINING_LAYER_NAME);
    }
  }

  /* :WARNING: The FSM is currently destroyed by the package 'prop',
     but it is built here! */
  fsm_be = BeFsm_create_from_sexp_fsm(be_enc,
                                      BOOL_SEXP_FSM(NuSMVEnv_get_value(env, ENV_BOOL_FSM)));


  /* Check if there already exists a BE fsm. However, should not be
     the case. */
  if (NuSMVEnv_has_value(env, ENV_BE_FSM)) {
    BeFsm_ptr be_fsm = BE_FSM(NuSMVEnv_remove_value(env, ENV_BE_FSM));
    BeFsm_destroy(be_fsm);
  }

  NuSMVEnv_set_value(env, ENV_BE_FSM, fsm_be);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Done \n");
  }
}

int Bmc_Pkg_bmc_setup(NuSMVEnv_ptr env,
                      boolean forced)
{
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  int retval = 0;

  Bmc_Init(env); /* encoding and layers */

  /* constructs the model only if coi is not enabled */
  if (opt_cone_of_influence(opts) && !forced) {
    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,
              "Construction of BE model is delayed due to use of COI\n");
    }

    return retval;
  }

  Bmc_Pkg_build_master_be_fsm(env);

  {
    /* register SAT based complete and partial executors */
    BeFsm_ptr fsm = BE_FSM(NuSMVEnv_get_value(env, ENV_BE_FSM));
    BeEnc_ptr enc = BeFsm_get_be_encoding(fsm);
    BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));

    TraceMgr_register_complete_trace_executor(
                 TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                 "sat", "SAT complete trace execution",
                 COMPLETE_TRACE_EXECUTOR(
                     SATCompleteTraceExecutor_create(fsm, enc, bdd_enc)));

    TraceMgr_register_partial_trace_executor(
                 TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                 "sat", "SAT partial trace execution (no restart)",
                 PARTIAL_TRACE_EXECUTOR(
                     SATPartialTraceExecutor_create(fsm, enc, bdd_enc, false)));

    TraceMgr_register_partial_trace_executor(
                 TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                 "sat_r", "SAT partial trace execution (restart)",
                 PARTIAL_TRACE_EXECUTOR(
                     SATPartialTraceExecutor_create(fsm, enc, bdd_enc, true)));
  }

  cmp_struct_set_bmc_setup(cmps);

  return retval;
}

int Bmc_check_if_model_was_built(NuSMVEnv_ptr env, FILE* err, boolean forced)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (cmp_struct_get_bmc_setup(cmps)) return 0;

  if (cmp_struct_get_bmc_init(cmps) && opt_cone_of_influence(opts) &&
      !forced) return 0;

  if (Compile_check_if_bool_model_was_built(env, errstream, forced)) return 1;

  if (err != (FILE*) NULL) {
    fprintf (err, "Bmc must be setup before. Use the command \"");

    if (forced && opt_cone_of_influence(opts)) {
      fprintf (err, "bmc_setup -f\" as Cone Of Influence is enabled.\n");
    }
    else fprintf (err, "bmc_setup\".\n");
  }

  return 1;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

