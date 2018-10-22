/* ---------------------------------------------------------------------------


  This file is part of the ``mc'' package of NuSMV version 2.
  Copyright (C) 2006 FBK-irst.

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
  \author Marco Roveri
  \brief Language Emptiness

  Check for language emptiness

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/mc/mc.h"
#include "nusmv/core/mc/mcInt.h"

#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/fsm/bdd/bdd.h" /* to check preconditions for EL_fwd */

#include <math.h>
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

static void mc_check_language_emptiness_el_bwd(NuSMVEnv_ptr env,
                                               const BddFsm_ptr fsm,
                                               boolean allinit,
                                               boolean verbose);
static void mc_check_language_emptiness_el_fwd(NuSMVEnv_ptr env,
                                               const BddFsm_ptr fsm,
                                               boolean allinit,
                                               boolean verbose);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Mc_CheckLanguageEmptiness(NuSMVEnv_ptr env, const BddFsm_ptr fsm,
                               boolean allinit, boolean verbose)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  BddOregJusticeEmptinessBddAlgorithmType alg;
  BddELFwdSavedOptions_ptr elfwd_saved_options =
    (BddELFwdSavedOptions_ptr) NULL;

  alg = get_oreg_justice_emptiness_bdd_algorithm(opts);

  if (alg == BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD) {
    elfwd_saved_options =
      Bdd_elfwd_check_set_and_save_options(env, BDD_ELFWD_OPT_FORWARD_SEARCH |
                                           BDD_ELFWD_OPT_USE_REACHABLE_STATES);
  }

  switch(alg) {
  case BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_BWD:
    mc_check_language_emptiness_el_bwd(env, fsm, allinit, verbose);
    break;
  case BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD:
    mc_check_language_emptiness_el_fwd(env, fsm, allinit, verbose);
    break;
  default:
    error_unreachable_code();
    break;
  }

  if (elfwd_saved_options != (BddELFwdSavedOptions_ptr) NULL) {
    Bdd_elfwd_restore_options(env,
                              BDD_ELFWD_OPT_FORWARD_SEARCH |
                              BDD_ELFWD_OPT_USE_REACHABLE_STATES,
                              elfwd_saved_options);
  }
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Checks whether the language is empty using the backward
  Emerson-Lei algorithm

  See Mc_CheckLanguageEmptiness.

  \sa BddFsm_get_fair_states
*/
static void mc_check_language_emptiness_el_bwd(NuSMVEnv_ptr env,
                                               const BddFsm_ptr fsm,
                                               boolean allinit,
                                               boolean verbose)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  bdd_ptr fair_states;
  bdd_ptr init, invar;
  BddEnc_ptr bddenc;
  DDMgr_ptr dd;

  /* Extracting the DD manager */
  bddenc = BddFsm_get_bdd_encoding(fsm);
  dd = BddEnc_get_dd_manager(bddenc);

  fair_states = BddFsm_get_fair_states(fsm);

  init = BddFsm_get_init(fsm);

  invar = BddFsm_get_state_constraints(fsm);

  /* We restrict the set of initial states and of fair states to the
     set of state constraints */
  bdd_and_accumulate(dd, &init, invar);

  /* TODO[MR] This step could be redundant. However there is no guarantee that
     fair_states are a strict subset of invar */
  bdd_and_accumulate(dd, &fair_states, invar);

  /* TODO[AT] the code can be simplified by using bdd_intersected and
     moving fprintf outside of ifs. */
  if (allinit) {
    if (0 == bdd_entailed(dd, init, fair_states)) {
      StreamMgr_print_output(streams,  "The language is empty\n");
    }
    else {
      StreamMgr_print_output(streams,  "The language is not empty\n");
      if (verbose) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "Mc_CheckLanguageEmptiness: verbose not yet implemented\n");
      }
    }
  }
  else {
    bdd_ptr fair_init;

    fair_init = bdd_and(dd, init, fair_states);

    if (bdd_is_false(dd, fair_init)) {
      StreamMgr_print_output(streams,  "The language is empty\n");
    }
    else {
      StreamMgr_print_output(streams,  "The language is not empty\n");

      if (verbose) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "Mc_CheckLanguageEmptiness: verbose not yet implemented\n");
      }
    }

    {/* prints the number of fair-init states */
      double reached_cardinality;
      double search_space_cardinality;
      bdd_ptr mask = BddEnc_get_state_frozen_vars_mask_bdd(bddenc);

      bdd_and_accumulate(dd, &fair_init, mask);
      reached_cardinality = BddEnc_count_states_of_bdd(bddenc, fair_init);
      search_space_cardinality = BddEnc_count_states_of_bdd(bddenc, mask);
      bdd_free(dd, mask);
      StreamMgr_print_output(streams,  "fair states: %g (2^%g) out of %g (2^%g)\n",
              reached_cardinality, log(reached_cardinality)/log(2.0),
              search_space_cardinality, log(search_space_cardinality)/log(2.0));
    }
    /* No longer needed */
    bdd_free(dd, fair_init);
  }

  /* No longer needed */
  bdd_free(dd, invar);
  bdd_free(dd, init);
  bdd_free(dd, fair_states);
}

/*!
  \brief Checks whether the language is empty using the forward
  Emerson-Lei algorithm

  See Mc_CheckLanguageEmptiness.

  \sa BddFsm_get_revfair_states
*/
static void mc_check_language_emptiness_el_fwd(NuSMVEnv_ptr env,
                                               const BddFsm_ptr fsm,
                                               boolean allinit,
                                               boolean verbose)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  BddEnc_ptr bddenc;
  DDMgr_ptr dd;
  bdd_ptr revfair_states;

  /* Preconditions */
  /* VS: In order to compute the value of allinit some additional work
     would have to be done - note that it is NOT sufficient to
     determine whether an initial state can reach a reverse fair
     state; rather one would really have to compute the set of
     (standard, not reverse) fair states. */
  nusmv_assert(!allinit);
  nusmv_assert(Bdd_elfwd_check_options(env, BDD_ELFWD_OPT_FORWARD_SEARCH |
                                       BDD_ELFWD_OPT_USE_REACHABLE_STATES,
                                       false));

  /* Extracting the DD manager */
  bddenc = BddFsm_get_bdd_encoding(fsm);
  dd = BddEnc_get_dd_manager(bddenc);

  revfair_states = BddFsm_get_revfair_states(fsm);

  if (bdd_is_false(dd, revfair_states)) {
    StreamMgr_print_output(streams,  "The language is empty\n");
  }
  else {
    StreamMgr_print_output(streams,  "The language is not empty\n");

    if (verbose) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,
              "Mc_CheckLanguageEmptiness: verbose not yet implemented\n");
    }
  }

  /* No longer needed */
  bdd_free(dd, revfair_states);
}
