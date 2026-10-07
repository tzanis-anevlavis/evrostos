/* ---------------------------------------------------------------------------


  This file is part of the ``fsm.bdd'' package of NuSMV version 2.
  Copyright (C) 2008 by FBK-irst.

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
  \author Viktor Schuppan
  \brief Miscellanous routines not really specific to other files

  Miscellanous routines not really specific to other files:

               - Conversion between
                 BddOregJusticeEmptinessBddAlgorithmType and const
                 char*

               - Ensuring the preconditions for forward Emerson-Lei
                 algorithm are met

              

*/


#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/fsm/bdd/bdd.h"
#include "nusmv/core/fsm/bdd/bddInt.h"

#include "nusmv/core/opt/opt.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/utils/StreamMgr.h"
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_BWD_STRING \
  "EL_bwd"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD_STRING \
  "EL_fwd"

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

typedef struct BddELFwdSavedOptions_TAG
{
  boolean forward_search;
  boolean ltl_tableau_forward_search;
  boolean use_reachable_states;
  boolean counter_examples;
} BddELFwdSavedOptions;

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

BddOregJusticeEmptinessBddAlgorithmType
  Bdd_BddOregJusticeEmptinessBddAlgorithmType_from_string(const char* name)
{
  BddOregJusticeEmptinessBddAlgorithmType res;

  if (strcmp(name,
             BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_BWD_STRING) == 0) {
    res = BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_BWD;
  }
  else if (strcmp(name,
                  BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD_STRING) == 0) {
    res = BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD;
  }
  else res = BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_INVALID;

  return res;
}

const char* Bdd_BddOregJusticeEmptinessBddAlgorithmType_to_string
  (const BddOregJusticeEmptinessBddAlgorithmType self)
{
  switch (self) {
  case BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_BWD:
    return BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_BWD_STRING;
    break;
  case BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD:
    return BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD_STRING;
    break;
  default:
    return "Unknown";
    break;
  }
}

void Bdd_print_available_BddOregJusticeEmptinessBddAlgorithms(FILE *file)
{
  int alg;

  fprintf(file, "The available algorithms are: ");
  for (alg = (int) BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_MIN_VALID;
       alg <= (int) BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_MAX_VALID;
       alg++) {
    fprintf(file,
            "%s ",
            Bdd_BddOregJusticeEmptinessBddAlgorithmType_to_string(
               (BddOregJusticeEmptinessBddAlgorithmType) alg));
  }
  fprintf(file, "\n");
}

boolean Bdd_elfwd_check_options(NuSMVEnv_ptr env,
                                unsigned int which_options,
                                boolean on_fail_print)
{
  boolean res=true;
  FlatHierarchy_ptr hierarchy = FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  nusmv_assert(Nil == FlatHierarchy_get_compassion(hierarchy));
  nusmv_assert(get_oreg_justice_emptiness_bdd_algorithm(opts) ==
               BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD);

  if ((which_options & BDD_ELFWD_OPT_FORWARD_SEARCH) &&
      !opt_forward_search(opts)) {
    if (on_fail_print) {
      StreamMgr_print_error(streams,
              "Forward Emerson-Lei must be used with option forward_search "\
              "enabled.\n");
    }
    res=false;
  }
  if ((which_options & BDD_ELFWD_OPT_LTL_TABLEAU_FORWARD_SEARCH) &&
      !opt_ltl_tableau_forward_search(opts)) {
    if (on_fail_print) {
      StreamMgr_print_error(streams,
              "Forward Emerson-Lei must be used with option "\
              "ltl_tableau_forward_search enabled.\n");
    }
    res=false;
  }
  if ((which_options & BDD_ELFWD_OPT_USE_REACHABLE_STATES) &&
      !opt_use_reachable_states(opts)) {
    if (on_fail_print) {
      StreamMgr_print_error(streams,
              "Forward Emerson-Lei must be used with option "\
              "use_reachable_states enabled.\n");
    }
    res=false;
  }
  if ((which_options & BDD_ELFWD_OPT_COUNTER_EXAMPLES) &&
      opt_counter_examples(opts)) {
    if (on_fail_print) {
      StreamMgr_print_error(streams,
              "Forward Emerson-Lei must be used with counterexamples "\
              "disabled (feature not implemented yet).\n");
    }
    res=false;
  }

  return res;
}

BddELFwdSavedOptions_ptr
Bdd_elfwd_check_set_and_save_options(NuSMVEnv_ptr env,
                                     unsigned int which_options)
{
  BddELFwdSavedOptions_ptr saved_options;
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  if (opt_verbose_level_gt(opts, 0)) {
/* warning [MD] here a logger call should be performed*/
    /* Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER)); */
    if (!Bdd_elfwd_check_options(env, which_options, true)) {
      StreamMgr_print_error(streams,
                            "Temporarily (un)setting options as required "
                            "for forward Emerson-Lei.\n");
    }
  } else {
    (void)Bdd_elfwd_check_options(env, which_options, false);
  }

  saved_options = ALLOC(BddELFwdSavedOptions, 1);
  nusmv_assert(saved_options);

  if (which_options & BDD_ELFWD_OPT_FORWARD_SEARCH) {
    saved_options->forward_search = opt_forward_search(opts);
    set_forward_search(opts);
  }
  if (which_options & BDD_ELFWD_OPT_LTL_TABLEAU_FORWARD_SEARCH) {
    saved_options->ltl_tableau_forward_search =
      opt_ltl_tableau_forward_search(opts);
    set_ltl_tableau_forward_search(opts);
  }
  if (which_options & BDD_ELFWD_OPT_USE_REACHABLE_STATES) {
    saved_options->use_reachable_states = opt_use_reachable_states(opts);
    set_use_reachable_states(opts);
  }
  if (which_options & BDD_ELFWD_OPT_COUNTER_EXAMPLES) {
    saved_options->counter_examples = opt_counter_examples(opts);
    unset_counter_examples(opts);
  }

  return saved_options;
}

void Bdd_elfwd_restore_options(NuSMVEnv_ptr env,
                               unsigned int which_options,
                               BddELFwdSavedOptions_ptr saved_options)
{
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  nusmv_assert(saved_options);

  if (which_options & BDD_ELFWD_OPT_FORWARD_SEARCH) {
    if (saved_options->forward_search) set_forward_search(opts);
    else unset_forward_search(opts);
  }
  if (which_options & BDD_ELFWD_OPT_LTL_TABLEAU_FORWARD_SEARCH) {
    if (saved_options->ltl_tableau_forward_search) {
      set_ltl_tableau_forward_search(opts);
    }
    else unset_ltl_tableau_forward_search(opts);
  }
  if (which_options & BDD_ELFWD_OPT_USE_REACHABLE_STATES) {
    if (saved_options->use_reachable_states) set_use_reachable_states(opts);
    else unset_use_reachable_states(opts);
  }
  if (which_options & BDD_ELFWD_OPT_COUNTER_EXAMPLES) {
    if (saved_options->counter_examples) set_counter_examples(opts);
    else unset_counter_examples(opts);
  }

  FREE(saved_options);
}
