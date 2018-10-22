/* ---------------------------------------------------------------------------

 This file is part of the ``bmc.sbmc'' package of NuSMV version 2.
  Copyright (C) 2006 Tommi Junttila.

  NuSMV version 2 is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2 of
  the License, or (at your option) any later version.

  NuSMV version 2 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
  USA.

  For more information of NuSMV see <http://nusmv.fbk.eu> or
  email to <nusmv-users@fbk.eu>.  Please report bugs to
  <nusmv-users@fbk.eu>.

  To contact the NuSMV development board, email to  <nusmv@fbk.eu>.

-----------------------------------------------------------------------------*/

/*!
  \author Tommi Junttila, Marco Roveri
  \brief High level functionalities for Incrememntal SBMC

  User-commands directly use function defined in this module.
  This is the highest level in the Incrememntal SBMC API architecture.

  For further information about this implementation, see:
  K. Heljanko, T. Junttila and T. Latvala.  Incremental and Complete
  Bounded Model Checking for Full PLTL.  In K. Etessami and
  S. Rajamani (eds.), Computer Aided Verification, Edinburgh,
  Scotland, Volume 3576 of LNCS, pp. 98-111, Springer, 2005.
  Copyright (C) Springer-Verlag.

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include <stdlib.h>
#include <stdio.h>

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/bmc/sbmc/sbmcStructs.h"
#include "nusmv/core/bmc/sbmc/sbmcUtils.h"
#include "nusmv/core/bmc/sbmc/sbmcTableauInc.h"
#include "nusmv/core/bmc/sbmc/sbmcTableauIncLTLformula.h"
#include "nusmv/core/bmc/sbmc/sbmcBmcInc.h"

#include "nusmv/core/bmc/bmcInt.h"
#include "nusmv/core/bmc/bmcModel.h"

#ifdef BENCHMARKING
# include <unistd.h>
# include <sys/times.h>
#endif

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/be/be.h"

#include "nusmv/core/sat/SatSolver.h"
#include "nusmv/core/sat/SatIncSolver.h"

#include "nusmv/core/enc/enc.h"
#include "nusmv/core/enc/be/BeEnc.h"
#include "nusmv/core/prop/Prop.h"
#include "nusmv/core/prop/PropDb.h"
#include "nusmv/core/prop/propPkg.h"
#include "nusmv/core/opt/opt.h"

#include "nusmv/core/mc/mc.h" /* for print_spec */

#include "nusmv/core/parser/symbols.h" /* for tokens */
#include "nusmv/core/prop/propProp.h"


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SBMC_INC_LAYER_NAME  "LTL translation vars"

/* Define this to disable simplepath constraint */
#undef SBMC_HAVENOSIMPLEPATH

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
#ifdef BENCHMARKING
static struct tms time_bmc_start;
/* static struct tms time_gen_start; */
static struct tms time_solver_start;
static struct tms time_temp;
static double time_diff(struct tms *start, struct tms *stop);

static double time_diff(struct tms *start, struct tms *stop)
{
  return ((((double)stop->tms_utime + (double)stop->tms_stime) -
           ((double)start->tms_utime + (double)start->tms_stime)) /
          (double)CLOCKS_PER_SEC);
}
#endif

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static hash_ptr
sbmc_bmc_inc_setup_layer(const NuSMVEnv_ptr env,
                         BeFsm_ptr be_fsm,
                         BddEnc_ptr bdd_enc,
                         const char* layer_name,
                         state_vars_struct* state_vars,
                         node_ptr bltlspec,
                         const int opt_do_virtual_unrolling,
                         const int opt_do_completeness_check,
                         const int opt_force_state_vars,
                         node_ptr * ass_SimplePath_node);

static void
sbmc_bmc_inc_shutdown_layer(BeFsm_ptr be_fsm,
                            BddEnc_ptr bdd_enc,
                            const char* layer_name,
                            hash_ptr layer_info_map);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

int Sbmc_zigzag_incr(NuSMVEnv_ptr env,
                     Prop_ptr ltlprop, const int max_k,
                     const int opt_do_virtual_unrolling,
                     const int opt_do_completeness_check)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  FILE* outstream = StreamMgr_get_output_stream(streams);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  node_ptr bltlspec;  /* Its booleanization */
  BeFsm_ptr be_fsm = BE_FSM(NULL); /* The corresponding be fsm */
  BeEnc_ptr be_enc;
  Be_Manager_ptr be_mgr;
  node_ptr ass_SimplePath_node;

  const Be_CnfAlgorithm cnf_alg = get_rbc2cnf_algorithm(opts);

  /* Structure for holding state variable information */
  state_vars_struct *state_vars = (state_vars_struct *)NULL;

  /* The solver sbmc interface */
  sbmc_MetaSolver *zolver = (sbmc_MetaSolver *)NULL;

  int current_k = 0;
  int previous_k = -1; /* used to create Be of execution from time 0 */
  boolean found_solution = 0;
  boolean formula_forced_to_true = 0;

  /* Associate each subformula node_ptr with sbmc_node_info */
  hash_ptr info_map;

  /* The LoopExists variable */
  be_ptr be_LoopExists = (be_ptr)NULL;

  /* Array of be_ptr,
     InLoop_array[i] has the be for InLoop_i (where i is a model index) */
  array_t* InLoop_array = (array_t *)NULL;

  /* Force each subformula to have its own state variable.
   * Costly, only for debugging/cross validation */
  const int opt_force_state_vars = 0;
  const int opt_do_optimization = 1;

  Prop_ptr inputprop = ltlprop;
  Prop_Rewriter_ptr rewriter = NULL;

#ifdef BENCHMARKING
  times(&time_bmc_start);
#endif

  /* checks that a property was selected: */
  nusmv_assert(ltlprop != PROP(NULL));

  if (Prop_get_status(ltlprop) != Prop_Unchecked) {
    return 0;
  }

  be_fsm = Prop_compute_ground_be_fsm(env, ltlprop);
  BE_FSM_CHECK_INSTANCE(be_fsm);

  rewriter = Prop_Rewriter_create(env, ltlprop,
                                  WFF_REWRITE_METHOD_DEADLOCK_FREE,
                                  WFF_REWRITER_REWRITE_INPUT_NEXT,
                                  FSM_TYPE_BE, bdd_enc);
  ltlprop = Prop_Rewriter_rewrite(rewriter);
  be_fsm = Prop_get_be_fsm(ltlprop);

  /* Get be encoding */
  be_enc = BeFsm_get_be_encoding(be_fsm);

  /*
   * Booleanized, negated and NNFed formula with fairness constraints
   */
  bltlspec = sbmc_make_boolean_formula(bdd_enc, ltlprop);
  nusmv_assert((node_ptr)NULL != bltlspec);


  /* Find manager */
  be_mgr = BeEnc_get_be_manager(be_enc);

  /*
   * Incremental SAT solver construction
   */
  zolver = sbmc_MS_create(be_enc);
  if ((sbmc_MetaSolver *)NULL == zolver) {
    /* Something went wrong */
    Prop_Rewriter_update_original_property(rewriter);
    Prop_Rewriter_destroy(rewriter); rewriter = NULL;

    return 1;
  }

  /*
   * Initialize the state variable structure
   */
  state_vars = sbmc_state_vars_create(env);
  nusmv_assert((state_vars_struct *)NULL != state_vars);

  if (opt_verbose_level_gt(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    FILE* stream = Logger_get_stream(logger);
    sbmc_state_vars_print(state_vars, stream);
  }

  /*
   * Get the state variables occurring in the transition relation and
   * add them to state_vars->transition_state_vars
   */
  {
    lsGen iterator;
    be_ptr be_var = (be_ptr)NULL;
    hash_ptr node_seen = (hash_ptr)NULL;
    lsList tr_be_vars =
      Bmc_Utils_get_vars_list_for_uniqueness(be_enc, ltlprop);
    lsList tsv = sbmc_state_vars_get_trans_state_vars(state_vars);

    node_seen = sbmc_set_create();
    lsForEachItem(tr_be_vars, iterator, be_var) {
      node_ptr node = BeEnc_var_to_name(be_enc, be_var);

      if (!sbmc_set_is_in(node_seen, node)) {
        lsNewEnd(tsv, (lsGeneric)node, LS_NH);
        sbmc_set_insert(node_seen, node);
      }
    }
    sbmc_set_destroy(node_seen);
    node_seen = (hash_ptr)NULL;
    lsDestroy(tr_be_vars, NULL);
    tr_be_vars = (lsList)NULL;
  }


  /* creates the translation var layer */
  info_map = sbmc_bmc_inc_setup_layer(env, be_fsm, bdd_enc, SBMC_INC_LAYER_NAME,
                                      state_vars, bltlspec,
                                      opt_do_virtual_unrolling,
                                      opt_do_completeness_check,
                                      opt_force_state_vars,
                                      &ass_SimplePath_node);

  /*
   * Find state and input variables that occurr in the formula.
   * Build the list of system variables for simple path constraints.
   *
   * state_vars->formula_state_vars will have the state vars occurring
   *   in the formula bltlspec
   * state_vars->formula_input_vars will have the input vars occurring
   *   in the formula bltlspec
   * state_vars->simple_path_system_vars will be the union of
   *   state_vars->transition_state_vars,
   *   state_vars->formula_state_vars, and
   *   state_vars->formula_input_vars
   */
  sbmc_find_relevant_vars(state_vars, be_fsm, bltlspec);

  if (opt_verbose_level_gt(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    FILE* stream = Logger_get_stream(logger);
    sbmc_state_vars_print(state_vars, stream);
  }

  /*
   * Create the LoopExists variable be
   * Simulate untimed variable by using only time index 0.
   */
  be_LoopExists =
    BeEnc_name_to_timed(be_enc,
                        sbmc_state_vars_get_LoopExists_var(state_vars),
                        0);

  /*
   * Initialize the InLoop array
   */
  InLoop_array = array_alloc(be_ptr, 1);

  /*
   * Initialize state vectors of aux states L and E
   */
  sbmc_init_state_vector(be_enc, bltlspec, info_map, sbmc_L_state(),
                         sbmc_state_vars_get_LastState_var(state_vars),
                         be_LoopExists);
  sbmc_init_state_vector(be_enc, bltlspec, info_map, sbmc_E_state(),
                         sbmc_state_vars_get_LastState_var(state_vars),
                         be_LoopExists);

  /*
   * Insert the initial condition into the sat solver permanently
   */
  {
    be_ptr be_init = Bmc_Model_GetInitI(be_fsm, sbmc_real_k(0));

    sbmc_MS_force_true(zolver, be_init, cnf_alg);

    if (opt_verbose_level_ge(opts, 6)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Forced initial condition: ");
      Be_DumpSexpr(be_mgr, be_init, errstream);
      Logger_log(logger, "\n");
    }
  }

  /*
   * Make base stuff
   */
  {
    lsList new_constraints =
      sbmc_unroll_base(be_enc, bltlspec, info_map, be_LoopExists,
                           opt_do_optimization);
    /* Force constraints to be true in the fixed frame */
    sbmc_MS_force_constraint_list(zolver, new_constraints, cnf_alg);
    lsDestroy(new_constraints, NULL);
    new_constraints = (lsList)NULL;
  }

  /*
   * Start problem generation:
   */
  previous_k = -1000; /* Start from deep prehistory (at least <= -2) */
  for (current_k = 0; current_k <= max_k && !found_solution; current_k++) {
    int i = 0;
    SatSolverResult satResult;

    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,
              "\nGenerating problem with bound %d, all possible loopbacks...\n",
              current_k);
    }

    /********************************************************************
     *
     * The k-invariant part of the translation
     *
     ********************************************************************/

    /*
     * Formula state vector initialization
     * Initialize new state vectors up to current_k+1
     * Assumes that state vectors are already initialized up to previous_k+1
     */
    for (i = max(previous_k + 2, 0); i <= current_k + 1; i++) {
      sbmc_init_state_vector(be_enc, bltlspec, info_map, sbmc_real_k(i),
                             sbmc_state_vars_get_LastState_var(state_vars),
                             be_LoopExists);
    }

    /*
     * Unroll the model transition relation up to current_k
     * Force it to the fixed frame 0
     * Assumes that it has already been unrolled up to previous_k
     */
    for (i = max(previous_k, 0); i < current_k; i++) {
      be_ptr be_TR = Bmc_Model_GetUnrolling(be_fsm,
                                            sbmc_real_k(i), sbmc_real_k(i+1));

      sbmc_MS_force_true(zolver, be_TR, cnf_alg);

      if (opt_verbose_level_gt(opts, 1))
        StreamMgr_print_error(streams,  "Forced T(%d,%d)", i, i+1);
      if (opt_verbose_level_ge(opts, 7)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, ": ");
        Be_DumpSexpr(be_mgr, be_TR, errstream);
      }
      if (opt_verbose_level_gt(opts, 1))
        StreamMgr_print_error(streams,  "\n");
    }

    /*
     * Unroll the (l_i => (s_{i-1} = s_E)) constraint up to current_k
     * Force it to the fixed frame 0
     * Assumes that it has already been unrolled up to previous_k
     */
    for (i = max(previous_k + 1, 0); i <= current_k; i++) {
      be_ptr be_l_i, be_constraint;

      be_l_i =
        BeEnc_name_to_timed(be_enc,
                            sbmc_state_vars_get_l_var(state_vars),
                            sbmc_real_k(i));

      if (i == 0) {
        /* l_0 <=> FALSE */
        be_constraint = Be_Iff(be_mgr, be_l_i, Be_Falsity(be_mgr));
        if (opt_verbose_level_ge(opts, 6)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, "Created (l_0 <=> false): ");
          Be_DumpSexpr(be_mgr, be_constraint, errstream);
          Logger_log(logger, "\n");
        }
      }
      else {
        be_constraint =
          sbmc_equal_vectors_formula(be_enc,
                                     sbmc_state_vars_get_simple_path_system_vars(state_vars),
                                     sbmc_real_k(i-1), sbmc_E_state());
        be_constraint = Be_Implies(be_mgr, be_l_i, be_constraint);
        if (opt_verbose_level_ge(opts, 6)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, "Created (l_%d => (s_%d = s_E)): ",
                  i, i-1);
          Be_DumpSexpr(be_mgr, be_constraint, errstream);
          Logger_log(logger, "\n");
        }
      }
      sbmc_MS_force_true(zolver, be_constraint, cnf_alg);
    }

    /*
     * Unroll the (LastState_i <=> False) constraint up to current_k-1
     * Force it to the fixed frame 0
     * Assumes that it has already been unrolled up to previous_k-2
     */
    for (i = max(previous_k, 0); i <= current_k-1; i++) {
      be_ptr be_LastState_i, be_constraint;

      be_LastState_i =
        BeEnc_name_to_timed(be_enc,
                            sbmc_state_vars_get_LastState_var(state_vars),
                            sbmc_real_k(i));

      be_constraint = Be_Iff(be_mgr, be_LastState_i, Be_Falsity(be_mgr));
      if (opt_verbose_level_ge(opts, 6)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "Created (LastState_%d <=> false): ", i);
        Be_DumpSexpr(be_mgr, be_constraint, errstream);
        Logger_log(logger, "\n");
      }
      sbmc_MS_force_true(zolver, be_constraint, cnf_alg);
    }

    /*
     * Define InLoop_i := (InLoop_{i-1} || l_i)
     * and build constraint InLoop_{i-1} => !l_i up to current_k + 1
     * Assumes that they have already been build up to previous_k + 1
     */
    for (i = max(previous_k + 1, 0); i <= current_k; i++) {
      be_ptr be_constraint = sbmc_build_InLoop_i(be_enc,
                                                 state_vars,
                                                 InLoop_array, i);
      if (be_constraint) {
        sbmc_MS_force_true(zolver, be_constraint, cnf_alg);
      }
    }

    if (opt_do_optimization) {
      /*
       * Unroll the (l_i => LoopExists) constraint up to current_k
       * Force 'em true in the fixed frame 0
       * Assumes that this has already been done upto previous_k
       */
      for (i = max(previous_k + 1, 0); i <= current_k; i++) {
        be_ptr be_l_i, be_constraint;

        be_l_i =
          BeEnc_name_to_timed(be_enc,
                              sbmc_state_vars_get_l_var(state_vars),
                              sbmc_real_k(i));

        be_constraint = Be_Implies(be_mgr, be_l_i, be_LoopExists);

        sbmc_MS_force_true(zolver, be_constraint, cnf_alg);

        if (opt_verbose_level_ge(opts, 6)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, "Created (l_%d => LoopExists): ",
                  current_k);
          Be_DumpSexpr(be_mgr, be_constraint, errstream);
          Logger_log(logger, "\n");
        }
      }
    }

    /*
     * Unroll future fragment from previous_k+1 to current_k
     * Unroll past fragment   from previous_k+1 to current_k
     */
    {
      lsList new_constraints = sbmc_unroll_invariant(be_enc,
                                                     bltlspec,
                                                     previous_k,
                                                     current_k,
                                                     state_vars,
                                                     InLoop_array,
                                                     info_map,
                                                     be_LoopExists,
                                                     opt_do_optimization);
      sbmc_MS_force_constraint_list(zolver, new_constraints, cnf_alg);
      lsDestroy(new_constraints, NULL);
      new_constraints = (lsList)NULL;
    }

    /*
     * Force negated NNF formula to true if not already done
     */
    if (!formula_forced_to_true) {
      sbmc_node_info* info;
      array_t*         past_array;
      be_ptr           be_f_0_0;

      /* Get info */
      info = sbmc_node_info_assoc_find(info_map, bltlspec);
      nusmv_assert((sbmc_node_info *)NULL != info);

      /* Get past_array */
      nusmv_assert((array_t *)NULL != sbmc_node_info_get_trans_bes(info));
      nusmv_assert(array_n(sbmc_node_info_get_trans_bes(info)) > 0);
      past_array = array_fetch(array_t *,
                               sbmc_node_info_get_trans_bes(info),
                               sbmc_real_k(0));
      nusmv_assert((array_t *)NULL != past_array);

      be_f_0_0 = array_fetch(be_ptr, past_array, 0);
      nusmv_assert((be_ptr)NULL != be_f_0_0);

      sbmc_MS_force_true(zolver, be_f_0_0, cnf_alg);

      if (opt_verbose_level_ge(opts, 6)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "Forced [[f]]_0^0 to true: ");
        Be_DumpSexpr(be_mgr, be_f_0_0, errstream);
        Logger_log(logger, "\n");
      }
      formula_forced_to_true = true;
    }


    if (opt_do_completeness_check) {
      /*
       * Unroll the SimplePath constraints
       */
#ifndef SBMC_HAVENOSIMPLEPATH
      for (i = max(previous_k+1, 0); i <= current_k; i++) {
        lsList new_constraints =
          sbmc_SimplePaths(be_enc, state_vars, InLoop_array, i);
        sbmc_MS_force_constraint_list(zolver, new_constraints, cnf_alg);
        lsDestroy(new_constraints, NULL);
        new_constraints = (lsList)NULL;
      }
#endif

#ifdef BENCHMARKING
      StreamMgr_print_error(streams,  ":START:benchmarking Solving\n");
      times(&time_solver_start);
#endif

      /*
       * Actually solve the problem
       */
      satResult = sbmc_MS_solve(zolver);

#ifdef BENCHMARKING
      {
        times(&time_temp);
        StreamMgr_print_error(streams,  ":UTIME = %.4f secs.\n",
                time_diff(&time_solver_start, &time_temp));
        StreamMgr_print_error(streams,  ":STOP:benchmarking Solving\n");
        StreamMgr_print_error(streams,
                "completeness, k=%d, solver time=%.4f, cumulative time=%.4f.\n",
                current_k,
                time_diff(&time_solver_start, &time_temp),
                time_diff(&time_bmc_start, &time_temp));
      }
#endif

      /* Processes the result: */
      switch (satResult) {
      case SAT_SOLVER_SATISFIABLE_PROBLEM:
        StreamMgr_print_output(streams,
                "-- could not terminate yet with bound %d",
                current_k);

        if (opt_verbose_level_gt(opts, 2)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, " for ");
          print_spec(Logger_get_ostream(logger),
                     inputprop, get_prop_print_method(opts));
        }

        StreamMgr_print_output(streams,  "\n");
        break;

      case SAT_SOLVER_UNSATISFIABLE_PROBLEM:
        StreamMgr_print_output(streams,  "-- terminating with bound %d.\n", current_k);
        StreamMgr_print_output(streams,  "-- ");
        print_spec(StreamMgr_get_output_ostream(streams),
                   inputprop, get_prop_print_method(opts));
        StreamMgr_print_output(streams,  "  is true\n");
        Prop_set_status(ltlprop, Prop_True);
        fflush(outstream);
        fflush(errstream);

        found_solution = true;
        break;

      case SAT_SOLVER_INTERNAL_ERROR:
        ErrorMgr_internal_error(errmgr, "Sorry, solver answered with a fatal Internal "
                       "Failure during problem solving.\n");
        break;

      case SAT_SOLVER_TIMEOUT:
      case SAT_SOLVER_MEMOUT:
        ErrorMgr_internal_error(errmgr, "Sorry, solver ran out of resources and aborted "
                       "the execution.\n");
        break;

      default:
        ErrorMgr_internal_error(errmgr, "Sbmc_zigzag_incr: Unexpected value in satResult");

      } /* switch */

      if (found_solution) break;

    } /* if (opt_do_completeness_check) */

    /********************************************************************
     *
     * The k-dependent part of the translation
     *
     ********************************************************************/

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Adding k-dependent stuff\n");
    }

    /*
     * Go to the volatile frame 1
     */
    sbmc_MS_goto_volatile_group(zolver);

     /*
      * Make the (LastState_k <=> True) constraint
      * Force it to the volatile frame
      */
     {
       be_ptr be_LastState_k, be_constraint;

       be_LastState_k =
         BeEnc_name_to_timed(be_enc,
                             sbmc_state_vars_get_LastState_var(state_vars),
                             sbmc_real_k(current_k));

       be_constraint = Be_Iff(be_mgr, be_LastState_k, Be_Truth(be_mgr));
       if (opt_verbose_level_ge(opts, 6)) {
         Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
         Logger_log(logger, "Created (LastState_%d <=> true): ", i);
         Be_DumpSexpr(be_mgr, be_constraint, errstream);
         Logger_log(logger, "\n");
       }
       sbmc_MS_force_true(zolver, be_constraint, cnf_alg);
     }

    /*
     * Add k-dependent constraints
     */
    {
      lsList new_constraints = sbmc_dependent(be_enc,
                                              bltlspec,
                                              current_k,
                                              state_vars,
                                              InLoop_array,
                                              be_LoopExists,
                                              info_map);
      sbmc_MS_force_constraint_list(zolver, new_constraints, cnf_alg);
      lsDestroy(new_constraints, NULL);
      new_constraints = (lsList)NULL;
    }

#ifdef BENCHMARKING
    StreamMgr_print_error(streams,  ":START:benchmarking Solving\n");
    times(&time_solver_start);
#endif

    /*
     * Actually solve the problem
     */
    satResult = sbmc_MS_solve(zolver);

#ifdef BENCHMARKING
    {
      times(&time_temp);
      StreamMgr_print_error(streams,  ":UTIME = %.4f secs.\n",
              time_diff(&time_solver_start, &time_temp));
      StreamMgr_print_error(streams,  ":STOP:benchmarking Solving\n");
      StreamMgr_print_error(streams,
              "counterexample, k=%d, solver time=%.4f, cumulative time=%.4f.\n",
              current_k,
              time_diff(&time_solver_start, &time_temp),
              time_diff(&time_bmc_start, &time_temp));
    }
#endif

    /* Processes the result: */
    switch (satResult) {
    case SAT_SOLVER_UNSATISFIABLE_PROBLEM: {
      StreamMgr_print_output(streams,
              "-- no counterexample found with bound %d",
              current_k);
      if (opt_verbose_level_gt(opts, 2)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, " for ");
        print_spec(Logger_get_ostream(logger),
                   inputprop, get_prop_print_method(opts));
      }
      StreamMgr_print_output(streams,  "\n");
      break;
    }
    case SAT_SOLVER_SATISFIABLE_PROBLEM:
      StreamMgr_print_output(streams,  "-- terminating with bound %d.\n", current_k);
      StreamMgr_print_output(streams,  "-- ");
      print_spec(StreamMgr_get_output_ostream(streams),
                 inputprop, get_prop_print_method(opts));
      StreamMgr_print_output(streams,  "  is false\n");
      Prop_set_status(ltlprop, Prop_False);
      fflush(outstream);
      fflush(errstream);

      found_solution = true;

      if (opt_counter_examples(opts)) {
        TraceMgr_ptr tm = TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
        BoolSexpFsm_ptr bsexp_fsm; /* needed for trace language */
        Trace_ptr trace = TRACE(NULL);

        bsexp_fsm = Prop_get_bool_sexp_fsm(ltlprop);
        if (BOOL_SEXP_FSM(NULL) == bsexp_fsm) {
          bsexp_fsm = \
            BOOL_SEXP_FSM(NuSMVEnv_get_value(env, ENV_BOOL_FSM));
          BOOL_SEXP_FSM_CHECK_INSTANCE(bsexp_fsm);
        }

        trace = \
          Sbmc_Utils_generate_and_print_cntexample(be_enc, tm, zolver,
                                  sbmc_state_vars_get_l_var(state_vars),
                                  current_k, "BMC Counterexample",
                                  SexpFsm_get_symbols_list(SEXP_FSM(bsexp_fsm)));

        Prop_set_trace(ltlprop, Trace_get_id(trace));
      }
      break;

    case SAT_SOLVER_INTERNAL_ERROR:
      ErrorMgr_internal_error(errmgr, "Sorry, solver answered with a fatal Internal "
                     "Failure during problem solving.\n");
      break;

    case SAT_SOLVER_TIMEOUT:
    case SAT_SOLVER_MEMOUT:
      ErrorMgr_internal_error(errmgr, "Sorry, solver ran out of resources and aborted "
                     "the execution.\n");
      break;

    default:
      ErrorMgr_internal_error(errmgr, "Sbmc_zigzag_incr: Unexpected value in satResult");
    } /* switch */

    if (opt_verbose_level_gt(opts, 1))
      StreamMgr_print_error(streams,  "Deleting k-dependent stuff\n");

    /*
     * Release the volatile frame 1
     */
    sbmc_MS_goto_permanent_group(zolver);

    /*
     * Remember the current_k
     */
    previous_k = current_k;
  }

  if (!found_solution) {
    StreamMgr_print_output(streams,  "-- terminating with bound %d.\n", previous_k);
  }

  /* shuts down the previously set up layer */
  sbmc_bmc_inc_shutdown_layer(be_fsm, bdd_enc, SBMC_INC_LAYER_NAME, info_map);

  /* Destroy InLoop_array */
  array_free(InLoop_array);

  /* Destroy the sat solver instance */
  sbmc_MS_destroy(zolver);

  /* cleanup */
  if (state_vars != (state_vars_struct*) NULL) {
    sbmc_state_vars_destroy(state_vars);
  }

  Prop_Rewriter_update_original_property(rewriter);
  Prop_Rewriter_destroy(rewriter); rewriter = NULL;

  return 0;
}

int Sbmc_zigzag_incr_assume(NuSMVEnv_ptr env,
                            Prop_ptr ltlprop, const int max_k,
                            const int opt_do_virtual_unrolling,
                            const int opt_do_completeness_check,
                            Slist_ptr assumptions,
                            Slist_ptr* conflict)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  node_ptr bltlspec;  /* Its booleanization */
  BeFsm_ptr be_fsm = BE_FSM(NULL); /* The corresponding be fsm */
  BeEnc_ptr be_enc;
  Be_Manager_ptr be_mgr;
  lsList assumptions_be;
  Slist_ptr assumptions_cnf;
  node_ptr ass_SimplePath_node;
#ifndef SBMC_HAVENOSIMPLEPATH
  be_ptr ass_SimplePath_be;
  int ass_SimplePath_cnflit;
#endif

  const Be_CnfAlgorithm cnf_alg = get_rbc2cnf_algorithm(opts);

  /* Structure for holding state variable information */
  state_vars_struct *state_vars = (state_vars_struct *)NULL;

  /* The solver sbmc interface */
  sbmc_MetaSolver *zolver = (sbmc_MetaSolver *)NULL;

  int current_k = 0;
  int previous_k = -1; /* used to create Be of execution from time 0 */
  boolean found_solution = 0;
  boolean formula_forced_to_true = 0;

  /* Associate each subformula node_ptr with sbmc_node_info */
  hash_ptr info_map;

  /* The LoopExists variable */
  be_ptr be_LoopExists = (be_ptr)NULL;

  /* Array of be_ptr,
     InLoop_array[i] has the be for InLoop_i (where i is a model index) */
  array_t* InLoop_array = (array_t *)NULL;

  /* Force each subformula to have its own state variable.
   * Costly, only for debugging/cross validation */
  const int opt_force_state_vars = 0;
  const int opt_do_optimization = 1;

  Prop_ptr inputprop = ltlprop;
  Prop_Rewriter_ptr rewriter = NULL;

#ifdef BENCHMARKING
  times(&time_bmc_start);
#endif
  nusmv_assert(!strcmp(get_sat_solver(opts), "MiniSat"));

  /* checks that a property was selected: */
  nusmv_assert(ltlprop != PROP(NULL));

  if (Prop_get_status(ltlprop) != Prop_Unchecked) {
    return 0;
  }

  be_fsm = Prop_compute_ground_be_fsm(env, ltlprop);
  BE_FSM_CHECK_INSTANCE(be_fsm);

  rewriter = Prop_Rewriter_create(env, ltlprop,
                                  WFF_REWRITE_METHOD_DEADLOCK_FREE,
                                  WFF_REWRITER_REWRITE_INPUT_NEXT,
                                  FSM_TYPE_BE, bdd_enc);
  ltlprop = Prop_Rewriter_rewrite(rewriter);
  be_fsm = Prop_get_be_fsm(ltlprop);

  /* Get be encoding */
  be_enc = BeFsm_get_be_encoding(be_fsm);

  /*
   * Booleanized, negated and NNFed formula with fairness constraints
   */
  bltlspec = sbmc_make_boolean_formula(bdd_enc, ltlprop);
  nusmv_assert((node_ptr)NULL != bltlspec);


  /* Find manager */
  be_mgr = BeEnc_get_be_manager(be_enc);

  /*
   * Incremental SAT solver construction
   */
  zolver = sbmc_MS_create(be_enc);
  if ((sbmc_MetaSolver *)NULL == zolver) {
    /* Something went wrong */
    Prop_Rewriter_update_original_property(rewriter);
    Prop_Rewriter_destroy(rewriter); rewriter = NULL;

    return 1;
  }

  /*
   * Initialize the state variable structure
   */
  state_vars = sbmc_state_vars_create(env);
  nusmv_assert((state_vars_struct *)NULL != state_vars);

  if (opt_verbose_level_gt(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    FILE* stream = Logger_get_stream(logger);
    sbmc_state_vars_print(state_vars, stream);
  }

  /*
   * Get the state variables occurring in the transition relation and
   * add them to state_vars->transition_state_vars
   */
  {
    lsGen iterator;
    be_ptr be_var = (be_ptr)NULL;
    hash_ptr node_seen = (hash_ptr)NULL;
    lsList tr_be_vars =
      Bmc_Utils_get_vars_list_for_uniqueness(be_enc, ltlprop);
    lsList tsv = sbmc_state_vars_get_trans_state_vars(state_vars);

    node_seen = sbmc_set_create();
    lsForEachItem(tr_be_vars, iterator, be_var) {
      node_ptr node = BeEnc_var_to_name(be_enc, be_var);

      if (!sbmc_set_is_in(node_seen, node)) {
        lsNewEnd(tsv, (lsGeneric)node, LS_NH);
        sbmc_set_insert(node_seen, node);
      }
    }
    sbmc_set_destroy(node_seen);
    node_seen = (hash_ptr)NULL;
    lsDestroy(tr_be_vars, NULL);
    tr_be_vars = (lsList)NULL;
  }


  /* creates the translation var layer */
  info_map = sbmc_bmc_inc_setup_layer(env, be_fsm, bdd_enc, SBMC_INC_LAYER_NAME,
                                      state_vars, bltlspec,
                                      opt_do_virtual_unrolling,
                                      opt_do_completeness_check,
                                      opt_force_state_vars,
                                      &ass_SimplePath_node);

  /*
   * Find state and input variables that occurr in the formula.
   * Build the list of system variables for simple path constraints.
   *
   * state_vars->formula_state_vars will have the state vars occurring
   *   in the formula bltlspec
   * state_vars->formula_input_vars will have the input vars occurring
   *   in the formula bltlspec
   * state_vars->simple_path_system_vars will be the union of
   *   state_vars->transition_state_vars,
   *   state_vars->formula_state_vars, and
   *   state_vars->formula_input_vars
   */
  sbmc_find_relevant_vars(state_vars, be_fsm, bltlspec);

  if (opt_verbose_level_gt(opts, 4)) {
    sbmc_state_vars_print(state_vars, errstream);
  }

  /*
   * Create the LoopExists variable be
   * Simulate untimed variable by using only time index 0.
   */
  be_LoopExists =
    BeEnc_name_to_timed(be_enc,
                        sbmc_state_vars_get_LoopExists_var(state_vars),
                        0);

  /*
   * Initialize the InLoop array
   */
  InLoop_array = array_alloc(be_ptr, 1);
  nusmv_assert(NULL != InLoop_array);

  /*
   * Initialize state vectors of aux states L and E
   */
  sbmc_init_state_vector(be_enc, bltlspec, info_map, sbmc_L_state(),
                         sbmc_state_vars_get_LastState_var(state_vars),
                         be_LoopExists);
  sbmc_init_state_vector(be_enc, bltlspec, info_map, sbmc_E_state(),
                         sbmc_state_vars_get_LastState_var(state_vars),
                         be_LoopExists);

  /*
   * Insert the initial condition into the sat solver permanently
   */
  {
    be_ptr be_init = Bmc_Model_GetInitI(be_fsm, sbmc_real_k(0));

    sbmc_MS_force_true(zolver, be_init, cnf_alg);

    if (opt_verbose_level_ge(opts, 6)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Forced initial condition: ");
      Be_DumpSexpr(be_mgr, be_init, errstream);
      Logger_log(logger, "\n");
    }
  }

  /*
   * Make base stuff
   */
  {
    lsList new_constraints =
      sbmc_unroll_base(be_enc, bltlspec, info_map, be_LoopExists,
                       opt_do_optimization);
    /* Force constraints to be true in the fixed frame */
    sbmc_MS_force_constraint_list(zolver, new_constraints, cnf_alg);
    lsDestroy(new_constraints, NULL);
    new_constraints = (lsList)NULL;
  }

  /*
   * Make assumptions
   */
  {
    Siter iterator;
    node_ptr assumption;

    assumptions_be = lsCreate();
    nusmv_assert(NULL != assumptions_be);
    assumptions_cnf = Slist_create();
    nusmv_assert(NULL != assumptions_cnf);

    /* assumptions for atoms */
    SLIST_FOREACH(assumptions, iterator) {
      node_ptr ass_name;
      be_ptr ass_be;
      int ass_ind, ass_lit, ass_cnf;

      assumption = (node_ptr) Siter_element(iterator);

      if (NOT == node_get_type(assumption)) {
        ass_name = car(assumption);
      } else {
        ass_name = assumption;
      }
      ass_be = BeEnc_name_to_timed(be_enc, ass_name, sbmc_real_k(0));
      if (NOT == node_get_type(assumption)) {
        lsNewEnd(assumptions_be, (lsGeneric) Be_Not(be_mgr, ass_be), LS_NH);
      } else {
        lsNewEnd(assumptions_be, (lsGeneric) ass_be, LS_NH);
      }
      ass_ind = Be_Var2Index(be_mgr, ass_be);
      nusmv_assert(0 != ass_ind);
      ass_lit = Be_BeIndex2BeLiteral(be_mgr, ass_ind);
      nusmv_assert(0 != ass_lit);
      ass_cnf = Be_BeLiteral2CnfLiteral(be_mgr, ass_lit);
      if (0 != ass_cnf) {
        if (NOT == node_get_type(assumption)) {
          Slist_push(assumptions_cnf,
                     PTR_FROM_INT(void*, Be_CnfLiteral_Negate(be_mgr, ass_cnf)));
        } else {
          Slist_push(assumptions_cnf, PTR_FROM_INT(void*, ass_cnf));
        }
      }
    }

#ifndef SBMC_HAVENOSIMPLEPATH
    /* assumption for SimplePath */
    ass_SimplePath_be = BeEnc_name_to_timed(be_enc,
                                            ass_SimplePath_node,
                                            0);
    lsNewEnd(assumptions_be, ass_SimplePath_be, LS_NH);
    ass_SimplePath_cnflit = 0;
    /* try to set and append ass_SimplePath_cnflit to assumptions_cnf
       after adding SimplePath constraints */
#endif
  }

  /*
   * Start problem generation:
   */
  previous_k = -1000; /* Start from deep prehistory (at least <= -2) */
  for (current_k = 0; current_k <= max_k && !found_solution; current_k++) {
    int i = 0;
    SatSolverResult satResult;

    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,
              "\nGenerating problem with bound %d, all possible loopbacks...\n",
              current_k);
    }

    /********************************************************************
     *
     * The k-invariant part of the translation
     *
     ********************************************************************/

    /*
     * Formula state vector initialization
     * Initialize new state vectors up to current_k+1
     * Assumes that state vectors are already initialized up to previous_k+1
     */
    for (i = max(previous_k + 2, 0); i <= current_k + 1; i++) {
      sbmc_init_state_vector(be_enc, bltlspec, info_map, sbmc_real_k(i),
                             sbmc_state_vars_get_LastState_var(state_vars),
                             be_LoopExists);
    }

    /*
     * Unroll the model transition relation up to current_k
     * Force it to the fixed frame 0
     * Assumes that it has already been unrolled up to previous_k
     */
    for (i = max(previous_k, 0); i < current_k; i++) {
      be_ptr be_TR = Bmc_Model_GetUnrolling(be_fsm,
                                            sbmc_real_k(i), sbmc_real_k(i+1));

      sbmc_MS_force_true(zolver, be_TR, cnf_alg);

      if (opt_verbose_level_gt(opts, 1)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));

        Logger_log(logger, "Forced T(%d,%d)", i, i+1);

        if (opt_verbose_level_gt(opts, 7)) {
          Logger_log(logger, ": ");
          Be_DumpSexpr(be_mgr, be_TR, errstream);
        }

        Logger_log(logger, "\n");
      }
    }

    /*
     * Unroll the (l_i => (s_{i-1} = s_E)) constraint up to current_k
     * Force it to the fixed frame 0
     * Assumes that it has already been unrolled up to previous_k
     */
    for (i = max(previous_k + 1, 0); i <= current_k; i++) {
      be_ptr be_l_i, be_constraint;

      be_l_i =
        BeEnc_name_to_timed(be_enc,
                            sbmc_state_vars_get_l_var(state_vars),
                            sbmc_real_k(i));

      if (i == 0) {
        /* l_0 <=> FALSE */
        be_constraint = Be_Iff(be_mgr, be_l_i, Be_Falsity(be_mgr));
        if (opt_verbose_level_ge(opts, 6)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, "Created (l_0 <=> false): ");
          Be_DumpSexpr(be_mgr, be_constraint, errstream);
          Logger_log(logger, "\n");
        }
      }
      else {
        be_constraint =
          sbmc_equal_vectors_formula(be_enc,
                                     sbmc_state_vars_get_simple_path_system_vars(state_vars),
                                     sbmc_real_k(i-1), sbmc_E_state());
        be_constraint = Be_Implies(be_mgr, be_l_i, be_constraint);
        if (opt_verbose_level_ge(opts, 6)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, "Created (l_%d => (s_%d = s_E)): ",
                  current_k, current_k-1);
          Be_DumpSexpr(be_mgr, be_constraint, errstream);
          Logger_log(logger, "\n");
        }
      }
      sbmc_MS_force_true(zolver, be_constraint, cnf_alg);
    }

    /*
     * Unroll the (LastState_i <=> False) constraint up to current_k-1
     * Force it to the fixed frame 0
     * Assumes that it has already been unrolled up to previous_k-2
     */
    for (i = max(previous_k, 0); i <= current_k-1; i++) {
      be_ptr be_LastState_i, be_constraint;

      be_LastState_i =
        BeEnc_name_to_timed(be_enc,
                            sbmc_state_vars_get_LastState_var(state_vars),
                            sbmc_real_k(i));

      be_constraint = Be_Iff(be_mgr, be_LastState_i, Be_Falsity(be_mgr));
      if (opt_verbose_level_ge(opts, 6)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "Created (LastState_%d <=> false): ", i);
        Be_DumpSexpr(be_mgr, be_constraint, errstream);
        Logger_log(logger, "\n");
      }
      sbmc_MS_force_true(zolver, be_constraint, cnf_alg);
    }

    /*
     * Define InLoop_i := (InLoop_{i-1} || l_i)
     * and build constraint InLoop_{i-1} => !l_i up to current_k + 1
     * Assumes that they have already been build up to previous_k + 1
     */
    for (i = max(previous_k + 1, 0); i <= current_k+1; i++) {
      be_ptr be_constraint = sbmc_build_InLoop_i(be_enc,
                                                 state_vars,
                                                 InLoop_array, i);
      if (be_constraint) {
        sbmc_MS_force_true(zolver, be_constraint, cnf_alg);
      }
    }

    if (opt_do_optimization) {
      /*
       * Unroll the (l_i => LoopExists) constraint up to current_k
       * Force 'em true in the fixed frame 0
       * Assumes that this has already been done upto previous_k
       */
      for (i = max(previous_k + 1, 0); i <= current_k; i++) {
        be_ptr be_l_i, be_constraint;

        be_l_i =
          BeEnc_name_to_timed(be_enc,
                              sbmc_state_vars_get_l_var(state_vars),
                              sbmc_real_k(i));

        be_constraint = Be_Implies(be_mgr, be_l_i, be_LoopExists);

        sbmc_MS_force_true(zolver, be_constraint, cnf_alg);

        if (opt_verbose_level_ge(opts, 6)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, "Created (l_%d => LoopExists): ",
                  current_k);
          Be_DumpSexpr(be_mgr, be_constraint, errstream);
          Logger_log(logger, "\n");
        }
      }
    }

    /*
     * Unroll future fragment from previous_k+1 to current_k
     * Unroll past fragment   from previous_k+1 to current_k
     */
    {
      lsList new_constraints = sbmc_unroll_invariant(be_enc,
                                                     bltlspec,
                                                     previous_k,
                                                     current_k,
                                                     state_vars,
                                                     InLoop_array,
                                                     info_map,
                                                     be_LoopExists,
                                                     opt_do_optimization);
      sbmc_MS_force_constraint_list(zolver, new_constraints, cnf_alg);
      lsDestroy(new_constraints, NULL);
      new_constraints = (lsList)NULL;
    }

    /*
     * Force negated NNF formula to true if not already done
     */
    if (!formula_forced_to_true) {
      sbmc_node_info* info;
      array_t*         past_array;
      be_ptr           be_f_0_0;

      /* Get info */
      info = sbmc_node_info_assoc_find(info_map, bltlspec);
      nusmv_assert((sbmc_node_info *)NULL != info);

      /* Get past_array */
      nusmv_assert((array_t *)NULL != sbmc_node_info_get_trans_bes(info));
      nusmv_assert(array_n(sbmc_node_info_get_trans_bes(info)) > 0);
      past_array = array_fetch(array_t *,
                               sbmc_node_info_get_trans_bes(info),
                               sbmc_real_k(0));
      nusmv_assert((array_t *)NULL != past_array);

      be_f_0_0 = array_fetch(be_ptr, past_array, 0);
      nusmv_assert((be_ptr)NULL != be_f_0_0);

      sbmc_MS_force_true(zolver, be_f_0_0, cnf_alg);

      if (opt_verbose_level_ge(opts, 6)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "Forced [[f]]_0^0 to true: ");
        Be_DumpSexpr(be_mgr, be_f_0_0, errstream);
        Logger_log(logger, "\n");
      }
      formula_forced_to_true = true;
    }


    if (opt_do_completeness_check) {
      /*
       * Unroll the SimplePath constraints
       */
#ifndef SBMC_HAVENOSIMPLEPATH
      for (i = max(previous_k+1, 0); i <= current_k; i++) {
        lsList new_constraints =
          sbmc_SimplePaths(be_enc, state_vars, InLoop_array, i);
        /* prefix SimplePath constraints with assumption */
        {
          lsList assumed_constraints;
          lsGen iterator;
          be_ptr constraint;

          assumed_constraints = lsCreate();
          nusmv_assert(NULL != assumed_constraints);
          nusmv_assert(assumed_constraints);
          lsForEachItem(new_constraints, iterator, constraint) {
            lsNewEnd(assumed_constraints,
                     Be_Implies(be_mgr, ass_SimplePath_be, constraint),
                     LS_NH);
          }
          lsDestroy(new_constraints, NULL);
          new_constraints = assumed_constraints;
        }
        sbmc_MS_force_constraint_list(zolver, new_constraints, cnf_alg);
        lsDestroy(new_constraints, NULL);
        new_constraints = (lsList)NULL;
      }

      if (0 == ass_SimplePath_cnflit) {
        /* try to set and append ass_SimplePath_cnflit to assumptions_cnf */
        int ass_sp_beind = Be_Var2Index(be_mgr, ass_SimplePath_be);
        nusmv_assert(0 != ass_sp_beind);
        ass_SimplePath_cnflit = Be_BeIndex2CnfLiteral(be_mgr, ass_sp_beind);
        if (0 != ass_SimplePath_cnflit) {
          Slist_push(assumptions_cnf, PTR_FROM_INT(void*,ass_SimplePath_cnflit));
        }
      }
#endif /* SBMC_HAVENOSIMPLEPATH */

#ifdef BENCHMARKING
      StreamMgr_print_error(streams,  ":START:benchmarking Solving\n");
      times(&time_solver_start);
#endif

      /* Actually solve the problem */
      satResult = sbmc_MS_solve_assume(zolver, assumptions_cnf);
      /*
  SatMinisat_solve_permanent_group_assume(SAT_MINISAT(sbmc_MS_get_solver(zolver)),
            assumptions_cnf);
      */
#ifdef BENCHMARKING
      {
        times(&time_temp);
        StreamMgr_print_error(streams,  ":UTIME = %.4f secs.\n",
                time_diff(&time_solver_start, &time_temp));
        StreamMgr_print_error(streams,  ":STOP:benchmarking Solving\n");
        StreamMgr_print_error(streams,
                "completeness, k=%d, solver time=%.4f, cumulative time=%.4f.\n",
                current_k,
                time_diff(&time_solver_start, &time_temp),
                time_diff(&time_bmc_start, &time_temp));
      }
#endif

      /* Processes the result: */
      switch (satResult) {
      case SAT_SOLVER_SATISFIABLE_PROBLEM:
        StreamMgr_print_output(streams,
                "-- could not terminate yet with bound %d",
                current_k);

        if (opt_verbose_level_gt(opts, 2)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, " for ");
          print_spec(Logger_get_ostream(logger),
                     inputprop, get_prop_print_method(opts));
        }

        StreamMgr_print_output(streams,  "\n");
        break;

      case SAT_SOLVER_UNSATISFIABLE_PROBLEM:
        StreamMgr_print_output(streams,  "-- terminating with bound %d.\n", current_k);
        StreamMgr_print_output(streams,  "-- ");
        print_spec(StreamMgr_get_output_ostream(streams),
                   inputprop, get_prop_print_method(opts));
        StreamMgr_print_output(streams,  "  is true\n");
        Prop_set_status(ltlprop, Prop_True);
        StreamMgr_flush_streams(streams);

        found_solution = true;

        /* obtain conflict */
        {
          Slist_ptr conflict_cnf;
#ifndef SBMC_HAVENOSIMPLEPATH
          boolean sp_in_conflict;
#endif
          Siter iterator;
          int conf_cnflit;

          conflict_cnf = sbmc_MS_get_conflicts(zolver);

          nusmv_assert(conflict_cnf);

#ifndef SBMC_HAVENOSIMPLEPATH
          /* is SimplePath part of the conflict? */
          sp_in_conflict = false;
          SLIST_FOREACH(conflict_cnf, iterator) {
            conf_cnflit = PTR_TO_INT(Siter_element(iterator));

            if (ass_SimplePath_cnflit == conf_cnflit) {
              sp_in_conflict = true;
              if (opt_verbose_level_gt(opts, 1)) {
                Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
                Logger_log(logger, "\n\nThe conflict clause contains SimplePath.\n\n");
              }
            }
          }
#endif /* SBMC_HAVENOSIMPLEPATH */

          *conflict = Slist_create();
          nusmv_assert(NULL != *conflict);

#ifndef SBMC_HAVENOSIMPLEPATH
          SLIST_FOREACH(!sp_in_conflict ?
                        conflict_cnf : assumptions_cnf, iterator) { /*1*/
            conf_cnflit = PTR_TO_INT(Siter_element(iterator));

            if (ass_SimplePath_cnflit != conf_cnflit)
#else
          SLIST_FOREACH(conflict_cnf, iterator) { /*1*/
            conf_cnflit = PTR_TO_INT(Siter_element(iterator));
#endif
            { /*2*/
              int conf_belit, conf_beind;
              be_ptr conf_be_timed, conf_be_untimed;
              node_ptr conf;

              conf_belit = Be_CnfLiteral2BeLiteral(be_mgr, conf_cnflit);
              conf_beind = Be_BeLiteral2BeIndex(be_mgr, abs(conf_belit));
              conf_be_timed = Be_Index2Var(be_mgr, conf_beind);
              conf_be_untimed = BeEnc_var_to_untimed(be_enc, conf_be_timed);
              conf = BeEnc_var_to_name(be_enc, conf_be_untimed);
              if (Be_BeLiteral_IsSignPositive(be_mgr, conf_belit)) {
                Slist_push(*conflict, (void*) conf);
              }
              else {
                Slist_push(*conflict, (void*) find_node(nodemgr, NOT, conf, Nil));
              }
            } /*2*/
          } /*1*/
        } /* obtain conflict */
        break;

      case SAT_SOLVER_INTERNAL_ERROR:
        ErrorMgr_internal_error(errmgr, "Sorry, solver answered with a fatal Internal "
                       "Failure during problem solving.\n");
        break;

      case SAT_SOLVER_TIMEOUT:
      case SAT_SOLVER_MEMOUT:
        ErrorMgr_internal_error(errmgr, "Sorry, solver ran out of resources and aborted "
                       "the execution.\n");
        break;

      default:
        ErrorMgr_internal_error(errmgr, "Sbmc_zigzag_incr: Unexpected value in satResult");

      } /* switch */

      if (found_solution) break;

      } /* if (opt_do_completeness_check) */

    /********************************************************************
     *
     * The k-dependent part of the translation
     *
     ********************************************************************/

    if (opt_verbose_level_gt(opts, 1))
      StreamMgr_print_error(streams,  "Adding k-dependent stuff\n");

    /*
     * Go to the volatile frame 1
     */
    sbmc_MS_goto_volatile_group(zolver);

     /*
      * Make the (LastState_k <=> True) constraint
      * Force it to the volatile frame
      */
     {
       be_ptr be_LastState_k, be_constraint;

       be_LastState_k =
         BeEnc_name_to_timed(be_enc,
                             sbmc_state_vars_get_LastState_var(state_vars),
                             sbmc_real_k(current_k));

       be_constraint = Be_Iff(be_mgr, be_LastState_k, Be_Truth(be_mgr));
       if (opt_verbose_level_ge(opts, 6)) {
         Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
         Logger_log(logger, "Created (LastState_%d <=> true): ", i);
         Be_DumpSexpr(be_mgr, be_constraint, errstream);
         Logger_log(logger, "\n");
       }
       sbmc_MS_force_true(zolver, be_constraint, cnf_alg);
     }

    /*
     * Add k-dependent constraints
     */
    {
      lsList new_constraints = sbmc_dependent(be_enc,
                                              bltlspec,
                                              current_k,
                                              state_vars,
                                              InLoop_array,
                                              be_LoopExists,
                                              info_map);
      sbmc_MS_force_constraint_list(zolver, new_constraints, cnf_alg);
      lsDestroy(new_constraints, NULL);
      new_constraints = (lsList)NULL;
    }

    /*
     * add assumptions
     */
    sbmc_MS_force_constraint_list(zolver, assumptions_be, cnf_alg);

#ifdef BENCHMARKING
    StreamMgr_print_error(streams,  ":START:benchmarking Solving\n");
    times(&time_solver_start);
#endif

    /*
     * Actually solve the problem
     */
    satResult = sbmc_MS_solve(zolver);

#ifdef BENCHMARKING
    {
      times(&time_temp);
      StreamMgr_print_error(streams,  ":UTIME = %.4f secs.\n",
              time_diff(&time_solver_start, &time_temp));
      StreamMgr_print_error(streams,  ":STOP:benchmarking Solving\n");
      StreamMgr_print_error(streams,
              "counterexample, k=%d, solver time=%.4f, cumulative time=%.4f.\n",
              current_k,
              time_diff(&time_solver_start, &time_temp),
              time_diff(&time_bmc_start, &time_temp));
    }
#endif

    /* Processes the result: */
    switch (satResult) {
    case SAT_SOLVER_UNSATISFIABLE_PROBLEM: {
      StreamMgr_print_output(streams,
              "-- no counterexample found with bound %d",
              current_k);
      if (opt_verbose_level_gt(opts, 2)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, " for ");
        print_spec(Logger_get_ostream(logger),
                   inputprop, get_prop_print_method(opts));
      }
      StreamMgr_print_output(streams,  "\n");
      break;
    }
    case SAT_SOLVER_SATISFIABLE_PROBLEM:
      StreamMgr_print_output(streams,  "-- terminating with bound %d.\n", current_k);
      StreamMgr_print_output(streams,  "-- ");
      print_spec(StreamMgr_get_output_ostream(streams),
                 inputprop, get_prop_print_method(opts));
      StreamMgr_print_output(streams,  "  is false\n");
      Prop_set_status(ltlprop, Prop_False);
      fflush(outstream);
      fflush(errstream);

      found_solution = true;

      if (opt_counter_examples(opts)) {
        TraceMgr_ptr tm = TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
        BoolSexpFsm_ptr bsexp_fsm; /* needed for trace language */
        Trace_ptr trace = TRACE(NULL);

        bsexp_fsm = Prop_get_bool_sexp_fsm(ltlprop);
        if (BOOL_SEXP_FSM(NULL) == bsexp_fsm) {
          bsexp_fsm = \
            BOOL_SEXP_FSM(NuSMVEnv_get_value(env, ENV_BOOL_FSM));
          BOOL_SEXP_FSM_CHECK_INSTANCE(bsexp_fsm);
        }

        trace = \
          Sbmc_Utils_generate_and_print_cntexample(be_enc, tm, zolver,
                                  sbmc_state_vars_get_l_var(state_vars),
                                  current_k, "BMC Counterexample",
                                  SexpFsm_get_symbols_list(SEXP_FSM(bsexp_fsm)));

        Prop_set_trace(ltlprop, Trace_get_id(trace));
      }
      break;

    case SAT_SOLVER_INTERNAL_ERROR:
      ErrorMgr_internal_error(errmgr, "Sorry, solver answered with a fatal Internal "
                     "Failure during problem solving.\n");
      break;

    case SAT_SOLVER_TIMEOUT:
    case SAT_SOLVER_MEMOUT:
      ErrorMgr_internal_error(errmgr, "Sorry, solver ran out of resources and aborted "
                     "the execution.\n");
      break;

    default:
      ErrorMgr_internal_error(errmgr, "Sbmc_zigzag_incr: Unexpected value in satResult");
    } /* switch */

    if (opt_verbose_level_gt(opts, 2))
      StreamMgr_print_error(streams,  "Deleting k-dependent stuff\n");

    /*
     * Release the volatile frame 1
     */
    sbmc_MS_goto_permanent_group(zolver);

    /*
     * Remember the current_k
     */
    previous_k = current_k;
  }

  Slist_destroy(assumptions_cnf);
  lsDestroy(assumptions_be, NULL);

  if (!found_solution) {
    StreamMgr_print_output(streams,  "-- terminating with bound %d.\n", previous_k);
  }

  /* shuts down the previously set up layer */
  sbmc_bmc_inc_shutdown_layer(be_fsm, bdd_enc, SBMC_INC_LAYER_NAME, info_map);

  /* Destroy InLoop_array */
  array_free(InLoop_array);

  /* Destroy the sat solver instance */
  sbmc_MS_destroy(zolver);

  /* cleanup */
  if (state_vars != (state_vars_struct*) NULL) {
    sbmc_state_vars_destroy(state_vars);
  }

  Prop_Rewriter_update_original_property(rewriter);
  Prop_Rewriter_destroy(rewriter); rewriter = NULL;

  return 0;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Creates and sets up a new layer whose name is provided

  This is a private service of Sbmc_zigzag_incr. Returned
  hash is used by the caller and will be destroyed later by
  bmc_inc_shutdown_layer
*/
static hash_ptr sbmc_bmc_inc_setup_layer(const NuSMVEnv_ptr env,
                                         BeFsm_ptr be_fsm,
                                         BddEnc_ptr bdd_enc,
                                         const char* layer_name,
                                         state_vars_struct* state_vars,
                                         node_ptr bltlspec,
                                         const int opt_do_virtual_unrolling,
                                         const int opt_do_completeness_check,
                                         const int opt_force_state_vars,
                                         node_ptr * ass_SimplePath_node
                                         )
{
  SymbTable_ptr symb_table;
  SymbLayer_ptr ltl_layer;
  BoolEnc_ptr bool_enc;
  BeEnc_ptr be_enc;
  hash_ptr layer_info_map = (hash_ptr)NULL;

  be_enc = BeFsm_get_be_encoding(be_fsm);
  bool_enc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(be_enc));
  symb_table = BaseEnc_get_symb_table(BASE_ENC(be_enc));

  /* Insert the new variables at the bottom of the ordering */
  ltl_layer = SymbTable_create_layer(symb_table, layer_name,
                                     SYMB_LAYER_POS_BOTTOM);

  /* add layer to artifacts class to tell the trace class to ignore it */
  SymbTable_layer_add_to_class(symb_table, layer_name,
                               ARTIFACTS_LAYERS_CLASS);

  /* Allocate the l_i variable */
  sbmc_state_vars_set_l_var(state_vars,
                            sbmc_add_new_state_variable(env, ltl_layer, "#LTL_l"));

  /* Allocate the LoopExists variable */
  sbmc_state_vars_set_LoopExists_var(state_vars,
                                     sbmc_add_new_state_variable(env, ltl_layer, "#LTL_LoopExists"));

  /* Allocate the ass_SimplePath_node variable */
  nusmv_assert(NULL != ass_SimplePath_node);
  *ass_SimplePath_node =
    sbmc_add_new_state_variable(env, ltl_layer, "#LTL_ass_SimplePath");

  /* Allocate the LastState variable */
  sbmc_state_vars_set_LastState_var(state_vars,
                                    sbmc_add_new_state_variable(env, ltl_layer, "#LTL_LastState"));

  /*
   * Associates each subformula node of ltlspec with a sbmc_LTL_info
   * Returns a hash from node_ptr to sbmc_LTL_info*.
   * New state variables named #LTL_t'i' can be allocate to 'layer'.
   * The new state vars are inserted in state_vars_formula_??? appropriately.
   */
  layer_info_map = sbmc_init_LTL_info(env, ltl_layer, bltlspec,
     sbmc_state_vars_get_translation_vars_pd0(state_vars),
     sbmc_state_vars_get_translation_vars_pdx(state_vars),
     sbmc_state_vars_get_translation_vars_aux(state_vars),
     opt_force_state_vars,
     opt_do_virtual_unrolling);

  /*
   * After introducing all new variables, commit ltl_layer
   */
  BaseEnc_commit_layer(BASE_ENC(bool_enc), SymbLayer_get_name(ltl_layer));
  BaseEnc_commit_layer(BASE_ENC(be_enc), SymbLayer_get_name(ltl_layer));
  BaseEnc_commit_layer(BASE_ENC(bdd_enc),
                       SymbLayer_get_name(ltl_layer));

  return layer_info_map;
}

/*!
  \brief Shuts down the layer that had been created by
  sbmc_bmc_inc_setup_layer

  This is a private service of Sbmc_zigzag_incr.
  info_map is the hash previously returned by sbmc_bmc_inc_setup_layer
*/
static void sbmc_bmc_inc_shutdown_layer(BeFsm_ptr be_fsm,
                                        BddEnc_ptr bdd_enc,
                                        const char* layer_name,
                                        hash_ptr layer_info_map)
{
  SymbTable_ptr symb_table;
  BoolEnc_ptr bool_enc;
  BeEnc_ptr be_enc;

  be_enc = BeFsm_get_be_encoding(be_fsm);
  bool_enc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(be_enc));
  symb_table = BaseEnc_get_symb_table(BASE_ENC(be_enc));

  /* Deallocates info_map */
  if (layer_info_map != (hash_ptr) NULL) {
    sbmc_node_info_assoc_free(&layer_info_map);
    nusmv_assert((hash_ptr)NULL == layer_info_map);
  }

  /* remove the layer from the encoders */
  BaseEnc_remove_layer(BASE_ENC(bdd_enc), layer_name);
  BaseEnc_remove_layer(BASE_ENC(be_enc), layer_name);
  BaseEnc_remove_layer(BASE_ENC(bool_enc), layer_name);

  /* Removes extra encoding layer */
  if (SymbTable_layer_class_exists(symb_table, ARTIFACTS_LAYERS_CLASS)) {
    SymbTable_layer_remove_from_class(symb_table, layer_name,
                                      ARTIFACTS_LAYERS_CLASS);
  }

  SymbTable_remove_layer(symb_table,
                         SymbTable_get_layer(symb_table, layer_name));
}
