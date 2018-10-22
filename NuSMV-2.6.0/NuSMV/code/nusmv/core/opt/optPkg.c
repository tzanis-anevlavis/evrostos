/* ---------------------------------------------------------------------------


   This file is part of the ``opt'' package.
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
  \brief This module contains the package handling functions

  This module contains the package handling functions

*/

#include "nusmv/core/opt/optInt.h"
#include "nusmv/core/opt/optPkg.h"
#include "nusmv/core/cinit/cinit.h"

#include "nusmv/core/utils/error.h"

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
static boolean opt_check_shown_states(OptsHandler_ptr opts,
                                      const char* val, void* arg);
static boolean opt_check_word_format(OptsHandler_ptr opts,
                                     const char* val, void* arg);
/* Options check functions */
static boolean opt_check_sat_solver(OptsHandler_ptr opts,
                                    const char* val, void* arg);
static void* opt_get_sat_solver(OptsHandler_ptr opts,
                                const char* val, void* arg);


/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Opt_Pkg_init(NuSMVEnv_ptr const env)
{
  OptsHandler_ptr opts = OptsHandler_create();
  boolean res = true;
  char* path;
  char* lib_name;

  NuSMVEnv_set_value(env, ENV_OPTS_HANDLER, opts);

  lib_name = CInit_NuSMVObtainLibrary();
  path = ALLOC(char, strlen(lib_name) + 3);
  sprintf(path, ".:%s", lib_name);

  OptsHandler_register_generic_option(opts, "open_path", path, true);
  FREE(lib_name);
  FREE(path);

  res = OptsHandler_register_generic_option(opts, PROGRAM_NAME,
                                            DEFAULT_PGM_NAME, false);
  nusmv_assert(res);

  res = OptsHandler_register_generic_option(opts, PROGRAM_PATH,
                                            DEFAULT_PGM_PATH, false);
  nusmv_assert(res);

  res = OptsHandler_register_generic_option(opts, INPUT_FILE,
                                            DEFAULT_INPUT_FILE,
                                            true);
  nusmv_assert(res);

  res = OptsHandler_register_generic_option(opts, PP_CPP_PATH,
                                            DEFAULT_PP_CPP_PATH,
                                            true);
  nusmv_assert(res);

  res = OptsHandler_add_option_trigger(opts, PP_CPP_PATH,
                                       opt_pp_cpp_path_trigger, env);

  nusmv_assert(res);

  res = OptsHandler_register_generic_option(opts, PP_M4_PATH,
                                            DEFAULT_PP_M4_PATH,
                                            true);
  nusmv_assert(res);

  res = OptsHandler_add_option_trigger(opts, PP_M4_PATH,
                                       opt_pp_m4_path_trigger, env);

  nusmv_assert(res);

  init_preprocessors(env);

  res = OptsHandler_register_generic_option(opts, SCRIPT_FILE,
                                            NULL, false);
  nusmv_assert(res);

  res = OptsHandler_add_option_trigger(opts, SCRIPT_FILE,
                                       opt_script_file_trigger, env);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, QUIET_MODE,
                                         false, false);
  nusmv_assert(res);

  res = OptsHandler_register_generic_option(opts, INPUT_ORDER_FILE,
                                            DEFAULT_INPUT_ORDER_FILE, true);
  nusmv_assert(res);

  res = OptsHandler_register_generic_option(opts, OUTPUT_ORDER_FILE,
                                            DEFAULT_OUTPUT_ORDER_FILE, true);
  nusmv_assert(res);

  res = OptsHandler_register_generic_option(opts, TRANS_ORDER_FILE,
                                            DEFAULT_TRANS_ORDER_FILE, true);
  nusmv_assert(res);

  res = OptsHandler_add_option_trigger(opts, TRANS_ORDER_FILE,
                                       opt_trans_order_file_trigger, env);
  nusmv_assert(res);

  res = OptsHandler_register_generic_option(opts, OUTPUT_FLATTEN_MODEL_FILE,
                                            (char*)NULL, true);
  nusmv_assert(res);

  res = OptsHandler_register_generic_option(opts, OUTPUT_BOOLEAN_MODEL_FILE,
                                            (char*)NULL, true);
  nusmv_assert(res);

  {
    char def[20];
    int c = snprintf(def, sizeof(def), "10");
    SNPRINTF_CHECK(c, sizeof(def));
    res = OptsHandler_register_option(opts, OUTPUT_WORD_FORMAT, def,
                                      (Opts_CheckFnType)opt_check_word_format,
                                      (Opts_ReturnFnType)opt_get_integer,
                                      true, INTEGER_OPTION, env);
    nusmv_assert(res);
  }

  res = OptsHandler_register_bool_option(opts, BACKWARD_COMPATIBILITY,
                                         DEFAULT_BACKWARD_COMPATIBILITY, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, TYPE_CHECKING_WARNING_ON,
                                         DEFAULT_TYPE_CHECKING_WARNING_ON, true);
  nusmv_assert(res);

  res = OptsHandler_register_int_option(opts, VERBOSE_LEVEL, 0, true);
  nusmv_assert(res);

  {
    res = OptsHandler_register_int_option(opts, DEFAULT_TRACE_PLUGIN, 0, true);
    nusmv_assert(res);

    res = OptsHandler_add_option_trigger(opts, DEFAULT_TRACE_PLUGIN,
                                         opt_trace_plugin_trigger, env);
    nusmv_assert(res);
  }

  res = OptsHandler_register_generic_option(opts, RUN_CPP, "", false);
  nusmv_assert(res);

  res = OptsHandler_add_option_trigger(opts, RUN_CPP,
                                       opt_run_cpp_trigger, env);
  nusmv_assert(res);

  res = OptsHandler_register_generic_option(opts, PP_LIST, "", true);
  nusmv_assert(res);

  res = OptsHandler_add_option_trigger(opts, PP_LIST, opt_pp_list_trigger, env);
  nusmv_assert(res);

  {
    char def[20];
    int chars = snprintf(def, 20, "%d", DEFAULT_SHOWN_STATES);
    SNPRINTF_CHECK(chars, 20);

    res = OptsHandler_register_option(opts, SHOWN_STATES, def,
                                      (Opts_CheckFnType)opt_check_shown_states,
                                      (Opts_ReturnFnType)opt_get_integer,
                                      true, INTEGER_OPTION, env);
    nusmv_assert(res);
  }

  res = OptsHandler_register_bool_option(opts, OPT_CHECK_FSM, false, true);
  nusmv_assert(res);

  /* Those below are batch related */
  res = OptsHandler_register_bool_option(opts, IGNORE_SPEC, false, false);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, IGNORE_COMPUTE, false, false);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, IGNORE_LTLSPEC, false, false);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, IGNORE_PSLSPEC, false, false);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, IGNORE_INVAR, false, false);
  nusmv_assert(res);
  /* Those above were batch related */

  res = OptsHandler_register_bool_option(opts, FORWARD_SEARCH, true, true);
  nusmv_assert(res);

  /* if this option is used, also their use is enabled */
  res = OptsHandler_add_option_trigger(opts, FORWARD_SEARCH,
                                       opt_set_reachable_states_trigger, env);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, LTL_TABLEAU_FORWARD_SEARCH,
                                         false, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, USE_REACHABLE_STATES,
                                         true, false);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, PRINT_REACHABLE, false, false);
  nusmv_assert(res);

  /* if this option is used, also their use is enabled */
  res = OptsHandler_add_option_trigger(opts, PRINT_REACHABLE,
                                       opt_set_reachable_states_trigger, env);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, ENABLE_REORDER, false, false);
  nusmv_assert(res);

  res = OptsHandler_add_option_trigger(opts, ENABLE_REORDER,
                                       opt_dynamic_reorder_trigger, env);
  nusmv_assert(res);

  {
    Opts_EnumRec reorders[20] = {
      {DynOrderTypeConvertToString(REORDER_RANDOM), REORDER_RANDOM},
      {DynOrderTypeConvertToString(REORDER_RANDOM_PIVOT), REORDER_RANDOM_PIVOT},
      {DynOrderTypeConvertToString(REORDER_SIFT), REORDER_SIFT},
      {DynOrderTypeConvertToString(REORDER_GROUP_SIFT), REORDER_GROUP_SIFT},
      {DynOrderTypeConvertToString(REORDER_SIFT_CONV), REORDER_SIFT_CONV},
      {DynOrderTypeConvertToString(REORDER_SYMM_SIFT), REORDER_SYMM_SIFT},
      {DynOrderTypeConvertToString(REORDER_SYMM_SIFT_CONV), REORDER_SYMM_SIFT_CONV},
      {DynOrderTypeConvertToString(REORDER_WINDOW2), REORDER_WINDOW2},
      {DynOrderTypeConvertToString(REORDER_WINDOW3), REORDER_WINDOW3},
      {DynOrderTypeConvertToString(REORDER_WINDOW4), REORDER_WINDOW3},
      {DynOrderTypeConvertToString(REORDER_WINDOW2_CONV), REORDER_WINDOW2_CONV},
      {DynOrderTypeConvertToString(REORDER_WINDOW3_CONV), REORDER_WINDOW3_CONV},
      {DynOrderTypeConvertToString(REORDER_WINDOW4_CONV), REORDER_WINDOW4_CONV},
      {DynOrderTypeConvertToString(REORDER_GROUP_SIFT_CONV), REORDER_GROUP_SIFT_CONV},
      {DynOrderTypeConvertToString(REORDER_ANNEALING), REORDER_ANNEALING},
      {DynOrderTypeConvertToString(REORDER_GENETIC), REORDER_GENETIC},
      {DynOrderTypeConvertToString(REORDER_EXACT), REORDER_EXACT},
      {DynOrderTypeConvertToString(REORDER_LINEAR), REORDER_LINEAR},
      {DynOrderTypeConvertToString(REORDER_LINEAR_CONV), REORDER_LINEAR_CONV},
      {DynOrderTypeConvertToString(REORDER_SAME), REORDER_SAME},
    };

    const char* def = DynOrderTypeConvertToString(DEFAULT_REORDER);
    res = OptsHandler_register_enum_option(opts, REORDER_METHOD, def,
                                           reorders, 20, true);
    nusmv_assert(res);

    res = OptsHandler_add_option_trigger(opts, REORDER_METHOD,
                                         opt_reorder_method_trigger, env);
    nusmv_assert(res);
  }

  res = OptsHandler_register_bool_option(opts, DYNAMIC_REORDER, false, true);
  nusmv_assert(res);

  res = OptsHandler_add_option_trigger(opts, DYNAMIC_REORDER,
                                       opt_dynamic_reorder_trigger, env);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, ENABLE_SEXP2BDD_CACHING, true, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, BATCH, true, false);
  nusmv_assert(res);

  {
    Opts_EnumRec pm[3] = {
      {TRANS_TYPE_MONOLITHIC_STRING, TRANS_TYPE_MONOLITHIC},
      {TRANS_TYPE_THRESHOLD_STRING, TRANS_TYPE_THRESHOLD},
      {TRANS_TYPE_IWLS95_STRING, TRANS_TYPE_IWLS95}
    };

    res = OptsHandler_register_enum_option(opts, PARTITION_METHOD,
                                           TRANS_TYPE_THRESHOLD_STRING,
                                           pm, 3, true);
    nusmv_assert(res);
  }

  res = OptsHandler_register_int_option(opts, CONJ_PART_THRESHOLD,
                                        DEFAULT_CONJ_PART_THRESHOLD, true);
  nusmv_assert(res);

  res = OptsHandler_register_int_option(opts, IMAGE_CLUSTER_SIZE,
                                        DEFAULT_IMAGE_CLUSTER_SIZE, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, IWLS95_PREORDER, false, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, AFFINITY_CLUSTERING, true, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, APPEND_CLUSTERS, true, false);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, IGNORE_INIT_FILE, false, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, AG_ONLY_SEARCH, false, true);

  res = OptsHandler_register_bool_option(opts, CONE_OF_INFLUENCE,
                                         false, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, LIST_PROPERTIES, false, false);
  nusmv_assert(res);

  {
    Opts_EnumRec pm[4] = {
      {"name", PROP_PRINT_FMT_NAME},
      {"index", PROP_PRINT_FMT_INDEX},
      {"truncated", PROP_PRINT_FMT_FORMULA_TRUNC},
      {"formula", PROP_PRINT_FMT_FORMULA}
    };

    res = OptsHandler_register_enum_option(opts, PROP_PRINT_METHOD,
                                           "formula", pm, 4, true);
    nusmv_assert(res);
  }

  res = OptsHandler_register_int_option(opts, PROP_NO, -1, false);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, USE_FAIR_STATES, true, false);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, COUNTER_EXAMPLES, true, true);
  nusmv_assert(res);

  res = OptsHandler_register_generic_option(opts, TRACES_HIDING_PREFIX,
                                            DEFAULT_TRACES_HIDING_PREFIX,
                                            true);
  nusmv_assert(res);

  /* This is an internal option not visible at user level */
  res = OptsHandler_register_bool_option(opts, BDD_ENCODE_WORD_BITS,
                                         DEFAULT_BDD_ENCODE_WORD_BITS, false);

#if NUSMV_HAVE_REGEX_H
  res = OptsHandler_register_generic_option(opts,
                                            TRACES_REGEXP, (char*)NULL, true);
  nusmv_assert(res);

  res = OptsHandler_add_option_trigger(opts, TRACES_REGEXP,
                                       opt_traces_regexp_trigger, env);
  nusmv_assert(res);

#endif

  res = OptsHandler_register_bool_option(opts, ON_FAILURE_SCRIPT_QUITS,
                                         false, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, WRITE_ORDER_DUMPS_BITS, true, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, USE_ANSI_C_DIV_OP, true, false);
  nusmv_assert(res);

  {
    Opts_EnumRec vot[6] = {
      {(char*)Enc_vars_ord_to_string(VARS_ORD_INPUTS_BEFORE),
       VARS_ORD_INPUTS_BEFORE},
      {(char*)Enc_vars_ord_to_string(VARS_ORD_INPUTS_AFTER),
       VARS_ORD_INPUTS_AFTER},
      {(char*)Enc_vars_ord_to_string(VARS_ORD_TOPOLOGICAL),
       VARS_ORD_TOPOLOGICAL},
      {(char*)Enc_vars_ord_to_string(VARS_ORD_INPUTS_BEFORE_BI),
       VARS_ORD_INPUTS_BEFORE_BI},
      {(char*)Enc_vars_ord_to_string(VARS_ORD_INPUTS_AFTER_BI),
       VARS_ORD_INPUTS_AFTER_BI},
      {(char*)Enc_vars_ord_to_string(VARS_ORD_TOPOLOGICAL_BI),
       VARS_ORD_TOPOLOGICAL_BI},
    };
    const char* def = Enc_vars_ord_to_string(VARS_ORD_INPUTS_BEFORE_BI);
    res = OptsHandler_register_enum_option(opts, VARS_ORD_TYPE,
                                           def, vot, 6, true);
    nusmv_assert(res);
  }

  {
    Opts_EnumRec bdd[2] = {
      {(char*)Enc_bdd_static_order_heuristics_to_string(
                                                        BDD_STATIC_ORDER_HEURISTICS_BASIC),
       BDD_STATIC_ORDER_HEURISTICS_BASIC},
      {(char*)Enc_bdd_static_order_heuristics_to_string(
                                                        BDD_STATIC_ORDER_HEURISTICS_NONE),
       BDD_STATIC_ORDER_HEURISTICS_NONE}
    };
    const char* def = Enc_bdd_static_order_heuristics_to_string(
                                                                BDD_STATIC_ORDER_HEURISTICS_BASIC);
    res = OptsHandler_register_enum_option(opts, BDD_STATIC_ORDER_HEURISTICS,
                                           def, bdd, 2, true);
    nusmv_assert(res);
  }

  res = OptsHandler_register_bool_option(opts, SYMB_INLINING,
                                         false, true);
  nusmv_assert(res);

#if NUSMV_HAVE_SAT_SOLVER
  {
    Opts_EnumRec rbc[2] = {
      {RBC_SHERIDAN_CONVERSION_NAME, RBC_SHERIDAN_CONVERSION},
      {RBC_TSEITIN_CONVERSION_NAME, RBC_TSEITIN_CONVERSION}
    };

    res = OptsHandler_register_enum_option(opts, RBC_CNF_ALGORITHM,
                                           RBC_SHERIDAN_CONVERSION_NAME,
                                           rbc, 2, true);
    nusmv_assert(res);
  }

  res = OptsHandler_register_bool_option(opts, RBC_INLINING,
                                         true, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, RBC_INLINING_LAZY,
                                         false, false);
  nusmv_assert(res);

  res = OptsHandler_add_option_trigger(opts, RBC_INLINING_LAZY,
                                       opt_rbc_inlining_lazy_trigger, env);
  nusmv_assert(res);

#endif

  res = OptsHandler_register_option(opts, A_SAT_SOLVER,
                                    DEFAULT_SAT_SOLVER,
                                    (Opts_CheckFnType)opt_check_sat_solver,
                                    (Opts_ReturnFnType)opt_get_sat_solver,
                                    true, GENERIC_OPTION, env);
  nusmv_assert(res);


  res = OptsHandler_register_bool_option(opts, SHOW_DEFINES_IN_TRACES,
                                         DEFAULT_SHOW_DEFINES_IN_TRACES, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, SHOW_DEFINES_WITH_NEXT,
                                         DEFAULT_SHOW_DEFINES_WITH_NEXT, true);

  nusmv_assert(res);

  {
#if NUSMV_HAVE_SAT_SOLVER
    Opts_EnumRec strategies[4] = {
#else
    Opts_EnumRec strategies[3] = {
#endif
      {(char*)opt_check_invar_strategy_to_string(FORWARD), FORWARD},
      {(char*)opt_check_invar_strategy_to_string(BACKWARD), BACKWARD},
      {(char*)opt_check_invar_strategy_to_string(FORWARD_BACKWARD),
       FORWARD_BACKWARD}
#if NUSMV_HAVE_SAT_SOLVER
      ,{(char*)opt_check_invar_strategy_to_string(BDD_BMC), BDD_BMC}
#endif
    };
    const char* def =
      opt_check_invar_strategy_to_string(DEFAULT_INVAR_CHECK_STRATEGY);

#if NUSMV_HAVE_SAT_SOLVER
    res = OptsHandler_register_enum_option(opts, INVAR_CHECK_STRATEGY,
                                           def, strategies, 4, true);
#else
    res = OptsHandler_register_enum_option(opts, INVAR_CHECK_STRATEGY,
                                           def, strategies, 3, true);
#endif
    nusmv_assert(res);
  }

  {
    Opts_EnumRec heuristics[2] = {
      {(char*)opt_check_invar_fb_heuristic_to_string(ZIGZAG_HEURISTIC),
       ZIGZAG_HEURISTIC},
      {(char*)opt_check_invar_fb_heuristic_to_string(SMALLEST_BDD_HEURISTIC),
       SMALLEST_BDD_HEURISTIC}
    };
    const char* def =
      opt_check_invar_fb_heuristic_to_string(
                                             DEFAULT_FORWARD_BACKWARD_ANALYSIS_HEURISTIC);

    res = OptsHandler_register_enum_option(opts, CHECK_INVAR_FB_HEURISTIC,
                                           def, heuristics, 2, true);
    nusmv_assert(res);
  }

  {
    Opts_EnumRec heuristics[2] = {
      {(char*)opt_check_invar_bddbmc_heuristic_to_string(STEPS_HEURISTIC),
       STEPS_HEURISTIC},
      {(char*)opt_check_invar_bddbmc_heuristic_to_string(SIZE_HEURISTIC),
       SIZE_HEURISTIC}
    };
    const char* def =
      opt_check_invar_bddbmc_heuristic_to_string(DEFAULT_BDD2BMC_HEURISTIC);

    res = OptsHandler_register_enum_option(opts, CHECK_INVAR_BDDBMC_HEURISTIC,
                                           def, heuristics, 2, true);
    nusmv_assert(res);
  }

  res = OptsHandler_register_int_option(opts,
                                        CHECK_INVAR_BDDBMC_HEURISTIC_THRESHOLD,
                                        DEFAULT_BDD2BMC_HEURISTIC_THRESHOLD, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts,
                                         DAGGIFIER_ENABLED,
                                         DEFAULT_DAGGIFIER_ENABLED, true);
  nusmv_assert(res);

  res = OptsHandler_register_int_option(opts,
                                        DAGGIFIER_COUNTER_THRESHOLD,
                                        DEFAULT_DAGGIFIER_COUNTER_THS, true);
  nusmv_assert(res);

  res = OptsHandler_register_int_option(opts,
                                        DAGGIFIER_DEPTH_THRESHOLD,
                                        DEFAULT_DAGGIFIER_DEPTH_THS, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts,
                                         DAGGIFIER_STATISTICS, false, true);
  nusmv_assert(res);

  {
    Opts_EnumRec oreg[2] = {
      {(char*)Bdd_BddOregJusticeEmptinessBddAlgorithmType_to_string(
                             BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_BWD),
       BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_BWD},
      {(char*)Bdd_BddOregJusticeEmptinessBddAlgorithmType_to_string(
                             BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD),
       BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD}
    };
    const char* def = Bdd_BddOregJusticeEmptinessBddAlgorithmType_to_string(
                                DEFAULT_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM);

    res = OptsHandler_register_enum_option(opts,
                                           OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM,
                                           def, oreg, 2, true);
    nusmv_assert(res);
  }

  res = OptsHandler_register_bool_option(opts,
                                         USE_COI_SIZE_SORTING,
                                         DEFAULT_USE_COI_SIZE_SORTING, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, DISABLE_SYNTACTIC_CHECKS,
                                         false, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, KEEP_SINGLE_VALUE_VARS,
                                         false, true);
  nusmv_assert(res);

  res = OptsHandler_register_int_option(opts, DEFAULT_SIMULATION_STEPS,
                                        10, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, LTL2SMV_SINGLE_JUSTICE,
                                         DEFAULT_LTL2SMV_SINGLE_JUSTICE, true);
  nusmv_assert(res);

  res = OptsHandler_register_bool_option(opts, BOOLEAN_CONVERSION_USES_PREDICATE_NORMALIZATION,
                                         DEFAULT_BOOLEAN_CONVERSION_USES_PREDICATE_NORMALIZATION, true);
  nusmv_assert(res);

}

void Opt_Pkg_deinit(NuSMVEnv_ptr const env)
{
  quit_preprocessors(env);
  OptsHandler_destroy(
      OPTS_HANDLER(NuSMVEnv_remove_value(env, ENV_OPTS_HANDLER)));
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Check function for the output word format

  Check function for the output word format
*/
static boolean opt_check_word_format(OptsHandler_ptr opts,
                                     const char* val, void* arg)
{
  const NuSMVEnv_ptr env = NUSMV_ENV(arg);
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  void * tmp = opt_get_integer(opts, val, arg);
  int new_format = PTR_TO_INT(tmp);

  if (tmp != OPTS_VALUE_ERROR) {
    if ((new_format == 2) || (new_format == 8) ||
        (new_format == 10) || (new_format == 16)) {
      return true;
    }
    else {
      StreamMgr_print_error(streams,  "Valid values are 2, 8, 10 and 16.\n");
    }
  }
  return false;
}

/*!
  \brief Check function for the number of shown states

  Check function for the number of shown states
*/
static boolean opt_check_shown_states(OptsHandler_ptr opts,
                                      const char* val, void* arg)
{
  const NuSMVEnv_ptr env = NUSMV_ENV(arg);
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  void * tmp = opt_get_integer(opts, val, arg);
  int int_val = PTR_TO_INT(tmp);

  if (tmp != OPTS_VALUE_ERROR) {
    if ((int_val < 1) || (int_val > MAX_SHOWN_STATES)) {
      StreamMgr_print_error(streams,  "Number must be 1 <= n <= %d\n", MAX_SHOWN_STATES);
    }
    else {
      return true;
    }
  }

  return false;
}

/*!
  \brief Check function for the sat_solver option.

  Check function for the sat_solver option.
*/
static boolean opt_check_sat_solver(OptsHandler_ptr opts,
                                    const char* val, void* arg)
{
  const NuSMVEnv_ptr env = NUSMV_ENV(arg);
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  boolean result = false;
  const char* name = Sat_NormalizeSatSolverName(val);

  result = ((const char*)NULL != name);

  if (!result) {
    FILE* err = StreamMgr_get_error_stream(streams);
    Sat_PrintAvailableSolvers(err);
  }

  return result;
}

/*!
  \brief get function for the sat_solver option.

  get function for the sat_solver option.
*/
static void* opt_get_sat_solver(OptsHandler_ptr opts,
                                const char* val, void* arg)
{
  return (void*)Sat_NormalizeSatSolverName(val);
}
