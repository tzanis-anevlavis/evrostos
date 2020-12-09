/* ---------------------------------------------------------------------------


   This file is part of the ``opt'' package.
   %COPYRIGHT%


-----------------------------------------------------------------------------*/

/*!
  \author Michele Dorigatti
  \brief Main functions for the opt package

  Main functions for the opt package

*/

#include "nusmv/core/opt/optInt.h"
#include "nusmv/core/opt/opt.h"

#include "nusmv/core/trans/trans.h"  /* to access TransType interface */

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/bmc/bmcUtils.h"
#include "nusmv/core/trace/pkg_trace.h"  /* to access TransType interface */
#include "nusmv/core/fsm/bdd/bdd.h" /* for BddOregJusticeEmptinessBddAlgorithmType */
#include "nusmv/core/enc/enc.h"

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
static char* remove_non_existant_pps(OptsHandler_ptr opts,
                                     const char* pp_list,
                                     const NuSMVEnv_ptr env);
static void check_user_preprocessor(const NuSMVEnv_ptr env,
                                    Trigger_Action action,
                                    const char* value);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/
/* PROGRAM_NAME */
void set_pgm_name(OptsHandler_ptr opt, char* str)
{
  boolean res = OptsHandler_set_option_value(opt, PROGRAM_NAME, str);
  nusmv_assert(res);
}
void reset_pgm_name(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, PROGRAM_NAME);
  nusmv_assert(res);
}

char* get_pgm_name(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, PROGRAM_NAME);
}

/******************************************************************************/
/* SCRIPT_FILE */
void set_script_file(OptsHandler_ptr opt, char* str)
{
  boolean res = OptsHandler_set_option_value(opt, SCRIPT_FILE, str);
  nusmv_assert(res);
}
void reset_script_file(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, SCRIPT_FILE);
  nusmv_assert(res);
}

char* get_script_file(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, SCRIPT_FILE);
}

/******************************************************************************/
/* PROGRAM PATH */
void set_pgm_path(OptsHandler_ptr opt, char* str)
{
  boolean res = OptsHandler_set_option_value(opt, PROGRAM_PATH, str);
  nusmv_assert(res);
}
void reset_pgm_path(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, PROGRAM_PATH);
  nusmv_assert(res);
}
char* get_pgm_path(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, PROGRAM_PATH);
}

/******************************************************************************/
/* INPUT_FILE */
void set_input_file(OptsHandler_ptr opt, const char* str)
{
  boolean res = OptsHandler_set_option_value(opt, INPUT_FILE, str);
  nusmv_assert(res);
}
void reset_input_file(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, INPUT_FILE);
  nusmv_assert(res);
}
char* get_input_file(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, INPUT_FILE);
}

/******************************************************************************/
/* INPUT_ORDER_FILE */
void set_input_order_file(OptsHandler_ptr opt, char* str)
{
  boolean res = OptsHandler_set_option_value(opt, INPUT_ORDER_FILE, str);
  nusmv_assert(res);
}
void reset_input_order_file(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, INPUT_ORDER_FILE);
  nusmv_assert(res);
}
char* get_input_order_file(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, INPUT_ORDER_FILE);
}

/******************************************************************************/
/* OUTPUT_ORDER_FILE */

void set_output_order_file(OptsHandler_ptr opt, char* str)
{
  boolean res = OptsHandler_set_option_value(opt, OUTPUT_ORDER_FILE, str);
  nusmv_assert(res);
}
void reset_output_order_file(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, OUTPUT_ORDER_FILE);
  nusmv_assert(res);
}
char* get_output_order_file(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, OUTPUT_ORDER_FILE);
}

boolean is_default_order_file(OptsHandler_ptr opt)
{
  char* oof;
  oof = get_output_order_file(opt);

  if (oof == NIL(char)) {
    return DEFAULT_OUTPUT_ORDER_FILE == NIL(char);
  }
  if (DEFAULT_OUTPUT_ORDER_FILE == NIL(char)) return false;
  return((strcmp(oof, DEFAULT_OUTPUT_ORDER_FILE) == 0));
}

/******************************************************************************/
/* TRANS_ORDER_FILE */
void set_trans_order_file(OptsHandler_ptr opt, char* str)
{
  boolean res = OptsHandler_set_option_value(opt, TRANS_ORDER_FILE, str);
  nusmv_assert(res);
}

void reset_trans_order_file(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, TRANS_ORDER_FILE);
  nusmv_assert(res);
}

char* get_trans_order_file(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, TRANS_ORDER_FILE);
}

/* WARNING [MD] very bad name */
boolean opt_trans_order_file(OptsHandler_ptr opt)
{
  return (get_trans_order_file(opt) != NULL);
}

/******************************************************************************/
/* OUTPUT_FLATTEN_MODEL */

void set_output_flatten_model_file(OptsHandler_ptr opt, char* str)
{
  boolean res = OptsHandler_set_option_value(opt, OUTPUT_FLATTEN_MODEL_FILE, str);
  nusmv_assert(res);
}
void reset_output_flatten_model_file(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, OUTPUT_FLATTEN_MODEL_FILE);
  nusmv_assert(res);
}
char* get_output_flatten_model_file(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, OUTPUT_FLATTEN_MODEL_FILE);
}

/******************************************************************************/
/* OUTPUT_BOOLEAN_MODEL */

void set_output_boolean_model_file(OptsHandler_ptr opt, char* str)
{
  boolean res = OptsHandler_set_option_value(opt, OUTPUT_BOOLEAN_MODEL_FILE, str);
  nusmv_assert(res);
}
void reset_output_boolean_model_file(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, OUTPUT_BOOLEAN_MODEL_FILE);
  nusmv_assert(res);
}
char* get_output_boolean_model_file(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, OUTPUT_BOOLEAN_MODEL_FILE);
}

/******************************************************************************/
/* OUTPUT_WORD_FORMAT */
void set_output_word_format(OptsHandler_ptr opt, int i)
{
  boolean res = OptsHandler_set_int_option_value(opt, OUTPUT_WORD_FORMAT, i);
  nusmv_assert(res);
}

int get_output_word_format(OptsHandler_ptr opt)
{
  return OptsHandler_get_int_option_value(opt, OUTPUT_WORD_FORMAT);
}

/* WARNING [MD] Missing reset function */

/******************************************************************************/
/* BACKWARD_COMP */
void set_backward_comp(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  BACKWARD_COMPATIBILITY, true);
  nusmv_assert(res);
}

void unset_backward_comp(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  BACKWARD_COMPATIBILITY, false);
  nusmv_assert(res);
}

boolean opt_backward_comp(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, BACKWARD_COMPATIBILITY);
}

/******************************************************************************/
/* TYPE_CHECKING_WARNING_ON */
void set_type_checking_warning_on(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, TYPE_CHECKING_WARNING_ON, true);
  nusmv_assert(res);
}

void unset_type_checking_warning_on(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, TYPE_CHECKING_WARNING_ON, false);
  nusmv_assert(res);
}

boolean opt_type_checking_warning_on(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, TYPE_CHECKING_WARNING_ON);
}

/******************************************************************************/
/* VERBOSE_LEVEL */
void set_verbose_level(OptsHandler_ptr opt, int level)
{
  boolean res = OptsHandler_set_int_option_value(opt, VERBOSE_LEVEL, level);
  nusmv_assert(res);
}

int get_verbose_level(OptsHandler_ptr opt)
{
  return OptsHandler_get_int_option_value(opt, VERBOSE_LEVEL);
}

boolean opt_verbose_level_eq(OptsHandler_ptr opt, int level)
{
  return((get_verbose_level(opt) == level));
}

boolean opt_verbose_level_gt(OptsHandler_ptr opt, int level)
{
  return((get_verbose_level(opt) > level));
}

boolean opt_verbose_level_ge(OptsHandler_ptr opt, int level)
{
  return((get_verbose_level(opt) >= level));
}

boolean opt_verbose_level_lt(OptsHandler_ptr opt, int level)
{
  return((get_verbose_level(opt) < level));
}

boolean opt_verbose_level_le(OptsHandler_ptr opt, int level)
{
  return((get_verbose_level(opt) <= level));
}

/******************************************************************************/
/* PP_LIST */
void set_pp_list(OptsHandler_ptr opt, char* pp_list, const NuSMVEnv_ptr env)
{
  char* new_pp_list;
  boolean res;

  if (strcmp(pp_list,"") != 0) {
    new_pp_list = remove_non_existant_pps(opt, pp_list, env);
  }
  else {
    new_pp_list = util_strsav("");
  }

  res = OptsHandler_set_option_value(opt, PP_LIST, new_pp_list);
  nusmv_assert(res);
}

char* get_pp_list(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, PP_LIST);
}

/******************************************************************************/
/* SHOWN_STATES_LEVEL */
void set_shown_states_level(OptsHandler_ptr opt, int level)
{
  boolean res = OptsHandler_set_int_option_value(opt, SHOWN_STATES, level);
  nusmv_assert(res);
}

/* WARNING [MD] VERY BAD NAME, THIS IS A GETTER! */
int opt_shown_states_level(OptsHandler_ptr opt)
{
  return OptsHandler_get_int_option_value(opt, SHOWN_STATES);
}

/******************************************************************************/
/* IGNORE_SPEC */
void set_ignore_spec(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, IGNORE_SPEC, true);
  nusmv_assert(res);
}

void unset_ignore_spec(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, IGNORE_SPEC, false);
  nusmv_assert(res);
}

boolean opt_ignore_spec(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, IGNORE_SPEC);
}

/******************************************************************************/
/* IGNORE_COMPUTE */
void set_ignore_compute(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, IGNORE_COMPUTE, true);
  nusmv_assert(res);
}

void unset_ignore_compute(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, IGNORE_COMPUTE, false);
  nusmv_assert(res);
}

boolean opt_ignore_compute(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, IGNORE_COMPUTE);
}

/******************************************************************************/
/* IGNORE_LTLSPEC */
void set_ignore_ltlspec(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, IGNORE_LTLSPEC, true);
  nusmv_assert(res);
}
void unset_ignore_ltlspec(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, IGNORE_LTLSPEC, false);
  nusmv_assert(res);
}
boolean opt_ignore_ltlspec(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, IGNORE_LTLSPEC);
}

/******************************************************************************/
/* IGNORE_PSLSPEC */
void set_ignore_pslspec(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, IGNORE_PSLSPEC, true);
  nusmv_assert(res);
}
void unset_ignore_pslspec(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, IGNORE_PSLSPEC, false);
  nusmv_assert(res);
}
boolean opt_ignore_pslspec(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, IGNORE_PSLSPEC);
}

/******************************************************************************/
/* IGNORE_INVAR */
void set_ignore_invar(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, IGNORE_INVAR, true);
  nusmv_assert(res);
}
void unset_ignore_invar(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, IGNORE_INVAR, false);
  nusmv_assert(res);
}
boolean opt_ignore_invar(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, IGNORE_INVAR);
}

/******************************************************************************/
/* CHECK_FSM */
void set_check_fsm(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, OPT_CHECK_FSM, true);
  nusmv_assert(res);
}
void unset_check_fsm(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, OPT_CHECK_FSM, false);
  nusmv_assert(res);
}
boolean opt_check_fsm(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, OPT_CHECK_FSM);
}

/******************************************************************************/
/* FORWARD_SEARCH */
void set_forward_search(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, FORWARD_SEARCH, true);
  nusmv_assert(res);
}
void unset_forward_search(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, FORWARD_SEARCH, false);
  nusmv_assert(res);
}
boolean opt_forward_search(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, FORWARD_SEARCH);
}

/******************************************************************************/
/* LTL_TABLEAU_FORWARD_SEARCH */
void set_ltl_tableau_forward_search(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, LTL_TABLEAU_FORWARD_SEARCH, true);
  nusmv_assert(res);
}
void unset_ltl_tableau_forward_search(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, LTL_TABLEAU_FORWARD_SEARCH, false);
  nusmv_assert(res);
}
boolean opt_ltl_tableau_forward_search(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, LTL_TABLEAU_FORWARD_SEARCH);
}

/******************************************************************************/
/* PRINT_REACHABLE */
void set_print_reachable(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, PRINT_REACHABLE, true);
  nusmv_assert(res);
}
void unset_print_reachable(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, PRINT_REACHABLE, false);
  nusmv_assert(res);
}
boolean opt_print_reachable(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, PRINT_REACHABLE);
}

/******************************************************************************/
/* ENABLE_REORDER */
void set_reorder(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, ENABLE_REORDER, true);
  nusmv_assert(res);
}
void unset_reorder(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, ENABLE_REORDER, false);
  nusmv_assert(res);
}
boolean opt_reorder(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, ENABLE_REORDER);
}

/******************************************************************************/
/* REORDER_METHOD */
void set_reorder_method(OptsHandler_ptr opt, unsigned int method)
{
  char* value = DynOrderTypeConvertToString(method);
  boolean res = OptsHandler_set_enum_option_value(opt, REORDER_METHOD, value);
  nusmv_assert(res);
}
unsigned int get_reorder_method(OptsHandler_ptr opt)
{
  return OptsHandler_get_int_option_value(opt, REORDER_METHOD);
}

/******************************************************************************/
/* DYNAMIC_REORDER */
void set_dynamic_reorder(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, DYNAMIC_REORDER, true);
  nusmv_assert(res);
}
void unset_dynamic_reorder(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, DYNAMIC_REORDER, false);
  nusmv_assert(res);
}
boolean opt_dynamic_reorder(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, DYNAMIC_REORDER);
}

/******************************************************************************/
/* ENABLE_SEXP2BDD_CACHING */
/* allows the memoization (caching) of computations
   in sexpr-to-bdd evaluations. */
void set_enable_sexp2bdd_caching(OptsHandler_ptr opt)
{
  boolean res =
    OptsHandler_set_bool_option_value(opt,
                                      ENABLE_SEXP2BDD_CACHING,
                                      true);
  nusmv_assert(res);
}
void unset_enable_sexp2bdd_caching(OptsHandler_ptr opt)
{
  boolean res =
    OptsHandler_set_bool_option_value(opt,
                                      ENABLE_SEXP2BDD_CACHING,
                                      false);
  nusmv_assert(res);
}
boolean opt_enable_sexp2bdd_caching(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt,
                                           ENABLE_SEXP2BDD_CACHING);
}

/******************************************************************************/
/* BATCH */
void set_batch(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, BATCH, true);
  nusmv_assert(res);
}
void unset_batch(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, BATCH, false);
  nusmv_assert(res);
}
boolean opt_batch(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, BATCH);
}

/******************************************************************************/
/* PARTITION_METHOD */
void set_partition_method(OptsHandler_ptr opt, const TransType method)
{
  char* str = NULL;
  boolean res;

  switch (method) {
  case TRANS_TYPE_MONOLITHIC:
    str = TRANS_TYPE_MONOLITHIC_STRING; break;
  case TRANS_TYPE_IWLS95:
    str = TRANS_TYPE_IWLS95_STRING; break;
  case TRANS_TYPE_THRESHOLD:
    str = TRANS_TYPE_THRESHOLD_STRING; break;
  default:
    error_unreachable_code(); break;
  }

  res = OptsHandler_set_enum_option_value(opt, PARTITION_METHOD, str);
  nusmv_assert(res);
}

TransType get_partition_method(OptsHandler_ptr opt)
{
  return (TransType)OptsHandler_get_enum_option_value(opt, PARTITION_METHOD);
}
void set_monolithic(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_enum_option_value(opt, PARTITION_METHOD,
                                                  TRANS_TYPE_MONOLITHIC_STRING);
  nusmv_assert(res);
}
void set_conj_partitioning(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_enum_option_value(opt, PARTITION_METHOD,
                                                  TRANS_TYPE_THRESHOLD_STRING);
  nusmv_assert(res);
}
void reset_partitioning_method(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, PARTITION_METHOD);
  nusmv_assert(res);
}
void set_iwls95cp_partitioning(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_enum_option_value(opt, PARTITION_METHOD,
                                                  TRANS_TYPE_IWLS95_STRING);
  nusmv_assert(res);
}
boolean opt_monolithic(OptsHandler_ptr opt)
{
  return((OptsHandler_get_enum_option_value(opt, PARTITION_METHOD) ==
          TRANS_TYPE_MONOLITHIC));
}
boolean opt_conj_partitioning(OptsHandler_ptr opt)
{
  return((OptsHandler_get_enum_option_value(opt, PARTITION_METHOD) ==
          TRANS_TYPE_THRESHOLD));
}
boolean opt_iwls95cp_partitioning(OptsHandler_ptr opt)
{
  return((OptsHandler_get_enum_option_value(opt, PARTITION_METHOD) ==
          TRANS_TYPE_IWLS95));
}

void print_partition_method (FILE * output_stream)
{
  fprintf(output_stream, "Monolithic, Threshold, Iwls95CP");
}

/******************************************************************************/
/* CONJ_PART_THRESHOLD */
void set_conj_part_threshold(OptsHandler_ptr opt, int threshold)
{
  boolean res = OptsHandler_set_int_option_value(opt, CONJ_PART_THRESHOLD,
                                                 threshold);
  nusmv_assert(res);
}
void reset_conj_part_threshold(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, CONJ_PART_THRESHOLD);
  nusmv_assert(res);
}
int get_conj_part_threshold(OptsHandler_ptr opt)
{
  return OptsHandler_get_int_option_value(opt, CONJ_PART_THRESHOLD);
}

/******************************************************************************/
/* IMAGE_CLUSTER_SIZE */
void set_image_cluster_size(OptsHandler_ptr opt, int threshold)
{
  boolean res = OptsHandler_set_int_option_value(opt, IMAGE_CLUSTER_SIZE,
                                                 threshold);
  nusmv_assert(res);
}
void reset_image_cluster_size(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, IMAGE_CLUSTER_SIZE);
  nusmv_assert(res);
}
int get_image_cluster_size(OptsHandler_ptr opt)
{
  return OptsHandler_get_int_option_value(opt, IMAGE_CLUSTER_SIZE);
}

/******************************************************************************/
/* IGNORE_INIT_FILE */
void set_ignore_init_file(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  IGNORE_INIT_FILE,
                                                  true);
  nusmv_assert(res);
}
void unset_ignore_init_file(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  IGNORE_INIT_FILE,
                                                  false);
  nusmv_assert(res);
}
boolean opt_ignore_init_file(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, IGNORE_INIT_FILE);
}

/******************************************************************************/
/* AG_ONLY_SEARCH */
void set_ag_only(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, AG_ONLY_SEARCH, true);
  nusmv_assert(res);
}
void unset_ag_only(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, AG_ONLY_SEARCH, false);
  nusmv_assert(res);
}
boolean opt_ag_only(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, AG_ONLY_SEARCH);
}

/******************************************************************************/
/* CONE_OF_INFLUENCE */
void set_cone_of_influence(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, CONE_OF_INFLUENCE, true);
  nusmv_assert(res);
}
void unset_cone_of_influence(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, CONE_OF_INFLUENCE, false);
  nusmv_assert(res);
}
boolean opt_cone_of_influence(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, CONE_OF_INFLUENCE);
}

/******************************************************************************/
/* LIST_PROPERTIES */
void set_list_properties(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, LIST_PROPERTIES, true);
  nusmv_assert(res);
}
void unset_list_properties(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, LIST_PROPERTIES, false);
  nusmv_assert(res);
}
boolean opt_list_properties(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, LIST_PROPERTIES);
}

/******************************************************************************/
/* PROP_PRINT_METHOD */
void set_prop_print_method(OptsHandler_ptr opt, const char* enum_string)
{
  boolean res = OptsHandler_set_enum_option_value(opt, PROP_PRINT_METHOD,
                                                  enum_string);
  nusmv_assert(res);
}

void reset_prop_print_method(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, PROP_PRINT_METHOD);
  nusmv_assert(res);
}

int get_prop_print_method(OptsHandler_ptr opt)
{
  return OptsHandler_get_enum_option_value(opt, PROP_PRINT_METHOD);
}

/******************************************************************************/
/* PROP_NO */
void set_prop_no(OptsHandler_ptr opt, int n)
{
  boolean res = (n >= 0);
  nusmv_assert(res);
  res = OptsHandler_set_int_option_value(opt, PROP_NO, n);
  nusmv_assert(res);
}

int get_prop_no(OptsHandler_ptr opt)
{
  return OptsHandler_get_int_option_value(opt, PROP_NO);
}

/******************************************************************************/
/* A_SAT_SOLVER */
void set_sat_solver(OptsHandler_ptr opt, const char* satSolver)
{
  boolean res = OptsHandler_set_option_value(opt, A_SAT_SOLVER, satSolver);
  nusmv_assert(res);
}

const char* get_sat_solver(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, A_SAT_SOLVER);
}

/******************************************************************************/
/* DEFAULT_TRACE_PLUGIN */
boolean set_default_trace_plugin(OptsHandler_ptr opt, int plugin)
{
  return OptsHandler_set_int_option_value(opt,
                                          DEFAULT_TRACE_PLUGIN,
                                          plugin);
}

int get_default_trace_plugin(OptsHandler_ptr opt)
{
  return OptsHandler_get_int_option_value(opt, DEFAULT_TRACE_PLUGIN);
}

/******************************************************************************/
/* IWLS95_PREORDER */
/* Only for testing purpose right now */
void set_iwls95_preorder(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, IWLS95_PREORDER, true);
  nusmv_assert(res);
}

void unset_iwls95_preorder(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, IWLS95_PREORDER, false);
  nusmv_assert(res);
}

boolean opt_iwls95_preorder(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, IWLS95_PREORDER);
}

/******************************************************************************/
/* AFFINITY_CLUSTERING */
void set_affinity(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, AFFINITY_CLUSTERING, true);
  nusmv_assert(res);
}

void unset_affinity(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, AFFINITY_CLUSTERING, false);
  nusmv_assert(res);
}

boolean opt_affinity(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, AFFINITY_CLUSTERING);
}

/******************************************************************************/
/* APPEND_CLUSTERS */
void set_append_clusters(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, APPEND_CLUSTERS, true);
  nusmv_assert(res);
}

void unset_append_clusters(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, APPEND_CLUSTERS, true);
  nusmv_assert(res);
}

boolean opt_append_clusters(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, APPEND_CLUSTERS);
}

/******************************************************************************/
/* COUNTER_EXAMPLES */
void set_counter_examples(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, COUNTER_EXAMPLES, true);
  nusmv_assert(res);
}

void unset_counter_examples(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, COUNTER_EXAMPLES, false);
  nusmv_assert(res);
}

boolean opt_counter_examples(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, COUNTER_EXAMPLES);
}

/******************************************************************************/
/* TRACES_HIDING_PREFIX */
void set_traces_hiding_prefix(OptsHandler_ptr opt, const char* prefix)
{
  boolean res = OptsHandler_set_option_value(opt, TRACES_HIDING_PREFIX,
                                             prefix);
  nusmv_assert(res);
}

const char* opt_traces_hiding_prefix(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, TRACES_HIDING_PREFIX);
}

/******************************************************************************/
/* BDD_ENCODE_WORD_BITS */
void set_bdd_encoding_word_bits(OptsHandler_ptr opt) {
  boolean res = OptsHandler_set_bool_option_value(opt, BDD_ENCODE_WORD_BITS, true);
  nusmv_assert(res);
}

void unset_bdd_encoding_word_bits(OptsHandler_ptr opt) {
  boolean res = OptsHandler_set_bool_option_value(opt, BDD_ENCODE_WORD_BITS, false);
  nusmv_assert(res);
}

void reset_bdd_encoding_word_bits(OptsHandler_ptr opt) {
  boolean res = OptsHandler_reset_option_value(opt, BDD_ENCODE_WORD_BITS);
  nusmv_assert(res);
}

boolean opt_bdd_encoding_word_bits(OptsHandler_ptr opt) {
  return OptsHandler_get_bool_option_value(opt, BDD_ENCODE_WORD_BITS);
}

/******************************************************************************/
/* TRACES_REGEXP */
#if NUSMV_HAVE_REGEX_H
/* Returns true if everything went smooth, false otherwise. In case of
   error, a message is printed */
boolean set_traces_regexp(OptsHandler_ptr opt, const char* re)
{
  boolean res = OptsHandler_set_option_value(opt, TRACES_REGEXP, re);
  return res;
}

const char* opt_traces_regexp(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, TRACES_REGEXP);
}
#endif

/******************************************************************************/
/* ON_FAILURE_SCRIPT_QUITS */
void set_on_failure_script_quits(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  ON_FAILURE_SCRIPT_QUITS,
                                                  true);
  nusmv_assert(res);
}

void unset_on_failure_script_quits(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  ON_FAILURE_SCRIPT_QUITS,
                                                  false);
  nusmv_assert(res);
}

boolean opt_on_failure_script_quits(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, ON_FAILURE_SCRIPT_QUITS);
}

/******************************************************************************/
/* WRITE_ORDER_DUMPS_BITS */
void set_write_order_dumps_bits(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  WRITE_ORDER_DUMPS_BITS,
                                                  true);
  nusmv_assert(res);
}

void unset_write_order_dumps_bits(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  WRITE_ORDER_DUMPS_BITS,
                                                  false);
  nusmv_assert(res);
}

boolean opt_write_order_dumps_bits(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, WRITE_ORDER_DUMPS_BITS);
}

/******************************************************************************/
/* USE_FAIR_STATES */
void set_use_fair_states(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, USE_FAIR_STATES, true);
  nusmv_assert(res);
}

void unset_use_fair_states(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, USE_FAIR_STATES, false);
  nusmv_assert(res);
}

boolean opt_use_fair_states(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, USE_FAIR_STATES);
}

/******************************************************************************/
/* USE_REACHABLE_STATES */
void set_use_reachable_states(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  USE_REACHABLE_STATES,
                                                  true);
  nusmv_assert(res);
}

void unset_use_reachable_states(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  USE_REACHABLE_STATES,
                                                  false);
  nusmv_assert(res);
}

boolean opt_use_reachable_states(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, USE_REACHABLE_STATES);
}

/******************************************************************************/
/* USE_ANSI_C_DIV_OP */

/* WARNING [MD] MISSING SETTER */
void unset_use_ansi_c_div_op(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, USE_ANSI_C_DIV_OP, false);
  nusmv_assert(res);
}

boolean opt_use_ansi_c_div_op(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, USE_ANSI_C_DIV_OP);
}

/******************************************************************************/
/* VARS_ORD_TYPE */
void set_vars_order_type(OptsHandler_ptr opt, VarsOrdType type)
{
  boolean res = OptsHandler_set_enum_option_value(opt, VARS_ORD_TYPE,
                                                  (char*)Enc_vars_ord_to_string(type));
  nusmv_assert(res);
}

VarsOrdType get_vars_order_type(OptsHandler_ptr opt)
{
  return (VarsOrdType)OptsHandler_get_enum_option_value(opt, VARS_ORD_TYPE);
}

/******************************************************************************/
/* BDD_STATIC_ORDER_HEURISTICS */
void set_bdd_static_order_heuristics(OptsHandler_ptr opt, BddSohEnum value)
{
  char* val = (char*)Enc_bdd_static_order_heuristics_to_string(value);
  boolean res = OptsHandler_set_enum_option_value(opt,
                                                  BDD_STATIC_ORDER_HEURISTICS,
                                                  val);
  nusmv_assert(res);
}

BddSohEnum get_bdd_static_order_heuristics(OptsHandler_ptr opt)
{
  return (BddSohEnum)OptsHandler_get_enum_option_value(opt,
                                                       BDD_STATIC_ORDER_HEURISTICS);
}

/******************************************************************************/
/* RBC_CNF_ALGORITHM */
void set_rbc2cnf_algorithm(OptsHandler_ptr opt, Be_CnfAlgorithm alg)
{
  char* val = (char*) NULL;
  boolean res;

  switch (alg) {
  case RBC_SHERIDAN_CONVERSION:
    val = RBC_SHERIDAN_CONVERSION_NAME;
    break;

  case RBC_TSEITIN_CONVERSION:
    val = RBC_TSEITIN_CONVERSION_NAME;
    break;

  default:
    error_unreachable_code();
    break;
  }

  res = OptsHandler_set_enum_option_value(opt, RBC_CNF_ALGORITHM, val);
  nusmv_assert(res);
}

void unset_rbc2cnf_algorithm(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, RBC_CNF_ALGORITHM);
  nusmv_assert(res);
}

Be_CnfAlgorithm get_rbc2cnf_algorithm(OptsHandler_ptr opt)
{
  return ((Be_CnfAlgorithm)
          OptsHandler_get_enum_option_value(opt, RBC_CNF_ALGORITHM));
}

/******************************************************************************/
/* SYMB_INLINING */
void set_symb_inlining(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, SYMB_INLINING, true);
  nusmv_assert(res);
}

void unset_symb_inlining(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, SYMB_INLINING, false);
  nusmv_assert(res);
}

boolean opt_symb_inlining(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, SYMB_INLINING);
}

/******************************************************************************/
/* RBC_INLINING */
void set_rbc_inlining(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, RBC_INLINING, true);
  nusmv_assert(res);
}

void unset_rbc_inlining(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, RBC_INLINING, false);
  nusmv_assert(res);
}

boolean opt_rbc_inlining(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, RBC_INLINING);
}

/******************************************************************************/
/* RBC_INLINING_LAZY */
void set_rbc_inlining_lazy(OptsHandler_ptr opt)
{
  OptsHandler_set_bool_option_value(opt, RBC_INLINING_LAZY, true);
}

void unset_rbc_inlining_lazy(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  RBC_INLINING_LAZY, false);
  nusmv_assert(res);
}

boolean opt_rbc_inlining_lazy(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, RBC_INLINING_LAZY);
}

/******************************************************************************/
/* SHOW_DEFINES_IN_TRACES */
void set_show_defines_in_traces (OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, SHOW_DEFINES_IN_TRACES,
                                                  true);
  nusmv_assert(res);
}

void unset_show_defines_in_traces (OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, SHOW_DEFINES_IN_TRACES,
                                                  false);
  nusmv_assert(res);
}

boolean opt_show_defines_in_traces (OptsHandler_ptr opt)
{
  nusmv_assert (opt != NULL);
  return OptsHandler_get_bool_option_value(opt, SHOW_DEFINES_IN_TRACES);
}

/******************************************************************************/
/* SHOW_DEFINES_WITH_NEXT */
void set_show_defines_with_next (OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, SHOW_DEFINES_WITH_NEXT,
                                                  true);
  nusmv_assert(res);
}

void unset_show_defines_with_next (OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, SHOW_DEFINES_WITH_NEXT,
                                                  false);
  nusmv_assert(res);
}

boolean opt_show_defines_with_next (OptsHandler_ptr opt)
{
  nusmv_assert (opt != NULL);
  return OptsHandler_get_bool_option_value(opt, SHOW_DEFINES_WITH_NEXT);
}

/******************************************************************************/
/* INVAR_CHECK_STRATEGY */
void set_check_invar_strategy (OptsHandler_ptr opt, Check_Strategy strategy)
{
  char* str = (char*)opt_check_invar_strategy_to_string(strategy);
  boolean res = OptsHandler_set_enum_option_value(opt,
                                                  INVAR_CHECK_STRATEGY,
                                                  str);
  nusmv_assert(res);
}

Check_Strategy opt_check_invar_strategy (OptsHandler_ptr opt)
{
  nusmv_assert (opt != NULL);
  return (Check_Strategy)OptsHandler_get_enum_option_value(opt,
                                                           INVAR_CHECK_STRATEGY);
}

char* opt_check_invar_strategy_to_string(Check_Strategy str)
{
  switch (str) {
  case FORWARD:
    return "forward";
  case BACKWARD:
    return "backward";
  case FORWARD_BACKWARD:
    return "forward-backward";
  case BDD_BMC:
    return "bdd-bmc";
  default:
    error_unreachable_code(); break;
  }

  return (char*)NULL;
}

const char* opt_check_invar_strategy_as_string (OptsHandler_ptr opt)
{
  nusmv_assert (opt != NULL);

  return opt_check_invar_strategy_to_string(opt_check_invar_strategy(opt));
}

/******************************************************************************/
/* CHECK_INVAR_FB_HEURISTIC */
/* Forward-Backward heuristic */
void set_check_invar_fb_heuristic (OptsHandler_ptr opt, FB_Heuristic h)
{
  const char* str = opt_check_invar_fb_heuristic_to_string(h);
  boolean res = OptsHandler_set_enum_option_value(opt,
                                                  CHECK_INVAR_FB_HEURISTIC,
                                                  str);
  nusmv_assert(res);
}

FB_Heuristic opt_check_invar_fb_heuristic (OptsHandler_ptr opt)
{
  nusmv_assert (opt != NULL);
  return (FB_Heuristic)OptsHandler_get_enum_option_value(opt,
                                                         CHECK_INVAR_FB_HEURISTIC);
}

const char* opt_check_invar_fb_heuristic_as_string (OptsHandler_ptr opt)
{
  nusmv_assert (opt != NULL);

  return opt_check_invar_fb_heuristic_to_string(opt_check_invar_fb_heuristic(opt));
}

/******************************************************************************/
/* PP_CPP_PATH */
void set_pp_cpp_path(OptsHandler_ptr opt, const char* str)
{
  boolean res = OptsHandler_set_option_value(opt, PP_CPP_PATH, str);

  nusmv_assert(res);
}

void reset_pp_cpp_path(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, PP_CPP_PATH);

  nusmv_assert(res);
}

char* get_pp_cpp_path(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, PP_CPP_PATH);
}

/******************************************************************************/
/* PP_M4_PATH */
void set_pp_m4_path(OptsHandler_ptr opt, const char* str)
{
  boolean res = OptsHandler_set_option_value(opt, PP_M4_PATH, str);

  nusmv_assert(res);
}

void reset_pp_m4_path(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, PP_M4_PATH);

  nusmv_assert(res);
}

char* get_pp_m4_path(OptsHandler_ptr opt)
{
  return OptsHandler_get_string_option_value(opt, PP_M4_PATH);
}

char* opt_check_invar_fb_heuristic_to_string(FB_Heuristic h)
{
  switch (h) {
  case ZIGZAG_HEURISTIC:
    return "zigzag";
  case SMALLEST_BDD_HEURISTIC:
    return "smallest";
  default:
    error_unreachable_code(); break;
  }

  return (char*)NULL;
}

/******************************************************************************/
/* CHECK_INVAR_BDDBMC_HEURISTIC */
/* Switch between BDD and BMC heuristic */
void set_check_invar_bddbmc_heuristic (OptsHandler_ptr opt, Bdd2bmc_Heuristic h)
{
  const char* str = opt_check_invar_bddbmc_heuristic_to_string(h);
  boolean res = OptsHandler_set_enum_option_value(opt,
                                                  CHECK_INVAR_BDDBMC_HEURISTIC,
                                                  str);
  nusmv_assert(res);
}

Bdd2bmc_Heuristic opt_check_invar_bddbmc_heuristic (OptsHandler_ptr opt)
{
  return (Bdd2bmc_Heuristic)OptsHandler_get_enum_option_value(opt,
                                                              CHECK_INVAR_BDDBMC_HEURISTIC);
}

const char* opt_check_invar_bddbmc_heuristic_as_string (OptsHandler_ptr opt)
{
  return opt_check_invar_bddbmc_heuristic_to_string(opt_check_invar_bddbmc_heuristic(opt));
}

char* opt_check_invar_bddbmc_heuristic_to_string (Bdd2bmc_Heuristic h)
{
  switch (h) {
  case STEPS_HEURISTIC:
    return "steps";
  case SIZE_HEURISTIC:
    return "size";
  default:
    error_unreachable_code(); break;
  }

  return (char*)NULL;
}

/******************************************************************************/
/* CHECK_INVAR_BDDBMC_HEURISTIC_THRESHOLD */
/* Switch between BDD and BMC heuristic threshold*/
void set_check_invar_bddbmc_heuristic_threshold (OptsHandler_ptr opt, int h)
{
  boolean res = OptsHandler_set_int_option_value(opt,
                                                 CHECK_INVAR_BDDBMC_HEURISTIC_THRESHOLD, h);
  nusmv_assert(res);
}

int opt_check_invar_bddbmc_heuristic_threshold (OptsHandler_ptr opt)
{
  return OptsHandler_get_int_option_value(opt,
                                          CHECK_INVAR_BDDBMC_HEURISTIC_THRESHOLD);
}

/******************************************************************************/
/* DAGGIFIER_ENABLED */
boolean opt_is_daggifier_enabled(OptsHandler_ptr opt) {
  return OptsHandler_get_bool_option_value(opt, DAGGIFIER_ENABLED);
}

void opt_enable_daggifier(OptsHandler_ptr opt) {
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  DAGGIFIER_ENABLED,
                                                  true);
  nusmv_assert(res);
}

void opt_disable_daggifier(OptsHandler_ptr opt) {
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  DAGGIFIER_ENABLED,
                                                  false);
  nusmv_assert(res);
}

/******************************************************************************/
/* DAGGIFIER_COUNTER_THRESHOLD */
int opt_get_daggifier_counter_threshold(OptsHandler_ptr opt) {
  return OptsHandler_get_int_option_value(opt, DAGGIFIER_COUNTER_THRESHOLD);
}

void opt_set_daggifier_counter_threshold(OptsHandler_ptr opt, int x) {
  boolean res = OptsHandler_set_int_option_value(opt,
                                                 DAGGIFIER_COUNTER_THRESHOLD,
                                                 x);
  nusmv_assert(res);
}

/******************************************************************************/
/* DAGGIFIER_DEPTH_THRESHOLD */
int opt_get_daggifier_depth_threshold(OptsHandler_ptr opt) {
  return OptsHandler_get_int_option_value(opt, DAGGIFIER_DEPTH_THRESHOLD);
}

void opt_set_daggifier_depth_threshold(OptsHandler_ptr opt,
                                       int x) {
  boolean res = OptsHandler_set_int_option_value(opt, DAGGIFIER_DEPTH_THRESHOLD, x);
  nusmv_assert(res);
}

/******************************************************************************/
/* DAGGIFIER_STATISTICS */
boolean opt_get_daggifier_statistics(OptsHandler_ptr opt) {
  return OptsHandler_get_bool_option_value(opt, DAGGIFIER_STATISTICS);
}

void set_daggifier_statistics(OptsHandler_ptr opt) {
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  DAGGIFIER_STATISTICS,
                                                  true);
  nusmv_assert(res);
}

void unset_daggifier_statistics(OptsHandler_ptr opt) {
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  DAGGIFIER_STATISTICS,
                                                  false);
  nusmv_assert(res);
}

/******************************************************************************/
/* OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM */
/* different BDD-based algorithms to check language emptiness for
   omega-regular properties */

BddOregJusticeEmptinessBddAlgorithmType
get_oreg_justice_emptiness_bdd_algorithm(OptsHandler_ptr opt)
{
  int res;

  res = OptsHandler_get_enum_option_value(opt,
                                          OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM);

  return (BddOregJusticeEmptinessBddAlgorithmType)res;
}

void set_oreg_justice_emptiness_bdd_algorithm(OptsHandler_ptr opt,
                                              BddOregJusticeEmptinessBddAlgorithmType alg)
{
  const char* str = Bdd_BddOregJusticeEmptinessBddAlgorithmType_to_string(alg);

  boolean res = OptsHandler_set_enum_option_value(opt,
                                                  OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM, str);
  nusmv_assert(res);
}

void reset_oreg_justice_emptiness_bdd_algorithm(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt,
                                               OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM);
  nusmv_assert(res);
}

/******************************************************************************/
/* USE_COI_SIZE_SORTING */
void set_use_coi_size_sorting(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  USE_COI_SIZE_SORTING, true);
  nusmv_assert(res);
}

void unset_use_coi_size_sorting(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  USE_COI_SIZE_SORTING, false);
  nusmv_assert(res);
}

boolean opt_use_coi_size_sorting(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, USE_COI_SIZE_SORTING);
}

/******************************************************************************/
/* DISABLE_SYNTACTIC_CHECKS */
void opt_disable_syntactic_checks(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  DISABLE_SYNTACTIC_CHECKS,
                                                  true);
  nusmv_assert(res);
}

void opt_enable_syntactic_checks(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  DISABLE_SYNTACTIC_CHECKS,
                                                  false);
  nusmv_assert(res);
}

boolean opt_syntactic_checks_disabled(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, DISABLE_SYNTACTIC_CHECKS);
}

/******************************************************************************/
/* KEEP_SINGLE_VALUE_VARS */
void set_keep_single_value_vars(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  KEEP_SINGLE_VALUE_VARS,
                                                  true);
  nusmv_assert(res);
}

void unset_keep_single_value_vars(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  KEEP_SINGLE_VALUE_VARS,
                                                  false);
  nusmv_assert(res);
}

boolean opt_keep_single_value_vars(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, KEEP_SINGLE_VALUE_VARS);
}

/******************************************************************************/
/* QUIET_MODE */
boolean opt_get_quiet_mode(OptsHandler_ptr opt) {
  return OptsHandler_get_bool_option_value(opt, QUIET_MODE);
}

void set_quiet_mode(OptsHandler_ptr opt) {
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  QUIET_MODE,
                                                  true);
  nusmv_assert(res);
}

void unset_quiet_mode(OptsHandler_ptr opt) {
  boolean res = OptsHandler_set_bool_option_value(opt,
                                                  QUIET_MODE,
                                                  false);
  nusmv_assert(res);
}

/******************************************************************************/
/* DEFAULT_SIMULATION_STEPS */
void set_default_simulation_steps(OptsHandler_ptr opt, int val)
{
  boolean res =
    OptsHandler_set_int_option_value(opt, DEFAULT_SIMULATION_STEPS, val);
  nusmv_assert(res);
}

void reset_default_simulation_steps(OptsHandler_ptr opt)
{
  boolean res =
    OptsHandler_reset_option_value(opt, DEFAULT_SIMULATION_STEPS);
  nusmv_assert(res);
}

int get_default_simulation_steps(OptsHandler_ptr opt)
{
  return OptsHandler_get_int_option_value(opt, DEFAULT_SIMULATION_STEPS);
}


/******************************************************************************/
/* LTL2SMV_SINGLE_JUSTICE */
void set_ltl2smv_single_justice(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, LTL2SMV_SINGLE_JUSTICE, true);
  nusmv_assert(res);
}

void unset_ltl2smv_single_justice(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, LTL2SMV_SINGLE_JUSTICE);
  nusmv_assert(res);
}

boolean opt_ltl2smv_single_justice(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, LTL2SMV_SINGLE_JUSTICE);
}


/******************************************************************************/
/* BOOLEAN_CONVERSION_USES_PREDICATE_NORMALIZATION */

void set_boolconv_uses_prednorm(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_set_bool_option_value(opt, BOOLEAN_CONVERSION_USES_PREDICATE_NORMALIZATION, true);
  nusmv_assert(res);
}
void unset_boolconv_uses_prednorm(OptsHandler_ptr opt)
{
  boolean res = OptsHandler_reset_option_value(opt, BOOLEAN_CONVERSION_USES_PREDICATE_NORMALIZATION);
  nusmv_assert(res);
}
boolean opt_boolconv_uses_prednorm(OptsHandler_ptr opt)
{
  return OptsHandler_get_bool_option_value(opt, BOOLEAN_CONVERSION_USES_PREDICATE_NORMALIZATION);
}

boolean opt_set_reachable_states_trigger(OptsHandler_ptr opts,
                                         const char* opt,
                                         const char* value,
                                         Trigger_Action action,
                                         void* arg)
{
  if (ACTION_SET == action ||
      ACTION_RESET == action) {
    boolean enable = (strcmp(value, OPTS_TRUE_VALUE) == 0);
    boolean res = OptsHandler_set_bool_option_value(opts,
                                                    USE_REACHABLE_STATES,
                                                    enable);
    if (strcmp(opt, FORWARD_SEARCH) != 0) {
      res &= OptsHandler_set_bool_option_value(opts,
                                               FORWARD_SEARCH,
                                               enable);
    }
    nusmv_assert(res);
  }


  return true;
}

boolean opt_reorder_method_trigger(OptsHandler_ptr opts,
                                   const char* opt,
                                   const char* value,
                                   Trigger_Action action,
                                   void* arg)
{
  NuSMVEnv_ptr env = NUSMV_ENV(arg);

  if (ACTION_SET == action) {
    DDMgr_ptr dd;
    unsigned int method = StringConvertToDynOrderType((char*)value);

    nusmv_assert(NuSMVEnv_has_value(env, ENV_DD_MGR));
    dd = (DDMgr_ptr )NuSMVEnv_get_value(env, ENV_DD_MGR);

    dd_autodyn_enable(dd, method);

    if (!OptsHandler_get_bool_option_value(opts, DYNAMIC_REORDER)) {
      dd_autodyn_disable(dd);
    }
  }

  return true;
}

boolean opt_dynamic_reorder_trigger(OptsHandler_ptr opts,
                                    const char* opt,
                                    const char* value,
                                    Trigger_Action action,
                                    void* arg)
{
  NuSMVEnv_ptr env = NUSMV_ENV(arg);
  DDMgr_ptr dd;

  nusmv_assert(NuSMVEnv_has_value(env, ENV_DD_MGR));
  dd = (DDMgr_ptr )NuSMVEnv_get_value(env, ENV_DD_MGR);

  switch (action) {
  case ACTION_SET:
    {

      int current_ordering_method =
        OptsHandler_get_int_option_value(opts, REORDER_METHOD);

      dd_autodyn_enable(dd, current_ordering_method);
      break;
    }
  case ACTION_RESET:
    dd_autodyn_disable(dd);
    break;
  default: break;
  }

  return true;
}

boolean opt_trace_plugin_trigger(OptsHandler_ptr opts,
                                 const char* opt,
                                 const char* value,
                                 Trigger_Action action,
                                 void* arg)
{
  int plug = PTR_TO_INT(opt_get_integer(opts, value, arg));

  if (ACTION_SET == action) {
    NuSMVEnv_ptr env = NUSMV_ENV(arg);
    TraceMgr_ptr tm = TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
    return TracePkg_set_default_trace_plugin(tm, plug);
  }

  return true;
}

boolean opt_trans_order_file_trigger(OptsHandler_ptr opts,
                                     const char* opt,
                                     const char* value,
                                     Trigger_Action action,
                                     void* arg)
{
  switch (action) {
  case ACTION_SET:
  case ACTION_RESET:
    /* without a transition order there is a need for affinity clustering */
    /* with a transition order there is no need for affinity clustering */
    OptsHandler_set_bool_option_value(opts, AFFINITY_CLUSTERING,
                                      (const char*)NULL == value);
    break;
  default: break;
  }

  return true;
}

boolean opt_run_cpp_trigger(OptsHandler_ptr opts,
                            const char* opt,
                            const char* value,
                            Trigger_Action action,
                            void* arg)
{
  const NuSMVEnv_ptr env = NUSMV_ENV(arg);
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  switch (action) {
  case ACTION_SET:
  case ACTION_RESET:
    StreamMgr_print_error(streams,
                          "Error: the \"%s\" option is no longer supported", RUN_CPP);
    StreamMgr_print_error(streams,  " - use \"%s cpp\" instead.\n", PP_LIST);
    return false;
  default: break;
  }
  return true;
}

boolean opt_pp_list_trigger(OptsHandler_ptr opts,
                            const char* opt,
                            const char* value,
                            Trigger_Action action,
                            void* arg)
{
  const NuSMVEnv_ptr env = NUSMV_ENV(arg);
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  switch (action) {
  case ACTION_SET:
  case ACTION_RESET:
    {
      char* stripped = remove_non_existant_pps(opts, value, env);
      if (strcmp(stripped, value) != 0) {
        StreamMgr_print_error(streams,  "Some of the specified preprocessors "
                              "does not exist\n");
        return false;
      }
    }
  default: break;
  }
  return true;
}

boolean opt_rbc_inlining_lazy_trigger(OptsHandler_ptr opts,
                                      const char* opt,
                                      const char* value,
                                      Trigger_Action action,
                                      void* arg)
{
  const NuSMVEnv_ptr env = NUSMV_ENV(arg);
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  switch (action) {
  case ACTION_SET:
    StreamMgr_print_error(streams,  "Warning: setting of variable rbc_inlining_lazy"
                          "is not currently allowed\n");
    return false;
  default: break;
  }
  return true;
}

#if NUSMV_HAVE_REGEX_H

boolean opt_traces_regexp_trigger(OptsHandler_ptr opts, const char* opt,
                                  const char* value, Trigger_Action act,
                                  void* arg)
{
  const NuSMVEnv_ptr env = NUSMV_ENV(arg);
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  boolean res = true; /* no error by default */
  switch (act) {
  case ACTION_SET:
    {
      regex_t re;
      int cr;

      nusmv_assert(NIL(char) != value);
      cr = regcomp(&re, value, REG_NOSUB);

      if (0 != cr) {
        size_t mlen = regerror(cr, &re, NULL, 0);
        char* buf = ALLOC(char, mlen+1);
        regerror (cr, &re, buf, mlen);

        StreamMgr_print_error(streams,
                              "Error: could not parse '%s' (%s)\n", value, buf);

        FREE(buf);
        res = false ;
      }

      regfree(&re);
    }
  default: break;
  }

  return res;
}
#endif

boolean opt_script_file_trigger(OptsHandler_ptr opts,
                                const char* opt,
                                const char* value,
                                Trigger_Action action,
                                void* arg)
{
  boolean res = true;

  /* The script file option can be only set once, with the option
     -load or -source. No need for checking whether we are
     setting/unsetting/resetting the option */

  unset_batch(opts);

  return res;
}

boolean opt_pp_cpp_path_trigger(OptsHandler_ptr opts,
                                const char* opt,
                                const char* value,
                                Trigger_Action action,
                                void* arg)
{
  const NuSMVEnv_ptr env = NUSMV_ENV(arg);

  check_user_preprocessor(env, action, value);

  return true;
}

boolean opt_pp_m4_path_trigger(OptsHandler_ptr opts,
                               const char* opt,
                               const char* value,
                               Trigger_Action action,
                               void* arg)
{
  const NuSMVEnv_ptr env = NUSMV_ENV(arg);

  check_user_preprocessor(env, action, value);

  return true;
}
/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void* opt_get_integer(OptsHandler_ptr opts,
                      const char *value, void* arg)
{
  void* result;
  char * e[1];

  e[0] = "";
  result = (void*)strtol(value, e, 10);
  if (strcmp(e[0], "") != 0) {
    result = OPTS_VALUE_ERROR;
  }
  return result;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Finds all preprocessor names occurring in the given string
   that are not actually available, and returns the set of the only
   available ones

  Returned string must be freed
*/
static char* remove_non_existant_pps(OptsHandler_ptr opts, const char* pp_list,
                                     const NuSMVEnv_ptr env)
{
  char* paths;
  char* open_path = "";
  char* valid_pps;
  char* pp_list_copy;
  char* pp;
  int len;
  OptsHandler_ptr opt = opts;
  len = strlen(pp_list);

  valid_pps = ALLOC(char, len+1);
  nusmv_assert(valid_pps != (char*) NULL);
  valid_pps[0] = '\0';

  pp_list_copy = ALLOC(char, len+2);
  nusmv_assert(pp_list_copy != (char*) NULL);
  strncpy(pp_list_copy, pp_list, len+1);
  pp_list_copy[len+1] = '\0'; /* additional '\0' for strtok below */

  /* gets an operating system variable PATH and NuSMV's variable open_path */
# if NUSMV_HAVE_GETENV
  paths = getenv("PATH");
# else
  paths = "."; /* default is the current dir */
# endif

  if (OptsHandler_is_option_registered(opt, "open_path")) {
    open_path = OptsHandler_get_string_option_value(opt, "open_path");
  }

  pp = strtok(pp_list_copy, " \t\n\r");
  while (pp != (char*) NULL) {
    char* pp_filename;

    pp_filename = get_preprocessor_filename(env, pp);
    if ((pp_filename != (char*) NULL) &&
        ( Utils_file_exists(pp_filename) ||
          1 == Utils_file_exists_in_paths(pp_filename, open_path, ":;") ||
          1 == Utils_file_exists_in_paths(pp_filename, paths, ":;") )) {

      if (valid_pps[0] != '\0') strcat(valid_pps, " ");
      strcat(valid_pps, pp);
    }
    /* "strtok" is not safe and can be changed by other functions.
       "pp + its length" is the pointer to the next string to be parsed.
    */
    pp = pp + strlen(pp) + 1; /* 1 is used to pass past the current '\0' */
    pp = strtok(pp, " \t\n\r");
  } /* while loop */

  FREE(pp_list_copy);
  return valid_pps;
}

/*!
  \brief Check and inform user if the file that he entered exists,
             also inform an user that he removed system value


*/
static void check_user_preprocessor(const NuSMVEnv_ptr env,
                                    Trigger_Action action,
                                    const char* value)
{
  StreamMgr_ptr streams =
  STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr = ERROR_MGR(NuSMVEnv_get_value(env,
                                                           ENV_ERROR_MANAGER));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env,
                                                         ENV_OPTS_HANDLER));

  char* path;
  const char* filename;

  switch (action){
  case ACTION_SET:
    filename = Utils_StripPath(value);
    path = ALLOC(char, (filename - value + 2));
    Utils_StripPathNoFilenameNoExtension(value, path);

    if(1 != Utils_file_exists_in_directory(filename, path)){
      ErrorMgr_error_set_preprocessor(errmgr, value, true);
    }
    FREE(path);
    break;
  case ACTION_RESET:
    if (opt_verbose_level_gt(opts, 0)) {
      StreamMgr_print_error(streams, "User specified %s preprocessor "
                                     "removed.\n", value);
    }
    break;
  default: break;
  }
}
