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
  \author Alessandro Mariotti
  \brief Helper for the NuSMV library. Helps with extending it or
   creating new tools out of it.

  \todo: Missing description

*/

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include <sys/stat.h>

#include "nusmv/core/cinit/cinitInt.h"

#if NUSMV_HAVE_INTERACTIVE_SHELL
#include "nusmv/shell/cmd/cmd.h"
#endif

#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Olist.h"

#include "nusmv/core/bmc/bmc.h"
#include "nusmv/core/trans/trans.h"

typedef struct CmdLineOpt_TAG {
  char* name;
  char* usage;
  char* parameter;

  /* Used for special options */
  boolean (*check_and_apply)(OptsHandler_ptr, char*, NuSMVEnv_ptr);

  /* Used for options which have an associated env var. */
  char* env_option;

  boolean deprecated;
  boolean public;

  string_ptr dependency;
  Olist_ptr conflicts;
} CmdLineOpt;

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef CmdLineOpt* CmdLineOpt_ptr;

typedef struct CoreData_TAG {
  char* tool_name;
  char* tool_rcfile;
  char* tool_version;
  char* build_date;
  char* prompt_string;
  char* email;
  char* website;
  char* bug_report_message;
  char* linked_addons;

  char* library_name;
  char* library_version;
  char* library_build_date;
  char* library_email;
  char* library_website;
  char* library_bug_report_message;

  void (*print_banner)(FILE*);
  /* [AMa] The batch function should defenitely return an exit value.. */
  void (*batch)(NuSMVEnv_ptr);

  hash_ptr line_options;

  UStringMgr_ptr string_mgr;
} CoreData;

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef  CoreData* CoreData_ptr;
/* Shouldn't introduce reentrancy problems */
static CoreData_ptr core_data = (CoreData_ptr)NULL;

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/
/* Environment related constants */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_FLAG_CORE_INITIALIZED    "_nusmv_core_init_"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_INIT_FUNS                "_nusmv_core_init_funs_"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_INIT_FUNS_NUM            "_nusmv_core_init_funs_num_"

/* End of environment related constants */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define PRINT_USAGE_IN_NEWLINE 1

/* RETURN VALUES */

/* Everything went smooth */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define RET_SUCCESS                0

/* The script file does not exits or it contains an error and
   set_on_failure_script_quits is set. */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define RET_SCRIPT_ERROR          -1

/* The help has been printed out */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define RET_HELP_PRINT             2

/* An unknown option is given */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define RET_UNKNOWN_OPTION        -1

/* An error occurred with this option */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define RET_INVALID_OPTION        -1

/* An invalid parameter is given */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define RET_INVALID_OPTION_PARAM  -1

/* A required parameter is not given */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define RET_MISSING_OPTION_PARAM  -1

/* The option depends among another option which is not given */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define RET_MISSING_OPTION_DEP    -1

/* The option conflicts with another option which is given */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define RET_CONFLICT_OPTION       -1

/* The option is given more than once */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define RET_DUPLICATE_OPTION       -1

/* Max width of a printed string (in characters) */
/* #define MAX_PRINT_WIDTH           75 */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define MAX_PRINT_WIDTH           70

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static CoreData_ptr nusmv_core_get_instance(void);
static void nusmv_core_deinit(void);

static int nusmv_core_parse_line_options(NuSMVEnv_ptr env,
                                         int argc, char ** argv);

static void nusmv_core_print_usage(NuSMVEnv_ptr env,
                                   boolean print_banner);

static CmdLineOpt_ptr nusmv_core_init_opt(void);
static void nusmv_core_deinit_opt(CmdLineOpt_ptr opt);

static void nusmv_core_print_string(FILE* out, char* str, int padding);

static void nusmv_core_olist_union(Olist_ptr a, Olist_ptr b);
static Olist_ptr
nusmv_core_olist_intersection(Olist_ptr a, Olist_ptr b);

static char* nusmv_core_merge(Olist_ptr set);
static Olist_ptr nusmv_core_split( UStringMgr_ptr strmgr, char* str);
static char* nusmv_core_tolower(char* str);

static void
nusmv_core_free_line_options(CoreData_ptr core_data);

/* ------------------------- CHECK AND APPLY FUNCTIONS --------------------- */
static boolean
nusmv_core_check_sin_fun(OptsHandler_ptr opt, char* val, NuSMVEnv_ptr env);
static boolean
nusmv_core_check_rbc_fun(OptsHandler_ptr opt, char* val, NuSMVEnv_ptr env);
static boolean
nusmv_core_set_mono_partition(OptsHandler_ptr opt, char* val, NuSMVEnv_ptr env);
static boolean
nusmv_core_set_iwls95_partition(OptsHandler_ptr opt, char* val, NuSMVEnv_ptr env);
static boolean
nusmv_core_set_thresh_partition(OptsHandler_ptr opt, char* val, NuSMVEnv_ptr env);
static boolean
nusmv_core_set_cpp(OptsHandler_ptr opt, char* val, NuSMVEnv_ptr env);
static boolean
nusmv_core_set_dp(OptsHandler_ptr opt, char* val, NuSMVEnv_ptr env);
static boolean
core_data_set_fs(OptsHandler_ptr opt, char* val, NuSMVEnv_ptr env);
static boolean
nusmv_core_set_pre(OptsHandler_ptr opt, char* val, NuSMVEnv_ptr env);

static int nusmv_core_start_interactive_shell_loop(NuSMVEnv_ptr env);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void NuSMVCore_init_data(void)
{
  CoreData_ptr data = nusmv_core_get_instance();

  NuSMVCore_set_tool_name(NUSMV_PACKAGE_NAME);
  NuSMVCore_set_tool_version(NUSMV_PACKAGE_VERSION);
  NuSMVCore_set_build_date(NUSMV_PACKAGE_BUILD_DATE);
  NuSMVCore_set_prompt_string("NuSMV > ");
  NuSMVCore_set_email("nusmv-users@list.fbk.eu");
  NuSMVCore_set_website("http://nusmv.fbk.eu");
  {
    char* fmt = "Please report bugs to <%s>";
    char* tmp = ALLOC(char, strlen(fmt) + strlen(NUSMV_PACKAGE_BUGREPORT) + 1);
    sprintf(tmp, fmt, NUSMV_PACKAGE_BUGREPORT);
    NuSMVCore_set_bug_report_message(tmp);
    FREE(tmp);
  }
  NuSMVCore_set_linked_addons(NUSMV_LINKED_CORE_ADDONS);

  /* Library's fields do not have a SETTER function, since they are
     unchangeable from the tools */
  data->library_name = util_strsav(NUSMV_LIBRARY_NAME);
  data->library_version = util_strsav(NUSMV_LIBRARY_VERSION);
  data->library_build_date = util_strsav(NUSMV_LIBRARY_BUILD_DATE);
  data->library_website = util_strsav(NUSMV_LIBRARY_WEBSITE);
  data->library_email = util_strsav(NUSMV_LIBRARY_EMAIL);
  data->library_bug_report_message = util_strsav(NUSMV_LIBRARY_BUGREPORT);

  data->batch = CInit_batch_main;
  data->print_banner = CInit_BannerPrint;
}

void NuSMVCore_init(NuSMVEnv_ptr env, FP_V_E funs[][2], int num_funs)
{
  FP_V_E** init_quit_funs = NULL;

  int i;

  if (num_funs > 0) {
    nusmv_assert(NULL != funs);

    init_quit_funs = ALLOC(FP_V_E*, num_funs);
    for (i = 0; i < num_funs; i++) {
      init_quit_funs[i] = ALLOC(FP_V_E, 2);
      init_quit_funs[i][0] = funs[i][0];
      init_quit_funs[i][1] = funs[i][1];
    }

    NuSMVEnv_set_value(env, ENV_INIT_FUNS, init_quit_funs);
    NuSMVEnv_set_value(env, ENV_INIT_FUNS_NUM, PTR_FROM_INT(void*, num_funs));
  }

  CInit_init(env);

#if NUSMV_HAVE_INTERACTIVE_SHELL
  /* this is not very coorect, as shell gets initialized in core
   * (although only when shell is enabled at compile-time). This is
   * due to the current design of the init/deinit mechanism */
  Cmd_Init(env);  /* inits the cmd services for the shell */
#endif

  for (i = 0; i < num_funs; ++i) {
    init_quit_funs[i][0](env);
  }

  NuSMVEnv_set_flag(env, ENV_FLAG_CORE_INITIALIZED, true);
}

void NuSMVCore_init_cmd_options(NuSMVEnv_ptr env)
{
  char* libraryName = CInit_NuSMVObtainLibrary();
  CoreData_ptr data = nusmv_core_get_instance();

  {
    const char* fmt = "does not read any initialization file "
      "(%s/master%s, ~/%s) (default in batch mode)";
    char* tmp = ALLOC(char, strlen(fmt) + strlen(libraryName) +
                (2 * strlen(data->tool_rcfile)) + 1);

    sprintf(tmp, fmt, libraryName, data->tool_rcfile, data->tool_rcfile);
    NuSMVCore_add_env_command_line_option("-s", tmp, NULL, IGNORE_INIT_FILE,
                                          true, false, NULL, NULL);
    FREE(tmp);
  }

  NuSMVCore_add_env_command_line_option("-old_div_op",
                                        "enables the old semantics of \"/\" "
                                        "and \"mod\" operations instead"
                                        " of ANSI C semantics",
                                        NULL, USE_ANSI_C_DIV_OP,
                                        true, false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-m", "sets the variable ordering "
                                        "method to \"method\". "
                                        "Reordering will be activated",
                                        "method", REORDER_METHOD,
                                        true, false, NULL, NULL);

  {
    BddOregJusticeEmptinessBddAlgorithmType alg, a1, a2;
    const char* fmt = "sets the algorthim used for BDD-based language "
      "emptiness of Buchi fair transition systems "
      "(default is %s). The available algorthims are: %s, %s";

    const char* salg;
    const char* sa1;
    const char* sa2;
    char* tmp;

    alg = DEFAULT_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM;
    a1 = BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_FWD;
    a2 = BDD_OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM_EL_BWD;

    salg = Bdd_BddOregJusticeEmptinessBddAlgorithmType_to_string(alg);
    sa1 = Bdd_BddOregJusticeEmptinessBddAlgorithmType_to_string(a1);
    sa2 = Bdd_BddOregJusticeEmptinessBddAlgorithmType_to_string(a2);

    tmp = ALLOC(char, strlen(fmt) + strlen(salg) +
                strlen(sa1) + strlen(sa2) + 1);
    nusmv_assert(NULL != tmp);

    sprintf(tmp, fmt, salg, sa1, sa2);

    NuSMVCore_add_env_command_line_option("-ojeba", tmp, "str",
                                          OREG_JUSTICE_EMPTINESS_BDD_ALGORITHM,
                                          true, false, NULL, NULL);
    FREE(tmp);
  }

  {
    char* tmp;
    char* preps_tmp;
    const char* fmt = "defines a space-separated list of pre-processors "
      "to run (in the order given) on the input file. "
      "The list must be in double quotes if there is more "
      "than one pre-processor named.\n%s";

    if (get_preprocessors_num(env) > 0) {
      const char* preps_fmt = "The available preprocessors are: %s";
      char* preps = get_preprocessor_names(env);
      preps_tmp = ALLOC(char, strlen(preps_fmt) + strlen(preps) + 1);
      sprintf(preps_tmp, preps_fmt, preps);
      FREE(preps);
    }
    else {
      const char* preps_fmt = "Warning: there are no available preprocessors";
      preps_tmp = ALLOC(char, strlen(preps_fmt) + 1);
      sprintf(preps_tmp, preps_fmt);
    }

    tmp = ALLOC(char, strlen(fmt) + strlen(preps_tmp) + 1);
    sprintf(tmp, fmt, preps_tmp);

    NuSMVCore_add_command_line_option("-pre", tmp, "pp_list",
                                      nusmv_core_set_pre,
                                      true, false, NULL, NULL);

    FREE(preps_tmp);
    FREE(tmp);
  }

  {
    const char* fmt = "sets the sat_solver variable, used by BMC. "
      "The available SAT solvers are: %s";

    char* tmp;
    char* solvers = Sat_GetAvailableSolversString();

    tmp = ALLOC(char, strlen(fmt) + strlen(solvers) + 1);
    sprintf(tmp, fmt, solvers);

    NuSMVCore_add_env_command_line_option("-sat_solver", tmp,
                                           "str", A_SAT_SOLVER,
                                          true, false, NULL, NULL);

    FREE(solvers);
    FREE(tmp);
  }

  NuSMVCore_add_command_line_option("-sin", "enables (on) or disables sexp"
                                    " inlining (default is off)", "on|off",
                                    nusmv_core_check_sin_fun, true,
                                    false, NULL, NULL);

  NuSMVCore_add_command_line_option("-rin", "enables (on) or disables rbc"
                                    " inlining (default is on)", "on|off",
                                    nusmv_core_check_rbc_fun, true, false,
                                    NULL, NULL);

  NuSMVCore_add_command_line_option("-mono",
                                    "enables monolithic transition relation",
                                    NULL, nusmv_core_set_mono_partition, true,
                                    false, NULL, "-thresh -iwls95 -cp");

  NuSMVCore_add_command_line_option("-iwls95",
                                    "enables Iwls95 conjunctive "
                                    "partitioning and sets",
                                    "cp_t", nusmv_core_set_iwls95_partition,
                                    true, false, NULL, "-thresh -mono -cp");

  NuSMVCore_add_command_line_option("-thresh",
                                    "conjunctive partitioning with "
                                    "threshold of each partition set"
                                    " to \"cp_t\" (DEFAULT, with cp_t=1000)",
                                    "cp_t", nusmv_core_set_thresh_partition,
                                    true, false, NULL, "-iwls95 -mono -cp");

  NuSMVCore_add_command_line_option("-cp",
                                    "conjunctive partitioning with threshold"
                                    " of each partition set to \"cp_t\" "
                                    "(DEFAULT, with cp_t=1000). "
                                    "Use -thresh instead of this.",
                                    "cp_t", nusmv_core_set_thresh_partition,
                                    true, true, NULL, "-iwls95 -mono -thresh");

#if NUSMV_HAVE_CPP
  /* --------------------------------------------------------------- */
  /* CPP OPTION */
  NuSMVCore_add_command_line_option("-cpp",
                                    "runs preprocessor on SMV files before "
                                    "any specified with -pre. "
# if NUSMV_HAVE_CPP
#   if NUSMV_HAVE_GETENV
                                    "Environment variable 'CPP' can be used to "
                                    "specify a different preprocessor. "
#   endif
# else
                                    "Preprocessor was not found when NuSMV"
                                    " had been configured, then 'cpp' will"
                                    " be searched at runtime when needed"
#   if NUSMV_HAVE_GETENV
                                    ", or the 'CPP' environment variable "
                                    "will be used when defined by the user"
#   endif
                                    "."
# endif
                                    , NULL, nusmv_core_set_cpp,
                                    true, true, NULL, NULL);
  /* --------------------------------------------------------------- */
#endif

  NuSMVCore_add_env_command_line_option("-r",
                                        "enables printing of reachable states",
                                        NULL, PRINT_REACHABLE, true,
                                        false, NULL, NULL);

  NuSMVCore_add_command_line_option("-f",
                                    "computes the reachable states"
                                    " (forward search) (default)",
                                    NULL, core_data_set_fs, true, true,
                                    NULL, "-df");

  NuSMVCore_add_env_command_line_option("-df", "disables the computation of"
                                        " reachable states",
                                        NULL, FORWARD_SEARCH, true, false,
                                        NULL, "-f");

  NuSMVCore_add_command_line_option("-dp", "UNSUPPORTED", NULL,
                                    nusmv_core_set_dp, false, true, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-old",
                                        "keeps backward compatibility"
                                        " with older versions of NuSMV",
                                        NULL, BACKWARD_COMPATIBILITY, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-ctt",
                                        "enables checking for the totality"
                                        " of the transition relation",
                                        NULL, OPT_CHECK_FSM, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-lp",
                                        "lists all properties in SMV model",
                                        NULL, LIST_PROPERTIES, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-n",
                                        "specifies which property of SMV "
                                        "model should be checked", "idx",
                                        PROP_NO, true, false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-is", "does not check SPEC", NULL,
                                        IGNORE_SPEC, true, false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-ic", "does not check COMPUTE",
                                        NULL, IGNORE_COMPUTE, true, false,
                                        NULL, NULL);

  NuSMVCore_add_env_command_line_option("-ils", "does not check LTLSPEC",
                                        NULL, IGNORE_LTLSPEC, true, false,
                                        NULL, NULL);

  NuSMVCore_add_env_command_line_option("-ips", "does not check PSLSPEC",
                                        NULL, IGNORE_PSLSPEC, true, false,
                                        NULL, NULL);

  NuSMVCore_add_env_command_line_option("-ii", "does not check INVARSPEC", NULL,
                                        IGNORE_INVAR, true, false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-flt",
                                        "computes the reachable states"
                                        " also for the LTL Tableau",
                                        NULL, LTL_TABLEAU_FORWARD_SEARCH,
                                        true, false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-i",
                                        "reads order of variables "
                                        "from file \"iv_file\"",
                                        "iv_file", INPUT_ORDER_FILE, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-o",
                                        "prints order of variables"
                                        "to file \"ov_file\"",
                                        "ov_file", OUTPUT_ORDER_FILE, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-t",
                                        "reads order of vars for clustering "
                                        "from file \"tv_file\"",
                                        "tv_file", TRANS_ORDER_FILE, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-AG",
                                        "enables AG only search",
                                        NULL, AG_ONLY_SEARCH, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-reorder",
                                        "enables reordering of "
                                        "variables before exiting",
                                        NULL, ENABLE_REORDER, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-dynamic",
                                        "enables dynamic reordering "
                                        "of variables",
                                        NULL, DYNAMIC_REORDER, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-disable_sexp2bdd_caching",
                                        "disables caching of expressions"
                                        "evaluation to BDD",
                                        NULL, ENABLE_SEXP2BDD_CACHING,
                                        true, false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-bdd_soh",
                                        "sets the static variable ordering "
                                        "heuristics to \"heuristics\"",
                                        "heuristics",
                                        BDD_STATIC_ORDER_HEURISTICS, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-coi",
                                        "enables cone of influence reduction",
                                        NULL, CONE_OF_INFLUENCE, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-noaffinity",
                                        "disables affinity clustering",
                                        NULL, AFFINITY_CLUSTERING, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-iwl95preorder",
                                        "enables iwls95 preordering",
                                        NULL, IWLS95_PREORDER, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-ofm",
                                        "prints flattened model to file "
                                        "\"fn_file\"",
                                        "fn_file", OUTPUT_FLATTEN_MODEL_FILE,
                                        true, false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-disable_daggifier",
                                        "disables the SMV model "
                                        "dumper daggifier",
                                        NULL, DAGGIFIER_ENABLED,
                                        true, false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-obm",
                                        "prints boolean model to"
                                        " file \"bn_file\"",
                                        "bn_file", OUTPUT_BOOLEAN_MODEL_FILE,
                                        true, false, NULL, NULL);
#if NUSMV_HAVE_INCREMENTAL_SAT
  NuSMVCore_add_env_command_line_option("-bmc_length",
                                        "sets bmc_length variable, used by BMC",
                                        "k", BMC_PB_LENGTH, true,
                                        false, "-bmc", NULL);

  NuSMVCore_add_env_command_line_option("-bmc",
                                        "enables BMC instead of "
                                        "BDD model checking",
                                        NULL, BMC_MODE, true,
                                        false, NULL, NULL);
#endif /* HAVE_INCREMENTAL_SAT */

  NuSMVCore_add_env_command_line_option("-int",
                                        "enables interactive mode",
                                        NULL, BATCH, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-dcx",
                                        "disables computation of"
                                        " counter-examples",
                                        NULL, COUNTER_EXAMPLES, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-load",
                                        "executes NuSMV commands from file",
                                        "sc_file", SCRIPT_FILE, true,
                                        true, NULL, "-source");

  NuSMVCore_add_env_command_line_option("-source",
                                        "executes NuSMV commands from file",
                                        "sc_file", SCRIPT_FILE, true,
                                        false, NULL, "-load");

  NuSMVCore_add_env_command_line_option("-quiet",
                                        "Quiet mode",
                                        NULL, QUIET_MODE, false,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-v",
                                        "sets verbose level to \"vl\"",
                                        "vl", VERBOSE_LEVEL, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-disable_syntactic_checks",
                                        "Skips some correctness checks over "
                                        "the input model. Warning: when using "
                                        "this option, the input model MUST be "
                                        "correct, otherwise the tool may crash",
                                        NULL, DISABLE_SYNTACTIC_CHECKS, true,
                                        false, NULL, NULL);

  NuSMVCore_add_env_command_line_option("-keep_single_value_vars",
                                        "Does not convert variables that have "
                                        "only one single possible value into "
                                        "constant DEFINEs",
                                        NULL, KEEP_SINGLE_VALUE_VARS, true,
                                        false, NULL, NULL);

  FREE(libraryName);
}

boolean NuSMVCore_main(NuSMVEnv_ptr env, int argc, char ** argv, int* status)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  OptsHandler_ptr opts = NuSMVEnv_get_value(env, ENV_OPTS_HANDLER);
  CoreData_ptr data = nusmv_core_get_instance();
  int quit_flag = 0;
  boolean requires_shutdown = true;

  nusmv_assert((int*)status != NULL);
  nusmv_assert(NuSMVEnv_get_flag(env, ENV_FLAG_CORE_INITIALIZED));

  *status = RET_SUCCESS;

  /* Parse command line options */
  *status = nusmv_core_parse_line_options(env, argc, argv);

  /* We don't need the command line options anymore. we can free
     them. */
  /* nusmv_core_free_line_options(data); */

  /* Everything went smooth with parsing the command line options */
  if ((*status) == RET_SUCCESS) {
    if (!opt_batch(opts)) { /* interactive mode */

      if (!opt_get_quiet_mode(opts)) {
        data->print_banner(outstream);
      }
      quit_flag = nusmv_core_start_interactive_shell_loop(env);
      *status = RET_SUCCESS;
    }
    else {
      if (!opt_get_quiet_mode(opts)) {
        data->print_banner(outstream);
      }

      if (opt_verbose_level_gt(opts, 0)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "Starting the batch interaction.\n");
      }

      data->batch(env);
    }
  }

  /* Value of "quit_flag" is determined by the "quit" command */
  if (quit_flag == -1 || quit_flag == -2 || quit_flag == -4) {
    *status = RET_SUCCESS;
  }

  /* exits quickly and silently with quit_flag = -4 */
  if (quit_flag == -4) {
    requires_shutdown = false;
  }

  if (quit_flag == -3 || quit_flag == -5) {
    /* Script failed and on_failure_script_quits is set */
    /* Or the script file does not exist. */
    *status = RET_SCRIPT_ERROR;
  }

  return requires_shutdown;
}

void NuSMVCore_quit(NuSMVEnv_ptr env)
{
  NuSMVCore_quit_extended(env, false);
}

void NuSMVCore_quit_extended(NuSMVEnv_ptr env,
                             const boolean keep_core_data)
{
  int i;
  FP_V_E ** init_quit_funs = NuSMVEnv_get_value(env, ENV_INIT_FUNS);
  int num = PTR_TO_INT(NuSMVEnv_get_value(env, ENV_INIT_FUNS_NUM));

  nusmv_assert(NuSMVEnv_get_flag(env, ENV_FLAG_CORE_INITIALIZED));

  for (i = (num - 1); i >= 0; --i) {
    init_quit_funs[i][1](env);
  }

  for (i = 0; i < num; ++i) {
    FREE(init_quit_funs[i]);
  }
  FREE(init_quit_funs);

#if NUSMV_HAVE_INTERACTIVE_SHELL
  /* this is not very coorect, as shell gets initialized in core
   * (although only when shell is enabled at compile-time). This is
   * due to the current design of the init/deinit mechanism */
  Cmd_End(env);  /* quits the cmd services for the shell */
#endif

  CInit_end(env);

  /* The structure may be useful until the very very
     end. nusmv_core_deinit is garanteed to use only simple
     independant structures */
  /* Free line_options if not already done by NuSMVCore_main */
  if (! keep_core_data) {
    CoreData_ptr data = nusmv_core_get_instance();
    nusmv_core_free_line_options(data);
    nusmv_core_deinit();
  }

  NuSMVEnv_set_flag(env, ENV_FLAG_CORE_INITIALIZED, false);
}

void NuSMVCore_reset(NuSMVEnv_ptr env)
{
  OptsHandler_ptr opts = NuSMVEnv_get_value(env, ENV_OPTS_HANDLER);
  FP_V_E ** init_quit_funs = NuSMVEnv_get_value(env, ENV_INIT_FUNS);
  int num = PTR_TO_INT(NuSMVEnv_get_value(env, ENV_INIT_FUNS_NUM));
  int i = 0;

  nusmv_assert(NuSMVEnv_get_flag(env, ENV_FLAG_CORE_INITIALIZED));

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Shutting down the system...\n");
    Logger_inc_indent_size(logger);
  }

  /* Quit all the registered libs */
  for (i = (num - 1); i >= 0; --i) {
    init_quit_funs[i][1](env);
  }

  CInit_reset_first(env);

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
               "Shutdown completed, rebooting the system...\n");
  }

  CInit_reset_last(env);

  for (i = 0; i < num; ++i) {
    init_quit_funs[i][0](env);
  }

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "The system is now up and ready\n");
    Logger_dec_indent_size(logger);
  }

}

void NuSMVCore_add_env_command_line_option(char* name,
                                           char* usage,
                                           char* parameter,
                                           char* env_var,
                                           boolean public,
                                           boolean deprecated,
                                           char* dependency,
                                           char* conflict)
{
  CoreData_ptr data = nusmv_core_get_instance();
  CmdLineOpt_ptr opt = nusmv_core_init_opt();

  nusmv_assert((char*)NULL != name);
  opt->name = util_strsav(name);

  nusmv_assert((char*)NULL != usage);
  opt->usage = util_strsav(usage);

  if ((char*)NULL != parameter) {
    opt->parameter = util_strsav(parameter);
  }

  nusmv_assert((char*)NULL != env_var);
  opt->env_option = util_strsav(env_var);

  opt->deprecated = deprecated;
  opt->public = public;

  if ((char*)NULL != dependency) {
    opt->dependency =  UStringMgr_find_string(data->string_mgr, dependency);
  }

  if ((char*)NULL != conflict) {
    Olist_ptr split = nusmv_core_split(data->string_mgr, conflict);
    nusmv_core_olist_union(opt->conflicts, split);
    Olist_destroy(split);
  }

  nusmv_assert((hash_ptr)NULL != data->line_options);

  insert_assoc(data->line_options,
               NODE_PTR( UStringMgr_find_string(data->string_mgr, name)),
               NODE_PTR(opt));
}

void
NuSMVCore_add_command_line_option(char* name,
                                  char* usage,
                                  char* parameter,
                                  boolean (*check_and_apply)(OptsHandler_ptr, char*, NuSMVEnv_ptr),
                                  boolean public,
                                  boolean deprecated,
                                  char* dependency,
                                  char* conflict)
{
  CoreData_ptr data = nusmv_core_get_instance();
  CmdLineOpt_ptr opt = nusmv_core_init_opt();

  nusmv_assert((char*)NULL != name);
  opt->name = util_strsav(name);

  nusmv_assert((char*)NULL != usage);
  opt->usage = util_strsav(usage);

  if ((char*)NULL != parameter) {
    opt->parameter = util_strsav(parameter);
  }

  nusmv_assert(NULL != check_and_apply);
  opt->check_and_apply = check_and_apply;

  opt->deprecated = deprecated;
  opt->public = public;

  if ((char*)NULL != dependency) {
    opt->dependency =  UStringMgr_find_string(data->string_mgr, dependency);
  }

  if ((char*)NULL != conflict) {
    Olist_ptr split = nusmv_core_split(data->string_mgr, conflict);
    nusmv_core_olist_union(opt->conflicts, split);
    Olist_destroy(split);
  }

  nusmv_assert((hash_ptr)NULL != data->line_options);

  insert_assoc(data->line_options,
               NODE_PTR( UStringMgr_find_string(data->string_mgr, name)),
               NODE_PTR(opt));
}

char* NuSMVCore_get_tool_name(void)
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->tool_name;
}

char* NuSMVCore_get_tool_rc_file_name(void)
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->tool_rcfile;
}

void NuSMVCore_set_tool_name(char* tool_name)
{
  CoreData_ptr self = nusmv_core_get_instance();
  char* tmp = (char*)NULL;
  char* l_tool_name = (char*)NULL;

  if ((char*)NULL != self->tool_name) {
    FREE(self->tool_name);
  }
  if ((char*)NULL != self->tool_rcfile) {
    FREE(self->tool_rcfile);
  }
  self->tool_name = util_strsav(tool_name);

  l_tool_name = nusmv_core_tolower(tool_name);
  tmp = ALLOC(char, strlen(tool_name) + 4);
  sprintf(tmp, ".%src", l_tool_name);

  self->tool_rcfile = tmp;

  FREE(l_tool_name);
}

char* NuSMVCore_get_tool_version()
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->tool_version;
}

void NuSMVCore_set_tool_version(char* tool_version)
{
  CoreData_ptr self = nusmv_core_get_instance();
  if ((char*)NULL != self->tool_version) {
    FREE(self->tool_version);
  }
  self->tool_version = util_strsav(tool_version);
}

char* NuSMVCore_get_build_date()
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->build_date;
}

void NuSMVCore_set_build_date(char* build_date)
{
  CoreData_ptr self = nusmv_core_get_instance();
  if ((char*)NULL != self->build_date) {
    FREE(self->build_date);
  }
  self->build_date = util_strsav(build_date);
}

char* NuSMVCore_get_prompt_string(void)
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->prompt_string;
}

void NuSMVCore_set_prompt_string(char* prompt_string)
{
  CoreData_ptr self = nusmv_core_get_instance();
  if ((char*)NULL != self->prompt_string) {
    FREE(self->prompt_string);
  }
  self->prompt_string = util_strsav(prompt_string);
}

char* NuSMVCore_get_email()
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->email;
}

void NuSMVCore_set_email(char* email)
{
  CoreData_ptr self = nusmv_core_get_instance();
  if ((char*)NULL != self->email) {
    FREE(self->email);
  }
  self->email = util_strsav(email);
}

char* NuSMVCore_get_website()
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->website;
}

void NuSMVCore_set_website(char* website)
{
  CoreData_ptr self = nusmv_core_get_instance();
  if ((char*)NULL != self->website) {
    FREE(self->website);
  }
  self->website = util_strsav(website);
}

char* NuSMVCore_get_bug_report_message()
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->bug_report_message;
}

void NuSMVCore_set_bug_report_message(char* bug_report_message)
{
  CoreData_ptr self = nusmv_core_get_instance();
  if ((char*)NULL != self->bug_report_message) {
    FREE(self->bug_report_message);
  }
  self->bug_report_message = util_strsav(bug_report_message);
}

char* NuSMVCore_get_linked_addons()
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->linked_addons;
}

void NuSMVCore_set_linked_addons(char* linked_addons)
{
  CoreData_ptr self = nusmv_core_get_instance();
  if ((char*)NULL != self->linked_addons) {
    FREE(self->linked_addons);
  }
  self->linked_addons = util_strsav(linked_addons);
}

char* NuSMVCore_get_library_name()
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->library_name;
}

void NuSMVCore_set_library_name(const char * library_name)
{
  CoreData_ptr self = nusmv_core_get_instance();
  self->library_name = util_strsav(library_name);
}

char* NuSMVCore_get_library_version()
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->library_version;
}

char* NuSMVCore_get_library_build_date()
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->library_build_date;
}

void NuSMVCore_set_library_build_date(const char * library_build_date)
{
  CoreData_ptr self = nusmv_core_get_instance();
  self->library_build_date = util_strsav(library_build_date);
}

char* NuSMVCore_get_library_email()
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->library_email;
}

void NuSMVCore_set_library_email(const char * library_email)
{
  CoreData_ptr self = nusmv_core_get_instance();
  self->library_email = util_strsav(library_email);
}

char* NuSMVCore_get_library_website()
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->library_website;
}

void NuSMVCore_set_library_website(const char * library_website)
{
  CoreData_ptr self = nusmv_core_get_instance();
  self->library_website = util_strsav(library_website);;
}

char* NuSMVCore_get_library_bug_report_message()
{
  CoreData_ptr self = nusmv_core_get_instance();
  return self->library_bug_report_message;
}

void NuSMVCore_set_banner_print_fun(void (*banner_print_fun)(FILE *))
{
  CoreData_ptr self = nusmv_core_get_instance();
  self->print_banner = banner_print_fun;
}

void NuSMVCore_set_batch_fun(void (*batch_fun)(NuSMVEnv_ptr))
{
  CoreData_ptr self = nusmv_core_get_instance();
  self->batch = batch_fun;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Initializes (once) the internal core_data variable

  Initializes (once) the internal core_data variable

  \sa nusmv_core_deinit
*/
static CoreData_ptr nusmv_core_get_instance(void)
{
  if ((CoreData_ptr)NULL == core_data) {
    core_data = ALLOC(CoreData, 1);

    nusmv_assert((CoreData_ptr)NULL != core_data);

    core_data->tool_name = (char*)NULL;
    core_data->tool_rcfile = (char*)NULL;
    core_data->tool_version = (char*)NULL;
    core_data->build_date = (char*)NULL;
    core_data->prompt_string = (char*)NULL;
    core_data->email = (char*)NULL;
    core_data->website = (char*)NULL;
    core_data->bug_report_message = (char*)NULL;
    core_data->linked_addons = (char*)NULL;

    core_data->library_name = (char*)NULL;
    core_data->library_version = (char*)NULL;
    core_data->library_build_date = (char*)NULL;
    core_data->library_email = (char*)NULL;
    core_data->library_website = (char*)NULL;
    core_data->library_bug_report_message = (char*)NULL;

    core_data->print_banner = NULL;
    core_data->batch = NULL;

    core_data->line_options = new_assoc();

    core_data->string_mgr =  UStringMgr_create();
  }

  return core_data;
}

/*!
  \brief Deinitializes and frees the internal variable
   core_data

  Deinitializes and frees the internal variable
   core_data. This function should be called
   only when terminating the program

  \sa nusmv_core_get_instance
*/
static void nusmv_core_deinit(void)
{
  if ((CoreData_ptr)NULL != core_data) {

    if ((char*)NULL != core_data->tool_name) {
      FREE(core_data->tool_name);
    }
    if ((char*)NULL != core_data->tool_rcfile) {
      FREE(core_data->tool_rcfile);
    }
    if ((char*)NULL != core_data->tool_version) {
      FREE(core_data->tool_version);
    }
    if ((char*)NULL != core_data->build_date) {
      FREE(core_data->build_date);
    }
    if ((char*)NULL != core_data->prompt_string) {
      FREE(core_data->prompt_string);
    }
    if ((char*)NULL != core_data->email) {
      FREE(core_data->email);
    }
    if ((char*)NULL != core_data->website) {
      FREE(core_data->website);
    }
    if ((char*)NULL != core_data->bug_report_message) {
      FREE(core_data->bug_report_message);
    }
    if ((char*)NULL != core_data->linked_addons) {
      FREE(core_data->linked_addons);
    }

    if ((char*)NULL != core_data->library_name) {
      FREE(core_data->library_name);
    }
    if ((char*)NULL != core_data->library_version) {
      FREE(core_data->library_version);
    }
    if ((char*)NULL != core_data->library_build_date) {
      FREE(core_data->library_build_date);
    }
    if ((char*)NULL != core_data->library_email) {
      FREE(core_data->library_email);
    }
    if ((char*)NULL != core_data->library_bug_report_message) {
      FREE(core_data->library_bug_report_message);
    }
    if ((char*)NULL != core_data->library_website) {
      FREE(core_data->library_website);
    }

     UStringMgr_destroy(core_data->string_mgr);

    FREE(core_data);
  }
}

/*!
  \brief Initializes the internal representation structure
   of a command line option

  Initializes the internal representation structure
   of a command line option

  \sa nusmv_core_deinit_opt
*/
static CmdLineOpt_ptr nusmv_core_init_opt()
{
  CmdLineOpt_ptr opt = ALLOC(CmdLineOpt, 1);

  opt->name = (char*)NULL;
  opt->usage = (char*)NULL;
  opt->parameter = (char*)NULL;

  opt->check_and_apply = NULL;
  opt->env_option = (char*)NULL;

  opt->deprecated = false;
  opt->public = false;

  opt->dependency = (string_ptr)NULL;
  opt->conflicts = Olist_create();

  return opt;
}

/*!
  \brief Deinitializes the internal representation structure
   of a command line option

  Deinitializes and frees the internal representation
   structure of a command line option

  \sa nusmv_core_init_opt
*/
static void nusmv_core_deinit_opt(CmdLineOpt_ptr opt)
{
  nusmv_assert((CmdLineOpt_ptr)NULL != opt);

  nusmv_assert((char*)NULL != opt->name);
  FREE(opt->name);
  nusmv_assert((char*)NULL != opt->usage);
  FREE(opt->usage);

  if ((char*)NULL != opt->parameter) { FREE(opt->parameter); }
  if ((char*)NULL != opt->env_option) { FREE(opt->env_option); }

  Olist_destroy(opt->conflicts);

  FREE(opt);
}

/*!
  \brief Prints the command line option usages

  Prints the command line option usages.
   If print_banner is true, also the banner is
   printed out. Printed command line options are
   taken from the list of registered ones.

  \sa NuSMVCore_add_command_line_option
   NuSMVCore_add_env_command_line_option
*/
static void nusmv_core_print_usage(NuSMVEnv_ptr env, boolean print_banner)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  CoreData_ptr data = nusmv_core_get_instance();
  OptsHandler_ptr opts = NuSMVEnv_get_value(env, ENV_OPTS_HANDLER);

  /* For lexical ordered printing */
  avl_tree* avl = avl_init_table((int (*)(char *, char *))Utils_strcasecmp);
  avl_generator* gen;
  char* option_name;
  char* option_struct_ptr;

  string_ptr key;
  CmdLineOpt_ptr opt;
  assoc_iter iter;

  /* Prepare the avl tree.. */
  ASSOC_FOREACH(data->line_options, iter, &key, &opt) {
    nusmv_assert((CmdLineOpt_ptr)NULL != opt);

    /* We print the usage only if the option should be available to
       the user */
    if (opt->public) {
      avl_insert(avl, (char*) UStringMgr_get_string_text(key), (char*)opt);
    }
  }

  /* Print the banner, if necessary */
  if (!opt_get_quiet_mode(opts) && print_banner) {
    data->print_banner(errstream);
  }

  StreamMgr_print_error(streams,  "Usage:  %s [-h | -help] [options]* [input_file]\n",
          data->tool_name);

  gen = avl_init_gen(avl, AVL_FORWARD);

  StreamMgr_print_error(streams,  "   -h | -help\n");
  nusmv_core_print_string(errstream, "prints out current message", 6);

  /* Now print all options.. */
  while (avl_gen(gen, &option_name, &option_struct_ptr)) {
    CmdLineOpt_ptr option_struct = (CmdLineOpt_ptr)option_struct_ptr;
    char* tmp;

    /* ---------- OPTION NAME + PARAMETER -------------- */
    if ((char*)NULL != option_struct->parameter) {
      tmp = ALLOC(char, strlen(option_name) +
                  strlen(option_struct->parameter) + 2);
      sprintf(tmp, "%s %s", option_name, option_struct->parameter);
    }
    else {
      tmp = ALLOC(char, strlen(option_name) + 1);
      sprintf(tmp, "%s", option_name);
    }
    nusmv_core_print_string(errstream, tmp, 3);

    /* ---------- OPTION USAGE ------------------------- */
    nusmv_core_print_string(errstream, option_struct->usage, 6);

    /* ---------- OPTION DEPENDENCIES ------------------ */
    if ((string_ptr)NULL != option_struct->dependency) {
      const char* fmt = "NOTE: Requires option \"%s\"";
      tmp = REALLOC(char, tmp, strlen(fmt) +
                    strlen(UStringMgr_get_string_text(option_struct->dependency)) + 1);

      sprintf(tmp, fmt, UStringMgr_get_string_text(option_struct->dependency));
      nusmv_core_print_string(errstream, tmp, 6);
    }

    /* ---------- OPTION CONFICTS ---------------------- */
    if (!Olist_is_empty(option_struct->conflicts)) {
      const char* fmt = "NOTE: Incompatible with option%s %s";
      char* conf = nusmv_core_merge(option_struct->conflicts);
      tmp = REALLOC(char, tmp, strlen(fmt) + strlen(conf) + 2);

      sprintf(tmp, fmt,
              (Olist_get_size(option_struct->conflicts) > 1 ? "s" : ""),
              conf);

      nusmv_core_print_string(errstream, tmp, 6);
      FREE(conf);
    }

    /* ---------- OPTION IS DEPRECATED ----------------- */
    if (option_struct->deprecated) {
      const char* fmt = "WARNING: option \"%s\" is deprecated";
      tmp = REALLOC(char, tmp, strlen(fmt) + strlen(option_name) + 1);

      sprintf(tmp, fmt, option_name);
      nusmv_core_print_string(errstream, tmp, 6);
    }

    FREE(tmp);
  }

  StreamMgr_print_error(streams,  "   input-file\n");
  nusmv_core_print_string(errstream,
                          "the file both the model and "
                          "the spec were read from", 6);

  avl_free_gen(gen);
  avl_free_table(avl, 0, 0);
}

/*!
  \brief Prints the given string on the given stream, padded by
   the given number.

  Prints the given string on the given stream, padded by
   the given number. If the given string is longer than MAX_PRINT_WIDTH, then
   a new-line is added and the remaining part of the string is prinded
   (padded as the previous one). The string is divided in such way
   that no words are truncated. Words are separated by spaces.
*/
static void nusmv_core_print_string(FILE* out, char* str, int padding)
{
#if 1
  /*
     This version may generate lines longer than
     MAX_PRINT_WIDTH. Indeed, it prints words (i.e. sequence of chars
     without ' '), and if the line of the printed sequence exceeds the
     MAX_PRINT_WIDTH, then it adds a new line. This implementation
     removes subsequent ' ' chars, and subsequent '\n' chars.
   */
  int j = 0, i = 0, c = 0, d = 0;

  for (i = 0; i < padding; ++i, ++c) {fputc(' ', out); }

  for (i = 0; '\0' != str[i]; ++i) {
    if (0 == c) {
      for (j = 0; j < padding; ++j, ++c) {fputc(' ', out); }
    }
    if ('\n' == str[i]) {
      if ('\0' != str[i+1] && '\n' != str[i+1]) {
        fputc(str[i], out);
        c = 0; d = 0;
      }
    }
    else if (' ' == str[i]) {
     if ('\0' != str[i+1] && ' ' != str[i+1] && 0 != d) {
       fputc(str[i], out);
       if (c >= MAX_PRINT_WIDTH) {
         fputc('\n', out);
         c = 0; d = 0;
       }
     }
    }
    else {
      c += 1; d += 1;
      fputc(str[i], out);
    }
  }
  if (0 < i && '\n' != str[i-1]) {
    fputc('\n', out);
  }
#else
  int i = 0;
  int j = padding;

  /* Lines cannot be more than MAX_PRINT_WIDTH characters long */
  char buff[MAX_PRINT_WIDTH + 1];

  for (i = 0; i < padding; ++i) {
    buff[i] = ' ';
  }

  for (i = 0; '\0' != str[i]; ++i) {

    if (str[i] == '\n' || j >= MAX_PRINT_WIDTH) {
      /* It is a letter! It may mean that the word will be
         cutted.. */
      /* [MD] It was always true: */
      /* if (str[i] != '\n' || str[i] != ' ') { */
      if (str[i] != '\n' && str[i] != ' ') {
        int k = 0, z = 0;
        int stolen = 0;
        char tmp[MAX_PRINT_WIDTH + 1] = "";

        /* Find where the last word ends.. */
        while (str[i - z] != ' ' && str[i - z] != '\n') { ++z; }

        /* Same the partial word, should be printed later.. */
        for (k = 0; k < z; k++) { tmp[k] = str[(i - z) + k]; }

        /* Remove the last partial word to the buffer... */
        buff[j - z] = '\0';
        fprintf(out, "%s\n", buff);

        /* ... add it to the next buffer! */
        for (k = 0, j = padding; j < (padding + z); ++k) {
          if (tmp[k] != ' ' && tmp[k] != '\n') {
            buff[j++] = tmp[k];
          }
          else {
            stolen++;
          }
        }
        j -= stolen;
      }
      else {
        buff[j] = '\0';
        fprintf(out, "%s\n", buff);
        j = padding;
      }
    }

    /* In this way, we aviod duplicated new lines, and white spaces at
       the beginning or at the end of a string. */
    if (str[i] != '\n' &&
        !(str[i] == ' ' && (j == padding || j >= MAX_PRINT_WIDTH))) {
      buff[j++] = str[i];
    }

  }

  buff[j] = '\0';
  fprintf(out, "%s\n", buff);
#endif
}

/*!
  \brief Parses the given command line options.

  Parses the given command line options.
   -h, -help and input-file are hardcoded options,
   all other options should be registered using
   NuSMVCore_add_env_command_line_option or
   NuSMVCore_add_command_line_option

  \sa NuSMVCore_add_env_command_line_option
   NuSMVCore_add_command_line_option
*/
static int nusmv_core_parse_line_options(NuSMVEnv_ptr env, int argc, char ** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  int status = RET_SUCCESS;
  CoreData_ptr data = nusmv_core_get_instance();
  OptsHandler_ptr opts = NuSMVEnv_get_value(env, ENV_OPTS_HANDLER);

  hash_ptr required = new_assoc();
  hash_ptr conflicting = new_assoc();
  Olist_ptr line_options = Olist_create();

  /* parses the Program Name */
  argc--;
  set_pgm_path(opts, *(argv++));

  while (argc > 0) {
    /* -help is hardcoded */
    if (strcmp(*argv, "-help") == 0 ||
        strcmp(*argv, "-h") == 0) {
      argv++; argc--;
      nusmv_core_print_usage(env, true);
      status = RET_HELP_PRINT;
      break;
    }
    /* The input file (last argument) is hardcoded */
    else if (argc == 1 && (**argv) != '-'){
      set_input_file(opts, *(argv++));
      argc--;
    }
    else {
      /* Lookup for the cmd line option. */
      char* opt_string = *argv;
      string_ptr opt_name =  UStringMgr_find_string(data->string_mgr, opt_string);
      CmdLineOpt_ptr opt = (CmdLineOpt_ptr)find_assoc(data->line_options,
                                                      NODE_PTR(opt_name));

      /* Skip already given command options. */
      if (Olist_contains(line_options, opt_name)) {
        StreamMgr_print_error(streams,
                "Warning: Option \"%s\" given more than once.\n"
                "Warning: Only the first occurrence will be taken into account\n",
                opt_string);
      }
      else {

        /* Add the cmd line option to the set of all defined cmd options. */
        Olist_append(line_options, opt_name);

        /* The cmd line option exists! */
        if ((CmdLineOpt_ptr)NULL != opt) {
          char* param = (char*)NULL;

          /* Add to the required table the dependencies of this option */
          insert_assoc(required, NODE_PTR(opt_name), NODE_PTR(opt->dependency));

          /* Add to the conflicting table the options conflicting with the
             current one */
          insert_assoc(conflicting, NODE_PTR(opt_name), NODE_PTR(opt->conflicts));

          /* A parameter is requested! */
          if ((char*)NULL != opt->parameter) {
            if (argc < 2) {
              StreamMgr_print_error(streams,
                      "The \"%s\" command line option requires an argument.\n",
                      opt_string);
              status = RET_MISSING_OPTION_PARAM;
              break;
            }
            argc--; argv++;
            param = *argv;
          }

          /* The cmd line option has an env option associated. */
          if ((char*)NULL != opt->env_option) {

            /* The cmd line option should not have both an env var and a
               check&apply fun.*/
            nusmv_assert(NULL == opt->check_and_apply);

            /* The env var has to exist.*/
            nusmv_assert(OptsHandler_is_option_registered(opts,
                                                          opt->env_option));

            /* If the option is boolean, invert the value. */
            if (OptsHandler_is_bool_option(opts, opt->env_option)) {
              boolean res;
              boolean curr;

              nusmv_assert((char*)NULL == param);
              curr = OptsHandler_get_bool_option_default_value(
                  opts,
                  opt->env_option);

              res = OptsHandler_set_bool_option_value(opts,
                                                      opt->env_option,
                                                      !curr);
              if (!res) {
                StreamMgr_print_error(streams,
                        "An error occurred with option \"%s\"\n", opt_string);
                status = RET_INVALID_OPTION;
              }
            }
            else {
              boolean res;

              nusmv_assert((char*)NULL != param);

              res = OptsHandler_set_option_value(opts,
                                                 opt->env_option, param);
              if (!res) {
                StreamMgr_print_error(streams,
                                      "Cannot set value \"%s\" "
                                      "to option \"%s\"\n",
                                      param, opt_string);
                status = RET_INVALID_OPTION_PARAM;
                break;
              }
            }
          }
          /* The cmd line option has it's own check & apply function */
          else {
            boolean res;

            nusmv_assert(NULL == opt->env_option);

            res = opt->check_and_apply(opts, param, env);

            if (!res) {
              if ((char*)NULL != param) {
                StreamMgr_print_error(streams, "Cannot set value \"%s\" "
                                      "to option \"%s\"\n",
                                      param, opt_string);
                status = RET_INVALID_OPTION_PARAM;
              }
              else {
                StreamMgr_print_error(streams, "Cannot use option \"%s\"\n",
                                      opt_string);
                status = RET_INVALID_OPTION;
              }
              break;
            }
          }

          if (opt->deprecated) {
            StreamMgr_print_error(streams, "Warning: %s is deprecated\n",
                                  opt_string);
          }
        }
        else { /* The option does not exist */
          StreamMgr_print_error(streams,
                  "The command line option \"%s\" is unknown\n",
                  opt_string);
          status = RET_UNKNOWN_OPTION;
          break;
        }
      } /* Command line option not already given */

      argc--; argv++;
    } /* Non-existing or not hardcoded option */
  }

  /* Lookup for conflicts and unsatisfied dependencies */
  if (status == RET_SUCCESS) {
    Oiter iter;
    Olist_ptr printed = Olist_create();

    OLIST_FOREACH(line_options, iter) {
      string_ptr option =
        (string_ptr)Oiter_element(iter);

      Olist_ptr conflicts =
        (Olist_ptr)find_assoc(conflicting, NODE_PTR(option));

      string_ptr dependency =
        (string_ptr)find_assoc(required, NODE_PTR(option));

      /* Found conflicts which have not been already printed! */
      if (!Olist_is_empty(conflicts) &&
          !Olist_contains(printed, option)) {
        Olist_ptr intersect =
          nusmv_core_olist_intersection(conflicts, line_options);

        nusmv_core_olist_union(printed, intersect);

        if (!Olist_is_empty(intersect)) {
          const char* fmt = "Option %s cannot be used with option%s %s";
          char* conf = nusmv_core_merge(intersect);
          char* tmp;

          tmp = ALLOC(char, strlen(conf) +
                      strlen(UStringMgr_get_string_text(option)) +
                      strlen(fmt) + 1 + (Olist_get_size(intersect) > 1));

          sprintf(tmp, fmt, UStringMgr_get_string_text(option),
                  (Olist_get_size(intersect) > 1 ? "s" : ""), conf);
          FREE(conf);

          nusmv_core_print_string(errstream, tmp, 0);
          status = RET_CONFLICT_OPTION;
          FREE(tmp);
        }

        Olist_destroy(intersect);
      }

      /* Unmet dependencies */
      if (((string_ptr)NULL != dependency) &&
          (!Olist_contains(line_options, dependency))) {

        StreamMgr_print_error(streams,
                              "Option \"%s\" requires option \"%s\"\n",
                              UStringMgr_get_string_text(option),
                              UStringMgr_get_string_text(dependency));

        status = RET_MISSING_OPTION_DEP;
      }
    }

    Olist_destroy(printed);
  }

  free_assoc(required);
  free_assoc(conflicting);
  Olist_destroy(line_options);

  return status;
}

/*!
  \brief Given a set of unique strings, returns a string
   representing the set of strings, separated by a
   white space

  Given a set of unique strings, returns a string
   representing the set of strings, separated by a
   white space

  \sa nusmv_core_split
*/
static char* nusmv_core_merge(Olist_ptr set)
{
  char* result = ALLOC(char, 1);
  Oiter iter;

  result[0] = '\0';

  OLIST_FOREACH(set, iter) {
    string_ptr conf = (string_ptr)Oiter_element(iter);
    const char* str = UStringMgr_get_string_text(conf);
    char* tmp;

    tmp = ALLOC(char, strlen(result) + 1);
    sprintf(tmp, "%s", result);

    result = REALLOC(char, result, strlen(result) + strlen(str) + 2);
    sprintf(result, "%s%s ", tmp, str);
  }

  return result;
}


/*!
  \brief Aux function for the nusmv_core_split function

  Aux function for the nusmv_core_split function

  \sa nusmv_core_split
*/

static int nusmv_core_get_next_word_length(char* string)
{
  char* pos;
  nusmv_assert((char*)NULL != string);

  /* Skip prefix white spaces */
  while (string[0] == ' ') { string++; }

  pos = strchr(string, ' ');

  /* No spaces, but a word exists. */
  if ((char*)pos == NULL) {
    return strlen(string);
  }

  return (pos - string);
}

/*!
  \brief Given a string of white-space separated strings,
   splits the string and builds a set of unique strings

  Given a string of white-space separated strings,
   splits the string and builds a set of unique strings

  \sa nusmv_core_merge
*/
static Olist_ptr nusmv_core_split( UStringMgr_ptr strmgr, char* string)
{
  Olist_ptr result = Olist_create();
  char* tmp = (char*)NULL;
  int i = 0;
  int j = 0;
  int next_length = nusmv_core_get_next_word_length(string);

  /* There is at least one word */
  if (next_length > 0) {
    /* Allocate the first memory for the first word */
    tmp = ALLOC(char, next_length + 1);

    /* Until the string is empty */
    for (i = 0; '\0' != string[i]; ++i) {
      if (string[i] == ' ') {
        /* Skip consecutives spaces */
        if (j > 0) {
          /* Found a word, put the terminal and push the ustring in
             the list */
          tmp[j] = '\0';
          Olist_append(result,  UStringMgr_find_string(strmgr, tmp));

          /* Reset the index and re-allocate memory for the next
             word */
          j = 0;
          next_length = nusmv_core_get_next_word_length(string);

          /* No new words left */
          if (next_length <= 0) { break; }
          tmp = REALLOC(char, tmp, next_length + 1);
        }
      }
      else {
        tmp[j++] = string[i];
      }
    }

    if (j > 0) {
      tmp[j] = '\0';
      Olist_append(result,  UStringMgr_find_string(strmgr, tmp));
    }

    FREE(tmp);
  }

  return result;
}

/*!
  \brief Lowercases a string

  Lowercases a string

  \sa nusmv_core_merge
*/
static char* nusmv_core_tolower(char* str)
{
  char* ret = ALLOC(char, strlen(str) + 1);
  int i;

  *ret = '\0';
  for (i = 0; '\0' != str[i]; ++i) {
    ret[i] = (isupper(str[i]) ? tolower(str[i]) : str[i]);
  }
  ret[i] = '\0';
  return ret;
}

/*!
  \brief Check and apply function for the -sin cmd line opt

  Check and apply function for the -sin cmd line opt
*/
static boolean nusmv_core_check_sin_fun(OptsHandler_ptr opt, char* val,
                                        NuSMVEnv_ptr env)
{
  if (strcmp(val, "off") == 0) {
    return OptsHandler_set_bool_option_value(opt, SYMB_INLINING, false);
  }
  else if (strcmp(val, "on") == 0) {
    return OptsHandler_set_bool_option_value(opt, SYMB_INLINING, true);
  }

  return false;
}

/*!
  \brief Check and apply function for the -rbc cmd line opt

  Check and apply function for the -rbc cmd line opt
*/
static boolean nusmv_core_check_rbc_fun(OptsHandler_ptr opt, char* val,
                                        NuSMVEnv_ptr env)
{
  if (strcmp(val, "off") == 0) {
    return OptsHandler_set_bool_option_value(opt, RBC_INLINING, false);
  }
  else if (strcmp(val, "on") == 0) {
    return OptsHandler_set_bool_option_value(opt, RBC_INLINING, true);
  }

  return false;
}

/*!
  \brief Check and apply function for the -mono cmd line opt

  Check and apply function for the -mono cmd line opt
*/
static boolean nusmv_core_set_mono_partition(OptsHandler_ptr opt, char* val,
                                             NuSMVEnv_ptr env)
{
  return OptsHandler_set_enum_option_value(opt, PARTITION_METHOD,
                                           TRANS_TYPE_MONOLITHIC_STRING);
}

/*!
  \brief Check and apply function for the -iwls95 cmd line opt

  Check and apply function for the -iwls95 cmd line opt
*/
static boolean nusmv_core_set_iwls95_partition(OptsHandler_ptr opt, char* val,
                                               NuSMVEnv_ptr env)
{
  boolean res = OptsHandler_set_enum_option_value(opt, PARTITION_METHOD,
                                                  TRANS_TYPE_IWLS95_STRING);

  res &= OptsHandler_set_option_value(opt, IMAGE_CLUSTER_SIZE, val);

  return res;
}

/*!
  \brief Check and apply function for the -thresh cmd line opt

  Check and apply function for the -thresh cmd line opt
*/
static boolean nusmv_core_set_thresh_partition(OptsHandler_ptr opt, char* val,
                                               NuSMVEnv_ptr env)
{
  boolean res = OptsHandler_set_enum_option_value(opt, PARTITION_METHOD,
                                                  TRANS_TYPE_THRESHOLD_STRING);

  res &= OptsHandler_set_option_value(opt, CONJ_PART_THRESHOLD, val);

  return res;
}

/*!
  \brief Check and apply function for the -cpp cmd line opt

  Check and apply function for the -cpp cmd line opt
*/
static boolean nusmv_core_set_cpp(OptsHandler_ptr opt, char* val,
                                  NuSMVEnv_ptr env)
{
  char* pp_list = OptsHandler_get_string_option_value(opt, PP_LIST);
  if (strcmp(pp_list, "") == 0) {
    set_pp_list(opt, "cpp", env);
  }
  else {
    char* new_pp_list;
    new_pp_list = ALLOC(char, strlen(pp_list) + 5);
    strcpy(new_pp_list, "cpp ");
    strcat(new_pp_list, pp_list);
    set_pp_list(opt, new_pp_list, env);
    FREE(new_pp_list);
  }

  return true;
}

/*!
  \brief Check and apply function for the -pre cmd line opt

  Check and apply function for the -pre cmd line opt
*/
static boolean nusmv_core_set_pre(OptsHandler_ptr opt, char* val,
                                  NuSMVEnv_ptr env)
{
  char* pp_list = OptsHandler_get_string_option_value(opt, PP_LIST);
  char* new_value;
  boolean result;

  if (strcmp(pp_list, "") == 0) {
    new_value = util_strsav(val);
  }
  else {
    new_value = ALLOC(char, strlen(pp_list) + strlen(val) + 2);
    sprintf(new_value, "%s %s", val, pp_list);
  }

  result = OptsHandler_set_option_value(opt, PP_LIST, new_value);

  FREE(new_value);
  return result;
}

/*!
  \brief Check and apply function for the -dp cmd line opt

  Check and apply function for the -dp cmd line opt
*/
static boolean nusmv_core_set_dp(OptsHandler_ptr opt, char* val,
                                 NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  StreamMgr_print_error(streams,
                        "WARNING: Disjunctive partitioning is no longer supported.\n");
  return false;
}

/*!
  \brief Check and apply function for the -f cmd line opt

  Check and apply function for the -f cmd line opt
*/
static boolean
core_data_set_fs(OptsHandler_ptr opt, char* val, NuSMVEnv_ptr env)
{
  return true;
}

/*!
  \brief Calculates the intersection list between a and b

  Calculates the intersection list between a and b.
                       The returned list must be freed by the caller
*/
static Olist_ptr nusmv_core_olist_intersection(Olist_ptr a, Olist_ptr b)
{
  Olist_ptr res = Olist_create();
  Oiter iter;

  OLIST_FOREACH(a, iter) {
    void* el = Oiter_element(iter);
    if (Olist_contains(b, el)) {
      Olist_append(res, el);
    }
  }

  return res;
}

/*!
  \brief Adds all elements in b to a, if a does
                       not contain it already

  Adds all elements in b to a, if a does
                       not contain it already
*/
static void nusmv_core_olist_union(Olist_ptr a, Olist_ptr b)
{
  Oiter iter;

  OLIST_FOREACH(b, iter) {
    void* el = Oiter_element(iter);
    if (!Olist_contains(a, el)) {
      Olist_append(a, el);
    }
  }

}

/*!
  \brief Frees the line_options hash and all it's contents

  Frees the line_options hash and all it's contents
*/
static void nusmv_core_free_line_options(CoreData_ptr core_data)
{
  /* We need to destroy complex structure BEFORE quitting all
     packages. */

  if ((hash_ptr)NULL != core_data->line_options) {
    string_ptr key;
    CmdLineOpt_ptr opt;
    assoc_iter iter;

    ASSOC_FOREACH(core_data->line_options, iter, &key, &opt) {
      nusmv_core_deinit_opt(opt);
    }
    free_assoc(core_data->line_options);
    core_data->line_options = (hash_ptr)NULL;
  }

}


/* This starts the interactive loop of the shell */
static int nusmv_core_start_interactive_shell_loop(NuSMVEnv_ptr env)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  OptsHandler_ptr opts = NuSMVEnv_get_value(env, ENV_OPTS_HANDLER);

  int quit_flag = 0;

#if NUSMV_HAVE_INTERACTIVE_SHELL
  if (!opt_ignore_init_file(opts)) {
    Cmd_Misc_NusmvrcSource(env);
  }

  /* There exists a script file */
  if ((char*)NULL != get_script_file(opts)) {
    char* script_file = get_script_file(opts);

    if (Utils_file_exists(script_file)) {
      char* command = ALLOC(char, strlen(script_file)
                            + strlen("source ") + 1);
      nusmv_assert(NULL != command);
      sprintf(command, "source %s", script_file);
      quit_flag = Cmd_CommandExecute(env, command);
      FREE(command);
    }
    else {
      StreamMgr_print_error(streams,  "No such file or directory. Exiting...\n");
      quit_flag = -5; /* require immediate quit */
    }
  }

  while (quit_flag >= 0) {
    quit_flag = Cmd_CommandExecute(env, "source -ip -");
  }

#else  /* NUSMV_HAVE_INTERACTIVE_SHELL */
  StreamMgr_print_error(streams,  "Interactive shell not available, use batch mode\n");
  quit_flag = -5; /* require immediate quit */
#endif  /* NUSMV_HAVE_INTERACTIVE_SHELL */

  return quit_flag;
}
