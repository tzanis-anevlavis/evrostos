/* ---------------------------------------------------------------------------


  This file is part of the ``bmc.sbmc'' package of NuSMV version 2.
  Copyright (C) 2004 Timo Latvala <timo.latvala@tkk.fi>
  Copyright (C) 2006 Tommi Junttila.

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
  \author Timo Latvala, Tommi Juntilla, Marco Roveri
  \brief Bmc.Cmd module

  This module contains all the sbmc commands implementation.
  Options parsing and checking is performed here, than the high-level SBMC
  layer is called

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/shell/cmd/cmd.h"
#include "nusmv/shell/bmc/sbmc/sbmcCmd.h"
#include "nusmv/shell/bmc/bmcCmd.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/bmc/sbmc/sbmcBmc.h"
#include "nusmv/core/bmc/sbmc/sbmcBmcInc.h"
#include "nusmv/core/bmc/sbmc/sbmcGen.h"
#include "nusmv/core/bmc/bmc.h"
#include "nusmv/core/bmc/bmcUtils.h"

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"

#include "nusmv/core/prop/Prop.h"
#include "nusmv/core/prop/PropDb.h"
#include "nusmv/core/prop/propPkg.h"

#include "nusmv/core/enc/enc.h"
#include "nusmv/core/sat/sat.h"

/* ---------------------------------------------------------------------- */


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define HAS_TO_CHECK_COMPL true

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define HAS_TO_UNROLL true

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

static int sbmc_CommandCheckPslSpecSbmc(NuSMVEnv_ptr env, int argc, char** argv);

#if NUSMV_HAVE_INCREMENTAL_SAT
static int sbmc_CommandCheckPslSpecSbmcInc(NuSMVEnv_ptr env, int argc, char** argv);
#endif

static int UsageSBMCCheckLtlSpec(const NuSMVEnv_ptr env);
static int UsageSBMCGenLtlSpec(const NuSMVEnv_ptr env);
static int UsageSBMCIncCheck(const NuSMVEnv_ptr env);
static int UsageCheckPslSpecSbmc(const NuSMVEnv_ptr env);
static int UsageCheckPslSpecSbmcInc(const NuSMVEnv_ptr env);

static Outcome
sbmc_cmd_options_handling(NuSMVEnv_ptr env,
                          int argc, char** argv,
                          Prop_Type prop_type,

                          /* output parameters: */
                          Prop_ptr* res_prop,
                          int* res_k,
                          int* res_l,
                          char** res_o,
                          boolean* res_N,
                          boolean* res_c,
                          boolean* res_1);

static inline int
sbmc_cmd_gen_solve_zigzag_inc_selected_or_all_props(const NuSMVEnv_ptr env,
                                                    const Prop_ptr ltlprop,
                                                    const int k,
                                                    const boolean do_virtual_unrolling,
                                                    const boolean do_completeness_check);

static inline int
sbmc_cmd_gen_solve_ltl_selected_or_all_props(const NuSMVEnv_ptr env,
                                             const Prop_ptr ltlprop,
                                             const int k,
                                             const int relative_loop,
                                             const char* fname,
                                             const boolean is_single_prob,
                                             const boolean has_to_solve);

static inline int
sbmc_cmd_gen_solve_psl_selected_or_all_props(NuSMVEnv_ptr const env,
                                             const int prop_no,
                                             const boolean bmc_dump,
                                             const boolean inc_sat,
                                             const boolean single_bmc_prob,
                                             const int k,
                                             const int l,
                                             const boolean check_compl,
                                             const boolean does_virtual_unrol);

/**AutomaticEnd***************************************************************/



/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void SBmc_AddCmd(NuSMVEnv_ptr env)
{
  /* These commands are re-entrant wrt Ctrl+C */
  Cmd_CommandAdd(env, "check_ltlspec_sbmc", Sbmc_CommandCheckLtlSpecSBmc, 0,
                 true);
  Cmd_CommandAdd(env, "gen_ltlspec_sbmc", Sbmc_CommandGenLtlSpecSBmc, 0, true);
  Cmd_CommandAdd(env, "check_pslspec_sbmc", sbmc_CommandCheckPslSpecSbmc, 0,
                 true);

#if NUSMV_HAVE_INCREMENTAL_SAT
  Cmd_CommandAdd(env, "check_ltlspec_sbmc_inc", Sbmc_CommandLTLCheckZigzagInc,
                 0, true);
  Cmd_CommandAdd(env, "check_pslspec_sbmc_inc", sbmc_CommandCheckPslSpecSbmcInc,
                 0, true);
#endif

}

void Sbmc_Cmd_quit(NuSMVEnv_ptr env)
{
  boolean status = true;

  status = status && Cmd_CommandRemove(env, "check_ltlspec_sbmc");
  status = status && Cmd_CommandRemove(env, "gen_ltlspec_sbmc");
  status = status && Cmd_CommandRemove(env, "check_pslspec_sbmc");

#if NUSMV_HAVE_INCREMENTAL_SAT
  status = status && Cmd_CommandRemove(env, "check_ltlspec_sbmc_inc");
  status = status && Cmd_CommandRemove(env, "check_pslspec_sbmc_inc");
#endif

  nusmv_assert(status);
}

int Sbmc_CommandCheckLtlSpecSBmc(NuSMVEnv_ptr env, int argc, char** argv)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  Prop_ptr ltlprop=(Prop_ptr)NULL;
  Outcome opt_handling_res;
  int k = get_bmc_pb_length(opts);
  boolean is_single_prob = false;
  char *fname = (char *)NULL;
  int relative_loop =
    Bmc_Utils_ConvertLoopFromString(get_bmc_pb_loop(opts), NULL);
  int res = 1;

  /* ----------------------------------------------------------------------- */
  /* Options handling: */
  opt_handling_res = sbmc_cmd_options_handling(env, argc, argv,
                                               Prop_Ltl, &ltlprop,
                                               &k, &relative_loop,
                                               &fname,
                                               NULL, /* -N */
                                               NULL, /* -c */
                                               &is_single_prob);

  if (opt_handling_res == OUTCOME_SUCCESS_REQUIRED_HELP) {
    return UsageSBMCCheckLtlSpec(env);
  }
  if (opt_handling_res != OUTCOME_SUCCESS) {
    if (fname != (char *)NULL) FREE(fname);
    return 1;
  }

  /* makes sure bmc has been set up */
  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }

  /* ----------------------------------------------------------------------- */

  res =
    sbmc_cmd_gen_solve_ltl_selected_or_all_props(env, ltlprop, k, relative_loop,
                                                 fname, is_single_prob,
                                                 BMC_HAS_TO_SOLVE);

  FREE(fname); fname = (char*)NULL;

  return res;
}

/*!
  \brief


*/
static int UsageSBMCCheckLtlSpec(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "\nusage: check_ltlspec_sbmc [-h | -n idx | -p \"formula\"] [-k max_length] [-l loopback]\n\t\t\t [-1] [-o <filename>]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage\n");
  StreamMgr_print_error(streams,  "  -n idx\tChecks the LTL property specified with <idx>\n");
  StreamMgr_print_error(streams,  "  -p \"formula\"\tChecks the specified LTL property\n");
  StreamMgr_print_error(streams,  "  -P \"name\"\tChecks the LTL property specified with <name>.\n");
  StreamMgr_print_error(streams,  "\t\tIf no property is specified, checks all LTL properties.\n");
  StreamMgr_print_error(streams,  "  -k max_length\tChecks the property using <max_length> value instead of using the\n\t\tvariable <bmc_length> value\n");
  StreamMgr_print_error(streams,  "  -l loopback\tChecks the property using <loopback> value instead of using the\n\t\tvariable <bmc_loopback> value\n");
  StreamMgr_print_error(streams,  "  -1 \t\tGenerates and solves a single problem.\n");
  StreamMgr_print_error(streams,  "  -o filename\tGenerates dimacs output file too. <filename> may contain patterns\n\n");

  return 1;
}

int Sbmc_CommandGenLtlSpecSBmc(NuSMVEnv_ptr env, int argc, char** argv)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  Prop_ptr ltlprop=(Prop_ptr)NULL;   /* The property being processed */
  Outcome opt_handling_res;
  int k = get_bmc_pb_length(opts);
  boolean is_single_prob = false;
  char *fname = (char *)NULL;
  int relative_loop =
    Bmc_Utils_ConvertLoopFromString(get_bmc_pb_loop(opts), NULL);

  /* ----------------------------------------------------------------------- */
  /* Options handling: */
  opt_handling_res = sbmc_cmd_options_handling(env, argc, argv, Prop_Ltl,
                                               &ltlprop,
                                               &k, &relative_loop,
                                               &fname,
                                               NULL, NULL /* -N -c */,
                                               &is_single_prob);

  if (opt_handling_res == OUTCOME_SUCCESS_REQUIRED_HELP) {
    return UsageSBMCGenLtlSpec(env);
  }
  if (opt_handling_res != OUTCOME_SUCCESS) {
    if (fname != (char *)NULL) FREE(fname);
    return 1;
  }

  /* makes sure bmc has been set up */
  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    if (fname != (char*) NULL) FREE(fname);
    return 1;
  }

  /* ----------------------------------------------------------------------- */

  if (fname == (char*) NULL) {
    fname = util_strsav(get_bmc_dimacs_filename(opts));
  }

  (void)sbmc_cmd_gen_solve_ltl_selected_or_all_props(env, ltlprop, k, relative_loop,
                                                 fname, is_single_prob, ! BMC_HAS_TO_SOLVE);

  FREE(fname); fname = (char*)NULL;

  return 0;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageSBMCGenLtlSpec(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "\nusage: gen_ltlspec_sbmc [-h | -n idx | -p \"formula\"] [-k max_length] [-l loopback]\n\t\t\t [-1] [-o <filename>]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage\n");
  StreamMgr_print_error(streams,  "  -n idx\tChecks the LTL property specified with <idx>\n");
  StreamMgr_print_error(streams,  "  -p \"formula\"\tChecks the specified LTL property\n");
  StreamMgr_print_error(streams,  "  -P \"name\"\tChecks the LTL property specified with <name>.\n");
  StreamMgr_print_error(streams,  "\t\tIf no property is specified, checks all LTL properties.\n");
  StreamMgr_print_error(streams,  "  -k max_length\tChecks the property using <max_length> value instead of using the\n\t\tvariable <bmc_length> value\n");
  StreamMgr_print_error(streams,  "  -l loopback\tChecks the property using <loopback> value instead of using the\n\t\tvariable <bmc_loopback> value\n");
  StreamMgr_print_error(streams,  "  -1 \t\tGenerates and solves a single problem.\n");
  StreamMgr_print_error(streams,  "  -o filename\tGenerates dimacs output file too. <filename> may contain patterns\n\n");

  return 1;
}

int Sbmc_CommandLTLCheckZigzagInc(NuSMVEnv_ptr env, int argc, char** argv)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  Prop_ptr ltlprop = PROP(NULL);   /* The property being processed */
  Outcome opt_handling_res;
  int k = get_bmc_pb_length(opts);
  boolean do_virtual_unrolling = true;
  boolean do_completeness_check = false;
  int res = 1;

  /* ----------------------------------------------------------------------- */
  /* Options handling: */
  opt_handling_res = sbmc_cmd_options_handling(env, argc, argv,
                                               Prop_Ltl, &ltlprop,
                                               &k,
                                               NULL, NULL, /* l, o */
                                               &do_virtual_unrolling,
                                               &do_completeness_check,
                                               NULL /* single problem */
                                               );

  if (opt_handling_res == OUTCOME_SUCCESS_REQUIRED_HELP) {
    return UsageSBMCIncCheck(env);
  }
  if (opt_handling_res != OUTCOME_SUCCESS)  return 1;

  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    return 1;
  }
  /* ----------------------------------------------------------------------- */

  res =
    sbmc_cmd_gen_solve_zigzag_inc_selected_or_all_props(env, ltlprop, k,
                                                        do_virtual_unrolling,
                                                        do_completeness_check);

  return res;
}

/*!
  \brief


*/
static int UsageSBMCIncCheck(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "\nusage: check_ltlspec_sbmc_inc [-h | -n idx | -p \"formula\"] [-k max_length] [-N] [-c]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -n idx\tChecks the LTL property specified with <idx>\n"
                        "        \t(using incremental algorithms).\n");
  StreamMgr_print_error(streams,  "  -p \"formula\"\tChecks the specified LTL property\n");
  StreamMgr_print_error(streams,  "  -P \"name\"\tChecks the LTL property specified with <name>.\n");
  StreamMgr_print_error(streams,  "\t\tIf no property is specified, checks all LTL properties (using \n"
                        "\t\tincremental algorithms).\n");
  StreamMgr_print_error(streams,  "  -k max_length\tChecks the property using <max_length> value instead of using \n\t\tthe variable <bmc_length> value.\n");
  StreamMgr_print_error(streams,  "  -N \t\tDoes not perform virtual unrolling.\n");
  StreamMgr_print_error(streams,  "  -c \t\tPerforms completeness check.\n");

  return 1;
}

/*!
  \command{check_pslspec_sbmc} Performs fair PSL model checking.

  \command_args{[-h] [-m | -o output-file] [-n number | -p "psl-expr [IN context]" | -P "name"]
  [-g] [-1] [-k bmc_length] [-l loopback]\}

  Performs fair PSL model checking using SBMC.<p>

  A <tt>psl-expr</tt> to be checked can be specified at command line
  using option <tt>-p</tt>. Alternatively, option <tt>-n</tt> can be used
  for checking a particular formula in the property database. If
  neither <tt>-n</tt> nor <tt>-p</tt> are used, all the PSLSPEC formulas in
  the database are checked.<p>

  Command options:<p>
  <dl>
    <dt> <tt>-m</tt>
       <dd> Pipes the output generated by the command in processing
           <tt>SPEC</tt>s to the program specified by the
           <tt>PAGER</tt> shell variable if defined, else
           through the <tt>UNIX</tt> command "more".
    <dt> <tt>-o output-file</tt>
       <dd> Writes the output generated by the command in processing
           <tt>PSLSPEC</tt>s to the file <tt>output-file</tt>.
    <dt> <tt>-p "psl-expr [IN context]"</tt>
       <dd> A PSL formula to be checked. <tt>context</tt> is the module
       instance name which the variables in <tt>ctl-expr</tt> must
       be evaluated in.
    <dt> <tt>-n number</tt>
       <dd> Checks the PSL property with index <tt>number</tt> in the property
            database.
    <dt> <tt>-P name</tt>
       <dd> Checks the PSL property named <tt>name</tt> in the property
            database.

    <dt> <tt>-g</tt> While solving a problem, dumps it as a DIMACS
    file whose name depends on the content of the system variable
    "bmc_dimacs_filename". This feature is not allowed when the option
    -i is used as well.

    <dt> <tt>-1</tt> Generates and solves a single problem instead of
    iterating through 0 and bmc_length.

    <dt> <tt>-k <i>bmc_length</i></tt>
       <dd> <i>bmc_length</i> is the maximum problem bound must be reached if
       the option -1 is not specified. If -1 is specified, <i>bmc_length</i> is
       the exact length of the problem to be generated.
       Only natural number are valid values for this option. If no value
       is given the environment variable <i>bmc_length</i> is considered
       instead.
    <dt> <tt>-l <i>loopback</i></tt>
       <dd> <i>loopback</i> value may be: <BR>
       - a natural number in (0, <i>bmc_length-1</i>). Positive sign ('+') can
       be also used as prefix of the number. Any invalid combination of length
       and loopback will be skipped during the generation/solving process.<BR>
       - a negative number in (-1, -<i>bmc_length</i>). In this case
       <i>loopback</i> is considered a value relative to <i>bmc_length</i>.
       Any invalid combination of length and loopback will be skipped
       during the generation/solving process.<BR>
       - the symbol 'X', which means "no loopback" <BR>
       - the symbol '*', which means "all possible loopback from zero to
       <i>bmc_length-1</i>"

  </dl><p>


*/
static int sbmc_CommandCheckPslSpecSbmc(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  PropDb_ptr const prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  FILE* errstream = StreamMgr_get_error_stream(streams);
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* old_outstream = outstream;
  int c;

  int k = -1;
  int l = 0;
  char* str_loop = (char*) NULL;
  boolean l_specified = false;
  boolean bmc_dump = false;
  boolean single_bmc_prob = false;

  int prop_no = -1;
  char* formula = NIL(char);
  char* formula_name = NIL(char);
  int status = 0;
  boolean useMore = false;
  char* dbgFileName = NIL(char);

  /* It is possible to extend sbmc_cmd_options_handling in order to handle the
     extra options of this function g m */
  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "h1gmo:n:p:P:k:l:")) != EOF) {
    switch (c) {
    case 'h': {
      status = BMC_USAGE; goto label_clean_and_exit;
    }
    case 'n':
      {
        if (formula != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (prop_no != -1) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula_name != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }

        prop_no = PropDb_get_prop_index_from_string(prop_db,
                                                    util_optarg);
        if (prop_no == -1) {
          status = 1; goto label_clean_and_exit;
        }

        break;
      }
    case 'P':
      {
        if (formula != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (prop_no != -1) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula_name != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }

        formula_name = util_strsav(util_optarg);

        prop_no = PropDb_prop_parse_name(prop_db,
                                         formula_name);

        if (prop_no == -1) {
          StreamMgr_print_error(streams,  "No property named \"%s\"\n", formula_name);

          status = 1; goto label_clean_and_exit;
        }

        break;
      }

    case 'g':
      bmc_dump = true;
      break;

    case '1':
      single_bmc_prob = true;
      break;

    case 'k':
      {
        char* str_k;

        /* check if a value has already been specified: */
        if (k != -1) {
          StreamMgr_print_error(streams,
                                "Option -k cannot be specified more than once.\n");
          status = 1; goto label_clean_and_exit;
        }

        str_k = util_strsav(util_optarg);

        if ((util_str2int(str_k, &k) != 0) || k<0) {
          ErrorMgr_error_invalid_number(errmgr, str_k);
          FREE(str_k);
          status = 1; goto label_clean_and_exit;
        }

        FREE(str_k);
        break;
      }

    case 'l':
      /* check if a value has already been specified: */
      if (l_specified) {
        StreamMgr_print_error(streams,
                              "Option -l cannot be specified more than once.\n");
        status = 1; goto label_clean_and_exit;
      }

      str_loop = util_strsav(util_optarg);
      l_specified = true;
      /* checking of loopback value is delayed after command line
         processing to allow any -k option evaluation before (see
         the cheking code below) */
      break;

    case 'p':
      {
        if (prop_no != -1) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula_name != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }

        formula = util_strsav(util_optarg);
        break;
      }
    case 'o':
      if (useMore) {
        status = BMC_USAGE; goto label_clean_and_exit;
      }
      dbgFileName = util_strsav(util_optarg);
      StreamMgr_print_output(streams,  "Output to file: %s\n", dbgFileName);
      break;

    case 'm':
      if (dbgFileName != NIL(char)) {
        status = BMC_USAGE; goto label_clean_and_exit;
      }
      useMore = true;
      break;
    default:
      status = BMC_USAGE; goto label_clean_and_exit;
    }
  }
  if (argc != util_optind) {
    status = BMC_USAGE; goto label_clean_and_exit;
  }

  /* ---------------------------------------------------------------------- */
  if (k == -1) k = get_bmc_pb_length(opts); /* default for k */

  if (OUTCOME_SUCCESS != Bmc_Cmd_compute_rel_loop(env, &l, str_loop, k)) {
    status = 1; goto label_clean_and_exit;
  }

  /* Checking compilation status */
  if (Compile_check_if_encoding_was_built(env, errstream)) {
    status = 1; goto label_clean_and_exit;
  }

  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    status = 1; goto label_clean_and_exit;
  }

  if (formula != NIL(char)) {
    SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
    prop_no = PropDb_prop_parse_and_add(prop_db, st,
                                        formula, Prop_Psl, Nil);
    if (prop_no == -1) { status = 1; goto label_clean_and_exit; }
  }

  if (OUTCOME_SUCCESS != Cmd_Misc_open_pipe_or_file(env, dbgFileName, &outstream)) {
    status = 1; goto label_clean_and_exit;
  }



  if (0 == status) {
    status =
      sbmc_cmd_gen_solve_psl_selected_or_all_props(env, prop_no, bmc_dump,
                                                   ! IS_INC_SAT,
                                                   single_bmc_prob,
                                                   k, l,
                                                   ! HAS_TO_CHECK_COMPL,
                                                   HAS_TO_UNROLL);
  }

 label_clean_and_exit:
  if (useMore) {
    FILE* reset_stream;

    CmdClosePipe(outstream);
    reset_stream = StreamMgr_reset_output_stream(streams);
    StreamMgr_set_output_stream(streams, old_outstream);

    nusmv_assert(reset_stream == outstream);

    outstream = (FILE*)NULL;
  }

  if ((char*)NULL != dbgFileName) {
    /* this closes the file stream as well  */
    StreamMgr_set_output_stream(streams, old_outstream);

    outstream = (FILE*)NULL;
  }

  FREE(str_loop); str_loop = (char*)NULL;
  FREE(formula_name); formula_name = (char*)NULL;

  if (BMC_USAGE == status) return UsageCheckPslSpecSbmc(env);
  else return status;
}

/*!
  \command{check_pslspec_sbmc_inc} Performs fair PSL model checking.

  \command_args{[-h] [-m | -o output-file] [-n number | -p "psl-expr [IN context]" | -P "name"]
  [-1] [-k bmc_length] [-l loopback]\}

  Performs fair PSL model checking using SBMC.<p>

  A <tt>psl-expr</tt> to be checked can be specified at command line
  using option <tt>-p</tt>. Alternatively, option <tt>-n</tt> can be used
  for checking a particular formula in the property database. If
  neither <tt>-n</tt> nor <tt>-p</tt> are used, all the PSLSPEC formulas in
  the database are checked.<p>

  Command options:<p>
  <dl>
    <dt> <tt>-m</tt>
       <dd> Pipes the output generated by the command in processing
           <tt>SPEC</tt>s to the program specified by the
           <tt>PAGER</tt> shell variable if defined, else
           through the <tt>UNIX</tt> command "more".
    <dt> <tt>-o output-file</tt>
       <dd> Writes the output generated by the command in processing
           <tt>PSLSPEC</tt>s to the file <tt>output-file</tt>.
    <dt> <tt>-p "psl-expr [IN context]"</tt>
       <dd> A PSL formula to be checked. <tt>context</tt> is the module
       instance name which the variables in <tt>ctl-expr</tt> must
       be evaluated in.
    <dt> <tt>-n number</tt>
       <dd> Checks the PSL property with index <tt>number</tt> in the property
            database.
    <dt> <tt>-P name</tt>
       <dd> Checks the PSL property named <tt>name</tt> in the property
            database.

    <dt> <tt>-1</tt> Generates and solves a single problem instead of
    iterating through 0 and bmc_length.

    <dt> <tt>-k <i>bmc_length</i></tt>
       <dd> <i>bmc_length</i> is the maximum problem bound must be reached if
       the option -1 is not specified. If -1 is specified, <i>bmc_length</i> is
       the exact length of the problem to be generated.
       Only natural number are valid values for this option. If no value
       is given the environment variable <i>bmc_length</i> is considered
       instead.
    <dt> <tt>-l <i>loopback</i></tt>
       <dd> <i>loopback</i> value may be: <BR>
       - a natural number in (0, <i>bmc_length-1</i>). Positive sign ('+') can
       be also used as prefix of the number. Any invalid combination of length
       and loopback will be skipped during the generation/solving process.<BR>
       - a negative number in (-1, -<i>bmc_length</i>). In this case
       <i>loopback</i> is considered a value relative to <i>bmc_length</i>.
       Any invalid combination of length and loopback will be skipped
       during the generation/solving process.<BR>
       - the symbol 'X', which means "no loopback" <BR>
       - the symbol '*', which means "all possible loopback from zero to
       <i>bmc_length-1</i>"

  </dl><p>


*/

#if NUSMV_HAVE_INCREMENTAL_SAT

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int sbmc_CommandCheckPslSpecSbmcInc(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  PropDb_ptr const prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  FILE* errstream = StreamMgr_get_error_stream(streams);
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* old_outstream = outstream;
  int c;

  int k = -1;
  int l = 0;
  char* str_loop = (char*) NULL;
  boolean l_specified = false;
  boolean single_bmc_prob = false;
  boolean does_virtual_unrol = true;
  boolean check_compl = false;
  boolean useMore = false;

  int prop_no = -1;
  char* formula = NIL(char);
  char* formula_name = NIL(char);
  int status = 0;
  char* dbgFileName = NIL(char);

  /* It is possible to extend sbmc_cmd_options_handling in order to handle the
     extra options of this function: g m */
  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hicN1gmo:n:p:P:k:l:")) != EOF) {
    switch (c) {
    case 'h': {
      status = BMC_USAGE; goto label_clean_and_exit;
    }
    case 'n':
      {
        if (formula != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (prop_no != -1) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula_name != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }

        prop_no = PropDb_get_prop_index_from_string(prop_db,
                                                    util_optarg);
        if (prop_no == -1) {
          status = 1; goto label_clean_and_exit;
        }

        break;
      }
    case 'P':
      {
        if (formula != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (prop_no != -1) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula_name != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }

        formula_name = util_strsav(util_optarg);

        prop_no = PropDb_prop_parse_name(prop_db,
                                         formula_name);

        if (prop_no == -1) {
          StreamMgr_print_error(streams,  "No property named \"%s\"\n", formula_name);

          status = 1; goto label_clean_and_exit;
        }

        break;
      }

    case 'N':
      does_virtual_unrol = false;
      break;

    case 'c':
      check_compl = true;
      break;

    case '1':
      single_bmc_prob = true;
      break;

    case 'k':
      {
        char* str_k;

        /* check if a value has already been specified: */
        if (k != -1) {
          StreamMgr_print_error(streams,
                                "Option -k cannot be specified more than once.\n");
          status = 1; goto label_clean_and_exit;
        }

        str_k = util_strsav(util_optarg);

        if ((util_str2int(str_k, &k) != 0) || k<0) {
          ErrorMgr_error_invalid_number(errmgr, str_k);
          FREE(str_k);
          status = 1; goto label_clean_and_exit;
        }

        FREE(str_k);
        break;
      }

    case 'l':
      /* check if a value has already been specified: */
      if (l_specified) {
        StreamMgr_print_error(streams,
                              "Option -l cannot be specified more than once.\n");
        status = 1; goto label_clean_and_exit;
      }

      str_loop = util_strsav(util_optarg);
      l_specified = true;
      /* checking of loopback value is delayed after command line
         processing to allow any -k option evaluation before (see
         the cheking code below) */
      break;

    case 'p':
      {
        if (prop_no != -1) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }
        if (formula_name != NIL(char)) {
          status = BMC_USAGE; goto label_clean_and_exit;
        }

        formula = util_strsav(util_optarg);
        break;
      }
    case 'o':
      if (useMore) {
        status = BMC_USAGE; goto label_clean_and_exit;
      }
      dbgFileName = util_strsav(util_optarg);
      StreamMgr_print_output(streams,  "Output to file: %s\n", dbgFileName);
      break;

    case 'm':
      if (dbgFileName != NIL(char)) {
        status = BMC_USAGE; goto label_clean_and_exit;
      }
      useMore = true;
      break;
    default:
      status = BMC_USAGE; goto label_clean_and_exit;
    }
  }
  if (argc != util_optind) {
    status = BMC_USAGE; goto label_clean_and_exit;
  }

  /* ---------------------------------------------------------------------- */
  if (k == -1) k = get_bmc_pb_length(opts); /* default for k */

  if (OUTCOME_SUCCESS != Bmc_Cmd_compute_rel_loop(env, &l, str_loop, k)) {
    status = 1; goto label_clean_and_exit;
  }

  /* Checking compilation status */
  if (Compile_check_if_encoding_was_built(env, errstream)) {
    status = 1; goto label_clean_and_exit;
  }

  if (Bmc_check_if_model_was_built(env, errstream, false)) {
    status = 1; goto label_clean_and_exit;
  }

  if (formula != NIL(char)) {
    SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
    prop_no = PropDb_prop_parse_and_add(prop_db, st,
                                        formula, Prop_Psl, Nil);
    if (prop_no == -1) { status = 1; goto label_clean_and_exit; }
  }

  if (OUTCOME_SUCCESS != Cmd_Misc_open_pipe_or_file(env, dbgFileName, &outstream)) {
    status = 1; goto label_clean_and_exit;
  }

  if (0 == status) {
    status =
      sbmc_cmd_gen_solve_psl_selected_or_all_props(env, prop_no, ! IS_BMC_DUMP,
                                                   IS_INC_SAT,
                                                   single_bmc_prob,
                                                   k, l,
                                                   check_compl,
                                                   does_virtual_unrol);
  }

 label_clean_and_exit:
  if (useMore) {
    FILE* reset_stream;

    CmdClosePipe(outstream);
    reset_stream = StreamMgr_reset_output_stream(streams);
    StreamMgr_set_output_stream(streams, old_outstream);

    nusmv_assert(reset_stream == outstream);

    outstream = (FILE*)NULL;
  }

  if ((char*)NULL != dbgFileName) {
    /* this closes the file stream as well  */
    StreamMgr_set_output_stream(streams, old_outstream);

    outstream = (FILE*)NULL;
  }

  FREE(str_loop); str_loop = (char*)NULL;
  FREE(formula_name); formula_name = (char*)NULL;

  if (BMC_USAGE == status) return UsageCheckPslSpecSbmcInc(env);
  else return status;
}
#endif

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Sbmc commands options handling for commands (optionally)
  acceping options -k -l -o -p -n -N -c

   Output variables called res_* are pointers to
  variables that will be changed if the user specified a value for the
  corresponding option. For example if the user specified "-k 2", then
  *res_k will be assigned to 2. The caller can selectively choose which
  options can be specified by the user, by passing either a valid pointer
  as output parameter, or NULL to disable the corresponding option.
  For example by passing NULL as actual parameter of res_l, option -l will
  be not accepted.

  If both specified, k and l will be checked for mutual consistency.
  Loop will contain a relative value, like the one the user specified.

  prop_type is the expected property type, if specified.

  All integers values will not be changed if the corresponding options
  had not be specified by the user, so the caller might assign them to
  default values before calling this function.

  All strings will be allocated by the function if the corresponding
  options had been used by the user. In this case it is responsability
  of the caller to free them. Strings will be assigned to NULL if the
  user had not specified any corresponding option.

  Returns OUTCOME_GENERIC_ERROR if an error has occurred;
  Returns OUTCOME_SUCCESS_REQUIRED_HELP if -h options had been specified;
  Returns OUTCOME_SUCCESS in all other cases.


  \se Result parameters might change
*/
static Outcome
sbmc_cmd_options_handling(NuSMVEnv_ptr env,
                          int argc, char** argv,
                          Prop_Type prop_type,

                          /* output parameters: */
                          Prop_ptr* res_prop,
                          int* res_k,
                          int* res_l,
                          char** res_o,
                          boolean* res_N,
                          boolean* res_c,
                          boolean* res_1)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  int c;
  int prop_idx;
  char* formula_name = (char*) NULL;
  char* str_formula = (char*) NULL;
  char* str_loop = (char*) NULL;

  boolean k_specified = false;
  boolean l_specified = false;

  /* If one or more options are added here, the size of this array
     must be changed. At the moment eight options are supported.  */
  char opt_string[9*2+1];


  /* ---------------------------------------------------------------------- */
  /* Fills up the string to pass to util_getopt, depending on which
     options are actually required */
  strcpy(opt_string, "h");  /* h is always needed */

  if (res_prop != (Prop_ptr*) NULL) {
    *res_prop = (Prop_ptr) NULL;
    strcat(opt_string, "n:p:P:");
  }

  if (res_k != (int*) NULL) strcat(opt_string, "k:");
  if (res_l != (int*) NULL) strcat(opt_string, "l:");

  if (res_o != (char**) NULL) {
    *res_o = (char*) NULL;
    strcat(opt_string, "o:");
  }

  if (res_N != (boolean*) NULL) strcat(opt_string, "N");
  if (res_c != (boolean*) NULL) strcat(opt_string, "c");
  if (res_1 != (boolean*) NULL) strcat(opt_string, "1");


  util_getopt_reset();
  while ((c = util_getopt((int)argc, (char**) argv, opt_string)) != EOF) {
    switch (c) {
    case 'h':
      return OUTCOME_SUCCESS_REQUIRED_HELP;

    case 'n': {
      char* str_prop_idx = (char*) NULL;

      nusmv_assert(res_prop != (Prop_ptr*) NULL);

      /* check if a formula has already been specified: */
      if ((*res_prop != PROP(NULL)) || (str_formula != (char*) NULL)) {
        ErrorMgr_error_property_already_specified(errmgr);
        return OUTCOME_GENERIC_ERROR;
      }

      str_prop_idx = util_strsav(util_optarg);

      /* check if property idx is ok */
      prop_idx = PropDb_get_prop_index_from_string(PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB)),
                                                   str_prop_idx);
      FREE(str_prop_idx);

      if (prop_idx == -1) {
        /* error messages have already been shown */
        return OUTCOME_GENERIC_ERROR;
      }

      /* here property idx is ok */
      *res_prop = PropDb_get_prop_at_index(PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB)),
                                           prop_idx);
      if ( Prop_check_type(*res_prop, prop_type) != 0 ) {
        /* specified property's type is not what the caller expected */
        return OUTCOME_GENERIC_ERROR;
      }

      break;
    } /* case 'n' */

    case 'P':
      {
        nusmv_assert(res_prop != (Prop_ptr*) NULL);

        /* check if a formula has already been specified: */
        if ((*res_prop != PROP(NULL)) || (str_formula != (char*) NULL) || (formula_name != (char*) NULL)) {
          ErrorMgr_error_property_already_specified(errmgr);
          return OUTCOME_GENERIC_ERROR;
        }

        formula_name = util_strsav(util_optarg);

        prop_idx = PropDb_prop_parse_name(PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB)),
                                          formula_name);

        if (prop_idx == -1) {
          StreamMgr_print_error(streams,  "No property named \"%s\"\n", formula_name);
          FREE(formula_name);
          /* error messages have already been shown */
          return OUTCOME_GENERIC_ERROR;
        }

        FREE(formula_name);

        /* here property idx is ok */
        *res_prop = PropDb_get_prop_at_index(PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB)),
                                             prop_idx);
        if ( Prop_check_type(*res_prop, prop_type) != 0 ) {
          /* specified property's type is not what the caller expected */
          return OUTCOME_GENERIC_ERROR;
        }

        break;
      } /* case 'P' */

    case 'p':
      nusmv_assert(res_prop != (Prop_ptr*) NULL);

      /* check if a formula has already been specified: */
      if ((*res_prop != PROP(NULL)) || (str_formula != (char*) NULL)) {
        ErrorMgr_error_property_already_specified(errmgr);
        return OUTCOME_GENERIC_ERROR;
      }

      str_formula = util_strsav(util_optarg);
      break;

    case 'k': {
      char* str_k;
      int k;

      nusmv_assert(res_k != (int*) NULL);

      /* check if a value has already been specified: */
      if (k_specified) {
        StreamMgr_print_error(streams,
                "Option -k cannot be specified more than once.\n");
        return OUTCOME_GENERIC_ERROR;
      }

      str_k = util_strsav(util_optarg);

      if (util_str2int(str_k, &k) != 0) {
        ErrorMgr_error_invalid_number(errmgr, str_k);
        FREE(str_k);
        return OUTCOME_GENERIC_ERROR;
      }

      if (k < 0) {
        ErrorMgr_error_invalid_number(errmgr, str_k);
        FREE(str_k);
        return OUTCOME_GENERIC_ERROR;
      }

      FREE(str_k);
      *res_k = k;
      k_specified = true;
      break;
    }

    case 'l':
      nusmv_assert(res_l != (int*) NULL);

      /* check if a value has already been specified: */
      if (l_specified) {
        StreamMgr_print_error(streams,
                "Option -l cannot be specified more than once.\n");
        return OUTCOME_GENERIC_ERROR;
      }

      str_loop = util_strsav(util_optarg);
      l_specified = true;
      /* checking of loopback value is delayed after command line
         processing to allow any -k option evaluation before (see the
         cheking code below) */
      break;

    case 'o':
      nusmv_assert(res_o != (char**) NULL);

      *res_o = util_strsav(util_optarg);
      break;

    case 'N':
      nusmv_assert(res_N != (boolean*) NULL);
      *res_N = false;
      break;

    case '1':
      nusmv_assert(res_1 != (boolean*) NULL);
      *res_1 = true;
      break;

    case 'c':
      nusmv_assert(res_c != (boolean*) NULL);
      *res_c = true;
      break;

    default:  return OUTCOME_GENERIC_ERROR;
    } /* switch case */
  } /* end of cmd line processing */

  /* checks if there are unexpected options: */
  if (argc != util_optind) {
    StreamMgr_print_error(streams,  "You specified one or more invalid options.\n\n");
    return OUTCOME_GENERIC_ERROR;
  }

  /* Checking of k,l constrains: */
  if (str_loop != (char*) NULL) {
    Outcome res;
    int rel_loop;

    rel_loop = Bmc_Utils_ConvertLoopFromString(str_loop, &res);

    if (res != OUTCOME_SUCCESS) {
      ErrorMgr_error_invalid_number(errmgr, str_loop);
      FREE(str_loop);
      return OUTCOME_GENERIC_ERROR;
    }
    FREE(str_loop);

    if (Bmc_Utils_Check_k_l(*res_k,
                            Bmc_Utils_RelLoop2AbsLoop(rel_loop, *res_k))
        != OUTCOME_SUCCESS) {
      ErrorMgr_error_bmc_invalid_k_l(errmgr, *res_k, rel_loop);
      return OUTCOME_GENERIC_ERROR;
    }

    *res_l = rel_loop;
  } /* k,l consistency check */


  /* Formula checking and commitment: */
  if (str_formula != (char*) NULL) {
    int idx;
    SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));

    /* make sure bmc has been set up */
    if (Bmc_check_if_model_was_built(env, errstream, false)) {
      FREE(str_formula);
      return OUTCOME_GENERIC_ERROR;
    }

    idx = PropDb_prop_parse_and_add(PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB)), st,
                                    str_formula, prop_type, Nil);
    if (idx == -1) {
      FREE(str_formula);
      return OUTCOME_GENERIC_ERROR;
    }

    /* index is ok */
    nusmv_assert(*res_prop == PROP(NULL));
    *res_prop = PropDb_get_prop_at_index(PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB)), idx);

    FREE(str_formula);
  } /* formula checking and commit */

  return OUTCOME_SUCCESS;
}

/*!
  \brief Call Bmc_SBMCGenSolveLtl on a selected property or over
  all the properties

  Call Bmc_SBMCGenSolveLtl on a selected property or over
  all the properties
*/

static inline
int sbmc_cmd_gen_solve_ltl_selected_or_all_props(const NuSMVEnv_ptr env,
                                                 const Prop_ptr ltlprop,
                                                 const int k,
                                                 const int relative_loop,
                                                 const char* fname,
                                                 const boolean is_single_prob,
                                                 const boolean has_to_solve)
{
  const Bmc_DumpType dump_type =
    (fname != (char*)NULL) ? BMC_DUMP_DIMACS : BMC_DUMP_NONE;

  if (ltlprop == PROP(NULL)) {
    const PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
    lsList props;
    lsGen  iterator;
    Prop_ptr prop;

    props = PropDb_prepare_prop_list(prop_db, Prop_Ltl);

    lsForEachItem(props, iterator, prop) {
      if  (Bmc_SBMCGenSolveLtl(env, prop, k, relative_loop,
                               ! is_single_prob,
                               has_to_solve,
                               dump_type,
                               fname) != 0) {
        return 1;
      }
    }

    lsDestroy(props, NULL);
  }
  else {
    if (Bmc_SBMCGenSolveLtl(env, ltlprop, k, relative_loop,
                               ! is_single_prob,
                               has_to_solve,
                               dump_type,
                               fname) != 0) {
      return 1;
    }
  }

  return 0;
}

/*!
  \brief Call Sbmc_zigzag_incr on a selected property or over all
  the properties

  Call Sbmc_zigzag_incr on a selected property or over all
  the properties
*/
static inline int
sbmc_cmd_gen_solve_zigzag_inc_selected_or_all_props(const NuSMVEnv_ptr env,
                                                    const Prop_ptr ltlprop,
                                                    const int k,
                                                    const boolean do_virtual_unrolling,
                                                    const boolean do_completeness_check)
{
  if (ltlprop == PROP(NULL)) {
    PropDb_ptr const prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
    lsGen  iterator;
    Prop_ptr prop;
    lsList const props = PropDb_prepare_prop_list(prop_db, Prop_Ltl);

    lsForEachItem(props, iterator, prop) {
      if (Sbmc_zigzag_incr(env, prop, k, do_virtual_unrolling,
                           do_completeness_check) != 0) {
        return 1;
      }
    }
    lsDestroy(props, NULL);
  }
  else {
    if (Sbmc_zigzag_incr(env, ltlprop, k, do_virtual_unrolling,
                         do_completeness_check) != 0)
      return 1;
  }

  return 0;
}

/*!
  \brief Call the sbmc check function over the selected psl
  property or over all the psl properties.

  Call the sbmc check function over the selected psl
  property or over all the psl properties. The PSL properties not convertible
  to LTL will be skipped.
*/
static inline int
sbmc_cmd_gen_solve_psl_selected_or_all_props(NuSMVEnv_ptr const env,
                                             const int prop_no,
                                             const boolean bmc_dump,
                                             const boolean inc_sat,
                                             const boolean single_bmc_prob,
                                             const int k,
                                             const int l,
                                             const boolean check_compl,
                                             const boolean does_virtual_unrol)
{
  const PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  int status = 0;

  if (prop_no != -1) {
    if (Prop_check_type(PropDb_get_prop_at_index(prop_db,
                                                 prop_no),
                        Prop_Psl) != 0) {
      status = 1;
    }
    else {
      status =
        Sbmc_Gen_check_psl_property(env,
                                    PropDb_get_prop_at_index(prop_db, prop_no),
                                    bmc_dump, inc_sat, check_compl,
                                    does_virtual_unrol, single_bmc_prob, k, l);

    }
  }
  else {
    lsList props;
    lsGen  iterator;
    Prop_ptr prop;

    props = PropDb_prepare_prop_list(prop_db, Prop_Psl);

    lsForEachItem(props, iterator, prop) {
      if (Prop_is_psl_ltl(prop)) {
        status =
          Sbmc_Gen_check_psl_property(env, prop, bmc_dump, inc_sat, check_compl,
                                      does_virtual_unrol, single_bmc_prob, k,
                                      l);
      }

      if (0 != status) break;
    }

    lsDestroy(props, NULL);
  }

  return status;
}

/*!
  \brief Usage function

  the output of -h option
*/
static int UsageCheckPslSpecSbmc(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  StreamMgr_print_error(streams, "usage: check_pslspec_sbmc [-h] [-m | -o file] [-n number | -p \"psl-expr\" | -P \"name\"]\n");
  StreamMgr_print_error(streams, "                          [-g] [-1] \n");
  StreamMgr_print_error(streams, "                          [-k bmc_length] [-l loopback]\n");
  StreamMgr_print_error(streams, "   -h \t\t\tPrints the command usage.\n");
  StreamMgr_print_error(streams, "   -m \t\t\tPipes output through the program specified\n");
  StreamMgr_print_error(streams, "      \t\t\tby the \"PAGER\" environment variable if defined,\n");
  StreamMgr_print_error(streams, "      \t\t\telse through the UNIX command \"more\".\n");
  StreamMgr_print_error(streams, "   -o file\t\tWrites the generated output to \"file\".\n");
  StreamMgr_print_error(streams, "   -n number\t\tChecks only the PSLSPEC with the given index number.\n");
  StreamMgr_print_error(streams, "   -p \"psl-expr\"\tChecks only the given PSL formula.\n");
  StreamMgr_print_error(streams, "   -P \"name\"\t\tChecks only the PSLSPEC with the given name.\n");
  StreamMgr_print_error(streams, "   -g \t\t\tDumps generated problems in DIMACS format.\n");
  StreamMgr_print_error(streams, "   -1 \t\t\tGenerates and solves single problems.\n");
  StreamMgr_print_error(streams, "   -k bmc_length\tChecks the property using <bmc_length> value instead \n\t\t\tof using the variable <bmc_length> value.\n");
  StreamMgr_print_error(streams, "   -l loopback\t\tChecks the property using <loopback> value\n\t\t\tinstead of using the variable <bmc_loopback> value.\n");

  return 1;
}

/*!
  \brief Usage function

  the output of -h option
*/
static int UsageCheckPslSpecSbmcInc(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  StreamMgr_print_error(streams, "usage: check_pslspec_sbmc_inc [-h] [-m | -o file] [-n number | -p \"psl-expr\" | -P \"name\"]\n");
  StreamMgr_print_error(streams, "                              [-1] [-c] [-N]\n");
  StreamMgr_print_error(streams, "                              [-k bmc_length] [-l loopback]\n");
  StreamMgr_print_error(streams, "   -h \t\t\tPrints the command usage.\n");
  StreamMgr_print_error(streams, "   -m \t\t\tPipes output through the program specified\n");
  StreamMgr_print_error(streams, "      \t\t\tby the \"PAGER\" environment variable if defined,\n");
  StreamMgr_print_error(streams, "      \t\t\telse through the UNIX command \"more\".\n");
  StreamMgr_print_error(streams, "   -o file\t\tWrites the generated output to \"file\".\n");
  StreamMgr_print_error(streams, "   -n number\t\tChecks only the PSLSPEC with the given index number.\n");
  StreamMgr_print_error(streams, "   -p \"psl-expr\"\tChecks only the given PSL formula.\n");
  StreamMgr_print_error(streams, "   -P \"name\"\t\tChecks only the PSLSPEC with the given name.\n");
  StreamMgr_print_error(streams, "   -1 \t\t\tGenerates and solves single problems.\n");
  StreamMgr_print_error(streams, "   -k bmc_length\tChecks the property using <bmc_length> value instead \n\t\t\tof using the variable <bmc_length> value.\n");
  StreamMgr_print_error(streams, "   -l loopback\t\tChecks the property using <loopback> value\n\t\t\tinstead of using the variable <bmc_loopback> value.\n");
  StreamMgr_print_error(streams, "   -c \t\t\tPerforms completeness check (implies -i)\n");
  StreamMgr_print_error(streams, "   -N \t\t\tDoes not perform virtual unrolling (implies -i).\n\n");

  return 1;
}
