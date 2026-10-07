/* ---------------------------------------------------------------------------


  This file is part of the ``enc'' package of NuSMV version 2.
  Copyright (C) 2009 by FBK-IRST.

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
  \author Andrei Tchaltsev
  \brief The shell interface of the ENC package

  Shell interface of the ENC package.

*/


#include "nusmv/shell/enc/encCmd.h"
#include "nusmv/shell/cmd/cmd.h"

#include "nusmv/core/compile/compile.h" /* for cmp_struct_get_encode_variables */
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/parser/parser.h"
#include "nusmv/core/prop/Prop.h"
#include "nusmv/core/prop/propPkg.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/error.h" /* internal error */

#include <math.h> /* for log10 */
#include <stdio.h> /* for fopen */
/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static int CommandCleanSexp2BDDCache(NuSMVEnv_ptr env, int argc, char** argv);
static int UsageCleanSexp2BDDCache(const NuSMVEnv_ptr env);

static int CommandPrintFormula(NuSMVEnv_ptr env, int argc, char** argv);
static int UsagePrintFormula(const NuSMVEnv_ptr env);

static int CommandDumpExpr(NuSMVEnv_ptr env, int argc, char** argv);
static int UsageDumpExpr(const NuSMVEnv_ptr env);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/
/*!
  \brief Adds commands related to Enc package



  SideEffects        []

  SeeAlso            [Enc_init_encodings]
*****************************************************************************

  \sa Enc_init_encodings
*/

/* WARNING [MD] quit function missing */
void Enc_add_commands(NuSMVEnv_ptr env)
{
  Cmd_CommandAdd(env, "clean_sexp2bdd_cache", CommandCleanSexp2BDDCache, 0, true);
  Cmd_CommandAdd(env, "print_formula", CommandPrintFormula, 0, true);
  Cmd_CommandAdd(env, "dump_expr", CommandDumpExpr, 0, true);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \command{clean_sexp2bdd_cache} Cleans the cache used during evaluation of
  expressions to ADD and BDD representations.

  \command_args{[-h] }

   During conversion of symbolic (node_ptr)
  expressions to ADD and BDD representations the results of
  evaluations are normally cached (see additionally NuSMV option
  enable_sexp2bdd_caching). This allows to save time by avoid the
  construction of BDD for the same expression several time.

  In some situations it  may be preferable to clean the cache and
  free collected ADD and BDD. This operation can be done, for example,
  to free some memory. Another possible reason is that dynamic
  reordering may modify all existing BDDs, and cleaning the cache
  thereby freeing the BDD may speed up the reordering.

  This command is designed specifically to free the internal cache of
  evaluated expressions and their ADDs and BDDs.

  Note that only the cache of exp-to-bdd evaluator is freed.  BDDs of
  variables, constants and expressions collected in BDD FSM or
  anywhere else are not touched.
*/
static int CommandCleanSexp2BDDCache(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  BddEnc_ptr enc;
  int c;
  extern cmp_struct_ptr cmps;

  /* Parse the options. */
  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "h")) != EOF) {
    switch (c) {
    case 'h': return(UsageCleanSexp2BDDCache(env));
    default:
      return(UsageCleanSexp2BDDCache(env));
    }
  }

  if (argc != util_optind) return(UsageCleanSexp2BDDCache(env));

  /* pre-conditions: */
  if (!cmp_struct_get_encode_variables(cmps)) {
    StreamMgr_print_error(streams,  "ERROR: BDD encoding has to be created before. "
            "Use \"encode_variables\" command.\n\n");
    return UsageCleanSexp2BDDCache(env);
  }

  enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));

  return Enc_clean_evaluation_cache(env, enc);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageCleanSexp2BDDCache(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: clean_sexp2bdd_cache [-h]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage\n"
          "The command cleans the cache and frees intermediate BDDs constructed\n"
          "during evaluation of symbolic expressions into ADD and BDD form\n");
  return 1;
}

/*!
  \command{print_formula} Prints a formula

  \command_args{[-h] | [-v] | [-f ] <expression>}

  In formula mode, the formula as the canonical
                      formula is printed.  In verbose mode, the
                      explicit assignments satisfying the formula are
                      printed. Prints the number of satsfying
                      assignments for the given formula.<p>

  Command Options:
  <dl>
  <dt> <tt>-v</tt>
  <dd> Verbosely prints the list of assignments satisfying the formula.
  <dt> <tt>-f</tt>
  <dd> Prints a canonical representation of input.
  </dl>
*/
static int CommandPrintFormula(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  int c;
  boolean verbose = false;
  boolean formula = false;
  char* str_constr = (char*) NULL;
  int parse_result;
  node_ptr constr = Nil;

  /* Parse the command line */
  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hvf")) != EOF) {
    switch (c) {
    case 'h': return UsagePrintFormula(env);
    case 'v': verbose = true; break;
    case 'f': formula = true; break;
    default:
      return UsagePrintFormula(env);
    }
  }

  if (util_optind < argc) {
    str_constr = util_strsav(argv[util_optind]);
  }

  if (verbose && formula) {
    UsagePrintFormula(env);
    goto CommandPrintFormula_fail;
  }

  if ((char *)NULL == str_constr || 0 == strlen(str_constr)) {
    StreamMgr_print_error(streams,  "No expression given.\n\n");
    UsagePrintFormula(env);
    goto CommandPrintFormula_fail;
  }

  if (Compile_check_if_model_was_built(env, errstream, true)) {
    goto CommandPrintFormula_fail;
  }

  { /* parsing given formula */
    const char* arv[2];
    const int arc = 2;

    arv[0] = (char*) NULL;
    arv[1] = (char*) str_constr;
    parse_result = Parser_ReadCmdFromString(env, arc, arv, "SIMPWFF ",
                                            ";", &constr);
  }

  if (parse_result != 0 || constr == Nil) {
    StreamMgr_print_error(streams,  "Parsing error: expected an expression.\n" );
    goto CommandPrintFormula_fail;
  }

  BddEnc_print_formula(env, constr, verbose, formula);

 CommandPrintFormula_fail:
  if ((char *)NULL != str_constr) FREE(str_constr);

  return 0;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsagePrintFormula(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: print_formula [-h] | [-v] | [-f] <expr>\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -v \t\tPrints explicit models of the formula.\n");
  StreamMgr_print_error(streams,
          "   -f \t\tPrints the simplified and canonical formula.\n");
  return 1;
}

/*!
  \command{dump_expr} Dumps a given expression (which can be
  contextualized) in the specified format. Example of supported format
  is 'dot'.

  \command_args{[-h] | -e <expression> -f <format> [-o <fname>]}

  Dumps an expression in the specified output format.

  Command Options:
  <dl>
  <dt> <tt>-e expression</tt>
  <dd> The required expression to be dumped.
  <dt> <tt>-f format</tt>
  <dd> The format to be used when dumping. Examples are dot, davinci.
  <dt> <tt>-o filename</tt>
  <dd> The name of the output file (default: standard output)
  </dl>
*/
static int CommandDumpExpr(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* errstream = StreamMgr_get_error_stream(streams);
  int c;
  int res = 0;
  node_ptr parsed_expr;
  char* str_constr = (char*) NULL;
  char* str_format = (char*) NULL;
  char* fname = (char*) NULL;
  FILE* outfile = (FILE*) NULL;
  FILE* const default_outfile = outstream;

  const struct {
    const char* name;
    t_format format;
  } supported_formats[] = {
    {"dot", DUMP_FORMAT_DOT},
    {"davinci", DUMP_FORMAT_DAVINCI},
  };

  t_format format = DUMP_FORMAT_INVALID;

  /* Parse the command line */
  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "he:o:f:")) != EOF) {
    switch (c) {
    case 'h':
      res = UsageDumpExpr(env);
      goto dump_expr_quit;

    case 'f':
      if ((char*) NULL != str_format) FREE(str_format);
      str_format = util_strsav(util_optarg);
      break;

    case 'e':
      if ((char*) NULL != str_constr) FREE(str_constr);
      str_constr = util_strsav(util_optarg);
      break;

    case 'o':
      if ((char*) NULL != fname) FREE(fname);
      fname = util_strsav(util_optarg);
      break;

    default:
      res = 1;
      goto dump_expr_quit;
    }
  }

  /* preconditions */
  if (Compile_check_if_encoding_was_built(env, errstream)) {
    res = 1;
    goto dump_expr_quit;
  }

  /* checks arguments */
  if ((char*) NULL == str_constr) {
    StreamMgr_print_error(streams,  "No expression given.\n");
    res = 1;
    goto dump_expr_quit;
  }

  if ((char*) NULL == str_format) {
    StreamMgr_print_error(streams,  "No format was specified.\n");
    res = 1;
    goto dump_expr_quit;
  }

  { /* checks format */
    unsigned int i;
    for (i=0; i<sizeof(supported_formats)/sizeof(supported_formats[0]); ++i) {
      if (Utils_strcasecmp(supported_formats[i].name, str_format) == 0) {
        format = supported_formats[i].format;
        break;
      }
    }
    if (DUMP_FORMAT_INVALID == format) {
      StreamMgr_print_error(streams,  "Invalid format. Valid formats are: ");
      for (i=0; i<sizeof(supported_formats)/sizeof(supported_formats[0]); ++i) {
        StreamMgr_print_error(streams,  "'%s' ", supported_formats[i].name);
      }
      StreamMgr_print_error(streams,  "\n");
      res = 1;
      goto dump_expr_quit;
    }
  }

  if (Parser_ReadNextExprFromString(env, str_constr, &parsed_expr)) {
    res = 1;
    goto dump_expr_quit;
  }

  if ((char*) NULL != fname) {
    outfile = fopen(fname, "w");
    if ((FILE*) NULL == outfile) {
      StreamMgr_print_error(streams,  "Problems opening output file '%s'.\n", fname);
      res = 1;
      goto dump_expr_quit;
    }
  }
  else {
    /* file name was not specified: use default output descriptor */
    outfile = default_outfile;
  }

  res = BddEnc_dump_expr(BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER)),
                         parsed_expr, str_constr, format, outfile);

 dump_expr_quit:
  if ((char*) NULL != str_constr) { FREE(str_constr); }
  if ((char*) NULL != str_format) { FREE(str_format); }
  if ((char*) NULL != fname) { FREE(fname); }
  if ((FILE*) NULL != outfile && outfile != default_outfile) {
    fclose(outfile);
  }
  return res;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageDumpExpr(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: dump_expr [-h] | -e <expr> -f <format> [-o fname]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -e <expr> \tExpression to be dumped.\n");
  StreamMgr_print_error(streams,  "   -f <format> \tThe format of dumping (e.g. 'dot').\n");
  StreamMgr_print_error(streams,  "   -o <fname> \tThe output file name.\n");
  return 1;
}
