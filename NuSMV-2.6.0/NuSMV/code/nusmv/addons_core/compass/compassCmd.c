/* ---------------------------------------------------------------------------


  This file is part of the ``compass'' package of NuSMV version 2.
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
  \author Roberto Cavada
  \brief \todo: Missing synopsis

  \todo: Missing description

*/

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/shell/cmd/cmd.h"

#include "nusmv/addons_core/compass/compassCmd.h"
#include "nusmv/addons_core/compass/compass.h"
#include "nusmv/core/compile/compile.h"

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/NodeList.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/parser.h"
#include "nusmv/addons_core/compass/parser/prob/ParserProb.h"
#include "nusmv/addons_core/compass/parser/ap/ParserAp.h"

#include "nusmv/addons_core/compass/compile/ProbAssign.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/prop/propPkg.h"
#include "nusmv/core/utils/ErrorMgr.h"

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static int UsageCompassGenSigref(const NuSMVEnv_ptr env);
static int CommandCompassGenSigref(NuSMVEnv_ptr env, int argc, char** argv);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Compass_init_cmd(NuSMVEnv_ptr env)
{
  Cmd_CommandAdd(env, "compass_gen_sigref", CommandCompassGenSigref, 0, true);
}

void Compass_Cmd_quit(NuSMVEnv_ptr env)
{
  Cmd_CommandRemove(env, "compass_gen_sigref");
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/
/*!
\command{compass_gen_sigref} generes sigref format.

  \command_args{[-h] [-b] [-i &lt;prob-fname&gt;] [-t &quot;&lt;tau&gt;&quot;] [-o &lt;sigref-fname&gt;]}

  Command options:<p>
  <dl>
    <dt> <tt>-h</tt>
       <dd> Prints the command usage.
    <dt> <tt>-i &lt;fname&gt;</tt>
       <dd> Read probabilistic info from fname.
    <dt> <tt>-t &quot;&lt;tau&gt;&quot;</tt>
       <dd> Read tau location from given assignment expression.
    <dt> <tt>-a &lt;fname&gt;</tt>
       <dd> Read atomic proposition list from file fname.
    <dt> <tt>-o &lt;fname&gt;</tt>
       <dd> Write result into file fname.
    <dt> <tt>-b</tt>
       <dd>  Beautify the XML output (default disabled).
  </dl>
*/

static int CommandCompassGenSigref(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* errstream = StreamMgr_get_error_stream(streams);
  ErrorMgr_ptr const errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  BddFsm_ptr bdd_fsm = NULL;
  int c = 0;
  const char* prob_fname = (char*) NULL;
  const char* sigref_fname = (char*) NULL;
  const char* tau_expr_str = (char*) NULL;
  const char* ap_fname = (char*) NULL;
  boolean do_indent = false;
  int retval = 0;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hbi:t:o:a:")) != EOF) {
    switch (c) {
    case 'b':
      do_indent = true;
      break;
    case 'h': goto __compass_gen_sigref_fail_help;
    case 'i':
      if (prob_fname != (char*) NULL) { FREE(prob_fname); }
      prob_fname = util_strsav(util_optarg);
      break;

    case 't':
      if (tau_expr_str != (char*) NULL) { FREE(tau_expr_str); }
      tau_expr_str = util_strsav(util_optarg);
      break;

    case 'o':
      if (sigref_fname != (char*) NULL) FREE(sigref_fname);
      sigref_fname = util_strsav(util_optarg);
      break;

    case 'a':
      if (ap_fname != (char*) NULL) FREE(ap_fname);
      ap_fname = util_strsav(util_optarg);
      break;

    default: goto __compass_gen_sigref_fail_help;
    }
  }

  if (argc != util_optind) goto __compass_gen_sigref_fail_help;

  /* preconditions */
  /* I think here we should use Compile_check_if_model_was_built instead of
     flat_model version, since bddfsm is assumed */
  if (Compile_check_if_flat_model_was_built(env, errstream, false) ||
      Compile_check_if_encoding_was_built(env, errstream)) {
    goto __compass_gen_sigref_fail;
  }

  {
    /* Input checking */
    Expr_ptr tau_expr = (Expr_ptr) NULL;
    FILE* prob_file = (FILE*) NULL;
    FILE* sigref_file = NULL;
    FILE* ap_file = NULL;

    if (tau_expr_str != (char*)NULL) {
      if (Parser_ReadSimpExprFromString(env, tau_expr_str, &tau_expr) != 0) {
        goto __compass_gen_sigref_fail;
      }
    }

    if (prob_fname != (char*) NULL) {
      prob_file = fopen(prob_fname, "r");
      if (prob_file == (FILE*) NULL) {
        StreamMgr_print_error(streams,  "Unable to open probability list file \"%s\".\n",
                              prob_fname);
        goto __compass_gen_sigref_fail;
      }
    }

    if (ap_fname != (char*) NULL) {
      ap_file = fopen(ap_fname, "r");
      if (ap_file == (FILE*) NULL) {
        StreamMgr_print_error(streams,  "Unable to atomic proposition list file \"%s\".\n",
                              prob_fname);
        goto __compass_gen_sigref_fail;
      }
    }

    /* Was file name specified for sigref output? */
    if (sigref_fname != (char*) NULL) {
      sigref_file = fopen(sigref_fname, "w");
      if (sigref_file == (FILE*) NULL) {
        StreamMgr_print_error(streams,  "Unable to open output sigref file \"%s\".\n",
                              sigref_fname);
        fclose(prob_file);
        goto __compass_gen_sigref_fail;
      }
    }
    else sigref_file = outstream;

    bdd_fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));
    if (NULL == bdd_fsm) goto __compass_gen_sigref_fail;

    /* end of input checkings */

    CATCH(errmgr) {
      retval = Compass_write_sigref(env, bdd_fsm, sigref_file, prob_file, ap_file,
                                    tau_expr, do_indent);
    }
    FAIL(errmgr) {
      retval = 1;
    }

    goto __compass_gen_sigref_exit;

  __compass_gen_sigref_fail_help:
    (void)UsageCompassGenSigref(env);

    FALLTHROUGH

  __compass_gen_sigref_fail:
    retval = 1;

  __compass_gen_sigref_exit:
    if (tau_expr_str != (char*) NULL) FREE(tau_expr_str);

    if (sigref_fname != (char*) NULL) {
      FREE(sigref_fname);

      if (NULL != sigref_file) {
        if (0 != fclose(sigref_file)) retval = 1;
      }
    }

    if (prob_fname != (char*) NULL) {
      FREE(prob_fname);

      if (NULL != prob_file) {
        if (0 != fclose(prob_file)) retval = 1;
      }
    }

    if (ap_fname  != (char*) NULL) {
      FREE(ap_fname);

      if (NULL != ap_file) {
        if (0 != fclose(ap_file)) retval = 1;
      }
    }
  }

  return retval;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageCompassGenSigref(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: compass_gen_sigref [-h] [-b] [-i <prob-fname>] [-t \"<tau>\"] [-o <sigref-fname>]\n");
  StreamMgr_print_error(streams,  "  -h \t\t Prints the command usage.\n");
  StreamMgr_print_error(streams,  "  -i <fname>\t Read probabilistic info from fname.\n");
  StreamMgr_print_error(streams,  "  -t \"<tau>\"\t Read tau location from given assignment expression.\n");
  StreamMgr_print_error(streams,  "  -a <fname>\t Read atomic proposition list from file fname.\n");
  StreamMgr_print_error(streams,  "  -o <fname>\t Write result into file fname.\n");
  StreamMgr_print_error(streams,  "  -b \t\t Beautify the XML output (default disabled).\n");

  return 1;
}
