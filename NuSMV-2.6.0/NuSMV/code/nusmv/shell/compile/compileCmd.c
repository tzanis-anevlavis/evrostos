/* ---------------------------------------------------------------------------


  This file is part of the ``compile'' package of NuSMV version 2.
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
  \author Marco Roveri
  \brief Shell interface for the compile package.

  This file contains the interface of the compile package
  with the interactive shell.

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/shell/cmd/cmd.h"
#include "nusmv/shell/compile/compileCmd.h"

#include "nusmv/core/compile/compile.h"

#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/defs.h"
#include "nusmv/core/utils/assoc.h"

#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/node/normalizers/MasterNormalizer.h"

#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/compile/symb_table/symb_table.h"
#include "nusmv/core/compile/PredicateNormaliser.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/parser.h"

#include "nusmv/core/fsm/FsmBuilder.h"
#include "nusmv/core/fsm/sexp/SexpFsm.h"
#include "nusmv/core/fsm/sexp/BoolSexpFsm.h"
#include "nusmv/core/fsm/bdd/BddFsm.h"

#include "nusmv/core/prop/propPkg.h"
#include "nusmv/core/mc/mc.h"
#include "nusmv/core/enc/enc.h"

#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/trace/exec/PartialTraceExecutor.h"
#include "nusmv/core/trace/exec/BDDPartialTraceExecutor.h"

#include "nusmv/core/trace/exec/CompleteTraceExecutor.h"
#include "nusmv/core/trace/exec/BDDCompleteTraceExecutor.h"

#include "nusmv/core/utils/ucmd.h"
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/utils/error.h" /* for CATCH(errmgr) */
#include "nusmv/core/utils/Olist.h"

#include <stdlib.h> /* for strtol */
#include "nusmv/core/utils/portability.h" /* for errno */
#include "nusmv/core/cinit/NuSMVEnv.h"
#include "nusmv/core/cinit/cinit.h"
#include "nusmv/core/node/anonymizers/NodeAnonymizerBase.h"
#include "nusmv/core/node/anonymizers/NodeAnonymizerST.h"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define RC_EXPERIMENTAL_CODE_PREDICATES 1

int CommandProcessModel(NuSMVEnv_ptr env, int argc, char** argv);
int CommandFlattenHierarchy(NuSMVEnv_ptr env, int argc, char** argv);
int CommandShowVars(NuSMVEnv_ptr env, int argc, char** argv);
int CommandEncodeVariables(NuSMVEnv_ptr env, int argc, char** argv);
int CommandBuildModel(NuSMVEnv_ptr env, int argc, char** argv);
int CommandBuildFlatModel(NuSMVEnv_ptr env, int argc, char** argv);
int CommandBuildBooleanModel(NuSMVEnv_ptr env, int argc, char** argv);
int CommandWriteOrder(NuSMVEnv_ptr env, int argc, char** argv);
int CommandIwls95PrintOption(NuSMVEnv_ptr env, int argc, char** argv);
int CommandCPPrintClusterInfo(NuSMVEnv_ptr env, int argc, char** argv);
int CommandPrintFsmStats(NuSMVEnv_ptr env, int argc, char** argv);
int CommandGo(NuSMVEnv_ptr env, int argc, char** argv);
int CommandGoBmc(NuSMVEnv_ptr env, int argc, char** argv);
int CommandGetInternalStatus(NuSMVEnv_ptr env, int argc, char** argv);
int CommandWriteModelFlat(NuSMVEnv_ptr env, int argc, char** argv);
int CommandWriteModelFlatUdg(NuSMVEnv_ptr env, int argc, char** argv);
int CommandWriteModelFlatBool(NuSMVEnv_ptr env, int argc, char** argv);
int CommandWriteCoiModel(NuSMVEnv_ptr env, int argc, char** argv);
int CommandShowDependencies(NuSMVEnv_ptr env, int argc, char** argv);

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
extern cmp_struct_ptr cmps;

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static int UsageProcessModel(const NuSMVEnv_ptr env);
static int UsageFlattenHierarchy(const NuSMVEnv_ptr env);
static int UsageShowVars(const NuSMVEnv_ptr env);
static int UsageEncodeVariables(const NuSMVEnv_ptr env);
static int UsageBuildModel(const NuSMVEnv_ptr env);
static int UsageBuildFlatModel(const NuSMVEnv_ptr env);
static int UsageBuildBooleanModel(const NuSMVEnv_ptr env);
static int UsageWriteOrder(const NuSMVEnv_ptr env);
static int UsageIwls95PrintOption(const NuSMVEnv_ptr env);
static int UsageGo(const NuSMVEnv_ptr env);
static int UsageGoBmc(const NuSMVEnv_ptr env);
static int UsageGetInternalStatus(const NuSMVEnv_ptr env);
static int UsageWriteModelFlat(const NuSMVEnv_ptr env);
static int UsageWriteModelFlatUdg(const NuSMVEnv_ptr env);
static int UsageWriteModelFlatBool(const NuSMVEnv_ptr env);
static int UsageWriteCoiModel(const NuSMVEnv_ptr env);
static int UsagePrintFsmStats(const NuSMVEnv_ptr env);
static int UsageShowDependencies(const NuSMVEnv_ptr env);

static inline void clean_memory_before_return(FILE* ofileid,
                                              boolean bSpecifiedFilename,
                                              char* output_file,
                                              FILE* outstream);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Compile_init_cmd(NuSMVEnv_ptr env)
{
  Cmd_CommandAdd(env, "process_model", CommandProcessModel, 0, false);
  Cmd_CommandAdd(env, "flatten_hierarchy", CommandFlattenHierarchy, 0, false);
  Cmd_CommandAdd(env, "show_vars", CommandShowVars, 0, true);
  Cmd_CommandAdd(env, "encode_variables", CommandEncodeVariables, 0, false);
  Cmd_CommandAdd(env, "build_model", CommandBuildModel, 0, false);
  Cmd_CommandAdd(env, "build_flat_model", CommandBuildFlatModel, 0, false);
  Cmd_CommandAdd(env, "build_boolean_model", CommandBuildBooleanModel, 0, false);
  Cmd_CommandAdd(env, "write_order", CommandWriteOrder, 0, true);
  Cmd_CommandAdd(env, "print_iwls95options", CommandIwls95PrintOption, 0, true);

  /* this is deprecated in 2.4 */
  Cmd_CommandAdd(env, "print_clusterinfo", CommandCPPrintClusterInfo, 0, true);

  Cmd_CommandAdd(env, "print_fsm_stats", CommandPrintFsmStats, 0, true);
  Cmd_CommandAdd(env, "go", CommandGo, 0, false);

#if NUSMV_HAVE_SAT_SOLVER
  Cmd_CommandAdd(env, "go_bmc", CommandGoBmc, 0, false);
#endif

  Cmd_CommandAdd(env, "get_internal_status", CommandGetInternalStatus, 0, true);
  Cmd_CommandAdd(env, "write_flat_model", CommandWriteModelFlat, 0, true);
  Cmd_CommandAdd(env, "write_flat_model_udg", CommandWriteModelFlatUdg, 0, true);
  Cmd_CommandAdd(env, "write_boolean_model", CommandWriteModelFlatBool, 0, true);

  Cmd_CommandAdd(env, "write_coi_model", CommandWriteCoiModel, 0, true);

  Cmd_CommandAdd(env, "show_dependencies", CommandShowDependencies, 0, true);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \command{process_model} Performs the batch steps and then returns control
  to the interactive shell.

  \command_args{[-h] [-f] [-r] [-i model-file] [-m Method]}

   Reads the model, compiles it into BDD and
  performs the model checking of all the specification contained in
  it. If the environment variable <tt>forward_search</tt> has been set
  before, then the set of reachable states is computed. If the
  option <tt>-r</tt> is specified, the reordering of variables is
  performed accordingly. This command simulates the batch behavior of
  NuSMV and then returns the control to the interactive shell.<p>

  Command options:<p>
  <dl>
    <dt> <tt>-f</tt>
       <dd> Forces model construction even when COI is enabled.
    <dt> <tt>-r</tt>
       <dd> Performs a variable ordering at the end of the
       computation, and dumps the variable ordering as the command
       line option <tt>-reorder</tt> does.
    <dt> <tt>-i model-file</tt>
       <dd> Sets the environment variable <tt>input_file</tt> to file
           <tt>model-file</tt>, and reads the model from file
           <tt>model-file</tt>.
    <dt> <tt>-m Method</tt>
       <dd> Sets the environment variable <tt>partition_method</tt> to
       <tt>Method</tt> and uses it as partitioning method.
  </dl>

*/

int CommandProcessModel(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  int c;
  char * partition_method = NIL(char);
  boolean force_reordering = false;
  boolean force_build = false;
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  util_getopt_reset();
  while((c = util_getopt(argc,argv,"hfri:m:")) != EOF){
    switch (c) {
    case 'f': force_build = true; break;
    case 'r': force_reordering = true; break;
    case 'i': {
      set_input_file(opts, util_optarg);
      break;
    }
    case 'm': {
      partition_method = ALLOC(char, strlen(util_optarg)+1);
      strcpy(partition_method, util_optarg);
      break;
    }
    case 'h': goto CommandProcessModel_exit_usage;
    default: goto CommandProcessModel_exit_usage;
    }
  }

  if (argc != util_optind) goto CommandProcessModel_exit_usage;

  if (get_input_file(opts) == (char *)NULL) {
    StreamMgr_print_error(streams,  "Input file is (null). You must set the input file before.\n");
    goto CommandProcessModel_exit_1;
  }

  if (partition_method != NIL(char)) {
    if (TransType_from_string(partition_method) != TRANS_TYPE_INVALID) {
      set_partition_method(opts, TransType_from_string(partition_method));
    } else {
      StreamMgr_print_error(streams,  "The only possible values for \"-m\" option are:\n\t");
      print_partition_method(errstream);
      StreamMgr_print_error(streams,  "\n");
      goto CommandProcessModel_exit_1;
    }
  }

  if (cmp_struct_get_read_model(cmps) == 0)
    if (Cmd_CommandExecute(env, "read_model")) goto CommandProcessModel_exit_1;
  if (cmp_struct_get_flatten_hrc(cmps) == 0)
    if (Cmd_CommandExecute(env, "flatten_hierarchy")) goto CommandProcessModel_exit_1;
  if (cmp_struct_get_encode_variables(cmps) == 0)
    if (Cmd_CommandExecute(env, "encode_variables")) goto CommandProcessModel_exit_1;
  if (cmp_struct_get_build_model(cmps) == 0) {
    if (!force_build) {
      if(Cmd_CommandExecute(env, "build_model")) goto CommandProcessModel_exit_1;
    }
    else if(Cmd_CommandExecute(env, "build_model -f")) {
      goto CommandProcessModel_exit_1;
    }
  }
  if (opt_forward_search(opts))
    if (Cmd_CommandExecute(env, "compute_reachable")) goto CommandProcessModel_exit_1;

  if (opt_check_fsm(opts))
    if (Cmd_CommandExecute(env, "check_fsm")) goto CommandProcessModel_exit_1;

  if (! opt_ignore_spec(opts))
    if (Cmd_CommandExecute(env, "check_ctlspec")) goto CommandProcessModel_exit_1;

  if (! opt_ignore_compute(opts))
    if (Cmd_CommandExecute(env, "check_compute")) goto CommandProcessModel_exit_1;

  if (! opt_ignore_ltlspec(opts))
    if (Cmd_CommandExecute(env, "check_ltlspec")) goto CommandProcessModel_exit_1;

  if (! opt_ignore_pslspec(opts))
    if (Cmd_CommandExecute(env, "check_pslspec")) goto CommandProcessModel_exit_1;

  if (! opt_ignore_invar(opts))
    if (Cmd_CommandExecute(env, "check_invar")) goto CommandProcessModel_exit_1;

  if (opt_verbose_level_gt(opts, 0))
    if (Cmd_CommandExecute(env, "print_usage")) goto CommandProcessModel_exit_1;

  if (force_reordering) { /* If the case activate reordering */
    DDMgr_ptr dd = (DDMgr_ptr )NuSMVEnv_get_value(env, ENV_DD_MGR);

    StreamMgr_print_output(streams,  "\n========= starting reordering ============\n");
    (void)dd_reorder(dd, get_reorder_method(opts), DEFAULT_MINSIZE);

    if (Cmd_CommandExecute(env, "write_order")) goto CommandProcessModel_exit_1;

    StreamMgr_print_output(streams,  "\n========= after reordering ============\n");

    if (opt_verbose_level_gt(opts, 0))
      if (Cmd_CommandExecute(env, "print_usage")) goto CommandProcessModel_exit_1;
  }

  if (partition_method != NIL(char)) FREE(partition_method);
  return 0;

 CommandProcessModel_exit_1:
  if (partition_method != NIL(char)) FREE(partition_method);
  return 1;

 CommandProcessModel_exit_usage:
  if (partition_method != NIL(char)) FREE(partition_method);
  return(UsageProcessModel(env));
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageProcessModel(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: process_model [-r] [-h] [-i model-file] [-m method]\n");
  StreamMgr_print_error(streams,  "   -h \t\t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -f \t\t\tForces model construction.\n");
  StreamMgr_print_error(streams,  "   -r \t\t\tForces a reordering and dumps the new vars order.\n");
  StreamMgr_print_error(streams,  "   -i model-file \tReads the model from file \"model-file\".\n");
  StreamMgr_print_error(streams,  "   -m method\t\tUses \"method\" as partition method in model construction.\n");
  return 1;
}

/*!
  \command{flatten_hierarchy} Flattens the hierarchy of modules

  \command_args{[-h] [-d]}


  This command is responsible of the instantiation of modules and
  processes. The instantiation is performed by substituting the actual
  parameters for the formal parameters, and then by prefixing the result via
  the instance name.
  <p>
  Command options:<p>
  <dl>
    <dt><tt>-d</tt>
     <dd>Delays the construction of vars constraints until needed
    </dl>

*/

int CommandFlattenHierarchy(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int c;
  boolean calc_vars_constrains = true;
  boolean expand_bounded_arrays = false;

  util_getopt_reset();
  while ((c = util_getopt(argc,argv,"hde")) != EOF) {
    switch (c) {
    case 'h': return UsageFlattenHierarchy(env);
    case 'd': calc_vars_constrains = false; break;
      /* [AI] need to add an option for array expansion
         Similarly interface for CompileFlatten_flatten_smv need to be
         modified with this new option */
    case 'e': expand_bounded_arrays = true; break;
    default:  return UsageFlattenHierarchy(env);
    }
  }
  if (argc != util_optind) return UsageFlattenHierarchy(env);
  if (cmp_struct_get_read_model(cmps) == 0) {
    StreamMgr_print_error(streams,  "A model must be read before. Use the \"read_model\" command.\n");
    return 1;
  }

  if (cmp_struct_get_flatten_hrc(cmps)) {
    StreamMgr_print_error(streams,  "The hierarchy has already been flattened.\n");
    return 1;
  }

  if (cmp_struct_get_hrc_built(cmps)) {
    /* the HRC was already built thanks to lax parser, but there are errors */
    StreamMgr_print_error(streams,  "The hierarchy cannot be flattened, as errors have been found.\n");
    StreamMgr_print_error(streams,  "At this stage you can dump the (partial) HRC, or use the\n");
    StreamMgr_print_error(streams,  "command 'reset' to restart.\n");
    return 1;
  }

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Flattening hierarchy...\n");
  }

  return CompileFlatten_flatten_smv(env, calc_vars_constrains, expand_bounded_arrays);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageFlattenHierarchy(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: flatten_hierarchy [-h]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage\n");
  StreamMgr_print_error(streams,  "   -d \t\tDelays the construction of vars constraints until needed\n");
  StreamMgr_print_error(streams,  "   -e \t\tExpands the bounded arrays into individual elements\n");
  return 1;
}

/*!
  \command{show_vars} Shows model's symbolic variables and their values

  \command_args{[-h] [-s] [-f] [-i] [-v] [-t|-V]
                      [-m | -o output-file]}


  Prints symbolic input, frozen and state variables of the model with their
  range of values (as defined in the input file).
  <p>
  Command Options:<p>
  <dl>
    <dt> <tt>-s</tt>
       <dd> Prints state variables.
    <dt> <tt>-f</tt>
       <dd> Prints frozen variables.
    <dt> <tt>-i</tt>
       <dd> Prints input variables.
    <dt> <tt>-t</tt>
       <dd> Prints only the number of variables (among selected
            kinds), grouped by type. Incompatible with -V.
    <dt> <tt>-V</tt>
       <dd> Prints only the list of variables with their types (among
            selected kinds), and no other summary
            information. Incompatible with -t.
    <dt> <tt>-D</tt>
       <dd> Prints only the list of defines. Incompatible with -V.
    <dt> <tt>-v</tt>
       <dd> Prints verbosely. With this option, all scalar variable values are
            printed
    <dt> <tt>-m</tt>
       <dd> Pipes the output to the program specified by the
           <tt>PAGER</tt> shell variable if defined, else through the
           <tt>UNIX</tt> command "more".
    <dt> <tt>-o output-file</tt>
       <dd> Writes the output generated by the command to <tt>output-file</tt>
  </dl>
  <p>

  By default, if no type specifiers (-s, -f, -i) are used, all
  variables type will be printed. When using one or more type
  specifiers (e.g. -s), only variables belonging to selected types
  will be printed.
*/

int CommandShowVars(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* errstream = StreamMgr_get_error_stream(streams);
  int c = 0;
  boolean statevars = false;
  boolean frozenvars = false;
  boolean inputvars = false;
  boolean noneselected = true;
  boolean verbose = false;
  boolean total_only = false;
  boolean vars_only = false;
  boolean defs_only = false;
  short int useMore = 0;
  char* dbgFileName = NIL(char);
  FILE* old_outstream = NIL(FILE);

  util_getopt_reset();
  while ((c = util_getopt(argc,argv,"hsiftVDvmo:")) != EOF){

    switch (c) {
    case 'h': goto show_vars__usage;
    case 's':
      statevars = true;
      noneselected = false;
      break;

    case 'f':
      frozenvars = true;
      noneselected = false;
      break;

    case 'i':
      inputvars = true;
      noneselected = false;
      break;

    case 't':
      if (vars_only || defs_only) {
        StreamMgr_print_error(streams,
                              "-t is incompatible with options -V or -D.\n");
        goto show_vars__fail;
      }
      total_only = true;
      break;

    case 'V':
      if (total_only || defs_only) {
        StreamMgr_print_error(streams,
                              "-V is incompatible with options -t or -D.\n");
        goto show_vars__fail;
      }
      vars_only = true;
      break;

    case 'D':
      if (vars_only || total_only) {
        StreamMgr_print_error(streams,
                              "-D is incompatible with options -V or -t.\n");
        goto show_vars__fail;
      }
      defs_only = true;
      break;

    case 'v':
      verbose = true;
      break;

    case 'o':
      if (useMore == 1) goto show_vars__usage;
      dbgFileName = util_strsav(util_optarg);
      StreamMgr_print_output(streams,  "Output to file: %s\n", dbgFileName);
      break;

    case 'm':
      if (dbgFileName != NIL(char)) goto show_vars__usage;
      useMore = 1;
      break;

    default:
      goto show_vars__usage;
    }
  }

  if (argc != util_optind)
    goto show_vars__usage;

  /* if (!statevars && !frozenvars && !inputvars) goto show_vars__usage; */

  /* we need only a flattened hierarchy to be able to see the variables */
  if (Compile_check_if_flattening_was_built(env, errstream))
    goto show_vars__fail;

  if (useMore) {
    old_outstream = outstream;
    outstream = CmdOpenPipe(env, useMore);
    if (outstream==(FILE*) NULL) {
      outstream=old_outstream;
      goto show_vars__fail;
    }
  }

  if (dbgFileName != NIL(char)) {
    old_outstream = outstream;
    outstream = CmdOpenFile(env, dbgFileName);
    if (outstream==(FILE*) NULL) {
      outstream = old_outstream;
      goto show_vars__fail;
    }
  }

  {
    OStream_ptr ostream;

    ostream = OStream_create(outstream);

    Compile_show_vars(env, total_only, defs_only, vars_only,
                      statevars || noneselected,
                      frozenvars || noneselected,
                      inputvars || noneselected,
                      ostream, verbose);

    if (useMore) {
      CmdClosePipe(outstream);
      outstream = old_outstream;
    }

    if (dbgFileName != NIL(char)) {
      CmdCloseFile(outstream);
      outstream = old_outstream;
      FREE(dbgFileName);
    }

    OStream_destroy_safe(ostream);
  }

  return 0;

 show_vars__fail:
  if (dbgFileName != NIL(char)) {
    FREE(dbgFileName);
  }
  return 1;

 show_vars__usage:
  if (dbgFileName != NIL(char)) FREE(dbgFileName);
  return UsageShowVars(env);
}


static int UsageShowVars (const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  const char* lines[] = {
    "usage: show_vars [-h] [-s] [-f] [-i] [-v] [-t | -V | -D] [-m | -o file]",
    "  -h \t\tPrints the command usage.",
    "  -s \t\tPrints the state variables.",
    "  -f \t\tPrints the frozen variables.",
    "  -i \t\tPrints the input variables.",
    "  -t \t\tPrints only the number of variables (among selected kinds), grouped by type.",
    "     \t\tThis option is incompatible with -V and -D.",
    "  -V \t\tPrints only the list of variables with their types (among selected kinds),",
    "     \t\twith no summary information. This option is incompatible with -t and -D",
    "  -D \t\tPrints only the list of defines. Incompatible with -t and -V.",
    "  -v \t\tPrints verbosely.",
    "  -m \t\tPipes output through the program specified by the \"PAGER\".",
    "     \t\tenvironment variable if defined, else through the UNIX command \"more\".",
    "  -o file\tWrites the generated output to \"file\".",
    "\n  By default, if no type specifiers (-s, -f, -i) are used, all",
    "  variable types will be printed. When using one or more type",
    "  specifiers (e.g. -s), only variables belonging to selected types",
    "  will be printed.",
  };

  int i;
  for (i=0; i<sizeof(lines)/sizeof(*lines); ++i) {
    StreamMgr_print_error(streams, lines[i]);
    StreamMgr_print_error(streams, "\n");
  }

  return 1;
}

/*!
  \command{encode_variables} Builds the BDD variables necessary to compile the
  model into BDD.

  \command_args{[-h] [-i order-file]}


  Generates the boolean BDD variables and the ADD needed to encode
  propositionally the (symbolic) variables declared in the model.<br>

  The variables are created as default in the order in which they
  appear in a depth first traversal of the hierarchy.<p>

  The input order file can be partial and can contain variables not
  declared in the model. Variables not declared in the model are
  simply discarded. Variables declared in the model which are not
  listed in the ordering input file will be created and appended at the
  end of the given ordering list, according to the default ordering.<p>

  Command options:<p>
  <dl>
    <dt> <tt>-i order-file</tt>
       <dd> Sets the environment variable <tt>input_order_file</tt> to
       <tt>order-file</tt>, and reads the variable ordering to be used from
       file <tt>order-file</tt>. This can be combined with the
       <tt>write_order</tt> command. The variable ordering is written to a
       file, which can be inspected and reordered by the user, and then
       read back in.
  </dl>
*/

int CommandEncodeVariables(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  int c;
  char* input_order_file_name = NIL(char);
  int res = 1;
  boolean bdd_enc_enum_only = false;

  util_getopt_reset();
  while ((c = util_getopt(argc,argv,"i:hn")) != EOF) {
    switch (c) {
    case 'i':
      /* Option cannot be defined twice */
      if (NIL(char) != input_order_file_name) { goto encode_variables_usage; }
      input_order_file_name = ALLOC(char, strlen(util_optarg)+1);
      strcpy(input_order_file_name, util_optarg);
      break;

    case 'n':
      if (bdd_enc_enum_only) { goto encode_variables_usage; }
      bdd_enc_enum_only = true;
      break;

    case 'h': goto encode_variables_usage;
    default:  goto encode_variables_usage;
    }
  }

  if (argc != util_optind) goto encode_variables_usage;

  /* pre-conditions: */
  if (Compile_check_if_flattening_was_built(env, errstream)) {
    goto encode_variables_free;
  }

  if (cmp_struct_get_encode_variables(cmps)) {
    StreamMgr_print_error(streams,  "The variables appear to be already built.\n");
    goto encode_variables_free;
  }

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Building variables...");
  }

  {
    int res;

    res = Compile_encode_variables(env, input_order_file_name,
                                   bdd_enc_enum_only);

    if (res != 0) { goto encode_variables_free; }
  }

  if (!opt_reorder(opts)
      && !is_default_order_file(opts)
      && !util_is_string_null(get_output_order_file(opts))) {
    VarOrderingType dump_type;
    BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));

    if (opt_write_order_dumps_bits(opts)) dump_type = DUMP_BITS;
    else dump_type = DUMP_DEFAULT;

    res = BddEnc_write_var_ordering(bdd_enc,
                                    get_output_order_file(opts),
                                    dump_type);

    if (0 != res) { goto encode_variables_free; }

    /* batch mode: */
    if (opt_batch(opts)) { ErrorMgr_nusmv_exit(errmgr, 0); }
  }

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "...done\n");
  }

  res = 0;
  goto encode_variables_free;

 encode_variables_usage:
  res = UsageEncodeVariables(env);
 encode_variables_free:
  if (NIL(char) != input_order_file_name) {
    FREE(input_order_file_name);
  }

  return res;
}

/*!
  \brief


*/
static int UsageEncodeVariables(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: encode_variables [-h] [-i <file>]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -i <file> \tReads variable ordering from file <file>.\n");
  StreamMgr_print_error(streams,  "   -n \t\tEncode with BDDs only if the model contains enum variables.\n");
  return 1;
}

/*!
  \command{build_model} Compiles the flattened hierarchy into BDD

  \command_args{[-h] [-f] [-m Method]}


  Compiles the flattened hierarchy into BDD (initial states, invariants,
  and transition relation) using the method specified in the environment
  variable <tt>partition_method</tt> for building the transition relation.<p>

  Command options:<p>
  <dl>
    <dt> <tt>-m Method</tt>
       <dd> Sets the environment variable <tt>partition_method</tt> to
           the value <tt>Method</tt>, and then builds the transition
           relation. Available methods are <code>Monolithic</code>,
           <code>Threshold</code> and <code>Iwls95CP</code>.
    <dt> <tt>-f</tt>
       <dd> Forces model construction. By default, only one partition
            method is allowed. This option allows to overcome this
            default, and to build the transition relation with different
            partitioning methods.
  </dl>
*/

int CommandBuildModel(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  int c;
  boolean force_build = false;
  char * partition_method = NIL(char);
  int retval = 0;
  TransType partition_method_enum = TRANS_TYPE_INVALID;

  util_getopt_reset();
  while((c = util_getopt(argc,argv,"m:fh")) != EOF){
    switch(c){
    case 'm': {
      partition_method = ALLOC(char, strlen(util_optarg)+1);
      strcpy(partition_method, util_optarg);
      break;
    }
    case 'f': {
      force_build = true;
      break;
    }
    case 'h': return(UsageBuildModel(env));
    default:  return(UsageBuildModel(env));
    }
  }
  if (argc != util_optind) {
    if (partition_method != NIL(char)) {
      FREE(partition_method);
    }
    return(UsageBuildModel(env));
  }

  /* pre-conditions: */
  if (Compile_check_if_encoding_was_built(env, errstream)) {
    if (partition_method != NIL(char)) {
      FREE(partition_method);
    }
    return 1;
  }
#if __BDDENC_LAZY_COMMIT_LAYER__
  /* The check is not needed since lazy commit will be performed */
#else
  else if (0 != Compile_check_if_model_layer_is_in_bddenc(env, errstream)) {
    /* [SM] Special case
       When using the "-n" option on encode_variables the model layer
       is NOT committed to the BDD encoder.
       In this case, the model cannot be built.
    */
    return 1;
  }
#endif
  if (!force_build && cmp_struct_get_build_model(cmps)) {
    StreamMgr_print_error(streams,  "A model appears to be already built from file: %s.\n",
            get_input_file(opts));
    if (partition_method != NIL(char)) {
      FREE(partition_method);
    }
    return 1;
  }

  if (partition_method != NIL(char)) {
    if (TransType_from_string(partition_method) != TRANS_TYPE_INVALID) {
      if ((force_build) &&
          (TransType_from_string(partition_method) == get_partition_method(opts))) {
        if (cmp_struct_get_build_model(cmps)) {
          StreamMgr_print_error(streams,  "A model for the chosen method has already been constructed.\n");
          FREE(partition_method);
          return 1;
        }
      }
      partition_method_enum = TransType_from_string(partition_method);
    } else {
      StreamMgr_print_error(streams,  "The only possible values for \"-m\" option are:\n\t");
      print_partition_method(errstream);
      StreamMgr_print_error(streams,  "\n");
      FREE(partition_method);
      return 1;
    }
  }

  /* constructs the model only if coi is not enabled */
  if (opt_cone_of_influence(opts) && !force_build) {
    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,
              "Construction of BDD model is delayed due to use of COI\n");
    }
    if (partition_method != NIL(char)) {
      FREE(partition_method);
    }
    return 0;
  }

  retval = Compile_build_model(env, partition_method_enum);

  if (partition_method != NIL(char)) {
    FREE(partition_method);
  }

  return retval;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageBuildModel(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  StreamMgr_print_error(streams,  "usage: build_model [-h] [-f] [-m Method]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage\n");
  StreamMgr_print_error(streams,  "   -m Method \tUses \"Method\" as partitioning method, and set it as default method\n");
  StreamMgr_print_error(streams,  "\t\tto be used in the following image computations.\n");
  StreamMgr_print_error(streams,  "\t\tThe currently available methods are:\n\t\t");
  print_partition_method(errstream);
  StreamMgr_print_error(streams,  "\n   -f \t\tForces the model re-construction, even if a model has already been built\n");
  return 1;
}

/*!
  \command{build_flat_model} Compiles the flattened hierarchy into SEXP

  \command_args{[-h]}


  Compiles the flattened hierarchy into SEXP (initial states, invariants,
  and transition relation).<p>
*/

int CommandBuildFlatModel(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  int c;

  util_getopt_reset();
  while((c = util_getopt(argc,argv,"h")) != EOF){
    switch(c){
    case 'h': return(UsageBuildFlatModel(env));
    default:  return(UsageBuildFlatModel(env));
    }
  }
  if (argc != util_optind) return(UsageBuildFlatModel(env));

  /* pre-conditions: */
  if (Compile_check_if_flattening_was_built(env, errstream)) return 1;

  if (cmp_struct_get_build_flat_model(cmps)) {
    StreamMgr_print_error(streams,  "A model appears to be already built from file: %s.\n",
            get_input_file(opts));
    return 1;
  }

  Compile_create_flat_model(env);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
            "\nThe sexp model has been built from file %s.\n",
      get_input_file(opts));
  }

  return 0;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageBuildFlatModel(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: build_flat_model [-h]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  return 1;
}


/*!
  \command{build_boolean_model} Compiles the flattened hierarchy into boolean SEXP

  \command_args{[-h] [-f]}


  Compiles the flattened hierarchy into boolean SEXP
  (initial states, invariants, and transition relation).<p>
*/

int CommandBuildBooleanModel(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int c;
  boolean forced = false;
  int retval = 0;

  util_getopt_reset();
  while((c = util_getopt(argc,argv,"hf")) != EOF){
    switch(c){
    case 'h': return(UsageBuildBooleanModel(env));
    case 'f': forced = true; break;
    default:  return(UsageBuildBooleanModel(env));
    }
  }
  if (argc != util_optind) return(UsageBuildBooleanModel(env));

  /* pre-conditions: */
  if (Compile_check_if_encoding_was_built(env, errstream)) {
    retval = 1; return retval;
  }

  if (cmp_struct_get_build_bool_model(cmps) && !forced) {
    StreamMgr_print_error(streams,  "A model appears to be already built from file: %s.\n",
            get_input_file(opts));
    retval = 1; return retval;
  }

  /* constructs the model only if coi is not enabled */
  if (opt_cone_of_influence(opts) && !forced) {
    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,
        "Construction of boolean model is delayed due to use of COI\n");
    }
    retval = 0; return retval;
  }

  /* creates the flat fsm */
  if (!NuSMVEnv_has_value(env, ENV_SEXP_FSM)) {
    Compile_create_flat_model(env);
  }

  /* creates the boolean fsm */
  retval = Compile_create_boolean_model(env);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
            "\nThe boolean sexp model has been built from file %s.\n",
      get_input_file(opts));
  }

  return retval;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageBuildBooleanModel(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: build_boolean_model [-h][-f]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -f \t\tForces the boolean model construction.\n");
  return 1;
}

/*!
  \command{write_order} Writes variable order to file.

  \command_args{[-h] [-b] [(-o | -f) order-file]}

  Writes the current order of BDD variables in the
  file specified via the -o option. If no option is specified the environment
  variable <tt>output_order_file</tt> will be considered. If the variable
  <tt>output_order_file</tt> is unset (or set to an empty value) then standard
  output will be used. The option <tt>-b</tt> forces the dumped
  variable ordering to contain only boolean variables.
  All the scalar variables will be substituted by those variables bits
  that encode them.  The variables bits will occur within the dumped
  variable ordering depending on the position they have within the
  system when the command is executed.
  <p>
  Command options:<p>
  <dl>
    <dt> <tt>-b</tt>
       <dd> Dumps bits of scalar variables instead of the single
       scalar variables. When specified, this option temporary
       overloads the current value of the system variable
       <tt>write_order_dumps_bits</tt>.

    <dt> <tt>-o order-file</tt>
       <dd> Sets the environment variable <tt>output_order_file</tt>
       to <tt>order-file</tt> and then dumps the ordering list into that file.
    <dt> <tt>-f order-file</tt>
       <dd> Alias for -o option. Supplied for backward compatibility.
  </dl>
*/

int CommandWriteOrder(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  int c;
  char* order_output_fname = NIL(char);
  VarOrderingType dump_type;
  int retval = 0;

  if (opt_write_order_dumps_bits(opts)) dump_type = DUMP_BITS;
  else dump_type = DUMP_DEFAULT;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "o:f:hb")) != EOF) {
    switch (c) {
    case 'b':
      dump_type = DUMP_BITS;
      break;

    case 'o':
    case 'f':
      if (order_output_fname != NIL(char)) {
        /* already called (via the alias): exit */
        retval = UsageWriteOrder(env);
        goto exit;
      }
      else {
        order_output_fname = ALLOC(char, strlen(util_optarg)+1);
        nusmv_assert(order_output_fname);
        strcpy(order_output_fname, util_optarg);
      }
      break;

    case 'h':
    default:
      retval = UsageWriteOrder(env);
      goto exit;
    }
  }

  if (!NuSMVEnv_has_value(env, ENV_DD_MGR)) {
    StreamMgr_print_error(streams,  "The DD Manager has not been created yet.\n");
    retval = 1;
    goto exit;
  }

  /* pre-conditions: */
  if (Compile_check_if_encoding_was_built(env, errstream)) {
    retval = 1; goto exit;
  }

  {
    BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));

    if (NULL == order_output_fname) {
      /* we copy the filename, this to simplify the handling since the
         get_output_order_file() does not make a copy of the string,
         and keeps the ownership. */
      order_output_fname = util_strsav(get_output_order_file(opts));
    }

    retval = BddEnc_write_var_ordering(bdd_enc,
                                       order_output_fname,
                                       dump_type);
  }

 exit:
  if (NULL != order_output_fname) {
    FREE(order_output_fname);
  }

  /* batch mode: */
  if (opt_batch(opts) && !opt_reorder(opts))  { ErrorMgr_nusmv_exit(errmgr, retval); }

  return retval;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageWriteOrder(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  StreamMgr_print_error(streams,  "usage: write_order [-h] | [-b] [(-o | -f) <file>]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,
    "   -b \t\tDumps bits of scalar variables instead of the single \n"\
    "      \t\tscalar variables. \n"\
    "      \t\tSee also the system variable write_order_dumps_bits.\n");
  StreamMgr_print_error(streams,  "   -o <file>\tWrites ordering to file <file>.\n");
  StreamMgr_print_error(streams,  "   -f <file>\tThe same of option -o. Supplied for backward compatibility.\n");

  return 1;
}

int CommandCPPrintClusterInfo(NuSMVEnv_ptr env, int argc, char** argv)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  int res;

  StreamMgr_print_error(streams,  "----------------------------------------------------------------------\n");
  StreamMgr_print_error(streams,  "--             Deprecated in 2.4: use 'print_fsm_stats'             --\n");
  StreamMgr_print_error(streams,  "----------------------------------------------------------------------\n");

  res = CommandPrintFsmStats(env, argc, argv);

  StreamMgr_print_error(streams,  "----------------------------------------------------------------------\n");
  StreamMgr_print_error(streams,  "--             Deprecated in 2.4: use 'print_fsm_stats'             --\n");
  StreamMgr_print_error(streams,  "----------------------------------------------------------------------\n");

  return res;
}


/*!
  \command{print_fsm_stats} Prints out information about the fsm and clustering.

  \command_args{[-h] | [-m] | [-p] | [-o output-file]}

  This command prints out information
  regarding the fsm and each cluster. In particular for each cluster
  it prints out the cluster number, the size of the cluster (in BDD
  nodes), the variables occurring in it, the size of the cube that has
  to be quantified out relative to the cluster and the variables to be
  quantified out.<p>

   Also the command can print all the normalized predicates the FMS
   consists of. A normalized predicate is a boolean expression which
   does not have other boolean sub-expressions. For example,
   expression (b<0 ? a/b : 0) = c is normalized into (b<0
   ? a/b=c : 0=c) which has 3 normalized predicates inside:
   b<0, a/b=c, 0=c.

  Command options:<p>
  <dl>
    <dt> <tt>-m</tt>
       <dd> Pipes the output generated by the command through the
            program specified by the <tt>PAGER</tt> shell variable if
            defined, or through the UNIX utility "more".
    <dt> <tt>-p</tt>
       <dd> Prints out the normalized predicates the FSM consists of.
    <dt> <tt>-o output-file</tt>
       <dd> Redirects the generated output to the file
            <tt>output-file</tt>.
  </dl>

*/

int CommandPrintFsmStats(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  BddFsm_ptr bdd_fsm = NULL;

  int c;
  int useMore = 0;
  int retval = 0;
  boolean printPreds = false;
  char* dbgFileName = (char*) NULL;
  FILE* old_outstream = NIL(FILE);

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hmpo:")) != EOF) {
    switch (c) {
    case 'h':
      retval = UsagePrintFsmStats(env);
      goto CommandPrintFsmStats_exit;

    case 'o':
      if (useMore == 1) {
        retval = UsagePrintFsmStats(env);
        goto CommandPrintFsmStats_exit;
      }
      if ((char*) NULL != dbgFileName) FREE(dbgFileName);
      dbgFileName = util_strsav(util_optarg);
      nusmv_assert((char*) NULL != dbgFileName);
      break;

    case 'm':
      if (dbgFileName != NIL(char)) {
        retval = UsagePrintFsmStats(env);
        goto CommandPrintFsmStats_exit;
      }
      useMore = 1;
      break;

    case 'p':
      printPreds = true;
      break;

    default:
      retval = UsagePrintFsmStats(env);
      goto CommandPrintFsmStats_exit;
    }
  } /* while */

  if (0 != Compile_check_if_model_was_built(env,
                                            StreamMgr_get_error_stream(streams),
                                            true)) {
    retval = 1;
    goto CommandPrintFsmStats_exit;
  }

  if (printPreds && !cmp_struct_get_flatten_hrc(cmps)) {
    StreamMgr_print_error(streams,  "\nError: option -p of print_fsm_stats requires "
            "the model be flattened. Use command \"flatten_hierarchy\".\n");
    retval = 1;
    goto CommandPrintFsmStats_exit;
  }

  if (useMore) {
    old_outstream = outstream;
    outstream = CmdOpenPipe(env, useMore);
    if (outstream==(FILE*) NULL) {
      retval = 1;
      goto CommandPrintFsmStats_exit;
    }
  }

  if (dbgFileName != NIL(char)) {
    old_outstream = outstream;
    outstream = CmdOpenFile(env, dbgFileName);
    if (outstream==(FILE*) NULL) {
      retval = 1;
      goto CommandPrintFsmStats_exit;
    }
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Output to file: %s\n", dbgFileName);
    }
  }

  bdd_fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));
  retval = Compile_print_fsm_stats(env, bdd_fsm, outstream, printPreds);

CommandPrintFsmStats_exit:
  if (useMore && (FILE*) NULL != old_outstream) {
    if (outstream != (FILE*) NULL) CmdClosePipe(outstream);
    outstream = old_outstream;
  }
  if (dbgFileName != NIL(char) && (FILE*) NULL != old_outstream) {
    CmdCloseFile(outstream);
    outstream = old_outstream;
    FREE(dbgFileName);
  }

  return retval;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsagePrintFsmStats(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: print_fsm_stats [-h] [-m] [-o file] [-p]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage\n");
  StreamMgr_print_error(streams,  "   -m \t\tPipes output through the program specified by\n");
  StreamMgr_print_error(streams,  "      \t\tthe \"PAGER\" shell variable if defined,\n");
  StreamMgr_print_error(streams,  "      \t\t else through the UNIX command \"more\"\n");
  StreamMgr_print_error(streams,  "   -p \t\tPrints out the normalized predicates the FSM\n");
  StreamMgr_print_error(streams,  "      \t\tconsists of. A normalized predicate is a boolean\n");
  StreamMgr_print_error(streams,  "      \t\texpression without boolean sub-expressions.\n");
  StreamMgr_print_error(streams,  "   -o file\tWrites the output to \"file\".\n");
  return 1;
}

int CommandIwls95PrintOption(NuSMVEnv_ptr env, int argc, char** argv)
{
  int c;
  ClusterOptions_ptr opts;
  const OptsHandler_ptr opt =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "h")) != EOF) {
    switch (c) {
    case 'h':
      return(UsageIwls95PrintOption(env));
    default:
      return(UsageIwls95PrintOption(env));
    }
  } /* while */

  opts = ClusterOptions_create(opt);
  ClusterOptions_print(opts, outstream);
  ClusterOptions_destroy(opts);

  return 0;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageIwls95PrintOption(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: print_iwls95options [-h]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  return 1;
}

/*!
  \command{go} Initializes the system for the verification.

  \command_args{[-h][-f]}

  This command initializes the system for
  verification. It is equivalent to the command sequence
  <tt>read_model</tt>, <tt>flatten_hierarchy</tt>,
  <tt>build_flat_model</tt>, <tt>encode_variables</tt>,
  <tt>build_model</tt>.<p>
  If some commands have already been
  executed, then only the remaining ones will be invoked.<p>
  Command options:<p>
  <dl><dt> -f
  <dd> Forces the model contruction.<p>
  <dt> -h
  <dd> Prints the command usage.<p>
  </dl>

*/

int CommandGo(NuSMVEnv_ptr env, int argc, char** argv)
{
  int c;
  boolean forced = false;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hf")) != EOF) {
    switch (c) {
    case 'h': return UsageGo(env);
    case 'f': forced = true; break;
    default: return UsageGo(env);
    }
  } /* while */

  if (cmp_struct_get_read_model(cmps) == 0)
    if (Cmd_CommandExecute(env, "read_model")) return 1;
  if (cmp_struct_get_flatten_hrc(cmps) == 0)
    if (Cmd_CommandExecute(env, "flatten_hierarchy")) return 1;
  if (cmp_struct_get_build_flat_model(cmps) == 0)
    if(Cmd_CommandExecute(env, "build_flat_model")) return 1;
  if (cmp_struct_get_encode_variables(cmps) == 0)
    if (Cmd_CommandExecute(env, "encode_variables")) return 1;
  if (cmp_struct_get_build_model(cmps) == 0) {
    if (!forced) { if (Cmd_CommandExecute(env, "build_model")) return 1; }
    else if (Cmd_CommandExecute(env, "build_model -f")) return 1;
  }
  return 0;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageGo(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: go [-h] | [-f]\n");
  StreamMgr_print_error(streams,  "   -f \t\tForces the model construction\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  return 1;
}

/*!
  \command{go_bmc} Initializes the system for the BMC verification.

  \command_args{[-h] | [-f]}

  This command initializes the system for
  verification. It is equivalent to the command sequence
  <tt>read_model</tt>, <tt>flatten_hierarchy</tt>,
  <tt>encode_variables</tt>, <tt>build_boolean_model</tt>, <tt>bmc_setup</tt>.
  If some commands have already been
  executed, then only the remaining ones will be invoked.<p>
  Command options:<p>
  <dl>
  <dt> -f
  <dd> Forces the model construction.<p>
  <dt> -h
  <dd> Prints the command usage.<p>
  </dl>

*/

int CommandGoBmc(NuSMVEnv_ptr env, int argc, char** argv)
{
  int c;
  boolean forced = false;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hf")) != EOF) {
    switch (c) {
    case 'h': return(UsageGoBmc(env));
    case 'f': forced = true; break;
    default: return(UsageGoBmc(env));
    }
  } /* while */

  if (cmp_struct_get_read_model(cmps) == 0)
    if (Cmd_CommandExecute(env, "read_model")) return 1;
  if (cmp_struct_get_flatten_hrc(cmps) == 0)
    if (Cmd_CommandExecute(env, "flatten_hierarchy")) return 1;
  if (cmp_struct_get_encode_variables(cmps) == 0)
    if (Cmd_CommandExecute(env, "encode_variables -n")) return 1;
  if (cmp_struct_get_build_bool_model(cmps) == 0) {
    if (!forced) { if (Cmd_CommandExecute(env, "build_boolean_model")) return 1; }
    else if (Cmd_CommandExecute(env, "build_boolean_model -f")) return 1;
  }
  if (cmp_struct_get_bmc_setup(cmps) == 0) {
    if (!forced) { if (Cmd_CommandExecute(env, "bmc_setup")) return 1; }
    else if (Cmd_CommandExecute(env, "bmc_setup -f")) return 1;
  }

  return 0;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageGoBmc(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: go_bmc [-h] | [-f]\n");
  StreamMgr_print_error(streams,  "   -f \t\tForces the model contruction\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage\n");
  return 1;
}

/*!
  \command{get_internal_status} Returns the internal status of the system.

  \command_args{[-h]}

  Prints out the internal status of the system. i.e.
  <ul>
  <li> -1 : read_model has not yet been executed or an error occurred
            during its execution. </li>
  <li>  0 : flatten_hierarchy has not yet been executed or an error
            occurred during its execution. </li>
  <li>  1 : encode_variables has not yet been executed or an error
            occurred during its execution. </li>
  <li>  2 : build_model has not yet been executed or an error occurred
            during its execution. </li>
  </ul>
  Command options:<p>
  <dl><dt> -h
  <dd> Prints the command usage.<p>
  </dl>

*/

int CommandGetInternalStatus(NuSMVEnv_ptr env, int argc, char** argv)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  int c;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "h")) != EOF) {
    switch (c) {
    case 'h':
      return(UsageGetInternalStatus(env));
    default:
      return(UsageGetInternalStatus(env));
    }
  } /* while */

  if (cmp_struct_get_read_model(cmps) == 0){
    StreamMgr_print_error(streams,  "The internal status is: -1\n");
    return 0;
  }
  if (cmp_struct_get_flatten_hrc(cmps) == 0){
    StreamMgr_print_error(streams,  "The internal status is: 0\n");
    return 0;
  }
  if (cmp_struct_get_encode_variables(cmps) == 0){
    StreamMgr_print_error(streams,  "The internal status is: 1\n");
    return 0;
  }
  if (cmp_struct_get_build_model(cmps) == 0){
    StreamMgr_print_error(streams,  "The internal status is: 2\n");
    return 0;
  }
  return 0;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageGetInternalStatus(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: get_internal_status [-h]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  return 1;
}


/*!
  \command{write_flat_model} Writes a flat model of a given SMV file

  \command_args{[-h] [-o filename] [-A] [-m]}

  Processes are eliminated
  and a corresponding equivalent model is printed out.
  If no file is specified, the file specified with the environment variable
  <tt>output_flatten_model_file</tt> is used if any, otherwise standard output
  is used as output.
  <p>
  Command options:<p>
  <dl>
     <dt><tt>-o filename</tt>
       <dd> Attempts to write the flat SMV model in <tt>filename</tt>.
     <dt><tt>-A</tt>
       <dd> Write the model using variables and defines rewriting to
       make it anonimized.
     <dt><tt>-m</tt>
       <dd> Disable printing of key map when writing anonimized model
  </dl>

*/

int CommandWriteModelFlat(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  FILE* outstream = StreamMgr_get_output_stream(streams);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));


  int c = 0;
  int rv = 0;
  char* output_file = NIL(char);
  FILE* ofileid = NIL(FILE);
  int bSpecifiedFilename = FALSE;

  boolean obfuscated = false;
  boolean print_map = true;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hAo:m")) != EOF) {
    switch (c) {
    case 'h':
      goto __write_model_flat_fail_help;

    case 'o':
      output_file = ALLOC(char, strlen(util_optarg)+1);
      nusmv_assert(output_file);
      strcpy(output_file, util_optarg);
      bSpecifiedFilename = TRUE;
      break;

    case 'A':
      obfuscated = true;
      break;

    case 'm':
      print_map = false;
      break;

    default:
      goto __write_model_flat_fail_help;

    }
  }

  if (argc != util_optind) goto __write_model_flat_fail_help;

  if (output_file == NIL(char)) {
    output_file = get_output_flatten_model_file(opts);
  }
  if (output_file == NIL(char)) {
    ofileid = outstream;
  }
  else {
    ofileid = fopen(output_file, "w");
    if (ofileid == NULL) {
      StreamMgr_print_error(streams,  "Unable to open file \"%s\".\n", output_file);
      goto __write_model_flat_fail;
    }
  }

  /* pre-conditions: */
  if (Compile_check_if_flattening_was_built(env, errstream)) goto __write_model_flat_fail;

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Writing flat model into file \"%s\"..",
      output_file == (char *)NULL ? "stdout" : output_file);
  }

  CATCH(errmgr) {
    SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
    FlatHierarchy_ptr hierarchy = FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));

    if (!obfuscated) {
      Compile_WriteFlattenModel(env, ofileid, st,
                                SymbTable_get_class_layer_names(st,
                                                            (const char*) NULL),
                                "MODULE main", hierarchy, true);
    }
    else {
      NodeAnonymizerBase_ptr anonymizer = NULL;

      anonymizer =
        NODE_ANONYMIZER_BASE(NodeAnonymizerST_create(env, NULL, 1000, st));

      Compile_WriteObfuscatedFlattenModel(env, ofileid, st,
                                          SymbTable_get_class_layer_names(st,
                                                            (const char*) NULL),
                                          "MODULE main", hierarchy,
                                          print_map, true,
                                          anonymizer);

      NodeAnonymizerBase_destroy(anonymizer);
    }

    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, ".. done.\n");
    }
  }
  FAIL(errmgr) {
    rv = 1;
  }
  fflush(ofileid);

  clean_memory_before_return(ofileid, bSpecifiedFilename,
                             output_file, outstream);
  return rv;

  /* failure handlers */
 __write_model_flat_fail_help:
  clean_memory_before_return(ofileid, bSpecifiedFilename,
                             output_file, outstream);
  return UsageWriteModelFlat(env);

 __write_model_flat_fail:
  clean_memory_before_return(ofileid, bSpecifiedFilename,
                             output_file, outstream);
  return 1;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageWriteModelFlat(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: write_flat_model [-h] [-A] [-m] [-o filename]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -o filename\tWrites output to \"filename\"\n");
  StreamMgr_print_error(streams,  "  -A Write the model using variables and defines rewriting to make it anonimized.\n");
  StreamMgr_print_error(streams,  "  -m Disable printing of key map when writing anonimized model\n");
  return 1;
}


/*!
  \command{write_flat_model_udg} Writes a flat model of a given SMV file in uDraw format

  \command_args{[-h] [-o filename]}

  Processes are eliminated
  and a corresponding equivalent model is printed out.
  If no file is specified, the file specified with the environment variable
  <tt>output_flatten_model_file</tt> is used if any, otherwise standard output
  is used as output.
  <p>
  Command options:<p>
  <dl>
     <dt><tt>-o filename</tt>
       <dd> Attempts to write the flat SMV model in <tt>filename</tt>.
  </dl>

*/

int CommandWriteModelFlatUdg(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  FILE* outstream = StreamMgr_get_output_stream(streams);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int c = 0;
  int rv = 0;
  char* output_file = NIL(char);
  FILE* ofileid = NIL(FILE);
  int bSpecifiedFilename = FALSE;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "ho:")) != EOF) {
    switch (c) {
    case 'h': goto __write_model_flat_udg_fail_help;
    case 'o':
      output_file = ALLOC(char, strlen(util_optarg)+1);
      nusmv_assert(output_file);
      strcpy(output_file, util_optarg);
      bSpecifiedFilename = TRUE;
      break;

    default:
      goto __write_model_flat_udg_fail_help;

    }
  }

  if (argc != util_optind) goto __write_model_flat_udg_fail_help;

  if (output_file == NIL(char)) {
    output_file = get_output_flatten_model_file(opts);
  }
  if (output_file == NIL(char)) {
    ofileid = outstream;
  }
  else {
    ofileid = fopen(output_file, "w");
    if (ofileid == (FILE*)NULL) {
      StreamMgr_print_error(streams,  "Unable to open file \"%s\".\n", output_file);
      goto __write_model_flat_udg_fail;
    }
  }

  /* pre-conditions: */
  if (Compile_check_if_flattening_was_built(env, errstream)) {
    goto __write_model_flat_udg_fail;
  }

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Writing flat model udg into file \"%s\"..",
      output_file == (char *)NULL ? "stdout" : output_file);
  }

  CATCH(errmgr) {

    SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
    FlatHierarchy_ptr hierarchy = FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));

    Compile_WriteFlattenModel_udg(env, ofileid, st,
                                  SymbTable_get_class_layer_names(st, (const char*) NULL),
                                  "MODULE main", hierarchy);

    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, ".. done.\n");
    }
  }
  FAIL(errmgr) {
    rv = 1;
  }
  fflush(ofileid);

  clean_memory_before_return(ofileid, bSpecifiedFilename,
                             output_file, outstream);
  return rv;

  /* failure handlers */
 __write_model_flat_udg_fail_help:
  clean_memory_before_return(ofileid, bSpecifiedFilename,
                             output_file, outstream);
  return UsageWriteModelFlatUdg(env);

 __write_model_flat_udg_fail:
  clean_memory_before_return(ofileid, bSpecifiedFilename,
                             output_file, outstream);
  return 1;

}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageWriteModelFlatUdg(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: write_flat_model_udg [-h] [-o filename]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -o filename\tWrites output to \"filename\"\n");
  return 1;
}



/*!
  \command{write_boolean_model} Writes a flattened and booleanized model of a
  given SMV file

  \command_args{[-h] [-o filename]}

  Writes the currently loaded SMV model in the
  specified file, after having flattened and booleanized it. Processes
  are eliminated and a corresponding equivalent model is printed
  out.

  If no file is specified, the file specified via the environment
  variable <tt>output_boolean_model_file</tt> is used if any,
  otherwise standard output is used.
  <p>
  Command options:<p>
  <dl>
     <dt><tt>-o filename</tt>
       <dd> Attempts to write the flat and boolean SMV model in
       <tt>filename</tt>.
  </dl>

  ** New in 2.4.0 and later **
  Scalar variables are dumped as DEFINEs whose body is their boolean
  encoding.

  This allows the user to still express and see parts of the generated
  boolean model in terms of the original model's scalar variables
  names and values, and still keeping the generated model purely boolean.

  Also, symbolic constants are dumped within a CONSTANTS statement to
  declare the values of the original scalar variables' for future
  reading of the generated file.
*/

int CommandWriteModelFlatBool(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  FILE* outstream = StreamMgr_get_output_stream(streams);
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int c = 0;
  int rv = 0;
  char* output_file = NIL(char);
  FILE* ofileid = NIL(FILE);
  int bSpecifiedFilename = FALSE;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "ho:")) != EOF) {
    switch (c) {
    case 'h':
      goto  __write_model_flat_bool_fail_help;

    case 'o':
      output_file = ALLOC(char, strlen(util_optarg)+1);
      nusmv_assert(output_file);
      strcpy(output_file, util_optarg);
      bSpecifiedFilename = TRUE;
      break;

    default:
      goto  __write_model_flat_bool_fail_help;

    }
  }

  if (argc != util_optind) goto  __write_model_flat_bool_fail_help;

  if (output_file == NIL(char)) {
    output_file = get_output_boolean_model_file(opts);
  }

  if (output_file == NIL(char)) {
    ofileid = outstream;
  }
  else {
    ofileid = fopen(output_file, "w");
    if (ofileid == NULL) {
      StreamMgr_print_error(streams,  "Unable to open file \"%s\".\n", output_file);
      goto  __write_model_flat_bool_fail;
    }
  }

  if (Compile_check_if_bool_model_was_built(env, errstream, true)) {
    goto  __write_model_flat_bool_fail;
  }

  rv = Compile_write_model_flat_bool(env, output_file, ofileid);

  clean_memory_before_return(ofileid, bSpecifiedFilename,
                             output_file, outstream);
  return rv;

  /* failure handlers */
 __write_model_flat_bool_fail_help:
  clean_memory_before_return(ofileid, bSpecifiedFilename,
                             output_file, outstream);
  return UsageWriteModelFlatBool(env);

 __write_model_flat_bool_fail:
  clean_memory_before_return(ofileid, bSpecifiedFilename,
                             output_file, outstream);
  return 1;

}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageWriteModelFlatBool(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: write_boolean_model [-h] [-o filename]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -o filename\tWrites output to \"filename\".\n");
  return 1;
}


/*!
  \command{write_coi_model} Writes a flat model of SMV file, restricted to the COI
  of the model properties

  \command_args{[-h] [-o filename] [-n <prop> | -p <expr>
                      | -P <name>] | [-c] | [-l] | [-i] | [-s] |
                      [-q] | [-p expr] | [-C] | [-g]}

  Writes the currently loaded SMV model in the
  specified file, after having flattened it. If a property is
  specified, the dumped model is the result of applying the COI over
  that property. otherwise, a restricted SMV model is dumped for each
  property in the property database.

  Processes are eliminated and a corresponding equivalent model is
  printed out.

  If no file is specified, stderr is used for output
  <p>
  Command options:<p>
  <dl>
     <dt><tt>-o filename</tt>
       <dd> Attempts to write the flat and boolean SMV model in
       <tt>filename</tt>.
    <dt> <tt>-c</tt>
       <dd> Dumps COI model for all CTL properties
    <dt> <tt>-l</tt>
       <dd> Dumps COI model for all LTL properties
    <dt> <tt>-i</tt>
       <dd> Dumps COI model for all INVAR properties
    <dt> <tt>-s</tt>
       <dd> Dumps COI model for all PSL properties
    <dt> <tt>-q</tt>
       <dd> Dumps COI model for all COMPUTE properties
     <dt><tt>-p expr</tt>
       <dd> Applies COI for the given expression "expr"
     <dt><tt>-n idx</tt>
       <dd> Applies COI for property stored at index "idx"
     <dt><tt>-P name</tt>
       <dd> Applies COI for property named "name" idx
     <dt><tt>-C</tt>
       <dd> Only prints the list of variables that are in
           the COI of properties
     <dt><tt>-g</tt>
       <dd> Dumps the COI model that represents the union of all COI properties
  </dl>


*/

int CommandWriteCoiModel(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  int res = 1;
  int c;
  char* formula = NIL(char);
  char* formula_name = NIL(char);
  char* file_name = NIL(char);
  OStream_ptr output_file = StreamMgr_get_output_ostream(streams);
  boolean print_coi = false;
  boolean global_coi_model = false;
  int prop_no = -1;

  PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  Prop_Type prop_type = Prop_NoType;
  SymbTable_ptr st;
  FlatHierarchy_ptr hierarchy;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "ho:p:P:n:qclisCg")) != EOF) {
    switch (c) {
    case 'h':
      goto write_coi_usage;

    case 'g':
      if (global_coi_model || print_coi) goto write_coi_usage;
      global_coi_model = true;
      break;

    case 'C':
      if (global_coi_model || print_coi) goto write_coi_usage;
      print_coi = true;
      break;

    case 'q':
      if ((NIL(char) != formula_name) || (prop_no != -1) ||
          (prop_type != Prop_NoType)) {
        goto write_coi_usage;
      }
      prop_type = Prop_Compute;
      break;
    case 'c':
      if ((NIL(char) != formula_name) || (prop_no != -1) ||
          (prop_type != Prop_NoType)) {
        goto write_coi_usage;
      }
      prop_type = Prop_Ctl;
      break;
    case 'l':
      if ((NIL(char) != formula_name) || (prop_no != -1) ||
          (prop_type != Prop_NoType)) {
        goto write_coi_usage;
      }
      prop_type = Prop_Ltl;
      break;
    case 'i':
      if ((NIL(char) != formula_name) || (prop_no != -1) ||
          (prop_type != Prop_NoType)) {
        goto write_coi_usage;
      }
      prop_type = Prop_Invar;
      break;
    case 's':
      if ((NIL(char) != formula_name) || (prop_no != -1) ||
          (prop_type != Prop_NoType)) {
        goto write_coi_usage;
      }
      prop_type = Prop_Psl;
      break;

    case 'n':
      if ((NIL(char) != formula) ||
          (NIL(char) != formula_name) || (prop_no != -1) ||
          (prop_type != Prop_NoType)) {
        goto write_coi_usage;
      }

      prop_no = PropDb_get_prop_index_from_string(prop_db, util_optarg);
      if (prop_no == -1) { goto write_coi_free; }
      break;
    case 'P':
      if ((NIL(char) != formula) ||
          (NIL(char) != formula_name) || (prop_no != -1) ||
          (prop_type != Prop_NoType)) {
        goto write_coi_usage;
      }
      formula_name = util_strsav(util_optarg);

      prop_no = PropDb_prop_parse_name(prop_db, formula_name);

      if (prop_no == -1) {
        StreamMgr_print_error(streams,  "No property named \"%s\"\n", formula_name);
        goto write_coi_free;
      }
      break;
    case 'p':
      if ((NIL(char) != formula) ||
          (NIL(char) != formula_name) || (prop_no != -1)) {
        goto write_coi_usage;
      }
      formula = util_strsav(util_optarg);
      break;
    case 'o':
      if (NIL(char) != file_name) { goto write_coi_usage; }
      file_name = util_strsav(util_optarg);
      break;
    default:
      goto write_coi_usage;
    }
  }

  if (argc != util_optind) { goto write_coi_usage; }

  /* pre-conditions: */
  if (Compile_check_if_flattening_was_built(env, errstream)) {
    goto write_coi_free;
  }

  st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
  hierarchy = FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));

  if (NIL(char) != formula) {
    if (Prop_NoType == prop_type) {
      StreamMgr_print_error(streams,  "No property type specified. Use one of the "
              "-l, -i, -s, -c or -q options\n");
      goto write_coi_usage;
    }
    prop_no = PropDb_prop_parse_and_add(prop_db, st, formula, prop_type, Nil);
    if (prop_no == -1) { goto write_coi_free; }
  }

  {
    /* Write COI model only for the requested property */
    if (prop_no != -1) {
      Prop_ptr prop = PropDb_get_prop_at_index(prop_db, prop_no);
      Set_t cone = Prop_compute_cone(prop, hierarchy, st);
      Set_t props = Set_MakeSingleton((Set_Element_t)prop);

      if (NIL(char) != file_name) {
        output_file = OStream_create_file(file_name, false);
        if (OSTREAM(NULL) == output_file) {
          StreamMgr_print_error(streams,  "Cannot open file '%s' for writing\n", file_name);
          goto write_coi_free;
        }
      }

      if (print_coi) {
        Compile_write_coi_prop(env, cone, props, output_file);
      }
      else {
        Compile_write_coi_prop_fsm(env, hierarchy, cone, props, output_file);
      }
      OStream_flush(output_file);
      if (NIL(char) != file_name) {
        OStream_destroy(output_file);
      }

      Set_ReleaseSet(props);
      Set_ReleaseSet(cone);
    }
    /* Write COI model for all property in the DB */
    else {
      /* User requested the global COI model: this is the result of
         all properties COI union */
      if (global_coi_model) {
        if (NIL(char) != file_name) {
          output_file = OStream_create_file(file_name, false);
          if (OSTREAM(NULL) == output_file) {
            StreamMgr_print_error(streams,  "Cannot open file '%s' for writing\n", file_name);
            goto write_coi_free;
          }
        }

        Compile_write_global_coi_fsm(env, hierarchy, prop_type, output_file);

        OStream_flush(output_file);
        if (NIL(char) != file_name) {
          OStream_destroy(output_file);
        }
      }
      else {
        /* Dump shared COI informations */
        res = Compile_write_properties_coi(env, hierarchy, prop_type,
                                               print_coi, file_name);
        if (res != 0) goto write_coi_free;
      }
    }
    /* Everything went OK */
    res = 0;
    goto write_coi_free;
  }
 write_coi_usage:
  res = UsageWriteCoiModel(env);
 write_coi_free:
  if (NIL(char) != formula) { FREE(formula); }
  if (NIL(char) != formula_name) { FREE(formula_name); }
  if (NIL(char) != file_name) { FREE(file_name); }
  return res;
}

/*!
  \brief Prints the usage for the write_coi_command


*/
static int UsageWriteCoiModel(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: write_coi_model [-h] [-o filename]"
          " [-n \"index\" | -p \"expr\"| -P \"name\"] [-c | -l | -i | -s | -q] [-C] [-g]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -n idx\tWrites COI model for property with index \"idx\"\n");
  StreamMgr_print_error(streams,  "  -p expr\tWrites COI model for the given property. The property type has to be specified\n");
  StreamMgr_print_error(streams,  "  -P name\tWrites COI model for property named \"name\"\n");
  StreamMgr_print_error(streams,  "  -C \t\tOnly print the list of variables in the COI\n");
  StreamMgr_print_error(streams,  "  -g \t\tGroups properties COI and generates one model\n");
  StreamMgr_print_error(streams,  "  -i \t\tWrite COI model only for INVAR properties\n");
  StreamMgr_print_error(streams,  "  -l \t\tWrite COI model only for LTL properties\n");
  StreamMgr_print_error(streams,  "  -s \t\tWrite COI model only for PSL properties\n");
  StreamMgr_print_error(streams,  "  -q \t\tWrite COI model only for COMPUTE properties\n");
  StreamMgr_print_error(streams,  "  -c \t\tWrite COI model only for CTL properties\n");
  StreamMgr_print_error(streams,  "  -o filename\tWrites output to \"filename\".\n");
  return 1;
}


/*!
  \command{show_dependencies} Shows the expression dependencies

  \command_args{[-h] [-k bound] -e expr}


  Shows the dependencies of the given expression
  <p>
  Command options:<p>
  <dl>
     <dt><tt>-k bound</tt>
       <dd> Stop dependencies computation at step "bound"
     <dt><tt>-e expr</tt>
       <dd> The expression on which the dependencies are computed on
  </dl>

*/

int CommandShowDependencies (NuSMVEnv_ptr env, int argc, char** argv)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  int c;
  int res = 0;
  int fixpoint = -1;
  char* next_formula_str = NIL(char);
  util_getopt_reset();

  while ((c = util_getopt(argc, argv, "hk:e:")) != EOF) {
    switch (c) {
    case 'h':
      goto show_deps_usage;
    case 'k':
      {
        int res;
        if (fixpoint != -1) goto show_deps_usage;

        res = util_str2int(util_optarg, &fixpoint);

        if (res != 0 || (fixpoint < 0)) {
          StreamMgr_print_error(streams,
                  "Error: '%s' is not a valid fixpoint\n",  util_optarg);
          goto show_deps_usage;
        }
        break;
      }
    case 'e':
      {
        if (NIL(char) != next_formula_str) goto show_deps_usage;
        next_formula_str = util_strsav(util_optarg);
        break;
      }
    default:
      goto show_deps_usage;
    }
  }
  /* More arguments than necessary */
  if (argc != util_optind) goto show_deps_usage;

  /* No expression specified */
  if (NIL(char) == next_formula_str) goto show_deps_usage;

  {
    node_ptr expression;
    Set_t dependencies;
    FlatHierarchy_ptr hierarchy = FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));
    SymbTable_ptr symb_table = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
    TypeChecker_ptr type_checker = SymbTable_get_type_checker(symb_table);
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    int parse_result = Parser_ReadNextExprFromString(env, next_formula_str, &expression);

    if (parse_result != 0 || Nil == expression) {
      StreamMgr_print_error(streams,  "Parsing error: expected an next expression.\n");
      goto show_deps_usage;
    }

    expression = car(expression);

    if (!TypeChecker_is_expression_wellformed(type_checker, expression, Nil)) {
      goto show_deps_usage;
    }

    dependencies = ComputeCOIFixpoint(symb_table, hierarchy,
                                      expression, fixpoint, (boolean*)NULL);

    Set_PrintSet(wffprint, errstream, dependencies, NULL, NULL);
    StreamMgr_print_error(streams,  "\n");

    Set_ReleaseSet(dependencies);
  }

  goto show_deps_free;

 show_deps_usage:
  res = UsageShowDependencies(env);
 show_deps_free:
  if (NIL(char) != next_formula_str) {
    FREE(next_formula_str);
  }
  return res;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageShowDependencies(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "show_dependencies [-h] [-k bound] -e <next_expr>\n");
  StreamMgr_print_error(streams,  "\t-k bound\tStop searching dependencies at step \"bound\"\n");
  StreamMgr_print_error(streams,  "\t-e expr\tThe expression on which dependencies are computed on\n");
  return 1;
}

/*!
  \brief Auxiliary function for CommandWriteModelFlat and
                      CommandWriteModelFlatUdg

  This avoid code duplication
*/
static inline void clean_memory_before_return(FILE* ofileid,
                                              boolean bSpecifiedFilename,
                                              char* output_file,
                                              FILE* outstream) {
  if (ofileid != outstream) {
    if ((FILE*)NULL != ofileid) fclose(ofileid);
    if (bSpecifiedFilename) FREE(output_file);
  }
}
