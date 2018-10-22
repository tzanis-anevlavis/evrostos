/* ---------------------------------------------------------------------------


  This file is part of the ``hrc'' package of NuSMV version 2.
  Copyright (C) 2009 by FBK.

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
  \author Sergio Mover
  \brief Shell interface to the hrc package.

  This file contains the interface of the compile package
  with the interactive shell.

*/

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/shell/cmd/cmd.h"
#include "nusmv/shell/hrc/hrcCmd.h"

#include "nusmv/core/hrc/hrc.h"

#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/parser.h"
#include "nusmv/core/parser/ord/ParserOrd.h"

#include "nusmv/core/compile/compile.h"



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
extern cmp_struct_ptr cmps;

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static int UsageHrcWriteModel(const NuSMVEnv_ptr env);
static int UsageHrcDumpModel(const NuSMVEnv_ptr env);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Hrc_init_cmd(NuSMVEnv_ptr env)
{
  Cmd_CommandAdd(env, "hrc_write_model", CommandHrcWriteModel, 0, false);
  Cmd_CommandAdd(env, "hrc_dump_model", CommandHrcDumpModel, 0, false);
}

void Hrc_quit_cmd(NuSMVEnv_ptr env)
{
  Cmd_CommandRemove(env, "hrc_write_model");
  Cmd_CommandRemove(env, "hrc_dump_model");
}

int CommandHrcWriteModel(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  int c = 0;
  int rv = 0;
  char* output_file = NIL(char);
  FILE* ofileid = NIL(FILE);
  boolean bSpecifiedFilename = false;
  boolean append_suffix = false;
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "ho:d")) != EOF) {
    switch (c) {
    case 'h': return UsageHrcWriteModel(env);
    case 'o':
      output_file = ALLOC(char, strlen(util_optarg)+1);
      nusmv_assert(output_file);
      strcpy(output_file, util_optarg);
      bSpecifiedFilename = true;
      break;
    case 'd':
      append_suffix = true;
      break;
    default:
      break;
    }
  }

  StreamMgr_print_error(streams, "Warning: %s has been deprecated in favor of %s\n",
                        argv[0], "hrc_dump_model -f smv");

  if (argc != util_optind) return UsageHrcWriteModel(env);

  if (!cmp_struct_get_hrc_built(cmps)) {
    StreamMgr_print_output(streams,
            "The HRC structure was not built, use command flatten_hierarchy\n");
    return 1;
  }

  if (!NuSMVEnv_has_value(env, ENV_HRC_HIERARCHY)) {
    StreamMgr_print_output(streams,
            "The HRC structure is not available, cannot dump the model\n");
    return 1;
  }

  if (output_file == NIL(char)) {
    ofileid = outstream;
  }
  else {
    ofileid = fopen(output_file, "w");
    if ((FILE *)NULL == ofileid) {
      StreamMgr_print_error(streams,  "Unable to open file \"%s\".\n", output_file);
      if (bSpecifiedFilename == true)  FREE(output_file);
      return 1;
    }
  }

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Writing hrc model into file \"%s\"..",
      output_file == (char *)NULL ? "stdout" : output_file);
  }

  CATCH(errmgr) {
    rv = Hrc_dump_model(env, HRC_DUMP_FORMAT_SMV, ofileid, append_suffix, true);

    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, ".. done.\n");
    }
  }
  FAIL(errmgr) {
    rv = 1;
  }

  if (bSpecifiedFilename) FREE(output_file);

  return rv;
}

int CommandHrcDumpModel(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  int c = 0;
  int rv = 0;
  char* dump_format = (char*) NULL;
  HrcDumpFormat dump_format_enum = 0;
  char* output_fname = (char*) NULL;
  FILE* ofileid = NIL(FILE);
  boolean append_suffix = false;
  boolean use_indent = true;
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "ho:f:di")) != EOF) {
    switch (c) {
    case 'h': goto CommandHrcDumpModel_exit_usage;
    case 'o':
      if ((char*) NULL != output_fname) { FREE(output_fname); }
      output_fname = util_strsav(util_optarg);
      nusmv_assert((char*) NULL != output_fname);
      break;

    case 'f':
      if ((char*) NULL != dump_format) { FREE(dump_format); }
      dump_format = util_strsav(util_optarg);
      nusmv_assert((char*) NULL != dump_format);
      break;

    case 'd':  append_suffix = true; break;

    case 'i':  use_indent = false; break;

    default: goto CommandHrcDumpModel_exit_usage;
    }
  }

  if (argc != util_optind) goto CommandHrcDumpModel_exit_usage;

  if ( (char*) NULL == dump_format) {
    StreamMgr_print_output(streams, "Dump format not specified!\n");
    goto CommandHrcDumpModel_exit_usage;
  }


  if (!cmp_struct_get_hrc_built(cmps)) {
    StreamMgr_print_output(streams,
            "The HRC structure was not built, use command flatten_hierarchy\n");
    return 1;
  }

  if (!NuSMVEnv_has_value(env, ENV_HRC_HIERARCHY)) {
    StreamMgr_print_output(streams,
            "The HRC structure is not available, cannot dump the model\n");
    rv = 1; goto CommandHrcDumpModel_exit;
  }

  if (output_fname == NIL(char)) {
    ofileid = outstream;
  }
  else {
    ofileid = fopen(output_fname, "w");
    if ((FILE *) NULL == ofileid) {
      StreamMgr_print_error(streams,  "Unable to open file \"%s\".\n", output_fname);
      rv = 1; goto CommandHrcDumpModel_exit;
    }
  }

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Dumping hrc model in format '%s' into file '%s'...",
               dump_format,
               (char*) NULL == output_fname ? "stdout" : output_fname);
  }

  dump_format_enum = Hrc_dump_format_str_to_enum(dump_format);
  if (HRC_DUMP_FORMAT_INVALID == dump_format_enum) {
    StreamMgr_print_error(streams,  "Invalid format '%s'\n", dump_format);

    StreamMgr_print_error(streams,  "Valid formats are: %s\n",
                          Hrc_dump_format_get_available());

    rv = 1; return rv;
  }


  Hrc_dump_model(env, dump_format_enum, ofileid, append_suffix, use_indent);

 CommandHrcDumpModel_exit:
  if ((char*) NULL != output_fname) {
    FREE(output_fname);
  }

  if ((char*) NULL != dump_format) { FREE(dump_format); }

  return rv;

 CommandHrcDumpModel_exit_usage:
  if ((char*) NULL != dump_format) { FREE(dump_format); }
  if ((char*) NULL != output_fname) { FREE(output_fname); }
  return UsageHrcDumpModel(env);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Prints the usage of the command UsageHrcWriteModel


*/
static int UsageHrcWriteModel(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: hrc_write_model [-h] [-o filename] [-d]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -o filename\tWrites output to \"filename\"\n");
  StreamMgr_print_error(streams,  "  -d Renames every module name appending the " \
          "suffix \"_hrc\"\n");
  return 1;
}

/*!
  \brief Prints the usage of the command UsageHrcDumpModel


*/
static int UsageHrcDumpModel(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: hrc_dump_model [-h]  -f format [-o filename] [-d][-i]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -f format\tDumps in the given format (debug, smv or xml)\n");
  StreamMgr_print_error(streams,  "  -o filename\tDumps output to \"filename\"\n");
  StreamMgr_print_error(streams,  "  -d \t\tRenames every module name appending the " \
          "suffix \"_hrc\"\n");
  StreamMgr_print_error(streams,  "  -i Disable indentation.\n");
  return 1;
}
