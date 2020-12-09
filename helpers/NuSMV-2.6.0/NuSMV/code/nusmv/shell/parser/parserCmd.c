/* ---------------------------------------------------------------------------


  This file is part of the ``parser'' package of NuSMV version 2.
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
  \brief Interface of the parser package with the shell.

  Provides command for reading the NuSMV input file and
  build an internal representation of it.

*/


#include "nusmv/shell/cmd/cmd.h"
#include "nusmv/shell/parser/parserCmd.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/parser/parser.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
extern cmp_struct_ptr cmps;

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static int UsageReadModel(const NuSMVEnv_ptr env);
static int CommandReadModel(NuSMVEnv_ptr env, int argc, char** argv);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/
/* WARNING [MD] quit function missing */
void Parser_Cmd_init(NuSMVEnv_ptr const env)
{
  Cmd_CommandAdd(env, "read_model", CommandReadModel, 0, true);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \command{read_model} Reads a NuSMV file into NuSMV.

  \command_args{[-h] [-i model-file]}

  Reads a NuSMV file. If the <tt>-i</tt> option is
  not specified, it reads from the file specified in the environment
  variable <tt>input_file</tt>.<p>
  Command options:<p>
  <dl>
    <dt> <tt>-i model-file</tt>
       <dd> Sets the environment variable <tt>input_file</tt> to
           <tt>model-file</tt>, and reads the model from the specified file.
  </dl>
*/

static int CommandReadModel(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int c;
  char* i_file = (char*)NULL;
  int res = 1;

  util_getopt_reset();
  while((c = util_getopt(argc,argv,"hi:")) != EOF){
    switch(c){
    case 'i': {
      /* -i already specified */
      if ((char*)NULL != i_file) { goto read_model_usage; }

      i_file = util_strsav(util_optarg);
      break;
    }
    case 'h': goto read_model_usage;
    default: goto read_model_usage;
    }
  }

  if (argc != util_optind) { goto read_model_usage; }

  if (cmp_struct_get_read_model(cmps)) {
    StreamMgr_print_error(streams,
            "A model appears to be already read from file: %s.\n",
            get_input_file(opts));
    goto read_model_free;
  }

  /* NULL input files are allowed in batch mode (that calls this
     command) when reading from stdin */
  if (NULL == i_file &&
      get_input_file(opts) == (char*)NULL &&
      !opt_batch(opts)) {
    StreamMgr_print_error(streams,
            "Input file is (null). You must set the input file before.\n");
    goto read_model_free;
  }

  res = Parser_read_model(env, i_file);

  goto read_model_free;

 read_model_usage:
  res = UsageReadModel(env);

 read_model_free:
  if ((char*)NULL != i_file) {
    FREE(i_file);
  }

  return res;
}

/*!
  \brief required

  optional

  \se required

  \sa optional
*/
static int UsageReadModel(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: read_model [-h] [-i <file>]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -i <file> \tReads the model from the specified <file>.\n");
  return(1);
}
