/* ---------------------------------------------------------------------------


  This file is part of the ``parser'' package.
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
  \brief The parser main routine

  \todo: Missing description

*/

#include "nusmv/core/parser/parserInt.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
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

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

int Parser_read_model(NuSMVEnv_ptr env,
                      char* ifile)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  if (NULL != ifile) {
    set_input_file(opts, ifile);
  }

  /* Parse the input file */
  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Parsing file \"%s\" ..... ",
            get_input_file(opts));
  }

  if (Parser_ReadSMVFromFile(env, get_input_file(opts))) {
    ErrorMgr_nusmv_exit(errmgr, 1);
  }

  { /* dumps erros if there are any */
    node_ptr errors = Parser_get_syntax_errors_list(env);
    if (Nil != errors) {
      StreamMgr_print_error(streams,  "\n");
      fflush(NULL); /* to flush all existing messages before outputting */

      while (Nil != errors) {
        Parser_print_syntax_error(car(errors), errstream);
        errors = cdr(errors);
      }
    }
  }

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "done.\n");
    fflush(errstream);
  }

  cmp_struct_set_read_model(cmps);

  return 0;
}

int Parser_skip_multiline_comment(int (*read_function)(void))
{
  register int c;
  int comment[3] = { 0, 0, 0 };
  int comment_value[3] = {'/', '-', '-'};
  register int k;

  do {
    c = read_function();

    for (k=(sizeof(comment)/sizeof(comment[0])-1); k>0; --k) {
      comment[k] = comment[k-1];
    }
    comment[0] = c;
     /* On Mac OS X, input return 0 instead of EOF, see
        https://gitlab.fbk.eu/es-tools-dev/ESTools/issues/66 */
#if __APPLE__
  } while ( memcmp(comment, comment_value, sizeof(comment)) != 0
            && c != EOF && c != 0);
#else
  } while ( memcmp(comment, comment_value, sizeof(comment)) != 0 && c != EOF );
#endif

#if __APPLE__
  if (EOF == c || 0 == c) {
#else
  if (EOF == c) {
#endif
    printf("\n Warning: There is no terminator for multiline comment!\n");
  }

  return 0;
}

int Parser_skip_one_line_comment(int (*read_function)(void))
{
   register int c;

   do {
     c = read_function();
     /* On Mac OS X, input return 0 instead of EOF, see
        https://gitlab.fbk.eu/es-tools-dev/ESTools/issues/66 */
#if __APPLE__
   } while ( c != '\n' && c != EOF && c != 0);
#else
   } while ( c != '\n' && c != EOF );
#endif
   return(0);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
