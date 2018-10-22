/* ---------------------------------------------------------------------------


  This file is part of the ``utils'' package of NuSMV version 2.
  Copyright (C) 2011 by FBK-irst.

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
  \brief Implementation of class 'Logger'

  The logging functions should take the desidered verbose_level as
  parameter and the class to have a copy of the OptsHandler. For now, I am
  duplicating the logging functions, and passing the opts as parameter too.
  See issue https://essvn.fbk.eu/bugs/view.php?id=3447

*/


#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include <stdarg.h>
#include <stdio.h>

#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/opt/opt.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

typedef struct Logger_TAG
{

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */

  OStream_ptr stream;

  /* -------------------------------------------------- */
  /*                  Virtual methods                   */
  /* -------------------------------------------------- */

} Logger;



/* ---------------------------------------------------------------------- */
/* Private methods to be used by derivated and friend classes only         */
/* ---------------------------------------------------------------------- */



/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'Logger_private.h' for class 'Logger' definition. */

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

static void logger_init(Logger_ptr self, FILE* stream);
static void logger_deinit(Logger_ptr self);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Logger_init(NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  Logger_ptr logger = Logger_create(errstream);

  NuSMVEnv_set_value(env, ENV_LOGGER, logger);
}

void Logger_quit(NuSMVEnv_ptr env)
{
  Logger_ptr logger = LOGGER(NuSMVEnv_remove_value(env, ENV_LOGGER));

  Logger_destroy(logger);
}

Logger_ptr Logger_create(FILE* stream)
{
  Logger_ptr self = ALLOC(Logger, 1);
  LOGGER_CHECK_INSTANCE(self);

  logger_init(self, stream);
  return self;
}

void Logger_destroy(Logger_ptr self)
{
  LOGGER_CHECK_INSTANCE(self);

  logger_deinit(self);

  FREE(self);
}

void Logger_log(const Logger_ptr self, const char* format, ...)
{
  va_list args;

  va_start(args, format);

  OStream_vprintf(self->stream, format, args);

  va_end(args);
}

void Logger_vlog(const Logger_ptr self,
                 OptsHandler_ptr opts,
                 const int verbose_level,
                 const char* format,
                 ...)
{
  if (opt_verbose_level_ge(opts, verbose_level)) {
    va_list args;

    va_start(args, format);

    OStream_vprintf(self->stream, format, args);

    va_end(args);
  }
}

void Logger_nlog(const Logger_ptr self,
                 const MasterPrinter_ptr node_printer,
                 const char* format, ...)
{
  va_list args;

  va_start(args, format);

  OStream_nvprintf(self->stream, node_printer, format, args);

  va_end(args);
}

void Logger_vnlog(const Logger_ptr self,
                  const MasterPrinter_ptr node_printer,
                  OptsHandler_ptr opts,
                  const int verbose_level,
                  const char* format,
                  ...)
{
  if (opt_verbose_level_ge(opts, verbose_level)) {
    va_list args;

    va_start(args, format);

    OStream_nvprintf(self->stream, node_printer, format, args);

    va_end(args);
  }
}

FILE* Logger_get_stream(const Logger_ptr self)
{
  return OStream_get_stream(self->stream);
}

OStream_ptr Logger_get_ostream(const Logger_ptr self)
{
  return self->stream;
}

void Logger_inc_indent_size(Logger_ptr self)
{
  OStream_inc_indent_size(self->stream);
}

void Logger_dec_indent_size(Logger_ptr self)
{
  OStream_dec_indent_size(self->stream);
}

int Logger_get_indent_size(const Logger_ptr self)
{
  return OStream_get_indent_size(self->stream);
}

void Logger_reset_indent_size(Logger_ptr self)
{
  OStream_reset_indent_size(self->stream);
}

void Logger_set_indent_size(Logger_ptr self, int n)
{
  OStream_set_indent_size(self->stream, n);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The Logger class private initializer

  The Logger class private initializer

  \sa Logger_create
*/
static void logger_init(Logger_ptr self, FILE* stream)
{
  /* members initialization */

  self->stream = OStream_create(stream);
}

/*!
  \brief The Logger class private deinitializer

  The Logger class private deinitializer

  \sa Logger_destroy
*/
static void logger_deinit(Logger_ptr self)
{
  /* members deinitialization */

  OStream_destroy(self->stream);
}



/**AutomaticEnd***************************************************************/
