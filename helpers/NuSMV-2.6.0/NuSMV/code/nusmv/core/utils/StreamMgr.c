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
  \brief Implementation of class 'StreamMgr'

  The stream manager class holds the error, output and
               input stream and provides some functionalities to print
               or read from those streams

*/


#include "nusmv/core/utils/OStream.h"
#include <stdarg.h>
#include <stdio.h>
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/utils_io.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct StreamMgr_TAG
{
  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */

  OStream_ptr err_stream;
  OStream_ptr out_stream;
  FILE* in_stream;

} StreamMgr;



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

static void stream_mgr_init(StreamMgr_ptr self);
static void stream_mgr_deinit(StreamMgr_ptr self);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Stream_init(NuSMVEnv_ptr env)
{
  StreamMgr_ptr mgr = StreamMgr_create();

  NuSMVEnv_set_value(env, ENV_STREAM_MANAGER, mgr);
}

void Stream_quit(NuSMVEnv_ptr env)
{
  StreamMgr_ptr mgr = NuSMVEnv_remove_value(env, ENV_STREAM_MANAGER);

  StreamMgr_destroy(mgr);
}

StreamMgr_ptr StreamMgr_create(void)
{
  StreamMgr_ptr self = ALLOC(StreamMgr, 1);
  STREAM_MGR_CHECK_INSTANCE(self);

  stream_mgr_init(self);
  return self;
}

void StreamMgr_destroy(StreamMgr_ptr self)
{
  STREAM_MGR_CHECK_INSTANCE(self);

  stream_mgr_deinit(self);
  FREE(self);
}

void StreamMgr_set_error_stream(StreamMgr_ptr self, FILE* err)
{
  OStream_set_stream(self->err_stream, err);
}

void StreamMgr_set_output_stream(StreamMgr_ptr self, FILE* out)
{
  OStream_set_stream(self->out_stream, out);
}

void StreamMgr_set_input_stream(StreamMgr_ptr self, FILE* in)
{
  self->in_stream = in;
}

FILE* StreamMgr_reset_output_stream(StreamMgr_ptr self)
{
  STREAM_MGR_CHECK_INSTANCE(self);

  return OStream_reset_stream(self->out_stream);
}

FILE* StreamMgr_reset_error_stream(StreamMgr_ptr self)
{
  STREAM_MGR_CHECK_INSTANCE(self);

  return OStream_reset_stream(self->err_stream);
}

FILE* StreamMgr_get_error_stream(const StreamMgr_ptr self)
{
  return OStream_get_stream(self->err_stream);
}

FILE* StreamMgr_get_output_stream(const StreamMgr_ptr self)
{
  return OStream_get_stream(self->out_stream);
}

FILE* StreamMgr_get_input_stream(const StreamMgr_ptr self)
{
  return self->in_stream;
}

OStream_ptr StreamMgr_get_output_ostream(const StreamMgr_ptr self)
{
  return self->out_stream;
}

OStream_ptr StreamMgr_get_error_ostream(const StreamMgr_ptr self)
{
  return self->err_stream;
}

void StreamMgr_print_output(const StreamMgr_ptr self,
                            const char* format, ...)
{
  va_list args;
  va_start(args, format);

  OStream_vprintf(self->out_stream, format, args);

  va_end(args);
}

void StreamMgr_nprint_output(const StreamMgr_ptr self,
                             const MasterPrinter_ptr printer,
                             const char* format, ...)
{
  va_list args;
  va_start(args, format);

  OStream_nvprintf(self->out_stream, printer, format, args);

  va_end(args);
}

void StreamMgr_print_error(const StreamMgr_ptr self,
                           const char* format, ...)
{
  va_list args;
  va_start(args, format);

  OStream_vprintf(self->err_stream, format, args);

  va_end(args);
}

void StreamMgr_nprint_error(const StreamMgr_ptr self,
                            const MasterPrinter_ptr printer,
                            const char* format, ...)
{
  va_list args;
  va_start(args, format);

  OStream_nvprintf(self->err_stream, printer, format, args);

  va_end(args);
}

void StreamMgr_flush_streams(const StreamMgr_ptr self)
{
  OStream_flush(self->err_stream);
  OStream_flush(self->out_stream);
}

void StreamMgr_inc_indent_size(StreamMgr_ptr self)
{
  OStream_inc_indent_size(self->out_stream);
  OStream_inc_indent_size(self->err_stream);
}

void StreamMgr_dec_indent_size(StreamMgr_ptr self)
{
  OStream_dec_indent_size(self->out_stream);
  OStream_dec_indent_size(self->err_stream);
}

int StreamMgr_get_indent_size(const StreamMgr_ptr self)
{
  return OStream_get_indent_size(self->out_stream);
}

void StreamMgr_reset_indent_size(StreamMgr_ptr self)
{
  OStream_reset_indent_size(self->out_stream);
  OStream_reset_indent_size(self->err_stream);
}

void StreamMgr_set_indent_size(StreamMgr_ptr self, int n)
{
  OStream_set_indent_size(self->out_stream, n);
  OStream_set_indent_size(self->err_stream, n);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The StreamMgr class private initializer

  The StreamMgr class private initializer

  \sa StreamMgr_create
*/
static void stream_mgr_init(StreamMgr_ptr self)
{
  /* members initialization */
  self->err_stream = OStream_create(stderr);
  self->out_stream = OStream_create(stdout);
  self->in_stream = stdin;
}

/*!
  \brief The StreamMgr class private deinitializer

  The StreamMgr class private deinitializer

  \sa StreamMgr_destroy
*/
static void stream_mgr_deinit(StreamMgr_ptr self)
{
  /* members deinitialization */

  OStream_destroy(self->err_stream);
  OStream_destroy(self->out_stream);

  if (self->in_stream != stdin) {
    fclose(self->in_stream);
  }
}



/**AutomaticEnd***************************************************************/

