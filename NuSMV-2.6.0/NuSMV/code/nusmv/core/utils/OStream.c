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
  \brief Implementation of class 'OStream'

  \todo: Missing description

*/


#include "nusmv/core/utils/OStream.h"
#include <stdarg.h>
#include <stdio.h>
#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/utils/error.h"


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BUF_SIZE 4096

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct OStream_TAG
{
  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  FILE* stream;
  int indent_size;
  boolean split_nl;

  boolean next_needs_indentation;
} OStream;



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

static void ostream_init(OStream_ptr self, FILE* stream);
static void ostream_deinit(OStream_ptr self, boolean close_stream);

static inline void ostream_indent(OStream_ptr self)
{
  int k;

  for (k = 0; k < self->indent_size; ++k) {
    fprintf(self->stream, "  ");
  }
}

/*!
  \brief Prints the given string, indenting each line

  Prints the given string, indenting each line
*/

static inline void print_splitted_node(OStream_ptr self,
                                       MasterPrinter_ptr printer,
                                       const char* format,
                                       va_list args)
{

  if ((FILE*)NULL != self->stream) {
    if (self->split_nl) {
      FILE* output = self->stream;
      size_t i = 0;
      char buf[32];

      nusmv_assert((const char*)NULL != format);

      while (format[i] != '\0') {

        /* First of all, indent if needed */
        if (self->next_needs_indentation) {
          ostream_indent(self);
          self->next_needs_indentation = false;
        }

        if (format[i] == '%') {
          if (format[i + 1] == '%') {
            fprintf(output, "%%");
            i += 2;
          }
          else if (format[i + 1] == 'N') {
            node_ptr node = va_arg(args, node_ptr);
            char* str;

            MASTER_PRINTER_CHECK_INSTANCE(printer);

            str = sprint_node(printer, node);
            fprintf(output, "%s", str);
            FREE(str);
            i += 2;
          }
          else {
            enum var_type type;
            enum var_modifier mod;
            int read;

            read = UtilsIO_get_param_len(format + i, &type, &mod);
            nusmv_assert(read < sizeof(buf)/sizeof(buf[0]));
            strncpy(buf, format + i, read);
            buf[read] = '\0'; /* terminator */

            switch (type) {

            case VAR_INT:
              switch(mod) {
              case MOD_NONE:
              case MOD_SHORT:
              case MOD_SHORT_SHORT: fprintf(output, buf, va_arg(args, int)); break;
              case MOD_SIZE_T: fprintf(output, buf, va_arg(args, size_t)); break;
              case MOD_LONG: fprintf(output, buf, va_arg(args, long int)); break;
              case MOD_LONG_LONG: fprintf(output, buf, va_arg(args, long long int)); break;

                /* The rest is an error (or unhandled feature) */
              default: error_unreachable_code();
              }
              break; /* case VAR_INT */

            case VAR_CHAR:
              switch(mod) {
              case MOD_NONE: fprintf(output, buf, va_arg(args, int)); break;

                /* The rest is an error (or unhandled feature) */
              default: error_unreachable_code();
              }
              break; /* case VAR_CHAR */

            case VAR_STRING:
              switch(mod) {
              case MOD_NONE: fprintf(output, buf, va_arg(args, char*)); break;

                /* The rest is an error (or unhandled feature) */
              default: error_unreachable_code();
              }
              break;  /* case VAR_STRING */

            case VAR_POINTER:
              switch(mod) {
              case MOD_NONE: fprintf(output, buf, va_arg(args, void*)); break;

                /* The rest is an error (or unhandled feature) */
              default: error_unreachable_code();
              }
              break;  /* case VAR_POINTER */

            case VAR_DOUBLE:
              switch(mod) {
              case MOD_NONE: fprintf(output, buf, va_arg(args, double)); break;
              case MOD_LONG_DOUBLE: fprintf(output, buf, va_arg(args, long double)); break;

                /* The rest is an error (or unhandled feature) */
              default: error_unreachable_code();
              }
              break; /* case VAR_DOUBLE */

              /* Errors */
            default: error_unreachable_code();
            }

            i += read;
          }
        } /* if (format[i] == '%') */
        else if (format[i] == '\n') {
          fprintf(output, "\n");
          self->next_needs_indentation = true;
          ++i;
        }
        else {
          fprintf(output, "%c", format[i]);
          ++i;
        }
      }
    }
    else {
      if (self->next_needs_indentation) {
        ostream_indent(self);
      }

      if (MASTER_PRINTER(NULL) == printer) {
        vfprintf(self->stream, format, args);
      }
      else {
        UtilsIO_node_vfprintf(printer, self->stream, format, args);
      }

      self->next_needs_indentation = (format[strlen(format) - 1] == '\n');
    }
  }
}

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

OStream_ptr OStream_create(FILE* stream)
{
  OStream_ptr self = ALLOC(OStream, 1);
  OSTREAM_CHECK_INSTANCE(self);

  ostream_init(self, stream);
  return self;
}

OStream_ptr OStream_create_file(const char* fname, boolean append)
{
  FILE* file = fopen(fname, append ? "a" : "w");

  if ((FILE*)NULL == file) {
    error_unreachable_code_msg("Unable to create file %s", fname);
  }

  return OStream_create(file);
}

OStream_ptr OStream_copy(OStream_ptr self)
{
  OStream_ptr copy = ALLOC(OStream, 1);

  OSTREAM_CHECK_INSTANCE(self);
  OSTREAM_CHECK_INSTANCE(copy);

  copy->stream = self->stream;
  copy->indent_size = self->indent_size;
  copy->split_nl = self->split_nl;

  copy->next_needs_indentation = self->next_needs_indentation;

  return copy;
}

FILE* OStream_get_stream(const OStream_ptr self)
{
  return self->stream;
}

void OStream_destroy(OStream_ptr self)
{
  OSTREAM_CHECK_INSTANCE(self);

  ostream_deinit(self, true);
  FREE(self);
}

void OStream_destroy_safe(OStream_ptr self)
{
  OSTREAM_CHECK_INSTANCE(self);

  ostream_deinit(self, false);
  FREE(self);
}

void OStream_printf(const OStream_ptr self, const char* format, ...)
{
  va_list args;

  va_start(args, format);
  print_splitted_node(self, NULL, format, args);
  va_end(args);
}

void OStream_nprintf(const OStream_ptr self,
                     const MasterPrinter_ptr node_printer,
                     const char* format, ...)
{
  va_list args;
  va_start(args, format);
  print_splitted_node(self, node_printer, format, args);
  va_end(args);
}

void OStream_vprintf(const OStream_ptr self, const char* format, va_list args)
{
  print_splitted_node(self, NULL, format, args);
}

void OStream_nvprintf(const OStream_ptr self,
                      const MasterPrinter_ptr node_printer,
                      const char* format, va_list args)
{
  print_splitted_node(self, node_printer, format, args);
}

void OStream_flush(const OStream_ptr self)
{
  fflush(self->stream);
}

void OStream_inc_indent_size(OStream_ptr self)
{
  self->indent_size++;
}

void OStream_dec_indent_size(OStream_ptr self)
{
  self->indent_size--;
}

int OStream_get_indent_size(const OStream_ptr self)
{
  return self->indent_size;
}

void OStream_reset_indent_size(OStream_ptr self)
{
  self->indent_size = 0;
}

void OStream_set_indent_size(OStream_ptr self, int n)
{
  self->indent_size = n;
}

void OStream_set_split_newline(OStream_ptr self, boolean enabled)
{
  self->split_nl = enabled;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void OStream_set_stream(OStream_ptr self, FILE* stream)
{
  if (self->stream != stream) {
    if ((FILE*)NULL != self->stream) {
      fflush(self->stream);

      if (stdout != self->stream && stderr != self->stream) {
        fclose(self->stream);
      }
    }

    self->stream = stream;
  }
}

FILE* OStream_reset_stream(OStream_ptr self)
{
  FILE* rv = self->stream;

  OSTREAM_CHECK_INSTANCE(self);

  self->stream = (FILE*)NULL;

  return rv;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The OStream class private initializer

  The OStream class private initializer

  \sa OStream_create
*/
static void ostream_init(OStream_ptr self, FILE* stream)
{
  /* members initialization */
  self->stream = stream;
  self->indent_size = 0;
  self->split_nl = true;

  self->next_needs_indentation = true;
}

/*!
  \brief The OStream class private deinitializer

  The OStream class private deinitializer

  \sa OStream_destroy
*/
static void ostream_deinit(OStream_ptr self, boolean close_stream)
{
  /* members deinitialization */

  if ((FILE*)NULL != self->stream) {
    fflush(self->stream);

    if (close_stream && stdout != self->stream && stderr != self->stream) {
      fclose(self->stream);
    }
  }

}



/**AutomaticEnd***************************************************************/

