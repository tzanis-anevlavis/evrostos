/* ---------------------------------------------------------------------------


  This file is part of the ``utils'' package of NuSMV version 2.
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
  \brief Simple pretty printing

  Routines that provides some simple pretty printing routines.

*/



#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "cudd/util.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/dd/dd.h"
#include "nusmv/core/rbc/rbc.h"
#include "nusmv/core/set/set.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/compile/compile.h"
/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

int UtilsIO_get_param_len(const char* fmt,
                          enum var_type* type,
                          enum var_modifier* mod)
{
  int i = 1;
  boolean done = false;

  *type = VAR_NONE;
  *mod = MOD_NONE;

  nusmv_assert(fmt[0] == '%');

  do {
    switch (fmt[i]) {

      /* Conversion specifiers (see man fprintf) */
    case 'o':
    case 'u':
    case 'x':
    case 'X':
    case 'd':
    case 'i': *type = VAR_INT; done = true; break;

    case 'G':
    case 'g':
    case 'F':
    case 'f':
    case 'E':
    case 'e': *type = VAR_DOUBLE; done = true; break;

    case 'c': *type = VAR_CHAR; done = true; break;

    case 's': *type = VAR_STRING; done = true; break;

    case 'p': *type = VAR_POINTER; done = true; break;

      /* Length modifiers (also in man fprintf) */
    case 'h':
      if (MOD_SHORT == *mod) *mod = MOD_SHORT_SHORT;
      else {
        nusmv_assert(MOD_NONE == *mod);
        *mod = MOD_SHORT;
      }
      break;

    case 'l':
      if (MOD_LONG == *mod) *mod = MOD_LONG_LONG;
      else {
        nusmv_assert(MOD_NONE == *mod);
        *mod = MOD_LONG;
      }
      break;

    case 'L':
      nusmv_assert(MOD_NONE == *mod);
      *mod = MOD_LONG_DOUBLE;
      break;

    case 'z':
      nusmv_assert(MOD_NONE == *mod);
      *mod = MOD_SIZE_T;
      break;

      /* Not handled */
    case 'N': /* N should be trapped by callers */

    case 'n':
    case 'j': error_unreachable_code();

      /* Error trapping */
    case '\0': error_unreachable_code();

    default: /* None */ break;
    }

    ++i;
  } while (!done);

  return i;
}

int UtilsIO_node_vsnprintf(const MasterPrinter_ptr printer,
                           char* output, size_t size,
                           const char* fmt, va_list args)
{
  int res = 0;
  size_t i = 0;
  char buf[32];

  MASTER_PRINTER_CHECK_INSTANCE(printer);
  nusmv_assert((const char*)NULL != fmt);

  output[0] = '\0';

  while (fmt[i] != '\0') {
    if (fmt[i] == '%') {
      if (fmt[i + 1] == '%') {

        if (res < size) {
          output = strncat(output, "%%", size - res - 1);
        }

        res += 2;
        i += 2;
      }
      else if (fmt[i + 1] == 'N') {
        node_ptr node = va_arg(args, node_ptr);
        char* str = sprint_node(printer, node);

        if (res < size) {
          output = strncat(output, str, size - res - 1);
        }

        res += strlen(str);
        FREE(str);
        i += 2;
      }
      else {
        enum var_type type;
        enum var_modifier mod;
        int read, wrote;
        char* tmp = ALLOC(char, size);
        nusmv_assert(NULL != tmp);

        read = UtilsIO_get_param_len(fmt + i, &type, &mod);

        snprintf(buf, read + 1, "%s", fmt + i);

        switch (type) {

        case VAR_INT:
          switch(mod) {
          case MOD_NONE:
          case MOD_SHORT:
          case MOD_SHORT_SHORT: wrote = snprintf(tmp, size, buf, va_arg(args, int)); break;
          case MOD_SIZE_T: wrote = snprintf(tmp, size, buf, va_arg(args, size_t)); break;
          case MOD_LONG: wrote = snprintf(tmp, size, buf, va_arg(args, long int)); break;
          case MOD_LONG_LONG: wrote = snprintf(tmp, size, buf, va_arg(args, long long int)); break;

            /* The rest is an error (or unhandled feature) */
          default: error_unreachable_code();
          }
          break; /* case VAR_INT */

        case VAR_CHAR:
          switch(mod) {
          case MOD_NONE: wrote = snprintf(tmp, size, buf, va_arg(args, int)); break;

            /* The rest is an error (or unhandled feature) */
          default: error_unreachable_code();
          }
          break; /* case VAR_CHAR */

        case VAR_STRING:
          switch(mod) {
          case MOD_NONE: wrote = snprintf(tmp, size, buf, va_arg(args, char*)); break;

            /* The rest is an error (or unhandled feature) */
          default: error_unreachable_code();
          }
          break;  /* case VAR_STRING */

        case VAR_POINTER:
          switch(mod) {
          case MOD_NONE: wrote = snprintf(tmp, size, buf, va_arg(args, void*)); break;

            /* The rest is an error (or unhandled feature) */
          default: error_unreachable_code();
          }
          break;  /* case VAR_POINTER */

        case VAR_DOUBLE:
          switch(mod) {
          case MOD_NONE: wrote = snprintf(tmp, size, buf, va_arg(args, double)); break;
          case MOD_LONG_DOUBLE: wrote = snprintf(tmp, size, buf, va_arg(args, long double)); break;

            /* The rest is an error (or unhandled feature) */
          default: error_unreachable_code();
          }
          break; /* case VAR_DOUBLE */

          /* Errors */
        default: error_unreachable_code();
        }

        if (res < size) {
          output = strncat(output, tmp, size - res - 1);
        }

        res += wrote;
        i += read;

        FREE(tmp);
      }
    } /* if (fmt[i] = '%') */
    else {
      if (res < size) {
        output[res] = fmt[i];
        output[res + 1] = '\0';
      }

      ++res;
      ++i;
    }
  }

  output[res + 1] = '\0';

  return res;
}

int UtilsIO_node_vfprintf(const MasterPrinter_ptr printer,
                          FILE* output, const char* fmt, va_list args)
{
  int res = 0;
  size_t i = 0;
  char buf[32];

  MASTER_PRINTER_CHECK_INSTANCE(printer);
  nusmv_assert((const char*)NULL != fmt);

  while (fmt[i] != '\0') {
    if (fmt[i] == '%') {
      if (fmt[i + 1] == '%') {
        res += fprintf(output, "%%");
        i += 2;
      }
      else if (fmt[i + 1] == 'N') {
        node_ptr node = va_arg(args, node_ptr);
        char* str = sprint_node(printer, node);
        res += fprintf(output, "%s", str);
        FREE(str);
        i += 2;
      }
      else {
        enum var_type type;
        enum var_modifier mod;
        int read;

        read = UtilsIO_get_param_len(fmt + i, &type, &mod);

        snprintf(buf, read + 1, "%s", fmt + i);

        switch (type) {

        case VAR_INT:
          switch(mod) {
          case MOD_NONE:
          case MOD_SHORT:
          case MOD_SHORT_SHORT: res += fprintf(output, buf, va_arg(args, int)); break;
          case MOD_SIZE_T: res += fprintf(output, buf, va_arg(args, size_t)); break;
          case MOD_LONG: res += fprintf(output, buf, va_arg(args, long int)); break;
          case MOD_LONG_LONG: res += fprintf(output, buf, va_arg(args, long long int)); break;

            /* The rest is an error (or unhandled feature) */
          default: error_unreachable_code();
          }
          break; /* case VAR_INT */

        case VAR_CHAR:
          switch(mod) {
          case MOD_NONE: res += fprintf(output, buf, va_arg(args, int)); break;

            /* The rest is an error (or unhandled feature) */
          default: error_unreachable_code();
          }
          break; /* case VAR_CHAR */

        case VAR_STRING:
          switch(mod) {
          case MOD_NONE: res += fprintf(output, buf, va_arg(args, char*)); break;

            /* The rest is an error (or unhandled feature) */
          default: error_unreachable_code();
          }
          break;  /* case VAR_STRING */

        case VAR_POINTER:
          switch(mod) {
          case MOD_NONE: res += fprintf(output, buf, va_arg(args, void*)); break;

            /* The rest is an error (or unhandled feature) */
          default: error_unreachable_code();
          }
          break;  /* case VAR_POINTER */

        case VAR_DOUBLE:
          switch(mod) {
          case MOD_NONE: res += fprintf(output, buf, va_arg(args, double)); break;
          case MOD_LONG_DOUBLE: res += fprintf(output, buf, va_arg(args, long double)); break;

            /* The rest is an error (or unhandled feature) */
          default: error_unreachable_code();
          }
          break; /* case VAR_DOUBLE */

          /* Errors */
        default: error_unreachable_code();
        }

        i += read;
      }
    } /* if (fmt[i] = '%') */
    else {
      res += fprintf(output, "%c", fmt[i]);
      ++i;
    }
  }

  return res;
}

int UtilsIO_node_fprintf(const MasterPrinter_ptr printer,
                         FILE* output, const char* fmt, ...)
{
  int res;
  va_list args;

  va_start(args, fmt);

  res = UtilsIO_node_vfprintf(printer, output, fmt, args);

  va_end(args);

  return res;
}

int UtilsIO_node_snprintf(const MasterPrinter_ptr printer,
                          char* output, size_t size,
                          const char* fmt, ...)
{
  int res;
  va_list args;

  va_start(args, fmt);

  res = UtilsIO_node_vsnprintf(printer, output, size, fmt, args);

  va_end(args);

  return res;
}


#if 0
/*
  This function prints out string "s" followed by the process
  name. This function is only used if verbose mode is greter than zero.
*/
void print_in_process(MasterPrinter_ptr printer, char *s, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(printer));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  StreamMgr_print_error(streams,  "%s", s);
  if(context != Nil) indent_node(printer, errstream, " in process ", context, "");
  StreamMgr_print_error(streams,  "...\n");
}

#endif
