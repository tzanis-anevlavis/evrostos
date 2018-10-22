/* ---------------------------------------------------------------------------


  This file is part of the ``node.printers'' package of NuSMV version 2.
  Copyright (C) 2006 by FBK-irst.

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
  \brief Implementaion of class 'MasterPrinter', derived from
  MasterNodeWalker

  \todo: Missing description

*/


#if HAVE_CONFIG_H
#  include "nusmv-config.h"
#endif

#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/node/printers/MasterPrinter_private.h"
#include "nusmv/core/node/printers/PrinterBase.h"
#include "nusmv/core/node/printers/PrinterBase_private.h"
#include "nusmv/core/node/MasterNodeWalker_private.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/node/printers/printersInt.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/utils.h"

#if NUSMV_HAVE_STRING_H
#  include <string.h> /* for strdup */
#else
#  ifndef strdup
char* strdup(const char*); /* forward declaration */
#  endif
#endif

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct MasterPrinter_TAG
{
  /* this MUST stay on the top */
  INHERITS_FROM(MasterNodeWalker);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  StreamType stream_type;
  union StreamTypeArg stream_arg;

  NodeList_ptr indent_stack; /* internal indentation levels stack */
  int current_ofs;  /* current offset on the current line */

  /* string stream is handled by using these varaibles: */
  char* sstream;       /* string stream */
  size_t sstream_cap;  /* sstream allocation capacity */
  size_t sstream_len;  /* sstream current usage */
  size_t sstream_grow_sum; /* sstream grow sum */
  int sstream_grow_num; /* number of resizes */

  StreamMgr_ptr streams;

  int (*inner_stream_print)(MasterPrinter_ptr self, const char* str);
  int (*external_stream_print)(void* stream, const char* str);

} MasterPrinter;

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BLANK_STR " "

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NEWLINE_CHR '\n'

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NEWLINE_STR "\n"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define TERM_CHR '\0'

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void master_printer_init(MasterPrinter_ptr self,
                                const NuSMVEnv_ptr env);

static void master_printer_deinit(MasterPrinter_ptr self);

static void master_printer_finalize(Object_ptr object, void* dummy);

static int
master_printer_fprint(MasterPrinter_ptr self, const char* str);

static int
master_printer_sprint(MasterPrinter_ptr self, const char* str);

static int
master_printer_get_level(MasterPrinter_ptr self);

static void
master_printer_reset_string_stream(MasterPrinter_ptr self);

/*---------------------------------------------------------------------------*/
/* Definition of TOP LEVEL exported functions                                */
/*---------------------------------------------------------------------------*/

void debug_print_node(NuSMVEnv_ptr env, node_ptr node)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* out = StreamMgr_get_output_stream(streams);

  print_node(wffprint, out, node);
}

void debug_print_sexp(NuSMVEnv_ptr env, node_ptr node)
{
  const MasterPrinter_ptr sexpprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_SEXP_PRINTER));
  StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* out = StreamMgr_get_output_stream(streams);

  print_node(sexpprint, out, node);
}

int print_node(MasterPrinter_ptr wff_printer, FILE *stream, node_ptr n)
{
  union StreamTypeArg sta;
  sta.file = stream;

  MasterPrinter_set_stream_type(wff_printer, STREAM_TYPE_FILE, &sta);
  MasterPrinter_reset_stream(wff_printer, 0);

  return
    MasterPrinter_print_node(wff_printer, n)  &&

    /* ensure proper flushing */
    MasterPrinter_flush_stream(wff_printer);
}

char* sprint_node(MasterPrinter_ptr mp, node_ptr n)
{
  boolean success;
  MasterPrinter_set_stream_type(mp,
                                STREAM_TYPE_STRING,
                                STREAM_TYPE_ARG_UNUSED);
  MasterPrinter_reset_stream(mp, 0);

  success = (MasterPrinter_print_node(mp, n) &&
             MasterPrinter_flush_stream(mp));

  return (success)? strdup(MasterPrinter_get_streamed_string(mp)) : NULL;
}

int print_node_indent_at(MasterPrinter_ptr mp,
                         FILE *stream, node_ptr n, int ofs)
{
  union StreamTypeArg sta;
  sta.file = stream;

  MasterPrinter_set_stream_type(mp, STREAM_TYPE_FILE, &sta);
  MasterPrinter_reset_stream(mp, ofs);

  return
    MasterPrinter_print_node(mp, n) &&

    /* ensure proper flushing */
    MasterPrinter_flush_stream(mp);
}

char* sprint_node_indent_at(MasterPrinter_ptr mp, node_ptr n, int ofs)
{
  boolean success;

  MasterPrinter_set_stream_type(mp,
                                STREAM_TYPE_STRING,
                                STREAM_TYPE_ARG_UNUSED);
  MasterPrinter_reset_stream(mp, ofs);

  success = (MasterPrinter_print_node(mp, n) &&
             /* ensure proper flushing */
             MasterPrinter_flush_stream(mp));

  return (success)? strdup(MasterPrinter_get_streamed_string(mp)) : NULL;
}

int print_node_indent(MasterPrinter_ptr mp, FILE *stream, node_ptr n)
{
  return print_node_indent_at(mp, stream, n, 0);
}

char* sprint_node_indent(MasterPrinter_ptr mp, node_ptr n)
{
  return sprint_node_indent_at(mp, n, 0);
}


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

MasterPrinter_ptr MasterPrinter_create(const NuSMVEnv_ptr env)
{
  MasterPrinter_ptr self = ALLOC(MasterPrinter, 1);
  MASTER_PRINTER_CHECK_INSTANCE(self);

  master_printer_init(self, env);
  return self;
}

int MasterPrinter_print_node(MasterPrinter_ptr self, node_ptr n)
{
  MASTER_PRINTER_CHECK_INSTANCE(self);
  return master_printer_print_node(self, n, 0);
}

int MasterPrinter_print_string(MasterPrinter_ptr self, const char* str)
{
  int res = 1;    /* defaults to ok */
  char* s = strdup(str);
  char* p = s;  /* beginning, end pointers to the current chunk */
  char* q;

  MASTER_PRINTER_CHECK_INSTANCE(self);

  /* if there are newlines in the string to be printed, cut the string where
   * first newline occurs, then print the fragment followed by a newline and
   * a number of spaces according to internal indentation stack head. Repeat
   * until all fragments have been printed.
   **/

  do {
    boolean pad = false;

    if ((q=strchr(p, NEWLINE_CHR))) {
      /* side effects on p */
      (*q) = TERM_CHR; q++;
      pad = true;
    }

    /* original printout */
    switch (self->stream_type) {
    case STREAM_TYPE_DEFAULT:
    case STREAM_TYPE_STDOUT:
    case STREAM_TYPE_STDERR:
    case STREAM_TYPE_FILE:
      if (!master_printer_fprint(self, p)) {
        res = 0; /* failure */
        goto leave;
      }
      break;

    case STREAM_TYPE_STRING:
      if (!master_printer_sprint(self, p)) {
        res = 0; /* failure */
        goto leave;
      }
      break;

    case STREAM_TYPE_FUNCTION:
      {
        void* arg = self->stream_arg.function.argument;
        if (!self->stream_arg.function.func_ptr(p, arg)) {
          res = 0; /* failure */
          goto leave;
        }
        break;
      }

    default:
      {
        const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
        const ErrorMgr_ptr errmgr =
          ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
        ErrorMgr_internal_error(errmgr, "MasterPrinter::print_string: Invalid stream type\n");
      }
    }

    /* update current ofs */
    self->current_ofs += strlen(p);

    /* post-processing padding */
    if (pad) {
      int i, padding = master_printer_get_level(self);

      /* add a newline and #padding spaces */
      for (i=0; i<=padding; i++) {
        char *tmp = (!i) ? NEWLINE_STR : BLANK_STR;

        switch (self->stream_type) {
        case STREAM_TYPE_DEFAULT:
        case STREAM_TYPE_STDOUT:
        case STREAM_TYPE_STDERR:
        case STREAM_TYPE_FILE:
          if (!master_printer_fprint(self, tmp)) {
            res = 0; /* failure */
            goto leave;
          }
          break;

        case STREAM_TYPE_STRING:
          if (!master_printer_sprint(self, tmp)) {
            res = 0; /* failure */
            goto leave;
          }
          break;

        case STREAM_TYPE_FUNCTION:
          {
            void* arg = self->stream_arg.function.argument;
            if (!self->stream_arg.function.func_ptr(tmp, arg)) {
              res = 0; /* failure */
              goto leave;
            }
            break;
          }

        default:
          {
            const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
            const ErrorMgr_ptr errmgr =
              ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
            ErrorMgr_internal_error(errmgr, "MasterPrinter::print_string: Invalid stream type\n");
          }
        }
      }

      /* update current ofs */
      self->current_ofs = padding;
    }

    p=q;
  } while (p); /* is there anything left to print? */

 leave:
  FREE(s);
  return res; /* 1 if no failures occurred */
}

const char* MasterPrinter_get_streamed_string(const MasterPrinter_ptr self)
{
  MASTER_PRINTER_CHECK_INSTANCE(self);
  nusmv_assert(self->stream_type == STREAM_TYPE_STRING);

  return (const char*) (self->sstream);
}

void MasterPrinter_set_stream_type(MasterPrinter_ptr self,
                                   StreamType type,
                                   const union StreamTypeArg* arg)
{
  MASTER_PRINTER_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    switch (type) {
    case STREAM_TYPE_DEFAULT:
    case STREAM_TYPE_STDOUT:
      self->stream_arg.file = StreamMgr_get_output_stream(self->streams);
      break;

    case STREAM_TYPE_STDERR:
      self->stream_arg.file = StreamMgr_get_error_stream(self->streams);
      break;

    case STREAM_TYPE_STRING:
      if (self->stream_type != STREAM_TYPE_STRING) {
        /* we start with a new string stream, but only if it is not
           currently set a string stream */
        master_printer_reset_string_stream(self);
      }
      break;

    case STREAM_TYPE_FILE:
      self->stream_arg.file = ((STREAM_TYPE_ARG_UNUSED != arg)?
                               arg->file : NULL);
      break;

    case STREAM_TYPE_FUNCTION:
      /*
        Copy the structure 'function' (Both function pointer and void* argument)
      */
      nusmv_assert(STREAM_TYPE_ARG_UNUSED != arg);
      self->stream_arg.function = arg->function;
      break;

    default:
      ErrorMgr_internal_error(errmgr, "MasterPrinter::set_stream_type: Invalid stream type\n");
    }

    self->stream_type = type;
  }
}

StreamType MasterPrinter_get_stream_type(const MasterPrinter_ptr self)
{
  MASTER_PRINTER_CHECK_INSTANCE(self);
  return self->stream_type;
}

void  MasterPrinter_reset_stream(MasterPrinter_ptr self, int initial_offset)
{
  ListIter_ptr p;
  MASTER_PRINTER_CHECK_INSTANCE(self);

  self->current_ofs = (0<initial_offset) ? initial_offset : 0;

  /* clear stack (should be empty anyway) */
  p = NodeList_get_first_iter(self->indent_stack);
  while (NULL != p) {
    NodeList_remove_elem_at(self->indent_stack, p);
    p = NodeList_get_first_iter(self->indent_stack);
  }

  /* stream dependant operations */
  switch (self->stream_type) {

  case STREAM_TYPE_DEFAULT:
  case STREAM_TYPE_STDOUT:
  case STREAM_TYPE_STDERR:
  case STREAM_TYPE_FILE:
  case STREAM_TYPE_FUNCTION:
    break;

  case STREAM_TYPE_STRING:
    master_printer_reset_string_stream(self);
    break;

  default:
    error_unreachable_code(); /* no other possible cases */
  }
}

void MasterPrinter_close_stream(MasterPrinter_ptr self)
{
  MASTER_PRINTER_CHECK_INSTANCE(self);

  switch (self->stream_type) {
  case STREAM_TYPE_DEFAULT:
  case STREAM_TYPE_STDOUT:
  case STREAM_TYPE_STDERR:
    break;

  case STREAM_TYPE_STRING:
    /* resets the string stream */
    master_printer_reset_string_stream(self);
    break;

  case STREAM_TYPE_FILE:
      /* closes the file stream if not nusmv_std{out,err} */
      if ((self->stream_arg.file != StreamMgr_get_output_stream(self->streams)) &&
          (self->stream_arg.file != StreamMgr_get_error_stream(self->streams))) {
        fclose(self->stream_arg.file);
      }
      break;

  case STREAM_TYPE_FUNCTION:
    break;

  default:
    error_unreachable_code(); /* no other possible cases */
  }

  /* sets the default stream type */
  MasterPrinter_set_stream_type(self, STREAM_TYPE_DEFAULT,
                                STREAM_TYPE_ARG_UNUSED);
}

int MasterPrinter_flush_stream(MasterPrinter_ptr self)
{
  MASTER_PRINTER_CHECK_INSTANCE(self);

  switch (self->stream_type) {
  case STREAM_TYPE_DEFAULT:
    /* not applicable here */
    break;

  case STREAM_TYPE_STDOUT:
    return !fflush(StreamMgr_get_output_stream(self->streams));
    break;

  case STREAM_TYPE_STDERR:
    return !fflush(StreamMgr_get_error_stream(self->streams));
    break;

  case STREAM_TYPE_STRING:
    /* not applicable here */
    break;

  case STREAM_TYPE_FILE:
    return !fflush(self->stream_arg.file);
    break;

  case STREAM_TYPE_FUNCTION:
    /* not applicable here */
    break;

  default:
    error_unreachable_code(); /* no other possible cases */
  }

  return 1;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

int master_printer_print_node(MasterPrinter_ptr self,
                              node_ptr n, int priority)
{
  ListIter_ptr iter;
  iter = NodeList_get_first_iter(MASTER_NODE_WALKER(self)->walkers);
  while (!ListIter_is_end(iter)) {
    PrinterBase_ptr pr =
      PRINTER_BASE(NodeList_get_elem_at(MASTER_NODE_WALKER(self)->walkers,
                                        iter));

    if (NodeWalker_can_handle(NODE_WALKER(pr), n)) {

      return PrinterBase_print_node(pr, n, priority);
    }

    iter = ListIter_get_next(iter);
  }

  return 1;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The MasterPrinter class private initializer

  The MasterPrinter class private initializer

  \sa MasterPrinter_create
*/
static void master_printer_init(MasterPrinter_ptr self, const NuSMVEnv_ptr env)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  /* base class initialization */
  master_node_walker_init(MASTER_NODE_WALKER(self), env);

  /* allocates a minimal string stream */
  self->streams = streams;
  self->sstream_cap = 1;
  self->sstream_len = 0;
  self->sstream_grow_sum = 0;
  self->sstream_grow_num = 0;
  self->sstream = ALLOC(char, self->sstream_cap);
  self->sstream[0] = '\0';

  /* initialize internal structure for pretty printing */
  self->indent_stack = NodeList_create();
  self->current_ofs  = 0;

  /* default stream */
  MasterPrinter_set_stream_type(self,
                                STREAM_TYPE_DEFAULT,
                                STREAM_TYPE_ARG_UNUSED);

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = master_printer_finalize;

}

/*!
  \brief The MasterPrinter class private deinitializer

  The MasterPrinter class private deinitializer

  \sa MasterPrinter_destroy
*/
static void master_printer_deinit(MasterPrinter_ptr self)
{
  NodeList_destroy(self->indent_stack);
  self->indent_stack = NODE_LIST(NULL);
  FREE(self->sstream);
  self->sstream = (char*) NULL;

  /* base class deinitialization */
  master_node_walker_deinit(MASTER_NODE_WALKER(self));
}

/*!
  \brief The PrinterBase class virtual finalizer

  Called by the class destructor
*/
static void master_printer_finalize(Object_ptr object, void* dummy)
{
  MasterPrinter_ptr self = MASTER_PRINTER(object);

  master_printer_deinit(self);
  FREE(self);
}

/*!
  \brief Appends a string to the internal string stream

  Warning: current stream type must be STREAM_TYPE_STRING
*/
static int master_printer_sprint(MasterPrinter_ptr self, const char* str)
{
  int len;

  nusmv_assert(self->stream_type == STREAM_TYPE_STRING);

  /* ignore empty strings */
  len = strlen(str);
  if (0 != len) {
    self->sstream_len += len;

    if (self->sstream_len >= self->sstream_cap) {
      /* resizes the sstream */
      self->sstream_grow_sum += self->sstream_len;
      self->sstream_grow_num += 1;
      self->sstream_cap = self->sstream_len + 1 +
        (self->sstream_grow_sum / self->sstream_grow_num);

      self->sstream = (char*) REALLOC(char, self->sstream, self->sstream_cap);
      nusmv_assert(self->sstream != (char*) NULL);
    }

    strcat(self->sstream, str);
  }

  return 1;
}

/*!
  \brief Stream the given string to the internally set file
  stream

  Warning: current stream type must be compatible
*/
static int master_printer_fprint(MasterPrinter_ptr self, const char* str)
{
  nusmv_assert( (self->stream_type == STREAM_TYPE_DEFAULT) ||
                (self->stream_type == STREAM_TYPE_STDOUT) ||
                (self->stream_type == STREAM_TYPE_STDERR) ||
                (self->stream_type == STREAM_TYPE_FILE));

  if (str[0] != '\0') {
    nusmv_assert(NULL != self->stream_arg.file);

    return fprintf(self->stream_arg.file, "%s", str);
  }

  else return 1;
}

int master_printer_indent(MasterPrinter_ptr self)
{
  MASTER_PRINTER_CHECK_INSTANCE(self);

  NodeList_prepend(self->indent_stack,
                   NODE_FROM_INT (self->current_ofs));

  /* never fails */
  return 1;
}

int master_printer_deindent(MasterPrinter_ptr self)
{
  ListIter_ptr head;
  MASTER_PRINTER_CHECK_INSTANCE(self);

  head = NodeList_get_first_iter(self->indent_stack);
  if (NULL == head) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    ErrorMgr_internal_error(errmgr, "printout failure: empty indentation stack!");
  }

  NodeList_remove_elem_at(self->indent_stack, head);

  /* never fails */
  return 1;

}

/*!
  \brief Get current level of indentation

  Returns an integer representing the current level of
  indentation (i.e. the number of whitespaces to pad the string
  with). If the internal stack is empty, assume indentation level is 0
  for backward compatibility.
*/

static int
master_printer_get_level(MasterPrinter_ptr self)
{
  ListIter_ptr head;

  MASTER_PRINTER_CHECK_INSTANCE(self);

  head = NodeList_get_first_iter(self->indent_stack);
  return (NULL != head)
    ? NODE_TO_INT(NodeList_get_elem_at(self->indent_stack, head))
    : 0 ;
}

/*!
  \brief Cleans up the stream that have been read so far.
  Any previoulsy read stream will be lost



  \sa get_streamed_string
*/

static void master_printer_reset_string_stream(MasterPrinter_ptr self)
{
  MASTER_PRINTER_CHECK_INSTANCE(self);

  self->sstream[0] = '\0';
  self->sstream_len = 0;
}

/**AutomaticEnd***************************************************************/
