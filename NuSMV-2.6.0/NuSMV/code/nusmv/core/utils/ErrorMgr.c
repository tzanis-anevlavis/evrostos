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
  \brief Implementation of class 'ErrorMgr'

  \todo: Missing description

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include <stdarg.h>

#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/utils/Stack.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/mc/mc.h"
#include "nusmv/core/cinit/cinit.h"
#include "nusmv/core/bmc/bmcUtils.h"
#include "nusmv/core/utils/EnvObject.h"
#include "nusmv/core/utils/EnvObject_private.h"
#include "nusmv/core/utils/assoc.h"
#include "cudd/st.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define MAXJMP 20

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/
/*!
  \struct
  \brief The settings related to a single tag

  A tag is just a name that can be used to mark a area of code. When marked
  code is met, actions selected by the user are executed. Currently, only
  enabling/disabling marked code is available.

  Typical usage:
  A user needs to disable a specific warning. He decide to tag
  this code after the name of the method, for instance
  "fsm_init_empty". He searches the method and surround the call to the
  printer with

  ERROR_MGR_BEGIN_TAG("fsm_init_empty")
  StreamMgr_flush_streams(self->streams);
  StreamMgr_print_error(self->streams, "\n********   WARNING   ********\n");
  StreamMgr_print_error(self->streams,
          "The initial states set of the finite state machine is empty.\n"
          "This might make results of model checking not trustable.\n");
  StreamMgr_print_error(self->streams, "******** END WARNING ********\n");
  ERROR_MGR_END_TAG

  Then, in the code corresponding to the flow for which he needs to disable the
  warning, he calls:

  ErrorMgr_disable_tag("fsm_init_empty");

  Before exiting its code, and keeping in mind also the error flows (longjumps,
  ...), he will call:

  ErrorMgr_enable_tag("fsm_init_empty");
*/
typedef struct _TagInfo {
  boolean enabled;
} TagInfo;

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct ErrorMgr_TAG
{
  INHERITS_FROM(EnvObject);
  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  JMPBUF jmp_buf_arr[MAXJMP];
  int jmp_buf_pos;

  Stack_ptr io_atom_stack;


  OptsHandler_ptr options;
  StreamMgr_ptr streams;
  UStringMgr_ptr strings;

  node_ptr the_node;

  hash_ptr tags;
} ErrorMgr;



/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/
/*!
  \brief Mark the begin of a tag
  See TagInfo struct for information about tags
*/
#define ERROR_MGR_BEGIN_TAG(/* string_ptr */ tag)    \
  if (error_mgr_is_tag_enabled(self, UStringMgr_find_string(self->strings, tag))) {

/*!
  \brief Mark the end of a tag
  See TagInfo struct for information about tags
*/
#define ERROR_MGR_END_TAG }

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void error_mgr_init(ErrorMgr_ptr self,
                           const NuSMVEnv_ptr env);

static void error_mgr_deinit(ErrorMgr_ptr self);

static void error_mgr_finalize(Object_ptr object, void* dummy);

static boolean error_mgr_is_tag_enabled(ErrorMgr_ptr self,
                                        string_ptr tags);

static void TagInfo_init(TagInfo* self);

static void error_mgr_deinit_tags(ErrorMgr_ptr self);

static enum st_retval error_mgr_clear_tags(char* key, char* value, char* arg);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

ErrorMgr_ptr ErrorMgr_create(const NuSMVEnv_ptr env)
{
  ErrorMgr_ptr self = ALLOC(ErrorMgr, 1);

  ERROR_MGR_CHECK_INSTANCE(self);

  error_mgr_init(self, env);
  return self;
}

void ErrorMgr_destroy(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

StreamMgr_ptr ErrorMgr_get_stream_manager(const ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  return self->streams;
}

UStringMgr_ptr ErrorMgr_get_string_manager(const ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  return self->strings;
}

OptsHandler_ptr  ErrorMgr_get_options_handler(const ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  return self->options;
}

node_ptr ErrorMgr_get_the_node(const ErrorMgr_ptr self)
{
  return self->the_node;
}


void ErrorMgr_set_the_node(ErrorMgr_ptr self, node_ptr node)
{
  self->the_node = node;
}

JMPBUF* ErrorMgr_new_long_jmp(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  nusmv_assert(self->jmp_buf_pos < MAXJMP);

  return (&(self->jmp_buf_arr[(self->jmp_buf_pos)++]));
}

void ErrorMgr_long_jmp(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  if (self->jmp_buf_pos > 0) {
    LONGJMP(self->jmp_buf_arr[--(self->jmp_buf_pos)], 2);
  }
}

void ErrorMgr_cancel_long_jmp(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);
  nusmv_assert((self->jmp_buf_pos) > 0);
  (self->jmp_buf_pos)--;
}

void ErrorMgr_reset_long_jmp(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);
  self->jmp_buf_pos = 0;
}

void ErrorMgr_start_parsing_err(ErrorMgr_ptr self)
{
  extern int nusmv_yylineno;
  ERROR_MGR_CHECK_INSTANCE(self);

  /* to flush all existing messages before outputting */
  StreamMgr_flush_streams(self->streams);
  StreamMgr_print_error(self->streams, "\n");
  if (get_input_file(self->options)) {
    StreamMgr_print_error(self->streams, "file %s: ",
                          get_input_file(self->options));
  }
  else {
    StreamMgr_print_error(self->streams, "file stdin: ");
  }

  if (nusmv_yylineno) StreamMgr_print_error(self->streams, "line %d: ", nusmv_yylineno);
}

void ErrorMgr_finish_parsing_err(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  StreamMgr_print_error(self->streams, "\n");

  ErrorMgr_print_io_atom_stack(self);
  ErrorMgr_nusmv_exit(self, 1);
}

void ErrorMgr_nusmv_exit(ErrorMgr_ptr self,
                         int n)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  ErrorMgr_long_jmp(self);

  if (opt_verbose_level_gt(self->options, 0) )
    StreamMgr_print_error(self->streams, "%s: exit(%d)\n",
                          get_pgm_name(self->options), n);
  StreamMgr_flush_streams(self->streams);
  exit(n);
}

void ErrorMgr_rpterr(ErrorMgr_ptr self, const char* fmt, ...)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  if ((fmt != (char*) NULL) && strlen(fmt) > 0) {
    FILE* err = StreamMgr_get_error_stream(self->streams);
    va_list args;

    ErrorMgr_start_parsing_err(self);
    va_start(args, fmt);
    (void) vfprintf(err, fmt, args);
    va_end(args);
  }

  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_rpterr_node(ErrorMgr_ptr self, node_ptr node,
                          const char* fmt, ...)
{
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  if ((fmt != (char*) NULL) && strlen(fmt) > 0) {
    FILE* err = StreamMgr_get_error_stream(self->streams);
    va_list args;

    ErrorMgr_start_parsing_err(self);
    va_start(args, fmt);
    (void) vfprintf(err, fmt, args);
    va_end(args);
    print_node(wffprint, err, node);
  }

  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_msg(const ErrorMgr_ptr self, const char* fmt, ...)
{
  ERROR_MGR_CHECK_INSTANCE(self);
  if ((fmt != (char*) NULL) && strlen(fmt) > 0) {
    FILE* err = StreamMgr_get_error_stream(self->streams);
    va_list args;

    va_start(args, fmt);
    (void) vfprintf(err, fmt, args);
    va_end(args);
  }
}

boolean ErrorMgr_io_atom_is_empty(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  return Stack_is_empty(self->io_atom_stack);
}

void ErrorMgr_io_atom_push(ErrorMgr_ptr self, node_ptr s)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  Stack_push(self->io_atom_stack, s);
}

void ErrorMgr_io_atom_pop(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  if (Stack_is_empty(self->io_atom_stack)) {
    ErrorMgr_internal_error(self, "io_atom_pop: stack empty");
  }

  Stack_pop(self->io_atom_stack);
}

node_ptr ErrorMgr_io_atom_head(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  if (Stack_is_empty(self->io_atom_stack)) {
    ErrorMgr_internal_error(self, "io_atom_pop: stack empty");
  }

  return Stack_top(self->io_atom_stack);
}

void ErrorMgr_print_io_atom_stack(ErrorMgr_ptr self)
{
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  while (!Stack_is_empty(self->io_atom_stack)) {
    FILE* err = StreamMgr_get_error_stream(self->streams);
    node_ptr s = Stack_pop(self->io_atom_stack);

    StreamMgr_print_error(self->streams, "in definition of ");
    print_node(wffprint, err, s);
    if (node_get_lineno(s)) {
      StreamMgr_print_error(self->streams, " at line %d", node_get_lineno(s));
    }

    StreamMgr_print_error(self->streams, "\n");
  }
}

void ErrorMgr_internal_error(ErrorMgr_ptr self, const char * fmt, ...)
{
  va_list args;
  FILE* err;

  ERROR_MGR_CHECK_INSTANCE(self);

  err = StreamMgr_get_error_stream(self->streams);

  StreamMgr_flush_streams(self->streams);
  va_start(args, fmt);
  fprintf(err, "\n\n*** internal error *** \n");
  (void) vfprintf(err, fmt, args);
  va_end(args);

  fprintf(err, "\n%s\n", NuSMVCore_get_bug_report_message());
  fprintf(err, "Send a copy of this output and your input.\n");

  ErrorMgr_nusmv_exit(self, 1);
}

void ErrorMgr_warning_msg(ErrorMgr_ptr self, const char * fmt, ...)
{
  va_list args;
  FILE* err;
  ERROR_MGR_CHECK_INSTANCE(self);

  err = StreamMgr_get_error_stream(self->streams);

  va_start(args, fmt);
  fprintf(err, "\n\n*** Warning *** \n");
  (void) vfprintf(err, fmt, args);
  va_end(args);
  fprintf(err, "\n*** Warning *** \n\n");
}

/* ------------------------------------------------------------------------- */
/* -- Specialized warning / error routines                                   */
/* ------------------------------------------------------------------------- */

void ErrorMgr_error_multiple_substitution(ErrorMgr_ptr self, node_ptr nodep)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "Multiple substitution for ");
  print_node(wffprint, err, nodep);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_report_failure_node(ErrorMgr_ptr self, node_ptr n)
{
  extern int nusmv_yylineno;

  ERROR_MGR_CHECK_INSTANCE(self);
  nusmv_assert(Nil != n && node_get_type(n) == FAILURE);

  nusmv_yylineno = ErrorMgr_failure_get_lineno(self, n); /* set the line number of the error */

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "%s\n", ErrorMgr_failure_get_msg(self, n));
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_warning_failure_node(ErrorMgr_ptr self, node_ptr n)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  nusmv_assert(Nil != n && node_get_type(n) == FAILURE);

  StreamMgr_print_error(self->streams, "WARNING: line: %d: %s\n",
                        ErrorMgr_failure_get_lineno(self, n),
                        ErrorMgr_failure_get_msg(self, n));
}

void ErrorMgr_warning_case_not_exhaustive(ErrorMgr_ptr self, node_ptr failure)
{
  ERROR_MGR_CHECK_INSTANCE(self);
  StreamMgr_print_error(self->streams,
                        "Warning: at line %d CASE expression might "    \
                        "contain not exhaustive conditions.\n"          \
                        "Use the BDD-based engine to check all CASEs "  \
                        "as current version cannot check\n"             \
                        "exhaustivity of boolean CASEs by using"        \
                        " Bounded Model Checking.\n\n",
                        ErrorMgr_failure_get_lineno(self, failure));
}

void ErrorMgr_warning_possible_div_by_zero(ErrorMgr_ptr self, node_ptr failure)
{
  ERROR_MGR_CHECK_INSTANCE(self);
  StreamMgr_print_error(self->streams,
                        "Warning: at line %d expression might " \
                        "contain a division by zero.\n\n",
                        ErrorMgr_failure_get_lineno(self, failure));
}

void ErrorMgr_warning_possible_array_out_of_bounds(ErrorMgr_ptr self,
                                                   node_ptr failure)
{
  ERROR_MGR_CHECK_INSTANCE(self);
  StreamMgr_print_error(self->streams,
                        "Warning: at line %d expression might result "  \
                        "in array subscripting out of bounds.\n\n",
                        ErrorMgr_failure_get_lineno(self, failure));
}

void ErrorMgr_error_array_out_of_bounds(ErrorMgr_ptr self, int index,
                                        int low, int high)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams,
                        "array index has value %d which is outside of " \
                        "allowed range [%d, %d]\n",
                        index, low, high);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_lhs_of_index_is_not_array(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams,
                        "left hand side of index sub-scripting is not an array");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_div_by_zero(ErrorMgr_ptr self, node_ptr expr)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "Division by zero: ");
  print_node(wffprint, err, expr);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_div_by_nonconst(ErrorMgr_ptr self, node_ptr expr)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "Division by non constant expression: ");
  print_node(wffprint, err, expr);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_mod_by_nonword(ErrorMgr_ptr self, node_ptr expr)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "Division by non word expression: ");
  print_node(wffprint, err, expr);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_type_error(ErrorMgr_ptr self, node_ptr n)
{
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  ErrorMgr_start_parsing_err(self);
  StreamMgr_nprint_error(self->streams, wffprint, "type error: value = %N", n);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_range_error(ErrorMgr_ptr self, node_ptr n, node_ptr var)
{
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  ErrorMgr_start_parsing_err(self);
  StreamMgr_nprint_error(self->streams, wffprint, "cannot assign value %N to variable %N", n, var);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_range_warning(ErrorMgr_ptr self, node_ptr n, node_ptr var)
{
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  StreamMgr_print_error(self->streams, "Warning: ");
  StreamMgr_nprint_error(self->streams, wffprint, "cannot assign value %N to variable %N", n, var);
  StreamMgr_print_error(self->streams, "\n");
}

void ErrorMgr_error_multiple_assignment(ErrorMgr_ptr self, node_ptr t1)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "multiply assigned: ");
  print_node(wffprint, err, t1);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_empty_range(ErrorMgr_ptr self, node_ptr name, int dim1, int dim2)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "empty range type %d..%d for ", dim1, dim2);
  print_node(wffprint, err, name);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_not_word_wsizeof(ErrorMgr_ptr self, node_ptr expr)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams,
          "sizeof operator applied to a non-word operand: ");
  print_node(wffprint, err, expr);
  StreamMgr_print_error(self->streams, "\n");
  ErrorMgr_nusmv_exit(self, 1);
}

void ErrorMgr_error_not_constant_extend_width(ErrorMgr_ptr self,
                                              const node_ptr expr)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams,
          "extend expression with non constant size specifier: ");
  print_node(wffprint, err, expr);
  StreamMgr_print_error(self->streams, "\n");
  ErrorMgr_nusmv_exit(self, 1);
}

void ErrorMgr_error_not_constant_resize_width(ErrorMgr_ptr self,
                                              const node_ptr expr)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams,
          "resize expression with non constant size specifier: ");
  print_node(wffprint, err, expr);
  StreamMgr_print_error(self->streams, "\n");
  ErrorMgr_nusmv_exit(self, 1);
}

void ErrorMgr_error_not_constant_wtoint(ErrorMgr_ptr self, const node_ptr expr)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams,
          "toint expression with non constant argument: ");
  print_node(wffprint, err, expr);
  StreamMgr_print_error(self->streams, "\n");
  ErrorMgr_nusmv_exit(self, 1);
}

void ErrorMgr_error_not_constant_width_of_word_type(ErrorMgr_ptr self,
                                                    node_ptr name)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "the width of a word variable ");
  print_node(wffprint, err, name);
  StreamMgr_print_error(self->streams, " is not a constant");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_not_constant_width_of_word_array_type(ErrorMgr_ptr self,
                                                          node_ptr name)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "at least one of the width of the Word Array variable ");
  print_node(wffprint, err, name);
  StreamMgr_print_error(self->streams, " is not a constant");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_not_constant_width_of_array_type(ErrorMgr_ptr self,
                                                     node_ptr name)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "at least one of the width of the Array variable ");
  print_node(wffprint, err, name);
  StreamMgr_print_error(self->streams, " is not a constant");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_wrong_word_operand(ErrorMgr_ptr self,
                                       const char* msg, node_ptr expr)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "Error: %s\n", msg);
  print_node(wffprint, err, expr);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_assign_both(ErrorMgr_ptr self, node_ptr v, node_ptr v1,
                                int lineno, int lineno2)
{
  FILE* err;
  extern int nusmv_yylineno;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  nusmv_yylineno = lineno;
  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "assigned ");
  print_node(wffprint, err,v);
  StreamMgr_print_error(self->streams,", line %d: assigned ", lineno2);
  print_node(wffprint, err, v1);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_unknown_var_in_order_file(ErrorMgr_ptr self, node_ptr n)
{
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  ErrorMgr_start_parsing_err(self);
  StreamMgr_nprint_error(self->streams, wffprint, "unknown variable in order file :%N", n);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_warning_variable_not_declared(ErrorMgr_ptr self, node_ptr vname)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  StreamMgr_flush_streams(self->streams); /* to flush all existing messages before outputting */
  StreamMgr_print_error(self->streams, "\n********   WARNING   ********\nThe variable: ");
  (void) print_node(wffprint, err, vname);
  StreamMgr_print_error(self->streams, "\nhas not been declared in the source file,\n");
  StreamMgr_print_error(self->streams, "but it appear in the input ordering file.\n");
  StreamMgr_print_error(self->streams, "Ignoring it.\n");
  StreamMgr_print_error(self->streams, "******** END WARNING ********\n");
}

void ErrorMgr_warning_missing_variable(ErrorMgr_ptr self, node_ptr vname)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  StreamMgr_flush_streams(self->streams); /* to flush all existing messages before outputting */
  StreamMgr_print_error(self->streams, "\n********   WARNING   ********\nThe variable: ");
  (void) print_node(wffprint, err, vname);
  StreamMgr_print_error(self->streams, "\nhas not been specified in the ordering file.\n");
  StreamMgr_print_error(self->streams, "It has been positioned at the end of the ordering.\n");
  StreamMgr_print_error(self->streams, "******** END WARNING ********\n");
}

void ErrorMgr_warning_missing_variables(ErrorMgr_ptr self,
                                        NodeList_ptr vars_list)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  StreamMgr_flush_streams(self->streams); /* to flush all existing messages before outputting */
  if (NodeList_get_length(vars_list) > 1) {
    StreamMgr_print_error(self->streams, "\n********   WARNING   ********\nThe variables: ");
  }
  else {
    StreamMgr_print_error(self->streams, "\n********   WARNING   ********\nThe variable: ");
  }

  NodeList_print_nodes(vars_list, wffprint, err);

  if (NodeList_get_length(vars_list) > 1) {
    StreamMgr_print_error(self->streams, "\nhave not been specified in the ordering file.\n");
    StreamMgr_print_error(self->streams, "They have been positioned at the end of the ordering.\n");
  }
  else {
    StreamMgr_print_error(self->streams, "\nhas not been specified in the ordering file.\n");
    StreamMgr_print_error(self->streams, "It has been positioned at the end of the ordering.\n");
  }

  StreamMgr_print_error(self->streams, "******** END WARNING ********\n");
}

void ErrorMgr_warning_non_ag_only_spec(ErrorMgr_ptr self, Prop_ptr prop)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(ENV_OBJECT(self)->environment, ENV_OPTS_HANDLER));


  ERROR_MGR_CHECK_INSTANCE(self);

  StreamMgr_flush_streams(self->streams); /* to flush all existing messages before outputting */
  StreamMgr_print_error(self->streams, "\n********   WARNING   ********\nThe ");
  print_spec(StreamMgr_get_error_ostream(self->streams),
             prop, get_prop_print_method(opts));
  StreamMgr_print_error(self->streams, "\nis not an AG-only formula: Skipped\n");
  StreamMgr_print_error(self->streams, "******** END WARNING ********\n");
}

void ErrorMgr_warning_ag_only_without_reachables(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  StreamMgr_flush_streams(self->streams); /* to flush all existing messages before outputting */
  StreamMgr_print_error(self->streams, "\n********   WARNING   ********\n");
  StreamMgr_print_error(self->streams,
          "AG-only requires the calculation of reachable states to be effective.\n" \
          "Since reeachable states have not been computed, the standard algorithm\n" \
          "will be used instead. \n");
  StreamMgr_print_error(self->streams, "******** END WARNING ********\n");
}

void ErrorMgr_warning_fsm_init_empty(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  ERROR_MGR_BEGIN_TAG("fsm_init_empty")
  StreamMgr_flush_streams(self->streams); /* to flush all existing messages before outputting */
  StreamMgr_print_error(self->streams, "\n********   WARNING   ********\n");
  StreamMgr_print_error(self->streams,
          "The initial states set of the finite state machine is empty.\n"
          "This might make results of model checking not trustable.\n");
  StreamMgr_print_error(self->streams, "******** END WARNING ********\n");
  ERROR_MGR_END_TAG
}

void ErrorMgr_warning_fsm_invar_empty(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  StreamMgr_flush_streams(self->streams); /* to flush all existing messages before outputting */
  StreamMgr_print_error(self->streams, "\n********   WARNING   ********\n");
  StreamMgr_print_error(self->streams,
          "The states set of the finite state machine is empty.\n"
          "This might make results of model checking not trustable.\n");
  StreamMgr_print_error(self->streams, "******** END WARNING ********\n");
}

void ErrorMgr_warning_fsm_fairness_empty(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  StreamMgr_flush_streams(self->streams); /* to flush all existing messages before outputting */
  StreamMgr_print_error(self->streams, "\n********   WARNING   ********\n");
  StreamMgr_print_error(self->streams,
          "Fair states set of the finite state machine is empty.\n"
          "This might make results of model checking not trustable.\n");
  StreamMgr_print_error(self->streams, "******** END WARNING ********\n");
}

void ErrorMgr_warning_fsm_init_and_fairness_empty(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  StreamMgr_flush_streams(self->streams); /* to flush all existing messages before outputting */
  StreamMgr_print_error(self->streams, "\n********   WARNING   ********\n");
  StreamMgr_print_error(self->streams,
          "The intersection of initial set of states and the set of fair states");
  StreamMgr_print_error(self->streams, " of the finite state machine is empty.\n"
          "This might make results of model checking not trustable.\n");
  StreamMgr_print_error(self->streams, "******** END WARNING ********\n");
}

void ErrorMgr_error_var_appear_twice_in_order_file(ErrorMgr_ptr self,
                                                   node_ptr n)
{
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  ErrorMgr_start_parsing_err(self);
  StreamMgr_nprint_error(self->streams, wffprint, "variable appears twice in order file:%N", n);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_warning_var_appear_twice_in_order_file(ErrorMgr_ptr self,
                                                     node_ptr n)
{
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  StreamMgr_flush_streams(self->streams); /* to flush all existing messages before outputting */
  StreamMgr_print_error(self->streams, "\n********   WARNING   ********\n");
  (void) StreamMgr_nprint_error(self->streams, wffprint, "variable appears twice in order file:%N", n);
}

void ErrorMgr_warning_id_appears_twice_in_idlist_file(ErrorMgr_ptr self,
                                                      node_ptr n)
{
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  StreamMgr_flush_streams(self->streams); /* to flush all existing messages before outputting */
  StreamMgr_print_error(self->streams, "\n********   WARNING   ********\n");
  (void) StreamMgr_nprint_error(self->streams, wffprint, "ID appears twice in id list file :%N\n", n);
}

void ErrorMgr_error_var_not_in_order_file(ErrorMgr_ptr self, node_ptr n)
{
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  ErrorMgr_start_parsing_err(self);
  StreamMgr_nprint_error(self->streams, wffprint, "not in order file: %N", n);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_not_proper_number(ErrorMgr_ptr self,
                                      const char* op, node_ptr n)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);

  StreamMgr_print_error(self->streams, "not suitable constant for operator '%s' : ", op);
  print_node(wffprint, err, n);

  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_not_proper_numbers(ErrorMgr_ptr self, const char* op,
                                       node_ptr n1, node_ptr n2)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);

  StreamMgr_print_error(self->streams, "not suitable constants for operator '%s' : ", op);
  print_node(wffprint, err, n1);
  StreamMgr_print_error(self->streams, " and ");
  print_node(wffprint, err, n2);

  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_ambiguous(ErrorMgr_ptr self, node_ptr s)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "\"");
  print_node(wffprint, err, s);
  StreamMgr_print_error(self->streams, "\" ambiguous");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_undefined(ErrorMgr_ptr self, node_ptr s)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "\"");
  print_node(wffprint, err, s);
  StreamMgr_print_error(self->streams, "\" undefined");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_shadowing(ErrorMgr_ptr self, node_ptr s)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "multiple declaration of identifier "
          "(previously declared as parameter): ");
  print_node(wffprint, err, s);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_redefining(ErrorMgr_ptr self, node_ptr s)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "multiple declaration of identifier: ");
  print_node(wffprint, err, s);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_redefining_operational_symbol(ErrorMgr_ptr self, node_ptr s)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "operational symbol \"");
  print_node(wffprint, err, s);
  StreamMgr_print_error(self->streams, "\" is redefined by a user");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_redefining_input_var(ErrorMgr_ptr self, node_ptr s)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "redefining or assigning input variable \"");
  print_node(wffprint, err, s);
  StreamMgr_print_error(self->streams, "\"");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_reassigning(ErrorMgr_ptr self, node_ptr s)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "variable is assigned more than once: ");
  print_node(wffprint, err, s);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_assign_input_var(ErrorMgr_ptr self, node_ptr s)
{
  FILE* err;
  node_ptr tmp = car(s);
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  if (node_get_type(s) == SMALLINIT) {
    StreamMgr_print_error(self->streams, "illegal assignment of initial value to input variable \"");
  }
  else if (node_get_type(s) == NEXT) {
    StreamMgr_print_error(self->streams, "illegal assignment of next value to input variable \"");
  }
  else {
    StreamMgr_print_error(self->streams, "illegal assignment of value to input variable \"");
    tmp = s;
  }
  print_node(wffprint, err, tmp);
  StreamMgr_print_error(self->streams, "\"");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_assign_frozen_var(ErrorMgr_ptr self, node_ptr s)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  nusmv_assert(node_get_type(s) != SMALLINIT); /* init assign is legal=>cannot be here*/
  StreamMgr_print_error(self->streams, "illegal assignment to frozen variable \"");
  print_node(wffprint, err, NEXT == node_get_type(s) ? car(s) : s);
  StreamMgr_print_error(self->streams, "\". Assignment of an initial value only is allowed.");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_assign_expected_var(ErrorMgr_ptr self, node_ptr s)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  nusmv_assert(EQDEF == node_get_type(s));
  StreamMgr_print_error(self->streams, "A variable is expected in left-hand-side of assignment:\n");
  print_node(wffprint, err, s);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_circular(ErrorMgr_ptr self, node_ptr s)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "recursively defined: ");
  print_node(wffprint, err, s);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_too_many_vars(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "too many variables");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_out_of_memory(ErrorMgr_ptr self, size_t size)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  StreamMgr_flush_streams(self->streams); /* to flush all existing messages before outputting */
  (void) StreamMgr_print_error(self->streams,
                 "\n##################################################\n");
  (void) StreamMgr_print_error(self->streams,
                 "### Out of memory allocating %" PRIuPTR " bytes\n", size);
  (void) StreamMgr_print_error(self->streams,
                 "##################################################\n");
  ErrorMgr_nusmv_exit(self, 1);
}

void ErrorMgr_error_invalid_subrange(ErrorMgr_ptr self, node_ptr range)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "Invalid subrange: ");
  print_node(wffprint, err, range);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_invalid_bool_cast(ErrorMgr_ptr self, node_ptr expr)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams,
          "Invalid boolean cast (Expected word1, boolean"
          " or integer expression): ");
  print_node(wffprint, err, expr);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_out_of_bounds_word_toint_cast(ErrorMgr_ptr self,
                                                  node_ptr expr)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams,
          "Invalid word to int cast. (Max word width is 32 for signed words, "
          "31 for unsigned words): ");
  print_node(wffprint, err, expr);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_invalid_toint_cast(ErrorMgr_ptr self, node_ptr expr)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams,
          "Invalid integer cast (Expected word, boolean"
          " or integer expression): ");
  print_node(wffprint, err, expr);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_invalid_count_operator(ErrorMgr_ptr self, node_ptr expr)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "Expected boolean expressions in count operator: ");
  print_node(wffprint, err, expr);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_invalid_enum_value(ErrorMgr_ptr self, node_ptr value)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "Invalid enumerative value: ");
  print_node(wffprint, err, value);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_game_definition_contains_input_vars(ErrorMgr_ptr self,
                                                        node_ptr var_name)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "The game difinition contains an input variable: ");
  print_node(wffprint, err, var_name);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_property_contains_input_vars(ErrorMgr_ptr self,
                                                 Prop_ptr prop)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "Property contains input variables:\n");
  Prop_print_db(prop, StreamMgr_get_error_ostream(self->streams),
                PROPDB_PRINT_FMT_DEFAULT);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_assign_exp_contains_input_vars(ErrorMgr_ptr self,
                                                   node_ptr exp)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "Assignment of ");
  print_node(wffprint, err, exp);
  StreamMgr_print_error(self->streams, " contains input variables, which is not allowed.");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_next_exp_contains_input_vars(ErrorMgr_ptr self,
                                                 node_ptr exp)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "\"next\" expression contains input variables:\n");
  print_node(wffprint, err, exp);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_invar_exp_contains_input_vars(ErrorMgr_ptr self,
                                                  node_ptr exp)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "INVAR expression contains input variables:\n");
  print_node(wffprint, err, exp);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_init_exp_contains_input_vars(ErrorMgr_ptr self, node_ptr exp)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "INIT expression contains input variables:\n");
  print_node(wffprint, err, exp);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_second_player_var(ErrorMgr_ptr self, node_ptr exp)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "Prohibited use of a variable of "
          "the second player in the expressions of the first player : ");
  print_node(wffprint, err, exp);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_second_player_next_var(ErrorMgr_ptr self, node_ptr exp)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "Prohibited use of a (next-state) variable of "
          "the second player in the expressions of the first player : ");
  print_node(wffprint, err, exp);
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_unknown_preprocessor(ErrorMgr_ptr self,
                                         const char* prep_name)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  StreamMgr_print_error(self->streams, "Unknown preprocessor: %s\n", prep_name);
  ErrorMgr_nusmv_exit(self, 1);
}

void ErrorMgr_error_set_preprocessor(ErrorMgr_ptr self,
                                     const char* name,
                                     boolean is_warning)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  StreamMgr_print_error(self->streams,
                        "Preprocessor file %s does not exist.\n", name);
  if(false == is_warning)
    ErrorMgr_nusmv_exit(self, 1);
}

void ErrorMgr_error_type_system_violation(ErrorMgr_ptr self)
{
  extern int nusmv_yylineno;

  ERROR_MGR_CHECK_INSTANCE(self);

  nusmv_yylineno = 0; /* to suppress line-number output */
  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "Type System Violation detected\n");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_psl_not_supported_feature(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "The specified PSL property requires features " \
          "that are not implemented yet.\n");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_psl_not_supported_feature_next_number(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  StreamMgr_print_error(self->streams, "expr in next*[expr] must be a constant positive "\
          "integer number.\n");
  ErrorMgr_error_psl_not_supported_feature(self);
}

void ErrorMgr_error_not_supported_feature(ErrorMgr_ptr self, const char* msg)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  ErrorMgr_start_parsing_err(self);

  StreamMgr_print_error(self->streams, "Error: unsupported feature.\n");
  StreamMgr_print_error(self->streams, "The requested feature has not been implemented yet:\n ");
  StreamMgr_print_error(self->streams, "%s", msg);
  StreamMgr_print_error(self->streams, "\n");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_expected_number(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "A number was expected but not found.\n");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_warning_psl_not_supported_feature(ErrorMgr_ptr self,
                                                node_ptr psl_spec, int index)
{
  FILE* err;
  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;

  ERROR_MGR_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  err = StreamMgr_get_error_stream(self->streams);

  StreamMgr_flush_streams(self->streams); /* to flush all existing messages before outputting */
  StreamMgr_print_error(self->streams, "\n********   WARNING   ********\n");
  StreamMgr_print_error(self->streams, "The following PSL property:\n");
  print_node(wffprint, err, psl_spec);
  StreamMgr_print_error(self->streams, "\nrequires features that are not yet available.\n");
  StreamMgr_print_error(self->streams,
          "The property will be inserted in the properties database at index %d,\n",
          index);
  StreamMgr_print_error(self->streams,
          "but the user will be not allowed to perform any model checking on it.\n");
  StreamMgr_print_error(self->streams,
          "Furthermore, no type nor definitions checking will be performed\n" \
          "on this property.\n");
  StreamMgr_print_error(self->streams, "******** END WARNING ********\n");
}

void ErrorMgr_error_psl_repeated_replicator_id(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  ErrorMgr_start_parsing_err(self);
  StreamMgr_print_error(self->streams, "Nested forall contains an ID that is "\
          "already used by an outer forall.\n");
  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_invalid_number(ErrorMgr_ptr self, const char* szNumber)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  if (szNumber != (const char*) NULL) {
    StreamMgr_print_error(self->streams,
            "Error: string \"%s\" is not valid. An integer was expected.\n",
            szNumber);
  }
  else {
    StreamMgr_print_error(self->streams,
            "Error: you have given an empty string when integer was "\
            "expected.\n");
  }
}

void ErrorMgr_error_bmc_invalid_k_l(ErrorMgr_ptr self,const int k, const int l)
{
  char buf[16];

  ERROR_MGR_CHECK_INSTANCE(self);

  Bmc_Utils_ConvertLoopFromInteger(l, buf, sizeof(buf));
  StreamMgr_print_error(self->streams,
          "Error: length=%d and loopback=%s are not compatible.\n",
          k, buf);
}

void ErrorMgr_error_property_already_specified(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  StreamMgr_print_error(self->streams,
          "Error: It is not possible to specify multiple properties to be" \
          " checked \n");
  StreamMgr_print_error(self->streams,
          "      using \"-p <formula>\" and/or \"-n <property_idx>\" " \
          "options\n\n");
}

void ErrorMgr_error_invalid_numeric_value(ErrorMgr_ptr self,
                                          int value, const char* reason)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  ErrorMgr_start_parsing_err(self);

  StreamMgr_print_error(self->streams,
          "Error: invalid numeric value: %d\n", value);
  if (reason != (char*) NULL) {
    StreamMgr_print_error(self->streams, "%s", reason);
    StreamMgr_print_error(self->streams, "\n");
  }

  ErrorMgr_finish_parsing_err(self);
}

void ErrorMgr_error_file_not_found(ErrorMgr_ptr self, const char* filename)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  StreamMgr_print_error(self->streams,
          "Error: file '%s' not found.\n", filename);
  ErrorMgr_nusmv_exit(self, 1);
}

void ErrorMgr_error_file_clobbering(ErrorMgr_ptr self, const char* filename)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  StreamMgr_print_error(self->streams,
          "Error: will not overwrite file '%s'.\n", filename);
  ErrorMgr_nusmv_exit(self, 1);
}

void ErrorMgr_warning_processes_deprecated(ErrorMgr_ptr self)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  StreamMgr_print_error(self->streams,
          "WARNING *** Processes are still supported, but deprecated.      ***\n");
  StreamMgr_print_error(self->streams,
          "WARNING *** In the future processes may be no longer supported. ***\n\n");
}

node_ptr ErrorMgr_failure_make(ErrorMgr_ptr self, const char* msg,
                               FailureKind kind, int lineno)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    return find_node(nodemgr, FAILURE,
                     find_node(nodemgr, COLON,
                               (node_ptr)  UStringMgr_find_string(self->strings,
                                                                  (char*) msg),
                               (node_ptr) kind),
                     NODE_FROM_INT(lineno));
  }
}

const char* ErrorMgr_failure_get_msg(ErrorMgr_ptr self, node_ptr failure)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  if (failure != (node_ptr) NULL &&

  node_get_type(failure) == FAILURE &&
      car(failure) != (node_ptr) NULL &&
      node_get_type(car(failure)) == COLON)
    return (const char*)  UStringMgr_get_string_text((string_ptr) car(car(failure)));
  return "Unknown";
}

FailureKind ErrorMgr_failure_get_kind(ErrorMgr_ptr self, node_ptr failure)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  if (failure != (node_ptr) NULL &&
      node_get_type(failure) == FAILURE &&
      car(failure) != Nil &&
      node_get_type(car(failure)) == COLON)
    return (FailureKind) cdr(car(failure));
  return FAILURE_UNSPECIFIED;
}

int ErrorMgr_failure_get_lineno(ErrorMgr_ptr self, node_ptr failure)
{
  ERROR_MGR_CHECK_INSTANCE(self);

  nusmv_assert(failure != (node_ptr) NULL &&
               node_get_type(failure) == FAILURE &&
               cdr(failure) != Nil);
  return NODE_TO_INT(cdr(failure));
}

void ErrorMgr_enable_tag(ErrorMgr_ptr self, const char* tag)
{
  TagInfo* taginfo;
  string_ptr utag;

  ERROR_MGR_CHECK_INSTANCE(self);

  utag = UStringMgr_find_string(self->strings, tag);
  taginfo = (TagInfo*)find_assoc(self->tags, NODE_PTR(utag));

  if (NULL != taginfo) {
    taginfo->enabled = true;
  }
}

void ErrorMgr_disable_tag(ErrorMgr_ptr self, const char* tag)
{
  TagInfo* taginfo;
  string_ptr utag;

  ERROR_MGR_CHECK_INSTANCE(self);

  utag = UStringMgr_find_string(self->strings, tag);
  taginfo = (TagInfo*)find_assoc(self->tags, NODE_PTR(utag));

  if (NULL == taginfo) {
    taginfo = ALLOC(TagInfo, 1);
    TagInfo_init(taginfo);
    insert_assoc(self->tags, NODE_PTR(utag), NODE_PTR(taginfo));
  }

  taginfo->enabled = false;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The ErrorMgr class private initializer

  The ErrorMgr class private initializer

  \sa ErrorMgr_create
*/
static void error_mgr_init(ErrorMgr_ptr self,
                           const NuSMVEnv_ptr env)
{
  env_object_init(ENV_OBJECT(self), env);

  /* members initialization */
  self->jmp_buf_pos = 0;
  self->strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
  self->options = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));;
  self->streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));;

  self->io_atom_stack = Stack_create();

  self->the_node = Nil;

  self->tags = new_assoc();

  OVERRIDE(Object, finalize) = error_mgr_finalize;
}

/*!
  \brief The ErrorMgr class virtual finalizer

  Called by the class destructor
*/
static void error_mgr_finalize(Object_ptr object, void* dummy)
{
  ErrorMgr_ptr self = ERROR_MGR(object);

  error_mgr_deinit(self);
  FREE(self);
}

/*!
  \brief The ErrorMgr class private deinitializer

  The ErrorMgr class private deinitializer

  \sa ErrorMgr_destroy
*/
static void error_mgr_deinit(ErrorMgr_ptr self)
{
  /* members deinitialization */
  Stack_destroy(self->io_atom_stack);

  error_mgr_deinit_tags(self);
  self->tags = NULL;

  env_object_deinit(ENV_OBJECT(self));
}

/*!
  \brief TagInfo initializer
  TagInfo initializer
*/
static void TagInfo_init(TagInfo* self)
{
  self->enabled = true;
}

/*!
  \brief Deinitializer for tags field
*/
static void error_mgr_deinit_tags(ErrorMgr_ptr self)
{
  clear_assoc_and_free_entries(self->tags, error_mgr_clear_tags);
  free_assoc(self->tags);
}

/*!
  \brief callback for error_mgr_deinit_tags
*/
static enum st_retval error_mgr_clear_tags(char* key, char* value, char* arg)
{
  UNUSED_PARAM(key);
  UNUSED_PARAM(arg);

  FREE(value);

  return ST_DELETE;
}

/*!
  \brief
*/
static boolean error_mgr_is_tag_enabled(ErrorMgr_ptr self,
                                        string_ptr tag)
{
  TagInfo* taginfo;

  taginfo = (TagInfo*)find_assoc(self->tags, NODE_PTR(tag));

  /* tags are enabled by default */
  if (NULL == taginfo) return true;
  else return taginfo->enabled;
}


/**AutomaticEnd***************************************************************/
