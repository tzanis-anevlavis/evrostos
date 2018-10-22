/* ---------------------------------------------------------------------------


   This file is part of the ``compile.symb_table'' package of NuSMV
   version 2.  Copyright (C) 2005 by FBK-irst.

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
  \author Andrei Tchaltsev
  \brief Implementation of functions dealing with the type of
   variables in a symbol table.

  \todo: Missing description

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include <stdarg.h>

#include "nusmv/core/compile/symb_table/SymbType.h"
#include "nusmv/core/compile/symb_table/SymbType_private.h"
#include "nusmv/core/compile/symb_table/symb_table.h"
#include "nusmv/core/compile/symb_table/symb_table_int.h"
#include "nusmv/core/compile/symb_table/NFunction.h"

#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/WordNumberMgr.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/EnvObject_private.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct SymbType_TAG {
  INHERITS_FROM(EnvObject);

  SymbTypeTag tag;
  node_ptr body;
  boolean isMemorySharingInstance;
} SymbType;


/*!
  \brief Data structure used to store information needed for printout.



  \sa SymbType_print, SymbType_sprint
*/

typedef struct SymbTypePrinterInfo_TAG
{
  /* The file stream used in case of file printout */
  FILE* file_stream;

  /* The string buffer (initilally NULL) to be used in case of string printout */
  char* string_buffer;

  /* The string buffer length */
  unsigned buffer_size;

  /* The string buffer remaining space */
  unsigned remaining_space;

  /* Enumeration used to switch between file and string printout*/
  enum {STRING_OUTPUT, FILE_OUTPUT} output_type;
} SymbTypePrinterInfo;

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef SymbTypePrinterInfo* SymbTypePrinterInfo_ptr;

/*---------------------------------------------------------------------------*/
/* macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief The initial buffer size used when printing on a string



  \sa SymbType_print, SymbType_sprint
*/
#define INITIAL_PRINT_BUFFER_SIZE 1000

/*!
  \brief  Initialize the given SymbTypePrinterInfo to print on a
                  string



  \sa SymbType_print, SymbType_sprint
*/
#define INITIALIZE_PRINTER_INFO_FOR_STRING(pinfo)                   \
  pinfo.output_type = STRING_OUTPUT;                                \
  pinfo.buffer_size = INITIAL_PRINT_BUFFER_SIZE;                    \
  pinfo.remaining_space = INITIAL_PRINT_BUFFER_SIZE;                \
  pinfo.string_buffer = ALLOC(char, INITIAL_PRINT_BUFFER_SIZE + 1); \
  *(pinfo.string_buffer) = '\0'

/*!
  \brief  Initialize the given SymbTypePrinterInfo to print on a
                  file



  \sa SymbType_print, SymbType_sprint
*/
#define INITIALIZE_PRINTER_INFO_FOR_FILE(pinfo, file_ptr)               \
  pinfo.output_type = FILE_OUTPUT;                                      \
  pinfo.file_stream = file_ptr

/*!
  \brief  Clear the given SymbTypePrinterInfo



  \sa SymbType_print, SymbType_sprint
*/
#define CLEAR_PRINTER_INFO(pinfo)                                       \
  if(STRING_OUTPUT == pinfo.output_type) FREE(pinfo.string_buffer)


/*---------------------------------------------------------------------------*/
/* Constants declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static boolean
node_equal(node_ptr n1, node_ptr n2);

static void symb_type_sprint_aux(const SymbType_ptr self,
                                 MasterPrinter_ptr wffprint,
                                 SymbTypePrinterInfo_ptr printer_info);

static void str_print(const NuSMVEnv_ptr env,
                      SymbTypePrinterInfo_ptr printer_info,
                      const char *fmt, ...);

static void symb_type_finalize(Object_ptr object, void* dummy);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

SymbType_ptr SymbType_create(const NuSMVEnv_ptr env,
                             SymbTypeTag tag, node_ptr body)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  SymbType_ptr self = ALLOC(SymbType, 1);
  SYMB_TYPE_CHECK_INSTANCE(self);

  nusmv_assert(tag <= SYMB_TYPE_ERROR);
  /* array type have to be create with SymbType_create_array */
  nusmv_assert(SYMB_TYPE_ARRAY != tag);

  env_object_init(ENV_OBJECT(self), env);

  self->tag = tag;
  self->isMemorySharingInstance = false;

  if (SYMB_TYPE_ENUM == tag) {
    /* these is an enum type. Find out which kind of enum this is.
       It can be a pure integer enum, a pure symbolic enum
       or integer-symbolic enum.
    */
    enum Enum_types enumType = -1; /* for sure incorrect value */
    node_ptr iter = body;
    int thereIsInt = 0;
    int thereIsSymbolic = 0;
    while (iter != Nil) {
      switch (node_get_type(car(iter))) {
      case ATOM: /* symbolic constant */
      case BIT:
      case DOT:
      case ARRAY:
        thereIsSymbolic = 1;
        break;
      case NUMBER: /* integer numbers */
        thereIsInt = 1;
        break;

      case TRUEEXP:
      case FALSEEXP:
        /* TRUE and FALSE cannot be in a enum
        */
        error_unreachable_code();

      default: /* only numbers, true, false, and
                  symbolic constants can be in the constant-list (enum-type).
               */
        {
          const MasterPrinter_ptr wffprint =
            MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

          StreamMgr_nprint_error(streams, wffprint, "%N", body); StreamMgr_print_error(streams, "\n\n");
          error_unreachable_code();
        }
      }
      iter = cdr(iter);
    }/* while */
    /* there must be constant or symbolic constant in the enum */
    nusmv_assert(thereIsInt | thereIsSymbolic);

    enumType = (thereIsInt && thereIsSymbolic)
      ?  ENUM_TYPE_INT_SYMBOLIC
      : thereIsInt
      ? ENUM_TYPE_PURE_INT
      : ENUM_TYPE_PURE_SYMBOLIC;
    /* we add additional node to remember the kind of the enum-type */
    self->body = new_node(nodemgr, enumType, body, Nil);
  }
  else {

    /* except ENUM type only Word and WordArray can can have body */
    nusmv_assert((Nil != body) == (SYMB_TYPE_SIGNED_WORD == tag ||
                                   SYMB_TYPE_UNSIGNED_WORD == tag ||
                                   SYMB_TYPE_WORDARRAY == tag ||
                                   SYMB_TYPE_INTARRAY == tag));
    /* usual type => just remember the body */
    self->body = body;
  }

  OVERRIDE(Object, finalize) = symb_type_finalize;

  return self;
}

SymbType_ptr SymbType_create_array(SymbType_ptr subtype,
                                   int lower_bound, int upper_bound)
{
  NuSMVEnv_ptr env;
  NodeMgr_ptr nodemgr;

  SymbType_ptr self = ALLOC(SymbType, 1);
  SYMB_TYPE_CHECK_INSTANCE(self);

  nusmv_assert(NULL != subtype);
  nusmv_assert(upper_bound >= lower_bound);

  env = EnvObject_get_environment(ENV_OBJECT(subtype));
  nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  env_object_init(ENV_OBJECT(self), env);

  self->tag = SYMB_TYPE_ARRAY;
  self->isMemorySharingInstance = false;
  /* the body of the array type is a list
     CONS(sub_type, CONS(lower bound, upper bound)) */
  self->body = cons(nodemgr, NODE_PTR(subtype),
                    cons(nodemgr, NODE_FROM_INT(lower_bound),
                                  NODE_FROM_INT(upper_bound)));

  OVERRIDE(Object, finalize) = symb_type_finalize;

  return self;
}

SymbType_ptr SymbType_create_nfunction(const NuSMVEnv_ptr env,
                                       NFunction_ptr body)
{
  NodeMgr_ptr nodemgr;

  SymbType_ptr self = ALLOC(SymbType, 1);
  SYMB_TYPE_CHECK_INSTANCE(self);

  nusmv_assert(NULL != body);

  nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  env_object_init(ENV_OBJECT(self), env);

  self->tag = SYMB_TYPE_NFUNCTION;
  self->isMemorySharingInstance = false;
  /* the body of the nfunction type is an NFunction */
  self->body = NODE_PTR(body);

  OVERRIDE(Object, finalize) = symb_type_finalize;

  return self;
}

void SymbType_destroy(SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  Object_destroy(OBJECT(self), NULL);
}

SymbType_ptr SymbType_copy(const SymbType_ptr self)
{
  NuSMVEnv_ptr env;

  SYMB_TYPE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));

  if (SYMB_TYPE_ARRAY == self->tag) {
    /* array type has a special constructor */
   return SymbType_create_array(SymbType_copy(SymbType_get_array_subtype(self)),
                                SymbType_get_array_lower_bound(self),
                                SymbType_get_array_upper_bound(self));
  }

  if (SYMB_TYPE_NFUNCTION == self->tag) {
    /* function type has a special constructor */
    return SymbType_create_nfunction(env,
                                     NFunction_copy(SymbType_get_nfunction_type(self)));
  }

  return SymbType_create(env, self->tag,
                         self->tag == SYMB_TYPE_ENUM
                         ? car(self->body) : self->body);
}

SymbType_ptr
SymbType_create_memory_sharing_type(const NuSMVEnv_ptr env,
                                    SymbTypeTag tag, node_ptr body)
{
  SymbType_ptr self = SymbType_create(env, tag, body);
  self->isMemorySharingInstance = true;
  return self;
}

SymbType_ptr
SymbType_create_memory_sharing_array_type(SymbType_ptr subtype,
                                          int lower_bound, int upper_bound)
{
  SymbType_ptr self;
  nusmv_assert(subtype->isMemorySharingInstance);

  self = SymbType_create_array(subtype, lower_bound, upper_bound);
  self->isMemorySharingInstance = true;
  return self;
}

void SymbType_destroy_memory_sharing_type(SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  /* this is a memory sharing instance */
  nusmv_assert(self->isMemorySharingInstance);

  /* array type is destroyed separately because we should not allow
     SymbType_destroy to go recursively down to subtype which
     may be already destroyed.

     NOTE FOR DEVELOPERS: keep implementation of this function and
     SymbType_destroy the same.
  */
  if (SYMB_TYPE_ARRAY == self->tag) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    /* debugging code to detect double freeing */
    nusmv_assert(self->body != NODE_FROM_INT(-1));

    /* for array_type 2 nodes are to be deleted */
    free_node(nodemgr, cdr(self->body));
    free_node(nodemgr, self->body);

    /* debugging code to detect double freeing */
    self->body = NODE_FROM_INT(-1);

    FREE(self);
    return;
  }

  self->isMemorySharingInstance = false;
  SymbType_destroy(self);
}

SymbTypeTag SymbType_get_tag(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  return self->tag;
}

boolean SymbType_is_enum(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  return SYMB_TYPE_ENUM == self->tag;
}

boolean SymbType_is_boolean(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  return SYMB_TYPE_BOOLEAN == self->tag;
}

boolean SymbType_is_integer(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  return SYMB_TYPE_INTEGER == self->tag;
}

boolean SymbType_is_real(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  return SYMB_TYPE_REAL == self->tag;
}

boolean SymbType_is_continuous(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);

  return SYMB_TYPE_CONTINUOUS == self->tag;
}

boolean SymbType_is_pure_int_enum(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  return SYMB_TYPE_ENUM == self->tag &&
    ENUM_TYPE_PURE_INT == node_get_type(self->body);
}

boolean SymbType_is_pure_symbolic_enum(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  return SYMB_TYPE_ENUM == self->tag &&
    ENUM_TYPE_PURE_SYMBOLIC == node_get_type(self->body);
}

boolean SymbType_is_int_symbolic_enum(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  return SYMB_TYPE_ENUM == self->tag &&
    ENUM_TYPE_INT_SYMBOLIC == node_get_type(self->body);
}

boolean SymbType_is_word_1(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  /* expects the body of Word type to be NUMBER */
  return SYMB_TYPE_UNSIGNED_WORD == self->tag && 1 == node_get_int(self->body);
}

boolean SymbType_is_unsigned_word(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  /* expects the body of Word type to be NUMBER */
  return SYMB_TYPE_UNSIGNED_WORD == self->tag;
}

boolean SymbType_is_signed_word(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  /* expects the body of Word type to be NUMBER */
  return SYMB_TYPE_SIGNED_WORD == self->tag;
}

boolean SymbType_is_word(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  /* expects the body of Word type to be NUMBER */
  return SYMB_TYPE_UNSIGNED_WORD == self->tag ||
    SYMB_TYPE_SIGNED_WORD == self->tag;
}

boolean SymbType_is_set(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  return SYMB_TYPE_SET_BOOL == self->tag ||
    SYMB_TYPE_SET_INT == self->tag ||
    SYMB_TYPE_SET_SYMB == self->tag ||
    SYMB_TYPE_SET_INT_SYMB == self->tag;
}

boolean SymbType_is_function(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  return SYMB_TYPE_NFUNCTION == self->tag;
}

boolean SymbType_is_error(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  return SYMB_TYPE_ERROR == self->tag;
}

boolean SymbType_is_statement(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);

  return SYMB_TYPE_STATEMENT == self->tag;
}

boolean SymbType_is_infinite_precision(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);

  return
    SYMB_TYPE_INTEGER == self->tag ||
    SYMB_TYPE_REAL == self->tag ||
    SYMB_TYPE_CONTINUOUS == self->tag;
}

boolean SymbType_is_array(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);

  return SYMB_TYPE_ARRAY == self->tag;
}

boolean SymbType_is_wordarray(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);

  return SYMB_TYPE_WORDARRAY == self->tag;
}

boolean SymbType_is_intarray(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);

  return SYMB_TYPE_INTARRAY == self->tag;
}

int SymbType_calculate_type_size(const SymbType_ptr self)
{
  int size = -1; /* for sure wrong value */

  SYMB_TYPE_CHECK_INSTANCE(self);
  nusmv_assert(!self->isMemorySharingInstance); /* must not be memory-shared */

  if (SYMB_TYPE_BOOLEAN == self->tag) {
    return 1;
  }
  else if (SYMB_TYPE_ENUM == self->tag) {
    int num = llength(SymbType_get_enum_type_values(self));
    nusmv_assert(0 != num); /* enum type cannot be without values */

    /* special case: enumeration of 1 value. Inside NuSMV this never happens
       but with external tools it is possible */
    if (num == 1) return 1;

    /* N values are encoded with numbers 0..N-1, i.e. it is necessary
       to have bits enough to represent N-1 as unsigned
       integer */
    num -= 1;

    size = 0;
    while (num != 0) { /* calculate logarithm */
      num = num >>1;
      ++size;
    }
  }
  else if (SYMB_TYPE_UNSIGNED_WORD == self->tag ||
           SYMB_TYPE_SIGNED_WORD == self->tag) {
    size = SymbType_get_word_width(self);
  }
  else {
    error_unreachable_code();/* only enum and word can be valid variable type */
  }

  return size;
}

node_ptr SymbType_generate_all_word_values(const SymbType_ptr self)
{
  NuSMVEnv_ptr env;
  NodeMgr_ptr nodemgr;
  WordNumberMgr_ptr words;
  /* create all the constants for the Word type */
  int width;
  unsigned long long value;
  node_ptr list;

  /* the function is implemented only for unsigned words */
  SYMB_TYPE_CHECK_INSTANCE(self);
  nusmv_assert(SYMB_TYPE_UNSIGNED_WORD == self->tag);
  /* All assertions are checked in the function SymbType_get_word_width */
  width = SymbType_get_word_width(self);
  nusmv_assert(width <= WordNumber_max_width());

  env = EnvObject_get_environment(ENV_OBJECT(self));
  nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  words = WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

  list = Nil;
  /* create the max value. Two left shifts are used because in C
     shifting by a full width is not allowed
  */
  value = ~((~0ull) << (width - 1) << 1);
  ++value;
  do {
    --value;
    list = cons(nodemgr, find_node(nodemgr, NUMBER_UNSIGNED_WORD,
                          (node_ptr) WordNumberMgr_integer_to_word_number(words, value, width),
                          Nil),
                list);
  }
  while ( value > 0);
  return list;
}

int SymbType_get_word_width(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  nusmv_assert((SYMB_TYPE_UNSIGNED_WORD == self->tag ||
                SYMB_TYPE_SIGNED_WORD == self->tag
                ) &&
               node_get_type(self->body) == NUMBER);
  return node_get_int(self->body); /* the width */
}

int SymbType_get_wordarray_awidth(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);

  nusmv_assert(SYMB_TYPE_WORDARRAY == self->tag &&
               node_get_type(self->body) == CONS &&
               node_get_type(car(self->body)) == NUMBER);

  return node_get_int(car(self->body)); /* the address width */
}

SymbType_ptr SymbType_get_wordarray_subtype(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);

  /* subtype is the second element in body list : see the array constructor */
  return SYMB_TYPE(cdr(self->body));
}

int SymbType_get_word_line_number(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  nusmv_assert(( ( SYMB_TYPE_UNSIGNED_WORD == self->tag ||
                   SYMB_TYPE_SIGNED_WORD == self->tag
                   ) &&
                 node_get_type(self->body) == NUMBER
                 ) ||
               ( SYMB_TYPE_WORDARRAY == self->tag &&
                 node_get_type(self->body) == CONS));
  nusmv_assert(!self->isMemorySharingInstance); /* type is not memory-shared */
  return node_get_lineno(self->body);
}

node_ptr SymbType_get_enum_type_values(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  nusmv_assert(SYMB_TYPE_ENUM == self->tag);
  nusmv_assert(!self->isMemorySharingInstance); /* type is not memory-shared */
  /* for the enum-type we added one addional node => skip it now */
  return car(self->body);
}

NFunction_ptr SymbType_get_nfunction_type(const SymbType_ptr self)
{
  SYMB_TYPE_CHECK_INSTANCE(self);
  nusmv_assert(SYMB_TYPE_NFUNCTION == self->tag);
  nusmv_assert(!self->isMemorySharingInstance); /* type is not memory-shared */

  return N_FUNCTION(self->body);
}

SymbType_ptr SymbType_get_array_subtype(const SymbType_ptr self) {
  nusmv_assert(SymbType_is_array(self));
  /* subtype is the first element in body list : see the array constructor */
  return SYMB_TYPE(car(self->body));
}

int SymbType_get_array_lower_bound(const SymbType_ptr self) {
  nusmv_assert(SymbType_is_array(self));
  /* low bound is the second element in body list : see the array constructor */
  return NODE_TO_INT(car(cdr(self->body)));
}

int SymbType_get_array_upper_bound(const SymbType_ptr self) {
  nusmv_assert(SymbType_is_array(self));
  /* up bound is the third element in body list : see the array constructor */
  return NODE_TO_INT(cdr(cdr(self->body)));
}

SymbType_ptr SymbType_get_intarray_subtype(const SymbType_ptr self) {
  nusmv_assert(SymbType_is_intarray(self));
  /* subtype is the first element in body list : see the intarray constructor */
  return SYMB_TYPE(car(self->body));
}

void SymbType_print(const SymbType_ptr self,
                    MasterPrinter_ptr printer,
                    FILE* output_stream)
{
  SymbTypePrinterInfo printer_info;

  INITIALIZE_PRINTER_INFO_FOR_FILE(printer_info, output_stream);
  symb_type_sprint_aux(self, printer, &printer_info);
  CLEAR_PRINTER_INFO(printer_info);
}

char* SymbType_sprint(const SymbType_ptr self,
                      MasterPrinter_ptr printer)
{
  SymbTypePrinterInfo printer_info;
  char* res;

  INITIALIZE_PRINTER_INFO_FOR_STRING(printer_info);

  symb_type_sprint_aux(self, printer, &printer_info);
  res = util_strsav(printer_info.string_buffer);
  CLEAR_PRINTER_INFO(printer_info);

  return res;
}

boolean SymbType_is_back_comp(const SymbType_ptr type)
{
  SYMB_TYPE_CHECK_INSTANCE(type);

  return SYMB_TYPE_BOOLEAN == type->tag ||
    SYMB_TYPE_ENUM == type->tag ||
    SYMB_TYPE_INTEGER == type->tag;
}

SymbType_ptr SymbType_get_greater(const SymbType_ptr type1,
                                  const SymbType_ptr type2)
{
  SymbType_ptr returnType;

  returnType = SymbType_convert_right_to_left(type1, type2);
  if (returnType) return returnType; /* conversion was successful */
  /* return with swapped types and return whatever is obtained */
  return SymbType_convert_right_to_left(type2, type1);
}

SymbType_ptr
SymbType_convert_right_to_left(SymbType_ptr leftType,
                               SymbType_ptr rightType)
{
  SYMB_TYPE_CHECK_INSTANCE(leftType);
  SYMB_TYPE_CHECK_INSTANCE(rightType);
  /* types have to be shared. So no worry about memory */
  nusmv_assert(leftType->isMemorySharingInstance &&
               rightType->isMemorySharingInstance);

  /* the same pointers, so, the same types, so, just return */
  if (leftType == rightType) return leftType;

  /* pointer are different => the types are different.
     Try to perform implicit convertion of the right operand to the left one.
     No need to care about word, word-array and no-type since
     they cannot be implicitly converted to any other type.
     NB: No-type is created for assignment, and symbolic (predicate-normalised)
     FSM may push assignments into case-expressions, for example.
  */

  /* real * int => real */
  if (leftType->tag == SYMB_TYPE_REAL) {
    return (rightType->tag == SYMB_TYPE_INTEGER)
      ? leftType : nullType;
  }

  /* int_symb_enum * (int or symb_enum) => int_symb_enum */
  if (SymbType_is_int_symbolic_enum(leftType)) {
    return (rightType->tag == SYMB_TYPE_INTEGER ||
            SymbType_is_pure_symbolic_enum(rightType))
      ? leftType : nullType;
  }

  /* continuous * real => continuous */
  /* continuous * integer => continuous */
  if (leftType->tag == SYMB_TYPE_CONTINUOUS) {
    return (rightType->tag == SYMB_TYPE_REAL ||
            rightType->tag == SYMB_TYPE_INTEGER)
      ? leftType : nullType;
  }

  /* arrays are compatible only with array of the same size and
     compatible subtypes */
  if (SymbType_is_array(leftType) || SymbType_is_array(rightType)) {
    if (!SymbType_is_array(leftType) ||
        !SymbType_is_array(rightType) ||
        SymbType_get_array_lower_bound(leftType)
          != SymbType_get_array_lower_bound(rightType) ||
        SymbType_get_array_upper_bound(leftType)
          != SymbType_get_array_upper_bound(rightType)) {
      return nullType;
    }

    {
      SymbType_ptr subLeft = SymbType_get_array_subtype(leftType);
      SymbType_ptr subRight = SymbType_get_array_subtype(rightType);
      SymbType_ptr subtype = SymbType_convert_right_to_left(subLeft, subRight);

      if (subtype == nullType)
        return nullType;

      /* create a proper memory shared array type */
      return SymbTablePkg_array_type(
          subtype,
          SymbType_get_array_lower_bound(leftType),
          SymbType_get_array_upper_bound(leftType));
    }
  }

  /* only set-types remain */

  /* int_symb_set * any set-type => int_symb_set  */
  if (leftType->tag == SYMB_TYPE_SET_INT_SYMB) {
    return SymbType_is_set(rightType) ? leftType : SYMB_TYPE(NULL);
  }

  /* all other combinations are illegal */
  return nullType;
}

SymbType_ptr
SymbType_get_minimal_common(SymbType_ptr type1,
                            SymbType_ptr type2)
{
  NuSMVEnv_ptr env;

  SYMB_TYPE_CHECK_INSTANCE(type1);
  SYMB_TYPE_CHECK_INSTANCE(type2);

  env = EnvObject_get_environment(ENV_OBJECT(type1));

  /* types have to be shared. So no worry about memory */
  nusmv_assert(type1->isMemorySharingInstance &&
               type2->isMemorySharingInstance);

  /* the same pointers, so, the same types, so, just return the type */
  if (type1 == type2) return type1;

  /* pointer are different => the types are different.
     Try to perform implicit convertion of both operand to
     a minimal type.
     No need to care about word, word-array and no-type since
     they cannot be implicitly converted to any other type.
     NB: No-type is created for assignment, and (predicate-normalised)
     symbolic FSM may push assignments into case-expressions, for example.
  */

  /* both are [Int or Real] -> Real */
  if ( ( type1->tag == SYMB_TYPE_INTEGER ||
         type1->tag == SYMB_TYPE_REAL ) &&
       ( type2->tag == SYMB_TYPE_INTEGER ||
         type2->tag == SYMB_TYPE_REAL ) ) {
    return SymbTablePkg_real_type(env);
  }

  /* One is Continuous and the other is [Int or Real] -> Continuous */
  else if ( ( SymbType_is_continuous(type1) &&
              SymbType_is_infinite_precision(type2) ) ||
            ( SymbType_is_continuous(type2) &&
              SymbType_is_infinite_precision(type1) ) ) {
    return SymbTablePkg_continuous_type(env);
  }

  /* both are [Int or Symbolic or SymbolicInt] -> SymbolicInt */
  else if ( ( type1->tag == SYMB_TYPE_INTEGER ||
              SymbType_is_pure_symbolic_enum(type1) ||
              SymbType_is_int_symbolic_enum(type1) ) &&
            ( type2->tag == SYMB_TYPE_INTEGER ||
              SymbType_is_pure_symbolic_enum(type2) ||
              SymbType_is_int_symbolic_enum(type2) ) ) {
    return SymbTablePkg_int_symbolic_enum_type(env);
  }

  /* arrays are compatible only with array of the same size and
     compatible subtypes */
  else if (SymbType_is_array(type1) || SymbType_is_array(type2)) {
    if (!SymbType_is_array(type1) ||
        !SymbType_is_array(type2) ||
        SymbType_get_array_lower_bound(type1)
        != SymbType_get_array_lower_bound(type2) ||
        SymbType_get_array_upper_bound(type1)
        != SymbType_get_array_upper_bound(type2)) {
      return SYMB_TYPE(NULL);
    }

    {
      SymbType_ptr sub1 = SymbType_get_array_subtype(type1);
      SymbType_ptr sub2 = SymbType_get_array_subtype(type2);
      SymbType_ptr subtype = SymbType_get_minimal_common(sub1, sub2);
      if (subtype == NULL)
        return SYMB_TYPE(NULL);

      /* create a proper memory shared array type */
      return SymbTablePkg_array_type(
          subtype,
          SymbType_get_array_lower_bound(type1),
          SymbType_get_array_upper_bound(type1));
    }
  }

  /* only set types remain */

  /* both are any set type => int_symb_set */
  else if (SymbType_is_set(type1) || SymbType_is_set(type2)) {
    return SymbTablePkg_integer_symbolic_set_type(env);
  }

  /* the given types are incompatible => error */
  else return SYMB_TYPE(NULL);
}

SymbType_ptr SymbType_make_set_type(const SymbType_ptr self)
{
  NuSMVEnv_ptr env;

  SYMB_TYPE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));

  /* converts to set types if it is required:
     bool -> bool_set
     int -> int_set
     symb -> symb_set
     int_symb_enum -> int_symb_set
     any set type -> the same set type
     other type -> NULL
  */

  /* WARNING [MD] Why SymbType_is_pure_int_enum is not considered here? */
  if (SymbType_is_boolean(self)) {
    return SymbTablePkg_boolean_set_type(env);
  }
  else if (self->tag == SYMB_TYPE_INTEGER) {
    return SymbTablePkg_integer_set_type(env);
  }
  else if (SymbType_is_pure_symbolic_enum(self)) {
    return SymbTablePkg_symbolic_set_type(env);
  }
  else if (SymbType_is_int_symbolic_enum(self)) {
    return SymbTablePkg_integer_symbolic_set_type(env);
  }
  else if (SymbType_is_set(self)) {
    return self;
  }
  else return nullType;
}

SymbType_ptr SymbType_make_from_set_type(const SymbType_ptr self)
{
  NuSMVEnv_ptr env;

  SYMB_TYPE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));

  if (self->tag == SYMB_TYPE_SET_BOOL) {
    return SymbTablePkg_boolean_type(env);
  }
  /* WARNING [MD] Why SymbType_is_pure_int_enum is not considered here? */
  else if (self->tag == SYMB_TYPE_SET_INT) {
    return SymbTablePkg_integer_type(env);
  }
  else if (self->tag == SYMB_TYPE_SET_SYMB) {
    return SymbTablePkg_pure_symbolic_enum_type(env);
  }
  else if (self->tag == SYMB_TYPE_SET_INT_SYMB) {
    return SymbTablePkg_int_symbolic_enum_type(env);
  }
  else return self;
}

SymbType_ptr SymbType_make_memory_shared(const SymbType_ptr self)
{
  SymbType_ptr res = self;
  NuSMVEnv_ptr env;

  SYMB_TYPE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));

  switch (self->tag) {
  case SYMB_TYPE_BOOLEAN:
    res = SymbTablePkg_boolean_type(env);
    break;

  case SYMB_TYPE_INTEGER:
    res = SymbTablePkg_integer_type(env);
    break;

  case SYMB_TYPE_REAL:
    res = SymbTablePkg_real_type(env);
    break;

  case SYMB_TYPE_CONTINUOUS:
    res = SymbTablePkg_continuous_type(env);
    break;

  case SYMB_TYPE_UNSIGNED_WORD:
    res = SymbTablePkg_unsigned_word_type(env, SymbType_get_word_width(self));
    break;

  case SYMB_TYPE_SIGNED_WORD:
    res = SymbTablePkg_signed_word_type(env, SymbType_get_word_width(self));
    break;

  case SYMB_TYPE_WORDARRAY:
    res =
      SymbTablePkg_wordarray_type(env,
                                  SymbType_get_wordarray_awidth(self),
                                  SymbType_make_memory_shared(SymbType_get_wordarray_subtype(self))
                                  );
    break;

  case SYMB_TYPE_ENUM:
    if (SymbType_is_pure_int_enum(self)) {
      /* RC: This was a tentative bug fix see 1435. It does not
         work as Integer type is largely used in the TC. In
         particular this fixes the mentioned issue 1435, but is
         breaks the whole type system, so that any expression like
         "scalar + 1" got broken. */
      /* res = SymbTablePkg_pure_int_enum_type(); */
      res = SymbTablePkg_integer_type(env); /* convert to Integer */
    }
    else if (SymbType_is_pure_symbolic_enum(self)) {
      res = SymbTablePkg_pure_symbolic_enum_type(env);
    }
    else {
      nusmv_assert(SymbType_is_int_symbolic_enum(self));
      res = SymbTablePkg_int_symbolic_enum_type(env);
    }
    break;

  case SYMB_TYPE_ARRAY:
    res = SymbTablePkg_array_type(
             SymbType_make_memory_shared(SymbType_get_array_subtype(self)),
             SymbType_get_array_lower_bound(self),
             SymbType_get_array_upper_bound(self));
    break;

  case SYMB_TYPE_INTARRAY:
    res = SymbTablePkg_intarray_type(env,
                                     SymbType_make_memory_shared(SymbType_get_intarray_subtype(self)));
    break;

  case SYMB_TYPE_NFUNCTION:
    error_unreachable_code_msg("Uninterpreted functions should be not converted to a memory shared type\n");
    break;

  default:
    error_unreachable_code(); /* unknown type */
  }

  return res;
}

boolean SymbType_is_memory_shared(SymbType_ptr self) {
  return self->isMemorySharingInstance;
}

boolean
SymbType_equals(SymbType_ptr self, SymbType_ptr oth)
{
  NuSMVEnv_ptr env;
  ErrorMgr_ptr errmgr;

  SYMB_TYPE_CHECK_INSTANCE(self);
  SYMB_TYPE_CHECK_INSTANCE(oth);

  if (self == oth)
    return true;

  env = EnvObject_get_environment(ENV_OBJECT(self));
  errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (self->tag == oth->tag) {
    if (SYMB_TYPE_ENUM == self->tag) {
      /*
         If the two types are exacly the same, i.e. the two body are
         exacly the same in term of list element. Order of elements is
         important.
      */
      return node_equal(car(self->body), car(oth->body));
    }
    else if ((SYMB_TYPE_SIGNED_WORD == self->tag) ||
             (SYMB_TYPE_UNSIGNED_WORD == self->tag)) {
      /* Sizes should be the same */
      return (SymbType_get_word_width(self) == SymbType_get_word_width(oth));
    }
    else if (SYMB_TYPE_WORDARRAY == self->tag) {
      return ((SymbType_get_wordarray_awidth(self) ==
               SymbType_get_wordarray_awidth(oth)) &&
               SymbType_equals(SymbType_get_wordarray_subtype(self),
                               SymbType_get_wordarray_subtype(oth)));
    }
    else if ( (SYMB_TYPE_INTEGER == self->tag) ||
              (SYMB_TYPE_CONTINUOUS == self->tag) ||
              (SYMB_TYPE_REAL == self->tag) ||
              (SYMB_TYPE_BOOLEAN == self->tag) ) {
      return true;
    }
    else if (SYMB_TYPE_INTARRAY == self->tag) {
      return SymbType_equals(SymbType_get_intarray_subtype(self),
                               SymbType_get_intarray_subtype(oth));
    }
    else if (SYMB_TYPE_ARRAY == self->tag) {
      if ((SymbType_get_array_lower_bound(self) ==
           SymbType_get_array_lower_bound(oth)) &&
          (SymbType_get_array_upper_bound(self) ==
           SymbType_get_array_upper_bound(oth))) {
        return SymbType_equals(SymbType_get_array_subtype(self),
                               SymbType_get_array_subtype(oth));
      }
    }
    else if (SYMB_TYPE_NFUNCTION == self->tag) {
      return NFunction_equals(SymbType_get_nfunction_type(self),
                              SymbType_get_nfunction_type(oth));
    }
    else {
      ErrorMgr_internal_error(errmgr, "SymbType_equals: Unknown type combination.");
      error_unreachable_code();
      return true;
    }
  }

  return false;
}

node_ptr SymbType_to_node(const SymbType_ptr self, NodeMgr_ptr nodemgr)
{
  SYMB_TYPE_CHECK_INSTANCE(self);

  switch (self->tag) {

  case SYMB_TYPE_NONE:
    return Nil;
    break;

  case SYMB_TYPE_STATEMENT:
    error_unreachable_code_msg("Not implemented yet");
    break;

  case SYMB_TYPE_BOOLEAN:
    return find_node(nodemgr, BOOLEAN, Nil, Nil);

  case SYMB_TYPE_ENUM:
      return find_node(nodemgr, SCALAR, car(self->body), Nil);

  case SYMB_TYPE_INTEGER:
    return find_node(nodemgr, INTEGER, Nil, Nil);

  case SYMB_TYPE_REAL:
    return find_node(nodemgr, REAL, Nil, Nil);

  case SYMB_TYPE_CONTINUOUS:
    return find_node(nodemgr, CONTINUOUS, Nil, Nil);

  case SYMB_TYPE_SIGNED_WORD:
    return find_node(nodemgr, SIGNED_WORD, self->body, Nil);

  case SYMB_TYPE_UNSIGNED_WORD:
    return find_node(nodemgr, UNSIGNED_WORD, self->body, Nil);

  case SYMB_TYPE_WORDARRAY:
  {
    SymbType_ptr subtype = SymbType_get_wordarray_subtype(self);
    int awidth = SymbType_get_wordarray_awidth(self);
    return find_node(nodemgr, WORDARRAY_TYPE,
                     find_node(nodemgr, NUMBER, NODE_FROM_INT(awidth), Nil),
                     SymbType_to_node(subtype, nodemgr)
                    );
  }

  case SYMB_TYPE_SET_BOOL:
  case SYMB_TYPE_SET_INT:
  case SYMB_TYPE_SET_SYMB:
  case SYMB_TYPE_SET_INT_SYMB:
    error_unreachable_code_msg("Not implemented yet");
    break;

  case SYMB_TYPE_ARRAY:
    {
      SymbType_ptr subtype = SYMB_TYPE(car(self->body));
      int lindex = NODE_TO_INT(car(cdr(self->body)));
      int rindex = NODE_TO_INT(cdr(cdr(self->body)));

      return find_node(nodemgr, ARRAY_TYPE,
                       find_node(nodemgr, TWODOTS,
                                 find_node(nodemgr, NUMBER, NODE_FROM_INT(lindex), Nil),
                                 find_node(nodemgr, NUMBER, NODE_FROM_INT(rindex), Nil)
                                 ),
                       SymbType_to_node(subtype, nodemgr)
                       );
    }
    break;

  case SYMB_TYPE_INTARRAY: {
    return find_node(nodemgr, INTARRAY_TYPE,
                     SymbType_to_node(SymbType_get_intarray_subtype(self),
                                      nodemgr), Nil);
  }

  case SYMB_TYPE_NFUNCTION:
    {
      NFunction_ptr nfunction = SymbType_get_nfunction_type(self);
      node_ptr rtype = SymbType_to_node(NFunction_get_return_type(nfunction),
                                        nodemgr);
      node_ptr ftype = new_list();
      NodeList_ptr args = NFunction_get_args(nfunction);
      ListIter_ptr iter;

      NODE_LIST_FOREACH(args, iter) {
        SymbType_ptr elem;
        node_ptr arg;

        elem = SYMB_TYPE(NodeList_get_elem_at(args, iter));
        arg = SymbType_to_node(elem, nodemgr);
        ftype = Node_conslist_add(nodemgr, ftype, arg);
      }

      return find_node(nodemgr, NFUNCTION_TYPE, ftype, rtype);
    }

  case SYMB_TYPE_ERROR:
    return Nil;

  default:
    return Nil;
  }

  return Nil;
}

boolean SymbType_is_single_value_enum(const SymbType_ptr self)
{
  boolean retval = false;
  node_ptr values = NULL;

  SYMB_TYPE_CHECK_INSTANCE(self);

  if (SymbType_is_enum(self)) {
    values = SymbType_get_enum_type_values(self);

    nusmv_assert(values != 0); /* every enum type has some values */

    if (Nil == cdr(values)) retval = true;
    else retval = false;
  }
  else retval = false;

  return retval;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief  Equality function for node_ptr, used to compare bodies of
  types in SymbType_equal.

  TODO[AMi] this function may not work well if nodes are
  semantically equivalent but not isomorphic


  \sa SymbType_equal
*/
static boolean node_equal(node_ptr n1, node_ptr n2)
{
  if (n1 == n2) return true;

  if ((Nil == n1) || (Nil == n2)) {
    return false;
  }

  if (node_get_type(n1) == node_get_type(n2)) {
    switch(node_get_type(n1)) {
    case NUMBER:
      return car(n1) == car(n2);

    case ATOM:
      if ((Nil != car(n1)) && (Nil != car(n2))) {
        /* If the atom is not Nil then compare strings */
        return !strcmp(UStringMgr_get_string_text(node_get_lstring(n1)),
                       UStringMgr_get_string_text(node_get_lstring(n2)));
      }
      else return false;
    default:
      return (node_equal(car(n1), car(n2)) &&
              node_equal(cdr(n1), cdr(n2)));

    }
  }

  return false;
}

/*!
  \brief  Utility of SymbType_sprint and SymbType_print to print
                       in an fprintf-like fashion using information in the given
                       SymbTypePrinterInfo

   This function prints the specified formatted string in
                       the pinfo

  \sa SymbType_print, SymbType_sprint
*/
static void str_print(const NuSMVEnv_ptr env,
                      SymbTypePrinterInfo_ptr pinfo,
                      const char *fmt, ...)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  va_list ap;

  if (pinfo->output_type == FILE_OUTPUT) {
    va_start(ap, fmt);
    vfprintf(pinfo->file_stream, fmt, ap);
    va_end(ap);
  }
  else if (pinfo->output_type == STRING_OUTPUT) {
    unsigned size;
    int n;
    char *np, *p;

    /* Guess we need no more than INITIAL_PRINT_BUFFER_SIZE bytes. */
    size = INITIAL_PRINT_BUFFER_SIZE;

    p = ALLOC(char, size);

    if(NIL(char) == p) {
      ErrorMgr_internal_error(errmgr, "Out of memory");
    }

    while (true) {
      /* Try to print in the allocated space. */
      va_start(ap, fmt);
      n = vsnprintf (p, size, fmt, ap);
      va_end(ap);

      /* If that worked, append it to the buffer res */
      if (n > -1 && n < size) {
        char* tmp;

        if (NIL(char) == pinfo->string_buffer) {
          pinfo->string_buffer = p;
          pinfo->buffer_size = n;
          return;
        }
        else {
          if (pinfo->remaining_space <= n) {
            /* We do not have enough memory, we have to REALLOC */
            tmp = REALLOC(char,
                          pinfo->string_buffer,
                          (pinfo->buffer_size * 2) + n + 1);

            if (NIL(char) == tmp) {
              ErrorMgr_internal_error(errmgr, "Out of memory");
            }

            pinfo->buffer_size += pinfo->buffer_size + n;
            pinfo->remaining_space += pinfo->buffer_size + n;
            pinfo->string_buffer = tmp;
          }

          /* Concatenate the buffer with p */
          strncat(pinfo->string_buffer, p, n);
          pinfo->remaining_space -= n;

          FREE(p);

          return;
        }
      }

      /* Else try again with more space. */
      if (n > -1) {
        /* glibc 2.1 */
        size = n+1; /* precisely what is needed */
      }
      else {
        /* glibc 2.0 */
        size *= 2;  /* twice the old size */
      }

      /* Resize p */
      np = REALLOC(char, p, size);

      if (NIL(char) == np) {
        FREE(p);
        ErrorMgr_internal_error(errmgr, "Out of memory");
      }
      else {
        p = np;
      }
    }
  }
  else {
    ErrorMgr_internal_error(errmgr, "Unknown output type");
    error_unreachable_code();
  }
}

/*!
  \brief  Utility of SymbType_sprint and SymbType_print to
                       actually print the type

   This is the function that prints the SymbType using the
                       file stream or the string buffer provided by pinfo

  \sa SymbType_print, SymbType_sprint
*/
static void symb_type_sprint_aux(const SymbType_ptr self,
                                 MasterPrinter_ptr wffprint,
                                 SymbTypePrinterInfo_ptr pinfo)
{
  NuSMVEnv_ptr env;

  SYMB_TYPE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(wffprint));

  switch (self->tag) {

  case SYMB_TYPE_NONE:
    str_print(env, pinfo, "no-type");
    break;

  case SYMB_TYPE_STATEMENT:
    str_print(env, pinfo, "statement");
    break;

  case SYMB_TYPE_BOOLEAN:
    str_print(env, pinfo, "boolean");
    break;

  case SYMB_TYPE_ENUM:
    if (!self->isMemorySharingInstance) {
      node_ptr l = SymbType_get_enum_type_values(self);
      str_print(env, pinfo, "{");
      while (l != Nil) {
        char* tmp;

        tmp = sprint_node(wffprint, car(l));
        str_print(env, pinfo, "%s", tmp);
        FREE(tmp);

        l = cdr(l);
        if (l != Nil) { str_print(env, pinfo, ", "); }
      }
      str_print(env, pinfo, "}");
    }
    else { /* this is memory sharing types (therefore, are "simplified") */
      switch (node_get_type(self->body)) {
      case ENUM_TYPE_PURE_INT:
        str_print(env, pinfo, "integer-enum");
        break;
      case ENUM_TYPE_PURE_SYMBOLIC:
        str_print(env,pinfo, "symbolic-enum");
        break;
      case ENUM_TYPE_INT_SYMBOLIC:
        str_print(env, pinfo, "integer-and-symbolic-enum");
        break;
      default: error_unreachable_code();
      }
    } /* else */
    break;

  case SYMB_TYPE_INTEGER:
    str_print(env, pinfo, "integer");
    break;

  case SYMB_TYPE_REAL:
    str_print(env, pinfo, "real");
    break;

  case SYMB_TYPE_CONTINUOUS:
    str_print(env, pinfo, "continuous");
    break;

  case SYMB_TYPE_SIGNED_WORD:
    str_print(env, pinfo, "signed word[");
    str_print(env, pinfo, "%d", SymbType_get_word_width(self));
    str_print(env, pinfo, "]");
    break;

  case SYMB_TYPE_UNSIGNED_WORD:
    str_print(env, pinfo, "unsigned word[");
    str_print(env, pinfo, "%d", SymbType_get_word_width(self));
    str_print(env, pinfo, "]");
    break;

  case SYMB_TYPE_WORDARRAY:
    str_print(env, pinfo, "array ");

    str_print(env, pinfo, "word[");
    str_print(env, pinfo, "%d", SymbType_get_wordarray_awidth(self));
    str_print(env, pinfo, "]");

    str_print(env, pinfo, " of ");

    symb_type_sprint_aux(SymbType_get_wordarray_subtype(self), wffprint, pinfo);
    break;

  case SYMB_TYPE_SET_BOOL:  str_print(env, pinfo, "boolean-set"); break;
  case SYMB_TYPE_SET_INT:  str_print(env, pinfo, "integer-set"); break;
  case SYMB_TYPE_SET_SYMB: str_print(env, pinfo, "symbolic-set"); break;
  case SYMB_TYPE_SET_INT_SYMB: str_print(env, pinfo, "integer-symbolic-set");
    break;

  case SYMB_TYPE_ARRAY:
    {
      str_print(env, pinfo, "array %d..%d of ",
              SymbType_get_array_lower_bound(self),
              SymbType_get_array_upper_bound(self));
      symb_type_sprint_aux(SymbType_get_array_subtype(self), wffprint, pinfo);
      break;
    }

  case SYMB_TYPE_INTARRAY:
    {
      str_print(env, pinfo, "array integer of ");
      symb_type_sprint_aux(SymbType_get_intarray_subtype(self), wffprint, pinfo);
      break;
    }

  case SYMB_TYPE_NFUNCTION:
    {
      NFunction_ptr function = SymbType_get_nfunction_type(self);
      NodeList_ptr args = NFunction_get_args(function);
      SymbType_ptr return_type = NFunction_get_return_type(function);
      ListIter_ptr iter;
      node_ptr arg;

      for (iter=NodeList_get_first_iter(args); !ListIter_is_end(iter); ){
        arg = NodeList_get_elem_at(args, iter);
        symb_type_sprint_aux(SYMB_TYPE(arg), wffprint, pinfo);
        iter=ListIter_get_next(iter);
        if (! ListIter_is_end(iter)){
          str_print(env, pinfo, " * ");
        }
      }

      str_print(env, pinfo, " -> ");
      symb_type_sprint_aux(return_type, wffprint, pinfo);
      break;
    }

  case SYMB_TYPE_ERROR:
    str_print(env, pinfo, "error-type");
    break;

  default: error_unreachable_code();
  }

  return;
}

/*!
  \brief Virtual Class SymbType destructor

  Virtual destructor for symb type

  \sa SymbType_destroy
*/
static void symb_type_finalize(Object_ptr object, void* dummy)
{
  SymbType_ptr self = SYMB_TYPE(object);

  if (!self->isMemorySharingInstance) { /* this is a usual type instance */
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    /* debugging code to detect double freeing */
    nusmv_assert(self->body != NODE_FROM_INT(-1));

    if (SYMB_TYPE_ENUM == self->tag) {
      /* for the enum-type we added one addional node => remove it now */
      node_node_setcar(self->body, Nil);
      free_node(nodemgr, self->body);
    }
    if (SYMB_TYPE_NFUNCTION == self->tag) {
      NFunction_destroy(SymbType_get_nfunction_type(self));
    }

    else if (SYMB_TYPE_ARRAY == self->tag) {
      /* for array_type 2 nodes are to be deleted and the subtype.

         NOTE FOR DEVELOPERS: keep implementation of this function andq
         SymbType_destroy the same.
      */
      SymbType_ptr subtype = SYMB_TYPE(car(self->body));
      SymbType_destroy(subtype);
      free_node(nodemgr, cdr(self->body));
      free_node(nodemgr, self->body);
    }

    /* debugging code to detect double freeing */
    self->body = NODE_FROM_INT(-1);

    env_object_deinit(ENV_OBJECT(self));

    FREE(self);
  }
}
