/* ---------------------------------------------------------------------------


  This file is part of the ``compile.symb_table'' package of NuSMV version 2.
  Copyright (C) 2005 by FBK-irst.

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
  \brief Implementation of the compile.symb_table package

  This package contains just a few public functions.
  Most functionality lays in the classes this package contains.
  So these classes for more info.

*/


#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/compile/symb_table/symb_table.h"
#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/compile/symb_table/SymbType_private.h"

#include "nusmv/core/compile/symb_table/symb_table_int.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/cinit/NuSMVEnv.h"
#include "nusmv/core/utils/StreamMgr.h"
/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define RM_TYPE(e, t)                                                   \
  SymbType_destroy_memory_sharing_type(SYMB_TYPE(NuSMVEnv_remove_value(e, t)))

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define GET_TYPE(e, t)                          \
  SYMB_TYPE(NuSMVEnv_get_value(e, t))

/*---------------------------------------------------------------------------*/
/* Constants declarations                                                    */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_NO_TYPE "symb_table_no_type"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_STATEMENT_TYPE "symb_table_statement_type"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_BOOLEAN_TYPE "symb_table_boolean_type"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_PURE_SYMBOL_ENUM_TYPE "symb_table_pure_symbol_enum_type"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_INT_SYMBOLIC_ENUM_TYPE "symb_table_int_sym_enum_type"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_PURE_INT_ENUM_TYPE "symb_table_pure_int_enum_type"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_INTEGER_TYPE "symb_table_integer_type"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_REAL_TYPE "symb_table_real_type"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_CONTINUOUS_TYPE "symb_table_continuous_type"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_BOOLEAN_SET_TYPE "symb_table_boolean_set_type"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_INTEGER_SET_TYPE "symb_table_integer_set_type"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_SYMBOLIC_SET_TYPE "symb_table_symbolic_set_type"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_INTEGER_SYMBOLIC_SET_TYPE "symb_table_integer_symbolic_set_type"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_ERROR_TYPE "symb_table_error_type"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_WIDTH_TO_WORD_TYPE_HASH "symb_table_width_to_word_type_hash"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_WIDTHS_TO_WORDARRAY_TYPE_HASH "symb_table_widths_to_wordarray_type_hash"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SYMB_TABLE_SUBTYPE_BOUND_TO_ARRAY_TYPE_HASH "symb_table_subtypes"
#define ENV_SYMB_TABLE_SUBTYPE_TO_INTARRAY_TYPE_HASH "symb_table_intarray_subtypes"

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

static void symb_table_init_symb_table(NuSMVEnv_ptr env)
{
  SymbTable_ptr st;
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (opt_verbose_level_gt(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
               "Instantiating the SymbTable instance "
               "within the given environment...\n");
  }

  st = SymbTable_create(env);

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE, st);
}

static void symb_table_deinit_symb_table(NuSMVEnv_ptr env)
{
  SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_remove_value(env, ENV_SYMB_TABLE));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (opt_verbose_level_gt(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
               "Clearing the SymbTable instance in "
               "the given environment...\n");
  }

  SymbTable_destroy(st);
}


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static enum st_retval
symb_table_types_hash_cleaner(char * c1, char * c2, char *c3);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void SymbTablePkg_init(NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  node_ptr atom;

  /* initializes the global symbol table */
  symb_table_init_symb_table(env);

  /* initialise the symbolic types with shared memory */

  atom = new_node(nodemgr, ATOM, Nil, Nil);


  /* all the possible "simplified" symbolic types SymbType. "Simplified" means
     that an enum type's body contains a list of some arbitrary
     (not real) values.
     The important feature is that the memory is shared,
     so you can compare pointers to compare types.
     De-initialisation of the package destroys these types.
     The only access to these types is through functions
     "SymbTablePkg_..._type" (such as SymbTablePkg_integer_type).
  */

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_NO_TYPE,
                     SymbType_create_memory_sharing_type(env, SYMB_TYPE_NONE, Nil));

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_STATEMENT_TYPE,
                     SymbType_create_memory_sharing_type(env, SYMB_TYPE_STATEMENT, Nil));

  /* the Enum type will contain the list of some some artificial values */
  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_BOOLEAN_TYPE,
                     SymbType_create_memory_sharing_type(env, SYMB_TYPE_BOOLEAN, Nil));

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_PURE_SYMBOL_ENUM_TYPE,
                     SymbType_create_memory_sharing_type(env, SYMB_TYPE_ENUM, cons(nodemgr, atom, Nil)));

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_INT_SYMBOLIC_ENUM_TYPE,
                     SymbType_create_memory_sharing_type(env, SYMB_TYPE_ENUM,
                                                         cons(nodemgr, atom, cons(nodemgr, ExprMgr_number(exprs, 0), Nil))));

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_PURE_INT_ENUM_TYPE,
                     SymbType_create_memory_sharing_type(env, SYMB_TYPE_ENUM,
                                                         cons(nodemgr, ExprMgr_number(exprs, 0), Nil)));

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_INTEGER_TYPE,
                     SymbType_create_memory_sharing_type(env, SYMB_TYPE_INTEGER, Nil));

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_REAL_TYPE,
                     SymbType_create_memory_sharing_type(env, SYMB_TYPE_REAL, Nil));

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_CONTINUOUS_TYPE,
                     SymbType_create_memory_sharing_type(env,
                                                         SYMB_TYPE_CONTINUOUS,
                                                         Nil));

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_BOOLEAN_SET_TYPE,
                     SymbType_create_memory_sharing_type(env, SYMB_TYPE_SET_BOOL, Nil));

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_INTEGER_SET_TYPE,
                     SymbType_create_memory_sharing_type(env, SYMB_TYPE_SET_INT, Nil));

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_SYMBOLIC_SET_TYPE,
                     SymbType_create_memory_sharing_type(env, SYMB_TYPE_SET_SYMB, Nil));

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_INTEGER_SYMBOLIC_SET_TYPE,
                     SymbType_create_memory_sharing_type(env, SYMB_TYPE_SET_INT_SYMB, Nil));

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_ERROR_TYPE,
                     SymbType_create_memory_sharing_type(env, SYMB_TYPE_ERROR, Nil));

  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_WIDTH_TO_WORD_TYPE_HASH, new_assoc());
  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_WIDTHS_TO_WORDARRAY_TYPE_HASH, new_assoc());
  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_SUBTYPE_BOUND_TO_ARRAY_TYPE_HASH, new_assoc());
  NuSMVEnv_set_value(env, ENV_SYMB_TABLE_SUBTYPE_TO_INTARRAY_TYPE_HASH, new_assoc());
}

void SymbTablePkg_quit(NuSMVEnv_ptr env)
{
  OptsHandler_ptr opt = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  hash_ptr symb_table_width_to_word_type_hash =
    (hash_ptr)NuSMVEnv_remove_value(env, ENV_SYMB_TABLE_WIDTH_TO_WORD_TYPE_HASH);
  hash_ptr symb_table_widths_to_wordarray_type_hash =
    (hash_ptr)NuSMVEnv_remove_value(env, ENV_SYMB_TABLE_WIDTHS_TO_WORDARRAY_TYPE_HASH);
  hash_ptr symb_table_subtype_bound_to_array_type_hash =
    (hash_ptr)NuSMVEnv_remove_value(env, ENV_SYMB_TABLE_SUBTYPE_BOUND_TO_ARRAY_TYPE_HASH);
  hash_ptr symb_table_subtype_to_intarray_type_hash =
    (hash_ptr)NuSMVEnv_remove_value(env, ENV_SYMB_TABLE_SUBTYPE_TO_INTARRAY_TYPE_HASH);

  if (opt_verbose_level_gt(opt, 3)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Clearing the symbol table package...\n");
  }

  /* deinits the global symbol table: */
  symb_table_deinit_symb_table(env);

  RM_TYPE(env, ENV_SYMB_TABLE_NO_TYPE);
  RM_TYPE(env, ENV_SYMB_TABLE_STATEMENT_TYPE);
  RM_TYPE(env, ENV_SYMB_TABLE_BOOLEAN_TYPE);
  RM_TYPE(env, ENV_SYMB_TABLE_PURE_SYMBOL_ENUM_TYPE);
  RM_TYPE(env, ENV_SYMB_TABLE_INT_SYMBOLIC_ENUM_TYPE);
  RM_TYPE(env, ENV_SYMB_TABLE_PURE_INT_ENUM_TYPE);
  RM_TYPE(env, ENV_SYMB_TABLE_INTEGER_TYPE);
  RM_TYPE(env, ENV_SYMB_TABLE_REAL_TYPE);
  RM_TYPE(env, ENV_SYMB_TABLE_CONTINUOUS_TYPE);
  RM_TYPE(env, ENV_SYMB_TABLE_BOOLEAN_SET_TYPE);
  RM_TYPE(env, ENV_SYMB_TABLE_INTEGER_SET_TYPE);
  RM_TYPE(env, ENV_SYMB_TABLE_SYMBOLIC_SET_TYPE);
  RM_TYPE(env, ENV_SYMB_TABLE_INTEGER_SYMBOLIC_SET_TYPE);
  RM_TYPE(env, ENV_SYMB_TABLE_ERROR_TYPE);

  clear_assoc_and_free_entries(symb_table_width_to_word_type_hash,
                               symb_table_types_hash_cleaner);
  free_assoc(symb_table_width_to_word_type_hash);
  clear_assoc_and_free_entries(symb_table_widths_to_wordarray_type_hash,
                               symb_table_types_hash_cleaner);
  free_assoc(symb_table_widths_to_wordarray_type_hash);
  clear_assoc_and_free_entries(symb_table_subtype_bound_to_array_type_hash,
                               symb_table_types_hash_cleaner);
  free_assoc(symb_table_subtype_bound_to_array_type_hash);
  clear_assoc_and_free_entries(symb_table_subtype_to_intarray_type_hash,
                               symb_table_types_hash_cleaner);
  free_assoc(symb_table_subtype_to_intarray_type_hash);
}

SymbType_ptr SymbTablePkg_no_type(const NuSMVEnv_ptr env)
{
  return GET_TYPE(env, ENV_SYMB_TABLE_NO_TYPE);
}

SymbType_ptr SymbTablePkg_statement_type(const NuSMVEnv_ptr env)
{
  return GET_TYPE(env, ENV_SYMB_TABLE_STATEMENT_TYPE);
}

SymbType_ptr SymbTablePkg_boolean_type(const NuSMVEnv_ptr env)
{
  return GET_TYPE(env, ENV_SYMB_TABLE_BOOLEAN_TYPE);
}

SymbType_ptr SymbTablePkg_pure_symbolic_enum_type(const NuSMVEnv_ptr env)
{
  return GET_TYPE(env, ENV_SYMB_TABLE_PURE_SYMBOL_ENUM_TYPE);
}

SymbType_ptr SymbTablePkg_int_symbolic_enum_type(const NuSMVEnv_ptr env)
{
  return GET_TYPE(env, ENV_SYMB_TABLE_INT_SYMBOLIC_ENUM_TYPE);
}

SymbType_ptr SymbTablePkg_pure_int_enum_type(const NuSMVEnv_ptr env)
{
  return GET_TYPE(env, ENV_SYMB_TABLE_PURE_INT_ENUM_TYPE);
}

SymbType_ptr SymbTablePkg_integer_type(const NuSMVEnv_ptr env)
{
  return GET_TYPE(env, ENV_SYMB_TABLE_INTEGER_TYPE);
}

SymbType_ptr SymbTablePkg_real_type(const NuSMVEnv_ptr env)
{
  return GET_TYPE(env, ENV_SYMB_TABLE_REAL_TYPE);
}

SymbType_ptr SymbTablePkg_continuous_type(const NuSMVEnv_ptr env)
{
  return GET_TYPE(env, ENV_SYMB_TABLE_CONTINUOUS_TYPE);
}

SymbType_ptr SymbTablePkg_unsigned_word_type(const NuSMVEnv_ptr env, int width)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  hash_ptr symb_table_width_to_word_type_hash =
    (hash_ptr)NuSMVEnv_get_value(env, ENV_SYMB_TABLE_WIDTH_TO_WORD_TYPE_HASH);

  SymbType_ptr type;

  nusmv_assert(width > 0); /* debugging check */

  type = SYMB_TYPE(find_assoc(symb_table_width_to_word_type_hash,
                              NODE_FROM_INT(width)));
  if (SYMB_TYPE(NULL) == type) {
    type = SymbType_create_memory_sharing_type(env, SYMB_TYPE_UNSIGNED_WORD,
                             find_node(nodemgr, NUMBER, NODE_FROM_INT(width), Nil));
    insert_assoc(symb_table_width_to_word_type_hash,
                 NODE_FROM_INT(width), (node_ptr) type);
  }

  return type;
}

SymbType_ptr SymbTablePkg_signed_word_type(const NuSMVEnv_ptr env, int width)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  hash_ptr symb_table_width_to_word_type_hash =
    (hash_ptr)NuSMVEnv_get_value(env, ENV_SYMB_TABLE_WIDTH_TO_WORD_TYPE_HASH);

  SymbType_ptr type;

  nusmv_assert(width > 0); /* debugging check */

  type = SYMB_TYPE(find_assoc(symb_table_width_to_word_type_hash,
                              NODE_FROM_INT(-width)));
  if (SYMB_TYPE(NULL) == type) {
    type = SymbType_create_memory_sharing_type(env, SYMB_TYPE_SIGNED_WORD,
                             find_node(nodemgr, NUMBER, NODE_FROM_INT(width), Nil));
    insert_assoc(symb_table_width_to_word_type_hash,
                 NODE_FROM_INT(-width), (node_ptr) type);
  }

  return type;
}

SymbType_ptr SymbTablePkg_wordarray_type(const NuSMVEnv_ptr env,
                                         int awidth, SymbType_ptr subtype)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  hash_ptr symb_table_widths_to_wordarray_type_hash =
    (hash_ptr)NuSMVEnv_get_value(env, ENV_SYMB_TABLE_WIDTHS_TO_WORDARRAY_TYPE_HASH);

  node_ptr a;
  node_ptr key;
  SymbType_ptr type;

  a = find_node(nodemgr, NUMBER, NODE_FROM_INT(awidth), Nil);
  key = find_node(nodemgr, CONS, a, NODE_PTR(subtype));

  type = SYMB_TYPE(find_assoc(symb_table_widths_to_wordarray_type_hash, key));
  if (SYMB_TYPE(NULL) == type) {
    type = SymbType_create_memory_sharing_type(env, SYMB_TYPE_WORDARRAY, key);
    insert_assoc(symb_table_widths_to_wordarray_type_hash, key, (node_ptr)type);
  }

  return type;
}

SymbType_ptr SymbTablePkg_array_type(SymbType_ptr subtype,
                                     int lower_bound,
                                     int upper_bound)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(subtype));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  hash_ptr symb_table_subtype_bound_to_array_type_hash =
    (hash_ptr)NuSMVEnv_get_value(env, ENV_SYMB_TABLE_SUBTYPE_BOUND_TO_ARRAY_TYPE_HASH);


  node_ptr key = find_node(nodemgr, CONS, NODE_PTR(subtype),
                           find_node(nodemgr, CONS,
                                     NODE_FROM_INT(lower_bound),
                                     NODE_FROM_INT(upper_bound)));

  SymbType_ptr type =
    SYMB_TYPE(find_assoc(symb_table_subtype_bound_to_array_type_hash, key));

  if (SYMB_TYPE(NULL) == type) {
    type = SymbType_create_memory_sharing_array_type(subtype,
                                                     lower_bound,
                                                     upper_bound);
    insert_assoc(symb_table_subtype_bound_to_array_type_hash, key,
                 NODE_PTR(type));
  }

  return type;
}

SymbType_ptr SymbTablePkg_intarray_type(const NuSMVEnv_ptr env,
                                         SymbType_ptr subtype)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  hash_ptr symb_table_intarray_subtype_hash =
    (hash_ptr)NuSMVEnv_get_value(env, ENV_SYMB_TABLE_SUBTYPE_TO_INTARRAY_TYPE_HASH);

  node_ptr key;
  SymbType_ptr type;

  key = find_node(nodemgr, CONS, NODE_PTR(subtype), Nil);

  type = SYMB_TYPE(find_assoc(symb_table_intarray_subtype_hash, key));
  if (SYMB_TYPE(NULL) == type) {
    type = SymbType_create_memory_sharing_type(env, SYMB_TYPE_INTARRAY, key);
    insert_assoc(symb_table_intarray_subtype_hash, key, (node_ptr)type);
  }

  return type;
}

SymbType_ptr SymbTablePkg_boolean_set_type(const NuSMVEnv_ptr env)
{
  return GET_TYPE(env, ENV_SYMB_TABLE_BOOLEAN_SET_TYPE);
}

SymbType_ptr SymbTablePkg_integer_set_type(const NuSMVEnv_ptr env)
{
  return GET_TYPE(env, ENV_SYMB_TABLE_INTEGER_SET_TYPE);
}

SymbType_ptr SymbTablePkg_symbolic_set_type(const NuSMVEnv_ptr env)
{
  return GET_TYPE(env, ENV_SYMB_TABLE_SYMBOLIC_SET_TYPE);
}

SymbType_ptr SymbTablePkg_integer_symbolic_set_type(const NuSMVEnv_ptr env)
{
  return GET_TYPE(env, ENV_SYMB_TABLE_INTEGER_SYMBOLIC_SET_TYPE);
}

SymbType_ptr SymbTablePkg_error_type(const NuSMVEnv_ptr env)
{
  return GET_TYPE(env, ENV_SYMB_TABLE_ERROR_TYPE);
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The function is used to free the memory from
  memory-sharing Word/WordArray/Array types stored in the hash table
  symb_table_..._type_hash. Used in the SymbTablePkg_quit only

  \todo Missing description
*/
static enum st_retval
symb_table_types_hash_cleaner(char * c1, char * c2, char *c3)
{
  SymbType_ptr type;

  type = SYMB_TYPE(c2);
  /* only Word type can be in the hash table */
  nusmv_assert(SymbType_is_word(type) ||
               SYMB_TYPE_WORDARRAY == SymbType_get_tag(type) ||
               SYMB_TYPE_ARRAY == SymbType_get_tag(type) ||
               SYMB_TYPE_INTARRAY == SymbType_get_tag(type));
  SymbType_destroy_memory_sharing_type(type);
  return ST_DELETE;
}
