/* ---------------------------------------------------------------------------


  This file is part of the ``compile.symb_table'' package of NuSMV
  version 2.  Copyright (C) 2004 by FBK-irst.

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
  \author Roberto Cavada, Alessandro Mariotti
  \brief The SymbCache class implementation

  \todo: Missing description

*/


#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/compile/symb_table/symb_table_int.h"

#include "nusmv/core/compile/symb_table/SymbCache.h"
#include "nusmv/core/compile/symb_table/SymbCache_private.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/Stack.h"
#include "nusmv/core/compile/symb_table/NFunction.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/node/NodeMgr.h"

#include "nusmv/core/compile/compileInt.h"

#include "nusmv/core/compile/flattening/MasterCompileFlattener.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SYMBOL_INFO_CHUNK_SIZE 1024

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define INITIAL_SYMBOLS_ARRAY_SIZE 128

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_SYMBOL_REMOVED (SymbolInfo*)1

typedef struct SymbolInfo_TAG {
  SymbTableType category;

  /*
    Constants:
      - field1 is the reference counter

    Variables:
      - field1 is the variable type

    Defines / Parameters / Array Defines
      - field1 is the context
      - field2 is the body
      - field3 is the flatten body (NULL if still not computed)

    Functions:
      - field1 is the context
      - field2 is the function type
  */

  void* field1;
  void* field2;
  void* field3;

  /* Pointer to next free SymbolInfo slice */
  struct SymbolInfo_TAG* next;

  /* The position in the ordered list */
  unsigned int position;
} SymbolInfo;

typedef struct SymbCache_TAG
{
  NuSMVEnv_ptr environment;
  OptsHandler_ptr options;
  NodeMgr_ptr nodes;
  MasterPrinter_ptr printer;
  Logger_ptr logger;

  SymbTable_ptr symb_table;
  hash_ptr symbol_hash;

  SymbolInfo* symbol_info_pool;
  Stack_ptr chunks;

  /* Insertion ordered list */
  node_ptr* symbols;
  unsigned int symbols_allocated;
  unsigned int symbols_index;
  unsigned int symbols_empty;

  /* Counters */
  int constants_num;
  int state_vars_num;
  int input_vars_num;
  int frozen_vars_num;
  int defines_num;
  int functions_num;
  int array_defines_num;
  int variable_arrays_num;
  int parameters_num;

  NodeList_ptr add_triggers;
  NodeList_ptr rem_triggers;
  NodeList_ptr redef_triggers;
} SymbCache;

typedef struct SymbCacheRemoveSymbolStruct_TAG {
  SymbTableTriggerFun trigger;
  void* arg;
  boolean must_free_arg;
} SymbCacheRemoveSymbolStruct;

/*---------------------------------------------------------------------------*/
/* macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_CATEGORY(si)                         \
  si->category

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_GET_CONSTANT_COUNT(si)               \
  NODE_TO_INT(si->field1)

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_SET_CONSTANT_COUNT(si, val)          \
  si->field1 = NODE_FROM_INT(val)

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_VAR_TYPE(si)                         \
  si->field1

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_DEFINE_CONTEXT(si)                   \
  si->field1

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_DEFINE_BODY(si)                      \
  si->field2

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_DEFINE_FLATTEN_BODY(si)              \
  si->field3

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_ARRAY_DEFINE_CONTEXT(si)             \
  si->field1

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_ARRAY_DEFINE_BODY(si)                \
  si->field2

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_ARRAY_DEFINE_FLAT_BODY(si)           \
  si->field3

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_PARAMETER_CONTEXT(si)                \
  si->field1

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_PARAMETER_ACTUAL(si)                 \
  si->field2

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_PARAMETER_FLATTEN_ACTUAL(si)         \
  si->field3

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_FUNCTION_CONTEXT(si)                 \
  si->field1

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_FUNCTION_TYPE(si)                    \
  si->field2

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define GET_SYMBOL(self, s)                             \
  ((SymbolInfo*)find_assoc(self->symbol_hash, s))

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_IS_DECLARED(si)                                 \
  (((SymbolInfo*)NULL != si) && ((SymbolInfo*)1 != si))

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SI_IS_REMOVED(si)                       \
  (SI_SYMBOL_REMOVED == si)

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void symb_cache_init(SymbCache_ptr self,
                            SymbTable_ptr symb_table,
                            NuSMVEnv_ptr env);

static void symb_cache_deinit(SymbCache_ptr self);


static inline SymbolInfo*
symb_cache_alloc_symb_info(const SymbCache_ptr self);

static inline void
symb_cache_free_symb_info(const SymbCache_ptr self, SymbolInfo* si);

static inline void
symb_cache_new_symbol(SymbCache_ptr self, const node_ptr sym,
                      SymbolInfo* symbol_info);

static inline void
symb_cache_remove_symbol(SymbCache_ptr self, const node_ptr sym,
                         const boolean shrink_if_needed);

static inline void
symb_cache_check_and_shrink_symbols(SymbCache_ptr self);

static void symb_cache_free_triggers(NodeList_ptr triggers);
/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

SymbCache_ptr SymbCache_create(SymbTable_ptr symb_table, NuSMVEnv_ptr env)
{
  SymbCache_ptr self = ALLOC(SymbCache, 1);

  SYMB_CACHE_CHECK_INSTANCE(self);

  symb_cache_init(self, symb_table, env);
  return self;
}

void SymbCache_destroy(SymbCache_ptr self)
{
  SYMB_CACHE_CHECK_INSTANCE(self);

  symb_cache_deinit(self);
  FREE(self);
}

NuSMVEnv_ptr SymbCache_get_environment(const SymbCache_ptr self)
{
  return self->environment;
}

void SymbCache_new_input_var(SymbCache_ptr self, node_ptr var,
                             SymbType_ptr type)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, var);
  /* The symbol must not exist */
  nusmv_assert(!SI_IS_DECLARED(si));

  si = symb_cache_alloc_symb_info(self);

  SI_CATEGORY(si) = STT_INPUT_VAR;
  SI_VAR_TYPE(si) = type;

  symb_cache_new_symbol(self, var, si);
}

void SymbCache_new_state_var(SymbCache_ptr self, node_ptr var,
                             SymbType_ptr type)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, var);
  /* The symbol must not exist */
  nusmv_assert(!SI_IS_DECLARED(si));

  si = symb_cache_alloc_symb_info(self);

  SI_CATEGORY(si) = STT_STATE_VAR;
  SI_VAR_TYPE(si) = type;

  symb_cache_new_symbol(self, var, si);
}

void SymbCache_new_frozen_var(SymbCache_ptr self, node_ptr var,
                               SymbType_ptr type)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, var);
  /* The symbol must not exist */
  nusmv_assert(!SI_IS_DECLARED(si));

  si = symb_cache_alloc_symb_info(self);

  SI_CATEGORY(si) = STT_FROZEN_VAR;
  SI_VAR_TYPE(si) = type;

  symb_cache_new_symbol(self, var, si);
}

/* TODO[MD] I think it is wrong to avoid calling remove and new symbol, because
   this way triggers are not called */
void SymbCache_redeclare_state_as_frozen_var(SymbCache_ptr self, node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);

  nusmv_assert(SymbCache_is_symbol_state_var(self, name));

  SI_CATEGORY(si) = STT_FROZEN_VAR;

  /* Since new_symbol or remove_symbol are not called, we need to
     update the counters manually */
  ++self->frozen_vars_num;
  --self->state_vars_num;
}

void SymbCache_remove_var(SymbCache_ptr self, node_ptr var)
{
  SYMB_CACHE_CHECK_INSTANCE(self);

  nusmv_assert(SymbCache_is_symbol_var(self, var));

  symb_cache_remove_symbol(self, var, true);
}

void SymbCache_new_define(SymbCache_ptr self, node_ptr name,
                          node_ptr ctx, node_ptr definition)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);
  /* The symbol must not exist */
  nusmv_assert(!SI_IS_DECLARED(si));

  si = symb_cache_alloc_symb_info(self);

  SI_CATEGORY(si) = STT_DEFINE;
  SI_DEFINE_CONTEXT(si) = ctx;
  SI_DEFINE_BODY(si) = definition;

  symb_cache_new_symbol(self, name, si);
}

void SymbCache_new_parameter(SymbCache_ptr self, node_ptr formal,
                             node_ptr ctx, node_ptr actual)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, formal);
  /* The symbol must not exist */
  nusmv_assert(!SI_IS_DECLARED(si));

  si = symb_cache_alloc_symb_info(self);

  SI_CATEGORY(si) = STT_PARAMETER;
  SI_PARAMETER_CONTEXT(si) = ctx;
  SI_PARAMETER_ACTUAL(si) = actual;

  symb_cache_new_symbol(self, formal, si);
}

void SymbCache_new_array_define(SymbCache_ptr self, node_ptr name,
                                 node_ptr ctx, node_ptr definition)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);
  /* The symbol must not exist */
  nusmv_assert(!SI_IS_DECLARED(si));

  si = symb_cache_alloc_symb_info(self);

  SI_CATEGORY(si) = STT_ARRAY_DEFINE;
  SI_ARRAY_DEFINE_CONTEXT(si) = ctx;
  SI_ARRAY_DEFINE_BODY(si) = definition;

  symb_cache_new_symbol(self, name, si);
}

void SymbCache_new_variable_array(SymbCache_ptr self, node_ptr name,
                                  SymbType_ptr type)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);
  /* The symbol must not exist */
  nusmv_assert(!SI_IS_DECLARED(si));

  si = symb_cache_alloc_symb_info(self);

  SI_CATEGORY(si) = STT_VARIABLE_ARRAY;
  SI_VAR_TYPE(si) = type;

  symb_cache_new_symbol(self, name, si);
}

void SymbCache_remove_define(SymbCache_ptr self, node_ptr define)
{
  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_define(self, define));

  symb_cache_remove_symbol(self, define, true);
}

void SymbCache_remove_function(SymbCache_ptr self, node_ptr fun)
{
  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_function(self, fun));

  symb_cache_remove_symbol(self, fun, true);
}

void SymbCache_remove_parameter(SymbCache_ptr self, node_ptr formal)
{
  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_parameter(self, formal));

  symb_cache_remove_symbol(self, formal, true);
}

void SymbCache_remove_variable_array(SymbCache_ptr self, node_ptr symbol)
{
  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_variable_array(self, symbol));

  symb_cache_remove_symbol(self, symbol, true);
}

void SymbCache_new_constant(SymbCache_ptr self, node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);

  /* Constant never defines */
  if (!SI_IS_DECLARED(si)) {
    si = symb_cache_alloc_symb_info(self);

    SI_CATEGORY(si) = STT_CONSTANT;
    SI_SET_CONSTANT_COUNT(si, 1);

    symb_cache_new_symbol(self, name, si);
  }
  else {
    SI_SET_CONSTANT_COUNT(si, SI_GET_CONSTANT_COUNT(si) + 1);
  }
}

void SymbCache_new_function(SymbCache_ptr self, node_ptr name,
                            node_ptr ctx, SymbType_ptr type)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);
  /* The symbol must not exist */
  nusmv_assert(!SI_IS_DECLARED(si));

  si = symb_cache_alloc_symb_info(self);

  SI_CATEGORY(si) = STT_FUNCTION;
  SI_FUNCTION_CONTEXT(si) = ctx;
  SI_FUNCTION_TYPE(si) = type;

  symb_cache_new_symbol(self, name, si);
}

node_ptr SymbCache_get_function_context(SymbCache_ptr self, node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_function(self, name));

  si = GET_SYMBOL(self, name);

  return SI_FUNCTION_CONTEXT(si);
}

void SymbCache_remove_constant(SymbCache_ptr self, node_ptr constant)
{
  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_constant(self, constant));

  symb_cache_remove_symbol(self, constant, true);
}

void SymbCache_remove_symbols(SymbCache_ptr self,
                              const node_ptr* symbols,
                              const unsigned int size)
{
  unsigned int i = 0;

  for (i = 0; i < size; ++i) {
    /* Shrink only at last if needed */
    if (Nil != symbols[i]) {
      symb_cache_remove_symbol(self, symbols[i], false);
    }
  }

  symb_cache_check_and_shrink_symbols(self);
}

SymbType_ptr SymbCache_get_var_type(const SymbCache_ptr self,
                                    const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_var(self, name));

  si = GET_SYMBOL(self, name);

  return SI_VAR_TYPE(si);
}

node_ptr SymbCache_get_define_body(const SymbCache_ptr self,
                                   const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_define(self, name));

  si = GET_SYMBOL(self, name);

  return SI_DEFINE_BODY(si);
}

node_ptr SymbCache_get_actual_parameter(const SymbCache_ptr self,
                                        const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_parameter(self, name));

  si = GET_SYMBOL(self, name);

  return SI_PARAMETER_ACTUAL(si);
}

node_ptr SymbCache_get_array_define_body(const SymbCache_ptr self,
                                          const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_array_define(self, name));

  si = GET_SYMBOL(self, name);

  return SI_ARRAY_DEFINE_BODY(si);
}

SymbType_ptr SymbCache_get_variable_array_type(const SymbCache_ptr self,
                                               const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_variable_array(self, name));

  si = GET_SYMBOL(self, name);

  return SI_VAR_TYPE(si);
}

SymbType_ptr SymbCache_get_function_type(const SymbCache_ptr self,
                                         const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_function(self, name));

  si = GET_SYMBOL(self, name);

  return SI_FUNCTION_TYPE(si);
}

node_ptr
SymbCache_get_define_flatten_body(const SymbCache_ptr self,
                                  const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_define(self, name));

  si = GET_SYMBOL(self, name);

  nusmv_assert(SI_IS_DECLARED(si));

  if (Nil == SI_DEFINE_FLATTEN_BODY(si)) {
    node_ptr res;

    res = Flatten_GetDefinition(self->symb_table, name, true);

    SI_DEFINE_FLATTEN_BODY(si) = res;
  }

  return SI_DEFINE_FLATTEN_BODY(si);
}

node_ptr
SymbCache_get_flatten_actual_parameter(const SymbCache_ptr self,
                                       const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_parameter(self, name));

  si = GET_SYMBOL(self, name);
  nusmv_assert(SI_IS_DECLARED(si));

  if (Nil == SI_PARAMETER_FLATTEN_ACTUAL(si)) {
    node_ptr flat;

    flat = NodeMgr_find_node(self->nodes, CONTEXT,
                                 SI_PARAMETER_CONTEXT(si),
                                 SI_PARAMETER_ACTUAL(si));

    SI_PARAMETER_FLATTEN_ACTUAL(si) = flat;
  }

  return SI_PARAMETER_FLATTEN_ACTUAL(si);
}

node_ptr SymbCache_get_define_context(const SymbCache_ptr self,
                                      const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_define(self, name));

  si = GET_SYMBOL(self, name);

  return SI_DEFINE_CONTEXT(si);
}

node_ptr SymbCache_get_actual_parameter_context(const SymbCache_ptr self,
                                                const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_parameter(self, name));

  si = GET_SYMBOL(self, name);

  return SI_PARAMETER_CONTEXT(si);
}

node_ptr SymbCache_get_array_define_context(const SymbCache_ptr self,
                                             const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(SymbCache_is_symbol_array_define(self, name));

  si = GET_SYMBOL(self, name);

  return SI_ARRAY_DEFINE_CONTEXT(si);
}

boolean SymbCache_is_symbol_state_var(const SymbCache_ptr self,
                                      const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);

  return (SI_IS_DECLARED(si) && STT_STATE_VAR == SI_CATEGORY(si));
}

boolean SymbCache_is_symbol_frozen_var(const SymbCache_ptr self,
                                       const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);

  return (SI_IS_DECLARED(si) && STT_FROZEN_VAR == SI_CATEGORY(si));
}

boolean SymbCache_is_symbol_state_frozen_var(const SymbCache_ptr self,
                                             const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);

  return (SI_IS_DECLARED(si) &&
          ((STT_FROZEN_VAR | STT_STATE_VAR) & SI_CATEGORY(si)));
}

boolean SymbCache_is_symbol_input_var(const SymbCache_ptr self,
                                      const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);

  return (SI_IS_DECLARED(si) && STT_INPUT_VAR == SI_CATEGORY(si));
}

boolean SymbCache_is_symbol_var(const SymbCache_ptr self, const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);

  return (SI_IS_DECLARED(si) && (STT_VAR & SI_CATEGORY(si)));
}

boolean SymbCache_is_symbol_declared(const SymbCache_ptr self,
                                     const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);

  return SI_IS_DECLARED(si);
}

boolean SymbCache_is_symbol_constant(const SymbCache_ptr self,
                                     const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);

  return (SI_IS_DECLARED(si) && STT_CONSTANT == SI_CATEGORY(si));
}

boolean SymbCache_is_symbol_define(const SymbCache_ptr self,
                                   const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);

  return (SI_IS_DECLARED(si) &&
          STT_DEFINE == SI_CATEGORY(si));
}

boolean SymbCache_is_symbol_function(const SymbCache_ptr self,
                                       const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);

  return (SI_IS_DECLARED(si) &&
          STT_FUNCTION == SI_CATEGORY(si));
}

boolean SymbCache_is_symbol_parameter(const SymbCache_ptr self,
                                      const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);

  return (SI_IS_DECLARED(si) &&
          STT_PARAMETER == SI_CATEGORY(si));
}

boolean SymbCache_is_symbol_array_define(const SymbCache_ptr self,
                                          const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);

  return (SI_IS_DECLARED(si) &&
          STT_ARRAY_DEFINE == SI_CATEGORY(si));
}

boolean SymbCache_is_symbol_variable_array(const SymbCache_ptr self,
                                           const node_ptr name)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, name);

  return (SI_IS_DECLARED(si) &&
          STT_VARIABLE_ARRAY == SI_CATEGORY(si));
}

boolean SymbCache_list_contains_input_var(const SymbCache_ptr self,
                                          const NodeList_ptr var_list)
{
  ListIter_ptr iter;
  SYMB_CACHE_CHECK_INSTANCE(self);

  NODE_LIST_FOREACH(var_list, iter) {
    node_ptr sym = NodeList_get_elem_at(var_list, iter);
    if (SymbCache_is_symbol_input_var(self, sym)) return true;
  }

  return false;
}

boolean SymbCache_list_contains_state_frozen_var(const SymbCache_ptr self,
                                                 const NodeList_ptr var_list)
{
  ListIter_ptr iter;
  SYMB_CACHE_CHECK_INSTANCE(self);

  NODE_LIST_FOREACH(var_list, iter) {
    node_ptr sym = NodeList_get_elem_at(var_list, iter);
    if (SymbCache_is_symbol_state_frozen_var(self, sym)) return true;
  }

  return false;
}

boolean SymbCache_list_contains_undef_var(const SymbCache_ptr self,
                                          const NodeList_ptr var_list)
{
  ListIter_ptr iter;
  SYMB_CACHE_CHECK_INSTANCE(self);

  NODE_LIST_FOREACH(var_list, iter) {
    node_ptr sym = NodeList_get_elem_at(var_list, iter);
    SymbolInfo* si = GET_SYMBOL(self, sym);
    if (!SI_IS_DECLARED(si)) return true;
  }

  return false;
}

void SymbCache_next_iter(const SymbCache_ptr self,
                         SymbTableIter* iter)
{
  node_ptr sym;
  SymbolInfo* si;
  boolean valid = true;

  /* Iterator not at it's end */
  nusmv_assert(iter->index != self->symbols_index);

  do {
    valid = true;
    ++iter->index;

    /* The end.. */
    if (iter->index == self->symbols_index) {
      break;
    }

    sym = self->symbols[iter->index];

    /* Empty cell, continue with next */
    if (Nil == sym) {
      valid = false;
      continue;
    }

    /* Filter is not valid for this symbol */
    if (((SymbTableIterFilterFun)NULL != (iter->filter)) &&
        !((iter->filter)((iter->st), sym, (iter->arg)))) {
      valid = false;
      continue;
    }

    si = GET_SYMBOL(self, sym);
    nusmv_assert(SI_IS_DECLARED(si));

  } while (!valid || ((SI_CATEGORY(si) & iter->mask) == 0)); /* Not a match */

}

void SymbCache_iter_set_filter(const SymbCache_ptr self,
                               SymbTableIter* iter,
                               SymbTableIterFilterFun filter,
                               void* arg)
{
  unsigned int index = iter->index;
  node_ptr sym;
  SymbolInfo* si;

  iter->filter = filter;
  iter->arg = arg;
  nusmv_assert(SYMB_TABLE(NULL) != iter->st);

  /* The list is not empty */
  if (index != self->symbols_index) {
    sym = self->symbols[index];
    si = GET_SYMBOL(self, sym);
    nusmv_assert(SI_IS_DECLARED(si));

    /* Current symbol does not match */
    while ((SI_CATEGORY(si) & iter->mask) == 0 ||
           !(filter(iter->st, sym, arg))) {
      ++index;

      /* The end.. */
      if (index == self->symbols_index) { break; }

      sym = self->symbols[index];

      /* Empty cell, continue with next */
      if (Nil == sym) { continue; }

      si = GET_SYMBOL(self, sym);
      nusmv_assert(SI_IS_DECLARED(si));
    }
  }

  iter->index = index;

}

void SymbCache_gen_iter(const SymbCache_ptr self,
                        SymbTableIter* iter,
                        const unsigned int mask)
{
  SymbolInfo* si;
  node_ptr sym;
  unsigned int index = 0;

  iter->mask = mask;
  iter->filter = (SymbTableIterFilterFun)NULL;
  iter->st = SYMB_TABLE(NULL);
  iter->arg = NULL;

  /* The list is not empty */
  if (index != self->symbols_index) {
    sym = self->symbols[index];

    si = (Nil != sym) ? GET_SYMBOL(self, sym) : (SymbolInfo*) NULL;

    /* Current symbol does not match */
    while ((Nil == sym) || (SI_CATEGORY(si) & mask) == 0) {
      ++index;

      /* The end.. */
      if (index == self->symbols_index) { break; }

      sym = self->symbols[index];

      /* Empty cell, continue with next */
      if (Nil == sym) { continue; }

      si = GET_SYMBOL(self, sym);
      nusmv_assert(SI_IS_DECLARED(si));
    }
  }

  iter->index = index;
}

boolean SymbCache_is_iter_end(const SymbCache_ptr self,
                              const SymbTableIter* iter)
{
  return self->symbols_index == iter->index;
}

node_ptr SymbCache_iter_get_symbol(const SymbCache_ptr self,
                                   const SymbTableIter* iter)
{
  nusmv_assert(!SymbCache_is_iter_end(self, iter));
  return self->symbols[iter->index];
}

SymbTableType
SymbCache_get_symbol_type(const SymbCache_ptr self, const node_ptr symbol)
{
  SymbolInfo* si;

  SYMB_CACHE_CHECK_INSTANCE(self);

  si = GET_SYMBOL(self, symbol);

  nusmv_assert(SI_IS_DECLARED(si));

  return SI_CATEGORY(si);
}

int SymbCache_get_constants_num(const SymbCache_ptr self)
{
  SYMB_CACHE_CHECK_INSTANCE(self);
  return self->constants_num;
}

int SymbCache_get_state_vars_num(const SymbCache_ptr self)
{
  SYMB_CACHE_CHECK_INSTANCE(self);
  return self->state_vars_num;
}

int SymbCache_get_frozen_vars_num(const SymbCache_ptr self)
{
  SYMB_CACHE_CHECK_INSTANCE(self);
  return self->frozen_vars_num;
}

int SymbCache_get_input_vars_num(const SymbCache_ptr self)
{
  SYMB_CACHE_CHECK_INSTANCE(self);
  return self->input_vars_num;
}

int SymbCache_get_defines_num(const SymbCache_ptr self)
{
  SYMB_CACHE_CHECK_INSTANCE(self);
  return self->defines_num;
}

int SymbCache_get_functions_num(const SymbCache_ptr self)
{
  SYMB_CACHE_CHECK_INSTANCE(self);
  return self->functions_num;
}

int SymbCache_get_parameters_num(const SymbCache_ptr self)
{
  SYMB_CACHE_CHECK_INSTANCE(self);
  return self->parameters_num;
}

int SymbCache_get_array_defines_num(const SymbCache_ptr self)
{
  SYMB_CACHE_CHECK_INSTANCE(self);
  return self->array_defines_num;
}

int SymbCache_get_variable_arrays_num(const SymbCache_ptr self)
{
  SYMB_CACHE_CHECK_INSTANCE(self);
  return self->variable_arrays_num;
}

int SymbCache_get_symbols_num(const SymbCache_ptr self)
{
  int res = 0;

  SYMB_CACHE_CHECK_INSTANCE(self);

  res += self->constants_num;
  res += self->state_vars_num;
  res += self->input_vars_num;
  res += self->frozen_vars_num;
  res += self->defines_num;
  res += self->functions_num;
  res += self->array_defines_num;
  res += self->variable_arrays_num;
  res += self->parameters_num;

  nusmv_assert(res == (self->symbols_index - self->symbols_empty));

  return res;
}

void SymbCache_add_trigger(const SymbCache_ptr self,
                           SymbTableTriggerFun trigger,
                           SymbTableTriggerAction action,
                           void* arg,
                           boolean must_free_arg)
{
  SymbCacheRemoveSymbolStruct* srss = ALLOC(SymbCacheRemoveSymbolStruct, 1);

  srss->trigger = trigger;
  srss->arg = arg;
  srss->must_free_arg = must_free_arg;

  switch (action) {
  case ST_TRIGGER_SYMBOL_ADD:
    if (NODE_LIST(NULL) == self->add_triggers) {
      self->add_triggers = NodeList_create();
    }

    if (!NodeList_belongs_to(self->add_triggers, NODE_PTR(srss))) {
      NodeList_append(self->add_triggers, NODE_PTR(srss));
    }
    break;

  case ST_TRIGGER_SYMBOL_REMOVE:
    if (NODE_LIST(NULL) == self->rem_triggers) {
      self->rem_triggers = NodeList_create();
    }

    if (!NodeList_belongs_to(self->rem_triggers, NODE_PTR(srss))) {
      NodeList_append(self->rem_triggers, NODE_PTR(srss));
    }
    break;

  case ST_TRIGGER_SYMBOL_REDECLARE:
    if (NODE_LIST(NULL) == self->redef_triggers) {
      self->redef_triggers = NodeList_create();
    }

    if (!NodeList_belongs_to(self->redef_triggers, NODE_PTR(srss))) {
      NodeList_append(self->redef_triggers, NODE_PTR(srss));
    }
    break;

  default:
    error_unreachable_code_msg("Invalid trigger action");
  }
}

void SymbCache_remove_trigger(const SymbCache_ptr self,
                              SymbTableTriggerFun trigger,
                              SymbTableTriggerAction action)
{
  /* NodeList_ptr triggers = self->remove_symbol_triggers; */
  ListIter_ptr iter;
  NodeList_ptr triggers = NODE_LIST(NULL);

  switch (action) {
  case ST_TRIGGER_SYMBOL_ADD:
    triggers = self->add_triggers;
    break;

  case ST_TRIGGER_SYMBOL_REMOVE:
    triggers = self->rem_triggers;
    break;

  case ST_TRIGGER_SYMBOL_REDECLARE:
    triggers = self->redef_triggers;
    break;

  default:
    error_unreachable_code_msg("Invalid trigger action");
  }

  /* We found the list, but it may be empty */
  if (NODE_LIST(NULL) != triggers) {
    NODE_LIST_FOREACH(triggers, iter) {
      SymbCacheRemoveSymbolStruct* srss =
        (SymbCacheRemoveSymbolStruct*)NodeList_get_elem_at(triggers, iter);

      if (srss->trigger == trigger) {
        NodeList_remove_elem_at(triggers, iter);

        if (srss->must_free_arg) {
          FREE(srss->arg);
        }
        FREE(srss);
        break;
      }
    }

    /* The list is now useless */
    if (NodeList_get_length(triggers) == 0) {
      switch (action) {
      case ST_TRIGGER_SYMBOL_ADD: self->add_triggers = NODE_LIST(NULL); break;
      case ST_TRIGGER_SYMBOL_REMOVE: self->rem_triggers = NODE_LIST(NULL);  break;
      case ST_TRIGGER_SYMBOL_REDECLARE: self->redef_triggers = NODE_LIST(NULL); break;
      default: error_unreachable_code_msg("Invalid trigger action");
      }
      NodeList_destroy(triggers);
    }
  }
}


/*--------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Private initializer

  Private initializer, called by the constructor

  \sa symb_cache_deinit
*/
static void symb_cache_init(SymbCache_ptr self,
                            SymbTable_ptr symb_table,
                            NuSMVEnv_ptr env)
{
  self->symb_table = symb_table;
  self->environment = env;

  self->options = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  self->nodes = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  self->printer = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  self->logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));

  self->symbol_hash = new_assoc();

  self->chunks = Stack_create();

  self->symbol_info_pool = (SymbolInfo*)NULL;

  self->symbols_allocated = INITIAL_SYMBOLS_ARRAY_SIZE;
  self->symbols_index = 0;
  self->symbols_empty = 0;
  self->symbols = ALLOC(node_ptr, self->symbols_allocated);

  /* Counters */
  self->constants_num = 0;
  self->state_vars_num = 0;
  self->input_vars_num = 0;
  self->frozen_vars_num = 0;
  self->defines_num = 0;
  self->functions_num = 0;
  self->array_defines_num = 0;
  self->variable_arrays_num = 0;
  self->parameters_num = 0;

  self->add_triggers = NODE_LIST(NULL);
  self->rem_triggers = NODE_LIST(NULL);
  self->redef_triggers = NODE_LIST(NULL);
}

/*!
  \brief Private destructor used by class destroyer

  

  \sa symb_table_deinit
*/

static assoc_retval sym_hash_free_vars(char *key, char *data, char *arg)
{
  SymbolInfo* si = (SymbolInfo*)data;

  /* This is marked as removed, no memory to be freed */
  if (SI_IS_REMOVED(si)) return ASSOC_DELETE;

  nusmv_assert(SI_IS_DECLARED(si));

  /* If the symbols has a symbol_type instance associated, free it. */
  if (STT_VAR & SI_CATEGORY(si) ||
      STT_VARIABLE_ARRAY & SI_CATEGORY(si)) {
    SymbType_destroy(SI_VAR_TYPE(si));
  }
  /* The same for symbols that are functions! */
  else if (STT_FUNCTION & SI_CATEGORY(si)) {
    SymbType_destroy(SI_FUNCTION_TYPE(si));
  }

  return ASSOC_DELETE;
}

/*!
  \brief Private deinitializer

  Private deinitializer, called by the destructor

  \sa symb_cache_init
*/
static void symb_cache_deinit(SymbCache_ptr self)
{
  /* Free SymbType instances */
  clear_assoc_and_free_entries(self->symbol_hash, sym_hash_free_vars);
  free_assoc(self->symbol_hash);

  while (!Stack_is_empty(self->chunks)) {
    SymbolInfo* chunk = Stack_pop(self->chunks);
    FREE(chunk);
  }

  Stack_destroy(self->chunks);
  FREE(self->symbols);

  symb_cache_free_triggers(self->add_triggers);
  symb_cache_free_triggers(self->rem_triggers);
  symb_cache_free_triggers(self->redef_triggers);
}

/*!
  \brief Aux function for symb_cache_deinit

  Clears the given list of triggers

  \sa symb_cache_init
*/
static void symb_cache_free_triggers(NodeList_ptr triggers)
{
  ListIter_ptr iter;

  if (NODE_LIST(NULL) != triggers) {

    NODE_LIST_FOREACH(triggers, iter) {
      SymbCacheRemoveSymbolStruct* srss =
        (SymbCacheRemoveSymbolStruct*)NodeList_get_elem_at(triggers, iter);

      if (srss->must_free_arg) {
        FREE(srss->arg);
      }
      FREE(srss);
    }

    NodeList_destroy(triggers);
  }
}

/*!
  \brief Creates an instance of a SymbolInfo

  The instance is popped from the pool, if available.
                      Otherwise, a new chunk of memory is allocated
                      and the pool is repopulated with
                      SYMBOL_INFO_CHUNK_SIZE instances of SymbolInfo
*/
static inline SymbolInfo*
symb_cache_alloc_symb_info(const SymbCache_ptr self)
{
  SymbolInfo* res = (SymbolInfo*)NULL;

  /* Pool does not contain any slices */
  if ((SymbolInfo*)NULL == self->symbol_info_pool) {
    SymbolInfo* chunk = ALLOC(SymbolInfo, SYMBOL_INFO_CHUNK_SIZE);
    int i;

    memset(chunk, 0, sizeof(SymbolInfo) * SYMBOL_INFO_CHUNK_SIZE);

    Stack_push(self->chunks, chunk);
    self->symbol_info_pool = (SymbolInfo*)chunk;

    for (i = 0; i < (SYMBOL_INFO_CHUNK_SIZE - 1); i++) {
      SymbolInfo* c = (chunk + i);
      SymbolInfo* cn = (chunk + i + 1);
      c->next = cn;
    }
  }

  res = self->symbol_info_pool;
  self->symbol_info_pool = res->next;

  nusmv_assert((SymbolInfo*)NULL != res);

  return res;
}

/*!
  \brief Frees the given SymbolInfo instance

  Pushes the given symbol info instance in the
                      SymbolInfo's instances pool, for future re-use
*/
static inline void
symb_cache_free_symb_info(const SymbCache_ptr self, SymbolInfo* si)
{
  SymbolInfo* tmp = self->symbol_info_pool;
  self->symbol_info_pool = si;

  memset(si, 0, sizeof(SymbolInfo));

  si->next = tmp;
}

/*!
  \brief A new symbols is added in the cache

  The given symbol is added in the cache.

                      The given symbol is added in the symbol_hash
                      associated to the relative SymbolInfo, and in
                      the symbols array (which may need a resizing).
*/
static inline void
symb_cache_new_symbol(SymbCache_ptr self, const node_ptr sym,
                      SymbolInfo* symbol_info)
{
  unsigned int index = self->symbols_index;
  SymbolInfo* old_si = GET_SYMBOL(self, sym);
  boolean redefined = SI_IS_REMOVED(old_si);

  insert_assoc(self->symbol_hash, sym, (node_ptr)symbol_info);

  if (index == self->symbols_allocated) {
    self->symbols_allocated *= 2;
    self->symbols = REALLOC(node_ptr, self->symbols, self->symbols_allocated);
  }

  self->symbols[index] = sym;
  symbol_info->position = index;

  self->symbols_index++;

  switch (SI_CATEGORY(symbol_info)) {

  case STT_INPUT_VAR: ++self->input_vars_num; break;
  case STT_FROZEN_VAR: ++self->frozen_vars_num; break;
  case STT_STATE_VAR: ++self->state_vars_num; break;
  case STT_VARIABLE_ARRAY: ++self->variable_arrays_num; break;
  case STT_CONSTANT: ++self->constants_num; break;
  case STT_DEFINE: ++self->defines_num; break;
  case STT_ARRAY_DEFINE: ++self->array_defines_num; break;
  case STT_PARAMETER: ++self->parameters_num; break;
  case STT_FUNCTION: ++self->functions_num; break;

  default:
    error_unreachable_code();
    break;
  }

  /* Call triggers */
  if (NODE_LIST(NULL) != self->add_triggers) {
    ListIter_ptr iter;
    if (opt_verbose_level_gt(self->options, 4)) {
      Logger_nlog(self->logger, self->printer,
                  "SymbCache: Calling add symbol "
                  "triggers for symbol '%N'\n", sym);
    }

    NODE_LIST_FOREACH(self->add_triggers, iter) {
      SymbCacheRemoveSymbolStruct* srss =
        (SymbCacheRemoveSymbolStruct*)NodeList_get_elem_at(self->add_triggers,
                                                           iter);
      srss->trigger(self->symb_table, sym, ST_TRIGGER_SYMBOL_ADD, srss->arg);
    }
  }

  if (redefined && NODE_LIST(NULL) != self->redef_triggers) {
    ListIter_ptr iter;
    if (opt_verbose_level_gt(self->options, 4)) {
      Logger_nlog(self->logger, self->printer,
                  "SymbCache: Calling redeclare symbol "
                  "triggers for symbol '%N'\n", sym);
    }

    NODE_LIST_FOREACH(self->redef_triggers, iter) {
      SymbCacheRemoveSymbolStruct* srss =
        (SymbCacheRemoveSymbolStruct*)NodeList_get_elem_at(
                                                self->redef_triggers,
                                                iter);
      srss->trigger(self->symb_table, sym,
                    ST_TRIGGER_SYMBOL_REDECLARE, srss->arg);
    }
  }
}

/*!
  \brief Shrinks the symbols array if needed

  Shrinks the symbols array if needed
*/
static inline void
symb_cache_check_and_shrink_symbols(SymbCache_ptr self)
{
  /* ~75% of the list is empty.. shrink */
  if ((self->symbols_allocated > INITIAL_SYMBOLS_ARRAY_SIZE) &&
      (((double) self->symbols_empty /
        (double) self->symbols_allocated) > 0.75)) {
    unsigned int i, j;
    node_ptr* old_symbols = self->symbols;
    node_ptr* new_symbols;

    self->symbols_allocated /= 2;
    new_symbols = ALLOC(node_ptr, self->symbols_allocated);
    self->symbols = new_symbols;

    for (i = 0, j = 0; j < self->symbols_index; ++j) {
      node_ptr sym = old_symbols[j];

      /* The symbol has not been removed.. */
      if (sym != Nil) {
        SymbolInfo* si = (SymbolInfo*)find_assoc(self->symbol_hash, sym);
        nusmv_assert(SI_IS_DECLARED(si));

        nusmv_assert(i < self->symbols_allocated);
        new_symbols[i] = sym;

        /* Update the position */
        si->position = i;

        ++i;
      }
    }

    self->symbols_index = i;

    /* After shrinking, there are no empty cells */
    self->symbols_empty = 0;

    FREE(old_symbols);
  }
}

/*!
  \brief Removes a symbol from the cache

  Removes a symbol from the cache.

                      The given symbol is removed from the symbol_hash
                      and from the symbols array. The associated
                      SymbolInfo is freed (i.e. is pushed in the
                      pool). This operation costs O(1), but if
                      shrink_if_needed is true and the symbols array
                      has too many holes, it is shrinked (O(n + h)
                      with n = number of symbols and h = number of
                      holes)
*/
static inline void
symb_cache_remove_symbol(SymbCache_ptr self, const node_ptr sym,
                         const boolean shrink_if_needed)
{
  SymbolInfo* si = (SymbolInfo*)find_assoc(self->symbol_hash, sym);
  ListIter_ptr iter;

  nusmv_assert(SI_IS_DECLARED(si));

  /* Call triggers */
  if (NODE_LIST(NULL) != self->rem_triggers) {
    if (opt_verbose_level_gt(self->options, 4)) {
      Logger_nlog(self->logger, self->printer,
                  "SymbCache: Calling remove symbol "
                  "triggers for symbol '%N'\n", sym);
    }

    NODE_LIST_FOREACH(self->rem_triggers, iter) {
      SymbCacheRemoveSymbolStruct* srss =
        (SymbCacheRemoveSymbolStruct*)NodeList_get_elem_at(self->rem_triggers,
                                                           iter);
      srss->trigger(self->symb_table, sym,
                    ST_TRIGGER_SYMBOL_REMOVE, srss->arg);
    }
  }

  switch (SI_CATEGORY(si)) {
  case STT_INPUT_VAR:
    --self->input_vars_num;
    SymbType_destroy(SI_VAR_TYPE(si));
    break;

  case STT_FROZEN_VAR:
    --self->frozen_vars_num;
    SymbType_destroy(SI_VAR_TYPE(si));
    break;

  case STT_STATE_VAR:
    --self->state_vars_num;
    SymbType_destroy(SI_VAR_TYPE(si));
    break;

  case STT_VARIABLE_ARRAY:
    --self->variable_arrays_num;
    SymbType_destroy(SI_VAR_TYPE(si));
    break;
  
    /* Constants have their own reference counter.. */
  case STT_CONSTANT:
    SI_SET_CONSTANT_COUNT(si, SI_GET_CONSTANT_COUNT(si) - 1);

    /* It is still not the moment for removing this constant */
    if (SI_GET_CONSTANT_COUNT(si) > 0) { return; }
    else { --self->constants_num; }
    break;

  case STT_DEFINE: --self->defines_num; break;
  case STT_ARRAY_DEFINE: --self->array_defines_num; break;
  case STT_PARAMETER: --self->parameters_num; break;

  case STT_FUNCTION:
    --self->functions_num;
    SymbType_destroy(SI_FUNCTION_TYPE(si));
    break;

  default:
    error_unreachable_code();
    break;
  }

  insert_assoc(self->symbol_hash, sym, NODE_PTR(SI_SYMBOL_REMOVED));

  /* Remove from the list */
  self->symbols[si->position] = Nil;
  self->symbols_empty++;

  symb_cache_free_symb_info(self, si);

  if (shrink_if_needed) {
    symb_cache_check_and_shrink_symbols(self);
  }
}
