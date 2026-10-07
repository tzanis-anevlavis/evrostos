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
  \brief Implementation of the system-wide SymbLayer

  \todo: Missing description

*/


#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/compile/symb_table/SymbLayer.h"
#include "nusmv/core/compile/symb_table/SymbLayer_private.h"
#include "nusmv/core/compile/symb_table/SymbCache.h"
#include "nusmv/core/compile/symb_table/SymbCache_private.h"
#include "nusmv/core/compile/symb_table/symb_table_int.h"

#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/compile/symb_table/NFunction.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"

#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/utils/ErrorMgr.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct SymbLayer_TAG {
  OptsHandler_ptr options;
  StreamMgr_ptr streams;
  MasterPrinter_ptr printer;
  ErrorMgr_ptr errors;
  Logger_ptr logger;

  char* name;
  LayerInsertPolicy insert_policy;

  /* the current number of encodings self is registered with */
  int committed_to_encs;

  /* Insertion ordered list */
  node_ptr* symbols;
  unsigned int symbols_allocated;
  unsigned int symbols_index;
  unsigned int symbols_empty;

  /* For removal in constant time */
  hash_ptr symbol2position;

  SymbCache_ptr cache;

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

  int bool_state_vars_num;
  int bool_input_vars_num;
  int bool_frozen_vars_num;
} SymbLayer;



/*---------------------------------------------------------------------------*/
/* macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define INITIAL_SYMBOLS_ARRAY_SIZE 8

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define IS_SYMBOL_UNDEF(self, sym)                      \
  (Nil == find_assoc(self->symbol2position, sym))

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void symb_layer_init(SymbLayer_ptr self, const char* name,
                            const LayerInsertPolicy policy,
                            SymbCache_ptr cache);

static void symb_layer_deinit(SymbLayer_ptr self, boolean clean_cache);

static inline void
symb_layer_check_and_shrink_symbols(SymbLayer_ptr self);


static inline void
symb_layer_remove_symbol(SymbLayer_ptr self, const node_ptr sym);

static inline void
symb_layer_new_symbol(SymbLayer_ptr self, const node_ptr sym);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

SymbLayer_ptr SymbLayer_create(const char* name,
                               const LayerInsertPolicy policy,
                               SymbCache_ptr cache)
{
  SymbLayer_ptr self = ALLOC(SymbLayer, 1);

  SYMB_LAYER_CHECK_INSTANCE(self);

  symb_layer_init(self, name, policy, cache);
  return self;
}

void SymbLayer_destroy(SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);

  symb_layer_deinit(self, true);
  FREE(self);
}

void SymbLayer_destroy_raw(SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);

  symb_layer_deinit(self, false);
  FREE(self);
}

NuSMVEnv_ptr SymbLayer_get_environment(const SymbLayer_ptr self)
{
  return SymbCache_get_environment(self->cache);
}

void SymbLayer_set_name(SymbLayer_ptr self, const char* new_name)
{
  SYMB_LAYER_CHECK_INSTANCE(self);

  /* frees the previous name if needed */
  if (self->name != (char*) NULL) { FREE(self->name); }
  if (new_name != (char*) NULL)  self->name = util_strsav((char*) new_name);
  else self->name = (char*) NULL;
}

const char* SymbLayer_get_name(const SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return (const char*) self->name;
}

LayerInsertPolicy SymbLayer_get_insert_policy(const SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  if (self->insert_policy == SYMB_LAYER_POS_DEFAULT) return SYMB_LAYER_POS_BOTTOM;
  else return self->insert_policy;
}

boolean SymbLayer_must_insert_before(const SymbLayer_ptr self,
                                     const SymbLayer_ptr other)
{
  LayerInsertPolicy p1, p2;

  SYMB_LAYER_CHECK_INSTANCE(self);

  p1 = SymbLayer_get_insert_policy(self);
  p2 = SymbLayer_get_insert_policy(other);

  /* checks that p1 and p2 do not refer the same forced position */
  nusmv_assert((p1 != p2) || ((p1 != SYMB_LAYER_POS_FORCE_TOP) &&
                              (p1 != SYMB_LAYER_POS_FORCE_BOTTOM)));

  switch (p1) {
  case SYMB_LAYER_POS_FORCE_TOP: return true;

  case SYMB_LAYER_POS_TOP:
    return p2 != SYMB_LAYER_POS_FORCE_TOP;

  case SYMB_LAYER_POS_DEFAULT:
  case SYMB_LAYER_POS_BOTTOM: return p2 == SYMB_LAYER_POS_FORCE_BOTTOM;

  default:
    ErrorMgr_internal_error(self->errors, "Unexpected layer insertion policy");
  }

  return false;
}

void SymbLayer_committed_to_enc(SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);

  self->committed_to_encs += 1;

  if (opt_verbose_level_gt(self->options, 4)) {
    Logger_log(self->logger,
               "SymbLayer '%s': committed to %d encodings\n",
               SymbLayer_get_name(self), self->committed_to_encs);
  }
}

void SymbLayer_removed_from_enc(SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  nusmv_assert(self->committed_to_encs > 0);

  self->committed_to_encs -= 1;

  if (opt_verbose_level_gt(self->options, 4)) {
    Logger_log(self->logger,
               "SymbLayer '%s': removed from encoding (%d remaining)\n",
               SymbLayer_get_name(self), self->committed_to_encs);
  }
}

boolean SymbLayer_can_declare_function(const SymbLayer_ptr self,
                                       const node_ptr name)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return !SymbCache_is_symbol_declared(self->cache, name);
}

boolean SymbLayer_can_declare_constant(const SymbLayer_ptr self,
                                       const node_ptr name)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return IS_SYMBOL_UNDEF(self, name);
}

boolean SymbLayer_can_declare_var(const SymbLayer_ptr self,
                                  const node_ptr name)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return !SymbCache_is_symbol_declared(self->cache, name);
}

boolean SymbLayer_can_declare_define(const SymbLayer_ptr self,
                                     const node_ptr name)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return !SymbCache_is_symbol_declared(self->cache, name);
}

boolean SymbLayer_can_declare_parameter(const SymbLayer_ptr self,
                                        const node_ptr name)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return !SymbCache_is_symbol_declared(self->cache, name);
}

boolean SymbLayer_can_declare_array_define(const SymbLayer_ptr self,
                                           const node_ptr name)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return !SymbCache_is_symbol_declared(self->cache, name);
}

boolean SymbLayer_can_declare_variable_array(const SymbLayer_ptr self,
                                             const node_ptr name)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return !SymbCache_is_symbol_declared(self->cache, name);
}

void SymbLayer_declare_function(SymbLayer_ptr self, node_ptr name,
                                node_ptr ctx, SymbType_ptr type)
{
  SYMB_LAYER_CHECK_INSTANCE(self);

  nusmv_assert(SymbLayer_can_declare_function(self, name));

  SymbCache_new_function(self->cache, name, ctx, type);

  symb_layer_new_symbol(self, name);

  ++self->functions_num;

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_nlog(self->logger, self->printer,
                "SymbLayer '%s': declared new function '%N'\n",
                self->name, name);
  }
}

void SymbLayer_declare_constant(SymbLayer_ptr self, node_ptr name)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  nusmv_assert(SymbLayer_can_declare_constant(self, name));

  SymbCache_new_constant(self->cache, name);

  symb_layer_new_symbol(self, name);

  ++self->constants_num;

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_nlog(self->logger, self->printer,
                "SymbLayer '%s': declared new constant '%N'\n",
                self->name, name);
  }
}

void SymbLayer_declare_input_var(SymbLayer_ptr self, node_ptr var_name,
                                 SymbType_ptr type)
{
  SYMB_LAYER_CHECK_INSTANCE(self);

  /* not already declared */
  if (!SymbLayer_can_declare_var(self, var_name)) {
    StreamMgr_nprint_error(self->streams, self->printer,
                           "Error: Cannot declare input variable '%N'\n",
                           var_name);
    ErrorMgr_internal_error(self->errors, "Symbol already declared");
  }

  SymbCache_new_input_var(self->cache, var_name, type);

  symb_layer_new_symbol(self, var_name);

  ++self->input_vars_num;
  if (SymbType_is_boolean(type)) {
    ++self->bool_input_vars_num;
  }

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_nlog(self->logger, self->printer,
                "SymbLayer '%s': declared new input variable '%N'\n",
                self->name, var_name);
  }

}

void SymbLayer_declare_state_var(SymbLayer_ptr self, node_ptr var_name,
                                 SymbType_ptr type)
{
  SYMB_LAYER_CHECK_INSTANCE(self);

  if (! SymbLayer_can_declare_var(self, var_name)) {
    StreamMgr_nprint_error(self->streams, self->printer,
                           "Error: Cannot declare state variable '%N'\n",
                           var_name);
    ErrorMgr_internal_error(self->errors, "Symbol already declared");
  }

  SymbCache_new_state_var(self->cache, var_name, type);

  symb_layer_new_symbol(self, var_name);

  ++self->state_vars_num;
  if (SymbType_is_boolean(type)) {
    ++self->bool_state_vars_num;
  }

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_nlog(self->logger, self->printer,
                "SymbLayer '%s': declared new state variable '%N'\n",
                self->name, var_name);
  }
}

void SymbLayer_declare_frozen_var(SymbLayer_ptr self, node_ptr var_name,
                                  SymbType_ptr type)
{
  SYMB_LAYER_CHECK_INSTANCE(self);

  if (! SymbLayer_can_declare_var(self, var_name)) {
    StreamMgr_nprint_error(self->streams, self->printer,
                           "Error: Cannot declare frozen variable '%N'\n",
                           var_name);
    ErrorMgr_internal_error(self->errors, "Symbol already declared");
  }

  SymbCache_new_frozen_var(self->cache, var_name, type);

  symb_layer_new_symbol(self, var_name);

  ++self->frozen_vars_num;
  if (SymbType_is_boolean(type)) {
    ++self->bool_frozen_vars_num;
  }

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_nlog(self->logger, self->printer,
                "SymbLayer '%s': declared new frozen variable '%N'\n",
                self->name, var_name);
  }
}

void
SymbLayer_redeclare_state_as_frozen_var(SymbLayer_ptr self, node_ptr var)
{
  SymbType_ptr type;
  SymbCache_redeclare_state_as_frozen_var(self->cache, var);

  ++self->frozen_vars_num;
  --self->state_vars_num;

  type = SymbCache_get_var_type(self->cache, var);

  if (SymbType_is_boolean(type)) {
    ++self->bool_frozen_vars_num;
    --self->bool_state_vars_num;
  }

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_nlog(self->logger, self->printer,
                "SymbLayer '%s': redeclared state var '%N' as frozen var\n",
                self->name, var);
  }
}

void SymbLayer_declare_define(SymbLayer_ptr self, node_ptr name,
                              node_ptr context, node_ptr definition)
{
  SYMB_LAYER_CHECK_INSTANCE(self);

  if (!SymbLayer_can_declare_define(self, name)) {
    StreamMgr_nprint_error(self->streams, self->printer,
                           "Error: Cannot declare DEFINE '%N'\n",
                           name);
    ErrorMgr_internal_error(self->errors, "SymbLayer_declare_define: name already declared\n");
  }

  SymbCache_new_define(self->cache, name, context, definition);

  symb_layer_new_symbol(self, name);

  ++self->defines_num;

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_nlog(self->logger, self->printer,
                "SymbLayer '%s': declared new DEFINE '%N'\n",
                self->name, name);
  }
}

void SymbLayer_declare_parameter(SymbLayer_ptr self, node_ptr formal,
                                 node_ptr context, node_ptr actual)
{
  SYMB_LAYER_CHECK_INSTANCE(self);

  if (!SymbLayer_can_declare_parameter(self, formal)) {
    StreamMgr_nprint_error(self->streams, self->printer,
                           "Error: Cannot declare parameter '%N'\n",
                           formal);
    ErrorMgr_internal_error(self->errors, "SymbLayer_declare_parameter: formal param already declared\n");
  }

  SymbCache_new_parameter(self->cache, formal, context, actual);

  symb_layer_new_symbol(self, formal);

  ++self->parameters_num;

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_nlog(self->logger, self->printer,
                "SymbLayer '%s': declared new parameter '%N'\n",
                self->name, formal);
  }
}

void SymbLayer_declare_array_define(SymbLayer_ptr self, node_ptr name,
                                    node_ptr context, node_ptr definition)
{
  SYMB_LAYER_CHECK_INSTANCE(self);

  if (!SymbLayer_can_declare_array_define(self, name)) {
    StreamMgr_nprint_error(self->streams, self->printer,
                           "Error: Cannot declare define array '%N'\n",
                           name);
    ErrorMgr_internal_error(self->errors, "SymbLayer_declare_define: name already declared\n");
  }

  SymbCache_new_array_define(self->cache, name, context, definition);

  symb_layer_new_symbol(self, name);

  ++self->array_defines_num;

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_nlog(self->logger, self->printer,
                "SymbLayer '%s': declared new define array '%N'\n",
                self->name, name);
  }
}

void SymbLayer_declare_variable_array(SymbLayer_ptr self, node_ptr name,
                                      SymbType_ptr type)
{
  SYMB_LAYER_CHECK_INSTANCE(self);

  if (! SymbLayer_can_declare_variable_array(self, name)) {
    StreamMgr_nprint_error(self->streams, self->printer,
                           "Error: Cannot declare variable array '%N'\n",
                           name);
    ErrorMgr_internal_error(self->errors, "Symbol already declared");
  }

  SymbCache_new_variable_array(self->cache, name, type);

  symb_layer_new_symbol(self, name);

  ++self->variable_arrays_num;

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_nlog(self->logger, self->printer,
                "SymbLayer '%s': declared new variable_array '%N'\n",
                self->name, name);
  }
}

void SymbLayer_remove_var(SymbLayer_ptr self, node_ptr name)
{
  SymbType_ptr type;

  /* IMPORTANT: do not remove this assertion! (read comment) */
  nusmv_assert(self->committed_to_encs == 0); /* not in use */

  nusmv_assert(SymbCache_is_symbol_var(self->cache, name));

  type = SymbCache_get_var_type(self->cache, name);


  if (SymbCache_is_symbol_state_var(self->cache, name)) {
    --self->state_vars_num;
    if (SymbType_is_boolean(type)) {
      --self->bool_state_vars_num;
    }
  }
  else if (SymbCache_is_symbol_frozen_var(self->cache, name)) {
    --self->frozen_vars_num;
    if (SymbType_is_boolean(type)) {
      --self->bool_frozen_vars_num;
    }
  }
  else if (SymbCache_is_symbol_input_var(self->cache, name)) {
    --self->input_vars_num;
    if (SymbType_is_boolean(type)) {
      --self->bool_input_vars_num;
    }
  }
  else {
    error_unreachable_code();
  }

  /* removes the variable  */
  SymbCache_remove_var(self->cache, name);

  symb_layer_remove_symbol(self, name);

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_nlog(self->logger, self->printer,
                "SymbLayer '%s': removed variable '%N'\n",
                self->name, name);
  }
}

void SymbLayer_remove_define(SymbLayer_ptr self, node_ptr name)
{
  /* IMPORTANT: do not remove this assertion! (read comment) */
  nusmv_assert(self->committed_to_encs == 0); /* not in use */

  nusmv_assert(SymbCache_is_symbol_define(self->cache, name));

  /* removes the variable  */
  SymbCache_remove_define(self->cache, name);

  symb_layer_remove_symbol(self, name);

  --self->defines_num;

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_nlog(self->logger, self->printer,
                "SymbLayer '%s': removed define '%N'\n",
                self->name, name);
  }
}

void SymbLayer_remove_function(SymbLayer_ptr self, node_ptr name)
{
  /* IMPORTANT: do not remove this assertion! (read comment) */
  nusmv_assert(self->committed_to_encs == 0); /* not in use */

  nusmv_assert(SymbCache_is_symbol_function(self->cache, name));

  /* removes the variable  */
  SymbCache_remove_function(self->cache, name);

  symb_layer_remove_symbol(self, name);

  --self->functions_num;

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_nlog(self->logger, self->printer,
                "SymbLayer '%s': removed function '%N'\n",
                self->name, name);
  }
}

void SymbLayer_remove_variable_array(SymbLayer_ptr self, node_ptr name)
{
  /* IMPORTANT: do not remove this assertion! (read comment) */
  nusmv_assert(self->committed_to_encs == 0); /* not in use */

  nusmv_assert(SymbCache_is_symbol_variable_array(self->cache, name));

  /* removes the variable  */
  SymbCache_remove_variable_array(self->cache, name);

  symb_layer_remove_symbol(self, name);

  --self->variable_arrays_num;

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_nlog(self->logger, self->printer,
                "SymbLayer '%s': removed array '%N'\n",
                self->name, name);
  }
}

void SymbLayer_remove_symbol(SymbLayer_ptr self, node_ptr name)
{
  if (SymbCache_is_symbol_var(self->cache, name)) {
    SymbLayer_remove_var(self, name);
  }
  else if (SymbCache_is_symbol_define(self->cache, name)) {
    SymbLayer_remove_define(self, name);
  }
  else if (SymbCache_is_symbol_function(self->cache, name)) {
    SymbLayer_remove_function(self, name);
  }
  else if (SymbCache_is_symbol_variable_array(self->cache, name)) {
    SymbLayer_remove_variable_array(self, name);
  }
  /* It is a programming error to try to remove a symbol that is neither a var,
     a define, a function or a variable array */
  else nusmv_assert(false);

}

int SymbLayer_get_symbols_num(const SymbLayer_ptr self)
{
  int res = 0;

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

int SymbLayer_get_vars_num(const SymbLayer_ptr self)
{
  int res = 0;

  res += self->state_vars_num;
  res += self->input_vars_num;
  res += self->frozen_vars_num;

  return res;
}

int SymbLayer_get_constants_num(const SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return self->constants_num;
}

int SymbLayer_get_state_vars_num(const SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return self->state_vars_num;
}

int SymbLayer_get_bool_state_vars_num(const SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return self->bool_state_vars_num;
}

int SymbLayer_get_frozen_vars_num(const SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return self->frozen_vars_num;
}

int SymbLayer_get_bool_frozen_vars_num(const SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return self->bool_frozen_vars_num;
}

int SymbLayer_get_input_vars_num(const SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return self->input_vars_num;
}

int SymbLayer_get_bool_input_vars_num(const SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return self->bool_input_vars_num;
}

int SymbLayer_get_defines_num(const SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return self->defines_num;
}

int SymbLayer_get_functions_num(const SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return self->functions_num;
}

int SymbLayer_get_parameters_num(const SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return self->parameters_num;
}

int SymbLayer_get_array_defines_num(const SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return self->array_defines_num;
}

int SymbLayer_get_variable_arrays_num(const SymbLayer_ptr self)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return self->variable_arrays_num;
}

boolean SymbLayer_is_variable_in_layer(SymbLayer_ptr self, node_ptr name)
{
  SYMB_LAYER_CHECK_INSTANCE(self);

  /* Must be a symbol in this layer and a variable in the cache */
  return (Nil != find_assoc(self->symbol2position, name) &&
          SymbCache_is_symbol_var(self->cache, name));
}

boolean SymbLayer_is_symbol_in_layer(SymbLayer_ptr self, node_ptr name)
{
  SYMB_LAYER_CHECK_INSTANCE(self);
  return (Nil != find_assoc(self->symbol2position, name));
}

void SymbLayer_iter_next(const SymbLayer_ptr self,
                         SymbLayerIter* iter)
{
  node_ptr sym;
  SymbTableType type;
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
    if (((SymbLayerIterFilterFun)NULL != iter->filter) &&
        !(iter->filter(self, sym, iter->arg))) {
      valid = false;
      continue;
    }

    type = SymbCache_get_symbol_type(self->cache, sym);

  } while (!valid || ((type & iter->mask) == 0)); /* Not a match */

}

void SymbLayer_gen_iter(const SymbLayer_ptr self,
                        SymbLayerIter* iter,
                        const unsigned int mask)
{
  SymbTableType type;
  node_ptr sym;
  unsigned int index = 0;

  iter->mask = mask;
  iter->filter = (SymbLayerIterFilterFun)NULL;
  iter->arg = NULL;

  /* The list is not empty */
  if (index != self->symbols_index) {
    sym = self->symbols[index];

    type = (Nil != sym) ? \
      SymbCache_get_symbol_type(self->cache, sym) : STT_NONE;

    /* Current symbol does not match */
    while ((Nil == sym) || (type & mask) == 0) {
      ++index;

      /* The end.. */
      if (index == self->symbols_index) { break; }

      sym = self->symbols[index];

      /* Empty cell, continue with next */
      if (Nil == sym) { continue; }

      type = SymbCache_get_symbol_type(self->cache, sym);
    }
  }

  iter->index = index;
}

void SymbLayer_iter_set_filter(const SymbLayer_ptr self,
                               SymbLayerIter* iter,
                               SymbLayerIterFilterFun filter,
                               void* arg)
{
  unsigned int index = iter->index;
  SymbTableType mask = iter->mask;
  SymbTableType type;
  node_ptr sym;

  iter->filter = filter;
  iter->arg = arg;

  /* The list is not empty */
  if (index != self->symbols_index) {
    sym = self->symbols[index];
    type = SymbCache_get_symbol_type(self->cache, sym);

    /* Current symbol does not match */
    while ((type & mask) == 0 || !(filter(self, sym, arg))) {
      ++index;

      /* The end.. */
      if (index == self->symbols_index) { break; }

      sym = self->symbols[index];

      /* Empty cell, continue with next */
      if (Nil == sym) { continue; }

      type = SymbCache_get_symbol_type(self->cache, sym);
    }
  }

  iter->index = index;
}

boolean SymbLayer_iter_is_end(const SymbLayer_ptr self,
                              const SymbLayerIter* iter)
{
  return self->symbols_index == iter->index;
}

node_ptr SymbLayer_iter_get_symbol(const SymbLayer_ptr self,
                                   const SymbLayerIter* iter)
{
  nusmv_assert(!SymbLayer_iter_is_end(self, iter));
  return self->symbols[iter->index];
}

Set_t SymbLayer_iter_to_set(const SymbLayer_ptr self, SymbLayerIter iter)
{
  Set_t res = Set_MakeEmpty();

  while (!SymbLayer_iter_is_end(self, &iter)) {
    res = Set_AddMember(res, SymbLayer_iter_get_symbol(self, &iter));
    SymbLayer_iter_next(self, &iter);
  }

  return res;
}

NodeList_ptr SymbLayer_iter_to_list(const SymbLayer_ptr self, SymbLayerIter iter)
{
  NodeList_ptr res = NodeList_create();

  while (!SymbLayer_iter_is_end(self, &iter)) {
    NodeList_append(res, SymbLayer_iter_get_symbol(self, &iter));
    SymbLayer_iter_next(self, &iter);
  }

  return res;
}

unsigned int SymbLayer_iter_count(const SymbLayer_ptr self, SymbLayerIter iter)
{
  unsigned int res = 0;

  while (!SymbLayer_iter_is_end(self, &iter)) {
    ++res;
    SymbLayer_iter_next(self, &iter);
  }

  return res;
}

boolean SymbLayer_iter_filter_bool_vars(const SymbLayer_ptr self,
                                        const node_ptr sym,
                                        void* arg)
{
  return SymbCache_is_symbol_var(self->cache, sym) &&
    SymbType_is_boolean(SymbCache_get_var_type(self->cache, sym));
}

/*--------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Private method called by the constructor

  Called by the constructor
*/
static void
symb_layer_init(SymbLayer_ptr self, const char* name,
                const LayerInsertPolicy policy, SymbCache_ptr cache)
{
  NuSMVEnv_ptr env = SymbCache_get_environment(cache);

  self->options = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  self->streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  self->logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
  self->printer = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  self->errors = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  self->name = (char*) NULL;
  SymbLayer_set_name(self, name);

  self->insert_policy = policy;
  self->committed_to_encs = 0;
  self->cache = cache;

  self->symbols_allocated = INITIAL_SYMBOLS_ARRAY_SIZE;
  self->symbols_index = 0;
  self->symbols_empty = 0;
  self->symbols = ALLOC(node_ptr, self->symbols_allocated);

  self->symbol2position = new_assoc();

  /* Counters */
  self->constants_num = 0;
  self->state_vars_num = 0;
  self->input_vars_num = 0;
  self->frozen_vars_num = 0;
  self->bool_state_vars_num = 0;
  self->bool_input_vars_num = 0;
  self->bool_frozen_vars_num = 0;
  self->defines_num = 0;
  self->functions_num = 0;
  self->array_defines_num = 0;
  self->variable_arrays_num = 0;
  self->parameters_num = 0;
}

/*!
  \brief Private method called by the destructor

  Called by the destructor
*/
static void symb_layer_deinit(SymbLayer_ptr self, boolean clean_cache)
{
  nusmv_assert(self->committed_to_encs == 0); /* not in use by encs */

  /* frees the name */
  if (self->name != (char*) NULL) { FREE(self->name); }

  if (clean_cache) {
    SymbCache_remove_symbols(self->cache, self->symbols, self->symbols_index);
  }

  free_assoc(self->symbol2position);
  FREE(self->symbols);
}

/*!
  \brief Adds the given symbol from the layer

  Adds the given symbol from the layer
*/
static inline void
symb_layer_new_symbol(SymbLayer_ptr self, const node_ptr sym)
{
  unsigned int index = self->symbols_index;

  /* Index is stored incremented by one, so it is possible to check
     for NULL */
  insert_assoc(self->symbol2position, sym, NODE_FROM_INT(index + 1));

  if (index == self->symbols_allocated) {
    self->symbols_allocated *= 2;
    self->symbols = REALLOC(node_ptr, self->symbols, self->symbols_allocated);
  }

  self->symbols[index] = sym;

  self->symbols_index++;
}

/*!
  \brief Removes the given symbol from the layer

  Removes the given symbol from the layer
*/
static inline void
symb_layer_remove_symbol(SymbLayer_ptr self, const node_ptr sym)
{
  unsigned int pos = NODE_TO_INT(remove_assoc(self->symbol2position, sym));

  nusmv_assert(0 != pos);

  /* Remove from the list */
  self->symbols[pos - 1] = Nil;
  self->symbols_empty++;

  symb_layer_check_and_shrink_symbols(self);
}

/*!
  \brief Shrinks the symbols array if needed

  Shrinks the symbols array if needed
*/
static inline void
symb_layer_check_and_shrink_symbols(SymbLayer_ptr self)
{
  /* ~75% of the list is empty.. shrink */
  if ((self->symbols_allocated > INITIAL_SYMBOLS_ARRAY_SIZE) &&
      ((double)self->symbols_empty / (double)self->symbols_allocated) > 0.75) {
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
        new_symbols[i] = sym;

        nusmv_assert(i < self->symbols_allocated);

        /* Update the position. Index is stored incremented by one, so
           it is possible to check for NULL */
        insert_assoc(self->symbol2position, sym, NODE_FROM_INT(i + 1));

        ++i;
      }
    }

    self->symbols_index = i;

    /* After shrinking, there are no empty cells */
    self->symbols_empty = 0;

    FREE(old_symbols);
  }
}

