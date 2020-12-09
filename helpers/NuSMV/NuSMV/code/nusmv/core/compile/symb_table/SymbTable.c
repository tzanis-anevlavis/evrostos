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
  \brief Implementation of the system-wide SymbolTable

  \todo: Missing description

*/


#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/compile/symb_table/SymbTable.h"

#include "nusmv/core/compile/symb_table/SymbLayer.h"
#include "nusmv/core/compile/symb_table/SymbLayer_private.h"
#include "nusmv/core/compile/symb_table/SymbCache.h"
#include "nusmv/core/compile/symb_table/SymbCache_private.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"

#include "nusmv/core/utils/Pair.h"
#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/compile/type_checking/TypeChecker.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/defs.h"
#include "nusmv/core/compile/symb_table/NFunction.h"

#include "nusmv/core/utils/TimerBench.h"

#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/UStringMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/EnvObject_private.h"
#include "nusmv/core/node/anonymizers/NodeAnonymizerBase.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef enum {
  ST_HANDLED_STRUCTS_HASH
} SymbTableHandledType;

typedef struct SymbTable_TAG
{
  INHERITS_FROM(EnvObject);

  OptsHandler_ptr options;
  StreamMgr_ptr streams;
  NodeMgr_ptr nodes;
  MasterPrinter_ptr printer;
  UStringMgr_ptr strings;

  SymbCache_ptr cache;
  hash_ptr categories;

  int temp_layer_name_suffix; /* used to create temporary names for layers */

  NodeList_ptr layers; /* the list of owned layers */

  hash_ptr class_layers; /* the list of layers organized by class */
  NodeList_ptr class_names; /* the list of class names */
  const char* default_class_name; /* name of the default class name */

  hash_ptr name2layer; /* to associate layers and names */

  /* A counter for declaration of determinization variables */
  size_t det_counter;

  TypeChecker_ptr tc; /* this is the type checker owned by the ST */

  ResolveSymbol_ptr resolver;

  hash_ptr handled_structures;
} SymbTable;

/**Union**********************************************************************

   Synopsis    [Allows to populate an array with both function pointers and
                object pointers]

   Description [Used in debugging code in SymbTable_get_handled_hash_ptr.]

******************************************************************************/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef union debug_union_TAG {
  hash_ptr hash_type;
  ST_PFICPCP compare_func_type;
  ST_PFICPI hash_func_type;
  ST_PFSR destroy_func_type;
  SymbTableTriggerFun trigger_type;
  int handler_type;
  Pair_ptr pair_type;
  void* void_ptr_type;
} debug_union;

/*---------------------------------------------------------------------------*/
/* macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SYMB_TABLE_TEMP_LAYER_NAME "__TempLayer_%d"

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void symb_table_init(SymbTable_ptr self, NuSMVEnv_ptr env);
static void symb_table_deinit(SymbTable_ptr self);
static void symb_table_finalize(Object_ptr object, void* dummy);

static boolean
symb_table_layer_exists(const SymbTable_ptr self,
                        const char* layer_name);

static NodeList_ptr
symb_table_filter_layers_symbols(SymbTable_ptr self,
                                 const array_t* layer_names,
                                 SymbTableIter* iter);

static void
symb_table_layer_rename_in_class(SymbTable_ptr self,
                                 const char* class_name,
                                 const char* old_layer_name,
                                 const char* new_layer_name);

static array_t*
symb_table_get_layers_from_class(const SymbTable_ptr self,
                                 const char* class_name);

static array_t*
symb_table_create_layers_class(const SymbTable_ptr self,
                               const char* class_name);

static array_t*
symb_table_get_layers_class(const SymbTable_ptr self,
                            const char* class_name);

static node_ptr
symb_table_flatten_array_define(const SymbTable_ptr self,
                                const node_ptr body,
                                const node_ptr context);

static SymbCategory
symb_table_detect_expr_category(const SymbTable_ptr st,
                                const Expr_ptr expr);
/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

SymbTable_ptr SymbTable_create(NuSMVEnv_ptr env)
{
  SymbTable_ptr self = ALLOC(SymbTable, 1);

  SYMB_TABLE_CHECK_INSTANCE(self);

  symb_table_init(self, env);
  return self;
}

void SymbTable_destroy(SymbTable_ptr self)
{
  SYMB_TABLE_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

TypeChecker_ptr SymbTable_get_type_checker(const SymbTable_ptr self)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return self->tc;
}

SymbLayer_ptr SymbTable_create_layer(SymbTable_ptr self,
                                     const char* layer_name,
                                     const LayerInsertPolicy ins_policy)
{
  NuSMVEnv_ptr env;
  SymbLayer_ptr layer;
  ListIter_ptr iter;
  char tmp_name[50];

  SYMB_TABLE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));

  if (layer_name == (char*) NULL) {
    int c = snprintf(tmp_name, sizeof(tmp_name) / sizeof(tmp_name[0]),
                     SYMB_TABLE_TEMP_LAYER_NAME, self->temp_layer_name_suffix);
    SNPRINTF_CHECK(c, sizeof(tmp_name) / sizeof(tmp_name[0]));

    ++(self->temp_layer_name_suffix);
    layer_name = tmp_name;
  }

  nusmv_assert(! symb_table_layer_exists(self, layer_name));
  layer = SymbLayer_create(layer_name, ins_policy, self->cache);

  /* searches the insertion point, and inserts the layer */
  iter = NodeList_get_first_iter(self->layers);
  while (!ListIter_is_end(iter)) {
    if (SymbLayer_must_insert_before(layer,
                                     SYMB_LAYER(NodeList_get_elem_at(self->layers, iter)))) {
      NodeList_insert_before(self->layers, iter, (node_ptr) layer);
      break;
    }

    iter = ListIter_get_next(iter);
  }

  /* if not inserted yet: */
  if (ListIter_is_end(iter)) NodeList_append(self->layers, (node_ptr) layer);

  /* we duplicate the key here, to allow the caller to free layer_name
     if dinamically created. Memory will be freed by the deiniter of
     SymbTable */
  insert_assoc(self->name2layer, (node_ptr)  UStringMgr_find_string(self->strings, (char*) layer_name),
               (node_ptr) layer);

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "SymbTable: created new layer '%s'\n",  layer_name);
  }

  return layer;
}

void SymbTable_remove_layer(SymbTable_ptr self, SymbLayer_ptr layer)
{
  NuSMVEnv_ptr env;
  ListIter_ptr iter;

  SYMB_TABLE_CHECK_INSTANCE(self);
  nusmv_assert(symb_table_layer_exists(self, SymbLayer_get_name(layer)));

  env = EnvObject_get_environment(ENV_OBJECT(self));

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "SymbTable: destroying layer '%s'\n",
               SymbLayer_get_name(layer));
  }

  { /* removes the layer from all existing layers' classes: */
    ListIter_ptr iter;
    NODE_LIST_FOREACH(self->class_names, iter) {
      const char* class_name =
        (const char*)NodeList_get_elem_at(self->class_names, iter);

      symb_table_layer_rename_in_class(self, class_name,
                                       SymbLayer_get_name(layer),
                                       (const char*) NULL);
    }
  }

  /* searches the layer */
  iter = NodeList_get_first_iter(self->layers);
  while (!ListIter_is_end(iter)) {
    SymbLayer_ptr lay;
    lay = SYMB_LAYER(NodeList_get_elem_at(self->layers, iter));
    if (layer == lay) {
      /* found the layer */
      NodeList_remove_elem_at(self->layers, iter);

      insert_assoc(self->name2layer,
                   (node_ptr)  UStringMgr_find_string(self->strings, (char*) SymbLayer_get_name(layer)),
                   (node_ptr) NULL);
      SymbLayer_destroy(layer);
      return;
    }

    iter = ListIter_get_next(iter);
  }

  nusmv_assert(!ListIter_is_end(iter)); /* This layer had not been found */
}


/*!
  \brief Given its name, returns a layer

  NULL is returned when the layer does not exist within
   self.  Returned SymbLayer instance belongs to self.
*/

/* Why returning NULL if layer_name is NULL. Could be useful to return the
   default layer */
SymbLayer_ptr
SymbTable_get_layer(const SymbTable_ptr self, const char* layer_name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);

  /* If the name is null, then avoid the UStringMgr_find_string */
  if ((char*)NULL == layer_name) {
    return SYMB_LAYER(NULL);
  }

  return SYMB_LAYER( find_assoc(self->name2layer,
                                (node_ptr)  UStringMgr_find_string(self->strings, (char*) layer_name)) );
}

boolean
SymbTable_has_layer(const SymbTable_ptr self, const char* layer_name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);

  nusmv_assert(NULL != layer_name);

  return NULL != SYMB_LAYER(find_assoc(self->name2layer,
                                       (node_ptr)UStringMgr_find_string(self->strings,
                                                                        (char*)layer_name)) );
}

void SymbTable_rename_layer(const SymbTable_ptr self,
                            const char* layer_name, const char* new_name)
{
  NuSMVEnv_ptr env;
  SymbLayer_ptr layer;

  SYMB_TABLE_CHECK_INSTANCE(self);

  layer = SymbTable_get_layer(self, layer_name);
  SYMB_LAYER_CHECK_INSTANCE(layer);

  env = EnvObject_get_environment(ENV_OBJECT(self));

  { /* renames the layer from all existing layers' classes: */
    ListIter_ptr iter;
    NODE_LIST_FOREACH(self->class_names, iter) {
      const char* class_name =
        (const char*)NodeList_get_elem_at(self->class_names, iter);
      symb_table_layer_rename_in_class(self, class_name,
                                       layer_name, new_name);
    }
  }

  /* sets the new name */
  SymbLayer_set_name(layer, new_name);

  /* removes previous name association */
  insert_assoc(self->name2layer, (node_ptr)  UStringMgr_find_string(self->strings, (char*) layer_name),
               (node_ptr) NULL);

  /* adds the new name association */
  insert_assoc(self->name2layer, (node_ptr)  UStringMgr_find_string(self->strings, (char*) new_name),
               (node_ptr) layer);

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "SymbTable: renamed layer '%s' to '%s'\n",
               layer_name, new_name);
  }
}

NodeList_ptr SymbTable_get_layers(const SymbTable_ptr self)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return self->layers;
}

void SymbTable_set_default_layers_class_name(SymbTable_ptr self,
                                             const char* class_name)
{
  NuSMVEnv_ptr env;

  SYMB_TABLE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));

  if (self->default_class_name != (const char*) NULL) {
    FREE(self->default_class_name);
    self->default_class_name = (const char*) NULL;
  }
  if (class_name != (const char*) NULL) {
    self->default_class_name = util_strsav((char*) class_name);
    if (opt_verbose_level_gt(self->options, 3)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "SymbTable: default layers class set to '%s'\n",
                 class_name);
    }
  }
}

const char* SymbTable_get_default_layers_class_name(const SymbTable_ptr self)
{ return self->default_class_name; }

void SymbTable_create_layer_class(SymbTable_ptr self, const char* class_name)
{
  array_t* _class;

  SYMB_TABLE_CHECK_INSTANCE(self);
  _class = symb_table_create_layers_class(self, class_name);
  nusmv_assert(_class != (array_t*) NULL);
  return;
}

boolean  SymbTable_layer_class_exists(SymbTable_ptr self, const char* class_name)
{
  array_t* _class;

  SYMB_TABLE_CHECK_INSTANCE(self);
  _class = symb_table_get_layers_class(self, class_name);

  return ((array_t*) NULL != _class);
}

void SymbTable_layer_add_to_class(SymbTable_ptr self,
                                  const char* layer_name,
                                  const char* class_name)
{
  NuSMVEnv_ptr env;
  array_t* _class;

  SYMB_TABLE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));

  nusmv_assert(symb_table_layer_exists(self, layer_name));

  _class = symb_table_create_layers_class(self, class_name);

  { /* checks that the class does not contain the layer already */
    const char* name; int i;
    arrayForEachItem(const char*, _class, i, name) {
      if (strcmp(name, layer_name) == 0) return;
    }
  }

  /* adds to the array */
  array_insert_last(const char*, _class, util_strsav((char*) layer_name));

  if (opt_verbose_level_gt(self->options, 3)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "SymbTable: Added layer '%s' to class ", layer_name);

    if ((char*)NULL != class_name) {
      Logger_log(logger, "'%s'\n", class_name);
    }
    else {
      const char* def = SymbTable_get_default_layers_class_name(self);
      nusmv_assert((char*)NULL != def);
      Logger_log(logger, "'%s'\n", def);
    }

  }
}

void SymbTable_layer_remove_from_class(SymbTable_ptr self,
                                       const char* layer_name,
                                       const char* class_name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  symb_table_layer_rename_in_class(self, class_name,
                                   layer_name, (const char*) NULL);
}

array_t* SymbTable_get_class_layer_names(SymbTable_ptr self,
                                         const char* class_name)
{
  array_t* res;

  SYMB_TABLE_CHECK_INSTANCE(self);

  res = symb_table_get_layers_from_class(self, class_name);
  nusmv_assert(res != (array_t*) NULL);
  return res;
}

boolean SymbTable_is_layer_in_class(SymbTable_ptr self,
                                    const char* layer_name,
                                    const char* class_name)
{
  array_t* lays;
  const char* name;
  int i;

  SYMB_TABLE_CHECK_INSTANCE(self);

  lays = SymbTable_get_class_layer_names(self, class_name);
  arrayForEachItem(const char*, lays, i, name) {
    if (strcmp(name, layer_name) == 0) return true;
  }

  return false;
}

const char* SymbTable_get_class_of_layer(const SymbTable_ptr self,
                                         const char* layer_name)
{
  ListIter_ptr iter;
  SYMB_TABLE_CHECK_INSTANCE(self);

  NODE_LIST_FOREACH(self->class_names, iter) {
    const char* class_name =
      (const char*)NodeList_get_elem_at(self->class_names, iter);

    if (SymbTable_is_layer_in_class(self, layer_name, class_name)) {
      return class_name;
    }
  }

  return (const char*) NULL;
}

int SymbTable_get_vars_num(const SymbTable_ptr self)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_state_vars_num(self->cache) +
    SymbCache_get_input_vars_num(self->cache) +
    SymbCache_get_frozen_vars_num(self->cache);
}

int SymbTable_get_state_vars_num(const SymbTable_ptr self)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_state_vars_num(self->cache);
}

int SymbTable_get_frozen_vars_num(const SymbTable_ptr self)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_frozen_vars_num(self->cache);
}

int SymbTable_get_input_vars_num(const SymbTable_ptr self)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_input_vars_num(self->cache);
}

int SymbTable_get_constants_num(const SymbTable_ptr self)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_constants_num(self->cache);
}

int SymbTable_get_defines_num(const SymbTable_ptr self)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_defines_num(self->cache);
}

int SymbTable_get_array_defines_num(const SymbTable_ptr self)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_array_defines_num(self->cache);
}

int SymbTable_get_parameters_num(const SymbTable_ptr self)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_parameters_num(self->cache);
}

int SymbTable_get_functions_num(const SymbTable_ptr self)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_functions_num(self->cache);
}

int SymbTable_get_symbols_num(const SymbTable_ptr self)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_symbols_num(self->cache);
}

NodeList_ptr SymbTable_get_layers_sf_vars(SymbTable_ptr self,
                                          const array_t* layer_names)
{
  SymbTableIter iter;

  SYMB_TABLE_CHECK_INSTANCE(self);

  SymbTable_gen_iter(self, &iter, STT_STATE_VAR | STT_FROZEN_VAR);

  return symb_table_filter_layers_symbols(self, layer_names, &iter);
}

NodeList_ptr SymbTable_get_layers_sf_symbols(SymbTable_ptr self,
                                             const array_t* layer_names)
{
  SymbTableIter iter;

  SYMB_TABLE_CHECK_INSTANCE(self);

  SymbTable_gen_iter(self, &iter, STT_ALL);
  SymbTable_iter_set_filter(self, &iter,
                            SymbTable_iter_filter_sf_symbols, NULL);

  return symb_table_filter_layers_symbols(self, layer_names, &iter);
}

NodeList_ptr SymbTable_get_layers_i_symbols(SymbTable_ptr self,
                                            const array_t* layer_names)
{
  SymbTableIter iter;

  SYMB_TABLE_CHECK_INSTANCE(self);

  SymbTable_gen_iter(self, &iter, STT_ALL);
  SymbTable_iter_set_filter(self, &iter,
                            SymbTable_iter_filter_i_symbols, NULL);

  return symb_table_filter_layers_symbols(self, layer_names, &iter);
}

NodeList_ptr SymbTable_get_layers_i_vars(SymbTable_ptr self,
                                         const array_t* layer_names)
{
  SymbTableIter iter;

  SYMB_TABLE_CHECK_INSTANCE(self);

  SymbTable_gen_iter(self, &iter, STT_INPUT_VAR);

  return symb_table_filter_layers_symbols(self, layer_names, &iter);
}

NodeList_ptr
SymbTable_get_layers_sf_i_symbols(SymbTable_ptr self,
                                  const array_t* layer_names)
{
  SymbTableIter iter;

  SYMB_TABLE_CHECK_INSTANCE(self);

  SymbTable_gen_iter(self, &iter, STT_ALL);
  SymbTable_iter_set_filter(self, &iter,
                            SymbTable_iter_filter_sf_i_symbols, NULL);

  return symb_table_filter_layers_symbols(self, layer_names, &iter);}

NodeList_ptr
SymbTable_get_layers_sf_i_vars(SymbTable_ptr self,
                               const array_t* layer_names)
{
  SymbTableIter iter;

  SYMB_TABLE_CHECK_INSTANCE(self);

  SymbTable_gen_iter(self, &iter, STT_VAR);

  return symb_table_filter_layers_symbols(self, layer_names, &iter);
}

SymbType_ptr
SymbTable_get_var_type(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SYMB_TYPE(SymbCache_get_var_type(self->cache, name));
}

SymbType_ptr
SymbTable_get_symbol_type(const SymbTable_ptr self,
                          const node_ptr name)
{
  SymbType_ptr retval = NULL;

  SYMB_TABLE_CHECK_INSTANCE(self);

  if (SymbCache_is_symbol_var(self->cache, name)) {
    retval = SymbCache_get_var_type(self->cache, name);
  }
  else if (SymbCache_is_symbol_variable_array(self->cache, name)) {
    retval = SymbCache_get_variable_array_type(self->cache, name);
  }
  else if (SymbCache_is_symbol_function(self->cache, name)) {
    retval = SymbCache_get_function_type(self->cache, name);
  }
  else if (SymbCache_is_symbol_define(self->cache, name) ||
           SymbCache_is_symbol_parameter(self->cache, name) ||
           SymbCache_is_symbol_constant(self->cache, name)) {
    node_ptr expansion = Compile_FlattenSexpExpandDefine(self, name, NULL);

    /* This is done because the type checker returns a less precise type */
    if (SymbCache_is_symbol_declared(self->cache, expansion) &&
        ! SymbCache_is_symbol_constant(self->cache, expansion)) {
      retval = SymbTable_get_symbol_type(self, expansion);
    }
    else retval = TypeChecker_get_expression_type(self->tc, expansion, NULL);
  }
  else retval = NULL;

  return retval;
}

node_ptr
SymbTable_get_define_body(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_define_body(self->cache, name);
}

node_ptr
SymbTable_get_actual_parameter(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_actual_parameter(self->cache, name);
}

node_ptr
SymbTable_get_array_define_body(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_array_define_body(self->cache, name);
}

SymbType_ptr
SymbTable_get_variable_array_type(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_variable_array_type(self->cache, name);
}

SymbType_ptr
SymbTable_get_function_type(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_function_type(self->cache, name);
}

NFunction_ptr
SymbTable_get_function(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbType_get_nfunction_type(SymbTable_get_function_type(self, name));
}

node_ptr
SymbTable_get_function_context(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_function_context(self->cache, name);
}

node_ptr
SymbTable_get_array_define_flatten_body(const SymbTable_ptr self,
                                        const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);

  return symb_table_flatten_array_define(self,
                                         SymbCache_get_array_define_body(self->cache, name),
                                         SymbCache_get_array_define_context(self->cache, name));
}

node_ptr
SymbTable_get_define_flatten_body(const SymbTable_ptr self,
                                  const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_define_flatten_body(self->cache, name);
}

node_ptr
SymbTable_get_flatten_actual_parameter(const SymbTable_ptr self,
                                       const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_flatten_actual_parameter(self->cache, name);
}

node_ptr
SymbTable_get_define_context(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_define_context(self->cache, name);
}

node_ptr
SymbTable_get_actual_parameter_context(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_actual_parameter_context(self->cache, name);
}

node_ptr
SymbTable_get_array_define_context(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_get_array_define_context(self->cache, name);
}

SymbCategory SymbTable_get_symbol_category(const SymbTable_ptr self,
                                           node_ptr symbol)
{
  SymbCategory res;

  SYMB_TABLE_CHECK_INSTANCE(self);

  /* 1. Handle memoized results */
  res = NODE_TO_INT(find_assoc(self->categories, symbol));
  if (SYMBOL_INVALID != res) return res;

  /* 2. Trying to detect category */
  if (SymbTable_is_symbol_constant(self, symbol)) {
    res = SYMBOL_CONSTANT;
  }
  else if (SymbTable_is_symbol_frozen_var(self, symbol)) {
    res = SYMBOL_FROZEN_VAR;
  }
  else if (SymbTable_is_symbol_state_var(self, symbol)) {
    res = SYMBOL_STATE_VAR;
  }
  else if (SymbTable_is_symbol_input_var(self, symbol)) {
    res = SYMBOL_INPUT_VAR;
  }
  else if (SymbTable_is_symbol_define(self, symbol)) {
    node_ptr def_flatten_body =
      SymbTable_get_define_flatten_body(self, symbol);
    res = symb_table_detect_expr_category(self, def_flatten_body);
  }
  else if (SymbTable_is_symbol_function(self, symbol)) {
    res = SYMBOL_FUNCTION;
  }
  else if (SymbTable_is_symbol_parameter(self, symbol)) {
    res = SYMBOL_PARAMETER;
  }
  else if (SymbTable_is_symbol_variable_array(self, symbol)) {
    res = SYMBOL_VARIABLE_ARRAY;
  }
  else if (SymbTable_is_symbol_array_define(self, symbol)) {
    node_ptr def_flatten_body =
      SymbTable_get_array_define_flatten_body(self, symbol);
    res = symb_table_detect_expr_category(self, def_flatten_body);
  }
  else {
    /* The only remaining possibility is array.
       otherwise it is undefined symbol. */
    if (ARRAY == node_get_type(symbol)) {
      /* only constant indexes are allowed in identifiers */
      nusmv_assert(node_is_leaf(cdr(symbol)));
      res = SymbTable_get_symbol_category(self, car(symbol));
    }
    else {
      /* only identifiers are allowed */
      nusmv_assert(DOT == node_get_type(symbol) || ATOM == node_get_type(symbol));
      res = SYMBOL_INVALID;
    }
  }

  insert_assoc(self->categories, symbol, NODE_FROM_INT(res));

  return res;
}

boolean
SymbTable_is_symbol_state_var(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_is_symbol_state_var(self->cache, name);
}

boolean
SymbTable_is_symbol_frozen_var(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_is_symbol_frozen_var(self->cache, name);
}

boolean
SymbTable_is_symbol_state_frozen_var(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_is_symbol_state_frozen_var(self->cache, name);
}

boolean
SymbTable_is_symbol_input_var(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_is_symbol_input_var(self->cache, name);
}

boolean
SymbTable_is_symbol_var(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_is_symbol_var(self->cache, name);
}

boolean
SymbTable_is_symbol_bool_var(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);

  if (SymbCache_is_symbol_var(self->cache, name)) {
    SymbType_ptr type = SymbTable_get_var_type(self, name);
    return (SYMB_TYPE(NULL) != type) && SymbType_is_boolean(type);
  }

  return false;
}

boolean
SymbTable_is_symbol_declared(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_is_symbol_declared(self->cache, name);
}

boolean
SymbTable_is_symbol_define(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_is_symbol_define(self->cache, name);
}

boolean
SymbTable_is_symbol_function(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_is_symbol_function(self->cache, name);
}

boolean
SymbTable_is_symbol_parameter(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_is_symbol_parameter(self->cache, name);
}

boolean
SymbTable_is_symbol_array_define(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_is_symbol_array_define(self->cache, name);
}

boolean
SymbTable_is_symbol_variable_array(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_is_symbol_variable_array(self->cache, name);
}

boolean
SymbTable_is_symbol_constant(const SymbTable_ptr self, const node_ptr name)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_is_symbol_constant(self->cache, name);
}

boolean SymbTable_is_var_finite(const SymbTable_ptr self, const node_ptr name)
{
  SymbType_ptr vtype;

  SYMB_TABLE_CHECK_INSTANCE(self);

  vtype = SymbTable_get_var_type(self, name);
  return !(SymbType_is_infinite_precision(vtype) ||
           SymbType_is_intarray(vtype) ||
           SymbType_is_wordarray(vtype));
}

boolean
SymbTable_list_contains_input_var(const SymbTable_ptr self,
                                  const NodeList_ptr var_list)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_list_contains_input_var(self->cache, var_list);
}

boolean
SymbTable_list_contains_state_frozen_var(const SymbTable_ptr self,
                                         const NodeList_ptr var_list)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_list_contains_state_frozen_var(self->cache, var_list);
}

boolean
SymbTable_list_contains_undef_var(const SymbTable_ptr self,
                                  const NodeList_ptr var_list)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return SymbCache_list_contains_undef_var(self->cache, var_list);
}

SymbLayer_ptr SymbTable_variable_get_layer(SymbTable_ptr  self, node_ptr name)
{
  NodeList_ptr layers;
  ListIter_ptr iter;
  SymbLayer_ptr layer, result = SYMB_LAYER(NULL);

  SYMB_TABLE_CHECK_INSTANCE(self);

  if (!SymbTable_is_symbol_var(self, name)) {
    return SYMB_LAYER(NULL);
  }

  layers = self->layers;

  NODE_LIST_FOREACH(layers, iter) {
    layer = SYMB_LAYER(NodeList_get_elem_at(layers, iter));
    if (SymbLayer_is_symbol_in_layer(layer, name)) {
      result = layer;
      break;
    }
  }

  nusmv_assert(SYMB_LAYER(NULL) != result);

  return result;
}

SymbLayer_ptr SymbTable_symbol_get_layer(SymbTable_ptr  self, node_ptr name)
{
  NodeList_ptr layers;
  ListIter_ptr iter;
  SymbLayer_ptr layer, result = SYMB_LAYER(NULL);

  SYMB_TABLE_CHECK_INSTANCE(self);

  if (!SymbTable_is_symbol_declared(self, name)) {
    return SYMB_LAYER(NULL);
  }

  layers = self->layers;

  NODE_LIST_FOREACH(layers, iter) {
    layer = SYMB_LAYER(NodeList_get_elem_at(layers, iter));
    if (SymbLayer_is_symbol_in_layer(layer, name)) {
      result = layer;
      break;
    }
  }

  nusmv_assert(SYMB_LAYER(NULL) != result);

  return result;
}

SymbLayer_ptr SymbTable_define_get_layer(SymbTable_ptr  self, node_ptr name)
{
  NodeList_ptr layers;
  ListIter_ptr iter;
  SymbLayer_ptr layer, result = SYMB_LAYER(NULL);

  SYMB_TABLE_CHECK_INSTANCE(self);

  if (!SymbTable_is_symbol_define(self, name)) {
    return SYMB_LAYER(NULL);
  }

  layers = self->layers;

  NODE_LIST_FOREACH(layers, iter) {
    layer = SYMB_LAYER(NodeList_get_elem_at(layers, iter));
    if (SymbLayer_is_symbol_in_layer(layer, name)) {
      result = layer;
      break;
    }
  }

  nusmv_assert(SYMB_LAYER(NULL) != result);

  return result;
}

SymbLayer_ptr SymbTable_function_get_layer(SymbTable_ptr self, node_ptr name)
{
  NodeList_ptr layers;
  SymbLayer_ptr layer, result = SYMB_LAYER(NULL);
  ListIter_ptr iter;

  SYMB_TABLE_CHECK_INSTANCE(self);

  if (!SymbTable_is_symbol_function(self, name)) {
    return SYMB_LAYER(NULL);
  }

  layers = self->layers;

  NODE_LIST_FOREACH(layers, iter) {
    layer = SYMB_LAYER(NodeList_get_elem_at(layers, iter));
    if (SymbLayer_is_symbol_in_layer(layer, name)) {
      result = layer;
      break;
    }
  }

  nusmv_assert(SYMB_LAYER(NULL) != result);

  return result;
}

node_ptr SymbTable_get_determinization_var_name(const SymbTable_ptr self)
{
  NuSMVEnv_ptr env;
  const char* INT_VAR_PREFIX = "__det_";
  char* name = (char*) NULL;
  node_ptr vname = Nil;
  int buf_size = strlen(INT_VAR_PREFIX) + 10;

  SYMB_TABLE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));

  /* 10 digits should be enough */
  nusmv_assert(self->det_counter < (size_t) (unsigned int) ~0);
  name = (char*) ALLOC(char, buf_size);

  /* searches for a not already declared symbol for the new determ var: */
  while (true) {
    int c = snprintf(name, buf_size, "%s%" PRIuPTR,
                     INT_VAR_PREFIX, self->det_counter++);
    SNPRINTF_CHECK(c, buf_size);
    vname = NodeMgr_find_node(self->nodes, DOT, Nil, sym_intern(env, name));
    if (!SymbTable_is_symbol_declared(self, vname)) break;
  }

  FREE(name);
  return vname;
}


node_ptr SymbTable_get_symbol_from_str(const SymbTable_ptr self,
                                       const char* symbol_str)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  return NodeMgr_find_node(
      self->nodes, DOT, Nil,
      NodeMgr_find_node(
          self->nodes,
          ATOM,
          (node_ptr) UStringMgr_find_string(self->strings, symbol_str),
          Nil));
}


node_ptr SymbTable_get_fresh_symbol_name(const SymbTable_ptr self,
                                         const char* prefix)
{
  static int i = 0;
  int size;
  char* varname;
  node_ptr res;

  SYMB_TABLE_CHECK_INSTANCE(self);

  if ((char*) NULL == prefix) {
    prefix = "__fresh_var_";
  }

  /* 15 digits are wide enough for any reasonable number */
  size = strlen(prefix) + 16;
  varname = ALLOC(char, size);

  do {
    snprintf(varname, size, "%s%d", prefix, i);
    ++i;
    res = SymbTable_get_symbol_from_str(self, varname);
  } while(SymbTable_is_symbol_declared(self, res));

  FREE(varname);

  return res;
}


SymbTable_ptr SymbTable_copy(const SymbTable_ptr self, Set_t blacklist)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  SymbTable_ptr new_st;
  SymbLayer_ptr new_layer, old_layer;
  ListIter_ptr layer_iter;
  NodeList_ptr layers;

  new_st = SymbTable_create(env);

  SymbTable_set_default_layers_class_name(new_st,
                                          SymbTable_get_default_layers_class_name(self));

  layers = SymbTable_get_layers(self);
  NODE_LIST_FOREACH(layers, layer_iter) {
    const char* class_name;
    SymbLayerIter iter;

    old_layer = (SymbLayer_ptr)NodeList_get_elem_at(layers, layer_iter);

    new_layer = SymbTable_create_layer(new_st,
                                       SymbLayer_get_name(old_layer),
                                       SymbLayer_get_insert_policy(old_layer));

    class_name = SymbTable_get_class_of_layer(self,
                                              SymbLayer_get_name(old_layer));
    if ((char*)NULL != class_name) {
      SymbTable_layer_add_to_class(new_st, SymbLayer_get_name(old_layer),
                                   class_name);
    }

    /* SYMB_LAYER_FOREACH(old_layer, iter, STT_CONSTANT) { */
    /*   node_ptr symbol = SymbLayer_iter_get_symbol(old_layer, &iter); */
    /*   SymbLayer_declare_constant(new_layer, symbol); */
    /* } */

    SYMB_LAYER_FOREACH(old_layer, iter, STT_ALL) {
      node_ptr symbol = SymbLayer_iter_get_symbol(old_layer, &iter);

      /* Skip blacklisted symbols*/
      if (!Set_IsMember(blacklist, symbol)) {
        /* We have different approaches for variables and symbols */
        if (SymbTable_is_symbol_var(self, symbol)) {
          SymbType_ptr type = SymbTable_get_var_type(self, symbol);

          if (SymbType_is_enum(type)) {
            node_ptr value_list = SymbType_get_enum_type_values(type);

            while (value_list != Nil) {
              node_ptr name = car(value_list);

              if (DOT == node_get_type(name)) {
                ResolveSymbol_ptr rs;
                rs = SymbTable_resolve_symbol(self, name, Nil);
                name = ResolveSymbol_get_resolved_name(rs);
              }
              else name = find_atom(nodemgr, name);

              if (SymbLayer_can_declare_constant(new_layer, name)) {
                SymbLayer_declare_constant(new_layer, name);
              }
              value_list = cdr(value_list);
            }
          } /* SymbType_is_enum */

          /* Create a copy of the type */
          type = SymbType_copy(type);

          /* Add the variable to the new symbol layer */
          if (SymbTable_is_symbol_state_var(self, symbol)) {
            SymbLayer_declare_state_var(new_layer, symbol, type);
          }
          else if (SymbTable_is_symbol_frozen_var(self, symbol)) {
            SymbLayer_declare_frozen_var(new_layer, symbol, type);
          }
          else if (SymbTable_is_symbol_input_var(self, symbol)) {
            SymbLayer_declare_input_var(new_layer, symbol, type);
          }
          else {
            ErrorMgr_rpterr(errmgr, "SymbTable_copy: Symbol %s not handled.\n",
                            sprint_node(wffprint, symbol));
            error_unreachable_code();
          }
        }
        /* Not variables */
        else {
          if (SymbTable_is_symbol_constant(self, symbol)) {
            /* Constants should have been already declared. */
            if (SymbLayer_can_declare_constant(new_layer, symbol))
              SymbLayer_declare_constant(new_layer, symbol);
          }
          else if (SymbTable_is_symbol_variable_array(self, symbol)) {
            SymbType_ptr type = SymbTable_get_variable_array_type(self, symbol);
            type = SymbType_copy(type);
            SymbLayer_declare_variable_array(new_layer, symbol, type);
          }
          else if (SymbTable_is_symbol_array_define(self, symbol)) {
            node_ptr body =
              SymbTable_get_array_define_flatten_body(self, symbol);

            /* If we remove this function call, when someone asks the
               body of the define in the new symbol table, the old body
               is returned (eg: WriteFlattenModel) */
            Flatten_remove_symbol_info(env, symbol);

            SymbLayer_declare_array_define(new_layer, symbol, Nil, body);
          }
          else if (SymbTable_is_symbol_define(self, symbol)) {
            node_ptr body = SymbTable_get_define_body(self, symbol);
            node_ptr ctx = SymbTable_get_define_context(self, symbol);

            /* If we remove this function call, when someone asks the
               body of the define in the new symbol table, the old body
               is returned (eg: WriteFlattenModel) */
            Flatten_remove_symbol_info(env, symbol);

            SymbLayer_declare_define(new_layer, symbol, ctx, body);
          }
          else if (SymbTable_is_symbol_parameter(self, symbol)) {
            node_ptr actual = SymbTable_get_actual_parameter(self, symbol);
            node_ptr ctx = SymbTable_get_actual_parameter_context(self, symbol);

            SymbLayer_declare_parameter(new_layer, symbol, ctx, actual);
          }
          else if (SymbTable_is_symbol_function(self, symbol)) {
            node_ptr ctx = SymbTable_get_function_context(self, symbol);
            SymbType_ptr type = SymbTable_get_function_type(self, symbol);
            type = SymbType_copy(type);

            SymbLayer_declare_function(new_layer, symbol, ctx, type);
          }

          else {
            char* n = sprint_node(wffprint, symbol);
            ErrorMgr_rpterr(errmgr, "SymbTable_copy: Symbol %s not handled.\n", n);
            FREE(n);
            error_unreachable_code();
          }
        }
      }
    }
  }
  return new_st;
}

/*!
  \brief Resolves the given symbol in the given context

  Resolves the given symbol in the given context.
   This function returns the internal instance of
   ResolveSymbol, which must NOT be freed by the
   caller. The ResolveSymbol internal instance is
   re-populated everytime that this function is
   called.
*/

ResolveSymbol_ptr SymbTable_resolve_symbol(SymbTable_ptr self,
                                           node_ptr expr, node_ptr context)
{
  SYMB_TABLE_CHECK_INSTANCE(self);
  ResolveSymbol_resolve(self->resolver, self, expr, context);
  return self->resolver;
}

void SymbTable_gen_iter(const SymbTable_ptr self,
                        SymbTableIter* iter,
                        unsigned int mask)
{
  SymbCache_gen_iter(self->cache, iter, mask);
}

void SymbTable_iter_next(const SymbTable_ptr self, SymbTableIter* iter)
{
  SymbCache_next_iter(self->cache, iter);
}

boolean SymbTable_iter_is_end(const SymbTable_ptr self,
                              const SymbTableIter* iter)
{
  return SymbCache_is_iter_end(self->cache, iter);
}

node_ptr SymbTable_iter_get_symbol(const SymbTable_ptr self,
                                   const SymbTableIter* iter)
{
  return SymbCache_iter_get_symbol(self->cache, iter);
}

void SymbTable_iter_set_filter(const SymbTable_ptr self,
                               SymbTableIter* iter,
                               SymbTableIterFilterFun fun,
                               void* arg)
{
  iter->st = self;
  SymbCache_iter_set_filter(self->cache, iter, fun, arg);
}

boolean SymbTable_iter_filter_sf_i_symbols(const SymbTable_ptr self,
                                           const node_ptr sym,
                                           void* arg)
{
  if (SymbTable_is_symbol_define(self, sym)) {
    SymbCategory cat = SymbTable_get_symbol_category(self, sym);
    return cat == SYMBOL_STATE_INPUT_NEXT_DEFINE ||
      cat == SYMBOL_STATE_INPUT_DEFINE;
  }

  return SymbTable_is_symbol_var(self, sym);
}

boolean SymbTable_iter_filter_sf_symbols(const SymbTable_ptr self,
                                         const node_ptr sym,
                                         void* arg)
{
  if (SymbTable_is_symbol_define(self, sym)) {
    SymbCategory cat = SymbTable_get_symbol_category(self, sym);
    return cat == SYMBOL_STATE_NEXT_DEFINE ||
      cat == SYMBOL_STATE_DEFINE ||
      cat == SYMBOL_CONSTANT;
  }

  return SymbTable_is_symbol_state_var(self, sym) ||
    SymbTable_is_symbol_frozen_var(self, sym);
}

boolean SymbTable_iter_filter_i_symbols(const SymbTable_ptr self,
                                        const node_ptr sym,
                                        void* arg)
{
  if (SymbTable_is_symbol_define(self, sym)) {
    SymbCategory cat = SymbTable_get_symbol_category(self, sym);
    return cat == SYMBOL_INPUT_NEXT_DEFINE ||
      cat == SYMBOL_INPUT_DEFINE;
  }

  return SymbTable_is_symbol_input_var(self, sym);
}

boolean SymbTable_iter_filter_out_var_array_elems(const SymbTable_ptr self,
                                                  const node_ptr sym,
                                                  void* arg)
{
  if (SymbTable_is_symbol_array_var_element(self, sym)) return false;
  else return true;
}


void SymbTable_foreach(const SymbTable_ptr self, unsigned int mask,
                       SymbTableForeachFun fun, void* arg)
{
  SymbTableIter iter;

  SYMB_TABLE_FOREACH(self, iter, mask) {
    fun(self, SymbCache_iter_get_symbol(self->cache, &iter), arg);
  }
}

Set_t SymbTable_iter_to_set(const SymbTable_ptr self,
                            SymbTableIter iter)
{
  Set_t res = Set_MakeEmpty();

  while (!SymbTable_iter_is_end(self, &iter)) {
    res = Set_AddMember(res, SymbTable_iter_get_symbol(self, &iter));
    SymbTable_iter_next(self, &iter);
  }

  return res;
}

NodeList_ptr SymbTable_iter_to_list(const SymbTable_ptr self,
                                    SymbTableIter iter)
{
  NodeList_ptr res = NodeList_create();

  while (!SymbTable_iter_is_end(self, &iter)) {
    NodeList_append(res, SymbTable_iter_get_symbol(self, &iter));
    SymbTable_iter_next(self, &iter);
  }

  return res;
}

unsigned int SymbTable_iter_count(const SymbTable_ptr self,
                                  SymbTableIter iter)
{
  unsigned int res = 0;

  while (!SymbTable_iter_is_end(self, &iter)) {
    ++res;
    SymbTable_iter_next(self, &iter);
  }

  return res;
}

void SymbTable_add_trigger(const SymbTable_ptr self,
                           SymbTableTriggerFun trigger,
                           SymbTableTriggerAction action,
                           void* arg, boolean must_free_arg)
{
  SymbCache_add_trigger(self->cache, trigger, action, arg, must_free_arg);
}

void SymbTable_remove_trigger(const SymbTable_ptr self,
                              SymbTableTriggerFun trigger,
                              SymbTableTriggerAction action)
{
  SymbCache_remove_trigger(self->cache, trigger, action);
}

hash_ptr SymbTable_get_handled_hash_ptr(SymbTable_ptr self,
                                        const char* key,
                                        ST_PFICPCP compare_func,
                                        ST_PFICPI hash_func,
                                        ST_PFSR destroy_func,
                                        SymbTableTriggerFun add_action,
                                        SymbTableTriggerFun remove_action,
                                        SymbTableTriggerFun redeclare_action)
{
  string_ptr strkey;
  Pair_ptr p;
  hash_ptr res;

  SYMB_TABLE_CHECK_INSTANCE(self);

  strkey = UStringMgr_find_string(self->strings, (char*)key);
  p = PAIR(find_assoc(self->handled_structures, NODE_PTR(strkey)));

  /* First time we need the hash, create it */
  if (PAIR(NULL) == p) {
    if ((ST_PFICPCP) NULL == compare_func &&
        (ST_PFICPI) NULL == hash_func) {
      p = Pair_create(new_assoc(),
                      (void*)ST_HANDLED_STRUCTS_HASH);
    }
    else {
      nusmv_assert((ST_PFICPCP) NULL != compare_func &&
                   (ST_PFICPI) NULL != hash_func);
      p = Pair_create(new_assoc_with_params(compare_func,
                                            hash_func),
                      (void*)ST_HANDLED_STRUCTS_HASH);
    }

    insert_assoc(self->handled_structures, NODE_PTR(strkey), NODE_PTR(p));

    res = (hash_ptr)Pair_get_first(p);

    /* Add the triggers: if already registered, will not be added as
       duplicates. We are garanteed that the hash passed as arg is
       always the same */
    if ((SymbTableTriggerFun) NULL != add_action) {
      SymbTable_add_trigger(self, add_action, ST_TRIGGER_SYMBOL_ADD,
                            (void*) res, false);
    }
    if ((SymbTableTriggerFun) NULL != remove_action) {
      AssocAndDestroy_ptr remove_action_arg = ALLOC(AssocAndDestroy, 1);
      nusmv_assert(NULL != remove_action_arg);
      remove_action_arg->assoc = res;
      remove_action_arg->destroy_func = (PF_STCPCPCP) destroy_func;

      SymbTable_add_trigger(self, remove_action, ST_TRIGGER_SYMBOL_REMOVE,
                            (void*) remove_action_arg, true);
    }
    if ((SymbTableTriggerFun) NULL != redeclare_action) {
      SymbTable_add_trigger(self, redeclare_action,
                            ST_TRIGGER_SYMBOL_REDECLARE,
                            (void*) res, false);
    }
  }
  /* The hash was already been created */
  else {
    res = (hash_ptr)Pair_get_first(p);
  }

  nusmv_assert(ST_HANDLED_STRUCTS_HASH ==
               (SymbTableHandledType)Pair_get_second(p));

  return res;
}

void SymbTable_clear_handled_remove_action_hash(const SymbTable_ptr st,
                                                const node_ptr sym,
                                                SymbTableTriggerAction action,
                                                void* arg)
{
  AssocAndDestroy_ptr assoc_and_destroy = (AssocAndDestroy_ptr) arg;
  hash_ptr hash = assoc_and_destroy->assoc;

  nusmv_assert((hash_ptr)NULL != hash);

  if (assoc_get_size(hash) > 0) {
    if (assoc_and_destroy->destroy_func) {
      clear_assoc_and_free_entries(hash, assoc_and_destroy->destroy_func);
    }
    else {
      clear_assoc(hash);
    }
  }
}

boolean
SymbTable_contains_infinite_precision_variables(const SymbTable_ptr self)
{
  SymbTableIter iter;

  if (SymbCache_get_functions_num(self->cache) > 0) return true;

  SYMB_TABLE_FOREACH(self, iter, STT_VAR) {
    node_ptr sym = SymbCache_iter_get_symbol(self->cache, &iter);
    SymbType_ptr type = SymbTable_get_var_type(self, sym);

    if (SymbType_is_infinite_precision(type)) {
      return true;
    }
  }

  return false;
}

boolean
SymbTable_contains_enum_variables(const SymbTable_ptr self)
{
  SymbTableIter iter;

  SYMB_TABLE_FOREACH(self, iter, STT_VAR) {
    node_ptr sym = SymbCache_iter_get_symbol(self->cache, &iter);
    SymbType_ptr type = SymbTable_get_var_type(self, sym);

    if (SymbType_is_enum(type) && !SymbType_is_boolean(type)) {
      return true;
    }
  }

  return false;
}

boolean
SymbTable_contains_word_variables(const SymbTable_ptr self)
{
  SymbTableIter iter;

  SYMB_TABLE_FOREACH(self, iter, STT_VAR) {
    node_ptr sym = SymbCache_iter_get_symbol(self->cache, &iter);
    SymbType_ptr type = SymbTable_get_var_type(self, sym);

    if (SymbType_is_word(type)) {
      return true;
    }
  }

  return false;
}

boolean
SymbTable_contains_array_variables(const SymbTable_ptr self)
{
  return SymbCache_get_variable_arrays_num(self->cache) > 0;
}

boolean
SymbTable_contains_functions(const SymbTable_ptr self)
{
  return SymbCache_get_functions_num(self->cache) > 0;
}

Set_t SymbTable_get_type_tags(const SymbTable_ptr self)
{
  Set_t res = Set_MakeEmpty();
  SymbTableIter iter;

  /* TODO[MD] here we should iterate also over variable array */
  SYMB_TABLE_FOREACH (self, iter, STT_VAR | STT_FUNCTION) {
    node_ptr sym = SymbCache_iter_get_symbol(self->cache, &iter);
    SymbType_ptr type = SymbTable_get_symbol_type(self, sym);

    res = Set_AddMember(res,
                        PTR_FROM_INT(Set_Element_t, SymbType_get_tag(type)));
  }

  return res;
}

char* SymbTable_sprint_category(SymbTable_ptr self,
                                node_ptr symbol)
{
  char* retval = NULL;
  SymbCategory category;

  category = SymbTable_get_symbol_category(self, symbol);

  switch(category) {
  case SYMBOL_INVALID:
    retval = "invalid";
    break;

  case SYMBOL_CONSTANT:
    retval = "constant";
    break;

  case SYMBOL_FROZEN_VAR:
    retval = "frozen variable";
    break;

  case SYMBOL_STATE_VAR:
    retval = "state variable";
    break;

  case SYMBOL_INPUT_VAR:
    retval = "input variable";
    break;

  case SYMBOL_STATE_DEFINE:
    retval = "state define";
    break;

  case SYMBOL_INPUT_DEFINE:
    retval = "input define";
    break;

  case SYMBOL_STATE_INPUT_DEFINE:
    retval = "state input define";
    break;

  case SYMBOL_NEXT_DEFINE:
    retval = "next define";
    break;

  case SYMBOL_STATE_NEXT_DEFINE:
    retval = "state next define";
    break;

  case SYMBOL_INPUT_NEXT_DEFINE:
    retval = "input next define";
    break;

  case SYMBOL_STATE_INPUT_NEXT_DEFINE:
    retval = "state input next define";
    break;

  case SYMBOL_DEFINE:
    retval = "define";
    break;

  case SYMBOL_FUNCTION:
    retval = "function";
    break;

  case SYMBOL_PARAMETER:
    retval = "parameter";
    break;

  case SYMBOL_ARRAY_DEFINE:
    retval = "array define";
    break;

  case SYMBOL_VARIABLE_ARRAY:
    retval = "variable array";
    break;

  default:
    error_unreachable_code();
  }

  return retval;
}

SymbTable_ptr SymbTable_anonymize(const SymbTable_ptr self,
                                  Set_t blacklist,
                                  NodeAnonymizerBase_ptr anonymizer)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  SymbTable_ptr new_st;
  SymbLayer_ptr new_layer, old_layer;
  ListIter_ptr layer_iter;
  NodeList_ptr layers;

  new_st = SymbTable_create(env);

  SymbTable_set_default_layers_class_name(new_st,
                                          SymbTable_get_default_layers_class_name(self));

  layers = SymbTable_get_layers(self);
  NODE_LIST_FOREACH(layers, layer_iter) {
    const char* class_name;
    SymbLayerIter iter;

    old_layer = (SymbLayer_ptr)NodeList_get_elem_at(layers, layer_iter);

    new_layer = SymbTable_create_layer(new_st,
                                       SymbLayer_get_name(old_layer),
                                       SymbLayer_get_insert_policy(old_layer));

    class_name = SymbTable_get_class_of_layer(self,
                                              SymbLayer_get_name(old_layer));
    if ((char*)NULL != class_name) {
      SymbTable_layer_add_to_class(new_st, SymbLayer_get_name(old_layer),
                                   class_name);
    }

    SYMB_LAYER_FOREACH(old_layer, iter, STT_ALL) {
      node_ptr symbol = SymbLayer_iter_get_symbol(old_layer, &iter);
      node_ptr anonymous_symbol = NULL;

      anonymous_symbol = NodeAnonymizerBase_map_expr(anonymizer, symbol);

      /* Skip blacklisted symbols*/
      if (!Set_IsMember(blacklist, symbol)) {
        /* We have different approaches for variables and symbols */
        if (SymbTable_is_symbol_var(self, symbol)) {
          SymbType_ptr type = SymbTable_get_var_type(self, symbol);

          if (SymbType_is_enum(type)) {
            node_ptr value_list = SymbType_get_enum_type_values(type);
            node_ptr anonymous_value_list;

            anonymous_value_list = NodeAnonymizerBase_map_expr(anonymizer,
                                                               value_list);

            while (value_list != Nil) {
              node_ptr name = car(value_list);
              node_ptr anonymous_name;

              anonymous_name = NodeAnonymizerBase_map_expr(anonymizer, name);

              if (DOT == node_get_type(name)) {
                ResolveSymbol_ptr rs;
                rs = SymbTable_resolve_symbol(self, name, Nil);
                name = ResolveSymbol_get_resolved_name(rs);
              }
              else name = find_atom(nodemgr, name);

              if (SymbLayer_can_declare_constant(new_layer, anonymous_name)) {
                SymbLayer_declare_constant(new_layer, anonymous_name);
              }
              value_list = cdr(value_list);
            }

            type = SymbType_create(env, SYMB_TYPE_ENUM, anonymous_value_list);
          } /* SymbType_is_enum */
          else type = SymbType_copy(type);

          /* Add the variable to the new symbol layer */
          if (SymbTable_is_symbol_state_var(self, symbol)) {
            SymbLayer_declare_state_var(new_layer, anonymous_symbol, type);
          }
          else if (SymbTable_is_symbol_frozen_var(self, symbol)) {
            SymbLayer_declare_frozen_var(new_layer, anonymous_symbol, type);
          }
          else if (SymbTable_is_symbol_input_var(self, symbol)) {
            SymbLayer_declare_input_var(new_layer, anonymous_symbol, type);
          }
          else {
            ErrorMgr_rpterr(errmgr, "SymbTable_copy: Symbol %s not handled.\n",
                            sprint_node(wffprint, symbol));
            error_unreachable_code();
          }
        }
        /* Not variables */
        else {
          if (SymbTable_is_symbol_constant(self, symbol)) {
            /* Constants should have been already declared. */
            if (SymbLayer_can_declare_constant(new_layer, anonymous_symbol))
              SymbLayer_declare_constant(new_layer, anonymous_symbol);
          }
          else if (SymbTable_is_symbol_variable_array(self, symbol)) {
            SymbType_ptr type = SymbTable_get_variable_array_type(self, symbol);
            type = SymbType_copy(type);
            SymbLayer_declare_variable_array(new_layer, anonymous_symbol, type);
          }
          else if (SymbTable_is_symbol_array_define(self, symbol)) {
            node_ptr body =
              SymbTable_get_array_define_flatten_body(self, symbol);
            /* If we remove this function call, when someone asks the
               body of the define in the new symbol table, the old body
               is returned (eg: WriteFlattenModel) */
            Flatten_remove_symbol_info(env, symbol);

            SymbLayer_declare_array_define(new_layer, anonymous_symbol, Nil, body);
          }
          else if (SymbTable_is_symbol_define(self, symbol)) {
            node_ptr body = SymbTable_get_define_body(self, symbol);
            node_ptr ctx = SymbTable_get_define_context(self, symbol);
            node_ptr anonymous_body;
            node_ptr anonymous_ctx;

            /* If we remove this function call, when someone asks the
               body of the define in the new symbol table, the old body
               is returned (eg: WriteFlattenModel) */
            Flatten_remove_symbol_info(env, symbol);

            anonymous_body = NodeAnonymizerBase_map_expr(anonymizer, body);
            anonymous_ctx = NodeAnonymizerBase_map_expr(anonymizer, ctx);
            SymbLayer_declare_define(new_layer, anonymous_symbol, anonymous_ctx,
                                     anonymous_body);
          }
          else if (SymbTable_is_symbol_parameter(self, symbol)) {
            node_ptr actual = SymbTable_get_actual_parameter(self, symbol);
            node_ptr ctx = SymbTable_get_actual_parameter_context(self, symbol);

            SymbLayer_declare_parameter(new_layer, anonymous_symbol, ctx, actual);
          }
          else if (SymbTable_is_symbol_function(self, symbol)) {
            node_ptr ctx = SymbTable_get_function_context(self, symbol);
            SymbType_ptr type = SymbTable_get_function_type(self, symbol);
            type = SymbType_copy(type);

            SymbLayer_declare_function(new_layer, anonymous_symbol, ctx, type);
          }

          else {
            char* n = sprint_node(wffprint, symbol);
            ErrorMgr_rpterr(errmgr, "SymbTable_anonymize: Symbol %s not handled.\n", n);
            FREE(n);
            error_unreachable_code();
          }
        }
      }
    }
  }

  return new_st;
}

boolean
SymbTable_is_symbol_array_var_element(const SymbTable_ptr self,
                                      const node_ptr name)
{
  /* See description of flattener_core_flatten for docs about arrays.
     ARRAY may be a part of identifier as well as index-subscript of
     an array.
  */

  /* here we check if name[N] is a part of array name.
     Note that for
     VAR b: array 0..1 of boolean;
         b[4] : word[3];
     b[4] is not part of b, but b[1] is.
  */
  if (node_get_type(name) == ARRAY &&
      node_get_type(cdr(name)) == NUMBER) {
    if (SymbTable_is_symbol_variable_array(self, car(name))) {
      SymbType_ptr type = SymbTable_get_variable_array_type(self, car(name));
      int val = node_get_int(cdr(name));
      if (SymbType_get_array_lower_bound(type) <= val &&
          SymbType_get_array_upper_bound(type) >= val) {
        return true;
      }
    }
    else return SymbTable_is_symbol_array_var_element(self, car(name));
  }

  return false;
}

node_ptr SymbTable_get_var_array_from_element(const SymbTable_ptr self,
                                              node_ptr element)
{
  node_ptr retval = NULL;

  nusmv_assert(NULL != element);
  nusmv_assert(SymbTable_is_symbol_array_var_element(self, element));

  if (SymbTable_is_symbol_array_var_element(self, car(element))) {
    retval = SymbTable_get_var_array_from_element(self, car(element));
  }
  else {
    nusmv_assert((SymbTable_is_symbol_variable_array(self, car(element))));
    retval = car(element);
  }

  nusmv_assert(NULL != retval);
  nusmv_assert(SymbTable_is_symbol_variable_array(self, car(element)));

  return retval;
}

boolean SymbTable_is_symbol_frozen_var_array(const SymbTable_ptr self,
                                             node_ptr array)
{
  nusmv_assert(NULL != array);

  if (SymbTable_is_symbol_variable_array(self, array)) {
    node_ptr lbound = SymbTable_get_array_lower_bound_variable(self, array);

    if (SymbTable_is_symbol_frozen_var(self, lbound)) return true;
    else return false;
  }
  else return false;
}

boolean SymbTable_is_symbol_state_var_array(const SymbTable_ptr self,
                                            node_ptr array)
{
  nusmv_assert(NULL != array);

  if (SymbTable_is_symbol_variable_array(self, array)) {
    node_ptr lbound = SymbTable_get_array_lower_bound_variable(self, array);

    if (SymbTable_is_symbol_state_var(self, lbound)) return true;
    else return false;
  }
  else return false;
}

boolean SymbTable_is_symbol_input_var_array(const SymbTable_ptr self,
                                      node_ptr array)
{
  nusmv_assert(NULL != array);

  if (SymbTable_is_symbol_variable_array(self, array)) {
    node_ptr lbound = SymbTable_get_array_lower_bound_variable(self, array);

    if (SymbTable_is_symbol_input_var(self, lbound)) return true;
    else return false;
  }
  else return false;
}


node_ptr SymbTable_get_array_lower_bound_variable(const SymbTable_ptr self,
                                                  node_ptr array)
{
  node_ptr retval = NULL;

  nusmv_assert(NULL != array);
  nusmv_assert(SymbTable_is_symbol_variable_array(self, array));

  {
    NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
    NodeMgr_ptr const nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
    ExprMgr_ptr const exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
    SymbType_ptr type = SymbTable_get_variable_array_type(self, array);
    SymbType_ptr subtype = SymbType_get_array_subtype(type);
    int lbound = SymbType_get_array_lower_bound(type);

    retval = find_node(nodemgr, ARRAY, array,
                       ExprMgr_number(exprs, lbound));
    if (SymbTable_is_symbol_variable_array(self, retval)) {
      retval = SymbTable_get_array_lower_bound_variable(self, retval);
    }
  }

  nusmv_assert(NULL != retval);
  nusmv_assert(SymbTable_is_symbol_var(self, retval));

  return retval;
}

node_ptr SymbTable_get_array_upper_bound_variable(const SymbTable_ptr self,
                                                  node_ptr array)
{
  node_ptr retval = NULL;

  nusmv_assert(NULL != array);
  nusmv_assert(SymbTable_is_symbol_variable_array(self, array));

  {
    NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
    NodeMgr_ptr const nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
    ExprMgr_ptr const exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
    SymbType_ptr type = SymbTable_get_variable_array_type(self, array);
    SymbType_ptr subtype = SymbType_get_array_subtype(type);
    int ubound = SymbType_get_array_upper_bound(type);

    retval = find_node(nodemgr, ARRAY, array, ExprMgr_number(exprs, ubound));
    if (SymbTable_is_symbol_variable_array(self, retval)) {
      retval = SymbTable_get_array_upper_bound_variable(self, retval);
    }
  }

  nusmv_assert(NULL != retval);
  nusmv_assert(SymbTable_is_symbol_var(self, retval));

  return retval;
}

/*--------------------------------------------------------------------------*/
/* Definition of static functions                                           */
/*--------------------------------------------------------------------------*/

/*!
  \brief Private initializer

  Private initializer, called by the constructor only

  \sa symb_table_deinit
*/
static void symb_table_init(SymbTable_ptr self, NuSMVEnv_ptr env)
{
  env_object_init(ENV_OBJECT(self), env);

  self->options = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  self->streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  self->nodes = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  self->printer = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  self->strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));

  self->cache = SymbCache_create(self, env);
  self->categories = new_assoc();
  self->temp_layer_name_suffix = 0;
  self->layers = NodeList_create();
  self->class_layers = new_assoc();
  nusmv_assert(self->class_layers != (hash_ptr) NULL);
  self->class_names = NodeList_create();
  self->default_class_name = (const char*) NULL;

  self->name2layer = new_assoc();
  nusmv_assert(self->name2layer != (hash_ptr) NULL);

  self->det_counter = 0;
  self->tc = TypeChecker_create_with_default_checkers(self);

  self->resolver = ResolveSymbol_create();

  self->handled_structures = new_assoc();

  OVERRIDE(Object, finalize) = symb_table_finalize;
}

/*!
  \brief Private destructor used by clas destroyer



  \sa symb_table_deinit
*/

static assoc_retval class_layers_hash_free(char *key, char *data, char * arg)
{
  array_t* _class = (array_t*) data;

  if (_class != NULL) {
    const char* name; int i;
    arrayForEachItem(const char*, _class, i, name) { FREE(name); }
    array_free(_class);
  }
  return ASSOC_DELETE;
}

/*!
  \brief Virtual deinitializer

  Virtual deinitializer
*/
static void symb_table_finalize(Object_ptr object, void* dummy)
{
  SymbTable_ptr self = SYMB_TABLE(object);
  symb_table_deinit(self);
  FREE(self);
}

/*!
  \brief Private deinitializer

  Private deinitializer, called by the destructor only

  \sa symb_table_init
*/
static void symb_table_deinit(SymbTable_ptr self)
{
  ListIter_ptr iter;
  assoc_iter aiter;
  Pair_ptr v;
  string_ptr k;

  TypeChecker_destroy(self->tc);
  self->tc = TYPE_CHECKER(NULL);

  /* destroys the layer classes: */
  NodeList_destroy(self->class_names);
  clear_assoc_and_free_entries(self->class_layers, class_layers_hash_free);
  free_assoc(self->class_layers);

  if (self->default_class_name != (const char*) NULL) {
    FREE(self->default_class_name);
  }

  /* get rid of all contained layers */
  iter = NodeList_get_first_iter(self->layers);
  while (! ListIter_is_end(iter)) {
    /* The cache will be destroyed, no need for cleaning it */
    SymbLayer_destroy_raw(SYMB_LAYER(NodeList_get_elem_at(self->layers, iter)));
    iter = ListIter_get_next(iter);
  }
  NodeList_destroy(self->layers);

  SymbCache_destroy(self->cache);
  free_assoc(self->categories);

  free_assoc(self->name2layer);

  ResolveSymbol_destroy(self->resolver);

  ASSOC_FOREACH(self->handled_structures, aiter, &k, &v) {
    SymbTableHandledType t = (SymbTableHandledType)Pair_get_second(v);
    void* val = Pair_get_first(v);

    switch (t) {
    case ST_HANDLED_STRUCTS_HASH:
      free_assoc((hash_ptr)val);
      break;

    default:
      error_unreachable_code_msg("Unhandled structure type");
    }

#ifdef MEMOIZED_HASH_DEBUG
    FREE(v);
#else
    Pair_destroy(v);
#endif
  }

  free_assoc(self->handled_structures);

  env_object_deinit(ENV_OBJECT(self));
}

/*!
  \brief Returns true if the given name corresponds to an already
   registered layer.
*/
static boolean
symb_table_layer_exists(const SymbTable_ptr self, const char* layer_name)
{
  return (SymbTable_get_layer(self, layer_name) != SYMB_LAYER(NULL));
}

/*!
  \brief Given a list of symbols and a list of layers names,
   returns a new list that contains only those symbols that have been
   declared within the given layers

  The caller is responsible for destroying the
   returned list
*/
static NodeList_ptr
symb_table_filter_layers_symbols(SymbTable_ptr self,
                                 const array_t* layer_names,
                                 SymbTableIter* iter)
{
  NodeList_ptr res;
  const char* layer_name;
  int idx;

  res = NodeList_create();

  while (!SymbTable_iter_is_end(self, iter)) {
    node_ptr sym = SymbTable_iter_get_symbol(self, iter);

    arrayForEachItem(const char*, (array_t*) layer_names, idx, layer_name) {
      SymbLayer_ptr layer = SymbTable_get_layer(self, layer_name);

      if (layer != SYMB_LAYER(NULL)) {
        if (SymbLayer_is_symbol_in_layer(layer, sym) &&
            !NodeList_belongs_to(res, sym)) {
          NodeList_append(res, sym);
        }
      }
    }

    SymbTable_iter_next(self, iter);
  }

  return res;
}

/*!
  \brief Removes or rename a given layer inside the given
   layers class. If class_name is NULL, then default class is taken
   (must be set before)

  Used internally by remove_layer and rename_layer.
   To remove, pass NULL as new_layer_name
*/
static void symb_table_layer_rename_in_class(SymbTable_ptr self,
                                             const char* class_name,
                                             const char* old_layer_name,
                                             const char* new_layer_name)
{
  array_t* _class;
  const char* name; int i;

  SYMB_TABLE_CHECK_INSTANCE(self);

  _class = SymbTable_get_class_layer_names(self, class_name);
  arrayForEachItem(const char*, _class, i, name) {
    if (strcmp(name, old_layer_name) == 0) {
      FREE(name);
      if (new_layer_name != (const char*) NULL) {
        array_insert(const char*, _class, i,
                     util_strsav((char*) new_layer_name));
      }
      else {
        /* removing, shifts to fill the hole */
        int j;
        for (j=i+1; j < array_n(_class); ++j) {
          array_insert(const char*, _class, j-1,
                       array_fetch(const char*, _class, j));
        }
        _class->num -= 1;
      }

      break;
    }
  }
}

/*!
  \brief Internal service used by methods that handle layer classes

  Returns the array of layer class, or NULL when not existing.
   Resolves NULL class_name to the default class
*/
static array_t* symb_table_get_layers_from_class(const SymbTable_ptr self,
                                                 const char* class_name)
{
  string_ptr key;
  if (class_name == (const char*) NULL) {
    class_name = SymbTable_get_default_layers_class_name(self);
  }
  nusmv_assert(class_name != (const char*) NULL); /* must be set before */

  key =  UStringMgr_find_string(self->strings, (char*) class_name);
  return (array_t*) find_assoc(self->class_layers, (node_ptr) key);
}

/*!
  \brief Internal service used by methods that handle layer classes

  Returns the array of layer class, creating it when
   needed
*/
static array_t* symb_table_create_layers_class(const SymbTable_ptr self,
                                               const char* class_name)
{
  string_ptr key;
  array_t* _class;

  if (class_name == (const char*) NULL) {
    class_name = SymbTable_get_default_layers_class_name(self);
  }
  nusmv_assert(class_name != (const char*) NULL); /* must be set before */

  key =  UStringMgr_find_string(self->strings, (char*) class_name);
  _class = (array_t*) find_assoc(self->class_layers, (node_ptr) key);

  if (_class == (array_t*) NULL) {
    _class = array_alloc(const char*, 1);
    insert_assoc(self->class_layers, (node_ptr) key, (node_ptr) _class);
    NodeList_append(self->class_names,
                    (node_ptr) UStringMgr_get_string_text(key));
  }

  return _class;
}

/*!
  \brief Internal service used by methods that handle layer classes

  Returns the array of layer class, NULL if class does
   not exist.
*/
static array_t* symb_table_get_layers_class(const SymbTable_ptr self,
                                            const char* class_name)
{
  string_ptr key;
  array_t* _class;

  if (class_name == (const char*) NULL) {
    class_name = SymbTable_get_default_layers_class_name(self);
  }
  nusmv_assert(class_name != (const char*) NULL); /* must be set before */

  key =  UStringMgr_find_string(self->strings, (char*) class_name);
  _class = (array_t*) find_assoc(self->class_layers, (node_ptr) key);

  return _class;
}

/*!
  \brief Internal service used by
   SymbTable_get_array_define_flatten_body

  Returns the flattened body of an array define

  \sa SymbTable_get_array_define_flatten_body
*/
static node_ptr
symb_table_flatten_array_define(const SymbTable_ptr self,
                                const node_ptr body,
                                const node_ptr context)
{
  if (Nil == body) {
    return Nil;
  }

  switch (node_get_type(body)) {
  case ARRAY_DEF:
  case CONS:
    return NodeMgr_find_node(self->nodes, node_get_type(body),
                             symb_table_flatten_array_define(self, car(body), context),
                             symb_table_flatten_array_define(self, cdr(body), context));

  default:
    return Compile_FlattenSexp(self, body, context);
  }
}

/*!
  \brief Returns the type of a define.

  Returns the type of a define, for a discussion on symbol categories
  look the SymbCategory enum description in SymbTable.h
*/
static SymbCategory symb_table_detect_expr_category(const SymbTable_ptr st,
                                                    const Expr_ptr expr)
{
  SymbCategory cat = SYMBOL_INVALID;
  int ta;

  nusmv_assert(Nil != expr);

  ta = node_get_type(expr);

  /* We are not going to memoize results for constants and symbols,
     because we may infeer with the internal category memoization,
     which associates symbols with the respective category. We only
     memoize non-atomic expressions: in this way we do not conflict
     with the previous memoization */
  if (NEXT == node_get_type(expr)) {
    cat = SYMBOL_NEXT_DEFINE;
  }
  else if (node_is_leaf(expr)) {
    cat = SYMBOL_CONSTANT;
  }
  else if (DOT == ta || ATOM == ta || ARRAY == ta) {

    if (SymbTable_is_symbol_constant(st, expr)) {
      cat = SYMBOL_CONSTANT;
    }
    else if (SymbTable_is_symbol_state_var(st, expr) ||
             SymbTable_is_symbol_frozen_var(st, expr)) {
      cat = SYMBOL_STATE_DEFINE;
    }
    else if (SymbTable_is_symbol_input_var(st, expr)) {
      cat = SYMBOL_INPUT_DEFINE;
    }
    else if (SymbTable_is_symbol_function(st, expr)) {
      cat = SYMBOL_FUNCTION;
    }
    else if (SymbTable_is_symbol_variable_array(st, expr)) {
      if (SymbTable_is_symbol_state_var_array(st, expr) ||
          SymbTable_is_symbol_frozen_var_array(st, expr)) {
        cat = SYMBOL_STATE_DEFINE;
      }
      else {
        nusmv_assert(SymbTable_is_symbol_input_var_array(st, expr));
        cat = SYMBOL_INPUT_DEFINE;
      }
    }
  }
  else {
    /* Defines / params.. should be already given expanded. */
    SymbCategory l_cat = SYMBOL_INVALID;
    SymbCategory r_cat = SYMBOL_INVALID;

    boolean has_next;
    boolean has_state;
    boolean has_input;

    nusmv_assert(!SymbTable_is_symbol_declared(st, expr));

    /* Check for memoized data */
    cat = NODE_TO_INT(find_assoc(st->categories, expr));
    if (SYMBOL_INVALID != cat) {
      return cat;
    }

    /* No memoized data found.. proceed with expression analysis */
    if (Nil != car(expr)) {
      l_cat = symb_table_detect_expr_category(st, car(expr));
    }

    if (Nil != cdr(expr)) {
      r_cat = symb_table_detect_expr_category(st, cdr(expr));
    }

    has_next = ((SYMBOL_STATE_INPUT_NEXT_DEFINE == l_cat) ||
                (SYMBOL_STATE_INPUT_NEXT_DEFINE == r_cat) ||
                (SYMBOL_INPUT_NEXT_DEFINE == l_cat) ||
                (SYMBOL_INPUT_NEXT_DEFINE == r_cat) ||
                (SYMBOL_STATE_NEXT_DEFINE == l_cat) ||
                (SYMBOL_STATE_NEXT_DEFINE == r_cat) ||
                (SYMBOL_NEXT_DEFINE == l_cat) ||
                (SYMBOL_NEXT_DEFINE == r_cat));

    has_input = ((SYMBOL_STATE_INPUT_NEXT_DEFINE == l_cat) ||
                 (SYMBOL_STATE_INPUT_NEXT_DEFINE == r_cat) ||
                 (SYMBOL_INPUT_NEXT_DEFINE == l_cat) ||
                 (SYMBOL_INPUT_NEXT_DEFINE == r_cat) ||
                 (SYMBOL_STATE_INPUT_DEFINE == l_cat) ||
                 (SYMBOL_STATE_INPUT_DEFINE == r_cat) ||
                 (SYMBOL_INPUT_DEFINE == l_cat) ||
                 (SYMBOL_INPUT_DEFINE == r_cat));

    has_state = ((SYMBOL_STATE_INPUT_NEXT_DEFINE == l_cat) ||
                 (SYMBOL_STATE_INPUT_NEXT_DEFINE == r_cat) ||
                 (SYMBOL_STATE_NEXT_DEFINE == l_cat) ||
                 (SYMBOL_STATE_NEXT_DEFINE == r_cat) ||
                 (SYMBOL_STATE_INPUT_DEFINE == l_cat) ||
                 (SYMBOL_STATE_INPUT_DEFINE == r_cat) ||
                 (SYMBOL_STATE_DEFINE == l_cat) ||
                 (SYMBOL_STATE_DEFINE == r_cat));

    if (has_next && has_input && has_state) {
      cat = SYMBOL_STATE_INPUT_NEXT_DEFINE;
    }
    else if (has_next && has_input) {
      cat = SYMBOL_INPUT_NEXT_DEFINE;
    }
    else if (has_next && has_state) {
      cat = SYMBOL_STATE_NEXT_DEFINE;
    }
    else if (has_state && has_input) {
      cat = SYMBOL_STATE_INPUT_DEFINE;
    }
    else if (has_input) {
      cat = SYMBOL_INPUT_DEFINE;
    }
    else if (has_next) {
      cat = SYMBOL_NEXT_DEFINE;
    }
    else if (has_state) {
      cat = SYMBOL_STATE_DEFINE;
    }
    else if ((SYMBOL_CONSTANT == l_cat) ||
             (SYMBOL_CONSTANT == r_cat)) {
      cat = SYMBOL_CONSTANT;
    }
    else {
      /* the only remaining values */
      nusmv_assert(SYMBOL_INVALID == l_cat && SYMBOL_INVALID == r_cat);

      cat = SYMBOL_INVALID;
    }

    /* Memoize the result for the given expression. */
    insert_assoc(st->categories, expr, NODE_FROM_INT(cat));
  }

  return cat;
}
