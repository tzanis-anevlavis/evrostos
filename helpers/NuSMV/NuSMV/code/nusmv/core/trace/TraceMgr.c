/* ---------------------------------------------------------------------------


  This file is part of the ``trace'' package of NuSMV version 2.
  Copyright (C) 2003 by FBK-irst.

  NuSMV version 2 is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  NuSMV version 2 is distributed in the hope that it will be
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
  \author Ashutosh Trivedi, Marco Pensallorto
  \brief Routines related to TraceMgr's functionality.

  Primitives to create, query and manipulate TraceMgr are
  provided.

*/

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/trace/pkg_traceInt.h"

#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/TraceLabel.h"
#include "nusmv/core/trace/TraceMgr.h"
#include "nusmv/core/trace/Trace_private.h" /* To access special methods of Trace class */

/* plugins */
#include "nusmv/core/trace/plugins/TraceExplainer.h"
#include "nusmv/core/trace/plugins/TraceTable.h"
#include "nusmv/core/trace/plugins/TraceCompact.h"
#include "nusmv/core/trace/plugins/TraceXmlDumper.h"
#include "nusmv/core/trace/plugins/TraceEmpty.h"

/* executors */
#include "nusmv/core/trace/exec/BaseTraceExecutor.h"
#include "nusmv/core/trace/exec/CompleteTraceExecutor.h"
#include "nusmv/core/trace/exec/PartialTraceExecutor.h"

/* evaluators */
#include "nusmv/core/trace/eval/BaseEvaluator.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/parser/symbols.h"

#include "nusmv/core/utils/EnvObject.h"
#include "nusmv/core/utils/EnvObject_private.h"


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static enum st_retval
trace_manager_destroy_executor_entry(char *key, char *data, char * arg);

static int cmp_string_ptr(string_ptr* a, string_ptr* b);

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct TraceMgr_TAG
{
  INHERITS_FROM(EnvObject);

  array_t* trace_list;
  array_t* plugin_list;
  array_t* layer_names;

  hash_ptr complete_trace_executors;
  hash_ptr partial_trace_executors;
  BaseEvaluator_ptr evaluator;
  TraceOpt_ptr default_opt;

  int current_trace_number;
  int default_plugin;

  int internal_plugins_num;
} TraceMgr;


static void trace_manager_finalize(Object_ptr object, void* dummy);

static void trace_manager_deinit(TraceMgr_ptr self);

/*---------------------------------------------------------------------------*/
/* Definition of external functions                                          */
/*---------------------------------------------------------------------------*/

TraceMgr_ptr TraceMgr_create(NuSMVEnv_ptr env)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  TraceMgr_ptr self = ALLOC(TraceMgr, 1);

  TRACE_MGR_CHECK_INSTANCE(self);

  env_object_init(ENV_OBJECT(self), env);

  self->trace_list = array_alloc(Trace_ptr, 1);
  nusmv_assert(self->trace_list != (array_t *) ARRAY_OUT_OF_MEM);

  self->plugin_list = array_alloc(TracePlugin_ptr, 1);
  nusmv_assert(self->plugin_list != (array_t *) ARRAY_OUT_OF_MEM);

  self->layer_names = array_alloc(const char*, 1);
  nusmv_assert(self->layer_names != (array_t *) ARRAY_OUT_OF_MEM);

  self->complete_trace_executors = new_assoc();
  nusmv_assert((hash_ptr)(NULL) != self->complete_trace_executors);

  self->partial_trace_executors = new_assoc();
  nusmv_assert((hash_ptr)(NULL) != self->partial_trace_executors);

  self->current_trace_number = -1 ;  /* Not yet assigned */
  self->evaluator = BASE_EVALUATOR(NULL);

  /* Default plugin */
  self->default_plugin = get_default_trace_plugin(opts);

  self->internal_plugins_num = 0; /* number of plugins within NuSMV */
  self->default_opt = TraceOpt_create_from_env(env);

  OVERRIDE(Object, finalize) = trace_manager_finalize;

  return self;
}

void TraceMgr_destroy(TraceMgr_ptr self)
{
  TRACE_MGR_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

int TraceMgr_get_size(const TraceMgr_ptr self)
{
  TRACE_MGR_CHECK_INSTANCE(self);

  return array_n(self->trace_list);
}

int TraceMgr_get_plugin_size(const TraceMgr_ptr self)
{
  TRACE_MGR_CHECK_INSTANCE(self);

  return array_n(self->plugin_list);
}

int TraceMgr_get_internal_plugin_size(const TraceMgr_ptr self)
{
  TRACE_MGR_CHECK_INSTANCE(self);

  return self->internal_plugins_num;
}

Trace_ptr TraceMgr_get_trace_at_index(const TraceMgr_ptr self,
                                          int index)
{
  Trace_ptr trace;

  TRACE_MGR_CHECK_INSTANCE(self);

  nusmv_assert(index < array_n(self->trace_list));
  nusmv_assert(index >= 0);

  trace = array_fetch(Trace_ptr, self->trace_list, index);

  return trace;
}

TracePlugin_ptr
TraceMgr_get_plugin_at_index(const TraceMgr_ptr self, int index)
{
  TracePlugin_ptr plugin;

  TRACE_MGR_CHECK_INSTANCE(self);
  nusmv_assert(index >= 0);
  nusmv_assert(index < array_n(self->plugin_list) );

  plugin = array_fetch(TracePlugin_ptr, self->plugin_list, index);

  return plugin;
}

int TraceMgr_register_trace(TraceMgr_ptr self, Trace_ptr trace)
{
  int index;
  boolean status;

  TRACE_MGR_CHECK_INSTANCE(self);

  nusmv_assert(!Trace_is_registered(trace));

  status = array_insert_last(Trace_ptr, self->trace_list, trace);
  nusmv_assert(status != ARRAY_OUT_OF_MEM);

  index = array_n(self->trace_list) - 1;
  Trace_register(trace, index+1);  /* Friend function of Trace Class */

  return index;
}

int TraceMgr_register_plugin(TraceMgr_ptr self, TracePlugin_ptr plugin)
{
  int res;
  boolean status;

  TRACE_MGR_CHECK_INSTANCE(self);

  status = array_insert_last(TracePlugin_ptr, self->plugin_list, plugin);
  nusmv_assert(status != ARRAY_OUT_OF_MEM);

  res = array_n(self->plugin_list);

  /* returns the index of the plugin for further use as handle */
  return (res - 1);
}

BaseEvaluator_ptr TraceMgr_get_evaluator(TraceMgr_ptr self)

{
  TRACE_MGR_CHECK_INSTANCE(self);
  return self->evaluator;
}

void TraceMgr_register_evaluator(TraceMgr_ptr self,
                                     BaseEvaluator_ptr eval)
{
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  TRACE_MGR_CHECK_INSTANCE(self);

  env = ENV_OBJECT(self)->environment;
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (BASE_EVALUATOR(NULL) != self->evaluator) {
    BaseEvaluator_destroy(self->evaluator);
    self->evaluator = BASE_EVALUATOR(NULL);
  }

  if (opt_verbose_level_ge(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Registering evaluator\n");
  }

  self->evaluator = eval;
}

void TraceMgr_unregister_evaluator(TraceMgr_ptr self)

{
  TRACE_MGR_CHECK_INSTANCE(self);
  if (BASE_EVALUATOR(NULL) != self->evaluator) {
    BaseEvaluator_destroy(self->evaluator);
    self->evaluator = BASE_EVALUATOR(NULL);
  }
}

void TraceMgr_register_complete_trace_executor(TraceMgr_ptr self,
                  const char* executor_name, const char* executor_desc,
                  const CompleteTraceExecutor_ptr executor)
{
  node_ptr entry;
  CompleteTraceExecutor_ptr previous = COMPLETE_TRACE_EXECUTOR(NULL);
  string_ptr executor_id;

  /* get rid of const warnings */
  char* _executor_name = (char*)(executor_name);
  char* _executor_desc = (char*)(executor_desc);
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;
  UStringMgr_ptr strings;
  NodeMgr_ptr nodemgr;

  TRACE_MGR_CHECK_INSTANCE(self);

  env = ENV_OBJECT(self)->environment;
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
  nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  COMPLETE_TRACE_EXECUTOR_CHECK_INSTANCE(executor);
  nusmv_assert(NIL(char) != _executor_name);

  /* retrieve previously registered executor instance */
  executor_id =  UStringMgr_find_string(strings, _executor_name);
  entry = find_assoc(self->complete_trace_executors, NODE_PTR(executor_id));

  /* if a previous executor was registered, it has to be replaced */
  if (Nil != entry) {
    if (opt_verbose_level_ge(opts, 4)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Replacing complete trace executor '%s'\n",
               UStringMgr_get_string_text(executor_id));
    }

    /* destroying previously registered executor instance */
    previous = COMPLETE_TRACE_EXECUTOR(cdr(entry));
    BaseTraceExecutor_destroy(BASE_TRACE_EXECUTOR(previous));

    /* replacing description */
    setcar(entry, NODE_PTR(UStringMgr_find_string(strings, _executor_desc)));

    /* replacing instance */
    setcdr(entry, NODE_PTR(executor));
  }

  else {
    if (opt_verbose_level_ge(opts, 4)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Registering complete trace executor '%s'\n",
 UStringMgr_get_string_text(executor_id));
    }

    insert_assoc(self->complete_trace_executors,
                 NODE_PTR(executor_id),
                 cons(nodemgr, NODE_PTR(UStringMgr_find_string(strings, _executor_desc)),
                      NODE_PTR(executor)));
  }
}

array_t*
TraceMgr_get_complete_trace_executor_ids (const TraceMgr_ptr self)
{
  array_t* res = array_alloc(string_ptr, 1);
  node_ptr e;
  assoc_iter iter;

  TRACE_MGR_CHECK_INSTANCE(self);

  ASSOC_FOREACH(self->complete_trace_executors, iter, &e, NULL) {
    array_insert_last(string_ptr, res, (string_ptr)(e));
  }

  /* sort entries according to lexicographic order */
  array_sort(res, (int (*)(const void*, const void*))cmp_string_ptr);

  return res;
}

CompleteTraceExecutor_ptr
TraceMgr_get_default_complete_trace_executor (const TraceMgr_ptr self)
{
  string_ptr executor_id;
  node_ptr entry;
  array_t* tmp = TraceMgr_get_complete_trace_executor_ids(self);

  if (0 == array_n(tmp)) { return COMPLETE_TRACE_EXECUTOR(NULL); }

  executor_id = array_fetch(string_ptr, tmp, 0);
  entry = find_assoc(self->complete_trace_executors, NODE_PTR(executor_id));
  array_free(tmp);

  return COMPLETE_TRACE_EXECUTOR(cdr(entry));
}

PartialTraceExecutor_ptr
TraceMgr_get_default_partial_trace_executor (const TraceMgr_ptr self)
{
  string_ptr executor_id;
  node_ptr entry;
  array_t* tmp = TraceMgr_get_partial_trace_executor_ids(self);

  if (0 == array_n(tmp)) { return PARTIAL_TRACE_EXECUTOR(NULL); }

  executor_id = array_fetch(string_ptr, tmp, 0);
  entry = find_assoc(self->partial_trace_executors, NODE_PTR(executor_id));
  array_free(tmp);

  return PARTIAL_TRACE_EXECUTOR(cdr(entry));
}

void TraceMgr_register_partial_trace_executor(TraceMgr_ptr self,
                  const char* executor_name, const char* executor_desc,
                  const PartialTraceExecutor_ptr executor)
{
  node_ptr entry;
  PartialTraceExecutor_ptr previous = PARTIAL_TRACE_EXECUTOR(NULL);
  string_ptr executor_id;

  /* get rid of const warnings */
  char* _executor_name = (char*)(executor_name);
  char* _executor_desc = (char*)(executor_desc);
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;
  UStringMgr_ptr strings;
  NodeMgr_ptr nodemgr;

  TRACE_MGR_CHECK_INSTANCE(self);

  env = ENV_OBJECT(self)->environment;
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
  nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  PARTIAL_TRACE_EXECUTOR_CHECK_INSTANCE(executor);
  nusmv_assert(NIL(char) != _executor_name);

  executor_id = UStringMgr_find_string(strings, _executor_name);
  entry = find_assoc(self->partial_trace_executors, NODE_PTR(executor_id));

  /* if a previous executor was registered, it has to be replaced */
  if (Nil != entry) {
    if (opt_verbose_level_ge(opts, 4)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Replacing partial trace executor '%s'\n",
 UStringMgr_get_string_text(executor_id));
    }

    previous = PARTIAL_TRACE_EXECUTOR(cdr(entry));
    BaseTraceExecutor_destroy(BASE_TRACE_EXECUTOR(previous));

    /* replacing description */
    setcar(entry, NODE_PTR(UStringMgr_find_string(strings, _executor_desc)));

    /* replacing instance */
    setcdr(entry, NODE_PTR(executor));
  }

  else {
    if (opt_verbose_level_ge(opts, 4)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Registering partial trace executor '%s'\n",
 UStringMgr_get_string_text(executor_id));
    }

    insert_assoc(self->partial_trace_executors,
                 NODE_PTR(executor_id),
                 cons(nodemgr, NODE_PTR(UStringMgr_find_string(strings, _executor_desc)),
                      NODE_PTR(executor)));
  }
}

array_t*
TraceMgr_get_partial_trace_executor_ids (const TraceMgr_ptr self)
{
  array_t* res = array_alloc(string_ptr, 1);
  node_ptr e;
  assoc_iter iter;
  TRACE_MGR_CHECK_INSTANCE(self);

  ASSOC_FOREACH(self->partial_trace_executors, iter, &e, NULL) {
    array_insert_last(string_ptr, res, (string_ptr)(e));
  }

  /* sort entries according to lexicographic order */
  array_sort(res, (int(*)(const void*, const void*))cmp_string_ptr);

  return res;
}

void TraceMgr_register_layer(TraceMgr_ptr self, const char* layer_name)
{
  const char* name;
  int idx;
  boolean found = false;
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  TRACE_MGR_CHECK_INSTANCE(self);

  env = ENV_OBJECT(self)->environment;
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  /* does not registers if already registered */
  if (TraceMgr_is_layer_registered(self, layer_name)) return;

  /* first search for a hole */
  arrayForEachItem(const char*, self->layer_names, idx, name) {
    if (name == (const char*) NULL) {
      array_insert(const char*, self->layer_names, idx,
                   util_strsav((char*) layer_name));
      found = true;
      break;
    }
  }

  if (!found) {
    /* if not inserted in a hole, push at the end */
    array_insert_last(const char*, self->layer_names,
                      util_strsav((char*) layer_name));
  }

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "TraceMgr: registered layer '%s'\n", layer_name);
  }

}

void TraceMgr_unregister_layer(TraceMgr_ptr self, const char* layer_name)
{
  int idx;
  const char* name;
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;
  ErrorMgr_ptr errmgr;

  TRACE_MGR_CHECK_INSTANCE(self);

  env = ENV_OBJECT(self)->environment;
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "TraceMgr: unregistering layer '%s'...\n",
            layer_name);
  }

  arrayForEachItem(const char*, self->layer_names, idx, name) {
    if ((name != (const char*) NULL) && strcmp(name, layer_name) == 0) {
      FREE(name);
      array_insert(const char*, self->layer_names, idx, NULL);
      return;
    }
  }

  ErrorMgr_internal_error(errmgr, "TraceMgr_unregister_layer: "\
                 "given layer had not been registered\n");
}

boolean TraceMgr_is_layer_registered(const TraceMgr_ptr self,
                                         const char* layer_name)
{
  int idx;
  const char* name;

  TRACE_MGR_CHECK_INSTANCE(self);

  arrayForEachItem(const char*, self->layer_names, idx, name) {
    if ((name != (const char*) NULL) &&
        strcmp(name, layer_name) == 0) return true;
  }

  return false;
}

array_t* TraceMgr_get_registered_layers(const TraceMgr_ptr self)
{
  TRACE_MGR_CHECK_INSTANCE(self);
  return self->layer_names;
}

int TraceMgr_execute_plugin(const TraceMgr_ptr self,
			    const TraceOpt_ptr opt,
			    int plugin_index,
			    int trace_index)
{
  StreamMgr_ptr streams;
  Trace_ptr trace;
  TracePlugin_ptr plugin;
  int res;

  NuSMVEnv_ptr env;

  TRACE_MGR_CHECK_INSTANCE(self);

  env = ENV_OBJECT(self)->environment;
  streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  trace = \
    TraceMgr_get_trace_at_index(self, (0 <= trace_index)
                                    ? trace_index
                                    : (array_n(self->trace_list) - 1));

  plugin = \
    TraceMgr_get_plugin_at_index(self, (0 <= plugin_index)
                                     ? plugin_index
                                     : TraceMgr_get_default_plugin(self));

  if (TraceMgr_get_plugin_size(self) < plugin_index) {
    StreamMgr_print_error(streams,
            "Warning: Requested plugin %d is not available.\n", plugin_index);
    return 1;
  }

  /* if no options were provided, update internal defaults from env */
  if (TRACE_OPT(NULL) == opt) {
    TraceOpt_update_from_env(self->default_opt, env);
  }

  res = TracePlugin_action(plugin, trace, TRACE_OPT(NULL) != opt
                           ? opt : self->default_opt);

  return res;
}

void TraceMgr_set_current_trace_number(TraceMgr_ptr self, int trace_id)
{
  self->current_trace_number = trace_id;
}

int TraceMgr_get_current_trace_number(TraceMgr_ptr self)
{
  return self->current_trace_number;
}

void TraceMgr_set_default_plugin(TraceMgr_ptr self, int plugin_id)
{
  self->default_plugin = plugin_id;
}

int TraceMgr_get_default_plugin(TraceMgr_ptr self)
{
  return self->default_plugin;
}

boolean TraceMgr_is_plugin_internal(const TraceMgr_ptr self, int index)
{
  return (index >= 0 && index < self->internal_plugins_num);
}

TraceIter TraceMgr_get_iterator_from_label(TraceMgr_ptr self,
                                           TraceLabel label)
{
  Trace_ptr trace = TRACE(NULL);
  TraceIter res = TRACE_END_ITER;
  int state_no, i;

  TRACE_MGR_CHECK_INSTANCE(self);

  state_no = TraceMgr_get_abs_index_from_label(self, label);
  trace = TraceMgr_get_trace_at_index(self,
                                          TraceLabel_get_trace(label));
  res = Trace_first_iter(trace);
  for (i = 0; i < state_no; ++ i) {
    res = TraceIter_get_next(res);
    if (TRACE_END_ITER == res) {
      const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
      ErrorMgr_internal_error(errmgr, "%s:%d:%s: invalid trace number (%d)",
                     __FILE__, __LINE__, __func__, state_no);
    }
  }

  return res;
}

int TraceMgr_get_abs_index_from_label(TraceMgr_ptr self,
                                      TraceLabel label)
{
  Trace_ptr trace;
  int trace_no = TraceLabel_get_trace(label);
  int state_no = TraceLabel_get_state(label);

  nusmv_assert(trace_no >= 0 && trace_no < TraceMgr_get_size(self));

  trace = TraceMgr_get_trace_at_index(self, trace_no);

  if (state_no < -1) state_no = Trace_get_length(trace) + (state_no+2);

  nusmv_assert(state_no >= 0 && state_no <= Trace_get_length(trace));
  return state_no;
}

boolean TraceMgr_is_label_valid(TraceMgr_ptr self, TraceLabel label)
{
  int trace_no, trace_len, state_no;

  trace_no = TraceLabel_get_trace(label) ;
  if (trace_no < 0 || trace_no >= TraceMgr_get_size(self))  { return false; }

  trace_len = Trace_get_length(TraceMgr_get_trace_at_index(self, trace_no));

  /* negative state numbers are legal, and they are used to denote
     states rightwards from the last state of the trace (i.e. -1 is
     last state, -2 is previous and so forth) */
  state_no = abs(TraceLabel_get_state(label) +1);

  if (state_no < 1 || (1 + trace_len) < state_no) { return false; }

  /* here label is valid */
  return true;
}

void TraceMgr_init_plugins(TraceMgr_ptr self)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  TracePlugin_ptr plugin;
  int index = -1;

  /* Not previously registered: */
  nusmv_assert(self->internal_plugins_num == 0);

  /* 0. TRACE EXPLAINER - changes only */
  plugin = TRACE_PLUGIN(TraceExplainer_create(true));
  index = TraceMgr_register_plugin(self, plugin);
  NuSMVEnv_set_value(env, ENV_TRACE_EXPLAINER_CHANGES_ONLY, PTR_FROM_INT(void*, index + 1));

  /* 1. BASIC TRACE EXPLAINER  */
  plugin = TRACE_PLUGIN(TraceExplainer_create(false));
  index = TraceMgr_register_plugin(self, plugin);
  NuSMVEnv_set_value(env, ENV_TRACE_EXPLAINER, PTR_FROM_INT(void*, index + 1));

  /* 2. TRACE TABLE PLUGIN -- column format */
  plugin = TRACE_PLUGIN(TraceTable_create(TRACE_TABLE_TYPE_COLUMN));
  index = TraceMgr_register_plugin(self, plugin);
  NuSMVEnv_set_value(env, ENV_TRACE_TABLE_COLUMN, PTR_FROM_INT(void*, index + 1));

  /* 3. TRACE TABLE PLUGIN -- row format */
  plugin = TRACE_PLUGIN(TraceTable_create(TRACE_TABLE_TYPE_ROW));
  index = TraceMgr_register_plugin(self, plugin);
  NuSMVEnv_set_value(env, ENV_TRACE_TABLE_ROW, PTR_FROM_INT(void*, index + 1));

  /* 4. TRACE XML DUMP PLUGIN */
  plugin = TRACE_PLUGIN(TraceXmlDumper_create(false));
  index = TraceMgr_register_plugin(self, plugin);
  NuSMVEnv_set_value(env, ENV_TRACE_XML_DUMPER, PTR_FROM_INT(void*, index + 1));

  /* 5. TRACE COMPACT PLUGIN  */
  plugin = TRACE_PLUGIN(TraceCompact_create());
  index = TraceMgr_register_plugin(self, plugin);
  NuSMVEnv_set_value(env, ENV_TRACE_COMPACT, PTR_FROM_INT(void*, index + 1));

  /* 6. TRACE EMBEDDED XML DUMP PLUGIN   */
  plugin = TRACE_PLUGIN(TraceXmlDumper_create(true));
  index = TraceMgr_register_plugin(self, plugin);
  NuSMVEnv_set_value(env, ENV_TRACE_EMBEDDED_XML_DUMPER, PTR_FROM_INT(void*, index + 1));

  /* 7. TRACE EMPTY PLUGIN   */
  plugin = TRACE_PLUGIN(TraceEmpty_create());
  index = TraceMgr_register_plugin(self, plugin);
  NuSMVEnv_set_value(env, ENV_TRACE_EMPTY_INDEX, VOIDPTR_FROM_INT(index+1));

  self->internal_plugins_num = TraceMgr_get_plugin_size(self);
}

const char* TraceMgr_get_complete_trace_executor_desc(const TraceMgr_ptr self,
                                                      const char* name)
{
  TRACE_MGR_CHECK_INSTANCE(self);
  nusmv_assert(NIL(char) != name);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    UStringMgr_ptr strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));

    node_ptr entry = find_assoc(self->complete_trace_executors,
                                NODE_PTR(UStringMgr_find_string(strings, (char*)(name))));
    nusmv_assert(Nil != entry);
    return UStringMgr_get_string_text((string_ptr)(car(entry)));
  }
}

CompleteTraceExecutor_ptr
TraceMgr_get_complete_trace_executor(const TraceMgr_ptr self,
                                         const char* name)
{
  TRACE_MGR_CHECK_INSTANCE(self);
  nusmv_assert(NIL(char) != name);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    UStringMgr_ptr strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));

    node_ptr entry = find_assoc(self->complete_trace_executors,
                                NODE_PTR(UStringMgr_find_string(strings, (char*)(name))));
    if (Nil == entry) { return COMPLETE_TRACE_EXECUTOR(NULL); }
    return COMPLETE_TRACE_EXECUTOR(cdr(entry));
  }
}

const char* TraceMgr_get_partial_trace_executor_desc(const TraceMgr_ptr self,
                                                     const char* name)
{
  TRACE_MGR_CHECK_INSTANCE(self);
  nusmv_assert(NIL(char) != name);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    UStringMgr_ptr strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));

    node_ptr entry = find_assoc(self->partial_trace_executors,
                                NODE_PTR(UStringMgr_find_string(strings, (char*)(name))));
    nusmv_assert(Nil != entry);
    return UStringMgr_get_string_text((string_ptr)(car(entry)));
  }
}

PartialTraceExecutor_ptr
TraceMgr_get_partial_trace_executor(const TraceMgr_ptr self,
                                        const char* name)
{
  TRACE_MGR_CHECK_INSTANCE(self);
  nusmv_assert(NIL(char) != name);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    UStringMgr_ptr strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));

    node_ptr entry = find_assoc(self->partial_trace_executors,
                                NODE_PTR(UStringMgr_find_string(strings, (char*)(name))));
    if (Nil == entry) { return PARTIAL_TRACE_EXECUTOR(NULL); }
    return PARTIAL_TRACE_EXECUTOR(cdr(entry));
  }
}

boolean TraceMgr_is_visible_symbol(TraceMgr_ptr self, node_ptr symbol)
{
  boolean res = true;
  char* name = NIL(char);

  NuSMVEnv_ptr env;
  MasterPrinter_ptr wffprint;
  const char* pref;

  TRACE_MGR_CHECK_INSTANCE(self);

  env = ENV_OBJECT(self)->environment;
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  /* update internal defaults from env */
  TraceOpt_update_from_env(self->default_opt, env);
  pref = TraceOpt_hiding_prefix(self->default_opt);

  nusmv_assert(Nil != symbol);

  {
    node_ptr iter = symbol;
    while ((Nil != iter) && (NIL(char) == name)) {

      switch (node_get_type(iter)) {
        case ATOM: case BIT: name = sprint_node(wffprint, iter); break;
        case ARRAY: iter = car(iter); break;
        case CONTEXT: iter = cdr(iter); break;
        case DOT: { /* check if it name.number */
          node_ptr r = cdr(iter);

          if ((Nil != r) && (NUMBER == node_get_type(r))) {
            /* found a name */
            name = sprint_node(wffprint, iter);
          }
          else
            iter = cdr(iter);

          break;
        }

        default: error_unreachable_code(); /* not handled */
      } /* switch */
    } /* while */
  }

  nusmv_assert(NIL(char) != name);

  if ((NIL(const char) != pref) && (name == strstr(name, pref))) {
    /* matches hidden prefix, this node is not printable */
    res = false;
  }

#if NUSMV_HAVE_REGEX_H
  if (res) {
    /* tries with matching regular expression */
    int cr = 1;

    regex_t* re = TraceOpt_regexp(self->default_opt);
    if ((regex_t*) NULL != re) {
      cr = regexec(re, name, 0, NULL, 0);
      res = (0 == cr);
    }
  }
#endif

  FREE(name);
  return res;
}

int TraceMgr_show_traces(TraceMgr_ptr const self,
                         const int plugin_index,
                         const boolean is_all,
                         const int trace,
                         TraceOpt_ptr const trace_opt,
                         const int traceno,
                         int from_state,
                         int to_state)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  StreamMgr_ptr const streams =
   STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  int res = 1;

  /* A trace header will be not printed when the plugin is the XML
     dumper or dynamically registered external plugins: */
  boolean print_header =
    (plugin_index != PTR_TO_INT(NuSMVEnv_get_value(env, ENV_TRACE_XML_DUMPER)) - 1) &&
    (plugin_index != PTR_TO_INT(NuSMVEnv_get_value(env, ENV_TRACE_EMBEDDED_XML_DUMPER)) - 1) &&
    (TraceMgr_is_plugin_internal(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), plugin_index));

  StreamMgr_set_indent_size(streams, 2);

  if (is_all == false) { /* state interval selection check */

    Trace_ptr t = TraceMgr_get_trace_at_index(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                                              trace - 1);

    { /* check from_state */

      /*  if backward index, translate it to forward */
      if (from_state < 0) {
        from_state = 2 + Trace_get_length(t) + from_state;
      }

      if (0 < from_state) { /* forward index */
        if (from_state > 1 + Trace_get_length(t)) {
          StreamMgr_print_error(streams,
                                "Invalid starting state specified: %d\n", from_state);
          res = 1; return res;
        }
      }

      TraceOpt_set_from_here(trace_opt, from_state);
    } /* check from state */


    { /* check to_state */
      /*  if backward index, translate it to forward */
      if (to_state < 0) {
        to_state = 2 + Trace_get_length(t) + to_state;
      }

      if (0 < to_state) { /* forward index */
        if (to_state > 1 + Trace_get_length(t)) {
          StreamMgr_print_error(streams,
                                "Invalid end state specified: %d\n", to_state);
          res = 1; return res;
        }
      }
      else { to_state = from_state; }

      TraceOpt_set_to_here(trace_opt, to_state);
    } /* check to_state */

    if (to_state < from_state) {
      StreamMgr_print_error(streams,  "Invalid state range specified: %d:%d\n",
                            from_state, to_state);

      res = 1; return res;
    }

    if (print_header) {
      StreamMgr_print_output(streams,
                             "<!-- ################### Trace number: %d #################"
                             "## -->\n", trace);
    }

    TraceMgr_execute_plugin(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                            trace_opt, plugin_index, trace - 1);
  }
  else {
    int c;
    for (c = 0; c < traceno; c++){
      if (print_header) {
        StreamMgr_print_output(streams,
                               "<!-- ################### Trace number: %d #################"\
                               "## -->\n", c+1);
      }

      TraceMgr_execute_plugin(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                              trace_opt, plugin_index, c);
    }
  }

  res = 0; return res;
}

int TraceMgr_show_plugins(TraceMgr_ptr const self,
                          const boolean is_show_all,
                          const int dp)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  StreamMgr_ptr const streams =
   STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  if (is_show_all || (dp < 0)) {
    int i;

    if (TraceMgr_get_plugin_size(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR))) <= 0) {
      StreamMgr_print_error(streams,  "There are no registered plugins to be shown\n");

      return 1;
    }

    for (i = 0; i < TraceMgr_get_plugin_size(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR))); i++){
      TracePlugin_ptr p_i = \
        TraceMgr_get_plugin_at_index(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), i);

      if (i == TraceMgr_get_default_plugin(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)))) {
        StreamMgr_print_output(streams,  "[D]  %d\t %s\n", i, TracePlugin_get_desc(p_i));
      }
      else {
        StreamMgr_print_output(streams,  "     %d\t %s\n", i, TracePlugin_get_desc(p_i));
      }
    }

    return 0;
  }
  else {
    TracePlugin_ptr p_i;

    if (dp < TraceMgr_get_plugin_size(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)))) {
      p_i = TraceMgr_get_plugin_at_index(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), dp);

      if (dp == TraceMgr_get_default_plugin(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)))) {
        StreamMgr_print_output(streams,  "[D]  %d\t %s\n", dp, TracePlugin_get_desc(p_i));
      }
      else {
        StreamMgr_print_output(streams,  "     %d\t %s\n", dp, TracePlugin_get_desc(p_i));
      }
    }
    else {
      StreamMgr_print_error(streams,  "Error: Plugin %d is not yet registered\n", dp);
      return 1;
    }
  }

  return 0;
}

int TraceMgr_execute_traces(TraceMgr_ptr const self,
                            CompleteTraceExecutor_ptr const executor,
                            const int first_trace,
                            const int last_trace)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  int trace_iter;

  for (trace_iter = first_trace; trace_iter <= last_trace; ++trace_iter) {
    Trace_ptr trace;
    int trace_index = trace_iter - 1;

    trace = TraceMgr_get_trace_at_index(self, trace_index);

    if (Trace_execute_trace(env, trace, executor)) return 1;
  }

  return 0;
}

int TraceMgr_execute_partial_traces(TraceMgr_ptr const self,
                                    PartialTraceExecutor_ptr const executor,
                                    const int first_trace,
                                    const int last_trace)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  int trace_iter;

  for (trace_iter = first_trace; trace_iter <= last_trace; ++trace_iter) {
    Trace_ptr trace;
    int trace_index = trace_iter - 1;

    trace = TraceMgr_get_trace_at_index(self, trace_index);
    {
      /* determine new trace language */
      SexpFsm_ptr sexp_fsm = SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));

      if (Trace_execute_partial_trace(env, trace, executor,
                                      SexpFsm_get_symbols_list(sexp_fsm))) {
        return 1;
      }
    }
  }

  return 0;
}

/*---------------------------------------------------------------------------*/
/* Static functions definitions                                              */
/*---------------------------------------------------------------------------*/
static enum st_retval
trace_manager_destroy_executor_entry (char *key, char *data, char * arg)
{
  node_ptr entry = NODE_PTR(data);
  BaseTraceExecutor_ptr executor = BASE_TRACE_EXECUTOR(cdr(entry));
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(executor));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  BaseTraceExecutor_destroy(executor); free_node(nodemgr, entry);
  return ST_DELETE;
}

/* string comparison function used by
   TraceMgr_get_partial_trace_executor_ids and
   TraceMgr_get_complete_trace_executor_ids */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int cmp_string_ptr(string_ptr* a, string_ptr* b)
{
  return strcmp(UStringMgr_get_string_text(*a), UStringMgr_get_string_text(*b));
}

/*!
  \brief The TraceMgr class virtual finalizer

  Called by the class destructor
*/
static void trace_manager_finalize(Object_ptr object, void* dummy)
{
  TraceMgr_ptr self = TRACE_MGR(object);

  trace_manager_deinit(self);
  FREE(self);
}

/*!
  \brief


*/
static void trace_manager_deinit(TraceMgr_ptr self)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  int i, num;

  num = TraceMgr_get_size(self);
  for (i = 0; i < num; ++i) {
    Trace_ptr trace = TraceMgr_get_trace_at_index(self, i);
    Trace_unregister(trace);
    Trace_destroy(trace);
  }
  array_free(self->trace_list);

  /* destroying registered plugins */
  num = array_n(self->plugin_list);
  for (i = 0; i < num; ++i) {
    Object_destroy(OBJECT(TraceMgr_get_plugin_at_index(self, i)), NULL);
  }
  array_free(self->plugin_list);

  NuSMVEnv_remove_value(env, ENV_TRACE_EXPLAINER_CHANGES_ONLY);
  NuSMVEnv_remove_value(env, ENV_TRACE_EXPLAINER);
  NuSMVEnv_remove_value(env, ENV_TRACE_TABLE_COLUMN);
  NuSMVEnv_remove_value(env, ENV_TRACE_TABLE_ROW);
  NuSMVEnv_remove_value(env, ENV_TRACE_XML_DUMPER);
  NuSMVEnv_remove_value(env, ENV_TRACE_COMPACT);
  NuSMVEnv_remove_value(env, ENV_TRACE_EMBEDDED_XML_DUMPER);
  NuSMVEnv_remove_value(env, ENV_TRACE_EMPTY_INDEX);

  num = array_n(self->layer_names);
  for (i = 0; i < num; ++i) {
    const char* name = array_fetch(const char*, self->layer_names, i);
    if (name != (const char*) NULL) { FREE(name); }
  }
  array_free(self->layer_names);

  /* destroying registered executors */
  clear_assoc_and_free_entries(self->complete_trace_executors,
                               trace_manager_destroy_executor_entry);
  clear_assoc_and_free_entries(self->partial_trace_executors,
                               trace_manager_destroy_executor_entry);
  free_assoc(self->complete_trace_executors);
  free_assoc(self->partial_trace_executors);

  /* destroying registered evaluator (if any) */
  TraceMgr_unregister_evaluator(self);

  /* free default trace opt instance */
  TraceOpt_destroy(self->default_opt);

}
