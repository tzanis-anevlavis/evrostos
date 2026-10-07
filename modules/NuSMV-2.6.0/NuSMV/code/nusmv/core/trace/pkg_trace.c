/* ---------------------------------------------------------------------------


  This file is part of the ``trace'' package of NuSMV version 2.
  Copyright (C) 2003 by FBK-irst.

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
  \author Ashutosh Trivedi
  \brief Routines related to the trace Package.

  This file contains routines related to Initializing and
               Quitting trace package. 

*/


#if HAVE_CONFIG_H
#include "nusmv-config.h"
#endif

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/trace/pkg_traceInt.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/cinit/NuSMVEnv.h"
#include "nusmv/core/trace/loaders/TraceXmlLoader.h"

#if NUSMV_HAVE_REGEX_H
 #include <regex.h>
#endif
/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of external functions                                          */
/*---------------------------------------------------------------------------*/

void TracePkg_init(NuSMVEnv_ptr env)
{
  TraceMgr_ptr tm;

  if (NuSMVEnv_has_value(env, ENV_TRACE_MGR)) {
    tm = TRACE_MGR(NuSMVEnv_remove_value(env, ENV_TRACE_MGR));
    TraceMgr_destroy(tm);
  }

  tm = TraceMgr_create(env);
  nusmv_assert(tm != TRACE_MGR(NULL));

  NuSMVEnv_set_value(env, ENV_TRACE_MGR, tm);

  TraceMgr_init_plugins(tm);

  TraceMgr_register_evaluator(tm, BaseEvaluator_create());
}

void TracePkg_quit(NuSMVEnv_ptr env)
{
  TraceMgr_ptr tm = NuSMVEnv_remove_value(env, ENV_TRACE_MGR);

  if (tm != TRACE_MGR(NULL)) { TraceMgr_destroy(tm); }
}

boolean TracePkg_set_default_trace_plugin(TraceMgr_ptr gtm, int dp)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(gtm));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  int avail_plugins = 1 + TraceMgr_get_plugin_size(gtm);

  /* negative indexes not allowed */
  if (dp < 0) {
    StreamMgr_print_error(streams, "Error: Not a proper plugin to show a trace \n");
    return false;
  }

  /* dp must be one of avail plugins */
  if (avail_plugins < dp) {
    StreamMgr_print_error(streams, "Error: Plugin %d is not currently available\n", dp);
    return false;
  }

  TraceMgr_set_default_plugin(gtm, dp);
  return true;
}

int TracePkg_get_default_trace_plugin(TraceMgr_ptr gtm)
{
  return TraceMgr_get_default_plugin(gtm);
}

NodeList_ptr TracePkg_get_filtered_symbols(TraceMgr_ptr gtm,
                                           const NodeList_ptr symbols)
{
  NodeList_ptr res = NodeList_create();
  ListIter_ptr iter;

  NODE_LIST_FOREACH(symbols, iter) {
    node_ptr symb = NodeList_get_elem_at(symbols, iter);
    if (TraceMgr_is_visible_symbol(gtm, symb)) {
      NodeList_append(res, symb);
    }
  }

  return res;
}

Trace_ptr TracePkg_read_trace(NuSMVEnv_ptr env,
                              SexpFsm_ptr sexp_fsm,
                              const char* filename,
                              boolean halt_if_undef,
                              boolean halt_if_wrong_section)
{
  Trace_ptr trace = NULL;

#if NUSMV_HAVE_LIBXML2
  TraceXmlLoader_ptr loader =
    TraceXmlLoader_create(filename, halt_if_undef, halt_if_wrong_section);

  trace = TraceLoader_load_trace(TRACE_LOADER(loader),
                                 SexpFsm_get_symb_table(sexp_fsm),
                                 SexpFsm_get_symbols_list(sexp_fsm));

  Object_destroy(OBJECT(loader), NULL);
#endif

  return trace;
}

int TracePkg_execute_traces(NuSMVEnv_ptr env,
                            TraceMgr_ptr trace_mgr,
                            FILE* output_stream,
                            char* engine,
                            int verbosity,
                            int trace_no)
{
  int first_trace = 0;
  int last_trace = 0;
  int res = 0;
  int old_verbosity = 0;
  FILE* old_stream = NULL;

  BaseTraceExecutor_ptr executor = NULL;
  
  if (NULL == engine) {
    executor =
      BASE_TRACE_EXECUTOR(TraceMgr_get_default_complete_trace_executor(trace_mgr));
  }
  else {
    executor =
      BASE_TRACE_EXECUTOR(TraceMgr_get_complete_trace_executor(trace_mgr, engine));
  }

  BASE_TRACE_EXECUTOR_CHECK_INSTANCE(executor);
  
  /* select traces range to be executed */
  if (0 == trace_no) {
    first_trace = 1;
    last_trace = TraceMgr_get_size(trace_mgr);
  }
  else {
    first_trace = trace_no;
    last_trace = trace_no;
  }

  old_verbosity = BaseTraceExecutor_get_verbosity(executor);
  BaseTraceExecutor_set_verbosity(executor, verbosity);

  old_stream = BaseTraceExecutor_get_output_stream(executor);

  BaseTraceExecutor_set_output_stream(executor, output_stream);

  res = TraceMgr_execute_traces(trace_mgr, COMPLETE_TRACE_EXECUTOR(executor),
                                first_trace, last_trace);

  /* restore previous values into the executor instance */
  BaseTraceExecutor_set_verbosity(executor, old_verbosity);
  BaseTraceExecutor_set_output_stream(executor, old_stream);

  return res;
}

int TracePkg_execute_partial_traces(NuSMVEnv_ptr env,
                                    TraceMgr_ptr trace_mgr,
                                    FILE* output_stream,
                                    char* engine,
                                    int verbosity,
                                    int trace_no)
{
  int first_trace = 0;
  int last_trace = 0;
  int res = 0;
  int old_verbosity = 0;
  FILE* old_stream = NULL;

  BaseTraceExecutor_ptr executor = NULL;
  
  if (NULL == engine) {
    executor =
      BASE_TRACE_EXECUTOR(TraceMgr_get_default_partial_trace_executor(trace_mgr));
  }
  else {
    executor =
      BASE_TRACE_EXECUTOR(TraceMgr_get_partial_trace_executor(trace_mgr, engine));
  }

  BASE_TRACE_EXECUTOR_CHECK_INSTANCE(executor);
  
  /* select traces range to be executed */
  if (0 == trace_no) {
    first_trace = 1;
    last_trace = TraceMgr_get_size(trace_mgr);
  }
  else {
    first_trace = trace_no;
    last_trace = trace_no;
  }

  old_verbosity = BaseTraceExecutor_get_verbosity(executor);
  BaseTraceExecutor_set_verbosity(executor, verbosity);

  old_stream = BaseTraceExecutor_get_output_stream(executor);

  BaseTraceExecutor_set_output_stream(executor, output_stream);

  res = TraceMgr_execute_partial_traces(trace_mgr, PARTIAL_TRACE_EXECUTOR(executor),
                                        first_trace, last_trace);

  /* restore previous values into the executor instance */
  BaseTraceExecutor_set_verbosity(executor, old_verbosity);
  BaseTraceExecutor_set_output_stream(executor, old_stream);

  return res;
}
