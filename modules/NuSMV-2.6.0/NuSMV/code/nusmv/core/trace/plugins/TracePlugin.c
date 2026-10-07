/* ---------------------------------------------------------------------------


  This file is part of the ``trace.plugins'' package of NuSMV version 2.
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
  \author Ashutosh Trivedi, Marco Pensallorto
  \brief Routines related to TracePlugin object.

  This file contains the definition of \"TracePlugin\" class.

*/

#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/trace/pkg_traceInt.h"

#include "nusmv/core/trace/plugins/TracePlugin.h"
#include "nusmv/core/trace/plugins/TracePlugin_private.h"

#include "nusmv/core/trace/TraceMgr.h"
#include "nusmv/core/trace/Trace_private.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/parser/symbols.h"

#include "nusmv/core/utils/utils_io.h"
/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void trace_plugin_finalize(Object_ptr object, void *dummy);
static void trace_plugin_prepare_action(TracePlugin_ptr self,
                                        Trace_ptr trace,
                                        TraceOpt_ptr opt);
static void trace_plugin_cleanup_action(TracePlugin_ptr self);
/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of external functions                                          */
/*---------------------------------------------------------------------------*/

int TracePlugin_action(const TracePlugin_ptr self, const Trace_ptr trace,
                       const TraceOpt_ptr opt)
{
  int res;
  unsigned from, to;

  SymbTable_ptr st;
  NuSMVEnv_ptr env;
  ErrorMgr_ptr errmgr;

  TRACE_PLUGIN_CHECK_INSTANCE(self);
  TRACE_CHECK_INSTANCE(self);
  TRACE_OPT_CHECK_INSTANCE(opt);

  st = Trace_get_symb_table(trace);
  env = EnvObject_get_environment(ENV_OBJECT(st));
  errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  /* verify iterators sanity before dumping */
  from = TraceOpt_from_here(opt);
  to = TraceOpt_to_here(opt);
  if ((0 < from) && (0 < to) && (to < from)) {
    ErrorMgr_internal_error(errmgr, "%s:%d:%s: invalid range detected (%d-%d). Aborting dump",
                   __FILE__, __LINE__, __func__, from, to);
  }

  { /* protected block */
    trace_plugin_prepare_action(self, trace, opt);

    CATCH(errmgr) {
      res = self->action(self);
    }
    FAIL(errmgr) { res = -1; }

    trace_plugin_cleanup_action(self);
  } /* protected block */

  return res;
}

void TracePlugin_print_symbol(const TracePlugin_ptr self, node_ptr symb)
{
  TRACE_PLUGIN_CHECK_INSTANCE(self);
  self->print_symbol(self, symb);
}

void TracePlugin_print_list(const TracePlugin_ptr self, node_ptr list)
{
  TRACE_PLUGIN_CHECK_INSTANCE(self);
  self->print_list(self, list);
}

void TracePlugin_print_assignment(const TracePlugin_ptr self, node_ptr symb,
                                  node_ptr val)
{
  TRACE_PLUGIN_CHECK_INSTANCE(self);
  self->print_assignment(self, symb, val);
}

char* TracePlugin_get_desc(const TracePlugin_ptr self)
{
  TRACE_PLUGIN_CHECK_INSTANCE(self);

  return self->desc;
}

int trace_plugin_action(const TracePlugin_ptr self)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
  return 0;
}

void trace_plugin_print_symbol(const TracePlugin_ptr self, node_ptr val)
{
  SymbTable_ptr st = trace_get_symb_table(self->trace);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  OStream_ptr ostr = TraceOpt_output_stream(self->opt);

    print_node(wffprint, OStream_get_stream(ostr),
               NULL != self->anonymizer
               ? NodeAnonymizerBase_map_expr(self->anonymizer, val)
               : val);
}

void trace_plugin_print_list(const TracePlugin_ptr self, node_ptr list)
{
  if (Nil == list) return;
  if (UNION == node_get_type(list)) {
    TracePlugin_print_symbol(self, car(list));
    OStream_printf(TraceOpt_output_stream(self->opt), ", ");
    TracePlugin_print_list(self, cdr(list));
  }
  else {
    TracePlugin_print_symbol(self, list);
  }
}

void trace_plugin_print_assignment(const TracePlugin_ptr self, node_ptr symb,
                                   node_ptr val)
{
  OStream_ptr out = TraceOpt_output_stream(self->opt);

  OStream_printf(out, "  ");
  TracePlugin_print_symbol(self, symb);
  OStream_printf(out, " = ");
  TracePlugin_print_list(self, val);
  OStream_printf(out, "\n");
}

void trace_plugin_init(TracePlugin_ptr self, char* desc)
{
  object_init(OBJECT(self));

  OVERRIDE(Object, finalize) = trace_plugin_finalize;
  OVERRIDE(TracePlugin, action) = trace_plugin_action;
  OVERRIDE(TracePlugin, print_symbol) = trace_plugin_print_symbol;
  OVERRIDE(TracePlugin, print_list) = trace_plugin_print_list;
  OVERRIDE(TracePlugin, print_assignment) = trace_plugin_print_assignment;

  self->desc = ALLOC(char, strlen(desc) + 1);
  nusmv_assert(self->desc != (char*) NULL);
  strncpy(self->desc, desc, strlen(desc) + 1);

  /* action params initialization */
  self->trace = TRACE(NULL);
  self->opt =  TRACE_OPT(NULL);
  self->visibility_map = (hash_ptr)(NULL);
  self->anonymizer = NULL;
}

void trace_plugin_deinit(TracePlugin_ptr self)
{
  FREE(self->desc);
  object_deinit(OBJECT(self));
}

/*---------------------------------------------------------------------------*/
/* Static functions                                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief Finalize method of plugin class.

  Pure virtual function. This must be refined by derived classes.
*/
static void trace_plugin_finalize(Object_ptr object, void* dummy)
{
  TracePlugin_ptr self = TRACE_PLUGIN(object);

  trace_plugin_deinit(self);
  error_unreachable_code();
}

boolean trace_plugin_is_visible_symbol(TracePlugin_ptr self, node_ptr symb)
{
  boolean res;
  SymbTable_ptr st = trace_get_symb_table(self->trace);
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  TraceMgr_ptr gtm = TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
  node_ptr lookup;

  /* private */
  const int SYMB_VISIBLE   = 1;
  const int SYMB_INVISIBLE = -1;

  nusmv_assert(Nil != symb);
  nusmv_assert((hash_ptr)(NULL) != self->visibility_map);

  lookup = find_assoc(self->visibility_map, symb);
  if (Nil != lookup) { return (SYMB_VISIBLE == NODE_TO_INT(lookup)); }

  res = TraceMgr_is_visible_symbol(gtm, symb);

  insert_assoc(self->visibility_map, symb,
               NODE_FROM_INT((res) ? SYMB_VISIBLE : SYMB_INVISIBLE));

  return res;
}

/*!
  \brief 

  
*/
static void trace_plugin_prepare_action(TracePlugin_ptr self, Trace_ptr trace,
                                        TraceOpt_ptr opt)
{
  /* 1. setup visibility map */
  nusmv_assert((hash_ptr)(NULL) == self->visibility_map);
  self->visibility_map = new_assoc();

  /* 2. register trace */
  nusmv_assert(TRACE(NULL) == self->trace);
  self->trace = trace;

  /* 3. create obfuscation map if required */
  nusmv_assert(NULL == self->anonymizer);
  self->anonymizer = TraceOpt_obfuscate(opt)
    ? NODE_ANONYMIZER_BASE(NodeAnonymizerST_create(EnvObject_get_environment(ENV_OBJECT(Trace_get_symb_table(trace))),
                                                   NULL, 1000, Trace_get_symb_table(trace)))
    : NULL;

  /* 4. register options */
  nusmv_assert(TRACE_OPT(NULL) == self->opt);
  self->opt = opt;
}

/*!
  \brief 

  
*/
static void trace_plugin_cleanup_action(TracePlugin_ptr self)
{
  /* 1. dispose visibility map */
  nusmv_assert((hash_ptr)(NULL) != self->visibility_map);
  free_assoc(self->visibility_map);
  self->visibility_map = (hash_ptr)(NULL);

  /* 2. unregister trace */
  self->trace = TRACE(NULL);

  /* 3. dispose obfuscation map (if any) */
  if (NULL != self->anonymizer) {
    NodeAnonymizerBase_destroy(self->anonymizer);
    self->anonymizer = NULL;
  }

  /* 4. unregisters options */
  self->opt = TRACE_OPT(NULL);
}


