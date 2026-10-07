/* ---------------------------------------------------------------------------


  This file is part of the ``trace.exec'' package of NuSMV version 2.
  Copyright (C) 2010 by FBK-irst.

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
  \author Marco Pensallorto
  \brief Implementation of class 'BaseTraceExecutor'

  \todo: Missing description

*/

#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include <stdio.h>

#include "nusmv/core/trace/exec/BaseTraceExecutor.h"
#include "nusmv/core/trace/exec/BaseTraceExecutor_private.h"

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/trace/pkg_traceInt.h"
#include "nusmv/core/trace/Trace.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'BaseTraceExecutor_private.h' for class 'BaseTraceExecutor' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void trace_executor_finalize(Object_ptr object, void* dummy);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

int
BaseTraceExecutor_get_verbosity (BaseTraceExecutor_ptr self)
{
  BASE_TRACE_EXECUTOR_CHECK_INSTANCE(self);
  return self->verbosity;
}

void
BaseTraceExecutor_set_verbosity (BaseTraceExecutor_ptr self, int verbosity)
{
  BASE_TRACE_EXECUTOR_CHECK_INSTANCE(self);
  self->verbosity = verbosity;
}

FILE*
BaseTraceExecutor_get_output_stream (BaseTraceExecutor_ptr self)
{
  BASE_TRACE_EXECUTOR_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const StreamMgr_ptr streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

    return (NIL(FILE) != self->output_stream ?
            self->output_stream :
            StreamMgr_get_output_stream(streams));
  }
}

void
BaseTraceExecutor_set_output_stream (BaseTraceExecutor_ptr self, FILE* output_stream)
{
  BASE_TRACE_EXECUTOR_CHECK_INSTANCE(self);
  self->output_stream = output_stream;
}

FILE*
BaseTraceExecutor_get_error_stream (BaseTraceExecutor_ptr self)
{
  BASE_TRACE_EXECUTOR_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const StreamMgr_ptr streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

    return (NIL(FILE) != self->error_stream ?
            self->error_stream :
            StreamMgr_get_error_stream(streams));
  }
}

void
BaseTraceExecutor_set_error_stream (BaseTraceExecutor_ptr self, FILE* error_stream)
{
  BASE_TRACE_EXECUTOR_CHECK_INSTANCE(self);
  self->error_stream = error_stream;
}

/*!
  \brief Virtual destructor for BaseTraceExecutor class


*/

VIRTUAL void
BaseTraceExecutor_destroy (BaseTraceExecutor_ptr self)
{
  BASE_TRACE_EXECUTOR_CHECK_INSTANCE(self);
  OBJECT(self)->finalize(OBJECT(self), NULL);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

boolean trace_executor_check_defines(const BaseTraceExecutor_ptr self,
                                     const Trace_ptr trace)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  boolean res = true; /* no mismatch */

  /* final check, analyze defines (mismatches are warnings) */
  if (opt_verbose_level_ge(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
            "checking defines...\n");
  }

  { /* check defines */
    TraceIter step; int i = 1;

    TRACE_FOREACH(trace, step) {
      NodeList_ptr failures = NodeList_create();

      if (!trace_step_check_defines(trace, step, failures)) {
        ListIter_ptr iter;

        fprintf(BaseTraceExecutor_get_error_stream(self), "*** Warning ***\n"
                "Inconsistencies detected while analyzing the trace "
                "(step %d)\n\n", i);

        res = false;

        NODE_LIST_FOREACH(failures, iter) {
          node_ptr failure = NodeList_get_elem_at(failures, iter);
          fprintf(BaseTraceExecutor_get_error_stream(self), "%s",
                  ErrorMgr_failure_get_msg(errmgr, failure));
          fprintf(BaseTraceExecutor_get_error_stream(self), "\n");
        } /* NODE_LIST_FOREACH */
      }

      NodeList_destroy(failures);

      ++ i;
    } /* TRACE_FOREACH */
  } /* check defines */

  return res;
}

void trace_executor_init(BaseTraceExecutor_ptr self, const NuSMVEnv_ptr env)
{
  /* base class initialization */
  env_object_init(ENV_OBJECT(self), env);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = trace_executor_finalize;

  self->verbosity = 0; /* default is quiet */

  self->output_stream = NIL(FILE); /* defaults to outstream */
  self->error_stream = NIL(FILE); /* defaults to errstream */
}

void trace_executor_deinit(BaseTraceExecutor_ptr self)
{
  /* base class deinitialization */
  env_object_deinit(ENV_OBJECT(self));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The BaseTraceExecutor class virtual finalizer

  Called by the class destructor
*/
static void trace_executor_finalize(Object_ptr object, void* dummy)
{
  BaseTraceExecutor_ptr self = BASE_TRACE_EXECUTOR(object);

  trace_executor_deinit(self);
  FREE(self);
}

/**AutomaticEnd***************************************************************/
