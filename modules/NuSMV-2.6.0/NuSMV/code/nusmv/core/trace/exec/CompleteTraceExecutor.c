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
  \brief Implementation of class 'CompleteTraceExecutor'

  \todo: Missing description

*/


#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/trace/exec/CompleteTraceExecutor.h"
#include "nusmv/core/trace/exec/CompleteTraceExecutor_private.h"

#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/Trace_private.h"
#include "nusmv/core/utils/utils.h"

#include "nusmv/core/opt/opt.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'CompleteTraceExecutor_private.h' for class 'CompleteTraceExecutor' definition. */

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

static void complete_trace_executor_finalize(Object_ptr object,
                                             void* dummy);

static boolean
complete_trace_executor_execute(const CompleteTraceExecutor_ptr self,
                                const Trace_ptr trace, int* n_steps);
/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

boolean CompleteTraceExecutor_execute (const CompleteTraceExecutor_ptr self,
                                       const Trace_ptr trace, int* n_steps)
{
  return (*self->execute)(self, trace, n_steps);
}

CompleteTraceExecutor_ptr CompleteTraceExecutor_create(const NuSMVEnv_ptr env)
{
  CompleteTraceExecutor_ptr self = ALLOC(CompleteTraceExecutor, 1);
  COMPLETE_TRACE_EXECUTOR_CHECK_INSTANCE(self);

  complete_trace_executor_init(self, env);
  return self;
}

void CompleteTraceExecutor_destroy(CompleteTraceExecutor_ptr self)
{
  COMPLETE_TRACE_EXECUTOR_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void complete_trace_executor_init(CompleteTraceExecutor_ptr self,
                                  const NuSMVEnv_ptr env)
{
  /* base class initialization */
  trace_executor_init(BASE_TRACE_EXECUTOR(self), env);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = complete_trace_executor_finalize;

  /* virtual abstract complete execution method */
  OVERRIDE(CompleteTraceExecutor, execute) = complete_trace_executor_execute;
}

void complete_trace_executor_deinit(CompleteTraceExecutor_ptr self)
{
  /* members deinitialization */


  /* base class deinitialization */
  trace_executor_deinit(BASE_TRACE_EXECUTOR(self));
}

boolean
complete_trace_executor_check_loopbacks (const CompleteTraceExecutor_ptr self,
                                         const Trace_ptr trace)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(Trace_get_symb_table(trace)));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  boolean res = true; /* no error */
  TraceIter step;

  int i = 1;

  /* local reference to superclass */
  BaseTraceExecutor_ptr executor = BASE_TRACE_EXECUTOR(self);

  COMPLETE_TRACE_EXECUTOR_CHECK_INSTANCE(self);
  TRACE_CHECK_INSTANCE(trace);

  /* as a last check it is necessary to make sure loopback information
     is consistent (if trace is thawed this check is trivially
     true) */
  if (opt_verbose_level_ge(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
            "now checking loopbacks...\n");
  }
  TRACE_FOREACH(trace, step) {
    if (trace_step_is_loopback(trace, step) &&
        !trace_step_test_loopback(trace, step)) {
      fprintf(BaseTraceExecutor_get_error_stream(executor), "*** Error ***\n"
              "Inconsistent loopback information found at step %d.\n", i);
      res = false;
      break; /*  continuing is pointless */
    }

    ++ i;
  } /* trace foreach */

  return res;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The CompleteTraceExecutor class virtual finalizer

  Called by the class destructor
*/
static void complete_trace_executor_finalize(Object_ptr object, void* dummy)
{
  CompleteTraceExecutor_ptr self = COMPLETE_TRACE_EXECUTOR(object);

  complete_trace_executor_deinit(self);
  FREE(self);
}

/*!
  \brief Abstract execution method for complete trace executors

  This is a pure virtual functions. Every derived class
  must overwrite this function. Raises an assertion if invoked
*/
static boolean
complete_trace_executor_execute(const CompleteTraceExecutor_ptr self,
                                const Trace_ptr trace, int* n_steps)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
  return 0;
}


/**AutomaticEnd***************************************************************/

