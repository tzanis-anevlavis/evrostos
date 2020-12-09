/* ---------------------------------------------------------------------------


  This file is part of the ``trace.exec'' package of NuSMV version 2.
  Copyright (C) 2009 by FBK.

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
  \brief This module contains the functions needed to support trace
               re-execution

  This module contains the functions needed to support trace
               re-execution

*/

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/trace/pkg_traceInt.h"

#include "nusmv/core/prop/propPkg.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/bmc/bmc.h"
#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/exec/traceExec.h"
/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

int Trace_execute_trace(const NuSMVEnv_ptr env,
                        const Trace_ptr trace,
                        const CompleteTraceExecutor_ptr executor)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  boolean success = true;

  SexpFsm_ptr sexp_fsm =  \
      SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));

  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  SEXP_FSM_CHECK_INSTANCE(sexp_fsm);

  if (!Trace_is_complete(trace, SexpFsm_get_vars_list(sexp_fsm), true)) {
        StreamMgr_print_error(streams,  "Error: cannot execute incomplete trace.\n");
        success = false;
  }

  else {
    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,
              "Executing trace of length %d\n", Trace_get_length(trace));
    }

    /* execute the trace using given executor. */
    success = CompleteTraceExecutor_execute(executor, trace, NIL(int));
  }

  return success ? 0 : 1;
}

int Trace_execute_partial_trace(const NuSMVEnv_ptr env,
                                const Trace_ptr trace,
                                const PartialTraceExecutor_ptr executor,
                                const NodeList_ptr language)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  boolean success = true;

  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  TRACE_CHECK_INSTANCE(trace);
  PARTIAL_TRACE_EXECUTOR_CHECK_INSTANCE(executor);
  NODE_LIST_CHECK_INSTANCE(language);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
            "Executing trace of length %d\n", Trace_get_length(trace));
  }

  { /* execute a partial trace using given executor, register complete
       trace upon succesful completion */
    Trace_ptr complete_trace = \
      PartialTraceExecutor_execute(executor, trace, language, NIL(int));

    if (TRACE(NULL) != complete_trace) {
      int trace_id = \
        TraceMgr_register_trace(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
                                                 complete_trace);
      StreamMgr_print_output(streams,
              "-- New complete trace is stored at %d index.\n",
              1 + trace_id);
    }
    else { success = false; }
  }

  return success ? 0 : 1;
}
