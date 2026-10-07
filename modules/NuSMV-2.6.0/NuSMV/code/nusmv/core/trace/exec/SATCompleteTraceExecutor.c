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
  \brief Implementation of class 'SATCompleteTraceExecutor'

  \todo: Missing description

*/

#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/trace/exec/PartialTraceExecutor.h"

#include "nusmv/core/trace/exec/SATCompleteTraceExecutor.h"
#include "nusmv/core/trace/exec/SATCompleteTraceExecutor_private.h"

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/opt/opt.h"

#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/Trace_private.h"
#include "nusmv/core/trace/pkg_trace.h"

#include "nusmv/core/bmc/bmc.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'SATCompleteTraceExecutor_private.h' for class 'SATCompleteTraceExecutor' definition. */

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

static void sat_complete_trace_executor_finalize(Object_ptr object,
                                                 void* dummy);

static boolean
sat_complete_trace_executor_execute(const CompleteTraceExecutor_ptr self,
                                    const Trace_ptr trace, int* n_steps);

static inline be_ptr
sat_complete_trace_executor_get_initial_state(BeFsm_ptr be_fsm);

static inline be_ptr
sat_complete_trace_executor_get_transition_relation(BeFsm_ptr be_fsm);

static inline void
bmc_add_be_into_solver_positively(SatSolver_ptr solver,
                                  SatSolverGroup group, be_ptr prob,
                                  BeEnc_ptr be_enc,
                                  Be_CnfAlgorithm alg);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

SATCompleteTraceExecutor_ptr
SATCompleteTraceExecutor_create(const BeFsm_ptr fsm, const BeEnc_ptr enc,
                                const BddEnc_ptr bdd_enc)
{
  SATCompleteTraceExecutor_ptr self = ALLOC(SATCompleteTraceExecutor, 1);
  SAT_COMPLETE_TRACE_EXECUTOR_CHECK_INSTANCE(self);

  sat_complete_trace_executor_init(self, fsm, enc, bdd_enc);
  return self;
}

void SATCompleteTraceExecutor_destroy(SATCompleteTraceExecutor_ptr self)
{
  SAT_COMPLETE_TRACE_EXECUTOR_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void sat_complete_trace_executor_init(SATCompleteTraceExecutor_ptr self,
                                      const BeFsm_ptr fsm, const BeEnc_ptr enc,
                                      const BddEnc_ptr bdd_enc)
{
  /* base class initialization */
  complete_trace_executor_init(COMPLETE_TRACE_EXECUTOR(self),
                               EnvObject_get_environment(ENV_OBJECT(enc)));

  /* members initialization */
  self->fsm = fsm;
  self->enc = enc;
  self->bdd_enc = bdd_enc;

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = sat_complete_trace_executor_finalize;

  OVERRIDE(CompleteTraceExecutor, execute) = sat_complete_trace_executor_execute;
}

void sat_complete_trace_executor_deinit(SATCompleteTraceExecutor_ptr self)
{
  /* members deinitialization */

  /* base class deinitialization */
  complete_trace_executor_deinit(COMPLETE_TRACE_EXECUTOR(self));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The SATCompleteTraceExecutor class virtual finalizer

  Called by the class destructor
*/
static void sat_complete_trace_executor_finalize(Object_ptr object, void* dummy)
{
  SATCompleteTraceExecutor_ptr self = SAT_COMPLETE_TRACE_EXECUTOR(object);

  sat_complete_trace_executor_deinit(self);
  FREE(self);
}

/*!
  \brief Executes a trace on the given fsm using SAT solver

  The trace is executed using SAT solver, that is a proof
  that the fsm is compatible with the trace is built (if such proof
  exists). Trace is assumed to be complete in order to perform
  execution. If a non complete trace is given, an error is raised.

  The number of performed steps (transitions) is returned. If the
  initial state is not compatible -1 is returned.
*/
static boolean
sat_complete_trace_executor_execute(const CompleteTraceExecutor_ptr complete_executor,
                                    const Trace_ptr trace, int* n_steps)
{
  /* local references to self */
  const SATCompleteTraceExecutor_ptr self = \
    SAT_COMPLETE_TRACE_EXECUTOR(complete_executor);

  const BaseTraceExecutor_ptr executor = \
    BASE_TRACE_EXECUTOR(complete_executor);

  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int count = -1; /* failure */
  TraceIter step;

  Be_Manager_ptr be_mgr = (Be_Manager_ptr)(NULL);

  SatIncSolver_ptr solver;
  SatSolverGroup satGroup;
  SatSolverResult satResult;

  be_ptr be_current;
  be_ptr be_problem;

  /* 0- Check prerequisites */
  solver  = Sat_CreateIncSolver(env, get_sat_solver(opts));
  SAT_INC_SOLVER_CHECK_INSTANCE(solver);

  SAT_COMPLETE_TRACE_EXECUTOR_CHECK_INSTANCE(self);
  TRACE_CHECK_INSTANCE(trace);

  BE_FSM_CHECK_INSTANCE(self->fsm);
  BE_ENC_CHECK_INSTANCE(self->enc);
  BDD_ENC_CHECK_INSTANCE(self->bdd_enc);

  /* check for trace completeness */
  if (!Trace_is_complete(trace, Trace_get_sf_vars(trace), false) ||
      !Trace_is_complete(trace, Trace_get_i_vars(trace), false)) {
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
    ErrorMgr_internal_error(errmgr, "%s:%d:%s: This executor does not support partial traces.",
                   __FILE__, __LINE__, __func__);
  }

  step = trace_first_iter(trace);
  nusmv_assert(TRACE_END_ITER != step);

  be_mgr = BeEnc_get_be_manager(self->enc);

  { /* 1- Check Start State */
    satGroup = SatIncSolver_create_group(solver);

    /* pick the initial state from the trace */
    be_current = \
      TraceUtils_fetch_as_be(trace, step, TRACE_ITER_SF_SYMBOLS,
                             self->enc, self->bdd_enc);

    be_problem = Be_And(be_mgr,  be_current,
                    sat_complete_trace_executor_get_initial_state(self->fsm));

    /* push the problem into the SAT solver */
    bmc_add_be_into_solver_positively(SAT_SOLVER(solver), satGroup,
                                      be_problem, self->enc,
                                      get_rbc2cnf_algorithm(opts));

    satResult = SatSolver_solve_all_groups(SAT_SOLVER(solver));
    SatIncSolver_destroy_group(solver, satGroup);
  }

  if (SAT_SOLVER_UNSATISFIABLE_PROBLEM == satResult) {
    fprintf(BaseTraceExecutor_get_error_stream(executor),
            "Error: starting state is not initial state\n");
  }
  else {
    boolean terminate = false;
    nusmv_assert(SAT_SOLVER_SATISFIABLE_PROBLEM == satResult);

    ++ count;

    /* in SAT/SMT approach it is possibile to factorize the trans. relation */
    bmc_add_be_into_solver_positively(SAT_SOLVER(solver),
              SatSolver_get_permanent_group(SAT_SOLVER(solver)),
              sat_complete_trace_executor_get_transition_relation(self->fsm),
                                      self->enc,
                                      get_rbc2cnf_algorithm(opts));

    /* 2- Check Consecutive States are related by transition relation */
    do {

      step = TraceIter_get_next(step);
      if (TRACE_END_ITER != step) {
        /* create problem and push into the SAT solver current state,
           next input, next transitional, next state.  SAT problem for
           complete trace re-execution (untimed) */
        be_ptr be_input = \
          TraceUtils_fetch_as_be(trace, step, TRACE_ITER_I_SYMBOLS,
                                 self->enc, self->bdd_enc);
        be_ptr be_comb = \
          TraceUtils_fetch_as_be(trace, step, TRACE_ITER_COMBINATORIAL,
                                 self->enc, self->bdd_enc);
        be_ptr be_next = \
          TraceUtils_fetch_as_be(trace, step, TRACE_ITER_SF_SYMBOLS,
                                 self->enc, self->bdd_enc);

        be_problem = \
          Be_And(be_mgr, be_current,
                 Be_And(be_mgr, be_comb,
                      Be_And(be_mgr, be_input,
                             BeEnc_shift_curr_to_next(self->enc, be_next))));

        if (BaseTraceExecutor_get_verbosity(executor)) {
          fprintf(BaseTraceExecutor_get_output_stream(executor),
                  "-- executing step %d ... ", 1 + count);
          fflush(BaseTraceExecutor_get_output_stream(executor));
        }

        satGroup = SatIncSolver_create_group(solver);
        bmc_add_be_into_solver_positively(SAT_SOLVER(solver), satGroup,
                                          be_problem, self->enc,
                                          get_rbc2cnf_algorithm(opts));

        satResult = SatSolver_solve_all_groups(SAT_SOLVER(solver));
        SatIncSolver_destroy_group(solver, satGroup);

        if (SAT_SOLVER_UNSATISFIABLE_PROBLEM == satResult) {
          if (0 < BaseTraceExecutor_get_verbosity(executor)) {
            fprintf(BaseTraceExecutor_get_output_stream(executor), "failed!\n");
          }
          terminate = true;
        }

        else {
          if (BaseTraceExecutor_get_verbosity(executor)) {
            fprintf(BaseTraceExecutor_get_output_stream(executor), "ok\n");
          }
          ++ count;

          nusmv_assert(SAT_SOLVER_SATISFIABLE_PROBLEM == satResult);
          be_current = be_next;
        }
      } else {
        if (0 == count) {
          fprintf(BaseTraceExecutor_get_error_stream(executor),
                  "Warning: trace has no transitions.\n");
        }
        terminate = true;
      }
    } while (!terminate); /* loop on state/input pairs */
  }

  /* release the SAT solver instance */
  SatIncSolver_destroy(solver);

  { /* as a last check, verify looback consistency using internal
       service, the trace is compatible iff exactly len(Trace) steps
       have been performed *and* loopback data is consistent. */
    boolean res = false;

    if (Trace_get_length(trace) == count) {
      if (complete_trace_executor_check_loopbacks(complete_executor, trace)) {
        fprintf(BaseTraceExecutor_get_output_stream(executor),
                "Trace %d execution completed successfully.\n"
                "%d steps performed.\n",
                Trace_get_id(trace),
                count );

        res = true;
      }
    }
    else {
      fprintf(BaseTraceExecutor_get_output_stream(executor),
              "Trace execution failed!\n"
              /* "(%d steps performed).\n", count */);
    }

    if (NIL(int) != n_steps) { *n_steps = count; }
    return res;
  }
}

/*!
  \brief Builds the initial state formula

  Private service of sat_complete_trace_executor_execute

  \se None
*/
static inline be_ptr
sat_complete_trace_executor_get_initial_state(BeFsm_ptr be_fsm)
{
  BeEnc_ptr be_enc = BeFsm_get_be_encoding(be_fsm);
  be_ptr init = Be_And(BeEnc_get_be_manager(be_enc),
                       BeFsm_get_init(be_fsm),
                       BeFsm_get_invar(be_fsm));
  return init;
}

/*!
  \brief Builds the transition relation formula

  Private service of sat_complete_trace_executor_execute

  \se None
*/

static inline be_ptr
sat_complete_trace_executor_get_transition_relation (BeFsm_ptr be_fsm)
{
  BeEnc_ptr be_enc = BeFsm_get_be_encoding(be_fsm);
  Be_Manager_ptr mgr = BeEnc_get_be_manager(be_enc);

  be_ptr invar = BeFsm_get_invar(be_fsm);
  be_ptr trans = BeFsm_get_trans(be_fsm);
  be_ptr n_invar = BeEnc_shift_curr_to_next(be_enc, invar);

  return Be_And(mgr, invar, Be_And(mgr, trans, n_invar));
}

/*!
  \brief Converts Be into CNF, and adds it into a group of a
  solver, sets polarity to 1, and then destroys the CNF.

  Private service of sat_complete_trace_executor_execute.

  \se Outputs into outstream the total time of conversion,
  adding, setting polarity and destroying BE.
*/
static inline void
bmc_add_be_into_solver_positively(SatSolver_ptr solver, SatSolverGroup group,
                                  be_ptr prob, BeEnc_ptr be_enc,
                                  Be_CnfAlgorithm cnf_alg)
{
  Be_Manager_ptr be_mgr = BeEnc_get_be_manager(be_enc);
  Be_Cnf_ptr cnf;

  /* We force inclusion of the conjunct set to guarantee soundness */
  cnf = Be_ConvertToCnf(be_mgr, Bmc_Utils_apply_inlining4inc(be_mgr, prob),
                        1, cnf_alg);
  SatSolver_add(solver, cnf, group);
  SatSolver_set_polarity(solver, cnf, 1, group);

  Be_Cnf_Delete(cnf);
}


/**AutomaticEnd***************************************************************/
