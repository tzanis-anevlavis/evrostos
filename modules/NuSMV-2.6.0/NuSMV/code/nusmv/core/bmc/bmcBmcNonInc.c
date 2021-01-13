/* ---------------------------------------------------------------------------

 This file is part of the ``bmc'' package of NuSMV
   version 2.  Copyright (C) 2004 by FBK-irst.

   NuSMV version 2 is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.

   NuSMV version 2 is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA.

   For more information on NuSMV see <http://nusmv.fbk.eu> or
   email to <nusmv-users@fbk.eu>.  Please report bugs to
   <nusmv-users@fbk.eu>.

   To contact the NuSMV development board, email to
   <nusmv@fbk.eu>.

-----------------------------------------------------------------------------*/

/*!
  \author Roberto Cavada
  \brief High level functionalities layer for non incremental sat
   solving

  \todo: Missing description

*/


#include "nusmv/core/be/be.h"
#include "nusmv/core/bmc/bmcBmc.h"
#include "nusmv/core/bmc/bmcConv.h"
#include "nusmv/core/bmc/bmcDump.h"
#include "nusmv/core/bmc/bmcGen.h"
#include "nusmv/core/bmc/bmcInt.h"
#include "nusmv/core/bmc/bmcModel.h"
#include "nusmv/core/bmc/bmcUtils.h"
#include "nusmv/core/dag/dag.h"
#include "nusmv/core/enc/be/BeEnc.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/mc/mc.h" /* for print_spec */
#include "nusmv/core/node/node.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/prop/propProp.h"
#include "nusmv/core/sat/SatIncSolver.h"
#include "nusmv/core/sat/SatSolver.h"
#include "nusmv/core/sat/sat.h" /* for solver and result */
#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/TraceMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/wff/w2w/w2w.h"
#include "nusmv/core/wff/wff.h"


#ifdef BENCHMARKING
#include <time.h>
clock_t start_time;
#endif
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BMC_REWRITE_INVARSPEC_LAYER_NAME "bmc_invarspec_rewrite_layer"

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

int Bmc_GenSolveLtl(NuSMVEnv_ptr env,
                    Prop_ptr ltlprop,
                    const int k, const int relative_loop,
                    const boolean must_inc_length,
                    const boolean must_solve,
                    const Bmc_DumpType dump_type,
                    const char* dump_fname_template)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  const Be_CnfAlgorithm cnf_alg = get_rbc2cnf_algorithm(opts);

  BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  node_ptr bltlspec;  /* Its booleanization */
  BeFsm_ptr be_fsm; /* The corresponding be fsm */
  BeEnc_ptr be_enc;
  Be_Manager_ptr be_mgr;

  Prop_ptr inputprop = NULL;
  Prop_Rewriter_ptr rewriter = NULL;

  /* ---------------------------------------------------------------------- */
  /* Here a property was selected                                           */
  /* ---------------------------------------------------------------------- */
  int k_max = k;
  int k_min = 0;
  int increasingK;
  boolean found_solution;

  /* checks that a property was selected: */
  nusmv_assert(ltlprop != PROP(NULL));

  if (Prop_get_status(ltlprop) != Prop_Unchecked) {
    return 0;
  }

  found_solution = false;
  if (!must_inc_length) k_min = k_max;


  if (opt_cone_of_influence(opts) == true) {
    Prop_apply_coi_for_bmc(env, ltlprop);
  }

  be_fsm = Prop_get_be_fsm(ltlprop);
  if (be_fsm == (BeFsm_ptr) NULL) {
    Prop_set_environment_fsms(env, ltlprop);
    be_fsm = Prop_get_be_fsm(ltlprop);
    nusmv_assert(be_fsm != (BeFsm_ptr) NULL);
  }

  rewriter = Prop_Rewriter_create(env, ltlprop,
                                  WFF_REWRITE_METHOD_DEADLOCK_FREE,
                                  WFF_REWRITER_REWRITE_INPUT_NEXT,
                                  FSM_TYPE_BE, bdd_enc);
  inputprop = ltlprop;
  ltlprop = Prop_Rewriter_rewrite(rewriter);
  be_fsm = Prop_get_be_fsm(ltlprop);

  /* booleanized, negated and NNFed formula: */
  bltlspec
    = Wff2Nnf(env, Wff_make_not(nodemgr, Compile_detexpr2bexpr(bdd_enc,
                                                               Prop_get_expr_core(ltlprop))));

  be_enc = BeFsm_get_be_encoding(be_fsm);
  be_mgr = BeEnc_get_be_manager(be_enc);

  /* Start problems generations: */
  for (increasingK = k_min; (increasingK <= k_max) && ! found_solution;
       ++increasingK) {
    int l;
    char szLoop[16]; /* to keep loopback string */
    be_ptr prob; /* The problem in BE format */
    Be_Cnf_ptr cnf; /* The CNFed be problem */

    /* the loopback value could be depending on the length
       if it were relative: */
    l = Bmc_Utils_RelLoop2AbsLoop(relative_loop, increasingK);

    /* this is for verbose messages */
    Bmc_Utils_ConvertLoopFromInteger(relative_loop, szLoop, sizeof(szLoop));

    /* prints a verbose message: */
    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      if (Bmc_Utils_IsNoLoopback(l)) {
        Logger_log(logger,
                "\nGenerating problem with bound %d, no loopback...\n",
                increasingK);
      }
      else if (Bmc_Utils_IsAllLoopbacks(l)) {
        Logger_log(logger,
                "\nGenerating problem with bound %d, all possible loopbacks...\n",
                increasingK);
      }
      else {
        /* l can be negative iff loopback from the user pov is < -length */
        if ((l < increasingK) && (l >= 0)) {
          Logger_log(logger,
                  "\nGenerating problem with bound %d, loopback %s...\n",
                  increasingK, szLoop);
        }
      }
    } /* verbose message */

    /* checks for loopback vs k compatibility */
    if (Bmc_Utils_IsSingleLoopback(l) && ((l >= increasingK) || (l < 0))) {
      StreamMgr_print_error(streams,
              "\nWarning: problem with bound %d and loopback %s is not allowed: skipped\n",
              increasingK, szLoop);
      continue;
    }

    /* generates the problem: */
#ifdef BENCHMARKING
    StreamMgr_print_output(streams, ":START:benchmarking Generation\n");
    start_time = clock();
#endif

    prob = Bmc_Gen_LtlProblem(be_fsm, bltlspec, increasingK, l);
    prob = Bmc_Utils_apply_inlining(be_mgr, prob); /* inline if needed */

#ifdef BENCHMARKING
    StreamMgr_print_output(streams, ":UTIME = %.4f secs.\n",
            ((double)(clock()-start_time))/CLOCKS_PER_SEC);
    StreamMgr_print_output(streams, ":STOP:benchmarking Generation\n");
#endif

    /* Problem is cnf-ed */
    cnf = (Be_Cnf_ptr) NULL;

    /* Problem dumping: */
    if (dump_type != BMC_DUMP_NONE) {
      cnf = Be_ConvertToCnf(be_mgr, prob, 1, cnf_alg);
      Bmc_Dump_WriteProblem(be_enc, cnf, ltlprop, increasingK, l,
                            dump_type, dump_fname_template);
    }

    /* SAT problem solving */
    if (must_solve) {
      SatSolver_ptr solver;
      SatSolverResult sat_res;

      /* Sat construction */
      solver = Sat_CreateNonIncSolver(env, get_sat_solver(opts));
      if (solver == SAT_SOLVER(NULL)) {
        StreamMgr_print_error(streams,
                "Non-incremental sat solver '%s' is not available.\n",
                get_sat_solver(opts));

        if (cnf != (Be_Cnf_ptr) NULL) Be_Cnf_Delete(cnf);
        return 1;
      }

      /* Cnf construction (if needed): */
      if (cnf == (Be_Cnf_ptr) NULL) {
        cnf = Be_ConvertToCnf(be_mgr, prob, 1, cnf_alg);
      }

#ifdef BENCHMARKING
      StreamMgr_print_output(streams,  ":START:benchmarking Solving\n");
      start_time = clock();
#endif

      /* SAT invokation */
      SatSolver_add(solver, cnf, SatSolver_get_permanent_group(solver));
      SatSolver_set_polarity(solver, cnf, 1,
                             SatSolver_get_permanent_group(solver));
      sat_res = SatSolver_solve_all_groups(solver);

#ifdef BENCHMARKING
      StreamMgr_print_output(streams,  ":UTIME = %.4f secs.\n",
              ((double)(clock()-start_time))/CLOCKS_PER_SEC);
      StreamMgr_print_output(streams,  ":STOP:benchmarking Solving\n");
#endif

      /* Processes the result: */
      switch (sat_res) {

      case SAT_SOLVER_UNSATISFIABLE_PROBLEM:
        {
          char szLoopMsg[16]; /* for loopback part of message */
          memset(szLoopMsg, 0, sizeof(szLoopMsg));

          if (Bmc_Utils_IsAllLoopbacks(l)) {
            strncpy(szLoopMsg, "", sizeof(szLoopMsg)-1);
          }
          else if (Bmc_Utils_IsNoLoopback(l)) {
            strncpy(szLoopMsg, " and no loop", sizeof(szLoopMsg)-1);
          }
          else {
            /* loop is Natural: */
            strncpy(szLoopMsg, " and loop at ", sizeof(szLoopMsg)-1);
            strncat(szLoopMsg, szLoop, sizeof(szLoopMsg)-1-strlen(szLoopMsg));
          }

          StreamMgr_print_output(streams,
                  "-- no counterexample found with bound %d%s",
                  increasingK, szLoopMsg);
          if (opt_verbose_level_gt(opts, 2)) {
            Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
            Logger_log(logger, " for ");
            print_spec(Logger_get_ostream(logger),
                       inputprop, get_prop_print_method(opts));
          }
          StreamMgr_print_output(streams,  "\n");

          break;
        }

      case SAT_SOLVER_SATISFIABLE_PROBLEM:
        StreamMgr_print_output(streams,  "-- ");
        print_spec(StreamMgr_get_output_ostream(streams),
                   inputprop, get_prop_print_method(opts));
        StreamMgr_print_output(streams,  "  is false\n");
        Prop_set_status(ltlprop, Prop_False);

        found_solution = true;

        if (opt_counter_examples(opts)) {
          TraceMgr_ptr tm = TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
          BoolSexpFsm_ptr bsexp_fsm; /* needed for trace language */
          Trace_ptr trace;

          bsexp_fsm = Prop_get_bool_sexp_fsm(ltlprop);
          if (BOOL_SEXP_FSM(NULL) == bsexp_fsm) {
            bsexp_fsm = \
              BOOL_SEXP_FSM(NuSMVEnv_get_value(env, ENV_BOOL_FSM));
            BOOL_SEXP_FSM_CHECK_INSTANCE(bsexp_fsm);
          }

          trace = \
            Bmc_Utils_generate_and_print_cntexample(be_enc,
                                                    tm,
                                                    solver,
                                                    prob, increasingK,
                                                    "BMC Counterexample",
                             SexpFsm_get_symbols_list(SEXP_FSM(bsexp_fsm)));

          Prop_set_trace(ltlprop, Trace_get_id(trace));
        }

        break;

      case SAT_SOLVER_INTERNAL_ERROR:
        ErrorMgr_internal_error(errmgr, "Sorry, solver answered with a fatal Internal "
                       "Failure during problem solving.\n");

      case SAT_SOLVER_TIMEOUT:
      case SAT_SOLVER_MEMOUT:
        ErrorMgr_internal_error(errmgr, "Sorry, solver ran out of resources and aborted "
                       "the execution.\n");

      default:
        ErrorMgr_internal_error(errmgr, "Bmc_GenSolveLtl: Unexpected value in sat result");

      } /* switch */

      SatSolver_destroy(solver);
    } /* must solve */

    if (cnf != (Be_Cnf_ptr) NULL) {
      Be_Cnf_Delete(cnf);
      cnf = (Be_Cnf_ptr) NULL;
    }

  } /* for all problems length */

  Prop_Rewriter_update_original_property(rewriter);
  Prop_Rewriter_destroy(rewriter); rewriter = NULL;

  return 0;
}

int Bmc_GenSolveInvar(NuSMVEnv_ptr env,
                      Prop_ptr invarprop,
                      const boolean must_solve,
                      const Bmc_DumpType dump_type,
                      const char* dump_fname_template)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  const Be_CnfAlgorithm cnf_alg = get_rbc2cnf_algorithm(opts);

  BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  node_ptr binvarspec;  /* Its booleanization */
  BeFsm_ptr be_fsm; /* The corresponding be fsm */
  BeEnc_ptr be_enc;
  Be_Manager_ptr be_mgr;
  Trace_ptr trace;
  Bmc_result result;

  Prop_ptr oldprop = invarprop;
  Prop_Rewriter_ptr rewriter = NULL;

  /* checks that a property was selected: */
  nusmv_assert(invarprop != PROP(NULL));

  /* checks if it has already been checked: */
  if (Prop_get_status(invarprop) != Prop_Unchecked) {
    /* aborts this check */
    return 0;
  }

  if (opt_cone_of_influence(opts) == true) {
    Prop_apply_coi_for_bmc(env, invarprop);
  }

  be_fsm = Prop_get_be_fsm(invarprop);
  if (be_fsm == (BeFsm_ptr) NULL) {
    Prop_set_environment_fsms(env, invarprop);
    be_fsm = Prop_get_be_fsm(invarprop);
    nusmv_assert(be_fsm != (BeFsm_ptr) NULL);
  }

  rewriter = Prop_Rewriter_create(env, invarprop,
                                  WFF_REWRITE_METHOD_DEADLOCK_FREE,
                                  WFF_REWRITER_REWRITE_INPUT_NEXT,
                                  FSM_TYPE_BE, bdd_enc);
  invarprop = Prop_Rewriter_rewrite(rewriter);
  be_fsm = Prop_get_be_fsm(invarprop);

  /* booleanized, negated and NNFed formula: */
  binvarspec =
    Wff2Nnf(env, Compile_detexpr2bexpr(bdd_enc,
                                        Prop_get_expr_core(invarprop)));

  be_enc = BeFsm_get_be_encoding(be_fsm);
  be_mgr = BeEnc_get_be_manager(be_enc);

  /* generates the problem: */
  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "\nGenerating invariant problem\n");
  }

  /* Problem dumping: */
  if (dump_type != BMC_DUMP_NONE) {
    be_ptr prob;
    Be_Cnf_ptr cnf;

    prob = Bmc_Gen_InvarProblem(be_fsm, binvarspec);
    prob = Bmc_Utils_apply_inlining(be_mgr, prob);

    cnf = Be_ConvertToCnf(be_mgr, prob, 0, cnf_alg);
    Bmc_Dump_WriteProblem(be_enc, cnf, invarprop,
                          1, Bmc_Utils_GetNoLoopback(),
                          dump_type, dump_fname_template);

    Be_Cnf_Delete(cnf);
  }

  if (must_solve) {
    {
      BoolSexpFsm_ptr bsexp_fsm; /* needed for trace language */

      bsexp_fsm = Prop_get_bool_sexp_fsm(invarprop);
      if (BOOL_SEXP_FSM(NULL) == bsexp_fsm) {
        bsexp_fsm = \
          BOOL_SEXP_FSM(NuSMVEnv_get_value(env, ENV_BOOL_FSM));
        BOOL_SEXP_FSM_CHECK_INSTANCE(bsexp_fsm);
      }

      result = \
        Bmc_induction_algorithm(env, be_fsm, binvarspec, &trace,
                                SexpFsm_get_symbols_list(SEXP_FSM(bsexp_fsm)));
    }

    if (result == BMC_TRUE) {
      StreamMgr_print_output(streams,  "-- ");
      print_invar(StreamMgr_get_output_ostream(streams),
                  oldprop, get_prop_print_method(opts));
      StreamMgr_print_output(streams,  "  is true\n");
      Prop_set_status(invarprop, Prop_True);
    }
    else if(result == BMC_UNKNOWN) {
      StreamMgr_print_output(streams,  "-- cannot prove the ");
      print_invar(StreamMgr_get_output_ostream(streams),
                  oldprop, get_prop_print_method(opts));
      StreamMgr_print_output(streams,  " is true or false : the induction fails\n");

      if (opt_counter_examples(opts)) {

        /* Print the trace using default plugin */
        StreamMgr_print_output(streams,
                "-- as demonstrated by the following execution sequence\n");

        TraceMgr_register_trace(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), trace);
        TraceMgr_execute_plugin(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), TRACE_OPT(NULL),
                                    TRACE_MGR_DEFAULT_PLUGIN,
                                    TRACE_MGR_LAST_TRACE);

        Prop_set_trace(invarprop, Trace_get_id(trace));

      }
    }
    else {
      /* no other handled cases */
      error_unreachable_code();
    }
  } /* must solve */

  Prop_Rewriter_update_original_property(rewriter);
  Prop_Rewriter_destroy(rewriter); rewriter = NULL;

  return 0;
}

Bmc_result Bmc_induction_algorithm(const NuSMVEnv_ptr env,
                                   BeFsm_ptr be_fsm,
                                   node_ptr binvarspec,
                                   Trace_ptr* trace,
                                   NodeList_ptr symbols)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  const Be_CnfAlgorithm cnf_alg = get_rbc2cnf_algorithm(opts);

  /* SAT problem solving */
  SatSolver_ptr solver;
  SatSolverResult sat_res;
  Be_Cnf_ptr cnf;
  be_ptr prob;
  int result;
  Be_Manager_ptr be_mgr;
  BeEnc_ptr be_enc;

  be_enc = BeFsm_get_be_encoding(be_fsm);
  be_mgr = BeEnc_get_be_manager(be_enc);

  prob = Bmc_Gen_InvarProblem(be_fsm, binvarspec);
  prob = Bmc_Utils_apply_inlining(be_mgr, prob);

  cnf = (Be_Cnf_ptr) NULL;

  /* Sat construction */
  solver = Sat_CreateNonIncSolver(env, get_sat_solver(opts));
  if (solver == SAT_SOLVER(NULL)) {
    StreamMgr_print_error(streams,
            "Non-incremental sat solver '%s' is not available.\n",
            get_sat_solver(opts));

    if (cnf != (Be_Cnf_ptr) NULL) Be_Cnf_Delete(cnf);
    return 1;
  }

  /* Cnf construction (if needed): */
  if (cnf == (Be_Cnf_ptr) NULL) {
    cnf = Be_ConvertToCnf(be_mgr, prob, 1, cnf_alg);
  }

  /* SAT invokation */
  SatSolver_add(solver, cnf, SatSolver_get_permanent_group(solver));
  SatSolver_set_polarity(solver, cnf, 1,
                         SatSolver_get_permanent_group(solver));
  sat_res = SatSolver_solve_all_groups(solver);

  result = BMC_ERROR;

  /* Processes the result: */
  switch (sat_res) {

  case SAT_SOLVER_UNSATISFIABLE_PROBLEM:
    result = BMC_TRUE;
    break;

  case SAT_SOLVER_SATISFIABLE_PROBLEM:

    if (opt_counter_examples(opts)) {
      be_enc = BeFsm_get_be_encoding(be_fsm);
      *trace = Bmc_Utils_generate_cntexample(be_enc,
                                             solver,
                                             prob, 1,
                                             "BMC Failed Induction",
                                             symbols);
    }
    result = BMC_UNKNOWN;
    break;

  case SAT_SOLVER_INTERNAL_ERROR:
    ErrorMgr_internal_error(errmgr, "Sorry, solver answered with a fatal Internal "
                   "Failure during problem solving.\n");

  case SAT_SOLVER_TIMEOUT:
  case SAT_SOLVER_MEMOUT:
    ErrorMgr_internal_error(errmgr, "Sorry, solver ran out of resources and aborted "
                   "the execution.\n");

  default:
    ErrorMgr_internal_error(errmgr, "Bmc_GenSolveLtl: Unexpected value in sat result");

  } /* switch */

  SatSolver_destroy(solver);
  if (cnf != (Be_Cnf_ptr) NULL) Be_Cnf_Delete(cnf);

  return result;
}

int Bmc_GenSolveInvar_EenSorensson(NuSMVEnv_ptr env,
                                   Prop_ptr invarprop,
                                   const int max_k,
                                   const Bmc_DumpType dump_type,
                                   const char* dump_fname_template,
                                   boolean use_extra_step)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  node_ptr binvarspec;  /* Its booleanization */
  BeFsm_ptr be_fsm; /* The corresponding be fsm */

  Prop_ptr oldprop = invarprop;
  Prop_Rewriter_ptr rewriter = NULL;
  Bmc_result result;
  Trace_ptr trace;

  /* checks that a property was selected: */
  nusmv_assert(invarprop != PROP(NULL));

  if (Prop_get_status(invarprop) != Prop_Unchecked) {
    return 0;
  }

  if (opt_cone_of_influence(opts)) {
    Prop_apply_coi_for_bmc(env, invarprop);
  }

  be_fsm = Prop_get_be_fsm(invarprop);
  if (be_fsm == (BeFsm_ptr) NULL) {
    Prop_set_environment_fsms(env, invarprop);
    be_fsm = Prop_get_be_fsm(invarprop);
    nusmv_assert(be_fsm != (BeFsm_ptr) NULL);
  }

  rewriter = Prop_Rewriter_create(env, invarprop,
                                  WFF_REWRITE_METHOD_DEADLOCK_FREE,
                                  WFF_REWRITER_REWRITE_INPUT_NEXT,
                                  FSM_TYPE_BE, bdd_enc);
  invarprop = Prop_Rewriter_rewrite(rewriter);
  be_fsm = Prop_get_be_fsm(invarprop);

  /* booleanized, negated and NNFed formula: */
  binvarspec = Wff2Nnf(env, Compile_detexpr2bexpr(bdd_enc,
                                                  Prop_get_expr_core(invarprop)));

  result = Bmc_een_sorensson_algorithm(env,
                                       be_fsm,
                                       Prop_get_bool_sexp_fsm(invarprop),
                                       binvarspec,
                                       max_k,
                                       dump_type,
                                       dump_fname_template,
                                       invarprop,
                                       oldprop,
                                       true,
                                       use_extra_step,
                                       &trace);

  switch (result) {
  case BMC_FALSE:
    StreamMgr_print_output(streams,  "-- ");
    print_invar(StreamMgr_get_output_ostream(streams),
                oldprop, get_prop_print_method(opts));
    StreamMgr_print_output(streams,  "  is false\n");
    Prop_set_status(invarprop, Prop_False);

    if (opt_counter_examples(opts)) {
      /* Print the trace using default plugin */
      StreamMgr_print_output(streams,
                             "-- as demonstrated by the following execution sequence\n");

      TraceMgr_register_trace(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), trace);
      TraceMgr_execute_plugin(TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)), TRACE_OPT(NULL),
                              TRACE_MGR_DEFAULT_PLUGIN,
                              TRACE_MGR_LAST_TRACE);

      Prop_set_trace(invarprop, Trace_get_id(trace));
    }
    break;

  case BMC_TRUE:
    StreamMgr_print_output(streams,  "-- ");
    print_invar(StreamMgr_get_output_ostream(streams),
                oldprop, get_prop_print_method(opts));
    StreamMgr_print_output(streams,  "  is true\n");
    Prop_set_status(invarprop, Prop_True);
    break;

  case BMC_UNKNOWN:
    StreamMgr_print_output(streams,  "-- cannot prove the ");
    print_invar(StreamMgr_get_output_ostream(streams),
                oldprop, get_prop_print_method(opts));
    StreamMgr_print_output(streams,  " is true or false.\n");
    break;

  default:
    error_unreachable_code();
  }

  Prop_Rewriter_update_original_property(rewriter);
  Prop_Rewriter_destroy(rewriter); rewriter = NULL;

  return 0;
}

Bmc_result
Bmc_een_sorensson_algorithm_without_dump(const NuSMVEnv_ptr env,
                                         BeFsm_ptr be_fsm,
                                         BoolSexpFsm_ptr bool_fsm,
                                         node_ptr binvarspec,
                                         int max_k,
                                         boolean use_extra_step,
                                         Trace_ptr* trace) {
  return Bmc_een_sorensson_algorithm(env,
                                     be_fsm,
                                     bool_fsm,
                                     binvarspec,
                                     max_k,
                                     BMC_DUMP_NONE,
                                     NULL,
                                     PROP(NULL),
                                     PROP(NULL),
                                     false,
                                     use_extra_step,
                                     trace);
}

Bmc_result Bmc_een_sorensson_algorithm(const NuSMVEnv_ptr env,
                                       BeFsm_ptr be_fsm,
                                       BoolSexpFsm_ptr bool_fsm,
                                       node_ptr binvarspec,
                                       int max_k,
                                       const Bmc_DumpType dump_type,
                                       const char* dump_fname_template,
                                       Prop_ptr pp,
                                       Prop_ptr oldprop,
                                       boolean print_steps,
                                       boolean use_extra_step,
                                       Trace_ptr* trace)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  const Be_CnfAlgorithm cnf_alg = get_rbc2cnf_algorithm(opts);

  BeEnc_ptr be_enc;
  Be_Manager_ptr be_mgr;
  Bmc_result result = BMC_ERROR;
  be_ptr be_invarspec;
  be_ptr be_init;
  boolean solved;

  char template_name[BMC_DUMP_FILENAME_MAXLEN];
  int k;
  lsList crnt_state_be_vars;

  be_enc = BeFsm_get_be_encoding(be_fsm);
  be_mgr = BeEnc_get_be_manager(be_enc);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "\nGenerating invariant problem (Een/Sorensson)\n");
  }

  be_invarspec = Bmc_Conv_Bexp2Be(be_enc, binvarspec);
  be_init = Bmc_Model_GetInit0(be_fsm);

  k = 0;
  solved = false;

  /* retrieves the list of bool variables needed to calculate the
     state uniqueness, taking into account of coi if enabled. */
  crnt_state_be_vars =
    Bmc_Utils_get_vars_list_for_uniqueness_fsm(be_enc, (SexpFsm_ptr) bool_fsm);

  while (!solved && (k <= max_k)) {
    be_ptr be_base;
    Be_Cnf_ptr cnf;
    int i;

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "\nBuilding the base for k=%d\n", k);
    }

    /* Get the unrolling (s_0,...,s_k)*/
    be_base = Bmc_Model_GetUnrolling(be_fsm, 0, k);
    /* Set the initial condition to hold in s_0 */
    be_base = Be_And(be_mgr, be_base, be_init);
    /* The invariant property should be true in all s_0...s_{k-1}*/
    for (i = 0; i < k; i++) {
      be_base = Be_And(be_mgr, be_base,
                       BeEnc_untimed_expr_to_timed(be_enc, be_invarspec, i));
    }

    /* The invariant property should be violated in s_k */
    be_base = Be_And(be_mgr, be_base,
                     Be_Not(be_mgr,
                            BeEnc_untimed_expr_to_timed(be_enc,
                                                        be_invarspec, k)));
    be_base = Bmc_Utils_apply_inlining(be_mgr, be_base);

    /* Problem is cnf-ed */
    cnf = (Be_Cnf_ptr) NULL;

    /* Problem dumping: */
    if (dump_type != BMC_DUMP_NONE) {
      cnf = Be_ConvertToCnf(be_mgr, be_base, 1, cnf_alg);

      strncpy(template_name, dump_fname_template, sizeof(template_name)-2);
      template_name[sizeof(template_name)-1] = '\0'; /* terms the string */
      strncat(template_name, "_base",
              sizeof(template_name) - strlen(template_name) - 1);
      template_name[sizeof(template_name)-1] = '\0'; /* terms the string */

      Bmc_Dump_WriteProblem(be_enc, cnf, pp,
                            1, Bmc_Utils_GetNoLoopback(),
                            dump_type, template_name);
    }

    /* SAT problem solving */
    {
      SatSolver_ptr solver;
      SatSolverResult sat_res;

      /* Sat construction */
      solver = Sat_CreateNonIncSolver(env, get_sat_solver(opts));
      if (solver == SAT_SOLVER(NULL)) {
        StreamMgr_print_error(streams,
                "Non-incremental sat solver '%s' is not available.\n",
                get_sat_solver(opts));

        if (cnf != (Be_Cnf_ptr) NULL) Be_Cnf_Delete(cnf);
        return BMC_ERROR;
      }

      /* Cnf construction (if needed): */
      if (cnf == (Be_Cnf_ptr) NULL) {
        cnf = Be_ConvertToCnf(be_mgr, be_base, 1, cnf_alg);
      }

      /* SAT invokation */
      SatSolver_add(solver, cnf, SatSolver_get_permanent_group(solver));
      SatSolver_set_polarity(solver, cnf, 1,
                             SatSolver_get_permanent_group(solver));
      sat_res = SatSolver_solve_all_groups(solver);

      /* Processes the result: */
      switch (sat_res) {

      case SAT_SOLVER_UNSATISFIABLE_PROBLEM:
        /* continue the loop */
        break;

      case SAT_SOLVER_SATISFIABLE_PROBLEM:
        result = BMC_FALSE;
        solved = true;

        if (opt_counter_examples(opts)) {

          *trace =                                              \
            Bmc_Utils_generate_cntexample(be_enc, solver,
                                          be_base, k, "BMC Counterexample",
                                  SexpFsm_get_symbols_list(SEXP_FSM(bool_fsm)));
        }
        break;

      case SAT_SOLVER_INTERNAL_ERROR:
        ErrorMgr_internal_error(errmgr, "Sorry, solver answered with a fatal Internal "
                       "Failure during problem solving.\n");

      case SAT_SOLVER_TIMEOUT:
      case SAT_SOLVER_MEMOUT:
        ErrorMgr_internal_error(errmgr, "Sorry, solver ran out of resources and aborted "
                       "the execution.\n");

      default:
        ErrorMgr_internal_error(errmgr, "Bmc_GenSolveLtl: Unexpected value in sat result");

      } /* switch */

      SatSolver_destroy(solver);
    } /* solving */

    /* base cnf no longer useful here */
    if (cnf != (Be_Cnf_ptr) NULL) {
      Be_Cnf_Delete(cnf);
      cnf = (Be_Cnf_ptr) NULL;
    }

    /* induction step */
    if (!solved) {
      /*
         steps[0]: P(S0) ^ ... ^ P(Si-1) ^ !P(Si) ^ path(0..i) ^
         loopFree(0..i)

         steps[1]: Init(S0) ^ !Init(S1) ^ ... ^ !Init(Si) ^ path(0..i)
         ^ loopFree(0..i)
      */
      be_ptr be_steps[3] = {(be_ptr)NULL, (be_ptr)NULL, (be_ptr)NULL};
      be_ptr be_unique;
      int j;

      if (opt_verbose_level_gt(opts, 0)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "\nBuilding the step for k=%d\n", k);
      }

      /* Get the unrolling (s_0,...,s_k)*/
      be_steps[0] = Bmc_Model_GetUnrolling(be_fsm, 0, k);

      if (use_extra_step) {
        /* The same for the extra step, plus the initial states @0 */
        be_steps[1] = Be_And(be_mgr, be_steps[0], be_init);
      }

      /* The invariant property should be true in all s_0...s_{k-1}*/
      for (i = 0; i < k; i++) {
        be_steps[0] = Be_And(be_mgr,
                             be_steps[0],
                             BeEnc_untimed_expr_to_timed(be_enc,
                                                         be_invarspec, i));

        if (use_extra_step) {
          be_steps[1] = Be_And(be_mgr, be_steps[1],
                               Be_Not(be_mgr,
                                      Bmc_Model_GetInitI(be_fsm, i + 1)));
        }
      }

      /* The invariant property should be violated in s_k */
      be_steps[0] = Be_And(be_mgr, be_steps[0],
                           Be_Not(be_mgr,
                                  BeEnc_untimed_expr_to_timed(be_enc,
                                                              be_invarspec,
                                                              k)));

      /* All states s_0,...,s_{k-1} should be different.
       * Insert and force to true s_j != s_i for each 0 <= j < i <= k-1
       * in frame 0 */
      be_unique = Be_Truth(be_mgr);
      for (i = 0; i < k ; i++) {
        for (j = 0; j < i; j++) {
          be_ptr not_equal = Be_Falsity(be_mgr);
          be_ptr be_var;
          lsGen gen;

          lsForEachItem(crnt_state_be_vars, gen, be_var) {
            be_ptr be_xor = Be_Xor(be_mgr,
                                   BeEnc_untimed_expr_to_timed(be_enc, be_var, i),
                                   BeEnc_untimed_expr_to_timed(be_enc, be_var, j));
            not_equal = Be_Or(be_mgr, not_equal, be_xor);
          }

          be_unique = Be_And(be_mgr, be_unique, not_equal);
        }
      } /* for i */
      be_steps[0] = Be_And(be_mgr, be_steps[0], be_unique);
      be_steps[0] = Bmc_Utils_apply_inlining(be_mgr, be_steps[0]);

      if (use_extra_step) {
        be_steps[1] = Be_And(be_mgr, be_steps[1], be_unique);
        be_steps[1] = Bmc_Utils_apply_inlining(be_mgr, be_steps[1]);
      }

      nusmv_assert(use_extra_step || (be_ptr)NULL == be_steps[1]);

      /* SAT problem solving */
      for (i = 0; ((be_ptr)NULL != be_steps[i]) && !solved; ++i) {
        SatSolver_ptr solver;
        SatSolverResult sat_res;

        /* Problem dumping: */
        if (dump_type != BMC_DUMP_NONE) {
          nusmv_assert((Be_Cnf_ptr)NULL == cnf);

          cnf = Be_ConvertToCnf(be_mgr, be_steps[i], 1, cnf_alg);

          strncpy(template_name, dump_fname_template, sizeof(template_name)-2);
          template_name[sizeof(template_name)-1] = '\0'; /* terms the string */
          strncat(template_name, "_step",
                  sizeof(template_name) - strlen(template_name) - 1);
          template_name[sizeof(template_name)-1] = '\0'; /* terms the string */

          Bmc_Dump_WriteProblem(be_enc, cnf, pp,
                                1, Bmc_Utils_GetNoLoopback(),
                                dump_type, template_name);
        }


        /* Sat construction */
        solver = Sat_CreateNonIncSolver(env, get_sat_solver(opts));
        if (solver == SAT_SOLVER(NULL)) {
          StreamMgr_print_error(streams,
                  "Non-incremental sat solver '%s' is not available.\n",
                  get_sat_solver(opts));

          if (cnf != (Be_Cnf_ptr) NULL) Be_Cnf_Delete(cnf);
          return BMC_ERROR;
        }

        /* Cnf construction (if needed): */
        if (cnf == (Be_Cnf_ptr) NULL) {
          cnf = Be_ConvertToCnf(be_mgr, be_steps[i], 1, cnf_alg);
        }

        /* SAT invokation */
        SatSolver_add(solver, cnf, SatSolver_get_permanent_group(solver));
        SatSolver_set_polarity(solver, cnf, 1,
                               SatSolver_get_permanent_group(solver));
        sat_res = SatSolver_solve_all_groups(solver);

        /* Processes the result: */
        switch (sat_res) {

        case SAT_SOLVER_UNSATISFIABLE_PROBLEM:
          result = BMC_TRUE;
          solved = true;
          break;

        case SAT_SOLVER_SATISFIABLE_PROBLEM:
          if (print_steps) {
            /* Prints out the current state of solving, and continues
               the loop */
            StreamMgr_print_output(streams,
                    "-- no proof or counterexample found with bound %d", k);
            if ((PROP(NULL) != pp) && opt_verbose_level_gt(opts, 2)) {
              Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
              Logger_log(logger, " for ");
              print_invar(Logger_get_ostream(logger),
                          pp, get_prop_print_method(opts));
            }
            StreamMgr_print_output(streams,  "\n");
          }
          break;

        case SAT_SOLVER_INTERNAL_ERROR:
          ErrorMgr_internal_error(errmgr, "Sorry, solver answered with a fatal Internal "
                         "Failure during problem solving.\n");

        case SAT_SOLVER_TIMEOUT:
        case SAT_SOLVER_MEMOUT:
          ErrorMgr_internal_error(errmgr, "Sorry, solver ran out of resources and aborted "
                         "the execution.\n");

        default:
          ErrorMgr_internal_error(errmgr, "Bmc_GenSolveLtl: Unexpected value in sat result");

        } /* switch */

        if (cnf != (Be_Cnf_ptr) NULL) {
          Be_Cnf_Delete(cnf);
          cnf = (Be_Cnf_ptr) NULL;
        }

        SatSolver_destroy(solver);
      } /* solving */


        /* base cnf no longer useful here */
      nusmv_assert((Be_Cnf_ptr)NULL == cnf);

    } /* induction step */

    k = k + 1;
  } /* while !solved */

  lsDestroy(crnt_state_be_vars, NULL);

  if (!solved) {
    return BMC_UNKNOWN;
  }
  else {
    return result;
  }
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/


/**AutomaticEnd***************************************************************/
