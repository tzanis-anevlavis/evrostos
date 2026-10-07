/* ---------------------------------------------------------------------------

 This file is part of the ``bmc.sbmc'' package of NuSMV version 2.
  Copyright (C) 2004 Timo Latvala <timo.latvala@tkk.fi>

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

  For more information of NuSMV see <http://nusmv.fbk.eu> or email
  to <nusmv-users@fbk.eu>.  Please report bugs to <nusmv-users@fbk.eu>.

  To contact the NuSMV development board, email to <nusmv@fbk.eu>.

-----------------------------------------------------------------------------*/

/*!
  \author Timo Latvala, Marco Roveri
  \brief High level functionalities for SBMC

  User-commands directly use function defined in this module.
  This is the highest level in the SBMC API architecture.

  For further information about this implementation see:
  T. Latvala, A. Biere, K. Heljanko, and T. Junttila. Simple is
  Better: Efficient Bounded Model Checking for Past LTL. In: R. Cousot
  (ed.), Verification, Model Checking, and Abstract Interpretation,
  6th International Conference VMCAI 2005, Paris, France, Volume 3385
  of LNCS, pp. 380-395, Springer, 2005.  Copyright (C)
  Springer-Verlag.


*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/bmc/sbmc/sbmcBmc.h"
#include "nusmv/core/bmc/sbmc/sbmcGen.h"
#include "nusmv/core/bmc/sbmc/sbmcUtils.h"

#include "nusmv/core/bmc/bmcInt.h"

#include "nusmv/core/bmc/bmcBmc.h"
#include "nusmv/core/bmc/bmcDump.h"
#include "nusmv/core/bmc/bmcModel.h"
#include "nusmv/core/wff/wff.h"
#include "nusmv/core/wff/w2w/w2w.h"
#include "nusmv/core/bmc/bmcConv.h"
#include "nusmv/core/bmc/bmcUtils.h"

#include "nusmv/core/node/node.h"
#include "nusmv/core/be/be.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/enc/be/BeEnc.h"

#include "nusmv/core/sat/sat.h" /* for solver and result */
#include "nusmv/core/sat/SatSolver.h"
#include "nusmv/core/sat/SatIncSolver.h"

#include "nusmv/core/mc/mc.h" /* for print_spec */

#include "nusmv/core/prop/propProp.h"

#include "nusmv/core/utils/error.h"


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


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/


static void
bmc_expandFilename(const OptsHandler_ptr opts,
                   const int k, const int l,
                   const int prop_idx,
                   const char* filename_to_be_expanded,
                   char* filename_expanded,
                   const size_t filename_expanded_maxlen);


/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

int Bmc_SBMCGenSolveLtl(NuSMVEnv_ptr env, Prop_ptr ltlprop,
                        const int k, const int relative_loop,
                        const boolean must_inc_length,
                        const boolean must_solve,
                        const Bmc_DumpType dump_type,
                        const char* dump_fname_template)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  node_ptr bltlspec;  /* Its booleanization */
  BeFsm_ptr be_fsm = BE_FSM(NULL); /* The corresponding be fsm  */
  BeEnc_ptr be_enc;
  Be_Manager_ptr be_mgr;
  const Be_CnfAlgorithm cnf_alg = get_rbc2cnf_algorithm(opts);

  /* ----------------------------------------------------------------------*/
  /* Here a property was selected                                          */
  /* ----------------------------------------------------------------------*/
  int k_max = k;
  int k_min = 0;
  int increasingK;
  int found_solution;

  Prop_ptr inputprop = ltlprop;
  Prop_Rewriter_ptr rewriter = NULL;

  /* checks that a property was selected: */
  nusmv_assert(ltlprop != (Prop_ptr)NULL);

  if (Prop_get_status(ltlprop) != Prop_Unchecked) {
    return 0;
  }

  found_solution = false;

  if (must_inc_length == false) k_min = k_max;

  be_fsm = Prop_compute_ground_be_fsm(env, ltlprop);
  BE_FSM_CHECK_INSTANCE(be_fsm);

  rewriter = Prop_Rewriter_create(env, ltlprop,
                                  WFF_REWRITE_METHOD_DEADLOCK_FREE,
                                  WFF_REWRITER_REWRITE_INPUT_NEXT,
                                  FSM_TYPE_BE, bdd_enc);
  ltlprop = Prop_Rewriter_rewrite(rewriter);
  be_fsm = Prop_get_be_fsm(ltlprop);

  be_enc = BeFsm_get_be_encoding(be_fsm);
  be_mgr = BeEnc_get_be_manager(be_enc);

  sbmc_add_loop_variable(bdd_enc, be_fsm);

  /* Start problems generations: */
  for (increasingK = k_min; (increasingK <= k_max) && !found_solution;
      ++increasingK) {
    int l;
    char szLoop[16]; /* to keep loopback string */
    be_ptr prob; /* The problem in BE format */
    Be_Cnf_ptr cnf; /* The CNFed be problem */

    /* the loopback value could be depending on the length
       if it were relative: */
    l = Bmc_Utils_RelLoop2AbsLoop(relative_loop, increasingK);

    /* prints a verbose message: */
    Bmc_Utils_ConvertLoopFromInteger(relative_loop, szLoop, sizeof(szLoop));

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
        if ((l < increasingK) && (l>=0)) {
          Logger_log(logger,
                  "\nGenerating problem with bound %d, loopback %s...\n",
                  increasingK, szLoop);
        }
      }
    } /* verbose messages */

    /* checks for loopback vs k compatibility */
    if (Bmc_Utils_IsSingleLoopback(l) && ((l >= increasingK) || (l<0))) {
      StreamMgr_print_error(streams,
              "\nWarning: problem with bound %d and loopback %s is not allowed: skipped\n",
              increasingK, szLoop);
      continue;
    }

#ifdef BENCHMARKING
    StreamMgr_print_error(streams, ":START:benchmarking bound\n");
    StreamMgr_print_error(streams, ":bound %d\n", increasingK);
    StreamMgr_print_error(streams, ":STOP:benchmarking bound\n");
    StreamMgr_print_output(streams, ":START:benchmarking Generation\n");
    start_time = clock();
#endif

    /* generates the problem: */
    bltlspec = sbmc_make_boolean_formula(bdd_enc, ltlprop);
    prob = Bmc_Gen_SBMCProblem(be_fsm, bltlspec, increasingK, l);
    prob = Bmc_Utils_apply_inlining(be_mgr, prob);

#ifdef BENCHMARKING
    StreamMgr_print_output(streams, ":UTIME = %.4f secs.\n",((double)(clock()-start_time))/CLOCKS_PER_SEC);
    StreamMgr_print_output(streams, ":STOP:benchmarking Generation\n");
#endif

    cnf = (Be_Cnf_ptr) NULL;

    /* Problem dumping: */
    if (dump_type != BMC_DUMP_NONE) {
      cnf = Be_ConvertToCnf(be_mgr, prob, 0, cnf_alg);
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
        sbmc_remove_loop_variable(bdd_enc, be_fsm);
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
                       inputprop, (Prop_PrintFmt) get_prop_print_method(opts));
          }
          StreamMgr_print_output(streams,  "\n");

          break;
        }

      case SAT_SOLVER_SATISFIABLE_PROBLEM:
        StreamMgr_print_output(streams,  "-- ");

        print_spec(StreamMgr_get_output_ostream(streams),
                   inputprop, (Prop_PrintFmt) get_prop_print_method(opts));

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
        sbmc_remove_loop_variable(bdd_enc, be_fsm);
        ErrorMgr_internal_error(errmgr, "Sorry, solver answered with a fatal Internal "
                       "Failure during problem solving.\n");

      case SAT_SOLVER_TIMEOUT:
      case SAT_SOLVER_MEMOUT:
        sbmc_remove_loop_variable(bdd_enc, be_fsm);
        ErrorMgr_internal_error(errmgr, "Sorry, solver ran out of resources and aborted "
                       "the execution.\n");

      default:
        sbmc_remove_loop_variable(bdd_enc, be_fsm);
        ErrorMgr_internal_error(errmgr, "Bmc_GenSolveLtl: Unexpected value in sat result");

      } /* switch */

      SatSolver_destroy(solver);
    } /* must solve */

    if (cnf != (Be_Cnf_ptr) NULL) {
      Be_Cnf_Delete(cnf);
      cnf = (Be_Cnf_ptr) NULL;
    }

  } /* for all problems length */
  sbmc_remove_loop_variable(bdd_enc, be_fsm);

  Prop_Rewriter_update_original_property(rewriter);
  Prop_Rewriter_destroy(rewriter); rewriter = NULL;

  return 0;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief This is only a useful wrapper for easily call
  Bmc_Utils_ExpandMacrosInFilename



  \se None
*/
static void
bmc_expandFilename(const OptsHandler_ptr opts,
                   const int k, const int l,
                   const int prop_idx,
                   const char* filename_to_be_expanded,
                   char* filename_expanded,
                   const size_t filename_expanded_maxlen)
{
  char szBuffer[1024];
  char szLoopback[16];

  /* Prepares the structure for macro-expansion: */
  SubstString aSubstTable[] =  { SYMBOL_CREATE(),
         SYMBOL_CREATE(),
         SYMBOL_CREATE(),
         SYMBOL_CREATE(),
         SYMBOL_CREATE(),
         SYMBOL_CREATE()
  };

  /* customizes the table with runtime values: */
  Utils_StripPathNoExtension(get_input_file(opts), szBuffer);
  Bmc_Utils_ConvertLoopFromInteger(l, szLoopback, sizeof(szLoopback));

  SYMBOL_ASSIGN(aSubstTable[0], "@F", string,  "%s", get_input_file(opts));
  SYMBOL_ASSIGN(aSubstTable[1], "@f", string,  "%s", szBuffer);
  SYMBOL_ASSIGN(aSubstTable[2], "@k", integer, "%d", k);
  SYMBOL_ASSIGN(aSubstTable[3], "@l", string, "%s", szLoopback);
  if (prop_idx != BMC_NO_PROPERTY_INDEX) {
    SYMBOL_ASSIGN(aSubstTable[4], "@n", integer, "%d", prop_idx);
  }
  else {
    SYMBOL_ASSIGN(aSubstTable[4], "@n", string, "%s", "undef");
  }
  SYMBOL_ASSIGN(aSubstTable[5], "@@", string,  "%s", "@");

  Bmc_Utils_ExpandMacrosInFilename(filename_to_be_expanded,
           aSubstTable,
           sizeof(aSubstTable)/sizeof(aSubstTable[0]),
           filename_expanded,
           filename_expanded_maxlen);
}
