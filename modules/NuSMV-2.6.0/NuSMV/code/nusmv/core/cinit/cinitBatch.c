/* ---------------------------------------------------------------------------

  This file is part of the ``cinit'' package of NuSMV version 2.
  Copyright (C) 1998-2001 by CMU and FBK-irst.

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
  \author Roberto Cavada
  \brief This file contain the main routine for the batch use of NuSMV.

  This file contain the main routine for the batch use of
  NuSMV. The batch main executes the various model checking steps in a
  predefined order. After the processing of the input file than it
  return to the calling shell.

*/

#include "nusmv/shell/cmd/cmd.h"

#include "nusmv/core/cinit/cinit.h"
#include "nusmv/core/cinit/cinitInt.h"

#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/error.h"

#include "nusmv/core/bmc/bmc.h"
#include "nusmv/core/bmc/bmcUtils.h"

#include "nusmv/core/set/set.h"
#include "nusmv/core/fsm/bdd/BddFsm.h"
#include "nusmv/core/prop/propPkg.h"
#include "nusmv/core/mc/mc.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/parser/parser.h"



void CInit_batch_main(NuSMVEnv_ptr env)
{
  ErrorMgr_ptr errmgr;
  OptsHandler_ptr opts;
  DDMgr_ptr dd;
  PropDb_ptr prop_db;
  StreamMgr_ptr streams;
  OStream_ptr outstream;

  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  dd = DD_MGR(NuSMVEnv_get_value(env, ENV_DD_MGR));
  prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  outstream = StreamMgr_get_output_ostream(streams);

  /* Necessary to have standard behavior in the batch mode */
  ErrorMgr_reset_long_jmp(errmgr);
  CATCH(errmgr) {
    int res;

    {  /* 1: Read the model */
      char* fname = get_input_file(opts);
      if (NULL == fname) {
        StreamMgr_print_error(streams,
            "Input file is (null). You must set the input file before.\n");
        goto batch_exit_fail;
      }
      res = Parser_read_model(env, fname);
      if (res)
        goto batch_exit_fail;
    }

    {  /* 2: Flatten hierarchy */
      res = CompileFlatten_flatten_smv(env, true, false);
      if (res)
        goto batch_exit_fail;
    }

    /* If the -lp option is used, list the properties and exit */
    if (opt_list_properties(opts)) {
      res = PropDb_show_property(
          prop_db,
          false  /* print_props_num */,
          PROPDB_PRINT_FMT_DEFAULT,
          Prop_NoType,
          Prop_NoStatus,
          -1  /* prop_no */,
          OStream_get_stream(outstream));

      if (res)
        goto batch_exit_fail;
      else
        goto batch_exit_success;
    }

    {  /* 3: Builds the encodings */
      res = Compile_encode_variables(env,
                                     NULL  /*input_order_file_name*/,
                                     false /*bdd_enc_enum_only*/);
      if (res)
        goto batch_exit_fail;
    }

    {  /* 4: Builds the flat FSMs */
      res = Compile_create_flat_model(env);

      if (res)
        goto batch_exit_fail;
    }

    /* --------------------------------------------------- */
    /*  Write the flat and bool FSMs (if required)         */
    /* ----------------------------------------------------*/
    if (get_output_flatten_model_file(opts) != NIL(char)) {
      char* fname = get_output_flatten_model_file(opts);
      FILE* _file;
      if (NULL == fname) {
        _file = OStream_get_stream(outstream);
      }
      else {
        _file = fopen(fname, "w");
        if (NULL == _file) {
          StreamMgr_print_error(streams,  "Unable to open file \"%s\".\n", fname);
          goto batch_exit_fail;
        }
      }
      {
        SymbTable_ptr st =
            SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
        FlatHierarchy_ptr hierarchy =
            FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));


        Compile_WriteFlattenModel(env, _file, st,
                                  SymbTable_get_class_layer_names(
                                      st, (const char*) NULL),
                                  "MODULE main", hierarchy, true);
      }
      if (NULL != fname) {
        fclose(_file);
      }
    }  /* write flat model */

    {
      char* fname = get_output_boolean_model_file(opts);
      if (NULL != fname) {
        FILE* _file = fopen(fname, "w");
         if (NULL == _file) {
           StreamMgr_print_error(streams,  "Unable to open file \"%s\".\n", fname);
           goto  batch_exit_fail;
         }
        res = Compile_create_boolean_model(env);
        res |= Compile_write_model_flat_bool(env, fname, _file);
        fclose(_file);

        if (res)
          goto batch_exit_fail;
      }
    }

#if NUSMV_HAVE_SAT_SOLVER
    if (opt_bmc_mode(opts)) {
      /*  5.1 BMC starts */

      if (opt_verbose_level_gt(opts, 0)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "Entering BMC mode...\n");
      }

      /* build_boolean_model may have been already called if the output
         boolean model was specified in the argument list. */
      if (Compile_check_if_bool_model_was_built(env, NULL, false)) {
        res = Compile_create_boolean_model(env);
        if (res)
          goto batch_exit_fail;
      }

      /* Initializes the bmc package, and commits both the model and the
         determinization layers: */
      res |= Bmc_Pkg_bmc_setup(env, false);
      if (res)
        goto batch_exit_fail;

      if (get_prop_no(opts) != -1) {
        int prop_no = get_prop_no(opts);
        Prop_ptr prop;

        if (opt_verbose_level_gt(opts, 0)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, "Verifying property %d...\n", prop_no);
        }

        if ((prop_no < 0) ||
            (prop_no >= PropDb_get_size(prop_db))) {
          StreamMgr_print_error(streams,
                                "Error: \"%d\" is not a valid property index\n", prop_no);
          ErrorMgr_nusmv_exit(errmgr, 1);
        }

        prop = PropDb_get_prop_at_index(prop_db, prop_no);

        switch (Prop_get_type(prop)) {
          case Prop_Ltl:
            /* skip if -ils option is given */
            if (!opt_ignore_ltlspec(opts)) {
              const char* loop = get_bmc_pb_loop(opts);
              int rel_loop = Bmc_Utils_ConvertLoopFromString(loop, NULL);

              Bmc_GenSolveLtl(env, prop, get_bmc_pb_length(opts),
                              rel_loop, /*increasing length*/TRUE,
                              TRUE, BMC_DUMP_NONE, NULL);
            }
            break;

          case Prop_Psl:
            {
              const char* loop = get_bmc_pb_loop(opts);
              int rel_loop = Bmc_Utils_ConvertLoopFromString(loop, NULL);

              /* skip if -ips option is given */
              if (opt_ignore_pslspec(opts)) break;

              Bmc_Gen_check_psl_property(env, prop, false, false, false,
                                         get_bmc_pb_length(opts),
                                         rel_loop);
              break;
            }

          case Prop_Invar:
            /* skip if -ii option is given */
            if (opt_ignore_invar(opts)) break;

            Bmc_GenSolveInvar(env, prop, TRUE, BMC_DUMP_NONE, NULL);
            break;

          default:
            StreamMgr_print_error(streams,
                                  "Error: only LTL, PSL and INVAR properties"
                                  " can be checked in BMC mode\n");
            ErrorMgr_nusmv_exit(errmgr, 1);
        } /* switch on type */

      }
      else {
        /* Checks all ltlspecs, invarspecs and pslspecs */

        if (! opt_ignore_ltlspec(opts)) {
          lsList props;
          lsGen  iterator;
          Prop_ptr prop;
          int rel_loop;

          if (opt_verbose_level_gt(opts, 0)) {
            Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
            Logger_log(logger, "Verifying the LTL properties...\n");
          }

          props = PropDb_get_props_of_type(prop_db, Prop_Ltl);
          nusmv_assert(props != LS_NIL);

          lsForEachItem(props, iterator, prop) {
            const char* loop = get_bmc_pb_loop(opts);
            rel_loop = Bmc_Utils_ConvertLoopFromString(loop, NULL);

            Bmc_GenSolveLtl(env, prop, get_bmc_pb_length(opts),
                            rel_loop, /*increasing length*/ TRUE, TRUE,
                            BMC_DUMP_NONE, NULL);
          }

          lsDestroy(props, NULL);
        }

        if (! opt_ignore_pslspec(opts)) {
          lsList props;
          lsGen  iterator;
          Prop_ptr prop;
          const char* loop = get_bmc_pb_loop(opts);
          int rel_loop = Bmc_Utils_ConvertLoopFromString(loop, NULL);

          if (opt_verbose_level_gt(opts, 0)) {
            Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
            Logger_log(logger, "Verifying the PSL properties...\n");
          }

          props = PropDb_get_props_of_type(prop_db, Prop_Psl);
          nusmv_assert(props != LS_NIL);

          lsForEachItem(props, iterator, prop) {
            if (Prop_is_psl_ltl(prop)) {
              int len = get_bmc_pb_length(opts);
              Bmc_Gen_check_psl_property(env, prop, false, false, false,
                                         len, rel_loop);
            }
          }

          lsDestroy(props, NULL); /* the list is no longer needed */
        }

        if (! opt_ignore_invar(opts)) {
          lsList props;
          lsGen  iterator;
          Prop_ptr prop;

          if (opt_verbose_level_gt(opts, 0)) {
            Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
            Logger_log(logger, "Verifying the INVAR properties...\n");
          }

          props = PropDb_get_props_of_type(prop_db,
                                           Prop_Invar);
          nusmv_assert(props != LS_NIL);

          lsForEachItem(props, iterator, prop) {
            Bmc_GenSolveInvar(env, prop, TRUE, BMC_DUMP_NONE, NULL);
          }

          lsDestroy(props, NULL);
        }
      }

      /* exits */
      goto batch_exit_success;
    } /* end of BMC */
#endif

    {  /* 5.2 BDD-based model checking starts */

      /* Builds the BDD FSM of the whole read model.  If COI is
         enabled there is no reason to create global BDD FSM since
         every property will have its one instance of a BDD FSM. */
      if (!opt_cone_of_influence(opts)) {
        res = Compile_build_model(env, TRANS_TYPE_INVALID);
        if (res)
          goto batch_exit_fail;
      }
      else {
        if (opt_verbose_level_gt(opts, 0)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger,
                     "Construction of BDD model is delayed due to use of COI\n");
        }
      }
    }

    /* checks the fsm if required */
    if (opt_check_fsm(opts)) {
      BddFsm_ptr bdd_fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));

      if (opt_cone_of_influence(opts)) {
        StreamMgr_print_error(streams,
                              "WARNING: Check for totality of the transition "
                              "relation cannot currently\n"
                              "performed in batch mode if the cone"
                              " of influence reduction has been enabled.\n");
        ErrorMgr_nusmv_exit(errmgr, 1);
      }

      BddFsm_check_machine(bdd_fsm);
    }

    {  /* check required properties */
      int prop_no = get_prop_no(opts);

      if (-1 != prop_no) {
        res = PropDb_check_property(prop_db, Prop_NoType, NULL, prop_no);
      }
      else {
        /* Evaluates the Specifications */
        if (!opt_ignore_spec(opts)) {
          res = PropDb_check_property(prop_db, Prop_Ctl, NULL, prop_no);
        }
        if (!opt_ignore_compute(opts)) {
          res |= PropDb_check_property(prop_db, Prop_Compute, NULL, prop_no);
        }
        if (!opt_ignore_ltlspec(opts)) {
          res |= PropDb_check_property(prop_db, Prop_Ltl, NULL, prop_no);
        }
        if (!opt_ignore_pslspec(opts)) {
          res |= PropDb_check_property(prop_db, Prop_Psl, NULL, prop_no);
        }
        if (!opt_ignore_invar(opts)) {
          res |= PropDb_check_property(prop_db, Prop_Invar, NULL, prop_no);
        }
      }
      if (res)
        goto batch_exit_fail;
    }

    /* Reporting of statistical information. */
    if (opt_verbose_level_gt(opts, 0)) {
      res = Compile_print_usage(env, outstream);
      if (res)
        goto batch_exit_fail;
    }

    /* Computing and Reporting of the Effect of Reordering */
    if (opt_reorder(opts)) {
      StreamMgr_print_output(streams,  "\n========= starting reordering ============\n");
      dd_reorder(dd, get_reorder_method(opts),
                 DEFAULT_MINSIZE);
      StreamMgr_print_output(streams,  "\n========= after reordering ============\n");
      if (opt_verbose_level_gt(opts, 0)) {
        res = Compile_print_usage(env, outstream);
        if (res)
          goto batch_exit_fail;
      }

      {  /* write order */
        BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));

        res = BddEnc_write_var_ordering(
            bdd_enc,
            get_output_order_file(opts),
            opt_write_order_dumps_bits(opts)? DUMP_BITS: DUMP_DEFAULT);

        if (res)
          goto batch_exit_fail;
      }
    }

    /* Reporting of Reachable States */
    if (opt_print_reachable(opts)) {
      BddFsm_ptr fsm = NULL;
      if (opt_cone_of_influence(opts)) {
        StreamMgr_print_error(streams,
                              "WARNING: Statistics of reachable "
                              "states is not currently available\n"
                              "in batch mode if cone of influence "
                              "reduction has been enabled.\n");
        goto batch_exit_fail;
      }

      fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));
      BddFsm_print_reachable_states_info(fsm,
                                         false, /* do not print states */
                                         false, /* do not print defines */
                                         false, /* do not print formula */
                                         outstream);
    }

  } FAIL(errmgr) {
    StreamMgr_print_error(streams,  "\n%s terminated by a signal\n",
                          NuSMVCore_get_tool_name());
    goto batch_exit_fail;
  }

batch_exit_success:
  return;

batch_exit_fail:
  StreamMgr_print_error(streams,  "\nAborting batch mode\n");
  ErrorMgr_nusmv_exit(errmgr, 1);
  return;
}
