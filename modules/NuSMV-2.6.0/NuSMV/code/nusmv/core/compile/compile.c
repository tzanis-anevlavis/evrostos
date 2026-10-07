/* ---------------------------------------------------------------------------

   This file is part of the ``compile'' package.
   Copyright (C) 2012 by FBK.

   NuSMV version 2 is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   NuSMV version 2 is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied
   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
   See the GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
   MA 02111-1307 USA.

   For more information on NuSMV see <http://nusmv.fbk.eu> or
   email to <nusmv-users@fbk.eu>.
   Please report bugs to <nusmv-users@fbk.eu>.

   To contact the NuSMV development board, email to <nusmv@fbk.eu>.

-----------------------------------------------------------------------------*/

/*!
  \author Michele Dorigatti
  \brief Main routines for compile package

  \todo: Missing description

*/


#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/prop/PropDb.h"
#include "nusmv/core/trace/TraceMgr.h"
#include "nusmv/core/trace/exec/CompleteTraceExecutor.h"
#include "nusmv/core/trace/exec/PartialTraceExecutor.h"
#include "nusmv/core/trace/exec/BDDPartialTraceExecutor.h"
#include "nusmv/core/trace/exec/BDDCompleteTraceExecutor.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
extern cmp_struct_ptr cmps;

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

int Compile_print_usage(NuSMVEnv_ptr env,
                        OStream_ptr file)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  DDMgr_ptr dd = (DDMgr_ptr )NuSMVEnv_get_value(env, ENV_DD_MGR);
  OptsHandler_ptr opts =
      OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  int retval = 0;

  OStream_printf(file,
          "###################################"
          "###################################\n");
  util_print_cpu_stats(OStream_get_stream(file));
  OStream_printf(file,
          "###################################"
          "###################################\n");
  OStream_printf(file, "BDD statistics\n");
  OStream_printf(file, "--------------------\n");
  OStream_printf(file, "BDD nodes allocated: %d\n",
                 get_dd_nodes_allocated(dd));
  OStream_printf(file, "--------------------\n");

  if (opt_cone_of_influence(opts) == false) {
    if (! Compile_check_if_model_was_built(env, (FILE*)NULL, false)) {
      BddFsm_ptr fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));
      BddFsm_print_info(fsm, file); }
    else {
      StreamMgr_print_error(
          streams,
          "The BddFsm has not been built. Use the command "
          "\"go\" to build it.\n");
    }
  }
  else {
    StreamMgr_print_error(streams,
            "WARNING: Model Statistics is not currently available\n");
    StreamMgr_print_error(streams,
            "if cone of influence reduction has been enabled.\n");
  }

  return retval;
}

int Compile_print_fsm_stats(NuSMVEnv_ptr env,
                            BddFsm_ptr fsm,
                            FILE* outstream,
                            boolean printPreds)
{
  int retval = 0;

  if (fsm != BDD_FSM(NULL)) {
    OStream_ptr ostr = OStream_create(outstream);
    BddFsm_print_info(fsm, ostr);
    /* The caller must take care of the stream, so use the
       destroy_safe method */
    OStream_destroy_safe(ostr);
  }

  if (printPreds) Compile_print_predicates(env);

  return retval;
}

int Compile_check_if_model_was_built(const NuSMVEnv_ptr env,
                                     FILE* err, boolean forced)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (cmp_struct_get_build_model(cmps)) return 0;

  if (Compile_check_if_encoding_was_built(env, err)) return 1;

  if (cmp_struct_get_build_model(cmps) == 0 &&
      opt_cone_of_influence(opts) && !forced) return 0;

  if (err != (FILE*) NULL) {
    if (opt_cone_of_influence(opts)) {
      fprintf(
          err,
          "Model construction was delayed due to the use of "
          "Cone Of Influence.\n"
          "Use the command \"build_model -f\" to force the model "
          "construction.\n");
    }
    else {
      fprintf(
          err,
          "A model must be built before. Use the \"build_model\" command.\n");
    }
  }

  return 1;
}

int Compile_check_if_bool_model_was_built(const NuSMVEnv_ptr env, FILE* err,
                                          boolean forced)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (cmp_struct_get_build_bool_model(cmps)) return 0;
  if (cmp_struct_get_build_bool_model(cmps) == 0 &&
      cmp_struct_get_encode_variables(cmps) &&
      opt_cone_of_influence(opts) && !forced) return 0;

  if (Compile_check_if_encoding_was_built(env, err)) return 1;

  if (err != (FILE*) NULL) {
    if (cmp_struct_get_build_bool_model(cmps) == 0) {
      fprintf(err, "The boolean model must be built before.\n");
      if (opt_cone_of_influence(opts) && forced) {
        fprintf(err, "(Use the command \"build_boolean_model -f\" "
                "as Cone Of Influence is enabled.)\n");
      }
      else fprintf(err, "(Use the command \"build_boolean_model\")\n");
    }
  }

  return 1;
}

int Compile_check_if_flat_model_was_built(const NuSMVEnv_ptr env,
                                          FILE* err, boolean forced)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (cmp_struct_get_build_flat_model(cmps)) return 0;

  if (cmp_struct_get_build_flat_model(cmps) == 0 &&
      opt_cone_of_influence(opts) && !forced) return 0;

  if (Compile_check_if_flattening_was_built(env, err)) return 1;

  if (err != (FILE*) NULL) {
    if (cmp_struct_get_build_flat_model(cmps) == 0) {
      fprintf(err, "The flat model must be built before "\
              "(Use the command \"build_flat_model\")\n");
    }
  }

  return 1;
}

int Compile_check_if_encoding_was_built(const NuSMVEnv_ptr env, FILE* err)
{
  if (cmp_struct_get_encode_variables(cmps)) return 0;

  /* TODO[RC] here we may force encoding to happen after the flat
     model has been built */
  if (Compile_check_if_flattening_was_built(env, err)) return 1;
  /* if (Compile_check_if_flat_model_was_built(err, false)) return 1; */

  if (err != (FILE*) NULL) {
    fprintf(err,
            "The variables must be built before. Use the "\
            "\"encode_variables\" command.\n");
  }

  return 1;
}

int Compile_check_if_model_layer_is_in_bddenc(const NuSMVEnv_ptr env,
                                              FILE* err)
{
  if (0 == Compile_check_if_encoding_was_built(env, err)) {
    BddEnc_ptr bdd_enc;

    bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
    if (BaseEnc_layer_occurs(BASE_ENC(bdd_enc), MODEL_LAYER_NAME)) {
      return 0;
    }
    else {
      if (err != (FILE*) NULL) {
        fprintf(err,
                "The \"encode_variable\" must be called without the "   \
                "\"-n\" option to construct the BDD variables.\n");
      }
      return 1;
    }
  } /* encoding still not built */
  else {
    return 1;
  }
}

int Compile_check_if_flattening_was_built(const NuSMVEnv_ptr env, FILE* err)
{
  if (cmp_struct_get_flatten_hrc(cmps)) return 0;

  if (err != (FILE*) NULL) {
    if (cmp_struct_get_read_model(cmps) == 0) {
      fprintf(
          err,
          "A model must be read before. Use the \"read_model\" command.\n");
    }

    else fprintf(err,
                 "The hierarchy must be flattened before. Use the "\
                 "\"flatten_hierarchy\" command.\n");
  }

  return 1;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief  Encodes variables in the model (BDD only).

   Ownership of "input_order_file_name" is taken, unless it is
  the value returned by get_input_order_file

  \se
*/

int Compile_encode_variables(NuSMVEnv_ptr env,
                             char* input_order_file_name,
                             boolean bdd_enc_enum_only)
{
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  BoolEnc_ptr bool_enc;
  BddEnc_ptr bdd_enc;
  SymbTable_ptr st;
  SymbLayer_ptr model_layer;

  /* Creates the bool encoding, and commit the model layer, that was
     created during the flattening phase */
  Enc_init_bool_encoding(env);
  bool_enc = BOOL_ENC(NuSMVEnv_get_value(env, ENV_BOOL_ENCODER));
  BaseEnc_commit_layer(BASE_ENC(bool_enc), MODEL_LAYER_NAME);

  /* Creates the bdd encoding, and again commit the model layer */
  Enc_init_bdd_encoding(env, input_order_file_name);
  bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));

  /* We commit in the bdd enc only if the model has scalar variables,
     more in detail: we commit the layer in the bdd encoder in the
     following cases:

     - bdd_enc_enum_only is true: it means that the bdd encoder is
       used only to encode enumerations
     - there are enum variables in the symbol table
     - there are constants in the layer

     This is needed since constants are encoded by the bdd encoder for
     operations and evaluation of defines that involve them.
  */
  st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
  model_layer = SymbTable_get_layer(st, MODEL_LAYER_NAME);
  nusmv_assert(SYMB_LAYER(NULL) != model_layer);
  if (!bdd_enc_enum_only || SymbTable_contains_enum_variables(st) ||
      SymbLayer_get_constants_num(model_layer) > 0) {
    BaseEnc_commit_layer(BASE_ENC(bdd_enc), MODEL_LAYER_NAME);
  }

  cmp_struct_set_encode_variables(cmps);
  return 0;
}

int Compile_create_flat_model(NuSMVEnv_ptr env)
{
  SymbTable_ptr st =
      SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
  FlatHierarchy_ptr hierarchy =
      FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));
  FsmBuilder_ptr builder =
      FSM_BUILDER(NuSMVEnv_get_value(env, ENV_FSM_BUILDER));
  OptsHandler_ptr opts =
      OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  Set_t vars;
  SexpFsm_ptr sexp_fsm;
  SymbLayer_ptr layer;
  SymbLayerIter iter;

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "\nCreating the scalar FSM...\n");
  }

  layer = SymbTable_get_layer(st, MODEL_LAYER_NAME);

  SymbLayer_gen_iter(layer, &iter, STT_VAR);
  vars = SymbLayer_iter_to_set(layer, iter);

  /* scalar fsm */
  sexp_fsm = FsmBuilder_create_scalar_sexp_fsm(builder, hierarchy, vars);

  Set_ReleaseSet(vars);

  NuSMVEnv_set_value(env, ENV_SEXP_FSM, sexp_fsm);

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Successfully created the scalar FSM\n");
  }

  /* We keep track that the master FSM has been built. */
  cmp_struct_set_build_flat_model(cmps);
  return 0;
}

int Compile_create_boolean_model(NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  BddEnc_ptr benc =
      BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  BoolEnc_ptr bool_enc =
      BOOL_ENC(NuSMVEnv_get_value(env, ENV_BOOL_ENCODER));
  OptsHandler_ptr opts =
      OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  SexpFsm_ptr scalar_fsm;
  BoolSexpFsm_ptr bool_fsm;
  SymbTable_ptr st;
  SymbLayer_ptr det_layer;

  int reord_status;
  dd_reorderingtype rt;
  DDMgr_ptr dd;
  int retval = 0;

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "\nCreating the boolean FSM...\n");
  }

  st = BaseEnc_get_symb_table(BASE_ENC(benc));
  dd = BddEnc_get_dd_manager(benc);

  /* temporary disables reordering */
  reord_status = dd_reordering_status(dd, &rt);
  if (reord_status == 1) { dd_autodyn_disable(dd); }

  /* Creates a new layer for determinization vars */
  det_layer = SymbTable_create_layer(st, DETERM_LAYER_NAME,
                                     SYMB_LAYER_POS_BOTTOM);
  /* the layer needs to belong to both the layer classes */
  SymbTable_layer_add_to_class(st, DETERM_LAYER_NAME, NULL);
  SymbTable_layer_add_to_class(st, DETERM_LAYER_NAME,
                               ARTIFACTS_LAYERS_CLASS);

  scalar_fsm = SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));

  CATCH(errmgr) {
    bool_fsm = BoolSexpFsm_create_from_scalar_fsm(scalar_fsm,
                                                  benc, det_layer);
  }
  FAIL(errmgr) {
    SymbTable_remove_layer(st, det_layer);
    ErrorMgr_rpterr(errmgr, NULL); /* re-throw */
  }

  NuSMVEnv_set_value(env, ENV_BOOL_FSM, bool_fsm);

  /* Possibly added determinization variables introduced while
     building the boolean sexp model must be committed to the
     encodings */
  BaseEnc_commit_layer(BASE_ENC(bool_enc), DETERM_LAYER_NAME);
  BaseEnc_commit_layer(BASE_ENC(benc), DETERM_LAYER_NAME);

  /* If dynamic reordering was enabled, then it is re-enabled */
  if (reord_status == 1) { dd_autodyn_enable(dd, rt); }

  /* We keep track that the master FSM has been built. */
  cmp_struct_set_build_bool_model(cmps);

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Successfully created the boolean FSM\n");
  }

  return retval;
}


/*!
  \brief  Builds the BDD fsm.

   builds the model as a BDD fsm

  \se
   Environment is filled with the BddFsm
   bdd executors are registered in the trace_mgr
   global cmps is updated to build_model
*/
int Compile_build_model(NuSMVEnv_ptr env,
                        TransType partition_method)
{
  BddEnc_ptr benc =
      BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  FsmBuilder_ptr builder =
      FSM_BUILDER(NuSMVEnv_get_value(env, ENV_FSM_BUILDER));
  OptsHandler_ptr opts =
      OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  int retval = 0;

  if (TRANS_TYPE_INVALID != partition_method) {
    set_partition_method(opts, partition_method);
  }

  CATCH(errmgr) {
    SexpFsm_ptr sexp_fsm;
    BddFsm_ptr bdd_fsm;

    /* creates the model only if required (i.e. build_flat_model not called) */
    if (!NuSMVEnv_has_value(env, ENV_SEXP_FSM)) {
      Compile_create_flat_model(env);
    }

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "\nCreating the BDD FSM...\n");
    }

    sexp_fsm = SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM));

    bdd_fsm = FsmBuilder_create_bdd_fsm(builder, benc, sexp_fsm,
                                        get_partition_method(opts));

    /* Finally stores the built FSMs: */
    NuSMVEnv_set_value(env, ENV_BDD_FSM, bdd_fsm);

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Successfully created the BDD FSM\n");
    }

    { /* register BDD complete and partial executors */
      BddFsm_ptr fsm = BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM));
      BddEnc_ptr enc = BddFsm_get_bdd_encoding(fsm);

      TraceMgr_register_complete_trace_executor(
          TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
          "bdd",
          "BDD complete trace execution",
          COMPLETE_TRACE_EXECUTOR(BDDCompleteTraceExecutor_create(fsm,
                                                                  enc)));

      TraceMgr_register_partial_trace_executor(
          TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR)),
          "bdd",
          "BDD partial trace execution",
          PARTIAL_TRACE_EXECUTOR(BDDPartialTraceExecutor_create(fsm,
                                                                enc)));
    }

    /* We keep track that the master FSM has been built. */
    cmp_struct_set_build_model(cmps);
  }
  FAIL(errmgr) {
    retval = 1;
  }

  /* log the result */
  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    if (0 == retval) {
      Logger_log(logger,
                 "\nThe model has been built from file %s.\n", get_input_file(opts));
    }
    else {
      Logger_log(logger,
                 "\nSomething went wrong while building the model from file %s.\n",
                 get_input_file(opts));
    }
  }

  return retval;
}


Expr_ptr Compile_remove_assignments(const NuSMVEnv_ptr env,
                                    Expr_ptr expr)
{
  const ExprMgr_ptr exprs =
      EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  if (Nil == expr) return ExprMgr_true(exprs);

  if (AND == node_get_type(expr)) {
    return ExprMgr_and(exprs, Compile_remove_assignments(env, car(expr)),
                       Compile_remove_assignments(env, cdr(expr)));
  }
  else if (EQDEF == node_get_type(expr)) {
    return ExprMgr_true(exprs);
  }

  return expr;
}

void Compile_write_coi_prop(const NuSMVEnv_ptr env,
                            Set_t cone, Set_t props,
                            OStream_ptr output_file)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  Set_Iterator_t iter;
  boolean multiple = (Set_GiveCardinality(props) > 1);
  boolean keep;

  OStream_printf(output_file, "Propert%s",
                 (multiple ? "ies\n" : "y "));

  if (multiple) {
    int count = 1;
    SET_FOREACH(props, iter) {
      Prop_ptr prop = PROP(Set_GetMember(props, iter));
      OStream_printf(output_file, "\t%d) ", (count++));
      Prop_print(prop, output_file,
                 get_prop_print_method(opts));
      OStream_printf(output_file, "\n");
    }
  }
  else {
    Prop_ptr prop;
    iter = Set_GetFirstIter(props);
    prop = PROP(Set_GetMember(props, iter));
    Prop_print(prop, output_file,
               get_prop_print_method(opts));
  }

  OStream_printf(output_file, "%s COI:\n",
                 (multiple ? "share" : "has"));
  OStream_printf(output_file, "   {\n   ");

  iter = Set_GetFirstIter(cone);
  keep = !Set_IsEndIter(iter);
  while (keep) {
    print_node(wffprint, OStream_get_stream(output_file),
               (node_ptr) Set_GetMember(cone, iter));

    iter = Set_GetNextIter(iter);
    keep = !Set_IsEndIter(iter);
    if (keep) OStream_printf(output_file, ",\n   ");
  }
  OStream_printf(output_file, "\n   }");
  OStream_printf(output_file, "\n");
}

void Compile_write_coi_prop_fsm(const NuSMVEnv_ptr env,
                                FlatHierarchy_ptr fh,
                                Set_t cone, Set_t props,
                                OStream_ptr output_file)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  OStream_ptr errstream = StreamMgr_get_error_ostream(streams);
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  SymbTable_ptr symb_table = FlatHierarchy_get_symb_table(fh);
  array_t* layers = SymbTable_get_class_layer_names(symb_table,
                                                    (const char*) NULL);
  SexpFsm_ptr fsm;
  FlatHierarchy_ptr coi_hierarchy = FLAT_HIERARCHY(NULL);
  FlatHierarchy_ptr fsm_fh = FLAT_HIERARCHY(NULL);

  /* The DD manager is not necessary since we don't build any BDD fsm. */
  FsmBuilder_ptr builder = FsmBuilder_create(env);

  fsm = FsmBuilder_create_scalar_sexp_fsm(builder, fh, cone);
  nusmv_assert(SEXP_FSM(NULL) != fsm);

  fsm_fh = SexpFsm_get_hierarchy(fsm);

  /* Do some post-processings over the COI hierarchy */
  {
    /* ----------------------------------------------------------------- */
    /* 1. -> INIT, TRANS, INVAR, INPUT, JUSTICE, COMPASSION. */
    {
      Expr_ptr init = FlatHierarchy_get_init(fsm_fh);
      Expr_ptr trans = FlatHierarchy_get_trans(fsm_fh);
      Expr_ptr invar = FlatHierarchy_get_invar(fsm_fh);

      coi_hierarchy =
        FlatHierarchy_create_from_members(
            symb_table,
            Compile_remove_assignments(env, init),
            Compile_remove_assignments(env, invar),
            Compile_remove_assignments(env, trans),
            FlatHierarchy_get_input(fsm_fh),
            FlatHierarchy_get_justice(fsm_fh),
            FlatHierarchy_get_compassion(fsm_fh));
    }

    /* ----------------------------------------------------------------- */
    /* 2. -> VARIABLES and ASSIGNMENTS. */
    {
      /* Add variables of the COI into the new Flat Hierarchy, and all
         it's relative assignments  */
      Set_Iterator_t iter;
      SET_FOREACH(cone, iter) {
        node_ptr var = Set_GetMember(cone, iter);
        FlatHierarchy_add_var(coi_hierarchy, var);

        {
          /* Add assignments for all variables in COI */
          node_ptr init_var = find_node(nodemgr, SMALLINIT, var, Nil);
          node_ptr next_var = find_node(nodemgr, NEXT, var, Nil);
          node_ptr tmp;

          /* First try with normal assignments */
          tmp = FlatHierarchy_lookup_assign(fsm_fh, var);
          if (Nil != tmp) {
            FlatHierarchy_insert_assign(coi_hierarchy, var, tmp);
          }
          /* Try with NEXT and INIT */
          else {
            /* init(var) := expr */
            tmp = FlatHierarchy_lookup_assign(fsm_fh, init_var);
            if (Nil != tmp) {
              FlatHierarchy_insert_assign(coi_hierarchy, init_var, tmp);
            }
            /* next(var) := expr */
            tmp = FlatHierarchy_lookup_assign(fsm_fh, next_var);
            if (Nil != tmp) {
              FlatHierarchy_insert_assign(coi_hierarchy, next_var, tmp);
            }
          }
        } /* Assignments treating */
      } /* SET_FOREACH(cone, iter) */
    }

    /* ----------------------------------------------------------------- */
    /* 3. -> SPECIFICATIONS. */
    {
      Set_Iterator_t iter;

      FlatHierarchy_set_ltlspec(coi_hierarchy, Nil);
      FlatHierarchy_set_spec(coi_hierarchy, Nil);
      FlatHierarchy_set_pslspec(coi_hierarchy, Nil);
      FlatHierarchy_set_compute(coi_hierarchy, Nil);
      FlatHierarchy_set_invarspec(coi_hierarchy, Nil);

      SET_FOREACH(props, iter) {
        Prop_ptr prop = PROP(Set_GetMember(props, iter));
        Expr_ptr prop_expr = Prop_get_expr(prop);
        node_ptr prop_name = Prop_get_name(prop);

        /* Add only the property for which we are dumping the model */
        switch (Prop_get_type(prop)) {
        case Prop_Ltl:
          FlatHierarchy_set_ltlspec(
              coi_hierarchy,
              cons(nodemgr, find_node(nodemgr, LTLSPEC, prop_expr, prop_name),
                   FlatHierarchy_get_ltlspec(coi_hierarchy)));
          break;
        case Prop_Ctl:
          FlatHierarchy_set_spec(
              coi_hierarchy,
              cons(nodemgr, find_node(nodemgr, SPEC, prop_expr, prop_name),
                   FlatHierarchy_get_spec(coi_hierarchy)));
          break;
        case Prop_Psl:
          FlatHierarchy_set_pslspec(
              coi_hierarchy,
              cons(nodemgr, find_node(nodemgr, PSLSPEC, prop_expr, prop_name),
                   FlatHierarchy_get_pslspec(coi_hierarchy)));
          break;
        case Prop_Compute:
          FlatHierarchy_set_compute(
              coi_hierarchy,
              cons(nodemgr, find_node(nodemgr, COMPUTE, prop_expr, prop_name),
                   FlatHierarchy_get_compute(coi_hierarchy)));
          break;
        case Prop_Invar:
          FlatHierarchy_set_invarspec(
              coi_hierarchy,
              cons(nodemgr, find_node(nodemgr, INVARSPEC, prop_expr, prop_name),
                   FlatHierarchy_get_invarspec(coi_hierarchy)));
          break;

        default:
          StreamMgr_print_error(streams,  "Unhandled property \"");
          Prop_print(prop, errstream,
                     get_prop_print_method(opts));
          StreamMgr_print_error(streams,  "\"\n");
          return;
        }
      }
    }
  } /* end of Hierarchy post-processings */

  Compile_WriteRestrictedFlattenModel(env, OStream_get_stream(output_file),
                                      symb_table, layers,
                                      "MODULE main", coi_hierarchy, true);

  FlatHierarchy_destroy(coi_hierarchy);
  FsmBuilder_destroy(builder);
  SexpFsm_destroy(fsm);
}

int Compile_get_bits(const SymbTable_ptr st, const NodeList_ptr lst)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  StreamMgr_ptr streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  ListIter_ptr iter;
  int res;

  res = 0;

  NODE_LIST_FOREACH(lst, iter) {
    node_ptr var;
    SymbType_ptr type;

    var = NodeList_get_elem_at(lst, iter);
    type = SymbTable_get_var_type(st, var);

    if (SymbType_is_boolean(type)) {
      res += 1;
    }
    else if (SymbType_is_enum(type)) {
      res +=
        Utils_log2_round(
            max(llength(SymbType_get_enum_type_values(type))-1, 1));
    }
    else if (SymbType_is_word(type)) {
      res += SymbType_get_word_width(type);
    }
    else if (SymbType_is_wordarray(type)){
      /* The casting is needed since in this case the subtype is a
         node_ptr cons list of subtypes */
      NodeList_ptr subtype_lst =
        NodeList_create_from_element(
            (node_ptr)SymbType_get_wordarray_subtype(type));
      res += (SymbType_get_wordarray_awidth(type) *
              Compile_get_bits(st, subtype_lst));
    }
    else {
      StreamMgr_print_error(
          streams,
          "**WARNING** Unknown variable type in bit counting: ");
      SymbType_print(type, wffprint, errstream);
      StreamMgr_print_error(streams,
                            ".\n");
    }
  }

  return res;
}

void Compile_write_global_coi_fsm(NuSMVEnv_ptr env,
                                  FlatHierarchy_ptr hierarchy,
                                  Prop_Type prop_type,
                                  OStream_ptr output_file)
{
  int prop_no;
  Set_t coi_union = Set_MakeEmpty();
  Set_t properties = Set_MakeEmpty();
  SymbTable_ptr symb_table = FlatHierarchy_get_symb_table(hierarchy);
  PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));

  for (prop_no = 0; prop_no < PropDb_get_size(prop_db); ++prop_no) {
    Prop_ptr prop = PropDb_get_prop_at_index(prop_db, prop_no);

    if ((prop_type == Prop_NoType) || (prop_type == Prop_get_type(prop))) {
      Set_t cone = Prop_compute_cone(prop, hierarchy, symb_table);
      coi_union = Set_Union(coi_union, cone);
      properties = Set_AddMember(properties, (Set_Element_t)prop);
      Set_ReleaseSet(cone);
    }
  }

  Compile_write_coi_prop_fsm(env, hierarchy, coi_union,
                             properties, output_file);

  Set_ReleaseSet(coi_union);
  Set_ReleaseSet(properties);
}

int Compile_write_properties_coi(NuSMVEnv_ptr env,
                                 FlatHierarchy_ptr hierarchy,
                                 Prop_Type prop_type,
                                 boolean only_dump_coi,
                                 const char* file_name)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  NodeList_ptr order =
    PropDb_get_coi_grouped_properties(
        PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB)), hierarchy);
  ListIter_ptr iter;
  int prop_no = 0;
  OStream_ptr output_file = StreamMgr_get_output_ostream(streams);

  NODE_LIST_FOREACH(order, iter) {
    node_ptr couple = NodeList_get_elem_at(order, iter);
    Set_t cone = (Set_t)car(couple);
    Set_t props = (Set_t)cdr(couple);
    Set_t filtered;
    char* new_file_name = NIL(char);

    if (Prop_NoType == prop_type) {
      filtered = Set_Copy(props);
    }
    else {
      Set_Iterator_t iter;
      filtered = Set_MakeEmpty();
      SET_FOREACH(props, iter) {
        Prop_ptr prop = PROP(Set_GetMember(props, iter));
        if (Prop_get_type(prop) == prop_type) {
          filtered = Set_AddMember(filtered, (Set_Element_t)prop);
        }
      }
    }

    if (!Set_IsEmpty(filtered)) {

      if (NIL(char) != file_name) {
        int max_len = strlen(file_name) + 5;
        int chars;

        new_file_name = ALLOC(char, max_len);
        chars = snprintf(new_file_name, max_len, "%s.%d", file_name, prop_no);
        SNPRINTF_CHECK(chars, max_len);

        output_file = OStream_create_file(new_file_name, false);
        if (OSTREAM(NULL) == output_file) {
          StreamMgr_print_error(
              streams,  "Cannot open file '%s' for writing\n", new_file_name);
          return 1;
        }
      }

      if (only_dump_coi) {
        Compile_write_coi_prop(env, cone, filtered, output_file);
      }
      else {
        Compile_write_coi_prop_fsm(env, hierarchy, cone,
                                   filtered, output_file);
      }

      OStream_flush(output_file);
      if (NIL(char) != file_name) {
        OStream_destroy(output_file);
        FREE(new_file_name);
      }

      ++prop_no;
    }

    Set_ReleaseSet(cone);
    Set_ReleaseSet(props);
    Set_ReleaseSet(filtered);
    free_node(nodemgr, couple);
  }
  NodeList_destroy(order);

  return 0;
}

void Compile_print_type(const NuSMVEnv_ptr env,
                        OStream_ptr file, node_ptr ntype, int threshold)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  switch(node_get_type(ntype)) {
  case BOOLEAN:
    OStream_printf(file, "boolean\n");
    break;
  case SCALAR:
    {
      int i = 0, missing = 0;
      node_ptr l = car(ntype);
      Olist_ptr values_list = Olist_create();
      Oiter iter;
      const char* fmt = "(other %d values)";

      OStream_printf(file, "{");
      while (l != Nil) {
        node_ptr val = car(l);
        l = cdr(l);

        /* One element has been already added to the missing set,
           all other elements must be skipped. */
        if (missing > 0) {
          /* Do not add the string representation of the node if it
             is not necessary.. */
          Olist_append(values_list, NULL);
          ++missing;
        }
        else {
          char* tmp = sprint_node(wffprint, val);
          Olist_append(values_list, tmp);

          i += strlen(tmp) + (l != Nil ? 2 : 0);

          /* Threshold == 0 means no limit.. */
          if (i > threshold && (threshold != 0)) {
            /* Restore the latests maximum length, so that the "fmt"
               string can be printed within the threshold length.. */
            Olist_ptr rev = Olist_copy_reversed(values_list);
            Oiter ri;

            OLIST_FOREACH(rev, ri) {
              char* v = Oiter_element(ri);
              int l = (strlen(v) + 2);

              ++missing;
              i -= l;

              /* +2: Save some space for big numbers: if the number
                 of missing values is very high, the number printed
                 in the fmt will use alot of characters. In this
                 way, we are assured that we can print correctly up
                 to 9999 values. */
              if (i <= (threshold - (strlen(fmt) + 2))) {
                break;
              }
            }
            Olist_destroy(rev);
          }
        }
      }

      i = 0;
      OLIST_FOREACH(values_list, iter) {
        char* el = (char*)Oiter_element(iter);

        if (i < (Olist_get_size(values_list) - missing)) {
          OStream_printf(file, "%s%s", el,
                         (++i == Olist_get_size(values_list) ? "" : ", "));
        }

        if ((char*)NULL != el) {
          FREE(el);
        }
      }
      if (missing > 0) { OStream_printf(file, fmt, missing); }

      OStream_printf(file, "}\n");
      Olist_destroy(values_list);
      break;
    }
  case SIGNED_WORD:
    OStream_printf(file, "signed word[%d]\n", PTR_TO_INT(car(ntype)));
    break;
  case UNSIGNED_WORD:
    OStream_printf(file, "unsigned word[%d]\n", PTR_TO_INT(car(ntype)));
    break;
  case REAL:
    OStream_printf(file, "real\n");
    break;
  case INTEGER:
    OStream_printf(file, "integer\n");
    break;
  default:
    ErrorMgr_rpterr(errmgr, "Unsupported type found.");
    error_unreachable_code();
  }

}

node_ptr Compile_get_var_type(const NuSMVEnv_ptr env,
                              SymbType_ptr type)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterNormalizer_ptr normalizer =
    MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));

  node_ptr ntype = Nil;
  int j;

  switch (SymbType_get_tag(type)) {
  case SYMB_TYPE_BOOLEAN:
    ntype = find_node(nodemgr, BOOLEAN, Nil, Nil);
    break;
  case SYMB_TYPE_ENUM:
    ntype = find_node(nodemgr, SCALAR,
                      MasterNormalizer_normalize_node(
                          normalizer,
                          SymbType_get_enum_type_values(type)),
                      Nil);
    break;
    /* (infinite-precision) integer */
  case SYMB_TYPE_INTEGER:
    ntype = find_node(nodemgr, INTEGER, Nil, Nil);
    break;

    /* (infinite-precision) rational */
  case SYMB_TYPE_REAL:
    ntype = find_node(nodemgr, REAL, Nil, Nil);
    break;

    /* word is like an arrary of booleans + signed arithmetic */
  case SYMB_TYPE_SIGNED_WORD:
    j = SymbType_get_word_width(type);
    ntype = find_node(nodemgr, SIGNED_WORD, PTR_FROM_INT(node_ptr, j), Nil);
    break;

    /* word is like an arrary of booleans + unsigned arithmetic */
  case SYMB_TYPE_UNSIGNED_WORD:
    j = SymbType_get_word_width(type);
    ntype = find_node(nodemgr, UNSIGNED_WORD, PTR_FROM_INT(node_ptr, j), Nil);
    break;

  default:
    ErrorMgr_rpterr(errmgr, "Unsupported type found.");
    error_unreachable_code();
  }

  return ntype;
}

void Compile_print_summary(const NuSMVEnv_ptr env,
                           OStream_ptr file, SymbTable_ptr st,
                           NodeList_ptr list, const char * str,
                           boolean limit_output)
{
  int j;
  SymbType_ptr type;
  ListIter_ptr iter;
  hash_ptr h = new_assoc();
  node_ptr name, val, ntype;
  assoc_iter aiter;

  OStream_printf(file, "Number of %s: %d\n", str, NodeList_get_length(list));

  NODE_LIST_FOREACH(list, iter) {
    j = 0;

    name = NodeList_get_elem_at(list, iter);

    /* only vars may be met here */
    nusmv_assert(SymbTable_is_symbol_var(st, name));

    type = SymbTable_get_var_type(st, name);

    ntype = Compile_get_var_type(env, type);

    j = PTR_TO_INT(find_assoc(h, ntype));

    if (0 == j) {
      insert_assoc(h, ntype, PTR_FROM_INT(node_ptr, 1));
    }
    else {
      j += 1;
      insert_assoc(h, ntype, PTR_FROM_INT(node_ptr, j));
    }
  } /* for */

  ASSOC_FOREACH(h, aiter, &ntype, &val) {
    j = PTR_TO_INT(val);
    OStream_printf(file, " %4d: ", j);
    Compile_print_type(env, file, ntype, (limit_output ? 71 : 0));
  }
  free_assoc(h);
}

void Compile_show_vars(const NuSMVEnv_ptr env, const boolean total_only,
                       const boolean defs_only, const boolean vars_only,
                       const boolean statevars, const boolean frozenvars,
                       const boolean inputvars, const OStream_ptr ostream,
                       const boolean verbose)
{
  const SymbTable_ptr st =
      SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);

  if (!total_only && !defs_only) {
    /* normal printout, not total only */
    NodeList_ptr list = NodeList_create();
    NodeList_ptr alls =  NodeList_create();
    NodeList_ptr allf =  NodeList_create();
    NodeList_ptr alli =  NodeList_create();

    { /* extracts the default class layer names */
      array_t* layer_names = SymbTable_get_class_layer_names(st, NULL);
      char* layer_name;
      int i;

      arrayForEachItem(char*, layer_names, i, layer_name) {
        SymbLayer_ptr layer = SymbTable_get_layer(st, layer_name);
        SymbLayerIter iter;

        SYMB_LAYER_FOREACH(layer, iter, STT_VAR) {
          node_ptr var = SymbLayer_iter_get_symbol(layer, &iter);

          if (SymbTable_is_symbol_state_var(st, var)) {
            if (!vars_only) { NodeList_append(alls, var); }
            if (statevars) { NodeList_append(list, var); }
          }
          else if (SymbTable_is_symbol_frozen_var(st, var)) {
            if (!vars_only) { NodeList_append(allf, var); }
            if (frozenvars) { NodeList_append(list, var); }
          }
          else if (SymbTable_is_symbol_input_var(st, var)) {
            if (!vars_only) { NodeList_append(alli, var); }
            if (inputvars) { NodeList_append(list, var); }
          }
          else { error_unreachable_code(); }
        }
      }
    }

    if (!vars_only) {
      Compile_print_summary(env, ostream, st, alli,
                            "Input Variables", !verbose);
      Compile_print_summary(env, ostream, st, alls,
                            "State Variables", !verbose);
      Compile_print_summary(env, ostream, st, allf,
                            "Frozen Variables", !verbose);
    }

    OStream_set_indent_size(ostream, 1);
    { /* prints the variables */
      ListIter_ptr iter;
      NODE_LIST_FOREACH(list, iter) {
        node_ptr name = NodeList_get_elem_at(list, iter);
        char* s_name = sprint_node(wffprint, name);
        node_ptr ntype;
        /* only vars may be met here */
        nusmv_assert(SymbTable_is_symbol_var(st, name));

        OStream_printf(ostream, "%s : ", s_name);


        ntype = Compile_get_var_type(env, SymbTable_get_var_type(st, name));
        Compile_print_type(env, ostream, ntype,
                           (verbose ? 0 : (80 - strlen(s_name) - 3)));

        OStream_printf(ostream, "\n");

        FREE(s_name);
      } /* for */
    }
    OStream_reset_indent_size(ostream);

    if (!vars_only) {
      /* Print Bits */
      int frozen, input, state;

      input = Compile_get_bits(st, alli);
      frozen = Compile_get_bits(st, allf);
      state = Compile_get_bits(st, alls);

      OStream_printf(ostream,
                     "Number of bits: %d (%d frozen, %d input, %d state)\n",
                     frozen + input + state, frozen, input, state);
    }

    NodeList_destroy(alls);
    NodeList_destroy(allf);
    NodeList_destroy(alli);
    NodeList_destroy(list);
  }

  else if (!total_only && defs_only) {
    OStream_set_indent_size(ostream, 1);

    { /* prints the defines */
      SymbTableIter iter;
      TypeChecker_ptr tc = SymbTable_get_type_checker(st);
      const MasterPrinter_ptr wffprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

      SYMB_TABLE_FOREACH(st, iter, STT_DEFINE) {
        node_ptr name = SymbTable_iter_get_symbol(st, &iter);
        SymbType_ptr type = TypeChecker_get_expression_type(tc, name, Nil);

        nusmv_assert(SymbTable_is_symbol_define(st, name));

        OStream_nprintf(ostream, wffprint, "%N : ", name);
        SymbType_print(type, wffprint, outstream);
        OStream_printf(ostream,  "\n");
      } /* for */
    }
    OStream_reset_indent_size(ostream);
  }
  else {
    /* total only */
    NodeList_ptr list = NodeList_create();

    /* extracts the default class layer names */
    array_t* layer_names = SymbTable_get_class_layer_names(st, NULL);
    char* layer_name;
    int i;

    arrayForEachItem(char*, layer_names, i, layer_name) {
      SymbLayer_ptr layer = SymbTable_get_layer(st, layer_name);
      SymbLayerIter iter;

      SYMB_LAYER_FOREACH(layer, iter, STT_VAR) {
        node_ptr var = SymbLayer_iter_get_symbol(layer, &iter);

        if (SymbTable_is_symbol_state_var(st, var)) {
          if (statevars) { NodeList_append(list, var); }
        }
        else if (SymbTable_is_symbol_frozen_var(st, var)) {
          if (frozenvars) { NodeList_append(list, var); }
        }
        else if (SymbTable_is_symbol_input_var(st, var)) {
          if (inputvars) { NodeList_append(list, var); }
        }
        else { error_unreachable_code(); }
      }
    }

    Compile_print_summary(env, ostream, st, list,
                          "all selected variables", !verbose);

    NodeList_destroy(list);
  }
}

void Compile_print_predicates(const NuSMVEnv_ptr env)
{
  /* [AT] Note: here the flatten hierarchy is normalized and then the
   * result is lost */
  const SymbTable_ptr st =
      SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
  const FlatHierarchy_ptr hierarchy =
    FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  Set_t predicates = Set_MakeEmpty();
  PredicateNormaliser_ptr normalizer = PredicateNormaliser_create(st);
  node_ptr tmp;
  array_t* layers_name;
  int i;
  const char* a_layer_name;

  /* normalize subparts of the hierarchy and collect the predicates.
     Note the normalized expressions are not freed because they are
     created with find_node.
  */
  tmp = FlatHierarchy_get_init(hierarchy);
  tmp = PredicateNormaliser_normalise_expr(normalizer, tmp);
  PredicateNormaliser_get_predicates_only(normalizer, &predicates, tmp);

  tmp = FlatHierarchy_get_invar(hierarchy);
  tmp = PredicateNormaliser_normalise_expr(normalizer, tmp);
  PredicateNormaliser_get_predicates_only(normalizer, &predicates, tmp);

  tmp = FlatHierarchy_get_trans(hierarchy);
  tmp = PredicateNormaliser_normalise_expr(normalizer, tmp);
  PredicateNormaliser_get_predicates_only(normalizer, &predicates, tmp);

  tmp = FlatHierarchy_get_input(hierarchy);
  tmp = PredicateNormaliser_normalise_expr(normalizer, tmp);
  PredicateNormaliser_get_predicates_only(normalizer, &predicates, tmp);

  tmp = FlatHierarchy_get_justice(hierarchy);
  tmp = PredicateNormaliser_normalise_expr(normalizer, tmp);
  PredicateNormaliser_get_predicates_only(normalizer, &predicates, tmp);

  tmp = FlatHierarchy_get_compassion(hierarchy);
  tmp = PredicateNormaliser_normalise_expr(normalizer, tmp);
  PredicateNormaliser_get_predicates_only(normalizer, &predicates, tmp);

  /* assignments require very special handling because
     FlatHierarchy_get_assign returns the assignments without
     CASE-expressions and "running" variables created when there are
     processes. To obtain the actual assignments it is necessary to
     collects assignments using FlatHierarchy_lookup_assign.

     NOTE: This code is terrible because API in FlatHierarchy does
     not provided there required function (to access actual assignments).
  */
  layers_name = SymbTable_get_class_layer_names(st, (const char*) NULL);

  arrayForEachItem(const char*, layers_name, i, a_layer_name) {
    SymbLayer_ptr layer = SymbTable_get_layer(st, a_layer_name);
    SymbLayerIter iter;

    SYMB_LAYER_FOREACH(layer, iter, STT_VAR) {
      node_ptr name = SymbLayer_iter_get_symbol(layer, &iter);
      node_ptr init_name = find_node(nodemgr, SMALLINIT, name, Nil);
      node_ptr next_name = find_node(nodemgr, NEXT, name, Nil);
      node_ptr invar_expr = FlatHierarchy_lookup_assign(hierarchy, name);
      node_ptr init_expr = FlatHierarchy_lookup_assign(hierarchy, init_name);
      node_ptr next_expr = FlatHierarchy_lookup_assign(hierarchy, next_name);

      if (invar_expr != Nil) {
        tmp = PredicateNormaliser_normalise_expr(
            normalizer, find_node(nodemgr, EQDEF, name, invar_expr));
        PredicateNormaliser_get_predicates_only(normalizer, &predicates, tmp);
      }
      if (init_expr != Nil) {
        tmp = PredicateNormaliser_normalise_expr(
            normalizer, find_node(nodemgr, EQDEF, init_name, init_expr));
        PredicateNormaliser_get_predicates_only(normalizer, &predicates, tmp);
      }
      if (next_expr != Nil) {
        tmp = PredicateNormaliser_normalise_expr(
            normalizer, find_node(nodemgr, EQDEF, next_name, next_expr));
        PredicateNormaliser_get_predicates_only(normalizer, &predicates, tmp);
      }
    }
  }

  StreamMgr_print_output(streams,
                         "\nFSM consists of the following Predicates:\n");
  {
    Set_Iterator_t iter;
    const MasterPrinter_ptr wffprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    SET_FOREACH(predicates, iter) {
        node_ptr pred = Set_GetMember(predicates, iter);
        StreamMgr_nprint_output(streams, wffprint, "%N", pred);
        StreamMgr_print_output(streams,  "\n\n");
    }
  }
  Set_ReleaseSet(predicates);
  PredicateNormaliser_destroy(normalizer);
}

int Compile_write_model_flat_bool(const NuSMVEnv_ptr env,
                                  const char* output_file,
                                  FILE* ofileid)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  int rv = 0;

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Writing boolean model into file \"%s\"..",
               output_file == (char *)NULL ? "stdout" : output_file);
  }

  CATCH(errmgr) {
    BoolEnc_ptr bool_enc =
        BOOL_ENC(NuSMVEnv_get_value(env, ENV_BOOL_ENCODER));
    BddEnc_ptr enc =
        BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
    SymbTable_ptr st =
        BaseEnc_get_symb_table(BASE_ENC(bool_enc));
    NodeList_ptr layers = NodeList_create();
    int i;
    const char* name;

    /* fills in layers list with those list that are in default class and have
       been committed */
    arrayForEachItem(const char*, SymbTable_get_class_layer_names(st, NULL),
                     i, name) {
      SymbLayer_ptr layer = SymbTable_get_layer(st, name);
      if (NodeList_belongs_to(BaseEnc_get_committed_layers(BASE_ENC(bool_enc)),
                              (node_ptr) layer)) {
        NodeList_append(layers, (node_ptr) layer);
      }
      { /* gets and add the corresponding boolean layer if exists */
        const char* bname =
            BoolEnc_scalar_layer_to_bool_layer(bool_enc, name);
        layer = SymbTable_get_layer(st, bname);
        if (layer != SYMB_LAYER(NULL) &&
            !NodeList_belongs_to(layers, (node_ptr) layer) &&
            NodeList_belongs_to(
                BaseEnc_get_committed_layers(BASE_ENC(bool_enc)),
                (node_ptr) layer)) {
          NodeList_append(layers, (node_ptr) layer);
        }
      }
    }

    Compile_WriteBoolModel(env, ofileid, enc, layers, "MODULE main",
                           BOOL_SEXP_FSM(
                               NuSMVEnv_get_value(env, ENV_BOOL_FSM)), true);

    NodeList_destroy(layers);

    if ((BddEnc_get_reordering_count(enc) > 0) &&
        (get_output_order_file(opts) != (char*) NULL) &&
        (ofileid != outstream)) {
      /* there was a reordering, so a variable ordering file is dumped */
      BddEnc_write_var_ordering(enc, get_output_order_file(opts),
                                opt_write_order_dumps_bits(opts) ?
                                DUMP_BITS : DUMP_DEFAULT);

      StreamMgr_print_error(
          streams,
          "%d reordering(s) occurred. Dumped variable ordering to '%s'\n",
          BddEnc_get_reordering_count(enc),
          get_output_order_file(opts));
      BddEnc_reset_reordering_count(enc);
    }

    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, ".. done.\n");
    }
  } FAIL(errmgr) {
    rv = 1;
  }
  fflush(ofileid);

  return rv;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
