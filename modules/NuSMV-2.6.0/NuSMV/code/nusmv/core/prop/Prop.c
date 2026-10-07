/* ---------------------------------------------------------------------------


  This file is part of the ``prop'' package of NuSMV version 2.
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
  \author Marco Roveri, Roberto Cavada
  \brief Implementation of class 'Prop'

  \todo: Missing description

*/


#if HAVE_CONFIG_H
#include "nusmv-config.h"
#endif

#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"

#include "nusmv/core/prop/Prop.h"
#include "nusmv/core/prop/Prop_private.h"
#include "nusmv/core/prop/propInt.h"
#include "nusmv/core/prop/PropDb.h"
#include "nusmv/core/prop/propPkg.h"

#include "nusmv/core/mc/mc.h"
#include "nusmv/core/ltl/ltl.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/psl/pslNode.h"
#include "nusmv/core/compile/compile.h"

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/utils_io.h"

#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/trace/exec/PartialTraceExecutor.h"
#include "nusmv/core/trace/exec/BDDPartialTraceExecutor.h"
#include "nusmv/core/trace/exec/SATPartialTraceExecutor.h"
#include "nusmv/core/trace/exec/CompleteTraceExecutor.h"
#include "nusmv/core/trace/exec/BDDCompleteTraceExecutor.h"
#include "nusmv/core/trace/exec/SATCompleteTraceExecutor.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/wff/wff.h"
#include "nusmv/core/parser/parser.h"

#include <string.h>

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'Prop_private.h' for class 'Prop' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/* Used to encode that a property MIN/MAX has not yet been checked. */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define PROP_UNCHECKED -2

/* Used to encode the infinite distanca between two set of states in
   MIN/MAX properties */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define PROP_INFINITE -1

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define PROP_UNDEFINED -3


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void prop_finalize(Object_ptr object, void* dummy);
static Expr_ptr prop_get_expr_core_for_coi(const Prop_ptr self);
static node_ptr prop_is_ltl_convertible_to_invar(Prop_ptr self);
static node_ptr prop_is_ctl_convertible_to_invar(Prop_ptr self);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

Prop_ptr Prop_create(const NuSMVEnv_ptr env)
{
  Prop_ptr self = ALLOC(Prop, 1);
  PROP_CHECK_INSTANCE(self);

  prop_init(self, env);
  return self;
}

Prop_ptr Prop_create_partial(const NuSMVEnv_ptr env, Expr_ptr expr, Prop_Type type)
{
  Prop_ptr self = Prop_create(env);
  PROP_CHECK_INSTANCE(self);

  self->index = (unsigned int)-1;
  self->status = Prop_Unchecked;
  self->prop = expr;
  self->type = type;

  return self;
}

Prop_ptr Prop_copy(Prop_ptr input)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(input));

  Prop_ptr self = ALLOC(Prop, 1);
  PROP_CHECK_INSTANCE(self);

  prop_init(self, env);

  self->index = input->index;
  self->prop = input->prop;

  /* the cone is just referenced in the property */
  self->cone = input->cone;
  self->type = input->type;
  self->status = input->status;
  self->number = input->number;
  self->trace = input->trace;
  prop_set_scalar_sexp_fsm(self, input->scalar_fsm, true);
  prop_set_bool_sexp_fsm(self, input->bool_fsm, true);
  prop_set_bdd_fsm(self, input->bdd_fsm, true);
  prop_set_be_fsm(self, input->be_fsm, true);
  self->name = input->name;

  OVERRIDE(Object, finalize) = OBJECT(input)->finalize;

  OVERRIDE(Prop, get_expr) = input->get_expr;
  OVERRIDE(Prop, get_type_as_string) = input->get_type_as_string;
  OVERRIDE(Prop, print) = input->print;
  OVERRIDE(Prop, print_truncated) = input->print_truncated;
  OVERRIDE(Prop, print_db_tabular) = input->print_db_tabular;
  OVERRIDE(Prop, print_db_xml) = input->print_db_xml;
  OVERRIDE(Prop, verify) = input->verify;
  OVERRIDE(Prop, set_environment_fsms) = input->set_environment_fsms;

  return self;
}

Prop_ptr Prop_create_from_string(NuSMVEnv_ptr env, char* str, Prop_Type type)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  Prop_ptr self = NULL;
  node_ptr property = Nil;

  const char* argv[2];
  int argc = -1;
  int res = -1;

  nusmv_assert(str != (char*) NULL);

  argc = 2;
  argv[0] = NULL;
  argv[1] = str;

  /* sanity checks on type */
  switch (type) {
  case Prop_Ctl:
  case Prop_Ltl:
  case Prop_Psl:
  case Prop_Invar:
  case Prop_Compute:
    break;

  case Prop_CompId:
    StreamMgr_print_error(streams,  "Required to parse a property of Prop_CompId. "
                          "Use PropDb_prop_parse_name instead\n");
    error_unreachable_code();
    break;

  case Prop_NoType:
    StreamMgr_print_error(streams,  "Required to parse a property of unknonw type.\n");
    error_unreachable_code();
    return self;
    break;

  default:
    StreamMgr_print_error(streams,  "Required to parse a property of unsupported type.\n");
    return self;
    break;
  } /* switch */

  /* actual parsing */
  {
    node_ptr parsed_command = Nil;

    if (type != Prop_Psl) {
      const char* parsing_type = PropType_to_parsing_string(type);
      int parse_result = Parser_ReadCmdFromString(env,
                                                  argc, argv,
                                                  (char*) parsing_type,
                                                  ";\n", &parsed_command);

      if (parse_result != 0 || parsed_command == Nil) {
        StreamMgr_print_error(streams,
                              "Parsing error: expected an \"%s\" expression.\n",
                              PropType_to_string(type));
        return self;
      }
      property = car(parsed_command);
    }
    else {
      int parse_result = Parser_read_psl_from_string(env,
                                                     argc, argv,
                                                     &parsed_command);
      if (parse_result != 0 || parsed_command == Nil) {
        StreamMgr_print_error(streams,
                              "Parsing error: expected an \"%s\" expression.\n",
                              PropType_to_string(type));
        return self;
      }
      /* makes possible context absolute */
      if (node_get_type(parsed_command) == CONTEXT) {
        node_ptr new_ctx = CompileFlatten_concat_contexts(env, Nil, car(parsed_command));
        property = PslNode_new_context(nodemgr, new_ctx, cdr(parsed_command));
      }
      else {
        property = PslNode_new_context(nodemgr, NULL, parsed_command);
      }
    }
  }

  nusmv_assert(NULL != property);

  self = ALLOC(Prop, 1);
  PROP_CHECK_INSTANCE(self);

  prop_init(self, env);

  self->prop = property;
  self->type = type;

  return self;
}

void Prop_destroy(Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  Object_destroy(OBJECT(self), NULL);
}

VIRTUAL Expr_ptr Prop_get_expr(const Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  return self->get_expr(self);
}

Expr_ptr Prop_get_expr_core(const Prop_ptr self)
{
  Expr_ptr res;

  PROP_CHECK_INSTANCE(self);

  /* PSL formulae are converted to SMV LTL or CTL: */
  if (Prop_get_type(self) == Prop_Psl) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    res = PslNode_convert_psl_to_core(env, Prop_get_expr(self));
  }
  else res = Prop_get_expr(self); /* usual expression */

  /* We get rid of the LTL bounded operators */
  res = Compile_remove_ltl_bop(ENV_OBJECT(self)->environment, res);

  return Compile_pop_distrib_ops(ENV_OBJECT(self)->environment, res);
}

Expr_ptr Prop_get_flattened_expr(const Prop_ptr self, SymbTable_ptr st) {
  Expr_ptr res;

  res = Prop_get_expr_core(self);

  if (CONTEXT == node_get_type(res)) {
    /* The expression has to be flattened to remove the "CONTEXT" and
       properly contextualize the symbols in the expression */
    res = Compile_FlattenSexp(st, cdr(res), car(res));
  }
  return res;
}

Expr_ptr Prop_get_expr_core_for_coi(const Prop_ptr self)
{
  return prop_get_expr_core_for_coi(self);
}

Set_t Prop_get_cone(const Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);

  return self->cone;
}

void Prop_set_cone(Prop_ptr self, Set_t cone)
{
  PROP_CHECK_INSTANCE(self);

  self->cone = cone;
}

Prop_Type Prop_get_type(const Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);

  return self->type ;
}

Prop_Status Prop_get_status(const Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);

  return self->status;
}

void Prop_set_status(Prop_ptr self, Prop_Status s)
{
  PROP_CHECK_INSTANCE(self);
  self->status = s;
}

int Prop_get_number(const Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  return self->number;
}

void Prop_set_number(Prop_ptr self, int n)
{
  PROP_CHECK_INSTANCE(self);
  self->number = n;
}

void Prop_set_number_infinite(Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  self->number = PROP_INFINITE;
}

void Prop_set_number_undefined(Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  self->number = PROP_UNDEFINED;
}

int Prop_get_trace(const Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  return self->trace;
}

void Prop_set_trace(Prop_ptr self, int t)
{
  PROP_CHECK_INSTANCE(self);
  self->trace = t;
}

int Prop_get_index(const Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  return self->index;
}

void Prop_set_index(Prop_ptr self, const int index)
{
  PROP_CHECK_INSTANCE(self);
  self->index = index;
}

node_ptr Prop_get_name(const Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  return (self->name);
}

char* Prop_get_name_as_string(const Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
    return sprint_node(wffprint, self->name);
  }
}

void Prop_set_name(const Prop_ptr self, const node_ptr name)
{
  PROP_CHECK_INSTANCE(self);
  self->name = name;
}

SymbTable_ptr Prop_get_symb_table(const Prop_ptr self)
{
  SymbTable_ptr result;

  PROP_CHECK_INSTANCE(self);

  if (NULL != Prop_get_bdd_fsm(self)) {
    result = BaseEnc_get_symb_table(
        BASE_ENC(BddFsm_get_bdd_encoding(Prop_get_bdd_fsm(self))));
  }
  else if (NULL != Prop_get_be_fsm(self)) {
    result = BaseEnc_get_symb_table(
        BASE_ENC(BeFsm_get_be_encoding(Prop_get_be_fsm(self))));
  }
  else if (NULL != Prop_get_scalar_sexp_fsm(self)) {
    result = SexpFsm_get_symb_table(Prop_get_scalar_sexp_fsm(self));
  }
  else {
    result = SYMB_TABLE(NULL);
  }

  return result;
}

void Prop_set_environment_fsms(const NuSMVEnv_ptr env, Prop_ptr self)
{
  self->set_environment_fsms(env, self);
}

SexpFsm_ptr Prop_compute_ground_sexp_fsm (const NuSMVEnv_ptr env,
                                          const Prop_ptr self)
{
  SexpFsm_ptr res = SEXP_FSM(NULL);
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  /* setup ground FSM taking COI into account */
  if (opt_cone_of_influence(opts)) {
    Prop_apply_coi_for_scalar(env, self);
  }

  /* COI not applied, and the property contains no FSMs. */
  if (SEXP_FSM(NULL) == Prop_get_scalar_sexp_fsm(self)) {
    Prop_set_environment_fsms(env, self);
  }

  /* at this point a private ground FSM exists */
  res = Prop_get_scalar_sexp_fsm(self);
  SEXP_FSM_CHECK_INSTANCE(res);

  return res;
}

BddFsm_ptr Prop_compute_ground_bdd_fsm (const NuSMVEnv_ptr env,
                                        const Prop_ptr self)
{
  BddFsm_ptr res = BDD_FSM(NULL);
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  /* setup ground FSM taking COI into account */
  if (opt_cone_of_influence(opts) == true) {
    Prop_apply_coi_for_bdd(env, self);
  }

  if (BDD_FSM(NULL) == Prop_get_bdd_fsm(self)) {
    Prop_set_environment_fsms(env, self);
  }

  /* at this point a private ground FSM exists */
  res = Prop_get_bdd_fsm(self);
  BDD_FSM_CHECK_INSTANCE(res);

  return res;
}

BeFsm_ptr Prop_compute_ground_be_fsm (const NuSMVEnv_ptr env,
                                      const Prop_ptr self)
{
  BeFsm_ptr res = BE_FSM(NULL);
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  /* setup ground FSM taking COI into account */
  if (opt_cone_of_influence(opts) == true) {
    Prop_apply_coi_for_bmc(env, self);
  }

  if (BE_FSM(NULL) == Prop_get_be_fsm(self)) {
    Prop_set_environment_fsms(env, self);
  }

  /* at this point a private ground FSM exists */
  res = Prop_get_be_fsm(self);
  BE_FSM_CHECK_INSTANCE(res);

  return res;
}

SexpFsm_ptr Prop_get_scalar_sexp_fsm(const Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  return self->scalar_fsm;
}

void Prop_set_scalar_sexp_fsm(Prop_ptr self, SexpFsm_ptr fsm)
{
  PROP_CHECK_INSTANCE(self);
  prop_set_scalar_sexp_fsm(self, fsm, true);
}

BoolSexpFsm_ptr Prop_get_bool_sexp_fsm(const Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  return self->bool_fsm;
}

void Prop_set_bool_sexp_fsm(Prop_ptr self, BoolSexpFsm_ptr fsm)
{
  PROP_CHECK_INSTANCE(self);
  prop_set_bool_sexp_fsm(self, fsm, true);
}

BddFsm_ptr Prop_get_bdd_fsm(Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  return self->bdd_fsm;
}

void Prop_set_bdd_fsm(Prop_ptr self, BddFsm_ptr fsm)
{
  PROP_CHECK_INSTANCE(self);
  prop_set_bdd_fsm(self, fsm, true);
}

BeFsm_ptr Prop_get_be_fsm(const Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  return self->be_fsm;
}

void Prop_set_be_fsm(Prop_ptr self, BeFsm_ptr fsm)
{
  PROP_CHECK_INSTANCE(self);
  prop_set_be_fsm(self, fsm, true);
}


/*!
  \brief  Check if the given property needs rewriting to be
                       checked

   Returns true if the property needs rewriting,
                       false otherwise
*/

/* TODO[AMa] This function is not reentrant! (and many others in this
   file) */
boolean Prop_needs_rewriting(SymbTable_ptr st, const Prop_ptr self)
{
  Set_t cone;
  boolean result;
  node_ptr expression;

  PROP_CHECK_INSTANCE(self);

  if (Prop_Psl == self->type) {
    result = false;
  }
  else {
    result = Wff_Rewrite_is_rewriting_needed(st, Prop_get_expr(self), Nil);
  }

  return result;
}

Set_t Prop_compute_cone(Prop_ptr self,
                        FlatHierarchy_ptr hierarchy,
                        SymbTable_ptr symb_table)
{
  Set_t cone;
  PROP_CHECK_INSTANCE(self);

  /* The point here is not to apply this to game properties. Hence, in
     general, this may be too restrictive and might have to be
     removed. */
  nusmv_assert(Prop_Prop_Type_First < Prop_get_type(self) &&
               Prop_Prop_Type_Last > Prop_get_type(self));

  {
    /* Here it is sufficient to call the prop_get_expr_core_for_coi
       that just remopved forall replicator instead of calling the
       Prop_get_expr that also try to convert the expression in LTL if
       possible and rises an error if the conversion is not
       possible */
    node_ptr spec = prop_get_expr_core_for_coi(self);

    /* Here we need to consider also the possible function occurring
       in the property */
    Set_t spec_dep = Formulae_GetDependenciesByType(symb_table, spec,
                                                    FlatHierarchy_get_justice(hierarchy),
                                                    FlatHierarchy_get_compassion(hierarchy),
                                                    VFT_CNIF | VFT_FUNCTION, false);

    cone = ComputeCOI(symb_table, hierarchy, spec_dep);
  }

  return cone;
}

void Prop_apply_coi_for_scalar(const NuSMVEnv_ptr env, Prop_ptr self)
{
  SexpFsm_ptr scalar_fsm;

  SymbTable_ptr symb_table = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  FlatHierarchy_ptr hierarchy = FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));
  FsmBuilder_ptr builder = FSM_BUILDER(NuSMVEnv_get_value(env, ENV_FSM_BUILDER));


  PROP_CHECK_INSTANCE(self);

  /* The point here is not to apply this to game properties. Hence, in
     general, this may be too restrictive and might have to be
     removed. */
  nusmv_assert(Prop_Prop_Type_First < Prop_get_type(self) &&
               Prop_Prop_Type_Last > Prop_get_type(self));

  /* scalar sexp fsm */
  {
    Set_t cone = Prop_compute_cone(self, hierarchy, symb_table);

    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Using cone of influence\n");
    }

    scalar_fsm = FsmBuilder_create_scalar_sexp_fsm(builder, hierarchy, cone);

    Prop_set_cone(self, cone);
    prop_set_scalar_sexp_fsm(self, scalar_fsm, false); /* does not dup */

  }
}

void Prop_apply_coi_for_bdd(const NuSMVEnv_ptr env, Prop_ptr self)
{
  FsmBuilder_ptr helper = FSM_BUILDER(NuSMVEnv_get_value(env, ENV_FSM_BUILDER));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  SexpFsm_ptr scalar_fsm;
  BddFsm_ptr  bdd_fsm;
  boolean applied = false;

  PROP_CHECK_INSTANCE(self);

  /* The point here is not to apply this to game properties. Hence, in
     general, this may be too restrictive and might have to be
     removed. */
  nusmv_assert(Prop_Prop_Type_First < Prop_get_type(self) &&
               Prop_Prop_Type_Last > Prop_get_type(self));

  scalar_fsm = Prop_get_scalar_sexp_fsm(self);
  bdd_fsm    = Prop_get_bdd_fsm(self);

  /* scalar sexp fsm does not exist in the property. */
  if (scalar_fsm == SEXP_FSM(NULL)) {
    Prop_apply_coi_for_scalar(env, self);
    scalar_fsm = Prop_get_scalar_sexp_fsm(self);

    applied = true;
  }

  /* bdd fsm */
  if (bdd_fsm == BDD_FSM(NULL)) {
    BddEnc_ptr bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
    bdd_fsm = FsmBuilder_create_bdd_fsm(helper, bdd_enc, scalar_fsm,
                                        get_partition_method(opts));
    prop_set_bdd_fsm(self, bdd_fsm, false); /* does not dup */
    applied = true;
  }

  if (! applied) {
    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Using previously built model for COI...\n");
    }
  }
}

void Prop_apply_coi_for_bmc(const NuSMVEnv_ptr env, Prop_ptr self)
{
  FsmBuilder_ptr helper = FSM_BUILDER(NuSMVEnv_get_value(env, ENV_FSM_BUILDER));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  BeEnc_ptr be_enc = BE_ENC(NuSMVEnv_get_value(env, ENV_BE_ENCODER));
  BoolSexpFsm_ptr bool_fsm;
  BeFsm_ptr be_fsm;

  PROP_CHECK_INSTANCE(self);

  /* The point here is not to apply this to game properties. Hence, in
     general, this may be too restrictive and might have to be
     removed. */
  nusmv_assert(Prop_Prop_Type_First < Prop_get_type(self) &&
               Prop_Prop_Type_Last > Prop_get_type(self));

  bool_fsm = Prop_get_bool_sexp_fsm(self);
  be_fsm = Prop_get_be_fsm(self);

  /* boolean sexp fsm */
  if (BOOL_SEXP_FSM(NULL) == bool_fsm) {
    SymbTable_ptr symb_table = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
    FlatHierarchy_ptr hierarchy = FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));

    SymbLayer_ptr det_layer;
    int layer_name_dim;
    char* layer_name = (char*) NULL;
    int c;

    Set_t cone = Prop_compute_cone(self, hierarchy, symb_table);

    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Using cone of influence\n");
    }

    layer_name_dim = strlen(DETERM_LAYER_NAME) + 6;
    layer_name = ALLOC(char, layer_name_dim);
    nusmv_assert(layer_name != (char*) NULL);

    c = snprintf(layer_name, layer_name_dim, "%s_%03d",
                 DETERM_LAYER_NAME, Prop_get_index(self));
    SNPRINTF_CHECK(c, layer_name_dim);

    det_layer = SymbTable_create_layer(symb_table, layer_name,
                                       SYMB_LAYER_POS_BOTTOM);

    {  /* commits the layer: */

      /* BddEnc is required as bdds are used when producing
         counter-examples: */
      BddEnc_ptr bdd_enc;
      BoolEnc_ptr bool_enc;

      bool_enc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(be_enc));
      bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));

      /* creates the boolean FSM */
      bool_fsm = FsmBuilder_create_boolean_sexp_fsm(helper,
                                                    hierarchy,
                                                    cone,
                                                    bdd_enc,
                                                    det_layer);

      BaseEnc_commit_layer(BASE_ENC(bool_enc), layer_name);
      BaseEnc_commit_layer(BASE_ENC(be_enc), layer_name);
      BaseEnc_commit_layer(BASE_ENC(bdd_enc), layer_name);
    }

    Prop_set_cone(self, cone);
    prop_set_bool_sexp_fsm(self, bool_fsm, false); /* does not dup */

    /* creates the BE FSM */
    nusmv_assert(be_fsm == BE_FSM(NULL));

    /* Notice that currently a single variable manager instance
       exists, and it is handled by the BMC package as a public global
       variable. Current implementation is temporary kept in this
       format. */
    be_fsm = BeFsm_create_from_sexp_fsm(be_enc, bool_fsm);
    prop_set_be_fsm(self, be_fsm, false); /* does not dup */

    FREE(layer_name);
  }
  else {
    if (be_fsm == BE_FSM(NULL)) {
      if (opt_verbose_level_gt(opts, 0)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "BeFsm was found unexpectedly to be constructed\n");
      }

      /* For some reason bool fsm is existing, but befsm is not. Make it */
      be_fsm = BeFsm_create_from_sexp_fsm(be_enc, bool_fsm);
      prop_set_be_fsm(self, be_fsm, false); /* does not dup */
    }
    else {
      if (opt_verbose_level_gt(opts, 0)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "Using previously built model for COI...\n");
      }
    }
  }
}

void Prop_destroy_coi_for_bmc(Prop_ptr self)
{
  int layer_name_dim;
  char* layer_name;
  BeFsm_ptr be_fsm;
  BoolEnc_ptr bool_enc;
  BddEnc_ptr bdd_enc;
  BeEnc_ptr be_enc;
  NuSMVEnv_ptr env;
  SymbTable_ptr symb_table;
  int c;

  PROP_CHECK_INSTANCE(self);
  nusmv_assert(Prop_get_bool_sexp_fsm(self) != BOOL_SEXP_FSM(NULL));
  nusmv_assert(Prop_get_be_fsm(self) != BE_FSM(NULL));

  be_fsm = Prop_get_be_fsm(self);

  /* The point here is not to apply this to game properties. Hence, in
     general, this may be too restrictive and might have to be
     removed. */
  nusmv_assert(Prop_Prop_Type_First < Prop_get_type(self) &&
               Prop_Prop_Type_Last > Prop_get_type(self));

  layer_name_dim = strlen(DETERM_LAYER_NAME) + 6;
  layer_name = ALLOC(char, layer_name_dim);
  nusmv_assert(layer_name != (char*) NULL);

  c = snprintf(layer_name, layer_name_dim, "%s_%03d",
               DETERM_LAYER_NAME, Prop_get_index(self));
  SNPRINTF_CHECK(c, layer_name_dim);

  env = ENV_OBJECT(self)->environment;

  be_enc = BeFsm_get_be_encoding(be_fsm);
  symb_table = BaseEnc_get_symb_table(BASE_ENC(be_enc));

  bdd_enc = BDD_ENC(NuSMVEnv_get_value(env, ENV_BDD_ENCODER));
  bool_enc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(be_enc));

  nusmv_assert(BaseEnc_layer_occurs(BASE_ENC(be_enc), layer_name));
  BaseEnc_remove_layer(BASE_ENC(be_enc), layer_name);
  nusmv_assert(BaseEnc_layer_occurs(BASE_ENC(bdd_enc), layer_name));
  BaseEnc_remove_layer(BASE_ENC(bdd_enc), layer_name);
  nusmv_assert(BaseEnc_layer_occurs(BASE_ENC(bool_enc), layer_name));
  BaseEnc_remove_layer(BASE_ENC(bool_enc), layer_name);
  SymbTable_remove_layer(symb_table,
                         SymbTable_get_layer(symb_table, layer_name));

  FREE(layer_name);
}

char* Prop_get_number_as_string(const Prop_ptr self)
{
  char buf[16];
  char* ret = NULL;
  int n, c = 0;

  PROP_CHECK_INSTANCE(self);

  nusmv_assert(Prop_get_type(self) == Prop_Compute); /* compute type only */

  n = Prop_get_number(self);
  if (n == PROP_UNCHECKED) c = snprintf(buf, 16, "Unchecked");
  else if (n == PROP_INFINITE) c = snprintf(buf, 16, "Infinite");
  else if (n == PROP_UNDEFINED) c = snprintf(buf, 16, "Undefined");
  else c = snprintf(buf, 16, "%d", n);

  SNPRINTF_CHECK(c, 16);

  ret = ALLOC(char, strlen(buf)+sizeof(char));
  nusmv_assert(ret != NULL);

  strcpy(ret, buf);
  return ret;
}

char* Prop_get_context_text(const Prop_ptr self)
{
  char* cntx = (char *)NULL;
  char* EMTPY_CONTEXT_STR = "Main";
  node_ptr context;
  PROP_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    context = (node_ptr) self->prop;

    if (node_get_type(context) == CONTEXT) {
      context = car(context);
      if (context != Nil) {
        cntx = sprint_node(wffprint, context);
      }
      else {
        cntx = ALLOC(char, strlen(EMTPY_CONTEXT_STR)+1);
        nusmv_assert(cntx != NULL);
        strcpy(cntx, EMTPY_CONTEXT_STR);
      }
    }
    else {
      cntx = ALLOC(char, strlen(EMTPY_CONTEXT_STR)+1);
      nusmv_assert(cntx != NULL);
      strcpy(cntx, EMTPY_CONTEXT_STR);
    }
  }

  return cntx;
}

char* Prop_get_text(const Prop_ptr self)
{
  node_ptr p;

  PROP_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    p = (node_ptr) Prop_get_expr(self);
    if (node_get_type(p) == CONTEXT) p = cdr(p);  /* skip context */

    return sprint_node(wffprint, p);
  }
}

VIRTUAL const char* Prop_get_type_as_string(Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  return self->get_type_as_string(self);
}

const char* Prop_get_status_as_string(const Prop_ptr self)
{
  char* res = (char*) NULL;
  Prop_Status t;

  PROP_CHECK_INSTANCE(self);

  t = Prop_get_status(self);

  switch (t) {
  case Prop_NoStatus:    res = PROP_NOSTATUS_STRING; break;
  case Prop_Unchecked:   res = PROP_UNCHECKED_STRING; break;
  case Prop_True:        res = PROP_TRUE_STRING; break;
  case Prop_False:       res = PROP_FALSE_STRING; break;
  case Prop_Number:      res = PROP_NUMBER_STRING; break;

  default:  error_unreachable_code(); /* invalid status */
  }

  return res;
}

int Prop_check_type(const Prop_ptr self, Prop_Type type)
{
  PROP_CHECK_INSTANCE(self);

  if (Prop_get_type(self) != type) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const StreamMgr_ptr streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

    if (Prop_Prop_Type_First < type && Prop_Prop_Type_Last > type) {
      StreamMgr_print_error(streams,
              "Error: specified property type is %s, "
              "but type %s was expected.\n",
              Prop_get_type_as_string(self), PropType_to_string(type));
    } else {
      StreamMgr_print_error(streams,
              "Error: specified property type is %s, "
              "but a different type (%d) was expected.\n",
              Prop_get_type_as_string(self), type);
    }
    return 1;
  }

  return 0;
}

VIRTUAL void Prop_verify(Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  self->verify(self);
}

VIRTUAL void Prop_print(const Prop_ptr self, OStream_ptr file, Prop_PrintFmt fmt)
{
  PROP_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    switch (fmt) {
    case PROP_PRINT_FMT_NAME:
      if (Nil != self->name) {
        OStream_nprintf(file, wffprint, "'%N'", self->name);
        break;
      }
      /* Else default on next */

    case PROP_PRINT_FMT_INDEX:
      /* TODO[MD] useless check, index is unsigned */
      if (-1 != self->index) {
        OStream_printf(file, "[%d] ", self->index);
        break;
      }
      /* Else default on next */

    case PROP_PRINT_FMT_FORMULA_TRUNC:
      self->print_truncated(self, file);
      break;

    case PROP_PRINT_FMT_FORMULA:
      self->print(self, file);
      break;

    default: error_unreachable_code();
    }
  }
}

VIRTUAL void Prop_print_db(const Prop_ptr self, OStream_ptr file,
                           PropDb_PrintFmt fmt)
{
  PROP_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    switch (fmt) {
    case PROPDB_PRINT_FMT_TABULAR:
      self->print_db_tabular(self, file);
      break;
    case PROPDB_PRINT_FMT_XML:
      self->print_db_xml(self, file);
      break;
    default:
      ErrorMgr_internal_error(errmgr, "Unsupported print format");
    }
  }
}

boolean Prop_is_psl_ltl(const Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    return (Prop_get_type(self) == Prop_Psl) &&
      PslNode_is_handled_psl(env, PslNode_remove_forall_replicators(env, self->prop));
  }
}

boolean Prop_is_psl_obe(const Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  return (Prop_get_type(self) == Prop_Psl) && PslNode_is_obe(self->prop);
}

Set_t Prop_set_from_formula_list(NuSMVEnv_ptr env, node_ptr list, Prop_Type type)
{
  Set_t retval;
  node_ptr iter;

  nusmv_assert(Nil == list || CONS == node_get_type(list));

  retval = Set_MakeEmpty();

  NODE_CONS_LIST_FOREACH(list, iter) {
    node_ptr element;
    Prop_ptr prop;

    element = car(iter);

    prop = Prop_create_partial(env, element, type);

    retval = Set_AddMember(retval, (Set_Element_t)prop);
  }

  return retval;
}

Prop_ptr Prop_convert_to_invar(Prop_ptr self)
{
  PROP_CHECK_INSTANCE(self);
  return self->convert_to_invar(self);
}

/*!
  \brief Convert an ltl to an invarspec, if possible

  Convert an ltl to an invarspec, if possible
*/

static node_ptr prop_is_ltl_convertible_to_invar(Prop_ptr self)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  SymbTable_ptr const symb_table =
    SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
  NodeMgr_ptr const nodemgr =
     NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr expr = NULL;
  node_ptr wff = NULL;
  node_ptr context = NULL;
  node_ptr retval = NULL;

  nusmv_assert(Prop_Ltl == Prop_get_type(self));

  expr = Prop_get_expr(self);
  nusmv_assert(CONTEXT == node_get_type(expr));

  context = car(expr);
  wff = cdr(expr);

  if (OP_GLOBAL == node_get_type(wff)) {
    retval = car(wff);
    if (! Wff_is_propositional(symb_table, retval, context, true)) retval = NULL;
    else retval = find_node(nodemgr, CONTEXT, context, retval);
  }
  else retval = NULL;

  return retval;
}

/*!
  \brief Convert a ctl to an invarspec, if possible

  Convert a ctl to an invarspec, if possible
*/

static node_ptr prop_is_ctl_convertible_to_invar(Prop_ptr self)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  SymbTable_ptr const symb_table =
    SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
  NodeMgr_ptr const nodemgr =
     NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr expr = NULL;
  node_ptr wff = NULL;
  node_ptr context = NULL;
  node_ptr retval = NULL;

  nusmv_assert(Prop_Ctl == Prop_get_type(self));

  expr = Prop_get_expr(self);
  nusmv_assert(CONTEXT == node_get_type(expr));

  context = car(expr);
  wff = cdr(expr);

  if (AG == node_get_type(wff)) {
    retval = car(wff);
    if (! Wff_is_propositional(symb_table, retval, context, true)) retval = NULL;
    else retval = find_node(nodemgr, CONTEXT, context, retval);
  }
  else retval = NULL;

  return retval;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void prop_init(Prop_ptr self, const NuSMVEnv_ptr env)
{
  /* base class initialization */
  env_object_init(ENV_OBJECT(self), env);


  /* members initialization */
  self->index = 0;
  self->prop = EXPR(NULL);
  self->cone = (Set_t) Set_MakeEmpty();
  self->type = Prop_NoType;
  self->status = Prop_NoStatus;
  self->number = PROP_UNCHECKED;
  self->trace = 0;
  self->scalar_fsm = SEXP_FSM(NULL);
  self->bool_fsm = BOOL_SEXP_FSM(NULL);
  self->bdd_fsm = BDD_FSM(NULL);
  self->be_fsm = (BeFsm_ptr) NULL;
  self->name = Nil;

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = prop_finalize;

  OVERRIDE(Prop, get_expr) = prop_get_expr;
  OVERRIDE(Prop, get_type_as_string) = prop_get_type_as_string;
  OVERRIDE(Prop, print) = prop_print;
  OVERRIDE(Prop, print_truncated) = prop_print_truncated;
  OVERRIDE(Prop, print_db_tabular) = prop_print_db_tabular;
  OVERRIDE(Prop, print_db_xml) = prop_print_db_xml;
  OVERRIDE(Prop, verify) = prop_verify;
  OVERRIDE(Prop, set_environment_fsms) = prop_set_environment_fsms;
  OVERRIDE(Prop, convert_to_invar) = prop_convert_to_invar;
}

void prop_deinit(Prop_ptr self)
{
  /* members deinitialization */
  {
    if (self->be_fsm != NULL) BeFsm_destroy(self->be_fsm);
    if (self->bdd_fsm != BDD_FSM(NULL)) BddFsm_destroy(self->bdd_fsm);
    if (self->bool_fsm != BOOL_SEXP_FSM(NULL)) BoolSexpFsm_destroy(self->bool_fsm);
    if (self->scalar_fsm != SEXP_FSM(NULL)) SexpFsm_destroy(self->scalar_fsm);
  }

  /* base class deinitialization */
  env_object_deinit(ENV_OBJECT(self));
}


/*!
  \brief Returns the property as it has been parsed and created

  Returns the property stored in the prop. If the
  property is PSL, the result should be converted to core symbols
  before model checking (see Prop_get_expr_core or
  PslNode_convert_psl_to_core).

  \sa Prop_get_expr_core
*/

Expr_ptr prop_get_expr(const Prop_ptr self)
{
  return self->prop;
}


/*!
  \brief  Returns the a string associated to the propertys type.

   Returns the string corresponding to the propertys type
                for printing it. Returned string must NOT be
                deleted.

  \se

  \sa
*/

const char* prop_get_type_as_string(const Prop_ptr self)
{
  return PropType_to_string(Prop_get_type(self));
}


/*!
  \brief Prints a property

  Prints a property. PSL properties are specially
  handled.
*/

void prop_print(const Prop_ptr self, OStream_ptr file)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  node_ptr p;
  node_ptr context;

  p = Prop_get_expr(self);
  context = Nil;

  if (p != Nil && node_get_type(p) == CONTEXT) {
    context = car(p);
    p = cdr(p);
  }

  OStream_nprintf(file, wffprint, "%N ", p);

  if (context != Nil) {
    OStream_nprintf(file, wffprint, "IN %N", context);
  }
}

/*!
  \brief Prints a property

  Prints a property. PSL properties are specially
  handled. The formula is truncated after the first 40 characters
*/

void prop_print_truncated(const Prop_ptr self, OStream_ptr file)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  node_ptr p;
  node_ptr context;
  char* prop;
  int len;

  p = Prop_get_expr(self);
  context = Nil;

  if (p != Nil && node_get_type(p) == CONTEXT) {
    context = car(p);
    p = cdr(p);
  }

  prop = sprint_node(wffprint, p);
  len = strlen(prop);

  if (len > 40) {
    prop[40] = '\0';
  }

  OStream_printf(file, prop);

  if (len > 40) {
    OStream_printf(file, " [...]");
  }

  FREE(prop);

  if (context != Nil) {
    OStream_nprintf(file, wffprint, "IN %N", context);
  }
}


/*!
  \brief Prints a property with info or its position and status
  within the database

  Prints a property on the specified FILE
  stream. Some of the information stored in the property structure are
  printed out (e.g. property, property kind, property status, ...).
*/

void prop_print_db_tabular(const Prop_ptr self, OStream_ptr file)
{
  PROP_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    OStream_printf(file, "%.3d :", self->index);
    prop_print(self, file);
    OStream_printf(file, "\n");

    OStream_printf(file, "  [%-15s", Prop_get_type_as_string(self));
    if (self->type == Prop_Compute) {
      char* str_number = Prop_get_number_as_string(self);
      OStream_printf(file, "%-15s", str_number);
      FREE(str_number);
    }
    else OStream_printf(file, "%-15s", Prop_get_status_as_string(self));

    if (self->trace == 0) OStream_printf(file, "N/A    ");
    else OStream_printf(file, "%-7d", self->trace);

    if (Nil != self->name) {
      OStream_nprintf(file, wffprint, "%N", self->name);
    }
    else {
      OStream_printf(file, "N/A");
    }
    OStream_printf(file, "]\n");
  }
}


/*!
  \brief Prints a property with info or its position and status
  within the database, in XML format

  Prints a property on the specified FILE stream, in XML
  format. Some of the information stored in the property structure are
  printed out (e.g. property, property kind, property status, ...).
*/

void prop_print_db_xml(const Prop_ptr self, OStream_ptr file)
{
  FILE* out = OStream_get_stream(file);
  char* str;

  PROP_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    OStream_printf(file, "  <property>\n");

    OStream_printf(file, "    <name>");
    if (Nil != self->name) {
      str = sprint_node(wffprint, self->name);
      Utils_str_escape_xml_file(str, out);
      FREE(str);
    }
    OStream_printf(file, "</name>\n");

    OStream_printf(file, "    <index>%d</index>\n", self->index);

    OStream_printf(file, "    <formula><![CDATA[\n");
    Prop_print(self, file, PROP_PRINT_FMT_FORMULA);
    OStream_printf(file, "\n]]>\n");
    OStream_printf(file, "</formula>\n");

    OStream_printf(file, "    <type>");
    Utils_str_escape_xml_file(Prop_get_type_as_string(self), out);
    OStream_printf(file, "</type>\n");

    OStream_printf(file, "    <status>");
    switch (Prop_get_status(self)) {
    case Prop_NoStatus:  OStream_printf(file, "UNKNOWN"); break;
    case Prop_Unchecked: OStream_printf(file, "UNCHECKED"); break;
    case Prop_True:      OStream_printf(file, "TRUE"); break;
    case Prop_False:     OStream_printf(file, "FALSE"); break;
    case Prop_Number:    OStream_printf(file, "NUMBER"); break;
    default:             error_unreachable_code(); /* invalid status */
    }
    OStream_printf(file, "</status>\n");

    OStream_printf(file, "    <bound>-1</bound>\n");

    OStream_printf(file, "    <trace>%d</trace>\n", self->trace);

    OStream_printf(file, " </property>\n\n");
  }
}


/*!
  \brief Verifies a given property

  Depending the property, different model checking
                      algorithms are called. The status of the
                      property is updated accordingly to the result
                      of the verification process.
*/

void prop_verify(Prop_ptr self)
{
  const NuSMVEnv_ptr env = ENV_OBJECT(self)->environment;
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (Prop_get_status(self) == Prop_Unchecked)  {
    switch (Prop_get_type(self)) {

    case Prop_Ctl:
      if (opt_ag_only(opts)) {
        if ( opt_forward_search(opts)) { Mc_CheckAGOnlySpec(env, self); }
        else {
          /* Cannot use AG-only since reachables must be calculated before */
          ErrorMgr_warning_ag_only_without_reachables(errmgr);
          Mc_CheckCTLSpec(env, self);
        }
      }
      else { Mc_CheckCTLSpec(env, self); }
      break;

    case Prop_Compute:  Mc_CheckCompute(env, self); break;

    case Prop_Invar:    Mc_CheckInvar(env, self); break;

    case Prop_Ltl:      Ltl_CheckLtlSpec(env, self); break;

    case Prop_Psl:
      if (Prop_is_psl_ltl(self)) { Ltl_CheckLtlSpec(env, self); }
      else {
        if (Prop_is_psl_obe(self)) {
          if (opt_ag_only(opts)) {
            if ( opt_forward_search(opts)) { Mc_CheckAGOnlySpec(env, self); }
            else {
              /* Cannot use AG-only since reachables must be calculated before */
              ErrorMgr_warning_ag_only_without_reachables(errmgr);
              Mc_CheckCTLSpec(env, self);
            }
          }
          else { Mc_CheckCTLSpec(env, self); }
        }
        else { ErrorMgr_error_psl_not_supported_feature(errmgr); }
      }
      break;

    default:  error_unreachable_code(); /* invalid type */
    }
  }
}

const char* PropType_to_string(const Prop_Type type)
{
  char* res = (char*) NULL;

  switch (type) {
  case Prop_NoType:  res = PROP_NOTYPE_STRING; break;
  case Prop_Ctl:     res = PROP_CTL_STRING; break;
  case Prop_Ltl:     res = PROP_LTL_STRING; break;
  case Prop_Psl:     res = PROP_PSL_STRING; break;
  case Prop_Invar:   res = PROP_INVAR_STRING; break;
  case Prop_Compute: res = PROP_COMPUTE_STRING; break;
  case Prop_CompId: error_unreachable_code_msg("COMPID unhandled!\n");
  default: error_unreachable_code();
  }

  return res;
}

short int PropType_to_node_type(const Prop_Type type)
{
  short int res = 0;

  switch (type) {
  case Prop_Ctl:     res = SPEC; break;
  case Prop_Ltl:     res = LTLSPEC; break;
  case Prop_Psl:     res = PSLSPEC; break;
  case Prop_Invar:   res = INVARSPEC; break;
  case Prop_Compute: res = COMPUTE; break;

  case Prop_CompId: error_unreachable_code_msg("COMPID unhandled!\n");

  case Prop_NoType:
  default:
    error_unreachable_code();
  }

  return res;
}

const char* PropType_to_parsing_string(const Prop_Type type)
{
  switch (type) {
  case Prop_NoType: break; /* to suppress compiler's warnings */
  case Prop_Ctl: return "CTLWFF ";
  case Prop_Ltl: return "LTLWFF ";
  case Prop_Psl: return "PSLWFF ";
  case Prop_Invar: return "NEXTWFF ";
  case Prop_Compute: return "COMPWFF ";
  case Prop_CompId:  return "COMPID ";
  default:  error_unreachable_code();
  }

  return "SIMPWFF ";
}


/*!
  \brief


*/

void prop_set_scalar_sexp_fsm(Prop_ptr self, SexpFsm_ptr fsm,
                              const boolean duplicate)
{
  if (self->scalar_fsm != SEXP_FSM(NULL)) {
    SexpFsm_destroy(self->scalar_fsm);
  }
  if (duplicate && (fsm != SEXP_FSM(NULL))) {
    self->scalar_fsm = SexpFsm_copy(fsm);
  }
  else {
    self->scalar_fsm = fsm;
  }
}


/*!
  \brief


*/

void prop_set_bool_sexp_fsm(Prop_ptr self, BoolSexpFsm_ptr fsm,
                            const boolean duplicate)
{
  if (self->bool_fsm != BOOL_SEXP_FSM(NULL)) {
    BoolSexpFsm_destroy(self->bool_fsm);
  }
  if (duplicate && (fsm != BOOL_SEXP_FSM(NULL))) {
    self->bool_fsm = BOOL_SEXP_FSM(SexpFsm_copy(SEXP_FSM(fsm)));
  }
  else self->bool_fsm = fsm;
}


/*!
  \brief


*/

void prop_set_bdd_fsm(Prop_ptr self, BddFsm_ptr fsm, const boolean duplicate)
{
  if (self->bdd_fsm != BDD_FSM(NULL)) BddFsm_destroy(self->bdd_fsm);
  if (duplicate && (fsm != BDD_FSM(NULL))) {
    self->bdd_fsm = BddFsm_copy(fsm);
  }
  else self->bdd_fsm = fsm;
}

void prop_set_environment_fsms(const NuSMVEnv_ptr env, Prop_ptr prop)
{
  if (NuSMVEnv_has_value(env, ENV_SEXP_FSM)) {
    Prop_set_scalar_sexp_fsm(prop, SEXP_FSM(NuSMVEnv_get_value(env, ENV_SEXP_FSM)));
  }
  if (NuSMVEnv_has_value(env, ENV_BOOL_FSM)) {
    Prop_set_bool_sexp_fsm(prop, BOOL_SEXP_FSM(NuSMVEnv_get_value(env, ENV_BOOL_FSM)));
  }
  if (NuSMVEnv_has_value(env, ENV_BDD_FSM)) {
    Prop_set_bdd_fsm(prop, BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM)));
  }
  if (NuSMVEnv_has_value(env, ENV_BE_FSM)) {
    Prop_set_be_fsm(prop, BE_FSM(NuSMVEnv_get_value(env, ENV_BE_FSM)));
  }
}

/*!
  \brief


*/

void prop_set_be_fsm(Prop_ptr self, BeFsm_ptr fsm, const boolean duplicate)
{
  if (self->be_fsm != (BeFsm_ptr) NULL) BeFsm_destroy(self->be_fsm);
  if (duplicate && (fsm != (BeFsm_ptr) NULL)) {
    self->be_fsm = BeFsm_copy(fsm);
  }
  else self->be_fsm = fsm;
}

Prop_ptr prop_convert_to_invar(Prop_ptr self)
{
  const NuSMVEnv_ptr env = ENV_OBJECT(self)->environment;
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  Prop_ptr retval = NULL;
  node_ptr expr = NULL;

#ifdef DEBUG_CONVERT_PROPERTY_TO_INVAR
  Logger_ptr const logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
  MasterPrinter_ptr const wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  Logger_vnlog_error(logger, wffprint, opts, "%s", "Input prop:\n");
  Prop_print(self, Logger_get_ostream(logger), PROP_PRINT_FMT_DEFAULT);
  Logger_log(logger, "\n");
#endif

  switch (Prop_get_type(self)) {
  case Prop_Ctl:
    expr = prop_is_ctl_convertible_to_invar(self);
    break;

  case Prop_Ltl:
    expr = prop_is_ltl_convertible_to_invar(self);
    break;

  case Prop_Compute:
  case Prop_Invar:
  case Prop_Psl:
    break;

  default:  error_unreachable_code();
  }

  if (NULL != expr) {
    retval = Prop_create_partial(env, expr, Prop_Invar);
  }
  else retval = NULL;

#ifdef DEBUG_CONVERT_PROPERTY_TO_INVAR
  {
    Logger_ptr const logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_vnlog_error(logger,  wffprint, opts, "%s", "Output prop:\n");
    if (NULL != retval) {
      Prop_print(retval, Logger_get_ostream(logger), PROP_PRINT_FMT_DEFAULT);
      Logger_log(logger, "\n");
    }
    else Logger_vnlog_error(logger,  wffprint, opts, "%s", "is NULL:\n");
  }
#endif

  return retval;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The Prop class virtual finalizer

  Called by the class destructor
*/
static void prop_finalize(Object_ptr object, void* dummy)
{
  Prop_ptr self = PROP(object);

  prop_deinit(self);
  FREE(self);
}

/*!
  \brief Derived from Prop_get_expr_core, but for PSL only
                      removes forall replicators rather than
                      converting the whole expression into
                      LTL. Written to be used with
                      Prop_apply_coi_for_{bdd,bmc}.


*/
static Expr_ptr prop_get_expr_core_for_coi(const Prop_ptr self)
{
  Expr_ptr res;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));

  if (Prop_get_type(self) == Prop_Psl) {
    res = PslNode_remove_forall_replicators(env, Prop_get_expr(self));
  }
  else res = Prop_get_expr(self); /* usual expression */

  return Compile_pop_distrib_ops(ENV_OBJECT(self)->environment, res);
}


/**AutomaticEnd***************************************************************/
