/* ---------------------------------------------------------------------------


   This file is part of the ``compile'' package of NuSMV version 2.
   Copyright (C) 1998-2005 by CMU and FBK-irst.

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
  \author Marco Roveri
  \brief Flattening of the model.

  Performs the flattening of the model.

   We start from the module <code>main</code> and we recursively
   instantiate all the modules or processes declared in it.<br>
   Consider the following example: <blockquote>

   MODULE main<br>
   ...<br>
   VAR<br>
   a : boolean;<br>
   b : foo;<br>
   ...<br><br>

   MODULE foo<br>
   VAR <br>
   z : boolean;<br>

   ASSIGN<br>
   z := 1;<br>

   </blockquote>

   The flattening instantiate the module foo in the
   <code>main</code> module. You can refer to the variables
   "<code>z</code>" declared in the module <code>foo</code> after
   the flattening by using the dot notation <code>b.z</code>.

   Warning: compileFlatten in reentrant w.r.t an environment instance
   But within the same environment, using it for 2 independant tasks
   may result in conflicts. The compiler will become eventually a
   class.
*/


#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/compile/FlatHierarchy.h"
#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/compile/symb_table/SymbLayer.h"
#include "nusmv/core/compile/symb_table/SymbType.h"
#include "nusmv/core/compile/symb_table/symb_table.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"
#include "nusmv/core/compile/symb_table/NFunction.h"

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/range.h"
#include "nusmv/core/utils/Pair.h"
#include "nusmv/core/utils/WordNumberMgr.h"

#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#include "nusmv/core/node/NodeMgr.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/parser.h"
#include "nusmv/core/parser/psl/pslNode.h"

#include "nusmv/core/hrc/hrc.h"
#include "nusmv/core/trace/TraceMgr.h"
#include "nusmv/core/prop/PropDb.h"
#include "nusmv/core/cinit/cinit.h"


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief Body of define in evaluation

  Indicates that the body of a define is under the
   flattening, it is usde to discover possible recursive definitions.

  \sa Flatten_GetDefinition
*/
#define BUILDING_FLAT_BODY (node_ptr)-11

#include "nusmv/core/compile/flattening/MasterCompileFlattener.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef enum {
  Get_Definition_Mode,
  Expand_Definition_Mode
} Definition_Mode_Type;

/*!
  \brief Cleans and frees the hash

  A utility used by internal clean up code for hash tables
*/
#define FREE_HASH(hash)                         \
  {                                             \
    if (hash != (hash_ptr) NULL) {              \
      free_assoc(hash);                         \
      hash = (hash_ptr) NULL;                   \
    }                                           \
  }

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_VARIABLE_INSTANTIATE_MODE "compile_variable_instantiate_mode"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_FUNCTION_INSTANTIATE_MODE "compile_function_instantiate_mode"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_DEFINITION_MODE "compile_definition_mode"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_MODULE_HASH     "compile_module_hash"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_FLATTEN_DEF_HASH "compile_flatten_def_hash"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_SET_INT(env, key, val)                                      \
  {                                                                     \
    nusmv_assert(-5 != val);                                            \
    NuSMVEnv_set_or_replace_value(env, key, NODE_FROM_INT(val + 5));    \
  }

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_GET_INT(env, key)                           \
  (NODE_TO_INT(NuSMVEnv_get_value(env, key)) - 5)

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/**Variable********************************************************************

   Synopsis    [The mode to perform variable instantiation.]

   Description [Depending the value of this variable we perform
   instantiation of state variables or input variables.]

******************************************************************************/
static void set_variable_instantiation_to_input (const NuSMVEnv_ptr env) {
  ENV_SET_INT(env, ENV_VARIABLE_INSTANTIATE_MODE,
              Input_Variables_Instantiation_Mode);
}

static void set_variable_instantiation_to_state (const NuSMVEnv_ptr env) {
  ENV_SET_INT(env, ENV_VARIABLE_INSTANTIATE_MODE,
              State_Variables_Instantiation_Mode);
}

static void set_variable_instantiation_to_frozen (const NuSMVEnv_ptr env) {
  ENV_SET_INT(env, ENV_VARIABLE_INSTANTIATE_MODE,
              Frozen_Variables_Instantiation_Mode);
}

static void set_function_instantiation_to_frozen (const NuSMVEnv_ptr env) {
  ENV_SET_INT(env, ENV_FUNCTION_INSTANTIATE_MODE,
              Frozen_Functions_Instantiation_Mode);
}

static void set_function_instantiation_to_state (const NuSMVEnv_ptr env) {
  ENV_SET_INT(env, ENV_FUNCTION_INSTANTIATE_MODE,
              State_Functions_Instantiation_Mode);
}


static Instantiation_Variables_Mode_Type
variable_instantiation_mode_get(const NuSMVEnv_ptr env)
{
  if (!NuSMVEnv_has_value(env, ENV_VARIABLE_INSTANTIATE_MODE)) {
    /* return the default value */
    return State_Variables_Instantiation_Mode;
  }
  return ENV_GET_INT(env, ENV_VARIABLE_INSTANTIATE_MODE);
}

static Instantiation_Functions_Mode_Type
function_instantiation_mode_get(const NuSMVEnv_ptr env)
{
  if (!NuSMVEnv_has_value(env, ENV_FUNCTION_INSTANTIATE_MODE)) {
    /* return the default value */
    return Frozen_Functions_Instantiation_Mode;
  }
  return ENV_GET_INT(env, ENV_FUNCTION_INSTANTIATE_MODE);
}

static void set_variable_instantiation_mode(const NuSMVEnv_ptr env,
                                            Instantiation_Variables_Mode_Type type)
{
  ENV_SET_INT(env, ENV_VARIABLE_INSTANTIATE_MODE, type);
}

static void set_function_instantiation_mode(const NuSMVEnv_ptr env,
                                            Instantiation_Functions_Mode_Type type)
{
  ENV_SET_INT(env, ENV_FUNCTION_INSTANTIATE_MODE, type);
}


/**Variable********************************************************************

   Synopsis    [The hash containing the definition of each module read in.]

   Description [This hash uses the name of the module as index, and for
   each module it stores the following data structure:<br>
   <center><code>&lt;LAMBDA , arguments, module_body&gt;</code></center><br>
   I.e. it is a node, whose type is <code>LAMBDA</code> and whose "car" are
   the module arguments and the "cdr" is the module body (i.e. normal
   assignments, init assignments and next assignments.
   ]

******************************************************************************/
void insert_module_hash(const NuSMVEnv_ptr env, node_ptr x, node_ptr y)
{
  hash_ptr module_hash = (hash_ptr)NuSMVEnv_get_value(env, ENV_MODULE_HASH);
  insert_assoc(module_hash, x, y);
}

node_ptr lookup_module_hash(const NuSMVEnv_ptr env, node_ptr x)
{
  hash_ptr module_hash = (hash_ptr)NuSMVEnv_get_value(env, ENV_MODULE_HASH);
  return(find_assoc(module_hash, x));
}

static void init_module_hash(const NuSMVEnv_ptr env)
{
  hash_ptr module_hash;

  /* Auxiliary variable used to traverse the parse tree. */
  node_ptr m;
  /* The parse tree representing the input files. */
  extern node_ptr parsed_tree;

  module_hash = new_assoc();
  NuSMVEnv_set_value(env, ENV_MODULE_HASH, module_hash);

  m = parsed_tree;
  while (m != Nil) {
    node_ptr cur_module = car(m);
    if (Nil != cur_module && node_get_type(cur_module) == MODULE) {
      CompileFlatten_hash_module(env, cur_module);
    }
    m = cdr(m);
  }

}

static void clear_module_hash(const NuSMVEnv_ptr env)
{
  hash_ptr module_hash = (hash_ptr)NuSMVEnv_remove_value(env, ENV_MODULE_HASH);
  FREE_HASH(module_hash);
}


/**Variable********************************************************************

   Synopsis    [The hash of flatten_def]

   Description [This hash associates to an atom corresponding to a
   defined symbol the corresponding flattened body.]

******************************************************************************/
static void init_flatten_def_hash(const NuSMVEnv_ptr env)
{
  hash_ptr flatten_def_hash = new_assoc();
  nusmv_assert(flatten_def_hash != (hash_ptr)NULL);
  NuSMVEnv_set_value(env, ENV_FLATTEN_DEF_HASH, flatten_def_hash);
}

static void insert_flatten_def_hash(const NuSMVEnv_ptr env,
                                    node_ptr key, node_ptr value)
{
  hash_ptr flatten_def_hash = (hash_ptr)NuSMVEnv_get_value(env, ENV_FLATTEN_DEF_HASH);
  insert_assoc(flatten_def_hash, key, (node_ptr)value);
}

static node_ptr lookup_flatten_def_hash(const NuSMVEnv_ptr env,
                                        node_ptr key)
{
  hash_ptr flatten_def_hash = (hash_ptr)NuSMVEnv_get_value(env, ENV_FLATTEN_DEF_HASH);
  return((node_ptr)find_assoc(flatten_def_hash, key));
}

#if 0
static assoc_retval flatten_def_hash_free(char *key, char *data, char * arg)
{
  node_ptr element = (node_ptr)data;
  /* Notice that this hash may contain elements set to
     BUILDING_FLAT_BODY in cases of errors inside the flattening
     procedure */
  if (element != (node_ptr)NULL && element != BUILDING_FLAT_BODY) {
    free_node(nodemgr, element);
  }
  return(ASSOC_DELETE);
}
#endif

static void clear_flatten_def_hash(const NuSMVEnv_ptr env)
{

  if (NuSMVEnv_has_value(env, ENV_FLATTEN_DEF_HASH)) {
    hash_ptr flatten_def_hash =
      (hash_ptr)NuSMVEnv_remove_value(env, ENV_FLATTEN_DEF_HASH);

    /* This was commented out, as this table contains values shared
       among different keys, and some of those values may be
       created with new_node (which would require to free
       them). However, this would end up in freeing multiple times
       the same node */
    /* clear_assoc_and_free_entries(flatten_def_hash,
                                    flatten_def_hash_free); */
    free_assoc(flatten_def_hash);
  }
}


/**Variable********************************************************************

   Synopsis    [The stack containing the nesting for modules.]

   Description [This variable contains the nesting of modules. It is
   used in the instantiation phase to check for recursively defined modules.]

******************************************************************************/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_MODULE_STACK "module_stack"


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void
compile_instantiate(const NuSMVEnv_ptr env,
                    SymbTable_ptr st,
                    SymbLayer_ptr,
                    node_ptr,
                    node_ptr,
                    node_ptr,
                    node_ptr*,
                    FlatHierarchy_ptr,
                    HrcNode_ptr,
                    hash_ptr,
                    boolean);

static void
compile_instantiate_by_name(const NuSMVEnv_ptr env,
                            SymbTable_ptr, SymbLayer_ptr,
                            node_ptr, node_ptr, node_ptr,
                            node_ptr *, FlatHierarchy_ptr, HrcNode_ptr,
                            hash_ptr, boolean);

static void
compile_add_vars_to_hierarhcy(node_ptr name, SymbType_ptr type,
                              FlatHierarchy_ptr fh);
static void
compile_instantiate_var(const NuSMVEnv_ptr env,
                        SymbTable_ptr st,
                        SymbLayer_ptr layer,
                        node_ptr,
                        node_ptr,
                        node_ptr,
                        node_ptr*,
                        FlatHierarchy_ptr,
                        HrcNode_ptr,
                        hash_ptr,
                        boolean);
static void
compile_instantiate_vars(const NuSMVEnv_ptr env,
                         SymbTable_ptr st,
                         SymbLayer_ptr layer, node_ptr, node_ptr,
                         node_ptr *, FlatHierarchy_ptr, HrcNode_ptr,
                         hash_ptr, boolean);

static node_ptr put_in_context(node_ptr, void*);

static node_ptr
compileFlattenProcess(const NuSMVEnv_ptr env,
                      const SymbTable_ptr,
                      node_ptr,
                      FlatHierarchy_ptr);

static void
compileFlattenProcessRecur(const NuSMVEnv_ptr env,
                           const SymbTable_ptr,
                           node_ptr,
                           node_ptr,
                           node_ptr,
                           FlatHierarchy_ptr);

static node_ptr
compile_flatten_eval_number(const MasterCompileFlattener_ptr flattener,
                            const SymbTable_ptr symb_table,
                            node_ptr n, node_ptr context);

static void
create_process_symbolic_variables(const NuSMVEnv_ptr env,
                                  SymbTable_ptr symb_table,
                                  SymbLayer_ptr, node_ptr);

static void
flatten_declare_constants_within_list(SymbTable_ptr symb_table,
                                      SymbLayer_ptr layer,
                                      node_ptr range);

static void resolve_range(SymbTable_ptr st,
                          node_ptr range, node_ptr context,
                          int* low, int* high);

static void
instantiate_array_define(SymbTable_ptr st,
                          SymbLayer_ptr layer,
                          node_ptr name,
                          node_ptr mod_name,
                          node_ptr definition);


static HrcNode_ptr get_hrc_root_node(HrcNode_ptr node);

static int compile_flatten_get_int(node_ptr value);

static node_ptr
compile_flatten_normalise_value_list(NodeMgr_ptr nodemgr,
                                     node_ptr old_value_list);


static node_ptr
compile_flatten_build_word_toint_ith_bit_case(const NuSMVEnv_ptr env,
                                              node_ptr wexpr,
                                              int bit,
                                              boolean is_negative);


static void _check_supported_function_types(const NuSMVEnv_ptr env,
                                            SymbType_ptr symbolicType, node_ptr name);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

FlatHierarchy_ptr Compile_FlattenHierarchy(
  const NuSMVEnv_ptr env,
  const SymbTable_ptr symb_table,
  SymbLayer_ptr layer, /* the symbolic layer to flat */
  node_ptr module_name,  /* the <code>ATOM</code> representing the
                            name of the module being instantiated at
                            the top of the hierarchy. */
  node_ptr inst_name, /* the name of the module instance
                         at the top of the hierarchy. */
  node_ptr actual, /* the actual module arguments */
  boolean create_process_variables, /* enables creation of process variables */
  boolean calc_vars_constr, /* triggers calc of vars constr, or delays it */
  boolean expand_bounded_arrays, /* enable bounded arrays expansion option */
  HrcNode_ptr hrc_result /* hrc node to be populated*/ )
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  FlatHierarchy_ptr result = FlatHierarchy_create(symb_table);

  /* Take care of redefinitions of module instances. */
  hash_ptr instances = new_assoc();

  /* creation of hrc structure */
  /* warning The way hrc structure is instantiated is still not definitive so
     the instantiation part of hrc will be refactored in a later time. */
  if (HRC_NODE(NULL) != hrc_result) {
    node_ptr mod_def = lookup_module_hash(env, find_atom(nodemgr, module_name));
    if (NODE_PTR(Nil) == mod_def) {
      ErrorMgr_error_undefined(errmgr, module_name); /* The module is undefined */
    }

    HrcNode_set_symbol_table(hrc_result, symb_table);
    HrcNode_set_lineno(hrc_result, node_get_lineno(mod_def));
    HrcNode_set_name(hrc_result, module_name);
    HrcNode_set_instance_name(hrc_result, inst_name);
  }


  /* collect all the constructs of a hierarchy */
  Compile_ConstructHierarchy(env, symb_table, layer, module_name,
                             inst_name, actual, result,
                             hrc_result, instances, expand_bounded_arrays);

  /* Process the created hierarchy. */
  Compile_ProcessHierarchy(env, symb_table, layer, result, inst_name,
                           create_process_variables,
                           calc_vars_constr);

  if (FlatHierarchy_get_compassion(result) != Nil) {
    StreamMgr_print_output(streams,
      "WARNING *** The model contains COMPASSION declarations.        ***\n"
      "WARNING *** Full fairness is not yet fully supported in NuSMV. ***\n"
      "WARNING *** Currently, COMPASSION declarations are only        ***\n"
      "WARNING *** supported for BDD-based LTL Model Checking.        ***\n"
      "WARNING *** Results of CTL Model Checking and of Bounded       ***\n"
      "WARNING *** Model Checking may be wrong.                       ***\n");
  }

  if (HRC_NODE(NULL) != hrc_result) {
    if (HrcNode_get_undef(hrc_result) != (void*)NULL) {
      StreamMgr_print_output(streams,
              "WARNING *** The model contains PROCESSes or ISAs. ***\n"
              "WARNING *** The HRC hierarchy will not be usable. ***\n");
    }
  }

  free_assoc(instances);
  return result;
}

void
Compile_ConstructHierarchy(
  const NuSMVEnv_ptr env,
  SymbTable_ptr st, /* the symbol table the layer belongs to */
  SymbLayer_ptr layer, /* the layer that must be filled in by the flattening */
  node_ptr module_name, /* the <code>ATOM</code> representing the name of the
                           module being instantiated */
  node_ptr instance_name, /* the name of the module instance to be
                             instantiated */
  node_ptr actual, /* the actual module arguments */
  FlatHierarchy_ptr result,
  HrcNode_ptr hrc_result,
  hash_ptr instances,
  boolean expand_bounded_arrays)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr tmp_assign = Nil;
  compile_instantiate_by_name(env, st, layer, module_name, instance_name, actual,
                              &tmp_assign, result, hrc_result, instances,
                              expand_bounded_arrays);

  /* create a list of pairs (process name,assignments in it), it to the result */
  tmp_assign = cons(nodemgr, cons(nodemgr, instance_name, tmp_assign),
                    FlatHierarchy_get_assign(result));
  FlatHierarchy_set_assign(result, tmp_assign);
}

void Compile_ProcessHierarchy(const NuSMVEnv_ptr env,
                              SymbTable_ptr symb_table,
                              SymbLayer_ptr layer,
                              FlatHierarchy_ptr hierarchy,
                              node_ptr name,
                              boolean create_process_variables,
                              boolean calc_vars_constr)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr tmp;

  /* --- 1 ---- */
  /* if processes are not allowed then no processes should exist */
  nusmv_assert(create_process_variables ||
               (Nil != FlatHierarchy_get_assign(hierarchy) &&
                Nil == cdr(FlatHierarchy_get_assign(hierarchy))));

  /* Create process_selector variables and running defines (if required at all)
     (this must be done before flattening processes and type checking).
  */
  if (create_process_variables) { /* 'map' is used to get all process names */
    tmp = map(nodemgr, car, FlatHierarchy_get_assign(hierarchy));
    create_process_symbolic_variables(env, symb_table, layer, tmp);
    /* do not free tmp, it is used to construct the type and saved there */
  }

  /* --- 2 ---- */

  /* Flatten the expressions INIT, TRANS, INVAR, JUSTICE and COMPASSION */
  tmp = Compile_FlattenSexp(symb_table,
                            FlatHierarchy_get_init(hierarchy),
                            name);
  FlatHierarchy_set_init(hierarchy, tmp);

  tmp = Compile_FlattenSexp(symb_table,
                            FlatHierarchy_get_trans(hierarchy),
                            name);
  FlatHierarchy_set_trans(hierarchy, tmp);

  tmp = Compile_FlattenSexp(symb_table,
                            FlatHierarchy_get_invar(hierarchy),
                            name);
  FlatHierarchy_set_invar(hierarchy, tmp);

  tmp = Compile_FlattenSexp(symb_table,
                            reverse(FlatHierarchy_get_justice(hierarchy)),
                            name);
  FlatHierarchy_set_justice(hierarchy, tmp);

  tmp = Compile_FlattenSexp(symb_table,
                            reverse(FlatHierarchy_get_compassion(hierarchy)),
                            name);
  FlatHierarchy_set_compassion(hierarchy, tmp);


  /* The SPEC, LTLSPEC, PSLSPEC, INVAR_SPEC, COMPUTE properties are
     simply reversed but NOT flattened. */

  /* RC: comments below are experiments to handle nested relative contexts */
  tmp = reverse(FlatHierarchy_get_spec(hierarchy));
  FlatHierarchy_set_spec(hierarchy, tmp/*compile_fix_nested_context(tmp)*/);

  tmp = reverse(FlatHierarchy_get_ltlspec(hierarchy));
  FlatHierarchy_set_ltlspec(hierarchy, tmp/*compile_fix_nested_context(tmp)*/);

  tmp = reverse(FlatHierarchy_get_invarspec(hierarchy));
  FlatHierarchy_set_invarspec(hierarchy,
                              tmp/*compile_fix_nested_context(tmp)*/);

  tmp = reverse(FlatHierarchy_get_pslspec(hierarchy));
  FlatHierarchy_set_pslspec(hierarchy, tmp/*compile_fix_nested_context(tmp)*/);

  tmp = reverse(FlatHierarchy_get_compute(hierarchy));
  FlatHierarchy_set_compute(hierarchy, tmp/*compile_fix_nested_context(tmp)*/);

  tmp = reverse(FlatHierarchy_get_property_patterns(hierarchy));
  FlatHierarchy_set_property_patterns(hierarchy, tmp);

  /* --- 3 ---- */
  /* assignments require special management:
     1. they are flattened
     2. running symbols are added to assignments (if required)
     3. a hash of (var-name -> its assignment, invar, init) is created.
  */
  tmp = compileFlattenProcess(env,
                              symb_table,
                              FlatHierarchy_get_assign(hierarchy),
                              hierarchy);

  FlatHierarchy_set_assign(hierarchy, tmp);

  /* --- 4 ---- */
  /* creation of association between vars and constraints */
  if (calc_vars_constr) {
    /* triggers the calculation of vars constrains */
    FlatHierarchy_calculate_vars_constrains(hierarchy);
  }

  /* --- 5 (optional) ---- */
  if (opt_syntactic_checks_disabled(opts)) {
    StreamMgr_print_output(streams,
            "WARNING *** Input model well-formance check skipped ***\n");
  }
  else {
    /* checks next operator in all the hierarchy */
    Compile_check_next(symb_table, FlatHierarchy_get_init(hierarchy), Nil,
                       false);
    Compile_check_next(symb_table, FlatHierarchy_get_invar(hierarchy), Nil,
                       false);
    Compile_check_next(symb_table, FlatHierarchy_get_trans(hierarchy), Nil,
                       true);
    Compile_check_next(symb_table, FlatHierarchy_get_justice(hierarchy), Nil,
                       false);
    Compile_check_next(symb_table,
                       FlatHierarchy_get_compassion(hierarchy),
                       Nil,
                       false);
    Compile_check_next(symb_table, FlatHierarchy_get_compute(hierarchy), Nil,
                       false);
    Compile_check_next(symb_table, FlatHierarchy_get_spec(hierarchy), Nil,
                       false);
    Compile_check_next(symb_table, FlatHierarchy_get_invarspec(hierarchy), Nil,
                       true);
    Compile_check_next(symb_table, FlatHierarchy_get_ltlspec(hierarchy), Nil,
                       true);
    Compile_check_next(symb_table, FlatHierarchy_get_pslspec(hierarchy), Nil,
                       false);

    /* Check [m]define bodies for nested nexts */
    {
      SymbTableIter iter;

      SYMB_TABLE_FOREACH(symb_table, iter, STT_DEFINE | STT_ARRAY_DEFINE) {
        node_ptr define = SymbTable_iter_get_symbol(symb_table, &iter);
        Compile_check_next(symb_table, define, Nil, true);
        Compile_check_input_next(symb_table, define, Nil);
      }
    }

    /* --- 6 ---- */
    /* type check the obtained module */
    /* TODO[MD] Here could be used the function FlatHierarchy_type_check */
    {
      boolean isOk = true;
      isOk =
        isOk &&
        TypeChecker_check_layer(SymbTable_get_type_checker(symb_table), layer);

      /* get rid of module names */
      tmp = map(nodemgr, cdr, FlatHierarchy_get_assign(hierarchy));

      isOk = isOk && TypeChecker_check_constrains(
                                 SymbTable_get_type_checker(symb_table),
                                 FlatHierarchy_get_init(hierarchy),
                                 FlatHierarchy_get_trans(hierarchy),
                                 FlatHierarchy_get_invar(hierarchy),
                                 tmp,
                                 FlatHierarchy_get_justice(hierarchy),
                                 FlatHierarchy_get_compassion(hierarchy));
      free_list(nodemgr, tmp);

      if (!isOk) ErrorMgr_error_type_system_violation(errmgr); /* error */
    }

    /* --- 7 ---- */
    /* if process variable should NOT be created then this is not a user module
       but module generated by NuSMV.
       So the input variables check and check of cycle dependencies of
       assignments may be skipped.
    */
    if (create_process_variables) {
      compileCheckForInputVars(symb_table, hierarchy);

      Compile_CheckAssigns(symb_table, FlatHierarchy_get_assign(hierarchy));
    }
  }
}

node_ptr Compile_FlattenSexp(const SymbTable_ptr symb_table, node_ptr sexp,
                             node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  node_ptr result;

  CATCH(errmgr) {
    MasterCompileFlattener_ptr flattener =
        MASTER_COMPILE_FLATTENER(NuSMVEnv_get_value(env, ENV_FLATTENER));
    result = MasterCompileFlattener_flatten(flattener, symb_table, sexp, context);

  }
  FAIL(errmgr) {
    ErrorMgr_rpterr(errmgr, NULL); /* rethrow */
  }

  return result;
}

node_ptr
Compile_FlattenSexpExpandDefine(const SymbTable_ptr symb_table,
                                node_ptr sexp, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  node_ptr result;


  CATCH(errmgr) {
    MasterCompileFlattener_ptr flattener =
        MASTER_COMPILE_FLATTENER(NuSMVEnv_get_value(env, ENV_FLATTENER));
    result = MasterCompileFlattener_flatten_expand_define(flattener,
                                                          symb_table,
                                                          sexp,
                                                          context);

  }
  FAIL(errmgr) {
    ErrorMgr_rpterr(errmgr, NULL); /* rethrow */
  }

  return result;
}


/*!
  \brief Gets the flattened version of an atom.

  Gets the flattened version of an atom. If the
   atom is a define then it is expanded. If the definition mode
   is set to "expand", then the expanded flattened version is returned,
   otherwise, the atom is returned.

  \se The <tt>flatten_def_hash</tt> is modified in
   order to memoize previously computed definition expansion.
*/

node_ptr Flatten_GetDefinition(const SymbTable_ptr symb_table, node_ptr atom,
                               const boolean expand_defines)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  MasterCompileFlattener_ptr flattener =
    MASTER_COMPILE_FLATTENER(NuSMVEnv_get_value(env, ENV_FLATTENER));
  MasterCompileFlattener_def_mode mode;
  node_ptr result = Nil;

  if (expand_defines) {
    mode = Flattener_Expand_Def_Mode;
  }
  else {
    mode = Flattener_Get_Def_Mode;
  }

  result = MasterCompileFlattener_get_definition(flattener,
                                                 symb_table,
                                                 atom, mode);
  return result;
}

node_ptr CompileFlatten_concat_contexts(const NuSMVEnv_ptr env,
                                        node_ptr ctx1, node_ptr ctx2)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const MasterNormalizer_ptr normalizer =
    MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));

  int op;
  if (ctx2 == Nil) return MasterNormalizer_normalize_node(normalizer, ctx1);

  op = node_get_type(ctx2);
  if (op == DOT && car(ctx2) == Nil) {
    return MasterNormalizer_normalize_node(normalizer, find_node(nodemgr, DOT, ctx1, cdr(ctx2)));
  }

  if (op == ATOM ||
      op == NUMBER) {
    return MasterNormalizer_normalize_node(normalizer, find_node(nodemgr, DOT, ctx1, ctx2));
  }

  if (op == BIT) {
    return find_node(nodemgr, BIT,
                     CompileFlatten_concat_contexts(env, ctx1, car(ctx2)),
                     cdr(ctx2));
  }

  return find_node(nodemgr, op,
                   CompileFlatten_concat_contexts(env, ctx1, car(ctx2)),
                   MasterNormalizer_normalize_node(normalizer, cdr(ctx2)));
}

void Flatten_remove_symbol_info(const NuSMVEnv_ptr env,
                                node_ptr name)
{
  /* [SM] - we may NOT have a module hash already set in the environment.

     This happens when the symbol table and hierarchy are built
     outside the standard NuSMV flow.
   */
  if (NuSMVEnv_has_value(env, ENV_MODULE_HASH)) {
    if (lookup_module_hash(env, name) != (node_ptr) NULL) {
      /* TODO[MD] This is a workaround since remove_module_hash is missing */
      insert_module_hash(env, name, (node_ptr) NULL);
    }
  }

  /* flatten def */
  MasterCompileFlattener_remove_define_info(
        MASTER_COMPILE_FLATTENER(NuSMVEnv_get_value(env, ENV_FLATTENER)),
        SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE)),
        name);
}

int CompileFlatten_flatten_smv(NuSMVEnv_ptr env, boolean calc_vars_constrains,
                               boolean expand_bounded_arrays)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  TraceMgr_ptr trace_mgr = TRACE_MGR(NuSMVEnv_get_value(env, ENV_TRACE_MGR));
  OptsHandler_ptr opt = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  PropDb_ptr db = (PropDb_ptr) NuSMVEnv_get_value(env, ENV_PROP_DB);

  /* Initializes the flattener, that must be initialized *after* the
     parsing phase */
  CompileFlatten_init_flattener(env);

  /* Processing of the parse tree and constructions of all the
    expressions for the state machine(s). Here the expansions are
    performed so that modules and processes are created. The expansion
    of modules is such that the formal parameters (if any) are
    replaced by the actual ones and the machine is replicated.
  */
  {
    SymbTable_ptr st;
    SymbLayer_ptr layer;
    FlatHierarchy_ptr hierarchy;
    int propErr;
    HrcNode_ptr hrc_node = HRC_NODE(NuSMVEnv_get_value(env, ENV_HRC_HIERARCHY));

    st = NuSMVEnv_get_value(env, ENV_SYMB_TABLE);
    layer = SymbTable_create_layer(st, MODEL_LAYER_NAME,
                                   SYMB_LAYER_POS_BOTTOM);

    /* register the new layer to the "model" layer class, and set
       the class to be the default */
    SymbTable_layer_add_to_class(st, MODEL_LAYER_NAME, MODEL_LAYERS_CLASS);
    SymbTable_set_default_layers_class_name(st,  MODEL_LAYERS_CLASS);

    nusmv_assert(!NuSMVEnv_has_value(env, ENV_FLAT_HIERARCHY));


    CATCH(errmgr) {
      hierarchy = Compile_FlattenHierarchy(env, st, layer, sym_intern(env, "main"), Nil, Nil,
                                           true, calc_vars_constrains, expand_bounded_arrays,
                                           hrc_node);
    }
    FAIL(errmgr) {
      NuSMVCore_reset(env);
      return 1;
    }

    /* Processes or Isa have been found, destroy the HRC hierarchy */
    if (hrc_node != HRC_NODE(NULL)) {
      if ((void*) NULL != HrcNode_get_undef(hrc_node)) {
        HrcNode_destroy_recur(hrc_node);
        NuSMVEnv_remove_value(env, ENV_HRC_HIERARCHY);
      }
    }

    /* We store properties in the DB of properties */
    CATCH(errmgr) {
    propErr = PropDb_fill(db, st,
                          FlatHierarchy_get_spec(hierarchy),
                          FlatHierarchy_get_compute(hierarchy),
                          FlatHierarchy_get_ltlspec(hierarchy),
                          FlatHierarchy_get_pslspec(hierarchy),
                          FlatHierarchy_get_invarspec(hierarchy));
    }
    FAIL(errmgr) {
      FlatHierarchy_destroy(hierarchy);
      SymbTable_remove_layer(st, layer);
      goto flattening_failed;
    }

    if (0 != propErr) {
      /* cleans up the initialized internal structures */
      FlatHierarchy_destroy(hierarchy);
      SymbTable_remove_layer(st, layer);
      goto flattening_failed; /* error */
    }

    NuSMVEnv_set_value(env, ENV_FLAT_HIERARCHY, hierarchy);
  }

  TraceMgr_register_layer(trace_mgr, MODEL_LAYER_NAME);

  /* if syntax errors have been found when parser mode is set to lax */
  if (Parser_get_syntax_errors_list(env) != Nil) {
    StreamMgr_print_error(streams,  "\nWarning! Syntax errors have been found, no "
            "flattening is possible.\n");
    StreamMgr_print_error(streams,  "However, as option '%s' is set, a partial "
            "construction\nof the HRC was done.\n",  OPT_PARSER_IS_LAX);
    StreamMgr_print_error(streams,  "This allows you to dump the HRC.\n");
    cmp_struct_set_hrc_built(cmps);
    return 0;
  }

  /* everything went ok */
  cmp_struct_set_hrc_built(cmps);
  cmp_struct_set_flatten_hrc(cmps);
  if (opt_verbose_level_gt(opt, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "...done\n");
  }

  return 0;

  /* Exception handling */
 flattening_failed:
  PropDb_clean(db);
  CompileFlatten_quit_flattener(env);
  cmp_struct_unset_read_model(cmps); /* resets also the command read_model */
  return 1; /* error */
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void error_bit_selection_assignment_not_supported(const NuSMVEnv_ptr env, node_ptr name)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  /* MR: This will be removed when assignment of bit selection is
   * implemented */
  extern int nusmv_yylineno;

  nusmv_yylineno = node_get_lineno(name);

  ErrorMgr_start_parsing_err(errmgr);
  StreamMgr_print_error(streams,  "Bit selection '");
  StreamMgr_nprint_error(streams, wffprint, "%N", name);
  StreamMgr_print_error(streams,  "':\n");
  StreamMgr_print_error(streams,
          "Error: Current version does not support assignment "\
          "of bit selections.\n");
  ErrorMgr_finish_parsing_err(errmgr);
}

node_ptr
compile_flatten_rewrite_word_toint_cast(const NuSMVEnv_ptr env, node_ptr body, SymbType_ptr type)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

  node_ptr result = Nil;
  int width = SymbType_get_word_width(type);
  int i;

  nusmv_assert(SymbType_is_word(type));


  if (SymbType_is_unsigned_word(type)) {
    /* Check the width of the word. For unsigned words, the limit is
       31 bits, because NuSMV ints are signed and limited to 32
       bits. */
    if (width > 31) {
      ErrorMgr_error_out_of_bounds_word_toint_cast(errmgr, body);
    }

    result =
      compile_flatten_build_word_toint_ith_bit_case(env, body, 0, false);

    for (i = 1; i < width; ++i) {
      node_ptr tmp =
        compile_flatten_build_word_toint_ith_bit_case(env, body, i, false);
      result = new_node(nodemgr, PLUS, result, tmp);
    }
  }
  else if (SymbType_is_signed_word(type)) {
    node_ptr w0 = find_node(nodemgr, NUMBER_UNSIGNED_WORD,
                            NODE_PTR(WordNumberMgr_integer_to_word_number(words, (WordNumberValue)0, 1)),
                            Nil);
    node_ptr msb = find_node(nodemgr, NUMBER, NODE_FROM_INT(width-1), Nil);
    node_ptr cond, positive, negative;
    int i;

    /* Check the width of the word. For unsigned words, the limit is
       32 bits, because NuSMV ints are signed and limited to 32
       bits. */
    if (width > 32) {
      ErrorMgr_error_out_of_bounds_word_toint_cast(errmgr, body);
    }

    /* w[msb:msb] = 0ud1_0 --> IS POSITIVE */
    cond = new_node(nodemgr, EQUAL,
                    new_node(nodemgr, BIT_SELECTION, body,
                             new_node(nodemgr, COLON, msb, msb)),
                    w0);

    /* Prepare the base steps for the sum (LSB) */
    positive =
      compile_flatten_build_word_toint_ith_bit_case(env, body, 0, false);
    negative =
      compile_flatten_build_word_toint_ith_bit_case(env, body, 0, true);

    /* Sum all other bits (exept the MSB) */
    for (i = 1; i < (width - 1); ++i) {
      node_ptr tmp =
        compile_flatten_build_word_toint_ith_bit_case(env, body, i, false);
      positive = new_node(nodemgr, PLUS, positive, tmp);

      tmp = compile_flatten_build_word_toint_ith_bit_case(env, body, i, true);
      negative = new_node(nodemgr, PLUS, negative, tmp);
    }

    negative = new_node(nodemgr, PLUS, negative,
                        find_node(nodemgr, NUMBER, NODE_FROM_INT(1), Nil));
    negative = new_node(nodemgr, UMINUS, negative, Nil);

    /* positive_word ? positive_circuit : negative_circuit */
    result = new_node(nodemgr, CASE, new_node(nodemgr, COLON, cond, positive), negative);
  } /* Is signed word */

  return result;
}

/*!
  \brief Flatten a hierarchy of SMV processes.

  This functions takes in input the list of process names
   and their assignments resulting from the instantiation step and
   fills in the hash table (parameter assign_hash) with the
   associations the following form:
   <ul>
   <li><tt>init(var) -> (init_assign)</tt><br>
   where <tt>init_assign</tt> is the right side of the initialization
   assignement of the variable <tt>var</tt>.
   <li><tt>next(var) -> (case P1.running : next_assign_1;
   case P2.running : next_assign_2;
   ...
   var)</tt><br>
   where  <tt>next_assign_i</tt> is the right side of the next
   assignement for the variable <tt>var</tt> in process <tt>i</tt>.
   When other processes not affecting the variable are running,
   the variable stutter.
   If there are no processes the data structure will degenerate
   into <tt>next(var) -> next_assign</tt>.
   <li><tt>var -> (normal_assign)</tt><br>
   where  <tt>normal_assign</tt> is the right side of the
   normal (invariant) assignement for the variable
   <tt>var</tt>.  </ul>

   The parameter proc_assignment_list is a list of pairs
   (process_name, a list of assignment in the process).

*/
static node_ptr compileFlattenProcess(const NuSMVEnv_ptr env,
                                      const SymbTable_ptr symb_table,
                                      node_ptr proc_assign_list,
                                      FlatHierarchy_ptr flattenHierarchy)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  node_ptr l;
  node_ptr result = Nil;
  node_ptr running = sym_intern(env, RUNNING_SYMBOL);
  /* to make the order of processes declarations the same as in an input file,
     reverse the list except the first element (which is "main" module and
     must be at the beginning)
  */
  proc_assign_list = cons(nodemgr, car(proc_assign_list),
                          reverse_ns(nodemgr, cdr(proc_assign_list)));

  for (l = proc_assign_list; l != Nil; l = cdr(l)) { /* Loops over processes */
    ResolveSymbol_ptr rs;
    node_ptr running_name;

    node_ptr process_assignments = Compile_FlattenSexp(symb_table,
                                                       cdr(car(l)), Nil);
    node_ptr process_name = car(car(l));

    rs = SymbTable_resolve_symbol(symb_table, running, process_name);
    running_name = ResolveSymbol_get_resolved_name(rs);

    result = cons(nodemgr, cons(nodemgr, process_name, process_assignments), result);

    compileFlattenProcessRecur(env, symb_table, process_assignments, Nil,
                               running_name, flattenHierarchy);
  }

  return result;
}

node_ptr CompileFlatten_expand_range(const NuSMVEnv_ptr env,
                                     int a, int b)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  node_ptr res = Nil;

  int i;
  for (i=b ; i>=a ; i--) {
    res = find_node(nodemgr, CONS, find_node(nodemgr, NUMBER, NODE_FROM_INT(i), Nil), res);
  }

  return res;
}

node_ptr CompileFlatten_normalise_value_list(const NuSMVEnv_ptr env,
                                             node_ptr old_values)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr values = compile_flatten_normalise_value_list(nodemgr, old_values);

  /* compile_flatten_normalise_value_list returns only a CONS,
     reverted version of the old_values list. We want to keep the
     original ordering */
  values = reverse(values);

  return values;
}

node_ptr CompileFlatten_resolve_number(SymbTable_ptr symb_table,
                                       node_ptr n, node_ptr context)
{
  NuSMVEnv_ptr env = ENV_OBJECT_GET_ENV(symb_table);
  MasterCompileFlattener_ptr flattener =
    MASTER_COMPILE_FLATTENER(NuSMVEnv_get_value(env, ENV_FLATTENER));

  node_ptr num = compile_flatten_eval_number(flattener, symb_table, n,
                                             context);

  if (NUMBER == node_get_type(num) ||
      NUMBER_UNSIGNED_WORD == node_get_type(num) ||
      NUMBER_SIGNED_WORD == node_get_type(num)) return num;

  return (node_ptr) NULL; /* not a number! */
}

node_ptr CompileFlatten_resolve_define_chains(const SymbTable_ptr symb_table,
                                              node_ptr expr, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  boolean is_it_next = false;

  /* loop while the exp is still identifier or a context */
  while (CONTEXT == node_get_type(expr) ||
         DOT == node_get_type(expr) ||
         ARRAY == node_get_type(expr) ||
         ATOM == node_get_type(expr) ||
         NEXT == node_get_type(expr)) {

    ResolveSymbol_ptr rs;
    node_ptr resolved;

    if (CONTEXT == node_get_type(expr)) {
      expr = Compile_FlattenSexp(symb_table, expr, context);
      context = Nil;
      continue;
    }

    if (NEXT == node_get_type(expr)) {
      expr = car(expr);
      /* nested next ? */
      nusmv_assert(!is_it_next);
      is_it_next = true;
      continue;
    }

    /* this is an identifier => process it */
    rs = SymbTable_resolve_symbol(symb_table, expr, context);
    resolved = ResolveSymbol_get_resolved_name(rs);

    /* expr is not identifier but expression */
    if (ResolveSymbol_is_undefined(rs)) {
      return Compile_FlattenSexp(symb_table, expr, context);
    }
    else {
      expr = resolved;
      context = Nil;
    }

    /* if the expression is not identifier => return immediately  */
    if (!(DOT == node_get_type(expr) ||
          ARRAY == node_get_type(expr) ||
          ATOM == node_get_type(expr))) {
      /* ...get_resolved_name gets rid of context */
      nusmv_assert(CONTEXT != node_get_type(expr));
      break;
    }

    /* ResolveSymbol_resolve wraps all identifiers into DOT, which
       is useless for symbol constants which are pure ATOM.
       Exception: artificially created symbolic constants may have DOT.
       This happens, for examples, with constants used to encode
       processes.
    */
    /* artificial constants (with DOT added by ResolveSymbol_resolve) */
    if (ResolveSymbol_is_constant(rs)) {
      break;
    }

    /* a variable */
    else if (ResolveSymbol_is_var(rs)) {
      break;
    }

    /* an array variable */
    else if (ResolveSymbol_is_array(rs)) {
      break;
    }

    /* a function */
    else if (ResolveSymbol_is_function(rs)) {
      break;
    }

    else if (DOT == node_get_type(expr) &&
             ATOM == node_get_type(cdr(expr)) &&
             ResolveSymbol_is_constant(rs)) {
      expr = cdr(expr);
      is_it_next = false;
      break;
    }

    /* is it parameter => resolve and continue */
    else if (ResolveSymbol_is_parameter(rs)) {
      context = SymbTable_get_actual_parameter_context(symb_table, expr);
      expr = SymbTable_get_actual_parameter(symb_table, expr);

      /* Flatten expression if needed */
      if (context != Nil) {
        expr = Compile_FlattenSexp(symb_table, expr, context);
        context = Nil;
      }

      continue;
    }

    /* is it define => resolve and continue */
    else if (ResolveSymbol_is_define(rs)) {
      context = SymbTable_get_define_context(symb_table, expr);
      expr = SymbTable_get_define_body(symb_table, expr);
      nusmv_assert(expr != Nil);

      /* Flatten expression if needed */
      if (context != Nil) {
        expr = Compile_FlattenSexp(symb_table, expr, context);
        context = Nil;
      }

      continue;
    }

    /* is it array define => it is not resolved because
       array expression are not yet allowed to be in arbitrary places */
    else if (ResolveSymbol_is_array_def(rs)) {
      break;
    }

    /* this is array expression. Still it potentially may be
       resolved to define, e.g. define v := [1,2,3], define d:=v, then
       d[1] is array expression but can be resolved to 2.
    */
    else if (ARRAY == node_get_type(expr) &&
             !SymbTable_is_symbol_declared(symb_table, expr)) {
      node_ptr tmp = Compile_FlattenSexp(symb_table, expr, context);
      nusmv_assert(tmp != expr); /* loop in recursion */
      expr = tmp;
      context = Nil;

      continue;
    }

    else {
      /* no other id are possible */
      ErrorMgr_rpterr(errmgr, "\nUnknown (%s) identifier : %s\n",
             SymbTable_is_symbol_declared(symb_table, expr) ?
             "declared" : "undeclared",
             sprint_node(wffprint, expr));
      error_unreachable_code();
    }
  }

  /* Re-add the NEXT if needed */
  if (is_it_next) {
    expr = ExprMgr_next(exprs, expr, symb_table);
  }

  /* If the expression is not flattened */
  if (Nil != context) {
    expr = Compile_FlattenSexp(symb_table, expr, context);
  }

  return expr;
}

void CompileFlatten_init_flattener(NuSMVEnv_ptr env)
{
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  boolean flattener_initialized = NuSMVEnv_get_flag(env, ENV_FLAG_FLATTENER_INITIALIZED);

  nusmv_assert(!flattener_initialized); /* not already initialized */

  if (opt_verbose_level_gt(opts, 2)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Initializing the flattener...\n");
  }

  init_module_hash(env);
  init_flatten_def_hash(env);
  NuSMVEnv_set_flag(env, ENV_FLAG_FLATTENER_INITIALIZED, true);
}

void CompileFlatten_quit_flattener(NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  boolean flattener_initialized = NuSMVEnv_get_flag(env, ENV_FLAG_FLATTENER_INITIALIZED);
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  /* deinits the flattener only if previously initialized */
  if (!flattener_initialized) return;

  if (opt_verbose_level_gt(opts, 3)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Clearing the flattener...\n");
  }

  clear_module_hash(env);
  clear_flatten_def_hash(env);

  /* ---------------------------------------------------------------------- */
  /*                        Reseting of variables                           */
  /* ---------------------------------------------------------------------- */

  /* lists: */
  if (NuSMVEnv_has_value(env, ENV_MODULE_STACK)) {
    node_ptr module_stack = NODE_PTR(NuSMVEnv_remove_value(env, ENV_MODULE_STACK));
    free_list(nodemgr, module_stack);
  }

  if (NuSMVEnv_has_value(env, ENV_PROC_SELECTOR_VNAME)) {
    node_ptr proc_selector_vname;
    proc_selector_vname = NODE_PTR(NuSMVEnv_remove_value(env, ENV_PROC_SELECTOR_VNAME));
    free_node(nodemgr, proc_selector_vname);
  }

  /* other vars: */
  set_variable_instantiation_mode(env, State_Variables_Instantiation_Mode);
  set_function_instantiation_mode(env, Frozen_Functions_Instantiation_Mode);

  NuSMVEnv_remove_flag(env, ENV_FLAG_FLATTENER_INITIALIZED);
}

void CompileFlatten_hash_module(const NuSMVEnv_ptr env, node_ptr parsed_module)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  /* We insert the definition of the current module in the module_hash
     in order to make it available for the Compile_FlattenHierarchy
     routines. */
  node_ptr name = find_atom(nodemgr, caar(parsed_module));
  node_ptr params = cdar(parsed_module);
  node_ptr def = cdr(parsed_module);

  if (lookup_module_hash(env, name)) ErrorMgr_error_redefining(errmgr, name);
  insert_module_hash(env, name, new_lined_node(nodemgr, LAMBDA, params, reverse(def),
                                          node_get_lineno(parsed_module)));
}

SymbType_ptr Compile_InstantiateType(SymbTable_ptr st, SymbLayer_ptr layer,
                                     node_ptr name, node_ptr type,
                                     node_ptr context,
                                     boolean expand_bounded_arrays)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  SymbType_ptr symbolicType = SYMB_TYPE(NULL);

  nusmv_yylineno = node_get_lineno(type);

  /* process the type */
  switch (node_get_type(type)) {
  case BOOLEAN:
    symbolicType = SymbType_create(env, SYMB_TYPE_BOOLEAN, Nil);
    break;

  case TWODOTS: {
    node_ptr expanded_range = Nil;
    int dim1, dim2;

    resolve_range(st, type, context, &dim1, &dim2);

    /* Checks if the range is a "range", i.e. a range is from "a" to "b"
       with the constraint that "b >= a" */
    expanded_range = CompileFlatten_expand_range(env, dim1, dim2);
    if (expanded_range == Nil) { ErrorMgr_error_empty_range(errmgr, name, dim1, dim2); }
    flatten_declare_constants_within_list(st, layer, expanded_range);

    symbolicType = SymbType_create(env, SYMB_TYPE_ENUM, expanded_range);
    break;
  }

  case SCALAR: {
    node_ptr value_list = CompileFlatten_normalise_value_list(env, car(type));
    node_ptr iter;

    /* check that all symbolic constants are not DOTs since only
       process_selector may have complex symbolic constants.
       Also TRUE and FALSE cannot be part of the constants
    */
    for (iter = value_list; Nil != iter; iter = cdr(iter)) {
      /* NOTE: the name of process_selector is the same as in
         create_process_symbolic_variables
      */
      if (DOT == node_get_type(car(iter)) &&
          name != find_node(nodemgr, DOT, Nil, sym_intern(env, PROCESS_SELECTOR_VAR_NAME))) {
        nusmv_yylineno = node_get_lineno(car(iter));
        ErrorMgr_rpterr(errmgr, "unexpected \".\" in a costant name \n");
      }

      if (TRUEEXP == node_get_type(car(iter)) ||
          FALSEEXP == node_get_type(car(iter))) {
        ErrorMgr_error_invalid_enum_value(errmgr, car(iter));
      }
    }

    flatten_declare_constants_within_list(st, layer, value_list);

    symbolicType = SymbType_create(env, SYMB_TYPE_ENUM, value_list);

    break;
  }

  case INTEGER:
    symbolicType = SymbType_create(env, SYMB_TYPE_INTEGER,  Nil /*body*/);
    break;

  case REAL:
    symbolicType = SymbType_create(env, SYMB_TYPE_REAL, Nil /*body*/);
    break;

  /* [SM] We have to handle the CONTINUOUS type here */
  case CONTINUOUS: {
    symbolicType = SymbType_create(env, SYMB_TYPE_CONTINUOUS, Nil /*body*/);
    break;
  }

  case UNSIGNED_WORD:
  case SIGNED_WORD: {
    /* the correctness of the width-expression is checked
       in the type-checking phase.
       Then the width expression is evaluated and checked for
       being constant NUMBER.
    */
    node_ptr num = CompileFlatten_resolve_number(st, car(type), context);
    if ((node_ptr) NULL == num || NUMBER != node_get_type(num)) {
      ErrorMgr_error_not_constant_width_of_word_type(errmgr, name);
    }
    symbolicType = SymbType_create(env, node_get_type(type) == SIGNED_WORD ?
                                   SYMB_TYPE_SIGNED_WORD : SYMB_TYPE_UNSIGNED_WORD,
                                   num);

    break;
  }

  case WORDARRAY_TYPE: {
    /* the correctness of the width-expressions are checked
       in the type-checking phase.
       Then the width expressions are evaluated and checked for
       being constant NUMBER.
    */
    const WordNumberMgr_ptr words =
        WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

    node_ptr dim1 = CompileFlatten_resolve_number(st, car(type), context);
    SymbType_ptr subtype;

    if (NUMBER != node_get_type(dim1) ) {
      ErrorMgr_error_not_constant_width_of_word_array_type(errmgr, name);
    }

    subtype =
        Compile_InstantiateType(st, layer,
                                find_node(nodemgr, WORDARRAY, name, dim1),
                                cdr(type), context, expand_bounded_arrays);

    /* do not expand arrays; keep the array information */
    if (!expand_bounded_arrays) {
      SymbType_ptr subtype =
        Compile_InstantiateType(st, layer,
                                find_node(nodemgr, WORDARRAY, name, dim1),
                                cdr(type), context, expand_bounded_arrays);
      symbolicType = SymbType_create(env, SYMB_TYPE_WORDARRAY,
                                     new_node(nodemgr, CONS, dim1,
                                              NODE_PTR(subtype)));
    }
    else {
      /* [AI] currently the array expansion code works for 1 dimensional array
         need to extend the functionality
         once done remove the below error and assert */
      ErrorMgr_internal_error(errmgr, "Array expansion in progress");
      nusmv_assert(false);

      { /* below is the array expansion trick */
          SymbType_ptr subtype =
            Compile_InstantiateType(st, layer,
                                    find_node(nodemgr, ARRAY, name, dim1),
                                    cdr(type), context, expand_bounded_arrays);
          int size = node_get_int(dim1);
          WordNumber_ptr maxWord = WordNumberMgr_max_unsigned_value(words, size);
          unsigned long long maxValue = WordNumber_get_unsigned_value(maxWord);
          symbolicType = SymbType_create_array(subtype, 0, maxValue - 1 );
      }
    }
    break;

  }

    /* unbounded array with integer index */
  case INTARRAY_TYPE: {
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    SymbType_ptr subtype;

    subtype = Compile_InstantiateType(st, layer,
                                      find_node(nodemgr, INTARRAY, name, car(type)),
                                      car(type), context, false);
    if (SymbType_is_enum(subtype)) {
      StreamMgr_print_error(streams,
                            "Enum/Range as element type for unbounded arrays not supported '%N",
                            type);
      StreamMgr_print_error(streams,  "'\n");
      ErrorMgr_error_not_supported_feature(errmgr, "");
    }

    symbolicType = SymbType_create(env, SYMB_TYPE_INTARRAY,
                                   new_node(nodemgr, CONS, NODE_PTR(subtype), Nil));

    break;
  }

    /* Array or matrix type */
  case ARRAY_TYPE: {
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    node_ptr tmp;
    node_ptr lower, upper;
    int lower_num, upper_num;
    SymbType_ptr subtype;

    /* Resolve the associated range and the subtype */
    tmp = car(car(type));
    lower = CompileFlatten_resolve_number(st, tmp, context);
    if ((node_ptr) NULL == lower) {
      /* error handling */
      extern int nusmv_yylineno;

      StreamMgr_print_error(streams,  "Unexpected value at token '");
      StreamMgr_nprint_error(streams, wffprint, "%N", tmp);
      StreamMgr_print_error(streams,  "'\n");
      nusmv_yylineno = node_get_lineno(tmp);
      ErrorMgr_error_expected_number(errmgr);
    }

    tmp = cdr(car(type));
    upper = CompileFlatten_resolve_number(st, tmp, context);
    if ((node_ptr) NULL == upper) {
      /* error handling */
      extern int nusmv_yylineno;

      StreamMgr_print_error(streams,  "Unexpected value at token '");
      StreamMgr_nprint_error(streams, wffprint, "%N", tmp);
      StreamMgr_print_error(streams,  "'\n");
      nusmv_yylineno = node_get_lineno(tmp);
      ErrorMgr_error_expected_number(errmgr);
    }

    /* here lower and upper are resolved to numbers */
    lower_num = compile_flatten_get_int(lower);
    upper_num = compile_flatten_get_int(upper);

    subtype = Compile_InstantiateType(st, layer,
                                      find_node(nodemgr, ARRAY, name, lower),
                                      cdr(type), context, false);

    symbolicType = SymbType_create_array(subtype, lower_num, upper_num);
    break;
  }

  case NFUNCTION_TYPE: {
    NFunction_ptr nfunction = NULL;
    node_ptr ftype = car(type);
    node_ptr rtype = cdr(type);
    SymbType_ptr ret = Compile_InstantiateType(st, layer, name, rtype, context, false);
    NodeList_ptr args_list = NodeList_create_from_list(ftype);
    int length = NodeList_get_length(args_list);
    SymbType_ptr* args = ALLOC(SymbType_ptr, length);

    ListIter_ptr iter;
    node_ptr arg;
    int i = 0;
    NODE_LIST_FOREACH(args_list, iter){
      arg = NodeList_get_elem_at(args_list, iter);
      args[i++] = Compile_InstantiateType(st, layer, name, arg, context, false);
    }

    NodeList_destroy(args_list);
    nfunction = NFunction_create_uninterpreted(length, args, ret);
    symbolicType = SymbType_create_nfunction(env, nfunction);
    _check_supported_function_types(env, symbolicType, name);
    break;
  }
  case MODTYPE:
  case PROCESS:
  default:
    ErrorMgr_internal_error(errmgr, "Compile_InstantiateType: type = %d",
                   node_get_type(type));
    break;
  } /* switch */

  nusmv_assert(NULL != symbolicType);
  return symbolicType;
}

boolean Compile_DeclareVariable(SymbTable_ptr st, SymbLayer_ptr layer,
                                node_ptr name, SymbType_ptr type,
                                node_ptr context,
                                Instantiation_Variables_Mode_Type mode)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  boolean result = false;
  ResolveSymbol_ptr rs;

  rs = SymbTable_resolve_symbol(st, name, context);
  name = ResolveSymbol_get_resolved_name(rs);

  /* Detect name clashes between Nil-context vars and constants */
  if ((DOT == node_get_type(name)) && (Nil == car(name)) &&
      (!SymbLayer_can_declare_constant(layer, cdr(name))))  {
    ErrorMgr_error_ambiguous(errmgr, name);
  }

  if (!SymbLayer_can_declare_var(layer, name)) {
    /* a more precise error message */
    if (SymbTable_is_symbol_parameter(st, name)) ErrorMgr_error_shadowing(errmgr, name);
    else ErrorMgr_error_redefining(errmgr, name);
  }

  /* process special cases.
     If one of special cases is detected then required declarations are done
     in below switch and yype is set to NULL. */
  switch (SymbType_get_tag(type)) {
  case SYMB_TYPE_ENUM: {
    /* special case is enumeration of 1 value => declare a define
       instead of a var */
    node_ptr values = SymbType_get_enum_type_values(type);
    nusmv_assert(values != 0); /* every enum type has some values */
    if (!(opt_keep_single_value_vars(opts)) &&
        (Nil == cdr(values))) {
      const MasterPrinter_ptr wffprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

      SymbLayer_declare_define(layer, name, context, car(values));
      SymbType_destroy(type);
      type = SYMB_TYPE(NULL);

      StreamMgr_print_error(streams,  "WARNING: single-value variable '");
      StreamMgr_nprint_error(streams, wffprint, "%N", name);
      StreamMgr_print_error(streams,  "' has been stored as a constant\n");
    }
    break;
  }

  case SYMB_TYPE_BOOLEAN:
  case SYMB_TYPE_INTEGER:
  case SYMB_TYPE_REAL:
  case SYMB_TYPE_CONTINUOUS: /* cannot work, added for future */
  case SYMB_TYPE_SIGNED_WORD:
  case SYMB_TYPE_UNSIGNED_WORD:
  case SYMB_TYPE_WORDARRAY:
  case SYMB_TYPE_INTARRAY:
    break; /* nothing special here */

  case SYMB_TYPE_ARRAY: {
    /* Array is fully special:
       Declare the array as a known symbol in this layer which is not a
       variable nor a constant nor a define.
       The recursively declare array elements.*/
    SymbType_ptr subtype;
    int lower, upper, i;

    result = true;
    SymbLayer_declare_variable_array(layer, name, type);

    subtype = SymbType_get_array_subtype(type);
    lower = SymbType_get_array_lower_bound(type);
    upper = SymbType_get_array_upper_bound(type);

    for (i=lower; i<=upper; ++i) {
      node_ptr index = find_node(nodemgr, NUMBER, NODE_FROM_INT(i), Nil);
      if (SymbLayer_can_declare_constant(layer, index)) {
        SymbLayer_declare_constant(layer, index);
      }
      /* Creates the name[i] variable (which could be an array as well)*/
      Compile_DeclareVariable(st, layer, find_node(nodemgr, ARRAY, name, index),
                              SymbType_copy(subtype), context, mode);
    }
    type = NULL; /* all declarations are done */
    break;
  }


  default:
    error_unreachable_code(); /* unsupported type */
  }

  /* if type is not NULL then this is a normal case
     and a variable has to be declared */
  if (SYMB_TYPE(NULL) != type) {
    result = true;
    switch (mode) {
    case State_Variables_Instantiation_Mode:
      SymbLayer_declare_state_var(layer, name, type);
      break;
    case Input_Variables_Instantiation_Mode:
      SymbLayer_declare_input_var(layer, name, type);
      break;
    case Frozen_Variables_Instantiation_Mode:
      SymbLayer_declare_frozen_var(layer, name, type);
      break;
    default:
      error_unreachable_code(); /* impossible mode */
      break;
    }
  }

  return result;
}

boolean Compile_DeclareFunction(SymbTable_ptr st, SymbLayer_ptr layer,
                                node_ptr name, SymbType_ptr type,
                                node_ptr context,
                                Instantiation_Functions_Mode_Type mode)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  boolean result = false;
  ResolveSymbol_ptr rs;
  NFunction_ptr nfunction;
  node_ptr local_name = name;

  rs = SymbTable_resolve_symbol(st, name, context);
  name = ResolveSymbol_get_resolved_name(rs);

  /* Detect name clashes between Nil-context vars and constants */
  if ((DOT == node_get_type(name)) && (Nil == car(name)) &&
      (!SymbLayer_can_declare_constant(layer, cdr(name))))  {
    ErrorMgr_error_ambiguous(errmgr, name);
  }

  if (!SymbLayer_can_declare_var(layer, name)) {
    /* a more precise error message */
    if (SymbTable_is_symbol_parameter(st, name)) ErrorMgr_error_shadowing(errmgr, name);
    else ErrorMgr_error_redefining(errmgr, name);
  }

  if (!SymbLayer_can_declare_function(layer, cdr(name)))  {
    ErrorMgr_error_ambiguous(errmgr, name);
  }

  /* if type is not NULL then this is a normal case
     and a variable has to be declared */
  if (SYMB_TYPE(NULL) != type) {
    result = true;
    switch (mode) {
    case State_Functions_Instantiation_Mode:
      error_unreachable_code(); /* currently unsupported */
      break;
    case Frozen_Functions_Instantiation_Mode:
      SymbLayer_declare_function(layer, local_name, context, type);
      break;
    default:
      error_unreachable_code(); /* impossible mode */
      break;
    }
  }

  return result;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void compile_make_params_hrc(const NuSMVEnv_ptr env,
                             node_ptr basename,
                             node_ptr actual_list,
                             node_ptr formal_list,
                             HrcNode_ptr hrc_result)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  Olist_ptr hrc_formals, hrc_actuals;

  nusmv_assert(HRC_NODE(NULL) != hrc_result);

  /* assume that there are no parameters in hrc_result.
     Otherwise the replace function does not work anymore.
   */
  nusmv_assert(Oiter_is_end(HrcNode_get_actual_parameters_iter(hrc_result)));
  nusmv_assert(Oiter_is_end(HrcNode_get_formal_parameters_iter(hrc_result)));

  /* DO NOT CHANGE nusmv_yylineno, now it points to declared instance basename */

  hrc_formals = Olist_create();
  hrc_actuals = Olist_create();
  while (formal_list) {
    node_ptr actual_parameter;
    node_ptr formal_parameter_hrc;
    node_ptr formal_parameter_node;
    node_ptr actual_parameter_node;

    if (!actual_list) {
      if (basename != (node_ptr) NULL) {
        const MasterPrinter_ptr wffprint =
          MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

        StreamMgr_print_error(streams,  "While creating instance ");
        StreamMgr_nprint_error(streams, wffprint, "%N", basename);
        ErrorMgr_rpterr(errmgr, "too few actual parameters");
      }
      else {
        ErrorMgr_rpterr(errmgr, "module 'main' cannot have formal parameters");
      }
    }

    /* get the current actual and formal parameters */
    formal_parameter_hrc = car(formal_list);
    actual_parameter = car(actual_list);

    /* advance actual and formal lists */
    formal_list = cdr(formal_list);
    actual_list = cdr(actual_list);

    /* [SM] When parameters will have a type Nil will be replaced by
     * the type */
    formal_parameter_node = cons(nodemgr, formal_parameter_hrc, Nil);
    Olist_append(hrc_formals, formal_parameter_node);

    /* When parameters will have a type Nil must be replaced */
    actual_parameter_node = cons(nodemgr, actual_parameter, Nil);
    Olist_append(hrc_actuals, actual_parameter_node);
  }


  /* the list passed to the function contains the elements in the
     reversed order (the parser builds a reversed list of elements) */
  Olist_reverse(hrc_actuals);
  Olist_reverse(hrc_formals);

  /* thw two lists now belong the the hrc node */
  HrcNode_replace_formal_parameters(hrc_result, hrc_formals);
  HrcNode_replace_actual_parameters(hrc_result, hrc_actuals);

  if (actual_list) ErrorMgr_rpterr(errmgr, "too many actual parameters");
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Given a fully resolved array name and its type the function
   adds all the variables in the array to the hierarchy


*/
static void compile_add_vars_to_hierarhcy(node_ptr name, SymbType_ptr type,
                                          FlatHierarchy_ptr fh)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(type));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  SymbType_ptr subtype = SymbType_get_array_subtype(type);
  int lower = SymbType_get_array_lower_bound(type);
  int upper = SymbType_get_array_upper_bound(type);
  int i;

  for (i=lower; i<=upper; ++i) {
    node_ptr index = find_node(nodemgr, NUMBER, NODE_FROM_INT(i), Nil);
    node_ptr new_name = find_node(nodemgr, ARRAY, name, index);
    if (SymbType_is_array(subtype)) { /* still array => recursively go down */
      compile_add_vars_to_hierarhcy(new_name, subtype, fh);
    }
    else { /* a var was reached => add to hierarchy */
      FlatHierarchy_add_var(fh, new_name);
    }
  } /* for */
}

/*!
  \brief Instantiates the given variable.

  It takes as input a variable and a context, and
   depending on the type of the variable some operation are performed in order
   to instantiate it in the given context:
   <br><br>
   <ul>
   <li><b>BOOLEAN</b><br>
   if the variable is of type boolean, then we add an entry in
   <code>symbol_hash</code> saying that the variable values are <code>{0,1}
   </code>.</li>
   <li><b>RANGE</b><br>
   if the variable is a range of the form <code>M..N</code>, then
   we add an entry in the <code>symbol_hash</code> saying that the
   variable values are <code>{M, M+1, ..., N-1, N}</code>. If
   <code>M</code> is less or equal to <code>N</code>, than an error occurs.
   </li> <li><b>ENUMERATION</b><br>
   if the variable is a scalar variable whose possible values are
   <code>{a1, a2, ..., aN}</code>, then we add an entry in the
   <code>symbol_hash</code> saying that the variable values are
   <code>{a1, ..., aN}</code>. </li>
   <li><b>ARRAY</b><br>
   for each element of the array it is created the corresponding
   symbol. Suppose you have the following definition "<code>VAR
   x : array 1..4 of boolean;</code>". We call this function
   for 4 times, asking at each call <code>i</code> (<code>i</code> from 1
   to 4) to declare the boolean variable <code>x\[i\]</code>.</li>
   <li><b>MODULE</b><br>
   If the variable is an instantiation of a module, than their
   arguments (if any) are contextualized, and passed as actual
   parameter to <code>instantiate_by_name<code> with the name of the
   instantiated module as root name (to extract its definition)
   and as variable name as the name of the module (to perform
   flattening).</li>
   <li><b>PROCESS</b><br>
   If the variable is of type process, than we extract the
   module name and args, we perform the contextualization of the
   process arguments and we perform a call to
   <tt>Compile_ConstructHierarchy</tt> using the variable name as process
   name (to perform flattening), the module name as root name (to
   extract its definition) and the computed actual parameters.</li>
   </ul><br>

   Depending on the kind of variable instantiation mode the variables of
   type boolean, scalar, and array are appended to <tt>input_variables</tt>,
   <tt>frozen_variables</tt> or <tt>state_variables</tt>, respectively.


  \sa compile_instantiate_vars
*/
static void compile_instantiate_var(const NuSMVEnv_ptr env,
                                    SymbTable_ptr st,
                                    SymbLayer_ptr layer, node_ptr name,
                                    node_ptr type, node_ptr context,
                                    node_ptr *assign, FlatHierarchy_ptr result,
                                    HrcNode_ptr hrc_result, hash_ptr instances,
                                    boolean expand_bounded_arrays)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  node_ptr hrc_var_name;
  ResolveSymbol_ptr rs;

  /* Resolve the module name in a standard way.. */
  node_ptr name_mod = find_node(nodemgr, MODTYPE, find_atom(nodemgr, context), find_atom(nodemgr, name));

  rs = SymbTable_resolve_symbol(st, name, context);

  nusmv_yylineno = node_get_lineno(name);

  hrc_var_name = name;   /* Name without context, used to build hrc */

  name = ResolveSymbol_get_resolved_name(rs);

  /* Check if the variable can be declared, and an instance with the
     same name does not exist! */
  if (!SymbLayer_can_declare_var(layer, name) ||
      (Nil != find_assoc(instances, name_mod))) {
    /* more precise error message */
    if (ResolveSymbol_is_parameter(rs)) ErrorMgr_error_shadowing(errmgr, name);
    else ErrorMgr_error_redefining(errmgr, name);
  }

  /* process the type */
  switch (node_get_type(type)) {
    /* Basic types */
  case BOOLEAN:
  case TWODOTS:
  case SCALAR:
  case INTEGER:
  case REAL:
  case UNSIGNED_WORD:
  case SIGNED_WORD:
  case WORDARRAY_TYPE:
  case INTARRAY_TYPE: {
    SymbType_ptr symbolicType = Compile_InstantiateType(st, layer,
                                                        name, type, context,
                                                        expand_bounded_arrays);

    boolean dv = Compile_DeclareVariable(st,
                                         layer,
                                         name,
                                         symbolicType,
                                         context,
                                         variable_instantiation_mode_get(env));

    if (true == dv) {
      FlatHierarchy_add_var(result, name);
    }

    if (HRC_NODE(NULL) != hrc_result) {
      if (true == dv) {
        Instantiation_Variables_Mode_Type mode =
          variable_instantiation_mode_get(env);

        node_ptr hrc_var = cons(nodemgr, hrc_var_name, type);

        switch (mode) {
        case State_Variables_Instantiation_Mode:
          HrcNode_add_state_variable(hrc_result, hrc_var);
          break;
        case Input_Variables_Instantiation_Mode:
          HrcNode_add_input_variable(hrc_result, hrc_var);
          break;
        case Frozen_Variables_Instantiation_Mode:
          HrcNode_add_frozen_variable(hrc_result, hrc_var);
          break;
        default:
          ErrorMgr_internal_error(errmgr, "compile_instantiate_var: impossible mode");
          break;
        }
      }
      /* Check for single-value enums declared as constants to be
         declared also into the HRC node */
      else if (SymbTable_is_symbol_define(st, name)) {
          node_ptr body = SymbTable_get_define_body(st, name);

          if (NUMBER != node_get_type(body)) {
            HrcNode_add_constants(hrc_result, cons(nodemgr, body, Nil));
            HrcNode_add_define(hrc_result, cons(nodemgr, hrc_var_name, body));
          }
          else {
            HrcNode_add_define(hrc_result, cons(nodemgr, hrc_var_name, body));
          }
      }
    }

    break;
  }

    /* Array or matrix type */
  case ARRAY_TYPE: {
    SymbType_ptr symbolicType = Compile_InstantiateType(st, layer,
                                                        name, type, context,
                                                        false);
    /* SymbLayer_declare_variable_array will be invoked in
       Compile_DeclareVariable */
    Compile_DeclareVariable(st, layer, name, symbolicType, context,
                            variable_instantiation_mode_get(env));

    compile_add_vars_to_hierarhcy(name, symbolicType, result);

    if (HRC_NODE(NULL) != hrc_result) {
      Instantiation_Variables_Mode_Type mode =
        variable_instantiation_mode_get(env);

      node_ptr hrc_var = cons(nodemgr, hrc_var_name, type);

      switch (mode) {
      case State_Variables_Instantiation_Mode:
        HrcNode_add_state_variable(hrc_result, hrc_var);
        break;
      case Input_Variables_Instantiation_Mode:
        HrcNode_add_input_variable(hrc_result, hrc_var);
        break;
      case Frozen_Variables_Instantiation_Mode:
        HrcNode_add_frozen_variable(hrc_result, hrc_var);
        break;
      default:
        ErrorMgr_internal_error(errmgr, "compile_instantiate_var: impossible mode");
        break;
      }
      /*
         Note that for array only entire array variable (array r..m of
         type) is kept in the hrc structure. So single variables
         created by the flattener for the array are NOT contained in
         the hrc structure.
      */
    }

    break;
  }

   /* Module Instantiation */
  case MODTYPE: {
    Pair pair;
    node_ptr actual;

    Pair_init(&pair, nodemgr, context);

    actual = map_param(nodemgr, put_in_context, cdr(type), &pair);

    /* Insert the instance in the instances map */
    insert_assoc(instances, name_mod, NODE_FROM_INT(1));

    if (HRC_NODE(NULL) == hrc_result) {
      compile_instantiate_by_name(env, st, layer, car(type), name, actual,
                                  assign, result, HRC_NODE(NULL),
                                  instances, expand_bounded_arrays);
    }
    else {
      HrcNode_ptr hrc_child;
      node_ptr crude_mod_name;
      node_ptr mod_name;
      node_ptr mod_def;

      /* In this way a unique reference to the module name is used,
         so the module name can be used as a key. */
      crude_mod_name = car(type);
      mod_name = find_atom(nodemgr, crude_mod_name);

      mod_def  = lookup_module_hash(env, mod_name);
      if (NODE_PTR(Nil) == mod_def) {
        /* The module is undefined */
        ErrorMgr_error_undefined(errmgr, mod_name);
      }

      hrc_child = HrcNode_create(env);
      HrcNode_set_symbol_table(hrc_child, st);
      HrcNode_set_name(hrc_child, crude_mod_name);
      HrcNode_set_lineno(hrc_child, node_get_lineno(mod_def));
      HrcNode_set_instance_name(hrc_child, hrc_var_name);
      HrcNode_set_parent(hrc_child, hrc_result);
      HrcNode_add_child_hrc_node(hrc_result, hrc_child);

      /* Adds formal/actual parameters to the module instance.
         This step is not performed in the make_params function
         because non-flattened actual parameters are needed in hrc
         structure.
       */
      compile_make_params_hrc(env,
                              name,
                              cdr(type) /* non-flattened actual parameters */,
                              car(mod_def),
                              hrc_child);

      compile_instantiate_by_name(env, st, layer, car(type), name, actual,
                                  assign, result, hrc_child, instances,
                                  expand_bounded_arrays);
    }
    free_list(nodemgr, actual);
    break;
  }

    /* Module process instantiation */
  case PROCESS: {
    Pair pair;
    node_ptr actual, pmod_name, pmod_args;

    if (HRC_NODE(NULL) != hrc_result) {
      HrcNode_ptr root = get_hrc_root_node(hrc_result);
      /* Set a flag so we know that the HRC hierarchy is not usable */
      HrcNode_set_undef(root, (void*)~0);
      hrc_result = HRC_NODE(NULL);
    }
    pmod_name = car(car(type));
    pmod_args = cdr(car(type));

    Pair_init(&pair, nodemgr, context);

    actual = map_param(nodemgr, put_in_context, pmod_args, &pair);

    Compile_ConstructHierarchy(env,
                               st,
                               layer,
                               pmod_name,
                               name,
                               actual,
                               result,
                               hrc_result,
                               instances,
                               expand_bounded_arrays);
    free_list(nodemgr, actual);
    break;
  }

  default:
    ErrorMgr_internal_error(errmgr, "compile_instantiate_var: type = %d",
                   node_get_type(type));
    break;
  }
}


/*!
  \brief Instantiates the given function.

  It takes as input a function and a context, and
   depending on the type of the function some operation are performed in order
   to instantiate it in the given context.

   Depending on the kind of function instantiation mode the functions are of
   type <tt>state_functions</tt> or <tt>frozen_functions</tt>, respectively.


  \sa compile_instantiate_funs
*/

static void compile_instantiate_fun(const NuSMVEnv_ptr env,
                                    SymbTable_ptr st,
                                    SymbLayer_ptr layer, node_ptr name,
                                    node_ptr type, node_ptr context,
                                    node_ptr *assign, FlatHierarchy_ptr result,
                                    HrcNode_ptr hrc_result, hash_ptr instances)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  node_ptr hrc_fun_name;
  ResolveSymbol_ptr rs;

  /* Resolve the module name in a standard way.. */
  node_ptr name_mod = find_node(nodemgr, MODTYPE, find_atom(nodemgr, context), find_atom(nodemgr, name));

  rs = SymbTable_resolve_symbol(st, name, context);

  nusmv_yylineno = node_get_lineno(name);

  hrc_fun_name = name;   /* Name without context, used to build hrc */

  name = ResolveSymbol_get_resolved_name(rs);

  /* Check if the variable can be declared, and an instance with the
     same name does not exist! */
  if (!SymbLayer_can_declare_var(layer, name) ||
      (Nil != find_assoc(instances, name_mod))) {
    /* more precise error message */
    if (ResolveSymbol_is_parameter(rs)) ErrorMgr_error_shadowing(errmgr, name);
    else ErrorMgr_error_redefining(errmgr, name);
  }

  /* process the type */
  switch (node_get_type(type)) {
    /* Basic types */
  case NFUNCTION_TYPE: {
    SymbType_ptr symbolicType = Compile_InstantiateType(st, layer,
                                                        name, type, context, false);
    /* SymbLayer_declare_variable_array will be invoked in
       Compile_DeclareVariable */
    Compile_DeclareFunction(st, layer, name, symbolicType, context,
                            function_instantiation_mode_get(env));

    if (HRC_NODE(NULL) != hrc_result) {
      Instantiation_Variables_Mode_Type mode =
        function_instantiation_mode_get(env);

      node_ptr hrc_fun = cons(nodemgr, hrc_fun_name, type);

      switch (mode) {
      case Frozen_Functions_Instantiation_Mode:
        HrcNode_add_frozen_function(hrc_result, hrc_fun);
        break;
      default:
        ErrorMgr_internal_error(errmgr, "compile_instantiate_fun: impossible mode");
        break;
      }
    }

    break;
  }

  default:
    ErrorMgr_internal_error(errmgr, "compile_instantiate_fun: type = %d",
                   node_get_type(type));
    break;
  }
}

/*!
  \brief Recursively applies <tt>compile_instantiate_var</tt>.

  Recursively applies <tt>compile_instantiate_var</tt> to
   a given list of variables declaration, and performs some check for
   multiple variable definitions.

  \sa compile_instantiate_var
*/
static void compile_instantiate_vars(const NuSMVEnv_ptr env,
                                     SymbTable_ptr st,
                                     SymbLayer_ptr layer, node_ptr var_list,
                                     node_ptr mod_name,
                                     node_ptr *assign,
                                     FlatHierarchy_ptr result,
                                     HrcNode_ptr hrc_result,
                                     hash_ptr instances,
                                     boolean expand_bounded_arrays)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr rev_vars_list;
  node_ptr iter;

  rev_vars_list = reverse_ns(nodemgr, var_list);
  iter = rev_vars_list;
  while (iter != Nil) {
    node_ptr cur_var = car(iter);
    node_ptr name = car(cur_var);
    node_ptr type = cdr(cur_var);

    compile_instantiate_var(env,
                            st,
                            layer,
                            name,
                            type,
                            mod_name,
                            assign,
                            result,
                            hrc_result,
                            instances,
                            expand_bounded_arrays);

    iter = cdr(iter);
  }

  free_list(nodemgr, rev_vars_list);
}

/*!
  \brief Recursively applies <tt>compile_instantiate_fun</tt>.

  Recursively applies <tt>compile_instantiate_fun</tt> to
   a given list of functions declaration, and performs some check for
   multiple function definitions.

  \sa compile_instantiate_fun
*/

static void compile_instantiate_funs(const NuSMVEnv_ptr env,
                                     SymbTable_ptr st,
                                     SymbLayer_ptr layer, node_ptr fun_list,
                                     node_ptr mod_name,
                                     node_ptr *assign,
                                     FlatHierarchy_ptr result,
                                     HrcNode_ptr hrc_result,
                                     hash_ptr instances)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr rev_funs_list;
  node_ptr iter;

  rev_funs_list = reverse_ns(nodemgr, fun_list);
  iter = rev_funs_list;
  while (iter != Nil) {
    node_ptr cur_fun = car(iter);
    node_ptr name = car(cur_fun);
    node_ptr type = cdr(cur_fun);

    compile_instantiate_fun(env,
                            st,
                            layer,
                            name,
                            type,
                            mod_name,
                            assign,
                            result,
                            hrc_result,
                            instances);

    iter = cdr(iter);
  }

  free_list(nodemgr, rev_funs_list);
}

/*!
  \brief This function takes a TWODOTS node, and tries to resolve
   the bounds to integer numbers which are returned.

  If it is not possible to resolve the bounds to numbers,
   an error is issued.
*/
static void resolve_range(SymbTable_ptr st,
                          node_ptr range, node_ptr context,
                          int* low, int* high)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  node_ptr ndim;

  nusmv_assert(TWODOTS == node_get_type(range));

  ndim = CompileFlatten_resolve_number(st, car(range), context);

  if ((node_ptr) NULL == ndim || NUMBER != node_get_type(ndim)) {
    nusmv_yylineno = node_get_lineno(range);
    ErrorMgr_error_invalid_subrange(errmgr, range);
  }
  *low = node_get_int(ndim);

  ndim = CompileFlatten_resolve_number(st, cdr(range), context);
  if ((node_ptr) NULL == ndim || NUMBER != node_get_type(ndim)) {
    nusmv_yylineno = node_get_lineno(range);
    ErrorMgr_error_invalid_subrange(errmgr, range);
  }
  *high = node_get_int(ndim);
}

/*!
  \brief Put a variable in the current "context"

  Put a variable in the current "context", where context
   is the second argument of the pair instance that must be passed to
   this function

  \se None
*/
static node_ptr put_in_context(node_ptr v, void* arg)
{
  Pair_ptr p = PAIR(arg);
  NodeMgr_ptr nodemgr = NODE_MGR(Pair_get_first(p));
  node_ptr param_context = NODE_PTR(Pair_get_second(p));

  return(find_node(nodemgr, CONTEXT, param_context, v));
}


/*!
  \brief Builds the parameters of a module from the list of formal
   parameters of the module itself.

  Builds the parameters of a module from the list
   of formal parameters of the module itself and a <tt>basename</tt>.<br>
   There must be a one to one correspondence between the elements of
   <tt>actual_list</tt> and <tt>formal_list</tt> parameters. If the
   number of elements of the lists are different then, an error occurs.

  \se In the symbol table the new parameter is
   associated to the old one.
*/

static void make_params(const NuSMVEnv_ptr env,
                        SymbLayer_ptr layer,
                        node_ptr basename,
                        node_ptr actual_list,
                        node_ptr formal_list)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  /* DO NOT CHANGE nusmv_yylineno, now it points to declared instance basename */
  const MasterNormalizer_ptr normalizer =
    MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));

  while (formal_list) {
    node_ptr actual_parameter, formal_parameter_flat;

    if (!actual_list) {
      if (basename != (node_ptr) NULL) {
        const MasterPrinter_ptr wffprint =
          MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

        StreamMgr_print_error(streams,  "While creating instance ");
        StreamMgr_nprint_error(streams, wffprint, "%N", basename);
        ErrorMgr_rpterr(errmgr, "too few actual parameters");
      }
      else {
        ErrorMgr_rpterr(errmgr, "module 'main' cannot have formal parameters");
      }
    }

    /* get the current actual and formal parameters */
    formal_parameter_flat =
      find_node(nodemgr, DOT, basename, find_atom(nodemgr, car(formal_list)));

    formal_parameter_flat = MasterNormalizer_normalize_node(normalizer, formal_parameter_flat);

    actual_parameter = car(actual_list);

    if (!SymbLayer_can_declare_parameter(layer, formal_parameter_flat)) {
      nusmv_yylineno = node_get_lineno(car(formal_list));
      ErrorMgr_error_redefining(errmgr, formal_parameter_flat);
    }

    SymbLayer_declare_parameter(layer, formal_parameter_flat,
                                car(actual_parameter),
                                cdr(actual_parameter));

    /* advance actual and formal lists */
    formal_list = cdr(formal_list);
    actual_list = cdr(actual_list);
  }
  if (actual_list) ErrorMgr_rpterr(errmgr, "too many actual parameters");
}

/*!
  \brief Instantiates all in the body of a module.

  This function is responsible of the
   instantiation of the body of a module. The module definition
   (parameter and body) is <tt>mod_def</tt> and the module instance name
   <tt>mod_name</tt> are passed as arguments. First we instantiate the
   arguments of the given module. Then it loops over the module
   definition searching for defined symbols (i.e. those introduced by
   the keyword <tt>DEFINE</tt>) and inserts their definition in the
   <tt>symbol_hash</tt>. After this preliminary phase it loops again
   over module body in order to performs the other instantiation, and
   to extract all the information needed to compile the automaton,
   i.e. the list of processes, the TRANS statements, the INIT
   statements, ... and so on.

   NB: After parsing and creating the module hash table, the order of
   declarations is correct (not reversed). This function reverse the order
   of SPEC, LTLSPEC, PSLSPEC, INVARSPEC, COMPUTE, JUSTICE AND COMPASSION


  \sa compile_instantiate_var compile_instantiate_vars
*/
static void compile_instantiate(const NuSMVEnv_ptr env,
                                SymbTable_ptr st,
                                SymbLayer_ptr layer,
                                node_ptr mod_def,
                                node_ptr mod_name,
                                node_ptr actual,
                                node_ptr *assign,
                                FlatHierarchy_ptr result,
                                HrcNode_ptr hrc_result,
                                hash_ptr instances,
                                boolean expand_bounded_arrays)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr mod_body_decls;
  node_ptr mod_formal_args  = car(mod_def); /* Module formal parameters */
  node_ptr mod_body         = cdr(mod_def); /* Module body */

  /* creates local parameters */
  make_params(env, layer, mod_name, actual, mod_formal_args);

  /* We first instantiate all the definitions, in case they are
     constants used in the array declarations.
     loop over module declaration
  */
  for (mod_body_decls = mod_body; mod_body_decls != Nil;
       mod_body_decls = cdr(mod_body_decls)) {

    node_ptr cur_decl = car(mod_body_decls);

    switch (node_get_type(cur_decl)) {
    case DEFINE:
      {
        node_ptr define_iter;
        /* loop over DEFINE declaration */
        for (define_iter = car(cur_decl); define_iter != Nil;
             define_iter = cdr(define_iter)) {
          node_ptr cur_define = car(define_iter);

          node_ptr local_name = car(cur_define);
          node_ptr definition = cdr(cur_define);
          ResolveSymbol_ptr rs;
          node_ptr name;

          rs = SymbTable_resolve_symbol(st, local_name, mod_name);
          name = ResolveSymbol_get_resolved_name(rs);

          nusmv_yylineno = node_get_lineno(define_iter);
          if (SymbLayer_can_declare_define(layer, name)) {
            /* If this is an array definition expand the definition
               just like array variables. Array defines are stored in
               parse-tree as normal defines, but they can be
               distinguished by ARRAY_DEF at top of the expression */
            if (ARRAY_DEF == node_get_type(definition)) {
              instantiate_array_define(st, layer, name, mod_name, definition);

              /* Inserts define array in hrc structure */
              if (HRC_NODE(NULL) != hrc_result) {
                /* Uses car(cur_define), the non-flattened name */
                node_ptr hrc_define = cons(nodemgr, car(cur_define), definition);

                /* Defines are visited in inversed order of
                   declaration, so the resulting list is in order */
                HrcNode_add_array_define(hrc_result, hrc_define);
              }

            }
            else {
              SymbLayer_declare_define(layer, name, mod_name, definition);

              if (HRC_NODE(NULL) != hrc_result) {
                /* Uses car(cur_define), the non-flattened name */
                node_ptr hrc_define = cons(nodemgr, car(cur_define), definition);

                /* Defines are visited in inversed order of
                   declaration, so the resulting list is in order */
                HrcNode_add_define(hrc_result, hrc_define);
              }
            }
          }
          else {
            nusmv_yylineno = node_get_lineno(local_name); /* set correct line info */
            /* more precise error message */
            if (SymbTable_is_symbol_parameter(st, name)) ErrorMgr_error_shadowing(errmgr, name);
            else ErrorMgr_error_redefining(errmgr, name);
          }
        }/* loop on defines */
      }
      break;
    default: break;
    }
  } /* loop over module declarations */


  /* Now, we instantiate all the other elements of a module.
     loop again over module declaration
  */
  for (mod_body_decls = mod_body; mod_body_decls != Nil;
       mod_body_decls = cdr(mod_body_decls)) {
    node_ptr cur_decl = car(mod_body_decls);
    node_ptr tmp;
    nusmv_yylineno = node_get_lineno(cur_decl);

    switch (node_get_type(cur_decl)) {
    case ISA:
      /* [AMa] Here ISAa must be processed when HRC supports them */
      if (HRC_NODE(NULL) != hrc_result) {
        HrcNode_ptr root = get_hrc_root_node(hrc_result);
        /* Set a flag so we know that the HRC hierarchy is not usable */
        HrcNode_set_undef(root, (void*)~0);
        hrc_result = HRC_NODE(NULL);
      }
      compile_instantiate_by_name(env, st, layer, car(cur_decl), mod_name,
                                  Nil, assign, result, hrc_result, instances,
                                  expand_bounded_arrays);
      break;

    case FUN:
      set_function_instantiation_to_frozen(env);
      compile_instantiate_funs(env, st, layer, car(cur_decl), mod_name,
                               assign, result, hrc_result, instances);
      break;

    case VAR:
      compile_instantiate_vars(env, st, layer, car(cur_decl), mod_name,
                               assign, result, hrc_result, instances,
                               expand_bounded_arrays);
      break;

    case FROZENVAR:
      set_variable_instantiation_to_frozen(env);
      compile_instantiate_vars(env, st, layer, car(cur_decl), mod_name,
                               assign, result, hrc_result, instances,
                               expand_bounded_arrays);
      set_variable_instantiation_to_state(env);
      break;

    case IVAR:
      set_variable_instantiation_to_input(env);
      compile_instantiate_vars(env, st, layer, car(cur_decl), mod_name,
                               assign, result, hrc_result, instances,
                               expand_bounded_arrays);
      set_variable_instantiation_to_state(env);
      break;

    case TRANS:
      tmp = find_node(nodemgr, AND, FlatHierarchy_get_trans(result),
                      find_node(nodemgr, CONTEXT, mod_name, car(cur_decl)));
      if (HRC_NODE(NULL) != hrc_result)
        HrcNode_add_trans_expr(hrc_result, car(cur_decl));
      FlatHierarchy_set_trans(result, tmp);
      break;

    case INIT:
      tmp = find_node(nodemgr, AND, FlatHierarchy_get_init(result),
                      find_node(nodemgr, CONTEXT, mod_name, car(cur_decl)));
      if (HRC_NODE(NULL) != hrc_result)
        HrcNode_add_init_expr(hrc_result, car(cur_decl));
      FlatHierarchy_set_init(result, tmp);
      break;

    case INVAR:
      tmp = find_node(nodemgr, AND, FlatHierarchy_get_invar(result),
                      find_node(nodemgr, CONTEXT, mod_name, car(cur_decl)));
      if (HRC_NODE(NULL) != hrc_result)
        HrcNode_add_invar_expr(hrc_result, car(cur_decl));
      FlatHierarchy_set_invar(result, tmp);
      break;

   /* ---------------------------------------------------------------------- */
   /* contexts of all kind of properties are 'flattened' here                */
    case SPEC:
      {
        node_ptr property_name = cdr(cur_decl);

        if (node_get_type(car(cur_decl)) == CONTEXT) {
          /* concatenates local context to the current module */
          node_ptr new_ctx = CompileFlatten_concat_contexts(env, mod_name,
                                                            caar(cur_decl));
          tmp = find_node(nodemgr, CONTEXT, new_ctx, cdr(car(cur_decl)));
        }
        else tmp = find_node(nodemgr, CONTEXT, mod_name, car(cur_decl));

        /* Support for property names */
        if (Nil != property_name) {
          property_name = CompileFlatten_concat_contexts(env, mod_name,
                                                         property_name);
          if (!FlatHierarchy_add_property_name(result, property_name)){
            ErrorMgr_error_redefining(errmgr, property_name);
          }
        }

        tmp = find_node(nodemgr, SPEC, tmp, property_name);
        tmp = cons(nodemgr, tmp, FlatHierarchy_get_spec(result));
        FlatHierarchy_set_spec(result, tmp);
        if (HRC_NODE(NULL) != hrc_result)
          HrcNode_add_ctl_property_expr(hrc_result, cur_decl);
      }
      break;

    case LTLSPEC:
      {
        node_ptr property_name = cdr(cur_decl);

        if (node_get_type(car(cur_decl)) == CONTEXT) {
          /* concatenates local context to the current module */
          node_ptr new_ctx = CompileFlatten_concat_contexts(env, mod_name,
                                                            caar(cur_decl));
          tmp = find_node(nodemgr, CONTEXT, new_ctx, cdr(car(cur_decl)));
        }
        else tmp = find_node(nodemgr, CONTEXT, mod_name, car(cur_decl));

        /* Support for property names */
        if (Nil != property_name) {
          property_name = CompileFlatten_concat_contexts(env, mod_name,
                                                         property_name);
          if (!FlatHierarchy_add_property_name(result, property_name)){
            ErrorMgr_error_redefining(errmgr, property_name);
          }
        }
        tmp = find_node(nodemgr, LTLSPEC, tmp, property_name);

        tmp = cons(nodemgr, tmp, FlatHierarchy_get_ltlspec(result));
        FlatHierarchy_set_ltlspec(result, tmp);

        if (HRC_NODE(NULL) != hrc_result)
          HrcNode_add_ltl_property_expr(hrc_result, cur_decl);
      }
      break;

    case PSLSPEC:
      {
        node_ptr property_name = cdr(cur_decl);

        if (node_get_type(car(cur_decl)) == CONTEXT) {
          /* concatenates local context to the current module */
          node_ptr new_ctx = CompileFlatten_concat_contexts(env, mod_name,
                                                            caar(cur_decl));
          tmp = PslNode_new_context(nodemgr, new_ctx, cdr(car(cur_decl)));
        }
        else tmp = find_node(nodemgr, CONTEXT, mod_name, car(cur_decl));

        /* Support for property names */
        if (Nil != property_name) {
          property_name = CompileFlatten_concat_contexts(env,mod_name,
                                                         property_name);
          if (!FlatHierarchy_add_property_name(result, property_name)){
            ErrorMgr_error_redefining(errmgr, property_name);
          }
        }
        tmp = find_node(nodemgr, PSLSPEC, tmp, property_name);

        tmp = cons(nodemgr, tmp, FlatHierarchy_get_pslspec(result));
        FlatHierarchy_set_pslspec(result, tmp);

        if (HRC_NODE(NULL) != hrc_result)
          HrcNode_add_psl_property_expr(hrc_result, cur_decl);
      }
      break;

    case INVARSPEC:
      {
        node_ptr property_name = cdr(cur_decl);

        if (node_get_type(car(cur_decl)) == CONTEXT) {
          /* concatenates local context to the current module */
          node_ptr new_ctx = CompileFlatten_concat_contexts(env, mod_name,
                                                            caar(cur_decl));
          tmp = find_node(nodemgr, CONTEXT, new_ctx, cdr(car(cur_decl)));
        }
        else tmp = find_node(nodemgr, CONTEXT, mod_name, car(cur_decl));

        /*  Support for property names */
        if (Nil != property_name) {
          property_name = CompileFlatten_concat_contexts(env, mod_name,
                                                         property_name);
          if (!FlatHierarchy_add_property_name(result, property_name)){
            ErrorMgr_error_redefining(errmgr, property_name);
          }
        }
        tmp = find_node(nodemgr, INVARSPEC, tmp, property_name);

        tmp = cons(nodemgr, tmp, FlatHierarchy_get_invarspec(result));
        FlatHierarchy_set_invarspec(result, tmp);

        if (HRC_NODE(NULL) != hrc_result)
          HrcNode_add_invar_property_expr(hrc_result, cur_decl);
      }
      break;

    case COMPUTE:
      {
        node_ptr property_name = cdr(cur_decl);

        if (node_get_type(car(cur_decl)) == CONTEXT) {
          /* concatenates local context to the current module */
          node_ptr new_ctx = CompileFlatten_concat_contexts(env, mod_name,
                                                            caar(cur_decl));
          tmp = find_node(nodemgr, CONTEXT, new_ctx, cdr(car(cur_decl)));
        }
        else tmp = find_node(nodemgr, CONTEXT, mod_name, car(cur_decl));

        /*  Support for property names */
        if (Nil != property_name) {
          property_name = CompileFlatten_concat_contexts(env, mod_name,
                                                         property_name);
          if (!FlatHierarchy_add_property_name(result, property_name)){
            ErrorMgr_error_redefining(errmgr, property_name);
          }
        }
        tmp = find_node(nodemgr, COMPUTE, tmp, property_name);

        tmp = cons(nodemgr, tmp, FlatHierarchy_get_compute(result));
        FlatHierarchy_set_compute(result, tmp);

        if (HRC_NODE(NULL) != hrc_result)
          HrcNode_add_compute_property_expr(hrc_result, cur_decl);
      }
      break;
   /* ---------------------------------------------------------------------- */

    case JUSTICE:
      tmp = cons(nodemgr, find_node(nodemgr, CONTEXT, mod_name, car(cur_decl)),
                 FlatHierarchy_get_justice(result));
      FlatHierarchy_set_justice(result, tmp);

      if (HRC_NODE(NULL) != hrc_result)
        HrcNode_add_justice_expr(hrc_result, car(cur_decl));
      break;

    case COMPASSION:
      tmp = cons(nodemgr, cons(nodemgr, find_node(nodemgr, CONTEXT, mod_name, car(car(cur_decl))),
                      find_node(nodemgr, CONTEXT, mod_name, cdr(car(cur_decl)))),
                 FlatHierarchy_get_compassion(result));
      FlatHierarchy_set_compassion(result, tmp);

      if (HRC_NODE(NULL) != hrc_result) {
        node_ptr hrc_compassion =
          cons(nodemgr, car(car(cur_decl)), cdr(car(cur_decl)));

        HrcNode_add_compassion_expr(hrc_result, hrc_compassion);
      }

      break;

    case ASSIGN:
      {
        /* an assign may be void */
        if (car(cur_decl) != Nil) {
          *assign = find_node(nodemgr, AND, *assign,
                              find_node(nodemgr, CONTEXT, mod_name, car(cur_decl)));
        }

        if (HRC_NODE(NULL) != hrc_result) {
          compile_add_assign_hrc(nodemgr, hrc_result, car(cur_decl));
        }
      }
      break;

    case DEFINE: break; /* already dealt with */

    case CONSTANTS:
      /* declares the contained constants: */
      flatten_declare_constants_within_list(st,
                                            layer,
                                            reverse_ns(nodemgr, car(cur_decl)));

      if (HRC_NODE(NULL) != hrc_result) {
        HrcNode_add_constants(hrc_result, car(cur_decl));
      }

      break;

    case PRED:
      {
        node_ptr pred_name = car(car(cur_decl));

        if (Nil != pred_name) {
          find_node(nodemgr, CONTEXT, mod_name, pred_name);
        }
        nusmv_assert(EQDEF == node_get_type(car(cur_decl)));
        tmp = find_node(nodemgr, PRED,
                        find_node(nodemgr, EQDEF, pred_name,
                                  find_node(nodemgr, CONTEXT,
                                            mod_name, cdr(car(cur_decl)))),
                        cdr(cur_decl));
        FlatHierarchy_add_pred(result, tmp);

        break;
      }
    case MIRROR:
      nusmv_assert(Nil == cdr(cur_decl));
      tmp = find_node(nodemgr, MIRROR,
                      find_node(nodemgr, CONTEXT, mod_name, car(cur_decl)),
                      Nil);
      FlatHierarchy_add_mirror(result, tmp);
      break;

      /* To be moved elsewhere */
    case DEFINE_PROPERTY:
      {
        node_ptr property_name = cdr(cur_decl);

        if (node_get_type(car(cur_decl)) == CONTEXT) {
          /* concatenates local context to the current module */
          node_ptr new_ctx = CompileFlatten_concat_contexts(env, mod_name,
                                                            caar(cur_decl));
          tmp = find_node(nodemgr, CONTEXT, new_ctx, cdr(car(cur_decl)));
        }
        else tmp = find_node(nodemgr, CONTEXT, mod_name, car(cur_decl));

        /*  Support for property names */
        if (Nil != property_name) {
          property_name = CompileFlatten_concat_contexts(env, mod_name,
                                                         property_name);
          if (!FlatHierarchy_add_property_name(result, property_name)){
            ErrorMgr_error_redefining(errmgr, property_name);
          }
        }
        tmp = find_node(nodemgr, DEFINE_PROPERTY, tmp, property_name);

        tmp = cons(nodemgr, tmp, FlatHierarchy_get_property_patterns(result));
        FlatHierarchy_set_property_patterns(result, tmp);

#if 0
        if (HRC_NODE(NULL) != hrc_result)
          HrcNode_add_property_pattern_expr(hrc_result, cur_decl);
#endif
      }
      break;

    default: error_unreachable_code(); /* unknown kind of declaration */
    }
  } /* loop over module declarations */

}

/*!
  \brief Starts the flattening from a given point in the
   module hierarchy.

  <tt>module_name</tt> is the name of the module being
   instantiated. The name of the module instance
   is <tt>instance_name</tt>. First checks if the module exists. Then it checks
   if the module is recursively defined, and if the case an error is
   printed out. If these checks are passed, then it proceeds in the
   instantiation of the body of the module.
*/
static void compile_instantiate_by_name(const NuSMVEnv_ptr env,
                                        SymbTable_ptr st,
                                        SymbLayer_ptr layer,
                                        node_ptr module_name,
                                        node_ptr instance_name,
                                        node_ptr actual,
                                        node_ptr *assign,
                                        FlatHierarchy_ptr result,
                                        HrcNode_ptr hrc_result,
                                        hash_ptr instances,
                                        boolean expand_bounded_arrays)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  node_ptr module_stack = Nil;
  node_ptr s;
  node_ptr mod_name = find_atom(nodemgr, module_name);         /* find module name */
  /* find module definition */
  node_ptr mod_def  = lookup_module_hash(env, mod_name);

  if (mod_def == (node_ptr) NULL) {
    /* The module is undefined */
    nusmv_yylineno = node_get_lineno(module_name);
    ErrorMgr_error_undefined(errmgr, module_name);
  }

  /* DO NOT CHANGE nusmv_yylineno, now it points to declared instance
     instance_name */

  /* scans module_stack in order to find if there are recursively
     defined modules */

  if (NuSMVEnv_has_value(env, ENV_MODULE_STACK)) {
    module_stack = NODE_PTR(NuSMVEnv_get_value(env, ENV_MODULE_STACK));
  }

  s = module_stack;
  while (s != Nil) {
    if (car(s) == mod_name) {
      ErrorMgr_rpterr(errmgr, "module \"%s\" is recursively defined",
                      UStringMgr_get_string_text((string_ptr)car(module_name)));
    }
    s = cdr(s);
  }

  /* append current module to module_stack */
  module_stack = cons(nodemgr, mod_name, module_stack);
  NuSMVEnv_set_or_replace_value(env, ENV_MODULE_STACK, module_stack);

  compile_instantiate(env, st, layer, mod_def, instance_name, actual,
                      assign, result, hrc_result, instances,
                      expand_bounded_arrays);

  /* elimination of current module form module_stack */
  s = cdr(module_stack);
  free_node(nodemgr, module_stack);
  module_stack = s;
  NuSMVEnv_set_or_replace_value(env, ENV_MODULE_STACK, module_stack);
}


/*!
  \brief Recursive definition of compileFlattenProcess

  Recursive definition of compileFlattenProcess.
   If running is Nil there are no processes => no need to create
   data structure with CASEs (for next-assignments).
*/
static void
compileFlattenProcessRecur(const NuSMVEnv_ptr env,
                           const SymbTable_ptr symb_table,
                           node_ptr assign, node_ptr context, node_ptr running,
                           FlatHierarchy_ptr flatHierarchy)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (assign == Nil) return;
  nusmv_yylineno = node_get_lineno(assign);
  switch (node_get_type(assign)) {
  case CONS:
  case AND:
    compileFlattenProcessRecur(env, symb_table, car(assign), context,
                               running, flatHierarchy);
    compileFlattenProcessRecur(env, symb_table, cdr(assign), context,
                               running, flatHierarchy);
    break;

  case CONTEXT:
    compileFlattenProcessRecur(env, symb_table, cdr(assign), car(assign),
                               running, flatHierarchy);
    break;

  case EQDEF:
    {
      node_ptr vname, lhsa, stored;
      node_ptr left  = car(assign);
      node_ptr right = cdr(assign);
      ResolveSymbol_ptr rs;

      switch (node_get_type(left)) {
      case SMALLINIT: /* init assignement */ {
        rs = SymbTable_resolve_symbol(symb_table, car(left), context);
        vname = ResolveSymbol_get_resolved_name(rs);
        lhsa = find_node(nodemgr, node_get_type(left), vname, Nil);
        stored = FlatHierarchy_lookup_assign(flatHierarchy, lhsa);

        if (Nil != stored) ErrorMgr_error_reassigning(errmgr, vname);
      }
        break;

      case NEXT: /* next assignement */
        {
          rs = SymbTable_resolve_symbol(symb_table, car(left), context);
          vname = ResolveSymbol_get_resolved_name(rs);
          lhsa = find_node(nodemgr, node_get_type(left), vname, Nil);
          stored = FlatHierarchy_lookup_assign(flatHierarchy, lhsa);


          /* there are processes => create CASE with "running" */
          if (NuSMVEnv_has_value(env, ENV_PROC_SELECTOR_VNAME)) {
            /* create default value for assignment, i.e. var name  */
            if (Nil == stored) stored = vname;
            /* create a CASE with running guard */
            right = new_node(nodemgr, CASE, new_node(nodemgr, COLON, running, right), stored);
          }
          else { /* no processes => no CASE things */
            if (Nil != stored) ErrorMgr_error_reassigning(errmgr, vname);
          }
        }
        break;

      default:
        /* Invariant assignment */
        {
          rs = SymbTable_resolve_symbol(symb_table, left, context);
          vname = lhsa = ResolveSymbol_get_resolved_name(rs);
          stored = FlatHierarchy_lookup_assign(flatHierarchy, lhsa);

          if (Nil != stored)  ErrorMgr_error_reassigning(errmgr, vname);
        }
      }
      FlatHierarchy_insert_assign(flatHierarchy, lhsa, right);


      break;
    } /* outer switch case EQDEF */

  default:
    ErrorMgr_internal_error(errmgr, "compileFlattenProcessRecur: type = %d",
                   node_get_type(assign));
  } /* outer switch case */

}

/*!
  \brief Creates the internal process selector variable, within
   the given layer

  Creates an input variable to denote
   the internal process selector, and the defines to denote
   the corresponding 'running' symbols.

   'process_name_list' is a list of existing processes names. If the list
   contains just one element ("main") no variables and defines are
   declared (no need). This happens if there is no "process" modules or
   the modules were flattened (which also removes "process" things).

   NB for developers: the internal process selector variable is by
   default positioned at the top of the ordering. It is attached to
   <tt>input_variables</tt> and <tt>all_variables</tt> too.

  \se <tt>input_variables</tt> and
   <tt>all_variables</tt> are affected.
*/
static void
create_process_symbolic_variables(const NuSMVEnv_ptr env,
                                  SymbTable_ptr symb_table,
                                  SymbLayer_ptr layer,
                                  node_ptr process_name_list)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  node_ptr proc_selector_vname = Nil;

  /* the list of process always contain one element */
  nusmv_assert(CONS == node_get_type(process_name_list));

  /* there is just one module (main). Therefore, nothing should be done */
  if (Nil == cdr(process_name_list)) {
    /* during flattening "main" is denoted by Nil */
    nusmv_assert(Nil == car(process_name_list));

    return;
    /* Note that the symbols "_process_selector_" or "running" may be
       already defined. This happens, for example, if flattened module
       is read. But there is no need to care about it. If there are
       no processes then user can define its one _process_selector_ or
       running as usual symbols.
    */
  }

  /* -- There are several process -- */
  ErrorMgr_warning_processes_deprecated(errmgr);

  /* initialise the global variable with "process_selector" name */
  proc_selector_vname =
    find_node(nodemgr, DOT, Nil, sym_intern(env, PROCESS_SELECTOR_VAR_NAME));
  NuSMVEnv_set_value(env, ENV_PROC_SELECTOR_VNAME, proc_selector_vname);

  {
    /* internally "main" is denoted by Nil. change now Nil to "main". */
    node_ptr l = process_name_list;
    while (Nil != l && Nil != car(l)) l = cdr(l);

    /* there should always be a Nil element ("main" module)*/
    nusmv_assert(Nil != l);
    setcar(l, sym_intern(env, "main"));
  }

  /* check that a user did not create its own  _process_selector_ */
  if (SymbTable_is_symbol_declared(symb_table, proc_selector_vname)) {
    ErrorMgr_error_redefining_operational_symbol(errmgr, proc_selector_vname);
  }

  /* declare the "process-selector" symbol with a proper values */
  {
    SymbType_ptr symbolicType;

    flatten_declare_constants_within_list(symb_table, layer, process_name_list);
    symbolicType = SymbType_create(env, SYMB_TYPE_ENUM, process_name_list);
    SymbLayer_declare_input_var(layer, proc_selector_vname, symbolicType);
  }


  /* Declare DEFINES representing "running"s symbols */
  {
    node_ptr main_atom = sym_intern(env, "main");
    node_ptr running_atom = sym_intern(env, RUNNING_SYMBOL);
    node_ptr iter;

    for (iter = process_name_list; iter != Nil; iter = cdr(iter)) {
      node_ptr module_name, def_name, def_body;
      ResolveSymbol_ptr rs;

      module_name = car(iter);

      if (module_name == main_atom) {
        /* internally main is represented as Nil */
        rs = SymbTable_resolve_symbol(symb_table, running_atom, Nil);
      }
      else {
        rs = SymbTable_resolve_symbol(symb_table, running_atom, module_name);
      }

      def_name = ResolveSymbol_get_resolved_name(rs);

      /* check that the symbol has not been already defined */
      if (ResolveSymbol_is_defined(rs)) {
        ErrorMgr_error_redefining_operational_symbol(errmgr, def_name);
      }

      /* creating the body of DEFINE: _process_selector = Pi */
      def_body = find_node(nodemgr, EQUAL, proc_selector_vname, module_name);

      /* The flatten hash has to be filled with the flattened
         body of the newly defined symbol.
      */
      insert_flatten_def_hash(env, def_name,
                              Compile_FlattenSexp(symb_table, def_body, Nil));

      /* declare the define: */
      SymbLayer_declare_define(layer, def_name, Nil /*context*/, def_body);
    } /* for */
  }
}


/*!
  \brief Tries to resolve recursively to a number

  This is a private service of function
   CompileFlatten_resolve_number
*/
static node_ptr compile_flatten_eval_number(const MasterCompileFlattener_ptr flattener,
                                            SymbTable_ptr st,
                                            node_ptr n, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));

  if ((node_ptr) NULL == n) return (node_ptr) NULL;

  switch (node_get_type(n)) {

  case CONTEXT:
    nusmv_assert((node_ptr) NULL == context);
    return compile_flatten_eval_number(flattener, st, cdr(n), car(n));

    /* leaves */
  case TRUEEXP:
  case FALSEEXP:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case FAILURE:
    {
      const NodeMgr_ptr nodemgr =
        NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

      return find_atom(nodemgr, n);
    }

  case ARRAY: {
    ResolveSymbol_ptr rs;

    rs = SymbTable_resolve_symbol(st, n, context);
    /* it might be a symbol that evaluates to a number */

    if (ResolveSymbol_is_undefined(rs)) {
      /* this is array expression. Still it potentially may be
         resolved to constant, e.g. if define v := [1,2,3], define d:=v, then
         d[1] is array expression but can be resolved to 2.
      */

      node_ptr tmp = MasterCompileFlattener_flatten(flattener, st, n, context);

      /* it is impossible that flattening returned undefined identifier. */
      nusmv_assert(tmp != n);
      return compile_flatten_eval_number(flattener, st, tmp, Nil);

    }
    else {
      /* array is actually identifier-with-brackets => process it with
         ATOM, DOT and BIT below */
    }
  }
  /* !! NO BREAK HERE !! */


  case ATOM:
  case DOT:
  case BIT: {
    ResolveSymbol_ptr rs;
    /* it might be a symbol that evaluates to a number */
    node_ptr name;

    rs = SymbTable_resolve_symbol(st, n, context);

    name = ResolveSymbol_get_resolved_name(rs);

    if (name != Nil && ResolveSymbol_is_define(rs)) {
      /* retrieves the define value, and checkes if it is a numeric constant */
      node_ptr body = SymbTable_get_define_flatten_body(st, name);
      return compile_flatten_eval_number(flattener, st, body, (node_ptr) NULL);
    }
    if (ResolveSymbol_is_parameter(rs)) {
      /* is it a formal parameter? tries with the corresponding actual
         parameter */
      node_ptr actual = SymbTable_get_flatten_actual_parameter(st, name);

      return compile_flatten_eval_number(flattener, st, actual, Nil);
    }
    return name;
  }

    /* These nodes need special treatment when used with
       Expr_resolve, since recursively enter into their cdr may
       break the formula. (Ranges with min = max are resolved as
       number by Expr_resolve). See issue 2194. */
  case EBF:
  case ABF:
  case EBG:
  case ABG:
  case EBU:
  case ABU:
    {
      const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

      nusmv_assert(Nil == cdr(n) || TWODOTS == node_get_type(cdr(n)));

      return ExprMgr_resolve(exprs, st, node_get_type(n),
                             compile_flatten_eval_number(flattener, st, car(n), context),
                             cdr(n));
      break;
    }

  default:
    {
      const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

      return ExprMgr_resolve(exprs, st, node_get_type(n),
                             compile_flatten_eval_number(flattener, st, car(n), context),
                             compile_flatten_eval_number(flattener, st, cdr(n), context)

);
    }
  }
}

/*!
  \brief Traverses the list of values, and declare all
   constants (leaves) it finds

  Constants will occur within the given layer
*/
static void flatten_declare_constants_within_list(SymbTable_ptr symb_table,
                                                  SymbLayer_ptr layer,
                                                  node_ptr value_list)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  while (value_list != Nil) {
    node_ptr name = car(value_list);

    /* HERE we cannot use the ResolveSymbol routines also for simple
       ATOM constants: this happens because being the symbol still NOT
       declared in the symbol table, it will not be recognized as a
       simple ATOM constant, and the get_resolved_name method will
       return a dotted notation node, which leads to problems in
       constant resolution later.. */
    if (node_get_type(name) == DOT) {
      ResolveSymbol_ptr rs;
      rs = SymbTable_resolve_symbol(symb_table, name, Nil);
      name = ResolveSymbol_get_resolved_name(rs);
    }
    else name = find_atom(nodemgr, name);

    if (SymbLayer_can_declare_constant(layer, name) &&
        (!SymbTable_is_symbol_declared(symb_table, name))) {

      SymbLayer_declare_constant(layer, name);
    }
    else {
      if (!SymbTable_is_symbol_constant(symb_table, name)) {
        ErrorMgr_error_redefining(errmgr, name);
      }
    }
    value_list = cdr(value_list);
  }
}

/*!
  \brief Instantiates the elements of an array define

  For every cell and every dimension create a correct
   binding in the symbol layer

  \se Elements are added to the layer an the symbol table
*/
static void
instantiate_array_define(SymbTable_ptr st,
                         SymbLayer_ptr layer,
                         node_ptr name,
                         node_ptr mod_name,
                         node_ptr definition)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (!SymbLayer_can_declare_define(layer, name)) {
    ErrorMgr_error_redefining(errmgr, name);
    error_unreachable_code();
  }

  switch (node_get_type(definition)) {
  case ARRAY_DEF:
    {
      node_ptr iter;
      int idx;

      nusmv_assert((cdr(definition) == Nil) &&
                   "Wrong node arity found: ARRAY_DEF must be unary!");

      /* Declare this symbol */
      SymbLayer_declare_array_define(layer, name, mod_name, definition);

      /* Instantiate every element of the array individually
         with first index = 0 */
      for (idx = 0, iter = car(definition);
           iter != Nil;
           idx += 1, iter = cdr(iter)) {
        /* definition has to be a list of values */
        node_ptr index;
        nusmv_assert(CONS == node_get_type(iter));

        /* Instantiate name[idx] element */
        index = find_node(nodemgr, NUMBER, NODE_FROM_INT(idx), Nil);
        instantiate_array_define(st, layer,
                                  find_node(nodemgr, ARRAY, name, index),
                                  mod_name, car(iter));
      }
      break;
    }

  default:
    {
      /* Declare this element */
      SymbLayer_declare_define(layer, name, mod_name, definition);
    }
  }
}

void compile_add_assign_hrc(NodeMgr_ptr nodemgr,
                            HrcNode_ptr hrc_result,
                            node_ptr assign_list)
{
  Slist_ptr stack;

  /* use a stack to preserve the order of assign declarations */
  stack = Slist_create();
  while (Nil != assign_list) {
    Slist_push(stack, cdr(assign_list));

    assign_list = car(assign_list);
  } /* end while on assign_list */

  while (! Slist_is_empty(stack)) {
    node_ptr assign_elem = NODE_PTR(Slist_pop(stack));
    node_ptr left_expr = car(assign_elem);
    node_ptr right_expr = cdr(assign_elem);

    /* determine init/next/invar part of an assign */
    switch (node_get_type(left_expr)) {
    case SMALLINIT:
      {
        /* init assign */
        node_ptr assign_node = new_node(nodemgr, ASSIGN,
                                        car(left_expr), right_expr);
        HrcNode_add_init_assign_expr(hrc_result, assign_node);
      }

      break;

    case NEXT:
      {
        /* next assign */
        node_ptr assign_node = new_node(nodemgr, ASSIGN,
                                        car(left_expr), right_expr);
        HrcNode_add_next_assign_expr(hrc_result, assign_node);
      }
      break;

    default:
      {
        /* Invar assign */
        node_ptr assign_node = new_node(nodemgr, ASSIGN,
                                        left_expr, right_expr);
        HrcNode_add_invar_assign_expr(hrc_result, assign_node);
      }
    }
  } /* end while on assign_list */

  Slist_destroy(stack);
}

/*!
  \brief Get the HRC root node from a child

  Get the HRC root node from a child.
   [AMa] This function can be removed when HRC will support processes and ISA
*/

static HrcNode_ptr get_hrc_root_node (HrcNode_ptr node)
{
  HrcNode_ptr res = node;
  while (!HrcNode_is_root(res)) {
    res = HrcNode_get_parent(res);
  }
  return res;
}

/*!
  \brief Given a numeric constant in node_ptr representation
   the function returns its value as int

  It is an error if overflow/underflow happens
*/
static int compile_flatten_get_int(node_ptr value)
{
  int res;
  WordNumberValue tmp;
  /* get the constants */
  switch (node_get_type(value)) {
  case NUMBER: res = node_get_int(value); break;
  case NUMBER_UNSIGNED_WORD:
    tmp = WordNumber_get_unsigned_value(WORD_NUMBER(car(value)));
    res = tmp;
    nusmv_assert(res == tmp); /* overflow detection */
    break;
  case NUMBER_SIGNED_WORD:
    tmp = WordNumber_get_unsigned_value(WORD_NUMBER(car(value)));
    res = tmp;
    nusmv_assert(res == tmp); /* overflow detection */
    break;
  default: /* error: value is not a constant */
    error_unreachable_code(); /* only numeric constants can be here */
  }
  return res;
}

/*!
  \brief Aux function for the CompileFlatten_normalise_value_list

  The normalisation includes: all TRUE and FALSE
   constants are substituted by 1 and 0 numbers
*/
static node_ptr compile_flatten_normalise_value_list(NodeMgr_ptr nodemgr,
                                                     node_ptr old_value_list)
{
  node_ptr new_tail;
  node_ptr new_head;

  /* the list is empty */
  if (old_value_list == Nil) return Nil;

  /* normalise the tail */
  new_tail = compile_flatten_normalise_value_list(nodemgr, cdr(old_value_list));

  /* normalise the head */
  new_head = car(old_value_list);

  /* create a new list with the line info kept */
  return new_lined_node(nodemgr, CONS, new_head, new_tail, node_get_lineno(old_value_list));
}

/*!
  \brief Aux function for the
                       compile_flatten_rewrite_word_toint_cast, which
                       is used for toint cast operator rewriting

  Creates the following expression:
                       wexpr[bit:bit] (is_neg ? "!=" : "=") 0ud1_1 ? (2^bit) : 0

                       For example, for wexpr = "word_var", bit = "2",
                       is_neg = "false" we have:

                       word_var[2:2] = 0ud1_1 ? 4 : 0
*/
static node_ptr
compile_flatten_build_word_toint_ith_bit_case(const NuSMVEnv_ptr env,
                                              node_ptr wexpr,
                                              int bit,
                                              boolean is_neg)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

  node_ptr cond, bit_node, mul, zero, w1, res;
  int operator, mul_int, i;

  nusmv_assert(bit <= 32);

  mul_int = 1;
  for (i = 0; i < bit; ++i) { mul_int = mul_int << 1; }

  operator = (is_neg ? NOTEQUAL : EQUAL);

  bit_node = find_node(nodemgr, NUMBER, NODE_FROM_INT(bit), Nil);
  mul = find_node(nodemgr, NUMBER, NODE_FROM_INT(mul_int), Nil);
  zero = find_node(nodemgr, NUMBER, NODE_FROM_INT(0), Nil);
  w1 = find_node(nodemgr, NUMBER_UNSIGNED_WORD,
                 NODE_PTR(WordNumberMgr_integer_to_word_number(words, (WordNumberValue)1, 1)),
                 Nil);

  /* w[bit:bit] (= !=) 0ud1_1 */
  cond = new_node(nodemgr, operator,
                  new_node(nodemgr, BIT_SELECTION, wexpr,
                           new_node(nodemgr, COLON, bit_node, bit_node)),
                  w1);

  /* cond ? 2^bit : 0 */
  res = new_node(nodemgr, IFTHENELSE, new_node(nodemgr, COLON, cond, mul), zero);

  return res;
}


/*!
  \brief Check for the type of functions

  Check for the type of functioons
*/
static void _check_supported_function_types(const NuSMVEnv_ptr env,
                                            SymbType_ptr symbolicType,
                                            node_ptr name)
{
  NFunction_ptr type = SymbType_get_nfunction_type(symbolicType);
  SymbType_ptr ret_type = NFunction_get_return_type(type);
  NodeList_ptr arg_types = NFunction_get_args(type);
  ListIter_ptr iter;

  switch (SymbType_get_tag(ret_type)) {
  case SYMB_TYPE_BOOLEAN:
  case SYMB_TYPE_INTEGER:
  case SYMB_TYPE_REAL:
  case SYMB_TYPE_UNSIGNED_WORD:
  case SYMB_TYPE_SIGNED_WORD:
    break;
  default:
    {
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
      const StreamMgr_ptr streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
      const MasterPrinter_ptr wffprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

      ErrorMgr_start_parsing_err(errmgr);
      StreamMgr_print_error(streams,  "Declaration of function '");
      StreamMgr_nprint_error(streams, wffprint, "%N", name);
      StreamMgr_print_error(streams,  "':\n");
      StreamMgr_print_error(streams,
                            "Error: Currently allowed return types are boolean, "\
                            "integer, real and (un)signed words\n");
      ErrorMgr_finish_parsing_err(errmgr);
    }
    break;
  }

  NODE_LIST_FOREACH(arg_types, iter) {
    SymbType_ptr t = SYMB_TYPE(NodeList_get_elem_at(arg_types, iter));

    switch (SymbType_get_tag(t)) {
    case SYMB_TYPE_BOOLEAN:
    case SYMB_TYPE_INTEGER:
    case SYMB_TYPE_REAL:
    case SYMB_TYPE_UNSIGNED_WORD:
    case SYMB_TYPE_SIGNED_WORD:
      break;
    default:
      {
        const ErrorMgr_ptr errmgr =
          ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
        const StreamMgr_ptr streams =
          STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
        const MasterPrinter_ptr wffprint =
          MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

        ErrorMgr_start_parsing_err(errmgr);
        StreamMgr_print_error(streams,  "Declaration of function '");
        StreamMgr_nprint_error(streams, wffprint, "%N", name);
        StreamMgr_print_error(streams,  "':\n");
        StreamMgr_print_error(streams,
                              "Error: Currently allowed arguments types are boolean, " \
                              "integer, real and (un)signed words\n");
        ErrorMgr_finish_parsing_err(errmgr);
        break;
      }
    }
  }
}
