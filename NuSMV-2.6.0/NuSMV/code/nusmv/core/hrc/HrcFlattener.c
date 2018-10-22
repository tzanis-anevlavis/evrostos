/* ---------------------------------------------------------------------------


  This file is part of the ``hrc'' package of NuSMV version 2.
  Copyright (C) 2009 by FBK-irst.

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
  \author Alessandro Mariotti
  \brief Implementation of class 'HrcFlattener'

  \todo: Missing description

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/hrc/HrcFlattener.h"
#include "nusmv/core/hrc/hrcPrefixUtils.h"
#include "nusmv/core/hrc/hrcSymbTableUtils.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/utils/Slist.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"
#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/utils/error.h"


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct HrcFlattener_TAG
{
  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  NuSMVEnv_ptr environment;

  FlatHierarchy_ptr hierarchy;
  SymbTable_ptr symb_table;
  SymbLayer_ptr layer;
  HrcNode_ptr root_node;

  boolean build_hierarchy;

} HrcFlattener;



/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/
#define _PRINT(txt)                             \
  StreamMgr_print_error(streams,  txt)

#define _PRINTLN(txt)                           \
  StreamMgr_print_error(streams,  "%s\n", txt)

#define _NPRINT(node)                           \
  StreamMgr_nprint_error(streams, wffprint, "%N", node)

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define EXPR_AND(l,r)                           \
  find_node(nodemgr, AND, l, r)

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void
hrc_flattener_init(HrcFlattener_ptr self,
                   const NuSMVEnv_ptr env,
                   HrcNode_ptr node,
                   SymbTable_ptr symb_table,
                   SymbLayer_ptr layer);

static void
hrc_flattener_deinit(HrcFlattener_ptr self);

static void
hrc_flattener_flatten_recur(HrcFlattener_ptr self,
                            HrcNode_ptr node,
                            node_ptr context);

static node_ptr
hrc_flattener_expression_and(NodeMgr_ptr nodemgr,
                             Oiter expr_list);

static void
hrc_flattener_populate_model(HrcFlattener_ptr self,
                             HrcNode_ptr node,
                             node_ptr context);

static void
hrc_flattener_flatten_recur(HrcFlattener_ptr self,
                            HrcNode_ptr node,
                            node_ptr context);

static node_ptr
hrc_flattener_build_properties(HrcFlattener_ptr self,
                               Oiter prop_iter,
                               node_ptr ctx,
                               short int type);

static node_ptr hrc_flattener_proc_assign_list(HrcFlattener_ptr self,
                                               HrcNode_ptr node,
                                               Oiter assign_iter,
                                               node_ptr instance_assign,
                                               const int assign_type);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

FlatHierarchy_ptr HrcToFlatHierarchy(const NuSMVEnv_ptr env,
                                     HrcNode_ptr node,
                                     SymbTable_ptr symb_table,
                                     SymbLayer_ptr layer)
{
  HrcFlattener_ptr hf = HrcFlattener_create(env, node, symb_table, layer);
  FlatHierarchy_ptr hierarchy;

  HrcFlattener_flatten_hierarchy(hf);

  hierarchy = HrcFlattener_get_flat_hierarchy(hf);

  HrcFlattener_destroy(hf);

  return hierarchy;
}

SexpFsm_ptr HrcToSexpFsm(NuSMVEnv_ptr env,
                         HrcNode_ptr node,
                         SymbTable_ptr symb_table,
                         SymbLayer_ptr layer)
{
  FlatHierarchy_ptr hierarchy;
  SexpFsm_ptr sexp;

  hierarchy = HrcToFlatHierarchy(env, node, symb_table, layer);
  sexp = SexpFsm_create(hierarchy,
                        FlatHierarchy_get_vars(hierarchy));

  /* SexpFsm creation duplicates the hierarchy */
  FlatHierarchy_destroy(hierarchy);

  return sexp;
}

HrcFlattener_ptr HrcFlattener_create(const NuSMVEnv_ptr env,
                                     HrcNode_ptr node,
                                     SymbTable_ptr symb_table,
                                     SymbLayer_ptr layer)
{
  HrcFlattener_ptr self = ALLOC(HrcFlattener, 1);
  HRC_FLATTENER_CHECK_INSTANCE(self);

  hrc_flattener_init(self, env, node, symb_table, layer);
  return self;
}

FlatHierarchy_ptr HrcFlattener_get_flat_hierarchy(HrcFlattener_ptr self)
{
  HRC_FLATTENER_CHECK_INSTANCE(self);
  return self->hierarchy;
}

SymbTable_ptr HrcFlattener_get_symbol_table(HrcFlattener_ptr self)
{
  HRC_FLATTENER_CHECK_INSTANCE(self);
  return self->symb_table;
}

SymbLayer_ptr HrcFlattener_get_symbol_layer(HrcFlattener_ptr self)
{
  HRC_FLATTENER_CHECK_INSTANCE(self);
  return self->layer;
}

void HrcFlattener_flatten_hierarchy(HrcFlattener_ptr self)
{
  HRC_FLATTENER_CHECK_INSTANCE(self);

  self->build_hierarchy = true;

  /* First of all, build the empty hierarchy */
  self->hierarchy = FlatHierarchy_create(self->symb_table);

  {
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(self->environment, ENV_NODE_MGR));
    /* Start from an empty assignment */
    FlatHierarchy_set_assign(self->hierarchy, cons(nodemgr, cons(nodemgr, Nil,Nil),Nil));
  }

  /* Contextualize exressions and properties, fill the ST */
  hrc_flattener_flatten_recur(self, self->root_node, Nil);

  /* Add all variables to the hierarchy */
  {
    SymbLayerIter iter;
    SYMB_LAYER_FOREACH(self->layer, iter, STT_VAR) {
      node_ptr var = SymbLayer_iter_get_symbol(self->layer, &iter);
      FlatHierarchy_add_var(self->hierarchy, var);
    }
  }
  /* Do the actual flattening */
  Compile_ProcessHierarchy(self->environment, self->symb_table,
                           self->layer, self->hierarchy,
                           Nil, true, true);
}

void HrcFlattener_populate_symbol_table(HrcFlattener_ptr self)
{
  self->build_hierarchy = false;
  hrc_flattener_flatten_recur(self, self->root_node, Nil);
}

void HrcFlattener_write_flatten_model(HrcFlattener_ptr self,
                                      FILE* out)
{
  array_t* names;

  HRC_FLATTENER_CHECK_INSTANCE(self);
  nusmv_assert(FLAT_HIERARCHY(NULL) != self->hierarchy);

  names = SymbTable_get_class_layer_names(self->symb_table, (char*)NULL);

  /* We MUST force flattening because of the properties */
  Compile_WriteFlattenModel(self->environment, out, self->symb_table, names,
                            "MODULE main", self->hierarchy, true);
}

void HrcFlattener_destroy(HrcFlattener_ptr self)
{
  HRC_FLATTENER_CHECK_INSTANCE(self);

  hrc_flattener_deinit(self);
  FREE(self);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Adds to the hierarchy model informations of the
                      given hrc node into the given context

  Does the actual flattening and contextualization
                      of expressions and properties.

  \sa hrc_flattener_flatten_recur
*/
static void hrc_flattener_populate_model(HrcFlattener_ptr self,
                                         HrcNode_ptr node,
                                         node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->symb_table));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  /* INIT */
  {
    node_ptr new_init =
      hrc_flattener_expression_and(nodemgr,
                                   HrcNode_get_init_exprs_iter(node));
    new_init = hrc_prefix_utils_contextualize_expr(env, new_init, context);
    FlatHierarchy_set_init(self->hierarchy,
                           EXPR_AND(new_init,
                                    FlatHierarchy_get_init(self->hierarchy)));

  }
  /* INVAR */
  {
    node_ptr new_invar =
      hrc_flattener_expression_and(nodemgr,
                                   HrcNode_get_invar_exprs_iter(node));
    new_invar = hrc_prefix_utils_contextualize_expr(env, new_invar, context);
    FlatHierarchy_set_invar(self->hierarchy,
                           EXPR_AND(new_invar,
                                    FlatHierarchy_get_invar(self->hierarchy)));

  }
  /* TRANS */
  {
    node_ptr new_trans =
      hrc_flattener_expression_and(nodemgr,
                                   HrcNode_get_trans_exprs_iter(node));
    new_trans = hrc_prefix_utils_contextualize_expr(env, new_trans, context);
    FlatHierarchy_set_trans(self->hierarchy,
                           EXPR_AND(new_trans,
                                    FlatHierarchy_get_trans(self->hierarchy)));

  }

  /* COMPASSION */
  {
    Oiter comp_iter = HrcNode_get_compassion_exprs_iter(node);
    node_ptr new_compassion = FlatHierarchy_get_compassion(self->hierarchy);

    for(; ! Oiter_is_end(comp_iter); comp_iter = Oiter_next(comp_iter)) {
      node_ptr comp = NODE_PTR(Oiter_element(comp_iter));

      comp = hrc_prefix_utils_contextualize_expr(env, comp, context);
      new_compassion = cons(nodemgr, comp, new_compassion);
    }
    FlatHierarchy_set_compassion(self->hierarchy, new_compassion);
  }

  /* JUSTICE */
  {
    Oiter justices = HrcNode_get_justice_exprs_iter(node);
    node_ptr new_justice = FlatHierarchy_get_justice(self->hierarchy);
    for(; ! Oiter_is_end(justices); justices = Oiter_next(justices)) {
      node_ptr just = NODE_PTR(Oiter_element(justices));

      just = hrc_prefix_utils_contextualize_expr(env, just, context);
      new_justice = cons(nodemgr, just, new_justice);
    }
    FlatHierarchy_set_justice(self->hierarchy, new_justice);
  }

  /* ASSIGN */
  {
    node_ptr assign_list = FlatHierarchy_get_assign(self->hierarchy);
    node_ptr instance_assign = Nil;

    /* -- Step 1 -> Create an unique big and of all assignments -- */
    instance_assign =
      hrc_flattener_proc_assign_list(self, node,
                                     HrcNode_get_init_assign_exprs_iter(node),
                                     instance_assign,
                                     SMALLINIT);
    instance_assign =
      hrc_flattener_proc_assign_list(self, node,
                                     HrcNode_get_next_assign_exprs_iter(node),
                                     instance_assign,
                                     NEXT);
    instance_assign =
      hrc_flattener_proc_assign_list(self, node,
                                     HrcNode_get_invar_assign_exprs_iter(node),
                                     instance_assign,
                                     INVAR);

    /* -- Step 2 -> Contextualize all assignments and add it to the
       hierarchy assignment field -- */
    if (Nil != instance_assign) {
      instance_assign = hrc_prefix_utils_contextualize_expr(env, instance_assign,
                                                            context);

      instance_assign = EXPR_AND(instance_assign, cdr(car(assign_list)));


      setcdr(car(assign_list), instance_assign);
    }
  }

  /* proces properties */
  {
    unsigned int idx;

    /* the struct defines all the functions used to get/set
       properties */
    struct {
      Oiter (*hrc_getter) (HrcNode_ptr);
      node_ptr (*fh_getter) (FlatHierarchy_ptr);
      void (*fh_setter) (FlatHierarchy_ptr, node_ptr);
      int prop_type;
    } to_process[] = {
      /* LTL */
      {HrcNode_get_ltl_properties_iter, FlatHierarchy_get_ltlspec,
       FlatHierarchy_set_ltlspec, LTLSPEC},
      /* CTL */
      {HrcNode_get_ctl_properties_iter, FlatHierarchy_get_spec,
       FlatHierarchy_set_spec, SPEC},
      /* PSL */
      {HrcNode_get_psl_properties_iter, FlatHierarchy_get_pslspec,
       FlatHierarchy_set_pslspec, PSLSPEC},
      /* COMPUTE */
      {HrcNode_get_compute_properties_iter, FlatHierarchy_get_compute,
       FlatHierarchy_set_compute, COMPUTE},
    };

    /* loop on all the elements of to_process:
         - build a list of flattened properties of node
         - append the list of flattened properties to the one of the
         hierarchy
         - set the new properties of the hierarchy
     */
    for (idx=0; idx < sizeof(to_process)/sizeof(to_process[0]); ++idx) {
      Oiter prop_iter = to_process[idx].hrc_getter(node);

      /* build a list of flattened properties */
      node_ptr prop_ctx =
        hrc_flattener_build_properties(self, prop_iter,
                                       context,
                                       to_process[idx].prop_type);

      /* set the properties in the hierarchy appending the list of
         flattend properties */
      node_ptr old_prop = to_process[idx].fh_getter(self->hierarchy);
      to_process[idx].fh_setter(self->hierarchy, append_ns(nodemgr,
                                                           prop_ctx,
                                                           old_prop));
    }
  } /* end of properties management */
}

/*!
  \brief Does the actual flattening, recursively

  Does the actual flattening and contextualization
                      of expressions and properties. Recursively
                      descends on module instances. All symbols are
                      flattened and declared into the symbol layer

  \sa HrcFlattener_flatten_hierarchy
*/
static void hrc_flattener_flatten_recur(HrcFlattener_ptr self,
                                        HrcNode_ptr node,
                                        node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->symb_table));

  hrc_symb_table_utils_populate_symb_table(self->symb_table, self->layer,
                                           node, context);

  if (self->build_hierarchy) {
    hrc_flattener_populate_model(self, node, context);
  }

  /* Instances: do recursion */
  {
    Siter iter;
    SLIST_FOREACH(HrcNode_get_child_hrc_nodes(node), iter) {
      HrcNode_ptr child = Siter_element(iter);
      node_ptr name = HrcNode_get_instance_name(child);
      node_ptr new_context = hrc_prefix_utils_concat_context(env, context, name);
      hrc_flattener_flatten_recur(self, child, new_context);
    }
  }
}

/*!
  \brief Returns a list of flathierarchy-structure
                      compliant properties

  Returns a list of properties decorated with
                      CONTEXT and property-type top-level node
                      (LTLSPEC, SPEC, COMPUTE, INVARSPEC, PSLSPEC)
*/
static node_ptr hrc_flattener_build_properties(HrcFlattener_ptr self,
                                               Oiter prop_iter,
                                               node_ptr ctx,
                                               short int type)
{
  const NuSMVEnv_ptr env =
    EnvObject_get_environment(ENV_OBJECT(self->symb_table));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  node_ptr ctx_prop = Nil;
  for (; ! Oiter_is_end(prop_iter); prop_iter = Oiter_next(prop_iter)) {
    node_ptr spec = NODE_PTR(Oiter_element(prop_iter));
    node_ptr prop_name = cdr(spec);

    if (Nil != prop_name) {
      ResolveSymbol_ptr rs;

      rs = SymbTable_resolve_symbol(self->symb_table, prop_name, ctx);
      prop_name = ResolveSymbol_get_resolved_name(rs);

      if (!FlatHierarchy_add_property_name(self->hierarchy, prop_name)) {
        ErrorMgr_internal_error(errmgr, "Property named %s already declared",
                       sprint_node(wffprint, prop_name));
      }
    }

    spec = hrc_prefix_utils_contextualize_expr(env, car(spec), ctx);
    spec = find_node(nodemgr, type, spec, prop_name);
    ctx_prop = cons(nodemgr, spec, ctx_prop);
  }
  return ctx_prop;
}

/*!
  \brief Returns a big-and of the given expression list

  Returns a big-and of the given expression list
*/
static node_ptr hrc_flattener_expression_and(NodeMgr_ptr nodemgr,
                                             Oiter expr_iter)
{
  node_ptr new_expr = Nil;

  for (; ! Oiter_is_end(expr_iter); expr_iter = Oiter_next(expr_iter)) {
    node_ptr expr = NODE_PTR(Oiter_element(expr_iter));
    new_expr = EXPR_AND(expr, new_expr);
  }
  return new_expr;
}

/*!
  \brief The HrcFlattener class private initializer

  The HrcFlattener class private initializer

  \sa HrcFlattener_create
*/
static void hrc_flattener_init(HrcFlattener_ptr self, const NuSMVEnv_ptr env,
                               HrcNode_ptr node, SymbTable_ptr symb_table,
                               SymbLayer_ptr layer)
{
  /* members initialization */
  self->environment = env;
  self->root_node = node;

  /* The flattener can only deal with root nodes. If one wants to
     flatten a single instance, should first use the Hrc localizer
     (see hrc_localize_localize_hierarchy in hrcLocalize.c) */
  nusmv_assert(HrcNode_is_root(node));

  self->symb_table = symb_table;

  /* If no layer is given, create a new one */
  if (SYMB_LAYER(NULL) == layer) {
    layer = SymbTable_create_layer(symb_table, MODEL_LAYER_NAME,
                                   SYMB_LAYER_POS_DEFAULT);

    SymbTable_layer_add_to_class(self->symb_table, MODEL_LAYER_NAME,
                                 MODEL_LAYERS_CLASS);

    SymbTable_set_default_layers_class_name(self->symb_table, MODEL_LAYERS_CLASS);
  }

  self->layer = layer;


  self->hierarchy = FLAT_HIERARCHY(NULL);
}

/*!
  \brief The HrcFlattener class private deinitializer

  The HrcFlattener class private deinitializer

  \sa HrcFlattener_destroy
*/
static void hrc_flattener_deinit(HrcFlattener_ptr self)
{
  /* members deinitialization */
}

/*!
  \brief Processes a list of assigns node and constructs
  the result in instance_assign.


*/
static node_ptr hrc_flattener_proc_assign_list(HrcFlattener_ptr self,
                                               HrcNode_ptr node,
                                               Oiter assign_iter,
                                               node_ptr instance_assign,
                                               const int assign_type)
{
  const NuSMVEnv_ptr env =
    EnvObject_get_environment(ENV_OBJECT(self->symb_table));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  for(; ! Oiter_is_end(assign_iter); assign_iter = Oiter_next(assign_iter)) {
    node_ptr assign = NODE_PTR(Oiter_element(assign_iter));

    switch (assign_type) {
    case SMALLINIT:
        assign = find_node(nodemgr, EQDEF,
                           find_node(nodemgr, SMALLINIT, car(assign), Nil),
                           cdr(assign));
        break;
    case NEXT:
      assign = find_node(nodemgr, EQDEF,
                         find_node(nodemgr, NEXT, car(assign), Nil),
                         cdr(assign));
      break;
    case INVAR:
      assign = find_node(nodemgr, EQDEF, car(assign), cdr(assign));
      break;
    default:
      error_unreachable_code();
    }

    instance_assign = EXPR_AND(assign, instance_assign);
  }

  return instance_assign;
}

/**AutomaticEnd***************************************************************/
