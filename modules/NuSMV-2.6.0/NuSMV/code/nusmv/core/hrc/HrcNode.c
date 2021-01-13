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
  \author Marco Roveri
  \brief Implementation of class 'HrcNode'

  \todo: Missing description

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/hrc/HrcNode.h"
#include "nusmv/core/hrc/HrcNode_private.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/hrc/hrcInt.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/EnvObject_private.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/Slist.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/utils.h"
#include <stdio.h>
#include <stdlib.h>

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

/*! Enable to self check for duplication (debug) in all HrcNode_add_*
 *  methods */
#define HRC_NODE_CHECK_DUPLICATION  0  /* this is for debug */
#define HRC_NODE_AVOID_DUPLICATION  1  /* this is for performances */


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void hrc_node_finalize(Object_ptr object, void* dummy);

static Olist_ptr hrc_node_copy_list(NodeMgr_ptr nodemgr,
                                    Olist_ptr src_list);

static void hrc_node_free_elements_in_list_and_list(NodeMgr_ptr nodemgr,
                                                    Olist_ptr list);

static void hrc_node_free_list_and_clear_assign_map(HrcNode_ptr self,
                                                    int assign_type);

static assoc_retval hrc_node_free_cons_map_fun(char *key,
                                               char *data,
                                               char * arg);
static void hrc_node_insert_assign_hash_list(HrcNode_ptr self,
                                             Olist_ptr assign_list,
                                             const int assign_type);
static void hrc_node_insert_assign_hash(HrcNode_ptr self,
                                        node_ptr assign,
                                        const int assign_type);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

HrcNode_ptr HrcNode_create(const NuSMVEnv_ptr env)
{
  HrcNode_ptr self = ALLOC(HrcNode, 1);
  HRC_NODE_CHECK_INSTANCE(self);

  hrc_node_init(self, env);
  return self;
}

void HrcNode_destroy(HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

void HrcNode_destroy_recur(HrcNode_ptr self)
{
  Slist_ptr child;
  Siter iter;

  HRC_NODE_CHECK_INSTANCE(self);

  child = HrcNode_get_child_hrc_nodes(self);

  for (iter = Slist_first(child);
       false == Siter_is_end(iter);
       iter = Siter_next(iter)) {
    HrcNode_destroy_recur((HrcNode_ptr)Siter_element(iter));
  }
  HrcNode_destroy(self);
}

void HrcNode_cleanup(HrcNode_ptr self)
{
  NuSMVEnv_ptr env;
  NodeMgr_ptr nodemgr;

  HRC_NODE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  /* members initialization */
  self->st = SYMB_TABLE(NULL);
  self->lineno = 0;
  self->name = Nil;
  self->instance_name = Nil;
  self->parent = HRC_NODE(NULL);
  FREE_LIST_AND_SET_TO_NIL(self, self->formal_parameters);
  self->formal_parameters = Olist_create();
  FREE_LIST_AND_SET_TO_NIL(self, self->actual_parameters);
  self->actual_parameters = Olist_create();
  FREE_LIST_AND_SET_TO_NIL(self, self->state_variables);
  self->state_variables = Olist_create();
  FREE_LIST_AND_SET_TO_NIL(self, self->input_variables);
  self->input_variables = Olist_create();
  FREE_LIST_AND_SET_TO_NIL(self, self->frozen_variables);
  self->frozen_variables = Olist_create();
  FREE_LIST_AND_SET_TO_NIL(self, self->frozen_functions);
  self->frozen_functions = Olist_create();
  FREE_LIST_AND_SET_TO_NIL(self, self->state_functions);
  self->state_functions = Olist_create();
  FREE_LIST_AND_SET_TO_NIL(self, self->defines);
  self->defines = Olist_create();
  FREE_LIST_AND_SET_TO_NIL(self, self->array_defines);
  self->array_defines = Olist_create();
  FREELIST_AND_SET_TO_NIL(self->init_expr);
  self->init_expr = Olist_create();
  FREELIST_AND_SET_TO_NIL(self->invar_expr);
  self->invar_expr = Olist_create();
  FREELIST_AND_SET_TO_NIL(self->next_expr);
  self->next_expr = Olist_create();
  FREELIST_AND_SET_TO_NIL(self->justice);
  self->justice = Olist_create();
  FREE_LIST_AND_SET_TO_NIL(self, self->compassion);
  self->compassion = Olist_create();
  FREELIST_AND_SET_TO_NIL(self->constants);
  self->constants = Olist_create();
  FREELIST_AND_SET_TO_NIL(self->ctl_props);
  self->ctl_props = Olist_create();
  FREELIST_AND_SET_TO_NIL(self->ltl_props);
  self->ltl_props = Olist_create();
  FREELIST_AND_SET_TO_NIL(self->psl_props);
  self->psl_props = Olist_create();
  FREELIST_AND_SET_TO_NIL(self->invar_props);
  self->invar_props = Olist_create();
  FREELIST_AND_SET_TO_NIL(self->compute_props);
  self->compute_props = Olist_create();

  self->undef = (void*)NULL;

  /* here the hrc_node_free_list_and_clear_assign_map is not used so
     we can avoid to pass 3 times through the hashmap, which is
     instead freed using the hrc_node_free_cons_map_fun */
  FREE_LIST_AND_SET_TO_NIL(self, self->init_assign);
  nodemgr, self->init_assign = Olist_create();
  FREE_LIST_AND_SET_TO_NIL(self, self->next_assign);
  nodemgr, self->next_assign = Olist_create();
  FREE_LIST_AND_SET_TO_NIL(self, self->invar_assign);
  nodemgr, self->invar_assign = Olist_create();

  /* Free all cons nodes in the map values - do not free the
     assigns_table */
  clear_assoc_and_free_entries_arg(self->assigns_table,
                                   hrc_node_free_cons_map_fun,
                                   (char*)nodemgr);

  {
    Slist_ptr child = HrcNode_get_child_hrc_nodes(self);
    Siter iter;

    child = HrcNode_get_child_hrc_nodes(self);

    for(iter = Slist_first(child);
        false == Siter_is_end(iter);
        iter = Siter_next(iter)) {
      HrcNode_ptr c;

      c = (HrcNode_ptr)Siter_element(iter);

      HrcNode_destroy_recur(c);
    }
  }
}

void HrcNode_set_symbol_table(HrcNode_ptr self, SymbTable_ptr st)
{
  HRC_NODE_CHECK_INSTANCE(self);
  self->st = st;
}

SymbTable_ptr HrcNode_get_symbol_table(HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  nusmv_assert(SYMB_TABLE(NULL) != self->st);
  return self->st;
}

void HrcNode_set_lineno(HrcNode_ptr self, int lineno)
{
  HRC_NODE_CHECK_INSTANCE(self);
  self->lineno = lineno;
}

int HrcNode_get_lineno(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return self->lineno;
}

void HrcNode_set_name(HrcNode_ptr self, node_ptr name)
{
  HRC_NODE_CHECK_INSTANCE(self);

  self->name = name;
}

node_ptr HrcNode_get_name(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    return find_atom(nodemgr, self->name);
  }
}

node_ptr HrcNode_get_crude_name(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return self->name;
}

void HrcNode_set_instance_name(HrcNode_ptr self, node_ptr name)
{
  HRC_NODE_CHECK_INSTANCE(self);

  nusmv_assert(Nil == self->instance_name);
  self->instance_name = name;
}

node_ptr HrcNode_get_instance_name(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return self->instance_name;
}

node_ptr HrcNode_get_flattened_instance_name(const HrcNode_ptr self)
{
  NodeMgr_ptr nodemgr;
  node_ptr flattened_name;
  HrcNode_ptr instance_iter;
  Slist_ptr variables_chain;
  NuSMVEnv_ptr env;

  HRC_NODE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  variables_chain = Slist_create();

  /* The hierarchy is visited upward until the root node.
     A stack of variables is created. At the top of the stack there should
     be the main module.
   */
  instance_iter = self;
  while (HRC_NODE(Nil) != instance_iter) {
    node_ptr instance_name;

    instance_name = HrcNode_get_instance_name(instance_iter);
    Slist_push(variables_chain, instance_name);

    instance_iter = HrcNode_get_parent(instance_iter);
  }

  /* From the variables stack creates the DOT tree for the flattened name
     of the instance.

     [SM] More efficient if popping out all variables!
  */
  flattened_name = Nil;

  while (! Slist_is_empty(variables_chain)) {
    node_ptr current_var;

    current_var = Slist_pop(variables_chain);
    if (NODE_PTR(Nil) != current_var) {
      const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
      /*
        The instance is not the main module.

        Here we use CompileFlatten_concat_contexts for two reasons:
          - find_node: is not able to manage instante names with
        intermediate dots.
          - CompileFlatten_resolve_name, ecc...: they do not handle
        instance names but only variables.
      */
      flattened_name = CompileFlatten_concat_contexts(env, flattened_name,
                                                      find_atom(nodemgr, current_var));
    }
  }

  Slist_destroy(variables_chain);

  return flattened_name;
}

void HrcNode_set_parent(HrcNode_ptr self, const HrcNode_ptr father)
{
  HRC_NODE_CHECK_INSTANCE(self);

  nusmv_assert(HRC_NODE(NULL) == self->parent);
  self->parent = father;

  return;
}

HrcNode_ptr HrcNode_get_parent(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return self->parent;
}

HrcNode_ptr HrcNode_get_root(const HrcNode_ptr self)
{
  HrcNode_ptr res = self;

  HRC_NODE_CHECK_INSTANCE(self);
  while (!HrcNode_is_root(res)) {
    res = HrcNode_get_parent(res);
  }
  return res;
}

void HrcNode_replace_formal_parameters(HrcNode_ptr self, Olist_ptr par)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != par);

  FREE_LIST_AND_SET_TO_NIL(self, self->formal_parameters);

  self->formal_parameters = par;
}

Oiter HrcNode_get_formal_parameters_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->formal_parameters);
}

void HrcNode_add_formal_parameter(HrcNode_ptr self, node_ptr par)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(Nil == par || CONS == node_get_type(par));
#if HRC_NODE_CHECK_DUPLICATION
  nusmv_assert(!Olist_contains(self->formal_parameters, par));
#endif

  Olist_append(self->formal_parameters, par);
}

int HrcNode_get_formal_parameters_length(HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);
  return Olist_get_size(self->formal_parameters);
}

void HrcNode_replace_actual_parameters(HrcNode_ptr self, Olist_ptr par)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != par);

  FREE_LIST_AND_SET_TO_NIL(self, self->actual_parameters);

  self->actual_parameters = par;
}

Oiter HrcNode_get_actual_parameters_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->actual_parameters);
}

void HrcNode_add_actual_parameter(HrcNode_ptr self, node_ptr par)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(Nil == par || CONS == node_get_type(par));
#if HRC_NODE_CHECK_DUPLICATION
  nusmv_assert(!Olist_contains(self->actual_parameters, par));
#endif

  Olist_append(self->actual_parameters, par);
}

int HrcNode_get_actual_parameters_length(HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);
  return Olist_get_size(self->actual_parameters);
}

void HrcNode_replace_state_variables(HrcNode_ptr self, Olist_ptr vars)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != vars);

  FREE_LIST_AND_SET_TO_NIL(self, self->state_variables);

  self->state_variables = vars;
}

Oiter HrcNode_get_state_variables_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->state_variables);
}

void HrcNode_add_state_variable(HrcNode_ptr self, node_ptr var)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(Nil == var || CONS == node_get_type(var));
#if HRC_NODE_CHECK_DUPLICATION
  nusmv_assert(!Olist_contains(self->state_variables, var));
#endif

  Olist_append(self->state_variables, var);
}

void HrcNode_replace_frozen_functions(HrcNode_ptr self, Olist_ptr functions)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != functions);

  FREE_LIST_AND_SET_TO_NIL(self, self->frozen_functions);

  self->frozen_functions = functions;
}

void HrcNode_add_frozen_function(HrcNode_ptr self, node_ptr fun)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(Nil == fun || CONS == node_get_type(fun));
#if HRC_NODE_CHECK_DUPLICATION
  nusmv_assert(!Olist_contains(self->frozen_functions, fun));
#endif

  Olist_append(self->frozen_functions, fun);
}

Oiter HrcNode_get_frozen_functions_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->frozen_functions);
}

void HrcNode_replace_input_variables(HrcNode_ptr self, Olist_ptr vars)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != vars);

  FREE_LIST_AND_SET_TO_NIL(self, self->input_variables);

  self->input_variables = vars;
}

Oiter HrcNode_get_input_variables_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->input_variables);
}

void HrcNode_add_input_variable(HrcNode_ptr self, node_ptr var)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(Nil == var || CONS == node_get_type(var));
#if HRC_NODE_CHECK_DUPLICATION
  nusmv_assert(!Olist_contains(self->input_variables, var));
#endif

  Olist_append(self->input_variables, var);
}

void HrcNode_replace_frozen_variables(HrcNode_ptr self, Olist_ptr vars)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != vars);

  FREE_LIST_AND_SET_TO_NIL(self, self->frozen_variables);

  self->frozen_variables = vars;
}

Oiter HrcNode_get_frozen_variables_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->frozen_variables);
}

void HrcNode_add_frozen_variable(HrcNode_ptr self, node_ptr var)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(Nil == var || CONS == node_get_type(var));
#if HRC_NODE_CHECK_DUPLICATION
  nusmv_assert(!Olist_contains(self->frozen_variables, var));
#endif

  Olist_append(self->frozen_variables, var);
}

void HrcNode_replace_defines(HrcNode_ptr self, Olist_ptr defs)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != defs);

  FREE_LIST_AND_SET_TO_NIL(self, self->defines);

  self->defines = defs;
}

Oiter HrcNode_get_defines_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->defines);
}

void HrcNode_add_define(HrcNode_ptr self, node_ptr def)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(CONS == node_get_type(def));
#if HRC_NODE_CHECK_DUPLICATION
  nusmv_assert(!Olist_contains(self->defines, def));
#endif

  Olist_append(self->defines, def);
}

void HrcNode_replace_array_defines(HrcNode_ptr self, Olist_ptr mdefs)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != mdefs);

  FREE_LIST_AND_SET_TO_NIL(self, self->array_defines);

  self->array_defines = mdefs;
}

Oiter HrcNode_get_array_defines_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->array_defines);
}

void HrcNode_add_array_define(HrcNode_ptr self, node_ptr mdef)
{
  HRC_NODE_CHECK_INSTANCE(self);
#if HRC_NODE_CHECK_DUPLICATION
  nusmv_assert(!Olist_contains(self->array_defines, mdef));
#endif

  Olist_append(self->array_defines, mdef);
}

void HrcNode_replace_init_exprs(HrcNode_ptr self, Olist_ptr exprs)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != exprs);

  FREELIST_AND_SET_TO_NIL(self->init_expr);

  self->init_expr = exprs;
}

Oiter HrcNode_get_init_exprs_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->init_expr);
}

void HrcNode_add_init_expr(HrcNode_ptr self, node_ptr expr)
{
  HRC_NODE_CHECK_INSTANCE(self);

#if HRC_NODE_AVOID_DUPLICATION
  if (!Olist_contains(self->init_expr, expr)) {
    Olist_append(self->init_expr, expr);
  }
#else
  Olist_append(self->init_expr, expr);
#endif
}

void HrcNode_replace_init_assign_exprs(HrcNode_ptr self, Olist_ptr assigns)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != assigns);

  hrc_node_free_list_and_clear_assign_map(self, SMALLINIT);
  /* free the list created by the hrc_node_free_list_and_clear_assign_map */
  nusmv_assert(OLIST(NULL) != self->init_assign);
  Olist_destroy(self->init_assign);

  hrc_node_insert_assign_hash_list(self, assigns, SMALLINIT);

  self->init_assign = assigns;
}

Oiter HrcNode_get_init_assign_exprs_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->init_assign);
}

void HrcNode_add_init_assign_expr(HrcNode_ptr self, node_ptr assign)
{
  HRC_NODE_CHECK_INSTANCE(self);

#if HRC_NODE_AVOID_DUPLICATION
  if (!Olist_contains(self->init_assign, assign)) {
    /* checked to be ok for copies as well */
    hrc_node_insert_assign_hash(self, assign, SMALLINIT);
    Olist_append(self->init_assign, assign);
  }
#else
  hrc_node_insert_assign_hash(self, assign, SMALLINIT);
  Olist_append(self->init_assign, assign);
#endif
}

void HrcNode_replace_invar_exprs(HrcNode_ptr self, Olist_ptr exprs)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != exprs);

  FREELIST_AND_SET_TO_NIL(self->invar_expr);

  self->invar_expr = exprs;
}

Oiter HrcNode_get_invar_exprs_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->invar_expr);
}

void HrcNode_add_invar_expr(HrcNode_ptr self, node_ptr expr)
{
  HRC_NODE_CHECK_INSTANCE(self);
#if HRC_NODE_AVOID_DUPLICATION
  if (!Olist_contains(self->invar_expr, expr)) {
    Olist_append(self->invar_expr, expr);
  }
#else
  Olist_append(self->invar_expr, expr);
#endif
}

void HrcNode_replace_invar_assign_exprs(HrcNode_ptr self, Olist_ptr assigns)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != assigns);

  hrc_node_free_list_and_clear_assign_map(self, INVAR);
  /* free the list created by the hrc_node_free_list_and_clear_assign_map */
  nusmv_assert(OLIST(NULL) != self->invar_assign);
  Olist_destroy(self->invar_assign);

  hrc_node_insert_assign_hash_list(self, assigns, INVAR);
  self->invar_assign = assigns;
}

Oiter HrcNode_get_invar_assign_exprs_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->invar_assign);
}

void HrcNode_add_invar_assign_expr(HrcNode_ptr self, node_ptr assign)
{
  HRC_NODE_CHECK_INSTANCE(self);
#if HRC_NODE_AVOID_DUPLICATION
  if (!Olist_contains(self->invar_assign, assign)) {
    /* checked to be ok for copies as well */
    hrc_node_insert_assign_hash(self, assign, INVAR);
    Olist_append(self->invar_assign, assign);
  }
#else
  hrc_node_insert_assign_hash(self, assign, INVAR);
  Olist_append(self->invar_assign, assign);
#endif
}

boolean HrcNode_can_declare_assign(HrcNode_ptr self, node_ptr symbol,
                                   int assign_type)
{
  node_ptr ass = find_assoc(self->assigns_table, symbol);

  if (Nil != ass) {
    /* Both INIT and NEXT have been already declared */
    if (Nil != cdr(ass)) return false;

    /* An assignment has been already declared, cannon declare an
       INVAR assignment in any case, or the already declared
       assignment is an INVAR one */
    if ((INVAR == assign_type) ||
        (NODE_FROM_INT(INVAR) == car(ass))) return false;

    /* Assignment of assign_type already declared. */
    if (car(ass) == NODE_FROM_INT(assign_type)) return false;
  }

  return true;
}

void HrcNode_replace_trans_exprs(HrcNode_ptr self, Olist_ptr exprs)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != exprs);

  FREELIST_AND_SET_TO_NIL(self->next_expr);

  self->next_expr = exprs;
}

Oiter HrcNode_get_trans_exprs_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->next_expr);
}

void HrcNode_add_trans_expr(HrcNode_ptr self, node_ptr expr)
{
  HRC_NODE_CHECK_INSTANCE(self);
#if HRC_NODE_AVOID_DUPLICATION
  if (!Olist_contains(self->next_expr, expr)) {
    Olist_append(self->next_expr, expr);
  }
#else
  Olist_append(self->next_expr, expr);
#endif
}

void HrcNode_replace_next_assign_exprs(HrcNode_ptr self, Olist_ptr assigns)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != assigns);

  hrc_node_free_list_and_clear_assign_map(self, NEXT);
  /* free the list created by the hrc_node_free_list_and_clear_assign_map */
  nusmv_assert(OLIST(NULL) != self->next_assign);
  Olist_destroy(self->next_assign);

  hrc_node_insert_assign_hash_list(self, assigns, NEXT);
  self->next_assign = assigns;
}

Oiter HrcNode_get_next_assign_exprs_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->next_assign);
}

void HrcNode_add_next_assign_expr(HrcNode_ptr self, node_ptr assign)
{
  HRC_NODE_CHECK_INSTANCE(self);
#if HRC_NODE_AVOID_DUPLICATION
  if (!Olist_contains(self->next_assign, assign)) {
    /* checked to be ok for copies as well */
    hrc_node_insert_assign_hash(self, assign, NEXT);
    Olist_append(self->next_assign, assign);
  }
#else
  hrc_node_insert_assign_hash(self, assign, NEXT);
  Olist_append(self->next_assign, assign);
#endif
}

void HrcNode_replace_justice_exprs(HrcNode_ptr self, Olist_ptr justices)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != justices);

  FREELIST_AND_SET_TO_NIL(self->justice);

  self->justice = justices;
}

Oiter HrcNode_get_justice_exprs_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->justice);
}

void HrcNode_add_justice_expr(HrcNode_ptr self, node_ptr justice)
{
  HRC_NODE_CHECK_INSTANCE(self);
#if HRC_NODE_AVOID_DUPLICATION
  if (!Olist_contains(self->justice, justice)) {
    Olist_append(self->justice, justice);
  }
#else
  Olist_append(self->justice, justice);
#endif
}

void HrcNode_replace_compassion_exprs(HrcNode_ptr self, Olist_ptr compassions)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != compassions);

  FREE_LIST_AND_SET_TO_NIL(self, self->compassion);

  self->compassion = compassions;
}

Oiter HrcNode_get_compassion_exprs_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->compassion);
}

void HrcNode_add_compassion_expr(HrcNode_ptr self, node_ptr compassion)
{
  HRC_NODE_CHECK_INSTANCE(self);
#if HRC_NODE_AVOID_DUPLICATION
  if (!Olist_contains(self->compassion, compassion)) {
    Olist_append(self->compassion, compassion);
  }
#else
  Olist_append(self->compassion, compassion);
#endif
}

void HrcNode_replace_constants(HrcNode_ptr self, Olist_ptr constants)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != constants);

  FREELIST_AND_SET_TO_NIL(self->constants);
  self->constants = constants;
}

Oiter HrcNode_get_constants_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->constants);
}

void HrcNode_add_constants(HrcNode_ptr self, node_ptr constant)
{
  HRC_NODE_CHECK_INSTANCE(self);

  while (Nil != constant) {
    nusmv_assert(Nil == constant || CONS == node_get_type(constant));
#if HRC_NODE_AVOID_DUPLICATION
    if (!Olist_contains(self->constants, car(constant))) {
      Olist_append(self->constants, car(constant));
    }
#else
    Olist_append(self->constants, car(constant));
#endif

    constant = cdr(constant);
  }
}

void HrcNode_replace_ctl_properties(HrcNode_ptr self, Olist_ptr ctls)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != ctls);

  FREELIST_AND_SET_TO_NIL(self->ctl_props);

  self->ctl_props = ctls;
}

Oiter HrcNode_get_ctl_properties_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->ctl_props);
}

void HrcNode_add_ctl_property_expr(HrcNode_ptr self, node_ptr ctl)
{
  HRC_NODE_CHECK_INSTANCE(self);
#if HRC_NODE_AVOID_DUPLICATION
  if (!Olist_contains(self->ctl_props, ctl)) {
    Olist_append(self->ctl_props, ctl);
  }
#else
  Olist_append(self->ctl_props, ctl);
#endif
}

void HrcNode_replace_ltl_properties(HrcNode_ptr self, Olist_ptr ltls)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != ltls);

  FREELIST_AND_SET_TO_NIL(self->ltl_props);

  self->ltl_props = ltls;
}

Oiter HrcNode_get_ltl_properties_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->ltl_props);
}

void HrcNode_add_ltl_property_expr(HrcNode_ptr self, node_ptr ltl)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(Nil == ltl || LTLSPEC == node_get_type(ltl));

#if HRC_NODE_AVOID_DUPLICATION
  if (!Olist_contains(self->ltl_props, ltl)) {
    Olist_append(self->ltl_props, ltl);
  }
#else
  Olist_append(self->ltl_props, ltl);
#endif
}

void HrcNode_replace_psl_properties(HrcNode_ptr self, Olist_ptr psls)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != psls);

  FREELIST_AND_SET_TO_NIL(self->psl_props);

  self->psl_props = psls;
}

Oiter HrcNode_get_psl_properties_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->psl_props);
}

void HrcNode_add_psl_property_expr(HrcNode_ptr self, node_ptr psl)
{
  HRC_NODE_CHECK_INSTANCE(self);

#if HRC_NODE_AVOID_DUPLICATION
  if (!Olist_contains(self->psl_props, psl)) {
    Olist_append(self->psl_props, psl);
  }
#else
  Olist_append(self->psl_props, psl);
#endif
}

void HrcNode_replace_invar_properties(HrcNode_ptr self, Olist_ptr invars)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != invars);

  FREELIST_AND_SET_TO_NIL(self->invar_props);

  self->invar_props = invars;
}

Oiter HrcNode_get_invar_properties_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->invar_props);
}

void HrcNode_add_invar_property_expr(HrcNode_ptr self, node_ptr invar)
{
  HRC_NODE_CHECK_INSTANCE(self);
#if HRC_NODE_AVOID_DUPLICATION
  if (!Olist_contains(self->invar_props, invar)) {
    Olist_append(self->invar_props, invar);
  }
#else
  Olist_append(self->invar_props, invar);
#endif
}

void HrcNode_replace_compute_properties(HrcNode_ptr self, Olist_ptr computes)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(OLIST(NULL) != computes);

  FREELIST_AND_SET_TO_NIL(self->compute_props);

  self->compute_props = computes;
}

Oiter HrcNode_get_compute_properties_iter(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Olist_first(self->compute_props);
}

void HrcNode_add_compute_property_expr(HrcNode_ptr self, node_ptr compute)
{
  HRC_NODE_CHECK_INSTANCE(self);

#if HRC_NODE_AVOID_DUPLICATION
  if (!Olist_contains(self->compute_props, compute)) {
    Olist_append(self->compute_props, compute);
  }
#else
  Olist_append(self->compute_props, compute);
#endif
}

void HrcNode_set_child_hrc_nodes(HrcNode_ptr self, Slist_ptr list)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(Slist_is_empty(self->childs));

  Slist_destroy(self->childs);

  {
    Siter iter;
    for(iter = Slist_first(list);
        (false == Siter_is_end(iter));
        iter = Siter_next(iter)) {
      nusmv_assert(HrcNode_get_parent(HRC_NODE(Siter_element(iter))) == self);
    }
  }

  self->childs = list;
}

/*!
  \brief Gets the list of child nodes.

  Gets the list of child nodes.

  \se None

  \sa optional
*/

/* Get the local list of child nodes for current node */
Slist_ptr HrcNode_get_child_hrc_nodes(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return self->childs;
}

void HrcNode_add_child_hrc_node(HrcNode_ptr self, HrcNode_ptr node)
{
  HRC_NODE_CHECK_INSTANCE(self);
  nusmv_assert(HrcNode_get_parent(node) == self);

  Slist_push(self->childs, (void *)node);
}

HrcNode_ptr HrcNode_find_hrc_node_by_mod_type(const HrcNode_ptr self,
                                              const node_ptr mod_type)
{
  HRC_NODE_CHECK_INSTANCE(self);

  if (HrcNode_get_name(self) == mod_type) return self;
  else {
    Siter iter;
    HrcNode_ptr r = HRC_NODE(NULL);
    for (iter = Slist_first(self->childs);
         false == Siter_is_end(iter);
         iter = Siter_next(iter)) {

      r = HrcNode_find_hrc_node_by_mod_type((HrcNode_ptr)Siter_element(iter),
                                            mod_type);
      if (HRC_NODE(NULL) != r) break;
    }
    return r;
  }
}

Olist_ptr HrcNode_find_hrc_nodes_by_mod_type(const HrcNode_ptr self,
                                             const node_ptr mod_type)
{
  Olist_ptr res = Olist_create();

  HRC_NODE_CHECK_INSTANCE(self);

  if (HrcNode_get_name(self) == mod_type) {
    Olist_append(res, self);
    /* nodes cannot contain modules of their same types, so it can exit */
  }
  else {
    /* inductive step */
    Siter iter;
    SLIST_FOREACH(self->childs, iter) {
      Olist_ptr child_list =
        HrcNode_find_hrc_nodes_by_mod_type((HrcNode_ptr) Siter_element(iter),
                                           mod_type);
      Olist_move_all(child_list, res);
      Olist_destroy(child_list);
    }
  }

  return res;
}

HrcNode_ptr HrcNode_find_hrc_node_by_instance_name(const HrcNode_ptr self,
                                                   node_ptr name)
{
  NuSMVEnv_ptr env;
  MasterNormalizer_ptr normalizer;

  HRC_NODE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  normalizer = MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));

  if (MasterNormalizer_normalize_node(normalizer, self->instance_name) ==
      MasterNormalizer_normalize_node(normalizer, name)) {
    return self;
  }
  else {
    Siter iter;
    HrcNode_ptr r = HRC_NODE(NULL);
    for (iter = Slist_first(self->childs);
         false == Siter_is_end(iter);
         iter = Siter_next(iter)) {
      r = HrcNode_find_hrc_node_by_instance_name((HrcNode_ptr)Siter_element(iter),
                                                 name);
      if (HRC_NODE(NULL) != r) break;
    }
    return r;
  }
}

boolean HrcNode_is_root(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return (HRC_NODE(NULL) == self->parent);
}

boolean HrcNode_is_leaf(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return Slist_is_empty(self->childs);
}

HrcNode_ptr HrcNode_copy(const HrcNode_ptr self)
{
  HrcNode_ptr hrc_copy;
  NuSMVEnv_ptr env;
  NodeMgr_ptr nodemgr;

  HRC_NODE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  hrc_copy = HrcNode_create(env);

  hrc_copy->st = self->st;
  hrc_copy->lineno = self->lineno;
  hrc_copy->name = self->name;
  hrc_copy->instance_name = self->instance_name;

  /* parent is set to Nil! The copy is not linked to any node. */
  hrc_copy->parent = HRC_NODE(Nil);

  /* List that can be copied directly */
  hrc_copy->init_expr = Olist_copy(self->init_expr);
  hrc_copy->invar_expr = Olist_copy(self->invar_expr);
  hrc_copy->next_expr = Olist_copy(self->next_expr);
  hrc_copy->justice = Olist_copy(self->justice);
  hrc_copy->constants = Olist_copy(self->constants);
  hrc_copy->ctl_props = Olist_copy(self->ctl_props);
  hrc_copy->ltl_props = Olist_copy(self->ltl_props);
  hrc_copy->psl_props = Olist_copy(self->psl_props);
  hrc_copy->invar_props = Olist_copy(self->invar_props);
  hrc_copy->compute_props = Olist_copy(self->compute_props);

  /* List that must be deep copied, copying also list elements */
  hrc_copy->formal_parameters = hrc_node_copy_list(nodemgr,
                                                   self->formal_parameters);
  hrc_copy->actual_parameters = hrc_node_copy_list(nodemgr,
                                                   self->actual_parameters);
  hrc_copy->state_variables = hrc_node_copy_list(nodemgr, self->state_variables);
  hrc_copy->input_variables = hrc_node_copy_list(nodemgr, self->input_variables);
  hrc_copy->frozen_variables = hrc_node_copy_list(nodemgr,
                                                  self->frozen_variables);
  hrc_copy->frozen_functions = hrc_node_copy_list(nodemgr,
                                                  self->frozen_functions);
  hrc_copy->state_functions = hrc_node_copy_list(nodemgr, self->state_functions);

  hrc_copy->defines = hrc_node_copy_list(nodemgr, self->defines);
  hrc_copy->array_defines = hrc_node_copy_list(nodemgr, self->array_defines);
  hrc_copy->init_assign = hrc_node_copy_list(nodemgr, self->init_assign);
  hrc_copy->invar_assign = hrc_node_copy_list(nodemgr, self->invar_assign);
  hrc_copy->next_assign = hrc_node_copy_list(nodemgr, self->next_assign);
  hrc_copy->compassion = hrc_node_copy_list(nodemgr, self->compassion);

  /* Copy the assign's table */
  {
    assoc_iter aiter;
    node_ptr sym;
    node_ptr val;

    ASSOC_FOREACH(self->assigns_table, aiter, &sym, &val) {
      insert_assoc(hrc_copy->assigns_table,
                   /* Use the same key, this is shared (find_node) */
                   sym,
                   /* Avoid memory sharing re-creating the cons node */
                   cons(nodemgr, car(val), cdr(val)));
    }
  }

  /* We use the same reference for the undef field of node */
  hrc_copy->undef = self->undef;

  return hrc_copy;
}

HrcNode_ptr HrcNode_copy_rename(const HrcNode_ptr self,
                                node_ptr new_module_name)
{
  HrcNode_ptr hrc_copy;

  hrc_copy = HrcNode_copy(self);

  hrc_copy->name = new_module_name;

  return hrc_copy;
}

void* HrcNode_get_undef(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);

  return self->undef;
}

void HrcNode_set_undef(const HrcNode_ptr self, void* undef)
{
  HRC_NODE_CHECK_INSTANCE(self);

  self->undef = undef;
}

HrcNode_ptr HrcNode_recursive_copy(const HrcNode_ptr self)
{
  HrcNode_ptr hrc_copy;
  Slist_ptr children;
  Siter iter;

  HRC_NODE_CHECK_INSTANCE(self);

  /* Copy the contents of the current node */
  hrc_copy = HrcNode_copy(self);

  /* Recursively copy children of node */
  children = Slist_copy_reversed(self->childs);
  SLIST_FOREACH(children, iter) {
    HrcNode_ptr child;
    HrcNode_ptr child_copy;

    child = HRC_NODE(Siter_element(iter));
    child_copy = HrcNode_recursive_copy(child);

    /* We must set the parent of child_copy */
    child_copy->parent = hrc_copy;

    HrcNode_add_child_hrc_node(hrc_copy, child_copy);
  }
  Slist_destroy(children);

  return hrc_copy;
}

node_ptr HrcNode_find_var_all(HrcNode_ptr self, node_ptr var_name)
{
  node_ptr res = Nil;

  int types[] = {VAR, FROZENVAR, VAR};
  int i;
  for (i=0; i<sizeof(types)/sizeof(types[0]); ++i) {
    res = HrcNode_find_var(self, var_name, types[i]);
    if (Nil != res) {
      break;
    }
  }

  return res;
}

node_ptr HrcNode_find_var(HrcNode_ptr self, node_ptr var_name, int type)
{
  NuSMVEnv_ptr env;
  MasterNormalizer_ptr normalizer;
  ErrorMgr_ptr errmgr;

  Oiter iter;

  node_ptr found_var;
  node_ptr normalized_var_name = NULL;

  HRC_NODE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  normalizer = MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));
  errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  found_var = Nil;

  switch (type) {
  case VAR:
    iter = HrcNode_get_state_variables_iter(self);
    break;
  case FROZENVAR:
    iter = HrcNode_get_frozen_variables_iter(self);
    break;
  case IVAR:
    iter = HrcNode_get_input_variables_iter(self);
    break;
  default:
    ErrorMgr_internal_error(errmgr, "HrcNode: %d is not a valid variable type!",
                            type);
  }

  /* Search normalizing node name! */
  normalized_var_name = MasterNormalizer_normalize_node(normalizer, var_name);

  for (; ! Oiter_is_end(iter); iter = Oiter_next(iter)) {
    node_ptr var, var_name;

    var = NODE_PTR(Oiter_element(iter));
    nusmv_assert(Nil != var);
    /* var must have a name */
    nusmv_assert(Nil != car(var));
    var_name = car(var);

    if  (MasterNormalizer_normalize_node(normalizer, var_name) ==
         normalized_var_name) {
      found_var = var;
      break;
    }
  }

  nusmv_assert(Nil == found_var || CONS == node_get_type(found_var));

  return found_var;
}

node_ptr HrcNode_find_formal_parameter(HrcNode_ptr self, node_ptr par_name)
{
  NuSMVEnv_ptr env;
  MasterNormalizer_ptr normalizer;
  ErrorMgr_ptr errmgr;

  Oiter iter;
  node_ptr normalized_name = NULL;
  node_ptr found_par = Nil;

  HRC_NODE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  normalizer = MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));
  errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  /* Search normalizing node name! */
  normalized_name = MasterNormalizer_normalize_node(normalizer, par_name);

  OLIST_FOREACH(self->formal_parameters, iter) {
    node_ptr par = NODE_PTR(Oiter_element(iter));

    /* par must have a name */
    nusmv_assert(Nil != par);
    nusmv_assert(Nil != car(par));

    if  (MasterNormalizer_normalize_node(normalizer, car(par)) ==
         normalized_name) {
      found_par = par;
      break;
    }
  }

  nusmv_assert(Nil == found_par || CONS == node_get_type(found_par));

  return found_par;
}

node_ptr HrcNode_find_define(HrcNode_ptr self, node_ptr def_name)
{
  NuSMVEnv_ptr env;
  MasterNormalizer_ptr normalizer;
  ErrorMgr_ptr errmgr;

  Oiter iter;
  node_ptr normalized_name = NULL;
  node_ptr found_def = Nil;

  HRC_NODE_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  normalizer = MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));
  errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  /* Search normalizing node name! */
  normalized_name = MasterNormalizer_normalize_node(normalizer, def_name);

  OLIST_FOREACH(self->defines, iter) {
    node_ptr def = NODE_PTR(Oiter_element(iter));

    /* def must have a name */
    nusmv_assert(Nil != def);
    nusmv_assert(Nil != car(def));

    if  (MasterNormalizer_normalize_node(normalizer, car(def)) ==
         normalized_name) {
      found_def = def;
      break;
    }
  }

  nusmv_assert(Nil == found_def || CONS == node_get_type(found_def));

  return found_def;
}

void HrcNode_link_nodes(HrcNode_ptr self, HrcNode_ptr child)
{
  HrcNode_set_parent(child, self);
  HrcNode_add_child_hrc_node(self, child);
}

void HrcNode_unlink_nodes(HrcNode_ptr self, HrcNode_ptr child)
{
  boolean status;

  child->parent = NULL;
  child->instance_name = NULL;
  status = Slist_remove(self->childs, child);

  if (0 < Olist_get_size(child->actual_parameters)) {
    FREE_LIST_AND_SET_TO_NIL(self, child->actual_parameters);
    child->actual_parameters = Olist_create();
  }

  nusmv_assert(status);
}

void HrcNode_remove_state_variable(HrcNode_ptr self,
                                   node_ptr var)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  NodeMgr_ptr const nodemgr =
     NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  nusmv_assert(CONS == node_get_type(var));

  Olist_remove(self->state_variables, var);
}

void HrcNode_remove_frozen_variable(HrcNode_ptr self,
                                    node_ptr var)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  NodeMgr_ptr const nodemgr =
     NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  nusmv_assert(CONS == node_get_type(var));

  Olist_remove(self->frozen_variables, var);
}

void HrcNode_remove_input_variable(HrcNode_ptr self,
                                   node_ptr var)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  NodeMgr_ptr const nodemgr =
     NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  nusmv_assert(CONS == node_get_type(var));

  Olist_remove(self->input_variables, var);
}

int HrcNode_get_vars_num(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);
  return
    HrcNode_get_state_vars_num(self) +
    HrcNode_get_input_vars_num(self) +
    HrcNode_get_frozen_vars_num(self);
}

int HrcNode_get_state_vars_num(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);
  return Olist_get_size(self->state_variables);
}

int HrcNode_get_frozen_vars_num(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);
  return Olist_get_size(self->frozen_variables);
}

int HrcNode_get_input_vars_num(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);
  return Olist_get_size(self->input_variables);
}

int HrcNode_get_constants_num(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);
  return Olist_get_size(self->constants);
}

int HrcNode_get_defines_num(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);
  return Olist_get_size(self->defines);
}

int HrcNode_get_array_defines_num(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);
  return Olist_get_size(self->array_defines);
}

int HrcNode_get_parameters_num(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);
  return Olist_get_size(self->formal_parameters);
}

int HrcNode_get_functions_num(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);
  return
    Olist_get_size(self->state_functions) +
    Olist_get_size(self->frozen_functions);
}

int HrcNode_get_symbols_num(const HrcNode_ptr self)
{
  HRC_NODE_CHECK_INSTANCE(self);
  return
    HrcNode_get_vars_num(self) +
    HrcNode_get_constants_num(self) +
    HrcNode_get_defines_num(self) +
    HrcNode_get_array_defines_num(self) +
    HrcNode_get_parameters_num(self) +
    HrcNode_get_functions_num(self);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void hrc_node_init(HrcNode_ptr self, const NuSMVEnv_ptr env)
{
  env_object_init(ENV_OBJECT(self), env);

  /* members initialization */
  self->st = SYMB_TABLE(NULL);
  self->lineno = 0;
  self->name = Nil;
  self->instance_name = Nil;
  self->parent = HRC_NODE(NULL);
  self->formal_parameters = Olist_create();
  self->actual_parameters = Olist_create();
  self->state_variables = Olist_create();
  self->input_variables = Olist_create();
  self->frozen_variables = Olist_create();
  self->frozen_functions = Olist_create();
  self->state_functions = Olist_create();
  self->defines = Olist_create();
  self->array_defines = Olist_create();
  self->init_expr = Olist_create();
  self->init_assign = Olist_create();
  self->invar_expr = Olist_create();
  self->invar_assign = Olist_create();
  self->next_expr = Olist_create();
  self->next_assign = Olist_create();
  self->justice = Olist_create();
  self->compassion = Olist_create();
  self->constants = Olist_create();
  self->invar_props = Olist_create();
  self->ctl_props = Olist_create();
  self->ltl_props = Olist_create();
  self->psl_props = Olist_create();
  self->compute_props = Olist_create();
  self->childs = Slist_create();
  self->undef = (void*)NULL;
  self->assigns_table = new_assoc();

  OVERRIDE(Object, finalize) = hrc_node_finalize;
}

void hrc_node_deinit(HrcNode_ptr self)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  /* members deinitialization */
  self->st = SYMB_TABLE(NULL);
  self->lineno = 0;
  self->name = Nil;
  self->instance_name = Nil;
  self->parent = HRC_NODE(NULL);

  FREE_LIST_AND_SET_TO_NIL(self, self->formal_parameters);
  FREE_LIST_AND_SET_TO_NIL(self, self->actual_parameters);
  FREE_LIST_AND_SET_TO_NIL(self, self->state_variables);
  FREE_LIST_AND_SET_TO_NIL(self, self->input_variables);
  FREE_LIST_AND_SET_TO_NIL(self, self->frozen_variables);
  FREE_LIST_AND_SET_TO_NIL(self, self->frozen_functions);
  FREE_LIST_AND_SET_TO_NIL(self, self->state_functions);
  FREE_LIST_AND_SET_TO_NIL(self, self->defines);
  FREE_LIST_AND_SET_TO_NIL(self, self->array_defines);
  FREELIST_AND_SET_TO_NIL(self->init_expr);
  FREELIST_AND_SET_TO_NIL(self->invar_expr);
  FREELIST_AND_SET_TO_NIL(self->next_expr);
  FREELIST_AND_SET_TO_NIL(self->justice);
  FREE_LIST_AND_SET_TO_NIL(self, self->compassion);
  FREELIST_AND_SET_TO_NIL(self->constants);
  FREELIST_AND_SET_TO_NIL(self->ctl_props);
  FREELIST_AND_SET_TO_NIL(self->ltl_props);
  FREELIST_AND_SET_TO_NIL(self->psl_props);
  FREELIST_AND_SET_TO_NIL(self->invar_props);
  FREELIST_AND_SET_TO_NIL(self->compute_props);

  /* here the hrc_node_free_list_and_clear_assign_map is not used so
     we can avoid to pass 3 times through the hashmap, which is
     instead freed using the hrc_node_free_cons_map_fun */
  FREE_LIST_AND_SET_TO_NIL(self, self->init_assign);
  FREE_LIST_AND_SET_TO_NIL(self, self->next_assign);
  FREE_LIST_AND_SET_TO_NIL(self, self->invar_assign);

  /* Free all cons nodes in the map values */
  clear_assoc_and_free_entries_arg(self->assigns_table,
                                   hrc_node_free_cons_map_fun,
                                   (char*)nodemgr);
  free_assoc(self->assigns_table);

  /* It is the responsibility of the creator to destroy the childrens. */
  Slist_destroy(self->childs);
  self->childs = SLIST(Nil);
  /* It is the responsibility of the creator to free this area before
     calling this function */
  if (self->undef != (void*)NULL) {
    self->undef = (void*)NULL;
  }

  env_object_deinit(ENV_OBJECT(self));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The HrcNode class private deinitializer

  The HrcNode class private deinitializer

  \sa HrcNod_edestroy
*/
static void hrc_node_finalize(Object_ptr object, void* dummy)
{
  HrcNode_ptr self = HRC_NODE(object);

  hrc_node_deinit(self);

  FREE(self);
}

/*!
  \brief Copy a list made of CONS elements.

  Copy a list made of CONS elements.

  Also CONS elements are copied when copying the list. In this way the returned
  list can be used in a copy of the current node.

  New node are used to create cons elements. The copy preserves order of
  elements.
*/
static Olist_ptr hrc_node_copy_list(NodeMgr_ptr nodemgr, Olist_ptr src_list)
{
  Oiter iter;
  Olist_ptr new_list;

  new_list = Olist_create();
  OLIST_FOREACH(src_list, iter) {
    node_ptr element = NODE_PTR(Oiter_element(iter));
    node_ptr new_element;

    /* copy the new element and append to the list */
    new_element = new_node(nodemgr, node_get_type(element), car(element),
                           cdr(element));
    Olist_append(new_list, new_element);
  }

  return new_list;
}

/*!
  \brief Frees all the elements contained in a list.

  Frees all the elements contained in a list.

  \se Elements contained in list are freed.
*/
static void hrc_node_free_elements_in_list_and_list(NodeMgr_ptr nodemgr,
                                                    Olist_ptr list)
{
  Oiter iter;
  OLIST_FOREACH(list, iter) {
    node_ptr element = NODE_PTR(Oiter_element(iter));

    if (NODE_PTR(Nil) != element) {
      free_node(nodemgr, element);
    }
  }
}

/*!
  \brief Frees all the elements contained in a ASSIGN list. and
                      removes entries from the assign map

  Frees all the elements contained in a ASSIGN list. and
                      removes entries from the assign map

  \se Elements contained in list are freed, the list is
                      freed and set to Nil into the HrcNode
*/
static void hrc_node_free_list_and_clear_assign_map(HrcNode_ptr self,
                                                    int assign_type)
{
  Olist_ptr old_list;
  Oiter iter;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  switch (assign_type) {
  case SMALLINIT:
    old_list = self->init_assign;
    self->init_assign = Olist_create();
    break;
  case NEXT:
    old_list = self->next_assign;
    self->next_assign =  Olist_create();
    break;
  case INVAR:
    old_list = self->invar_assign;
    self->invar_assign =  Olist_create();
    break;
  default:
    error_unreachable_code();
  }

  nusmv_assert(OLIST(NULL) != old_list);

  /* Destroys the internal nodes used in the list.
     Removes the entries from the assign hash table.
   */
  OLIST_FOREACH(old_list, iter) {
    node_ptr elem = NODE_PTR(Oiter_element(iter));

    /* we just need the symb name */
    node_ptr tmp = find_assoc(self->assigns_table, car(elem));

    /* The assignment must exist */
    nusmv_assert(Nil != tmp);

    /* This assignment is in the car part of the node */
    if (NODE_FROM_INT(assign_type) == car(tmp)) {
      /* If this is the only assignment for this symbol, we can remove
         the node from the hash and free it */
      if (Nil == cdr(tmp)) {
        remove_assoc(self->assigns_table, car(elem));
        free_node(nodemgr, tmp);
      }
      /* Otherwise shift cdr to car */
      else {
        setcar(tmp, cdr(tmp));
        setcdr(tmp, Nil);
      }
    }
    /* Otherwise it must be in the right part, which will be set to
       Nil */
    else {
      nusmv_assert(Nil != cdr(tmp) &&
                   NODE_FROM_INT(assign_type) == cdr(tmp));
      setcdr(tmp, Nil);
    }

    /* Free the ASSIGN node */
    free_node(nodemgr, elem);
  }

  /* Free the old list */
  Olist_destroy(old_list);
}

/*!
  \brief Function for freeing cons nodes into a map

  Function for freeing cons nodes into a map
*/
static assoc_retval hrc_node_free_cons_map_fun(char *key,
                                               char *data,
                                               char *arg)
{
  const NodeMgr_ptr nodemgr = NODE_MGR(arg);

  if ((char*)NULL != data) {
    free_node(nodemgr, NODE_PTR(data));
  }
  return ASSOC_DELETE;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void hrc_node_insert_assign_hash_list(HrcNode_ptr self,
                                             Olist_ptr assign_list,
                                             const int assign_type)
{
  Oiter iter;

  HRC_NODE_CHECK_INSTANCE(self);

  OLIST_FOREACH(assign_list, iter) {
    node_ptr assign = Oiter_element(iter);
    nusmv_assert(NULL != assign);
    hrc_node_insert_assign_hash(self, assign, assign_type);
  }
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void hrc_node_insert_assign_hash(HrcNode_ptr self,
                                        node_ptr assign,
                                        const int assign_type)
{
  node_ptr ass;

  ass = find_assoc(self->assigns_table, car(assign));

  if (Nil == ass) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    insert_assoc(self->assigns_table, car(assign),
                 cons(nodemgr, NODE_FROM_INT(assign_type), Nil));
  }
  else {
    /* We can have both SMALLINIT and NEXT, but not INVAR and
       INIT/NEXT nor 2 INVAR assigns */
    switch (assign_type) {
    case SMALLINIT:
      nusmv_assert(NODE_FROM_INT(NEXT) == car(ass));
      setcdr(ass, NODE_FROM_INT(SMALLINIT));
      break;
    case NEXT:
      nusmv_assert(NODE_FROM_INT(SMALLINIT) == car(ass));
      setcdr(ass, NODE_FROM_INT(NEXT));
      break;
    case INVAR:
      error_unreachable_code();
      break;
    default:
      error_unreachable_code();
    }
  }
}

/**AutomaticEnd***************************************************************/
