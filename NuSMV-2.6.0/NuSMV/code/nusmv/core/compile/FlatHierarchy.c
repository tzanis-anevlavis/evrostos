/* ---------------------------------------------------------------------------


This file is part of the ``compile'' package of NuSMV version 2.
Copyright (C) 2005 by FBK-irst.

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
  \author Andrei Tchaltsev
  \brief The class is used to store results of flattening a hierarchy

  See description in FlatHierarchy.h

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/compile/FlatHierarchy.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/wff/ExprMgr.h"
#include "nusmv/core/utils/error.h"


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

struct FlatHierarchy
{
  SymbTable_ptr st;
  boolean vars_constr_built; /* whether vars constrains have been built */
  NodeMgr_ptr nodemgr;

  node_ptr init_expr;
  node_ptr invar_expr;
  node_ptr trans_expr;
  node_ptr input_expr;
  node_ptr assign_expr;
  node_ptr justice_expr; /* it is a cons list */
  node_ptr compassion_expr;
  node_ptr spec_expr;
  node_ptr ltlspec_expr;
  node_ptr invarspec_expr;
  node_ptr pslspec_expr;
  node_ptr compute_expr;

  node_ptr pred_list;
  node_ptr mirror_list;
  node_ptr property_patterns;

  Set_t var_set; /* variables declared in the given hierarchy */

  hash_ptr assign_hash; /* a hash table that for every variable
                           'var_name' the hash contains the following associations:
                           init(var_name) -> CONS(rhs, init_list)
                           where rhs is the right handsides of init-assignments of the
                           given variable
                           init-list is a list of INIT expressions of this hierarchy,
                           which contain the variable
                           next(var_name) -> CONS( rhs, trans-list)
                           where rhs can be
                           case module-1.running : rhs-1;
                           case module-2.running : rhs-2;
                           ...
                           case 1: var_name;

                           where rhs-n is the right hand side of
                           the next-assignment in the process module-n.
                           If there are no processes then the structure degrades
                           to just one 'rhs'.
                           trans-list is a list of TRANS of this hierarchy,
                           which contain var_name or expression next(var_name)

                           var_name : the structure is the same as for init(var_name) except
                           that the rhs-n are the right handside of invar-assignment, and
                           init-list is a list of INVAR expressions  */

  hash_ptr const_constr_hash;

  /* Hash table used for properties names univocity */
  hash_ptr property_hash;
};

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void flat_hierarchy_init(FlatHierarchy_ptr self,
                                SymbTable_ptr st);
static void flat_hierarchy_deinit(FlatHierarchy_ptr self);
static void flat_hierarchy_copy(const FlatHierarchy_ptr self,
                                FlatHierarchy_ptr other);
static void
flat_hierarchy_mergeinto(FlatHierarchy_ptr self,
                         FlatHierarchy_ptr other);

static void
flat_hierarchy_calc_var_const_aux(FlatHierarchy_ptr self,
                                  node_ptr expr, int type);

static void
flat_hierarchy_calc_var_const_recur(FlatHierarchy_ptr self,
                                    node_ptr expr, int type);

static boolean flat_hierarchy_check_const_deps(FlatHierarchy_ptr self,
                                               node_ptr expr, int type);

static void
flat_hiearchy_self_check_expr(const FlatHierarchy_ptr self, node_ptr expr);

static const char* constr_type_to_string(int type);

static void flat_hierarchy_visit_dag(hash_ptr outbounds, hash_ptr visited,
                                     node_ptr var, NodeList_ptr result);

static assoc_retval
flat_hierarchy_free_assign_fun(char * key, char * data, char * arg);
/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

FlatHierarchy_ptr FlatHierarchy_create(SymbTable_ptr st)
{
  FlatHierarchy_ptr self = ALLOC(struct FlatHierarchy, 1);
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  flat_hierarchy_init(self, st);
  return self;
}

FlatHierarchy_ptr
FlatHierarchy_create_from_members(SymbTable_ptr st,
                                  node_ptr init, node_ptr invar,
                                  node_ptr trans, node_ptr input,
                                  node_ptr justice, node_ptr compassion)
{
  FlatHierarchy_ptr self = FlatHierarchy_create(st);

  FlatHierarchy_set_init(self, init);
  FlatHierarchy_set_invar(self, invar);
  FlatHierarchy_set_trans(self, trans);
  FlatHierarchy_set_input(self, input);
  FlatHierarchy_set_justice(self, justice);
  FlatHierarchy_set_compassion(self, compassion);

  return self;
}

void FlatHierarchy_destroy(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  flat_hierarchy_deinit(self);
  FREE(self);
}

NuSMVEnv_ptr FlatHierarchy_get_environment(const FlatHierarchy_ptr self)
{
  return EnvObject_get_environment(ENV_OBJECT(self->st));
}

FlatHierarchy_ptr FlatHierarchy_copy(const FlatHierarchy_ptr self)
{
  FlatHierarchy_ptr res;

  FLAT_HIERARCHY_CHECK_INSTANCE(self);

  res = FlatHierarchy_create(self->st);
  flat_hierarchy_copy(self, res);
  return res;
}

void FlatHierarchy_mergeinto(FlatHierarchy_ptr self,
                             const FlatHierarchy_ptr other)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  FLAT_HIERARCHY_CHECK_INSTANCE(other);

  flat_hierarchy_mergeinto(self, other);
}

SymbTable_ptr FlatHierarchy_get_symb_table(const FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return self->st;
}

void FlatHierarchy_set_symb_table(const FlatHierarchy_ptr self,
                                  SymbTable_ptr symb_table)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  self->st = symb_table;
}

node_ptr FlatHierarchy_get_init(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return(self->init_expr);
}

void FlatHierarchy_set_init(FlatHierarchy_ptr self, node_ptr n)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);

  /* later recalculates vars constr if needed */
  self->vars_constr_built = self->vars_constr_built && (self->init_expr == n);
  self->init_expr = n;
}

void FlatHierarchy_add_init(FlatHierarchy_ptr self, node_ptr n)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
  ExprMgr_ptr const exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  node_ptr old;

  FLAT_HIERARCHY_CHECK_INSTANCE(self);

  old = self->init_expr;

  /* later recalculates vars constr if needed */
  self->init_expr = ExprMgr_and_nil(exprs, self->init_expr, n);
  self->vars_constr_built = self->vars_constr_built && (self->init_expr == old);
}


node_ptr FlatHierarchy_get_invar(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return(self->invar_expr);
}
void FlatHierarchy_set_invar(FlatHierarchy_ptr self, node_ptr n)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  /* later recalculates vars constr if needed */
  self->vars_constr_built = self->vars_constr_built && (self->invar_expr == n);
  self->invar_expr = n;
}

void FlatHierarchy_add_invar(FlatHierarchy_ptr self, node_ptr n)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
  ExprMgr_ptr const exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  node_ptr old;

  FLAT_HIERARCHY_CHECK_INSTANCE(self);

  old = self->invar_expr;

  /* later recalculates vars constr if needed */
  self->invar_expr = ExprMgr_and_nil(exprs, self->invar_expr, n);
  self->vars_constr_built = self->vars_constr_built && (self->invar_expr == old);
}

node_ptr FlatHierarchy_get_trans(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return(self->trans_expr);
}
void FlatHierarchy_set_trans(FlatHierarchy_ptr self, node_ptr n)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  /* later recalculates vars constr if needed */
  self->vars_constr_built = self->vars_constr_built && (self->trans_expr == n);
  self->trans_expr = n;
}

void FlatHierarchy_add_trans(FlatHierarchy_ptr self, node_ptr n)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
  ExprMgr_ptr const exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  node_ptr old;

  FLAT_HIERARCHY_CHECK_INSTANCE(self);

  old = self->trans_expr;

  /* later recalculates vars constr if needed */
  self->trans_expr = ExprMgr_and_nil(exprs, self->trans_expr, n);
  self->vars_constr_built = self->vars_constr_built && (self->trans_expr == old);
}

node_ptr FlatHierarchy_get_input(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return(self->input_expr);
}
void FlatHierarchy_set_input(FlatHierarchy_ptr self, node_ptr n)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  self->input_expr = n;
}

/* Warning: after flattening is done this function returns has a list
   of pairs <process-name, its-assignments>, where its-assignments
   consists of the original assignments from the input without the
   wrap of CASE with condition over "running" variable.  To access the
   actual assignments used in FSM it is necessary to access FlatHierarchy_lookup_assign
   as it is done in, for example, compile_write_flat_asgn.
*/
node_ptr FlatHierarchy_get_assign(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return(self->assign_expr);
}
void FlatHierarchy_set_assign(FlatHierarchy_ptr self, node_ptr n)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  self->assign_expr = n;
}

node_ptr FlatHierarchy_get_justice(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return(self->justice_expr);
}

void FlatHierarchy_set_justice(FlatHierarchy_ptr self, node_ptr n)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  self->justice_expr = n;
}

node_ptr FlatHierarchy_get_compassion(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return(self->compassion_expr);
}
void FlatHierarchy_set_compassion(FlatHierarchy_ptr self, node_ptr n)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  self->compassion_expr = n;
}

boolean FlatHierarchy_add_property_name(FlatHierarchy_ptr self,
                                        node_ptr name)
{
  /* Return false if the name is already in the hash. This means the
     name is duplicate */
  if (NODE_FROM_INT(1) == find_assoc(self->property_hash, name)){
    return false;
  }

  insert_assoc(self->property_hash, name, NODE_FROM_INT(1));
  return true;
}

node_ptr FlatHierarchy_get_spec(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return (self->spec_expr);
}

void FlatHierarchy_set_spec(FlatHierarchy_ptr self, node_ptr n)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  self->spec_expr = n;
}

node_ptr FlatHierarchy_get_ltlspec(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return(self->ltlspec_expr);
}
void FlatHierarchy_set_ltlspec(FlatHierarchy_ptr self, node_ptr n)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  self->ltlspec_expr = n;
}

node_ptr FlatHierarchy_get_invarspec(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return(self->invarspec_expr);
}
void FlatHierarchy_set_invarspec(FlatHierarchy_ptr self, node_ptr n)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  self->invarspec_expr = n;
}

node_ptr FlatHierarchy_get_pslspec(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return(self->pslspec_expr);
}
void FlatHierarchy_set_pslspec(FlatHierarchy_ptr self, node_ptr n)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  self->pslspec_expr = n;
}

node_ptr FlatHierarchy_get_compute(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return(self->compute_expr);
}
void FlatHierarchy_set_compute(FlatHierarchy_ptr self, node_ptr n)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  self->compute_expr = n;
}

Set_t FlatHierarchy_get_vars(const FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return self->var_set;
}

NodeList_ptr
FlatHierarchy_get_ordered_vars(const FlatHierarchy_ptr self,
                               hash_ptr* outbound_edges)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const MasterNormalizer_ptr normalizer =
    MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));

  NodeList_ptr result;
  SymbTable_ptr symb_table = self->st;
  Set_t nodes = Set_MakeEmpty();
  hash_ptr outbounds = new_assoc();
  assoc_iter iter;
  node_ptr var, assignment;

  /* Foreach assign variable, get it's assignment expression and build
     the DAG */
  ASSOC_FOREACH(self->assign_hash, iter, &var, &assignment) {
    boolean is_invar_assignment = false;
    Set_t deps;

    if (SMALLINIT == node_get_type(var)) {
      var = car(var);
    }
    else if (!(NEXT == node_get_type(var))) {
      is_invar_assignment = true;
    }

    deps = Formula_GetDependenciesByType(symb_table,
                                         MasterNormalizer_normalize_node(normalizer, assignment),
                                         Nil, VFT_CNIF, true);

    nodes = Set_AddMember(nodes, (Set_Element_t)var);
    if (is_invar_assignment) {
      /* Normal Assignment: We add both init(x) and next(x) */
      nodes = Set_AddMember(nodes, (Set_Element_t)ExprMgr_next(exprs, var, symb_table));
    }

    {
      Set_Iterator_t iter;
      SET_FOREACH(deps, iter) {
        node_ptr dep = Set_GetMember(deps, iter);
        Set_t out = (Set_t)find_assoc(outbounds, dep);

        if ((Set_t)NULL == out) {
          out = Set_MakeEmpty();
        }

        /* Create the edge */
        if (dep != var) {
          out = Set_AddMember(out, (Set_Element_t)var);
          if (is_invar_assignment) {
            /* invar (normal) Assignment: We add both init(x) and next(x) */
            out = Set_AddMember(out, (Set_Element_t)ExprMgr_next(exprs, var, symb_table));
          }
        }
        insert_assoc(outbounds, dep, (Set_Element_t)out);
        nodes = Set_AddMember(nodes, (Set_Element_t)dep);
      }
    }

    Set_ReleaseSet(deps);
  }

  /* Topological sort */
  {
    hash_ptr visited = new_assoc();
    Set_Iterator_t iter;
    result = NodeList_create();

    SET_FOREACH(nodes, iter) {
      node_ptr var = NODE_PTR(Set_GetMember(nodes, iter));
      flat_hierarchy_visit_dag(outbounds, visited, var, result);
    }

    free_assoc(visited);
  }

  Set_ReleaseSet(nodes);

  if ((hash_ptr*)NULL != outbound_edges) {
    *outbound_edges = outbounds;
  }
  /* If the caller did not request the hash map, free it */
  else {
    assoc_iter iter;
    node_ptr tmp;
    Set_t set;

    ASSOC_FOREACH(outbounds, iter, &tmp, &set) {
      Set_ReleaseSet(set);
    }
  }

  return result;
}

void FlatHierarchy_add_var(FlatHierarchy_ptr self, node_ptr n)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  self->var_set = Set_AddMember(self->var_set, (Set_Element_t) n);
}

void FlatHierarchy_remove_var(FlatHierarchy_ptr self, node_ptr n)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  self->var_set = Set_RemoveMember(self->var_set, (Set_Element_t) n);
}

node_ptr FlatHierarchy_lookup_assign(FlatHierarchy_ptr self, node_ptr name)
{

  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self->st));
  node_ptr res;
  nusmv_assert(self != NULL);

  /* Currently, bit selection is not allowed on the left side of
     assignments */
  if (node_get_type(name) == BIT_SELECTION ||
      ((node_get_type(name) == NEXT || node_get_type(name) == SMALLINIT) &&
       node_get_type(car(name)) == BIT_SELECTION)) {
    error_bit_selection_assignment_not_supported(env, name);
  }

  /* common consistency check : the name should be correctly formed */
  nusmv_assert(SMALLINIT == node_get_type(name) ||
               NEXT == node_get_type(name) ||
               DOT == node_get_type(name) || ATOM == node_get_type(name) ||
               ARRAY == node_get_type(name) || BIT == node_get_type(name));

  res = find_assoc(self->assign_hash, name);
  if (Nil == res) return Nil;

  nusmv_assert(CONS == node_get_type(res));

  return car(res);
}

void FlatHierarchy_insert_assign(FlatHierarchy_ptr self, node_ptr name,
                                 node_ptr assign)
{
  node_ptr cont;

  nusmv_assert(self != NULL);
  cont = find_assoc(self->assign_hash, name);

  if (Nil == cont) { /* there was no container before => create a new one */
    cont = cons(self->nodemgr, Nil, Nil);
    insert_assoc(self->assign_hash, name, cont);
  }

  /* name and init(name) can have only one assignment */
  nusmv_assert( (node_get_type(name) != SMALLINIT &&
                 node_get_type(name) != DOT) || Nil == car(cont));
  /* If change this, we *MUST* also change simp/fsm/logic_elimination.c
     which uses this feature to overwrite next after a copy.
   */

  /* set car is allowed here as the node was created with new_node. */
  setcar(cont, assign);
}

node_ptr FlatHierarchy_lookup_constrains(FlatHierarchy_ptr self,
                                         node_ptr name)
{
  node_ptr res;
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  FLAT_HIERARCHY_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->st));
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  /* common consistency check : the name should be correctly formed */
  nusmv_assert(SMALLINIT == node_get_type(name) || NEXT == node_get_type(name) ||
               DOT == node_get_type(name) ||
               ARRAY == node_get_type(name) || BIT == node_get_type(name) ||
               ATOM == node_get_type(name));

  /* if not previously calculated (or if invalidated by later
     settings) triggers calculation of var constrains */
  if (!self->vars_constr_built) {
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Triggering postponed "\
              "calculation of vars constrains\n");
    }
    FlatHierarchy_calculate_vars_constrains(self);
  }

  res = find_assoc(self->assign_hash, name);
  if (Nil == res) return Nil;

  nusmv_assert(CONS == node_get_type(res));

  return cdr(res);
}

void FlatHierarchy_add_constrains(FlatHierarchy_ptr self, node_ptr name,
                                  node_ptr expr)
{
  node_ptr cont;

  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  cont = find_assoc(self->assign_hash, name);

  if (Nil == cont) { /* there was no container before => create a new one */
    cont = cons(self->nodemgr, Nil, Nil);
    insert_assoc(self->assign_hash, name, cont);
  }

  if (Nil == cdr(cont)) setcdr(cont, expr);
  else setcdr(cont, find_node(self->nodemgr, AND, cdr(cont), expr));
}

node_ptr FlatHierarchy_lookup_constant_constrains(FlatHierarchy_ptr self,
                                                  int type)
{
  node_ptr res;
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  FLAT_HIERARCHY_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->st));
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  nusmv_assert(INIT == type || TRANS == type || INVAR == type);

  /* if not previously calculated (or if invalidated by later
     settings) triggers calculation of var constrains */
  if (!self->vars_constr_built) {
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Triggering postponed "\
              "calculation of vars constrains\n");
    }
    FlatHierarchy_calculate_vars_constrains(self);
  }

  res = find_assoc(self->const_constr_hash, NODE_FROM_INT(type));
  return res;
}

void FlatHierarchy_add_constant_constrains(FlatHierarchy_ptr self,
                                           node_ptr expr,
                                           int type)
{
  node_ptr tmp;

  FLAT_HIERARCHY_CHECK_INSTANCE(self);

  nusmv_assert(INIT == type || TRANS == type || INVAR == type);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
    const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

    tmp = find_assoc(self->const_constr_hash, NODE_FROM_INT(type));

    /* If a constrain already exist, put it in AND with the new one.. */
    if (Nil != tmp) { tmp = ExprMgr_and(exprs, tmp, expr); }
    else {
      tmp = expr;
    }

    insert_assoc(self->const_constr_hash, NODE_FROM_INT(type), tmp);
  }
}

hash_ptr FlatHierarchy_get_constants_associations(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return self->const_constr_hash;
}

void FlatHierarchy_set_constants_associations(FlatHierarchy_ptr self,
                                              hash_ptr h)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  self->const_constr_hash = h;
}

void FlatHierarchy_clear_constants_associations(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  clear_assoc(self->const_constr_hash);
}

void FlatHierarchy_calculate_vars_constrains(FlatHierarchy_ptr self)
{
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  FLAT_HIERARCHY_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->st));
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "FlatHierarchy: "         \
            "calculating vars constrains...\n");
  }

  flat_hierarchy_calc_var_const_aux(self, FlatHierarchy_get_init(self), INIT);
  flat_hierarchy_calc_var_const_aux(self, FlatHierarchy_get_invar(self), INVAR);
  flat_hierarchy_calc_var_const_aux(self, FlatHierarchy_get_trans(self), TRANS);

  self->vars_constr_built = true;
  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "FlatHierarchy: vars constrains calculated\n");
  }
}

hash_ptr FlatHierarchy_get_var_expr_associations(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  return self->assign_hash;
}

void FlatHierarchy_set_var_expr_associations(FlatHierarchy_ptr self,
                                             hash_ptr h)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  self->assign_hash = h;
}

void FlatHierarchy_clear_var_expr_associations(FlatHierarchy_ptr self)
{
  FLAT_HIERARCHY_CHECK_INSTANCE(self);
  clear_assoc(self->assign_hash);
}

node_ptr FlatHierarchy_get_preds(FlatHierarchy_ptr cmp)
{
  return cmp->pred_list;
}

void FlatHierarchy_add_pred(FlatHierarchy_ptr cmp, node_ptr n)
{
  cmp->pred_list = cons(cmp->nodemgr, n, cmp->pred_list);
}

void FlatHierarchy_set_pred(FlatHierarchy_ptr cmp, node_ptr n)
{
  cmp->pred_list = n;
}

node_ptr FlatHierarchy_get_mirrors(FlatHierarchy_ptr cmp)
{
  return cmp->mirror_list;
}

void FlatHierarchy_add_mirror(FlatHierarchy_ptr cmp, node_ptr n)
{
  cmp->mirror_list = cons(cmp->nodemgr, n, cmp->mirror_list);
}

void FlatHierarchy_set_mirror(FlatHierarchy_ptr cmp, node_ptr n)
{
  cmp->mirror_list = n;
}

node_ptr FlatHierarchy_get_property_patterns(FlatHierarchy_ptr cmp)
{
  return cmp->property_patterns;
}

void FlatHierarchy_add_property_pattern(FlatHierarchy_ptr cmp, node_ptr n)
{
  cmp->property_patterns = cons(cmp->nodemgr, n, cmp->property_patterns);
}

void FlatHierarchy_set_property_patterns(FlatHierarchy_ptr cmp, node_ptr n)
{
  cmp->property_patterns = n;
}


void FlatHierarchy_self_check(const FlatHierarchy_ptr self)
{
  flat_hiearchy_self_check_expr(self, self->init_expr);
  flat_hiearchy_self_check_expr(self, self->invar_expr);
  flat_hiearchy_self_check_expr(self, self->trans_expr);
  flat_hiearchy_self_check_expr(self, self->input_expr);
  flat_hiearchy_self_check_expr(self, self->assign_expr);
  flat_hiearchy_self_check_expr(self, self->justice_expr);
  flat_hiearchy_self_check_expr(self, self->compassion_expr);
  flat_hiearchy_self_check_expr(self, self->spec_expr);
  flat_hiearchy_self_check_expr(self, self->ltlspec_expr);
  flat_hiearchy_self_check_expr(self, self->invarspec_expr);
  flat_hiearchy_self_check_expr(self, self->compute_expr);
}

/*!
  \brief


*/

void FlatHierarchy_type_check(FlatHierarchy_ptr self)
THROWS_EXCEPTION
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self->st));
  NodeMgr_ptr const nodemgr =
     NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  ErrorMgr_ptr const errmgr =
   ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  boolean isOk = true;
  SymbTable_ptr symb_table = self->st;
  TypeChecker_ptr type_checker = SymbTable_get_type_checker(symb_table);
  NodeList_ptr layers = SymbTable_get_layers(symb_table);
  ListIter_ptr iter;
  SymbLayer_ptr layer = NULL;
  node_ptr assign_list = Nil;

  NODE_LIST_FOREACH(layers, iter) {
    layer = SYMB_LAYER(NodeList_get_elem_at(layers, iter));

    isOk = isOk && TypeChecker_check_layer(type_checker, layer);
  }

  /* get rid of module names */
  assign_list = map(nodemgr, cdr, FlatHierarchy_get_assign(self));

  isOk = isOk && TypeChecker_check_constrains(type_checker,
                                              FlatHierarchy_get_init(self),
                                              FlatHierarchy_get_trans(self),
                                              FlatHierarchy_get_invar(self),
                                              assign_list,
                                              FlatHierarchy_get_justice(self),
                                              FlatHierarchy_get_compassion(self));

  if (!isOk) ErrorMgr_error_type_system_violation(errmgr);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/* Static function definitions                                               */
/*---------------------------------------------------------------------------*/

/*!
  \brief initialisation function used by the constructor


*/
static void flat_hierarchy_init(FlatHierarchy_ptr self, SymbTable_ptr st)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  self->nodemgr = nodemgr;
  self->st                   = st;
  self->vars_constr_built    = false;

  /* if these are list, they should be initialized with new_list() */
  self->init_expr            = Nil;
  self->invar_expr           = Nil;
  self->trans_expr           = Nil;
  self->input_expr           = Nil;
  self->assign_expr          = Nil;
  self->justice_expr         = Nil;
  self->compassion_expr      = Nil;
  self->spec_expr            = Nil;
  self->compute_expr         = Nil;
  self->ltlspec_expr         = Nil;
  self->pslspec_expr         = Nil;
  self->invarspec_expr       = Nil;
  self->var_set              = Set_MakeEmpty();

  self->pred_list            = Nil;
  self->mirror_list          = Nil;
  self->property_patterns    = Nil;

  self->assign_hash = new_assoc();
  self->property_hash = new_assoc();
  self->const_constr_hash = new_assoc();
}

/*!
  \brief


*/
static assoc_retval
flat_hierarchy_free_assign_fun(char * key, char * data, char * arg)
{
  NodeMgr_ptr nodemgr = NODE_MGR(arg);
  node_ptr n = NODE_PTR(data);

  UNUSED_PARAM(key);

  if (Nil != n) {
    free_node(nodemgr, n);
  }

  return ASSOC_DELETE;
}

/*!
  \brief de-initialisation function used by the destructor


*/
static void flat_hierarchy_deinit(FlatHierarchy_ptr self)
{
  /* memory leak, the lists are not freed */
  /* not all field are reset */

  Set_ReleaseSet(self->var_set);

  clear_assoc_and_free_entries_arg(self->assign_hash,
                                   flat_hierarchy_free_assign_fun,
                                   (char*)self->nodemgr);
  free_assoc(self->assign_hash);
  free_assoc(self->property_hash);
  free_assoc(self->const_constr_hash);
}

/*!
  \brief Copies self's data into other, so that other contains
the same information

*/
static void
flat_hierarchy_copy(const FlatHierarchy_ptr self, FlatHierarchy_ptr other)
{
  other->nodemgr              = self->nodemgr;
  other->st                   = self->st;
  other->vars_constr_built    = self->vars_constr_built;
  other->init_expr            = self->init_expr;
  other->invar_expr           = self->invar_expr;
  other->trans_expr           = self->trans_expr;
  other->input_expr           = self->input_expr;
  other->assign_expr          = self->assign_expr;
  other->justice_expr         = self->justice_expr;
  other->compassion_expr      = self->compassion_expr;
  other->spec_expr            = self->spec_expr;
  other->compute_expr         = self->compute_expr;
  other->ltlspec_expr         = self->ltlspec_expr;
  other->pslspec_expr         = self->pslspec_expr;
  other->invarspec_expr       = self->invarspec_expr;
  other->var_set              = Set_Copy(self->var_set);

  other->pred_list            = self->pred_list;
  other->mirror_list          = self->mirror_list;
  other->property_patterns    = self->property_patterns;

  /* Make a valid copy of the assignments hash. Note that all key
     associated values are CONS nodes, and need to be copied in order
     to not share any node instance. Sharing nodes means that
     modifying the "self" flat hierarchy instance, will have
     side-effects on the "other" flat hierarchy instance */
  clear_assoc(other->assign_hash);
  {
    node_ptr old_cons;
    node_ptr tmp;
    assoc_iter iter;

    ASSOC_FOREACH(self->assign_hash, iter, &tmp, &old_cons) {
      node_ptr new_cons = cons(self->nodemgr, car(old_cons), cdr(old_cons));
      insert_assoc(other->assign_hash, tmp, new_cons);
    }
  }

  free_assoc(other->property_hash);
  other->property_hash = copy_assoc(self->property_hash);

  free_assoc(other->const_constr_hash);
  other->const_constr_hash = copy_assoc(self->const_constr_hash);
}

/*!
  \brief Performs the actual merging.


*/
static void
flat_hierarchy_mergeinto(FlatHierarchy_ptr self,
                         const FlatHierarchy_ptr other)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  nusmv_assert(self->st == other->st); /* same symbol table */

  /* exprs: conjunct or concatenate (components of) the two */
  self->init_expr       = NODE_PTR(ExprMgr_and_nil(exprs, EXPR(self->init_expr),
                                                EXPR(other->init_expr)));
  self->invar_expr      = NODE_PTR(ExprMgr_and_nil(exprs, EXPR(self->invar_expr),
                                                EXPR(other->invar_expr)));
  self->trans_expr      = NODE_PTR(ExprMgr_and_nil(exprs, EXPR(self->trans_expr),
                                                EXPR(other->trans_expr)));
  self->input_expr      = NODE_PTR(ExprMgr_and_nil(exprs, EXPR(self->input_expr),
                                                EXPR(other->input_expr)));

/* in assignments first search if each module in other exists in
 * self. In this case adds that module assiments to self. Otherwise
 * append simply the module assigments to self */
  {
    /* this hash is used to reduce complexy from N^2 to linear in
       size of other+self */
    hash_ptr other_assgns = new_assoc();
    node_ptr iter, name, assgn;
    assoc_iter aiter;

    /* 1. Here assignments in other are collected */
    for (iter=other->assign_expr; iter != Nil; iter=cdr(iter)) {
      node_ptr assgn, inst_assgn;
      nusmv_assert(node_get_type(iter) == CONS);

      assgn = car(iter);
      nusmv_assert(node_get_type(assgn) == CONS);

      /* inst name in in car, inst assign in cdr */
      inst_assgn = cdr(assgn);
      if (inst_assgn != Nil && !ExprMgr_is_true(exprs, inst_assgn)) {
        /* stores the assignement to be used later when traversing self */
        node_ptr inst_name = car(assgn);
        node_ptr old_assgn = ExprMgr_and_nil(exprs, find_assoc(other_assgns, inst_name),
                                          inst_assgn);
        insert_assoc(other_assgns, inst_name, old_assgn);
      }
    }

    /* 2. How traverses assignments in self and conjunct those
     * found in other */
    for (iter=self->assign_expr; iter != Nil; iter=cdr(iter)) {
      node_ptr assgn, inst_name, other_assgn;
      nusmv_assert(node_get_type(iter) == CONS);

      assgn = car(iter);
      nusmv_assert(node_get_type(assgn) == CONS);

      /* inst name in in car, inst assign in cdr */
      inst_name = car(assgn);

      /* is this in other as well? */
      other_assgn = find_assoc(other_assgns, inst_name);
      if (other_assgn != Nil) {
        node_ptr inst_assgn = cdr(assgn);
        inst_assgn = ExprMgr_and_nil(exprs, inst_assgn, other_assgn);
        setcdr(assgn, inst_assgn);

        /* reset assign in hash so at the end all remaining non-nil
         * values will be appended at the end, as they are those
         * assignments in other that do not occur in self */
        insert_assoc(other_assgns, inst_name, Nil);
      }
    }

    /* 3. Finally adds remaining (new) assigments. true here is
          essential to avoid previously already handled
          assignments */
    ASSOC_FOREACH(other_assgns, aiter, &name, &assgn) {
      self->assign_expr = cons(self->nodemgr, cons(self->nodemgr, name, assgn), self->assign_expr);
    }

    free_assoc(other_assgns);
  } /* end of assignments merging */

  /* Constant constrains merging */
  {
    node_ptr tmp = FlatHierarchy_lookup_constant_constrains(other, INIT);
    FlatHierarchy_add_constant_constrains(self, tmp, INIT);

    tmp = FlatHierarchy_lookup_constant_constrains(other, INVAR);
    FlatHierarchy_add_constant_constrains(self, tmp, INVAR);

    tmp = FlatHierarchy_lookup_constant_constrains(other, TRANS);
    FlatHierarchy_add_constant_constrains(self, tmp, TRANS);
  }

  self->justice_expr    = append_ns(nodemgr, self->justice_expr,
                                    other->justice_expr);
  self->compassion_expr = append_ns(nodemgr, self->compassion_expr,
                                    other->compassion_expr);
  self->spec_expr       = append_ns(nodemgr, self->spec_expr,
                                    other->spec_expr);
  self->ltlspec_expr    = append_ns(nodemgr, self->ltlspec_expr,
                                    other->ltlspec_expr);
  self->invarspec_expr  = append_ns(nodemgr, self->invarspec_expr,
                                    other->invarspec_expr);
  self->pslspec_expr    = append_ns(nodemgr, self->pslspec_expr,
                                    other->pslspec_expr);
  self->compute_expr    = append_ns(nodemgr, self->compute_expr,
                                    other->compute_expr);

  /* pred_list and mirror_list : append other to self */
  self->pred_list = append_ns(nodemgr, self->pred_list,
                              other->pred_list);

  /* [MD] Mirror list was not merged, I think that it was a bug */
  self->mirror_list = append_ns(nodemgr, self->mirror_list,
                                other->mirror_list);

  self->property_patterns = append_ns(nodemgr, self->property_patterns,
                                      other->property_patterns);

  /* assign_hash: merge other in; deal with assign_hash before
     var_set to still have both var_set available.  Additional
     checking, can be disabled for performance; follows [AT]s comment
     above. */
  {
    Set_t intersect = Set_Copy(self->var_set);
    Set_Iterator_t iter;

    intersect = Set_Intersection(intersect, other->var_set);
    SET_FOREACH(intersect, iter) {
      node_ptr var = Set_GetMember(intersect, iter);
      node_ptr init_var = find_node(self->nodemgr, SMALLINIT, var, Nil);
      node_ptr next_var = find_node(self->nodemgr, NEXT, var, Nil);
      boolean self_has_init, self_has_next, self_has_invar;
      boolean other_has_init, other_has_next, other_has_invar;

      self_has_init = (Nil != FlatHierarchy_lookup_assign(self, init_var));
      self_has_next = (Nil != FlatHierarchy_lookup_assign(self, next_var));
      self_has_invar = (Nil != FlatHierarchy_lookup_assign(self, var));

      other_has_init = (Nil != FlatHierarchy_lookup_assign(other, init_var));
      other_has_next = (Nil != FlatHierarchy_lookup_assign(other, next_var));
      other_has_invar = (Nil != FlatHierarchy_lookup_assign(other, var));

      /* No duplicate init assignments */
      nusmv_assert(!(self_has_init && other_has_init));
      /* No duplicate next assignments */
      nusmv_assert(!(self_has_next && other_has_next));
      /* No duplicate invar assignments */
      nusmv_assert(!(self_has_invar && other_has_invar));

      /* No assignments at all if invar (normal) assignment
         other_has_invar -> !self_has_init & !self_has_next
         self_has_invar -> !other_has_init & !other_has_next
       */
      nusmv_assert((!other_has_invar) |
                   ((!self_has_init) && (!self_has_next)));
      nusmv_assert((!self_has_invar) |
                   ((!other_has_init) && (!other_has_next)));

    }

    Set_ReleaseSet(intersect);
  }

  /* appends the var list of other to self */
  self->var_set = Set_Union(self->var_set, other->var_set);

  {
    Set_t vars = FlatHierarchy_get_vars(other);
    Set_Iterator_t vars_iter;

  /* merge assigns and constrains
   * [VS] note that no simplification occurs. MR said that should be ok */
    SET_FOREACH(vars, vars_iter) {
      node_ptr var, ivar, nvar, ass, con;

      /* var */
      var = Set_GetMember(vars, vars_iter);
      ass = FlatHierarchy_lookup_assign(other, var);
      if (Nil != ass) {
        FlatHierarchy_insert_assign(self, var, ass);
      }
      con = FlatHierarchy_lookup_constrains(other, var);
      if (Nil != con) {
        FlatHierarchy_add_constrains(self, var, con);
      }

      /* init(var) */
      ivar = find_node(self->nodemgr, SMALLINIT, var, Nil);
      ass = FlatHierarchy_lookup_assign(other, ivar);
      if (Nil != ass) {
        FlatHierarchy_insert_assign(self, ivar, ass);
      }
      con = FlatHierarchy_lookup_constrains(other, ivar);
      if (Nil != con) {
        FlatHierarchy_add_constrains(self, ivar, con);
      }

      /* next(var) */
      nvar = find_node(self->nodemgr, NEXT, var, Nil);
      ass = FlatHierarchy_lookup_assign(other, nvar);
      if (Nil != ass) {
        FlatHierarchy_insert_assign(self, nvar, ass);
      }
      con = FlatHierarchy_lookup_constrains(other, nvar);
      if (Nil != con) {
        FlatHierarchy_add_constrains(self, nvar, con);
      }
    } /* loop over other's vars */
  }

  {
    assoc_iter aiter;
    node_ptr name;
    ASSOC_FOREACH(other->property_hash, aiter, &name, NULL) {
      nusmv_assert(NODE_FROM_INT(1) != find_assoc(self->property_hash, name));
      insert_assoc(self->property_hash, name, NODE_FROM_INT(1));
    }
  }
}

/*!
  \brief

  see compileFlattenSexpModel
*/
static void
flat_hierarchy_calc_var_const_aux(FlatHierarchy_ptr self,
                                  node_ptr expr, int type)
{
  int saved_yylineno = nusmv_yylineno;
  if (expr == Nil) return;
  nusmv_yylineno = node_get_lineno(expr);
  flat_hierarchy_calc_var_const_recur(self, expr, type);
  nusmv_yylineno = saved_yylineno;
}

/*!
  \brief

  see compileFlattenSexpModel
*/
static void
flat_hierarchy_calc_var_const_recur(FlatHierarchy_ptr self,
                                    node_ptr expr, int type)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  if (expr == Nil) return;

  if (node_get_type(expr) == AND) {
    flat_hierarchy_calc_var_const_aux(self, car(expr), type);
    flat_hierarchy_calc_var_const_aux(self, cdr(expr), type);
  }
  else {
    Set_t deps;
    Set_Iterator_t iter;
    deps = Formula_GetDependencies(self->st, expr, Nil);

    /* If there are no variables in the dependencies set, and the
       expression is not simply "TRUE", add it to the set of constant
       constrains. */
    if (Set_IsEmpty(deps) &&
        flat_hierarchy_check_const_deps(self, expr, type)) {
      FlatHierarchy_add_constant_constrains(self, expr, type);
    }

    SET_FOREACH(deps, iter) {
      node_ptr var = (node_ptr) Set_GetMember(deps, iter);
      node_ptr index;

      switch (type) {
      case INIT: index = find_node(self->nodemgr, SMALLINIT, var, Nil); break;
      case INVAR: index = var; break;
      case TRANS: index = find_node(self->nodemgr, NEXT, var, Nil); break;
      default:
        {
          const ErrorMgr_ptr errmgr =
            ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

          StreamMgr_print_error(streams,
                  "flat_hierarchy_calc_var_const_recur: Unknown expr type\n");
          ErrorMgr_nusmv_exit(errmgr, 1);
        }
        break;
      }
      FlatHierarchy_add_constrains(self, index, expr);
    } /* for loop*/

    Set_ReleaseSet(deps);
  }
}

/*!
  \brief Called when a constant has been found in INVAR, INIT or
TRANS

  If the constant is trivially true, false is returned.
                     In all other cases, this function returns true
                     NB: This function has been keept ONLY for the
                     verbosity it produces
*/
static boolean flat_hierarchy_check_const_deps(FlatHierarchy_ptr self,
                                               node_ptr expr, int type)
{
  ErrorMgr_ptr errmgr;
  boolean keep = false;
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;
  ExprMgr_ptr exprs;
  MasterPrinter_ptr wffprint;

  FLAT_HIERARCHY_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->st));
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  wffprint = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Constant expression found in a %s statement in",
            constr_type_to_string(type));
    ErrorMgr_start_parsing_err(errmgr);

    Logger_log(logger, " The expression is \"");
    Logger_nlog(logger, wffprint, "%N",  expr);
    Logger_log(logger, "\"");
  }

  if (ExprMgr_is_true(exprs, expr)) {
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, " (Skipped)\n");
    }
    keep = false;
  }
  else {
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "\n");
    }
    keep = true;
  }

  return keep;
}

/*!
  \brief Internal service to self-check that a given
                       expression's language is contained within
                       the set of variables which this FH was
                       declared to have at construction time

  optional

  \sa optional
*/
static void
flat_hiearchy_self_check_expr(const FlatHierarchy_ptr self, node_ptr expr)
{
  Set_t deps = Formula_GetDependencies(self->st, expr, Nil);

  if (! Set_Contains(self->var_set, deps)) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->st));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
    MasterPrinter_ptr const wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
    StreamMgr_ptr const streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

    StreamMgr_nprint_error(streams, wffprint, "The dependency set of expr\n%N\n",
                           expr);

    Set_PrintSet(wffprint,
                 StreamMgr_get_error_stream(streams),
                 deps,
                 NULL, NULL);

    StreamMgr_print_error(streams, "is not cointained in the set of vars of the flat hierarchy");

    Set_PrintSet(wffprint,
                 StreamMgr_get_error_stream(streams),
                 self->var_set,
                 NULL, NULL);

    ErrorMgr_internal_error(errmgr, "FlatHierachy failed self-check.");
  }

  Set_ReleaseSet(deps);
}

/*!
  \brief required

  optional

  \sa optional
*/
static const char* constr_type_to_string(int type)
{
  switch (type) {
  case INIT: return "INIT";
  case INVAR: return "INVAR";
  case TRANS: return "TRANS";
  default:
    error_unreachable_code();
  }
}

typedef struct dag_TAG {
  node_ptr var;
  Set_t inbounds;
  Set_t outbounds;
} dag;

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void flat_hierarchy_visit_dag(hash_ptr outbounds, hash_ptr visited,
                                     node_ptr var, NodeList_ptr result)
{
  /* Not not yet visited */
  if (Nil == find_assoc(visited, var)) {
    Set_Iterator_t iter;
    Set_t outb = (Set_t)find_assoc(outbounds, var);

    /* Set the node to visited */
    insert_assoc(visited, var, NODE_FROM_INT(1));
    SET_FOREACH(outb, iter) {
      node_ptr out = (node_ptr)Set_GetMember(outb, iter);
      flat_hierarchy_visit_dag(outbounds, visited, out, result);
    }
    NodeList_prepend(result, var);
  }
}
