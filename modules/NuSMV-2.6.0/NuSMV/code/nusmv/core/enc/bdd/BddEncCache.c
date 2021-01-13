/* ---------------------------------------------------------------------------


  This file is part of the ``enc.bdd'' package of NuSMV version 2.
  Copyright (C) 2003 by FBK-irst.

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
  \brief The BddEncCache class implementation. This class is
  intended to be used exclusively by the class BddEnc.

  \todo: Missing description

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/enc/bdd/BddEncCache.h"
#include "nusmv/core/enc/bdd/BddEnc_private.h"
#include "nusmv/core/enc/bdd/bddInt.h"
#include "nusmv/core/utils/Logger.h"

#include "nusmv/core/utils/error.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/compile/compile.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct BddEncCache_TAG
{
  SymbTable_ptr symb_table;
  DDMgr_ptr dd;

  /* This hash associates to an atom the corresponding ADD leaf if
  defined. Suppose to have a declaration of this kind:
     VAR state : {idle, stopped}
  then in the constant hash for the atom idle there is the
  corresponding leaf ADD, i.e. the ADD whose value is the symbol
  idle. This hash is used by the evaluator.
  Also, this keeps track of reference counting of multiple times
  declared constants. */
  hash_ptr constant_hash;

  /* associates var names with corresponding ADDs */
  hash_ptr vars_hash;

  /* hash table used by the evaluator */
  hash_ptr eval_hash;

} BddEncCache;


/*---------------------------------------------------------------------------*/
/* macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void
bdd_enc_cache_init(BddEncCache_ptr self, SymbTable_ptr symb_table,
                   DDMgr_ptr dd);

static void bdd_enc_cache_deinit(BddEncCache_ptr self);

static assoc_retval
hash_free_add(char* key, char* data, char* arg);

static assoc_retval
hash_free_add_array(char* key, char* data, char* arg);

static assoc_retval
hash_free_add_counted(char* key, char* data, char* arg);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

BddEncCache_ptr BddEncCache_create(SymbTable_ptr symb_table, DDMgr_ptr dd)
{
  BddEncCache_ptr self = ALLOC(BddEncCache, 1);

  BDD_ENC_CACHE_CHECK_INSTANCE(self);

  bdd_enc_cache_init(self, symb_table, dd);
  return self;
}

void BddEncCache_destroy(BddEncCache_ptr self)
{
  BDD_ENC_CACHE_CHECK_INSTANCE(self);

  bdd_enc_cache_deinit(self);
  FREE(self);
}

void BddEncCache_new_constant(BddEncCache_ptr self, node_ptr constant,
                              add_ptr constant_add)
{
  node_ptr data;

  BDD_ENC_CACHE_CHECK_INSTANCE(self);

  /* we don't store number constants into the symb table, so we are
     lazy in that case */
  nusmv_assert(SymbTable_is_symbol_constant(self->symb_table, constant)
               || (node_get_type(constant) == NUMBER));

  /* Not already defined. We reuse already defined leaf */
  if (! BddEncCache_is_constant_encoded(self, constant)) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->dd));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    data = new_node(nodemgr, CONS, NODE_FROM_INT(1), (node_ptr) add_dup(constant_add));
    insert_assoc(self->constant_hash, constant, data);
  }
  else { /* increments the ref counter: */
    data = find_assoc(self->constant_hash, constant);

    nusmv_assert(data != NODE_PTR(NULL));
    /* ADD for a constant cannot change over time */
    nusmv_assert(constant_add == (add_ptr)cdr(data));

    setcar(data, NODE_FROM_INT(NODE_TO_INT(car(data)) + 1));
  }

}

void BddEncCache_remove_constant(BddEncCache_ptr self, node_ptr constant)
{
  node_ptr data;
  int num;

  BDD_ENC_CACHE_CHECK_INSTANCE(self);

  data = find_assoc(self->constant_hash, constant);
  nusmv_assert(data != (node_ptr) NULL);
  num = NODE_TO_INT(car(data));

  if (num <= 1) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->dd));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    add_free(self->dd, (add_ptr) cdr(data));
    free_node(nodemgr, data);
    remove_assoc(self->constant_hash, constant);
  }
  else {
    setcar(data, NODE_FROM_INT(num - 1));
  }
}

boolean BddEncCache_is_constant_encoded(const BddEncCache_ptr self,
                                        node_ptr constant)
{
  node_ptr data;

  BDD_ENC_CACHE_CHECK_INSTANCE(self);

  data = find_assoc(self->constant_hash, constant);

  return ((data != (node_ptr) NULL) && (NODE_TO_INT(car(data)) > 0));
}

add_ptr BddEncCache_lookup_constant(const BddEncCache_ptr self,
                                    node_ptr constant)
{
  node_ptr data;
  add_ptr res = (add_ptr) NULL;

  BDD_ENC_CACHE_CHECK_INSTANCE(self);

  data = find_assoc(self->constant_hash, constant);
  if ((data != (node_ptr) NULL) &&  (NODE_TO_INT(car(data)) > 0)) {
    res = (add_ptr) cdr(data);
    if (res != (add_ptr) NULL) { add_ref(res); }
  }

  return res;
}

void BddEncCache_new_boolean_var(BddEncCache_ptr self, node_ptr var_name,
                                 add_ptr var_add)
{
  BDD_ENC_CACHE_CHECK_INSTANCE(self);

  /* only variables declared inside the symbolic table
     and optionally wrapped in NEXT are allowed here */
  if (SymbTable_is_symbol_var(self->symb_table, var_name) ||
      (node_get_type(var_name) == NEXT &&
       SymbTable_is_symbol_var(self->symb_table, car(var_name)))) {
    /* not already encoded */
    nusmv_assert(! BddEncCache_is_boolean_var_encoded(self, var_name));

    add_ref(var_add);
    insert_assoc(self->vars_hash, var_name, (node_ptr) var_add);
  }
  else {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->dd));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    ErrorMgr_internal_error(errmgr, "BddEncCache: trying to encode a new var not "   \
                   "previously declared\n");
  }
}

void BddEncCache_remove_boolean_var(BddEncCache_ptr self, node_ptr var_name)
{
  add_ptr add;

  BDD_ENC_CACHE_CHECK_INSTANCE(self);

  add = (add_ptr) remove_assoc(self->vars_hash, var_name);
  nusmv_assert(add != (add_ptr) NULL);

  add_free(self->dd, add);
}

boolean BddEncCache_is_boolean_var_encoded(const BddEncCache_ptr self,
                                           node_ptr var_name)
{
  BDD_ENC_CACHE_CHECK_INSTANCE(self);
  return (find_assoc(self->vars_hash, var_name) != (node_ptr) NULL);
}

add_ptr BddEncCache_lookup_boolean_var(const BddEncCache_ptr self,
                                       node_ptr var_name)
{
  add_ptr res;

  BDD_ENC_CACHE_CHECK_INSTANCE(self);

  res = (add_ptr) find_assoc(self->vars_hash, var_name);
  if (res != (add_ptr) NULL) { add_ref(res); }

  return res;
}

void BddEncCache_set_evaluation(BddEncCache_ptr self, node_ptr expr,
                                AddArray_ptr add_array)
{
  AddArray_ptr old_array;

  BDD_ENC_CACHE_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->dd));
    const OptsHandler_ptr opts =
      OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

    if (!opt_enable_sexp2bdd_caching(opts)) { /* caching is disabled */
      if (add_array != BDD_ENC_EVALUATING && add_array != ADD_ARRAY(NULL)) {
        AddArray_destroy(self->dd, add_array);
      }
      return;
    }

    old_array = ADD_ARRAY(find_assoc(self->eval_hash, expr));
    if ((old_array != BDD_ENC_EVALUATING) && (old_array != ADD_ARRAY(NULL))){
      nusmv_assert(old_array != add_array);
      AddArray_destroy(self->dd, old_array);
    }

    {
      static unsigned int _counter = 0;
      NuSMVEnv_ptr env = EnvObject_env(ENV_OBJECT(self->symb_table));
      const OptsHandler_ptr opts =
        OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
      if (opt_verbose_level_gt(opts, 4)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        const MasterPrinter_ptr wffprint =
          MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

        Logger_nlog(logger, wffprint,
                    "BddEncCache %x: (%u) inserting evaluation of expression: "
                    "\n--->  '%N'\n",
                    self, ++_counter, expr);
      }
    }
    insert_assoc(self->eval_hash, expr, (node_ptr) add_array);
  }
}

void BddEncCache_remove_evaluation(BddEncCache_ptr self, node_ptr expr)
{
  NuSMVEnv_ptr env = EnvObject_env(ENV_OBJECT(self->symb_table));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  AddArray_ptr old_array;
  BDD_ENC_CACHE_CHECK_INSTANCE(self);

  if (opt_verbose_level_gt(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    Logger_nlog(logger, wffprint,
                "BddEncCache %x: removing evaluation of expression: "
                "\n--->  '%N'\n",
                self, expr);
  }

  old_array = ADD_ARRAY(remove_assoc(self->eval_hash, expr));
  if ((old_array != BDD_ENC_EVALUATING) && (old_array != ADD_ARRAY(NULL))){
    AddArray_destroy(self->dd, old_array);
  }
}

AddArray_ptr BddEncCache_get_evaluation(BddEncCache_ptr self, node_ptr expr)
{
  AddArray_ptr res;
  BDD_ENC_CACHE_CHECK_INSTANCE(self);
  res = ADD_ARRAY(find_assoc(self->eval_hash, expr));
  /* create a duplicate, if it is possible */
  if (ADD_ARRAY(NULL) == res || BDD_ENC_EVALUATING == res) return res;
  return AddArray_duplicate(res);
}

void BddEncCache_clean_evaluation_about(BddEncCache_ptr self,
                                        NodeList_ptr symbs)
{
  NuSMVEnv_ptr env = EnvObject_env(ENV_OBJECT(self->symb_table));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  node_ptr expr;
  assoc_iter iter;
  Set_t expr_to_be_removed = Set_MakeEmpty();

  if (opt_verbose_level_gt(opts, 3)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    Logger_nlog(logger, wffprint,
                "BddEncCache %x: removing evaluation of symbols (%d):\n",
                self, NodeList_get_length(symbs));
    NodeList_print_nodes(symbs, wffprint, Logger_get_stream(logger));
    Logger_log(logger, "\n");
  }

  ASSOC_FOREACH(self->eval_hash, iter, &expr, NULL) {
    /* Here we also need DEFINEs, since they are also part of the
       evaluation cache. Not removing them at this point may result in
       an undefined symbol later (e.g. next call of this
       function). See issue 3133, whose fix is adding defines to this
       call. */
    Set_t deps = Formula_GetDependenciesByType(self->symb_table, expr, Nil,
                                               VFT_CNIFD | VFT_CONSTANTS,
                                               false);
    if (Set_IsEmpty(deps)) {
      if (NodeList_belongs_to(symbs, expr))
        expr_to_be_removed = Set_AddMember(expr_to_be_removed, expr);
      else if (CONTEXT == node_get_type(expr) && Nil == car(expr) &&
               NodeList_belongs_to(symbs, cdr(expr))) {
        expr_to_be_removed = Set_AddMember(expr_to_be_removed, expr);
      }
    }
    else {
      ListIter_ptr var_iter;

      NODE_LIST_FOREACH(symbs, var_iter) {
        node_ptr name = NodeList_get_elem_at(symbs, var_iter);
        if (Set_IsMember(deps, (Set_Element_t) name)) {
          expr_to_be_removed = Set_AddMember(expr_to_be_removed, expr);
          break;
        }
      }
    }

    Set_ReleaseSet(deps);
  }

  {
    Set_Iterator_t set_iter;
    SET_FOREACH(expr_to_be_removed, set_iter) {
      expr = Set_GetMember(expr_to_be_removed, set_iter);
      BddEncCache_remove_evaluation(self, expr);
    }
  }

  if (opt_verbose_level_gt(opts, 3)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    Logger_nlog(logger, wffprint,
                "BddEncCache %x: done removal of evaluation of symbols (%d)\n",
                self, NodeList_get_length(symbs));
    NodeList_print_nodes(symbs, wffprint, Logger_get_stream(logger));
    Logger_log(logger, "\n");
  }
}

void BddEncCache_clean_evaluation(BddEncCache_ptr self)
{
  st_foreach(self->eval_hash, &hash_free_add_array, (char*) self->dd);
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Private initializer

  Private initializer, called by the constructor

  \sa bdd_enc_cache_deinit
*/
static void bdd_enc_cache_init(BddEncCache_ptr self,
                               SymbTable_ptr symb_table, DDMgr_ptr dd)
{
  self->symb_table = symb_table;
  self->dd = dd;

  self->constant_hash = new_assoc();
  nusmv_assert(self->constant_hash != (hash_ptr) NULL);

  self->vars_hash = new_assoc();
  nusmv_assert(self->vars_hash != (hash_ptr) NULL);

  self->eval_hash = new_assoc();
  nusmv_assert(self->eval_hash != (hash_ptr) NULL);
}

/*!
  \brief Private deinitializer

  Private deinitializer, called by the destructor

  \sa bdd_enc_cache_init
*/
static void bdd_enc_cache_deinit(BddEncCache_ptr self)
{
  st_foreach(self->constant_hash, &hash_free_add_counted,
             (char*) self->dd);
  free_assoc(self->constant_hash);

  st_foreach(self->vars_hash, &hash_free_add, (char*) self->dd);
  free_assoc(self->vars_hash);

  st_foreach(self->eval_hash, &hash_free_add_array, (char*) self->dd);
  free_assoc(self->eval_hash);
}

/*!
  \brief Private micro function used when destroying caches of
  adds

  Called when pushing the status, and during
  deinitialization
*/
static assoc_retval
hash_free_add(char* key, char* data, char* arg)
{
  if ((data != (char*) NULL)){
    add_free((DDMgr_ptr ) arg, (add_ptr) data);
  }
  return ASSOC_DELETE;
}

/*!
  \brief Private micro function used when destroying caches of
  adds

  Called when pushing the status, and during
  deinitialization
*/
static assoc_retval
hash_free_add_array(char* key, char* data, char* arg)
{
  if ((data != (char*) NULL) && (ADD_ARRAY(data) != BDD_ENC_EVALUATING)) {
    AddArray_destroy((DDMgr_ptr ) arg, ADD_ARRAY(data));
  }
  return ASSOC_DELETE;
}

/*!
  \brief Private micro function used when destroying caches of
  adds

  Called when pushing the status, and during
  deinitialization. The kind of nodes that must be removed here is
  CONS(integer, add). Of course it is the add that must be freed.
*/
static assoc_retval
hash_free_add_counted(char* key, char* data, char* arg)
{
  DDMgr_ptr dd = DD_MGR(arg);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr cons = (node_ptr) data;
  if ((cons != (node_ptr)NULL)) {
    nusmv_assert(node_get_type(cons) == CONS);
    add_free(dd, (add_ptr) cdr(cons));
    free_node(nodemgr, cons);
  }

  return ASSOC_DELETE;
}
