/* ---------------------------------------------------------------------------


  This file is part of the ``compile.flattening'' package of NuSMV version 2.
  Copyright (C) 2013 by FBK-irst.

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
  \author Sergio Mover
  \brief Implementation of class 'MasterCompileFlattener'

  Implement an extensible for expressions.

  Additional types of expressions can be handled creating new flattenrs.
  

*/


#include "nusmv/core/compile/flattening/MasterCompileFlattener.h"
#include "nusmv/core/compile/flattening/MasterCompileFlattener_private.h"
#include "nusmv/core/compile/flattening/FlattenerBase.h"

#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/ErrorMgr.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'MasterCompileFlattener_private.h' for class 'MasterCompileFlattener' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief Name used to lookup the hash used to memoize expanded defines.

  
*/
#define COMPILE_FLATTEN_DEF_HASH "compile_flatten_def_hash"

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void master_compile_flattener_finalize(Object_ptr object, void* dummy);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

MasterCompileFlattener_ptr
MasterCompileFlattener_create(const NuSMVEnv_ptr env)
{
  MasterCompileFlattener_ptr self = ALLOC(MasterCompileFlattener, 1);
  MASTER_COMPILE_FLATTENER_CHECK_INSTANCE(self);

  master_compile_flattener_init(self, env);
  return self;
}

void MasterCompileFlattener_destroy(MasterCompileFlattener_ptr self)
{
  MASTER_COMPILE_FLATTENER_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

node_ptr MasterCompileFlattener_flatten(MasterCompileFlattener_ptr self,
                                        SymbTable_ptr symb_table,
                                        node_ptr sexp,
                                        node_ptr context)
{
  hash_ptr def_hash;

  MASTER_COMPILE_FLATTENER_CHECK_INSTANCE(self);

  def_hash = master_compile_flattener_get_def_hash(self, symb_table);
  nusmv_assert((hash_ptr) NULL != def_hash);

  return master_compile_flattener_flatten(self, symb_table,
                                          def_hash,
                                          sexp, context,
                                          Flattener_Get_Def_Mode);
}

node_ptr
MasterCompileFlattener_flatten_expand_define(MasterCompileFlattener_ptr self,
                                             SymbTable_ptr symb_table,
                                             node_ptr sexp,
                                             node_ptr context)
{
  hash_ptr def_hash;

  MASTER_COMPILE_FLATTENER_CHECK_INSTANCE(self);

  def_hash = master_compile_flattener_get_def_hash(self, symb_table);
  nusmv_assert((hash_ptr) NULL != def_hash);

  return master_compile_flattener_flatten(self, symb_table,
                                          def_hash,
                                          sexp, context,
                                          Flattener_Expand_Def_Mode);
}

node_ptr
MasterCompileFlattener_get_definition(MasterCompileFlattener_ptr self,
                                      SymbTable_ptr symb_table,
                                      node_ptr name,
                                      MasterCompileFlattener_def_mode mode)
{
  hash_ptr def_hash;

  MASTER_COMPILE_FLATTENER_CHECK_INSTANCE(self);

  def_hash = master_compile_flattener_get_def_hash(self, symb_table);
  nusmv_assert((hash_ptr) NULL != def_hash);

  return master_compile_flattener_get_definition(self,
                                                 symb_table, def_hash,
                                                 name, mode);
}

void MasterCompileFlattener_remove_define_info(MasterCompileFlattener_ptr self,
                                               SymbTable_ptr symb_table,
                                               node_ptr name)
{
  hash_ptr def_hash;
  MASTER_COMPILE_FLATTENER_CHECK_INSTANCE(self);

  def_hash = master_compile_flattener_get_def_hash(self, symb_table);
  nusmv_assert((hash_ptr) NULL != def_hash);

  if (NODE_PTR(NULL) != find_assoc(def_hash, name)) {
    insert_assoc(def_hash, name, NODE_PTR(NULL));
  }
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

node_ptr
master_compile_flattener_flatten(MasterCompileFlattener_ptr self,
                                 SymbTable_ptr symb_table,
                                 hash_ptr def_hash,
                                 node_ptr sexp,
                                 node_ptr context,
                                 MasterCompileFlattener_def_mode def_mode)
{
  ListIter_ptr iter;
  iter = NodeList_get_first_iter(MASTER_NODE_WALKER(self)->walkers);
  while (! ListIter_is_end(iter)) {
    FlattenerBase_ptr  pr =
      FLATTENER_BASE(NodeList_get_elem_at(MASTER_NODE_WALKER(self)->walkers,
                                          iter));

    if (NodeWalker_can_handle(NODE_WALKER(pr), sexp)) {
      return FlattenerBase_flatten(pr, symb_table, def_hash, sexp,
                                   context, def_mode);
    }

    iter = ListIter_get_next(iter);
  }

  return Nil;
}

node_ptr master_compile_flattener_get_definition(
                                  MasterCompileFlattener_ptr self,
                                  SymbTable_ptr symb_table,
                                  hash_ptr def_hash,
                                  node_ptr atom,
                                  MasterCompileFlattener_def_mode def_mode)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr = ERROR_MGR(NuSMVEnv_get_value(env,
                                                           ENV_ERROR_MANAGER));

  node_ptr result = Nil;

  if (SymbTable_is_symbol_var(symb_table, atom)) result = atom;
  else if (SymbTable_is_symbol_constant(symb_table, atom)) result = atom;
  else if (SymbTable_is_symbol_define(symb_table, atom)) {
    node_ptr exp = find_assoc(def_hash, atom);

    /* Check for circular recursive definitions */
    if (exp == MASTER_COMPILE_FLATTENER_BUILDING_FLAT_BODY) {
      ErrorMgr_error_circular(errmgr, atom);
    }
    if (exp == (node_ptr) NULL) {
      /* The body of a definition is flattened and the flattening is
         saved.  The flattened body is not returned. */
      insert_assoc(def_hash, atom, MASTER_COMPILE_FLATTENER_BUILDING_FLAT_BODY);

      ErrorMgr_io_atom_push(errmgr, atom);
      {
        /*
           We need to force the expansion mode it to be expand here
           since we are attempting to expand the body of defined
           symbols, and we need to to do it recursively. If this is
           not done, then the expansion of further defined symbols
           occurring in the body is not performed.
        */
        exp = MasterCompileFlattener_flatten_expand_define(
                       self,
                       symb_table,
                       SymbTable_get_define_body(symb_table, atom),
                       SymbTable_get_define_context(symb_table, atom));
      }
      ErrorMgr_io_atom_pop(errmgr);
      insert_assoc(def_hash, atom, exp);
    }

    if (Flattener_Expand_Def_Mode == def_mode) result = exp;
    else result = atom;
  }
  else if (SymbTable_is_symbol_array_define(symb_table, atom)) {
    /* ARRAY DEFINE are never expanded. Instead when connected with
       index-subscript (at higher level) it will become a define and
       then expanded */
    result = atom;
  }
  else if (SymbTable_is_symbol_variable_array(symb_table, atom)) {
    /* If this is a symbol-type, return it as it is because it is a
       array or an array */
    result = atom;
  }
  else if (SymbTable_is_symbol_function(symb_table, atom)) {
    /* If this is a symbol-type, return it as it is because it is a
       array or an array */
    result = atom;
  }

  else {
    /* Throw an error */
    ErrorMgr_error_undefined(errmgr, atom);
  }

  return result;
}

hash_ptr master_compile_flattener_get_def_hash(MasterCompileFlattener_ptr self,
                                               SymbTable_ptr symb_table)
{
  return SymbTable_get_handled_hash_ptr(
                      symb_table,
                      COMPILE_FLATTEN_DEF_HASH,
                      (ST_PFICPCP) NULL,
                      (ST_PFICPI) NULL,
                      (ST_PFSR) NULL,
                      (SymbTableTriggerFun) NULL,
                      SymbTable_clear_handled_remove_action_hash,
                      (SymbTableTriggerFun) NULL);
}

void master_compile_flattener_init(MasterCompileFlattener_ptr self,
                                   NuSMVEnv_ptr env)
{
  /* base class initialization */
  master_node_walker_init(MASTER_NODE_WALKER(self), env);

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = master_compile_flattener_finalize;
}

void master_compile_flattener_deinit(MasterCompileFlattener_ptr self)
{
  /* members deinitialization */

  /* base class deinitialization */
  master_node_walker_deinit(MASTER_NODE_WALKER(self));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The MasterCompileFlattener class virtual finalizer

  Called by the class destructor
*/

static void master_compile_flattener_finalize(Object_ptr object, void* dummy)
{
  MasterCompileFlattener_ptr self = MASTER_COMPILE_FLATTENER(object);

  master_compile_flattener_deinit(self);
  FREE(self);
}

/**AutomaticEnd***************************************************************/
