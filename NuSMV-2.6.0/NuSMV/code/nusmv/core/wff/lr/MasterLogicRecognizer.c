/* ---------------------------------------------------------------------------


  This file is part of the ``wff.lr'' package of NuSMV version 2.
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
  \author Michele Dorigatti
  \brief Implementation of class 'MasterLogicRecognizer'

  \todo: Missing description

*/


#include "nusmv/core/wff/lr/MasterLogicRecognizer.h"
#include "nusmv/core/wff/lr/MasterLogicRecognizer_private.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/wff/lr/LogicRecognizerBase.h"
#include "nusmv/core/wff/lr/LogicRecognizerCore.h"
#include "nusmv/core/utils/StreamMgr.h"

#ifndef NDEBUG
#  include "nusmv/core/compile/type_checking/TypeChecker.h"
#  include "nusmv/core/compile/symb_table/SymbTable.h"
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
/* See 'MasterLogicRecognizer_private.h' for class 'MasterLogicRecognizer' definition. */

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

static void master_logic_recognizer_finalize(Object_ptr object, void* dummy);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

MasterLogicRecognizer_ptr MasterLogicRecognizer_create(NuSMVEnv_ptr env)
{
  MasterLogicRecognizer_ptr self = ALLOC(MasterLogicRecognizer, 1);
  MASTER_LOGIC_RECOGNIZER_CHECK_INSTANCE(self);

  master_logic_recognizer_init(self, env);
  return self;
}

MasterLogicRecognizer_ptr MasterLogicRecognizer_create_with_default_recognizers(NuSMVEnv_ptr env)
{
  MasterLogicRecognizer_ptr self = ALLOC(MasterLogicRecognizer, 1);
  NodeWalker_ptr walker;
  boolean has_been_registered = false;
  MASTER_LOGIC_RECOGNIZER_CHECK_INSTANCE(self);

  master_logic_recognizer_init(self, env);

  walker = NODE_WALKER(LogicRecognizerCore_create(env));
  NODE_WALKER_CHECK_INSTANCE(walker);
  has_been_registered = MasterNodeWalker_register_walker(MASTER_NODE_WALKER(self),
                                                         walker);
  nusmv_assert(has_been_registered);

  return self;
}

void MasterLogicRecognizer_destroy(MasterLogicRecognizer_ptr self)
{
  MASTER_LOGIC_RECOGNIZER_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

/*!
  \brief Returns the logic to which expression belongs

  Given an expression and its context detects the logic or
  the expression type to which it belongs.

  \sa LogicType
*/

LogicType MasterLogicRecognizer_recognize(MasterLogicRecognizer_ptr self,
                                                node_ptr expression,
                                                node_ptr context)
{
  LogicType retval = EXP_NONE;

  MASTER_LOGIC_RECOGNIZER_CHECK_INSTANCE(self);

#ifndef NDEBUG
  {
    NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
    SymbTable_ptr const symb_table =
      SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
     TypeChecker_ptr type_checker = SymbTable_get_type_checker(symb_table);

     nusmv_assert(TypeChecker_is_expression_wellformed(type_checker, expression,
                                                       context));
  }
#endif

  retval = master_logic_recognizer_lookup(self, expression, context);

  if (EXP_NONE == retval) {
    retval = master_logic_recognizer_recognize(self, expression, context);
  }

  nusmv_assert(EXP_NONE != retval);

  return retval;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void master_logic_recognizer_init(MasterLogicRecognizer_ptr self,
                                  NuSMVEnv_ptr env)
{
  /* base class initialization */
  master_node_walker_init(MASTER_NODE_WALKER(self), env);

  /* members initialization */
  self->expr2logic = new_assoc();

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = master_logic_recognizer_finalize;

  /* for example, to override a base class' virtual method: */
  /*OVERRIDE(MasterNodeWalker, virtual_method) = master_logic_recognizer_virtual_method;*/
}

void master_logic_recognizer_deinit(MasterLogicRecognizer_ptr self)
{
  /* members deinitialization */
  free_assoc(self->expr2logic); self->expr2logic = NULL;

  /* base class deinitialization */
  master_node_walker_deinit(MASTER_NODE_WALKER(self));
}

LogicType master_logic_recognizer_lookup(MasterLogicRecognizer_ptr self,
                                               node_ptr expression,
                                               node_ptr context)
{
  LogicType retval = EXP_NONE;
  node_ptr key = NULL;

  if (Nil != context) {
    NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
    NodeMgr_ptr const nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    key = find_node(nodemgr, CONTEXT, context, expression);
  }
  else key = expression;

  retval = (LogicType)NODE_TO_INT(find_assoc(self->expr2logic, key));

  return retval;
}

void master_logic_recognizer_insert(MasterLogicRecognizer_ptr self,
                                    node_ptr expression,
                                    node_ptr context,
                                    LogicType logic)
{
  node_ptr key = NULL;

  if (Nil != context) {
    NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
    NodeMgr_ptr const nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    key = find_node(nodemgr, CONTEXT, context, expression);
  }
  else key = expression;

  insert_assoc(self->expr2logic, key, NODE_PTR(logic));
}

LogicType master_logic_recognizer_recognize(MasterLogicRecognizer_ptr self,
                                                  node_ptr expression,
                                                  node_ptr context)
{
  ListIter_ptr iter;
  iter = NodeList_get_first_iter(MASTER_NODE_WALKER(self)->walkers);
  while (!ListIter_is_end(iter)) {
    LogicRecognizerBase_ptr recognizer =
      LOGIC_RECOGNIZER_BASE(NodeList_get_elem_at(MASTER_NODE_WALKER(self)->walkers,
                                                 iter));

    if (NodeWalker_can_handle(NODE_WALKER(recognizer), expression)) {
      return LogicRecognizerBase_recognize(recognizer, expression, context);
    }

    iter = ListIter_get_next(iter);
  }

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const StreamMgr_ptr streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
    const MasterPrinter_ptr sexpprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_SEXP_PRINTER));

    StreamMgr_print_error(streams,  "Warning: no compatible logic recognizer found for expression:\n");
    StreamMgr_nprint_error(streams, wffprint, "%N", expression);
    StreamMgr_print_error(streams,  "\n");
    StreamMgr_nprint_error(streams, sexpprint, "%N", expression);
  }

  return EXP_ERROR;
}

LogicType master_logic_recognizer_merge(MasterLogicRecognizer_ptr master,
                                              LogicType left,
                                              LogicType right)
{
  LogicType retval = EXP_NONE;

  UNUSED_PARAM(master);

  LOGIC_RECOGNIZED_ASSERT_HAS_VALUE(left);
  LOGIC_RECOGNIZED_ASSERT_HAS_VALUE(right);

  if (left == right) {
    retval = left;
  }
  else {
    LogicType higher = EXP_NONE;
    LogicType lower = EXP_NONE;

    if (left > right) {
      higher = left;
      lower = right;
    }
    else {
      higher = right;
      lower = left;
    }
    
    /* impossible cases */
    nusmv_assert(! ((EXP_CTL == higher) && (EXP_LTL == lower)));

    retval = higher;
  } /* end of top level else */

  return retval;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The MasterLogicRecognizer class virtual finalizer

  Called by the class destructor
*/
static void master_logic_recognizer_finalize(Object_ptr object, void* dummy)
{
  MasterLogicRecognizer_ptr self = MASTER_LOGIC_RECOGNIZER(object);

  master_logic_recognizer_deinit(self);
  FREE(self);
}



/**AutomaticEnd***************************************************************/

