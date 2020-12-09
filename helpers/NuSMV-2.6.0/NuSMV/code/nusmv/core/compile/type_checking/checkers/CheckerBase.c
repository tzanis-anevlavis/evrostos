/* ---------------------------------------------------------------------------


  This file is part of the ``compile.type_checking.checkers'' package
  of NuSMV version 2. Copyright (C) 2006 by FBK-irst.

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
  \brief Implementaion of class 'CheckerBase'

  \todo: Missing description

*/


#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/compile/type_checking/checkers/CheckerBase.h"
#include "nusmv/core/compile/type_checking/checkers/CheckerBase_private.h"
#include "nusmv/core/compile/type_checking/TypeChecker_private.h"

#include "nusmv/core/utils/utils.h"
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
/* See 'CheckerBase_private.h' for class 'CheckerBase' definition. */

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

static void checker_base_finalize(Object_ptr object, void* dummy);

static SymbType_ptr
checker_base_check_expr(CheckerBase_ptr self,
                        node_ptr expression, node_ptr context);

static boolean
checker_base_viol_handler(CheckerBase_ptr checker,
                          TypeSystemViolation violation,
                          node_ptr expression);



/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

CheckerBase_ptr
CheckerBase_create(const NuSMVEnv_ptr env, const char* name, int low, size_t num)
{
  CheckerBase_ptr self = ALLOC(CheckerBase, 1);
  CHECKER_BASE_CHECK_INSTANCE(self);

  checker_base_init(self, env, name, low, num);
  return self;
}

VIRTUAL SymbType_ptr
CheckerBase_check_expr(CheckerBase_ptr self, node_ptr exp, node_ptr ctx)
{
  CHECKER_BASE_CHECK_INSTANCE(self);

  return self->check_expr(self, exp, ctx);
}



/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void checker_base_init(CheckerBase_ptr self, const NuSMVEnv_ptr env,
                       const char* name, int low, size_t num)
{
  /* base class initialization */
  node_walker_init(NODE_WALKER(self), env, name, low, num,
                   false /* not handle NULL case */);

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = checker_base_finalize;
  OVERRIDE(CheckerBase, check_expr) = checker_base_check_expr;
  OVERRIDE(CheckerBase, viol_handler) = checker_base_viol_handler;
}

void checker_base_deinit(CheckerBase_ptr self)
{
  /* members deinitialization */

  /* base class initialization */
  node_walker_deinit(NODE_WALKER(self));
}

VIRTUAL boolean checker_base_manage_violation(CheckerBase_ptr self,
                                              TypeSystemViolation violation,
                                              node_ptr expression)
{
  return self->viol_handler(self, violation, expression);
}




/**Static function*************************************************************

  Synopsis           [Just prints out the type of an expression to
  output_stream]

  Description        [
  The expr must be a correct expression which has been already type checked
  by 'checker'.
  The last parameter 'context' is the context where the expression
  has been found.
  The function is used in printing of an error messages only.]

  SideEffects        []

  SeeAlso            []
******************************************************************************/
void checker_base_print_type(CheckerBase_ptr self, FILE* output_stream,
                             node_ptr expression, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  SymbType_ptr type =
    TypeChecker_get_expression_type(TYPE_CHECKER(NODE_WALKER(self)->master),
                                    expression, context);

  /* this expression must have been checked by this type checker. and
     it must not be 0 since this is an sub-expression of a problematic
     expression */
  if (type == SYMB_TYPE(NULL)) {
    ErrorMgr_internal_error(errmgr, "check_base_print_type: the type checker was not able \n" \
                   "to retrieve the given expression type");
  }

  SymbType_print(type, wffprint, output_stream);
  return;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The CheckerBase class virtual finalizer

  Called by the class destructor
*/
static void checker_base_finalize(Object_ptr object, void* dummy)
{
  CheckerBase_ptr self = CHECKER_BASE(object);

  checker_base_deinit(self);
  FREE(self);
}

/*!
  \brief 

  
*/
static SymbType_ptr
checker_base_check_expr(CheckerBase_ptr self,
                        node_ptr expression, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  ErrorMgr_internal_error(errmgr, "CheckerBase: Pure virtual method check_expression " \
                 "not implemented\n");
  return 0;
}

/*!
  \brief 

  
*/
static boolean
checker_base_viol_handler(CheckerBase_ptr checker,
                          TypeSystemViolation violation, node_ptr expression)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(checker));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  ErrorMgr_internal_error(errmgr, "CheckerBase: Pure virtual method viol_handler " \
                 "not implemented\n");
  return 0;
}


/**AutomaticEnd***************************************************************/

