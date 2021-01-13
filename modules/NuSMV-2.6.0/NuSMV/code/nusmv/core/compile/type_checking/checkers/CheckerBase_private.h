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
  \brief Private and protected interface of class 'CheckerBase'

  This file can be included only by derived and friend classes

*/



#ifndef __NUSMV_CORE_COMPILE_TYPE_CHECKING_CHECKERS_CHECKER_BASE_PRIVATE_H__
#define __NUSMV_CORE_COMPILE_TYPE_CHECKING_CHECKERS_CHECKER_BASE_PRIVATE_H__


#include "nusmv/core/compile/type_checking/checkers/CheckerBase.h"
#include "nusmv/core/compile/type_checking/checkers/checkersInt.h"

#include "nusmv/core/node/NodeWalker_private.h"
#include "nusmv/core/compile/type_checking/TypeChecker.h"
#include "nusmv/core/compile/type_checking/TypeChecker_private.h"

#include "nusmv/core/utils/utils.h"


/*!
  \brief Short way of calling type_checking_check_expression

  Use this macro to recursively recall
  type_checking_check_expression. Must be used from within a method. This
  can be used when expressions that are node_ptr
*/

/* WARNING [MD] This macro should take self as parameter */
#define _THROW(exp, ctx)                                                  \
   (NodeWalker_can_handle(NODE_WALKER(self), exp) ?                       \
    CHECKER_BASE(self)->check_expr(self, exp, ctx) :                      \
   type_checker_check_expression(TYPE_CHECKER(NODE_WALKER(self)->master), \
                                 exp, ctx))

/*!
  \brief CheckerBase class definition derived from
               class node.NodeWalker

  

  \sa Base class Object
*/

typedef struct CheckerBase_TAG
{
  /* this MUST stay on the top */
  INHERITS_FROM(NodeWalker);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  /* -------------------------------------------------- */
  /*                  Virtual methods                   */
  /* -------------------------------------------------- */
  TypeCheckingViolationHandler_ptr viol_handler;

  SymbType_ptr (*check_expr)(CheckerBase_ptr self, node_ptr exp, node_ptr ctx);

} CheckerBase;



/* ---------------------------------------------------------------------- */
/* Private methods to be used by derivated and friend classes only         */
/* ---------------------------------------------------------------------- */

/*!
  \methodof CheckerBase
  \brief Creates and initializes a checker.
  To be usable, the checker will have to be registered to a TypeChecker.

  To each checker is associated a partition of
  consecutive indices over the symbols set. The lowest index of the
  partition is given through the parameter low, while num is the
  partition size. Name is used to easily identify checker instances.

  This method is private, as this class is virtual

  \sa NodeWalker_destroy
*/
CheckerBase_ptr CheckerBase_create(const NuSMVEnv_ptr env,
                                          const char* name,
                                          int low, size_t num);

/*!
  \methodof CheckerBase
  \brief The CheckerBase class private initializer

  The CheckerBase class private initializer

  \sa CheckerBase_create
*/
void
checker_base_init(CheckerBase_ptr self, const NuSMVEnv_ptr env,
                  const char* name, int low, size_t num);

/*!
  \methodof CheckerBase
  \brief The CheckerBase class private deinitializer

  The CheckerBase class private deinitializer

  \sa CheckerBase_destroy
*/
void checker_base_deinit(CheckerBase_ptr self);

/*!
  \methodof CheckerBase
  \brief Virtual protected method for violation handling

  This virtual method is called by checkers to handle
  their violations. Tipically this method is not called directly, but
  throw the macro _VIOLATION that casts its arguments and improves
  readability (at least as main tentative idea)
*/
VIRTUAL boolean
checker_base_manage_violation(CheckerBase_ptr self,
                              TypeSystemViolation violation,
                              node_ptr expression);

/*!
  \methodof CheckerBase
  \brief \todo Missing synopsis

  \todo Missing description
*/
void
checker_base_print_type(CheckerBase_ptr self, FILE* output_stream,
                        node_ptr expression, node_ptr context);


#endif /* __NUSMV_CORE_COMPILE_TYPE_CHECKING_CHECKERS_CHECKER_BASE_PRIVATE_H__ */
