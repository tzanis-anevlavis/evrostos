/* ---------------------------------------------------------------------------


  This file is part of the ``compile.type_checking.checkers'' package of NuSMV version 2.
  Copyright (C) 2006 by FBK-irst.

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
  \brief Private and protected interface of class 'CheckerCore'

  This file can be included only by derived and friend classes

*/



#ifndef __NUSMV_CORE_COMPILE_TYPE_CHECKING_CHECKERS_CHECKER_CORE_PRIVATE_H__
#define __NUSMV_CORE_COMPILE_TYPE_CHECKING_CHECKERS_CHECKER_CORE_PRIVATE_H__

#include "nusmv/core/compile/type_checking/checkers/CheckerCore.h"

#include "nusmv/core/compile/type_checking/checkers/CheckerBase.h"
#include "nusmv/core/compile/type_checking/checkers/CheckerBase_private.h"

#include "nusmv/core/utils/utils.h"



/*!
  \brief Short way of calling tc_set_expression_type

  Use this macro to set an expression type within the
  master type checker. Must be used from within a method. This
  can be used when expressions that are node_ptr
*/

#define _SET_TYPE(expr, type) \
   tc_set_expression_type(TYPE_CHECKER(NODE_WALKER(self)->master), expr, type)


/*!
  \brief Short way of calling tc_lookup_expr_type

  Use this macro to get an expression type from the
  master type checker. Must be used from within a method. This
  can be used when expressions that are node_ptr
*/

#define _GET_TYPE(expr) \
   tc_lookup_expr_type(TYPE_CHECKER(NODE_WALKER(self)->master), expr)


/*!
  \brief Short way of calling checker_base_manage_violation

  Use this macro to manage a violation at the master
  type checker level. Must be used from within a method. This
  can be used when expressions that are node_ptr
*/

#define _VIOLATION(viol_id, expr) \
   checker_base_manage_violation(CHECKER_BASE(self), viol_id, expr)



/*!
  \brief Short way of calling type_checker_print_error_message

  Use this macro to recursively call
  type_checking_check_expression into violation handlers. This
  can be used when expressions that are node_ptr
*/

#define _PRINT_ERROR_MSG(exp, is_error)                                      \
   type_checker_print_error_message(TYPE_CHECKER(NODE_WALKER(self)->master), \
                                 exp, is_error)




/*!
  \brief CheckerCore class definition derived from
               class CheckerBase

  

  \sa Base class CheckerBase
*/

typedef struct CheckerCore_TAG
{
  /* this MUST stay on the top */
  INHERITS_FROM(CheckerBase);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */


  /* -------------------------------------------------- */
  /*                  Virtual methods                   */
  /* -------------------------------------------------- */

} CheckerCore;



/* ---------------------------------------------------------------------- */
/* Private methods to be used by derivated and friend classes only         */
/* ---------------------------------------------------------------------- */

/*!
  \methodof CheckerCore
  \brief The CheckerCore class private initializer

  The CheckerCore class private initializer

  \sa CheckerCore_create
*/
void
checker_core_init(CheckerCore_ptr self, const NuSMVEnv_ptr env,
                  const char* name, int low, size_t num);

/*!
  \methodof CheckerCore
  \brief The CheckerCore class private deinitializer

  The CheckerCore class private deinitializer
*/
void checker_core_deinit(CheckerCore_ptr self);



#endif /* __NUSMV_CORE_COMPILE_TYPE_CHECKING_CHECKERS_CHECKER_CORE_PRIVATE_H__ */
