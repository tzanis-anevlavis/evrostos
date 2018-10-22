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
  \brief Private and protected interface of class 'CheckerStatement'

  This file can be included only by derived and friend classes

*/



#ifndef __NUSMV_CORE_COMPILE_TYPE_CHECKING_CHECKERS_CHECKER_STATEMENT_PRIVATE_H__
#define __NUSMV_CORE_COMPILE_TYPE_CHECKING_CHECKERS_CHECKER_STATEMENT_PRIVATE_H__


#include "nusmv/core/compile/type_checking/checkers/CheckerStatement.h"

#include "nusmv/core/compile/type_checking/checkers/CheckerCore.h"
#include "nusmv/core/compile/type_checking/checkers/CheckerCore_private.h"

#include "nusmv/core/utils/utils.h"


/*!
  \brief CheckerStatement class definition derived from
               class CheckerCore

  

  \sa Base class CheckerCore
*/

typedef struct CheckerStatement_TAG
{
  /* this MUST stay on the top */
  INHERITS_FROM(CheckerCore);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  boolean inside_attime; /* to check ATTIME is not nested */

  /* -------------------------------------------------- */
  /*                  Virtual methods                   */
  /* -------------------------------------------------- */

} CheckerStatement;



/* ---------------------------------------------------------------------- */
/* Private methods to be used by derivated and friend classes only         */
/* ---------------------------------------------------------------------- */

/*!
  \methodof CheckerStatement
  \brief The CheckerStatement class private initializer

  The CheckerStatement class private initializer

  \sa CheckerStatement_create
*/
void checker_statement_init(CheckerStatement_ptr self,
                                   const NuSMVEnv_ptr env);

/*!
  \methodof CheckerStatement
  \brief The CheckerStatement class private deinitializer

  The CheckerStatement class private deinitializer

  \sa CheckerStatement_destroy
*/
void checker_statement_deinit(CheckerStatement_ptr self);



#endif /* __NUSMV_CORE_COMPILE_TYPE_CHECKING_CHECKERS_CHECKER_STATEMENT_PRIVATE_H__ */
