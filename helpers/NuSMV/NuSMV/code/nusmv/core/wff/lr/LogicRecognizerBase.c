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
  \brief Implementation of class 'LogicRecognizerBase'

  \todo: Missing description

*/


#include "nusmv/core/wff/lr/LogicRecognizerBase.h"
#include "nusmv/core/wff/lr/LogicRecognizerBase_private.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/portability.h"  /* for __func__ */

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'LogicRecognizerBase_private.h' for class 'LogicRecognizerBase' definition. */

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

static void logic_recognizer_base_finalize(Object_ptr object, void* dummy);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

LogicType LogicRecognizerBase_recognize(LogicRecognizerBase_ptr self,
                                              node_ptr wff,
                                              node_ptr context)
{
  LogicType retval = EXP_NONE;

  LOGIC_RECOGNIZER_BASE_CHECK_INSTANCE(self);

  retval = self->recognize(self, wff, context);

  return retval;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void logic_recognizer_base_init(LogicRecognizerBase_ptr self,
                                const NuSMVEnv_ptr env,
                                const char* name,
                                int low,
                                size_t num)
{
  /* base class initialization */
  node_walker_init(NODE_WALKER(self), env, name, low, num, false);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = logic_recognizer_base_finalize;
  OVERRIDE(LogicRecognizerBase, recognize) = logic_recognizer_base_recognize;
}

void logic_recognizer_base_deinit(LogicRecognizerBase_ptr self)
{
  /* members deinitialization */


  /* base class deinitialization */
  node_walker_deinit(NODE_WALKER(self));
}

LogicType logic_recognizer_base_recognize(LogicRecognizerBase_ptr self,
                                                node_ptr wff,
                                                node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  ErrorMgr_internal_error(errmgr, "%s: Pure virtual method\n", __func__);
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The LogicRecognizerBase class virtual finalizer

  Called by the class destructor
*/
static void logic_recognizer_base_finalize(Object_ptr object, void* dummy)
{
  LogicRecognizerBase_ptr self = LOGIC_RECOGNIZER_BASE(object);

  logic_recognizer_base_deinit(self);
  FREE(self);
}



/**AutomaticEnd***************************************************************/

