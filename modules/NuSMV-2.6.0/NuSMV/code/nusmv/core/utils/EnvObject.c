/* ---------------------------------------------------------------------------


  This file is part of the ``utils'' package of NuSMV version 2.
  Copyright (C) 2011 by FBK-irst.

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
  \author Alessandro Mariotti
  \brief Implementation of class 'EnvObject'

  \todo: Missing description

*/


#include "nusmv/core/utils/EnvObject.h"
#include "nusmv/core/utils/EnvObject_private.h"
#include "nusmv/core/utils/utils.h"


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'EnvObject_private.h' for class 'EnvObject' definition. */

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

static void env_object_finalize(Object_ptr object, void* dummy);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

EnvObject_ptr EnvObject_create(const NuSMVEnv_ptr env)
{
  EnvObject_ptr self = ALLOC(EnvObject, 1);
  ENV_OBJECT_CHECK_INSTANCE(self);

  env_object_init(self, env);
  return self;
}

void EnvObject_destroy(EnvObject_ptr self)
{
  ENV_OBJECT_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

NuSMVEnv_ptr EnvObject_get_environment(const EnvObject_ptr self)
{
  ENV_OBJECT_CHECK_INSTANCE(self);

  return self->environment;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void env_object_init(EnvObject_ptr self, NuSMVEnv_ptr env)
{
  /* base class initialization */
  object_init(OBJECT(self));

  /* members initialization */

  self->environment = env;

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = env_object_finalize;

  /* for example, to override a base class' virtual method: */
  /*OVERRIDE(Object, virtual_method) = env_object_virtual_method;*/
}

void env_object_copy_aux(const EnvObject_ptr self, EnvObject_ptr copy)
{
  object_copy_aux(OBJECT(self), OBJECT(copy));

  copy->environment = self->environment;
}

void env_object_deinit(EnvObject_ptr self)
{
  /* members deinitialization */


  /* base class deinitialization */
  object_deinit(OBJECT(self));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The EnvObject class virtual finalizer

  Called by the class destructor
*/
static void env_object_finalize(Object_ptr object, void* dummy)
{
  EnvObject_ptr self = ENV_OBJECT(object);

  env_object_deinit(self);
  FREE(self);
}



/**AutomaticEnd***************************************************************/
