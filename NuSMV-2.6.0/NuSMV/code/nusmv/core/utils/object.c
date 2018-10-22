/* ---------------------------------------------------------------------------


  This file is part of the ``utils'' package of NuSMV version 2.
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
  \brief Basic services for object-oriented design

  Class Object is a simple pure base class, to be used as base
  for a class hierarchy

*/



#include "nusmv/core/utils/object.h"
#include "nusmv/core/utils/object_private.h"
#include "nusmv/core/utils/error.h"
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void object_finalize(Object_ptr self, void* arg);
static Object_ptr object_copy(const Object_ptr self);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

VIRTUAL void Object_destroy(Object_ptr self, void* arg)
{
  OBJECT_CHECK_INSTANCE(self);
  self->finalize(self, arg);
}

VIRTUAL Object_ptr Object_copy(const Object_ptr self)
{
  return self->copy(self);
}


/*---------------------------------------------------------------------------*/
/* Definition of private functions                                           */
/*---------------------------------------------------------------------------*/

void object_init(Object_ptr self)
{
  OVERRIDE(Object, finalize) = object_finalize;
  OVERRIDE(Object, copy)     = object_copy;
}

void object_deinit(Object_ptr self)
{
  OVERRIDE(Object, finalize) = NULL;
  OVERRIDE(Object, copy)     = NULL;
}

void object_copy_aux(const Object_ptr self, Object_ptr copy)
{
  copy->finalize = self->finalize;
  copy->copy     = self->copy;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Private copy constructor

  In order to provide copy feature, any derived class must
  implement two copy private functions: copy and copy_aux functions, to split
  copy creation and actual copy operations. copy creates an instance and
  passes it to copy_aux. In any derived class, the base class' copy_aux
  method must be called by the copy_aux method before any other operation.
  If the derived class does not override the object's copy constructor,
  and the user tries to copy the derived class instance by calling the
  Object_copy method, then an assertion is fired since Object is a class
  that cannot be instantiated. 

  \sa object_copy_aux
*/
static Object_ptr object_copy(const Object_ptr self)
{
  error_unreachable_code(); /* this is a virtual class, no real instances
                          are allowed */
  return OBJECT(NULL);
}

/*!
  \brief Private pure destructor

  Since the Object class cannot be really instantiated,
  the virtual destructor must be overrided by derived classes. If the derived
  class does not override the finalizer, then an assertion is fired when
  the virtual destroyer Object_destroy is called.
*/
static void object_finalize(Object_ptr self, void* arg)
{
  object_deinit(self);

  error_unreachable_code(); /* this is a virtual class, no real instances
                               are allowed */
}
