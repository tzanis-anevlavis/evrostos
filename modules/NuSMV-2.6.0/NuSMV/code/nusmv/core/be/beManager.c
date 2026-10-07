/* ---------------------------------------------------------------------------


  This file is part of the ``be'' package of NuSMV version 2.
  Copyright (C) 2000-2001 by FBK-irst and University of Trento.

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
  \brief The generic Boolean Expressions Manager implementation

  This implementation is independent on the low-level structure is
  being used.

*/


#include "nusmv/core/be/be.h"
#include "nusmv/core/be/beManagerInt.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Declarations of internal functions                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/


/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

Be_Manager_ptr Be_Manager_Create(const NuSMVEnv_ptr env,
                                 void* spec_manager,
                                 Be_Spec2Be_fun      spec2be_converter,
                                 Be_Be2Spec_fun      be2spec_converter)
{
  Be_Manager_ptr self = NULL;
  nusmv_assert(spec_manager != NULL);

  self = (Be_Manager_ptr) ALLOC(Be_Manager, 1);
  nusmv_assert(self != NULL);

  self->environment = env;
  self->spec_manager = spec_manager;
  self->support_data = NULL;
  self->spec2be_converter      = spec2be_converter;
  self->be2spec_converter      = be2spec_converter;

  return self;
}

void Be_Manager_Delete(Be_Manager_ptr self)
{
  FREE(self);
}

NuSMVEnv_ptr Be_Manager_GetEnvironment(const Be_Manager_ptr self)
{
  return self->environment;
}

be_ptr Be_Manager_Spec2Be(const Be_Manager_ptr self, void* spec_expr)
{
  return self->spec2be_converter(self, spec_expr);
}

void* Be_Manager_Be2Spec(const Be_Manager_ptr self, be_ptr be)
{
  return self->be2spec_converter(self, be);
}

void* Be_Manager_GetData(const Be_Manager_ptr self)
{
  return self->support_data;
}

void  Be_Manager_SetData(Be_Manager_ptr self, void* data)
{
  self->support_data = data;
}

void* Be_Manager_GetSpecManager(Be_Manager_ptr self)
{
  return self->spec_manager;
}

