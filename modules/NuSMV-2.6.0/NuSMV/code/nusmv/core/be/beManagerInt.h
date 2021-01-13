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
  \brief The private interface of the Be_Manager class

  This interface is privately used into this package only.
  Be_Manager can be considered as a "virtual base class"
  which must be instantiated via inheritance by more specific classes
  whose implementations depend on the real low-level structure them use
  (i.e. the rbc manager)
  Files beRbc.{h,c} define and implement the derived class which implements
  the RBC layer. Look at them as a possible template and example.

*/


#ifndef __NUSMV_CORE_BE_BE_MANAGER_INT_H__
#define __NUSMV_CORE_BE_BE_MANAGER_INT_H__

#include "nusmv/core/be/be.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief The generic Boolean Expressions Manager (private declaration)

  To access this structure you must use the Be_Manager_ptr type.

  \sa Be_Manager_ptr
*/

typedef struct Be_Manager_TAG {
  NuSMVEnv_ptr environment;

  void* spec_manager; /* the low-level manager */
  void* support_data; /* any support structure can be stored here */

  /* Gateway: */
  Be_Spec2Be_fun       spec2be_converter;
  Be_Be2Spec_fun       be2spec_converter;

} Be_Manager;


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \methodof Be_Manager
  \brief Creates a generic Be_Manager

  spec_manager is the specific structure which is used to manage
  the low-level structure. For example the RbcManager class in the
  RBC dependant implementation.
  This does not assume the ownership of 'spec_manager'. If you have dynamically
  created the spec_manager instance, you are responsible for deleting it after
  you deleted the Be_manager instance.
  This "virtual" function is supplied in order to be called by any
  specific class derived from Be_Manager, in its constructor code.
  spec2be and be2spec converters are gateways in order to polymorphically
  convert the low level support structure (for example a rbc pointer) to
  the generic be_ptr and viceversa.

  \sa Be_RbcManager_Create, Be_Manager_Delete
*/
Be_Manager_ptr
Be_Manager_Create(const NuSMVEnv_ptr env,
                  void* spec_manager,
                  Be_Spec2Be_fun      spec2be_converter,
                  Be_Be2Spec_fun      be2spec_converter);

/*!
  \methodof Be_Manager
  \brief Be_Manager destroyer

  Call this function from the destructor of the derived class
  that implements the Be_Manager class. Any other use is to be considered
  unusual.

  \se self will be deleted from memory.

  \sa Be_RbcManager_Delete, Be_Manager_Create
*/
void Be_Manager_Delete(Be_Manager_ptr self);

/* Access: */

/*!
  \methodof Be_Manager
  \brief Derived classes data can be retrieved by this method

  When you instantiate a derived BE manager (for example the
  rbc manager) you can store any useful specific data by using
  Be_Manager_SetData. Those data can be lately retrieved by Be_Manager_GetData
  which gets a generic, structure-independent Be_Manager.

  \sa Be_Manager_SetData
*/
void* Be_Manager_GetData(const Be_Manager_ptr self);

/*!
  \methodof Be_Manager
  \brief Sets specific structure manager data into the generic
  manager

  You can retieve saved data by using the method
  Be_Manager_GetData. This implements a kind of inheritance mechanism.

  \se self will change its internal state.

  \sa Be_Manager_GetData
*/
void  Be_Manager_SetData(Be_Manager_ptr self, void* data);


/**AutomaticEnd***************************************************************/

#endif /* __NUSMV_CORE_BE_BE_MANAGER_INT_H__ */


