/* ---------------------------------------------------------------------------


  This file is part of the ``dd'' package of NuSMV version 2.
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
  \brief Private and protected interface of class 'DDMgr'

  This file can be included only by derived and friend classes

*/



#ifndef __NUSMV_CORE_DD_DDMGR_PRIVATE_H__
#define __NUSMV_CORE_DD_DDMGR_PRIVATE_H__


#include "nusmv/core/dd/DDMgr.h"
#include "nusmv/core/utils/EnvObject.h"
#include "nusmv/core/utils/EnvObject_private.h"
#include "nusmv/core/utils/utils.h"


/*!
  \brief DDMgr class definition derived from
               class EnvObject

  

  \sa Base class EnvObject
*/

typedef struct DDMgr_TAG
{
  /* this MUST stay on the top */
  INHERITS_FROM(EnvObject);

  DdManager* dd;
  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */


  /* -------------------------------------------------- */
  /*                  Virtual methods                   */
  /* -------------------------------------------------- */

} DDMgr;



/* ---------------------------------------------------------------------- */
/* Private methods to be used by derivated and friend classes only         */
/* ---------------------------------------------------------------------- */

/*!
  \methodof DDMgr
  \brief The DDMgr class private initializer

  The DDMgr class private initializer

  \sa DDMgr_create
*/
void dd_manager_init(DDMgr_ptr self, const NuSMVEnv_ptr env);

/*!
  \methodof DDMgr
  \brief The DDMgr class private deinitializer

  The DDMgr class private deinitializer

  \sa DDMgr_destroy
*/
void dd_manager_deinit(DDMgr_ptr self);



#endif /* __NUSMV_CORE_DD_DDMGR_PRIVATE_H__ */
