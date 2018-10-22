/* ---------------------------------------------------------------------------


  This file is part of the ``node'' package of NuSMV version 2.
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
  \brief Private interface of class 'MasterNodeWalker', to be used by
  derivated classes

  \todo: Missing description

*/



#ifndef __NUSMV_CORE_NODE_MASTER_NODE_WALKER_PRIVATE_H__
#define __NUSMV_CORE_NODE_MASTER_NODE_WALKER_PRIVATE_H__

#include "nusmv/core/node/MasterNodeWalker.h"

#include "nusmv/core/utils/EnvObject.h"
#include "nusmv/core/utils/EnvObject_private.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/NodeList.h"


/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \brief MasterNodeWalker class definition

  
*/

typedef struct MasterNodeWalker_TAG
{
  /* this MUST stay on the top */
  INHERITS_FROM(EnvObject);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  NodeList_ptr walkers;

} MasterNodeWalker;


/* ---------------------------------------------------------------------- */
/* Private methods to be used by derivated and friend classes only         */
/* ---------------------------------------------------------------------- */

/*!
  \methodof MasterNodeWalker
  \brief The MasterNodeWalker class private initializer

  The MasterNodeWalker class private initializer

  \sa MasterNodeWalker_create
*/
void master_node_walker_init(MasterNodeWalker_ptr self,
                                    const NuSMVEnv_ptr env);

/*!
  \methodof MasterNodeWalker
  \brief The MasterNodeWalker class private deinitializer

  The MasterNodeWalker class private deinitializer

  \sa MasterNodeWalker_destroy
*/
void master_node_walker_deinit(MasterNodeWalker_ptr self);



/**AutomaticEnd***************************************************************/


#endif /* __NUSMV_CORE_NODE_MASTER_NODE_WALKER_PRIVATE_H__ */
