/* ---------------------------------------------------------------------------


  This file is part of the ``node.normalizers'' package of NuSMV version 2.
  Copyright (C) 2004 by FBK-irst.

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
  \brief Private and protected interface of class 'NormalizerCore'

  This file can be included only by derived and friend classes

*/



#ifndef __NUSMV_CORE_NODE_NORMALIZERS_NORMALIZER_CORE_PRIVATE_H__
#define __NUSMV_CORE_NODE_NORMALIZERS_NORMALIZER_CORE_PRIVATE_H__


#include "nusmv/core/node/normalizers/NormalizerCore.h"
#include "nusmv/core/node/normalizers/NormalizerBase.h"
#include "nusmv/core/node/normalizers/NormalizerBase_private.h"
#include "nusmv/core/utils/utils.h"


/*!
  \brief NormalizerCore class definition derived from
               class NormalizerBase

  

  \sa Base class NormalizerBase
*/

typedef struct NormalizerCore_TAG
{
  /* this MUST stay on the top */
  INHERITS_FROM(NormalizerBase);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */


  /* -------------------------------------------------- */
  /*                  Virtual methods                   */
  /* -------------------------------------------------- */

} NormalizerCore;



/* ---------------------------------------------------------------------- */
/* Private methods to be used by derivated and friend classes only         */
/* ---------------------------------------------------------------------- */

/*!
  \methodof NormalizerCore
  \brief The NormalizerCore class private initializer

  The NormalizerCore class private initializer

  \sa NormalizerCore_create
*/
void
normalizer_core_init(NormalizerCore_ptr self, const NuSMVEnv_ptr env,
                     const char* name, int low, size_t num);

/*!
  \methodof NormalizerCore
  \brief The NormalizerCore class private deinitializer

  The NormalizerCore class private deinitializer

  \sa NormalizerCore_destroy
*/
void normalizer_core_deinit(NormalizerCore_ptr self);

/*!
  \methodof NormalizerCore
  \brief Virtual menthod that normalizes the given node
   (core nodes are handled here)

  
*/
node_ptr
normalizer_core_normalize_node(NormalizerBase_ptr self, node_ptr n);

#endif /* __NUSMV_CORE_NODE_NORMALIZERS_NORMALIZER_CORE_PRIVATE_H__ */
