/* ---------------------------------------------------------------------------


  This file is part of the ``node'' package of NuSMV version 2. 
  Copyright (C) 1998-2001 by CMU and FBK-irst. 

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
  \author Marco Roveri
  \brief The internal header of the <tt>node</tt> package.

  None

*/


#ifndef __NUSMV_CORE_NODE_NODE_INT_H__
#define __NUSMV_CORE_NODE_NODE_INT_H__

#include <stdio.h>

#include "nusmv/core/node/node.h"
#include "nusmv/core/opt/opt.h"

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/cinit/NuSMVEnv.h"

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
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \brief Initializes the <tt>node</tt> manager.

  The <tt>node</tt> manager is initialized.

  \se None
*/
void node_init(NuSMVEnv_ptr env);

/*!
  \brief De-initializes the <tt>node</tt> manager.

  The <tt>node</tt> manager is de-initialized.

  \se None
*/
void node_quit(NuSMVEnv_ptr env);

#endif /* __NUSMV_CORE_NODE_NODE_INT_H__ */
