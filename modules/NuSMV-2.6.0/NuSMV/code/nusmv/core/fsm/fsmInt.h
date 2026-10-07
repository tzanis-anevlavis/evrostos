/* ---------------------------------------------------------------------------


  This file is part of the ``fsm'' package of NuSMV version 2. 
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
  \brief Internal interfaces for package fsm

  \todo: Missing description

*/


#ifndef __NUSMV_CORE_FSM_FSM_INT_H__
#define __NUSMV_CORE_FSM_FSM_INT_H__


#include "nusmv/core/fsm/FsmBuilder.h"

#include "nusmv/core/compile/FlatHierarchy.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/utils/utils.h"


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
extern FsmBuilder_ptr global_fsm_builder; 
extern FlatHierarchy_ptr mainFlatHierarchy;



extern int nusmv_yylineno;

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

#endif /* __NUSMV_CORE_FSM_FSM_INT_H__ */
