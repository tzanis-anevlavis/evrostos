 /* ---------------------------------------------------------------------------


  This file is part of the ``simulate'' package of NuSMV version 2.
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
  \author Andrea Morichetti
  \brief Internal Header File for the simulate package

  Internal Header File for the simulate package

*/


#ifndef __NUSMV_CORE_SIMULATE_SIMULATE_INT_H__
#define __NUSMV_CORE_SIMULATE_SIMULATE_INT_H__

#if HAVE_CONFIG_H
#include "nusmv-config.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#include "nusmv/core/simulate/simulate.h"

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/dd/dd.h"
#include "nusmv/core/opt/opt.h"

#include "nusmv/core/fsm/FsmBuilder.h"
#include "nusmv/core/fsm/bdd/BddFsm.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/TraceMgr.h"


#if NUSMV_HAVE_SYS_SIGNAL_H
#  include <sys/signal.h>
#endif
#if NUSMV_HAVE_SIGNAL_H
#  include <signal.h>
#endif

#include <setjmp.h>
#include <assert.h>



/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/
/* Length of the string used for the choice entered in interac. sim.*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CHOICE_LENGTH 8

extern cmp_struct_ptr cmps;
extern int trace_number;


extern TraceMgr_ptr global_trace_manager;
extern FsmBuilder_ptr global_fsm_builder;

extern char* simulation_buffer;
extern size_t simulation_buffer_size;

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \brief Picks one state, to be used for BDD simulation

  Returns the trace index on success, -1 otherwise

  \se required

  \sa optional
*/
int
Simulate_CmdPickOneState(NuSMVEnv_ptr, BddFsm_ptr,
                         Simulation_Mode, int, bdd_ptr);



#endif /* __NUSMV_CORE_SIMULATE_SIMULATE_INT_H__ */
