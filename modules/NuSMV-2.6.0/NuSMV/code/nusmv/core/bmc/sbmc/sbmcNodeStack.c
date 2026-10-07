/* ---------------------------------------------------------------------------


  This file is part of the ``bmc.sbmc'' package of NuSMV version 2. 
  Copyright (C) 2004 by Timo Latvala <timo.latvala@tkk.fi>. 

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

  For more information of NuSMV see <http://nusmv.fbk.eu>
  or email to <nusmv-users@fbk.eu>.
  Please report bugs to <nusmv-users@fbk.eu>.

  To contact the NuSMV development board, email to <nusmv@fbk.eu>. 

-----------------------------------------------------------------------------*/

/*!
  \author Timo Latvala
  \brief A stack of node_ptr

  A stack of node_ptr

*/


#include "nusmv/core/utils/ErrorMgr.h"
#include <stdlib.h>
#include <stdio.h>

#include "nusmv/core/bmc/sbmc/sbmcNodeStack.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/error.h"
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define STACK_SIZE 127

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

Bmc_Stack_ptr Bmc_Stack_new_stack(const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  unsigned i;
  Bmc_Stack_ptr thestack = (Bmc_Stack_ptr)ALLOC(struct nodeStack, 1);
  thestack->alloc = STACK_SIZE;
  thestack->first_free = 0;
  thestack->table = (node_ptr *) ALLOC(node_ptr, thestack->alloc);
  if (thestack->table == NULL) {
    ErrorMgr_internal_error(errmgr, "Bmc_Stack_new_stack: Out of Memory");
  }

  for (i=0; i < thestack->alloc; ++i) {
    thestack->table[i] = NULL;
  }
  return thestack;
}

void Bmc_Stack_push(Bmc_Stack_ptr thestack, node_ptr node) 
{ 
  if (thestack->first_free >= thestack->alloc) { /**The stack needs to grow*/
    unsigned i;
    node_ptr* temp;
    thestack->alloc = 2*thestack->alloc;
    temp = ALLOC(node_ptr, thestack->alloc); 
    nusmv_assert(temp != NULL);
    for (i = thestack->first_free; i--; ) {
      temp[i] = thestack->table[i];
    }
    FREE(thestack->table);
    thestack->table = temp;    
  }
  /**Put node on stack*/
  thestack->table[thestack->first_free] = node;
  thestack->first_free++;
  return;
}

unsigned Bmc_Stack_size(Bmc_Stack_ptr thestack) 
{  
  return thestack->first_free;
}

node_ptr Bmc_Stack_pop (Bmc_Stack_ptr thestack)
{
  nusmv_assert(thestack->first_free > 0);
  thestack->first_free--;
  return thestack->table[thestack->first_free];
}

void Bmc_Stack_delete(Bmc_Stack_ptr thestack)
{
  FREE(thestack->table);
  FREE(thestack);
}

node_ptr Bmc_Stack_top(Bmc_Stack_ptr thestack)
{
  nusmv_assert(thestack->first_free > 0);
  return thestack->table[thestack->first_free - 1];
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
