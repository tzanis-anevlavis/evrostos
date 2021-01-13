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
  \brief The package internal interface for the <tt>be</tt> package

  \todo: Missing description

*/



#ifndef __NUSMV_CORE_BE_BE_INT_H__
#define __NUSMV_CORE_BE_BE_INT_H__

#include "nusmv/core/utils/assoc.h" /* for st_table */

#include "nusmv/core/be/beManagerInt.h" /* private interface */
#include "nusmv/core/be/be.h"

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/



/*---------------------------------------------------------------------------*/
/* Structures declarations                                                   */
/*---------------------------------------------------------------------------*/

/*!
  \brief shift_memoize_key is a private struct used in shift memoizing

  Used in order to contain the key values in hash operation 
  involved in shift memoizing
*/

typedef struct shift_memoize_key_TAG
{
  be_ptr  be;
  int     shift;
} shift_memoize_key; 


#endif /* __NUSMV_CORE_BE_BE_INT_H__ */
