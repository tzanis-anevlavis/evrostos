/* ---------------------------------------------------------------------------

This file is part of the ``rbc.clg'' package 
  of NuSMV version 2. Copyright (C) 2007 by FBK-irst.

  NuSMV version 2 is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2 of
  the License, or (at your option) any later version.

  NuSMV version 2 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA. 

  For more information on NuSMV see <http://nusmv.fbk.eu>
  or email to <nusmv-users@fbk.eu>.
  Please report bugs to <nusmv-users@fbk.eu>.

  To contact the NuSMV development board, email to <nusmv@fbk.eu>.

-----------------------------------------------------------------------------*/

/*!
  \author Dan Sheridan & Marco Roveri
  \brief The internal header of the <tt>tl</tt> package.

  None

*/


#ifndef __NUSMV_CORE_RBC_CLG_CLG_INT_H__
#define __NUSMV_CORE_RBC_CLG_CLG_INT_H__

#include "nusmv/core/rbc/clg/clg.h"
#include "nusmv/core/utils/utils.h"


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CLG_CONJ 10

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CLG_DISJ 11

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

struct Clg_Vertex {
  int label;
  int size;
  struct Clg_Vertex* left; 
  struct Clg_Vertex* right; 
};

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


/**AutomaticEnd***************************************************************/

#endif /* _TL_INT_h */
