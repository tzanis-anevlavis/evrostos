/* ---------------------------------------------------------------------------


  This file is part of the ``compile.symb_table'' package of NuSMV
  version 2.  Copyright (C) 2004 by FBK-irst.

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
  \brief The private interface of the package compile.symb_table

  \todo: Missing description

*/


#ifndef __NUSMV_CORE_COMPILE_SYMB_TABLE_SYMB_TABLE_INT_H__
#define __NUSMV_CORE_COMPILE_SYMB_TABLE_SYMB_TABLE_INT_H__

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/cinit/NuSMVEnv.h"

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

extern int nusmv_yylineno;

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \brief Initialises the class package.

  This initialisation can be performed only after
  the Node package and the variable boolean_range have been initialised.
*/
void SymbTablePkg_init(NuSMVEnv_ptr env);

/*!
  \brief Shut down the package.

  WARNING: the package de-initialisation
  destroys types created with the function SymbTablePkg_..._type.

  NB: The reason behind this constrain is the following: these functions
  exploit memory sharing, and this memory is freed during
  de-initialisation.

  In any case, the de-initialisation is performed by
  system "reset" command, and this command also frees all node_ptr,
  so in any case the symbolic types will be unusable, because they
  use node_ptr inside.
  SO, DESTROY ALL SYMBOLIC TYPES CREATED SO FAR BEFORE THE symb_table
  PACKAGE DE-INITIALISATION!
  
*/
void SymbTablePkg_quit(NuSMVEnv_ptr env);


#endif /* __NUSMV_CORE_COMPILE_SYMB_TABLE_SYMB_TABLE_INT_H__ */
