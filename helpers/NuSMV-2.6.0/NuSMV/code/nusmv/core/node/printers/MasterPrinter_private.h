
/* ---------------------------------------------------------------------------


  This file is part of the ``node.printers'' package of NuSMV version 2. 
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
  \brief Private interface of class 'MasterPrinter', to be used by printers

  \todo: Missing description

*/



#ifndef __NUSMV_CORE_NODE_PRINTERS_MASTER_PRINTER_PRIVATE_H__
#define __NUSMV_CORE_NODE_PRINTERS_MASTER_PRINTER_PRIVATE_H__

#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/utils/utils.h" 

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \methodof MasterPrinter
  \brief Internal version of the method print_node, callable
  internally and by printers

  
*/
int 
master_printer_print_node(MasterPrinter_ptr self, node_ptr n, 
                int priority);

/*!
  \methodof MasterPrinter
  \brief Pushes the current level of indentation

  

  \se The internal status of the master printer is changed

  \sa master_printer_deindent
*/
int master_printer_indent(MasterPrinter_ptr self);

/*!
  \methodof MasterPrinter
  \brief Restore previous level of indentation

  Restore previous level of indentation. Raises an
  internal error if an inconsisten internal state is detected.

  \se The internal status of the master printer is changed

  \sa master_printer_indent
*/
int master_printer_deindent(MasterPrinter_ptr self);

/**AutomaticEnd***************************************************************/

#endif /* __NUSMV_CORE_NODE_PRINTERS_MASTER_PRINTER_PRIVATE_H__ */
