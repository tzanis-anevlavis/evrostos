/* ---------------------------------------------------------------------------


  This file is part of the ``node.printers'' package of NuSMV version 2.
  Copyright (C) 2009 by FBK.

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
  \author Marco Pensallorto
  \brief Private and protected interface of class 'PrinterIWffCore'

  This file can be included only by derived and friend classes

*/



#ifndef __NUSMV_CORE_NODE_PRINTERS_PRINTER_IWFF_CORE_PRIVATE_H__
#define __NUSMV_CORE_NODE_PRINTERS_PRINTER_IWFF_CORE_PRIVATE_H__


#include "nusmv/core/node/printers/PrinterIWffCore.h"

#include "nusmv/core/node/printers/PrinterWffCore.h"
#include "nusmv/core/node/printers/PrinterWffCore_private.h"

#include "nusmv/core/node/printers/PrinterBase.h"
#include "nusmv/core/node/printers/PrinterBase_private.h"

#include "nusmv/core/utils/utils.h"


/*!
  \brief PrinterIWffCore class definition derived from class
  PrinterWffCore

  

  \sa Base class PrinterIWffCore
*/

typedef struct PrinterIWffCore_TAG
{
  /* this MUST stay on the top */
  INHERITS_FROM(PrinterWffCore);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */


  /* -------------------------------------------------- */
  /*                  Virtual methods                   */
  /* -------------------------------------------------- */

} PrinterIWffCore;


/* ---------------------------------------------------------------------- */
/* Private methods to be used by derivated and friend classes only        */
/* ---------------------------------------------------------------------- */

/*!
  \methodof PrinterIWffCore
  \brief The PrinterIWffCore class private initializer

  The PrinterIWffCore class private initializer

  \sa PrinterIWffCore_create
*/
void
printer_iwff_core_init(PrinterIWffCore_ptr self, const NuSMVEnv_ptr env,
                       const char* name, int low, size_t num);

/*!
  \methodof PrinterIWffCore
  \brief The PrinterIWffCore class private deinitializer

  The PrinterIWffCore class private deinitializer

  \sa PrinterIWffCore_destroy
*/
void
printer_iwff_core_deinit(PrinterIWffCore_ptr self);

/*!
  \methodof PrinterIWffCore
  \brief Virtual menthod that prints the given node
  (only indentantion capable nodes are handled here)

  
*/
int
printer_iwff_core_print_node(PrinterBase_ptr self, node_ptr n,
                            int priority);

#endif /* __NUSMV_CORE_NODE_PRINTERS_PRINTER_IWFF_CORE_PRIVATE_H__ */
