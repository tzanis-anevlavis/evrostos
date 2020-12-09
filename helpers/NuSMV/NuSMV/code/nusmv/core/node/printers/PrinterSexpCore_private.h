
/* ---------------------------------------------------------------------------


  This file is part of the ``node.printers'' package of NuSMV version 2.
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
  \brief Private and protected interface of class 'PrinterSexpCore'

  This file can be included only by derived and friend classes

*/



#ifndef __NUSMV_CORE_NODE_PRINTERS_PRINTER_SEXP_CORE_PRIVATE_H__
#define __NUSMV_CORE_NODE_PRINTERS_PRINTER_SEXP_CORE_PRIVATE_H__


#include "nusmv/core/node/printers/PrinterSexpCore.h"
#include "nusmv/core/node/printers/PrinterBase.h" /* fix this */
#include "nusmv/core/node/printers/PrinterBase_private.h" /* fix this */
#include "nusmv/core/utils/utils.h"


/*!
  \brief PrinterSexpCore class definition derived from
               class PrinterBase

  

  \sa Base class PrinterBase
*/

typedef struct PrinterSexpCore_TAG
{
  /* this MUST stay on the top */
  INHERITS_FROM(PrinterBase);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */


  /* -------------------------------------------------- */
  /*                  Virtual methods                   */
  /* -------------------------------------------------- */

} PrinterSexpCore;



/* ---------------------------------------------------------------------- */
/* Private methods to be used by derivated and friend classes only         */
/* ---------------------------------------------------------------------- */

/*!
  \methodof PrinterSexpCore
  \brief The PrinterSexpCore class private initializer

  The PrinterSexpCore class private initializer

  \sa PrinterSexpCore_create
*/
void
printer_sexp_core_init(PrinterSexpCore_ptr self, const NuSMVEnv_ptr env,
                       const char* name, int low, size_t num);

/*!
  \methodof PrinterSexpCore
  \brief The PrinterSexpCore class private deinitializer

  The PrinterSexpCore class private deinitializer

  \sa PrinterSexpCore_destroy
*/
void printer_sexp_core_deinit(PrinterSexpCore_ptr self);

/*!
  \methodof PrinterSexpCore
  \brief Virtual menthod that prints the given node
   (core nodes are handled here)

  
*/
int
printer_sexp_core_print_node(PrinterBase_ptr self, node_ptr n,
                            int priority);

#endif /* __NUSMV_CORE_NODE_PRINTERS_PRINTER_SEXP_CORE_PRIVATE_H__ */
