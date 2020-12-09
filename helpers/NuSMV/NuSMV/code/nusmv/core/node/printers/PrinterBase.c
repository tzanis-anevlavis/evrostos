
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
  \brief Implementaion of class 'PrinterBase'

  \todo: Missing description

*/


#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/node/printers/PrinterBase.h"
#include "nusmv/core/node/printers/PrinterBase_private.h"

#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/node/printers/MasterPrinter_private.h"

#include "nusmv/core/node/MasterNodeWalker.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'PrinterBase_private.h' for class 'PrinterBase' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void printer_base_finalize(Object_ptr object, void* dummy);

static int
printer_base_print_node(PrinterBase_ptr self, node_ptr n, int priority);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

PrinterBase_ptr
PrinterBase_create(const NuSMVEnv_ptr env, const char* name, int low, size_t num)
{
  PrinterBase_ptr self = ALLOC(PrinterBase, 1);
  PRINTER_BASE_CHECK_INSTANCE(self);

  printer_base_init(self, env, name, low, num, false);
  return self;
}


VIRTUAL int
PrinterBase_print_node(PrinterBase_ptr self, node_ptr n, int priority)
{
  PRINTER_BASE_CHECK_INSTANCE(self);

  n = node_walker_run_transformation_chain(NODE_WALKER(self), n);
  return self->print_node(self, n, priority);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void printer_base_init(PrinterBase_ptr self, const NuSMVEnv_ptr env,
                       const char* name, int low, size_t num,
                       boolean can_handle_null)
{
  /* base class initialization */
  node_walker_init(NODE_WALKER(self), env, name, low, num, can_handle_null);

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = printer_base_finalize;
  OVERRIDE(PrinterBase, print_node) = printer_base_print_node;
}

void printer_base_deinit(PrinterBase_ptr self)
{
  /* base class initialization */
  node_walker_deinit(NODE_WALKER(self));
}


int printer_base_throw_print_node(PrinterBase_ptr self, node_ptr n, int prior)
{
  if (NodeWalker_can_handle(NODE_WALKER(self), n)) {
    /* checks if self can handle the node without need of re-throw
       to the master */
    return PrinterBase_print_node(self, n, prior);
  }
  return master_printer_print_node(
           MASTER_PRINTER(NODE_WALKER(self)->master), n, prior);
}

int printer_base_print_string(PrinterBase_ptr self, const char* str)
{
  return MasterPrinter_print_string(
      MASTER_PRINTER(NODE_WALKER(self)->master), str);
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The PrinterBase class virtual finalizer

  Called by the class destructor
*/
static void printer_base_finalize(Object_ptr object, void* dummy)
{
  PrinterBase_ptr self = PRINTER_BASE(object);

  printer_base_deinit(self);
  FREE(self);
}

/*!
  \brief Virtual menthod that prints the given node

  This is a pure virtual method, to be implemented by derived
  class, and cannot be called
*/
static int
printer_base_print_node(PrinterBase_ptr self, node_ptr n, int priority)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  ErrorMgr_internal_error(errmgr, "PrinterBase: Pure virtual method print_node " \
                 "not implemented\n");
  return 0;
}

/**AutomaticEnd***************************************************************/
