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
  \brief Implementation of class 'PrinterIWffCore'

  \todo: Missing description

*/


#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/node/printers/PrinterIWffCore.h"
#include "nusmv/core/node/printers/PrinterIWffCore_private.h"
#include "nusmv/core/node/printers/MasterPrinter_private.h"
#include "nusmv/core/parser/symbols.h"

#include "nusmv/core/utils/WordNumberMgr.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'PrinterIWffCore_private.h' for class 'PrinterIWffCore' definition. */

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief Short way of calling printer_base_throw_print_node

  Use this macro to recursively recall print_node
*/

#define _THROW(n, p)  printer_base_throw_print_node(PRINTER_BASE(self), n, p)


/*!
  \brief Short way of calling printer_base_print_string

  Use to print a string (that will be redirected to the
  currently used stream)
*/

#define _PRINT(str)  printer_base_print_string(PRINTER_BASE(self), str)
#define _NEWLINE()   printer_base_print_string(PRINTER_BASE(self), "\n")

/*!
  \brief Short way of calling master_printer_indent

  Use to augment current level of indentation
*/

#define _INDENT()							\
  master_printer_indent(						\
			MASTER_PRINTER(NODE_WALKER(self)->master))

/*!
  \brief Short way of calling master_printer_deindentt

  Use to revert to previous level of indentation
*/

#define _DEINDENT(x)                                                    \
  master_printer_deindent(                                              \
			MASTER_PRINTER(NODE_WALKER(self)->master))

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void printer_iwff_core_finalize(Object_ptr object, void* dummy);

static int
printer_iwff_core_print_case(PrinterIWffCore_ptr self, node_ptr n);

static int
printer_iwff_core_print_case_body(PrinterIWffCore_ptr self, node_ptr n);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

PrinterIWffCore_ptr PrinterIWffCore_create(const NuSMVEnv_ptr env,
                                           const char* name)
{
  PrinterIWffCore_ptr self = ALLOC(PrinterIWffCore, 1);
  PRINTER_IWFF_CORE_CHECK_INSTANCE(self);

  printer_iwff_core_init(self, env, name,
                         NUSMV_CORE_SYMBOL_FIRST,
                         NUSMV_CORE_SYMBOL_LAST - NUSMV_CORE_SYMBOL_FIRST);
  return self;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void printer_iwff_core_init(PrinterIWffCore_ptr self,
                            const NuSMVEnv_ptr env,
                            const char* name,
                            int low, size_t num)
{
  /* base class initialization */
  printer_wff_core_init(PRINTER_WFF_CORE(self), env, name, low, num);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = printer_iwff_core_finalize;
  OVERRIDE(PrinterBase, print_node) = printer_iwff_core_print_node;
}

void printer_iwff_core_deinit(PrinterIWffCore_ptr self)
{
  /* members deinitialization */

  /* base class initialization */
  printer_wff_core_deinit(PRINTER_WFF_CORE(self));
}

int printer_iwff_core_print_node(PrinterBase_ptr self, node_ptr n, int priority)
{
  switch (node_get_type(n)) {
  case CASE:
  case IFTHENELSE:
    return printer_iwff_core_print_case(PRINTER_IWFF_CORE(self), n);

    /* for all any other case delegate responsibility to ancestor class */
  default:
    return printer_wff_core_print_node(PRINTER_BASE(self), n, priority);
  }
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The PrinterWffCore class virtual finalizer

  Called by the class destructor
*/
static void printer_iwff_core_finalize(Object_ptr object, void* dummy)
{
  PrinterIWffCore_ptr self = PRINTER_IWFF_CORE(object);

  printer_iwff_core_deinit(self);
  FREE(self);
}

/*!
  \brief required

  optional

  \se required

  \sa optional
*/
static int printer_iwff_core_print_case(PrinterIWffCore_ptr self, node_ptr n)
{
  return
    _PRINT("case")
    && _INDENT()
    && _NEWLINE()

    /* recursively inside the case */
    && printer_iwff_core_print_case_body(self, n)

    /* newline *after* the DEINDENT */
    && _DEINDENT()
    && _NEWLINE()

    && _PRINT("esac");
}

/*!
  \brief required

  optional

  \se required

  \sa optional
*/
static int
printer_iwff_core_print_case_body(PrinterIWffCore_ptr self, node_ptr n)
{
  int res;

  nusmv_assert(n != Nil);
  res = _THROW(car(car(n)), 0) && _PRINT(" : ") &&
    _THROW(cdr(car(n)), 0) && _PRINT(";\n");

  if (res == 0) return 0; /* previous error */

  nusmv_assert(cdr(n) != Nil); /* Now there is always a last(default) case */

  /* continue to print the tail of the case list */
  if (node_get_type(cdr(n)) == CASE || node_get_type(cdr(n)) == IFTHENELSE) {
    return printer_iwff_core_print_case_body(self, cdr(n));
  }
  /* print the last(default) element. Do not print artificial FAILURE node */
  else if (node_get_type(cdr(n)) != FAILURE) {
    return _PRINT("TRUE : ") && /* the last (default) element */
      _THROW(cdr(n), 0) && _PRINT(";");
  }

  return res; /* the last element is FAILURE node */
}

/**AutomaticEnd***************************************************************/

