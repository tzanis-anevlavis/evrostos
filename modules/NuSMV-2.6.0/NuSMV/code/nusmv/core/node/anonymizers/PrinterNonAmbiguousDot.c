/* ---------------------------------------------------------------------------


  This file is part of the ``core.node.anonymizer'' package of NuSMV version 2.
  Copyright (C) 2014 by FBK-irst.

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
  \author Michele Dorigatti
  \brief Implementation of class 'PrinterNonAmbiguousDot'

  Prints an identifier in a format that allows to restore the DOT
  structure when it is read
  
  The format is the following:

  * DOT is printed as "."
  * Childrens of DOT are separated by ","
  
  Note: Being ",." redundant, it is printed just as "."

*/


#include "nusmv/core/node/anonymizers/PrinterNonAmbiguousDot.h"
#include "nusmv/core/node/anonymizers/PrinterNonAmbiguousDot_private.h"
#include "nusmv/core/node/anonymizers/NodeAnonymizerBase.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/ErrorMgr.h"
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
/* See 'PrinterNonAmbiguousDot_private.h' for class 'PrinterNonAmbiguousDot' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief Short way of calling printer_base_throw_print_node

  Use this macro to recursively recall print_node
*/

#define _THROW(n)  printer_base_throw_print_node(PRINTER_BASE(self), n, 0)


/*!
  \brief Short way of calling printer_base_print_string

  Use to print a string (that will be redirected to the
  currently used stream)
*/

#define _PRINT(str)  printer_base_print_string(PRINTER_BASE(self), str)


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void printer_anon_map_entry_finalize(Object_ptr object, void* dummy);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

PrinterNonAmbiguousDot_ptr PrinterNonAmbiguousDot_create(const NuSMVEnv_ptr env)
{
  PrinterNonAmbiguousDot_ptr self = ALLOC(PrinterNonAmbiguousDot, 1);
  PRINTER_ANON_MAP_ENTRY_CHECK_INSTANCE(self);

  printer_anon_map_entry_init(self, env, "",
                              ATOM,
                              DOT - ATOM + 1);
  return self;
}

void PrinterNonAmbiguousDot_destroy(PrinterNonAmbiguousDot_ptr self)
{
  PRINTER_ANON_MAP_ENTRY_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

int printer_anon_map_entry_print_node(PrinterBase_ptr self,
                                      node_ptr n,
                                      int priority)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (Nil == n) return 1;
  
  switch(node_get_type(n)) {
  case DOT:
    {
      /* since "." is also a separator, we can skip print the "," if
         immediately followed by "." */
      return
        _PRINT(NODE_ANONYMIZER_DOT_STR) &&
        _THROW(car(n)) &&
        ((Nil != cdr(n) && DOT == node_get_type(cdr(n))) || _PRINT(NODE_ANONYMIZER_SEPARATOR_STR)) &&
        _THROW(cdr(n));
    }

  case ATOM:
    /* Here ideally we would let the PrinterWffCore handle it. We can't because
       PrinterWffCore and this printer both handle DOT nodes. So we are
       duplicating the PrinterWffCore code for ATOM and NUMBER */
    if (!_PRINT(UStringMgr_get_string_text((string_ptr) car(n)))) return 0;
    if (cdr(n)) {
      char buf[20];
      int chars = snprintf(buf, 20, "_%d", NODE_TO_INT(cdr(n)));
      SNPRINTF_CHECK(chars, 20);

      return _PRINT(buf);
    }
    return 1;

  case NUMBER:
    {
      char buf[20];
      int c = snprintf(buf, 20, "%d", NODE_TO_INT(car(n)));
      SNPRINTF_CHECK(c, 20);

      return _PRINT(buf);
    }
    /* end of code duplication */

  default:
    ErrorMgr_internal_error(errmgr, "%s: not supported type = %d",
                            __func__,
                            node_get_type(n));
  }
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void printer_anon_map_entry_init(PrinterNonAmbiguousDot_ptr self,
                                 const NuSMVEnv_ptr env,
                                 const char* name,
                                 int low,
                                 size_t num)
{
  /* base class initialization */
  printer_base_init(PRINTER_BASE(self), env, name, low, num, true);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = printer_anon_map_entry_finalize;
  OVERRIDE(PrinterBase, print_node) = printer_anon_map_entry_print_node;
}

void printer_anon_map_entry_deinit(PrinterNonAmbiguousDot_ptr self)
{
  /* members deinitialization */


  /* base class deinitialization */
  printer_base_deinit(PRINTER_BASE(self));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The PrinterNonAmbiguousDot class virtual finalizer

  Called by the class destructor
*/
static void printer_anon_map_entry_finalize(Object_ptr object, void* dummy)
{
  PrinterNonAmbiguousDot_ptr self = PRINTER_ANON_MAP_ENTRY(object);

  printer_anon_map_entry_deinit(self);
  FREE(self);
}



/**AutomaticEnd***************************************************************/

