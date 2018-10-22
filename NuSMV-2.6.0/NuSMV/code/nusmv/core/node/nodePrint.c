/* ---------------------------------------------------------------------------


  This file is part of the ``node'' package of NuSMV version 2.
  Copyright (C) 1998-2001 by CMU and FBK-irst.

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
  \author Marco Roveri
  \brief Pretty prints a node struct.

  This function pretty print a node struct, in a way
  similar to a s-expression in LISP.

*/

#include "nusmv/core/node/nodeInt.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/WordNumberMgr.h" /* for WordNumber_ptr printing */
#include "nusmv/core/node/printers/MasterPrinter.h"

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void print_array_type_rec(const NuSMVEnv_ptr env, FILE* out, const node_ptr body);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void print_array_type(const NuSMVEnv_ptr env, FILE* output_stream, const node_ptr body)
{
  nusmv_assert(ARRAY_TYPE == node_get_type(body));
  print_array_type_rec(env, output_stream, body);
}

/*!
  \brief Private function of print_array_type

  

  \sa print_array_type
*/
static void print_array_type_rec(const NuSMVEnv_ptr env, FILE* out, const node_ptr body)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  switch (node_get_type(body)) {
  case ARRAY_TYPE:
    fprintf(out, "array ");
    print_array_type_rec(env, out, car(body));
    fprintf(out, " of ");
    print_array_type_rec(env, out, cdr(body));
    break;

  case TWODOTS:
    print_node(wffprint, out, car(body));
    fprintf(out, " .. ");
    print_node(wffprint, out, cdr(body));
    break;

  case BOOLEAN:
    fprintf(out, "boolean");
    break;

  case UNSIGNED_WORD:
    fprintf(out, "word[");
    print_node(wffprint, out, car(body));
    fprintf(out, "]");
    break;

  case INTEGER:
    fprintf(out, "integer");
    break;

  case REAL:
    fprintf(out, "real");
    break;

  case SCALAR:
    fprintf(out, "{ ");
    print_array_type_rec(env, out, car(body));
    fprintf(out, " }");
    break;

  case CONS:
    print_array_type_rec(env, out, car(body));
    if (cdr(body) != Nil) {
      fprintf(out, ", ");
      print_array_type_rec(env, out, cdr(body));
    }
    break;

  default:
    print_node(wffprint, out, body);
    break;
  }
}
