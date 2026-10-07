/* ---------------------------------------------------------------------------


  This file is part of the ``enc'' package of NuSMV version 2.
  Copyright (C) 2003 by FBK-irst.

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
  \brief Internal API for the enc package

  \todo: Missing description

*/


#ifndef __NUSMV_CORE_ENC_ENC_INT_H__
#define __NUSMV_CORE_ENC_ENC_INT_H__

#include "nusmv/core/enc/utils/OrdGroups.h"
#include "nusmv/core/enc/bool/BoolEnc.h"

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/dd/dd.h"

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure definitions                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

extern int nusmv_yylineno;
extern node_ptr boolean_range;



/*---------------------------------------------------------------------------*/
/* Functions declarations                                                    */
/*---------------------------------------------------------------------------*/

/*!
  \brief Parses the given ordering file, and produces an OrdGroups
  instance.

  The returned instance belongs to the caller. It is a
  caller's responsability to destroy it. order_filename can be NULL
*/
OrdGroups_ptr
enc_utils_parse_ordering_file(const NuSMVEnv_ptr env,
                              const char* order_filename,
                              const BoolEnc_ptr bool_enc);

/*!
  \brief Given a boolean variable or a bit the function inserts it
   into the sorted list at proper position

  
   The higher bits are added to the beginning of the
   list and lower bits are added at the end. The boolean variables
   are added at the beginning of the list before the bits.

   A new element is added at the end of the group of equal elements,
   e.g. a boolean var is added after existing boolean vars but before
   the bit vars.

   Parameter 'sorting_cache' is used to speed up insertion (sorting a
   list will be linear instead of quadratic).  Initially
   'sorting_cache' has to point to a pointer which points to Nil and
   sorted_list has to be an empty list. It is the invoker
   responsibility to free the sorted list and cache (with free_list)
   after last invoking Enc_append_bit_to_sorted_list (the same
   sorting_cache and sorted_list can be used for several runs of this
   function).
   
*/
void Enc_append_bit_to_sorted_list(SymbTable_ptr symb_table,
                                          NodeList_ptr sorted_list,
                                          node_ptr var,
                                          node_ptr* sorting_cache);

/*!
  \brief Given a list of variables representing a new variable ordering,
  produces an OrdGroups instance.

  The returned instance belongs to the caller. It is a
  caller's responsability to destroy it.
*/
OrdGroups_ptr
enc_utils_create_vars_ord_groups(BoolEnc_ptr bool_enc,
                                 NodeList_ptr vars);

#endif /* __NUSMV_CORE_ENC_ENC_INT_H__ */
