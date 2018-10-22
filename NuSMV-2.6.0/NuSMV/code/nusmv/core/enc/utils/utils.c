/* ---------------------------------------------------------------------------


  This file is part of the ``enc.utils'' package of NuSMV version 2.
  Copyright (C) 2005 by FBK-irst.

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
  \brief Utilities for encodings stuff

  \todo: Missing description

*/


#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/enc/encInt.h"
#include "nusmv/core/parser/ord/ParserOrd.h"
#include "nusmv/core/compile/symb_table/SymbTable.h"

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/utils/ucmd.h"
#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

OrdGroups_ptr
enc_utils_parse_ordering_file(const NuSMVEnv_ptr env,
                              const char* order_filename,
                              const BoolEnc_ptr bool_enc)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  OrdGroups_ptr groups;

  if (!util_is_string_null(order_filename)) {
    ParserOrd_ptr parser;
    FILE* f;

    /* Parses the provided ordering file */
    parser = ParserOrd_create(env);
    f = fopen(order_filename, "r");
    if (f == (FILE*) NULL) {
      ErrorMgr_error_file_not_found(errmgr, order_filename);
    }
    /* parse the ordering file */
    ParserOrd_parse_from_file(parser, f);

    groups = enc_utils_create_vars_ord_groups(bool_enc,
                                              ParserOrd_get_vars_list(parser));
    fclose(f);
    ParserOrd_destroy(parser);
  }
  else {
    groups = OrdGroups_create();
  }
  return groups;
}

OrdGroups_ptr enc_utils_create_vars_ord_groups(BoolEnc_ptr bool_enc,
                                               NodeList_ptr vars)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(bool_enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  OrdGroups_ptr groups;
  SymbTable_ptr symb_table;
  ListIter_ptr iter;

  groups = OrdGroups_create();

  symb_table = BaseEnc_get_symb_table(BASE_ENC(bool_enc));

  /* Iterates on the list of varianles in vars:

     If the symbol is a bool var: append to the ord list B.
     If the symbol is a scalar var:
       Iterates on the encoding, adding any bool var that do not
       belong both to A and B.

     Any duplicated symbol will be warned, as well as any non-defined
     symbol.

     After that, any remaining defined boolean var will be added.
  */

  NODE_LIST_FOREACH(vars, iter) {
    node_ptr name = NodeList_get_elem_at(vars, iter);

    if (!SymbTable_is_symbol_var(symb_table, name)) {
      ErrorMgr_warning_variable_not_declared(errmgr, name);
      continue;
    }

    if (SymbTable_is_symbol_bool_var(symb_table, name)) {
      int gr = OrdGroups_get_var_group(groups, name);
      if (gr == -1) {
        gr = OrdGroups_create_group(groups);
        OrdGroups_add_variable(groups, name, gr);
      }
      else ErrorMgr_warning_var_appear_twice_in_order_file(errmgr, name);
    }
    else {
      /* Variable is scalar. If one or more bits of that scalar var
         have been previously specified in the ordering file, than
         the single bits that belong to the scalar variable will NOT
         be grouped.  If no bit has been previously specified, then
         all the bits of the scalar var that have not been possibly
         specified in the ordering file will be grouped */
      NodeList_ptr bits;
      ListIter_ptr bits_iter;
      boolean grouped = true;
      int group = -1;

      /* Searches any previously specified bit */
      bits = BoolEnc_get_var_bits(bool_enc, name);
      NODE_LIST_FOREACH(bits, bits_iter) {
        node_ptr bit = NodeList_get_elem_at(bits, bits_iter);

        if (OrdGroups_get_var_group(groups, bit) != -1) {
          grouped = false;
          break;
        }
      }

      /* adds all bits that do not occur in the ordering file,
         either grouping or not depending on specific flag: */
      NODE_LIST_FOREACH(bits, bits_iter) {
        node_ptr bit = NodeList_get_elem_at(bits, bits_iter);

        if (-1 == group || !grouped) {
          group = OrdGroups_create_group(groups);
        }
        if (!NodeList_belongs_to(vars, bit) &&
            (-1 == OrdGroups_get_var_group(groups, bit))) {
          OrdGroups_add_variable(groups, bit, group);
        }
      }

      NodeList_destroy(bits);
    } /* scalar case */
  } /* loop on variables */

  return groups;
}
