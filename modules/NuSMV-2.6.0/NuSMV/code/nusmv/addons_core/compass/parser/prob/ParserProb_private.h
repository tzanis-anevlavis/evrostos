/* ---------------------------------------------------------------------------


  This file is part of the ``parser.prob'' package of NuSMV version 2.
  Copyright (C) 2008 by FBK-irst.

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
  \brief  The private header file of ParserProb class.

  \todo: Missing description

*/


#ifndef __NUSMV_ADDONS_CORE_COMPASS_PARSER_PROB_PARSER_PROB_PRIVATE_H__
#define __NUSMV_ADDONS_CORE_COMPASS_PARSER_PROB_PARSER_PROB_PRIVATE_H__

#include "nusmv/addons_core/compass/parser/prob/ParserProb.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/node/node.h"

/*!
  \methodof ParserProb
  \todo
*/
void parser_prob_add(ParserProb_ptr self, node_ptr prob);
/*!
  \methodof ParserProb
  \todo
*/
node_ptr parser_prob_mk_prob(ParserProb_ptr self,
                             node_ptr assigns, node_ptr prob);

/*!
  \methodof ParserProb
  \todo
*/
node_ptr parser_prob_mk_var_assign(ParserProb_ptr self,
                                  node_ptr var, node_ptr val);

/*!
  \methodof ParserProb
  \todo
*/
node_ptr
parser_prob_mk_var_assigns(ParserProb_ptr self,
                           node_ptr left, node_ptr right);

/*!
  \methodof ParserProb
  \todo
*/
node_ptr
parser_prob_mk_dot(ParserProb_ptr self, node_ptr left, node_ptr right);

/*!
  \methodof ParserProb
  \todo
*/
node_ptr
parser_prob_mk_array(ParserProb_ptr self, node_ptr left, node_ptr right);

/*!
  \methodof ParserProb
  \todo
*/
node_ptr parser_prob_mk_atom(ParserProb_ptr self, const char* name);

/*!
  \methodof ParserProb
  \todo
*/
node_ptr parser_prob_mk_num(ParserProb_ptr self, const int num);

/*!
  \methodof ParserProb
  \todo
*/
node_ptr parser_prob_mk_real(ParserProb_ptr self, const char* real_text);

/*!
  \methodof ParserProb
  \todo
*/
node_ptr parser_prob_mk_true(ParserProb_ptr self);
/*!
  \methodof ParserProb
  \todo
*/
node_ptr parser_prob_mk_false(ParserProb_ptr self);


#endif /* __NUSMV_ADDONS_CORE_COMPASS_PARSER_PROB_PARSER_PROB_PRIVATE_H__ */
