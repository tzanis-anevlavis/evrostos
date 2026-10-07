/* ---------------------------------------------------------------------------


  This file is part of the ``parser.ord'' package of NuSMV version 2. 
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
  \brief  The private header file of ParserOrd class.

  \todo: Missing description

*/


#ifndef __NUSMV_CORE_PARSER_ORD_PARSER_ORD_PRIVATE_H__
#define __NUSMV_CORE_PARSER_ORD_PARSER_ORD_PRIVATE_H__

#include "nusmv/core/parser/ord/ParserOrd.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/node/node.h"

/*!
  \methodof ParserOrd
  \brief 

  
*/
void 
parser_ord_add_var(ParserOrd_ptr self, node_ptr var);

/*!
  \methodof ParserOrd
  \brief 

  
*/
node_ptr 
parser_ord_mk_atom(ParserOrd_ptr self, const char* name);

/*!
  \methodof ParserOrd
  \brief 

  
*/
node_ptr 
parser_ord_mk_bit(ParserOrd_ptr self, node_ptr l, int suffix);

/*!
  \methodof ParserOrd
  \brief 

  
*/
node_ptr 
parser_ord_mk_array(ParserOrd_ptr self, node_ptr l, node_ptr r);

/*!
  \methodof ParserOrd
  \brief 

  
*/
node_ptr 
parser_ord_mk_dot(ParserOrd_ptr self, node_ptr l, node_ptr r);

/*!
  \methodof ParserOrd
  \brief 

  
*/
node_ptr parser_ord_mk_num(ParserOrd_ptr self, const int num);


#endif /* __NUSMV_CORE_PARSER_ORD_PARSER_ORD_PRIVATE_H__ */
