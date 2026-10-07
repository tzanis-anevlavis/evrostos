/**CHeaderFile*****************************************************************

  FileName    [apInt.h]

  PackageName [compass.parser.ap]

  Synopsis    [The internal header file the package]

  Description []

  SeeAlso     []

  Author      [Marco Roveri]

  Copyright   [
  This file is part of the ``parser.ord'' package of NuSMV version 2. 
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
  Please report bugs to <nusmv-users@fbk.eu>.]
******************************************************************************/

#ifndef __NUSMV_ADDONS_CORE_COMPASS_PARSER_AP_AP_INT_H__
#define __NUSMV_ADDONS_CORE_COMPASS_PARSER_AP_AP_INT_H__

#include "nusmv/addons_core/compass/parser/ap/ParserAp.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/opt/opt.h"


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
#ifndef YY_TYPEDEF_YY_BUFFER_STATE
#define YY_TYPEDEF_YY_BUFFER_STATE
typedef struct yy_buffer_state* YY_BUFFER_STATE;
#endif


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

extern int parser_ap_lineno;
extern FILE* parser_ap_in;


/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

void parser_ap_set_global_parser(ParserAp_ptr parser);
void parser_ap_reset_global_parser(ParserAp_ptr parser);
ParserAp_ptr parser_ap_get_global_parser(void);


/* from generated code: */
int parser_ap_lex(void);
int parser_ap_parse(void);
void parser_ap_restart(FILE* input_file);

void 
parser_ap__switch_to_buffer(YY_BUFFER_STATE new_buffer);

YY_BUFFER_STATE 
parser_ap__create_buffer(FILE* file, int size);

void parser_ap__delete_buffer(YY_BUFFER_STATE buf);

YY_BUFFER_STATE 
parser_ap__scan_string(const char* str);


#endif /* __NUSMV_ADDONS_CORE_COMPASS_PARSER_AP_AP_INT_H__ */
