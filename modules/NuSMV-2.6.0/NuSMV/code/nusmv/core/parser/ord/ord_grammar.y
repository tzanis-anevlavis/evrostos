%{
/**CFile***********************************************************************

  FileName    [ord_grammar.y]

  PackageName [parser.ord]

  Synopsis    [Yacc for variable ordering parser]

  SeeAlso     [input.l]

  Author      [Roberto Cavada]

  Copyright   [
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

  To contact the NuSMV development board, email to <nusmv@fbk.eu>. ]

******************************************************************************/

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/utils/StreamMgr.h"
#include <setjmp.h>

#if NUSMV_HAVE_MALLOC_H
# if NUSMV_HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif  
# include <malloc.h>
#elif defined(NUSMV_HAVE_SYS_MALLOC_H) && NUSMV_HAVE_SYS_MALLOC_H
# if NUSMV_HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif  
# include <sys/malloc.h>
#elif NUSMV_HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "nusmv/core/parser/ord/ParserOrd.h"
#include "nusmv/core/parser/ord/ParserOrd_private.h"
#include "nusmv/core/parser/ord/ordInt.h"

#include "nusmv/core/node/node.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/utils.h"
static boolean _is_main_var = true;

int parser_ord_wrap(void);

void parser_ord_error(char *s);

%}

%union {
  node_ptr node;
}

/*
  All of the terminal grammar symbols (tokens recognized by the lexical analyzer) 
  Note: all binary operators associate from left to right,  operators are
        listed from lowest to highest priority 

  Note: The following token are not used inside the grammar, but are
  used by other modules inside the system (i.e. the compiler, mc).
  CONTEXT EU AU EBU ABU MINU MAXU
  CONS OVER BIT
*/

%left LB RB
%left <node> ATOM NUMBER 
%left DOT MINUS

/* all nonterminals return a parse tree node */
%type <node> var_id  vars_list_item  vars_list


%start begin
%%
begin         : vars_list { }
              ;

/* Repetition of variables */
vars_list     : {}
              | vars_list_item vars_list 
                {
                  parser_ord_add_var(parser_ord_get_global_parser(), $1);
                }
              ;

vars_list_item : { _is_main_var = true; } var_id { $$ = $2; }
                 ;


var_id      :
            ATOM {
              if (_is_main_var) {
                $$ = parser_ord_mk_dot(parser_ord_get_global_parser(), 
                                       Nil, $1);
                _is_main_var = false;
              }
              else {
                $$ = $1;
              }
            }
            | var_id DOT NUMBER
            {
              $$ = parser_ord_mk_bit(parser_ord_get_global_parser(),
                                     $1, NODE_TO_INT(car($3)));
            }
            | var_id LB NUMBER RB 
                   {
                     $$ = parser_ord_mk_array(parser_ord_get_global_parser(), 
                                              $1, $3);
                   }
            | var_id LB MINUS NUMBER RB 
                   {
                     int i = node_get_int($4);
                     node_ptr num = parser_ord_mk_num(parser_ord_get_global_parser(), 
                                                      -i);
                     $$ = parser_ord_mk_array(parser_ord_get_global_parser(), 
                                              $1, num);

                   }
            | var_id DOT var_id
                   {
                     $$ = parser_ord_mk_dot(parser_ord_get_global_parser(), 
                                            $1, $3);
                   }
              ;

%%

/* Additional source code */
void parser_ord_error(char *s)
{
  extern char* parser_ord_text;

  ParserOrd_ptr parser = parser_ord_get_global_parser();
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(parser));
  const OptsHandler_ptr options =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  StreamMgr_print_error(streams, "\n");
  if (get_output_order_file(options)) {
    StreamMgr_print_error(streams,  "file %s: ", get_output_order_file(options));
  }
  else {
    StreamMgr_print_error(streams,  "file stdin: ");
  }

  if (parser_ord_lineno) {
    StreamMgr_print_error(streams,  "line %d: ", parser_ord_lineno);
  }

  StreamMgr_print_error(streams,  "at token \"%s\": %s\n", parser_ord_text, s);
  if (opt_batch(options)) {
    /* exits the execution */
    StreamMgr_print_error(streams,  "\n");
    ErrorMgr_print_io_atom_stack(errmgr);
    ErrorMgr_nusmv_exit(errmgr, 1);
  }
}

int parser_ord_wrap(void)  { return 1; }


