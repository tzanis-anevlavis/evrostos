%{
/**CFile***********************************************************************

  FileName    [prob_grammar.y]

  PackageName [parser.prob]

  Synopsis    [Yacc for probability input file]

  SeeAlso     [prob_input.l]

  Author      [Roberto Cavada]

  Copyright   [
  This file is part of the ``compass.parser.prob'' package of NuSMV version 2.
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

#include "nusmv/addons_core/compass/parser/prob/ParserProb.h"
#include "nusmv/addons_core/compass/parser/prob/ParserProb_private.h"
#include "nusmv/addons_core/compass/parser/prob/probInt.h"

#include "nusmv/core/node/node.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/utils.h"


void parser_prob_error(char *s);
int parser_prob_wrap(void);

%}

%union {
  node_ptr node;
}

/*
  All of the terminal grammar symbols (tokens recognized by the
  lexical analyzer) Note: all binary operators associate from left
  to right, operators are listed from lowest to highest priority

  Note: The following token are not used inside the grammar, but
  are used by other modules inside the system (i.e. the compiler,
  mc).  CONTEXT EU AU EBU ABU MINU MAXU CONS OVER BIT
*/

%left TK_EQUAL TK_COLON
%left TK_LB TK_RB
%left <node> TK_ATOM TK_NUMBER TK_REAL TK_TRUE TK_FALSE
%left TK_AND TK_DOT TK_SEMI

/* all nonterminals return a parse tree node */
%type <node> identifier  constant  identifier_or_constant  main_identifier
%type <node> prob_list_item  prob_list  prob_value  var_assign  var_assigns

%start begin
%%
begin         : prob_list { }
              ;

/* Repetition of variables */
prob_list     : {}
             | prob_list_item prob_list
             {
               parser_prob_add(parser_prob_get_global_parser(), $1);
             }
;

prob_list_item : var_assigns TK_COLON prob_value optsemi
{
  $$ = parser_prob_mk_prob(parser_prob_get_global_parser(), $1, $3);
}
;

var_assigns : var_assign
| var_assigns TK_AND var_assign
{ $$ = parser_prob_mk_var_assigns(parser_prob_get_global_parser(), $3, $1); }
;

var_assign : main_identifier TK_EQUAL identifier_or_constant
{
  $$ = parser_prob_mk_var_assign(parser_prob_get_global_parser(), $1, $3);
}
;

main_identifier : TK_ATOM
{
  $$ = parser_prob_mk_dot(parser_prob_get_global_parser(), Nil, $1);
}

| main_identifier TK_LB TK_NUMBER TK_RB
{
  $$ = parser_prob_mk_array(parser_prob_get_global_parser(), $1, $3);
}

| main_identifier TK_DOT identifier
{
  $$ = parser_prob_mk_dot(parser_prob_get_global_parser(),
                         $1, $3);
}
;

identifier_or_constant :
   identifier
   | constant
   ;

identifier   : TK_ATOM
| identifier TK_LB TK_NUMBER TK_RB
{
  $$ = parser_prob_mk_array(parser_prob_get_global_parser(),
                           $1, $3);
}
| identifier TK_DOT identifier
{
  $$ = parser_prob_mk_dot(parser_prob_get_global_parser(),
                         $1, $3);
}
| identifier TK_DOT TK_NUMBER
{
  $$ = parser_prob_mk_dot(parser_prob_get_global_parser(),
                         $1, $3);
}
;

prob_value
: TK_NUMBER
| TK_REAL
| identifier
;

constant
: TK_NUMBER
| TK_TRUE
| TK_FALSE
;


/* parse an optional semicolon */
optsemi : | TK_SEMI {};


%%


/* Additional source code */
void parser_prob_error(char *s)
{
  const ParserProb_ptr ap = parser_prob_get_global_parser();
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(ap));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  extern char* parser_prob_text;

  StreamMgr_print_error(streams, "\n");
  if (get_output_order_file(opts)) {
    StreamMgr_print_error(streams,  "file %s: ", get_output_order_file(opts));
  }
  else {
    StreamMgr_print_error(streams,  "file stdin: ");
  }

  if (parser_prob_lineno) {
    StreamMgr_print_error(streams,  "line %d: ", parser_prob_lineno);
  }

  StreamMgr_print_error(streams,  "at token \"%s\": %s\n", parser_prob_text, s);
  if (opt_batch(opts)) {
    /* exits the execution */
    StreamMgr_print_error(streams,  "\n");
    ErrorMgr_print_io_atom_stack(errmgr);
    ErrorMgr_nusmv_exit(errmgr, 1);
  }
}

int parser_prob_wrap(void)  { return 1; }


