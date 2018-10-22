/***************************************************************  -*-C-*-  ***/
%{
/**CFile***********************************************************************

  FileName    [ap_input.l]

  PackageName [compass.parser.ap]

  Synopsis    [Lexical analyzer for the atomic proposition input language]

  SeeAlso     [ap_grammar.y]

  Author      [Marco Roveri]

  Copyright   [
  This file is part of the ``compass.parser.ap'' package of NuSMV version 2.
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

#if HAVE_CONFIG_H
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv-config.h"
#endif

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/WordNumberMgr.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/addons_core/compass/parser/ap/apInt.h"
#include "nusmv/core/parser/parser.h"

#include "nusmv/addons_core/compass/parser/ap/ap_grammar.h" /* For yacc/lex interaction */
#include "nusmv/addons_core/compass/parser/ap/ParserAp.h"
#include "nusmv/addons_core/compass/parser/ap/ParserAp_private.h"
/* The below code is only meaningful for flex version 2.5.31 which is
   buggy since it does not define the yy_prev_more_offset static
   variable */
#if defined YY_FLEX_SUBMINOR_VERSION &&         \
  defined YY_FLEX_MAJOR_VERSION &&              \
  defined YY_FLEX_MINOR_VERSION
#if YY_FLEX_SUBMINOR_VERSION == 31 &&           \
  YY_FLEX_MINOR_VERSION == 5 &&                 \
  YY_FLEX_MAJOR_VERSION == 2
static int yy_prev_more_offset = 0;
#endif
#endif


#if ! NUSMV_HAVE_ISATTY
static inline int isatty(int d) { return 0; }
#endif

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/* The parser instance used by flex/bison generated code */
static ParserAp_ptr _parser = PARSER_AP(NULL);

void parser_ap_set_global_parser(ParserAp_ptr parser)
{
  nusmv_assert(_parser == PARSER_AP(NULL));
  _parser = parser;
}

void parser_ap_reset_global_parser(ParserAp_ptr parser)
{
  PARSER_AP_CHECK_INSTANCE(_parser);
  nusmv_assert(parser == _parser);
  _parser = PARSER_AP(NULL);
}

ParserAp_ptr parser_ap_get_global_parser(void)
{
   PARSER_AP_CHECK_INSTANCE(_parser);
   return _parser;
}


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static int nusmv_skip_comment(void);
static int nusmv_skip_multiline_comment(void);

/* The following are needed in order to allow parsing of long
   comments, that otherwise will cause errors since flex thinks that
   it is using REJECT even if it isn't.  The "%option noreject" seems
   to be ignored in the latest (and previous) versions of flex.
*/
#ifdef YY_USES_REJECT
#undef YY_USES_REJECT
#endif

%}
%a	15000
%o	25000

%option pointer
%option yylineno
%option noreject
%option prefix="parser_ap_"
     /* %option outfile="lex.yy.c" */


%%
[ \n\t\r\f]             ;
"--"                    nusmv_skip_comment();
"/--"                   nusmv_skip_multiline_comment();
"("                     return(TOK_LP);
")"                     return(TOK_RP);
"["                     return(TOK_LB);
"]"                     return(TOK_RB);
"{"                     return(TOK_LCB);
"}"                     return(TOK_RCB);
"FALSE"                 {
                                const ParserAp_ptr ap = parser_ap_get_global_parser();
                                const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(ap));
                                const NodeMgr_ptr nodemgr =
                                  NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
                                parser_ap_lval.node = new_node(nodemgr, FALSEEXP,Nil,Nil);
                                return(TOK_FALSEEXP);
                        }
"TRUE"                  {
                                const ParserAp_ptr ap = parser_ap_get_global_parser();
                                const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(ap));
                                const NodeMgr_ptr nodemgr =
                                  NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
                                parser_ap_lval.node = new_node(nodemgr, TRUEEXP,Nil,Nil);
                                return(TOK_TRUEEXP);
                        }
"case"                  return(TOK_CASE);
"esac"                  return(TOK_ESAC);
"+"                     return(TOK_PLUS);
"-"                     return(TOK_MINUS);
"*"                     return(TOK_TIMES);
"/"                     return(TOK_DIVIDE);
"mod"                   return(TOK_MOD);
"<<"                    return(TOK_LSHIFT);
">>"                    return(TOK_RSHIFT);
"<<<"                   return(TOK_LROTATE);
">>>"                   return(TOK_RROTATE);
"="                     return(TOK_EQUAL);
"!="                    return(TOK_NOTEQUAL);
"<="                    return(TOK_LE);
">="                    return(TOK_GE);
"<"                     return(TOK_LT);
">"                     return(TOK_GT);
"next"                  return(TOK_NEXT);
"init"                  return(TOK_SMALLINIT);
"self"                  return(TOK_SELF);
"union"                 return(TOK_UNION);
"in"                    return(TOK_SETIN);
".."                    return(TOK_TWODOTS);
"."                     return(TOK_DOT);
"->"                    return(TOK_IMPLIES);
"<->"                   return(TOK_IFF);
"|"                     return(TOK_OR);
"&"                     return(TOK_AND);
"xor"                   return(TOK_XOR);
"xnor"                  return(TOK_XNOR);
"!"                     return(TOK_NOT);
","                     return(TOK_COMMA);
":"                     return(TOK_COLON);
";"                     return(TOK_SEMI);
"::"                    return(TOK_CONCATENATION);
 /* word constants */
"0"([us])?[bBoOdDhH][0-9]*"_"[0-9a-fA-F][0-9a-fA-F_]* {
  const NuSMVEnv_ptr env =
    EnvObject_get_environment(ENV_OBJECT(parser_ap_get_global_parser()));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  char* errorString;
  WordNumber_ptr wordConstant
    = WordNumberMgr_parsed_string_to_word_number(words, parser_ap_text, &errorString);
  /* check for errors */
  if (WORD_NUMBER(NULL) == wordConstant) ErrorMgr_rpterr(errmgr, "%s", errorString);
  parser_ap_lval.node = new_node(nodemgr, 's' == parser_ap_text[1]
                                 ? NUMBER_SIGNED_WORD : NUMBER_UNSIGNED_WORD,
                                 (node_ptr)wordConstant,Nil);
  return(TOK_NUMBER_WORD);
}

 /* real, fractional and exponential constants */
[fF]"'"[0-9]+"/"[0-9]+  {
                  const NuSMVEnv_ptr env =
                    EnvObject_get_environment(ENV_OBJECT(parser_ap_get_global_parser()));
                  const UStringMgr_ptr strings =
                    USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
                  const NodeMgr_ptr nodemgr =
                    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

                  parser_ap_text[0] = 'f';
                  parser_ap_lval.node = new_node(nodemgr, NUMBER_FRAC,
                                         (node_ptr)UStringMgr_find_string(strings, parser_ap_text),Nil);
                  return(TOK_NUMBER_FRAC);
                 }

[0-9]+("."[0-9]+)?[eE][+-]?[0-9]+ {
                  const NuSMVEnv_ptr env =
                    EnvObject_get_environment(ENV_OBJECT(parser_ap_get_global_parser()));
                  const UStringMgr_ptr strings =
                    USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
                  const NodeMgr_ptr nodemgr =
                    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

                  parser_ap_lval.node = new_node(nodemgr, NUMBER_EXP,
                                         (node_ptr)UStringMgr_find_string(strings, parser_ap_text),Nil);
                  return(TOK_NUMBER_EXP);
                 }

[0-9]+"."[0-9]+  {
                  const NuSMVEnv_ptr env =
                    EnvObject_get_environment(ENV_OBJECT(parser_ap_get_global_parser()));
                  const UStringMgr_ptr strings =
                    USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
                  const NodeMgr_ptr nodemgr =
                    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

                    parser_ap_lval.node = new_node(nodemgr, NUMBER_REAL,
                                           (node_ptr)UStringMgr_find_string(strings, parser_ap_text),Nil);
                    return(TOK_NUMBER_REAL);
                 }

 /* integer number */
[0-9]+           {
                  const NuSMVEnv_ptr env =
                    EnvObject_get_environment(ENV_OBJECT(parser_ap_get_global_parser()));
                  const NodeMgr_ptr nodemgr =
                    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

                  int i;
                  sscanf(parser_ap_text,"%d",&i);
                  parser_ap_lval.node = new_node(nodemgr, NUMBER, NODE_FROM_INT(i), Nil);
                  return(TOK_NUMBER);
                 }

 /* identifier */
[A-Za-z_][A-Za-z0-9_\$#-]*  {
                  const NuSMVEnv_ptr env =
                    EnvObject_get_environment(ENV_OBJECT(parser_ap_get_global_parser()));
                  const UStringMgr_ptr strings =
                    USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
                  const NodeMgr_ptr nodemgr =
                    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

                  parser_ap_lval.node = new_node(nodemgr, ATOM,
                                                 (node_ptr)UStringMgr_find_string(strings, parser_ap_text),Nil);
                  return(TOK_ATOM);
                 }
\"[^\"]*\"       {
                  const NuSMVEnv_ptr env =
                    EnvObject_get_environment(ENV_OBJECT(parser_ap_get_global_parser()));
                  const UStringMgr_ptr strings =
                    USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
                  const NodeMgr_ptr nodemgr =
                    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

                  parser_ap_lval.node = new_node(nodemgr, ATOM,(node_ptr)UStringMgr_find_string(strings, parser_ap_text),Nil);
                  return(TOK_ATOM);
                 } /* Unused token */

.                {
                   const NuSMVEnv_ptr env =
                     EnvObject_get_environment(ENV_OBJECT(parser_ap_get_global_parser()));
                   const ErrorMgr_ptr errmgr =
                     ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

                   ErrorMgr_rpterr(errmgr, "illegal character : \"%s\"", parser_ap_text);
                 }
%%

static int nusmv_skip_comment() 
{
#ifdef __cplusplus
  return Parser_skip_one_line_comment(yyinput);
#else
  return Parser_skip_one_line_comment(input);
#endif
}

static int nusmv_skip_multiline_comment()
{
#ifdef __cplusplus
  return Parser_skip_multiline_comment(yyinput);
#else
  return Parser_skip_multiline_comment(input);
#endif
}
