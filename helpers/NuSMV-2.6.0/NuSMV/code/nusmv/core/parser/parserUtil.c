/* ---------------------------------------------------------------------------


  This file is part of the ``parser'' package of NuSMV version 2.
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
  \brief Parser utilities

  This file contains some parser utilities that allows
  for example to parse from a string, instead that from a file.

*/


#if HAVE_CONFIG_H
#  include "nusmv-config.h"
#endif

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"

#include "nusmv/core/parser/parserInt.h"
#include "nusmv/core/parser/symbols.h"

#include "nusmv/core/parser/psl/pslInt.h"

#include "nusmv/core/cinit/cinit.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/ustring.h"

/*---------------------------------------------------------------------------*/
/* Macro definitions                                                         */
/*---------------------------------------------------------------------------*/

#define YY_BUF_SIZE 16384
#define YY_CURRENT_BUFFER nusmv_yy_current_buffer
#define YY_END_OF_BUFFER_CHAR 0

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_ERROR_REVERSED "parsUtilErrRev"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_PARSED_ERRORS "parsUtilParsErr"

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
/* TODO[REAMa] NOT REENTRANT (But the parser is not reentrant too) */
static char* tmpfname1 = (char*) NULL;
/* TODO[REAMa] NOT REENTRANT (But the parser is not reentrant too) */
static char* tmpfname2 = (char*) NULL;

/* the current buffer of the lexer - only one buffer can exist at any time*/
/* TODO[REAMa] NOT REENTRANT */
static YY_BUFFER_STATE nusmv_yy_current_buffer = NULL;

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void parser_open_input_pp(const NuSMVEnv_ptr env, const char* filename);
static void parser_close_input_pp(const NuSMVEnv_ptr env);


/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief Open a file and inform the parser to read from it

  Open a file and inform the parser to start
  reading tokens from this file. If no input file is provided, then it
  inform the parser to start reading tokens from the standard input.
  Invoke Parser_CloseInput to close the file and associated buffer.

  \sa Parser_CloseInput
*/

extern FILE* psl_yyin;

void Parser_OpenInput(const NuSMVEnv_ptr env, const char *filename)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (filename != (char*) NULL) {
    nusmv_yyin = fopen(filename,"r");
    if (nusmv_yyin == (FILE*) NULL) ErrorMgr_rpterr(errmgr, "cannot open input file %s",filename);
    nusmv_yylineno = 1;
  }
  else {
    nusmv_yyin = stdin;
    nusmv_yylineno = 0;
  }

  psl_yyin = nusmv_yyin;

  /* buffer has not been initialized before: */
  nusmv_assert(NULL == YY_CURRENT_BUFFER);
  YY_CURRENT_BUFFER = nusmv_yy_create_buffer(nusmv_yyin, YY_BUF_SIZE);
  /* Flushes the current input buffer */
  (void) nusmv_yy_switch_to_buffer(YY_CURRENT_BUFFER);

  (void) nusmv_yyrestart(nusmv_yyin);
}

void Parser_CloseInput(void)
{
  nusmv_assert(NULL != YY_CURRENT_BUFFER);/* buffer should be initialized before */
  nusmv_yy_delete_buffer(YY_CURRENT_BUFFER);
  YY_CURRENT_BUFFER = NULL;

  if (stdin != nusmv_yyin) fclose(nusmv_yyin);
}

void Parser_switch_to_psl()
{
  psl_yyrestart(psl_yyin);
}


void Parser_switch_to_smv()
{
  nusmv_yyrestart(nusmv_yyin);
}

int Parser_ReadSMVFromFile(NuSMVEnv_ptr env, const char *filename)
{
  int retval = 0;

  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  extern NuSMVEnv_ptr __nusmv_parser_env__;
  extern NuSMVEnv_ptr __psl_parser_env__;

  if (strcmp(get_pp_list(opts), "") != 0) parser_open_input_pp(env, filename);
  else Parser_OpenInput(env, filename);

  __nusmv_parser_env__ = env;
  __psl_parser_env__ = env;

  parsed_tree = Nil;
  parser_free_parsed_syntax_errors(env);
  parse_mode_flag = PARSE_MODULES;

  CATCH(errmgr) {
    if (nusmv_yyparse()) retval = 1;
    else {
      nusmv_yylineno = 0;
    }
  }
  FAIL(errmgr) {
    if (strcmp(get_pp_list(opts), "") != 0) parser_close_input_pp(env);
    else Parser_CloseInput();
    ErrorMgr_rpterr(errmgr, "Parser error");
  }

  if (strcmp(get_pp_list(opts), "") != 0) parser_close_input_pp(env);
  else Parser_CloseInput();

  return retval;
}

int Parser_ReadCmdFromString(NuSMVEnv_ptr env, int argc, const char** argv,
                             const char* head, const char* tail,
                             node_ptr* pc)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  int i;
  char * old_input_file;
  int l;
  int status = 0;
  char* cmd = NIL(char);
  char* cmd1 = NIL(char);
  YY_BUFFER_STATE buf;
  extern NuSMVEnv_ptr __nusmv_parser_env__;
  extern NuSMVEnv_ptr __psl_parser_env__;
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  l = strlen(head);

  /* Overflow? int is smaller than size_t */
  nusmv_assert(l >= 0);
  nusmv_assert(l == strlen(head));

  for (i = 1; i < argc; i++) l += strlen(argv[i]) + 1;
  l += strlen(tail) +2+1; /* 2 is for last two YY_END_OF_BUFFER_CHAR */
  cmd = ALLOC(char, l);
  cmd1 = ALLOC(char, l);
  nusmv_assert(cmd != NIL(char));
  nusmv_assert(cmd1 != NIL(char));

  sprintf(cmd, "%s", head);
  for (i = 1; i < argc; i++) {
    sprintf(cmd1, "%s%s ", cmd, argv[i]);
    strcpy(cmd, cmd1);
  }
  sprintf(cmd1, "%s%s%c%c", cmd, tail,
          YY_END_OF_BUFFER_CHAR, YY_END_OF_BUFFER_CHAR);

  if (opt_verbose_level_gt(opts, 3))
    StreamMgr_print_error(streams,  "%s\n", cmd1);

  /* Get a local copy of the old input file. */
  old_input_file = get_input_file(opts);
  if (NIL(char) != old_input_file) {
    old_input_file = util_strsav(old_input_file);
  }
  set_input_file(opts, "<command-line>");

  __nusmv_parser_env__ = env;
  __psl_parser_env__ = env;

  parsed_tree = Nil;
  parser_free_parsed_syntax_errors(env);
  parse_mode_flag = PARSE_COMMAND;
  buf = nusmv_yy_scan_buffer(cmd1, l-1);
  nusmv_assert(buf != (YY_BUFFER_STATE) NULL);

  status = (nusmv_yyparse() != 0);

  nusmv_yy_delete_buffer(buf); /* frees the buffer */
  FREE(cmd);
  FREE(cmd1);

  /* We need to reset the input buffer */
  set_input_file(opts, old_input_file);

  /* Free the local copy of the input file */
  if (NIL(char) != old_input_file) {
    FREE(old_input_file);
  }

  *pc = parsed_tree;
  return(status);
}

int Parser_ReadCmdFromFile(NuSMVEnv_ptr env,
                           const char *filename, node_ptr* res)
{
  int status;
  char * old_input_file;

  /* TODO[REAMa] Change when the parser will be reentrant */
  extern NuSMVEnv_ptr __nusmv_parser_env__;
  extern NuSMVEnv_ptr __psl_parser_env__;
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  __nusmv_parser_env__ = env;
  __psl_parser_env__ = env;

  parsed_tree = Nil;
  parser_free_parsed_syntax_errors(env);
  parse_mode_flag = PARSE_COMMAND;

  old_input_file = get_input_file(opts);
  if (NIL(char) != old_input_file) {
    old_input_file = util_strsav(old_input_file);
  }
  set_input_file(opts,
                 (char*)(NULL == filename ? "<stdin>" : filename));

  Parser_OpenInput(env, filename);
  status = nusmv_yyparse();
  Parser_CloseInput();

  *res = parsed_tree;

  set_input_file(opts, old_input_file);
  if (NIL(char) != old_input_file) {
    FREE(old_input_file);
  }

  return status;
}

int Parser_ReadNextExprFromFile(NuSMVEnv_ptr env,
                                const char *filename, node_ptr* res)
{
  return Parser_ReadCmdFromFile(env, filename, res);
}

int Parser_ReadSimpExprFromString(NuSMVEnv_ptr env,
                                  const char* str_expr, node_ptr* res)
{
  const char* argv[2];
  const char* head = "SIMPWFF ";
  const char* tail =  ";\n";

  /* prepare argv for parsing */
  argv[0] = (const char*) NULL;
  argv[1] = str_expr;

  *res = Nil;
  return Parser_ReadCmdFromString(env, sizeof(argv)/sizeof(argv[0]), argv,
                                  head, tail, res);
}

int Parser_ReadNextExprFromString(NuSMVEnv_ptr env,
                                  const char* str_expr, node_ptr* res)
{
  const char* argv[2];
  const char* head = "NEXTWFF ";
  const char* tail =  ";\n";

  /* prepare argv for parsing */
  argv[0] = (const char*) NULL;
  argv[1] = str_expr;

  *res = Nil;
  return Parser_ReadCmdFromString(env, sizeof(argv)/sizeof(argv[0]), argv,
                                  head, tail, res);
}

int Parser_ReadTypeFromString(NuSMVEnv_ptr env,
                              const char* str_type, node_ptr* res)
{
  const char* argv[2];
  const char* head = "ITYPE ";
  const char* tail =  ";\n";

  /* prepare argv for parsing */
  argv[0] = (const char*) NULL;
  argv[1] = str_type;

  *res = Nil;
  return Parser_ReadCmdFromString(env, sizeof(argv)/sizeof(argv[0]), argv,
                                  head, tail, res);
}

int Parser_ReadIdentifierExprFromString(NuSMVEnv_ptr env,
                                        const char* str_expr, node_ptr* res)
{
  const char* argv[2];
  const char* head = "COMPID ";
  const char* tail =  ";\n";

  /* prepare argv for parsing */
  argv[0] = (const char*) NULL;
  argv[1] = str_expr;

  *res = Nil;
  return Parser_ReadCmdFromString(env, sizeof(argv)/sizeof(argv[0]), argv,
                                  head, tail, res);
}

int Parser_ReadLtlExprFromFile(NuSMVEnv_ptr env, const char *filename)
{
  int retval;
  char * old_input_file;
  extern NuSMVEnv_ptr __nusmv_parser_env__;
  extern NuSMVEnv_ptr __psl_parser_env__;
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  __nusmv_parser_env__ = env;
  __psl_parser_env__ = env;

  parsed_tree = Nil;
  parser_free_parsed_syntax_errors(env);
  parse_mode_flag = PARSE_LTL_EXPR;

  old_input_file = get_input_file(opts);
  if (NIL(char) != old_input_file) {
    old_input_file = util_strsav(old_input_file);
  }

  set_input_file(opts,
                 (char*)(NULL == filename ? "<stdin>" : filename));

  Parser_OpenInput(env, filename);
  retval = nusmv_yyparse();
  Parser_CloseInput();

  set_input_file(opts, old_input_file);

  if (NIL(char) != old_input_file) {
    FREE(old_input_file);
  }

  return retval;
}

int Parser_read_psl_from_string(const NuSMVEnv_ptr env,
                                int argc, const char** argv, node_ptr* res)
{
  /* Invokes the PSL parser directly */
  char* cmd;
  char* cmd1;
  int len = 0;
  int i;
  int status = 0;
  YY_BUFFER_STATE buf;
  extern NuSMVEnv_ptr __nusmv_parser_env__;
  extern NuSMVEnv_ptr __psl_parser_env__;

  *res = Nil;

  for (i=0; i < argc; ++i) {
    if (argv[i] != (char*) NULL) len += strlen(argv[i]) + 1;
  }

  len += 1+2+1;  /* semicolon, last two YY_END_OF_BUFFER_CHAR, terminator */
  cmd = ALLOC(char, len);
  cmd1 = ALLOC(char, len);

  cmd[0] = '\0';
  for (i=0; i < argc; ++i) {
    if (argv[i] != (char*) NULL) {
      strcat(cmd, argv[i]);
      strcat(cmd, " ");
    }
  }
  sprintf(cmd1, "%s;%c%c", cmd, YY_END_OF_BUFFER_CHAR, YY_END_OF_BUFFER_CHAR);

  __nusmv_parser_env__ = env;
  __psl_parser_env__ = env;
  psl_parsed_tree = Nil;
  parser_free_parsed_syntax_errors(env);

  psl_property_name = Nil;
  buf = psl_yy_scan_buffer(cmd1, len-1);
  nusmv_assert(buf != (YY_BUFFER_STATE) NULL);
  status = (psl_yyparse() != 0);
  psl_yy_delete_buffer(buf); /* frees the buffer */

  FREE(cmd);
  FREE(cmd1);

  /* We need to reset the input buffer */
  //psl_yyrestart(psl_yyin);
  *res = psl_parsed_tree;
  return status;
}

int Parser_read_psl_from_file(const NuSMVEnv_ptr env,
                              const char* filename, node_ptr* res)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  extern NuSMVEnv_ptr __psl_parser_env__;
  extern NuSMVEnv_ptr __nusmv_parser_env__;

  int status = 1; /* by default there is a problem */
  char * old_input_file;

  __nusmv_parser_env__ = env;
  __psl_parser_env__ = env;
  psl_parsed_tree = Nil;
  parser_free_parsed_syntax_errors(env);

  psl_property_name = Nil;

  /* open the file */
  nusmv_assert(filename != (char*) NULL);

  psl_yyin = fopen(filename,"r");
  if (psl_yyin == (FILE*) NULL) ErrorMgr_rpterr(errmgr, "cannot open input file %s",filename);
  nusmv_yylineno = 1;
  psl_yylineno = 1;

  old_input_file = get_input_file(opts);
  if (NIL(char) != old_input_file) {
    old_input_file = util_strsav(old_input_file);
  }

  set_input_file(opts, (char*)filename);

  /* buffer has not been initialized before: */
  nusmv_assert(NULL == YY_CURRENT_BUFFER);
  YY_CURRENT_BUFFER = psl_yy_create_buffer(psl_yyin, YY_BUF_SIZE);
  /* Flushes the current input buffer */
  (void) psl_yy_switch_to_buffer(YY_CURRENT_BUFFER);

  (void) psl_yyrestart(psl_yyin);

  CATCH(errmgr) { /* catch errors in parsing */
    /* parse the file */
    status = psl_yyparse();

    /* [MRAT]: Do we really need these checks?
       [MRAT]: We can simply say that whatever follows the
       [MRAT]: "sequence {SERE};" will be ignored.
       [MRAT]: Am I missing something? These error messages
       [MRAT]: are misleading on some cases since there are no
       [MRAT]: really additional tokens, but just spaces left. */

    /* parser does not check EndOfFile. check it here. Note: this
       solution may be not perfect because the buffer could have read one
       last token and not return it to the parser and we will not see it. */
    if (!status && !feof(psl_yyin)) {
#define buf_size 50
      char buf[buf_size]; /* output just buf_size-1 bytes */
      size_t n;
      nusmv_yylineno = psl_yylineno;
      n = fread(buf, 1, buf_size-1, psl_yyin);
      buf[n] = '\0';
      ErrorMgr_rpterr(errmgr, "unexpected text left after parsing "
                      "done : \"%s\".", buf);
    }
  }
  FAIL(errmgr) {
    /* Do not exit now. Do cleaning at first */
    status = 1;
  }

  /* close the file */
  nusmv_assert(NULL != YY_CURRENT_BUFFER);/* buffer is to be initialized before */
  psl_yy_delete_buffer(YY_CURRENT_BUFFER);
  YY_CURRENT_BUFFER = NULL;

  fclose(psl_yyin);

  set_input_file(opts, old_input_file);

  if (NIL(char) != old_input_file) {
    FREE(old_input_file);
  }

  if (!status) *res = psl_parsed_tree;

  return status;
}

node_ptr Parser_get_syntax_errors_list(const NuSMVEnv_ptr env)
{
  node_ptr parsed_errors = Nil;

  if (NuSMVEnv_has_value(env, ENV_PARSED_ERRORS)) {

    if (!NuSMVEnv_get_flag(env, ENV_ERROR_REVERSED)) {
      parsed_errors = NODE_PTR(NuSMVEnv_remove_value(env, ENV_PARSED_ERRORS));
      parsed_errors = reverse(parsed_errors);
      NuSMVEnv_set_value(env, ENV_PARSED_ERRORS, parsed_errors);
      NuSMVEnv_set_flag(env, ENV_ERROR_REVERSED, true);
    }
    else {
      parsed_errors = NODE_PTR(NuSMVEnv_get_value(env, ENV_PARSED_ERRORS));
    }

  }

  return parsed_errors;
}

void Parser_get_syntax_error(node_ptr node,
                             const char** out_filename,
                             int* out_lineno,
                             const char** out_token,
                             const char** out_message)
{
  nusmv_assert(Nil != node);
  nusmv_assert(SYNTAX_ERROR == node_get_type(node));

  if ((const char**) NULL != out_filename) {
    nusmv_assert(COLON == node_get_type(car(node)));
    *out_filename = UStringMgr_get_string_text((string_ptr) caar(node));
  }

  if ((int*) NULL != out_lineno) {
    nusmv_assert(COLON == node_get_type(car(node)));
    *out_lineno = PTR_TO_INT(cdar(node));
  }

  if ((const char**) NULL != out_token) {
    nusmv_assert(COLON == node_get_type(cdr(node)));
    *out_token = (const char*) cadr(node);
  }

  if ((const char**) NULL != out_message) {
    nusmv_assert(COLON == node_get_type(cdr(node)));
    *out_message = (const char*) cddr(node);
  }
}

void Parser_print_syntax_error(node_ptr error, FILE* fout)
{
  const char* fname;
  const char* token;
  const char* msg;
  int lineno;

  Parser_get_syntax_error(error, &fname, &lineno, &token, &msg);
  if ((char*) NULL != fname) {
    fprintf(fout, "file %s: ", fname);
  }
  else {
    fprintf(fout, "file stdin: ");
  }

  fprintf(fout, "line %d: ", lineno);

  if ((const char*) NULL != token) {
    fprintf(fout, "at token \"%s\"", token);
  }
  fprintf(fout, ": %s\n", msg);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void parser_add_syntax_error(const NuSMVEnv_ptr env,
                             const char* fname, int lineno,
                             const char* token, const char* err_msg)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  UStringMgr_ptr strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
  node_ptr  parsed_errors = Nil;

  node_ptr err = new_node(nodemgr, SYNTAX_ERROR,
                          new_node(nodemgr, COLON,
                                   (node_ptr) UStringMgr_find_string(strings, (char*) fname),
                                   NODE_FROM_INT(lineno)),
                          new_node(nodemgr, COLON,
                                   (node_ptr) util_strsav((char*) token),
                                   (node_ptr) util_strsav((char*) err_msg)));

  if (NuSMVEnv_has_value(env, ENV_PARSED_ERRORS)) {
    parsed_errors = NODE_PTR(NuSMVEnv_remove_value(env, ENV_PARSED_ERRORS));
  }

  parsed_errors = cons(nodemgr, err, parsed_errors);
  NuSMVEnv_set_value(env, ENV_PARSED_ERRORS, parsed_errors);
}

void parser_free_parsed_syntax_errors(const NuSMVEnv_ptr env)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  node_ptr iter = Nil;

  if (NuSMVEnv_has_value(env, ENV_PARSED_ERRORS)) {
    iter = NODE_PTR(NuSMVEnv_remove_value(env, ENV_PARSED_ERRORS));
  }

  while (iter != Nil) {
    node_ptr se;
    char* str;

    nusmv_assert(CONS == node_get_type(iter));

    se = car(iter);
    nusmv_assert(SYNTAX_ERROR == node_get_type(se));
    nusmv_assert(COLON == node_get_type(car(se)));
    nusmv_assert(COLON == node_get_type(cdr(se)));

    free_node(nodemgr, car(se)); /* COLON fname and lineno */

    /*token string*/
    str = (char*) cadr(se); if ((char*) NULL != str) FREE(str);

    /*message string*/
    str = (char*) cddr(se); if ((char*) NULL != str) FREE(str);

    free_node(nodemgr, cdr(se)); /* COLON token and message */

    free_node(nodemgr, se); /* SYNTAX_ERROR */

    { /* frees the list node */
      node_ptr tmp = iter;
      iter=cdr(iter);
      free_node(nodemgr, tmp);
    }
  }
  NuSMVEnv_set_flag(env, ENV_ERROR_REVERSED, false);
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Open a file and inform the parser to read from it

  Open a file, pre-process it, and inform the parser to
  start reading tokens from this file. The directory in which the original file
  resides is used to store the temporary files. This is so that any #includes
  statements used by the second or later pre-processor work properly.

  \se Creates temporary files which are subsequently deleted.
*/
static void parser_open_input_pp(const NuSMVEnv_ptr env, const char* filename)
{
  Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));


#if NUSMV_HAVE_POPEN
  const char* STDIN_OPT_NAME = " -";
  char c;

  char* pp_curr;
  char* pp_next;
  char* pp_cmd;
  char* pp_exec;

  char* path;

  int tmp_len;

  FILE* tempstream1;
  FILE* tempstream2;

  char* pp_list = get_pp_list(opts);
  char* pp_list_copy;

  pp_list_copy = ALLOC(char, strlen(pp_list) + 1);
  nusmv_assert(pp_list_copy != NULL);
  pp_list_copy[0] = '\0';

  /* Set tmpfname1 to initial file to use as input */
  if (tmpfname1 != (char*) NULL) FREE(tmpfname1);

  if (filename != (char*) NULL) {
    tmpfname1 = ALLOC(char, strlen(filename) + 1);
    nusmv_assert(tmpfname1 != (char*) NULL);
    strcpy(tmpfname1, filename);

    /* Get path to initial file */
    tmp_len = strlen(filename) - strlen(Utils_StripPath(filename));
    path = ALLOC(char, tmp_len + 1);
    strncpy(path, filename, tmp_len);
    path[tmp_len] = '\0';
  }
  else {
    tmpfname1 = (char*) NULL;

    path = ALLOC(char, 1);
    path[0] = '\0';
  }

  strcpy(pp_list_copy, pp_list);

  pp_curr = strtok(pp_list_copy, " ");
  pp_next = strtok(NULL, " ");

  /* Need to indicate that tmpfname1 refers to the original input file
     so that it does not get deleted when Parser_ClosePP() is called.
     This is done by making tmpfname2 not NULL */
  if (pp_next == NULL) {
    if (tmpfname2 != (char*) NULL) FREE(tmpfname2);
    tmpfname2 = ALLOC(char, 2);
    nusmv_assert(tmpfname2 != (char*) NULL);
    strcpy(tmpfname2, "x");
  }

  while (pp_next != NULL) {
    if (tmpfname1 != (char*) NULL) {
      /* Test whether it is possible to open the file */
      if(!(tempstream1 = fopen(tmpfname1, "r"))) {
        ErrorMgr_rpterr(errmgr, "cannot open input file %s",tmpfname1);
      }
      (void) fclose(tempstream1);
    }

    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "Calling %s preprocessor\n", pp_curr);
    }
    pp_exec = get_preprocessor_call(env, pp_curr);
    if (pp_exec == (char*) NULL) {
      ErrorMgr_error_unknown_preprocessor(errmgr, pp_curr);
    }

    if (tmpfname1 != (char*)NULL) {
      pp_cmd = ALLOC(char, strlen(pp_exec) + strlen(tmpfname1) + 2);
    }
    else {
      pp_cmd = ALLOC(char, strlen(pp_exec) + strlen(STDIN_OPT_NAME) + 1);
    }
    nusmv_assert(pp_cmd != NIL(char));

    strcpy(pp_cmd, pp_exec);
    if (tmpfname1 != (char*)NULL) {
      strcat(pp_cmd, " ");
      strcat(pp_cmd, tmpfname1);
    }
    else { strcat(pp_cmd, STDIN_OPT_NAME); }
    nusmv_yylineno = 1;

    if (opt_verbose_level_gt(opts, 2)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "\nInvoking the command: '%s'...\n", pp_cmd);
    }
    /* Invoke command */

    if ( !(tempstream1 = popen(pp_cmd, "r")) ) {
      ErrorMgr_rpterr(errmgr, "error executing command \"%s\"", pp_cmd);
    }

    /* Get unique temporary filename */
    if (tmpfname2 != (char*) NULL) FREE(tmpfname2);
    tmpfname2 = Utils_get_temp_filename_in_dir(path, "NuSMVXXXXXX");
    if (tmpfname2 == NULL) {
      ErrorMgr_rpterr(errmgr, "Unable to generate unique temporary file. (Previously generated temporary file: %s)\n", tmpfname1);
    }

    tempstream2 = fopen(tmpfname2, "w");
    if(!tempstream2) ErrorMgr_rpterr(errmgr, "cannot open input file %s",tmpfname2);

    while (feof(tempstream1) == 0) {
      c = getc(tempstream1);
      if (c != EOF) {putc(c, tempstream2);}
    }
    (void) pclose(tempstream1);
    (void) fclose(tempstream2);

    if (((char*)NULL == filename || strcmp(tmpfname1, filename) != 0) &&
        (tempstream1 = fopen(tmpfname1,"r"))) {
      (void) fclose(tempstream1);
      if (remove(tmpfname1) == -1) {
        ErrorMgr_rpterr(errmgr, "error deleting temporary file \"%s\"", tmpfname1);
      }
    }

    if (tmpfname1 != (char*) NULL) FREE(tmpfname1);
    tmpfname1 = ALLOC(char, strlen(tmpfname2) + 1);
    nusmv_assert(tmpfname1 != (char*) NULL);
    strcpy(tmpfname1, tmpfname2);

    FREE(tmpfname2); tmpfname2 = (char*) NULL;
    FREE(pp_cmd);

    pp_curr = pp_next;
    pp_next = strtok(NULL, " ");
  } /* End of while (pp_list != NULL) */

  pp_exec = get_preprocessor_call(env, pp_curr);
  if (pp_exec == (char*) NULL) {
    ErrorMgr_error_unknown_preprocessor(errmgr, pp_curr);
  }

  /* Set nusmv_yyin to result of running last pre-processor */
  if (tmpfname1 != (char*) NULL) {
    /* Test whether it is possible to open the file */
    if(!(tempstream1 = fopen(tmpfname1, "r"))) {
      ErrorMgr_rpterr(errmgr, "cannot open input file %s",tmpfname1);
    }
    (void) fclose(tempstream1);

    pp_cmd = ALLOC(char, strlen(pp_exec) + strlen(tmpfname1) + 2);
    strcpy(pp_cmd, pp_exec);
    strcat(pp_cmd, " ");
    strcat(pp_cmd, tmpfname1);
  }
  else {
    pp_cmd = ALLOC(char, strlen(pp_exec) + strlen(STDIN_OPT_NAME) + 1);
    strcpy(pp_cmd, pp_exec);
    strcat(pp_cmd, STDIN_OPT_NAME);
  }

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Calling %s preprocessor\n", pp_curr);
  }
  if (opt_verbose_level_gt(opts, 2)) {
    Logger_log(logger, "\nInvoking the command: '%s'...\n", pp_cmd);
  }
  if( !(nusmv_yyin = popen(pp_cmd, "r")) ) {
    FREE(pp_list_copy);
    ErrorMgr_rpterr(errmgr, "error executing command \"%s\"", pp_cmd);
  }

  FREE(pp_cmd);
  FREE(pp_list_copy);

  psl_yyin = nusmv_yyin;

  /* Flushes the current input buffer */
  (void) nusmv_yy_switch_to_buffer(nusmv_yy_create_buffer(nusmv_yyin, YY_BUF_SIZE));
  (void) nusmv_yyrestart(nusmv_yyin);

#endif /* NUSMV_HAVE_POPEN */
}

/*!
  \brief Close the input file

  Closes the input file used from parser to read tokens.

  \se Deletes any temporary files created by
  parser_open_input_pp.
*/
static void parser_close_input_pp(const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

#if NUSMV_HAVE_POPEN
  FILE* stream;

  /* tmpfname refers to original input file */
  if (tmpfname2 != NULL) FREE(tmpfname2);
  else {
    stream = fopen(tmpfname1,"r");
    if (stream != (FILE*) NULL) {
      (void) fclose(stream);
      if (remove(tmpfname1) == -1) {
        ErrorMgr_rpterr(errmgr, "error deleting file \"%s\"", tmpfname1);
      }
    }
  }

  FREE(tmpfname1); tmpfname1 = (char*) NULL;

  (void) pclose(nusmv_yyin);
#endif
}
