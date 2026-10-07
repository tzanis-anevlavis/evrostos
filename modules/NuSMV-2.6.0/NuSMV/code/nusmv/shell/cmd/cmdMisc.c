/* ---------------------------------------------------------------------------


  This file is part of the ``cmd'' package of NuSMV version 2.
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
  \author Adapted to NuSMV by Marco Roveri
  \brief Variable table; miscellaneous commands related to the general
  system.

  \todo: Missing description

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/shell/cmd/cmdInt.h"

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/cinit/cinit.h"

#include <stdio.h>

#if NUSMV_HAVE_STRING_H
#include <string.h> /* for strdup */
#else
#ifndef strdup
char* strdup(const char*); /* forward declaration */
#endif
#endif
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_PURIFY_FILE_CREATED "cmdPurFileCreated"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

#if NUSMV_HAVE_LIBREADLINE

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef char **rl_completion_func_t(const char *, int, int);
typedef char *rl_compentry_func_t(const char *, int);
#endif

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/* This variable has bee added to make the FlushBuffers reentrant. It
   has to be updated before each call of the FlushBuffers
   function. */
StreamMgr_ptr __flush_buffers_streams__ = STREAM_MGR(NULL);

#if NUSMV_HAVE_LIBREADLINE
extern const char *rl_readline_name;
extern int rl_completion_type;
extern rl_completion_func_t *rl_attempted_completion_function;
#endif

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/



static void FlushBuffers(int sigtype);

#if NUSMV_HAVE_LIBREADLINE
static char ** command_completion(const char *text, int start, int end);
static char *command_completion_generator(const char *text, int state);
char **rl_completion_matches(const char *, rl_compentry_func_t *);
#endif

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Cmd_Init(NuSMVEnv_ptr env)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  avl_tree* cmdTable = avl_init_table((int (*)(char*, char*))strcmp);
  avl_tree* aliasTable = avl_init_table((int (*)(char*, char*))strcmp);
  array_t* cmdHistoryArray = array_alloc(char *, 0);
  int start_time;

  NuSMVEnv_set_value(env, ENV_CMD_COMMAND_TABLE, cmdTable);
  NuSMVEnv_set_value(env, ENV_CMD_ALIAS_TABLE, aliasTable);
  NuSMVEnv_set_value(env, ENV_CMD_COMMAND_HISTORY, cmdHistoryArray);

  NuSMVEnv_set_flag(env, ENV_PURIFY_FILE_CREATED, false);

  /* Program the signal of type USR1 to flush outstream and errstream */
#ifdef SIGUSR1
  __flush_buffers_streams__ = streams;
  (void) signal(SIGUSR1, FlushBuffers);
#endif

  /* Initialize libreadline's completion machinery */
#if NUSMV_HAVE_LIBREADLINE
  rl_readline_name = "NuSMV";
  /*
   * rl_completion_type specifies the type of completion readline is
   * currently attempting.  `!' means to display all of the possible
   * completions, if there is more than one, as well as performing
   * partial completion.
   */
  rl_completion_type = '!';
  rl_attempted_completion_function = command_completion;
#endif

  nusmv_assert(!NuSMVEnv_has_value(env, ENV_START_TIME));
  start_time = util_cpu_time();
  /* we use the usual offset.. */
  NuSMVEnv_set_or_replace_value(env, ENV_START_TIME,
                                PTR_FROM_INT(void*, start_time + 1));
}

void
Cmd_End(NuSMVEnv_ptr env)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  avl_tree* cmdTable = (avl_tree*)
      NuSMVEnv_remove_value(env, ENV_CMD_COMMAND_TABLE);
  avl_tree* aliasTable = (avl_tree*)
      NuSMVEnv_remove_value(env, ENV_CMD_ALIAS_TABLE);
  array_t* cmdHistoryArray = (array_t*)
      NuSMVEnv_remove_value(env, ENV_CMD_COMMAND_HISTORY);

  avl_free_table(cmdTable, (void (*)()) 0, CmdCommandFree);
  avl_free_table(aliasTable, (void (*)()) 0, CmdAliasFree);

  {
    int c;
    char *dummy;

    for (c = array_n(cmdHistoryArray); c-- > 0; ){
      dummy = array_fetch(char *, cmdHistoryArray, c);
      FREE(dummy);
    }
  }

  array_free(cmdHistoryArray);

  if (NuSMVEnv_get_flag(env, ENV_PURIFY_FILE_CREATED)) {
    StreamMgr_print_output(
        streams,  "Purify has created a temporary file. The file");
    StreamMgr_print_output(streams,  " must be deleted.\n");
  }
}

FILE* CmdOpenPipe(const NuSMVEnv_ptr env, int useMore)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

#if NUSMV_HAVE_POPEN
  FILE* rf = NIL(FILE);
  char* pager = NIL(char);

#if NUSMV_HAVE_GETENV
  pager = getenv("PAGER");
  if (pager == (char*) NULL) {
    rf = popen("more", "w");
    if (rf == (FILE*)NULL) {
      StreamMgr_print_error(streams, "Unable to open pipe with \"more\".\n");
    }
  }
  else {
    rf = popen(pager, "w");
    if (rf == NULL) {
      StreamMgr_print_error(streams,
                            "Unable to open pipe with \"%s\".\n", pager);
    }
  }
#else
  rf = popen("more", "w");
  if (rf == (FILE*) NULL) {
    StreamMgr_print_error(streams, "Unable to open pipe with \"more\".\n");
  }
#endif
  return rf;

#else /* NUSMV_HAVE_POPEN */
  return (FILE*) NULL;
#endif
}

void CmdClosePipe(FILE* file)
{
#if NUSMV_HAVE_POPEN
  pclose(file);
#endif
}

FILE* CmdOpenFile(const NuSMVEnv_ptr env, const char* filename)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  FILE* rf = NIL(FILE);

  if (filename != NIL(char)) {
    rf = fopen(filename, "w");
    if (rf == (FILE*) NULL) {
      StreamMgr_print_error(streams, "Unable to open file \"%s\".\n",
                            filename);
    }
  } else {
    StreamMgr_print_error(streams, "CmdOpenFile: file name is NULL\n");
  }
  return(rf);
}

void CmdCloseFile(FILE* file)
{
  fflush(file);
  fclose(file);
}

Outcome Cmd_Misc_open_pipe_or_file(NuSMVEnv_ptr const env,
                                   const char* dbgFileName,
                                   FILE** outstream)
{
  StreamMgr_ptr const streams =
   STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  if ((const char*)NULL == dbgFileName) {
    *outstream = CmdOpenPipe(env, true);

    if (*outstream != (FILE*)NULL) {
      StreamMgr_set_output_stream(streams, *outstream);
    }
    else return OUTCOME_GENERIC_ERROR;
  }
  else if (dbgFileName != NIL(char)) {
    *outstream = CmdOpenFile(env, dbgFileName);

    if (*outstream != (FILE*) NULL) {
      StreamMgr_set_output_stream(streams, *outstream);
    }
    else return OUTCOME_GENERIC_ERROR;
  }

  return OUTCOME_SUCCESS;
}

int Cmd_Misc_set_global_out_stream(NuSMVEnv_ptr env,
                                   char* filename,
                                   boolean use_a_pipe,
                                   FILE** prev_outstream)
{
  FILE* stream = NULL;
  /* Caller asks either for streaming on file or on pipe */
  nusmv_assert(! (NULL == filename && ! use_a_pipe));

  /* Caller cannot asks both */
  nusmv_assert(NULL != filename || use_a_pipe);

  if (NULL != filename) {
    stream = CmdOpenFile(env, filename);
  }
  else {
    nusmv_assert(use_a_pipe);
    stream = CmdOpenPipe(env, true);
  }

  if (NULL == stream) {
    *prev_outstream = NULL;
    return 1;
  }
  else {
    StreamMgr_ptr const streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
    *prev_outstream = StreamMgr_reset_output_stream(streams);
    StreamMgr_set_output_stream(streams, stream);
    return 0;
  }
}

void Cmd_Misc_restore_global_out_stream(NuSMVEnv_ptr env,
                                        char* filename,
                                        boolean use_a_pipe,
                                        FILE* prev_outstream)
{
  StreamMgr_ptr const streams =
   STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* stream = StreamMgr_reset_output_stream(streams);

  nusmv_assert(NULL != prev_outstream);

  if (filename != NIL(char)) CmdCloseFile(stream);
  else {
    nusmv_assert(use_a_pipe);
    CmdClosePipe(stream);
  }
  StreamMgr_set_output_stream(streams, prev_outstream);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void
CmdFreeArgv(int  argc,  char ** argv)
{
  int i;

  for(i = 0; i < argc; i++) {
    FREE(argv[i]);
  }
  FREE(argv);
}

void
CmdAliasFree(
  char * value)
{
  CmdAliasDescr_t *alias = (CmdAliasDescr_t *) value;

  CmdFreeArgv(alias->argc, alias->argv);
  FREE(alias->name);            /* same as key */
  FREE(alias);
}


int Cmd_Misc_NusmvrcSource(NuSMVEnv_ptr env)
{
  char *commandLine;
  char *libraryName;
  char *rcFileName;
  char *homeFile;
  char *tmpRcTilde;
  char *tmpCmd;
  int status0;
  int status1 = TRUE;
  int status2 = TRUE;
  const char* cmd_pattern = "source -s %s/master%s";
  const char* rc_tilde_pattern = "~/%s";
  const char* source_pattern = "source -s %s";
  boolean home_exists, cur_exists;

  /*
   * First execute the standard .nusmvrc.
   */
  libraryName = CInit_NuSMVObtainLibrary();
  rcFileName = NuSMVCore_get_tool_rc_file_name();
  commandLine = ALLOC(char, strlen(cmd_pattern) + strlen(libraryName) +
                      strlen(rcFileName)+1);
  sprintf(commandLine, cmd_pattern, libraryName, rcFileName);
  FREE(libraryName);
  status0 = Cmd_CommandExecute(env, commandLine);
  FREE(commandLine);

  /*
   * Look in home directory and current directory for .nusmvrc.
   */
  /* ~/.nusmvrc */
  tmpRcTilde = ALLOC(char,
                     strlen(rcFileName) + strlen(rc_tilde_pattern) + 1);
  sprintf(tmpRcTilde, rc_tilde_pattern, rcFileName);

  /* .nusmvrc */
  homeFile = util_tilde_expand(tmpRcTilde);
  home_exists = Utils_file_exists(homeFile);
  cur_exists = Utils_file_exists(rcFileName);
  FREE(homeFile);
  /*
   * If .nusmvrc is present in both the home and current directories, then read
   * it from the home directory.  Otherwise, read it from wherever it's
   * located.
   */
  if (home_exists && cur_exists &&
      Utils_files_are_the_same(homeFile, rcFileName)) {
    /* ~/.nusmvrc == .nusmvrc : Source the file only once */
    tmpCmd = ALLOC(char, strlen(source_pattern) + strlen(tmpRcTilde) + 1);
    sprintf(tmpCmd, source_pattern, tmpRcTilde);
    status1 = Cmd_CommandExecute(env, tmpCmd);
    FREE(tmpCmd);
  }
  else {
    if (home_exists) {
      tmpCmd = ALLOC(char, strlen(tmpRcTilde) + strlen(source_pattern) + 1);
      sprintf(tmpCmd, source_pattern, tmpRcTilde);
      status1 = Cmd_CommandExecute(env, tmpCmd);
      FREE(tmpCmd);
    }
    if (cur_exists) {
      tmpCmd = ALLOC(char, strlen(rcFileName) + strlen(source_pattern) + 1);
      sprintf(tmpCmd, source_pattern, rcFileName);
      status2 = Cmd_CommandExecute(env, tmpCmd);
      FREE(tmpCmd);
    }
  }

  FREE(tmpRcTilde);

  return (status0 && status1 && status2);
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/



#if NUSMV_HAVE_LIBREADLINE

/*!
  \brief Generate completion matches for readline.

  Based on the partial input and the list of installed commands
                generates the possible completions.

  \se none

  \sa Cmd_Completion
*/
static char *command_completion_generator(const char *text, int state)
{
  extern avl_tree* __rl_cmd_table__;
  static int list_index, len;
  char* key;
  avl_generator* gen;
  int cnt;

  if (!state) {
    list_index = 0;
    len = strlen(text);
  }

  cnt = 0;

  avl_foreach_item(__rl_cmd_table__, gen, AVL_FORWARD, &key, NIL(char *)){
    if (strncmp(text, key, len) == 0) { // possible match
      if (cnt++ == list_index) {
        list_index++;
        return strdup(key);
      }
    }
  }

  return NULL;
}

/*!
  \brief Sets up command or filename completion on reading user input.

  We use the regular NuSMV command completion function for the
                first word on the line, and filename completion for the rest.

  \se none

  \sa CmdCompletion_Generator
*/
static char ** command_completion(const char *text, int start, int end)
{
  if (start) return NULL; /* not on line beginning: filename */
  else return rl_completion_matches(text, command_completion_generator);
}
#endif

/*!
  \brief Function to flush outstream and errstream.

  This function is the signal handler for the SIGUSR1
  signal. Whenever that signal is received, this function is executed and the
  output channels of NuSMV are flushed.

  \sa Cmd_Init
*/
static void
FlushBuffers(
  int sigtype)
{
  StreamMgr_flush_streams(__flush_buffers_streams__);

  /* Reprogram again the handler */
#ifdef SIGUSR1
  (void) signal(SIGUSR1, FlushBuffers);
#endif
} /* End of FlushBuffers */
