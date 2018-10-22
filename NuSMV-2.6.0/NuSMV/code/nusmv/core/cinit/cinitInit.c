/* ---------------------------------------------------------------------------

  This file is part of the ``cinit'' package of NuSMV version 2.
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
  \brief Initializes and ends NuSMV.

  \todo: Missing description

*/

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/cinit/cinitInt.h"
#include "nusmv/core/cinit/NuSMVEnv.h"

#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/WordNumber.h" /* for WordNumber_init and ..._quit */
#include "nusmv/core/utils/ErrorMgr.h"

#include "nusmv/core/compile/FlatHierarchy.h"
#include "nusmv/core/dd/VarsHandler.h"
#include "nusmv/core/wff/wff.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/simulate/simulate.h"
#include "nusmv/core/parser/parser.h"

/* package initializers/deinitializers */
#include "nusmv/core/opt/optPkg.h"
#include "nusmv/core/prop/propPkg.h"
#include "nusmv/core/bmc/bmcPkg.h"
#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/fsm/fsm.h"
#include "nusmv/core/hrc/hrc.h"

#include <string.h>

/*---------------------------------------------------------------------------*/
/* Constants declarations                                                    */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NUSMV_CPP_NAME "cpp"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NUSMV_M4_NAME "m4"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_PREPROCESSORS_LIST "preprocessors_list"

/* number of fields in structure preprocessors_list: */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define PP_FIELDS_NUM  3

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

FILE* nusmv_historyFile;
FILE* nusmv_stdpipe;

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static char* get_executable_name(const char* command);

static const char* user_preprocessor(const NuSMVEnv_ptr env,
                                     const char* name);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void CInit_reset_first(NuSMVEnv_ptr env)
{
  OptsHandler_ptr opt =
      (OptsHandler_ptr) NuSMVEnv_get_value(env, ENV_OPTS_HANDLER);

  TracePkg_quit(env);
  PropPkg_quit(env);

#if NUSMV_HAVE_SAT_SOLVER
  Bmc_Quit(env);
#endif

  Simulate_Pkg_quit(env);
  Fsm_quit();
  Enc_quit_encodings(env);

  Compile_quit(env);

  wff_pkg_quit(env);

  Hrc_quit(env);
  Parser_Quit(env);

  /* WARNING [MD]: bad use of a generic name. Did you mean NDEBUG? */
  #ifdef DEBUG
  if (opt_verbose_level_gt(opt, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    int result = dd_checkzeroref(dd);
    if (result != 0) {
      Logger_log(logger,
                 "\n%d non-zero DD reference counts "
                 "after dereferencing.\n",
                 result);
    }
  }
  #endif

  if (opt_verbose_level_gt(opt, 2)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Clearing DD and node packages....");
  }

  Dd_quit(env);

  WordNumber_quit(env);

  set_pkg_quit();
  node_pkg_quit(env);

  if (opt_verbose_level_gt(opt, 2)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Done\n");
  }
}

void CInit_reset_last(NuSMVEnv_ptr env)
{
  OptsHandler_ptr opt = NuSMVEnv_get_value(env, ENV_OPTS_HANDLER);
  DDMgr_ptr dd;

  node_pkg_init(env);
  set_pkg_init();

  WordNumber_init(env);

  if (opt_verbose_level_gt(opt, 2)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Restarting the compiler....\n");
  }

  Parser_Init(env);
  Hrc_init(env);

  wff_pkg_init(env);

  Compile_init(env);

  if (opt_verbose_level_gt(opt, 2)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Done\n");
    Logger_log(logger, "Restarting the DD manager....");
  }

  Dd_init(env);

  dd = NuSMVEnv_get_value(env, ENV_DD_MGR);

  /* restores the dynamic reordering method if enabled */
  if (opt_dynamic_reorder(opt)) {
    dd_autodyn_enable(dd, get_reorder_method(opt));
  }

  Enc_init_encodings(env);
  Fsm_init();

  PropPkg_init(env);
  TracePkg_init(env);
  Simulate_Pkg_init(env);

  if (opt_verbose_level_gt(opt, 2)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Done\n");
  }
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void CInit_init(NuSMVEnv_ptr env)
{
  init_memory();
  nusmv_historyFile = NIL(FILE);
  Stream_init(env);

  /* win32 platforms seem to be "lazy" when they need to empty stream
     buffers. In this case we force buffers to be emptied explicitly */
#if NUSMV_HAVE_SETVBUF && (defined(__MINGW32__) || defined(__CYGWIN__))
  {
    StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
# if SETVBUF_REVERSED
    setvbuf(StreamMgr_get_output_stream(streams), _IOLBF, (char *) NULL, 0);
    setvbuf(StreamMgr_get_error_stream(streams), _IONBF, (char *) NULL, 0);
# else
    setvbuf(StreamMgr_get_output_stream(streams), (char *) NULL, _IOLBF, 0);
    setvbuf(StreamMgr_get_error_stream(streams), (char *) NULL, _IONBF, 0);
# endif
  }
#endif

  Logger_init(env);

  init_string(env);

  Opt_Pkg_init(env);

  Error_init(env);

  node_pkg_init(env);
  set_pkg_init();

  WordNumber_init(env);

  PropPkg_init(env);

  Parser_Init(env);
  Hrc_init(env);

  wff_pkg_init(env);

  Compile_init(env);

  Dd_init(env);

  Enc_init_encodings(env); /* Does nothing at the moment */

  Fsm_init();

  /* Other init here */
#if NUSMV_HAVE_SAT_SOLVER
  Bmc_init_opt(env);
#endif

  TracePkg_init(env);

  Utils_pkg_init(env);
  Simulate_Pkg_init(env); /* Does nothing at the moment */
}

void CInit_end(NuSMVEnv_ptr env)
{
  boolean print_final = false;
  OptsHandler_ptr opt = NuSMVEnv_get_value(env, ENV_OPTS_HANDLER);
  StreamMgr_ptr streams = NuSMVEnv_get_value(env, ENV_STREAM_MANAGER);

  Utils_pkg_quit(env);

  TracePkg_quit(env);
  PropPkg_quit(env);
#if NUSMV_HAVE_SAT_SOLVER
  Bmc_Quit(env);
#endif

  Enc_quit_encodings(env);

  wff_pkg_quit(env);

  Parser_Quit(env);

  Compile_quit(env);
  Hrc_quit(env);
  Simulate_Pkg_quit(env);

#ifdef DEBUG
  if (opt_verbose_level_gt(opt, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    DDMgr_ptr dd = NuSMVEnv_get_value(env, ENV_DD_MGR);
    int result = dd_checkzeroref(dd);
    if (result != 0) {
      Logger_log(logger,
              "%d non-zero DD reference counts after dereferencing\n",
              result);
    }
  }
#endif

  Dd_quit(env);

  WordNumber_quit(env);

  set_pkg_quit();
  node_pkg_quit(env);

  print_final = opt_verbose_level_gt(opt, 0);

  Error_quit(env);

  Opt_Pkg_deinit(env);
  quit_string(env);

  if (print_final) {
    StreamMgr_print_error(streams, "\nSuccessful termination\n");
  }

  if (nusmv_historyFile != NIL(FILE)) fclose(nusmv_historyFile);
  nusmv_historyFile = NIL(FILE);

  Logger_quit(env);
  Stream_quit(env);
}

/*!
  \brief Initializes information about the pre-processors avaliable.

  \todo Missing description
*/
void init_preprocessors(const NuSMVEnv_ptr env)
{
  /* This array is used to store the names of the avaliable
     pre-processors on the system, as well as the command to excecute
     them. The names are stored in even indices, with the
     corresponding command stored and the location immediately
     succeeding the name. The last two entries are NULL to indicate
     that there are no more entries. */
  char** preprocessors_list;
  char* cpp_call = (char*) NULL;

  /* const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER)); */
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  /* two triplets preprocessor/filename/command, one triplet for
     termination */
  preprocessors_list = ALLOC(char*, PP_FIELDS_NUM*3);
  nusmv_assert(preprocessors_list != (char**) NULL);

  NuSMVEnv_set_value(env, ENV_PREPROCESSORS_LIST, preprocessors_list);

  /* sets the executable file for cpp preprocessor */
#if NUSMV_HAVE_GETENV
  cpp_call = getenv("CPP");
#endif

  if (cpp_call == (char*) NULL) {
#if NUSMV_HAVE_CPP
    cpp_call = NUSMV_PROG_CPP;
#else
    /* Tries anyway an executable: */
    cpp_call = NUSMV_CPP_NAME;
#endif
  }

  if (cpp_call == (char*) NULL) {
    //ErrorMgr_internal_error(errmgr, "The pre-proprocessor could not be found.\n");
    StreamMgr_print_error(streams, "The pre-proprocessor could not be found.\n");
  }

  /* Stores the names of avaliable pre-processors along with the
     command to actually execute them. The NUL entries signify the end
     of the list: */

  /* cpp */
  preprocessors_list[0] = util_strsav(NUSMV_CPP_NAME);
  preprocessors_list[1] = get_executable_name(cpp_call);
  preprocessors_list[2] = util_strsav(cpp_call);

  /* m4 */
  preprocessors_list[3] = util_strsav(NUSMV_M4_NAME);
  preprocessors_list[4] = get_executable_name(NUSMV_M4_NAME);
  preprocessors_list[5] = util_strsav(NUSMV_M4_NAME);

  /* terminators: */
  preprocessors_list[6] = (char*) NULL;
  preprocessors_list[7] = (char*) NULL;
  preprocessors_list[8] = (char*) NULL;
}

/*!
  \brief Removes information regarding the avaliable pre-processors.

  \todo Missing description
*/
void quit_preprocessors(const NuSMVEnv_ptr env)
{
  char** preprocessors_list =
    (char**)NuSMVEnv_remove_value(env, ENV_PREPROCESSORS_LIST);

  char** iter;

  nusmv_assert(preprocessors_list != (char**) NULL);
  iter = preprocessors_list;
  while (*iter != (char*) NULL) {
    char** n = iter;
    int count = 0;
    while (count < PP_FIELDS_NUM){
      char* nn = *n;
      FREE(nn);
      ++n;
      ++count;
    }

    iter += PP_FIELDS_NUM;
  }

  FREE(preprocessors_list);
}

/*!
  \brief Given a command, returns the executable file name (with
  extension if required)

  \todo Missing description

  \se If not already specified, extension suffix is appended
  to the returned string. Returned string must be freed.
*/
static char* get_executable_name(const char* command)
{
  char* space;
  char* exec_name = (char*) NULL;
  size_t exec_len;
  size_t exeext_len;

  space = strchr(command, ' ');
  if (space != (char*) NULL) exec_len = (size_t) (space - command);
  else exec_len = strlen(command);

  exeext_len = strlen(NUSMV_EXEEXT);

  exec_name = ALLOC(char, exec_len + exeext_len + 1);
  nusmv_assert(exec_name != (char*) NULL);

  strncpy(exec_name, command, exec_len);
  exec_name[exec_len] = '\0'; /* adds a terminator */

  if ((exec_len > exeext_len) && (exeext_len > 0)) {
    /* the command might already contain NUSMV_EXEEXT: */
    char* pos;
    pos = strstr(exec_name, NUSMV_EXEEXT);
    if ( (pos == (char*) NULL) ||
         (((int) (pos - exec_name)) < (exec_len-exeext_len)) ) {
      /* add the suffix: */
      strcat(exec_name, NUSMV_EXEEXT);
    }
  }
  else {
    /* it can't contain the suffix: add it */
    strcat(exec_name, NUSMV_EXEEXT);
  }

  return exec_name;
}


/*!
  \brief Check and returns path of user preprocessors,
               returns Null if does not exist

  \todo Missing description
*/
static const char* user_preprocessor(const NuSMVEnv_ptr env, const char* name)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  const char* res = (char*) NULL;

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef char* (*getPP_ptr) (OptsHandler_ptr);

  struct UserPreprocessorStructure {
      const char* name;
      getPP_ptr get_pp;
  } user_pp_stack[] = {{NUSMV_CPP_NAME, get_pp_cpp_path},
                       {NUSMV_M4_NAME, get_pp_m4_path }};

  int i;
  int size_user_pp_stack;
  size_user_pp_stack = sizeof(user_pp_stack) / sizeof(user_pp_stack[0]);

  for(i=0; i<size_user_pp_stack; i++){
    if (strncmp(name,user_pp_stack[i].name, strlen(user_pp_stack[i].name)) == 0){
      res =user_pp_stack[i].get_pp(opts);
      if (NULL != res){
        if (true == Utils_file_exists(res))
          return res;
        else
          ErrorMgr_error_set_preprocessor(errmgr, res, false);
      }
    }
  }

  return res;
}

char* get_preprocessor_call(const NuSMVEnv_ptr env, const char* name)
{
  char** preprocessors_list =
    (char**)NuSMVEnv_get_value(env, ENV_PREPROCESSORS_LIST);

  const char* res = (char*) NULL;
  char** iter;

  /* check if user set the preprocessor */
  /* if it is, return user value */
  res = user_preprocessor(env, name);
  if (NULL != res) return (char*) res;

  /* returns default preprocessor */
  iter = preprocessors_list;
  while (*iter != (char*) NULL) {
    if (strncmp(*iter, name, strlen(name) + 1) == 0) {
      res = *(iter + 2);
      break;
    }
    iter += PP_FIELDS_NUM;
  }
  return (char*) res;
}

char* get_preprocessor_filename(const NuSMVEnv_ptr env, const char* name)
{
  char** preprocessors_list =
    (char**)NuSMVEnv_get_value(env, ENV_PREPROCESSORS_LIST);

  const char* res = (char*) NULL;
  char** iter;

  /* check if user set the preprocessor */
  /* if it is, return user value */
  res = user_preprocessor(env, name);
  if (NULL != res) return (char*) Utils_StripPath(res);

  /* returns default preprocessor */
  iter = preprocessors_list;
  while (*iter != (char*) NULL) {
    if (strncmp(*iter, name, strlen(name) + 1) == 0) {
      res = *(iter + 1);
      break;
    }
    iter += PP_FIELDS_NUM;
  }
  return (char*) res;
}

int get_preprocessors_num(const NuSMVEnv_ptr env)
{
  char** preprocessors_list =
    (char**)NuSMVEnv_get_value(env, ENV_PREPROCESSORS_LIST);

  int len = 0;
  char** iter;

  iter = preprocessors_list;
  while (*iter != (char*) NULL) {
    ++len;
    iter += PP_FIELDS_NUM;
  }

  return len;
}

char* get_preprocessor_names(const NuSMVEnv_ptr env)
{
  char** preprocessors_list =
    (char**)NuSMVEnv_get_value(env, ENV_PREPROCESSORS_LIST);

  int len;
  char* names;
  char** iter;

  /* length of the string: */
  len = 0;
  iter = preprocessors_list;
  while (*iter != (char*) NULL) {
    len += strlen(*iter) + 1; /* for the additional space */
    iter += PP_FIELDS_NUM;
  }

  names = ALLOC(char, len+1);
  nusmv_assert(names != (char*) NULL);

  names[0] = '\0';
  iter = preprocessors_list;
  while (*iter != (char*) NULL) {
    strncat(names, *iter, strlen(*iter));
    strncat(names, " ", 1);
    iter += PP_FIELDS_NUM;
  }

  names[len] = '\0';
  return names;
}
