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
  \brief Command table and command execution.

  \todo: Missing description

*/


#if HAVE_CONFIG_H
#include "nusmv-config.h"
#endif

#include "nusmv/shell/cmd/cmdInt.h"

#include "nusmv/core/utils/error.h" /* for CATCH(errmgr) */
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"

#if NUSMV_HAVE_SIGNAL_H
#include <signal.h>
#endif

/*
 * Support to define unused varibles
 */
/*---------------------------------------------------------------------------*/
/* Stucture declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/* This is a work around for argument passing to sigterm
   function. Keeps reentrancy */
static NuSMVEnv_ptr __sigterm_env__;

static char NuSMVShellChar = '!';      /* can be reset using the "set shell_char" */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_AUTOEXEC "cmdautoexec"            /* indicates currently in autoexec */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_CMD_REENTRANT "cmdreentrant"

/* to check currently executed command reentrancy capability */

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static int com_dispatch(NuSMVEnv_ptr env, int argc, char ** argv);
static int apply_alias(NuSMVEnv_ptr env, int * argcp,
                       char *** argvp, int * loop);
static void variableInterpolation(const NuSMVEnv_ptr env, int argc, char **argv);
static char * variableInterpolationRecur(const NuSMVEnv_ptr env, char *str);
static char * split_line(const NuSMVEnv_ptr env, char * command, int * argc, char *** argv);
static int check_shell_escape(const NuSMVEnv_ptr env, char * p, int * status);
static void disarm_signal_andler(const NuSMVEnv_ptr env);
static void arm_signal_andler(const NuSMVEnv_ptr env);

#if NUSMV_HAVE_SIGNAL_H
static void sigterm(int sig);
#endif

/**AutomaticEnd***************************************************************/

void Cmd_CommandAdd(NuSMVEnv_ptr env, char* name, PFI  funcFp,
                    int  changes, boolean reentrant)
{
  avl_tree* commandTable = (avl_tree*)NuSMVEnv_get_value(env, ENV_CMD_COMMAND_TABLE);
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  char *key, *value;
  CommandDescr_t *descr;
  int status;

  key = name;
  if (avl_delete(commandTable, &key, &value)) {
    if (opt_verbose_level_gt(opts, 1)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "warning: redefining '%s'\n", name);
    }
    CmdCommandFree(value);
  }

  descr = ALLOC(CommandDescr_t, 1);
  descr->name = util_strsav(name);
  descr->command_fp = funcFp;
  descr->changes_hmgr = changes;
  descr->reentrant = reentrant;
  status = avl_insert(commandTable, descr->name, (char *) descr);
  nusmv_assert(!status);  /* error here in SIS version, TRS, 8/4/95 */
}

boolean Cmd_CommandRemove(NuSMVEnv_ptr env, const char* name)
{
  avl_tree* commandTable = (avl_tree*)NuSMVEnv_get_value(env, ENV_CMD_COMMAND_TABLE);
  char *key, *value;
  boolean status;

  key = (char*) name;
  status = (avl_delete(commandTable, &key, &value) != 0);
  if (status) CmdCommandFree(value);

  return status;
}

boolean Cmd_CommandDefined(NuSMVEnv_ptr env, const char* name)
{
  avl_tree* commandTable = (avl_tree*)NuSMVEnv_get_value(env, ENV_CMD_COMMAND_TABLE);
  char *key;
  boolean status;

  key = (char*) name;
  status = (avl_lookup(commandTable, key, (char**) NULL) != 0);

  return status;
}

CommandDescr_t *Cmd_CommandGet(NuSMVEnv_ptr env, const char* name)
{
  avl_tree* commandTable = (avl_tree*)NuSMVEnv_get_value(env, ENV_CMD_COMMAND_TABLE);
  char *key;
  CommandDescr_t *value;
  boolean status;

  key = (char*) name;
  status = (avl_lookup(commandTable, key, (char**) &value) != 0);

  if (status) {
    return value;
  } else {
    return (CommandDescr_t*) NULL;
  }
}

int Cmd_CommandExecute(NuSMVEnv_ptr env, char* command)
{
  int status, argc;
  int loop;
  char *commandp, **argv;

  disarm_signal_andler(env);
  commandp = command;
  do {
    if (check_shell_escape(env, commandp, &status)) break;

    commandp = split_line(env, commandp, &argc, &argv);
    loop = 0;
    status = apply_alias(env, &argc, &argv, &loop);
    if (status == 0) {
      variableInterpolation(env, argc, argv);
      status = com_dispatch(env, argc, argv);
    }
    CmdFreeArgv(argc, argv);
  } while (status == 0 && *commandp != '\0');

  return status;
}

int Cmd_SecureCommandExecute(NuSMVEnv_ptr env, char* command)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  int res;

  CATCH(errmgr) {
    res = Cmd_CommandExecute(env, command);
  } FAIL(errmgr) {
    res = 1;
  }
  return(res);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void CmdCommandFree(char * value)
{
  CommandDescr_t *command = (CommandDescr_t *) value;

  FREE(command->name);          /* same as key */
  FREE(command);
}

CommandDescr_t *
CmdCommandCopy(
  CommandDescr_t * value)
{
  CommandDescr_t * res;

  nusmv_assert(value != (CommandDescr_t*) NULL);

  res = ALLOC(CommandDescr_t, 1);
  nusmv_assert(res != (CommandDescr_t*) NULL);

  res->name         = util_strsav(value->name);
  res->command_fp   = value->command_fp;
  res->changes_hmgr = value->changes_hmgr;
  res->reentrant    = value->reentrant;

  return res;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief required

  optional

  \se required

  \sa optional
*/
static int
com_dispatch(NuSMVEnv_ptr env, int  argc, char ** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  avl_tree* commandTable = (avl_tree*)NuSMVEnv_get_value(env, ENV_CMD_COMMAND_TABLE);

  int status;
  char *value;
  CommandDescr_t *descr;

  if (argc == 0) {              /* empty command */
    return 0;
  }

  if (! avl_lookup(commandTable, argv[0], &value)) {
    StreamMgr_print_error(streams,  "unknown command '%s'\n", argv[0]);
    return 1;
  }

  descr = (CommandDescr_t *) value;

  arm_signal_andler(env);

  CATCH(errmgr) {
    NuSMVEnv_set_flag(env, ENV_CMD_REENTRANT, descr->reentrant);
    status = (*descr->command_fp)(env, argc, argv);
    NuSMVEnv_set_flag(env, ENV_CMD_REENTRANT, true);

    /* automatic execution of arbitrary command after each command */
    /* usually this is a passive command ... */
    if (status == 0 && ! NuSMVEnv_get_flag(env, ENV_AUTOEXEC)) {
      OptsHandler_ptr opt = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
      if (OptsHandler_is_option_registered(opt, "autoexec")) {
        value = OptsHandler_get_string_option_value(opt, "autoexec");

        nusmv_assert((char*)NULL != value);

        NuSMVEnv_set_flag(env, ENV_AUTOEXEC, true);
        status = Cmd_CommandExecute(env, value);
        NuSMVEnv_set_flag(env, ENV_AUTOEXEC, false);
      }
    }
  } FAIL(errmgr) {
    return(1);
  }

  disarm_signal_andler(env);

  return status;
}

/*!
  \brief Applies alias.

  Applies alias.  If perform a history substitution in expanding
  an alias, remove all the orginal trailing arguments.  For example:<p>

    > alias t rl \!:1<br>
    > t lion.blif  would otherwise expand to   rl lion.blif lion.blif <br>

  \sa optional
*/
static int
apply_alias(NuSMVEnv_ptr env, int * argcp, char *** argvp, int * loop)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  int i, argc, stopit, added, offset, did_subst, subst, status, newc, j;
  char *arg, **argv, **newv;
  CmdAliasDescr_t *alias;
  avl_tree* aliasTable = (avl_tree*)NuSMVEnv_get_value(env, ENV_CMD_ALIAS_TABLE);

  argc = *argcp;
  argv = *argvp;
  stopit = 0;
  for(; *loop < 20; (*loop)++) {
    if (argc == 0) {
      return 0;
    }
    if (stopit != 0 || avl_lookup(aliasTable, argv[0], (char **) &alias) == 0) {
      return 0;
    }
    if (strcmp(argv[0], alias->argv[0]) == 0) {
      stopit = 1;
    }
    FREE(argv[0]);
    added = alias->argc - 1;

    /* shift all the arguments to the right */
    if (added != 0) {
      argv = REALLOC(char *, argv, argc + added);
      for (i = argc - 1; i >= 1; i--) {
        argv[i + added] = argv[i];
      }
      for (i = 1; i <= added; i++) {
        argv[i] = NIL(char);
      }
      argc += added;
    }
    subst = 0;
    for (i = 0, offset = 0; i < alias->argc; i++, offset++) {
      arg = CmdHistorySubstitution(env, alias->argv[i], &did_subst);
      if (arg == NIL(char)) {
        *argcp = argc;
        *argvp = argv;
        return(1);
      }
      if (did_subst != 0) {
        subst = 1;
      }
      status = 0;
      do {
        arg = split_line(env, arg, &newc, &newv);
        /*
         * If there's a complete `;' terminated command in `arg',
         * when split_line() returns arg[0] != '\0'.
         */
        if (arg[0] == '\0') {   /* just a bunch of words */
          break;
        }
        status = apply_alias(env, &newc, &newv, loop);
        if (status == 0) {
          status = com_dispatch(env, newc, newv);
        }
        CmdFreeArgv(newc, newv);
      } while (status == 0);
      if (status != 0) {
        *argcp = argc;
        *argvp = argv;
        return(1);
      }
      added = newc - 1;
      if (added != 0) {
        argv = REALLOC(char *, argv, argc + added);
        for (j = argc - 1; j > offset; j--) {
          argv[j + added] = argv[j];
        }
        argc += added;
      }
      for (j = 0; j <= added; j++) {
        argv[j + offset] = newv[j];
      }
      FREE(newv);
      offset += added;
    }
    if (subst == 1) {
      for (i = offset; i < argc; i++) {
        FREE(argv[i]);
      }
      argc = offset;
    }
    *argcp = argc;
    *argvp = argv;
  }

  StreamMgr_print_error(streams,  "error -- alias loop\n");
  return 1;
}

/*!
  \brief Allows interpolation of variables

  Allows interpolation of variables. Here it is implemented by
  allowing variables to be referred to with the prefix of '$'. The variables
  are set using the "set" command. So for example, the following can be done <p>

  <code>
  NuSMV> set foo bar <br>
  NuSMV> echo $foo <br>
  bar <br>
  </code>

  The last line "bar" will the output produced by NuSMV.

  The following can also be done: <p>

  <code>
  NuSMV> set foo $foo:foobar <br>
  NuSMV> echo $foobar <br>
  bar:foobar <br>
  </code>
  The last line will be the output produced by NuSMV. <p>

  These variables can
  be used in recursive definitions. The following termination characters are
  recognized for the variables \\n, \\0, ' ', \\t,  :,  ;,  #,  /.

  Although the set command allows the usage of the some of the
  above termination characters between quotes,
  the variable interpolation procedure has the restriction
  that the two characters ':' and '/' may not be used with quotes.
  A variable with spaces in it may be used only if it is enclosed
  within quotes.

  \se required

  \sa optional
*/
static void
variableInterpolation(const NuSMVEnv_ptr env, int argc, char **argv)
{
  int i;          /* to iterate through the arguments */
  char *newStr;   /* string returned by the expanded value */
  char dollar;    /* character to store reference to the variable, now '$' */


  dollar = '$';

  /* step through all argvs */
  for (i = 0; i < argc; i++) {
    if (strchr((char *)argv[i], (int)dollar) != NULL) {
      /* expanded string returned by the procedure */
      newStr = variableInterpolationRecur(env, argv[i]);
      FREE(argv[i]);
      /* replace old value with new */
      argv[i] = newStr;
    }
  } /* end of iterating through all arguments */
}/* end of variable interpolation */

/*!
  \brief Recursive procedure that expands the interpolation variables

  Recursive procedure that expands the interpolation variables.
  This procedure is designed to handle multiple occurrences of variables
  in a string and recursive definitions. If the expanded variable has another
  variable, then the procedure is called recursively. The existence of a
  variable is identified by the $ sign in the string. But since this may be
  an environment variable too, the variable is untouched if not found in
  this table. A sophisticated check can be made to see if this variable
  exists in the environment, but it is NOT done here. Therefore, detection
  of bogus values cannot be done. The procedure steps through the string
  to see if any variables are present. If a termination character (one of
  :, /) is found after the '$', then the variable
  is identified and looked up in the flag table. If the returned string again
  has a dollar, then the procedure is called recursively. If not, the returned
  value replaces the variable and the stepping through continues. If the
  variable is not found, then it might be an environment variable.So the
  procedure leaves the variable there.

  \se required

  \sa optional
*/
static char *
variableInterpolationRecur(const NuSMVEnv_ptr env, char *str)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int i;               /* iterators */
  int findEndDollar;   /* flag to denote that a $ has been found. So
                        * search till end of variable
                        */
  int endDollarIndex;  /* index in the current string of the end of the
                        * variable
                        */
  int dollarIndex;     /* index in the current string of the dollar sign */
  int singleQuote;     /* flag that symbolizes that a quote is started */
  int doubleQuote;     /* flag that symbolizes that a quote is started */
  char *value;         /* value of the variable that is returned by the table */
  char *subStr;        /* string to store the variable */
  unsigned int curStrIndex;     /* index to step through the current string */
  int subLen;          /* length of the variable */
  int index;           /* variable use to step through the various strings */
  char *curStr;        /* current string which may change as substitutions
                        * take place
                        */
  char *newCurStr;     /* new string pieced together with the expanded value */
  char c;              /* current character in the string */

  int freeNewValue;    /* new value of string returned by recursion needs
                        * to be freed.
                        */
  char dollar;         /* character that stores the dollar sign */
  int lastPos;         /* position of the last character of the variable
                        * in the string.
                        */
  int envVar;           /* flag to say that the variable is not found in
                         * the table, hence may be an environment variable
                         */

  dollar = '$';
  curStrIndex = 0;
  value = (char*) NULL;
  subLen = 0;
  findEndDollar = 0;
  singleQuote = 0;
  doubleQuote = 0;
  dollarIndex = -1;
  endDollarIndex = -1;
  /* make a local copy since the string may change */
  curStr = ALLOC(char, strlen(str)+1);
  curStr = strncpy(curStr, str, strlen(str)+1);
  /* search through the end of string including te \0 character to detect
   * end of variable, if required.
   */
  while (curStrIndex <= strlen(curStr)) {
    /* current character */
    c = curStr[curStrIndex];

    /* detect a $ if not within quotes */
    if ((c == '$') ) {
      if (findEndDollar == 1) {
        (void)StreamMgr_print_error(streams,  "Cannot have nested $ signs, not found termination\n");
        break;
      }
      /* note the beginning of the dollar position */
      dollarIndex = curStrIndex;
      /* start quest for end of dollar */
      findEndDollar = 1;
      endDollarIndex = -1;
    }
    /* termination characters are \0, :, / when not within quotes.
     * Although, some of these may never be encountered
     * since this is called after split_line and apply_alias
     * Termination characters except '\0' are ignored within quotes
     */
    if ((findEndDollar) &&
        ((c == '\0') ||(c == ':') || (c == '/'))) {
      /*     if (((c == '\n') || (c == '\t') || (isspace(c)) ||
             (c == ':') || (c == ';') || (c == '\0') ||
             (c == '#') || (c == '/')) && (findEndDollar)) { */
      findEndDollar = 0;
      endDollarIndex = curStrIndex;
    } /* end of find termination characters */

    /* found the interpolation variable and its end*/
    if (!findEndDollar && (endDollarIndex != -1)) {
      /* found an interpolation variable */
      subLen = 0;
      freeNewValue = 0;
      envVar = 0;
      subStr = NULL;
      if (endDollarIndex > (dollarIndex +1)) {
        OptsHandler_ptr opt = opts;
        /* if not empty string */
        subStr = ALLOC(char, endDollarIndex - dollarIndex);
        /* copy the variable into another string */
        for ( i = 0; i <  endDollarIndex - dollarIndex - 1; i++) {
          subStr[i] = curStr[dollarIndex+1+i];
        }
        subStr[i] = '\0';
        /* quiet if of the form var$:iable or var$foo:iable and
         * $foo not in flag table
         */
        if (OptsHandler_is_option_registered(opt, subStr)) {

          value = OptsHandler_get_string_representation_option_value(opt,
                                                                     subStr);
          /* NULL strings are returned as "NULL" string (4 chars +
             delimiter) instead of NULL pointer (0x0). */
          nusmv_assert((char*)NULL != value);

          /* found the variable in the alias table */
          if (strchr((char *)value, (int)dollar) != NULL) {
            /* if more $s in the value */
            value = variableInterpolationRecur(env, value);
            subLen = strlen(value);
            /* to be freed later since variableInterpolationRecur
             * returns a new string to be freed later.
             */
            freeNewValue = 1;

          }  else {
            /* if no dollars in the value, substitute the return value
             * in the string
             */
            subLen = strlen(value);
          }
        } else {
          /* if the variable is not found, it might be an
           * environment variable and so keep it. This might be
           * a hazard for bogus variables but that is upto the user.
           */
          value = subStr;
          /* for environment variable keep the $ sign */
          subLen = strlen(value) +1;
          envVar = 1;
        }

      } /* end of interpolation variable not trivial */
      /* prefix + strlen(substituted value) + suffix */
      newCurStr = ALLOC(char, dollarIndex +
                        subLen +
                        strlen(curStr) - endDollarIndex + 1);


      /* copy prefix */
      newCurStr = strncpy(newCurStr, curStr, dollarIndex);
      i = dollarIndex;
      if (subLen) {
        /* copy substituted value */
        if (envVar) {
          /* if it is an environment variable, keep the $ sign */
          newCurStr[i++] = '$';
        }
        index = 0;
        while (value[index] != '\0') {
          newCurStr[i++] = value[index++];
        }
        if (freeNewValue) {
          FREE(value);
        }
      }
      /* freed here cos value might be subStr in one case */
      if (subStr != NULL) {
        FREE(subStr);
      }
      /* copy suffix */
      index = endDollarIndex;
      /* figure out where to start the next search */
      lastPos = i;
      while (curStr[index] != '\0') {
        newCurStr[i++] = curStr[index++];
      }
      newCurStr[i] = '\0';
      FREE(curStr);
      curStr = newCurStr;
      /* reset counter for further search. Due to recursive calling of this
       * function eventually, the value that is substituted will not have a $
       */
      curStrIndex = lastPos;
      dollarIndex = -1;
      endDollarIndex = -1;
      /* end of found a variable */
    } else { /* if a variable is not found, keep going */
      curStrIndex++;
    }
  } /* end of stepping through the string */
  return(curStr);
} /* end of variableInterpolationRecur */

/*!
  \brief required

  optional

  \se required

  \sa optional
*/
static char *
split_line(const NuSMVEnv_ptr env,
           char * command,
           int * argc,
           char *** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  const char rational_prefix = 'f';

  register char *p, *start, c;
  register int i;
  array_t *argv_array;
  boolean single_quote, double_quote;

  argv_array = array_alloc(char *, 5);

  p = command;
  while (true) {
    /* skip leading white space */
    while (isspace(*p)) p++;

    /* skip until end of this token */
    single_quote = double_quote = false;

    for (start = p; (c = *p) != '\0'; p++) {
      if (c == ';' || c == '#' || isspace(c)) {
        if (!single_quote && !double_quote) break;
      }

      if (c == '\'') {
        /* Issue 4575: to handle rationals in the form f'number */
        if ((single_quote || p == start || rational_prefix != *(p-1)) &&
            !double_quote) {
          single_quote = !single_quote;
        }
      }
      else if (c == '\"' && !single_quote) {
        double_quote = !double_quote;
      }
    }
    if (single_quote || double_quote) {
      StreamMgr_print_error(streams,  "ignoring unbalanced quote ...\n");
    }
    if (start == p)
      break;

    { /* constructs the token */
      register char* new_arg = ALLOC(char, p - start + 1);
      register int j = 0;
      /* intially true to left-trim spaces */
      register boolean lastly_added_space = true;

      nusmv_assert(new_arg != (char*) NULL);

      single_quote = double_quote = false;

      for (i = 0; i < p - start; i++) {
        c = start[i];

        if (c == '\'' && !double_quote && (!double_quote ||
          (!single_quote && i > 0 &&
            rational_prefix != start[i-1]))) {
          single_quote = !single_quote;
        } else if (c=='\"' && !single_quote) {
          double_quote = !double_quote;
        }

        if ((c != '\'' || double_quote) && (c != '\"' || single_quote)){
          /* avoid adding multiple spaces */
          if (isspace(c)) {
            if (! lastly_added_space) {
              new_arg[j++] = ' ';
              lastly_added_space = true;
            }
          }
          else {
            new_arg[j++] = c;
            lastly_added_space = false;
          }
        }
      }
      new_arg[j] = '\0';
      array_insert_last(char*, argv_array, new_arg);
    }
  }

  *argc = array_n(argv_array);
  *argv = array_data(char *, argv_array);
  array_free(argv_array);
  if (*p == ';') {
    p++;
  }
  else if (*p == '#') {
    for(; *p != 0; ++p) ;               /* skip to end of line */
  }

  return p;
}

/*!
  \brief required

  optional

  \se required

  \sa optional
*/
static int check_shell_escape(const NuSMVEnv_ptr env, char* p, int* status)
{
#if NUSMV_HAVE_SYSTEM
  const OptsHandler_ptr opt = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  while (isspace(*p)) p++;

  if (OptsHandler_is_option_registered(opt, "shell_char")) {
    const char* tmp = OptsHandler_get_string_option_value(opt, "shell_char");
    NuSMVShellChar = tmp[0];
  }
  if (*p == NuSMVShellChar) {
    *status = system(p+1);
    return 1;
  }
#endif

  return 0;
}


#if NUSMV_HAVE_SIGNAL_H

/*!
  \brief Signal handler.

  \todo Missing description

  \sa com_dispatch
*/
static void
sigterm(int sig)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(__sigterm_env__, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(__sigterm_env__, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(__sigterm_env__, ENV_OPTS_HANDLER));

  StreamMgr_print_output(streams,  "Interrupt\n");
  if (!opt_batch(opts) && !NuSMVEnv_get_flag(__sigterm_env__, ENV_CMD_REENTRANT)) {
    StreamMgr_print_error(streams,
            "Warning: %s status may be not consistent. Use 'reset' "\
            "command if needed.\n", get_pgm_name(opts));
  }

  (void) signal(sig, sigterm);
  ErrorMgr_long_jmp(errmgr);
}
#endif /* NUSMV_HAVE_SIGNAL_H */

/*!
  \brief Enable signal trapping depending on the interactive/batch
  mode.

  \todo Missing description

  \sa com_dispatch
*/
static void arm_signal_andler(const NuSMVEnv_ptr env)
{
  const OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (! opt_batch(opts)) { /* interactive mode */
#if NUSMV_HAVE_SIGNAL_H
    __sigterm_env__ = env;
    (void) signal(SIGINT, sigterm);
#endif
  }
}

/*!
  \brief Enable signal trapping depending on the interactive/batch
  mode.

  \todo Missing description

  \sa com_dispatch
*/
static void disarm_signal_andler(const NuSMVEnv_ptr env)
{
  const OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (! opt_batch(opts)) { /* interactive mode */
#if NUSMV_HAVE_SIGNAL_H
    (void) signal(SIGINT, SIG_IGN);
#endif
  }
}
