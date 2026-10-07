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
  \brief File open, and file completion.

  \todo: Missing description

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include <stdio.h>

#include "nusmv/shell/cmd/cmdInt.h"
#include "nusmv/shell/cmd/cmd.h"

#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/StreamMgr.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ESC     '\033'

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BEEP    '\007'

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define HIST    '%'

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define SUBST   '^'

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define STDIN   0

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define STDOUT  1

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define MAX_BUF 65536

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_CMD_HISTORY_CHAR "cmdhistchr" /* can be changed by "set hist_char" */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
static const char *seperator = " \t\n;";

#if NUSMV_HAVE_LIBREADLINE
/* This variable needs to be updated before every readline() call */
avl_tree* __rl_cmd_table__ = (avl_tree*)NULL;
#endif

#if ! NUSMV_HAVE_ISATTY
static inline int isatty(int d) { return 0; }
#endif

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

#if defined NUSMV_HAVE_IOCTL_WITH_TIOCGETC && NUSMV_HAVE_IOCTL_WITH_TIOCGETC
static int cmp(char ** s1, char ** s2);
static int match(char * newmatch, char * lastmatch, char * actual);
#endif
static int getnum(char ** linep);
static char * getarg(char * line, int num);
static char * bad_event(StreamMgr_ptr streams, int n);
static char * do_subst(char * dest, char * new);
static void print_prompt(const NuSMVEnv_ptr env, array_t* historyArray, char * prompt);

#if NUSMV_HAVE_LIBREADLINE
static char *removeWhiteSpaces(char *string);
#endif

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

FILE *
Cmd_FileOpen(const NuSMVEnv_ptr env,
             char * fileName,
             char * mode,
             char ** realFileName_p,
             int  silent)
{
  char *realFileName, *path, *user_path = NIL(char);
  char *lib_name;
  FILE *fp;
  const OptsHandler_ptr opt = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  nusmv_assert(NULL != fileName);

  if (strcmp(fileName, "-") == 0) {
    if (strcmp(mode, "w") == 0) {
      realFileName = util_strsav("stdout");
      fp = stdout;
    }
    else {
      realFileName = util_strsav("stdin");
      fp = stdin;
    }
  }
  else {
    realFileName = NIL(char);
    if (strcmp(mode, "r") == 0) {

      if (OptsHandler_is_option_registered(opt, "open_path")) {
        user_path = OptsHandler_get_string_option_value(opt, "open_path");
      }

      if (user_path != NIL(char)) {
        lib_name = CInit_NuSMVObtainLibrary();
        path = ALLOC(char, strlen(user_path)+strlen(lib_name)+10);
        (void) sprintf(path, "%s:%s", user_path, lib_name);

        /*
         * If the fileName begins with ./, ../, ~/, or /, AND the file doesn't
         * actually exist, then NuSMV will look in the open path (which includes
         * the sis library) for the file.  This could lead to unexpected behavior:
         * the user is looking for ./msu.genlib, and since that isn't there, the
         * users gets sis_lib/msu.genlib, and no error is reported.  The following
         * pseudo code fixes this:
         *
         * if (the beginning of file_name is : ./ || ../ || ~/ || /) {
         * realFileName = util_file_search(fileName, NIL(char), "r");
         * } else
         */
        realFileName = util_file_search(fileName, path, "r");
        FREE(path);
        FREE(lib_name);
      }
    }
    if (realFileName == NIL(char)) {
      realFileName = util_tilde_expand(fileName);
    }
    if ((fp = fopen(realFileName, mode)) == NIL(FILE)) {
      if (! silent) {
#if NUSMV_HAVE_ERRNO_H
        perror(realFileName);
#else
        StreamMgr_print_error(streams,
                "File '%s': an error occurred during file open, but the " \
                "system does not support a better identification\n",
                realFileName);
#endif
      }
    }
  }
  if (realFileName_p != 0) {
    *realFileName_p = realFileName;
  }
  else {
    FREE(realFileName);
  }
  return fp;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal and static functions                               */
/*---------------------------------------------------------------------------*/

#if defined NUSMV_HAVE_IOCTL_WITH_TIOCGETC && NUSMV_HAVE_IOCTL_WITH_TIOCGETC

/*
 * Words are seperated by any of the characters in `seperator'.  The seperator
 * is used to distinguish words from each other in file completion and history
 * substitution. The recommeded seperator string is " \t\n;".
 */

char* CmdFgetsFilec(NuSMVEnv_ptr env, char* buf, unsigned int size, FILE* stream, char* prompt)
{
  int n_read, i, len, maxlen, col, sno, modname;
  struct tchars tchars, oldtchars;
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  array_t* historyArray = (array_t*)NuSMVEnv_get_value(env, ENV_CMD_COMMAND_HISTORY);
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  DIR *dir;
  struct dirent *dp;
#if NUSMV_HAVE_LIBREADLINE
  char *dupline;
  char *cleanLine;
#endif

#if NUSMV_HAVE_TERM_INTERRUPTS
  int omask;
  struct sgttyb tty, oldtty;    /* To mask interuupts */
  int pending = LPENDIN;
#endif

  char *last_word, *file, *path, *name, *line;
  char last_char, found[MAXNAMLEN];
  array_t *names = NIL(array_t);  /* initialize so that lint doesn't complain */

#if (defined __CYGWIN32__) || (defined __MINGW32__)
  fflush(stdout);
  fflush(stderr);
#endif

  sno = fileno(stream);
  if (sno != STDIN || !isatty(sno)) {
    print_prompt(env, historyArray, prompt);
    return fgets(buf, size, stream);
  }
  else if (!OptsHandler_is_option_registered(opt, "filec")) {
#if NUSMV_HAVE_LIBREADLINE
    avl_tree* commandTable =
      (avl_tree*)NuSMVEnv_get_value(env, ENV_CMD_COMMAND_TABLE);

    /* Effectively read one line of input printing the prompt */
    /* Ignore ^D */
    __rl_cmd_table__ = commandTable;
    while ( NIL(char) == (dupline = (char*) readline(prompt)) );
    cleanLine = removeWhiteSpaces(dupline);

    /* Check if an EOF has been read */
    if (cleanLine != NIL(char)) {
      /* If the line is non empty, add it to the history */
      if (*cleanLine) {
        add_history(cleanLine);
      }

      /* Copy the contents of cleanLine to buf, to simulate fgets */
      strncpy(buf, cleanLine, size);
      if (strlen(cleanLine) >= size) {
        buf[size-1] = '\0';
      }
      line = buf;
    }
    else {
      line = NIL(char);
    }
    FREE(dupline);
#else
    /* Get rid of the trailing newline */
    print_prompt(env, historyArray, prompt);

    line = fgets(buf, size, stream);
    if (line != NIL(char)) {
      len = strlen(line);
      if (len > 0 && line[len-1] == '\n') {
        line[len-1] = '\0';
      }
    }
#endif
    return line;
  }
  else {
    print_prompt(env, historyArray, prompt);
  }

  /* Allow hitting ESCAPE to break a read() */

  (void) ioctl(sno, TIOCGETC, (char *) &tchars);
  oldtchars = tchars;
  tchars.t_brkc = ESC;
  (void) ioctl(sno, TIOCSETC, (char *) &tchars);

  while ((n_read = read(sno, buf, size)) > 0) {
    buf[n_read] = '\0';
    last_word = &buf[n_read - 1];
    last_char = *last_word;
    if (last_char == '\n' || n_read == size) {
      (void) ioctl(sno, TIOCSETC, (char *) &oldtchars);
      *last_word = '\0';
      return(buf);
    }
    if (last_char == ESC) {
      *last_word-- = '\0';
      StreamMgr_print_output(streams,  "\b\b  \b\b");
    }
    else {
      names = array_alloc(char *, 10);
      (void) fputc('\n', stdout);
    }
    for (; last_word >= buf; --last_word) {
      if (strchr(seperator, *last_word) != NIL(char)) {
        break;
      }
    }
    last_word++;
    file = strrchr(buf, '/');
    if (file == NIL(char)) {
      file = last_word;
      modname = 0;
      path = ".";
    }
    else {
      *file++ = '\0';
      modname = 1;
      path = (*last_word == '~') ? util_tilde_expand(last_word) :
          last_word;
    }
    len = strlen(file);
    dir = opendir(path);
    if (dir == NIL(DIR) || len > MAXNAMLEN) {
      (void) fputc(BEEP, stdout);
    }
    else {
      *found = '\0';
      maxlen = 0;
      while ((dp = readdir(dir)) != NIL(struct dirent)) {
        if (strncmp(file, dp->d_name, len) == 0) {
          if (last_char == ESC) {
            if (match(dp->d_name, found, file) == 0) {
              break;
            }
          }
          else if (len != 0 || *(dp->d_name) != '.') {
            if (maxlen < NAMLEN(dp)) {
              maxlen = NAMLEN(dp);
            }
            array_insert_last(char *, names, util_strsav(dp->d_name));
          }
        }
      }
      (void) closedir(dir);
        if (last_char == ESC) {
          if (*found == '\0' || strcmp(found, file) == 0) {
            (void) fputc(BEEP, stdout);
          }
          else {
            (void) strcpy(file, found);
            StreamMgr_print_output(streams,  "%s", &buf[n_read - 1]);
          }
        }
        else {
          maxlen += 2;
          col = maxlen;
          array_sort(names, cmp);
          for (i = 0; i < array_n(names); i++) {
            name = array_fetch(char *, names, i);
            StreamMgr_print_output(streams,  "%-*s", maxlen, name);
            FREE(name);
            col += maxlen;
            if (col >= 80) {
              col = maxlen;
              (void) fputc('\n', stdout);
            }
          }
          array_free(names);
          if (col != maxlen) {
            (void) fputc('\n', stdout);
          }
        }
      }
      (void) fflush(stdout);
      if (modname != 0) {
        if (path != last_word) {
          FREE(path);
        }
        *--file = '/';
      }

#if NUSMV_HAVE_TERM_INTERRUPTS
      /* mask interrupts temporarily */
      omask = sigblock(sigmask(SIGINT));
      (void) ioctl(STDOUT, TIOCGETP, (char *)&tty);
      oldtty = tty;
      tty.sg_flags &= ~(ECHO|CRMOD);
      (void) ioctl(STDOUT, TIOCSETN, (char *)&tty);
#endif

      /* reprint prompt */
      (void) write(STDOUT, "\r", 1);
      print_prompt(env, historyArray, prompt);

      /* shove chars from buf back into the input queue */
      for (i = 0; buf[i]; i++) {
        (void) ioctl(STDOUT, TIOCSTI, &buf[i]);
      }
#if NUSMV_HAVE_TERM_INTERRUPTS
      /* restore interrupts */
      (void) ioctl(STDOUT, TIOCSETN, (char *)&oldtty);
      (void) sigsetmask(omask);
      (void) ioctl(STDOUT, TIOCLBIS, (char *) &pending);
#endif
    }
    /* restore read() behavior */
    (void) ioctl(sno, TIOCSETC, (char *) &oldtchars);
    return(NIL(char));
}

#else

/*!
  \brief required

  optional

  \se required

  \sa optional
*/

char* CmdFgetsFilec(NuSMVEnv_ptr env, char* buf, unsigned int size, FILE* stream, char* prompt)
{
  array_t* historyArray = (array_t*)NuSMVEnv_get_value(env, ENV_CMD_COMMAND_HISTORY);

#if NUSMV_HAVE_LIBREADLINE
  char *dupline;
  char *cleanLine;
#endif
  char *line;
  int sno;
#if !NUSMV_HAVE_LIBREADLINE
  int len;
#endif

#if (defined __CYGWIN32__) || (defined __MINGW32__)
  fflush(stdout);
  fflush(stderr);
#endif

  sno = fileno(stream);
  if (sno != STDIN || !isatty(sno)) {
    print_prompt(env, historyArray, prompt);
    return (fgets(buf, size, stream));
  }
  else {
#if NUSMV_HAVE_LIBREADLINE
    avl_tree* commandTable =
      (avl_tree*)NuSMVEnv_get_value(env, ENV_CMD_COMMAND_TABLE);

    /* Effectively read one line of input printing the prompt */
    /* Ignore ^D */
    __rl_cmd_table__ = commandTable;
    while ( NIL(char) == (dupline = (char*) readline(prompt)));
    cleanLine = removeWhiteSpaces(dupline);

    /* Check if an EOF has been read */
    if (cleanLine != NIL(char)) {
      /* If the line is non empty, add it to the history */
      if (*cleanLine) {
        add_history(cleanLine);
      }

      /* Copy the contents of cleanLine to buf, to simulate fgets */
      strncpy(buf, cleanLine, size);
      if (strlen(cleanLine) >= size) {
        buf[size-1] = '\0';
      }
      line = buf;
    }
    else {
      line = NIL(char);
    }
    FREE(dupline);
#else
    /* Get rid of the trailing newline */
    print_prompt(env, historyArray, prompt);
    line = fgets(buf, size, stream);
    if (line != NIL(char)) {
      len = strlen(line);
      if (len > 0 && line[len-1] == '\n') {
        line[len-1] = '\0';
      }
    }
#endif
    return line;
  }

}

#endif /* NUSMV_HAVE_IOCTL_WITH_TIOCGETC */


#if defined NUSMV_HAVE_IOCTL_WITH_TIOCGETC && NUSMV_HAVE_IOCTL_WITH_TIOCGETC
/*!
  \brief required

  optional

  \se required

  \sa optional
*/

static int
cmp(
  char ** s1,
  char ** s2)
{
    return(strcmp(*s1, *s2));
}


/*!
  \brief required

  optional

  \se required

  \sa optional
*/

static int
match(
  char * newmatch,
  char * lastmatch,
  char * actual)
{
  int i = 0;

  if (*actual == '\0' && *newmatch == '.') {
    return(1);
  }
  if (*lastmatch == '\0') {
    (void) strcpy(lastmatch, newmatch);
    return(1);
  }
  while (*newmatch++ == *lastmatch) {
    lastmatch++;
    i++;
  }
  *lastmatch = '\0';
  return(i);
}
#endif /* NUSMV_HAVE_IOCTL_WITH_TIOCGETC */

char * CmdHistorySubstitution(NuSMVEnv_ptr env, char * line, int * changed)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  array_t* cmdHistoryArray = (array_t*)NuSMVEnv_get_value(env, ENV_CMD_COMMAND_HISTORY);

  static char buf[MAX_BUF], c;
  char *last, *old, *new, *start, *b, *l;
  int n, len, i, num, internal_change;
  OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  char NuSMVHistChar = HIST;

  if (NuSMVEnv_has_value(env, ENV_CMD_HISTORY_CHAR)) {
    NuSMVHistChar = PTR_TO_INT(NuSMVEnv_get_value(env, ENV_CMD_HISTORY_CHAR));
  }

  *changed = 0;
  internal_change = 0;
  while (isspace(*line)) {
    line++;
  }
  if (*line == '\0') {
    return(line);
  }
  n = array_n(cmdHistoryArray);
  last = (n > 0) ? array_fetch(char *, cmdHistoryArray, n - 1) : "";

  b = buf;
  if (*line == SUBST) {
    old = line + 1;
    new = strchr(old, SUBST);
    if (new == NIL(char)) {
      goto bad_modify;
    }
    *new++ = '\0';                    /* makes change in contents of line */
    start = strstr(last, old);
    if (start == NIL(char)) {
      *--new = SUBST;
      bad_modify:
      StreamMgr_print_error(streams,  "Modifier failed\n");
      return(NIL(char));
    }
    while (last != start) {
      *b++ = *last++;
    }
    b = do_subst(b, new);
    last += strlen(old);
    while ((*b++ = *last++)) {
    }
    *changed = 1;
    return(buf);
  }

  if (OptsHandler_is_option_registered(opts, "history_char")) {
    const char* tmp = OptsHandler_get_string_option_value(opts, "history_char");
    NuSMVHistChar = tmp[0];

    NuSMVEnv_set_or_replace_value(env, ENV_CMD_HISTORY_CHAR,
                                  PTR_FROM_INT(void*, NuSMVHistChar));
  }

  for (l = line; (*b = *l); l++) {
    if (*l == NuSMVHistChar) {
      /*
       * If a \ immediately preceeds a HIST char, pass just HIST char
       * Otherwise pass both \ and the character.
       */
      if (l > line && l[-1] == '\\') {
        b[-1] = NuSMVHistChar;
        internal_change = 1;
        continue;
      }
      if (n == 0) {
        return(bad_event(streams, 0));
      }
      l++;
      /* Cannot use a switch since the history char is a variable !!! */
      if (*l == NuSMVHistChar){
        /* replace !! in line with last */
        b = do_subst(b, last);
      }
      else if (*l == '$'){
        /* replace !$ in line with last arg of last */
        b = do_subst(b, getarg(last, -1));
      }
      else if (*l == '*'){
        b = do_subst(b, getarg(last, -2));
      }
      else if (*l == ':'){
        /* replace !:n in line with n'th arg of last */
        l++;
        num = getnum(&l);
        new = getarg(last, num);
        if (new == NIL(char)) {
          StreamMgr_print_error(streams,  "Bad %c arg selector\n", NuSMVHistChar);
          return(NIL(char));
        }
        b = do_subst(b, new);
      }
      else if (*l == '-'){
        /* replace !-n in line with n'th prev cmd */
        l++;
        num = getnum(&l);
        if (num > n || num == 0) {
          return(bad_event(streams, n - num + 1));
        }
        b = do_subst(b, array_fetch(char *, cmdHistoryArray, n - num));
      }
      else {
        /* replace !n in line with n'th command */
        if (isdigit(*l)) {
          num = getnum(&l);
          if (num > n || num == 0) {
            return(bad_event(streams, num));
          }
          b = do_subst(b, array_fetch(char *, cmdHistoryArray, num - 1));
        }
        else {  /* replace !boo w/ last cmd beginning w/ boo */
          start = l;
          while (*l && strchr(seperator, *l) == NIL(char)) {
            l++;
          }
          c = *l;
          *l = '\0';
          len = strlen(start);
          for (i = n - 1; i >= 0; i--) {
            old = array_fetch(char *, cmdHistoryArray, i);
            if (strncmp(old, start, len) == 0) {
              b = do_subst(b, old);
              break;
            }
          }
          if (i < 0) {
            StreamMgr_print_error(streams,  "Event not found: %s\n", start);
            *l = c;
            return(NIL(char));
          }
          *l-- = c;

        }
      }
      *changed = 1;
    }
    else {
      b++;
    }
  }
  if (*changed != 0 || internal_change != 0) {
    return(buf);
  }
  return(line);
}


/*!
  \brief required

  optional

  \se required

  \sa optional
*/

static int
getnum(
  char ** linep)
{
  int num = 0;
  char *line = *linep;

  for (; isdigit(*line); line++) {
    num *= 10;
    num += *line - '0';
  }
  *linep = line - 1;
  return(num);
}


/*!
  \brief required

  optional

  \se required

  \sa optional
*/

static char *
getarg(
  char * line,
  int  num)
{
  static char buf[128];
  char *b, *c;
  int i;

  if (num == -1) {
    i = 123456;
  }
  else if (num == -2) {
    i = 1;
  }
  else {
    i = num;
  }

  c = line;
  do {
    b = line = c;
    while (*line && strchr(seperator, *line) == NIL(char)) {
      line++;
    }
    c = line;
    while (*c && strchr(seperator, *c) != NIL(char)) {
      c++;
    }
    if (*c == '\0') {
      break;
    }
  } while (--i >= 0);

  if (i > 0) {
    if (num == -1) {
      return(b);
    }
    return(NIL(char));
  }
  if (num < 0) {
    return(b);
  }
  c = buf;
  do {
    *c++ = *b++;
  } while (b < line && c < &buf[127]);
  *c = '\0';
  return(buf);
}


/*!
  \brief required

  optional

  \se required

  \sa optional
*/

static char *
bad_event(StreamMgr_ptr streams, int  n)
{
  StreamMgr_print_error(streams,  "Event %d not found\n", n);
  return(NIL(char));
}


/*!
  \brief required

  optional

  \se required

  \sa optional
*/

static char *
do_subst(
  char * dest,
  char * new)
{
  while ((*dest = *new++)) {
    dest++;
  }
  return(dest);
}


/*!
  \brief required

  optional

  \se required

  \sa optional
*/

static void print_prompt(const NuSMVEnv_ptr env, array_t* historyArray, char* prompt)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  char NuSMVHistChar = HIST;
  char buf[256];

  if (prompt == NIL(char)) return;

  if (NuSMVEnv_has_value(env, ENV_CMD_HISTORY_CHAR)) {
    NuSMVHistChar = PTR_TO_INT(NuSMVEnv_get_value(env, ENV_CMD_HISTORY_CHAR));
  }

  /* Here I added this calls to fflush because fprintf is buffered and the
     function write() uses a different buffer, which may lead the prompt being
     output before some command output. See issue 2189. */
  StreamMgr_flush_streams(streams);

  while (*prompt != '\0') {
    if (*prompt == NuSMVHistChar) {
      int c = snprintf(buf, sizeof(buf), "%d", array_n(historyArray) + 1);
      SNPRINTF_CHECK(c, sizeof(buf));
      write(STDOUT, buf, strlen(buf));
    }
    else {
      (void) write(STDOUT, prompt, 1);
    }
    prompt++;
  }
  fflush(outstream);
}

#if NUSMV_HAVE_LIBREADLINE
/*!
  \brief Removes tabs and spaces from the beginning and end of string.

  \todo Missing description
*/

static char *
removeWhiteSpaces(
  char *string)
{
  char *left;
  char *right;

  /* Traverse the beginning of the string */
  for (left = string; *left == ' ' || *left == '\t'; left++);

  /* If we reached the end of the string */
  if (*left == 0) {
    return left;
  }

  /* Traverse the end of the string */
  right = left + strlen(left) - 1;
  while (right > left && (*right == ' ' || *right == '\t')) {
    right--;
  }
  /* Set the new end of string */
  *++right = '\0';

  return left;
} /* End of removeWhiteSpaces */
#endif
