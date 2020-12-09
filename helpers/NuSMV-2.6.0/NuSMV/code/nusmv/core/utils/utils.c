/* ---------------------------------------------------------------------------

  This file is part of the ``utils'' package of NuSMV version 2.
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
  \author Roberto Cavada
  \brief Contains useful functions and structures

  \todo: Missing description

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#if NUSMV_HAVE_DIRENT_H
# if NUSMV_HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif
# include <dirent.h>
#endif

#if !NUSMV_HAVE_STRCASECMP
# include <ctype.h>
#endif

#if NUSMV_HAVE_UNISTD_H
#  include <unistd.h>
#endif

#if NUSMV_HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#if defined(_MSC_VER)
#include <io.h>  /* for _access */
#endif

#include <limits.h>
#include <math.h>
#include <stdio.h>

#if defined(_MSC_VER)
/* this is used to substitute stat */
# include "nusmv/core/utils/utils_msvc.h"
#endif

#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/TimerBench.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/utils.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_HASH_TIMERS "hash_timers"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef void (*hash_timers_DESTROY)(TimerBench_ptr);

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/



/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void freeListOfLists_aux(lsList list);

static void hash_timers_init(const NuSMVEnv_ptr env);
static void hash_timers_quit_fun(const NuSMVEnv_ptr env, hash_timers_DESTROY);
static TimerBench_ptr
hash_timers_lookup(const NuSMVEnv_ptr env, const char* key);
static void
hash_timers_insert(const NuSMVEnv_ptr env, const char* key,
                   TimerBench_ptr val);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Utils_pkg_init(const NuSMVEnv_ptr env)
{
  hash_timers_init(env);
}

void Utils_pkg_quit(const NuSMVEnv_ptr env)
{
  hash_timers_quit_fun(env, TimerBench_destroy);
}

const char* Utils_StripPath(const char* pathfname)
{
  const char* old_pos = pathfname;

  while ((*pathfname) != '\0') {
    if ((*pathfname) == '/') old_pos = pathfname + 1;
    ++pathfname;
  }
  return old_pos;
}

void Utils_StripPathNoExtension(const char* fpathname, char* filename)
{
  char* szExt = NULL;
  const char* szNoPath = Utils_StripPath(fpathname);

  /* We are going to use str(n)cpy, we need the two strings to not overlap */
  nusmv_assert(fpathname != filename);

  szExt = strrchr(szNoPath, '.');
  if (szExt != NULL) {
    strncpy(filename, szNoPath, (size_t)(szExt - szNoPath));
    *(filename+(size_t)(szExt - szNoPath)) = '\0'; /* terminates the string */
  }
  else {
    strcpy(filename, szNoPath);
  }
}

void Utils_StripPathNoFilenameNoExtension(const char* fpathname, char* dirname)
{
  int pos = 0;
  int pos_last_sep = 0;

  nusmv_assert(fpathname != (char *) NULL);
  nusmv_assert(dirname != (char *) NULL);
  /* We are going to use str(n)cpy, we need the two strings to not overlap */
  nusmv_assert(fpathname != dirname);

  while(fpathname[pos] != '\0') {
    if (fpathname[pos] == '/') {
      pos_last_sep = pos;
    }
    ++pos;
  }

  strncpy(dirname, fpathname, pos_last_sep);
  dirname[pos_last_sep] = '\0';
}

void Utils_FreeListOfLists(lsList list_of_lists)
{
  lsDestroy(list_of_lists, (void (*)(lsGeneric))&freeListOfLists_aux);
}




/*!
  \brief Return a string to be used as temporary file

  This functions gets a template parameter for the file
  name, with 6 'X' that will be substituted by an unique id. See
  mkstemp for further info. Ensures that the filename is not already
  in use in the given directory. If NULL is passed as the directory,
  then the standard temporary directory is used instead. Returned
  string must be freed. Returtns NULL if the filename cannot be found
  or if we do not have write priviledges in the specified directory.
*/

#define _TEMPDIR "/tmp"
char* Utils_get_temp_filename_in_dir(const char* dir, const char* templ)
{
  char* dirname = (char*) NULL;
  char* name = (char*) NULL;
  char* var;
  int len;

# if NUSMV_HAVE_MKSTEMP
  int fn;
# endif

# if defined(__MINGW32__)
  char dir_separator = '\\';
# else
  char dir_separator = '/';
# endif


  if (dir == NULL) {
    /* 1) Search for the directory */
#   if defined(__MINGW32__)
    var = (char*) NULL;
#   if NUSMV_HAVE_GETENV
    var = getenv("TEMP");
    if (var == (char*) NULL) var = getenv("TMP");
#   endif /* NUSMV_HAVE_GETENV */

    if (var != (char*) NULL) dirname = util_strsav(var);
    else dirname = util_strsav(".");

#   else /* ! defined __MINGW32__ */
    var = (char*) NULL;
#   if NUSMV_HAVE_GETENV
    var = getenv("TEMPDIR");
#   endif /* NUSMV_HAVE_GETENV */

    if (var != (char*) NULL) dirname = util_strsav(var);
    else dirname = util_strsav(_TEMPDIR);
#   endif /* __MINGW32__ */
  }
  else {
    dirname = ALLOC(char, strlen(dir) + 1);
    strcpy(dirname, dir);
    if (dir[strlen(dir) - 1] == dir_separator) {
      dirname[strlen(dir) - 1] = '\0';
    }
  }

  nusmv_assert(dirname != (char*) NULL);


  /* 2) Tries to open the file: */
  len = strlen(dirname) + 1 + strlen(templ) + 1;
  name = ALLOC(char, len);
  nusmv_assert(name != (char*) NULL);

  snprintf(name, len, "%s%c%s", dirname, dir_separator, templ);
  FREE(dirname);

# if NUSMV_HAVE_MKSTEMP
  fn = mkstemp(name);
  if (fn == -1) {
    /* tries with the current dir */
    sprintf(name, "%s", templ);
    fn = mkstemp(name);
    if (fn == -1) {
      /* no way */
      FREE(name);
      name = (char*) NULL;
    }
  }

  if (name != (char*) NULL) {
    nusmv_assert(fn != -1);
    close(fn);
    /* the file created needs to be removed */
    if (remove(name) == -1) {
      FREE(name);
      name = (char*) NULL;
    }
  }

#elif NUSMV_HAVE_MKTEMP
  if (mktemp(name) == (char*) NULL) {
    /* tries with the current dir */
    sprintf(name, "%s", templ);
    if (mktemp(name) == (char*) NULL) {
      /* no way */
      FREE(name);
      name = (char*) NULL;
    }
  }
#elif NUSMV_HAVE_TMPNAM
  #include <stdio.h>
  if (tmpnamp(name) == (char*) NULL) {
    /* tries with the current dir */
    sprintf(name, "%s", templ);
    if (mktemp(name) == (char*) NULL) {
      /* no way */
      FREE(name);
      name = (char*) NULL;
    }
  }
#else /* no support from OS */
  snprintf(name, len, "TMP%d", utils_random());
#endif

  return name;
}

int Utils_file_exists_in_paths(const char* filename,
                               const char* paths,
                               const char* delimiters)
{
  int result = 0;

#if NUSMV_HAVE_DIRENT_H
  char pathscopy[strlen(paths) + 1];
  char* dir;

  strcpy(pathscopy, paths);
  dir = strtok(pathscopy, delimiters);

  while ((0 == result) && (dir != NULL)) {
    result = Utils_file_exists_in_directory(filename, dir);
    dir = strtok(NULL, delimiters);
  }
#else
  result = 2;
#endif

  return result;
}

boolean Utils_file_exists(const char* filename)
{
#if NUSMV_HAVE_UNISTD_H
  return (access(filename, F_OK) == 0);
#elif defined(_MSC_VER)
  return (_access(filename, 0) == 0);
#else
  /* fallback to using fopen */
  boolean res = false;
  FILE* fp = fopen(filename, "r");
  if ((FILE*) NULL != fp) {
    res = true;
    fclose(fp);
  }
  return res;
#endif
}

boolean Utils_file_can_be_written(const char* filename)
{
  boolean res = false;

  /* we open for appending to avoid truncating a previous file */
  FILE* fp = fopen(filename, "a");
  if ((FILE*) NULL != fp) {
    res = true;
    fclose(fp);
  }
  return res;
}


boolean Utils_files_are_the_same(const char* fname1, const char* fname2)
{
#if defined(_MSC_VER)
  int res = Utils_msvc_files_are_the_same(fname1, fname2);
  /* one or both do not exist */
  return res == 1;

#elif NUSMV_HAVE_SYS_STAT_H
  struct stat ss1, ss2;
  int s1 = stat(fname1, &ss1);
  int s2 = stat(fname2, &ss2);

  return (0 == s1 && 0 == s2 &&
          ss1.st_ino == ss2.st_ino);

#else
# error "Did not find a way to compile Utils_files_are_the_same"
#endif
}

boolean Utils_exe_file_exists(const char* filename)
{
#if NUSMV_HAVE_UNISTD_H
  return (access(filename, X_OK) == 0);
#else
  /* fallback to using stat */
# if NUSMV_HAVE_SYS_STAT_H && !defined(_MSC_VER)
  /* it seems that it is not trivial to check if a file is
     executable under windows... */
  struct stat st_stat;
  int pmask;
  int r = stat(filename, &st_stat);
  if (r)
      return false;
  pmask = (S_IXUSR | S_IXGRP | S_IXOTH);
  return (S_IFREG & st_stat.st_mode) & (pmask & st_stat.st_mode);
#  else
  /* final fall back to file_exist, and see at runtime if execution
     fails... */
  return Utils_file_exists(filename);
# endif
#endif
}

int Utils_file_exists_in_directory(const char* filename, char* directory)
{
  int fileexists = 0;

#if NUSMV_HAVE_DIRENT_H
  struct dirent *dirfile;
  int l1 = strlen(filename);

  DIR* dir = opendir(directory);

  if (dir != NULL){
    while ((0 == fileexists) && (dirfile = readdir(dir))) {
      if (strlen(dirfile->d_name) == l1) {
        if (strcmp(filename, dirfile->d_name) == 0) {
          fileexists = 1;
        }
        else fileexists = 0;
      }
    }
    (void) closedir(dir);
  }
#else
  fileexists = 2;
#endif

  return fileexists;
}

int Utils_strcasecmp(const char* s1, const char* s2)
{
#if NUSMV_HAVE_STRCASECMP
  return strcasecmp(s1, s2);
#else
  int res = 0;

  while (1) {
    int c1 = tolower(*s1);
    int c2 = tolower(*s2);

    if (c1 != c2) {
      if (c1 < c2) return -1;
      return 1;
    }

    if (*s1 == '\0') break;
    s1 += 1; s2 += 1;
  }

  return 0;
#endif
}

void Utils_start_timer(const NuSMVEnv_ptr env, const char* name)
{
  TimerBench_ptr timer = hash_timers_lookup(env, name);

  if (timer == TIMER_BENCH(NULL)) {
    // timer must be created
    timer = TimerBench_create(name);
    hash_timers_insert(env, name, timer);
  }

  TimerBench_start(timer);
}

void Utils_stop_timer(const NuSMVEnv_ptr env, const char* name)
{
  TimerBench_ptr timer = hash_timers_lookup(env, name);
  TIMER_BENCH_CHECK_INSTANCE(timer);

  TimerBench_stop(timer);
}

void Utils_reset_timer(const NuSMVEnv_ptr env, const char* name)
{
  TimerBench_ptr timer = hash_timers_lookup(env, name);
  TIMER_BENCH_CHECK_INSTANCE(timer);

  TimerBench_reset(timer);
}

void Utils_print_timer(const NuSMVEnv_ptr env, const char* name,
                       const char* msg)
{
  TimerBench_ptr timer = hash_timers_lookup(env, name);
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  TIMER_BENCH_CHECK_INSTANCE(timer);

  TimerBench_print(timer, errstream, msg);
}

void Utils_str_escape_xml_file(const char* str, FILE* file)
{
  /* this table is used to associate a character to a string, for
     character escaping

     IMPORTANT!! This table if changed has to be updated along with
     escape_table_begin and escape_table_end
  */
  static char escape_table_begin = '\t';
  static char escape_table_end = '>';
  static char* escape_table[] = {
    "&#009;",  /* '\t' */
    "&#010;",  /* '\n' */
    "&#011;",  /* '\v' */
    "&#012;",  /* '\f' */
    "&#013;",  /* '\r' */
    "&#014;",  /* SO   */
    "&#015;",  /* SI   */
    "&#016;",  /* DLE  */
    "&#017;",  /* DC1  */
    "&#018;",  /* DC2  */
    "&#019;",  /* DC3  */
    "&#020;",  /* DC4  */
    "&#021;",  /* NAK  */
    "&#022;",  /* SYN  */
    "&#023;",  /* ETB  */
    "&#024;",  /* CAN  */
    "&#025;",  /* EM   */
    "&#026;",  /* SUB  */
    "&#027;",  /* ESC  */
    "&#028;",  /* FS   */
    "&#029;",  /* GS   */
    "&#030;",  /* RS   */
    "&#031;",  /* US   */
    "&#032;",  /* ' '  */
    "&#033;",  /* '!'  */
    "&quot;",  /* '"'  */
    "#",       /* '#'  */
    "$",       /* '$'  */
    "%",       /* '%'  */
    "&amp;",   /* '&'  */
    "&apos;",  /* '''  */
    "(",       /* '('  */
    ")",       /* ')'  */
    "*",       /* '*'  */
    "+",       /* '+'  */
    ",",       /* ','  */
    "-",       /* '-'  */
    ".",       /* '.'  */
    "/",       /* '/'  */
    "0",       /* '0'  */
    "1",       /* '1'  */
    "2",       /* '2'  */
    "3",       /* '3'  */
    "4",       /* '4'  */
    "5",       /* '5'  */
    "6",       /* '6'  */
    "7",       /* '7'  */
    "8",       /* '8'  */
    "9",       /* '9'  */
    ":",       /* ':'  */
    ";",       /* ';'  */
    "&lt;",    /* '<'  */
    "=",       /* '='  */
    "&gt;",    /* '>'  */
  };

  if ((char*) NULL != str) {
    const char* iter;
    char c;
    for (iter=str, c=*iter; c != '\0'; c=*(++iter)) {
      if (escape_table_begin <= c && c <= escape_table_end) {
        /* in table */
        fputs(escape_table[c-escape_table_begin], file);
      }
      else { /* not in table */
        fputc(c, file);
      }
    }
  }
}

int Utils_log2_round(unsigned long long int a)
{
  int res;

  if (0 == a) return 1; /* special case */

  res = 0;
  while (a != 0) {
    a >>= 1;
    ++res;
  }
  return res;
}

int Utils_ptr_compar(const void* a, const void* b)
{
  nusmv_assert(NULL != a);
  nusmv_assert(NULL != b);

  return *(char**)a - *(char**)b;
}

char* Utils_int_to_str(const int an_int)
{
  char* str = NULL;
  size_t buffsize = 0;
  int c = -1;

  buffsize = Utils_int_size_as_string(an_int);
  str = ALLOC(char, buffsize);
  c = snprintf(str, buffsize, "%d", an_int);
  SNPRINTF_CHECK(c, buffsize);

  return str;
}

size_t Utils_int_size_as_string(const int an_int)
{
  size_t retval;

  retval = (int)(ceil(log10((double)an_int))+2)*sizeof(char);

  return retval;
}

char* Utils_util_tilde_expand(char* fname)
{
  nusmv_assert(NULL != fname);

  return util_tilde_expand(fname);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/



/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Private service for Utils_FreeListOfLists

  \todo Missing description

  \sa Utils_FreeListOfLists
*/
static void freeListOfLists_aux(lsList list)
{
  lsDestroy(list, NULL);
}

/*!
  \brief Initializes the hash_timers hash

  Initializes the hash_timers hash
*/
static void hash_timers_init(const NuSMVEnv_ptr env)
{
  hash_ptr hash_timers = new_assoc();
  nusmv_assert((hash_ptr)NULL != hash_timers);

  NuSMVEnv_set_value(env, ENV_HASH_TIMERS, hash_timers);
}

/*!
  \brief


*/

static enum st_retval hash_timers_quit_fun_aux(char*k, char* e, char* a)
{
  /* WARNING [MD] I think it should be (hash_timers_DESTROY)a */
  hash_timers_DESTROY fun = *(hash_timers_DESTROY*)a;
  fun((TimerBench_ptr)e);
  return ASSOC_DELETE;
}

/*!
  \brief Deinitializes the hash_timers hash

  Deinitializes the hash_timers hash
*/
static void hash_timers_quit_fun(const NuSMVEnv_ptr env,
                                 hash_timers_DESTROY fun)
{
  hash_ptr hash_timers = (hash_ptr)NuSMVEnv_remove_value(env, ENV_HASH_TIMERS);

  clear_assoc_and_free_entries_arg(hash_timers,
                                   hash_timers_quit_fun_aux,
                                   (char*)&fun);
  free_assoc(hash_timers);
}

/*!
  \brief Looks up in the hash_timers hash

  Looks up in the hash_timers hash
*/
static TimerBench_ptr
hash_timers_lookup(const NuSMVEnv_ptr env, const char* key)
{
  hash_ptr hash_timers = (hash_ptr)NuSMVEnv_get_value(env, ENV_HASH_TIMERS);

  return (TimerBench_ptr)find_assoc(hash_timers, (node_ptr)key);
}

/*!
  \brief Inserts into the hash_timers hash

  Inserts into the hash_timers hash
*/
static void hash_timers_insert(const NuSMVEnv_ptr env, const char* key,
                               TimerBench_ptr val)
{
  hash_ptr hash_timers = (hash_ptr)NuSMVEnv_get_value(env, ENV_HASH_TIMERS);

  insert_assoc(hash_timers, (node_ptr)key, (node_ptr)val);
}
