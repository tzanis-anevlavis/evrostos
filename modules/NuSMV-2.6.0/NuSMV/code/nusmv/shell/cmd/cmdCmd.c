/* ---------------------------------------------------------------------------


  This file is part of the ``'' package.
  %COPYRIGHT%


-----------------------------------------------------------------------------*/

/*!
  \author Michele Dorigatti
  \brief \todo: Missing synopsis

  \todo: Missing description

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/shell/cmd/cmdInt.h"
#include "nusmv/shell/cmd/cmdCmd.h"
#include "nusmv/shell/cmd/cmdCmdInt.h"

#include "nusmv/core/utils/OStream.h"
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
#define MAX_STR         65536

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


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
static int CommandTime(NuSMVEnv_ptr env, int argc, char** argv);
static int CommandEcho(NuSMVEnv_ptr env, int argc, char** argv);
static int CommandMemoryProfile(NuSMVEnv_ptr env, int argc, char** argv);
static int CommandQuit(NuSMVEnv_ptr env, int argc, char** argv);
static int CommandUsage(NuSMVEnv_ptr env, int argc, char** argv);
static int CommandWhich(NuSMVEnv_ptr env, int argc, char** argv);
static int CommandHistory(NuSMVEnv_ptr env, int argc, char** argv);
static int CommandAlias(NuSMVEnv_ptr env, int argc, char** argv);
static int CommandUnalias(NuSMVEnv_ptr env, int argc, char** argv);
static int CommandHelp(NuSMVEnv_ptr env, int argc, char** argv);
static int CommandSource(NuSMVEnv_ptr env, int argc, char** argv);
static int CommandShowHelp(NuSMVEnv_ptr env, int argc, char** argv);

static void print_alias(const NuSMVEnv_ptr env, char * value);
static char * command_alias_help(avl_tree* aliasTable, char * command);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Cmd_init_cmd(NuSMVEnv_ptr env)
{
  Cmd_CommandAdd(env, "alias", CommandAlias, 0, true);
  Cmd_CommandAdd(env, "echo", CommandEcho, 0, true);
  Cmd_CommandAdd(env, "help", CommandHelp, 0, true);
  Cmd_CommandAdd(env, "quit", CommandQuit, 0, true);
  Cmd_CommandAdd(env, "source", CommandSource, 0, true);
  Cmd_CommandAdd(env, "unalias", CommandUnalias, 0, true);
  Cmd_CommandAdd(env, "time", CommandTime, 0, true);
  Cmd_CommandAdd(env, "usage", CommandUsage, 0, true);
  Cmd_CommandAdd(env, "history", CommandHistory, 0, true);
  Cmd_CommandAdd(env, "which", CommandWhich, 0, true);
  Cmd_CommandAdd(env, "_memory_profile", CommandMemoryProfile, 0, true);
  Cmd_CommandAdd(env, "_show_help", CommandShowHelp, 0, true);

  /*Here initialize the command help hash table  */
  cmd_help_init(env);
}

void Cmd_quit_cmd(NuSMVEnv_ptr env)
{
  /*Here deinitialize the command help hash table  */
  cmd_help_quit(env);
}

int Cmd_command_not_available(NuSMVEnv_ptr env, int argc, char** argv)
{
  StreamMgr_ptr const streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  StreamMgr_print_error(streams, "Error: command %s is currently disabled\n",
                        argv[0]);

  return 1;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \command{time} Provides a simple CPU elapsed time value

  \command_args{[-h]}

  Prints the processor time used since the last invocation
  of the \"time\" command, and the total processor time used since NuSMV
  was started.
*/
static int CommandTime(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  static long last_time = 0;
  long time;
  int c;
  int start_time;

  nusmv_assert(NuSMVEnv_has_value(env, ENV_START_TIME));
  /* Remember the 1 offset */
  start_time = PTR_TO_INT(NuSMVEnv_get_value(env, ENV_START_TIME)) - 1;

  util_getopt_reset();
  while ((c = util_getopt(argc,argv,"h")) != EOF){
    switch(c){
      case 'h':
        goto usage;
      default:
        goto usage;
    }
  }

  if (argc != util_optind) {
    goto usage;
  }

  time = util_cpu_time();
  StreamMgr_print_output(streams,
           "elapse: %2.2f seconds, total: %2.2f seconds\n",
           (time - start_time - last_time) / 1000.0,
           (time - start_time) / 1000.0 );
  last_time = time;
  return 0;

usage:
  StreamMgr_print_error(streams,  "usage: time [-h]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  return 1;
}

/*!
  \command{echo} Merely echoes the arguments. File redirection is allowed.

  \command_args{[-h] [-2] [-n] [-o filename [-a]] &lt;args&gt;}

  Echoes its arguments to standard output.
  <p>
  Command options:<p>
  <dl>
    <dt> <tt>-2</tt>
     <dd> Redirects output to the standard error instead of the
     standard output. This cannot be used in combination with -o.
    <dt> <tt>-n</tt>
     <dd> Does not output the trailing newline.
    <dt> <tt>-o filename</tt>
     <dd> Echoes on the specified file instead of on the standard output.
    <dt> <tt>-a</tt>
     <dd> When used with option -o, appends the output to the specified file
          instead of overwriting it.
  </dl>
*/
static int CommandEcho(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  FILE* outstream = StreamMgr_get_output_stream(streams);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  int init_idx = 1;
  int i;
  int c;

  FILE* fout = outstream;
  char* fname = (char*) NULL;
  boolean must_append = false;
  boolean trailing_nl = true;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "ha2no:")) != EOF) {
    switch(c) {
    case 'h':
        goto usage;
        break;

    case 'o':
      if (fname != (char*) NULL) FREE(fname);
      fname = ALLOC(char, strlen(util_optarg)+1);
      nusmv_assert(fname != (char*) NULL);
      strcpy(fname, util_optarg);
      init_idx += 2;
      break;

    case 'a':
      must_append = true;
      init_idx += 1;
      break;

    case '2':
      fout = errstream;
      init_idx += 1;
      break;

    case 'n':
      trailing_nl = false;
      init_idx += 1;
      break;

    default:
      if (fname != (char*) NULL) FREE(fname);
      goto usage;
    }
  }

  if (fname != (char*) NULL) {
    /* the user asked to dump to a file */
    if (must_append) fout = fopen(fname, "a");
    else fout = fopen(fname, "w");

    if (fout == (FILE*) NULL) {
      /* counld not successfully open */
      StreamMgr_print_error(streams,  "echo: unable to open file %s for writing.\n",
              fname);
      FREE(fname);
      ErrorMgr_rpterr(errmgr, "echo: an error occured");
    }

    FREE(fname);
  }

  for (i = init_idx; i < argc; i++) { fprintf(fout, "%s ", argv[i]); }
  if (trailing_nl) fprintf(fout, "\n");

  if (fout != outstream && fout != errstream) fclose(fout);
  return 0;

  usage:
  StreamMgr_print_error(streams,  "usage: echo [-h] [-2] [-n] [[-o filename] [-a]] string \n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -2 \t\tRedirects to the standard error.\n");
  StreamMgr_print_error(streams,  "   -n \t\tDoes not output the trailing newline.\n");
  StreamMgr_print_error(streams,  "   -o filename \tRedirects the output to the specified file.\n");
  StreamMgr_print_error(streams,
          "   -a \t\tAppends the output to the end of the file specified \n"\
          "      \t\tby the option -o\n");
  return 1;
}

/*!
  \command{_memory_profile} It shows the amount of memory used by every package.

  \command_args{[-f &lt;filename&gt;] [-h] [-p] [-u &lt;units&gt;]}

  This command intregrates the output from purify with a
  function map generated by a perlscript plus another perlscript to generate a
  memory profile of NuSMV.<p>

  This command relies on the output of purify to a file to call the script
  "memoryaccount" and produces a summary of how much memory has been allocated
  by each package. Although this command may appear simple it requires the
  interaction of two scripts and three files, so special care should be taken
  when attempting to modify it.<p>

  Here is the way it works. The code in this command is conditionally compiled
  depending on the definition of the symbol <tt>PURIFY</tt>. If the symbol is
  not defined, the program prints a message notifying that the command is not
  operative in this executable. If <tt>PURIFY</tt> has been defined, there are
  certain things that are assumed. The executable has been linked with
  purify. The output of purify is being redirected to a file with name
  <tt>purify.log</tt>. The perl script <tt>memoryaccount</tt> is in
  <tt>$NUSMV_LIBRARY_PATH/common/share</tt> and it is
  executable. There exists a file whose name is <tt>.fmap</tt>, located
  in the same directory which script memoryaccount is located in. This file
  maps function names to packages which contain them.<p>

  The command then calls <tt>purify_all_inuse()</tt> to force purify to dump to
  the file <tt>purify.log</tt> all information about the memory that is
  currently visible to the program. This memory is not the total memory
  allocated by the program since there may be leaked memory that is no longer
  accessible. A temporary file is created and the script <tt>memoryaccount</tt>
  is called to analyze the file <tt>purify.log</tt> and write in the temporary
  file the memory profile obtained from it. Once the script is done, the
  temporary file is dumped to <tt>outstream</tt> and deleted.<p>

  Since most of the computation in this command is done by the pearlscript
  <tt>memoryaccount</tt>, for more information please refer to the message
  printed when the script is invoked with the option <tt>-h</tt>.

  Command options:<p>

  <dl>
     <dt> -f &lt;filename&gt;
        <dd> File to read the dump from. The default is
             purify.log. This option should be used if and only if the
             option <tt>-log-file</tt> has been used at the linking
             stage when building the executable.
     <dt> -p
          <dd> Prints also the packages that did not allocated any detectable
          memory
     <dt> -u &lt;units&gt;
         <dd> Units to print the memory usage in. It may be "b" for
               bytes, "k" for kilobytes, "m" for megabytes and "g" for
               gigabytes. The default is bytes.
  </dl>

*/
static int
CommandMemoryProfile(NuSMVEnv_ptr env, int  argc, char ** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  int   c;
  char  options[128];
#ifdef PURIFY
  char  tmpFileName[128];
  FILE  *fp;
  char  command[256];
  char  *NuSMVDirectoryName;
  int   systemStatus;
#endif
  /*
   * Parse command line options.
   */
  options[0] = 0;
  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "f:hpu:")) != EOF) {
    switch(c) {
      case 'f':
        strcat(options, " -f ");
        strcat(options, util_optarg);
        break;
      case 'h':
        goto usage;
        break;
      case 'p':
        strcat(options, " -p ");
        break;
      case 'u':
        strcat(options, " -u ");
        strcat(options, util_optarg);
        break;
      default:
        goto usage;
    }
  }


#if defined(PURIFY) && NUSMV_HAVE_SYSTEM
  /* Flag to remember that a file has been created by purify */
  NuSMVEnv_set_flag(env, ENV_PURIFY_FILE_CREATED, true);

  /* Obtain the name of a temporary file */
  tmpnam(tmpFileName);

  /* Kick purify to dump the data in the file */
  purify_all_inuse();

  /* Obtain the path to the perl script */
  NuSMVDirectoryName = CInit_NuSMVObtainLibrary();

  /* Prepare the string to be sent to a shell */
  c = snprintf(command, sizeof(command), "%s/memoryaccount %s %s/.fmap ./.fmap >%s",
               NuSMVDirectoryName, options, NuSMVDirectoryName,
               tmpFileName);
  SNPRINTF_CHECK(c, sizeof(command));

  /* Effectively execute the perlscript */
  systemStatus = system(command);
  if (systemStatus != 0) {
    return 1;
  }

  fp = Cmd_FileOpen(env, tmpFileName, "r", NIL(char *), 1);

  /* Check if the open has been successful */
  if (fp == NIL(FILE)) {
    StreamMgr_print_error(streams,  "File %s was not found\n", tmpFileName);
    return 1;
  }

  /* Dump the contents of the result file in outstream */
  while(fgets(command, 128, fp) != NIL(char)) {
    StreamMgr_print_output(streams,  "%s", command);
  }
  fclose(fp);

  /* Remove the temporary file */
#if NUSMV_HAVE_UNLINK
  unlink(tmpFileName);
#endif
#else
  StreamMgr_print_error(streams,  "Command not available: " \
          NUSMV_PACKAGE_NAME " has not been ");
  StreamMgr_print_error(streams,  "compiled with purify.\n");
#endif

  return 0;             /* normal exit */

  usage:
  StreamMgr_print_error(streams,  "usage: _memory_profile [-h] [-f <filename>]");
  StreamMgr_print_error(streams,  "[-p] [-u <units>] <filenames>\n");
  StreamMgr_print_error(streams,  "   -h\t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -f <file>\tFile to read the purify dump");
  StreamMgr_print_error(streams,  " from. The default is \"purify.log\".\n");
  StreamMgr_print_error(streams,  "   -p\t\tPrints also the packages that do not ");
  StreamMgr_print_error(streams,  "allocate any memory.\n");
  StreamMgr_print_error(streams,  "   -u <units>\tUnits to print the memory usage");
  StreamMgr_print_error(streams,  " in. It may be b for bytes\n");
  StreamMgr_print_error(streams,  "     \t\tk for kilobytes, m for megabytes and ");
  StreamMgr_print_error(streams,  "g for gigabytes.\n");
  return 1;             /* error exit */
}

/*!
  \command{quit} exits NuSMV

  \command_args{[-h] [-s] [-x]}

  Stops the program.  Does not save the current network
  before exiting.<p>

  Command options:<p>
  <dl>
     <dt> -s
     <dd> Frees all the used memory before quitting.
          This is slower, and it is used for finding memory leaks.
     <dt> -x
     <dd> Leave immediately. Skip all the cleanup code, leave it to
          the OS. This can save quite a long time.

  </dl>

*/
static int
CommandQuit(NuSMVEnv_ptr env, int  argc, char ** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  int c;

  util_getopt_reset();
  while ((c = util_getopt(argc,argv,"hsx")) != EOF){
    switch(c){
      case 'h':
        goto usage;
        break;
      case 's':
        return -2;
      case 'x':
        return -4;

      default:
        goto usage;
    }
  }

  if ( argc != util_optind){
    goto usage;
  }
  return -1;

  usage:
    StreamMgr_print_error(streams,  "usage: quit [-h] [-s] | [-x] \n");
    StreamMgr_print_error(streams,  "   -h  Prints the command usage.\n");
    StreamMgr_print_error(streams,  "   -s  Frees all the used memory before quitting.\n");
    StreamMgr_print_error(streams,  "   -x  Exits abruptly and silently.\n");
    return 1;
}

/*!
  \command{usage} Provides a dump of process statistics

  \command_args{[-h]}

  Prints a formatted dump of processor-specific usage
  statistics. For Berkeley Unix, this includes all of the information in the
  getrusage() structure.
*/
static int
CommandUsage(NuSMVEnv_ptr env, int  argc, char ** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  int c;

  util_getopt_reset();
  while ((c = util_getopt(argc,argv,"h")) != EOF){
    switch(c){
      case 'h':
        goto usage;
        break;
      default:
        goto usage;
    }
  }

  if (argc != util_optind){
    goto usage;
  }
  util_print_cpu_stats(outstream);
  return 0;

  usage:
    StreamMgr_print_error(streams,  "usage: usage [-h]\n");
    StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
    return 1;
}

/*!
  \command{which} Looks for a file called \"file_name\"

  \command_args{[-h] &lt;file_name&gt;}

  Looks for a file in a set of directories
  which includes the current directory as well as those in the NuSMV path.
  If it finds the specified file, it reports the found file's path.
  The searching path is specified through the "<tt>set open_path</tt>" command
  in \"<tt>.nusmvrc</tt>\".<p>

  Command options:<p>
  <dl>
     <dt> &lt;file_name&gt;
         <dd> File to be searched
  </dl>

  \sa set
*/
static int
CommandWhich(NuSMVEnv_ptr env, int  argc, char ** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE *fp;
  char *filename;
  int c;

  util_getopt_reset();
  while ((c = util_getopt(argc,argv,"h")) != EOF){
    switch(c){
      case 'h':
        goto usage;
        break;
      default:
        goto usage;
    }
  }

  if (argc-1 != util_optind){
    goto usage;
  }

  fp = Cmd_FileOpen(env, argv[1], "r", &filename, 0);
  if (fp != 0) {
    StreamMgr_print_output(streams,  "%s\n", filename);
    (void) fclose(fp);
  }
  FREE(filename);
  return 0;

  usage:
    StreamMgr_print_error(streams, "usage: which [-h] file_name\n");
    StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
    return 1;
}

/*!
  \command{history} list previous commands and their event numbers

  \command_args{[-h] [&lt;num&gt;]}

  Lists previous commands and their event numbers.
  This is a UNIX-like history mechanism inside the NuSMV shell.<p>
  Command options:<p>
  <dl>
     <dt> &lt;num&gt;
         <dd> Lists the last &lt;num&gt; events.  Lists the last 30
              events if &lt;num&gt; is not specified.
  </dl><p>

  History Substitution:<p>

  The history substitution mechanism is a simpler version of the csh history
  substitution mechanism.  It enables you to reuse words from previously typed
  commands.<p>

  The default history substitution character is the `%' (`!' is default for
  shell escapes, and `#' marks the beginning of a comment). This can be changed
  using the "set" command. In this description '%' is used as the history_char.
  The `%' can appear anywhere in a line.  A line containing a history
  substitution is echoed to the screen after the substitution takes place.
  `%' can be preceded by a `\\' in order to escape the substitution,
  for example, to enter a `%' into an alias or to set the prompt.<br><p>

  Each valid line typed at the prompt is saved.  If the "history" variable
  is set (see help page for "set"), each line is also echoed to the history
  file.  You can use the "history" command to list the previously typed
  commands. <p>

  Substitutions: <p>

  At any point in a line these history substitutions are
  available.<p>
        <dl><dt>%:0   <dd>  Initial word of last command.</dl>
        <dl><dt>%:n   <dd>   n-th argument of last command.</dl>
        <dl><dt>%$    <dd>   Last argument of last command.</dl>
        <dl><dt>%*    <dd>   All but initial word of last command.</dl>

        <dl><dt>%%    <dd>   Last command.</dl>
        <dl><dt>%stuf <dd>   Last command beginning with "stuf".</dl>
        <dl><dt>%n    <dd>   Repeat the n-th command.</dl>
        <dl><dt>%-n   <dd>   Repeat the n-th previous command.</dl>
        <dl><dt>^old^new  <dd>       Replace "old" with "new" in previous command.
        Trailing spaces are significant during substitution.
        Initial spaces are not significant.</dl>

  \sa set
*/
static int
CommandHistory(NuSMVEnv_ptr env, int  argc, char ** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  array_t* cmdHistoryArray = (array_t*)NuSMVEnv_get_value(env, ENV_CMD_COMMAND_HISTORY);
  int i, num, lineno;
  int size;
  int c;


  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "h")) != EOF) {
    switch(c) {
      case 'h':
        goto usage;
        break;
      default:
        goto usage;
    }
  }

  if (argc > 3) {
    goto usage;
  }
  num = 30;
  lineno = 1;
  for (i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      if (argv[i][1] == 'h') {
        lineno = 0;
      }
      else {
        goto usage;
      }
    }
    else {
      num = atoi(argv[i]);
      if (num <= 0) {
        goto usage;
      }
    }
  }
  size = array_n(cmdHistoryArray);
  num = (num < size) ? num : size;
  for (i = size - num; i < size; i++) {
    if (lineno != 0) {
      StreamMgr_print_output(streams,  "%d\t", i + 1);
    }
    StreamMgr_print_output(streams,  "%s\n", array_fetch(char *, cmdHistoryArray, i));
  }
  return(0);

usage:
  StreamMgr_print_error(streams,  "usage: history [-h] [num]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   num \t\tPrints the last num commands.\n");
  return(1);
}

/*!
  \command{alias} Provides an alias for a command

  \command_args{[-h] [&lt;name&gt; [&lt;string&gt;]]}

  The "alias" command, if given no arguments, will print
  the definition of all current aliases.  <p>

  Given a single argument, it will print the definition of that alias (if any).

  Given two arguments, the keyword <tt>&lt;name&gt;</tt> becomes an alias for
  the command string <tt>&lt;string&gt;</tt>, replacing any other alias with
  the same name.<p>

  Command options:
  <dl>
     <dt> &lt;name&gt;
        <dd> Alias
     <dt> &lt;string&gt;
        <dd> Command string
  </dl>

  It is possible to create aliases that take arguments by using the history
  substitution mechanism. To protect the history substitution
  character `<tt>%</tt>' from immediate expansion, it must be preceded
  by a `<tt>\\</tt>' when entering the alias.<p>

  For example:<p>
  <code>
   NuSMV> alias read "read_model -i \\%:1.smv ; set input_order_file \\%:1.ord"
   NuSMV> read short
  </code><p>
  will create an alias `read', execute "read_model -i short.smv;
    set input_order_file short.ord".<p>

  And again:<p>
  <code>
  NuSMV> alias echo2 "echo Hi ; echo \\%* !" <br>
  NuSMV> echo2 happy birthday
  </code><p>

  will print:<p>

  <code>
  Hi<br>
  happy birthday !
  </code><br>

  CAVEAT: Currently there is no check to see if there is a circular
  dependency in the alias definition. e.g.<p>

  <code>
  NuSMV> alias foo "echo print_bdd_stats; foo"
  </code><br>

  creates an alias which refers to itself. Executing the command <tt>foo</tt>
  will result an infinite loop during which the command
  <tt>print_bdd_stats</tt> will be executed.


  \sa unalias
*/
static int
CommandAlias(NuSMVEnv_ptr env, int  argc, char ** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  avl_tree* aliasTable = (avl_tree*)NuSMVEnv_get_value(env, ENV_CMD_ALIAS_TABLE);
  int i;
  char *key, *value;
  CmdAliasDescr_t *alias;
  avl_generator *gen;
  int status;
  int c;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "h")) != EOF) {
    switch(c) {
      case 'h':
        goto usage;
        break;
      default:
        goto usage;
    }
  }


  if (argc == 1) {
    avl_foreach_item(aliasTable, gen, AVL_FORWARD, &key, &value) {
      print_alias(env, value);
    }
    return 0;

  }
  else if (argc == 2) {
    if (avl_lookup(aliasTable, argv[1], &value)) {
      print_alias(env, value);
    }
    return 0;
  }

  /* delete any existing alias */
  key = argv[1];
  if (avl_delete(aliasTable, &key, &value)) {
    CmdAliasFree(value);
  }

  alias = ALLOC(CmdAliasDescr_t, 1);
  alias->name = util_strsav(argv[1]);
  alias->argc = argc - 2;
  alias->argv = ALLOC(char *, alias->argc);
  for(i = 2; i < argc; i++) {
    alias->argv[i-2] = util_strsav(argv[i]);
  }
  status = avl_insert(aliasTable, alias->name, (char *) alias);
  nusmv_assert(!status);  /* error here in SIS version, TRS, 8/4/95 */
  return 0;

  usage:
    StreamMgr_print_error(streams,  "usage: alias [-h] [command [string]]\n");
    StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
    return (1);
}

/*!
  \command{unalias} Removes the definition of an alias.

  \command_args{[-h] &lt;alias-names&gt;}

  Removes the definition of an alias specified via the
  alias command.<p>

  Command options:<p>
  <dl>
     <dt> &lt;alias-names&gt;
     <dd> Aliases to be removed
  </dl>

  \sa alias
*/
static int
CommandUnalias(NuSMVEnv_ptr env, int  argc, char ** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  avl_tree* aliasTable = (avl_tree*)NuSMVEnv_get_value(env, ENV_CMD_ALIAS_TABLE);
  int i;
  char *key, *value;
  int c;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "h")) != EOF) {
    switch(c) {
      case 'h':
        goto usage;
        break;
      default:
        goto usage;
    }
  }

  if (argc < 2) {
    goto usage;
  }

  for(i = 1; i < argc; i++) {
    key = argv[i];
    if (avl_delete(aliasTable, &key, &value)) {
      CmdAliasFree(value);
    }
  }
  return 0;

  usage:
    StreamMgr_print_error(streams,  "usage: unalias [-h] alias_names\n");
    StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
    return 1;
}

/*!
  \command{help} Provides on-line information on commands

  \command_args{[-h] [-a] [-p] [&lt;command&gt;]}

  If invoked with no arguments "help" prints the list of
  all commands known to the command interpreter.
  If a command name is given, detailed information for that command will be
  provided.<p>

  Command options:<p>
  <dl>
      <dt> -a
          <dd> Provides a list of all internal commands, whose names begin
          with the underscore character ('_') by convention.

      <dt> -p
          <dd> Disables the use of a pager like 'more' or any set
          in environment variable 'PAGER'. </dl>
*/
static int CommandHelp(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  avl_tree* commandTable = (avl_tree*)NuSMVEnv_get_value(env, ENV_CMD_COMMAND_TABLE);
  avl_tree* aliasTable = (avl_tree*)NuSMVEnv_get_value(env, ENV_CMD_ALIAS_TABLE);
  int c, i, all;
  char *key;
  avl_generator *gen;
  char *command;
  char *lib_name;
#if NUSMV_HAVE_GETENV
  char *pager;
#endif
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* old_outstream = NIL(FILE);
  OStream_ptr ostream;
  short int useMore = 1;

  util_getopt_reset();
  all = 0;
  while ((c = util_getopt(argc, argv, "hap")) != EOF) {
    switch(c) {
    case 'a':
      all = 1;
      break;
    case 'h':
      goto usage;
      break;
    case 'p':
      useMore = 0;
      break;

    default:
      goto usage;
    }
  }

  if (argc - util_optind == 0) {
    boolean nl_printed = false;
    i = 0;

    avl_foreach_item(commandTable, gen, AVL_FORWARD, &key, NIL(char *)) {
      if ((key[0] == '_') == all) {
        boolean ow = (strlen(key) >= 35);

        /* If command should be printed on the second column, but it
           is too width, newline */
        if ((i % 2) == 1 && ow) {
          StreamMgr_print_output(streams,  "\n");
        }

        StreamMgr_print_output(streams,  "%-35s", key);
        ++i;

        if ((i % 2) == 0 || ow) {
          StreamMgr_print_output(streams,  "\n");
          nl_printed = true;
        }
        else { nl_printed = false; }

        /* One long command takes 2 columns! If a newline has been
           printed because of a long command, next iteration we need
           to skip one newline printing! */
        if (ow && (i % 2) == 1) ++i;
      }
    }
    if (!nl_printed) {
      StreamMgr_print_output(streams,  "\n");
    }
  }
  else if (argc - util_optind == 1) {
    char* command_description;

#if NUSMV_HAVE_SYSTEM
    command = command_alias_help(aliasTable, argv[util_optind]);
    lib_name = CInit_NuSMVObtainLibrary();

    /*Here find the command help in the hash table, if not found return NULL */
    command_description = cmd_help_get(env, command);

    if (command_description == NULL) {
      StreamMgr_print_error(streams,  "Unfound help for command: %s.\n", command);
      return 1;
    }

    if (useMore) {
#if NUSMV_HAVE_POPEN
    old_outstream = outstream;
#if NUSMV_HAVE_GETENV
    pager = getenv("PAGER");
    if (pager == NULL) {
      outstream = popen("more", "w");
      if (outstream == NULL) {
        StreamMgr_print_error(streams,  "Unable to open pipe with \"more\".\n");
        StreamMgr_print_error(streams,
                              "Use option -p to disable the use of pager.\n");
        outstream = old_outstream;
        return 1;
      }
    }
    else {
      outstream = popen(pager, "w");
      if (outstream == NULL) {
        StreamMgr_print_error(streams,  "Unable to open pipe with \"%s\".\n", pager);
        StreamMgr_print_error(streams,
                              "Use option -p to disable the use of pager.\n");
        outstream = old_outstream;
        return 1;
      }
    }
#else /* NUSMV_HAVE_GETENV */
    outstream = popen("more", "w");
    if (outstream == NULL) {
      StreamMgr_print_error(streams,  "Unable to open pipe with \"more\".\n");
      StreamMgr_print_error(streams,
                            "Use option -p to disable the use of pager.\n");
      outstream = old_outstream;
      return 1;
    }
#endif /* NUSMV_HAVE_GETENV */

#else /* NUSMV_HAVE_POPEN */
    StreamMgr_print_error(streams,  "Pipe is not supported\n");
    StreamMgr_print_error(streams,
                          "Use option -p to disable the use of pager.\n");
    return 1;
#endif /* NUSMV_HAVE_POPEN */
    }

  ostream = OStream_create(outstream);
  OStream_printf(ostream, "%s\n", command_description);

#if NUSMV_HAVE_POPEN
  if (useMore) {
    pclose(outstream);
    outstream = old_outstream;
  }
#endif

  OStream_destroy_safe(ostream);

#else /* NUSMV_HAVE_SYSTEM */
    StreamMgr_print_error(streams,  "The manual is not available.\n");
#endif
  }
  else {
    goto usage;
  }

  return 0;

 usage:
  StreamMgr_print_error(streams,  "usage: help [-h] [-a] [-p] [command]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "   -a \t\tPrints help for all internal (hidden) commands.\n");
  StreamMgr_print_error(streams,  "   -p \t\tDisable the use of a pager like 'more'\n");
  return 1;
}

/*!
  \command{source} Executes a sequence of commands from a file

  \command_args{[-h] [-p] [-s] [-x] &lt;file&gt; [&lt;args&gt;]}

  Reads and executes commands from a file.<p>
  Command options:<p>
  <dl>
    <dt> -p
       <dd> Prints a prompt before reading each command.
    <dt> -s
       <dd> Silently ignores an attempt to execute commands from a nonexistent file.
    <dt> -x
       <dd> Echoes each command before it is executed.
    <dt> &lt;file&gt;
       <dd> File name
  </dl>

  Arguments on the command line after the filename are remembered but not
  evaluated.  Commands in the script file can then refer to these arguments
  using the history substitution mechanism.<p>

  EXAMPLE:<p>

  Contents of test.scr:<p>

  <br><code>
  read_model -i %:2<br>
  flatten_hierarchy<br>
  build_variables<br>
  build_model<br>
  </code><br>

  Typing "source test.scr short.smv" on the command line will execute the
  sequence<p>

  <br><code>
  read_model -i short.smv<br>
  flatten_hierarchy<br>
  build_variables<br>
  build_model<br>
  </code><br>

  (In this case <code>%:0</code> gets "source", <code>%:1</code> gets
  "test.scr", and <code>%:2</code> gets "short.smv".)
  If you type "alias st source test.scr" and then type "st short.smv bozo",
  you will execute<p>

  <br><code>
  read_model -i bozo<br>
  flatten_hierarchy<br>
  build_variables<br>
  build_model<br>
  </code><br>

  because "bozo" was the second argument on the last command line typed.  In
  other words, command substitution in a script file depends on how the script
  file was invoked. Switches passed to a command are also counted as
  positional parameters. Therefore, if you type "st -x short.smv bozo",
  you will execute

  <br><code>
  read_model -i short.smv<br>
  flatten_hierarchy<br>
  build_variables<br>
  build_model<br>
  </code><br>

  To pass the "-x" switch (or any other switch) to "source" when the
  script uses positional parameters, you may define an alias. For
  instance, "alias srcx source -x".<p>

  returns -3 if an error occurs and the flag 'on_failure_script_quits'
  is set.

  \sa history
*/
static int
CommandSource(NuSMVEnv_ptr env, int  argc, char ** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  array_t* cmdHistoryArray = (array_t*)NuSMVEnv_get_value(env, ENV_CMD_COMMAND_HISTORY);
  int c, echo, prompt, silent, interactive, quit_count, lp_count;
  int status = 0; /* initialize so that lint doesn't complain */
  int lp_file_index, did_subst;
  char *real_filename, line[MAX_STR], *command;
  FILE *fp;

  interactive = silent = prompt = echo = 0;
  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hipsx")) != EOF) {
    switch(c) {
        case 'h':
          goto usage ;
          break;
        case 'i':               /* a hack to distinguish EOF from stdin */
          interactive = 1;
          break;
        case 'p':
          prompt = 1;
          break;
        case 's':
          silent = 1;
          break;
        case 'x':
          echo = 1;
          break;
      default:
          goto usage;
    }
  }

  /* added to avoid core-dumping when no script file is specified */
  if (argc == util_optind){
    goto usage;
  }
  lp_file_index = util_optind;
  lp_count = 0;

  /*
   * FIX (Tom, 5/7/95):  I'm not sure what the purpose of this outer do loop
   * is. In particular, lp_file_index is never modified in the loop, so it
   * looks it would just read the same file over again.  Also, SIS had
   * lp_count initialized to -1, and hence, any file sourced by SIS (if -l or
   * -t options on "source" were used in SIS) would actually be executed
   * twice.
   */
  do {
    lp_count ++; /* increment the loop counter */

    fp = Cmd_FileOpen(env, argv[lp_file_index], "r", &real_filename, silent);
    if (fp == NULL) {
      FREE(real_filename);
      return ! silent;  /* error return if not silent */
    }

    quit_count = 0;
    do {
      char* prompt_string = (char*) NULL;

      if (prompt) {
        char* stmp = ALLOC(char, strlen(NuSMVCore_get_prompt_string())+1);
        nusmv_assert(stmp != (char*) NULL);

        stmp[0] = '\0';
        strcat(stmp, NuSMVCore_get_prompt_string());

        prompt_string = stmp;
      }

      /* clear errors -- e.g., EOF reached from stdin */
      clearerr(fp);
      /* read another command line */
      if (CmdFgetsFilec(env, line, MAX_STR, fp, prompt_string) == NULL) {

        if (prompt_string != (char*) NULL) FREE(prompt_string);

        if (interactive) {
          if (quit_count++ < 5) {
            StreamMgr_print_error(streams,  "\nUse \"quit\" to leave %s.\n",
                    get_pgm_name(opts));
            continue;
          }
          status = -1;          /* fake a 'quit' */
        }
        else {
          status = 0;           /* successful end of 'source' ; loop? */
        }
        break;
      }
      else if (prompt_string != (char*) NULL) FREE(prompt_string);
      quit_count = 0;

      if (echo) {
        StreamMgr_print_output(streams,  "%s", line);
      }
      command = CmdHistorySubstitution(env, line, &did_subst);
      if (command == NIL(char)) {
        status = 1;
        break;
      }
      if (did_subst) {
        if (interactive) {
          StreamMgr_print_output(streams,  "%s\n", command);
        }
      }
      if (command != line) {
        (void) strcpy(line, command);
      }
      if (interactive && *line != '\0') {
        array_insert_last(char *, cmdHistoryArray, util_strsav(line));
        if (nusmv_historyFile != NIL(FILE)) {
          fprintf(nusmv_historyFile, "%s\n", line);
          (void) fflush(nusmv_historyFile);
        }
      }

      status = Cmd_CommandExecute(env, line);

    } while (status == 0);

    if (fp != stdin) {
      if (status > 0) {
        StreamMgr_print_error(streams,  "aborting 'source %s'\n", real_filename);
      }
      (void) fclose(fp);
    }
    FREE(real_filename);

  } while ((status == 0) && (lp_count <= 0));
  /* An error occured during script execution */
  if (opt_on_failure_script_quits(opts) && (status > 0)) return -3;

  return status;

usage:
  StreamMgr_print_error(streams,  "source [-h] [-p] [-s] [-x] file_name [args]\n");
  StreamMgr_print_error(streams,  "\t-h Prints the command usage.\n");
  StreamMgr_print_error(streams,  "\t-p Supplies prompt before reading each line.\n");
  StreamMgr_print_error(streams,  "\t-s Silently ignores nonexistent file.\n");
  StreamMgr_print_error(streams,  "\t-x Echoes each line as it is executed.\n");
  return 1;
}

/*!
  \command{_show_help} Provides on-line information for all commands

  \command_args{[-f] [-h] [&lt;command&gt;]}

  If invoked with no arguments prints the short help
  for all commands known to the command interpreter including
  hidden commands (those whose name starts with _).<p>

  Command options:<p>
  <dl>
      <dt> -f </dt>
          <dd> Prints for each command the long help.</dd>

  </dl>
*/
static int CommandShowHelp(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  avl_tree* commandTable = (avl_tree*)NuSMVEnv_get_value(env, ENV_CMD_COMMAND_TABLE);
  int c, longmaual;
  char *key, *lib_name;
  char ch;
  avl_generator *gen;
  char command[1024];
  char fname[1024];
  FILE* fileid;

  util_getopt_reset();
  longmaual = 0;
  while ((c = util_getopt(argc, argv, "f")) != EOF) {
    switch(c) {
    case 'f':
      longmaual = 1;
      break;
    case 'h':
      goto usage;
      break;
    default:
      goto usage;
    }
  }

  lib_name = CInit_NuSMVObtainLibrary();

  avl_foreach_item(commandTable, gen, AVL_FORWARD, &key, NIL(char *)) {
    StreamMgr_print_error(streams,
            "==============================================================================\n");
    if (1 == longmaual) {
      int c = snprintf(fname, 1023, "%s/help/%sCmd.txt", lib_name, key);
      SNPRINTF_CHECK(c, 1023);

      fileid = fopen(fname, "r");
      if ((FILE *)NULL == fileid) {
        StreamMgr_print_error(streams,  "The manual for the command '%s'"\
                "is not available.\n", key);
      } else {
        while(!feof(fileid)) {
          ch = fgetc(fileid);
          if (EOF != ch) fputc(ch, errstream);
        }
        fclose(fileid);
      }
    }
    else {
      int c = 0;
      StreamMgr_print_error(streams,  "COMMAND = %s\n", key);
      c = snprintf(command, 1023, "%s -h", key);
      SNPRINTF_CHECK(c, 1023);
      Cmd_CommandExecute(env, command);
    }
    StreamMgr_print_error(streams,
            "==============================================================================\n");
  }
  FREE(lib_name);

  return 0;

  usage:
    StreamMgr_print_error(streams,  "usage: _show_help [-f] [-h]\n");
    StreamMgr_print_error(streams,  "   -f \t\tPrints the long help for all commands.\n");
    StreamMgr_print_error(streams,  "      \t\tBy default the short help is printed.\n");
    return 1;
}

/*!
  \brief required

  optional

  \se required

  \sa optional
*/
static void
print_alias(const NuSMVEnv_ptr env, char * value)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  int i;
  CmdAliasDescr_t *alias;

  alias = (CmdAliasDescr_t *) value;
  StreamMgr_print_output(streams,  "%s\t", alias->name);
  for(i = 0; i < alias->argc; i++) {
    StreamMgr_print_output(streams,  " %s", alias->argv[i]);
  }
  StreamMgr_print_output(streams,  "\n");
}

/*!
  \brief required

  optional

  \se required

  \sa optional
*/
static char * command_alias_help(avl_tree* aliasTable, char * command)
{
  char *value;
  CmdAliasDescr_t *alias;

  if (!avl_lookup(aliasTable, command, &value)) {
    return command;
  }
  alias = (CmdAliasDescr_t *) value;
  return alias->argv[0];
}
