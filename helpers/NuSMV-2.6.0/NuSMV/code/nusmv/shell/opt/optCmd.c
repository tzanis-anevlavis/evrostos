/* ---------------------------------------------------------------------------


   This file is part of the ``opt'' package of NuSMV version 2.
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
  \brief The option command file.

  optional

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/shell/cmd/cmd.h"
#include "nusmv/shell/opt/optCmd.h"

#include "nusmv/core/opt/opt.h"
#include "nusmv/core/opt/OptsHandler.h"
#include "nusmv/core/cinit/cinit.h"
#include "nusmv/core/cinit/cinit.h"
#include "nusmv/core/compile/compile.h"

static int CommandSetVariable(NuSMVEnv_ptr env, int argc, char** argv);
static int CommandUnsetVariable(NuSMVEnv_ptr env, int argc, char** argv);

/* Adds a command which generates tests for the options handler */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define TEST_OPTS_HANDLER 0
#if TEST_OPTS_HANDLER
static int CommandGenTestOptsHandler(NuSMVEnv_ptr env, int argc, char** argv);
#endif

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
extern cmp_struct_ptr cmps;

/*---------------------------------------------------------------------------*/
/* Macros                                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static boolean opt_input_file_trigger(OptsHandler_ptr opts,
                                      const char* name,
                                      const char* value,
                                      Trigger_Action action,
                                      void* arg);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Opt_Cmd_init(NuSMVEnv_ptr env)
{

  Cmd_CommandAdd(env, "set", CommandSetVariable, 0, true);
  Cmd_CommandAdd(env, "unset", CommandUnsetVariable, 0, true);

#if TEST_OPTS_HANDLER
  Cmd_CommandAdd(env, "_gen_test_opts_handler", CommandGenTestOptsHandler, 0, true);
#endif
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \command{set} Sets an environment variable

  \command_args{[-h] [&lt;name&gt;] [&lt;value&gt;]}

   A variable environment is maintained by the command
   interpreter.
   The "set" command sets a variable to a particular value, and the
   "unset" command removes the definition of a variable.
   If "set" is given no arguments, it prints the current value of all variables.<p>

   Command options:<p>
   <dl> <dt> -h
   <dd> Prints the command usage.
   </dl>
   <dl> <dt> &lt;name&gt;
   <dd> Variable name
   </dl>
   <dl> <dt> &lt;value&gt;
   <dd> Value to be assigned to the variable.
   </dl>

   <p>
   Interpolation of variables is allowed when using the set command. The
   variables are referred to with the prefix of '$'. So for example,
   what follows can be done to check the value of a set variable:<br>
   <code>
   NuSMV> set foo bar<br>
   NuSMV> echo $foo<br>
   bar <br>
   </code>

   The last line "bar" will be the output produced by NuSMV.<p>

   Variables can be extended by using the character ':' to concatenate
   values. For example: <br>
   <code>
   NuSMV> set foo bar<br>
   NuSMV> set foo $foo:foobar<br>
   NuSMV> echo $foo<br>
   bar:foobar<br>
   </code>
   The variable <code> foo </code> is extended with the value <code>
   foobar </code>. <p>

   Whitespace characters may be present within quotes. However, variable
   interpolation lays the restriction that the characters ':' and '/' may
   not be used within quotes. This is to allow for recursive interpolation.
   So for example, the following is allowed<br>
   <code>
   NuSMV> set "foo bar" this<br>
   NuSMV> echo $"foo bar"<br>
   this <br>
   </code>
   The last line will be the output produced by NuSMV. <br>
   But in the following, the  value of the variable <code> foo/bar </code>
   will not be interpreted correctly:<p>
   <code>
   NuSMV> set "foo/bar" this<br>
   NuSMV> echo $"foo/bar"<br>
   foo/bar<br>
   </code>
   If a variable is not set by the "set" command, then the variable is returned
   unchanged.
   <p>

   Different commands use environment information for different purposes.
   The command interpreter makes use of the following parameters:<p>

   <dl>
   <dt><b>autoexec</b>
   <dd>     Defines a command string to be automatically executed after every
   command processed by the command interpreter.
   This is useful for things like timing commands, or tracing the
   progress of optimization.
   </dl>


   <dl><dt><b>open_path</b>
   <dd>      "open_path" (in analogy to the shell-variable PATH) is a list of
   colon-separated strings giving directories to be searched whenever
   a file is opened for read.  Typically the current directory (.) is
   the first item in this list. The standard system library (typically
   $NUSMV_LIBRARY_PATH) is always implicitly appended to the current
   path.
   This provides a convenient short-hand mechanism for reaching
   standard library files.
   </dl>
   <dl><dt> <b>errstream </b>
   <dd>   Standard error (normally stderr) can be re-directed to a file
   by setting the variable errstream.
   </dl>

   <dl><dt>  <b>outstream</b>
   <dd>           Standard output (normally stdout) can be re-directed to a file
   by setting the variable outstream.
   </dl>


  \sa unset
*/
static int CommandSetVariable(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* errstream = StreamMgr_get_error_stream(streams);

  char *flag_value, *key;
  int c;
  boolean has_param = false;

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
  if (argc == 0 || argc > 3) {
    goto usage ;
  }
  else if (argc == 1) {
    OptsHandler_print_all_options(opts,
                                  outstream, false);
    return 0;
  }
  else {
    key = util_strsav(argv[1]);

    flag_value = argc == 2 ? util_strsav("") : util_strsav(argv[2]);
    has_param = (argc == 3);

    if (strcmp(argv[1], "nusmv_stdout") == 0) {
      if (outstream != stdout) {
        (void) fclose(outstream);
      }
      if (strcmp(flag_value, "") == 0) {
        FREE(flag_value);
        flag_value = util_strsav("-");
      }
      outstream = Cmd_FileOpen(env, flag_value, "w", NIL(char*), 0);
      if (outstream == NULL) {
        outstream = stdout;
      }
#if NUSMV_HAVE_SETVBUF
# if defined SETVBUF_REVERSED && SETVBUF_REVERSED
      setvbuf(outstream, _IOLBF, (char*) NULL, 0);
# else
      setvbuf(outstream, (char*) NULL, _IOLBF, 0);
# endif
#endif
    }
    if (strcmp(argv[1], "nusmv_stderr") == 0) {
      if (errstream != stderr) {
        (void) fclose(errstream);
      }
      if (strcmp(flag_value, "") == 0) {
        FREE(flag_value);
        flag_value = util_strsav("-");
      }
      errstream = Cmd_FileOpen(env, flag_value, "w", NIL(char*), 0);
      if (errstream == NULL) {
        errstream = stderr;
      }
#if NUSMV_HAVE_SETVBUF
# if defined SETVBUF_REVERSED && SETVBUF_REVERSED
      setvbuf(errstream, _IOLBF, (char*) NULL, 0);
# else
      setvbuf(errstream, (char*) NULL, _IOLBF, 0);
# endif
#endif
    }
    if (strcmp(argv[1], "history") == 0) {
      if (nusmv_historyFile != NIL(FILE)) {
        (void) fclose(nusmv_historyFile);
      }
      if (strcmp(flag_value, "") == 0) {
        nusmv_historyFile = NIL(FILE);
      }
      else {
        nusmv_historyFile = Cmd_FileOpen(env, flag_value, "w", NIL(char*), 0);
        if (nusmv_historyFile == NULL) {
          nusmv_historyFile = NIL(FILE);
        }
      }
    }

    /* TODO[AMa] Check this. */
    /* Add triggers that should only be enabled when using the set
       function (ie: internal uses of the option may do things that
       the user CANNOT!) */
    OptsHandler_add_option_trigger(opts, INPUT_FILE,
                                   opt_input_file_trigger, env);

    if (OptsHandler_is_option_registered(opts, key)) {
      if (OptsHandler_is_option_public(opts, key)) {

        /* Handle boolean options. These do not need explicitly a flag value. */
        if (!has_param) {
          if (OptsHandler_is_bool_option(opts, key)) {
            OptsHandler_set_bool_option_value(opts,
                                              key, true);
          }
          else {
            StreamMgr_print_error(streams,
                    "Please provide a value for option \"%s\"\n", key);
          }
        }
        /* A value has been provided */
        else {
          boolean res = OptsHandler_set_option_value(opts,
                                                     key, flag_value);
          if (!res) {
            /* If possible values are known, print them */
            if (OptsHandler_is_enum_option(opts, key) ||
                OptsHandler_is_bool_option(opts, key)) {
              int i, num;
              char** values;

              OptsHandler_get_enum_option_values(opts,
                                                 key, &values, &num);

              StreamMgr_print_error(streams,  "Possible values are: \"");
              for (i = 0; i < num; ++i) {
                StreamMgr_print_error(streams,  "%s%s", values[i],
                        (i == (num - 1) ? "" : " "));
                FREE(values[i]);
              }
              StreamMgr_print_error(streams,  "\"\n");
              FREE(values);
            } /* Enum possibilities printing */
            else if (OptsHandler_is_int_option(opts, key)) {
              StreamMgr_print_error(streams,  "The option requires an integer argument\n");
            }

            StreamMgr_print_error(streams,
                    "Cannot assign value \"%s\" to option \"%s\"\n",
                    flag_value, key);

            FREE(flag_value);
            FREE(key);
            return 1;
          } /* Error in setting the new option value */
        } /* Option value has been provided */
      }
      /* Private option, print an error message */
      else {
        StreamMgr_print_error(streams,
                "Option \"%s\" is private. Cannot set value for \"%s\"\n",
                key, key);
        FREE(flag_value);
        FREE(key);
        return 1;
      }
    }
    /* Unregistered options are just added as key -> value */
    else {
      boolean res;

      StreamMgr_print_output(streams,  "Defining new environment variable \"%s\"\n", key);

      /* Promote the new variable to boolean if no parameter is given.. */
      if (!has_param) {
        FREE(flag_value);
        flag_value = util_strsav("1");
      }

      res = OptsHandler_register_user_option(opts, key, flag_value);
      if (!res) {
        StreamMgr_print_error(streams,
                "Some error occurred while registering option \"%s\"\n", key);
        FREE(flag_value);
        FREE(key);
        return 1;
      }
    }

    FREE(flag_value);
    FREE(key);
  }

  OptsHandler_remove_option_trigger(opts, INPUT_FILE,
                                    opt_input_file_trigger);


  return 0;

 usage:
  (void) StreamMgr_print_output(streams, "usage: set [-h] [name [value]]\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  return 1;
}

/*!
  \command{unset} Unsets an environment variable

  \command_args{[-h] &lt;variables&gt;}

  A variable environment is maintained by the command
   interpreter.
   The "set" command sets a variable to a particular value, and the
   "unset" command removes the definition of a variable. <p>
   Command options:<p>
   <dl><dt> -h
   <dd> Prints the command usage.
   </dl>
   <dl><dt> &lt;variables&gt;
   <dd> Variables to be unset
   </dl>


  \sa set
*/
static int CommandUnsetVariable(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int i;
  char *key;
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

  for (i = 1; i < argc; ++i) {
    key = util_strsav(argv[i]);
    if (OptsHandler_is_option_registered(opts, key)) {
      /* Reset the option value. If the variable is user defined, just
         remove it! */
      if (OptsHandler_is_user_option(opts, key)) {
        OptsHandler_unregister_option(opts, key);
      }
      else {
        /* Unset of boolean options means set it to 0 */
        if (OptsHandler_is_bool_option(opts, key)) {
          OptsHandler_set_bool_option_value(opts, key, false);
        }
        /* Unset of other options means reset it to default */
        else {
          OptsHandler_reset_option_value(opts, key);
        }
      }
    }
    else {
      StreamMgr_print_error(streams,  "Warning: Option \"%s\" is not registered\n", key);
    }
    FREE(key);
  }

  return 0;


 usage:
  StreamMgr_print_error(streams,  "usage: unset [-h] variables \n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage.\n");
  return 1;
}

/*!
  \brief


*/

#if TEST_OPTS_HANDLER

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int CommandGenTestOptsHandler(NuSMVEnv_ptr env, int argc, char** argv)
{
  char* filename = (char*)NULL;
  int c;
  int ret = 0;
  boolean unset = false;
  FILE* out = outstream;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "huo:")) != EOF) {
    switch(c) {
    case 'h':
      goto test_opt_handler_usage;
      break;
    case 'u':
      if (unset) { goto test_opt_handler_usage; }
      unset = true;
      break;
    case 'o':
      if ((char*)NULL != filename) { goto test_opt_handler_usage; }
      filename = util_strsav(util_optarg);
      break;
    default:
      goto test_opt_handler_usage;
    }
  }

  if ((char*)NULL != filename) {
    out = fopen(filename, "w");
    if ((FILE*)NULL == out) {
      StreamMgr_print_error(streams,  "Cannot open %s for writing\n", filename);
      ret = 1;
      goto test_opt_handler_free;
    }
  }

  OptsHandler_generate_test(opts, out, unset);

  fflush(out);
  if ((char*)NULL != filename) {
    fclose(out);
  }

  goto test_opt_handler_free;

 test_opt_handler_usage:
  ret = 1;
  StreamMgr_print_error(streams,  "usage: _gen_test_opts_handler: [-h] [-u] [-o filename]\n");
  StreamMgr_print_error(streams,  "       -h      : Show help\n");
  StreamMgr_print_error(streams,  "       -u      : Generate test of unset command\n");
  StreamMgr_print_error(streams,  "       -o file : Save output to file\n");

 test_opt_handler_free:
  if ((char*)NULL != filename) {
    FREE(filename);
  }

  return ret;
}
#endif

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Input file check function

  Input file check function
*/
static boolean opt_input_file_trigger(OptsHandler_ptr opts, const char* name,
                               const char* val, Trigger_Action action,
                               void* arg)
{
  const NuSMVEnv_ptr env = NUSMV_ENV(arg);
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  if (ACTION_SET == action && cmp_struct_get_read_model(cmps))  {
    StreamMgr_print_output(streams,
                           "***** Warning: a model is already loaded -- input_file not changed.\n");
    StreamMgr_print_output(streams,
                           "***** The model should be reset (e.g., using command \"reset\")\n");
    StreamMgr_print_output(streams,
                           "***** before the input_file can be changed.\n");
    return false;
  }

  return true;
}
