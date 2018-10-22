/* ---------------------------------------------------------------------------


  This file is part of the ``prop'' package of NuSMV version 2.
  Copyright (C) 2000-2001 by FBK-irst.

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
  \brief Shell interface for the prop package.

  This file contains the interface of the prop package
  with the interactive shell.

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/shell/cmd/cmd.h"
#include "nusmv/shell/prop/propCmd.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/prop/Prop.h"
#include "nusmv/core/prop/PropDb.h"
#include "nusmv/core/prop/propPkg.h"

#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"

#include "nusmv/core/parser/parser.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/mc/mc.h"
#include "nusmv/core/prop/propProp.h"

#include "nusmv/core/utils/error.h" /* for CATCH(errmgr) */

/* prototypes of the command functions */
int CommandShowProperty(NuSMVEnv_ptr env, int argc, char** argv);
int CommandAddProperty(NuSMVEnv_ptr env, int argc, char** argv);
int CommandCheckProperty(NuSMVEnv_ptr env, int argc, char** argv);
int CommandConvertPropertyToInvar(NuSMVEnv_ptr env, int argc, char** argv);

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define COMMAND_CONVERT_PROPERTY_TO_INVAR_NAME "convert_property_to_invar"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
extern cmp_struct_ptr cmps;


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static int UsageShowProperty(const NuSMVEnv_ptr env);
static int UsageAddProperty(const NuSMVEnv_ptr env);
static int UsageCheckProperty(const NuSMVEnv_ptr env);
static int UsageConvertPropertyToInvar(const NuSMVEnv_ptr env);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void PropPkg_init_cmd(NuSMVEnv_ptr env)
{
  Cmd_CommandAdd(env, "show_property",  CommandShowProperty, 0, true);
  Cmd_CommandAdd(env, "add_property",   CommandAddProperty, 0, true);
  Cmd_CommandAdd(env, "check_property", CommandCheckProperty, 0, true);
  Cmd_CommandAdd(env, COMMAND_CONVERT_PROPERTY_TO_INVAR_NAME,
                 CommandConvertPropertyToInvar, 0, true);
}

void PropPkg_quit_cmd(NuSMVEnv_ptr env)
{
  boolean status = true;

  status = status && Cmd_CommandRemove(env, "show_property");
  status = status && Cmd_CommandRemove(env, "add_property");
  status = status && Cmd_CommandRemove(env, "check_property");
  status = status && Cmd_CommandRemove(env, COMMAND_CONVERT_PROPERTY_TO_INVAR_NAME);

  nusmv_assert(status);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \command{show_property} Shows the currently stored properties

  \command_args{[-h] [[-c | -l | -i | -q] [-u | -t | -f]]
  | [-n property_number] | [-P property_name] | [-s]
  [-m | -o output-file] [-F format]}


  Shows the properties currently stored in the list of properties. This list
  is initialized with the properties (CTL, LTL, INVAR, COMPUTE) present
  in the input file, if any; then all of the properties added by the user
  with the relative <tt>check</tt> or <tt>add_property<\tt> commands are appended
  to this list.
  For every property, the following informations are displayed:
  <ul>
  <li>the identifier of the property (a progressive number);
  <li>the property formula;
  <li>the type (CTL, LTL, INVAR, COMPUTE)
  <il>the status of the formula (Unchecked, True, False) or the result of the
  quantitative expression, if any (it can be infinite);
  <li>if the formula has been found to be false, the number of the
  corresponding counterexample trace.
  </ul>
  By default, all the properties currently stored in the list of properties
  are shown. Specifying the suitable options, properties with a certain
  status (Unchecked, True, False) and/or of a certain type (e.g. CTL,
  LTL), or with a given identifier, it is possible to let the system show a
  restricted set of properties. It is allowed to insert only one option
  per status and one option per type.
  <p>
  Command options:<p>
  <dl>
    <dt> <tt>-c</tt>
       <dd> Prints only CTL properties.
    <dt> <tt>-l</tt>
       <dd> Prints only LTL properties.
    <dt> <tt>-i</tt>
       <dd> Prints only INVAR properties.
    <dt> <tt>-q</tt>
       <dd> Prints only quantitative (COMPUTE) properties.
    <dt> <tt>-u</tt>
       <dd> Prints only unchecked properties.
    <dt> <tt>-t</tt>
       <dd> Prints only those properties found to be true.
    <dt> <tt>-f</tt>
       <dd> Prints only those properties found to be false.
    <dt> <tt>-n property-number</tt>
       <dd> Prints out the property numbered <tt>property-number</tt>.
    <dt> <tt>-P property-name</tt>
       <dd> Prints out the property named <tt>property-name</tt>.
    <dt> <tt>-m</tt>
       <dd> Pipes the output through the program specified by the
       <tt>PAGER</tt> shell variable if defined, else through the
       <tt>UNIX</tt> "more" command.
    <dt> <tt>-o output-file</tt>
       <dd> Writes the output generated by the command to <tt>output-file<\tt>.
    <dt> <tt>-F format</tt>
       <dd> print with given format. Use -F help to see available formats.
    <dt> <tt>-s Prints the number of stored properties. </tt>
       <dd> <\tt>.
  </dl>

  \sa add_property check_ctlspec check_ltlspec check_invar compute
*/

int CommandShowProperty(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* errstream = StreamMgr_get_error_stream(streams);
  int c;
  int retval = 0;
  int prop_no = -1;
  int useMore = 0;
  boolean print_props_num = false;
  Prop_Type type = Prop_NoType;
  Prop_Status status = Prop_NoStatus;
  char* outFileName = NIL(char);
  FILE* old_outstream = NULL;
  PropDb_PrintFmt fmt = PROPDB_PRINT_FMT_DEFAULT;
  PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));

  util_getopt_reset();
  while((c = util_getopt(argc, argv, "hlciqutfn:vmo:F:sP:")) != EOF){
    switch(c){
    case 'h':
      {
        if (outFileName != NIL(char)) FREE(outFileName);
        return(UsageShowProperty(env));
      }
    case 'c':
      {
        if ((type != Prop_NoType) || (prop_no != -1)) {
          if (outFileName != NIL(char)) FREE(outFileName);
          return(UsageShowProperty(env));
        }
        type = Prop_Ctl;
        break;
      }
    case 'l':
      {
        if ((type != Prop_NoType) || (prop_no != -1)) {
          if (outFileName != NIL(char)) FREE(outFileName);
          return(UsageShowProperty(env));
        }
        type = Prop_Ltl;
        break;
      }
    case 'i':
      {
        if ((type != Prop_NoType) || (prop_no != -1)) {
          if (outFileName != NIL(char)) FREE(outFileName);
          return(UsageShowProperty(env));
        }
        type = Prop_Invar;
        break;
      }
    case 'q':
      {
        if ((type != Prop_NoType) || (prop_no != -1)) {
          if (outFileName != NIL(char)) FREE(outFileName);
          return(UsageShowProperty(env));
        }
        type = Prop_Compute;
        break;
      }
    case 'u':
      {
        if ((status != Prop_NoStatus) || (prop_no != -1)) {
          if (outFileName != NIL(char)) FREE(outFileName);
          return(UsageShowProperty(env));
        }
        status = Prop_Unchecked;
        break;
      }
    case 't':
      {
        if ((status != Prop_NoStatus) || (prop_no != -1)) {
          if (outFileName != NIL(char)) FREE(outFileName);
          return(UsageShowProperty(env));
        }
        status = Prop_True;
        break;
      }
    case 'f':
      {
        if ((status != Prop_NoStatus) || (prop_no != -1)) {
          if (outFileName != NIL(char)) FREE(outFileName);
          return(UsageShowProperty(env));
        }
        status = Prop_False;
        break;
      }
    case 'n':
      {
        if ((type != Prop_NoType)     ||
            (status != Prop_NoStatus) ||
            (prop_no != -1)) {
          if (outFileName != NIL(char)) FREE(outFileName);
          return(UsageShowProperty(env));
        }

        prop_no = PropDb_get_prop_index_from_string(prop_db, util_optarg);
        if (prop_no == -1) {
          if (outFileName != NIL(char)) FREE(outFileName);
          return(1);
        }

        break;
      }
    case 'P':
      {
        char* formula_name;
        if ((type != Prop_NoType)     ||
            (status != Prop_NoStatus) ||
            (prop_no != -1)) {
          if (outFileName != NIL(char)) FREE(outFileName);
          return(UsageShowProperty(env));
        }

        formula_name = util_strsav(util_optarg);

        prop_no = PropDb_prop_parse_name(prop_db, formula_name);
        if (prop_no == -1) {
          StreamMgr_print_error(streams,  "No property named '%s'\n", formula_name);
          if (outFileName != NIL(char)) FREE(outFileName);
          FREE(formula_name);
          return(1);
        }
        FREE(formula_name);

        break;
      }
    case 'o':
      {
        if (useMore == 1 || outFileName != NIL(char)) {
          if (outFileName != NIL(char)) FREE(outFileName);
          return(UsageShowProperty(env));
        }
        outFileName = util_strsav(util_optarg);
        break;
      }

    case 'F':
      {
        static struct { char* name; PropDb_PrintFmt fmt; } formats[] = {
          {"tabular", PROPDB_PRINT_FMT_TABULAR},
          {"xml", PROPDB_PRINT_FMT_XML},
        };

        int i;
        if (strcmp(util_optarg, "help") == 0) {
          StreamMgr_print_error(streams,  "Available formats are: ");
          for (i=sizeof(formats)/sizeof(formats[0])-1; i>=0; --i) {
            fputs(formats[i].name, errstream);
            if (i>0) StreamMgr_print_error(streams,  ", ");
          }
          StreamMgr_print_error(streams,  "\n");
          if (outFileName != NIL(char)) FREE(outFileName);
          return 0;
        }

        {
          boolean found = false;
          for (i=0; i<sizeof(formats)/sizeof(formats[0]); ++i) {
            if (strcmp(util_optarg, formats[i].name) == 0) {
              fmt = formats[i].fmt;
              found = true;
              break;
            }
          }
          if (!found) {
            StreamMgr_print_error(streams,  "Format '%s' is not valid, use '-F help'"
                    "to see the valid formats.\n", util_optarg);
            if (outFileName != NIL(char)) FREE(outFileName);
            return 1;
          }
        break;
        }
      }

    case 'm':
      {
        if (outFileName != NIL(char)) {
          FREE(outFileName);
          return(UsageShowProperty(env));
        }
        useMore = 1;
        break;
      }
    case 's': print_props_num = true; break;

    default:
      {
        if (outFileName != NIL(char)) FREE(outFileName);
        return(UsageShowProperty(env));
      }
    }
  }

  if (argc != util_optind) {
    if (outFileName != NIL(char)) FREE(outFileName);
    return(UsageShowProperty(env));
  }

  /* command hierarchy control */
  if (Compile_check_if_flattening_was_built(env, errstream)) {
    if (outFileName != NIL(char)) FREE(outFileName);
    return 1;
  }

  if (useMore == 1) {
    nusmv_assert(outFileName == NIL(char));
    old_outstream = outstream;
    outstream = CmdOpenPipe(env, useMore);
    if (outstream == NIL(FILE)) {
      outstream = old_outstream;
      return(1);
    }
  }
  if (outFileName != NIL(char)) {
    old_outstream = outstream;
    outstream = CmdOpenFile(env, outFileName);
    if (outstream == NIL(FILE)) {
      outstream = old_outstream;
      FREE(outFileName);
      return(1);
    }
  }

  retval = PropDb_show_property(prop_db, print_props_num, fmt, type, status,
                                prop_no, outstream);

  if (1 == useMore) {
    CmdClosePipe(outstream);
    outstream = old_outstream;
  }
  if (outFileName != NIL(char)) {
    CmdCloseFile(outstream);
    outstream = old_outstream;
    FREE(outFileName);
  }

  return retval;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageShowProperty(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: show_property [-h] [ [[-c | -l | -i | -q] [-u | -t | -f]] | [-n index] | [-P name] ] \n");
  StreamMgr_print_error(streams,  "\t\t [-m | -o file] [-F format]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -c \t\tPrints only CTL properties.\n");
  StreamMgr_print_error(streams,  "  -l \t\tPrints only LTL properties.\n");
  StreamMgr_print_error(streams,  "  -i \t\tPrints only INVAR properties.\n");
  StreamMgr_print_error(streams,  "  -q \t\tPrints only quantitative properties (COMPUTE).\n");
  StreamMgr_print_error(streams,  "  -u \t\tPrints only unchecked properties.\n");
  StreamMgr_print_error(streams,  "  -t \t\tPrints only those properties found to be true.\n");
  StreamMgr_print_error(streams,  "  -f \t\tPrints only those properties found to be false.\n");
  StreamMgr_print_error(streams,  "  -n index\tPrints out the property numbered \"index\".\n");
  StreamMgr_print_error(streams,  "  -P name\tPrints out the property named \"name\".\n");
  StreamMgr_print_error(streams,  "  -m \t\tPipes output through the program specified by the \"PAGER\"\n");
  StreamMgr_print_error(streams,  "    \t\tenvironment variable if defined, else through UNIX \"more\".\n");
  StreamMgr_print_error(streams,  "  -o file\tWrites the generated output to \"file\".\n");
  StreamMgr_print_error(streams,  "  -F format\tPrints in given format. Use '-F help' to see available formats.\n");
  StreamMgr_print_error(streams,  "  -s \t\tPrints the number of stored properties.\n");
  return(1);
}

/*!
  \command{add_property} Adds a property to the list of properties

  \command_args{[-h] [(-c | -l | -i | -q | -s) -p "formula [IN context]"] [-n "name"]}


  Adds a property in the list of properties. It is possible to insert
  LTL, CTL, INVAR, PSL and quantitative (COMPUTE) properties. Every
  newly inserted property is initialized to unchecked. A type option
  must be given to properly execute the command.
  <p>
  Command options:<p>
  <dl>
    <dt> <tt>-c</tt>
       <dd> Adds a CTL property.
    <dt> <tt>-l</tt>
       <dd> Adds an LTL property.
    <dt> <tt>-i</tt>
       <dd> Adds an INVAR property.
    <dt> <tt>-s</tt>
       <dd> Adds a PSL property.
    <dt> <tt>-q</tt>
       <dd> Adds a quantitative (COMPUTE) property.
    <dt> <tt>-p "formula [IN context]"</tt>
       <dd> Adds the <tt>formula</tt> specified on the command-line. <BR>
            <tt>context</tt> is the module instance name which the variables
            in <tt>formula</tt> must be evaluated in.
    <dt> <tt>-n name</tt>
       <dd> Names the added property as "name"
  </dl>

  \sa show_property
*/

int CommandAddProperty (NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  int c = 0;
  short int usedType = 0;
  Prop_Type type = Prop_NoType;
  char* prop = NIL(char);
  char* name = NIL(char);
  node_ptr expr_name = Nil;
  PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  int res;
  const SymbTable_ptr symb_table =
    SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));

  util_getopt_reset();
  while((c = util_getopt(argc, argv, "hlcisqp:n:")) != EOF){
    switch(c){
    case 'h': return(UsageAddProperty(env));
    case 'l':
      if (++usedType > 1) return(UsageAddProperty(env));
      type = Prop_Ltl;
      break;
    case 'c':
      if (++usedType > 1) return(UsageAddProperty(env));
      type = Prop_Ctl;
      break;
    case 'i':
      if (++usedType > 1) return(UsageAddProperty(env));
      type = Prop_Invar;
      break;
    case 's':
      if (++usedType > 1) return(UsageAddProperty(env));
      type = Prop_Psl;
      break;
    case 'q':
      if (++usedType > 1) return(UsageAddProperty(env));
      type = Prop_Compute;
      break;
    case 'p':
      /* this option is optional */
      if (usedType == 0 || prop != NIL(char)) return(UsageAddProperty(env));
      prop = util_optarg;
      break;
    case 'n':
      if (NIL(char) != name) { return UsageAddProperty(env); }
      name = util_strsav(util_optarg);
      break;

    default: return(UsageAddProperty(env));
    }
  }

  if (prop == NIL(char)) {
    /* option -p not used: the property has still to be parsed */
    if (argc != util_optind + 1) return(UsageAddProperty(env));
    prop = argv[util_optind];
  }
  else {
    /* option -p used: the property has already been parsed */
    if (argc != util_optind) return(UsageAddProperty(env));
  }

  if (NIL(char) != name) {
    int res = Parser_ReadIdentifierExprFromString(env, name, &expr_name);

    FREE(name);

    if (res != 0 || expr_name == Nil) {
      StreamMgr_print_error(streams,
              "Parsing error: expected a property name.\n");
      return UsageAddProperty(env);
    }

    expr_name = CompileFlatten_concat_contexts(env, Nil, car(expr_name));

    if (PropDb_get_prop_name_index(prop_db, expr_name) >= 0) {
      StreamMgr_print_error(streams,
              "The property database already contains a"
              " property named \"");
      StreamMgr_nprint_error(streams, wffprint, "%N", expr_name);
      StreamMgr_print_error(streams,  "\"\n");
      return 1;
    }
  }

  if (type == Prop_NoType) {
    return(UsageAddProperty(env));
  }

  /* command hierarchy control */
  if (Compile_check_if_flattening_was_built(env, errstream)) return 1;

  argv += util_optind-1;
  argc -= util_optind-1;

  res = PropDb_prop_parse_and_add(prop_db, symb_table, prop, type, expr_name);

  if (-1 < res) return 0;
  else return 1;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageAddProperty(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: add_property [-h] [(-c | -l | -i | -s | -q)"\
          " -p \"formula\" [IN context] ] [-n \"name\"]\n");
  StreamMgr_print_error(streams,  "  -h \t\tPrints the command usage.\n");
  StreamMgr_print_error(streams,  "  -c \t\tAdds a CTL property.\n");
  StreamMgr_print_error(streams,  "  -l \t\tAdds an LTL property.\n");
  StreamMgr_print_error(streams,  "  -i \t\tAdds an INVAR property.\n");
  StreamMgr_print_error(streams,  "  -s \t\tAdds a PSL property.\n");
  StreamMgr_print_error(streams,  "  -q \t\tAdds a quantitative (COMPUTE) property.\n");
  StreamMgr_print_error(streams,  "  -p \"formula\"\tThe property to be added. "
          "It must be of the type specified.\n");
  StreamMgr_print_error(streams,  "  -n \"name\"\tSets the name of the property to \"name\"\n");
  return(1);
}


/*!
  \command{check_property} Checks a property into the current list of properties,
  or a newly specified property

  \command_args{[-h] [-n number | -P "name"] | [(-c | -l | -i | -s | -q )
  [-p "formula [IN context]"]] }


  Checks the specified property taken from the property list, or adds
  the new specified property and checks it.

  <p>
  Command options:<p>
  <dl>
    <dt> <tt>-h</tt>
       <dd> Prints the help.
    <dt> <tt>-c</tt>
       <dd> Checks all the CTL properties not already checked
    <dt> <tt>-l</tt>
       <dd> Checks all the LTL properties not already checked
    <dt> <tt>-i</tt>
       <dd> Checks all the INVAR properties not already checked
    <dt> <tt>-s</tt>
       <dd> Checks all the PSL properties not already checked
    <dt> <tt>-q</tt>
       <dd> Checks all the COMPUTE properties not already checked
    <dt> <tt>-n number</tt>
       <dd> Checks the property with id <tt>number</tt> in the
            property list if it exists.
    <dt> <tt>-P name</tt>
       <dd> Checks the property named <tt>named</tt> in the
            property list if it exists.
    <dt> <tt>-p "formula [IN context]"</tt>
       <dd> Checks the <tt>formula</tt> specified on the command-line. <BR>
            <tt>context</tt> is the module instance name which the variables
            in <tt>formula</tt> must be evaluated in.
  </dl>
  If no property has been specified via <tt>-n</tt> or <tt>-p</tt> or <tt>-P</tt>,
  then all the properties (of a given type) in the property list will be
  evaluated.

  \sa check_property
*/

int CommandCheckProperty (NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  int c = 0;
  int prop_no = -1;
  Prop_Type pt = Prop_NoType;
  char* formula = NIL(char);
  char* formula_name = NIL(char);
  PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));

  util_getopt_reset();
  while((c = util_getopt(argc, argv, "hn:p:clisqP:")) != EOF){
    switch(c){
    case 'h': return(UsageCheckProperty(env));
    case 'n':
      {
        if (pt != Prop_NoType) return(UsageCheckProperty(env));
        if (formula != NIL(char)) return(UsageCheckProperty(env));
        if (prop_no != -1) return(UsageCheckProperty(env));
        if (formula_name != NIL(char)) return UsageCheckProperty(env);

        prop_no = PropDb_get_prop_index_from_string(prop_db, util_optarg);
        if (prop_no == -1) return(1);

        break;
      }
    case 'P':
      {
        if (pt != Prop_NoType) return(UsageCheckProperty(env));
        if (formula != NIL(char)) return UsageCheckProperty(env);
        if (prop_no != -1) return UsageCheckProperty(env);
        if (formula_name != NIL(char)) return UsageCheckProperty(env);

        formula_name = util_strsav(util_optarg);

        prop_no = PropDb_prop_parse_name(prop_db, formula_name);

        if (prop_no == -1) {
          StreamMgr_print_error(streams,  "No property named \"%s\"\n", formula_name);
          FREE(formula_name);
          return 1;
        }
        break;
      }
    case 'p':
      {
        if (pt == Prop_NoType) return(UsageCheckProperty(env));
        if (prop_no != -1) return(UsageCheckProperty(env));
        if (formula != NIL(char)) return(UsageCheckProperty(env));
        if (formula_name != NIL(char)) return UsageCheckProperty(env);

        formula = util_strsav(util_optarg);
        break;
      }
    case 'c':
      {
        if (prop_no != -1 || pt != Prop_NoType) {
          return(UsageCheckProperty(env));
        }
        pt = Prop_Ctl;
        break;
      }
    case 'l':
      {
        if (prop_no != -1 || pt != Prop_NoType) {
          return(UsageCheckProperty(env));
        }
        pt = Prop_Ltl;
        break;
      }
    case 'i':
      {
        if (prop_no != -1 || pt != Prop_NoType) {
          return(UsageCheckProperty(env));
        }
        pt = Prop_Invar;
        break;
      }
    case 's':
      {
        if (prop_no != -1 || pt != Prop_NoType) {
          return(UsageCheckProperty(env));
        }
        pt = Prop_Psl;
        break;
      }
    case 'q':
      {
        if (prop_no != -1 || pt != Prop_NoType) {
          return(UsageCheckProperty(env));
        }
        pt = Prop_Compute;
        break;
      }
    default:
      return(UsageCheckProperty(env));
    }
  }
  if (argc != util_optind) return(UsageCheckProperty(env));

  /* command hierarchy control */
  if (Compile_check_if_model_was_built(env, errstream, false)) return 1;

  return PropDb_check_property(prop_db, pt, formula, prop_no);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageCheckProperty(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: check_property [-h]\n" \
          "       [-n number |  -P \"name\"] | [-c | -l | -i | -s | -q [-p \"formula\"]]\n");
  StreamMgr_print_error(streams,  "  -h \t\t Prints the command usage.\n");
  StreamMgr_print_error(streams,  "  -n number \t Checks property number.\n");
  StreamMgr_print_error(streams,  "  -P \"name\" \t Checks property name.\n");
  StreamMgr_print_error(streams,  "  -c \t\t Checks CTL properties.\n");
  StreamMgr_print_error(streams,  "  -l \t\t Checks LTL properties.\n");
  StreamMgr_print_error(streams,  "  -i \t\t Checks INVAR properties.\n");
  StreamMgr_print_error(streams,  "  -s \t\t Checks PSL properties.\n");
  StreamMgr_print_error(streams,  "  -q \t\t Checks COMPUTE properties.\n");
  StreamMgr_print_error(streams,  "  -p \"formula\"\t Checks the given formula.\n");
  return(1);
}

/*!
  \command{convert_property_to_invar}

  \command_args{[-n number | -P \"name\" | -p \"formula\"]}


*/

int CommandConvertPropertyToInvar(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  ErrorMgr_ptr const errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  FILE* errstream = StreamMgr_get_error_stream(streams);
  int c = 0;
  int prop_no = -1;
  Prop_Type pt = Prop_NoType;
  char* formula = NIL(char);
  char* formula_name = NIL(char);
  PropDb_ptr prop_db = PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB));
  boolean n_specified = false;
  boolean p_specified = false;
  boolean P_specified = false;
  int retval = 1;
  lsList converted = NULL;
  Prop_ptr property = NULL;
  lsList props = NULL;
  lsStatus ls_status = LS_NIL;

  util_getopt_reset();
  while((c = util_getopt(argc, argv, "hn:p:P:")) != EOF){
    switch(c) {
    case 'h': return(UsageConvertPropertyToInvar(env));
    case 'n':
      {
        if (n_specified || p_specified || P_specified) return UsageConvertPropertyToInvar(env);

        prop_no = PropDb_get_prop_index_from_string(prop_db, util_optarg);

        if (prop_no == -1) return 1;

        n_specified = true;

        break;
      }

    case 'P':
      {
        if (n_specified || p_specified || P_specified) return UsageConvertPropertyToInvar(env);

        formula_name = util_strsav(util_optarg);

        prop_no = PropDb_prop_parse_name(prop_db, formula_name);

        if (prop_no == -1) {
          StreamMgr_print_error(streams,  "No property named \"%s\"\n", formula_name);
          FREE(formula_name);
          return 1;
        }

        P_specified = true;

        break;
      }

    case 'p':
      {
        if (n_specified || p_specified || P_specified) return UsageConvertPropertyToInvar(env);

        formula = util_strsav(util_optarg);

        p_specified = true;

        break;
      }

    default:
      return UsageConvertPropertyToInvar(env);
    }
  }

  if (argc != util_optind) return UsageConvertPropertyToInvar(env);

  if (0 == cmp_struct_get_read_model(cmps)) {
    StreamMgr_print_error(streams,
                          "A model must be read before. Use command read_model\n");
    retval = 1;
    return retval;
  }

#ifdef DEBUG_CONVERT_PROPERTY_TO_INVAR
  StreamMgr_print_error(streams, "Properties before conversion\n");
  PropDb_print_all(prop_db, StreamMgr_get_error_ostream(streams));
#endif

  props = lsCreate();

  CATCH(errmgr) {
    if (n_specified || P_specified) {
      nusmv_assert(-1 != prop_no);
      property = PropDb_get_prop_at_index(prop_db, prop_no);
      ls_status = lsNewEnd(props, (lsGeneric)property, LS_NH);
      nusmv_assert(LS_OK == ls_status);
      converted = Prop_convert_props_to_invar(prop_db, props);
    }
    else if (p_specified) {
      nusmv_assert(NULL != formula);
      nusmv_assert(Prop_NoType != pt);
      property = Prop_create_from_string(env, formula, pt);
      ls_status = lsNewEnd(props, (lsGeneric)property, LS_NH);
      nusmv_assert(LS_OK == ls_status);
      converted = Prop_convert_props_to_invar(prop_db, props);
    }
    else {
      int i;
      PROP_DB_FOREACH(prop_db, i) {
        property = PropDb_get_prop_at_index(prop_db, i);
        ls_status = lsNewEnd(props, (lsGeneric)property, LS_NH);
        nusmv_assert(LS_OK == ls_status);
      }

      converted = Prop_convert_props_to_invar(prop_db, props);
    }

    UNUSED_VAR(converted);
    retval = 0;
  }
  FAIL(errmgr) {
    retval = 1;
  }

#ifdef DEBUG_CONVERT_PROPERTY_TO_INVAR
  StreamMgr_print_error(streams, "Properties after conversion\n");
  PropDb_print_all(prop_db, StreamMgr_get_error_ostream(streams));
#endif

  /* clean */
  if (NULL != props) {
    lsDestroy(props, NULL); props = NULL;
  }

  return retval;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageConvertPropertyToInvar(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,
                        "For each model property that is convertible to an invarspec,\n" \
                        "convert it and add it to the property database.\n"
                        );
  StreamMgr_print_error(streams,
                        "usage: %s" \
                        " [-n number | -P \"name\" |\n       -p \"(G next-expr | AG next-expr)\"]\n",
                        COMMAND_CONVERT_PROPERTY_TO_INVAR_NAME);
  StreamMgr_print_error(streams,  "  -h \t\t Prints the command usage.\n");
  StreamMgr_print_error(streams,  "  -n number \t Convert CTL or LTL property with index ``number''.\n");
  StreamMgr_print_error(streams,  "  -P \"name\" \t Convert CTL or LTL property named ``name''.\n");
  StreamMgr_print_error(streams,  "  -p \"formula\" \t Convert the given CTL or LTL formula.\n");

  return 1;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
