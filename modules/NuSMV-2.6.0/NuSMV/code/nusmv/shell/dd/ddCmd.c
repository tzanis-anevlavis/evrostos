/* ---------------------------------------------------------------------------


  This file is part of the ``dd'' package of NuSMV version 2.
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
  \brief The shell interface of the DD package

  Shell interface of the DD package. here are provided
  the shell commands to modyfy all the modifiable DD options.

*/


#include "nusmv/shell/cmd/cmd.h"
#include "nusmv/shell/dd/ddCmd.h"

#include "nusmv/core/opt/opt.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/error.h"

int CommandDynamicVarOrdering(NuSMVEnv_ptr env, int argc, char** argv);
int CommandSetBddParameters(NuSMVEnv_ptr env, int argc, char** argv);
int CommandPrintBddStats(NuSMVEnv_ptr env, int argc, char** argv);

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static int UsageDynamicVarOrdering(const NuSMVEnv_ptr env);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void dd_AddCmd(NuSMVEnv_ptr env)
{
  Cmd_CommandAdd(env, "dynamic_var_ordering", CommandDynamicVarOrdering, 0, true);
  Cmd_CommandAdd(env, "set_bdd_parameters" , CommandSetBddParameters, 0, true);
  Cmd_CommandAdd(env, "print_bdd_stats" , CommandPrintBddStats, 0, true);

}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \command{dynamic_var_ordering} Deals with the dynamic variable ordering.

  \command_args{[-d] [-e &lt;method&gt;] [-f &lt;method&gt;] [-h]}


  Controls the application and the modalities of (dynamic) variable
  ordering. Dynamic ordering is a technique to reorder the BDD variables
  to reduce the size of the existing BDDs. When no options are specified,
  the current status of dynamic ordering is displayed. At most one of the
  options <tt>-e</tt>, <tt>-f</tt>, and <tt>-d</tt> should be specified.<p>

  Dynamic ordering may be time consuming, but can often reduce the size of
  the BDDs dramatically. A good point to invoke dynamic ordering
  explicitly (using the <tt>-f</tt> option) is after the commands
  <tt>build_model</tt>, once the transition relation has been built.  It is
  possible to save the ordering found using <tt>write_order</tt> in order to
  reuse it (using <tt>build_model -i order-file</tt>) in the future.<p>

  Command options:<p>
  <dl>
    <dt> <tt>-d</tt>
       <dd> Disable dynamic ordering from triggering automatically.
    <dt> <tt>-e &lt;method&gt;</tt>
       <dd> Enable dynamic ordering to trigger automatically whenever a
            certain threshold on the overall BDD size is reached.
            <tt>&lt;method&gt;</tt> must be one of the following:
            <ul>
            <li> <b>sift</b>: Moves each variable throughout the order to
                 find an optimal position for that variable (assuming all
                 other variables are fixed).  This generally achieves
                 greater size reductions than the window method, but is slower.
            <li> <b>random</b>: Pairs of variables are randomly chosen, and
                 swapped in the order. The swap is performed by a series of
                 swaps of adjacent variables. The best order among those
                 obtained by the series of swaps is retained. The number of
                 pairs chosen for swapping equals the number of variables
                 in the diagram.
            <li> <b>random_pivot</b>: Same as <b>random</b>, but the two
                 variables are chosen so that the first is above the
                 variable with the largest number of nodes, and the second
                 is below that variable.  In case there are several
                 variables tied for the maximum number of nodes, the one
                 closest to the root is used.
            <li> <b>sift_converge</b>: The <b>sift</b> method is iterated
                 until no further improvement is obtained.
            <li> <b>symmetry_sift</b>: This method is an implementation of
                 symmetric sifting. It is similar to sifting, with one
                 addition: Variables that become adjacent during sifting are
                 tested for symmetry. If they are symmetric, they are linked
                 in a group. Sifting then continues with a group being
                 moved, instead of a single variable.
            <li> <b>symmetry_sift_converge</b>: The <b>symmetry_sift</b>
                 method is iterated until no further improvement is obtained.
            <li> <b>window{2,3,4}</b>: Permutes the variables within windows
                 of "n" adjacent variables, where "n" can be either 2, 3 or 4,
                 so as to minimize the overall BDD size.<p>
            <li> <b>window{2,3,4}_converge</b>: The <b>window{2,3,4}</b> method
                 is iterated until no further improvement is obtained.
            <li> <b>group_sift</b>: This method is similar to
                 <b>symmetry_sift</b>, but uses more general criteria to
                 create groups.
            <li> <b>group_sift_converge</b>: The <b>group_sift</b> method is
                 iterated until no further improvement is obtained.
            <li> <b>annealing</b>: This method is an implementation of
                 simulated annealing for variable ordering. This method is
                 potentially very slow.
            <li> <b>genetic</b>: This method is an implementation of a
                 genetic algorithm for variable ordering. This method is
                 potentially very slow.
            <li> <b>exact</b>: This method implements a dynamic programming
                 approach to exact reordering. It only stores a BDD
                 at a time. Therefore, it is relatively efficient in
                 terms of memory. Compared to other reordering
                 strategies, it is very slow, and is not recommended
                 for more than 16 boolean variables.
            <li> <b>linear</b>: This method is a combination of
                 sifting and linear transformations.
            <li> <b>linear_converge</b>: The <b>linear</b> method is
                 iterated until no further improvement is obtained.
            </ul><br>
    <dt> <tt>-f &lt;method&gt;</tt>
     <dd> Force dynamic ordering to be invoked immediately. The values for
          <tt>&lt;method&gt;</tt> are the same as in option <tt>-e</tt>.
  </dl>
*/

int CommandDynamicVarOrdering(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  DDMgr_ptr const dd_manager = DD_MGR(NuSMVEnv_get_value(env, ENV_DD_MGR));
  int c;
  dd_reorderingtype currentMethod;
  int dynOrderingMethod = REORDER_NONE; /* for lint */
  boolean currentlyEnabled = false;
  boolean disableFlag   = false;
  boolean enableFlag    = false;
  boolean forceFlag     = false;
  DdDynVarOrderAction action = 0;
  int retval = 0;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "df:e:h")) != EOF) {
    switch (c) {
    case 'f':
      forceFlag = true;
      dynOrderingMethod = StringConvertToDynOrderType(util_optarg);
      if (dynOrderingMethod == REORDER_NONE) {
        StreamMgr_print_error(streams,  "unknown method: %s\n", util_optarg);
        retval = UsageDynamicVarOrdering(env); goto exit;
      }
      break;

    case 'e':
      enableFlag = true;
      dynOrderingMethod = StringConvertToDynOrderType(util_optarg);
      if (dynOrderingMethod == REORDER_NONE) {
        StreamMgr_print_error(streams,  "unknown method: %s\n", util_optarg);
        retval = UsageDynamicVarOrdering(env); goto exit;
      }
      break;

    case 'd':
      disableFlag = true;
      break;

    case 'h':
    default:
      retval = UsageDynamicVarOrdering(env); goto exit;
    }
  }

  if (!NuSMVEnv_has_value(env, ENV_DD_MGR)) {
    StreamMgr_print_error(streams,  "The DD Manager has not been created yet.\n");
    retval = 1; goto exit;
  }

  /* At most one option is allowed. */
  if ((disableFlag && enableFlag) ||
      (disableFlag && forceFlag) ||
      (enableFlag && forceFlag)) {
    StreamMgr_print_error(streams,  "Only one of -d, -f, -e is allowed.\n");
    retval = 1; goto exit;
  }
  /*
   * Get the current method for reading and to save in case temporarily
   * overwritten.
  */
  currentlyEnabled = dd_reordering_status(dd_manager, &currentMethod);

  /* if no option selected, print the status and exit */
  if (!(disableFlag || enableFlag || forceFlag)) {
    if (currentlyEnabled) {
      StreamMgr_print_output(streams,  "Dynamic variable ordering is enabled ");
      StreamMgr_print_output(streams,  "with method: \"%s\".\n",
                     DynOrderTypeConvertToString(currentMethod));
    }
    else {
      StreamMgr_print_output(streams,  "Dynamic variable ordering is disabled.\n");
    }

    retval = 0;
  }
  else {
    if (disableFlag) action = DD_DYN_VAR_ORDER_ACTION_DISABLE;
    else if (enableFlag) action = DD_DYN_VAR_ORDER_ACTION_ENABLE;
    else if (forceFlag) action = DD_DYN_VAR_ORDER_ACTION_FORCE;
    else error_unreachable_code();

    retval = Dd_dynamic_var_ordering(env, dd_manager, dynOrderingMethod, action);
  }

 exit:
  return retval;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int UsageDynamicVarOrdering(const NuSMVEnv_ptr env)
{
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_error(streams,  "usage: dynamic_var_ordering [[-d] | [-e method] | [-f method] [-h]]\n");
  StreamMgr_print_error(streams,  "   -d \t\tDisables dynamic ordering\n");
  StreamMgr_print_error(streams,  "   -e method \tEnables dynamic ordering with method\n");
  StreamMgr_print_error(streams,  "   -f method \tForces dynamic ordering with method\n");
  StreamMgr_print_error(streams,  "   -h \t\tPrints the command usage\n");
  return 1;
}


/*!
  \command{set_bdd_parameters} Creates a table with the value of all currently
  active NuSMV flags and change accordingly the configurable parameters
  of the BDD package.

  \command_args{[-h] [-s]}

  Applies the variables table of the NuSMV environnement
  to the BDD package, so the user can set specific BDD parameters to the
  given value. This command works in conjunction with the
  <tt>print_bdd_stats</tt> and <tt>set</tt> commands.<p>

  <tt>print_bdd_stats</tt> first prints a report of the parameters and
  statistics of the current bdd_manager. By using the command <tt>set</tt>,
  the user may modify the value of any of the parameters of the
  underlying BDD package. The way to do it is by setting a value in
  the variable <tt>BDD.parameter name</tt> where <tt>parameter
  name</tt> is the name of the parameter exactly as printed by the
  <tt>print_bdd_stats</tt> command.<p>

  Command options:<p>

  <dl>
    <dt> -s
       <dd> Prints the BDD parameter and statistics after the modification.

  </dl>

*/

int CommandSetBddParameters(NuSMVEnv_ptr env, int argc, char** argv)
{
  StreamMgr_ptr const streams =
   STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  boolean  showAfter;
  int c;
  DDMgr_ptr dd_manager;

  showAfter = false;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "hs")) != EOF) {
    switch (c) {
      case 'h':
        goto usage;
        break;
      case 's':
        showAfter = true;
        break;
      default:
        goto usage;
    }
  }

  /* flatten_hierarchy and static_order must have been invoked already. */
  if (!NuSMVEnv_has_value(env, ENV_DD_MGR)) {
    StreamMgr_print_error(streams,  "The DD Manager has not been created yet.\n");
    return 1;
  }

  dd_manager = (DDMgr_ptr )NuSMVEnv_get_value(env, ENV_DD_MGR);

  return Dd_set_bdd_parameters(env, dd_manager, showAfter);

usage:
  StreamMgr_print_error(streams,  "usage: set_bdd_parameters [-h | -s]\n");
  StreamMgr_print_error(streams,  "   -h  Prints the command usage.\n");
  StreamMgr_print_error(streams,  "   -s  Prints also the bdd statistics.\n");

  return 1;
}

/*!
  \command{print_bdd_stats} Prints out the BDD statistics and parameters

  \command_args{[-h]}

  Prints the statistics for the BDD package. The
  amount of information depends on the BDD package configuration
  established at compilation time. The configurtion parameters are
  printed out too. More information about statistics and parameters
  can be found in the documentation of the CUDD Decision Diagram
  package.
*/

int CommandPrintBddStats(NuSMVEnv_ptr env, int argc, char** argv)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  DDMgr_ptr dd = (DDMgr_ptr )NuSMVEnv_get_value(env, ENV_DD_MGR);

  int c;

  util_getopt_reset();
  while ((c = util_getopt(argc, argv, "h")) != EOF) {
    switch (c) {
      case 'h':
        goto usage;
        break;
      default:
        goto usage;
    }
  }

  if (!NuSMVEnv_has_value(env, ENV_DD_MGR)) {
    StreamMgr_print_error(streams,  "The DD Manager has not been created yet.\n");
    return 1;
  }

  return dd_print_stats(env, dd, outstream);

usage:
  StreamMgr_print_error(streams,  "usage: print_bdd_stats [-h]\n");
  StreamMgr_print_error(streams,  "   -h  Prints the command usage.\n");

  return 1;
}
