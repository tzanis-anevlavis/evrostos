/* ---------------------------------------------------------------------------


  This file is part of the ``compile'' package of NuSMV version 2.
  Copyright (C) 1998-2004 by CMU and FBK-irst.

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
  \author Marco Roveri, Roberto Cavada
  \brief Internal declaration needed for the compilation.

  This file provides the user routines to perform
  compilation of the model

*/


#ifndef __NUSMV_CORE_COMPILE_COMPILE_INT_H__
#define __NUSMV_CORE_COMPILE_COMPILE_INT_H__


#include "nusmv/core/compile/compile.h"

#include "nusmv/core/compile/symb_table/symb_table_int.h"
#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/compile/symb_table/SymbLayer.h"

#include "nusmv/core/fsm/FsmBuilder.h"

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/utils/assoc.h"

#include "nusmv/core/opt/opt.h"
#include "nusmv/core/dd/dd.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/set/set.h"

#include "nusmv/core/hrc/HrcNode.h"

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/* The hash table for Compile_check_next memoization */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ST_CHECK_NEXT_HASH "ccnh"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ST_CHECK_INPUTS_HASH "ccih"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ST_BEVAL_EXPR2BEXPR_HASH "cbeh"

/* This hash associates to a variable the corresponding complete cone of
   influence, once it has been computes.
   (node_ptr)var --> (Set_t)complete_cone_of_influence */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ST_CONE_COI_HASH "ccch"

/* This hash associates to each formula the corresponding set of
   dependencies. The formula is fully traversed and symbols are resolved.
   (node_ptr)formula --> (Set_t)dependencies */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ST_CONE_DEPENDENCIES_HASH "ccdh"

/* This hash associates to each formula the corresponding set of constants
   (node_ptr)formula --> (Set_t)constants */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ST_CONE_CONSTS_HASH "cccoh"

/* This hash associates to a variable the corresponding cone of influence at
   depth 0, once it has been computes.
   (node_ptr)var --> (Set_t)cone_of_influence_at_depth_0 */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ST_CONE_COI0_HASH "cc0h"

/*!
  \brief  This hash associates to each formula --> 1 (false) or 2 (true) 
  depending on expr contains attime nodes or not in it. */
/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ST_CHECK_NESTED_ATTIME_HASH "ccnah"

extern int nusmv_yylineno;

extern cmp_struct_ptr cmps;

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \command{print_iwls95options} Prints the Iwls95 Options.

  \command_args{[-h]}

  This command prints out the configuration
  parameters of the IWLS95 clustering algorithm, i.e.
  <tt>image_verbosity</tt>, <tt>image_cluster_size</tt> and
  <tt>image_W{1,2,3,4}</tt>.
*/
int CommandIwls95PrintOption(NuSMVEnv_ptr env, int argc, char** argv);

/*!
  \command{print_clusterinfo} Prints out  information about the
  clustering. This command is *deprecated* in 2.4

  \command_args{[-h] | [-m] | [-o output-file]}

  Deprecated in 2.4: use print_fsm_stats instead.

  This command prints out information
  regarding each cluster. In particular for each cluster it prints
  out the cluster number, the size of the cluster (in BDD nodes), the
  variables occurring in it, the size of the cube that has to be
  quantified out relative to the cluster and the variables to be
  quantified out.<p>

  Command options:<p>
  <dl>
    <dt> <tt>-m</tt>
       <dd> Pipes the output generated by the command through the
            program specified by the <tt>PAGER</tt> shell variable if
            defined, or through the UNIX utility "more".
    <dt> <tt>-o output-file</tt>
       <dd> Redirects the generated output to the file
            <tt>output-file</tt>.
  </dl>
  
*/
int CommandCPPrintClusterInfo(NuSMVEnv_ptr env, int argc, char** argv);

/*!
  \brief Initializes the cmp structure

  
*/
cmp_struct_ptr cmp_struct_init(void);

/*!
  \brief Free the cmp structure

  
*/
void cmp_struct_quit(cmp_struct_ptr);

/*!
  \brief Traverses the module hierarchy and extracts the
   information needed to compile the automaton.

  This function is a subfunction of
                       Compile_FlattenHierarchy.

   This function traverses the module hierarchy and extracts the
   information needed to compile the automaton. The hierarchy of modules
   is flattened, the variables are added to the symbol table, all the
   necessary parts of the model are collected (i.e. the formulae to be
   verified, the initial expressions, etc).

   The returned value is a structure constraining all the collected parts
   which are:
   the list of TRANS, INIT, INVAR, ASSIGN, SPEC, COMPUTE, LTLSPEC,
   PSLSPEC, INVARSPEC, JUSTICE, COMPASSION,
   a full list of variables declared in the hierarchy,
   a hash table associating variables to their assignments and constrains.
   See FlatHierarchy class for more info.
   
*/
void Compile_ConstructHierarchy(const NuSMVEnv_ptr env, SymbTable_ptr symb_table,
SymbLayer_ptr, node_ptr, node_ptr,
node_ptr, FlatHierarchy_ptr, HrcNode_ptr, hash_ptr, boolean);

/*!
  \brief Builds the parameters of a module from the list of formal
and actual parameters of the module itself.

  Builds the parameters of a module from the list
of formal parameters of the module itself.<br>There must be a one to
one correspondence between the elements of <tt>actual_list</tt> and
<tt>formal_list</tt> parameters. If the number of elements of the
lists are different then, an error occurs. The list
<tt>actual_list</tt> must be a list of non-flattened actual
parameters. For hrc structure it is not necessary to store the
flattening information that is implicit in the hierarchy.

  \se In <tt>hrc_result</tt> the lists of formal and
actual parameter used to instatiate a module is changed.
*/
void compile_make_params_hrc(const NuSMVEnv_ptr env,
                                    node_ptr basename,
                                    node_ptr actual_list,
                                    node_ptr formal_list,
                                    HrcNode_ptr hrc_result);

/*!
  \brief Add assign declarations in hrc_result.

  Add assign declarations found in the assign list in
  hrc_result. The type of assign is inferred by the node type
  found.

  \se Contents of hrc_result is changed adding an
  assign constraint.
*/
void compile_add_assign_hrc(NodeMgr_ptr nodemgr,
                                   HrcNode_ptr hrc_result,
                                   node_ptr assign_list);

/* This will be removed when assignment of bit selection is implemented */

/* WARNING: (MR) This will be removed when assignment of bit selection is
   implemented */

/*!
  \brief Error message for unsupported feature

  
*/
void error_bit_selection_assignment_not_supported(NuSMVEnv_ptr env,
                                                         node_ptr name);

/*!
  \brief Rewrites the toint operator for word expressions
                       conversion

  This functions takes a word expression and rewrites it
                       as a circuit in order to convert the word
                       expression into an integer expression.

                       For unsigned word[N], we rewrite the operator as follows:

                       (w[0:0] = 0ud1_1 ? 1 : 0) +
                       (w[1:1] = 0ud1_1 ? 2 : 0) +
                       ..... +
                       (w[N-1:N-1] = 0ud1_1 ? 2^(N-1) : 0)

                       For signed word[N], we do the following:
                       case
                       w[N-1:N-1] = 0ud1_0 :
                       (w[0:0] = 0ud1_1 ? 1 : 0) +
                       (w[1:1] = 0ud1_1 ? 2 : 0) +
                       ..... +
                       (w[N-2:N-2] = 0ud1_1 ? 2^(N-2) : 0);
                       TRUE:
                       -((w[0:0] = 0ud1_1 ? 0 : 1) +
                       (w[1:1] = 0ud1_1 ? 0 : 2) +
                       ..... +
                       (w[N-2:N-2] = 0ud1_1 ? 0 : 2^(N-2)) + 1);
                       esac
*/
node_ptr
compile_flatten_rewrite_word_toint_cast(const NuSMVEnv_ptr env,
                                        node_ptr body,
                                        SymbType_ptr type);



#endif /* __NUSMV_CORE_COMPILE_COMPILE_INT_H__ */
