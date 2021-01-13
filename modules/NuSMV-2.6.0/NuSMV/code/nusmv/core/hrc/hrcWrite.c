/* ---------------------------------------------------------------------------


  This file is part of the ``hrc'' package of NuSMV version 2.
  Copyright (C) 2009 by FBK.

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
  \author Sergio Mover
  \brief DEPRECATED Creation of an SMV file of an Hrc structure

  Deprecated module, use hrcDump and the dumpers instead!

  Creates a SMV file from the hrc
  structure.

  The exported function Hrc_WriteModel allows to print a HrcNode_ptr
  structure on a file.

  The file contains static functions needed to print an SMV file given
  the hrc structure.

*/



#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/hrc/hrc.h"
#include "nusmv/core/hrc/HrcNode.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/Slist.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/compile/compile.h" /* for Compile_print_array_define */
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


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

/*!
  \brief Suffix used to rename module names and module variables

  Suffix used to rename module names and module variables
*/
#define HRC_WRITE_MODULE_SUFFIX "_hrc"

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void hrc_write_expr_split(const NuSMVEnv_ptr env,
                                 FILE* out,
                                 Oiter n,
                                 const char* s);

static void hrc_write_spec_split(const NuSMVEnv_ptr env,
                                 FILE* out,
                                 Oiter iter,
                                 const char* s);

static boolean hrc_write_assign_list(const NuSMVEnv_ptr env,
                                     FILE* out,
                                     int assign_node_type,
                                     Oiter assign_iter);

static void hrc_write_print_assign(const NuSMVEnv_ptr env,
                                   FILE * out,
                                   node_ptr lhs,
                                   node_ptr rhs);

static void hrc_write_module_instance(FILE* ofile,
                                      HrcNode_ptr hrcNode,
                                      st_table* printed_module_map,
                                      boolean append_suffix);

static void hrc_write_parameters(const NuSMVEnv_ptr env,
                                 FILE* ofile,
                                 Oiter parameters_iter);

static void
hrc_write_declare_module_variables(FILE* ofile,
                                   HrcNode_ptr child,
                                   st_table* printed_module_map,
                                   boolean append_suffix);

static void hrc_write_print_vars(const NuSMVEnv_ptr env,
                                 FILE* out, HrcNode_ptr hrcNode);

static void hrc_write_print_var_list(const NuSMVEnv_ptr env,
                                     FILE* out, Oiter var_iter);

static void hrc_write_print_defines(FILE* out, HrcNode_ptr hrcNode);

static void hrc_write_print_array_defines(FILE* out, HrcNode_ptr hrcNode);

static void hrc_write_specifications(const NuSMVEnv_ptr env,
                                     FILE* out, HrcNode_ptr hrcNode);

static void hrc_write_spec_pair_list(const NuSMVEnv_ptr env,
                                     FILE* out,
                                     Oiter iter,
                                     const char* section_name);

static boolean hrc_write_constants(const NuSMVEnv_ptr env,
                                   FILE* out, Oiter constants_iter);

static void print_variable_type(const NuSMVEnv_ptr env,
                                FILE* out, node_ptr node);

static void print_scalar_type(const NuSMVEnv_ptr env,
                              FILE* out, node_ptr node);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

int Hrc_WriteModel(HrcNode_ptr hrcNode, FILE * ofile, boolean append_suffix)
{
  int retval = 0;
  st_table* printed_module_map; /* hash table used to keep track of
                                   previously printed modules. */

  HRC_NODE_CHECK_INSTANCE(hrcNode);
  nusmv_assert((FILE *)NULL != ofile);

  printed_module_map = new_assoc();

  /* call the recursive creation of the modules */
  hrc_write_module_instance(ofile,
                            hrcNode,
                            printed_module_map,
                            append_suffix);

  clear_assoc(printed_module_map);
  free_assoc(printed_module_map);

  fflush(ofile);

  return retval;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/



/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Writes the SMV translation of the instance
  module contained in hrcNode on file.

  Writes the SMV translation of the instance
  module contained in hrcNode on file.

  \se printed_module_map is changed to keep track of
  printed modules.
*/
static void hrc_write_module_instance(FILE * ofile,
                                      HrcNode_ptr hrcNode,
                                      st_table* printed_module_map,
                                      boolean append_suffix)
{
  Siter iter;
  node_ptr module_name;
  Slist_ptr rev_child_stack;
  SymbTable_ptr st = HrcNode_get_symbol_table(hrcNode);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  module_name = HrcNode_get_name(hrcNode);

  /* Set the module as printed  */
  insert_assoc(printed_module_map, module_name, PTR_FROM_INT(node_ptr, 1));

  /* prints MODEL name(formal_parameters) */
  fprintf(ofile, "MODULE ");
  print_node(wffprint, ofile, module_name);

  /* main module is never changed with the suffix */
  if ((HRC_NODE(NULL) != HrcNode_get_parent(hrcNode)) &&
      append_suffix) {
    fprintf(ofile, "%s", HRC_WRITE_MODULE_SUFFIX);
  }

  /* print formal parameters of module */
  hrc_write_parameters(env, ofile,
                       HrcNode_get_formal_parameters_iter(hrcNode));
  fprintf(ofile, "\n\n");


  /* Iterates over all children of this node, creating variables and
     assigning module names.
     Children stack is reversed in order to preserve order.
  */
  rev_child_stack =
    Slist_copy_reversed(HrcNode_get_child_hrc_nodes(hrcNode));


  /* All the children of the current nodes are visited instantiating
     each module variable.
  */
  if (! Slist_is_empty(rev_child_stack)) {
    fprintf(ofile, "VAR\n");
  }
  SLIST_FOREACH(rev_child_stack, iter) {
    HrcNode_ptr child;

    child = HRC_NODE(Siter_element(iter));

    /* Declares the new module variables */
    hrc_write_declare_module_variables(ofile,
                                       child,
                                       printed_module_map,
                                       append_suffix);
  }
  if (! Slist_is_empty(rev_child_stack)) {
    fprintf(ofile, "\n");
  }

  /* Prints state, invar and frozen variables */
  hrc_write_print_vars(env, ofile, hrcNode);

  /* Prints define */
  hrc_write_print_defines(ofile, hrcNode);

  /* Prints array define */
  hrc_write_print_array_defines(ofile, hrcNode);

  /* CONSTANTS */
  if (hrc_write_constants(env, ofile,
                          HrcNode_get_constants_iter(hrcNode))) {
    fprintf(ofile, "\n");
  }

  /* ASSIGN: invar, init, next  */
  if (hrc_write_assign_list(env,
                            ofile,
                            -1,
                            HrcNode_get_invar_assign_exprs_iter(hrcNode))) {
    fprintf(ofile, "\n");
  }

  if (hrc_write_assign_list(env,
                            ofile,
                            SMALLINIT,
                            HrcNode_get_init_assign_exprs_iter(hrcNode))) {
    fprintf(ofile, "\n");
  }

  if (hrc_write_assign_list(env,
                            ofile,
                            NEXT,
                            HrcNode_get_next_assign_exprs_iter(hrcNode))) {
    fprintf(ofile, "\n");
  }

  /* CONSTRAINS: INIT, INVAR, TRANS */
  /* JUSTICE/FAIRNESS */
  {
    struct {
      Oiter iter;
      const char* type;
    } lists[] = {
        {{HrcNode_get_init_exprs_iter(hrcNode).node}, "INIT\n"},
        {{HrcNode_get_invar_exprs_iter(hrcNode).node}, "INVAR\n"},
        {{HrcNode_get_trans_exprs_iter(hrcNode).node}, "TRANS\n"},
        {{HrcNode_get_justice_exprs_iter(hrcNode).node}, "JUSTICE\n"},
    };
    unsigned int idx;
    for (idx=0; idx < sizeof(lists)/sizeof(lists[0]); ++idx) {
      hrc_write_expr_split(env, ofile,
                           lists[idx].iter, lists[idx].type);
    }
  }
  /* COMPASSION */
  hrc_write_spec_pair_list(env, ofile,
                           HrcNode_get_compassion_exprs_iter(hrcNode),
                           "COMPASSION\n");

  /* Writes specifications (INVARSPEC CTLSPEC LTLSPEC PSLSPEC
     COMPUTE) */
  hrc_write_specifications(env, ofile, hrcNode);

  /* Recursive creation of child modules.
     Reversed children stack is used to preserve child definition order.
  */
  SLIST_FOREACH(rev_child_stack, iter) {
    HrcNode_ptr child;
    node_ptr assoc_key;
    node_ptr child_module_name;

    child = HRC_NODE(Siter_element(iter));
    child_module_name = HrcNode_get_name(child);

    /* Avoids to print the module multiple times */
    assoc_key = find_assoc(printed_module_map, child_module_name);
    if (Nil == assoc_key) {
      hrc_write_module_instance(ofile,
                                child,
                                printed_module_map,
                                append_suffix);
    }
  } /* end loop on children */

  Slist_destroy(rev_child_stack);
}

/*!
  \brief Declare a module variables, setting the module
  to use and the actual parameters.

  Declare a module variables, setting the module
  to use and the actual parameters.

  \se printed_module_map is changed in the recursive
  calls of the the function.
*/
static void hrc_write_declare_module_variables(FILE * ofile,
                                               HrcNode_ptr child,
                                               st_table* printed_module_map,
                                               boolean append_suffix)
{
  node_ptr instance_name;
  node_ptr child_module_name;
  SymbTable_ptr st = HrcNode_get_symbol_table(child);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  child_module_name = HrcNode_get_name(child);

  /* Declare module variable */
  instance_name = HrcNode_get_instance_name(child);
  print_node(wffprint, ofile, instance_name);
  fprintf(ofile, " : ");
  print_node(wffprint, ofile, child_module_name);

  if (append_suffix) {
    fprintf(ofile, "%s", HRC_WRITE_MODULE_SUFFIX);
  }

  /* Prints actual parameters */
  hrc_write_parameters(env, ofile,
                       HrcNode_get_actual_parameters_iter(child));

  fprintf(ofile, ";\n");
}

/*!
  \brief Prints a list of parameters for module
  declaration or instantiation.

  Prints a list of parameters for module
  declaration or instantiation.

  The parameter list is printed enclosed by brackets, every parameter
  is separated by colon.
*/
static void hrc_write_parameters(const NuSMVEnv_ptr env, FILE* ofile,
                                 Oiter parameters_iter)
{
  boolean first_parameter, has_parameters;
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  has_parameters = ! Oiter_is_end(parameters_iter);

  if (has_parameters) {
    fprintf(ofile, "(");
  }

  first_parameter = true;

  /* Loops all parameters list and print them */
  for(; ! Oiter_is_end(parameters_iter);
      parameters_iter = Oiter_next(parameters_iter)) {
    node_ptr parameter;
    node_ptr parameter_name;
    node_ptr parameter_type;

    parameter = NODE_PTR(Oiter_element(parameters_iter));
    nusmv_assert(Nil != parameter);
    nusmv_assert(CONS == node_get_type(parameter));

    parameter_name = car(parameter);
    nusmv_assert(Nil != parameter_name);

    if (! first_parameter) {
      fprintf(ofile, ", ");
    }
    print_node(wffprint, ofile, parameter_name);


    parameter_type = cdr(parameter);

    /* Parameter type can be Nil now */
    if (Nil != parameter_type) {
      fprintf(ofile, ": ");
      print_node(wffprint, ofile, parameter_type);
    }

    first_parameter = false;
  }

  if (has_parameters) {
    fprintf(ofile, ")");
  }

  return;

}

/*!
  \brief Prints the variable of the module contained in hrcNode.

  Prints the variable of the module contained in
  hrcNode.
  The sections printed are VAR, IVAR and FROZENVAR.
*/
static void hrc_write_print_vars(const NuSMVEnv_ptr env,
                                 FILE* out, HrcNode_ptr hrcNode)
{

  if (! Oiter_is_end(HrcNode_get_state_variables_iter(hrcNode))) {
    fprintf(out, "VAR\n");
    hrc_write_print_var_list(env, out, HrcNode_get_state_variables_iter(hrcNode));
    fprintf(out, "\n");
  }

  if (! Oiter_is_end(HrcNode_get_input_variables_iter(hrcNode))) {
    fprintf(out, "IVAR\n");
    hrc_write_print_var_list(env, out, HrcNode_get_input_variables_iter(hrcNode));
    fprintf(out, "\n");
  }

  if (! Oiter_is_end(HrcNode_get_frozen_variables_iter(hrcNode))) {
    fprintf(out, "FROZENVAR\n");
    hrc_write_print_var_list(env, out, HrcNode_get_frozen_variables_iter(hrcNode));
    fprintf(out, "\n");
  }
}

/*!
  \brief Prints a list of variables.


*/
static void hrc_write_print_var_list(const NuSMVEnv_ptr env,
                                     FILE* out, Oiter var_iter)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));


  /* Visit the list printing each variable */
  for(; ! Oiter_is_end(var_iter); var_iter = Oiter_next(var_iter)) {
    node_ptr variable_node;
    node_ptr variable_name;
    node_ptr variable_type;

    variable_node = NODE_PTR(Oiter_element(var_iter));
    assert(Nil != variable_node);

    /* prints the variable name */
    variable_name = car(variable_node);
    nusmv_assert(Nil != variable_name);
    print_node(wffprint, out, variable_name);
    fprintf(out, " : ");

    /* prints the variable type */
    variable_type = cdr(variable_node);
    print_variable_type(env, out, variable_type);
    fprintf(out, ";\n");
  }

  return;
}

/*!
  \brief Writes DEFINE declarations in SMV format on a
  file.

  Writes DEFINE declarations in SMV format on a
  file.
*/
static void hrc_write_print_defines(FILE* out, HrcNode_ptr hrcNode)
{
  Oiter define_iter;
  boolean has_define;
  SymbTable_ptr st = HrcNode_get_symbol_table(hrcNode);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));


  define_iter = HrcNode_get_defines_iter(hrcNode);
  has_define = ! Oiter_is_end(define_iter);
  if (has_define) {
    fprintf(out, "DEFINE\n");
  }

  /* Visit the list printing each define statement */
  for(; ! Oiter_is_end(define_iter); define_iter = Oiter_next(define_iter)) {
    node_ptr define_node;
    node_ptr define_name;
    node_ptr define_body;

    define_node = NODE_PTR(Oiter_element(define_iter));
    nusmv_assert(Nil != define_node);

    /* prints the define name */
    define_name = car(define_node);
    nusmv_assert(Nil != define_name);
    print_node(wffprint, out, define_name);
    fprintf(out, " := ");

    /* prints the body of the define */
    define_body = cdr(define_node);
    nusmv_assert(Nil != define_body);
    print_node(wffprint, out, define_body);
    fprintf(out, ";\n");
  }

  if (has_define) {
    fprintf(out, "\n");
  }

  return;
}

/*!
  \brief Writes the ARRAY DEFINE declarations contained in hrcNode.

  Writes the ARRAY DEFINE declarations contained in hrcNode.
*/
static void hrc_write_print_array_defines(FILE* out, HrcNode_ptr hrcNode)
{
  Oiter array_define_iter;
  boolean has_array_define;
  SymbTable_ptr st = HrcNode_get_symbol_table(hrcNode);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  array_define_iter = HrcNode_get_array_defines_iter(hrcNode);
  has_array_define = ! Oiter_is_end(array_define_iter);
  if (has_array_define) {
    fprintf(out, "MDEFINE\n");
  }

  /* Visit the list printing each array define statement */
  for(; ! Oiter_is_end(array_define_iter);
      array_define_iter = Oiter_next(array_define_iter)) {
    node_ptr array_define_node;
    node_ptr array_define_name;
    node_ptr array_define_body;

    array_define_node = NODE_PTR(Oiter_element(array_define_iter));
    nusmv_assert(Nil != array_define_node);

    /* prints the array define name */
    array_define_name = car(array_define_node);
    nusmv_assert(Nil != array_define_name);
    print_node(wffprint, out, array_define_name);
    fprintf(out, " := ");

    /* prints the body of the array define */
    array_define_body = cdr(array_define_node);
    nusmv_assert(Nil != array_define_body);
    Compile_print_array_define(env, out, array_define_body);
    fprintf(out, ";\n");
  }

  if (has_array_define) {
    fprintf(out, "\n");
  }

  return;
}

/*!
  \brief Writes an expression in SMV format on a file.

  Writes a generic expression prefixed by a given
  string in SMV format on a file.

  Returns true if at least one expression was printed.
*/
static void hrc_write_expr_split(const NuSMVEnv_ptr env,
                                 FILE* out, Oiter iter, const char* s)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  if (! Oiter_is_end(iter)) {
    for(; ! Oiter_is_end(iter); iter = Oiter_next(iter)) {
      node_ptr elem = NODE_PTR(Oiter_element(iter));

      fprintf(out, "%s ", s);
      print_node(wffprint, out, elem);
      fprintf(out, "\n");
    }
    fprintf(out, "\n");
  }

  return;
}

/*!
  \brief Writes a specification list in SMV format on a file.

  Writes a specification list prefixed by a given
  string in SMV format on a file.
*/
static void hrc_write_spec_split(const NuSMVEnv_ptr env,
                                 FILE* out, Oiter iter, const char* s)
{
  if (! Oiter_is_end(iter)) {
    for(; ! Oiter_is_end(iter); iter = Oiter_next(iter)) {
      const MasterPrinter_ptr wffprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

      node_ptr spec = NODE_PTR(Oiter_element(iter));
      node_ptr expr = car(spec);
      node_ptr name = cdr(spec);

      nusmv_assert((SPEC == node_get_type(spec)) ||
                   (LTLSPEC == node_get_type(spec)) ||
                   (INVARSPEC == node_get_type(spec)) ||
                   (PSLSPEC == node_get_type(spec)) ||
                   (COMPUTE == node_get_type(spec)));

      fprintf(out, "%s ", s);

      /* Support for property Names: Old property structure is in car(n),
         property name is in cdr(n).  */
      if (Nil != name){
        fprintf(out, "NAME ");
        print_node(wffprint, out, name);
        fprintf(out, " := ");
      }

      print_node(wffprint, out, expr);
      /* semicolon is needed for PSLSPEC */
      fprintf(out, ";");
      fprintf(out, "\n");
    }

    fprintf(out, "\n");
  }

  return;
}

/*!
  \brief Writes all the specifications of a module instance.

  Writes all the specifications of a module instance.
*/
static void hrc_write_specifications(const NuSMVEnv_ptr env,
                                     FILE* out,
                                     HrcNode_ptr hrcNode)
{
  hrc_write_spec_split(env, out,
                       HrcNode_get_ctl_properties_iter(hrcNode),
                       "CTLSPEC\n");
  hrc_write_spec_split(env, out,
                       HrcNode_get_ltl_properties_iter(hrcNode),
                       "LTLSPEC\n");
  hrc_write_spec_split(env, out,
                       HrcNode_get_compute_properties_iter(hrcNode),
                       "COMPUTE\n");
  hrc_write_spec_split(env, out,
                       HrcNode_get_invar_properties_iter(hrcNode),
                       "INVARSPEC\n");
  hrc_write_spec_split(env, out,
                       HrcNode_get_psl_properties_iter(hrcNode),
                       "PSLSPEC\n");
}

/*!
  \brief Writes ASSIGN declarations in SMV format on a file.

  Writes ASSIGN declarations in SMV format on a
  file.

  Function returns true if at least an assign was written
*/
static boolean hrc_write_assign_list(const NuSMVEnv_ptr env,
                                     FILE* out,
                                     int assign_node_type,
                                     Oiter assign_iter)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  boolean has_assign;

  has_assign = ! Oiter_is_end(assign_iter);

  if (has_assign) {
    fprintf(out, "ASSIGN\n");
  }

  for(; ! Oiter_is_end(assign_iter); assign_iter = Oiter_next(assign_iter)) {
    node_ptr assign_expression;
    node_ptr assign_lhs_name;
    node_ptr assign_lhs_node;
    node_ptr assign_rhs_node;

    assign_expression = NODE_PTR(Oiter_element(assign_iter));
    nusmv_assert(Nil != assign_expression);

    /* The node to be printed for an assign can be:
       next(assign_lhs_name) for next assign
       init(assign_lhs_name) for init assign
       assign_lhs_name for invar assign

       This is decided by the caller of the function.
    */
    assign_lhs_name = car(assign_expression);
    nusmv_assert(Nil != assign_lhs_name);

    if (assign_node_type < 0) {
      assign_lhs_node = assign_lhs_name;
    } else {
      assign_lhs_node = find_node(nodemgr, assign_node_type, assign_lhs_name, Nil);
    }

    /* Get assign right expression */
    assign_rhs_node = cdr(assign_expression);
    nusmv_assert(Nil != assign_rhs_node);

    hrc_write_print_assign(env, out, assign_lhs_node, assign_rhs_node);
  }

  if (has_assign) {
    fprintf(out, "\n");
  }


  return has_assign;
}

/*!
  \brief Prints an assignement statement

  Prints an assignement statement
*/
static void hrc_write_print_assign(const NuSMVEnv_ptr env,
                                   FILE * out, node_ptr lhs, node_ptr rhs)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  print_node(wffprint, out, lhs);
  fprintf(out, " := ");
  print_node(wffprint, out, rhs);
  fprintf(out, ";\n");
}

/*!
  \brief Writes a list of specification that contains
  pairs in SMV format.

  Writes a list of specification that contains
  pairs in SMV format.
*/
static void hrc_write_spec_pair_list(const NuSMVEnv_ptr env,
                                     FILE* out,
                                     Oiter iter,
                                     const char* section_name)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  /* Visit the list printing each statement */
  if (! Oiter_is_end(iter)) {
    for(; ! Oiter_is_end(iter); iter = Oiter_next(iter)) {
      node_ptr pair;

      pair = NODE_PTR(Oiter_element(iter));
      nusmv_assert(Nil != pair);
      nusmv_assert(CONS == node_get_type(pair));

      fprintf(out, "%s(", section_name);
      print_node(wffprint, out, car(pair));
      fprintf(out, ", ");
      print_node(wffprint, out, cdr(pair));
      fprintf(out, ")\n");
    }
    fprintf(out, "\n");
  }

  return;
}

/*!
  \brief Prints in the output file the SMV
  representations of constants list.

  Prints in the output file the SMV
  representations of constants list.

  Function returns true if at least a constant was printed.
*/
static boolean hrc_write_constants(const NuSMVEnv_ptr env,
                                   FILE *out, Oiter constants_iter)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  boolean first;

  first = true;

  for(; ! Oiter_is_end(constants_iter);
      constants_iter = Oiter_next(constants_iter)) {
    node_ptr constant;
    constant = NODE_PTR(Oiter_element(constants_iter));

    if (first) {
      fprintf(out, "CONSTANTS\n");
    } else {
      fprintf(out, ", ");
    }

    print_node(wffprint, out, constant);

    first = false;
  }

  if (! first) {
    fprintf(out, ";\n");
  }

  return (! first);
}

/*!
  \brief Prints the type of a variable.

  Prints the type of a variable. The printers used
  in compileWrite.c in compile package cannot be used in hrc, unless
  symbol table is used.

  The printer manages the following types: BOOLEAN, INTEGER, REAL,
  UNSIGNED_WORD, SIGNED_WORD, SCALAR, WORD_ARRAY and  ARRAY_TYPE.
*/
static void print_variable_type(const NuSMVEnv_ptr env,
                                FILE* out, node_ptr node)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  int node_type;

  nusmv_assert(Nil != node);
  node_type = node_get_type(node);

  switch (node_type) {
  case BOOLEAN:
  case TWODOTS: /* range */
    print_node(wffprint, out, node);
    break;

  case INTEGER:
    fprintf(out, "integer");
    break;

  case REAL:
    fprintf(out, "real");
    break;

  case SIGNED_WORD:
    fprintf(out, "signed word[");
    print_node(wffprint, out, car(node));
    fprintf(out, "]");
    break;

  case UNSIGNED_WORD:
    fprintf(out, "unsigned word[");
    print_node(wffprint, out, car(node));
    fprintf(out, "]");
    break;

  case SCALAR:
    print_scalar_type(env, out, node);
    break;

  case WORDARRAY_TYPE:
    fprintf(out, "array ");

    fprintf(out, "word[");
    print_node(wffprint, out, car(node));
    fprintf(out, "]");

    fprintf(out, " of ");

    /* recursively prints the array type */
    print_variable_type(env, out, cdr(node));
    break;

  case INTARRAY_TYPE:
    fprintf(out, "array integer of ");

    /* recursively prints the array type */
    print_variable_type(env, out, car(node));
    break;

  case ARRAY_TYPE:

    fprintf(out, "array ");

    /* Prints subrange of array n..m */
    print_node(wffprint, out, car(node));

    fprintf(out, " of ");

    /* recursively prints the array type */
    print_variable_type(env, out, cdr(node));

    break;
  default:
    StreamMgr_print_error(streams,  "Type %d not supported by hrc emitter.\n", node_type);
  }

  return;
}

/*!
  \brief Prints the scalar type of a variable.

  Prints the scalar type of a variable. The
  printer takes care of reversing the CONS list that contains the
  enumeration to preserve the order of the literals in the source
  model.

  \sa print_variable_type
*/
static void print_scalar_type(const NuSMVEnv_ptr env,
                              FILE* out, node_ptr node)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  int node_type;
  node_ptr reversed_literals;
  node_ptr iterator;
  boolean first_literal;

  nusmv_assert(Nil != node);

  node_type = node_get_type(node);
  nusmv_assert(SCALAR == node_type);

  fprintf(out, "{");

  /* reverse the literals of the enumerations to preserve their
     original order */
  reversed_literals = reverse_ns(nodemgr, car(node));

  iterator = reversed_literals;
  first_literal = true;
  while (Nil != iterator) {
    node_ptr literal;

    literal = car(iterator);
    nusmv_assert(Nil != literal);

    if (! first_literal) {
      fprintf(out, ", ");
    }
    print_node(wffprint, out, literal);


    first_literal = false;
    iterator = cdr(iterator);
  }

  fprintf(out, "}");

  free_list(nodemgr, reversed_literals);

  return;
}
