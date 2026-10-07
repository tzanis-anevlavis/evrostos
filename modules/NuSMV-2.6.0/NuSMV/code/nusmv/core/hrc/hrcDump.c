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
  \brief Creation of an SMV file of an Hrc structure

  Creates a SMV file from the hrc
  structure.

  The exported function Hrc_WriteModel allows to print a HrcNode_ptr
  structure on a file.

  The file contains static functions needed to print an SMV file given
  the hrc structure.

*/



#include "nusmv/core/hrc/hrc.h"
#include "nusmv/core/hrc/HrcNode.h"

#include "nusmv/core/hrc/dumpers/HrcDumper.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/parser.h"
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
#define _DUMP_SNIPPET_BEGIN(snippet, dumper, info)  \
  (info)->stage = HRC_STAGE_BEGIN;                  \
  HrcDumper_dump_snippet(dumper, snippet, info)

#define _DUMP_SNIPPET_BEGIN_INDENT(snippet, dumper, info)  \
  (info)->stage = HRC_STAGE_BEGIN;                         \
  HrcDumper_dump_snippet(dumper, snippet, info);           \
  HrcDumper_inc_indent(dumper)

#define _DUMP_SNIPPET_END(snippet, dumper, info)    \
  (info)->stage = HRC_STAGE_END;                    \
  HrcDumper_dump_snippet(dumper, snippet, info)

#define _DUMP_SNIPPET_END_INDENT(snippet, dumper, info)    \
  HrcDumper_dec_indent(dumper);                            \
  (info)->stage = HRC_STAGE_END;                           \
  HrcDumper_dump_snippet(dumper, snippet, info)

#define _DUMP_SNIPPET_BEGIN_END(snippet, dumper, info)  \
  (info)->stage = HRC_STAGE_BEGIN | HRC_STAGE_END;      \
  HrcDumper_dump_snippet(dumper, snippet, info)


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void hrc_dump_module_instance(HrcNode_ptr hrcNode,
                                     HrcDumper_ptr dumper,
                                     HrcDumperInfo* info,
                                     hash_ptr printed_module_map);

static void hrc_dump_compile_info(HrcNode_ptr hrcNode,
                                  HrcDumper_ptr dumper,
                                  HrcDumperInfo* info);


/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Hrc_DumpModel(HrcNode_ptr hrcNode, HrcDumper_ptr dumper)
{
  HrcDumperInfo info;
  hash_ptr printed_module_map; /* hash table used to keep track of
                                  previously printed modules. */

  HRC_NODE_CHECK_INSTANCE(hrcNode);

  printed_module_map = new_assoc();

  /* top-level */
  info.hrcNode = hrcNode;
  _DUMP_SNIPPET_BEGIN(HDS_HRC_TOP, dumper, &info);

  /* list of modules */
  info.list_is_empty = ((HrcNode_ptr) NULL == hrcNode);
  _DUMP_SNIPPET_BEGIN(HDS_LIST_MODS, dumper, &info);

  /* call the recursive creation of the modules */
  hrc_dump_module_instance(hrcNode, dumper, &info, printed_module_map);

  info.hrcNode = hrcNode;
  info.list_is_empty = ((HrcNode_ptr) NULL == hrcNode);
  _DUMP_SNIPPET_END(HDS_LIST_MODS, dumper, &info);

  /* compiler info */
  hrc_dump_compile_info(hrcNode, dumper, &info);

  _DUMP_SNIPPET_END(HDS_HRC_TOP, dumper, &info);

  free_assoc(printed_module_map);
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
static void hrc_dump_module_instance(HrcNode_ptr hrcNode,
                                     HrcDumper_ptr dumper,
                                     HrcDumperInfo* info,
                                     hash_ptr printed_module_map)
{
  node_ptr module_name = HrcNode_get_name(hrcNode);
  Slist_ptr rev_child_stack = \
    Slist_copy_reversed(HrcNode_get_child_hrc_nodes(hrcNode));

  /* Set the module as printed  */
  insert_assoc(printed_module_map, module_name, PTR_FROM_INT(node_ptr, 1));

  /* sets the currently processed node */
  info->hrcNode = hrcNode;

  /* ---------------------------------------------------------------------- */
  { /* module prototype */

    info->n1.name = HrcNode_get_crude_name(hrcNode);
    info->n2.lineno = HrcNode_get_lineno(hrcNode);
    _DUMP_SNIPPET_BEGIN_INDENT(HDS_MOD, dumper, info);
    /* name */
    _DUMP_SNIPPET_BEGIN_END(HDS_MOD_NAME, dumper, info);

    { /* formal params */

      Oiter params;

      params = HrcNode_get_formal_parameters_iter(hrcNode);

      info->list_is_empty = (Oiter_is_end(params));
      _DUMP_SNIPPET_BEGIN(HDS_LIST_MOD_FORMAL_PARAMS, dumper, info);

      while(! Oiter_is_end(params)) {
        info->n1.name = car(NODE_PTR(Oiter_element(params)));
        info->n2.type = cdr(NODE_PTR(Oiter_element(params)));
        info->last_in_list = Oiter_is_end(Oiter_next(params));
        _DUMP_SNIPPET_BEGIN_END(HDS_MOD_FORMAL_PARAM, dumper, info);

        params = Oiter_next(params);
      }

      _DUMP_SNIPPET_END(HDS_LIST_MOD_FORMAL_PARAMS, dumper, info);
    } /* end of formal parameters */
  }

  /* ---------------------------------------------------------------------- */
  { /* Iterates over all children of this node, creating variables
     and assigning module names. Children stack is reversed in
     order to preserve order. */
    Siter iter;

    info->symb_cat = SYMBOL_STATE_VAR;
    info->list_is_empty = Slist_is_empty(rev_child_stack);
    _DUMP_SNIPPET_BEGIN_INDENT(HDS_LIST_MOD_INSTANCES, dumper, info);

    SLIST_FOREACH (rev_child_stack, iter) {
      HrcNode_ptr child = HRC_NODE(Siter_element(iter));

      info->last_in_list = Siter_is_end(Siter_next(iter));
      info->n1.name = HrcNode_get_instance_name(child);
      info->n2.type = HrcNode_get_name(child);
      _DUMP_SNIPPET_BEGIN(HDS_MOD_INSTANCE, dumper, info);

      _DUMP_SNIPPET_BEGIN_END(HDS_MOD_INSTANCE_VARNAME, dumper, info);

      info->n1.name = HrcNode_get_name(child);
      _DUMP_SNIPPET_BEGIN_END(HDS_MOD_INSTANCE_MODNAME, dumper, info);

      { /* actual parameters */
        Oiter actuals = HrcNode_get_actual_parameters_iter(child);

        info->list_is_empty = (Oiter_is_end(actuals));
        _DUMP_SNIPPET_BEGIN(HDS_LIST_MOD_INSTANCE_ACTUAL_PARAMS, dumper, info);

        for (; ! Oiter_is_end(actuals);
             actuals = Oiter_next(actuals)) {
          info->n1.value = car(Oiter_element(actuals));
          info->last_in_list = Oiter_is_end(Oiter_next(actuals));
          _DUMP_SNIPPET_BEGIN_END(HDS_MOD_INSTANCE_ACTUAL_PARAM, dumper, info);
        }

        _DUMP_SNIPPET_END(HDS_LIST_MOD_INSTANCE_ACTUAL_PARAMS, dumper, info);
      } /* end of actual params */

      info->last_in_list = Siter_is_end(Siter_next(iter));
      _DUMP_SNIPPET_END(HDS_MOD_INSTANCE, dumper, info);
    }

    info->list_is_empty = Slist_is_empty(rev_child_stack);
    _DUMP_SNIPPET_END_INDENT(HDS_LIST_MOD_INSTANCES, dumper, info);
  } /* end of mod instances */

  /* ---------------------------------------------------------------------- */
  { /* data (non-instance) variables */
      Oiter oi = HrcNode_get_state_variables_iter(hrcNode);

    struct {
      Oiter iter;
      SymbCategory cat;
    } lists[] = {
        { {HrcNode_get_state_variables_iter(hrcNode).node}, SYMBOL_STATE_VAR },
        { {HrcNode_get_input_variables_iter(hrcNode).node}, SYMBOL_INPUT_VAR },
        { {HrcNode_get_frozen_variables_iter(hrcNode).node}, SYMBOL_FROZEN_VAR },
    };
    unsigned int idx;
    for (idx=0; idx < sizeof(lists)/sizeof(lists[0]); ++idx) {
      Oiter iter;

      info->symb_cat = lists[idx].cat;
      info->list_is_empty = Oiter_is_end(lists[idx].iter);
      _DUMP_SNIPPET_BEGIN_INDENT(HDS_LIST_SYMBOLS, dumper, info);

      for (iter = lists[idx].iter; ! Oiter_is_end(iter);
           iter = Oiter_next(iter)) {
        node_ptr elem = NODE_PTR(Oiter_element(iter));
        info->n1.name = car(elem);
        info->n2.type = cdr(elem);
        info->last_in_list = Oiter_is_end(Oiter_next(iter));
        _DUMP_SNIPPET_BEGIN_END(HDS_SYMBOL, dumper, info);
      }

      _DUMP_SNIPPET_END_INDENT(HDS_LIST_SYMBOLS, dumper, info);
    }
  }

  /* ---------------------------------------------------------------------- */
  { /* data (non-instance) defines */
    Oiter lists[] = {
        {HrcNode_get_defines_iter(hrcNode).node},
        {HrcNode_get_array_defines_iter(hrcNode).node}
    };
    unsigned int idx;
    for (idx=0; idx < sizeof(lists)/sizeof(lists[0]); ++idx) {
      Oiter iter;

      info->symb_cat = SYMBOL_DEFINE;
      info->list_is_empty = Oiter_is_end(lists[idx]);
      _DUMP_SNIPPET_BEGIN_INDENT(HDS_LIST_SYMBOLS, dumper, info);

      for (iter = lists[idx]; ! Oiter_is_end(iter);
           iter = Oiter_next(iter)) {
        node_ptr elem = NODE_PTR(Oiter_element(iter));
        info->n1.name = car(elem);
        info->n2.body = cdr(elem);
        info->last_in_list = Oiter_is_end(Oiter_next(iter));
        _DUMP_SNIPPET_BEGIN_END(HDS_SYMBOL, dumper, info);
      }

      _DUMP_SNIPPET_END_INDENT(HDS_LIST_SYMBOLS, dumper, info);
    }
  }

  /* ---------------------------------------------------------------------- */
  { /* uninterpreted functions  */
    Oiter lists[] = {
        {HrcNode_get_frozen_functions_iter(hrcNode).node},
    };
    unsigned int idx;
    for (idx=0; idx < sizeof(lists)/sizeof(lists[0]); ++idx) {
      Oiter iter;

      info->symb_cat = SYMBOL_FUNCTION;
      info->list_is_empty = Oiter_is_end(lists[idx]);
      _DUMP_SNIPPET_BEGIN_INDENT(HDS_LIST_SYMBOLS, dumper, info);

      for (iter = lists[idx]; ! Oiter_is_end(iter);
           iter = Oiter_next(iter)) {
        node_ptr elem = NODE_PTR(Oiter_element(iter));
        info->n1.name = car(elem);
        info->n2.body = cdr(elem);
        info->last_in_list = Oiter_is_end(Oiter_next(iter));
        _DUMP_SNIPPET_BEGIN_END(HDS_SYMBOL, dumper, info);
      }

      _DUMP_SNIPPET_END_INDENT(HDS_LIST_SYMBOLS, dumper, info);
    }
  }

  /* ---------------------------------------------------------------------- */
  { /* constants */
    Oiter iter = HrcNode_get_constants_iter(hrcNode);

    info->symb_cat = SYMBOL_CONSTANT;
    info->list_is_empty = Oiter_is_end(iter);
    _DUMP_SNIPPET_BEGIN_INDENT(HDS_LIST_SYMBOLS, dumper, info);

    for (; ! Oiter_is_end(iter); iter = Oiter_next(iter)) {
      info->n1.name = NODE_PTR(Oiter_element(iter));
      info->last_in_list = Oiter_is_end(Oiter_next(iter));
      _DUMP_SNIPPET_BEGIN_END(HDS_SYMBOL, dumper, info);
    }

    _DUMP_SNIPPET_END_INDENT(HDS_LIST_SYMBOLS, dumper, info);
  }

  /* ---------------------------------------------------------------------- */
  { /* ASSIGN: invar, init, next */
    struct {
      HrcDumperSnippet id;
      Oiter iter;
    } lists[] = {
        { HDS_ASSIGN_INIT, {HrcNode_get_init_assign_exprs_iter(hrcNode).node} },
        { HDS_ASSIGN_INVAR, {HrcNode_get_invar_assign_exprs_iter(hrcNode).node} },
        { HDS_ASSIGN_NEXT, {HrcNode_get_next_assign_exprs_iter(hrcNode).node} },
    };

    boolean is_empty = true;
    unsigned int idx;
    for (idx=0; idx<sizeof(lists)/sizeof(lists[0]); ++idx) {
      if (! Oiter_is_end(lists[idx].iter)) {
        is_empty = false;
        break;
      }
    }

    info->list_is_empty = is_empty;
    _DUMP_SNIPPET_BEGIN_INDENT(HDS_LIST_ASSIGNS, dumper, info);
    for (idx=0; idx<sizeof(lists)/sizeof(lists[0]); ++idx) {
      Oiter iter;
      for (iter = lists[idx].iter; ! Oiter_is_end(iter);
           iter = Oiter_next(iter)) {
        node_ptr elem = NODE_PTR(Oiter_element(iter));
        info->n1.name = car(elem);
        info->n2.expr = cdr(elem);
        info->last_in_list = Oiter_is_end(Oiter_next(iter));
        _DUMP_SNIPPET_BEGIN_END(lists[idx].id, dumper, info);
      }
    }

    _DUMP_SNIPPET_END_INDENT(HDS_LIST_ASSIGNS, dumper, info);
  }

  /* ---------------------------------------------------------------------- */
  { /* CONSTRAINS: INIT, INVAR, TRANS */
    struct {
      HrcDumperSnippet id;
      Oiter iter;
    } lists[] = {
        { HDS_CONSTRAINT_INIT, {HrcNode_get_init_exprs_iter(hrcNode).node} },
        { HDS_CONSTRAINT_INVAR, {HrcNode_get_invar_exprs_iter(hrcNode).node} },
        { HDS_CONSTRAINT_TRANS, {HrcNode_get_trans_exprs_iter(hrcNode).node} },
    };

    boolean is_empty = true;
    unsigned int idx;
    for (idx=0; idx<sizeof(lists)/sizeof(lists[0]); ++idx) {
      if (! Oiter_is_end(lists[idx].iter)) {
        is_empty = false;
        break;
      }
    }
    info->list_is_empty = is_empty;
    _DUMP_SNIPPET_BEGIN(HDS_LIST_CONSTRAINTS, dumper, info);
    /* RC: dumped in reversed order here: (issue 2552)" */
    for (idx=0; idx<sizeof(lists)/sizeof(lists[0]); ++idx) {
      Oiter iter;
      for (iter = lists[idx].iter; ! Oiter_is_end(iter);
           iter = Oiter_next(iter)) {
        info->n1.expr = Oiter_element(iter);
        info->last_in_list = Oiter_is_end(Oiter_next(iter));
        _DUMP_SNIPPET_BEGIN_END(lists[idx].id, dumper, info);
      }
    }
    _DUMP_SNIPPET_END(HDS_LIST_CONSTRAINTS, dumper, info);
  }

  /* ---------------------------------------------------------------------- */
  { /* JUSTICE/FAIRNESS and COMPASSION */
    struct {
      HrcDumperSnippet id;
      Oiter iter;
    } lists[] = {
        { HDS_JUSTICE, {HrcNode_get_justice_exprs_iter(hrcNode).node} },
      { HDS_COMPASSION, {HrcNode_get_compassion_exprs_iter(hrcNode).node} },
    };

    boolean is_empty = true;
    unsigned int idx;
    for (idx=0; idx<sizeof(lists)/sizeof(lists[0]); ++idx) {
      if (! Oiter_is_end(lists[idx].iter)) {
        is_empty = false;
        break;
      }
    }
    info->list_is_empty = is_empty;
    _DUMP_SNIPPET_BEGIN(HDS_LIST_FAIRNESS, dumper, info);

    for (idx=0; idx<sizeof(lists)/sizeof(lists[0]); ++idx) {
      Oiter iter;
      for (iter = lists[idx].iter; ! Oiter_is_end(iter);
           iter = Oiter_next(iter)) {
        node_ptr elem = NODE_PTR(Oiter_element(iter));
        if (HDS_COMPASSION == lists[idx].id) {
          info->n1.expr = car(elem);
          info->n2.expr = cdr(elem);
        }
        else {
          info->n1.expr = elem;
        }

        info->last_in_list = Oiter_is_end(Oiter_next(iter));
        _DUMP_SNIPPET_BEGIN_END(lists[idx].id, dumper, info);
      }
    }
    _DUMP_SNIPPET_END(HDS_LIST_FAIRNESS, dumper, info);
  }

  /* ---------------------------------------------------------------------- */
  { /* specifications (INVARSPEC CTLSPEC LTLSPEC PSLSPEC COMPUTE) */
    struct {
      Prop_Type spec_type;
      Oiter iter;
    } lists[] = {
      { Prop_Ctl, {HrcNode_get_ctl_properties_iter(hrcNode).node} },
      { Prop_Ltl, {HrcNode_get_ltl_properties_iter(hrcNode).node} },
      { Prop_Psl, {HrcNode_get_psl_properties_iter(hrcNode).node} },
      { Prop_Invar, {HrcNode_get_invar_properties_iter(hrcNode).node} },
      { Prop_Compute, {HrcNode_get_compute_properties_iter(hrcNode).node} },
    };

    boolean is_empty = true;
    unsigned int idx;
    for (idx=0; idx<sizeof(lists)/sizeof(lists[0]); ++idx) {
      if (! Oiter_is_end(lists[idx].iter)) {
        is_empty = false;
        break;
      }
    }
    info->list_is_empty = is_empty;
    _DUMP_SNIPPET_BEGIN(HDS_LIST_SPECS, dumper, info);

    for (idx=0; idx<sizeof(lists)/sizeof(lists[0]); ++idx) {
      Oiter iter;
      for (iter = lists[idx].iter; ! Oiter_is_end(iter);
           iter = Oiter_next(iter)) {
        node_ptr elem = NODE_PTR(Oiter_element(iter));
        info->spec_type = lists[idx].spec_type;
        info->n1.name = cdr(elem);
        info->n2.expr = car(elem);
        info->last_in_list = Oiter_is_end(Oiter_next(iter));
        _DUMP_SNIPPET_BEGIN_END(HDS_SPEC, dumper, info);
      }
    }

    _DUMP_SNIPPET_END(HDS_LIST_SPECS, dumper, info);
  }


  /* ---------------------------------------------------------------------- */
  /* close module */
  info->n1.name = HrcNode_get_crude_name(hrcNode);
  info->n2.lineno = HrcNode_get_lineno(hrcNode);
  info->hrcNode = hrcNode;
  _DUMP_SNIPPET_END_INDENT(HDS_MOD, dumper, info);


  /* ---------------------------------------------------------------------- */
  { /* Recursive creation of child modules. Reversed children stack
       is used to preserve child definition order. */
    Siter iter;
    SLIST_FOREACH(rev_child_stack, iter) {
      HrcNode_ptr child;
      node_ptr assoc_key;
      node_ptr child_module_name;

      child = HRC_NODE(Siter_element(iter));
      child_module_name = HrcNode_get_name(child);

      /* Avoids to print the module multiple times */
      assoc_key = find_assoc(printed_module_map, child_module_name);
      if (Nil == assoc_key) {
        hrc_dump_module_instance(child, dumper, info, printed_module_map);
      }
    } /* end loop on children */
  }

  /* cleanup and exit */
  Slist_destroy(rev_child_stack);
}

/*!
  \brief Dumps the compiler information

  
*/
static void hrc_dump_compile_info(HrcNode_ptr hrcNode,
                                  HrcDumper_ptr dumper,
                                  HrcDumperInfo* info)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(hrcNode));
  node_ptr errors = Parser_get_syntax_errors_list(env);

  info->hrcNode = hrcNode;
  info->list_is_empty = (Nil == errors);
  _DUMP_SNIPPET_BEGIN_INDENT(HDS_LIST_COMPILER_INFO, dumper, info);

  info->list_is_empty = (Nil == errors);
  _DUMP_SNIPPET_BEGIN_INDENT(HDS_LIST_SYNTAX_ERRORS, dumper, info);

  {
    node_ptr iter = errors;
    while (iter != Nil) {
      Parser_get_syntax_error(car(iter),
                              &(info->error.filename),
                              &(info->error.lineno),
                              &(info->error.token),
                              &(info->error.message));
      _DUMP_SNIPPET_BEGIN_END(HDS_ERROR, dumper, info);
      iter = cdr(iter);
    }
  }

  _DUMP_SNIPPET_END_INDENT(HDS_LIST_SYNTAX_ERRORS, dumper, info);
  _DUMP_SNIPPET_END_INDENT(HDS_LIST_COMPILER_INFO, dumper, info);
}
