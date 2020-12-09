/* ---------------------------------------------------------------------------


  This file is part of the ``hrc'' package of NuSMV version 2.
  Copyright (C) 2013 by FBK.

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
  \brief Utilities used to declare symbols in a hrc node in a symbol table

  Utilities used to declare symbols in a hrc node in a symbol table

*/


#include "nusmv/core/hrc/hrcSymbTableUtils.h"
#include "nusmv/core/hrc/hrcPrefixUtils.h"
#include "nusmv/core/cinit/NuSMVEnv.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/opt/OptsHandler.h"

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


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void
hrc_symb_table_utils_declare_variables(SymbTable_ptr symb_table,
                                       SymbLayer_ptr layer,
                                       Oiter vars_iter,
                                       node_ptr context,
                                       Instantiation_Variables_Mode_Type mode);

static void
hrc_symb_table_utils_instantiate_array_define(SymbTable_ptr st,
                                              SymbLayer_ptr layer,
                                              node_ptr name,
                                              node_ptr mod_name,
                                              node_ptr definition);


/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief Populates the symbol table for the given node in
                      the given context

  All symbols of the HRC node are flattened and
                      declared into the symbol layer
*/

void hrc_symb_table_utils_populate_symb_table(SymbTable_ptr symb_table,
                                              SymbLayer_ptr layer,
                                              HrcNode_ptr node,
                                              node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const MasterNormalizer_ptr normalizer =
    MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));
  const ErrorMgr_ptr errmgr = ERROR_MGR(NuSMVEnv_get_value(env,
                                                           ENV_ERROR_MANAGER));

  /* PARAMETERS */
  {
    Oiter actuals = HrcNode_get_actual_parameters_iter(node);
    Oiter formals = HrcNode_get_formal_parameters_iter(node);

    for(; ! Oiter_is_end(actuals);
        actuals = Oiter_next(actuals),
        formals = Oiter_next(formals)) {
      node_ptr actual, formal, form_flat;

      nusmv_assert(! Oiter_is_end(formals));
      /* Parameters are stored in HRC as cons nodes with car = name
         and cdr = type. We just need the name */
      actual = car(NODE_PTR(Oiter_element(actuals)));
      formal = car(NODE_PTR(Oiter_element(formals)));
      form_flat = hrc_prefix_utils_concat_context(env, context, formal);

      SymbLayer_declare_parameter(layer, form_flat, car(context), actual);
    }
  }

  /* State Variables */
  hrc_symb_table_utils_declare_variables(symb_table,
                                         layer,
                                         HrcNode_get_state_variables_iter(node),
                                         context,
                                         State_Variables_Instantiation_Mode);
  /* Frozen Variables */
  hrc_symb_table_utils_declare_variables(symb_table,
                                         layer,
                                         HrcNode_get_frozen_variables_iter(node),
                                         context,
                                         Frozen_Variables_Instantiation_Mode);
  /* Input Variables */
  hrc_symb_table_utils_declare_variables(symb_table,
                                         layer,
                                         HrcNode_get_input_variables_iter(node),
                                         context,
                                         Input_Variables_Instantiation_Mode);


  /* FUNCTIONs */
  {
    Oiter functions = HrcNode_get_frozen_functions_iter(node);
    for(; ! Oiter_is_end(functions); functions = Oiter_next(functions)) {
      node_ptr name = car(NODE_PTR(Oiter_element(functions)));
      node_ptr type = cdr(NODE_PTR(Oiter_element(functions)));

      ResolveSymbol_ptr rs;

      rs = SymbTable_resolve_symbol(symb_table, name, context);
      name = ResolveSymbol_get_resolved_name(rs);

      if (! SymbLayer_can_declare_function(layer, name)) {
        /* a more precise error message */
        if (SymbTable_is_symbol_parameter(symb_table, name)) {
          ErrorMgr_error_shadowing(errmgr, name);
        }
        else ErrorMgr_error_redefining(errmgr, name);
      }

      nusmv_assert(SymbLayer_can_declare_function(layer, name));

      {
        SymbType_ptr fun_type =
            Compile_InstantiateType(symb_table, layer,
                                    name, type, context, false);

        SymbLayer_declare_function(layer, name, context, fun_type);
      }
    }
  }

  /* DEFINEs */
  {
    Oiter defines = HrcNode_get_defines_iter(node);
    for(; ! Oiter_is_end(defines); defines = Oiter_next(defines)) {
      node_ptr name = car(NODE_PTR(Oiter_element(defines)));
      node_ptr body = cdr(NODE_PTR(Oiter_element(defines)));
      ResolveSymbol_ptr rs;

      rs = SymbTable_resolve_symbol(symb_table, name, context);
      name = ResolveSymbol_get_resolved_name(rs);

      if (! SymbLayer_can_declare_define(layer, name)) {
        /* a more precise error message */
        if (SymbTable_is_symbol_parameter(symb_table, name)) {
          ErrorMgr_error_shadowing(errmgr, name);
        }
        else ErrorMgr_error_redefining(errmgr, name);
      }

      nusmv_assert(SymbLayer_can_declare_define(layer, name));
      SymbLayer_declare_define(layer, name, context, body);
    }
  }

  /*  ARRAY DEFINE */
  {
    Oiter array_defines = HrcNode_get_array_defines_iter(node);
    for(; ! Oiter_is_end(array_defines); array_defines = Oiter_next(array_defines)) {
      node_ptr name = car(NODE_PTR(Oiter_element(array_defines)));
      node_ptr body = cdr(NODE_PTR(Oiter_element(array_defines)));
      ResolveSymbol_ptr rs;

      rs = SymbTable_resolve_symbol(symb_table, name, context);
      name = ResolveSymbol_get_resolved_name(rs);

      nusmv_assert(SymbLayer_can_declare_array_define(layer, name));
      /* SymbLayer_declare_array_define(layer, name, context, body); */

      hrc_symb_table_utils_instantiate_array_define(symb_table, layer,
                                                    name, context, body);
    }
  }

  /* CONSTANTS */
  /* Constants do not need any type of flattening */
  {
    Oiter constants = HrcNode_get_constants_iter(node);
    for(; ! Oiter_is_end(constants); constants = Oiter_next(constants)) {
      /* [SM] 25/08/2010: added normalization to constant.
         The constant inserted in the symbol table MUST be normalized,
         as it happens in the compile flatten process.
       */
      node_ptr constant =
        MasterNormalizer_normalize_node(normalizer,
                                        NODE_PTR(Oiter_element(constants)));

      if (SymbLayer_can_declare_constant(layer, constant)) {
        SymbLayer_declare_constant(layer, constant);
      }
    }
  }
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Declares each variable of the given array within
                      the symbol table

  Declares each variable of the given array within
                      the symbol table.
*/
static void hrc_symb_table_utils_declare_variables(SymbTable_ptr symb_table,
                                                   SymbLayer_ptr layer,
                                                   Oiter vars_iter,
                                                   node_ptr context,
                                                   Instantiation_Variables_Mode_Type mode)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const MasterNormalizer_ptr normalizer =
    MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));

  for(; ! Oiter_is_end(vars_iter); vars_iter = Oiter_next(vars_iter)) {
    node_ptr var_node = NODE_PTR(Oiter_element(vars_iter));
    node_ptr var = car(var_node);
    node_ptr type = cdr(var_node);

    /* Some identifiers may not have been created with find_node. Trap
       this here. */
    var = MasterNormalizer_normalize_node(normalizer, var);

    if (PROCESS == node_get_type(type)) {
      StreamMgr_print_error(streams,  "%s\n",
                            "Processes are not yet supported by the HRC hierarchy");

      error_unreachable_code();
    }
    else {
      const OptsHandler_ptr opts =
        OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

      SymbType_ptr symbolicType
        = Compile_InstantiateType(symb_table,
                                  layer,
                                  var, type, context, false);
      boolean success
        = Compile_DeclareVariable(symb_table, layer,
                                  var, symbolicType, context, mode);
      nusmv_assert(success ||
                   ( (! opt_keep_single_value_vars(opts)) &&
                    SymbTable_is_symbol_define(symb_table,
                                               hrc_prefix_utils_concat_context(env, context, var))));
    }
  } /* loop on vars */
}

/*!
  \brief Instantiates the elements of a array define

  For every cell and every dimension create a correct
   binding in the symbol layer

  \se Elements are added to the layer an the symbol table
*/
static void hrc_symb_table_utils_instantiate_array_define(SymbTable_ptr st,
                                                          SymbLayer_ptr layer,
                                                          node_ptr name,
                                                          node_ptr mod_name,
                                                          node_ptr definition)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (!SymbLayer_can_declare_define(layer, name)) {
    ErrorMgr_error_redefining(errmgr, name);
    error_unreachable_code();
  }

  switch (node_get_type(definition)) {
  case ARRAY_DEF:
    {
      node_ptr iter;
      int idx;
      nusmv_assert((cdr(definition) == Nil) &&
                   "Wrong node arity found: ARRAY_DEF must be unary!");

      /* Declare this symbol */
      SymbLayer_declare_array_define(layer, name, mod_name, definition);

      /* Instantiate every element of the array individually
         with first index = 0 */
      for (idx = 0, iter = car(definition);
           iter != Nil;
           idx += 1, iter = cdr(iter)) {
        /* definition has to be a list of values */
        nusmv_assert(CONS == node_get_type(iter));

        {  /* Instantiate name[idx] element */
          node_ptr index =
              find_node(nodemgr, NUMBER, NODE_FROM_INT(idx), Nil);
          hrc_symb_table_utils_instantiate_array_define(
              st, layer,
              find_node(nodemgr, ARRAY, name, index),
              mod_name, car(iter));
        }
      }
      break;
    }

  default:
    {
      /* Declare this element */
      SymbLayer_declare_define(layer, name, mod_name, definition);
    }
  }
}
