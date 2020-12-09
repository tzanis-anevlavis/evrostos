/* ---------------------------------------------------------------------------


   This file is part of the ``compile'' package of NuSMV version 2.
   Copyright (C) 2011 by FBK-irst.

   NuSMV version 2 is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   NuSMV version 2 is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied
   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
   See the GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
   MA 02111-1307 USA.

   For more information on NuSMV see <http://nusmv.fbk.eu> or
   email to <nusmv-users@fbk.eu>.
   Please report bugs to <nusmv-users@fbk.eu>.

   To contact the NuSMV development board, email to <nusmv@fbk.eu>. 

-----------------------------------------------------------------------------*/

/*!
  \author Roberto Cavada
  \brief Constructs HrcNodes out of a parse tree

  \todo: Missing description

*/



#include "nusmv/core/compile/compile.h"
#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/ErrorMgr.h"


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void compile_fill_hrc_add_var(const NuSMVEnv_ptr env,
                                     NodeMgr_ptr nomgr,
                                     node_ptr var_name, node_ptr var_type,
                                     int var_kind, HrcNode_ptr hrc_result,
                                     hash_ptr mod_defs);

static void compile_fill_hrc_invalidate(HrcNode_ptr hrc);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

HrcNode_ptr Compile_hrc_from_parse_tree(const NuSMVEnv_ptr env,
                                        NodeMgr_ptr nomgr,
                                        node_ptr mod_name,
                                        node_ptr parse_tree)
{
  const ErrorMgr_ptr ermgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  HrcNode_ptr hrc_result;
  hash_ptr mod_defs;

  /* first constructs the module hash */
  mod_defs = new_assoc();
  nusmv_assert((hash_ptr) NULL != mod_defs);

  {
    node_ptr iter;
    for (iter=parse_tree; Nil != iter; iter=cdr(iter)) {
      node_ptr mod_def = car(iter);
      if (Nil != mod_def && node_get_type(mod_def) == MODULE) {
        node_ptr name = NodeMgr_find_atom(nomgr, caar(mod_def));
        insert_assoc(mod_defs, name, mod_def);
      }
    }
  }

  /* now generate the HrcNodes */
  {
    node_ptr mod_def;

    mod_name = NodeMgr_find_atom(nomgr, mod_name); /* make it unique */

    mod_def = find_assoc(mod_defs, mod_name);
    if (Nil == mod_def) { /* mod_name must be found */
      ErrorMgr_error_undefined(ermgr, mod_name);
    }

    hrc_result = HrcNode_create(env);

    HrcNode_set_lineno(hrc_result, node_get_lineno(mod_def));

    { /* Adds formal/actual parameters to the module instance */
      node_ptr iter;
      for (iter=car(mod_def); Nil != iter; iter=cdr(iter)) {
        node_ptr par = NodeMgr_cons(nomgr, car(iter), Nil);
        HrcNode_add_formal_parameter(hrc_result, par);
      }
    }

    /* fills the hrc */
    Compile_fill_hrc_from_mod_body(env,
                                   nomgr,
                                   cdr(mod_def), /* body */
                                   hrc_result,
                                   mod_defs);
  }

  free_assoc(mod_defs);

  return hrc_result;
}

void Compile_fill_hrc_from_mod_body(const NuSMVEnv_ptr env,
                                    NodeMgr_ptr nomgr,
                                    node_ptr mod_body,
                                    HrcNode_ptr hrc_result,
                                    hash_ptr mod_defs)
{
  node_ptr decls;

  for (decls = mod_body; Nil != decls; decls = cdr(decls)) {
    node_ptr cur_decl = car(decls);

    switch (node_get_type(cur_decl)) {
    case DEFINE: {
      node_ptr decl_iter;
      for (decl_iter = car(cur_decl);
           decl_iter != Nil;
           decl_iter = cdr(decl_iter)) {
        node_ptr cur_define = car(decl_iter);
        HrcNode_add_define(hrc_result,
                           NodeMgr_cons(nomgr,
                                            car(cur_define),
                                            cdr(cur_define)));
      }
      break;
    }

    case VAR:
    case FROZENVAR:
    case IVAR: {
      node_ptr decl_iter;
      for (decl_iter = car(cur_decl);
           decl_iter != Nil;
           decl_iter = cdr(decl_iter)) {
        node_ptr cur_var = car(decl_iter);

        compile_fill_hrc_add_var(env, nomgr,
                                 car(cur_var), cdr(cur_var),
                                 node_get_type(cur_decl),
                                 hrc_result, mod_defs);
      }
      break;
    }

    case CONSTANTS:
      HrcNode_add_constants(hrc_result, car(cur_decl));
      break;

    case TRANS: {
      node_ptr decl_iter;
      for (decl_iter = car(cur_decl);
           decl_iter != Nil;
           decl_iter = cdr(decl_iter)) {
        HrcNode_add_trans_expr(hrc_result, car(decl_iter));
      }
      break;
    }

    case INIT: {
      node_ptr decl_iter;
      for (decl_iter = car(cur_decl);
           decl_iter != Nil;
           decl_iter = cdr(decl_iter)) {
        HrcNode_add_init_expr(hrc_result, car(decl_iter));
      }
      break;
    }

    case INVAR: {
      node_ptr decl_iter;
      for (decl_iter = car(cur_decl);
           decl_iter != Nil;
           decl_iter = cdr(decl_iter)) {
        HrcNode_add_invar_expr(hrc_result, car(decl_iter));
      }
      break;
    }

    case ASSIGN:
      compile_add_assign_hrc(nomgr, hrc_result, car(cur_decl));
      break;

    case SPEC: {
      node_ptr decl_iter;
      for (decl_iter = car(cur_decl);
           decl_iter != Nil;
           decl_iter = cdr(decl_iter)) {
        HrcNode_add_ctl_property_expr(hrc_result, car(decl_iter));
      }
      break;
    }

    case LTLSPEC: {
      node_ptr decl_iter;
      for (decl_iter = car(cur_decl);
           decl_iter != Nil;
           decl_iter = cdr(decl_iter)) {
        HrcNode_add_ltl_property_expr(hrc_result, car(decl_iter));
      }
      break;
    }

    case PSLSPEC: {
      node_ptr decl_iter;
      for (decl_iter = car(cur_decl);
           decl_iter != Nil;
           decl_iter = cdr(decl_iter)) {
        HrcNode_add_psl_property_expr(hrc_result, car(decl_iter));
      }
      break;
    }

    case INVARSPEC: {
      node_ptr decl_iter;
      for (decl_iter = car(cur_decl);
           decl_iter != Nil;
           decl_iter = cdr(decl_iter)) {
        HrcNode_add_invar_property_expr(hrc_result, car(decl_iter));
      }
      break;
    }

    case COMPUTE: {
      node_ptr decl_iter;
      for (decl_iter = car(cur_decl);
           decl_iter != Nil;
           decl_iter = cdr(decl_iter)) {
        HrcNode_add_compute_property_expr(hrc_result, car(decl_iter));
      }
      break;
    }

    case JUSTICE: {
      node_ptr decl_iter;
      for (decl_iter = car(cur_decl);
           decl_iter != Nil;
           decl_iter = cdr(decl_iter)) {
        HrcNode_add_justice_expr(hrc_result, car(decl_iter));
      }
      break;
    }

    case COMPASSION: {
      node_ptr decl_iter;
      for (decl_iter = car(cur_decl);
           decl_iter != Nil;
           decl_iter = cdr(decl_iter)) {
        node_ptr compassion =
          NodeMgr_cons(nomgr, caar(decl_iter), cdar(decl_iter));
        HrcNode_add_compassion_expr(hrc_result, compassion);
      }
      break;
    }

    /* ignored cases */
    case PRED:
    case MIRROR:
      /* no action here */
      break;

      /* unsupported cases which invalidate the hierarchy root */
    case ISA:
      compile_fill_hrc_invalidate(hrc_result);

    default:
      error_unreachable_code(); /* unknown kind of declaration */

    } /* switch-case */

  } /* top-level loop on definitions */
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief 

  

  \se None
*/

static void compile_fill_hrc_add_var(const NuSMVEnv_ptr env,
                                     NodeMgr_ptr nomgr,
                                     node_ptr var_name, node_ptr var_type,
                                     int var_kind, HrcNode_ptr hrc_result,
                                     hash_ptr mod_defs)
{
  const ErrorMgr_ptr ermgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  switch (node_get_type(var_type)) {
    /* Basic types */
  case BOOLEAN:
  case TWODOTS:
  case SCALAR:
  case INTEGER:
  case REAL:
  case UNSIGNED_WORD:
  case SIGNED_WORD:
  case WORDARRAY:

    /* Array or matrix type */
  case ARRAY_TYPE: {
    node_ptr hrc_var = NodeMgr_cons(nomgr, var_name, var_type);
    /* here single-value domains are declared as variables, and
       will be moved to defines if needed later when elaborating
       the hrc */
    switch (var_kind) {
    case VAR:
      HrcNode_add_state_variable(hrc_result, hrc_var);
      break;
    case IVAR:
      HrcNode_add_input_variable(hrc_result, hrc_var);
      break;
    case FROZENVAR:
      HrcNode_add_frozen_variable(hrc_result, hrc_var);
      break;
    default:
      error_unreachable_code(); /* unknown kind of variable */
    }
    break;
  }

  case MODTYPE: {
    /* searches the definition in the cache */
    HrcNode_ptr hrc_child;
    node_ptr child_crude_mod_name;
    node_ptr child_mod_name;
    node_ptr child_mod_def;

    /* In this way a unique reference to the module name is used,
       so the module name can be used as a key. */
    child_crude_mod_name = car(var_type);
    child_mod_name = NodeMgr_find_atom(nomgr, child_crude_mod_name);

    /* hash has to be provided here */
    nusmv_assert((hash_ptr) NULL != mod_defs);
    child_mod_def  = (node_ptr) find_assoc(mod_defs, child_mod_name);
    if (NODE_PTR(Nil) == child_mod_def) {
      /* The module is undefined */
      ErrorMgr_error_undefined(ermgr, child_mod_name);
    }

    hrc_child = HrcNode_create(env);
    HrcNode_set_lineno(hrc_child, node_get_lineno(child_mod_def));
    HrcNode_set_instance_name(hrc_child, var_name);
    HrcNode_set_parent(hrc_child, hrc_result);
    HrcNode_add_child_hrc_node(hrc_result, hrc_child);

    /* Adds formal/actual parameters to the module instance */
    compile_make_params_hrc(env,
                            var_name,
                            cdr(var_type), /* non-flattened actual parameters */
                            car(child_mod_def), /* formal params */
                            hrc_child);

    /* recursive call to fill the child */
    Compile_fill_hrc_from_mod_body(env,
                                   nomgr,
                                   cdr(child_mod_def),
                                   hrc_child,
                                   mod_defs);
    break;
  }

  case PROCESS:
    compile_fill_hrc_invalidate(hrc_result);
    break;

  default:
    ErrorMgr_internal_error(ermgr, "compile_fill_hrc_add_var: type = %d",
                            node_get_type(var_type));
  } /* var type switch */
}


/*!
  \brief 

  

  \se None
*/

static void compile_fill_hrc_invalidate(HrcNode_ptr hrc)
{
  HrcNode_ptr iter;
  for (iter = hrc;
       !HrcNode_is_root(iter);
       iter = HrcNode_get_parent(iter)) {
    /* Set a flag so we know that the HRC hierarchy is not usable */
    HrcNode_set_undef(iter, (void*)~0);
  }
}
