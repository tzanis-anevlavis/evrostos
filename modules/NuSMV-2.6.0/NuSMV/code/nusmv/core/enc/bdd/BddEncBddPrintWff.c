/* ---------------------------------------------------------------------------


  This file is part of the ``bdd.enc'' package of NuSMV version 2.
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
  \author Marco Pensallorto
  \brief Bdd to Well Formed Formulas conversions/printout

  This module exports functions to generate or print a
  Bdd as an optimized Well Formed Formula.

*/

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include <math.h>

#include "nusmv/core/enc/enc.h"
#include "nusmv/core/enc/bdd/bddInt.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/compile/compile.h"
/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief A shorthand to ease reading of bdd_enc_bdd_to_wff_rec

  Builds a node representing the Bool casting of a single
  word bit.

  \se none
*/
#define WORD_BIT(wb)                                                    \
  find_node(nodemgr,                                                             \
            CAST_BOOL,                                                  \
            find_node(nodemgr,                                                   \
                      BIT_SELECTION,                                    \
                      (car((wb))),                                      \
                      find_node(nodemgr,                                         \
                                COLON,                                  \
                                find_node(nodemgr, NUMBER, (cdr((wb))), Nil),    \
                                find_node(nodemgr, NUMBER, (cdr((wb))), Nil))),  \
            Nil)

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static bdd_ptr
bdd_enc_get_scalar_essentials(BddEnc_ptr self, bdd_ptr bdd,
                              NodeList_ptr vars);
static node_ptr
bdd_enc_bdd_to_wff_rec(BddEnc_ptr self, NodeList_ptr vars,
                       bdd_ptr bdd, hash_ptr cache);
static assoc_retval
bdd_enc_hash_free_bdd_counted(char* key, char* data, char* arg);

static NodeList_ptr
bdd_enc_get_preprocessed_vars(BddEnc_ptr self, NodeList_ptr vars);

#ifdef PRINT_BDD_DEBUG
static void
bdd_enc_debug_bdd_to_wff(BddEnc_ptr self, bdd_ptr bdd, node_ptr expr);
#endif

/**AutomaticEnd***************************************************************/

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void BddEnc_print_formula_info(BddEnc_ptr self,
                               Expr_ptr formula,
                               boolean print_models,
                               boolean print_formula,
                               OStream_ptr out)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  bdd_ptr phi;
  double cardinality;

  phi = BddEnc_expr_to_bdd(self, formula, Nil);
  cardinality = BddEnc_get_minterms_of_bdd(self, phi);

  OStream_printf(out, "formula models: %g (2^%g)\n",
                 cardinality, log(cardinality)/log(2.0));

  /* one of these flags can be enabled, not both */
  nusmv_assert(!print_models || !print_formula);
  if (print_models) {
    BddEnc_print_set_of_trans_models(self, phi, /* false, */ out);
  }
  else if (print_formula) {
    BoolEnc_ptr benc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(self));

    const array_t* layer_names =
      BaseEnc_get_committed_layer_names(BASE_ENC(self));

    SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(self));
    NodeList_ptr all_vars = SymbTable_get_layers_sf_vars(st, layer_names);
    NodeList_ptr scalar_vars = NodeList_create();
    ListIter_ptr iter;

    /* encoding variables are not allowed in the wff printer */
    NODE_LIST_FOREACH(all_vars, iter) {
      node_ptr v = NodeList_get_elem_at(all_vars, iter);
      if (BoolEnc_is_var_bit(benc, v)) continue;
      NodeList_append(scalar_vars, v);
    }
    NodeList_destroy(all_vars);

    StreamMgr_print_output(streams,  "\nFORMULA = \n");
    BddEnc_print_bdd_wff(self, phi, scalar_vars,
                         true, false, 0, out);

    NodeList_destroy(scalar_vars);
  }

  bdd_free(BddEnc_get_dd_manager(self), phi);
}

void BddEnc_print_bdd_wff(BddEnc_ptr self,
                          bdd_ptr bdd,         /* the input bdd */
                          NodeList_ptr vars,   /* variables */
                          boolean do_sharing,  /* requires dag sharing */
                          boolean do_indent,   /* requires indentation */
                          int start_at_column, /* align to column (indent only) */
                          OStream_ptr out            /* the stream to write to */
                          )
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  node_ptr expr = BddEnc_bdd_to_wff(self, bdd, vars);

  hash_ptr dag_info_hash = NULL;
  hash_ptr defines_hash = NULL;

  /* sharing pre-processing: if sharing is enabled, convert the
   * expression using compile_convert_to_dag.
   **/
  if (do_sharing) {
    node_ptr dag_expr;

    dag_info_hash = new_assoc();
    defines_hash = new_assoc();

    Compile_make_dag_info(env, expr, dag_info_hash);
    dag_expr = Compile_convert_to_dag(env,
                                      BaseEnc_get_symb_table(BASE_ENC(self)),
                                      expr,
                                      dag_info_hash,
                                      defines_hash);

    /* the expression to print is now the result of the previous operation */
    expr = dag_expr;
  }

  /* print the expression using the appropriate function */
  if (do_indent) {
    const MasterPrinter_ptr iwffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_IWFF_PRINTER));

    /* indented printout */
    print_node_indent_at(iwffprint,
                         OStream_get_stream(out),
                         expr, start_at_column);
  }

  else {
    /* non indented printout */
    print_node(wffprint, OStream_get_stream(out), expr);
  }

  OStream_printf(out, "\n");

  /* sharing post-processing: if sharing is enabled, print the defines
   * introduced by the dumper.
  **/
  if (do_sharing) {
    OStream_printf(out, "\n");
    Compile_write_dag_defines(env, OStream_get_stream(out), defines_hash);
  }

  /* this cleanup is required only if sharing was enabled. */
  if (do_sharing) {
    Compile_destroy_dag_info(env, dag_info_hash, defines_hash);

    free_assoc(dag_info_hash);
    free_assoc(defines_hash);
  }
}

node_ptr BddEnc_bdd_to_wff(
                           BddEnc_ptr self,
                           bdd_ptr bdd,        /* the input bdd */
                           NodeList_ptr vars   /* variables */
                           )
{
  /* DD manager local reference */
  DDMgr_ptr dd = BddEnc_get_dd_manager(self);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  NodeList_ptr preprocessed_vars;

  hash_ptr memoization_hash;
  node_ptr res;

  if (bdd_is_false(dd, bdd)) {
    return ExprMgr_false(exprs);
  }

  if (bdd_is_true(dd, bdd)) {
    return ExprMgr_true(exprs);
  }

  /* preprocess variables list (see bdd_enc_get_preprocessed_vars) */
  preprocessed_vars = bdd_enc_get_preprocessed_vars(self, vars);

  /* memoized recursive inner function */
  memoization_hash = new_assoc();
  res = bdd_enc_bdd_to_wff_rec(self,
                               preprocessed_vars,
                               bdd,
                               memoization_hash);

#ifdef PRINT_BDD_DEBUG
  /* correctnesss check (this may halt NuSMV) */
  bdd_enc_debug_bdd_to_wff(self, bdd, res);
#endif

  /* cleanup */
  NodeList_destroy(preprocessed_vars);

  /* underef all the BDDs */
  clear_assoc_and_free_entries_arg(
                                   memoization_hash,
                                   bdd_enc_hash_free_bdd_counted,
                                   (char*) dd);
  free_assoc(memoization_hash);

  return res;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Used to deref bdds in the sharing hashtable

  \todo Missing description
*/


static assoc_retval
bdd_enc_hash_free_bdd_counted (char* key, char* data, char* arg)
{
  bdd_free((DDMgr_ptr ) arg, (bdd_ptr) key);
  return ASSOC_DELETE;
}

/*!
  \brief Compute scalar essentials of a bdd.

  Computes the scalar essentials of a bdd, picking
  identifiers from the variables in vars list. Used as part of
  bdd_enc_bdd_to_wff_rec implementation.

  \sa bdd_enc_bdd_to_wff
*/
static bdd_ptr
bdd_enc_get_scalar_essentials(
                              BddEnc_ptr self,  /* current encoding */
                              bdd_ptr bdd,      /* the formula */
                              NodeList_ptr vars /* variables */
                              )
{
  /* the result of this computation */
  bdd_ptr res_bdd;

  /* DD manager local reference */
  DDMgr_ptr dd = BddEnc_get_dd_manager(self);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  /* get binary essentials, 0 and 1 need no further processing */
  bdd_ptr work_bdd = bdd_compute_essentials(dd, bdd);

  bdd_ptr work_cube;
  add_ptr work_cube_add;
  ListIter_ptr i;

  if (bdd_is_false(dd, work_bdd) ||
      bdd_is_true(dd, work_bdd)) {

    /* res bdd is referenced */
    res_bdd = work_bdd;
    goto leave;
  }

  /* reference res bdd */
  res_bdd = bdd_true(dd);

  work_cube = bdd_support(dd, work_bdd);
  work_cube_add = bdd_to_add(dd, work_cube);

  /* iterate over variables in binary essentials BDD */

  NODE_LIST_FOREACH(vars, i) {
    node_ptr v = (node_ptr) NodeList_get_elem_at(vars, i);

    bdd_ptr ass_bdd;
    NodeList_ptr singleton;
    node_ptr ass_list;
    bdd_ptr ass_cube;

    /* skip vars that do _not_ appear in the binary essentials bdd */
    if (!BddEnc_is_var_in_cube(self, v, work_cube_add)) continue;

    /* common variable support for both boolean and scalars  */
    singleton = NodeList_create();
    NodeList_append(singleton, v);
    ass_list = BddEnc_assign_symbols(self, work_bdd, singleton,
                                     true, &ass_bdd);

    ass_cube = bdd_support(dd, ass_bdd);

    if(bdd_entailed(dd, work_cube, ass_cube)) {
      bdd_ptr tmp_bdd;
      bdd_and_accumulate(dd, &res_bdd, ass_bdd);

      /* recalculate essentials cube and its add representation */
      tmp_bdd = bdd_forsome(dd, work_bdd, ass_cube );

      bdd_free(dd, work_bdd);
      work_bdd = bdd_dup(tmp_bdd);

      bdd_free(dd, work_cube);
      work_cube = bdd_support(dd, work_bdd);

      add_free(dd, work_cube_add);
      work_cube_add = bdd_to_add(dd, work_cube);

      /* cleanup */
      bdd_free(dd, tmp_bdd);
    }

    /* cleanup */
    NodeList_destroy(singleton);
    free_list(nodemgr, ass_list);

    bdd_free(dd, ass_bdd);
    bdd_free(dd, ass_cube);

    /* if work_bdd is one there's no need to
       iterate over remaining variables */
    if (bdd_is_true(dd, work_bdd)) break;
  }

  /* cleanup */
  add_free(dd, work_cube_add);
  bdd_free(dd, work_cube);
  bdd_free(dd, work_bdd);

 leave:
  return res_bdd;
}

/*!
  \brief Preprocesses variables list, as part of the
  bdd_enc_bdd_to_wff implementation.

  This function is used to preprocess variables list to
  provide to bddenc_print_wff_bdd. As the algorithm implemented in the
  latter does not support word variables, word variables (if any)
  shall be substituted with their bit variables
  representatives. Moreover, this function takes care of adding NEXT
  variables for state variables. For this reason no NEXT nor BITS are
  allowed as input to this function.

  The result NodeList must be destroyed by the caller.

  \se none

  \sa BddEnc_bdd_to_wff
*/
static NodeList_ptr
bdd_enc_get_preprocessed_vars(
                              BddEnc_ptr self,  /* current encoding */
                              NodeList_ptr vars /* variables to be preprocessed */
                              )
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  NodeList_ptr res = NodeList_create();

  /* Symbol table local reference */
  SymbTable_ptr st =
    BaseEnc_get_symb_table(BASE_ENC(self));

  /* Boolean encoding client */
  BoolEnc_ptr benc =
    BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(self));

  /* iterate over variables list */
  ListIter_ptr i;
  NODE_LIST_FOREACH(vars, i) {
    node_ptr v = (node_ptr) NodeList_get_elem_at(vars, i);

    /* no next variables allowed here */
    boolean is_next = (node_get_type(v) == NEXT);
    boolean is_enc;
    boolean is_word;

    if (is_next) {
      char *var_name = sprint_node(wffprint, v);
      ErrorMgr_internal_error(errmgr, "No NEXT variables allowed here (got %s)", var_name);
      FREE(var_name);
    }

    /* no encoding bits allowed here */
    is_enc = BoolEnc_is_var_bit(benc, v);
    if (is_enc) {
      char *var_name = sprint_node(wffprint, v);
      ErrorMgr_internal_error(errmgr, "No bit encoding variables allowed here (got %s)", var_name);
      FREE(var_name);
    }

    /* ok, at this point we're sure there's no NEXTs nor encoding
     * BITs. Substitute words with their bit encoding variables. Add
     * NEXTs if var is a state variable.
     */
    is_word = SymbType_is_word(SymbTable_get_var_type(st, v));
    if (is_word) {
      ListIter_ptr j;
      NodeList_ptr bits = BoolEnc_get_var_bits(benc, v);
      NODE_LIST_FOREACH( bits,  j) {
        node_ptr b = (node_ptr) NodeList_get_elem_at(bits, j);

        NodeList_append(res, b);
        if (SymbTable_is_symbol_state_var(st, b)) {
          NodeList_append(
                          res,
                          find_node(nodemgr,
                                    NEXT,
                                    b,
                                    Nil)
                          );
        }
      } NodeList_destroy(bits);
    }

    /* not a word, add variable to res list. Add NEXT if var is a state variable. */
    else {
      NodeList_append(res, v);
      if (SymbTable_is_symbol_state_var(st, v)) {
        NodeList_append(
                        res,
                        find_node(nodemgr,
                                  NEXT,
                                  v,
                                  Nil)
                        );
      }
    }
  }

  return res;
}

#ifdef PRINT_BDD_DEBUG
/* [MP] define PRINT_BDD_DEBUG_MASKS to apply masks if necessary. */
/* Failing to do it will result in the following assertion */
/* to fail if masks were applied to the original bdd to be printed out. */
/* However, applying masks to test the printout of an unmasked bdd */
/* will cause the test assertion to fail as well. */

/*!
  \brief Debug code for BddEnc_bdd_to_wff

  \todo Missing description

  \se Halts NuSMV if the expression is not a correct
  representation of bdd.

  \sa BddEnc_bdd_to_wff
*/

static void
bdd_enc_debug_bdd_to_wff (BddEnc_ptr self, bdd_ptr bdd, node_ptr expr)
{
  /* DD manager local reference */
  DDMgr_ptr dd = BddEnc_get_dd_manager(self);

  bdd_ptr test_bdd = BddEnc_expr_to_bdd(
                                        self,
                                        expr,
                                        Nil);

#ifndef PRINT_BDD_DEBUG_MASKS
  nusmv_assert(bdd == test_bdd);

#else
  /* apply masks */
  bdd_ptr one_bdd = bdd_true(dd);

  bdd_ptr masked_bdd =
    BddEnc_apply_state_frozen_input_vars_mask_bdd(
                                                  self,
                                                  test_bdd);

  bdd_ptr m2_bdd =
    BddEnc_state_var_to_next_state_var(
                                       self,
                                       BddEnc_apply_state_frozen_vars_mask_bdd(
                                                                               self,
                                                                               one_bdd));
  bdd_and_accumulate(dd, &masked_bdd, m2_bdd);
  nusmv_assert( bdd == masked_bdd );

  bdd_free(dd, one_bdd);
  bdd_free(dd, masked_bdd);
  bdd_free(dd, m2_bdd);
#endif

  bdd_free(dd, test_bdd);
}
#endif

/*!
  \brief Recursively build a sexp representing a formula encoded as
  a BDD

  This function accepts a list of variables as part of
its inputs.The present algorithm assumes that a variable in vars list
is a boolean only in two distinct cases:

1. Pure booleans
2. Boolean

variables belonging to words (i.e. Boolean variables belonging to a
scalar encoding are _not_ allowed in the input list of this
function. This would invariably cause this implementation to
fail). This assumptions are enforced by its public top-level caller.

  \se none

  \sa BddEnc_bdd_to_wff
*/
static node_ptr
bdd_enc_bdd_to_wff_rec(
                       BddEnc_ptr self,   /* The encoding manager */
                       NodeList_ptr vars, /* Variables list (see below) */
                       bdd_ptr bdd,       /* the BDD to be represented*/
                       hash_ptr cache     /* memoization hashtable for DAG traversal */
                       )


{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  /* DD manager local reference */
  DDMgr_ptr dd = BddEnc_get_dd_manager(self);

  /*
   * BASE: if there is a memoized result or input bdd is either zero or one
   * return an expr accordingly, no need to do any further computation.
   * ------------------------------------------------------------------------------
   */
  node_ptr lookup = find_assoc(cache, (node_ptr) bdd);
  if (Nil != lookup) {
    return lookup;
  }

  if (bdd_is_true(dd, bdd)) {
    return ExprMgr_true(exprs);
  }

  if (bdd_is_false(dd, bdd)) {
    return ExprMgr_false(exprs);
  }

  /*
   * STEP: recursively build a sexp representing bdd.
   * ------------------------------------------------------------------------------
   */
  {
    /* Symbol table local reference */
    SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(self));

    /* Boolean encoding client */
    BoolEnc_ptr benc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(self));

    node_ptr res = ExprMgr_true(exprs);

    /* a local copy in order not to make
     * side effects on input bdd.
     **/
    bdd_ptr local_bdd = bdd_dup(bdd);

    /* extract scalar essentials (may be zero or one) */
    bdd_ptr essentials = bdd_enc_get_scalar_essentials(self, local_bdd, vars);
    if (!bdd_is_false(dd, essentials) && !bdd_is_true(dd, essentials)) {

      node_ptr avars = BddEnc_assign_symbols(self, essentials, vars, true, NULL);

      /* build the expression with scalar and booleans assignments */
      node_ptr p;
      for (p = avars; Nil != p; p = cdr(p)) {
        node_ptr v = car(p);

        /* the actual variable, its value and name without next */
        node_ptr var_expr = car(v);
        boolean is_next = (node_get_type(var_expr) == NEXT);
        node_ptr var_value = cdr(v);
        node_ptr var_name = (is_next)
          ? car(var_expr)
          : var_expr;

        /* variables are _not_ necessarily bits here... */
        boolean is_bit = SymbTable_is_symbol_bool_var(st, var_name);

        if (is_bit) {
          boolean is_enc = BoolEnc_is_var_bit(benc, var_name);

          /* (is_bit) && (is_enc) -> it's a wordbit */
          if (is_bit && is_enc) {

            node_ptr x = BoolEnc_get_scalar_var_from_bit(benc, var_name);
            SymbType_ptr x_type = SymbTable_get_var_type(st, x);
            nusmv_assert (SymbType_is_word(x_type));

            var_name = WORD_BIT(var_name);

            /* propagate the variable rewrite handling NEXT */
            var_expr = (!is_next)
              ? var_name
              : find_node(nodemgr, NEXT, var_name, Nil);
          }
        }

        res = ExprMgr_and(exprs,
                          res,

                          (is_bit)

                          ? /* boolean case */
                          ExprMgr_is_true (exprs, var_value) ? var_expr : ExprMgr_not(exprs, var_expr)

                          : /* scalar case */
                          ExprMgr_equal(exprs, var_expr,
                                        var_value,
                                        st));


        /* quantify out the essentials variables from the bdd */
        {
          bdd_ptr cube = bdd_support(dd, essentials);
          bdd_ptr tmp = bdd_forsome(dd, local_bdd, cube);

          /* cleanup */
          bdd_free(dd, cube);
          bdd_free(dd, local_bdd);

          local_bdd = tmp;
        }
      } /* if (!bdd_is_false(dd, essentials) && !bdd_is_true(dd, essentials)) */

      /* at this point the bdd may be 1, in this case no further
         computation is required. */
      if (bdd_is_true(dd, local_bdd)) {
        goto leave;
      }
    }

    {
      /* cofactorization */
      node_ptr cofact_expr = Nil;

      int root_ndx = bdd_index(dd, local_bdd);
      node_ptr root_node = BddEnc_get_var_name_from_index (self, root_ndx);

      /* to determine the type of the the variable we need to take care of
       * the intermediate NEXT node, descending on the left branch.
       */
      boolean is_next = node_get_type(root_node) == NEXT;
      node_ptr var_name = (is_next)
          ? car(root_node)
          : root_node;

      /* determine whether this bit is either:
       * 1. a pure boolean variable
       * 2. a bit belonging to a word
       * 3. a bit belonging to a scalar
       */
      boolean is_enc_bit = BoolEnc_is_var_bit(benc, var_name);
      boolean is_pure_bit= (!is_enc_bit);
      boolean is_word_bit = false;

      if (is_enc_bit) {
        node_ptr x = BoolEnc_get_scalar_var_from_bit(benc, var_name);
        SymbType_ptr x_type = SymbTable_get_var_type(st, x);

        /* word bits need some special handling */
        if (SymbType_is_word(x_type)) {
          is_word_bit = true;
          var_name = WORD_BIT(var_name);
        }
      }

      /* boolean case (pure booleans and word elements) */
      if ((is_pure_bit) || (is_word_bit)) {

        node_ptr var_expr = (!is_next)
            ? var_name
            : find_node(nodemgr, NEXT, var_name, Nil);

        bdd_ptr t_bdd = bdd_then(dd, local_bdd);
        bdd_ptr e_bdd = bdd_else(dd, local_bdd);

        /* BDD normalization */
        if (bdd_iscomplement(dd, local_bdd)) {
          t_bdd = bdd_not(dd, t_bdd);
          e_bdd = bdd_not(dd, e_bdd);
        }

        else {
          /* then and else branches need one more ref */
          bdd_ref(t_bdd);
          bdd_ref(e_bdd);
        }

        /* cofactorization is simple in the boolean case */
        cofact_expr = ExprMgr_ite(exprs,
                                  var_expr,
                                  bdd_enc_bdd_to_wff_rec(self, vars, t_bdd, cache),
                                  bdd_enc_bdd_to_wff_rec(self, vars, e_bdd, cache),
                                  st);

        /* cleanup */
        bdd_free(dd, t_bdd);
        bdd_free(dd, e_bdd);
      }

      /* scalar case */
      else {
        node_ptr var_expr;

        add_ptr root_add;
        add_ptr tmp_cube;
        bdd_ptr root_cube;

        SymbType_ptr root_type;
        node_ptr range;

        /* from now on var_name will contain the scalar name */
        var_name = BoolEnc_get_scalar_var_from_bit(benc, var_name);

        var_expr = (!is_next)
            ? var_name
            : find_node(nodemgr, NEXT, var_name, Nil);

        root_add = BddEnc_expr_to_add(self, var_expr, Nil);
        tmp_cube = add_support(dd, root_add);
        root_cube = add_to_bdd(dd, tmp_cube);

        /* cleanup */
        add_free(dd, root_add);
        add_free(dd, tmp_cube);

        root_type = SymbTable_get_var_type(st, var_name);
        range = SymbType_get_enum_type_values(root_type);

        /* base condition */
        cofact_expr = ExprMgr_false(exprs);

        {
          node_ptr p;
          for (p = range; Nil != p; p = cdr(p)) {
            node_ptr x = car(p);

            /* iterate over all possible values in the range of the scalar variable */
            Expr_ptr clause = ExprMgr_equal(exprs, var_expr, x, st);

            /* Existentially quantify root cube out of (phi & (root_scalar = x)) */
            bdd_ptr clause_bdd = BddEnc_expr_to_bdd(self, clause, Nil);
            bdd_ptr tmp_bdd = bdd_and_abstract(dd, local_bdd, clause_bdd, root_cube);

            if (bdd_isnot_false(dd, tmp_bdd)) {
              cofact_expr = ExprMgr_ite(exprs, clause,
                                        bdd_enc_bdd_to_wff_rec(self,
                                                               vars,
                                                               tmp_bdd,
                                                               cache),
                                        cofact_expr,
                                        st);
            }

            /* cleanup */
            bdd_free(dd, clause_bdd);
            bdd_free(dd, tmp_bdd);
          }
        }

        add_free(dd, root_cube);
      }

      /* cofactorization */
      res = ExprMgr_and(exprs, res, cofact_expr);
    }

  leave:
    bdd_free(dd, local_bdd);
    bdd_free(dd, essentials);

    /* memoize the result and leave */
    bdd_ref(bdd);
    insert_assoc( cache, (node_ptr) bdd, res );

    return res;
  }
}
