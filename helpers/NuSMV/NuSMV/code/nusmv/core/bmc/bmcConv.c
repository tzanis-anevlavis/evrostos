/* ---------------------------------------------------------------------------


  This file is part of the ``bmc'' package of NuSMV version 2.
  Copyright (C) 2000-2001 by FBK-irst and University of Trento.
  Copyright (C) 2011 by FBK.

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
  \author Alessandro Cimatti, Lorenzo Delana, Michele Dorigatti
  \brief Convertion function of BE to corresponding BDD boolean
               expression, and viceversa

  \todo: Missing description

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"

#include "nusmv/core/wff/wff.h"
#include "nusmv/core/wff/w2w/w2w.h"

#include "nusmv/core/bmc/bmcInt.h"
#include "nusmv/core/bmc/bmcConv.h"

#include "nusmv/core/parser/symbols.h" /* for NUMBER et similia */
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/rbc/rbc.h"

#include "nusmv/core/utils/array.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief Be2bexpDfsData

  The data structure used for DFS traversal of RBC
*/

typedef struct Be2bexpDfsData_TAG {
  BeEnc_ptr be_enc;
  array_t* stack;
  int head;
} Be2bexpDfsData;


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

static void bmc_conv_set_cache(hash_ptr hash, node_ptr bexp, be_ptr be);
static be_ptr bmc_conv_query_cache(hash_ptr hash, node_ptr bexp);

static be_ptr
bmc_conv_bexp2be_recur(hash_ptr, BeEnc_ptr be_enc, node_ptr bexp);

/* Primitives for stack used in BE traversal */
static void Be2bexpDfsData_push(Be2bexpDfsData*, node_ptr);
static node_ptr Be2bexpDfsData_head(Be2bexpDfsData*);
static node_ptr Be2bexpDfsData_pop(Be2bexpDfsData*);

/* BE traversal functions */
static int  Be2bexp_Set(be_ptr be, char* Be2bexpData, nusmv_ptrint sign);
static void Be2bexp_First(be_ptr be, char* Be2bexpData, nusmv_ptrint sign);
static void Be2bexp_Back(be_ptr be, char* Be2bexpData, nusmv_ptrint sign);
static void Be2bexp_Last(be_ptr be, char* Be2bexpData, nusmv_ptrint sign);

static hash_ptr bmc_conv_get_handled_hash(SymbTable_ptr symb_table,
                                          char* hash_str);

static be_ptr bmc_apply_time_on_expr(hash_ptr bexpr2be, BeEnc_ptr be_enc,
                                     node_ptr bexp, int time);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

node_ptr Bmc_Conv_Be2Bexp(BeEnc_ptr be_enc, be_ptr be)
{
  RbcDfsFunctions_t Be2bexpFunctions;
  Be2bexpDfsData Be2bexpData;
  node_ptr ret;
  Be_Manager_ptr be_mgr;
  Rbc_t* rbc;
  Rbc_Manager_t* rbc_manager;

  be_mgr = BeEnc_get_be_manager(be_enc);
  rbc_manager = (Rbc_Manager_t*)Be_Manager_GetSpecManager(be_mgr);
  rbc = (Rbc_t*)Be_Manager_Be2Spec(be_mgr, be);

  /* Cleaning the user fields. */
  Rbc_Dfs_clean_exported(rbc, rbc_manager);

  /* Setting up the DFS functions. */
  Be2bexpFunctions.Set        = Be2bexp_Set;
  Be2bexpFunctions.FirstVisit = Be2bexp_First;
  Be2bexpFunctions.BackVisit  = Be2bexp_Back;
  Be2bexpFunctions.LastVisit  = Be2bexp_Last;

  /* Setting up the DFS data. */
  /* :TODO??: optimizations on the quantity of nodes */
  Be2bexpData.be_enc = be_enc;
  Be2bexpData.stack = array_alloc(node_ptr, 10);
  Be2bexpData.head = -1;

  /* Calling DFS on f. */
  Rbc_Dfs_exported(rbc, &Be2bexpFunctions, (void*)(&Be2bexpData), rbc_manager);
  ret = Be2bexpDfsData_head(&Be2bexpData);

  array_free(Be2bexpData.stack);

  return ret;
}

be_ptr Bmc_Conv_Bexp2Be(BeEnc_ptr be_enc, node_ptr bexp)
{
  SymbTable_ptr symb_table = BaseEnc_get_symb_table(BASE_ENC(be_enc));

  hash_ptr bexpr2be =
    bmc_conv_get_handled_hash(symb_table, ST_BMC_CONV_BEXPR2BE_HASH);

  return bmc_conv_bexp2be_recur(bexpr2be, be_enc, bexp);
}

node_ptr Bmc_Conv_BexpList2BeList(BeEnc_ptr be_enc, node_ptr bexp_list)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  if (bexp_list == Nil)
  {
    return(Nil);
  }
  else
  {
    return cons(nodemgr,  (node_ptr)Bmc_Conv_Bexp2Be(be_enc, car(bexp_list)),
                 Bmc_Conv_BexpList2BeList(be_enc, cdr(bexp_list)) );
  }
}

void Bmc_Conv_get_BeModel2SymbModel(const BeEnc_ptr be_enc,
                                    const Slist_ptr be_model,
                                    int k,
                                    boolean convert_to_scalars,
                                    node_ptr* frozen,
                                    array_t** states,
                                    array_t** inputs)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  Be_Manager_ptr be_mgr = BeEnc_get_be_manager(be_enc);
  SymbTable_ptr symb_table = BaseEnc_get_symb_table(BASE_ENC(be_enc));

  Siter genLit;
  nusmv_ptrint lit;
  int iter;

  *frozen = Nil;
  *states = array_alloc(node_ptr, k+1);
  *inputs = array_alloc(node_ptr, k+1);

  for (iter=0; iter <= k; ++iter) {
    array_insert(node_ptr, *states, iter, Nil);
    array_insert(node_ptr, *inputs, iter, Nil);
  }


  /* ---- scan list of literals, convert them to bits and add to
          corresponding result expression ---- */
  SLIST_FOREACH(be_model, genLit) {
    int var_idx, ut_index, time;
    node_ptr var_expr;

    lit = (nusmv_ptrint) Siter_element(genLit);

    var_idx = Be_BeLiteral2BeIndex(be_mgr, lit);
    ut_index = BeEnc_index_to_untimed_index(be_enc, var_idx);
    time = BeEnc_index_to_time(be_enc, var_idx);

    var_expr = BeEnc_index_to_name(be_enc, ut_index);
    if (lit < 0) var_expr = ExprMgr_not(exprs, var_expr);    /* negate if needed: */

    if (BeEnc_is_index_untimed_curr(be_enc, ut_index)) {
      /* state var can be only timed and in proper range */
      node_ptr expr;

      nusmv_assert(!BeEnc_is_index_untimed(be_enc, var_idx));
      nusmv_assert(0 <= time && time <= k);

      expr = array_fetch(node_ptr, *states, time);
      array_insert(node_ptr, *states, time,
                   new_node(nodemgr, AND, var_expr, expr));
    }
    else if (BeEnc_is_index_untimed_input(be_enc, ut_index)) {
      /* input var can be only timed and in proper range */
      node_ptr expr;

      nusmv_assert(!BeEnc_is_index_untimed(be_enc, var_idx));
      nusmv_assert(0 <= time && time <= k);

      expr = array_fetch(node_ptr, *inputs, time);
      array_insert(node_ptr, *inputs, time,
                   new_node(nodemgr, AND, var_expr, expr));
    }
    else {
      /* frozen vars are always untimed */
      nusmv_assert(BeEnc_is_index_untimed_frozen(be_enc, var_idx));
      nusmv_assert(time == BE_CURRENT_UNTIMED);

      *frozen = new_node(nodemgr, AND, var_expr, *frozen);
    }
  } /* end of literal scan */

  /* if conversion to scalars is not required then job is done */
  if (!convert_to_scalars) return;

  { /* ------ convert to scalar all the collected bits -------- */
    /* This is done the following way:
       for every expression list a hash table is created mapping
       a var to value of its bits. After the list is processed
       a new list is created with var=const expressions.
       NOTE: that some unassigned bits may become assigned in this process
    */

    /* to simplify iteration create a global set of all expressions */
    BoolEnc_ptr bool_enc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(be_enc));

    int all_array_iter;
    array_t* all_arrays[3];
    all_arrays[0] = *states; 
    all_arrays[1] = *inputs;
    all_arrays[2] = array_alloc(node_ptr, 1);
    array_insert(node_ptr, all_arrays[2], 0, *frozen);

    /* -- iterate over all arrays */
    for(all_array_iter = 0; all_array_iter < 3; ++all_array_iter) {
      array_t* array = all_arrays[all_array_iter];
      int array_iter;
      node_ptr expr_list;
      /* -- iterate over given arrays, i.e. time steps */
      arrayForEachItem(node_ptr, array, array_iter, expr_list) {
        node_ptr new_expr_list = Nil; /* a scalar list instead of bits expr_list */
        node_ptr expr_iter;
        hash_ptr var2BitValues = new_assoc();

        /* -- iterate over a list of expressions */
        for (expr_iter = expr_list; Nil != expr_iter; expr_iter = cdr(expr_iter)) {
          node_ptr expr = car(expr_iter);
          boolean isNegation = node_get_type(expr) == NOT;
          node_ptr bit = isNegation ? car(expr) : expr;
          node_ptr var = BoolEnc_is_var_bit(bool_enc, bit) ?
            BoolEnc_get_scalar_var_from_bit(bool_enc, bit) : bit;
          int index;
          BitValues_ptr bitValues;

          /* we manage to get a proper scalar var */
          nusmv_assert(SymbTable_is_symbol_var(symb_table, var));

          /* if this is a boolean var => insert directly into the new list */
          if (var == bit) {
            expr = find_node(nodemgr, EQUAL, var,
                             isNegation ? ExprMgr_false(exprs) : ExprMgr_true(exprs));
            new_expr_list = new_node(nodemgr, AND, expr, new_expr_list);
            continue;
          }
          /* otherwise this is a proper bit of scalar var */
          nusmv_assert(bit != var);

          index = BoolEnc_get_index_from_bit(bool_enc, bit);

          /* get/create BitValues for a given var */
          bitValues = BIT_VALUES(find_assoc(var2BitValues, var));
          if (NULL == bitValues) {
            bitValues = BitValues_create(bool_enc, var);
            insert_assoc(var2BitValues, var, NODE_PTR(bitValues));
          }

          /* set the bit value */
          BitValues_set(bitValues, index,
                        isNegation ? BIT_VALUE_FALSE : BIT_VALUE_TRUE);
        } /* all expressions in the list has been processed */

        { /* iterate over elements of the hash table (i.e. scalar
             vars) and add to the new list "var=value" */
          assoc_iter aiter;
          node_ptr var;
          BitValues_ptr bitValues;

          ASSOC_FOREACH(var2BitValues, aiter, &var, &bitValues) {
            node_ptr value = BoolEnc_get_value_from_var_bits(bool_enc,
                                                             bitValues);

            new_expr_list = new_node(nodemgr, AND,
                                     new_node(nodemgr, EQUAL, var, value),
                                     new_expr_list);

            BitValues_destroy(bitValues);
          }
        }
        free_assoc(var2BitValues);

        /* free previous list of expression and insert the new one */
        free_list(nodemgr, expr_list);
        array_insert(node_ptr, array, array_iter, new_expr_list);
      } /* end of one array iteration */
    } /* end of all arrays iteration */

    /* get the frozen expression back */
    *frozen = array_fetch(node_ptr, all_arrays[2], 0);
    array_free(all_arrays[2]);
  }

}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Sets a node into the stack

  

  \se None
*/
static void Be2bexpDfsData_push(Be2bexpDfsData* data,
                                node_ptr value)
{
  (data -> head) ++;

  array_insert(node_ptr, data -> stack, data -> head, value);
}

/*!
  \brief Be2bexpDfsData_head

  

  \se None
*/
static node_ptr Be2bexpDfsData_head(Be2bexpDfsData* data)
{
  /* there is space available into the stack: */
  nusmv_assert((data -> head) != (-1));

  return (node_ptr) array_fetch(node_ptr, data->stack, data->head);
}

/*!
  \brief Be2bexpDfsData_pop

  

  \se None
*/
static node_ptr Be2bexpDfsData_pop(Be2bexpDfsData* data)
{
  node_ptr value = (node_ptr)Be2bexpDfsData_head(data);

  (data->head) --;

  return(value);
}

/*!
  \brief Be2bexpSet

  

  \se None
*/
static int Be2bexp_Set(be_ptr be, char* Be2bexpData, nusmv_ptrint sign)
{
  return -1;
}

/*!
  \brief Be2bexpFirst

  

  \se None
*/
static void Be2bexp_First(be_ptr be, char* Be2bexpData, nusmv_ptrint sign)
{
  return;
}

/*!
  \brief Be2bexp_Back

  

  \se None
*/
static void Be2bexp_Back(be_ptr be, char* Be2bexpData, nusmv_ptrint sign)
{
  return;
}

/*!
  \brief Be2bexp_Last

  

  \se None
*/
static void Be2bexp_Last(be_ptr be, char* Be2bexpData, nusmv_ptrint sign)
{
  int identifier = 0;
  int time, var_id;
  node_ptr left, right;
  Be_Manager_ptr be_mgr;
  Rbc_t* rbc;

  BeEnc_ptr be_enc = ((Be2bexpDfsData*)Be2bexpData)->be_enc;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  nusmv_assert(be_enc != NULL);

  be_mgr = BeEnc_get_be_manager(be_enc);
  rbc = (Rbc_t*)Be_Manager_Be2Spec(be_mgr, be);

  if (Rbc_is_top(rbc)) {
    if (sign == RBC_FALSE) {
      Be2bexpDfsData_push((Be2bexpDfsData*)Be2bexpData, Wff_make_falsity(nodemgr));
    }
    else {
      Be2bexpDfsData_push((Be2bexpDfsData*)Be2bexpData, Wff_make_truth(nodemgr));
    }
  }

  else if (Rbc_is_var(rbc)) {
    /* substitute the variable index, in the stack, with its correspondent
       state or frozen variable */

    time = BeEnc_index_to_time(be_enc, BeEnc_var_to_index(be_enc, rbc));
    /* frozen var has time BE_CURRENT_UNTIMED => make it 0 to avoid adding NEXTs */
    if (BE_CURRENT_UNTIMED == time) time = 0;
    var_id = BeEnc_index_to_untimed_index(be_enc,
                                          BeEnc_var_to_index(be_enc, rbc));

    if (sign == RBC_FALSE) {
      Be2bexpDfsData_push( (Be2bexpDfsData*) Be2bexpData,
        Wff_make_not(nodemgr, Wff_make_opnext_times(nodemgr, BeEnc_index_to_name(be_enc, var_id),
                                         time)) );
    }
    else {
      Be2bexpDfsData_push( (Be2bexpDfsData*) Be2bexpData,
           Wff_make_opnext_times(nodemgr, BeEnc_index_to_name(be_enc, var_id), time) );
    }
  }

  else if ((Rbc_is_and(rbc)) || (Rbc_is_iff(rbc))) {
    /* get the left bexp from the stack */
    right = Be2bexpDfsData_pop((Be2bexpDfsData*) Be2bexpData);

    /* get the right bexp from the stack */
    left = Be2bexpDfsData_pop((Be2bexpDfsData*) Be2bexpData);

    if (Rbc_is_and(rbc)) identifier = AND;
    else if (Rbc_is_iff(rbc)) identifier = IFF;

    if (sign == RBC_FALSE) {
      Be2bexpDfsData_push( (Be2bexpDfsData*) Be2bexpData,
                           Wff_make_not(nodemgr, find_node(nodemgr, identifier, left, right)) );
    }
    else {
      Be2bexpDfsData_push( (Be2bexpDfsData*) Be2bexpData,
                            find_node(nodemgr, identifier, left, right) );
    }
  }

  else {
    /* execution should never be here: */
    ErrorMgr_internal_error(errmgr, "rbc had an invalid value.\n");
  }
}

/*!
  \brief Update the bexpr -> be cache

  
*/
static void bmc_conv_set_cache(hash_ptr bexpr2be, node_ptr bexp, be_ptr be)
{
  nusmv_assert(bexpr2be != (hash_ptr) NULL);
  insert_assoc(bexpr2be, bexp, (node_ptr) be);
}

/*!
  \brief Queries the bexpr->be cache

  Return NULL if association not found
*/
static be_ptr bmc_conv_query_cache(hash_ptr bexpr2be, node_ptr bexp)
{
  nusmv_assert(bexpr2be != (hash_ptr) NULL);
  return (be_ptr) find_assoc(bexpr2be, bexp);
}

/*!
  \brief Private service for bmc_conv_bexp2be_recur for handling 
   attime expression 
   
   Return an expression in BE format where for each
   untimed variable is replaced by the
   corresponding BE variable at the given 'time'

  \sa 
*/
static be_ptr bmc_apply_time_on_expr(hash_ptr bexpr2be, BeEnc_ptr be_enc,
                                       node_ptr bexp, int time)
{
 const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  be_ptr result = (be_ptr) NULL;

  /* if given expression is Nil, returns truth be */
  /* Nil value can be used in AND sequences, so a true value must
     be returned */
  if (bexp == Nil) return Be_Truth(BeEnc_get_be_manager(be_enc));

  switch (node_get_type(bexp)) {
  case TRUEEXP:
    result = Be_Truth(BeEnc_get_be_manager(be_enc));
    break;

  case FALSEEXP:
    result = Be_Falsity(BeEnc_get_be_manager(be_enc));
    break;

  case NEXT:
    {
      SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(be_enc));

      nusmv_assert(SymbTable_is_symbol_state_frozen_var(st, car(bexp)));

      if (SymbTable_is_symbol_frozen_var(st, car(bexp))) {
        result = BeEnc_name_to_untimed(be_enc, car(bexp));
      }
      else {
        result = BeEnc_name_to_untimed(be_enc, bexp);
      }
    }
    break;

  case NOT:
    /* NOT arg is converted into an be, and its negation is result =ed */
    result = Be_Not( BeEnc_get_be_manager(be_enc),
                     bmc_apply_time_on_expr(bexpr2be, be_enc, car(bexp), time) );
    break;

  case CONS:
  case AND:
    result = Be_And( BeEnc_get_be_manager(be_enc),
		     bmc_apply_time_on_expr(bexpr2be, be_enc, car(bexp), time),
		     bmc_apply_time_on_expr(bexpr2be, be_enc, cdr(bexp), time) );
    break;

  case OR:
    result = Be_Or( BeEnc_get_be_manager(be_enc),
		    bmc_apply_time_on_expr(bexpr2be, be_enc, car(bexp), time),
		    bmc_apply_time_on_expr(bexpr2be, be_enc, cdr(bexp), time) );
    break;

  case XOR:
    result = Be_Xor( BeEnc_get_be_manager(be_enc),
		     bmc_apply_time_on_expr(bexpr2be, be_enc, car(bexp), time),
		     bmc_apply_time_on_expr(bexpr2be, be_enc, cdr(bexp), time) );
    break;

  case XNOR:
    result =
      Be_Not( BeEnc_get_be_manager(be_enc),
              Be_Xor( BeEnc_get_be_manager(be_enc),
                      bmc_apply_time_on_expr(bexpr2be, be_enc, car(bexp), time),
                      bmc_apply_time_on_expr(bexpr2be, be_enc, cdr(bexp), time) ));
    break;

  case IFF: 
    result = Be_Iff( BeEnc_get_be_manager(be_enc),
		     bmc_apply_time_on_expr(bexpr2be, be_enc, car(bexp), time),
		     bmc_apply_time_on_expr(bexpr2be, be_enc, cdr(bexp), time) );
    break;

  case IMPLIES:
    result = Be_Implies( BeEnc_get_be_manager(be_enc),
			 bmc_apply_time_on_expr(bexpr2be, be_enc, car(bexp), time),
			 bmc_apply_time_on_expr(bexpr2be, be_enc, cdr(bexp), time) );
    break;

  case EQUAL:
  case ASSIGN:
    result = Be_Iff( BeEnc_get_be_manager(be_enc),
		     bmc_apply_time_on_expr(bexpr2be, be_enc, car(bexp), time),
		     bmc_apply_time_on_expr(bexpr2be, be_enc, cdr(bexp), time) );
    break;

  case IFTHENELSE:
  case CASE: {
    /* converts "if"-"then"-"else" args of CASE into three BEs, and result =
       a BE If-Then-Else with converted BEs as childs */

    /* lazy evaluation and simplification is used here to get rid of
       FAILURE node in case-expressions
    */

    be_ptr cond = bmc_apply_time_on_expr(bexpr2be, be_enc, caar(bexp), time);
    if (cond == Be_Truth(BeEnc_get_be_manager(be_enc))) {
      result = bmc_apply_time_on_expr(bexpr2be, be_enc, cdar(bexp), time);
    }
    else if (cond == Be_Falsity(BeEnc_get_be_manager(be_enc))) {
      result = bmc_apply_time_on_expr(bexpr2be, be_enc, cdr(bexp), time);
    }
    /* no simplification possible since the condition is not a constant */
    else result = Be_Ite( BeEnc_get_be_manager(be_enc),
                          cond,
                          bmc_apply_time_on_expr(bexpr2be, be_enc, cdar(bexp), time),
                          bmc_apply_time_on_expr(bexpr2be, be_enc, cdr(bexp), time));
    break;
  }

  case BIT:                              /* Variable */
  case DOT:                              /* Variable */
    result = BeEnc_name_to_timed(be_enc, bexp, time);
    break;

  case ARRAY:
    /* Must be a boolean variable: */
    nusmv_assert(SymbTable_is_symbol_bool_var(
                      BaseEnc_get_symb_table(BASE_ENC(be_enc)),
                      bexp));

    result = BeEnc_name_to_timed(be_enc, bexp, time);
    break;

  case ATOM:                             /* Variable */
    ErrorMgr_internal_error(errmgr, "Not DOT node as variable has been found!\n");

  case UNSIGNED_WORD:
    ErrorMgr_internal_error(errmgr, "Words cannot be met in boolean expressions!\n");

  case FAILURE:
    {
      if (ErrorMgr_failure_get_kind(errmgr, bexp) == FAILURE_CASE_NOT_EXHAUSTIVE) {
        ErrorMgr_warning_case_not_exhaustive(errmgr, bexp);
        /* forces a default */
        result = Be_Truth(BeEnc_get_be_manager(be_enc));
        break;
      }
      else if (ErrorMgr_failure_get_kind(errmgr, bexp) == FAILURE_DIV_BY_ZERO) {
        ErrorMgr_warning_possible_div_by_zero(errmgr, bexp);
        /* forces a default */
        result = Be_Truth(BeEnc_get_be_manager(be_enc));
        break;
      }
      else if (ErrorMgr_failure_get_kind(errmgr, bexp) == FAILURE_ARRAY_OUT_OF_BOUNDS) {
        ErrorMgr_warning_possible_array_out_of_bounds(errmgr, bexp);
        /* forces a default */
        result = Be_Truth(BeEnc_get_be_manager(be_enc));
        break;
      }
      else {
        ErrorMgr_report_failure_node(errmgr, bexp); /* some error in the input expr */
      }
    }

  case NUMBER:
    /* zero and one are casted to their respective truth
       values. Other values fall down to an error */
    if (node_get_int(bexp) == 0) {
      result = Be_Falsity(BeEnc_get_be_manager(be_enc));
      break;
    }

    if (node_get_int(bexp) == 1) {
      result = Be_Truth(BeEnc_get_be_manager(be_enc));
      break;
    }

    /* no other values are allowed */
    ErrorMgr_internal_error(errmgr, "bmc_apply_time_on_expr: Unexpected number value [%d]\n",
                   node_get_int(bexp));

  default:
    {
      const MasterPrinter_ptr sexpprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_SEXP_PRINTER));
      print_node(sexpprint, stderr, bexp);
      ErrorMgr_internal_error(errmgr, "bmc_apply_time_on_expr: Unexpected case value. Node type = %d\n",
                     node_get_type(bexp));
    }
  }
  return result;
}

/*!
  \brief Private service for Bmc_Conv_Bexp2Be

  Recursive service for Bmc_Conv_Bexp2Be, with caching of
  results

  \sa Bmc_Conv_Bexp2Be
*/
static be_ptr bmc_conv_bexp2be_recur(hash_ptr bexpr2be, BeEnc_ptr be_enc,
                                     node_ptr bexp)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  be_ptr result = (be_ptr) NULL;

  /* if given expression is Nil, returns truth be */
  /* Nil value can be used in AND sequences, so a true value must
     be returned */
  if (bexp == Nil) return Be_Truth(BeEnc_get_be_manager(be_enc));

  /* queries the cache: */
  result = bmc_conv_query_cache(bexpr2be, bexp);
  if (result != (be_ptr) NULL) return result;

  switch (node_get_type(bexp)) {
  case TRUEEXP:
    result = Be_Truth(BeEnc_get_be_manager(be_enc));
    break;

  case FALSEEXP:
    result = Be_Falsity(BeEnc_get_be_manager(be_enc));
    break;

  case NEXT:
    {
      SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(be_enc));

      nusmv_assert(SymbTable_is_symbol_state_frozen_var(st, car(bexp)));

      if (SymbTable_is_symbol_frozen_var(st, car(bexp))) {
        result = BeEnc_name_to_untimed(be_enc, car(bexp));
      }
      else {
        result = BeEnc_name_to_untimed(be_enc, bexp);
      }
    }
    break;

  case NOT:
    /* NOT arg is converted into an be, and its negation is result =ed */
    result = Be_Not( BeEnc_get_be_manager(be_enc),
                     bmc_conv_bexp2be_recur(bexpr2be, be_enc, car(bexp)) );
    break;

  case CONS:
  case AND:
    result = Be_And( BeEnc_get_be_manager(be_enc),
                   bmc_conv_bexp2be_recur(bexpr2be, be_enc, car(bexp)),
                   bmc_conv_bexp2be_recur(bexpr2be, be_enc, cdr(bexp)) );
    break;

  case OR:
    result = Be_Or( BeEnc_get_be_manager(be_enc),
                  bmc_conv_bexp2be_recur(bexpr2be, be_enc, car(bexp)),
                  bmc_conv_bexp2be_recur(bexpr2be, be_enc, cdr(bexp)) );
    break;

  case XOR:
    result = Be_Xor( BeEnc_get_be_manager(be_enc),
                  bmc_conv_bexp2be_recur(bexpr2be, be_enc, car(bexp)),
                  bmc_conv_bexp2be_recur(bexpr2be, be_enc, cdr(bexp)) );
    break;

  case XNOR:
    result =
      Be_Not( BeEnc_get_be_manager(be_enc),
              Be_Xor( BeEnc_get_be_manager(be_enc),
                      bmc_conv_bexp2be_recur(bexpr2be, be_enc, car(bexp)),
                      bmc_conv_bexp2be_recur(bexpr2be, be_enc, cdr(bexp)) ));
    break;

  case IFF:
    /* converts IFF args into two BEs, and result = an IFF BE with converted
       BEs as childs */
    result = Be_Iff( BeEnc_get_be_manager(be_enc),
                   bmc_conv_bexp2be_recur(bexpr2be, be_enc, car(bexp)),
                   bmc_conv_bexp2be_recur(bexpr2be, be_enc, cdr(bexp)) );
    break;

  case IMPLIES:
    /* convert IMPLIES args into two BEs, and result = the IMPLIES BE with
       converted BEs as childs */
    result = Be_Implies( BeEnc_get_be_manager(be_enc),
                       bmc_conv_bexp2be_recur(bexpr2be, be_enc, car(bexp)),
                       bmc_conv_bexp2be_recur(bexpr2be, be_enc, cdr(bexp)) );
    break;

  case EQUAL:
  case ASSIGN:
    /* converts EQUAL and ASSIGN args into two BEs, and result = an IFF BE
       with converted BEs as childs */
    result = Be_Iff( BeEnc_get_be_manager(be_enc),
                   bmc_conv_bexp2be_recur(bexpr2be, be_enc, car(bexp)),
                   bmc_conv_bexp2be_recur(bexpr2be, be_enc, cdr(bexp)) );
    break;

  case IFTHENELSE:
  case CASE: {
    /* converts "if"-"then"-"else" args of CASE into three BEs, and result =
       a BE If-Then-Else with converted BEs as childs */

    /* lazy evaluation and simplification is used here to get rid of
       FAILURE node in case-expressions
    */

    be_ptr cond = bmc_conv_bexp2be_recur(bexpr2be, be_enc, caar(bexp));
    if (cond == Be_Truth(BeEnc_get_be_manager(be_enc))) {
      result = bmc_conv_bexp2be_recur(bexpr2be, be_enc, cdar(bexp));
    }
    else if (cond == Be_Falsity(BeEnc_get_be_manager(be_enc))) {
      result = bmc_conv_bexp2be_recur(bexpr2be, be_enc, cdr(bexp));
    }
    /* no simplification possible since the condition is not a constant */
    else result = Be_Ite( BeEnc_get_be_manager(be_enc),
                          cond,
                          bmc_conv_bexp2be_recur(bexpr2be, be_enc, cdar(bexp)),
                          bmc_conv_bexp2be_recur(bexpr2be, be_enc, cdr(bexp)));
    break;
  }

  case BIT:                              /* Variable */
  case DOT:                              /* Variable */
    result = BeEnc_name_to_untimed(be_enc, bexp);
    break;

  case ARRAY:
    /* Must be a boolean variable: */
    nusmv_assert(SymbTable_is_symbol_bool_var(
                      BaseEnc_get_symb_table(BASE_ENC(be_enc)),
                      bexp));

    result = BeEnc_name_to_untimed(be_enc, bexp);
    break;

  case ATOM:                             /* Variable */
    ErrorMgr_internal_error(errmgr, "Not DOT node as variable has been found!\n");

  case UNSIGNED_WORD:
    ErrorMgr_internal_error(errmgr, "Words cannot be met in boolean expressions!\n");
    /* result = bmc_conv_bexp2be_recur(bexpr2be, be_enc, car(bexp)); */
    /* break; */

  case FAILURE:
    {
      if (ErrorMgr_failure_get_kind(errmgr, bexp) == FAILURE_CASE_NOT_EXHAUSTIVE) {
        ErrorMgr_warning_case_not_exhaustive(errmgr, bexp);
        /* forces a default */
        result = Be_Truth(BeEnc_get_be_manager(be_enc));
        break;
      }
      else if (ErrorMgr_failure_get_kind(errmgr, bexp) == FAILURE_DIV_BY_ZERO) {
        ErrorMgr_warning_possible_div_by_zero(errmgr, bexp);
        /* forces a default */
        result = Be_Truth(BeEnc_get_be_manager(be_enc));
        break;
      }
      else if (ErrorMgr_failure_get_kind(errmgr, bexp) == FAILURE_ARRAY_OUT_OF_BOUNDS) {
        ErrorMgr_warning_possible_array_out_of_bounds(errmgr, bexp);
        /* forces a default */
        result = Be_Truth(BeEnc_get_be_manager(be_enc));
        break;
      }
      else {
        ErrorMgr_report_failure_node(errmgr, bexp); /* some error in the input expr */
      }
    }

  case NUMBER:
    /* zero and one are casted to their respective truth
       values. Other values fall down to an error */
    if (node_get_int(bexp) == 0) {
      result = Be_Falsity(BeEnc_get_be_manager(be_enc));
      break;
    }

    if (node_get_int(bexp) == 1) {
      result = Be_Truth(BeEnc_get_be_manager(be_enc));
      break;
    }

  case ATTIME:
    {
      int time_val = ExprMgr_attime_get_time(exprs, bexp);
      result = bmc_apply_time_on_expr(bexpr2be, be_enc,
                                      car(bexp), time_val);
      break;
    }

    /* no other values are allowed */
    ErrorMgr_internal_error(errmgr, "bmc_conv_bexp2be_recur: Unexpected number value [%d]\n",
                   node_get_int(bexp));

  default:
    {
      const MasterPrinter_ptr sexpprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_SEXP_PRINTER));
      print_node(sexpprint, stderr, bexp);
      ErrorMgr_internal_error(errmgr, "bmc_conv_bexp2be_recur: Unexpected case value. Node type = %d\n",
                     node_get_type(bexp));
    }
  }

  /* updates the cache and returns result*/
  bmc_conv_set_cache(bexpr2be, bexp, result);
  return result;
}

/*!
  \brief Call SymbTable_get_handled_hash_ptr with proper
                       arguments

  
*/
static hash_ptr bmc_conv_get_handled_hash(SymbTable_ptr symb_table,
                                          char* hash_str)
{
  if (! strcmp(ST_BMC_CONV_BEXPR2BE_HASH, hash_str)) {
    return
    SymbTable_get_handled_hash_ptr(symb_table,
                                   ST_BMC_CONV_BEXPR2BE_HASH,
                                   (ST_PFICPCP)NULL,
                                   (ST_PFICPI)NULL,
                                   (ST_PFSR)NULL,
                                   (SymbTableTriggerFun)NULL,
                                   SymbTable_clear_handled_remove_action_hash,
                                   (SymbTableTriggerFun)NULL
                                   );
  }
  else error_unreachable_code();
}
