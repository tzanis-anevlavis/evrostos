/* ---------------------------------------------------------------------------


  This file is part of the ``bmc'' package of NuSMV version 2.
  Copyright (C) 2000-2001 by FBK-irst and University of Trento.

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
  \author Roberto Cavada
  \brief Bmc.Tableau module

  This module contains all the operations related to the
               construction of tableaux for LTL formulas

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/bmc/bmcInt.h"
#include "nusmv/core/bmc/bmcUtils.h"
#include "nusmv/core/bmc/bmcModel.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/assoc.h"
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
static be_ptr
bmc_tableauGetUntilAtTime_aux(const BeEnc_ptr be_enc,
                              const node_ptr p, const node_ptr q,
                              const int time, const int k, const int l,
                              const int steps);

static be_ptr
bmc_tableauGetReleasesAtTime_aux(const BeEnc_ptr be_enc,
                                 const node_ptr p, const node_ptr q,
                                 const int time, const int k, const int l,
                                 const int steps);


/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

be_ptr
BmcInt_Tableau_GetAtTime(const BeEnc_ptr be_enc, const node_ptr ltl_wff,
                         const int time, const int k, const int l)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  be_ptr result = (be_ptr)NULL;
  SymbTable_ptr st;
  Be_Manager_ptr be_mgr;
  node_ptr key;
  hash_ptr tableau_ltl_hash;

  /* checks out the validity of [l, k] only if a loop exists: */
  nusmv_assert(Bmc_Utils_IsNoLoopback(l) || (k > l));
  nusmv_assert((time < k) || (time==k && Bmc_Utils_IsNoLoopback(l)) );

  st = BaseEnc_get_symb_table(BASE_ENC(be_enc));
  be_mgr = BeEnc_get_be_manager(be_enc);

  tableau_ltl_hash = Bmc_Tableau_get_handled_hash(st, ST_BMC_TABLEAU_LTL_HASH);

  key = bmc_tableau_memoization_get_key(nodemgr, ltl_wff, time, k, l);
  result = bmc_tableau_memoization_lookup(tableau_ltl_hash, key);
  if (result != (be_ptr) NULL) return result;

  switch (node_get_type(ltl_wff)) {
  case TRUEEXP:
    return Be_Truth(be_mgr); /* not memoized */

  case FALSEEXP:
    return Be_Falsity(be_mgr); /* not memoized */

  case BIT:
  case DOT:
    if ((time == k) &&
        BeEnc_is_index_input_var(be_enc,
                                 BeEnc_name_to_index(be_enc, ltl_wff))) {
      /* input vars when time == max_time evaluate to false: */
      return Be_Falsity(be_mgr); /* not memoized */
    }

    return BeEnc_name_to_timed(be_enc, ltl_wff, time); /* not memoized */

  case ARRAY:
    if (!SymbTable_is_symbol_declared(st, ltl_wff)) {
      ErrorMgr_internal_error(errmgr, "Unexpected array node\n");
    }

    if (!SymbTable_is_symbol_bool_var(st, ltl_wff)) {
      StreamMgr_print_error(streams,  "Detected scalar array variable '");
      StreamMgr_nprint_error(streams, wffprint, "%N", ltl_wff);
      StreamMgr_print_error(streams,  "'");
      ErrorMgr_internal_error(errmgr, "Scalar array variable has been found where a boolean "
                     "variable had to be used instead.\n"
                     "This might be due to a bug on your model.");
    }

    if ((time == k) &&
        BeEnc_is_index_input_var(be_enc,
                                 BeEnc_name_to_index(be_enc, ltl_wff))) {
      /* input vars when time == max_time evaluate to false: */
      return Be_Falsity(be_mgr); /* not memoized */
    }

    return BeEnc_name_to_timed(be_enc, ltl_wff, time); /* not memoized */

  case NOT:
    /* checks out that argument of NOT operator is actually a variable: */
    nusmv_assert( node_get_type(car(ltl_wff)) == DOT ||
                  node_get_type(car(ltl_wff)) == BIT ||
                  node_get_type(car(ltl_wff)) == ARRAY);

    if (!SymbTable_is_symbol_declared(st, car(ltl_wff))) {
      ErrorMgr_internal_error(errmgr, "Unexpected scalar or undefined node\n");
    }

    if ((node_get_type(car(ltl_wff)) == ARRAY) &&
        ! SymbTable_is_symbol_bool_var(st, car(ltl_wff))) {
      StreamMgr_print_error(streams,  "Detected scalar array variable '");
      StreamMgr_nprint_error(streams, wffprint, "%N", car(ltl_wff));
      StreamMgr_print_error(streams,  "'");
      ErrorMgr_internal_error(errmgr, "Scalar array variable has been found where a boolean "
                     "variable had to be used instead.\n"
                     "This might be due to a bug on your model.");
    }

    if ((time == k) &&
        BeEnc_is_index_input_var(be_enc,
                                 BeEnc_name_to_index(be_enc, car(ltl_wff)))) {
      /* input vars when time == max_time evaluate to false: */
      result = Be_Falsity(be_mgr);
      break;
    }

    result = Be_Not(be_mgr, BeEnc_name_to_timed(be_enc, car(ltl_wff), time));
    break;

  case AND:
    result = Be_And(be_mgr,
                    BmcInt_Tableau_GetAtTime(be_enc,car(ltl_wff), time, k, l),
                    BmcInt_Tableau_GetAtTime(be_enc,cdr(ltl_wff), time, k, l));
    break;

  case OR:
    result = Be_Or(be_mgr,
                   BmcInt_Tableau_GetAtTime(be_enc,car(ltl_wff), time, k, l),
                   BmcInt_Tableau_GetAtTime(be_enc,cdr(ltl_wff), time, k, l));
    break;

  case IFF:
    result = Be_Iff(be_mgr,
                    BmcInt_Tableau_GetAtTime(be_enc,car(ltl_wff),time,k,l),
                    BmcInt_Tableau_GetAtTime(be_enc,cdr(ltl_wff),time,k,l));
    break;

  case OP_NEXT:
    result = bmc_tableauGetNextAtTime(be_enc, car(ltl_wff), time, k, l);
    break;

  case OP_GLOBAL:
    result = bmc_tableauGetGloballyAtTime(be_enc, car(ltl_wff), time, k, l);
    break;

  case OP_FUTURE: /* EVENTUALLY */
    result = bmc_tableauGetEventuallyAtTime(be_enc, car(ltl_wff), time, k, l);

    break;

  case UNTIL:
    result = bmc_tableauGetUntilAtTime(be_enc, car(ltl_wff), cdr(ltl_wff),
                                       time, k, l);
    break;

  case RELEASES:
    result = bmc_tableauGetReleasesAtTime(be_enc, car(ltl_wff), cdr(ltl_wff),
                                          time, k, l );
    break;

  case IMPLIES:
    ErrorMgr_internal_error(errmgr, "'Implies' should had been nnf-ed away!\n");

  case ATOM:
  case EX:
  case AX:
  case EF:
  case AF:
  case EG:
  case AG:
  case EBF:
  case EBG:
  case ABF:
  case ABG:
    ErrorMgr_internal_error(errmgr,  "Unexpected CTL operator, node type %d\n",
                    node_get_type(ltl_wff) );

  default:
    /* no other type are available here: */
    error_unreachable_code();
  }

  nusmv_assert(result != NULL); /*it must be assigned! */
  bmc_tableau_memoization_insert(tableau_ltl_hash, key, result);
  return result;
}



/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

be_ptr
bmc_tableauGetNextAtTime(const BeEnc_ptr be_enc, const node_ptr ltl_wff,
                         const int time, const int k, const int l)
{
  int succtime;
  be_ptr tableau;

  nusmv_assert((time < k) || (time==k && Bmc_Utils_IsNoLoopback(l)) );

  /* checks out the validity of [l, k] only if a loop exists: */
  nusmv_assert(Bmc_Utils_IsNoLoopback(l) || (k > l) );

  succtime = Bmc_Utils_GetSuccTime(time, k, l);

  if (!Bmc_Utils_IsNoLoopback(succtime)) {
    tableau = BmcInt_Tableau_GetAtTime(be_enc, ltl_wff, succtime, k, l);
  }
  else {
    tableau = Be_Falsity(BeEnc_get_be_manager(be_enc));
  }

  return tableau;
}

be_ptr
bmc_tableauGetEventuallyAtTime(const BeEnc_ptr be_enc,
                               const node_ptr ltl_wff,
                               const int intime, const int k, const int l)
{
  Be_Manager_ptr be_mgr;
  int time;
  be_ptr tableau;
  int stop_time;
  int start_time;

  nusmv_assert((intime < k) || (intime==k && Bmc_Utils_IsNoLoopback(l)) );

  /* checks out the validity of [l, k] only if a loop exists: */
  nusmv_assert(Bmc_Utils_IsNoLoopback(l) || (k > l));

  be_mgr = BeEnc_get_be_manager(be_enc);
  tableau = Be_Falsity(be_mgr);

  /* there exist three cases:
     1) no loop: iterates from k downto intime;
     2) loop, (intime < l): iterates from k-1 downto intime;
     3) loop, (l <= intime < k) : iterates from k-1 downto l */

  if (Bmc_Utils_IsNoLoopback(l)) {
    /* The first case */
    start_time = k;
    stop_time  = intime;
  }
  else {
    /* The second and third case*/
    start_time = k-1;
    stop_time  = min(intime,l);
  }

  for (time = start_time; time>=stop_time; --time) {
    /* lazy evaluation: */
    be_ptr tableau_at_time = BmcInt_Tableau_GetAtTime(be_enc, ltl_wff,
                                                      time, k, l);

    if ( Be_IsTrue(be_mgr, tableau_at_time) ) {
      tableau = tableau_at_time;
      break;
    }
    tableau = Be_Or(be_mgr,
           tableau_at_time, tableau);
  } /* loop */

  return tableau;
}

be_ptr
bmc_tableauGetGloballyAtTime(const BeEnc_ptr be_enc,
                             const node_ptr ltl_wff,
                             const int intime, const int k, const int l)
{
  Be_Manager_ptr be_mgr;
  int time;
  be_ptr tableau;
  int stop_time;

  nusmv_assert((intime < k) || (intime==k && Bmc_Utils_IsNoLoopback(l)) );

  /* checks out the validity of [l, k] only if a loop exists: */
  nusmv_assert(Bmc_Utils_IsNoLoopback(l) || (k > l));

  be_mgr = BeEnc_get_be_manager(be_enc);

  /* there exist three cases:
     1) no loop: cannot assure nothing, so return falsity;
     2) loop, (intime < l): iterates from intime to k-1;
     3) loop, (l <= intime < k) : iterates from intime to k-1, and then from
        l to intime-1 (so more efficiently from l to k-1.)  */
  if (Bmc_Utils_IsNoLoopback(l)) {
    tableau = Be_Falsity(be_mgr);
  }
  else {
    /* second and third cases */
    tableau = Be_Truth(be_mgr);

    stop_time = min(intime, l);
    for (time=k-1; time >= stop_time; --time) {
      /* lazy evaluation: */
      be_ptr tableau_at_time = BmcInt_Tableau_GetAtTime(be_enc, ltl_wff,
                                                        time, k, l);
      if ( Be_IsFalse(be_mgr, tableau_at_time) ) {
        tableau = tableau_at_time;
        break;
      }
      tableau = Be_And(be_mgr,
                       tableau_at_time, tableau);
    }
  }

  return tableau;
}

be_ptr
bmc_tableauGetUntilAtTime(const BeEnc_ptr be_enc,
                          const node_ptr p, const node_ptr q,
                          const int time, const int k, const int l)
{
  int steps;

  nusmv_assert((time < k) || (time==k && Bmc_Utils_IsNoLoopback(l)) );

  if (Bmc_Utils_IsNoLoopback(l)) {
    steps = k - time + 1 ; /* no loop, interval [time, k] */
  }
  else {
    steps = (k-1) - min(time,l) + 1; /* loop, full round */
  }

  return bmc_tableauGetUntilAtTime_aux(be_enc, p, q, time, k, l, steps);
}

be_ptr
bmc_tableauGetReleasesAtTime(const BeEnc_ptr be_enc,
                             const node_ptr p, const node_ptr q,
                             const int time, const int k, const int l)
{
  int steps;

  nusmv_assert (time <= k);

  if (Bmc_Utils_IsNoLoopback(l)) {
    steps = k - time + 1 ; /* no loop, interval [time, k] */
  }
  else {
    steps = (k-1) - min(time,l) + 1; /* loop, full round */
  }

  return bmc_tableauGetReleasesAtTime_aux(be_enc, p, q, time, k, l, steps);
}

hash_ptr Bmc_Tableau_get_handled_hash(SymbTable_ptr symb_table,
                                      char* hash_str)
{
  if (! strcmp(ST_BMC_TABLEAU_LTL_HASH, hash_str)) {
    return
      SymbTable_get_handled_hash_ptr(
                      symb_table,
                      ST_BMC_TABLEAU_LTL_HASH,
                      (ST_PFICPCP)NULL,
                      (ST_PFICPI)NULL,
                      (ST_PFSR)NULL,
                      (SymbTableTriggerFun)NULL,
                      SymbTable_clear_handled_remove_action_hash,
                      (SymbTableTriggerFun)NULL);
  }
  else error_unreachable_code();
}

node_ptr
bmc_tableau_memoization_get_key(NodeMgr_ptr nodemgr,
                                node_ptr wff, int time, int k, int l)
{
  return find_node(nodemgr, CONS, wff,
                   find_node(nodemgr, CONS, PTR_FROM_INT(node_ptr, time),
                             find_node(nodemgr, CONS, PTR_FROM_INT(node_ptr, k),
                                       PTR_FROM_INT(node_ptr, l))));
}

void bmc_tableau_memoization_insert(hash_ptr tableau_ltl_hash,
                                           node_ptr key, be_ptr be)
{
  nusmv_assert(tableau_ltl_hash != (hash_ptr) NULL);

  insert_assoc(tableau_ltl_hash, key, (node_ptr) be);
}

be_ptr bmc_tableau_memoization_lookup(hash_ptr tableau_ltl_hash,
                                             node_ptr key)
{
  nusmv_assert(tableau_ltl_hash != (hash_ptr) NULL);

  return (be_ptr) find_assoc(tableau_ltl_hash, key);
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief auxiliary part of bmc_tableauGetUntilAtTime

  
*/
static be_ptr
bmc_tableauGetUntilAtTime_aux(const BeEnc_ptr be_enc,
                              const node_ptr p, const node_ptr q,
                              const int time, const int k, const int l,
                              const int steps)
{
  Be_Manager_ptr be_mgr;
  be_ptr tableau_temp; /* for increasing of performances */
  be_ptr tableau_following; /* to increase readability */

  nusmv_assert((time < k) || (time==k && Bmc_Utils_IsNoLoopback(l)) );
  nusmv_assert (steps >= 1);

  be_mgr = BeEnc_get_be_manager(be_enc);
  tableau_temp = BmcInt_Tableau_GetAtTime(be_enc, q, time, k, l);

  if (steps > 1) {
    tableau_following =
      bmc_tableauGetUntilAtTime_aux(be_enc, p, q,
                                    Bmc_Utils_GetSuccTime(time, k, l),
                                    k, l, steps - 1);

    tableau_temp =
      Be_Or( be_mgr,
             tableau_temp,
             Be_And(be_mgr,
                    BmcInt_Tableau_GetAtTime(be_enc, p, time, k, l),
                    tableau_following) );
  }
  return tableau_temp;
}

/*!
  \brief auxiliary part of bmc_tableauGetReleasesAtTime

  Builds the release operator expression
*/
static be_ptr
bmc_tableauGetReleasesAtTime_aux(const BeEnc_ptr be_enc,
                                 const node_ptr p, const node_ptr q,
                                 const int time, const int k, const int l,
                                 const int steps)
{
  be_ptr tableau_p;
  be_ptr tableau_q;
  be_ptr tableau_result;

  nusmv_assert((time < k) || (time==k && Bmc_Utils_IsNoLoopback(l)) );
  nusmv_assert (steps >= 1);

  tableau_p = BmcInt_Tableau_GetAtTime(be_enc, p, time, k, l);
  tableau_q = BmcInt_Tableau_GetAtTime(be_enc, q, time, k, l);

  if (steps == 1) {
    if (Bmc_Utils_IsNoLoopback(l)) { /* q & p */
      tableau_result = Be_And(BeEnc_get_be_manager(be_enc),
                              tableau_p, tableau_q);
    } else { /* q */
      tableau_result = tableau_q;
    }
  }
  else { /* q & ( p | X(p R q) ) */
    be_ptr tableau_following =
      bmc_tableauGetReleasesAtTime_aux(be_enc, p, q,
                                       Bmc_Utils_GetSuccTime(time, k, l),
                                       k, l, steps - 1);
    tableau_result =
      Be_And(BeEnc_get_be_manager(be_enc),
             tableau_q,
             Be_Or(BeEnc_get_be_manager(be_enc),
                   tableau_p, tableau_following));
  }

  return tableau_result;
}
