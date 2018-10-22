/* ---------------------------------------------------------------------------


  This file is part of the ``ltl'' package of NuSMV version 2.
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
  \author Rik Eshuis
  \brief Routines to perform strongly fair LTL model checking

  The technique adopted has been taken from [1].
  <ol>
    <li>O. Y. Kesten, A. Pnueli, and L. Raviv. Algorithmic
        Verification of Linear Temporal Logic Specifications. In
        K.G. Larsen, S. Skyum, and G. Winskel, editors,

        Proceedings of the 25th International Colloquium on Automata,
        Languages, and Programming (ICALP 1998), volume 1443 of
        Lecture Notes in Computer Science, pages
        1-16. Springer-Verlag, 1998.
    </li>
  </ol>

*/


#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/ltl/ltl.h"
#include "nusmv/core/ltl/ltlInt.h"
#include "nusmv/core/fsm/bdd/BddFsm.h"
#include "nusmv/core/fsm/bdd/FairnessList.h"
/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static bdd_ptr
successor(BddEnc_ptr enc, bdd_ptr from, bdd_ptr relation);

static bdd_ptr
successors(BddEnc_ptr enc, bdd_ptr from, bdd_ptr relation);

static bdd_ptr
predecessor(BddEnc_ptr enc, bdd_ptr from, bdd_ptr relation);

static bdd_ptr
predecessors(BddEnc_ptr enc, bdd_ptr from, bdd_ptr relation);

static node_ptr
path(BddEnc_ptr enc, bdd_ptr source, bdd_ptr dest, bdd_ptr R);

static node_ptr
fill_path_with_inputs(BddFsm_ptr fsm, BddEnc_ptr enc, node_ptr path);

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

bdd_ptr feasible(BddFsm_ptr fsm, BddEnc_ptr enc)
{
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  bdd_ptr new, old, R, init_bdd, trans_bdd;
  bdd_ptr invar_bdd, next_invar_bdd;
  JusticeList_ptr justice;
  CompassionList_ptr compassion;
  DDMgr_ptr dd_manager = BddEnc_get_dd_manager(enc);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "checking feasability...");
    Logger_log(logger, "\n");
  }

  init_bdd  = BddFsm_get_init(fsm);
  trans_bdd = BddFsm_get_monolithic_trans_bdd(fsm);
  invar_bdd = BddFsm_get_state_constraints(fsm);

  next_invar_bdd = BddEnc_state_var_to_next_state_var(enc, invar_bdd);

  bdd_and_accumulate(dd_manager, &init_bdd, invar_bdd);
  bdd_and_accumulate(dd_manager, &trans_bdd, invar_bdd);
  bdd_and_accumulate(dd_manager, &trans_bdd, next_invar_bdd);

  justice = BddFsm_get_justice(fsm);
  compassion = BddFsm_get_compassion(fsm);

  old = bdd_false(dd_manager);
  R = bdd_dup(trans_bdd);
  bdd_free(dd_manager, trans_bdd);

  new = successors(enc, init_bdd, R);

  bdd_free(dd_manager, init_bdd);

  while (new != old) {
    bdd_free(dd_manager, old);
    old = bdd_dup(new);
    {
      FairnessListIterator_ptr iter =
        FairnessList_begin(FAIRNESS_LIST(justice));

      while( ! FairnessListIterator_is_end(iter)) {
        /* Loop over specifications */
        bdd_ptr spec = JusticeList_get_p(justice, iter);

        if (opt_verbose_level_gt(opts, 0)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, "evaluating justice constraint");
          Logger_log(logger, "\n");
        }
        {
          bdd_ptr result;

          bdd_and_accumulate(dd_manager, &spec, new);
          result = successors(enc, spec, R);
          bdd_free(dd_manager, spec);
          bdd_free(dd_manager, new);
          new = bdd_dup(result);
          bdd_free(dd_manager, result);
        }
        {
          bdd_ptr nextnew;

          nextnew = BddEnc_state_var_to_next_state_var(enc, new);
          bdd_and_accumulate(dd_manager, &R, nextnew);
          bdd_free(dd_manager, nextnew);
        }

        iter = FairnessListIterator_next(iter);
      }
    }
    {
      FairnessListIterator_ptr iter =
        FairnessList_begin(FAIRNESS_LIST(compassion));

      while( ! FairnessListIterator_is_end(iter)) {
        /* Loop over specifications */
        bdd_ptr p_spec = CompassionList_get_p(compassion, iter);
        bdd_ptr q_spec = CompassionList_get_q(compassion, iter);

        if (opt_verbose_level_gt(opts, 0)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, "evaluating compassion constraint");
          Logger_log(logger, "\n");
        }
        {
          bdd_ptr result1, result2, temp;

          temp=bdd_not(dd_manager, p_spec);
          result1= bdd_and(dd_manager, new, temp);
          bdd_free(dd_manager,temp);
          bdd_and_accumulate(dd_manager, &q_spec, new);
          result2 = successors(enc, q_spec, R);
          bdd_free(dd_manager, p_spec);
          bdd_free(dd_manager, q_spec);
          bdd_free(dd_manager, new);
          new = bdd_or(dd_manager, result1, result2);
          bdd_free(dd_manager, result1);
          bdd_free(dd_manager, result2);
          {
            bdd_ptr nextnew;
            nextnew = BddEnc_state_var_to_next_state_var(enc, new);
            bdd_and_accumulate(dd_manager, &R, nextnew);
            bdd_free(dd_manager, nextnew);
          }
        }
        iter = FairnessListIterator_next(iter);
      }
    }
    {
      bdd_ptr temp, succ;

      succ = successor(enc, new, R);
      temp = bdd_and(dd_manager, new, succ);
      while (new != temp){
        bdd_free(dd_manager, new);
        new = bdd_dup(temp);
        bdd_free(dd_manager, temp);
        bdd_free(dd_manager, succ);
        succ = successor(enc, new, R);
        temp = bdd_and(dd_manager, new, succ);
      }
      bdd_free(dd_manager, temp);
      bdd_free(dd_manager, succ);
    }
 }

 return new;
}

node_ptr witness(BddFsm_ptr fsm, BddEnc_ptr enc, bdd_ptr feasib)
{
  bdd_ptr final, R, state;
  bdd_ptr init_bdd, trans_bdd, invar_bdd, next_invar_bdd;
  node_ptr period, prefix, result;
  JusticeList_ptr justice;
  CompassionList_ptr compassion;
  DDMgr_ptr dd_manager = BddEnc_get_dd_manager(enc);
  NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd_manager));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  prefix = Nil;

  init_bdd  = BddFsm_get_init(fsm);
  trans_bdd = BddFsm_get_monolithic_trans_bdd(fsm);
  invar_bdd = BddFsm_get_state_constraints(fsm);

  next_invar_bdd = BddEnc_state_var_to_next_state_var(enc, invar_bdd);

  bdd_and_accumulate(dd_manager, &init_bdd, invar_bdd);
  bdd_and_accumulate(dd_manager, &trans_bdd, invar_bdd);
  bdd_and_accumulate(dd_manager, &trans_bdd, next_invar_bdd);

  justice = BddFsm_get_justice(fsm);
  compassion = BddFsm_get_compassion(fsm);

  final = bdd_dup(feasib);
  R = bdd_and(dd_manager, trans_bdd, final);

  state = BddEnc_pick_one_state(enc, final);
  {
    bdd_ptr succ, prec, not, diff;

    succ = successors(enc, state, R);
    prec = predecessors(enc, R, state);
    not = bdd_not(dd_manager, succ);
    diff = bdd_and(dd_manager, prec, not);
    while (bdd_isnot_false(dd_manager, diff)){
      bdd_free(dd_manager, state);
      state = BddEnc_pick_one_state(enc, diff);
      bdd_free(dd_manager, succ);
      bdd_free(dd_manager, prec);
      bdd_free(dd_manager, diff);
      bdd_free(dd_manager, not);
      succ  = successors(enc, state, R);
      prec  = predecessors(enc, R, state);
      not = bdd_not(dd_manager, succ);
      diff = bdd_and(dd_manager, prec, not);
    }

    bdd_free(dd_manager, diff);
    bdd_free(dd_manager, final);
    final = bdd_and(dd_manager, succ, prec);
    bdd_free(dd_manager, not);
    bdd_free(dd_manager, succ);
    bdd_free(dd_manager, prec);
    bdd_and_accumulate(dd_manager, &R, final);
    {
      bdd_ptr next_final;

      next_final = BddEnc_state_var_to_next_state_var(enc, final);
      bdd_and_accumulate(dd_manager, &R, next_final);
    }
  }

  prefix = reverse(path(enc, init_bdd, final, trans_bdd));
  bdd_free(dd_manager, trans_bdd);
  bdd_free(dd_manager, init_bdd);
  period = cons(nodemgr, last(prefix), Nil);

  {
   FairnessListIterator_ptr iter =
        FairnessList_begin(FAIRNESS_LIST(justice));

    while( ! FairnessListIterator_is_end(iter) ) {
      /* Loop over specifications */
      bdd_ptr spec = JusticeList_get_p(justice, iter);

      if (opt_verbose_level_gt(opts, 0)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "evaluating ");
        Logger_log(logger, "\n");
      }
      {
        node_ptr curr_period;
        int found = 0;

        curr_period = period;
        while (! found) {
          bdd_ptr tmp = bdd_and(dd_manager, spec, (bdd_ptr)car(curr_period));

          found = bdd_isnot_false(dd_manager, tmp);
          curr_period = cdr(curr_period);
          bdd_free(dd_manager, tmp);
          if (curr_period == Nil) break;
        }
        if (!found){
          bdd_ptr tmp = bdd_and(dd_manager, final, spec);

          period = append(period,
                          reverse(path(enc, (bdd_ptr)last(period),
                                       tmp, R)));
          bdd_free(dd_manager, tmp);
        }

      }
      bdd_free(dd_manager, spec);
      iter = FairnessListIterator_next(iter);
    }
  }
  {
   FairnessListIterator_ptr iter =
        FairnessList_begin(FAIRNESS_LIST(compassion));

    while( ! FairnessListIterator_is_end(iter) ) {
      /* Loop over specifications */
      bdd_ptr p_spec = CompassionList_get_p(compassion, iter);
      bdd_ptr q_spec = CompassionList_get_q(compassion, iter);

      {
        node_ptr curr_period;
        int found = 0;

        curr_period = period;

        while (! found) {
          bdd_ptr tmp = bdd_and(dd_manager, q_spec,
                                (bdd_ptr)car(curr_period));
          found = bdd_isnot_false(dd_manager, tmp);
          curr_period = cdr(curr_period);
          bdd_free(dd_manager, tmp);
          if (curr_period == Nil) break;
        }
        if (!found){
          bdd_ptr tmp = bdd_and(dd_manager, final, p_spec);

          if (bdd_isnot_false(dd_manager, tmp)) {
            bdd_ptr tmp_1 = bdd_and(dd_manager, final, q_spec);

            period = append(period,
                            reverse(path(enc, (bdd_ptr)last(period), tmp, R)));
            bdd_free(dd_manager, tmp_1);
          }
          bdd_free(dd_manager, tmp);
        }
      }
      bdd_free(dd_manager, p_spec);
      bdd_free(dd_manager, q_spec);
      iter = FairnessListIterator_next(iter);
    }
  }

  {
    node_ptr temp;
    temp = reverse(path(enc, (bdd_ptr)last(period), (bdd_ptr)last(prefix), R));
    temp = cdr(temp);
    period = append(period, temp);
  }

  prefix = append(prefix, cdr(period));

  result = fill_path_with_inputs(fsm, enc, prefix);

  { /* We release the prefix list and its contents */
    node_ptr p = prefix;

    while(p != Nil) {
      node_ptr m = p;

      p = cdr(p);
      bdd_free(dd_manager, (bdd_ptr)car(m));
      free_node(nodemgr, m);
    }
  }
  return result;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Compute the direct successor of a state

  Given a state from and transition relation, compute the
  direct successor state.
*/
static bdd_ptr successor(BddEnc_ptr enc, bdd_ptr from, bdd_ptr relation)
{
  bdd_ptr result, next_result, vars;
  DDMgr_ptr dd_manager = BddEnc_get_dd_manager(enc);

  vars = BddEnc_get_state_vars_cube(enc);
  next_result = bdd_and_abstract(dd_manager, relation, from, vars);
  result = BddEnc_next_state_var_to_state_var(enc, next_result);
  bdd_free(dd_manager, next_result);
  bdd_free(dd_manager, vars);

  return result;
}

/*!
  \brief Compute the direct and indirect successors of a state

  Given a state from and transition relation, compute the
  direct and indirect successor states (transitive closure of
  successor).
*/
static bdd_ptr successors(BddEnc_ptr enc, bdd_ptr from, bdd_ptr relation)
{
  DDMgr_ptr dd_manager = BddEnc_get_dd_manager(enc);
  bdd_ptr old, new;

  old = bdd_false(dd_manager);
  new = bdd_dup(from);
  while (old != new){
    bdd_ptr image;

    bdd_free(dd_manager, old);
    old = bdd_dup(new);
    image = successor(enc, old, relation);
    bdd_or_accumulate(dd_manager, &new, image);
    bdd_free(dd_manager, image);
  }
  bdd_free(dd_manager, old);

  return new;
}

/*!
  \brief Compute the direct predecessor of a state

  Given a state to and a transition relation, compute the
  direct predecessor state.
*/
static bdd_ptr predecessor(BddEnc_ptr enc, bdd_ptr relation, bdd_ptr to)
{
  bdd_ptr result, next_to, vars;
  DDMgr_ptr dd_manager = BddEnc_get_dd_manager(enc);

  next_to = BddEnc_state_var_to_next_state_var(enc, to);
  vars = BddEnc_get_next_state_vars_cube(enc);
  result = bdd_and_abstract(dd_manager, relation, next_to, vars);
  bdd_free(dd_manager, next_to);
  bdd_free(dd_manager, vars);

  return result;
}

/*!
  \brief Compute the direct and indirect predecessors of a state

  Given a state to and a transition relation, compute the
  direct and indirect predecessor states (transitive closure of
  predecessor).
*/
static bdd_ptr predecessors(BddEnc_ptr enc, bdd_ptr relation, bdd_ptr to)
{
  DDMgr_ptr dd_manager = BddEnc_get_dd_manager(enc);
  bdd_ptr old, new;

  old = bdd_false(dd_manager);
  new = bdd_dup(to);
  while (old != new){
    bdd_ptr bwdimage;

    bdd_free(dd_manager,old);
    old = bdd_dup(new);
    bwdimage = predecessor(enc, relation,old);
    bdd_or_accumulate(dd_manager, &new, bwdimage);
    bdd_free(dd_manager,bwdimage);
  }
  bdd_free(dd_manager, old);
  return new;
}

/*!
  \brief Compute a path from source to destination

  Computes a path given the bdds representind the source
  states, the target states, and the transition relation.
*/
static node_ptr path(BddEnc_ptr enc, bdd_ptr source,
                     bdd_ptr dest, bdd_ptr R) {
  node_ptr L;
  bdd_ptr start, test, f, s, tmp;
  DDMgr_ptr dd_manager = BddEnc_get_dd_manager(enc);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  L = Nil;
  start = bdd_dup(source);
  f = predecessor(enc, R, dest);

  tmp = bdd_and(dd_manager, start, f);
  while (bdd_is_false(dd_manager, tmp)){
    bdd_ptr fold;

    fold = f;
    f = predecessor(enc, R, fold);
    bdd_free(dd_manager, fold);
    bdd_free(dd_manager, tmp);
    tmp = bdd_and(dd_manager, start, f);
  }
  bdd_free(dd_manager, tmp);
  {
    bdd_ptr temp;

    temp = bdd_and(dd_manager, start, f);
    s = BddEnc_pick_one_state(enc, temp);
    bdd_free(dd_manager, temp);
  }
  L = cons(nodemgr, (node_ptr)bdd_dup(s), L);

  bdd_free(dd_manager, start);
  start = successor(enc, s, R);
  test = bdd_and(dd_manager, start, dest);
  bdd_free(dd_manager, f);
  test = bdd_and(dd_manager, start, dest);
  while (bdd_is_false(dd_manager, test)){
    bdd_ptr tmp;

    f = predecessor(enc, R, dest);

    tmp = bdd_and(dd_manager, start, f);
    while (bdd_is_false(dd_manager, tmp)){
      bdd_ptr fold;

      fold = f;
      f = predecessor(enc, R, fold);
      bdd_free(dd_manager, fold);
      bdd_free(dd_manager, tmp);
      tmp =  bdd_and(dd_manager, start, f);
    }
    bdd_free(dd_manager, tmp);
    {
      bdd_ptr temp;

      temp = bdd_and(dd_manager, start, f);
      s = BddEnc_pick_one_state(enc, temp);
      bdd_free(dd_manager, temp);
    }
    L = cons(nodemgr, (node_ptr)bdd_dup(s), L);
    bdd_free(dd_manager, start);
    start = successor(enc, s, R);
    bdd_free(dd_manager, test);
    test = bdd_and(dd_manager, start, dest);
    bdd_free(dd_manager, f);
  }
  {
    bdd_ptr last, tmp;

    tmp = bdd_and(dd_manager, start, dest);
    last = BddEnc_pick_one_state(enc, tmp);
    bdd_free(dd_manager, tmp);
    L = cons(nodemgr, (node_ptr)bdd_dup(last), L);
    bdd_free(dd_manager, last);
    return L;
  }
}

/*!
  \brief Fill a path with inputs.

  Fills a path with inputs.
*/
static node_ptr fill_path_with_inputs(BddFsm_ptr fsm,
                                      BddEnc_ptr enc,
                                      node_ptr path)
{
  node_ptr p;
  node_ptr result = Nil;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  DDMgr_ptr dd_manager = BddEnc_get_dd_manager(enc);

  for (p = path; (p != Nil) && (cdr(p) != Nil); p = cdr(p)) {
    bdd_ptr inputs, input;
    /* We assume start and next being singletons */
    bdd_ptr start = bdd_dup((bdd_ptr)car(p));
    bdd_ptr next = bdd_dup((bdd_ptr)car(cdr(p)));

    inputs = BddFsm_states_to_states_get_inputs(fsm, start, next);
    input = BddEnc_pick_one_input(enc, inputs);

    result = cons(nodemgr, cons(nodemgr, (node_ptr)bdd_dup(input), (node_ptr)bdd_dup(start)), result);
    bdd_free(dd_manager, input);
    bdd_free(dd_manager, inputs);
    bdd_free(dd_manager, next);
    bdd_free(dd_manager, start);
  }
  if (p != Nil) {
    /* We take care of the last state if any */
    result = cons(nodemgr, (node_ptr)bdd_dup((bdd_ptr)car(p)), result);
  }

  /* We reverse the list since it was built reversed */
  return reverse(result);
}

