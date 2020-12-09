/* ---------------------------------------------------------------------------


  This file is part of the ``mc'' package of NuSMV version 2.
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
  \brief Internal header file of the mc package.

  Internal header file of the mc package.

*/


#ifndef __NUSMV_CORE_MC_MC_INT_H__
#define __NUSMV_CORE_MC_MC_INT_H__

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/dd/dd.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/fsm/bdd/BddFsm.h"
#include "nusmv/core/fsm/FsmBuilder.h"
#include "nusmv/core/trace/TraceMgr.h"
#include "nusmv/core/trace/Trace.h"

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

extern int nusmv_yylineno;
extern cmp_struct_ptr cmps;

extern FsmBuilder_ptr global_fsm_builder;
extern TraceMgr_ptr global_trace_manager;

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \brief This function computes a path that is a witness
   for <i>EX(f)</i>.

  This function finds a path that is a witness for
   <i>EX(f)</i>. <code>path<code> is a BDD which represents the first
   state of the path. It essentially is an initial state from which the
   example can be found.  The formula <i>EX(f)</i> holds under
   fairness constraints in a state <i>s_i</i> iff there is a
   successor state <i>s_{i+1}</i> such that <i>s_{i+1}</i>
   satisfies <i>f</i> and </i>s_{i+1}</i> is the beginning of some
   fair computation path. We look for states that can be reached from
   the state stored as first element in <code>path</code>, which are fair and
   in which <i>f</i> is satisfied. The algorithm computes more than
   one state, in order to have only one state we apply
   <code>bdd_pick_one_state</code>. The result of this application is
   then put in AND with <code>path</code> to form the witness.

  \sa explain
*/
node_ptr ex_explain(BddFsm_ptr, BddEnc_ptr, node_ptr, bdd_ptr);

/*!
  \brief This function finds a path that is a witness
   for <i>E\[f U g\]</i>

  This function finds a path of single states that is a
   witness for <i>E\[f U g\]</i>. The first element of
   <code>path</code> is a BDD <code>p</code> representing a set of
   states from which the first state of the witness path is taken.

   The procedure is to try to execute <code>eu(f,g)</code> again,
   looking for a path from <code>p</code> along states satisfying
   <code>f</code> to a state where <i>g</i> is valid.  The found path
   is then stored in <code>path</code> in reverse order.  Note that in
   the found path every state is a minterm, i.e. a single state, not a
   set. Also the original first element of <code>path</code> is reset
   to minterm.

   If a witness is not found then Nil is return and nothing is modified.
   

  \sa explain
*/
node_ptr eu_explain(BddFsm_ptr, BddEnc_ptr,
                           node_ptr, bdd_ptr, bdd_ptr);

/*!
  \brief This function finds a path that is a witness
   for <i>E\[f U g\]</i> when g is a set of  state-inputs 

  

  \sa explain
*/
node_ptr eu_si_explain(BddFsm_ptr fsm, BddEnc_ptr enc,
                              node_ptr path, bdd_ptr f, bdd_ptr g_si,
                              bdd_ptr hulk);

/*!
  \brief Set of states satisfying <i>EG(g)</i>.

  Computes the set of states satisfying <i>EG(g)</i>.

  \sa eu ex ef
*/
BddStatesInputs ex_si(BddFsm_ptr fsm, bdd_ptr si);

/*!
  \brief Computes the set of state-input pairs that satisfy
  E(f U g), with f and g sets of state-input pairs.

  
*/
BddStatesInputs eu_si(BddFsm_ptr fsm, bdd_ptr f, bdd_ptr g);

/*!
  \brief Set of states-inputs satisfying <i>EG(g)</i>.

  

  \sa eu ex
*/
BddStatesInputs eg_si(BddFsm_ptr fsm, bdd_ptr g_si);

/*!
  \brief This function finds a path that is a witness
   for <i>E\[f U g\]^{sup}_{inf}</i>.

  This function finds a path that is a witness
   for <i>E\[f U g\]^{sup}_{inf}</i>. The first element of
   <code>path</code> is a BDD that represents the first state of the
   path. It is an initial state from which the example can be found.
   The procedure is to try to execute <code>ebu(f, g, inf,
   sup)</code>, looking for a path, with length <code>(sup -
   inf)<code>, from <code>p</code> to a state where <i>g</i> is valid
   using only transitions from states satisfying <i>f</i>.

  \sa explain
*/
node_ptr
ebu_explain(BddFsm_ptr, BddEnc_ptr, node_ptr, bdd_ptr,
            bdd_ptr, int, int);

/*!
  \brief This function finds a path that is an example
   for <i>EG(g)</i>.

  This function finds a path that is an example for
   <i>EG(g)</i>. The first element <code>p</code> is the BDD that
   represents the first state of the path. It is an initial state from
   which the example can be found.<br>

   The procedure is based on the greatest fixed point characterization
   for the CTL operator <b>EG</b>. The CTL formula <i>EG(g)</i> under
   fairness constraints means that there exists a path beginning with
   current state on which <i>g</i> holds globally (invariantly) and
   each formula in the set of fairness constraints holds infinitely
   often on the path.  If we denote with <i>EG(g)</i> the set of states
   that satisfy <i>EG(g)</i> under fairness constraints, we can
   construct the witness path incrementally by giving a sequence of
   prefixes of the path of increasing length until a cycle is found. At
   each step in the construction we must ensure that the current prefix
   can be extended to a fair path along which each state satisfies
   <i>EG(g)</i>.

  \sa explain
*/
node_ptr eg_explain(BddFsm_ptr, BddEnc_ptr, node_ptr, bdd_ptr);

/*!
  \brief This function finds a path of length
   <tt>(sup-inf)</tt> that is an example for
   <i>EG(g)^{sup}_{inf}</i>.

  This function finds a path of length
   <tt>(sup-inf)</tt> that is an example for <i>EG(g)^{sup}_{inf}</i>.
   The first element of <code>p</code> is the BDD that represents the
   first state of the path. It is an initial state from which the
   example has to be found.

  \sa explain
*/
node_ptr ebg_explain(BddFsm_ptr, BddEnc_ptr, node_ptr,
                            bdd_ptr, int, int);

#endif /* __NUSMV_CORE_MC_MC_INT_H__ */
