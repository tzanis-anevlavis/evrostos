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
  \author Marco Roveri
  \brief Internal header of the <tt>ltl</tt> package.

  \todo: Missing description

*/


#ifndef __NUSMV_CORE_LTL_LTL_INT_H__
#define __NUSMV_CORE_LTL_LTL_INT_H__

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/dd/dd.h"
#include "nusmv/core/opt/opt.h"

#include "nusmv/core/compile/compile.h"
#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/compile/symb_table/SymbLayer.h"
#include "nusmv/core/compile/symb_table/SymbType.h"

#include "nusmv/core/fsm/FsmBuilder.h"
#include "nusmv/core/fsm/bdd/BddFsm.h"
#include "nusmv/core/trace/TraceMgr.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef enum {
  LTL_REWRITE_STANDARD,
  LTL_REWRITE_DEADLOCK_FREE,
} LtlRewriteType;

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
extern cmp_struct_ptr cmps;
extern bdd_ptr trans_bdd;
extern bdd_ptr fair_states_bdd;
extern node_ptr fairness_constraints_bdd;
extern bdd_ptr invar_bdd;
extern bdd_ptr init_bdd;

extern FsmBuilder_ptr global_fsm_builder;
extern TraceMgr_ptr global_trace_manager;

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \brief Compute a withness of feasability

  Computes fair path from one of the states
  passed as parameter.
*/
node_ptr
witness(BddFsm_ptr fsm, BddEnc_ptr enc, bdd_ptr feasible);

/*!
  \brief Check for feasability

  Checks whether the model has a fair path and returns
  the initial state of the path.
*/
bdd_ptr feasible(BddFsm_ptr fsm, BddEnc_ptr enc);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
node_ptr
Ltl_RewriteInput(SymbTable_ptr symb_table, node_ptr expr,
                 SymbLayer_ptr layer,
                 node_ptr* init, node_ptr* invar, node_ptr* trans,
                 LtlRewriteType rewrite_type);

#endif /* __NUSMV_CORE_LTL_LTL_INT_H__ */
