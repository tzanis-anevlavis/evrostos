/* ---------------------------------------------------------------------------


  This file is part of the ``trace'' package of NuSMV version 2.
  Copyright (C) 2010 by FBK.

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
  \brief This module contains support functions to the trace class.

  optional

*/

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/parser/symbols.h"

#include "nusmv/core/fsm/bdd/bdd.h"
#include "nusmv/core/enc/enc.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/bmc/bmc.h"
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

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

bdd_ptr TraceUtils_fetch_as_bdd(Trace_ptr trace, TraceIter step,
                                TraceIteratorType iter_type,
                                BddEnc_ptr bdd_enc)
{
  DDMgr_ptr dd = BddEnc_get_dd_manager(bdd_enc);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(bdd_enc));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  TraceStepIter iter;
  node_ptr var, val;
  SymbTable_ptr symb_table = Trace_get_symb_table(trace);

  bdd_ptr res = bdd_true(dd);
  TRACE_STEP_FOREACH(trace, step, iter_type, iter, var, val) {
    bdd_ptr tmp = BddEnc_expr_to_bdd(bdd_enc,
                                     ExprMgr_equal(exprs, var, val, symb_table),
                                     Nil);
    bdd_and_accumulate(dd, &res, tmp);
    bdd_free(dd, tmp);
  }

  return res;
}

be_ptr TraceUtils_fetch_as_be(Trace_ptr trace, TraceIter step,
                              TraceIteratorType iter_type,
                              BeEnc_ptr be_enc, BddEnc_ptr bdd_enc)
{
  TraceStepIter iter;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(bdd_enc));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  node_ptr var, val;
  Be_Manager_ptr be_mgr;
  bdd_ptr res;

  TRACE_CHECK_INSTANCE(trace);
  BE_ENC_CHECK_INSTANCE(be_enc);
  BDD_ENC_CHECK_INSTANCE(bdd_enc);

  be_mgr = BeEnc_get_be_manager(be_enc);
  res = Be_Truth(be_mgr);
  TRACE_STEP_FOREACH(trace, step, iter_type, iter, var, val) {
    be_ptr tmp = Bmc_Conv_Bexp2Be(be_enc, Compile_detexpr2bexpr(bdd_enc,
                                                        ExprMgr_equal(exprs, var, val, SYMB_TABLE(NULL))));
    res = Be_And(be_mgr, res, tmp);
  }

  return res;
}

Expr_ptr TraceUtils_fetch_as_sexp(Trace_ptr trace, TraceIter step,
                                  TraceIteratorType iter_type)
{
  Expr_ptr res = Nil;

  TRACE_CHECK_INSTANCE(trace);

  {
    node_ptr var, val;
    TraceStepIter iter;
    SymbTable_ptr symb_table = Trace_get_symb_table(trace);

    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
    const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

    res = ExprMgr_true(exprs);

    TRACE_STEP_FOREACH(trace, step, iter_type, iter, var, val) {
      res = ExprMgr_and(exprs, ExprMgr_equal(exprs, var, val, symb_table), res);
    }
  }

  return res;
}

Expr_ptr TraceUtils_fetch_as_big_and(Trace_ptr trace, TraceIter step,
                                     TraceIteratorType iter_type)
{
  Expr_ptr res = NULL;
  node_ptr var, val;
  TraceStepIter iter;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(trace));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));


  TRACE_STEP_FOREACH(trace, step, iter_type, iter, var, val) {
    if ((Expr_ptr)NULL == res) {
      res = find_node(nodemgr, EQUAL, var, val);
    }
    else res = find_node(nodemgr, AND, find_node(nodemgr, EQUAL, var, val), res);
  }

  return res;
}
/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
