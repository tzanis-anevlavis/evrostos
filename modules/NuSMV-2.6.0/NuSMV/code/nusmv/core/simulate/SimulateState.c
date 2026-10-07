/* ---------------------------------------------------------------------------


  This file is part of the ``simulate'' package of NuSMV version 2.
  Copyright (C) 2012 by FBK-irst.

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
  \author Michele Dorigatti
  \brief Implementation of class 'SimulateState'

  This class represents a state of a trace. It is used to save in
  the environment the current simulation state, which all simulate commands
  depend on.

*/

#include "nusmv/core/simulate/simulateInt.h"
#include "nusmv/core/simulate/SimulateState.h"
#include "nusmv/core/simulate/SimulateState_private.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'SimulateState_private.h' for class 'SimulateState' definition. */

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

static void simulate_state_finalize(Object_ptr object, void* dummy);

#ifndef NDEBUG
static inline void check_integrity(SimulateState_ptr const self);
#else

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define check_integrity(self)
#endif

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

SimulateState_ptr SimulateState_create(DDMgr_ptr const dd_mgr,
                                       bdd_ptr const bdd,
                                       TraceLabel const trace_label)
{
  SimulateState_ptr self = ALLOC(SimulateState, 1);
  SIMULATE_STATE_CHECK_INSTANCE(self);

  simulate_state_init(self, dd_mgr, bdd, trace_label);

  check_integrity(self);

  return self;
}

void SimulateState_destroy(SimulateState_ptr self)
{
  SIMULATE_STATE_CHECK_INSTANCE(self);
  check_integrity(self);

  Object_destroy(OBJECT(self), NULL); self = SIMULATE_STATE(NULL);
}

bdd_ptr SimulateState_get_bdd(SimulateState_ptr const self)
{
  bdd_ptr result;

  SIMULATE_STATE_CHECK_INSTANCE(self);
  check_integrity(self);

  if ((bdd_ptr)NULL != self->bdd &&
      TRACE_LABEL_INVALID != self->trace_label) {
    result = bdd_dup(self->bdd);
  }
  else result = (bdd_ptr)NULL;

  check_integrity(self);

  return result;
}

TraceLabel SimulateState_get_trace_label(SimulateState_ptr const self)
{
  TraceLabel trace_label;

  check_integrity(self);

  trace_label = self->trace_label;

  check_integrity(self);

  return trace_label;
}

void SimulateState_set_all(SimulateState_ptr const self,
                           bdd_ptr const state,
                           TraceLabel const label)
{
  SIMULATE_STATE_CHECK_INSTANCE(self);
  check_integrity(self);

  if ((bdd_ptr)NULL != self->bdd) {
    bdd_free(self->dd_mgr, self->bdd); self->bdd = (bdd_ptr)NULL;
  }

  self->bdd = bdd_dup(state);
  self->trace_label = label;

  check_integrity(self);
}

SimulateState_ptr SimulateState_set_in_env(NuSMVEnv_ptr const env,
                                           bdd_ptr const bdd,
                                           TraceLabel const trace_label)
{
  DDMgr_ptr const dd_mgr = DD_MGR(NuSMVEnv_get_value(env, ENV_DD_MGR));
  SimulateState_ptr current_state;

  if (NuSMVEnv_has_value(env, ENV_SIMULATE_STATE)) {
    current_state =
      SIMULATE_STATE(NuSMVEnv_get_value(env, ENV_SIMULATE_STATE));
    SimulateState_set_all(current_state, bdd, trace_label);
  }
  else {
    current_state = SimulateState_create(dd_mgr, bdd, trace_label);
    NuSMVEnv_set_value(env, ENV_SIMULATE_STATE, (void*)current_state);
  }

  check_integrity(current_state);

  return current_state;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void simulate_state_init(SimulateState_ptr const self,
                         DDMgr_ptr const dd_mgr,
                         bdd_ptr const bdd,
                         TraceLabel const trace_label)
{
  /* base class initialization */
  object_init(OBJECT(self));

  /* members initialization */
  self->dd_mgr = dd_mgr;
  self->bdd = bdd_dup(bdd);
  self->trace_label = trace_label;

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = simulate_state_finalize;
}

void simulate_state_deinit(SimulateState_ptr self)
{
  /* members deinitialization */
  bdd_free(self->dd_mgr, self->bdd); self->bdd = (bdd_ptr)NULL;
  self->dd_mgr = DD_MGR(NULL);
  self->trace_label = TRACE_LABEL_INVALID;

  /* base class deinitialization */
  object_deinit(OBJECT(self));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The SimulateState class virtual finalizer

  Called by the class destructor
*/
static void simulate_state_finalize(Object_ptr object, void* dummy)
{
  SimulateState_ptr self = SIMULATE_STATE(object);

  simulate_state_deinit(self);
  FREE(self); self = SIMULATE_STATE(NULL);
}

/*!
  \brief Debugging class integrity check

  To be called at the beginning and at the end of each
  public method, helps a lot in early debugging.
*/

#ifndef NDEBUG

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static inline void check_integrity(SimulateState_ptr const self)
{
  /* fields not null/invalid */
  nusmv_assert((bdd_ptr)NULL != self->bdd);
  nusmv_assert(TRACE_LABEL_INVALID != self->trace_label);
}
#endif

/**AutomaticEnd***************************************************************/

