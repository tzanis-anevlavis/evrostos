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
  \brief This module contains the public part of the implementation
               of the Trace class.

  A trace is logically defined as follows:

  T := S (i S) *

  That is, a trace consists of an initial state, optionally followed
  by an arbitrary number of <input, next state> pairs. The internal
  representation has been designed to match as closely as possible the
  logical definition given above.

  A trace is internally represented as a doubly linked list of custom
  containers called \"frames\".

  The Trace class provides iterators and methods to populate and query
  its frames. Just for the sake of clarity, A graphical representation
  of the internal representation follows:

 +----+----+-+   +-+----+----+-+   +-+----+----+-+
 |    |    |*|<->|*|    |    |*|<->|*|    |    |0| (terminator)
 | ## | S1 | |   | | i2 | S2 | |   | | i3 | S3 | |
 |    |    |*|   |*|    |    |*|   |*|    |    |*|
 +----+----+++   +++----+----+++   +++----+----+++
            |     |           |     |           |
            v     v           v     v           v
 +---+     +-------+         +-------+          +-------+
 | F |     |  D12  |         |  D23  |          |  D34  |
 +-+-+     +-------+         +-------+          +-------+

Remarks:

 * The list is doubly-linked. It can be traversed in both directions
   using iterators.

 * Frozen values (F) are stored in a separate frame. All the
   steps keep a pointer to the data without reallocating it.

 * Initial step does not contain any input.

 * Defines over (Si, i+1, Si+1) are stored in an auxiliary frame, which
   is accessible from both steps (i-1, Si), (i, Si+1).

   Defines are functions over a certain set of variables (dependency
   set). They can be catoegorized according to the kins of variables
   in their dependency set:

   Thus 7 distinct categories of defines can be distinguished as
   described in the following table:

  +-------------+--------------+---------------+------------------------------+
  | curr. state |     input    |  next state   |    Observable category       |
  +-------------+--------------+---------------+------------------------------+
  |     N       |      N       |      N        |          CONSTANT            |
  |     Y       |      N       |      N        |           STATE              |
  |     N       |      Y       |      N        |           INPUT              |
  |     Y       |      Y       |      N        |        STATE-INPUT           |
  |     N       |      N       |      Y        |           NEXT               |
  |     Y       |      N       |      Y        |        STATE-NEXT            |
  |     N       |      Y       |      Y        |        INPUT_NEXT            |
  |     Y       |      Y       |      Y        |     STATE-INPUT-NEXT         |
  +-------------+--------------+---------------+------------------------------+

  The 2 classes of variables (STATE, INPUT) and the 8 categories of
  defines described above account for 9 distinct sections of symbols
  for each step. FROZENVARs are considered as STATE variables and are
  always prepended in iterators. CONSTANTs are considered as STATE defines

*/

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/trace/pkg_trace.h"
#include "nusmv/core/trace/pkg_traceInt.h"
#include "nusmv/core/trace/Trace_private.h"
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/
static const char* TRACE_TYPE_UNSPECIFIED_STRING  = "Unspecified";
static const char* TRACE_TYPE_CNTEXAMPLE_STRING   = "Counterexample";
static const char* TRACE_TYPE_SIMULATION_STRING   = "Simulation";
static const char* TRACE_TYPE_EXECUTION_STRING    = "Execution";

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
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

Trace_ptr Trace_create (const SymbTable_ptr st, const char* desc,
                        const TraceType type, const NodeList_ptr symbols,
                        boolean is_volatile)
{
  return trace_create(st, desc, type, symbols, is_volatile,
		      false /* bits not allowed */);
}

Trace_ptr Trace_create_allow_bits (const SymbTable_ptr st, const char* desc,
				   const TraceType type,
				   const NodeList_ptr symbols,
				   boolean is_volatile)
{
  return trace_create(st, desc, type, symbols, is_volatile,
		      true /* bits allowed */);
}

Trace_ptr
Trace_copy (const Trace_ptr self, const TraceIter until_here,
            boolean is_volatile)
{
  TRACE_CHECK_INSTANCE(self);
  return trace_copy(self, until_here, is_volatile);
}

Trace_ptr
Trace_concat (Trace_ptr self, Trace_ptr* other)
{
  TRACE_CHECK_INSTANCE(self);
  return trace_concat(self, other);
}

void Trace_destroy(Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);
  trace_destroy(self);
}

TraceIter Trace_first_iter (const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);
  return trace_first_iter(self);
}

TraceIter Trace_ith_iter (const Trace_ptr self, unsigned i)
{
 TRACE_CHECK_INSTANCE(self);
 nusmv_assert(i > 0);

 return trace_ith_iter(self, i);
}

TraceIter Trace_last_iter (const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);
  return trace_last_iter(self);
}

TraceIter TraceIter_get_next (const TraceIter iter)
{
  TRACE_ITER_CHECK_INSTANCE(iter);
  return trace_iter_get_next(iter);
}

TraceIter TraceIter_get_prev (const TraceIter iter)
{
  TRACE_ITER_CHECK_INSTANCE(iter);
  return trace_iter_get_prev(iter);
}

boolean TraceIter_is_end(const TraceIter iter)
{
  return (TRACE_END_ITER == iter);
}

TraceIter
Trace_append_step(Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);
  return trace_append_step(self);
}

boolean Trace_step_is_loopback(Trace_ptr self, TraceIter step)
{
  TRACE_CHECK_INSTANCE(self);
  if (TRACE_END_ITER == step) return false;

  return trace_step_is_loopback(self, step);
}

boolean Trace_is_volatile (const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);
  return trace_is_volatile(self);
}

boolean Trace_is_frozen (const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);
  return trace_is_frozen(self);
}

boolean
Trace_is_thawed (const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);
  return trace_is_thawed(self);
}

void Trace_freeze (Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);
  trace_freeze(self);
}

void Trace_thaw (Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);
  trace_thaw(self);
}

boolean Trace_equals(const Trace_ptr self, const Trace_ptr other)
{
  return trace_equals(self, other);
}

void Trace_step_force_loopback (const Trace_ptr self, TraceIter step)
{
  TRACE_CHECK_INSTANCE(self);
  TRACE_ITER_CHECK_INSTANCE(step);

  trace_step_force_loopback(self, step);
}

boolean Trace_symbol_in_language (Trace_ptr self, node_ptr symb)
{
  TRACE_CHECK_INSTANCE(self);

  return trace_symbol_in_language(self, symb);
}

boolean Trace_covers_language(const Trace_ptr self, NodeList_ptr symbols)
{
  ListIter_ptr iter;
  NODE_LIST_FOREACH(symbols, iter) {
    if (!Trace_symbol_in_language(self, NodeList_get_elem_at(symbols, iter))) {
      return false;
    }
  }

  return true;
}

SymbTable_ptr Trace_get_symb_table(Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);

  return trace_get_symb_table(self);
}

NodeList_ptr Trace_get_symbols (const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);

  return trace_get_symbols(self);
}

NodeList_ptr Trace_get_s_vars (const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);

  return trace_get_s_vars(self);
}

NodeList_ptr Trace_get_sf_vars (const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);

  return trace_get_sf_vars(self);
}

NodeList_ptr Trace_get_i_vars (const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);

  return trace_get_i_vars(self);
}

boolean Trace_symbol_is_assigned(Trace_ptr self, TraceIter step, node_ptr symb)
{
  TRACE_CHECK_INSTANCE(self);
  return Trace_step_get_value(self, step, symb) != Nil;
}

boolean Trace_is_complete (Trace_ptr self, NodeList_ptr vars, boolean report)
{
  StreamMgr_ptr streams;
  NuSMVEnv_ptr env;
  FILE* err;

  TRACE_CHECK_INSTANCE(self);
  env = EnvObject_get_environment(ENV_OBJECT(self));
  streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  err = (report) ? StreamMgr_get_error_stream(streams) : NIL(FILE);
  return trace_is_complete_vars(self, vars, err);
}


/*!
  \brief Returns a string corresponding to a TraceType.


*/

const char* TraceType_to_string(const NuSMVEnv_ptr env,
                                const TraceType trace_type)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  switch (trace_type){
  case TRACE_TYPE_UNSPECIFIED :
    return TRACE_TYPE_UNSPECIFIED_STRING;
  case TRACE_TYPE_CNTEXAMPLE:
    return TRACE_TYPE_CNTEXAMPLE_STRING;
  case TRACE_TYPE_SIMULATION:
    return TRACE_TYPE_SIMULATION_STRING;
  case TRACE_TYPE_EXECUTION:
    return TRACE_TYPE_EXECUTION_STRING;
  default: ErrorMgr_internal_error(
      errmgr,
      "%s:%d:%s: unexpected trace type. (%d)",
      __FILE__, __LINE__, __func__, trace_type);
  }

  error_unreachable_code(); /* unreachable */
  return (const char*)(NULL);
}

boolean Trace_step_put_value (Trace_ptr self, TraceIter step,
                              node_ptr symb, node_ptr value)
{
  TRACE_CHECK_INSTANCE(self);

  {
    const SymbTable_ptr st = trace_get_symb_table(self);
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));


    if (TRACE_END_ITER == step) {
      ErrorMgr_internal_error(errmgr, "%s:%d:%s: invalid iterator.",
                              __FILE__, __LINE__, __func__);
    }

    return trace_step_put_value(self, step, symb, value);
  }
}

node_ptr Trace_step_get_value (Trace_ptr self, TraceIter step,
                               node_ptr symb)
{
  TRACE_CHECK_INSTANCE(self);

  {
    const SymbTable_ptr st = trace_get_symb_table(self);
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    if (TRACE_END_ITER == step) {
      ErrorMgr_internal_error(errmgr, "%s:%d:%s: invalid iterator.",
                              __FILE__, __LINE__, __func__);
    }

    return trace_step_get_value(self, step, symb);
  }
}

TraceStepIter Trace_step_iter (const Trace_ptr self, const TraceIter step,
                               const TraceIteratorType iter_type)
{
  TRACE_CHECK_INSTANCE(self);
  if (TRACE_END_ITER == step) {
    SymbTable_ptr st = trace_get_symb_table(self);
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    ErrorMgr_internal_error(errmgr, "%s:%d:%s: invalid iterator.",
                   __FILE__, __LINE__, __func__);
  }

  return trace_step_iter(self, step, iter_type);
}

TraceSymbolsIter Trace_symbols_iter (const Trace_ptr self,
                                     TraceIteratorType iter_type)
{
  TRACE_CHECK_INSTANCE(self);

  return trace_symbols_iter(self, iter_type);
}

boolean Trace_step_iter_fetch(TraceStepIter* step_iter,
                              node_ptr* symb, node_ptr* value)
{
  if (NULL == step_iter) return false;
  return trace_step_iter_fetch(step_iter, symb, value);
}

boolean Trace_symbols_iter_fetch(TraceSymbolsIter* symbols_iter,
                                 node_ptr* symb)
{
  return trace_symbols_iter_fetch(symbols_iter, symb);
}

int Trace_get_id(Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);

  return trace_get_id(self);
}

void Trace_register(Trace_ptr self, int id)
{
  TRACE_CHECK_INSTANCE(self);

  trace_register(self, id);
}

void Trace_unregister(Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);

  trace_unregister(self);
}

boolean Trace_is_registered(const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);

  return trace_is_registered(self);
}

const char* Trace_get_desc (const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);

  return trace_get_desc(self);
}

void Trace_set_desc (Trace_ptr self, const char* desc)
{
  TRACE_CHECK_INSTANCE(self);

  trace_set_desc(self, desc);
}

TraceType Trace_get_type (const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);

  return trace_get_type(self);
}

void Trace_set_type(const Trace_ptr self, TraceType trace_type)
{
  TRACE_CHECK_INSTANCE(self);

  trace_set_type(self, trace_type);
}

unsigned Trace_get_length (const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);

  return trace_get_length(self);
}

boolean Trace_is_empty (const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);

  return trace_is_empty(self);
}

boolean Trace_has_loopback(const Trace_ptr self)
{
  TRACE_CHECK_INSTANCE(self);

  {
    TraceIter iter;
    TRACE_FOREACH (self, iter) {
      if ( Trace_step_is_loopback(self, iter) ) {
        return true;
      }
    }

    return false;
  }
}

boolean Trace_validate_loopback(const Trace_ptr self, int loopback_idx)
{
  TRACE_CHECK_INSTANCE(self);

  {
    TraceIter iter;
    int i = 0;

    TRACE_FOREACH (self, iter) {
      if ( Trace_step_is_loopback(self, iter) ) {
        if (loopback_idx == i) {
          return true;
        }
      }
      i = i + 1;
    }

    return false;
  }
}

int Trace_get_first_loopback_from(const Trace_ptr self, TraceIter* step)
{
  TraceIter iter;
  int step_count;

  TRACE_CHECK_INSTANCE(self);

  iter = ((NULL != step)? *step : Trace_first_iter(self));

  step_count = ((NULL == step)? 0: trace_iter_i(self, *step)-1);

  while (TRACE_END_ITER != iter) {
    if ( Trace_step_is_loopback(self, iter) ) {

      if (step != NULL) {
        *step = iter;
      }

      return step_count;
    }

    ++step_count;
    iter = TraceIter_get_next(iter);
  }
  return -1;
}

void Trace_print_loopbacks(const Trace_ptr self, OStream_ptr stream)
{
  int step_count;
  TraceIter iter;
  boolean found;

  TRACE_CHECK_INSTANCE(self);

  step_count = 0;
  found = false;

  TRACE_FOREACH (self, iter) {
    if ( Trace_step_is_loopback(self, iter) ) {
      if ( !found ) {
        found = true;
        OStream_printf(stream, "The available loopbacks are:");
        OStream_printf(stream, " %d", step_count);
      }
      else {
        OStream_printf(stream, ", %d", step_count);
      }
    }
    ++step_count;
  }

  if ( !found ) {
    OStream_printf(stream, "The trace does not contain any loopback");
  }

  OStream_printf(stream, ". \n");
}
