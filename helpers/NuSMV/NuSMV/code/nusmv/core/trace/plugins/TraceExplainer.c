/* ---------------------------------------------------------------------------


  This file is part of the ``trace.plugins'' package of NuSMV version 2.
  Copyright (C) 2003 by FBK-irst.

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
  \author Ashutosh Trivedi, Marco Pensallorto
  \brief Routines related to TraceExplainer object.

   This file contains the definition of \"TraceExplainer\"
  class. TraceExplainer plugin simply prints the trace.

*/


#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/trace/plugins/TraceExplainer_private.h"
#include "nusmv/core/trace/plugins/TraceExplainer.h"
#include "nusmv/core/trace/plugins/TracePlugin.h"
#include "nusmv/core/trace/Trace.h"
#include "nusmv/core/trace/pkg_traceInt.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/utils_io.h"
#include "nusmv/core/parser/symbols.h"
/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void trace_explainer_finalize(Object_ptr object, void* dummy);

/*---------------------------------------------------------------------------*/
/* Definition of external functions                                          */
/*---------------------------------------------------------------------------*/

TraceExplainer_ptr TraceExplainer_create(boolean changes_only)
{
  TraceExplainer_ptr self = ALLOC(TraceExplainer, 1);

  TRACE_EXPLAINER_CHECK_INSTANCE(self);

  trace_explainer_init(self, changes_only);
  return self;
}

/* ---------------------------------------------------------------------- */
/*     Private Methods                                                    */
/* ---------------------------------------------------------------------- */

/*!
  \brief Initializes trace explain object.


*/

void trace_explainer_init(TraceExplainer_ptr self, boolean changes_only)
{
  if (changes_only) {
    trace_plugin_init(TRACE_PLUGIN(self),
                      "BASIC TRACE EXPLAINER - shows changes only");
  }
  else {
    trace_plugin_init(TRACE_PLUGIN(self),
                      "BASIC TRACE EXPLAINER - shows all variables");
  }

  OVERRIDE(Object, finalize) = trace_explainer_finalize;
  OVERRIDE(TracePlugin, action) = trace_explainer_action;

  self->changes_only = changes_only;
}

/*!
  \brief Deinitializes Explain object.


*/

void trace_explainer_deinit(TraceExplainer_ptr self)
{
  trace_plugin_deinit(TRACE_PLUGIN(self));
}

/*!
  \brief Action method associated with TraceExplainer class.

   The action associated with TraceExplainer is to print the trace
  on the TraceOpt_output_stream(self->opt). If <tt>changes_only</tt> is 1, than only state variables
  which assume a different value from the previous printed one are printed
  out.

  \se <tt>print_hash</tt> is modified.
*/

int trace_explainer_action(const TracePlugin_ptr self)
{
  const Trace_ptr trace = self->trace;
  const SymbTable_ptr symb_table = Trace_get_symb_table(trace);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));

  TraceIter start_iter;
  TraceIter stop_iter;
  TraceIter step;
  TraceIteratorType input_iter_type;
  TraceIteratorType state_iter_type;
  TraceIteratorType combo_iter_type;
  hash_ptr changed_states;
  int i;

  OStream_ptr out = TraceOpt_output_stream(self->opt);

  start_iter = (0 != TraceOpt_from_here(self->opt))
    ? trace_ith_iter(trace, TraceOpt_from_here(self->opt))
    : trace_first_iter(trace);

  stop_iter = (0 != TraceOpt_to_here(self->opt))
    ? trace_ith_iter(trace, 1 + TraceOpt_to_here(self->opt))
    : TRACE_END_ITER;

  input_iter_type = TraceOpt_show_defines(self->opt)
    ? TRACE_ITER_I_SYMBOLS : TRACE_ITER_I_VARS;

  state_iter_type = TraceOpt_show_defines(self->opt)
    ? TRACE_ITER_SF_SYMBOLS : TRACE_ITER_SF_VARS;

  combo_iter_type = TraceOpt_show_defines(self->opt)
    ? (TraceOpt_show_defines_with_next(self->opt)
       ? TRACE_ITER_COMBINATORIAL
       : TRACE_ITER_SI_DEFINES)
    : TRACE_ITER_NONE;

  OStream_printf(out, "Trace Description: %s \n", Trace_get_desc(trace));
  OStream_printf(out, "Trace Type: %s \n", TraceType_to_string(env, Trace_get_type(trace)));

  /* indent */
  OStream_inc_indent_size(out);

  changed_states = new_assoc();
  nusmv_assert(changed_states != (hash_ptr)NULL);

  i = MAX(1, TraceOpt_from_here(self->opt)); step = start_iter;
  while (stop_iter != step) {
    TraceStepIter iter;
    node_ptr symb;
    node_ptr val;

    boolean input_header = false;

    /* lazy defines evaluation */
    if (TraceOpt_show_defines(self->opt)) {
      trace_step_evaluate_defines(trace, step);
    }

    /* COMBINATORIAL SECTION (optional) */
    TRACE_STEP_FOREACH(trace, step, combo_iter_type, iter, symb, val) {
      /* skip non-visible symbols */
      if (!trace_plugin_is_visible_symbol(self, symb)) continue;

      /* if required, print only symbols with changed values */
      if (TRACE_EXPLAINER(self)->changes_only) {
        if (val == find_assoc(changed_states, symb)) { continue; }
        insert_assoc(changed_states, symb, val);
      }

      TracePlugin_print_assignment(self, symb, val);
    } /* foreach SI_DEFINES */

    /* INPUT SECTION (optional) */
    TRACE_STEP_FOREACH(trace, step, input_iter_type, iter, symb, val) {
      if (false == input_header) {
        OStream_printf(out, "-> Input: %d.%d <-\n", Trace_get_id(trace), i);
        input_header = true;
      }
      /* skip non-visible symbols */
      if (!trace_plugin_is_visible_symbol(self, symb)) continue;

      /* if required, print only symbols with changed values */
      if (TRACE_EXPLAINER(self)->changes_only) {
        if (val == find_assoc(changed_states, symb)) { continue; }
        insert_assoc(changed_states, symb, val);
      }

      TracePlugin_print_assignment(self, symb, val);
    } /* foreach I_SYMBOLS */

    if (Trace_step_is_loopback(trace, step)) {
      OStream_printf(out, "-- Loop starts here\n");
    }

    /* STATE SECTION (mandatory) */
    OStream_printf(out, "-> State: %d.%d <-\n", Trace_get_id(trace), i);
    TRACE_STEP_FOREACH(trace, step, state_iter_type, iter, symb, val) {
      /* skip non-visible symbols */
      if (!trace_plugin_is_visible_symbol(self, symb)) continue;

      /* if required, print only symbols with changed values */
      if (TRACE_EXPLAINER(self)->changes_only) {
        if (val == find_assoc(changed_states, symb)) { continue; }
        insert_assoc(changed_states, symb, val);
      }

      TracePlugin_print_assignment(self, symb, val);
    } /* foreach SF_SYMBOLS */

    ++ i; step = TraceIter_get_next(step);
  } /* while */

  free_assoc(changed_states);

  /* deindent */
  OStream_dec_indent_size(out);

  return 0;
}

/*---------------------------------------------------------------------------*/
/* Static functions                                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief Trace Explainer finalize method.


*/
static void trace_explainer_finalize(Object_ptr object, void* dummy)
{
  TraceExplainer_ptr self = TRACE_EXPLAINER(object);

  trace_explainer_deinit(self);
  FREE(self);
}
