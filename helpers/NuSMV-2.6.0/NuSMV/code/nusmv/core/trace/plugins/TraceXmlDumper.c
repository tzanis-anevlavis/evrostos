/* ---------------------------------------------------------------------------


  This file is part of the ``trace.plugins'' package of NuSMV version 2.
  Copyright (C) 2004 by FBK-irst.

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
  \author Ashutosh Trivedi, Roberto Cavada, Marco Pensallorto
  \brief Routines related to TraceXmlDumper class

   This file contains the definition of TraceXmlDumper
  class.

*/


#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/trace/plugins/TraceXmlDumper.h"
#include "nusmv/core/trace/plugins/TraceXmlDumper_private.h"
#include "nusmv/core/trace/Trace_private.h"

#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/parser/symbols.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/
static const char* TRACE_XML_VERSION_INFO_STRING =      \
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void trace_xml_dumper_finalize(Object_ptr object, void* dummy);

/*---------------------------------------------------------------------------*/
/* Definition of external functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief Action method associated with TraceXmlDumper class.

   Given trace is written into the file pointed by
  given additional parameter 
*/

int trace_xml_dumper_action(TracePlugin_ptr self)
{
  const Trace_ptr trace = self->trace;
  TraceIter start_iter;
  TraceIter stop_iter;
  TraceIter step;
  TraceIteratorType input_iter_type;
  TraceIteratorType state_iter_type;
  TraceIteratorType combo_iter_type;
  int i;

  NodeList_ptr loops = NodeList_create(); /* contains loops information */
  ListIter_ptr loops_iter;

  OStream_ptr out = TraceOpt_output_stream(self->opt);

  boolean first_node = true;
  boolean is_embedded = TRACE_XML_DUMPER(self)->is_embedded;

  start_iter = (0 != TraceOpt_from_here(self->opt))
    ? trace_ith_iter(trace, TraceOpt_from_here(self->opt))
    : trace_first_iter(trace);

  /* safe way to skip one more step */
  stop_iter =
    (0 != TraceOpt_to_here(self->opt))
    ? trace_ith_iter(trace, 1 + TraceOpt_to_here(self->opt))
    : TRACE_END_ITER;

  input_iter_type = TraceOpt_show_defines(self->opt)
    ? TRACE_ITER_I_SYMBOLS : TRACE_ITER_I_VARS;

  state_iter_type = TraceOpt_show_defines(self->opt)
    ? TRACE_ITER_SF_SYMBOLS : TRACE_ITER_SF_VARS;

  combo_iter_type = TraceOpt_show_defines(self->opt)
    ? TRACE_ITER_COMBINATORIAL : TRACE_ITER_NONE;

  if (! is_embedded) OStream_printf(out,"%s\n", TRACE_XML_VERSION_INFO_STRING);

  if (Trace_is_registered(trace)) {
    OStream_printf(out,"<%s type=\"%d\" id=\"%d\" desc=\"%s\" >\n",
                   TRACE_XML_CNTX_TAG_STRING, Trace_get_type(trace),
                   Trace_get_id(trace), Trace_get_desc(trace));
  }
  else {
    OStream_printf(out,"<%s type=\"%d\" desc=\"%s\" >\n",
                   TRACE_XML_CNTX_TAG_STRING, Trace_get_type(trace),
                   Trace_get_desc(trace));
  }
  first_node = true;
  i = MAX(1, TraceOpt_from_here(self->opt)); step = start_iter;
  while (stop_iter != step) {
    TraceStepIter iter;
    node_ptr symb, val;

    boolean combo_header = false;
    boolean input_header = false;

    /* lazy defines evaluation */
    if (TraceOpt_show_defines(self->opt)) {
      trace_step_evaluate_defines(trace, step);
    }

    if (Trace_step_is_loopback(trace, step)) {
      NodeList_append(loops, NODE_FROM_INT(i));
    }

    TRACE_STEP_FOREACH(trace, step, combo_iter_type, iter, symb, val){
      /* skip non-visible symbols */
      if (!trace_plugin_is_visible_symbol(self, symb)) continue;

      if (false == combo_header) {
        OStream_printf(out, "\t\t<%s id=\"%d\">\n", TRACE_XML_COMB_TAG_STRING, i);
        combo_header = true;
      }

      TracePlugin_print_assignment(self, symb, val);
    } /* foreach COMBINATORIAL */

    if (combo_header) {
      OStream_printf(out, "\t\t</%s>\n", TRACE_XML_COMB_TAG_STRING);
    }

    TRACE_STEP_FOREACH(trace, step, input_iter_type, iter, symb, val) {
      /* skip non-visible symbols */
      if (!trace_plugin_is_visible_symbol(self, symb)) continue;

      if (false == input_header) {
        OStream_printf(out, "\t\t<%s id=\"%d\">\n", TRACE_XML_INPUT_TAG_STRING, i);
        input_header = true;
      }

      TracePlugin_print_assignment(self, symb, val);
    } /* foreach INPUT */

    if (input_header) {
      OStream_printf(out, "\t\t</%s>\n", TRACE_XML_INPUT_TAG_STRING);
    }

    /* </node> */
    if (!first_node) {
      OStream_printf(out, "\t</%s>\n", TRACE_XML_NODE_TAG_STRING);
    }

    /* <node> */
    OStream_printf(out, "\t<%s>\n", TRACE_XML_NODE_TAG_STRING);  first_node = false;
    OStream_printf(out, "\t\t<%s id=\"%d\">\n", TRACE_XML_STATE_TAG_STRING,i);

    TRACE_STEP_FOREACH(trace, step, state_iter_type, iter, symb, val) {
      /* skip non-visible symbols */
      if (!trace_plugin_is_visible_symbol(self, symb)) continue;

      TracePlugin_print_assignment(self, symb, val);
    } /* foreach SF_SYMBOLS */

    OStream_printf(out, "\t\t</%s>\n", TRACE_XML_STATE_TAG_STRING);

    ++ i; step = TraceIter_get_next(step);
  } /* TRACE_FOR_EACH */

  /* <node> */
  OStream_printf(out,"\t</%s>\n", TRACE_XML_NODE_TAG_STRING);

  /* dumps loop info  */
  OStream_printf(out,"\t<%s> ", TRACE_XML_LOOPS_TAG_STRING);
  loops_iter=NodeList_get_first_iter(loops) ;
  if (!ListIter_is_end(loops_iter)) {
    do {
      OStream_printf(out, "%d ", NODE_TO_INT(NodeList_get_elem_at(loops, loops_iter)));

      loops_iter = ListIter_get_next(loops_iter);
      if (ListIter_is_end(loops_iter)) break;

      OStream_printf(out, ",");
    } while (true);
  }

  OStream_printf(out,"</%s>\n", TRACE_XML_LOOPS_TAG_STRING);
  OStream_printf(out,"</%s>\n", TRACE_XML_CNTX_TAG_STRING);

  NodeList_destroy(loops);

  return 0;
}

TraceXmlDumper_ptr TraceXmlDumper_create(boolean is_embedded)
{
  TraceXmlDumper_ptr self = ALLOC(TraceXmlDumper, 1);

  TRACE_XML_DUMPER_CHECK_INSTANCE(self);

  trace_xml_dumper_init(self, is_embedded);
  return self;
}


/* ---------------------------------------------------------------------- */
/*     Protected Methods                                                  */
/* ---------------------------------------------------------------------- */
/*!
  \brief 

  
*/

void trace_xml_dumper_print_symbol(TracePlugin_ptr self, node_ptr symb)
{
  const SymbTable_ptr st = trace_get_symb_table(self->trace);
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  OStream_ptr out = TraceOpt_output_stream(self->opt);
  char* symb_repr = \
    sprint_node(wffprint, NULL != self->anonymizer
                ? NodeAnonymizerBase_map_expr(self->anonymizer, symb)
                : symb);

  /* substituting XML entities */
  Utils_str_escape_xml_file(symb_repr, OStream_get_stream(out));
  FREE(symb_repr);
}

/*!
  \brief 

  
*/

void trace_xml_dumper_print_assignment(TracePlugin_ptr self,
                                       node_ptr symb, node_ptr val)
{
  OStream_ptr out = TraceOpt_output_stream(self->opt);

  OStream_printf(out, "\t\t\t<%s variable=\"", TRACE_XML_VALUE_TAG_STRING);

  TracePlugin_print_symbol(self, symb);
  OStream_printf(out, "\">");

  TracePlugin_print_list(self, val);
  OStream_printf(out, "</%s>\n", TRACE_XML_VALUE_TAG_STRING);
}


/*!
  \brief Class initializer

  
*/

void trace_xml_dumper_init(TraceXmlDumper_ptr self,
                           boolean is_embedded)
{
  if (is_embedded) {
    trace_plugin_init(TRACE_PLUGIN(self),"TRACE XML EMBEDDED DUMP PLUGIN - an xml element");
  }
  else {
    trace_plugin_init(TRACE_PLUGIN(self),"TRACE XML DUMP PLUGIN - an xml document");
  }

  /* virtual methods overriding: */
  OVERRIDE(Object, finalize) = trace_xml_dumper_finalize;
  OVERRIDE(TracePlugin, action) = trace_xml_dumper_action;
  OVERRIDE(TracePlugin, print_symbol) = trace_xml_dumper_print_symbol;
  OVERRIDE(TracePlugin, print_assignment) = trace_xml_dumper_print_assignment;

  self->is_embedded = is_embedded;
}


/*!
  \brief Deinitializes the TraceXmlDumper Plugin object.

  
*/

void trace_xml_dumper_deinit(TraceXmlDumper_ptr self)
{
  trace_plugin_deinit(TRACE_PLUGIN(self));
}



/* ---------------------------------------------------------------------- */
/*     Private Methods                                                    */
/* ---------------------------------------------------------------------- */

/*!
  \brief Plugin finalize method.

  
*/
static void trace_xml_dumper_finalize(Object_ptr object, void* dummy)
{
  TraceXmlDumper_ptr self = TRACE_XML_DUMPER(object);

  trace_xml_dumper_deinit(self);
  FREE(self);
}
