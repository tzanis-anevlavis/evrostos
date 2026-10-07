/* ---------------------------------------------------------------------------


  This file is part of the ``trace.loader'' package of NuSMV version 2.
  Copyright (C) 2010 by FBK-irst.

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
  \brief Routines related to TraceXmlLoader class

  This file contains the definition of TraceXmlLoader
               class.

*/

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#include "nusmv/core/utils/defs.h"

#include "nusmv/core/trace/loaders/TraceXmlLoader.h"
#include "nusmv/core/trace/loaders/TraceXmlLoader_private.h"
#include "nusmv/core/trace/Trace_private.h"

#include "nusmv/core/parser/parser.h"
#include "nusmv/core/parser/symbols.h"

#include <stdio.h>

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/* a few constants */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define state_section  "STATE"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define input_section  "INPUT"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define combo_section  "COMBINATORIAL"

/* strings identifying a category of a symbol */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define undecl_symb          "undeclared symbol"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define input_var            "input var"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define input_def            "input define"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define state_var            "state var"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define state_def            "state define"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define state_input_def      "state-input define"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define state_input_next_def "state-input-next define"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define invalid_undefined_symbol   0

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define invalid_wrong_section      1

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef void (*TagStartFunction_ptr)(void* , const xmlChar*, const xmlChar**);
typedef void (*TagEndFunction_ptr)(void* , const xmlChar*);
typedef void (*CharHandlerFunction_ptr)(void*, const xmlChar*, int);

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void trace_xml_loader_finalize(Object_ptr object, void* dummy);

static void trace_xml_loader_prepare(TraceXmlLoader_ptr self,
                                     const SymbTable_ptr st,
                                     const NodeList_ptr symbols);

static void trace_xml_loader_reset(TraceXmlLoader_ptr self);

static void
trace_xml_loader_tag_begin(TraceXmlLoader_ptr self,
                           const char* name, const char** atts);

static void trace_xml_loader_tag_end(TraceXmlLoader_ptr self,
                                     const char *name);

static void trace_xml_loader_char_handler(TraceXmlLoader_ptr self,
                                          const char *txt, int txtlen);

static void
trace_xml_report_invalid_assignment(TraceXmlLoader_ptr self,
                                    node_ptr symbol, int reason);
static inline int
trace_xml_load_put_expr(TraceXmlLoader_ptr self, node_ptr eq);

static inline node_ptr
trace_xml_loader_flatten_symbol(const NuSMVEnv_ptr env, node_ptr symbol);

static void trace_xml_loader_store_loopbacks(TraceXmlLoader_ptr self);

static xmlParserCtxtPtr trace_xml_parser_create(TraceXmlLoader_ptr t);
static void trace_xml_parser_free(xmlParserCtxtPtr p);

TraceXmlLoader_ptr TraceXmlLoader_create(const char* xml_filename,
                                         boolean halt_on_undefined_symbols,
                                         boolean halt_on_wrong_section)
{
  TraceXmlLoader_ptr self = ALLOC(TraceXmlLoader, 1);

  TRACE_XML_LOADER_CHECK_INSTANCE(self);

  trace_xml_loader_init(self, xml_filename,
                        halt_on_undefined_symbols, halt_on_wrong_section);
  return self;
}


/* ---------------------------------------------------------------------- */
/*   Protected Methods                                                    */
/* ---------------------------------------------------------------------- */

/*!
  \brief


*/

void trace_xml_loader_init(TraceXmlLoader_ptr self,
                           const char* xml_filename,
                           boolean halt_on_undefined_symbols,
                           boolean halt_on_wrong_section)
{
  trace_loader_init(TRACE_LOADER(self), "TRACE XML LOADER");

  /* pre-allocate current symbol buf */
  self->curr_symb = ALLOC(char, MAX_ID_LEN);
  nusmv_assert(NIL(char) != self->curr_symb);
  memset(self->curr_symb, 0, MAX_ID_LEN * sizeof(char));

  /* pre-allocate current value buf */
  self->curr_val = ALLOC(char, MAX_VL_LEN);
  nusmv_assert(NIL(char) != self->curr_val);
  memset(self->curr_val, 0, MAX_VL_LEN * sizeof(char));

  /* pre-allocate current equality buf */
  self->curr_eq = ALLOC(char, MAX_EQ_LEN);
  nusmv_assert(NIL(char) != self->curr_eq);
  memset(self->curr_eq, 0, MAX_EQ_LEN);

  /* pre-allocate libxml2 parser buf */
  self->stream_buf = ALLOC(char, LIBXML2_BUFSIZE);
  nusmv_assert(NIL(char) != self->stream_buf);
  memset(self->stream_buf, 0, LIBXML2_BUFSIZE * sizeof(char));

  self->nusmv_input_file = NIL(char);
  self->parser = (xmlParserCtxtPtr)(NULL);
  self->trace = TRACE(NULL);

  /* storing data from constructor parameters */
  self->xml_filename = ALLOC(char, strlen(xml_filename) + 1);
  nusmv_assert(self->xml_filename != (char*) NULL);
  strncpy(self->xml_filename, xml_filename, strlen(xml_filename) + 1);

  self->halt_on_undefined_symbols = halt_on_undefined_symbols;
  self->halt_on_wrong_section =  halt_on_wrong_section;

  /* virtual methods overriding: */
  OVERRIDE(Object, finalize) = trace_xml_loader_finalize;
  OVERRIDE(TraceLoader, load) = trace_xml_loader_load;
}


/*!
  \brief Deallocates internal structures


*/

void trace_xml_loader_deinit(TraceXmlLoader_ptr self)
{
  if (self->parser != (xmlParserCtxtPtr) NULL) {
    trace_xml_parser_free(self->parser);
  }

  FREE(self->curr_symb);
  FREE(self->curr_val);
  FREE(self->curr_eq);
  FREE(self->stream_buf);
  FREE(self->xml_filename);

  trace_loader_deinit(TRACE_LOADER(self));
}


/*!
  \brief Read the trace from the XML file

  Returns a valid trace
*/

Trace_ptr trace_xml_loader_load(TraceLoader_ptr loader,
                                const SymbTable_ptr st,
                                const NodeList_ptr symbols)
{
  FILE* stream;

  const TraceXmlLoader_ptr self = TRACE_XML_LOADER(loader);
  nusmv_assert(TRACE(NULL) == self->trace);

  stream = fopen(self->xml_filename, "rt");
  if (NIL(FILE) != stream) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
    const StreamMgr_ptr streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

    /* inits the parser */
    trace_xml_loader_prepare(self, st, symbols);

    CATCH(errmgr) {
      /* parses the XML file */
      do {
        size_t len;
        boolean ok;
        int error_status;

        /* read one chunk of data */
        len = fread(self->stream_buf, sizeof(char), LIBXML2_BUFSIZE, stream);

        if (ferror(stream)) {
          StreamMgr_print_error(streams,
                  "I/O Error occurred while reading file '%s'\n",
                  self->xml_filename);

          trace_destroy(self->trace); self->trace = TRACE(NULL);
          break;
        }

        /* done with I/O, launches the parser */
        CATCH(errmgr) {
          error_status = xmlParseChunk(self->parser, self->stream_buf, len,
                                       len == 0);
          ok = !error_status && (!self->parse_error);
        }
        FAIL(errmgr) { ok = false; }

        if (!ok) {
          xmlErrorPtr err = xmlCtxtGetLastError(self->parser);

          if ((xmlErrorPtr)NULL != err) {
            const char* msg = err->message;
            int line = err->line;

            /* an error occurred */
            if (msg != (const char*) NULL) {
              StreamMgr_print_error(streams, "At line %d: %s",
                                    line, msg);
            }
          }

          trace_destroy(self->trace); self->trace = TRACE(NULL);
          break;
        }
      } while (!feof(stream));

      /* store loopback information into the new trace and freeze it */
      if (TRACE(NULL) != self->trace) {
        trace_xml_loader_store_loopbacks(self);
      }
    }

    /* catch any error silently */
    FAIL(errmgr) { }

    /* shuts down the parser */
    trace_xml_loader_reset(self);

    nusmv_assert(NIL(FILE) != stream);
    fclose(stream);
  } /* NIL(FILE) != stream */

  /* traces produced by the XML trace loader are frozen */
  nusmv_assert(TRACE(NULL) == self->trace || trace_is_frozen(self->trace));

  return self->trace;
}


/* ---------------------------------------------------------------------- */
/*     Private Methods                                                    */
/* ---------------------------------------------------------------------- */

/*!
  \brief Virtual destructor


*/
static void trace_xml_loader_finalize(Object_ptr object, void* dummy)
{
  TraceXmlLoader_ptr self = TRACE_XML_LOADER(object);

  trace_xml_loader_deinit(self);
  FREE(self);
}

/*!
  \brief


*/
static void trace_xml_loader_prepare(TraceXmlLoader_ptr self,
                                     const SymbTable_ptr st,
                                     const NodeList_ptr symbols)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  OptsHandler_ptr opts;

  self->environment = env;

  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  /* setup XML parser based on libxml2 library */
  nusmv_assert(self->parser == (xmlParserCtxtPtr) NULL);

  self->parser = trace_xml_parser_create(self);
  nusmv_assert(self->parser != (xmlParserCtxtPtr) NULL);

  self->parse_error = false;

  /* creates a new trace, and prepares internal structures */
  self->trace = Trace_create(st, "(no description available)",
                             TRACE_TYPE_UNSPECIFIED, symbols, false);

  self->step = trace_first_iter(self->trace);
  self->last_time = 1; /* initial time */
  self->requires_value = false;
  self->all_wrong_symbols = new_assoc();
  self->loopback_states = NodeList_create();

  /* backup nusmv parser internal information */
  self->nusmv_yylineno = nusmv_yylineno;
  nusmv_assert(NIL(char) == self->nusmv_input_file);
  self->nusmv_input_file = \
    util_strsav(get_input_file(opts));

  /* try to provide more informative messages upon parsing errors */
  nusmv_yylineno = -1;
  set_input_file(opts, self->xml_filename);

  if (opt_verbose_level_ge(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "xml parser ready\n");
  }
}

/*!
  \brief Cleans up after reading of xml source


*/
static void trace_xml_loader_reset(TraceXmlLoader_ptr self)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(self->environment, ENV_OPTS_HANDLER));

  if (opt_verbose_level_ge(opts, 4)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(self->environment, ENV_LOGGER));
    Logger_log(logger, "xml parser reset\n");
  }

  self->environment = NUSMV_ENV(NULL);

  self->curr_parsing = TRACE_XML_INVALID_TAG;
  self->parse_error = false;

  memset(self->stream_buf, 0, LIBXML2_BUFSIZE * sizeof(char));

  nusmv_assert(self->parser != (xmlParserCtxtPtr) NULL);
  trace_xml_parser_free(self->parser);
  self->parser = (xmlParserCtxtPtr) NULL;

  free_assoc(self->all_wrong_symbols);
  self->all_wrong_symbols = (hash_ptr)(NULL);

  /* restore parser internal information */
  nusmv_yylineno = self->nusmv_yylineno;
  set_input_file(opts, self->nusmv_input_file);
  FREE(self->nusmv_input_file); self->nusmv_input_file = NIL(char);


}

/*!
  \brief Store loopback information and freezes loaded trace


*/
static void trace_xml_loader_store_loopbacks(TraceXmlLoader_ptr self)
{
  ListIter_ptr liter;

  Trace_freeze(self->trace); /* loopbacks can be added only to frozen traces */
  NODE_LIST_FOREACH(self->loopback_states, liter) {
    TraceIter loop_iter = trace_ith_iter(self->trace,
                NODE_TO_INT(NodeList_get_elem_at(self->loopback_states, liter)));

    Trace_step_force_loopback(self->trace, loop_iter);
  }
}

/*!
  \brief Function that gets called when parser encounter start of
                    some tag.


*/
static void
trace_xml_loader_tag_begin(TraceXmlLoader_ptr self,
                           const char* name, const char** atts)
{
  int i, time = self->last_time;

  /* clear buffers */
  memset(self->curr_symb, 0, MAX_ID_LEN * sizeof(char));
  memset(self->curr_val, 0, MAX_VL_LEN * sizeof(char));

  /* By default, tags do not have a text value. See specific tags for
     exceptions (e.g. TRACE_XML_VALUE_TAG) */
  self->requires_value = false;

  switch (TraceXmlTag_from_string(name)) {

  case TRACE_XML_CNTX_TAG:
    /* Attributes. */
    for (i = 0; atts[i]; i += 2) {

      if (! strncmp("type", atts[i], 4)) {
        trace_set_type(self->trace, atoi(atts[i+1]));
      }

      if (! strncmp("desc", atts[i], 4))  {
        trace_set_desc(self->trace, atts[i+1]);
      }
    }
    break;

    /*  deprecated tag, we plainly ignore it */
  case TRACE_XML_NODE_TAG:
    break;

  case TRACE_XML_STATE_TAG:
    /* Attributes. */
    for (i = 0; atts[i]; i += 2) {
      if (! strncmp("id", atts[i], 2)) {
        time = atoi(atts[i+1]);
      }
    }
    self->curr_parsing = TRACE_XML_STATE_TAG;
    break;

  case TRACE_XML_COMB_TAG:
     /* Attributes. */
    for (i = 0; atts[i]; i += 2) {
      if (! strncmp("id", atts[i], 2)) {
        time = atoi(atts[i+1]);
      }
    }
    self->curr_parsing = TRACE_XML_COMB_TAG;
    break;

  case TRACE_XML_INPUT_TAG:
     /* Attributes. */
    for (i = 0; atts[i]; i += 2) {
      if (! strncmp("id", atts[i], 2)) {
        time = atoi(atts[i+1]);
      }
    }
    self->curr_parsing = TRACE_XML_INPUT_TAG;
    break;

  case TRACE_XML_VALUE_TAG:
    /* This tag requires a text value to be read */
    self->requires_value = true;

    for (i = 0; atts[i]; i += 2) {
      if (! strncmp("variable", atts[i], 8)) {
        strncpy(self->curr_symb, atts[i+1], MAX_ID_LEN);
      }
    }
    break;

  case TRACE_XML_LOOPS_TAG:
    /* This tag requires a text value to be read */
    self->requires_value = true;
    self->curr_parsing = TRACE_XML_LOOPS_TAG;
    break;

  case TRACE_XML_INVALID_TAG:
    {
      const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->trace));
      const StreamMgr_ptr streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

      StreamMgr_print_error(streams, "Invalid TAG : <%s> Encountered in XML File\n", name);
      self->parse_error = true;
      return;
    }

  default:
    {
      const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->trace));
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

      /* unknown tag */
      ErrorMgr_internal_error(errmgr, "%s:%d:%s: trace_xml_loader_tag_begin: unknown tag '%s'\n",
                              __FILE__, __LINE__, __func__, name);
    }
  }

  /* time must be monotonic not-decreasing */
  if (time < self->last_time) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->trace));
    const StreamMgr_ptr streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

    StreamMgr_print_error(streams, "Invalid time : <%d> detected in XML File\n", time);
    self->parse_error = true;
  }

  /* add more steps as required, this implementation allows "holes" in
     the XML traces managing them correctly. Entire sections can be
     missing. Naturally, such traces are partial. */
  while (time > self->last_time) {

    ++ self->last_time;
    self->step = (TRACE_END_ITER != trace_iter_get_next(self->step))
      ? trace_iter_get_next(self->step)
      : trace_append_step(self->trace);
  }
} /* trace_xml_loader_tag_begin */

/*!
  \brief Function that gets called when end of any tag is
                   encountered by the parser.


*/
static void trace_xml_loader_tag_end(TraceXmlLoader_ptr self, const char *name)
{
  node_ptr parsed;

  switch (TraceXmlTag_from_string(name)) {
  case TRACE_XML_CNTX_TAG:
    break;

  case TRACE_XML_NODE_TAG:
    break;

  case TRACE_XML_STATE_TAG:
    self->curr_parsing = TRACE_XML_INVALID_TAG;
    break;

  case TRACE_XML_COMB_TAG:
    self->curr_parsing = TRACE_XML_INVALID_TAG;
    break;

  case TRACE_XML_INPUT_TAG:
    self->curr_parsing = TRACE_XML_INVALID_TAG;
    break;

  case TRACE_XML_VALUE_TAG:
    if (!self->parse_error) { /* if already in error state do nothing. */
      /* an hack to support non-deterministic assignments in traces
         (see issue #1802) */
      if (NIL(char) == strchr(self->curr_val, ',')) {
        int c = snprintf(self->curr_eq, MAX_EQ_LEN, "%s = %s", self->curr_symb, self->curr_val);
        SNPRINTF_CHECK(c, MAX_EQ_LEN);
      }
      else {
        int c = snprintf(self->curr_eq, MAX_EQ_LEN, "%s in { %s }", self->curr_symb, self->curr_val);
        SNPRINTF_CHECK(c, MAX_EQ_LEN);
      }

      if (0 == Parser_ReadSimpExprFromString(self->environment, self->curr_eq, &parsed)) {
        const NodeMgr_ptr nodemgr =
          NODE_MGR(NuSMVEnv_get_value(self->environment, ENV_NODE_MGR));

        if (0 != trace_xml_load_put_expr(self, cdar(parsed))) {
          self->parse_error = true;
        }
        free_node(nodemgr, parsed);
      } else self->parse_error = true;
    }
    break;

    case TRACE_XML_LOOPS_TAG:
      {
        const char COMMA_CHR = ',';
        const char TERM_CHR = '\0';

        char* p0 = self->curr_val;
        char *p = p0;
        char* q;

        /* the loopbacks list is initially empty */
        nusmv_assert(0 == NodeList_get_length(self->loopback_states));
        do { /* iterate over a comma separated list */
          int node_idx;
          if ((q=strchr(p, COMMA_CHR))) {
            /* side effects on p */
            (*q) = TERM_CHR; q++;
          }

          node_idx = atoi(p);
          if (0 < node_idx) { /* state indexing starts at 1 */
            NodeList_append(self->loopback_states,
                            PTR_FROM_INT(node_ptr,node_idx ));
          }

          p=q;
        } while (p); /* is there anything left to parse? */

        break;
      } /* XML LOOPS TAG */

    case TRACE_XML_INVALID_TAG:
      self->parse_error = true;

  default:
    {
      SymbTable_ptr st = trace_get_symb_table(self->trace);
      const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

      /* unknown tag */
      ErrorMgr_internal_error(errmgr, "trace_xml_loader_tag_end: unknown tag '%s'\n", name);
    }
  } /* switch */

}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static inline int trace_xml_load_put_expr(TraceXmlLoader_ptr self, node_ptr eq)
{
  NuSMVEnv_ptr env;
  SymbTable_ptr st;
  TraceSection section;
  unsigned dummy;
  node_ptr symb, value;
  int time_ofs = 0;
  MasterNormalizer_ptr normalizer;

  nusmv_assert(EQUAL == node_get_type(eq) || SETIN == node_get_type(eq));
  st = trace_get_symb_table(self->trace);

  env = EnvObject_get_environment(ENV_OBJECT(st));
  normalizer = MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));

  /* here Compile_FlattenSexp cannot be used, because we need */
  /* to avoid rewriting for arrays (see issue #1243, note 2064) */
  symb = trace_xml_loader_flatten_symbol(env, car(eq));
  value = MasterNormalizer_normalize_node(normalizer, Compile_FlattenSexp(st, cdr(eq), Nil));

  if (trace_symbol_fwd_lookup(self->trace, symb, &section, &dummy)) {

    switch (section) {
      /* state */
    case TRACE_SECTION_FROZEN_VAR:
    case TRACE_SECTION_STATE_VAR:
    case TRACE_SECTION_STATE_DEFINE:
      if (self->curr_parsing != TRACE_XML_STATE_TAG) {

        if (self->curr_parsing == TRACE_XML_INPUT_TAG || \
            self->curr_parsing == TRACE_XML_COMB_TAG) { time_ofs = +1; }

        trace_xml_report_invalid_assignment(self, symb, invalid_wrong_section);
        if (self->halt_on_wrong_section) return 1;
      }
      break;

      /* input */
    case TRACE_SECTION_INPUT_VAR:
    case TRACE_SECTION_INPUT_DEFINE:
      if (self->curr_parsing != TRACE_XML_INPUT_TAG) {

        if (self->curr_parsing == TRACE_XML_STATE_TAG) { time_ofs = -1; }

        trace_xml_report_invalid_assignment(self, symb, invalid_wrong_section);
        if (self->halt_on_wrong_section) return 1;
      }
      break;

      /* combinatorials */
    case TRACE_SECTION_STATE_INPUT_DEFINE:
    case TRACE_SECTION_NEXT_DEFINE:
    case TRACE_SECTION_INPUT_NEXT_DEFINE:
    case TRACE_SECTION_STATE_NEXT_DEFINE:
    case TRACE_SECTION_STATE_INPUT_NEXT_DEFINE:
      if (self->curr_parsing !=  TRACE_XML_COMB_TAG) {

        if (self->curr_parsing == TRACE_XML_STATE_TAG) { time_ofs = -1; }

        trace_xml_report_invalid_assignment(self, symb, invalid_wrong_section);
        if (self->halt_on_wrong_section) return 1;
      }
      break;

    default:  error_unreachable_code(); /* unreachable */
    } /* switch */

    { /* time adjusting */
      TraceIter step = TRACE_END_ITER;

      if (0 < time_ofs) { /* move backward */
        step = trace_iter_get_prev(self->step);
      }
      else if (time_ofs < 0) { /* move forward */
        step = trace_iter_get_next(self->step);
        if (TRACE_END_ITER == step) { step = trace_append_step(self->trace); }
      }
      else { step = self->step; } /* no time adjusting necessary */

     nusmv_assert(TRACE_END_ITER != step);
     if (!trace_step_put_value(self->trace, step, symb, value)) {
        return 1; /* put value reported a type error */
      }
    }
  } /* lookup */

  else  {
    trace_xml_report_invalid_assignment(self, symb, invalid_undefined_symbol);
    if (self->halt_on_undefined_symbols) return 1;
  }

  return 0; /* success */
} /* trace_xml_load_put_expr */

/*!
  \brief Character Handler used by parser.


*/
static void trace_xml_loader_char_handler(TraceXmlLoader_ptr self,
                                          const char *txt, int txtlen)
{
  /* There is no need for reading these chars.. Ignore them */
  if (!self->requires_value) return;

  strncat(self->curr_val, txt, txtlen);
  nusmv_assert(strlen(self->curr_val)  < MAX_VL_LEN);
}

/*!
  \brief Private service of trace_xml_loader_fill_trace

  Private service of trace_xml_loader_fill_trace

  \sa trace_xml_loader_fill_trace
*/
static inline node_ptr
trace_xml_loader_flatten_symbol(const NuSMVEnv_ptr env, node_ptr symbol)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const MasterNormalizer_ptr normalizer =
    MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));

  int op;

  op = node_get_type(symbol);
  if (op == DOT && car(symbol) == Nil) {
    return MasterNormalizer_normalize_node(normalizer, find_node(nodemgr, DOT, Nil, cdr(symbol)));
  }

  if (op == ATOM) {
    return MasterNormalizer_normalize_node(normalizer, find_node(nodemgr, DOT, Nil, symbol));
  }

  if (op == ARRAY) {
    nusmv_assert(Nil != cdr(symbol));

    /* Indexes in array are not parsed correctly: The unary minus node
       is left (e.g. UMINUS NUMBER <positive_number>) instead of
       having NUMBER <negative_number>. Fix the tree here */
    if (UMINUS == node_get_type(cdr(symbol))) {
      symbol = find_node(nodemgr, op, car(symbol),
                         ExprMgr_unary_minus(exprs, car(cdr(symbol))));
    }
  }

  return find_node(nodemgr, op,
                   trace_xml_loader_flatten_symbol(env, car(symbol)),
                   MasterNormalizer_normalize_node(normalizer, cdr(symbol)));
}

/*!
  \brief This function reports an error/warning message when LHS of
               an assignment is found to be in invalid section

  Here assignment is invalid if LHS is undefined symbol
               or a symbol which is inconsistent with a given section,
               e.g. input var cannot be in STATE section.

               'all_wrong_symbols' is a hash to remember already
               reported symbols and report them only once. 'isError'
               is a flag to report an error; warning is reported
               otherwise

               The returned value is isError argument.

  \sa trace_xml_loader_fill_trace
*/
static void trace_xml_report_invalid_assignment(TraceXmlLoader_ptr self,
                                                node_ptr symbol, int reason)
{
  const NuSMVEnv_ptr env = self->environment;
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);

  const char* cat_repr = \
    (trace_symbol_in_language(self->trace, symbol))
    ? trace_symb_category_to_string(trace_symbol_get_category(self->trace,
                                                              symbol))
    : ""; /* should never see this */

  if (Nil == find_assoc(self->all_wrong_symbols, symbol)) {

    if (invalid_wrong_section == reason) {
      StreamMgr_print_error(streams, "%s: %s '", (self->halt_on_wrong_section)
              ? "Error" : "Warning", cat_repr);
    }
    else if (invalid_undefined_symbol == reason) {
      StreamMgr_print_error(streams, "%s: undefined symbol '",
              (self->halt_on_undefined_symbols)
              ? "Error" : "Warning");
    }
    else error_unreachable_code(); /* unreachable */

    print_node(wffprint, errstream, symbol);
    StreamMgr_print_error(streams, "' is in section %s (time %d).\n",
            TraceXmlTag_to_string (self->curr_parsing), self->last_time);

    insert_assoc(self->all_wrong_symbols, symbol, NODE_FROM_INT(true));
    StreamMgr_print_error(streams, "(Each symbol is reported only once)\n\n");
  } /* Nil == find_assoc */
}

/*!
  \brief Create the libxml2 parser

  Creates a Parser Context and tells it to use SAX
                      interface passing it a sax handler
*/
static xmlParserCtxtPtr trace_xml_parser_create(TraceXmlLoader_ptr self)
{
  xmlParserCtxtPtr parser;
  xmlSAXHandlerPtr sax;

  sax = ALLOC(xmlSAXHandler, 1);
  memset((void*)sax, 0, sizeof(xmlSAXHandler));

  sax->startElement = (TagStartFunction_ptr)trace_xml_loader_tag_begin;
  sax->endElement = (TagEndFunction_ptr)trace_xml_loader_tag_end;
  sax->characters =
    (CharHandlerFunction_ptr)trace_xml_loader_char_handler;

  parser = xmlCreatePushParserCtxt(sax, self, (char*)NULL, 0, (char*)NULL);

  FREE(sax);
  return parser;
}

/*!
  \brief Free the libxml2 parser


*/
static void trace_xml_parser_free(xmlParserCtxtPtr parser)
{
  FREE(parser->sax);
  xmlFreeParserCtxt(parser);
}
