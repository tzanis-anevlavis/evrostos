/* ---------------------------------------------------------------------------


  This file is part of the ``trace'' package of NuSMV version 2.
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
  \author Alessandro Mariotti, Marco Pensallorto
  \brief Implementation of class 'TraceOpt'

  \todo: Missing description

*/


#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/trace/TraceOpt.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/trace/pkg_traceInt.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct TraceOpt_TAG
{
  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  StreamMgr_ptr streams;

  boolean obfuscate;
  boolean show_defines;
  boolean show_defines_with_next;

  unsigned from_here;
  unsigned to_here;

  OStream_ptr output_stream;
  char* hiding_prefix;

#if NUSMV_HAVE_REGEX_H
  regex_t* regexp;
#endif
} TraceOpt;



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

static void trace_opt_init(TraceOpt_ptr self, StreamMgr_ptr streams);
static void trace_opt_deinit(TraceOpt_ptr self);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

TraceOpt_ptr TraceOpt_create(StreamMgr_ptr streams)
{
  TraceOpt_ptr self = ALLOC(TraceOpt, 1);
  TRACE_OPT_CHECK_INSTANCE(self);

  trace_opt_init(self, streams);
  return self;
}

TraceOpt_ptr TraceOpt_create_from_env(const NuSMVEnv_ptr env)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  TraceOpt_ptr self = TraceOpt_create(streams);

  TRACE_OPT_CHECK_INSTANCE(self);

  TraceOpt_update_from_env(self, env);
  return self;
}

void TraceOpt_update_from_env(TraceOpt_ptr self, const NuSMVEnv_ptr env)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  TRACE_OPT_CHECK_INSTANCE(self);

  /* We don't own the FILE*, so we need to reset it to avoid OStream_set_stream
     to close or flush it! */
  (void)OStream_reset_stream(self->output_stream);
  OStream_set_stream(self->output_stream,
                     StreamMgr_get_output_stream(streams));

  self->show_defines = opt_show_defines_in_traces(opts);
  self->show_defines_with_next = opt_backward_comp(opts) ?
    false : opt_show_defines_with_next(opts);
  self->hiding_prefix = (char*)opt_traces_hiding_prefix(opts);

#if NUSMV_HAVE_REGEX_H
    {
      const char* pattern = opt_traces_regexp(opts);

      /* free previous regexp contents if any */
      if ((regex_t *)(NULL) != self->regexp) {
        regfree(self->regexp); FREE(self->regexp);
        self->regexp = (regex_t*)(NULL);
      }

      /* replace regexp if any has been defined */
      if (NIL(char) != pattern) {
        self->regexp = ALLOC(regex_t, 1);
        if (0 != regcomp(self->regexp, pattern, REG_EXTENDED|REG_NOSUB)) {
          ErrorMgr_internal_error(errmgr, "%s:%d:%s: processing regular expression: %s",
                         __FILE__, __LINE__, __func__, pattern);
        }
      }
    }
#endif
}

void TraceOpt_destroy(TraceOpt_ptr self)
{
  TRACE_OPT_CHECK_INSTANCE(self);

  trace_opt_deinit(self);
  FREE(self);
}

boolean TraceOpt_obfuscate(TraceOpt_ptr self)
{
  TRACE_OPT_CHECK_INSTANCE(self);
  return self->obfuscate;
}

void TraceOpt_set_obfuscate(TraceOpt_ptr self, boolean obfuscate)
{
  TRACE_OPT_CHECK_INSTANCE(self);
  self->obfuscate = obfuscate;
}

unsigned TraceOpt_from_here(TraceOpt_ptr self)
{
  TRACE_OPT_CHECK_INSTANCE(self);
  return self->from_here;
}

void TraceOpt_set_from_here(TraceOpt_ptr self, unsigned index)
{
  TRACE_OPT_CHECK_INSTANCE(self);
  self->from_here = index;
}

unsigned TraceOpt_to_here(TraceOpt_ptr self)
{
  TRACE_OPT_CHECK_INSTANCE(self);
  return self->to_here;
}

void TraceOpt_set_to_here(TraceOpt_ptr self, unsigned index)
{
  TRACE_OPT_CHECK_INSTANCE(self);
  self->to_here = index;
}

OStream_ptr TraceOpt_output_stream(TraceOpt_ptr self)
{
  TRACE_OPT_CHECK_INSTANCE(self);

  return self->output_stream;
}

void TraceOpt_set_output_stream(TraceOpt_ptr self, FILE* out)
{
  TRACE_OPT_CHECK_INSTANCE(self);

  /* We don't own the FILE*, so we need to reset it to avoid OStream_set_stream
     to close or flush it! */
  (void)OStream_reset_stream(self->output_stream);
  OStream_set_stream(self->output_stream, out);
}

boolean TraceOpt_show_defines(TraceOpt_ptr self)
{
  TRACE_OPT_CHECK_INSTANCE(self);
  return self->show_defines;
}

void TraceOpt_set_show_defines(TraceOpt_ptr self, boolean show_defines)
{
  TRACE_OPT_CHECK_INSTANCE(self);
  self->show_defines = show_defines;
}

boolean TraceOpt_show_defines_with_next(TraceOpt_ptr self)
{
  TRACE_OPT_CHECK_INSTANCE(self);
  return self->show_defines_with_next;
}

void TraceOpt_set_show_defines_with_next(TraceOpt_ptr self, boolean show_defines_with_next)
{
  TRACE_OPT_CHECK_INSTANCE(self);
  self->show_defines_with_next = show_defines_with_next;
}

const char* TraceOpt_hiding_prefix(TraceOpt_ptr self)
{
  TRACE_OPT_CHECK_INSTANCE(self);
  return self->hiding_prefix;
}

void TraceOpt_set_hiding_prefix(TraceOpt_ptr self, const char* hiding_prefix)
{
  TRACE_OPT_CHECK_INSTANCE(self);
  self->hiding_prefix = util_strsav(hiding_prefix);
}


#if NUSMV_HAVE_REGEX_H

regex_t* TraceOpt_regexp(TraceOpt_ptr self)
{
  TRACE_OPT_CHECK_INSTANCE(self);
  return self->regexp;
}

#endif

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The TraceOpt class private initializer

  The TraceOpt class private initializer

  \sa TraceOpt_create
*/
static void trace_opt_init(TraceOpt_ptr self, StreamMgr_ptr streams)
{
  /* members initialization */

  self->streams = streams;
  self->obfuscate = false;
  self->show_defines = true;
  self->show_defines_with_next = true;

  self->from_here = 0;
  self->to_here = 0;

  self->output_stream = OStream_create(StreamMgr_get_output_stream(streams));
  self->hiding_prefix = (char*)NULL;

#if NUSMV_HAVE_REGEX_H
  self->regexp = (regex_t*)NULL;
#endif
}

/*!
  \brief The TraceOpt class private deinitializer

  The TraceOpt class private deinitializer

  \sa TraceOpt_destroy
*/
static void trace_opt_deinit(TraceOpt_ptr self)
{
  /* members deinitialization */
  if ((char*)NULL == self->hiding_prefix) {
    FREE(self->hiding_prefix);
  }

  OStream_destroy_safe(self->output_stream);
}



/**AutomaticEnd***************************************************************/

