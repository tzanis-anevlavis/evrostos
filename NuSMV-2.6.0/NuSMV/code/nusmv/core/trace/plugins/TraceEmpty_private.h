/* ---------------------------------------------------------------------------


  %COPYRIGHT%

-----------------------------------------------------------------------------*/

/*!
  \author Michele Dorigatti
  \brief Private and protected interface of class 'TraceEmpty'

  This file can be included only by derived and friend classes

*/



#ifndef __NUSMV_CORE_TRACE_PLUGINS_TRACE_EMPTY_PRIVATE_H__
#define __NUSMV_CORE_TRACE_PLUGINS_TRACE_EMPTY_PRIVATE_H__


#include "nusmv/core/trace/plugins/TraceEmpty.h"
#include "nusmv/core/trace/plugins/TracePlugin.h"
#include "nusmv/core/trace/plugins/TracePlugin_private.h"
#include "nusmv/core/utils/defs.h"


/*!
  \brief TraceEmpty class definition derived from
               class TracePlugin

  

  \sa Base class TracePlugin
*/

typedef struct TraceEmpty_TAG
{
  /* this MUST stay on the top */
  INHERITS_FROM(TracePlugin);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */


  /* -------------------------------------------------- */
  /*                  Virtual methods                   */
  /* -------------------------------------------------- */

} TraceEmpty;



/* ---------------------------------------------------------------------- */
/* Private methods to be used by derivated and friend classes only         */
/* ---------------------------------------------------------------------- */

/*!
  \methodof TraceEmpty
  \brief The TraceEmpty class private initializer

  The TraceEmpty class private initializer

  \sa TraceEmpty_create
*/
void trace_empty_init(TraceEmpty_ptr self);

/*!
  \methodof TraceEmpty
  \brief The TraceEmpty class private deinitializer

  The TraceEmpty class private deinitializer

  \sa TraceEmpty_destroy
*/
void trace_empty_deinit(TraceEmpty_ptr self);

/*!
  \methodof TraceEmpty
  \brief Action method associated with this class

  Does nothing.
  Useful when it is not possible to control directly the call to execute
  plugin
*/
int trace_empty_action(const TracePlugin_ptr self);


#endif /* __NUSMV_CORE_TRACE_PLUGINS_TRACE_EMPTY_PRIVATE_H__ */
