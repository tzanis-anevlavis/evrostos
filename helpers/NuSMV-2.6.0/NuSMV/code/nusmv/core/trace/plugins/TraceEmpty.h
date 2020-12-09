/* ---------------------------------------------------------------------------


  %COPYRIGHT%

-----------------------------------------------------------------------------*/

/*!
  \author Michele Dorigatti
  \brief Public interface of class 'TraceEmpty'

  \todo: Missing description

*/



#ifndef __NUSMV_CORE_TRACE_PLUGINS_TRACE_EMPTY_H__
#define __NUSMV_CORE_TRACE_PLUGINS_TRACE_EMPTY_H__


#include "nusmv/core/trace/plugins/TracePlugin.h"
#include "nusmv/core/utils/defs.h"

/*!
  \struct TraceEmpty
  \brief Definition of the public accessor for class TraceEmpty

  
*/
typedef struct TraceEmpty_TAG*  TraceEmpty_ptr;

/*!
  \brief To cast and check instances of class TraceEmpty

  These macros must be used respectively to cast and to check
  instances of class TraceEmpty
*/
#define TRACE_EMPTY(self) \
         ((TraceEmpty_ptr) self)

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define TRACE_EMPTY_CHECK_INSTANCE(self) \
         (nusmv_assert(TRACE_EMPTY(self) != TRACE_EMPTY(NULL)))



/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \methodof TraceEmpty
  \brief The TraceEmpty class constructor

  The TraceEmpty class constructor

  \sa TraceEmpty_destroy
*/
TraceEmpty_ptr TraceEmpty_create(void);

/*!
  \methodof TraceEmpty
  \brief The TraceEmpty class destructor

  The TraceEmpty class destructor

  \sa TraceEmpty_create
*/
void TraceEmpty_destroy(TraceEmpty_ptr self);


/**AutomaticEnd***************************************************************/



#endif /* __NUSMV_CORE_TRACE_PLUGINS_TRACE_EMPTY_H__ */
