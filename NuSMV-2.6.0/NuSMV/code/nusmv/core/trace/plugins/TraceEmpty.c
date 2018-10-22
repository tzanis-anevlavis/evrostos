/* ---------------------------------------------------------------------------


  %COPYRIGHT%

-----------------------------------------------------------------------------*/

/*!
  \author Michele Dorigatti
  \brief Implementation of class 'TraceEmpty'

  \todo: Missing description

*/


#include "nusmv/core/trace/plugins/TraceEmpty.h"
#include "nusmv/core/trace/plugins/TraceEmpty_private.h"


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'TraceEmpty_private.h' for class 'TraceEmpty' definition. */

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

static void trace_empty_finalize(Object_ptr object, void* dummy);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

TraceEmpty_ptr TraceEmpty_create(void)
{
  TraceEmpty_ptr self = ALLOC(TraceEmpty, 1);
  TRACE_EMPTY_CHECK_INSTANCE(self);

  trace_empty_init(self);
  return self;
}

void TraceEmpty_destroy(TraceEmpty_ptr self)
{
  TRACE_EMPTY_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

int trace_empty_action(const TracePlugin_ptr self)
{
  UNUSED_PARAM(self);
  
  return 0;
}

void trace_empty_init(TraceEmpty_ptr self)
{
  /* base class initialization */
  trace_plugin_init(TRACE_PLUGIN(self), "Empty Trace Plugin");

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = trace_empty_finalize;
  OVERRIDE(TracePlugin, action) = trace_empty_action;

  /* for example, to override a base class' virtual method: */
  /*OVERRIDE(TracePlugin, virtual_method) = trace_empty_virtual_method;*/
}

void trace_empty_deinit(TraceEmpty_ptr self)
{
  /* members deinitialization */


  /* base class deinitialization */
  trace_plugin_deinit(TRACE_PLUGIN(self));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The TraceEmpty class virtual finalizer

  Called by the class destructor
*/
static void trace_empty_finalize(Object_ptr object, void* dummy)
{
  TraceEmpty_ptr self = TRACE_EMPTY(object);

  UNUSED_PARAM(dummy);

  trace_empty_deinit(self);
  FREE(self);
}



/**AutomaticEnd***************************************************************/

