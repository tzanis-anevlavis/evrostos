/* ---------------------------------------------------------------------------


  This file is part of the ``core.node.anonymizers'' package of NuSMV version 2.
  Copyright (C) 2014 by FBK-irst.

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
  \brief Implementation of class 'NodeAnonymizerDot'

  This class recognized as identifiers the DOT nodes and ATOM
  nodes. A DOT expression will not be traversed. This anonymizer could be used
  over a flattened model, but NOT over a non flattened one.  The identification
  is purely syntactical

*/


#include "nusmv/core/node/anonymizers/NodeAnonymizerDot.h"
#include "nusmv/core/node/anonymizers/NodeAnonymizerDot_private.h"
#include "nusmv/core/parser/symbols.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'NodeAnonymizerDot_private.h' for class 'NodeAnonymizerDot' definition. */

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

static void node_anonymizer_dot_finalize(Object_ptr object, void* dummy);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

NodeAnonymizerDot_ptr NodeAnonymizerDot_create(NuSMVEnv_ptr env,
                                               const char* default_prefix,
                                               size_t memoization_threshold)
{
  NodeAnonymizerDot_ptr self = ALLOC(NodeAnonymizerDot, 1);
  NODE_ANONYMIZER_DOT_CHECK_INSTANCE(self);

  node_anonymizer_dot_init(self, env, default_prefix, memoization_threshold);
  return self;
}

void NodeAnonymizerDot_destroy(NodeAnonymizerDot_ptr self)
{
  NODE_ANONYMIZER_DOT_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void node_anonymizer_dot_init(NodeAnonymizerDot_ptr self,
                              NuSMVEnv_ptr env,
                              const char* default_prefix,
                              size_t memoization_threshold)
{
  /* base class initialization */
  node_anonymizer_base_init(NODE_ANONYMIZER_BASE(self), env, default_prefix,
                            memoization_threshold);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = node_anonymizer_dot_finalize;
  OVERRIDE(NodeAnonymizerBase, is_id) = node_anonymizer_dot_is_id;
}

void node_anonymizer_dot_deinit(NodeAnonymizerDot_ptr self)
{
  /* members deinitialization */


  /* base class deinitialization */
  node_anonymizer_base_deinit(NODE_ANONYMIZER_BASE(self));
}

boolean node_anonymizer_dot_is_id(NodeAnonymizerBase_ptr self,
                                  node_ptr id)
{
  boolean retval = false;

  if (id == Nil) { /* just return false */ }
  else if (DOT == node_get_type(id)) retval = true;
  else if (ATOM == node_get_type(id)) retval = true;
  else { /* just return false */ }

  return retval;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The NodeAnonymizerDot class virtual finalizer

  Called by the class destructor
*/
static void node_anonymizer_dot_finalize(Object_ptr object, void* dummy)
{
  NodeAnonymizerDot_ptr self = NODE_ANONYMIZER_DOT(object);

  node_anonymizer_dot_deinit(self);
  FREE(self);
}



/**AutomaticEnd***************************************************************/

