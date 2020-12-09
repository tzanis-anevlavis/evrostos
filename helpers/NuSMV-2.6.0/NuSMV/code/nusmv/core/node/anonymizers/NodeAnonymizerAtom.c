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
  \brief Implementation of class 'NodeAnonymizerAtom'

  This class recognized as identifiers the ATOM nodes. So, for
  instance, a DOT node will be treated as an expression and just navigated.
  The identification is purely syntactical

*/


#include "nusmv/core/node/anonymizers/NodeAnonymizerAtom.h" 
#include "nusmv/core/node/anonymizers/NodeAnonymizerAtom_private.h" 
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
/* See 'NodeAnonymizerAtom_private.h' for class 'NodeAnonymizerAtom' definition. */

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

static void node_anonymizer_atom_finalize(Object_ptr object, void* dummy);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

NodeAnonymizerAtom_ptr NodeAnonymizerAtom_create(NuSMVEnv_ptr env,
                                                 const char* default_prefix,
                                                 size_t memoization_threshold)
{
  NodeAnonymizerAtom_ptr self = ALLOC(NodeAnonymizerAtom, 1);
  NODE_ANONYMIZER_ATOM_CHECK_INSTANCE(self);

  node_anonymizer_atom_init(self, env, default_prefix, memoization_threshold);
  return self;
}

void NodeAnonymizerAtom_destroy(NodeAnonymizerAtom_ptr self)
{
  NODE_ANONYMIZER_ATOM_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void node_anonymizer_atom_init(NodeAnonymizerAtom_ptr self,
                               NuSMVEnv_ptr env,
                               const char* default_prefix,
                               size_t memoization_threshold)
{
  /* base class initialization */
  node_anonymizer_base_init(NODE_ANONYMIZER_BASE(self), env, default_prefix,
                            memoization_threshold);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = node_anonymizer_atom_finalize;
  OVERRIDE(NodeAnonymizerBase, is_id) = node_anonymizer_atom_is_id;
}

void node_anonymizer_atom_deinit(NodeAnonymizerAtom_ptr self)
{
  /* members deinitialization */


  /* base class deinitialization */
  node_anonymizer_base_deinit(NODE_ANONYMIZER_BASE(self));
}

boolean node_anonymizer_atom_is_id(NodeAnonymizerBase_ptr self,
                                   node_ptr id)
{
  boolean retval = false;

  if (id == Nil) { /* just return false */ }
  else if (ATOM == node_get_type(id)) retval = true;
  else { /* just return false */ }

  return retval;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The NodeAnonymizerAtom class virtual finalizer

  Called by the class destructor
*/
static void node_anonymizer_atom_finalize(Object_ptr object, void* dummy)
{
  NodeAnonymizerAtom_ptr self = NODE_ANONYMIZER_ATOM(object);

  node_anonymizer_atom_deinit(self);
  FREE(self);
}



/**AutomaticEnd***************************************************************/

