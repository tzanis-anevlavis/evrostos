/* ---------------------------------------------------------------------------


  This file is part of the ``compass.compile'' package of NuSMV version 2. 
  Copyright (C) 2008 by FBK-irst. 

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
  \author Roberto Cavada
  \brief Implementation of class 'ProbAssign'

  \todo: Missing description

*/


#include "nusmv/addons_core/compass/compile/ProbAssign.h" 
#include "nusmv/core/utils/utils.h" 

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct ProbAssign_TAG
{
  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  node_ptr assigns;
  node_ptr value;

} ProbAssign;



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

static void prob_assign_init(ProbAssign_ptr self, node_ptr assigns, 
                             node_ptr value);
static void prob_assign_deinit(ProbAssign_ptr self);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

ProbAssign_ptr ProbAssign_create(node_ptr assigns, node_ptr value)
{
  ProbAssign_ptr self = ALLOC(ProbAssign, 1);
  PROB_ASSIGN_CHECK_INSTANCE(self);

  prob_assign_init(self, assigns, value);
  return self;
}

void ProbAssign_destroy(ProbAssign_ptr self)
{
  PROB_ASSIGN_CHECK_INSTANCE(self);

  prob_assign_deinit(self);
  FREE(self);
}

node_ptr ProbAssign_get_assigns_expr(const ProbAssign_ptr self)
{
  PROB_ASSIGN_CHECK_INSTANCE(self);
  return self->assigns;
}

node_ptr ProbAssign_get_prob(const ProbAssign_ptr self)
{
  PROB_ASSIGN_CHECK_INSTANCE(self);
  return self->value;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The ProbAssign class private initializer

  The ProbAssign class private initializer

  \sa ProbAssign_create
*/
static void prob_assign_init(ProbAssign_ptr self, 
                             node_ptr assigns, node_ptr value)
{
  /* members initialization */
  self->assigns = assigns;
  self->value = value;
}

/*!
  \brief The ProbAssign class private deinitializer

  The ProbAssign class private deinitializer

  \sa ProbAssign_destroy
*/
static void prob_assign_deinit(ProbAssign_ptr self)
{
  /* members deinitialization */

}


/**AutomaticEnd***************************************************************/

