/* ---------------------------------------------------------------------------


  This file is part of the ``hrc'' package of NuSMV version 2.
  Copyright (C) 2009 by FBK-irst.

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
  \author Sergio Mover
  \brief Implementation of class 'HrcVarDependencies'

  'HrcVarDependencies' keeps a set of dependencies. The
  dependencies are stored in four sets: a set contains variables, a
  set contains define, a set contains formal parameter and a set
  contains actual parameters.

  This class is used in hrcLocalize to change multiple sets in
  different function.

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/hrc/HrcVarDependencies.h"

#include "nusmv/core/set/set.h"
#include "nusmv/core/utils/assoc.h"
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

typedef struct HrcVarDependencies_TAG
{
  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  NodeMgr_ptr nodes;
  Set_t variables_set;
  Set_t defines_set;
  Set_t formal_par_set;
  Set_t actual_par_set;

} HrcVarDependencies;



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

static void hrc_var_dependencies_init(HrcVarDependencies_ptr self,
                                      NodeMgr_ptr nodemgr);
static void hrc_var_dependencies_deinit(HrcVarDependencies_ptr self);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

HrcVarDependencies_ptr HrcVarDependencies_create(NodeMgr_ptr nodemgr)
{
  HrcVarDependencies_ptr self = ALLOC(HrcVarDependencies, 1);
  HRC_VAR_DEPENDENCIES_CHECK_INSTANCE(self);

  hrc_var_dependencies_init(self, nodemgr);
  return self;
}

void HrcVarDependencies_destroy(HrcVarDependencies_ptr self)
{
  HRC_VAR_DEPENDENCIES_CHECK_INSTANCE(self);

  hrc_var_dependencies_deinit(self);
  FREE(self);
}

NodeMgr_ptr HrcVarDependencies_get_node_manager(const HrcVarDependencies_ptr self)
{
  HRC_VAR_DEPENDENCIES_CHECK_INSTANCE(self);

  return self->nodes;
}

void HrcVarDependencies_add_variable(HrcVarDependencies_ptr self,
                                     node_ptr variable)
{
  HRC_VAR_DEPENDENCIES_CHECK_INSTANCE(self);

  self->variables_set = Set_AddMember(self->variables_set,
                                      (Set_Element_t) variable);
}

void HrcVarDependencies_add_define(HrcVarDependencies_ptr self,
                                   node_ptr define)
{
  HRC_VAR_DEPENDENCIES_CHECK_INSTANCE(self);

  self->defines_set = Set_AddMember(self->defines_set, (Set_Element_t) define);
}

void HrcVarDependencies_add_parameter(HrcVarDependencies_ptr self,
                                      node_ptr formal_name,
                                      node_ptr actual)
{
  HRC_VAR_DEPENDENCIES_CHECK_INSTANCE(self);

  self->formal_par_set = Set_AddMember(self->formal_par_set,
                                       (Set_Element_t) find_atom(self->nodes,
                                                                 formal_name));
  self->actual_par_set = Set_AddMember(self->actual_par_set,
                                       (Set_Element_t) actual);
}

Set_t HrcVarDependencies_get_variables_set(HrcVarDependencies_ptr self)
{
  HRC_VAR_DEPENDENCIES_CHECK_INSTANCE(self);

  return self->variables_set;
}

Set_t HrcVarDependencies_get_defines_set(HrcVarDependencies_ptr self)
{
  HRC_VAR_DEPENDENCIES_CHECK_INSTANCE(self);

  return self->defines_set;
}

Set_t HrcVarDependencies_get_formal_par_set(HrcVarDependencies_ptr self)
{
  HRC_VAR_DEPENDENCIES_CHECK_INSTANCE(self);

  return self->formal_par_set;
}

Set_t HrcVarDependencies_get_actual_par_set(HrcVarDependencies_ptr self)
{
  HRC_VAR_DEPENDENCIES_CHECK_INSTANCE(self);

  return self->actual_par_set;
}

boolean HrcVarDependencies_has_formal_parameter(HrcVarDependencies_ptr self,
                                                 node_ptr formal)
{
  HRC_VAR_DEPENDENCIES_CHECK_INSTANCE(self);

  return Set_IsMember(self->formal_par_set,
                      (Set_Element_t) find_atom(self->nodes, formal));
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The HrcVarDependencies class private initializer

  The HrcVarDependencies class private initializer

  \sa HrcVarDependencies_create
*/
static void hrc_var_dependencies_init(HrcVarDependencies_ptr self,
                                      NodeMgr_ptr nodemgr)
{
  /* members initialization */
  self->nodes = nodemgr;
  self->variables_set = Set_MakeEmpty();
  self->defines_set = Set_MakeEmpty();
  self->formal_par_set = Set_MakeEmpty();
  self->actual_par_set = Set_MakeEmpty();
}

/*!
  \brief The HrcVarDependencies class private deinitializer

  The HrcVarDependencies class private deinitializer

  \sa HrcVarDependencies_destroy
*/
static void hrc_var_dependencies_deinit(HrcVarDependencies_ptr self)
{
  /* members deinitialization */
  Set_ReleaseSet(self->variables_set);
  Set_ReleaseSet(self->defines_set);
  Set_ReleaseSet(self->formal_par_set);
  Set_ReleaseSet(self->actual_par_set);
}



/**AutomaticEnd***************************************************************/

