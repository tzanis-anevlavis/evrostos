
/* ---------------------------------------------------------------------------


  This file is part of the ``utils'' package of NuSMV version 2.
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
  \author Roberto Cavada
  \brief Implementation of class 'NodeGraph'

  \todo: Missing description

*/


#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/utils/NodeGraph.h"

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/assoc.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct NodeGraph_TAG
{
  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  hash_ptr graph_to;
  hash_ptr graph_from;
  Set_t graph_removed;
  Set_t graph_nodes;

} NodeGraph;



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

static void node_graph_init(NodeGraph_ptr self);
static void node_graph_deinit(NodeGraph_ptr self);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

NodeGraph_ptr NodeGraph_create()
{
  NodeGraph_ptr self = ALLOC(NodeGraph, 1);
  NODE_GRAPH_CHECK_INSTANCE(self);

  node_graph_init(self);
  return self;
}

void NodeGraph_destroy(NodeGraph_ptr self)
{
  NODE_GRAPH_CHECK_INSTANCE(self);

  node_graph_deinit(self);
  FREE(self);
}

void NodeGraph_add_children(NodeGraph_ptr self, node_ptr var,
                            const Set_t children)
{
  Set_Iterator_t iter;
  Set_t down;

  NODE_GRAPH_CHECK_INSTANCE(self);

  self->graph_nodes = Set_AddMember(self->graph_nodes, var);
  self->graph_nodes = Set_Union(self->graph_nodes, children);

  down = (Set_t) find_assoc(self->graph_to, var);
  if ((Set_t) NULL == down) {
    down = Set_MakeEmpty();
  }

  SET_FOREACH(children, iter) {
    node_ptr child = Set_GetMember(children, iter);
    Set_t up = (Set_t) find_assoc(self->graph_from, child);
    if ((Set_t) NULL == up) up = Set_MakeEmpty();

    if (child != var) { /* self-loops are ignored */
      down = Set_AddMember(down, child);
      up = Set_AddMember(up, var);
    }
    insert_assoc(self->graph_from, child, (Set_Element_t) up);
  }

  insert_assoc(self->graph_to, var, (Set_Element_t) down);
}

void NodeGraph_remove_nodes(NodeGraph_ptr self, const Set_t nodes)
{
  NODE_GRAPH_CHECK_INSTANCE(self);
  self->graph_removed = Set_Union(self->graph_removed, nodes);
}

void NodeGraph_clear_removed_nodes(NodeGraph_ptr self)
{
  NODE_GRAPH_CHECK_INSTANCE(self);
  Set_ReleaseSet(self->graph_removed);
  self->graph_removed = Set_MakeEmpty();
}

boolean NodeGraph_is_empty(const NodeGraph_ptr self)
{
  int diff;

  NODE_GRAPH_CHECK_INSTANCE(self);

  diff = (Set_GiveCardinality(self->graph_nodes) -
          Set_GiveCardinality(self->graph_removed));
  nusmv_assert(diff >= 0);
  return diff == 0;
}

Set_t NodeGraph_get_leaves(const NodeGraph_ptr self)
{
  Set_t res;
  Set_Iterator_t iter;

  NODE_GRAPH_CHECK_INSTANCE(self);

  res = Set_MakeEmpty();
  if (NodeGraph_is_empty(self)) return res;

  SET_FOREACH(self->graph_nodes, iter) {
    node_ptr var = Set_GetMember(self->graph_nodes, iter);

    if (!Set_IsMember(self->graph_removed, var)) {
      Set_t set_to = (Set_t) find_assoc(self->graph_to, var);
      if (Set_Contains(self->graph_removed, set_to)) {
        res = Set_AddMember(res, (Set_Element_t) var);
      }
    }
  }

  return res;
}

Set_t NodeGraph_get_parents(const NodeGraph_ptr self, node_ptr child)
{
  Set_t parents;

  NODE_GRAPH_CHECK_INSTANCE(self);

  parents = (Set_t) find_assoc(self->graph_from, child);

  if ((Set_t) NULL == parents) return Set_MakeEmpty();

  parents = Set_Copy(parents);
  if (Set_IsEmpty(self->graph_removed)) return parents; /* optimization */
  else return Set_Difference(parents, self->graph_removed);
}

void NodeGraph_print(const NodeGraph_ptr self,
                     MasterPrinter_ptr printer,
                     FILE* out)
{
  Set_Iterator_t iter;

  NODE_GRAPH_CHECK_INSTANCE(self);

  SET_FOREACH(self->graph_nodes, iter) {
    node_ptr var = Set_GetMember(self->graph_nodes, iter);

    if (!Set_IsMember(self->graph_removed, var)) {
      Set_t set_to = (Set_t) find_assoc(self->graph_to, var);
      Set_t set_rem = Set_Difference(Set_Copy(set_to), self->graph_removed);
      print_node(printer, out, var);
      fprintf(out, " ==> ");
      Set_PrintSet(printer, out, set_rem, NULL, NULL);
      fprintf(out, "\n");

      Set_ReleaseSet(set_rem);
    }
  }

  fprintf(out, "Removed nodes are:\n");
  Set_PrintSet(printer, out, self->graph_removed, NULL, NULL);
  fprintf(out, "\n");
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief


*/

static assoc_retval node_graph_assoc_free_set(char* key, char* data, char* arg)
{
  Set_t set = (Set_t) data;
  if ((Set_t) NULL != set) Set_ReleaseSet(set);
  return ASSOC_DELETE;
}

/*!
  \brief The NodeGraph class private initializer

  The NodeGraph class private initializer

  \sa NodeGraph_create
*/
static void node_graph_init(NodeGraph_ptr self)
{
  /* members initialization */
  self->graph_to = new_assoc();
  self->graph_from = new_assoc();
  self->graph_removed = Set_MakeEmpty();
  self->graph_nodes = Set_MakeEmpty();
}

/*!
  \brief The NodeGraph class private deinitializer

  The NodeGraph class private deinitializer

  \sa NodeGraph_destroy
*/
static void node_graph_deinit(NodeGraph_ptr self)
{
  /* members deinitialization */
  Set_ReleaseSet(self->graph_nodes);
  Set_ReleaseSet(self->graph_removed);

  clear_assoc_and_free_entries(self->graph_from, node_graph_assoc_free_set);
  free_assoc(self->graph_from);

  clear_assoc_and_free_entries(self->graph_to, node_graph_assoc_free_set);
  free_assoc(self->graph_to);
}



/**AutomaticEnd***************************************************************/
