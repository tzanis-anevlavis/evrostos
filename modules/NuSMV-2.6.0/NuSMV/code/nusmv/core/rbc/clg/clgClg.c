/* ---------------------------------------------------------------------------

This file is part of the ``rbc.clg'' package
  of NuSMV version 2. Copyright (C) 2007 by FBK-irst.

  NuSMV version 2 is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2 of
  the License, or (at your option) any later version.

  NuSMV version 2 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.

  For more information on NuSMV see <http://nusmv.fbk.eu>
  or email to <nusmv-users@fbk.eu>.
  Please report bugs to <nusmv-users@fbk.eu>.

  To contact the NuSMV development board, email to <nusmv@fbk.eu>.

-----------------------------------------------------------------------------*/

/*!
  \author Dan Sheridan and Marco Roveri
               Alessandro Casagrande changed recursive code of CNFizer 
               to be iterative
  \brief Clause graphs - main file

  Manage clause graphs

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/rbc/clg/clgInt.h"

#include "nusmv/core/node/node.h"
#include "nusmv/core/utils/error.h"


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

typedef struct ClgManager_TAG {
  clause_graph * clgs;  /* All of the clause graphs. */
  int clg_count;      /* How many clgs have been allocated */
  int max_clg_count;    /* size of *clgs */
} ClgManager;

typedef struct Extract_Elem {
  clause_graph head;
  node_ptr follow;
  int clause_size;
} Extract_Elem;

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define extract_array_push(extract_array, head, follow, clause_size, index) \
{                                                                           \
    Extract_Elem ee = {head, follow, clause_size};                          \
    array_insert(Extract_Elem, extract_array, index, ee);                   \
    ++index;                                                                \
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define extract_array_pop(extract_array, index, elem, tmp)              \
  ( --index,                                                            \
    tmp = array_fetch_p(Extract_Elem, extract_array, index),            \
    elem->head = tmp->head,                                             \
    elem->follow = tmp->follow,                                         \
    elem->clause_size = tmp->clause_size )                              \


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void Extract(const NuSMVEnv_ptr env, clause_graph head,
                    node_ptr follow, int clause_size, int type,
                    Clg_Commit commit, void *data);

static int AddToClause(int** clause, int* max_clause_size, 
		       int pos_lit, int neg_lit, int size);

static clause_graph new_clg(ClgManager_ptr clgmanager);

/**AutomaticEnd***************************************************************/

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

ClgManager_ptr ClgManager_create()
{
  ClgManager_ptr self = ALLOC(ClgManager, 1);

  self->clg_count = 0;
  self->clgs = NULL;
  self->max_clg_count = 0;

  return self;
}

void ClgManager_destroy(ClgManager_ptr clgmgr)
{
  for ( ; clgmgr->clg_count>0 ; clgmgr->clg_count--) {
    FREE(clgmgr->clgs[(clgmgr->clg_count)-1]);
    clgmgr->clgs[(clgmgr->clg_count)-1] = (clause_graph)NULL;
  }
  FREE(clgmgr->clgs);
  clgmgr->clgs = (clause_graph *)NULL;
  FREE(clgmgr);
}

clause_graph Clg_Lit(ClgManager_ptr clgmanager, int literal)
{
  clause_graph lit;

  nusmv_assert(0 != literal);

  lit = new_clg(clgmanager);
  lit->label = literal;
  lit->size = 1;
  lit->left = NULL;
  lit->right = NULL;

  return lit;
}

clause_graph Clg_Conj(ClgManager_ptr clgmanager,
                      clause_graph left, clause_graph right)
{
  clause_graph vtx;
  if (left == NULL) return right;
  if (right == NULL) return left;
  vtx = new_clg(clgmanager);
  vtx->label = CLG_CONJ;
  vtx->size = left->size + right->size;
  vtx->left = left;
  vtx->right = right;

  return vtx;
}

clause_graph Clg_Disj(ClgManager_ptr clgmanager,
                      clause_graph left, clause_graph right)
{
  clause_graph vtx;
  if (left == NULL) return right;
  if (right == NULL) return left;
  vtx = new_clg(clgmanager);
  vtx->label = CLG_DISJ;
  vtx->size = left->size * right->size;
  vtx->left = left;
  vtx->right = right;

  return vtx;
}

int Clg_Size(clause_graph graph)
{
  nusmv_assert(graph != (clause_graph)NULL);
  return graph->size;
}

void Clg_Extract(const NuSMVEnv_ptr env, clause_graph head,
                 int type, Clg_Commit commit, void *data)
{
  Extract(env, head, Nil, 0, type, commit, data);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Extract the clauses, passing them to commit

  Walk the data structure from the head, creating clauses
                      each time one is seen complete. See Footnote 936 for
                      details of algorithm
*/

static void Extract(const NuSMVEnv_ptr env, clause_graph head,
                    node_ptr follow, int clause_size, int type,
                    Clg_Commit commit, void *data)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  array_t* extract_array = NULL;
  int index = 0;
  int array_size = 10;

  Extract_Elem* elem = NULL;
  Extract_Elem* tmp = NULL;

  int max_clause_size = 256;
  int* clause = ALLOC(int, max_clause_size);

  nusmv_assert(clause != (int *)NULL);
  nusmv_assert(head != NULL);

  elem = ALLOC(Extract_Elem, 1);

  extract_array = array_alloc(Extract_Elem, array_size);

  extract_array_push(extract_array, head, follow, clause_size, index);

  while (index > 0) {

    extract_array_pop(extract_array, index, elem, tmp);

    if (elem->head == (clause_graph)NULL) { 
      free_node(nodemgr, elem->follow); 
    }
    else {

      /* Check for leaf node */
      /* literals are such that both left and right are NULL. A better
         approach would be to add a field type or kind to the clg_graph so
         that it can be LITERAL, CONJ, DISJ */
      if (elem->head->left == NULL && elem->head->right == NULL) {

        switch (type) {
        case CLG_DIMACS:
        case CLG_NUSMV:
          clause_size = AddToClause(&clause, &max_clause_size, 
				    elem->head->label,
                                    -(elem->head->label), 
				    elem->clause_size);
          break;

        case CLG_ZCHAFF:
          if (elem->head->label < 0) {
            clause_size = AddToClause(&clause, &max_clause_size, 
				      1 - (elem->head->label) * 2,
                                      -(elem->head->label) * 2, 
				      elem->clause_size);
          }
          else {
            clause_size = AddToClause(&clause, &max_clause_size, 
				      (elem->head->label) * 2,
                                      1 + (elem->head->label) * 2, 
				      elem->clause_size);
          }
          break;

        default:
          ErrorMgr_internal_error(errmgr, "Clg_Extract: Bad extract type\n");
        }

        if (clause_size == 0) {
          /* All clauses from here on would be redundant, so backtrack now */
          continue;
        }
        if (elem->follow == Nil) {
          /* Nothing else to do: commit this clause! */
          commit(data, clause, clause_size);
        }
        else {
          /* Follow the follow list */
          extract_array_push(extract_array, (clause_graph)car(elem->follow),
                             cdr(elem->follow), clause_size, index);
        }
      }  /* if (elem->head->left == NULL && elem->head->right == NULL) */

      else if (elem->head->label == CLG_CONJ) {

        /* Internal conjunction node: branch */
        extract_array_push(extract_array, elem->head->right, elem->follow,
                           elem->clause_size, index);

        extract_array_push(extract_array, elem->head->left, elem->follow,
                           elem->clause_size, index);
      }

      else if (elem->head->label == CLG_DISJ) {
        node_ptr nfollow;

        /* Internal disjunction node: chain */
        /* Heuristic to speed up search */
        nusmv_assert(elem->head->left != (clause_graph)NULL);
        nusmv_assert(elem->head->right != (clause_graph)NULL);

        if (elem->head->left->size < elem->head->right->size) {
          nfollow = cons(nodemgr, (node_ptr)(elem->head->right),
                         elem->follow);

          extract_array_push(extract_array, NULL, nfollow, elem->clause_size,
                             index);

          extract_array_push(extract_array, elem->head->left, nfollow,
                             elem->clause_size, index);
        }
        else {
          nfollow = cons(nodemgr, (node_ptr)(elem->head->left),
                         elem->follow);

          extract_array_push(extract_array, NULL, nfollow, elem->clause_size,
                             index);

          extract_array_push(extract_array, elem->head->right, nfollow,
                             elem->clause_size, index);
        }
      }

      else {
        /* Something's wrong */
        ErrorMgr_internal_error(errmgr,
                                "Clg_Extract: Nonsense clause graph vertex\n");
      }
    }
  }

  FREE(elem);
  array_free(extract_array);
  FREE(clause);
}


/*!
  \brief Insert a literal into the current clause

  Insert pos_lit, where neg_lit is the corresponding
               literal of opposite polarity. If neg_lit is already in
               the clause then the clause is cancelled; if pos_lit is
               already in the clause then it is not reinserted.
*/

static int AddToClause(int** clause, int* max_clause_size, 
		       int pos_lit, int neg_lit, int size)
{
  int i;

  nusmv_assert((*max_clause_size) > 0);

  /* Check size and reallocate if necessary */
  if (size+1 == (*max_clause_size)) {
    (*max_clause_size) *= 2;
    nusmv_assert(*clause != (int *)NULL);
    *clause = REALLOC(int, *clause, (*max_clause_size));
    nusmv_assert(*clause != (int *)NULL);
  }

  /* O(n) method for inserting... but who cares? */
  for (i=0; i<size; i++) {
    if ((*clause)[i]==pos_lit) {
      return size;
    }
    if ((*clause)[i]==neg_lit) {
      return 0;
    }
  }
  (*clause)[i]=pos_lit;
  return size+1;
}


/*!
  \brief Allocate a new CLG node.

  
*/

static clause_graph new_clg(ClgManager_ptr clgmanager)
{
  clause_graph result;

  if (clgmanager->clg_count == 0) {
    clgmanager->clgs = ALLOC(clause_graph, 4096);
    nusmv_assert(clgmanager->clgs != (clause_graph *) NULL);
    clgmanager->max_clg_count = 4096;
  }
  if ((clgmanager->clg_count) + 1 >= clgmanager->max_clg_count) {
    clgmanager->max_clg_count *= 2;
    clgmanager->clgs = REALLOC(clause_graph, clgmanager->clgs, 
			       clgmanager->max_clg_count);
    nusmv_assert(clgmanager->clgs != (clause_graph *) NULL);
  }

  result = ALLOC(struct Clg_Vertex, 1);
  nusmv_assert(result != (struct Clg_Vertex *) NULL);

  clgmanager->clgs[clgmanager->clg_count] = result;
  clgmanager->clg_count++;

  return result;
}
