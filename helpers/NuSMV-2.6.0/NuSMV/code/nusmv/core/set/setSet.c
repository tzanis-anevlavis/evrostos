/* ---------------------------------------------------------------------------


  This file is part of the ``set'' package of NuSMV version 2.
  Copyright (C) 2000-2001 by FBK-irst and 2008 by FBK-irst.

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
  \author Marco Roveri, Roberto Cavada
  \brief Generic Set Data Structure

  This package provides an implementation of sets. Sets
  are ordered, meaning that elements can be traversed in the same
  chronological order they have been added. Uniqueness is not assured,
  meaning that you might have to idential sets that are stored into
  two different structures. This means that you cannot compare two
  sets by comparing their sets. For further details see the
  description about the Set_t structure

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/set/setInt.h"
#include "nusmv/core/utils/NodeList.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

typedef struct Set_TAG {
  NodeList_ptr list;
  int* fam;

} Set;


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
static NodeList_ptr set_empty_list = NODE_LIST(NULL);
static unsigned int set_empty_list_ref_count = 0;


/* "[RC]: code for memoizing Sets operations has to be cleaned up" */

/* #include "utils/assoc.h" */
/* static hash_ptr set_op_hash = (hash_ptr) NULL; */
/* static void insert_op_hash(node_ptr key, Set_t value)  */
/* { */
/*   nusmv_assert(set_op_hash != (hash_ptr) NULL); */
/*   if (value != NULL) {  */
/*     Set_Freeze(value);  */
/*     value = Set_Copy(value); */
/*   } */
/*   insert_assoc(set_op_hash, key, (node_ptr) value); */
/* } */
/* static Set_t lookup_op_hash(node_ptr key)  */
/* { */
/*   nusmv_assert(set_op_hash != (hash_ptr) NULL); */
/*   return (Set_t) find_assoc(set_op_hash, key); */
/* } */


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void set_union_to_set_aux(NodeMgr_ptr nodemgr,
                                 node_ptr a, Set_t* set);


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static Set_t set_create(void);
static Set_t set_copy(const Set_t self);
static Set_t set_copy_actual(const Set_t self);
static void set_destroy(Set_t self);
static void set_check_list(Set_t self);
static Set_t set_check_frozen(Set_t self);
static inline boolean set_is_frozen(Set_t const set);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void set_pkg_init(void)
{
  if (set_empty_list == NODE_LIST(NULL)) {
    set_empty_list = NodeList_create();
  }

  set_empty_list_ref_count++;

/*
  if (set_op_hash == (hash_ptr) NULL) {
    set_op_hash = new_assoc();
  }
*/
}

void set_pkg_quit()
{
  nusmv_assert(set_empty_list_ref_count >= 1);
  set_empty_list_ref_count--;

  if (set_empty_list_ref_count == 0) {
    /* assert that set_pkg_init() has been called before set_pkg_quit() */
    nusmv_assert(NODE_LIST(NULL) != set_empty_list);
    NodeList_destroy(set_empty_list);
    set_empty_list = NODE_LIST(NULL);
  }

/*  if (set_op_hash != (hash_ptr) NULL) {
    free_assoc(set_op_hash);
    set_op_hash = (hash_ptr) NULL;
  }
*/
}

Set_t Set_MakeEmpty(void)
{
  return (Set_t) NULL;
}

Set_t Set_Make(node_ptr l)
{
  Set_t result;

  if (l == Nil) return Set_MakeEmpty();
  result = set_create();
  for (; l != Nil; l = cdr(l)) {
    result = Set_AddMember(result, (Set_Element_t) car(l));
  }
  return result;
}

Set_t Set_MakeSingleton(Set_Element_t el)
{
  Set_t set = set_create();
  set = Set_AddMember(set, el);
  return set;
}

Set_t Set_MakeFromUnion(NodeMgr_ptr nodemgr, node_ptr _union)
{
  Set_t result = Set_MakeEmpty();
  set_union_to_set_aux(nodemgr, _union, &result);
  return result;
}

node_ptr Set_Set2Union(const Set_t set, NodeMgr_ptr nodemgr)
{
  node_ptr res = Nil;
  Set_Iterator_t s_iter;

  SET_FOREACH(set, s_iter) {
    node_ptr el = Set_GetMember(set, s_iter);
    res = (Nil == res) ? el : NodeMgr_find_node(nodemgr, UNION, el, res);
  }

  return res;
}

node_ptr Set_Set2Node(const Set_t set, NodeMgr_ptr nodemgr) {
  node_ptr res = new_list();
  Set_Iterator_t s_iter;

  SET_FOREACH(set, s_iter) {
    node_ptr el = Set_GetMember(set, s_iter);
    res = Node_conslist_add(nodemgr, res, el);
  }

  return res;
}

Set_t Set_Copy(const Set_t set)
{
  return set_copy(set);
}

void Set_ReleaseSet(Set_t set)
{
  set_destroy(set);
}

void Set_ReleaseSetOfSet(Set_t set)
{
  Set_Iterator_t iter;
  SET_FOREACH(set, iter) {
    Set_t sub_set = (Set_t)Set_GetMember(set, iter);
    Set_ReleaseSet(sub_set);
  }
  Set_ReleaseSet(set);
}

boolean Set_IsEmpty (const Set_t set)
{
  return (set == NULL || set->list == NULL ||
          NodeList_get_length(set->list) == 0);
}

boolean Set_IsMember(const Set_t set, Set_Element_t el)
{
  return (set != NULL && set->list != NULL &&
          NodeList_belongs_to(set->list, (node_ptr) el));
}

int Set_GiveCardinality(const Set_t set)
{
  if (set == NULL || set->list == NULL) return 0;
  return NodeList_get_length(set->list);
}

Set_t Set_AddMember(Set_t set, Set_Element_t el)
{
  if (set == NULL) set = Set_MakeSingleton(el);
  else {
    if (Set_IsMember(set, el)) return set;
/*
    node_ptr key = find_node(nodemgr, COLON, (node_ptr) __func__, find_node(nodemgr, COLON, (node_ptr) set, (node_ptr) el));
    Set_t res = lookup_op_hash(key);
    if (res != NULL) return res;
*/
    set = set_check_frozen(set);
    set_check_list(set);
    if (!NodeList_belongs_to(set->list, (node_ptr) el)) {
      NodeList_append(set->list, (node_ptr) el);
    }

/*    insert_op_hash(key, res); */
  }

  return set;
}

Set_t Set_RemoveMember(Set_t set, Set_Element_t el)
{
  if (!Set_IsEmpty(set) && Set_IsMember(set, el)) {
    Set_t sel;

    set = set_check_frozen(set);
    sel = Set_MakeSingleton(el);
    set = Set_Difference(set, sel);
    Set_ReleaseSet(sel);
  }
  return set;
}

Set_t Set_AddMembersFromList(Set_t set, const NodeList_ptr list)
{
  ListIter_ptr iter;
  NODE_LIST_FOREACH(list, iter) {
    set = Set_AddMember(set, (Set_Element_t) NodeList_get_elem_at(list, iter));
  }
  return set;
}

boolean Set_Contains(const Set_t set1, const Set_t set2)
{
  Set_Iterator_t iter;

  if (Set_IsEmpty(set2) || (set1 == set2)) return true;
  if (Set_GiveCardinality(set1) < Set_GiveCardinality(set2)) return false;
  SET_FOREACH(set2, iter) {
    if (!Set_IsMember(set1, Set_GetMember(set2, iter))) return false;
  }

  return true;
}

boolean Set_Equals(const Set_t set1, const Set_t set2)
{
  if (set1 == set2) return true;
  if (Set_GiveCardinality(set1) != Set_GiveCardinality(set2)) return false;
  return Set_Contains(set1, set2);
}

boolean Set_Intersects(const Set_t set1, const Set_t set2)
{
  Set_Iterator_t iter;

  if (Set_IsEmpty(set1) || Set_IsEmpty(set2)) return false;
  if (set1 == set2) return true;
  SET_FOREACH(set1, iter) {
    if (Set_IsMember(set2, Set_GetMember(set1, iter))) return true;
  }

  return false;
}

Set_t Set_Union(Set_t set1, const Set_t set2)
{
  if (Set_IsEmpty(set1)) return Set_Copy(set2);
  if (Set_IsEmpty(set2)) return set1;
  if (Set_Contains(set1, set2)) return set1;

  set1 = set_check_frozen(set1);
  NodeList_concat_unique(set1->list, set2->list);
  return set1;
}

Set_t Set_Intersection(Set_t set1, const Set_t set2)
{
  Set_t set11;

  if (Set_IsEmpty(set1) || Set_IsEmpty(set2)) {
    Set_ReleaseSet(set1);
    return Set_MakeEmpty();
  }

  set11 = Set_Copy(set1);
  set11 = Set_Difference(set11, set2);

  set1 = set_check_frozen(set1);
  set1 = Set_Difference(set1, set11);
  Set_ReleaseSet(set11);

  return set1;
}

Set_t Set_Difference(Set_t set1, const Set_t set2)
{
  if (Set_IsEmpty(set1) || Set_IsEmpty(set2)) return set1;

  set1 = set_check_frozen(set1);
  NodeList_remove_elems(set1->list, set2->list, NULL /* no disposal */, NULL);

  if (Set_IsEmpty(set1)) {
    Set_ReleaseSet(set1);
    set1 = Set_MakeEmpty();
  }

  return set1;
}

Set_t Set_GetRest(const Set_t set, Set_Iterator_t from)
{
  Set_t res = Set_MakeEmpty();
  Set_Iterator_t iter;
  for (iter=from; !Set_IsEndIter(iter); iter=Set_GetNextIter(iter)) {
    res = Set_AddMember(res, Set_GetMember(set, iter));
  }
  return res;
}

Set_t Set_Freeze(Set_t set)
{
  if (set != NULL && set->fam == (int*) NULL) {
    set->fam = ALLOC(int, 1);
    nusmv_assert(set->fam != (int*) NULL);
    *(set->fam) = 1;
  }
  return set;
}

Set_Iterator_t Set_GetFirstIter(Set_t set1)
{
  if (Set_IsEmpty(set1)) return (Set_Iterator_t) ListIter_get_end();
  return (Set_Iterator_t) NodeList_get_first_iter(set1->list);
}

Set_Iterator_t Set_GetNextIter(Set_Iterator_t iter)
{
  return (Set_Iterator_t) ListIter_get_next((ListIter_ptr) iter);
}

boolean Set_IsEndIter(Set_Iterator_t iter)
{
  return ListIter_is_end((ListIter_ptr) iter);
}

Set_Element_t Set_GetMember(const Set_t set, Set_Iterator_t iter)
{
  nusmv_assert(!Set_IsEmpty(set));
  return NodeList_get_elem_at(set->list, (ListIter_ptr) iter);
}

NodeList_ptr Set_Set2List(const Set_t set)
{
  set_check_list(set);
  if (set == NULL) {
    nusmv_assert(set_empty_list != NODE_LIST(NULL) &&
                 NodeList_get_length(set_empty_list) == 0);
    return set_empty_list;
  }
  return set->list;
}


void Set_PrintSet(MasterPrinter_ptr wffprint,
                  FILE * file,
                  const Set_t s,
                  void (*printer)(FILE* file, Set_Element_t el, void* arg),
                  void* printer_arg)
{
  Set_Iterator_t iter;
  boolean keep;
  fprintf(file, "{");

  iter = Set_GetFirstIter(s);
  keep = !Set_IsEndIter(iter);
  while (keep) {
    if (printer != NULL) printer(file, Set_GetMember(s, iter), printer_arg);
    else print_node(wffprint, file, (node_ptr) Set_GetMember(s, iter));
    iter = Set_GetNextIter(iter);
    keep = !Set_IsEndIter(iter);
    if (keep) fprintf(file, ", ");
  }
  fprintf(file, "}");
}

boolean Set_IsSingleton(Set_t const set)
{
  return 1 == NodeList_get_length(set->list);
}

boolean Set_IsFrozen(Set_t const set)
{
  return set_is_frozen(set);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Internal constructor

  Internal constructor
*/
static Set_t set_create(void)
{
  Set_t self = ALLOC(Set, 1);
  nusmv_assert(self != (Set_t) NULL);
  self->list = NODE_LIST(NULL);
  self->fam = (int*) NULL;

  return self;
}

/*!
  \brief Internal copy constructor

  Internal copy constructor
*/
static Set_t set_copy(const Set_t self)
{
  if (Set_IsEmpty(self)) return Set_MakeEmpty();

  if (self->fam != (int*) NULL) {
    /* it is frozen */
    *(self->fam) += 1;
    return self;
  }

  return set_copy_actual(self);
}

/*!
  \brief Internal copy constructor

  Internal copy constructor
*/
static Set_t set_copy_actual(const Set_t self)
{
  Set_t copy;
  if (Set_IsEmpty(self)) return Set_MakeEmpty();

  copy = ALLOC(Set, 1);
  nusmv_assert(self != (Set_t) NULL);
  copy->list = NodeList_copy(self->list);
  copy->fam = (int*) NULL;

  return copy;
}

/*!
  \brief Internal destructor

  Internal destructor
*/
static void set_destroy(Set_t self)
{
  if (self != NULL) {
    if (self->fam == (int*) NULL || *(self->fam) == 1) {
      if (self->list != NODE_LIST(NULL)) {
        NodeList_destroy(self->list);
        self->list = NODE_LIST(NULL);
      }
      if (self->fam != (int*) NULL) {
        FREE(self->fam);
        self->fam = (int*) NULL;
      }
      FREE(self);
    }
    else {
      *(self->fam) -= 1;
    }

  }
}

/*!
  \brief This methods checks family counter and returns either a
  new instance of self

  Used internally by functions that change the instance
  to handle frozen set
*/
static Set_t set_check_frozen(Set_t self)
{
  if (self == NULL || self->fam == (int*) NULL) return self;

  /* copies as it is changing */
  *(self->fam) -= 1; /* deref self */
  return set_copy_actual(self);
}

/*!
  \brief Fix the internal list if used actually

  This method is used internally to allow late allocation
  of the list
*/
static void set_check_list(Set_t self)
{
  if (self != NULL && self->list == NULL) {
    self->list = NodeList_create();
  }
}

/*!
  \brief Given a node (possibly a UNION node) returns the
  corresponding set

  Auxiliary function for constructor from union nodes

  \se Given set will be added of found expressions
*/
static void set_union_to_set_aux(NodeMgr_ptr nodemgr,
                                 node_ptr a, Set_t* set)
{
  if (Nil == a)
    return;

  if (UNION == node_get_type(a)) {
    set_union_to_set_aux(nodemgr, car(a), set);
    set_union_to_set_aux(nodemgr, cdr(a), set);
    return;
  }

  if (TWODOTS == node_get_type(a)) {
    int first, last;
    /* ranges may consist of constants NUMBERS only */
    nusmv_assert(NUMBER == node_get_type(car(a)) &&
                 NUMBER == node_get_type(cdr(a)));
    for (first = node_get_int(car(a)), last = node_get_int(cdr(a));
         first <= last;
         first++) {
      *set = Set_AddMember(*set, find_node(nodemgr, NUMBER, NODE_FROM_INT(first), Nil));
    }
  }
  /* normal arbitrary expression */
  else {
    *set = Set_AddMember(*set, a);
  }

}

/*!
  \brief Checks if the set is frozen

  Checks if the set is frozen
*/
static inline boolean set_is_frozen(Set_t const set)
{
  nusmv_assert((Set_t)NULL != set);

  return set->fam != (int*)NULL;
}
