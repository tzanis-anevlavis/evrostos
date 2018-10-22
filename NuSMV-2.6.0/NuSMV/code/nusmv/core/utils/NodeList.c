/* ---------------------------------------------------------------------------


  This file is part of the ``utils'' package of NuSMV version 2.
  Copyright (C) 2003 by FBK-irst.

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
  \author Roberto Cavada, Andrea Micheli,
               complete internal reimplementation by Alessandro Mariotti
  \brief This is a class exporting a node_ptr based list, but with
  a higher level and better performances

  It is supported by calling the
  constructor create_from_list.  Notice that at the moment a minimal
  bunch of functionalities is exported 

*/

#if HAVE_CONFIG_H
#  include "nusmv-config.h"
#endif

#if NUSMV_HAVE_STDLIB_H
#  include <stdlib.h>
#endif

/* needed just for testing */
#if NUSMV_HAVE_LIMITS_H
#  include <limits.h>
#endif
#include "nusmv/core/utils/ucmd.h"
/* end of needed just for testing */

#include <stdarg.h>

#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/Stack.h"


/*---------------------------------------------------------------------------*/
/* Types definition                                                          */
/*---------------------------------------------------------------------------*/

typedef struct Link_TAG {
  struct Link_TAG* prev;
  struct Link_TAG* next;

  node_ptr element;
} Link;

typedef struct NodeList_TAG {
  Link* head;
  Link* tail;

  unsigned long size;

  hash_ptr count_hash;
} NodeList;

/*---------------------------------------------------------------------------*/
/* Macros definition                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define LINK_CHUNK_SIZE 1024

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NULL_LINK                               \
  (Link*)NULL


/*---------------------------------------------------------------------------*/
/* Variables definition                                                      */
/*---------------------------------------------------------------------------*/

static Link* pool = NULL_LINK;
static Stack_ptr chunks = STACK(NULL);
static size_t reference_counter = 0;


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void node_list_init(NodeList_ptr self, node_ptr list);
static void node_list_deinit(NodeList_ptr self);

static inline Link*
node_list_alloc_link(const NodeList_ptr self);

static inline void
node_list_free_link(const NodeList_ptr self, Link* link);

static inline void
node_list_update_count(const NodeList_ptr self, const node_ptr elem,
                       const boolean deleting);

static void node_list_fill_from_strings(NodeList_ptr self, unsigned int size,
                                        ...);
static int node_list_test(NodeList_ptr self);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

NodeList_ptr NodeList_create(void)
{
  NodeList_ptr self = ALLOC(NodeList, 1);
  NODE_LIST_CHECK_INSTANCE(self);

  node_list_init(self, Nil);
  return self;
}

NodeList_ptr NodeList_create_from_list(node_ptr list)
{
  NodeList_ptr self = ALLOC(NodeList, 1);
  NODE_LIST_CHECK_INSTANCE(self);

  node_list_init(self, list);
  return self;
}

NodeList_ptr NodeList_create_from_element(node_ptr node)
{
  NodeList_ptr list = NodeList_create();
  NodeList_append(list, node);
  return list;
}

void NodeList_destroy(NodeList_ptr self)
{
  NODE_LIST_CHECK_INSTANCE(self);

  node_list_deinit(self);
  FREE(self);
}

NodeList_ptr NodeList_copy(const NodeList_ptr self)
{
  NodeList_ptr copy;
  ListIter_ptr iter;

  NODE_LIST_CHECK_INSTANCE(self);

  copy = ALLOC(NodeList, 1);
  node_list_init(copy, Nil);

  NODE_LIST_FOREACH(self, iter) {
    node_ptr elem = NodeList_get_elem_at(self, iter);
    NodeList_append(copy, elem);
  }

  return copy;
}

void NodeList_append(NodeList_ptr self, node_ptr elem)
{
  Link* new_link;

  NODE_LIST_CHECK_INSTANCE(self);

  new_link = node_list_alloc_link(self);

  if (NULL_LINK == self->tail) {
    nusmv_assert(NULL_LINK == self->head);

    self->head = new_link;
  }
  else {
    new_link->prev = self->tail;
    self->tail->next = new_link;
  }

  self->tail = new_link;

  /* Finally set the element */
  new_link->element = elem;

  /* Remember the size.. */
  ++self->size;

  node_list_update_count(self, elem, false);
}

void NodeList_prepend(NodeList_ptr self, node_ptr elem)
{
  Link* new_link;

  NODE_LIST_CHECK_INSTANCE(self);

  new_link = node_list_alloc_link(self);

  if (NULL_LINK == self->head) {
    nusmv_assert(NULL_LINK == self->tail);

    self->tail = new_link;
  }
  else {
    new_link->next = self->head;
    self->head->prev = new_link;
  }

  self->head = new_link;

  /* Finally set the element */
  new_link->element = elem;

  /* Remember the size.. */
  ++self->size;

  node_list_update_count(self, elem, false);
}

void NodeList_insert_before(NodeList_ptr self, ListIter_ptr iter,
                            node_ptr elem)
{
  NODE_LIST_CHECK_INSTANCE(self);

  /* Insert tail */
  if (ListIter_is_end(iter)) {
    NodeList_append(self, elem);
  }
  else {
    Link* new_link = node_list_alloc_link(self);

    if (iter->prev == NULL_LINK) {
      nusmv_assert(iter == self->head);
      nusmv_assert(NULL_LINK != self->tail);

      self->head = new_link;
    }
    else {
      iter->prev->next = new_link;
    }

    new_link->prev = iter->prev;
    new_link->next = iter;
    iter->prev = new_link;

    /* Finally set the element */
    new_link->element = elem;

    ++self->size;
    node_list_update_count(self, elem, false);
  }
}

void NodeList_insert_after(NodeList_ptr self, ListIter_ptr iter, node_ptr elem)
{
  Link* new_link;
  Link* next;

  NODE_LIST_CHECK_INSTANCE(self);

  /* Insert tail */
  nusmv_assert(!ListIter_is_end(iter));

  new_link = node_list_alloc_link(self);

  next = iter->next;

  iter->next = new_link;
  new_link->prev = iter;

  new_link->next = next;

  if (NULL_LINK == next) {
    nusmv_assert(self->tail == iter);
    self->tail = new_link;
  }
  else {
    next->prev = new_link;
  }

  /* Finally set the element */
  new_link->element = elem;

  ++self->size;
  node_list_update_count(self, elem, false);
}

node_ptr NodeList_remove_elem_at(NodeList_ptr self, ListIter_ptr iter)
{
  node_ptr elem;

  NODE_LIST_CHECK_INSTANCE(self);
  nusmv_assert(NULL_LINK != iter);

  elem = iter->element;

  if (NULL_LINK == iter->prev) {
    nusmv_assert(iter == self->head);
    self->head = iter->next;
  }
  else {
    iter->prev->next = iter->next;
  }

  if (NULL_LINK == iter->next) {
    nusmv_assert(iter == self->tail);
    self->tail = iter->prev;
  }
  else {
    iter->next->prev = iter->prev;
  }

  node_list_free_link(self, iter);
  node_list_update_count(self, elem, true);

  --self->size;

  return elem;
}

int NodeList_remove_elems(NodeList_ptr self, const NodeList_ptr other,
                          NodeListPred disposer, void* disposer_arg)
{
  ListIter_ptr iter;
  int count = 0;

  NODE_LIST_CHECK_INSTANCE(self);

  if (NodeList_get_length(other) == 0) {
    return 0;
  }

  iter = NodeList_get_first_iter(self);
  while(!ListIter_is_end(iter)) {
    node_ptr elem = NodeList_get_elem_at(self, iter);
    ListIter_ptr tmp = iter;
    iter = ListIter_get_next(iter);

    /* Chech whether elem has to be removed */
    if (NodeList_belongs_to(other, elem)) {
      NodeList_remove_elem_at(self, tmp);
      count ++;

      if (disposer != NULL) {
        boolean keep;

        keep = disposer(elem, disposer_arg);
        if (!keep) {
          return count;
        }
      }
    }
  }

  return count;
}

int NodeList_foreach(NodeList_ptr self, NODE_LIST_FOREACH_FUN_P foo,
                     void* user_data)
{
  ListIter_ptr iter;
  boolean cont = true;
  int walks = 0;

  NODE_LIST_CHECK_INSTANCE(self);

  iter = NodeList_get_first_iter(self);
  while ( (! ListIter_is_end(iter)) && cont ) {
    cont = foo(self, iter, user_data);
    ++walks;
    iter = ListIter_get_next(iter);
  }

  return walks;
}

int NodeList_get_length(const NodeList_ptr self)
{
  NODE_LIST_CHECK_INSTANCE(self);

  return self->size;
}

boolean NodeList_is_empty(const NodeList_ptr self)
{
  NODE_LIST_CHECK_INSTANCE(self);

  return (0 == self->size);
}

void NodeList_reverse(NodeList_ptr self)
{
  Link* curr = self->head;
  Link* tmp;

  self->head = self->tail;
  self->tail = curr;

  while (NULL_LINK != curr) {
    tmp = curr->next;
    curr->next = curr->prev;
    curr->prev = tmp;

    curr = tmp;
  }
}

void NodeList_concat(NodeList_ptr self, const NodeList_ptr src)
{
  Link* iter;

  NODE_LIST_CHECK_INSTANCE(self);
  NODE_LIST_CHECK_INSTANCE(src);

  iter = src->head;

  while (NULL_LINK != iter) {
    NodeList_append(self, iter->element);
    iter = iter->next;
  }
}

void NodeList_concat_unique(NodeList_ptr self, const NodeList_ptr src)
{
  Link* iter;

  NODE_LIST_CHECK_INSTANCE(self);
  NODE_LIST_CHECK_INSTANCE(src);

  iter = src->head;

  while (NULL_LINK != iter) {

    if (!NodeList_belongs_to(self, iter->element)) {
      NodeList_append(self, iter->element);
    }

    iter = iter->next;
  }
}

boolean NodeList_belongs_to(const NodeList_ptr self, node_ptr elem)
{
  return (Nil != find_assoc(self->count_hash, elem));
}

ListIter_ptr
NodeList_search(const NodeList_ptr self, NodeListPred pred, void* arg)
{
  ListIter_ptr iter;

  if (pred == NULL) {
    if (!NodeList_belongs_to(self, (node_ptr) arg)) return NULL_LINK;

    NODE_LIST_FOREACH(self, iter) {
      if (NodeList_get_elem_at(self, iter) == (node_ptr) arg) return iter;
    }
  }
  else {
    NODE_LIST_FOREACH(self, iter) {
      if (pred(NodeList_get_elem_at(self, iter), arg)) return iter;
    }
  }

  return iter; /* end of list */
}

int NodeList_count_elem(const NodeList_ptr self, node_ptr elem)
{
  return NODE_TO_INT(find_assoc(self->count_hash, elem));
}

ListIter_ptr NodeList_get_first_iter(const NodeList_ptr self)
{
  return self->head;
}

node_ptr NodeList_get_elem_at(const NodeList_ptr self, const ListIter_ptr iter)
{
  nusmv_assert(NULL_LINK != iter);
  return iter->element;
}

ListIter_ptr ListIter_get_next(const ListIter_ptr self)
{
  nusmv_assert(NULL_LINK != self);

  return self->next;
}

boolean ListIter_is_end(const ListIter_ptr self)
{
  return (NULL_LINK == self);
}

ListIter_ptr ListIter_get_end(void)
{
  return NULL_LINK;
}

NodeList_ptr NodeList_map(const NodeList_ptr self, NPFN foo)
{
  NodeList_ptr res;
  ListIter_ptr iter;

  NODE_LIST_CHECK_INSTANCE(self);
  res = NodeList_create();
  for (iter=NodeList_get_first_iter(self); !ListIter_is_end(iter);
       iter=ListIter_get_next(iter)) {
    NodeList_append(res, foo(NodeList_get_elem_at(self, iter)));
  }

  return res;
}

NodeList_ptr NodeList_filter(const NodeList_ptr self, BPFN foo)
{
  NodeList_ptr res;
  ListIter_ptr iter;

  NODE_LIST_CHECK_INSTANCE(self);
  res = NodeList_create();
  for (iter=NodeList_get_first_iter(self); !ListIter_is_end(iter);
       iter=ListIter_get_next(iter)) {
    node_ptr el = NodeList_get_elem_at(self, iter);
    if (foo(el)) NodeList_append(res, el);
  }

  return res;
}

void NodeList_print_nodes(const NodeList_ptr self, MasterPrinter_ptr printer,
                          FILE* out)
{
  ListIter_ptr iter;
  for (iter = NodeList_get_first_iter(self);
       !ListIter_is_end(iter); iter = ListIter_get_next(iter)) {
    print_node(printer, out, NodeList_get_elem_at(self, iter));
    fprintf(out, " ");
  }
}

void NodeList_sort(NodeList_ptr self,
                   int (*cmp)(const void* el1, const void* el2))
{
  unsigned long input_size = self->size;

  /* If less than 2 elements, there is nothing to sort */
  if (self->size > 1) {
    int j;
    void** array;
    ListIter_ptr iter;

    array = ALLOC(void*, self->size);
    nusmv_assert(NULL != array);

    for (iter=NodeList_get_first_iter(self), j = 0;
         ! NodeList_is_empty(self);
         iter=NodeList_get_first_iter(self), ++j) {
      array[j] = NodeList_remove_elem_at(self, iter); 
    }

    qsort((void*)array, (size_t)input_size, sizeof(void*), cmp);

    nusmv_assert(j > 0);

    do {
      NodeList_prepend(self, array[j-1]);
    } while (--j);

    FREE(array);
  }

  nusmv_assert(input_size == self->size);
}

int NodeList_test(NuSMVEnv_ptr env)
{
  int retval = 0;
  NodeList_ptr list;
  
  printf("BEGIN TEST 001\n");
  list = NodeList_create();
  node_list_fill_from_strings(list, 4, "1", "0", "2", "1");
  NodeList_sort(list, Utils_ptr_compar);
  node_list_test(list);
  NodeList_destroy(list);
  printf("\nEND TEST 001\n");

  printf("BEGIN TEST 001\n");
  list = NodeList_create();
  NodeList_sort(list, Utils_ptr_compar);
  node_list_test(list);
  NodeList_destroy(list);
  printf("\nEND TEST 001\n");

  printf("BEGIN TEST 001\n");
  list = NodeList_create();
  node_list_fill_from_strings(list, 1, "1");
  NodeList_sort(list, Utils_ptr_compar);
  node_list_test(list);
  NodeList_destroy(list);
  printf("\nEND TEST 001\n");

  printf("BEGIN TEST 001\n");
  list = NodeList_create();
  node_list_fill_from_strings(list, 4, "1", "2", "3", "4");
  NodeList_sort(list, Utils_ptr_compar);
  node_list_test(list);
  NodeList_destroy(list);
  printf("\nEND TEST 001\n");

  printf("BEGIN TEST 001\n");
  list = NodeList_create();
  node_list_fill_from_strings(list, 4, "4", "3", "2", "1");
  NodeList_sort(list, Utils_ptr_compar);
  node_list_test(list);
  NodeList_destroy(list);
  printf("\nEND TEST 001\n");

  return retval;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Private initializer

  
*/
static void node_list_init(NodeList_ptr self, node_ptr list)
{

  self->head = NULL_LINK;
  self->tail = NULL_LINK;

  if (0 == reference_counter) {
    nusmv_assert(NULL_LINK == pool);
    nusmv_assert(STACK(NULL) == chunks);

    pool = NULL_LINK;
    chunks = Stack_create();
  }

  ++reference_counter;

  self->count_hash = new_assoc();
  self->size = 0;

  while (Nil != list) {
    NodeList_append(self, car(list));
    list = cdr(list);
  }
}

/*!
  \brief Private deinitializer

  
*/
static void node_list_deinit(NodeList_ptr self)
{

  nusmv_assert(reference_counter > 0);
  --reference_counter;

  if (0 == reference_counter) {
    nusmv_assert(NULL_LINK != pool);
    nusmv_assert(STACK(NULL) != chunks);

    while (!Stack_is_empty(chunks)) {
      Link* chunk = Stack_pop(chunks);
      FREE(chunk);
    }

    Stack_destroy(chunks);

    chunks = STACK(NULL);
    pool = NULL_LINK;
  }
  else {
    Link* l = self->head;
    Link* t;
    int i = 0;

    while (NULL_LINK != l) {
      t = l->next;
      node_list_free_link(self, l);
      l = t; ++i;
    }

    nusmv_assert(i == self->size);
  }

  free_assoc(self->count_hash);
}

/*!
  \brief Retrieves a Link instance in the pool

  Retrieves a Link instance in the pool.
                      If the pool is empty, allocates LINK_CHUNK_SIZE
                      new Link instances (by using only 1 ALLOC)
*/
static inline Link* node_list_alloc_link(const NodeList_ptr self)
{
  Link* res = NULL_LINK;

  nusmv_assert(STACK(NULL) != chunks);

  if (NULL_LINK == pool) {
    Link* chunk = ALLOC(Link, LINK_CHUNK_SIZE);
    int i;

    Stack_push(chunks, chunk);
    pool = chunk;

    for (i = 0; i < LINK_CHUNK_SIZE - 1; ++i) {
      (chunk + i)->next = (chunk + i + 1);
    }

    (chunk + i)->next = NULL_LINK;
  }

  res = pool;
  pool = res->next;

  nusmv_assert(NULL_LINK != res);

  res->prev = NULL_LINK;
  res->next = NULL_LINK;
  res->element = Nil;

  return res;
}

/*!
  \brief Puts the given Link instance in the pool

  Puts the given Link instance in the pool
*/
static inline void node_list_free_link(const NodeList_ptr self, Link* link)
{
  Link* tmp = pool;
  pool = link;
  link->next = tmp;
}

/*!
  \brief Keeps count of the number of duplicate elements
                      in the list

  Keeps count of the number of duplicate elements
                      in the list
*/
static inline void
node_list_update_count(const NodeList_ptr self, const node_ptr elem,
                       const boolean deleting)
{
  int val = NODE_TO_INT(find_assoc(self->count_hash, elem));
  insert_assoc(self->count_hash, elem,
               (deleting ? NODE_FROM_INT(val - 1) : NODE_FROM_INT(val + 1)));
}

/*!
  \brief Insert into self the elements given as vararg

  Testing function. Elements have to be naturals given as
  strings
*/

static void node_list_fill_from_strings(NodeList_ptr self,
                                        unsigned int size,
                                        ...)
{
  unsigned int i;
  va_list args;
  va_start(args, size);

  for (i = 0; i < size; ++i) {
    char* elem = va_arg(args, char*);
    int elem_int = 0;
    (void)util_str2int(elem, &elem_int);
    NodeList_append(self, NODE_FROM_INT(elem_int));
  }

  va_end(args);
}

/*!
  \brief Check if a list is sorted

  Testing function
*/

static int node_list_test(NodeList_ptr self)
{
  int retval = 0;
  int last_elem = INT_MIN;
  ListIter_ptr iter;

  NODE_LIST_FOREACH(self, iter) {
    int elem = NODE_TO_INT(NodeList_get_elem_at(self, iter));

    printf("%d ", elem);
    fflush(NULL);

    /* for now using assert, if needed a return value can be returned */
    assert(elem >= last_elem);

    last_elem = elem;
  }

  return retval;
}
