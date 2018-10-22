/* ---------------------------------------------------------------------------


  This file is part of the ``utils'' package.
  %COPYRIGHT%

-----------------------------------------------------------------------------*/

/*!
  \author Andrei Tchaltsev
  \brief Implementation of DLlist class

  DLlist class is a Doubly-Linked list of pointers.
  

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/DLlist.h"
#include <stddef.h> /* for offsetof */

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

struct DLnode_TAG {
  void* element; /* an element stored in this node */
  DLnode_ptr prev; /* previous node of a list */
  DLnode_ptr next; /* next node of a list */
};

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef struct DLnode_TAG DLnode;

struct DLlist_TAG {
  DLnode past_last; /* this node is past the last one. Its "prev"
                      points to the last element of a list (if any),
                      "next" is always NULL and "element" is not used.
                      It is better to have this field at the beginning of a struct
                      because then taking the address of this field is
                      simplified to taking address to the whole struct.
                   */
  DLnode_ptr first; /* pointer to a first node of the list */
  int size; /* the size of the list */
};

typedef struct DLlist_TAG DLlist;


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/
/* Macro DEBUG is used for debugging the functions in this file */
#if 1

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define DEBUG(a)  a
#else
#define DEBUG(a)
#endif

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void dl_list_init(DLlist_ptr self);
static void dl_list_deinit(DLlist_ptr self);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

DLlist_ptr DLlist_create(void)
{
  DLlist_ptr self = ALLOC(DLlist, 1);

  DL_LIST_CHECK_INSTANCE(self);

  dl_list_init(self);
  return self;
}

void DLlist_destroy (DLlist_ptr self)
{
  DL_LIST_CHECK_INSTANCE(self);

  dl_list_deinit(self);
  FREE(self);
}

DLlist_ptr DLlist_copy(DLlist_ptr self)
{
  DLlist_ptr retval = DLlist_create();
  /* copying elements from the end to beginning */
  DLnode_ptr new = &(retval->past_last);
  DLnode_ptr old;

  DL_LIST_CHECK_INSTANCE(self);

  for (old = self->past_last.prev; old != NULL; old = old->prev) {
    DLnode_ptr tmp = ALLOC(DLnode, 1);
    tmp->element = old->element;
    tmp->next = new;
    new->prev = tmp;
    new = tmp;
  }

  new->prev = NULL;
  retval->first = new;
  retval->size = self->size;

  return retval;
}

DLlist_ptr DLlist_copy_reversed(DLlist_ptr self)
{
  DLlist_ptr retval = DLlist_create();
  DLnode_ptr new = &(retval->past_last);
  DLnode_ptr old;

  DL_LIST_CHECK_INSTANCE(self);

  for (old = self->first; old != &(self->past_last); old = old->next) {
    DLnode_ptr tmp = ALLOC(DLnode, 1);

    tmp->element = old->element;
    tmp->next = new;
    new->prev = tmp;
    new = tmp;
  }

  new->prev = NULL;
  retval->first = new;
  retval->size = self->size;

  return retval;
}

void DLlist_reverse(DLlist_ptr self)
{
  DLnode_ptr new = &(self->past_last);
  DLnode_ptr old = self->first;

  DL_LIST_CHECK_INSTANCE(self);

  while (old != &(self->past_last)) {
    DLnode_ptr tmp = old;
    old = old->next;
    tmp->next = new;
    new->prev = tmp;
    new = tmp;
  }
  new->prev = NULL;
  self->first = new;;
}

void DLlist_prepend(DLlist_ptr self, void* element)
{
  /* create and initialize a new node */
  DLnode_ptr node = ALLOC(DLnode, 1);

  DL_LIST_CHECK_INSTANCE(self);

  node->element = element;
  node->prev = NULL;
  node->next = self->first;
  self->first->prev = node;
  self->first = node;
  self->size += 1;
}

void DLlist_append(DLlist_ptr self, void* element)
{
  /* create and initialize a new node */
  DLnode_ptr node = ALLOC(DLnode, 1);

  DL_LIST_CHECK_INSTANCE(self);

  node->element = element;
  node->prev = self->past_last.prev;
  node->next = &(self->past_last);
  self->past_last.prev = node;

  /* update the first element if required or previous otherwise*/
  if (&(self->past_last) == self->first) {
    self->first = node;
  }
  else {
    node->prev->next = node;
  }
  self->size += 1;
}

void* DLlist_delete_first(DLlist_ptr self)
{
  DLnode_ptr node;
  void* element;

  DL_LIST_CHECK_INSTANCE(self);
  nusmv_assert(self->first != &(self->past_last)); /* list must not be empty */

  node = self->first;
  element = node->element;
  self->first = node->next;
  self->first->prev = NULL;

  node->element = PTR_FROM_INT(void*, 1); /* for sure incorrect value for debugging */
  node->prev = PTR_FROM_INT(void*, 1); /* for sure incorrect value for debugging */
  node->next = PTR_FROM_INT(void*, 1); /* for sure incorrect value for debugging */
  FREE(node);
  self->size -= 1;
  nusmv_assert(self->size >= 0); /* the size cannot be negative */

  return element;
}

void* DLlist_delete_last(DLlist_ptr self)
{
  DLnode_ptr node;
  void* element;

  nusmv_assert(self->first != &(self->past_last)); /* list must not be empty */

  node = self->past_last.prev;
  element = node->element;
  self->past_last.prev = node->prev;

  /* update the first element if first == last */
  if (self->first == node) {
    self->first = &(self->past_last);
  }
  else {
    node->prev->next = &(self->past_last);
  }

  node->element = PTR_FROM_INT(void*, 1); /* for sure incorrect value for debugging */
  node->prev = PTR_FROM_INT(void*, 1); /* for sure incorrect value for debugging */
  node->next = PTR_FROM_INT(void*, 1); /* for sure incorrect value for debugging */
  FREE(node);
  self->size -= 1;
  nusmv_assert(self->size >= 0); /* the size cannot be negative */

  return element;
}

int DLlist_get_size(DLlist_ptr self)
{
  DL_LIST_CHECK_INSTANCE(self);
  nusmv_assert(self->size >= 0); /* the size cannot be negative */
  nusmv_assert((self->first == &(self->past_last)) == (0 == self->size));

  return self->size;
}

boolean DLlist_is_empty(DLlist_ptr self)
{
  DL_LIST_CHECK_INSTANCE(self);
  nusmv_assert(self->size >= 0); /* the size cannot be negative */
  nusmv_assert((self->first == &(self->past_last)) == (0 == self->size));
  return self->size == 0;
}

DLiter DLlist_first(DLlist_ptr self)
{
  DLiter iter;
  /* the iterator point to a corresponding node of a list or
   self->past_last (i.e. past the last element of a list) */

  DL_LIST_CHECK_INSTANCE(self);

  iter.node = self->first;
  return iter;
}

DLiter DLlist_end(DLlist_ptr self)
{
  DLiter iter;
  /* the iterator point to a corresponding node of a list or
   NULL (i.e. past the list) */

  DL_LIST_CHECK_INSTANCE(self);

  iter.node = &(self->past_last);
  return iter;
}

boolean DLiter_is_first(DLiter self)
{
  return self.node->prev == NULL /* it is the first element */
    && self.node->next != NULL /* it is not past the last one */;
}

boolean DLiter_is_end(DLiter self)
{
  return NULL == self.node->next;
}

DLiter DLiter_next(DLiter self)
{
  DLiter new;

  nusmv_assert(self.node->next != NULL); /* iterator is past the last element */

  new.node = self.node->next;
  return new;
}

DLiter DLiter_prev(DLiter self)
{
  DLiter new;

  nusmv_assert(self.node->prev != NULL); /* iterator points to the first */

  new.node = self.node->prev;
  return new;
}

void* DLiter_element(DLiter self)
{
  nusmv_assert(self.node->next != NULL); /* iterator is past the last element */
  return self.node->element;
}

DLiter DLlist_insert_after(DLlist_ptr self, DLiter iter, void* element)
{
  DLnode_ptr node = ALLOC(DLnode, 1);

  DL_LIST_CHECK_INSTANCE(self);
  nusmv_assert(iter.node->next != NULL); /* points past the last element */

  node->element = element;
  node->next = iter.node->next;
  iter.node->next->prev = node;

  iter.node->next = node;
  node->prev = iter.node;

  self->size += 1;

  iter.node = iter.node->next;
  return iter;
}

DLiter DLlist_insert_before(DLlist_ptr self, DLiter iter, void* element)
{
  DLnode_ptr node = ALLOC(DLnode, 1);

  DL_LIST_CHECK_INSTANCE(self);

  node->element = element;
  node->next = iter.node;
  node->prev = iter.node->prev;

  iter.node->prev = node;

  if (NULL == node->prev) { /* update the first element */
    self->first = node;
  }
  else {
    node->prev->next = node;
  }

  self->size += 1;

  iter.node = iter.node->prev;
  return iter;
}

DLiter DLlist_delete(DLlist_ptr self, DLiter iter, void** element)
{
  DLnode_ptr node = iter.node;

  DL_LIST_CHECK_INSTANCE(self);
  nusmv_assert(iter.node->next != NULL); /* should not point past the last element */

  if (element != NULL) *element = node->element;

  node->next->prev = node->prev;

  if (NULL == node->prev) { /* the first element was removed => care required */
    self->first = node->next;
  }
  else {
    node->prev->next = node->next;
  }
  iter.node = node->next;

  node->element = PTR_FROM_INT(void*, 1); /* for sure incorrect value for debugging */
  node->prev = PTR_FROM_INT(void*, 1); /* for sure incorrect value for debugging */
  node->next = PTR_FROM_INT(void*, 1); /* for sure incorrect value for debugging */
  FREE(node);

  self->size -= 1;
  nusmv_assert(self->size >= 0);

  return iter;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void dl_list_testing_function(StreamMgr_ptr streams)
{
  DLlist_ptr list1, list2, keeper;
  DLiter iter1;
  void* v1 = PTR_FROM_INT(void*, 1);
  void* v2 = PTR_FROM_INT(void*, 2);
  void* v3 = PTR_FROM_INT(void*, 3);
  void* v4 = PTR_FROM_INT(void*, 4);
  void* elem;

  StreamMgr_print_output(streams, "TESTING DLlist class : in process ....\n");

  list1 = DLlist_create();                  keeper = list1;
  DLlist_destroy(list1);

  list1 = DLlist_create();
  DLlist_append(list1, v1);
  nusmv_assert(DLiter_element(DLlist_first(list1)) == v1);
  DLlist_destroy(list1);

  list1 = DLlist_create();
  DLlist_prepend(list1, v1);
  nusmv_assert(DLiter_element(DLlist_first(list1)) == v1);
  DLlist_destroy(list1);

  list1 = DLlist_create();
  DLlist_append(list1, v1);
  DLlist_prepend(list1, v2);
  nusmv_assert(DLiter_element(DLlist_first(list1)) == v2);
  DLlist_destroy(list1);

  list1 = DLlist_create();
  nusmv_assert(DLlist_is_empty(list1));
  nusmv_assert(0 == DLlist_get_size(list1));
  DLlist_prepend(list1, v1);
  DLlist_append(list1, v2);
  nusmv_assert(DLiter_element(DLlist_first(list1)) == v1);
  nusmv_assert(!DLlist_is_empty(list1));
  nusmv_assert(2 == DLlist_get_size(list1));
  elem = DLlist_delete_first(list1);
  nusmv_assert(elem == v1);
  nusmv_assert(DLiter_element(DLlist_first(list1)) == v2);
  nusmv_assert(!DLlist_is_empty(list1));
  nusmv_assert(1 == DLlist_get_size(list1));
  elem = DLlist_delete_first(list1);
  nusmv_assert(elem == v2);
  nusmv_assert(DLlist_is_empty(list1));
  nusmv_assert(0 == DLlist_get_size(list1));
  DLlist_destroy(list1);

  list1 = DLlist_create();
  nusmv_assert(DLlist_is_empty(list1));
  nusmv_assert(0 == DLlist_get_size(list1));
  DLlist_append(list1, v2);
  DLlist_prepend(list1, v1);
  nusmv_assert(DLiter_element(DLiter_prev(DLlist_end(list1))) == v2);
  nusmv_assert(!DLlist_is_empty(list1));
  nusmv_assert(2 == DLlist_get_size(list1));
  elem = DLlist_delete_last(list1);
  nusmv_assert(elem == v2);
  nusmv_assert(DLiter_element(DLiter_prev(DLlist_end(list1))) == v1);
  nusmv_assert(!DLlist_is_empty(list1));
  nusmv_assert(1 == DLlist_get_size(list1));
  elem = DLlist_delete_last(list1);
  nusmv_assert(elem == v1);
  nusmv_assert(DLlist_is_empty(list1));
  nusmv_assert(0 == DLlist_get_size(list1));
  DLlist_destroy(list1);

  list1 = DLlist_create();
  DLlist_prepend(list1, v1);
  DLlist_prepend(list1, v2);
  DLlist_prepend(list1, v3);
  iter1 = DLlist_first(list1);
  nusmv_assert(!DLiter_is_end(iter1));
  nusmv_assert(DLiter_element(iter1) == v3);
  iter1 = DLiter_next(iter1);
  nusmv_assert(!DLiter_is_end(iter1));
  nusmv_assert(DLiter_element(iter1) == v2);
  iter1 = DLiter_next(iter1);
  nusmv_assert(!DLiter_is_end(iter1));
  nusmv_assert(DLiter_element(iter1) == v1);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_is_end(iter1));
  DLlist_destroy(list1);

  list1 = DLlist_create();
  DLlist_append(list1, v1);
  DLlist_append(list1, v2);
  DLlist_append(list1, v3);

  iter1 = DLlist_first(list1);
  nusmv_assert(DLiter_is_first(iter1));
  nusmv_assert(!DLiter_is_end(iter1));
  nusmv_assert(DLiter_element(iter1) == v1);
  iter1 = DLiter_next(iter1);
  nusmv_assert(!DLiter_is_first(iter1));
  nusmv_assert(!DLiter_is_end(iter1));
  nusmv_assert(DLiter_element(iter1) == v2);
  iter1 = DLiter_next(iter1);
  nusmv_assert(!DLiter_is_first(iter1));
  nusmv_assert(!DLiter_is_end(iter1));
  nusmv_assert(DLiter_element(iter1) == v3);
  iter1 = DLiter_next(iter1);
  nusmv_assert(!DLiter_is_first(iter1));
  nusmv_assert(DLiter_is_end(iter1));

  iter1 = DLlist_end(list1);
  nusmv_assert(!DLiter_is_first(iter1));
  nusmv_assert(DLiter_is_end(iter1));
  iter1 = DLiter_prev(iter1);
  nusmv_assert(!DLiter_is_first(iter1));
  nusmv_assert(!DLiter_is_end(iter1));
  nusmv_assert(DLiter_element(iter1) == v3);
  iter1 = DLiter_prev(iter1);
  nusmv_assert(!DLiter_is_first(iter1));
  nusmv_assert(!DLiter_is_end(iter1));
  nusmv_assert(DLiter_element(iter1) == v2);
  iter1 = DLiter_prev(iter1);
  nusmv_assert(DLiter_is_first(iter1));
  nusmv_assert(!DLiter_is_end(iter1));
  nusmv_assert(DLiter_element(iter1) == v1);

  DLlist_destroy(list1);

  list2 = DLlist_create();
  DLlist_append(list2, v1);
  DLlist_append(list2, v2);
  DLlist_append(list2, v3);
  list1 = DLlist_copy(list2);
  DLlist_destroy(list2);
  iter1 = DLlist_first(list1);
  nusmv_assert(DLiter_element(iter1) == v1);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_element(iter1) == v2);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_element(iter1) == v3);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_is_end(iter1));
  DLlist_destroy(list1);

  list2 = DLlist_create();
  DLlist_append(list2, v1);
  DLlist_append(list2, v2);
  DLlist_append(list2, v3);
  list1 = DLlist_copy_reversed(list2);
  DLlist_destroy(list2);
  iter1 = DLlist_first(list1);
  nusmv_assert(DLiter_element(iter1) == v3);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_element(iter1) == v2);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_element(iter1) == v1);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_is_end(iter1));
  DLlist_destroy(list1);

  list1 = DLlist_create();
  DLlist_append(list1, v1);
  DLlist_append(list1, v2);
  DLlist_append(list1, v3);
  DLlist_reverse(list1);
  iter1 = DLlist_first(list1);
  nusmv_assert(DLiter_element(iter1) == v3);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_element(iter1) == v2);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_element(iter1) == v1);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_is_end(iter1));
  DLlist_destroy(list1);

  list1 = DLlist_create();
  DLlist_append(list1, v2);
  DLlist_append(list1, v4);
  iter1 = DLlist_first(list1);
  iter1 = DLlist_insert_after(list1, iter1, v3);
  nusmv_assert(DLiter_element(iter1) == v3);
  iter1 = DLiter_prev(iter1);
  iter1 = DLlist_insert_before(list1, iter1, v1);
  nusmv_assert(DLiter_element(iter1) == v1);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_element(iter1) == v2);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_element(iter1) == v3);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_element(iter1) == v4);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_is_end(iter1));
  DLlist_destroy(list1);

  list1 = DLlist_create();
  DLlist_append(list1, v1);
  DLlist_append(list1, v3);
  iter1 = DLiter_next(DLlist_first(list1));
  iter1 = DLlist_insert_after(list1, iter1, v4);
  nusmv_assert(DLiter_element(iter1) == v4);
  iter1 = DLiter_next(DLlist_first(list1));
  iter1 = DLlist_insert_before(list1, iter1, v2);
  nusmv_assert(DLiter_element(iter1) == v2);
  iter1 = DLlist_first(list1);
  nusmv_assert(DLiter_element(iter1) == v1);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_element(iter1) == v2);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_element(iter1) == v3);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_element(iter1) == v4);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_is_end(iter1));
  DLlist_destroy(list1);

  list1 = DLlist_create();
  DLlist_append(list1, v1);
  DLlist_append(list1, v2);
  iter1 = DLiter_next(DLiter_next(DLlist_first(list1)));
  iter1 = DLlist_insert_before(list1, iter1, v3);
  nusmv_assert(DLiter_element(iter1) == v3);
  iter1 = DLlist_first(list1);
  nusmv_assert(DLiter_element(iter1) == v1);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_element(iter1) == v2);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_element(iter1) == v3);
  iter1 = DLiter_next(iter1);
  nusmv_assert(DLiter_is_end(iter1));
  DLlist_destroy(list1);

  list1 = DLlist_create();
  DLlist_append(list1, v1);
  DLlist_append(list1, v2);
  DLlist_append(list1, v3);
  DLlist_append(list1, v4);
  iter1 = DLlist_first(list1);
  iter1 = DLlist_delete(list1, iter1, &elem);
  nusmv_assert(elem == v1);
  nusmv_assert(DLiter_element(iter1) == v2);
  iter1 = DLiter_next(iter1);
  iter1 = DLlist_delete(list1, iter1, &elem);
  nusmv_assert(elem == v3);
  nusmv_assert(DLiter_element(iter1) == v4);
  iter1 = DLlist_delete(list1, iter1, &elem);
  nusmv_assert(elem == v4);
  nusmv_assert(DLiter_is_end(iter1));
  nusmv_assert(DLlist_get_size(list1) == 1);
  DLlist_destroy(list1);

  /* This assert is based on a guess that the memory is allocated
     always the same way.  Since all the memory allocated in this
     function is freed this allocation should be exactly the same as
     the first allocation of a list.
  */
  list1 = DLlist_create();                  nusmv_assert(keeper == list1);
  DLlist_destroy(list1);

  StreamMgr_print_output(streams, "TESTING DLlist class : DONE\n");
  return;
}



/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Initializes the memory for a listt instance

  
*/
static void dl_list_init(DLlist_ptr self)
{
  self->past_last.prev = NULL;
  self->past_last.next = NULL;
  self->past_last.element = NULL; /* for debugging only. This field is not used */
  self->first = &(self->past_last);
  self->size = 0;
}

/*!
  \brief Deinitializes the memory from a list

  
*/
static void dl_list_deinit(DLlist_ptr self)
{
  DLnode_ptr node = self->first;

  while (node != &(self->past_last)) {
    DLnode_ptr tmp = node;
    node = tmp->next;
    tmp->prev = PTR_FROM_INT(void*, 1); /* for sure incorrect value for debugging */
    tmp->next = PTR_FROM_INT(void*, 1); /* for sure incorrect value for debugging */
    FREE(tmp);

    /* decrease the size of a list */
    self->size -= 1;
    /* there is a problem with memory. It is likely iter
       of one list was used with another list in insert or delete functions */
    nusmv_assert(self->size >= 0);
  };

  nusmv_assert(0 == self->size);/* there is a problem with size */

  self->first = PTR_FROM_INT(void*, 1); /* for sure incorrect value for debugging */
  self->past_last.prev = PTR_FROM_INT(void*, 1); /* for sure incorrect value for debugging */
  self->size = -1; /* for sure incorrect value for debugging */
}

