/* ---------------------------------------------------------------------------


  This file is part of the ``addon.omcare'' package of NuSMV version 2.
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
  \author Andrei Tchaltsev
  \brief Implementation of Slist (Simple List) class

  Slist class is very simple one-directional list
  of pointers with modest functionality but as efficient implementation
  as possible.

  The main functionalities are:
  * it is possible to add and remove an element at the beginning
  of a list.
  * iterate over elements from beginning to the end.
  * reverse a list or create a reversed copy.

  To have more functionalities you can use Olist and have slightly less efficient
  implementation.

*/


#include "nusmv/core/utils/Slist.h"

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

struct Slist_TAG {
  unsigned int size;
  Snode_ptr first; /* first node of the list */
};

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef struct Slist_TAG Slist;

struct Snode_TAG {
  void* element; /* an element stored in this node */
  Snode_ptr next; /* next node of a list */
};

typedef struct Snode_TAG Snode;


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/
/* Macro DEBUG is used for debugging the function in this file */
#if 0

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
static void slist_init(Slist_ptr self);
static void slist_deinit(Slist_ptr self);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

Slist_ptr Slist_create(void)
{
  Slist_ptr self = ALLOC(Slist, 1);
  slist_init(self);
  return self;
}

void Slist_destroy (Slist_ptr self)
{
  slist_deinit(self);
  FREE(self);
}

Slist_ptr Slist_copy(Slist_ptr self)
{
  Slist_ptr new_list = Slist_create();
  Snode_ptr* new = &(new_list->first);

  Snode_ptr old;
  for (old = self->first; old != NULL; old = old->next) {
    *new = ALLOC(Snode, 1);
    (*new)->element = old->element;
    new = &((*new)->next);
   }
  (*new) = NULL;
  new_list->size = self->size;
  return new_list;
}

Slist_ptr Slist_copy_reversed(Slist_ptr self)
{
  Snode_ptr new = NULL;
  Slist_ptr new_list;

  Snode_ptr old;
  for (old = self->first; old != NULL; old = old->next) {
    Snode_ptr tmp = ALLOC(Snode, 1);
    tmp->element = old->element;
    tmp->next = new;
    new = tmp;
  }

  new_list = Slist_create();
  new_list->first = new;
  new_list->size = self->size;
  return new_list;
}

void Slist_reverse(Slist_ptr self)
{
  Snode_ptr old = self->first;
  Snode_ptr new = NULL;

  while (old) {
    Snode_ptr tmp = old;
    old = old->next;
    tmp->next = new;
    new = tmp;
  }
  self->first = new;
}

void Slist_push(Slist_ptr self, void* element)
{
  /* create and initialize a new node */
  Snode_ptr node = ALLOC(Snode, 1);
  node->element = element;
  node->next = self->first;
  self->first = node;
  self->size += 1;
}

unsigned int Slist_get_size(Slist_ptr self)
{
  return self->size;
}

void* Slist_pop(Slist_ptr self)
{
  void* element;
  Snode_ptr node = self->first;

  self->first = node->next;
  element = node->element;
  FREE(node);

  self->size -= 1;
  return element;
}

void* Slist_top(Slist_ptr self)
{
  Snode_ptr node = self->first;
  void* element = node->element;
  return element;
}

boolean Slist_is_empty(Slist_ptr self)
{
  return NULL == self->first;
}

boolean Slist_equals(const Slist_ptr self, const Slist_ptr other)
{
  Siter si, oi;

  if (self == other) {
    return true;
  }

  /* Different size => not equal */
  if (Slist_get_size(self) != Slist_get_size(other)) {
    return false;
  }

  /* Two empty lists are equal */
  if (Slist_get_size(self) == 0) {
    return true;
  }

  si = Slist_first(self);
  oi = Slist_first(other);

  while(!Siter_is_end(si)) {
    if(Siter_element(si) != Siter_element(oi)) {
      return false;
    }

    si = Siter_next(si);
    oi = Siter_next(oi);
  }

  return true;
}

Siter Slist_first(Slist_ptr self)
{
  Siter iter;
  iter.node = self->first;
  return iter;
}

Siter Slist_end()
{
  Siter iter;
  iter.node = NULL;
  return iter;
}

boolean Siter_is_end(Siter iter)
{
  return NULL == iter.node;
}

boolean Siter_is_last(Siter iter)
{
  if (Siter_is_end(iter)) return false;
  return Siter_is_end(Siter_next(iter));
}

Siter Siter_next(Siter iter)
{
  Siter new;
  new.node = iter.node->next;
  return new;
}

void* Siter_element(Siter iter)
{
  nusmv_assert(iter.node != NULL); /* iterator is past the last element */
  return (iter.node)->element;
}

Siter Slist_find(Slist_ptr self, const void* element)
{
  Siter iter;
  for (iter = Slist_first(self); !Siter_is_end(iter); iter = Siter_next(iter)) {
    if (Siter_element(iter) == element) break;
  }
  return iter;
}

boolean Slist_contains(Slist_ptr self, const void* element)
{
  return !Siter_is_end(Slist_find(self, element));
}

boolean Slist_remove(Slist_ptr self, const void* element)
{
  Snode_ptr current, previous;
  boolean res;

  SLIST_CHECK_INSTANCE(self);

  res = false;
  previous = (Snode_ptr) NULL;
  current = self->first;
  while(current != (Snode_ptr)NULL) {
    if (current->element == element) {
      Snode_ptr next;

      next = current->next;

      if ((Snode_ptr)NULL != previous) {
        previous->next = next;
      }
      else {
        /* We are going to remove the first element */
        self->first = next;
      }
      FREE(current);
      --self->size;
      res = true;
      current = next; /* Do not change previous */
    }
    else {
      previous = current;
      current = current->next;
    }
  }

  return res;
}

void Slist_destroy_and_free_elements(Slist_ptr self,
                                     Slist_free_function f)
{
  Siter iter;
  SLIST_CHECK_INSTANCE(self);

  for (iter = Slist_first(self); !Siter_is_end(iter); iter = Siter_next(iter)) {
    f(Siter_element(iter));
  }

  Slist_destroy(self);
}

void Slist_clear(Slist_ptr self)
{
  Siter iter;
  SLIST_CHECK_INSTANCE(self);

  for (iter = Slist_first(self); !Siter_is_end(iter); iter = Siter_next(iter)) {
    Slist_pop(self);
  }
}

void Slist_append(Slist_ptr self, const Slist_ptr other)
{
  Siter iter;
  SLIST_CHECK_INSTANCE(self);

  for (iter = Slist_first(other); !Siter_is_end(iter); iter = Siter_next(iter)) {
    Slist_push(self, Siter_element(iter));
  }
}

void Slist_sort(Slist_ptr self,
                int (*cmp)(void* el1, void* el2, void* extra),
                void* extra)
{
  Snode_ptr list = self->first;
  Snode_ptr tail;
  int insize;

  if (NULL == list)
    return; /* no change */

  insize = 1;

  while (true) {
    Snode_ptr p = list;
    int merges = 0; /* count number of merges we do in this pass */

    list = tail = NULL;
    while (NULL != p) {
      Snode_ptr q = p;
      int psize, qsize, i;

      merges += 1;  /* there exists a merge to be done */

      psize = 0;
      /* step `insize' places along from p */
      for (i = 0; i < insize; ++i) {
        psize += 1;
        q = q->next;
        if (NULL == q) break;
      }

      /* if q hasn't fallen off end, we have two lists to merge */
      qsize = insize;

      /* now we have two lists; merge them */
      while (psize > 0 || (qsize > 0 && NULL != q)) {
        Snode_ptr e;

        /* decide whether next element of merge comes from p or q */
        if (psize == 0) {
          /* p is empty; e must come from q. */
          e = q; q = q->next; qsize--;
        } else if (qsize == 0 || NULL == q) {
          /* q is empty; e must come from p. */
          e = p; p = p->next; psize--;
        } else if (cmp(p->element, q->element, extra) <= 0) {
          /* First element of p is lower (or same);
           * e must come from p. */
          e = p; p = p->next; psize--;
        } else {
          /* First element of q is lower; e must come from q. */
          e = q; q = q->next; qsize--;
        }

        /* add the next element to the merged list */
        if (NULL != tail) tail->next = e;
        else list = e;
        tail = e;
      }

      /* now p has stepped `insize' places along, and q has too */
      p = q;
    }
    tail->next = NULL;

    /* If we have done only one merge, we're finished. */
    if (merges <= 1) break;

    /* keep going merging twice the size */
    insize <<= 1;
  }

  self->first = list;
}

void Slist_print(Slist_ptr const self,
                 MasterPrinter_ptr wffprint,
                 void (*printer)(FILE* file, void* el, void* arg),
                 FILE* file,
                 void* printer_arg)
{
  Siter iter;

  SLIST_CHECK_INSTANCE(self);
  MASTER_PRINTER_CHECK_INSTANCE(wffprint);
  nusmv_assert(NULL != file);

  SLIST_FOREACH(self, iter) {
    void* element = Siter_element(iter);

    if (NULL == printer) {
      print_node(wffprint, file, NODE_PTR(element));
    }
    else printer(file, element, printer_arg);

    fprintf(file, "\n");
  }
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Initializes the memory for a list instance


*/
static void slist_init(Slist_ptr self)
{
  SLIST_CHECK_INSTANCE(self);
  self->size = 0;
  self->first = NULL;
}

/*!
  \brief Deinitializes the memory from a list


*/
static void slist_deinit(Slist_ptr self)
{
  Snode_ptr node;

  SLIST_CHECK_INSTANCE(self);

  node = self->first;
  while (node) {
    Snode_ptr tmp = node;
    node = tmp->next;
    FREE(tmp);
  };
  self->size = 0;
  self->first = NULL; /* for debugging */
}
