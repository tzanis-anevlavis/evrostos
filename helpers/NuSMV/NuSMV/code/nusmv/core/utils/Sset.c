/* ---------------------------------------------------------------------------


  This file is part of the ``utils'' package of NuSMV version 2.
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
  \brief Implementation of Sset (Sorted Set) class

  Sset class implements a set (container) of element ordered
  according to their keys and having some data (void* values). Note that keys
  have to be unique, i.e. this class does not support multi-sets.

  WARNING: It is user responsibility to free the keys, if needed.

  The implementation of this classes is based on AVL Tree -- a balanced binary
  search tree kept balanced using the AVL algorithm invented by
  G.M. Adelson-Velsky and E.M. Landis. The description of the algorithm is
  taken from "The art of computer programming" vol 3 by Knuth. Some inspiration
  to implementation was taken from Goletas Library by Maksim Goleta.

  The main functionality is: * the insertion, search for and deletion of an
  element is performed in time O(log2 N) where N is the number of elements in
  the set.

*/

#include "nusmv/core/utils/Sset.h"

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/error.h" /* for rpterr */
#include "nusmv/core/utils/defs.h"
#include "nusmv/core/utils/ErrorMgr.h"

#include "nusmv/core/node/NodeMgr.h"

/* external libraries */
#include <stddef.h>



/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/
/*!
  \brief Define a range of possible balances of a tree node

  Note for developers: See the description of struct Ssnode_TAG
  and for more info
*/

enum SSET_BALANCE {
  SSET_BALANCED = 0, /* both children have the same height */
  SSET_R_BALANCED = 1, /* right child has height 1 greater than the left one */
  SSET_L_BALANCED = 3, /* left child has height 1 greater than the right one */
};

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief This macro returns the balance field of a tree node

  See the description of struct Ssnode_TAG for more info.
*/
#define SSET_GET_BALANCE(node)    (PTR_TO_INT((node)->parent) & 0x3)

/*!
  \brief This macro sets up the balance field of a tree node

  See the description of struct Ssnode_TAG for more info.
  NOTE: the integer number "balance" can be only one of SSET_BALANCE
*/
#define SSET_SET_BALANCE(node, balance) \
  { DEBUG(nusmv_assert((balance & ~0x3) == 0));                         \
    ((node)->parent =                                                    \
        PTR_FROM_INT(void*, (PTR_TO_INT((node)->parent) & ~0x3) | (balance))); \
  }

/*!
  \brief This macro increments balance

  If balance was SSET_L_BALANCED it becomes SSET_BALANCED,
  if it was SSET_BALANCED it becomes SSET_R_BALANCED, otherwise
  the behavior is undefined.
  See the description of struct Ssnode_TAG for more info.
*/
#define SSET_INC_BALANCE(node) \
  ((node)->parent =                                                     \
      PTR_FROM_INT(void*, (((PTR_TO_INT((node)->parent) & 0x3) + 1) & 0x3) \
                          | (PTR_TO_INT((node)->parent) & ~0x3))          \
  )

/*!
  \brief This macro decrement balance

  If balance was SSET_R_BALANCED it becomes SSET_BALANCED,
  if it was SSET_BALANCED it becomes SSET_L_BALANCED, otherwise
  the behavior is undefined.
  See the description of struct Ssnode_TAG for more info.
*/
#define SSET_DEC_BALANCE(node) \
  ((node)->parent =                                                     \
      PTR_FROM_INT(void*, (((PTR_TO_INT((node)->parent) & 0x3) - 1) & 0x3) \
                          | (PTR_TO_INT((node)->parent) & ~0x3))          \
  )

/*!
  \brief Returns a pointer to the parent of a give node

  See the description of struct Ssnode_TAG for more info.
*/
#define SSET_GET_PARENT(node) PTR_FROM_INT(void*, PTR_TO_INT((node)->parent) & ~0x3)

/*!
  \brief Sets a pointer to the parent of a give node

  See the description of struct Ssnode_TAG for more info.
*/
#define SSET_SET_PARENT(node, _parent) \
  { DEBUG(nusmv_assert((PTR_TO_INT(_parent) & 0x3) == 0));               \
    (node)->parent = PTR_FROM_INT(void*, (PTR_TO_INT((node)->parent) & 0x3) \
                                         | PTR_TO_INT(_parent));             \
  }

/*!
  \brief Sets the parent pointer and the balance of a given node

  See the description of struct Ssnode_TAG for more info.
  "balance" has to be one of SSET_BALANCE
*/
#define SSET_SET_PARENT_BALANCE(node, _parent, balance) \
  { DEBUG(nusmv_assert((PTR_TO_INT(_parent) & 0x3) == 0 && ((balance) & ~0x3) == 0)); \
    ((node)->parent = PTR_FROM_INT(void*, PTR_TO_INT(_parent) | (balance))) ; \
  }

/*!
  \brief Sets the parent pointer and the balance of a given node
  taking the values from another node

  See the description of struct Ssnode_TAG for more info.
  "balance" has to be one of SSET_BALANCE
*/
#define SSET_COPY_PARENT_BALANCE_TO(node, another) \
  ((node)->parent = (another)->parent)

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

struct Sset_TAG {
  Ssnode_ptr root; /* the root of the binary tree */
  size_t size; /* the number of elements in the tree */
  PFIVPVP compare; /* comparison function */
};

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef struct Sset_TAG Sset;

struct Ssnode_TAG {
  Sset_key key;  /* the key of a node */
  Ssnode_ptr left; /* the left child of the node, i.e. a set of smaller elements.
                      This pointer may be NULL */
  Ssnode_ptr right; /* the right child of the node, i.e. a set of greater elements.
                       This pointer may be NULL */
  Ssnode_ptr parent; /* the parent of the node. It can be NULL iff
                        the given node is the root of the tree */
  void* element; /* an element stored in this node */
};

typedef struct Ssnode_TAG Ssnode;


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/
/* Macro DEBUG is used for debugging the functions in this file */
#ifndef NDEBUG

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
static void s_set_init(Sset_ptr self, PFIVPVP compare);
static void s_set_deinit(Sset_ptr self);

static Ssnode_ptr s_set_copy(Ssnode_ptr node,
                             void* (*func)(void*, void*), void* arg);

static Ssnode_ptr s_set_new_node(Sset_key key,
                                 Ssnode_ptr parent);
static size_t s_set_free_nodes(Sset_ptr self, Ssnode_ptr node);

static Ssnode_ptr s_set_find(Sset_ptr self, Sset_key key);
static Ssnode_ptr s_set_find_closest_le(Sset_ptr self,
                                        Sset_key key);
static Ssnode_ptr s_set_find_closest_ge(Sset_ptr self,
                                        Sset_key key);
static Ssnode_ptr s_set_find_insert(Sset_ptr self,
                                    Sset_key key,
                                    boolean* is_found);

static void s_set_delete_node(Sset_ptr self, Ssnode_ptr node);

static inline void
s_set_assign_cmp_keys(const Sset_ptr self,
                      const Sset_key key,
                      const Sset_key p_key,
                      signed long long int* cmp_key,
                      signed long long int* cmp_parent_key,
                      const boolean is_there_compare);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

Sset_ptr Sset_create()
{
  return Sset_create_with_param((PFIVPVP)NULL);
}

Sset_ptr Sset_create_with_param(PFIVPVP compare)
{
  Sset_ptr self = ALLOC(Sset, 1);

  s_set_init(self, compare);

  return self;
}

void Sset_destroy (Sset_ptr self)
{
  s_set_deinit(self);
  FREE(self);
}

Sset_ptr Sset_copy (const Sset_ptr self)
{
  return Sset_copy_func(self, NULL, NULL);
}

Sset_ptr Sset_copy_func (const Sset_ptr self,
                         void* (*func)(void*, void*), void* arg)
{
  Sset_ptr new_set = Sset_create();

  new_set->size = self->size;
  new_set->root = self->root == NULL ? NULL : s_set_copy(self->root, func, arg);
  new_set->compare = self->compare;

  return new_set;
}

boolean Sset_insert(Sset_ptr self, Sset_key key, void* element)
{

  boolean is_found;
  Ssnode_ptr new_node = s_set_find_insert(self, key, &is_found);

  if (is_found) return false;

  new_node->element = element;
  return true;
}

Ssiter Sset_find(Sset_ptr self, Sset_key key)
{
  Ssiter iter = {s_set_find(self, key)};
  return iter;
}

Ssiter Sset_find_le(Sset_ptr self, Sset_key key)
{
  Ssiter iter = {s_set_find_closest_le(self, key)};
  return iter;
}

Ssiter Sset_find_ge(Sset_ptr self, Sset_key key)
{
  Ssiter iter = {s_set_find_closest_ge(self, key)};
  return iter;
}

Ssiter Sset_find_insert(Sset_ptr self, Sset_key key,
                        boolean* is_found)
{
  boolean _is_found;
  Ssiter iter = {s_set_find_insert(self, key, &_is_found)};
  if (NULL != is_found) *is_found = _is_found;
  return iter;
}

void* Sset_delete(Sset_ptr self, Sset_key key, boolean* is_found)
{
  Ssnode_ptr to_be_deleted = s_set_find(self, key); /* a node to be deleted */
  void* element;

  if (NULL == to_be_deleted) { /* no element with key "key" exists */
    if (NULL != is_found) *is_found = false;
    return NULL;
  }

  if (NULL != is_found) *is_found = true;
  element = to_be_deleted->element;
  s_set_delete_node(self, to_be_deleted);

  return element;

}

void Sset_delete_iter(Sset_ptr self, Ssiter iter)
{
  s_set_delete_node(self, iter.node);
  return ;
}

size_t Sset_get_size(Sset_ptr self)
{
  return self->size;
}

boolean Sset_is_empty(Sset_ptr self)
{
  return NULL == self->root;
}

Ssiter Sset_first(Sset_ptr self)
{
  Ssiter iter = {self->root };
  if (NULL != iter.node) {
    while (iter.node->left != NULL) iter.node = iter.node->left;
  }

  return iter;
}

Ssiter Sset_last(Sset_ptr self)
{
  Ssiter iter = {self->root};
  if (NULL != iter.node) {
    while (iter.node->right != NULL) iter.node = iter.node->right;
  }

  return iter;
}

Ssiter Ssiter_next(Ssiter iter)
{
  Ssnode_ptr p = iter.node;

  /* if there is a right child then the next element is
     its lowest left child */
  if (p->right != NULL) {
    p = p->right;
    while (p->left != NULL) p = p->left;
  }
  else {
    /* if there is no right child => find the first parent
       which we reach through a left child */
    Ssnode_ptr parent;
    while ((parent = SSET_GET_PARENT(p)) != NULL && p == parent->right) {
      p = parent;
    }
    p = parent;
  }

  iter.node = p;
  return iter;
}

Ssiter Ssiter_prev(Ssiter iter)
{
  Ssnode_ptr p = iter.node;

  /* if there is a left child then the previous element is
     its lowest right child */
  if (p->left != NULL) { /* return the left child if any */
    p = p->left;
    while (p->right != NULL) p = p->right;
  }
  else {
    /* if there is no left child => find the first parent
       which we reach through a right child */
    Ssnode_ptr parent;
    while ((parent = SSET_GET_PARENT(p)) != NULL && p == parent->left) {
      p = parent;
    }
    p = parent;
  }

  iter.node = p;
  return iter;
}

boolean Ssiter_is_valid(Ssiter iter)
{
  return NULL != iter.node;
}

void* Ssiter_element(Ssiter iter)
{
  nusmv_assert(iter.node != NULL); /* iterator is past the last element */
  return iter.node->element;
}

Sset_key Ssiter_key(Ssiter iter)
{
  nusmv_assert(iter.node != NULL); /* iterator is past the last element */
  return iter.node->key;
}

void Ssiter_set_element(Ssiter iter, void* element)
{
  nusmv_assert(iter.node != NULL); /* iterator is past the last element */
  iter.node->element = element;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/* used in Ssiter_test */
/* Since used for testing, do not introduce reentrancy problems */
DEBUG(static long time_to_free;)
DEBUG(static size_t num_of_free;)
/*!
  \brief The function tests the class implementation

  This function should be used only by developers to test the
  changed in the class.
*/

#ifndef NDEBUG
void Sset_test(const NuSMVEnv_ptr env)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  Sset_ptr set1, set2;
  Ssiter iter1, iter2;
  int i, n1, n2;
  long t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11;
  /* the number of created elements: */
  #define N 1000000 
  Sset_key buf[N];
  boolean b;

  if (DEBUG(0 &&) 1) {
    ErrorMgr_rpterr(errmgr,
                    "To test the class Sset macro DEBUG should be enabled"
                    " in file Sset.c \n");
  }

  set1 = Sset_create();   nusmv_assert(Sset_is_empty(set1));
  Sset_destroy(set1);

  set1 = Sset_create();   nusmv_assert(Sset_get_size(set1) == 0);
  set2 = Sset_copy(set1); nusmv_assert(Sset_get_size(set2) == 0);
  Sset_destroy(set1);     nusmv_assert(Sset_get_size(set2) == 0);
  Sset_destroy(set2);

  for (i = 0; i < N; ++i) buf[i] = PTR_FROM_INT(Sset_key, -1);

  /* find-insertion */
  t1 = util_cpu_time();
  set1 = Sset_create();
  for (i = 0, n1 = 0; i < N; ++i) {
    int r = utils_random(); if (r == -1) r = 0; /* -1 is a special value */
    iter1 = Sset_find_insert(set1, PTR_FROM_INT(Sset_key, r), &b);

    if (!b) {
      n1 += 1;
      Ssiter_set_element(iter1, PTR_FROM_INT(void*, i));
      buf[i] = PTR_FROM_INT(Sset_key, r);
    }
  }
  t1 = util_cpu_time() - t1;
  nusmv_assert(Sset_get_size(set1) == n1);

  StreamMgr_print_output(streams, "TEST of Sorted Set Class (Sset) is finished successfully.\n"
         "The following statistics has been collected (for a table with %d elements):\n"
         "\t time to create a set (with %lu repeated tries): %.3f\n",
         n1, N - n1, (float)t1 / 1000);

  /* forward walking + accessing */
  t2 = util_cpu_time();
  n2 = 0;
  for (iter1 = Sset_first(set1); Ssiter_is_valid(iter1) ; iter1 = Ssiter_next(iter1)) {
    n2 ++;
    nusmv_assert(buf[(size_t)Ssiter_element(iter1)] == Ssiter_key(iter1));
  }
  t2 = util_cpu_time() - t2;
  nusmv_assert(n1 == n2);

  StreamMgr_print_output(streams, "\t time of forward walk with access: %.3f\n", (float)t2 / 1000);

  /* backward walking + accessing */
  t3 = util_cpu_time();
  n2 = 0;
  for (iter1 = Sset_last(set1); Ssiter_is_valid(iter1) ; iter1 = Ssiter_prev(iter1)) {
    n2 ++;
    nusmv_assert(buf[(size_t)Ssiter_element(iter1)] == Ssiter_key(iter1));
  }
  t3 = util_cpu_time() - t3;
  nusmv_assert(n1 == n2);

  StreamMgr_print_output(streams, "\t time of backward walk with access: %.3f\n",(float)t3 / 1000);

  /* finding */
  t4 = util_cpu_time();
  for (i = 0; i < N; ++i) {
    iter1 = Sset_find(set1, buf[i]);
    nusmv_assert((buf[i] == PTR_FROM_INT(Sset_key, -1)) == !Ssiter_is_valid(iter1));
    nusmv_assert(!Ssiter_is_valid(iter1) || (size_t)Ssiter_element(iter1) == i);
  }
  t4 = util_cpu_time() - t4;

  StreamMgr_print_output(streams, "\t time of searching for all elements (with %lu failed tries): %.3f\n",
         N - n1, (float)t4 / 1000);

  /* copying */
  t5 = util_cpu_time();
  set2 = Sset_copy(set1);
  t5 = util_cpu_time() - t5;

  StreamMgr_print_output(streams, "\t time of copying: %.3f\n", (float)t5 / 1000);

  /* destroying*/
  t6 = util_cpu_time();
  time_to_free = 0;
  num_of_free = 0;
  Sset_destroy(set1);
  t6 = util_cpu_time() - t6;

  StreamMgr_print_output(streams, "\t time of destroying the set of %" PRIuPTR \
         " elements: %.3f (freeing took %.3f)\n",
         num_of_free, (float)t6 / 1000, (float)time_to_free / 1000);

  /* removing elements randomly */
  t7 = util_cpu_time();
  time_to_free = 0;
  num_of_free = 0;
  for (i = 0; i < N; ++i) {
    size_t x = (size_t)Sset_delete(set2, buf[i], &b);
    nusmv_assert((b && i == x) || (!b && buf[i] == PTR_FROM_INT(Sset_key, -1) && x == 0));
  }
  t7 = util_cpu_time() - t7;
  nusmv_assert(Sset_is_empty(set2));
  Sset_destroy(set2);

  StreamMgr_print_output(streams, "\t time of deleting %" PRIuPTR \
         " elements randomly (with %" PRIuPTR " failed tries) : " \
         "%.3f(freeing took %.3f)\n",
         num_of_free, N - n1, (float)t7 / 1000, (float)time_to_free / 1000);

  /* inserting elements in direct order */
  t8 = util_cpu_time();
  set1 = Sset_create();
  for (i = 0; i < n1; ++i) {
    b = Sset_insert(set1, PTR_FROM_INT(Sset_key, i), PTR_FROM_INT(void*, i));
    nusmv_assert(b);
  }
  t8 = util_cpu_time() - t8;

  StreamMgr_print_output(streams, "\t time of inserting element in direct order: %.3f\n", (float)t8 / 1000);

  /* removing elements in direct order */
  t9 = util_cpu_time();
  time_to_free = 0;
  num_of_free = 0;
  iter1 = Sset_first(set1);
  i = 0;
  while (Ssiter_is_valid(iter1)) {
    iter2 = Ssiter_next(iter1);
    /* nusmv_assert(PTR_TO_INT(iter1) == i && (size_t)Ssiter_element(iter1) == i); */
    Sset_delete_iter(set1, iter1);
    iter1 = iter2;
    ++i;
  }
  t9 = util_cpu_time() - t9;
  nusmv_assert(i == n1);
  nusmv_assert(Sset_is_empty(set1));
  Sset_destroy(set1);

  StreamMgr_print_output(streams, "\t time of removing %" PRIuPTR \
         " elements in direct order: %.3f (freeing took %.3f)\n",
         num_of_free, (float)t9 / 1000, (float)time_to_free / 1000);

  /* inserting elements in opposite order */
  t10 = util_cpu_time();
  set1 = Sset_create();
  for (i = n1-1; i >=0; --i) {
    b = Sset_insert(set1, PTR_FROM_INT(Sset_key, i), PTR_FROM_INT(void*, i));
    nusmv_assert(b);
  }
  t10 = util_cpu_time() - t10;

  StreamMgr_print_output(streams, "\t time of inserting elements in opposite order: %.3f\n", (float)t10/ 1000);

  /* removing elements in opposite order */
  t11 = util_cpu_time();
  time_to_free = 0;
  num_of_free = 0;
  iter1 = Sset_last(set1);
  i = n1 - 1;
  while (Ssiter_is_valid(iter1)) {
    iter2 = Ssiter_prev(iter1);
    /* nusmv_assert(PTR_TO_INT(iter1) == i && (size_t)Ssiter_element(iter1) == i); */
    Sset_delete_iter(set1, iter1);
    iter1 = iter2;
    --i;
  }
  t11 = util_cpu_time() - t11;
  nusmv_assert(i == -1);
  nusmv_assert(Sset_is_empty(set1));
  Sset_destroy(set1);

  StreamMgr_print_output(streams, "\t time of removing %" PRIuPTR \
         " elements in opposite order: %.3f (freeing took %.3f)\n",
         num_of_free, (float)t11 / 1000, (float)time_to_free / 1000);

  StreamMgr_print_output(streams, "\t total time: %.3f\n",
         (float)(t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11)/1000);

  return;
}
#endif

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Initializes the memory for a set instance


*/
static void s_set_init(Sset_ptr self, PFIVPVP compare)
{
  SSET_CHECK_INSTANCE(self);

  self->root = NULL;
  self->size = 0;
  self->compare = compare;
}

/*!
  \brief Deinitializes the memory from a set


*/
static void s_set_deinit(Sset_ptr self)
{
  SSET_CHECK_INSTANCE(self);

  if (NULL != self->root) {
    DEBUG(size_t i = ) s_set_free_nodes(self, self->root);
    DEBUG(nusmv_assert(i == self->size));
  }
  else {
    nusmv_assert(0 == self->size);
  }
  self->root = NULL; /* for debugging */
  self->size = -1; /* for debugging */
  self->compare = NULL; /* for debugging */
}

/*!
  \brief Creates a copy of a given tree

  Precondition: the tree should no be empty.
  Warning: the parent of returned node equals the parent of the input node.

  Optional func parameter is a function takinf voi* and returning void*, for
  copying the elements
*/
static Ssnode_ptr s_set_copy(Ssnode_ptr node, void* (*func)(void*, void*), void* arg)
{
  Ssnode_ptr left  = node->left != NULL ? s_set_copy(node->left, func, arg) : NULL;
  Ssnode_ptr new_node = ALLOC(Ssnode, 1);
  Ssnode_ptr right = node->right != NULL ? s_set_copy(node->right, func, arg) : NULL;

  new_node->key = node->key;
  new_node->left = left;
  new_node->right = right;
  /* note: the parent is set of another tree but it will be reset
     by previously invoked instance of this function */
  SSET_COPY_PARENT_BALANCE_TO(new_node, node);

  new_node->element = (NULL == func) ? node->element : func(node->element, arg);

  if (left != NULL)  SSET_SET_PARENT(left, new_node);
  if (right != NULL) SSET_SET_PARENT(right, new_node);

  return new_node;
}

/*!
  \brief Function allocates a new Ssnode and sets its
  fields to the provided values.

  "left", "right", "element" and "balance" is set to 0.
*/
static Ssnode_ptr s_set_new_node(Sset_key key, Ssnode_ptr parent)
{
  Ssnode_ptr p = ALLOC(Ssnode, 1);
  p->key = key;
  p->left = NULL;
  p->right = NULL;
  SSET_SET_PARENT_BALANCE(p, parent, SSET_BALANCED);
  p->element = NULL;

  return p;
}

/*!
  \brief Function de-allocates a Ssnode and all its children

  The elements themselves are not freed.  The returned
  value is the number of freed elements (returned only if DEBUG is
  enabled).
*/
static size_t s_set_free_nodes(Sset_ptr self, Ssnode_ptr node)
{
  DEBUG(signed long long int cmp_key);
  DEBUG(signed long long int cmp_p_key);
  DEBUG(boolean is_there_compare = (NULL != self->compare));

  DEBUG(size_t i = 0);
  if (NULL != node->left) {
    DEBUG(s_set_assign_cmp_keys(self, node->left->key, node->key, &cmp_key,
                                &cmp_p_key, is_there_compare));
    DEBUG(nusmv_assert(cmp_key < cmp_p_key));
    DEBUG(i +=) s_set_free_nodes(self, node->left);
  }
  if (NULL != node->right) {
    DEBUG(s_set_assign_cmp_keys(self, node->right->key, node->key, &cmp_key,
                                &cmp_p_key, is_there_compare));
    DEBUG(nusmv_assert(cmp_key > cmp_p_key));
    DEBUG(i +=) s_set_free_nodes(self, node->right);
  }

  DEBUG(time_to_free -= util_cpu_time());

  FREE(node);

  DEBUG(time_to_free += util_cpu_time(); num_of_free += 1;);

  return DEBUG(i + ) 1;
}

/*!
  \brief Looks for an element with a given key

  The found element is returned. If no such element
  exists NULL is returned
*/
static Ssnode_ptr s_set_find(Sset_ptr self, Sset_key key)
{
  Ssnode_ptr p = self->root;
  boolean is_there_compare = (NULL != self->compare);

  while (p != NULL) {
    signed long long int cmp_key;
    signed long long int cmp_p_key;

    s_set_assign_cmp_keys(self, key, p->key, &cmp_key, &cmp_p_key,
                          is_there_compare);

    if (cmp_key < cmp_p_key) p = p->left;
    else if (cmp_key > cmp_p_key) p = p->right;
    else return p;
  }

  return NULL;
}

/*!
  \brief Looks for an element closest (less or equal than) a given key


*/
static Ssnode_ptr s_set_find_closest_le(Sset_ptr self, Sset_key key)
{
  Ssnode_ptr p = self->root;
  Ssnode_ptr best = (Ssnode_ptr) NULL;
  boolean is_there_compare = (NULL != self->compare);

  while (p != NULL) {
    signed long long int cmp_key;
    signed long long int cmp_p_key;

    s_set_assign_cmp_keys(self, key, p->key, &cmp_key, &cmp_p_key,
                          is_there_compare);

    if (cmp_key < cmp_p_key) p = p->left;
    else if (cmp_key > cmp_p_key)  {
      best = p;
      p = p->right; /* see if can find some better value */
    }
    else return p; /* found it! */
  }

  return best;
}

/*!
  \brief Looks for an element closest (greater or equal than) a given key


*/
static Ssnode_ptr s_set_find_closest_ge(Sset_ptr self, Sset_key key)
{
  Ssnode_ptr p = self->root;
  Ssnode_ptr best = (Ssnode_ptr) NULL;
  boolean is_there_compare = (NULL != self->compare);

  while (p != NULL) {
    signed long long int cmp_key;
    signed long long int cmp_p_key;

    s_set_assign_cmp_keys(self, key, p->key, &cmp_key, &cmp_p_key,
                          is_there_compare);

    if (cmp_key < cmp_p_key) {
      best = p;
      p = p->left; /* see if can find some better value */
    }
    else if (cmp_key > cmp_p_key) p = p->right;
    else return p; /* found it! */
  }

  return best;
}

/*!
  \brief Looks for an element with a given key. If such element
  does not exists it is created.


  is_found is set to true if the element exists in the tree and false otherwise.
  Precondition: is_found should not be NULL;
  Note: all the existing iterators remain valid.
  Returns a node with the given key.

  SideEffects []

  SeeAlso     []

*****************************************************************************[EXTRACT_DOC_NOTE: * /]

*/
static Ssnode_ptr s_set_find_insert(Sset_ptr self, Sset_key key,
                                    boolean* is_found)
{
  Ssnode_ptr p = self->root;
  Ssnode_ptr new_node;
  boolean is_there_compare = (NULL != self->compare);
  Ssnode_ptr parent;

  if (p == NULL) { /* special case: empty tree */
    p = s_set_new_node(key, NULL);

    self->root = p;
    self->size = 1;
    *is_found = false;
    return p;
  }

  while (true) {
    signed long long int cmp_key;
    signed long long int cmp_p_key;

    s_set_assign_cmp_keys(self, key, p->key, &cmp_key, &cmp_p_key,
                          is_there_compare);

    if (cmp_key < cmp_p_key) {
      if (p->left != NULL) p = p->left;
      else {
        new_node = s_set_new_node(key, p);
        p->left = new_node;
        SSET_DEC_BALANCE(p);
        break;
      }
    }
    else if (cmp_key > cmp_p_key) {
      if (p->right != NULL) p = p->right;
      else {
        new_node = s_set_new_node(key, p);
        p->right = new_node;
        SSET_INC_BALANCE(p);
        break;
      }
    }
    else { /* the key is found */
      *is_found = true;
      return p;
    }
  } /* while */

  /* such key has not been found and new element is created.
     Now it is time to balance the tree.
  */

  self->size += 1;

  parent = SSET_GET_PARENT(p);

  while (SSET_GET_BALANCE(p) != SSET_BALANCED && parent != NULL) {

    if (parent->left == p) {

      p = parent;
      parent = SSET_GET_PARENT(p);

      if (SSET_GET_BALANCE(p) != SSET_L_BALANCED) SSET_DEC_BALANCE(p);
      else {
        Ssnode_ptr b = p->left;

        if (SSET_GET_BALANCE(b) == SSET_L_BALANCED) { /* left reflection of case 1
                                                        in Knuth's book */
          SSET_SET_PARENT(b, parent);

          if (parent == NULL) self->root = b;
          else {
            if (parent->left == p) parent->left = b;
            else parent->right = b;
          }

          p->left = b->right;

          if (p->left != NULL) SSET_SET_PARENT(p->left, p);

          b->right = p;
          SSET_SET_PARENT(p, b);

          SSET_SET_BALANCE(b, SSET_BALANCED);
          SSET_SET_BALANCE(p, SSET_BALANCED);
        }
        else { /* left reflection of case 2 in Knuth's book */
          Ssnode_ptr x = b->right;

          SSET_SET_PARENT(x, parent);

          if (parent == NULL) self->root = x;
          else {
            if (parent->left == p) parent->left = x;
            else parent->right = x;
          }

          b->right = x->left;

          if (b->right != NULL) SSET_SET_PARENT(b->right, b);

          p->left = x->right;

          if (p->left != NULL) SSET_SET_PARENT(p->left, p);


          x->left = b;
          x->right = p;

          SSET_SET_PARENT(b, x);
          SSET_SET_PARENT(p, x);

          if (SSET_GET_BALANCE(x) == SSET_L_BALANCED) {
            SSET_SET_BALANCE(b, SSET_BALANCED);
            SSET_SET_BALANCE(p, SSET_R_BALANCED);
          }
          else if (SSET_GET_BALANCE(x) == SSET_BALANCED) {
            SSET_SET_BALANCE(b, SSET_BALANCED);
            SSET_SET_BALANCE(p, SSET_BALANCED);
          }
          else { /* SSET_GET_BALANCE(x) == SSET_R_BALANCED */
            SSET_SET_BALANCE(b, SSET_L_BALANCED);
            SSET_SET_BALANCE(p, SSET_BALANCED);
          }

          SSET_SET_BALANCE(x, SSET_BALANCED);
        }

        break;
      } /* else */
    } /* end of if (parent->left == p) */
    else { /* parent->right == p */

      p = parent;
      parent = SSET_GET_PARENT(p);

      if (SSET_GET_BALANCE(p) != SSET_R_BALANCED) SSET_INC_BALANCE(p);
      else {
        Ssnode_ptr b = p->right;

        if (SSET_GET_BALANCE(b) == SSET_R_BALANCED) { /* case 1 in Knuth's book */
          SSET_SET_PARENT(b, parent);

          if (parent == NULL) self->root = b;
          else {
            if (parent->left == p) parent->left = b;
            else parent->right = b;
          }

          p->right = b->left;
          if (p->right != NULL) SSET_SET_PARENT(p->right, p);


          b->left = p;
          SSET_SET_PARENT(p, b);

          SSET_SET_BALANCE(b, SSET_BALANCED);
          SSET_SET_BALANCE(p, SSET_BALANCED);
        }
        else { /* case 2 in Knuth's book */
          Ssnode_ptr x = b->left;

          SSET_SET_PARENT(x, parent);

          if (parent == NULL) self->root = x;
          else {
            if (parent->left == p) parent->left = x;
            else parent->right = x;
          }

          b->left = x->right;
          if (b->left != NULL) SSET_SET_PARENT(b->left, b);

          p->right = x->left;
          if (p->right != NULL) SSET_SET_PARENT(p->right, p);

          x->right = b;
          x->left = p;

          SSET_SET_PARENT(b, x);
          SSET_SET_PARENT(p, x);

          if (SSET_GET_BALANCE(x) == SSET_R_BALANCED) {
            SSET_SET_BALANCE(b, SSET_BALANCED);
            SSET_SET_BALANCE(p, SSET_L_BALANCED);
          }
          else if (SSET_GET_BALANCE(x) == SSET_BALANCED) {
            SSET_SET_BALANCE(b, SSET_BALANCED);
            SSET_SET_BALANCE(p, SSET_BALANCED);
          }
          else { /* SSET_GET_BALANCE(x) == SSET_L_BALANCED */
            SSET_SET_BALANCE(b, SSET_R_BALANCED);
            SSET_SET_BALANCE(p, SSET_BALANCED);
          }

          SSET_SET_BALANCE(x, SSET_BALANCED);
        } /* else */

        break;
      }
    }
  } /* while */

  *is_found = false;
  return new_node;
}

/*!
  \brief Removes a given node from the set.


  Precondition: the node has to belong to the provided set.
  The operation takes O(log2 N) time (N is the size of the set).
*/
static void s_set_delete_node(Sset_ptr self, Ssnode_ptr to_be_deleted)
{
  boolean leftDecrease; /* a flag of which child trees is decreased, left or right */
  Ssnode_ptr to_be_balanced; /* a node which requires balancing */

  Ssnode_ptr parent = SSET_GET_PARENT(to_be_deleted);

  /* --- remove the node from the tree and reform the adjusting sub-trees --- */

  /* -- Case 1 : node to_be_deleted does not have a right child */
  if (to_be_deleted->right == NULL) {
    if (to_be_deleted->left != NULL) SSET_SET_PARENT(to_be_deleted->left, parent);

    if (parent == NULL) {
      self->root = to_be_deleted->left;
      to_be_balanced = NULL; /* no balance required */
      leftDecrease = false; /* this is to avoid warning */
    }
    else {
      if (to_be_deleted == parent->left) {
        parent->left = to_be_deleted->left;
        leftDecrease = true;
      }
      else {
        parent->right = to_be_deleted->left;
        leftDecrease = false;
      }
      to_be_balanced = parent;
    }
  }
  /* -- Case 2 : the right child of to_be_deleted does no have a left child */
  else if (to_be_deleted->right->left == NULL) {
    if (to_be_deleted->left != NULL) {
      SSET_SET_PARENT(to_be_deleted->left, to_be_deleted->right);
      to_be_deleted->right->left = to_be_deleted->left;
    }

    SSET_COPY_PARENT_BALANCE_TO(to_be_deleted->right, to_be_deleted);

    if (parent == NULL) self->root = to_be_deleted->right;
    else {
      if (to_be_deleted == parent->left) parent->left = to_be_deleted->right;
      else parent->right = to_be_deleted->right;
    }

    to_be_balanced = to_be_deleted->right;
    leftDecrease = false;
  }
  /* -- Case 3: the right child of to_be_deleted has a left child */
  else  {
    /* find a node which is just greater than the removed one. */
    Ssnode_ptr s = to_be_deleted->right->left;
    Ssnode_ptr s_parent;

    while (s->left != NULL)
      s = s->left;

    s_parent = SSET_GET_PARENT(s);

    /* detach this greater node from the tree */
    s_parent->left = s->right;

    if (s->right != NULL) {
      SSET_SET_PARENT(s->right, s_parent);
    }

    /* move this greater node to the place of the removed one */
    if (to_be_deleted->left != NULL) {
      SSET_SET_PARENT(to_be_deleted->left, s);
      s->left = to_be_deleted->left;
    }

    SSET_SET_PARENT(to_be_deleted->right, s);
    s->right = to_be_deleted->right;


    SSET_COPY_PARENT_BALANCE_TO(s, to_be_deleted);

    if (parent == NULL) self->root = s;
    else {
      if (to_be_deleted == parent->left) parent->left = s;
      else parent->right = s;
    }

    to_be_balanced = s_parent;
    leftDecrease = true;
  }


  /* --- re-balance the tree if it is required --- */
  while (NULL != to_be_balanced) {

    parent = SSET_GET_PARENT(to_be_balanced);

    /* -- the height of the left child has been decreased */
    if (leftDecrease) {

      if (SSET_GET_BALANCE(to_be_balanced) == SSET_BALANCED) {
        SSET_SET_BALANCE(to_be_balanced, SSET_R_BALANCED);
        to_be_balanced = NULL; /* end of balancing */
      }
      else if (SSET_GET_BALANCE(to_be_balanced) == SSET_L_BALANCED) {
        SSET_SET_BALANCE(to_be_balanced, SSET_BALANCED);
        /* continue balancing up */
      }
      else { /* re-balance is required since sub-tree was SSET_R_BALANCED (i.e. 1)
                and now the difference in heights becomes 2. */
        Ssnode_ptr x = to_be_balanced->right;

        /* case 1 : the balance of the node and its right child is opposite */
        if (SSET_GET_BALANCE(x) == SSET_L_BALANCED) {
          Ssnode_ptr w = x->left;

          SSET_SET_PARENT(w, parent);
          if (parent == NULL) self->root = w;
          else {
            if (parent->left == to_be_balanced) parent->left = w;
            else parent->right = w;
          }

          x->left = w->right;
          if (x->left != NULL) SSET_SET_PARENT(x->left, x);

          to_be_balanced->right = w->left;
          if (to_be_balanced->right != NULL) {
            SSET_SET_PARENT(to_be_balanced->right, to_be_balanced);
          }

          w->right = x;
          SSET_SET_PARENT(x, w);

          w->left = to_be_balanced;
          SSET_SET_PARENT(to_be_balanced, w);

          if (SSET_GET_BALANCE(w) == SSET_R_BALANCED) {
            SSET_SET_BALANCE(x, SSET_BALANCED);
            SSET_SET_BALANCE(to_be_balanced, SSET_L_BALANCED);
          }
          else if (SSET_GET_BALANCE(w) == SSET_BALANCED) {
            SSET_SET_BALANCE(x, SSET_BALANCED);
            SSET_SET_BALANCE(to_be_balanced, SSET_BALANCED);
          }
          else { /* SSET_GET_BALANCE(w) == SSET_L_BALANCED */
            SSET_SET_BALANCE(x, SSET_R_BALANCED);
            SSET_SET_BALANCE(to_be_balanced, SSET_BALANCED);
          }

          SSET_SET_BALANCE(w, SSET_BALANCED);

          to_be_balanced = w; /* continue balancing up */
        }
        else {
          /* case 2 : the balance of the node and its right child is NOT opposing */

          SSET_SET_PARENT(x, parent);
          if (parent != NULL) {
            if (parent->left == to_be_balanced) parent->left = x;
            else parent->right = x;
          }
          else self->root = x;

          to_be_balanced->right = x->left;
          if (to_be_balanced->right != NULL) {
            SSET_SET_PARENT(to_be_balanced->right, to_be_balanced);
          }

          x->left = to_be_balanced;
          SSET_SET_PARENT(to_be_balanced, x);

          if (SSET_GET_BALANCE(x) == SSET_BALANCED) {
            SSET_SET_BALANCE(x, SSET_L_BALANCED);
            SSET_SET_BALANCE(to_be_balanced, SSET_R_BALANCED);

            to_be_balanced = NULL; /* end the balancing */
          }
          else { /* balance of x is R-balance (i.e. 1) */
            SSET_SET_BALANCE(x, SSET_BALANCED);
            SSET_SET_BALANCE(to_be_balanced, SSET_BALANCED);

            to_be_balanced = x; /* continue balancing up */
          }
        }
      }
    } /* end of if (leftDecrease) */

    /* -- the height of the right child has been decreased */
    else { /* leftDecrease == 0 */

      if (SSET_GET_BALANCE(to_be_balanced) == SSET_BALANCED) {
        SSET_SET_BALANCE(to_be_balanced, SSET_L_BALANCED);
        to_be_balanced = NULL; /* end of balancing */
      }
      else if (SSET_GET_BALANCE(to_be_balanced) == SSET_R_BALANCED) {
        SSET_SET_BALANCE(to_be_balanced, SSET_BALANCED);
        /* continue balancing up */
      }
      else { /* re-balance is required since sub-tree was SSET_L_BALANCED (i.e. -1)
                and now the difference in heights becomes 2. */
        Ssnode_ptr x = to_be_balanced->left;

        /* case 1 : the balance of the node and its left child is opposite */
        if (SSET_GET_BALANCE(x) == SSET_R_BALANCED) {
          Ssnode_ptr w = x->right;

          SSET_SET_PARENT(w, parent);
          if (parent == NULL) self->root = w;
          else {
            if (parent->left == to_be_balanced) parent->left = w;
            else parent->right = w;
          }

          x->right = w->left;
          if (x->right != NULL) SSET_SET_PARENT(x->right, x);

          to_be_balanced->left = w->right;
          if (to_be_balanced->left != NULL) {
            SSET_SET_PARENT(to_be_balanced->left, to_be_balanced);
          }

          w->left = x;
          SSET_SET_PARENT(x, w);

          w->right = to_be_balanced;
          SSET_SET_PARENT(to_be_balanced, w);

          if (SSET_GET_BALANCE(w) == SSET_L_BALANCED) {
            SSET_SET_BALANCE(x, SSET_BALANCED);
            SSET_SET_BALANCE(to_be_balanced, SSET_R_BALANCED);
          }
          else if (SSET_GET_BALANCE(w) == SSET_BALANCED) {
            SSET_SET_BALANCE(x, SSET_BALANCED);
            SSET_SET_BALANCE(to_be_balanced, SSET_BALANCED);
          }
          else {/* SSET_GET_BALANCE(w) == SSET_R_BALANCED */
            SSET_SET_BALANCE(x, SSET_L_BALANCED);
            SSET_SET_BALANCE(to_be_balanced, SSET_BALANCED);
          }

          SSET_SET_BALANCE(w, SSET_BALANCED);

          to_be_balanced = w; /* continue balancing up */
        }
        else {
          /* case 2 : the balance of the node and its right child is NOT opposing */
          SSET_SET_PARENT(x, parent);
          if (parent != NULL) {
            if (parent->left == to_be_balanced) parent->left = x;
            else parent->right = x;
          }
          else self->root = x;

          to_be_balanced->left = x->right;
          if (to_be_balanced->left != NULL) {
            SSET_SET_PARENT(to_be_balanced->left, to_be_balanced);
          }

          x->right = to_be_balanced;
          SSET_SET_PARENT(to_be_balanced, x);

          if (SSET_GET_BALANCE(x) == SSET_BALANCED) {
            SSET_SET_BALANCE(x, SSET_R_BALANCED);
            SSET_SET_BALANCE(to_be_balanced, SSET_L_BALANCED);

            to_be_balanced = NULL; /* end the balancing */
          }
          else { /* balance of x is L-balance (i.e. -1) */
            SSET_SET_BALANCE(x, SSET_BALANCED);
            SSET_SET_BALANCE(to_be_balanced, SSET_BALANCED);

            to_be_balanced = x; /* continue balancing up */
          }
        }
      }
    }

    /* -- if balancing is still required go to the parent */
    if (to_be_balanced != NULL) {
      if (parent == NULL) to_be_balanced = NULL;  /* end of balancing */
      else {
        leftDecrease = (to_be_balanced == parent->left);
        to_be_balanced = parent;
      }
    }
  } /* while */


  /* --- free the removed node and update the size of the tree --- */
  DEBUG(time_to_free -= util_cpu_time());

  FREE(to_be_deleted);

  DEBUG(time_to_free += util_cpu_time(); num_of_free += 1;);

  self->size -= 1;
  DEBUG(nusmv_assert(self->size != (size_t)(-1)));

  return ;
}

/*!
  \brief Handles the two cases of key comparison

  If is_there_compare is false, keys should be integer,
  otherwise compare is called.
*/
static inline void s_set_assign_cmp_keys(const Sset_ptr self,
                                         const Sset_key key,
                                         const Sset_key p_key,
                                         signed long long int* cmp_key,
                                         signed long long int* cmp_p_key,
                                         const boolean is_there_compare)
{
  if (is_there_compare) {
    *cmp_key = (signed long long int)self->compare(key, p_key);
    *cmp_p_key = 0;
  }
  else {
    *cmp_key = PTR_TO_INT(key);
    *cmp_p_key = PTR_TO_INT(p_key);
  }
}
