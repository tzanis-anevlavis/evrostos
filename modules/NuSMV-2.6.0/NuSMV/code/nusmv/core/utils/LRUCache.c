/* ---------------------------------------------------------------------------


  This file is part of the ``utils'' package of NuSMV version 2.
  Copyright (C) 2011 by FBK-irst.

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
  \author Alessandro Mariotti
  \brief Implementation of class 'LRUCache'

  \todo: Missing description

*/


#include "nusmv/core/utils/LRUCache.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/OAHash_private.h"
#include "nusmv/core/utils/object.h"
#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct LRUNode_TAG {
  struct LRUNode_TAG* prev;
  struct LRUNode_TAG* next;

  void* key; /* This is held just for freeing it */
  void* value;
} LRUNode;

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef enum {
  LRU_NODE_ACTION_NONE,
  LRU_NODE_ACTION_REMOVE,
  LRU_NODE_ACTION_CLEAR
} LRUNodeActionEnum;

typedef struct LRUCache_TAG
{
  INHERITS_FROM(OAHash);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */

  LRUNode* head;
  LRUNode* tail;

  size_t threshold;

  /* Those are used for the custom free_entry_fun */
  LRUNodeActionEnum _free_lru_node;
  LRU_CACHE_FREE_FUN _free_fun;
  void* _free_fun_argument;
} LRUCache;



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
#define LRUNULL ((LRUNode*)NULL)

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void lru_cache_init(LRUCache_ptr self, size_t threshold,
                           LRU_CACHE_EQ_FUN key_eq_fun,
                           LRU_CACHE_HASH_FUN key_hash_fun,
                           LRU_CACHE_FREE_FUN free_entry_fun,
                           void* free_entry_fun_arg);

static void lru_cache_deinit(LRUCache_ptr self);

static void lru_cache_free_entry_fun(void* k, void* v, void* arg);


/*!
  \brief Removes the given node from the double linked list

  Removes the given node from the double linked list
*/

static inline void lru_cache_remove(LRUCache_ptr self, LRUNode* node)
{
  if (LRUNULL == node->next) {
    nusmv_assert(self->head == node);
    self->head = node->prev;
  }
  else {
    node->next->prev = node->prev;
  }

  if (LRUNULL == node->prev) {
    nusmv_assert(self->tail == node);
    self->tail = node->next;
  }
  else {
    node->prev->next = node->next;
  }
}


/*!
  \brief Appends the given node to the tail of the
                      double linked list

  Appends the given node to the tail of the
                      double linked list
*/

static inline void lru_cache_append(LRUCache_ptr self, LRUNode* node)
{
  /* Update to defaults the prev and next */
  node->prev = LRUNULL;
  node->next = LRUNULL;

  if (LRUNULL == self->tail) {
    nusmv_assert(LRUNULL == self->head);
    self->head = node;
  }
  else {
    node->next = self->tail;
    self->tail->prev = node;
  }

  /* Put the node at the tail of the list */
  self->tail = node;
}


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

LRUCache_ptr LRUCache_create(size_t threshold,
                             LRU_CACHE_EQ_FUN key_eq_fun,
                             LRU_CACHE_HASH_FUN key_hash_fun,
                             LRU_CACHE_FREE_FUN free_entry_fun,
                             void* custom_arg)
{
  LRUCache_ptr self = ALLOC(LRUCache, 1);

  LRU_CACHE_CHECK_INSTANCE(self);
  nusmv_assert(NULL != key_hash_fun);
  nusmv_assert(NULL != key_eq_fun);

  lru_cache_init(self, threshold, key_eq_fun, key_hash_fun,
                 free_entry_fun, custom_arg);
  return self;
}

void LRUCache_destroy(LRUCache_ptr self)
{
  LRU_CACHE_CHECK_INSTANCE(self);

  lru_cache_deinit(self);
  FREE(self);
}

boolean LRUCache_insert(LRUCache_ptr self, const void* key, const void* value)
{
  /* Get and remove the possibly duplicate */
  LRUNode* curr_val = (LRUNode*)OAHash_lookup(OA_HASH(self), key);
  boolean replaced = false;
  boolean res;

  nusmv_assert(LRU_NODE_ACTION_NONE == self->_free_lru_node);

  if (LRUNULL != curr_val) {

    /* Cleanup of key and value is done by the OAHash. */
    res = OAHash_remove(OA_HASH(self), key);
    nusmv_assert(res);

    /* This node is going to be recycled */
    lru_cache_remove(self, curr_val);

    replaced = true;
  }
  else {
    if (OAHash_get_size(OA_HASH(self)) >= self->threshold) {
      /* Recycle the node we remove */
      curr_val = self->head;

      /* Cleanup of key and value is done by the OAHash. */
      res = OAHash_remove(OA_HASH(self), curr_val->key);
      nusmv_assert(res);

      /* Remove the head.. */
      lru_cache_remove(self, curr_val);
    }
    else {
      curr_val = ALLOC(LRUNode, 1);
    }
  }

  /* The key is expected NOT to exist */
  res = OAHash_insert(OA_HASH(self), key, curr_val);
  nusmv_assert(!res);

  lru_cache_append(self, curr_val);

  /* Update the value */
  curr_val->key = (void*)key;
  curr_val->value = (void*)value;

  return replaced;
}

void* LRUCache_lookup(LRUCache_ptr self, const void* key)
{
  LRUNode* curr_val = (LRUNode*)OAHash_lookup(OA_HASH(self), key);

  if (LRUNULL == curr_val) {
    return (void*)NULL;
  }

  /* Update the node position, moving it to the tail (lowest priority
     for removal). No need of value nor key freeing */
  lru_cache_remove(self, curr_val);
  lru_cache_append(self, curr_val);

  return curr_val->value;
}

boolean LRUCache_remove(LRUCache_ptr self, const void* key)
{
  boolean res;

  /* Tell the freeing function that we want to free the LRUNode
     associated with the given key */
  self->_free_lru_node = LRU_NODE_ACTION_REMOVE;
  res = OAHash_remove(OA_HASH(self), key);
  self->_free_lru_node = LRU_NODE_ACTION_NONE;

  return res;
}

void LRUCache_clear(LRUCache_ptr self)
{
  LRU_CACHE_CHECK_INSTANCE(self);

  /* Tell the freeing function that we want to free all LRUNodes in
     the hash.. */
  self->_free_lru_node = LRU_NODE_ACTION_CLEAR;
  OAHash_clear(OA_HASH(self));
  self->_free_lru_node = LRU_NODE_ACTION_NONE;

  self->tail = LRUNULL;
  self->head = LRUNULL;
}

void LRUCache_iter_values(const LRUCache_ptr self, const LRUCacheIter iter,
                          void** key, void** value)
{
  OAHash_iter_values(OA_HASH(self), (OAHashIter)iter, key, value);

  if ((void**)NULL != value) {
    *value = ((LRUNode*)(*value))->value;
  }
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The LRUCache class private initializer

  The LRUCache class private initializer

  \sa LRUCache_create
*/
static void lru_cache_init(LRUCache_ptr self, size_t threshold,
                           LRU_CACHE_EQ_FUN key_eq_fun,
                           LRU_CACHE_HASH_FUN key_hash_fun,
                           LRU_CACHE_FREE_FUN free_entry_fun,
                           void* custom_arg)
{
  /* members initialization */
  oa_hash_init(OA_HASH(self), key_eq_fun, key_hash_fun,
               lru_cache_free_entry_fun, custom_arg);

  self->threshold = threshold;
  self->head = LRUNULL;
  self->tail = LRUNULL;

  /* Those are for lru_cache_free_entry_fun */

  /* The argument passed by the OAHash to the free_entry_fun is
     overrided, and points to "self", which contains the needed
     structures to deal with LRUNodes as values */
  OA_HASH(self)->free_fun_arg = self;

  self->_free_fun_argument = custom_arg;
  self->_free_fun = free_entry_fun;
  self->_free_lru_node = LRU_NODE_ACTION_NONE;
}

/*!
  \brief The LRUCache class private deinitializer

  The LRUCache class private deinitializer

  \sa LRUCache_destroy
*/
static void lru_cache_deinit(LRUCache_ptr self)
{
  /* oa_hash_deinit will also free our LRUNodes */
  self->_free_lru_node = LRU_NODE_ACTION_CLEAR;
  oa_hash_deinit(OA_HASH(self));
}

/*!
  \brief This function wraps the free_entry_fun provided to the
                      OAHash

  This function wraps the free_entry_fun provided to the
                      OAHash. This is done since values in the hash
                      are not the ones inserted when calling
                      LRUCache_insert, but LRUNodes that are used for
                      the double linked list

  \sa LRUCache_destroy
*/
static void lru_cache_free_entry_fun(void* k, void* v, void* arg)
{
  LRUCache_ptr self = LRU_CACHE(arg);
  LRUNode* lrunode = (LRUNode*)v;

  if (NULL != self->_free_fun) {
    self->_free_fun(lrunode->key, lrunode->value,
                    self->_free_fun_argument);
  }

  switch (self->_free_lru_node) {
  case LRU_NODE_ACTION_NONE:
    /* Do nothing */
    break;

    /* If freeing because LRUCache_remove was used, we need to leave
       the LRUCache in a consistent state: remove the LRUNode before
       freeing it */
  case LRU_NODE_ACTION_REMOVE:
    lru_cache_remove(self, lrunode);
    /* No break since the lrunode needs to be freed */

  case LRU_NODE_ACTION_CLEAR:
    FREE(lrunode);
    break;

  default:
    error_unreachable_code();
  }
}

/**AutomaticEnd***************************************************************/

