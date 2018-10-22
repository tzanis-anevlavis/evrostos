
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
  \brief Implementation of class 'OAHash'

  \todo: Missing description

*/


#include "nusmv/core/utils/OAHash.h"
#include "nusmv/core/utils/OAHash_private.h"
#include "nusmv/core/utils/utils.h"
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

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/* We just need it's pointer. */
static const size_t dummy = (~0ULL);

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/



/*!
  \brief Finds the next first entry from "index" in the hash

  Finds the next first entry from "index" in the hash
*/

static inline OAHashIter oa_hash_iter_validate(const OAHash_ptr self,
                                               OAHashIter index)
{
  OAEntry* table = self->table;
  OAEntry* ep;

  if (index > self->mask) {
    return index;
  }

  ep = &(table[index]);

  while (((void*)NULL == ep->key) || (ep->key == (void*)dummy)) {
    ++index;

    if (index > self->mask) {
      /* Reached the end of the array */
      return index;
    }

    ep = &(table[index]);
  }

  return index;
}


/*!
  \brief Finds the location of the given key with the given hash

  Finds the location of the given key with the given hash
*/

static inline OAEntry* oa_hash_lookup(OAHash_ptr self,
                                      const void* key,
                                      const size_t hash)
{
  OAEntry* ep = (OAEntry*)NULL;
  OAEntry* result = (OAEntry*)NULL;

  size_t i = hash & self->mask;
  size_t perturb;

  ep = &(self->table[i]);

  /* If the slot is not set or the key matches */
  if (((void*)NULL == ep->key) || (ep->key == key)) {
    /* Thats it, we can reuse this entry if available or create a new
       one in this position */
    return ep;
  }

  if (ep->key == (void*)dummy) {
    result = ep;
  }
  else {
    if ((ep->hash == hash) &&
        (self->key_eq_fun(ep->key, key, self->custom_arg))) {
      return ep;
    }

    result = (OAEntry*)NULL;
  }

  /* In the loop, key == &dummy is by far (factor of 100s) the
     least likely outcome, so test for that last. */
  for (perturb = hash; ; perturb >>= PERTURB_SHIFT) {
    i = (i << 2) + i + perturb + 1;
    ep = &(self->table[i & self->mask]);

    if ((void*)NULL == ep->key)
      return (OAEntry*)NULL == result ? ep : result;

    if ((ep->key == key) ||
        ((ep->hash == hash) && (ep->key != (void*)dummy)
         && (self->key_eq_fun(ep->key, key, self->custom_arg)))) {
      return ep;
    }

    if (ep->key == (void*)dummy && (OAEntry*)NULL == result) {
      result = ep;
    }
  }

  error_unreachable_code();
  return (OAEntry*)NULL;
}


/*!
  \brief Destroys the given key and value if necessary

  Destroys the given key and value using the
                      free_entry_fun function, if provided @ build time
*/

static inline void oa_hash_clean_entry(const OAHash_ptr self, void* key, void* value)
{
  if (NULL != self->free_entry_fun) {
    self->free_entry_fun(key, value, self->free_fun_arg);
  }
}


/*!
  \brief Inserts a new entry with the given key - hash - value triple

  Inserts a new entry with the given key - hash - value triple
*/

static inline boolean
oa_hash_insert(OAHash_ptr self, const void* key,
               const size_t hash, const void* value)
{
  boolean exists = false;
  OAEntry* ep;

  if (key == (void*)dummy) {
    error_unreachable_code_msg("Inserting key that conflicts with dummy value");
  }

  ep = oa_hash_lookup(self, key, hash);

  if (ep == NULL) {
    error_unreachable_code();
  }

  /* We are replacing a key -> value entry*/
  if ((ep->key != (void*)NULL) && (ep->key != (void*)dummy)) {
    void* old_value = ep->value;
    void* old_key = ep->key;

    ep->hash = hash;
    ep->value = (void*)value;
    ep->key = (void*)key;

    oa_hash_clean_entry(self, old_key, old_value);
    exists = true;
  }
  /* We have a new entry */
  else {
    if (ep->key == NULL) {
      self->fill++;
    }
    else {
      assert(ep->key == (void*)dummy);
    }

    ep->key = (void*)key;
    ep->hash = hash;
    ep->value = (void*)value;
    self->used++;
  }

  return exists;
}

/*!
  \brief Aux routine for op_hash_resize

  Aux routine for op_hash_resize. Updates the values of
                      an entry in the table with the given values
*/

static inline void
oa_hash_resize_clean(OAHash_ptr self, const void* key,
                     const size_t hash, const void* value)
{
  size_t i;
  size_t perturb;
  size_t mask = self->mask;
  OAEntry* ep0 = self->table;
  OAEntry* ep;

  i = hash & mask;

  ep = &ep0[i];

  for (perturb = hash; (void*)NULL != ep->key; perturb >>= PERTURB_SHIFT) {
    i = (i << 2) + i + perturb + 1;
    ep = &ep0[i & mask];
  }

  /* "self" uses already the new array: we can assert that all entries
     are clean */
  assert((void*)NULL == ep->value);

  self->fill++;
  ep->key = (void*)key;
  ep->hash = hash;
  ep->value = (void*)value;
  self->used++;
}


/*!
  \brief Resizes the OAHash table if necessary

  Resizes the OAHash table if necessary
*/

static inline void oa_hash_resize(OAHash_ptr self, const size_t minused)
{
  size_t newsize;
  OAEntry* old_table;
  OAEntry* new_table;
  OAEntry* ep;
  boolean allocated = false;
  OAEntry small_copy[OA_HASH_MINSIZE];
  size_t i;

  /* Find the smallest table size > minused. */
  for (newsize = OA_HASH_MINSIZE;
       newsize <= minused && newsize > 0;
       newsize <<= 1);

  old_table = self->table;
  allocated = (old_table != self->small_table);

  if (OA_HASH_MINSIZE == newsize) {
    /* A large table is shrinking, or we can't get any smaller. */
    new_table = self->small_table;

    if (new_table == old_table) {
      if (self->fill == self->used) {
        /* We don't need any operation */
        return;
      }

      nusmv_assert(self->fill > self->used);
      memcpy(small_copy, old_table, sizeof(small_copy));
      old_table = small_copy;
    }
  }
  else {
    new_table = ALLOC(OAEntry, newsize);
  }

  nusmv_assert(old_table != new_table);

  /* Prepare the "self" instance for the new resized table */
  self->table = new_table;
  self->mask = newsize - 1;
  memset(new_table, 0, sizeof(OAEntry) * newsize);
  self->used = 0;
  i = self->fill;
  self->fill = 0;

  /* Copy old values in the new table */
  for (ep = old_table; i > 0; ep++) {
    if ((void*)NULL != ep->key && ep->key != (void*)dummy) { /* active entry */
      --i;
      oa_hash_resize_clean(self, ep->key, ep->hash, ep->value);
    }
    else if ((void*)NULL != ep->key) {          /* dummy entry */
      --i;
      assert(ep->key == (void*)dummy);
    }
    /* else key == value == NULL:  nothing to do */
  }

  /* The table is NOT the small_table (which is pre-allocated) */
  if (allocated) { FREE(old_table); }
}

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

OAHash_ptr OAHash_create(OA_HASH_EQ_FUN key_eq_fun,
                         OA_HASH_HASH_FUN key_hash_fun,
                         OA_HASH_FREE_FUN free_entry_fun,
                         void* custom_arg)
{
  OAHash_ptr self = ALLOC(OAHash, 1);
  OA_HASH_CHECK_INSTANCE(self);
  nusmv_assert(NULL != key_hash_fun);
  nusmv_assert(NULL != key_eq_fun);

  oa_hash_init(self, key_eq_fun, key_hash_fun,
               free_entry_fun, custom_arg);
  return self;
}

OAHash_ptr OAHash_copy(const OAHash_ptr self)
{
  OAHash_ptr copy = ALLOC(OAHash, 1);

  OA_HASH_CHECK_INSTANCE(self);
  OA_HASH_CHECK_INSTANCE(copy);

  copy->fill = self->fill;
  copy->used = self->used;
  copy->mask = self->mask;

  copy->table = copy->small_table;

  if (self->table != self->small_table) {
    copy->table = ALLOC(OAEntry, self->mask + 1);
  }

  copy->table = memcpy(copy->table, self->table,
                       (self->mask + 1) * sizeof(OAEntry));

  copy->key_eq_fun = self->key_eq_fun;
  copy->key_hash_fun = self->key_hash_fun;
  copy->free_entry_fun = self->free_entry_fun;

  copy->custom_arg = self->custom_arg;
  copy->free_fun_arg = copy->free_fun_arg;

  return copy;
}

boolean OAHash_insert(OAHash_ptr self, const void* key, const void* value)
{
  boolean res = false;
  size_t n_used = self->used;
  size_t hash_code = self->key_hash_fun(key, self->custom_arg);

  /* NULL keys are NOT admitted */
  nusmv_assert((void*)NULL != key);

  res = oa_hash_insert(self, key, hash_code, value);

  if ((self->used > n_used && self->fill * 3 >= (self->mask + 1) * 2)) {
    /* We need to resize. Quadriply if the size is less than 50000,
       double otherwise. */
    oa_hash_resize(self, (self->used > 50000 ? 2 : 4) * self->used);
  }

  return res;
}

void* OAHash_lookup(OAHash_ptr self, const void* key)
{
  size_t hash_code = self->key_hash_fun(key, self->custom_arg);
  OAEntry* entry = oa_hash_lookup(self, key, hash_code);

  nusmv_assert((OAEntry*)NULL != entry);

  return entry->value;
}

boolean OAHash_has_key(OAHash_ptr self, const void* key)
{
  size_t hash_code = self->key_hash_fun(key, self->custom_arg);
  OAEntry* entry = oa_hash_lookup(self, key, hash_code);

  nusmv_assert((OAEntry*)NULL != entry);

  return (entry->key != (void*)NULL && entry->key != (void*)dummy);
}

boolean OAHash_remove(OAHash_ptr self, const void* key)
{
  OAEntry* ep;
  void* old_value;
  void* old_key;

  size_t hash = self->key_hash_fun(key, self->custom_arg);

  ep = oa_hash_lookup(self, key, hash);

  nusmv_assert((OAEntry*)NULL != ep);

  if ((void*)NULL == ep->key || ep->key == (void*)dummy) {
    return false;
  }

  old_key = ep->key;
  old_value = ep->value;

  ep->key = (void*)dummy;
  ep->value = (void*)NULL;
  ep->hash = 0;

  self->used--;

  oa_hash_clean_entry(self, old_key, old_value);

  return true;
}

size_t OAHash_get_size(const OAHash_ptr self)
{
  return self->used;
}

OAHashIter OAHash_get_first_iter(const OAHash_ptr self)
{
  return oa_hash_iter_validate(self, 0);
}

OAHashIter OAHash_iter_next(const OAHash_ptr self, const OAHashIter iter)
{
  return oa_hash_iter_validate(self, iter + 1);
}

boolean OAHash_iter_is_end(const OAHash_ptr self, const OAHashIter iter)
{
  return iter > self->mask;
}

void OAHash_iter_values(const OAHash_ptr self, const OAHashIter iter,
                        void** key, void** value)
{
  OAEntry* ep;

  nusmv_assert(!OAHash_iter_is_end(self, iter));

  ep = &(self->table[iter]);

  if ((void**)NULL != key) {
    *key = ep->key;
  }

  if ((void**)NULL != value) {
    *value = ep->value;
  }
}

void OAHash_clear(OAHash_ptr self)
{
  size_t i;
  OAEntry* ep;

  for (i = oa_hash_iter_validate(self, 0);
       i <= self->mask;
       i = oa_hash_iter_validate(self, i + 1)) {

    ep = &(self->table[i]);

    oa_hash_clean_entry(self, ep->key, ep->value);
  }

  if (self->table != self->small_table) {
    FREE(self->table);
    self->table = self->small_table;
  }

  memset(self->table, 0, OA_HASH_MINSIZE * sizeof(OAEntry));
  self->fill = 0;
  self->used = 0;
  self->mask = OA_HASH_MINSIZE - 1;
}

void OAHash_destroy(OAHash_ptr self)
{
  OA_HASH_CHECK_INSTANCE(self);

  oa_hash_deinit(self);
  FREE(self);
}

boolean OAHash_pointer_eq_fun(const void* k1, const void* k2, void* arg)
{
  return k1 == k2;
}

size_t OAHash_pointer_hash_fun(const void* k1, void* arg)
{
  return (size_t)k1;
}

boolean OAHash_string_eq_fun(const void* k1, const void* k2, void* arg)
{
  return (k1 == k2) || (strcmp((const char*)k1, (const char*) k2) == 0);
}

size_t OAHash_string_hash_fun(const void* k1, void* arg)
{
  int i;
  int len;
  size_t hash;

  hash = 0;
  len = strlen((const char*) k1);

  for (i=0; i<len; ++i) {
    hash += ((const char*) k1)[i];
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);

  return hash;
}



/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief The OAHash class private initializer

  The OAHash class private initializer

  \sa OAHash_create
*/

void oa_hash_init(OAHash_ptr self,
                  OA_HASH_EQ_FUN key_eq_fun,
                  OA_HASH_HASH_FUN key_hash_fun,
                  OA_HASH_FREE_FUN free_entry_fun,
                  void* custom_arg)
{
  /* members initialization */
  self->fill = 0;
  self->used = 0;
  self->mask = OA_HASH_MINSIZE - 1;
  self->table = self->small_table;

  memset(self->table, 0, OA_HASH_MINSIZE * sizeof(OAEntry));

  self->key_eq_fun = key_eq_fun;
  self->key_hash_fun = key_hash_fun;
  self->free_entry_fun = free_entry_fun;

  self->custom_arg = custom_arg;
  self->free_fun_arg = custom_arg;
}


/*!
  \brief The OAHash class private deinitializer

  The OAHash class private deinitializer

  \sa OAHash_destroy
*/

void oa_hash_deinit(OAHash_ptr self)
{
  /* members deinitialization */

  /* If needed, destroy all keys and values using the provided
     free_entry_fun */
  if (NULL != self->free_entry_fun) {
    size_t i;

    for (i = 0; i <= self->mask; ++i) {
      if ((void*)NULL != self->table[i].key &&
          self->table[i].key != (void*)dummy) {
        self->free_entry_fun(self->table[i].key,
                             self->table[i].value,
                             self->free_fun_arg);
      }
    }
  }

  if (self->small_table != self->table) {
    FREE(self->table);
  }
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/**AutomaticEnd***************************************************************/

