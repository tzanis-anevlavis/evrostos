/* ---------------------------------------------------------------------------


   This file is part of the ``utils'' package of NuSMV version 2.
   Copyright (C) 1998-2001 by CMU and FBK-irst.

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
  \author Marco Roveri
  \brief A simple associative list

  This file provides the user with a data structure that
   implemnts an associative list. If there is already an entry with
   the same ky in the table, than the value associated is replaced with
   the new one.

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include <stdlib.h>
#include "cudd/util.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/utils.h" /* for nusmv_assert */
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/Triple.h"
#include "nusmv/core/utils/error.h"
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/
/*
  Initial size of the associative table.
*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ASSOC_HASH_SIZE 127
/*
  Largest everage number of entries per hash element before the table
  is grown.
*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ASSOC_MAX_DENSITY  ST_DEFAULT_MAX_DENSITY

/*
  The factor the table is grown when it becames too full.
*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ASSOC_GROW_FACTOR  ST_DEFAULT_GROW_FACTOR

/*
  If is non-zero, then every time an entry is found, it is moved on
  top of the chain.
*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ASSOC_REORDER_FLAG ST_DEFAULT_REORDER_FLAG

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static unsigned long assoc_hash_fun(node_ptr, int);
static int assoc_string_key_hash_fun(node_ptr key, int size);

static int assoc_eq_fun(node_ptr, node_ptr);
static int assoc_neq_fun(node_ptr a1, node_ptr a2);
static int assoc_string_key_eq_fun(node_ptr, node_ptr);
static int assoc_string_key_neq_fun(node_ptr, node_ptr);

static enum st_retval
assoc_get_key_aux(char *key, char *data, char * arg);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

hash_ptr new_assoc(void)
{
  return new_assoc_with_params((ST_PFICPCP) assoc_neq_fun,
                               (ST_PFICPI) assoc_hash_fun);
}

hash_ptr new_assoc_with_size(int initial_size)
{
  st_table * new_table = st_init_table_with_params((ST_PFICPCP) assoc_neq_fun,
                                                   (ST_PFICPI) assoc_hash_fun,
                                                   initial_size,
                                                   ASSOC_MAX_DENSITY,
                                                   ASSOC_GROW_FACTOR,
                                                   ASSOC_REORDER_FLAG);
  if (new_table == (st_table *)NULL) {
    error_unreachable_code_msg("new_assoc: Out of Memory\n");
  }

  return ((hash_ptr)new_table);
}

hash_ptr new_assoc_with_params(ST_PFICPCP compare_fun, ST_PFICPI hash_fun)
{
  st_table * new_table = st_init_table_with_params(compare_fun,
                                                   hash_fun,
                                                   ASSOC_HASH_SIZE,
                                                   ASSOC_MAX_DENSITY,
                                                   ASSOC_GROW_FACTOR,
                                                   ASSOC_REORDER_FLAG);
  if (new_table == (st_table *)NULL) {
    error_unreachable_code_msg("new_assoc: Out of Memory\n");
  }

  return ((hash_ptr)new_table);
}

hash_ptr new_assoc_string_key(void)
{
  st_table * new_table =
    st_init_table_with_params((ST_PFICPCP) assoc_string_key_neq_fun,
                              (ST_PFICPI) assoc_string_key_hash_fun,
                              ASSOC_HASH_SIZE,
                              ASSOC_MAX_DENSITY,
                              ASSOC_GROW_FACTOR,
                              ASSOC_REORDER_FLAG);

  if (new_table == (st_table *)NULL) {
    error_unreachable_code_msg("new_assoc_string_key: Out of Memory\n");
  }
  return ((hash_ptr)new_table);
}

hash_ptr copy_assoc(hash_ptr hash)
{
  return (hash_ptr) st_copy((st_table *) hash);
}

/*!
  \brief Performs a deep copy of self

  "copy_fun" will be called as
  copy_fun(key, value, copying_hash).
  This follows the style of clear_assoc_and_free_entries. For an example see
  ocra/core/OcraComponent.c:oc_contract_assoc_copy_func
*/

hash_ptr assoc_deep_copy(hash_ptr hash, ST_PFSR copy_fun)
{
  st_table* self = (st_table*)hash;
  hash_ptr copy = st_init_table_with_params(self->compare,
                                            self->hash,
                                            self->num_bins,
                                            self->max_density,
                                            self->grow_factor,
                                            self->reorder_flag);

  st_foreach(self, copy_fun, (char*)copy);

  return copy;
}

node_ptr find_assoc(hash_ptr hash, node_ptr key)
{
  node_ptr data;

  if (st_lookup((st_table *)hash, (char *)key, (char **)&data)) return(data);
  else return(Nil);
}

node_ptr assoc_get_keys(hash_ptr hash, NodeMgr_ptr nodemgr, boolean ignore_nils)
{
  Triple triple;
  Triple_init(&triple, nodemgr, Nil, (node_ptr)ignore_nils);

  st_foreach(hash, assoc_get_key_aux, (char*)&triple);
  return NODE_PTR(Triple_get_second(&triple));
}

/*!
  \brief 

  
*/

/* Inserts association key -> data. If the key has already been in the
   table then the old associated data is rewritten */
void insert_assoc(hash_ptr hash, node_ptr key, node_ptr data)
{
  (void)st_insert((st_table *)hash, (char *)key, (char *)data);
}

node_ptr remove_assoc(hash_ptr hash, node_ptr key)
{
  node_ptr data;
  int tmp = st_delete((st_table *)hash, (char **)&key, (char **)&data);
  return tmp ? data : Nil;
}

void free_assoc(hash_ptr hash)
{
  (void)st_free_table((st_table *)hash);
}

/*!
  \brief 

  
*/

static enum st_retval delete_entry(char *key, char *data, char * arg)
{
  return(ST_DELETE);
}

void clear_assoc(hash_ptr hash)
{ st_foreach(hash, delete_entry, NULL); }

void clear_assoc_and_free_entries(hash_ptr hash, ST_PFSR fn)
{ clear_assoc_and_free_entries_arg(hash, fn, NULL); }

void clear_assoc_and_free_entries_arg(hash_ptr hash, ST_PFSR fn, char* arg)
{
  nusmv_assert(hash != NULL);
  st_foreach(hash, fn, arg);
}

void assoc_foreach(hash_ptr hash, ST_PFSR fn, char *arg) {
  nusmv_assert((hash_ptr)NULL != hash);

  st_foreach(hash, fn, arg);
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static unsigned long assoc_hash_fun(node_ptr key, int size)
{ return((unsigned long)(key) % size); }

/*!
  \brief One-at-a-Time Hash function

  Used to hash string keys.
*/
static int assoc_string_key_hash_fun(node_ptr key, int size)
{
  int hash, i;
  int len;

  hash = 0;
  len = strlen((const char*) key);

  for (i=0; i<len; ++i) {
    hash += ((const char*) key)[i];
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);

  if (hash < 0)
    hash = -hash;

  return (hash % size);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int assoc_eq_fun(node_ptr a1, node_ptr a2)
{ return((a1) == (a2)); }

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int assoc_neq_fun(node_ptr a1, node_ptr a2)
{ return((a1) != (a2)); }

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int assoc_string_key_eq_fun(node_ptr a1, node_ptr a2)
{
  return (assoc_eq_fun(a1, a2) ||
          (strcmp((const char*) a1, (const char*) a2) == 0));
}

/*!
  \brief 

  
*/
static int assoc_string_key_neq_fun(node_ptr a1, node_ptr a2)
{
  return (assoc_neq_fun(a1, a2) &&
          (strcmp((const char*) a1, (const char*) a2) != 0));
}

/*!
  \brief 

  
*/

/* A private service for assoc_get_keys */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static enum st_retval assoc_get_key_aux(char *key, char *data, char * arg)
{
  Triple_ptr triple = TRIPLE(arg);
  NodeMgr_ptr nodemgr = NODE_MGR(Triple_get_first(triple));
  node_ptr res = NODE_PTR(Triple_get_second(triple));
  boolean ignore_nils = (boolean)NODE_PTR(Triple_get_third(triple));

  if (!ignore_nils || data != (char*) NULL) {
    Triple_set_second(triple, cons(nodemgr, (node_ptr) key, res));
  }

  return ST_CONTINUE;
}
