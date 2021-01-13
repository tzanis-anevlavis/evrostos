/* ---------------------------------------------------------------------------


  This file is part of the ``cinit'' package of NuSMV version 2.
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
  \brief Implementation of class 'NuSMVEnv'

  The NuSMV Environment class implementation. The NuSMV
               Environment is intended to be a container, which maps
               string keys into void* (class instances) and string
               keys into booleans (flags)

*/


#ifdef NUSMV_ENV_DEBUG_MODE
#include <stdio.h>
#endif

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/cinit/NuSMVEnv.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/OAHash.h"
#include "nusmv/core/utils/UStringMgr.h"
#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define FAST_ARRAY_SIZE 223

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef enum  {
  DEF_STRUCT_HASH_PTR
} DefStructType;

typedef struct NuSMVEnvDefStruct_TAG
{
  DefStructType type;
  void* instance;
} NuSMVEnvDefStruct;

typedef struct NuSMVEnv_TAG
{

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */

  /* Custom */
  OAHash_ptr custom_values;
  hash_ptr flag_values;
  UStringMgr_ptr strmgr;

#ifdef NUSMV_ENV_DEBUG_MODE
  size_t instance_id;
#endif

  void* fast_values[FAST_ARRAY_SIZE];

  NodeList_ptr handled_structures;

} NuSMVEnv;

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

#ifdef NUSMV_ENV_DEBUG_MODE
static size_t nusmv_env_instance_count = 0;
#endif

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void nusmv_env_init(NuSMVEnv_ptr self);
static void nusmv_env_deinit(NuSMVEnv_ptr self);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

NuSMVEnv_ptr NuSMVEnv_create(void)
{
  NuSMVEnv_ptr self = ALLOC(NuSMVEnv, 1);
  NUSMV_ENV_CHECK_INSTANCE(self);

  nusmv_env_init(self);
  return self;
}

void NuSMVEnv_destroy(NuSMVEnv_ptr self)
{
  NUSMV_ENV_CHECK_INSTANCE(self);

  nusmv_env_deinit(self);
  FREE(self);
}

void* NuSMVEnv_get_value(const NuSMVEnv_ptr self, const char* key)
{
  void* res = (void*)NULL;

#ifdef NUSMV_ENV_DEBUG_MODE
  fprintf(stderr,  "[NuSMVEnv %zu] Getting value for instance '%s'\n",
          self->instance_id, key);
#endif

  if (key[0] == '+') {
    res = self->fast_values[(short int)key[1] - 33];
  }
  else {
    string_ptr ustring = UStringMgr_find_string(self->strmgr, (char*)key);

    nusmv_assert(OA_HASH(NULL) != self->custom_values);

    res = (void*)OAHash_lookup(self->custom_values, ustring);
  }

  nusmv_assert((void*)NULL != res);

  return res;
}

boolean NuSMVEnv_has_value(const NuSMVEnv_ptr self, const char* key)
{
  string_ptr ustring;

  if (key[0] == '+') {
    return ((void*)NULL != (self->fast_values[(short int)key[1] - 33]));
  }

  if (OA_HASH(NULL) == self->custom_values) {
    return false;
  }

  ustring =  UStringMgr_find_string(self->strmgr, (char*)key);

  return (Nil != OAHash_lookup(self->custom_values, ustring));
}

void NuSMVEnv_set_value(NuSMVEnv_ptr self, const char* key, void* instance)
{
#ifdef NUSMV_ENV_DEBUG_MODE
  fprintf(stderr,  "[NuSMVEnv %zu] Setting value for instance '%s'\n",
          self->instance_id, key);
#endif

  nusmv_assert(NULL != instance);

  if (key[0] == '+') {
    nusmv_assert((void*)NULL == self->fast_values[(short int)key[1] - 33]);
    self->fast_values[(short int)key[1] - 33] = instance;
  }
  else {
    string_ptr ustring;

    if (OA_HASH(NULL) == self->custom_values) {
      self->custom_values = OAHash_create(OAHash_pointer_eq_fun,
                                          OAHash_pointer_hash_fun,
                                          NULL, NULL);
    }

    ustring =  UStringMgr_find_string(self->strmgr, (char*)key);

    nusmv_assert(Nil == OAHash_lookup(self->custom_values, ustring));

    OAHash_insert(self->custom_values, ustring, instance);
  }
}

void* NuSMVEnv_set_or_replace_value(NuSMVEnv_ptr self,
                                    const char* key, void* instance)
{
  void* old_value = (void*)NULL;

#ifdef NUSMV_ENV_DEBUG_MODE
  fprintf(stderr,
          "[NuSMVEnv %zu] Setting or replacing value for instance '%s'\n",
          self->instance_id, key);
#endif

  if (key[0] == '+') {
    old_value = self->fast_values[(short int)key[1] - 33];
    self->fast_values[(short int)key[1] - 33] = instance;
  }
  else {
    string_ptr ustring;

    if (OA_HASH(NULL) == self->custom_values) {
      self->custom_values = OAHash_create(OAHash_pointer_eq_fun,
                                          OAHash_pointer_hash_fun,
                                          NULL, NULL);
    }

    ustring =  UStringMgr_find_string(self->strmgr, (char*)key);

    old_value = OAHash_lookup(self->custom_values, ustring);

    OAHash_insert(self->custom_values, ustring, instance);
  }

  return old_value;
}

void* NuSMVEnv_remove_value(NuSMVEnv_ptr self, const char* key)
{
  string_ptr ustring;
  void* res = (void*)NULL;

#ifdef NUSMV_ENV_DEBUG_MODE
  fprintf(stderr,  "[NuSMVEnv %zu] Removing value for instance '%s'\n",
          self->instance_id, key);
#endif

  if (key[0] == '+') {
    res = self->fast_values[(short int)key[1] - 33];
    self->fast_values[(short int)key[1] - 33] = (void*)NULL;
  }
  else {
    if (OA_HASH(NULL) == self->custom_values) {
      return (void*)NULL;
    }

    ustring =  UStringMgr_find_string(self->strmgr, (char*)key);

    res = OAHash_lookup(self->custom_values, ustring);
    OAHash_remove(self->custom_values, ustring);

    nusmv_assert((void*)NULL != res);
  }

  return res;
}

boolean NuSMVEnv_get_flag(const NuSMVEnv_ptr self, const char* key)
{
  string_ptr ustring;

  if ((hash_ptr)NULL == self->flag_values) {
    return false;
  }

  ustring =  UStringMgr_find_string(self->strmgr, (char*)key);

  return NODE_TO_INT(find_assoc(self->flag_values, NODE_PTR(ustring))) == 2 ? true : false;
}

boolean NuSMVEnv_has_flag(const NuSMVEnv_ptr self, const char* key)
{
  string_ptr ustring;

  if ((hash_ptr)NULL == self->flag_values) {
    return false;
  }

  ustring =  UStringMgr_find_string(self->strmgr, (char*)key);

  return (Nil != find_assoc(self->flag_values, NODE_PTR(ustring)));
}

void NuSMVEnv_set_flag(NuSMVEnv_ptr self, const char* key, boolean value)
{
  string_ptr ustring;

  if ((hash_ptr)NULL == self->flag_values) {
    self->flag_values = new_assoc();
  }

  ustring =  UStringMgr_find_string(self->strmgr, (char*)key);

  insert_assoc(self->flag_values, NODE_PTR(ustring),
               (value ? NODE_FROM_INT(2) : NODE_FROM_INT(1)));
}

boolean NuSMVEnv_remove_flag(NuSMVEnv_ptr self, const char* key)
{
  string_ptr ustring;

  if ((hash_ptr)NULL == self->flag_values) {
    return false;
  }

  ustring =  UStringMgr_find_string(self->strmgr, (char*)key);

  return NODE_TO_INT(remove_assoc(self->flag_values, NODE_PTR(ustring))) == 2 ? true : false;
}

hash_ptr NuSMVEnv_get_handled_hash_ptr(NuSMVEnv_ptr self, const char* key)
{
  hash_ptr res;

  NUSMV_ENV_CHECK_INSTANCE(self);

  if (NuSMVEnv_has_value(self, key)) {
    res = (hash_ptr)NuSMVEnv_get_value(self, key);
  }
  else {
    NuSMVEnvDefStruct* neds = ALLOC(NuSMVEnvDefStruct, 1);

    res = new_assoc();

    NuSMVEnv_set_value(self, key, res);

    if (NODE_LIST(NULL) == self->handled_structures) {
      self->handled_structures = NodeList_create();
    }

    neds->type = DEF_STRUCT_HASH_PTR;
    neds->instance = res;

    NodeList_append(self->handled_structures, NODE_PTR(neds));
  }

  return res;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The NuSMVEnv class private initializer

  The NuSMVEnv class private initializer

  \sa NuSMVEnv_create
*/
static void nusmv_env_init(NuSMVEnv_ptr self)
{
#ifdef NUSMV_ENV_DEBUG_MODE
  self->instance_id = nusmv_env_instance_count;
  nusmv_env_instance_count++;
  fprintf(stderr,  "[NuSMVEnv %zu] Created instance\n", self->instance_id);
#endif

  /* members initialization */
  self->custom_values = OA_HASH(NULL);
  self->flag_values = (hash_ptr)NULL;
  self->strmgr =  UStringMgr_create();
  self->handled_structures = NODE_LIST(NULL);

  memset(self->fast_values, 0, sizeof(void*) * FAST_ARRAY_SIZE);
}

/*!
  \brief The NuSMVEnv class private deinitializer

  The NuSMVEnv class private deinitializer

  \sa NuSMVEnv_destroy
*/
static void nusmv_env_deinit(NuSMVEnv_ptr self)
{

#ifdef NUSMV_ENV_DEBUG_MODE
  fprintf(stderr,  "[NuSMVEnv %zu] Destroying instance\n", self->instance_id);
#endif
  /* members deinitialization */
  if (OA_HASH(NULL) != self->custom_values) {
    OAHash_destroy(self->custom_values);
  }

  if ((hash_ptr)NULL != self->flag_values) {
    free_assoc(self->flag_values);
  }

  if (NODE_LIST(NULL) != self->handled_structures) {
    ListIter_ptr iter;

    NODE_LIST_FOREACH(self->handled_structures, iter) {
      NuSMVEnvDefStruct* neds =
        (NuSMVEnvDefStruct*)NodeList_get_elem_at(self->handled_structures, iter);

      switch (neds->type) {
      case DEF_STRUCT_HASH_PTR:
        free_assoc((hash_ptr)neds->instance);
        break;
      default:
        error_unreachable_code_msg("Unhandled structure type");
      }

      FREE(neds);
    }

    NodeList_destroy(self->handled_structures);
  }

  UStringMgr_destroy(self->strmgr);
}


/**AutomaticEnd***************************************************************/

