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
  \brief Implementation of class ' UStringMgr'

  \todo: Missing description

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/UStringMgr.h"
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

typedef struct  UStringMgr_TAG
{
  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  long allocated;             /* Number of string struct allocated */
  long memused;               /* Total memory allocated by the string mgr */
  string_ptr * memoryList;    /* memory manager */
  string_ptr nextFree;        /* list of free strings */
  string_ptr * string_hash;   /* the string hash table */

}  UStringMgr;



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
#define STRING_HASH_SIZE 511

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define STRING_MEM_CHUNK 1022


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void ustring_mgr_init( UStringMgr_ptr self);
static void ustring_mgr_deinit( UStringMgr_ptr self);


static int ustring_mgr_string_hash_fun(string_ptr string);
static int ustring_mgr_string_eq_fun(string_ptr a1, string_ptr a2);
static string_ptr ustring_mgr_string_alloc( UStringMgr_ptr self);
static void ustring_mgr_string_free(string_ptr str);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

UStringMgr_ptr  UStringMgr_create(void)
{
   UStringMgr_ptr self = ALLOC( UStringMgr, 1);
  USTRING_MGR_CHECK_INSTANCE(self);

  ustring_mgr_init(self);
  return self;
}

void  UStringMgr_destroy( UStringMgr_ptr self)
{
  USTRING_MGR_CHECK_INSTANCE(self);

  ustring_mgr_deinit(self);
  FREE(self);
}

string_ptr  UStringMgr_find_string(UStringMgr_ptr self, const char* text)
{
  string_rec str;
  string_ptr * string_hash;
  string_ptr looking;
  int pos;

  str.text = text;
  string_hash = self->string_hash;
  pos = ustring_mgr_string_hash_fun(&str);
  looking = string_hash[pos];

  while (looking != (string_ptr)NULL) {
    if (ustring_mgr_string_eq_fun(&str, looking)) return(looking);
    looking = looking->link;
  }
  /* The string is not in the hash, it is created and then inserted in the hash */
  looking = ustring_mgr_string_alloc(self);
  if (looking == (string_ptr)NULL) {
    error_unreachable_code_msg("find_string: Out of Memory\n");
    return((string_ptr)NULL);
  }
  looking->text = strdup(text);
  looking->link = string_hash[pos];
  string_hash[pos] = looking;
  return looking;
}

const char* UStringMgr_get_string_text(string_ptr str)
{
  nusmv_assert(str != (string_ptr)NULL);
  return (str->text);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The  UStringMgr class private initializer

  The  UStringMgr class private initializer

  \sa  UStringMgr_create
*/
static void ustring_mgr_init( UStringMgr_ptr self)
{
  /* members initialization */
  self->allocated  = 0;
  self->memused    = 0;
  self->memoryList = (string_ptr *)NULL;
  self->nextFree   = (string_ptr)NULL;

  self->string_hash = (string_ptr *)ALLOC(string_ptr, STRING_HASH_SIZE);
  if (self->string_hash == (string_ptr *)NULL) {
    error_unreachable_code_msg("UStringMgr: Out of Memory in allocating the string hash.\n");
  }

  { /* Initializes the node cache */
    int i;

    for(i = 0; i < STRING_HASH_SIZE; i++) self->string_hash[i] = (string_ptr)NULL;
  }

}

/*!
  \brief The  UStringMgr class private deinitializer

  The  UStringMgr class private deinitializer

  \sa  UStringMgr_destroy
*/
static void ustring_mgr_deinit( UStringMgr_ptr self)
{
  /* members deinitialization */
  int i;

  /* Free the hash table and all string copies. */
  for (i = 0; i < STRING_HASH_SIZE; ++i) {
    string_ptr curr = self->string_hash[i];
    while ((string_ptr)NULL != curr) {
      string_ptr next = curr->link;
      ustring_mgr_string_free(curr);
      curr = next;
    }
  }

  FREE(self->string_hash);

  /* Free memory chunks */
  {
    string_ptr * curr = self->memoryList;
    while ((string_ptr*)NULL != curr) {
      string_ptr * next = (string_ptr *) curr[0];
      FREE(curr);
      curr = next;
    }
  }
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int ustring_mgr_string_hash_fun(string_ptr string)
{
  const char* p = string->text;
  unsigned h = 0;

  while (*p) h = (h<<1) + *(p++);
  return(h % STRING_HASH_SIZE);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int ustring_mgr_string_eq_fun(string_ptr a1, string_ptr a2)
{
  return(strcmp(a1->text,a2->text) == 0);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static string_ptr ustring_mgr_string_alloc( UStringMgr_ptr self)
{
  int i;
  string_ptr str;

  if(self->nextFree == (string_ptr)NULL) { /* Memory is full */
    string_ptr list;
    string_ptr * mem = (string_ptr *)ALLOC(string_rec, STRING_MEM_CHUNK + 1);

    if (mem == (string_ptr *)NULL) { /* out of memory */
      error_unreachable_code_msg("string_alloc: out of memory"
                                 "Memory in use for UStringMgr = %ld\n",
                                 self->memused);
      return((string_ptr)NULL);
    }
    else { /* Adjust manager data structure */
      self->memused += (STRING_MEM_CHUNK + 1)* sizeof(string_rec);
      mem[0] = (string_ptr)self->memoryList;
      self->memoryList = mem;
      list = (string_ptr)mem;

      /* Link the new set of allocated strings together */
      i = 1;
      do {
        list[i].link = &list[i+1];
      } while (++i < STRING_MEM_CHUNK);
      list[STRING_MEM_CHUNK].link = (string_ptr)NULL;

      self->nextFree = &list[1];
    }
  }
  /* Now the list of nextFree is not empty */
  self->allocated++;
  str = self->nextFree; /* Takes the first free available string */
  self->nextFree = str->link;
  str->link = (string_ptr)NULL;
  return(str);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void ustring_mgr_string_free(string_ptr str)
{
  FREE(str->text);
}


/**AutomaticEnd***************************************************************/
