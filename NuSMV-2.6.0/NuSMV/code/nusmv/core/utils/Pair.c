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
  \brief Implementation of class 'Pair'

  \todo: Missing description

*/


#include "nusmv/core/utils/Pair.h"
#include "nusmv/core/utils/utils.h"


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

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void pair_init(Pair_ptr self);
static void pair_deinit(Pair_ptr self);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

Pair_ptr Pair_create(void* first, void* second)
{
  Pair_ptr self = ALLOC(Pair, 1);
  PAIR_CHECK_INSTANCE(self);

  pair_init(self);

  Pair_set_values(self, first, second);

  return self;
}

void Pair_init(Pair_ptr self, void* first, void* second)
{
  pair_init(self);
  Pair_set_values(self, first, second);
}

void Pair_destroy(Pair_ptr self)
{
  PAIR_CHECK_INSTANCE(self);

  pair_deinit(self);
  FREE(self);
}

void Pair_freeze(Pair_ptr self)
{
  self->frozen = true;
}

boolean Pair_is_freezed(const Pair_ptr self)
{
  return self->frozen;
}

void* Pair_get_first(const Pair_ptr self)
{
  return self->first;
}

void* Pair_get_second(const Pair_ptr self)
{
  return self->second;
}

void Pair_set_first(Pair_ptr self, void* first)
{
  nusmv_assert(!Pair_is_freezed(self));
  self->first = first;
}

void Pair_set_second(Pair_ptr self, void* second)
{
  nusmv_assert(!Pair_is_freezed(self));
  self->second = second;
}

void Pair_set_values(Pair_ptr self, void* first, void* second)
{
  nusmv_assert(!Pair_is_freezed(self));
  self->first = first;
  self->second = second;
}

int Pair_compare(const Pair_ptr a, const Pair_ptr b)
{
  if (a == b) { return 0; }

  if (a->first != b->first) {
    return (char*)(b->first) - (char*)(a->first);
  }

  if (a->second != b->second) {
    return (char*)(b->second) - (char*)(a->second);
  }

  return 0;
}

unsigned long Pair_hash(const Pair_ptr self, int size)
{
  size_t ret = ((((size_t)self->first) + 31) +
                (((size_t)self->second) << 1));

  return (unsigned long)(ret % size);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The Pair class private initializer

  The Pair class private initializer

  \sa Pair_create
*/
static void pair_init(Pair_ptr self)
{
  /* members initialization */
  self->first = NULL;
  self->second = NULL;
  self->frozen = false;
}

/*!
  \brief The Pair class private deinitializer

  The Pair class private deinitializer

  \sa Pair_destroy
*/
static void pair_deinit(Pair_ptr self)
{
  /* members deinitialization */
}



/**AutomaticEnd***************************************************************/

