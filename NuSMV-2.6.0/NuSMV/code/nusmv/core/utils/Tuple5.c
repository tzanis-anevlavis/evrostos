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
  \brief Implementation of class 'Tuple5'

  This class realizes a tuple of 5 elements. It can be very useful
               for hash keys. W.r.t. using CONS of node_ptr, it can save a lot
               of memory.

*/


#include "nusmv/core/utils/Tuple5.h"
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

static void tuple5_init(Tuple5_ptr self);
static void tuple5_deinit(Tuple5_ptr self);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

Tuple5_ptr Tuple5_create(void* first, void* second, void* third,
                         void* forth, void* fifth)
{
  Tuple5_ptr self = ALLOC(Tuple5, 1);
  TUPLE_5_CHECK_INSTANCE(self);

  tuple5_init(self);

  Tuple5_set_values(self, first, second, third, forth, fifth);

  return self;
}

void Tuple5_init(Tuple5_ptr self, void* first, void* second, void* third,
                 void* forth, void* fifth)
{
  tuple5_init(self);
  Tuple5_set_values(self, first, second, third, forth, fifth);
}

void Tuple5_destroy(Tuple5_ptr self)
{
  TUPLE_5_CHECK_INSTANCE(self);

  tuple5_deinit(self);
  FREE(self);
}

void Tuple5_freeze(Tuple5_ptr self)
{
  self->frozen = true;
}

boolean Tuple5_is_freezed(const Tuple5_ptr self)
{
  return self->frozen;
}

void* Tuple5_get_first(const Tuple5_ptr self)
{
  return self->first;
}

void* Tuple5_get_second(const Tuple5_ptr self)
{
  return self->second;
}

void* Tuple5_get_third(const Tuple5_ptr self)
{
  return self->third;
}

void* Tuple5_get_forth(const Tuple5_ptr self)
{
  return self->forth;
}

void* Tuple5_get_fifth(const Tuple5_ptr self)
{
  return self->fifth;
}

void Tuple5_set_first(Tuple5_ptr self, void* first)
{
  nusmv_assert(!Tuple5_is_freezed(self));
  self->first = first;
}

void Tuple5_set_second(Tuple5_ptr self, void* second)
{
  nusmv_assert(!Tuple5_is_freezed(self));
  self->second = second;
}

void Tuple5_set_third(Tuple5_ptr self, void* third)
{
  nusmv_assert(!Tuple5_is_freezed(self));
  self->third = third;
}

void Tuple5_set_forth(Tuple5_ptr self, void* forth)
{
  nusmv_assert(!Tuple5_is_freezed(self));
  self->forth = forth;
}

void Tuple5_set_fifth(Tuple5_ptr self, void* fifth)
{
  nusmv_assert(!Tuple5_is_freezed(self));
  self->fifth = fifth;
}

void Tuple5_set_values(Tuple5_ptr self,
                       void* first, void* second, void* third,
                       void* forth, void* fifth)
{
  nusmv_assert(!Tuple5_is_freezed(self));
  self->first = first;
  self->second = second;
  self->third = third;
  self->forth = forth;
  self->fifth = fifth;
}

int Tuple5_compare(const Tuple5_ptr a, const Tuple5_ptr b)
{
  if (a == b) { return 0; }

  if (a->first != b->first) {
    return (char*)(b->first) - (char*)(a->first);
  }

  if (a->second != b->second) {
    return (char*)(b->second) - (char*)(a->second);
  }

  if (a->third != b->third) {
    return (char*)(b->third) - (char*)(a->third);
  }

  if (a->forth != b->forth) {
    return (char*)(b->forth) - (char*)(a->forth);
  }

  if (a->fifth != b->fifth) {
    return (char*)(b->fifth) - (char*)(a->fifth);
  }

  return 0;
}

unsigned long Tuple5_hash(const Tuple5_ptr self, int size)
{
  size_t ret = ((((size_t)self->first ) + 31) +
                (((size_t)self->second) << 1) +
                (((size_t)self->third ) << 2) +
                (((size_t)self->forth ) << 3) +
                (((size_t)self->fifth ) << 4));


  return (unsigned long)(ret % size);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The Tuple5 class private initializer

  The Tuple5 class private initializer

  \sa Tuple5_create
*/
static void tuple5_init(Tuple5_ptr self)
{
  /* members initialization */
  self->first = NULL;
  self->second = NULL;
  self->third = NULL;
  self->forth = NULL;
  self->fifth = NULL;
  self->frozen = false;
}

/*!
  \brief The Tuple5 class private deinitializer

  The Tuple5 class private deinitializer

  \sa Tuple5_destroy
*/
static void tuple5_deinit(Tuple5_ptr self)
{
  /* members deinitialization */

}



/**AutomaticEnd***************************************************************/

