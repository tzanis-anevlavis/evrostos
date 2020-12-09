/* ---------------------------------------------------------------------------


  This file is part of the ``nusmv.core.hrc.dumpers'' package of NuSMV version 2.
  Copyright (C) 2014 by FBK-irst.

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
  \author Michele Dorigatti
  \brief Public interface of class 'HrcDumperAnonymizer'

  \todo: Missing description

*/



#ifndef __NUSMV_CORE_HRC_DUMPERS_HRC_DUMPER_ANONYMIZER_H__
#define __NUSMV_CORE_HRC_DUMPERS_HRC_DUMPER_ANONYMIZER_H__


#include "nusmv/core/hrc/dumpers/HrcDumperSmv.h"
#include "nusmv/core/utils/defs.h"
#include "nusmv/core/node/anonymizers/NodeAnonymizerBase.h"

/*!
  \struct HrcDumperAnonymizer
  \brief Definition of the public accessor for class HrcDumperAnonymizer

  
*/
typedef struct HrcDumperAnonymizer_TAG*  HrcDumperAnonymizer_ptr;

/*!
  \brief To cast and check instances of class HrcDumperAnonymizer

  These macros must be used respectively to cast and to check
  instances of class HrcDumperAnonymizer
*/
#define HRC_DUMPER_ANONYMIZER(self) \
         ((HrcDumperAnonymizer_ptr) self)

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define HRC_DUMPER_ANONYMIZER_CHECK_INSTANCE(self) \
         (nusmv_assert(HRC_DUMPER_ANONYMIZER(self) != HRC_DUMPER_ANONYMIZER(NULL)))



/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \methodof HrcDumperAnonymizer
  \brief The HrcDumperAnonymizer class constructor

  The HrcDumperAnonymizer class constructor

  \sa HrcDumperAnonymizer_destroy
*/
HrcDumperAnonymizer_ptr
HrcDumperAnonymizer_create(const NuSMVEnv_ptr env,
                           FILE* fout,
                           NodeAnonymizerBase_ptr anonymizer);

/*!
  \methodof HrcDumperAnonymizer
  \brief The HrcDumperAnonymizer class destructor

  The HrcDumperAnonymizer class destructor

  \sa HrcDumperAnonymizer_create
*/
void HrcDumperAnonymizer_destroy(HrcDumperAnonymizer_ptr self);


/**AutomaticEnd***************************************************************/



#endif /* __NUSMV_CORE_HRC_DUMPERS_HRC_DUMPER_ANONYMIZER_H__ */
