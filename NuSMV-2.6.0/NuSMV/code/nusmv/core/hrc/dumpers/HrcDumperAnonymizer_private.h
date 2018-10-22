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
  \brief Private and protected interface of class 'HrcDumperAnonymizer'

  This file can be included only by derived and friend classes

*/



#ifndef __NUSMV_CORE_HRC_DUMPERS_HRC_DUMPER_ANONYMIZER_PRIVATE_H__
#define __NUSMV_CORE_HRC_DUMPERS_HRC_DUMPER_ANONYMIZER_PRIVATE_H__


#include "nusmv/core/hrc/dumpers/HrcDumperAnonymizer.h"
#include "nusmv/core/hrc/dumpers/HrcDumperSmv.h"
#include "nusmv/core/hrc/dumpers/HrcDumperSmv_private.h"
#include "nusmv/core/utils/defs.h"


/*!
  \brief HrcDumperAnonymizer class definition derived from
               class HrcDumperSmv

  

  \sa Base class HrcDumperSmv
*/

typedef struct HrcDumperAnonymizer_TAG
{
  /* this MUST stay on the top */
  INHERITS_FROM(HrcDumperSmv);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  NodeAnonymizerBase_ptr anonymizer;

  /* -------------------------------------------------- */
  /*                  Virtual methods                   */
  /* -------------------------------------------------- */

} HrcDumperAnonymizer;



/* ---------------------------------------------------------------------- */
/* Private methods to be used by derivated and friend classes only         */
/* ---------------------------------------------------------------------- */

/*!
  \methodof HrcDumperAnonymizer
  \brief The HrcDumperAnonymizer class private initializer

  The HrcDumperAnonymizer class private initializer

  \sa HrcDumperAnonymizer_create
*/
void hrc_dumper_anonymizer_init(HrcDumperAnonymizer_ptr self,
                                       const NuSMVEnv_ptr env,
                                       FILE* fout,
                                       NodeAnonymizerBase_ptr anonymizer);

/*!
  \methodof HrcDumperAnonymizer
  \brief The HrcDumperAnonymizer class private deinitializer

  The HrcDumperAnonymizer class private deinitializer

  \sa HrcDumperAnonymizer_destroy
*/
void hrc_dumper_anonymizer_deinit(HrcDumperAnonymizer_ptr self);

/*!
  \methodof HrcDumper
  \brief Dumps a node

  
*/
void hrc_dumper_anonymizer_dump_node(HrcDumper_ptr self, node_ptr node);

/*!
  \methodof HrcDumper
  \brief Dumps a snippet

  Here we handle only the few needed case (e.g. we need to
  not anonymize "main", for the others we call the super method)
*/
void hrc_dumper_anonymizer_dump_snippet(HrcDumper_ptr self,
                                               HrcDumperSnippet snippet,
                                               const HrcDumperInfo* info);



#endif /* __NUSMV_CORE_HRC_DUMPERS_HRC_DUMPER_ANONYMIZER_PRIVATE_H__ */
