/* ---------------------------------------------------------------------------


  This file is part of the ``sat'' package of NuSMV version 2.
  Copyright (C) 2004 by FBK-irst.

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
  \brief The private interface of class SatZchaff

  Private definition to be used by derived classes

*/

#ifndef __NUSMV_CORE_SAT_SOLVERS_SAT_ZCHAFF_PRIVATE_H__
#define __NUSMV_CORE_SAT_SOLVERS_SAT_ZCHAFF_PRIVATE_H__

#include "nusmv/core/sat/solvers/SatZchaff.h"
#include "nusmv/core/sat/solvers/satZChaffIfc.h"


#include "nusmv/core/sat/SatIncSolver_private.h"
#include "nusmv/core/utils/assoc.h"

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/*!
  \brief SatZchaff Class

   This class defines a prototype for a generic SatZchaff. This
  class is virtual and must be specialized. 
*/

typedef struct SatZchaff_TAG
{
  INHERITS_FROM(SatIncSolver);

  SAT_Manager zchaffSolver; /* actual instance of zchaff */
  /* All input variables are represented by the  internal ones inside the 
     SatZchaff. Bellow two hash table perform the convertion in both ways */
  hash_ptr cnfVar2zchaffVar;/* converts CNF variable to internal variable */
  hash_ptr zchaffVar2cnfVar;/* converts internal variable into CNF variable */

  /* contains set of conflicting assumptions after using
     SatMinisat_solve_permanent_group_assume */
  Slist_ptr conflict;
} SatZchaff;

/**AutomaticStart*************************************************************/ 

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/
/*!
  \methodof SatZchaff
  \todo
*/
void sat_zchaff_init(SatZchaff_ptr self,
                     const NuSMVEnv_ptr env,
                     const char* name);

/*!
  \methodof SatZchaff
  \todo
*/
void sat_zchaff_deinit(SatZchaff_ptr self);

/*!
  \methodof SatZchaff
  \todo
*/
int sat_zchaff_cnfLiteral2zchaffLiteral(SatZchaff_ptr self,
                                        int cnfLitaral);
/*!
  \methodof SatZchaff
  \todo
*/
int sat_zchaff_zchaffLiteral2cnfLiteral(SatZchaff_ptr self,
                                        int zchaffLiteral);


/* virtual function from SatSolver */
/*!
  \methodof SatZchaff
  \todo
*/
void sat_zchaff_add(const SatSolver_ptr self,
                    const Be_Cnf_ptr cnfProb,
                    SatSolverGroup group);

/*!
  \methodof SatZchaff
  \todo
*/
void sat_zchaff_set_polarity(const SatSolver_ptr self,
                             const Be_Cnf_ptr cnfProb,
                             int polarity,
                             SatSolverGroup group);

/*!
  \methodof SatZchaff
  \todo
*/
void sat_zchaff_set_preferred_variables(const SatSolver_ptr self,
                                        const Slist_ptr cnfVars);

/*!
  \methodof SatZchaff
  \todo
*/
void sat_zchaff_clear_preferred_variables(const SatSolver_ptr self);

/*!
  \methodof SatZchaff
  \todo
*/
SatSolverResult sat_zchaff_solve_all_groups(const SatSolver_ptr self);

/*!
  \methodof SatZchaff
  \todo
*/
Slist_ptr sat_zchaff_make_model(const SatSolver_ptr self);

/* virtual functions from SatIncSolver */
/*!
  \methodof SatZchaff
  \todo
*/
SatSolverGroup 
sat_zchaff_create_group(const SatIncSolver_ptr self);

/*!
  \methodof SatZchaff
  \todo
*/
void
sat_zchaff_destroy_group(const SatIncSolver_ptr self,
                         SatSolverGroup group);

/*!
  \methodof SatZchaff
  \todo
*/
void
sat_zchaff_move_to_permanent_and_destroy_group(const SatIncSolver_ptr self,
SatSolverGroup group);
/*!
  \methodof SatZchaff
  \todo
*/
SatSolverResult
sat_zchaff_solve_groups(const SatIncSolver_ptr self,
                        const Olist_ptr groups);

/*!
  \methodof SatZchaff
  \todo
*/
SatSolverResult
sat_zchaff_solve_without_groups(const SatIncSolver_ptr self,
                                const Olist_ptr groups);

/*!
  \methodof SatZchaff
  \todo
*/
SatSolverResult sat_zchaff_solve_permanent_group_assume(const SatSolver_ptr self, Slist_ptr assumption);

Slist_ptr sat_zchaff_get_conflicts(const SatSolver_ptr);

/*!
  \methodof SatZchaff
  \brief Obtains the set of conflicting assumptions from zCahdd

  

  \sa sat_zchaff_solve_permanent_group_assume,
  sat_zchaff_get_conflict
*/
Slist_ptr sat_zchaff_make_conflicts(const SatZchaff_ptr self);

/**AutomaticEnd***************************************************************/

#endif /* __NUSMV_CORE_SAT_SOLVERS_SAT_ZCHAFF_PRIVATE_H__ */
