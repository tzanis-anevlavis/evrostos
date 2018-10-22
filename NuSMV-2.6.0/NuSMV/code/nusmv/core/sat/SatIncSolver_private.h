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
  \brief The private interface of class SatIncSolver

  Private definition to be used by derived classes

*/

#ifndef __NUSMV_CORE_SAT_SAT_INC_SOLVER_PRIVATE_H__
#define __NUSMV_CORE_SAT_SAT_INC_SOLVER_PRIVATE_H__

#include "nusmv/core/sat/SatIncSolver.h"
#include "nusmv/core/sat/SatSolver_private.h"

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/*!
  \brief SatIncSolver Class

   This class defines a prototype for a generic SatIncSolver. This
  class is virtual and must be specialized. 
*/

typedef struct SatIncSolver_TAG
{
  INHERITS_FROM(SatSolver);

  /* ---------------------------------------------------------------------- */
  /* Virtual Methods                                                        */
  /* ---------------------------------------------------------------------- */

  /* creates a new group */
  VIRTUAL SatSolverGroup
  (*create_group) (const SatIncSolver_ptr self);

  /* destroys existing group */
  VIRTUAL void
  (*destroy_group) (const SatIncSolver_ptr self, SatSolverGroup group);
  /* moves formulas from 'group' into permanent one and destroys 'group' */
  VIRTUAL void
  (*move_to_permanent_and_destroy_group) (const SatIncSolver_ptr self,
                                          SatSolverGroup group);

  /* tries to solve formulas in the groups in the given list and the permanent
     group */
  VIRTUAL SatSolverResult
  (*solve_groups) (const SatIncSolver_ptr self, const Olist_ptr groups);

  /* tries to solve the formulas belonging to the solver except those
     in the group from the given list. Permanent group should not be
     in the list ever. */
  VIRTUAL SatSolverResult
  (*solve_without_groups) (const SatIncSolver_ptr self,
                           const Olist_ptr groups);
} SatIncSolver;

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/
/*!
  \methodof SatIncSolver
  \todo
*/
void sat_inc_solver_init(SatIncSolver_ptr self,
                         const NuSMVEnv_ptr env,
                         const char* name);

/*!
  \methodof SatIncSolver
  \todo
*/
void sat_inc_solver_deinit(SatIncSolver_ptr self);

/* pure virtual functions */
/*!
  \methodof SatIncSolver
  \todo
*/
SatSolverGroup
sat_inc_solver_create_group(const SatIncSolver_ptr self);

/*!
  \methodof SatIncSolver
  \todo
*/
void
sat_inc_solver_destroy_group(const SatIncSolver_ptr self,
                             SatSolverGroup group);

/*!
  \methodof SatIncSolver
  \todo
*/
void
sat_inc_solver_move_to_permanent_and_destroy_group(const SatIncSolver_ptr self,
SatSolverGroup group);
/*!
  \methodof SatIncSolver
  \todo
*/
SatSolverResult
sat_inc_solver_solve_groups(const SatIncSolver_ptr self,
                            const Olist_ptr groups);

/*!
  \methodof SatIncSolver
  \todo
*/
SatSolverResult
sat_inc_solver_solve_without_groups(const SatIncSolver_ptr self,
                                    const Olist_ptr groups);

/**AutomaticEnd***************************************************************/

#endif /* __NUSMV_CORE_SAT_SAT_INC_SOLVER_PRIVATE_H__ */
