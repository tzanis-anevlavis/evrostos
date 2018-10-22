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
  \brief The private interface of class SatSolver

  Private definition to be used by derived classes

*/

#ifndef __NUSMV_CORE_SAT_SAT_SOLVER_PRIVATE_H__
#define __NUSMV_CORE_SAT_SAT_SOLVER_PRIVATE_H__

#include "nusmv/core/sat/SatSolver.h"
#include "nusmv/core/utils/EnvObject_private.h"
#include "nusmv/core/utils/Olist.h"
#include "nusmv/core/utils/list.h"

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/*!
  \brief SatSolver Class

   This class defines a prototype for a generic SatSolver. This
  class is virtual and must be specialized. 
*/

typedef struct SatSolver_TAG
{
  INHERITS_FROM(EnvObject);

  char* name; /* name of the solver */
  long solvingTime; /* time of the last solving */
  Slist_ptr model; /* the model created during last successful solving */
  Slist_ptr conflicts; /* the conflict generated after last call to solve
                       under assumption */
  /* groups belonging to the solver. The permanent group is always the first
     in the list!
  */
  Olist_ptr existingGroups;
  Olist_ptr unsatisfiableGroups; /* groups which contains unsatisfiable formulas */

  boolean interpolation; /* true when interpolation is used */

  /* ---------------------------------------------------------------------- */
  /* Virtual Methods                                                        */
  /* ---------------------------------------------------------------------- */
  /* adds a set of CNF clauses in the solver, but not specifies the polarity
     of formula */
  VIRTUAL void (*add) (const SatSolver_ptr self,
                       const Be_Cnf_ptr cnfProb,
                       SatSolverGroup group);

  /* sets the polarity of a formula in a group */
  VIRTUAL void (*set_polarity) (const SatSolver_ptr self,
                                const Be_Cnf_ptr cnfProb,
                                int polarity,
                                SatSolverGroup group);

  /* sets the preferred variables in the solver */
  VIRTUAL void (*set_preferred_variables) (const SatSolver_ptr self,
                                           const Slist_ptr cnfVars);

  /* clears the preferred variables in the solver */
  VIRTUAL void (*clear_preferred_variables) (const SatSolver_ptr self);

  /* solves formulas in the groups 'groupList' beloning to the solver */
  VIRTUAL SatSolverResult (*solve_all_groups) (const SatSolver_ptr self);

  /* solves formulas in the groups 'groupList' beloning to the solver
     under assumptions */
  VIRTUAL SatSolverResult (*solve_all_groups_assume) (const SatSolver_ptr self,
                                                      Slist_ptr assumption);

  /* creates the model. The previous call to 'solve' must return SATISFIABLE */
  VIRTUAL Slist_ptr (*make_model) (const SatSolver_ptr self);

  /* converts an internal minisat literal into a cnf literal. This is used by
     interpolation based algorithms */
  VIRTUAL int (*get_cnf_var) (const SatSolver_ptr self, int lit);

  /* creates the conflicts, solving under assumptions must be invoked before */
  VIRTUAL Slist_ptr (*get_conflicts) (const SatSolver_ptr self);

  /* Random polarity and polarity mode setter/getter */
  VIRTUAL void (*set_random_mode) (SatSolver_ptr self, double seed);
  VIRTUAL void (*set_polarity_mode) (SatSolver_ptr self, int mode);
  VIRTUAL int  (*get_polarity_mode) (const SatSolver_ptr self);

  /* Interpolation groups management */
  VIRTUAL SatSolverItpGroup (*curr_itp_group) (SatSolver_ptr self);
  VIRTUAL SatSolverItpGroup (*new_itp_group) (SatSolver_ptr self);

  /* callback-based interpolator (available only when using custom MiniSat) */
  VIRTUAL Term (*extract_interpolant)(SatSolver_ptr self, int nof_ga_groups,
                                      SatSolverItpGroup* ga_groups,
                                      TermFactoryCallbacks_ptr callbacks,
                                      TermFactoryCallbacksUserData_ptr user_data);
} SatSolver;

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/
/*!
  \methodof SatSolver
  \todo
*/
void sat_solver_init(SatSolver_ptr self,
                     const NuSMVEnv_ptr env,
                     const char* name);

/*!
  \methodof SatSolver
  \todo
*/
void sat_solver_deinit(SatSolver_ptr self);

/* pure virtual functions */
/*!
  \methodof SatSolver
  \todo
*/
void sat_solver_add(const SatSolver_ptr self,
                    const Be_Cnf_ptr cnfClause,
                    SatSolverGroup group);

/*!
  \methodof SatSolver
  \todo
*/
void sat_solver_set_polarity(const SatSolver_ptr self,
                             const Be_Cnf_ptr cnfClause,
                             int polarity,
                             SatSolverGroup group);

/*!
  \methodof SatSolver
  \todo
*/
void sat_solver_set_preferred_variables(const SatSolver_ptr self,
                                  const Slist_ptr cnfVars);

/*!
  \methodof SatSolver
  \todo
*/
void sat_solver_clear_preferred_variables(const SatSolver_ptr self);

/*!
  \methodof SatSolver
  \todo
*/
SatSolverResult sat_solver_solve_all_groups(const SatSolver_ptr self);
/*!
  \methodof SatSolver
  \todo
*/
SatSolverResult sat_solver_solve_all_groups_assume(const SatSolver_ptr self,
                                                   Slist_ptr assumptions);

/*!
  \methodof SatSolver
  \todo
*/
Slist_ptr sat_solver_make_model(const SatSolver_ptr self);

Slist_ptr sat_solver_get_conflicts(const SatSolver_ptr);

/* polarity mode */
/*!
  \methodof SatSolver
  \todo
*/
void sat_solver_set_random_mode(SatSolver_ptr self, double seed);
/*!
  \methodof SatSolver
  \todo
*/
void sat_solver_set_polarity_mode(SatSolver_ptr self, int mode);
/*!
  \methodof SatSolver
  \todo
*/
int  sat_solver_get_polarity_mode(const SatSolver_ptr self);

void sat_solver_RemoveFromList(lsList list, const lsGeneric element);
/*!
  \methodof SatSolver
  \todo
*/
int sat_solver_get_cnf_var(const SatSolver_ptr self, int lit);

/**AutomaticEnd***************************************************************/

#endif /* __NUSMV_CORE_SAT_SAT_SOLVER_PRIVATE_H__ */
