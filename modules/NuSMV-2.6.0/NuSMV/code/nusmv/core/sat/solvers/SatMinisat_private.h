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
  \brief The private interface of class SatMinisat

  Private definition to be used by derived classes

*/

#ifndef __NUSMV_CORE_SAT_SOLVERS_SAT_MINISAT_PRIVATE_H__
#define __NUSMV_CORE_SAT_SOLVERS_SAT_MINISAT_PRIVATE_H__

#include "nusmv/core/sat/solvers/SatMinisat.h"
#include "nusmv/core/sat/solvers/satMiniSatIfc.h"

#include "nusmv/core/sat/SatIncSolver_private.h"
#include "nusmv/core/utils/assoc.h"

#include "nusmv/core/utils/Stack.h"

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/*!
  \brief SatMinisat Class

   This class defines a prototype for a generic SatMinisat. This
  class is virtual and must be specialized. 
*/

typedef struct SatMinisat_TAG
{
  INHERITS_FROM(SatIncSolver);

  MiniSat_ptr minisatSolver; /* actual instance of minisat */
  /* All input variables are represented by the  internal ones inside the 
     SatMinisat. Bellow two hash table perform the convertion in both ways */
  hash_ptr cnfVar2minisatVar;/* converts CNF variable to internal variable */
  hash_ptr minisatVar2cnfVar;/* converts internal variable into CNF variable */

  /* contains set of conflicting assumptions after using
     SatMinisat_solve_permanent_group_assume */
  Slist_ptr conflict;

  /* A clause and its current maximum length. This was added here in
     order to replace a statically allocated, fixed-size array and
     length indicator in sat_minisat_add. */
  int* minisatClause;

  unsigned int minisatClauseSize;

  Stack_ptr minisat_itp_groups;
} SatMinisat;

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/
/*!
  \methodof SatMinisat
  \todo
*/
void sat_minisat_init(SatMinisat_ptr self, const NuSMVEnv_ptr env,
                      const char* name, boolean enable_proof_logging);
/*!
  \methodof SatMinisat
  \todo
*/
void sat_minisat_deinit(SatMinisat_ptr self);

/*!
  \methodof SatMinisat
  \todo
*/
int sat_minisat_cnfLiteral2minisatLiteral(SatMinisat_ptr self,
                                          int cnfLitaral);
/*!
  \methodof SatMinisat
  \todo
*/
int sat_minisat_minisatLiteral2cnfLiteral(SatMinisat_ptr self,
                                          int minisatLiteral);

/* virtual function from SatSolver */
/*!
  \methodof SatMinisat
  \todo
*/
void sat_minisat_add(const SatSolver_ptr self,
                     const Be_Cnf_ptr cnfProb,
                     SatSolverGroup group);

/*!
  \methodof SatMinisat
  \todo
*/
void sat_minisat_set_polarity(const SatSolver_ptr self,
                              const Be_Cnf_ptr cnfProb,
                              int polarity,
                              SatSolverGroup group);

/*!
  \methodof SatMinisat
  \todo
*/
void sat_minisat_set_preferred_variables(const SatSolver_ptr self,
                                         const Slist_ptr cnfVars);

/*!
  \methodof SatMinisat
  \todo
*/
void sat_minisat_clear_preferred_variables(const SatSolver_ptr self);

/*!
  \methodof SatMinisat
  \todo
*/
SatSolverResult sat_minisat_solve_all_groups(const SatSolver_ptr self);

/*!
  \methodof SatMinisat
  \todo
*/
SatSolverResult sat_minisat_solve_permanent_group_assume(const SatSolver_ptr self, Slist_ptr assumption);

Slist_ptr sat_minisat_get_conflicts(const SatSolver_ptr);

/*!
  \methodof SatMinisat
  \todo
*/
Slist_ptr sat_minisat_make_model(const SatSolver_ptr self);

/* virtual functions from SatIncSolver */
/*!
  \methodof SatMinisat
  \todo
*/
SatSolverGroup 
sat_minisat_create_group(const SatIncSolver_ptr self);

/*!
  \methodof SatMinisat
  \todo
*/
void
sat_minisat_destroy_group(const SatIncSolver_ptr self,
                          SatSolverGroup group);

/*!
  \methodof SatMinisat
  \todo
*/
void
sat_minisat_move_to_permanent_and_destroy_group(const SatIncSolver_ptr self,
SatSolverGroup group);
/*!
  \methodof SatMinisat
  \todo
*/
SatSolverResult
sat_minisat_solve_groups(const SatIncSolver_ptr self,
                         const Olist_ptr groups);

/*!
  \methodof SatMinisat
  \todo
*/
SatSolverResult
sat_minisat_solve_without_groups(const SatIncSolver_ptr self,
                                 const Olist_ptr groups);

/* the assumptions/conflict interface of MiniSat */
/*!
  \methodof SatMinisat
  \todo
*/
Slist_ptr sat_minisat_make_conflicts(const SatMinisat_ptr self);

/*!
  \methodof SatMinisat
  \todo
*/
int* sat_minisat_get_minisatClause(const SatMinisat_ptr self);
/*!
  \methodof SatMinisat
  \todo
*/
int sat_minisat_get_minisatClauseSize(const SatMinisat_ptr self);
/*!
  \methodof SatMinisat
  \todo
*/
void sat_minisat_enlarge_minisatClause(const SatMinisat_ptr self,
                                       unsigned int minSize);

/* polarity mode */
/*!
  \methodof SatMinisat
  \todo
*/
void sat_minisat_set_random_mode(SatSolver_ptr self, double seed);
/*!
  \methodof SatMinisat
  \todo
*/
void sat_minisat_set_polarity_mode(SatSolver_ptr self, int mode);
/*!
  \methodof SatMinisat
  \todo
*/
int  sat_minisat_get_polarity_mode(const SatSolver_ptr self);

int sat_minisat_get_cnf_var(const SatSolver_ptr solver, int var);

/**AutomaticEnd***************************************************************/

#endif /* __NUSMV_CORE_SAT_SOLVERS_SAT_MINISAT_PRIVATE_H__ */
