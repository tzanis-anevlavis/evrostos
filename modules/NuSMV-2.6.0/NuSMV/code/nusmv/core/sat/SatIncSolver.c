/**CFsaile***********************************************************************

  FileName    [SatIncSolver.c]

  PackageName [SatIncSolver]

  Synopsis    [Routines related to SatIncSolver object.]

  Description [ This file contains the definition of \"SatIncSolver\" class.]

  SeeAlso     []

  Author      [Andrei Tchaltsev]

  Copyright   [
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

  To contact the NuSMV development board, email to <nusmv@fbk.eu>. ]

******************************************************************************/

#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/sat/satInt.h" /* just for 'options' */
#include "nusmv/core/sat/SatIncSolver_private.h"
/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void sat_inc_solver_finalize(Object_ptr object, void *dummy);

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of external functions                                          */
/*---------------------------------------------------------------------------*/

void SatIncSolver_destroy(SatIncSolver_ptr self)
{
  SatSolver_destroy(SAT_SOLVER(self));
}

SatSolverGroup
SatIncSolver_create_group(const SatIncSolver_ptr self)
{
  SAT_INC_SOLVER_CHECK_INSTANCE(self);
  return self->create_group(self);
}

void
SatIncSolver_destroy_group(const SatIncSolver_ptr self, SatSolverGroup group)
{
  SAT_INC_SOLVER_CHECK_INSTANCE(self);
  self->destroy_group(self, group);
}

void
SatIncSolver_move_to_permanent_and_destroy_group(const SatIncSolver_ptr self,
                                                 SatSolverGroup group)
{
  SAT_INC_SOLVER_CHECK_INSTANCE(self);
  self->move_to_permanent_and_destroy_group(self, group);
}

SatSolverResult
SatIncSolver_solve_groups(const SatIncSolver_ptr self, const Olist_ptr groups)
{
  SatSolverResult result;
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  SAT_INC_SOLVER_CHECK_INSTANCE(self);

  env = ENV_OBJECT(self)->environment;
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  Slist_destroy(SAT_SOLVER(self)->model); /* destroy the model of
                                             previous solving */
  SAT_SOLVER(self)->model = (Slist_ptr)NULL;

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Invoking solver '%s'...\n",
            SatSolver_get_name(SAT_SOLVER(self)));
  }

  SAT_SOLVER(self)->solvingTime = util_cpu_time();
  result = self->solve_groups(self, groups);
  SAT_SOLVER(self)->solvingTime = util_cpu_time() - SAT_SOLVER(self)->solvingTime;

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Solver '%s' returned after %f secs \n",
            SatSolver_get_name(SAT_SOLVER(self)),
            SatSolver_get_last_solving_time(SAT_SOLVER(self))/1000.0);
  }

  return result;
}

SatSolverResult
SatIncSolver_solve_without_groups(const SatIncSolver_ptr self,
                                  const Olist_ptr groups)
{
  SatSolverResult result;
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  SAT_INC_SOLVER_CHECK_INSTANCE(self);

  env = ENV_OBJECT(self)->environment;
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if ((Slist_ptr)NULL != SAT_SOLVER(self)->model) {
    Slist_destroy(SAT_SOLVER(self)->model); /* destroy the model of
                                               previous solving */
  }

  SAT_SOLVER(self)->model = (Slist_ptr)NULL;

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Invoking solver '%s'...\n",
            SatSolver_get_name(SAT_SOLVER(self)));
  }

  SAT_SOLVER(self)->solvingTime = util_cpu_time();
  result = self->solve_without_groups(self, groups);
  SAT_SOLVER(self)->solvingTime = util_cpu_time() - SAT_SOLVER(self)->solvingTime;

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Solver '%s' returned after %f secs \n",
            SatSolver_get_name(SAT_SOLVER(self)),
            SatSolver_get_last_solving_time(SAT_SOLVER(self))/1000.0);
  }

  return result;
}


/* ---------------------------------------------------------------------- */
/* Private Methods                                                        */
/* ---------------------------------------------------------------------- */

/*!
  \brief Pure virtual function, create a new group

  It is a pure virtual function and SatIncSolver is an abstract
  base class. Every derived class must ovewrwrite this function.

  \sa SatIncSolver_add
*/

SatSolverGroup
sat_inc_solver_create_group (const SatIncSolver_ptr self)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
  return -1;
}

/*!
  \brief Pure virtual function, destroys existing group

  It is a pure virtual function and SatIncSolver is an abstract
  base class. Every derived class must ovewrwrite this function.

  \sa SatIncSolver_add
*/

void
sat_inc_solver_destroy_group (const SatIncSolver_ptr self,
                              SatSolverGroup group)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
}

/*!
  \brief Pure virtual function, moves all formulas from
  a given group into the permanent one, and then destroys the given group

  It is a pure virtual function and SatIncSolver is an abstract
  base class. Every derived class must ovewrwrite this function.

  \sa SatIncSolver_add
*/

void
sat_inc_solver_move_to_permanent_and_destroy_group (const SatIncSolver_ptr self,
                                                    SatSolverGroup group)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
}

/*!
  \brief Pure virtual function, tries to solve formulas from
  the groups in the list. the permanent group is automatically added to
  the list

  It is a pure virtual function and SatIncSolver is an abstract
  base class. Every derived class must ovewrwrite this function.

  \sa SatIncSolver_add
*/

SatSolverResult
sat_inc_solver_solve_groups (const SatIncSolver_ptr self, const Olist_ptr groups)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
  return SAT_SOLVER_INTERNAL_ERROR;
}

/*!
  \brief Pure virtual function, tries to solve formulas from
  the groups belonging to the solver except the groups in the list.
  the permanent group must not be in the list

  It is a pure virtual function and SatIncSolver is an abstract
  base class. Every derived class must ovewrwrite this function.

  \sa SatIncSolver_add
*/

SatSolverResult
sat_inc_solver_solve_without_groups (const SatIncSolver_ptr self,
                                     const Olist_ptr groups)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
  return SAT_SOLVER_INTERNAL_ERROR;
}

/* ---------------------------------------------------------------------- */
/* Initializer and De-initializer                                         */
/* ---------------------------------------------------------------------- */


/*!
  \brief This function initializes the SatIncSolver class.

  
*/

void sat_inc_solver_init(SatIncSolver_ptr self,
                         const NuSMVEnv_ptr env,
                         const char* name)
{
  SAT_INC_SOLVER_CHECK_INSTANCE(self);

  sat_solver_init(SAT_SOLVER(self), env, name);

  OVERRIDE(Object, finalize) = sat_inc_solver_finalize;
  /* use default SatSolver pure-virtual functions */
  OVERRIDE(SatIncSolver, create_group) = sat_inc_solver_create_group;
  OVERRIDE(SatIncSolver, destroy_group) = sat_inc_solver_destroy_group;
  OVERRIDE(SatIncSolver, move_to_permanent_and_destroy_group)
    = sat_inc_solver_move_to_permanent_and_destroy_group;
  OVERRIDE(SatIncSolver, solve_groups) = sat_inc_solver_solve_groups;
  OVERRIDE(SatIncSolver, solve_groups) = sat_inc_solver_solve_groups;
  OVERRIDE(SatIncSolver, solve_without_groups)
    = sat_inc_solver_solve_without_groups;

  /* inits members: */
}

/*!
  \brief This function de-initializes the SatIncSolver class.

  
*/

void sat_inc_solver_deinit(SatIncSolver_ptr self)
{
  SAT_INC_SOLVER_CHECK_INSTANCE(self);

  sat_solver_deinit(SAT_SOLVER(self));
}

/*---------------------------------------------------------------------------*/
/* Static functions                                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief Finalize method of SatIncSolver class.

  Pure virtual function. This must be refined by derived classes.
*/
static void sat_inc_solver_finalize(Object_ptr object, void* dummy)
{
  SatIncSolver_ptr self = SAT_INC_SOLVER(object);

  SAT_INC_SOLVER_CHECK_INSTANCE(self);

  sat_inc_solver_deinit(self);
  error_unreachable_code();
}

