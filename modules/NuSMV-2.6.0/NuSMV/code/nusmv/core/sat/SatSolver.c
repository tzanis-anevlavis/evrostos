/**CFsaile***********************************************************************

  FileName    [SatSolver.c]

  PackageName [SatSolver]

  Synopsis    [Routines related to SatSolver object.]

  Description [ This file contains the definition of \"SatSolver\" class.]

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
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
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
#include "nusmv/core/sat/satInt.h" /* just for 'options' */
#include "nusmv/core/sat/SatSolver_private.h"
#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void sat_solver_finalize(Object_ptr object, void *dummy);

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of external functions                                          */
/*---------------------------------------------------------------------------*/

void SatSolver_destroy(SatSolver_ptr self)
{
  const NuSMVEnv_ptr env = ENV_OBJECT(self)->environment;
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  nusmv_assert(SAT_SOLVER(NULL) != self);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Destroying a SAT solver instance '%s'\n",
            SatSolver_get_name(self));
  }

  Object_destroy(OBJECT(self), (char*) NULL);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Done\n");
  }
}

SatSolverGroup
SatSolver_get_permanent_group(const SatSolver_ptr self)
{
  SAT_SOLVER_CHECK_INSTANCE(self);

  /* the first element in the list of existing group is always
     the permanent group */
  return (SatSolverGroup) Oiter_element(Olist_first(self->existingGroups));
}

void
SatSolver_add(const SatSolver_ptr self, const Be_Cnf_ptr cnfProb,
              SatSolverGroup group)
{
  SAT_SOLVER_CHECK_INSTANCE(self);
  /* the formula is a constant => do nothing (see SatSolver_set_polarity) */
  if (Be_Cnf_GetFormulaLiteral(cnfProb) == INT_MAX) return;

  self->add(self, cnfProb, group);
}

void
SatSolver_set_polarity(const SatSolver_ptr self,
                       const Be_Cnf_ptr cnfProb,
                       int polarity,
                       SatSolverGroup group)
{
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  SAT_SOLVER_CHECK_INSTANCE(self);

  env = ENV_OBJECT(self)->environment;
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  nusmv_assert( (-1 == polarity) || (1 == polarity) );
  nusmv_assert(Olist_contains(self->existingGroups,
                              (void*)group));

  /* special case: the formula is a constant */
  if (Be_Cnf_GetFormulaLiteral(cnfProb) == INT_MAX) {
    Slist_ptr clauses = Be_Cnf_GetClausesList(cnfProb);
    int length = Slist_get_size(clauses);
    int theConstant;

    /* cehck whether the constant value is true or false (see Be_Cnf_ptr) */
    if (0 == length) theConstant = 1;
    else {
      /* the list must be a list with one empty clause */
      int * clause = (int *)NULL;
      nusmv_assert(1 == length);
      clause = (int*) Slist_top(clauses);
      nusmv_assert((int *)NULL != clause);
      nusmv_assert(0 == clause[0]);

      theConstant = -1;
    }
    theConstant *= polarity; /* consider the polarity */
    /* insert the formula */
    if (1 == theConstant) {
      if (opt_verbose_level_gt(opts, 0)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger,
                "The true constant has been added to a solver\n");
      }
      /* the formula is true => do nothing */
    }
    else {
      if (opt_verbose_level_gt(opts, 0)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger,
                "The false constant has been added to a solver\n");
      }
      /* the formula is false. Remember this group as unsatisfiable */
      if (!self->interpolation) {
        if (! Olist_contains(self->unsatisfiableGroups, (void*)group)) {
          Olist_prepend(self->unsatisfiableGroups, (void*)group);
        }
      } else {
        self->set_polarity(self, cnfProb, polarity, group);
      }
    }
    return;
  } /* if INT_MAX */

  self->set_polarity(self, cnfProb, polarity, group);
}

void SatSolver_set_preferred_variables(const SatSolver_ptr self,
                                       const Slist_ptr cnfVars)
{
  SAT_SOLVER_CHECK_INSTANCE(self);

  self->set_preferred_variables(self, cnfVars);
}

void SatSolver_clear_preferred_variables(const SatSolver_ptr self)
{
  SAT_SOLVER_CHECK_INSTANCE(self);

  self->clear_preferred_variables(self);
}

SatSolverResult SatSolver_solve_all_groups(const SatSolver_ptr self)
{
  SatSolverResult result;
  const NuSMVEnv_ptr env = ENV_OBJECT(self)->environment;
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  SAT_SOLVER_CHECK_INSTANCE(self);

  /* destroy the model of previous solving */
  if ((Slist_ptr)NULL != self->model) {
    Slist_destroy(self->model);
  }
  self->model = (Slist_ptr)NULL;

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Invoking solver '%s'...\n",
            SatSolver_get_name(self));
  }

  self->solvingTime = util_cpu_time();

  /* we have unsatisfiable formulas in some groups */
  if (0 != Olist_get_size(self->unsatisfiableGroups)) {
    result = SAT_SOLVER_UNSATISFIABLE_PROBLEM;
  }
  else result = self->solve_all_groups(self);

  self->solvingTime = util_cpu_time() - self->solvingTime;

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Solver '%s' returned after %f secs \n",
            SatSolver_get_name(self),
            SatSolver_get_last_solving_time(self)/1000.0);
  }

  return result;
}

SatSolverResult SatSolver_solve_all_groups_assume(const SatSolver_ptr self,
                                                  Slist_ptr assumptions)
{
  SatSolverResult result;
  const NuSMVEnv_ptr env = ENV_OBJECT(self)->environment;
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  SAT_SOLVER_CHECK_INSTANCE(self);

  /* destroy the model of previous solving */
  Slist_destroy(self->model);
  self->model = (Slist_ptr)NULL;

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Invoking solver '%s'...\n",
            SatSolver_get_name(self));
  }

  self->solvingTime = util_cpu_time();

  result = self->solve_all_groups_assume(self, assumptions);

  self->solvingTime = util_cpu_time() - self->solvingTime;

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Solver '%s' returned after %f secs \n",
            SatSolver_get_name(self),
            SatSolver_get_last_solving_time(self)/1000.0);
  }

  return result;
}

Slist_ptr SatSolver_get_conflicts(const SatSolver_ptr self)
{
  SAT_SOLVER_CHECK_INSTANCE(self);

  if ((Slist_ptr)NULL == self->conflicts) {
    self->conflicts = self->get_conflicts(self);
  }

  return self->conflicts;
}

Slist_ptr SatSolver_get_model(const SatSolver_ptr self)
{
  SAT_SOLVER_CHECK_INSTANCE(self);

  if ((Slist_ptr)NULL == self->model) {
    self->model = self->make_model(self);
  }

  return self->model;
}

int SatSolver_get_cnf_var(const SatSolver_ptr self, int var)
{
  SAT_SOLVER_CHECK_INSTANCE(self);
  return self->get_cnf_var(self, var);
}

void SatSolver_set_random_mode(SatSolver_ptr self, double seed)
{
  SAT_SOLVER_CHECK_INSTANCE(self);
  self->set_random_mode(self, seed);
}

void SatSolver_set_polarity_mode(SatSolver_ptr self, int mode)
{
  SAT_SOLVER_CHECK_INSTANCE(self);
  self->set_polarity_mode(self, mode);
}

int SatSolver_get_polarity_mode(const SatSolver_ptr self)
{
  SAT_SOLVER_CHECK_INSTANCE(self);
  return self->get_polarity_mode(self);
}

const char* SatSolver_get_name(const SatSolver_ptr self)
{
  SAT_SOLVER_CHECK_INSTANCE(self);
  return self->name;
}

long SatSolver_get_last_solving_time(const SatSolver_ptr self)
{
  SAT_SOLVER_CHECK_INSTANCE(self);
  return self->solvingTime;
}

SatSolverItpGroup SatSolver_curr_itp_group(const SatSolver_ptr self)
{
  SAT_SOLVER_CHECK_INSTANCE(self);
  return self->curr_itp_group(self);
}

SatSolverItpGroup SatSolver_new_itp_group(const SatSolver_ptr self)
{
  SAT_SOLVER_CHECK_INSTANCE(self);
  self->interpolation = true;
  return self->new_itp_group(self);
}

Term SatSolver_extract_interpolant(const SatSolver_ptr self,
                                   int nof_ga_groups,
                                   SatSolverItpGroup* ga_groups,
                                   TermFactoryCallbacks_ptr callbacks,
                                   TermFactoryCallbacksUserData_ptr user_data)
{
  SAT_SOLVER_CHECK_INSTANCE(self);
  return self->extract_interpolant(self, nof_ga_groups,
                                   ga_groups, callbacks, user_data);
}

/* ---------------------------------------------------------------------- */
/* Private Methods                                                        */
/* ---------------------------------------------------------------------- */

/*!
  \brief Pure virtual function, adds a formula to a group

  It is a pure virtual function and SatSolver is an abstract
  base class. Every derived class must ovewrwrite this function.

  \sa SatSolver_add
*/

void sat_solver_add(const SatSolver_ptr self,
                    const Be_Cnf_ptr cnfProb,
                    SatSolverGroup group)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
}

/*!
  \brief Pure virtual function, sets the polarity of a formula

  It is a pure virtual function and SatSolver is an abstract
  base class. Every derived class must ovewrwrite this function.

  \sa SatSolver_set_polarity
*/

void sat_solver_set_polarity(const SatSolver_ptr self,
                             const Be_Cnf_ptr cnfProb,
                             int polarity,
                             SatSolverGroup group)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
}

/*!
  \brief Pure virtual function, tries to solve formulas in the group and
  the permanent group

  It is a pure virtual function and SatSolver is an abstract
  base class. Every derived class must ovewrwrite this function.
*/

SatSolverResult sat_solver_solve_all_groups(const SatSolver_ptr self)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
  return SAT_SOLVER_INTERNAL_ERROR;
}


/*!
  \brief Pure virtual function, tries to solve formulas in the group and
  the permanent group under assumption

  It is a pure virtual function and SatSolver is an abstract
  base class. Every derived class must ovewrwrite this function.
*/

SatSolverResult sat_solver_solve_all_groups_assume(const SatSolver_ptr self,
                                                   Slist_ptr assumption)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
  return SAT_SOLVER_INTERNAL_ERROR;
}


/*!
  \brief Pure virtual function, creates a model for last successful
  solving

  It is a pure virtual function and SatSolver is an abstract
  base class. Every derived class must ovewrwrite this function.
  It is an error if the last solving was unsuccessful.
*/

Slist_ptr sat_solver_make_model(const SatSolver_ptr self)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
  return (Slist_ptr)NULL;
}

/*!
  \brief Pure virtual function, get CNF literal corresponding to given
  internal Minisat literal


*/

int sat_solver_get_cnf_var(const SatSolver_ptr self, int lit)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
  return 0;
}

/*!
  \brief Pure virtual function, returns the conflicts after
  a previous call to solve udner assumption

  It is a pure virtual function and SatSolver is an abstract
  base class. Every derived class must ovewrwrite this function.
  It is an error if the last solving was unsuccessful.
*/

Slist_ptr sat_solver_get_conflicts(const SatSolver_ptr self)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
  return (Slist_ptr)NULL;
}


/*!
  \brief Pure virtual function, sets random polarity mode if seed is
  not zero, otherwise sets default non-random polarity mode.

  It is a pure virtual function and SatSolver is an abstract
  base class. Every derived class must ovewrwrite this function.
  It is an error if the last solving was unsuccessful.
*/

void sat_solver_set_random_mode(SatSolver_ptr self, double seed)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
}


/*!
  \brief Pure virtual function, sets polarity mode accordingly to the
  passed value.

  It is a pure virtual function and SatSolver is an abstract
  base class. Every derived class must ovewrwrite this function.
  It is an error if the last solving was unsuccessful.
*/

void sat_solver_set_polarity_mode(SatSolver_ptr self, int mode)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
}

/*!
  \brief Pure virtual function, returns currently set polarity mode.

  It is a pure virtual function and SatSolver is an abstract
  base class. Every derived class must ovewrwrite this function.
  It is an error if the last solving was unsuccessful.
*/

int sat_solver_get_polarity_mode(const SatSolver_ptr self)
{
  error_unreachable_code(); /* Pure Virtual Member Function */
  return -1;
}


/*!
  \brief

  Pure virtual function. This must be refined by derived classes.
*/

static SatSolverItpGroup sat_solver_curr_itp_group(SatSolver_ptr self)
{
  error_unreachable_code();
}


/*!
  \brief

  Pure virtual function. This must be refined by derived classes.
*/

static SatSolverItpGroup sat_solver_new_itp_group(SatSolver_ptr self)
{
  error_unreachable_code();
}


/*!
  \brief

  Pure virtual function. This must be refined by derived classes.
*/

static Term sat_solver_extract_interpolant(SatSolver_ptr self, int nof_ga_groups,
                                           SatSolverItpGroup* ga_groups, TermFactoryCallbacks_ptr callbacks,
                                           TermFactoryCallbacksUserData_ptr user_data)
{
  error_unreachable_code();
}


/*!
  \brief removes an element from the list

  If there is no such element in the list => do nothing
*/

void sat_solver_RemoveFromList(lsList list, const lsGeneric element)
{
  lsGen gen = lsStart(list);
  lsGeneric data;
  while (lsNext(gen, &data, LS_NH) == LS_OK) {
    if (element == data) { /* delete the item */
      lsDelBefore(gen, &data);
      nusmv_assert(element == data);
      lsFinish(gen);
      return;
    }
  }
  lsFinish(gen);
  return ;
}

/*---------------------------------------------------------------------------*/
/* Initializer, De-initializer, Finalizer                                    */
/*---------------------------------------------------------------------------*/

/*!
  \brief This function initializes the SatSolver class.


*/

void sat_solver_init(SatSolver_ptr self, const NuSMVEnv_ptr env, const char* name)
{
  const char* defName;
  SAT_SOLVER_CHECK_INSTANCE(self);

  env_object_init(ENV_OBJECT(self), env);

  OVERRIDE(Object, finalize) = sat_solver_finalize;
  OVERRIDE(SatSolver, add) = sat_solver_add;
  OVERRIDE(SatSolver, set_polarity) = sat_solver_set_polarity;
  OVERRIDE(SatSolver, solve_all_groups) = sat_solver_solve_all_groups;
  OVERRIDE(SatSolver, solve_all_groups_assume) =
    sat_solver_solve_all_groups_assume;
  OVERRIDE(SatSolver, make_model) = sat_solver_make_model;
  OVERRIDE(SatSolver, get_cnf_var) = sat_solver_get_cnf_var;
  OVERRIDE(SatSolver, get_conflicts) = sat_solver_get_conflicts;
  OVERRIDE(SatSolver, set_random_mode) = sat_solver_set_random_mode;
  OVERRIDE(SatSolver, set_polarity_mode) = sat_solver_set_polarity_mode;
  OVERRIDE(SatSolver, get_polarity_mode) = sat_solver_get_polarity_mode;
  OVERRIDE(SatSolver, curr_itp_group) = sat_solver_curr_itp_group;
  OVERRIDE(SatSolver, new_itp_group) = sat_solver_new_itp_group;
  OVERRIDE(SatSolver, extract_interpolant) = sat_solver_extract_interpolant;

  /* inits members: */
  defName = ((char*)NULL) != name ? name : "Unknown";
  self->name = ALLOC(char, strlen(defName)+1);
  nusmv_assert(self->name != (char*)NULL);
  strcpy(self->name, defName);

  self->solvingTime = 0;
  self->model = (Slist_ptr)NULL;
  self->conflicts = (Slist_ptr)NULL;
  self->existingGroups = Olist_create();
  /* insert the permanent group with ID '-1',
     !!! In a concrete class you should remove the inserted permanent group
     and insert a real one */
  Olist_append(self->existingGroups, (void*)-1);
  self->unsatisfiableGroups = Olist_create();

  self->interpolation = false;
}

/*!
  \brief This function de-initializes the SatSolver class.


*/

void sat_solver_deinit(SatSolver_ptr self)
{
  SAT_SOLVER_CHECK_INSTANCE(self);

  FREE(self->name);

  if((Slist_ptr)NULL != self->model) {
    Slist_destroy(self->model);
  }

  if((Slist_ptr)NULL != self->conflicts) {
    Slist_destroy(self->conflicts);
  }

  Olist_destroy(self->existingGroups);
  Olist_destroy(self->unsatisfiableGroups);

  env_object_deinit(ENV_OBJECT(self));
}

/*!
  \brief Finalize method of SatSolver class.

  Pure virtual function. This must be refined by derived classes.
*/
static void sat_solver_finalize(Object_ptr object, void* dummy)
{
  SatSolver_ptr self = SAT_SOLVER(object);

  sat_solver_deinit(self);
  error_unreachable_code();
}
