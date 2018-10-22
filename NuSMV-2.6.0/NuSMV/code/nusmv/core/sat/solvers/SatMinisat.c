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
  \author Andrei Tchaltsev, Marco Roveri, Marco Pensallorto
  \brief Routines related to SatMinisat object.

   This file contains the definition of \"SatMinisat\" class.
  The solver contains its own coding of varibales, so input variables may
  by in any range from 1 .. INT_MAX, with possible holes in the range.

  Group Control:
   To control groups, every group has its ID, which is an usual internal
   variable. If a formula is added to a permanent group, then literals are just
   converted into the internal literals and the clauses are permamently
   added to the solver.
   If a formula is added to non-permanent group, then after convertion of
   literals, every clause in the  group will additionally obtain one
   literal which is just group id, and then the clauses are added permanently
   to solver.
   Then if a group is turn on, then just its negated ID is added temporary to
   the solver. If we want to turn the group off, the just its ID
   is added temporary to the solver.


*/


#if HAVE_CONFIG_H
#include "nusmv-config.h"
#endif

#include "nusmv/core/sat/solvers/SatMinisat_private.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/Sset.h"
/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void sat_minisat_finalize(Object_ptr object, void *dummy);
static int _get_clause_size(const int * clause);

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of external functions                                          */
/*---------------------------------------------------------------------------*/

SatMinisat_ptr SatMinisat_create(const NuSMVEnv_ptr env, const char* name,
                                 boolean enable_proof_logging)
{
  SatMinisat_ptr self = ALLOC(SatMinisat, 1);

  SAT_MINISAT_CHECK_INSTANCE(self);

  sat_minisat_init(self, env, name, enable_proof_logging);
  return self;
}

void SatMinisat_destroy (SatMinisat_ptr self)
{
  SatSolver_destroy(SAT_SOLVER(self));
}

/* ---------------------------------------------------------------------- */
/* Private Methods                                                        */
/* ---------------------------------------------------------------------- */

/*!
  \brief Convert a cnf literal into an internal literal used by minisat

  The literal may not be 0 (because 0 cannot have sign).
  First, the function obtains the cnf variable (removes the sign),
  obtains associated internal var through hash table(creates if necessary
  an internal variable)
  and then converts it in minisat literal (just adjust the sign).
  If necessary a new minisat variable is created.

  \sa sat_minisat_minisatLiteral2cnfLiteral
*/

int sat_minisat_cnfLiteral2minisatLiteral(SatMinisat_ptr self, int cnfLiteral)
{
  int cnfVar = abs(cnfLiteral);
  int minisatVar;

  SAT_MINISAT_CHECK_INSTANCE(self);
  nusmv_assert(cnfVar > 0);

  minisatVar = NODE_TO_INT(find_assoc(self->cnfVar2minisatVar,
                                      NODE_FROM_INT(cnfVar)));

  if (0 == minisatVar) {
    /* create a new internal var and associate with cnf */
    minisatVar = MiniSat_New_Variable(self->minisatSolver);

    insert_assoc(self->cnfVar2minisatVar,
                 NODE_FROM_INT(cnfVar), NODE_FROM_INT(minisatVar));

    insert_assoc(self->minisatVar2cnfVar,
                 NODE_FROM_INT(minisatVar), NODE_FROM_INT(cnfVar));
  }

  return cnfLiteral > 0 ? minisatVar : - minisatVar;
}

/*!
  \brief Convert an internal minisat literal into a cnf literal

  The variable in the literal has to be created by
   sat_minisat_cnfLiteral2minisatLiteral only.
  First, the function obtains the minisat variable from the literal,
  obtains associated cnf variable (there must already be the association),
  and then converts it in cnf literal (adjust the sign)

  \sa sat_minisat_cnfLiteral2minisatLiteral
*/

int sat_minisat_minisatLiteral2cnfLiteral(SatMinisat_ptr self, int minisatLiteral)
{
  int minisatVar = abs(minisatLiteral);
  int cnfVar = NODE_TO_INT(find_assoc(self->minisatVar2cnfVar,
                                      NODE_FROM_INT(minisatVar)));

#if 0
  We cannot check that cnfVar != Nil, since some internal variables
  can be used as group id-s.
  We cannnot check that internal variable is a group id, because
  some groups may be deleted and their id-s are removed from the list
  'existing group'.

  /* cnf var is Nill only if the corresponding internal var represents
     a group id, otherwise is always greater then 0 */
  nusmv_assert( ((int) Nil != cnfVar) ||
                sat_solver_BelongToList(SAT_SOLVER(self)->existingGroups,
                                        (lsGeneric)minisatVar) );
#endif

  return minisatLiteral > 0 ? cnfVar : - cnfVar;
}

/*!
  \brief Adds a clause to the solver database.

  converts all CNF literals into the internal literals,
  adds a group id to every clause (if group is not permament) and then add
  obtained clauses to actual Minisat
*/

void sat_minisat_add(const SatSolver_ptr solver,
                     const Be_Cnf_ptr cnfProb,
                     SatSolverGroup group)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);

  int * clause = (int *)NULL;
  Siter genClause;
  int* minisatClause;

  /* just for efficiency */
  const int groupIsNotPermanent =
    SatSolver_get_permanent_group(SAT_SOLVER(self)) != group;


  SAT_MINISAT_CHECK_INSTANCE(self);

  minisatClause = sat_minisat_get_minisatClause(self);

  SLIST_FOREACH (Be_Cnf_GetClausesList(cnfProb), genClause) {
    int literal, i;
    int clause_size;
    int literalNumber = 0;

    clause = (int*) Siter_element(genClause);
    clause_size = _get_clause_size(clause);

    if (sat_minisat_get_minisatClauseSize(self) - 4 <= clause_size) {
      sat_minisat_enlarge_minisatClause(self, clause_size + 5);
      minisatClause = sat_minisat_get_minisatClause(self);
    }
    i = 0;
    while (clause[i] != 0) {
      literal = clause[i];
      minisatClause[literalNumber]
        = sat_minisat_cnfLiteral2minisatLiteral(self, literal);
      ++literalNumber;
      i++;
    }

    if (groupIsNotPermanent) { /* add group id to the clause */
      minisatClause[literalNumber] = group;
      ++literalNumber;
    }
#if NUSMV_HAVE_MINISAT_INTERPOLATION
    /* add to real minisat */
    if (SatSolver_curr_itp_group(solver)) {
      minisatClause[literalNumber] = SatSolver_curr_itp_group(solver);
      ++literalNumber;
    }
#endif
    MiniSat_Add_Clause(self->minisatSolver, minisatClause,
                       literalNumber);
    /* with the new interface of minisat there is not reason to remember
       that an unsatisfiable clause has been added to the solver */

  } /* while() */
}

/*!
  \brief Sets the polarity of the formula.

  Sets the polarity of the formula.
  Polarity 1 means the formula is considered as positive, and -1 means
  the negation of the formula will be solved.
  A unit clause of the literal (with sign equal to polarity)
  corresponding to the given CNF formula is added to the solve.
*/

void sat_minisat_set_polarity(const SatSolver_ptr solver,
                             const Be_Cnf_ptr cnfProb,
                             int polarity,
                             SatSolverGroup group)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);

  int cnfLiteral;
  int minisatLiteral;
  int minisatClause[5]; /* only one or two literals may be in the clause */
  int numlits;

  SAT_MINISAT_CHECK_INSTANCE(self);
  numlits = 0;

  if (Be_Cnf_GetFormulaLiteral(cnfProb) == INT_MAX) {
    /* special case, we have added the constant false */
    nusmv_assert(SAT_SOLVER(self)->interpolation);

    /* check that we are actually adding the constant false
       (see SatSolver.c, SatSolver_set_polarity) */
    nusmv_assert(
      (Slist_get_size(Be_Cnf_GetClausesList(cnfProb)) == 0 ? 1 : -1) *
      polarity != 1);

    if (group == SatSolver_get_permanent_group(SAT_SOLVER(self))) {
      minisatClause[0] = group;
      numlits = 1;
#if NUSMV_HAVE_MINISAT_INTERPOLATION
      if (SatSolver_curr_itp_group(solver)) {
        minisatClause[numlits++] = SatSolver_curr_itp_group(solver);
      }
#endif
      MiniSat_Add_Clause(self->minisatSolver, minisatClause, numlits);
    } else {
      /* add empty clause */
#if NUSMV_HAVE_MINISAT_INTERPOLATION
      if (SatSolver_curr_itp_group(solver)) {
        minisatClause[numlits++] = SatSolver_curr_itp_group(solver);
      }
#endif
      MiniSat_Add_Clause(self->minisatSolver, minisatClause, numlits);
    }

    return;
  }

  cnfLiteral = polarity * Be_Cnf_GetFormulaLiteral(cnfProb);
  minisatLiteral = sat_minisat_cnfLiteral2minisatLiteral ( self,
                                                           cnfLiteral);
  minisatClause[numlits++] = minisatLiteral;

  if ( SatSolver_get_permanent_group(SAT_SOLVER(self)) == group ) {
#if NUSMV_HAVE_MINISAT_INTERPOLATION
      if (SatSolver_curr_itp_group(solver)) {
        minisatClause[numlits++] = SatSolver_curr_itp_group(solver);
      }
#endif
    MiniSat_Add_Clause(self->minisatSolver, minisatClause, numlits);
  }
  else { /* add group id to clause to controle the CNF formula */
    minisatClause[numlits++] = group;
#if NUSMV_HAVE_MINISAT_INTERPOLATION
      if (SatSolver_curr_itp_group(solver)) {
        minisatClause[numlits++] = SatSolver_curr_itp_group(solver);
      }
#endif
    MiniSat_Add_Clause(self->minisatSolver, minisatClause, numlits);
  }
}

/*!
  \brief Sets preferred variables in the solver

  Sets preferred variables in the solver. A preferred variable is
               split upon with priority, with respect to non-preferedd ones.

  \sa SatSolver_clear_preferred_variables
*/

void sat_minisat_set_preferred_variables(const SatSolver_ptr solver,
                                         const Slist_ptr cnfVars)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);

  nusmv_ptrint cnfVar;
  Siter genVar;
  int minisatVar;

  SAT_MINISAT_CHECK_INSTANCE(self);

  SLIST_FOREACH (cnfVars, genVar) {
    cnfVar = (nusmv_ptrint) Siter_element(genVar);
    nusmv_assert(((int) cnfVar) > 0);

    minisatVar = NODE_TO_INT(find_assoc(self->cnfVar2minisatVar,
                                        NODE_FROM_INT((int) cnfVar)));
    /* skip var, if not existing */
    if (0 != minisatVar) {
      MiniSat_Set_Preferred_Variable(self->minisatSolver, minisatVar-1);
    }
  }
}

/*!
  \brief Clears preferred variables in the solver

  Clears preferred variables in the solver. A preferred variable
               is split upon with priority, with respect to non-preferedd
               ones.

  \sa SatSolver_set_preferred_variables
*/

void sat_minisat_clear_preferred_variables(const SatSolver_ptr solver)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);

  SAT_MINISAT_CHECK_INSTANCE(self);

  MiniSat_Clear_Preferred_Variables(self->minisatSolver);
}

/*!
  \brief Tries to solve all added formulas


*/

SatSolverResult sat_minisat_solve_all_groups(const SatSolver_ptr solver)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);
  SAT_MINISAT_CHECK_INSTANCE(self);

  return sat_minisat_solve_groups(SAT_INC_SOLVER(self),
                                  SAT_SOLVER(self)->existingGroups);
}


/*!
  \brief Solves the permanent group under set of assumptions

  Obtain set of conflicting assumptions with
  sat_minisat_get_conflict

  \sa sat_minisat_get_conflict, sat_minisat_make_conflicts
*/

SatSolverResult
sat_minisat_solve_permanent_group_assume(const SatSolver_ptr sol,
                                         const Slist_ptr assumptions)
{
  SatMinisat_ptr self = SAT_MINISAT(sol);

  SatSolver_ptr solver;
  SatSolverGroup permanentGroup;
  int minisatResult;
  int numberOfAssumptions;
  int* lits;
  Siter gen;
  int aAss;

  SAT_MINISAT_CHECK_INSTANCE(self);

  solver = SAT_SOLVER(self);

  /* destroy the model of previous solving */
  Slist_destroy(solver->model);
  solver->model = (Slist_ptr)NULL;

  /* destroy the conflict of previous solving */
  Slist_destroy(self->conflict);
  self->conflict = (Slist_ptr)NULL;

  permanentGroup = SatSolver_get_permanent_group(solver);

  if (Olist_contains(solver->unsatisfiableGroups,
                     (void*)permanentGroup) ) {
    minisatResult = SAT_SOLVER_UNSATISFIABLE_PROBLEM;
    /* no assumptions needed to obtain conflict =>
       remains empty */
  } else {
    numberOfAssumptions = Slist_get_size(assumptions);
    nusmv_assert( numberOfAssumptions>= 0 );
    lits = ALLOC(int, numberOfAssumptions);

    numberOfAssumptions = 0;
    SLIST_FOREACH (assumptions, gen) {
      aAss = PTR_TO_INT(Siter_element(gen));

      /* add assumption to list of assumptions */
      lits[numberOfAssumptions] =
        sat_minisat_cnfLiteral2minisatLiteral(self, aAss);
      ++numberOfAssumptions;
    } /* end of lsForEachItem */

    /* try to solver (MiniSat_Solve will invoke internal simplifyDB) */
    minisatResult = MiniSat_Solve_Assume(self->minisatSolver,
                                         numberOfAssumptions, lits);
    FREE(lits);
  }

  if (1 == minisatResult) {
    return SAT_SOLVER_SATISFIABLE_PROBLEM;
  }
  else
    return SAT_SOLVER_UNSATISFIABLE_PROBLEM;
}

/*!
  \brief Returns set of conflicting assumptions

  Only use with SatMinisat_solve_permanent_group_assume

  \sa sat_minisat_solve_permanent_group_assume,
  sat_minisat_make_conflicts
*/

Slist_ptr sat_minisat_get_conflicts(const SatSolver_ptr solver)
{

  SatMinisat_ptr self = SAT_MINISAT(solver);

  SAT_MINISAT_CHECK_INSTANCE(self);

  if ((Slist_ptr)NULL == self->conflict)
    self->conflict = sat_minisat_make_conflicts(self);

  return self->conflict;
}


/*!
  \brief This function creates a model (in the original CNF variables)

  The previous invocation of SAT_Solve should have been successful
*/

Slist_ptr sat_minisat_make_model (const SatSolver_ptr solver)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);
  int index;
  Slist_ptr model;
  int varNumber;

  SAT_MINISAT_CHECK_INSTANCE(self);
  /* a model is created only if there is no model */
  nusmv_assert((Slist_ptr)NULL == SAT_SOLVER(self)->model);

  model = Slist_create();
  varNumber = MiniSat_Nof_Variables(self->minisatSolver);

  for (index = 1; index <= varNumber; ++index) {
    int cnfLiteral = sat_minisat_minisatLiteral2cnfLiteral(self, index);

    if (cnfLiteral > 0) { /* it is a real variable */
      switch (MiniSat_Get_Value(self->minisatSolver, index)) {

      case 0: /* negative polarity => change the polarity of CNF var */
        cnfLiteral = -cnfLiteral;
      case 1:  /* positive polarity => do nothing */
        /* appends the model: */
        Slist_push(model, PTR_FROM_INT(void*, cnfLiteral));
        break;

      case -1: break; /* does not store unassigned vars */
      default:
        error_unreachable_code(); /* no other values should be provided */
      }
    }
    else {   /* just debugging */
      /*
        We cannot check that cnfVar != Nil, since some internal variables
        can be used as group id-s.
        We cannnot check that internal variable is a group id, because
        some groups may be deleted and their id-s are removed from the list
        'existing group'.
      */
    }
  } /* for() */

  return model;
}

/* MP */
int sat_minisat_get_cnf_var(const SatSolver_ptr solver, int var)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);
  SAT_MINISAT_CHECK_INSTANCE(self);

  return sat_minisat_minisatLiteral2cnfLiteral(self, /*1 + */ var); /* AG */
}

/*!
  \brief Creates a new group and returns its ID

  Adds the group at the END of the existing groups list

  \sa SatIncSolver_destroy_group,
  SatIncSolver_move_to_permanent_and_destroy_group
*/

SatSolverGroup
sat_minisat_create_group(const SatIncSolver_ptr solver)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);
  int newGroup;
  SAT_MINISAT_CHECK_INSTANCE(self);
  newGroup = MiniSat_New_Variable(self->minisatSolver);
  Olist_append(SAT_SOLVER(self)->existingGroups,
               PTR_FROM_INT(void*, newGroup));
  return newGroup;
}


/*!
  \brief Destroy an existing group (which has been returned by
  SatIncSolver_create_group) and all formulas in it.

  Just adds to the solver a unit clause with positive literal
  of a variable with index  equal to group id

  \sa SatIncSolver_create_group
*/

void
sat_minisat_destroy_group(const SatIncSolver_ptr solver, SatSolverGroup group)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);
  int minisatClause[2];


  SAT_MINISAT_CHECK_INSTANCE(self);
  /* it should not be a permanent group */
  nusmv_assert(SatSolver_get_permanent_group(SAT_SOLVER(self)) != group);
  /* the group should exist */
  nusmv_assert(Olist_contains(SAT_SOLVER(self)->existingGroups, (void*)group));

  /* delete the group from the lists */
  Olist_remove(SAT_SOLVER(self)->existingGroups, (void*)group);
  Olist_remove(SAT_SOLVER(self)->unsatisfiableGroups, (void*)group);

  /* add literal corresponding to group id to the solver (to
     make all clauses contaning it true, so useless */
  minisatClause[0] = group;
  MiniSat_Add_Clause(self->minisatSolver, minisatClause, 1);
  /* with new minisat interface it is not necessary to check
     the successfulness of adding a clause */
}

/*!
  \brief Moves all formulas from a group into the permanent group of
  the solver and then destroy the given group.

  just adds  to minisat a unit clause with negative literal
  of a variable with index equal to group id

  \sa SatIncSolver_create_group, SatSolver_get_permanent_group,

*/

void
sat_minisat_move_to_permanent_and_destroy_group(const SatIncSolver_ptr solver,
                                                SatSolverGroup group)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);
  int minisatClause[2];
  SatSolverGroup permamentGroup;


  SAT_MINISAT_CHECK_INSTANCE(self);

  permamentGroup = SatSolver_get_permanent_group(SAT_SOLVER(self));

  /* it should not be a permanent group */
  nusmv_assert( permamentGroup != group);
  /* the group should exist */
  nusmv_assert(Olist_contains(SAT_SOLVER(self)->existingGroups,
                              (void*)group));

  /* if the group is unsatisfiable, make the permanent group unsatisfiable */
  if (Olist_contains(SAT_SOLVER(self)->unsatisfiableGroups, (void*)group) &&
      ! Olist_contains(SAT_SOLVER(self)->unsatisfiableGroups,
                       (void*)permamentGroup) ) {
    Olist_prepend(SAT_SOLVER(self)->unsatisfiableGroups, (void*)permamentGroup);
  }

  /* delete the group from the lists */
  Olist_remove(SAT_SOLVER(self)->existingGroups,
               (void*)group);
  Olist_remove(SAT_SOLVER(self)->unsatisfiableGroups,
               (void*)group);

  /* add negated literal corresponding to group id to the solver (to
     remove the group id literal from all the clauses belonding to the group */
  minisatClause[0] = -group;
  MiniSat_Add_Clause(self->minisatSolver, minisatClause, 1);
  /* with new minisat interface it is not necessary to check
     the successfulness of adding a clause */
  MiniSat_simplifyDB(self->minisatSolver);
}

/*!
  \brief Tries to solve formulas from the groups in the list.

  The permanent group is automatically added to the list.
  Returns a flag whether the solving was successful. If it was successful only
  then SatSolver_get_model may be invoked to obtain the model
*/

SatSolverResult
sat_minisat_solve_groups(const SatIncSolver_ptr solver, const Olist_ptr groups)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);
  SatSolverGroup permanentGroup;
  int minisatResult;
  int* assumptions;
  int numberOfGroups;
  Oiter gen;
  SatSolverGroup aGroup;

  SAT_MINISAT_CHECK_INSTANCE(self);

  permanentGroup = SatSolver_get_permanent_group(SAT_SOLVER(self));

  /* if the permanent group is unsatisfiable => return.
   We check it here because the input list may not contain permanent group */
  if (Olist_contains(SAT_SOLVER(self)->unsatisfiableGroups,
                     (void*)permanentGroup)) {
    return SAT_SOLVER_UNSATISFIABLE_PROBLEM;
  }

  numberOfGroups = Olist_get_size(groups);
  nusmv_assert( numberOfGroups>= 0 );

#if NUSMV_HAVE_MINISAT_INTERPOLATION
  numberOfGroups += Stack_get_size(self->minisat_itp_groups);
#endif

  assumptions = ALLOC(int, numberOfGroups);

  numberOfGroups = 0;
  OLIST_FOREACH(groups, gen) {
    aGroup = (SatSolverGroup) Oiter_element(gen);

    /* the group existins */
    nusmv_assert(Olist_contains(SAT_SOLVER(self)->existingGroups,
                                (void*)aGroup));

    /* the group is unsatisfiable => exit */
    if (Olist_contains(SAT_SOLVER(self)->unsatisfiableGroups,
                       (void*)aGroup)) {
      FREE(assumptions);
      return SAT_SOLVER_UNSATISFIABLE_PROBLEM;
    }

    /* add negated literal of group id to the assumptions (if this is not
       a permanent group) */
    if (permanentGroup != aGroup) {
      assumptions[numberOfGroups] = -aGroup;
      ++numberOfGroups;
    }
  }

#if NUSMV_HAVE_MINISAT_INTERPOLATION
  {
    size_t i, sz;
    for (i = 0, sz = Stack_get_size(self->minisat_itp_groups); i < sz; ++i) {
      int g = (int)((self->minisat_itp_groups->array)[i]);
      assumptions[numberOfGroups++] = -g;
    }
  }
#endif

  /* try to solver (MiniSat_Solve will invoke internal simplifyDB) */
  minisatResult = MiniSat_Solve_Assume(self->minisatSolver,
                                       numberOfGroups, assumptions);
  FREE(assumptions);
  if (1 == minisatResult) {
    return SAT_SOLVER_SATISFIABLE_PROBLEM;
  }
  else
    return SAT_SOLVER_UNSATISFIABLE_PROBLEM;
}


/*!
  \brief Tries to solve formulas in groups belonging to the solver
  except the groups in the list.

  The permanent group must not be in the list.
  Returns a flag whether the solving was successful. If it was successful only
  then SatSolver_get_model may be invoked to obtain the model

  \sa SatSolverResult,SatSolver_get_permanent_group,
  SatIncSolver_create_group, SatSolver_get_model
*/

SatSolverResult
sat_minisat_solve_without_groups(const SatIncSolver_ptr solver,
                                 const Olist_ptr groups)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);

  SatSolverResult result;
  Olist_ptr includeGroups;
  Oiter gen;
  SatSolverGroup aGroup;

  SAT_MINISAT_CHECK_INSTANCE(self);
  nusmv_assert(!Olist_contains(groups,
                        (void*)SatSolver_get_permanent_group(SAT_SOLVER(self))));

  /* create a list of all groups except the groups in the list */
  includeGroups = Olist_create();
  OLIST_FOREACH(SAT_SOLVER(self)->existingGroups, gen) {
    aGroup = (SatSolverGroup) Oiter_element(gen);
    if(!Olist_contains(groups, (void*)aGroup)) {
      Olist_append(includeGroups, (void*)aGroup);
    }
  }

  result = sat_minisat_solve_groups(solver, includeGroups);
  Olist_destroy(includeGroups);

  return result;
}

/*!
  \brief Obtains the set of conflicting assumptions from MiniSat



  \sa sat_minisat_solve_permanent_group_assume,
  sat_minisat_get_conflict
*/

Slist_ptr sat_minisat_make_conflicts(const SatMinisat_ptr self)
{
  int index;
  Slist_ptr conflict;
  int litNumber;
  int* lits;

  SAT_MINISAT_CHECK_INSTANCE(self);
  /* a conflict is created only if there is no conflict */
  nusmv_assert((Slist_ptr)NULL == self->conflict);

  conflict = Slist_create();
  litNumber = MiniSat_Get_Nof_Conflict_Lits(self->minisatSolver);
  lits = ALLOC(int, litNumber);
  nusmv_assert(NULL != lits);
  MiniSat_Get_Conflict_Lits(self->minisatSolver, lits);
  for (index = 0; index < litNumber; index++) {
    int cnfLiteral = sat_minisat_minisatLiteral2cnfLiteral(self, lits[index]);

    /* appends the conflict */
    Slist_push(conflict, PTR_FROM_INT(void*, cnfLiteral));
  } /* for() */

  FREE(lits);

  return conflict;
}


/*!
  \brief Pure virtual function, sets random polarity mode if seed is
  not zero, otherwise sets default non-random polarity mode.

  It is a pure virtual function and SatSolver is an abstract
  base class. Every derived class must ovewrwrite this function.
  It is an error if the last solving was unsuccessful.
*/

void sat_minisat_set_random_mode(SatSolver_ptr solver, double seed)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);
  SAT_MINISAT_CHECK_INSTANCE(self);

  if (seed == (double)0) {
    sat_minisat_set_polarity_mode(solver, polarity_user);
  }
  else {
    MiniSat_Set_Random_Seed(self->minisatSolver, seed);
    sat_minisat_set_polarity_mode(solver, polarity_rnd);
  }
}


/*!
  \brief Pure virtual function, sets polarity mode accordingly to the
  passed value.

  It is a pure virtual function and SatSolver is an abstract
  base class. Every derived class must ovewrwrite this function.
  It is an error if the last solving was unsuccessful.
*/

void sat_minisat_set_polarity_mode(SatSolver_ptr solver, int mode)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);
  SAT_MINISAT_CHECK_INSTANCE(self);

  MiniSat_Set_Polarity_Mode(self->minisatSolver, mode);
}


/*!
  \brief Pure virtual function, returns currently set polarity mode.

  It is a pure virtual function and SatSolver is an abstract
  base class. Every derived class must ovewrwrite this function.
  It is an error if the last solving was unsuccessful.
*/

int sat_minisat_get_polarity_mode(const SatSolver_ptr solver)
{
  const SatMinisat_ptr self = SAT_MINISAT(solver);
  SAT_MINISAT_CHECK_INSTANCE(self);

  return MiniSat_Get_Polarity_Mode(self->minisatSolver);
}


/*!
  \brief Getter for minisatClause


*/

int* sat_minisat_get_minisatClause (const SatMinisat_ptr self)
{
  SAT_MINISAT_CHECK_INSTANCE(self);

  return self->minisatClause;
}


/*!
  \brief Getter for minisatClauseSize


*/

int sat_minisat_get_minisatClauseSize (const SatMinisat_ptr self)
{
  SAT_MINISAT_CHECK_INSTANCE(self);

  return self->minisatClauseSize;
}


/*!
  \brief Enlarge minisatClause, adapt minisatClauseSize

  Enlarges minisatClause until it has at least size minSize.

  \se minisatClause might be reallocated, minisatClauseSize
               changes value.

  \sa sat_minisat_add
*/

void sat_minisat_enlarge_minisatClause (const SatMinisat_ptr self,
                                        unsigned int minSize)
{
  SAT_MINISAT_CHECK_INSTANCE(self);

  while (self->minisatClauseSize < minSize) {
    self->minisatClauseSize = self->minisatClauseSize * 2 + 1;
    self->minisatClause = REALLOC(int,
                                  self->minisatClause,
                                  self->minisatClauseSize);
    nusmv_assert(self->minisatClause != (int*) NULL);
  }
}

#if NUSMV_HAVE_MINISAT_INTERPOLATION
/*!
  \brief


*/

static SatSolverItpGroup sat_minisat_curr_itp_group(SatSolver_ptr solver)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);
  SAT_MINISAT_CHECK_INSTANCE(self);

  if (STACK_IS_EMPTY(self->minisat_itp_groups)) {
    return 0;
  } else {
    return (SatSolverItpGroup)STACK_TOP(self->minisat_itp_groups);
  }
}


/*!
  \brief


*/

static SatSolverItpGroup sat_minisat_new_itp_group(SatSolver_ptr solver)
{
  SatMinisat_ptr self = SAT_MINISAT(solver);
  int g;
  SAT_MINISAT_CHECK_INSTANCE(self);

  g = MiniSat_New_Variable(self->minisatSolver);
  Stack_push(self->minisat_itp_groups, PTR_FROM_INT(void *, g));

  return g;
}

/*!
  \brief


*/

static Term
sat_minisat_extract_interpolant(SatSolver_ptr solver, int nof_ga_groups,
                                SatSolverItpGroup* ga_groups, TermFactoryCallbacks_ptr callbacks,
                                TermFactoryCallbacksUserData_ptr user_data)
{
  /* McMillan interpolation algorithm */
  SatMinisat_ptr self = SAT_MINISAT(solver);
  MiniSat_Proof unsat_proof;

  Stack_ptr to_process;
  hash_ptr cache;
  int i, n;
  Sset_ptr b_groups;
  Sset_ptr b_lits;
  Oiter g_it;

  Term ret;

  SAT_MINISAT_CHECK_INSTANCE(self);

  unsat_proof = MiniSat_Get_Unsat_Proof(self->minisatSolver);
  b_groups = Sset_create();
  for (i = 0, n = Stack_get_size(self->minisat_itp_groups); i < n; ++i) {
    Sset_insert(b_groups,
                PTR_FROM_INT(Sset_key, self->minisat_itp_groups->array[i]),
                PTR_FROM_INT(void *, 1));
  }
  for (i = 0; i < nof_ga_groups; ++i) {
    Sset_delete(b_groups, PTR_FROM_INT(Sset_key, ga_groups[i]), NULL);
  }

  b_lits = Sset_create();
  OLIST_FOREACH(SAT_SOLVER(self)->existingGroups, g_it) {
    Sset_insert(b_lits, Oiter_element(g_it), PTR_FROM_INT(void *, 1));
  }

  to_process = Stack_create();
  cache = new_assoc();

  Stack_push(to_process, (void *)unsat_proof);

  /* first, we collect all the literals occurring in B-leaf clauses */
  while (!STACK_IS_EMPTY(to_process)) {
    MiniSat_Proof p = (MiniSat_Proof)Stack_top(to_process);

    if (find_assoc(cache, (node_ptr)p)) {
      Stack_pop(to_process);
      continue;
    }
    insert_assoc(cache, (node_ptr)p, NODE_FROM_INT(1));

    if (MiniSat_Proof_IsRes(self->minisatSolver, p)) {
      int pivot;
      MiniSat_Proof c;

      for (i = 0, n = MiniSat_Proof_Size(self->minisatSolver, p); i < n; ++i) {
        MiniSat_Proof_Res(self->minisatSolver, p, i, &pivot, &c);
        if (!find_assoc(cache, (node_ptr)c)) {
          Stack_push(to_process, (void *)c);
        }
      }
    } else {
      boolean of_b;

      Stack_pop(to_process);

      of_b = false;
      n = MiniSat_Proof_Size(self->minisatSolver, p);

      for (i = 0; i < n; ++i) {
        int l = MiniSat_Proof_Lit(self->minisatSolver, p, i);
        Sset_key k = PTR_FROM_INT(Sset_key, l < 0 ? -l : l);

        if (Ssiter_is_valid(Sset_find(b_groups, k))) {
          of_b = true;
          break;
        }
      }

      if (of_b) {
        for (i = 0, n = MiniSat_Proof_Size(self->minisatSolver, p); i < n; ++i){
          int l = MiniSat_Proof_Lit(self->minisatSolver, p, i);
          Sset_key k = PTR_FROM_INT(Sset_key, l < 0 ? -l : l);

          Sset_insert(b_lits, k, PTR_FROM_INT(void *, 1));
        }
      }
    }
  }


  /* then, we can construct the interpolant */
  clear_assoc(cache);
  Stack_push(to_process, (void *)unsat_proof);

  while (!STACK_IS_EMPTY(to_process)) {
    MiniSat_Proof p = (MiniSat_Proof)Stack_top(to_process);

    if (find_assoc(cache, (node_ptr)p)) {
      Stack_pop(to_process);
      continue;
    }

    if (MiniSat_Proof_IsRes(self->minisatSolver, p)) {
      boolean done = true;
      int pivot;
      MiniSat_Proof c;

      for (i = 0, n = MiniSat_Proof_Size(self->minisatSolver, p); i < n; ++i) {
        MiniSat_Proof_Res(self->minisatSolver, p, i, &pivot, &c);
        if (!find_assoc(cache, (node_ptr)c)) {
          Stack_push(to_process, (void *)c);
          done = false;
        }
      }

      if (done) {
        Term f;
        Stack_pop(to_process);
        MiniSat_Proof_Res(self->minisatSolver, p, 0, &pivot, &c);
        f = (Term)find_assoc(cache, (node_ptr)c);

        for (i = 1, n = MiniSat_Proof_Size(self->minisatSolver, p); i < n; ++i){
          Term cur;
          bool of_a;
          Sset_key k;

          MiniSat_Proof_Res(self->minisatSolver, p, i, &pivot, &c);

          cur = (Term)find_assoc(cache, (node_ptr)c);
          k = PTR_FROM_INT(Sset_key, pivot < 0 ? -pivot : pivot);
          of_a = !Ssiter_is_valid(Sset_find(b_lits, k));
          if (of_a) {
            f = callbacks->make_or(f, cur, user_data);
          } else {
            f = callbacks->make_and(f, cur, user_data);
          }
        }

        insert_assoc(cache, (node_ptr)p, (node_ptr)f);
      }
    } else {
      boolean of_b;

      Stack_pop(to_process);

      of_b = false;
      n = MiniSat_Proof_Size(self->minisatSolver, p);

      for (i = 0; i < n; ++i) {
        int l = MiniSat_Proof_Lit(self->minisatSolver, p, i);
        Sset_key k = PTR_FROM_INT(Sset_key, l < 0 ? -l : l);

        if (Ssiter_is_valid(Sset_find(b_groups, k))) {
          of_b = true;
          break;
        }
      }
      if (!of_b && n == 1) {
        /* special case: clauses of size 1 can be made of backtracking groups,
           which we have to treat as coming from b */
        int l = MiniSat_Proof_Lit(self->minisatSolver, p, 0);
        Sset_key k = PTR_FROM_INT(Sset_key, l < 0 ? -l : l);
        of_b = Ssiter_is_valid(Sset_find(b_lits, k));
      }

      if (of_b) {
        insert_assoc(cache, (node_ptr)p, callbacks->make_true(user_data));
      } else {
        Term b_part = callbacks->make_false(user_data);
        for (i = 0, n = MiniSat_Proof_Size(self->minisatSolver, p); i < n; ++i){
          int l = MiniSat_Proof_Lit(self->minisatSolver, p, i);
          Sset_key k = PTR_FROM_INT(Sset_key, l < 0 ? -l : l);

          if (Ssiter_is_valid(Sset_find(b_lits, k))) {
            int idx = l < 0 ? -l : l;
            int cnf_lit = sat_minisat_minisatLiteral2cnfLiteral(self, idx);
            if (cnf_lit > 0) {
              Term v = callbacks->make_var(idx, user_data);
              if (l < 0) {
                v = callbacks->make_not(v, user_data);
              }
              b_part = callbacks->make_or(b_part, v, user_data);
            }
          }
        }

        insert_assoc(cache, (node_ptr)p, b_part);
      }
    }
  }

  ret = (Term)find_assoc(cache, (node_ptr)unsat_proof);

  free_assoc(cache);
  Stack_destroy(to_process);
  Sset_destroy(b_lits);
  Sset_destroy(b_groups);

  return ret;
}
#endif

/*---------------------------------------------------------------------------*/
/* Initializer, De-initializer, Finalizer                                    */
/*---------------------------------------------------------------------------*/
/*!
  \brief Initializes Sat Minisat object.


*/

void sat_minisat_init(SatMinisat_ptr self, const NuSMVEnv_ptr env,
                      const char* name, boolean enable_proof_logging)
{
  SAT_MINISAT_CHECK_INSTANCE(self);

  sat_inc_solver_init(SAT_INC_SOLVER(self), env, name);

  OVERRIDE(Object, finalize) = sat_minisat_finalize;

  OVERRIDE(SatSolver, add) = sat_minisat_add;
  OVERRIDE(SatSolver, set_polarity) = sat_minisat_set_polarity;
  OVERRIDE(SatSolver, set_preferred_variables) =
    sat_minisat_set_preferred_variables;
  OVERRIDE(SatSolver, clear_preferred_variables) =
    sat_minisat_clear_preferred_variables;
  OVERRIDE(SatSolver, solve_all_groups) = sat_minisat_solve_all_groups;
  OVERRIDE(SatSolver, solve_all_groups_assume) =
    sat_minisat_solve_permanent_group_assume;

  OVERRIDE(SatSolver, make_model) = sat_minisat_make_model;
  OVERRIDE(SatSolver, get_cnf_var) = sat_minisat_get_cnf_var;
  OVERRIDE(SatSolver, get_conflicts) = sat_minisat_get_conflicts;

  OVERRIDE(SatIncSolver, create_group) = sat_minisat_create_group;
  OVERRIDE(SatIncSolver, destroy_group) = sat_minisat_destroy_group;
  OVERRIDE(SatIncSolver, move_to_permanent_and_destroy_group)
    = sat_minisat_move_to_permanent_and_destroy_group;
  OVERRIDE(SatIncSolver, solve_groups) = sat_minisat_solve_groups;
  OVERRIDE(SatIncSolver, solve_without_groups)
    = sat_minisat_solve_without_groups;

  OVERRIDE(SatSolver, set_random_mode) = sat_minisat_set_random_mode;
  OVERRIDE(SatSolver, set_polarity_mode) = sat_minisat_set_polarity_mode;
  OVERRIDE(SatSolver, get_polarity_mode) = sat_minisat_get_polarity_mode;

#if NUSMV_HAVE_MINISAT_INTERPOLATION
  OVERRIDE(SatSolver, curr_itp_group) = sat_minisat_curr_itp_group;
  OVERRIDE(SatSolver, new_itp_group) = sat_minisat_new_itp_group;
  OVERRIDE(SatSolver, extract_interpolant) = sat_minisat_extract_interpolant;

  self->minisatSolver = MiniSat_CreateExt(enable_proof_logging);
  self->minisat_itp_groups = Stack_create();
#else
  self->minisatSolver = MiniSat_Create();
#endif

  /* the exisiting (-1) permanent group is OK, since minisat always
     deals with variables greater then 0 */

  self->cnfVar2minisatVar = new_assoc();
  self->minisatVar2cnfVar = new_assoc();

  self->minisatClauseSize = 10;
  self->minisatClause = ALLOC(int, self->minisatClauseSize);
  nusmv_assert(self->minisatClause != (int*) NULL);
}

/*!
  \brief Deinitializes SatMinisat object.


*/

void sat_minisat_deinit(SatMinisat_ptr self)
{
  SAT_MINISAT_CHECK_INSTANCE(self);

  FREE(self->minisatClause);

  free_assoc(self->cnfVar2minisatVar);
  free_assoc(self->minisatVar2cnfVar);

#if NUSMV_HAVE_MINISAT_INTERPOLATION
  Stack_destroy(self->minisat_itp_groups);
#endif
  MiniSat_Delete(self->minisatSolver);

  sat_solver_deinit(SAT_SOLVER(self));
}

/*!
  \brief Finalize method of SatMinisat class.

  Pure virtual function. This must be refined by derived classes.
*/
static void  sat_minisat_finalize(Object_ptr object, void* dummy)
{
  SatMinisat_ptr self = SAT_MINISAT(object);
  sat_minisat_deinit(self);
  FREE(self);
}

/*!
  \brief Computes the size of a clause.

  Computes the size of a clause.
*/
static int _get_clause_size(const int * clause) {
  int j = 0;

  if ((int *)NULL != clause) {
    while (clause[j] != 0) j++;
  }
  return j;
}
