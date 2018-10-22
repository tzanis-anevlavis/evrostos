/* ---------------------------------------------------------------------------


  This file is part of the ``be'' package of NuSMV version 2.
  Copyright (C) 2000-2001 by FBK-irst and University of Trento.
  Copyright (C) 2011 by FBK.

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
  \author Roberto Cavada, Marco Roveri, Michele Dorigatti
  \brief Conjunctive Normal Form of boolean extpressions

  This module defines the Be_Cnf structure and any related
  method. When converting a be into cnf form the Be_ConvertToCnf function
  returns a Be_Cnf structure. The Be_Cnf structure is a base class for the
  structure Bmc_Problem.

*/


#include "nusmv/core/be/be.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/parser/symbols.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*!
  \brief Private definition for Boolean Expression in CNF form

  In order to use a Be_Cnf instance see the Be_Cnf_ptr type.

  \sa Be_Cnf_ptr
*/

typedef struct Be_Cnf_TAG {
  be_ptr originalBe; /* the original BE problem */
  Slist_ptr cnfVars;    /* The list of CNF variables */
  Slist_ptr cnfClauses; /* The list of CNF clauses */
  int    cnfMaxVarIdx;  /* The maximum CNF variable index */

  /* literal assigned to whole CNF formula. (It may be negative)
     If the formula is a constant, see Be_Cnf_ptr. */
  int formulaLiteral;
} Be_Cnf;


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Declarations of internal functions                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void _be_cnf_destroy_clause(void* data);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

Be_Cnf_ptr Be_Cnf_Create(const be_ptr be)
{
  Be_Cnf_ptr self = ALLOC(Be_Cnf, 1);
  nusmv_assert(self != NULL);

  self->originalBe = be;
  self->cnfVars = Slist_create();
  self->cnfClauses = Slist_create();
  self->cnfMaxVarIdx = 0;
  self->formulaLiteral = 0;

  return self;
}

void Be_Cnf_Delete(Be_Cnf_ptr self)
{
  nusmv_assert(self != NULL);

  Slist_destroy_and_free_elements(self->cnfClauses, _be_cnf_destroy_clause);
  Slist_destroy(self->cnfVars);

  FREE(self);
}

void Be_Cnf_RemoveDuplicateLiterals(Be_Cnf_ptr self)
{
  int i, j;
  Siter iter;
  int * clause = (int *)NULL;
  hash_ptr lits = (hash_ptr)NULL;

  nusmv_assert(self != NULL);

  lits = new_assoc();

  for (iter = Slist_first(Be_Cnf_GetClausesList(self));
       !Siter_is_end(iter);
       iter = Siter_next(iter)) {

    clause = (int*) Siter_element(iter);

    i = 0;
    while (clause[i] != 0) {
      if (Nil != find_assoc(lits, NODE_FROM_INT(clause[i]))) {
        j = i+1;
        while (clause[j] != 0) {
          clause[j-1] = clause[j];
          j++;
        }
      }
      else {
        insert_assoc(lits, NODE_FROM_INT(clause[i]), NODE_FROM_INT(1));
      }
      i++;
    }

    /* this clear the hash */
    clear_assoc(lits);
  }

  free_assoc(lits);
}

be_ptr Be_Cnf_GetOriginalProblem(const Be_Cnf_ptr self)
{
  return self->originalBe;
}

int Be_Cnf_GetFormulaLiteral(const Be_Cnf_ptr self)
{
  return self->formulaLiteral;
}

Slist_ptr Be_Cnf_GetVarsList(const Be_Cnf_ptr self) { return self->cnfVars; }

Slist_ptr Be_Cnf_GetClausesList(const Be_Cnf_ptr self)
{
  return self->cnfClauses;
}

int Be_Cnf_GetMaxVarIndex(const Be_Cnf_ptr self)
{
  return self->cnfMaxVarIdx;
}

size_t Be_Cnf_GetVarsNumber(const Be_Cnf_ptr self)
{
  return Slist_get_size(Be_Cnf_GetVarsList(self));
}

size_t Be_Cnf_GetClausesNumber(const Be_Cnf_ptr self)
{
  return Slist_get_size(Be_Cnf_GetClausesList(self));
}

void Be_Cnf_SetFormulaLiteral(Be_Cnf_ptr self, const int  formula_literal)
{
  self->formulaLiteral =  formula_literal;
}

void Be_Cnf_SetMaxVarIndex(Be_Cnf_ptr self, const int max_idx)
{
  self->cnfMaxVarIdx = max_idx;
}

void Be_Cnf_PrintStat(const Be_Cnf_ptr self, FILE* outFile, char* prefix)
{
  /* compute values */
  int max_clause_size = 0;
  float sum_clause_size = 0;
  Siter cnf;

  nusmv_assert(self != (Be_Cnf_ptr)NULL);

  SLIST_FOREACH(Be_Cnf_GetClausesList(self), cnf) {
    int* clause = (int*)Siter_element(cnf);
    int clause_size;

    SLIST_CHECK_INSTANCE(Be_Cnf_GetClausesList(self));

    for (clause_size = 0; clause[clause_size] != 0; ++clause_size) { }

    sum_clause_size += clause_size;
    if (clause_size > max_clause_size) max_clause_size = clause_size;
  }

  /* print out values */
    fprintf(outFile,
            "%s Clause number: %i\n"
            "%s Var number: %i\n"
            "%s Max var index: %i\n"
            "%s Average clause size: %.2f\n"
            "%s Max clause size: %i\n",
            prefix,
            (int)Be_Cnf_GetClausesNumber(self),
            prefix,
            (int)Be_Cnf_GetVarsNumber(self),
            prefix,
            Be_Cnf_GetMaxVarIndex(self),
            prefix,
            /* the average clause size */
            (double)(sum_clause_size / Slist_get_size(Be_Cnf_GetClausesList(self))),
            prefix,
            max_clause_size);
}

/*!
  \brief Frees the array used to store the clause.

  
*/
static void _be_cnf_destroy_clause(void* data) {
  int * _data = (int *)data;

  FREE(_data);
}
/**AutomaticEnd***************************************************************/

