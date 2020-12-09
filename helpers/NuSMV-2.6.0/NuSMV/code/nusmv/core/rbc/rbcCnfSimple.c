/* ---------------------------------------------------------------------------


  This file is part of the ``rbc'' package of NuSMV version 2.
  Copyright (C) 2000-2001 by University of Genova.
  Copyright (C) 2007 by FBK-irst.

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
  \author Armando Tacchella, Tommi Junttila and Marco Roveri 
  \brief Conjunctive Normal Form (CNF) conversions.

  External functions included in this module:
                <ul>
                <li> <b>Rbc_Convert2CnfSimple()</b>
                </ul>

*/


#include "nusmv/core/utils/ErrorMgr.h"
#include <limits.h>

#include "nusmv/core/rbc/rbcInt.h"
#include "nusmv/core/utils/error.h" /* for internal_error */
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Stucture declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief Data passing in cnf-DFS.

  Data passing in cnf-DFS.
*/

struct CnfDfsData {
  Rbc_Manager_t* rbcManager;
  Slist_ptr clauses;
  Slist_ptr vars;
  int result;
};


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef struct CnfDfsData CnfDfsData_t;


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static int CnfSet(Rbc_t * f, char * cnfData, nusmv_ptrint sign);
static void CnfFirst(Rbc_t * f, char * cnfData, nusmv_ptrint sign);
static void CnfBack(Rbc_t * f, char * cnfData, nusmv_ptrint sign);
static void CnfLast(Rbc_t * f, char * cnfData, nusmv_ptrint sign);
static lsGeneric SwapSign(lsGeneric data);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of external functions                                          */
/*---------------------------------------------------------------------------*/


/*!
  \brief Translates the rbc into the corresponding (equisatisfiable)
               set of clauses.

  Given `rbcManager' and `f', `clauses' is filled with the
               disjunctions corresponding to the rbc nodes according to
               the rules:

               f = A & B => -f A              f = A <-> B =>  f  A  B
                            -f B                              f -A -B
                             f -A -B                         -f -A  B
                                                             -f  A -B

               f = if A then B else C =>  f  A -C
                                          f -A -B
                                         -f  A  C
                                         -f -A  B

               `vars' is filled with the variables that occurred in `f'
               (original or model variables converted into corresponding CNF
               variables). It is user's responsibility
               to create `clauses' and `vars' *before* calling the function.
               New variables are added by the conversion: the maximum
               index is returned by the function.
               The literal associated to 'f' is assigned to parameter
               *literalAssignedToWholeFormula (it may be negative).
               Special case - A CONSTANT (this is consistent with description
                 of Be_Cnf_ptr): if the formula is a constant
                 then *literalAssignedToWholeFormula will be INT_MAX
                 and the return value will 0.
                 if formula is true, `clauses' is the empty list,
                 if formula is false, `clauses' contains a single empty clause.

  \se `clauses', `vars' and '*literalAssignedToWholeFormula'
              are filled up. Fields inside rbcManager might change 
*/

int Rbc_Convert2CnfSimple(Rbc_Manager_t* rbcManager, Rbc_t* f,
                          Slist_ptr clauses, Slist_ptr vars,
                          int* literalAssignedToWholeFormula)
{
  Dag_DfsFunctions_t cnfFunctions;
  CnfDfsData_t       cnfData;

  /* The caller will ensure this */
  nusmv_assert(*literalAssignedToWholeFormula == INT_MAX);

   /* Setting up the DFS functions. */
  cnfFunctions.Set        = (PF_IVPCPI)CnfSet;
  cnfFunctions.FirstVisit = (PF_VPVPCPI)CnfFirst;
  cnfFunctions.BackVisit  = (PF_VPVPCPI)CnfBack;
  cnfFunctions.LastVisit  = (PF_VPVPCPI)CnfLast;

  /* Setting up the DFS data. */
  cnfData.rbcManager = rbcManager;
  cnfData.clauses    = clauses;
  cnfData.vars       = vars;
  cnfData.result     = 0;

  /* Calling DFS on f. */
  Dag_Dfs(f, &cnfFunctions, (char*)(&cnfData));

  /* Remember the unit clause (literal) that stands for the whole formula f */
  *literalAssignedToWholeFormula = cnfData.result;

  return rbcManager->maxCnfVariable;

} /* End of Rbc_Convert2CnfSimple. */


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Dfs Set for CNF conversion.

  Dfs Set for CNF conversion.

  \se None
*/

static int CnfSet(Rbc_t* f, char* cnfData, nusmv_ptrint sign)
{
  CnfDfsData_t* cd = (CnfDfsData_t*)cnfData;

  /* Set the current integer reference as result. */
  cd->result = (sign != 0 ? -1 * (f->iRef) : f->iRef);

  /* All nodes should be visited once and only once. */
  return 0;
} /* End of CnfSet. */


/*!
  \brief Dfs FirstVisit for CNF conversion.

  Dfs FirstVisit for CNF conversion.

  \se None
*/

static void CnfFirst(Rbc_t* f, char* cnfData, nusmv_ptrint sign)
{
  if (f->symbol != RBCVAR) {
    /* Create a temporary list (use vertex's own general reference). */
    f->gRef = (char*) lsCreate();
  }
} /* End of CnfFirst. */


/*!
  \brief Dfs BackVisit for CNF conversion.

  Dfs BackVisit for CNF conversion.

  \se None
*/

static void
CnfBack(Rbc_t* f, char* cnfData, nusmv_ptrint sign)
{
  CnfDfsData_t * cd   = (CnfDfsData_t*)cnfData;

  /* Get the current result and add it (negated) to the temporary list. */
  (void) lsNewEnd((lsList)(f->gRef),
                  PTR_FROM_INT(lsGeneric, -1 * (cd->result)), LS_NH);

  return;

} /* End of CnfBack. */


/*!
  \brief Dfs LastVisit for CNF conversion.

  Dfs LastVisit for CNF conversion.

  \se None
*/

static void CnfLast(Rbc_t* f, char* cnfData, nusmv_ptrint sign)
{
  lsGen     gen;
  nusmv_ptrint s;
  int pol;

  CnfDfsData_t* cd = (CnfDfsData_t*) cnfData;
  lsList sons = (lsList) (f->gRef);

  int cnfVar = Rbc_get_node_cnf(cd->rbcManager, f,
                                &cd->rbcManager->maxCnfVariable);

  const NuSMVEnv_ptr env = Rbc_ManagerGetEnvironment(cd->rbcManager);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));


  f->iRef = cnfVar;

  if (f->symbol == RBCVAR) {

    /* Fill in the list of 'original' model vars converted into CNF vars */
    Slist_push(cd->vars, PTR_FROM_INT(void*, cnfVar));
  }
  else {

    /* Generate and append clauses. */
    if (f->symbol == RBCAND) {
      int j = 0;
      int clause_size = lsLength(sons) + 2;
      int * tmpCl1 = (int *)NULL;
      int * tmpCl2 = (int *)NULL;

      /* Compute the binary clauses {-f s_i} */
      /* Compute the clause {f -s_1 -s_2} */
      tmpCl1 = ALLOC(int, clause_size);
      gen = lsStart(sons);
      while (lsNext(gen, (lsGeneric*) &s, LS_NH) == LS_OK) {
        tmpCl1[j++] = (int)s;
        tmpCl2      = ALLOC(int, 3);
        tmpCl2[0]   = -1 * cnfVar;
        tmpCl2[1]   = -1 * (int)s;
        tmpCl2[2]   = 0;
        /* Add the binary clauses {-f s_i} */
        Slist_push(cd->clauses, (void*)tmpCl2);
        tmpCl2      = (int *)NULL; /* to avoid possible problems */
      }
      lsFinish(gen);
      tmpCl1[j++] = cnfVar;
      tmpCl1[j++] = 0;
      nusmv_assert(j == clause_size);
      /* Add the clause {f -s_1 -s_2} */
      Slist_push(cd->clauses, (void*)tmpCl1);

      lsDestroy(sons, NULL);

    } else if (f->symbol == RBCIFF) {
      int i;
      int * tmpCl1 = (int *)NULL;
      int * tmpCl2 = (int *)NULL;
      int clause_size = lsLength(sons) + 2;

      /* Compute clause {-f s_1 -s_2} */
      tmpCl1 = ALLOC(int, clause_size);
      i = 0;
      tmpCl1[i++] = -1 * cnfVar;
      pol = -1;
      gen = lsStart(sons);
      while (lsNext(gen, (lsGeneric*) &s, LS_NH) == LS_OK) {
        tmpCl1[i++] = (int) s * pol;
        pol *= -1;
      }
      lsFinish(gen);
      tmpCl1[i++] = 0;
      nusmv_assert(i == clause_size);
      /* Add the clause {-f s_1 -s_2} */
      Slist_push(cd->clauses, (void*)tmpCl1);

      /* Compute clause {-f -s_1 s_2} */
      tmpCl1 = ALLOC(int, clause_size);
      i = 0;
      tmpCl1[i++] = -1 * cnfVar;
      pol = 1;
      gen = lsStart(sons);
      while (lsNext(gen, (lsGeneric*) &s, LS_NH) == LS_OK) {
        tmpCl1[i++] = pol * (int) s;
        pol *= -1;
      }
      lsFinish(gen);
      tmpCl1[i++] = 0;
      nusmv_assert(i == clause_size);
      /* Add the clause {-f -s_1 s_2} */
      Slist_push(cd->clauses, (void*)tmpCl1);

      /* Compute clause {f s_1 s_2} */
      /* Compute clause {f -s_1 -s_2} */
      tmpCl1 = ALLOC(int, clause_size);
      tmpCl2 = ALLOC(int, clause_size);
      i = 0;
      gen = lsStart(sons);
      while (lsNext(gen, (lsGeneric*) &s, LS_NH) == LS_OK) {
        tmpCl1[i] = -1 * (int)s;
        tmpCl2[i] =      (int)s;
        i++;
      }
      lsFinish(gen);
      tmpCl1[i] = cnfVar;
      tmpCl2[i] = cnfVar;
      i++;
      tmpCl1[i] = 0;
      tmpCl2[i] = 0;
      i++;
      nusmv_assert(i == clause_size);

      /* Add the clause {f s_1 s_2} */
      Slist_push(cd->clauses, (void*)tmpCl1);
      /* Add the clause {f -s_1 -s_2} */
      Slist_push(cd->clauses, (void*)tmpCl2);

      lsDestroy(sons, NULL);

    } else if (f->symbol == RBCITE) {
      int * tmpCl = (int *)NULL;
      nusmv_ptrint i, t, e;

      gen = lsStart(sons);
      /* Should have three children */
      if(lsNext(gen, (lsGeneric*) &i, LS_NH) != LS_OK) error_unreachable_code();
      if(lsNext(gen, (lsGeneric*) &t, LS_NH) != LS_OK) error_unreachable_code();
      if(lsNext(gen, (lsGeneric*) &e, LS_NH) != LS_OK) error_unreachable_code();
      lsFinish(gen);

      /* Compute the clause {-f -i t} */
      tmpCl = ALLOC(int, 4);
      tmpCl[0] = -1 * cnfVar;
      tmpCl[1] = (int)i;
      tmpCl[2] = -1 * (int)t;
      tmpCl[3] = 0;
      /* Add the clause {-f -i t} */
      Slist_push(cd->clauses, (void*) tmpCl);

      /* Compute the clause {-f i e} */
      tmpCl = ALLOC(int, 4);
      tmpCl[0] = -1 * cnfVar;
      tmpCl[1] = -1 * (int)i;
      tmpCl[2] = -1 * (int)e;
      tmpCl[3] = 0;
      /* Add the clause {-f i e} */
      Slist_push(cd->clauses, (void*) tmpCl);

      /* Compute the clause {f -i -t} */
      tmpCl = ALLOC(int, 4);
      tmpCl[0] = cnfVar;
      tmpCl[1] = (int)i;
      tmpCl[2] = (int)t;
      tmpCl[3] = 0;
      /* Add the clause {f -i -t} */
      Slist_push(cd->clauses, (void*) tmpCl);

      /* Compute the clause {f i -e} */
      tmpCl = ALLOC(int, 4);
      tmpCl[0] = cnfVar;
      tmpCl[1] = -1 * (int)i;
      tmpCl[2] = (int)e;
      tmpCl[3] = 0;
      /* Add the clause {f i -e} */
      Slist_push(cd->clauses, (void*) tmpCl);

      lsDestroy(sons, NULL);
    }
    else
      ErrorMgr_internal_error(errmgr, "CnfLast: unkown RBC symbol");

  }

  /* Adjust the sign of the result. */
  cd->result = (sign != 0 ? -1 * (f->iRef) : f->iRef);

  return;

} /* End of CnfLast. */


/*!
  \brief Swaps the sign of the argument.

  Swaps the sign of the argument.

  \se None
*/

static lsGeneric SwapSign(lsGeneric data)
{
  return PTR_FROM_INT(lsGeneric, -1 * PTR_TO_INT(data));
} /* End of CnfSwapSign. */



