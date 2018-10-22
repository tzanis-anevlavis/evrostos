/* ---------------------------------------------------------------------------


  This file is part of the ``rbc'' package of NuSMV version 2.
  Copyright (C) 2000-2001 by University of Genova.

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
  \author Armando Tacchella and Tommi Junttila
  \brief Formula substitutions.

  External functions included in this module:
    <ul>
    <li> <b>Rbc_Subst()</b> Substitute variables with variables
    <li> <b>Rbc_Shift()</b> Shift the variables along an offset
    <li> <b>Rbc_SubstRbc()</b> Substitute variables with formulas
    </ul>

*/


#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/rbc/rbcInt.h"

#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

static const unsigned int RBC_MAX_OUTDEGREE = 3;

/*---------------------------------------------------------------------------*/
/* Stucture declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief Data passing in substitution-DFS (variables to variables).

  Data passing in substitution-DFS (variables to variables).
*/

struct SubstDfsData {
  Rbc_Manager_t * rbcManager;
  int           * subst;
  Rbc_t         * result;
  const int   * log2phy; /* introduced with NuSMV-2.4 */
  const int   * phy2log; /* introduced with NuSMV-2.4 */
};

/*!
  \brief Data passing in shift-DFS (variables to variables).

  Data passing in shift-DFS (variables to variables).
*/

struct ShiftDfsData {
  Rbc_Manager_t * rbcManager;
  int             shift;
  Rbc_t         * result;
  const int     * log2phy; /* introduced with NuSMV-2.4 */
  const int     * phy2log; /* introduced with NuSMV-2.4 */
};

/*!
  \brief Data passing in substitution-DFS (variables to formula).

  Data passing in substitution-DFS (variables to formula).
*/

struct SubstRbcDfsData {
  Rbc_Manager_t  * rbcManager;
  Rbc_t         ** substRbc;
  Rbc_t          * result;
  int            * phy2log; /* introduced with NuSMV-2.4 */
};

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef struct SubstDfsData     SubstDfsData_t;
typedef struct ShiftDfsData     ShiftDfsData_t;
typedef struct SubstRbcDfsData  SubstRbcDfsData_t;

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

static int SubstSet(Rbc_t * f, char * SubstData, nusmv_ptrint sign);
static void SubstFirst(Rbc_t * f, char * SubstData, nusmv_ptrint sign);
static void SubstBack(Rbc_t * f, char * SubstData, nusmv_ptrint sign);
static void SubstLast(Rbc_t * f, char * SubstData, nusmv_ptrint sign);
static void LogicalSubstLast(Rbc_t * f, char * SubstData, nusmv_ptrint sign);
static int ShiftSet(Rbc_t * f, char * shiftData, nusmv_ptrint sign);
static void ShiftFirst(Rbc_t * f, char * shiftData, nusmv_ptrint sign);
static void ShiftBack(Rbc_t * f, char * shiftData, nusmv_ptrint sign);
static void ShiftLast(Rbc_t * f, char * shiftData, nusmv_ptrint sign);
static void LogicalShiftLast(Rbc_t * f, char * shiftData, nusmv_ptrint sign);
static int SubstRbcSet(Rbc_t * f, char * SubstRbcData, nusmv_ptrint sign);
static void SubstRbcFirst(Rbc_t * f, char * SubstRbcData, nusmv_ptrint sign);
static void SubstRbcBack(Rbc_t * f, char * SubstRbcData, nusmv_ptrint sign);
static void SubstRbcLast(Rbc_t * f, char * SubstRbcData, nusmv_ptrint sign);
static void LogicalSubstRbcLast(Rbc_t * f, char * SubstRbcData,
        nusmv_ptrint sign);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of external functions                                          */
/*---------------------------------------------------------------------------*/

Rbc_t *
Rbc_Subst(Rbc_Manager_t * rbcManager,
          Rbc_t         * f,
          int           * subst)
{
  Dag_DfsFunctions_t SubstFunctions;
  SubstDfsData_t     SubstData;

  /* Cleaning the user fields. */
  Dag_Dfs(f, Rbc_ManagerGetDfsCleanFun(rbcManager), NIL(char));

  /* Setting up the DFS functions. */
  SubstFunctions.Set        = (PF_IVPCPI)SubstSet;
  SubstFunctions.FirstVisit = (PF_VPVPCPI)SubstFirst;
  SubstFunctions.BackVisit  = (PF_VPVPCPI)SubstBack;
  SubstFunctions.LastVisit  = (PF_VPVPCPI)SubstLast;

  /* Setting up the DFS data. */
  SubstData.rbcManager = rbcManager;
  SubstData.subst   = subst;
  SubstData.log2phy = (const int *) NULL;
  SubstData.phy2log = (const int *) NULL;
  SubstData.result     = NIL(Rbc_t);

  /* Calling DFS on f. */
  Dag_Dfs(f, &SubstFunctions, (char*)(&SubstData));

  return SubstData.result;

} /* End of Rbc_Subst. */

Rbc_t* Rbc_LogicalSubst(Rbc_Manager_t* rbcManager,
                        Rbc_t* f,
                        int* subst,
                        const int* log2phy, const int* phy2log)
{
  Dag_DfsFunctions_t SubstFunctions;
  SubstDfsData_t     SubstData;

  /* Cleaning the user fields. */
  Dag_Dfs(f, Rbc_ManagerGetDfsCleanFun(rbcManager), NIL(char));

  /* Setting up the DFS functions. */
  SubstFunctions.Set        = (PF_IVPCPI)SubstSet;
  SubstFunctions.FirstVisit = (PF_VPVPCPI)SubstFirst;
  SubstFunctions.BackVisit  = (PF_VPVPCPI)SubstBack;
  SubstFunctions.LastVisit  = (PF_VPVPCPI)LogicalSubstLast;

  /* Setting up the DFS data. */
  SubstData.rbcManager = rbcManager;
  SubstData.subst   = subst;
  SubstData.log2phy = log2phy;
  SubstData.phy2log = phy2log;
  SubstData.result     = NIL(Rbc_t);

  /* Calling DFS on f. */
  Dag_Dfs(f, &SubstFunctions, (char*)(&SubstData));

  return SubstData.result;

} /* End of Rbc_Subst. */

Rbc_t* Rbc_Shift(Rbc_Manager_t* rbcManager,
                 Rbc_t* f,
                 int shift)
{
  Dag_DfsFunctions_t shiftFunctions;
  ShiftDfsData_t     shiftData;

  /* Cleaning the user fields. */
  Dag_Dfs(f, Rbc_ManagerGetDfsCleanFun(rbcManager), NIL(char));

  /* Setting up the DFS. */
  shiftFunctions.Set        = (PF_IVPCPI)ShiftSet;
  shiftFunctions.FirstVisit = (PF_VPVPCPI)ShiftFirst;
  shiftFunctions.BackVisit  = (PF_VPVPCPI)ShiftBack;
  shiftFunctions.LastVisit  = (PF_VPVPCPI)ShiftLast;

  /* Setting up the DFS data. */
  shiftData.rbcManager = rbcManager;
  shiftData.shift      = shift;
  shiftData.log2phy    = (const int*) NULL;
  shiftData.phy2log    = (const int*) NULL;
  shiftData.result     = NIL(Rbc_t);

  /* Calling DFS on f. */
  Dag_Dfs(f, &shiftFunctions, (char*)(&shiftData));

  return shiftData.result;

} /* End of Rbc_Shift. */

Rbc_t* Rbc_LogicalShift(Rbc_Manager_t* rbcManager,
                        Rbc_t* f,
                        int shift,
                        const int* log2phy, const int* phy2log)
{
  Dag_DfsFunctions_t shiftFunctions;
  ShiftDfsData_t     shiftData;

  /* Cleaning the user fields. */
  Dag_Dfs(f, Rbc_ManagerGetDfsCleanFun(rbcManager), NIL(char));

  /* Setting up the DFS. */
  shiftFunctions.Set        = (PF_IVPCPI)ShiftSet;
  shiftFunctions.FirstVisit = (PF_VPVPCPI)ShiftFirst;
  shiftFunctions.BackVisit  = (PF_VPVPCPI)ShiftBack;
  shiftFunctions.LastVisit  = (PF_VPVPCPI)LogicalShiftLast;

  /* Setting up the DFS data. */
  shiftData.rbcManager = rbcManager;
  shiftData.shift      = shift;
  shiftData.log2phy    = log2phy;
  shiftData.phy2log    = phy2log;
  shiftData.result     = NIL(Rbc_t);

  /* Calling DFS on f. */
  Dag_Dfs(f, &shiftFunctions, (char*)(&shiftData));

  return shiftData.result;

} /* End of Rbc_Shift. */

Rbc_t *
Rbc_SubstRbc(
             Rbc_Manager_t  * rbcManager,
             Rbc_t          * f,
             Rbc_t         ** substRbc)
{
  Dag_DfsFunctions_t SubstRbcFunctions;
  SubstRbcDfsData_t  SubstRbcData;

  if (Rbc_IsConstant(rbcManager, f)) return f;

  /* Cleaning the user fields. */
  Dag_Dfs(f, Rbc_ManagerGetDfsCleanFun(rbcManager), NIL(char));

  /* Setting up the DFS functions. */
  SubstRbcFunctions.Set        = (PF_IVPCPI)SubstRbcSet;
  SubstRbcFunctions.FirstVisit = (PF_VPVPCPI)SubstRbcFirst;
  SubstRbcFunctions.BackVisit  = (PF_VPVPCPI)SubstRbcBack;
  SubstRbcFunctions.LastVisit  = (PF_VPVPCPI)SubstRbcLast;

  /* Setting up the DFS data. */
  SubstRbcData.rbcManager = rbcManager;
  SubstRbcData.substRbc   = substRbc;
  SubstRbcData.phy2log    = (int*) NULL;
  SubstRbcData.result     = NIL(Rbc_t);

  /* Calling DFS on f. */
  Dag_Dfs(f, &SubstRbcFunctions, (char*)(&SubstRbcData));

  return SubstRbcData.result;

} /* End of Rbc_SubstRbc. */

Rbc_t* Rbc_LogicalSubstRbc(Rbc_Manager_t* rbcManager,
                           Rbc_t* f,
                           Rbc_t** substRbc,
                           int* phy2log)
{
  Dag_DfsFunctions_t SubstRbcFunctions;
  SubstRbcDfsData_t  SubstRbcData;

  /* Cleaning the user fields. */
  Dag_Dfs(f, Rbc_ManagerGetDfsCleanFun(rbcManager), NIL(char));

  /* Setting up the DFS functions. */
  SubstRbcFunctions.Set        = (PF_IVPCPI)SubstRbcSet;
  SubstRbcFunctions.FirstVisit = (PF_VPVPCPI)SubstRbcFirst;
  SubstRbcFunctions.BackVisit  = (PF_VPVPCPI)SubstRbcBack;
  SubstRbcFunctions.LastVisit  = (PF_VPVPCPI)LogicalSubstRbcLast;

  /* Setting up the DFS data. */
  SubstRbcData.rbcManager = rbcManager;
  SubstRbcData.substRbc   = substRbc;
  SubstRbcData.phy2log    = phy2log;
  SubstRbcData.result     = NIL(Rbc_t);

  /* Calling DFS on f. */
  Dag_Dfs(f, &SubstRbcFunctions, (char*)(&SubstRbcData));

  return SubstRbcData.result;

} /* End of Rbc_SubstRbc. */


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Dfs Set for substitution.

  Dfs Set for substitution.

  \se None
*/

static int
SubstSet(Rbc_t  * f,
         char   * SubstData,
         nusmv_ptrint sign)
{
  SubstDfsData_t * sd = (SubstDfsData_t*)SubstData;

  /* Set the current general reference as result. */
  sd -> result = RbcId((Rbc_t*)(f -> gRef), sign);

  /* All nodes should be visited once and only once. */
  return (0);

} /* End of SubstSet. */


/*!
  \brief Dfs FirstVisit for substitution.

  Dfs FirstVisit for substitution.

  \se None
*/

static void
SubstFirst(Rbc_t  * f,
           char   * SubstData,
           nusmv_ptrint sign)
{

  /* Create a temporary list (use vertex own general reference). */
  if (f -> symbol != RBCVAR) {
    f -> gRef = (char*)ALLOC(Rbc_t*, RBC_MAX_OUTDEGREE);
    f -> iRef = 0;
  }

  return;

} /* End of SubstFirst. */


/*!
  \brief Dfs BackVisit for substitution.

  Dfs BackVisit for substitution.

  \se None
*/

static void
SubstBack(Rbc_t  * f,
          char   * SubstData,
          nusmv_ptrint sign)
{
  SubstDfsData_t * sd   = (SubstDfsData_t*)SubstData;

  /* Get the current result and add it to the temporary list. */
  nusmv_assert(f -> iRef < RBC_MAX_OUTDEGREE);
  ((Rbc_t**)(f -> gRef))[(f -> iRef)++] = sd -> result;

  return;

} /* End of SubstBack. */



/*!
  \brief Dfs LastVisit for Substitution.

  Dfs LastVisit for Substitution.

  \se None
*/

static void SubstLast(Rbc_t* f, char* SubstData, nusmv_ptrint sign)
{
  SubstDfsData_t * sd   = (SubstDfsData_t*)SubstData;
  Rbc_t         ** sons = (Rbc_t**)(f -> gRef);

  const NuSMVEnv_ptr env = Rbc_ManagerGetEnvironment(sd->rbcManager);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));


  if (f -> symbol == RBCVAR) {
    /* Variables need to be subsituted. */
    int idx = ((int*)sd -> subst)[PTR_TO_INT(f -> data)];
    if (RBC_INVALID_SUBST_VALUE == idx) {
      ErrorMgr_internal_error(errmgr, "%s: Tried to substitute an invalid index", __func__);
    }
    sd -> result = Rbc_GetIthVar(sd -> rbcManager, idx);
  }
  else {
    /* Substitutions may trigger simplifications. */
    if (f -> symbol == RBCAND)
      sd->result = Rbc_MakeAnd(sd -> rbcManager, sons[0], sons[1], RBC_TRUE);
    else if (f -> symbol == RBCIFF)
      sd->result = Rbc_MakeIff(sd -> rbcManager, sons[0], sons[1], RBC_TRUE);
    else if (f -> symbol == RBCITE)
      sd->result = Rbc_MakeIte(sd -> rbcManager, sons[0], sons[1],
                               sons[2], RBC_TRUE);
    else {
      ErrorMgr_internal_error(errmgr, "SubstLast: unknown RBC symbol");
    }
    /* Clean the temporary sons list. */
    FREE(sons);
  }

  /* Adjust the sign and leave the result in the gRef too. */
  f -> gRef = (char*)(sd -> result);
  sd -> result = RbcId(sd -> result, sign);

  return;

} /* End of SubstLast. */


/*!
  \brief Dfs LastVisit for logical Substitution.

  Dfs LastVisit for logical Substitution.

  \se None
*/

static void LogicalSubstLast(Rbc_t* f, char* SubstData, nusmv_ptrint sign)
{
  SubstDfsData_t * sd   = (SubstDfsData_t*)SubstData;
  Rbc_t         ** sons = (Rbc_t**)(f -> gRef);

  const NuSMVEnv_ptr env = Rbc_ManagerGetEnvironment(sd -> rbcManager);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (f -> symbol == RBCVAR) {
    /* Variables need to be subsituted. Passes throught the levels phy
       to log, shifts, and then passes again down from log to phy: */
    int idx = sd->subst[sd->phy2log[PTR_TO_INT(f -> data)]];
    if (RBC_INVALID_SUBST_VALUE == idx) {
      ErrorMgr_internal_error(errmgr, "%s: Tried to substitute an invalid index", __func__);
    }

    sd -> result = Rbc_GetIthVar(sd->rbcManager, sd->log2phy[idx]);
  }
  else {
    /* Substitutions may trigger simplifications. */
    if (f -> symbol == RBCAND)
      sd->result = Rbc_MakeAnd(sd -> rbcManager, sons[0], sons[1], RBC_TRUE);
    else if (f -> symbol == RBCIFF)
      sd->result = Rbc_MakeIff(sd -> rbcManager, sons[0], sons[1], RBC_TRUE);
    else if (f -> symbol == RBCITE)
      sd->result = Rbc_MakeIte(sd -> rbcManager, sons[0], sons[1],
                               sons[2], RBC_TRUE);
    else {
      ErrorMgr_internal_error(errmgr, "LogicalSubstLast: unknown RBC symbol");
    }
    /* Clean the temporary sons list. */
    FREE(sons);
  }

  /* Adjust the sign and leave the result in the gRef too. */
  f -> gRef = (char*)(sd -> result);
  sd -> result = RbcId(sd -> result, sign);

  return;

} /* End of LogicalSubstLast. */


/*!
  \brief Dfs Set for shifting.

  Dfs Set for shifting.

  \se None
*/

static int
ShiftSet(Rbc_t  * f,
         char   * shiftData,
         nusmv_ptrint sign)
{
  ShiftDfsData_t * sd = (ShiftDfsData_t*)shiftData;

  /* Set the current general reference as result. */
  sd -> result = RbcId((Rbc_t*)(f -> gRef), sign);

  /* All the nodes are visited once and only once. */
  return (0);

} /* End of ShiftSet. */


/*!
  \brief Dfs FirstVisit for shifting.

  Dfs FirstVisit for shifting.

  \se None
*/

static void
ShiftFirst(Rbc_t  * f,
           char   * shiftData,
           nusmv_ptrint sign)
{

  /* Create a temporary list (use vertex own general reference). */
  if (f -> symbol != RBCVAR) {
    f -> gRef = (char*)ALLOC(Rbc_t*, RBC_MAX_OUTDEGREE);
    f -> iRef = 0;
  }

  return;

} /* End of ShiftFirst. */


/*!
  \brief Dfs BackVisit for shifting.

  Dfs BackVisit for shifting.

  \se None
*/

static void
ShiftBack(Rbc_t  * f,
          char   * shiftData,
          nusmv_ptrint sign)
{
  ShiftDfsData_t * sd   = (ShiftDfsData_t*)shiftData;

  /* Get the current result and add it to the temporary list. */
  nusmv_assert(f -> iRef < RBC_MAX_OUTDEGREE);
  ((Rbc_t**)(f -> gRef))[(f -> iRef)++] = sd -> result;

  return;

} /* End of ShiftBack. */


/*!
  \brief Dfs LastVisit for shifting.

  Dfs LastVisit for shifting.

  \se None
*/

static void
ShiftLast(Rbc_t  * f,
          char   * shiftData,
          nusmv_ptrint sign)
{
  ShiftDfsData_t * sd   = (ShiftDfsData_t*)shiftData;
  Rbc_t         ** sons = (Rbc_t**)(f -> gRef);

  const NuSMVEnv_ptr env = Rbc_ManagerGetEnvironment(sd -> rbcManager);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (f -> symbol == RBCVAR) {
    /* Variables need to be shifted */
    sd -> result =
      Rbc_GetIthVar(sd -> rbcManager, (int)(sd -> shift)+PTR_TO_INT(f ->data));
  } else {
    /* Substitutions may trigger simplifications. */
    if (f -> symbol == RBCAND)
      sd->result = Rbc_MakeAnd(sd -> rbcManager, sons[0], sons[1], RBC_TRUE);
    else if (f -> symbol == RBCIFF)
      sd->result = Rbc_MakeIff(sd -> rbcManager, sons[0], sons[1], RBC_TRUE);
    else if (f -> symbol == RBCITE)
      sd->result = Rbc_MakeIte(sd->rbcManager, sons[0], sons[1], sons[2], RBC_TRUE);
    else
      ErrorMgr_internal_error(errmgr, "ShiftLast: unknown RBC symbol");
    /* Clean the temporary sons list. */
    FREE(sons);
  }

  /* Adjust the sign and leave the result in the gRef too. */
  f -> gRef = (char*)(sd -> result);
  sd -> result = RbcId(sd -> result, sign);

  return;

} /* End of ShiftLast. */


/*!
  \brief Dfs LastVisit for logical shifting.

  Dfs LastVisit for logical shifting.

  \se None
*/

static void LogicalShiftLast(Rbc_t* f, char* shiftData, nusmv_ptrint sign)
{
  ShiftDfsData_t * sd   = (ShiftDfsData_t*)shiftData;
  Rbc_t         ** sons = (Rbc_t**)(f -> gRef);

  const NuSMVEnv_ptr env = Rbc_ManagerGetEnvironment(sd -> rbcManager);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));


  if (f -> symbol == RBCVAR) {
    /* Variables need to be shifted. Passes throught the levels phy to
   log, shifts, and then passes again down from log to phy: */
    sd -> result = Rbc_GetIthVar(sd->rbcManager,
     sd->log2phy[sd->phy2log[PTR_TO_INT(f->data)] + sd->shift]);
  }
  else {
    /* Substitutions may trigger simplifications. */
    if (f -> symbol == RBCAND)
      sd->result = Rbc_MakeAnd(sd -> rbcManager, sons[0], sons[1], RBC_TRUE);
    else if (f -> symbol == RBCIFF)
      sd->result = Rbc_MakeIff(sd -> rbcManager, sons[0], sons[1], RBC_TRUE);
    else if (f -> symbol == RBCITE)
      sd->result = Rbc_MakeIte(sd->rbcManager, sons[0], sons[1], sons[2],
             RBC_TRUE);
    else {
      ErrorMgr_internal_error(errmgr, "LogicalShiftLast: unknown RBC symbol");
    }
    /* Clean the temporary sons list. */
    FREE(sons);
  }

  /* Adjust the sign and leave the result in the gRef too. */
  f -> gRef = (char*)(sd -> result);
  sd -> result = RbcId(sd -> result, sign);

  return;
} /* End of LogicalShiftLast. */


/*!
  \brief Dfs Set for substitution (variables to formulas).

  Dfs Set for substitution (variables to formulas).

  \se None
*/

static int
SubstRbcSet(Rbc_t  * f,
            char   * SubstRbcData,
            nusmv_ptrint sign)
{
  SubstRbcDfsData_t * sd = (SubstRbcDfsData_t*)SubstRbcData;

  /* Set the current general reference as result. */
  sd -> result = RbcId((Rbc_t*)(f -> gRef), sign);

  /* All nodes should be visited once and only once. */
  return (0);

} /* End of SubstRbcSet. */


/*!
  \brief Dfs FirstVisit for substitution (variables to formulas).

  Dfs FirstVisit for substitution (variables to formulas).

  \se None
*/

static void
SubstRbcFirst(Rbc_t  * f,
              char   * SubstRbcData,
              nusmv_ptrint sign)
{

  /* Create a temporary list (use vertex own general reference). */
  if (f -> symbol != RBCVAR) {
    f -> gRef = (char*)ALLOC(Rbc_t*, RBC_MAX_OUTDEGREE);
    f -> iRef = 0;
  }

  return;

} /* End of SubstRbcFirst. */


/*!
  \brief Dfs BackVisit for substRbcitution.

  Dfs BackVisit for substRbcitution.

  \se None
*/

static void
SubstRbcBack(Rbc_t  * f,
             char   * SubstRbcData,
             nusmv_ptrint sign)
{
  SubstRbcDfsData_t * sd   = (SubstRbcDfsData_t*)SubstRbcData;

  /* Get the current result and add it to the temporary list. */
  nusmv_assert(f -> iRef < RBC_MAX_OUTDEGREE);
  ((Rbc_t**)(f -> gRef))[(f -> iRef)++] = sd -> result;

  return;

} /* End of SubstRbcBack. */


/*!
  \brief Dfs LastVisit for SubstRbcitution.

  Dfs LastVisit for SubstRbcitution.

  \se None
*/

static void
SubstRbcLast(Rbc_t  * f,
             char   * SubstRbcData,
             nusmv_ptrint sign)
{
  SubstRbcDfsData_t * sd   = (SubstRbcDfsData_t*)SubstRbcData;
  Rbc_t         ** sons = (Rbc_t**)(f -> gRef);

  const NuSMVEnv_ptr env = Rbc_ManagerGetEnvironment(sd -> rbcManager);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (f -> symbol == RBCVAR) {
    /* Variables need to be substituted by formulas. */
    sd->result = sd->substRbc[PTR_TO_INT(f->data)];
  } else {
    /* Substitutions may trigger simplifications. */
    if (f -> symbol == RBCAND)
      sd->result = Rbc_MakeAnd(sd -> rbcManager, sons[0], sons[1], RBC_TRUE);
    else if (f -> symbol == RBCIFF)
      sd->result = Rbc_MakeIff(sd -> rbcManager, sons[0], sons[1], RBC_TRUE);
    else if (f -> symbol == RBCITE)
      sd->result = Rbc_MakeIte(sd -> rbcManager, sons[0], sons[1], sons[2],
             RBC_TRUE);
    else {
      ErrorMgr_internal_error(errmgr, "SubstRbcLast: unknown RBC symbol");
    }
    /* Clean the temporary sons list. */
    FREE(sons);
  }

  /* Adjust the sign and leave the result in the gRef too. */
  f -> gRef = (char*)(sd -> result);
  sd -> result = RbcId(sd -> result, sign);

  return;

} /* End of SubstRbcLast. */


/*!
  \brief Dfs LastVisit for logical Rbc Substitution.

  Dfs LastVisit for logical Rbc Substitution.

  \se None
*/

static void
LogicalSubstRbcLast(Rbc_t* f, char* SubstRbcData, nusmv_ptrint sign)
{
  SubstRbcDfsData_t * sd   = (SubstRbcDfsData_t*)SubstRbcData;
  Rbc_t         ** sons = (Rbc_t**)(f -> gRef);

  const NuSMVEnv_ptr env = Rbc_ManagerGetEnvironment(sd -> rbcManager);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (f -> symbol == RBCVAR) {
    /* Variables need to be subsituted by formulas. */
    sd -> result = sd->substRbc[sd->phy2log[PTR_TO_INT(f -> data)]];
  }
  else {
    /* Substitutions may trigger simplifications. */
    if (f -> symbol == RBCAND)
      sd->result = Rbc_MakeAnd(sd -> rbcManager, sons[0], sons[1], RBC_TRUE);
    else if (f -> symbol == RBCIFF)
      sd->result = Rbc_MakeIff(sd -> rbcManager, sons[0], sons[1], RBC_TRUE);
    else if (f -> symbol == RBCITE)
      sd->result = Rbc_MakeIte(sd -> rbcManager, sons[0], sons[1], sons[2],
                               RBC_TRUE);
    else {
      ErrorMgr_internal_error(errmgr, "LogicalSubstRbcLast: unknown RBC symbol");
    }
    /* Clean the temporary sons list. */
    FREE(sons);
  }

  /* Adjust the sign and leave the result in the gRef too. */
  f -> gRef = (char*)(sd -> result);
  sd -> result = RbcId(sd -> result, sign);

  return;

} /* End of LogicalSubstRbcLast. */
