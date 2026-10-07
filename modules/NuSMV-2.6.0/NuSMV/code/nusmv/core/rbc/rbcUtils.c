/* ---------------------------------------------------------------------------


  This file is part of the ``rbc'' package of NuSMV version 2. 
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
  \author Roberto Cavada
  \brief Some general functions working on RBCs

  \todo: Missing description

*/


#include "nusmv/core/rbc/rbcInt.h"

#include "nusmv/core/dag/dag.h"
#include "nusmv/core/utils/Slist.h"


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Stucture declarations                                                     */
/*---------------------------------------------------------------------------*/


/*!
  \brief Data passing in inlining-DFS 

  Data passing in inlining-DFS 
*/

typedef struct DepDfsData_TAG {
  Rbc_Manager_t* mgr;
  Slist_ptr list;
} DepDfsData;


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

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
static int rbc_dep_set(Rbc_t* f, char* _data, nusmv_ptrint sign);
static void rbc_dep_first(Rbc_t* f, char* _data, nusmv_ptrint sign);
static void rbc_dep_back(Rbc_t* f, char* _data, nusmv_ptrint sign);
static void rbc_dep_last(Rbc_t* f, char* _data, nusmv_ptrint sign);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Rbc_pkg_init()
{
}

void Rbc_pkg_quit()
{
}

Slist_ptr RbcUtils_get_dependencies(Rbc_Manager_t* rbcManager, Rbc_t* f,
                                    boolean reset_dag)
{
  Dag_DfsFunctions_t funcs;
  DepDfsData data;

  /* lazy evaluation */
  if ((f == rbcManager->one) || (f == rbcManager->zero)) {
    return Slist_create();
  }

  /* clears the user fields. */
  if (reset_dag) { Dag_Dfs(f, Rbc_ManagerGetDfsCleanFun(rbcManager), (char*) NULL); }

  /* sets up the DFS functions */
  funcs.Set        = (PF_IVPCPI)rbc_dep_set;
  funcs.FirstVisit = (PF_VPVPCPI)rbc_dep_first;
  funcs.BackVisit  = (PF_VPVPCPI)rbc_dep_back;
  funcs.LastVisit  = (PF_VPVPCPI)rbc_dep_last;

  /* sets up the DFS data */
  data.mgr = rbcManager;
  data.list = Slist_create();

  /* Calling DFS on f. */
  Dag_Dfs(f, &funcs, (char*)(&data));

  /* processes result */
  return data.list;
}

void Rbc_Dfs_exported(Rbc_t* rbc,
                      RbcDfsFunctions_t* dfsFun,
                      void* dfsData,
                      Rbc_Manager_t* rbc_manager)
{
  Dag_Dfs((Dag_Vertex_t*)rbc, dfsFun, (char*)dfsData);
}

void Rbc_Dfs_clean_exported(Rbc_t* rbc, Rbc_Manager_t* rbc_manager)
{
  Dag_Dfs(rbc, Rbc_ManagerGetDfsCleanFun(rbc_manager), (char*)NULL);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/


/*!
  \brief 

  

  \se None
*/

static int rbc_dep_set(Rbc_t* f, char* _data, nusmv_ptrint sign)
{ return 0; }
static void rbc_dep_first(Rbc_t* f, char* _data, nusmv_ptrint sign)
{}
static void rbc_dep_back(Rbc_t* f, char* _data, nusmv_ptrint sign)
{}


/*!
  \brief 

  

  \se None
*/

static void rbc_dep_last(Rbc_t* f, char* _data, nusmv_ptrint sign)
{
  DepDfsData* data = (DepDfsData*) _data;

  if (f->symbol == RBCVAR) {
    Slist_push(data->list, (void *) f);
  }
}
