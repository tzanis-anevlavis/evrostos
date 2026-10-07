/* ---------------------------------------------------------------------------


  This file is part of the ``dag'' package of NuSMV version 2.
  Copyright (C) 2000-2001 by University of Genova.
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
  \author Armando Tacchella, Michele Dorigatti
  \brief \todo: Missing synopsis

  \todo: Missing description

*/

#include "nusmv/core/dag/dag.h"
#include "nusmv/core/dag/dagInt.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define MAX_DEGREE 10000

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define MAX_DEPTH  100000

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef struct Statistics Statistics_t;
typedef struct StatData StatData_t;

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/
/*!
  \brief 

  
*/

struct Statistics {
  int degree_stat    [MAX_DEGREE];
  int depth_stat     [MAX_DEPTH];
  int var_depth_stat [MAX_DEPTH];
  int nodes_num;
};

/*!
  \brief 

  
*/

struct StatData{
  int fatherNum;
  int depth;
  int knownDepthFatherNum;
};

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief Makes the code more readable

  Makes the code more readable
*/
#define FatherNum(N)           ((StatData_t*)((N)->gRef))->fatherNum

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define KnownDepthFatherNum(N) ((StatData_t*)((N)->gRef))->knownDepthFatherNum

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define Depth(N)               ((StatData_t*)((N)->gRef))->depth

/**AutomaticStart*************************************************************/
/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void
doNothingAndReturnVoid(Dag_Vertex_t* f, char* visData, nusmv_ptrint sign);

static int
doNothingAndReturnZero(Dag_Vertex_t* f, char * visData, nusmv_ptrint sign);

static void ResetStat(Statistics_t* stat);

static int
ComputeFatherAndSonNum(Dag_Vertex_t* f, char * visData, nusmv_ptrint sign);

static void ComputeDepth(Dag_Vertex_t* v, int p_depth, Statistics_t* stat);

static void _PrintStat(Statistics_t* stat, FILE* statFile, char* prefix);

/**AutomaticEnd***************************************************************/
/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void PrintStat(Dag_Vertex_t* dfsRoot, FILE* statFile, char* prefix)
{
  if (dfsRoot != (Dag_Vertex_t*)NULL) {

    Dag_DfsFunctions_t fathnsonDFS, statDFS;
    Statistics_t stat;
    ResetStat(&stat);

    fathnsonDFS.FirstVisit = statDFS.FirstVisit =
      fathnsonDFS.BackVisit  = statDFS.BackVisit  =
      fathnsonDFS.LastVisit  = statDFS.LastVisit  = (PF_VPVPCPI)doNothingAndReturnVoid;

    fathnsonDFS.Set = (PF_IVPCPI)ComputeFatherAndSonNum;
    statDFS.Set     = (PF_IVPCPI)doNothingAndReturnZero;

    Dag_Dfs     (dfsRoot, Dag_ManagerGetDfsCleanFun(dfsRoot->dag),   NIL(char));
    Dag_Dfs     (dfsRoot, &fathnsonDFS, (char*)&stat);
    ComputeDepth(dfsRoot, 0           ,        &stat);
    Dag_Dfs     (dfsRoot, &statDFS    , (char*)&stat);

    _PrintStat(&stat, statFile, prefix);
  }
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
/*!
  \brief Dfs function doing nothing

  Dfs function doing nothing
*/

static void
doNothingAndReturnVoid(Dag_Vertex_t* f, char* visData, nusmv_ptrint sign)
{}

/*!
  \brief Dfs function returning zero

  Dfs function returning zero
*/

static int
doNothingAndReturnZero(Dag_Vertex_t* f, char * visData, nusmv_ptrint sign)
{ return 0; }

/*!
  \brief Reset the statistics data

  Reset the statistics data
*/

static void ResetStat(Statistics_t* stat)
{
  int i;

  stat->nodes_num = 0;

  for (i=0;i<MAX_DEGREE; i++)
    (stat->degree_stat)[i]=0;

  for (i=0;i<MAX_DEPTH; i++)
    (stat->var_depth_stat)[i]=(stat->depth_stat)[i]=0;
}

/*!
  \brief Dfs function

  Dfs function
*/

static int
ComputeFatherAndSonNum(Dag_Vertex_t* f, char * visData, nusmv_ptrint sign)
{
  if (f->gRef == (char*)NULL) {

    f->gRef = (char *)ALLOC(StatData_t,1);
    KnownDepthFatherNum(f) = FatherNum(f) = Depth(f) = 0;

    (((Statistics_t*)visData)->
      degree_stat[((f->outList)==NULL) ? 0 : f->numSons])++;
  }

  FatherNum(f)++;
  return (0);
}

/*!
  \brief Dfs function

  Dfs function
*/

static void ComputeDepth(Dag_Vertex_t* v, int p_depth, Statistics_t* stat)
{
  unsigned gen;
  Dag_Vertex_t* vSon;

  v = Dag_VertexGetRef(v);

  Depth(v) = MAX(p_depth,Depth(v));

  if ((++(KnownDepthFatherNum(v))) == FatherNum(v)) {

     ((stat->depth_stat)[Depth(v)])++;

    if (v -> outList != (Dag_Vertex_t**) NULL) {
      if ((v->numSons)==0) {
         ((stat->var_depth_stat)[Depth(v)])++;
      }

      for (gen=0; gen<v->numSons; gen++) {
        vSon = v->outList[gen];

        ComputeDepth(vSon, Depth(v)+1,stat);
      }
    }
    else {
       ((stat->var_depth_stat)[Depth(v)])++;
    }
  }

  return;
}

/*!
  \brief Print out the number of nodes by degree and depth

  Print these data:
                      1. Total nodes per number of children;
                      2. Total nodes and total leaves per depth.

  \se data are appended to statFile

  \sa PrintStat()
*/

static void _PrintStat(Statistics_t* stat, FILE* statFile, char* prefix)
{
  int i;

  for (i=0;i<MAX_DEGREE; i++)
    if ((stat->degree_stat)[i]>0)
      fprintf(statFile,
              "%s Nodes with %i sons: %i\n",
              prefix,
              i,
              (stat->degree_stat)[i]);

  for (i=0;i<MAX_DEPTH; i++)
    if ((stat->depth_stat)[i]>0)
      fprintf(statFile,
              "%s Nodes at depth %i: %i, leaves among them: %i\n",
              prefix,
              i,
              (stat->depth_stat)[i],
              (stat->var_depth_stat)[i]);
}

