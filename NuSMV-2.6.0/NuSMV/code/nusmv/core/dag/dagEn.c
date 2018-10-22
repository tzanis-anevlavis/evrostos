/* ---------------------------------------------------------------------------


  This file is part of the ``dag'' package of NuSMV version 2.
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
  \author \todo: Missing author
  \brief The routine constructs an ennary dag corresponding to an arbitrary
               binary dag.

  External procedures included in this module:
        <ul>
                <li> <b>Dag_Dfs()<b> Generic depth first search engine.
        </ul>

*/


#include "nusmv/core/dag/dag.h"
#include "nusmv/core/dag/dagInt.h"

#include "nusmv/core/bmc/bmcBmc.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef struct EnData EnData_t;


/*---------------------------------------------------------------------------*/
/* Stucture declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief Structure used for internal purposes.

  The fields are:
                 <ul>
                 <li> fatherNum, number of node's fathers
                 <li> enDualNode,node corrisponding to the corrent node in
                      the ennary graph
                 <li> dfs2Visit, mark used to avoid repeating the same visit
                 </ul>

  SeeAlso       []
*****************************************************************************
*/


struct EnData {
  int           fatherNum;
  int           dfs2Visit;
  Dag_Vertex_t* enDualNode;
};


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief Returns the pointer with bit annotation

  The annotation bit is filtered to 0. The result is the pointer
               purified from the bit annotation.The annotation bit is forced
               to 1 or 0 according to the result of bitwise-or with S.

  \se none
*/
#define SignedEdge(E,S) (Dag_Vertex_t *) (((nusmv_ptrint)(EnDualNode(E))&(~DAG_ANNOTATION_BIT))|S)

/*!
  \brief Select a particular field of the struct associated via gRef
               to each node

  

  \se none
*/
#define FatherNum(N)  ((EnData_t*)((N)->gRef))->fatherNum

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define EnDualNode(N) ((EnData_t*)((N)->gRef))->enDualNode

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define Dfs2Visit(N)  ((EnData_t*)((N)->gRef))->dfs2Visit


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static Dag_Vertex_t* DFS1(Dag_Vertex_t* v, nusmv_ptrint vBit);
static void          DFS2(Dag_Vertex_t* v, nusmv_ptrint vBit, lsList sons,
			  int father_symbol);

static lsList getEnnarySons(Dag_Vertex_t* v,nusmv_ptrint sign);
static int Compare( Dag_Vertex_t* v,lsList list,nusmv_ptrint vBit);

static int  SetupSet(Dag_Vertex_t* f, char * visData, nusmv_ptrint sign);
static void doNothingAndReturnVoid(Dag_Vertex_t* f, char * visData, nusmv_ptrint sign) {};

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of external functions                                          */
/*---------------------------------------------------------------------------*/


/*!
  \brief Constructs an ennary dag corrispondig to an arbitrary binary dag
               and returns a reference to it.

  The parameters are:
               <ul>
               <li> dfsRoot, the dag vertex of the binary dag to transform into
                    ennary dag
               </ul>
               

  \se none
*/


Dag_Vertex_t*
Dag_Ennarize (Dag_Vertex_t* dfsRoot)
{
 Dag_Vertex_t* enRoot = NIL(Dag_Vertex_t);

 if (dfsRoot != NIL(Dag_Vertex_t)) {

     Dag_DfsFunctions_t setupDFS;

     setupDFS.FirstVisit =
     setupDFS.BackVisit  =
     setupDFS.LastVisit  = doNothingAndReturnVoid;
     setupDFS.Set        = SetupSet;

     Dag_Dfs(dfsRoot, &dag_DfsClean, NIL(char));
     Dag_Dfs(dfsRoot, &setupDFS,     NIL(char));

     ++(Dag_VertexGetRef(dfsRoot) -> dag -> dfsCode);

     enRoot = DFS1(Dag_VertexGetRef(dfsRoot),  Dag_VertexIsSet(dfsRoot));
 }

 return enRoot;
}/* End of Rbc_Ennarize. */


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Performs a generic (recursive) DFS on the dag.

  The parameters are:
               <ul>
               <li> v, the current dag vertex
               <li> vBit, the incoming link annotation (0 or not-0)
               </ul>

  \se none
*/


static Dag_Vertex_t*
DFS1(Dag_Vertex_t* v, nusmv_ptrint vBit)
{
  lsGen gen;
  lsList sonsList;
  Dag_Vertex_t * vSon;

  if ( (v -> visit != v -> dag -> dfsCode)) {

      lsList enSonsList = lsCreate();
      v -> visit = v -> dag -> dfsCode;

      if ((sonsList = getEnnarySons(v,vBit)) != (lsList) NULL) {

          gen = lsStart(sonsList);

          while (lsNext(gen, (lsGeneric*) &vSon, LS_NH) == LS_OK) {
              Dag_Vertex_t* enSon = DFS1(Dag_VertexGetRef(vSon), Dag_VertexIsSet(vSon));
              (void) lsNewEnd(enSonsList, (lsGeneric) enSon, LS_NH);
          }
          lsFinish(gen);
      }

      EnDualNode(v) = Dag_VertexLookup(v->dag, v->symbol, v->data, enSonsList);
  }

 return SignedEdge(v,vBit);

} /* End of DFS1. */



/*!
  \brief Finds all sons of a node and returns the list containing them.

  The parameters are:
               <ul>
               <li> v, the current dag vertex
               <li> sign, the incoming link annotation (0 or not-0)
               </ul>

  \se None
*/

static lsList
getEnnarySons(Dag_Vertex_t * v, nusmv_ptrint sign)
{
    lsList sons = (lsList) NULL;
    lsGen gen;
    Dag_Vertex_t * vSon;

    if (v -> outList != (lsList) NULL) {
        sons = lsCreate();
        gen = lsStart(v -> outList);

        while (lsNext(gen, (lsGeneric*) &vSon, LS_NH) == LS_OK)
            DFS2(Dag_VertexGetRef(vSon), Dag_VertexIsSet(vSon),sons,v->symbol);

        lsFinish(gen);
    }

    return sons;
}/* End of getEnnarySons. */



/*!
  \brief Performs a generic (recursive) DFS on the dag.

  The parameters are:
               <ul>
               <li> v, the current dag vertex
               <li> vBit, the incoming link annotation (0 or not-0)
               <li> sons, the list of sons of the corrent dag vertex
               <li> father_symbol, the integer code of the father vertex
                    of the corrent dag vertex
               </ul>

  \se None
*/

static void
DFS2( Dag_Vertex_t * v, nusmv_ptrint vBit, lsList sons, int father_symbol)
{
 lsGen           gen;
 Dag_Vertex_t * vSon;

 if (vBit!=0                    || /* edge is negated */
     v->symbol != father_symbol || /* symbol changes moving to the son */
     FatherNum(v)>1) {             /* the node has more than one father */

     Dfs2Visit(v) = (v -> dag -> dfsCode);

     if (!Compare (v,sons,vBit))
         (void) lsNewEnd(sons, (lsGeneric)(((nusmv_ptrint)(v)&(~DAG_ANNOTATION_BIT))|vBit), LS_NH);
     return;
 }

 if ( (Dfs2Visit(v) != v -> dag -> dfsCode)) {

     Dfs2Visit(v) = (v -> dag -> dfsCode);

     /* Visit each son (if any). */
     if (v -> outList != (lsList) NULL) {

         gen = lsStart(v -> outList);
         while (lsNext(gen, (lsGeneric*) &vSon, LS_NH) == LS_OK)
             DFS2(Dag_VertexGetRef(vSon), Dag_VertexIsSet(vSon),sons,v->symbol);

         lsFinish(gen);
     }
 }

return;

} /* End of DFS2. */


/*!
  \brief Checks if an element belongs to a list.

  The parameters are:
               <ul>
               <li> v, the current dag vertex
               <li> list, the list of sons of the corrent dag vertex
               <li> vBit, the incoming link annotation (0 or not-0)
               </ul>

  \se None
*/


static int
Compare(Dag_Vertex_t* v, lsList list, nusmv_ptrint vBit)
{
    lsGen gen;
    Dag_Vertex_t* h;

    gen = lsStart(list);
    while (lsNext(gen, (lsGeneric*) &h, LS_NH) == LS_OK)
        if ((lsGeneric)(((nusmv_ptrint)(v)&(~DAG_ANNOTATION_BIT))|vBit)==(lsGeneric)(((nusmv_ptrint)(h)&(~DAG_ANNOTATION_BIT))|Dag_VertexIsSet(h)) )
            return (1);

    lsFinish(gen);
    return (0);
}/* End of Compare. */




/*!
  \brief Dfs Set for ennarization.

  Dfs Set for ennarization.

  \se None
*/


static int SetupSet(Dag_Vertex_t* f, char * visData, nusmv_ptrint sign)
{
 if (f->gRef == (char*)NULL) {
     f->iRef = 0;
     f->gRef = (char *)ALLOC(EnData_t,1);
     FatherNum(f) = 0;
 }

 FatherNum(f)++;

 return (0);
}/* End of SetupSet. */


