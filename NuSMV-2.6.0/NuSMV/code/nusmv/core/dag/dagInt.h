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
  \author Armando Tacchella and Tommi Junttila
  \brief Directed acyclic graphs with sharing.

  Internal functions and data structures of the dag package.

*/


#ifndef __NUSMV_CORE_DAG_DAG_INT_H__
#define __NUSMV_CORE_DAG_DAG_INT_H__

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/dag/dag.h"


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define DAGMAX_WORDS  ((int) 10)

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define DAGWORD_SIZE  ((int) (NUSMV_SIZEOF_VOID_P * 4))


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Stucture declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief DAG manager.

  Holds the vertices of a dag:
                 <ul>
                 <li> vTable, the vertices hash (maintains uniqueness);
                 <li> gcList, the free list (candidates for GC);
                 <li> dfsCode, initially 0 is the code of the current DFS;
                 <li> stats, for bookkeeping.
                 </ul>
*/

struct DagManager {
  st_table     * vTable;
  int            hashFn[DAGMAX_WORDS];
  lsList         gcList;
  int            dfsCode;

  int            stats[DAG_MAX_STAT];

  Dag_DfsFunctions_t* dag_DfsClean;
};

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \brief Vertex initialization.

  Performs several tasks:
               <ul>
               <li> connects the vertex to the sons by increasing the sons'
                    marks
               <li> removes sons from the free list if their mark
                    is increased to one for the first time;
               <li> clears the vertex mark and stores the vertex in the 
                    free list;
               <li> clears other internal fields.
               </ul>

  \se none
*/
void DagVertexInit(Dag_Manager_t * dagManager, Dag_Vertex_t * v);

/*!
  \brief Compare two vertices.

  Gets two vertex pointers v1, v2, (as char pointers) and
               compares the symbol, the generic data reference and the
               pointers to the sons. Returns -1 if v1 < v2, 0 if v1 =
               v2 and 1 if v1 > v2, in lexicographic order of fields.

  \se None
*/
int DagVertexComp(const char * v1, const char * v2);

/*!
  \brief Calculate the hash key of a vertex.

  Calculate a preliminary index as follows:
                  v -> symbol                            + 
                  8 low order bits of (int) (v -> data)  +
                 16 low order bits of each son up to MAXSON +
                  1 for each son whose edge is annotated
               Return the modulus of the index and the actual hash size.

  \se None
*/
int DagVertexHash(char * v, int modulus);

/**AutomaticEnd***************************************************************/

#endif /* __NUSMV_CORE_DAG_DAG_INT_H__ */
