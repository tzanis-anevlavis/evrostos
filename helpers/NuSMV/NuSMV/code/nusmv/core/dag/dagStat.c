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
  \brief DAG manager statistics.

  External procedures included in this module:
		<ul>
		<li> <b>Dag_GetStats()</b> Get statistics;
		<li> <b>Dag_PrintStats()</b> Print statistics;
		</ul>

*/


#include <math.h>

#include "nusmv/core/dag/dagInt.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Stucture declarations                                                     */
/*---------------------------------------------------------------------------*/


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


/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of external functions                                          */
/*---------------------------------------------------------------------------*/

void
Dag_PrintStats(
  Dag_Manager_t * dagManager,            
  int             clustSz,
  FILE          * outFile)
{

  int               min, max, runSz, totClust;
  int               i, j = 0;
  st_table_entry *  b;

  int               shared = 0;
  float             total = 0.0;
  float             variance = 0.0;
  float             mean = 0.0;
  int               numBins = dagManager -> vTable -> num_bins;
  st_table_entry ** theHash = dagManager -> vTable -> bins;

  /* First pass: calculating the total, shared vertices and (eventually) 
     printing the bin's data. */
  runSz = clustSz;
  totClust = 0;
  for (i = 0; i < numBins; i++) {
    /* Scan each bin entries. */
    for (j = 0, b = theHash[i]; b != NIL(st_table_entry); b = b -> next, j++) {
      /* For each entry, check if the vertex is shared. */
      if (((Dag_Vertex_t*)(b -> key)) -> mark > 1) {
	shared += 1;
      }
    }
    total += (float)j;
    totClust += j;
    runSz -= 1;
    if (runSz == 0) {
      /* clustSz bins were visited, print out the information. */
      fprintf(outFile, "%6d\n", totClust);
      runSz = clustSz;
      totClust = 0;
    }
  } 
  if (clustSz > 0) {
    fprintf(outFile, "%6d\n", totClust);
  }
  mean = total / (float)numBins;

  /* Min and max initialized to the last value read in the first pass. */
  min = max = j;

  /* Second pass: variance, min and max. */
  for (i = 0; i < numBins; i++) {
    for (j = 0, b = theHash[i]; b != NIL(st_table_entry); b = b -> next, j++);
    /* [MD] pow takes double */
    variance = pow(((double)j - (double)mean), (double)2.0) + (double)variance;
    if (j > max) {
      max = j;
    } 
    if (j < min) {
      min = j;
    }
  } 
  variance = variance / (float)(numBins - 1);

  fprintf(outFile, "Mean     %10.3f\n", (double)mean);
  fprintf(outFile, "Variance %10.3f\n", (double)variance);
  fprintf(outFile, "Min      %10d\n", min);
  fprintf(outFile, "Max      %10d\n", max); 

  fprintf(outFile, "Total    %10.0f\n", (double)total);
  fprintf(outFile, "Shared   %10d\n", shared);

  return;

} /* End of Dag_PrintStats. */


