/* ---------------------------------------------------------------------------


  This file is part of the ``bmc'' package of NuSMV version 2.
  Copyright (C) 2000-2001 by FBK-irst and University of Trento.

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
  \brief Some useful functions to check propositional formulae.
  Temporary located into the <tt>bmc</tt> package

  \todo: Missing description

*/

#include "nusmv/core/bmc/bmcInt.h"   /* for 'options' */
#include "nusmv/core/bmc/bmcCheck.h"

#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/error.h"

#include "nusmv/core/parser/symbols.h"  /* for constants */
#include "nusmv/core/wff/lr/MasterLogicRecognizer.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define MAX_MATCHES 2048

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
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


static int bmc_is_propositional_formula_aux(const NuSMVEnv_ptr env,
                                            node_ptr wff, int index,
                                            void* pOpt);

static int bmc_check_if_wff_is_valid(const NuSMVEnv_ptr env,
                                     node_ptr wff, int index,
                                     void* aiIndexes);

static void bmc_add_valid_wff_to_list(const NuSMVEnv_ptr env,
                                      node_ptr wff, int index,
                                      void* list);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

node_ptr Bmc_CheckFairnessListForPropositionalFormulae(const NuSMVEnv_ptr env,
                                                       node_ptr wffList)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  unsigned int aiOffendingWffIdxs[MAX_MATCHES];
  char szNumber[6];
  char szBuffer[MAX_MATCHES * (sizeof(szNumber)+2)];

  int iMatches;
  int i;
  char szSingleMatch[]="Warning!\n  One offending fairness formula contains one or more temporal operators.\n  The offending formula has been found at index [%s] into the fairness list.\n  (The first formula is located at index zero.)\n  The invalid formula will be ignored.\n";
  char szMultipleMatches[]="Warning!\n  %d offending fairness formulae contain one or more temporal operators.\n  The offending formulae have been found at indexes [%s] into the fairness list.\n  (The first formula is located at index zero.)\n  All invalid formulae will be ignored.\n";

  node_ptr list_valid_wff=Nil;
  memset(szBuffer, 0, sizeof(szBuffer));
  memset(szNumber, 0, sizeof(szNumber));

  iMatches = Bmc_WffListMatchProperty(env, wffList,
            &bmc_is_propositional_formula_aux,
            (void*)NULL, /* no check opt args */
            -1, /*search for all occurrences */
            aiOffendingWffIdxs,
            NULL, /* no answer */
            (void*)NULL
          );
  /* prepare output string: */
  for (i=0; i<iMatches; ++i) {
    int chars = snprintf(szNumber, 6, "%d", aiOffendingWffIdxs[i]);
    SNPRINTF_CHECK(chars, 6);

    strcat(szBuffer, szNumber);
    if ( i<(iMatches-1) ) {
      /* not the last index: */
      strcat(szBuffer, ", ");
    }
  }


  /* prepare list of valid wff only: */
  (void)Bmc_WffListMatchProperty(env, wffList,
                                 &bmc_check_if_wff_is_valid,
                                 aiOffendingWffIdxs, /* par for checking fun */
                                 -1, /*search for all occurrences */
                                 NULL, /* no matched index array required */
                                 &bmc_add_valid_wff_to_list,
                                 &list_valid_wff
                                 );

  /* reverse list to restore correct order: */
  list_valid_wff = reverse(list_valid_wff);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    if (iMatches > 0 ) {
      /* szBuffer must contain invalid formula's index: */
      nusmv_assert(strlen(szBuffer)>0);

      if (iMatches>1) {
        Logger_log(logger, szMultipleMatches, iMatches, szBuffer);
      }
      else {
        Logger_log(logger, szSingleMatch, szBuffer);
      }
    }
  }

  return list_valid_wff;
}

int Bmc_WffListMatchProperty(const NuSMVEnv_ptr env,
                             node_ptr wffList,
                             BMC_PF_MATCH pCheck,
                             void* pCheckOptArgument,
                             int  iMaxMatches,
                             unsigned int* aiMatchedIndexes,
                             BMC_PF_MATCH_ANSWER pAnswer,
                             void* pAnswerOptArgument)
{
  int iMatchesAvail;
  int index=0;
  node_ptr wff=Nil;
  node_ptr wffList_iterator=wffList;

  if (iMaxMatches == -1) {
    iMaxMatches = MAX_MATCHES-1;
  }

  /* index array size is limited to MAX_MATCHES-1, and no other negative
     values but -1 are allowed for the iMaxMatched parameter */
  nusmv_assert((iMaxMatches>=0) && (iMaxMatches < MAX_MATCHES));

  iMatchesAvail=iMaxMatches;

  while ( (iMatchesAvail>0) && (wffList_iterator != Nil) ) {
    wff = car(wffList_iterator);
    nusmv_assert(wff != Nil);
    if (pCheck(env, wff, index, pCheckOptArgument) == 0) {
      /* here wff matches searching criteria: */
      if (aiMatchedIndexes != NULL) {
        aiMatchedIndexes[iMaxMatches - iMatchesAvail] = index;
      }

      if (pAnswer != NULL) pAnswer(env, wff, index, pAnswerOptArgument);
      --iMatchesAvail;
    }

    /* continue with the next list element: */
    ++index;
    wffList_iterator = cdr(wffList_iterator);
  } /* end of while cycle */

  /* sign the end of aiMatchedIndexes with a terminator: */
  if (aiMatchedIndexes != NULL) {
    aiMatchedIndexes[iMaxMatches-iMatchesAvail] = -1;
  }


  return (iMaxMatches - iMatchesAvail);
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Useful wrapper for
  Bmc_CheckFairnessListForPropositionalFormulae

  Wrapper that makes
  Bmc_CheckFairnessListForPropositionalFormulae able to call
  MasterLogicRecognizer_recognize with a mode generic interface.
  Arguments 2 and 3 are practically unused, supplied to respect the generic
  interface only.

  \sa Bmc_CheckFairnessListForPropositionalFormulae
*/
static int
bmc_is_propositional_formula_aux(const NuSMVEnv_ptr env, node_ptr wff, int index, void* pOpt)
{
  MasterLogicRecognizer_ptr const master_recogn =
     MASTER_LOGIC_RECOGNIZER(NuSMVEnv_get_value(env, ENV_MASTER_LOGIC_RECOGNIZER));
  boolean retval = false;
  LogicType logic = EXP_NONE;

  UNUSED_PARAM(index);
  UNUSED_PARAM(pOpt);
  
  logic = MasterLogicRecognizer_recognize(master_recogn, wff, Nil);
  retval = (logic == EXP_SIMPLE);

  return retval;
}

/*!
  \brief private service for
  Bmc_CheckFairnessListForPropositionalFormulae

  

  \sa Bmc_CheckFairnessListForPropositionalFormulae
*/
static int
bmc_check_if_wff_is_valid(const NuSMVEnv_ptr env,
                          node_ptr wff, int index, void* _aiIndexes)
{
  int i=0;
  int bInvalid=0;
  int* aiIndexes=(int*)_aiIndexes;

  /* search into ordered array of invalid wff's indexes the index of wff.
     If found return 0 (wff is invalid), else return 1 */
  while ( (aiIndexes[i] != -1) || (aiIndexes[i]>index) ) {
    if (aiIndexes[i] == index) {
      bInvalid = 1;
      break;
    }
    ++i;
  }

  return bInvalid;
}

/*!
  \brief private service for
  Bmc_CheckFairnessListForPropositionalFormulae

  

  \sa Bmc_CheckFairnessListForPropositionalFormulae
*/
static void bmc_add_valid_wff_to_list(const NuSMVEnv_ptr env,
                                      node_ptr wff, int index, void* _pList)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr* pList=(node_ptr*)_pList;
  *pList = cons(nodemgr, wff, *pList );
}

