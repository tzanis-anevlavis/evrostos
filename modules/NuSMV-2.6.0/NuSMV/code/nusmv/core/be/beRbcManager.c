/* ---------------------------------------------------------------------------


  This file is part of the ``be'' package of NuSMV version 2.
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
  \brief Implementation for the RBC-based Boolean Expressions module.

  This implementation is a wrapper for the RBC structure.

*/


#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/be/beRbcManager.h"
#include "nusmv/core/be/be.h"
#include "nusmv/core/be/beInt.h"

#include "nusmv/core/rbc/rbc.h"
#include "nusmv/core/rbc/InlineResult.h"

#include "nusmv/core/opt/opt.h"
#include "nusmv/core/utils/defs.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

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

/*!
  \brief Given a be_manager returns the contained rbc manager.

  This is a macro which can be used to simplify the code.
*/
#define GET_RBC_MGR(be_manager) \
  (Rbc_Manager_t*) be_manager->spec_manager

/*!
  \brief Converts a rbc into a be

  This is a macro which can be used to simplify the code.

  \sa RBC
*/
#define BE(be_manager, spec) \
  be_manager->be2spec_converter(be_manager, (void*)spec)

/*!
  \brief Converts a be into a rbc

  This is a macro which can be used to simplify the code.

  \sa BE
*/
#define RBC(be_manager, be) \
  (Rbc_t*) be_manager->spec2be_converter(be_manager, be)



/*---------------------------------------------------------------------------*/
/* Declarations of internal functions                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void* beRbc_Be2Rbc(Be_Manager_ptr mgr, be_ptr be);
static be_ptr beRbc_Rbc2Be(Be_Manager_ptr mgr, void* rbc);


/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

Be_Manager_ptr Be_RbcManager_Create(const NuSMVEnv_ptr env, const size_t capacity)
{
  Rbc_Manager_t* spec = Rbc_ManagerAlloc(env, capacity);
  Be_Manager_ptr self = Be_Manager_Create(env, spec, &beRbc_Rbc2Be, &beRbc_Be2Rbc);
  return self;
}

void Be_RbcManager_Delete(Be_Manager_ptr self)
{
  Rbc_ManagerFree((Rbc_Manager_t*) self->spec_manager);
  Be_Manager_Delete(self);
}

void Be_RbcManager_Reserve(Be_Manager_ptr self, const size_t size)
{
  Rbc_ManagerReserve((Rbc_Manager_t*)self->spec_manager, size);
}

void Be_RbcManager_Reset (const Be_Manager_ptr self)
{
  Rbc_ManagerReset((Rbc_Manager_t*)self->spec_manager);
}

boolean Be_IsTrue(Be_Manager_ptr manager, be_ptr arg)
{
  return (arg == Be_Truth(manager))? true : false;
}

boolean Be_IsFalse(Be_Manager_ptr manager, be_ptr arg)
{
  return (arg == Be_Falsity(manager))? true : false;
}

boolean Be_IsConstant(Be_Manager_ptr manager, be_ptr arg)
{
  return (Be_IsTrue(manager, arg) || Be_IsFalse(manager, arg));
}

be_ptr Be_Truth(Be_Manager_ptr manager)
{
  return BE(manager, Rbc_GetOne(GET_RBC_MGR(manager)));
}

be_ptr Be_Falsity(Be_Manager_ptr manager)
{
  return BE(manager, Rbc_GetZero(GET_RBC_MGR(manager)));
}

be_ptr Be_Not(Be_Manager_ptr manager, be_ptr left)
{
  return BE( manager, Rbc_MakeNot(GET_RBC_MGR(manager), RBC(manager, left)) );
}

be_ptr Be_And(Be_Manager_ptr manager, be_ptr left, be_ptr right)
{
  return BE(manager, Rbc_MakeAnd(GET_RBC_MGR(manager),
                                 RBC(manager, left),
                                 RBC(manager, right),
                                 RBC_TRUE));
}

be_ptr Be_Or(Be_Manager_ptr manager, be_ptr left, be_ptr right)
{
  return BE(manager, Rbc_MakeOr(GET_RBC_MGR(manager),
                                RBC(manager, left),
                                RBC(manager, right),
                                RBC_TRUE));
}

be_ptr Be_Xor(Be_Manager_ptr manager, be_ptr left, be_ptr right)
{
  return BE(manager, Rbc_MakeXor(GET_RBC_MGR(manager),
                                 RBC(manager, left),
                                 RBC(manager, right),
                                 RBC_TRUE));
}

be_ptr Be_Implies(Be_Manager_ptr manager, be_ptr arg1, be_ptr arg2)
{
  /* (a -> b) <-> !(a & !b) <-> (!a | b) */
  return Be_Or(manager, Be_Not(manager, arg1), arg2);
}

be_ptr Be_Iff(Be_Manager_ptr manager, be_ptr left, be_ptr right)
{
  return BE(manager, Rbc_MakeIff(GET_RBC_MGR(manager),
                                 RBC(manager, left),
                                 RBC(manager, right),
                                 RBC_TRUE));
}

be_ptr Be_Ite(Be_Manager_ptr manager, be_ptr c, be_ptr t, be_ptr e)
{
  return BE( manager, Rbc_MakeIte(GET_RBC_MGR(manager),
                                  RBC(manager, c),
                                  RBC(manager, t),
                                  RBC(manager, e),
                                  RBC_TRUE) );
}

be_ptr Be_LogicalShiftVar(Be_Manager_ptr manager, be_ptr f, int shift,
                          const int* log2phy, const int* phy2log)
{
  /* lazy evaluation: */
  if (Be_IsConstant(manager, f)) return f;

  return BE(manager,
            Rbc_LogicalShift(GET_RBC_MGR(manager),
                             RBC(manager, f), shift,
                             log2phy, phy2log));
}

be_ptr Be_LogicalVarSubst(Be_Manager_ptr manager, be_ptr f, int* subst,
                          const int* log2phy, const int* phy2log)
{
  return BE( manager, Rbc_LogicalSubst(GET_RBC_MGR(manager), RBC(manager, f),
                                       subst, log2phy, phy2log) );
}

Be_Cnf_ptr Be_ConvertToCnf(Be_Manager_ptr manager, be_ptr f, int polarity,
                           Be_CnfAlgorithm alg)
{
  const NuSMVEnv_ptr env = Be_Manager_GetEnvironment(manager);
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  Be_Cnf_ptr cnf;
  int max_var_idx;
  int literalAssignedToWholeFormula = INT_MIN;

  /* performs the cnf conversion: */
  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "\nConverting the BE problem into CNF problem...\n");
  }

  cnf = Be_Cnf_Create(f);
  max_var_idx = Rbc_Convert2Cnf(GET_RBC_MGR(manager),
                                RBC(manager, f),
                                polarity, alg,
                                Be_Cnf_GetClausesList(cnf),
                                Be_Cnf_GetVarsList(cnf),
                                &literalAssignedToWholeFormula);

  nusmv_assert(literalAssignedToWholeFormula >= INT_MIN);

  Be_Cnf_RemoveDuplicateLiterals(cnf);

  Be_Cnf_SetMaxVarIndex(cnf, max_var_idx);

  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, " Conversion returned maximum variable index = %d\n",
            Be_Cnf_GetMaxVarIndex(cnf));
    Logger_log(logger, " Length of list of clauses = %" PRIuPTR "\n",
            Be_Cnf_GetClausesNumber(cnf));
    Logger_log(logger, " Length of list of variables = %" PRIuPTR "\n",
            Be_Cnf_GetVarsNumber(cnf));
  }

  Be_Cnf_SetFormulaLiteral(cnf, literalAssignedToWholeFormula);
  return cnf;
}

Slist_ptr Be_CnfModelToBeModel(Be_Manager_ptr manager, Slist_ptr cnfModel)
{
  Slist_ptr beModel = Slist_create();
  nusmv_ptrint cnfLiteral, beLiteral;
  Siter iter;

  SLIST_FOREACH(cnfModel, iter) {
    cnfLiteral = (nusmv_ptrint) Siter_element(iter);

    beLiteral = (nusmv_ptrint) Be_CnfLiteral2BeLiteral(manager, cnfLiteral);
    /* if there is corresponding rbc variable => remember it */
    if (0 != beLiteral) {
      Slist_push(beModel, (void*)beLiteral);
    }
  }

  return beModel;
}

int Be_CnfLiteral2BeLiteral(const Be_Manager_ptr self, int cnfLiteral)
{
  int cnfIndex;
  int rbcIndex;

  /* literal is always != 0, otherwise the sign cannot be represented. */
  nusmv_assert(0 != cnfLiteral);

  cnfIndex = abs(cnfLiteral);
  rbcIndex = Rbc_CnfVar2RbcIndex(GET_RBC_MGR(self), cnfIndex);

  if (-1 != rbcIndex) return (cnfLiteral > 0) ? (rbcIndex+1) : (-rbcIndex-1);
  else return 0;
}

int Be_BeLiteral2CnfLiteral(const Be_Manager_ptr self, int beLiteral)
{
  int be_idx = Be_BeLiteral2BeIndex(self, beLiteral);
  return (beLiteral > 0) ?
    Be_BeIndex2CnfLiteral(self, be_idx) :
    -Be_BeIndex2CnfLiteral(self, be_idx);
}

int Be_BeLiteral2BeIndex(const Be_Manager_ptr self, int beLiteral)
{
  nusmv_assert(beLiteral != 0);
  return abs(beLiteral)-1;
}

int Be_BeIndex2BeLiteral(const Be_Manager_ptr self, int beIndex)
{
  nusmv_assert(beIndex >= 0);
  return beIndex+1;
}

int Be_BeIndex2CnfLiteral(const Be_Manager_ptr self, int beIndex)
{
  return Rbc_RbcIndex2CnfVar(GET_RBC_MGR(self), beIndex);
}

void Be_DumpDavinci(Be_Manager_ptr manager, be_ptr f, FILE* outFile)
{
  Rbc_OutputDaVinci(GET_RBC_MGR(manager), RBC(manager, f), outFile);
}

void Be_DumpGdl(Be_Manager_ptr manager, be_ptr f, FILE* outFile)
{
  Rbc_OutputGdl(GET_RBC_MGR(manager), RBC(manager, f), outFile);
}

void Be_DumpSexpr(Be_Manager_ptr manager, be_ptr f, FILE* outFile)
{
  Rbc_OutputSexpr(GET_RBC_MGR(manager), RBC(manager, f), outFile);
}

be_ptr Be_Index2Var(Be_Manager_ptr manager, int varIndex)
{
  return BE(manager, Rbc_GetIthVar(GET_RBC_MGR(manager), varIndex));
}

int Be_Var2Index(Be_Manager_ptr manager, be_ptr var)
{
  return Rbc_GetVarIndex( RBC(manager, var) );
}

void Be_PrintStats(Be_Manager_ptr manager, int clustSize, FILE* outFile)
{
  Rbc_PrintStats(GET_RBC_MGR(manager), clustSize, outFile);
}

boolean Be_CnfLiteral_IsSignPositive(const Be_Manager_ptr self, int cnfLiteral)
{
  nusmv_assert(cnfLiteral != 0);
  return cnfLiteral > 0;
}

int Be_CnfLiteral_Negate(const Be_Manager_ptr self, int cnfLiteral)
{
  nusmv_assert(cnfLiteral != 0);
  return -cnfLiteral;
}

boolean Be_BeLiteral_IsSignPositive(const Be_Manager_ptr self, int beLiteral)
{
  nusmv_assert(beLiteral != 0);
  return beLiteral > 0;
}

int Be_BeLiteral_Negate(const Be_Manager_ptr self, int beLiteral)
{
  nusmv_assert(beLiteral != 0);
  return -beLiteral;
}

be_ptr Be_apply_inlining(Be_Manager_ptr mgr, be_ptr f, boolean add_conj)
{
  be_ptr res;
  InlineResult_ptr ir;

  /* lazy evaluation: */
  if (Be_IsConstant(mgr, f))
    return f;

  ir = RbcInline_apply_inlining(GET_RBC_MGR(mgr), RBC(mgr, f));

  if (add_conj)
    res = BE(mgr, InlineResult_get_inlined_f_and_c(ir));
  else
    res = BE(mgr, InlineResult_get_inlined_f(ir));

  InlineResult_destroy(ir);
  return res;
}


/*---------------------------------------------------------------------------*/
/* Definitions of internal functions                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definitions of static functions                                           */
/*---------------------------------------------------------------------------*/

/*!
  \brief Converts a be into a rbc

  The current implementation is really a simple type renaming.
  Internally used by Be_Manager via the inheritance mechanism.
*/

static void* beRbc_Be2Rbc(Be_Manager_ptr mgr, be_ptr be)
{
  return (Rbc_t*) be;
}


/*!
  \brief Converts a rbc into a be

  The current implementation is really a simple type renaming.
  Internally used by Be_Manager via the inheritance mechanism.
*/

static be_ptr beRbc_Rbc2Be(Be_Manager_ptr mgr, void* rbc)
{
  return (be_ptr) rbc;
}
