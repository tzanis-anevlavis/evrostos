/* ---------------------------------------------------------------------------


  This file is part of the ``sat'' package of NuSMV version 2.
  Copyright (C) 2004 by FBK-irst.

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
  \author Andrei Tchaltsev, Roberto Cavada
  \brief Sat module

  \todo: Missing description

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/sat/satInt.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/utils.h"

#if NUSMV_HAVE_SOLVER_ZCHAFF
#include "nusmv/core/sat/solvers/SatZchaff.h"
#endif

#if NUSMV_HAVE_SOLVER_MINISAT
#include "nusmv/core/sat/solvers/SatMinisat.h"
#endif

#include <string.h>

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ZCHAFF_NAME  "ZChaff"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define MINISAT_NAME "MiniSat"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/**Variable*******************************************************************
  Synopsis           [The sat solvers names NuSMV provides]
  Description        []
  SideEffects        []
  SeeAlso            []
******************************************************************************/
static const char* sat_solver_names[] = {
# if NUSMV_HAVE_SOLVER_ZCHAFF
  ZCHAFF_NAME
# endif
#if NUSMV_HAVE_SOLVER_MINISAT
#if NUSMV_HAVE_SOLVER_ZCHAFF
  ,MINISAT_NAME
#else
  MINISAT_NAME
#endif
#endif
};
/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief Returns the number of element in a statically allocated array

  
*/
#define GET_ARRAY_LENGTH(array) \
  (sizeof(array)/sizeof(array[0]))


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

SatSolver_ptr Sat_CreateNonIncSolver(const NuSMVEnv_ptr env, const char* satSolver)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  SatSolver_ptr solver = SAT_SOLVER(NULL);

  nusmv_assert(satSolver != (char*) NULL);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Creating a SAT solver instance '%s' ...\n",
            satSolver);
  }

  if (strcasecmp(ZCHAFF_NAME, satSolver) == 0) {
# if NUSMV_HAVE_SOLVER_ZCHAFF
    solver = SAT_SOLVER(SatZchaff_create(env, ZCHAFF_NAME));
# endif

  } else if (strcasecmp(MINISAT_NAME, satSolver) == 0) {
# if NUSMV_HAVE_SOLVER_MINISAT
    solver = SAT_SOLVER(SatMinisat_create(env, MINISAT_NAME, false)); /* no proof logging */
# endif
  }

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    if (solver != SAT_SOLVER(NULL)) {
      Logger_log(logger, "Created an SAT solver instance '%s'\n",
              satSolver);
    }
    else {
      Logger_log(logger, "Failed: '%s' is not available\n", satSolver);
    }
  }

  return solver;
}

SatSolver_ptr Sat_CreateNonIncProofSolver(const NuSMVEnv_ptr env,
                                          const char* satSolver)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  SatSolver_ptr solver = SAT_SOLVER(NULL);

  nusmv_assert(satSolver != (char*) NULL);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Creating a SAT solver instance '%s' ...\n",
            satSolver);
  }

  if (strcasecmp(ZCHAFF_NAME, satSolver) == 0) {
# if NUSMV_HAVE_SOLVER_ZCHAFF
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
    ErrorMgr_internal_error(errmgr, "Proof logging not supported when using the zchaff "
                   " SAT Solver. Please retry using MiniSat");
    solver = SAT_SOLVER(SatZchaff_create(env, ZCHAFF_NAME));
# endif

  } else if (strcasecmp(MINISAT_NAME, satSolver) == 0) {
# if NUSMV_HAVE_SOLVER_MINISAT
    solver = SAT_SOLVER(SatMinisat_create(env, MINISAT_NAME, true));
# endif
  }

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    if (solver != SAT_SOLVER(NULL)) {
      Logger_log(logger, "Created an SAT solver instance '%s'\n",
              satSolver);
    }
    else {
      Logger_log(logger, "Failed: '%s' is not available\n", satSolver);
    }
  }

  return solver;
}

SatIncSolver_ptr Sat_CreateIncSolver(const NuSMVEnv_ptr env,
                                     const char* satSolver)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  SatIncSolver_ptr solver = SAT_INC_SOLVER(NULL);

  nusmv_assert(satSolver != (char*) NULL);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
            "Creating an incremental SAT solver instance '%s'...\n",
            satSolver);
  }

  if (strcasecmp(ZCHAFF_NAME, satSolver) == 0) {
# if NUSMV_HAVE_SOLVER_ZCHAFF
    solver = SAT_INC_SOLVER(SatZchaff_create(env, ZCHAFF_NAME));
# endif
  } else if (strcasecmp(MINISAT_NAME, satSolver) == 0) {
# if NUSMV_HAVE_SOLVER_MINISAT
    solver = SAT_INC_SOLVER(SatMinisat_create(env, MINISAT_NAME, false));
# endif
  }

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    if (solver != SAT_INC_SOLVER(NULL)) {
      Logger_log(logger, "Created an incremental SAT solver instance '%s'\n",
              satSolver);
    }
    else {
      Logger_log(logger, "Failed: '%s' is not available\n", satSolver);
    }
  }

  return solver;
}

SatIncSolver_ptr Sat_CreateIncProofSolver(const NuSMVEnv_ptr env,
                                          const char* satSolver)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  SatIncSolver_ptr solver = SAT_INC_SOLVER(NULL);

  nusmv_assert(satSolver != (char*) NULL);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
            "Creating an incremental SAT solver instance '%s'...\n",
            satSolver);
  }

  if (strcasecmp(ZCHAFF_NAME, satSolver) == 0) {
# if NUSMV_HAVE_SOLVER_ZCHAFF
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
    ErrorMgr_internal_error(errmgr, "Proof logging not supported when using the zchaff "
                   " SAT Solver. Please retry using MiniSat");
# endif
  } else if (strcasecmp(MINISAT_NAME, satSolver) == 0) {
# if NUSMV_HAVE_SOLVER_MINISAT
    solver = SAT_INC_SOLVER(SatMinisat_create(env, MINISAT_NAME, true));
# endif
  }

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    if (solver != SAT_INC_SOLVER(NULL)) {
      Logger_log(logger, "Created an incremental SAT solver instance '%s'\n",
              satSolver);
    }
    else {
      Logger_log(logger, "Failed: '%s' is not available\n", satSolver);
    }
  }

  return solver;
}

const char* Sat_NormalizeSatSolverName(const char* solverName)
{
  unsigned int i;
  for (i=0; i<GET_ARRAY_LENGTH(sat_solver_names); ++i) {
    if (strcasecmp(solverName, sat_solver_names[i]) == 0) {
      return sat_solver_names[i];
    }
  }

  return (const char*)NULL;
}

void Sat_PrintAvailableSolvers(FILE* file)
{
  int i;
  fprintf(file, "The available SAT solvers are: ");
  for (i=0; i<GET_ARRAY_LENGTH(sat_solver_names); ++i) {
    fprintf(file, "%s ", sat_solver_names[i]);
  }
  fprintf(file, "\n");
}

char* Sat_GetAvailableSolversString()
{
  char* solvers;
  int i;
  int all = 1;

  for (i=0; i<GET_ARRAY_LENGTH(sat_solver_names); ++i) {
    all += strlen(sat_solver_names[i]) + 1;
  }

  solvers = ALLOC(char, all);
  *solvers = '\0';

  for (i=0; i<GET_ARRAY_LENGTH(sat_solver_names); ++i) {
    char* tmp = ALLOC(char, strlen(solvers) + 1);
    sprintf(tmp, "%s", solvers);
    sprintf(solvers, "%s%s ", tmp, sat_solver_names[i]);
    FREE(tmp);
  }

  return solvers;
}

/**AutomaticEnd***************************************************************/
