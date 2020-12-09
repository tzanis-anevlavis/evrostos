/* ---------------------------------------------------------------------------


  This file is part of the ``compile'' package.
  %COPYRIGHT%


-----------------------------------------------------------------------------*/

/*!
  \author Michele Dorigatti
  \brief Package initialization and deinitialization functions

  \todo: Missing description

*/

#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/cinit/NuSMVEnv.h"
#include "nusmv/core/fsm/FsmBuilder.h"
#include "nusmv/core/compile/symb_table/symb_table.h"
#include "nusmv/core/compile/flattening/flatteningPkg.h"
#include "nusmv/core/compile/dependency/dependencyPkg.h"
#include "nusmv/core/utils/Logger.h"

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


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Compile_init(NuSMVEnv_ptr env)
{
  FsmBuilder_ptr builder;

  cmps = cmp_struct_init();

  builder = FsmBuilder_create(env);
  NuSMVEnv_set_value(env, ENV_FSM_BUILDER, builder);

  /* modules and sub packages initialization */
  SymbTablePkg_init(env);
  FlatteningPkg_init(env);
  DependencyPkg_init(env);
}

void Compile_quit(NuSMVEnv_ptr env)
{
  OptsHandler_ptr opt = NuSMVEnv_get_value(env, ENV_OPTS_HANDLER);
  Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));

  if (opt_verbose_level_gt(opt, 2)) {
    Logger_log(logger, "Shutting down the compiler...\n");
  }

  if (NuSMVEnv_has_value(env, ENV_BDD_FSM)) {
    BddFsm_ptr bdd_fsm = BDD_FSM(NuSMVEnv_remove_value(env, ENV_BDD_FSM));

    if (opt_verbose_level_gt(opt, 1)) {
      Logger_log(logger, "Forcing destruction of previoulsy created BDD FSM...\n");
    }

    BddFsm_destroy(bdd_fsm);
  }

  if (NuSMVEnv_has_value(env, ENV_BOOL_FSM)) {
    BoolSexpFsm_ptr bool_sexp_fsm =
      BOOL_SEXP_FSM(NuSMVEnv_remove_value(env, ENV_BOOL_FSM));

    if (opt_verbose_level_gt(opt, 1)) {
      Logger_log(logger, "Forcing destruction of previoulsy created BOOL FSM...\n");
    }

    BoolSexpFsm_destroy(bool_sexp_fsm);
  }

  if (NuSMVEnv_has_value(env, ENV_SEXP_FSM)) {
    SexpFsm_ptr sexp_fsm =
      SEXP_FSM(NuSMVEnv_remove_value(env, ENV_SEXP_FSM));

    if (opt_verbose_level_gt(opt, 1)) {
      Logger_log(logger, "Forcing destruction of previoulsy created SEXP FSM...\n");
    }

    SexpFsm_destroy(sexp_fsm);
  }

  if (NuSMVEnv_has_value(env, ENV_FLAT_HIERARCHY)) {
    FlatHierarchy_ptr fh = NuSMVEnv_remove_value(env, ENV_FLAT_HIERARCHY);
    FlatHierarchy_destroy(fh);
  }

  /* local structures: */
  if (NuSMVEnv_has_value(env, ENV_FSM_BUILDER)) {
    FsmBuilder_ptr fb = NuSMVEnv_remove_value(env, ENV_FSM_BUILDER);
    FsmBuilder_destroy(fb);
  }

  /* modules and sub packages initialization */
  DependencyPkg_quit(env);
  FlatteningPkg_quit(env);
  SymbTablePkg_quit(env);
  CompileFlatten_quit_flattener(env);

  cmp_struct_quit(cmps);
  cmps = (cmp_struct_ptr) NULL;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
