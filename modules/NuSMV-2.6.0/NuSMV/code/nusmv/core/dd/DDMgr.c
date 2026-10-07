/* ---------------------------------------------------------------------------


  This file is part of the ``dd'' package of NuSMV version 2.
  Copyright (C) 2011 by FBK-irst.

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
  \author Alessandro Mariotti
  \brief Implementation of class 'DDMgr'

  \todo: Missing description

*/


#include "nusmv/core/dd/DDMgr.h"
#include "nusmv/core/dd/DDMgr_private.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/ErrorMgr.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'DDMgr_private.h' for class 'DDMgr' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define common_error(err, variable, message)                      \
  if (NULL == (variable)) {                                       \
    ErrorMgr_rpterr(errmgr, "%s", message);                       \
    ErrorMgr_nusmv_exit(errmgr, 1);                               \
  }


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void dd_manager_finalize(Object_ptr object, void* dummy);

static int dd_manager_print_node_fun(DdManager* dd, FILE* output,
                                     node_ptr node, void* arg);

static char* dd_manager_sprint_node_fun(DdManager* dd,
                                        node_ptr node, void* arg);

static void dd_manager_type_error_fun(DdManager* dd, FILE* output,
                                      node_ptr node, void* arg);

static void dd_manager_fatal_error_fun(DdManager* dd, FILE* output,
                                       const char* msg, void* arg);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

DDMgr_ptr DDMgr_create(const NuSMVEnv_ptr env)
{
  DDMgr_ptr self = ALLOC(DDMgr, 1);
  DD_MGR_CHECK_INSTANCE(self);

  dd_manager_init(self, env);
  return self;
}

void DDMgr_destroy(DDMgr_ptr self)
{
  DD_MGR_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

DdManager* DDMgr_get_dd_manager(const DDMgr_ptr self)
{
  return self->dd;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void dd_manager_init(DDMgr_ptr self, const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const ExprMgr_ptr exprs =
    EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  /* base class initialization */
  env_object_init(ENV_OBJECT(self), env);

  self->dd = Cudd_Init(0, /* numVars */
                       0, /* numVarsZ */
                       UNIQUE_SLOTS, /* numSlots */
                       CACHE_SLOTS,  /* cacheSize */
                       0, /* maxMemory */
                       ExprMgr_number(exprs, 0),
                       ExprMgr_number(exprs, 1),
                       ExprMgr_false(exprs),
                       ExprMgr_true(exprs),
                       dd_manager_print_node_fun,
                       dd_manager_sprint_node_fun,
                       dd_manager_type_error_fun,
                       dd_manager_fatal_error_fun,
                       env);

  common_error(errmgr, self->dd,
               "init_dd_package: Unable to initialize the manager.");

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = dd_manager_finalize;

  /* for example, to override a base class' virtual method: */
  /*OVERRIDE(EnvObject, virtual_method) = dd_manager_virtual_method;*/
}

void dd_manager_deinit(DDMgr_ptr self)
{
  /* members deinitialization */

  Cudd_Quit(self->dd);

  /* base class deinitialization */
  env_object_deinit(ENV_OBJECT(self));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The DDMgr class virtual finalizer

  Called by the class destructor
*/
static void dd_manager_finalize(Object_ptr object, void* dummy)
{
  DDMgr_ptr self = DD_MGR(object);

  dd_manager_deinit(self);
  FREE(self);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static int dd_manager_print_node_fun(DdManager* dd, FILE* output,
                                      node_ptr node, void* arg)
{
  const NuSMVEnv_ptr env = NUSMV_ENV(arg);
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  return print_node(wffprint, output, node);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static char* dd_manager_sprint_node_fun(DdManager* dd,
                                        node_ptr node, void* arg)
{
  const NuSMVEnv_ptr env = NUSMV_ENV(arg);
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  return sprint_node(wffprint, node);
}

/* TODO[AMa] Streams used by errmgr and var. output may be different */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void dd_manager_type_error_fun(DdManager* dd, FILE* output,
                                      node_ptr node, void* arg)
{
  const NuSMVEnv_ptr env = NUSMV_ENV(arg);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  ErrorMgr_start_parsing_err(errmgr);
  fprintf(output,"\ntype error: value = ");
  print_node(wffprint, output, node);
  fprintf(output,"\nExpected a boolean expression\n");
  ErrorMgr_finish_parsing_err(errmgr);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void dd_manager_fatal_error_fun(DdManager* dd, FILE* output,
                                       const char* msg, void* arg)
{
  const NuSMVEnv_ptr env = NUSMV_ENV(arg);
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  ErrorMgr_start_parsing_err(errmgr);
  fprintf(output ,"\nFatal error: %s\n", msg);
  ErrorMgr_finish_parsing_err(errmgr);
}

/**AutomaticEnd***************************************************************/

