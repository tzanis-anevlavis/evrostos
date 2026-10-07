/* ---------------------------------------------------------------------------


  This file is part of the ``dd'' package of NuSMV version 2.
  Copyright (C) 1998-2001 by CMU and FBK-irst.

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
  \author Marco Roveri
  \brief NuSMV interface to the Decision Diagram Package of the
  University of Colorado.

  This file implements the interface between the NuSMV
  system and the California University Decision Diagram (henceforth
  referred as CUDD). The CUDD package is a generic implementation of a
  decision diagram data structure. For the time being, only Boole
  expansion is implemented and the leaves in the in the nodes can be
  the constants zero, one or any arbitrary value. A coding standard
  has been defined. I.e all the functions acting on BDD and ADD have
  \"bdd\" and \"add\" respectively as prefix.
  <p><br>
  The BDD or ADD returned as a result of an operation are always
  referenced (see the CUDD User Manual for more details about this),
  and need to be dereferenced when the result is no more necessary to
  computation, in order to release the memory associated to it when
  garbage collection occurs.
  All the functions takes as first argument the decision diagram
  manager (henceforth referred as DdManager).

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/dd/ddInt.h"
#include "nusmv/core/enc/operators.h"
#include "nusmv/core/parser/symbols.h" /* for FAILURE value */
#include "nusmv/core/dd/DDMgr_private.h"
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
#define common_error(err, variable, message)                  \
  if ((variable) == NULL) {                                   \
    ErrorMgr_rpterr(err, "%s", message);                      \
    ErrorMgr_nusmv_exit(err, 1);                              \
  }

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define common_error2(err, dd, variable, variable2, message)          \
  if ((variable) == NULL) {                                           \
    ErrorMgr_rpterr(err, "%s", message);                              \
    Cudd_RecursiveDeref((dd),(variable2));                            \
    ErrorMgr_nusmv_exit(err, 1);                                      \
  }


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void InvalidType(FILE *file, char *field, char *expected);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Dd_init(NuSMVEnv_ptr env)
{
  DDMgr_ptr dd = DDMgr_create(env);
  VarsHandler_ptr dd_vars = VarsHandler_create(dd);

  NuSMVEnv_set_value(env, ENV_DD_MGR, dd);
  NuSMVEnv_set_value(env, ENV_DD_VARS_HANDLER, dd_vars);
}

void Dd_quit(NuSMVEnv_ptr env)
{
  DDMgr_ptr dd = NuSMVEnv_remove_value(env, ENV_DD_MGR);
  VarsHandler_ptr dd_vars = NuSMVEnv_remove_value(env, ENV_DD_VARS_HANDLER);

  VarsHandler_destroy(dd_vars);
  DDMgr_destroy(dd);
}

int dd_checkzeroref(DDMgr_ptr dd)
{
  return Cudd_CheckZeroRef(dd->dd);
} /* end of dd_checkzeroref */

int get_dd_nodes_allocated(DDMgr_ptr dd){
  return(Cudd_ReadKeys(dd->dd));
}

node_ptr map_dd(DDMgr_ptr dd, NPFDD f, node_ptr l)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr t;

  if (l == Nil) return(Nil);
  t = (*f)(dd, (DdNode *)car(l));
  return(cons(nodemgr, t,map_dd(dd, f, cdr(l))));
}

void walk_dd(DDMgr_ptr dd, VPFDD f, node_ptr l)
{
  if (l == Nil) return;
  (*f)(dd, (DdNode *)car(l));
  walk_dd(dd, f, cdr(l));
}

int dd_print_stats(NuSMVEnv_ptr env, DDMgr_ptr mgr, FILE *file)
{
  Cudd_PrintInfo(mgr->dd, file);

  /* Print some guidance to the parameters */
  fprintf(file, "\nMore detailed information about the semantics ");
  fprintf(file, "and values of these parameters\n");
  fprintf(file, "can be found in the documentation about the CU ");
  fprintf(file, "Decision Diagram Package.\n");

  return 0;
} /* end of dd_print_stats */

dd_block * dd_new_var_block(DDMgr_ptr dd, int start_index, int offset)
{
  MtrNode *group;

  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (start_index == MAX_VAR_INDEX) return(NULL);
  /*
    We use MTR_FIXED because we want the internal order of variablesa
    in the groups to be preserved (MTR_FIXED does not preserve them.).
  */
  group = Cudd_MakeTreeNode(dd->dd, start_index, offset, MTR_FIXED);
  common_error(errmgr, group, "dd_new_var_block: group = NULL");

  if (opt_verbose_level_gt(opts, 5)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "dd_new_var_block: low=%d, idx=%d, size=%d\n",
            group->low, group->index, group->size);
  }

  return((dd_block *) group);
} /* end of dd_new_var_block */

int dd_free_var_block(DDMgr_ptr dd, dd_block* group)
{

  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int res;

  if (opt_verbose_level_gt(opts, 5)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "dd_free_var_block: low=%d, idx=%d, size=%d\n",
            group->low, group->index, group->size);
  }

  if (Mtr_DissolveGroup((MtrNode*) group) == (MtrNode*) NULL) res = 1;
  else res = 0;

  return res;
}

int dd_get_index_at_level(DDMgr_ptr dd, int level)
{
  int result;

  result = Cudd_ReadInvPerm(dd->dd, level);
  if (result < 0) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    ErrorMgr_rpterr(errmgr, "dd_get_index_at_level: level %d out of bound.", level);
    ErrorMgr_nusmv_exit(errmgr, 1);
  }
  return(result);
} /* end of dd_get_index_at_level */

int dd_get_level_at_index(DDMgr_ptr dd, int index)
{
  int result;

  result = Cudd_ReadPerm(dd->dd, index);
  return result;
}

int dd_get_size(DDMgr_ptr dd)
{
  return Cudd_ReadSize(dd->dd);
} /* end of dd_get_size */

int dd_set_order(DDMgr_ptr dd, int* permutation)
{
  return Cudd_ShuffleHeap(dd->dd, permutation);
} /* end of dd_set_order */

void dd_autodyn_enable(DDMgr_ptr dd, dd_reorderingtype method)
{
  Cudd_AutodynEnable(dd->dd, method);
} /* end of dd_autodyn_enable */

void dd_autodyn_disable(DDMgr_ptr dd)
{
  Cudd_AutodynDisable(dd->dd);
} /* end of dd_autodyn_disable */

int dd_reordering_status(DDMgr_ptr dd, dd_reorderingtype * method)
{
  return(Cudd_ReorderingStatus(dd->dd, method));
} /* end of dd_reordering_status */

int dd_reorder(DDMgr_ptr dd, int method, int minsize)
{
  int result;

  result = Cudd_ReduceHeap(dd->dd, method, minsize);
  if (result == 0) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    ErrorMgr_rpterr(errmgr, "dd_reorder: reordering of ADD/BDD fails.");
    ErrorMgr_nusmv_exit(errmgr, 1);
  }
  return(result);
} /* end of dd_reorder */

int dd_get_reorderings(DDMgr_ptr dd)
{
  return Cudd_ReadReorderings(dd->dd);
}

dd_reorderingtype dd_get_ordering_method (DDMgr_ptr dd)
{
  dd_reorderingtype method;

  (void) Cudd_ReorderingStatus(dd->dd, &method);
  return(method);
}

int StringConvertToDynOrderType(char *string)
{

  if (strcmp("random", string) == 0) {
    return REORDER_RANDOM;
  }
  else if (strcmp("random_pivot", string) == 0) {
    return  REORDER_RANDOM_PIVOT;
  }
  else if (strcmp("sift", string) == 0) {
    return REORDER_SIFT;
  }
  else if (strcmp("sift_converge", string) == 0) {
    return  REORDER_SIFT_CONV;
  }
  else if (strcmp("symmetry_sift", string) == 0) {
    return  REORDER_SYMM_SIFT;
  }
  else if (strcmp("symmetry_sift_converge", string) == 0) {
    return  REORDER_SYMM_SIFT_CONV;
  }
  else if (strcmp("window2", string) == 0) {
    return REORDER_WINDOW2;
  }
  else if (strcmp("window3", string) == 0) {
    return  REORDER_WINDOW3;
  }
  else if (strcmp("window4", string) == 0) {
    return  REORDER_WINDOW4;
  }
  else if (strcmp("window2_converge", string) == 0) {
    return  REORDER_WINDOW2_CONV;
  }
  else if (strcmp("window3_converge", string) == 0) {
    return  REORDER_WINDOW3_CONV;
  }
  else if (strcmp("window4_converge", string) == 0) {
    return  REORDER_WINDOW4_CONV;
  }
  else if (strcmp("group_sift", string) == 0) {
    return  REORDER_GROUP_SIFT;
  }
  else if (strcmp("group_sift_converge", string) == 0) {
    return  REORDER_GROUP_SIFT_CONV;
  }
  else if (strcmp("annealing", string) == 0) {
    return  REORDER_ANNEALING;
  }
  else if (strcmp("genetic", string) == 0) {
    return  REORDER_GENETIC;
  }
  else if (strcmp("exact", string) == 0) {
    return  REORDER_EXACT;
  }
  else if (strcmp("linear", string) == 0) {
    return  REORDER_LINEAR;
  }
  else if (strcmp("linear_converge", string) == 0) {
    return  REORDER_LINEAR_CONV;
  }
  else if (strcmp("same", string) == 0) {
    return REORDER_SAME;
  }
  else {
    return REORDER_NONE;
  }
}

char * DynOrderTypeConvertToString(int method)
{
  if (method == REORDER_NONE) {
    return "";
  }
  else if (method == REORDER_RANDOM) {
    return "random";
  }
  else if (method == REORDER_RANDOM_PIVOT) {
    return "random_pivot";
  }
  else if ((method == REORDER_SIFT)) {
    return "sift";
  }
  else if (method == REORDER_SIFT_CONV) {
    return "sift_converge";
  }
  else if (method == REORDER_SYMM_SIFT) {
    return "symmetry_sift";
  }
  else if (method == REORDER_SYMM_SIFT_CONV) {
    return "symmetry_sift_converge";
  }
  else if (method == REORDER_WINDOW2) {
    return "window2";
  }
  else if (method == REORDER_WINDOW3) {
    return "window3";
  }
  else if (method == REORDER_WINDOW4) {
    return "window4";
  }
  else if (method == REORDER_WINDOW2_CONV) {
    return "window2_converge";
  }
  else if (method == REORDER_WINDOW3_CONV) {
    return "window3_converge";
  }
  else if (method == REORDER_WINDOW4_CONV) {
    return "window4_converge";
  }
  else if (method == REORDER_GROUP_SIFT) {
    return "group_sift";
  }
  else if (method == REORDER_GROUP_SIFT_CONV) {
    return "group_sift_converge";
  }
  else if (method == REORDER_ANNEALING) {
    return "annealing";
  }
  else if (method == REORDER_GENETIC) {
    return "genetic";
  }
  else if (method == REORDER_EXACT) {
    return "exact";
  }
  else if (method == REORDER_LINEAR) {
    return "linear";
  }
  else if (method == REORDER_LINEAR_CONV) {
    return "linear_converge";
  }
  else if (method == REORDER_SAME) {
    return "same";
  }
  else {
    fail("unrecognized method");
  }
}

int dd_set_parameters(DDMgr_ptr mgr, OptsHandler_ptr opt, FILE *file)
{
  int reorderMethod;
  dd_reorderingtype zddReorderMethod;
  st_table *newValueTable;
  st_generator *stgen;
  char *paramName;
  char *paramValue;

  /* Initial value of the variables. */
  reorderMethod = REORDER_SAME;
  zddReorderMethod = REORDER_SAME;

  /* Build a new table with the parameter names but with
  ** the prefix removed. */
  newValueTable = st_init_table(st_ptrcmp, st_ptrhash);
  OPTS_FOREACH_OPTION(opt, (char **)&paramName, (char **)&paramValue) {
    if (strncmp(paramName, "BDD.", 4) == 0) {
      st_insert(newValueTable, (char *)&paramName[4],
                (char *)paramValue);
    }
  }

  st_foreach_item(newValueTable, stgen, (char **)&paramName, (char **)&paramValue) {
    unsigned int uvalue;
    char *invalidChar;

    invalidChar = (char *)NULL;
    if (strcmp(paramName, "Hard limit for cache size") == 0) {
      uvalue = (unsigned int) strtol(paramValue, &invalidChar, 10);
      /* WARNING [MD] no check for the sign of the parameter is
         performed. Note that function strtoul can be used. */
      if (*invalidChar) {
        InvalidType(file, "Hard limit for cache size", "unsigned integer");
      }
      else {
        Cudd_SetMaxCacheHard(mgr->dd, uvalue);
      }
    }
    else if (strcmp(paramName, "Cache hit threshold for resizing") == 0) {
      uvalue = (unsigned int) strtol(paramValue, &invalidChar, 10);
      if (*invalidChar) {
                InvalidType(file, "Cache hit threshold for resizing",
                            "unsigned integer");
      }
      else {
        Cudd_SetMinHit(mgr->dd, uvalue);
      }
    }
    else if (strcmp(paramName, "Garbage collection enabled") == 0) {
      if (strcmp(paramValue, "yes") == 0) {
        Cudd_EnableGarbageCollection(mgr->dd);
      }
      else if (strcmp(paramValue, "no") == 0) {
        Cudd_DisableGarbageCollection(mgr->dd);
      }
      else {
        InvalidType(file, "Garbage collection enabled", "(yes,no)");
      }
    }
    else if (strcmp(paramName, "Limit for fast unique table growth") == 0) {
      uvalue = (unsigned int) strtol(paramValue, &invalidChar, 10);
      if (*invalidChar) {
        InvalidType(file, "Limit for fast unique table growth", "unsigned integer");
      }
      else {
        Cudd_SetLooseUpTo(mgr->dd, uvalue);
      }
    }
    else if (strcmp(paramName, "Maximum number of variables sifted per reordering")
             == 0) {
      uvalue = (unsigned int) strtol(paramValue, &invalidChar, 10);
      if (*invalidChar) {
        InvalidType(file, "Maximum number of variables sifted per reordering",
                    "unsigned integer");
      }
      else {
        Cudd_SetSiftMaxVar(mgr->dd, uvalue);
      }
    }
    else if (strcmp(paramName, "Maximum number of variable swaps per reordering")
             == 0) {
      uvalue = (unsigned int) strtol(paramValue, &invalidChar, 10);
      if (*invalidChar) {
        InvalidType(file, "Maximum number of variable swaps per reordering",
                    "unsigned integer");
      }
      else {
        Cudd_SetSiftMaxSwap(mgr->dd, uvalue);
      }
    }
    else if (strcmp(paramName,
                    "Maximum growth while sifting a variable") == 0) {
      double value;

      value = strtod(paramValue, &invalidChar);
      if (*invalidChar) {
        InvalidType(file, "Maximum growth while sifting a variable", "real");
      }
      else {
        Cudd_SetMaxGrowth(mgr->dd, value);
      }
    }
    else if (strcmp(paramName, "Dynamic reordering of BDDs enabled") == 0) {
      if (strcmp(paramValue, "yes") == 0) {
        Cudd_AutodynEnable(mgr->dd, reorderMethod);
      }
      else if (strcmp(paramValue, "no") == 0) {
        Cudd_AutodynDisable(mgr->dd);
      }
      else {
        InvalidType(file, "Dynamic reordering of BDDs enabled", "(yes,no)");
      }
    }
    else if (strcmp(paramName, "Default BDD reordering method") == 0) {
      dd_reorderingtype reorderInt;

      reorderMethod = (unsigned int) strtol(paramValue, &invalidChar, 10);
      if (*invalidChar || reorderMethod < 0) {
        InvalidType(file, "Default BDD reordering method", "integer");
      }
      else {
        if (Cudd_ReorderingStatus(mgr->dd, &reorderInt)) {
          Cudd_AutodynEnable(mgr->dd, reorderMethod);
        }
      }
    }
    else if (strcmp(paramName, "Dynamic reordering of ZDDs enabled") == 0) {
      if (strcmp(paramValue, "yes") == 0) {
        Cudd_AutodynEnableZdd(mgr->dd, zddReorderMethod);
      }
      else if (strcmp(paramValue, "no") == 0) {
        Cudd_AutodynDisableZdd(mgr->dd);
      }
      else {
        InvalidType(file, "Dynamic reordering of ZDDs enabled", "(yes,no)");
      }
    }
    else if (strcmp(paramName, "Default ZDD reordering method") == 0) {
      dd_reorderingtype reorderInt;

      zddReorderMethod = (unsigned int) strtol(paramValue, &invalidChar, 10);
      if (*invalidChar) {
        InvalidType(file, "Default ZDD reordering method", "integer");
      }
      else {
        if (Cudd_ReorderingStatusZdd(mgr->dd, &reorderInt)) {
          Cudd_AutodynEnableZdd(mgr->dd, zddReorderMethod);
        }
      }
    }
    else if (strcmp(paramName, "Realignment of ZDDs to BDDs enabled") == 0) {
      if (strcmp(paramValue, "yes") == 0) {
        Cudd_zddRealignEnable(mgr->dd);
      }
      else if (strcmp(paramValue, "no") == 0) {
        Cudd_zddRealignDisable(mgr->dd);
      }
      else {
        InvalidType(file, "Realignment of ZDDs to BDDs enabled", "(yes,no)");
      }
    }
    else if (strcmp(paramName, "Realignment of BDDs to ZDDs enabled") == 0) {
      if (strcmp(paramValue, "yes") == 0) {
        Cudd_bddRealignEnable(mgr->dd);
      }
      else if (strcmp(paramValue, "no") == 0) {
        Cudd_bddRealignDisable(mgr->dd);
      }
      else {
        InvalidType(file, "Realignment of BDDs to ZDDs enabled", "(yes,no)");
      }
    }
    else if (strcmp(paramName, "Dead nodes counted in triggering reordering") == 0) {
      if (strcmp(paramValue, "yes") == 0) {
        Cudd_TurnOnCountDead(mgr->dd);
      }
      else if (strcmp(paramValue, "no") == 0) {
        Cudd_TurnOffCountDead(mgr->dd);
      }
      else {
        InvalidType(file, "Dead nodes counted in triggering reordering", "(yes,no)");
      }
    }
    else if (strcmp(paramName, "Group checking criterion") == 0) {
      uvalue = (unsigned int) strtol(paramValue, &invalidChar, 10);
      if (*invalidChar) {
        InvalidType(file, "Group checking criterion", "integer");
      }
      else {
        Cudd_SetGroupcheck(mgr->dd, uvalue);
      }
    }
    else if (strcmp(paramName, "Recombination threshold") == 0) {
      uvalue = (unsigned int) strtol(paramValue, &invalidChar, 10);
      if (*invalidChar) {
        InvalidType(file, "Recombination threshold", "integer");
      }
      else {
        Cudd_SetRecomb(mgr->dd, uvalue);
      }
    }
    else if (strcmp(paramName, "Symmetry violation threshold") == 0) {
      uvalue = (unsigned int) strtol(paramValue, &invalidChar, 10);
      if (*invalidChar) {
        InvalidType(file, "Symmetry violation threshold", "integer");
      }
      else {
        Cudd_SetSymmviolation(mgr->dd, uvalue);
      }
    }
    else if (strcmp(paramName, "Arc violation threshold") == 0) {
      uvalue = (unsigned int) strtol(paramValue, &invalidChar, 10);
      if (*invalidChar) {
        InvalidType(file, "Arc violation threshold", "integer");
      }
      else {
        Cudd_SetArcviolation(mgr->dd, uvalue);
      }
    }
    else if (strcmp(paramName, "GA population size") == 0) {
      uvalue = (unsigned int) strtol(paramValue, &invalidChar, 10);
      if (*invalidChar ) {
        InvalidType(file, "GA population size", "integer");
      }
      else {
        Cudd_SetPopulationSize(mgr->dd, uvalue);
      }
    }
    else if (strcmp(paramName, "Number of crossovers for GA") == 0) {
      uvalue = (unsigned int) strtol(paramValue, &invalidChar, 10);
      if (*invalidChar) {
        InvalidType(file, "Number of crossovers for GA", "integer");
      }
      else {
        Cudd_SetNumberXovers(mgr->dd, uvalue);
      }
    }
    else {
      fprintf(file, "Warning: Parameter %s not recognized.", paramName);
      fprintf(file, " Ignored.\n");
    }
  } /* end of st_foreach_item */

  /* Clean up. */
  st_free_table(newValueTable);

  return(1);

} /* end of dd_set_parameters */

int dd_printminterm(
  DDMgr_ptr manager,
  dd_ptr node)
{
  return(Cudd_PrintMinterm(manager->dd, node));
}

int dd_dump_dot(
  DDMgr_ptr dd /* manager */,
  int  n /* number of output nodes to be dumped */,
  dd_ptr * f /* array of output nodes to be dumped */,
  const char ** inames /* array of input names (or NULL) */,
  const char ** onames /* array of output names (or NULL) */,
  FILE * fp /* pointer to the dump file */)
{
  return(Cudd_DumpDot(dd->dd, n, (DdNode **)f,
		      (char**) inames, (char**) onames, fp));
}

int dd_dump_davinci(
  DDMgr_ptr dd /* manager */,
  int  n /* number of output nodes to be dumped */,
  dd_ptr * f /* array of output nodes to be dumped */,
  const char ** inames /* array of input names (or NULL) */,
  const char ** onames /* array of output names (or NULL) */,
  FILE * fp /* pointer to the dump file */)
{
  return(Cudd_DumpDaVinci(dd->dd, n, (DdNode **)f,
			  (char**) inames, (char**) onames, fp));
}

add_ptr add_true(DDMgr_ptr dd)
{
  DdNode * result = Cudd_ReadTrue(dd->dd);

  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_then(DDMgr_ptr dd, add_ptr f)
{
  return((add_ptr)Cudd_T(f));
}

int add_index(DDMgr_ptr dd, add_ptr f)
{
  return(Cudd_NodeReadIndex((DdNode *)f));
}

add_ptr add_else(DDMgr_ptr dd, add_ptr f)
{
  return((add_ptr)Cudd_E(f));
}

add_ptr add_false(DDMgr_ptr dd)
{
  DdNode * result = Cudd_ReadFalse(dd->dd);

  Cudd_Ref(result);
  return((add_ptr)result);
}

int add_is_true(DDMgr_ptr dd, add_ptr f)
{
  return((DdNode *)f == Cudd_ReadTrue(dd->dd));
}

int add_is_false(DDMgr_ptr dd, add_ptr f)
{
  return((DdNode *)f == Cudd_ReadFalse(dd->dd));
}

add_ptr add_one(DDMgr_ptr dd)
{
  DdNode * result = Cudd_ReadOne(dd->dd);

  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_zero(DDMgr_ptr dd)
{
  DdNode * result = Cudd_ReadZero(dd->dd);

  Cudd_Ref(result);
  return((add_ptr)result);
}

int add_is_one(DDMgr_ptr dd, add_ptr f)
{
  return((DdNode *)f == Cudd_ReadOne(dd->dd));
}

int add_is_zero(DDMgr_ptr dd, add_ptr f)
{
  return((DdNode *)f == Cudd_ReadZero(dd->dd));
}

void add_ref(add_ptr fn)
{
  Cudd_Ref(fn);
}

void add_deref(add_ptr fn)
{
  Cudd_Deref(fn);
}

void add_free(DDMgr_ptr dd, add_ptr dd_node)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  common_error(errmgr, dd_node, "add_free: dd_node = NULL");
  Cudd_RecursiveDeref(dd->dd, (DdNode *)dd_node);
}

add_ptr add_dup(add_ptr dd_node)
{
  Cudd_Ref(dd_node);
  return(dd_node);
}

add_ptr add_leaf(DDMgr_ptr dd, node_ptr leaf_node)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_addConst(dd->dd,leaf_node);
  common_error(errmgr, result, "add_leaf: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

int add_isleaf(add_ptr dd_node)
{
  return(Cudd_IsConstant(dd_node));
}

int bdd_isleaf(bdd_ptr dd_node)
{
  return(Cudd_IsConstant(dd_node));
}

node_ptr add_get_leaf(DDMgr_ptr dd, add_ptr Leaf)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (!Cudd_IsConstant((DdNode *)Leaf)) {
    ErrorMgr_rpterr(errmgr, "add_get_leaf: not a leaf!");
    ErrorMgr_nusmv_exit(errmgr, 1);
  }
  return((node_ptr)Cudd_V(Leaf));
}

add_ptr add_build(DDMgr_ptr dd, int level, add_ptr t, add_ptr e)
{
  DdNode * result = NULL;

  if (t == e) return add_dup(t);

  /* A reorder might take place. In this case, keey retrying */
  while (result == NULL) {
    result = (DdNode *)cuddUniqueInter(dd->dd, level, (DdNode *)t, (DdNode *)e);

    /* If result is null and no reorderind took place,
       then something went wrong */
    nusmv_assert(result != NULL || dd->dd->reordered);
  }

  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_new_var_with_index(DDMgr_ptr dd, int index)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  add_ptr result;

  if ((DdHalfWord)index >= (MAX_VAR_INDEX - 1)) {

    ErrorMgr_rpterr(errmgr, "Unable to allocate a new BDD variable, max. number exceeded (%d)", index);
    ErrorMgr_nusmv_exit(errmgr, 1);
  }

  result = Cudd_addIthVar(dd->dd, index);
  common_error(errmgr, result, "add_new_var_with_index: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_new_var_at_level(DDMgr_ptr dd, int level)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_addNewVarAtLevel(dd->dd,level);
  common_error(errmgr, result, "add_new_var_at_level: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

bdd_ptr add_to_bdd(DDMgr_ptr dd, add_ptr fn)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  DdNode * result;

  result = Cudd_addBddBooleanMap(dd->dd,fn);
  common_error(errmgr, result, "add_to_bdd: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr add_to_bdd_strict_threshold(DDMgr_ptr dd, add_ptr fn, int k)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  /* Note for developers:
     Why not use cudd version?
     [AMa] Because the Cudd version cannot handle NuSMV numbers.
     [AMa] So we use the boolean version preparing the add using NuSMV node_gt
  */
  add_ptr add_k = add_leaf(dd, find_node(nodemgr, NUMBER, NODE_FROM_INT(k), Nil));
  add_ptr tmp = add_apply(dd, node_gt, fn, add_k);
  bdd_ptr result = add_to_bdd(dd, tmp);
  add_free(dd, tmp);
  add_free(dd, add_k);
  return result;
}

add_ptr bdd_to_add(DDMgr_ptr dd, bdd_ptr fn)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_BddToAdd(dd->dd, (DdNode *)fn);
  common_error(errmgr, result, "bdd_to_add: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr bdd_to_01_add(DDMgr_ptr dd, bdd_ptr fn)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_BddTo01Add(dd->dd, (DdNode *)fn);
  common_error(errmgr, result, "bdd_to_01_add: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_and(DDMgr_ptr dd, add_ptr a, add_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_addAnd(dd->dd, (DdNode *)a, (DdNode *)b);
  common_error(errmgr, result, "add_and: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_or(DDMgr_ptr dd, add_ptr a, add_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  DdNode * result;

  result = Cudd_addOr(dd->dd, (DdNode *)a, (DdNode *)b);
  common_error(errmgr, result, "add_or: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_xor(DDMgr_ptr dd, add_ptr a, add_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_addXor(dd->dd, (DdNode *)a, (DdNode *)b);
  common_error(errmgr, result, "add_xor: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_xnor(DDMgr_ptr dd, add_ptr a, add_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * tmp;
  DdNode * result;

  tmp = Cudd_addNot(dd->dd,(DdNode *)b);
  common_error(errmgr, tmp, "add_xnor: not(b) = NULL");
  Cudd_Ref(tmp);

  result = Cudd_addXor(dd->dd, (DdNode *)a, (DdNode *)tmp);
  common_error(errmgr, result, "add_xor: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_not(DDMgr_ptr dd, add_ptr a)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_addNot(dd->dd, (DdNode *)a);
  common_error(errmgr, result, "add_not: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_implies(DDMgr_ptr dd, add_ptr a, add_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * tmp;
  DdNode * result;

  tmp = Cudd_addNot(dd->dd,(DdNode *)a);
  common_error(errmgr, tmp, "add_implies: not(a) = NULL");
  Cudd_Ref(tmp);
  result = Cudd_addOr(dd->dd, tmp, (DdNode *)b);
  common_error2(errmgr, dd->dd, result, tmp, "add_implies: result = NULL");
  Cudd_Ref(result);
  Cudd_RecursiveDeref(dd->dd, tmp);
  return((add_ptr)result);
}

add_ptr add_iff(DDMgr_ptr dd, add_ptr a, add_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * tmp;
  DdNode * result;

  tmp = Cudd_addXor(dd->dd, (DdNode *)a, (DdNode *)b);
  common_error(errmgr, tmp, "add_iff: xor(a,b) = NULL");
  Cudd_Ref(tmp);

  result = Cudd_addNot(dd->dd, tmp);
  common_error2(errmgr, dd->dd, result, tmp, "add_iff: result = NULL");
  Cudd_Ref(result);

  Cudd_RecursiveDeref(dd->dd, tmp);
  return((add_ptr) result);
}

void add_and_accumulate(DDMgr_ptr dd, add_ptr *a, add_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_addAnd(dd->dd,(DdNode *) *a, (DdNode *)b);
  common_error(errmgr, result, "add_and_accumulate: result = NULL");
  Cudd_Ref(result);
  Cudd_RecursiveDeref(dd->dd, (DdNode *) *a);
  *a = (add_ptr)result;
}

void add_or_accumulate(DDMgr_ptr dd, add_ptr *a, add_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_addOr(dd->dd,(DdNode *) *a, (DdNode *)b);
  common_error(errmgr, result, "add_or_accumulate: result = NULL");
  Cudd_Ref(result);
  Cudd_RecursiveDeref(dd->dd, (DdNode *) *a);
  *a = (add_ptr)result;
}

add_ptr add_apply(DDMgr_ptr dd, NPFNNE op, add_ptr f, add_ptr g)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_addApply(dd->dd,
                         (CUDD_VALUE_TYPE (*)(DdManager*, DD_AOP, DdNode*, DdNode*))op,
                         (DdNode *)f,
                         (DdNode *)g,
                         ENV_OBJECT(dd)->environment);
  common_error(errmgr, result, "add_apply: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_monadic_apply(DDMgr_ptr dd, NPFNNE/*NPFCVT*/ op, add_ptr f)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  /* Function Cudd_addMonadicApply appears in later version of CUDD.
     Here we try to mimic its behaviour */
  add_ptr _true = add_true(dd);
  DdNode * result;

  result = Cudd_addApply(dd->dd, (CUDD_VALUE_TYPE (*)(DdManager*, DD_AOP, DdNode*, DdNode*))op, (DdNode *)f, (DdNode *) _true, env);
  common_error(errmgr, result, "add_monadic_apply: result = NULL");
  Cudd_Ref(result);

  add_free(dd, _true);
  return((add_ptr)result);
}

add_ptr add_exist_abstract(DDMgr_ptr dd, add_ptr a, bdd_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * cube;
  DdNode * result = (DdNode*)NULL;

  cube = Cudd_BddToAdd(dd->dd, (DdNode *)b);
  common_error(errmgr, cube, "add_exist_abstract: cube = NULL");

  result = Cudd_addAbstract(dd->dd, (CUDD_VALUE_TYPE (*)(CUDD_VALUE_TYPE))node_plus, (DdNode *)a, (DdNode *)cube,
                            ENV_OBJECT(dd)->environment);
  common_error(errmgr, result, "add_exist_abstract: result = NULL");

  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_ifthenelse(DDMgr_ptr dd, add_ptr If, add_ptr Then, add_ptr Else)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_addIte(dd->dd, (DdNode *)If, (DdNode *)Then, (DdNode *)Else);
  common_error(errmgr, result, "add_ifthenelse: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_cube_diff(DDMgr_ptr dd, add_ptr a, add_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_addCubeDiff(dd->dd, (DdNode *)a, (DdNode *)b);
  common_error(errmgr, result, "add_cube_diff: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_permute(DDMgr_ptr dd, add_ptr fn, int * permut)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode *result;

  result = Cudd_addPermute(dd->dd, (DdNode *)fn, permut);
  common_error(errmgr, result, "add_permute: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_support(DDMgr_ptr dd, add_ptr fn)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * tmp_1, * result;

  tmp_1 = Cudd_Support(dd->dd, (DdNode *)fn);
  common_error(errmgr, tmp_1, "add_support: tmp_1 = NULL");
  Cudd_Ref(tmp_1);
  result = Cudd_BddToAdd(dd->dd, tmp_1);
  common_error2(errmgr, dd->dd, result, tmp_1, "add_support: result = NULL");
  Cudd_RecursiveDeref(dd->dd, tmp_1);
  Cudd_Ref(result);
  return((add_ptr)result);
}

add_ptr add_simplify_assuming(DDMgr_ptr dd, add_ptr a, add_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_addRestrict(dd->dd, (DdNode *)a, (DdNode *)b);
  common_error(errmgr, result, "add_simplify_assuming: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

int add_size(DDMgr_ptr dd, add_ptr fn)
{
  return(Cudd_DagSize((DdNode *)fn));
}

double add_count_minterm(DDMgr_ptr dd, add_ptr fn, int nvars)
{
  return(Cudd_CountMinterm(dd->dd, (DdNode *)fn, nvars));
}

node_ptr add_value(DDMgr_ptr dd, add_ptr fn)
{
  node_ptr result;

  result = Cudd_add_value((DdNode *)fn);
  return(result);
}

add_ptr add_if_then(DDMgr_ptr dd, add_ptr I, add_ptr T)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_addIfThen(dd->dd, (DdNode *)I, (DdNode *)T);

  common_error(errmgr, result, "add_if_then: result = NULL");
  Cudd_Ref(result);
  return((add_ptr)result);
}

void add_walkleaves(DDMgr_ptr dd, VPFDDCVT op, add_ptr f)
{
  Cudd_addWalkLeaves(dd->dd, (void (*)(void *, node_ptr))op,
                     (DdNode *)f, (void*)dd);
  return;
}

bdd_ptr bdd_true(DDMgr_ptr dd)
{
  DdNode * result = Cudd_ReadTrue(dd->dd);

  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr bdd_false(DDMgr_ptr dd)
{
  DdNode * result = Cudd_ReadLogicFalse(dd->dd);

  Cudd_Ref(result);
  return((bdd_ptr)result);
}

int bdd_is_true(DDMgr_ptr dd, bdd_ptr f)
{
  return((DdNode *)f == Cudd_ReadTrue(dd->dd));
}

int bdd_isnot_true(DDMgr_ptr dd, bdd_ptr f)
{
  return((DdNode *)f != Cudd_ReadTrue(dd->dd));
}

int bdd_is_false(DDMgr_ptr dd, bdd_ptr f)
{
  return((DdNode *)f == Cudd_ReadLogicFalse(dd->dd));
}

int bdd_isnot_false(DDMgr_ptr dd, bdd_ptr f)
{
  return((DdNode *)f != Cudd_ReadLogicFalse(dd->dd));
}

void bdd_ref(bdd_ptr dd_node)
{
  Cudd_Ref(dd_node);
}

void bdd_deref(bdd_ptr dd_node)
{
  Cudd_Deref(dd_node);
}

void bdd_free(DDMgr_ptr dd, bdd_ptr dd_node)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  common_error(errmgr, dd_node, "bdd_free: dd_node = NULL");

  Cudd_RecursiveDeref(dd->dd, (DdNode *)dd_node);
}

bdd_ptr bdd_dup(bdd_ptr dd_node)
{
  Cudd_Ref(dd_node);
  return(dd_node);
}

bdd_ptr bdd_not(DDMgr_ptr dd, bdd_ptr fn)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_Not(fn);
  common_error(errmgr, result, "bdd_not: result == NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr bdd_and(DDMgr_ptr dd, bdd_ptr a, bdd_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_bddAnd(dd->dd, (DdNode *)a, (DdNode *)b);
  common_error(errmgr, result, "bdd_and: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr bdd_or(DDMgr_ptr dd, bdd_ptr a, bdd_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_bddOr(dd->dd, (DdNode *)a, (DdNode *)b);
  common_error(errmgr, result, "bdd_or: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr bdd_xor(DDMgr_ptr dd, bdd_ptr a, bdd_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_bddXor(dd->dd, (DdNode *)a, (DdNode *)b);
  common_error(errmgr, result, "bdd_xor: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr bdd_iff(DDMgr_ptr dd, bdd_ptr a, bdd_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * tmp_1;
  DdNode * result;

  tmp_1 = Cudd_bddXor(dd->dd, (DdNode *)a, (DdNode *)b);
  common_error(errmgr, tmp_1, "bdd_iff: bdd_xor(a,b) = NULL");
  Cudd_Ref(tmp_1);
  result = Cudd_Not(tmp_1);
  common_error2(errmgr, dd->dd, result, tmp_1, "bdd_iff: result = NULL");
  Cudd_Ref(result);
  Cudd_RecursiveDeref(dd->dd, tmp_1);
  return((bdd_ptr)result);
}

bdd_ptr bdd_imply(DDMgr_ptr dd, bdd_ptr a, bdd_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * tmp_1;
  DdNode * result;

  tmp_1 = Cudd_Not((DdNode *)a);
  common_error(errmgr, tmp_1, "bdd_imply: not(a) = NULL");
  Cudd_Ref(tmp_1);
  result = Cudd_bddOr(dd->dd, tmp_1, (DdNode *)b);
  common_error2(errmgr, dd->dd, result, tmp_1, "bdd_imply: result = NULL");
  Cudd_Ref(result);
  Cudd_RecursiveDeref(dd->dd, tmp_1);
  return((bdd_ptr)result);
}

void bdd_and_accumulate(DDMgr_ptr dd, bdd_ptr * a, bdd_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_bddAnd(dd->dd, (DdNode *)*a, (DdNode *)b);
  common_error(errmgr, result, "bdd_and_accumulate: result = NULL");
  Cudd_Ref(result);
  Cudd_RecursiveDeref(dd->dd, (DdNode *)*a);
  *a = result;
  return;
}

void bdd_or_accumulate(DDMgr_ptr dd, bdd_ptr * a, bdd_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_bddOr(dd->dd, (DdNode *)*a, (DdNode *)b);
  common_error(errmgr, result, "bdd_or_accumulate: result = NULL");
  Cudd_Ref(result);
  Cudd_RecursiveDeref(dd->dd, (DdNode *) *a);
  *a = result;
  return;
}

bdd_ptr bdd_forsome(DDMgr_ptr dd, bdd_ptr fn, bdd_ptr cube)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_bddExistAbstract(dd->dd, (DdNode *)fn, (DdNode *)cube);
  common_error(errmgr, result, "bdd_forsome: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr bdd_forall(DDMgr_ptr dd, bdd_ptr fn, bdd_ptr cube)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_bddUnivAbstract(dd->dd, (DdNode *)fn, (DdNode *)cube);
  common_error(errmgr, result, "bdd_forall: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr bdd_permute(DDMgr_ptr dd, bdd_ptr fn, int * permut)
{
  DdNode * result;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  result = Cudd_bddPermute(dd->dd, (DdNode *)fn, permut);
  common_error(errmgr, result, "bdd_permute: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr bdd_and_abstract(DDMgr_ptr dd, bdd_ptr T, bdd_ptr S, bdd_ptr V)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_bddAndAbstract(dd->dd, (DdNode *)T, (DdNode *)S, (DdNode *)V);
  common_error(errmgr, result, "bdd_and_abstract: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr bdd_simplify_assuming(DDMgr_ptr dd, bdd_ptr fn, bdd_ptr c)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_bddRestrict(dd->dd, (DdNode *)fn, (DdNode *)c);
  common_error(errmgr, result, "bdd_simplify_assuming: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr bdd_minimize(DDMgr_ptr dd, bdd_ptr fn, bdd_ptr c)
{
  DdNode * result;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  result = Cudd_bddRestrict(dd->dd, (DdNode *)fn, (DdNode *)c);
  common_error(errmgr, result, "bdd_minimize: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
} /* end of bdd_minimize */

bdd_ptr bdd_cofactor(DDMgr_ptr dd, bdd_ptr f, bdd_ptr g)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode *result;

  /* We use Cudd_bddConstrain instead of Cudd_Cofactor for generality. */
  result = Cudd_bddConstrain(dd->dd, (DdNode *)f, (DdNode *)g);
  common_error(errmgr, result, "bdd_cofactor: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
} /* end of bdd_cofactor */

bdd_ptr bdd_between(DDMgr_ptr dd, bdd_ptr f_min, bdd_ptr f_max)
{
  bdd_ptr care_set, ret;

  care_set = bdd_imply(dd, f_min, f_max);
  ret = bdd_minimize(dd, f_min, care_set);
  bdd_free(dd, care_set);
  /*
    The size of ret is never larger than the size of f_min. We need
    only to check ret against f_max.
  */
  if (bdd_size(dd, f_max) <= bdd_size(dd, ret)) {
    bdd_free(dd, ret);
    return(bdd_dup(f_max));
  } else {
    return(ret);
  }
} /* end of bdd_between */

int bdd_entailed(DDMgr_ptr dd, bdd_ptr f, bdd_ptr g)
{
  int result;

  result = Cudd_bddLeq(dd->dd, (DdNode *)f, (DdNode *)g);
  return(result);
}

int bdd_intersected(DDMgr_ptr dd, bdd_ptr f, bdd_ptr g)
{
  int result;

  if (bdd_is_false(dd, f) || bdd_is_false(dd, g))
    return 0;

  result = Cudd_bddLeq(dd->dd, (DdNode *)f,
                       (DdNode *) Cudd_Not((DdNode *)g));

  return !result;
}

bdd_ptr bdd_then(DDMgr_ptr dd, bdd_ptr f)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  bdd_ptr result;

  if (Cudd_IsConstant((DdNode *)f)) {
    ErrorMgr_rpterr(errmgr, "bdd_then: called on a constant bdd.");
    result = (bdd_ptr)NULL;
    ErrorMgr_nusmv_exit(errmgr, 1);
  }
  else {
    result = (bdd_ptr)Cudd_T(f);
  }
  return(result);
}

bdd_ptr bdd_else(DDMgr_ptr dd, bdd_ptr f)
{
  bdd_ptr result;

  if (Cudd_IsConstant((DdNode *)f)) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    ErrorMgr_rpterr(errmgr, "bdd_else: called on a constant bdd.");
    result = (bdd_ptr)NULL;
    ErrorMgr_nusmv_exit(errmgr, 1);
  }
  else {
    result = (bdd_ptr)Cudd_E(f);
  }
  return(result);
}

int bdd_iscomplement(DDMgr_ptr dd, bdd_ptr f)
{
  return(Cudd_IsComplement((DdNode *)f));
}

int bdd_readperm(DDMgr_ptr dd, bdd_ptr f)
{
  int result;

  result = Cudd_ReadPerm(dd->dd, Cudd_NodeReadIndex((DdNode *)f));
  return(result);
}

int bdd_index(DDMgr_ptr dd, bdd_ptr f)
{
  int result;

  result = Cudd_NodeReadIndex((DdNode *)f);
  return(result);
}

bdd_ptr bdd_ite(DDMgr_ptr dd, bdd_ptr i, bdd_ptr t, bdd_ptr e)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_bddIte(dd->dd, (DdNode *)i, (DdNode *)t, (DdNode *)e);
  common_error(errmgr, result, "bdd_ite: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

int bdd_size(DDMgr_ptr dd, bdd_ptr fn)
{
  return(Cudd_DagSize((DdNode *)fn));
}

double bdd_count_minterm(DDMgr_ptr dd, bdd_ptr fn, int nvars)
{
  return(floor(Cudd_CountMinterm(dd->dd, (DdNode *)fn, nvars)));
}

bdd_ptr bdd_support(DDMgr_ptr dd, bdd_ptr fn)
{
  DdNode * result;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  result = Cudd_Support(dd->dd, (DdNode *)fn);
  common_error(errmgr, result, "bdd_support: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr bdd_pick_one_minterm(DDMgr_ptr dd, bdd_ptr fn, bdd_ptr * vars, int n)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  if (bdd_is_false(dd, fn)) {
    Cudd_Ref(fn);
    return(fn);
  }
  else {
    result = Cudd_bddPickOneMintermNR(dd->dd, (DdNode *)fn, (DdNode **)vars, n);
    common_error(errmgr, result, "bdd_pick_one_minterm: result = NULL");
    Cudd_Ref(result);
    return((bdd_ptr)result);
  }
}

bdd_ptr bdd_pick_one_minterm_rand(DDMgr_ptr dd, bdd_ptr fn, bdd_ptr * vars, int n)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  if (bdd_is_false(dd, fn)) {
    Cudd_Ref(fn);
    return(fn);
  }
  else {
    result = Cudd_bddPickOneMinterm(dd->dd, (DdNode *)fn, (DdNode **)vars, n);
    common_error(errmgr, result, "bdd_pick_one_minterm_rand: result = NULL");
    Cudd_Ref(result);
    return((bdd_ptr)result);
  }
}

int bdd_pick_all_terms(
  DDMgr_ptr dd           /* dd manager */,
  bdd_ptr   pick_from_set  /* minterm from which to pick  all term */,
  bdd_ptr   * vars         /* The array of vars to be put in the returned array */,
  int       vars_dim       /* The size of the above array */,
  bdd_ptr   * result       /* The array used as return value */,
  int       result_dim     /* The size of the above array */)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  if (bdd_is_true(dd, pick_from_set)) {
    bdd_ptr not_var0 = bdd_not(dd, vars[0]);

    if (Cudd_PickAllTerms(dd->dd, vars[0], vars, vars_dim, result) == 1) {
      StreamMgr_print_error(streams,  "Error from Cudd_PickAllTerms.\n");
      bdd_free(dd, not_var0);
      return 1;
    }
    nusmv_assert((result_dim % 2) == 0);
    if ( Cudd_PickAllTerms(dd->dd, not_var0, vars, vars_dim,
                          result + result_dim/2) == 1 ) {
      StreamMgr_print_error(streams,  "Error from Cudd_PickAllTerms.\n");
      bdd_free(dd, not_var0);
      return 1;
    }
    bdd_free(dd, not_var0);
  }
  else
    if (Cudd_PickAllTerms(dd->dd, pick_from_set, vars, vars_dim, result) == 1) {
      StreamMgr_print_error(streams,  "Error from Cudd_PickAllTerms.\n");
      return 1;
    }
  return 0;
}

bdd_ptr bdd_new_var_with_index(DDMgr_ptr dd, int index)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_bddIthVar(dd->dd, index);
  common_error(errmgr, result, "bdd_new_var_with_index: result = NULL");
  /* bdd var does not require to be referenced when created */
  return bdd_dup((bdd_ptr) result);
}

bdd_ptr bdd_cube_diff(DDMgr_ptr dd, bdd_ptr a, bdd_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_bddCubeDiff(dd->dd, (DdNode *)a, (DdNode *)b);
  common_error(errmgr, result, "bdd_cube_diff: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr bdd_cube_union(DDMgr_ptr dd, bdd_ptr a, bdd_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  bdd_ptr result;

  result = bdd_and(dd,a,b);
  common_error(errmgr, result, "bdd_cube_union: result = NULL");
  return(result);
}

bdd_ptr bdd_cube_intersection(DDMgr_ptr dd, bdd_ptr a, bdd_ptr b)
{
  bdd_ptr result,tmp;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  tmp = bdd_cube_diff(dd , a , b);
  result= bdd_cube_diff(dd , a , tmp);
  bdd_free(dd,tmp);
  common_error(errmgr, result, "bdd_cube_intersection: result = NULL");
  return(result);
}

int bdd_get_lowest_index(DDMgr_ptr dd, bdd_ptr a)
{
  int result;

  result = Cudd_BddGetLowestVar(dd->dd, (DdNode *)a);
  return(result);
}

bdd_ptr bdd_get_one_sparse_sat(DDMgr_ptr dd, bdd_ptr d)
{
  bdd_ptr result, old, zero;
  bdd_ptr T, E, node, vi, nvi;

  int reord_status;
  dd_reorderingtype rt;

  /* temporary disables reordering */
  reord_status = dd_reordering_status(dd, &rt);
  if (reord_status == 1) { dd_autodyn_disable(dd); }

  /* if it is a constant BDD (true or false), then return it */
  if (bdd_is_true(dd, d)) return bdd_true(dd);
  if (bdd_is_false(dd, d)) return bdd_false(dd);

  node = d;
  zero = bdd_false(dd);
  old = bdd_true(dd);

  while (true) {
    /* no need to free them, reference is not incremented */
    T = bdd_then(dd, node);
    E = bdd_else(dd, node);

    /* Take care of the fact that the node can be complemented */
    if (bdd_iscomplement(dd, node)) {
      T = bdd_not(dd, T);
      bdd_deref(T);
      E = bdd_not(dd, E);
      bdd_deref(E);
    }

    /* Get the bdd variable at the given level */
    vi = bdd_new_var_with_index(dd, bdd_index(dd, node));
    /* reference it since bdd_new_var_with_index does not reference */
    bdd_ref(vi);

    /* If then is true, build assignment and stop */
    if (bdd_is_true(dd, T)) {
      result = bdd_ite(dd, old, vi, zero);
      bdd_free(dd, vi);
      bdd_free(dd, old);
      break;
    }

    /* If else is true, build assignment and stop */
    if (bdd_is_true(dd, E)) {
      nvi = bdd_not(dd, vi);
      result = bdd_ite(dd, old, nvi, zero);
      bdd_free(dd, nvi);
      bdd_free(dd, vi);
      bdd_free(dd, old);
      break;
    }

    /* If then is false, build assignment and recurse on the else */
    if (bdd_is_false(dd, T)) {
      nvi = bdd_not(dd, vi);
      result = bdd_ite(dd, old, nvi, zero);
      bdd_free(dd, nvi);
      bdd_free(dd, old);
      old = result;
      node = E;
    }
    /* If else is false, build assignment and recurse on the then */
    else if (bdd_is_false(dd, E)) {
      result = bdd_ite(dd, old, vi, zero);
      bdd_free(dd, old);
      old = result;
      node = T;
    }
    /* If neither then nor else are true/false, build assignment
       considering the current variable positive, and recurse on the
       then */
    else {
      result = bdd_ite(dd, old, vi, zero);
      bdd_free(dd, old);
      old = result;
      node = T;
    }
    bdd_free(dd, vi);

  } /* loop */

  bdd_free(dd, zero);

  /* re-enable reordering if it is required */
  if (reord_status == 1) { dd_autodyn_enable(dd,rt); }

  return result;
}

bdd_ptr bdd_make_prime(DDMgr_ptr dd, bdd_ptr cube, bdd_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  DdNode * result;

  result = Cudd_bddMakePrime(dd->dd, (DdNode *)cube, (DdNode *)b);
  common_error(errmgr, result, "bdd_make_prime: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr bdd_largest_cube(DDMgr_ptr dd, bdd_ptr b, int *length)
{
  DdNode * result;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  result = Cudd_LargestCube(dd->dd, (DdNode *)b, length);
  common_error(errmgr, result, "bdd_largest_cube: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

bdd_ptr bdd_compute_prime_low(DDMgr_ptr dd, bdd_ptr b, bdd_ptr low) {
  int length;
  bdd_ptr result, implicant;

  /* a minterm is an implicant */
  implicant = bdd_largest_cube(dd, low, &length);
  /* Expand the minterm to be a prime implicant */
  result = bdd_make_prime(dd, implicant, b);
  bdd_free(dd, implicant);

  return result;
}

array_t * bdd_compute_primes_low(DDMgr_ptr dd, bdd_ptr b, bdd_ptr low) {
  array_t * result = array_alloc(bdd_ptr, 0);
  bdd_ptr curr_low = bdd_dup(low);

  while(bdd_isnot_false(dd, curr_low)) {
    bdd_ptr prime, notprime;

    prime = bdd_compute_prime_low(dd, b, curr_low);
    array_insert_last(bdd_ptr, result, prime);
    /* */
    notprime = bdd_not(dd, prime);
    bdd_and_accumulate(dd, &curr_low, notprime);
    bdd_free(dd, notprime);
  }
  bdd_free(dd, curr_low);
  return result;
}

array_t * bdd_compute_primes(DDMgr_ptr dd, bdd_ptr b) {
  array_t * result;
  bdd_ptr low = bdd_dup(b);

  result = bdd_compute_primes_low(dd, b, low);
  bdd_free(dd, low);

  return result;
}

bdd_ptr bdd_compute_essentials(DDMgr_ptr dd, bdd_ptr b)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  DdNode * result;

  result = Cudd_FindEssential(dd->dd, b);
  common_error(errmgr, result, "bdd_compute_essentials: result = NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

int bdd_leq(DDMgr_ptr dd, bdd_ptr f, bdd_ptr g) {
  return(Cudd_bddLeq(dd->dd, f, g));
}

bdd_ptr bdd_swap_variables(DDMgr_ptr dd, bdd_ptr f,
                           bdd_ptr *x_varlist,
                           bdd_ptr *y_varlist,
                           int n) {

  DdNode *result;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  result = Cudd_bddSwapVariables(dd->dd, f, x_varlist, y_varlist, n);
  common_error(errmgr, result, "bdd_swap_variables: result == NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

int bdd_DumpBlif(
  DDMgr_ptr dd     /* manager */,
  int n             /* number of output nodes to be dumped */,
  bdd_ptr* f        /* array of output nodes to be dumped */,
  char** inames     /* array of input names (or NULL) */,
  char** onames     /* array of output names (or NULL) */,
  char* mname       /* model name (or NULL) */,
  FILE* fp          /* pointer to the dump file */)
{
  return(Cudd_DumpBlif(dd->dd, n, f, inames, onames, mname, fp));
}

int bdd_DumpBlifBody(
  DDMgr_ptr dd     /* manager */,
  int  n            /* number of output nodes to be dumped */,
  bdd_ptr* f        /* array of output nodes to be dumped */,
  char** inames     /* array of input names (or NULL) */,
  char** onames     /* array of output names (or NULL) */,
  FILE* fp          /* pointer to the dump file */)
{
  return(Cudd_DumpBlifBody(dd->dd, n, f, inames, onames, fp));
}

bdd_ptr bdd_compose(DDMgr_ptr dd, bdd_ptr f, bdd_ptr g, int v)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  DdNode *result;

  result = Cudd_bddCompose(dd->dd, f, g, v);
  common_error(errmgr, result, "bdd_compose: result == NULL");
  Cudd_Ref(result);
  return((bdd_ptr)result);
}

int bdd_ref_count(DDMgr_ptr dd, bdd_ptr n)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  int result;
  DdNode *N;

  N = Cudd_Regular((DdNode*)n);
  common_error(errmgr, N, "Could not make node regular, while getting ref-count.");
  result = N->ref;
  return(result);
}

int calculate_bdd_value(DDMgr_ptr mgr, bdd_ptr f, int* values)
{
  unsigned char invert = Cudd_IsComplement(f) ? 1u : 0u;
  while (!Cudd_IsConstant(f)) {
    if (values[(Cudd_Regular(f))->index]) f = Cudd_T(f);
    else f = Cudd_E(f);
    invert ^= (Cudd_IsComplement(f) ? 1u : 0u);
  }
  return(invert ? 0 : 1);
}

int Dd_dynamic_var_ordering(NuSMVEnv_ptr env,
                            DDMgr_ptr dd_manager,
                            int dynOrderingMethod,
                            DdDynVarOrderAction action)
{
  StreamMgr_ptr const streams =
   STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int retval = 0;
  dd_reorderingtype currentMethod = -1;

  switch(action) {
  case DD_DYN_VAR_ORDER_ACTION_DISABLE:
    {
      (void)dd_reordering_status(dd_manager, &currentMethod);

      if (currentMethod == REORDER_NONE) {
        StreamMgr_print_output(streams,  "Dynamic variable ordering is already disabled.\n");
      }
      else {
        StreamMgr_print_output(streams,  "Dynamic variable ordering is disabled.\n");
        dd_autodyn_disable(dd_manager);
        unset_dynamic_reorder(opts);
      }

      break;
    }

    /*
     * Set the dynamic ordering method.
     */
  case DD_DYN_VAR_ORDER_ACTION_ENABLE:
    {
      dd_autodyn_enable(dd_manager, dynOrderingMethod);
      set_reorder_method(opts, dynOrderingMethod);
      set_dynamic_reorder(opts);
      StreamMgr_print_output(streams,  "Dynamic variable ordering is enabled ");
      StreamMgr_print_output(streams,  "with method \"%s\".\n",
                             DynOrderTypeConvertToString(dynOrderingMethod));

      break;
    }

  /*
   * Force a reordering.  Note that the DdManager has to have the method set
   * before calling bdd_reorder.
  */
  case DD_DYN_VAR_ORDER_ACTION_FORCE:
    {
      StreamMgr_print_output(streams,  "Dynamic variable ordering forced ");
      StreamMgr_print_output(streams,  "with method %s....\n",
                             DynOrderTypeConvertToString(dynOrderingMethod));
      dd_reorder(dd_manager, dynOrderingMethod, DEFAULT_MINSIZE);

      break;
    }

  default:
    error_unreachable_code();
  }

  return retval;
}

/*!
  \brief Convert a string to an action

  -1 is return in case of failing to recognize
*/

DdDynVarOrderAction Dd_action_str_to_enum(char* action)
{
  if (0 == strcmp("disable_dyn_var_order", action)) {
    return DD_DYN_VAR_ORDER_ACTION_DISABLE;
  }
  else if (0 == strcmp("enable_dyn_var_order", action)) {
    return DD_DYN_VAR_ORDER_ACTION_ENABLE;
  }
  else if (0 == strcmp("force_dyn_var_order", action)) {
    return DD_DYN_VAR_ORDER_ACTION_FORCE;
  }
  else return -1;
}

/*!
  \brief Convert an action to a string

  NULL is returned in caso of error
*/

char* Dd_action_enum_to_str(DdDynVarOrderAction action)
{
  switch(action) {
  case DD_DYN_VAR_ORDER_ACTION_DISABLE:
    return "disable_dyn_var_order";

  case DD_DYN_VAR_ORDER_ACTION_ENABLE:
    return "enable_dyn_var_order";

  case DD_DYN_VAR_ORDER_ACTION_FORCE:
    return "force_dyn_var_order";

  default:
    return NULL;
  }
}

int Dd_set_bdd_parameters(NuSMVEnv_ptr env,
                          DDMgr_ptr dd_manager,
                          boolean showAfter)
{
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);

  int retval = 0;

  /* Create the table of variable->value */
  retval = dd_set_parameters(dd_manager, opts, outstream);

  /* the returned value has an opposite meaning */
  if (0 == retval) retval = 1;
  else if (1 == retval) retval = 0;
  else error_unreachable_code();

  if (0 == retval) {
    if (showAfter) {
      retval = dd_print_stats(env, dd_manager, outstream);
    }
  }
  else {
    StreamMgr_print_error(streams,
                          "Something went wrong creating the cudd"
                          " bdd parameters table\n");
  }

  return retval;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Function to print a warning that an illegal value was read.

  \todo Missing description

  \sa bdd_set_parameters
*/

static void InvalidType(FILE *file, char *field, char *expected)
{
    fprintf(file, "Warning: In parameter \"%s\"\n", field);
    fprintf(file, "Illegal type detected. %s expected\n", expected);

} /* end of InvalidType */
