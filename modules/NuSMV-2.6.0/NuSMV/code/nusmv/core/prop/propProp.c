/* ---------------------------------------------------------------------------


  This file is part of the ``prop'' package.
  %COPYRIGHT%
  

-----------------------------------------------------------------------------*/

/*!
  \author Michele Dorigatti
  \brief The public implementation of the prop package

  \todo: Missing description

*/

#include "nusmv/core/prop/propInt.h"
#include "nusmv/core/prop/propProp.h"
#include "nusmv/core/prop/PropDb.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/ucmd.h"

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

lsList Prop_convert_props_to_invar(PropDb_ptr prop_db, lsList props)
{
  lsGen iterator;
  Prop_ptr prop = NULL;
  Prop_ptr invar = NULL;
  lsList retval = NULL;

  retval = lsCreate();

  lsForEachItem(props, iterator, prop) {
    invar = Prop_convert_to_invar(prop);

    if (NULL != invar) {
      int index = 0;
      boolean status = true;
      lsStatus ls_status = LS_NIL;

#ifndef NDEBUG
      {
        NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(prop_db));
        SymbTable_ptr const symb_table =
          SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
        TypeChecker_ptr type_checker = SymbTable_get_type_checker(symb_table);

        nusmv_assert(TypeChecker_check_property(type_checker, invar));
      }
#endif

      status = PropDb_add(prop_db, invar);
      nusmv_assert(! status);

      index = Prop_get_index(invar);
      ls_status = lsNewEnd(retval, PTR_FROM_INT(lsGeneric, index), LS_NH);
      nusmv_assert(LS_OK == ls_status);
    }
  }

  return retval;
}

Set_t Prop_propset_from_indices(NuSMVEnv_ptr env, const char* indices)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const char* delimiters=",:";
  Set_t res = Set_MakeEmpty();
  char* _copy_indices = util_strsav(indices);
  char* token;

  for (token=strtok(_copy_indices, delimiters); token != (char*) NULL;
       token=strtok((char*) NULL, delimiters)) {
    char* dash = strchr(token, '-');
    int low, high, idx;

    if (dash == (char*) NULL) { /* no range */
      if (util_str2int(token, &low) != 0) {
        ErrorMgr_error_invalid_number(errmgr, token);
        FREE(_copy_indices);
        ErrorMgr_rpterr(errmgr, NULL);
      }
      high = low;
    }
    else { /* a range has been specified */
      *dash = '\0'; /* splits the range */
      if (util_str2int(token, &low) != 0) {
        ErrorMgr_error_invalid_number(errmgr, token);
        FREE(_copy_indices);
        Set_ReleaseSet(res);
        ErrorMgr_rpterr(errmgr, NULL);
      }
      if (util_str2int(dash+1, &high) != 0) {
        ErrorMgr_error_invalid_number(errmgr, token);
        FREE(_copy_indices);
        Set_ReleaseSet(res);
        ErrorMgr_rpterr(errmgr, NULL);
      }
    }

    if (low > high) {
      FREE(_copy_indices);
      Set_ReleaseSet(res);
      ErrorMgr_rpterr(errmgr, "Range error: %d-%d", low, high);
    }

    for (idx=low; idx <= high; ++idx) {
      Prop_ptr prop = PropDb_get_prop_at_index(PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB)), idx);
      if (prop == (Prop_ptr) NULL) {
        FREE(_copy_indices);
        Set_ReleaseSet(res);
        ErrorMgr_rpterr(errmgr, "Property index %d is not valid (must be in the range [0,%d])",
               idx, PropDb_get_size(PROP_DB(NuSMVEnv_get_value(env, ENV_PROP_DB))));
      }

      res = Set_AddMember(res, (Set_Element_t) prop);
    } /* for all property indices */
  }

  FREE(_copy_indices);
  return res;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

