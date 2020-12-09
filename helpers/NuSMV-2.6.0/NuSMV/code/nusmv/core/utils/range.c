/* ---------------------------------------------------------------------------


  This file is part of the ``utils'' package of NuSMV version 2.
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
  \author Roberto Cavada
  \brief Contains function for checking of ranges and subranges

  \todo: Missing description

*/


#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/range.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h"

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
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_UTILS_CHECK_RANGE_VAR "eucrv"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_UTILS_CHECK_RANGE_RANGE "eucrr"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_UTILS_CHECK_RANGE_IS_FATAL "eucrif"

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/


/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Utils_set_data_for_range_check(const NuSMVEnv_ptr env, node_ptr var, node_ptr range)
{
  NuSMVEnv_set_or_replace_value(env, ENV_UTILS_CHECK_RANGE_VAR, var);
  NuSMVEnv_set_or_replace_value(env, ENV_UTILS_CHECK_RANGE_RANGE, range);
}

void Utils_set_mode_for_range_check(const NuSMVEnv_ptr env, boolean is_fatal)
{
  NuSMVEnv_set_flag(env, ENV_UTILS_CHECK_RANGE_IS_FATAL, is_fatal);
}

void Utils_range_check(const NuSMVEnv_ptr env, node_ptr n)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  node_ptr the_var = NODE_PTR(NuSMVEnv_get_value(env, ENV_UTILS_CHECK_RANGE_VAR));
  node_ptr the_range = NODE_PTR(NuSMVEnv_get_value(env, ENV_UTILS_CHECK_RANGE_RANGE));
  boolean range_err = true;

  if (NuSMVEnv_has_flag(env, ENV_UTILS_CHECK_RANGE_IS_FATAL)) {
     range_err = NuSMVEnv_get_flag(env, ENV_UTILS_CHECK_RANGE_IS_FATAL);
  }

  if (n == Nil) {
    ErrorMgr_internal_error(errmgr, "Utils_range_check: n == Nil");
  }

  if (node_get_type(n) == CONS) {
    while (n != (node_ptr) NULL) {
      /* ignore FAILURE nodes */
      if (node_get_type(car(n)) != FAILURE && !in_list(car(n), the_range)) {
        if (range_err) { ErrorMgr_range_error(errmgr, car(n), the_var); }
        else { ErrorMgr_range_warning(errmgr, car(n), the_var); }
      }
      n = cdr(n);
    }
  }
  else {
    /* ignore FAILURE nodes */
    if (node_get_type(n) != FAILURE && !in_list(n, the_range)) {
      if (range_err) { ErrorMgr_range_error(errmgr, n, the_var); }
      else { ErrorMgr_range_warning(errmgr, n, the_var); }
    }
  }
}

void Utils_failure_node_check(const NuSMVEnv_ptr env, node_ptr n)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  if (node_get_type(n) == FAILURE) ErrorMgr_report_failure_node(errmgr, n);
}

boolean Utils_is_in_range(node_ptr s, node_ptr d)
{
  if (d == Nil) return false;

  if (node_get_type(s) == CONS) {
    while (s != Nil) {
      nusmv_assert(CONS == node_get_type(s));
      if (in_list(car(s), d) == 0) return false;
      s = cdr(s);
    }
    return true;
  }

  /* s is a singleton */
  return (in_list(s, d) == 1);
}

boolean Utils_check_subrange(node_ptr subrange)
{
  int inf, sup;
  nusmv_assert(node_get_type(subrange) == TWODOTS);

  inf = node_get_int(car(subrange));
  sup = node_get_int(cdr(subrange));

  return inf <= sup;
}

boolean Utils_check_subrange_not_negative(node_ptr subrange)
{
  int inf;
  nusmv_assert(node_get_type(subrange) == TWODOTS);

  inf = node_get_int(car(subrange));
  return (inf >= 0) && Utils_check_subrange(subrange);
}
