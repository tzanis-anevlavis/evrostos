/* ---------------------------------------------------------------------------


  This file is part of the ``opt'' package of NuSMV version 2.
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
  \brief The internal header file of the opt package.

  The internal header file of the opt package.

*/


#ifndef __NUSMV_CORE_OPT_OPT_INT_H__
#define __NUSMV_CORE_OPT_OPT_INT_H__

#include <stdio.h>
#include <limits.h>

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/cinit/cinit.h"
#include "cudd/util.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/set/set.h"
#include "nusmv/core/dd/dd.h"
#include "nusmv/core/rbc/rbc.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/prop/Prop.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/utils/ucmd.h"
#include "nusmv/core/trans/trans.h" /* for TransType */
#include "nusmv/core/fsm/bdd/bdd.h" /* for BddOregJusticeEmptinessBddAlgorithmType */



/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

extern cmp_struct_ptr cmps;

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/
/* Triggers */

/*!
  \brief Trigger that sets the use_reachable_states flag if needed

  Trigger that sets the use_reachable_states flag if needed
*/
boolean opt_set_reachable_states_trigger(OptsHandler_ptr opts,
                                                const char* opt,
                                                const char* value,
                                                Trigger_Action action,
                                                void* arg);

/*!
  \brief reordering method trigger: enables / disables dd_autodyn

  reordering method trigger: enables / disables dd_autodyn
*/
boolean opt_reorder_method_trigger(OptsHandler_ptr opts,
                                          const char* opt,
                                          const char* value,
                                          Trigger_Action action,
                                          void* arg);

/*!
  \brief Trigger for the default_trace_plugin option. 

  Trigger for the default_trace_plugin option: Updates the
   default plugin in the trace pkg.
*/
boolean opt_trace_plugin_trigger(OptsHandler_ptr opts,
                                        const char* opt,
                                        const char* value,
                                        Trigger_Action action,
                                        void* arg);

/*!
  \brief Dynamic reordering trigger: enables / disables dd_autodyn

  Dynamic reordering trigger: enables / disables dd_autodyn
*/
boolean opt_dynamic_reorder_trigger(OptsHandler_ptr opts,
                                           const char* opt,
                                           const char* value,
                                           Trigger_Action action,
                                           void* arg);

/*!
  \brief Trigger function for the trans_order_file option

  Trigger function for the trans_order_file option:
   Enables/disables AFFINITY_CLUSTERING if needed
*/
boolean opt_trans_order_file_trigger(OptsHandler_ptr opts,
                                            const char* opt,
                                            const char* value,
                                            Trigger_Action action,
                                            void* arg);

/*!
  \brief Trigger function for the run_cpp option

  Trigger function for the run_cpp option: Tells that the
   option is deprecated. No side-effect on the option
   value will be performed
*/
boolean opt_run_cpp_trigger(OptsHandler_ptr opts,
                                   const char* opt,
                                   const char* value,
                                   Trigger_Action action,
                                   void* arg);

/*!
  \brief Trigger function for the pp_list option

  Trigger function for the pp_list option. Checks that
   the given list of preprocessors is valid or not
*/
boolean opt_pp_list_trigger(OptsHandler_ptr opts,
                                   const char* opt,
                                   const char* value,
                                   Trigger_Action action,
                                   void* arg);

/*!
  \brief Trigger function for the run_cpp option

  Trigger function for the run_cpp option: Tells that the
   option is deprecated. No side-effect on the option
   value will be performed
*/
boolean opt_rbc_inlining_lazy_trigger(OptsHandler_ptr opts,
                                             const char* opt,
                                             const char* value,
                                             Trigger_Action action,
                                             void* arg);

/*!
  \brief 

  
*/
boolean opt_script_file_trigger(OptsHandler_ptr opts,
                                       const char* opt,
                                       const char* value,
                                       Trigger_Action action,
                                       void* arg);

/*!
  \brief 

  
*/
boolean opt_pp_cpp_path_trigger(OptsHandler_ptr opts,
                                       const char* opt,
                                       const char* value,
                                       Trigger_Action action,
                                       void* arg);

/*!
  \brief 

  
*/
boolean opt_pp_m4_path_trigger(OptsHandler_ptr opts,
                                      const char* opt,
                                      const char* value,
                                      Trigger_Action action,
                                      void* arg);

#if NUSMV_HAVE_REGEX_H

/*!
  \brief Trigger function for the traces_regexp option

  Trigger function for the counter_examples_show_re
   option: tries to compile regexp pattern and rejects it if
   compilation fails.
*/
boolean
opt_traces_regexp_trigger(OptsHandler_ptr opts, const char* opt,
                          const char* value, Trigger_Action action,
                          void* arg);
#endif

/* triggers end */
/******************************************************************************/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
char* opt_check_invar_strategy_to_string(Check_Strategy str);

/*!
  \brief Convert the FB_Heuristic to it's string representation

  Convert the FB_Heuristic to it's string representation
*/
char* opt_check_invar_fb_heuristic_to_string(FB_Heuristic h);

/*!
  \brief Convert the Bdd2bmc_Heuristic to it's string representation

  Convert the Bdd2bmc_Heuristic to it's string representation
*/
char*
opt_check_invar_bddbmc_heuristic_to_string(Bdd2bmc_Heuristic h);

/*!
  \brief Get the integer representation of the given string

  Get the integer representation of the given string
*/
void* opt_get_integer(OptsHandler_ptr opts, const char* val, void* arg);

#endif /* __NUSMV_CORE_OPT_OPT_INT_H__ */
