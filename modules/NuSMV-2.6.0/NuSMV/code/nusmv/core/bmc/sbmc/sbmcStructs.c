/* ---------------------------------------------------------------------------


  This file is part of the ``bmc.sbmc'' package of NuSMV version 2.
  Copyright (C) 2006 by Tommi Junttila, Timo Latvala.

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
  \author Tommi Junttila, Timo Latvala, Marco Roveri
  \brief Utilities for SBMC functionalities

  This file contains utilities for the Simple Bounded
  Model Checking algorithms published in the paper by Heljanko,
  Junttila  and Latvala at CAV 2005.

*/



#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/bmc/sbmc/sbmcStructs.h"
#include "nusmv/core/bmc/sbmc/sbmcUtils.h"

#include "nusmv/core/node/node.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/array.h"
#include "nusmv/core/utils/list.h"
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

struct state_vars_struct_TAG
{
  /* lsLists below are lists of node_ptr */
  NuSMVEnv_ptr environment;

  /* System state variables occurring in transition relation */
  lsList   transition_state_vars;

  /* Auxiliary variables needed in the PLTL BMC encoding */
  lsList   translation_vars_pd0;  /* [[f]]_i^0 vars */
  lsList   translation_vars_pdx;  /* [[f]]_i^d, d > 0 vars */
  lsList   translation_vars_aux;  /* <<F f>>_i and <<G f>>_i vars */
  node_ptr l_var;                 /* The loop selector variable */
  node_ptr LoopExists_var;        /* The loop exists variable */
  node_ptr LastState_var;         /* The last state variable */

  /* System state variables occurring in formula */
  lsList   formula_state_vars;

  /* System input variables occurring in formula */
  lsList   formula_input_vars;

  /* System variables used to create constraint of equality of states. Union of
     transition_state_vars, formula_state_vars, formula_input_vars.
  */
  lsList   simple_path_system_vars;
};

struct sbmc_node_info_struct
{
  unsigned int past_depth;   /* The past depth of the subformula */

  /* The variables for [[f]]^d if translated via variables, 0 otherwise */
  array_t*     trans_vars;   /* node_ptr[past_depth] of node_ptr */
  /* The bes for [[f]]_i^d */
  array_t*     trans_bes;    /* array_t [time] array_t [past_depth] of be_ptr*/

  /* The variable for the auxiliary <<F f>> translation if needed */
  node_ptr     aux_F_node;
  /* The bes for <<F f>>_i */
  array_t*     aux_F_trans;  /* be_ptr aux_F_trans[time] */

  /* The variable for the auxiliary <<G f>> translation if needed */
  node_ptr     aux_G_node;
  /* The bes for <<G f>>_i */
  array_t*     aux_G_trans;  /* be_ptr aux_G_trans[time] */
};


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

static enum st_retval sbmc_node_info_free_func(char *k, char *r, char *a);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

state_vars_struct* sbmc_state_vars_create(const NuSMVEnv_ptr env)
{
  state_vars_struct *svs = ALLOC(state_vars_struct, 1);
  nusmv_assert(svs != (state_vars_struct *) NULL);

  /* We increment to counter for the creation of unique ids among
     different calls */
  sbmc_increment_unique_id(env);

  svs->environment = env;
  svs->transition_state_vars = lsCreate();
  svs->l_var = (node_ptr)NULL;
  svs->LoopExists_var = (node_ptr)NULL;
  svs->LastState_var = (node_ptr)NULL;
  svs->translation_vars_pd0 = lsCreate();
  svs->translation_vars_pdx = lsCreate();
  svs->translation_vars_aux = lsCreate();
  svs->formula_state_vars = lsCreate();
  svs->formula_input_vars = lsCreate();
  svs->simple_path_system_vars = lsCreate();

  return svs;
}

void sbmc_state_vars_destroy(state_vars_struct* svs)
{
  nusmv_assert(svs);

  /* frees members */
  lsDestroy(svs->simple_path_system_vars, NULL);
  lsDestroy(svs->formula_input_vars, NULL);
  lsDestroy(svs->formula_state_vars, NULL);
  lsDestroy(svs->translation_vars_aux, NULL);
  lsDestroy(svs->translation_vars_pdx, NULL);
  lsDestroy(svs->translation_vars_pd0, NULL);
  lsDestroy(svs->transition_state_vars, NULL);

  FREE(svs);
}


/* Getters and Setters */

lsList sbmc_state_vars_get_trans_state_vars(const state_vars_struct * ss)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  return ss->transition_state_vars;
}

node_ptr sbmc_state_vars_get_l_var(const state_vars_struct * ss)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  return ss->l_var;
}

node_ptr sbmc_state_vars_get_LoopExists_var(const state_vars_struct * ss)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  return ss->LoopExists_var;
}

node_ptr sbmc_state_vars_get_LastState_var(const state_vars_struct * ss)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  return ss->LastState_var;
}

lsList sbmc_state_vars_get_translation_vars_pd0(const state_vars_struct * ss)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  return ss->translation_vars_pd0;
}

lsList sbmc_state_vars_get_translation_vars_pdx(const state_vars_struct * ss)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  return ss->translation_vars_pdx;
}

lsList sbmc_state_vars_get_translation_vars_aux(const state_vars_struct * ss)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  return ss->translation_vars_aux;
}

lsList sbmc_state_vars_get_formula_state_vars(const state_vars_struct * ss)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  return ss->formula_state_vars;
}

lsList sbmc_state_vars_get_formula_input_vars(const state_vars_struct * ss) {
  nusmv_assert((state_vars_struct *)NULL != ss);
  return ss->formula_input_vars;
}

lsList sbmc_state_vars_get_simple_path_system_vars(const state_vars_struct * ss)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  return ss->simple_path_system_vars;
}

void sbmc_state_vars_set_trans_state_vars(state_vars_struct * ss, lsList f)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  ss->transition_state_vars = f;
}

void sbmc_state_vars_set_l_var(state_vars_struct * ss, node_ptr f)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  ss->l_var = f;
}

void sbmc_state_vars_set_LoopExists_var(state_vars_struct * ss, node_ptr f)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  ss->LoopExists_var = f;
}

void sbmc_state_vars_set_LastState_var(state_vars_struct * ss, node_ptr f)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  ss->LastState_var = f;
}

void sbmc_state_vars_set_translation_vars_pd0(state_vars_struct * ss, lsList f)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  ss->translation_vars_pd0 = f;
}

void sbmc_state_vars_set_translation_vars_pdx(state_vars_struct * ss, lsList f)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  ss->translation_vars_pdx = f;
}

void sbmc_state_vars_set_translation_vars_aux(state_vars_struct * ss, lsList f)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  ss->translation_vars_aux = f;
}

void sbmc_state_vars_set_formula_state_vars(state_vars_struct * ss, lsList f)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  ss->formula_state_vars = f;
}

void sbmc_state_vars_set_formula_input_vars(state_vars_struct * ss, lsList f)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  ss->formula_input_vars = f;
}

void sbmc_state_vars_set_simple_path_system_vars(state_vars_struct * ss,
                                                 lsList f)
{
  nusmv_assert((state_vars_struct *)NULL != ss);
  ss->simple_path_system_vars = f;
}

void sbmc_state_vars_print(state_vars_struct *svs, FILE* out)
{
  const NuSMVEnv_ptr env = svs->environment;
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  fprintf(out, "The %d state variables in transition relation are: ",
          lsLength(svs->transition_state_vars));
  sbmc_print_node_list(env, out, svs->transition_state_vars);
  fprintf(out, "\n");

  fprintf(out, "The l variable is: ");
  print_node(wffprint, out, svs->l_var);
  fprintf(out, "\n");

  fprintf(out, "The LoopExists variable is: ");
  print_node(wffprint, out, svs->LoopExists_var);
  fprintf(out, "\n");

  fprintf(out, "The LastState variable is: ");
  print_node(wffprint, out, svs->LastState_var);
  fprintf(out, "\n");

  fprintf(out, "Translation state variables at depth 0 are: ");
  sbmc_print_node_list(env, out, svs->translation_vars_pd0);
  fprintf(out, "\n");

  fprintf(out, "Translation state variables at depth d>0 are: ");
  sbmc_print_node_list(env, out, svs->translation_vars_pdx);
  fprintf(out, "\n");

  fprintf(out, "Translation state variables for aux translation are: ");
  sbmc_print_node_list(env, out, svs->translation_vars_aux);
  fprintf(out, "\n");

  fprintf(out, "State variables in formula are: ");
  sbmc_print_node_list(env, out, svs->formula_state_vars);
  fprintf(out, "\n");

  fprintf(out, "Input variables in formula are: ");
  sbmc_print_node_list(env, out, svs->formula_input_vars);
  fprintf(out, "\n");

  fprintf(out, "The %d system variables in the simple path constraint are: ",
          lsLength(svs->simple_path_system_vars));
  sbmc_print_node_list(env, out, svs->simple_path_system_vars);
  fprintf(out, "\n");
}

hash_ptr sbmc_set_create()
{
  hash_ptr hash = new_assoc();
  nusmv_assert(hash != (hash_ptr) NULL);

  return hash;
}

void sbmc_set_destroy(hash_ptr hash)
{
  if (hash != (hash_ptr) NULL) {
    free_assoc(hash);
  }
}

void sbmc_set_insert(hash_ptr hash, node_ptr bexp)
{
  nusmv_assert(hash != (hash_ptr) NULL);
  nusmv_assert((node_ptr)1 != (node_ptr)NULL);
  insert_assoc(hash, bexp, (node_ptr)1);
}

int sbmc_set_is_in(hash_ptr hash, node_ptr bexp)
{
  node_ptr result;
  nusmv_assert(hash != (hash_ptr) NULL);
  result = find_assoc(hash, bexp);
  if (result == (node_ptr)NULL)
    return 0;
  return 1;
}

sbmc_node_info * sbmc_alloc_node_info()
{
  sbmc_node_info * info = ALLOC(sbmc_node_info, 1);

  info->past_depth = (unsigned int)0;
  info->trans_vars = (array_t *)NULL;
  info->trans_bes = array_alloc(array_t *, 1);
  array_insert(array_t*, info->trans_bes, 0, (array_t *)NULL);
  info->aux_F_node = (node_ptr)NULL;
  info->aux_F_trans = (array_t *)NULL;
  info->aux_G_node = (node_ptr)NULL;
  info->aux_G_trans = (array_t *)NULL;
  return info;
}

void sbmc_node_info_free(sbmc_node_info * info) {
  if ((array_t *)NULL != info->trans_vars) {
    array_free(info->trans_vars);
    info->trans_vars = (array_t *)NULL;
  }
  if ((array_t *)NULL != info->trans_bes) {
    int i;
    for (i = 0; i < array_n(info->trans_bes); i++) {
      array_t * a = array_fetch(array_t*, info->trans_bes, i);

      if ((array_t *)NULL != a) {
        array_free(a);
        array_insert(array_t*, info->trans_bes, i, (array_t *)NULL);
      }
    }
    array_free(info->trans_bes);
    info->trans_bes = (array_t *)NULL;
  }
  if ((array_t *)NULL != info->aux_F_trans) {
    array_free(info->aux_F_trans);
    info->aux_F_trans = (array_t *)NULL;
  }
  if ((array_t *)NULL != info->aux_G_trans) {
    array_free(info->aux_G_trans);
    info->aux_G_trans = (array_t *)NULL;
  }
  FREE(info);
  info = 0;
}

/* Getters and Setters */

unsigned int sbmc_node_info_get_past_depth(sbmc_node_info * h)
{
  nusmv_assert((sbmc_node_info*)NULL != h);
  return h->past_depth;
}

array_t * sbmc_node_info_get_trans_vars(sbmc_node_info * h)
{
  nusmv_assert((sbmc_node_info*)NULL != h);
  return h->trans_vars;
}
array_t * sbmc_node_info_get_trans_bes(sbmc_node_info * h)
{
  nusmv_assert((sbmc_node_info*)NULL != h);
  return h->trans_bes;
}
node_ptr sbmc_node_info_get_aux_F_node(sbmc_node_info * h)
{
  nusmv_assert((sbmc_node_info*)NULL != h);
  return h->aux_F_node;
}
array_t * sbmc_node_info_get_aux_F_trans(sbmc_node_info * h)
{
  nusmv_assert((sbmc_node_info*)NULL != h);
  return h->aux_F_trans;
}
node_ptr sbmc_node_info_get_aux_G_node(sbmc_node_info * h)
{
  nusmv_assert((sbmc_node_info*)NULL != h);
  return h->aux_G_node;
}
array_t * sbmc_node_info_get_aux_G_trans(sbmc_node_info * h)
{
  nusmv_assert((sbmc_node_info*)NULL != h);
  return h->aux_G_trans;
}

void sbmc_node_info_set_past_depth(sbmc_node_info * h, unsigned int s)
{
  nusmv_assert((sbmc_node_info*)NULL != h);
  h->past_depth = s;
}

void sbmc_node_info_set_past_trans_vars(sbmc_node_info * h, array_t * s)
{
  nusmv_assert((sbmc_node_info*)NULL != h);
  h->trans_vars = s;
}
void sbmc_node_info_set_trans_bes(sbmc_node_info * h, array_t * s)
{
  nusmv_assert((sbmc_node_info*)NULL != h);
  h->trans_bes = s;
}
void sbmc_node_info_set_aux_F_node(sbmc_node_info * h, node_ptr s)
{
  nusmv_assert((sbmc_node_info*)NULL != h);
  h->aux_F_node = s;
}
void sbmc_node_info_set_aux_F_trans(sbmc_node_info * h, array_t * s)
{
  nusmv_assert((sbmc_node_info*)NULL != h);
  h->aux_F_trans = s;
}
void sbmc_node_info_set_aux_G_node(sbmc_node_info * h, node_ptr s)
{
  nusmv_assert((sbmc_node_info*)NULL != h);
  h->aux_G_node = s;
}
void sbmc_node_info_set_aux_G_trans(sbmc_node_info * h, array_t * s)
{
  nusmv_assert((sbmc_node_info*)NULL != h);
  h->aux_G_trans = s;
}

hash_ptr sbmc_node_info_assoc_create(void) {
  return new_assoc();
}

void sbmc_node_info_assoc_free(hash_ptr * a) {
  clear_assoc_and_free_entries(*a, sbmc_node_info_free_func);
  free_assoc(*a);
  *a = (hash_ptr) NULL;
}

void sbmc_node_info_assoc_insert(hash_ptr a, node_ptr n,
                                 sbmc_node_info * i) {
  insert_assoc(a, n, (node_ptr)i);
}

sbmc_node_info * sbmc_node_info_assoc_find(hash_ptr a, node_ptr n) {
  return (sbmc_node_info *)find_assoc(a, n);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*
 * Deallocate a sbmc_node_info structure given as 'r'
 */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static enum st_retval sbmc_node_info_free_func(char *k, char *r, char *a)
{
  if ((char *)NULL != r) {
    sbmc_node_info* info = (sbmc_node_info*)r;
    sbmc_node_info_free(info);
  }
  return ST_DELETE;
}


