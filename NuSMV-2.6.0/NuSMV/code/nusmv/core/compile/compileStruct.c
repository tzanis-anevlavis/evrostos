/* ---------------------------------------------------------------------------


  This file is part of the ``compile'' package of NuSMV version 2. 
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
  \brief Structure used to store compilation results.

  Structure used to store compilation results.

*/


#include "nusmv/core/compile/compileInt.h" 
/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

struct cmp_struct {
  int      read_model;
  int      hrc_built;
  int      flatten_hierarchy;
  int      encode_variables;
  int      process_selector;
  int      build_frames;
  int      build_model;
  int      build_flat_model;
  int      build_bool_model;
  int      bmc_init;
  int      bmc_setup;
  int      fairness_constraints;
  int      coi;
};

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/**Variable**********************************************************************

  Synopsis    [ This is a global variable responsable for 
  keeping track of performed phases.]

  Description [It is used in interactive mode,
  to distinguish which commands can or cannot be executed.]

******************************************************************************/
cmp_struct_ptr cmps = (cmp_struct_ptr) NULL;

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

cmp_struct_ptr cmp_struct_init(void)
{
  cmp_struct_ptr cmp;
  cmp = ALLOC(struct cmp_struct, 1);
  cmp->read_model           = 0;
  cmp->hrc_built            = 0;
  cmp->flatten_hierarchy    = 0;
  cmp->encode_variables     = 0;
  cmp->process_selector     = 0;
  cmp->build_frames         = 0;
  cmp->build_model          = 0;
  cmp->build_flat_model     = 0;
  cmp->build_bool_model     = 0;
  cmp->bmc_init             = 0;
  cmp->bmc_setup            = 0;
  cmp->fairness_constraints = 0;
  cmp->coi                  = 0;
  return(cmp);
}

void cmp_struct_quit(cmp_struct_ptr cmp)
{
  nusmv_assert((cmp_struct_ptr) NULL != cmp);
  FREE(cmp);
}

int cmp_struct_get_read_model(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  return(cmp->read_model);
}

void cmp_struct_set_read_model(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->read_model = 1;
}

void cmp_struct_unset_read_model(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->read_model = 0;
}

int cmp_struct_get_hrc_built(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  return(cmp->hrc_built);
}

void cmp_struct_set_hrc_built(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->hrc_built = 1;
}

int cmp_struct_get_flatten_hrc(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  return(cmp->flatten_hierarchy);
}

void cmp_struct_set_flatten_hrc(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->flatten_hierarchy = 1;
}

int cmp_struct_get_encode_variables(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  return(cmp->encode_variables);
}

void cmp_struct_set_encode_variables(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->encode_variables = 1;
}

int cmp_struct_get_process_selector(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  return(cmp->process_selector);
}

void cmp_struct_set_process_selector(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->process_selector = 1;
}

int cmp_struct_get_build_frames(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  return(cmp->build_frames);
}

void cmp_struct_set_build_frames(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->build_frames = 1;
}

int cmp_struct_get_build_model(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  return(cmp->build_model);
}

void cmp_struct_set_build_model(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->build_model = 1;
}

int cmp_struct_get_build_flat_model(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  return(cmp->build_flat_model);
}

void cmp_struct_set_build_flat_model(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->build_flat_model = 1;
}

int cmp_struct_get_build_bool_model(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  return(cmp->build_bool_model);
}

void cmp_struct_set_build_bool_model(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->build_bool_model = 1;
}

int cmp_struct_get_bmc_init(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  return(cmp->bmc_init);
}
void cmp_struct_set_bmc_init(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->bmc_init = 1;
}
void cmp_struct_unset_bmc_init(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->bmc_init = 0;
}

int cmp_struct_get_bmc_setup(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  return(cmp->bmc_setup);
}

void cmp_struct_set_bmc_setup(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->bmc_setup = 1;
}

void cmp_struct_unset_bmc_setup(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->bmc_setup = 0;
}

int cmp_struct_get_fairness(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  return(cmp->fairness_constraints);
}

void cmp_struct_set_fairness(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->fairness_constraints = 1;
}

int cmp_struct_get_coi(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  return(cmp->coi);
}

void cmp_struct_set_coi(cmp_struct_ptr cmp)
{
  nusmv_assert(cmp != NULL);
  cmp->coi = 1;
}



