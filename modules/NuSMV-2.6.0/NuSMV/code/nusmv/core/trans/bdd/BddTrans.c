/* ---------------------------------------------------------------------------


  This file is part of the ``trans.bdd'' package of NuSMV version 2.
  Copyright (C) 2003 by FBK-irst.

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
  \brief Routines related to Transition Relation in Bdd form

  This file contains the definition of the \"BddTrans\" class
  definition

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/trans/bdd/BddTrans.h"
#include "nusmv/core/trans/transInt.h"
#include "nusmv/core/trans/generic/GenericTrans.h"
#include "nusmv/core/trans/generic/GenericTrans_private.h"

#include "nusmv/core/trans/bdd/ClusterList.h"
#include "nusmv/core/trans/bdd/Cluster.h"
#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct BddTrans_TAG
{
  INHERITS_FROM(GenericTrans);
  /* generic data structure representing the concrete implementation
     of BddTrans class. */
  void* transition;

  /* takes a transition data structure and returns its copy */
  void* (*trans_copy)(void* transition);

  /* takes a instance of transition data structure and frees it and its
     substructure */
  void  (*trans_destroy)(void* transition);

  /* takes a bdd and computes an image, i.e. conjunct
     the bdd with transition and abstract away some of state/input/next variables
     depending on the image kind. For example, TRANS_IMAGE_FORWARD_STATE
     abstract away input and next variables, leaving only current state ones.
     Depending on the transition data structure the BDD may have some
     limitations, e.g. for forward images provided BDD may have only
     current state and input variables.
     If there are such limitation the user is responsible to guarantee
     that algorithms invoking BddTrans functions computing images
     respect such limitations.
     Computed BDD is returned and has to be freed by invoker. */
  bdd_ptr (*trans_compute_image)(void* transition,
                           bdd_ptr bdd, TransImageKind kind);

  /* the same as compute_image but k-image is computed. */
  bdd_ptr (*trans_compute_k_image)(void* transition,
                                   bdd_ptr bdd, int k, TransImageKind kind);

  /* returns a BDD representing the whole
     transition relation. To be freed by invoker. */
  bdd_ptr (*trans_get_monolithic_bdd)(void* transition);

  /* returns a clusterlist representing the whole
     transition relation. To be freed by invoker. */
  ClusterList_ptr (*trans_get_clusterlist)(void* transition);

  /* given 2 transition data structure,
    add synchronously the second transition to the first one.
    The first transition is updated. The second one remains unchanged. */
  void (*trans_synchronous_product)(void* transition1,
                              void* const transition2);

  /* print some short into about the provided transition */
  void (*trans_print_short_info)(void* transition, FILE* file);
} BddTrans;


/*!
  \brief Implementation of transition based on
  ClusterList.

   This class contains informations about a transition
  relation based on ClusterList implementation.
  A pointer to this structure is passed to BddTrans_generic_create
  and its functions. The content is: <br>
          <dl>
            <dt><code>forward_trans</code>
                <dd> The list of clusters representing the transition relation
                used when a forward image is performed
            <dt><code>backward_trans</code>
                <dd> The list of clusters representing the transition
                relation used when a backwad image is performed
        </dl>
*/

typedef struct ClusterBasedTrans_TAG
{
  /* The list of clusters representing the transition
     relation used when a forward image is performed */
  ClusterList_ptr forward_trans;

  /* The list of clusters representing the transition
     relation used when a backwad image is performed */
  ClusterList_ptr backward_trans;

} ClusterBasedTrans;

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef ClusterBasedTrans* ClusterBasedTrans_ptr;

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void bdd_trans_init(BddTrans_ptr self,
                           const NuSMVEnv_ptr env,
                           const TransType trans_type,
                           void* transition,
                           void* (*trans_copy)(void* transition),
                           void  (*trans_destroy)(void* transition),
                           bdd_ptr (*trans_compute_image)(void* transition,
                                                          bdd_ptr bdd, TransImageKind kind),
                           bdd_ptr (*trans_compute_k_image)(void* transition,
                                                            bdd_ptr bdd, int k, TransImageKind kind),
                           bdd_ptr (*trans_get_monolithic_bdd)(void* transition),
                           void (*trans_synchronous_product)(void* transition1,
                                                             void* const transition2),
                           void (*trans_print_short_info)(void* transition, FILE* file),
                           ClusterList_ptr (*trans_get_clusterlist)(void* transition)
);

static void bdd_trans_finalize(Object_ptr object, void* dummy);

static void bdd_trans_deinit(BddTrans_ptr self);

static Object_ptr bdd_trans_copy(const Object_ptr object);

#ifdef TRANS_DEBUG_THRESHOLD  /* this is used only for debugging purposes */

static boolean
bdd_trans_debug_partitioned(const ClusterBasedTrans_ptr self,
                            const ClusterList_ptr basic_clusters,
                            FILE* file);
#endif


/* implementation of function for ClusterBasedTrans */
static void* bdd_trans_clusterlist_copy(void* transition);
static void bdd_trans_clusterlist_destroy(void* transition);
static bdd_ptr bdd_trans_clusterlist_compute_image(void* transition,
                                             bdd_ptr bdd,
                                             TransImageKind kind);
static bdd_ptr bdd_trans_clusterlist_compute_k_image(void* transition,
                                               bdd_ptr bdd, int k,
                                               TransImageKind kind);
static bdd_ptr bdd_trans_clusterlist_get_monolithic_bdd(void* transition);
static ClusterList_ptr 
bdd_trans_clusterlist_get_clusterlist(void* transition);
static void bdd_trans_clusterlist_synchronous_product(void* transition1,
                                                void* const transition2);
static void bdd_trans_clusterlist_print_short_info(void* transition,
                                                   FILE* file);


/*---------------------------------------------------------------------------*/
/* Exported function definitions                                             */
/*---------------------------------------------------------------------------*/

BddTrans_ptr BddTrans_create(DDMgr_ptr dd_manager,
                             const ClusterList_ptr clusters_bdd,
                             bdd_ptr state_vars_cube,
                             bdd_ptr input_vars_cube,
                             bdd_ptr next_state_vars_cube,
                             const TransType trans_type,
                             const ClusterOptions_ptr cl_options)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd_manager));
  ClusterBasedTrans_ptr trans = ALLOC(ClusterBasedTrans, 1);
  BddTrans_ptr self = NULL;

  switch (trans_type) {

  case TRANS_TYPE_MONOLITHIC:
    trans->forward_trans  = ClusterList_apply_monolithic(clusters_bdd);
    trans->backward_trans = ClusterList_copy(trans->forward_trans);
    break;

  case TRANS_TYPE_THRESHOLD:
    trans->forward_trans = ClusterList_apply_threshold(clusters_bdd,
                                                       cl_options);
    trans->backward_trans = ClusterList_copy(trans->forward_trans);
    break;

  case TRANS_TYPE_IWLS95:
    trans->forward_trans =
      ClusterList_apply_iwls95_partition(clusters_bdd,
                                         state_vars_cube,
                                         input_vars_cube,
                                         next_state_vars_cube,
                                         cl_options);

    trans->backward_trans =
      ClusterList_apply_iwls95_partition(clusters_bdd,
                                         next_state_vars_cube,
                                         input_vars_cube,
                                         state_vars_cube,
                                         cl_options);
    break;

  default:
    error_unreachable_code(); /* no other types available */

  } /* switch on type */

  ClusterList_build_schedule(trans->forward_trans,
                             state_vars_cube, input_vars_cube);

  ClusterList_build_schedule(trans->backward_trans,
                             next_state_vars_cube, input_vars_cube);

# ifdef TRANS_DEBUG_THRESHOLD  /* trans checking of partitioned trans */
  if (trans_type != TRANS_TYPE_MONOLITHIC) {
    bdd_trans_debug_partitioned(trans, clusters_bdd, errstream);
  }
# endif


  self = BddTrans_generic_create(env,
                                 trans_type,
                                 trans,
                                 bdd_trans_clusterlist_copy,
                                 bdd_trans_clusterlist_destroy,
                                 bdd_trans_clusterlist_compute_image,
                                 bdd_trans_clusterlist_compute_k_image,
                                 bdd_trans_clusterlist_get_monolithic_bdd,
                                 bdd_trans_clusterlist_synchronous_product,
                                 bdd_trans_clusterlist_print_short_info,
                                 bdd_trans_clusterlist_get_clusterlist);
  return self;
}

BddTrans_ptr
BddTrans_generic_create(const NuSMVEnv_ptr env,
                        const TransType trans_type,
                        void* transition,
                        void* (*trans_copy)(void* transition),
                        void  (*trans_destroy)(void* transition),
                        bdd_ptr (*trans_compute_image)(void* transition,
                                                       bdd_ptr bdd, TransImageKind kind),
                        bdd_ptr (*trans_compute_k_image)(void* transition,
                                                         bdd_ptr bdd, int k, TransImageKind kind),
                        bdd_ptr (*trans_get_monolithic_bdd)(void* transition),
                        void (*trans_synchronous_product)(void* transition1,
                                                          void* const transition2),
                        void (*trans_print_short_info)(void* transition, FILE* file),
                        ClusterList_ptr (*trans_get_clusterlist)(void* transition))
{
  BddTrans_ptr self = ALLOC(BddTrans, 1);

  BDD_TRANS_CHECK_INSTANCE(self);

  bdd_trans_init(self, env, trans_type,
                 transition, trans_copy, trans_destroy,
                 trans_compute_image, trans_compute_k_image,
                 trans_get_monolithic_bdd, trans_synchronous_product,
                 trans_print_short_info,
                 trans_get_clusterlist);

  return self;
}

void BddTrans_apply_synchronous_product(BddTrans_ptr self,
                                        const BddTrans_ptr other)
{
  BDD_TRANS_CHECK_INSTANCE(self);

  /* both transitions belong to the kind of transition. thus their
     copy functions are the same */
  nusmv_assert(self->trans_copy == other->trans_copy);

  self->trans_synchronous_product(self->transition, other->transition);
}

bdd_ptr BddTrans_get_monolithic_bdd(BddTrans_ptr self)
{
  BDD_TRANS_CHECK_INSTANCE(self);

  return self->trans_get_monolithic_bdd(self->transition);
}

ClusterList_ptr BddTrans_get_clusterlist(const BddTrans_ptr self)
{
  BDD_TRANS_CHECK_INSTANCE(self);

  return self->trans_get_clusterlist(self->transition);
}

bdd_ptr BddTrans_get_forward_image_state(const BddTrans_ptr self, bdd_ptr s)
{
  BDD_TRANS_CHECK_INSTANCE(self);
  return self->trans_compute_image(self->transition, s,
                                   TRANS_IMAGE_FORWARD_STATE);

}

bdd_ptr BddTrans_get_forward_image_state_input(const BddTrans_ptr self, bdd_ptr s)
{
  BDD_TRANS_CHECK_INSTANCE(self);
  return self->trans_compute_image(self->transition, s,
                                   TRANS_IMAGE_FORWARD_STATE_INPUT);
}

bdd_ptr BddTrans_get_backward_image_state(const BddTrans_ptr self, bdd_ptr s)
{
  BDD_TRANS_CHECK_INSTANCE(self);
  return self->trans_compute_image(self->transition, s,
                                   TRANS_IMAGE_BACKWARD_STATE);
}

bdd_ptr BddTrans_get_backward_image_state_input(const BddTrans_ptr self,
                                                bdd_ptr s)
{
  BDD_TRANS_CHECK_INSTANCE(self);
  return self->trans_compute_image(self->transition, s,
                                   TRANS_IMAGE_BACKWARD_STATE_INPUT);
}

bdd_ptr BddTrans_get_k_forward_image_state(const BddTrans_ptr self,
                                           bdd_ptr s, int k)
{
  BDD_TRANS_CHECK_INSTANCE(self);
  return self->trans_compute_k_image(self->transition, s, k,
                                     TRANS_IMAGE_FORWARD_STATE);
}

bdd_ptr BddTrans_get_k_forward_image_state_input(const BddTrans_ptr self, bdd_ptr s, int k)
{
  BDD_TRANS_CHECK_INSTANCE(self);
  return self->trans_compute_k_image(self->transition, s, k,
                                     TRANS_IMAGE_FORWARD_STATE_INPUT);
}

bdd_ptr BddTrans_get_k_backward_image_state(const BddTrans_ptr self, bdd_ptr s, int k)
{
  BDD_TRANS_CHECK_INSTANCE(self);
  return self->trans_compute_k_image(self->transition, s, k,
                                     TRANS_IMAGE_BACKWARD_STATE);
}

bdd_ptr BddTrans_get_k_backward_image_state_input(const BddTrans_ptr self,
                                                  bdd_ptr s,
                                                  int k)
{
  BDD_TRANS_CHECK_INSTANCE(self);
  return self->trans_compute_k_image(self->transition, s, k,
                                     TRANS_IMAGE_BACKWARD_STATE_INPUT);
}

void BddTrans_print_short_info(const BddTrans_ptr self, FILE* file)
{
  BDD_TRANS_CHECK_INSTANCE(self);
  self->trans_print_short_info(self->transition, file);
}


/*---------------------------------------------------------------------------*/
/* Static functions definitions                                              */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void bdd_trans_init(BddTrans_ptr self,
                           const NuSMVEnv_ptr env,
                           const TransType trans_type,
                           void* transition,
                           void* (*trans_copy)(void* transition),
                           void  (*trans_destroy)(void* transition),
                           bdd_ptr (*trans_compute_image)(void* transition,
                                                          bdd_ptr bdd, TransImageKind kind),
                           bdd_ptr (*trans_compute_k_image)(void* transition,
                                                            bdd_ptr bdd, int k, TransImageKind kind),
                           bdd_ptr (*trans_get_monolithic_bdd)(void* transition),
                           void (*trans_synchronous_product)(void* transition1,
                                                             void* const transition2),
                           void (*trans_print_short_info)(void* transition, FILE* file),
                           ClusterList_ptr (*trans_get_clusterlist)(void* transition)
                           )
{
  generic_trans_init(GENERIC_TRANS(self), env, trans_type);

  OVERRIDE(Object, finalize)   = bdd_trans_finalize;
  OVERRIDE(Object, copy)       = bdd_trans_copy;

  self->transition = transition;
  self->trans_copy = trans_copy;
  self->trans_destroy = trans_destroy;
  self->trans_compute_image = trans_compute_image;
  self->trans_compute_k_image = trans_compute_k_image;
  self->trans_get_monolithic_bdd = trans_get_monolithic_bdd;
  self->trans_synchronous_product = trans_synchronous_product;
  self->trans_print_short_info = trans_print_short_info;
  self->trans_get_clusterlist = trans_get_clusterlist;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void bdd_trans_finalize(Object_ptr object, void* dummy)
{
  BddTrans_ptr self = BDD_TRANS(object);

  bdd_trans_deinit(self);
  FREE(self);
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void bdd_trans_deinit(BddTrans_ptr self)
{
  generic_trans_deinit(GENERIC_TRANS(self));

  self->trans_destroy(self->transition);
  self->transition = NULL; /* debugging code */
}

/*!
  \brief Copy constructor

  Return a copy of the self.
*/
static Object_ptr bdd_trans_copy(const Object_ptr object)
{
  BddTrans_ptr self;
  BddTrans_ptr copy;

  self = BDD_TRANS(object);

  BDD_TRANS_CHECK_INSTANCE(self);

  /* copy is the owner of forward and backward: we do not destroy them */
  copy = ALLOC(BddTrans, 1);
  BDD_TRANS_CHECK_INSTANCE(copy);

  generic_trans_copy_aux(GENERIC_TRANS(self), GENERIC_TRANS(copy));

  copy->transition = self->trans_copy(self->transition);
  copy->trans_copy = self->trans_copy;
  copy->trans_destroy = self->trans_destroy;
  copy->trans_compute_image = self->trans_compute_image;
  copy->trans_compute_k_image = self->trans_compute_k_image;
  copy->trans_get_monolithic_bdd = self->trans_get_monolithic_bdd;
  copy->trans_synchronous_product = self->trans_synchronous_product;
  copy->trans_print_short_info = self->trans_print_short_info;
  copy->trans_get_clusterlist = self->trans_get_clusterlist;

  return OBJECT(copy);
}



#ifdef TRANS_DEBUG_THRESHOLD

/*!
  \brief  Checks the equality between given Monolithic and
  Partitioned transition relations.

   It checks the equality in terms of transition relation
  and quantification schedule. 
*/
static boolean
bdd_trans_debug_partitioned(const ClusterBasedTrans_ptr self,
                            const ClusterList_ptr basic_clusters,
                            FILE* file)
{
  boolean res = true;
  boolean tmp;

  fprintf(file, "\nChecking partitioned transition:\n");

  fprintf(file, " Checking equality Partitioned vs. Monolithic:\n");
  tmp = ClusterList_check_equality(self->backward_trans,
                                   basic_clusters);
  fprintf(file, "\t  Backward : %d\n", tmp);
  res = res && tmp;

  tmp = ClusterList_check_equality(self->forward_trans,
                                   basic_clusters);
  fprintf(file, "\t  Forward : %d\n", tmp);
  res = res && tmp;

  StreamMgr_print_error(streams, " Checking Quantification Schedule:\n");
  tmp = ClusterList_check_schedule(self->backward_trans);

  fprintf(file, "\t  Backward : %d\n", tmp);
  res = res && tmp;

  tmp = ClusterList_check_schedule(self->forward_trans);

  fprintf(file, "\t  Forward : %d\n", tmp);
  res = res && tmp;

  return res;
}

#endif /* TRANS_DEBUG_THRESHOLD is defined */



/***************************************************************************

 ---------------------------------------------------------------------------
  BELOW ARE IMPLEMENTATION OF FUNCTION PARAMETERS OF BddTrans_generic_create
  HAVING 'transition' OF TYPE ClusterBasedTrans
 ---------------------------------------------------------------------------

***************************************************************************/

/*!
  \brief Implementation of 'copy' parameter of
  BddTrans_generic_create having 'transition' of type ClusterBasedTrans_ptr

  See struct BddTrans_TAG for specification of this function
*/
static void* bdd_trans_clusterlist_copy(void* transition)
{
  ClusterBasedTrans_ptr trans = (ClusterBasedTrans_ptr) transition;

  ClusterBasedTrans_ptr self = ALLOC(ClusterBasedTrans, 1);
  self->forward_trans = ClusterList_copy(trans->forward_trans);
  self->backward_trans = ClusterList_copy(trans->backward_trans);

  return self;
}

/*!
  \brief Implementation of 'destroy' parameter of
  BddTrans_generic_create having 'transition' of type ClusterBasedTrans_ptr

  See struct BddTrans_TAG for specification of this function
*/
static void bdd_trans_clusterlist_destroy(void* transition)
{
  ClusterBasedTrans_ptr self = (ClusterBasedTrans_ptr) transition;
  ClusterList_destroy(self->forward_trans);
  ClusterList_destroy(self->backward_trans);
  FREE(self);
}

/*!
  \brief Implementation of 'compute_image' parameter of
  BddTrans_generic_create having 'transition' of type ClusterBasedTrans_ptr

  See struct BddTrans_TAG for specification of this function
*/
static bdd_ptr bdd_trans_clusterlist_compute_image(void* transition,
                                                   bdd_ptr bdd,
                                                   TransImageKind kind)
{
  ClusterBasedTrans_ptr trans = (ClusterBasedTrans_ptr) transition;


  switch(kind) {
  case TRANS_IMAGE_FORWARD_STATE:
    return ClusterList_get_image_state(trans->forward_trans, bdd);

  case TRANS_IMAGE_FORWARD_STATE_INPUT:
    return ClusterList_get_image_state_input(trans->forward_trans, bdd);

  case TRANS_IMAGE_BACKWARD_STATE:
    return ClusterList_get_image_state(trans->backward_trans, bdd);

  case TRANS_IMAGE_BACKWARD_STATE_INPUT:
    return ClusterList_get_image_state_input(trans->backward_trans, bdd);

  default:
    {
      /* This is done only to retrieve the errmgr */
      ClusterList_ptr const forward_trans = trans->forward_trans;
      DDMgr_ptr dd_mgr = ClusterList_get_dd_manager(forward_trans);
      NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(dd_mgr));

      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

      ErrorMgr_internal_error(errmgr, "impossible code in bdd_trans_clusterlist_compute_image");
    }
    return NULL;
  } /* switch */
}

/*!
  \brief Implementation of 'compute_k_image' parameter of
  BddTrans_generic_create having 'transition' of type ClusterBasedTrans_ptr

  See struct BddTrans_TAG for specification of this function
*/
static bdd_ptr bdd_trans_clusterlist_compute_k_image(void* transition,
                                                     bdd_ptr bdd, int k,
                                                     TransImageKind kind)
{
  ClusterBasedTrans_ptr trans = (ClusterBasedTrans_ptr) transition;
  switch(kind) {
  case TRANS_IMAGE_FORWARD_STATE:
    return ClusterList_get_k_image_state(trans->forward_trans, bdd, k);

  case TRANS_IMAGE_FORWARD_STATE_INPUT:
    return ClusterList_get_k_image_state_input(trans->forward_trans, bdd,
                                               k);

  case TRANS_IMAGE_BACKWARD_STATE:
    return ClusterList_get_k_image_state(trans->backward_trans, bdd, k);

  case TRANS_IMAGE_BACKWARD_STATE_INPUT:
    return ClusterList_get_k_image_state_input(trans->backward_trans, bdd,
                                               k);

  default:
    {
      DDMgr_ptr dd = ClusterList_get_dd_manager(trans->forward_trans);
      const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));

      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

      ErrorMgr_internal_error(errmgr, "impossible code in bdd_trans_clusterlist_compute_image");
    }
    return NULL;
  } /* switch */
}

/*!
  \brief Implementation of 'get_monolithic_bdd' parameter of
  BddTrans_generic_create having 'transition' of type ClusterBasedTrans_ptr

  See struct BddTrans_TAG for specification of this function
*/
static bdd_ptr bdd_trans_clusterlist_get_monolithic_bdd(void* transition)
{
  ClusterBasedTrans_ptr trans = (ClusterBasedTrans_ptr) transition;
  return ClusterList_get_monolithic_bdd(trans->forward_trans);
}

/*!
  \brief Implementation of 'get_clusterlist' parameter of
  BddTrans_generic_create having 'transition' of type ClusterBasedTrans_ptr

  See struct BddTrans_TAG for specification of this function
*/
static ClusterList_ptr bdd_trans_clusterlist_get_clusterlist(void* transition)
{
  ClusterBasedTrans_ptr trans = (ClusterBasedTrans_ptr) transition;
  return ClusterList_copy(trans->forward_trans);
}

/*!
  \brief Implementation of 'synchronous_product' parameter of
  BddTrans_generic_create having 'transition' of type ClusterBasedTrans_ptr

  See struct BddTrans_TAG for specification of this function
*/
static void bdd_trans_clusterlist_synchronous_product(void* transition1,
                                                      void* const transition2)
{
  ClusterBasedTrans_ptr trans1 = (ClusterBasedTrans_ptr) transition1;
  const ClusterBasedTrans_ptr trans2 = (ClusterBasedTrans_ptr) transition2;

  ClusterList_apply_synchronous_product(trans1->forward_trans,
                                        trans2->forward_trans);

  ClusterList_apply_synchronous_product(trans1->backward_trans,
                                        trans2->backward_trans);
}

/*!
  \brief Implementation of 'print_short_info' parameter of
  BddTrans_generic_create having 'transition' of type ClusterBasedTrans_ptr

  See struct BddTrans_TAG for specification of this function
*/
static void bdd_trans_clusterlist_print_short_info(void* transition,
                                                   FILE* file)
{
  ClusterBasedTrans_ptr trans = (ClusterBasedTrans_ptr) transition;

  fprintf(file, "Forward Partitioning Schedule BDD cluster size (#nodes):\n");
  ClusterList_print_short_info(trans->forward_trans, file);

  fprintf(file, "Backward Partitioning Schedule BDD cluster size (#nodes):\n");
  ClusterList_print_short_info(trans->backward_trans, file);
}
