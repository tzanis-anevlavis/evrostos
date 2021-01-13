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
  \brief Routines related to clusters of transition relation. 

   This file conains the definition of Cluster and all derived 
  classes

*/


#include "nusmv/core/trans/bdd/Cluster.h"
#include "nusmv/core/utils/object_private.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct Cluster_TAG
{
  INHERITS_FROM(Object);
  
  /* The current Cluster */
  bdd_ptr curr_cluster; /* was Ti */            
  
  /* Variables that can be existentially quantified when curr_cluster is
     multiplied in the product (state and input vars) */ 
  bdd_ptr ex_state_input; /* was Ei_StateInput */
  
  /* Variables that can be existentially quantified when curr_cluster is
     multiplied in the product (only state vars) */ 
  bdd_ptr ex_state; /* was Ei_State */

/* ---------------------------------------------------------------------- */ 
/*     Virtual Methods                                                    */
/* ---------------------------------------------------------------------- */   
  
} Cluster;

typedef struct ClusterIwls95_TAG
{
  INHERITS_FROM(Cluster);
  
  double benefit;

} ClusterIwls95; 

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void cluster_init(Cluster_ptr self, DDMgr_ptr dd);
static void cluster_deinit(Cluster_ptr self, DDMgr_ptr dd);
static void cluster_finalize(Object_ptr object, void* arg);
static Object_ptr cluster_copy(const Object_ptr object);

static void cluster_copy_aux(const Cluster_ptr object, 
                             Cluster_ptr copy);

static void cluster_iwls95_init(ClusterIwls95_ptr self, 
                                DDMgr_ptr dd,  
                                const ClusterOptions_ptr cl_options, 
                                const double v_c, 
                                const double w_c, 
                                const double x_c, 
                                const double y_c, 
                                const double z_c, 
                                const double min_c, 
                                const double max_c);

static void cluster_iwls95_deinit(ClusterIwls95_ptr self, DDMgr_ptr dd);
static void cluster_iwls95_finalize(Object_ptr object, void* arg);

static Object_ptr cluster_iwls95_copy(const Object_ptr object);

static void 
cluster_iwls95_copy_aux(const ClusterIwls95_ptr object, 
                        ClusterIwls95_ptr copy);

Cluster_ptr Cluster_create(DDMgr_ptr dd)
{
  Cluster_ptr self = ALLOC(Cluster, 1);
  CLUSTER_CHECK_INSTANCE(self);

  cluster_init(self, dd);
  return self;
}

boolean Cluster_is_equal(const Cluster_ptr self, const Cluster_ptr other)
{
  CLUSTER_CHECK_INSTANCE(self);
  CLUSTER_CHECK_INSTANCE(other);

  return (self->curr_cluster == other->curr_cluster);
}

bdd_ptr Cluster_get_trans(const Cluster_ptr self)
{
  bdd_ptr tmp = (bdd_ptr)NULL;
  
  CLUSTER_CHECK_INSTANCE(self);
  if (self->curr_cluster != (bdd_ptr)NULL) {
    tmp = bdd_dup(self->curr_cluster);
  }

  return tmp;  
}

void Cluster_set_trans(Cluster_ptr self, DDMgr_ptr dd, bdd_ptr current)
{
  CLUSTER_CHECK_INSTANCE(self);

  if (self->curr_cluster != (bdd_ptr) NULL) {
    bdd_free(dd, self->curr_cluster);
    self->curr_cluster = (bdd_ptr) NULL;
  }
  if (current != (bdd_ptr) NULL) {
    self->curr_cluster = bdd_dup(current);
  }
}

bdd_ptr Cluster_get_quantification_state_input(const Cluster_ptr self)
{
  bdd_ptr tmp = (bdd_ptr)NULL;
  
  CLUSTER_CHECK_INSTANCE(self);
  if (self->ex_state_input != (bdd_ptr) NULL) {
    tmp = bdd_dup(self->ex_state_input);
  }

  return tmp;  
}

void Cluster_set_quantification_state_input(Cluster_ptr self, 
                                            DDMgr_ptr dd, bdd_ptr new)
{
  CLUSTER_CHECK_INSTANCE(self);

  if (self->ex_state_input != (bdd_ptr) NULL) {
    bdd_free(dd, self->ex_state_input);
    self->ex_state_input = (bdd_ptr) NULL;
  }
  if (new != (bdd_ptr) NULL) {
    self->ex_state_input = bdd_dup(new);
  }
}

bdd_ptr Cluster_get_quantification_state(const Cluster_ptr self)
{
  bdd_ptr tmp = (bdd_ptr)NULL;
  
  CLUSTER_CHECK_INSTANCE(self);
  if (self->ex_state != (bdd_ptr) NULL) {
    tmp = bdd_dup(self->ex_state);
  }

  return tmp;  
}

void Cluster_set_quantification_state(Cluster_ptr self, 
                                      DDMgr_ptr dd, bdd_ptr new)
{
  CLUSTER_CHECK_INSTANCE(self);

  if (self->ex_state != (bdd_ptr) NULL) {
    bdd_free(dd, self->ex_state);
    self->ex_state = (bdd_ptr) NULL;
  }

  if (new != (bdd_ptr) NULL) {
    self->ex_state = bdd_dup(new);
  }
}


/* ====================================================================== */

ClusterIwls95_ptr ClusterIwls95_create(DDMgr_ptr dd, 
                                       const ClusterOptions_ptr cl_options, 
                                       const double v_c, 
                                       const double w_c, 
                                       const double x_c, 
                                       const double y_c, 
                                       const double z_c, 
                                       const double min_c, 
                                       const double max_c)
{
  ClusterIwls95_ptr self =  ALLOC(ClusterIwls95, 1);

  CLUSTER_IWLS95_CHECK_INSTANCE(self); 

  cluster_iwls95_init(self, dd, cl_options, v_c, w_c, x_c, 
                      y_c, z_c, min_c, max_c);
  return self;
}

double ClusterIwls95_get_benefit(const ClusterIwls95_ptr self)
{
  CLUSTER_IWLS95_CHECK_INSTANCE(self); 
  return self->benefit;
}

/*!
  \brief Initializes the cluster with default values.

  
*/
static void cluster_init(Cluster_ptr self, DDMgr_ptr dd)
{
  object_init(OBJECT(self));

  self->curr_cluster    = bdd_true(dd);
  self->ex_state_input  = bdd_true(dd);
  self->ex_state        = bdd_true(dd);

  OVERRIDE(Object, finalize) = cluster_finalize;
  OVERRIDE(Object, copy) = cluster_copy; 
}

/*!
  \brief Deinitializes the cluster. 

  Releases the contained bdds.
*/
static void cluster_deinit(Cluster_ptr self, DDMgr_ptr dd)
{
  object_deinit(OBJECT(self));

  /* Releases contained bdds: */
  if (self->curr_cluster != (bdd_ptr) NULL) bdd_free(dd, self->curr_cluster);
  if (self->ex_state_input != (bdd_ptr) NULL) bdd_free(dd, self->ex_state_input);
  if (self->ex_state != (bdd_ptr) NULL) bdd_free(dd, self->ex_state);
}

/*!
  \brief  Finalize a cluster.

  
*/
static void cluster_finalize(Object_ptr object, void* arg)
{
  Cluster_ptr self = CLUSTER(object); 

  cluster_deinit(self, (DDMgr_ptr ) arg);
  FREE(self);
}

/*!
  \brief  Copies the given cluster.

  It is the callback function that the copy constructor
  virtually calls.

  \sa cluster_copy_aux
*/
static Object_ptr cluster_copy(const Object_ptr object)
{
  Cluster_ptr self = CLUSTER(object);
  Cluster_ptr copy = CLUSTER(NULL);

  copy = ALLOC(Cluster, 1);
  CLUSTER_CHECK_INSTANCE(copy);

  cluster_copy_aux(self, copy);
  return OBJECT(copy);
}

/*!
  \brief It helps to copy the given cluster.

  

  \sa cluster_copy
*/
static void cluster_copy_aux(const Cluster_ptr self, Cluster_ptr copy)
{
  /* copies the base class: */
  object_copy_aux(OBJECT(self), OBJECT(copy));

  /* copies class members: */
  copy->curr_cluster = bdd_dup(self->curr_cluster);  
  copy->ex_state_input = bdd_dup(self->ex_state_input); 
  copy->ex_state = bdd_dup(self->ex_state);
}

/*!
  \brief  Initializes Iwls95 cluster. 

  The parameters passed to this private function correspond
  to cluster options and different factors (v_c, w_c, x_c, y_c, z_c, min_c and
  max_c) as explained in IWLS95 paper.
*/
static void cluster_iwls95_init(ClusterIwls95_ptr self, 
                                DDMgr_ptr dd, 
                                const ClusterOptions_ptr cl_options, 
                                const double v_c, 
                                const double w_c, 
                                const double x_c, 
                                const double y_c, 
                                const double z_c, 
                                const double min_c, 
                                const double max_c)

{
  double w1; 
  double w2; 

  cluster_init( CLUSTER(self), dd );
  
  w1 = (double) ClusterOptions_get_w1(cl_options); 
  w2 = (double) ClusterOptions_get_w2(cl_options); 
  
  if (w_c != 0)  self->benefit = (v_c / w_c) * w1; 
  else  self->benefit = 0.0; 

  if (x_c != 0)   self->benefit += (w_c / x_c) * w2; 
  if (z_c != 0)   self->benefit -= (y_c / z_c) * w2;
  if (max_c != 0) self->benefit += (min_c / max_c) * w2;

  OVERRIDE(Object, finalize) = cluster_iwls95_finalize;
  OVERRIDE(Object, copy) = cluster_iwls95_copy; 
}

/*!
  \brief  Deinitialized Iwls95 cluster. 

  
*/
static void cluster_iwls95_deinit(ClusterIwls95_ptr self, DDMgr_ptr dd)
{
  /* nothing to clean up in self */
  cluster_deinit( CLUSTER(self), dd );
}

/*!
  \brief  Finalize iwls95 cluster. 

   The virtual destructor calls this method to destroy the
  instance self.
*/
static void cluster_iwls95_finalize(Object_ptr object, void* arg)
{
  ClusterIwls95_ptr self = CLUSTER_IWLS95(object); 
  cluster_iwls95_deinit(self, (DDMgr_ptr ) arg);
  FREE(self);
}

/*!
  \brief  Copies iwls95 cluster.

   Callback function that copy constructor virtually calls
  to copy an instance of iwls95 cluster.

  \sa cluster_iwls95_copy_aux
*/
static Object_ptr cluster_iwls95_copy(const Object_ptr object)
{
  ClusterIwls95_ptr self = CLUSTER_IWLS95(object); 
  ClusterIwls95_ptr copy;

  copy = ALLOC(ClusterIwls95, 1);
  CLUSTER_IWLS95_CHECK_INSTANCE(copy); 

  cluster_iwls95_copy_aux(self, copy);
  return OBJECT(copy); 
}

/*!
  \brief  It helps to copy iwls95 cluster.

  

  \sa cluster_iwls95_copy
*/
static void cluster_iwls95_copy_aux(const ClusterIwls95_ptr self, 
                                    ClusterIwls95_ptr copy)
{
  cluster_copy_aux(CLUSTER(self), CLUSTER(copy));

  copy->benefit = self->benefit;
}

