/* ---------------------------------------------------------------------------


  This file is part of the ``dd'' package of NuSMV version 2.
  Copyright (C) 2010 by FBK-irst.

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
  \brief Implementation of class 'VarsHandler'

  VarsHandler handles the allocation of new variables,
  and their organization within 'groups' (blocks in dd
  terminology). This is done to allow multiple BddEnc instances to
  share the same dd space. For all details see the structure
  description below.

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif


#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/dd/VarsHandler.h"

#include "nusmv/core/set/set.h"
#include "nusmv/core/dd/dd.h"
#include "nusmv/core/dd/ddInt.h"
#include "nusmv/core/utils/Olist.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/defs.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/
enum GroupSearchRes {
  GS_PERF_MATCH_FOUND, /* found group matches perfectly */
  GS_ROOT_FOUND, /* no perfect match, only root found */

  GS_NOT_FOUND, /* the group was not found */
  GS_OVERLAP_FOUND, /* overlap found */
  GS_WRONG_CHUNK, /* not compatible chunk size */
};


/*!
  \brief Minimal block size

  Minimal block size depends on the version of
   CUDD. Cudd-2.4 requires that groups are created also for single
   variables, whereas 2.3 does not allow groups for single
   variables.
*/

#if NUSMV_HAVE_CUDD_24
# define BDD_ENC_MIN_BLOCK_SIZE 1
#else
# define BDD_ENC_MIN_BLOCK_SIZE 2
#endif


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct VarsHandler_TAG
{
  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  DDMgr_ptr dd;
  Olist_ptr forest; /* the set of VarsGroup roots */

  size_t id_counter; /* group id number counter */
} VarsHandler;


typedef struct VarsGroup_TAG {
  int lev_low;  /* low level */
  int lev_high; /* high level */
  int idx_low;  /* index of the lowest level (for traceability) */
  int chunk;    /* minimal group size */
  dd_block* block; /* dd block */

  Olist_ptr gis; /* the set of users of this group (GroupInfo structures) */

  /* tree support structures */
  struct VarsGroup_TAG* parent;
  Olist_ptr children;
} VarsGroup;


typedef struct GroupInfo_TAG {
  /* respect this order! */
  size_t id;    /* id number */
  int lev_low;  /* original group low level (used when searching) */
  int lev_high; /* original group high level (used when searching) */
} GroupInfo;


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

/* VarsHandler related */
static void vars_handler_init(VarsHandler_ptr self, DDMgr_ptr dd);
static void vars_handler_deinit(VarsHandler_ptr self);

static void vars_handler_add_group(VarsHandler_ptr self,
                                   VarsGroup* parent, VarsGroup* group);

static int
vars_handler_remove_group(VarsHandler_ptr self,
                          Olist_ptr list, const GroupInfo_ptr ginfo);

static int vars_handler_update_levels(VarsHandler_ptr self,
                                      VarsGroup* root);

static Oiter vars_handler_promote_group(VarsHandler_ptr self,
                                        Olist_ptr list,
                                        const GroupInfo_ptr ginfo);

static int vars_handler_get_first_free_level(VarsHandler_ptr self,
                                             int from_lev, int size);


/* VarsGroup related */
static VarsGroup*
VarsGroup_create(int lev_low, int lev_high, int idx_low,
                 int chunk);

static void VarsGroup_destroy(VarsGroup* self, DDMgr_ptr dd);

static void VarsGroup_add_ginfo(VarsGroup* group, GroupInfo_ptr gi);
static boolean
VarsGroup_remove_ginfo(VarsGroup* self, GroupInfo_ptr gi);

static Oiter
VarsGroup_find_ginfo(const VarsGroup* self, GroupInfo_ptr gi);

static VarsGroup*
vars_handler_search_group(const VarsHandler_ptr self,
                          int lev_low, size_t size, int chunk,
                          enum GroupSearchRes* res);

static VarsGroup*
vars_group_search_group_aux(const VarsGroup* in_group,
                            int lev_low, size_t size,
                            int chunk,
                            enum GroupSearchRes* res);

static int vars_group_sort(void* g1, void* g2, void* extra);

static void vars_group_print(VarsGroup* group, FILE* _file, int indent);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

VarsHandler_ptr VarsHandler_create(DDMgr_ptr dd)
{
  VarsHandler_ptr self = ALLOC(VarsHandler, 1);
  VARS_HANDLER_CHECK_INSTANCE(self);

  vars_handler_init(self, dd);
  return self;
}

void VarsHandler_destroy(VarsHandler_ptr self)
{
  VARS_HANDLER_CHECK_INSTANCE(self);

  vars_handler_deinit(self);
  FREE(self);
}

DDMgr_ptr VarsHandler_get_dd_manager(const VarsHandler_ptr self)
{
  VARS_HANDLER_CHECK_INSTANCE(self);
  return self->dd;
}

boolean VarsHandler_can_group(const VarsHandler_ptr self,
                              int level, int size, int chunk)
{
  enum GroupSearchRes res;

  (void)vars_handler_search_group(self, level, size, chunk, &res);
  return (GS_OVERLAP_FOUND != res && GS_WRONG_CHUNK != res);
}

GroupInfo_ptr VarsHandler_reserve_group(VarsHandler_ptr self,
                                        int from_lev, int size, int chunk,
                                        boolean can_share, int* lev_low)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  VarsGroup* group = (VarsGroup*) NULL;
  enum GroupSearchRes res = GS_NOT_FOUND;

  VARS_HANDLER_CHECK_INSTANCE(self);

  if (can_share && from_lev >= 0) {
    group = vars_handler_search_group(self, from_lev, size, chunk, &res);
  }

  if (GS_PERF_MATCH_FOUND != res) { /* group has to be created here */
    VarsGroup* child;

    if ((VarsGroup*) NULL == group) {
      /* at root level: creates variables and groups */
      dd_reorderingtype reord_type;
      int reord_status = dd_reordering_status(self->dd, &reord_type);
      int new_lev, new_idx;

      dd_autodyn_disable(self->dd);
      new_lev = vars_handler_get_first_free_level(self, from_lev, size);
      new_idx = dd_get_index_at_level(self->dd, new_lev);

      child = VarsGroup_create(new_lev, new_lev+size-1, new_idx, chunk);
      if (size >= BDD_ENC_MIN_BLOCK_SIZE) {
        const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->dd));
        const OptsHandler_ptr opts =
          OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

        if (opt_verbose_level_gt(opts, 9)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, "VarsHandler: creating physical var block "\
                  "at index %d, size %d\n", new_idx, size);
        }
        CATCH(errmgr) {
          child->block = dd_new_var_block(self->dd, new_idx, size);
        }
        FAIL(errmgr) {
          if (1 == reord_status) {
            dd_autodyn_enable(self->dd, reord_type);
          }
          ErrorMgr_internal_error(errmgr, "vars_handler: Failure during variable group construction\n");
        }
      }
      if (1 == reord_status) {
        dd_autodyn_enable(self->dd, reord_type);
      }
    }
    else {
      /* a containing group already exists, creates a logical
         sub-group of the found root */
      child = VarsGroup_create(from_lev, from_lev+size-1,
                               dd_get_index_at_level(self->dd, from_lev),
                               chunk);
    }

    vars_handler_add_group(self, group, child);
    group = child;
  }

  { /* result group info constrution */
    GroupInfo_ptr ginfo = ALLOC(GroupInfo, 1);
    nusmv_assert((GroupInfo_ptr) NULL != ginfo);
    ginfo->id = self->id_counter++;
    ginfo->lev_low = group->lev_low;
    ginfo->lev_high = group->lev_low+size-1;

    VarsGroup_add_ginfo(group, ginfo);

    *lev_low = group->lev_low;
    return ginfo;
  }
}

boolean VarsHandler_release_group(VarsHandler_ptr self, GroupInfo_ptr gid)
{
  int num = 0;

  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  VARS_HANDLER_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->dd));
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (opt_verbose_level_gt(opts, 9)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "VarsHandler: freeing group: id=%" PRIuPTR \
            " low=%d, len=%d\n",
            gid->id, gid->lev_low, gid->lev_high-gid->lev_low+1);
  }

  /* checks if this groups is still valid */
  if (0 <= gid->lev_low && 0 <= gid->lev_high) {
    num = vars_handler_remove_group(self, self->forest, gid);
  }

  FREE(gid);
  return num != 0;
}

void VarsHandler_dissolve_group(VarsHandler_ptr self, GroupInfo_ptr gid)
{
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;

  VARS_HANDLER_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->dd));
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (opt_verbose_level_gt(opts, 9)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
            "VarsHandler: dissolving group: id=%" PRIuPTR \
            " low=%d, len=%d\n",
            gid->id, gid->lev_low, gid->lev_high-gid->lev_low+1);
  }

  VarsHandler_update_levels(self);

  /* checks if this groups is still valid */
  if (0 <= gid->lev_low && 0 <= gid->lev_high) {
    VarsGroup* group;
    Oiter iter = vars_handler_promote_group(self, self->forest, gid);

    Olist_delete(self->forest, iter, (void**) &group);
    VarsGroup_destroy(group, self->dd);
  }

  FREE(gid);
}

void VarsHandler_update_levels(VarsHandler_ptr self)
{
  Oiter iter;
  int prev_delta = INT_MAX;
  boolean order_may_change = false;

  VARS_HANDLER_CHECK_INSTANCE(self);


  OLIST_FOREACH(self->forest, iter) {
    VarsGroup* child = Oiter_element(iter);
    int delta = vars_handler_update_levels(self, child);

    order_may_change |= (prev_delta != INT_MAX && prev_delta != delta);
    prev_delta = delta;
  }

  if (order_may_change) Olist_sort(self->forest, vars_group_sort, NULL);
}

void VarsHandler_print(const VarsHandler_ptr self, FILE* _file)
{
  Oiter iter;
  VARS_HANDLER_CHECK_INSTANCE(self);

  OLIST_FOREACH(self->forest, iter) {
    VarsGroup* child = Oiter_element(iter);
    vars_group_print(child, _file, 0);
  }
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Internal service for VarsHandler_release_group


*/
static int vars_handler_remove_group(VarsHandler_ptr self,
                                     Olist_ptr list,
                                     const GroupInfo_ptr ginfo)
{
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;
  int removed;
  Oiter iter;

  VARS_HANDLER_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->dd));
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  removed = 0;
  iter = Olist_first(list);

  while (!Oiter_is_end(iter)) {

    VarsGroup* group = (VarsGroup*) Oiter_element(iter);
    if (ginfo->lev_high < group->lev_low) break; /* search is over */

    if (ginfo->lev_low <= group->lev_low &&
        ginfo->lev_high >= group->lev_high) {
      /* group id can be here and in children */
      boolean res = VarsGroup_remove_ginfo(group, ginfo);

      if (res && Olist_is_empty(group->gis)) {
        /* this group can be destroyed, and children moved up */
        if ((Olist_ptr) NULL != group->children) {
          Oiter insert_iter = iter;
          Oiter iter2;
          OLIST_FOREACH(group->children, iter2) {
            VarsGroup* child = (VarsGroup*) Oiter_element(iter2);
            child->parent = group->parent;
            insert_iter = Olist_insert_after(list, insert_iter, child);
          }
        }

        if (opt_verbose_level_gt(opts, 9)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger,
                  "VarsHandler: removing %s group at level %d, size %d\n",
                  ((dd_block*) NULL != group->block) ? "physical" : "logical",
                  group->lev_low, group->lev_high-group->lev_low+1);
        }

        /* if physical, removes the dd block as well */
        if ((dd_block*) NULL != group->block) {
          int res = dd_free_var_block(self->dd, group->block);
          nusmv_assert(0 == res);
          group->block = (dd_block*) NULL;
        }

        ++removed;
        iter = Olist_delete(list, iter, NULL);
        nusmv_assert(NULL != group);
        VarsGroup_destroy(group, self->dd);
        continue;
      }
      else if ((Olist_ptr) NULL != group->children) {
        /* not find in the parent, search within the children */
        removed += vars_handler_remove_group(self, group->children, ginfo);
      }
    }

    iter = Oiter_next(iter);
  } /* loop on input list */

  return removed;
}

/*!
  \brief Brings the given group at top level (along with all its
  children), splitting all the parents accordingly.

  Internal service of
  VarsHandler_dissolve_group, as before dissolving
  groups, they has to be brough to the top-level. Returns an
  iterator to the top level which contains the group.
*/
static Oiter vars_handler_promote_group(VarsHandler_ptr self,
                                        Olist_ptr list,
                                        const GroupInfo_ptr ginfo)
{
  ErrorMgr_ptr errmgr;
  NuSMVEnv_ptr env;
  OptsHandler_ptr opts;
  Oiter iter;

  VARS_HANDLER_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self->dd));
  opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  iter = Olist_first(list);
  while (!Oiter_is_end(iter)) {

    VarsGroup* group = (VarsGroup*) Oiter_element(iter);
    if (ginfo->lev_high < group->lev_low) { /* search is over */
      iter = Olist_end(list);
      break;
    }

    if (ginfo->lev_low <= group->lev_low &&
        ginfo->lev_high >= group->lev_high) {
      /* group id can be here and in children */
      if (!Oiter_is_end(VarsGroup_find_ginfo(group, ginfo))) {
        /* found the group, returns it up, after  */
        return iter;
      }
      /* may be in children */
      else if ((Olist_ptr) NULL != group->children) {
        Oiter iter_child = vars_handler_promote_group(self, group->children,
                                                      ginfo);
        /* if found, substitutes at the current iteration point */
        if (!Oiter_is_end(iter_child)) {
          Oiter iter2, next;

          if ((dd_block*) NULL != group->block) {
            /* this is a physical block, dissolve it and creates
               new blocks out of its children */
            int res = dd_free_var_block(self->dd, group->block);
            nusmv_assert(0 == res);
            group->block = (dd_block*) NULL;

            /* moves up the children */
            OLIST_FOREACH(group->children, iter2) {
              VarsGroup* child = (VarsGroup*) Oiter_element(iter2);
              if (opt_verbose_level_gt(opts, 9)) {
                Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
                Logger_log(logger, "VarsHandler: promoting physical var block " \
                        "at index %d, size %d\n", child->idx_low,
                        child->lev_high - child->lev_low + 1);
              }

              CATCH(errmgr) {
                nusmv_assert(child->lev_low == ginfo->lev_low &&
                             child->lev_high == ginfo->lev_high);
                child->block =
                  dd_new_var_block(self->dd, child->idx_low,
                                   child->lev_high - child->lev_low + 1);
              }
              FAIL(errmgr) {
                ErrorMgr_internal_error(errmgr, "vars_handler: Failure during variable group construction\n");
              }
            }
          }

          /* the set of pointing groupinfo has to be set as eliminated */
          OLIST_FOREACH(group->gis, iter2) {
            GroupInfo_ptr ginfo = Oiter_element(iter2);
            ginfo->lev_low = -1;
            ginfo->lev_high = -1;
          }

          next = Olist_delete(list, iter, NULL);
          Olist_move(group->children, list, next);
        }
        return iter;
      }
    }

    iter = Oiter_next(iter);
  }

  return iter;
}

/*!
  \brief Searches a good place where the group can be inserted.

  This method is used for searching a given group, or
  for searching a good place where a new group can be
  inserted.

  There are four input parameters: The VarsHandler instances, the
  minimal level at which a group shall be searched from, the size
  of the searched group, and the chunk size (2 for state variables,
  1 for frozen and input).

  There are three output parameters: a group, the actual found
  minimal level, and a value identifying the result of the search.

  If a perfect match is found (the searched group is found), the
  group is returned, and lev_low will be kept untouched.

  If a root is found, it means that the searched group does not
  exist, but a group was found which can contain it. In this case
  the parent is returned.

  If not found, NULL is returned as VarsGroup, and lev_low will
  contain the first usable level which can be used to create a new
  top-level group (see how this case is used in
  VarsHandler_reserve_group)
*/
static VarsGroup* vars_handler_search_group(const VarsHandler_ptr self,
                                            int lev_low, size_t size,
                                            int chunk,
                                            enum GroupSearchRes* res)
{
  Oiter iter;
  OLIST_FOREACH(self->forest, iter) {
    VarsGroup* root = (VarsGroup*) Oiter_element(iter);
    VarsGroup* group = vars_group_search_group_aux(root, lev_low, size,
                                                   chunk, res);
    if ((VarsGroup*) NULL != group) return group;
    if (GS_NOT_FOUND != *res) break;
  }

  return (VarsGroup*) NULL;
}

/*!
  \brief Adds the given group to the forest

  If parent is NULL, group has to be a physical
*/
static void vars_handler_add_group(VarsHandler_ptr self,
                                   VarsGroup* parent, VarsGroup* group)
{
  Olist_ptr list;
  Oiter iter;

  if ((VarsGroup*) NULL == parent) {
    /* it has to be a physical group */
    nusmv_assert((dd_block*) NULL != group->block);
    list = self->forest;
  }
  else {
    /* it has to be a logical group */
    nusmv_assert((dd_block*) NULL == group->block);
    if ((Olist_ptr) NULL == parent->children) {
      parent->children = Olist_create();
    }
    list = parent->children;
    group->parent = parent;
  }

  OLIST_FOREACH(list, iter) {
    VarsGroup* el = (VarsGroup*) Oiter_element(iter);
    if (group->lev_low < el->lev_low) { /* insert point */
      nusmv_assert(group->lev_high < el->lev_low); /* no overlap */
      Olist_insert_before(list, iter, group);
      return;
    }
  }

  /* append */
  Olist_append(list, group);
}

/*!
  \brief Updates the levels of the root and all its children

  Internal service of VarsHandler_update_levels
*/
static int vars_handler_update_levels(VarsHandler_ptr self, VarsGroup* root)
{
  const int delta = (dd_get_level_at_index(self->dd, root->idx_low) -
                     root->lev_low);
  Oiter iter;

  if (0 != delta) {
    root->lev_low += delta;
    root->lev_high += delta;

    OLIST_FOREACH(root->gis, iter) {
      GroupInfo_ptr ginfo = Oiter_element(iter);
      if (0 <= ginfo->lev_low && 0 <= ginfo->lev_high) {
        ginfo->lev_low += delta;
        ginfo->lev_high += delta;
      }
    }
    /* ordering of gis is kept */
  }

  if ((Olist_ptr) NULL != root->children) { /* update children */
    int prev_child_delta = INT_MAX;
    boolean children_order_may_change = false;
    OLIST_FOREACH(root->children, iter) {
      VarsGroup* child = Oiter_element(iter);
      int child_delta = vars_handler_update_levels(self, child);

      children_order_may_change |= (prev_child_delta != INT_MAX &&
                                    prev_child_delta != child_delta);
      prev_child_delta = child_delta;
    }

    if (children_order_may_change) {
      Olist_sort(root->children, vars_group_sort, NULL);
    }
  }

  return delta;
}

/*!
  \brief Searches the closest group

  Internal service of vars_group_search_group
*/
static VarsGroup* vars_group_search_group_aux(const VarsGroup* in_group,
                                              int lev_low, size_t size,
                                              int chunk,
                                              enum GroupSearchRes* res)
{
  int lev_high = lev_low + size - 1;

  /* perfect match */
  if (lev_low == in_group->lev_low &&
      lev_high == in_group->lev_high &&
      chunk == in_group->chunk) {
    *res = GS_PERF_MATCH_FOUND;
    return (VarsGroup*) in_group;
  }

  /* match fail: outside */
  if (lev_low > in_group->lev_high || lev_high < in_group->lev_low) {
    *res = GS_NOT_FOUND;
    return (VarsGroup*) NULL;
  }

  /* match fail: overlap */
  if ((lev_low < in_group->lev_low && lev_high >= in_group->lev_low) ||
      (lev_high > in_group->lev_high && lev_low <= in_group->lev_high)) {
    *res = GS_OVERLAP_FOUND;
    return (VarsGroup*) NULL;
  }

  /* wrong chunk size */
  if (chunk != in_group->chunk) {
    *res = GS_WRONG_CHUNK;
    return (VarsGroup*) NULL;
  }

  /* here is inside, search the most precise sub-group */
  nusmv_assert(lev_low >= in_group->lev_low && lev_high <= in_group->lev_high);

  /* searches among the children */
  if ((Olist_ptr) NULL != in_group->children ) {
    Oiter iter;
    OLIST_FOREACH(in_group->children, iter) {
      VarsGroup* child = Oiter_element(iter);

      if (lev_low >= child->lev_low && lev_high <= child->lev_high) {
        /* the group has to be in child */
        return vars_group_search_group_aux(child, lev_low, size, chunk, res);
      }

      if (lev_low < child->lev_low) break; /* it is over */
    } /* for in_group's children */
  }

  *res = GS_ROOT_FOUND;
  return (VarsGroup*) in_group;
}

/*!
  \brief The VarsHandler class private initializer

  The VarsHandler class private initializer

  \sa VarsHandler_create
*/
static void vars_handler_init(VarsHandler_ptr self, DDMgr_ptr dd)
{
  /* members initialization */
  nusmv_assert((DDMgr_ptr ) NULL != dd);
  self->dd = dd;

  self->forest = Olist_create();
  self->id_counter = 0;
}

/*!
  \brief The VarsHandler class private deinitializer

  The VarsHandler class private deinitializer

  \sa VarsHandler_destroy
*/
static void vars_handler_deinit(VarsHandler_ptr self)
{
  /* members deinitialization */
  Oiter iter;
  OLIST_FOREACH(self->forest, iter) {
    VarsGroup* group = Oiter_element(iter);
    VarsGroup_destroy(group, self->dd);
  }
  Olist_destroy(self->forest);
}

/*!
  \brief VarsGroups cosntructor


*/
static VarsGroup* VarsGroup_create(int lev_low, int lev_high, int idx_low,
                                   int chunk)
{
  VarsGroup* self = ALLOC(VarsGroup, 1);
  nusmv_assert((VarsGroup*) NULL != self);

  nusmv_assert(lev_low <= lev_high);
  nusmv_assert(chunk > 0);

  self->lev_low = lev_low;
  self->lev_high = lev_high;
  self->chunk = chunk;
  self->idx_low = idx_low;
  self->block = (dd_block*) NULL;

  self->gis = Olist_create();
  self->parent = (VarsGroup*) NULL;
  self->children = (Olist_ptr) NULL;

  return self;
}

/*!
  \brief VarsGroups destroyer

  Traverses all the tree, and frees it
*/
static void VarsGroup_destroy(VarsGroup* self, DDMgr_ptr dd)
{
  Oiter iter;

  if ((dd_block*) NULL != self->block) {
    int res = dd_free_var_block(dd, self->block);
    nusmv_assert(0 == res);
    self->block = (dd_block*) NULL;
  }

  /* invalidates all pointing group info  */
  OLIST_FOREACH(self->gis, iter) {
    GroupInfo_ptr ginfo = Oiter_element(iter);
    ginfo->lev_low = -1;
    ginfo->lev_high = -1;
  }
  Olist_destroy(self->gis);

  if ((Olist_ptr) NULL != self->children) {
    Oiter iter;
    OLIST_FOREACH(self->children, iter) {
      VarsGroup* child = Oiter_element(iter);
      VarsGroup_destroy(child, dd);
    }
    Olist_destroy(self->children);
  }

  FREE(self);
}

/*!
  \brief Adds the given GroupInfo to the given VarsGroup

  Since internal list of group info is sorted for the
  sake of good performances, this method inserts in order.
*/
static void VarsGroup_add_ginfo(VarsGroup* self, GroupInfo_ptr gi)
{
  Oiter iter;

  OLIST_FOREACH(self->gis, iter) {
    GroupInfo_ptr _gi = (GroupInfo_ptr) Oiter_element(iter);
    nusmv_assert(gi->id != _gi->id); /* not already created id */

    if (_gi->id > gi->id) { /* found insertion point */
      Olist_insert_before(self->gis, iter, (void*) gi);
      return;
    }
  }

  /* append */
  Olist_append(self->gis, (void*) gi);
}

/*!
  \brief Removes the id if found.

  Returns: true if found, false otherwise.

  \se Changes the gis internal list. Do not call when
  iterating on it.
*/
static boolean VarsGroup_remove_ginfo(VarsGroup* self, GroupInfo_ptr gi)
{
  Oiter iter = VarsGroup_find_ginfo(self, gi);
  if (!Oiter_is_end(iter)) {
    Olist_delete(self->gis, iter, (void**) NULL);
    return true;
  }
  return false;
}

/*!
  \brief Searches the given group information.

  Returns the iterator pointing to it in the internal list
*/
static Oiter VarsGroup_find_ginfo(const VarsGroup* self, GroupInfo_ptr gi)
{
  Oiter iter;

  OLIST_FOREACH(self->gis, iter) {
    GroupInfo_ptr _gi = (GroupInfo_ptr) Oiter_element(iter);
    if (_gi->id == gi->id) break;
    if (_gi->id > gi->id) {
      iter = Olist_end(self->gis);
      break; /* limit found */
    }
  }

  return iter;
}

/*!
  \brief Internal service used when sorting VarsGroup lists


*/
static int vars_group_sort(void* _g1, void* _g2, void* extra)
{ return (((VarsGroup*)_g1)->lev_low - ((VarsGroup*)_g2)->lev_low); }


/*!
  \brief Internal service used when printing groups


*/

static void vars_group_print(VarsGroup* group, FILE* _file, int indent)
{
  int i;
  for (i=indent; i>0; --i) fprintf(_file, "  ");
  fprintf(_file, "%s %d-%d idx:%d chunk:%d (%d users:",
          (NULL != group->block) ? "P" : "L",
          group->lev_low, group->lev_high, group->idx_low, group->chunk,
          Olist_get_size(group->gis));

  {
    Oiter iter;
    OLIST_FOREACH(group->gis, iter) {
      GroupInfo_ptr ginfo = Oiter_element(iter);
      fprintf(_file, " %" PRIuPTR, ginfo->id);
    }
    fprintf(_file, ")\n");
  }

  if ((Olist_ptr) NULL != group->children) {
    Oiter iter;
    OLIST_FOREACH(group->children, iter) {
      VarsGroup* child = Oiter_element(iter);
      vars_group_print(child, _file, indent + 1);
    }
  }
}

/*!
  \brief Searches among the available groups for holes, and
  return the first free level starting from given level.

  If needed, creates size new variables from the
  frontier, in particular, if from_lev is < 0.
*/
static int vars_handler_get_first_free_level(VarsHandler_ptr self,
                                             int from_lev, int size)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self->dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (from_lev >= 0) {
    int _from_lev = from_lev;
    Oiter iter;


    /* searches for a gap in already created levels */
    OLIST_FOREACH(self->forest, iter) {
      VarsGroup* curr = Oiter_element(iter);

      /* sees if there is a usable gap between requested level and curr */
      if (curr->lev_low - _from_lev >= size) {
        return _from_lev;
      }

      /* found the searched threshold: now searches above it */
      if (curr->lev_low >= from_lev) {
        _from_lev = curr->lev_high+1;
      }
    }

    /* there is space after the last group and the frontier */
    if (_from_lev+size-1 < dd_get_size(self->dd)) return _from_lev;
  }

  { /* no previously created level is available: create new
       variables from the frontier */
    dd_reorderingtype reord_type;
    int reord_status = dd_reordering_status(self->dd, &reord_type);
    int new_idx = dd_get_size(self->dd);
    int i;

    /* avoid index 0 */
    if (0 == new_idx) new_idx += 1;

    if (new_idx >= MAX_VAR_INDEX) ErrorMgr_error_too_many_vars(errmgr);

    dd_autodyn_disable(self->dd);
    for (i=0; i<size; ++i) {
      add_ptr add = add_new_var_with_index(self->dd, i+new_idx);
      nusmv_assert(dd_get_level_at_index(self->dd, i+new_idx) == i+new_idx);
      add_free(self->dd, add);
    }

    if (1 == reord_status) {
      dd_autodyn_enable(self->dd, reord_type);
    }
    return new_idx; /* index and level coincide in this case */
  }
}



/**AutomaticEnd***************************************************************/
