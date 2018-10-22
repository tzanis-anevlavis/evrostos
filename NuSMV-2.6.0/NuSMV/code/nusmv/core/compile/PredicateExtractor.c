/* ---------------------------------------------------------------------------


   This file is part of the ``compile'' package of NuSMV version 2.
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
  \author Andrei Tchaltsev
  \brief A Predicate-Extractor class

  See PredicateExtractor.h for more info

*/



#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/compile/PredicateExtractor.h"
#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/WordNumberMgr.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"
#include "nusmv/core/utils/EnvObject_private.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct PredicateExtractor_TAG
{
  INHERITS_FROM(EnvObject);

  Set_t all_preds;   /* all predicates : Set_t of node_ptr */
  Set_t unclustered_preds;   /* subset of all_preds for which clusters
                                were not computed : Set_t of node_ptr */
  Set_t all_clusters;   /* all clusters : Set_t of Set_t of node_ptr.
                           This is the actual owner of all clusters.*/

  hash_ptr var2cluster; /* var -> cluster it belongs to. node_ptr -> Set_t */
  hash_ptr cluster2preds;   /* cluster -> its predicates. Owner of preds sets.
                               Set_t -> Set_t of node_ptr */

  hash_ptr expr2preds; /* node_ptr -> Set_t of node_ptr.  For
                          not-boolean expr the associated value is set
                          of subparts of predicates in it.  For
                          processed boolean expressions the associated
                          value is one of PREDICATES_TRUE (if the
                          expression can be simplified to constant
                          true), PREDICATES_FALSE (if expression can
                          be simplified to FALSE) or
                          PREDICATES_ARBITRARY (for all other cases).
                          This hash is the owner of preds sets.
                       */
  Set_t special_word_preds[3]; /* array of 3 special predicates subparts:
                                  {0d1_0}, {0d1_1}, and {0d1_0,0d1_1} */

  TypeChecker_ptr checker; /* type-checker is used to get type info
                              of processed expressions and type check
                              generated expressions */
  SymbTable_ptr st;  /* the symbol table */
  boolean use_approx; /* if over-approximation has to be used when
                         extracting predicates (see issue 1934) */

} PredicateExtractor;


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief These are special values to mark expressions
   which have been analyzed and which do not have predicate subparts

  Any boolean expression cannot have predicate subparts because
   boolean may have only complete predicates, not its subparts.
   Apparently, only not-boolean operation expressions may have
   predicates subparts.  These values are used in self->expr2preds
   hash.
   For better optimizations simplifications 3 values are introduced:
   PREDICATES_TRUE -- represent a set of predicates, consisting of {TRUE} only.
   PREDICATES_FALSE -- represent a set of predicates, consisting of {FALSE} only.
   PREDICATES_ARBITRARY -- represent arbitrary set of predicates.

   PREDICATES_OVERAPPROX -- represent approximanted value (to give
   up the extraction)

*/
#define PREDICATES_TRUE ((Set_t)1)

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define PREDICATES_FALSE ((Set_t)2)

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define PREDICATES_ARBITRARY ((Set_t)3)

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define PREDICATES_OVERAPPROX ((Set_t)4)

/*!
  \brief The threshold used when deciding whether over approximate
   or not the predicate extraction


*/
#define OVER_APPROX_THRESHOLD 600000


/* below macro is TRUE iff the set is not an actually a Set_t,
   i.e. it is a valide predicate constant or the constant
   indicating the overapproximation.
   Note: that expression 'set' should not include function call. */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define IS_FLAG_PREDICATES(set)                                 \
 (IS_FLAG_VALID_PREDICATES(set) || IS_OVER_APPROX(set))

/* below macro is TRUE iff the set is not an actually a Set_t,
   i.e. it is one of the constant values PREDICATES_TRUE,
   PREDICATES_FALSE or PREDICATES_ARBITRARY.
   Note: that expression 'set' should not include function call. */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define IS_FLAG_VALID_PREDICATES(set)                           \
 ((set)==PREDICATES_TRUE||(set)==PREDICATES_FALSE||(set)==PREDICATES_ARBITRARY)



/* below macro is TRUE iff the set is not an actually a Set_t, but it represents
   an over-approximation, i.e. it is PREDICATES_OVERAPPROX */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define IS_OVER_APPROX(set)                     \
  ((set)==PREDICATES_OVERAPPROX)

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/


static void pred_extract_init(PredicateExtractor_ptr self,
                              SymbTable_ptr st, boolean use_approx);

static void pred_extract_deinit(PredicateExtractor_ptr self);

static void pred_extract_finalize(Object_ptr self, void* dummy);

static Set_t pred_extract_process_recur(PredicateExtractor_ptr self,
                                        node_ptr expr,
                                        node_ptr context);

static boolean pred_extract_is_bool_preds(PredicateExtractor_ptr self,
                                          Set_t result);

static Set_t pred_extract_fix_any_preds(PredicateExtractor_ptr self,
                                        Set_t result);

static Set_t pred_extract_apply_unary(PredicateExtractor_ptr self,
                                      int type,
                                      Set_t childResult);

static Set_t pred_extract_apply_binary(PredicateExtractor_ptr self,
                                       int type,
                                       Set_t leftResult,
                                       Set_t rightResult);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

PredicateExtractor_ptr PredicateExtractor_create(SymbTable_ptr st,
                                                 boolean use_approx)
{
  PredicateExtractor_ptr self = ALLOC(PredicateExtractor, 1);

  PREDICATE_EXTRACTOR_CHECK_INSTANCE(self);

  pred_extract_init(self, st, use_approx);
  return self;
}

void PredicateExtractor_destroy(PredicateExtractor_ptr self)
{
  PREDICATE_EXTRACTOR_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

void
PredicateExtractor_compute_preds(PredicateExtractor_ptr self,
                                 node_ptr expr)
{
  int lineno_tmp;

  PREDICATE_EXTRACTOR_CHECK_INSTANCE(self);

  if (Nil == expr) return;

  /* new node will be created with for sure error line number */
  lineno_tmp = nusmv_yylineno;
  nusmv_yylineno = -1;

  /* sometimes top-level expressions are connected together with CONS
     or AND node created with new_node. Such pseudo-expressions are then
     freed and can be reused later in other places.
     As this function has memoization it may become a problem.

     The solution is to process such high level CONS and AND separately
     without type checking/memoization.

     Note: that if normal expressions with AND or CONS happened to be
     at the top they will be processed correctly, but just not
     memoized (some efficiency may be lost).

     NOTE: the right solution would be to use a special connector
     for high level expressions, not generic AND or CONS.
  */
  if (AND == node_get_type(expr) || CONS == node_get_type(expr)) {
    PredicateExtractor_compute_preds(self, car(expr));
    PredicateExtractor_compute_preds(self, cdr(expr));
  }
  else {
    /* this is a usual expression */
    pred_extract_process_recur(self, expr, Nil);
  }

  nusmv_yylineno = lineno_tmp; /* restore line number */
  return;
}

void
PredicateExtractor_compute_preds_from_hierarchy(PredicateExtractor_ptr self,
                                                FlatHierarchy_ptr fh)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  int i;
  node_ptr expr;
  array_t * layers_name;
  const char* a_layer_name;

  node_ptr (*fh_access[])(FlatHierarchy_ptr)  = {
    FlatHierarchy_get_init, FlatHierarchy_get_invar,
    FlatHierarchy_get_trans, FlatHierarchy_get_input,
    FlatHierarchy_get_justice, FlatHierarchy_get_compassion,
    NULL};

  for(i = 0; fh_access[i] != NULL; i++) {
    expr = (*fh_access[i])(fh);
    PredicateExtractor_compute_preds(self, expr);
  }

  /* 1. FlatHierarchy_get_init/invar/trans do NOT return assignments.
     2. FlatHierarchy_lookup_assign return assignments with "running".
     3. FlatHierarchy_get_assign returns assignments without "running".
     4. We do NOT care about "running" and created corresponding CASE-expression
        because boolean vars are ignored by predicate extractor and
        assignment of a var to itself does not create new predicate
     Thus it is better to use FlatHierarchy_get_assign instead of
     FlatHierarchy_lookup_assign (there will be no need to iterate over all vars).
  */

  /* Assignments require very special handling because
     FlatHierarchy_get_assign returns the assignments without
     CASE-expressions and "running" variables created when there are
     processes.  To obtain the actual assignments it is necessary to
     collects assignments using FlatHierarchy_lookup_assign.

     NOTE: This code is terrible because API in FlatHierarchy does
     not provided the required function (to access actual assignments).
  */
  layers_name = SymbTable_get_class_layer_names(self->st, (const char*) NULL);

  arrayForEachItem(const char*, layers_name, i, a_layer_name) {
    SymbLayer_ptr layer = SymbTable_get_layer(self->st, a_layer_name);
    SymbLayerIter iter;

    SYMB_LAYER_FOREACH(layer, iter, STT_VAR) {
      node_ptr name = SymbLayer_iter_get_symbol(layer, &iter);
      node_ptr init_name = find_node(nodemgr, SMALLINIT, name, Nil);
      node_ptr next_name = find_node(nodemgr, NEXT, name, Nil);
      node_ptr invar_expr = FlatHierarchy_lookup_assign(fh, name);
      node_ptr init_expr = FlatHierarchy_lookup_assign(fh, init_name);
      node_ptr next_expr = FlatHierarchy_lookup_assign(fh, next_name);

      if (invar_expr != Nil) {
        expr = find_node(nodemgr, EQDEF, name, invar_expr);
        PredicateExtractor_compute_preds(self, expr);
      }
      if (init_expr != Nil) {
        expr = find_node(nodemgr, EQDEF, init_name, init_expr);
        PredicateExtractor_compute_preds(self, expr);
      }
      if (next_expr != Nil) {
        expr = find_node(nodemgr, EQDEF, next_name, next_expr);
        PredicateExtractor_compute_preds(self, expr);
      }
    }
  }

  return;
}

Set_t
PredicateExtractor_get_all_preds(const PredicateExtractor_ptr self)
{
  PREDICATE_EXTRACTOR_CHECK_INSTANCE(self);

  return self->all_preds;
}

Set_t
PredicateExtractor_get_all_clusters(const PredicateExtractor_ptr self)
{
  PREDICATE_EXTRACTOR_CHECK_INSTANCE(self);

  /* there are un-clustered predicates => process them at first */
  if (!Set_IsEmpty(self->unclustered_preds)) {

    Set_Iterator_t pred_iter;

    /* iterate over all the predicates */
    SET_FOREACH(self->unclustered_preds, pred_iter) {

      node_ptr predicate = Set_GetMember(self->unclustered_preds, pred_iter);
      Set_t deps = Formula_GetDependencies(self->st, predicate, Nil);

      /* NOTE: if simplification was not done then a predicate may consist
         of constants only. In this case dependency is empty and the predicate
         can be ignored.
      */
      if (!Set_IsEmpty(deps)) {

        /* first var is dealt differently : if a var is not yet in the
           table => create a new cluster for it, otherwise use the
           existing one */
        Set_Iterator_t it = Set_GetFirstIter(deps);
        node_ptr var = Set_GetMember(deps, it);

        Set_t cluster = (Set_t)find_assoc(self->var2cluster, var);

        /* if there is no cluster => create a new one */
        if (NULL == cluster) {
          cluster = Set_MakeSingleton(var);
          insert_assoc(self->var2cluster, var, NODE_PTR(cluster));
          self->all_clusters = Set_AddMember(self->all_clusters, NODE_PTR(cluster));

          /* create new cluster->predicates association and add the predicate */
          insert_assoc(self->cluster2preds, NODE_PTR(cluster),
                       NODE_PTR(Set_MakeSingleton(NODE_PTR(predicate))));
        }
        /* cluster already exist => insert the predicate into existing
           cluster2preds associated */
        else {
          Set_t cl_preds = (Set_t)find_assoc(self->cluster2preds, NODE_PTR(cluster));
          Set_t tmp;
          nusmv_assert(NULL != cl_preds); /* every cluster has some predicate */
          tmp = Set_AddMember(cl_preds, predicate);
          nusmv_assert(tmp == cl_preds); /* debug: the pointer did not change */
        }
        /* note that every cluster always has at least one var, and one
           predicate */

        /* check other vars => insert the var in the cluster or merge
           the clusters */
        for (it = Set_GetNextIter(it); !Set_IsEndIter(it);
             it = Set_GetNextIter(it)) {
          Set_t another_cluster;
          var = Set_GetMember(deps, it);
          another_cluster = (Set_t) find_assoc(self->var2cluster, var);

          /* var has no cluster => add the var to the cluster of previous var */
          if ((Set_t)NULL == another_cluster) {
            another_cluster = Set_AddMember(cluster, var);
            /* debug: the pointer does not change */
            nusmv_assert(cluster == another_cluster);
            insert_assoc(self->var2cluster, var, (node_ptr)cluster);
          }
          /* var has cluster but it is the same as of prev. var => do nothing */
          else if (cluster == another_cluster) {
            /* do nothing */
          }
          /* the var already has its own cluster => push all the info
             into that other cluster and reset the
             hash: var -> cluster and hash: cluster->preds */
          else {
            /* merge the cluster into the other one.  Because of Set class
               implementation, pointer to another_cluster will not change, i.e.
               only cluster associations have to be changed */
            Set_t cl_preds, other_preds, tmp;
            Set_Iterator_t cl_iter;
            SET_FOREACH(cluster, cl_iter) {
              node_ptr a_var = Set_GetMember(cluster, cl_iter);
              Set_t tmp = Set_AddMember(another_cluster, a_var);
              /* debug: the pointer does not change */
              nusmv_assert(another_cluster == tmp);
              insert_assoc(self->var2cluster, a_var, NODE_PTR(another_cluster));
            }
            /* merge the associated predicates */
            cl_preds = (Set_t) find_assoc(self->cluster2preds,
                                          NODE_PTR(cluster));
            other_preds = (Set_t) find_assoc(self->cluster2preds,
                                             NODE_PTR(another_cluster));
            /* every cluster has at least 1 predicate */
            nusmv_assert(NULL != cl_preds && NULL != other_preds);

            tmp = Set_Union(other_preds, cl_preds);
            nusmv_assert(tmp == other_preds); /* debug: other_preds is a union now */

            Set_ReleaseSet(cl_preds);
            remove_assoc(self->cluster2preds, NODE_PTR(cluster));

            self->all_clusters = Set_RemoveMember(self->all_clusters,
                                                  NODE_PTR(cluster));
            Set_ReleaseSet(cluster);

            cluster = another_cluster;
          }
        } /* for */
      } /* if predicate is not empty */

      Set_ReleaseSet(deps);
    }

    Set_ReleaseSet(self->unclustered_preds);
    self->unclustered_preds = Set_MakeEmpty();
  }

  return self->all_clusters;
}

Set_t PredicateExtractor_get_var_cluster(const PredicateExtractor_ptr self,
                                         node_ptr var)
{
  /* to trigger cluster computation */
  PredicateExtractor_get_all_clusters(self);

  return (Set_t)find_assoc(self->var2cluster, var);
}

Set_t
PredicateExtractor_get_preds_of_a_cluster(const PredicateExtractor_ptr self,
                                          Set_t cluster)
{
  Set_t preds;

  /* PredicateExtractor_compute_preds was called after
     PredicateExtractor_get_all_clusters which is an error */
  nusmv_assert(Set_IsEmpty(self->unclustered_preds));

  preds = (Set_t)find_assoc(self->cluster2preds, NODE_PTR(cluster));

  nusmv_assert(preds != NULL); /* every cluster has predicates */

  return preds;
}

void
PredicateExtractor_print(const PredicateExtractor_ptr self,
                         FILE* stream,
                         boolean printPredicates,
                         boolean printClusters)
{
  Set_t set;
  Set_Iterator_t iter;
  int clst_num = 0;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  if (!printPredicates && !printClusters) {
    ErrorMgr_rpterr(errmgr, "Function PredicateExtractor_print needs at least one "
           "of printPredicates and printClusters to be true.");
  }

  /* ----------- print just predicates */
  if (printPredicates && ! printClusters) {
    fprintf(stream, "\nPredicates are :\n-------------------------------\n");
    set = PredicateExtractor_get_all_preds(self);
    SET_FOREACH(set, iter) {
      fprintf(stream, "\n   ");
      print_node(wffprint, stream, Set_GetMember(set, iter));
    }
    fprintf(stream, "\n------------------------------------\n");
    return;
  }

  /* -------------  print clusters */
  set = PredicateExtractor_get_all_clusters(self);
  SET_FOREACH(set, iter) {
    Set_t cluster = (Set_t) Set_GetMember(set, iter);
    Set_Iterator_t sit;

    /* output the clusters */
    fprintf(stream,
            "\n--------------------------------------------------\n"
            "---- Cluster %d \n \t [\n", clst_num);
    /* Clusters */
    SET_FOREACH(cluster, sit) {
      node_ptr var = Set_GetMember(cluster, sit);
      fprintf(stream, " \t   ");
      print_node(wffprint, stream, var);
      fprintf(stream, " : ");
      SymbType_print(SymbTable_get_var_type(self->st, var), wffprint, stream);
      fprintf(stream, "\n");
    }
    fprintf(stream, " \t ]\n");

    /* stream the predicates */
    if (printPredicates) {
      /* Preds */
      Set_t preds = (Set_t)find_assoc(self->cluster2preds,
                                      NODE_PTR(cluster));
      nusmv_assert(NULL != preds); /* every cluster has at least one predicate */

      fprintf(stream, " \t Predicates for Cluster %d\n \t (\n", clst_num);
      SET_FOREACH(preds, sit) {
        node_ptr pr = Set_GetMember(cluster, sit);
        fprintf(stream, " \t   ");
        print_node(wffprint, stream, pr);
        fprintf(stream, "\n");
      }
      fprintf(stream, " \t )\n\n");
    }

    return;
  }

}


/*---------------------------------------------------------------------------*/
/* Static function definitions                                               */
/*---------------------------------------------------------------------------*/

/*!
  \brief initialiser of an object of this class


*/
static void pred_extract_init(PredicateExtractor_ptr self,
                              SymbTable_ptr st, boolean use_approx)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  node_ptr w0,w1;

  env_object_init(ENV_OBJECT(self), env);

  self->all_preds = Set_MakeEmpty();
  self->unclustered_preds = Set_MakeEmpty();
  self->all_clusters = Set_MakeEmpty();
  self->var2cluster = new_assoc();
  self->cluster2preds = new_assoc();
  self->expr2preds = new_assoc();

  w0 = find_node(nodemgr, NUMBER_UNSIGNED_WORD,
                 NODE_PTR(WordNumberMgr_integer_to_word_number(words, 0, 1)), Nil);
  w1 = find_node(nodemgr, NUMBER_UNSIGNED_WORD,
                 NODE_PTR(WordNumberMgr_integer_to_word_number(words, 1, 1)), Nil);
  self->special_word_preds[0] = Set_MakeSingleton(w0);
  self->special_word_preds[1] = Set_MakeSingleton(w1);
  self->special_word_preds[2] = Set_AddMember(Set_MakeSingleton(w0), w1);

  self->st = st;
  self->checker = SymbTable_get_type_checker(st);
  self->use_approx = use_approx;

  OVERRIDE(Object, finalize) = pred_extract_finalize;
}

/*!
  \brief finalizer  of this class

*/
static void pred_extract_finalize(Object_ptr object, void* dummy)
{
  PredicateExtractor_ptr self = PREDICATE_EXTRACTOR(object);

  pred_extract_deinit(self);

  FREE(self);
}

/*!
  \brief de-initialiser of an object of this class

*/
static void pred_extract_deinit(PredicateExtractor_ptr self)
{
  assoc_iter iter;
  Set_t cluster, preds, tmp;
  node_ptr expr;

  nusmv_assert(TYPE_CHECKER(NULL) != self->checker);

  /* free 3 special predicate subparts */
  Set_ReleaseSet(self->special_word_preds[0]);
  Set_ReleaseSet(self->special_word_preds[1]);
  Set_ReleaseSet(self->special_word_preds[2]);

  /* free Set_t of predicate subparts in expr2preds.  different
     expressions may point to the same set of predicates (e.g. this
     happens with defines) => collect all sets in one set and then
     release them.  This allows to avoid double releases. */
  tmp = Set_MakeEmpty();

  ASSOC_FOREACH(self->expr2preds, iter, &expr, &preds) {
    /* preds must exist and should not be one of special predicates set */
    nusmv_assert(preds != NULL &&
                 preds != self->special_word_preds[0] &&
                 preds != self->special_word_preds[1] &&
                 preds != self->special_word_preds[2]);

    if (!IS_FLAG_PREDICATES(preds)) {
      tmp = Set_AddMember(tmp, NODE_PTR(preds));
    }
  }
  Set_ReleaseSetOfSet(tmp);
  free_assoc(self->expr2preds);

  /* free Set_t of predicates in cluster2preds */
  ASSOC_FOREACH(self->cluster2preds, iter, &cluster, &preds) {
    nusmv_assert(preds != NULL);
    Set_ReleaseSet(preds);
  }
  free_assoc(self->cluster2preds);

  /* no need to free cluster in var2cluster */
  free_assoc(self->var2cluster);

  /* free clusters which are Set_t of Set_t */
  Set_ReleaseSetOfSet(self->all_clusters);

  /* predicate sets are just Set_t of node_ptr */
  Set_ReleaseSet(self->unclustered_preds);
  Set_ReleaseSet(self->all_preds);


  /* debugging code : setting to NULL */
  self->all_preds = (Set_t)NULL;
  self->unclustered_preds = (Set_t)NULL;
  self->all_clusters = (Set_t)NULL;
  self->var2cluster = (hash_ptr)NULL;
  self->cluster2preds = (hash_ptr)NULL;
  self->expr2preds = (hash_ptr)NULL;
  self->special_word_preds[0] = (Set_t)NULL;
  self->special_word_preds[1] = (Set_t)NULL;
  self->special_word_preds[2] = (Set_t)NULL;

  self->st = SYMB_TABLE(NULL);
  self->checker = TYPE_CHECKER(NULL);

  env_object_deinit(ENV_OBJECT(self));
}

/*!
  \brief Performs the predicates extraction

  See PredicateExtractor_compute_preds for more info.

   This is the main function for extraction.

   The function returns the set of predicate subparts, i.e. Set_t of
   node_ptr. For expressions having whole predicates (i.e. boolean
   expressions) PREDICATES_TRUE/PREDICATES_FALSE/PREDICATES_ARBITRARY
   value is returned.

   Returned set of predicate subparts belong to self->expr2preds. The
   expression (predicates subparts are find_node-ed and belong to
   whole system).
*/
static Set_t pred_extract_process_recur(PredicateExtractor_ptr self,
                                        node_ptr expr,
                                        node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ExprMgr_ptr exprs =
    EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  SymbType_ptr type;
  Set_t result, left, right;
  node_ptr tmp, key;
  int node_type;

  nusmv_assert(Nil != expr);

  key = find_node(nodemgr, CONTEXT, context, expr);

  /* is already processed. */
  result = (Set_t) find_assoc(self->expr2preds, key);
  if (NULL != result) return result;

  type = TypeChecker_get_expression_type(self->checker, expr, context);
  nusmv_assert(!SymbType_is_error(type));

  node_type = node_get_type(expr);

  /* for sure incorrect value for debugging */
  result = left = right = (Set_t) -1;

  /* process every kind of an expression */
  switch (node_type) {
  case CONTEXT: {
    node_ptr new_ctx = CompileFlatten_concat_contexts(env, context, car(expr));

    result = pred_extract_process_recur(self, cdr(expr), new_ctx);
    break;
  }

    /* list of simple boolean constants => they are not part of predicates */
  case FAILURE:
    /* failures are dealt in IF and CASE expressions.  It must be deal
       there because otherwise optimizations with only TRUE/only FALSE
       predicates may become impossible */
    error_unreachable_code();
    result = PREDICATES_ARBITRARY;
    break;

  case FALSEEXP:
    result = PREDICATES_FALSE;
    break;

  case TRUEEXP:
    result = PREDICATES_TRUE;
    break;

  case NUMBER:
    /* NUMBER may be boolean and not-boolean. Here we always consider
       it as not-boolean. The outer expression will decide what to do
       with the result. */
    /* list of simple not-boolean constants => they become predicates subpart */
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case UWCONST:
  case SWCONST:
  case TWODOTS:
    tmp = ExprMgr_resolve(exprs, self->st, node_type, car(expr), cdr(expr));
    result = Set_MakeSingleton(tmp);
    break;

  case NFUNCTION: {
    node_ptr list = cdr(expr);
    result = Set_MakeEmpty();

    /* We extract the predicates from the arguments, and then we
       consider the expression */
    while (Nil != list) {
      Set_t elem = pred_extract_process_recur(self, car(list), context);

      if (IS_OVER_APPROX(elem)) break;
      list = cdr(list);
    }
    if (Nil != list) {
      Set_ReleaseSet(result);
      result = PREDICATES_OVERAPPROX;
    }
    else {
      if (!SymbType_is_boolean(type)) {
        result = Set_AddMember(result, (Set_Element_t)expr);
      }
      else if (Set_IsEmpty(result)) {
        Set_ReleaseSet(result);
        result = PREDICATES_ARBITRARY;
      }
      /* boolean expressions make predicates have arbitrary values */
      /* Arbitrary Union S --> S */
    }
    break;
  }

  case ARRAY: {
    ResolveSymbol_ptr rs;

    rs = SymbTable_resolve_symbol(self->st, expr, context);

    if (ResolveSymbol_is_undefined(rs)) {
      /* Array may be an identifier-with-brackets and may be
         expression.  Here an array-expression is detected =>
         expression is to be flattened at first to resolve array
         identifiers-with-brackets (see description of
         flattener_core_flattenas for details) and then general
         predicate extractor is to be invoked */
      node_ptr tmp = Compile_FlattenSexp(self->st, expr, context);
      nusmv_assert(tmp != expr); /* loop in recursion is impossible */
      result = pred_extract_process_recur(self, tmp, Nil);
      break;
    }
    else {
      /* array is actually identifier => process it with other identifiers */
    }
    /* NO BREAK HERE */
  }

  case DOT:
  case ATOM:
  case BIT: {
    /* The expression is a symbol.
       It can be a variable, a define, a constant or a parameter.
       The expression may have been flattened as well as not flattened.

       Note, that NO ERRORS CAN BE HERE, since all the error
       situations have been checked during type-checking of the
       original expression.
    */

    /* First, try to resolve the symbol */
    ResolveSymbol_ptr rs;
    node_ptr resolvedName;

    rs = SymbTable_resolve_symbol(self->st, expr, context);
    resolvedName = ResolveSymbol_get_resolved_name(rs);

    /* Check whether this is a variable */
    if (ResolveSymbol_is_var(rs)) {
      if (!SymbType_is_boolean(type)) {
        result = Set_MakeSingleton(resolvedName);
      } /* boolean vars make predicates have arbitrary values */
      else result = PREDICATES_ARBITRARY;
    }

    /* check whether is a define */
    else if (ResolveSymbol_is_define(rs)) {
      node_ptr def = SymbTable_get_define_body(self->st, resolvedName);
      node_ptr ctx = SymbTable_get_define_context(self->st, resolvedName);

      /* the context is Nil because expr is already flattened */
      result = pred_extract_process_recur(self, def, ctx);

      /* special case: array define may be declared with Integer (or
         higher) subtype and at the same time has a boolean element.
         In this case the boolean element has to be casted to integer.
      */
      if (ARRAY == node_type &&
          !SymbType_is_boolean(type) &&
          SymbType_is_boolean(TypeChecker_get_expression_type(self->checker,
                                                              def, ctx))) {
        /* boolean can be casted to Int, Int-Symb or their Sets only
           thus conversion to integer is enough*/
        nusmv_assert(SymbType_is_integer(type) ||
                     SymbType_is_int_symbolic_enum(type) ||
                     SYMB_TYPE_SET_INT == SymbType_get_tag(type) ||
                     SYMB_TYPE_SET_INT_SYMB == SymbType_get_tag(type));
      }
    }
    /* check whether this symbol is a constant. The ResolveSymbol
       takes care of simple/complex constants */
    else if (ResolveSymbol_is_constant(rs) ||
             ResolveSymbol_is_function(rs)) {
      result = Set_MakeSingleton(resolvedName);
    }
    /* check whether this symbol is a parameter */
    else {
      node_ptr param = Nil;
      node_ptr new_ctx;

      /* it must be a parameter but being a parameter is the last
         possibility */
      nusmv_assert(ResolveSymbol_is_parameter(rs));

      param = SymbTable_get_actual_parameter(self->st, resolvedName);

      new_ctx = SymbTable_get_actual_parameter_context(self->st, resolvedName);

      result = pred_extract_process_recur(self, param, new_ctx);
    }

    break;
  } /* ATOM */

    /* boolean unary expression or boolean binary expressions those
       right child can be ignored and which have not to be optimized */
  case EX: case AX: case EF: case AF: case EG: case AG:
  case OP_NEXT: case OP_PREC: case OP_NOTPRECNOT: case OP_GLOBAL:
  case OP_HISTORICAL: case OP_FUTURE: case OP_ONCE:
  case EBF: case ABF: case EBG: case ABG: /* ignore the number..number part */
  case ABU: case EBU: /* ignore the number..number part */
    nusmv_assert(SymbType_is_boolean(type)); /* only boolean can be here */
    result = pred_extract_process_recur(self, car(expr), context);
    result = PREDICATES_ARBITRARY;
    break;

    /* unary operations which may or may not be boolean */
  case NOT:
    nusmv_assert(Nil == cdr(expr)); /* checking that indeed no right child */
    result = PREDICATES_OVERAPPROX;
    left = pred_extract_process_recur(self, car(expr), context);

    /* if it is boolean => apply the operator on possible constant
       value */
    if (!IS_OVER_APPROX(left)) {
      if (SymbType_is_boolean(type)) {
        if (left == PREDICATES_TRUE) result = PREDICATES_FALSE;
        else if (left == PREDICATES_FALSE) result = PREDICATES_TRUE;
        else result = PREDICATES_ARBITRARY;
      }
      else { /* otherwise apply the bitwise operator */
        if (!IS_FLAG_VALID_PREDICATES(left)) {
          result = pred_extract_apply_unary(self, NOT, left);
        }
      }
    }
    break;

    /* unary operations: cannot have boolean operand */
  case UMINUS:
  case FLOOR:
  /* [AI] adding typeof unary operator */
  case TYPEOF:
    nusmv_assert(Nil == cdr(expr));    /* checking that indeed no right child */
    result = PREDICATES_OVERAPPROX;

    left = pred_extract_process_recur(self, car(expr), context);

    if (!IS_OVER_APPROX(left)) {
      if (!IS_FLAG_VALID_PREDICATES(left)) {
        result = pred_extract_apply_unary(self, node_type, left);
      }
    }

    break;

    /* binary boolean operations which cannot be optimized.
       CONS is artificial expression, it is boolean and may have empty
       right child */
  case CONS:
  case UNTIL: case SINCE:
  case AU: case EU:
    result = PREDICATES_OVERAPPROX;

    left = pred_extract_process_recur(self, car(expr), context);

    if (!IS_OVER_APPROX(left)) {
      if (Nil != cdr(expr)) {
        right = pred_extract_process_recur(self, cdr(expr), context);
      }
      result = PREDICATES_ARBITRARY;
    }

    break;

    /* binary expression which may or may not be boolean and
       which can be optimized.
       note: children here always have the same type as the
       expression */
  case AND: case OR: case XOR: case XNOR: case IFF: case IMPLIES:

    left = pred_extract_process_recur(self, car(expr), context);
    result = PREDICATES_OVERAPPROX;

    if (!IS_OVER_APPROX(left)) {
      if (SymbType_is_boolean(type)) {
        /* optimization : check if the first operator result is enough */
        if ((left == PREDICATES_FALSE && node_type == AND) ||
            (left == PREDICATES_TRUE && node_type == OR)) {
          result = left;
          break;
        }
        else if (left == PREDICATES_FALSE && node_type == IMPLIES) {
          result = PREDICATES_TRUE;
          break;
        }
        /* process the second argument (as optimization did not work) */
        right = pred_extract_process_recur(self, cdr(expr), context);

        if (IS_OVER_APPROX(right)) {
          result = PREDICATES_OVERAPPROX;
          break;
        }

        /* compute the value if possible */
        switch (node_type) {
        case AND:
          result = (right == PREDICATES_FALSE)
            ? PREDICATES_FALSE
            : ( (left == PREDICATES_TRUE && right == PREDICATES_TRUE)
                ? PREDICATES_TRUE
                : PREDICATES_ARBITRARY
                );
          break;

        case OR:
          result = (right == PREDICATES_TRUE)
            ? PREDICATES_TRUE
            : ( (left == PREDICATES_FALSE && right == PREDICATES_FALSE)
                ? PREDICATES_FALSE
                : PREDICATES_ARBITRARY
                );
          break;

        case XOR:
          result = (left == PREDICATES_ARBITRARY || right == PREDICATES_ARBITRARY)
            ? PREDICATES_ARBITRARY
            : (left != right ? PREDICATES_TRUE : PREDICATES_FALSE);
          break;

        case XNOR:
        case IFF:
          result = (left == PREDICATES_ARBITRARY || right == PREDICATES_ARBITRARY)
            ? PREDICATES_ARBITRARY
            : (left == right ? PREDICATES_TRUE : PREDICATES_FALSE);
          break;

        case IMPLIES:
          result = (right == PREDICATES_TRUE)
            ? PREDICATES_TRUE
            : ( (left == PREDICATES_TRUE && right == PREDICATES_FALSE)
                ? PREDICATES_FALSE
                : PREDICATES_ARBITRARY
                );
          break;

        default: error_unreachable_code(); /* impossible code */
        }
        /* debug: result was set up to a constant */
        nusmv_assert(IS_FLAG_PREDICATES(result));
      }
      else {
        /* this is not a boolean => it can be only word operations,
           i.e.  apply the binary operator to results. */
        result = PREDICATES_OVERAPPROX;
        right = pred_extract_process_recur(self, cdr(expr), context);

        if (!IS_OVER_APPROX(right)) {
          if (!IS_FLAG_VALID_PREDICATES(left) &&
              !IS_FLAG_VALID_PREDICATES(right)) {
            result = pred_extract_apply_binary(self, node_type, left, right);
          }
          else {
            result = Set_MakeSingleton(expr);
          }
        }
      }
    }
    break;

    /* not-boolean unary operators */
  case CAST_UNSIGNED:
  case CAST_SIGNED:
    nusmv_assert(Nil == cdr(expr));    /* checking that indeed no right child */
    result = PREDICATES_OVERAPPROX;
    left = pred_extract_process_recur(self, car(expr), context);
    if (!IS_OVER_APPROX(left)) {
      if (!IS_FLAG_VALID_PREDICATES(left)) {
        result = pred_extract_apply_unary(self, node_type, left);
      }
      else {
        result = Set_MakeSingleton(expr);
      }
    }
    break;

    /* "next" and "init" are here as normal unary operation because
       EQDEF is a normal binary operation. No cast is done here. */
  case NEXT:
  case SMALLINIT:
    nusmv_assert(Nil == cdr(expr)); /* checking that indeed no right child */
    result = pred_extract_process_recur(self, car(expr), context);

    /* note that here init and next are applied without modifications,
       i.e.  next(x) := 3 will be kept as it is whereas next(x := 3)
       will be kept as x:=3 because next is applied outside of the
       predicate, not inside.

       It does not matter if we (not) get rid of "init" or "next".
    */
    if (!IS_FLAG_PREDICATES(result)) {
      /* this is not true boolean => apply the operator */
      result = pred_extract_apply_unary(self, node_type, result);
    }
    else {
      /* the true/false value is not changed by next/init => do nothing */
    }
    break;

    /* relational operators: they convert not-boolean to boolean  */
  case EQDEF: case SETIN:
  case EQUAL: case NOTEQUAL: case LT: case GT: case LE: case GE: {
    SymbType_ptr type1 =
      TypeChecker_get_expression_type(self->checker, car(expr), context);
    SymbType_ptr type2 =
      TypeChecker_get_expression_type(self->checker, cdr(expr), context);

    nusmv_assert(!SymbType_is_error(type1));
    nusmv_assert(!SymbType_is_error(type2));

    result = PREDICATES_OVERAPPROX;

    left = pred_extract_process_recur(self, car(expr), context);

    if (IS_OVER_APPROX(left)) break;

    right = pred_extract_process_recur(self, cdr(expr), context);
    if (IS_OVER_APPROX(right)) break;

    /* both operands are boolean (or bool-set for EQDEF and SETIN) */
    if ((SymbType_is_boolean(type1) ||
         SYMB_TYPE_SET_BOOL == SymbType_get_tag(type1)) &&
        (SymbType_is_boolean(type2) ||
         SYMB_TYPE_SET_BOOL == SymbType_get_tag(type2))) {
      /* compute the value of expression if possible */
      switch (node_type) {
      case EQDEF:
      case SETIN:
      case EQUAL:
        result = (left == PREDICATES_ARBITRARY || right == PREDICATES_ARBITRARY)
          ? PREDICATES_ARBITRARY
          : (left == right ? PREDICATES_TRUE : PREDICATES_FALSE);
        break;

      case NOTEQUAL:
        result = (left == PREDICATES_ARBITRARY || right == PREDICATES_ARBITRARY)
          ? PREDICATES_ARBITRARY
          : (left != right ? PREDICATES_TRUE : PREDICATES_FALSE);
        break;

      case GT: /* exchange right and left and jump to LT */
        {Set_t tmp = left; left = right; right = tmp;}
        /* no break here! */
      case LT:
        result = (left == PREDICATES_TRUE || right == PREDICATES_FALSE)
          ? PREDICATES_FALSE
          : (left == PREDICATES_FALSE && right == PREDICATES_TRUE)
          ? PREDICATES_TRUE
          : PREDICATES_ARBITRARY;
        break;

      case GE: /* exchange right and left and jump to LE */
        {Set_t tmp = left; left = right; right = tmp;}
        /* no break here! */
      case LE:
        result = (left == PREDICATES_FALSE || right == PREDICATES_TRUE)
          ? PREDICATES_TRUE
          : (left == PREDICATES_TRUE && right == PREDICATES_FALSE)
          ? PREDICATES_FALSE
          : PREDICATES_ARBITRARY;
        break;

      default: error_unreachable_code(); /* impossible code */
      }
    }
    else {
      /* both operands are scalar => do bool2int to cast "true
         boolean" operand (if there is one) and apply the binary
         operator */
      if (!IS_FLAG_VALID_PREDICATES(left) &&
          !IS_FLAG_VALID_PREDICATES(right)) {
        result = pred_extract_apply_binary(self, node_type, left, right);
      }
      else {
        result = Set_MakeSingleton(expr);
      }
      /* remember the results */
      result = pred_extract_fix_any_preds(self, result);
    }
    break;
  }

    /* These exprs are always scalar. No optimizations are done here. */
  case TIMES: case DIVIDE: case PLUS :case MINUS: case MOD:
  case LSHIFT: case RSHIFT: /*case LROTATE: case RROTATE: */
  case EXTEND: case WRESIZE:
  case CONST_ARRAY: {
    result = PREDICATES_OVERAPPROX;
    left = pred_extract_process_recur(self, car(expr), context);
    if (!IS_OVER_APPROX(left)) {
      right = pred_extract_process_recur(self, cdr(expr), context);
      if (!IS_FLAG_VALID_PREDICATES(left) &&
          !IS_FLAG_VALID_PREDICATES(right)) {
        result = pred_extract_apply_binary(self, node_type, left, right);
      }
      else {
        result = Set_MakeSingleton(expr);
      }
    }
    break;
  }

  case WAREAD: {
    result = PREDICATES_OVERAPPROX;
    if (!SymbType_is_boolean(type)) {
      left = pred_extract_process_recur(self, car(expr), context);
      if (!IS_OVER_APPROX(left)) {
        right = pred_extract_process_recur(self, cdr(expr), context);
        if (!IS_FLAG_VALID_PREDICATES(left) &&
            !IS_FLAG_VALID_PREDICATES(right)) {
          result = pred_extract_apply_binary(self, node_type, left, right);
        }
        else {
          result = Set_MakeSingleton(expr);
        }
      }
    }
    break;
  }

  case WAWRITE: {
    nusmv_assert(node_get_type(cdr(expr))==WAWRITE);

    result = PREDICATES_OVERAPPROX;
    left = pred_extract_process_recur(self, car(expr), context);
    if (!IS_OVER_APPROX(left) && !IS_FLAG_VALID_PREDICATES(left)) {
      Set_t index, value;
      index = pred_extract_process_recur(self, car(cdr(expr)), context);
      if (!IS_OVER_APPROX(index)) {
        value = pred_extract_process_recur(self, cdr(cdr(expr)), context);
        if (!IS_OVER_APPROX(value)) {
          if (!IS_FLAG_VALID_PREDICATES(index) &&
              !IS_FLAG_VALID_PREDICATES(value)) {
            right = pred_extract_apply_binary(self, node_type, index, value);
            result = pred_extract_apply_binary(self, node_type, left, right);
          }
          else {
            result = Set_MakeSingleton(expr);
          }
        }
      }
    }
    break;
  }

    /* COLON cannot go as a independent operation */
  case COLON: error_unreachable_code();

  case BIT_SELECTION: {
    /* just consistency check */
    nusmv_assert(COLON == node_get_type(cdr(expr)));
    result = PREDICATES_OVERAPPROX;
    left = pred_extract_process_recur(self, car(expr), context);
    if (!IS_OVER_APPROX(left)) {
      if (!IS_FLAG_VALID_PREDICATES(left)) {
        right = Set_MakeSingleton(find_node(nodemgr,
                                            COLON,
                                            find_atom(nodemgr, car(cdr(expr))),
                                            find_atom(nodemgr, cdr(cdr(expr)))));
        result = pred_extract_apply_binary(self, node_type, left, right);
        Set_ReleaseSet(right);
      }
      else {
        /* The expression is propagated up */
        result = Set_MakeSingleton(expr);
      }
    }
    break;
  }

  case WSIZEOF: {
    /* sizeof returns the bit-size of word expressions without evaluating
       the expression itself */
    int width;
    nusmv_assert(SymbType_is_word(type));
    width = SymbType_get_word_width(type);
    result = Set_MakeSingleton(find_node(nodemgr, NUMBER, NODE_FROM_INT(width), Nil));
    break;
  }

  case CAST_TOINT: {
    nusmv_assert(Nil == cdr(expr)); /* indeed no right child */
    result = PREDICATES_OVERAPPROX;
    left = pred_extract_process_recur(self, car(expr), context);
    if (!IS_OVER_APPROX(left)) {
      if (!IS_FLAG_VALID_PREDICATES(left)) {
        result = pred_extract_apply_unary(self, node_type, left);
      }
      else {
        node_ptr fexpr = Compile_FlattenSexp(self->st, expr, context);
        result = Set_MakeSingleton(fexpr);
      }
    }
    break;
  }

  case COUNT: {
    node_ptr list = car(expr);
    result = PREDICATES_OVERAPPROX;

    while (Nil != list) {
      Set_t elem = pred_extract_process_recur(self, car(list), context);

      if (IS_OVER_APPROX(elem)) break;
      list = cdr(list);
    }
    if (Nil == list) {
      node_ptr fexpr = Compile_FlattenSexp(self->st, expr, context);
      result = Set_MakeSingleton(fexpr);
    }
    break;
  }

    /* concatenation requires two word arguments (i.e. scalar).
       the only required thing is to convert bool to word1.
    */
  case CONCATENATION: {
    SymbType_ptr type1 =
      TypeChecker_get_expression_type(self->checker, car(expr), context);
    SymbType_ptr type2 =
      TypeChecker_get_expression_type(self->checker, cdr(expr), context);

    nusmv_assert(!SymbType_is_error(type1));
    nusmv_assert(!SymbType_is_error(type2));

    result = PREDICATES_OVERAPPROX;
    left = pred_extract_process_recur(self, car(expr), context);
    if (!IS_OVER_APPROX(left)) {
      right = pred_extract_process_recur(self, cdr(expr), context);

      if (!IS_OVER_APPROX(right)) {
        if (!IS_FLAG_VALID_PREDICATES(left) && !IS_FLAG_VALID_PREDICATES(right)) {
          result = pred_extract_apply_binary(self, node_type, left, right);
        }
        else {
          result = Set_MakeSingleton(expr);
        }
      }
    }
    break;
  }

    /* cast to bool is the same as comparison with 0d1_1 */
  case CAST_BOOL: {
    nusmv_assert(cdr(expr) == Nil); /* indeed no right child */
    result = PREDICATES_OVERAPPROX;
    left = pred_extract_process_recur(self, car(expr), context);

    if (!IS_OVER_APPROX(left) && !IS_FLAG_VALID_PREDICATES(left)) {
      result = pred_extract_apply_binary(self, EQUAL, left,
                                         self->special_word_preds[1]);
    }
    /* remember the results */
    result = pred_extract_fix_any_preds(self, result);
    break;
  }

  case CAST_WORD1:
    nusmv_assert(cdr(expr) == Nil);
    result = PREDICATES_OVERAPPROX;
    left = pred_extract_process_recur(self, car(expr), context);
    if (!IS_OVER_APPROX(left)) {
      /* create a word1 possible values */
      if (left == PREDICATES_FALSE) result = self->special_word_preds[0];
      else if (left == PREDICATES_TRUE) result = self->special_word_preds[1];
      else result = self->special_word_preds[2];
      result = Set_Copy(result); /* create a copy of special predicate set */
    }
    break;

    /* UNION just perform union of possible predicate subparts.
       The same as in PredicateNormaliser.c boolean are always casted
       to int.

       The difference from PredicateNormaliser.c is that here UNION is
       not applied. It is unclear which way to go, .e.g should "A in
       (B union C)" become "(A in B) union (A in C)" or not?
       Especially taking into account that cast from bool-set to int
       does not exist.

       NB: if this code is to be changed then change also the same
       part in PredicateNormaliser.c.
    */
  case UNION:
    left = pred_extract_process_recur(self, car(expr), context);
    result = PREDICATES_OVERAPPROX;

    if (!IS_OVER_APPROX(left)) {
      right = pred_extract_process_recur(self, cdr(expr), context);

      if (!IS_OVER_APPROX(right)) {
        if (SymbType_is_boolean(type) ||
            (SYMB_TYPE_SET_BOOL == SymbType_get_tag(type))) {
          if (left == right) {
            result = right;
          }
          else {
            result = PREDICATES_ARBITRARY;
          }
        }
        else if (!IS_FLAG_VALID_PREDICATES(left) &&
                 !IS_FLAG_VALID_PREDICATES(right)) {
          result = Set_Union(Set_Copy(left), right);
        }
        else {
          result = Set_MakeSingleton(expr);
        }
      }
    }
    break;

  case IFTHENELSE:
  case CASE: {
    node_ptr c, t, e;
    Set_t cond, then, tail;
    node_ptr simp_cond_expr;

    nusmv_assert(COLON == node_get_type(car(expr)));

    result = PREDICATES_OVERAPPROX;

    c = car(car(expr));
    t = cdr(car(expr));
    e = cdr(expr);

    /* simplification added for issue 02590: we first simplify the
     condition, by flattening it as Exp_simplfy does not support
     contextualized expressions */
    c = Compile_FlattenSexp(self->st, c, context);
    simp_cond_expr = ExprMgr_simplify(exprs, self->st, c);

    /* since simp_cond_expr is flattened, context becomes Nil here: */
    cond = pred_extract_process_recur(self, simp_cond_expr, Nil);

    if (IS_OVER_APPROX(cond)) break;

    /* if condition is a constant => process only one branch.
       also if tail is FAILURE => ignore it. */
    if (cond == PREDICATES_TRUE || FAILURE == node_get_type(e)) {
      result = pred_extract_process_recur(self, t, context);
    }
    else if (cond == PREDICATES_FALSE) {
      result = pred_extract_process_recur(self, e, context);
    }
    else { /* process both branches */
      /* the only remaining value */
      nusmv_assert(cond == PREDICATES_ARBITRARY);

      then = pred_extract_process_recur(self, t, context);

      if (!IS_OVER_APPROX(then)) {
        tail = pred_extract_process_recur(self, e, context);

        if (!IS_OVER_APPROX(tail)) {
          /* if expression is boolean then get rid of all predicates,
             otherwise make the union of the results */
          if (SymbType_is_boolean(type)) {
            /* optimization : both branches return the same constant =>
               return it as the value of the whole expression */
            if (then == tail) result = then;
            else result = PREDICATES_ARBITRARY;
          }
          else {
            /* make union of the results from then and tail */
            /*
              {T} Union S ---> S
              {F} Union S ---> S
              Arbitrary Union S ---> S
              Overapprox Union S ---> Overapprox
            */
            if (IS_FLAG_PREDICATES(then)) result = tail;
            else if (IS_FLAG_PREDICATES(tail)) result = then;
            else {
              result = Set_Union(Set_Copy(then), tail);
            }
          }
        }
      }
    }
    break;
  }

  case ATTIME: {
    left = pred_extract_process_recur(self, car(expr), context);
    /* do not normalise right operand, it is just number */
    result = PREDICATES_OVERAPPROX;

    if (!IS_OVER_APPROX(left)) {
      if (!IS_FLAG_VALID_PREDICATES(left)) {
        right = Set_MakeSingleton(find_atom(nodemgr, cdr(expr)));
        result = pred_extract_apply_binary(self, node_type, left, right);
        Set_ReleaseSet(right);
      }
      else {
        result = Set_MakeSingleton(expr);
      }
    }
    break;
  }

  case CAST_TO_UNSIGNED_WORD:
    result = PREDICATES_OVERAPPROX;
    left = pred_extract_process_recur(self, car(expr), context);
    if (!IS_OVER_APPROX(left)) {
      right = pred_extract_process_recur(self, cdr(expr), context);
      if (!IS_OVER_APPROX(right)) {
        if (!IS_FLAG_VALID_PREDICATES(left) &&
            !IS_FLAG_VALID_PREDICATES(right)) {
          result = pred_extract_apply_binary(self, node_type, left, right);
        }
        else {
          result = Set_MakeSingleton(expr);
        }
      }
    }
    break;

  default:
    /* below condition is introduced in PredicateNormalization by RC and
       copied here by AT. */
#ifndef NDEBUG
    /* Here we assume there are no other boolean operators  */
    result = PREDICATES_OVERAPPROX;
    left = pred_extract_process_recur(self, car(expr), context);
    if (!IS_OVER_APPROX(left)) {
      right = pred_extract_process_recur(self, cdr(expr), context);
      if (!IS_OVER_APPROX(right)) {
        if (!IS_FLAG_VALID_PREDICATES(left) &&
            !IS_FLAG_VALID_PREDICATES(right)) {
          result = pred_extract_apply_binary(self, node_type, left, right);
        }
        else {
          result = Set_MakeSingleton(expr);
        }
      }
    }
    break;
#else
    {
      const MasterPrinter_ptr sexpprinter =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_SEXP_PRINTER));
      const StreamMgr_ptr streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

      StreamMgr_nprint_error(streams, sexpprinter, "%N", expr);
      StreamMgr_print_error(streams,  "unknown token = %d\n", node_type);
      error_unreachable_code(); /* unknown kind of an expression */
    }
#endif
  } /* switch */

  /* debug that result was properly set up */
  nusmv_assert(result != (Set_t) -1);

  /* preds returned should not be one of special predicates set
     which used only to construct other predicates */
  nusmv_assert(result != self->special_word_preds[0] &&
               result != self->special_word_preds[1] &&
               result != self->special_word_preds[2]);

  /* boolean expressions may have only boolean predicate or nothing.
     not-boolean expressions always have proper predicate set */
  nusmv_assert(!(SymbType_is_boolean(type) ||
                 SYMB_TYPE_SET_BOOL == SymbType_get_tag(type)) ||
               pred_extract_is_bool_preds(self, result));
  nusmv_assert((SymbType_is_boolean(type) ||
                SYMB_TYPE_SET_BOOL == SymbType_get_tag(type)) ||
               !IS_FLAG_PREDICATES(result) ||
               IS_OVER_APPROX(result));

  /* remember the processed expression */
  insert_assoc(self->expr2preds, key, NODE_PTR(result));
  return result;
}

/*!
  \brief This function returns true iff the result set of predicates
   subparts may belong only to boolean expression

  There 2 expressions which can be boolean and not
   boolean at the same time: "0" and 1".  These can be considered
   as predicate subparts as well as complete predicates.

   This function returns true iff the result consists of such kind
   of predicates.

  \se pred_extract_process_recur
*/
static boolean pred_extract_is_bool_preds(PredicateExtractor_ptr self, Set_t result)
{
  Set_Iterator_t iter;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  /* a flag that expressions is truly boolean => not predicate subparts */
  if (IS_FLAG_PREDICATES(result)) return true;

  SET_FOREACH(result, iter) {
    node_ptr expr = Set_GetMember(result, iter);
    /* there may be "next" or "init" wrapping boolean predicates */
    node_ptr unnexted =
      (node_get_type(expr) == NEXT || node_get_type(expr) == SMALLINIT)
      ? car(expr) : expr;

    /* for 0 and 1 just do nothing. such predicates are useless. */
    if (!ExprMgr_is_true(exprs, unnexted) &&
        !ExprMgr_is_false(exprs, unnexted)) {
      return false;
    }
  } /* loop */

  return true;
}

/*!
  \brief This function put any expression in the set into
   "self" as complete predicates. "result" if released by this
   function.

  \se pred_extract_process_recur
*/
static Set_t pred_extract_fix_any_preds(PredicateExtractor_ptr self,
                                        Set_t result)
{
  Set_Iterator_t iter;
  boolean there_is_0 = false;
  boolean there_is_1 = false;
  boolean there_is_arbit = false;

  /* overapproximation resolves to <T,F> */
  if (IS_OVER_APPROX(result)) return PREDICATES_ARBITRARY;

  nusmv_assert(!IS_FLAG_PREDICATES(result)); /* only proper sets are expected */

  SET_FOREACH(result, iter) {
    node_ptr expr = Set_GetMember(result, iter);

    /* optimization: skip 0 and 1, TRUE and FALSE as useless predicates. */
    if (FALSEEXP == node_get_type(expr)) {
      there_is_0 = true;
    }
    else if (TRUEEXP == node_get_type(expr)) {
      there_is_1 = true;
    }
    else {
      there_is_arbit = true;
      /* remember the obtained predicates (only if it is new) */
      if (!Set_IsMember(self->all_preds, expr)) {
        self->all_preds = Set_AddMember(self->all_preds, expr);
        self->unclustered_preds = Set_AddMember(self->unclustered_preds, expr);
      }
    }
  } /* loop */

  Set_ReleaseSet(result);

  if (there_is_0 && !there_is_1 && !there_is_arbit) return PREDICATES_FALSE;
  if (!there_is_0 && there_is_1 && !there_is_arbit) return PREDICATES_TRUE;

  return PREDICATES_ARBITRARY;
}

/*!
  \brief This function take a unary operator,
   the result of predicates extraction of the child expression
   and returns new result for the whole expression.

  This function is used only by pred_extract_process_recur.
   Created set belongs to the invoker, i.e. to pred_extract_process_recur
   which will insert them into self->expr2preds.

  \se pred_extract_process_recur
*/
static Set_t pred_extract_apply_unary(PredicateExtractor_ptr self,
                                      int type,
                                      Set_t childResult)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  Set_t result;
  Set_Iterator_t iter;

  /* keep approximation */
  if (IS_OVER_APPROX(childResult)) {
    return PREDICATES_OVERAPPROX;
  }

  /* child result is properly created set of predicates to apply the operator */
  nusmv_assert(!IS_FLAG_PREDICATES(childResult));

  result = Set_MakeEmpty();

  /* apply the operator to every element of the predicate subparts */

  SET_FOREACH(childResult, iter) {
    node_ptr expr = Set_GetMember(childResult, iter);
    expr = ExprMgr_resolve(exprs, self->st, type, expr, Nil);
    result = Set_AddMember(result, expr);
  }

  return result;
}

/*!
  \brief This function take a binary operator,
   the results of predicates extraction of the children subexpressions
   and returns new result for the whole expression

  This function is used only by pred_extract_process_recur.
   Created set belongs to an invoker, pred_extract_process_recur which will
   insert them into self->expr2preds.


  \se pred_extract_process_recur
*/
static Set_t pred_extract_apply_binary(PredicateExtractor_ptr self,
                                       int type,
                                       Set_t leftResult,
                                       Set_t rightResult)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  Set_t result;
  Set_Iterator_t l_iter;

  /* keep approximation */
  if (IS_OVER_APPROX(leftResult) || IS_OVER_APPROX(rightResult)) {
    return PREDICATES_OVERAPPROX;
  }

  /* children results are properly created predicates to apply the operator */
  nusmv_assert(!IS_FLAG_PREDICATES(leftResult) &&
               !IS_FLAG_PREDICATES(rightResult));

  if (self->use_approx &&
      (((size_t) Set_GiveCardinality(leftResult) *
        (size_t) Set_GiveCardinality(rightResult)) > OVER_APPROX_THRESHOLD)) {
    /* too-big: gives up */
    return PREDICATES_OVERAPPROX;
  }

  result = Set_MakeEmpty();

  /* create Cartesian produce of predicate subparts and apply the
     operator to every pair */
  SET_FOREACH(leftResult, l_iter) {
    Set_Iterator_t r_iter;
    node_ptr l_expr = Set_GetMember(leftResult, l_iter);
    nusmv_assert(Nil != l_expr); /* expression is well-formed */

    SET_FOREACH(rightResult, r_iter) {
      node_ptr r_expr = Set_GetMember(rightResult, r_iter);
      nusmv_assert(Nil != r_expr); /* expression is well-formed */

      result = Set_AddMember(result,
                             ExprMgr_resolve(exprs, self->st,
                                             type, l_expr, r_expr));
    }
  }
  return result;
}
