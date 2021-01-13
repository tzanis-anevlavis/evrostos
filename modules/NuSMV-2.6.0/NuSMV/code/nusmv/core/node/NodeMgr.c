/* ---------------------------------------------------------------------------


  This file is part of the ``node'' package of NuSMV version 2.
  Copyright (C) 2011 by FBK-irst.

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
  \author Alessandro Mariotti
  \brief Implementation of class 'NodeMgr'

  \todo: Missing description

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/utils/EnvObject.h"
#include "nusmv/core/utils/EnvObject_private.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/* Threashold for the reallocation of internal hash. It is the
   overaproximated ratio between the average entries size and the
   total number of hashed entries. Value 0.85 proved to be good in
   average.*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define HASH_REALLOC_THREASHOLD 0.85


/* Define the internal list organization. Only one has to be
   defined.
   SIMPLE: new nodes a always prepended to the hash lists.

   PROXIMITY: most frequenlty searched nodes are kept close to the
              beginning of lists in the hash.

   SORTED: hash lists are kept sorted to optimize searches.
 */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define INSERT_NODE_SIMPLE 0

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define INSERT_NODE_PROXIMITY 1 /* this proved to be experimentally better */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define INSERT_NODE_SORTED 0

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NODE_MEM_CHUNK 1022

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NODE_PRIMES_SIZE (sizeof(node_primes) / sizeof(node_primes[0]))

static const unsigned int node_primes[] = {
  49157, 98317, 196613, 393241,
  786433, 1572869, 3145739, 6291469,
  12582917, 25165843, 50331653, 100663319,
  201326611, 402653189, 805306457, 1610612741,
  3221225081u
};

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

#ifdef PROFILE_NODE
/*!
  \brief The data structure used for profiling the internal hashtable

  
*/

struct node_profile_info_TAG {
  node_ptr bucket;
  unsigned long load;
  unsigned index;
};
#endif

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef struct node_profile_info_TAG node_profile_info;
typedef struct node_profile_info_TAG* node_profile_info_ptr;

typedef struct NodeMgr_TAG
{
  INHERITS_FROM(EnvObject);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */

  size_t allocated;          /* Number of nodes allocated till now */
  size_t hashed;             /* Number of nodes find_noded till now */
  size_t memused;            /* Total memory allocated by the node manager */

  node_ptr* nodelist;        /* The node hash table */
  node_ptr* memoryList;      /* Memory manager for symbol table */
  node_ptr nextFree;         /* List of free nodes */
  hash_ptr subst_hash;       /* The substitution hash */
  unsigned int nodelist_size;
  unsigned char nodelist_size_idx;
} NodeMgr;



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

static void node_manager_finalize(Object_ptr object, void* dummy);

static void node_manager_init(NodeMgr_ptr self,
                              const NuSMVEnv_ptr env);

static void node_manager_deinit(NodeMgr_ptr self);

static inline unsigned int node_hash_fun(node_ptr node);
static unsigned node_eq_fun(node_ptr node1, node_ptr node2);
static int node_cmp_fun(node_ptr node1, node_ptr node2);
static node_ptr node_alloc(NodeMgr_ptr self);
static node_ptr insert_node(NodeMgr_ptr self, node_ptr node);
static void _node_realloc_nodelist(NodeMgr_ptr self);

#ifdef PROFILE_NODE
static int profile_info_cmp(const void *a, const void *b);
#endif

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

NodeMgr_ptr NodeMgr_create(const NuSMVEnv_ptr env)
{
  NodeMgr_ptr self = ALLOC(NodeMgr, 1);
  NODE_MGR_CHECK_INSTANCE(self);

  node_manager_init(self, env);
  return self;
}

void NodeMgr_destroy(NodeMgr_ptr self)
{
  NODE_MGR_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}


#ifdef PROFILE_NODE

void NodeMgr_show_profile_stats(NodeMgr_ptr self, FILE* out)
{
  const unsigned int csize = self->nodelist_size;

  fprintf(out, "\nnode profiling results:\n");
  fprintf(out, "----------------------------------------------\n");

  node_profile_info* profile_info = ALLOC(node_profile_info, csize);

  unsigned long current_load=0;
  double load_avg = 0.0;
  double load_var = 0.0;
  int i; node_ptr p;

  /* collect load information and calculate stats */
  for (i=0; i<csize; ++i) {
    p = self->nodelist[i];

    profile_info[i].bucket = self->nodelist + i;
    profile_info[i].load = 0;
    profile_info[i].index = i;

    while (Nil != p) {
      ++ profile_info[i].load,  p = p->link;
    }

    current_load += profile_info[i].load;
  }

  /* calculate load average */
  load_avg = (double) current_load / (double) csize;

  /* calculate load variance */
  for (i=0;i<csize; ++i) {
    double var = (double) profile_info[i].load - load_avg;
    var *= var;

    load_var += var;
  }

  fprintf(out,
          "current load: %ld (%.3f%%)\n",
          current_load,
          (double) current_load / csize * 100);

  fprintf(out, "Avg (load): %.3f\n", load_avg);
  fprintf(out, "Var (load): %.3f\n", load_var );

  /* show 10 most loaded buckets */
  qsort(profile_info,
        csize,
        sizeof(node_profile_info),
        profile_info_cmp);

  fprintf(out, "\n10 most loaded buckets:\n");
  fprintf(out, "----------------------------------------------\n");
  for (i=0;i<10;i++) {
    fprintf(out,
            "%2d. index = %5d (@%8lx), load = %ld\n",
            i+1,
            (&profile_info[i])->index,
            (unsigned long) (&profile_info[i])->bucket,
            (&profile_info[i])->load);
  }

  FREE (profile_info);
}
#endif

node_ptr NodeMgr_new_node(NodeMgr_ptr self, int type,
                              node_ptr left, node_ptr right)
{
  extern int nusmv_yylineno;
  node_ptr node;

  node = node_alloc(self);
  /*node->locked         = 0;*/
  node->type           = type;
  node->lineno         = nusmv_yylineno;
  node->left.nodetype  = left;
  node->right.nodetype = right;
  return node;
}

node_ptr NodeMgr_new_lined_node(NodeMgr_ptr self, int type,
                                    node_ptr left, node_ptr right, int lineno)
{
  node_ptr node;

  node = node_alloc(self);
  /*node->locked         = 0;*/
  node->type           = type;
  node->lineno         = lineno;
  node->left.nodetype  = left;
  node->right.nodetype = right;

  return node;
}

void NodeMgr_free_node(NodeMgr_ptr self, node_ptr node)
{
  const unsigned int csize = self->nodelist_size;
  /* Check whether the node is in the hash. If it is, it should not
     be freed. */

  /*if (node->locked) return; */ /* hashed */

  {
    node_ptr * nodelist;
    node_ptr looking;
    unsigned int pos;

    nodelist = self->nodelist;

    pos = node_hash_fun(node) % csize;
    looking = nodelist[pos];
    while ((node_ptr) NULL != looking) {
      if (node == looking) return;
#ifdef DEBUG_FREE_NODE
      /* This tests that entries in the hash position have not
         been changed.  It might be the case that this test fails
         if an entry was previously changed by calling setcar or
         setcdr that should be not used with hashed nodes. */
      nusmv_assert(node_hash_fun(looking) % csize == pos);
#endif

#if INSERT_NODE_SORTED
      /* if sorted, we can exploit the ordering to decide that a
         node cannot be in the list */
      if (0 > node_cmp_fun(node, looking)) break; /* found limit */
#endif

      looking = looking->link;
    }
  }

  /*
    The node is not in the hash, so it can be freed.
  */
  /*nusmv_assert(!node->locked);*/
  node->link = self->nextFree;
  self->nextFree = node;

  /* This is a debugging feature to detect double freeing of the same
     node_ptr. This feature may considerably slowdown NuSMV.
     To use this feature compile NuSMV with "-DDEBUG_FREE_NODE" added to CFLAGS.
     For example, run
     make "CFLAGS=-Wall -g -DDEBUG_FREE_NODE"
     Do not forget to "touch src/node/node.c" before that.
  */
#ifdef DEBUG_FREE_NODE
  {
    node_ptr iter;
    for (iter = self->nextFree->link; iter != Nil; iter = iter->link) {
      if (iter == node) {
        NuSMVEnv_ptr env;

        env = EnvObject_get_environment(ENV_OBJECT(self));

        const ErrorMgr_ptr errmgr =
          ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
        /* print_sexp(errstream, node); */
        ErrorMgr_internal_error(errmgr, "The same node_ptr is freed twice.");
      }
    }
  }
#endif /* DEBUG_FREE_NODE */
}

node_ptr NodeMgr_find_node(NodeMgr_ptr self, int type,
                               node_ptr left, node_ptr right)
{
  extern int nusmv_yylineno;
  node_rec node;

  /*node.locked = 0;*/
  node.type = type;
  node.lineno = nusmv_yylineno;
  node.left.nodetype = left;
  node.right.nodetype = right;

  return insert_node(self, &node);
}

node_ptr NodeMgr_find_atom(NodeMgr_ptr self, node_ptr a)
{
  if (a == Nil) return(a);
  return NodeMgr_find_node(self, a->type,
                               a->left.nodetype, a->right.nodetype);
}

node_ptr NodeMgr_cons(NodeMgr_ptr self, node_ptr x, node_ptr y)
{
  return NodeMgr_new_node(self, CONS, x, y);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/* This is used for debugging purposes, it is exported (not static) 
   but not exposed in the interface */
/* [MD] I exposed it to avoid having a function without a prototype. I
   properly warned the user. */
void NodeMgr_self_check(NodeMgr_ptr self, boolean check_repeated)
{
  const unsigned int csize = self->nodelist_size;

  unsigned int i;
  for (i = 0; i < csize; ++i) {
    register node_ptr el;
    node_ptr prev;

    for (el = self->nodelist[i], prev = (node_ptr) NULL;
         (node_ptr) NULL != el;
         prev = el, el=el->link) {
      int j;

      /* ordering is guaranteed in each list */
      if (prev != (node_ptr) NULL) nusmv_assert(node_cmp_fun(prev, el) < 0);

      if (check_repeated) {
        /* there are no repeated elements in the hash */
        for (j=i+1; j < csize; ++j) {
          register node_ptr el2;
          for (el2 = self->nodelist[j]; (node_ptr) NULL != el2; el2=el2->link) {
            nusmv_assert(node_cmp_fun(el, el2) != 0);
          }
        }
      }
    }
  }
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The NodeMgr class private initializer

  The NodeMgr class private initializer

  \sa NodeMgr_create
*/
static void node_manager_init(NodeMgr_ptr self, const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  env_object_init(ENV_OBJECT(self), env);

  /* members initialization */
  self->allocated  = 0;
  self->hashed     = 0;
  self->memused    = 0;
  self->nodelist   = (node_ptr*) NULL;
  self->nodelist_size = 0;
  self->nodelist_size_idx = 0;
  self->memoryList = (node_ptr*) NULL;
  self->nextFree   = (node_ptr) NULL;

  { /* first allocation of nodelist */
    unsigned int newsize = node_primes[self->nodelist_size_idx];

    self->nodelist = (node_ptr*) ALLOC(node_ptr, newsize);
    if ((node_ptr*) NULL == self->nodelist) {
      ErrorMgr_internal_error(errmgr, "node_init: Out of Memory in allocating the node hash\n");
    }

    memset(self->nodelist, 0, newsize * sizeof(node_ptr));
    self->nodelist_size = newsize;
    self->nodelist_size_idx += 1;
  }

  self->subst_hash = new_assoc();

  OVERRIDE(Object, finalize) = node_manager_finalize;
}

/*!
  \brief The NodeMgr class virtual finalizer

  

  \sa NodeMgr_destroy
*/
static void node_manager_finalize(Object_ptr object, void* dummy)
{
  NodeMgr_ptr self = NODE_MGR(object);

  node_manager_deinit(self);

  FREE(self);
}

/*!
  \brief The NodeMgr class private deinitializer

  The NodeMgr class private deinitializer

  \sa NodeMgr_destroy
*/
static void node_manager_deinit(NodeMgr_ptr self)
{
  /* members deinitialization */
#ifdef PROFILE_NODE
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  StreamMgr_ptr streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* errstream = StreamMgr_get_error_stream(streams);
  NodeMgr_show_profile_stats(self, errstream);
#endif

#ifdef DEBUG_FREE_NODE
  /* Check that shared nodes have not been modified (see also
     free_node). */
  {
    const unsigned int csize = self->nodelist_size;
    int i;
    node_ptr p;

    for (i = 0; i < csize; ++i) {
      p =  self->nodelist[i];
      while (Nil != p) {
        nusmv_assert(node_hash_fun(p) % csize == i);
        p = p->link;
      }
    }
  }
#endif

  /* Shut down the node manager */
  node_ptr * next;
  node_ptr * memlist = self->memoryList;

  while(memlist != NULL) {
    next = (node_ptr *) memlist[0];
    FREE(memlist);
    memlist = next;
  }
  self->nextFree = (node_ptr)NULL;
  self->memoryList = (node_ptr *)NULL;
  clear_assoc(self->subst_hash);
  free_assoc(self->subst_hash);
  self->subst_hash = (hash_ptr)NULL;

  FREE(self->nodelist);
}

/*!
  \brief Inserts a node in the <tt>node</tt> hash.

  Checks if node is in the cache, if it is the
  case then the hashed value is returned, else a new one is created,
  stored in the hash and returned.

  \se None

  \sa find_node
*/
static node_ptr insert_node(NodeMgr_ptr self, node_ptr node)
#if INSERT_NODE_SIMPLE
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));

  node_ptr* nodelist;
  node_ptr looking;
  unsigned int pos;

  if ((((double) self->hashed) / self->nodelist_size) >
      HASH_REALLOC_THREASHOLD) {
    _node_realloc_nodelist(self);
  }

  nodelist = self->nodelist;

  pos = node_hash_fun(node) % self->nodelist_size;
  looking = nodelist[pos];

  while ((node_ptr) NULL != looking) {
    if (node_eq_fun(node, looking)) return looking;
    looking = looking->link;
  }

  /* The node is not in the hash, it is created and then inserted
     in it. */
  looking = node_alloc();
  if (looking == (node_ptr) NULL) {
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
    ErrorMgr_internal_error(errmgr, "insert_node: Out of Memory\n");
  }
  /*looking->locked = 1;*/
  looking->type = node->type;
  looking->lineno = node->lineno;
  looking->left.nodetype = node->left.nodetype;
  looking->right.nodetype = node->right.nodetype;
  looking->link = nodelist[pos];
  looking->extra_data = NULL;
  nodelist[pos] = looking;

  self->hashed += 1;
  return looking;
}


#elif INSERT_NODE_PROXIMITY
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  node_ptr* nodelist;
  node_ptr looking;
  node_ptr prev, pprev;
  unsigned int pos;

  if ((((double) self->hashed) / self->nodelist_size) >
      HASH_REALLOC_THREASHOLD) {
    _node_realloc_nodelist(self);
  }

  nodelist = self->nodelist;

  pos = node_hash_fun(node) % self->nodelist_size;
  looking = nodelist[pos];
  pprev = prev = (node_ptr) NULL; /* for swapping */
  while ((node_ptr) NULL != looking) {
    if (node_eq_fun(node, looking)) {
      if ((node_ptr) NULL != prev) {
        prev->link = looking->link;
        looking->link = prev;

        if ((node_ptr) NULL != pprev) pprev->link = looking;
        else { /* looking becomes the head */
          nodelist[pos] = looking;
        }
      }
      return looking;
    }
    pprev = prev; prev = looking;
    looking = looking->link;
  }

  /* The node is not in the hash, it is created and then inserted
     in it. */
  looking = node_alloc(self);
  if ((node_ptr) NULL == looking) {
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
    ErrorMgr_internal_error(errmgr, "insert_node: Out of Memory\n");
  }
  /*looking->locked = 1;*/
  looking->type = node->type;
  looking->lineno = node->lineno;
  looking->left.nodetype = node->left.nodetype;
  looking->right.nodetype = node->right.nodetype;
  looking->link = nodelist[pos];
  looking->extra_data = NULL;
  nodelist[pos] = looking;

  self->hashed += 1;
  return looking;
}


#elif INSERT_NODE_SORTED
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  node_ptr* nodelist;
  node_ptr looking;
  node_ptr prev;
  unsigned int pos;

  if ((((double) self->hashed) / ((double) self->nodelist_size)) >
      HASH_REALLOC_THREASHOLD) {
    _node_realloc_nodelist(self);
  }

  nodelist = self->nodelist;

  pos = node_hash_fun(node) % self->nodelist_size;
  looking = nodelist[pos];
  prev = (node_ptr) NULL; /* for inserting after it */
  while ((node_ptr) NULL != looking) {
    const int cmp = node_cmp_fun(node, looking);
    if (0 == cmp) return looking;
    if (0 > cmp) break; /* found greater element, insert before it */
    prev = looking;
    looking = looking->link;
  }

  /* The node is not in the hash, it is created and then inserted
     in it (either at prev position, or prepended. */
  /*nusmv_assert(!node->locked);*/ /* node cannot be locked here */
  looking = node_alloc();
  if ((node_ptr) NULL == looking) {
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
    ErrorMgr_internal_error(errmgr, "insert_node: Out of Memory\n");
  }
  /*looking->locked = 1;*/
  looking->type = node->type;
  looking->lineno = node->lineno;
  looking->left.nodetype = node->left.nodetype;
  looking->right.nodetype = node->right.nodetype;
  looking->extra_data = NULL;

  if ((node_ptr) NULL != prev) { /* insert point is after prev */
    looking->link = prev->link;
    prev->link = looking;
  }
  else {
    /* appends to the head */
    looking->link = nodelist[pos];
    nodelist[pos] = looking;
  }

  self->hashed += 1;
  return looking;
}
#else
# error "Insert node type unrecognized"
#endif

/*!
  \brief Hash function for <tt>node</tt>s.

  \todo Missing description

  \se None

  \sa node_eq_fun
*/
static inline unsigned int node_hash_fun(node_ptr node)
{
#if 1 /* new little-endian, seems to perform better with pointer-based
         ordering of sub-formulas (See Expr.c) */
  return (unsigned int) (((((size_t)  node->type )  + 31  )  +
                          (((size_t)  node->left.nodetype ) << 2)  +
                          (((size_t)  node->right.nodetype) << 1)));
#elif 0 /* little-endian */
  return (unsigned int) (((((size_t)  node->type )     )  +
                          (((size_t)  node->left.nodetype ) << 1)  +
                          (((size_t)  node->right.nodetype) << 2)));
#elif 0 /* big-endian 0 (TO BE TESTED) */
  return (unsigned int)
    (((((size_t)  node->type )     )  +
      (((size_t)  node->left.nodetype ) >> (NUSMV_SIZEOF_VOID * 4 - 1))  +
      (((size_t)  node->right.nodetype) << (NUSMV_SIZEOF_VOID * 4 - 2))));

#else /* big-endian 1 (TO BE TESTED) */
  return (unsigned int)
    ((((size_t)  node->type ))  +
     ((((size_t)  node->left.nodetype) << 17) ^ (((size_t)  node->left.nodetype) >> 16)) +
     ((((size_t)  node->right.nodetype) << 17) ^ (((size_t)  node->right.nodetype) >> 16)));

#endif
}

/*!
  \brief Equality function for <tt>node</tt> hash.

  \todo Missing description

  \se None

  \sa node_hash_fun
*/
static unsigned node_eq_fun(node_ptr node1, node_ptr node2)
{
  return((node1->left.nodetype == node2->left.nodetype) &&
         (node1->right.nodetype == node2->right.nodetype) &&
         (node1->type == node2->type));
}

/*!
  \brief Comparison function for <tt>node</tt> sorted insertion.
  Returns is < 0 if node1 < node2, 0 if node1 == node2, and > 0 if
  node1 > node2

  \todo Missing description

  \se None

  \sa node_hash_fun
*/
static int node_cmp_fun(node_ptr node1, node_ptr node2)
{
  if (node1->type == node2->type) {
    if (node1->left.nodetype == node2->left.nodetype) {
      if (node1->right.nodetype == node2->right.nodetype) return 0;
      return (node1->right.nodetype > node2->right.nodetype) ? 1 : -1;
    }
    return (node1->left.nodetype > node2->left.nodetype) ? 1 : -1;
  }

  return (node1->type > node2->type) ? 1 : -1;
}

/*!
  \brief Allocates NODE_MEM_CHUNK records and stores them
  in the free list of the <tt>node</tt> manager.

  Allocates NODE_MEM_CHUNK records and stores them
  in the free list of the <tt>node</tt> manager.

  \se The free list of the <tt>node</tt> manager is
  updated by appending the new allocated nodes.
*/
static node_ptr node_alloc(NodeMgr_ptr self)
{
  int i;
  node_ptr node;

  if (self->nextFree == (node_ptr) NULL) { /* memory is full */
    node_ptr list;
    node_ptr* mem = (node_ptr*) ALLOC(node_rec, NODE_MEM_CHUNK + 1);

    if (mem == (node_ptr*) NULL) { /* out of memory */
      const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
      const StreamMgr_ptr streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

      StreamMgr_print_error(streams,  "node_alloc: out of memory\n");
      ErrorMgr_internal_error(errmgr, "Memory in use for nodes = %ld\n", self->memused);
    }
    else { /* Adjust manager data structure */
      self->memused += (NODE_MEM_CHUNK + 1) * sizeof(node_rec);
      mem[0] = (node_ptr) self->memoryList;
      self->memoryList = mem;

      list = (node_ptr) mem;
      /* Link the new set of allocated node together */
      i = 1;
      do {
        list[i].link = &list[i+1];
      } while (++i < NODE_MEM_CHUNK);
      list[NODE_MEM_CHUNK].link = (node_ptr) NULL;

      self->nextFree = &list[1];
    }
  }

  /* Now the list of nextFree is not empty */
  self->allocated++;
  node = self->nextFree; /* Takes the first free available node */
  self->nextFree = node->link;
  node->link = (node_ptr) NULL;
  return node;
}

/*!
  \brief Reallocation of the hash

  If possible (i.e. upper limit not reached yet)
  reallcoates the hash table of nodes. There are two strategies:
  the first try allocating a new bunch of memory, the second (if
  the former fails due to low memory) tries to enarge the existing
  hash. The hash is reallocated when a given load threashold is
  reached, when inserting nodes in the hash.

  \se None
*/
static void _node_realloc_nodelist(NodeMgr_ptr self)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  node_ptr* nodelist;
  unsigned int newsize;

  if (self->nodelist_size_idx >= NODE_PRIMES_SIZE) {
    /* reached the size limit, sorry size remains untouched */
    return;
  }

  if (opt_verbose_level_gt(opts, 3)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Node: Reallocating buckets... ");
  }

  newsize = node_primes[self->nodelist_size_idx];
  nodelist = (node_ptr*) ALLOC(node_ptr, newsize);

  if ((node_ptr*) NULL != nodelist) {

    memset(nodelist, 0, newsize * sizeof(node_ptr));

    {
      const unsigned int csize = self->nodelist_size;
      node_ptr* cnodelist = self->nodelist; /* alias */
      register node_ptr el;
      unsigned int newpos;
      unsigned int i;
      for (i = 0; i < csize; ++i) {
        for (el = cnodelist[i]; (node_ptr) NULL != el; el = cnodelist[i]) {
          cnodelist[i] = el->link; /* removes the element */
          newpos = node_hash_fun(el) % newsize;

#if INSERT_NODE_SIMPLE
          /* This algorithm is 'unstable', as it reverses the list
             alternatively */
          el->link = nodelist[newpos];
          nodelist[newpos] = el;

#elif INSERT_NODE_PROXIMITY
          { /* inserts at the end to keep stability of the list */
            register node_ptr el2 = nodelist[newpos];
            if ((node_ptr) NULL != el2) {
              while ((node_ptr) NULL != el2->link) el2 = el2->link;
              el2->link = el; /* append */
            }
            else { /* head of the (empty) list */
              nodelist[newpos] = el;
            }
            el->link = (node_ptr) NULL; /* last element */
          }

#elif INSERT_NODE_SORTED
          { /* similar to proximity, but searches the insertion
               point in the new list instead of appending */
            register node_ptr el2 = nodelist[newpos];
            node_ptr prev = (node_ptr) NULL;
            while ((node_ptr) NULL != el2) {
              const int cmp = node_cmp_fun(el, el2);
              nusmv_assert(0 != cmp); /* el cannot be in the list already */
              if (0 > cmp) break; /* found limit */
              prev = el2;
              el2 = el2->link;
            }

            if ((node_ptr) NULL != prev) { /* in the middle, after prev */
              el->link = prev->link;
              prev->link = el;
            }
            else { /* head of the list (possibly empty) */
              el->link = nodelist[newpos];
              nodelist[newpos] = el;
            }
          }
#else
# error "Insert node type unrecognized"
#endif
        }
      }

      FREE(cnodelist);
    }
  }
  else {
    /* not enough memory: fallback try with realloc */
    nodelist = (node_ptr*) REALLOC(node_ptr, self->nodelist, newsize);
    nusmv_assert((node_ptr*) NULL != nodelist);

    /* succeeded: resets the added part */
    memset(nodelist+self->nodelist_size, 0,
           (newsize - self->nodelist_size) * sizeof(node_ptr));

    { /* now moves elements in buckets' lists */
      const unsigned int csize = self->nodelist_size; /* alias */
      unsigned int i;

      for (i=0; i<csize; ++i) {
        node_ptr pel = (node_ptr) NULL;
        register node_ptr el;

        for (el = nodelist[i]; (node_ptr) NULL != el;) {
          unsigned int newpos = node_hash_fun(el) % newsize;

          if (newpos == i) { /* position remains untouched */
            /* the list keeps its ordering properties */
            pel = el;
            el = el->link;
          }
          else { /* element has to be moved into another bucket */
            node_ptr tmp = el->link; /* tmp points to the next
                                        element in the input
                                        list */

            /* removes the element 'el' from the old bucket */
            if ((node_ptr) NULL != pel) pel->link = tmp;
            else nodelist[i] = tmp;

#if INSERT_NODE_SIMPLE
            /* This algorithm is 'unstable', as it reverses the
               list alternatively */
            el->link = nodelist[newpos];
            nodelist[newpos] = el;

#elif INSERT_NODE_PROXIMITY
            { /* inserts at the end to keep stability of the list */
              register node_ptr el2 = nodelist[newpos];
              if ((node_ptr) NULL != el2) {
                while ((node_ptr) NULL != el2->link) el2 = el2->link;
                el2->link = el; /* append */
              }
              else { /* head of the (empty) list */
                nodelist[newpos] = el;
              }
              el->link = (node_ptr) NULL; /* last element */
            }

#elif INSERT_NODE_SORTED
            { /* similar to proximity, but searches the insertion
                 point in the new list instead of appending */
              register node_ptr el2 = nodelist[newpos];
              node_ptr prev = (node_ptr) NULL;

              while ((node_ptr) NULL != el2) {
                const int cmp = node_cmp_fun(el, el2);
                nusmv_assert(0 != cmp); /* el cannot be in the list already */
                if (0 > cmp) break; /* found limit */
                prev = el2;
                el2 = el2->link;
              }

              if ((node_ptr) NULL != prev) { /* in the middle, after prev */
                el->link = prev->link;
                prev->link = el;
              }
              else { /* head of the list (possibly empty) */
                el->link = nodelist[newpos];
                nodelist[newpos] = el;
              }
            }
#else
# error "Insert node type unrecognized"
#endif
            el = tmp;
          }
        }
      }
    } /* block */
  }

  self->nodelist = nodelist;
  self->nodelist_size = newsize;
  self->nodelist_size_idx += 1;


  /*_node_self_check(true);*/
  if (opt_verbose_level_gt(opts, 3)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Done. Size is now %d\n", newsize);
  }
}


#ifdef PROFILE_NODE

/*!
  \brief Comparison function used for profiling

  \todo Missing description
*/
static
int profile_info_cmp(const void *a, const void *b)
{
  long res =
    (*((node_profile_info_ptr) b)).load -
    (*((node_profile_info_ptr) a)).load;

  return (int) res;
}
#endif


/**AutomaticEnd***************************************************************/

