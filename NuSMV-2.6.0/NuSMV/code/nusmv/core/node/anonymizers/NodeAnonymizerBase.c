/* ---------------------------------------------------------------------------


  This file is part of the ``node.anonymizers'' package of NuSMV version 2.
  Copyright (C) 2014 by FBK-irst.

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
  \brief Implementation of class 'NodeAnonymizerBase'

  \todo: Missing description

*/

#if HAVE_CONFIG_H
#  include "nusmv-config.h"
#endif

#include "nusmv/core/node/anonymizers/NodeAnonymizerBase.h"
#include "nusmv/core/node/anonymizers/NodeAnonymizerBase_private.h"
#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#include "nusmv/core/compile/compileUtil.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/node/anonymizers/PrinterNonAmbiguousDot.h"
#include "nusmv/core/utils/object.h"
#include "nusmv/core/node/MasterNodeWalker.h"
#include "nusmv/core/parser/symbols.h"

#if NUSMV_HAVE_STRING_H
#  include <string.h>
#else
#  error "string.h has not been found, but is required for strlen"
#endif

#include <stdio.h>

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'NodeAnonymizerBase_private.h' for class 'NodeAnonymizerBase' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief Max length of a line in the map file


*/
#define NAB_MAX_LINE_LEN 1000


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void node_anonymizer_base_finalize(Object_ptr object, void* dummy);
static node_ptr node_anonymizer_base_search_mapping_back(NodeAnonymizerBase_ptr self,
                                                         node_ptr id);
static node_ptr nab_map_expr_rec(NodeAnonymizerBase_ptr self,
                                 node_ptr expr);
static node_ptr nab_map_back_rec(NodeAnonymizerBase_ptr self,
                                 node_ptr expr);
static int nab_print_map_entry(NodeAnonymizerBase_ptr self,
                               node_ptr orig,
                               MasterPrinter_ptr anon_print);
static node_ptr nab_convert_id_str_to_node(NodeAnonymizerBase_ptr self,
                                           char** id);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void NodeAnonymizerBase_destroy(NodeAnonymizerBase_ptr self)
{
  NODE_ANONYMIZER_BASE_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

int NodeAnonymizerBase_map(NodeAnonymizerBase_ptr self,
                           node_ptr id,
                           const char* prefix)
{
  int retval = 0;

  NODE_ANONYMIZER_BASE_CHECK_INSTANCE(self);
  nusmv_assert(self->is_id(self, id));
  nusmv_assert(! node_anonymizer_base_is_id_original(self, id));
  nusmv_assert(NULL != prefix);

  (void)self->translate(self, id, prefix);

  return retval;
}

int NodeAnonymizerBase_force_map(NodeAnonymizerBase_ptr self,
                                 node_ptr original,
                                 node_ptr anonymous)
{
  int retval = 0;

  NODE_ANONYMIZER_BASE_CHECK_INSTANCE(self);
  nusmv_assert(self->is_id(self, original));
  nusmv_assert(self->is_id(self, anonymous));
  nusmv_assert(! node_anonymizer_base_is_id_original(self, original));
  nusmv_assert(! node_anonymizer_base_is_id_anonymous(self, anonymous));

  node_anonymizer_base_insert_mapping(self, original, anonymous);

  return retval;
}

boolean NodeAnonymizerBase_is_id_original(NodeAnonymizerBase_ptr self,
                                          node_ptr id)
{
  NODE_ANONYMIZER_BASE_CHECK_INSTANCE(self);

  return node_anonymizer_base_is_id_original(self, id);
}

boolean NodeAnonymizerBase_is_id_anonymous(NodeAnonymizerBase_ptr self,
                                           node_ptr id)
{
  NODE_ANONYMIZER_BASE_CHECK_INSTANCE(self);

  return node_anonymizer_base_is_id_anonymous(self, id);
}

node_ptr NodeAnonymizerBase_map_expr(NodeAnonymizerBase_ptr self,
                                     node_ptr expr)
{
  node_ptr retval = NULL;

  NODE_ANONYMIZER_BASE_CHECK_INSTANCE(self);

  retval = node_anonymizer_base_map_expr(self, expr);

  nusmv_assert(Nil == expr || (node_ptr)NULL !=  retval);

  return retval;
}

node_ptr NodeAnonymizerBase_map_back(NodeAnonymizerBase_ptr self,
                                     node_ptr expr)
{
  node_ptr retval;

  NODE_ANONYMIZER_BASE_CHECK_INSTANCE(self);
  nusmv_assert((node_ptr)NULL !=  expr);

  retval = node_anonymizer_base_map_back(self, expr);

  return retval;
}

int NodeAnonymizerBase_print_map(NodeAnonymizerBase_ptr self,
                                 FILE* stream)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  MasterPrinter_ptr anon_print = NULL;
  int retval = 0;
  BiMapIter iter;
  PrinterNonAmbiguousDot_ptr entry_print;

  NODE_ANONYMIZER_BASE_CHECK_INSTANCE(self);
  nusmv_assert(NULL != stream);
  nusmv_assert(! BiMap_is_empty(self->map));

  /* Printer setup */
  anon_print = MasterPrinter_create(env);
  {
    union StreamTypeArg sta;
    sta.file = stream;
    MasterPrinter_set_stream_type(anon_print, STREAM_TYPE_FILE, &sta);
  }
  entry_print = PrinterNonAmbiguousDot_create(env);
  (void)MasterNodeWalker_register_walker(MASTER_NODE_WALKER(anon_print),
                                         NODE_WALKER(entry_print));

  /* print header */
  fprintf(stream, "ORIGINAL ID %s ANONYMOUS ID\n",
          NODE_ANONYMIZER_DELIMITER_STR);

  /* print data */
  BI_MAP_FOREACH(self->map, iter) {
    node_ptr orig;

    orig = BiMap_iter_get_domain_element(self->map, &iter);

    retval = nab_print_map_entry(self, orig, anon_print);
  }

  /* Printer deinit, walker is destroyed as well, but stream is not touched */
  Object_destroy(OBJECT(anon_print), NULL);

  fflush(stream);

  return retval;
}

int NodeAnonymizerBase_read_map_from_stream(NodeAnonymizerBase_ptr self,
                                            FILE* stream)
{
  int status = 1;

  /* First read the map. In case of errors, self is not modified */
  BiMap_ptr tmp_map;
  char* line = ALLOC(char, NAB_MAX_LINE_LEN);

  tmp_map = BiMap_create();

  /* consume the header */
  line = fgets(line, NAB_MAX_LINE_LEN, stream);

  line = fgets(line, NAB_MAX_LINE_LEN, stream);

  NAB_DEBUG_PRINT("%s INPUT\n%s", __func__, line);

  /* foreach line */
  while (NULL != line) {
    char* sorig = NULL;
    char* sanonymous = NULL;

    NAB_DEBUG_PRINT("%s INPUT\n%s", __func__, line);

    /* get the two identifiers */
    {
      char* pch;

      sorig = strtok(line, NODE_ANONYMIZER_DELIMITER_STR);
      if (NULL == sorig) goto error;

      /* do not read the newline as well */
      sanonymous = strtok(NULL, NODE_ANONYMIZER_DELIMITER_STR"\r\n");
      if (NULL == sanonymous) goto error;

      pch = strtok(NULL, NODE_ANONYMIZER_DELIMITER_STR);
      /* This could be just a warning */
      if (NULL != pch) goto error;
    }

    /* convert them to nodes and add to the bimap */
    {
      node_ptr norig;
      node_ptr nanonymous;

      norig = nab_convert_id_str_to_node(self, &sorig);
      if (NULL == norig) goto error;
      nanonymous = nab_convert_id_str_to_node(self, &sanonymous);
      if (NULL == nanonymous) goto error;

      NAB_DEBUG_PRINT("%s OUTPUT\n%N\n%N", __func__, norig, nanonymous);

      BiMap_put(tmp_map, norig, nanonymous);
    }

    line = fgets(line, NAB_MAX_LINE_LEN, stream);
  }

  /* If no error, update self */
  status = node_anonymizer_read_map_from_bimap(self, tmp_map);

  if (0 != status) goto error;

  /* Success */
  status = 0;
  goto exit;

 error:
  status = 1;

  /* Cleanup */
 exit:
  FREE(line);
  if (NULL != tmp_map) BiMap_destroy(tmp_map);

  return status;
}

int NodeAnonymizer_read_map_from_bimap(NodeAnonymizerBase_ptr self,
                                       BiMap_ptr map)
{
  NODE_ANONYMIZER_BASE_CHECK_INSTANCE(self);

  return node_anonymizer_read_map_from_bimap(self,  map);
}

size_t NodeAnonymizerBase_get_map_size(NodeAnonymizerBase_ptr self)
{
  NODE_ANONYMIZER_BASE_CHECK_INSTANCE(self);

  return (size_t)BiMap_size(self->map);
}

boolean NodeAnonymizerBase_is_map_empty(NodeAnonymizerBase_ptr self)
{
  NODE_ANONYMIZER_BASE_CHECK_INSTANCE(self);

  return BiMap_is_empty(self->map);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

boolean node_anonymizer_base_is_id_original(NodeAnonymizerBase_ptr self,
                                            node_ptr id)
{
  boolean retval;
  node_ptr anonymous;

  nusmv_assert(self->is_id(self, id));

  anonymous = node_anonymizer_base_search_mapping(self, id);
  retval = NULL != anonymous;

  return retval;
}

boolean node_anonymizer_base_is_id_anonymous(NodeAnonymizerBase_ptr self,
                                             node_ptr id)
{
  boolean retval;
  node_ptr original;

  nusmv_assert(self->is_id(self, id));

  original = node_anonymizer_base_search_mapping_back(self, id);
  retval = NULL != original;

  return retval;
}

void node_anonymizer_base_init(NodeAnonymizerBase_ptr self,
                               NuSMVEnv_ptr env,
                               const char* default_prefix,
                               size_t memoization_threshold)
{
  /* base class initialization */
  env_object_init(ENV_OBJECT(self), env);

  /* members initialization */
  self->map = BiMap_create();
  self->memoization_threshold = memoization_threshold;
  self->orig2anon = LRUCache_create(self->memoization_threshold / 2,
                                    OAHash_pointer_eq_fun,
                                    OAHash_pointer_hash_fun,
                                    NULL,
                                    NULL);
  self->anon2orig = LRUCache_create(self->memoization_threshold / 2,
                                    OAHash_pointer_eq_fun,
                                    OAHash_pointer_hash_fun,
                                    NULL,
                                    NULL);
  self->counter = 1;
  if (NULL != default_prefix) {
    self->default_prefix = default_prefix;
  }
  else self->default_prefix = "x";

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = node_anonymizer_base_finalize;

  OVERRIDE(NodeAnonymizerBase, translate) = node_anonymizer_base_translate;
  OVERRIDE(NodeAnonymizerBase, build_anonymous) =
    node_anonymizer_base_build_anonymous;
  OVERRIDE(NodeAnonymizerBase, is_leaf) = node_anonymizer_base_is_leaf;
  OVERRIDE(NodeAnonymizerBase, is_id) = node_anonymizer_base_is_id;

#ifdef NODE_ANONYMIZER_BASE_DEBUG
  nab_debug_stream = fopen("nab_debug.txt", "w");
  nab_debug_logger = Logger_create(nab_debug_stream);
#endif
}

void node_anonymizer_base_deinit(NodeAnonymizerBase_ptr self)
{
  /* members deinitialization */
  BiMap_destroy(self->map); self->map = NULL;
  LRUCache_destroy(self->orig2anon); self->orig2anon = NULL;
  LRUCache_destroy(self->anon2orig); self->anon2orig = NULL;
  self->memoization_threshold = 0;
  self->counter = 0;
  self->default_prefix = NULL;

  /* base class deinitialization */
  env_object_deinit(ENV_OBJECT(self));

#ifdef NODE_ANONYMIZER_BASE_DEBUG
  if (NULL != nab_debug_logger) Logger_destroy(nab_debug_logger);
  nab_debug_logger = NULL;
  nab_debug_stream = NULL;
#endif
}

node_ptr node_anonymizer_base_translate(NodeAnonymizerBase_ptr self,
                                        node_ptr id,
                                        const char* prefix)
{
  node_ptr retval = NULL;

  nusmv_assert(self->is_id);
  nusmv_assert((char*)NULL !=  prefix);

  NAB_DEBUG_PRINT("%s INPUT\n%N", __func__, id);

  /* See if it is already in the map */
  retval = node_anonymizer_base_search_mapping(self, id);

  if (NULL == retval) {
    NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
    /* Not in the map: build the string */
    const char* anonymous_name = NULL;
    NodeMgr_ptr const nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    do {
      anonymous_name = self->build_anonymous(self, id, prefix);

      /* build the node */
      retval = sym_intern(env, (char*)anonymous_name);
      FREE(anonymous_name);
    } while (NodeAnonymizerBase_is_id_anonymous(self, retval));

    /* insert it in the map */
    node_anonymizer_base_insert_mapping(self, id, retval);
  }

  NAB_DEBUG_PRINT("%s INPUT\n%N", __func__, retval);

  nusmv_assert((node_ptr)NULL !=  retval);

  return retval;
}

boolean node_anonymizer_base_is_leaf(NodeAnonymizerBase_ptr self,
                                     node_ptr node)
{
  boolean retval;

  retval = node_is_leaf(node) || self->is_id(self, node);

  return retval;
}

boolean node_anonymizer_base_is_id(NodeAnonymizerBase_ptr self,
                                   node_ptr id)
{
  boolean retval = false;

  error_unreachable_code_msg("Error: NodeAnonymizerBase is an abstract class and \
method is_id has to be implemented by the subclasses");

  return retval;
}

const char* node_anonymizer_base_choose_prefix(NodeAnonymizerBase_ptr self,
                                               const char* prefix)
{
  const char* out_prefix = NULL;

  nusmv_assert(NULL != prefix);

  if ('\0' == prefix[0]) {
    out_prefix = self->default_prefix;
  }
  else out_prefix = prefix;

  nusmv_assert(NULL != out_prefix);

  return out_prefix;
}

node_ptr node_anonymizer_base_map_expr(NodeAnonymizerBase_ptr self,
                                       node_ptr expr)
{
  node_ptr retval;

  if (Nil == expr) {
    retval = Nil;
  }
  else retval = nab_map_expr_rec(self, expr);

  nusmv_assert(Nil == expr || (node_ptr)NULL !=  retval);

  return retval;
}

node_ptr node_anonymizer_base_map_back(NodeAnonymizerBase_ptr self,
                                       node_ptr expr)
{
  node_ptr retval;

  nusmv_assert((node_ptr)NULL !=  expr);

  retval = nab_map_back_rec(self, expr);

  return retval;
}

node_ptr node_anonymizer_base_search_expr_cache(NodeAnonymizerBase_ptr self,
                                                node_ptr expr)
{
  node_ptr retval;

  nusmv_assert((node_ptr)NULL !=  expr);
  nusmv_assert(! self->is_leaf(self, expr));
  nusmv_assert(! self->is_id(self, expr));

  retval = NODE_PTR(LRUCache_lookup(self->orig2anon, expr));

  return retval;
}

void node_anonymizer_base_insert_expr_cache(NodeAnonymizerBase_ptr self,
                                            node_ptr expr,
                                            node_ptr anonymous_expr)
{
  boolean replaced = true;

  nusmv_assert((node_ptr)NULL !=  expr);
  nusmv_assert(! self->is_leaf(self, expr));
  nusmv_assert(! self->is_id(self, expr));
  nusmv_assert((node_ptr)NULL !=  anonymous_expr);
  nusmv_assert(! self->is_leaf(self, anonymous_expr));
  nusmv_assert(! self->is_id(self, anonymous_expr));

  replaced = LRUCache_insert(self->orig2anon,
                             (void*)expr,
                             (void*)anonymous_expr);

  nusmv_assert(! replaced);
}

node_ptr node_anonymizer_base_search_anon2orig(NodeAnonymizerBase_ptr self,
                                               node_ptr expr)
{
  node_ptr retval;

  nusmv_assert((node_ptr)NULL !=  expr);
  nusmv_assert(! self->is_leaf(self, expr));
  nusmv_assert(! self->is_id(self, expr));

  retval = NODE_PTR(LRUCache_lookup(self->anon2orig, expr));

  return retval;
}

void node_anonymizer_base_insert_anon2orig(NodeAnonymizerBase_ptr self,
                                           node_ptr anonymous_expr,
                                           node_ptr expr)
{
  boolean replaced = true;

  nusmv_assert((node_ptr)NULL !=  expr);
  nusmv_assert(! self->is_leaf(self, expr));
  nusmv_assert(! self->is_id(self, expr));
  nusmv_assert((node_ptr)NULL !=  anonymous_expr);
  nusmv_assert(! self->is_leaf(self, anonymous_expr));
  nusmv_assert(! self->is_id(self, anonymous_expr));

  replaced = LRUCache_insert(self->anon2orig,
                             (void*)anonymous_expr,
                             (void*)expr);

  nusmv_assert(! replaced);
}

int node_anonymizer_read_map_from_bimap(NodeAnonymizerBase_ptr self,
                                        BiMap_ptr map)
{
  int status = 0;
  BiMapIter iter;

  /* This could be a BiMap_merge method */
  /* Here we should first check if input is safe, and then add all the
     elements */
  BI_MAP_FOREACH(map, iter) {
    node_ptr domain = NODE_PTR(BiMap_iter_get_domain_element(map, &iter));
    node_ptr codomain = NODE_PTR(BiMap_iter_get_codomain_element(map, &iter));

    nusmv_assert(self->is_id(self, domain));
    nusmv_assert(self->is_id(self, codomain));

    if (BiMap_domain_contains(self->map, domain)) {
      node_ptr current_codomain = BiMap_get(self->map, domain);

      if (current_codomain != codomain) {
        status = 1;
        break;
      }
    }
    else if (BiMap_codomain_contains(self->map, codomain)) {
      status = 1;
      break;
    }
    else {
      node_anonymizer_base_insert_mapping(self, domain, codomain);
    }
  }

  return status;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The NodeAnonymizerBase class virtual finalizer

  Called by the class destructor
*/
static void node_anonymizer_base_finalize(Object_ptr object, void* dummy)
{
  NodeAnonymizerBase_ptr self = NODE_ANONYMIZER_BASE(object);

  node_anonymizer_base_deinit(self);
  FREE(self);
}

const char* node_anonymizer_base_build_anonymous(NodeAnonymizerBase_ptr self,
                                                 node_ptr id,
                                                 const char* prefix)
{
  const char* anonymous_name;
  const char* out_prefix;
  const char* str_counter;
  size_t len_prefix;
  size_t len_counter;
  size_t len_anonymous;

  out_prefix = node_anonymizer_base_choose_prefix(self, prefix);

  len_prefix = strlen(out_prefix);
  len_counter = Utils_int_size_as_string((int)self->counter);
  /* terminator char is already considered by Utils_int_size_as_string */
  len_anonymous = len_prefix + len_counter;

  str_counter = Utils_int_to_str((int)self->counter);

  anonymous_name = ALLOC(char, len_anonymous);

  {
    int c = snprintf((char*)anonymous_name, len_anonymous, "%s%s",
                     out_prefix,
                     str_counter);
    SNPRINTF_CHECK(c, len_anonymous);
  }

  self->counter = self->counter + 1;

  return anonymous_name;
}

node_ptr node_anonymizer_base_search_mapping(NodeAnonymizerBase_ptr self,
                                             node_ptr id)
{
  node_ptr retval = NULL;
  node_ptr id_norm = NULL;
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  MasterNormalizer_ptr const master_norm =
    MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));

  id = MasterNormalizer_normalize_node(master_norm, id);

  if (BiMap_domain_contains(self->map, id)) {
    retval = BiMap_get(self->map, id);
  }
  else retval = NULL;

  return retval;
}

/*!
  \brief Searches id in the mapping back


  @node_ptr the original id corresponding to id if found, otherwise null
*/

static node_ptr node_anonymizer_base_search_mapping_back(NodeAnonymizerBase_ptr self,
                                                         node_ptr id)
{
  node_ptr retval = NULL;
  node_ptr id_norm = NULL;
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  MasterNormalizer_ptr const master_norm =
    MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));

  id = MasterNormalizer_normalize_node(master_norm, id);

  if (BiMap_codomain_contains(self->map, id)) {
    retval = BiMap_inverse_get(self->map, id);
  }
  else retval = NULL;

  return retval;
}

void node_anonymizer_base_insert_mapping(NodeAnonymizerBase_ptr self,
                                         node_ptr id,
                                         node_ptr anonymous)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  MasterNormalizer_ptr const master_norm =
    MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));

  id = MasterNormalizer_normalize_node(master_norm, id);

  nusmv_assert(! BiMap_domain_contains(self->map, id));
  nusmv_assert(NULL != anonymous);

  BiMap_put(self->map, id, anonymous);
}

/*!
  \brief Recursively translated an expression, caching the results

  identifiers and leaves are not cached
*/

static node_ptr nab_map_expr_rec(NodeAnonymizerBase_ptr self,
                                 node_ptr expr)
{
  node_ptr retval;

  nusmv_assert((node_ptr)NULL !=  expr);

  NAB_DEBUG_PRINT("%s INPUT\n%N", __func__, expr)

  if (self->is_id(self, expr)) {
    retval = self->translate(self, expr, "");
  }
  else if (self->is_leaf(self, expr)) retval = expr;
  else {
    /* check cache */
    retval = node_anonymizer_base_search_expr_cache(self, expr);

    if (NULL == retval) {
      /* not in cache */
      NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
      NodeMgr_ptr const nodemgr =
        NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

      retval =
        find_node(nodemgr, node_get_type(expr),
                  Nil != car(expr) ? nab_map_expr_rec(self, car(expr)) : Nil,
                  Nil != cdr(expr) ? nab_map_expr_rec(self, cdr(expr)) : Nil);

      /* update cache */
      node_anonymizer_base_insert_expr_cache(self, expr, retval);
    }
  }

  NAB_DEBUG_PRINT("%s OUTPUT\n%N", __func__, retval)

  nusmv_assert((node_ptr)NULL !=  retval);

  return retval;
}

/*!
  \brief Recursively translated an expression, caching the results

  identifiers and leaves are not cached
*/

static node_ptr nab_map_back_rec(NodeAnonymizerBase_ptr self,
                                 node_ptr expr)
{
  node_ptr retval;

  nusmv_assert((node_ptr)NULL !=  expr);

  NAB_DEBUG_PRINT("%s INPUT\n%N", __func__, expr)

  if (self->is_id(self, expr)) {
    retval = node_anonymizer_base_search_mapping_back(self, expr);
  }
  else if (self->is_leaf(self, expr)) retval = expr;
  else {
    /* check cache */
    retval = node_anonymizer_base_search_anon2orig(self, expr);

    if (NULL == retval) {
      /* not in cache */
      NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
      NodeMgr_ptr const nodemgr =
        NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

      retval =
        find_node(nodemgr, node_get_type(expr),
                  Nil != car(expr) ? nab_map_back_rec(self, car(expr)) : Nil,
                  Nil != cdr(expr) ? nab_map_back_rec(self, cdr(expr)) : Nil);

      /* update cache */
      node_anonymizer_base_insert_anon2orig(self, expr, retval);
    }
  }

  NAB_DEBUG_PRINT("%s OUTPUT\n%N", __func__, retval)

  return retval;
}

/*!
  \brief Print an entry of the map


*/

static int nab_print_map_entry(NodeAnonymizerBase_ptr self,
                               node_ptr orig,
                               MasterPrinter_ptr anon_print)
{
  node_ptr anonymous;
  int retval = 0;

  NAB_DEBUG_PRINT("%s INPUT\n%N", __func__, orig);

  anonymous = BiMap_get(self->map, orig);

  retval = MasterPrinter_print_node(anon_print, orig);
  MasterPrinter_print_string(anon_print, NODE_ANONYMIZER_DELIMITER_STR);
  retval = MasterPrinter_print_node(anon_print, anonymous);
  MasterPrinter_print_string(anon_print, "\n");

  NAB_DEBUG_PRINT("%s OUTPUT\n%N", __func__, anonymous);

  return retval;
}

/*!
  \brief



  \sa NodeAnonymizerBase_read_map_from_stream
*/

static node_ptr nab_convert_id_str_to_node(NodeAnonymizerBase_ptr self,
                                           char** id)
{
  NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  NodeMgr_ptr const nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr retval = NULL;
  node_ptr nleft = NULL;
  node_ptr nright = NULL;
  char* str = NULL;
  size_t span;

  NAB_DEBUG_PRINT("%s INPUT\n%s", __func__, *id);

  switch(**id) {
  case NODE_ANONYMIZER_DOT_CHAR:
    {
      *id = *id + 1;

      nleft = nab_convert_id_str_to_node(self, id);
      nright = nab_convert_id_str_to_node(self, id);

      retval = find_node(nodemgr, DOT, nleft, nright);
    }
    break;

  case NODE_ANONYMIZER_SEPARATOR_CHAR:
    {
      *id = *id + 1;
      retval = NULL;
    }
    break;

  default:
    {
      /* It is an identifier */
      span = strcspn(*id, NODE_ANONYMIZER_DOT_STR""NODE_ANONYMIZER_SEPARATOR_STR);
      str = ALLOC(char, span + 1);
      str[0] = '\0';
      (void)strncat(str, *id, span);

      retval = sym_intern(env, str);

      *id = *id + span;
      if (**id == NODE_ANONYMIZER_SEPARATOR_CHAR) *id = *id + 1;

      FREE(str);
    }
  }

  NAB_DEBUG_PRINT("%s OUTPUT\n%N", __func__, retval);

  return retval;
}

/**AutomaticEnd***************************************************************/
