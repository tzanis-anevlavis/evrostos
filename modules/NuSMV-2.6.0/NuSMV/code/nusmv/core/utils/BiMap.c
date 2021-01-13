/* ---------------------------------------------------------------------------


  %COPYRIGHT%

-----------------------------------------------------------------------------*/

/*!
  \author Mirco Giacobbe
  \brief Implementation of class 'BiMap'

  \todo: Missing description

*/


#include "nusmv/core/utils/BiMap.h"
#include "nusmv/core/utils/assoc.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct BiMap_TAG
{
  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */

  hash_ptr d2c; /* domain to codomain hash map */
  hash_ptr c2d; /* codomain to domain hash map */
  NodeList_ptr d_list; /* domain as list for fast iteration */
  NodeList_ptr c_list; /* codomain as list for fast iteration */
  node_ptr d_cache;
  node_ptr c_cache;
} BiMap;

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void bi_map_init(BiMap_ptr self);
static void bi_map_deinit(BiMap_ptr self);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

BiMap_ptr BiMap_create(void)
{
  BiMap_ptr self = ALLOC(BiMap, 1);
  BI_MAP_CHECK_INSTANCE(self);

  bi_map_init(self);
  return self;
}

void BiMap_destroy(BiMap_ptr self)
{
  BI_MAP_CHECK_INSTANCE(self);

  bi_map_deinit(self);
  FREE(self);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void BiMap_put(BiMap_ptr self, 
               node_ptr domain_element, 
               node_ptr codomain_element)
{
  BI_MAP_CHECK_INSTANCE(self);
  nusmv_assert(find_assoc(self->d2c, domain_element) == Nil);
  nusmv_assert(find_assoc(self->c2d, codomain_element) == Nil);

  insert_assoc(self->d2c, domain_element, codomain_element);
  insert_assoc(self->c2d, codomain_element, domain_element);

  NodeList_append(self->d_list, domain_element);
  NodeList_append(self->c_list, codomain_element);

  self->d_cache = domain_element;
  self->c_cache = codomain_element;
}

node_ptr BiMap_get(BiMap_ptr self, node_ptr domain_element)
{
  BI_MAP_CHECK_INSTANCE(self);
  nusmv_assert(find_assoc(self->d2c, domain_element) != Nil);

  if (domain_element != self->d_cache){
    self->d_cache = domain_element;
    self->c_cache = find_assoc(self->d2c, domain_element);
  }
  return self->c_cache;
}

node_ptr BiMap_inverse_get(BiMap_ptr self, node_ptr codomain_element)
{
  BI_MAP_CHECK_INSTANCE(self);
  nusmv_assert(find_assoc(self->c2d, codomain_element) != Nil);

  if (codomain_element != self->c_cache) {
    self->c_cache = codomain_element;
    self->d_cache = find_assoc(self->c2d, codomain_element);
  }
  return self->d_cache;
}

boolean BiMap_domain_contains(BiMap_ptr self, node_ptr domain_element)
{
  BI_MAP_CHECK_INSTANCE(self);

  if (domain_element != self->d_cache) {
    self->d_cache = domain_element;
    self->c_cache = find_assoc(self->d2c, self->d_cache);
  }
  return self->c_cache != Nil;
}

boolean BiMap_codomain_contains(BiMap_ptr self, node_ptr codomain_element)
{
  BI_MAP_CHECK_INSTANCE(self);

  if (codomain_element != self->c_cache) {
    self->c_cache = codomain_element;
    self->d_cache = find_assoc(self->c2d, self->c_cache);
  }
  return self->d_cache != Nil;
}

unsigned BiMap_size(BiMap_ptr self)
{
  BI_MAP_CHECK_INSTANCE(self);
  nusmv_assert(assoc_get_size(self->c2d) == assoc_get_size(self->d2c));
  return (unsigned) assoc_get_size(self->c2d);
}

boolean BiMap_is_empty(BiMap_ptr self)
{
  BI_MAP_CHECK_INSTANCE(self);
  return BiMap_size(self) == 0;
}

void BiMap_clear(BiMap_ptr self)
{
  BI_MAP_CHECK_INSTANCE(self);

  bi_map_deinit(self);
  bi_map_init(self);
}

void BiMap_gen_iter(BiMap_ptr self, BiMapIter* iter)
{
#ifndef BI_MAP_FAST_ITERATOR
  iter->gen.table = self->d2c;
  iter->gen.entry = NIL(st_table_entry);
  iter->gen.index = 0;
  iter->end = st_gen(&(iter->gen), &(iter->key_p), &(iter->value_p));
#else
  iter->d_iter = NodeList_get_first_iter(self->d_list);
  iter->c_iter = NodeList_get_first_iter(self->c_list);
#endif
}

boolean BiMap_iter_is_end(BiMap_ptr self, BiMapIter* iter)
{
#ifndef BI_MAP_FAST_ITERATOR
  return iter->end == 0;
#else
  return ListIter_is_end(iter->d_iter);
#endif
}

void BiMap_iter_next(BiMap_ptr self, BiMapIter* iter)
{
#ifndef BI_MAP_FAST_ITERATOR
  iter->end = st_gen(&(iter->gen), &(iter->key_p), &(iter->value_p));
#else
  iter->d_iter = ListIter_get_next(iter->d_iter);
  iter->c_iter = ListIter_get_next(iter->c_iter);
#endif
}

node_ptr BiMap_iter_get_domain_element(BiMap_ptr self, BiMapIter* iter)
{
#ifndef BI_MAP_FAST_ITERATOR
  return NODE_PTR(iter->key_p);
#else
  return NodeList_get_elem_at(self->d_list, iter->d_iter);
#endif
}

node_ptr BiMap_iter_get_codomain_element(BiMap_ptr self, BiMapIter* iter)
{
#ifndef BI_MAP_FAST_ITERATOR
  return NODE_PTR(iter->value_p);
#else
  return NodeList_get_elem_at(self->c_list, iter->c_iter);
#endif
}

NodeList_ptr BiMap_domain(BiMap_ptr self)
{
  return self->d_list;
}

NodeList_ptr BiMap_codomain(BiMap_ptr self)
{
  return self->c_list;
}

hash_ptr BiMap_map(BiMap_ptr self)
{
  return self->d2c;
}

hash_ptr BiMap_inverse_map(BiMap_ptr self)
{
  return self->c2d;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The BiMap class private initializer

  The BiMap class private initializer

  \sa BiMap_create
*/
static void bi_map_init(BiMap_ptr self)
{
  self->d2c = new_assoc();
  self->c2d = new_assoc();

  self->d_list = NodeList_create();
  self->c_list = NodeList_create();

  self->d_cache = Nil;
  self->c_cache = Nil;
}

/*!
  \brief The BiMap class private deinitializer

  The BiMap class private deinitializer

  \sa BiMap_destroy
*/
static void bi_map_deinit(BiMap_ptr self)
{
  free_assoc(self->d2c);
  free_assoc(self->c2d);

  NodeList_destroy(self->d_list);
  NodeList_destroy(self->c_list);
}
