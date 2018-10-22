/* ---------------------------------------------------------------------------


  This file is part of the ``enc.base'' package of NuSMV version 2.
  Copyright (C) 2004 by FBK-irst.

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
  \brief Implementaion of pure base class 'BaseEnc'

  \todo: Missing description

*/


#include "nusmv/core/enc/base/BaseEnc.h"
#include "nusmv/core/enc/base/BaseEnc_private.h"

#include "nusmv/core/utils/utils.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'BaseEnc_private.h' for class 'BaseEnc' definition. */

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

static void base_enc_finalize(Object_ptr object, void* dummy);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

VIRTUAL void BaseEnc_destroy(BaseEnc_ptr self)
{
  BASE_ENC_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

boolean BaseEnc_layer_occurs(const BaseEnc_ptr self, const char* layer_name)
{
  SymbLayer_ptr layer;

  BASE_ENC_CHECK_INSTANCE(self);

  layer = SymbTable_get_layer(self->symb_table, layer_name);
  return NodeList_belongs_to(self->committed_layers, (node_ptr) layer);
}

NodeList_ptr BaseEnc_get_committed_layers(const BaseEnc_ptr self)
{
  BASE_ENC_CHECK_INSTANCE(self);
  return self->committed_layers;
}

const array_t* BaseEnc_get_committed_layer_names(BaseEnc_ptr self)
{
  BASE_ENC_CHECK_INSTANCE(self);

  /* allocates the list if needed */
  if (self->layer_names == (array_t*) NULL) {
    ListIter_ptr iter;

    self->layer_names = array_alloc(const char*,
                   NodeList_get_length(self->committed_layers));
    nusmv_assert(self->layer_names != (array_t*) NULL);

    for (iter = NodeList_get_first_iter(self->committed_layers);
         !ListIter_is_end(iter);
         iter = ListIter_get_next(iter)) {
      SymbLayer_ptr layer =
        SYMB_LAYER(NodeList_get_elem_at(self->committed_layers, iter));
      array_insert_last(const char*, (self->layer_names),
                        SymbLayer_get_name(layer));
    }
  }

  return self->layer_names;
}

int BaseEnc_commit_layers(BaseEnc_ptr self,
                          const array_t* layer_names)
{
  int res = 0;

  const char* layer_name;
  int idx;
  arrayForEachItem(const char*, layer_names, idx, layer_name) {
    if (!BaseEnc_layer_occurs(self, layer_name)) {
      BaseEnc_commit_layer(self, layer_name);
      ++res;
    }
  }

  return res;
}

int BaseEnc_remove_layers(BaseEnc_ptr self,
                          const array_t* layer_names)
{
  int res = 0;

  const char* layer_name;
  int idx;
  arrayForEachItem(const char*, layer_names, idx, layer_name) {
    if (BaseEnc_layer_occurs(self, layer_name)) {
      BaseEnc_remove_layer(self, layer_name);
      ++res;
    }
  }

  return res;
}

SymbTable_ptr BaseEnc_get_symb_table(const BaseEnc_ptr self)
{
  BASE_ENC_CHECK_INSTANCE(self);
  return self->symb_table;
}

TypeChecker_ptr BaseEnc_get_type_checker(const BaseEnc_ptr self)
{
  BASE_ENC_CHECK_INSTANCE(self);
  return SymbTable_get_type_checker(self->symb_table);
}

VIRTUAL void
BaseEnc_commit_layer(BaseEnc_ptr self, const char* layer_name)
{
  BASE_ENC_CHECK_INSTANCE(self);
  self->commit_layer(self, layer_name);
}

VIRTUAL void
BaseEnc_remove_layer(BaseEnc_ptr self, const char* layer_name)
{
  BASE_ENC_CHECK_INSTANCE(self);
  self->remove_layer(self, layer_name);
}



/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief The BaseEnc class private initializer

  The BaseEnc class private initializer

  \sa BaseEnc_create
*/

void base_enc_init(BaseEnc_ptr self, SymbTable_ptr symb_table)
{
  /* base class initialization */
  env_object_init(ENV_OBJECT(self), EnvObject_get_environment(ENV_OBJECT(symb_table)));

  /* members initialization */
  self->symb_table = symb_table;
  self->committed_layers = NodeList_create();
  self->layer_names = (array_t*) NULL;

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = base_enc_finalize;

  /* these must be inherited, and called by inherited methods: */
  OVERRIDE(BaseEnc, commit_layer) = base_enc_commit_layer;
  OVERRIDE(BaseEnc, remove_layer) = base_enc_remove_layer;
}


/*!
  \brief The BaseEnc class private deinitializer

  The BaseEnc class private deinitializer

  \sa BaseEnc_destroy
*/

void base_enc_deinit(BaseEnc_ptr self)
{
  ListIter_ptr iter;

  /* members deinitialization */

  /* unlock all layers that are still committed in */
  iter = NodeList_get_first_iter(self->committed_layers);
  while (! ListIter_is_end(iter)) {
    SymbLayer_ptr lyr;
    lyr = SYMB_LAYER(NodeList_get_elem_at(self->committed_layers, iter));
    SymbLayer_removed_from_enc(lyr);
    iter = ListIter_get_next(iter);
  }
  NodeList_destroy(self->committed_layers);

  if (self->layer_names != (array_t*) NULL) {
    array_free(self->layer_names);
    self->layer_names = (array_t*) NULL;
  }

  /* base class deinitialization */
  env_object_deinit(ENV_OBJECT(self));
}


/*!
  \brief Register and store a new layer to be committed

  This method must always be called by derived classes
*/

void base_enc_commit_layer(BaseEnc_ptr self, const char* layer_name)
{
  SymbLayer_ptr layer;

  /* not already added: */
  nusmv_assert(!BaseEnc_layer_occurs(self, layer_name));

  layer = SymbTable_get_layer(self->symb_table, layer_name);

  /* register as user to the layer, and add the layer at the right level */
  SymbLayer_committed_to_enc(layer);

  {
    ListIter_ptr iter = NodeList_get_first_iter(self->committed_layers);
    while (!ListIter_is_end(iter)) {
      if (SymbLayer_must_insert_before(layer,
             SYMB_LAYER(NodeList_get_elem_at(self->committed_layers, iter)))) {
        NodeList_insert_before(self->committed_layers, iter, (node_ptr) layer);
        break;
      }

      iter = ListIter_get_next(iter);
    }

    if (ListIter_is_end(iter)) {
      /* reached the end and not inserted: append */
      NodeList_append(self->committed_layers, (node_ptr) layer);
    }
  }

  /* frees the layer names (will be possibly re-calculated) */
  if (self->layer_names != (array_t*) NULL) {
    array_free(self->layer_names);
    self->layer_names = (array_t*) NULL;
  }
}


/*!
  \brief Unregister and remove from the list of layers
  the given layer.

  This method must always be called by derived methods,
  after they have done their work.

  WARNING: If the layer has been
  renamed after having been committed, it is the *new* name (the name
  the layer has when it is being removed) that must be used, and *not*
  the name that had been used when commiting it.
*/

void base_enc_remove_layer(BaseEnc_ptr self, const char* layer_name)
{
  SymbLayer_ptr layer;
  ListIter_ptr iter;

  /* must be belonging to the layers list: */
  nusmv_assert(BaseEnc_layer_occurs(self, layer_name));

  layer = SymbTable_get_layer(self->symb_table, layer_name);

  /* search it, then unregister and remove it */
  iter = NodeList_get_first_iter(self->committed_layers);
  while (!ListIter_is_end(iter)) {
    if (layer == SYMB_LAYER(NodeList_get_elem_at(self->committed_layers,
                                                 iter))) {
      NodeList_remove_elem_at(self->committed_layers, iter);
      SymbLayer_removed_from_enc(layer);
      break;
    }
    iter = ListIter_get_next(iter);
  }

  /* frees the layer names (will be possibly re-calculated) */
  if (self->layer_names != (array_t*) NULL) {
    array_free(self->layer_names);
    self->layer_names = (array_t*) NULL;
  }

}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The BaseEnc class virtual finalizer

  Called by the class destructor
*/
static void base_enc_finalize(Object_ptr object, void* dummy)
{
  BaseEnc_ptr self = BASE_ENC(object);

  base_enc_deinit(self);
  FREE(self);
}



/**AutomaticEnd***************************************************************/
