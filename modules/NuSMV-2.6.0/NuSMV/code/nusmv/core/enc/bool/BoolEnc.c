/* ---------------------------------------------------------------------------


  This file is part of the ``enc.bool'' package of NuSMV version 2.
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
  \brief Implementaion of class 'BoolEnc'

  \todo: Missing description

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/enc/bool/BoolEnc.h"
#include "nusmv/core/enc/bool/BoolEnc_private.h"

#include "nusmv/core/enc/encInt.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/set/set.h"

#include "nusmv/core/utils/WordNumberMgr.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/error.h"

#include "nusmv/core/utils/ustring.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BOOL_ENC_DEFAULT_LAYER_SUFFIX \
  "_bool"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_BOOLEAN_TYPE "bool_enc_boolean_type"


/* Initialized the first time an instance of class BoolEnc is created
  within an environment (see SymbTable constructor), and deinitialized
  when the last one is destroyed */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_BOOL_ENC_COUNT "bool_enc_counter"

/* Contains information about all layers that are created and used by
   any BoolEnc. This is used to handle deallocation of layers when the
   last owning encoder is gone. The hash associates layer instances
   with a counter representing the number of users of that layer. When
   the last user is being destroyed, or when the layer is begin
   removed from the last user, the layer is removed from the symbol
   table which it is contained into. When the number of bool enc
   instances goes to 0, the hash is freed.  */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define ENV_BOOL_ENC_OWNED_LAYERS   "bool_enc_owned_layers"

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'BoolEnc_private.h' for class 'BoolEnc' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/
/* warning: the first of these set to 1 will be taken (see issue 2549) */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BOOL_ENCODING_HIGHER_TO_LOWER_BALANCED  1 /* this is best in average */

/* these are kept for record and testing */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BOOL_ENCODING_LOWER_TO_HIGHER_BALANCED  0 /* the old default */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define BOOL_ENCODING_HIGHER_TO_LOWER_INCREMENTAL  0 /* this is better
                                                        in some cases */

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void bool_enc_finalize(Object_ptr object, void* dummy);

static void
bool_enc_encode_var(BoolEnc_ptr self, node_ptr var,
                    SymbLayer_ptr src_layer, SymbLayer_ptr dest_layer);

static node_ptr
bool_enc_encode_scalar_var(BoolEnc_ptr self, node_ptr name, int suffix,
                           node_ptr values,
                           SymbLayer_ptr src_layer,
                           SymbLayer_ptr dest_layer);

static void
bool_enc_set_var_encoding(BoolEnc_ptr self, node_ptr name,
                          node_ptr enc);

static node_ptr
bool_enc_get_var_encoding(const BoolEnc_ptr self, node_ptr name);

static void
bool_enc_traverse_encoding(const BoolEnc_ptr self,
                           node_ptr enc, NodeList_ptr list);

static node_ptr
bool_enc_get_var_mask_recur(const BoolEnc_ptr self,
                            node_ptr enc,
                            NodeList_ptr cube,
                            ListIter_ptr cube_iter);

static node_ptr
bool_enc_compute_set_encoding(BoolEnc_ptr self, node_ptr set,
                              node_ptr bit_prefix, int bit_suffix,
                              Set_t* out_bits, boolean top);

static boolean bool_enc_is_boolean_range(const ExprMgr_ptr exprs, node_ptr values);

static node_ptr bool_enc_get_boolean_type(const BoolEnc_ptr self);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

BoolEnc_ptr BoolEnc_create(SymbTable_ptr symb_table)
{
  BoolEnc_ptr self = ALLOC(BoolEnc, 1);
  BOOL_ENC_CHECK_INSTANCE(self);

  bool_enc_init(self, symb_table);
  return self;
}

VIRTUAL void BoolEnc_destroy(BoolEnc_ptr self)
{
  BOOL_ENC_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

boolean BoolEnc_is_var_bit(const BoolEnc_ptr self, node_ptr name)
{
  BOOL_ENC_CHECK_INSTANCE(self);
  return (node_get_type(name) == BIT);
}

boolean BoolEnc_is_var_scalar(const BoolEnc_ptr self, node_ptr name)
{
  node_ptr enc;

  BOOL_ENC_CHECK_INSTANCE(self);

  enc = BoolEnc_get_var_encoding(self, name);
  return enc != bool_enc_get_boolean_type(self);
}

node_ptr BoolEnc_get_scalar_var_from_bit(const BoolEnc_ptr self, node_ptr name)
{
  BOOL_ENC_CHECK_INSTANCE(self);
  nusmv_assert(BoolEnc_is_var_bit(self, name));

  return car(name);
}

node_ptr BoolEnc_make_var_bit(const BoolEnc_ptr self, node_ptr name, int index)
{
  BOOL_ENC_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
    return find_node(nodemgr, BIT, name, NODE_FROM_INT(index));
  }
}

int BoolEnc_get_index_from_bit(const BoolEnc_ptr self, node_ptr name)
{
  BOOL_ENC_CHECK_INSTANCE(self);
  nusmv_assert(BoolEnc_is_var_bit(self, name));
  return NODE_TO_INT(cdr(name));
}

NodeList_ptr BoolEnc_get_var_bits(const BoolEnc_ptr self, node_ptr name)
{
  NodeList_ptr res;
  node_ptr enc;

  BOOL_ENC_CHECK_INSTANCE(self);

  enc = BoolEnc_get_var_encoding(self, name);
  res = NodeList_create();
  bool_enc_traverse_encoding(self, enc, res);
  return res;
}

node_ptr BoolEnc_get_var_encoding(const BoolEnc_ptr self, node_ptr name)
{
  node_ptr enc;

  BOOL_ENC_CHECK_INSTANCE(self);

  enc = bool_enc_get_var_encoding(self, name);
  nusmv_assert(enc != Nil); /* must be previoulsy encoded */
  return enc;
}

node_ptr BoolEnc_get_values_bool_encoding(const BoolEnc_ptr self,
                                          node_ptr values,
                                          Set_t* bits)
{
  node_ptr var_name;

  BOOL_ENC_CHECK_INSTANCE(self);

  var_name = SymbTable_get_determinization_var_name(BASE_ENC(self)->symb_table);
  return bool_enc_compute_set_encoding(self, values, var_name, 0, bits, true);
}

node_ptr BoolEnc_get_var_mask(const BoolEnc_ptr self, node_ptr name)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  node_ptr enc;
  NodeList_ptr cube;
  ListIter_ptr iter;
  node_ptr res;
  SymbType_ptr var_type;
  SymbTable_ptr st;

  BOOL_ENC_CHECK_INSTANCE(self);

  st = BaseEnc_get_symb_table(BASE_ENC(self));
  var_type = SymbTable_get_var_type(st, name);

  /* Computing the mask is meaningful only for enumerative non
     boolean variables: */
  if (SymbType_is_enum(var_type) && !SymbType_is_boolean(var_type)) {
    /* check memoized mask */
    res = find_assoc(self->var2mask, name);
    if (Nil != res) return res; /* hit */

    enc = BoolEnc_get_var_encoding(self, name);
    nusmv_assert(Nil != enc); /* must be previoulsy encoded */
    cube = BoolEnc_get_var_bits(self, name);

    NODE_LIST_CHECK_INSTANCE(cube);

    iter = NodeList_get_first_iter(cube);

    res = bool_enc_get_var_mask_recur(self, enc, cube, iter);

    NodeList_destroy(cube);

    /* memoizes the result */
    insert_assoc(self->var2mask, name, res);
  }
  else {
    res = ExprMgr_true(exprs);
  }

  return res;
}

node_ptr BoolEnc_get_value_from_var_bits(const BoolEnc_ptr self,
                                         const BitValues_ptr bit_values)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr var = BitValues_get_scalar_var(bit_values);
  SymbType_ptr var_type = SymbTable_get_var_type(
                           BaseEnc_get_symb_table(BASE_ENC(self)), var);
  node_ptr enc = BoolEnc_get_var_encoding(self, var);

  if (SymbType_is_enum(var_type)) {
    while (true) {
      if (enc == Nil) return Nil;

      switch (node_get_type(enc)) {
      case IFTHENELSE: {
        int bit_index = BoolEnc_get_index_from_bit(self, car(car(enc)));
        switch (BitValues_get(bit_values, bit_index)) {
        case BIT_VALUE_FALSE: enc = cdr(enc); break;
        case BIT_VALUE_TRUE:
        case BIT_VALUE_DONTCARE: enc = cdr(car(enc)); break;
        default: error_unreachable_code(); /* no other known values */
        }
        break;
      }

      case DOT: case ATOM: case NUMBER: case TRUEEXP: case FALSEEXP:
        return enc; /* found the value */

      default: error_unreachable_code(); /* no other known cases */
      }
    } /* traversing loop */
  } /* a scalar variable */

  if (SymbType_is_word(var_type)) {
    /* re-constructs word number value from values of bits */
    const WordNumberMgr_ptr wmgr =
      WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

    WordNumber_ptr one;
    WordNumber_ptr value;
    node_ptr iter;
    nusmv_assert(node_get_type(enc) == UNSIGNED_WORD ||
                 node_get_type(enc) == SIGNED_WORD);

    one = WordNumberMgr_integer_to_word_number(wmgr, 1ULL,
                                    SymbType_get_word_width(var_type));
    value = WordNumberMgr_integer_to_word_number(wmgr, 0ULL,
                                    SymbType_get_word_width(var_type));

    for (iter = car(enc); iter != Nil; iter = cdr(iter)) {
      node_ptr bit;
      int bit_index;
      nusmv_assert(node_get_type(iter) == CONS);
      bit = car(iter);
      bit_index = BoolEnc_get_index_from_bit(self, bit);

      value = WordNumberMgr_left_shift(wmgr, value, 1);

      switch (BitValues_get(bit_values, bit_index)) {
      case BIT_VALUE_FALSE:
        break;

      case BIT_VALUE_TRUE:
      case BIT_VALUE_DONTCARE:
        value = WordNumberMgr_or(wmgr, value, one);
        break;

      default: error_unreachable_code(); /* no other known values */
      }
    }

    /* representation of vars is always UNSIGNED (as on 2010.01.30).
       but constants can be signed and unsigned.
       check the type for var for sign instead */
    if (SymbType_is_unsigned_word(var_type)) {
      return find_node(nodemgr, NUMBER_UNSIGNED_WORD, (node_ptr) value, Nil);
    }
    else {
      return find_node(nodemgr, NUMBER_SIGNED_WORD, (node_ptr) value, Nil);
    }
  } /* a word variable */

  error_unreachable_code(); /* no other known var types */
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief The BoolEnc class private initializer

  The BoolEnc class private initializer

  \sa BoolEnc_create
*/

void bool_enc_init(BoolEnc_ptr self, SymbTable_ptr symb_table)
{
  int counter = 0;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));

  /* base class initialization */
  base_enc_init(BASE_ENC(self), symb_table);

  /* static members */
  if (NuSMVEnv_has_value(env, ENV_BOOL_ENC_COUNT)) {
    counter = NODE_TO_INT(NuSMVEnv_get_value(env, ENV_BOOL_ENC_COUNT));
  }
  NuSMVEnv_set_or_replace_value(env, ENV_BOOL_ENC_COUNT, NODE_FROM_INT(counter + 1));

  if (counter == 0) {
    hash_ptr bool_enc_owned_layers = new_assoc();
    NuSMVEnv_set_value(env, ENV_BOOL_ENC_OWNED_LAYERS, bool_enc_owned_layers);
  }

  /* members initialization */
  self->var2enc = new_assoc();
  self->var2mask = new_assoc();

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = bool_enc_finalize;

  /* inherited by BaseEnc: */
  OVERRIDE(BaseEnc, commit_layer) = bool_enc_commit_layer;
  OVERRIDE(BaseEnc, remove_layer) = bool_enc_remove_layer;
}


/*!
  \brief The BoolEnc class private deinitializer

  The BoolEnc class private deinitializer

  \sa BoolEnc_destroy
*/

void bool_enc_deinit(BoolEnc_ptr self)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  hash_ptr bool_enc_owned_layers = (hash_ptr)NuSMVEnv_get_value(env, ENV_BOOL_ENC_OWNED_LAYERS);
  int bool_enc_instances = 0;

  /* members deinitialization */
  free_assoc(self->var2mask);
  free_assoc(self->var2enc);

  { /* destroys all owned layers if self is the last user: */
    NodeList_ptr layers = BaseEnc_get_committed_layers(BASE_ENC(self));
    ListIter_ptr iter = NodeList_get_first_iter(layers);
    while (!ListIter_is_end(iter)) {
      SymbLayer_ptr lyr;
      int count;
      lyr = SYMB_LAYER(NodeList_get_elem_at(layers, iter));
      count = PTR_TO_INT(find_assoc(bool_enc_owned_layers, (node_ptr) lyr));
      nusmv_assert(count >= 0);

      if (count == 1) { /* the layer has to be destroyed */
        ListIter_ptr niter = ListIter_get_next(iter);

        SymbLayer_removed_from_enc(lyr);

        /* SymbTable_remove_layer also performs the destruction of lyr */
        SymbTable_remove_layer(BASE_ENC(self)->symb_table, lyr);

        insert_assoc(bool_enc_owned_layers, (node_ptr) lyr,
                     PTR_FROM_INT(node_ptr, 0));
        NodeList_remove_elem_at(layers, iter);

        iter = niter;
        continue;
      }

      else if (count > 1) {
        insert_assoc(bool_enc_owned_layers, (node_ptr) lyr,
                     PTR_FROM_INT(node_ptr, count-1));
      }

      iter = ListIter_get_next(iter);
    }
  }

  /* base class deinitialization */
  base_enc_deinit(BASE_ENC(self));

  bool_enc_instances = NODE_TO_INT(NuSMVEnv_get_value(env, ENV_BOOL_ENC_COUNT));

  if (bool_enc_instances == 1) { /* This is the last instance in the env */
    hash_ptr bool_enc_owned_layers =
      (hash_ptr)NuSMVEnv_remove_value(env, ENV_BOOL_ENC_OWNED_LAYERS);

    NuSMVEnv_remove_value(env, ENV_BOOL_ENC_COUNT);

    free_assoc(bool_enc_owned_layers);

    /* also remove the boolean type, since it is find-noded, otherwise if we
       are resetting we will use a invalid node */
    if (NuSMVEnv_has_value(env, ENV_BOOLEAN_TYPE)) {
      (void)NuSMVEnv_remove_value(env, ENV_BOOLEAN_TYPE);
    }
  }
  else {
    NuSMVEnv_set_or_replace_value(env, ENV_BOOL_ENC_COUNT,
                                  NODE_FROM_INT(bool_enc_instances - 1));
  }
}



/*!
  \brief Encodes all variables within the given layer

  A new layer will be constructed if there is not yet
  any.  The new layer will be called ${layer_name}_bool and will be
  added to the symbol table that self uses. The new layer will be
  locked by self either until the layer is was originally created from
  is released or until self is destroyed. Given a committed layer, it
  is always possible to obtain the corresponding created boolean layer
  by calling BoolEnc_scalar_layer_to_bool_layer. 

  \se A new layer will be created if not already existing

  \sa bool_enc_remove_layer
*/

VIRTUAL void bool_enc_commit_layer(BaseEnc_ptr base_enc,
                                   const char* layer_name)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(base_enc));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  hash_ptr bool_enc_owned_layers =
    (hash_ptr)NuSMVEnv_get_value(env, ENV_BOOL_ENC_OWNED_LAYERS);

  BoolEnc_ptr self;
  SymbLayer_ptr src_layer, dest_layer;
  const char* dest_layer_name;
  SymbLayerIter iter;

  self = BOOL_ENC(base_enc);

  if (opt_verbose_level_ge(opts, 3)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "BoolEnc committing layer '%s'\n",
            layer_name);
  }

  /* Calls the base method to add this layer */
  base_enc_commit_layer(base_enc, layer_name);

  src_layer = SymbTable_get_layer(BASE_ENC(self)->symb_table, layer_name);

  /* queries for the corresponding boolean layer, to see if it is
     already up and running: */
  dest_layer_name = BoolEnc_scalar_layer_to_bool_layer(self, layer_name);
  dest_layer = SymbTable_get_layer(BASE_ENC(self)->symb_table, dest_layer_name);

  if (dest_layer == SYMB_LAYER(NULL)) {
    /* does not exist yet, creates one. It will be created with the same policy
       the layer it derives from has */
    dest_layer = SymbTable_create_layer(BASE_ENC(self)->symb_table,
                                        dest_layer_name,
                                        SymbLayer_get_insert_policy(src_layer));

    /* encoders become the new layer owners */
    nusmv_assert(find_assoc(bool_enc_owned_layers, (node_ptr) dest_layer) ==
                 (node_ptr) NULL);
    insert_assoc(bool_enc_owned_layers, (node_ptr) dest_layer,
                 PTR_FROM_INT(node_ptr, 1));
  }
  else { /* if it is a layer whose owner is a another bool enc,
            increments the counter of users */
    int count = PTR_TO_INT(
                 find_assoc(bool_enc_owned_layers, (node_ptr) dest_layer));
    nusmv_assert(count >= 0);
    if (count > 0) {
      insert_assoc(bool_enc_owned_layers, (node_ptr) dest_layer,
                   PTR_FROM_INT(node_ptr, count+1));
    }
  }

  /* becomes an user of the bool layer: */
  base_enc_commit_layer(base_enc, dest_layer_name);

  /* now encodes all variables within the given layer, and puts them into the
     boolean layer */
  SYMB_LAYER_FOREACH(src_layer, iter, STT_VAR) {
    node_ptr var = SymbLayer_iter_get_symbol(src_layer, &iter);

    if (opt_verbose_level_gt(opts, 4)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_nlog(logger, wffprint, "BoolEnc: encoding variable '%N'...\n", var);
    }

    bool_enc_encode_var(self, var, src_layer, dest_layer);
  }
}


/*!
  \brief Removes the encoding of all variables occurring within
  the given layer, and those that had been created within the corresponding
  boolean layer during the boolean encoding. Then releases both the layers,
  and removes the boolean layer from the symbol table.

    WARNING: If the layer has been
  renamed after having been committed, it is the *new* name (the name
  the layer has when it is being removed) that must be used, and *not*
  the name that had been used when commiting it.

  \sa bool_enc_commit_layer
*/

VIRTUAL void bool_enc_remove_layer(BaseEnc_ptr base_enc,
                                   const char* layer_name)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(base_enc));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  hash_ptr bool_enc_owned_layers =
    (hash_ptr)NuSMVEnv_get_value(env, ENV_BOOL_ENC_OWNED_LAYERS);

  BoolEnc_ptr self;
  SymbLayer_ptr layer, bool_layer;
  SymbLayerIter iter;
  const char* bool_layer_name;
  SymbType_ptr type;

  self = BOOL_ENC(base_enc);

  bool_layer_name = BoolEnc_scalar_layer_to_bool_layer(self, layer_name);
  layer = SymbTable_get_layer(BASE_ENC(self)->symb_table, layer_name);
  bool_layer = SymbTable_get_layer(BASE_ENC(self)->symb_table, bool_layer_name);

  /* removes all encodings from vars within layer */
  SYMB_LAYER_FOREACH(layer, iter, STT_VAR) {
    node_ptr var = SymbLayer_iter_get_symbol(layer, &iter);

    type = SymbTable_get_var_type(BASE_ENC(self)->symb_table, var);

    /*  Only ENUM and WORD types are implemented at the moment */
    switch (SymbType_get_tag(type)) {
    case SYMB_TYPE_BOOLEAN:
    case SYMB_TYPE_ENUM: /* ENUM type */
    case SYMB_TYPE_UNSIGNED_WORD: /* Word type */
    case SYMB_TYPE_SIGNED_WORD: /* Word type */
      if (opt_verbose_level_gt(opts, 4)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_nlog(logger, wffprint, "BoolEnc: removing encoding of variable '%N'...\n", var);
      }

      if (bool_enc_get_var_encoding(self, var) != Nil) {
        NodeList_ptr bits;
        ListIter_ptr iter;

        /* Gets rid of all bits that were created by the encoding process
           of this variable */
        bits = BoolEnc_get_var_bits(self, var);
        iter = NodeList_get_first_iter(bits);
        while (!ListIter_is_end(iter)) {
          node_ptr bit;
          bit = NodeList_get_elem_at(bits, iter);
          if (bool_enc_get_var_encoding(self, bit) != Nil) {
            bool_enc_set_var_encoding(self, bit, Nil);
          }
          iter = ListIter_get_next(iter);
        }
        NodeList_destroy(bits);

        /* gets rid of the var's encoding and mask as well */
        bool_enc_set_var_encoding(self, var, Nil);
        if (SymbType_is_enum(type) && !SymbType_is_boolean(type)) {
          insert_assoc(self->var2mask, var, Nil);
        }
      }
      break;
    case SYMB_TYPE_WORDARRAY: /* WordArray type */
      StreamMgr_print_error(streams,  "Unable to booleanize WordArrays.\n");
      nusmv_assert((false));
      break;
    case SYMB_TYPE_INTEGER:
    case SYMB_TYPE_REAL:
    case SYMB_TYPE_CONTINUOUS:
    case SYMB_TYPE_INTARRAY:
      /* for cegar hybrid added these two cases to ignore encoding
         for reals and integers */
      break;

    default:
      error_unreachable_code();
    }
  } /* end of loops over vars */

  /* No need to clean the booleanizer cache, it is done by a symbol table
     trigger at each symbol removal */

  /* Calls the base method to get rid of both the source and boolean layer */
  base_enc_remove_layer(base_enc, layer_name);
  base_enc_remove_layer(base_enc, bool_layer_name);

  /* Removes the boolean layer from the symb table if this
     is the last user of it */
  {
    int count = PTR_TO_INT(
          find_assoc(bool_enc_owned_layers, (node_ptr) bool_layer));
    nusmv_assert(count >= 0);

    if (count != 0) {
      insert_assoc(bool_enc_owned_layers, (node_ptr) bool_layer,
                   PTR_FROM_INT(node_ptr, count-1));

      if (count == 1) {
        SymbTable_remove_layer(BASE_ENC(self)->symb_table, bool_layer);
      }
    }
  }
}

const char* BoolEnc_scalar_layer_to_bool_layer(const BoolEnc_ptr self,
                                               const char* layer_name)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
   UStringMgr_ptr strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));

  char* bool_layer_name;
  string_ptr str;

  bool_layer_name = ALLOC(char, strlen(layer_name) +
                          strlen(BOOL_ENC_DEFAULT_LAYER_SUFFIX) + 1);
  nusmv_assert(bool_layer_name != (char*) NULL);
  strcpy(bool_layer_name, layer_name);
  strcat(bool_layer_name, BOOL_ENC_DEFAULT_LAYER_SUFFIX);

  /* the strings are used to avoid caring about memory */
  str =  UStringMgr_find_string(strings, bool_layer_name);
  FREE(bool_layer_name);

  return UStringMgr_get_string_text(str);
}

boolean BoolEnc_is_bool_layer(const char* layer_name)
{
  unsigned name_length;
  unsigned bool_length = strlen(BOOL_ENC_DEFAULT_LAYER_SUFFIX);

  nusmv_assert(NIL(char) != layer_name);
  name_length = strlen(layer_name);

  return (name_length > bool_length) && \
    (0 == strcmp(layer_name + name_length - bool_length,
                 BOOL_ENC_DEFAULT_LAYER_SUFFIX));
}



/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The BoolEnc class virtual finalizer

  Called by the class destructor
*/

static VIRTUAL void bool_enc_finalize(Object_ptr object, void* dummy)
{
  BoolEnc_ptr self = BOOL_ENC(object);

  bool_enc_deinit(self);
  FREE(self);
}

/*!
  \brief Encodes a single variable 

  If it is a scalar variable, its values are expanded and a
  set of bits (new boolean variables) will be created within the
  dest_layer. All leaves (constant values of the values) will be
  created within the src_layer, it they are not defined yet.
*/
static void
bool_enc_encode_var(BoolEnc_ptr self, node_ptr var,
                    SymbLayer_ptr src_layer, SymbLayer_ptr dest_layer)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr scalar_enc;
  SymbType_ptr type;

  type = SymbTable_get_var_type(BASE_ENC(self)->symb_table, var);

  /*  Only ENUM and WORD types are implemented at the moment */
  switch (SymbType_get_tag(type)) {
  case SYMB_TYPE_BOOLEAN:
    scalar_enc = bool_enc_get_boolean_type(self);
    break;
  case SYMB_TYPE_ENUM: { /* ENUM type */
    node_ptr values = SymbType_get_enum_type_values(type);
    nusmv_assert(Nil != values);
    scalar_enc = bool_enc_encode_scalar_var(self, var, 0, values,
                                            src_layer, dest_layer);
    break;
  }

  case SYMB_TYPE_UNSIGNED_WORD: /* unsigned and signed Word types */
  case SYMB_TYPE_SIGNED_WORD: {
    /* encode Word as an array of bits (ADD trees) */
    int width = SymbType_get_word_width(type);
    int suffix;
    node_ptr iter;
    node_ptr bits = Nil;
    /* higher bits are submitted first to make them higher in the BDD var order */
    for (suffix = width-1; suffix >= 0; --suffix) {
      node_ptr bitVar = BoolEnc_make_var_bit(self, var, suffix);
      /* declare a new boolean var -- bit of the Word */
      if (! SymbTable_is_symbol_var(BASE_ENC(self)->symb_table, bitVar)) {
        /* the type is created every time, because the "reset" frees them */
        SymbType_ptr type = SymbType_create(env, SYMB_TYPE_BOOLEAN, Nil);

        if (SymbTable_is_symbol_input_var(BASE_ENC(self)->symb_table, var)) {
          SymbLayer_declare_input_var(dest_layer, bitVar, type);
        }
        else if (SymbTable_is_symbol_state_var(BASE_ENC(self)->symb_table, var)) {
          SymbLayer_declare_state_var(dest_layer, bitVar, type);
        }
        else {
          SymbLayer_declare_frozen_var(dest_layer, bitVar, type);
        }

        bool_enc_set_var_encoding(self, bitVar, bool_enc_get_boolean_type(self));
      }
      /* create a list of bits (high bits now goes last) */
      bits = cons(nodemgr, bitVar, bits);
    } /* for */
    /* reverese the list (high bit will go first) and find_node it.
       NB: representation of a variable is given by node_ptr and
       (not array_t, for example), and find_node is required because
       the destructor does not free any memory
    */
    for (scalar_enc = Nil, iter = bits; iter != Nil; iter = cdr(iter)) {
      scalar_enc = find_node(nodemgr, CONS, car(iter), scalar_enc);
    }
    free_list(nodemgr, bits);
    /* wrap the list into a unsigned WORD node =>
     result is always *unsigned* because it is just representation
     and the type is of no importance */
    scalar_enc = find_node(nodemgr, UNSIGNED_WORD,
                           scalar_enc,
                           find_node(nodemgr, NUMBER, NODE_FROM_INT(width), Nil));
    break;
  }

    /* skip array types */
  case SYMB_TYPE_ARRAY:
    return;

    /* for cegar hybrid added these two cases to ignore encoding
       for reals and integers */
  case SYMB_TYPE_INTEGER:
  case SYMB_TYPE_REAL:
  case SYMB_TYPE_CONTINUOUS:
  case SYMB_TYPE_INTARRAY:
  case SYMB_TYPE_WORDARRAY:
    return;

  default: error_unreachable_code(); /* no other kinds of types are implemented */
  } /* switch */

  bool_enc_set_var_encoding(self, var, scalar_enc);
}

/*!
  \brief Encodes a scalar variable, by creating all boolean vars
  (bits) needed to encode the var itself. Created bool vars are pushed
  within the given destination layer. 

  The returned structure is a tree, whose internal nodes
  are ITE nodes or BITS variables, and leaves are constants values.

  <expr> :: ITE ( COLON (bit, left), right) |
            constant

  where bit is a variable name (a bit), and left and right are <expr>.
  
*/
static node_ptr
bool_enc_encode_scalar_var(BoolEnc_ptr self, node_ptr name, int suffix,
                           node_ptr values,
                           SymbLayer_ptr src_layer, SymbLayer_ptr dest_layer)
{
  node_ptr result;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));

  { /* declare constants if needed */
    node_ptr iter;
    for (iter=values; iter != Nil; iter = cdr(iter)) {
      node_ptr val = car(iter);
      if (SymbLayer_can_declare_constant(src_layer, val)) {
        SymbLayer_declare_constant(src_layer, val);
      }
    }
  }

  { /* calculates the encoding, and declares needed bits */
    Set_t bits = Set_MakeEmpty();
    Set_Iterator_t iter;

    result = bool_enc_compute_set_encoding(self, values, name, suffix, &bits, true);

    SET_FOREACH(bits, iter) {
      node_ptr bit = (node_ptr) Set_GetMember(bits, iter);

      if (! SymbTable_is_symbol_var(BASE_ENC(self)->symb_table, bit)) {
        SymbType_ptr type = SymbType_create(env, SYMB_TYPE_BOOLEAN, Nil);
        if (SymbTable_is_symbol_input_var(BASE_ENC(self)->symb_table, name)) {
          SymbLayer_declare_input_var(dest_layer, bit, type);
        }
        else if (SymbTable_is_symbol_state_var(BASE_ENC(self)->symb_table, name)) {
          SymbLayer_declare_state_var(dest_layer, bit, type);
        }
        else {
          SymbLayer_declare_frozen_var(dest_layer, bit, type);
        }
      }

      bool_enc_set_var_encoding(self, bit, bool_enc_get_boolean_type(self));
    }

    Set_ReleaseSet(bits);
  }

  return result;
}

/*!
  \brief Return true if the given list is {TRUE,FALSE}

  Return true if the given list is {TRUE,FALSE}
*/
static boolean bool_enc_is_boolean_range(const ExprMgr_ptr exprs, node_ptr values)
{
  if (ExprMgr_is_boolean_range(exprs, values)) return true;

  while (Nil != values) {
    node_ptr v = car(values);

    if (!(TRUEEXP == node_get_type(v) ||
          FALSEEXP == node_get_type(v))) {
      return false;
    }

    values = cdr(values);
  }

  return true;
}

/*!
  \brief Computes the boolean encoding of which can be used to
            represent a set of values.

  Constructs a ITEs tree whose leaves are
            the values occurring in the input set, in a logarithmic
            boolean encoding. Conditions of the ITEs are BIT nodes
            whose name is constructed by using bit_prefix and
            bit_suffix values.

            Returns the containing the boolean logarithmic
            encoding, and as output parameter the set of BITs nodes
            used in the encoding.

  \se BITs nodes are added to the out_bits set
*/

#if BOOL_ENCODING_LOWER_TO_HIGHER_BALANCED
/* this is the default encoding, see issue 2549 */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static node_ptr
bool_enc_compute_set_encoding(const BoolEnc_ptr self, node_ptr set,
                              node_ptr bit_prefix, int bit_suffix,
                              Set_t* out_bits, boolean top)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr var, left, right;

  /* Final case: we reached a leaf */
  if (cdr(set) == Nil) {
    return find_atom(nodemgr, car(set));
  }

  /* Intermediate case, declare the scalar variable */
  if ((true == top) && bool_enc_is_boolean_range(exprs, set)) {
    var = bit_prefix;
  }
  else {
    var = BoolEnc_make_var_bit(self, bit_prefix, bit_suffix);
  }
  *out_bits = Set_AddMember(*out_bits, (Set_Element_t) var);

  { /* Finally construct the sub binary tree, by decomposing left
       and right sides: */
    node_ptr ls_left;
    node_ptr ls_right;

    ls_left = even_elements(nodemgr, set);
    left  = bool_enc_compute_set_encoding(self, ls_left,
                                          bit_prefix, bit_suffix + 1,
                                          out_bits, false);
    free_list(nodemgr, ls_left);

    ls_right = odd_elements(nodemgr, set);
    right = bool_enc_compute_set_encoding(self, ls_right,
                                          bit_prefix, bit_suffix + 1,
                                          out_bits, false);
    free_list(nodemgr, ls_right);
  }

  return find_node(nodemgr, IFTHENELSE, find_node(nodemgr, COLON, var, left), right);
}

#elif BOOL_ENCODING_HIGHER_TO_LOWER_BALANCED
/* this is proved to increase performances */

static node_ptr
bool_enc_compute_set_encoding_aux(const BoolEnc_ptr self, node_ptr set,
                                  node_ptr bit_prefix, int bit_suffix,
                                  Set_t* out_bits, boolean top);

static node_ptr
bool_enc_compute_set_encoding(const BoolEnc_ptr self, node_ptr set,
                              node_ptr bit_prefix, int bit_suffix,
                              Set_t* out_bits, boolean top)
{
  /* 'max()' to fix issue 2563, as at least one bit is required */
  int bits = Utils_log2_round(max(llength(set)-1, 1));
  nusmv_assert(bits>0);
  return bool_enc_compute_set_encoding_aux(self, set, bit_prefix,
                                           bit_suffix + bits - 1,
                                           out_bits, top);
}

static node_ptr
bool_enc_compute_set_encoding_aux(const BoolEnc_ptr self, node_ptr set,
                                  node_ptr bit_prefix, int bit_suffix,
                                  Set_t* out_bits, boolean top)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr var, left, right;

  /* Final case: we reached a leaf */
  if (cdr(set) == Nil) {
    nusmv_assert(bit_suffix <= 0);
    return find_atom(nodemgr, car(set));
  }

  /* Intermediate case, declare the scalar variable */
  if ((true == top) && bool_enc_is_boolean_range(exprs, set)) {
    var = bit_prefix;
  }
  else {
    var = BoolEnc_make_var_bit(self, bit_prefix, bit_suffix);
  }
  *out_bits = Set_AddMember(*out_bits, (Set_Element_t) var);

  { /* Finally construct the sub binary tree, by decomposing left
       and right sides: */
    node_ptr ls_left;
    node_ptr ls_right;

    ls_left = even_elements(nodemgr, set);
    left  = bool_enc_compute_set_encoding_aux(self, ls_left,
                                              bit_prefix, bit_suffix - 1,
                                              out_bits, false);
    free_list(nodemgr, ls_left);

    ls_right = odd_elements(nodemgr, set);
    right = bool_enc_compute_set_encoding_aux(self, ls_right,
                                              bit_prefix, bit_suffix - 1,
                                              out_bits, false);
    free_list(nodemgr, ls_right);
  }

  return find_node(nodemgr, IFTHENELSE, find_node(nodemgr, COLON, var, left), right);
}

#elif BOOL_ENCODING_HIGHER_TO_LOWER_INCREMENTAL
/* This still have higher bits at hiegher levels, and it is
   constructed more efficiency, but the encoding results
   unbalanced. However its incrementality may result in better
   performances. See issue 2549 */
static node_ptr
bool_enc_compute_set_encoding(const BoolEnc_ptr self, node_ptr set,
                              node_ptr bit_prefix, int bit_suffix,
                              Set_t* out_bits, boolean top)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  array_t* array1;
  node_ptr res;
  node_ptr bit_list = Nil; /* used to reverse added bits in out_bits */

  array1 = array_alloc(node_ptr, 2);
  nusmv_assert((array_t*) NULL != array1);

  { /* first loop along the given list, to avoid one unneeded traversal */
    node_ptr var = Nil;
    node_ptr iter = set;
    while (iter != Nil) {
      /* are there at least two elements? */
      if (cdr(iter) != Nil) {
        node_ptr el;
        node_ptr el1 = car(iter);
        node_ptr el2 = cadr(iter);

        if (Nil == var) {
          if ((ExprMgr_is_false(exprs, el1) && ExprMgr_is_true(exprs, el2)) ||
              (ExprMgr_is_false(exprs, el2) && ExprMgr_is_true(exprs, el1))) {
            var = bit_prefix; /* reuse bit for boolean local encoding */
          }
          else {
            var = BoolEnc_make_var_bit(self, bit_prefix, bit_suffix);
          }
          bit_list = cons(nodemgr, var, bit_list);
        }
        el = find_node(nodemgr, IFTHENELSE, find_node(nodemgr, COLON, var, el1), el2);
        array_insert_last(node_ptr, array1, el);

        iter = cddr(iter); /* advance two elements */
      }
      else {
        /* append the last remaining element */
        array_insert_last(node_ptr, array1, car(iter));
        iter = cdr(iter); /* advance one element */
      }
    }
  }

  /* now proceed with the arrays only */
  while (array_n(array1) > 1) {
    array_t* array2;
    int idx;
    node_ptr var;

    array2 = array_alloc(node_ptr, array_n(array1) / 2 + 1);
    nusmv_assert((array_t*) NULL != array1);

    /* a new bit has to be created */
    bit_suffix += 1;
    var = Nil;

    idx = 0;
    /* traverses array1 greedily taking pairs into array2 */
    while (idx < array_n(array1)) {
      if (idx + 1 < array_n(array1)) {
        /* there are at least two elements */
        node_ptr el;
        node_ptr el1 = array_fetch(node_ptr, array1, idx);
        node_ptr el2 = array_fetch(node_ptr, array1, idx+1);

        if (Nil == var) {
          if ((ExprMgr_is_false(exprs, el1) && ExprMgr_is_true(exprs, el2)) ||
              (ExprMgr_is_false(exprs, el2) && ExprMgr_is_true(exprs, el1))) {
            var = bit_prefix; /* reuse bit for boolean local encoding */
          }
          else {
            var = BoolEnc_make_var_bit(self, bit_prefix, bit_suffix);
          }
          bit_list = cons(nodemgr, var, bit_list);
        }
        el = find_node(nodemgr, IFTHENELSE, find_node(nodemgr, COLON, var, el1), el2);

        array_insert_last(node_ptr, array2, el);
        idx += 2;
      }
      else {
        /* last element */
        array_insert_last(node_ptr, array2,
                          array_fetch(node_ptr, array1, idx));
        idx += 1;
      }
    }

    { /* now array2 substitutes array1 */
      array_t* tmp = array1;
      array1 = array2;
      array_free(tmp); /* frees previous array1 */
    }
  }

  { /* dumps all collected bits in reversed order, to keep the
       right order (higher indices before lower) */
    for (; Nil!=bit_list; bit_list=cdr(bit_list)) {
      *out_bits = Set_AddMember(*out_bits, (Set_Element_t) car(bit_list));
    }
  }

  /* clean up and exit */
  nusmv_assert(array_n(array1) == 1);
  res = array_fetch(node_ptr, array1, 0);
  array_free(array1);
  free_list(nodemgr, bit_list);
  return res;
}
#else
#error "Invalid bool encoding"
#endif

/*!
  \brief Associates the given variable with the specified
  boolean encoding

  
*/
static void
bool_enc_set_var_encoding(BoolEnc_ptr self, node_ptr name, node_ptr enc)
{ insert_assoc(self->var2enc, name, enc); }

/*!
  \brief Given a variable, returns its boolean encoding, or NULL
  if not encoded

  Private service
*/
static node_ptr
bool_enc_get_var_encoding(const BoolEnc_ptr self, node_ptr name)
{ return find_assoc(self->var2enc, name); }

/*!
  \brief Fills the given list with the BIT vars which
  occurs into the given var encoding

  
*/
static void bool_enc_traverse_encoding(const BoolEnc_ptr self,
                                       node_ptr enc, NodeList_ptr list)
{
  node_ptr bit;

  /* constant or number terminate (numbers are not stored as constants): */
  if ( SymbTable_is_symbol_constant(BASE_ENC(self)->symb_table, enc)
       || (node_get_type(enc) == NUMBER) || (enc == bool_enc_get_boolean_type(self))) return;

  if (node_get_type(enc) == IFTHENELSE) { /* usual IFTHENELSE encoding */
    bit = caar(enc);
    if (! NodeList_belongs_to(list, bit)) NodeList_append(list, bit);

    bool_enc_traverse_encoding(self, cdar(enc), list); /* 'then' */
    bool_enc_traverse_encoding(self, cdr(enc), list);      /* 'else' */
  }
  else if (node_get_type(enc) == UNSIGNED_WORD) { /* Word, i.e. array of bit-vars */
    node_ptr iter;
    for (iter = car(enc); iter != Nil; iter = cdr(iter)) {
      if(!NodeList_belongs_to(list, car(iter))) NodeList_append(list, car(iter));
    }
  }
  /* no other kind of node can appear at this level: */
  else error_unreachable_code();
}


/*!
  \brief Given a variable, it returns the mask of its encoding

  Returns an expression representing the mask that
  removes repetitions of leaves in a variable encoding.  This function
  assumes that the order in which we encounter variables in the
  expression representing the boolean encoding is the same as cube.

  As an example of what this function does, let us consider a variable
  x having range 0..4. It can be encoded with 3 bits are needed to
  encode it: x0, x1, x2. The encodeding performed by NuSMV is

     ITE(x0, ITE(x1, 1, 3), ITE(x1, 2, ITE(x2, 4,  0))).

  Thus x=2 corresponds to assignment !x0&x1 where x2 is a dont'care.
  Similarly for x=1 and x=3 (for x=0 and x=4) there is a unique
  complete assignment to the x0, x1, x2 variables that represent the
  respective encoding). This function fixes a value for x2 in the
  assignments representing x=2, x=1 and x=3 respectively (it force x2
  to be false). Thus it builds the formula in this case:

     ITE(x0, ITE(x2, 0, 1), ITE(x1, 1, ITE(x2, 0,  1)))

  that removes the redundant assignments where needed. 
*/

#define _IS_LEAF(self,enc) (SymbTable_is_symbol_constant(BASE_ENC(self)->symb_table, enc) || \
                            ((Nil != enc) && (node_get_type(enc) == NUMBER)) || \
                            (enc == bool_enc_get_boolean_type(self)))
#define _COND(enc) car(car(enc))
#define _THEN(enc) cdr(car(enc))
#define _ELSE(enc) cdr(enc)
#define _IS_ITE(enc) ((enc != Nil) && (IFTHENELSE == node_get_type(enc)) && \
                      (Nil != car(enc)) && (COLON ==  node_get_type(car(enc))))

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static node_ptr bool_enc_get_var_mask_recur(const BoolEnc_ptr self,
                                            node_ptr enc,
                                            NodeList_ptr cube,
                                            ListIter_ptr cube_iter)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  node_ptr res;
  SymbTable_ptr symb_table = BaseEnc_get_symb_table(BASE_ENC(self));

  if (ListIter_is_end(cube_iter)) {
    /* We reached the end of the cube:
       we must be guaranteed to be on a leaf of the DAG */
    nusmv_assert(_IS_LEAF(self, enc));
    res = ExprMgr_true(exprs);
  }
  else {
    node_ptr var = NodeList_get_elem_at(cube, cube_iter);

    if (_IS_LEAF(self, enc) || _COND(enc) != var) {
      /* There is a gap:
         we assign a value to missing variables in cube */
      node_ptr t = bool_enc_get_var_mask_recur(self,
                                               enc,
                                               cube,
                                               ListIter_get_next(cube_iter));

      res = ExprMgr_ite(exprs, var, ExprMgr_false(exprs), t, symb_table);
    }
    else {
      /* It is a variable:
         we keep visiting the dag, searching for gaps to fill */

      /* Assumption that the order in which we encounter variables in
         the enc is the same as cube */
      node_ptr t;
      node_ptr e;

      nusmv_assert(_COND(enc) == var);

      t = bool_enc_get_var_mask_recur(self, _THEN(enc), cube,
                                               ListIter_get_next(cube_iter));

      e = bool_enc_get_var_mask_recur(self, _ELSE(enc), cube,
                                               ListIter_get_next(cube_iter));

      res = ExprMgr_ite(exprs, _COND(enc), t, e, symb_table);
    }
  }

  return res;
}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static node_ptr bool_enc_get_boolean_type(const BoolEnc_ptr self)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  node_ptr boolean_type;

  if (!NuSMVEnv_has_value(env, ENV_BOOLEAN_TYPE)) {
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    boolean_type = find_node(nodemgr, BOOLEAN, Nil, Nil);

    NuSMVEnv_set_value(env, ENV_BOOLEAN_TYPE, boolean_type);
  }
  else {
    boolean_type = NODE_PTR(NuSMVEnv_get_value(env, ENV_BOOLEAN_TYPE));
  }

  return boolean_type;
}

/**AutomaticEnd***************************************************************/
