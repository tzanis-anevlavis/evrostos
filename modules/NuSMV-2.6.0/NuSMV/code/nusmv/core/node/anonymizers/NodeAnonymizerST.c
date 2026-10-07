/* ---------------------------------------------------------------------------


  This file is part of the ``core.node.anonymizers'' package of NuSMV version 2.
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
  \author Michele Dorigatti
  \brief Implementation of class 'NodeAnonymizerST'
  This class inherits from NodeAnonymizersDot the method for recognizing ids. It
  overwrites just the way the translation is done. Using the symbol table each
  symbol is prefix with a letter representing its type

  \todo: Missing description

*/


#include "nusmv/core/node/anonymizers/NodeAnonymizerST.h" 
#include "nusmv/core/node/anonymizers/NodeAnonymizerST_private.h" 
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/ucmd.h"
#include "nusmv/core/compile/compileUtil.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'NodeAnonymizerST_private.h' for class 'NodeAnonymizerST' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief The max lenght of an identifier

  
*/
#define NAST_MAX_ID_LEN 32

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void node_anonymizer_st_finalize(Object_ptr object, void* dummy);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

NodeAnonymizerST_ptr NodeAnonymizerST_create(NuSMVEnv_ptr env,
                                             const char* default_prefix,
                                             size_t memoization_threshold,
                                             SymbTable_ptr symb_table)
{
  NodeAnonymizerST_ptr self = ALLOC(NodeAnonymizerST, 1);
  NODE_ANONYMIZER_ST_CHECK_INSTANCE(self);

  nusmv_assert(NULL == default_prefix);

  node_anonymizer_st_init(self, env, default_prefix, memoization_threshold,
                          symb_table);
  return self;
}

void NodeAnonymizerST_destroy(NodeAnonymizerST_ptr self)
{
  NODE_ANONYMIZER_ST_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void node_anonymizer_st_init(NodeAnonymizerST_ptr self,
                             NuSMVEnv_ptr env,
                             const char* default_prefix,
                             size_t memoization_threshold,
                             SymbTable_ptr symb_table)
{
  /* base class initialization */
  node_anonymizer_dot_init(NODE_ANONYMIZER_DOT(self), env, default_prefix,
                           memoization_threshold);

  /* members initialization */
  self->symb_table = symb_table;

  {
    int i;
    for (i = NAST_COUNTERS_CARDINALITY + 1;--i; /* empty */) {
      self->counters[i-1] = 0;
    }
  }

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = node_anonymizer_st_finalize;
  OVERRIDE(NodeAnonymizerBase, translate) = node_anonymizer_st_translate;
  OVERRIDE(NodeAnonymizerBase, build_anonymous) =
    node_anonymizer_st_build_anonymous;
}

void node_anonymizer_st_deinit(NodeAnonymizerST_ptr self)
{
  /* members deinitialization */
  self->symb_table = NULL;

  {
    int i;
    for (i = NAST_COUNTERS_CARDINALITY + 1;--i; /* empty */) {
      self->counters[i-1] = 0;
    }
  }

  /* base class deinitialization */
  node_anonymizer_dot_deinit(NODE_ANONYMIZER_DOT(self));
}

node_ptr node_anonymizer_st_translate(NodeAnonymizerBase_ptr self,
                                      node_ptr id,
                                      const char* prefix)
{
  node_ptr retval = NULL;
  node_ptr resolved_name = NULL;

  nusmv_assert(self->is_id);
  nusmv_assert((char*)NULL !=  prefix);

  NAB_DEBUG_PRINT("%s INPUT\n%N", __func__, id);

  /* First, try to resolve it */
  /* TODO[MD] This is likely not efficient, probably it would be better to
     first lookup, then resolve if not found, and then lookup again */
  {
    NodeAnonymizerST_ptr _self = NODE_ANONYMIZER_ST(self);
    ResolveSymbol_ptr resolve_symbol = NULL;
    resolve_symbol = SymbTable_resolve_symbol(_self->symb_table, id, Nil);
    resolved_name = ResolveSymbol_resolve(resolve_symbol, _self->symb_table, id, Nil);

    if (ResolveSymbol_is_undefined(resolve_symbol)) {
      /* Symbol does not belong to the symb table. It must not belong to the
         model, no need to anonymize it */
      retval = id;
    }
    else {
      /* See if it is already in the map */
      retval = node_anonymizer_base_search_mapping(self, resolved_name);
    }
  }

  if (NULL == retval) {
    NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
    /* Not in the map: build the string */
    const char* anonymous_name = NULL;
    NodeMgr_ptr const nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    do {
      anonymous_name = self->build_anonymous(self, resolved_name, prefix);

      /* build the node */
      retval = sym_intern(env, (char*)anonymous_name);
      FREE(anonymous_name);
    } while (NodeAnonymizerBase_is_id_anonymous(self, retval));

    /* insert it in the map */
    node_anonymizer_base_insert_mapping(self, resolved_name, retval);
  }

  NAB_DEBUG_PRINT("%s OUTPUT\n%N", __func__, retval);

  nusmv_assert((node_ptr)NULL !=  retval);

  return retval;
}

/*!
  \brief Build the string to be used for the anonymous id

  We never use default_prefix.
  If prefix is not empty, anonymizer base is used. Otherwise the symb table is
  used to create a meaningful prefix.
  @return the returned string must be freed
*/

/* TODO[MD] We could use macros for the prefixes */
const char* node_anonymizer_st_build_anonymous(NodeAnonymizerBase_ptr self,
                                               node_ptr id,
                                               const char* prefix)
{
  NodeAnonymizerST_ptr _self = NODE_ANONYMIZER_ST(self);
  char* anonymous_name = NULL;

  NAB_DEBUG_PRINT("%s INPUT\n%s", __func__, id);

  if (0 != strcmp("", prefix)) {
    /* prefix provided, call base anonymizer */
    anonymous_name = (char*)node_anonymizer_base_build_anonymous(self, id, prefix);
  }
  else {
    anonymous_name = ALLOC(char, NAST_MAX_ID_LEN);

    if (SymbTable_is_symbol_var(_self->symb_table, id)) {
      /* First information: input|frozen|state */
      if (SymbTable_is_symbol_input_var(_self->symb_table, id)) {
        anonymous_name[0] = 'i';
      }
      else if (SymbTable_is_symbol_frozen_var(_self->symb_table, id)) {
        anonymous_name[0] = 'f';
      }
      else {
        anonymous_name[0] = 's';
      }

      /* Second information: type */
      /* TODO[MD] This code could be factorized with the code that handles the
         defines */
      {
        SymbType_ptr type;
        type = SymbTable_get_var_type(_self->symb_table, id);

        if (SymbType_is_word(type)) {
          int size = 0;
          int chars;

          size = SymbType_get_word_width(type);

          chars = snprintf(&anonymous_name[1], NAST_MAX_ID_LEN - 1, "w%d_%d",
                           size,
                           _self->counters[0]);
          SNPRINTF_CHECK(chars, NAST_MAX_ID_LEN - 1);

          /* TODO[MD] It would cleaner to use an enum */
          _self->counters[0] = _self->counters[0] + 1;
        }
        else if (SymbType_is_boolean(type)) {
          /* TODO[MD] This block of code and the ones below could be a var arg
             function */
          int chars = snprintf(&anonymous_name[1], NAST_MAX_ID_LEN - 1, "b_%d",
                               _self->counters[3]);
          SNPRINTF_CHECK(chars, NAST_MAX_ID_LEN - 1);

          _self->counters[3] = _self->counters[3] + 1;
        }
        else if (SymbType_is_enum(type)) {
          int chars = snprintf(&anonymous_name[1], NAST_MAX_ID_LEN - 1, "s_%d",
                               _self->counters[2]);
          SNPRINTF_CHECK(chars, NAST_MAX_ID_LEN - 1);

          _self->counters[2] = _self->counters[2] + 1;
        }
        else if (SymbType_is_infinite_precision(type)) {
          int chars = snprintf(&anonymous_name[1], NAST_MAX_ID_LEN - 1, "i_%d",
                               _self->counters[1]);
          SNPRINTF_CHECK(chars, NAST_MAX_ID_LEN - 1);

          _self->counters[1] = _self->counters[1] + 1;
        }
        else if (SymbType_is_wordarray(type)) {
          int chars = snprintf(&anonymous_name[1], NAST_MAX_ID_LEN - 1, "wa_%d",
                               _self->counters[8]);
          SNPRINTF_CHECK(chars, NAST_MAX_ID_LEN - 1);

          _self->counters[8] = _self->counters[8] + 1;
        }
        else if (SymbType_is_intarray(type)) {
          int chars = snprintf(&anonymous_name[1], NAST_MAX_ID_LEN - 1, "ia_%d",
                               _self->counters[10]);
          SNPRINTF_CHECK(chars, NAST_MAX_ID_LEN - 1);

          _self->counters[8] = _self->counters[10] + 1;          
        }
        else {
          NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
          ErrorMgr_ptr const errmgr =
            ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
          ErrorMgr_rpterr(errmgr, "Unknown type for obfuscation.");
          error_unreachable_code();
        }
      }
    }
    else if (SymbTable_is_symbol_constant(_self->symb_table, id)) {
      int chars = snprintf(anonymous_name, NAST_MAX_ID_LEN, "c_%d",
                           _self->counters[4]);
      SNPRINTF_CHECK(chars, NAST_MAX_ID_LEN);

      _self->counters[4] = _self->counters[4] + 1;
    }
    else if(SymbTable_is_symbol_variable_array(_self->symb_table, id)) {
      int chars;

      chars = snprintf(anonymous_name, NAST_MAX_ID_LEN, "a%d",
                       _self->counters[6]);
      SNPRINTF_CHECK(chars, NAST_MAX_ID_LEN);

      _self->counters[6] = _self->counters[6] + 1;
    }
    else if (SymbTable_is_symbol_define(_self->symb_table, id)) {
      SymbType_ptr dtype;
      char* t;
      int size = 0;
      TypeChecker_ptr type_checker =
        SymbTable_get_type_checker(_self->symb_table);

      dtype =
        TypeChecker_get_expression_type(type_checker,
                                        SymbTable_get_define_body(_self->symb_table,
                                                                  id),
                                        SymbTable_get_define_context(_self->symb_table,
                                                                     id));

      if (SymbType_is_word(dtype)) {
        size = SymbType_get_word_width(dtype);
        t = "w";
      }
      else if (SymbType_is_boolean(dtype)) {
        t = "b";
      }
      else if (SymbType_is_enum(dtype)) {
        t = "s";
      }
      else if (SymbType_is_infinite_precision(dtype)) {
        t = "i";
      }
      else if (SymbType_is_wordarray(dtype)) {
        t = "wa";
      }
      else if (SymbType_is_array(dtype)){
        t = "a";
      }
      else if (SymbType_is_intarray(dtype)){
        t = "ia";
      }
      else {
        t = "u";
      }

      if (SymbType_is_word(dtype)) {
        /* Print also the word size */
        int chars = snprintf(anonymous_name, NAST_MAX_ID_LEN, "d%s%d_%d",
                             t,
                             size,
                             _self->counters[5]);
        SNPRINTF_CHECK(chars, NAST_MAX_ID_LEN);
      }
      else {
        int chars = snprintf(anonymous_name, NAST_MAX_ID_LEN, "d%s_%d",
                             t,
                             _self->counters[5]);
        SNPRINTF_CHECK(chars, NAST_MAX_ID_LEN);
      }

      _self->counters[5] = _self->counters[5] + 1;
    }
    else if (SymbTable_is_symbol_array_define(_self->symb_table, id)) {
      int chars;

      chars = snprintf(anonymous_name, NAST_MAX_ID_LEN, "m%d",
                       _self->counters[7]);
      SNPRINTF_CHECK(chars, NAST_MAX_ID_LEN);

      _self->counters[7] = _self->counters[7] + 1;
    }
    else if (SymbTable_is_symbol_function(_self->symb_table, id)) {

      int chars = snprintf(anonymous_name, NAST_MAX_ID_LEN, "f_%d",
                           _self->counters[9]);
      SNPRINTF_CHECK(chars, NAST_MAX_ID_LEN);

      _self->counters[9]++;
    }
    else error_unreachable_code_msg("Unsupported symbol category");
  }

  NAB_DEBUG_PRINT("%s INPUT\n%s", __func__, anonymous_name);

  nusmv_assert(! util_is_string_null(anonymous_name));

  return anonymous_name;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The NodeAnonymizerST class virtual finalizer

  Called by the class destructor
*/
static void node_anonymizer_st_finalize(Object_ptr object, void* dummy)
{
  NodeAnonymizerST_ptr self = NODE_ANONYMIZER_ST(object);

  node_anonymizer_st_deinit(self);
  FREE(self);
}


/**AutomaticEnd***************************************************************/

