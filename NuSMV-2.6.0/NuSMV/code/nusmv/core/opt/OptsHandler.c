/* ---------------------------------------------------------------------------


  This file is part of the ``opt'' package of NuSMV version 2.
  Copyright (C) 2009 by FBK.

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
  \author Marco Roveri, Alessandro Mariotti
  \brief Generic handler of options.

  Generic handler of options. An option is uniquely
  identified in an option handler by:
  <ul>
    <li> a name (it is the string of its name).
    <li> a default value (it is the string of its name).
    <li> a value (it is the string of its name).
  </ul>
  When registering an option the user must specify two functions. The
  first is responsible of checking that the value passed while setting
  a value is a valid value for the given option. The second function
  is a function that transforms the stored value in a value suitable
  to be used in the calling program. <br>

  For boolean options are provided special methods to register the
  option and for setting and getting a value associated to it.<br>

  For enumerative options are provided special methods to register the
  option and for setting and getting a value associated to it. An
  enumertive option is registered by providing an array of structures
  of type Opts_EnumRec. Similarly to the below declaration:<br>
  <pre>typedef enum {foo1, ...., fooN} fooenumtype;
    Opts_EnumRec foo[] = {"foo1", foo1,
                          "foo2", foo2,
                          ....
                          "fooN", fooN};
   ....
   handler = OptsHandler_create();
   OptsHandler_register_enum_option(handler, "foooption", "foo1", foo, N);

  if (OptsHandler_get_enum_option_value(handler, "foooption") == foo2) {
     ...
  }

  ...

  switch(OptsHandler_get_enum_option_value(handler, "foooption")) {
    case foo1:
    ...
    case fooN:
    ...
    default:
    ...
  }
  </pre>


*/


#include "nusmv/core/utils/StreamMgr.h"
#include <string.h>
#include "nusmv/core/opt/OptsHandler.h"
#include "cudd/st.h"
#include "nusmv/core/utils/Slist.h"
#include "nusmv/core/utils/avl.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/compile/compileUtil.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef enum st_retval opts_st_retval;

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef struct _option_structure opt_rec;
typedef struct _option_structure* opt_ptr;
typedef struct _option_value_list ovl_rec;
typedef struct _option_value_list* ovl_ptr;

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef boolean (* Opts_CheckOplFnType)(OptsHandler_ptr,
                                        const char *, ovl_ptr);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef void* (* Opts_GetOplFnType)(OptsHandler_ptr, const char *, ovl_ptr);

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

struct _OptsHandler_Rec {
   UStringMgr_ptr strings;
  hash_ptr table;
  st_generator* gen;
  unsigned int opt_max_length;
};

struct _option_structure {
  char* name;
  char* default_value;
  char* value;
  ovl_ptr possible_values;
  boolean public;
  Opts_CheckFnType check;
  Opts_ReturnFnType getvalue;
  Option_Type type;
  boolean user_defined;
  Slist_ptr triggers;
  void* arg;
};

struct _option_value_list {
  ovl_ptr next;
  char* values;
  int valuee;
};

typedef struct _option_trigger_TAG {
  Opts_TriggerFnType trigger;
  void* arg;
} option_trigger;

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define OPTS_STR_CMP_FUNC strcmp

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define OPTS_BOOLEAN_REC {{OPTS_FALSE_VALUE, false},{OPTS_TRUE_VALUE, true}}

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define OPTS_DELETE_ENTRY ST_DELETE

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static boolean opts_isinteger(OptsHandler_ptr opts, const char *name, void* arg);
static void* opts_getinteger(OptsHandler_ptr opts, const char *value, void* arg);

static boolean opts_check_string(OptsHandler_ptr opts, const char* name, void* arg);
static void* opts_get_string(OptsHandler_ptr opts, const char* value, void* arg);

static opts_st_retval opts_hash_free(char *key, char *data, char* arg);
static opt_ptr option_alloc(void);
static void option_free(opt_ptr* o);
static char* opts_strsav(const char *s);
static opt_ptr option_create(const char* name,
                             const char* default_value,
                             const char* value,
                             ovl_ptr opl,
                             Opts_CheckFnType check,
                             Opts_ReturnFnType getvalue,
                             boolean is_public,
                             Option_Type opt_type,
                             boolean user_defined,
                             void* arg);

static boolean opt_enum_check(OptsHandler_ptr opts,
                              const char* value, ovl_ptr l);
static void* opt_enum_get(OptsHandler_ptr opts,
                           const char* value, ovl_ptr l);
static ovl_ptr ovl_rec_alloc(void);
static ovl_ptr ovl_create_empty(void);
static int ovl_isempty(ovl_ptr l);
static int ovl_isnotempty(ovl_ptr l);
static ovl_ptr ovl_create(const char* value, int valuee);
static ovl_ptr ovl_get_next(ovl_ptr l);
static ovl_ptr ovl_set_next(ovl_ptr l, ovl_ptr n);
static void ovl_free(ovl_ptr *l);
static int ovl_ispresent(ovl_ptr l, const char *value);
static ovl_ptr ovl_copy(ovl_ptr src);
static boolean check_boolean(ovl_ptr l);

static boolean opts_handler_register_generic_option(OptsHandler_ptr self,
                                                    const char* name,
                                                    const char* def,
                                                    ovl_ptr ovl,
                                                    Opts_CheckFnType check,
                                                    Opts_ReturnFnType get,
                                                    boolean is_public,
                                                    Option_Type type,
                                                    boolean user_defined,
                                                    void* arg);

static boolean
opts_handler_run_triggers(OptsHandler_ptr self, opt_ptr opt,
                          const char* name, const char* val,
                          Trigger_Action action);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

OptsHandler_ptr OptsHandler_create(void)
{
  OptsHandler_ptr result;

  result = ALLOC(OptsHandler_Rec, 1);
  OPTS_HANDLER_CHECK_INSTANCE(result);

  {
    hash_ptr h = new_assoc();

    if (NIL(st_table) == h) {
      error_unreachable_code();
    }
    else {
      result->table = h;
      result->gen = (st_generator *)NULL;
      result->opt_max_length = 0;
    }
  }

  result->strings =  UStringMgr_create();

  return result;
}

void OptsHandler_destroy(OptsHandler_ptr self)
{
  OPTS_HANDLER_CHECK_INSTANCE(self);

  assoc_foreach(self->table, opts_hash_free, (char*)NULL);
  free_assoc(self->table);
   UStringMgr_destroy(self->strings);

  FREE(self);
}

boolean OptsHandler_register_option(OptsHandler_ptr self,
                                    const char* name,
                                    const char* def,
                                    Opts_CheckFnType check,
                                    Opts_ReturnFnType get,
                                    boolean is_public,
                                    Option_Type type,
                                    void* arg)
{
  ovl_ptr ovl = ovl_create_empty();

  OPTS_HANDLER_CHECK_INSTANCE(self);

  return opts_handler_register_generic_option(self, name, def, ovl, check, get,
                                              is_public, type, false, arg);
}

boolean OptsHandler_register_generic_option(OptsHandler_ptr self,
                                            const char* name,
                                            const char* def,
                                            boolean is_public)
{
  ovl_ptr ovl = ovl_create_empty();

  OPTS_HANDLER_CHECK_INSTANCE(self);

  return opts_handler_register_generic_option(self, name, def, ovl,
                                              opts_check_string,
                                              opts_get_string,
                                              is_public, GENERIC_OPTION,
                                              false, NULL);
}

boolean OptsHandler_register_user_option(OptsHandler_ptr self,
                                         const char* name,
                                         const char* value)
{
  boolean result = false;
  ovl_ptr ovl =  ovl_create_empty();

  OPTS_HANDLER_CHECK_INSTANCE(self);

  result = opts_handler_register_generic_option(self, name, value, ovl,
                                                opts_check_string,
                                                opts_get_string,
                                                true, USER_OPTION, true,
                                                NULL);

  return(result);
}

boolean OptsHandler_register_enum_option(OptsHandler_ptr self,
                                         const char* name,
                                         const char* def,
                                         Opts_EnumRec pv[],
                                         int npv,
                                         boolean is_public)
{
  int n;
  opt_ptr opt;
  ovl_ptr ovl;
  boolean result = false;
  string_ptr uname =  UStringMgr_find_string(self->strings, (char*)name);

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table, NODE_PTR(uname));

  if ((opt_ptr)NULL == opt) {
    ovl = ovl_create_empty();

    for (n = 0; n < npv; n++) {
      ovl_ptr l = ovl_create(pv[n].v, pv[n].e);

      ovl_set_next(l, ovl);
      ovl = l;
    }

    result =
      opts_handler_register_generic_option(self, name, def, ovl,
                                           (Opts_CheckFnType)opt_enum_check,
                                           (Opts_ReturnFnType)opt_enum_get,
                                           is_public, ENUM_OPTION, false, NULL);
  }
  return(result);
}

boolean OptsHandler_register_bool_option(OptsHandler_ptr self,
                                         const char* name,
                                         boolean value,
                                         boolean is_public)
{
  boolean result = false;
  char* def;
  ovl_ptr ovl;
  Opts_EnumRec pv[2] = OPTS_BOOLEAN_REC;
  int i;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  ovl = ovl_create_empty();
  for (i = 0; i < 2; ++i) {
    ovl_ptr l = ovl_create(pv[i].v, pv[i].e);
    ovl_set_next(l, ovl);
    ovl = l;
  }

  def = (value == true) ? OPTS_TRUE_VALUE : OPTS_FALSE_VALUE;

  result =
    opts_handler_register_generic_option(self, name, def, ovl,
                                         (Opts_CheckFnType)opt_enum_check,
                                         (Opts_ReturnFnType)opt_enum_get,
                                         is_public, BOOL_OPTION, false, NULL);

  return(result);
}

boolean OptsHandler_register_int_option(OptsHandler_ptr self,
                                        const char* name,
                                        int value,
                                        boolean is_public)
{
  boolean result = false;
  char def[100];
  ovl_ptr ovl = ovl_create_empty();
  int chars;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  chars = snprintf(def, 100, "%d", value);
  SNPRINTF_CHECK(chars, 100);

  result = opts_handler_register_generic_option(self, name, def, ovl,
                                                opts_isinteger, opts_getinteger,
                                                is_public, INTEGER_OPTION, false, NULL);
  return(result);
}

boolean OptsHandler_is_option_public(OptsHandler_ptr self,
                                     const char* name)
{
  opt_ptr opt = (opt_ptr)NULL;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table, NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));
  nusmv_assert((opt_ptr)NULL != opt);

  return opt->public;
}

boolean OptsHandler_is_user_option(OptsHandler_ptr self,
                                   const char* name)
{
  opt_ptr opt;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table, NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));
  nusmv_assert((opt_ptr)NULL != opt);

  return (USER_OPTION == opt->type);
}

boolean OptsHandler_is_bool_option(OptsHandler_ptr self,
                                   const char* name)
{
  opt_ptr opt;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table, NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));
  nusmv_assert((opt_ptr)NULL != opt);

  return (BOOL_OPTION == opt->type);
}

boolean OptsHandler_is_int_option(OptsHandler_ptr self,
                                  const char* name)
{
  opt_ptr opt;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table, NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));
  nusmv_assert((opt_ptr)NULL != opt);

  return (INTEGER_OPTION == opt->type);
}

boolean OptsHandler_is_enum_option(OptsHandler_ptr self,
                                   const char* name)
{
  opt_ptr opt;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table,
                            NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));
  nusmv_assert((opt_ptr)NULL != opt);

  return (ENUM_OPTION == opt->type);
}

boolean OptsHandler_is_generic_option(OptsHandler_ptr self,
                                      const char* name)
{
  opt_ptr opt;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table,
                            NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));
  nusmv_assert((opt_ptr)NULL != opt);

  return (GENERIC_OPTION == opt->type);
}

void OptsHandler_get_enum_option_values(OptsHandler_ptr self,
                                        const char* name,
                                        char*** values,
                                        int* num_values)
{
  opt_ptr opt;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table,
                            NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));
  nusmv_assert((opt_ptr)NULL != opt);

  nusmv_assert(ENUM_OPTION == opt->type || BOOL_OPTION == opt->type);

  {
    ovl_ptr l = opt->possible_values;
    int num = 0;
    for ( ; ovl_isnotempty(l); l = ovl_get_next(l)) {
      ++num;
    }

    *values = ALLOC(char*, num);
    *num_values = num;
    l = opt->possible_values;
    num = 0;
    for ( ; ovl_isnotempty(l); l = ovl_get_next(l), ++num) {
      (*values)[num] = strdup(l->values);
    }
  }
}

node_ptr OptsHandler_get_enum_option_values_as_node(OptsHandler_ptr self,
                                                    NuSMVEnv_ptr env,
                                                    const char* name,
                                                    int* num_values)
{
  NodeMgr_ptr const nodemgr =
     NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  opt_ptr opt;
  node_ptr retval;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table,
                            NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));
  nusmv_assert((opt_ptr)NULL != opt);

  nusmv_assert(ENUM_OPTION == opt->type || BOOL_OPTION == opt->type);

  {
    ovl_ptr l = opt->possible_values;
    int num = 0;
    for ( ; ovl_isnotempty(l); l = ovl_get_next(l)) {
      ++num;
    }

    *num_values = num;
    l = opt->possible_values;
    retval = new_list();
    num = 0;
    for ( ; ovl_isnotempty(l); l = ovl_get_next(l), ++num) {
      node_ptr element = sym_intern(env, l->values);
      retval = Node_conslist_add(nodemgr, retval, element);
    }
  }

  return retval;
}

boolean OptsHandler_is_option_registered(OptsHandler_ptr self,
                                         const char* name)
{
  opt_ptr opt;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table,
                            NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));
  return ((opt_ptr)NULL != opt);
}

boolean OptsHandler_is_option_not_registered(OptsHandler_ptr self,
                                             const char* name)
{
  opt_ptr opt;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table,
                            NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));
  return ((opt_ptr)NULL == opt);
}

boolean OptsHandler_unregister_option(OptsHandler_ptr self, const char* name)
{
  opt_ptr opt;
  boolean result = false;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)remove_assoc(self->table,
                              NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));
  if ((opt_ptr)NULL != opt) {
    option_free(&opt);
    result = true;
  }

  return(result);
}

boolean OptsHandler_set_option_value(OptsHandler_ptr self,
                                     const char* name,
                                     const char* value)
{
  opt_ptr opt;
  boolean result = false;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table,
                            NODE_PTR( UStringMgr_find_string(self->strings,
                                                             (char*)name)));

  if ((opt_ptr)NULL != opt) {

    if (ovl_isempty(opt->possible_values) == 1) {
      /* user defined option */
      if ( (* opt->check)(self, (char *)value, opt->arg) == true ) {
        /* Run triggers, if any.. */
        if (!opts_handler_run_triggers(self, opt, name, value, ACTION_SET)) {
          return false;
        }

        if (value != opt->value) {
          if ((char*) NULL != opt->value) {
            FREE(opt->value);
          }
          opt->value = opts_strsav(value);
        }
        result = true;
      }
    }
    else {
      Opts_CheckOplFnType f;

      f = (Opts_CheckOplFnType)opt->check;
      /* internally handled options */
      if ((*f)(self, value, opt->possible_values) == true) {
        /* Run triggers, if any.. */
        if (!opts_handler_run_triggers(self, opt, name, value, ACTION_SET)) {
          return false;
        }

        if (value != opt->value) {
          if ((char*) NULL != opt->value) {
            FREE(opt->value);
          }
          opt->value = opts_strsav(value);
        }
        result = true;
      }
    }
  }
  return result;
}

boolean OptsHandler_set_enum_option_value(OptsHandler_ptr self,
                                          const char* name,
                                          const char* value)
{
  OPTS_HANDLER_CHECK_INSTANCE(self);
  return(OptsHandler_set_option_value(self, name, value));
}

boolean OptsHandler_set_bool_option_value(OptsHandler_ptr self,
                                          const char* name,
                                          boolean value)
{
  char* v;
  opt_ptr opt;
  boolean result = false;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table,
                            NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));

  if ((opt_ptr)NULL != opt) {
    nusmv_assert(check_boolean(opt->possible_values));

    v = (value == true) ? OPTS_TRUE_VALUE : OPTS_FALSE_VALUE;

    /* Run triggers, if any.. */
    if (!opts_handler_run_triggers(self, opt, name, v, ACTION_SET)) {
      return false;
    }

    result = true;
    if ((char *)NULL != opt->value) {
      FREE(opt->value);
    }
    opt->value = opts_strsav(v);
  }
  return(result);
}

boolean OptsHandler_set_int_option_value(OptsHandler_ptr self,
                                         const char* name,
                                         int value)
{
  char val[100];
  int chars;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  chars = snprintf(val, 100, "%d", value);
  SNPRINTF_CHECK(chars, 100);

  return(OptsHandler_set_option_value(self, name, val));
}

boolean OptsHandler_reset_option_value(OptsHandler_ptr self, const char* name)
{
  opt_ptr opt;
  boolean result = false;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table,
                            NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));

  if ((opt_ptr)NULL != opt) {

    /* Run triggers, if any.. */
    if (!opts_handler_run_triggers(self, opt, name, opt->value, ACTION_RESET)) {
      return false;
    }

    if ((char *)NULL != opt->value) {
      FREE(opt->value);
    }
    if ((char *)NULL != opt->default_value) {
      opt->value = opts_strsav(opt->default_value);
    }
    else {
      opt->value = (char*)NULL;
    }
    result = true;
  }
  return(result);
}

void* OptsHandler_get_option_value(OptsHandler_ptr self, const char* name)
{
  opt_ptr opt;
  void* result = NULL;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table,
                            NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));

  if ((opt_ptr)NULL != opt) {

    /* Run triggers, if any.. */
    opts_handler_run_triggers(self, opt, name, opt->value, ACTION_GET);

    if ((char *)NULL != opt->value) {
      if (ovl_isempty(opt->possible_values) == 1) {
        /* user defined option */
        result = (* opt->getvalue)(self, opt->value, opt->arg);
      }
      else {
        /* internally handled option */
        Opts_GetOplFnType f;

        f = (Opts_GetOplFnType)opt->getvalue;
        result = (*f)(self, opt->value, opt->possible_values);
      }
    }
  }
  else {
    result = OPTS_VALUE_ERROR;
  }

  return(result);
}

void* OptsHandler_get_option_default_value(OptsHandler_ptr self,
                                            const char* name)
{
  opt_ptr opt;
  void* result = NULL;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table,
                            NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));

  if ((opt_ptr)NULL != opt) {
    /* Run triggers, if any.. */
    opts_handler_run_triggers(self, opt, name, opt->value, ACTION_GET);

    if ((char *)NULL != opt->default_value) {
      if (ovl_isempty(opt->possible_values) == 1) {
        /* user defined option */
        result = (* opt->getvalue)(self, opt->default_value, opt->arg);
      }
      else {
        /* internally handled option */
        Opts_GetOplFnType f;

        f = (Opts_GetOplFnType)opt->getvalue;
        result = (*f)(self, opt->default_value, opt->possible_values);
      }
    }
  }
  else {
    result = OPTS_VALUE_ERROR;
  }

  return(result);
}

char* OptsHandler_get_string_representation_option_value(OptsHandler_ptr self,
                                                         const char* name)
{
  opt_ptr opt = (opt_ptr)NULL;
  char* result = NULL;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table, NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));

  if ((opt_ptr)NULL != opt) {

    /* Run triggers, if any.. */
    opts_handler_run_triggers(self, opt, name, opt->value, ACTION_GET);

    if ((char*)NULL != opt->value) {
      result = opts_strsav(opt->value);
    }
    else {
      result = opts_strsav("NULL");
    }
  }

  return result;
}

char*
OptsHandler_get_string_representation_option_default_value(OptsHandler_ptr self,
                                                           const char* name)
{
  opt_ptr opt = (opt_ptr)NULL;
  char* result = NULL;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table, NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));

  if ((opt_ptr)NULL != opt) {

    /* Run triggers, if any.. */
    opts_handler_run_triggers(self, opt, name, opt->value, ACTION_GET);

    if ((char*)NULL != opt->default_value) {
      result = opts_strsav(opt->default_value);
    }
    else {
      result = opts_strsav("NULL");
    }
  }

  return result;
}

char* OptsHandler_get_string_option_value(OptsHandler_ptr self,
                                          const char* name)
{
  OPTS_HANDLER_CHECK_INSTANCE(self);

  return (char*)OptsHandler_get_option_value(self, name);
}

char* OptsHandler_get_string_option_default_value(OptsHandler_ptr self,
                                                  const char* name)
{
  OPTS_HANDLER_CHECK_INSTANCE(self);

  return (char*)OptsHandler_get_option_default_value(self, name);
}

int OptsHandler_get_enum_option_value(OptsHandler_ptr self, const char* name)
{
  OPTS_HANDLER_CHECK_INSTANCE(self);

  return PTR_TO_INT(OptsHandler_get_option_value(self, name));
}

int OptsHandler_get_enum_option_default_value(OptsHandler_ptr self,
                                              const char* name)
{
  OPTS_HANDLER_CHECK_INSTANCE(self);

  return PTR_TO_INT(OptsHandler_get_option_default_value(self, name));
}

boolean OptsHandler_get_bool_option_value(OptsHandler_ptr self,
                                          const char* name)
{
  opt_ptr opt;
  boolean result = (boolean)OPTS_VALUE_ERROR;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table, NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));

  if ((opt_ptr)NULL != opt) {
    nusmv_assert(check_boolean(opt->possible_values));

    /* Run triggers, if any.. */
    opts_handler_run_triggers(self, opt, name, opt->value, ACTION_GET);

    if ((char *)NULL != opt->value) {
        Opts_GetOplFnType f;

        f = (Opts_GetOplFnType)opt->getvalue;
        result = (boolean)(*f)(self, opt->value, opt->possible_values);
    }
  }
  return(result);
}

boolean OptsHandler_get_bool_option_default_value(OptsHandler_ptr self,
                                                  const char* name)
{
  opt_ptr opt;
  boolean result = (boolean)OPTS_VALUE_ERROR;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table, NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));

  if ((opt_ptr)NULL != opt) {
    nusmv_assert(check_boolean(opt->possible_values));

    /* Run triggers, if any.. */
    opts_handler_run_triggers(self, opt, name, opt->value, ACTION_GET);

    if ((char *)NULL != opt->default_value) {
      Opts_GetOplFnType f;

      f = (Opts_GetOplFnType)opt->getvalue;
      result = (boolean)(*f)(self, opt->default_value, opt->possible_values);
    }
  }

 return result;
}

int OptsHandler_get_int_option_value(OptsHandler_ptr self, const char* name)
{
  OPTS_HANDLER_CHECK_INSTANCE(self);
  return PTR_TO_INT(OptsHandler_get_option_value(self, name));
}

int OptsHandler_get_int_option_default_value(OptsHandler_ptr self,
                                             const char* name)
{
  OPTS_HANDLER_CHECK_INSTANCE(self);
  return PTR_TO_INT(OptsHandler_get_option_default_value(self, name));
}

void Opts_Gen_init(OptsHandler_ptr self)
{
  st_generator* gen;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  nusmv_assert((st_generator *)NULL == self->gen);

  gen = st_init_gen((st_table*)self->table);
  if ((st_generator *)NULL != gen) {
    self->gen = gen;
  }
  else {
    error_unreachable_code_msg("Opts_GenInit: Unable to allocate generator\n");
  }
}

int Opts_Gen_next(OptsHandler_ptr self, char **name, char **value)
{
  string_ptr n;
  opt_ptr opt = (opt_ptr)1;
  int result;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  nusmv_assert((st_generator *)NULL != self->gen);

  *value = (char *)NULL;
  result = st_gen(self->gen, (char**)&n, (char **)&opt);
  if (result != 0) {
    nusmv_assert((opt_ptr)NULL != opt);
    *name = opt->name;
    *value = opt->value;
  }
  return(result);
}

void Opts_Gen_deinit(OptsHandler_ptr self)
{
  OPTS_HANDLER_CHECK_INSTANCE(self);

  nusmv_assert((st_generator *)NULL != self->gen);

  st_free_gen(self->gen);
  self->gen = (st_generator *)NULL;
}

boolean OptsHandler_add_option_trigger(OptsHandler_ptr self, const char* name,
                                       Opts_TriggerFnType trigger, void* arg)
{
  opt_ptr opt;
  option_trigger* trigger_struct = ALLOC(option_trigger, 1);

  trigger_struct->trigger = trigger;
  trigger_struct->arg = arg;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table, NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));

  if ((opt_ptr)NULL != opt) {
    Slist_push(opt->triggers, (void*) trigger_struct);
    return true;
  }

  return false;
}

boolean OptsHandler_remove_option_trigger(OptsHandler_ptr self, const char* name,
                                          Opts_TriggerFnType trigger)
{
  opt_ptr opt;
  boolean res = false;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  opt = (opt_ptr)find_assoc(self->table, NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));

  if ((opt_ptr)NULL != opt) {
    Siter iter;
    option_trigger* trigger_struct = (option_trigger*)NULL;

    SLIST_FOREACH(opt->triggers, iter) {
      trigger_struct = (option_trigger*)Siter_element(iter);

      if (trigger_struct->trigger == trigger) {
        break;
      }
    }

    if (trigger_struct->trigger == trigger) {
      res = Slist_remove(opt->triggers, (void*) trigger_struct);
      FREE(trigger_struct);
      res = true;
    }
  }

  return res;
}

void OptsHandler_print_all_options(OptsHandler_ptr self, FILE* fd,
                                   boolean print_private)
{
  char* name;
  char* value;
  unsigned int j = 0;
  avl_tree* avl = avl_init_table((int (*)(char*, char*))Utils_strcasecmp);
  avl_generator* gen;

  OPTS_HANDLER_CHECK_INSTANCE(self);

  nusmv_assert((FILE *)NULL != fd);

  OPTS_FOREACH_OPTION(self, &name, &value) {
    if (print_private || OptsHandler_is_option_public(self, name)) {
      avl_insert(avl, name, value);
    }
  }

  gen = avl_init_gen(avl, AVL_FORWARD);

  while (avl_gen(gen, &name, &value) == 1) {
    fprintf(fd, "%s ", name);

    /* Indent.. */
    for (j = 0; j <= (self->opt_max_length - strlen(name)); ++j) {
      fprintf(fd, " ");
    }

    if ((char*)NULL != value) {
      fprintf(fd, " \"%s\"\n", value);
    }
    else {
      fprintf(fd, " NULL\n");
    }
  }

  avl_free_gen(gen);
  avl_free_table(avl, 0, 0);
}

/* Internal function for generating test for SET / UNSET commands */
void OptsHandler_generate_test(OptsHandler_ptr self, FILE* of,
                               boolean gen_unset)
{
  char* name;
  char* value;

  if (gen_unset) {
    fprintf(of, "COMMAND unset\n");
  }
  else {
    fprintf(of, "COMMAND set\n");
  }

  fprintf(of, "MODELS nomodelneeded\n");
  if (gen_unset) {
    fprintf(of, "OPTS {\"-h\", FAIL[usage: unset]}\n");
  }
  else {
    fprintf(of, "OPTS {\"-h\", FAIL[usage: set]}\n");
  }
  fprintf(of, "OPTS ");

  OPTS_FOREACH_OPTION(self, &name, &value) {
    if (OptsHandler_is_option_public(self, name)) {
      if (gen_unset) {
        fprintf(of, " {%s, PASS[]}\n|", name);
      }
      else {
        /* SPECIAL CASES */
        if (strcmp(name, "output_word_format") == 0) {
          fprintf(of, " {%s 2, PASS[]}\n|", name);
          fprintf(of, " {%s 8, PASS[]}\n|", name);
          fprintf(of, " {%s 10, PASS[]}\n|", name);
          fprintf(of, " {%s 16, PASS[]}\n|", name);
          fprintf(of, " {%s 1, FAIL[]}\n|", name);
          fprintf(of, " {%s 7, FAIL[]}\n|", name);
        }
        else if (strcmp(name, "sat_solver") == 0) {
          fprintf(of, " {%s zchaff, PASS[]}\n|", name);
          fprintf(of, " {%s minisat, PASS[]}\n|", name);
          fprintf(of, " {%s iamnotasatsolver, FAIL[]}\n|", name);
        }
        else if (strcmp(name, "pp_list") == 0) {
          fprintf(of, " {%s m4, PASS[]}\n|", name);
          fprintf(of, " {%s \"m4 cpp\", PASS[]}\n|", name);
          fprintf(of, " {%s \"cpp\", PASS[]}\n|", name);
        }
        else if (strcmp(name, "bmc_loopback") == 0) {
          fprintf(of, " {%s \"*\", PASS[]}\n|", name);
        }
        else if (strcmp(name, "bmc_inc_invar_alg") == 0) {
          fprintf(of, " {%s zigzag, PASS[]}\n|", name);
          fprintf(of, " {%s dual, PASS[]}\n|", name);
          fprintf(of, " {%s falsification, PASS[]}\n|", name);
        }
        else if (strcmp(name, "bmc_invar_alg") == 0) {
          fprintf(of, " {%s classic, PASS[]}\n|", name);
          fprintf(of, " {%s een-sorensson, PASS[]}\n|", name);
        }
        /* STANDARD VALUES VARIABLES */
        else if (OptsHandler_is_bool_option(self, name)) {
          fprintf(of, " {%s, PASS[]}\n|", name);
          fprintf(of, " {%s 1, PASS[]}\n|", name);
          fprintf(of, " {%s 0, PASS[]}\n|", name);
          fprintf(of, " {%s 2, FAIL[]}\n|", name);
        }
        else if (OptsHandler_is_enum_option(self, name)) {
          char** values;
          int num_values, i;
          OptsHandler_get_enum_option_values(self, name, &values, &num_values);
          for (i = 0; i < num_values; ++i) {
            fprintf(of, " {%s %s, PASS[]}\n|", name, values[i]);
          }
          fprintf(of, " {%s __i_am_not_valid__, FAIL[]}\n|", name);
        }
        else if (OptsHandler_is_int_option(self, name)) {
          fprintf(of, " {%s 1, PASS[]}\n|", name);
          fprintf(of, " {%s 2, PASS[]}\n|", name);
          fprintf(of, " {%s 3, PASS[]}\n|", name);
          fprintf(of, " {%s NaN, FAIL[]}\n|", name);
        }
        else if (OptsHandler_is_generic_option(self, name)) {
          fprintf(of, " {%s \"\", PASS[]}\n|", name);
          fprintf(of, " {%s \"custom_string\", PASS[]}\n|", name);
        }
      }
    }
  }
}

void OptsHandler_copy(OptsHandler_ptr src_opts, OptsHandler_ptr dst_opts)
{
  char* name;
  char* value;

  OPTS_FOREACH_OPTION(src_opts, &name, &value) {
    opt_ptr opt =
      (opt_ptr) find_assoc(src_opts->table,
                           NODE_PTR(UStringMgr_find_string(src_opts->strings,
                                                           name)));
    nusmv_assert( (opt_ptr) NULL != opt);

    if (! OptsHandler_is_option_registered(dst_opts, name)) {
      /* copy values */
      ovl_ptr dst_ovl = ovl_copy(opt->possible_values);

      opts_handler_register_generic_option(dst_opts,
                                           name,
                                           opt->default_value,
                                           dst_ovl,
                                           opt->check,
                                           opt->getvalue,
                                           opt->public,
                                           opt->type,
                                           opt->user_defined,
                                           NULL);
    }
    {
      boolean copy;
      opt_ptr dst_opt =
        (opt_ptr) find_assoc(dst_opts->table,
                             NODE_PTR(UStringMgr_find_string(dst_opts->strings,
                                                             name)));
      nusmv_assert( (opt_ptr) NULL != dst_opt);
      /* dst_opt->value = opts_strsav(value); */

      copy = false;
      if (dst_opt->value != value) {
        if ((char*) NULL != dst_opt->value && (char*) NULL != value) {
          copy = 0 != strcmp(dst_opt->value, value);
        }
      }

      if (copy) {
        int res;
        res = OptsHandler_set_option_value(dst_opts, name, value);
        nusmv_assert(res);
      }
    }
  }
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief Creates a copy of the given string

  Creates a copy of the given string. Must be freed
*/
static char* opts_strsav(const char *s)
{
  if ((char*)NULL != s) {
    return(strcpy(ALLOC(char, strlen(s)+1), s));
  }

  return (char*) NULL;
}

/*!
  \brief Dummy function for string options handling

  Dummy function for string options handling
*/
static boolean opts_check_string(OptsHandler_ptr opts, const char* name, void* arg)
{
  return true;
}

/*!
  \brief Dummy function for string options handling

  Dummy function for string options handling
*/
static void* opts_get_string(OptsHandler_ptr opts, const char* value, void* arg)
{
  return (void *)value;
}

/*!
  \brief Check if a string represents an integer

  Check if a string represents an integer
*/
static boolean opts_isinteger(OptsHandler_ptr opts, const char *name, void* arg)
{
  char* e[1];
  boolean result = false;

  e[0] = "";
  (void)strtol(name, e, 10);
  if (strcmp(e[0], "") == 0) {
    result = true;
  }
  return(result);
}

/*!
  \brief Get the integer representation of the given string

  Get the integer representation of the given string
*/
static void* opts_getinteger(OptsHandler_ptr opts, const char *value, void* arg)
{
  int result;
  char* e[1];

  e[0] = "";
  result = (int)strtol(value, e, 10);
  if (strcmp(e[0], "") != 0) {
    return OPTS_VALUE_ERROR;
  }
  return PTR_FROM_INT(void*, result);
}

/*!
  \brief Allocates an instance of option

  Allocates an instance of option

  \sa option_free
*/
static opt_ptr option_alloc(void)
{
  opt_ptr result;

  result = ALLOC(opt_rec, 1);
  if (NIL(opt_rec) == result) {
    error_unreachable_code_msg("option_alloc: unable to allocate option entry.\n");
    return((opt_ptr)NULL);
  }
  result->name            = (char *)NULL;
  result->default_value   = (char *)NULL;
  result->value           = (char *)NULL;
  result->possible_values = ovl_create_empty();
  result->check           = (Opts_CheckFnType)NULL;
  result->getvalue        = (Opts_ReturnFnType)NULL;
  result->public          = false;
  result->type            = GENERIC_OPTION;
  result->user_defined    = false;
  result->triggers        = SLIST(NULL);
  return(result);
}

/*!
  \brief Frees the given option instance

  Frees the given option instance

  \sa option_alloc
*/
static void option_free(opt_ptr* p)
{
  opt_ptr o = *p;
  nusmv_assert( o != (opt_ptr)NULL);

  if ((char *)NULL != o->name) FREE(o->name);
  if ((char *)NULL != o->default_value) FREE(o->default_value);
  if ((char *)NULL != o->value) FREE(o->value);
  if (ovl_isnotempty(o->possible_values) == 1) {
    ovl_ptr q = o->possible_values;
    ovl_free(&q);
  }
  if (SLIST(NULL) != o->triggers) {
    Siter iter;
    SLIST_FOREACH(o->triggers, iter) {
      option_trigger* ot = (option_trigger*)Siter_element(iter);
      FREE(ot);
    }
    Slist_destroy(o->triggers);
  }
  FREE(*p);
}

/*!
  \brief Option Hash table freeing function

  Option Hash table freeing function
*/
static opts_st_retval opts_hash_free(char *key, char *data, char* arg)
{
  opt_ptr entry = (opt_ptr)data;

  if ((opt_ptr)NULL != entry) {
    option_free(&entry);
  }
  return(OPTS_DELETE_ENTRY);
}

/*!
  \brief Creates and initializes a new instance of option

  Creates and initializes a new instance of option

  \sa option_alloc
*/
static opt_ptr option_create(const char* name,
                             const char* default_value,
                             const char* value,
                             ovl_ptr       pvalues,
                             Opts_CheckFnType check,
                             Opts_ReturnFnType getvalue,
                             boolean is_public,
                             Option_Type type,
                             boolean user_defined,
                             void* arg)
{
  opt_ptr result = option_alloc();

  if ((opt_ptr)NULL != result) {
    result->name            = opts_strsav(name);
    result->default_value   = opts_strsav(default_value);
    result->value           = opts_strsav(value);
    result->possible_values = pvalues;
    result->check           = check;
    result->getvalue        = getvalue;
    result->public          = is_public;
    result->type            = type;
    result->user_defined    = user_defined;
    result->triggers        = Slist_create();
    result->arg             = arg;
  }
  return(result);
}

/*!
  \brief Checks if the given enumerative is in the given ovl

  Checks if the given enumerative is in the given ovl
*/
static boolean opt_enum_check(OptsHandler_ptr opts, const char* value, ovl_ptr l)
{
  boolean result = false;
  if (ovl_ispresent(l, value) == 1) {
    result = true;
  }
  return(result);
}

/*!
  \brief Gen the given enumerative value

  Gen the given enumerative value
*/
static void* opt_enum_get(OptsHandler_ptr opts, const char* value, ovl_ptr l)
{
  void* result = OPTS_VALUE_ERROR;
  int found = 0;

  for ( ; (ovl_isnotempty(l) && (found == 0)); l = ovl_get_next(l)) {
    if (strcmp(l->values, value) == 0) {
      found = 1;
      result = PTR_FROM_INT(void*, l->valuee);
    }
  }
  return(result);
}

/*!
  \brief Allocates and initializes an ovl_ptr

  Allocates and initializes an ovl_ptr
*/
static ovl_ptr ovl_rec_alloc(void)
{
  ovl_ptr result;

  result = ALLOC(ovl_rec, 1);
  if ((ovl_ptr)NULL == result) {
    error_unreachable_code_msg("ovl_rec_alloc: unable to allocate a value record.\n");
  }

  result->next = ovl_create_empty();
  result->values = (char *)NULL;
  result->valuee = PTR_TO_INT(OPTS_VALUE_ERROR);
  return(result);
}

/*!
  \brief Creates an empty ovl_ptr (represented by NULL)

  Creates an empty ovl_ptr (represented by NULL)
*/
static ovl_ptr ovl_create_empty(void)
{
  return ((ovl_ptr)NULL);
}

/*!
  \brief Checks if the given ovl is empty

  Checks if the given ovl is empty
*/
static int ovl_isempty(ovl_ptr l)
{
  return((l == ovl_create_empty()));
}

/*!
  \brief Checks if the given ovl is not empty

  Checks if the given ovl is not empty
*/
static int ovl_isnotempty(ovl_ptr l)
{
  return((l != ovl_create_empty()));
}

/*!
  \brief Creates a new instance of ovl and sets
                      it with the given values

  Creates a new instance of ovl and sets
                      it with the given values
*/
static ovl_ptr ovl_create(const char* values, int valuee)
{
  ovl_ptr result = (ovl_ptr)NULL;

  result = ovl_rec_alloc();
  if ((ovl_ptr)NULL != result) {
    result->values = opts_strsav(values);
    result->valuee = valuee;
  }
  return(result);
}

/*!
  \brief Get the next ovl in the list

  Get the next ovl in the list
*/
static ovl_ptr ovl_get_next(ovl_ptr l)
{
  nusmv_assert(ovl_isnotempty(l) == 1);
  return(l->next);
}

/*!
  \brief Sets the next ovl in the list

  Sets the next ovl in the list
*/
static ovl_ptr ovl_set_next(ovl_ptr l, ovl_ptr n)
{
  nusmv_assert(ovl_isnotempty(l) == 1);
  l->next = n;
  return(l);
}

/*!
  \brief Frees the given ovl instance

  Frees the given ovl instance
*/
static void ovl_free(ovl_ptr *l)
{
  ovl_ptr p;

  p = *l;
  while (ovl_isnotempty(p)) {
    ovl_ptr q = p;

    p = ovl_get_next(p);
    FREE(q->values);
    FREE(q);
  }
}

/*!
  \brief Checks whatever the given value is in the given list

  Checks whatever the given value is in the given list
*/
static int ovl_ispresent(ovl_ptr l, const char *value)
{
  int result = 0;

  for ( ; (ovl_isnotempty(l) && (result == 0)); l = ovl_get_next(l)) {
    result = (strcmp(l->values, value) == 0);
  }
  return(result);
}

/*!
  \brief Copy the ovl list source


*/
static ovl_ptr ovl_copy(ovl_ptr src)
{
  ovl_ptr dst, src_next;

  if (ovl_isnotempty(src)) {
    dst = ovl_create(src->values, src->valuee);

    src_next = ovl_get_next(src);
    if (ovl_isnotempty(src_next)) {
      ovl_ptr dst_next = ovl_copy(src_next);
      ovl_set_next(dst, dst_next);
    }
  }
  else {
    dst = ovl_create_empty();
  }

  return dst;
}

/*!
  \brief Check if the given list contains boolean values

  Check if the given list contains boolean values
*/
static boolean check_boolean(ovl_ptr l)
{
  int result = true;

  for ( ; (ovl_isnotempty(l) && (result == 0)); l = ovl_get_next(l)) {
    result &= ((strcmp(l->values, OPTS_TRUE_VALUE) == 0) ||
               (strcmp(l->values, OPTS_FALSE_VALUE) == 0));
  }
  return result;
}

/*!
  \brief Internal function for option registration

  Internal function for option registration
*/
static boolean opts_handler_register_generic_option(OptsHandler_ptr self,
                                                    const char* name,
                                                    const char* def,
                                                    ovl_ptr ovl,
                                                    Opts_CheckFnType check,
                                                    Opts_ReturnFnType get,
                                                    boolean is_public,
                                                    Option_Type type,
                                                    boolean user_defined,
                                                    void* arg)
{
  opt_ptr opt;
  boolean result = false;

  opt = (opt_ptr)find_assoc(self->table, NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)));

  if ((opt_ptr)NULL == opt) {
    opt = option_create(name, def, def, ovl, check, get,
                        is_public, type, user_defined, arg);

    if ((opt_ptr)NULL != opt) {
      unsigned int l;

      insert_assoc(self->table, (node_ptr)NODE_PTR( UStringMgr_find_string(self->strings, (char*)name)), (node_ptr)opt);
      result = true;

      /* Remember the longest command name, for good indentation. */
      l = strlen(name);
      if (l > self->opt_max_length) {
        self->opt_max_length = l;
      }
    }
  }
  return(result);
}

/*!
  \brief Internal function for trigger run

  Internal function for trigger run
*/
static boolean opts_handler_run_triggers(OptsHandler_ptr self, opt_ptr opt,
                                         const char* name, const char* val,
                                         Trigger_Action action)
{
  boolean result = true;
  Siter iter;

  SLIST_FOREACH(opt->triggers, iter) {
    option_trigger* trigger_struct = (option_trigger*)Siter_element(iter);
    Opts_TriggerFnType f = (Opts_TriggerFnType) trigger_struct->trigger;
    void* arg = trigger_struct->arg;

    result &= (*f)(self, name, val, action, arg);
  }

  return result;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
