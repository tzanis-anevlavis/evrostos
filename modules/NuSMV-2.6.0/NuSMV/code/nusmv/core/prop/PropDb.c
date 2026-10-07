/* ---------------------------------------------------------------------------


  This file is part of the ``prop'' package of NuSMV version 2.
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
  \author marco Roveri, Roberto Cavada
  \brief Implementation of class 'PropDb'

  \todo: Missing description

*/


#if HAVE_CONFIG_H
#include "nusmv-config.h"
#endif

#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"

#include "nusmv/core/prop/PropDb.h"
#include "nusmv/core/prop/PropDb_private.h"
#include "nusmv/core/prop/Prop.h"
#include "nusmv/core/prop/Prop_private.h"
#include "nusmv/core/prop/propInt.h"


#include "nusmv/core/compile/compile.h"
#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/parser.h"
#include "nusmv/core/parser/psl/pslNode.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/ucmd.h"



/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'PropDb_private.h' for class 'PropDb' definition. */

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

static void prop_db_finalize(Object_ptr object, void* dummy);

static int
prop_db_prop_parse_from_arg_and_add(PropDb_ptr self,
                                    SymbTable_ptr symb_table,
                                    int argc, const char** argv,
                                    const Prop_Type type);
static const char*
prop_db_get_prop_type_as_parsing_string(PropDb_ptr self,
                                        const Prop_Type type);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

PropDb_ptr PropDb_create(NuSMVEnv_ptr env)
{
  PropDb_ptr self = ALLOC(PropDb, 1);
  PROP_DB_CHECK_INSTANCE(self);

  prop_db_init(self, env);
  return self;
}

void PropDb_destroy(PropDb_ptr self)
{
  PROP_DB_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

void PropDb_clean(PropDb_ptr self)
{
  NuSMVEnv_ptr env;

  PROP_DB_CHECK_INSTANCE(self);

  env = ENV_OBJECT(self)->environment;

  prop_db_deinit(self);
  prop_db_init(self, env);
}

int PropDb_fill(PropDb_ptr self, SymbTable_ptr symb_table,
                node_ptr ctlspec, node_ptr computespec,
                node_ptr ltlspec, node_ptr pslspec,
                node_ptr invarspec)
{
  node_ptr l;
  int res;
  Prop_ptr prop;

  PROP_DB_CHECK_INSTANCE(self);
  SYMB_TABLE_CHECK_INSTANCE(symb_table);

  /* [AM] Named specs
   * Named:
   *    [LTL;PSL;COMPUTE;INVAR;]SPEC
   *       /                 \
   *      CONTEXT            [p_name (DOT/ATOM)]
   *     /    \
   *   ...    ...
   */

  for (l = ctlspec; l != Nil; l = cdr(l)) {
    res = PropDb_prop_create_and_add(self, symb_table, car(car(l)),
                                     Prop_Ctl);
    if (res == -1) return 1;
    if (Nil != cdr(car(l))){
      prop = PropDb_get_prop_at_index(self, res);
      Prop_set_name(prop, cdr(car(l)));
    }
  }
  for (l = computespec; l != Nil; l = cdr(l)) {
    res = PropDb_prop_create_and_add(self, symb_table, car(car(l)),
                                     Prop_Compute);
    if (res == -1) return 1;
    if (Nil != cdr(car(l))){
      prop = PropDb_get_prop_at_index(self, res);
      Prop_set_name(prop, cdr(car(l)));
    }
  }
  for (l = ltlspec; l != Nil; l = cdr(l)) {
    res = PropDb_prop_create_and_add(self, symb_table, car(car(l)),
                                     Prop_Ltl);
    if (res == -1) return 1;
    if (Nil != cdr(car(l))){
      prop = PropDb_get_prop_at_index(self, res);
      Prop_set_name(prop, cdr(car(l)));
    }
  }
  for (l = pslspec; l != Nil; l = cdr(l)) {
    res = PropDb_prop_create_and_add(self, symb_table,
                                     car(car(l)), Prop_Psl);
    if (res == -1) return 1;
    if (Nil != cdr(car(l))){
      prop = PropDb_get_prop_at_index(self, res);
      Prop_set_name(prop, cdr(car(l)));
    }
  }
  for (l = invarspec; l != Nil; l = cdr(l)) {
    res = PropDb_prop_create_and_add(self, symb_table, car(car(l)),
                                     Prop_Invar);
    if (res == -1) return 1;
    if (Nil != cdr(car(l))){
      prop = PropDb_get_prop_at_index(self, res);
      Prop_set_name(prop, cdr(car(l)));
    }
  }

  return 0;
}

boolean PropDb_add(PropDb_ptr self, Prop_ptr p)
{
  PROP_DB_CHECK_INSTANCE(self);
  PROP_CHECK_INSTANCE(p);

  if (Prop_get_index(p) == -1) {
    Prop_set_index(p, PropDb_get_size(self));
  }
  return (array_insert_last(Prop_ptr, self->prop_database, p) ==
          ARRAY_OUT_OF_MEM);
}

VIRTUAL int PropDb_prop_create_and_add(PropDb_ptr self,
                                       SymbTable_ptr symb_table,
                                       node_ptr spec,
                                       Prop_Type type)
{
  PROP_DB_CHECK_INSTANCE(self);
  SYMB_TABLE_CHECK_INSTANCE(symb_table);
  return self->prop_create_and_add(self, symb_table, spec, type);
}

Prop_ptr PropDb_get_last(const PropDb_ptr self)
{
  PROP_DB_CHECK_INSTANCE(self);
  return array_fetch_last(Prop_ptr, self->prop_database);
}

Prop_ptr PropDb_get_prop_at_index(const PropDb_ptr self, int index)
{
  Prop_ptr res;
  PROP_DB_CHECK_INSTANCE(self);

  if (index >= array_n(self->prop_database)) res = PROP(NULL);
  else res = array_fetch(Prop_ptr, self->prop_database, index);

  return res;
}

Set_t PropDb_get_props_at_indices(const PropDb_ptr self,
				  const ErrorMgr_ptr errmgr,
				  const char* indices)
{
  const char* delimiters=",:";
  Set_t res = Set_MakeEmpty();
  char* _copy_indices = util_strsav(indices);
  char* token;

  for (token=strtok(_copy_indices, delimiters); token != (char*) NULL;
       token=strtok((char*) NULL, delimiters)) {
    char* dash = strchr(token, '-');
    int low, high, idx;

    if (dash == (char*) NULL) { /* no range */
      if (util_str2int(token, &low) != 0) {
        ErrorMgr_error_invalid_number(errmgr, token);
        FREE(_copy_indices);
        ErrorMgr_rpterr(errmgr, NULL);
      }
      high = low;
    }
    else { /* a range has been specified */
      *dash = '\0'; /* splits the range */
      if (util_str2int(token, &low) != 0) {
        ErrorMgr_error_invalid_number(errmgr, token);
        FREE(_copy_indices);
        Set_ReleaseSet(res);
        ErrorMgr_rpterr(errmgr, NULL);
      }
      if (util_str2int(dash+1, &high) != 0) {
        ErrorMgr_error_invalid_number(errmgr, token);
        FREE(_copy_indices);
        Set_ReleaseSet(res);
        ErrorMgr_rpterr(errmgr, NULL);
      }
    }

    if (low > high) {
      FREE(_copy_indices);
      Set_ReleaseSet(res);
      ErrorMgr_rpterr(errmgr, "Range error: %d-%d", low, high);
    }

    for (idx=low; idx <= high; ++idx) {
      Prop_ptr prop = PropDb_get_prop_at_index(self, idx);
      if (prop == (Prop_ptr) NULL) {
        FREE(_copy_indices);
        Set_ReleaseSet(res);
        ErrorMgr_rpterr(errmgr, "Property index %d is not valid (must be in the range [0,%d])",
			idx, PropDb_get_size(self)-1);
      }

      res = Set_AddMember(res, (Set_Element_t) prop);
    } /* for all property indices */
  }

  FREE(_copy_indices);
  return res;
}

int PropDb_get_prop_name_index(const PropDb_ptr self, const node_ptr name)
{
  int i;

  PROP_DB_CHECK_INSTANCE(self);

  for (i = 0; i < PropDb_get_size(self); ++i) {
    Prop_ptr prop = PropDb_get_prop_at_index(self, i);
    if (Prop_get_name(prop) == name) return i;
  }

  return -1; /* not found */
}

int PropDb_get_size(const PropDb_ptr self)
{
  PROP_DB_CHECK_INSTANCE(self);
  return array_n(self->prop_database);
}

PropDb_PrintFmt PropDb_get_print_fmt(const PropDb_ptr self)
{
  PROP_DB_CHECK_INSTANCE(self);
  return self->print_fmt;
}

PropDb_PrintFmt PropDb_set_print_fmt(const PropDb_ptr self,
                                     PropDb_PrintFmt new_fmt)
{
  PropDb_PrintFmt old;

  PROP_DB_CHECK_INSTANCE(self);

  old = self->print_fmt;
  self->print_fmt = new_fmt;
  return old;
}

void PropDb_print_list_header(const PropDb_ptr self, OStream_ptr file)
{
  PROP_DB_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    switch (PropDb_get_print_fmt(self)) {
    case PROPDB_PRINT_FMT_TABULAR:
      OStream_printf(file,
              "**** PROPERTY LIST [ Type, Status, Counter-example Number, Name ] ****\n");
      OStream_printf(file,
              "--------------------------  PROPERTY LIST  -------------------------\n");
      break;

    case PROPDB_PRINT_FMT_XML:
      OStream_printf(file, "<?xml version=\"1.0\"?>\n");
      OStream_printf(file, "<properties xmlns=\"http://es.fbk.eu\"\n");
      OStream_printf(file, "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
      OStream_printf(file, "xsi:schemaLocation=\"http://es.fbk.eu/xsd properties.xsd\">\n\n");
      break;

    default:
      ErrorMgr_internal_error(errmgr, "Unsupported prop print format");
    }
  }
}

void PropDb_print_list_footer(const PropDb_ptr self, OStream_ptr file)
{
  PROP_DB_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    switch (PropDb_get_print_fmt(self)) {
    case PROPDB_PRINT_FMT_TABULAR:
      break;

    case PROPDB_PRINT_FMT_XML:
      OStream_printf(file, "</properties>\n");
      break;

    default:
      ErrorMgr_internal_error(errmgr, "Unsupported prop print format");
    }
  }
}


/*!
  \brief Prints the specified property from the DB

  Prints on the given file stream the property
  whose unique identifier is specified
*/

int PropDb_print_prop_at_index(const PropDb_ptr self,
                               OStream_ptr file, const int index)
{
  int retval;
  Prop_ptr prop;

  PROP_DB_CHECK_INSTANCE(self);

  prop = PropDb_get_prop_at_index(self, index);
  if (prop != PROP(NULL)) {
    Prop_print_db(prop, file, PropDb_get_print_fmt(self));
    retval = 0;
  }
  else {
    retval = 1;
  }

  return retval;
}

void PropDb_print_all(const PropDb_ptr self, OStream_ptr file)
{
  PROP_DB_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    PropDb_print_all_status_type(self, file, Prop_NoStatus, Prop_NoType);

    if (PropDb_get_size(self) == 0) {
      switch (PropDb_get_print_fmt(self)) {
      case PROPDB_PRINT_FMT_TABULAR:
        OStream_printf(file, "The properties DB is empty.\n");
        break;
      case PROPDB_PRINT_FMT_XML:
        OStream_printf(file, "  <!-- The properties DB is empty. -->\n");
        break;
      default:
        ErrorMgr_internal_error(errmgr, "Invalid print format");
      }
    }
  }
}

void PropDb_print_all_status_type(const PropDb_ptr self, OStream_ptr file,
                                  Prop_Status status, Prop_Type type)
{
  int i;

  PROP_DB_CHECK_INSTANCE(self);

  for (i = 0; i < PropDb_get_size(self); ++i) {
    Prop_ptr p = PropDb_get_prop_at_index(self, i);

    if (((type == Prop_NoType) || (Prop_get_type(p) == type)) &&
        ((status == Prop_NoStatus) || (Prop_get_status(p) == status))) {
      Prop_print_db(p, file, PropDb_get_print_fmt(self));
    }
  }
}

void PropDb_print_all_type(const PropDb_ptr self, OStream_ptr file, Prop_Type type)
{
  PROP_DB_CHECK_INSTANCE(self);

  PropDb_print_all_status_type(self, file, Prop_NoStatus, type);
}

void PropDb_print_all_status(const PropDb_ptr self,
                             OStream_ptr file, Prop_Status status)
{
  PROP_DB_CHECK_INSTANCE(self);

  PropDb_print_all_status_type(self, file, status, Prop_NoType);
}

lsList PropDb_get_ordered_props_of_type(const PropDb_ptr self,
                                        const FlatHierarchy_ptr hierarchy,
                                        const Prop_Type type)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  NodeList_ptr list;
  lsList result;
  ListIter_ptr iter;

  PROP_DB_CHECK_INSTANCE(self);

  result = lsCreate();
  nusmv_assert((lsList) NULL != result);

  list = PropDb_get_ordered_properties(self, hierarchy);
  NODE_LIST_FOREACH(list, iter) {
    node_ptr couple = NodeList_get_elem_at(list, iter);
    Prop_ptr prop = PROP(car(couple));

    if (Prop_get_type(prop) == type) {
      lsNewEnd(result, (lsGeneric)prop, LS_NH);
    }

    Set_ReleaseSet((Set_t)cdr(couple));
    free_node(nodemgr, couple);
  }

  NodeList_destroy(list);
  return result;
}

lsList PropDb_get_props_of_type(const PropDb_ptr self, const Prop_Type type)
{
  lsList result;
  int i;

  PROP_DB_CHECK_INSTANCE(self);

  result = lsCreate();
  nusmv_assert((lsList) NULL != result);

  for (i=0; i < PropDb_get_size(self); ++i) {
    Prop_ptr p = PropDb_get_prop_at_index(self, i);

    if (Prop_get_type(p) == type) {
      lsNewEnd(result, (lsGeneric)p, LS_NH);
    }
  }

  return result;
}

int PropDb_prop_parse_and_add(const PropDb_ptr self,
                              SymbTable_ptr symb_table,
                              const char* str,
                              const Prop_Type type,
                              const node_ptr expr_name)
{
  const char* argv[2];
  int argc = 2;
  int res;

  PROP_DB_CHECK_INSTANCE(self);
  nusmv_assert(str != (char*) NULL);

  argv[0] = (char*) NULL;
  argv[1] = (char*) str;

  res = prop_db_prop_parse_from_arg_and_add(self, symb_table,
                                            argc, argv, type);

  if (-1 == res) return -1;
  else if (Nil != expr_name) {
    Prop_ptr property = PropDb_get_prop_at_index(self, res);

    Prop_set_name(property, expr_name);
  }

  return res;
}

int PropDb_prop_parse_name(const PropDb_ptr self, const char* str)
{
  StreamMgr_ptr streams;
  node_ptr property;
  node_ptr parsed_command = Nil;
  int parse_result;
  NuSMVEnv_ptr env;

  PROP_DB_CHECK_INSTANCE(self);
  env = EnvObject_get_environment(ENV_OBJECT(self));
  streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  nusmv_assert(str != (char*) NULL);

  parse_result =
    Parser_ReadIdentifierExprFromString(env, str, &parsed_command);

  if (parse_result != 0 || parsed_command == Nil) {
    StreamMgr_print_error(streams,
            "Parsing error: expected a property name.\n");
    return -1;
  }

  property = car(parsed_command);
  property = CompileFlatten_concat_contexts(env, Nil, property);

  return PropDb_get_prop_name_index(self, property);
}

int PropDb_get_prop_index_from_string(const PropDb_ptr self, const char* idx)
{
  int idxTemp, db_size;
  NuSMVEnv_ptr env;
  StreamMgr_ptr streams;

  PROP_DB_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  db_size = PropDb_get_size(self);
  if ( db_size <= 0 ) {
    if (cmp_struct_get_flatten_hrc(cmps) == 0) {
      StreamMgr_print_error(streams,
              "The hierarchy must be flattened before. "\
              "Use the \"flatten_hierarchy\" command.\n");
    }
    else {
      StreamMgr_print_error(streams, "Error: there isn\'t any property available.\n");
    }
    return -1;
  }

  if (util_str2int(idx, &idxTemp) != 0) {
    StreamMgr_print_error(streams,
            "Error: property index \"%s\" is not a valid value "\
            "(must be integer).\n", idx);
    return -1;
  }

  if ( (idxTemp < 0) || (idxTemp >= db_size) ) {
    StreamMgr_print_error(streams,
            "Error: property index \"%d\" is not valid (must be in "\
            "the range [0,%d]).\n",
            idxTemp, db_size-1);
    return -1;
  }

  return idxTemp;
}

int PropDb_get_prop_index_from_trace_index(const PropDb_ptr self,
                                           const int trace_idx)
{
  int i, result;

  PROP_DB_CHECK_INSTANCE(self);

  result = -1;
  for (i=0; i < PropDb_get_size(self) && result == -1; ++i) {
    Prop_ptr prop = PropDb_get_prop_at_index(self, i);

    if (Prop_get_trace(prop) == (trace_idx + 1)) {
      result = i;
    }
  }
  return result;
}

void PropDb_verify_prop_at_index(const PropDb_ptr self, const int index)
{
  int size;
  PROP_DB_CHECK_INSTANCE(self);

  size = PropDb_get_size(self);
  if (size < index) {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
    const StreamMgr_ptr streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

    StreamMgr_print_error(streams,
            "Property indexed by %d not present in the database.\n", index);
    StreamMgr_print_error(streams,
            "Valid index are in the range [0..%d]\n", size-1);
    ErrorMgr_nusmv_exit(errmgr, 1);
  }
  else {
    Prop_ptr p = PropDb_get_prop_at_index(self, index);
    Prop_verify(p);
  }
}

void PropDb_verify_all_type(const PropDb_ptr self, Prop_Type type)
{
  int i;
  PROP_DB_CHECK_INSTANCE(self);

  for (i=0; i < PropDb_get_size(self); ++i) {
    Prop_ptr p = PropDb_get_prop_at_index(self, i);
    if (Prop_get_type(p) == type) Prop_verify(p);
  }
}

VIRTUAL void PropDb_verify_all(const PropDb_ptr self)
{
  PROP_DB_CHECK_INSTANCE(self);
  self->verify_all(self);
}

void PropDb_ordered_verify_all(const PropDb_ptr self,
                               const FlatHierarchy_ptr hierarchy)
{
  PROP_DB_CHECK_INSTANCE(self);
  PropDb_ordered_verify_all_type(self, hierarchy, Prop_NoType);
}

void PropDb_ordered_verify_all_type(const PropDb_ptr self,
                                    const FlatHierarchy_ptr hierarchy,
                                    const Prop_Type type)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  NodeList_ptr list;
  ListIter_ptr iter;

  PROP_DB_CHECK_INSTANCE(self);

  list = PropDb_get_ordered_properties(self, hierarchy);
  NODE_LIST_FOREACH(list, iter) {
    node_ptr couple = NodeList_get_elem_at(list, iter);
    Prop_ptr prop = PROP(car(couple));

    if ((Prop_NoType == type) || (Prop_get_type(prop) == type)) {
      Prop_verify(prop);
    }

    Set_ReleaseSet((Set_t)cdr(couple));
    free_node(nodemgr, couple);
  }

  NodeList_destroy(list);
}

NodeList_ptr PropDb_get_ordered_properties(const PropDb_ptr self,
                                           const FlatHierarchy_ptr hierarchy)
{
  const NuSMVEnv_ptr env = ENV_OBJECT(self)->environment;
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
const NodeMgr_ptr nodemgr =
  NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  SymbTable_ptr symb_table;
  NodeList_ptr res;
  int i;

  PROP_DB_CHECK_INSTANCE(self);

  symb_table = FlatHierarchy_get_symb_table(hierarchy);
  res = NodeList_create();

  if (opt_verbose_level_ge(opts, 2)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Ordering properties by COI size\n");
  }

  for (i = 0; i < PropDb_get_size(self); ++i) {
    Prop_ptr prop = PropDb_get_prop_at_index(self, i);
    Set_t cone = Prop_compute_cone(prop, hierarchy, symb_table);
    int card = Set_GiveCardinality(cone);
    boolean inserted = false;
    /* Order insert into the list */
    node_ptr new_entry = cons(nodemgr, NODE_PTR(prop), NODE_PTR(cone));

    ListIter_ptr iter;
    NODE_LIST_FOREACH(res, iter) {
      node_ptr entry = NodeList_get_elem_at(res, iter);
      Set_t entry_cone = (Set_t)cdr(entry);

      if (Set_GiveCardinality(entry_cone) > card) {
        NodeList_insert_before(res, iter, new_entry);
        inserted = true;
        break;
      }
    }

    if (!inserted) NodeList_append(res, new_entry);
  }

  if (opt_verbose_level_ge(opts, 2)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "Properties ordering done\n");
  }

  return res;
}

NodeList_ptr
PropDb_get_coi_grouped_properties(const PropDb_ptr self,
                                  const FlatHierarchy_ptr hierarchy)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  NodeList_ptr result, order;
  ListIter_ptr iter;

  PROP_DB_CHECK_INSTANCE(self);

  result = NodeList_create();
  order = PropDb_get_ordered_properties(self, hierarchy);
  NODE_LIST_FOREACH(order, iter) {
    boolean found = false;
    ListIter_ptr res_iter;
    node_ptr entry = NodeList_get_elem_at(order, iter);
    Set_t cone = (Set_t)cdr(entry);
    Prop_ptr prop = PROP(car(entry));

    NODE_LIST_FOREACH(result, res_iter) {
      node_ptr res_entry = NodeList_get_elem_at(result, res_iter);
      Set_t props = (Set_t)cdr(res_entry);
      Set_t res_cone = (Set_t)car(res_entry);
      if (Set_Equals(res_cone, cone)) {
        props = Set_AddMember(props, (Set_Element_t)prop);
        setcdr(res_entry, NODE_PTR(props));
        found = true;
        break;
      }
    }

    if (!found) {
      Set_t props = Set_MakeSingleton((Set_Element_t)prop);
      node_ptr new_entry = cons(nodemgr, NODE_PTR(cone), NODE_PTR(props));
      NodeList_append(result, new_entry);
    }
    else {
      Set_ReleaseSet(cone);
    }

    free_node(nodemgr, entry);
  }

  return result;
}

int PropDb_show_property(const PropDb_ptr self,
                         const boolean print_props_num,
                         const PropDb_PrintFmt fmt,
                         const Prop_Type type,
                         const Prop_Status status,
                         const int prop_no,
                         FILE* outstream)
{
  const NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  const StreamMgr_ptr const streams =
   STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  int retval = 0;

  if (print_props_num) {
    StreamMgr_print_output(streams,  "Current number of stored properties: %d\n",
                           PropDb_get_size(self));
    retval = 0;
  }
  else {
    PropDb_PrintFmt old_fmt = PropDb_set_print_fmt(self, fmt);
    OStream_ptr output = OStream_create(outstream);

    PropDb_print_list_header(self, output);
    if ((type == Prop_NoType) && (status == Prop_NoStatus) && (prop_no == -1)) {
      PropDb_print_all(self, output);
    } else if ((type != Prop_NoType) && (status != Prop_NoStatus)) {
      PropDb_print_all_status_type(self, output, status, type);
    } else if ((type != Prop_NoType) && (status == Prop_NoStatus)) {
      PropDb_print_all_type(self, output, type);
    } else if ((type == Prop_NoType) && (status != Prop_NoStatus)) {
      PropDb_print_all_status(self, output, status);
    } else if (prop_no != -1) {
      retval = PropDb_print_prop_at_index(self, output, prop_no);
    }
    PropDb_print_list_footer(self, output);

    PropDb_set_print_fmt(self, old_fmt);

    /* caller is going to take care of the stream, so use the
       destroy_safe method */
    OStream_destroy_safe(output);
  }

  return retval;
}

int PropDb_check_property(const PropDb_ptr self,
                          const Prop_Type pt,
                          const char* formula,
                          const int prop_no)
{
  const NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr const errmgr =
   ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (pt != Prop_NoType) {
    if (formula == NIL(char)) {
      CATCH(errmgr) {
        PropDb_verify_all_type_wrapper(self, pt);
      }
      FAIL(errmgr) {
        return(1);
      }
    }
    else {
      int result;
      SymbTable_ptr st = SYMB_TABLE(NuSMVEnv_get_value(env, ENV_SYMB_TABLE));
      result = PropDb_prop_parse_and_add(self, st, formula, pt, Nil);
      if (result == -1) return(1);
      PropDb_verify_prop_at_index(self, result);
    }
  }
  else {
    if (prop_no == -1) {
      CATCH(errmgr) {
        if (opt_use_coi_size_sorting(opts)) {
          FlatHierarchy_ptr hierarchy =
            FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));

          PropDb_ordered_verify_all(self, hierarchy);
        }
        else PropDb_verify_all(self);
      } FAIL(errmgr) {
        return(1);
      }
    }
    else {
      CATCH(errmgr) {
        PropDb_verify_prop_at_index(self, prop_no);
      }
      FAIL(errmgr) {
        return(1);
      }
    }
  }

  return(0);
}

lsList PropDb_prepare_prop_list(const PropDb_ptr self, const Prop_Type type)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const OptsHandler_ptr opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  lsList props;

  if (opt_use_coi_size_sorting(opts)) {
    FlatHierarchy_ptr hierarchy =
      FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));

    props = PropDb_get_ordered_props_of_type(self, hierarchy, type);
  }
  else props = PropDb_get_props_of_type(self, type);

  nusmv_assert(props != LS_NIL);

  return props;
}

void PropDb_verify_all_type_wrapper(PropDb_ptr const self,
                                    const Prop_Type type)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const OptsHandler_ptr opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  if (opt_use_coi_size_sorting(opts)) {
    FlatHierarchy_ptr hierarchy =
      FLAT_HIERARCHY(NuSMVEnv_get_value(env, ENV_FLAT_HIERARCHY));
    PropDb_ordered_verify_all_type(self, hierarchy, type);
  }
  else {
    PropDb_verify_all_type(self, type);
  }
}

boolean PropDb_is_prop_registered(PropDb_ptr self,
                                  Prop_ptr prop)
{
  int prop_index = 0;
  Prop_ptr retrieved_prop = NULL;

  prop_index = Prop_get_index(prop);
  /* can't be registered */
  if (prop_index < 0) return false;

  retrieved_prop = PropDb_get_prop_at_index(self, prop_index);
  /* prop is not equal to the correspondeing property in the database */
  if (retrieved_prop != prop) return false;

  return true;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void prop_db_init(PropDb_ptr self, NuSMVEnv_ptr env)
{
  /* base class initialization */
  env_object_init(ENV_OBJECT(self), env);

  /* members initialization */
  self->prop_database = array_alloc(Prop_ptr, 1);
  assert((array_t*) NULL != self->prop_database);

  self->print_fmt = PROPDB_PRINT_FMT_DEFAULT;

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = prop_db_finalize;
  OVERRIDE(PropDb, prop_create_and_add) = prop_db_prop_create_and_add;
  OVERRIDE(PropDb, verify_all) = prop_db_verify_all;
}

void prop_db_deinit(PropDb_ptr self)
{
  /* members deinitialization */
  int i;

  for (i = 0; i < PropDb_get_size(self); ++i) {
    Prop_ptr prop = PropDb_get_prop_at_index(self, i);
    Prop_destroy(prop);
  }
  array_free(self->prop_database);

  /* base class deinitialization */
  env_object_deinit(ENV_OBJECT(self));
}

int prop_db_prop_create_and_add(PropDb_ptr self, SymbTable_ptr symb_table,
                                node_ptr spec, Prop_Type type)
{
  const NuSMVEnv_ptr env = ENV_OBJECT(self)->environment;
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int retval, index;
  boolean allow_adding, allow_checking, is_ctl;
  Prop_ptr prop;

  retval = 0;
  index = PropDb_get_size(self);
  allow_adding = true;
  allow_checking = true;
  is_ctl = (type == Prop_Ctl);
  prop = NULL;

  /* PSL properties need to be converted to CTL or LTL specifications */
  if (type == Prop_Psl) {
    PslNode_ptr psl_prop = PslNode_convert_from_node_ptr(spec);
    /* removal of forall */
    psl_prop = PslNode_remove_forall_replicators(env, psl_prop);
    if (!PslNode_is_handled_psl(env, psl_prop)) {
      /* here the property may be either OBE or unmanageable */
      if (PslNode_is_obe(psl_prop)) is_ctl = true;
      else {
        /* it is not supported */
        ErrorMgr_warning_psl_not_supported_feature(errmgr, spec, index);
        allow_checking = false;
      }
    }
  }

  prop = Prop_create_partial(ENV_OBJECT(self)->environment, spec, type);

  Prop_set_index(prop, index);

  if (allow_checking) {
    nusmv_assert(SYMB_TABLE(NULL) != symb_table);

    if (!TypeChecker_check_property(SymbTable_get_type_checker(symb_table),
                                    prop)) {
      const OStream_ptr oerr = StreamMgr_get_error_ostream(streams);

      StreamMgr_print_error(streams,  "ERROR: Property \"");
      Prop_print(prop, oerr,
                 get_prop_print_method(opts));
      StreamMgr_print_error(streams,  "\b\" is not correct or not well typed.\n");

      return -1; /* type violation */
    }

    /* Checks for input vars */
    if (is_ctl || (type == Prop_Compute)) {
      Set_t expr_vars;

      if (opt_verbose_level_gt(opts, 5)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger,
                "Checking %s property (index %d) for input variables. \n",
                Prop_get_type_as_string(prop), index);
      }

      /* Get list of variables in the expression, and check for inputs */
      expr_vars = Formula_GetDependencies(symb_table,
                                          Prop_get_expr_core(prop),
                                          Nil);

      allow_adding = !SymbTable_list_contains_input_var(symb_table, Set_Set2List(expr_vars));
      Set_ReleaseSet(expr_vars);
    }

    /* Check for next operators. Only invarspecs and ltl can contain next
       operators */
    {
      node_ptr core = Prop_get_expr_core(prop);
      node_ptr context = car(core);
      node_ptr body = cdr(core);

      if (Prop_Invar == type || Prop_Ltl == type) {
        Compile_check_next(symb_table, body, context, true);

        Compile_check_input_next(symb_table, body, context);
      }
      else {
        Compile_check_next(symb_table, body, context, false);
      }
    }

  }

  /* If no input vars present then add property to database */
  if (allow_adding) {
    if (opt_verbose_level_gt(opts, 3)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger,
               "Attempting to add %s property (index %d) to property list.\n",
               Prop_get_type_as_string(prop), index);
    }
    retval = PropDb_add(self, prop);

    if (opt_verbose_level_gt(opts, 3)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      if (retval == 1) {
        Logger_log(logger, \
                "Failing to add %s property (index %d) to property list.\n", \
                Prop_get_type_as_string(prop), index);
      }
      else {
        Logger_log(logger, \
                "%s property (index %d) successfully added to property list.\n",\
                Prop_get_type_as_string(prop), index);
      }
    }
  }
  else {
    /* Property contains input variables */
    ErrorMgr_error_property_contains_input_vars(errmgr, prop);
  }

  retval = (retval == 1) ? -1 : index;
  return retval;
}

void prop_db_verify_all(const PropDb_ptr self)
{
  PropDb_verify_all_type(self, Prop_Ctl);
  PropDb_verify_all_type(self, Prop_Compute);
  PropDb_verify_all_type(self, Prop_Ltl);
  PropDb_verify_all_type(self, Prop_Psl);
  PropDb_verify_all_type(self, Prop_Invar);
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The PropDb class virtual finalizer

  Called by the class destructor
*/
static void prop_db_finalize(Object_ptr object, void* dummy)
{
  PropDb_ptr self = PROP_DB(object);

  UNUSED_PARAM(dummy);

  prop_db_deinit(self);
  FREE(self);
}

/*!
  \brief Add a property to the database from an arg structure
  and a type

  Parses and creates a property of a given type from
  an arg structure. If the formula is correct, it is added to the
  property database and its index is returned.
  Otherwise, -1 is returned.
  Valid types are Prop_Ctl, Prop_Ltl, Prop_Psl, Prop_Invar and Prop_Compute.
*/
static int
prop_db_prop_parse_from_arg_and_add(PropDb_ptr self,
                                    SymbTable_ptr symb_table,
                                    int argc, const char** argv,
                                    const Prop_Type type)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  NodeMgr_ptr nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  switch (type) {
  case Prop_Ctl:
  case Prop_Ltl:
  case Prop_Psl:
  case Prop_Invar:
  case Prop_Compute:
    /* All ok */
    break;

    /* Property name given as command argument, should use PropDb_prop_parse_name */
  case Prop_CompId:
    {
      StreamMgr_print_error(streams,  "Required to parse a property of Prop_CompId. "
              "Use PropDb_prop_parse_name instead\n");
      return -1;
    }
    break;

  case Prop_NoType:
    StreamMgr_print_error(streams,  "Required to parse a property of unknonw type.\n");
    return -1;
    break;

  default:
    StreamMgr_print_error(streams,  "Required to parse a property of unsupported type.\n");
    return -1;
    break;
  } /* switch */

  {
    node_ptr property;
    node_ptr parsed_command = Nil;

    if (type != Prop_Psl) {
      const char* parsing_type =
        prop_db_get_prop_type_as_parsing_string(self, type);
      int parse_result = Parser_ReadCmdFromString(env,
                                                  argc, argv,
                                                  (char*) parsing_type,
                                                  ";\n", &parsed_command);

      if (parse_result != 0 || parsed_command == Nil) {
        StreamMgr_print_error(streams,
                "Parsing error: expected an \"%s\" expression.\n",
                PropType_to_string(type));
        return -1;
      }
      property = car(parsed_command);
    }
    else {
      int parse_result = Parser_read_psl_from_string(env,
                                                     argc, argv,
                                                     &parsed_command);
      if (parse_result != 0 || parsed_command == Nil) {
        StreamMgr_print_error(streams,
                "Parsing error: expected an \"%s\" expression.\n",
                PropType_to_string(type));
        return -1;
      }
      /* makes possible context absolute */
      if (node_get_type(parsed_command) == CONTEXT) {
        node_ptr new_ctx = CompileFlatten_concat_contexts(env, Nil, car(parsed_command));
        property = PslNode_new_context(nodemgr, new_ctx, cdr(parsed_command));
      }
      else {
        property = PslNode_new_context(nodemgr, NULL, parsed_command);
      }
    }

    return PropDb_prop_create_and_add(self, symb_table, property, type);
  }
}

/*!
  \brief Returns the parsing type given the property type

  Returns the parsing type given the property type.
  The returned string must NOT be freed.
*/
static const char*
prop_db_get_prop_type_as_parsing_string(PropDb_ptr self, const Prop_Type type)
{
  UNUSED_PARAM(self);

  switch (type) {
  case Prop_NoType: break; /* to suppress compiler's warnings */
  case Prop_Ctl: return "CTLWFF ";
  case Prop_Ltl: return "LTLWFF ";
  case Prop_Psl: return "PSLWFF ";
  case Prop_Invar: return "NEXTWFF ";
  case Prop_Compute: return "COMPWFF ";
  case Prop_CompId:  return "COMPID ";
  default: break; /* to suppress compiler's warnings */
  }

  return "SIMPWFF ";
}


/**AutomaticEnd***************************************************************/
