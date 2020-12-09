/* ---------------------------------------------------------------------------


  This file is part of the ``parser.ord'' package of NuSMV version 2.
  Copyright (C) 2003 by FBK-irst.

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
  \brief \todo: Missing synopsis

  \todo: Missing description

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/parser/ord/ordInt.h"
#include "nusmv/core/parser/ord/ParserOrd.h"
#include "nusmv/core/parser/ord/ParserOrd_private.h"

#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/error.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/EnvObject.h"
#include "nusmv/core/utils/EnvObject_private.h"


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct ParserOrd_TAG
{
  INHERITS_FROM(EnvObject);

  NodeList_ptr vars_list;
} ParserOrd;


/*---------------------------------------------------------------------------*/
/* macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void parser_ord_init(ParserOrd_ptr self, const NuSMVEnv_ptr env);
static void parser_ord_deinit(ParserOrd_ptr self);
static void parser_ord_finalize(Object_ptr object, void* dummy);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

ParserOrd_ptr ParserOrd_create(const NuSMVEnv_ptr env)
{
  ParserOrd_ptr self = ALLOC(ParserOrd, 1);
  PARSER_ORD_CHECK_INSTANCE(self);

  parser_ord_init(self, env);
  return self;
}

void ParserOrd_destroy(ParserOrd_ptr self)
{
  PARSER_ORD_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

void ParserOrd_parse_from_file(ParserOrd_ptr self, FILE* f)
{
  StreamMgr_ptr streams;
  NuSMVEnv_ptr env;
  YY_BUFFER_STATE buf;

  PARSER_ORD_CHECK_INSTANCE(self);

  parser_ord_set_global_parser(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  if (f == (FILE*) NULL) parser_ord_in = StreamMgr_get_input_stream(streams);
  else parser_ord_in = f;

  buf = parser_ord__create_buffer(parser_ord_in, 16384);
  parser_ord__switch_to_buffer(buf);
  parser_ord_restart(parser_ord_in);
  (void)parser_ord_parse();
  parser_ord__delete_buffer(buf);

  parser_ord_reset_global_parser(self);
}

void ParserOrd_parse_from_string(ParserOrd_ptr self, const char* str)
{
  YY_BUFFER_STATE buf;

  PARSER_ORD_CHECK_INSTANCE(self);

  parser_ord_set_global_parser(self);

  buf = parser_ord__scan_string(str);
  (void)parser_ord_parse();
  parser_ord__delete_buffer(buf);

  parser_ord_reset_global_parser(self);
}

NodeList_ptr ParserOrd_get_vars_list(const ParserOrd_ptr self)
{
  PARSER_ORD_CHECK_INSTANCE(self);
  return self->vars_list;
}

void ParserOrd_reset(ParserOrd_ptr self)
{
  PARSER_ORD_CHECK_INSTANCE(self);

  NodeList_destroy(self->vars_list);
  self->vars_list = NodeList_create();
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void parser_ord_add_var(ParserOrd_ptr self, node_ptr name)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  PARSER_ORD_CHECK_INSTANCE(self);

  if (NodeList_belongs_to(self->vars_list, name)) {
    ErrorMgr_warning_var_appear_twice_in_order_file(errmgr, name);
  }
  else {
    NodeList_prepend(self->vars_list, name);
  }
}

node_ptr parser_ord_mk_dot(ParserOrd_ptr self, node_ptr left, node_ptr right)
{
  PARSER_ORD_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    return find_node(nodemgr, DOT, left, right);
  }
}

node_ptr parser_ord_mk_array(ParserOrd_ptr self, node_ptr left, node_ptr right)
{
  PARSER_ORD_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    return find_node(nodemgr, ARRAY, left, right);
  }
}

node_ptr parser_ord_mk_bit(ParserOrd_ptr self, node_ptr left, int suffix)
{
  PARSER_ORD_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    return find_node(nodemgr, BIT, left, NODE_FROM_INT(suffix));
  }
}

node_ptr parser_ord_mk_atom(ParserOrd_ptr self, const char* name)
{
  NuSMVEnv_ptr env;
  UStringMgr_ptr strings;
  NodeMgr_ptr nodemgr;
  node_ptr atom;

  PARSER_ORD_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
  nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  atom = find_node(nodemgr, ATOM, (node_ptr) UStringMgr_find_string(strings, (char*) name), Nil);
  return atom;
}

node_ptr parser_ord_mk_num(ParserOrd_ptr self, const int num)
{
  PARSER_ORD_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
    return find_node(nodemgr, NUMBER, NODE_FROM_INT(num), Nil);
  }
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief 

  
*/
static void parser_ord_init(ParserOrd_ptr self, const NuSMVEnv_ptr env)
{
  env_object_init(ENV_OBJECT(self), env);

  self->vars_list = NodeList_create();

  OVERRIDE(Object, finalize) = parser_ord_finalize;
}

/*!
  \brief 

  
*/
static void parser_ord_deinit(ParserOrd_ptr self)
{
  NodeList_destroy(self->vars_list);

  env_object_deinit(ENV_OBJECT(self));
}

/*!
  \brief 

  
*/
static void parser_ord_finalize(Object_ptr object, void* dummy)
{
  ParserOrd_ptr self = PARSER_ORD(object);

  UNUSED_PARAM(dummy);

  parser_ord_deinit(self);

  FREE(self);
}

