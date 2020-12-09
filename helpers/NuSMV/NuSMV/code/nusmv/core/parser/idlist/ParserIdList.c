/* ---------------------------------------------------------------------------


  This file is part of the ``parser.idlist'' package of NuSMV version 2.
  Copyright (C) 2006 by FBK-irst.

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
#include "nusmv/core/parser/idlist/idlist_int.h"
#include "nusmv/core/parser/idlist/ParserIdList.h"
#include "nusmv/core/parser/idlist/ParserIdList_private.h"

#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/EnvObject_private.h"
#include "nusmv/core/parser/symbols.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct ParserIdList_TAG
{
  INHERITS_FROM(EnvObject);

  NodeList_ptr id_list;
} ParserIdList;


/*---------------------------------------------------------------------------*/
/* macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void parser_id_list_init(ParserIdList_ptr self, const NuSMVEnv_ptr env);
static void parser_id_list_deinit(ParserIdList_ptr self);
static void parser_id_list_finalize(Object_ptr object, void* dummy);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

ParserIdList_ptr ParserIdList_create(const NuSMVEnv_ptr env)
{
  ParserIdList_ptr self = ALLOC(ParserIdList, 1);
  PARSER_ID_LIST_CHECK_INSTANCE(self);

  parser_id_list_init(self, env);
  return self;
}

void ParserIdList_destroy(ParserIdList_ptr self)
{
  PARSER_ID_LIST_CHECK_INSTANCE(self);

  parser_id_list_deinit(self);
  FREE(self);
}

void ParserIdList_parse_from_file(ParserIdList_ptr self, FILE* f)
{
  NuSMVEnv_ptr env;
  ErrorMgr_ptr errmgr;
  StreamMgr_ptr streams;
  YY_BUFFER_STATE buf;

  PARSER_ID_LIST_CHECK_INSTANCE(self);

  parser_idlist_set_global_parser(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  errmgr = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  if (f == (FILE*) NULL) parser_idlist_in = StreamMgr_get_input_stream(streams);
  else parser_idlist_in = f;

  buf = parser_idlist__create_buffer(parser_idlist_in, 16384);
  parser_idlist__switch_to_buffer(buf);
  parser_idlist_restart(parser_idlist_in);

  CATCH(errmgr) { parser_idlist_parse(); }
  FAIL(errmgr) {
    parser_idlist__delete_buffer(buf);
    parser_idlist_reset_global_parser(self);
    ErrorMgr_nusmv_exit(errmgr, 1);
  }

  parser_idlist__delete_buffer(buf);
  parser_idlist_reset_global_parser(self);
}

void ParserIdList_parse_from_string(ParserIdList_ptr self, const char* str)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  YY_BUFFER_STATE buf = NULL;

  PARSER_ID_LIST_CHECK_INSTANCE(self);

  parser_idlist_set_global_parser(self);

  CATCH(errmgr) {
    buf = parser_idlist__scan_string(str);
    (void)parser_idlist_parse();
  }
  FAIL(errmgr) {
    if (((YY_BUFFER_STATE) NULL) != buf) {
        parser_idlist__delete_buffer(buf);
      }
    parser_idlist_reset_global_parser(self);
    ErrorMgr_nusmv_exit(errmgr, 1);
  }

  parser_idlist__delete_buffer(buf);
  parser_idlist_reset_global_parser(self);
}

NodeList_ptr ParserIdList_get_id_list(const ParserIdList_ptr self)
{
  PARSER_ID_LIST_CHECK_INSTANCE(self);
  return self->id_list;
}

void ParserIdList_reset(ParserIdList_ptr self)
{
  PARSER_ID_LIST_CHECK_INSTANCE(self);

  parser_id_list_deinit(self);
  parser_id_list_init(self, EnvObject_get_environment(ENV_OBJECT(self)));
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void parser_id_list_add_id(ParserIdList_ptr self, node_ptr name)
{
  PARSER_ID_LIST_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    if (NodeList_belongs_to(self->id_list, name)) {
      ErrorMgr_warning_id_appears_twice_in_idlist_file(errmgr, name);
    }
    else {
      NodeList_prepend(self->id_list, name);
    }
  }
}

node_ptr parser_id_list_mk_dot(ParserIdList_ptr self, node_ptr left,
                               node_ptr right)
{
  PARSER_ID_LIST_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
    return find_node(nodemgr, DOT, left, right);
  }
}

node_ptr parser_id_list_mk_array(ParserIdList_ptr self, node_ptr left,
                                 node_ptr right)
{
  PARSER_ID_LIST_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    return find_node(nodemgr, ARRAY, left, right);
  }
}

node_ptr parser_id_list_mk_bit(ParserIdList_ptr self, node_ptr left,
                               int suffix)
{
  PARSER_ID_LIST_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
    return find_node(nodemgr, BIT, left, NODE_FROM_INT(suffix));
  }
}

node_ptr parser_id_list_mk_atom(ParserIdList_ptr self, const char* name)
{
  NodeMgr_ptr nodemgr;
  node_ptr atom;
  NuSMVEnv_ptr env;
   UStringMgr_ptr strings;

  PARSER_ID_LIST_CHECK_INSTANCE(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
  nodemgr = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  atom = find_node(nodemgr, ATOM, (node_ptr) UStringMgr_find_string(strings, (char*) name), Nil);
  return atom;
}

node_ptr parser_id_list_mk_num(ParserIdList_ptr self, const int num)
{
  PARSER_ID_LIST_CHECK_INSTANCE(self);
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
static void parser_id_list_init(ParserIdList_ptr self, const NuSMVEnv_ptr env)
{
  env_object_init(ENV_OBJECT(self), env);

  self->id_list = NodeList_create();

  OVERRIDE(Object, finalize) = parser_id_list_finalize;
}

/*!
  \brief 

  
*/
static void parser_id_list_finalize(Object_ptr object, void* dummy)
{
  ParserIdList_ptr self = PARSER_ID_LIST(object);

  UNUSED_PARAM(dummy);

  parser_id_list_deinit(self);

  FREE(self);
}

/*!
  \brief 

  
*/
static void parser_id_list_deinit(ParserIdList_ptr self)
{
  NodeList_destroy(self->id_list);

  env_object_deinit(ENV_OBJECT(self));
}

