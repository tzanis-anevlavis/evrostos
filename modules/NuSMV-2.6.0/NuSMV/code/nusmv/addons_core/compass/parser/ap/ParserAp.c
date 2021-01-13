/* ---------------------------------------------------------------------------


  This file is part of the ``compass.parser.ap'' package of NuSMV version 2.
  Copyright (C) 2008 by FBK-irst.

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

-----------------------------------------------------------------------------*/

/*!
  \author Marco Roveri
  \brief \todo: Missing synopsis

  \todo: Missing description

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/addons_core/compass/parser/ap/apInt.h"
#include "nusmv/addons_core/compass/parser/ap/ParserAp.h"
#include "nusmv/addons_core/compass/parser/ap/ParserAp_private.h"

#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/error.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/EnvObject.h"
#include "nusmv/core/utils/EnvObject_private.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct ParserAp_TAG
{
  INHERITS_FROM(EnvObject);

  NodeList_ptr ap_list;
} ParserAp;


/*---------------------------------------------------------------------------*/
/* macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void parser_ap_init(ParserAp_ptr self, const NuSMVEnv_ptr env);
static void parser_ap_deinit(ParserAp_ptr self);
static void parser_ap_finalize(Object_ptr object, void* dummy);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

ParserAp_ptr ParserAp_create(const NuSMVEnv_ptr env)
{
  ParserAp_ptr self = ALLOC(ParserAp, 1);
  PARSER_AP_CHECK_INSTANCE(self);

  parser_ap_init(self, env);
  return self;
}

void ParserAp_destroy(ParserAp_ptr self)
{
  PARSER_AP_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

void ParserAp_parse_from_file(ParserAp_ptr self, FILE* f)
{
  NuSMVEnv_ptr env;
  StreamMgr_ptr streams;
  YY_BUFFER_STATE buf;

  PARSER_AP_CHECK_INSTANCE(self);

  parser_ap_set_global_parser(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  if (f == (FILE*) NULL) parser_ap_in = StreamMgr_get_input_stream(streams);
  else parser_ap_in = f;

  buf = parser_ap__create_buffer(parser_ap_in, 16384);
  parser_ap__switch_to_buffer(buf);
  parser_ap_restart(parser_ap_in);
  parser_ap_parse();
  parser_ap__delete_buffer(buf);

  parser_ap_reset_global_parser(self);
}

void ParserAp_parse_from_string(ParserAp_ptr self, const char* str)
{
  YY_BUFFER_STATE buf;

  PARSER_AP_CHECK_INSTANCE(self);

  parser_ap_set_global_parser(self);

  buf = parser_ap__scan_string(str);
  parser_ap__delete_buffer(buf);

  parser_ap_reset_global_parser(self);
}

NodeList_ptr ParserAp_get_ap_list(const ParserAp_ptr self)
{
  PARSER_AP_CHECK_INSTANCE(self);
  return self->ap_list;
}

void ParserAp_reset(ParserAp_ptr self)
{
  PARSER_AP_CHECK_INSTANCE(self);

  parser_ap_deinit(self);
  self->ap_list = NodeList_create();
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief 

  
*/

void parser_ap_add(ParserAp_ptr self, node_ptr ap)
{
  node_ptr apass;

  PARSER_AP_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    nusmv_assert(COLON == node_get_type(ap));
    apass = cons(nodemgr, car(ap), cdr(ap));
  }

  NodeList_prepend(self->ap_list, (node_ptr) apass);
}


/*!
  \brief 

  
*/

node_ptr parser_ap_mk_ap(ParserAp_ptr self,
                         node_ptr label, node_ptr ap)
{
  PARSER_AP_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
    return new_node(nodemgr, COLON, label, ap);
  }
}



/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief 

  
*/
static void parser_ap_init(ParserAp_ptr self, const NuSMVEnv_ptr env)
{
  env_object_init(ENV_OBJECT(self), env);

  self->ap_list = NodeList_create();

  OVERRIDE(Object, finalize) = parser_ap_finalize;
}

/*!
  \brief 

  
*/
static void parser_ap_finalize(Object_ptr object, void* dummy)
{
  ParserAp_ptr self = PARSER_AP(object);

  parser_ap_deinit(self);

  FREE(self);
}

/*!
  \brief 

  
*/
static void parser_ap_deinit(ParserAp_ptr self)
{
  ListIter_ptr iter;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  NODE_LIST_FOREACH(self->ap_list, iter) {
    free_node(nodemgr, NodeList_get_elem_at(self->ap_list, iter));
  }
  NodeList_destroy(self->ap_list);
  self->ap_list = NODE_LIST(NULL);

  env_object_deinit(ENV_OBJECT(self));
}

