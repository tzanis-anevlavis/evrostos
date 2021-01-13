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
#include "nusmv/addons_core/compass/parser/prob/probInt.h"
#include "nusmv/addons_core/compass/parser/prob/ParserProb.h"
#include "nusmv/addons_core/compass/parser/prob/ParserProb_private.h"

#include "nusmv/addons_core/compass/compile/ProbAssign.h"

#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/error.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/EnvObject.h"
#include "nusmv/core/utils/EnvObject_private.h"

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct ParserProb_TAG
{
  INHERITS_FROM(EnvObject);

  NodeList_ptr prob_list;
} ParserProb;


/*---------------------------------------------------------------------------*/
/* macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void parser_prob_init(ParserProb_ptr self, const NuSMVEnv_ptr env);
static void parser_prob_deinit(ParserProb_ptr self);
static void parser_prob_finalize(Object_ptr object, void* dummy);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

ParserProb_ptr ParserProb_create(const NuSMVEnv_ptr env)
{
  ParserProb_ptr self = ALLOC(ParserProb, 1);
  PARSER_PROB_CHECK_INSTANCE(self);

  parser_prob_init(self, env);
  return self;
}

void ParserProb_destroy(ParserProb_ptr self)
{
  PARSER_PROB_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

void ParserProb_parse_from_file(ParserProb_ptr self, FILE* f)
{
  YY_BUFFER_STATE buf;
  NuSMVEnv_ptr env;
  StreamMgr_ptr streams;

  PARSER_PROB_CHECK_INSTANCE(self);

  parser_prob_set_global_parser(self);

  env = EnvObject_get_environment(ENV_OBJECT(self));
  streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  if (f == (FILE*) NULL) parser_prob_in = StreamMgr_get_input_stream(streams);
  else parser_prob_in = f;

  buf = parser_prob__create_buffer(parser_prob_in, 16384);
  parser_prob__switch_to_buffer(buf);
  parser_prob_restart(parser_prob_in);
  parser_prob_parse();
  parser_prob__delete_buffer(buf);

  parser_prob_reset_global_parser(self);
}

void ParserProb_parse_from_string(ParserProb_ptr self, const char* str)
{
  YY_BUFFER_STATE buf;

  PARSER_PROB_CHECK_INSTANCE(self);

  parser_prob_set_global_parser(self);

  buf = parser_prob__scan_string(str);
  parser_prob__delete_buffer(buf);

  parser_prob_reset_global_parser(self);
}

NodeList_ptr ParserProb_get_prob_list(const ParserProb_ptr self)
{
  PARSER_PROB_CHECK_INSTANCE(self);
  return self->prob_list;
}

void ParserProb_reset(ParserProb_ptr self)
{
  PARSER_PROB_CHECK_INSTANCE(self);

  parser_prob_deinit(self);
  self->prob_list = NodeList_create();
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief 

  
*/

void parser_prob_add(ParserProb_ptr self, node_ptr prob)
{
  ProbAssign_ptr probass;
  PARSER_PROB_CHECK_INSTANCE(self);

  nusmv_assert(COLON == node_get_type(prob));
  probass = ProbAssign_create(car(prob), cdr(prob));

  NodeList_prepend(self->prob_list, (node_ptr) probass);
}


/*!
  \brief 

  
*/

node_ptr parser_prob_mk_prob(ParserProb_ptr self,
                           node_ptr assigns, node_ptr prob)
{
  PARSER_PROB_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
    return find_node(nodemgr, COLON, assigns, prob);
  }
}

/*!
  \brief 

  
*/

node_ptr parser_prob_mk_var_assign(ParserProb_ptr self,
                                  node_ptr var, node_ptr val)

{
  PARSER_PROB_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    return find_node(nodemgr, EQUAL, var, val);
  }
}

/*!
  \brief 

  
*/

node_ptr parser_prob_mk_var_assigns(ParserProb_ptr self,
                                   node_ptr left, node_ptr right)

{
  PARSER_PROB_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    return find_node(nodemgr, AND, left, right);
  }
}


/*!
  \brief 

  
*/

node_ptr parser_prob_mk_dot(ParserProb_ptr self, node_ptr left, node_ptr right)
{
  PARSER_PROB_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    return find_node(nodemgr, DOT, left, right);
  }
}


/*!
  \brief 

  
*/

node_ptr parser_prob_mk_array(ParserProb_ptr self, node_ptr left, node_ptr right)
{
  PARSER_PROB_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    return find_node(nodemgr, ARRAY, left, right);
  }
}


/*!
  \brief 

  
*/

node_ptr parser_prob_mk_atom(ParserProb_ptr self, const char* name)
{
  node_ptr atom;
  PARSER_PROB_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    UStringMgr_ptr strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    atom = find_node(nodemgr, ATOM, (node_ptr) UStringMgr_find_string(strings, (char*) name), Nil);
  }
  return atom;
}


/*!
  \brief 

  
*/

node_ptr parser_prob_mk_num(ParserProb_ptr self, const int num)
{
  PARSER_PROB_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    return find_node(nodemgr, NUMBER, NODE_FROM_INT(num), Nil);
  }
}


/*!
  \brief 

  
*/

node_ptr parser_prob_mk_true(ParserProb_ptr self)
{
  PARSER_PROB_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    return find_node(nodemgr, TRUEEXP, Nil, Nil);
  }
}


/*!
  \brief 

  
*/

node_ptr parser_prob_mk_false(ParserProb_ptr self)
{
  PARSER_PROB_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    return find_node(nodemgr, FALSEEXP, Nil, Nil);
  }
}


/*!
  \brief 

  
*/

node_ptr parser_prob_mk_real(ParserProb_ptr self, const char* real_text)
{
  PARSER_PROB_CHECK_INSTANCE(self);
  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    UStringMgr_ptr strings = USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

    return find_node(nodemgr, NUMBER_REAL,
                     (node_ptr) UStringMgr_find_string(strings, (char*) real_text), Nil);
  }
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief 

  
*/
static void parser_prob_init(ParserProb_ptr self, const NuSMVEnv_ptr env)
{
  env_object_init(ENV_OBJECT(self), env);

  self->prob_list = NodeList_create();

  OVERRIDE(Object, finalize) = parser_prob_finalize;
}

/*!
  \brief 

  
*/
static void parser_prob_finalize(Object_ptr object, void* dummy)
{
  ParserProb_ptr self = PARSER_PROB(object);

  parser_prob_deinit(self);

  FREE(self);
}

/*!
  \brief 

  
*/
static void parser_prob_deinit(ParserProb_ptr self)
{
  ListIter_ptr iter;
  NODE_LIST_FOREACH(self->prob_list, iter) {
    ProbAssign_destroy(PROB_ASSIGN(NodeList_get_elem_at(self->prob_list, iter)));
  }
  NodeList_destroy(self->prob_list);
  self->prob_list = NODE_LIST(NULL);
}

