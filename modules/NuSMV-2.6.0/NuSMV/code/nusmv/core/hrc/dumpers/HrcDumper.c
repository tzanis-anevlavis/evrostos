/* ---------------------------------------------------------------------------


  This file is part of the ``hrc.dumpers'' package of NuSMV version 2.
  Copyright (C) 2011 by FBK-irst.

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
  \brief Implementation of class 'HrcDumper'

  \todo: Missing description

*/


#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/hrc/dumpers/HrcDumper.h"
#include "nusmv/core/hrc/dumpers/HrcDumper_private.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'HrcDumper_private.h' for class 'HrcDumper' definition. */

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
#define HRC_DEFAULT_INDENT_SIZE 4


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void hrc_dumper_finalize(Object_ptr object, void* dummy);

static void
hrc_dumper_dump_scalar_type(HrcDumper_ptr self, node_ptr node);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

HrcDumper_ptr HrcDumper_create(const NuSMVEnv_ptr env,
                               FILE* fout)
{
  HrcDumper_ptr self = ALLOC(HrcDumper, 1);
  HRC_DUMPER_CHECK_INSTANCE(self);

  hrc_dumper_init(self, env, fout);
  return self;
}

void HrcDumper_destroy(HrcDumper_ptr self)
{
  HRC_DUMPER_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

VIRTUAL void HrcDumper_dump_snippet(HrcDumper_ptr self,
                                    HrcDumperSnippet snippet,
                                    const HrcDumperInfo* info)
{
  HRC_DUMPER_CHECK_INSTANCE(self);
  self->dump_snippet(self, snippet, info);
}

void HrcDumper_enable_indentation(HrcDumper_ptr self, boolean flag)
{
  HRC_DUMPER_CHECK_INSTANCE(self);
  self->use_indentation = flag;
}

void HrcDumper_inc_indent(HrcDumper_ptr self)
{
  HRC_DUMPER_CHECK_INSTANCE(self);
  nusmv_assert(self->indent >= 0);
  self->indent += 1;
}

void HrcDumper_dec_indent(HrcDumper_ptr self)
{
  HRC_DUMPER_CHECK_INSTANCE(self);
  nusmv_assert(self->indent > 0);
  self->indent -= 1;
}

void HrcDumper_enable_mod_suffix(HrcDumper_ptr self, boolean flag)
{
  HRC_DUMPER_CHECK_INSTANCE(self);
  self->use_mod_suffix = flag;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void hrc_dumper_init(HrcDumper_ptr self, const NuSMVEnv_ptr env, FILE* fout)
{
  /* base class initialization */
  env_object_init(ENV_OBJECT(self), env);

  /* members initialization */
  self->printer = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  self->fout = fout;
  self->use_indentation = true;
  self->indent_pending = false;
  self->indent = 0;
  self->indent_size = HRC_DEFAULT_INDENT_SIZE;
  self->columns = HRC_DEFAULT_COLUMNS;
  self->use_mod_suffix = false;

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = hrc_dumper_finalize;
  OVERRIDE(HrcDumper, dump_snippet) = hrc_dumper_dump_snippet;
  OVERRIDE(HrcDumper, dump_comment) = hrc_dumper_dump_comment;
  OVERRIDE(HrcDumper, dump_header) = hrc_dumper_dump_header;
  OVERRIDE(HrcDumper, dump_node) = hrc_dumper_dump_node;
}

void hrc_dumper_deinit(HrcDumper_ptr self)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  FILE* outstream = StreamMgr_get_output_stream(streams);
  FILE* errstream = StreamMgr_get_error_stream(streams);

  /* members deinitialization */
  if (self->fout && self->fout != errstream && self->fout != outstream) {
    fclose(self->fout);
  }

  /* base class deinitialization */
  env_object_deinit(ENV_OBJECT(self));
}

void hrc_dumper_dump_snippet(HrcDumper_ptr self,
                             HrcDumperSnippet snippet,
                             const HrcDumperInfo* info)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  switch (snippet) {
  case HDS_HRC_TOP:
    /* Available information:
       info->hrcNode
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_LIST_MODS:
    /* Available information:
       info->hrcNode
       info->list_is_empty
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_MOD:
    /* Available information:
       info->hrcNode
       info->n1.name : name of module
       info->n2.lineno : line number of MODULE definition
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_MOD_NAME:
    /* Available information:
       info->hrcNode
       info->n1.name : name of module
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_LIST_MOD_FORMAL_PARAMS:
    /* Available information:
       info->hrcNode
       info->list_is_empty
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_MOD_FORMAL_PARAM:
    /* Available information:
       info->hrcNode
       info->n1.name : name of param
       info->n2.type : type of param (can be Nil)
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_LIST_MOD_INSTANCES:
    /* Available information:
       info->hrcNode
       info->symb_cat : the var kind of the instances
       info->list_is_empty
   */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_MOD_INSTANCE:
    /* Available information:
       info->hrcNode
       info->n1.name : the instance name
       info->n2.type : the module instance name
       info->symb_cat : the var kind of the instance
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_MOD_INSTANCE_VARNAME:
    /* Available information:
       info->hrcNode
       info->n1.name : name of the instance
       info->symb_cat : the var kind of the instance
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_MOD_INSTANCE_MODNAME:
    /* Available information:
       info->hrcNode
       info->n1.name : name of the instance's type (module)
       info->symb_cat : the var kind of the instance
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_LIST_MOD_INSTANCE_ACTUAL_PARAMS:
    /* Available information:
       info->hrcNode
       info->list_is_empty
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_MOD_INSTANCE_ACTUAL_PARAM:
    /* Available information:
       info->hrcNode
       info->n1.value : value of the actual param
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_LIST_SYMBOLS:
    /* Available information:
       info->hrcNode
       info->symb_cat : the var/define/constant kind of the symbols list
       info->list_is_empty
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_SYMBOL:
    /* Available information:
       info->hrcNode
       info->n1.name : name of the symbol
       info->n2.type : type of variable (for vars)
       info->n2.body : body of define (for defines)
       info->symb_cat : the var/define/constant kind of the symbol
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_LIST_ASSIGNS:
    /* Available information:
       info->hrcNode
       info->list_is_empty
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_ASSIGN_INIT:
    /* Available information:
       info->hrcNode
       info->n1.name : name of the variable
       info->n2.expr : expression of the assignment
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_ASSIGN_INVAR:
    /* Available information:
       info->hrcNode
       info->n1.name : name of the variable
       info->n2.expr : expression of the assignment
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_ASSIGN_NEXT:
    /* Available information:
       info->hrcNode
       info->n1.name : name of the variable
       info->n2.expr : expression of the assignment
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_LIST_CONSTRAINTS:
    /* Available information:
       info->hrcNode
       info->list_is_empty
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_CONSTRAINT_INIT:
    /* Available information:
       info->hrcNode
       info->n1.expr : expression of the constraint
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_CONSTRAINT_INVAR:
    /* Available information:
       info->hrcNode
       info->n1.expr : expression of the constraint
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_CONSTRAINT_TRANS:
    /* Available information:
       info->hrcNode
       info->n1.expr : expression of the constraint
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_LIST_FAIRNESS:
    /* Available information:
       info->hrcNode
       info->list_is_empty
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_JUSTICE:
    /* Available information:
       info->hrcNode
       info->n1.expr : expression of the justice
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_COMPASSION:
    /* Available information:
       info->hrcNode
       info->n1.expr : first expression of the compassion
       info->n2.expr : second expression of the compassion
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_LIST_SPECS:
    /* Available information:
       info->hrcNode
       info->list_is_empty
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_SPEC:
    /* Available information:
       info->hrcNode
       info->spec_type : PropType of the specification
       info->n1.name : name of the spec (or Nil)
       info->n2.expr : expression of the specification
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_LIST_COMPILER_INFO:
    /* Available information:
       info->hrcNode
       info->list_is_empty
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_LIST_SYNTAX_ERRORS:
    /* Available information:
       info->hrcNode
       info->list_is_empty
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  case HDS_ERROR:
    /* Available information:
       info->hrcNode
       info->error.filename
       info->error.lineno
       info->error.message
       info->error.token
       info->last_in_list
    */
    if (info->stage & HRC_STAGE_BEGIN) {
    }
    if (info->stage & HRC_STAGE_END) {
    }
    break;

  default:
    ErrorMgr_internal_error(errmgr, "Unexpected node %d", snippet);
  }
}

void hrc_dumper_dump_comment(HrcDumper_ptr self, const char* msg)
{
  error_unreachable_code_msg("TBI");
}

void hrc_dumper_dump_header(HrcDumper_ptr self, const char* msg)
{
  error_unreachable_code_msg("TBI");
}

void hrc_dumper_dump_node(HrcDumper_ptr self, node_ptr node)
{
  print_node(self->printer, self->fout, node);
}

void hrc_dumper_dump_indent(HrcDumper_ptr self)
{
  if (self->use_indentation & self->indent_pending) {
    const size_t spaces = self->indent * self->indent_size;
    size_t i;
    for (i=0; i<spaces; ++i) {
      fprintf(self->fout, " ");
    }
    self->indent_pending = false;
  }
}

void hrc_dumper_nl(HrcDumper_ptr self)
{
  fprintf(self->fout, "\n");
  self->indent_pending = true;
}

void hrc_dumper_dump_var_type(HrcDumper_ptr self, node_ptr node)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  int node_type = node_get_type(node);

  switch (node_type) {
  case BOOLEAN:
  case TWODOTS: /* range */
    _HRC_DUMP_NODE(node);
    break;

  case INTEGER:
    _HRC_DUMP_STR("integer");
    break;

  case REAL:
    _HRC_DUMP_STR("real");
    break;

  case SIGNED_WORD:
    _HRC_DUMP_STR("signed word[");
    _HRC_DUMP_NODE(car(node));
    _HRC_DUMP_STR("]");
    break;

  case UNSIGNED_WORD:
    _HRC_DUMP_STR("unsigned word[");
    _HRC_DUMP_NODE(car(node));
    _HRC_DUMP_STR("]");
    break;

  case SCALAR:
    hrc_dumper_dump_scalar_type(self, node);
    break;

  case WORDARRAY_TYPE:
    _HRC_DUMP_STR("array word[");
    _HRC_DUMP_NODE(car(node));
    _HRC_DUMP_STR("]");

    _HRC_DUMP_STR(" of ");
    
    /* recursively prints the array type */
    hrc_dumper_dump_var_type(self, cdr(node));
    break;

  case INTARRAY_TYPE:
    _HRC_DUMP_STR("array integer of ");
    
    /* recursively prints the array type */
    hrc_dumper_dump_var_type(self, car(node));
    break;

  case ARRAY_TYPE:
    _HRC_DUMP_STR("array ");

    /* Prints subrange of array n..m */
    _HRC_DUMP_NODE(car(node));

    _HRC_DUMP_STR(" of ");

    /* recursively prints the array type */
    hrc_dumper_dump_var_type(self, cdr(node));
    break;

  case NFUNCTION_TYPE:
    {
      node_ptr iter;
      boolean is_first = true;
      NODE_CONS_LIST_FOREACH(car(node), iter) {
        node_ptr elem = Node_conslist_get(iter);

        if (! is_first) {
          _HRC_DUMP_STR(" * ");
        }
        is_first = false;

        hrc_dumper_dump_var_type(self, elem);
      }

      _HRC_DUMP_STR(" -> ");
      hrc_dumper_dump_var_type(self, cdr(node));
      break;
    }

  default:
    ErrorMgr_internal_error(errmgr, "Type %d not supported by hrc emitter.\n", node_type);
  }

  return;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The HrcDumper class virtual finalizer

  Called by the class destructor
*/
static void hrc_dumper_finalize(Object_ptr object, void* dummy)
{
  HrcDumper_ptr self = HRC_DUMPER(object);

  hrc_dumper_deinit(self);
  FREE(self);
}

/*!
  \brief Dumps the scalar type of a variable.

  Dumps the scalar type of a variable. The
  printer takes care of reversing the CONS list that contains the
  enumeration to preserve the order of the literals in the source
  model.

  \sa hrc_dumper_dump_var_type
*/
static void hrc_dumper_dump_scalar_type(HrcDumper_ptr self, node_ptr node)
{
  int node_type;
  node_ptr reversed_literals;
  node_ptr iterator;
  boolean first_literal;
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_type = node_get_type(node);
  nusmv_assert(SCALAR == node_type);

  _HRC_DUMP_STR("{");

  /* reverse the literals of the enumerations to preserve their
     original order */
  reversed_literals = reverse_ns(nodemgr, car(node));

  iterator = reversed_literals;
  first_literal = true;
  while (Nil != iterator) {
    node_ptr literal;

    literal = car(iterator);
    nusmv_assert(Nil != literal);

    if (! first_literal) {
      _HRC_DUMP_STR(", ");
    }
    _HRC_DUMP_NODE(literal);

    first_literal = false;
    iterator = cdr(iterator);
  }

  _HRC_DUMP_STR("}");

  free_list(nodemgr, reversed_literals);
}



/**AutomaticEnd***************************************************************/
