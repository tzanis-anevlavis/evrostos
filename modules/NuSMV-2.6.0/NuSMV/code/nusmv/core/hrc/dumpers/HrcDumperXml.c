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
  \brief Implementation of class 'HrcDumperXml'

  \todo: Missing description

*/


#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/hrc/dumpers/HrcDumperXml.h"
#include "nusmv/core/hrc/dumpers/HrcDumperXml_private.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define HRC_DUMPER_XML_COMMENT_PREFIX "<-- "

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define HRC_DUMPER_XML_COMMENT_SUFFIX " -->"

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'HrcDumperXml_private.h' for class 'HrcDumperXml' definition. */

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

static void hrc_dumper_xml_finalize(Object_ptr object, void* dummy);
static void hrc_dumper_xml_dump_escaped_node(HrcDumperXml_ptr self,
                                             node_ptr node);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

HrcDumperXml_ptr HrcDumperXml_create(const NuSMVEnv_ptr env,
                                     FILE* fout)
{
  HrcDumperXml_ptr self = ALLOC(HrcDumperXml, 1);
  HRC_DUMPER_XML_CHECK_INSTANCE(self);

  hrc_dumper_xml_init(self, env, fout);
  return self;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void hrc_dumper_xml_init(HrcDumperXml_ptr self,
                         const NuSMVEnv_ptr env,
                         FILE* fout)
{
  /* base class initialization */
  hrc_dumper_init(HRC_DUMPER(self), env, fout);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = hrc_dumper_xml_finalize;
  OVERRIDE(HrcDumper, dump_snippet) = hrc_dumper_xml_dump_snippet;
  OVERRIDE(HrcDumper, dump_comment) = hrc_dumper_xml_dump_comment;
}

void hrc_dumper_xml_deinit(HrcDumperXml_ptr self)
{
  /* members deinitialization */


  /* base class deinitialization */
  hrc_dumper_deinit(HRC_DUMPER(self));
}

void hrc_dumper_xml_dump_snippet(HrcDumper_ptr self,
                                 HrcDumperSnippet snippet,
                                 const HrcDumperInfo* info)
{
  #define buf_size 150
  char buf[buf_size+1];
  int c;
  const char* tag;

  HRC_DUMPER_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    switch (snippet) {
    case HDS_HRC_TOP:
      tag = "SmvHRC";
      if (info->stage & HRC_STAGE_BEGIN) {
        c = snprintf(buf, buf_size, "<%s xmlns=\"%s\">", tag, SMV_XSD_NS);
        SNPRINTF_CHECK(c, buf_size);
        _HRC_DUMP_STR(buf);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_LIST_MODS:
      if (info->list_is_empty) break;
      tag = "Modules";
      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_MOD:
      tag = "Module";
      if (info->stage & HRC_STAGE_BEGIN) {
        c = snprintf(buf, buf_size,
                     "<%s lineno=\"%d\">", tag, info->n2.lineno);
        SNPRINTF_CHECK(c, buf_size);
        _HRC_DUMP_STR(buf);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_MOD_NAME:
      tag="name";
      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
        /* [MD] Shouldn't be _HRC_DUMP_XML_NODE? */
        _HRC_DUMP_NODE(info->n1.name);
        if (self->use_mod_suffix &&
            /* top level name must not be changed */
            HRC_NODE(NULL) != HrcNode_get_parent(info->hrcNode)) {
          _HRC_DUMP_STR(HRC_MODULE_SUFFIX);
        }
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_LIST_MOD_FORMAL_PARAMS:
      if (info->list_is_empty) break;
      tag = "FormalParameters";
      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_MOD_FORMAL_PARAM:
      tag="FormalParameter";
      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);

        _HRC_DUMP_XML_NODE_BEGIN_END("name", info->n1.name);
        nusmv_assert(Nil == info->n2.type); /* type not supported yet! */
        _HRC_DUMP_XML_NODE_BEGIN_END("type", info->n2.type);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_LIST_MOD_INSTANCES:
      if (info->list_is_empty) break;
      tag = "ModInstances";
      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_MOD_INSTANCE:
      tag = "ModInstance";
      if (info->stage & HRC_STAGE_BEGIN) {
        c = snprintf(buf, buf_size,
                     "<%s lineno=\"%d\">", tag, node_get_lineno(info->n1.name));
        SNPRINTF_CHECK(c, buf_size);
        _HRC_DUMP_STR(buf);

      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_MOD_INSTANCE_VARNAME:
      tag = "name";
      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
        _HRC_DUMP_XML_NODE(info->n1.name);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_MOD_INSTANCE_MODNAME:
      tag = "type";
      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
        _HRC_DUMP_XML_NODE(info->n1.name);
        if (self->use_mod_suffix) {
          _HRC_DUMP_STR(HRC_MODULE_SUFFIX);
        }
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_LIST_MOD_INSTANCE_ACTUAL_PARAMS:
      tag = "ActualParameters";
      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_MOD_INSTANCE_ACTUAL_PARAM:
      tag = "value";
      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
        _HRC_DUMP_XML_NODE(info->n1.value);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_LIST_SYMBOLS:
      if (info->list_is_empty) break;

      switch (info->symb_cat) {
      case SYMBOL_STATE_VAR: tag="StateVars"; break;
      case SYMBOL_INPUT_VAR: tag="InputVars"; break;
      case SYMBOL_FROZEN_VAR: tag="FrozenVars"; break;
      case SYMBOL_CONSTANT: tag="Constants"; break;
      case SYMBOL_DEFINE: tag="Defines"; break;
      case SYMBOL_FUNCTION: tag="Functions"; break;
      default: ErrorMgr_internal_error(errmgr, "Unexpected type of list of symbols");
      }

      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_SYMBOL:
      if (info->stage & HRC_STAGE_BEGIN) {
        switch (info->symb_cat) {
        case SYMBOL_STATE_VAR:
        case SYMBOL_INPUT_VAR:
        case SYMBOL_FROZEN_VAR:
          tag = "Var";
          c = snprintf(buf, buf_size,
                       "<%s lineno=\"%d\">", tag, node_get_lineno(info->n1.name));
          SNPRINTF_CHECK(c, buf_size);
          _HRC_DUMP_STR(buf);

          _HRC_DUMP_XML_NODE_BEGIN_END("name", info->n1.name);
          _HRC_DUMP_XML_TAG_BEGIN("type");
          hrc_dumper_dump_var_type(self, info->n2.type);
          _HRC_DUMP_XML_TAG_END("type") ;

          _HRC_DUMP_XML_TAG_END(tag);
          break;

        case SYMBOL_CONSTANT:
          tag = "Constant";
          c = snprintf(buf, buf_size,
                       "<%s lineno=\"%d\">", tag, node_get_lineno(info->n1.name));
          SNPRINTF_CHECK(c, buf_size);
          _HRC_DUMP_STR(buf);

          _HRC_DUMP_XML_NODE_BEGIN_END("name", info->n1.name);

          _HRC_DUMP_XML_TAG_END(tag);
          break;

        case SYMBOL_DEFINE:
          tag = "Define";
          c = snprintf(buf, buf_size,
                       "<%s lineno=\"%d\">", tag, node_get_lineno(info->n1.name));
          SNPRINTF_CHECK(c, buf_size);
          _HRC_DUMP_STR(buf);

          _HRC_DUMP_XML_NODE_BEGIN_END("name", info->n1.name);
          _HRC_DUMP_XML_NODE_BEGIN_END("body", info->n2.body);

          _HRC_DUMP_XML_TAG_END(tag);
          break;

        case SYMBOL_FUNCTION:
          tag = "Function";
          c = snprintf(buf, buf_size,
                       "<%s lineno=\"%d\">", tag, node_get_lineno(info->n1.name));
          SNPRINTF_CHECK(c, buf_size);
          _HRC_DUMP_STR(buf);

          _HRC_DUMP_XML_NODE_BEGIN_END("name", info->n1.name);
          _HRC_DUMP_XML_TAG_BEGIN("type");
          hrc_dumper_dump_var_type(self, info->n2.type);
          _HRC_DUMP_XML_TAG_END("type") ;

          _HRC_DUMP_XML_TAG_END(tag);
          break;

        default: ErrorMgr_internal_error(errmgr, "Unexpected symbol type");
        }
      }
      if (info->stage & HRC_STAGE_END) {
        /* tag already closed in begin stage */
        nusmv_assert(info->stage & HRC_STAGE_BEGIN); /* begin also processed */
      }
      break; /* end of case HDS_SYMBOL */

    case HDS_LIST_ASSIGNS:
      if (info->list_is_empty) break;

      tag="Assigns";
      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_ASSIGN_INIT:
      tag="InitAssign";
      if (info->stage & HRC_STAGE_BEGIN) {
        c = snprintf(buf, buf_size,
                     "<%s lineno=\"%d\">", tag, node_get_lineno(info->n1.name));
        SNPRINTF_CHECK(c, buf_size);
        _HRC_DUMP_STR(buf);

        _HRC_DUMP_XML_NODE_BEGIN_END("name", info->n1.name);
        _HRC_DUMP_XML_NODE_BEGIN_END("value", info->n2.expr);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_ASSIGN_INVAR:
      tag="InvarAssign";
      if (info->stage & HRC_STAGE_BEGIN) {
        c = snprintf(buf, buf_size,
                     "<%s lineno=\"%d\">", tag, node_get_lineno(info->n1.name));
        SNPRINTF_CHECK(c, buf_size);
        _HRC_DUMP_STR(buf);

        _HRC_DUMP_XML_NODE_BEGIN_END("name", info->n1.name);
        _HRC_DUMP_XML_NODE_BEGIN_END("value", info->n2.expr);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_ASSIGN_NEXT:
      tag="NextAssign";
      if (info->stage & HRC_STAGE_BEGIN) {
        c = snprintf(buf, buf_size,
                     "<%s lineno=\"%d\">", tag, node_get_lineno(info->n1.name));
        SNPRINTF_CHECK(c, buf_size);
        _HRC_DUMP_STR(buf);

        _HRC_DUMP_XML_NODE_BEGIN_END("name", info->n1.name);
        _HRC_DUMP_XML_NODE_BEGIN_END("value", info->n2.expr);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_LIST_CONSTRAINTS:
      if (info->list_is_empty) break;

      tag="Constraints";
      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_CONSTRAINT_INIT:
      tag="Init";
      if (info->stage & HRC_STAGE_BEGIN) {
        c = snprintf(buf, buf_size,
                     "<%s lineno=\"%d\">", tag, node_get_lineno(info->n1.expr));
        SNPRINTF_CHECK(c, buf_size);
        _HRC_DUMP_STR(buf);
        _HRC_DUMP_XML_NODE_BEGIN_END("expr", info->n1.expr);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_CONSTRAINT_INVAR:
      tag="Invar";
      if (info->stage & HRC_STAGE_BEGIN) {
        c = snprintf(buf, buf_size,
                     "<%s lineno=\"%d\">", tag, node_get_lineno(info->n1.expr));
        SNPRINTF_CHECK(c, buf_size);
        _HRC_DUMP_STR(buf);
        _HRC_DUMP_XML_NODE_BEGIN_END("expr", info->n1.expr);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_CONSTRAINT_TRANS:
      tag="Trans";
      if (info->stage & HRC_STAGE_BEGIN) {
        c = snprintf(buf, buf_size,
                     "<%s lineno=\"%d\">", tag, node_get_lineno(info->n1.expr));
        SNPRINTF_CHECK(c, buf_size);
        _HRC_DUMP_STR(buf);
        _HRC_DUMP_XML_NODE_BEGIN_END("expr", info->n1.expr);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_LIST_FAIRNESS:
      if (info->list_is_empty) break;

      tag="Fairness";
      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_JUSTICE:
      tag="Justice";
      if (info->stage & HRC_STAGE_BEGIN) {
        c = snprintf(buf, buf_size,
                     "<%s lineno=\"%d\">", tag, node_get_lineno(info->n1.expr));
        SNPRINTF_CHECK(c, buf_size);
        _HRC_DUMP_STR(buf);
        _HRC_DUMP_XML_NODE_BEGIN_END("expr", info->n1.expr);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_COMPASSION:
      tag="Compassion";
      if (info->stage & HRC_STAGE_BEGIN) {
        c = snprintf(buf, buf_size,
                     "<%s lineno=\"%d\">", tag, node_get_lineno(info->n1.expr));
        SNPRINTF_CHECK(c, buf_size);
        _HRC_DUMP_STR(buf);
        _HRC_DUMP_XML_NODE_BEGIN_END("expr1", info->n1.expr);
        _HRC_DUMP_XML_NODE_BEGIN_END("expr2", info->n2.expr);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_LIST_SPECS:
      if (info->list_is_empty) break;

      tag="Specs";
      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_SPEC:
      tag = "Spec";
      if (info->stage & HRC_STAGE_BEGIN) {
        c = snprintf(buf, buf_size,
                     "<%s lineno=\"%d\">", tag, node_get_lineno(info->n2.expr));
        SNPRINTF_CHECK(c, buf_size);
        _HRC_DUMP_STR(buf);

        _HRC_DUMP_XML_NODE_BEGIN_END("name", info->n1.name);

        _HRC_DUMP_XML_TAG_BEGIN("proptype");
        switch (info->spec_type) {
        case Prop_Ctl:
          _HRC_DUMP_STR("CTL");
          break;
        case Prop_Ltl:
          _HRC_DUMP_STR("LTL");
          break;
        case Prop_Psl:
          _HRC_DUMP_STR("PSL");
          break;
        case Prop_Invar:
          _HRC_DUMP_STR("INVAR");
          break;
        case Prop_Compute:
          _HRC_DUMP_STR("COMPUTE");
          break;
        default:
          ErrorMgr_internal_error(errmgr, "Invalid property type");
        }
        _HRC_DUMP_XML_TAG_END("proptype");

        _HRC_DUMP_XML_NODE_BEGIN_END("expr", info->n2.expr);
      }

      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_LIST_COMPILER_INFO:
      if (info->list_is_empty) break;
      tag = "CompilerInfo";

      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_LIST_SYNTAX_ERRORS:
      if (info->list_is_empty) break;
      tag = "SyntaxErrors";

      if (info->stage & HRC_STAGE_BEGIN) {
        _HRC_DUMP_XML_TAG_BEGIN(tag);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    case HDS_ERROR:
      tag = "Error";

      if (info->stage & HRC_STAGE_BEGIN) {
        char buf[12];
        c = snprintf(buf, 12, "%d", info->error.lineno);
        SNPRINTF_CHECK(c, 12);

        _HRC_DUMP_XML_TAG_BEGIN(tag);

        _HRC_DUMP_XML_TAG_BEGIN_END("filename", info->error.filename);
        _HRC_DUMP_XML_TAG_BEGIN_END("lineno", buf);
        _HRC_DUMP_XML_TAG_BEGIN_END("token", info->error.token);
        _HRC_DUMP_XML_TAG_BEGIN_END("message", info->error.message);
      }
      if (info->stage & HRC_STAGE_END) {
        _HRC_DUMP_XML_TAG_END(tag);
      }
      break;

    default:
      /* not handled here, try with the base */
      hrc_dumper_dump_snippet(HRC_DUMPER(self), snippet, info);
    }
  }
}

void hrc_dumper_xml_dump_comment(HrcDumper_ptr self, const char* msg)
{
  _HRC_DUMP_STR(HRC_DUMPER_XML_COMMENT_PREFIX);
  _HRC_DUMP_STR(msg);
  _HRC_DUMP_STR_NL(HRC_DUMPER_XML_COMMENT_SUFFIX);
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The HrcDumperXml class virtual finalizer

  Called by the class destructor
*/
static void hrc_dumper_xml_finalize(Object_ptr object, void* dummy)
{
  HrcDumperXml_ptr self = HRC_DUMPER_XML(object);

  hrc_dumper_xml_deinit(self);
  FREE(self);
}

/*!
  \brief Dumps escaped node

  
*/
static void hrc_dumper_xml_dump_escaped_node(HrcDumperXml_ptr self,
                                             node_ptr node)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  char* node_str = sprint_node(wffprint, node);
  hrc_dumper_dump_indent(HRC_DUMPER(self));
  Utils_str_escape_xml_file(node_str, HRC_DUMPER(self)->fout);
  FREE(node_str);
}

/**AutomaticEnd***************************************************************/

