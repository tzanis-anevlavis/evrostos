/* ---------------------------------------------------------------------------


  This file is part of the ``nusmv.core.hrc.dumpers'' package of NuSMV version 2.
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
  \brief Implementation of class 'HrcDumperAnonymizer'

  Anonymize and dump an hrc

*/


#include "nusmv/core/hrc/dumpers/HrcDumperAnonymizer.h"
#include "nusmv/core/hrc/dumpers/HrcDumperAnonymizer_private.h"
#include "nusmv/core/node/anonymizers/NodeAnonymizerBase.h"
#include "nusmv/core/utils/ErrorMgr.h"
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
/* See 'HrcDumperAnonymizer_private.h' for class 'HrcDumperAnonymizer' definition. */

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

static void hrc_dumper_anonymizer_finalize(Object_ptr object, void* dummy);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

HrcDumperAnonymizer_ptr HrcDumperAnonymizer_create(const NuSMVEnv_ptr env,
                                                   FILE* fout,
                                                   NodeAnonymizerBase_ptr anonymizer)
{
  HrcDumperAnonymizer_ptr self = ALLOC(HrcDumperAnonymizer, 1);

  HRC_DUMPER_ANONYMIZER_CHECK_INSTANCE(self);
  NUSMV_ENV_CHECK_INSTANCE(env);
  nusmv_assert(NULL != fout);
  NODE_ANONYMIZER_BASE_CHECK_INSTANCE(anonymizer);
  
  hrc_dumper_anonymizer_init(self, env, fout, anonymizer);
  return self;
}

void HrcDumperAnonymizer_destroy(HrcDumperAnonymizer_ptr self)
{
  HRC_DUMPER_ANONYMIZER_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void hrc_dumper_anonymizer_init(HrcDumperAnonymizer_ptr self,
                                const NuSMVEnv_ptr env,
                                FILE* fout,
                                NodeAnonymizerBase_ptr anonymizer)
{
  /* base class initialization */
  hrc_dumper_smv_init(HRC_DUMPER_SMV(self), env, fout);

  /* members initialization */
  self->anonymizer = anonymizer;

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = hrc_dumper_anonymizer_finalize;
  OVERRIDE(HrcDumper, dump_node) = hrc_dumper_anonymizer_dump_node;
  OVERRIDE(HrcDumper, dump_snippet) = hrc_dumper_anonymizer_dump_snippet;
}

void hrc_dumper_anonymizer_deinit(HrcDumperAnonymizer_ptr self)
{
  /* members deinitialization */
  self->anonymizer = NULL;

  /* base class deinitialization */
  hrc_dumper_smv_deinit(HRC_DUMPER_SMV(self));
}

void hrc_dumper_anonymizer_dump_snippet(HrcDumper_ptr self,
                                        HrcDumperSnippet snippet,
                                        const HrcDumperInfo* info)
{
  HRC_DUMPER_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    switch (snippet) {
    case HDS_MOD_NAME:
      if (info->stage & HRC_STAGE_BEGIN) {
        if (sym_intern(env, "main") == info->n1.name) {
          /* do not anonymize "main", call super method */
          hrc_dumper_dump_indent(self);
          hrc_dumper_dump_node(self, info->n1.name);
        }
        else {
            _HRC_DUMP_NODE(info->n1.name);
        }
        if (self->use_mod_suffix &&
            /* top level name must not be changed */
            HRC_NODE(NULL) != HrcNode_get_parent(info->hrcNode)) {
          _HRC_DUMP_STR(HRC_MODULE_SUFFIX);
        }
      }
      break;

    default:
      /* not handled here, try with the base */
      hrc_dumper_smv_dump_snippet(self, snippet, info);
    }
  }
}

void hrc_dumper_anonymizer_dump_node(HrcDumper_ptr self, node_ptr node)
{
  node_ptr anonymous_node;
  
  anonymous_node =
    NodeAnonymizerBase_map_expr(HRC_DUMPER_ANONYMIZER(self)->anonymizer, node);
  print_node(HRC_DUMPER(self)->printer, HRC_DUMPER(self)->fout, anonymous_node);
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The HrcDumperAnonymizer class virtual finalizer

  Called by the class destructor
*/
static void hrc_dumper_anonymizer_finalize(Object_ptr object, void* dummy)
{
  HrcDumperAnonymizer_ptr self = HRC_DUMPER_ANONYMIZER(object);

  hrc_dumper_anonymizer_deinit(self);
  FREE(self);
}



/**AutomaticEnd***************************************************************/

