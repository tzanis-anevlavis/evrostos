/* ---------------------------------------------------------------------------


  This file is part of the ``hrc'' package of NuSMV version 2.
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
  \author Sergio Mover
  \brief Package level routines for hrc package.

  This file contains the package level routines for the
  hrc package. Among these routines there are the init and quit
  routines that initializes/deinitializes the environment and the
  package commands.

*/

#include "nusmv/core/hrc/hrcInt.h"

#include "nusmv/core/hrc/dumpers/HrcDumperDebug.h"
#include "nusmv/core/hrc/dumpers/HrcDumperSmv.h"
#include "nusmv/core/hrc/dumpers/HrcDumperXml.h"

#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"

#include "nusmv/core/cinit/NuSMVEnv.h"

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
typedef HrcDumper_ptr (*HrcDumperFactory)(const NuSMVEnv_ptr env, FILE* fout);

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/


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

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Hrc_init(NuSMVEnv_ptr env)
{
  HrcNode_ptr node = HrcNode_create(env);

  NuSMVEnv_set_value(env, ENV_HRC_HIERARCHY, node);
}

void Hrc_quit(NuSMVEnv_ptr env)
{
  if (NuSMVEnv_has_value(env, ENV_HRC_HIERARCHY)) {
    HrcNode_ptr node = HRC_NODE(NuSMVEnv_remove_value(env, ENV_HRC_HIERARCHY));
    HrcNode_destroy_recur(node);
  }
}

int Hrc_dump_model(const NuSMVEnv_ptr env,
                   HrcDumpFormat dump_format,
                   FILE* ofileid,
                   const boolean append_suffix,
                   const boolean use_indent)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  HrcDumper_ptr dumper = NULL;

  int rv = 1;

  switch(dump_format) {
  case HRC_DUMP_FORMAT_DEBUG:
    dumper = HRC_DUMPER(HrcDumperDebug_create(env, ofileid));
    break;
      
  case HRC_DUMP_FORMAT_SMV:
    dumper = HRC_DUMPER(HrcDumperSmv_create(env, ofileid));
    break;
      
  case HRC_DUMP_FORMAT_XML:
    dumper = HRC_DUMPER(HrcDumperXml_create(env, ofileid));
    break;
      
  default:
    error_unreachable_code();
  }

  HrcDumper_enable_mod_suffix(dumper, append_suffix);
  HrcDumper_enable_indentation(dumper, use_indent);

  CATCH(errmgr) {
    HrcNode_ptr hrcNode = HRC_NODE(NuSMVEnv_get_value(env, ENV_HRC_HIERARCHY));

    Hrc_DumpModel(hrcNode, dumper);

    if (opt_verbose_level_gt(opts, 0)) {
      Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
      Logger_log(logger, "  done.\n");
    }
    rv = 0;
  }
  FAIL(errmgr) {
    rv = 1;
  }

  HrcDumper_destroy(dumper); dumper = NULL;

  return rv;
}

/*!
  \brief Convert a string to an HrcDumpFormat

  If the string is not recognized, an invalide value is set
*/

HrcDumpFormat Hrc_dump_format_str_to_enum(char* format)
{
  if (0 == strcmp("debug", format)) {
    return HRC_DUMP_FORMAT_DEBUG;
  }
  else if (0 == strcmp("smv", format)) {
    return HRC_DUMP_FORMAT_SMV;
  }
  else if (0 == strcmp("xml", format)) {
    return HRC_DUMP_FORMAT_XML;
  }
  else return HRC_DUMP_FORMAT_INVALID;
}

/*!
  \brief Convert a format to a string type

  If the format is not recognized, NULL is returned
*/

char* Hrc_dump_format_enum_to_str(HrcDumpFormat format)
{
  switch(format) {
  case HRC_DUMP_FORMAT_DEBUG:
    return "debug";

  case HRC_DUMP_FORMAT_SMV:
    return "smv";

  case HRC_DUMP_FORMAT_XML:
    return "xml";

  default:
    return NULL;
  }
}

/*!
  \brief Returns a string with the list of the possible dump
  format

  Useful for usage message
*/

char* Hrc_dump_format_get_available(void)
{
  return "debug, smv, xml";
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/



