/* ---------------------------------------------------------------------------


  This file is part of the ``node'' package of NuSMV version 2.
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
  \brief Initialization and deinitialization for package node and
  subpackages

  Initialization and deinitialization for package node and
  subpackages

*/


#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/node/nodeInt.h"

#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/node/printers/PrinterWffCore.h"
#include "nusmv/core/node/printers/PrinterIWffCore.h"
#include "nusmv/core/node/printers/PrinterPsl.h"
#include "nusmv/core/node/printers/PrinterSexpCore.h"

#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#include "nusmv/core/node/normalizers/NormalizerBase.h"
#include "nusmv/core/node/normalizers/NormalizerCore.h"
#include "nusmv/core/node/normalizers/NormalizerPsl.h"

#include "nusmv/core/utils/error.h" /* for CATCH(errmgr) */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void node_pkg_init(NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  MasterPrinter_ptr wff_printer;
  MasterPrinter_ptr iwff_printer;
  MasterPrinter_ptr sexp_printer;
  MasterNormalizer_ptr normalizer;

  node_init(env);

  if (!NuSMVEnv_has_value(env, ENV_WFF_PRINTER)) {

    /* Core printer (legacy) */
    wff_printer = MasterPrinter_create(env);
    NuSMVEnv_set_value(env, ENV_WFF_PRINTER, wff_printer);

    CATCH(errmgr) {

      PrinterBase_ptr printer;
      printer = PRINTER_BASE(PrinterWffCore_create(env, "Core Wff Printer"));
      MasterNodeWalker_register_walker(MASTER_NODE_WALKER(wff_printer),
                                       NODE_WALKER(printer));

      /* printer for PSL: */
      printer = PRINTER_BASE(PrinterPsl_create(env, "PSL Printer"));
      MasterNodeWalker_register_walker(MASTER_NODE_WALKER(wff_printer),
                                       NODE_WALKER(printer));

    }

    FAIL(errmgr) {
      node_pkg_quit(env);
      ErrorMgr_nusmv_exit(errmgr, 1);
    }
  }

  if (!NuSMVEnv_has_value(env, ENV_IWFF_PRINTER)) {

    /* Core printer (indenting) */
    iwff_printer = MasterPrinter_create(env);
    NuSMVEnv_set_value(env, ENV_IWFF_PRINTER, iwff_printer);

    CATCH(errmgr) {

      PrinterBase_ptr printer;
      printer = PRINTER_BASE(PrinterIWffCore_create(env, "Core IWff Printer"));
      MasterNodeWalker_register_walker(MASTER_NODE_WALKER(iwff_printer),
                                       NODE_WALKER(printer));
      /* printer for PSL: */
      printer = PRINTER_BASE(PrinterPsl_create(env, "PSL Printer"));
      MasterNodeWalker_register_walker(MASTER_NODE_WALKER(iwff_printer),
                                       NODE_WALKER(printer));
    }

    FAIL(errmgr) {
      node_pkg_quit(env);
      ErrorMgr_nusmv_exit(errmgr, 1);
    }
  }

  if (!NuSMVEnv_has_value(env, ENV_SEXP_PRINTER)) {

    /* Core printer (sexp) */
    sexp_printer = MasterPrinter_create(env);
    NuSMVEnv_set_value(env, ENV_SEXP_PRINTER, sexp_printer);

    CATCH(errmgr) {

      PrinterBase_ptr printer;
      printer = PRINTER_BASE(PrinterSexpCore_create(env, "Core Sexp Printer"));
      MasterNodeWalker_register_walker(MASTER_NODE_WALKER(sexp_printer),
                                       NODE_WALKER(printer));
    }

    FAIL(errmgr) {
      node_pkg_quit(env);
      ErrorMgr_nusmv_exit(errmgr, 1);
    }
  }

  if (!NuSMVEnv_has_value(env, ENV_NODE_NORMALIZER)) {
    normalizer = MasterNormalizer_create(env);
    NuSMVEnv_set_value(env, ENV_NODE_NORMALIZER, normalizer);

    CATCH(errmgr) {

      NormalizerBase_ptr tmp_normalizer;
      tmp_normalizer = NORMALIZER_BASE(NormalizerCore_create(env, "Core Normalizer"));
      MasterNodeWalker_register_walker(MASTER_NODE_WALKER(normalizer),
                                       NODE_WALKER(tmp_normalizer));

      tmp_normalizer = NORMALIZER_BASE(NormalizerPsl_create(env, "Psl Normalizer"));
      MasterNodeWalker_register_walker(MASTER_NODE_WALKER(normalizer),
                                       NODE_WALKER(tmp_normalizer));
    }

    FAIL(errmgr) {
      node_pkg_quit(env);
      ErrorMgr_nusmv_exit(errmgr, 1);
    }
  }
}

void node_pkg_quit(NuSMVEnv_ptr env)
{
  MasterPrinter_ptr wff_printer = NuSMVEnv_remove_value(env, ENV_WFF_PRINTER);
  MasterPrinter_ptr iwff_printer = NuSMVEnv_remove_value(env, ENV_IWFF_PRINTER);
  MasterPrinter_ptr sexp_printer = NuSMVEnv_remove_value(env, ENV_SEXP_PRINTER);
  MasterNormalizer_ptr normalizer = NuSMVEnv_remove_value(env, ENV_NODE_NORMALIZER);

  MasterNodeWalker_destroy(MASTER_NODE_WALKER(wff_printer));
  MasterNodeWalker_destroy(MASTER_NODE_WALKER(iwff_printer));
  MasterNodeWalker_destroy(MASTER_NODE_WALKER(sexp_printer));

  MasterNodeWalker_destroy(MASTER_NODE_WALKER(normalizer));

  node_quit(env);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

