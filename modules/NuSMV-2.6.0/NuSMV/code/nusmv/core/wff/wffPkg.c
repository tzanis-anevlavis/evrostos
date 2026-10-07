/* ---------------------------------------------------------------------------


  This file is part of the ``wff'' package of NuSMV version 2.
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
  \author Alessandro Mariotti
  \brief Initialization and deinitialization for package wff and
  subpackages

  Initialization and deinitialization for package wff and
  subpackages

*/


#include "nusmv/core/wff/wff.h"
#include "nusmv/core/wff/w2w/w2wInt.h"
#include "nusmv/core/wff/ExprMgr.h"
#include "nusmv/core/wff/lr/MasterLogicRecognizer.h"

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void wff_pkg_init(const NuSMVEnv_ptr env)
{
  {
    ExprMgr_ptr expr_mgr = ExprMgr_create(env);
    NuSMVEnv_set_value(env, ENV_EXPR_MANAGER, expr_mgr);
  }

  {
    MasterLogicRecognizer_ptr master_recogn =
      MasterLogicRecognizer_create_with_default_recognizers(env);
    NuSMVEnv_set_value(env, ENV_MASTER_LOGIC_RECOGNIZER, master_recogn);
  }
}

void wff_pkg_quit(const NuSMVEnv_ptr env)
{

  {
    hash_ptr wff2nnf = NuSMVEnv_get_handled_hash_ptr(env, ENV_W2W_WFF2NNF_HASH);
    clear_assoc(wff2nnf);
  }

  {
    ExprMgr_ptr expr_mgr = EXPR_MGR(NuSMVEnv_remove_value(env, ENV_EXPR_MANAGER));
    ExprMgr_destroy(expr_mgr);
  }

  {
    MasterLogicRecognizer_ptr master_recogn =
      NuSMVEnv_remove_value(env, ENV_MASTER_LOGIC_RECOGNIZER);
    MasterLogicRecognizer_destroy(master_recogn);
  }
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

