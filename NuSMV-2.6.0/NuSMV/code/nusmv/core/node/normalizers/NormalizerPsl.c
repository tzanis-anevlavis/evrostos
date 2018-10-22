/* ---------------------------------------------------------------------------


   This file is part of the ``node.normalizers'' package of NuSMV version 2.
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
  \author Alessandro Mariotti
  \brief Implementaion of class 'NormalizerPsl'

  \todo: Missing description

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/node/normalizers/NormalizerPsl.h"
#include "nusmv/core/node/normalizers/NormalizerPsl_private.h"

#include "nusmv/core/parser/psl/psl_symbols.h"

#include "nusmv/core/utils/WordNumberMgr.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/compile/compile.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'NormalizerPsl_private.h' for class 'NormalizerPsl' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief Short way of calling normalizer_base_throw_normalize_node

  Use this macro to recursively recall normalize_node
*/

#define _THROW(n)                                                       \
  normalizer_base_throw_normalize_node(NORMALIZER_BASE(self), n)

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void normalizer_psl_finalize(Object_ptr object, void* dummy);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

NormalizerPsl_ptr NormalizerPsl_create(const NuSMVEnv_ptr env, const char* name)
{
  NormalizerPsl_ptr self = ALLOC(NormalizerPsl, 1);
  NORMALIZER_PSL_CHECK_INSTANCE(self);

  normalizer_psl_init(self, env, name,
                      NUSMV_PSL_SYMBOL_FIRST,
                      NUSMV_PSL_SYMBOL_LAST - NUSMV_PSL_SYMBOL_FIRST);
  return self;
}




/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void normalizer_psl_init(NormalizerPsl_ptr self, const NuSMVEnv_ptr env,
                         const char* name, int low, size_t num)
{
  /* base class initialization */
  normalizer_base_init(NORMALIZER_BASE(self), env, name, low,
                       num, true /*handles NULL*/);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = normalizer_psl_finalize;
  OVERRIDE(NormalizerBase, normalize_node) = normalizer_psl_normalize_node;

}

void normalizer_psl_deinit(NormalizerPsl_ptr self)
{
  /* members deinitialization */


  /* base class initialization */
  normalizer_base_deinit(NORMALIZER_BASE(self));
}

node_ptr normalizer_psl_normalize_node(NormalizerBase_ptr self, node_ptr node)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  if (Nil == node) return Nil;

  return find_node(nodemgr, node_get_type(node),
                   _THROW(car(node)),
                   _THROW(cdr(node)));
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions*/
/*---------------------------------------------------------------------------*/

/*!
  \brief The NormalizerPsl class virtual finalizer

  Called by the class destructor
*/
static void normalizer_psl_finalize(Object_ptr object, void* dummy)
{
  NormalizerPsl_ptr self = NORMALIZER_PSL(object);

  normalizer_psl_deinit(self);
  FREE(self);
}

/**AutomaticEnd***************************************************************/

