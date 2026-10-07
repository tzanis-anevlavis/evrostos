/* ---------------------------------------------------------------------------


  This file is part of the ``wff.lr'' package of NuSMV version 2.
  Copyright (C) 2013 by FBK-irst.

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
  \brief Implementation of class 'LogicRecognizerCore'

  \todo: Missing description

*/


#include "nusmv/core/wff/lr/LogicRecognizerCore.h"
#include "nusmv/core/wff/lr/LogicRecognizerCore_private.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"

#ifdef DEBUG_CONVERT_PROPERTY_TO_INVAR
#  include "nusmv/core/utils/Logger.h"
#  include "nusmv/core/opt/opt.h"
#endif

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'LogicRecognizerCore_private.h' for class 'LogicRecognizerCore' definition. */

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

static void logic_recognizer_core_finalize(Object_ptr object, void* dummy);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

LogicRecognizerCore_ptr LogicRecognizerCore_create(NuSMVEnv_ptr env)
{
  LogicRecognizerCore_ptr self = ALLOC(LogicRecognizerCore, 1);
  LOGIC_RECOGNIZER_CORE_CHECK_INSTANCE(self);

  logic_recognizer_core_init(self, env, "Core WFF Logic Recognizer",
                             NUSMV_EXPR_SYMBOL_FIRST,
                             NUSMV_EXPR_SYMBOL_LAST - NUSMV_EXPR_SYMBOL_FIRST);

  return self;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void logic_recognizer_core_init(LogicRecognizerCore_ptr self,
                                const NuSMVEnv_ptr env,
                                const char* name,
                                int low,
                                size_t num)
{
  /* base class initialization */
  logic_recognizer_base_init(LOGIC_RECOGNIZER_BASE(self),
                             env, name, low, num);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = logic_recognizer_core_finalize;
  OVERRIDE(LogicRecognizerBase, recognize) = logic_recognizer_core_recognize;
}

void logic_recognizer_core_deinit(LogicRecognizerCore_ptr self)
{
  /* members deinitialization */


  /* base class deinitialization */
  logic_recognizer_base_deinit(LOGIC_RECOGNIZER_BASE(self));
}

LogicType logic_recognizer_core_recognize(LogicRecognizerBase_ptr self,
                                                node_ptr exp,
                                                node_ptr context)
{
  MasterLogicRecognizer_ptr master = MASTER_LOGIC_RECOGNIZER(NODE_WALKER(self)->master);
  LogicType retval = EXP_NONE;
  LogicType left = EXP_NONE;
  LogicType right = EXP_NONE;
  short int exptype = 0;

  nusmv_assert(NULL != exp);

#ifdef DEBUG_CONVERT_PROPERTY_TO_INVAR
  {
    NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
    Logger_ptr const logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    MasterPrinter_ptr const sexpprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_SEXP_PRINTER));
    OptsHandler_ptr const opts =
      OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

    Logger_vnlog_error(logger, sexpprint, opts, "Input:\n%N\n",
                       exp);
  }
#endif

  exptype = node_get_type(exp);

  /* fast return */
  switch (exptype) {
  case CONTEXT:
    retval = LR_THROW(self, cdr(exp), car(exp));;
    break;
  case FAILURE:
  case FALSEEXP:
  case TRUEEXP:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case UWCONST:
  case SWCONST:
  case TWODOTS:
  case DOT:
  case ATOM:
  case BIT:
  case SELF:
  case ARRAY:
  case COUNT:
    retval = EXP_SIMPLE; break;

  case NEXT:
    retval = EXP_NEXT; break;

    /* CTL */
  case EX: case AX: case EF: case AF: case EG: case AG:
  case ABU: case EBU:
  case EBF: case ABF: case EBG: case ABG:
  case AU: case EU:
    retval = EXP_CTL; break;

    /* LTL */
  case OP_NEXT: case OP_PREC: case OP_NOTPRECNOT: case OP_GLOBAL:
  case OP_HISTORICAL: case OP_FUTURE: case OP_ONCE:
  case UNTIL: case SINCE:
    retval = EXP_LTL; break;

  default:
    retval = master_logic_recognizer_lookup(master, exp, context);
  } /* end of first switch */

  if (EXP_NONE == retval) {
    switch(exptype) {
      /* unary operators */
    case CAST_BOOL:
    case CAST_WORD1:
    case CAST_SIGNED:
    case CAST_UNSIGNED:
    case WSIZEOF:
    case CAST_TOINT:
    case NOT:
    case UMINUS:
      retval = LR_THROW(self, car(exp), context);
      break;

      /* binary operators */
    case CONS:
      left = LR_THROW(self, car(exp), context);

      if (NULL != cdr(exp)) {
        right = LR_THROW(self, cdr(exp), context);
        retval = master_logic_recognizer_merge(master, left, right);
      }
      else retval = LR_THROW(self, car(exp), context);

      break;

    case BIT_SELECTION:
    case CASE: case COLON:
    case CONCATENATION:
    case TIMES: case DIVIDE: case PLUS :case MINUS: case MOD:
    case LSHIFT: case RSHIFT: case LROTATE: case RROTATE:
    case WAREAD: case WAWRITE:
    case UNION: case SETIN:
    case IFTHENELSE:
    case EXTEND:
    case WRESIZE:
    case AND: case OR: case XOR: case XNOR: case IFF: case IMPLIES:
    case EQUAL: case NOTEQUAL: case LT: case GT: case LE: case GE:
      left = LR_THROW(self, car(exp), context);
      right = LR_THROW(self, cdr(exp), context);
      retval = master_logic_recognizer_merge(master, left, right);
      break;

    case NFUNCTION: {
      node_ptr args = cdr(exp);
      retval = EXP_SIMPLE;

      while (Nil != args) {
        left = LR_THROW(self, car(args), context);
        nusmv_assert((left == EXP_SIMPLE) ||
                     (left == EXP_NEXT));
        retval = master_logic_recognizer_merge(master, retval, left);
        args = cdr(args);
      }
      break;
    }

    default:
      {
        NuSMVEnv_ptr const env = EnvObject_get_environment(ENV_OBJECT(self));
        StreamMgr_ptr const streams =
          STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
        MasterPrinter_ptr const sexpprint =
          MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_SEXP_PRINTER));
        ErrorMgr_ptr const errmgr =
          ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

        StreamMgr_nprint_error(streams, sexpprint,
                               "Internal Error: unhandled operator in expr \n%N\n", exp);
        ErrorMgr_nusmv_exit(errmgr, 1);
      }
    } /* end of switch */

    master_logic_recognizer_insert(master, exp, context, retval);
  }

  return retval;
}

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The LogicRecognizerCore class virtual finalizer

  Called by the class destructor
*/
static void logic_recognizer_core_finalize(Object_ptr object, void* dummy)
{
  LogicRecognizerCore_ptr self = LOGIC_RECOGNIZER_CORE(object);

  logic_recognizer_core_deinit(self);
  FREE(self);
}



/**AutomaticEnd***************************************************************/

