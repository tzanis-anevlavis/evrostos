/* ---------------------------------------------------------------------------


  This file is part of the ``node.printers'' package of NuSMV version 2.
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
  \brief Implementaion of class 'PrinterPsl'

  \todo: Missing description

*/


#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/node/printers/PrinterPsl.h"
#include "nusmv/core/node/printers/PrinterPsl_private.h"

#include "nusmv/core/parser/psl/psl_symbols.h"
#include "nusmv/core/parser/psl/pslNode.h"
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
/* See 'PrinterPsl_private.h' for class 'PrinterPsl' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief Short way of calling printer_base_throw_print_node

  Use this macro to recursively recall print_node,
  with no need of specifing the priority
*/

#define _THROW(n)  \
   printer_base_throw_print_node(PRINTER_BASE(self),                  \
                                 PslNode_convert_to_node_ptr(n), priority)


/*!
  \brief Short way of calling printer_base_throw_print_node

  Use this macro to recursively recall print_node,
  by specifying a priority
*/

#define _THROWP(n, p)  \
   printer_base_throw_print_node(PRINTER_BASE(self),                  \
                                 PslNode_convert_to_node_ptr(n), p)



/*!
  \brief Short way of calling printer_base_print_string

  Use to print a string (that will be redirected to the
  currently used stream)
*/

#define _PRINT(str)  printer_base_print_string(PRINTER_BASE(self), str)



/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void printer_psl_finalize(Object_ptr object, void* dummy);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

PrinterPsl_ptr PrinterPsl_create(const NuSMVEnv_ptr env, const char* name)
{
  PrinterPsl_ptr self = ALLOC(PrinterPsl, 1);
  PRINTER_PSL_CHECK_INSTANCE(self);

  printer_psl_init(self, env, name,
                   NUSMV_PSL_SYMBOL_FIRST,
                   NUSMV_PSL_SYMBOL_LAST - NUSMV_PSL_SYMBOL_FIRST);
  return self;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void printer_psl_init(PrinterPsl_ptr self, const NuSMVEnv_ptr env,
                      const char* name, int low, size_t num)
{
  /* base class initialization */
  printer_base_init(PRINTER_BASE(self), env, name, low, num,
                    false /*NULL not handled*/);

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = printer_psl_finalize;
  OVERRIDE(PrinterBase, print_node) = printer_psl_print_node;

}

void printer_psl_deinit(PrinterPsl_ptr self)
{
  /* members deinitialization */


  /* base class initialization */
  printer_base_deinit(PRINTER_BASE(self));
}

int printer_psl_print_node(PrinterBase_ptr self, node_ptr n, int priority)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  PslNode_ptr psl = PslNode_convert_from_node_ptr(n);

  switch (psl_node_get_op(psl)) {

  case PSL_INF: return _PRINT("inf");

  case PSL_SERE:
    {
      int retval;

      if (!psl_node_is_sere(psl_node_get_left(psl))) retval = _PRINT("{");
      else retval = 1;

      retval = retval && _THROW(psl_node_get_left(psl));

      if (!psl_node_is_sere(psl_node_get_left(psl))) retval = _PRINT("}");
      return retval;
    }

  case PSL_SERECONCAT:
    return _PRINT("{") && \
      _THROW(psl_node_get_left(psl)) && _PRINT("; ") &&
      _THROW(psl_node_get_right(psl)) && _PRINT("}");

  case PSL_SEREFUSION:
    return _PRINT("{") && \
      _THROW(psl_node_get_left(psl)) && _PRINT(" : ") &&
      _THROW(psl_node_get_right(psl)) && _PRINT("}");

  case PSL_SERECOMPOUND:
    {
      int retval;
      PslNode_ptr op = psl_node_get_left(psl);
      nusmv_assert(op != PSL_NULL);

      /* left operand */
      retval = _PRINT("{") && _THROW(psl_node_get_left(op));

      switch (psl_node_get_op(op)) {
      case AND: retval = retval && _PRINT(" & "); break;
      case OR: retval = retval && _PRINT(" | "); break;
      case PSL_AMPERSANDAMPERSAND: retval = retval && _PRINT(" && "); break;
      default:
        ErrorMgr_internal_error(errmgr, "printer_psl_print_node: Unsupported sere "\
                       "compound operator");
      }

      /* right operand */
      return _THROW(psl_node_get_right(op)) && _PRINT("}");
    }

  case PSL_SEREREPEATED:
    {
      int retval;
      PslNode_ptr sere, count;

      nusmv_assert(psl_node_get_left(psl) != PSL_NULL);
      sere = psl_node_get_left(psl_node_get_left(psl));
      count = psl_node_get_right(psl);

      retval = _PRINT("{");
      if (sere != PSL_NULL) retval = retval && _THROW(sere);
      retval = retval && _PRINT("[");

      switch (psl_node_get_op(psl_node_get_left(psl))) {
      case PSL_LBSPLAT: retval = retval && _PRINT("*"); break;
      case PSL_LBPLUSRB: retval = retval && _PRINT("+"); break;
      case PSL_LBEQ: retval = retval && _PRINT("="); break;
      case PSL_LBMINUSGT: retval = retval && _PRINT("->"); break;
      default:
        ErrorMgr_internal_error(errmgr, "printer_psl_print_node: Unsupported sere "\
                       "repeated operator");
      }

      if (count != PSL_NULL) retval = retval && _THROW(count);
      return retval && _PRINT("]}");
    }

  case PSL_CONCATENATION:
    return _PRINT("{") && \
      _THROW(psl_node_get_left(psl)) && _PRINT("}");

  case PSL_REPLPROP:
    return _PRINT("( ") &&
      _THROW(psl_node_get_left(psl)) && /* replicator */
      _PRINT(" : ") &&
      _THROW(psl_node_get_right(psl)) && /* property */
      _PRINT(" )");

  case PSL_FORALL: /* replicator */
  case PSL_FORANY:
    {
      PslNode_ptr rvs = psl_node_get_replicator_value_set(psl);
      PslNode_ptr rr = psl_node_get_replicator_range(psl);
      int res;

      if (psl_node_get_op(psl) == PSL_FORALL) res = _PRINT("forall ");
      else res = _PRINT("forany ");
      res = res && _THROW(psl_node_get_replicator_id(psl));

      if (rr != PSL_NULL) {
        res = res && _PRINT(" [") && _THROW(rr) && _PRINT("]");
      }

      res = res && _PRINT(" in ");
      if (psl_node_is_boolean_type(rvs)) res = res && _THROW(rvs);
      else {
        res = res && _PRINT("{") && _THROW(rvs) && _PRINT("}");
      }
      return res;
    }


  case PSL_PIPEMINUSGT:
    {
      int res = _PRINT("(") &&
        _THROW(psl_node_get_left(psl_node_get_left(psl))) &&
        _PRINT(" |-> ") && _THROW(psl_node_get_right(psl_node_get_left(psl)));
      /* is it strong? */
      if (psl_node_get_right(psl) != PSL_NULL) res = res && _PRINT("!");
      return res && _PRINT(")");
    }

  case PSL_DIAMONDMINUSGT:
    {
      int res = _PRINT("(") &&
        _THROW(psl_node_get_left(psl_node_get_left(psl))) &&
        _PRINT(" <>-> ") && _THROW(psl_node_get_right(psl_node_get_left(psl)));
      /* is it strong? */
      if (psl_node_get_right(psl) != PSL_NULL) res = res && _PRINT("!");
      return res && _PRINT(")");
    }

  case PSL_PIPEEQGT:
    {
      int res = _PRINT("(") &&
        _THROW(psl_node_get_left(psl_node_get_left(psl))) &&
        _PRINT(" |=> ") && _THROW(psl_node_get_right(psl_node_get_left(psl)));
      /* is it strong? */
      if (psl_node_get_right(psl) != PSL_NULL) res = res && _PRINT("!");
      return res && _PRINT(")");
    }

  case PSL_ALWAYS:
  case PSL_NEVER:
  case PSL_EVENTUALLYBANG:
    {
      int res;
      switch (psl_node_get_op(psl)) {
      case PSL_ALWAYS: res = _PRINT("always "); break;
      case PSL_NEVER: res = _PRINT("never "); break;
      case PSL_EVENTUALLYBANG: res = _PRINT("eventually! "); break;
      default: error_unreachable_code(); /* no other cases */
      }

      if (!psl_node_is_sere(psl_node_get_left(psl))) res = res && _PRINT("(");

      res = res && _THROW(psl_node_get_left(psl));

      if (!psl_node_is_sere(psl_node_get_left(psl))) res = res && _PRINT(")");
      return res;
    }

  case PSL_WITHINBANG:
  case PSL_WITHIN:
  case PSL_WITHINBANG_:
  case PSL_WITHIN_:
    {
      int res;
      switch (psl_node_get_op(psl)) {
      case PSL_WITHINBANG: res = _PRINT("within!"); break;
      case PSL_WITHIN: res = _PRINT("within"); break;
      case PSL_WITHINBANG_: res = _PRINT("within!_"); break;
      case PSL_WITHIN_: res = _PRINT("within_"); break;
      default: error_unreachable_code(); /* no other cases here */
      }

    return res && _PRINT(" (") &&
      _THROW(psl_node_get_left(psl_node_get_left(psl))) &&
      _PRINT(", ") &&
      _THROW(psl_node_get_right(psl_node_get_left(psl))) &&
      _PRINT(") ") && _THROW(psl_node_get_right(psl));
    }

  case PSL_WHILENOTBANG:
  case PSL_WHILENOT:
  case PSL_WHILENOTBANG_:
  case PSL_WHILENOT_:
    {
      int res;
      switch (psl_node_get_op(psl)) {
      case PSL_WHILENOTBANG: res = _PRINT("whilenot!"); break;
      case PSL_WHILENOT: res = _PRINT("whilenot"); break;
      case PSL_WHILENOTBANG_: res = _PRINT("whilenot!_"); break;
      case PSL_WHILENOT_: res = _PRINT("whilenot_"); break;
      default: error_unreachable_code(); /* no other cases here */
      }

    return res && _PRINT(" (") &&
      _THROW(psl_node_get_left(psl)) && _PRINT(") ") &&
      _THROW(psl_node_get_right(psl));

    }

  case PSL_NEXT_EVENT_ABANG:
  case PSL_NEXT_EVENT_A:
  case PSL_NEXT_EVENT_EBANG:
  case PSL_NEXT_EVENT_E:
    {
      int res;
      switch (psl_node_get_op(psl)) {
      case PSL_NEXT_EVENT_ABANG: res = _PRINT("next_event_a!"); break;
      case PSL_NEXT_EVENT_A: res = _PRINT("next_event_a"); break;
      case PSL_NEXT_EVENT_EBANG: res = _PRINT("next_event_e!"); break;
      case PSL_NEXT_EVENT_E: res = _PRINT("next_event_e"); break;
      default: error_unreachable_code(); /* no other cases here */
      }

    return res && _PRINT(" (") &&
      _THROW(psl_node_get_right(psl_node_get_right(psl))) &&
      _PRINT(")") && _PRINT(" [") &&
      _THROW(psl_node_get_left(psl_node_get_right(psl))) &&
      _PRINT("]") && _PRINT(" (") &&
      _THROW(psl_node_get_left(psl)) && _PRINT(")");
    }


  case PSL_NEXT_EVENTBANG:
  case PSL_NEXT_EVENT:
    {
      int res;
      switch (psl_node_get_op(psl)) {
      case PSL_NEXT_EVENTBANG: res = _PRINT("next_event!"); break;
      case PSL_NEXT_EVENT: res = _PRINT("next_event"); break;
      default: error_unreachable_code(); /* no other cases here */
      }

      res = res && _PRINT(" (") &&
        _THROW(psl_node_get_right(psl_node_get_right(psl))) &&
        _PRINT(")");

      if (psl_node_get_left(psl_node_get_right(psl)) != PSL_NULL) {
        res = res && _PRINT(" [") &&
          _THROW(psl_node_get_left(psl_node_get_right(psl))) &&
          _PRINT("]");
      }

      return res && _PRINT(" (") &&
        _THROW(psl_node_get_left(psl)) && _PRINT(")");
    }

  case PSL_NEXT_ABANG:
  case PSL_NEXT_EBANG:
  case PSL_NEXT_A:
  case PSL_NEXT_E:
  case PSL_NEXTBANG:
  case PSL_NEXT:
    {
      int res;
      switch (psl_node_get_op(psl)) {
      case PSL_NEXT_ABANG: res = _PRINT("next_a!"); break;
      case PSL_NEXT_EBANG: res = _PRINT("next_e!"); break;
      case PSL_NEXT_A: res = _PRINT("next_a"); break;
      case PSL_NEXT_E: res = _PRINT("next_e"); break;
      case PSL_NEXTBANG: res = _PRINT("next!"); break;
      case PSL_NEXT: res = _PRINT("next"); break;
      default: error_unreachable_code(); /* no other cases here */
      }

      if (psl_node_get_right(psl) != PSL_NULL &&
          psl_node_get_left(psl_node_get_right(psl)) != PSL_NULL) {
        res = res && _PRINT(" [") &&
          _THROW(psl_node_get_left(psl_node_get_right(psl))) &&
          _PRINT("]");
      }

      return res && _PRINT(" (") &&
        _THROW(psl_node_get_left(psl)) && _PRINT(")");
    }

  case PSL_RANGE:
    return _THROW(psl_node_get_left(psl)) && _PRINT(":") &&
      _THROW(psl_node_get_right(psl));

    /* binary operators */
  case PSL_BEFOREBANG:
  case PSL_BEFORE:
  case PSL_BEFOREBANG_:
  case PSL_BEFORE_:
  case PSL_UNTILBANG:
  case PSL_UNTIL:
  case PSL_UNTILBANG_:
  case PSL_UNTIL_:
  case PSL_ABORT:
  case PSL_W:
  case PSL_OR:
  case PSL_CARET:
  case PSL_TILDE:
  case PSL_EQEQ:
  case PSL_PIPEPIPE:
  case PSL_AMPERSANDAMPERSAND:
    {
      int res;

      res = _PRINT("(") && _THROW(psl_node_get_left(psl)) && _PRINT(" ");

      switch (psl_node_get_op(psl)) {
      case PSL_BEFOREBANG: res = res && _PRINT("before!"); break;
      case PSL_BEFORE: res = res && _PRINT("before"); break;
      case PSL_BEFOREBANG_: res = res && _PRINT("before!_"); break;
      case PSL_BEFORE_: res = res && _PRINT("before_"); break;
      case PSL_UNTILBANG: res = res && _PRINT("until!"); break;
      case PSL_UNTIL: res = res && _PRINT("until"); break;
      case PSL_UNTILBANG_: res = res && _PRINT("until!_"); break;
      case PSL_UNTIL_: res = res && _PRINT("until_"); break;
      case PSL_ABORT:  res = res && _PRINT("abort"); break;
      case PSL_W: res = res && _PRINT("W"); break;
      case PSL_OR: res = res && _PRINT("or"); break;
      case PSL_CARET: res = res && _PRINT("^"); break;
      case PSL_TILDE: res = res && _PRINT("~"); break;
      case PSL_EQEQ: res = res && _PRINT("=="); break;
      case PSL_PIPEPIPE: res = res && _PRINT("||"); break;
      case PSL_AMPERSANDAMPERSAND: res = res && _PRINT("&&"); break;
      default: error_unreachable_code(); /* no other cases here */
      }

      return _PRINT(" ") && _THROW(psl_node_get_right(psl)) && _PRINT(")");
    }

  case PSL_X:
  case PSL_XBANG:
    {
      int res;

      switch (psl_node_get_op(psl)) {
      case PSL_X: res = _PRINT("X"); break;
      case PSL_XBANG: res = _PRINT("X!"); break;
      default: error_unreachable_code(); /* no other cases here */
      }

      if (psl_node_get_right(psl) != PSL_NULL) { /* simple next expression */
        res = res && _PRINT(" [") &&
          _THROW(psl_node_get_left(psl_node_get_right(psl))) && _PRINT("] ");
      }

      return res && _PRINT("(") &&
        _THROW(psl_node_get_left(psl)) && _PRINT(")");
    }

  case PSL_ITE:
      return _THROW(psl_node_get_ite_cond(psl)) &&
        _PRINT(" ? ") &&
        _THROW(psl_node_get_ite_then(psl)) && _PRINT(" : ") &&
        _THROW(psl_node_get_ite_else(psl));

  case PSL_WSELECT:
    return _PRINT("select(") &&
      _THROW(psl_node_get_left(psl)) &&
      _PRINT(", ") &&
      _THROW(psl_node_get_left(psl_node_get_right(psl))) &&
      _PRINT(", ") &&
      _THROW(psl_node_get_right(psl_node_get_right(psl))) &&
      _PRINT(")");
  default:
    ErrorMgr_internal_error(errmgr, "printer_psl_print_node: not supported type");
  }

  return 0;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The PrinterPsl class virtual finalizer

  Called by the class destructor
*/
static void printer_psl_finalize(Object_ptr object, void* dummy)
{
  PrinterPsl_ptr self = PRINTER_PSL(object);

  printer_psl_deinit(self);
  FREE(self);
}



/**AutomaticEnd***************************************************************/

