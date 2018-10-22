/* ---------------------------------------------------------------------------


   This file is part of the ``wff.w2w'' package of NuSMV version 2.
   Copyright (C) 2000-2011 by FBK-irst and University of Trento.

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
  \author Alessandro Cimatti, Lorenzo Delana, Alessandro Mariotti
  \brief Well Formed Formula to Negation Normal Form conversion

  \todo: Missing description

*/


#if HAVE_CONFIG_H
#include "nusmv-config.h"
#endif

#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/wff/wff.h"
#include "nusmv/core/wff/w2w/w2wInt.h"

#include "nusmv/core/parser/symbols.h" /* for constants */
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/assoc.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

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
static void
w2w_wff2nnf_hash_insert_entry(hash_ptr hash, NodeMgr_ptr nodemgr,
                              node_ptr wff, boolean polarity,
                              node_ptr nnf);

static node_ptr
w2w_wff2nnf_hash_lookup_entry(hash_ptr hash,NodeMgr_ptr nodemgr,
                              node_ptr wff, boolean polarity);

static node_ptr w2w_wff_expand_case_aux(const NuSMVEnv_ptr env, node_ptr wff);

static node_ptr w2w_wff_expand_case(const NuSMVEnv_ptr env, node_ptr wff);

static node_ptr w2w_wff_mk_nnf(hash_ptr, NodeMgr_ptr nodemgr,
                               node_ptr wff, boolean pol);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

node_ptr Wff2Nnf(const NuSMVEnv_ptr env, node_ptr wff)
{
  hash_ptr wff2nnf;
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  wff2nnf = NuSMVEnv_get_handled_hash_ptr(env, ENV_W2W_WFF2NNF_HASH);

  return w2w_wff_mk_nnf(wff2nnf, nodemgr, wff, true);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Memoizes the given entry in the wff2nff memoization hash

  Memoizes the given entry in the wff2nff memoization hash
*/
static void w2w_wff2nnf_hash_insert_entry(hash_ptr wff2nnf, NodeMgr_ptr nodemgr,
                                          node_ptr wff, boolean polarity,
                                          node_ptr nnf)
{
  nusmv_assert(wff2nnf != (hash_ptr) NULL);
  insert_assoc(wff2nnf, find_node(nodemgr, CONS, wff, (node_ptr) polarity), nnf);
}

/*!
  \brief Looks up in the wff2nff memoization hash for
   the given entry

  Looks up in the wff2nff memoization hash for
   the given entry
*/
static node_ptr w2w_wff2nnf_hash_lookup_entry(hash_ptr wff2nnf,
                                              NodeMgr_ptr nodemgr,
                                              node_ptr wff, boolean polarity)
{
  nusmv_assert(wff2nnf != (hash_ptr) NULL);
  return find_assoc(wff2nnf, find_node(nodemgr, CONS, wff, (node_ptr) polarity));
}

/*!
  \brief Expands the given case expression

  Expands the given case expression
*/
static node_ptr w2w_wff_expand_case(const NuSMVEnv_ptr env, node_ptr wff)
{
  node_ptr res;

  nusmv_assert(CASE == node_get_type(wff) || IFTHENELSE == node_get_type(wff));
  res = w2w_wff_expand_case_aux(env, wff);

  return res;
}

/*!
  \brief Aux fun of w2w_wff_expand_case

  Aux fun of w2w_wff_expand_case
*/
static node_ptr w2w_wff_expand_case_aux(const NuSMVEnv_ptr env, node_ptr wff)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  if (CASE == node_get_type(wff) || IFTHENELSE == node_get_type(wff)) {

    node_ptr cur_cond  = car(car(wff));
    node_ptr cur_rslt  = cdr(car(wff));
    node_ptr case_rest = cdr(wff);
    node_ptr res;

    nusmv_assert(node_get_type(car(wff)) == COLON);

    /* here lazy evaluation is required to get rid of FAILURE node
       in case-expressions (see this function below)
    */
    if (cur_cond == Wff_make_truth(nodemgr)) return cur_rslt;
    if (cur_cond == Wff_make_falsity(nodemgr)) return case_rest;

    res = Wff_make_or(nodemgr,  Wff_make_and(nodemgr, cur_cond, cur_rslt),
                       Wff_make_and(nodemgr, Wff_make_not(nodemgr, cur_cond),
                                    w2w_wff_expand_case_aux(env, case_rest))
                       );
    return res;

  }
  else {
    const ErrorMgr_ptr errmgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

    if (FAILURE == node_get_type(wff)) {
      if (ErrorMgr_failure_get_kind(errmgr, wff) == FAILURE_CASE_NOT_EXHAUSTIVE) {
        ErrorMgr_warning_case_not_exhaustive(errmgr, wff);
        /* forces a default */
        return Wff_make_truth(nodemgr);
      }
      else if (ErrorMgr_failure_get_kind(errmgr, wff) == FAILURE_DIV_BY_ZERO) {
        ErrorMgr_warning_possible_div_by_zero(errmgr, wff);
        /* forces a default */
        return Wff_make_truth(nodemgr);
      }
      else if (ErrorMgr_failure_get_kind(errmgr, wff) == FAILURE_ARRAY_OUT_OF_BOUNDS) {
        ErrorMgr_warning_possible_array_out_of_bounds(errmgr, wff);
        /* forces a default */
        return Wff_make_truth(nodemgr);
      }
      else {
        ErrorMgr_report_failure_node(errmgr, wff); /* some error in the input expr */
        error_unreachable_code();
      }
    }

    return wff;
  }
}

/*!
  \brief The private function that does the actual
                       wff2nnf conversion

  The private function that does the actual
                       wff2nnf conversion
*/
static node_ptr w2w_wff_mk_nnf(hash_ptr wff2nnf, NodeMgr_ptr nodemgr,
                               node_ptr wff, boolean pol)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(nodemgr));
  node_ptr res;

  /* if reached a Nil branch then end recursion with a Nil node */
  if (wff == Nil) return Nil;

  res = w2w_wff2nnf_hash_lookup_entry(wff2nnf, nodemgr, wff, pol);
  if (res != (node_ptr) NULL) return res;

  switch (node_get_type(wff)) {
  case TRUEEXP:
    if (pol) res = Wff_make_truth(nodemgr);
    else res = Wff_make_falsity(nodemgr);  /* !1 <-> 0 */
    break;

  case FALSEEXP:
    if (pol) res = Wff_make_falsity(nodemgr);
    else res = Wff_make_truth(nodemgr);    /* !0 <-> 1 */
    break;

  case NOT:
    /* !(a) <-> (!a) */
    /* !(!a) <-> a */
    res = w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), !pol);
    break;

  case AND:
    if (pol) {
      res =
        Wff_make_and(nodemgr,
                     w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), true),
                     w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), true));
    }
    else {
      /* !(a & b) <-> (!a | !b) */
      res = Wff_make_or(nodemgr,
                        w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), false),
                        w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), false));
    }
    break;

  case OR:
    if (pol) res = Wff_make_or(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), true),
                               w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), true));
    else {
      /* !(a | b) <-> (!a & !b) */
      res = Wff_make_and(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), false),
                         w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), false));
    }
    break;

  case IMPLIES:
    if (pol) {
      /* (a -> b) <-> !(a & !b) <-> (!a | b) */
      res = Wff_make_or(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), false),
                        w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), true));
    }
    else {
      /* !(a -> b) <-> (a & !b) */
      res = Wff_make_and(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), true),
                         w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), false));
    }
    break;

  case IFF:
    if (pol) {
      /* (a <-> b) <->
         !(a & !b) & !(b & !a) <->
         (!a | b) & (!b | a) */
      res = Wff_make_and(nodemgr,  Wff_make_or(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), false),
                                      w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), true)),
                          Wff_make_or(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), true),
                                      w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), false)) );
    }
    else {
      /* !(a <-> b) <->
         !(!(a & !b) & !(b & !a)) <->
         (a & !b) | (b & !a) */
      res = Wff_make_or(nodemgr,  Wff_make_and(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), true),
                                      w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), false)),
                         Wff_make_and(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), false),
                                      w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), true)) );
    }
    break;

  case XOR:
    if (pol) {
      /* (a xor b) <-> (a & !b) | (!a & b) */
      res = Wff_make_or(nodemgr,  Wff_make_and(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), true),
                                      w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), false)),
                         Wff_make_and(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), false),
                                      w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), true)) );
    }
    else {
      /* !(a xnor b) <-> (a | !b) & (!a | b) */
      res = Wff_make_and(nodemgr,  Wff_make_or(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), true),
                                      w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), false)),
                          Wff_make_or(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), false),
                                      w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), true)) );
    }
    break;

  case XNOR:
    if (pol) {
      /* (a xnor b) <-> (!a | b) & (!b | a) */
      res = Wff_make_and(nodemgr,  Wff_make_or(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), false),
                                      w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), true)),
                          Wff_make_or(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), true),
                                      w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), false)) );
    }
    else {
      /* !(a xnor b) <-> (a & !b) | (!a & b) */
      res = Wff_make_or(nodemgr,  Wff_make_and(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), true),
                                      w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), false)),
                         Wff_make_and(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), false),
                                      w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), true)) );
    }
    break;

  case OP_NEXT:
    /* !X(a) <-> X(!a) */
    res = Wff_make_opnext(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol));
    break;

  case OP_PREC:
    /* !Y(a) <-> Z(!a) */
    if (pol) res = Wff_make_opprec(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol));
    else res = Wff_make_opnotprecnot(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol));
    break;

  case OP_NOTPRECNOT:
    /* !Z(a) <-> Y(!a) */
    if (pol) res = Wff_make_opnotprecnot(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol));
    else res = Wff_make_opprec(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol));
    break;

  case OP_GLOBAL:
    if (pol) res = Wff_make_globally(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol));
    else {
      /* !G(a) <-> F(!a) */
      res = Wff_make_eventually(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol));
    }
    break;

  case OP_HISTORICAL:
    if (pol) res = Wff_make_historically(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol));
    else {
      /* !H(a) <-> O(!a) */
      res = Wff_make_once(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol));
    }
    break;

  case OP_FUTURE:
    if (pol) res = Wff_make_eventually(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol));
    else {
      /* !F(a) <-> G(!a) */
      res = Wff_make_globally(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol));
    }
    break;

  case OP_ONCE:
    if (pol) res = Wff_make_once(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol));
    else {
      /* !O(a) <-> H(!a) */
      res = Wff_make_historically(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol));
    }
    break;

  case UNTIL:
    if (pol) res = Wff_make_until(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol),
                                  w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), pol));
    else {
      /* !(a U b) <-> (!a V !b) */
      res = Wff_make_releases(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol),
                              w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), pol));
    }
    break;

  case SINCE:
    if (pol) res = Wff_make_since(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol),
                                  w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), pol));
    else {
      /* !(a S b) <-> (!a T !b) */
      res = Wff_make_triggered(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol),
                               w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), pol));
    }
    break;

  case RELEASES:
    if (pol) res = Wff_make_releases(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol),
                                     w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), pol));
    else {
      /* !(a V b) <-> (!a U !b) */
      res = Wff_make_until(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol),
                           w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), pol));
    }
    break;

  case TRIGGERED:
    if (pol) res = Wff_make_triggered(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol),
                                      w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), pol));
    else {
      /* !(a T b) <-> (!a S !b) */
      res = Wff_make_since(nodemgr, w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol),
                           w2w_wff_mk_nnf(wff2nnf, nodemgr, cdr(wff), pol));
    }
    break;

  case IFTHENELSE:
  case CASE:
    {
      node_ptr nocase_wff = w2w_wff_expand_case(env, wff);
      res = w2w_wff_mk_nnf(wff2nnf, nodemgr, nocase_wff, pol);
      break;
    }

  case BIT:
  case DOT:
  case ARRAY:
    /* it is a bexp var */
    if (pol) res = wff;
    else res = Wff_make_not(nodemgr, wff);
    break;

    /* Uninterpreted functions have not to be traversed, since their parameters
       can be of a type different thatn boolean. Also, they should be treated
       like frozen vars */
  case NFUNCTION:
    if (pol) res = wff;
    else res = Wff_make_not(nodemgr, wff);
    break;

  case ATOM:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
    {
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

      /* internal format for atoms that should have been previously
         hidden within DOT and ARRAY */
      ErrorMgr_internal_error(errmgr, "w2w_wff_mk_nnf: unexpected leaf %d\n",
                              node_get_type(wff));
      res = (node_ptr) NULL;
    }
    break;

  case MOD:
    {
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

      ErrorMgr_internal_error(errmgr, "w2w_wff_mk_nnf: unexpected mod operator\n");
    }
    /* stop recursion when a predicate is found */
  case LE: case LT:
  case GE: case GT:
  case EQUAL: case NOTEQUAL:
  case SETIN:
    res = (pol) ? wff : Wff_make_not(nodemgr, wff);
      break;

      /* recur inside it */
  case NEXT:
    {
      res = w2w_wff_mk_nnf(wff2nnf, nodemgr, car(wff), pol);
      res = Wff_make_next(nodemgr, res);
      break;
    }

  default:
    {
      const ErrorMgr_ptr errmgr =
        ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

      ErrorMgr_internal_error(errmgr, "w2w_wff_mk_nnf: unexpected TOKEN %d\n",
                              node_get_type(wff));
    }

  }

  if (res != (node_ptr) NULL) {
    w2w_wff2nnf_hash_insert_entry(wff2nnf, nodemgr, wff, pol, res);
  }

  return res;
}
