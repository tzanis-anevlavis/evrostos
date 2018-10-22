/* ---------------------------------------------------------------------------


  This file is part of the ``node'' package of NuSMV version 2.
  Copyright (C) 1998-2001 by CMU and FBK-irst.
  Copyright (C) 2011 by FBK.

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.

  For more information on NuSMV see <http://nusmv.fbk.eu>
  or email to <nusmv-users@fbk.eu>.
  Please report bugs to <nusmv-users@fbk.eu>.

  To contact the NuSMV development board, email to <nusmv@fbk.eu>.

-----------------------------------------------------------------------------*/

/*!
  \author Marco Roveri
  \brief The main routines of the <tt>node</tt> package.

  This file provides an abstract data type a la
  s-expression in LISP.

*/

#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/node/nodeInt.h"
#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#include <stdlib.h>

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

node_ptr car(node_ptr x)
{ return(x->left.nodetype);}

node_ptr cdr(node_ptr x)
{ return(x->right.nodetype); }

void setcar(node_ptr x, node_ptr y)
{
  /*nusmv_assert(!x->locked);*/
  x->left.nodetype = y;
}

void setcdr(node_ptr x, node_ptr y)
{
  /*nusmv_assert(!x->locked);*/
  x->right.nodetype = y;
}

void node_set_type (node_ptr x, int type)
{
  /*nusmv_assert(!x->locked);*/
  x->type = type;
}

int node_is_failure(node_ptr x)
{
  nusmv_assert(NULL != x);

  return FAILURE == node_get_type(x);
}


/*!
  \brief Tells if the given node is a numeric/boolean leaf

  Returns 0 if the given node is not a numeric/boolean/failure
   constant.  This is done a purely syntactic manner. To know if a
   *symbol* is constant declared within a symbol tablea, use method
   SymbTable_is_symbol_constant instead.
*/

/* WARNING [MD] This must be moved to ExprMgr */
/* WARNING [MD] This function should return a boolean */
int node_is_leaf(node_ptr node)
{
  nusmv_assert(NULL != node);

  switch (node_get_type(node)) {
  case NUMBER:
  case TRUEEXP:
  case FALSEEXP:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case FAILURE:
    return 1;

  default:
    break;
  }

  return 0;
}

boolean Node_is_symbol(node_ptr const node) {
  nusmv_assert(NULL != node);

  switch (node_get_type(node)) {
  case DOT:
  case ATOM:
  case BIT:
  case ARRAY:
    return true;

  default:
    break;
  }

  return false;
}

/*!
  \brief Syntactically checks if a node is a relational expression

  Syntactically checks if a node is a relational expression
*/

/* WARNING [MD] This must be moved to ExprMgr */
boolean Node_is_relation(node_ptr const expr)
{
  nusmv_assert(NULL != expr);

  switch (node_get_type(expr)) {
  case EQUAL:
  case NOTEQUAL:
  case GT:
  case GE:
  case LT:
  case LE:
    return true;

  default:
    break;
  }

  return false;
}

boolean Node_is_temporal_op(node_ptr const node)
{
  nusmv_assert(NULL != node);

  switch (node_get_type(node)) {
    /* CTL unary expressions */
  case EX: case AX: case EF: case AF: case EG: case AG:
  case ABU: case EBU:
  case EBF: case ABF: case EBG: case ABG:
    /* CTL binary expressions */
  case AU: case EU:
    /* LTL unary expressions */
  case OP_NEXT: case OP_PREC: case OP_NOTPRECNOT: case OP_GLOBAL:
  case OP_HISTORICAL: case OP_FUTURE: case OP_ONCE:
    /* LTL binary expressions */
  case UNTIL: case SINCE:
    return true;

  default:
    break;
  }

  return false;
}

node_ptr Node_find_boolean_type(NodeMgr_ptr nodemgr)
{
  return find_node(nodemgr, BOOLEAN, Nil, Nil);
}

node_ptr Node_find_integer_type(NodeMgr_ptr nodemgr)
{
  return find_node(nodemgr, INTEGER, Nil, Nil);
}

node_ptr Node_find_real_type(NodeMgr_ptr nodemgr)
{
  return find_node(nodemgr, REAL, Nil, Nil);
}

node_ptr Node_find_error_type(NodeMgr_ptr nodemgr)
{
  return Nil;
}

node_ptr new_list()  { return Nil; }

node_ptr copy_list(NodeMgr_ptr nodemgr, node_ptr list)
{
  node_ptr new_list;

  /* create a reversed copy of the list */
  for (new_list = Nil; list != Nil; list = cdr(list)) {
    /* nusmv_assert(CONS == node_get_type(list)); */

    new_list = cons(nodemgr, car(list), new_list);
  }

  /* reverse the created list */
  new_list = reverse(new_list);
  return new_list;
}

void free_list(NodeMgr_ptr nodemgr, node_ptr l)
{
  while(l != Nil) {
    node_ptr tmp = l;

    /* nusmv_assert(CONS == node_get_type(l)); */

    l = cdr(l);
    free_node(nodemgr, tmp);
  }
}

int is_list_empty(node_ptr list) { return list == Nil; }

int in_list(node_ptr n, node_ptr list)
{
  while (list) {
    /* nusmv_assert(CONS == node_get_type(list)); */

    if (car(list) == n) return(1);
    list = cdr(list);
  }
  return(0);
}

unsigned int llength(node_ptr r)
{
  unsigned int l = 0;

  while (r) {
    nusmv_assert(CONS == node_get_type(r));

    l++;
    r = cdr(r);
  }
  return(l);
}

node_ptr Node_conslist_add(NodeMgr_ptr nodemgr,
                           node_ptr list,
                           node_ptr element)
{
  nusmv_assert(Nil == list || CONS == node_get_type(list));

  return cons(nodemgr, element, list);
}

node_ptr Node_conslist_remove(NodeMgr_ptr nodemgr,
                              node_ptr list,
                              node_ptr element)
{
  node_ptr result_list;
  node_ptr subtrahend;
  node_ptr minuend;

  nusmv_assert(Nil == list || CONS == node_get_type(list));

  minuend = list;
  subtrahend = cons(nodemgr, element, Nil);
  result_list = node_subtract(nodemgr, subtrahend, minuend);

  free_list(nodemgr, minuend);
  free_node(nodemgr, subtrahend);

  return result_list;
}

boolean Node_is_conslist(node_ptr list)
{
  boolean retval = true;

  while (Nil != list) {
    if (CONS != node_get_type(list)) {
      retval = false;
      break;
    }

    list = cdr(list);
  }

  return retval;
}

node_ptr append(node_ptr x, node_ptr y)
{
  if (x == Nil) return y;

  /* nusmv_assert(CONS == node_get_type(x)); */
  /* nusmv_assert(Nil == y || */
  /*              CONS == node_get_type(y)); */

  x->right.nodetype = append(x->right.nodetype,y);

  return(x);
}

node_ptr append_ns(NodeMgr_ptr nodemgr, node_ptr x, node_ptr y)
{
  if(x==Nil)return(copy_list(nodemgr, y));

  /* nusmv_assert(CONS == node_get_type(x)); */
  /* nusmv_assert(Nil == y || */
  /* CONS == node_get_type(y)); */

  return(cons(nodemgr, car(x), append_ns(nodemgr, cdr(x), y)));
}

node_ptr reverse(node_ptr x)
{
  node_ptr y=Nil;

  while (x != Nil) {
    node_ptr z = x->right.nodetype;

    /* nusmv_assert(CONS == node_get_type(x)); */

    x->right.nodetype = y;
    y = x;
    x = z;
  }

  return(y);
}

node_ptr reverse_ns(NodeMgr_ptr nodemgr, node_ptr l)
{
  node_ptr res = Nil;
  node_ptr iter;

  for (iter = l; iter != Nil; iter = cdr(iter)) {
    /* nusmv_assert(CONS == node_get_type(l)); */

    res = cons(nodemgr, car(iter), res);
  }

  return res;
}

node_ptr last(node_ptr x)
{
  nusmv_assert(Nil != x);
  /* nusmv_assert(CONS == node_get_type(x)); */

  if (!cdr(x)) return car(x);
  return last(cdr(x));
}

node_ptr map(NodeMgr_ptr nodemgr, NPFN fun, node_ptr l)
{
  node_ptr res = Nil;
  node_ptr t;

  for (t=l; Nil != t; t=cdr(t)) {
    res = cons(nodemgr, (node_ptr)(*fun)(car(t)), res);
  }

  return reverse(res);
}

node_ptr map_param(NodeMgr_ptr nodemgr,
                   node_ptr (*fun)(node_ptr, void*),
                   node_ptr l, void* arg)
{
  node_ptr res = Nil;
  node_ptr t;

  for (t=l; Nil != t; t=cdr(t)) {
    res = cons(nodemgr, (node_ptr)(*fun)(car(t), arg), res);
  }

  return reverse(res);
}

node_ptr map2(NodeMgr_ptr nodemgr, NPFNN fun, node_ptr l1, node_ptr l2)
{
  node_ptr res = Nil;

  while (l1 != Nil) {
    node_ptr t;

    nusmv_assert(l2 != Nil);
    /* nusmv_assert(CONS == node_get_type(l1)); */
    /* nusmv_assert(CONS == node_get_type(l2)) */;

    t = (node_ptr)(*fun)(car(l1), car(l2));
    res = cons(nodemgr, t, res);

    l1 = cdr(l1); l2 = cdr(l2);
  }

  nusmv_assert(l2 == Nil);

  return reverse(res);
}

node_ptr odd_elements(NodeMgr_ptr nodemgr, node_ptr l)
{
  if (l == Nil) return(Nil);

  /* nusmv_assert(CONS == node_get_type(l)); */

  return(cons(nodemgr, car(l),even_elements(nodemgr, cdr(l))));
}

node_ptr even_elements(NodeMgr_ptr nodemgr, node_ptr l)
{
  if(l == Nil)return(Nil);

  /* nusmv_assert(CONS == node_get_type(l)); */

  return(odd_elements(nodemgr, cdr(l)));
}

void walk(VPFN fun, node_ptr l)
{
  if (l == Nil) return;

  /* nusmv_assert(CONS == node_get_type(l)); */

  (void)(*fun)(car(l));
  walk(fun,cdr(l));
}

node_ptr node_subtract(NodeMgr_ptr nodemgr,
                       node_ptr set1, node_ptr set2)
{
  if (set2 == Nil) {
    return Nil;
  }
  else if (Nil == set1) {
    return copy_list(nodemgr, set2);
  }

  /* nusmv_assert(CONS == node_get_type(set1)); */
  /* nusmv_assert(CONS == node_get_type(set2)); */

  if (in_list(car(set2),set1) == 1) {
    return node_subtract(nodemgr, set1,cdr(set2));
  }
  else {
    return cons(nodemgr, car(set2),node_subtract(nodemgr, set1,cdr(set2)));
  }
}

void swap_nodes(node_ptr *n1, node_ptr *n2)
{
  node_ptr temp = *n1;

  *n1 = *n2;
  *n2 = temp;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void node_init(NuSMVEnv_ptr env)
{
  NodeMgr_ptr mgr = NodeMgr_create(env);

  NuSMVEnv_set_value(env, ENV_NODE_MGR, mgr);
}

void node_quit(NuSMVEnv_ptr env)
{
  NodeMgr_ptr mgr = NuSMVEnv_remove_value(env, ENV_NODE_MGR);

  nusmv_assert(NODE_MGR(NULL) != mgr);

  NodeMgr_destroy(mgr);
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
