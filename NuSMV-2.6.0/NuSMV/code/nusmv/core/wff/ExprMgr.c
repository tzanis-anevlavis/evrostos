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
  \brief Implementation of class 'ExprMgr'

  \todo: Missing description

*/


#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/wff/ExprMgr.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/WordNumberMgr.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"
#include "nusmv/core/utils/WordNumberMgr.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/enc/operators.h"
#include "nusmv/core/utils/EnvObject.h"
#include "nusmv/core/utils/EnvObject_private.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/
#if NUSMV_NO_POINTER_ORDERING
#  define DISABLE_EXPR_POINTERS_ORDERING 1
#else
#if !defined(_MSC_VER)
#  warning "Compiling in non-deterministic mode"
#endif
#  define DISABLE_EXPR_POINTERS_ORDERING 0
#endif

/*
  \brief The name of the hash used for the simplification within
  the Symbol Table, when global hash is enabled
 */
#define EXPR_SIMPLIFIER_HASH "___expr_simplifier_hash___"


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct ExprMgr_TAG
{
  INHERITS_FROM(EnvObject);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */

  NodeMgr_ptr nodes;
  ErrorMgr_ptr errors;
  WordNumberMgr_ptr words;


  Expr_ptr expr_true;
  Expr_ptr expr_false;

} ExprMgr;



/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define FN(self, type, r, l)                            \
  NodeMgr_find_node(self->nodes, type, r, l)

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/


static void expr_mgr_finalize(Object_ptr object, void* dummy);

static void expr_mgr_init(ExprMgr_ptr self, const NuSMVEnv_ptr env);
static void expr_mgr_deinit(ExprMgr_ptr self);


static Expr_ptr expr_simplify_aux(ExprMgr_ptr self,
                                  SymbTable_ptr st, Expr_ptr expr,
                                  hash_ptr hash);

static Expr_ptr expr_bool_to_word1(const ExprMgr_ptr self, const Expr_ptr a);

static int expr_get_curr_time(ExprMgr_ptr self,
                              SymbTable_ptr st,
                              node_ptr expr,
                              hash_ptr cache);
static void expr_get_curr_time_interval(ExprMgr_ptr self,
                                        SymbTable_ptr st,
                                        node_ptr expr,
                                        hash_ptr cache,
                                        int* min,
                                        int* max);
static Expr_ptr expr_timed_to_untimed(const ExprMgr_ptr self,
                                      SymbTable_ptr st, Expr_ptr expr,
                                      int curr_time, boolean in_next,
                                      hash_ptr cache);

static boolean expr_is_timed_aux(Expr_ptr expr, hash_ptr cache);

static boolean expr_is_bool(const ExprMgr_ptr self, const Expr_ptr a);

static inline boolean expr_is_wordnumber_max(const ExprMgr_ptr self,
                                             const WordNumber_ptr word,
                                             const int type);

static Expr_ptr move_next_to_leaves_recur(const ExprMgr_ptr self,
                                          SymbTable_ptr st,
                                          Expr_ptr expr,
                                          boolean in_next);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

ExprMgr_ptr ExprMgr_create(const NuSMVEnv_ptr env)
{
  ExprMgr_ptr self = ALLOC(ExprMgr, 1);
  EXPR_MGR_CHECK_INSTANCE(self);

  expr_mgr_init(self, env);
  return self;
}

void ExprMgr_destroy(ExprMgr_ptr self)
{
  EXPR_MGR_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

NodeMgr_ptr ExprMgr_get_node_manager(const ExprMgr_ptr self)
{
  return self->nodes;
}

Expr_ptr ExprMgr_true(const ExprMgr_ptr self)
{
  return EXPR(self->expr_true);
}

Expr_ptr ExprMgr_false(const ExprMgr_ptr self)
{
  return EXPR(self->expr_false);
}

boolean ExprMgr_is_true(const ExprMgr_ptr self, const Expr_ptr expr)
{
  return expr == self->expr_true || TRUEEXP == node_get_type(NODE_PTR(expr));
}

boolean ExprMgr_is_false(const ExprMgr_ptr self, const Expr_ptr expr)
{
  return expr == self->expr_false || FALSEEXP == node_get_type(NODE_PTR(expr));
}

Expr_ptr ExprMgr_boolean_range(const ExprMgr_ptr self)
{
  return EXPR(FN(self, CONS, self->expr_false, FN(self, CONS, self->expr_true, Nil)));
}

boolean ExprMgr_is_boolean_range(const ExprMgr_ptr self, Expr_ptr expr)
{
  return (expr == ExprMgr_boolean_range(self));
}

Expr_ptr ExprMgr_number(const ExprMgr_ptr self, int value)
{
  return EXPR(FN(self, NUMBER, NODE_FROM_INT(value), Nil));
}

boolean ExprMgr_is_number(const ExprMgr_ptr self, const Expr_ptr expr, const int value)
{
  return (NUMBER == node_get_type(expr)) && (NODE_TO_INT(car(expr)) == value);
}

Expr_ptr ExprMgr_word_max_value(const ExprMgr_ptr self,
                                const int size, const int type) {
  if (type == NUMBER_UNSIGNED_WORD) {
    return EXPR(FN(self, type,
                   NODE_PTR(WordNumberMgr_max_unsigned_value(self->words, size)),
                   Nil));
  }
  else if (type == NUMBER_SIGNED_WORD) {
    return EXPR(FN(self, type,
                   NODE_PTR(WordNumberMgr_max_signed_value(self->words, size)),
                   Nil));
  }
  nusmv_assert(false);
  return Nil;
}

Expr_ptr ExprMgr_and(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  /* boolean */
  if (a == EXPR(NULL) && b == EXPR(NULL)) return self->expr_true;
  if (a == EXPR(NULL) || ExprMgr_is_true(self, a))  return b;
  if (b == EXPR(NULL) || ExprMgr_is_true(self, b))  return a;
  if (ExprMgr_is_false(self, a)) return a;
  if (ExprMgr_is_false(self, b)) return b;
  if (a == b)           return a;
  {
    int ta = node_get_type(NODE_PTR(a)); int tb = node_get_type(NODE_PTR(b));

#if 0
    /* This simplification is correct iff both a and b are boolean. If
       they are word a proper constant expression of size of a (or b)
       shall be constructed with all bits to FALSE.
     */
    if ((ta == NOT && EXPR(car(NODE_PTR(a))) == b) ||
        (tb == NOT && EXPR(car(NODE_PTR(b))) == a)) return self->expr_false;
#endif

    /* bitwise */
    if ((ta == NUMBER_UNSIGNED_WORD && tb == NUMBER_UNSIGNED_WORD) ||
        (ta == NUMBER_SIGNED_WORD && tb == NUMBER_SIGNED_WORD)) {

#if ! DISABLE_EXPR_POINTERS_ORDERING
      /* Take in count pointers to increment node sharing */
      if (car(NODE_PTR(a)) > car(NODE_PTR(b))) {
        return EXPR(FN(self, ta,
                       NODE_PTR(WordNumberMgr_and(self->words, WORD_NUMBER(car(b)),
                                                  WORD_NUMBER(car(a)))),
                       Nil));
      }
#endif

      return EXPR(FN(self, ta,
                     NODE_PTR(WordNumberMgr_and(self->words, WORD_NUMBER(car(a)),
                                             WORD_NUMBER(car(b)))),
                     Nil));
    }
  }

#if ! DISABLE_EXPR_POINTERS_ORDERING
  /* no simplification is possible, but take in count pointers for
     better node sharing */
  if (a > b) {
    return EXPR(FN(self, AND, NODE_PTR(b), NODE_PTR(a)));
  }
#endif

  return EXPR(FN(self, AND, NODE_PTR(a), NODE_PTR(b)));
}

Expr_ptr ExprMgr_and_nil(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  Expr_ptr result;
  Expr_ptr atmp, btmp;

  atmp = (EXPR(NULL) != a) ? a : self->expr_true;
  btmp = (EXPR(NULL) != b) ? b : self->expr_true;
  result = ExprMgr_and(self, atmp, btmp);

  return result;
}

Expr_ptr ExprMgr_and_from_list(const ExprMgr_ptr self, node_ptr list, SymbTable_ptr symb_table)
{

  int type;
  if (list == Nil) return self->expr_true;

  type = node_get_type(list);
  if (CONS != type && AND != type) {
    return ExprMgr_resolve(self, symb_table,
                        type, EXPR(car(list)), EXPR(cdr(list)));
  }

  /* recursive step */
  return ExprMgr_and_nil(self, EXPR(car(list)),
                      ExprMgr_and_from_list(self, cdr(list), symb_table));
}

Expr_ptr ExprMgr_not(const ExprMgr_ptr self, const Expr_ptr expr)
{
  /* boolean */
  if (ExprMgr_is_true(self, expr)) return self->expr_false;
  if (ExprMgr_is_false(self, expr)) return self->expr_true;

  {
    int ta = node_get_type(NODE_PTR(expr));
    if (NOT == ta) return EXPR(car(NODE_PTR(expr)));

    /* bitwise */
    if (ta == NUMBER_UNSIGNED_WORD || ta == NUMBER_SIGNED_WORD) {
      return FN(self, ta,
                NODE_PTR(WordNumberMgr_not(self->words, WORD_NUMBER(car(expr)))),
                Nil);
    }
  }

  /* no simplification is possible */
  return EXPR( FN(self, NOT, NODE_PTR(expr), Nil) );
}

Expr_ptr ExprMgr_or(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  nusmv_assert(NULL != a);
  nusmv_assert(NULL != b);

  /* boolean */
  if (ExprMgr_is_true(self, a)) return a;
  if (ExprMgr_is_true(self, b)) return b;
  if (ExprMgr_is_false(self, a)) return b;
  if (ExprMgr_is_false(self, b)) return a;
  if (a==b) return a;
  {
    int ta = node_get_type(NODE_PTR(a)); int tb = node_get_type(NODE_PTR(b));

#if 0
    /* This simplification is correct iff both a and b are boolean. If
       they are word a proper constant expression of size of a (or b)
       shall be constructed with all bits to TRUE.
     */
    if ((ta == NOT && EXPR(car(NODE_PTR(a))) == b) ||
        (tb == NOT && EXPR(car(NODE_PTR(b))) == a)) return self->expr_true;
#endif

    if ((ta == AND) && (tb == AND)) {
      /* ((A & B) || (A & !B)) ---> A */
      /* ((A & !B) || (A & B)) ---> A */
      if ((car(NODE_PTR(a)) == car(NODE_PTR(b))) &&
          (((node_get_type(cdr(NODE_PTR(b))) == NOT) &&
            (car(cdr(NODE_PTR(b))) == cdr(a))) ||
           ((node_get_type(cdr(NODE_PTR(a))) == NOT) &&
            (car(cdr(NODE_PTR(a))) == cdr(NODE_PTR(b)))))) {
        return car(a);
      }

      /* ((A & B) || (!A & B)) ---> B */
      /* ((!A & B) || ( A & B)) ---> B */
      if ((cdr(a) == cdr(b)) &&
          (((node_get_type(car(b)) == NOT) &&
            (car(car(b)) == car(a))) ||
           ((node_get_type(car(a)) == NOT) &&
            (car(car(a)) == car(b))))) {
        return cdr(a);
      }

      /* (( A & B) || (B & !A)) ---> B */
      /* ((!A & B) || (B & A)) ---> B */
      if ((cdr(a) == car(b)) &&
          (((node_get_type(cdr(b)) == NOT) &&
            (car(cdr(b)) == car(a))) ||
           ((node_get_type(car(a)) == NOT) &&
            (car(car(a)) == cdr(b))))) {
        return cdr(a);
      }

      /* ((A & B) || (!B & A)) ---> A */
      /* ((A & !B) || ( B & A)) ---> A */
      if ((car(a) == cdr(b)) &&
          (((node_get_type(car(b)) == NOT) &&
            (car(car(b)) == cdr(a))) ||
           (((node_get_type(cdr(a)) == NOT) &&
             (car(cdr(a)) == car(b)))))) {
        return car(a);
      }
    }

    /* bitwise */
    if ((ta == NUMBER_UNSIGNED_WORD && tb == NUMBER_UNSIGNED_WORD) ||
        (ta == NUMBER_SIGNED_WORD && tb == NUMBER_SIGNED_WORD)) {

#if ! DISABLE_EXPR_POINTERS_ORDERING
      /* Swap if needed, for better sharing */
      if (car(a) > car(b)) {
        return FN(self, ta,
                  (node_ptr)WordNumberMgr_or(self->words, WORD_NUMBER(car(b)),
                                             WORD_NUMBER(car(a))),
                  Nil);
      }
#endif
      return FN(self, ta,
                (node_ptr)WordNumberMgr_or(self->words, WORD_NUMBER(car(a)),
                                           WORD_NUMBER(car(b))),
                Nil);
    }
  }

#if ! DISABLE_EXPR_POINTERS_ORDERING
  /* no simplification is possible, but improve node sharing by
     ordering the children */
  if (a > b) {
    return EXPR( FN(self, OR, NODE_PTR(b), NODE_PTR(a)) );
  }
#endif

  return EXPR( FN(self, OR, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_xor(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  /* boolean */
  if (ExprMgr_is_true(self, a)) return ExprMgr_not(self, b);
  if (ExprMgr_is_true(self, b)) return ExprMgr_not(self, a);
  if (ExprMgr_is_false(self, a)) return b;
  if (ExprMgr_is_false(self, b)) return a;

  {
    int ta = node_get_type(a); int tb = node_get_type(b);

#if 0
    /* This simplification is correct iff both a and b are boolean. If
       they are word a proper constant expression of size of a (or b)
       shall be constructed with all bits to TRUE.
     */

    if ((ta == NOT && car(a) == b) ||
        (tb == NOT && car(b) == a)) return self->expr_true;
#endif

    /* bitwise */
    if ((ta == NUMBER_UNSIGNED_WORD && tb == NUMBER_UNSIGNED_WORD) ||
        (ta == NUMBER_SIGNED_WORD && tb == NUMBER_SIGNED_WORD)) {

#if ! DISABLE_EXPR_POINTERS_ORDERING
      if (car(a) > car(b)) {
        return FN(self, ta,
                  (node_ptr)WordNumberMgr_xor(self->words, WORD_NUMBER(car(b)),
                                              WORD_NUMBER(car(a))),
                  Nil);
      }
#endif
      return FN(self, ta,
                (node_ptr)WordNumberMgr_xor(self->words, WORD_NUMBER(car(a)),
                                            WORD_NUMBER(car(b))),
                Nil);
    }
  }

#if ! DISABLE_EXPR_POINTERS_ORDERING
  /* no simplification is possible, order children by pointer for
     better node sharing */
  if (a > b) {
    return EXPR( FN(self, XOR, NODE_PTR(b), NODE_PTR(a)) );
  }
#endif

  return EXPR( FN(self, XOR, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_xnor(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  /* boolean */
  if (ExprMgr_is_true(self, a)) return b;
  if (ExprMgr_is_true(self, b)) return a;
  if (ExprMgr_is_false(self, a)) return ExprMgr_not(self, b);
  if (ExprMgr_is_false(self, b)) return ExprMgr_not(self, a);

  {
    int ta = node_get_type(a); int tb = node_get_type(b);

#if 0
    /* This simplification is correct iff both a and b are boolean. If
       they are word a proper constant expression of size of a (or b)
       shall be constructed with all bits to FALSE.
     */
    if ((ta == NOT && car(a) == b) ||
        (tb == NOT && car(b) == a)) return self->expr_false;
#endif

    /* bitwise */
    if ((ta == NUMBER_UNSIGNED_WORD && tb == NUMBER_UNSIGNED_WORD) ||
        (ta == NUMBER_SIGNED_WORD && tb == NUMBER_SIGNED_WORD)) {

#if ! DISABLE_EXPR_POINTERS_ORDERING
      if (car(a) > car(b)) {
        return FN(self, ta,
                  (node_ptr)WordNumberMgr_xnor(self->words, WORD_NUMBER(car(b)),
                                               WORD_NUMBER(car(a))),
                  Nil);
      }
#endif

      return FN(self, ta,
                (node_ptr)WordNumberMgr_xnor(self->words, WORD_NUMBER(car(a)),
                                             WORD_NUMBER(car(b))),
                Nil);
    }
  }

#if ! DISABLE_EXPR_POINTERS_ORDERING
  /* no simplification is possible, remember pointer ordering  */
  if (a > b) {
    return EXPR( FN(self, XNOR, NODE_PTR(b), NODE_PTR(a)) );
  }
#endif

  return EXPR( FN(self, XNOR, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_iff(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  /* boolean */
  if (ExprMgr_is_true(self, a)) return b;
  if (ExprMgr_is_true(self, b)) return a;
  if (ExprMgr_is_false(self, a)) return ExprMgr_not(self, b);
  if (ExprMgr_is_false(self, b)) return ExprMgr_not(self, a);

  {
    int ta = node_get_type(a); int tb = node_get_type(b);

#if 0
    /* This simplification is correct iff both a and b are boolean. If
       they are word a proper constant expression of size of a (or b)
       shall be constructed with all bits to FALSE.
     */
    if ((ta == NOT && car(a) == b) ||
        (tb == NOT && car(b) == a)) return self->expr_false;
#endif

    /* bitwise */
    if ((ta == NUMBER_UNSIGNED_WORD && tb == NUMBER_UNSIGNED_WORD) ||
        (ta == NUMBER_SIGNED_WORD && tb == NUMBER_SIGNED_WORD)) {

#if ! DISABLE_EXPR_POINTERS_ORDERING
      if (car(a) > car(b)) {
        return FN(self, ta,
                  (node_ptr)WordNumberMgr_iff(self->words, WORD_NUMBER(car(b)),
                                              WORD_NUMBER(car(a))),
                  Nil);
      }
#endif

      return FN(self, ta,
                (node_ptr)WordNumberMgr_iff(self->words, WORD_NUMBER(car(a)),
                                            WORD_NUMBER(car(b))),
                Nil);
    }
  }

#if ! DISABLE_EXPR_POINTERS_ORDERING
  /* no simplification is possible, remember pointer ordering */
  if (a > b) {
    return EXPR( FN(self, IFF, NODE_PTR(b), NODE_PTR(a)) );
  }
#endif

  return EXPR( FN(self, IFF, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_simplify_iff(const ExprMgr_ptr self, const SymbTable_ptr st,
                              const Expr_ptr a, const Expr_ptr b)
{
  /* boolean */
  if (ExprMgr_is_true(self, a)) return b;
  if (ExprMgr_is_true(self, b)) return a;
  if (ExprMgr_is_false(self, a)) return ExprMgr_not(self, b);
  if (ExprMgr_is_false(self, b)) return ExprMgr_not(self, a);

  {
    int ta = node_get_type(a); int tb = node_get_type(b);

#if 0
    /* This simplification is correct iff both a and b are boolean. If
       they are word a proper constant expression of size of a (or b)
       shall be constructed with all bits to FALSE.
     */
    if ((ta == NOT && car(a) == b) ||
        (tb == NOT && car(b) == a)) return self->expr_false;
#endif

    /* bitwise */
    if ((ta == NUMBER_UNSIGNED_WORD && tb == NUMBER_UNSIGNED_WORD) ||
        (ta == NUMBER_SIGNED_WORD && tb == NUMBER_SIGNED_WORD)) {

#if ! DISABLE_EXPR_POINTERS_ORDERING
      if (car(a) > car(b)) {
        return FN(self, ta,
                  (node_ptr)WordNumberMgr_iff(self->words, WORD_NUMBER(car(b)),
                                              WORD_NUMBER(car(a))),
                  Nil);
      }
#endif

      return FN(self, ta,
                (node_ptr)WordNumberMgr_iff(self->words, WORD_NUMBER(car(a)),
                                            WORD_NUMBER(car(b))),
                Nil);
    }
  }

  if (SYMB_TABLE(NULL) != st) {
    SymbType_ptr at, bt;
    TypeChecker_ptr tc = SymbTable_get_type_checker(st);

    at = TypeChecker_get_expression_type(tc, NODE_PTR(a), Nil);
    bt = TypeChecker_get_expression_type(tc, NODE_PTR(b), Nil);

    if (!SymbType_is_word(at) || !SymbType_is_word(bt)) {
      /* For non word expressions A <-> A is true */
      if (a == b) {
        return self->expr_true;
      }
    }
  }

#if ! DISABLE_EXPR_POINTERS_ORDERING
  /* no simplification is possible, but order pointers */
  if (a > b) {
    return EXPR( FN(self, IFF, NODE_PTR(b), NODE_PTR(a)) );
  }
#endif

  return EXPR( FN(self, IFF, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_implies(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  /* boolean */
  if (ExprMgr_is_true(self, a))  return b;
  if (ExprMgr_is_false(self, a)) return self->expr_true;
  if (ExprMgr_is_true(self, b))  return self->expr_true;
  if (ExprMgr_is_false(self, b)) return ExprMgr_not(self, a);

  {
    int ta = node_get_type(a); int tb = node_get_type(b);

    if ((ta == NOT && car(a) == b) ||
        (tb == NOT && car(b) == a)) return b;

    /* bitwise */
    if ((ta == NUMBER_UNSIGNED_WORD && tb == NUMBER_UNSIGNED_WORD) ||
        (ta == NUMBER_SIGNED_WORD && tb == NUMBER_SIGNED_WORD)) {
      return FN(self, ta,
                (node_ptr)WordNumberMgr_implies(self->words,
                                                WORD_NUMBER(car(a)),
                                                WORD_NUMBER(car(b))),
                Nil);
    }
  }

  /* no simplification is possible */
  return ExprMgr_or(self, ExprMgr_not(self, a), b);
}

Expr_ptr ExprMgr_ite(const ExprMgr_ptr self, const Expr_ptr cond,
                     const Expr_ptr t,
                     const Expr_ptr e,
                     const SymbTable_ptr symb_table)
{
  node_ptr tmp;

  if (ExprMgr_is_true(self, cond)) return t;
  if (ExprMgr_is_false(self, cond)) return e;

  if (t == e) return t;

  /* ITE(cond, TRUE, FALSE) -> cond */
  if (ExprMgr_is_true(self, t) && ExprMgr_is_false(self, e)) return cond;

  /* ITE(cond, FALSE, TRUE) -> NOT cond */
  if (ExprMgr_is_false(self, t) && ExprMgr_is_true(self, e)) return ExprMgr_not(self, cond);

  /* We can apply simplifications only if the return type is not a
     set, because only CASE expressions allow sets. */
  if (ExprMgr_is_false(self, t)) {
    if (FAILURE == node_get_type(e)) {
      ErrorMgr_warning_failure_node(self->errors, e);
      return ExprMgr_not(self, cond);
    }
    else if (SYMB_TABLE(NULL) != symb_table) {
      TypeChecker_ptr tc = SymbTable_get_type_checker(symb_table);
      SymbType_ptr et = TypeChecker_get_expression_type(tc, e, Nil);
      if (!SymbType_is_set(et)) { return ExprMgr_and(self, ExprMgr_not(self, cond), e); }
    }
  }
  if (ExprMgr_is_true(self, t)) {
    if (FAILURE == node_get_type(e)) {
      ErrorMgr_warning_failure_node(self->errors, e);
      return cond;
    }
    else if (SYMB_TABLE(NULL) != symb_table) {
      TypeChecker_ptr tc = SymbTable_get_type_checker(symb_table);
      SymbType_ptr et = TypeChecker_get_expression_type(tc, e, Nil);
      if (!SymbType_is_set(et)) { return ExprMgr_or(self, cond, e); }
    }
  }

  if (ExprMgr_is_false(self, e) && (SYMB_TABLE(NULL) != symb_table)) {
    TypeChecker_ptr tc = SymbTable_get_type_checker(symb_table);
    SymbType_ptr tt = TypeChecker_get_expression_type(tc, t, Nil);
    if (!SymbType_is_set(tt)) { return ExprMgr_and(self, cond, t); }
  }
  if (ExprMgr_is_true(self, e) && (SYMB_TABLE(NULL) != symb_table)) {
    TypeChecker_ptr tc = SymbTable_get_type_checker(symb_table);
    SymbType_ptr tt = TypeChecker_get_expression_type(tc, t, Nil);
    if (!SymbType_is_set(tt)) { return ExprMgr_or(self, ExprMgr_not(self, cond), t); }
  }

  /*
    case                case
    C1 : E1;            C1 | C2 : E1;
    C2 : E1;  --->      C3 : E2;
    C3 : E2;         esac
    esac
  */
  {
    if ((CASE == node_get_type(e)) ||
        (IFTHENELSE == node_get_type(e))) {
      node_ptr _c, _t, _e;

      nusmv_assert(COLON == node_get_type(car(e)));

      _c = car(car(e));
      _t = cdr(car(e));
      _e = cdr(e);

      if (_t == t) {
        return ExprMgr_ite(self, ExprMgr_or(self, cond, _c), t, _e, symb_table);
      }
    }
  }

  /*
    case
    cond1 : case
    cond1 : expr1;
    ...

    esac;
    ...
    esac;

    simplifies into

    case
    cond1 : expr1;
    ...
    esac
  */
  if (((CASE == node_get_type(t)) ||
       (IFTHENELSE == node_get_type(t))) &&
      (cond == car(car(t)))) {
    tmp = FN(self, COLON, NODE_PTR(cond), cdr(car(NODE_PTR(t))));
  }
  else {
    tmp = FN(self, COLON, NODE_PTR(cond), NODE_PTR(t));
  }
  return EXPR( FN(self, CASE, tmp, NODE_PTR(e)) );
}

Expr_ptr ExprMgr_next(const ExprMgr_ptr self, const Expr_ptr a,
                      const SymbTable_ptr symb_table)
{
  int ta;

  /* boolean constant */
  if (ExprMgr_is_true(self, a) || ExprMgr_is_false(self, a)) return a;

  /* scalar constants */
  ta = node_get_type(a);
  if (ta == NUMBER || ta == NUMBER_UNSIGNED_WORD || ta == NUMBER_SIGNED_WORD) {
    return a;
  }

  /* a range? */
  if (ta == TWODOTS &&
      NUMBER == node_get_type(car(a)) &&
      NUMBER == node_get_type(cdr(a))) {
    return a;
  }

  /* enumerative? */
  if (symb_table != SYMB_TABLE(NULL) &&
      SymbTable_is_symbol_constant(symb_table, a)) {
    return a;
  }

  /* set of constants ? */
  if (symb_table != SYMB_TABLE(NULL) && UNION == node_get_type(a)) {
    Set_t set = Set_MakeFromUnion(self->nodes, a);
    boolean is_const = true;
    Set_Iterator_t iter;
    SET_FOREACH(set, iter) {
      if (!SymbTable_is_symbol_constant(symb_table,
                                        (node_ptr) Set_GetMember(set, iter))) {
        is_const = false;
        break;
      }
    }

    Set_ReleaseSet(set);
    if (is_const) return a;
  }

  /* fall back */
  return EXPR( FN(self, NEXT, NODE_PTR(a), Nil)  );
}

Expr_ptr ExprMgr_equal(const ExprMgr_ptr self, const Expr_ptr a,
                       const Expr_ptr b,
                       const SymbTable_ptr st)
{

  if (a == b) return self->expr_true;
  if (ExprMgr_is_true(self, a) && ExprMgr_is_true(self, b)) return self->expr_true;
  if (ExprMgr_is_true(self, a) && ExprMgr_is_false(self, b)) return self->expr_false;
  if (ExprMgr_is_false(self, a) && ExprMgr_is_false(self, b)) return self->expr_true;
  if (ExprMgr_is_false(self, a) && ExprMgr_is_true(self, b)) return self->expr_false;

  {
    int ta, tb;
    ta = node_get_type(a); tb = node_get_type(b);

    if ((ta == NOT && car(a) == b) ||
        (tb == NOT && car(b) == a)) return self->expr_false;

    /* scalar constants */
    if (NUMBER == ta && NUMBER == tb) {
      int va = node_get_int(a);
      int vb = node_get_int(b);
      return (va == vb) ? self->expr_true : self->expr_false;
    }
    /* words */
    else if (NUMBER_UNSIGNED_WORD == ta || NUMBER_UNSIGNED_WORD == tb ||
             NUMBER_SIGNED_WORD == ta || NUMBER_SIGNED_WORD == tb) {
      WordNumber_ptr va =
        (NUMBER_UNSIGNED_WORD == ta || NUMBER_SIGNED_WORD == ta)
        ? WORD_NUMBER(car(a)) : WORD_NUMBER(NULL);
      WordNumber_ptr vb =
        (NUMBER_UNSIGNED_WORD == tb || NUMBER_SIGNED_WORD == tb)
        ? WORD_NUMBER(car(b)) : WORD_NUMBER(NULL);

      if (va != WORD_NUMBER(NULL) && vb != (WORD_NUMBER(NULL)))
        return WordNumber_equal(va, vb)
          ? self->expr_true : self->expr_false;
    }
  }

  /* additional simplifications */
  if (SYMB_TABLE(NULL) != st) {
    SymbType_ptr ta, tb;
    TypeChecker_ptr tc = SymbTable_get_type_checker(st);

    /* enumerative? */
    if (SymbTable_is_symbol_constant(st, a) &&
        SymbTable_is_symbol_constant(st, b)) {
      return (a == b) ? self->expr_true : self->expr_false;
    }

    /* TRUE = B --------> B */
    if (ExprMgr_is_true(self, a)) {
      tb = TypeChecker_get_expression_type(tc, b, Nil);
      if (SymbType_is_boolean(tb)) {
        return b;
      }
    }
    /* A = TRUE --------> A */
    else if (ExprMgr_is_true(self, b)) {
      ta = TypeChecker_get_expression_type(tc, a, Nil);
      if (SymbType_is_boolean(ta)) {
        return a;
      }
    }
    /* FALSE = B --------> !B */
    else if (ExprMgr_is_false(self, a)) {
      tb = TypeChecker_get_expression_type(tc, b, Nil);
      if (SymbType_is_boolean(tb)) {
        return ExprMgr_not(self, b);
      }
    }
    /* A = FALSE --------> !A */
    else if (ExprMgr_is_false(self, b)) {
      ta = TypeChecker_get_expression_type(tc, a, Nil);
      if (SymbType_is_boolean(ta)) {
        return ExprMgr_not(self, a);
      }
    }
  }

#if ! DISABLE_EXPR_POINTERS_ORDERING
  /* no simplification is possible, remember ordering */
  if (a > b) {
    return EXPR( FN(self, EQUAL, NODE_PTR(b), NODE_PTR(a)) );
  }
#endif

  return EXPR( FN(self, EQUAL, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_notequal(const ExprMgr_ptr self, const Expr_ptr a,
                          const Expr_ptr b,
                          const SymbTable_ptr st)
{

  if (a == b) return self->expr_false;
  if (ExprMgr_is_true(self, a) && ExprMgr_is_true(self, b)) return self->expr_false;
  if (ExprMgr_is_true(self, a) && ExprMgr_is_false(self, b)) return self->expr_true;
  if (ExprMgr_is_false(self, a) && ExprMgr_is_false(self, b)) return self->expr_false;
  if (ExprMgr_is_false(self, a) && ExprMgr_is_true(self, b)) return self->expr_true;

  {
    int ta, tb;
    ta = node_get_type(a); tb = node_get_type(b);

    if ((ta == NOT && car(a) == b) ||
        (tb == NOT && car(b) == a)) return self->expr_true;

    /* scalar constants */
    if (NUMBER == ta && NUMBER == tb) {
      int va = node_get_int(a);
      int vb = node_get_int(b);
      return (va != vb) ? self->expr_true : self->expr_false;
    }
    else if (NUMBER_UNSIGNED_WORD == ta || NUMBER_UNSIGNED_WORD == tb ||
             NUMBER_SIGNED_WORD == ta || NUMBER_SIGNED_WORD == tb) {
      WordNumber_ptr va =
        (NUMBER_UNSIGNED_WORD == ta || NUMBER_SIGNED_WORD == ta)
        ? WORD_NUMBER(car(a)) : WORD_NUMBER(NULL);
      WordNumber_ptr vb =
        (NUMBER_UNSIGNED_WORD == tb || NUMBER_SIGNED_WORD == tb)
        ? WORD_NUMBER(car(b)) : WORD_NUMBER(NULL);

      if (va != WORD_NUMBER(NULL) && vb != (WORD_NUMBER(NULL)))
        return WordNumber_not_equal(va, vb)
          ? self->expr_true : self->expr_false;
    }
  }

  if (SYMB_TABLE(NULL) != st) {
    SymbType_ptr ta, tb;
    TypeChecker_ptr tc = SymbTable_get_type_checker(st);

    /* enumerative? */
    if (SymbTable_is_symbol_constant(st, a) &&
        SymbTable_is_symbol_constant(st, b)) {
      return (a == b) ? self->expr_false : self->expr_true;
    }

    /* TRUE != B --------> !B */
    if (ExprMgr_is_true(self, a)) {
      tb = TypeChecker_get_expression_type(tc, b, Nil);
      if (SymbType_is_boolean(tb)) {
        return ExprMgr_not(self, b);
      }
    }
    /* A != TRUE --------> !A */
    else if (ExprMgr_is_true(self, b)) {
      ta = TypeChecker_get_expression_type(tc, a, Nil);
      if (SymbType_is_boolean(ta)) {
        return ExprMgr_not(self, a);
      }
    }
    /* FALSE != B --------> B */
    else if (ExprMgr_is_false(self, a)) {
      tb = TypeChecker_get_expression_type(tc, b, Nil);
      if (SymbType_is_boolean(tb)) {
        return b;
      }
    }
    /* A != FALSE --------> A */
    else if (ExprMgr_is_false(self, b)) {
      ta = TypeChecker_get_expression_type(tc, a, Nil);
      if (SymbType_is_boolean(ta)) {
        return a;
      }
    }
  }

#if ! DISABLE_EXPR_POINTERS_ORDERING
  /* no simplification is possible */
  if (a > b) {
    return EXPR( FN(self, NOTEQUAL, NODE_PTR(b), NODE_PTR(a)) );
  }
#endif

  return EXPR( FN(self, NOTEQUAL, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_lt(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  if (a == b) return self->expr_false;

  /* Booleans are not valid for this operator */
  nusmv_assert(! (expr_is_bool(self, a) || expr_is_bool(self, b)));

  {
    int ta, tb;
    ta = node_get_type(a); tb = node_get_type(b);

    /* scalar constants */
    if (NUMBER == ta && NUMBER == tb) {
      int va = node_get_int(a);
      int vb = node_get_int(b);
      return (va < vb) ? self->expr_true : self->expr_false;
    }
    else if (NUMBER_UNSIGNED_WORD == ta || NUMBER_UNSIGNED_WORD == tb ||
             NUMBER_SIGNED_WORD == ta || NUMBER_SIGNED_WORD == tb) {

      WordNumber_ptr va =
        (NUMBER_UNSIGNED_WORD == ta || NUMBER_SIGNED_WORD == ta)
        ? WORD_NUMBER(car(a)) : WORD_NUMBER(NULL);
      WordNumber_ptr vb =
        (NUMBER_UNSIGNED_WORD == tb || NUMBER_SIGNED_WORD == tb)
        ? WORD_NUMBER(car(b)) : WORD_NUMBER(NULL);

      /* if both are constants => evaluate */
      if (va != NULL && vb != NULL) {
        nusmv_assert(ta == tb); /* signess has to be the same by type rules */

        return (NUMBER_UNSIGNED_WORD == ta
                ?WordNumber_unsigned_less(va, vb)
                :WordNumber_signed_less(va, vb))
          ? self->expr_true : self->expr_false;
      }
      /* expr < uwconst(<size>,0)  =========> FALSE
         uwconst(<size>,max_value) < expr =========> FALSE
         swconst(<size>,max_value) < expr =========> FALSE */
      else if ((tb == NUMBER_UNSIGNED_WORD &&
                WordNumber_is_zero(vb))
               ||
               expr_is_wordnumber_max(self, va, ta))
               {
        return self->expr_false;
      }
      /* go to no-simplification return */
    }
  }

  /* no simplification is possible */
  return EXPR( FN(self, LT, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_simplify_lt(const ExprMgr_ptr self, const SymbTable_ptr st,
                             const Expr_ptr a, const Expr_ptr b)
{
  Expr_ptr res = ExprMgr_lt(self, a, b);
  if (ExprMgr_is_true(self, res) || ExprMgr_is_false(self, res)) return res;

  /* no simplification is possible */
  return res;
}

Expr_ptr ExprMgr_le(const ExprMgr_ptr self, const Expr_ptr a,
                    const Expr_ptr b,
                    const SymbTable_ptr symb_table)
{
  if (a == b) return self->expr_true;
  nusmv_assert(! (expr_is_bool(self, a) || expr_is_bool(self, b)));

  {
    int ta, tb;
    ta = node_get_type(a); tb = node_get_type(b);

    /* scalar constants */
    if (NUMBER == ta && NUMBER == tb) {
      int va = node_get_int(a);
      int vb = node_get_int(b);
      return (va <= vb) ? self->expr_true : self->expr_false;
    }
    /* words */
    else if (NUMBER_UNSIGNED_WORD == ta || NUMBER_UNSIGNED_WORD == tb ||
             NUMBER_SIGNED_WORD == ta || NUMBER_SIGNED_WORD == tb) {
      WordNumber_ptr va =
        (NUMBER_UNSIGNED_WORD == ta || NUMBER_SIGNED_WORD == ta)
        ? WORD_NUMBER(car(a)) : WORD_NUMBER(NULL);
      WordNumber_ptr vb =
        (NUMBER_UNSIGNED_WORD == tb || NUMBER_SIGNED_WORD == tb)
        ? WORD_NUMBER(car(b)) : WORD_NUMBER(NULL);

      /* if both are constants => evaluate */
      if (va != NULL && vb != NULL) {
        nusmv_assert(ta == tb); /* signess has to be the same by type rules */

        return (NUMBER_UNSIGNED_WORD == ta
                ?WordNumber_unsigned_less_or_equal(va, vb)
                :WordNumber_signed_less_or_equal(va, vb))
          ? self->expr_true : self->expr_false;
      }
      /* expr <= uwconst(<size>,0) =========> expr = uwconst(<size>,0) */
      else if (tb == NUMBER_UNSIGNED_WORD &&
               WordNumber_is_zero(vb)) {
        return ExprMgr_equal(self, a,b, symb_table);
      }
      /* uwconst(<size>,0) <= expr =========> TRUE
         expr <= uwconst(<size>,max_value) =========> TRUE
         expr <= swconst(<size>,max_value) =========> TRUE */
      else if ((ta == NUMBER_UNSIGNED_WORD &&
                WordNumber_is_zero(va))
               ||
               expr_is_wordnumber_max(self, vb, tb)) {
        return self->expr_true;
      }
      /* go to no-simplification return */
    }
  }

  /* no simplification is possible */
  return EXPR( FN(self, LE, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_gt(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  if (a == b) return self->expr_false;

  nusmv_assert(! (expr_is_bool(self, a) || expr_is_bool(self, b)));

  {
    int ta, tb;
    ta = node_get_type(a); tb = node_get_type(b);

    /* scalar constants */
    if (NUMBER == ta && NUMBER == tb) {
      int va = node_get_int(a);
      int vb = node_get_int(b);
      return (va > vb) ? self->expr_true : self->expr_false;
    }
    /* words */
    else if (NUMBER_UNSIGNED_WORD == ta || NUMBER_UNSIGNED_WORD == tb ||
             NUMBER_SIGNED_WORD == ta || NUMBER_SIGNED_WORD == tb) {
      WordNumber_ptr va =
        (NUMBER_UNSIGNED_WORD == ta || NUMBER_SIGNED_WORD == ta)
        ? WORD_NUMBER(car(a)) : WORD_NUMBER(NULL);
      WordNumber_ptr vb =
        (NUMBER_UNSIGNED_WORD == tb || NUMBER_SIGNED_WORD == tb)
        ? WORD_NUMBER(car(b)) : WORD_NUMBER(NULL);

      /* if both are constants => evaluate */
      if (va != NULL && vb != NULL) {
        nusmv_assert(ta == tb); /* signess has to be the same by type rules */

        return (NUMBER_UNSIGNED_WORD == ta
                ? WordNumber_unsigned_greater(va, vb)
                : WordNumber_signed_greater(va, vb))
          ? self->expr_true : self->expr_false;
      }
      /* uwconst(<size>,0) > expr =========> FALSE
         expr > uwconst(<size>,max_value) =========> FALSE
         expr > swconst(<size>,max_value) =========> FALSE */
      else if ((ta == NUMBER_UNSIGNED_WORD &&
                WordNumber_is_zero(va))
               ||
               expr_is_wordnumber_max(self, vb, tb)) {
        return self->expr_false;
      }
    }
  }

  /* no simplification is possible */
  return EXPR( FN(self, GT, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_simplify_gt(const ExprMgr_ptr self, const SymbTable_ptr st,
                             const Expr_ptr a, const Expr_ptr b)
{
  Expr_ptr res = ExprMgr_gt(self, a, b);

  return res;
}

Expr_ptr ExprMgr_ge(const ExprMgr_ptr self, const Expr_ptr a,
                    const Expr_ptr b,
                    const SymbTable_ptr symb_table)
{
  if (a == b) return self->expr_true;

  nusmv_assert(! (expr_is_bool(self, a) || expr_is_bool(self, b)));

  {
    int ta, tb;
    ta = node_get_type(a); tb = node_get_type(b);

    /* scalar constants */
    if (NUMBER == ta && NUMBER == tb) {
      int va = node_get_int(a);
      int vb = node_get_int(b);
      return (va >= vb) ? self->expr_true : self->expr_false;
    }
    /* words */
    else if (NUMBER_UNSIGNED_WORD == ta || NUMBER_UNSIGNED_WORD == tb ||
             NUMBER_SIGNED_WORD == ta || NUMBER_SIGNED_WORD == tb) {
      WordNumber_ptr va =
        (NUMBER_UNSIGNED_WORD == ta || NUMBER_SIGNED_WORD == ta)
        ? WORD_NUMBER(car(a)) : WORD_NUMBER(NULL);
      WordNumber_ptr vb =
        (NUMBER_UNSIGNED_WORD == tb || NUMBER_SIGNED_WORD == tb)
        ? WORD_NUMBER(car(b)) : WORD_NUMBER(NULL);

      /* if both are constants => evaluate */
      if (va != NULL && vb != NULL) {
        nusmv_assert(ta == tb); /* signess has to be the same by type rules */

        return (NUMBER_UNSIGNED_WORD == ta
                ?WordNumber_unsigned_greater_or_equal(va, vb)
                :WordNumber_signed_greater_or_equal(va, vb))
          ? self->expr_true : self->expr_false;
      }
      /*  uwconst(<size>,0) >= expr =========> uwconst(<size>,0) = expr*/
      else if (ta == NUMBER_UNSIGNED_WORD &&
               WordNumber_is_zero(va)) {
        return ExprMgr_equal(self, a, b, symb_table);
      }
      /* expr >= uwconst(<size>,0) =========> TRUE
         uwconst(<size>,max_value) >= expr =========> TRUE
         swconst(<size>,max_value) >= expr=========> TRUE */
      else if ((tb == NUMBER_UNSIGNED_WORD &&
                WordNumber_is_zero(vb))
               ||
               expr_is_wordnumber_max(self, va, ta)) {
        return self->expr_true;
      }
      /* go to no-simplification return */
    }
  }

  /* no simplification is possible */
  return EXPR( FN(self, GE, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_plus(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  int ta = node_get_type(a);
  int tb = node_get_type(b);

  nusmv_assert(! (expr_is_bool(self, a) || expr_is_bool(self, b)));

  /* Simplify constants *******************************************************/
  if (ta == NUMBER && tb == NUMBER) {
    return FN(self, NUMBER,
              NODE_FROM_INT((node_get_int(a) +
                             node_get_int(b))),
              Nil);
  }

  if ((ta == NUMBER_UNSIGNED_WORD && tb == NUMBER_UNSIGNED_WORD) ||
      (ta == NUMBER_SIGNED_WORD && tb == NUMBER_SIGNED_WORD)) {

#if ! DISABLE_EXPR_POINTERS_ORDERING
    if (car(a) > car(b)) {
      return FN(self, ta,
                (node_ptr)WordNumberMgr_plus(self->words, WORD_NUMBER(car(b)),
                                             WORD_NUMBER(car(a))),
                Nil);
    }
#endif

    return FN(self, ta,
              (node_ptr)WordNumberMgr_plus(self->words, WORD_NUMBER(car(a)),
                                           WORD_NUMBER(car(b))),
              Nil);
  }

  /* Simplify sum with 0 ******************************************************/
  /* 0 + A = A */
  if (((ta == NUMBER) && (0 == node_get_int(a))) ||
      (((ta == NUMBER_SIGNED_WORD) || (ta == NUMBER_UNSIGNED_WORD)) &&
       WordNumber_is_zero(WORD_NUMBER(car(a))))) {
    return b;
  }
  /* A + 0 = A */
  if (((tb == NUMBER) && (0 == node_get_int(b))) ||
      (((tb == NUMBER_SIGNED_WORD) || (tb == NUMBER_UNSIGNED_WORD)) &&
       WordNumber_is_zero(WORD_NUMBER(car(b))))) {
    return a;
  }

#if ! DISABLE_EXPR_POINTERS_ORDERING
  /* no simplification is possible, remember pointer ordering */
  if (a > b) {
    return EXPR( FN(self, PLUS, NODE_PTR(b), NODE_PTR(a)) );
  }
#endif

  return EXPR( FN(self, PLUS, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_minus(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  int ta = node_get_type(a);
  int tb = node_get_type(b);

  nusmv_assert(! (expr_is_bool(self, a) || expr_is_bool(self, b)));

  if (ta == NUMBER && tb == NUMBER) {
    return FN(self, NUMBER,
              NODE_FROM_INT((node_get_int(a) -
                             node_get_int(b))),
              Nil);
  }

  if ((ta == NUMBER_UNSIGNED_WORD && tb == NUMBER_UNSIGNED_WORD) ||
      (ta == NUMBER_SIGNED_WORD && tb == NUMBER_SIGNED_WORD)) {
    return FN(self, ta,
              (node_ptr)WordNumberMgr_minus(self->words, WORD_NUMBER(car(a)),
                                            WORD_NUMBER(car(b))),
              Nil);
  }

  /* 0 - A = -A */
  if (((ta == NUMBER) && (0 == node_get_int(a))) ||
      (((ta == NUMBER_SIGNED_WORD) || (ta == NUMBER_UNSIGNED_WORD)) &&
       WordNumber_is_zero(WORD_NUMBER(car(a))))) {
    return ExprMgr_unary_minus(self, b);
  }
  /* A - 0 = A */
  if (((tb == NUMBER) && (0 == node_get_int(b))) ||
      (((tb == NUMBER_SIGNED_WORD) || (tb == NUMBER_UNSIGNED_WORD))  &&
       WordNumber_is_zero(WORD_NUMBER(car(b))))) {
    return a;
  }

  /* no simplification is possible */
  return EXPR( FN(self, MINUS, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_times(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  int ta = node_get_type(a);
  int tb = node_get_type(b);

  nusmv_assert(! (expr_is_bool(self, a) || expr_is_bool(self, b)));

  if (ta == NUMBER && tb == NUMBER) {
    return FN(self, NUMBER,
              NODE_FROM_INT((node_get_int(a) *
                             node_get_int(b))),
              Nil);
  }
  if ((ta == NUMBER_UNSIGNED_WORD && tb == NUMBER_UNSIGNED_WORD) ||
      (ta == NUMBER_SIGNED_WORD && tb == NUMBER_SIGNED_WORD)) {

#if ! DISABLE_EXPR_POINTERS_ORDERING
    if (car(a) > car(b)) {
      return FN(self, ta,
                (node_ptr)WordNumberMgr_times(self->words, WORD_NUMBER(car(b)),
                                              WORD_NUMBER(car(a))),
                Nil);
    }
#endif

    return FN(self, ta,
              (node_ptr)WordNumberMgr_times(self->words, WORD_NUMBER(car(a)),
                                            WORD_NUMBER(car(b))),
              Nil);
  }

  /* 0 * A = 0 */
  if (((ta == NUMBER) && (0 == node_get_int(a))) ||
      ((tb == NUMBER) && (0 == node_get_int(b)))) {
    return FN(self, NUMBER, NODE_FROM_INT(0), Nil);
  }
  /* A * 0 = 0 */
  if ((((ta == NUMBER_SIGNED_WORD) || (ta == NUMBER_UNSIGNED_WORD)) &&
       WordNumber_is_zero(WORD_NUMBER(car(a)))) ||
      (((tb == NUMBER_SIGNED_WORD) || (tb == NUMBER_UNSIGNED_WORD)) &&
       WordNumber_is_zero(WORD_NUMBER(car(b))))) {
    return ((ta == NUMBER_SIGNED_WORD) ||
            (ta == NUMBER_UNSIGNED_WORD)) ? a : b;
  }

  /* A * 1 = A */
  if ((tb == NUMBER) && (1 == node_get_int(b)))
    return a;
  /* 1 * B = B */
  if ((ta == NUMBER) && (1 == node_get_int(a)))
    return b;


#if ! DISABLE_EXPR_POINTERS_ORDERING
  /* no simplification is possible, remember pointer ordering */
  if (a > b) {
    return EXPR( FN(self, TIMES, NODE_PTR(b), NODE_PTR(a)) );
  }
#endif

  return EXPR( FN(self, TIMES, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_divide(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  int ta = node_get_type(a);
  int tb = node_get_type(b);

  nusmv_assert(! (expr_is_bool(self, a) || expr_is_bool(self, b)));

  if (ta == NUMBER && tb == NUMBER) {
    int vb = node_get_int(b);
    if (vb == 0) ErrorMgr_error_div_by_zero(self->errors, b);
    return FN(self, NUMBER,
              NODE_FROM_INT((node_get_int(a) / vb)),
              Nil);
  }
  if (ta == NUMBER_UNSIGNED_WORD && tb == NUMBER_UNSIGNED_WORD) {
    if (WordNumber_is_zero(WORD_NUMBER(car(b)))) ErrorMgr_error_div_by_zero(self->errors, b);
    return FN(self, ta,
              (node_ptr)WordNumberMgr_unsigned_divide(self->words, WORD_NUMBER(car(a)),
                                                      WORD_NUMBER(car(b))),
              Nil);
  }
  if (ta == NUMBER_SIGNED_WORD && tb == NUMBER_SIGNED_WORD) {
    if (WordNumber_is_zero(WORD_NUMBER(car(b)))) ErrorMgr_error_div_by_zero(self->errors, b);
    return FN(self, ta,
              (node_ptr)WordNumberMgr_signed_divide(self->words, WORD_NUMBER(car(a)),
                                                    WORD_NUMBER(car(b))),
              Nil);
  }

  /* A / 1 = A */
  if ((tb == NUMBER) && (1 == node_get_int(b)))
    return a;
  /* 0 / B = 0 */
  if ((ta == NUMBER) && (0 == node_get_int(a)))
    return FN(self, NUMBER, NODE_FROM_INT(0), Nil);

  /* no simplification is possible */
  return EXPR( FN(self, DIVIDE, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_mod(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  int ta = node_get_type(a);
  int tb = node_get_type(b);

  nusmv_assert(! (expr_is_bool(self, a) || expr_is_bool(self, b)));

  if (ta == NUMBER && tb == NUMBER) {
    int vb = node_get_int(b);
    if (vb == 0) ErrorMgr_error_div_by_zero(self->errors, b);

    return FN(self, NUMBER,
              NODE_FROM_INT((node_get_int(a) % vb)),
              Nil);
  }
  if (ta == NUMBER_UNSIGNED_WORD && tb == NUMBER_UNSIGNED_WORD) {
    if (WordNumber_is_zero(WORD_NUMBER(car(b)))) ErrorMgr_error_div_by_zero(self->errors, b);
    return FN(self, ta,
              (node_ptr)WordNumberMgr_unsigned_mod(self->words, WORD_NUMBER(car(a)),
                                                   WORD_NUMBER(car(b))),
              Nil);
  }
  if (ta == NUMBER_SIGNED_WORD && tb == NUMBER_SIGNED_WORD) {
    if (WordNumber_is_zero(WORD_NUMBER(car(b)))) ErrorMgr_error_div_by_zero(self->errors, b);
    return FN(self, ta,
              (node_ptr)WordNumberMgr_signed_mod(self->words, WORD_NUMBER(car(a)),
                                                 WORD_NUMBER(car(b))),
              Nil);
  }

  /* no simplification is possible */
  return EXPR( FN(self, MOD, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_unary_minus(const ExprMgr_ptr self, const Expr_ptr a)
{
  nusmv_assert(! expr_is_bool(self, a));
  switch (node_get_type(a)) {
  case NUMBER: return FN(self, NUMBER,
                         NODE_FROM_INT(-node_get_int(a)), Nil);
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
    return FN(self, node_get_type(a),
              (node_ptr)WordNumberMgr_unary_minus(self->words, WORD_NUMBER(car(a))),
              Nil);
  }

  /* no simplification is possible */
  return EXPR( FN(self, UMINUS, NODE_PTR(a), Nil) );
}

Expr_ptr ExprMgr_array_read(const ExprMgr_ptr self,
                         const Expr_ptr a, const Expr_ptr i)
{
  return EXPR( FN(self, WAREAD, NODE_PTR(a), NODE_PTR(i)) );
}

Expr_ptr ExprMgr_array_write(const ExprMgr_ptr self,
                          const Expr_ptr a, const Expr_ptr i, const Expr_ptr v)
{
  return EXPR( FN(self, WAWRITE, NODE_PTR(a),
                  FN(self, WAWRITE, NODE_PTR(i), NODE_PTR(v))) );
}

Expr_ptr ExprMgr_array_const(const ExprMgr_ptr self,
                          const Expr_ptr a, const Expr_ptr v)
{
  return EXPR( FN(self, CONST_ARRAY,
                  FN(self, TYPEOF, NODE_PTR(a), Nil),
                  NODE_PTR(v)) );
}

Expr_ptr ExprMgr_word_left_shift(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  int ta = node_get_type(a);

  if (ta == NUMBER_UNSIGNED_WORD || ta == NUMBER_SIGNED_WORD) {
    int bits;
    nusmv_assert(!expr_is_bool(self, b));

    switch (node_get_type(b)) {
    case NUMBER: bits = node_get_int(b); break;
    case NUMBER_UNSIGNED_WORD:
      bits =WordNumber_get_unsigned_value(WORD_NUMBER(car(b))); break;
    case NUMBER_SIGNED_WORD:
      bits =WordNumber_get_signed_value(WORD_NUMBER(car(b))); break;
    default: bits = -1;
    }

    if (bits == 0) return a;
    if (bits > 0) {
      if (bits >WordNumber_get_width(WORD_NUMBER(car(a)))) {
        ErrorMgr_error_wrong_word_operand(self->errors,
                                          "Right operand of shift is out of range", b);
      }
      return FN(self, ta,
                NODE_PTR(WordNumberMgr_left_shift(self->words, WORD_NUMBER(car(a)),
                                               bits)),
                Nil);
    }
    /* b here is not a constant */
  }

  /* no simplification is possible */
  return EXPR( FN(self, LSHIFT, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_word_right_shift(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  int ta = node_get_type(a);

  if (ta == NUMBER_UNSIGNED_WORD || ta == NUMBER_SIGNED_WORD) {
    int bits;
    nusmv_assert(!expr_is_bool(self, b));

    switch (node_get_type(b)) {
    case NUMBER: bits = node_get_int(b); break;
    case NUMBER_UNSIGNED_WORD:
      bits =WordNumber_get_unsigned_value(WORD_NUMBER(car(b))); break;
    case NUMBER_SIGNED_WORD:
      bits =WordNumber_get_signed_value(WORD_NUMBER(car(b))); break;
    default: bits = -1;
    }

    if (bits == 0) return a;
    if (bits > 0) {
      WordNumber_ptr rs;

      if (bits >WordNumber_get_width(WORD_NUMBER(car(a)))) {
        ErrorMgr_error_wrong_word_operand(self->errors,
                                          "Right operand of shift is out of range", b);
      }
      if (ta == NUMBER_UNSIGNED_WORD) {
        rs = WordNumberMgr_unsigned_right_shift(self->words, WORD_NUMBER(car(a)), bits);
      }
      else {
        rs = WordNumberMgr_signed_right_shift(self->words, WORD_NUMBER(car(a)), bits);
      }

      return FN(self, ta, (node_ptr) rs, Nil);
    }
    /* b here is not a constant */
  }

  /* no simplification is possible */
  return EXPR( FN(self, RSHIFT, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_word_left_rotate(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  int ta = node_get_type(a);

  if (ta == NUMBER_UNSIGNED_WORD || ta == NUMBER_SIGNED_WORD) {
    int bits;

    nusmv_assert(!expr_is_bool(self, b));

    switch (node_get_type(b)) {
    case NUMBER: bits = node_get_int(b); break;
    case NUMBER_UNSIGNED_WORD:
      bits = WordNumber_get_unsigned_value(WORD_NUMBER(car(b))); break;
    case NUMBER_SIGNED_WORD:
      bits = WordNumber_get_signed_value(WORD_NUMBER(car(b))); break;
    default: bits = -1;
    }

    if (bits == 0) return a;
    if (bits > 0) {
      if (bits >WordNumber_get_width(WORD_NUMBER(car(a)))) {
        ErrorMgr_error_wrong_word_operand(self->errors,
                                          "Right operand of rotate is out of range", b);
      }
      return FN(self, ta,
                NODE_PTR(WordNumberMgr_left_rotate(self->words, WORD_NUMBER(car(a)),
                                                bits)),
                Nil);
    }
    /* b here is not a constant */
  }

  /* no simplification is possible */
  return EXPR( FN(self, LROTATE, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_word_right_rotate(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  int ta = node_get_type(a);

  if (ta == NUMBER_UNSIGNED_WORD || ta == NUMBER_SIGNED_WORD) {
    int bits;
    nusmv_assert(!expr_is_bool(self, b));
    switch (node_get_type(b)) {
    case NUMBER: bits = node_get_int(b); break;
    case NUMBER_UNSIGNED_WORD:
      bits = WordNumber_get_unsigned_value(WORD_NUMBER(car(b))); break;
    case NUMBER_SIGNED_WORD:
      bits = WordNumber_get_signed_value(WORD_NUMBER(car(b))); break;
    default: bits = -1;
    }

    if (bits == 0) return a;
    if (bits > 0) {
      if (bits >WordNumber_get_width(WORD_NUMBER(car(a)))) {
        ErrorMgr_error_wrong_word_operand(self->errors,
                                          "Right operand of rotate is out of range", b);
      }
      return FN(self, ta,
                NODE_PTR(WordNumberMgr_right_rotate(self->words, WORD_NUMBER(car(a)),
                                                 bits)),
                Nil);
    }
    /* b here is not a constant */
  }

  /* no simplification is possible */
  return EXPR( FN(self, RROTATE, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_word_bit_select(const ExprMgr_ptr self, const Expr_ptr w, const Expr_ptr r)
{
  /* Expr_ptr _r = expr_bool_const_to_number(r); */
  if (/* Simplification can be done iff the range is constant. If the
         range is a constant expression, we simply return the
         BIT_SELECTION node */
      (NUMBER == node_get_type(car(r)) &&
       NUMBER == node_get_type(cdr(r))) &&

      (((node_get_type(w) == UNSIGNED_WORD ||
         node_get_type(w) == SIGNED_WORD) &&
        (node_word_get_width(w) > 0))
       || (node_get_type(w) == NUMBER_UNSIGNED_WORD)
       || (node_get_type(w) == NUMBER_SIGNED_WORD))) {
    return EXPR(node_word_selection(w, r, EnvObject_get_environment(ENV_OBJECT(self))));
  }

  return EXPR(FN(self, BIT_SELECTION, w, r));
}

Expr_ptr ExprMgr_simplify_word_bit_select(const ExprMgr_ptr self, const SymbTable_ptr st,
                                          const Expr_ptr w, const Expr_ptr r)
{
  if (SYMB_TABLE(NULL) != st) {
    TypeChecker_ptr tc = SymbTable_get_type_checker(st);
    SymbType_ptr wt = TypeChecker_get_expression_type(tc, w, Nil);
    int argt_width = SymbType_get_word_width(wt);
    node_ptr msb, lsb;
    int sel_msb, sel_lsb;

    /* Simplify constant expressions */
    msb = CompileFlatten_resolve_number(st, car(r), Nil);
    lsb = CompileFlatten_resolve_number(st, cdr(r), Nil);

    nusmv_assert(COLON == node_get_type(r));
    nusmv_assert(Nil != msb && Nil != lsb &&
                 NUMBER == node_get_type(msb) &&
                 NUMBER == node_get_type(lsb));

    sel_msb = node_get_int(msb);
    sel_lsb = node_get_int(lsb);

    /* these simplification apply to unsigned words only */
    if (SymbType_is_unsigned_word(wt)) {

      /* Discard useless bit selection operations */
      if (0 == sel_lsb && (argt_width -1) == sel_msb)
        return w;

      if (EXTEND == node_get_type(w)) {
        Expr_ptr _w = car(w);
        SymbType_ptr _wt = TypeChecker_get_expression_type(tc, _w, Nil);

        int orig_width = SymbType_get_word_width(_wt);
        nusmv_assert(0 < orig_width && argt_width >= orig_width);

        {
          Expr_ptr res = Nil;
          int pivot = orig_width; /* starting bit position for '0' padding */


          /* if the selection is from the extension only rewrite as as
             0 word constant of appropriate width */
          if (sel_lsb >= pivot) {
            res = \
              FN(self, NUMBER_UNSIGNED_WORD,
                 NODE_PTR(WordNumberMgr_integer_to_word_number(self->words,
                                                  0LL,
                                                  sel_msb - sel_lsb +1)),
                 Nil);
          }
          /* if the selection is from the original word only, discard the
             EXTEND operation */
          else if (sel_msb < pivot) {
            res = ExprMgr_simplify_word_bit_select(self, st, _w, r);
          }
          /* if the selection is from both the extension and the original
             word, rewrite it as the extension to appropriate size of the
             selection of the relevant part of the word. */
          else {
            nusmv_assert(sel_msb >= pivot && pivot > sel_lsb);
            res = ExprMgr_simplify_word_extend(
                self, st,
                ExprMgr_simplify_word_bit_select(
                    self, st, _w,
                    FN(self, COLON,
                       FN(self, NUMBER,
                          NODE_FROM_INT(pivot-1), Nil),
                       FN(self, NUMBER,
                          NODE_FROM_INT(sel_lsb), Nil))),
                FN(self, NUMBER,
                   NODE_FROM_INT(sel_msb - pivot +1), Nil));
          }

          return res;
        }
      }
    }
  }
  /* fallback */
  return ExprMgr_word_bit_select(self, w, r);
}

Expr_ptr ExprMgr_word_concatenate(const ExprMgr_ptr self,
                                  const Expr_ptr a,
                                  const Expr_ptr b)
{
  int ta = node_get_type(a);
  int tb = node_get_type(b);

  nusmv_assert(! (expr_is_bool(self, a) || expr_is_bool(self, b)));

  if ((ta == NUMBER_UNSIGNED_WORD || ta == NUMBER_SIGNED_WORD) &&
      (tb == NUMBER_UNSIGNED_WORD || tb == NUMBER_SIGNED_WORD)) {
    return FN(self, NUMBER_UNSIGNED_WORD,
              (node_ptr)WordNumberMgr_concatenate(self->words, WORD_NUMBER(car(a)),
                                                  WORD_NUMBER(car(b))),
              Nil);
  }

  /* no simplification is possible */
  return EXPR( FN(self, CONCATENATION, NODE_PTR(a), NODE_PTR(b)) );
}

Expr_ptr ExprMgr_word1_to_bool(const ExprMgr_ptr self, Expr_ptr w)
{
  int tw = node_get_type(w);
  if (tw == NUMBER_UNSIGNED_WORD || tw == NUMBER_SIGNED_WORD) {
    WordNumber_ptr wn = WORD_NUMBER(car(w));
    return (WordNumber_get_unsigned_value(wn) != 0)
      ? self->expr_true
      : self->expr_false;
  }

  /* no simplification is possible */
  return EXPR( FN(self, CAST_BOOL, NODE_PTR(w), Nil) );
}

Expr_ptr ExprMgr_bool_to_word1(const ExprMgr_ptr self, Expr_ptr a)
{
  Expr_ptr _a = expr_bool_to_word1(self, a);
  if (_a != a) return _a;

  /* no simplification is possible */
  return EXPR( FN(self, CAST_WORD1, NODE_PTR(a), Nil) );
}

Expr_ptr ExprMgr_signed_word_to_unsigned(const ExprMgr_ptr self, Expr_ptr w)
{
  if (node_get_type(w) == NUMBER_SIGNED_WORD) {
    return FN(self, NUMBER_UNSIGNED_WORD, car(w), cdr(w));
  }

  /* no simplification is possible */
  return EXPR( FN(self, CAST_UNSIGNED, NODE_PTR(w), Nil) );
}

Expr_ptr ExprMgr_unsigned_word_to_signed(const ExprMgr_ptr self, Expr_ptr w)
{
  if (node_get_type(w) == NUMBER_UNSIGNED_WORD) {
    return FN(self, NUMBER_SIGNED_WORD, car(w), cdr(w));
  }

  /* no simplification is possible */
  return EXPR( FN(self, CAST_SIGNED, NODE_PTR(w), Nil) );
}

Expr_ptr ExprMgr_simplify_word_resize(const ExprMgr_ptr self, const SymbTable_ptr st,
                                      Expr_ptr w,
                                      Expr_ptr i)
{
  Expr_ptr _i;
  int w_type = node_get_type(w);

  /* if (SYMB_TABLE(NULL) != st) { */
  _i = CompileFlatten_resolve_number(st, i, Nil);
  /* } */
  /* else _i = i; */

  if (Nil != _i &&
      NUMBER == node_get_type(_i) &&
      (NUMBER_UNSIGNED_WORD == w_type || NUMBER_SIGNED_WORD == w_type)) {

    int m =WordNumber_get_width(WORD_NUMBER(car(w)));

    int n = node_get_int(i); /* shouldn't be (_i)? */

    nusmv_assert(0 < n);

    if (m == n) { return w; }
    else if (m < n) {
      return ExprMgr_simplify_word_extend(self, st, w,
                                       FN(self, NUMBER, NODE_FROM_INT(n - m), Nil));
    }
    else { /* n < m */
      if (NUMBER_UNSIGNED_WORD == node_get_type(w)) { /* unsigned */

        return ExprMgr_word_bit_select(self, w,
                                    FN(self, COLON,
                                       FN(self, NUMBER,
                                          NODE_FROM_INT(n - 1),
                                          Nil),
                                       FN(self, NUMBER,
                                          NODE_FROM_INT(0),
                                          Nil)));
      }
      else {  /* signed */
        node_ptr msb_sel, rightmost_sel, nexpr;
        nusmv_assert(NUMBER_SIGNED_WORD ==  node_get_type(w));

        msb_sel =
            FN(self, COLON,
               FN(self, NUMBER, NODE_FROM_INT(m-1), Nil),
               FN(self, NUMBER, NODE_FROM_INT(m-1), Nil));

        rightmost_sel =
            FN(self, COLON,
               FN(self, NUMBER, NODE_FROM_INT(n-2), Nil),
               FN(self, NUMBER, NODE_FROM_INT(0), Nil));

        nexpr =
            ExprMgr_word_concatenate(
                self, ExprMgr_word_bit_select(self, w, msb_sel),
                ExprMgr_word_bit_select(self, w, rightmost_sel));

        return ExprMgr_unsigned_word_to_signed(self, nexpr);
      }
    }
  }

  /* no simplification possible */
  return FN(self, WRESIZE, w, _i);
}

Expr_ptr ExprMgr_word_extend(const ExprMgr_ptr self, Expr_ptr w, Expr_ptr i, const SymbTable_ptr symb_table)
{
  int tw = node_get_type(w);
  node_ptr _i;

  nusmv_assert(! expr_is_bool(self, i));

  _i = CompileFlatten_resolve_number(symb_table, i, Nil);
  nusmv_assert(Nil != _i && node_get_type(_i) == NUMBER);

  if (tw == NUMBER_UNSIGNED_WORD) {
    return FN(self, NUMBER_UNSIGNED_WORD,
              (node_ptr)WordNumberMgr_unsigned_extend(self->words, WORD_NUMBER(car(w)),
                                                      node_get_int(_i)),
              Nil);
  }
  if (tw == NUMBER_SIGNED_WORD) {
    return FN(self, NUMBER_SIGNED_WORD,
              (node_ptr)WordNumberMgr_signed_extend(self->words, WORD_NUMBER(car(w)),
                                                    node_get_int(_i)),
              Nil);
  }

  /* no simplification is possible */
  return EXPR( FN(self, EXTEND, NODE_PTR(w), NODE_PTR(_i)) );
}

Expr_ptr ExprMgr_simplify_word_extend(const ExprMgr_ptr self, const SymbTable_ptr st,
                                      Expr_ptr w, Expr_ptr i)
{
  Expr_ptr _i;
  int tw = node_get_type(w);

  _i = CompileFlatten_resolve_number(st, i, Nil);
  nusmv_assert(Nil != _i && node_get_type(_i) == NUMBER);

  if (tw == NUMBER_UNSIGNED_WORD) {
    return FN(self, NUMBER_UNSIGNED_WORD,
              (node_ptr)WordNumberMgr_unsigned_extend(self->words, WORD_NUMBER(car(w)),
                                                      node_get_int(_i)),
              Nil);
  }
  if (tw == NUMBER_SIGNED_WORD) {
    return FN(self, NUMBER_SIGNED_WORD,
              (node_ptr)WordNumberMgr_signed_extend(self->words, WORD_NUMBER(car(w)),
                                                    node_get_int(_i)),
              Nil);
  }

  /* no simplification is possible */
  return EXPR( FN(self, EXTEND, NODE_PTR(w), NODE_PTR(_i)) );
}

Expr_ptr ExprMgr_attime(const ExprMgr_ptr self, Expr_ptr e, int time, const SymbTable_ptr symb_table)
{
  int te;

  /* boolean constant */
  if (ExprMgr_is_true(self, e) || ExprMgr_is_false(self, e)) return e;

  /* scalar constants */
  te = node_get_type(e);
  if (te == NUMBER || te == NUMBER_UNSIGNED_WORD || te == NUMBER_SIGNED_WORD) {
    return e;
  }

  /* a range? */
  if (te == TWODOTS &&
      NUMBER == node_get_type(car(e)) && NUMBER == node_get_type(cdr(e))) {
    return e;
  }

  /* enumerative? */
  if (symb_table != SYMB_TABLE(NULL) &&
      SymbTable_is_symbol_constant(symb_table, e)) {
    return e;
  }

  /* set of constants ? */
  if (symb_table != SYMB_TABLE(NULL) && UNION == node_get_type(e)) {
    Set_t set = Set_MakeFromUnion(self->nodes, e);
    boolean is_const = true;
    Set_Iterator_t iter;
    SET_FOREACH(set, iter) {
      if (!SymbTable_is_symbol_constant(symb_table,
                                        (node_ptr) Set_GetMember(set, iter))) {
        is_const = false;
        break;
      }
    }

    Set_ReleaseSet(set);
    if (is_const) return e;
  }

  /* fallback */
  return FN(self, ATTIME, e,
            FN(self, NUMBER, NODE_FROM_INT(time), Nil));
}

int ExprMgr_attime_get_time(const ExprMgr_ptr self, Expr_ptr e)
{
  nusmv_assert(ATTIME == node_get_type(e));
  return node_get_int(cdr(e));
}

Expr_ptr ExprMgr_attime_get_untimed(const ExprMgr_ptr self, Expr_ptr e)
{
  nusmv_assert(ATTIME == node_get_type(e));
  return car(e);
}

Expr_ptr ExprMgr_union(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  Expr_ptr res;

  if (Nil == a) return b;
  if (Nil == b) return a;
  if (a == b) return a;

  res = FN(self, UNION, a, b);

  { /* checks if cardinality is 1 */
    Set_t set = Set_MakeFromUnion(self->nodes, res);
    if (Set_GiveCardinality(set) == 1) {
      res = (Expr_ptr) Set_GetMember(set, Set_GetFirstIter(set));
    }
    Set_ReleaseSet(set);
  }

  return res;
}

Expr_ptr ExprMgr_range(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b)
{
  if (Nil == a) return b;
  if (Nil == b) return a;
  if (a == b) return a;

  if (NUMBER == node_get_type(a) && NUMBER == node_get_type(b) &&
      node_get_int(a) == node_get_int(b)) {
    return a;
  }

  return FN(self, TWODOTS, a, b);
}

Expr_ptr ExprMgr_setin(const ExprMgr_ptr self, const Expr_ptr a, const Expr_ptr b, const SymbTable_ptr symb_table)
{
  Expr_ptr res;
  Set_t seta = Set_MakeFromUnion(self->nodes, a);
  Set_t setb = Set_MakeFromUnion(self->nodes, b);

  /* checks if it can syntactically resolve it */
  if (Set_Contains(setb, seta)) res = self->expr_true;
  else {
    if (symb_table != SYMB_TABLE(NULL)) {
      /* see if the sets are made of only constants */
      boolean a_b_const = true;
      Set_Iterator_t iter;
      SET_FOREACH(seta, iter) {
        a_b_const = SymbTable_is_symbol_constant(symb_table,
                                                 (node_ptr) Set_GetMember(seta, iter));
        if (!a_b_const) break;
      }
      if (a_b_const) {
        SET_FOREACH(setb, iter) {
          a_b_const = SymbTable_is_symbol_constant(symb_table,
                                                   (node_ptr) Set_GetMember(setb, iter));
          if (!a_b_const) break;
        }
      }

      if (a_b_const) {
        /* both sets contain only constants, so since seta is not
           contained into setb, seta is not containted in setb */
        res = self->expr_false;
      }
      else res = FN(self, SETIN, a, b); /* fallback */
    }
    else {
      /* symbol table is not available, so nothing can be said */
      res = FN(self, SETIN, a, b);
    }
  }

  Set_ReleaseSet(setb);
  Set_ReleaseSet(seta);

  return res;
}

Expr_ptr ExprMgr_function(const ExprMgr_ptr self, const Expr_ptr name,
                          const Expr_ptr params)
{
  return EXPR(FN(self, NFUNCTION, name, params));
}


Expr_ptr ExprMgr_cast_to_unsigned_word(const ExprMgr_ptr self,
                                       Expr_ptr width, Expr_ptr arg)
{
  /* no simplification at the moment */
  return EXPR(FN(self, CAST_TO_UNSIGNED_WORD, NODE_PTR(arg), NODE_PTR(width)));
}


Expr_ptr ExprMgr_resolve(const ExprMgr_ptr self, SymbTable_ptr st,
                         int type,
                         Expr_ptr left,
                         Expr_ptr right)
{
  switch (type) {
    /* boolean leaves */
  case TRUEEXP: return self->expr_true;
  case FALSEEXP: return self->expr_false;

    /* other leaves */
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case BIT:
  case DOT:
  case ATOM:
  case ARRAY:
  case FAILURE:
    return FN(self, type, left, right);

  case UWCONST:
  case SWCONST:
    return ExprMgr_word_constant(self, st, type, left, right);

  case WSIZEOF:
    return ExprMgr_wsizeof(self, left, right);

  case CAST_TOINT:
    return ExprMgr_cast_toint(self, left, right);

  case WRESIZE: return ExprMgr_simplify_word_resize(self, st, left, right);

  case FLOOR: return ExprMgr_simplify_floor(self, st, left);

    /* boolean */
  case AND: return ExprMgr_and(self, left, right);
  case OR: return ExprMgr_or(self, left, right);
  case NOT: return ExprMgr_not(self, left);
  case IMPLIES: return ExprMgr_implies(self, left, right);
  case IFF: return ExprMgr_simplify_iff(self, st, left, right);
  case XOR: return ExprMgr_xor(self, left, right);
  case XNOR: return ExprMgr_xnor(self, left, right);

    /* predicates */
  case EQUAL: return ExprMgr_equal(self, left, right, st);
  case NOTEQUAL: return ExprMgr_notequal(self, left, right, st);
  case LT: return ExprMgr_simplify_lt(self, st, left, right);
  case LE: return ExprMgr_le(self, left, right, st);
  case GT: return ExprMgr_simplify_gt(self, st, left, right);
  case GE: return ExprMgr_ge(self, left, right, st);

    /* case */
  case IFTHENELSE:
  case CASE:
    nusmv_assert(node_get_type(left) == COLON);
    return ExprMgr_ite(self, car(left), cdr(left), right, st);

  case NEXT: return ExprMgr_next(self, left, st);

    /* scalar */
  case UMINUS: return ExprMgr_unary_minus(self, left);
  case PLUS: return ExprMgr_plus(self, left, right);
  case MINUS: return ExprMgr_minus(self, left, right);
  case TIMES: return ExprMgr_times(self, left, right);
  case DIVIDE: return ExprMgr_divide(self, left, right);
  case MOD: return ExprMgr_mod(self, left, right);

    /* function */
  case NFUNCTION:
    return ExprMgr_function(self, left, right);

    /* word-specific */
  case CAST_WORD1: return ExprMgr_bool_to_word1(self, left);
  case CAST_BOOL: return ExprMgr_word1_to_bool(self, left);
  case CAST_SIGNED: return ExprMgr_unsigned_word_to_signed(self, left);
  case CAST_UNSIGNED: return ExprMgr_signed_word_to_unsigned(self, left);
  case EXTEND: return ExprMgr_simplify_word_extend(self, st, left, right);
  case LSHIFT: return ExprMgr_word_left_shift(self, left, right);
  case RSHIFT: return ExprMgr_word_right_shift(self, left, right);
  case LROTATE: return ExprMgr_word_left_rotate(self, left, right);
  case RROTATE: return ExprMgr_word_right_rotate(self, left, right);
  case BIT_SELECTION: return ExprMgr_simplify_word_bit_select(self, st, left, right);
  case CONCATENATION: return ExprMgr_word_concatenate(self, left, right);

    /* wants number rsh */
  case ATTIME:
    nusmv_assert(node_get_type(right) == NUMBER);
    return ExprMgr_attime(self, left, node_get_int(right), st);

    /* sets are simplified when involving constants only */
  case UNION: return ExprMgr_union(self, left, right);

    /* sets are simplified when involving constants only */
  case SETIN: return ExprMgr_setin(self, left, right, st);

    /* ranges are simplified when low and high coincide */
  case TWODOTS: return ExprMgr_range(self, left, right);

    /* no simplification */
  case EQDEF:
  case CONS:
  case CONTEXT:
  case COLON:
  case SMALLINIT:

    /* no simplification in current implementation: */
  case EX:
  case AX:
  case EG:
  case AG:
  case EF:
  case AF:
  case OP_NEXT:
  case OP_PREC:
  case OP_NOTPRECNOT:
  case OP_FUTURE:
  case OP_ONCE:
  case OP_GLOBAL:
  case OP_HISTORICAL:
  case EBF:
  case ABF:
  case EBG:
  case ABG:
  case EBU:
  case ABU:
  case EU:
  case AU:
  case MINU:
  case MAXU:
  case UNTIL:
  case RELEASES:
  case SINCE:
  case TRIGGERED:
    return FN(self, type, left, right);

  default:
    return FN(self, type, left, right);
  }

  return EXPR(NULL);
}

Expr_ptr ExprMgr_simplify(const ExprMgr_ptr self, SymbTable_ptr st, Expr_ptr expr)
{
  Expr_ptr res;
  hash_ptr hash_memoize;

  /* NOTE FOR DEVELOPERS. In order to disable global memoization of
     simplification results make the below macro be equal to 0 instead of 1.
     This is purely debugging feature
  */
#define _ENABLE_GLOBAL_SIMPLIFICATION_MEMOIZATION_ 1

#if _ENABLE_GLOBAL_SIMPLIFICATION_MEMOIZATION_
  hash_memoize = SymbTable_get_handled_hash_ptr(
      st,
      EXPR_SIMPLIFIER_HASH,
      NULL, NULL, NULL, NULL,
      SymbTable_clear_handled_remove_action_hash, /* remove action */
      NULL);
#else
  hash_memoize = new_assoc();
#endif

  CATCH(self->errors) {
    /* There are several cases where this does not work */

    res = expr_simplify_aux(self, st, expr, hash_memoize);
  }
  FAIL(self->errors) {
    ErrorMgr_rpterr(self->errors, "An error occurred during Expr_simplify");
  }

#if ! _ENABLE_GLOBAL_SIMPLIFICATION_MEMOIZATION_
  free_assoc(hash_memoize);
#endif

  return res;
}

boolean ExprMgr_is_timed(const ExprMgr_ptr self, Expr_ptr expr, hash_ptr cache)
{
  /* It would be nice to memoize this function */
  boolean res;
  if((hash_ptr)NULL == cache) {
    cache = new_assoc();
    res = expr_is_timed_aux(expr, cache);
    free_assoc(cache);
  }
  else {
    res = expr_is_timed_aux(expr, cache);
  }

  return res;
}

int ExprMgr_get_time(const ExprMgr_ptr self, SymbTable_ptr st, Expr_ptr expr)
{
  hash_ptr h;
  int res;

  h = new_assoc();
  res = expr_get_curr_time(self, st, expr, h);
  free_assoc(h);

  return res;
}

boolean ExprMgr_time_is_dont_care(const ExprMgr_ptr self, int time)
{
  return time == EXPR_UNTIMED_DONTCARE;
}

boolean ExprMgr_time_is_current(const ExprMgr_ptr self, int time)
{
  return time == EXPR_UNTIMED_CURRENT;
}

boolean ExprMgr_time_is_next(const ExprMgr_ptr self, int time)
{
  return time == EXPR_UNTIMED_NEXT;
}

Expr_ptr ExprMgr_untimed(const ExprMgr_ptr self, SymbTable_ptr st, Expr_ptr expr)
{
  int time;

  time = ExprMgr_get_time(self, st, expr);
  return ExprMgr_untimed_explicit_time(self, st, expr, time);
}

Expr_ptr ExprMgr_untimed_explicit_time(const ExprMgr_ptr self, SymbTable_ptr st, Expr_ptr expr,
                                       int curr_time)
{
  hash_ptr h;
  Expr_ptr res;

  h = new_assoc();
  res = expr_timed_to_untimed(self, st, expr, curr_time, false, h);
  free_assoc(h);

  return res;
}

Expr_ptr ExprMgr_word_constant(const ExprMgr_ptr self,
                               const SymbTable_ptr symb_table,
                               int type,
                               Expr_ptr l,
                               Expr_ptr r)
{
  node_ptr value;
  node_ptr size;

  int size_int;
  int value_int;

  int size_type = AND;
  int value_type = AND;

  WordNumber_ptr value_word;
  unsigned long long tmp;

  nusmv_assert((type == UWCONST || type == SWCONST));

  if (SYMB_TABLE(NULL) != symb_table) {
    value = CompileFlatten_resolve_number(symb_table, NODE_PTR(l), Nil);
    size = CompileFlatten_resolve_number(symb_table, NODE_PTR(r), Nil);

    size_type = node_get_type(size);
    value_type = node_get_type(value);
  }

  if ((NUMBER == size_type ||
       NUMBER_UNSIGNED_WORD == size_type ||
       NUMBER_SIGNED_WORD == size_type) &&
      (NUMBER == value_type ||
       NUMBER_UNSIGNED_WORD == value_type ||
       NUMBER_SIGNED_WORD == value_type)) {
    /*  process the size: it can be an integer or word number in range */
    /*  [0, max-allowed-size] */
    switch (node_get_type(size)) {
    case NUMBER:
      size_int = node_get_int(size);
      break;

    case NUMBER_UNSIGNED_WORD:
      tmp = WordNumber_get_unsigned_value(WORD_NUMBER(car(size)));
      size_int = tmp;
      if (tmp != size_int) {
        ErrorMgr_rpterr(self->errors, "size specifier of swconst/uwconst operator is "
                        "not representable as int");
      }
      break;

    case NUMBER_SIGNED_WORD:
      tmp = WordNumber_get_signed_value(WORD_NUMBER(car(size)));
      size_int = tmp;
      if (tmp != size_int) {
        ErrorMgr_rpterr(self->errors, "size specifier of swconst/uwconst operator is "
                        "not representable as int");
      }
      break;

    default: error_unreachable_code();
    }

    if (size_int <= 0 || size_int > WordNumberMgr_max_width()) {
      ErrorMgr_rpterr(self->errors, "size specifier is out of range [0, %i]",
                      WordNumberMgr_max_width());
    }

    /*  process the value: it can be only integer and has to be */
    /*  representable with given size. */
    if (NUMBER != node_get_type(value)) {
      ErrorMgr_rpterr(self->errors, "value specifier of swconst/uwconst operator is not "
                      "an integer constant");
    }

    value_int = node_get_int(value);

    /* two shifts are done because shift by the full width isn't allowed in
       C. If value is positive, an extra bit of width is needed to avoid
       overflow. */
    if ((value_int > 0 &&
         ((UWCONST == type && value_int >> (size_int-1) >> 1 != 0) ||
          (SWCONST == type && value_int >> (size_int-2) >> 1 != 0))) ||
        (value_int < 0 && value_int >> (size_int-1) != -1)) {
      ErrorMgr_rpterr(self->errors, "value specifier of swconst/uwconst operator is not "
                      "representable with provided width");
    }

    if (value_int >= 0) {
      value_word = WordNumberMgr_integer_to_word_number(self->words,
                                                        value_int,
                                                        size_int);
    }
    else {
      value_word = WordNumberMgr_signed_integer_to_word_number(self->words,
                                                               value_int,
                                                               size_int);
    }

    nusmv_assert(WORD_NUMBER(NULL) != value_word);

    if (UWCONST == type) {
      return FN(self, NUMBER_UNSIGNED_WORD, NODE_PTR(value_word), Nil);
    }
    else return FN(self, NUMBER_SIGNED_WORD, NODE_PTR(value_word), Nil);
  }

  /* no simplification possible */
  return FN(self, type, NODE_PTR(l), NODE_PTR(r));
}

Expr_ptr ExprMgr_wsizeof(const ExprMgr_ptr self, Expr_ptr l, Expr_ptr r)
{
  int width;
  int type;

  nusmv_assert(EXPR(NULL) == r);

  type = node_get_type(NODE_PTR(l));

  if (NUMBER_SIGNED_WORD == type || NUMBER_UNSIGNED_WORD == type) {
    width = WordNumber_get_width(WORD_NUMBER(car(NODE_PTR(l))));

    nusmv_assert(0 < width);

    return FN(self, NUMBER, NODE_FROM_INT(width), Nil);
  }
  return FN(self, WSIZEOF, NODE_PTR(l), Nil);
}

Expr_ptr ExprMgr_cast_toint(const ExprMgr_ptr self, Expr_ptr l, Expr_ptr r)
{
  int type;

  nusmv_assert(EXPR(NULL) == r);

  type = node_get_type(NODE_PTR(l));

  if (NUMBER == type || INTEGER == type) return l;
  else return FN(self, CAST_TOINT, NODE_PTR(l), Nil);
}

Expr_ptr ExprMgr_floor(const ExprMgr_ptr self, Expr_ptr l)
{
  int type;

  type = node_get_type(NODE_PTR(l));

  if (NUMBER == type || INTEGER == type) return l;
  else return FN(self, FLOOR, NODE_PTR(l), Nil);
}

Expr_ptr ExprMgr_simplify_floor(const ExprMgr_ptr self, const SymbTable_ptr symb_table, Expr_ptr body)
{
  TypeChecker_ptr type_checker;
  SymbType_ptr symb_type;

  type_checker = SymbTable_get_type_checker(symb_table);
  symb_type  = TypeChecker_get_expression_type(type_checker, body, Nil);

  nusmv_assert(SymbType_is_infinite_precision(symb_type));

  if (SymbType_is_integer(symb_type)) {
    return body;
  }
  else return FN(self, FLOOR, NODE_PTR(body), Nil);
}

Expr_ptr ExprMgr_plus_one(const ExprMgr_ptr self, const Expr_ptr a)
{
  const WordNumberMgr_ptr words = self->words;
  int ta = node_get_type(a);

  nusmv_assert(! expr_is_bool(self, a));
  nusmv_assert((ta == NUMBER) ||
               (ta == NUMBER_UNSIGNED_WORD) ||
               (ta == NUMBER_SIGNED_WORD));

  if (ta == NUMBER) {
    return FN(self, NUMBER,
              NODE_FROM_INT((node_get_int(a) + 1)),
              Nil);
  }
  else {
    const WordNumber_ptr one =
      WordNumberMgr_integer_to_word_number(words, 1ULL,
                                           WordNumber_get_width(WORD_NUMBER(car(a))));

#if ! DISABLE_EXPR_POINTERS_ORDERING
    if (WORD_NUMBER(car(a)) > one) {
      return FN(self, ta,
                (node_ptr)WordNumberMgr_plus(self->words, one,
                                             WORD_NUMBER(car(a))),
                Nil);
    }
#endif

    return FN(self, ta,
              (node_ptr)WordNumberMgr_plus(self->words, WORD_NUMBER(car(a)),
                                           one),
              Nil);
  }
}

Expr_ptr ExprMgr_minus_one(const ExprMgr_ptr self, const Expr_ptr a)
{
  const WordNumberMgr_ptr words = self->words;
  int ta = node_get_type(a);

  nusmv_assert(! expr_is_bool(self, a));
  nusmv_assert((ta == NUMBER) ||
               (ta == NUMBER_UNSIGNED_WORD) ||
               (ta == NUMBER_SIGNED_WORD));

  if (ta == NUMBER) {
    return FN(self, NUMBER,
              NODE_FROM_INT((node_get_int(a) - 1)),
              Nil);
  }
  else {
    const WordNumber_ptr one =
      WordNumberMgr_integer_to_word_number(words, 1ULL,
                                           WordNumber_get_width(WORD_NUMBER(car(a))));

#if ! DISABLE_EXPR_POINTERS_ORDERING
    if (WORD_NUMBER(car(a)) > one) {
      return FN(self, ta,
                (node_ptr)WordNumberMgr_minus(self->words, one,
                                              WORD_NUMBER(car(a))),
                Nil);
    }
#endif

    return FN(self, ta,
              (node_ptr)WordNumberMgr_minus(self->words, WORD_NUMBER(car(a)),
                                            one),
              Nil);
  }
}

boolean ExprMgr_is_equal_to_zero(const ExprMgr_ptr self, const Expr_ptr input)
{
  nusmv_assert((node_get_type(input) == NUMBER) ||
               (node_get_type(input) == NUMBER_UNSIGNED_WORD) ||
               (node_get_type(input) == NUMBER_SIGNED_WORD));

  UNUSED_PARAM(self);

  switch(node_get_type(input)) {
  case NUMBER:
    return (0 == node_get_int(input));
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
    return WordNumber_is_zero(WORD_NUMBER(car(input)));
  default:
    error_unreachable_code();
  }
}

boolean ExprMgr_is_ge_to_number(const ExprMgr_ptr self, const Expr_ptr input,
                                const Expr_ptr number)
{
  const WordNumberMgr_ptr words = self->words;

  nusmv_assert((node_get_type(input) == NUMBER) ||
               (node_get_type(input) == NUMBER_UNSIGNED_WORD) ||
               (node_get_type(input) == NUMBER_SIGNED_WORD));

  nusmv_assert(node_get_type(number) == NUMBER);

  UNUSED_PARAM(self);

  switch(node_get_type(input)) {
  case NUMBER:
    return (node_get_int(input) >= node_get_int(number));
  case NUMBER_UNSIGNED_WORD:
    {
      WordNumber_ptr value =
        WordNumberMgr_integer_to_word_number(words, node_get_int(number),
                                             WordNumber_get_width(WORD_NUMBER(car(input))));
      return WordNumber_unsigned_greater_or_equal(WORD_NUMBER(car(input)), value);
    }
  case NUMBER_SIGNED_WORD:
    {
      WordNumber_ptr value =
        WordNumberMgr_integer_to_word_number(words, node_get_int(number),
                                             WordNumber_get_width(WORD_NUMBER(car(input))));
      return WordNumber_signed_greater_or_equal(WORD_NUMBER(car(input)), value);
    }

  default:
    error_unreachable_code();
  }
}

Expr_ptr ExprMgr_plus_number(const ExprMgr_ptr self, const Expr_ptr a,
                             const Expr_ptr b)
{
  const WordNumberMgr_ptr words = self->words;
  int ta = node_get_type(a);
  int tb = node_get_type(b);

  nusmv_assert(! (expr_is_bool(self, a) || expr_is_bool(self, b)));
  nusmv_assert(NUMBER == tb);
  nusmv_assert((ta == NUMBER) ||
               (ta == NUMBER_UNSIGNED_WORD) ||
               (ta == NUMBER_SIGNED_WORD));


  if (ta == NUMBER) {
    return FN(self, NUMBER,
              NODE_FROM_INT((node_get_int(a) +
                             node_get_int(b))),
              Nil);
  }
  else {
    WordNumber_ptr converted =
      WordNumberMgr_signed_integer_to_word_number(words, node_get_int(b),
                                                  WordNumber_get_width(WORD_NUMBER(car(a))));

    return ExprMgr_plus(self, a, FN(self, ta, NODE_PTR(converted), Nil));

  }
}

boolean ExprMgr_is_syntax_correct(Expr_ptr exp, ExprKind expectedKind)
{
  switch(node_get_type(exp)) {
    /* WARNING [MD] Some of these are not leaves, maybe they must be visited */
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
  case SELF:
  case ARRAY:
  case COUNT:
    return true;

    /* unary operators incompatible with LTL and CTL operator */
  case CAST_BOOL:
  case CAST_WORD1:
  case CAST_SIGNED:
  case CAST_UNSIGNED:
  case WSIZEOF:
  case CAST_TOINT:
  case TYPEOF:
    if (EXPR_CTL == expectedKind) {
      return ExprMgr_is_syntax_correct(car(exp), EXPR_SIMPLE);
    }
    else if (EXPR_LTL == expectedKind) {
      return ExprMgr_is_syntax_correct(car(exp), EXPR_NEXT);
    }

    FALLTHROUGH

      /* unary operators compatible with LTL and CTL operator */
  case NOT:
  case UMINUS:
    return ExprMgr_is_syntax_correct(car(exp), expectedKind);

    /* binary opertors incompatible with LTL and CTL operator */
  case BIT_SELECTION:
  case CASE: case COLON:
  case CONCATENATION:
  case TIMES: case DIVIDE: case PLUS :case MINUS: case MOD:
  case LSHIFT: case RSHIFT: case LROTATE: case RROTATE:
  case WAREAD: case WAWRITE: /* AC ADDED THESE */
  case CONST_ARRAY:
  case UNION: case SETIN:
  case EQUAL: case NOTEQUAL: case LT: case GT: case LE: case GE:
  case IFTHENELSE:
  case EXTEND:
  case WRESIZE:
  case CAST_TO_UNSIGNED_WORD:
    if (EXPR_CTL == expectedKind) {
      return ExprMgr_is_syntax_correct(car(exp), EXPR_SIMPLE)
        && ExprMgr_is_syntax_correct(cdr(exp), EXPR_SIMPLE);
    }
    else if (EXPR_LTL == expectedKind) {
      return ExprMgr_is_syntax_correct(car(exp), EXPR_NEXT)
        && ExprMgr_is_syntax_correct(cdr(exp), EXPR_NEXT);
    }

    FALLTHROUGH

    /* binary opertors compatible LTL and CTL operator */
  case AND: case OR: case XOR: case XNOR: case IFF: case IMPLIES:
    return ExprMgr_is_syntax_correct(car(exp), expectedKind)
      && ExprMgr_is_syntax_correct(cdr(exp), expectedKind);

    /* next expression */
  case NEXT:
    if (EXPR_NEXT != expectedKind &&
        EXPR_LTL != expectedKind) {
      return false;
    }
    /* NEXT cannot contain NEXT */
    return ExprMgr_is_syntax_correct(car(exp), EXPR_SIMPLE);

    /* CTL unary expressions */
  case EX: case AX: case EF: case AF: case EG: case AG:
  case ABU: case EBU:
  case EBF: case ABF: case EBG: case ABG:
    if (EXPR_CTL != expectedKind) {
      return false;
    }
    return ExprMgr_is_syntax_correct(car(exp), EXPR_CTL);

    /* CTL binary expressions */
  case AU: case EU:
    if (EXPR_CTL != expectedKind) {
      return false;
    }
    return ExprMgr_is_syntax_correct(car(exp), EXPR_CTL)
      && ExprMgr_is_syntax_correct(cdr(exp), EXPR_CTL);


    /* LTL unary expressions */
  case OP_NEXT: case OP_PREC: case OP_NOTPRECNOT: case OP_GLOBAL:
  case OP_HISTORICAL: case OP_FUTURE: case OP_ONCE:
    if (EXPR_LTL != expectedKind) {
      return false;
    }
    return ExprMgr_is_syntax_correct(car(exp), EXPR_LTL);

    /* LTL binary expressions */
  case UNTIL: case SINCE:
    if (EXPR_LTL != expectedKind) {
      return false;
    }
    return ExprMgr_is_syntax_correct(car(exp), EXPR_LTL)
      && ExprMgr_is_syntax_correct(cdr(exp), EXPR_LTL);

  default:
    break;
  }

  return false;
}

int* ExprMgr_get_time_interval(const ExprMgr_ptr self,
                               SymbTable_ptr st,
                               Expr_ptr expr)
{
  hash_ptr h;
  int* res = ALLOC(int, 2);

  h = new_assoc();
  expr_get_curr_time_interval(self, st, expr, h, &res[0], &res[1]);
  free_assoc(h);

  return res;
}

Expr_ptr ExprMgr_move_next_to_leaves(const ExprMgr_ptr self,
                                     SymbTable_ptr st, Expr_ptr expr)
{
  return move_next_to_leaves_recur(self, st, expr, false);
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The ExprMgr class private initializer

  The ExprMgr class private initializer

  \sa ExprMgr_create
*/
static void expr_mgr_init(ExprMgr_ptr self, const NuSMVEnv_ptr env)
{
  env_object_init(ENV_OBJECT(self), env);

  /* members initialization */
  self->nodes = NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  self->words = WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));
  self->errors = ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  /* Those are used widely, initialize them now */
  self->expr_true = FN(self, TRUEEXP, Nil, Nil);
  self->expr_false = FN(self, FALSEEXP, Nil, Nil);

  OVERRIDE(Object, finalize) = expr_mgr_finalize;
}

/*!
  \brief The ExprMgr class private deinitializer

  The ExprMgr class private deinitializer

  \sa ExprMgr_destroy
*/
static void expr_mgr_deinit(const ExprMgr_ptr self)
{
  /* members deinitialization */

  env_object_deinit(ENV_OBJECT(self));
}

/*!
  \brief The ExprMgr class virtual finalizer

  Called by the class destructor
*/
static void expr_mgr_finalize(Object_ptr object, void* dummy)
{
  ExprMgr_ptr self = EXPR_MGR(object);

  expr_mgr_deinit(self);
  FREE(self);
}

/*!
  \brief Converts a timed node into an untimed node

  Converts a timed node into an untimed node

  \se None
*/
static Expr_ptr
expr_timed_to_untimed(const ExprMgr_ptr self,
                      SymbTable_ptr st, Expr_ptr expr, int curr_time,
                      boolean in_next, hash_ptr cache)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  node_ptr key;
  node_ptr res;

  if (expr == Nil)
    return Nil;

  key = in_next ? find_node(self->nodes, NEXT, expr, Nil) : expr;
  res = find_assoc(cache, key);

  if (Nil != res)
    return res;

  switch (node_get_type(expr)) {
    /* leaves */
  case FAILURE:
  case ARRAY:
  case BIT:
  case DOT:
  case ATOM:
  case NUMBER_SIGNED_WORD:
  case NUMBER_UNSIGNED_WORD:
  case UWCONST:
  case SWCONST:
  case WORDARRAY:
  case NUMBER:
  case NUMBER_REAL:
  case NUMBER_FRAC:
  case NUMBER_EXP:
  case TRUEEXP:
  case FALSEEXP:
    res  = expr;
    break;

  case ATTIME:
    {
      /* a frozen var must be time compatible with any time */
      int time2 = SymbTable_is_symbol_frozen_var(st, car(expr))
        ? curr_time : node_get_int(cdr(expr));

      if (time2 == EXPR_UNTIMED_CURRENT || time2 == curr_time) {
        res = expr_timed_to_untimed(self, st, car(expr), curr_time,
                                    in_next, cache);
      }
      else if (time2 == EXPR_UNTIMED_NEXT || time2 == curr_time+1) {
        if (in_next) {
          const MasterPrinter_ptr wffprint =
            MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

          ErrorMgr_internal_error(self->errors, "%s:%d:%s: Invalid nested NEXT (%s)",
                                  __FILE__, __LINE__, __func__,
                                  sprint_node(wffprint, expr));
        }
        res = find_node(self->nodes, NEXT,
                        expr_timed_to_untimed(self, st, car(expr),
                                              curr_time,
                                              true, cache),
                        Nil);
      }
      else {
        const MasterPrinter_ptr wffprint =
          MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
        ErrorMgr_internal_error(self->errors, "%s:%d:%s: Invalid ATTIME node (%s)",
                                __FILE__, __LINE__, __func__, sprint_node(wffprint, expr));
      }

      break;
    }

  case NEXT:
    if (in_next) {
      const MasterPrinter_ptr wffprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
      ErrorMgr_internal_error(self->errors, "%s:%d:%s: Invalid nested NEXT (%s)",
                              __FILE__, __LINE__, __func__,
                              sprint_node(wffprint, expr));
    }
    res = find_node(self->nodes, NEXT,
                    expr_timed_to_untimed(self, st, car(expr),
                                          curr_time,
                                          true, cache),
                    Nil);

    break;

  default:
    {
      node_ptr lt = expr_timed_to_untimed(self, st, car(expr),
                                          curr_time,
                                          in_next, cache);

      node_ptr rt = expr_timed_to_untimed(self, st, cdr(expr),
                                          curr_time,
                                          in_next, cache);

      res = find_node(self->nodes, node_get_type(expr), lt, rt);

      break;
    }

  } /* switch */

  insert_assoc(cache, key, res);
  nusmv_assert(Nil != res);
  return res;
}

/*!
  \brief Calculates current time for an expression

  Private service of Expr_get_time

  \se None
*/
static int expr_get_curr_time(const ExprMgr_ptr self,
                              SymbTable_ptr st, node_ptr expr, hash_ptr cache)
{
  node_ptr tmp = find_assoc(cache, expr);
  int res;

  if (Nil != tmp)
    return NODE_TO_INT(tmp) - EXPR_TIME_OFS;

  if (expr == Nil) {
    return EXPR_UNTIMED_DONTCARE;
  }

  res = 0;
  switch (node_get_type(expr)) {

    /* leaves */
  case DOT:
  case ATOM:
    {
      ResolveSymbol_ptr rs;
      node_ptr name;

      rs = SymbTable_resolve_symbol(st, expr, Nil);
      name = ResolveSymbol_get_resolved_name(rs);

      /* handle frozenvars as a special case with lookahead */
      if (SymbTable_is_symbol_frozen_var(st, name)) {
        return EXPR_UNTIMED_DONTCARE;
      }
    }

  case FAILURE:
  case ARRAY:
  case BIT:
  case NUMBER_SIGNED_WORD:
  case NUMBER_UNSIGNED_WORD:
  case UWCONST:
  case SWCONST:
  case WORDARRAY:
  case NUMBER:
  case NUMBER_REAL:
  case NUMBER_FRAC:
  case NUMBER_EXP:
  case TRUEEXP:
  case FALSEEXP:
      return EXPR_UNTIMED_CURRENT;

  case ATTIME: {
    int time1 = node_get_int(cdr(expr));
    int time2 = expr_get_curr_time(self, st, car(expr), cache);

    if (time2 == EXPR_UNTIMED_DONTCARE) {
      res = EXPR_UNTIMED_DONTCARE;
    }
    else if (time2 == EXPR_UNTIMED_CURRENT) {
      res = time1; /* time1 is absolute */
    }
    else if (time2 == EXPR_UNTIMED_NEXT) {
      ErrorMgr_internal_error(self->errors, "%s:%d:%s: Unexpected NEXT",
                     __FILE__, __LINE__, __func__);
    }
    else { /* time2 is absolute and this is wrong */
      nusmv_assert(0 <= time2);
      ErrorMgr_internal_error(self->errors, "%s:%d:%s: Invalid nested ATTIME",
                     __FILE__, __LINE__, __func__);
    }

    break;
  }

  default:
    {
      int time1 = expr_get_curr_time(self, st, car(expr), cache);
      int time2 = expr_get_curr_time(self, st, cdr(expr), cache);

      /* both are DON'T CARE? */
      if ((EXPR_UNTIMED_DONTCARE == time1) && (EXPR_UNTIMED_DONTCARE == time2)) {
        res = EXPR_UNTIMED_DONTCARE;
      }

      /* one (but not both) is DON'T CARE? */
      else if (EXPR_UNTIMED_DONTCARE == time1) {
        res = time2;
      }

      else if (EXPR_UNTIMED_DONTCARE == time2) {
        res = time1;
      }

      /* both are CURRENT? */
      else if ((EXPR_UNTIMED_CURRENT == time1) &&
          (EXPR_UNTIMED_CURRENT == time2)) {
        res = EXPR_UNTIMED_CURRENT;
      }

      /* one is CURRENT, the other is not */
      else if (EXPR_UNTIMED_CURRENT == time1) {
        res = time2;
      }

      else if (EXPR_UNTIMED_CURRENT == time2) {
        res = time1;
      }

      else {
        /* times can only be absolute beyond this point */
        nusmv_assert ((0 <= time1) && (0 <= time2));
        res = MIN(time1, time2);
      }

      break;
    }

  } /* switch */

  /* Cache */
  insert_assoc(cache, expr, NODE_FROM_INT(res + EXPR_TIME_OFS));
  return res;
}

/*!
  \brief Recursive auxiliary function for Expr_simplify

  Recursive auxiliary function for Expr_simplify

  \sa Expr_simplify
*/
static Expr_ptr expr_simplify_aux(ExprMgr_ptr self,
                                  SymbTable_ptr st, Expr_ptr expr,
                                  hash_ptr hash)
{
  node_ptr res = (node_ptr) NULL;
  int type;

  if (expr == Nil) return Nil;

  /* check memoization */
  res = find_assoc(hash, expr);
  if (res != (node_ptr) NULL) {
    return res;
  }

  type = node_get_type(expr);
  switch (type) {
    /* boolean leaves */
  case TRUEEXP: return ExprMgr_true(self);
  case FALSEEXP: return ExprMgr_false(self);

    /* name leaves */
  case ATOM:
  case BIT:
    return find_node(self->nodes, type, car(expr), cdr(expr));

    /* other leaves */
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
  case FAILURE:
    return find_node(self->nodes, type, car(expr), cdr(expr));

  case UWCONST:
  case SWCONST:
  case WRESIZE:
    {
      Expr_ptr left = expr_simplify_aux(self, st, car(expr), hash);
      Expr_ptr right = expr_simplify_aux(self, st, cdr(expr), hash);
      res = ExprMgr_resolve(self, st, type, left, right);
      break;
    }

  case DOT:
  case ARRAY:
    return find_node(self->nodes, type,
                     expr_simplify_aux(self, st, car(expr), hash),
                     expr_simplify_aux(self, st, cdr(expr), hash));

    /* unary */
  case NOT:
  case NEXT:
  case UMINUS:
  case WSIZEOF:
  case CAST_TOINT:
  case FLOOR:
  {
    Expr_ptr left = expr_simplify_aux(self, st, car(expr), hash);
    res = ExprMgr_resolve(self, st, type, left, Nil);
    break;
  }

    /* binary with lazy eval */
  case AND:
  {
    Expr_ptr left = expr_simplify_aux(self, st, car(expr), hash);
    if (ExprMgr_is_false(self, left)) res = left;
    else res = ExprMgr_resolve(self, st, type, left, expr_simplify_aux(self, st, cdr(expr), hash));
    break;
  }

  case OR:
  {
    Expr_ptr left = expr_simplify_aux(self, st, car(expr), hash);
    if (ExprMgr_is_true(self, left)) res = left;
    else res = ExprMgr_resolve(self, st, type, left, expr_simplify_aux(self, st, cdr(expr), hash));
    break;
  }

  case IMPLIES:
  {
    Expr_ptr left = expr_simplify_aux(self, st, car(expr), hash);
    if (ExprMgr_is_false(self, left)) res = ExprMgr_true(self);
    else res = ExprMgr_resolve(self, st, type, left, expr_simplify_aux(self, st, cdr(expr), hash));
    break;
  }

  /* binary, no lazyness */
  case IFF:
  case XOR:
  case XNOR:
  case EQUAL:
  case NOTEQUAL:
  case LT:
  case LE:
  case GT:
  case GE:
  case PLUS:
  case MINUS:
  case TIMES:
  case DIVIDE:
  case MOD:
  case CAST_WORD1:
  case CAST_BOOL:
  case CAST_SIGNED:
  case CAST_UNSIGNED:
  case EXTEND:
  case LSHIFT:
  case RSHIFT:
  case LROTATE:
  case RROTATE:
  case BIT_SELECTION:
  case CONCATENATION:
    res = ExprMgr_resolve(self, st, type,
                       expr_simplify_aux(self, st, car(expr), hash),
                       expr_simplify_aux(self, st, cdr(expr), hash));
    break;

    /* case with lazyness on condition */
  case IFTHENELSE:
  case CASE:
  {
    Expr_ptr cond = expr_simplify_aux(self, st, car(car(expr)), hash);
    Expr_ptr _then, _else;

    if (ExprMgr_is_true(self, cond)) {
      _then = expr_simplify_aux(self, st, cdr(car(expr)), hash);
      _else = cdr(expr);
    }
    else if (ExprMgr_is_false(self, cond)) {
      _then = cdr(car(expr));
      _else = expr_simplify_aux(self, st, cdr(expr), hash);
    }
    else {
      _then = expr_simplify_aux(self, st, cdr(car(expr)), hash);
      _else = expr_simplify_aux(self, st, cdr(expr), hash);
    }

    res = ExprMgr_resolve(self, st, type, find_node(self->nodes, COLON, cond, _then), _else);
    break;
  }

  /* sets are simplified when possible */
  case SETIN:
  case UNION:
    res = ExprMgr_resolve(self, st, type,
                       expr_simplify_aux(self, st, car(expr), hash),
                       expr_simplify_aux(self, st, cdr(expr), hash));
    break;

    /* ranges are simplified */
  case TWODOTS:
    res = ExprMgr_resolve(self, st, type,
                       expr_simplify_aux(self, st, car(expr), hash),
                       expr_simplify_aux(self, st, cdr(expr), hash));
    break;

    /* no simplification */
  case EQDEF:
  case CONS:
  case CONTEXT:

    /* no simplification in current implementation: */
  case EX:
  case AX:
  case EG:
  case AG:
  case EF:
  case AF:
  case OP_NEXT:
  case OP_PREC:
  case OP_NOTPRECNOT:
  case OP_FUTURE:
  case OP_ONCE:
  case OP_GLOBAL:
  case OP_HISTORICAL:
  case EU:
  case AU:
  case MINU:
  case MAXU:
  case UNTIL:
  case RELEASES:
  case SINCE:
  case TRIGGERED:
    res = find_node(self->nodes, type,
                    expr_simplify_aux(self, st, car(expr), hash),
                    expr_simplify_aux(self, st, cdr(expr), hash));
    break;

    /* These nodes need special treatment when used with
       Expr_resolve, since recursively enter into their cdr may
       break the formula. (Ranges with min = max are resolved as
       number by Expr_resolve). See issue 2194. */
  case EBF:
  case ABF:
  case EBG:
  case ABG:
  case EBU:
  case ABU:
    nusmv_assert(Nil == cdr(expr) || TWODOTS == node_get_type(cdr(expr)));

    res = ExprMgr_resolve(self, st, node_get_type(expr),
                       expr_simplify_aux(self, st, car(expr), hash),
                       cdr(expr));
    break;

  default:
    res = find_node(self->nodes, type,
                    expr_simplify_aux(self, st, car(expr), hash),
                    expr_simplify_aux(self, st, cdr(expr), hash));
    break;
  }

  /* memoize */
  insert_assoc(hash, expr, res);
  return EXPR(res);
}

/*!
  \brief casts boolean constants to WORD[1]


*/
static Expr_ptr expr_bool_to_word1(const ExprMgr_ptr self, const Expr_ptr a)
{
  if (ExprMgr_is_true(self, a)) {
    return find_node(self->nodes, NUMBER_UNSIGNED_WORD,
                     (node_ptr) WordNumberMgr_integer_to_word_number(self->words, 1,1), Nil);
  }

  if (ExprMgr_is_false(self, a)) {
    return find_node(self->nodes, NUMBER_UNSIGNED_WORD,
                     (node_ptr) WordNumberMgr_integer_to_word_number(self->words, 0,1), Nil);
  }

  return a;
}

/*!
  \brief true if expression is timed

  Private service of Expr_is_timed.
                       To represent 'true' in cache we use the constant 2 for
                       'false' we use 1 to avoid representation problems wrt Nil

  \se cache can be updated
*/
static boolean expr_is_timed_aux(Expr_ptr expr, hash_ptr cache)
{
  Expr_ptr tmp;
  boolean result;

  nusmv_assert((hash_ptr)NULL != cache);

  if (expr == Nil) return false;

  tmp = find_assoc(cache, expr);
  if(Nil != tmp) {
    return (NODE_TO_INT(tmp) == 2);
  }

  switch (node_get_type(expr)) {
    /* leaves */
  case FAILURE:
  case ARRAY:
  case BIT:
  case DOT:
  case ATOM:
  case NUMBER_SIGNED_WORD:
  case NUMBER_UNSIGNED_WORD:
  case UWCONST:
  case SWCONST:
  case WORDARRAY:
  case NUMBER:
  case NUMBER_REAL:
  case NUMBER_FRAC:
  case NUMBER_EXP:
  case TRUEEXP:
  case FALSEEXP:
    return false;

  case ATTIME:
    result = true;
    break;
  case NEXT:
    result = false;
    break;

  default:
    {
      boolean ll;

      ll = expr_is_timed_aux(car(expr), cache);
      if(ll) {
        result = true;
      }
      else {
        result = expr_is_timed_aux(cdr(expr), cache);
      }
    }
  } /* switch */

  if(result) {
    insert_assoc(cache, expr, NODE_FROM_INT(2));
  }
  else {
    insert_assoc(cache, expr, NODE_FROM_INT(1));
  }

  return result;
}

/*!
  \brief Check for an expr being boolean

  Check for an expr being boolean
*/
static boolean expr_is_bool(const ExprMgr_ptr self, const Expr_ptr a)
{
  return (ExprMgr_is_true(self, a) || ExprMgr_is_false(self, a));
}

/*!
  \brief


*/
static inline boolean expr_is_wordnumber_max(const ExprMgr_ptr self,
                                             const WordNumber_ptr word,
                                             const int type)
{
  return
    ((type == NUMBER_UNSIGNED_WORD &&
     WordNumber_equal(WordNumberMgr_max_unsigned_value(self->words,
                                                       WordNumber_get_width(word)),
                      word))
     ||
     (type == NUMBER_SIGNED_WORD &&
     WordNumber_equal(WordNumberMgr_max_signed_value(self->words,
                                                     WordNumber_get_width(word)),
                      word)));
}

/*!
  \brief Recursive aux function of ExprMgr_get_time_interval


*/
static void expr_get_curr_time_interval(ExprMgr_ptr self,
                                        SymbTable_ptr st,
                                        node_ptr expr,
                                        hash_ptr cache,
                                        int* min,
                                        int* max)
{
  node_ptr tmp = find_assoc(cache, expr);
  if (Nil != tmp) {
    *min = NODE_TO_INT(car(tmp)) - EXPR_TIME_OFS;
    *max = NODE_TO_INT(cdr(tmp)) - EXPR_TIME_OFS;
    return;
  }

  if (expr == Nil) {
    *min = EXPR_UNTIMED_DONTCARE;
    *max = EXPR_UNTIMED_DONTCARE;
    return;
  }

  switch (node_get_type(expr)) {

    /* leaves */
  case DOT:
  case ATOM:
    {
      ResolveSymbol_ptr rs;
      node_ptr name;

      rs = SymbTable_resolve_symbol(st, expr, Nil);
      name = ResolveSymbol_get_resolved_name(rs);

      /* handle frozenvars as a special case with lookahead */
      if (SymbTable_is_symbol_frozen_var(st, name)) {
        *min = EXPR_UNTIMED_DONTCARE;
        *max = EXPR_UNTIMED_DONTCARE;
        return;
      }
    }

  case FAILURE:
  case ARRAY:
  case BIT:
  case NUMBER_SIGNED_WORD:
  case NUMBER_UNSIGNED_WORD:
  case UWCONST:
  case SWCONST:
  case WORDARRAY:
  case NUMBER:
  case NUMBER_REAL:
  case NUMBER_FRAC:
  case NUMBER_EXP:
  case TRUEEXP:
  case FALSEEXP:
    *min = EXPR_UNTIMED_CURRENT;
    *max = EXPR_UNTIMED_CURRENT;
    return;

  case ATTIME: {
    int at_time = node_get_int(cdr(expr));
    int min1 = INT_MIN, max1 = INT_MIN;
    expr_get_curr_time_interval(self, st, car(expr), cache, &min1, &max1);
    nusmv_assert(min1 == max1);/* must be single time inside attime */

    if (min1 == EXPR_UNTIMED_DONTCARE) {
      *min = EXPR_UNTIMED_DONTCARE;
      *max = EXPR_UNTIMED_DONTCARE;
    }
    else if (min1 == EXPR_UNTIMED_CURRENT) {
      *min = at_time; /* at_time is absolute */
      *max = at_time;
    }
    else if (min1 == EXPR_UNTIMED_NEXT) {
      ErrorMgr_internal_error(self->errors, "%s:%d:%s: Unexpected NEXT",
                     __FILE__, __LINE__, __func__);
    }
    else { /* time2 is absolute and this is wrong */
      nusmv_assert(0 <= at_time);
      ErrorMgr_internal_error(self->errors, "%s:%d:%s: Invalid nested ATTIME",
                     __FILE__, __LINE__, __func__);
    }

    break;
  }

  case NEXT: {
    int min1 = INT_MIN, max1 = INT_MIN;
    expr_get_curr_time_interval(self, st, car(expr), cache, &min1, &max1);
    nusmv_assert(min1 == max1);/* must be single time inside next */

    if (min1 == EXPR_UNTIMED_DONTCARE) {
      *min = EXPR_UNTIMED_DONTCARE;
      *max = EXPR_UNTIMED_DONTCARE;
    }
    else if (min1 == EXPR_UNTIMED_CURRENT) {
      *min = EXPR_UNTIMED_NEXT; /* at_time is absolute */
      *max = EXPR_UNTIMED_NEXT;
    }
    else if (min1 == EXPR_UNTIMED_NEXT) {
      ErrorMgr_internal_error(self->errors, "%s:%d:%s: Unexpected NEXT",
                     __FILE__, __LINE__, __func__);
    }
    else { /* time2 is absolute and this is wrong */
      ErrorMgr_internal_error(self->errors, "%s:%d:%s: Invalid nexted ATTIME",
                     __FILE__, __LINE__, __func__);
    }

    break;
  }
  default:
    {
      int min1 = INT_MIN, max1 = INT_MIN;
      int min2 = INT_MIN, max2 = INT_MIN;
      expr_get_curr_time_interval(self, st, car(expr), cache, &min1, &max1);
      expr_get_curr_time_interval(self, st, cdr(expr), cache, &min2, &max2);

      /* both are DON'T CARE? */
      if ((EXPR_UNTIMED_DONTCARE == min1) && (EXPR_UNTIMED_DONTCARE == min2)) {
        *min = EXPR_UNTIMED_DONTCARE;
        *max = EXPR_UNTIMED_DONTCARE;
      }

      /* one (but not both) is DON'T CARE? */
      else if (EXPR_UNTIMED_DONTCARE == min1) {
        *min = min2;
        *max = max2;
      }

      else if (EXPR_UNTIMED_DONTCARE == min2) {
        *min = min1;
        *max = max1;
      }

      else if (max1 < 0 && max2 < 0){
        *min = MIN(min1, min2);
        *max = MAX(max1, max2);
      }

      else if (max1 < 0){
        *min = min2;
        *max = max2;
      }

      else if (max2 < 0){
        *min = min1;
        *max = max1;
      }

      else {
        *min = MIN(min1, min2);
        *max = MAX(max1, max2);
      }

      break;
    }

  } /* switch */

  /* Cache */
  insert_assoc(cache, expr, FN(self,
                               CONS,
                               NODE_FROM_INT(*min + EXPR_TIME_OFS),
                               NODE_FROM_INT(*max + EXPR_TIME_OFS)));

  /* postconditions */
  nusmv_assert(*min <= *max);
  nusmv_assert(*max < 0 || *min >= 0);
  nusmv_assert(
    (*min == EXPR_UNTIMED_DONTCARE && *max == EXPR_UNTIMED_DONTCARE) ||
    (*min != EXPR_UNTIMED_DONTCARE && *max != EXPR_UNTIMED_DONTCARE));
}

/*!
  \brief Recursive aux function of ExprMgr_move_next_to_leaves


*/
static Expr_ptr move_next_to_leaves_recur(const ExprMgr_ptr self,
                                          SymbTable_ptr st,
                                          Expr_ptr expr, boolean in_next)
{
  if (expr == Nil) return Nil;

  switch (node_get_type(expr)) {
  case FAILURE:
  case TRUEEXP:
  case FALSEEXP:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
    return expr;

  case ATOM:
  case DOT:
  case ARRAY:
    {
      SymbCategory category;
      ResolveSymbol_ptr rs;
      node_ptr name;

      rs = SymbTable_resolve_symbol(st, expr, Nil);
      name = ResolveSymbol_get_resolved_name(rs);

      category = SymbTable_get_symbol_category(st, name);

      switch (category) {
      case SYMBOL_CONSTANT:
        /* It is an enumerative, then simply return it */
        return expr;

      case SYMBOL_STATE_VAR:
        return (in_next) ? ExprMgr_next(self, expr, st) : expr;

      case SYMBOL_FROZEN_VAR:
        /* In this case, regardless of next, the next(v) = v, thus
           return simply v */
        return expr;

      case SYMBOL_INPUT_VAR:
        /* It is an input, thus we expect not to be inside a next */
        if (!in_next) return expr;
        ErrorMgr_internal_error(self->errors, "%s:%d:%s: Invalid next arounf an input",
                                __FILE__, __LINE__, __func__);
        nusmv_assert(false);

      default:
        {
          const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
          const MasterPrinter_ptr wffprint =
            MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

          ErrorMgr_internal_error(self->errors, "%s:%d:%s: Expression not flattened and/or define not inlined.\n \"%s\"\n",
                                  __FILE__, __LINE__, __func__, sprint_node(wffprint, expr));
          nusmv_assert(false);
        }
      }
    }

  case NEXT:
    nusmv_assert(!in_next);
    return move_next_to_leaves_recur(self, st, car(expr), true);

  default:
    break;
  }

  {
    Expr_ptr left = move_next_to_leaves_recur(self, st, car(expr), in_next);
    Expr_ptr right = move_next_to_leaves_recur(self, st, cdr(expr), in_next);

    return ExprMgr_resolve(self, st, node_get_type(expr), left, right);
  }
}

/**AutomaticEnd***************************************************************/
