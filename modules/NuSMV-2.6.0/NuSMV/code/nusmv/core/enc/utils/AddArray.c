/* ---------------------------------------------------------------------------


  This file is part of the ``enc.utils'' package of NuSMV version 2.
  Copyright (C) 2005 by FBK-irst.

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
  \author Andrei Tchaltsev
  \brief \todo: Missing synopsis

  \todo: Missing description

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/enc/utils/AddArray.h"
#include "nusmv/core/dd/dd.h" /* required by functions of word arithmetic operations */
#include "nusmv/core/enc/operators.h" /* for word arithmetic operation */
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/WordNumberMgr.h"



/*---------------------------------------------------------------------------*/
/* Types definition                                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief A local pointer definition used by signed operators

  A pointer to a function in the form: AddArray_ptr
   foo(DDMgr_ptr , AddArray_ptr, AddArray_ptr) Used to minimize code
   of signed operators

  NB for developers: the struct AddArray_TAG is actually never
  defined, but just used to create a new type (for proper
  type-checking).  The actually type below AddArray_ptr is array_t*
*/
typedef AddArray_ptr (*APFDAA)(DDMgr_ptr , AddArray_ptr, AddArray_ptr);


/*---------------------------------------------------------------------------*/
/* Macro definition                                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief converts a type from (actually used) array_t* to our
  artificial AddArray_ptr

  The type AddArray_ptr is used just to hide
  array_t (to enable type-checking by compilers).
  But the actuall data-structure used is array_t.
*/
#define array2AddArray(array) ADD_ARRAY(array)

/*!
  \brief converts a type from our
  artificial AddArray_ptr to (actually used) array_t*

  See array2AddArray
*/
#define AddArray2array(addArray) ((array_t*)(addArray))


/*---------------------------------------------------------------------------*/
/* Variable definition                                                       */
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void add_array_full_adder(DDMgr_ptr dd,
                                 add_ptr arg1,
                                 add_ptr arg2,
                                 add_ptr carry_in,
                                 add_ptr* sum,
                                 add_ptr* carry_out);

static void add_array_adder(DDMgr_ptr dd,
                            AddArray_ptr arg1,
                            AddArray_ptr arg2,
                            add_ptr carry_in,
                            AddArray_ptr* res,
                            add_ptr* carry_out);

static AddArray_ptr add_array_word_plus_negated_and_one(DDMgr_ptr dd,
                                                        AddArray_ptr arg1,
                                                        AddArray_ptr arg2,
                                                        add_ptr* carry);

static AddArray_ptr add_array_negate_bits(DDMgr_ptr dd,
                                          AddArray_ptr arg);

static AddArray_ptr add_array_word_extend(DDMgr_ptr dd,
                                          AddArray_ptr arg,
                                          AddArray_ptr arg_repeat,
                                          add_ptr paddingBit);

static AddArray_ptr add_array_word_right_shift(DDMgr_ptr dd,
                                               AddArray_ptr arg,
                                               AddArray_ptr number,
                                               boolean isSigned);

static void add_array_unsigned_division_remainder(DDMgr_ptr dd,
                                                  AddArray_ptr arg1,
                                                  AddArray_ptr arg2,
                                                  AddArray_ptr* quotient,
                                                  AddArray_ptr* remainder);

static void
add_array_signed_division_remainder_simple(DDMgr_ptr dd,
                                           AddArray_ptr arg1,
                                           AddArray_ptr arg2,
                                           AddArray_ptr* quotient,
                                           AddArray_ptr* remainder);
static void
add_array_signed_division_remainder_hardware(DDMgr_ptr dd,
                                             AddArray_ptr arg1,
                                             AddArray_ptr arg2,
                                             AddArray_ptr* quotient,
                                             AddArray_ptr* remainder);

static add_ptr
add_array_create_default_value_of_shift_operation(DDMgr_ptr dd, AddArray_ptr number,
int width, add_ptr defaultBit, const char* errMessage);


static AddArray_ptr
add_array_word_signed_comparison(DDMgr_ptr dd, APFDAA op,
                               AddArray_ptr arg1, AddArray_ptr arg2);

static inline boolean
add_array_is_word(DDMgr_ptr dd, const AddArray_ptr number);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

AddArray_ptr AddArray_create(int number)
{
  nusmv_assert(number > 0);
  return array2AddArray(array_alloc(add_ptr, number));
}

AddArray_ptr AddArray_from_word_number(DDMgr_ptr dd, WordNumber_ptr wn)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  /* list of ADD encoding of Word */
  AddArray_ptr res;
  int i;
  const int width = WordNumber_get_width(wn);

  res = AddArray_create(width);
  for (i = 0; i < width; ++i) {
    node_ptr value = WordNumber_get_bit(wn, i) ? ExprMgr_true(exprs) : ExprMgr_false(exprs);
    AddArray_set_n(res, i, add_leaf(dd, value));
  }
  return res;
}

AddArray_ptr AddArray_from_add(add_ptr add)
{
  array_t* array = array_alloc(add_ptr, 1);
  array_insert(add_ptr, array, 0, add);
  return array2AddArray(array);
}

AddArray_ptr AddArray_duplicate(AddArray_ptr self)
{
  array_t* new;
  array_t* old;
  int i;

  old = AddArray2array(self);
  new = array_alloc(add_ptr, array_n(old));
  for (i = 0; i < array_n(old); ++i) {
    add_ptr add = array_fetch(add_ptr, old, i);
    add_ref(add);
    array_insert(add_ptr, new, i, add);
  }
  return array2AddArray(new);
}

void AddArray_destroy(DDMgr_ptr dd, AddArray_ptr self)
{
  int i;
  array_t* array = AddArray2array(self);
  for (i = 0; i < array_n(array); ++i) {
    add_free(dd, array_fetch(add_ptr, array, i));
  }
  array_free(array);
}

int AddArray_get_size(AddArray_ptr self)
{
  return array_n(AddArray2array(self));
}

add_ptr AddArray_get_add(AddArray_ptr self)
{
  array_t* array = AddArray2array(self);
  nusmv_assert(array_n(array) == 1);
  return array_fetch(add_ptr, array, 0);
}

add_ptr AddArray_get_n(AddArray_ptr self, int number)
{
  array_t* array = AddArray2array(self);
  return array_fetch(add_ptr, array, number);
}

array_t* AddArray_get_array(AddArray_ptr self)
{
  return AddArray2array(self);
}

size_t AddArray_get_add_size(const AddArray_ptr self, DDMgr_ptr dd)
{
  size_t size = 0;
  int i;

  for (i=AddArray_get_size(self)-1; i>=0; --i) {
    size += add_size(dd, AddArray_get_n(self, i));
  }
  return size;
}

void AddArray_set_n(AddArray_ptr self, int number, add_ptr add)
{
  array_t* array = AddArray2array(self);
  array_insert(add_ptr, array, number, add);
}

AddArray_ptr AddArray_word_apply_unary(DDMgr_ptr dd,
                                       AddArray_ptr arg1,
                                       FP_A_DA op)
{
  const int width = AddArray_get_size(arg1);
  AddArray_ptr res;
  int i;

  nusmv_assert(width > 0);

  res = AddArray_create(width);
  /* at every interation, process one bit and collect the result in res */
  for (i = 0; i < width; ++i) {
    AddArray_set_n(res, i, op(dd, AddArray_get_n(arg1, i)));
  }

  return res;
}

AddArray_ptr AddArray_word_apply_binary(DDMgr_ptr dd,
                                        AddArray_ptr arg1,
                                        AddArray_ptr arg2,
                                        FP_A_DAA op)
{
  const int width = AddArray_get_size(arg1);
  AddArray_ptr res;
  int i;

  /* the size of array should be the same and positive */
  nusmv_assert(width == AddArray_get_size(arg2) && width > 0);

  res = AddArray_create(width);
  /* at every interation, process one bit and collect the result in res */
  for (i = 0; i < width; ++i) {
    AddArray_set_n(res, i,
                   op(dd,
                      AddArray_get_n(arg1, i),
                      AddArray_get_n(arg2, i)));
  }

  return res;
}

add_ptr AddArray_make_disjunction(DDMgr_ptr dd, AddArray_ptr arg)
{
  add_ptr res;
  int i;

  nusmv_assert(AddArray_get_size(arg) > 0);

  res = add_false(dd);
  for (i=AddArray_get_size(arg)-1; i>=0; --i) {
    add_or_accumulate(dd, &res, AddArray_get_n(arg, i));
  }

  return res;
}

add_ptr AddArray_make_conjunction(DDMgr_ptr dd, AddArray_ptr arg)
{
  add_ptr res;
  int i;

  res = add_true(dd);
  for (i=AddArray_get_size(arg)-1; i>=0; --i) {
    add_and_accumulate(dd, &res, AddArray_get_n(arg, i));
  }

  return res;
}

AddArray_ptr AddArray_word_plus(DDMgr_ptr dd, AddArray_ptr arg1,
                                               AddArray_ptr arg2)
{
  AddArray_ptr res;
  add_ptr carry;
  add_ptr zero = add_false(dd);

  add_array_adder(dd, arg1, arg2, zero, &res, &carry);

  add_free(dd, zero);
  add_free(dd, carry);
  return res;
}

AddArray_ptr AddArray_word_minus(DDMgr_ptr dd, AddArray_ptr arg1,
                                                AddArray_ptr arg2)
{
  AddArray_ptr res;
  add_ptr carry;

  /* arg1 - arg2 ==  arg1 + (not arg2) + 1 */
  res = add_array_word_plus_negated_and_one(dd, arg1, arg2, &carry);

  add_free(dd, carry);
  return res;
}

AddArray_ptr AddArray_word_unary_minus(DDMgr_ptr dd, AddArray_ptr arg)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

  const int width = AddArray_get_size(arg);
  WordNumber_ptr wn = WordNumberMgr_integer_to_word_number(words, 0, width);

  AddArray_ptr zero = AddArray_from_word_number(dd, wn);

  AddArray_ptr res = AddArray_word_minus(dd, zero, arg);

  AddArray_destroy(dd, zero);

  return res;
}

AddArray_ptr AddArray_word_times(DDMgr_ptr dd,
                                 AddArray_ptr arg1, AddArray_ptr arg2)
{

  int index;
  const int N = AddArray_get_size(arg1);
  AddArray_ptr res = AddArray_create(N);

  nusmv_assert( N > 0 && N == AddArray_get_size(arg2));

  /* set the result array to (arg2 & arg1[0]), i.e. process first bit of arg1 */
  for (index = 0 ; index < N; ++index) {
    AddArray_set_n(res, index, add_and(dd, AddArray_get_n(arg1, 0),
                                       AddArray_get_n(arg2, index)));
  }

  /* A*B = ((B & A[0])<<0) +...+ ((B & A[i])<<i) +...+ ((B & A[N-1])<<N-1) */

  /* sum up all (arg2 & arg1[i]).
   Index begins with 1 because first bit has been already processed */
  for (index = 1; index < N; ++index) {
    int k;
    add_ptr carry_in;

    /* create an adder, i.e. (N - index) lower bits of (B & A[index])*/
    AddArray_ptr adder = AddArray_create(N - index);
    for (k = 0; k < N - index; ++ k){
      add_ptr add = add_and(dd, AddArray_get_n(arg1, index),
                            AddArray_get_n(arg2, k));
      AddArray_set_n(adder, k, add);
    } /* for k */

    /* add the adder to the (N - index) higher bits of the result */
    carry_in = add_false(dd); /* it will be de-referenced in the loop */

    for (k = 0; k < N - index; ++k) {
      add_ptr sum;
      add_ptr tmp_carry;

      add_array_full_adder(dd, AddArray_get_n(res, k + index),
                           AddArray_get_n(adder, k),
                           carry_in, &sum, &tmp_carry);
      add_free(dd, AddArray_get_n(res, k + index));
      AddArray_set_n(res, k + index, sum);
      add_free(dd, carry_in);
      carry_in = tmp_carry;
    } /* for k */

    AddArray_destroy(dd, adder);
  } /* for index */
  return res;
}

AddArray_ptr AddArray_word_unsigned_divide(DDMgr_ptr dd,
                                           AddArray_ptr arg1, AddArray_ptr arg2)
{
  AddArray_ptr quotient;
  AddArray_ptr remainder;

  add_array_unsigned_division_remainder(dd, arg1, arg2, &quotient, &remainder);

  AddArray_destroy(dd, remainder);
  return quotient;
}

AddArray_ptr AddArray_word_unsigned_mod(DDMgr_ptr dd,
                                        AddArray_ptr arg1, AddArray_ptr arg2)
{
  AddArray_ptr quotient;
  AddArray_ptr remainder;

  add_array_unsigned_division_remainder(dd, arg1, arg2, &quotient, &remainder);

  AddArray_destroy(dd, quotient);
  return remainder;
}

AddArray_ptr AddArray_word_signed_divide(DDMgr_ptr dd,
                                         AddArray_ptr arg1, AddArray_ptr arg2)
{
  AddArray_ptr quotient;
  AddArray_ptr remainder;

  /* proper benchmarking required to choose one of these functions */
#if 0
  add_array_signed_division_remainder_simple(dd, arg1, arg2, &quotient, &remainder);
#else
  add_array_signed_division_remainder_hardware(dd, arg1, arg2, &quotient, &remainder);
#endif

  AddArray_destroy(dd, remainder);
  return quotient;
}

AddArray_ptr AddArray_word_signed_mod(DDMgr_ptr dd,
                                      AddArray_ptr arg1, AddArray_ptr arg2)
{
  AddArray_ptr quotient;
  AddArray_ptr remainder;

#if 0
  add_array_signed_division_remainder_simple(dd, arg1, arg2, &quotient, &remainder);
#else
  add_array_signed_division_remainder_hardware(dd, arg1, arg2, &quotient, &remainder);
#endif

  AddArray_destroy(dd, quotient);
  return remainder;
}

AddArray_ptr AddArray_word_left_shift(DDMgr_ptr dd, AddArray_ptr arg,
                                      AddArray_ptr number)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

  const int width = AddArray_get_size(arg);
  const int numWidth = AddArray_get_size(number);
  boolean is_word = add_array_is_word(dd, number);
  AddArray_ptr res;
  add_ptr def_case;
  unsigned long long maxPossibleValue;
  int i;

  nusmv_assert(width>0 && numWidth>0);

  res = AddArray_create(width);

  /* creates the default case, i.e. the last ITE (see description) */
  {
    add_ptr zero = add_false(dd);
    def_case = add_array_create_default_value_of_shift_operation(dd,
                              number, width, zero,
                              "Right operand of left-shift is out of range");
    add_free(dd, zero);
  }

  /* NOTE: if numWidth >1 then 'number' is a word (unsigned).  Actually
     it can be a word of 1 bit but we cannot distinguish that and
     process it as a usual integer
  */

  /* find the highest value for number (word or int) */
  if (is_word) maxPossibleValue = ((2ULL << (numWidth-1)) - 1);
  else maxPossibleValue = ~0U; /* value for sure greater that width */

  /* proceed one bit at every iteration */
  for (i = 0; i < width; ++i) {
    add_ptr bit = add_dup(def_case);
    int n;

    /* create all other ITEs */
    for (n = MIN(i, maxPossibleValue); n >= 0; --n) {
      add_ptr numeqn_add, tmp;

      /* create  ITE(number=n, arg[i-n], ...) */
      if (is_word) {  /* number is a word */
        AddArray_ptr an = AddArray_from_word_number(dd,
                              WordNumberMgr_integer_to_word_number(words, n, numWidth));
        AddArray_ptr numeqn = AddArray_word_equal(dd, number, an);
        numeqn_add = add_dup(AddArray_get_add(numeqn));
        AddArray_destroy(dd, numeqn);
        AddArray_destroy(dd, an);
      }
      else { /* number is not a word.  */
        add_ptr n_add = add_leaf(dd, find_node(nodemgr, NUMBER, NODE_FROM_INT(n), Nil));
        numeqn_add = add_apply(dd, node_equal,
                               AddArray_get_add(number), n_add);
        add_free(dd, n_add);
      }

      tmp = add_ifthenelse(dd, numeqn_add, AddArray_get_n(arg, i - n),
                           bit);

      add_free(dd, numeqn_add);
      add_free(dd, bit);
      bit = tmp;
    } /* for (n) */

    /* set the obtained bit into the returned array */
    AddArray_set_n(res, i, bit);
  } /* for (i) */

  add_free(dd, def_case);

  return res;
}

AddArray_ptr AddArray_word_unsigned_right_shift(DDMgr_ptr dd, AddArray_ptr arg,
                                                AddArray_ptr number)
{
  return add_array_word_right_shift(dd, arg, number, false);
}

AddArray_ptr AddArray_word_signed_right_shift(DDMgr_ptr dd, AddArray_ptr arg,
                                                AddArray_ptr number)
{
  return add_array_word_right_shift(dd, arg, number, true);
}

AddArray_ptr AddArray_word_left_rotate(DDMgr_ptr dd, AddArray_ptr arg,
                                       AddArray_ptr number)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

  const int width = AddArray_get_size(arg);
  const int numWidth = AddArray_get_size(number);
  AddArray_ptr res;
  add_ptr err_case;
  boolean is_word = add_array_is_word(dd, number);
  unsigned long long maxPossibleValue;
  int i;

  nusmv_assert(width > 0);

  res = AddArray_create(width);

  /* create the default case, i.e. the last ITE (see description) */
  err_case = add_leaf(dd,
               ErrorMgr_failure_make(errmgr, "Right operand of rotate operation is out of range",
                            FAILURE_UNSPECIFIED,
                            (int) node_get_lineno(ErrorMgr_get_the_node(errmgr))));

  if (is_word) maxPossibleValue = MIN(((2ULL << (numWidth-1)) - 1), width);
  else maxPossibleValue = width;

  /* proceed one bit at every iteration */
  for (i = 0; i < width; ++i) {
    int k;
    add_ptr bit = add_dup(err_case); /* it is de-ref in the loop */

    /* create all other ITEs */
    for (k=maxPossibleValue ; k >= 0; --k) {
      add_ptr neqk, tmp;

      if (is_word) {
        AddArray_ptr ak = AddArray_from_word_number(dd,
                              WordNumberMgr_integer_to_word_number(words, k, numWidth));
        AddArray_ptr aneqk = AddArray_word_equal(dd, number, ak);
        neqk = add_dup(AddArray_get_add(aneqk));
        AddArray_destroy(dd, aneqk);
        AddArray_destroy(dd, ak);
      }
      else {
        add_ptr k_add = add_leaf(dd, find_node(nodemgr, NUMBER, NODE_FROM_INT(k), Nil));
        neqk = add_apply(dd, node_equal, AddArray_get_add(number), k_add);
        add_free(dd, k_add);
      }

      tmp = add_ifthenelse(dd, neqk,
                           AddArray_get_n(arg,
                                          (i >= k) ? i-k : i-k+width),
                           bit);

      add_free(dd, neqk);
      add_free(dd, bit);
      bit = tmp;
    } /* for (k) */

    /* set the obtained bit into the returned array */
    AddArray_set_n(res, i, bit);
  } /* for (i) */

  add_free(dd, err_case);

  return res;
}

AddArray_ptr AddArray_word_right_rotate(DDMgr_ptr dd, AddArray_ptr arg,
                                        AddArray_ptr number)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));


  const int width = AddArray_get_size(arg);
  const int numWidth = AddArray_get_size(number);
  AddArray_ptr res;
  add_ptr err_case;
  boolean is_word = add_array_is_word(dd, number);
  unsigned long long maxPossibleValue;
  int i;

  nusmv_assert(width > 0);

  res = AddArray_create(width);

  /* create the default case, i.e. the last ITE (see description) */
  err_case = add_leaf(dd,
               ErrorMgr_failure_make(errmgr, "Right operand of rotate operation is out of range",
                            FAILURE_UNSPECIFIED,
                            (int) node_get_lineno(ErrorMgr_get_the_node(errmgr))));

  if (is_word) maxPossibleValue = MIN(((2ULL << (numWidth-1)) - 1), width);
  else maxPossibleValue = width;

  /* proceed one bit at every iteration */
  for (i = 0; i < width; ++i) {
    int k;
    add_ptr bit = add_dup(err_case); /* it is de-ref in the loop */

    /* create all other ITEs */
    for (k=maxPossibleValue ; k >= 0; --k) {
      add_ptr neqk, tmp;

      if (is_word) {
        AddArray_ptr ak = AddArray_from_word_number(dd,
                              WordNumberMgr_integer_to_word_number(words, k, numWidth));
        AddArray_ptr aneqk = AddArray_word_equal(dd, number, ak);
        neqk = add_dup(AddArray_get_add(aneqk));
        AddArray_destroy(dd, aneqk);
        AddArray_destroy(dd, ak);
      }
      else {
        add_ptr k_add = add_leaf(dd, find_node(nodemgr, NUMBER, NODE_FROM_INT(k), Nil));
        neqk = add_apply(dd, node_equal, AddArray_get_add(number), k_add);
        add_free(dd, k_add);
      }

      tmp = add_ifthenelse(dd, neqk,
                           AddArray_get_n(arg, ((i+k) % width)),
                           bit);

      add_free(dd, neqk);
      add_free(dd, bit);
      bit = tmp;
    } /* for (k) */

    /* set the obtained bit into the returned array */
    AddArray_set_n(res, i, bit);
  } /* for (i) */

  add_free(dd, err_case);

  return res;
}

AddArray_ptr AddArray_word_unsigned_less(DDMgr_ptr dd,
                                         AddArray_ptr arg1,
                                         AddArray_ptr arg2)
{
  /* A < B
     === NOT (A >= B)
     === NOT ("carry bit of" (B - A - 1))
     === NOT NOT ("carry bit of" (B + 11...111 + 1 - A - 1))
     === "carry bit of" ((not A) + B)
  */

  AddArray_ptr not_arg1 = add_array_negate_bits(dd, arg1);
  add_ptr zero = add_false(dd);

  AddArray_ptr plus;
  add_ptr carry;

  add_array_adder(dd, not_arg1, arg2, zero, &plus, &carry);

  AddArray_destroy(dd, plus);
  add_free(dd, zero);
  AddArray_destroy(dd, not_arg1);

  return AddArray_from_add(carry);

  /* For developers:  the same operation could have been implemented as
     A < B
     === "carry bit of" A - B
     === NOT "carry bit of" A + (NOT B) + 1
     i.e. !carry of add_array_word_plus_negated_and_one(arg1,arg2,carry)
     But above implementation looks slightly more efficient
  */
}

AddArray_ptr AddArray_word_unsigned_less_equal(DDMgr_ptr dd,
                                               AddArray_ptr arg1,
                                               AddArray_ptr arg2)
{
  /* A <= B ===
     A < (B + 1) ==
     === "carry bit of" ((not A) + (B + 1))    (see  AddArray_word_less)

     A <= B
     === NOT (A > B)
     === NOT "carry bit of" B - A
     === NOT NOT "carry bit of" B - A + 111..11 + 1
     === "carry bit of" B + !A + 1
  */
  add_ptr carry;
  AddArray_ptr tmp = add_array_word_plus_negated_and_one(dd, arg2, arg1, &carry);
  AddArray_destroy(dd, tmp);

  return AddArray_from_add(carry);
}

AddArray_ptr AddArray_word_unsigned_greater(DDMgr_ptr dd,
                                           AddArray_ptr arg1,
                                           AddArray_ptr arg2)
{
  return AddArray_word_unsigned_less(dd, arg2, arg1);
}

AddArray_ptr AddArray_word_unsigned_greater_equal(DDMgr_ptr dd,
                                                 AddArray_ptr arg1,
                                                 AddArray_ptr arg2)
{
    return AddArray_word_unsigned_less_equal(dd, arg2, arg1);
}

AddArray_ptr AddArray_word_signed_less(DDMgr_ptr dd,
                                       AddArray_ptr arg1,
                                       AddArray_ptr arg2)
{
  return add_array_word_signed_comparison(dd, AddArray_word_unsigned_less,
                                          arg1, arg2);
}

AddArray_ptr AddArray_word_signed_less_equal(DDMgr_ptr dd,
                                             AddArray_ptr arg1,
                                             AddArray_ptr arg2)
{
  return add_array_word_signed_comparison(dd, AddArray_word_unsigned_less_equal,
                                          arg1, arg2);
}

AddArray_ptr AddArray_word_signed_greater(DDMgr_ptr dd,
                                          AddArray_ptr arg1, AddArray_ptr arg2)
{
  /* A >s B === B <s A */
  return add_array_word_signed_comparison(dd, AddArray_word_unsigned_less,
                                          arg2, arg1);
}

AddArray_ptr AddArray_word_signed_greater_equal(DDMgr_ptr dd,
                                                AddArray_ptr arg1,
                                                AddArray_ptr arg2)
{
  /* A >=s B === B <=s A */
  return add_array_word_signed_comparison(dd, AddArray_word_unsigned_less_equal,
                                          arg2, arg1);
}

AddArray_ptr AddArray_word_signed_extend(DDMgr_ptr dd,
                                       AddArray_ptr arg,
                                       AddArray_ptr arg_repeat)
{
  add_ptr paddingBit = AddArray_get_n(arg, AddArray_get_size(arg) - 1);
  AddArray_ptr res = add_array_word_extend(dd, arg, arg_repeat, paddingBit);
  return res;
}

AddArray_ptr AddArray_word_unsigned_extend(DDMgr_ptr dd,
                                           AddArray_ptr arg,
                                           AddArray_ptr arg_repeat)
{
  add_ptr paddingBit = add_false(dd);
  AddArray_ptr res = add_array_word_extend(dd, arg, arg_repeat, paddingBit);
  add_free(dd, paddingBit);
  return res;
}

AddArray_ptr AddArray_word_equal(DDMgr_ptr dd,
                                 AddArray_ptr arg1, AddArray_ptr arg2)
{
  add_ptr result = add_true(dd);
  int i;
  const int width = AddArray_get_size(arg1);

  nusmv_assert(width == AddArray_get_size(arg2) && width > 0);

  for (i = 0; i < width; ++i) {
    add_ptr bit_equal = add_iff(dd, AddArray_get_n(arg1, i),
                                AddArray_get_n(arg2, i));
    add_ptr tmp = add_and(dd, bit_equal, result);
    add_free(dd, bit_equal);
    add_free(dd, result);
    result = tmp;

    if (add_is_false(dd, result)) break; /* lazy eval */
  }

  return AddArray_from_add(result);
}

AddArray_ptr AddArray_word_not_equal(DDMgr_ptr dd,
                                     AddArray_ptr arg1, AddArray_ptr arg2)
{
  add_ptr result = add_false(dd);
  int i;
  const int width = AddArray_get_size(arg1);

  nusmv_assert(width == AddArray_get_size(arg2) && width > 0);

  for (i = 0; i < width; ++i) {
    add_ptr bit_not_equal = add_xor(dd, AddArray_get_n(arg1, i),
                                    AddArray_get_n(arg2, i));
    add_ptr tmp = add_or(dd, bit_not_equal, result);
    add_free(dd, bit_not_equal);
    add_free(dd, result);
    result = tmp;
  }

  return AddArray_from_add(result);
}

AddArray_ptr AddArray_word_ite(DDMgr_ptr dd,
                               AddArray_ptr _if,
                               AddArray_ptr _then, AddArray_ptr _else)
{
  int width, i;
  AddArray_ptr res;
  boolean is_else_failure;
  add_ptr if_add;

  width = AddArray_get_size(_then);

  /* flags else if size is 1 (may be a failure node when width > 1) */
  is_else_failure = AddArray_get_size(_else) == 1;
  nusmv_assert(is_else_failure || width == AddArray_get_size(_else));

  /* prepares if_add */
  if_add = AddArray_get_add(_if);

  res = AddArray_create(width);
  /* process every pair of bits */
  for (i = 0; i < width; ++i) {
    add_ptr add = add_ifthenelse(dd,
                                 if_add,
                                 AddArray_get_n(_then, i),
                                 is_else_failure ? AddArray_get_add(_else)
                                 : AddArray_get_n(_else, i));
    /* if ELSE is a failure node then only one ADD will be there */
    AddArray_set_n(res, i, add);
  }

  return res;
}

AddArray_ptr AddArray_word_bit_selection(DDMgr_ptr dd,
                                         AddArray_ptr word,
                                         AddArray_ptr range)
{
  const int width = AddArray_get_size(word);
  int i;
  int highBit;
  int lowBit;
  AddArray_ptr res;

  /* convert the ADD representation of high and low bits into integers */
  {
    add_ptr range_add = AddArray_get_add(range);/*one ADD may be in range*/
    node_ptr range_node;

    nusmv_assert(add_isleaf(range_add));/* range must be a value, not ADD tree */

    range_node = add_get_leaf(dd, range_add);
    nusmv_assert(node_get_type(range_node) == RANGE &&
                 node_get_type(car(range_node)) == NUMBER &&
                 node_get_type(cdr(range_node)) == NUMBER);

    highBit = NODE_TO_INT(car(car(range_node)));
    lowBit = NODE_TO_INT(car(cdr(range_node)));

    nusmv_assert(0 <= lowBit && lowBit <= highBit && highBit < width);
  }

  i = highBit - lowBit + 1;
  res = AddArray_create(i);
  for ( --i; i >= 0; --i){
    add_ptr add = AddArray_get_n(word, i + lowBit);
    add_ref(add);
    AddArray_set_n(res, i, add);
  }
  return res;
}

AddArray_ptr AddArray_word_signed_resize(DDMgr_ptr dd,
                                         AddArray_ptr word,
                                         AddArray_ptr new_size)
{
  const int width = AddArray_get_size(word);
  int i, new_width;

  /* convert the ADD representation of size into integer */
  {
    add_ptr size_add = AddArray_get_add(new_size);/*one ADD may be in size*/
    node_ptr size_node;

    nusmv_assert(add_isleaf(size_add));/* range must be a value, not ADD tree */

    size_node = add_get_leaf(dd, size_add);
    nusmv_assert(NUMBER == node_get_type(size_node));

    new_width = node_get_int(size_node);
    nusmv_assert(0 < new_width);
  }

  if (width == new_width) { return AddArray_duplicate(word); }

  {
    AddArray_ptr res = AddArray_create(new_width);

    if (width < new_width) { /* signed extension */
      /* copies the argument */
      i = 0;
      for (; i < width; ++i) {
        AddArray_set_n(res, i, add_dup(AddArray_get_n(word, i)));
      }

      /* extends sign bit */
      for (;i < new_width; ++i) {
        AddArray_set_n(res, i,
                       add_dup(AddArray_get_n(word, width - 1)));
      }
    }
    else {
      /* copy bits in the range (N-2:0) */
      i = 0;
      for (; i < new_width-1; ++i){
        AddArray_set_n(res, i, add_dup(AddArray_get_n(word, i)));
      }

      /* preserve sign bit */
      AddArray_set_n(res, new_width-1,
                     add_dup(AddArray_get_n(word, width - 1)));
    }

    return res;
  }
}

AddArray_ptr AddArray_word_unsigned_resize(DDMgr_ptr dd,
                                           AddArray_ptr word,
                                           AddArray_ptr new_size)
{
  const int width = AddArray_get_size(word);
  int i, new_width;

  /* convert the ADD representation of high and low bits into integers */
  {
    add_ptr size_add = AddArray_get_add(new_size);/*one ADD may be in size*/
    node_ptr size_node;

    nusmv_assert(add_isleaf(size_add));/* range must be a value, not ADD tree */

    size_node = add_get_leaf(dd, size_add);
    nusmv_assert(NUMBER == node_get_type(size_node));

    new_width = node_get_int(size_node);
    nusmv_assert(0 < new_width);
  }

  if (width == new_width) { return AddArray_duplicate(word); }

  {
    AddArray_ptr res = AddArray_create(new_width);

    if (width < new_width) { /* unsigned extension */

      /* copies the argument */
      i = 0;
      for (; i < width; ++i) {
        AddArray_set_n(res, i, add_dup(AddArray_get_n(word, i)));
      }

      /* pads with zeroes */
      for (;i < new_width; ++i) { AddArray_set_n(res, i, add_false(dd)); }
    }

    else { /* bit-selection */
      /* copy bits in range [N-1:0] */
      i = 0;
      for (; i < new_width; ++i){
        AddArray_set_n(res, i, add_dup(AddArray_get_n(word, i)));
      }
    }

    return res;
  }
}

AddArray_ptr AddArray_word_concatenation(DDMgr_ptr dd,
                                         AddArray_ptr arg1, AddArray_ptr arg2)
{
  const int width1 = AddArray_get_size(arg1);
  const int width2 = AddArray_get_size(arg2);
  int i;

  AddArray_ptr res = AddArray_create(width1 + width2);

  for (i = 0; i < width2; ++i) {
    add_ptr add = AddArray_get_n(arg2, i);
    add_ref(add);
    AddArray_set_n(res, i, add);
  }

  for (i = 0; i < width1; ++i) {
    add_ptr add = AddArray_get_n(arg1, i);
    add_ref(add);
    AddArray_set_n(res, i + width2, add);
  }
  return res;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Performes the full-adder operation on the arguments arg1, arg2
  and carry_in. The returned sum-bit and carry-bit are returned in
  sum and carry_out.

  The returned ADD (sum and carry_out) are referenced.
*/
static void add_array_full_adder(DDMgr_ptr dd,
                                 add_ptr arg1,
                                 add_ptr arg2,
                                 add_ptr carry_in,
                                 add_ptr* sum,
                                 add_ptr* carry_out)
{
  add_ptr arg1_xor_arg2;
  add_ptr arg1_and_arg2;
  add_ptr arg1_xor_arg2_and_carry_in;

  nusmv_assert( sum != (add_ptr*)NULL && carry_out != (add_ptr*)NULL);

  /* sum = arg1 XOR arg2 XOR carry_in */
  arg1_xor_arg2 = add_xor(dd, arg1, arg2);
  *sum = add_xor(dd, arg1_xor_arg2, carry_in);

  /* curry_out = (arg1 and arg2) OR ((arg1 XOR arg2) AND carry_in) */
  arg1_and_arg2 = add_and(dd, arg1, arg2);
  arg1_xor_arg2_and_carry_in = add_and(dd, arg1_xor_arg2, carry_in);
  *carry_out = add_or(dd, arg1_and_arg2, arg1_xor_arg2_and_carry_in);

  add_free(dd, arg1_xor_arg2);
  add_free(dd, arg1_and_arg2);
  add_free(dd, arg1_xor_arg2_and_carry_in);
  return;
}

/*!
  \brief Performes the addition of two Word expressions and
  a carry-in bit.

  The sum is returned by the parameter res (the invoker
  must destroy this array), and the final carry-bit is returned by the
  parameter carry_out (the ADD is referenced).
  The size of input arrays must be equal(and positive).
*/
static void add_array_adder(DDMgr_ptr dd, AddArray_ptr arg1, AddArray_ptr arg2,
                            add_ptr carry_in,
                            AddArray_ptr* res,  add_ptr* carry_out)
{
  const int width = AddArray_get_size(arg1);
  int i;

  nusmv_assert( res != (AddArray_ptr*)NULL && carry_out != (add_ptr*)NULL);
  nusmv_assert(AddArray_get_size(arg2) == width && width > 0);


  *res = AddArray_create(width);
  add_ref(carry_in);/* it will be de-referenced in the loop or in an outter fun */

  for (i = 0; i < width; ++i) {
    add_ptr sum;
    add_ptr tmp_carry;

    add_array_full_adder(dd, AddArray_get_n(arg1, i), AddArray_get_n(arg2, i),
                         carry_in, &sum, &tmp_carry);
    AddArray_set_n(*res, i, sum);
    add_free(dd, carry_in);
    carry_in = tmp_carry;
  } /* for */

  *carry_out = carry_in;
  return;
}


/*!
  \brief Perform "arg1 + (not arg2) + 1"
  of two Word expressions represented as an array of ADD.
  This operation correspond to the subtraction operations.


  The size of both arguments should be the same.
  'carry' must be not zero and is used to return carry bit of
  performed operation.
  Note the overflow or underflow can be detected by checking (not
  carry-bit).
*/

AddArray_ptr add_array_word_plus_negated_and_one(DDMgr_ptr dd,
                                                 AddArray_ptr arg1,
                                                 AddArray_ptr arg2,
                                                 add_ptr* carry)
{
  AddArray_ptr res;

  AddArray_ptr not_arg2;
  add_ptr one = add_true(dd);

  not_arg2 = add_array_negate_bits(dd, arg2);

  add_array_adder(dd, arg1, not_arg2, one, &res, carry);

  add_free(dd, one);
  AddArray_destroy(dd, not_arg2);
  return res;
}

/*!
  \brief Performs division operations on two unsigned Word expressions
  (which are encoded as arrays of bits)

  The quotient and the remainder is returned in the
  parameters "quotient" and "remainder" respectively.
  The invoker should free the returned arrays.

  The size of arguments should be the same (and positive).

  Every bit of the resulting arrays is wrapped in ITE which check
  the second argument (of the operation) for not being zero.

  \sa add_array_signed_division_remainder
*/
static void add_array_unsigned_division_remainder(DDMgr_ptr dd,
                                                  AddArray_ptr arg1,
                                                  AddArray_ptr arg2,
                                                  AddArray_ptr* quotient,
                                                  AddArray_ptr* remainder)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  int index;
  const int N = AddArray_get_size(arg1);
  AddArray_ptr quot;
  AddArray_ptr rem;

  nusmv_assert(N > 0 && AddArray_get_size(arg2) == N);

  /* create remainder and quotient and set the remainder to the zero value */
  quot = AddArray_create(N);
  rem = AddArray_create(N);
  for (index = 0; index < N; ++index) {
    AddArray_set_n(rem, index, add_false(dd));
  }

  /* calculate the division operation */
  for (index = N - 1; index >= 0; --index) {
    add_ptr isDividable;

    /* shift remainder left by 1 bit and add the bit arg1[index] on the left */
    {
      int k;
      add_ptr tmp;
      add_free(dd, AddArray_get_n(rem, N-1));
      for (k = N - 1; k > 0; --k) {
        AddArray_set_n(rem, k, AddArray_get_n(rem, k - 1));
      }
      tmp = AddArray_get_n(arg1, index);
      add_ref(tmp);
      AddArray_set_n(rem, 0, tmp);
    }

    /* calculate ADD for (remainder >= arg2), i.e. flag that
       subtraction can be performed. Set the quotinet[index] to this value.
    */
    {
      AddArray_ptr tmp = AddArray_word_unsigned_greater_equal(dd, rem, arg2);
      isDividable = AddArray_get_add(tmp);
      add_ref(isDividable);
      AddArray_destroy(dd, tmp);
      AddArray_set_n(quot, index, isDividable);
    }

    /* set remainder to (remainder - (arg2 AND (remainder >= arg2))) */
    {
      AddArray_ptr diff;
      AddArray_ptr guardedArg = AddArray_create(N);
      int k;

      for (k = 0; k < N; ++k) {
        add_ptr add = add_and(dd, AddArray_get_n(arg2, k), isDividable);
        AddArray_set_n(guardedArg, k, add);
      }

      diff = AddArray_word_minus(dd, rem, guardedArg);

      AddArray_destroy(dd, rem);
      AddArray_destroy(dd, guardedArg);
      rem = diff;
    }
  } /* for index */

  /* Calculation of the quotient and the remainder is finished.
     Now guard every bit of quotient and remainder by the condition that
     the divisor is not equal to zero
  */
  {
    add_ptr isNotZero;
    add_ptr error;

    /* Check the second argument for being NOT zero */
    for (isNotZero = add_false(dd), index = 0; index < N; ++index) {
      add_or_accumulate(dd, &isNotZero, AddArray_get_n(arg2, index));
    }


    /* create an error message */
    error = add_leaf(dd,
                     ErrorMgr_failure_make(errmgr, "Division by zero",
                                           FAILURE_DIV_BY_ZERO,
                                           (int) node_get_lineno(ErrorMgr_get_the_node(errmgr))));

    for (index = 0; index < N; ++index) {
      add_ptr guarded;
      /* quotient */
      guarded = add_ifthenelse(dd, isNotZero, AddArray_get_n(quot, index),
                               error);
      add_free(dd, AddArray_get_n(quot, index));
      AddArray_set_n(quot, index, guarded);
      /* remainder */
      guarded = add_ifthenelse(dd, isNotZero, AddArray_get_n(rem, index),
                               error);
      add_free(dd, AddArray_get_n(rem, index));
      AddArray_set_n(rem, index, guarded);
    }

    add_free(dd, error);
    add_free(dd, isNotZero);
  }

  *remainder = rem;
  *quotient = quot;
  return;
}

/*!
  \brief Performs division operations on two unsigned Word expressions
  (which are encoded as arrays of bits)

  The quotient and the remainder is returned in the
  parameters "quotient" and "remainder" respectively.
  The invoker should free the returned arrays.

  The size of arguments should be the same (and positive).

  Every bit of the resulting arrays is wrapped in ITE which check
  the second argument (of the operation) for not being zero.

  NOTE FOR DEVELOPER: the provided functionality was implemented in
  two forms: as function add_array_signed_division_remainder_simple
  (which is the simplest) and as function
  add_array_signed_division_remainder_harware (which resembles the
  hardware implemenation of signed division). Preliminary benchmarking
  showed that add_array_signed_division_remainder_harware runs quicker
  (so it is used now).  Proper benchmarks are still needed to choose
  one and remove the other one.

  \sa add_array_unsigned_division_remainder,
  add_array_signed_division_remainder_harware
*/
static void add_array_signed_division_remainder_simple(DDMgr_ptr dd,
                                                       AddArray_ptr arg1,
                                                       AddArray_ptr arg2,
                                                       AddArray_ptr* quotient,
                                                       AddArray_ptr* remainder)
{
  int index;
  const int N = AddArray_get_size(arg1);
  AddArray_ptr quot;
  AddArray_ptr rem;
  AddArray_ptr arg1_positive, arg2_positive;
  add_ptr sign1;
  add_ptr sign2;

  nusmv_assert(N > 0 && AddArray_get_size(arg2) == N);

  sign1 = AddArray_get_n(arg1, N-1);
  sign2 = AddArray_get_n(arg2, N-1);

  /* created negated arguments */
  arg1_positive = AddArray_word_unary_minus(dd, arg1);
  arg2_positive = AddArray_word_unary_minus(dd, arg2);

  /* now obtain for sure not-negative arg1 and arg2 */
  for (index = 0; index < N; ++index) {
    add_ptr bitPositive, new;

    bitPositive = AddArray_get_n(arg1_positive, index);
    new = add_ifthenelse(dd, sign1, bitPositive, AddArray_get_n(arg1, index));
    AddArray_set_n(arg1_positive, index, new);
    add_free(dd, bitPositive);

    bitPositive = AddArray_get_n(arg2_positive, index);
    new = add_ifthenelse(dd, sign2, bitPositive, AddArray_get_n(arg2, index));
    AddArray_set_n(arg2_positive, index, new);
    add_free(dd, bitPositive);
  }

  add_array_unsigned_division_remainder(dd,
                                        arg1_positive,
                                        arg2_positive,
                                        &quot,
                                        &rem);

  AddArray_destroy(dd, arg1_positive);
  AddArray_destroy(dd, arg2_positive);

  /* negate the remainder if the dividend was negative */
  {
    AddArray_ptr rem_neg = AddArray_word_unary_minus(dd, rem);
    for (index = 0; index < N; ++index) {
      add_ptr old, new;

      old = AddArray_get_n(rem, index);
      new = add_ifthenelse(dd, sign1, AddArray_get_n(rem_neg, index), old);
      AddArray_set_n(rem, index, new);
      add_free(dd, old);
    }
    AddArray_destroy(dd, rem_neg);
  }

  /* negate the quotient if the dividend and divisor have different signs */
  {
    add_ptr diff_signs = add_xor(dd, sign1, sign2);
    AddArray_ptr quot_neg = AddArray_word_unary_minus(dd, quot);
    for (index = 0; index < N; ++index) {
      add_ptr old, new;

      old = AddArray_get_n(quot, index);
      new = add_ifthenelse(dd, diff_signs, AddArray_get_n(quot_neg, index), old);
      AddArray_set_n(quot, index, new);
      add_free(dd, old);
    }
    AddArray_destroy(dd, quot_neg);
    add_free(dd, diff_signs);
  }

  *remainder = rem;
  *quotient = quot;

  return;
}

/*!
  \brief Performs division operations on two unsigned Word expressions
  (which are encoded as arrays of bits)

  The quotient and the remainder is returned in the
  parameters "quotient" and "remainder" respectively.
  The invoker should free the returned arrays.

  The size of arguments should be the same (and positive).

  Every bit of the resulting arrays is wrapped in ITE which check
  the second argument (of the operation) for not being zero.

  NOTE FOR DEVELOPER: the provided functionality was implemented in
  two forms: as function add_array_signed_division_remainder_simple
  (which is the simplest) and as function
  add_array_signed_division_remainder_harware (which resembles the
  hardware implemenation of signed division). Preliminary benchmarking
  showed that add_array_signed_division_remainder_harware runs quicker
  (so it is used now).  Proper benchmarks are still needed to choose
  one and remove the other one.

  \sa add_array_signed_division_remainder_simple
*/
static void add_array_signed_division_remainder_hardware(DDMgr_ptr dd,
                                                         AddArray_ptr arg1,
                                                         AddArray_ptr arg2,
                                                         AddArray_ptr* quotient,
                                                         AddArray_ptr* remainder)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  int index;
  const int N = AddArray_get_size(arg1);
  AddArray_ptr quot;
  AddArray_ptr rem;

  nusmv_assert(N > 0 && AddArray_get_size(arg2) == N);

  /* create remainder and quotient. Set the remainder bits to the sign bit
     of the arg1 (the dividend) */
  quot = AddArray_create(N);
  rem = AddArray_create(N);
  for (index = 0; index < N; ++index) {
    AddArray_set_n(rem, index, add_dup(AddArray_get_n(arg1, N-1)));
  }

  /* calculate the division operation */
  for (index = N - 1; index >= 0; --index) {
    AddArray_ptr rem_next;
    add_ptr isDividable;

    /* shift remainder left by 1 bit and add the bit arg1[index] on the left */
    {
      int k;
      add_ptr tmp;
      add_free(dd, AddArray_get_n(rem, N-1));
      for (k = N - 1; k > 0; --k) {
        AddArray_set_n(rem, k, AddArray_get_n(rem, k - 1));
      }
      tmp = AddArray_get_n(arg1, index);
      AddArray_set_n(rem, 0, add_dup(tmp));
    }

    /* calculate next (possible) reminder rem_next:
       ITE(sign1 == sign2, rem - arg2, rem + arg2).
       I.e. if current remainder (arg1) and arg2 are of differenct signs
       then addition is used,
       otherwise subtraction is used the same way as in unsigned division.
    */

    {
      AddArray_ptr plus = AddArray_word_plus(dd, rem, arg2);
      AddArray_ptr minus = AddArray_word_minus(dd, rem, arg2);
      add_ptr areSignsEqual = add_iff(dd, AddArray_get_n(rem, N-1),
                                      AddArray_get_n(arg2, N-1));
      int k;
      rem_next = AddArray_create(N);
      for (k = 0; k < N; ++k) {
        AddArray_set_n(rem_next, k,
                       add_ifthenelse(dd, areSignsEqual,
                                      AddArray_get_n(minus, k),
                                      AddArray_get_n(plus, k)));
      }

      add_free(dd, areSignsEqual);
      AddArray_destroy(dd, minus);
      AddArray_destroy(dd, plus);
    }

    /*
       Then the flag isDividable is calulated indicating whether
       division can be performed at his step or not.
       The condition is the following:
       1) if the sign of previous remainder and next (possible) remainder
          is the same then division can be performe.
       2) if signs are different but next-remainder=0 and all the remaining
       numbers in arg1 are 0.
    */
    {
      add_ptr isNotZero;
      int k;
      add_ptr isZero;

      isDividable = add_iff(dd,
                            AddArray_get_n(rem, N-1),
                            AddArray_get_n(rem_next, N-1));

      isNotZero = AddArray_make_disjunction(dd, rem_next);

      //for (k = N-1; k > index; --k) {
      //  add_or_accumulate(dd, &isNotZero, AddArray_get_n(quot, k));
      //}
      for (k = index-1; k >= 0; --k) {
        add_or_accumulate(dd, &isNotZero, AddArray_get_n(arg1, k));
      }

      isZero = add_not(dd, isNotZero);
      add_free(dd, isNotZero);

      add_or_accumulate(dd, &isDividable, isZero);
      add_free(dd, isZero);
    }

    /* Set the quotinet[index] to the flag isDividable computed above.
       Set remainder to its next value if division is possible (isDividable is on)
       and restore the remainder otherwise, i.e.
       remainder = ITE(isDividable, next-remainder, remainder)
    */
    {
      int k;
      AddArray_set_n(quot, index, isDividable); /* do not free isDividable */

      for (k = 0; k < N; ++k) {
        add_ptr old_bit = AddArray_get_n(rem, k);
        add_ptr new_bit = add_ifthenelse(dd, isDividable,
                                         AddArray_get_n(rem_next, k),
                                         old_bit);
        add_free(dd, old_bit);
        AddArray_set_n(rem, k, new_bit);
      }
      AddArray_destroy(dd, rem_next);
    }
  } /* for index */

  /* Negate the quotient if the sign of the dividend (arg1) and divisor (arg2)
     disagrees.
  */
  {
    AddArray_ptr quot_negated  = AddArray_word_unary_minus(dd, quot);
    add_ptr areSignsEqual = add_iff(dd, AddArray_get_n(arg1, N-1),
                                    AddArray_get_n(arg2, N-1));
    int k;
    for (k = 0; k < N; ++k) {
      add_ptr old_bit = AddArray_get_n(quot, k);
      add_ptr new_bit = add_ifthenelse(dd, areSignsEqual,
                                       old_bit,
                                       AddArray_get_n(quot_negated, k));
      add_free(dd, old_bit);
      AddArray_set_n(quot, k, new_bit);
    }
    add_free(dd, areSignsEqual);
    AddArray_destroy(dd, quot_negated);
  }

  /* Calculation of the quotient and the remainder is finished.
     Now guard every bit of quotient and remainder by the condition that
     the divisor is not equal to zero
  */
  {
    /* Check the second argument for being NOT zero */
    add_ptr isNotZero = AddArray_make_disjunction(dd, arg2);

    /* create an error message */
    add_ptr error = add_leaf(dd,
                     ErrorMgr_failure_make(errmgr, "Division by zero",
                                  FAILURE_DIV_BY_ZERO,
                                  (int) node_get_lineno(ErrorMgr_get_the_node(errmgr))));

    for (index = 0; index < N; ++index) {
      add_ptr guarded;
      /* quotient */
      guarded = add_ifthenelse(dd, isNotZero, AddArray_get_n(quot, index),
                               error);
      add_free(dd, AddArray_get_n(quot, index));
      AddArray_set_n(quot, index, guarded);
      /* remainder */
      guarded = add_ifthenelse(dd, isNotZero, AddArray_get_n(rem, index),
                               error);
      add_free(dd, AddArray_get_n(rem, index));
      AddArray_set_n(rem, index, guarded);
    }

    add_free(dd, error);
    add_free(dd, isNotZero);
  }

  *remainder = rem;
  *quotient = quot;

  return;
}

/*!
  \brief Create an ADD of the default case in shift operations, i.e. ADD of
  ITE(nubmer >=0 && number <= width, zero, FAILURE).


  This function is used in shift operations.
  See, for example, AddArray_word_left_shift.

  The 'number' is ADD of the number of bit the Word is shifted.
  'width' is the width of the given Word expression.
  'defaultBit' is a bit which pads the shifted bit.
  'errMessage' is the error message to print if number is out of range,
  for example, "Right operand of left-shift is out of range".

  NB: The returned ADD is referenced.
*/
static add_ptr
add_array_create_default_value_of_shift_operation(DDMgr_ptr dd,
                                                  AddArray_ptr number,
                                                  int width,
                                                  add_ptr defaultBit,
                                                  const char* errMessage)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

  int numWidth = AddArray_get_size(number);
  add_ptr res = (add_ptr) NULL;
  boolean is_word = add_array_is_word(dd, number);
  add_ptr err_val = add_leaf(dd,
                        ErrorMgr_failure_make(errmgr, errMessage, FAILURE_UNSPECIFIED,
                        (int) node_get_lineno(ErrorMgr_get_the_node(errmgr))));

  if (is_word) { /* 'number' is a word. Words (unsigned) are always >= 0 */
    if ((width >= (2ULL<<(numWidth-1)) - 1)) { /* width >= (2^numWidth - 1). */
      /* 'number' cannot represent values > width. Thus
         a check that number<=width is not required. */
      res = add_dup(defaultBit);
    }
    else {
      const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
      const WordNumberMgr_ptr words =
        WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

      AddArray_ptr maxValid = AddArray_from_word_number(dd,
                                              WordNumberMgr_integer_to_word_number(words, width, numWidth));
      AddArray_ptr nlemax = AddArray_word_unsigned_less_equal(dd, number, maxValid);
      add_ptr cond = AddArray_get_add(nlemax); /* only one add can be there */
      res = add_ifthenelse(dd, cond, defaultBit, err_val);

      AddArray_destroy(dd, nlemax);
      AddArray_destroy(dd, maxValid);
    }
  }
  else { /* 'number' is not a word.
            Actually 'number' sill may be a word of width 2 but we cannot
            detect that here. So process it as a usual integer */
    add_ptr zero, aw, nge0, nlew, nge0_lew;
    add_ptr add_number = (add_ptr)NULL;

    add_number = AddArray_get_add(number);

    zero = add_leaf(dd, find_node(nodemgr, NUMBER, NODE_FROM_INT(0), Nil));
    aw = add_leaf(dd, find_node(nodemgr, NUMBER, NODE_FROM_INT(width), Nil));
    nge0 = add_apply(dd, node_ge, add_number, zero);
    nlew = add_apply(dd, node_le, add_number, aw);
    nge0_lew = add_and(dd, nge0, nlew);
    res = add_ifthenelse(dd, nge0_lew, defaultBit, err_val);

    add_free(dd, nge0_lew);
    add_free(dd, nlew);
    add_free(dd, nge0);
    add_free(dd, aw);
    add_free(dd, zero);
  }

  add_free(dd, err_val);

  return res;
}

/*!
  \brief A function takes an array and negates every bit of it

  the result of the functions is a new array
        [!arg[0], !arg[1],  ..., !arg[width-1]].

  NB: The invoker should free the returned array.
*/
static AddArray_ptr add_array_negate_bits(DDMgr_ptr dd, AddArray_ptr arg)
{
  AddArray_ptr res;
  int i;
  const int width = AddArray_get_size(arg);
  /* array should be of the positive size */
  nusmv_assert(width > 0);

  res = AddArray_create(width);
  for (i = 0; i < width; ++i) {
    add_ptr add = add_not(dd, AddArray_get_n(arg, i));
    AddArray_set_n(res, i, add);
  }
  return res;
}

/*!
  \brief Extends the width of a Word expression by padding the
  expression with a padding bit on the left (i.e. highest bits)

  This extension means that a padding bit (paddingBit) is
  added 'arg_repeat' times on the left.  'arg_repeat' has to be a
  constant number.

  \sa AddArray_word_signed_extend, AddArray_word_unsigned_extend
*/
static AddArray_ptr add_array_word_extend(DDMgr_ptr dd,
                                          AddArray_ptr arg,
                                          AddArray_ptr arg_repeat,
                                          add_ptr paddingBit)
{
  const int width = AddArray_get_size(arg);

  AddArray_ptr res;
  int repeat;
  int new_width;

  nusmv_assert(width > 0);

  {
    add_ptr add_repeat;
    node_ptr node_repeat;

    add_repeat = AddArray_get_add(arg_repeat);
    nusmv_assert(add_isleaf(add_repeat));

    node_repeat = add_get_leaf(dd, add_repeat);
    nusmv_assert(node_get_type(node_repeat) == NUMBER);
    repeat = NODE_TO_INT(car(node_repeat));
    nusmv_assert(repeat >= 0);
  }

  new_width = width + repeat;
  res = AddArray_create(new_width);

  /* copies the argument */
  {
    int i = 0;
    for (; i < width; ++i) {
      AddArray_set_n(res, i, add_dup(AddArray_get_n(arg, i)));
    }

    /* extends the sign */
    for (;i < new_width; ++i) AddArray_set_n(res, i, add_dup(paddingBit));
  }

  return res;
}


/*!
  \brief Performs right shift operations
  on a Word expression represented as an array of ADD.
  Every ADD corresponds to a bit of a Word expression

  The "number" argument represent
  the number of bits to shift. "number" should have only one ADD.
  "isSigned" is a flag that the word is signed or unsigned.

  NB: The invoker should destroy the returned array.

  Every i-th bit of returned array will be:
       ITE(number=0 , arg[i],
           ITE(number=1, arg[i+1],
            ...
             ITE(number=width-1-i, arg[width-1],
              ITE(number >=0 && number <= width, SpecBit, FAILURE)
   where SpecBit is zero for unsigned words (isSigned==false) and
   arg[width-1] for signed words (isSigned==true).

   Does anyone have a better encoding?

*/

AddArray_ptr add_array_word_right_shift(DDMgr_ptr dd, AddArray_ptr arg,
                                       AddArray_ptr number,
                                       boolean isSigned)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(dd));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));

  const int width = AddArray_get_size(arg);
  const int numWidth = AddArray_get_size(number);
  AddArray_ptr res;
  add_ptr def_case;
  boolean is_word = add_array_is_word(dd, number);
  unsigned long long maxPossibleValue;
  int i;

  nusmv_assert(width>0 && numWidth>0);

  res = AddArray_create(width);

  /* creates the default case, i.e. the last ITE (see description) */
  {
    add_ptr defaultBit = isSigned
      ? add_dup(AddArray_get_n(arg, width-1)) : add_false(dd);

    def_case = add_array_create_default_value_of_shift_operation(dd,
                                number, width, defaultBit,
                                "Right operand of right-shift is out of range");
    add_free(dd, defaultBit);
  }

  /* find the highest value for number (word or int) */
  if (is_word) maxPossibleValue = (2ULL << (numWidth-1)) - 1;
  else maxPossibleValue = ~0U; /* value for sure greater that width  */

  /* proceed one bit at every iteration */
  for (i = 0; i < width; ++i) {
    add_ptr bit = add_dup(def_case);
    int n;

    /* create all other ITEs */
    for (n=MIN(maxPossibleValue, width - i - 1); n >= 0; --n) {
      add_ptr numeqn_add, tmp;

      if (is_word) {/* number is a word */
        AddArray_ptr an = AddArray_from_word_number(dd,
                                 WordNumberMgr_integer_to_word_number(words, n, numWidth));
        AddArray_ptr numeqn = AddArray_word_equal(dd, number, an);
        numeqn_add = add_dup(AddArray_get_add(numeqn));
        AddArray_destroy(dd, numeqn);
        AddArray_destroy(dd, an);
      }
      else { /* number is not a word */
        add_ptr n_add = add_leaf(dd,
                                 find_node(nodemgr, NUMBER, NODE_FROM_INT(n), Nil));
        numeqn_add = add_apply(dd, node_equal,
                               AddArray_get_add(number), n_add);
        add_free(dd, n_add);
      }

      tmp = add_ifthenelse(dd, numeqn_add, AddArray_get_n(arg, i+n),
                           bit);

      add_free(dd, numeqn_add);
      add_free(dd, bit);
      bit = tmp;
    } /* for (n) */

    /* set the obtained bit into the returned array */
    AddArray_set_n(res, i, bit);
  } /* for (i) */

  add_free(dd, def_case);
  return res;
}

/*!
  \brief Private service

  op can be: any signed relational functions such as
  AddArray_word_less, AddArray_word_less_equal, etc

  \sa AddArray_word_signed_less, AddArray_word_signed_less_equal,
  AddArray_word_signed_greater, AddArray_word_signed_greater_equal
*/
static AddArray_ptr
add_array_word_signed_comparison(DDMgr_ptr dd, APFDAA op,
                                 AddArray_ptr arg1, AddArray_ptr arg2)
{
  /* to compare signed words it is enough to swap sign bits
     between the words and then perform normal (unsigned) comparison */

  const int N = AddArray_get_size(arg1) - 1;

  /* swap the sign bits */
  add_ptr sign1 = AddArray_get_n(arg1, N);
  add_ptr sign2 = AddArray_get_n(arg2, N);
  AddArray_ptr res;

  AddArray_set_n(arg1, N, sign2);
  AddArray_set_n(arg2, N, sign1);

  res = op(dd, arg1, arg2);

  /* restore the sign bit */
  AddArray_set_n(arg1, N, sign1);
  AddArray_set_n(arg2, N, sign2);

  return res;


#if 0
  /* A <s B   ===   (A[msb] & !B[msb]) | ((A[msb] = B[msb]) & (A <u B))

     A <=s B  ===   (A[msb] & !B[msb]) | ((A[msb] = B[msb]) & (A <=u B))
  */

  const int width = AddArray_get_size(arg1);

  add_ptr arg1_msb, arg2_msb; /* Not to be freed */
  add_ptr tmp1, tmp2;
  add_ptr not_arg2_msb;
  AddArray_ptr less_op;
  add_ptr res;

  nusmv_assert(width == AddArray_get_size(arg2) && width > 0);

  /* prepare the arguments */
  arg1_msb = AddArray_get_n(arg1, width-1);
  arg2_msb = AddArray_get_n(arg2, width-1);
  not_arg2_msb = add_not(dd, arg2_msb);
  less_op = op(dd, arg1, arg2);

  /* Calculates tmp2 := ((A[msb] = B[msb]) & (A <u B)) */
  tmp1 = add_apply(dd, node_equal, arg1_msb, arg2_msb);
  tmp2 = add_and(dd, tmp1, AddArray_get_add(less_op));
  AddArray_destroy(dd, less_op);
  add_free(dd, tmp1);

  /* Calculates tmp1 := (A[msb] & !B[msb]) */
  tmp1 = add_and(dd, arg1_msb, not_arg2_msb);

  /* The final result */
  res = add_or(dd, tmp1, tmp2);

  add_free(dd, tmp1);
  add_free(dd, tmp2);
  add_free(dd, not_arg2_msb);

  return AddArray_from_add(res);
#endif
}

static inline boolean _add_is_true_false_add(DDMgr_ptr dd, add_ptr f)
{
  /* TODO: This can be improved by using memoizing */
  if ((add_ptr)NULL == f) return false;

  if (add_isleaf(f)) {
    if (add_is_true(dd, f) || (add_is_false(dd, f))) return true;
    else return false;
  }
  return _add_is_true_false_add(dd, add_then(dd, f)) &&
    _add_is_true_false_add(dd, add_else(dd, f));
}

/*!
  \brief Private service

  Checks whether the given AddArray is a word or not.
*/
static inline boolean add_array_is_word(DDMgr_ptr dd,
                                        const AddArray_ptr number)
{
  boolean is_word = false;
  const int numWidth = AddArray_get_size(number);

  if (numWidth > 1) {
    is_word = true;
  }
  else {
    add_ptr add_number;
    nusmv_assert(numWidth == 1);
    add_number = AddArray_get_add(number);

    /* Is this a word[1]?, i.e. it is a constant TRUE/FALSE or it is a
       boolean expression whose leaves are boolean constants, not
       numbers. */
    is_word = _add_is_true_false_add(dd, add_number);
  }

  return is_word;
}
