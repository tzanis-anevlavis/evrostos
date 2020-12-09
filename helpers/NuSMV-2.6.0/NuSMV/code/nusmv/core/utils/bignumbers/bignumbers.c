/* ---------------------------------------------------------------------------


  This file is part of the ``utils/bignumbers'' package of NuSMV version 2.
  Copyright (C) 2012 by FBK-irst.

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
  \author \todo: Missing author
  \brief Link between Numbers API and QNumber implementation

  \todo: Missing description

*/


#include "nusmv/core/utils/bignumbers/numbersInt.h"
#include "nusmv/core/utils/bignumbers/bvnumbersInt.h"
#include "nusmv/core/utils/bignumbers/bignumbers.h"
#include "nusmv/core/utils/portability.h" /* for LLONG_MAX, errno */


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

/*****************************************************************************
 * Translation functions between Interface and Implementation Layer
 *****************************************************************************/
static QNumber* number2qnumber(Number n);

static Number qnumber2number(QNumber* t);

/*****************************************************************************
 * Exported functions
 *****************************************************************************/

Number BigNumber_bit_complement(Number* n)
{
  QNumber* org = (number2qnumber(*n));
  QNumber value = BVQNumber_bit_complement(org);
  QNumber* nnumber = QNumber_copy_to_heap(&value);
  return qnumber2number(nnumber);
}

Number BigNumber_twos_complement(Number* n, int width)
{
  const QNumber*  org;
  QNumber value;
  QNumber* nnumber;

  nusmv_assert(width > 0);

  org = (number2qnumber(*n));
  value = BVQNumber_twos_complement(org, width);
  nnumber = QNumber_copy_to_heap(&value);

  return qnumber2number(nnumber);
}

Number BigNumber_bit_and(Number* left, Number* right)
{
  const QNumber* orgl = (number2qnumber(*left));
  const QNumber* orgr = (number2qnumber(*right));
  QNumber value = BVQNumber_bit_and(orgl, orgr);
  QNumber* nnumber = QNumber_copy_to_heap(&value);
  return qnumber2number(nnumber);
}

Number BigNumber_bit_or(Number* left, Number* right)
{
  const QNumber* orgl = (number2qnumber(*left));
  const QNumber* orgr = (number2qnumber(*right));
  QNumber value = BVQNumber_bit_or(orgl, orgr);
  QNumber* nnumber = QNumber_copy_to_heap(&value);
  return qnumber2number(nnumber);
}

Number BigNumber_bit_xor(Number* left, Number* right)
{
  const QNumber* orgl = (number2qnumber(*left));
  const QNumber* orgr = (number2qnumber(*right));
  QNumber value = BVQNumber_bit_xor(orgl, orgr);
  QNumber* nnumber = QNumber_copy_to_heap(&value);
  return qnumber2number(nnumber);
}

Number BigNumber_bit_left_shift(Number* number, int amount)
{
  const QNumber* org;
  QNumber value;
  QNumber* nnumber;

  nusmv_assert(amount >= 0);
  org = (number2qnumber(*number));
  value = BVQNumber_bit_left_shift(org, amount);
  nnumber = QNumber_copy_to_heap(&value);

  return qnumber2number(nnumber);
}

Number BigNumber_bit_right_shift(Number* number, int amount)
{

  const QNumber* org;
  QNumber value;
  QNumber* nnumber;

  nusmv_assert(amount >= 0);

  org = (number2qnumber(*number));
  value = BVQNumber_bit_right_shift(org, amount);
  nnumber = QNumber_copy_to_heap(&value);

  return qnumber2number(nnumber);
}

Number BigNumber_pow2(int widht)
{
  QNumber value;
  QNumber* nnumber;

  nusmv_assert(widht >= 0);
  value = BVQNumber_pow2(widht);

  nnumber = QNumber_copy_to_heap(&value);
  return qnumber2number(nnumber);
}

Number BigNumber_max_unsigned_int(int widht)
{
  QNumber value;
  QNumber one;
  QNumber* nnumber;

  nusmv_assert(widht >= 0);

  value = BVQNumber_pow2(widht);
  one = QNumber_from_long(1);
  QNumber_operator_self_minus(&value, &one);
  nnumber = QNumber_copy_to_heap(&value);
  return qnumber2number(nnumber);
}

Number BigNumber_make_number_from_long(long n)
{
  QNumber value = QNumber_from_long(n);
  QNumber* nnumber = QNumber_copy_to_heap(&value);
  return qnumber2number(nnumber);
}

boolean BigNumber_to_unsigned_long_long(Number* number,
                                        unsigned long long* target)
{
  QNumber* org = (number2qnumber(*number));

  nusmv_assert(target != (unsigned long long*) NULL);

  if (!QNumber_big(org)) {
    long t;
    int b = BVQNumber_to_long(org, &t);
    nusmv_assert(b);
    *target = (unsigned long long) t;
    return b;
  }
  else {
    unsigned long long ull;
    char* str;
    char* err;

    str = QNumber_print_integer(org, 10);
    ull = strtoull(str, &err, 10);
    if (NULL == (char*) str || '\0' != *err) {
      return false;
    }
    *target = ull;
    return true;
  }
}

void BigNumber_set_bit(Number* number, int location, int value)
{
  QNumber* org;

  nusmv_assert(location >= 0);
  nusmv_assert(value >=0 && value <= 1);

  org = (number2qnumber(*number));
  BVQNumber_set_bit(org, location, value);
}

boolean BigNumber_test_bit(Number* number, int location)
{
  QNumber* org = (number2qnumber(*number));
  boolean rvalue;

  nusmv_assert(location >= 0);

  rvalue = BVQNumber_test_bit(org, location);

  return rvalue;
}

Number BigNumber_make_number_from_unsigned_long_long(unsigned long long n)
{
  QNumber value = QNumber_make_number_from_unsigned_long_long(n);
  Number res = qnumber2number(QNumber_copy_to_heap(&value));
  return res;
}

Number BigNumber_multiplication(Number* left, Number* right)
{
  const QNumber* orgl = (number2qnumber(*left));
  const QNumber* orgr = (number2qnumber(*right));
  QNumber value = QNumber_operator_mul(orgl, orgr);
  QNumber* nnumber = QNumber_copy_to_heap(&value);
  return qnumber2number(nnumber);
}

Number BigNumber_plus(Number* left, Number* right)
{
  const QNumber* orgl = (number2qnumber(*left));
  const QNumber* orgr = (number2qnumber(*right));
  QNumber value = QNumber_operator_plus(orgl, orgr);
  QNumber* nnumber = QNumber_copy_to_heap(&value);
  return qnumber2number(nnumber);
}

Number BigNumber_minus(Number* left, Number* right)
{
  const QNumber* orgl = (number2qnumber(*left));
  const QNumber* orgr = (number2qnumber(*right));
  QNumber value = QNumber_operator_minus(orgl, orgr);
  QNumber* nnumber = QNumber_copy_to_heap(&value);
  return qnumber2number(nnumber);
}

Number BigNumber_copy(Number* orig)
{
  QNumber* org = (number2qnumber(*orig));
  return qnumber2number(QNumber_copy_to_heap(org));
}

boolean BigNumber_less_than(Number* left, Number* right)
{
  const QNumber* orgl = (number2qnumber(*left));
  const QNumber* orgr = (number2qnumber(*right));
  return QNumber_operator_less_than(orgl, orgr);
}

boolean BigNumber_identity(Number* left, Number* right)
{
  const QNumber* orgl = (number2qnumber(*left));
  const QNumber* orgr = (number2qnumber(*right));
  return (orgl == orgr);
}

boolean BigNumber_equal(Number* left, Number* right)
{
  const QNumber* orgl = (number2qnumber(*left));
  const QNumber* orgr = (number2qnumber(*right));
  return QNumber_operator_equals(orgl, orgr);
}

boolean BigNumber_does_integer_fit_into_number_of_bits(Number* value,
                                                       int widht)
{
  const QNumber* org = (number2qnumber(*value));
  return BVQNumber_fits(org, widht);
}

void BigNumber_divmod(Number* left, Number* right, Number* q, Number* r)
{
  const QNumber* orgl = (number2qnumber(*left));
  const QNumber* orgr = (number2qnumber(*right));
  QNumber* target_q = (number2qnumber(*q));
  QNumber* target_r = (number2qnumber(*r));

  nusmv_assert(q != (Number*) NULL);
  nusmv_assert(r != (Number*) NULL);

  QNumber_divmod(orgl, orgr, target_q, target_r);
}

void BigNumber_free_number(Number* number)
{
  QNumber* tofree = number2qnumber(*number);
  QNumber_clean_gmp(tofree);
  FREE(tofree);
}

char* BigNumber_print_as_number(Number* number, int base)
{
  QNumber* toPrint = number2qnumber(*number);
  return QNumber_print_integer(toPrint, base);
}

int BigNumber_assign_number_from_string(char* string,
                                        char* error_char,
                                        int base,
                                        Number* numb)
{
  int b = QNumber_integer_from_string(string,
                                      error_char,
                                      base,
                                      (number2qnumber(*numb)));
  return b == 0 ? 1 : 0;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/


/*!
  \brief Translates a QNumber into a Number

  Translates a QNumber into a Number
*/

inline Number qnumber2number(QNumber* t)
{
  Number ret;
  ret.repr = (void *) t;
  return ret;
}


/*!
  \brief Translates a Number into a QNumber

  Translates a Number into a QNumber
*/

inline QNumber* number2qnumber(Number n)
{
  void* struct_p = n.repr;
  QNumber* rvalue = ((QNumber*) struct_p);

  nusmv_assert(struct_p != (void*) NULL);
  return rvalue;
}
