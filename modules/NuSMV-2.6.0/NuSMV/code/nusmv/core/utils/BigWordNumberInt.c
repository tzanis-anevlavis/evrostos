/* ---------------------------------------------------------------------------


  This file is part of the ``enc.utils'' package of NuSMV version 2.
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
  \brief \todo: Missing synopsis

  \todo: Missing description

*/


#if HAVE_CONFIG_H
#include "nusmv-config.h"
#endif

#include "nusmv/core/utils/BigWordNumber_private.h"

WordNumberValue_intern
WordNumber_create_WordNumberValue_intern(Number number, int width)
{
  WordNumberValue_intern value;
  value.dat = number;
  value.width = width;

  nusmv_assert(width > 0);

  return value;
}

WordNumberValue_intern
WordNumber_copy_WordNumberValue_intern(WordNumberValue_intern* original) {
  WordNumberValue_intern value;
  value.dat = BigNumber_copy(&(original->dat));
  value.width = original->width;
  return value;
}

/*!
  \brief frees the space allocated to the BigNumber in
                      the wordvalue

  frees the space allocated to the BigNumber in
                      the wordvalue
*/

void
WNV_free_WordNumberValue_intern(WordNumberValue_intern_ptr value)
{
  BigNumber_free_number(&(value->dat));
}

WordNumberValue_intern
WordNumber_evaluate_unsigned_extend(WordNumberValue_intern v,
                                    int numberOfTimes)
{
  WordNumberValue_intern value;
  int new_width;

  value = WordNumber_copy_WordNumberValue_intern(&v);
  new_width = value.width + numberOfTimes;

  value = WordNumber_create_WordNumberValue_intern(value.dat, new_width);

  return value;
}

WordNumberValue_intern
WordNumber_evaluate_signed_extend(WordNumberValue_intern v,
                                  int numberOfTimes)
{
  int msb;
  Number extension;
  boolean sign;
  WordNumberValue_intern extension_v;
  WordNumberValue_intern value;

  if (numberOfTimes == 0) {
    return v;
  }

  msb = v.width - 1;
  sign = BigNumber_test_bit(&(v.dat), msb);
  if (sign) {
    extension = BigNumber_max_unsigned_int(numberOfTimes);
  }
  else {
    extension = BigNumber_make_number_from_unsigned_long_long(0);
  }

  extension_v = WordNumber_create_WordNumberValue_intern(extension,
                                                         numberOfTimes);
  value = WordNumber_evaluate_concat(extension_v, v);
  WNV_free_WordNumberValue_intern(&extension_v);
  return value;
}

WordNumberValue_intern
WordNumber_evaluate_right_rotate(WordNumberValue_intern v, int numberOfBits)
{
  WordNumberValue_intern rvalue;
  size_t msb;
  size_t lsb;
  int i;

  /* TODO(Bas): implement this using shifts and quicker */
  numberOfBits = numberOfBits % v.width;
  rvalue = WordNumber_copy_WordNumberValue_intern(&v);
  msb = rvalue.width - 1;
  lsb = 0;

  for (i = 0; i < numberOfBits; ++i) {
    int sign = BigNumber_test_bit(&(rvalue.dat), lsb);
    Number new_v = BigNumber_bit_right_shift(&(rvalue.dat), 1);
    BigNumber_set_bit(&new_v, msb, sign);
    if (i > 0) {
      /* delete reference to old number */
      BigNumber_free_number(&(rvalue.dat));
    }
    rvalue.dat = new_v;
  }

  return rvalue;
}

WordNumberValue_intern
WordNumber_evaluate_left_rotate(WordNumberValue_intern v, int numberOfBits)
{
  WordNumberValue_intern raw_value;
  size_t msb;
  size_t lsb;
  int i;
  Number max;
  Number q;
  Number r;

  /* TODO(Bas): implement this using shifts and quicker */
  numberOfBits = numberOfBits % v.width;
  raw_value = WordNumber_copy_WordNumberValue_intern(&v);
  msb = v.width - 1;
  lsb = 0;
  for (i = 0; i < numberOfBits; ++i) {
    int sign = BigNumber_test_bit(&(raw_value.dat), msb);
    Number new_v = BigNumber_bit_left_shift(&(raw_value.dat), 1);
    BigNumber_set_bit(&new_v, lsb, sign);
    if (i > 0) {
      /*   delete reference to old number */
      BigNumber_free_number(&(raw_value.dat));
    }
    raw_value = WordNumber_create_WordNumberValue_intern(new_v, v.width);
  }

  max = BigNumber_pow2(v.width);
  q = BigNumber_make_number_from_unsigned_long_long(0);
  r = BigNumber_make_number_from_unsigned_long_long(0);

  BigNumber_divmod(&(raw_value.dat), &max, &q, &r);
  BigNumber_free_number(&max);
  WNV_free_WordNumberValue_intern(&raw_value);
  BigNumber_free_number(&q);

  return WordNumber_create_WordNumberValue_intern(r, v.width);
}

WordNumberValue_intern
WordNumber_evaluate_left_shift(WordNumberValue_intern v, int numberOfBits) 
{
  Number raw_result = BigNumber_bit_left_shift(&(v.dat), numberOfBits);
  Number q = BigNumber_make_number_from_unsigned_long_long(0);
  Number r = BigNumber_make_number_from_unsigned_long_long(0);
  Number modfactor = BigNumber_pow2(v.width);
  WordNumberValue_intern value;
  BigNumber_divmod(&raw_result, &modfactor, &q, &r);
  value = WordNumber_create_WordNumberValue_intern(r, v.width);
  BigNumber_free_number(&raw_result);
  BigNumber_free_number(&modfactor);
  BigNumber_free_number(&q);
  return value;
}

WordNumberValue_intern
WordNumber_evaluate_sright_shift(WordNumberValue_intern v, int numberOfBits)
{
  int msb = v.width - 1;
  boolean sign = BigNumber_test_bit(&(v.dat), msb);

  if (v.width == 1) {
    return v;
  }

  if (sign) {
    WordNumberValue_intern not_v = WordNumber_evaluate_not(v);
    WordNumberValue_intern lshr;
    WordNumberValue_intern nlshr;

    lshr  = WordNumber_evaluate_uright_shift(not_v, numberOfBits);
    nlshr = WordNumber_evaluate_not(lshr);
    WNV_free_WordNumberValue_intern(&not_v);
    WNV_free_WordNumberValue_intern(&lshr);

    return nlshr;
  }
  else {
    WordNumberValue_intern lshr;
    lshr = WordNumber_evaluate_uright_shift(v, numberOfBits);
    return lshr;
  }
}

WordNumberValue_intern
WordNumber_evaluate_uright_shift(WordNumberValue_intern v, int numberOfBits)
{
  Number n = BigNumber_bit_right_shift(&(v.dat), numberOfBits);
  WordNumberValue_intern rvalue;
  rvalue = WordNumber_create_WordNumberValue_intern(n, v.width);
  return rvalue;
}

WordNumberValue_intern
WordNumber_evaluate_select(WordNumberValue_intern v, int highBit, int lowBit)
{
  Number argument_number = BigNumber_bit_right_shift(&(v.dat), lowBit);
  Number modfactor = BigNumber_pow2((highBit - lowBit) + 1);
  Number q = BigNumber_make_number_from_unsigned_long_long(0);
  Number r = BigNumber_make_number_from_unsigned_long_long(0);
  int newwidth = (highBit - lowBit) + 1;
  WordNumberValue_intern rvalue;

  BigNumber_divmod(&argument_number, &modfactor, &q, &r);

  rvalue = WordNumber_create_WordNumberValue_intern(r, newwidth);

  BigNumber_free_number(&argument_number);
  BigNumber_free_number(&modfactor);
  BigNumber_free_number(&q);

  return rvalue;
}

WordNumberValue_intern
WordNumber_evaluate_concat(WordNumberValue_intern v1,
                           WordNumberValue_intern v2)
{
  Number l = BigNumber_bit_left_shift(&(v1.dat), v2.width);
  Number res = BigNumber_plus(&l, &(v2.dat));
  int new_width = v1.width + v2.width;
  /* Number res = twos_complement(res_raw, new_width); Check this */
  WordNumberValue_intern rvalue = WordNumber_create_WordNumberValue_intern(res,
                                                                    new_width);

  BigNumber_free_number(&l);

  return rvalue;
}

WordNumberValue_intern
WordNumber_evaluate_implies(WordNumberValue_intern v1,
                            WordNumberValue_intern v2)
{
  WordNumberValue_intern not_v1=  WordNumber_evaluate_not(v1);
  WordNumberValue_intern implies = WordNumber_evaluate_or(not_v1, v2);

  return implies;
}

WordNumberValue_intern
WordNumber_evaluate_xnor(WordNumberValue_intern v1, WordNumberValue_intern v2)
{
  WordNumberValue_intern xor_v = WordNumber_evaluate_xor(v1, v2);

  return WordNumber_evaluate_not(xor_v);
}

WordNumberValue_intern
WordNumber_evaluate_xor(WordNumberValue_intern v1,
                        WordNumberValue_intern v2)
{
  Number res = BigNumber_bit_xor(&(v1.dat), &(v2.dat));
  WordNumberValue_intern rvalue = WordNumber_create_WordNumberValue_intern(res,
                                                                    v1.width);

  return rvalue;

}

WordNumberValue_intern
WordNumber_evaluate_or(WordNumberValue_intern v1, WordNumberValue_intern v2)
{
  Number res = BigNumber_bit_or(&(v1.dat), &(v2.dat));
  WordNumberValue_intern rvalue = WordNumber_create_WordNumberValue_intern(res,
                                                                    v1.width);

  return rvalue;
}

WordNumberValue_intern
WordNumber_evaluate_and(WordNumberValue_intern v1, WordNumberValue_intern v2)
{
  Number res = BigNumber_bit_and(&(v1.dat), &(v2.dat));
  WordNumberValue_intern rvalue;
  rvalue = WordNumber_create_WordNumberValue_intern(res, v1.width);

  return rvalue;
}

WordNumberValue_intern
WordNumber_evaluate_srem(WordNumberValue_intern v1, WordNumberValue_intern v2)
{
  boolean msb_v1, msb_v2;
  WordNumberValue_intern rvalue;

  msb_v1 = BigNumber_test_bit(&(v1.dat), v1.width - 1);
  msb_v2 = BigNumber_test_bit(&(v2.dat), v2.width - 1);

  if (!msb_v1 && !msb_v2) {
    return WordNumber_evaluate_urem(v1, v2);
  }
  else if (msb_v1 && !msb_v2) {
    WordNumberValue_intern neg_v1 = WordNumber_evaluate_unary_minus(v1);
    WordNumberValue_intern umod = WordNumber_evaluate_urem(neg_v1, v2);
    rvalue = WordNumber_evaluate_unary_minus(umod);

    WNV_free_WordNumberValue_intern(&neg_v1);
    WNV_free_WordNumberValue_intern(&umod);
  }
  else if (!msb_v1 && msb_v2) {
    WordNumberValue_intern neg_v2 = WordNumber_evaluate_unary_minus(v2);
    rvalue = WordNumber_evaluate_urem(v1, neg_v2);

    WNV_free_WordNumberValue_intern(&neg_v2);
  }
  else {
    WordNumberValue_intern neg_v1 = WordNumber_evaluate_unary_minus(v1);
    WordNumberValue_intern neg_v2 = WordNumber_evaluate_unary_minus(v2);
    WordNumberValue_intern umod_of_negs = WordNumber_evaluate_urem(neg_v1,
                                                                   neg_v2);
    rvalue = WordNumber_evaluate_unary_minus(umod_of_negs);

    WNV_free_WordNumberValue_intern(&neg_v1);
    WNV_free_WordNumberValue_intern(&neg_v2);
    WNV_free_WordNumberValue_intern(&umod_of_negs);
  }

  return rvalue;
}

WordNumberValue_intern
WordNumber_evaluate_urem(WordNumberValue_intern v1, WordNumberValue_intern v2)
{
  Number q = BigNumber_make_number_from_unsigned_long_long(0);
  Number r = BigNumber_make_number_from_unsigned_long_long(0);
  BigNumber_divmod(&(v1.dat), &(v2.dat), &q, &r);

  BigNumber_free_number(&q);

  return WordNumber_create_WordNumberValue_intern(r, v1.width);
}

WordNumberValue_intern
WordNumber_evaluate_sdiv(WordNumberValue_intern v1, WordNumberValue_intern v2)
{
  boolean bit1, bit2;
  WordNumberValue_intern rvalue;
  bit1 = BigNumber_test_bit(&(v1.dat), v1.width - 1);
  bit2 = BigNumber_test_bit(&(v2.dat), v2.width - 1);

  if (!bit1 && !bit2) {
    return WordNumber_evaluate_udiv(v1, v2);
  }
  else if (bit1 && !bit2) {
    WordNumberValue_intern neg_v1 = WordNumber_evaluate_unary_minus(v1);
    WordNumberValue_intern dif = WordNumber_evaluate_udiv(neg_v1, v2);
    rvalue = WordNumber_evaluate_unary_minus(dif);

    WNV_free_WordNumberValue_intern(&dif);
    WNV_free_WordNumberValue_intern(&neg_v1);
  }
  else if (!bit1 && bit2) {
    WordNumberValue_intern neg_v2 = WordNumber_evaluate_unary_minus(v2);
    WordNumberValue_intern dif = WordNumber_evaluate_udiv(v1, neg_v2);
    rvalue = WordNumber_evaluate_unary_minus(dif);

    WNV_free_WordNumberValue_intern(&dif);
    WNV_free_WordNumberValue_intern(&neg_v2);
  }
  else {
    WordNumberValue_intern neg_v1 = WordNumber_evaluate_unary_minus(v1);
    WordNumberValue_intern neg_v2 = WordNumber_evaluate_unary_minus(v2);
    rvalue = WordNumber_evaluate_udiv(neg_v1, neg_v2);

    WNV_free_WordNumberValue_intern(&neg_v1);
    WNV_free_WordNumberValue_intern(&neg_v2);
  }
  return rvalue;
}

WordNumberValue_intern
WordNumber_evaluate_not(WordNumberValue_intern v)
{
  Number res_raw = BigNumber_bit_complement(&(v.dat));
  Number res = BigNumber_twos_complement(&res_raw, v.width);

  BigNumber_free_number(&res_raw);

  return WordNumber_create_WordNumberValue_intern(res, v.width);
}

WordNumberValue_intern
WordNumber_evaluate_udiv(WordNumberValue_intern v1, WordNumberValue_intern v2)
{
  Number q = BigNumber_make_number_from_unsigned_long_long(0);
  Number r = BigNumber_make_number_from_unsigned_long_long(0);

  BigNumber_divmod(&(v1.dat), &(v2.dat), &q, &r);

  BigNumber_free_number(&r);

  return WordNumber_create_WordNumberValue_intern(q, v1.width);
}

WordNumberValue_intern
WordNumber_evaluate_mul(WordNumberValue_intern v1, WordNumberValue_intern v2)
{
  Number q = BigNumber_make_number_from_unsigned_long_long(0);
  Number r = BigNumber_make_number_from_unsigned_long_long(0);
  Number n = BigNumber_multiplication(&(v1.dat), &(v2.dat));
  Number modfactor = BigNumber_pow2(v1.width);

  BigNumber_divmod(&n, &modfactor, &q, &r);

  BigNumber_free_number(&q);
  BigNumber_free_number(&n);
  BigNumber_free_number(&modfactor);

  return WordNumber_create_WordNumberValue_intern(r, v1.width);
}

WordNumberValue_intern
WordNumber_evaluate_minus(WordNumberValue_intern v1, WordNumberValue_intern v2)
{
  WordNumberValue_intern negv2 = WordNumber_evaluate_unary_minus(v2);
  return WordNumber_evaluate_plus(v1, negv2);
}

WordNumberValue_intern
WordNumber_evaluate_plus(WordNumberValue_intern v1, WordNumberValue_intern v2)
{
  Number q = BigNumber_make_number_from_unsigned_long_long(0);
  Number r = BigNumber_make_number_from_unsigned_long_long(0);
  Number n = BigNumber_plus(&(v1.dat), &(v2.dat));
  Number modfactor = BigNumber_pow2(v1.width);

  BigNumber_divmod(&n, &modfactor, &q, &r);

  BigNumber_free_number(&q);
  BigNumber_free_number(&n);
  BigNumber_free_number(&modfactor);

  return WordNumber_create_WordNumberValue_intern(r, v1.width);
}

WordNumberValue_intern
WordNumber_evaluate_unary_minus(WordNumberValue_intern v)
{
  WordNumberValue_intern not_v = WordNumber_evaluate_not(v);
  WordNumberValue_intern one;
  WordNumberValue_intern add;
  Number n = BigNumber_make_number_from_unsigned_long_long(1);

  one = WordNumber_create_WordNumberValue_intern(n, v.width);
  add = WordNumber_evaluate_plus(not_v, one);

  WNV_free_WordNumberValue_intern(&not_v);
  WNV_free_WordNumberValue_intern(&one);

  return add;
}

