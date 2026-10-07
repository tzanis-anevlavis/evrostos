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
# include "nusmv-config.h"
#endif

#include "nusmv/core/utils/WordNumberMgr.h"
#include "nusmv/core/utils/BigWordNumber_private.h"

#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/utils/portability.h" /* for LLONG_MAX, errno */
#include "nusmv/core/utils/error.h"

#include <stdlib.h> /* for strtol */

/*---------------------------------------------------------------------------*/
/* Types definition                                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro definition                                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable definition                                                       */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void WordNumber_init(NuSMVEnv_ptr env)
{
  WordNumberMgr_ptr mgr = WordNumberMgr_create(env);

  NuSMVEnv_set_value(env, ENV_WORD_NUMBER_MGR, mgr);
}

void WordNumber_quit(NuSMVEnv_ptr env)
{
  WordNumberMgr_ptr mgr;
  mgr = WORD_NUMBER_MGR(NuSMVEnv_remove_value(env, ENV_WORD_NUMBER_MGR));

  WordNumberMgr_destroy(mgr);
}

int WordNumber_max_width()
{
  return WordNumberMgr_max_width();
}

unsigned long long WordNumber_get_unsigned_value(WordNumber_ptr self)
{
  unsigned long long value;
  int ok;

  WORD_NUMBER_CHECK_INSTANCE(self);

  value = 0;
  ok = BigNumber_to_unsigned_long_long(&(self->value.dat), &value);

  nusmv_assert(ok);

  return value;
}

signed long long WordNumber_get_signed_value(WordNumber_ptr self)
{
  WORD_NUMBER_CHECK_INSTANCE(self);

  return word_number_to_signed_c_value(self);
}

boolean WordNumber_get_sign(WordNumber_ptr self)
{
  WORD_NUMBER_CHECK_INSTANCE(self);

  return WordNumber_get_bit(self, WordNumber_get_width(self)-1);
}

int WordNumber_get_width(WordNumber_ptr self)
{
  WORD_NUMBER_CHECK_INSTANCE(self);

  return self->value.width;
}

boolean WordNumber_get_bit(WordNumber_ptr self, int n)
{
  WORD_NUMBER_CHECK_INSTANCE(self);
  nusmv_assert(n >= 0 && n < self->value.width);

  return BigNumber_test_bit(&((self->value).dat), n);
}

const char* WordNumber_get_parsed_string(WordNumber_ptr self)
{
  WORD_NUMBER_CHECK_INSTANCE(self);

  /*if ((string_ptr) NULL != self->parsedString) {
    return UStringMgr_get_string_text(self->parsedString);
  }*/
  return (const char*) NULL;
}

int WordNumber_based_print(FILE* output_stream,
                           WordNumber_ptr self,
                           int base,
                           boolean isSigned)
{
  char* result = WordNumber_to_based_string(self, base, isSigned);

  if ((char*) NULL == result) {
    return -1;
  }
  return fprintf(output_stream, "%s", result);
}

char* WordNumber_to_based_string(WordNumber_ptr self,
                                 int base,
                                 boolean isSigned)
{
  WORD_NUMBER_CHECK_INSTANCE(self);

  return WordNumber_Internal_value_to_based_string(self->value,
                                                   base,
                                                   isSigned);
}

char* WordNumber_Internal_value_to_based_string(WordNumberValue_intern value,
                                                int base,
                                                boolean isSigned)
{
  static char* buffer = (char*) NULL;
  static int buffer_size = 0;
  int printed = INT_MAX;
  int width = value.width;
  char* number_rep = (char*) NULL;
  boolean sign = isSigned;
  char base_char = ' ';
  Number copy;

  /*
   * allocate a buffer. The requires string is very
   * likely less than width+20
   */
  if (buffer_size < (width + 20)) {
    buffer_size = width + 20;
    buffer = REALLOC(char, buffer, buffer_size);
  }

  switch (base) {
    case 2:
      base_char = 'b';
      break;
    case 8:
      base_char = 'o';
      break;
    case 10:
      base_char = 'd';
      break;
    case 16:
      base_char = 'h';
      break;
    default: error_unreachable_code();
      break;
  }

  switch (base) {
    case 2:
    case 8:
    case 16:
      /*
       * Bas: Simple case only display the unsigned value of the word
       * with the appropiate information
       */
      number_rep = BigNumber_print_as_number(&(value.dat), base);
      printed = snprintf(buffer,
                         buffer_size, "0%s%c%d_%s",
                         isSigned ? "s" : "u" ,
                         base_char,
                         value.width,
                         number_rep);
      break;
    case 10:
      sign = sign && BigNumber_test_bit(&(value.dat), value.width - 1);

      if (sign && value.width > 1) {
        /* Bas: -1 & 0 of size 1 can be handled as unsigned numbers */
        Number minus_factor = BigNumber_pow2(value.width - 1);
        WordNumberValue_intern plus_factor;
        plus_factor = WordNumber_evaluate_select(value, (value.width - 2), 0);
        copy = BigNumber_minus(&minus_factor,  &(plus_factor.dat));
        BigNumber_free_number(&minus_factor);
        BigNumber_free_number(&(plus_factor.dat));
      }
      else {
        copy = BigNumber_copy(&(value.dat));
      }

      number_rep = BigNumber_print_as_number(&(copy), base);
      printed = snprintf(buffer,
                         buffer_size,
                         "%s0%s%c%d_%s",
                         (sign ? "-" : ""),
                         (isSigned ? "s" : "u"),
                         base_char,
                         value.width,
                         number_rep);
      BigNumber_free_number(&copy);
      break;
    default: error_unreachable_code();
      break;
  }

  /* char* number_interpretation = BigNumber_print_as_number(&res, base); */
  SNPRINTF_CHECK(printed, buffer_size);


  if (number_rep != NULL) {
    FREE (number_rep);
  }

  if (printed <= 0) {
    return (char*) NULL; /* error in printing */
  }
  nusmv_assert(printed < buffer_size);
  return buffer;
}

boolean WordNumber_is_zero(WordNumber_ptr v)
{
  WORD_NUMBER_CHECK_INSTANCE(v);

  return (WordNumber_get_unsigned_value(v) == 0LL);
}

boolean WordNumber_equal(WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue_intern_ptr value1;
  WordNumberValue_intern_ptr value2;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  value1 = &(v1->value);
  value2 = &(v2->value);

  return BigNumber_equal(&((*value1).dat), &((*value2).dat));
}

boolean WordNumber_not_equal(WordNumber_ptr v1, WordNumber_ptr v2)
{
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  return !WordNumber_equal(v1, v2);
}

boolean WordNumber_unsigned_less(WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue_intern_ptr value1;
  WordNumberValue_intern_ptr value2;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  value1 = &(v1->value);
  value2 = &(v2->value);

  return BigNumber_less_than(&((*value1).dat), &((*value2).dat));
}

boolean WordNumber_unsigned_less_or_equal(WordNumber_ptr v1, WordNumber_ptr v2)
{
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  return !WordNumber_unsigned_less(v2, v1);
}

boolean WordNumber_unsigned_greater(WordNumber_ptr v1, WordNumber_ptr v2)
{
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  return !WordNumber_unsigned_less_or_equal(v1, v2);
}

boolean WordNumber_unsigned_greater_or_equal(WordNumber_ptr v1,
                                             WordNumber_ptr v2)
{
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  return !WordNumber_unsigned_greater(v2, v1);
}

boolean WordNumber_signed_less(WordNumber_ptr v1, WordNumber_ptr v2)
{
  boolean bit1, bit2;
  WordNumberValue_intern_ptr value1;
  WordNumberValue_intern_ptr value2;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  value1 = &(v1->value);
  value2 = &(v2->value);

  bit1 = BigNumber_test_bit(&((*value1).dat), v1->value.width - 1);
  bit2 = BigNumber_test_bit(&((*value2).dat), v2->value.width - 1);

  if (bit1 == bit2) {
    return WordNumber_unsigned_less(v1, v2);
  }
  else {
    return bit1;
  }

}

boolean WordNumber_signed_less_or_equal(WordNumber_ptr v1, WordNumber_ptr v2)
{
  return !WordNumber_signed_less(v2, v1);
}

boolean WordNumber_signed_greater(WordNumber_ptr v1, WordNumber_ptr v2)
{
  return !WordNumber_signed_less_or_equal(v1, v2);
}

boolean WordNumber_signed_greater_or_equal(WordNumber_ptr v1,
                                           WordNumber_ptr v2)
{
  return !WordNumber_signed_greater(v2, v1);
}

/*!
  \brief Returns max(v1, v2)


*/

WordNumber_ptr WordNumber_max(WordNumber_ptr v1,
                              WordNumber_ptr v2,
                              boolean isSigned)
{
  if (isSigned) {
    return WordNumber_signed_less(v1, v2) ? v2 : v1;
  }
  return WordNumber_unsigned_less(v1, v2) ? v2 : v1;
}

/*!
  \brief Returns min(v1, v2), taking sign into account


*/

WordNumber_ptr WordNumber_min(WordNumber_ptr v1,
                              WordNumber_ptr v2,
                              boolean isSigned)
{
  if (isSigned) {
    return WordNumber_signed_less(v1, v2) ? v1 : v2;
  }
  return WordNumber_unsigned_less(v1, v2) ? v1 : v2;
}


/*!
  \brief Method transported for old WordNumber


*/

WordNumberValue word_number_to_signed_c_value(const WordNumber_ptr self)
{
  WordNumberValue uv = WordNumber_get_unsigned_value(self);
  int sign = uv >> (self->value.width - 1);
  WordNumberValue l;

  nusmv_assert(0 == sign || 1 == sign); /* one the highest bit only */

  if (sign == 0) {
    return uv;
  }

  l = (((~0ULL) << (self->value.width - 1)) << 1);
  return uv | l;
}
