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


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/utils/WordNumber_private.h"
#include "nusmv/core/utils/WordNumberMgr.h"

#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/utils/portability.h" /* for LLONG_MAX, errno */
#include "nusmv/core/utils/error.h"

#include <stdlib.h> /* for strtol */
#include "nusmv/core/utils/defs.h"

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

/*!
  \brief initialiser of the class


*/

void WordNumber_init(NuSMVEnv_ptr env)
{
  WordNumberMgr_ptr mgr = WordNumberMgr_create(env);

  NuSMVEnv_set_value(env, ENV_WORD_NUMBER_MGR, mgr);
}

/*!
  \brief deinitialiser of the class


*/

void WordNumber_quit(NuSMVEnv_ptr env)
{
  WordNumberMgr_ptr mgr =
    WORD_NUMBER_MGR(NuSMVEnv_remove_value(env, ENV_WORD_NUMBER_MGR));

  WordNumberMgr_destroy(mgr);
}


/*!
  \brief The functions returns the maximal width a Word constant
  can have. This is implemenatation-dependent limit


*/

int WordNumber_max_width()
{
  return WordNumberMgr_max_width();
}

/*!
  \brief returns the value of a WordNumber, as unsigned word


*/

unsigned long long WordNumber_get_unsigned_value(WordNumber_ptr self)
{
  WORD_NUMBER_CHECK_INSTANCE(self);
  return self->value;
}


/*!
  \brief returns the value of a WordNumber, interpreted as a signed
  word


*/

signed long long WordNumber_get_signed_value(WordNumber_ptr self)
{
  WORD_NUMBER_CHECK_INSTANCE(self);
  return word_number_to_signed_c_value(self);
}


/*!
  \brief returns the status (true or false) of the sign bit


*/

boolean WordNumber_get_sign(WordNumber_ptr self)
{
  WORD_NUMBER_CHECK_INSTANCE(self);
  return WordNumber_get_bit(self, WordNumber_get_width(self)-1);
}


/*!
  \brief returns the width of a WordNumber


*/

int WordNumber_get_width(WordNumber_ptr self)
{
  WORD_NUMBER_CHECK_INSTANCE(self);
  return self->width;
}


/*!
  \brief returns the status (true or false) of a particular bit

  the bit number should be in the range \[0, width-1\].
*/

boolean WordNumber_get_bit(WordNumber_ptr self, int n)
{
  WORD_NUMBER_CHECK_INSTANCE(self);
  nusmv_assert(n >= 0 && n < self->width);
  return (self->value >> n) & 1;
}


/*!
  \brief returns a string which was given
  to WordNumber_from_parsed_string constructor. If the number was created
  by any other constructor, NULL is returned.


*/

const char* WordNumber_get_parsed_string(WordNumber_ptr self)
{
  WORD_NUMBER_CHECK_INSTANCE(self);
  if ((string_ptr)NULL != self->parsedString) {
    return  UStringMgr_get_string_text(self->parsedString);
  }
  return (const char*)NULL;
}

/*!
  \brief prints a Word constant in a provided base.

  returns negative value in a case of error.
  Only 2, 8, 10, 16 bits bases are allowed.
  If base is 10 then isSigned is taken into account, i.e. if it is true then
  the number is ouput as signed word, and as unsigned word otherwise.

  \sa WordNumber_print
*/

int WordNumber_based_print(FILE* output_stream, WordNumber_ptr self, int base,
                           boolean isSigned)
{
  char* result = WordNumber_to_based_string(self, base, isSigned);
  if ((char*)NULL == result) return -1;
  return fprintf(output_stream, "%s", result);
}

/*!
  \brief prints a Word constant in a provided base to a string.

  This function is the same as WordNumber_based_print,
  except this function outputs to a string, not a stream.
  Only 2, 8, 10, 16 bits bases are allowed.
  If base is 10 then isSigned is taken into account, i.e. if it is true then
  the number is ouput as signed word, and as unsigned word otherwise.
  In case of any problem, NULL is returned.

  Note: The returned string belongs to the funcion. Do not modify this
  string.
  Note: The next invocation of this function or WordNumber_to_string
  makes the previously returned string unusable

  \sa WordNumber_based_print, WordNumber_to_string
*/

char* WordNumber_to_based_string(WordNumber_ptr self, int base, boolean isSigned)
{
  static char* buffer = (char*)NULL;
  static int buffer_size = 0;

  int printed;

  WORD_NUMBER_CHECK_INSTANCE(self);

  /* allocate a buffer. The requires string is very likely less than width+20*/
  if (buffer_size < (self->width + 20)) {
    buffer_size = self->width + 20;
    buffer = REALLOC(char, buffer, buffer_size);
  }


  /* print the "0", the base, the width and the value */
  switch (base) {

  case 2: {
    int n = self->width - 1;
    WordNumberValue v = self->value;
    char* str = buffer;

    printed = snprintf(str, buffer_size, "0%sb%i_", isSigned ? "s" : "u", self->width);
    SNPRINTF_CHECK(printed, buffer_size);

    if (printed <= 0) return (char*)NULL; /* error in printing */
    else str += printed;

    /* while (((v>>n)&1) == 0 && n > 0) --n; */ /* skip initial zeros */

    while (n >= 0) {
      printed = snprintf(str, buffer_size, "%u", (int)((v>>n)&1));
      SNPRINTF_CHECK(printed, buffer_size);

      if ( printed <= 0) return (char*)NULL; /* error in printing */
      else str += printed;
      --n;
    }
    nusmv_assert((str - buffer) < buffer_size);
    return buffer;
  }

  case 8:
    printed = snprintf(buffer, buffer_size, "0%so%d_%"PRIoMAX,
                       isSigned ? "s" : "u", self->width, self->value);
    SNPRINTF_CHECK(printed, buffer_size);
    break;
  case 16:
    printed = snprintf(buffer, buffer_size, "0%sh%d_%"PRIXMAX,
                      isSigned ? "s" : "u" , self->width, self->value);
    SNPRINTF_CHECK(printed, buffer_size);
    break;
  case 10: {
    WordNumberValue value = self->value;
    int sign = 0;
    const char* const format = "%s0%sd%d_%"PRIuMAX;

    if (isSigned) { /* the word is signed => check the sign */
      sign = value >> (self->width-1);
      nusmv_assert(0 == sign || 1 == sign); /* one the highest bit only */
      if (sign) { /* get positive number */
        value = -((((~0ULL) >> (self->width-1)) << (self->width-1)) | value);
      }
    }
    printed = snprintf(buffer, buffer_size, format, sign ? "-" : "",
                       isSigned ? "s" : "u", self->width, value);
    SNPRINTF_CHECK(printed, buffer_size);
    break;
  }
  default: error_unreachable_code(); /* unknown base */
  }/* switch */

  if (printed <= 0) return (char*)NULL; /* error in printing */
  nusmv_assert(printed < buffer_size);
  return buffer;
}


/*!
  \brief Checks wether the word is the constant word of
  all bit set to zero


*/

boolean WordNumber_is_zero(WordNumber_ptr v)
{
  WORD_NUMBER_CHECK_INSTANCE(v);

  return (WordNumber_get_unsigned_value(v) == 0LL);
}


/*!
  \brief returns TRUE if operands are equal

  the width of operands should be equal
*/

boolean WordNumber_equal(WordNumber_ptr v1, WordNumber_ptr v2)
{
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  return v1->value == v2->value;
}


/*!
  \brief returns TRUE if operands are NOT equal

  the width of operands should be equal
*/

boolean WordNumber_not_equal(WordNumber_ptr v1, WordNumber_ptr v2)
{
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  return v1->value != v2->value;
}


/*!
  \brief returns TRUE if left operand is less than
  the right one (numbers are considered as unsigned)

  the width of operands should be equal
*/

boolean WordNumber_unsigned_less(WordNumber_ptr v1, WordNumber_ptr v2)
{
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  return v1->value < v2->value;
}


/*!
  \brief returns TRUE if left operand is less than, or equal to,
  the right one (numbers are considered as unsigned)

  the width of operands should be equal
*/

boolean WordNumber_unsigned_less_or_equal(WordNumber_ptr v1, WordNumber_ptr v2)
{
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  return v1->value <= v2->value;
}


/*!
  \brief returns TRUE if left operand is greater than
  the right one (numbers are considered as unsigned)

  the width of operands should be equal
*/

boolean WordNumber_unsigned_greater(WordNumber_ptr v1, WordNumber_ptr v2)
{
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  return v1->value > v2->value;
}


/*!
  \brief returns TRUE if left operand is greate than, or eqaul to,
  the right one (numbers are considered as unsigned)

  the width of operands should be equal
*/

boolean WordNumber_unsigned_greater_or_equal(WordNumber_ptr v1, WordNumber_ptr v2)
{
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  return v1->value >= v2->value;
}


/*!
  \brief returns TRUE if left operand is signed less than
  the right one (numbers are considered as signed)

  the width of operands should be equal
*/

boolean WordNumber_signed_less(WordNumber_ptr v1, WordNumber_ptr v2)
{
  boolean bit1, bit2;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  bit1 = WordNumber_get_bit(v1, v1->width - 1);
  bit2 = WordNumber_get_bit(v2, v1->width - 1);

  return bit1 > bit2 || (bit1 == bit2 && v1->value < v2->value);
}


/*!
  \brief returns TRUE if left operand is signed less than,
  or equal to, the right one (numbers are considered as signed)

  the width of operands should be equal
*/

boolean WordNumber_signed_less_or_equal(WordNumber_ptr v1, WordNumber_ptr v2)
{
  boolean bit1, bit2;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  bit1 = WordNumber_get_bit(v1, v1->width - 1);
  bit2 = WordNumber_get_bit(v2, v1->width - 1);

  return bit1 > bit2 || (bit1 == bit2 && v1->value <= v2->value);
}


/*!
  \brief returns TRUE if left operand is signed greater than
  the right one (numbers are considered as signed)

  the width of operands should be equal
*/

boolean WordNumber_signed_greater(WordNumber_ptr v1, WordNumber_ptr v2)
{
  boolean bit1, bit2;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  bit1 = WordNumber_get_bit(v1, v1->width - 1);
  bit2 = WordNumber_get_bit(v2, v1->width - 1);

  return bit1 < bit2 || (bit1 == bit2 && v1->value > v2->value);
}


/*!
  \brief returns TRUE if left operand is signed greate than,
  or eqaul to, the right one (numbers are considered as signed)

  the width of operands should be equal
*/

boolean
WordNumber_signed_greater_or_equal(WordNumber_ptr v1, WordNumber_ptr v2)
{
  boolean bit1, bit2;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  bit1 = WordNumber_get_bit(v1, v1->width - 1);
  bit2 = WordNumber_get_bit(v2, v1->width - 1);

  return bit1 < bit2 || (bit1 == bit2 && v1->value >= v2->value);
}

WordNumber_ptr WordNumber_max(WordNumber_ptr v1,
                              WordNumber_ptr v2,
                              boolean isSigned)
{
  if (isSigned) {
    return WordNumber_signed_less(v1, v2) ? v2 : v1;
  }
  return WordNumber_unsigned_less(v1, v2) ? v2 : v1;
}

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
  \brief


*/

WordNumberValue
word_number_to_signed_c_value(const WordNumber_ptr self)
{
  WordNumberValue uv = WordNumber_get_unsigned_value(self);
  int sign = uv >> (self->width-1);
  WordNumberValue l;
  nusmv_assert(0 == sign || 1 == sign); /* one the highest bit only */

  if (sign == 0) return uv;
  l = (((~ 0ULL) << (self->width - 1)) << 1);
  return uv | l;
}
