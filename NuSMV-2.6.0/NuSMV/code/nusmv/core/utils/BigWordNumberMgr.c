/* ---------------------------------------------------------------------------


  This file is part of the ``utils'' package of NuSMV version 2.
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
  \author \todo: Missing author
  \brief Implementation of class 'WordNumberMgr'

  \todo: Missing description

*/


#include "nusmv/core/utils/WordNumberMgr.h"
#include "nusmv/core/utils/WordNumber.h"
#include "nusmv/core/utils/BigWordNumber_private.h"

#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/UStringMgr.h"
#include "nusmv/core/utils/portability.h" /* for LLONG_MAX, errno */
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/EnvObject.h"
#include "nusmv/core/utils/EnvObject_private.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct WordNumberMgr_TAG
{
  INHERITS_FROM(EnvObject);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  /*
    - For internally generated word constants

        node_ptr(width, value(half_1), value(half_2)) -> -> WordNumber_ptr

    - For parsed word constants, to allow printing with the same
      format as parsed

          string_ptr("0bN_....") -> -> WordNumber_ptr
  */
  hash_ptr hashTable;
  NodeMgr_ptr nodeMgr;
  UStringMgr_ptr ustrMgr;
} WordNumberMgr;


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

static int wordNumberMaxWidth = 0;

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void word_number_mgr_finalize(Object_ptr object, void* dummy);

static void word_number_mgr_init(WordNumberMgr_ptr self,
                                 const NuSMVEnv_ptr env);
static void word_number_mgr_deinit(WordNumberMgr_ptr self);

static WordNumber_ptr word_number_create(WordNumberMgr_ptr self,
                                         WordNumberValue_intern value,
                                         int width,
                                         const char* parsedString);

static void word_number_destroy(WordNumber_ptr);
/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

WordNumberMgr_ptr WordNumberMgr_create(const NuSMVEnv_ptr env)
{
  WordNumberMgr_ptr self = ALLOC(WordNumberMgr, 1);
  WORD_NUMBER_MGR_CHECK_INSTANCE(self);

  word_number_mgr_init(self, env);
  return self;
}

void WordNumberMgr_destroy(WordNumberMgr_ptr self)
{
  WORD_NUMBER_MGR_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}

int WordNumberMgr_max_width()
{
  return INT_MAX;
}

/*!
  \brief The functions returns the maximal width a Word constant
  can have if it is to be stored in a unsigned long long.
  This is implemenatation-dependent limit


*/

static int BigWordNumberMgr_max_ull_width(void)
{
  int calculated_size = 0;
  int i = 1;
  /* compute the maximal size of unsigned long long vales we can handle */
  if (0 == wordNumberMaxWidth) {/* the constant has not been initialized*/
    WordNumberValue ull = 1;
    while ((ull << 1) > ull) {
      ull = ull << 1;
      ++i;
    }
    wordNumberMaxWidth = i; /* max number of bits in "unsigned long long" */

    /* two int must be enough to hold one Word constant (this is
     used in caching, see word_number_create). So, if required,
     decrease the number of bits to make it possible.
     */
    calculated_size = sizeof(int) * 2 * CHAR_BIT;
    if (calculated_size < wordNumberMaxWidth) {
      wordNumberMaxWidth = sizeof(int) * 2 * CHAR_BIT;
    }
  }

  return wordNumberMaxWidth;
}

WordNumber_ptr WordNumberMgr_max_unsigned_value(WordNumberMgr_ptr self,
                                                 int width)
{
  /* max unsigned value of given width consists of 1 at positions[width-1:0] */
  unsigned long long maxuv = 0;
  Number number;
  WordNumberValue_intern value;

  /* implementation limit */
  nusmv_assert(width > 0 && width <= BigWordNumberMgr_max_ull_width());

  maxuv = (~0ULL) >> (BigWordNumberMgr_max_ull_width() - width);
  number = BigNumber_make_number_from_unsigned_long_long(maxuv);
  value = WordNumber_create_WordNumberValue_intern(number, width);

  return word_number_create(self, value, width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_max_signed_value(WordNumberMgr_ptr self,
                                               int width)
{
  unsigned long long maxsv = 0;
  Number number;
  WordNumberValue_intern value;

  /* implementation limit */
  nusmv_assert(width > 0 && width <= BigWordNumberMgr_max_ull_width());
  /* max signed value of given width is 1 at positions [width-2:0]
     and 0 everything else */

  maxsv = ((~ 0ULL) >> (BigWordNumberMgr_max_ull_width() - width) >> 1);
  number = BigNumber_make_number_from_unsigned_long_long(maxsv);
  value = WordNumber_create_WordNumberValue_intern(number, width);

  return word_number_create(self, value, width, (char*)NULL);
}

WordNumber_ptr WordNumberMgr_min_signed_value(WordNumberMgr_ptr self,
                                              int width)
{
  /* min signed value of given width consists of 0 at positions[width-1:0]
     and 1 everthing else */
  unsigned long long minsv = (1ULL) << (width - 1);
  Number number = BigNumber_make_number_from_unsigned_long_long(minsv);
  WordNumberValue_intern value;
  value = WordNumber_create_WordNumberValue_intern(number, width);

  return word_number_create(self, value, width, (char*)NULL);
}

WordNumber_ptr WordNumberMgr_string_to_word_number(WordNumberMgr_ptr self,
                                                   char* str, int base)
{
  WordNumberValue_intern value;
  int width;
  char* tmpStr = (char*) NULL;
  int succes;

  /* value = strtoull(str, &tmpStr, base); */
  Number number = BigNumber_make_number_from_unsigned_long_long(0);
  succes = BigNumber_assign_number_from_string(str, tmpStr, base, &number);

#if NUSMV_HAVE_ERRNO_H
  errno = 0;
#endif

  /* an error happened */
  if ((NULL == (char*) str) || !succes ) {
    /* Bas: This error value cannot be displayed */
    return WORD_NUMBER(NULL);
  }

  /* calculate the width */
  width = strlen(str);
  switch (base) {
  case 2:  /* nothing */ break;
  case 8:  width *= 3; break;
  case 16: width *= 4; break;
  /* only 2,8 and 16 bits base are allowed */
  default: error_unreachable_code();
  }

  value = WordNumber_create_WordNumberValue_intern(number, width);
  return word_number_create(self, value, width, (char*)NULL);
}

WordNumber_ptr
WordNumberMgr_sized_string_to_word_number(WordNumberMgr_ptr self,
                                          char* str, int base,
                                                         int width)
{
  WordNumberValue_intern value;
  Number number;
  char* tmpStr = (char*) NULL;
  int succes ;

  /* only this bases are allowed here */
  nusmv_assert(2 == base || 8 == base || 10 == base || 16 == base);

#if NUSMV_HAVE_ERRNO_H
  errno = 0;
#endif
  /* value = strtoull(str, &tmpStr, base); */
  number = BigNumber_make_number_from_unsigned_long_long(0);
  succes = BigNumber_assign_number_from_string(str, tmpStr, base, &number);

  /* an error happened */
  if ((NULL == (char* ) str) || !succes) {
    return WORD_NUMBER(NULL);
  }
  value = WordNumber_create_WordNumberValue_intern(number, width);
  /* the width is the number of digit multiplied by the base */
  return word_number_create(self, value, width, (char*)NULL);
}

WordNumber_ptr
WordNumberMgr_parsed_string_to_word_number(WordNumberMgr_ptr self,
                                           char* str,
                                           char** errorString)
{
  WordNumberValue_intern value;
  Number pow2;
  Number number;
  long width;
  int base;
  int succes;
  boolean isSigned;

  char* currentStr = (char*) NULL;
  char* tmpStr = (char*) NULL;
  char* error_char = (char*) NULL;

  /* buffer for error messages. 200 chars should be enough to
     reprsent any number and any error message
  */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define err_buf_size  200
  static char err_buf[err_buf_size];

  /* the buffer is used to get rid of '_' from the string */
  static char* buffer = (char*)NULL;

  static int bufferSize = 0;

  nusmv_assert((char*)NULL != str);
  currentStr = str;

  /* 1. remove the first "0", check check whether the number signed or not
     and get the base specifier */
  nusmv_assert('0' == *currentStr);/*first character of Word constant is "0"*/
  ++currentStr;

  if ('s' == *currentStr) {
    isSigned = true;
    ++currentStr;
  }
  else {
    isSigned = false;
    if ('u' == *currentStr) ++currentStr;
  }


  switch (toupper(*currentStr)) {
  case 'B': base = 2;  break;
  case 'O': base = 8;  break;
  case 'D': base = 10; break;
  case 'H': base = 16; break;
  /* something wrong with the base specifier */
  /* this error should be impossible => assertion */
  default: error_unreachable_code();
  } /* switch */
  ++currentStr; /* get past the base specifier */


  /* 2. calculate the explicit width of the Word constant*/
  if ('_' != *currentStr) { /* the width is explicitly specified */
    nusmv_assert(isdigit(*currentStr)); /* only a digit can go here */

#if NUSMV_HAVE_ERRNO_H
    errno = 0;
#endif

    width = strtol(currentStr, &tmpStr, 10);

#if NUSMV_HAVE_ERRNO_H /* proper errno usage from the strtol manpage */
    if ((ERANGE == errno && (LONG_MAX == width || LONG_MIN == width))
        || (0 != errno && 0 == width)) {
#else
    /* error : width specifier overflow or underflow*/
    if (width != (int) width) {
#endif
      if (NULL != (char*) errorString) {
        char* c = (char*) NULL;
        c = "overflow or underflow in the width specifier of a Word constant";
        *errorString = c;
      }
      return WORD_NUMBER(NULL);
    }

    /* error in the width specifier */
    if ('_' != *tmpStr) { /* 'underscore must go after the width specifier */
      error_unreachable_code();/* in current implementation this code
                             is impossible */
      if (NULL != errorString) {
        int i = snprintf(err_buf, err_buf_size,
                         "erroneous character '%c' in the width specifier "
                         "of a Word constant", *tmpStr);
        SNPRINTF_CHECK(i, err_buf_size); /* above print was successful */
        *errorString = err_buf;
      }
      return WORD_NUMBER(NULL);
    }

    /* 2.1 move past the optional width specifier */
    for ( ; isdigit(*currentStr); ++currentStr) {}
  }
  else width = -1; /* for sure incorrect value */


  /* 3. copy the value string into a buffer and remove '_' from the string */
  nusmv_assert('_' == *currentStr); /* underscore before the value part */

  if (bufferSize < ((int) strlen(currentStr))) { /* reallocate the buffer */
    bufferSize = strlen(currentStr);
    buffer = REALLOC(char, buffer, bufferSize);
  }

  for (tmpStr = buffer; '\0' != *currentStr; ++currentStr) {
    if ('_' != *currentStr) {
      *tmpStr = *currentStr; /* = toupper(*str); not needed at the moment */
      ++tmpStr;
    }
  } /* for */
  *tmpStr = '\0';

  /* 4. calculate the implicit width of the Word constant */
  if (-1 == width) {/* there was no width specifier => calculate it */
    /* calculate the number of digits */
    width = strlen(buffer);

    switch (base) {
    case 2: /* nothing */ break;
    case 8: width *= 3; break;
    case 10:  /* error */
      if ((char**) NULL !=  errorString) {
        *errorString = "decimal Word constant without width specifier";
      }
      return WORD_NUMBER(NULL);
    case 16:width *= 4; break;
    default: error_unreachable_code(); /* impossible error */
    } /* switch */
  }
  /* check the wellformedness of the width of the Word */
  if (width <= 0 || width > WordNumberMgr_max_width()) {
    if ((char**) NULL != errorString) {
      int i = snprintf(err_buf, err_buf_size,
                       "width of a Word constant is out of range 1..%i",
                       WordNumberMgr_max_width());
      SNPRINTF_CHECK(i, err_buf_size); /* above print was successful */
      *errorString = err_buf;
    }
    return WORD_NUMBER(NULL);
  }

  /* 5. calculate the value */
#if NUSMV_HAVE_ERRNO_H
  errno = 0;
#endif
  number = BigNumber_make_number_from_long(0);
  succes = BigNumber_assign_number_from_string(buffer,
                                               error_char,
                                               base,
                                               &number);
  /* error in the value */
  if (!succes) {
    if (((char*) NULL) != error_char) {
       /* TODO(Bas): We cannot print the car yet*/
      int i = snprintf(err_buf, err_buf_size,
                       "erroneous character '%c' in a Word constant "
                       "value (BigNumChar is Undef)",
                       *error_char);
      SNPRINTF_CHECK(i, err_buf_size);  /* above print was successful */
      *errorString = err_buf;
    }
    return WORD_NUMBER(NULL);
  }

  /* here two shifts are performed because shift by the width of
     the type in C is illegal
  */
  if (!BigNumber_does_integer_fit_into_number_of_bits(&number, width)) {
    if ((char**) NULL != errorString) {
      int i = snprintf(err_buf, err_buf_size,
                       "value of a Word constant %s is outside of its width",
                       str);
      SNPRINTF_CHECK(i, err_buf_size);  /* above print was successful */
      *errorString = err_buf;
    }
    return WORD_NUMBER(NULL);
  }

  /* for signed decimal words values have to be in range 0 .. 2^(width-1) */
  pow2 = BigNumber_pow2(width - 1);
  if (isSigned
      && 10 == base
      && BigNumber_less_than(&pow2, &number)) {
    /* NOTE: To represent negative constants value 2^(width-1) was
       allowed though positive constant cannot have such values.
       2^(width-1) == -2^(width-1) holds in this case. */
    BigNumber_free_number(&(pow2));
    if ((char**) NULL != errorString) {
      int i = snprintf(err_buf, err_buf_size,
                       "value of a decimal Signed Word constant "
                       "%s is outside of its width",
                       str);
      SNPRINTF_CHECK(i, err_buf_size); /* above print was successful */
      *errorString = err_buf;
    }
    return WORD_NUMBER(NULL);
  }
  BigNumber_free_number(&(pow2));

  if ((char**) NULL != errorString) { /* potential error in the constructor */
    *errorString = "undefined error with a Word constant";
  }

  value = WordNumber_create_WordNumberValue_intern(number, width);
  return word_number_create(self, value, width, (char*)NULL);
}

WordNumber_ptr WordNumberMgr_integer_to_word_number(WordNumberMgr_ptr self,
                                                    unsigned long long value,
                                                    int width)
{
  Number number = BigNumber_make_number_from_unsigned_long_long(value);
  WordNumberValue_intern wnv;
  wnv = WordNumber_create_WordNumberValue_intern(number, width);

  return word_number_create(self, wnv, width, (char*)NULL);
}

WordNumber_ptr
WordNumberMgr_signed_integer_to_word_number(WordNumberMgr_ptr self,
                                            WordNumberValue value,
                                            int width)
{
  Number n;
  WordNumberValue new_value;
  WordNumberValue_intern nvalue;

  /* implementation limit */
  nusmv_assert(width > 0 && width <= BigWordNumberMgr_max_ull_width());

  /* simply put 0s at positions [max-width-1, width] */

  new_value = ((~ 0ULL) >> (BigWordNumberMgr_max_ull_width() - width)) & value;

  /* DEBUGGING CODE:
     the value is representable with given width iff
     either all bits [max-width-1, width-1] are 0 (for positive signed values)
     or all are 1 (for negative signed values).
  */
  nusmv_assert((value >> (width-1)) == 0 ||
               (value >> (width-1)) == ((~ 0ULL) >> (width-1)));

  n = BigNumber_make_number_from_unsigned_long_long(new_value);
  nvalue = WordNumber_create_WordNumberValue_intern(n, width);

  return word_number_create(self, nvalue, width, (char*)NULL);
}

WordNumber_ptr WordNumberMgr_normalize_word_number(WordNumberMgr_ptr self,
                                                   const WordNumber_ptr number)
{
  return number;
}


/* ========================================================================= */
/* ARITHMETIC OPERATIONS                                                     */
/* ========================================================================= */

WordNumber_ptr WordNumberMgr_unary_minus(WordNumberMgr_ptr self,
                                         WordNumber_ptr v)
{
  WordNumberValue_intern newv;

  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v);
  newv = WordNumber_evaluate_unary_minus(v->value);

  return word_number_create(self, newv, v->value.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_plus(WordNumberMgr_ptr self,
                                  WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue_intern newv;

  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  newv = WordNumber_evaluate_plus(v1->value, v2->value);

  return word_number_create(self, newv, v1->value.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_minus(WordNumberMgr_ptr self,
                                   WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue_intern newv;

  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  newv = WordNumber_evaluate_minus(v1->value, v2->value);

  return word_number_create(self, newv, v2->value.width, (char*)NULL);
}

WordNumber_ptr WordNumberMgr_times(WordNumberMgr_ptr self,
                                   WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue_intern newv;

  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);

  newv= WordNumber_evaluate_mul(v1->value, v2->value);

  return word_number_create(self, newv, v2->value.width, (char*)NULL);
}

WordNumber_ptr WordNumberMgr_unsigned_divide(WordNumberMgr_ptr self,
                                             WordNumber_ptr v1,
                                             WordNumber_ptr v2)
{
  WordNumberValue_intern newv;

  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  newv = WordNumber_evaluate_udiv(v1->value, v2->value);

  return word_number_create(self, newv, v2->value.width, (char*)NULL);
}

WordNumber_ptr WordNumberMgr_signed_divide(WordNumberMgr_ptr self,
                                           WordNumber_ptr v1,
                                           WordNumber_ptr v2)
{
  WordNumberValue_intern newv;

  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  newv = WordNumber_evaluate_sdiv(v1->value, v2->value);

  return word_number_create(self, newv, v2->value.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_unsigned_mod(WordNumberMgr_ptr self,
                                          WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue_intern newv;

  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  newv = WordNumber_evaluate_urem(v1->value, v2->value);

  return word_number_create(self, newv, v2->value.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_signed_mod(WordNumberMgr_ptr self,
                                        WordNumber_ptr v1,  WordNumber_ptr v2)
{
  WordNumberValue_intern newv;

  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  newv = WordNumber_evaluate_srem(v1->value, v2->value);

  return word_number_create(self, newv, v2->value.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_not(WordNumberMgr_ptr self,
                                 WordNumber_ptr v)
{
  WordNumberValue_intern newv;
  WORD_NUMBER_CHECK_INSTANCE(v);

  newv = WordNumber_evaluate_not(v->value);

  return word_number_create(self, newv, v->value.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_and(WordNumberMgr_ptr self,
                                 WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue_intern newv;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  newv = WordNumber_evaluate_and(v1->value, v2->value);

  return word_number_create(self, newv, v1->value.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_or(WordNumberMgr_ptr self,
                                WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue_intern newv;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  newv = WordNumber_evaluate_or(v1->value, v2->value);

  return word_number_create(self, newv, v1->value.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_xor(WordNumberMgr_ptr self,
                                 WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue_intern newv;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  newv = WordNumber_evaluate_xor(v1->value, v2->value);

  return word_number_create(self, newv, v1->value.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_xnor(WordNumberMgr_ptr self,
                                  WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue_intern newv;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  newv = WordNumber_evaluate_xnor(v1->value, v2->value);

  return word_number_create(self, newv, v1->value.width, (char*) NULL);

}

WordNumber_ptr WordNumberMgr_implies(WordNumberMgr_ptr self,
                                     WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue_intern newv;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width == v2->value.width);

  newv = WordNumber_evaluate_implies(v1->value,
                                                            v2->value);

  return word_number_create(self, newv, v1->value.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_iff(WordNumberMgr_ptr self,
                                 WordNumber_ptr v1, WordNumber_ptr v2)
{
  return WordNumberMgr_xnor(self, v1, v2);
}

WordNumber_ptr WordNumberMgr_concatenate(WordNumberMgr_ptr self,
                                         WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue_intern nvalue;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->value.width + v2->value.width <= WordNumberMgr_max_width());

  nvalue = WordNumber_evaluate_concat(v1->value, v2->value);

  return word_number_create(self,
                            nvalue,
                            v1->value.width + v2->value.width,
                            (char*) NULL);
}

WordNumber_ptr WordNumberMgr_bit_select(WordNumberMgr_ptr self,
                                        WordNumber_ptr v,
                                        int highBit, int lowBit)
{
  WordNumberValue_intern nvalue;

  WORD_NUMBER_CHECK_INSTANCE(v);
  nusmv_assert(v->value.width > highBit && highBit >= lowBit && lowBit >= 0);

  nvalue = WordNumber_evaluate_select(v->value, highBit, lowBit);

  return word_number_create(self, nvalue, nvalue.width, (char*) NULL);
}

WordNumber_ptr
WordNumberMgr_unsigned_right_shift(WordNumberMgr_ptr self,
                                   WordNumber_ptr v,
                                   int numberOfBits)
{
  WordNumberValue_intern nvalue;

  WORD_NUMBER_CHECK_INSTANCE(v);
  nusmv_assert(v->value.width >= numberOfBits && numberOfBits >= 0);

  nvalue = WordNumber_evaluate_uright_shift(v->value, numberOfBits);

  return word_number_create(self, nvalue, v->value.width, (char*) NULL);
}

WordNumber_ptr
WordNumberMgr_signed_right_shift(WordNumberMgr_ptr self,
                                 WordNumber_ptr v,
                                 int numberOfBits)
{
  WordNumberValue_intern nvalue;

  WORD_NUMBER_CHECK_INSTANCE(v);
  nusmv_assert(v->value.width >= numberOfBits && numberOfBits >= 0);

  nvalue = WordNumber_evaluate_sright_shift(v->value, numberOfBits);

  return word_number_create(self, nvalue, v->value.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_left_shift(WordNumberMgr_ptr self,
                                        WordNumber_ptr v,
                                        int numberOfBits)
{
  WordNumberValue_intern nvalue;

  WORD_NUMBER_CHECK_INSTANCE(v);
  nusmv_assert(v->value.width >= numberOfBits && numberOfBits >= 0);

  nvalue = WordNumber_evaluate_left_shift(v->value, numberOfBits);

  return word_number_create(self, nvalue, v->value.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_right_rotate(WordNumberMgr_ptr self,
                                          WordNumber_ptr v, int numberOfBits)
{
  WordNumberValue_intern nvalue;

  WORD_NUMBER_CHECK_INSTANCE(v);
  nusmv_assert(v->value.width >= numberOfBits && numberOfBits >= 0);

  nvalue = WordNumber_evaluate_right_rotate(v->value, numberOfBits);

  return word_number_create(self, nvalue, v->value.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_left_rotate(WordNumberMgr_ptr self,
                                         WordNumber_ptr v,
                                         int numberOfBits)
{
  WordNumberValue_intern nvalue;

  WORD_NUMBER_CHECK_INSTANCE(v);
  nusmv_assert(v->value.width >= numberOfBits && numberOfBits >= 0);

  nvalue = WordNumber_evaluate_left_rotate(v->value, numberOfBits);

  return word_number_create(self, nvalue, v->value.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_signed_extend(WordNumberMgr_ptr self,
                                           WordNumber_ptr v,
                                           int numberOfTimes)
{
  WordNumberValue_intern nvalue;
  WORD_NUMBER_CHECK_INSTANCE(v);
  nusmv_assert(v->value.width + numberOfTimes <= WordNumberMgr_max_width());

  nvalue = WordNumber_evaluate_signed_extend(v->value, numberOfTimes);

  return word_number_create(self, nvalue, nvalue.width, (char*) NULL);
}

WordNumber_ptr WordNumberMgr_unsigned_extend(WordNumberMgr_ptr self,
                                             WordNumber_ptr v,
                                             int numberOfTimes)
{
  WordNumberValue_intern nvalue;
  nvalue = WordNumber_evaluate_unsigned_extend(v->value, numberOfTimes);

  return word_number_create(self, nvalue , nvalue.width, (char*) NULL);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The WordNumberMgr class private initializer

  The WordNumberMgr class private initializer

  \sa WordNumberMgr_create
*/
static void word_number_mgr_init(WordNumberMgr_ptr self,
                                 const NuSMVEnv_ptr env)
{
  env_object_init(ENV_OBJECT(self), env);

  /* members initialization */
  self->hashTable = new_assoc();
  self->nodeMgr = NodeMgr_create(env);
  self->ustrMgr = UStringMgr_create();

  OVERRIDE(Object, finalize) = word_number_mgr_finalize;
}


/* hash table cleaner for the  WordNumberMgr_quit */
static enum st_retval word_number_mgr_hashTableCleaner(char* key,
                                                       char* record,
                                                       char* arg)
{
  /*
   * Method is inherited from BigWordNumber, I choose not to refactor this
   * part of the code.
   */
  WordNumber_ptr number = WORD_NUMBER(record);
  UNUSED_PARAM(key);
  UNUSED_PARAM(arg);
  nusmv_assert(WORD_NUMBER(NULL) != number);
  word_number_destroy(number);
  return ST_DELETE;
}

/*!
  \brief The WordNumberMgr class virtual destroyer



  \sa WordNumberMgr_destroy
*/
static void word_number_mgr_finalize(Object_ptr object, void* dummy)
{
  WordNumberMgr_ptr self = WORD_NUMBER_MGR(object);
  UNUSED_PARAM(dummy);

  word_number_mgr_deinit(self);

  FREE(self);
}

/*!
  \brief The WordNumberMgr class private deinitializer

  The WordNumberMgr class private deinitializer

  \sa WordNumberMgr_destroy
*/
static void word_number_mgr_deinit(WordNumberMgr_ptr self)
{
  /* members deinitialization */
  clear_assoc_and_free_entries(self->hashTable,
                               word_number_mgr_hashTableCleaner);
  free_assoc(self->hashTable);

  NodeMgr_destroy(self->nodeMgr);
   UStringMgr_destroy(self->ustrMgr);
}

/*!
  \brief The constructor of WordNumber_ptr. Also adds the created
  number to the memory manager

  In case of any errors, returns NULL.
  This function can be invoked only by constructor-converter functions.
*/
static WordNumber_ptr word_number_create(WordNumberMgr_ptr self,
                                         WordNumberValue_intern value,
                                         int width,
                                         const char* parsedString)
{
  WordNumber_ptr word = (WordNumber_ptr)NULL;
  node_ptr key = (node_ptr)NULL;
  /* string_ptr ps = (string_ptr)NULL; */
  char *key_value = (char*) NULL;
  UNUSED_PARAM(parsedString);

  /* implementation limit */
  nusmv_assert(width > 0 && width <= WordNumberMgr_max_width());

  key_value = WordNumber_Internal_value_to_based_string(value, 16, false);

  key = (node_ptr) UStringMgr_find_string(self->ustrMgr, key_value);
  nusmv_assert(key != ((node_ptr) NULL));

  word = WORD_NUMBER(find_assoc(self->hashTable, key));

  if (WORD_NUMBER(NULL) != word) {
    /*
     * We return the original stored WordNumber_ptr so we can delete
     * the other value if it is supurflous.
    */
    if (!BigNumber_identity(&(value.dat), &(word->value.dat))) {
      BigNumber_free_number(&(value.dat));
    }
    return word;
  }

  word = ALLOC(WordNumber, 1);
  if (WORD_NUMBER(NULL) == word) {
    return WORD_NUMBER(NULL);
  }

  word->value = value;
  /* word->parsedString = ps; */

  insert_assoc(self->hashTable, key, (node_ptr)word);

  return word;
}



/*!
  \brief Destructor of a WordNumber_ptr

  Destructor can be invoked only by the class
  deinitializer, when all the WordNumber_ptr numbers are destroyed.
*/

void word_number_destroy(WordNumber_ptr word)
{
  BigNumber_free_number(&(word->value.dat));
  FREE(word);
}



/**AutomaticEnd***************************************************************/
