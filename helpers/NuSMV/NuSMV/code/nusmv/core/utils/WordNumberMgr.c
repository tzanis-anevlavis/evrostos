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
  \author Alessandro Mariotti
  \brief Implementation of class 'WordNumberMgr'

  \todo: Missing description

*/


#include "nusmv/core/utils/WordNumberMgr.h"
#include "nusmv/core/utils/WordNumber_private.h"
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

/*!
  \brief WordNumberMgr class definition


*/

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
                                         WordNumberValue value, int width,
                                         const char* parsedString);

static void word_number_destroy(WordNumber_ptr);
/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief The WordNumberMgr class constructor

  The WordNumberMgr class constructor

  \sa WordNumberMgr_destroy
*/

WordNumberMgr_ptr WordNumberMgr_create(const NuSMVEnv_ptr env)
{
  WordNumberMgr_ptr self = ALLOC(WordNumberMgr, 1);
  WORD_NUMBER_MGR_CHECK_INSTANCE(self);

  word_number_mgr_init(self, env);
  return self;
}


/*!
  \brief The WordNumberMgr class destructor

  The WordNumberMgr class destructor

  \sa WordNumberMgr_create
*/

void WordNumberMgr_destroy(WordNumberMgr_ptr self)
{
  WORD_NUMBER_MGR_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}



/*!
  \brief The functions returns the maximal width a Word constant
  can have. This is implemenatation-dependent limit


*/

int WordNumberMgr_max_width(void)
{
  /* compute the maximal size of Word constants we can handle */
  if (0 == wordNumberMaxWidth) {/* the constant has not been initialized*/
    int i = 1;
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
    if (sizeof(int) * 2 * CHAR_BIT < wordNumberMaxWidth) {
      wordNumberMaxWidth = sizeof(int) * 2 * CHAR_BIT;
    }
  }

  return wordNumberMaxWidth;
}


/*!
  \brief returns a maximal value of unsigned word of given width


*/

WordNumber_ptr WordNumberMgr_max_unsigned_value(WordNumberMgr_ptr self,
                                                int width)
{
  unsigned long long maxuv;

  /* implementation limit */
  nusmv_assert(width > 0 && width <= WordNumberMgr_max_width());

  /* max unsigned value of given width consists of 1 at
   * positions[width-1:0] */
  maxuv = (~ 0ULL) >> (WordNumberMgr_max_width() - width);
  return word_number_create(self, maxuv, width, NULL);
}


/*!
  \brief returns a maximal value of signed word of given width.


*/

WordNumber_ptr WordNumberMgr_max_signed_value(WordNumberMgr_ptr self,
                                              int width)
{
  unsigned long long maxsv;

  /* implementation limit */
  nusmv_assert(width > 0 && width <= WordNumberMgr_max_width());
  /* max signed value of given width is 1 at positions [width-2:0]
     and 0 everything else */
  maxsv = ((~ 0ULL) >> (WordNumberMgr_max_width() - width) >> 1);
  return word_number_create(self, maxsv, width, NULL);
}


/*!
  \brief returns a minimal value of signed word of given width.


*/

WordNumber_ptr  WordNumberMgr_min_signed_value(WordNumberMgr_ptr self,
                                               int width)
{
  /* min signed value of given width consists of 0 at positions[width-1:0]
     and 1 everthing else */
  unsigned long long minsv = (1ULL) << (width - 1);
  return word_number_create(self, minsv, width, NULL);
}


/*!
  \brief Constructs a Word number WordNumber_ptr from the string
  representation

  The string and base should be proper for standard
  "strtoull" function.  The base can be 2, 8 or 16.  In the case of
  any problem NULL is returned.

  Note: base 10 is not allowed, because it does not provide enough info
  about the width of the Word number.

  NOTE: Memory sharing is used, i.e. given a string with the same
  value of WordNumber this constructor will return the same pointer
  (this is important for node_ptr hashing)
*/

WordNumber_ptr WordNumberMgr_string_to_word_number(WordNumberMgr_ptr self,
                                                   char* str, int base)
{
  WordNumberValue value;
  int width;
  char* tmpStr;

#if NUSMV_HAVE_ERRNO_H
  errno = 0;
#endif

  value = strtoull(str, &tmpStr, base);

  /* an error happened */
  if (NULL == str ||

#if NUSMV_HAVE_ERRNO_H
      /* proper errno usage from the strtoull manpage */
      (((ERANGE == errno && (LLONG_MAX == value)) ||
        (0 != errno && 0 == value))) ||
#endif
      '\0' != *tmpStr) {

    return WORD_NUMBER(NULL);
  }

  /* calculate the width */
  width = tmpStr - str;
  switch (base) {
  case 2:  /* nothing */ break;
  case 8:  width *= 3; break;
  case 16: width *= 4; break;
  default: error_unreachable_code(); /* only 2,8 and 16 bits base are allowed */
  }

  return word_number_create(self, value, width, NULL);
}


/*!
  \brief Constructs a Word number WordNumber_ptr from the string
  representation

  The string and base should be proper for standard
  "strtoull" function. The base can be 2, 8, 10 or 16. The number
  should be in the range supposed by the width. The provided width of
  the constant should be enough to hold the obtained number. In the
  case of any problem NULL is returned.

  NOTE: Memory sharing is used, i.e. given a string with the same
  value of WordNumber this constructor will return the same pointer
  (this is important for node_ptr hashing)
*/

WordNumber_ptr WordNumberMgr_sized_string_to_word_number(
    WordNumberMgr_ptr self,
    char* str, int base,
    int width)
{
  WordNumberValue value;
  char* tmpStr;

  /* only this bases are allowed here */
  nusmv_assert(2 == base || 8 == base || 10 == base || 16 == base);

#if NUSMV_HAVE_ERRNO_H
  errno = 0;
#endif
  value = strtoull(str, &tmpStr, base);

  /* an error happened */
  if (NULL == str ||

#if NUSMV_HAVE_ERRNO_H
      /* proper errno usage from the strtoull manpage */
      ((ERANGE == errno && (LLONG_MAX == value))
       || (0 != errno && 0 == value)) ||
#endif
      '\0' != *tmpStr) {
    return WORD_NUMBER(NULL);
  }

  /* the width is the number of digit multiplied by the base */
  return word_number_create(self, value, width, NULL);
}


/*!
  \brief Constructs a Word number WordNumber_ptr from the string
  representation obtained during parsing

  The string is the string obtained during parsing. The
  string should correspond to the NuSMV lexer token "word constants",
  i.e. "0" character followed by the base, optional signed specifier,
  optional width (decimal number), "_" character and the value
  (binary, octal, decimal or hexadecimal number).  The base and the
  digits should correspond each other.

  The limit for width is implementation dependant.
  In the case of any problem NULL is returned, and if errorString is not NULL,
  it is set to a text string explaining the cause of the error.
  The returned error-string belongs to this function (it may change during next
  function invocation).

  NOTE: this constructor is NOT memory shared, i.e. given the same
  string twice different pointers may be returned. (Actually the
  returned pointers may be the same but different from
  pointers returned by memory shared constructors.)
*/

WordNumber_ptr WordNumberMgr_parsed_string_to_word_number(
    WordNumberMgr_ptr self,
    char* str,
    char** errorString)
{
  WordNumberValue value;
  long width;
  int base;
  boolean isSigned;

  char* currentStr;
  char* tmpStr;

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
  default: error_unreachable_code();/* something wrong with the base specifier */
    /* this error should be impossible => assertion */
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
      if (NULL != errorString) {
        *errorString = "overflow or underflow in the width specifier of a Word constant";
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

  if (bufferSize < strlen(currentStr)) { /* reallocate the buffer */
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
      if (NULL != errorString) {
        *errorString = "decimal Word constant without width specifier";
      }
      return WORD_NUMBER(NULL);
    case 16:width *= 4; break;
    default: error_unreachable_code(); /* impossible error */
    } /* switch */
  }
  /* check the wellformedness of the width of the Word */
  if (width <= 0 || width > WordNumberMgr_max_width()) {
    if (NULL != errorString) {
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
  value = strtoull(buffer, &tmpStr, base);

  /* error : value overflow or underflow*/
#if NUSMV_HAVE_ERRNO_H /* proper errno usage from the strtoull manpage */
  if  (((ERANGE == errno && (LLONG_MAX == value)))
       || (0 != errno && 0 == value)) {

    if (NULL != errorString) {
      *errorString = "overflow or underflow in the value of a Word constant";
    }
    return WORD_NUMBER(NULL);
  }
#endif

  /* error in the value */
  if (*tmpStr != '\0') {
    if (NULL != errorString) {
      int i = snprintf(err_buf, err_buf_size,
                       "erroneous character '%c' in a Word constant value",
                       *tmpStr);
      SNPRINTF_CHECK(i, err_buf_size); /* above print was successful */
      *errorString = err_buf;
    }
    return WORD_NUMBER(NULL);
  }

  /* here two shifts are performed because shift by the width of
     the type in C is illegal
  */
  if ( ((value >> (width-1)) >> 1) != 0) {
    if (NULL != errorString) {
      int i = snprintf(err_buf, err_buf_size,
                       "value of a Word constant %s is outside of its width",
                       str);
      SNPRINTF_CHECK(i, err_buf_size); /* above print was successful */
      *errorString = err_buf;
    }
    return WORD_NUMBER(NULL);
  }
  /* for signed decimal words values have to be in range 0 .. 2^(width-1) */
  if (isSigned && 10 == base && value > (1ULL << (width-1))) {
    /* NOTE: To represent negative constants value 2^(width-1) was
       allowed though positive constant cannot have such values.
       2^(width-1) == -2^(width-1) holds in this case. */
    if (NULL != errorString) {
      int i = snprintf(err_buf, err_buf_size,
                       "value of a decimal Signed Word constant %s is outside of its width",
                       str);
      SNPRINTF_CHECK(i, err_buf_size); /* above print was successful */
      *errorString = err_buf;
    }
    return WORD_NUMBER(NULL);
  }

  if (NULL != errorString) { /* potential error in the constructor */
    *errorString = "undefined error with a Word constant";
  }

  return word_number_create(self, value, width, str);
}



/*!
  \brief returns a WordNumber

  value and width should be correct, i.e. in a proper
  range. See WordNumber_from_signed_integer if original value is signed.

  NOTE: Memory sharing is used, i.e. given the same parameter this
  constructor will return the same pointer (this is important for
  node_ptr hashing)

  \sa WordNumber_from_signed_integer
*/

WordNumber_ptr WordNumberMgr_integer_to_word_number(WordNumberMgr_ptr self,
                                                    WordNumberValue value,
                                                    int width)
{
  return word_number_create(self, value, width, NULL);
}


/*!
  \brief returns a WordNumber

   This constructor is the same as
  WordNumber_from_integer except than value is interpreted as signed
  value casted to WordNumberValue.

  The difference is that signed negative value casted to WordNumberValue
  will have 1s at positions greater than width. These bits are ignored
  but in WordNumber_from_integer they cause assertion violation.

  For originally positive values both constructors behave the same.


  \sa WordNumber_from_integer
*/

WordNumber_ptr WordNumberMgr_signed_integer_to_word_number(WordNumberMgr_ptr self,
                                                           WordNumberValue value,
                                                           int width)
{
  WordNumberValue new_value;

  /* implementation limit */
  nusmv_assert(width > 0 && width <= WordNumberMgr_max_width());

  /* simply put 0s at positions [max-width-1, width] */
  new_value = ((~ 0ULL) >> (WordNumberMgr_max_width() - width)) & value;

  /* DEBUGGING CODE:
     the value is representable with given width iff
     either all bits [max-width-1, width-1] are 0 (for positive signed values)
     or all are 1 (for negative signed values).
  */
  nusmv_assert((value >> (width-1)) == 0 ||
               (value >> (width-1)) == ((~ 0ULL) >> (width-1)));

  return word_number_create(self, new_value, width, NULL);
}


/*!
  \brief returns a memory shared WordNumber

  If a word number was created with a constructor
  WordNumberMgr_from_parsed_string then such a WordNumber is NOT memory
  shared.
  This function takes such WordNumber and returns its memory shared
  analog, i.e. with the same width and value but without string
  information.
  If WordNumber was created with memory-shared constructor (for
  example, WordNumber_from_integer), there is no need to use this
  function since the returned value will be the same as input
*/

WordNumber_ptr WordNumberMgr_normalize_word_number(WordNumberMgr_ptr self,
                                                   const WordNumber_ptr number)
{
  /* Here NULL was passed, now passing the parsed string too, if
     any. Fix for issue 2220 */
  return word_number_create(self, WordNumber_get_unsigned_value(number),
                            WordNumber_get_width(number),
                            (NULL != number->parsedString ?
                             UStringMgr_get_string_text(number->parsedString)
                             : NULL));
}


/* ========================================================================= */
/* ARITHMETIC OPERATIONS                                                     */
/* ========================================================================= */

/*!
  \brief perform the negation operation


*/

WordNumber_ptr WordNumberMgr_unary_minus(WordNumberMgr_ptr self,
                                         WordNumber_ptr v)
{
  WordNumberValue l;
  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v);

  /* create a constant of 'width' number of 1 bits.
     The left shifts are used because in C shift by a full width is not allowed
  */
  l = ~ (((~ 0ULL) << (v->width - 1)) << 1);

  return word_number_create(self, (- v->value)&l, v->width, NULL);
}


/*!
  \brief perform summation operation

  the width of operands should be equal
*/

WordNumber_ptr WordNumberMgr_plus(WordNumberMgr_ptr self,
                                  WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue l;

  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  /* create a constant of 'width' number of 1 bits.
     The left shifts are used because in C shift by a full width is not allowed
  */
  l = ~ (((~ 0ULL) << (v1->width - 1)) << 1);

  return word_number_create(self, (v1->value + v2->value)&l,
                            v1->width, NULL);
}


/*!
  \brief perform subtraction operation on Words

  the width of operands should be equal
*/

WordNumber_ptr WordNumberMgr_minus(WordNumberMgr_ptr self,
                                   WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue l;

  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  /* create a constant of 'width' number of 1 bits.
     The left shifts are used because in C shift by a full width is not allowed
  */
  l = ~ (((~ 0ULL) << (v1->width - 1)) << 1);

  return word_number_create(self, (v1->value - v2->value)&l,
                            v1->width, NULL);
}


/*!
  \brief perform multiplidation operation on Words

  the width of operands should be equal
*/

WordNumber_ptr WordNumberMgr_times(WordNumberMgr_ptr self,
                                   WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue l;

  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);

  /* create a constant of 'width' number of 1 bits.
     The left shifts are used because in C shift by a full width is not allowed
  */
  l = ~ (((~ 0ULL) << (v1->width - 1)) << 1);

  nusmv_assert(v1->width == v2->width);
  return word_number_create(self, (v1->value * v2->value)&l,
                            v1->width, NULL);
}


/*!
  \brief perform unsigned division operation on Words

  the width of operands should be equal. The
  right operand should not be 0.
*/

WordNumber_ptr WordNumberMgr_unsigned_divide(WordNumberMgr_ptr self,
                                             WordNumber_ptr v1,
                                             WordNumber_ptr v2)
{
  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  nusmv_assert(0 != v2->value);
  /* division is rounded DOWN (as in evaluation procedure).
   Since 'value' is unsigned, there is no need to worry about it.
  */
  return word_number_create(self, v1->value / v2->value, v1->width, NULL);
}


/*!
  \brief perform signed division operation on Words

  the width of operands should be equal. The
  right operand should not be 0
*/

WordNumber_ptr WordNumberMgr_signed_divide(WordNumberMgr_ptr self,
                                           WordNumber_ptr v1,  WordNumber_ptr v2)
{
  signed long long int _v1;
  signed long long int _v2;
  signed long long int _res;
  WordNumberValue l;

  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);
  nusmv_assert(0 != v2->value);

  _v1 = word_number_to_signed_c_value(v1);
  _v2 = word_number_to_signed_c_value(v2);
  _res = (_v1 / _v2);

  l = ~ (((~ 0ULL) << (v1->width - 1)) << 1);

  return word_number_create(self, _res & l, v1->width, NULL);
}


/*!
  \brief perform remainder unsigned operation on Words

  the width of operands should be equal. The right
  operand should not be 0.
  Note: numbers are considered as unsigned.
*/

WordNumber_ptr WordNumberMgr_unsigned_mod(WordNumberMgr_ptr self,
                                          WordNumber_ptr v1, WordNumber_ptr v2)
{
  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  nusmv_assert(0 != v2->value);
  /* C's division is rounded towards 0, but NuSMV rounds down.
   Since 'value' is unsigned, there is no need to worry about it.
  */
  return word_number_create(self, v1->value % v2->value, v1->width, NULL);
}


/*!
  \brief perform remainder signed operation on Words

  the width of operands should be equal. The right
  operand should not be 0
*/

WordNumber_ptr WordNumberMgr_signed_mod(WordNumberMgr_ptr self,
                                        WordNumber_ptr v1,  WordNumber_ptr v2)
{
  signed long long int _v1;
  signed long long int _v2;
  signed long long int _res;
  WordNumberValue l;

  WORD_NUMBER_MGR_CHECK_INSTANCE(self);
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);
  nusmv_assert(0 != v2->value);

  _v1 = word_number_to_signed_c_value(v1);
  _v2 = word_number_to_signed_c_value(v2);
  _res = (_v1 % _v2);

  l = ~ (((~ 0ULL) << (v1->width - 1)) << 1);

  return word_number_create(self, _res & l, v1->width, NULL);
}



/*!
  \brief returns bitwise NOT of a Word number


*/

WordNumber_ptr WordNumberMgr_not(WordNumberMgr_ptr self,
                                 WordNumber_ptr v)
{
  WordNumberValue l;
  WORD_NUMBER_CHECK_INSTANCE(v);

  /* create a constant of 'width' number of 1 bits.
     The left shifts are used because in C shift by a full width is not allowed
  */
  l = ~ (((~ 0ULL) << (v->width - 1)) << 1);

  return word_number_create(self, (~ v->value) & l, v->width, NULL);
}


/*!
  \brief returns bitwise AND of two Word numbers

  the width of operands should be equal
*/

WordNumber_ptr WordNumberMgr_and(WordNumberMgr_ptr self,
                                 WordNumber_ptr v1, WordNumber_ptr v2)
{
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  return word_number_create(self, v1->value & v2->value, v1->width, NULL);
}


/*!
  \brief returns bitwise OR of two Word numbers

  the width of operands should be equal
*/

WordNumber_ptr WordNumberMgr_or(WordNumberMgr_ptr self,
                                WordNumber_ptr v1, WordNumber_ptr v2)
{
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  return word_number_create(self, v1->value | v2->value, v1->width, NULL);
}


/*!
  \brief returns bitwise XOR of two Word numbers

  the width of operands should be equal
*/

WordNumber_ptr WordNumberMgr_xor(WordNumberMgr_ptr self,
                                 WordNumber_ptr v1, WordNumber_ptr v2)
{
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  return word_number_create(self, v1->value ^ v2->value, v1->width, NULL);
}


/*!
  \brief returns bitwise XNOR(or IFF) of two Word numbers

  the width of operands should be equal
*/

WordNumber_ptr WordNumberMgr_xnor(WordNumberMgr_ptr self,
                                  WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue l;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  /* create a constant of 'width' number of 1 bits.
     The left shifts are used because in C shift by a full width is not allowed
  */
  l = ~ (((~ 0ULL) << (v1->width - 1)) << 1);

  return word_number_create(self, (~ (v1->value ^ v2->value)) & l,
                            v1->width, NULL);
}


/*!
  \brief returns bitwise IMPLIES of two Word numbers

  the width of operands should be equal
*/

WordNumber_ptr WordNumberMgr_implies(WordNumberMgr_ptr self,
                                     WordNumber_ptr v1, WordNumber_ptr v2)
{
  WordNumberValue l;

  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width == v2->width);

  /* create a constant of 'width' number of 1 bits.
     The left shifts are used because in C shift by a full width is not allowed
  */
  l = ~ (((~ 0ULL) << (v1->width - 1)) << 1);

  return word_number_create(self, ((~ v1->value) | v2->value) & l,
                            v1->width, NULL);
}


/*!
  \brief returns bitwise IFF(or XNOR) of two Word numbers

  the width of operands should be equal
*/

WordNumber_ptr WordNumberMgr_iff(WordNumberMgr_ptr self,
                                 WordNumber_ptr v1, WordNumber_ptr v2)
{
  return WordNumberMgr_xnor(self, v1, v2);
}


/*!
  \brief returns a concatenation of two Word numbers


*/

WordNumber_ptr WordNumberMgr_concatenate(WordNumberMgr_ptr self,
                                         WordNumber_ptr v1, WordNumber_ptr v2)
{
  WORD_NUMBER_CHECK_INSTANCE(v1);
  WORD_NUMBER_CHECK_INSTANCE(v2);
  nusmv_assert(v1->width + v2->width <= WordNumberMgr_max_width());

  return word_number_create(self, (v1->value << v2->width) | v2->value,
                            v1->width + v2->width,  NULL);
}


/*!
  \brief returns a Word number consisting of the
  bits [highBit .. lowBit] from a given Word number

  highBit should be less than the Word width and greater or
  equal to lowBit. lowBit should be greater or equal to 0.
*/

WordNumber_ptr WordNumberMgr_bit_select(WordNumberMgr_ptr self,
                                        WordNumber_ptr v, int highBit, int lowBit)
{
  WordNumberValue l;
  int newWidth = highBit - lowBit + 1;

  WORD_NUMBER_CHECK_INSTANCE(v);
  nusmv_assert(v->width > highBit && highBit >= lowBit && lowBit >= 0);

  /* create a constant of 'width' number of 1 bits.
     Two left shift are used because in C shift by a full width is not allowed
  */
  l = ~ (((~ 0ULL) << (newWidth - 1)) << 1);

  return word_number_create(self, (v->value >> lowBit) & l, newWidth, NULL);
}


/*!
  \brief perform right shift on a Word numbers

  the number of shifted bits should be in the range
  \[0, width\]. The word is padded with zeros.
*/

WordNumber_ptr
WordNumberMgr_unsigned_right_shift(WordNumberMgr_ptr self,
                                   WordNumber_ptr v, int numberOfBits)
{
  WORD_NUMBER_CHECK_INSTANCE(v);
  nusmv_assert(v->width >= numberOfBits && numberOfBits >= 0);

  return word_number_create(self, (v->value >> (numberOfBits-1)) >> 1,
                            v->width, NULL);
}


/*!
  \brief perform right shift on a Word numbers

  the number of shifted bits should be in the range
  \[0, width\]. The word is padded with zeros.
*/

WordNumber_ptr
WordNumberMgr_signed_right_shift(WordNumberMgr_ptr self,
                                 WordNumber_ptr v, int numberOfBits)
{
  WordNumberValue l;
  WORD_NUMBER_CHECK_INSTANCE(v);
  nusmv_assert(v->width >= numberOfBits && numberOfBits >= 0);

  if (numberOfBits == v->width) {
    numberOfBits -= 1;
  }

  /* prepares a mask for sign bits if sign is set */
  if (((1ULL << (v->width - 1)) & v->value) != 0) {
    l = (~((~0ULL) << (numberOfBits))) << (v->width - numberOfBits);
  }
  else l = 0; /* no sign bit */

  return word_number_create(self, (v->value >> numberOfBits) | l,
                            v->width, NULL);
}


/*!
  \brief perform left shift on a Word numbers

  the number of shifted bits should be in the range
  \[0, width\]. The word is padded with zeros.
*/

WordNumber_ptr WordNumberMgr_left_shift(WordNumberMgr_ptr self,
                                        WordNumber_ptr v, int numberOfBits)
{
  WordNumberValue l;

  WORD_NUMBER_CHECK_INSTANCE(v);
  nusmv_assert(v->width >= numberOfBits && numberOfBits >= 0);

  if (v->width == numberOfBits) {
    return word_number_create(self, 0, v->width, NULL);
  }

  /* create a constant of 'width' number of 1 bits.  The left
     shifts are used because in C shift by a full width is not
     allowed */
  l = ~ (((~ 0ULL) << (v->width - 1)) << 1);

  return word_number_create(self, (v->value << numberOfBits) & l,
                            v->width, NULL);
}


/*!
  \brief perform right rotate on a Word numbers

  the number of rotated bits should be in the range
  \[0, width\].
*/

WordNumber_ptr WordNumberMgr_right_rotate(WordNumberMgr_ptr self,
                                          WordNumber_ptr v, int numberOfBits)
{
  WordNumberValue l;

  WORD_NUMBER_CHECK_INSTANCE(v);
  nusmv_assert(v->width >= numberOfBits && numberOfBits >= 0);

  if (v->width == numberOfBits) {
    return word_number_create(self, v->value, v->width, NULL);
  }

  /* create a constant of 'width' number of 1 bits.  The left
     shifts are used because in C shift by a full width is not
     allowed */
  l = ~ (((~ 0ULL) << (v->width - 1)) << 1);

  return word_number_create(self, ( (v->value >> numberOfBits)
                            | (v->value << (v->width - numberOfBits))
                            ) & l,
                            v->width, NULL);
}


/*!
  \brief perform left rotate on a Word numbers

  the number of rotated bits should be in the range
  \[0, width\].
*/

WordNumber_ptr WordNumberMgr_left_rotate(WordNumberMgr_ptr self,
                                         WordNumber_ptr v, int numberOfBits)
{
  WordNumberValue l;

  WORD_NUMBER_CHECK_INSTANCE(v);
  nusmv_assert(v->width >= numberOfBits && numberOfBits >= 0);

  if (v->width == numberOfBits) {
    return word_number_create(self, v->value, v->width, NULL);
  }

  /* create a constant of 'width' number of 1 bits.  The left
     shifts are used because in C shift by a full width is not
     allowed */
  l = ~ (((~ 0ULL) << (v->width - 1)) << 1);

  return word_number_create(self, ( (v->value << numberOfBits)
                            | (v->value >> (v->width - numberOfBits))
                            ) & l,
                            v->width, NULL);
}


/*!
  \brief performs sign extend, i.e. concatenates 'numberOfTimes'
  number of times the highest bit of v with v.


*/

WordNumber_ptr WordNumberMgr_signed_extend(WordNumberMgr_ptr self,
                                           WordNumber_ptr v, int numberOfTimes)
{
  WordNumberValue highestBit;
  WordNumberValue value;
  int newWidth;

  WORD_NUMBER_CHECK_INSTANCE(v);
  nusmv_assert(v->width + numberOfTimes <= WordNumberMgr_max_width());

  /* optimisation */
  if (0 == numberOfTimes) return v;

  highestBit = WordNumber_get_bit(v, v->width - 1);
  highestBit <<= v->width;

  newWidth = v->width + numberOfTimes;

  for (value = v->value; numberOfTimes > 0; --numberOfTimes) {
    value |= highestBit;
    highestBit <<= 1;
  }

  return word_number_create(self, value, newWidth, NULL);
}


/*!
  \brief performs unsign extend


*/

WordNumber_ptr WordNumberMgr_unsigned_extend(WordNumberMgr_ptr self,
                                             WordNumber_ptr v, int numberOfTimes)
{
  return word_number_create(self, WordNumber_get_unsigned_value(v),
                            WordNumber_get_width(v)+numberOfTimes,
                            NULL);
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
static void word_number_mgr_init(WordNumberMgr_ptr self, const NuSMVEnv_ptr env)
{
  env_object_init(ENV_OBJECT(self), env);

  /* members initialization */
  self->hashTable= new_assoc();
  self->nodeMgr = NodeMgr_create(env);
  self->ustrMgr =  UStringMgr_create();

  OVERRIDE(Object, finalize) = word_number_mgr_finalize;
}


/* hash table cleaner for the  WordNumberMgr_quit */
static enum st_retval word_number_mgr_hashTableCleaner(char* key,
                                                       char* record, char* arg)
{
  WordNumber_ptr number = WORD_NUMBER(record);
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
  The function creates a copy of the parameter parsedString.
*/
static WordNumber_ptr word_number_create(WordNumberMgr_ptr self,
                                         WordNumberValue value, int width,
                                         const char* parsedString)
{
  WordNumber_ptr word = (WordNumber_ptr)NULL;
  node_ptr key = (node_ptr)NULL;
  string_ptr ps = (string_ptr)NULL;

  /* implementation limit */
  nusmv_assert(width > 0 && width <= WordNumberMgr_max_width());

  /* the number is out of the range for a given width.

     NB: Two shifts are performed since shift by the full width isn't
     allowed in C
  */
  nusmv_assert(((value >> (width - 1)) >> 1) == 0);

  /* there is a string from parser => we use the string as key, as to
     allow printing the same model read, otherwise we use the width
     and value */
  if (NULL != parsedString) {
    ps =  UStringMgr_find_string(self->ustrMgr, parsedString);
    key = (node_ptr)ps;
  }
  else {
    key = NodeMgr_find_node(self->nodeMgr, width,
                    NODE_FROM_INT((int)value),
                    NODE_FROM_INT((int)(value>>(WordNumberMgr_max_width()/2))));
  }

  word = WORD_NUMBER(find_assoc(self->hashTable, key));

  if (WORD_NUMBER(NULL) != word) return word;

  word = ALLOC(WordNumber, 1);
  if (WORD_NUMBER(NULL) == word) return WORD_NUMBER(NULL);

  word->value = value;
  word->width = width;
  word->parsedString = ps;

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
  FREE(word);
}



/**AutomaticEnd***************************************************************/
