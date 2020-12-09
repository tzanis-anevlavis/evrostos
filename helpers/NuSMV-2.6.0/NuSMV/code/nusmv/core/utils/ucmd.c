/* ---------------------------------------------------------------------------


  This file is part of the ``utils'' package of NuSMV version 2.
  Copyright (C) 1998-2001 by CMU and FBK-irst.

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
  \author Roberto Cavada
  \brief Contains useful function and structure to be used when
  processing command line strings.

  \todo: Missing description

*/

#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#include "nusmv/core/utils/portability.h"
#include "nusmv/core/utils/ucmd.h"
#include "nusmv/core/utils/error.h"
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

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void apply_string_macro_expansion(const SubstString* const subst,
				  char* string, size_t buf_len)
{
  static char szSubst[256];
  char* szSymbolPos = NULL;
  size_t string_len = strlen(string);
  int c = 0;

  nusmv_assert(string_len < buf_len); /* is buffer large enought? */

  /* substitutes all symbols in input string, forall symbols: */
  szSymbolPos = strstr(string, subst->symbol);
  while(szSymbolPos != NULL) {
    /* applies format: */
    switch(subst->value.type) {
    case sv_string:
      c = snprintf(szSubst, sizeof(szSubst), subst->format,
                   subst->value.assign.string);
      break;

    case sv_integer:
      c = snprintf(szSubst, sizeof(szSubst), subst->format,
                   subst->value.assign.integer);
      break;

    case sv_floating:
      c = snprintf(szSubst, sizeof(szSubst), subst->format,
                   subst->value.assign.floating);
      break;

    case sv_undef:
      error_unreachable_code(); /* did you use the SYMBOL_ASSIGN macro? */
      break;

    case sv_pointer:
    default:
      c = snprintf(szSubst, sizeof(szSubst), subst->format,
                   subst->value.assign.pointer);
      break;
    } /* switch end */

    SNPRINTF_CHECK(c, sizeof(szSubst));

    {
      /* now substitutes the symbol with the string: */
      size_t symbol_len = strlen(subst->symbol);
      size_t subst_len  = strlen(szSubst);
      char*  moveTo   = szSymbolPos+subst_len;
      char*  moveFrom = szSymbolPos+symbol_len;

      /* prepare space for subst: */
      memmove( moveTo, moveFrom, string_len - (moveFrom - string) + 1 );
      string_len += (subst_len-symbol_len);

      /* substitutes the symbol :*/
      memcpy(szSymbolPos, szSubst, subst_len);

      /* searches for the following reference to this symbol: */
      szSymbolPos = strstr(szSymbolPos + subst_len, subst->symbol);
    }
  } /* end of search for the single symbol */
}

int util_str2int(const char* str, int* value)
{
  int res = 1;
  char* error_pos = NIL(char);
  long dummy;

  dummy = strtol(str, &error_pos, 10);

  if ( (str != NIL(char)) && (error_pos == NULL ||
                              error_pos == (str+strlen(str)) ||
                              *error_pos == ' ' ||
                              *error_pos == '\t') ) {

    /* conversion has been succesfully carried out */
    res = 0;

    /* as an additional check, verify that the value can fit into an
       ordinary int */
/* warning [MD] on 32 bit, INT_MIN == LONG_MIN, so the test is always false */
#if ! ((INT_MIN == LONG_MIN) && (INT_MAX == LONG_MAX))
    if ((dummy < INT_MIN) || (INT_MAX < dummy)) {
      res = 1;
    }
#endif

    *value = (int)dummy;
  }

  return res;
}

int util_str2int_incr(const char* str, char **endptr, int* out)
{
  long tmp; /* required by strtol */
  int res = 0; /* no error */

  errno = 0;    /* To distinguish success/failure after call */
  tmp = strtol(str, endptr, 10);

  /* Check for various possible errors */

  if ((errno == ERANGE && (tmp == LONG_MAX || tmp == LONG_MIN))
      || (errno != 0 && tmp == 0)) {
    res = 1; goto leave;
  }

  /* as an additional check, verify that the value can fit into an
     ordinary int */
  /* warning [MD] on 32 bit, INT_MIN == LONG_MIN, so the test is always false */
#if ! ((INT_MIN == LONG_MIN) && (INT_MAX == LONG_MAX))
  if ((tmp < INT_MIN) || (INT_MAX < tmp)) {
    res = 1; goto leave;
  }
#endif

  /* here tmp can be safely cast to int */
  (*out) = (int)(tmp);

 leave:
  return res;
} /* util_str2int */

int util_is_string_null(const char* string)
{
  int ret = 0;
  if (string == (char*)NULL) ret = 1;
  else {
    if (strcmp(string, "") == 0) ret = 1;
    else {
      if (strcmp(string, OPT_USER_POV_NULL_STRING) == 0) ret = 1;
    }
  }

  return ret;
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/



/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/



