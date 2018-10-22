/* ---------------------------------------------------------------------------


  This file is part of the ``cinit'' package of NuSMV version 2.
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
  \author Adapted to NuSMV by Marco Roveri
  \brief Supplies the compile date and version information.

  \todo: Missing description

*/


#if HAVE_CONFIG_H
# include "nusmv-config.h"
#endif

#if defined(_MSC_VER)
#include <stdlib.h>  /* for getenv in MSVC */
#endif

#include "nusmv/core/cinit/cinitInt.h"
#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/
#ifndef NUSMV_PACKAGE_BUILD_DATE

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NUSMV_PACKAGE_BUILD_DATE  "<compile date not supplied>"
#endif

#ifndef NUSMV_PACKAGE_STRING

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NUSMV_PACKAGE_STRING         "NuSMV 2.5.x"
#endif

#if NUSMV_HAVE_SOLVER_ZCHAFF
# define NUSMV_PACKAGE_STRING_POSTFIX " zchaff"
#else
# define NUSMV_PACKAGE_STRING_POSTFIX ""
#endif

#ifndef NUSMV_SHARE_PATH
# ifdef DATADIR
#  define NUSMV_SHARE_PATH DATADIR "/nusmv"
# elif defined(_WIN32) || defined(__CYGWIN__)
#  define NUSMV_SHARE_PATH "./nusmv"
# else
#  define NUSMV_SHARE_PATH "/usr/share/nusmv"
# endif
#endif


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static void cinit_banner_print(FILE* file, boolean is_linked);
static char * DateReadFromDateString(char * datestr);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

char* CInit_NuSMVReadVersion()
{
  int max_len = 20 +
    strlen(NuSMVCore_get_tool_name()) +
    strlen(NuSMVCore_get_tool_version()) +
    strlen(NuSMVCore_get_build_date());

  char* version = ALLOC(char, max_len);
  int c;

  c = snprintf(version, max_len, "%s %s (compiled on %s)",
              NuSMVCore_get_tool_name(), NuSMVCore_get_tool_version(),
              NuSMVCore_get_build_date());
  SNPRINTF_CHECK(c, max_len);
  return version;
}

char* CInit_NuSMVObtainLibrary(void)
{
#if NUSMV_HAVE_GETENV || defined(_MSC_VER)
  const char* tool_path_suffix = "_LIBRARY_PATH";
  char * nusmv_lib_path;
  const char* tool_name = NuSMVCore_get_tool_name();
  char* path = ALLOC(char, strlen(tool_name) + strlen(tool_path_suffix) + 1);
  int i;

  for (i = 0; i < strlen(tool_name); ++i) {
    path[i] = toupper(tool_name[i]);
  }
  path[i] = '\0';

  path = strcat(path, tool_path_suffix);
  nusmv_lib_path = getenv(path);
  FREE(path);

  if (nusmv_lib_path) {
    return util_tilde_expand(nusmv_lib_path);
  }
  else {
    return util_tilde_expand(NUSMV_SHARE_PATH);
  }
#else
  return util_tilde_expand(NUSMV_SHARE_PATH);
#endif
}

void CInit_BannerPrint(FILE * file)
{
  cinit_banner_print(file, false);
}

void CInit_BannerPrintLibrary(FILE * file)
{
  cinit_banner_print(file, true);
}

void CInit_BannerPrint_nusmv_library(FILE * file)
{
  // J-edit: Suppress output
  // fprintf(file, "*** This version of %s is linked to %s %s.\n",
  //         NuSMVCore_get_tool_name(), NuSMVCore_get_library_name(),
  //         NuSMVCore_get_library_version());

  // fprintf(file, "*** For more information on %s see <%s>\n",
  //         NuSMVCore_get_library_name(), NuSMVCore_get_library_website());

  // fprintf(file, "*** or email to <%s>.\n", NuSMVCore_get_library_email());

  // fprintf(file, "*** Copyright (C) 2010-2014, Fondazione Bruno Kessler\n\n");
}

void CInit_BannerPrint_cudd(FILE * file)
{
  // J-edit: Suppress output
  // fprintf(file,
  //         "*** This version of %s is linked to the CUDD library version %s\n",
  //         NuSMVCore_get_tool_name(), CUDD_VERSION);
  // fprintf(file,
  //         "*** Copyright (c) 1995-2004, Regents of the University of Colorado\n\n");

  // fflush(NULL); /* to flush all the banner before any other output */
}

void CInit_BannerPrint_minisat(FILE * file)
{
  // J-edit: Suppress output
  // fprintf(file,
  //         "*** This version of %s is linked to the MiniSat SAT solver. \n",
  //         NuSMVCore_get_tool_name());
  // fprintf(file,
  //         "*** See http://minisat.se/MiniSat.html\n");
  // fprintf(file,
  //         "*** Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson\n"
  //         "*** Copyright (c) 2007-2010, Niklas Sorensson\n\n");

  // fflush(NULL); /* to flush all the banner before any other output */
}

void CInit_BannerPrint_zchaff(FILE * file)
{
  int i;

  fprintf(file,
          "WARNING *** This version of %s is linked to the zchaff SAT",
          NuSMVCore_get_tool_name());
  for (i = 0; i < 13 - strlen(NuSMVCore_get_tool_name()); i++) {
    fprintf(file, " ");
  }
  fprintf(file, " ***\n");
  fprintf(file,"WARNING *** solver (see http://www.princeton.edu/~chaff/zchaff.html). ***\n");
  fprintf(file, "WARNING *** Zchaff is used in Bounded Model Checking when the         ***\n");
  fprintf(file, "WARNING *** system variable \"sat_solver\" is set to \"zchaff\".          ***\n");
  fprintf(file, "WARNING *** Notice that zchaff is for non-commercial purposes only.   ***\n");
  fprintf(file, "WARNING *** NO COMMERCIAL USE OF ZCHAFF IS ALLOWED WITHOUT WRITTEN    ***\n");
  fprintf(file, "WARNING *** PERMISSION FROM PRINCETON UNIVERSITY.                     ***\n");
  fprintf(file, "WARNING *** Please contact Sharad Malik (malik@ee.princeton.edu)      ***\n");
  fprintf(file, "WARNING *** for details.                                              ***\n\n");

  fflush(NULL); /* to flush all the banner before any other output */
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Prints the banner of NuSMV.

  Prints the banner of NuSMV. If is_linked is true,
               also the NuSMV library banner is output
*/
static void cinit_banner_print(FILE* file, boolean is_linked)
{
  // J-edit: Suppress output
  fprintf(file, "*** This is %s %s (compiled on %s)\n",
          NuSMVCore_get_tool_name(),
          NuSMVCore_get_tool_version(),
          NuSMVCore_get_build_date());
# ifdef NUSMV_LINKED_CORE_ADDONS
  /* linked addons are printed only if not empty */
  // J-edit: Suppress output
  // if (strcmp(NuSMVCore_get_linked_addons(), "") != 0) {
  //   fprintf(file, "*** Enabled addons are: %s\n",
  //           NuSMVCore_get_linked_addons());
  // }
#endif
  // J-edit: Suppress output
  // fprintf(file, "*** For more information on %s see <%s>\n",
  //         NuSMVCore_get_tool_name(), NuSMVCore_get_website());

  // fprintf(file, "*** or email to <%s>.\n",
  //         NuSMVCore_get_email());
  // fprintf(file, "*** %s\n\n",
  //         NuSMVCore_get_bug_report_message());

  // fprintf(file, "*** Copyright (c) 2010-2014, Fondazione Bruno Kessler\n\n");

  if (is_linked) {
    CInit_BannerPrint_nusmv_library(file);
  }

  /* Cudd license */
  CInit_BannerPrint_cudd(file);

# if NUSMV_HAVE_SOLVER_MINISAT
  CInit_BannerPrint_minisat(file);
# endif

# if NUSMV_HAVE_SOLVER_ZCHAFF
  CInit_BannerPrint_zchaff(file);
# endif

  fflush(NULL); /* to flush all the banner before any other output */
}

/*!
  \brief Returns the date in a brief format assuming its coming from
  the program `date'.

  optional
*/
static char *
DateReadFromDateString(
  char * datestr)
{
  static char result[25];
  char        day[10];
  char        month[10];
  char        zone[10];
  char       *at;
  int         date;
  int         hour;
  int         minute;
  int         second;
  int         year;
  int c;

  if (sscanf(datestr, "%s %s %2d %2d:%2d:%2d %s %4d",
             day, month, &date, &hour, &minute, &second, zone, &year) == 8) {
    if (hour >= 12) {
      if (hour >= 13) hour -= 12;
      at = "PM";
    }
    else {
      if (hour == 0) hour = 12;
      at = "AM";
    }
    c = snprintf(result, sizeof(result), "%d-%3s-%02d at %d:%02d %s",
                 date, month, year % 100, hour, minute, at);
    SNPRINTF_CHECK(c, sizeof(result));
    return result;
  }
  else {
    return datestr;
  }
}
