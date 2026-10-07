/* ---------------------------------------------------------------------------


  This file is part of the ``bmc'' package of NuSMV version 2.
  Copyright (C) 2004 by FBK-irst.

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
  \author Roberto Cavada, Marco Roveri
  \brief Dumping functionalities, like dimacs and others

  This module supplies services that dump a Bmc problem
  into a file, in DIMACS format and others

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include <errno.h>

#include "nusmv/core/bmc/bmcDump.h"
#include "nusmv/core/bmc/bmcInt.h"
#include "nusmv/core/bmc/bmcUtils.h"

#include "nusmv/core/cinit/cinit.h"
#include "nusmv/core/prop/Prop.h"
#include "nusmv/core/utils/ucmd.h" /* for SubstString */
#include "nusmv/core/utils/defs.h" /* for PRIuPTR */
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

static void
bmc_dump_expandFilename(const NuSMVEnv_ptr env,
                        const int k, const int l,
                        const int prop_idx,
                        const char* filename_to_be_expanded,
                        char* filename_expanded,
                        const size_t filename_expanded_maxlen);

static int
bmc_dump_openDimacsFile(const NuSMVEnv_ptr env,
                        const char* filename, FILE** file_ref);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Bmc_Dump_WriteProblem(const BeEnc_ptr be_enc,
                           const Be_Cnf_ptr cnf,
                           Prop_ptr prop,
                           const int k, const int loop,
                           const Bmc_DumpType dump_type,
                           const char* dump_fname_template)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  char dumpFilenameExpanded[BMC_DUMP_FILENAME_MAXLEN];

  if (dump_type == BMC_DUMP_NONE) return;

  nusmv_assert(dump_fname_template != (char*) NULL);

  /* 10 here is the maximum length of extension */
  bmc_dump_expandFilename(env, k, loop,
                          Prop_get_index(prop),
                          dump_fname_template,
                          dumpFilenameExpanded,
                          sizeof(dumpFilenameExpanded)-10);

  switch (dump_type) {

  case BMC_DUMP_DIMACS:

    strcat(dumpFilenameExpanded, ".dimacs");

    if (Prop_get_type(prop) == Prop_Invar) {
      Bmc_Dump_DimacsInvarProblemFilename(be_enc, cnf, dumpFilenameExpanded);
    }
    else {
      Bmc_Dump_DimacsProblemFilename(be_enc, cnf, dumpFilenameExpanded, k);
    }
    break;

  case BMC_DUMP_DA_VINCI:
    {
      FILE* davinci_file = NULL;

      strcat(dumpFilenameExpanded, ".davinci");

      if (opt_verbose_level_gt(opts, 1)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "\nOpening file '%s' for writing...\n",
                dumpFilenameExpanded);
      }
      davinci_file = fopen(dumpFilenameExpanded, "w");
      if (davinci_file == (FILE*) NULL) {
        int errsv = errno;
        StreamMgr_print_output(streams, 
                "\n*************    WARNING    *************"
                "\n An error has occurred when writing the file \"%s\"."
                "\n (error was '%s')"
                "\n DA VINCI dumping aborted."
                "\n*************  END WARNING  *************\n\n",
                dumpFilenameExpanded, strerror(errsv));
        break;
      }

      Be_DumpDavinci(BeEnc_get_be_manager(be_enc),
                     Be_Cnf_GetOriginalProblem(cnf),
                     davinci_file);

      if (opt_verbose_level_gt(opts, 1)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger,
               dumpFilenameExpanded);
      }

      fclose(davinci_file);
      break;
    }

  case BMC_DUMP_GDL:
    {
      FILE* gdl_file = NULL;

      strcat(dumpFilenameExpanded, ".gdl");

      if (opt_verbose_level_gt(opts, 1)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, "\nOpening file '%s' for writing...\n",
                dumpFilenameExpanded);
      }
      gdl_file = fopen(dumpFilenameExpanded, "w");
      if (gdl_file == (FILE*) NULL) {
        int errsv = errno;
        StreamMgr_print_output(streams, 
                "\n*************    WARNING    *************"
                "\n An error has occurred when writing the file \"%s\"."
                "\n (error was '%s')"
                "\n GDL dumping aborted."
                "\n*************  END WARNING  *************\n\n",
                dumpFilenameExpanded, strerror(errsv));
        break;
      }

      Be_DumpGdl(BeEnc_get_be_manager(be_enc),
                 Be_Cnf_GetOriginalProblem(cnf),
                 gdl_file);
      if (opt_verbose_level_gt(opts, 1)) {
        Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
        Logger_log(logger, dumpFilenameExpanded);
      }

      fclose(gdl_file);
      break;
    }

  default:
    ErrorMgr_internal_error(errmgr, "Bmc_DumpProblem: Unexpected value in dump_type");
  }
}

int Bmc_Dump_DimacsInvarProblemFilename(const BeEnc_ptr be_enc,
                                        const Be_Cnf_ptr cnf,
                                        const char* filename)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));

  FILE* file;
  int ret = bmc_dump_openDimacsFile(env, filename, &file);

  if (ret == 0) {
    Bmc_Dump_DimacsInvarProblem(be_enc, cnf, file);
    fclose(file);
  }
  return ret;
}

int Bmc_Dump_DimacsProblemFilename(const BeEnc_ptr be_enc,
                                   const Be_Cnf_ptr cnf,
                                   const char* filename,
                                   const int k)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));

  FILE* file;
  int ret = bmc_dump_openDimacsFile(env, filename, &file);

  if (ret == 0) {
    Bmc_Dump_DimacsProblem(be_enc, cnf, k, file);
    fclose(file);
  }
  return ret;
}

void Bmc_Dump_DimacsInvarProblem(const BeEnc_ptr be_enc,
                                 const Be_Cnf_ptr cnf,
                                 FILE* dimacsfile)
{
  Bmc_Dump_DimacsProblem(be_enc, cnf, 1, dimacsfile);
}

void Bmc_Dump_DimacsProblem(const BeEnc_ptr be_enc,
                            const Be_Cnf_ptr cnf,
                            const int k,
                            FILE* dimacsfile)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  int time;
  Be_Manager_ptr be_mgr;

  nusmv_assert(dimacsfile != NULL);

  be_mgr = BeEnc_get_be_manager(be_enc);

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger,
            "Dumping problem to Dimacs file (problem length is %d)\n", k);
  }

  /* Writes the readable mapping table as a comment: */
  fprintf(dimacsfile, "c BMC problem generated by %s\n",
          NuSMVCore_get_tool_name());
  fprintf(dimacsfile,
          "c Time steps from 0 to %d, %d State Variables,"
          " %d Frozen Variables and %d Input Variables\n",
          k,
          BeEnc_get_state_vars_num(be_enc),
          BeEnc_get_frozen_vars_num(be_enc),
          BeEnc_get_input_vars_num(be_enc));

  fprintf(dimacsfile, "c Model to Dimacs Conversion Table\n");

  for (time = 0; time <= k; ++time) {
    int iter;
    fprintf(dimacsfile, "c \nc @@@@@ Time %d\n", time);

    iter = BeEnc_get_first_untimed_var_index(be_enc,
                                             BE_VAR_TYPE_CURR |
                                             BE_VAR_TYPE_FROZEN |
                                             BE_VAR_TYPE_INPUT);
    while (BeEnc_is_var_index_valid(be_enc, iter)) {
      /* to avoid dumping of input variables at time k and
         frozen variable at time > 0 (since such variables do not exist) */
      if (! ( (BeEnc_is_index_input_var(be_enc, iter) && time == k)
            ||(BeEnc_is_index_frozen_var(be_enc, iter) && time > 0))) {
        int cnf_index;

        cnf_index = Be_BeIndex2CnfLiteral(be_mgr,
                BeEnc_var_to_index(be_enc,
                                   BeEnc_index_to_timed(be_enc, iter, time)));

        if (cnf_index != 0) {
          /* it is a cnf index of a real variable */
          fprintf(dimacsfile, "c CNF variable %d => Time %d, Model Variable ",
                  cnf_index, time);
          print_node(wffprint, dimacsfile, BeEnc_index_to_name(be_enc, iter));
          fprintf(dimacsfile, "\n");
        }
      }

      iter = BeEnc_get_next_var_index(be_enc, iter,
                      BE_VAR_TYPE_CURR | BE_VAR_TYPE_FROZEN | BE_VAR_TYPE_INPUT);
    }
  } /* time cycle */
  fprintf(dimacsfile, "c \n");

  /* Actually writes the dimacs data: */
  {
    int * cl = (int *)NULL;
    Siter  genCl, genLit;
    nusmv_ptrint lit = 0;

    fprintf(dimacsfile, "c Beginning of the DIMACS dumping\n");
    /* Prints the model variables as a "special" comment line: */
    fprintf(dimacsfile, "c model %" PRIuPTR "\n", Be_Cnf_GetVarsNumber(cnf));
    fprintf(dimacsfile, "c ");

    SLIST_FOREACH(Be_Cnf_GetVarsList(cnf), genLit) {
      lit = (nusmv_ptrint) Siter_element(genLit);
      fprintf(dimacsfile, "%" PRIdPTR " ", lit);
    }
    fprintf(dimacsfile, "0\n");

    /* print the clauses with the literal responsible for the polarity of
       the formula.  This may be changed in future! */
    if (Be_Cnf_GetFormulaLiteral(cnf) == INT_MAX) {
      /* the formula is a constant. see Be_Cnf_ptr for more detail */
      /* check whether the constant value is true or false (see Be_Cnf_ptr) */
      if (0 == Slist_get_size(Be_Cnf_GetClausesList(cnf))) {
        fprintf(dimacsfile, "p cnf %d 0\n", Be_Cnf_GetMaxVarIndex(cnf));
        /* the constand is true => just output a comment */
        fprintf(dimacsfile, "c Warning: the true constant is printed out\n");
      }
      else {
        /* the constant is false => output a comment and a false formula */
        fprintf(dimacsfile, "p cnf %d 2\n", Be_Cnf_GetMaxVarIndex(cnf));
        fprintf(dimacsfile, "c Warning: the false constant is printed out\n");
        fprintf(dimacsfile,"1 0\n-1 0\n"); /* this is always false */
      }
    }
    else { /* the formula is a usual formula, output its formula literal */
      /* Prints the problem header. */
      fprintf(dimacsfile, "p cnf %d %" PRIuPTR "\n",
              Be_Cnf_GetMaxVarIndex(cnf),
              Be_Cnf_GetClausesNumber(cnf)+1); /* 1 is the formula literal */

      fprintf(dimacsfile, "%d 0\n", Be_Cnf_GetFormulaLiteral(cnf));

      /* Prints the clauses. */
      SLIST_FOREACH(Be_Cnf_GetClausesList(cnf), genCl) {
        int i = 0;

        cl = (int*) Siter_element(genCl);
        while (cl[i] != 0) {
          lit = (nusmv_ptrint)cl[i];
          fprintf(dimacsfile, "%" PRIdPTR " ", lit);
          i++;
        }
        fprintf(dimacsfile, "0\n");
      }
    }
    fprintf(dimacsfile, "c End of dimacs dumping\n");
  }

  if (opt_verbose_level_gt(opts, 0)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "End of dump.\n");
  }
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Open a file named filename and returns its descriptor

  The file is opened with the writable attribute. The
  file descriptor is returned via the file_ref parameter. Returns 0 if the
  function succedeed, otherwise the function prints out a warning in the
  standard output and returns 1.

  \se file_ref will change
*/
static int bmc_dump_openDimacsFile(const NuSMVEnv_ptr env,
                                   const char* filename, FILE** file_ref)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int ret = 0;
  if (opt_verbose_level_gt(opts, 1)) {
    Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    Logger_log(logger, "\nOpening file '%s' for writing...\n", filename);
  }

  *file_ref = fopen(filename, "w");
  if ((*file_ref) == NULL)  {
    int errsv = errno;
    StreamMgr_print_output(streams, 
            "\n*************    WARNING    *************"
            "\n An error has occurred when writing the file \"%s\"."
            "\n (error was '%s')"
            "\n DIMACS dumping aborted."
            "\n*************  END WARNING  *************\n\n",
            filename, strerror(errsv));
    ret = 1;
  }

  return ret;
}

/*!
  \brief This is only a useful wrapper for easily call
  Bmc_Utils_ExpandMacrosInFilename

  

  \se None
*/
static void
bmc_dump_expandFilename(const NuSMVEnv_ptr env,
                        const int k, const int l,
                        const int prop_idx,
                        const char* filename_to_be_expanded,
                        char* filename_expanded,
                        const size_t filename_expanded_maxlen)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  char szBuffer[1024];
  char szLoopback[16];

  /* Prepares the structure for macro-expansion: */
  SubstString aSubstTable[] =  {
    SYMBOL_CREATE(), /* 0 */
    SYMBOL_CREATE(), /* 1 */
    SYMBOL_CREATE(), /* 2 */
    SYMBOL_CREATE(), /* 3 */
    SYMBOL_CREATE(), /* 4 */
    SYMBOL_CREATE(), /* 5 */
    SYMBOL_CREATE()  /* 6 */
  };

  /* customizes the table with runtime values: */
  Utils_StripPathNoExtension(get_input_file(opts), szBuffer);
  Bmc_Utils_ConvertLoopFromInteger(l, szLoopback, sizeof(szLoopback));

  /* this to protect @@ (see last rule) */
  SYMBOL_ASSIGN(aSubstTable[0], "@@", string,  "%s", "\1");

  SYMBOL_ASSIGN(aSubstTable[1], "@F", string,  "%s", get_input_file(opts));
  SYMBOL_ASSIGN(aSubstTable[2], "@f", string,  "%s", szBuffer);
  SYMBOL_ASSIGN(aSubstTable[3], "@k", integer, "%d", k);
  SYMBOL_ASSIGN(aSubstTable[4], "@l", string, "%s", szLoopback);
  if (prop_idx != BMC_NO_PROPERTY_INDEX) {
    SYMBOL_ASSIGN(aSubstTable[5], "@n", integer, "%d", prop_idx);
  }
  else {
    SYMBOL_ASSIGN(aSubstTable[5], "@n", string, "%s", "undef");
  }

  /* this to restore @@ as @ */
  SYMBOL_ASSIGN(aSubstTable[6], "\1", string,  "%s", "@");

  Bmc_Utils_ExpandMacrosInFilename(filename_to_be_expanded,
                                   aSubstTable,
                                   sizeof(aSubstTable)/sizeof(aSubstTable[0]),
                                   filename_expanded,
                                   filename_expanded_maxlen);
}
