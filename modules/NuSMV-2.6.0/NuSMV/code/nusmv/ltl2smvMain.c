/* ---------------------------------------------------------------------------


  This file is part of the ``ltl2smv'' package of NuSMV version 2.
  Copyright (C) 1998-2005 by CMU and FBK-irst.

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
  \author Adapted to NuSMV by Marco Roveri.
               Extended to the past operators by Ariel Fuxman.
               Addopted to use NuSMV library by Andrei Tchaltsev
  \brief The LTL to SMV Translator

  This file contains the main function
  which invokes a routine for reducing LTL model
  checking to CTL model checking. see file ltl2smv.h

*/

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/ltl/ltl2smv/ltl2smv.h"

#include "nusmv/core/utils/error.h"
#include "nusmv/core/cinit/cinit.h"
#include "nusmv/core/cinit/cinit.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/parser/parser.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/ustring.h"
#include <stdio.h>

static void ltl2smv_print_module(const NuSMVEnv_ptr env,
                                 FILE* ostream, node_ptr);

static void print_usage(StreamMgr_ptr streams, int argc, char **argv) {
    StreamMgr_print_error(streams,  "%s: Converts an LTL formula to a fragment of an "
            "SMV program.\n", argv[0]);
    StreamMgr_print_error(streams,  "%s: %s [-s] # <ifile> [<ofile>]\n", argv[0], argv[0]);
    StreamMgr_print_error(streams,  "Where:\n"
            "\t[-s]\t if passed, inform to generate a single justice constraint\n"
            "\t\tinstead of posisbly more than one\n"
            "\t#\t is a positive number which is converted to a string\n"
            "\t\t and then used as a part of the generated SMV model\n"
            "\t\t name _LTL#_SPECF_N_.\n");
    StreamMgr_print_error(streams,  "\t<ifile>\t is the file from which the LTL Formula "
            "to be translated\n\t\t is read.\n");
    StreamMgr_print_error(streams,  "\t<ofile>\t is the file in which the SMV code "
            "corresponding to the\n\t\t tableau of LTL Formula is "
            "written in.\n\t\t If not specified than stdout is used.\n");
}

int main(int argc, char **argv)
{
  unsigned int uniqueId;
  node_ptr in_ltl_expr;
  FILE* output_file;
  NuSMVEnv_ptr env = NuSMVEnv_create();
  StreamMgr_ptr streams;
  int i = 1;
  boolean single_fairness = false;

  /* initialise all the packages even though they are not required */
  NuSMVCore_init_data();
  NuSMVCore_init(env, NULL, 0);

  streams = STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  /* check the number of arguments */
  if ((argc > 5) || (argc < 3)) {
    print_usage(streams, argc, argv);
    exit(1);
  }

  if (strcmp(argv[i], "-s") == 0) {
    i++;
    single_fairness = true;
  }

  /* check the first argument -- a zero or positive number */
  {
    char* err_occ = "";
    long int r = strtol(argv[i], &err_occ, 10);
    if (strcmp(err_occ, "") != 0) {
      StreamMgr_print_error(streams,  "Error: \"%s\" is not a natural number.\n", err_occ);
      exit(1);
    }
    if (r < 0) {
      StreamMgr_print_error(streams,  "Error: \"%ld\" is not a positive number.\n", r);
      exit(1);
    }
    if (r != (unsigned int)r) {
      StreamMgr_print_error(streams,  "Error: \"%ld\" is a too large number.\n", r);
      exit(1);
    }
    uniqueId = (unsigned int)r;
  }
  i++;
  /* open and parse from the input file */
  {
    extern node_ptr parsed_tree; /* the result of parsing */

    if ((char*)NULL == argv[i]) {
      StreamMgr_print_error(streams,  "Error: ltl2smv : the input file is not specified.\n");
      exit(1);
    }

    if (Parser_ReadLtlExprFromFile(env, argv[i])) {
      StreamMgr_print_error(streams,  "Paring Error in LTL expression.\n");
      exit(1);
    }
    in_ltl_expr = parsed_tree; /* the result of parsing */
  }
  i++;
  /* open the output file */
  {
    if (NULL != argv[i]) {
      output_file = fopen(argv[i], "w");
      if (output_file == (FILE *)NULL) {
        StreamMgr_print_error(streams,  "Error: Unable to open file \"%s\" for writing.\n",
                argv[i]);
        exit(1);
      }
    } else {
      output_file = stdout;
    }
  }

  i++;

  if (i < argc) {
    printf("KKKKKKKKKKK %d %d\n", argc, i);
    print_usage(streams, argc, argv);
    exit(1);
  }

  /* transform the expression and print it out */
  {
    node_ptr out_smv_module = ltl2smv_core(env,
                                           uniqueId,
                                           Compile_pop_distrib_ops(env, in_ltl_expr),
                                           single_fairness, NULL);

    ltl2smv_print_module(env, output_file, out_smv_module);

    if (output_file != stdout) fclose(output_file);
  }

  NuSMVEnv_destroy(env);

  /* do not deinitilise the packages. What for? */
  return 0;
}


/* static function which prints a module */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void ltl2smv_print_module(const NuSMVEnv_ptr env,
                                 FILE* ostream, node_ptr module)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
\
  node_ptr iter;

  nusmv_assert(Nil != module);
  nusmv_assert(MODULE == node_get_type(module));
  /* print the name */
  nusmv_assert(MODTYPE == node_get_type(car(module)));
  nusmv_assert(ATOM == node_get_type(car(car(module))));
  nusmv_assert(Nil == cdr(car(module)));

  fprintf(ostream, "MODULE %s\n", UStringMgr_get_string_text((string_ptr)car(car(car(module)))));

  iter = cdr(module);
  while (Nil != iter) {
    nusmv_assert(CONS == node_get_type(iter));
    switch (node_get_type(car(iter))) {

    case VAR: { /* variable declarations */
      node_ptr var;
      var = car(car(iter));
      if ( Nil != var) {
        fprintf(ostream, "VAR\n");
        while (Nil != var) { /* iterate over variable declarations */

          nusmv_assert(CONS == node_get_type(var));
          nusmv_assert(COLON == node_get_type(car(var)));
          nusmv_assert(ATOM == node_get_type(car(car(var))));
          nusmv_assert(BOOLEAN == node_get_type(cdr(car(var))));

          fprintf(ostream, "   %s : boolean;\n",
                  UStringMgr_get_string_text((string_ptr)car(car(car(var)))));

          var = cdr(var);
        }
      }
      break;
    }

    case DEFINE: { /* define declarations */
      node_ptr def;
      def = car(car(iter));
      if ( Nil != def) {
        fprintf(ostream, "DEFINE\n");
        while (Nil != def) { /* iterate over define declarations */

          nusmv_assert(CONS == node_get_type(def));
          nusmv_assert(EQDEF == node_get_type(car(def)));

          fprintf(ostream, "   ");
          print_node(wffprint, ostream, car(def));
          fprintf(ostream, ";\n");

          def = cdr(def);
        }
      } /* if */
      break;
    }

    case INIT: /* INIT  declarations */
      fprintf(ostream, "INIT\n   ");
      print_node(wffprint, ostream, car(car(iter)));
      fprintf(ostream, "\n");
      break;

    case TRANS: /* TRANS  declarations */
      fprintf(ostream, "TRANS\n   ");
      print_node(wffprint, ostream, car(car(iter)));
      fprintf(ostream, "\n");
      break;

    case JUSTICE: /* JUSTICE  declarations */
      fprintf(ostream, "JUSTICE\n   ");
      print_node(wffprint, ostream, car(car(iter)));
      fprintf(ostream, "\n");
      break;
    default: error_unreachable_code(); /* unexpected node */
    } /*switch */

    iter = cdr(iter);
  } /* while */

  return;
}
