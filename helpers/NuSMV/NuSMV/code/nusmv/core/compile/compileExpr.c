/* ---------------------------------------------------------------------------


   This file is part of the ``compile'' package.
   Copyright (C) 2014 by FBK.

   NuSMV version 2 is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   NuSMV version 2 is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied
   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
   See the GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
   MA 02111-1307 USA.

   For more information on NuSMV see <http://nusmv.fbk.eu> or
   email to <nusmv-users@fbk.eu>.
   Please report bugs to <nusmv-users@fbk.eu>.

   To contact the NuSMV development board, email to <nusmv@fbk.eu>. 

-----------------------------------------------------------------------------*/

/*!
  \author Roberto Cavada
  \brief Main routines for compile exoressions from strings

  \todo: Missing description

*/

#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/parser/parser.h"
#include "nusmv/core/parser/symbols.h"


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

Expr_ptr Compile_compile_simpwff_from_string(NuSMVEnv_ptr env,
                                             const SymbTable_ptr st,
                                             const char* str_formula)
{
  Expr_ptr res;

  if (str_formula != (char*) NULL) {
    TypeChecker_ptr tc;

    if (Parser_ReadSimpExprFromString(env, str_formula, &res) != 0 ||
        res == Nil || node_get_type(res) != SIMPWFF) {
      const StreamMgr_ptr streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

      StreamMgr_print_error(streams,  "Parsing error: formula must be "
                            "\"simple expression\".\n");
      return EXPR(NULL);
    }
    res = car(res); /* get rid of SIMPWFF/NEXTWFF token */

    tc = SymbTable_get_type_checker(st);
    if (!TypeChecker_is_expression_wellformed(tc, res, Nil)) {
      const StreamMgr_ptr streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

      StreamMgr_print_error(streams, "Type System Violation\n");
      return EXPR(NULL);
    }
  }
  else {
    res = ExprMgr_true(EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER)));
  }

  return res;
}

Expr_ptr Compile_compile_nextwff_from_string(NuSMVEnv_ptr env,
                                             const SymbTable_ptr st,
                                             const char* str_formula)
{
  Expr_ptr res;

  if (str_formula != (char*) NULL) {
    TypeChecker_ptr tc;

    if (Parser_ReadNextExprFromString(env, str_formula, &res) != 0 ||
        res == Nil || node_get_type(res) != NEXTWFF) {
      const StreamMgr_ptr streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

      StreamMgr_print_error(streams,  "Parsing error: formula must be "
                            "\"next expression\".\n");
      return EXPR(NULL);
    }
    res = car(res); /* get rid of SIMPWFF/NEXTWFF token */

    tc = SymbTable_get_type_checker(st);
    if (!TypeChecker_is_expression_wellformed(tc, res, Nil)) {
      const StreamMgr_ptr streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

      StreamMgr_print_error(streams, "Type System Violation\n");
      return EXPR(NULL);
    }
  }
  else {
    res = ExprMgr_true(EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER)));
  }

  return res;
}

Expr_ptr Compile_compile_spec_from_string(NuSMVEnv_ptr env,
                                          const SymbTable_ptr st,
                                          const char* str_formula,
                                          const Prop_Type prop_type)
{
  Expr_ptr res = Nil;

  if (str_formula != (char*) NULL) {
    const char* argv[2];
    const int argc = 2;
    int parse_result;

    argv[0] = (char*) NULL;
    argv[1] = (char*) str_formula;

    if (Prop_Psl == prop_type) {
      parse_result = Parser_read_psl_from_string(env, argc, argv, &res);
    }
    else {
      char* prefix = NULL;
      switch (prop_type) {
        /* WATCH OUT! The final space is required! */
      case Prop_Ctl:
        prefix = "CTLWFF ";
        break;
      case Prop_Ltl:
        prefix = "LTLWFF ";
        break;
      case Prop_Invar:
        prefix = "NEXTWFF ";
        break;
      case Prop_Compute:
        prefix = "COMPWFF ";
        break;
      default:
        nusmv_assert(false);  /* not supported type */
      }

      parse_result = Parser_ReadCmdFromString(env, argc, argv, prefix,
                                              ";\n", &res);

      /* fixes res in the case != PSL as parser gives results in
         different format */
      if (Nil != res) {
        res = car(res);
      }
    }

    /* error checking */
    if (parse_result != 0 || res == Nil) {
      const StreamMgr_ptr streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

      StreamMgr_print_error(streams,
              "Parsing error: expected a \"%s\" expression.\n",
              PropType_to_string(prop_type));
      return NULL;
    }

    { /* type check it */
      TypeChecker_ptr tc = SymbTable_get_type_checker(st);
      if (!TypeChecker_is_expression_wellformed(tc, res, Nil)) {
        const StreamMgr_ptr streams =
          STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

        StreamMgr_print_error(streams, "Type System Violation\n");
        return EXPR(NULL);
      }
    }
  }
  else {
    res = ExprMgr_true(EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER)));
  }

  return res;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
