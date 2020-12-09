/* ---------------------------------------------------------------------------


  This file is part of the ``bmc'' package of NuSMV version 2.
  Copyright (C) 2000-2001 by FBK-irst and University of Trento.

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
  \author Roberto Cavada, Marco Benedetti
  \brief Test routines for <tt>bmc</tt> package

  \todo: Missing description

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include <math.h>

#include "nusmv/core/bmc/bmc.h"
#include "nusmv/core/bmc/bmcInt.h"
#include "nusmv/core/bmc/bmcUtils.h"
#include "nusmv/core/bmc/bmcTableau.h"
#include "nusmv/core/bmc/bmcConv.h"
#include "nusmv/core/bmc/bmcTest.h"

#include "nusmv/core/wff/wff.h"
#include "nusmv/core/wff/w2w/w2w.h"

#include "nusmv/core/enc/enc.h"
#include "nusmv/core/enc/be/BeEnc.h"
#include "nusmv/core/be/be.h"

#include "nusmv/core/fsm/be/BeFsm.h"

#include "nusmv/core/prop/propPkg.h"
#include "nusmv/core/parser/symbols.h" /* for constants */
#include "nusmv/core/utils/error.h"

/* [AT] I did not updated this file after introduction of the frozen
   vars. Look in text for "BE_VAR_TYPE_CURR" and "state" for critical
   code parts. */


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define GEN_WFF_CONSES_OP_NUMBER 15

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
static int generated_formulas = 0;

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
static node_ptr
bmc_test_mk_loopback_ltl(const BeEnc_ptr be_enc,
                         const int k, const int l);

static node_ptr
bmc_test_gen_wff(const BeEnc_ptr be_enc,
                 int max_depth, int max_conns,
                 boolean usePastOperators);

static node_ptr
bmc_test_gen_tableau(const BeFsm_ptr be_fsm, const node_ptr ltl_nnf_wff,
                     const int k, const int l,
                     boolean usePastOperators);

static void
bmc_test_bexpr_output(const BeEnc_ptr be_enc, FILE* f,
                      const node_ptr bexp, const int output_type);


/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Bmc_TestReset()
{
  generated_formulas = 0;
}

int Bmc_Test_test_tableau(NuSMVEnv_ptr env,
                          node_ptr wff,
                          GenWffOperator wff_operator,
                          int max_depth,
                          int max_conns,
                          boolean usePastOperators,
                          boolean crossComparison,
                          int k,
                          int l)
{
  BeFsm_ptr const be_fsm = BE_FSM(NuSMVEnv_get_value(env, ENV_BE_FSM));
  BeEnc_ptr const be_enc = BeFsm_get_be_encoding(be_fsm);
  NodeMgr_ptr const nodemgr =
     NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  FILE *f,*f1,*f2;
  char szLoopback[16];
  node_ptr tableau_test;

  nusmv_assert(generated_formulas >= 0);

  if (wff == NULL) {
    /* generates a random wff: */
    switch (wff_operator) {

    case GWO_None:
      wff = bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators);
      break;

    case GWO_Globally:
      wff = Wff_make_globally(nodemgr, bmc_test_gen_wff(be_enc, max_depth,
                                                max_conns, usePastOperators));
      break;

    case GWO_Future:
      wff = Wff_make_eventually(nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns,
                                                  usePastOperators));
      break;

    case GWO_Until:
      wff = Wff_make_until(nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators),
                            bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));
      break;

    case GWO_Releases:
      wff = Wff_make_releases(nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators),
                               bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));
      break;

    case GWO_Historically:
      wff = Wff_make_historically(nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));
      break;

    case GWO_Once:
      wff = Wff_make_once(nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));
      break;

    case GWO_Since:
      wff = Wff_make_since(nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators),
                            bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));
      break;

    case GWO_Triggered:
      wff = Wff_make_triggered(nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators),
                                bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));
      break;


    default:
      error_unreachable_code();
    }

    wff = Wff2Nnf(env, wff);
  }

  if (!crossComparison) {
    /* generates the test tableau */
    tableau_test = bmc_test_gen_tableau(be_fsm, wff, k, l, usePastOperators);

    /* writes down the imply formula */
    if (generated_formulas == 0) {
      int i=0;

      f = fopen("Bmc_TestTableau.smv", "w");
      nusmv_assert(f != NULL);

      /* writes down the non-deterministic model */
      fprintf(f, "MODULE main\nVAR\n");
      for (i = 0; i < BeEnc_get_state_vars_num(be_enc); i++) {
        fprintf(f, "p%d: boolean;\n", i);
      }
    }
    else {
      /* this command has already been invoked */
      f = fopen("Bmc_TestTableau.smv", "a");
      nusmv_assert(f != NULL);
    }

    Bmc_Utils_ConvertLoopFromInteger(l, szLoopback, sizeof(szLoopback));
    fprintf(f, "\n\n-- Property %d (k=%d, l=%s, max_depth=%d, max_conns=%d): \n",
            generated_formulas, k, szLoopback, max_depth, max_conns);
    fprintf(f, "LTLSPEC ");

    ++generated_formulas;

    fprintf (f, "\n");
    bmc_test_bexpr_output(be_enc, f, tableau_test,
                          BMC_BEXP_OUTPUT_SMV);
    fprintf(f, "\n\n");

    fclose(f);
  }
  else {
    /* writes down the formula */
    if (generated_formulas == 0) {
      int i=0;

      f1 = fopen("Bmc_TestTableau_BMC.smv", "w");
      f2 = fopen("Bmc_TestTableau_BDD.smv", "w");
      nusmv_assert(f1 != NULL);
      nusmv_assert(f2 != NULL);

      /* writes down the non-deterministic model */
      fprintf(f1, "MODULE main\nVAR\n");
      fprintf(f2, "MODULE main\nVAR\n");
      for (i = 0; i < BeEnc_get_state_vars_num(be_enc); i++) {
        fprintf(f1, "p%d: boolean;\n", i);
        fprintf(f2, "p%d: boolean;\n", i);
      }
    }
    else {
      /* this command has already been invoked */
      f1 = fopen("Bmc_TestTableau_BMC.smv", "a");
      f2 = fopen("Bmc_TestTableau_BDD.smv", "a");
      nusmv_assert(f1 != NULL);
      nusmv_assert(f2 != NULL);
    }

    Bmc_Utils_ConvertLoopFromInteger(l, szLoopback, sizeof(szLoopback));
    fprintf(f1, "\n\n-- Property %d (k=%d, l=%s, max_depth=%d, max_conns=%d): \n",
            generated_formulas, k, szLoopback, max_depth, max_conns);
    fprintf(f1, "LTLSPEC ");
    fprintf(f2, "\n\n-- Property %d (k=%d, l=%s, max_depth=%d, max_conns=%d): \n",
            generated_formulas, k, szLoopback, max_depth, max_conns);
    fprintf(f2, "LTLSPEC ");

    ++generated_formulas;

    fprintf (f1, "\n");
    fprintf (f2, "\n");

    bmc_test_bexpr_output(be_enc, f1, wff, BMC_BEXP_OUTPUT_SMV);

    wff = Wff_make_implies(nodemgr, bmc_test_mk_loopback_ltl(be_enc, k, l), wff);


    bmc_test_bexpr_output(be_enc, f2, wff, BMC_BEXP_OUTPUT_SMV);

    fprintf(f1, "\n\n");
    fprintf(f2, "\n\n");

    fclose(f1);
    fclose(f2);
  }

  return 0;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief For each variable p in the set of state variables,
  generates the global equivalence of p and X^(loop length), starting from
  the loop start

   In the following example we suppose the loop starts
  from 2 and finishes to 6 (the bound).

  <PRE>
        ,-----------.
        V           |
  o--o--o--o--o--o--o--o--o--o--o--o--o- (...continues indefinitely)
  0  1  2  3  4  5  6  7  8  9  10 11 12

  </PRE>


  In general all state variables in time 2 must be forced to be equivalent
  to the corresponding variables timed in 6, the variables in 3 to 7,
  and so on up to the variables in 6 (equivalent to variables in
  10). Then variables in 7 (or 3 again) must be forced to be equivalent
  to the varaibles in 11, and so on indefinitely.
  <BR><BR>
  In formula (let suppose we have only one boolean variable):
  <BR>
  (p2 <-> p6) && (p6 <-> p10) ...
  <BR><BR>
  In a more compact (and finite!) form, related to this example:
  XX(G (p <-> XXXX(p)))

  The first two neXtes force the formula to be effective only from the loop
  starting point.
  The generic formula implemented in the code is the following one:
  <PRE>
  X^(l) (G ((p0 <-> X^(k-l)(p0)) &&
            (p1 <-> X^(k-l)(p1)) &&
                        .
                        .
                        .
            (pn <-> X^(k-l)(pn)))
        )
  </PRE>
 where:
   p0..pn are all boolean variables into the model
   X^(n) is expanded to XXX..X n-times.
 Note that frozen vars can be ignored since they are always equal to their previous
 values
*/
static node_ptr
bmc_test_mk_loopback_ltl(const BeEnc_ptr be_enc, const int k, const int l)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr result;
  node_ptr bigand_vars;
  node_ptr single_var_eq;
  node_ptr var;
  int idx;
  int loop_len = 0;

  nusmv_assert( !Bmc_Utils_IsNoLoopback(l) && (l < k) );
  nusmv_assert( BeEnc_get_state_vars_num(be_enc) > 0 );

  loop_len = k-l;

  /* first cycle is performed manually, in order to optimize a bit */
  idx = BeEnc_get_first_untimed_var_index(be_enc, BE_VAR_TYPE_CURR);
  var = BeEnc_index_to_name(be_enc, idx);
  bigand_vars = Wff_make_iff(nodemgr, var, Wff_make_opnext_times(nodemgr, var, loop_len));

  /* iterates across the remaining variables: */
  idx = BeEnc_get_next_var_index(be_enc, idx, BE_VAR_TYPE_CURR);
  while (BeEnc_is_var_index_valid(be_enc, idx)) {
    var = BeEnc_index_to_name(be_enc, idx);
    single_var_eq = Wff_make_iff(nodemgr, var, Wff_make_opnext_times(nodemgr, var, loop_len));
    bigand_vars = Wff_make_and(nodemgr, bigand_vars, single_var_eq);
    idx = BeEnc_get_next_var_index(be_enc, idx, BE_VAR_TYPE_CURR);
  }

  result = Wff_make_globally(nodemgr, bigand_vars);
  result = Wff_make_opnext_times(nodemgr, result, l); /* shifts to loop starting point */

  return result;
}

/*!
  \brief Given a WFF in NNF, converts it into a tableau
  formula, then back to WFF_(k,l) and returns WFF -> WFF_(k,l)

  This function is used to test tableau formulae
*/
static node_ptr
bmc_test_gen_tableau(const BeFsm_ptr be_fsm, const node_ptr ltl_nnf_wff,
                     const int k, const int l, boolean usePastOperators)
{
  node_ptr tableau_as_wff;
  node_ptr implies_formula;
  be_ptr tableau_k_l_ltl_wff;
  BeEnc_ptr be_enc = BeFsm_get_be_encoding(be_fsm);

  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  /* generates the tableau (with no fairness): */
  tableau_k_l_ltl_wff = Bmc_GetTestTableau(be_enc, ltl_nnf_wff, k, l);

  /* reconvert the tableau back to a wff_(k,l) */
  tableau_as_wff = Bmc_Conv_Be2Bexp(be_enc, tableau_k_l_ltl_wff);

  /* build the implies: */
  if (Bmc_Utils_IsNoLoopback(l)) {
    implies_formula = Wff_make_implies(nodemgr, tableau_as_wff, ltl_nnf_wff);
  }
  else {
    nusmv_assert(!Bmc_Utils_IsAllLoopbacks(l)); /* all loops are not allowed nowadays */
    implies_formula = Wff_make_implies(nodemgr,
                        Wff_make_and(nodemgr, tableau_as_wff,
                                      bmc_test_mk_loopback_ltl(be_enc, k, l)),
                        ltl_nnf_wff);
  }

  return implies_formula;
}

/*!
  \brief Builds a <b>random LTL WFF</b> with specified
  <tt>max</tt> depth and <tt>max</tt> connectives.



  \se node hash may change
*/
static node_ptr bmc_test_gen_wff(const BeEnc_ptr be_enc,
                                 int max_depth, int max_conns,
                                 boolean usePastOperators)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  int rnd;
  double rnd_tmp;

  /* generates a random number which refers to either a state variable or
     an operator. Propositional and future time operators are always
     allowed, whereas past time operators can be only generated when
     the "usePastOperators" flag is true.*/

  do {
   rnd_tmp = floor(rand()) / (RAND_MAX + 1.0);

   rnd = (int) floor((GEN_WFF_CONSES_OP_NUMBER + BeEnc_get_state_vars_num(be_enc))
                     * rnd_tmp) + 1;
  }
  while (!usePastOperators && (rnd>10 && rnd<=15));

  /* if depth or connses of wff are exausted get a random number such that:
     (rnd >= 0) && (rnd < 'number of state variables')... */
  if ((max_depth < 0) || (max_conns < 0)) {
    int idx;
    rnd = (int) (((float) BeEnc_get_state_vars_num(be_enc) * rand()) /
                 (RAND_MAX + 1.0));

    idx = BeEnc_get_var_index_with_offset(be_enc,
                  BeEnc_get_first_untimed_var_index(be_enc, BE_VAR_TYPE_CURR),
                  rnd, BE_VAR_TYPE_CURR);

    /* ...then return correspondent state variable to the random integer */
    return BeEnc_index_to_name(be_enc, idx);
  }

  /* exclude atoms from depth and connses decrement contributes */
  if (rnd <= GEN_WFF_CONSES_OP_NUMBER) {
    --max_depth; --max_conns;
  }

  switch (rnd) {
  /* Propositional operators */
  case 1:
    return Wff_make_not(nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));

  case 2:
    return Wff_make_or (nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators),
                         bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));

  case 3:
    return Wff_make_and(nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators),
                         bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));

  case 4:
    return Wff_make_implies(nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators),
                             bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));

  case 5:
    return Wff_make_iff(nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators),
                         bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));

  /* Future operators */
  case 6:
    return Wff_make_opnext    (nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));

  case 7:
    return Wff_make_eventually(nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));

  case 8:
    return Wff_make_globally  (nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));

  case 9:
    return Wff_make_until     (nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators),
                                bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));
  case 10:
    return Wff_make_releases  (nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators),
                                bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));

  /* Past operators */
  case 11:
    return Wff_make_opprec      (nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));

  case 12:
    return Wff_make_once        (nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));

  case 13:
    return Wff_make_historically(nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));

  case 14:
    return Wff_make_since       (nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators),
                                  bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));
  case 15:
    return Wff_make_triggered   (nodemgr, bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators),
                                  bmc_test_gen_wff(be_enc, max_depth, max_conns, usePastOperators));

  default:
    {
      int idx = BeEnc_get_var_index_with_offset(be_enc,
        BeEnc_get_first_untimed_var_index(be_enc, BE_VAR_TYPE_CURR),
        rnd - GEN_WFF_CONSES_OP_NUMBER - 1, BE_VAR_TYPE_CURR);

      return BeEnc_index_to_name(be_enc, idx);
    }
  }
}

/*!
  \brief <b>Write</b> to specified FILE stream given node_ptr
  <b>formula</b> with specified <tt>output_type</tt> format. There are
  follow formats: <tt>BMC_BEXP_OUTPUT_SMV, BMC_BEXP_OUTPUT_LB</tt>



  \se None
*/
static void
bmc_test_bexpr_output(const BeEnc_ptr be_enc, FILE* f,
                      const node_ptr bexp, const int output_type)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(be_enc));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  int type;

  nusmv_assert(f != NULL);

  /* exit from recursion if given formula is Nil */
  if (bexp == Nil) return;

  /* assert that input formula type can't be a NEXT operator, that is
     used in model specification (section ASSIGN). We use here only OP_NEXT
     operator used in the module (section LTLSPEC). */
  nusmv_assert (node_get_type (bexp) != NEXT);

  type = node_get_type (bexp);

  switch (type) {
  case FALSEEXP:                        /* FALSEEXP  */
    fprintf (f, "%s", (output_type == BMC_BEXP_OUTPUT_SMV) ? "FALSE" : "false");
    break;

  case TRUEEXP:                         /* TRUEEXP   */
    fprintf (f, "%s", (output_type == BMC_BEXP_OUTPUT_SMV) ? "TRUE" : "true");
    break;

  case AND:                             /* AND       */
    fprintf(f, "(");
    bmc_test_bexpr_output(be_enc, f, car (bexp), output_type);
    fprintf(f, " %s ", (output_type == BMC_BEXP_OUTPUT_SMV) ? "&" : "/\\");
    bmc_test_bexpr_output(be_enc, f, cdr (bexp), output_type);
    fprintf(f, ")");
    break;

  case OR:                              /* OR        */
    fprintf (f, "(");
    bmc_test_bexpr_output(be_enc, f, car (bexp), output_type);
    fprintf (f, " %s ", (output_type == BMC_BEXP_OUTPUT_SMV) ? "|" : "\\/");
    bmc_test_bexpr_output(be_enc, f, cdr (bexp), output_type);
    fprintf (f, ")");
    break;

  case NOT:                             /* NOT       */
    fprintf (f, "%c", (output_type == BMC_BEXP_OUTPUT_SMV) ? '!' : '~');
    fprintf (f, "(");
    bmc_test_bexpr_output(be_enc, f, car (bexp), output_type);
    fprintf (f, ")");
    break;

  case IMPLIES:                         /* IMPLIES   */
    fprintf (f, "(");
    bmc_test_bexpr_output(be_enc, f, car (bexp), output_type);
    fprintf (f, " -> ");
    bmc_test_bexpr_output(be_enc, f, cdr (bexp), output_type);
    fprintf (f, ")");
    break;

  case IFF:                             /* IFF       */
    fprintf (f, "(");
    bmc_test_bexpr_output(be_enc, f, car (bexp), output_type);
    fprintf (f, " <-> ");
    bmc_test_bexpr_output(be_enc, f, cdr (bexp), output_type);
    fprintf (f, ")");
    break;

  case OP_FUTURE:                       /* OP_FUTURE */
    fprintf (f, "F(");
    bmc_test_bexpr_output(be_enc, f, car (bexp), output_type);
    fprintf (f, ")");
    break;

  case OP_ONCE:                       /* OP_ONCE */
    fprintf (f, "O(");
    bmc_test_bexpr_output(be_enc, f, car (bexp), output_type);
    fprintf (f, ")");
    break;

  case OP_GLOBAL:                       /* OP_GLOBAL */
    fprintf (f, "G(");
    bmc_test_bexpr_output(be_enc, f, car (bexp), output_type);
    fprintf (f, ")");
    break;

  case OP_HISTORICAL:                       /* OP_HISTORICAL */
    fprintf (f, "H(");
    bmc_test_bexpr_output(be_enc, f, car (bexp), output_type);
    fprintf (f, ")");
    break;

  case UNTIL:                           /* UNTIL     */
    fprintf (f, "(");
    bmc_test_bexpr_output(be_enc, f, car (bexp), output_type);
    fprintf (f, " U ");
    bmc_test_bexpr_output(be_enc, f, cdr (bexp), output_type);
    fprintf (f, ")");
    break;

  case SINCE:                           /* SINCE     */
    fprintf (f, "(");
    bmc_test_bexpr_output(be_enc, f, car (bexp), output_type);
    fprintf (f, " S ");
    bmc_test_bexpr_output(be_enc, f, cdr (bexp), output_type);
    fprintf (f, ")");
    break;

  case RELEASES:                        /* RELEASES  */
    fprintf (f, "(");
    bmc_test_bexpr_output(be_enc, f, car (bexp), output_type);
    fprintf (f, " V ");
    bmc_test_bexpr_output(be_enc, f, cdr (bexp), output_type);
    fprintf (f, ")");
    break;

  case TRIGGERED:                       /* TRIGGERED  */
    fprintf (f, "(");
    bmc_test_bexpr_output(be_enc, f, car (bexp), output_type);
    fprintf (f, " T ");
    bmc_test_bexpr_output(be_enc, f, cdr (bexp), output_type);
    fprintf (f, ")");
    break;

  case OP_NEXT:                         /* OP_NEXT   */
    {
      node_ptr temp_bexp = bexp;
      int i = 0;
      {
        /* prints out "X(" suffix while OP_NEXT is encountred */
        do {
          fprintf (f, "X(");
          temp_bexp = car(temp_bexp);
          nusmv_assert(temp_bexp != Nil);
          i++;
        } while (node_get_type(temp_bexp) == OP_NEXT);
      }

      /* then print the internal bexp */
      bmc_test_bexpr_output(be_enc, f, temp_bexp, output_type);

      while ((i--) > 0) fprintf (f, ")");
    }
    break;

  case OP_PREC:                         /* OP_PREC   */
    {
      node_ptr temp_bexp = bexp;
      int i = 0;
      {
        /* prints out "Y(" suffix while OP_PREC is encountred */
        do {
          fprintf (f, "Y(");
          temp_bexp = car(temp_bexp);
          nusmv_assert(temp_bexp != Nil);
          i++;
        } while (node_get_type(temp_bexp) == OP_PREC);
      }

      /* then print the internal bexp */
      bmc_test_bexpr_output(be_enc, f, temp_bexp, output_type);

      while ((i--) > 0) fprintf (f, ")");
    }
    break;

  case OP_NOTPRECNOT:                         /* OP_PREC   */
    {
      node_ptr temp_bexp = bexp;
      int i = 0;
      {
  /* prints out "Z(" suffix while OP_NOTPRECNOT is encountred */
        do {
          fprintf (f, "Z(");
          temp_bexp = car(temp_bexp);
          nusmv_assert(temp_bexp != Nil);
          i++;
        } while (node_get_type(temp_bexp) == OP_NOTPRECNOT);
      }

      /* then print the internal bexp */
      bmc_test_bexpr_output(be_enc, f, temp_bexp, output_type);

      while ((i--) > 0) fprintf (f, ")");
    }
    break;


  default:                              /* (default action) */
    {
      be_ptr r;

      /* gets the the be correspondent to the state variable */
      r = BeEnc_name_to_untimed(be_enc, bexp);

      /* if bexp is a really state variable, then prints out the index
         of correspondent be variable */
      if (r != (be_ptr) NULL) {
        fprintf(f, "p%d", Be_Var2Index(BeEnc_get_be_manager(be_enc), r));
      }
      else {
        ErrorMgr_internal_error(errmgr, "bmc_test_bexpr_output: given wff atom isn\' in BE environ\n");
      }
    }
  }

}
