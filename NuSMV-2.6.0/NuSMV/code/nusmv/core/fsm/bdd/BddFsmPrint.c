/* ---------------------------------------------------------------------------

   This file is part of the ``fsm.bdd'' package of NuSMV version 2.
   Copyright (C) 2015 by FBK-irst.

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
  \brief Defines functions for printing functionalities of BddFsm

*/


#include "nusmv/core/fsm/bdd/BddFsm.h"
#include "nusmv/core/fsm/bdd/BddFsm_private.h"
#include "nusmv/core/fsm/bdd/bddInt.h"

#include "nusmv/core/utils/OStream.h"
#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/Vector.h"

#include "nusmv/core/compile/symb_table/SymbTable.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/parser/symbols.h"

/* libraries */
#include <math.h>


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief Limit used when printing
*/
#define MAX_ELEMENTS_PRINTABLE                \
  (double)(pow((double)2.0, (double)16.0))


#define HEADER_SEPARATOR  \
  "######################################################################\n"


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/** this is used to get values of trans when traversing states in
 * explicit way */
struct BddFsmTrans {
  BddEnc_ptr enc;
  BddStatesInputs source;
  BddStates source_curr;  /* without inputs */
  int source_id;  /* unique name for source (current vars) */
  BddStates dest_next;  /* destination in next variables */
  BddStates dest_curr;  /* destination in current variables */
  int dest_id;  /* unique name for destination (current vars) */
};


/** This is the iterator used to traverse incrementally minterms */
struct BddFsmMinterArrayIterator {
  int idx;  /* iteration index on the array */

  bdd_ptr iter_bdd;  /* the iterated bdd which is being iterated */
  double array_size;  /* size of the array */
  bdd_ptr* array;  /* the array for minterms */
};


/** Iterator structure for iterating among transitions */
struct BddFsmTransIterator {
  BddFsm_ptr fsm;  /* the FSM the iterator was created by */
  BddEnc_ptr enc;  /* the encoder used by the FSM */

  struct BddFsmMinterArrayIterator source_iter;
  struct BddFsmMinterArrayIterator dest_iter;

  NodeList_ptr sf_symb_list;  /* list of state and frozen symbols */
  NodeList_ptr n_symb_list;  /* list of next state and frozen symbols */
  NodeList_ptr i_symb_list;  /* list of input symbols */

  double trans_counter;
  double pair_counter;

  int id_counter;  /* counter to generate unique names */
  hash_ptr bdd2id;  /* This is used to give a unique readable name to
                     * bdds when printing to dot and csv formats */
  Vector_ptr id2bdd;  /* reverse map of bdd2id */
};


/* this is used in printers, to distinguish the different document
 * sections */
enum BddFsmTransPrinterSection {
  BDD_FSM_TRANS_PRINTER_SECTION_HEADER,
  BDD_FSM_TRANS_PRINTER_SECTION_BODY,
  BDD_FSM_TRANS_PRINTER_SECTION_FOOTER,
};


/* ---------------------------------------------------------------------- */
/*                     Static functions prototypes                        */
/* ---------------------------------------------------------------------- */

/* top level for starting iteration among transitions in given bdd */
static void BddFsm_trans_iterator_begin(const BddFsm_ptr self,
                                        bdd_ptr state_input,
                                        /* outputs: */
                                        struct BddFsmTransIterator* iter,
                                        struct BddFsmTrans* trans);

static void BddFsm_trans_iterator_end(const BddFsm_ptr self,
                                      struct BddFsmTransIterator* iter);

/* call to perform the traversal */
static int BddFsm_trans_iter_next(const BddFsm_ptr self,
                                  struct BddFsmTransIterator* iter,
                                  /* outputs: */
                                  struct BddFsmTrans* trans,
                                  boolean* inside_inner);

static void BddFsmTrans_print(
    const NuSMVEnv_ptr env,
    const struct BddFsmTransIterator* iter,
    const struct BddFsmTrans* trans_info,
    const enum BddFsmTransPrinterFormat format,
    const enum BddFsmTransPrinterSection section,
    OStream_ptr file);

static void BddFsmTrans_deinit(struct BddFsmTrans* self);

/* ---------------------------------------------------------------------- */
/*                          public methods                                */
/* ---------------------------------------------------------------------- */


void BddFsm_print_info(const BddFsm_ptr self, OStream_ptr file)
{
  BddStates init_bdd;
  BddInvarStates state_bdd;
  BddInvarInputs input_bdd;

  BDD_FSM_CHECK_INSTANCE(self);

  init_bdd = BddFsm_get_init(self);
  state_bdd = BddFsm_get_state_constraints(self);
  input_bdd = BddFsm_get_input_constraints(self);

  OStream_printf(file, "Statistics on BDD FSM machine.\n");

  if ((init_bdd != NULL)) {
    OStream_printf(file, "BDD nodes representing init set of states: %d\n",
            bdd_size(self->dd, init_bdd));
    bdd_free(self->dd, init_bdd);
  }

  if ((state_bdd != NULL)) {
    OStream_printf(file, "BDD nodes representing state constraints: %d\n",
            bdd_size(self->dd, state_bdd));
    bdd_free(self->dd, state_bdd);
  }

  if ((input_bdd != NULL)) {
    OStream_printf(file, "BDD nodes representing input constraints: %d\n",
            bdd_size(self->dd, input_bdd));
    bdd_free(self->dd, input_bdd);
  }

  BddTrans_print_short_info(BddFsm_get_trans(self), OStream_get_stream(file));
}


void BddFsm_print_reachable_states_info(const BddFsm_ptr self,
                                        const boolean print_states,
                                        const boolean print_defines,
                                        const boolean print_formula,
                                        OStream_ptr file)
{
  bdd_ptr reachable;
  bdd_ptr mask;
  double reached_cardinality;
  double search_space_cardinality;

  BDD_FSM_CHECK_INSTANCE(self);

  mask = BddEnc_get_state_frozen_vars_mask_bdd(self->enc);

  reachable = BddFsm_get_reachable_states(self);

  bdd_and_accumulate(self->dd, &reachable, mask);

  reached_cardinality = BddEnc_count_states_of_bdd(self->enc, reachable);
  search_space_cardinality = BddEnc_count_states_of_bdd(self->enc, mask);
  bdd_free(self->dd, mask);

  /* If we have diameter info, print it. Otherwise, we can only print
     the number of reachable states (ie. We do not have onion rings
     informations. For example, reachable states have been computed
     with Guided Reachability  */
  if (BddFsm_reachable_states_computed(self)) {
    OStream_printf(file, "system diameter: %d\n", BddFsm_get_diameter(self));
  }
  else {
    nusmv_assert(BddFsm_has_cached_reachable_states(self));
    OStream_printf(file, "system diameter: N/A\n");
  }

  OStream_printf(file, "reachable states: %g (2^%g) out of %g (2^%g)\n",
          reached_cardinality, log(reached_cardinality)/log(2.0),
          search_space_cardinality, log(search_space_cardinality)/log(2.0));

  /* one of these flags can be enabled, not both */
  nusmv_assert(!print_states || !print_formula);
  if (print_states) {
    BddEnc_print_set_of_states(self->enc, reachable, false, print_defines,
                               (VPFBEFNNV) NULL, file, NULL);
  }
  else if (print_formula) {
    BoolEnc_ptr benc = BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(self->enc));

    const array_t* layer_names =
      BaseEnc_get_committed_layer_names(BASE_ENC(self->enc));

    SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(self->enc));
    NodeList_ptr all_vars = SymbTable_get_layers_sf_vars(st, layer_names);
    NodeList_ptr scalar_vars = NodeList_create();
    ListIter_ptr iter;

    /* encoding variables are not allowed in the wff printer */
    NODE_LIST_FOREACH(all_vars, iter) {
      node_ptr v = NodeList_get_elem_at(all_vars, iter);
      if (BoolEnc_is_var_bit(benc, v)) continue;
      NodeList_append(scalar_vars, v);
    }
    NodeList_destroy(all_vars);

    OStream_printf(file, "\nFORMULA = \n");
    BddEnc_print_bdd_wff(self->enc, reachable, scalar_vars,
                         true, false, 0, file);

    NodeList_destroy(scalar_vars);
  }

  bdd_free(self->dd, reachable);
}


void BddFsm_print_fair_states_info(const BddFsm_ptr self,
                                   const boolean print_states,
                                   OStream_ptr file)
{
  bdd_ptr fair_states;
  bdd_ptr mask;
  double reached_cardinality;
  double search_space_cardinality;

  BDD_FSM_CHECK_INSTANCE(self);

  fair_states = BddFsm_get_fair_states(self);
  mask = BddEnc_get_state_frozen_vars_mask_bdd(self->enc);
  bdd_and_accumulate(self->dd, &fair_states, mask);

  reached_cardinality = BddEnc_count_states_of_bdd(self->enc, fair_states);
  search_space_cardinality = BddEnc_count_states_of_bdd(self->enc, mask);
  bdd_free(self->dd, mask);

  OStream_printf(file, "fair states: %g (2^%g) out of %g (2^%g)\n",
                 reached_cardinality, log(reached_cardinality)/log(2.0),
                 search_space_cardinality, log(search_space_cardinality)/log(2.0));

  if (print_states) {
    BddEnc_print_set_of_states(self->enc, fair_states, false, true,
                               (VPFBEFNNV) NULL, file, NULL);
  }
  bdd_free(self->dd, fair_states);
}


void BddFsm_print_fair_transitions_info(
    const BddFsm_ptr self,
    const enum BddFsmTransPrinterFormat format,
    OStream_ptr file)
{
  const NuSMVEnv_ptr env =
      EnvObject_get_environment(ENV_OBJECT(self->enc));
  const StreamMgr_ptr streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  bdd_ptr fair_trans;
  bdd_ptr mask;
  double fairtrans_cardinality;
  double search_space_cardinality;

  BDD_FSM_CHECK_INSTANCE(self);

  fair_trans = BddFsm_get_fair_states_inputs(self);
  mask = BddEnc_get_state_frozen_input_vars_mask_bdd(self->enc);
  bdd_and_accumulate(self->dd, &fair_trans, mask);

  fairtrans_cardinality = BddFsm_count_transitions(self, fair_trans);
  search_space_cardinality = BddEnc_get_minterms_of_bdd(self->enc, mask);
  bdd_free(self->dd, mask);

  StreamMgr_print_output(
      streams,
      "Fair transitions: %g (2^%g) out of %g (2^%g)\n",
      fairtrans_cardinality, log(fairtrans_cardinality)/log(2.0),
      search_space_cardinality, log(search_space_cardinality)/log(2.0));

  if (BDD_FSM_TRANS_PRINTER_SILENT != format) {
    struct BddFsmTransIterator iter;
    struct BddFsmTrans trans_info;

    BddFsm_trans_iterator_begin(self, fair_trans, &iter, &trans_info);

    if (BDD_FSM_TRANS_PRINTER_SMV == format) {
      OStream_inc_indent_size(file);
      BddEnc_print_bdd_begin(self->enc, iter.sf_symb_list, false);
    }

    BddFsmTrans_print(env, &iter, &trans_info, format,
                      BDD_FSM_TRANS_PRINTER_SECTION_HEADER,
                      file);

    while (true) {
      /* this is true if still in inner loop (destinations) */
      boolean inner = true;

      BddFsmTrans_print(env, &iter, &trans_info, format,
                        BDD_FSM_TRANS_PRINTER_SECTION_BODY,
                        file);

      BddFsmTrans_deinit(&trans_info);
      if (!BddFsm_trans_iter_next(self, &iter, &trans_info, &inner))
        break;  /* end of iteration */

      if (iter.trans_counter > MAX_ELEMENTS_PRINTABLE) {
        StreamMgr_print_error(streams,
                              "warning: stopped printing earlier\n");
        break;
      }
    }

    BddFsmTrans_print(env, &iter, &trans_info, format,
                      BDD_FSM_TRANS_PRINTER_SECTION_FOOTER,
                      file);

    if (BDD_FSM_TRANS_PRINTER_SMV == format) {
      OStream_dec_indent_size(file);
      BddEnc_print_bdd_end(self->enc);
    }

    BddFsm_trans_iterator_end(self, &iter);
  }

  bdd_free(self->dd, fair_trans);
}


void BddFsm_print_fair_state_input_pairs_info(
    const BddFsm_ptr self,
    const boolean print_transitions,
    OStream_ptr file)
{
  bdd_ptr fair_trans;
  bdd_ptr mask;
  double fairstates_cardinality;
  double search_space_cardinality;

  BDD_FSM_CHECK_INSTANCE(self);

  fair_trans = BddFsm_get_fair_states_inputs(self);
  mask = BddEnc_get_state_frozen_input_vars_mask_bdd(self->enc);
  bdd_and_accumulate(self->dd, &fair_trans, mask);

  fairstates_cardinality = BddEnc_get_minterms_of_bdd(self->enc, fair_trans);
  search_space_cardinality = BddEnc_get_minterms_of_bdd(self->enc, mask);
  bdd_free(self->dd, mask);

  OStream_printf(file, "Fair state-input pairs: %g (2^%g) out of %g (2^%g)\n",
          fairstates_cardinality, log(fairstates_cardinality)/log(2.0),
          search_space_cardinality, log(search_space_cardinality)/log(2.0));

  if (print_transitions) {
    BddEnc_print_set_of_state_input_pairs(self->enc, fair_trans, false,
                                          (VPFBEFNNV) NULL, file, NULL);
  }

  bdd_free(self->dd, fair_trans);
}


int BddFsm_print_reachable_states(BddFsm_ptr self,
                                  NuSMVEnv_ptr env, OStream_ptr stream,
                                  boolean verbose, boolean print_defines,
                                  boolean formula)
{
  OptsHandler_ptr const opts =
     OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const StreamMgr_ptr streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  int retval = 0;

  set_forward_search(opts);
  set_print_reachable(opts);

  StreamMgr_print_output(streams, HEADER_SEPARATOR);
  BddFsm_print_reachable_states_info(
      BDD_FSM(NuSMVEnv_get_value(env, ENV_BDD_FSM)),
      verbose, print_defines, formula, stream);
  StreamMgr_print_output(streams, HEADER_SEPARATOR);

  return retval;
}


int BddFsm_print_fair_states(BddFsm_ptr self,
                             const NuSMVEnv_ptr env,
                             const OStream_ptr outstream,
                             const boolean verbose)
{
  const StreamMgr_ptr streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_output(streams, HEADER_SEPARATOR);
  BddFsm_print_fair_states_info(self, verbose, outstream);
  StreamMgr_print_output(streams, HEADER_SEPARATOR);

  return 0;
}


int BddFsm_print_fair_transitions(BddFsm_ptr self,
                                  const NuSMVEnv_ptr env,
                                  const enum BddFsmTransPrinterFormat format,
                                  const OStream_ptr outstream)
{
  const StreamMgr_ptr streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  StreamMgr_print_output(streams, HEADER_SEPARATOR);
  BddFsm_print_fair_transitions_info(self, format, outstream);
  StreamMgr_print_output(streams, HEADER_SEPARATOR);

  return 0;
}


int BddFsm_print_fair_state_input_pairs(BddFsm_ptr self,
                                        const NuSMVEnv_ptr env,
                                        const OStream_ptr outstream,
                                        const boolean verbose)
{
  const StreamMgr_ptr streams =
      STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  StreamMgr_print_output(streams, HEADER_SEPARATOR);
  BddFsm_print_fair_state_input_pairs_info(self, verbose, outstream);
  StreamMgr_print_output(streams, HEADER_SEPARATOR);

  return 0;
}


static struct {
  const char* name;
  const enum BddFsmTransPrinterFormat value;
} bddfsm_trans_printer_formats[] = {
  {"smv", BDD_FSM_TRANS_PRINTER_SMV},
  {"csv", BDD_FSM_TRANS_PRINTER_CSV},
  {"dot", BDD_FSM_TRANS_PRINTER_DOT},
};

enum BddFsmTransPrinterFormat
BddFsm_trans_printer_format_from_string(const char* format_str)
{
  int i;
  for (i=0; i<(sizeof(bddfsm_trans_printer_formats) /
               sizeof(*bddfsm_trans_printer_formats)); ++i) {
    if (0 == strcmp(format_str, bddfsm_trans_printer_formats[i].name))
      return bddfsm_trans_printer_formats[i].value;
  }

  return BDD_FSM_TRANS_PRINTER_INVALID;
}


const char*
BddFsm_trans_printer_format_to_string(enum BddFsmTransPrinterFormat format)
{
  int i;
  for (i=0; i<(sizeof(bddfsm_trans_printer_formats) /
               sizeof(*bddfsm_trans_printer_formats)); ++i) {
    if (format == bddfsm_trans_printer_formats[i].value)
      return bddfsm_trans_printer_formats[i].name;
  }

  return NULL;
}


enum BddFsmTransPrinterFormat*
BddFsm_trans_printer_get_avail_formats(size_t* num)
{
  enum BddFsmTransPrinterFormat* res;
  int i;

  *num = (sizeof(bddfsm_trans_printer_formats) /
          sizeof(*bddfsm_trans_printer_formats));
  res = ALLOC(enum BddFsmTransPrinterFormat, *num);
  nusmv_assert(NULL != res);
  for (i=0; i<(sizeof(bddfsm_trans_printer_formats) /
               sizeof(*bddfsm_trans_printer_formats)); ++i)
    res[i] = bddfsm_trans_printer_formats[i].value;

  return res;
}


/* ---------------------------------------------------------------------- */
/*                         Static functions                               */
/* ---------------------------------------------------------------------- */

/* this is the printer for the CSV format */
static void BddFsmTrans_print_dot(
    const NuSMVEnv_ptr env,
    const struct BddFsmTransIterator* iter,
    const struct BddFsmTrans* trans_info,
    const enum BddFsmTransPrinterSection section,
    OStream_ptr file,
    void* arg)
{
  const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  switch (section) {
    case BDD_FSM_TRANS_PRINTER_SECTION_HEADER:
      OStream_printf(file, "strict digraph {\n");
      break;

    case BDD_FSM_TRANS_PRINTER_SECTION_BODY:
      OStream_printf(file,
                     "%d -> %d\n",
                     trans_info->source_id, trans_info->dest_id);
      break;

    case BDD_FSM_TRANS_PRINTER_SECTION_FOOTER:
      {  /* dump legends for states and labels, to explain IDs */
        const char* lagend_header = "{ rank = sink;\n"
            "  Legend [shape=none, margin=0, label=<\n"
            "    <TABLE BORDER=\"1\" CELLBORDER=\"0\""
            " CELLSPACING=\"0\" CELLPADDING=\"2\">\n"
            "     <TR>\n"
            "      <TD COLSPAN=\"2\"><B>States Legend</B></TD>\n"
            "     </TR>\n";
        const char* legend_footer = " </TABLE> >];\n}\n";

        const char* state_prefix =
            "<TR><TD><FONT COLOR=\"blue\">%d:</FONT></TD><TD><FONT COLOR=\"black\">";
        const char* state_suffix = "</FONT></TD></TR>\n";
        int i;

        OStream_printf(file, lagend_header);

        for (i=0; i < VECTOR_SIZE(iter->id2bdd); ++i) {
          int id = i+1;
          bdd_ptr bdd = VECTOR_AT(iter->id2bdd, i);
          node_ptr values =
              BddEnc_assign_symbols(iter->enc, bdd,
                                    iter->sf_symb_list,
                                    true, NULL);
          node_ptr l;

          OStream_printf(file, state_prefix, id);
          for (l=values; Nil != l; l = cdr(l)) {
            OStream_nprintf(file, wffprint, "%N%s", car(l),
                            (Nil != cdr(l))? ", " : "");
          }
          free_list(nodemgr, values);
          OStream_printf(file, state_suffix);
        }
        OStream_printf(file, legend_footer);

        /* close the digraph */
        OStream_printf(file, "\n}");
        break;
      }

    default:
      {
        const ErrorMgr_ptr errmgr =
            ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
        ErrorMgr_internal_error(errmgr, "Invalid section");
      }
  }
}


/* this is the printer for the CSV format */
static void BddFsmTrans_print_csv(
    const NuSMVEnv_ptr env,
    const struct BddFsmTransIterator* iter,
    const struct BddFsmTrans* trans_info,
    const enum BddFsmTransPrinterSection section,
    OStream_ptr file,
    void* arg)
{
  const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  const char* sep = "\t";
  const struct {
    const NodeList_ptr list;
    const bdd_ptr bdd;
  } symb_bdd_pairs[] = {
    {iter->sf_symb_list, trans_info->source},  /* state and frozen */
    {iter->i_symb_list, trans_info->source},  /* inputs */
    {iter->n_symb_list, trans_info->dest_next},  /* next and frozen */
  };

  switch (section) {

    case BDD_FSM_TRANS_PRINTER_SECTION_HEADER:
      {  /* print headers: states, inputs, next states, IDs */
        const int pairs = sizeof(symb_bdd_pairs)/sizeof(*symb_bdd_pairs);
        int i;
        for (i=0; i < pairs; ++i) {
          ListIter_ptr iter;
          NODE_LIST_FOREACH(symb_bdd_pairs[i].list, iter) {
            node_ptr var = NodeList_get_elem_at(symb_bdd_pairs[i].list, iter);
            OStream_nprintf(file, wffprint, "\"%N\"%s", var, sep);
          }
        }
        /* current and next state id (header) */
        OStream_printf(file, "CurrID%sNextID", sep);

        OStream_printf(file, "\n");
        break;
      }

    case BDD_FSM_TRANS_PRINTER_SECTION_BODY:
      {  /* print currs, inputs, nexts */
        const int pairs = sizeof(symb_bdd_pairs)/sizeof(*symb_bdd_pairs);
        int i;
        for (i=0; i < pairs; ++i) {
          node_ptr values =
              BddEnc_assign_symbols(trans_info->enc, symb_bdd_pairs[i].bdd,
                                    symb_bdd_pairs[i].list,
                                    false, NULL);
          node_ptr l;

          for (l=values; Nil != l; l = cdr(l)) {
            OStream_nprintf(file, wffprint, "\"%N\"%s", cdr(car(l)), sep);
          }
          free_list(nodemgr, values);
        }


        /* current and next state ids (values) */
        OStream_printf(file,
                       "%d%s%d",
                       trans_info->source_id, sep, trans_info->dest_id);

        OStream_printf(file, "\n");
        break;
      }

    case BDD_FSM_TRANS_PRINTER_SECTION_FOOTER:
      break;

    default:
      {
        const ErrorMgr_ptr errmgr =
            ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
        ErrorMgr_internal_error(errmgr, "Invalid section");
      }
  }
}


/* this is the printer for SMV format (the list printed by default) */
static void BddFsmTrans_print_smv(
    const NuSMVEnv_ptr env,
    const struct BddFsmTransIterator* iter,
    const struct BddFsmTrans* trans_info,
    const enum BddFsmTransPrinterSection section,
    OStream_ptr file,
    void* arg)
{

  if (BDD_FSM_TRANS_PRINTER_SECTION_BODY != section)
    return;

  OStream_printf(
      file, "------- Trans: %4.f (Curr State ID: %d, Next State ID: %d) ------\n",
      iter->trans_counter, trans_info->source_id, trans_info->dest_id);
  /* prints the set of states... */
  BddEnc_print_bdd(iter->enc, trans_info->source, (VPFBEFNNV) NULL, file, NULL);

  /* print inputs */
  OStream_inc_indent_size(file);
  BddEnc_print_bdd_begin(iter->enc, iter->i_symb_list, false);
  BddEnc_print_bdd(iter->enc, trans_info->source, (VPFBEFNNV) NULL, file, NULL);
  BddEnc_print_bdd_end(iter->enc);
  OStream_dec_indent_size(file);

  /* print next */
  BddEnc_print_bdd_begin(iter->enc, iter->n_symb_list, false);
  BddEnc_print_bdd(iter->enc, trans_info->dest_next, (VPFBEFNNV) NULL, file, NULL);
  BddEnc_print_bdd_end(iter->enc);

  if (iter->trans_counter > MAX_ELEMENTS_PRINTABLE) {
    OStream_printf(file,
                   "\n-- reached limit of number of transitions: "
                   "there are more than %d transitions\n",
                   MAX_ELEMENTS_PRINTABLE);
  }
}


static void BddFsmTrans_print(
    const NuSMVEnv_ptr env,
    const struct BddFsmTransIterator* iter,
    const struct BddFsmTrans* trans_info,
    const enum BddFsmTransPrinterFormat format,
    const enum BddFsmTransPrinterSection section,
    OStream_ptr file)
{
  void (*printer)(
      const NuSMVEnv_ptr env,
      const struct BddFsmTransIterator* iter,
      const struct BddFsmTrans* trans_info,
      const enum BddFsmTransPrinterSection section,
      OStream_ptr file,
      void* arg);

  void* arg = NULL;

  switch (format) {
    case BDD_FSM_TRANS_PRINTER_SMV:
      printer = BddFsmTrans_print_smv;
      break;

    case BDD_FSM_TRANS_PRINTER_CSV:
      printer = BddFsmTrans_print_csv;
      break;

    case BDD_FSM_TRANS_PRINTER_DOT:
      printer = BddFsmTrans_print_dot;
      break;

    default:
      {
        const ErrorMgr_ptr errmgr =
            ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));

        ErrorMgr_internal_error(errmgr, "Invalid value for format: %d\n", format);
      }
  }

  printer(env, iter, trans_info, section, file, arg);
}


/* returned bdd is NOT referenced */
static bdd_ptr BddFsmMinterArrayIterator_get(
    const struct BddFsmMinterArrayIterator* self)
{
  nusmv_assert(self->idx < self->array_size);
  return self->array[self->idx];
}


/* output bdd is NOT referenced */
static int BddFsmMinterArrayIterator_begin(
    struct BddFsmMinterArrayIterator* self,
    bdd_ptr bdd, BddEnc_ptr enc,
    boolean contains_inputs,
    /* outputs */
    bdd_ptr* first_bdd)
{
  boolean res;

  self->idx = 0;
  self->iter_bdd = bdd_dup(bdd);
  if (contains_inputs)
    self->array_size = BddEnc_count_states_inputs_of_bdd(enc, bdd);
  else
    self->array_size = BddEnc_count_states_of_bdd(enc, bdd);
  nusmv_assert(self->array_size >= 0);

  self->array = ALLOC(bdd_ptr, self->array_size);
  nusmv_assert(NULL != self->array);

  if (contains_inputs)
    res = BddEnc_pick_all_terms_states_inputs(
        enc, bdd, self->array, self->array_size);
  else
    res = BddEnc_pick_all_terms_states(
        enc, bdd, self->array, self->array_size);
  nusmv_assert(!res);  /* an error occurred */

  if (self->array_size > 0) {
    *first_bdd = BddFsmMinterArrayIterator_get(self);
    return 1;
  }
  else {
    *first_bdd = NULL;
    return 0;
  }
}


static void BddFsmMinterArrayIterator_end(
    struct BddFsmMinterArrayIterator* self,
    BddEnc_ptr enc)
{
  int j;

  /* free the bdd array */
  for (j=0; j < self->array_size; ++j) {
    bdd_free(BddEnc_get_dd_manager(enc), self->array[j]);
  }
  if (NULL != self->array) {
    FREE(self->array);
  }

  if (NULL != self->iter_bdd) {
    bdd_free(BddEnc_get_dd_manager(enc), self->iter_bdd);
  }

  /* reset the iterator structure for possible reuse */
  memset(self, 0, sizeof(*self));
}


/* returned bdd is NOT referenced */
static int BddFsmMinterArrayIterator_next(
    struct BddFsmMinterArrayIterator* self,
    bdd_ptr* bdd)
{
  if (self->idx < self->array_size-1) {
    self->idx += 1;
    *bdd = BddFsmMinterArrayIterator_get(self);
    return 1;
  }

  *bdd = NULL;
  return 0;
}


/* returns the readable ID of a given bdd */
static int
BddFsm_trans_iterator_get_bdd_id(struct BddFsmTransIterator* iter,
                                 bdd_ptr bdd)
{  /* computes the source id if needed */
  int id = PTR_TO_INT(find_assoc(iter->bdd2id, (node_ptr) bdd));
  if (0 == id) {
    id = ++iter->id_counter;
    nusmv_assert(0 != id);
    insert_assoc(iter->bdd2id, (node_ptr) bdd_dup(bdd),
                 PTR_FROM_INT(node_ptr, id));

    /* not referenced as bdd2id protects from dereferencing. The index
     * is ID-1 */
    Vector_push(iter->id2bdd, bdd);
  }

  return id;
}


static boolean _iter_filter_no_bits(const SymbTable_ptr self,
                                    const node_ptr sym,
                                    void* arg)
{
  return !BoolEnc_is_var_bit(BOOL_ENC(arg), sym);
}


static void BddFsm_trans_iterator_compute_dest_image(
    const BddFsm_ptr self,
    struct BddFsmTransIterator* iter,
    struct BddFsmTrans* trans)
{
  bdd_ptr bdd;
  bdd_ptr image;
  bdd_ptr mask_image;
  BddStates one = bdd_true(self->dd);
  int res;

  nusmv_assert(self->enc == trans->enc);
  nusmv_assert(NULL != trans->source);

  image = BddFsm_get_constrained_forward_image(self, one, trans->source);
  /* not clear if there other constraints (fair states?) */

  mask_image = BddEnc_apply_state_frozen_vars_mask_bdd(trans->enc, image);

  bdd_free(self->dd, one);
  bdd_free(self->dd, image);

  res = BddFsmMinterArrayIterator_begin(&(iter->dest_iter), mask_image,
                                        iter->enc, false, &bdd);
  bdd_free(self->dd, mask_image);

  nusmv_assert(res);
  trans->dest_curr = bdd_dup(bdd);
  trans->dest_next = BddEnc_state_var_to_next_state_var(iter->enc, bdd);
  trans->dest_id = BddFsm_trans_iterator_get_bdd_id(iter, bdd);
}


/* top level for starting iteration among transitions in given bdd */
static void BddFsm_trans_iterator_begin(const BddFsm_ptr self,
                                        bdd_ptr state_input,
                                        /* outputs: */
                                        struct BddFsmTransIterator* iter,
                                        struct BddFsmTrans* trans)
{
  int res;
  bdd_ptr bdd;

  BDD_FSM_CHECK_INSTANCE(self);

  iter->fsm = self;
  iter->enc = BddFsm_get_bdd_encoding(self);

  iter->id_counter = 0;
  iter->bdd2id = new_assoc();  /* referenced bdd -> number */
  iter->id2bdd = Vector_create();

  trans->enc = iter->enc;

  res = BddFsmMinterArrayIterator_begin(&(iter->source_iter), state_input,
                                        iter->enc, true, &bdd);
  nusmv_assert(res);
  trans->source = bdd_dup(bdd);

  trans->source_curr = BddFsm_states_inputs_to_states(self, bdd);
  trans->source_id = BddFsm_trans_iterator_get_bdd_id(iter, trans->source_curr);

  BddFsm_trans_iterator_compute_dest_image(self, iter, trans);

  {  /* prepare symbol lists */
    SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(iter->enc));
    SymbTableIter sym_iter;

    /* state and frozen vars */
    SymbTable_gen_iter(st, &sym_iter, STT_STATE_VAR | STT_FROZEN_VAR);
    SymbTable_iter_set_filter(st, &sym_iter, _iter_filter_no_bits,
      BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(iter->enc)));
    iter->sf_symb_list = SymbTable_iter_to_list(st, sym_iter);

    {  /* next vars */
      NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(iter->enc));
      const NodeMgr_ptr nodemgr =
          NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
      ListIter_ptr list_iter;

      iter->n_symb_list = NodeList_create();
      NODE_LIST_FOREACH(iter->sf_symb_list, list_iter) {
        node_ptr v = NodeList_get_elem_at(iter->sf_symb_list, list_iter);
        nusmv_assert(SymbTable_is_symbol_state_frozen_var(st, v));

        NodeList_append(iter->n_symb_list, find_node(nodemgr, NEXT, v, Nil));
      }
    }

    /* input vars */
    SymbTable_gen_iter(st, &sym_iter, STT_INPUT_VAR);
    SymbTable_iter_set_filter(st, &sym_iter, _iter_filter_no_bits,
      BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(iter->enc)));

    iter->i_symb_list = SymbTable_iter_to_list(st, sym_iter);
  }

  iter->trans_counter = iter->pair_counter = 1;
}


/* this is used by BddFsm_trans_iterator_end to clear internal cache */
static enum st_retval
bdd_fsm_trans_iter_free_bdd_num_cache(char* key, char* val, char* dd)
{
  if (NULL != key) {
    bdd_free((DDMgr_ptr) dd, (bdd_ptr) key);
  }
  return ST_DELETE;
}


/* call when done with the BddFsmTransIterator iterator */
static void BddFsm_trans_iterator_end(const BddFsm_ptr self,
                                      struct BddFsmTransIterator* iter)
{
  nusmv_assert(self == iter->fsm);

  Vector_destroy(iter->id2bdd);
  clear_assoc_and_free_entries_arg(iter->bdd2id,
                                   bdd_fsm_trans_iter_free_bdd_num_cache,
                                   (char*) BddEnc_get_dd_manager(iter->enc));
  free_assoc(iter->bdd2id);

  NodeList_destroy(iter->i_symb_list);
  NodeList_destroy(iter->n_symb_list);
  NodeList_destroy(iter->sf_symb_list);

  BddFsmMinterArrayIterator_end(&(iter->dest_iter), iter->enc);
  BddFsmMinterArrayIterator_end(&(iter->source_iter), iter->enc);
}


/* call to perform the traversal */
static int BddFsm_trans_iter_next(const BddFsm_ptr self,
                                  struct BddFsmTransIterator* iter,
                                  /* outputs: */
                                  struct BddFsmTrans* trans,
                                  boolean* inside_inner)
{
  int res;
  bdd_ptr next_dest;

  res = BddFsmMinterArrayIterator_next(&(iter->dest_iter), &next_dest);

  if (res) {
    /* smooth of iteration in the inner loop */
    bdd_ptr source = BddFsmMinterArrayIterator_get(&(iter->source_iter));
    if (source != trans->source) {
      trans->enc = iter->enc;
      trans->source = bdd_dup(
          BddFsmMinterArrayIterator_get(&(iter->source_iter)));

      trans->source_curr = BddFsm_states_inputs_to_states(self, trans->source);
      trans->source_id = BddFsm_trans_iterator_get_bdd_id(iter, trans->source_curr);
    }

    trans->dest_curr = bdd_dup(next_dest);
    trans->dest_next = BddEnc_state_var_to_next_state_var(iter->enc, next_dest);
    trans->dest_id = BddFsm_trans_iterator_get_bdd_id(iter, next_dest);

    iter->trans_counter += 1;
    *inside_inner = true;
  }
  else {
    /* end of iteration in the inner loop */
    bdd_ptr next_source;

    *inside_inner = false;
    BddFsmMinterArrayIterator_end(&(iter->dest_iter), iter->enc);

    res = BddFsmMinterArrayIterator_next(&(iter->source_iter),
      &next_source);

    iter->trans_counter += 1;
    iter->pair_counter += 1;

    if (res) {
      /* smooth iteration in the outer loop, start new iteration in
       * the inner one */
      trans->enc = iter->enc;
      trans->source = bdd_dup(next_source);
      trans->source_curr = BddFsm_states_inputs_to_states(self, trans->source);
      trans->source_id = BddFsm_trans_iterator_get_bdd_id(iter, trans->source_curr);

      BddFsm_trans_iterator_compute_dest_image(self, iter, trans);
    }
    else {  /* end of iteration of outer loop */
      memset(trans, 0, sizeof(*trans));
      return 0;  /* end iteration */
    }
  }

  return 1;  /* keep iteration */
}


static void BddFsmTrans_deinit(struct BddFsmTrans* self)
{
  if (NULL != self->enc) {
    if (NULL != self->source) {
      bdd_free(BddEnc_get_dd_manager(self->enc), self->source);
    }
    if (NULL != self->source_curr) {
      bdd_free(BddEnc_get_dd_manager(self->enc), self->source_curr);
    }
    if (NULL != self->dest_next) {
      bdd_free(BddEnc_get_dd_manager(self->enc), self->dest_next);
    }
    if (NULL != self->dest_curr) {
      bdd_free(BddEnc_get_dd_manager(self->enc), self->dest_curr);
    }
  }
  memset(self, 0, sizeof(*self));
}
