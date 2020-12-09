/* ---------------------------------------------------------------------------

   This file is part of the ``compile'' package of NuSMV version 2.
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
  \author Marco Roveri, Roberto Cavada, Cristian Mattarei
  \brief Creation of a UDG file containing the flattened or booleanized
   model.

  Creation of a UDG file containing the flattened or
   booleanized model, processes will be removed by explicitly
   introducing a process variable and modifying assignments to take
   care of inertia.

*/



#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#include "nusmv/core/compile/compileInt.h"
#include "nusmv/core/compile/compile.h"

#include "nusmv/core/compile/symb_table/SymbLayer.h"
#include "nusmv/core/compile/symb_table/SymbType.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/psl/pslNode.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/ustring.h"


/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
typedef enum ModelSection_TAG{
  INIT_T,
  INVAR_T,
  DEFINE_T,
  ASSIGN_T,
  TRANS_T,
  INVARSPEC_T,
  LTLSPEC_T,
  CTLSPEC_T,
  JUSTICE_T,
  FAIRNESS_T,
  COMPASSION_T
}ModelSectionTag;

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Define declarations                                                       */
/*---------------------------------------------------------------------------*/

//#define DEBUG

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define VAR_COLOR "#ffff00"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CONS_COLOR "#00ff00"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NEXT_COLOR "#00ffff"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define INIT_COLOR "#ff8000"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define INPUT_CASE_COLOR "#0000ff"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CONTROL_CASE_COLOR "#ff0000"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define STANDARD_COLOR "#000000"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define INVAR_COLOR "#ff8000"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define EDGE_ATTS "a(\"EDGEPATTERN\",\"single;solid;1;0\"),\
                   a(\"HEAD\",\"farrow\"),a(\"_DIR\",\"inverse\")],"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define EDGE_ATTS_1 "a(\"EDGEPATTERN\",\"single;solid;1;0\"),\
                     a(\"HEAD\",\"farrow\"),a(\"_DIR\",\"normal\")],"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define EDGE_ATTS_C "a(\"EDGEPATTERN\",\"single;solid;1;0\"),\
                     a(\"HEAD\",\"farrow\"),a(\"_DIR\",\"inverse\"),\
                     a(\"EDGECOLOR\",\""

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NODE_STYLE_1 "\"), a(\"_GO\",\"rhombus"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CLOSE_NODE ")]))"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CLOSE_EDGE "\")],"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NODE_1_UDG "l(\"%p\",n(\"node\",[a(\"COLOR\",\""

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NODE_1E_UDG "l(\"\",n(\"node\",[a(\"COLOR\",\""

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NODE_2_UDG "\"), a(\"OBJECT\",\""

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NODE_2_UDG_I "\"), a(\"INFO\",\""

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define NODE_3_UDG "\")]"

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define EDGE_1_UDG "e(\"anything\",["


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
/* Do not create reentrancy problems */
static unsigned int defines_count = 0;
static unsigned int dag_hits = 0;

hash_ptr visited;
#ifdef DEBUG
int nvisited = 0;
#endif

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/
/* these control the way dag for dumping is created */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define DAG_ENABLED 1

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define GET_DAG_HITS_NUMBER() dag_hits

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define RESET_DAG_HITS_NUMBER() dag_hits = 0

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define INCREMENT_HITS_NUMBER() dag_hits++

/*!
  \brief Short way of calling printer_base_throw_print_node

  Use this macro to recursively recall print_node
*/

#define _THROW(n)  compile_write_udg_print_node(env, out, n, true, shared, "")


/*!
  \brief Short way of print a node with sharing

  Short way of print a node with sharing
*/

#define _PRINT_WS(code, str, style, color)                              \
  ((Nil != find_assoc(visited, code) && (shared))?                      \
   fprintf(out, "r(\"%p\")", (void*)code):                              \
   (insert_assoc(visited, code,(void*)1),                               \
    fprintf(out,                                                        \
            NODE_1_UDG color NODE_2_UDG                                 \
            "%s" style NODE_3_UDG "%s",                                 \
            (void*)code,                                                \
            str, (close?",[]))":",["))))

/*!
  \brief Short way of print a node without sharing

  Short way of print a node without sharing
*/


#define _PRINT_NS(code, str, style, color)                              \
  (insert_assoc(visited, code,(void*)1) ,                               \
   fprintf(out, NODE_1E_UDG color NODE_2_UDG                            \
           "%s" style NODE_3_UDG "%s",                                  \
           str, (close?",[]))":",[")))


/*!
  \brief Short way of print a node

  Short way of print a node. The sharing depends on shared
                       variable
*/


#define _PRINT(code, str, style, color)                                 \
  (shared?_PRINT_WS(code,str,style,color):                              \
   _PRINT_NS(code,str,style,color))

#define _NPRINT(code, str) 1

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void compile_print_assign_udg(const NuSMVEnv_ptr env,
                                     SymbTable_ptr st,
                                     FILE * out,
                                     node_ptr lhs,
                                     node_ptr rhs,
                                     hash_ptr dag_info,
                                     hash_ptr defines);

static int
compile_write_udg_flat_define(const NuSMVEnv_ptr env,
                              const SymbTable_ptr symb_table,
                              FILE* out,
                              const SymbLayer_ptr layer,
                              const SymbTableType sym_mask,
                              hash_ptr,
                              hash_ptr);

static int
compile_write_udg_flat_asgn(const NuSMVEnv_ptr env,
                            const SymbTable_ptr symb_table,
                            FILE* out,
                            const SymbLayer_ptr layer,
                            const SymbTableType sym_mask,
                            FlatHierarchy_ptr hierarchy,
                            hash_ptr,
                            hash_ptr);

static int
compile_write_udg_flatten_vars(const SymbTable_ptr symb_table,
                               FILE* out,
                               const SymbLayer_ptr layer,
                               const SymbTableType var_type);

static int
compile_write_udg_flatten_bool_vars(const NuSMVEnv_ptr env,
                                    const SymbTable_ptr symb_table,
                                    const BoolEnc_ptr bool_enc,
                                    FILE* out,
                                    const SymbLayer_ptr layer,
                                    const SymbTableType sym_mask);
static int
compile_write_udg_flatten_expr(const NuSMVEnv_ptr env,
                               const SymbTable_ptr symb_table,
                               FILE* out, node_ptr l, ModelSectionTag mst,
                               hash_ptr, hash_ptr);

static int
compile_write_udg_flatten_expr_split(const NuSMVEnv_ptr env,
                                     const SymbTable_ptr symb_table,
                                     FILE* out, node_ptr n,
                                     ModelSectionTag mst,
                                     hash_ptr dag_info,
                                     hash_ptr defines);

static int
compile_write_udg_flatten_spec(const NuSMVEnv_ptr env,
                               const SymbTable_ptr symb_table,
                               FILE* out, node_ptr l, ModelSectionTag mst,
                               hash_ptr, hash_ptr);

static int
compile_write_udg_flatten_spec_split(const NuSMVEnv_ptr env,
                                     const SymbTable_ptr symb_table,
                                     FILE* out, node_ptr n,
                                     ModelSectionTag mst,
                                     hash_ptr dag_info,
                                     hash_ptr defines);

static int
compile_write_udg_flatten_expr_pair(const NuSMVEnv_ptr env,
                                    const SymbTable_ptr symb_table,
                                    FILE* out, node_ptr l,
                                    ModelSectionTag mst,
                                    hash_ptr, hash_ptr);

static int
compile_write_udg_flatten_bfexpr(const NuSMVEnv_ptr env,
                                 BddEnc_ptr enc,
                                 const SymbTable_ptr symb_table,
                                 SymbLayer_ptr det_layer,
                                 FILE* out, node_ptr n, const char* s,
                                 hash_ptr, hash_ptr);

static int
compile_write_udg_flatten_psl(const NuSMVEnv_ptr env,
                              const SymbTable_ptr symb_table,
                              FILE* out, node_ptr n,
                              hash_ptr, hash_ptr);

static void
compile_write_udg_flat_fsm(const NuSMVEnv_ptr env,
                           FILE* out,
                           const SymbTable_ptr symb_table,
                           const array_t* layer_names,
                           const char* fsm_name,
                           FlatHierarchy_ptr hierarchy,
                           hash_ptr dag_info, hash_ptr defines);

static void
compile_write_udg_bool_fsm(const NuSMVEnv_ptr env,
                           FILE* out,
                           const SymbTable_ptr symb_table,
                           NodeList_ptr layers,
                           const char* fsm_name,
                           BoolSexpFsm_ptr bool_sexp_fsm,
                           hash_ptr dag_info, hash_ptr defines);

static void
compile_write_udg_flat_specs(const NuSMVEnv_ptr env,
                             FILE* out,
                             const SymbTable_ptr st,
                             FlatHierarchy_ptr hierarchy,
                             hash_ptr dag_info, hash_ptr defines);

static void
compile_write_udg_bool_spec(const NuSMVEnv_ptr env,
                            FILE* out, BddEnc_ptr enc,
                            node_ptr spec, const char* msg,
                            SymbLayer_ptr det_layer,
                            hash_ptr dag_info, hash_ptr defines);

static void
compile_write_udg_bool_specs(const NuSMVEnv_ptr env, FILE* out,
                             BddEnc_ptr enc,
                             SymbLayer_ptr det_layer,
                             FlatHierarchy_ptr hierarchy,
                             hash_ptr dag_info,
                             hash_ptr defines);

static int compile_write_udg_constants(const SymbTable_ptr symb_table,
                                       FILE* out);

static hash_ptr
compile_create_dag_info_from_hierarchy_udg(SymbTable_ptr st,
                                           FlatHierarchy_ptr hierarchy,
                                           SymbLayer_ptr det_layer,
                                           BddEnc_ptr enc);

static node_ptr compile_pack_dag_info_udg(NodeMgr_ptr nodemgr,
                                          unsigned int count,
                                          unsigned int depth);
static void compile_unpack_dag_info_udg(node_ptr info,
                                        unsigned int* count,
                                        unsigned int* depth);

static void
compile_set_dag_info_udg(node_ptr info,
                         unsigned int count, unsigned int depth);

static node_ptr compile_convert_to_dag_aux_udg(const NuSMVEnv_ptr env,
                                               SymbTable_ptr symb_table,
                                               node_ptr expr, hash_ptr hash,
                                               unsigned int num_thres,
                                               unsigned int dep_thres,
                                               hash_ptr defines,
                                               const char* defines_prefix);


static node_ptr compile_make_dag_info_aux_udg(const NuSMVEnv_ptr env,
                                              node_ptr expr,
                                              hash_ptr hash);

static assoc_retval compile_free_node_udg(char *key,
                                          char *data,
                                          char * arg);

static assoc_retval compile_free_define_udg(char *key,
                                            char *data,
                                            char * arg);

static int
compile_write_flat_array_define_udg(const NuSMVEnv_ptr env,
                                    const SymbTable_ptr symb_table,
                                    FILE* out,
                                    const SymbLayer_ptr layer,
                                    const SymbTableType sym_mask,
                                    hash_ptr dag_info,
                                    hash_ptr defines);

static boolean
is_array_define_cell_udg(const SymbTable_ptr st, const node_ptr name);

static node_ptr
compile_write_udg_flatten_array_define(SymbTable_ptr st, node_ptr body,
                                        node_ptr context);

static int compile_write_udg_print_node(const NuSMVEnv_ptr env,
                                        FILE* out,
                                        node_ptr n,
                                        boolean close,
                                        boolean shared,
                                        const char* style);

static inline int insert_assoc_w(hash_ptr, node_ptr, node_ptr);

static inline int compile_write_udg_print_2_ary(const NuSMVEnv_ptr env,
                                                FILE* buffer,
                                                node_ptr code,
                                                const char* str,
                                                boolean close,
                                                boolean shared,
                                                const char* color1,
                                                const char* color2);

static inline int compile_write_udg_print_2_arya(const NuSMVEnv_ptr env,
                                                 FILE* buffer,
                                                 node_ptr code,
                                                 const char* str,
                                                 boolean close,
                                                 boolean shared);


static inline int compile_write_udg_print_1_ary(const NuSMVEnv_ptr env,
                                                FILE* buffer,
                                                node_ptr code,
                                                const char* str,
                                                boolean close,
                                                boolean shared,
                                                const char* color1);

static inline int compile_write_udg_print_3_aryc(const NuSMVEnv_ptr env,
                                                 FILE* buffer,
                                                 node_ptr code,
                                                 const char* str,
                                                 node_ptr fst,
                                                 node_ptr snd,
                                                 node_ptr trd,
                                                 boolean close,
                                                 boolean shared);

static inline int
compile_write_udg_print_3_aryc_color(const NuSMVEnv_ptr env,
                                     FILE* buffer,
                                     node_ptr code,
                                     const char* str,
                                     node_ptr fst,
                                     node_ptr snd,
                                     node_ptr trd,
                                     boolean close,
                                     boolean shared,
                                     const char* color1,
                                     const char* color2,
                                     const char* color3);

#ifdef DEBUG
static void compile_write_print_node_type(FILE* buffer, node_ptr node);
#endif

static void Compile_write_dag_defines_udg(const NuSMVEnv_ptr env,
                                          FILE* out, hash_ptr defines);

static void Compile_destroy_dag_info_udg(const NuSMVEnv_ptr env,
                                         hash_ptr dag_info, hash_ptr defines);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Compile_WriteFlattenModel_udg(const NuSMVEnv_ptr env,
                                   FILE* out,
                                   const SymbTable_ptr st,
                                   const array_t* layer_names,
                                   const char* fsm_name,
                                   FlatHierarchy_ptr hierarchy)
{
  /* these are used for making the tree a dag */
  hash_ptr dag_info = (hash_ptr) NULL;
  hash_ptr defines = (hash_ptr) NULL;

  visited = new_assoc();

  RESET_DAG_HITS_NUMBER();

#if DAG_ENABLED
  dag_info = compile_create_dag_info_from_hierarchy_udg(st,
                                                        hierarchy,
                                                        SYMB_LAYER(NULL),
                                                        BDD_ENC(NULL));
  defines = new_assoc();
  nusmv_assert(defines != (hash_ptr) NULL);
#endif

  /* dumps the FSM */
  compile_write_udg_flat_fsm(env, out, st, layer_names, fsm_name, hierarchy,
                             dag_info, defines);

  /* dumps the specifications */
  compile_write_udg_flat_specs(env, out, st, hierarchy, dag_info, defines);

#if DAG_ENABLED
  Compile_write_dag_defines_udg(env, out, defines);
  Compile_destroy_dag_info_udg(env, dag_info, defines);
#endif

  if (defines != (hash_ptr) NULL) free_assoc(defines);
  if (dag_info != (hash_ptr) NULL) free_assoc(dag_info);
}

void Compile_WriteFlattenFsm_udg(const NuSMVEnv_ptr env,
                                 FILE* out,
                                 const SymbTable_ptr st,
                                 const array_t* layer_names,
                                 const char* fsm_name,
                                 FlatHierarchy_ptr hierarchy)
{
  compile_write_udg_flat_fsm(env, out, st, layer_names, fsm_name, hierarchy,
                             (hash_ptr) NULL, (hash_ptr) NULL);
}

void Compile_WriteFlattenSpecs_udg(const NuSMVEnv_ptr env,
                                   FILE* out,
                                   const SymbTable_ptr st,
                                   FlatHierarchy_ptr hierarchy)
{
  compile_write_udg_flat_specs(env, out, st, hierarchy,
                               (hash_ptr) NULL, (hash_ptr) NULL);
}

void Compile_WriteBoolModel_udg(const NuSMVEnv_ptr env,
                                FILE* out,
                                BddEnc_ptr enc,
                                NodeList_ptr layers,
                                const char* fsm_name,
                                BoolSexpFsm_ptr bool_sexp_fsm)
{
  FlatHierarchy_ptr fh;
  SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(enc));
  SymbLayer_ptr det_layer;

  /* these are used for making the tree a dag */
  hash_ptr dag_info = (hash_ptr) NULL;
  hash_ptr defines = (hash_ptr) NULL;

  det_layer = SymbTable_create_layer(st, (char*) NULL, /*temp name*/
                                     SYMB_LAYER_POS_DEFAULT);

  NodeList_append(layers, (node_ptr) det_layer);

  fh = SexpFsm_get_hierarchy(SEXP_FSM(bool_sexp_fsm));
#if DAG_ENABLED
  dag_info = compile_create_dag_info_from_hierarchy_udg(st, fh,
                                                        det_layer, enc);
  defines = new_assoc();
  nusmv_assert(defines != (hash_ptr) NULL);
#endif

  /* dumps the FSM */
  compile_write_udg_bool_fsm(env, out, st, layers, fsm_name, bool_sexp_fsm,
                             dag_info, defines);

  compile_write_udg_bool_specs(env, out, enc, det_layer, fh,
                               dag_info, defines);

#if DAG_ENABLED
  Compile_write_dag_defines_udg(env, out, defines);
  Compile_destroy_dag_info_udg(env, dag_info, defines);
#endif

  if (defines != (hash_ptr) NULL) free_assoc(defines);
  if (dag_info != (hash_ptr) NULL) free_assoc(dag_info);

  SymbTable_remove_layer(st, det_layer);
}

void Compile_WriteBoolFsm_udg(const NuSMVEnv_ptr env,
                              FILE* out, const SymbTable_ptr st,
                              NodeList_ptr layers, const char* fsm_name,
                              BoolSexpFsm_ptr bool_sexp_fsm)
{
  compile_write_udg_bool_fsm(env, out, st, layers, fsm_name, bool_sexp_fsm,
                             (hash_ptr) NULL, (hash_ptr) NULL);
}

void Compile_WriteBoolSpecs_udg(const NuSMVEnv_ptr env, FILE* out,
                                BddEnc_ptr enc,
                                FlatHierarchy_ptr hierarchy)
{
  SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(enc));
  SymbLayer_ptr det_layer;
  det_layer = SymbTable_create_layer(st, (char*) NULL, /*temp name*/
                                     SYMB_LAYER_POS_DEFAULT);

  compile_write_udg_bool_specs(env, out, enc, det_layer, hierarchy,
                               (hash_ptr) NULL, (hash_ptr) NULL);

  SymbTable_remove_layer(st, det_layer);
}

node_ptr Compile_make_dag_info_udg(const NuSMVEnv_ptr env,
                                   node_ptr expr, hash_ptr hash)
{
  return compile_make_dag_info_aux_udg(env, expr, hash);
}

/*!
  \brief Frees the content of given structures.

  Warning: the hashes are not freed, only the content
*/

void Compile_destroy_dag_info_udg(const NuSMVEnv_ptr env,
                                  hash_ptr dag_info, hash_ptr defines)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  clear_assoc_and_free_entries_arg(dag_info, compile_free_node_udg,
                                   (char*) nodemgr);
  clear_assoc_and_free_entries_arg(defines, compile_free_define_udg,
                                   (char*) nodemgr);
  defines_count = 0;
}

/*!
  \brief


*/

void Compile_write_dag_defines_udg(const NuSMVEnv_ptr env, FILE* out,
                                   hash_ptr defines)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  char *key, *value;
  st_generator *gen;
  boolean msg_printed = false;

  st_foreach_item(defines, gen, &key, &value) {
    node_ptr define = (node_ptr) value;
    nusmv_assert(define == Nil || node_get_type(define) == COLON);
    if (define != Nil) {
      unsigned int count = PTR_TO_INT(cdr(define));
      if (count > 0) {
        if (!msg_printed) {
          fprintf(out, "-- Symbols introduced by the dumper:\n");
          msg_printed = true;
        }
        if (opt_verbose_level_gt(opts, 0)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, "-- occurrences: %d\n", count+1);
        }
        compile_write_udg_print_node(env, out, car(define), true, true, "");
      }
    }
  }
}

node_ptr Compile_convert_to_dag_udg(const NuSMVEnv_ptr env,
                                    SymbTable_ptr symb_table,
                                    node_ptr expr,
                                    hash_ptr dag_hash,
                                    hash_ptr defines)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterNormalizer_ptr normalizer =
    MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));

  return compile_convert_to_dag_aux_udg(
      env, symb_table,
      MasterNormalizer_normalize_node(normalizer, expr), dag_hash,
      opt_get_daggifier_counter_threshold(opts),
      opt_get_daggifier_depth_threshold(opts),
      defines,
      opt_traces_hiding_prefix(opts));
}


/* node_ptr Compile_convert_to_dag_udg(node_ptr expr,  */
/*                                     hash_ptr dag_hash, hash_ptr defines) */
/* { */
  /* return compile_convert_to_dag_aux_udg( */
  /*     expr, dag_hash, */
  /*     DAG_MIN_COUNTER, */
  /*     DAG_MIN_DEPTH, */
  /*     defines, */
  /*     opt_counter_examples_hiding_prefix(opts)); */
/* } */

void Compile_print_array_define_udg(const NuSMVEnv_ptr env, FILE* out,
                                    const node_ptr n)
{
  Compile_print_array_define(env, out, n);
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Writes DEFINE declarations in SMV format on a
   file.

  Writes DEFINE declarations in SMV format on a
   file.
*/
static node_ptr
compile_write_udg_flatten_array_define(SymbTable_ptr st, node_ptr body,
                                        node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr iter;
  node_ptr list = Nil;

  nusmv_assert(ARRAY_DEF == node_get_type(body) && Nil == cdr(body));

  for (iter = car(body); iter != Nil; iter = cdr(iter)) {
    node_ptr tmp;
    nusmv_assert(CONS == node_get_type(iter));

    tmp = car(iter);

    if (ARRAY_DEF == node_get_type(tmp)) {
      /* process sub-array */
      tmp = compile_write_udg_flatten_array_define(st,
                                                tmp,
                                                context);
    }
    else {
      /* process an element */
      tmp = Compile_FlattenSexp(st, tmp, context);
    }

    list = cons(nodemgr, tmp, list);
  } /* for */


  /* reverse the order of the list and find_node it */
  body = Nil;
  for (iter = list; iter != Nil; iter = cdr(iter)) {
    body = find_node(nodemgr, CONS, car(iter), body);
  }
  free_list(nodemgr, list);

  return find_node(nodemgr, ARRAY_DEF, body, Nil);
}

/*!
  \brief Writes DEFINE declarations in SMV format on a
   file.

  Writes DEFINE declarations in SMV format on a
   file.
*/
static int compile_write_udg_flat_define(const NuSMVEnv_ptr env,
                                         const SymbTable_ptr symb_table,
                                         FILE* out,
                                         const SymbLayer_ptr layer,
                                         const SymbTableType sym_mask,
                                         hash_ptr dag_info, hash_ptr defines)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  SymbLayerIter iter;

  SYMB_LAYER_FOREACH(layer, iter, sym_mask) {
    node_ptr name = SymbLayer_iter_get_symbol(layer, &iter);
    node_ptr fdef = SymbTable_get_define_body(symb_table, name);
    node_ptr ctx = SymbTable_get_define_context(symb_table, name);

    if (ctx != NULL) fdef = Compile_FlattenSexp(symb_table, fdef, ctx);

    if (fdef != Nil) {
      /* Skip array defined cells */
      if (!is_array_define_cell_udg(symb_table, name)) {
        fprintf(out, ",");
        compile_write_udg_print_node(env, out, name, false, true, "");
        fprintf(out, EDGE_1_UDG EDGE_ATTS);
        compile_write_udg_print_node(env, out,
                                     Compile_convert_to_dag_udg(env,
                                                                symb_table,
                                                                fdef,
                                                                dag_info,
                                                                defines),
                                     true,
                                     true,
                                     "");
        fprintf(out, CLOSE_NODE);
      }
    }
    else {
      StreamMgr_print_error(
          streams,  "compile_write_udg_flat_define: Flattening failed\n");
    }
  }

  fprintf(out, "\n");
  return 1;
}

/*!
  \brief Writes ARRAY DEFINE declarations in SMV format on a
   file.

  Writes ARRAY DEFINE declarations in SMV format on a
   file.
*/
static int compile_write_flat_array_define_udg(const NuSMVEnv_ptr env,
                                               const SymbTable_ptr symb_table,
                                               FILE* out,
                                               const SymbLayer_ptr layer,
                                               const SymbTableType sym_mask,
                                               hash_ptr dag_info,
                                               hash_ptr defines)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  SymbLayerIter iter;

  if (SymbLayer_get_array_defines_num(layer) > 0) {
    fprintf(out, "MDEFINE\n");
  }

  SYMB_LAYER_FOREACH(layer, iter, sym_mask) {
    node_ptr name = SymbLayer_iter_get_symbol(layer, &iter);
    node_ptr def = SymbTable_get_array_define_body(symb_table, name);
    node_ptr ctx = SymbTable_get_array_define_context(symb_table, name);

    node_ptr fdef = compile_write_udg_flatten_array_define(symb_table,
                                                            def, ctx);

    if (fdef != Nil) {
      /* Skip partial array defines */
      if (node_get_type(name) != ARRAY) {
        fprintf(out, ",");
        compile_write_udg_print_node(env, out, name ,false, true, "");
        fprintf(out, EDGE_1_UDG EDGE_ATTS);
        compile_write_udg_print_node(env, out, fdef, true, true, "");
        fprintf(out, CLOSE_NODE);
      }
    }
    else {
      StreamMgr_print_error(
          streams,  "compile_write_udg_flat_define: Flattening failed\n");
    }
  }

  fprintf(out, "\n");
  return 1;
}

/*!
  \brief Writes flattened ASSIGN declarations in SMV format on a
   file.

  Writes flattened ASSIGN declarations in SMV format on a
   file.
*/
static int compile_write_udg_flat_asgn(const NuSMVEnv_ptr env,
                                       const SymbTable_ptr symb_table,
                                       FILE* out, const SymbLayer_ptr layer,
                                       const SymbTableType sym_mask,
                                       FlatHierarchy_ptr hierarchy,
                                       hash_ptr dag_info, hash_ptr defines)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  SymbLayerIter iter;

  SYMB_LAYER_FOREACH(layer, iter, sym_mask) {
    node_ptr name = SymbLayer_iter_get_symbol(layer, &iter);
    node_ptr init_name = find_node(nodemgr, SMALLINIT, name, Nil);
    node_ptr next_name = find_node(nodemgr, NEXT, name, Nil);
    node_ptr invar_expr = FlatHierarchy_lookup_assign(hierarchy, name);
    node_ptr init_expr = FlatHierarchy_lookup_assign(hierarchy, init_name);
    node_ptr next_expr = FlatHierarchy_lookup_assign(hierarchy, next_name);

    if ((init_expr != (node_ptr) NULL) ||
        (next_expr != (node_ptr) NULL) ||
        (invar_expr != (node_ptr) NULL)) {
    }

    if (init_expr != (node_ptr) NULL) compile_print_assign_udg(env,
                                                               symb_table,
                                                               out,
                                                               init_name,
                                                               init_expr,
                                                               dag_info,
                                                               defines);
    if (invar_expr != (node_ptr) NULL) compile_print_assign_udg(env,
                                                                symb_table,
                                                                out,
                                                                name,
                                                                invar_expr,
                                                                dag_info,
                                                                defines);
    if (next_expr != (node_ptr) NULL) compile_print_assign_udg(env,
                                                               symb_table,
                                                               out,
                                                               next_name,
                                                               next_expr,
                                                               dag_info,
                                                               defines);

    if ((init_expr != (node_ptr)NULL) ||
        (next_expr != (node_ptr)NULL) ||
        (invar_expr != (node_ptr)NULL)) {
      fprintf(out, "\n");
    }
  }

  fprintf(out, "\n");
  return 1;
}

/*!
  \brief Prints an assignement statement

  Prints an assignement statement
*/
static void compile_print_assign_udg(const NuSMVEnv_ptr env,
                                     SymbTable_ptr st, FILE * out,
                                     node_ptr lhs, node_ptr rhs,
                                     hash_ptr dag_info, hash_ptr defines)
{
  if(node_get_type(lhs) == SMALLINIT) return;
  fprintf(out, ",");
  compile_write_udg_print_node(env, out, lhs, false, false, "");
  fprintf(out, EDGE_1_UDG EDGE_ATTS);
  compile_write_udg_print_node(env, out,
                               Compile_convert_to_dag_udg(env,
                                                          st,
                                                          rhs,
                                                          dag_info,
                                                          defines),
                               true,
                               true,
                               "");
  fprintf(out, CLOSE_NODE);
}

/*!
  \brief Writes VAR, FROZENVAR, and IVAR declarations in
   SMV format on a file.


*/
static int compile_write_udg_flatten_vars(const SymbTable_ptr symb_table,
                                          FILE* out, const SymbLayer_ptr layer,
                                          const SymbTableType var_type)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  SymbLayerIter iter;
  hash_ptr printed_arrays = new_assoc();
  enum { WFV_UNKNOWN, WFV_INPUT, WFV_STATE, WFV_FROZEN } last_insert =
                                                             WFV_UNKNOWN;
  boolean printed = false;

  SymbLayer_gen_iter(layer, &iter, var_type);

  while (!SymbLayer_iter_is_end(layer, &iter)) {
    node_ptr name = SymbLayer_iter_get_symbol(layer, &iter);

    if (SymbTable_is_symbol_var(symb_table, name)) {
      printed = false;

      if (SymbTable_is_symbol_state_var(symb_table, name)
          && last_insert != WFV_STATE) {
        last_insert = WFV_STATE;
      }
      else if (SymbTable_is_symbol_frozen_var(symb_table, name)
               && last_insert != WFV_FROZEN) {
        last_insert = WFV_FROZEN;
      }
      else if (SymbTable_is_symbol_input_var(symb_table, name)
               &&
               last_insert != WFV_INPUT) {
        last_insert = WFV_INPUT;
      }

      if ((!printed) && (Nil == find_assoc(visited, name))) {
        printed = true;
        fprintf(out, NODE_1_UDG VAR_COLOR NODE_2_UDG_I, (void*) name);
        SymbType_print(SymbTable_get_var_type(symb_table, name),
                       wffprint, out);
        fprintf(out, NODE_2_UDG);
        print_node(wffprint, out, name);
        fprintf(out, "\")],[]))");
        insert_assoc(visited, name, (void*)1);
      }
    }

    SymbLayer_iter_next(layer, &iter);
    if(printed) fprintf(out, (SymbLayer_iter_is_end(layer, &iter)) ? "" : ",");
  } /* loop */

  /* Destroy the printed arrays assoc */
  free_assoc(printed_arrays);
  fprintf(out, "\n");
  return printed?1:0;
}

/*!
  \brief Writes boolean VAR, FROZENVAR and IVAR declarations in SMV
   format on a file. Non boolean vars are dumped as defines for the
   sake of readability of conterexamples.
*/
static int compile_write_udg_flatten_bool_vars(
    const NuSMVEnv_ptr env,
    const SymbTable_ptr symb_table,
    const BoolEnc_ptr bool_enc,
    FILE* out, const SymbLayer_ptr layer,
    const SymbTableType sym_mask)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  SymbLayerIter iter;
  enum { WFV_UNKNOWN, WFV_DEFINE,
         WFV_INPUT, WFV_STATE, WFV_FROZEN } last_insert = WFV_UNKNOWN;


  SYMB_LAYER_FOREACH(layer, iter, sym_mask) {
    node_ptr name = SymbLayer_iter_get_symbol(layer, &iter);

    if (SymbTable_is_symbol_var(symb_table, name)) {
      if (!SymbTable_is_symbol_bool_var(symb_table, name)) {
        if (!opt_backward_comp(opts)) {
          /* dumps the scalar variable as a define */
          node_ptr body;
          if (last_insert != WFV_DEFINE) {
            fprintf(out, "DEFINE\n");
            last_insert = WFV_DEFINE;
          }
          print_node(wffprint, out, name);
          fprintf(out, " := ");
          body = BoolEnc_get_var_encoding(bool_enc, name);
          print_node(wffprint, out, body);
          fprintf(out, ";\n");
        }
      }

      else {
        /* dumps it as boolean var */
        if (SymbTable_is_symbol_state_var(symb_table, name)
            && last_insert != WFV_STATE) {
          fprintf(out, "VAR\n");
          last_insert = WFV_STATE;
        }
        else if (SymbTable_is_symbol_frozen_var(symb_table, name)
                 && last_insert != WFV_FROZEN) {
          fprintf(out, "FROZENVAR\n");
          last_insert = WFV_FROZEN;
        }
        else if (SymbTable_is_symbol_input_var(symb_table, name)
                 &&
                 last_insert != WFV_INPUT) {
          fprintf(out, "IVAR\n");
          last_insert = WFV_INPUT;
        }

        print_node(wffprint, out, name);
        fprintf(out, " : ");
        SymbType_print(SymbTable_get_var_type(symb_table, name),
                       wffprint, out);
        fprintf(out, ";\n"); /* end of the variable output */
      }
    }
  } /* loop */

  fprintf(out, "\n");
  return 1;
}

/*!
  \brief Writes flattened spec in SMV format on a file.

  Writes a generic spec prefixed by a given
   string in SMV format on a file.
*/
static int
compile_write_udg_flatten_spec_split(const NuSMVEnv_ptr env,
                                     const SymbTable_ptr symb_table,
                                     FILE* out, node_ptr n,
                                     ModelSectionTag mst,
                                     hash_ptr dag_info, hash_ptr defines)
{
  if (n == Nil) return 0;

  switch (node_get_type(n)) {
  case CONS:
  case AND:
    compile_write_udg_flatten_spec_split(env, symb_table, out, car(n), mst,
                                         dag_info, defines);

    compile_write_udg_flatten_spec_split(env, symb_table, out, cdr(n), mst,
                                         dag_info, defines);
    break;

  default:
    compile_write_udg_flatten_spec(env, symb_table, out, n, mst,
                                   dag_info, defines);
  } /* switch */

  return 1;
}

/*!
  \brief Writes flattened spec in SMV format on a file.

  Writes a generic spec prefixed by a given
   string in SMV format on a file.
   Returns true if at least one character was printed, and false otherwise.
*/
static int compile_write_udg_flatten_spec(const NuSMVEnv_ptr env,
                                          const SymbTable_ptr symb_table,
                                          FILE* out, node_ptr n,
                                          ModelSectionTag mst,
                                          hash_ptr dag_info, hash_ptr defines)
{
  const ExprMgr_ptr exprs =
      EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  node_ptr expr, name;

  if (n == Nil || ExprMgr_is_true(exprs, n))
      return 0;

  nusmv_assert((SPEC == node_get_type(n)) ||
               (LTLSPEC == node_get_type(n)) ||
               (INVARSPEC == node_get_type(n)) ||
               (PSLSPEC == node_get_type(n)) ||
               (COMPUTE == node_get_type(n)));

  expr = car(n);
  name = cdr(n);

  //  fprintf(out, "%s ", s);

  /* Support for property Names: Old property structure is in car(n),
     property name is in cdr(n).  */
  if (Nil != name) {
    fprintf(out, "NAME ");
    print_node(wffprint, out, name);
    fprintf(out, " := ");
  }

  /* flatten only if required, i.e. there is explicit context */
  if (CONTEXT == node_get_type(expr)) {
    expr = Compile_FlattenSexp(symb_table, cdr(expr), car(expr));
  }

  print_node(wffprint, out,
             Compile_convert_to_dag_udg(env, symb_table, expr,
                                        dag_info, defines));
  fprintf(out, "\n\n");
  return 1;
}

/*!
  \brief Writes PSL properties as they are.


*/
static int compile_write_udg_flatten_psl(const NuSMVEnv_ptr env,
                                         const SymbTable_ptr symb_table,
                                         FILE* out, node_ptr n,
                                         hash_ptr dag_info, hash_ptr defines)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  if (n == Nil) return 0;

  switch (node_get_type(n)) {
  case CONS:
  case AND:
    compile_write_udg_flatten_psl(env, symb_table, out, car(n),
                                  dag_info, defines);
    compile_write_udg_flatten_psl(env, symb_table, out, cdr(n),
                                  dag_info, defines);
    break;

  default:
    {
      node_ptr expr, name;
      nusmv_assert(PSLSPEC == node_get_type(n));

      expr = car(n);
      name = cdr(n);

      fprintf(out, "-- PSLSPEC\n--   ");

      /* Support for property names */
      if (Nil != name) {
        fprintf(out, "NAME ");
        print_node(wffprint, out, name);
        fprintf(out, " := ");
      }

      print_node(wffprint, out,
                 Compile_convert_to_dag_udg(env, symb_table, expr,
                                            dag_info, defines));
      fprintf(out, "\n\n");
    }
  } /* switch */

  return 1;
}

/*!
  \brief Writes flattened expression in SMV format on a file.

  Writes a generic expression prefixed by a given
   string in SMV format on a file. The given layer is intended to hold the
   determization variables that are created by the booleanization process of
   the properties, that are kept not booleanized within the system.
*/
static int compile_write_udg_flatten_bfexpr(const NuSMVEnv_ptr env,
                                            BddEnc_ptr enc,
                                            const SymbTable_ptr symb_table,
                                            SymbLayer_ptr det_layer,
                                            FILE* out,
                                            node_ptr n,
                                            const char* s,
                                            hash_ptr dag_info,
                                            hash_ptr defines)
{
  const ExprMgr_ptr exprs =
      EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  /* Nil and TRUEEXP indicate the end of a list */
  if (n == Nil || ExprMgr_is_true(exprs, n)) return 0;

  switch (node_get_type(n)) {
  case CONS:
  case AND:
    compile_write_udg_flatten_bfexpr(env,
                                     enc,
                                     symb_table,
                                     det_layer,
                                     out,
                                     car(n),
                                     s,
                                     dag_info,
                                     defines);
    compile_write_udg_flatten_bfexpr(env,
                                     enc,
                                     symb_table,
                                     det_layer,
                                     out,
                                     cdr(n),
                                     s,
                                     dag_info,
                                     defines);
    break;

  default:
    {
      const MasterPrinter_ptr wffprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
      node_ptr expr, name;
      /* Support for property names */
      nusmv_assert(SPEC == node_get_type(n) ||
                   LTLSPEC == node_get_type(n) ||
                   INVARSPEC == node_get_type(n) ||
                   PSLSPEC == node_get_type(n) ||
                   COMPUTE == node_get_type(n));

      expr = car(n);
      name = cdr(n);

      /* specifications are wrapped into CONTEXT during hierarchy creation */
      nusmv_assert(CONTEXT == node_get_type(expr));

      /* booleanized property before printing */
      fprintf(out, "\n%s", s);

      if (Nil != name) {
        fprintf(out, "NAME ");
        print_node(wffprint, out, name);
        fprintf(out, " := ");
      }

      expr = Compile_convert_to_dag_udg(
          env, symb_table,
          Compile_expr2bexpr(enc, det_layer, expr),
          dag_info, defines);
      print_node(wffprint, out, expr);
      fprintf(out, "\n");
    }
  } /* switch */

  return 1;
}

/*!
  \brief Writes flattened expression in SMV format on a file.

  Writes a generic expression prefixed by a given
   string in SMV format on a file.
*/
static int
compile_write_udg_flatten_expr_split(const NuSMVEnv_ptr env,
                                     const SymbTable_ptr symb_table,
                                     FILE* out, node_ptr n,
                                     ModelSectionTag mst,
                                     hash_ptr dag_info, hash_ptr defines)
{
  if (n == Nil) return 0;

  switch (node_get_type(n)) {
  case CONS:
  case AND:
    compile_write_udg_flatten_expr_split(env, symb_table, out, car(n), mst,
                                         dag_info, defines);

    compile_write_udg_flatten_expr_split(env, symb_table, out, cdr(n), mst,
                                         dag_info, defines);
    break;

  default:
    /* INIT DEFINE */
    if(mst  == INIT_T)
      return 1;

    fprintf(out, ",");
    compile_write_udg_flatten_expr(env, symb_table, out, n, mst,
                                   dag_info, defines);
  } /* switch */

  return 1;
}

/*!
  \brief Writes flattened expression in SMV format on a file.

  Writes a generic expression prefixed by a given
   string in SMV format on a file.
   Returns true if at least one character was printed, and false otherwise.
*/
static int compile_write_udg_flatten_expr(const NuSMVEnv_ptr env,
                                          const SymbTable_ptr symb_table,
                                          FILE* out, node_ptr n,
                                          ModelSectionTag mst,
                                          hash_ptr dag_info, hash_ptr defines)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  if (n == Nil || (node_get_type(n) == TRUEEXP /*this chech is optimization*/&&
                   n == find_node(nodemgr, TRUEEXP, Nil, Nil))) return 0;

  /* flatten only if required, i.e. there is explicit context */
  if (CONTEXT == node_get_type(n)) {
    n = Compile_FlattenSexp(symb_table, cdr(n), car(n));
  }
  if(mst == INVAR_T){
    compile_write_udg_print_node(env, out,
                                 Compile_convert_to_dag_udg(env,
                                                            symb_table,
                                                            n,
                                                            dag_info,
                                                            defines),
                                 true,
                                 true,
                                 INVAR_COLOR);
  }else{
    compile_write_udg_print_node(env, out,
                                 Compile_convert_to_dag_udg(env,
                                                            symb_table,
                                                            n,
                                                            dag_info,
                                                            defines),
                                 true,
                                 true,
                                 "");
  }
  return 1;
}

/*!
  \brief Writes flattened expression pairs in SMV format on a
   file.

  Writes a list of flattened expression pairs prefixed by
   a given string in SMV format on a file.
*/
static int compile_write_udg_flatten_expr_pair(const NuSMVEnv_ptr env,
                                               const SymbTable_ptr symb_table,
                                               FILE* out,
                                               node_ptr l,
                                               ModelSectionTag mst,
                                               hash_ptr dag_info,
                                               hash_ptr defines)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  if (l == Nil) return 0;

  while (l) {
    node_ptr n = car(l);
    l = cdr(l);
    nusmv_assert(node_get_type(n) == CONS);

    //    fprintf(out, "%s (", s);

    if (node_get_type(n) == CONTEXT) {
      /* flats the context */
      node_ptr fn = Compile_FlattenSexp(symb_table, car(n), Nil);
      fn = Compile_convert_to_dag_udg(env, symb_table, fn, dag_info, defines);
      print_node(wffprint, out, fn);
    }
    else print_node(wffprint, out,
                    Compile_convert_to_dag_udg(env, symb_table, car(n),
                                               dag_info, defines));

    fprintf(out, ", ");
    if (node_get_type(n) == CONTEXT) {
      /* flats the definition */
      node_ptr fn = Compile_FlattenSexp(symb_table, cdr(n), Nil);
      fn = Compile_convert_to_dag_udg(env, symb_table, fn, dag_info, defines);
      print_node(wffprint, out, fn);
    }
    else print_node(wffprint, out,
                    Compile_convert_to_dag_udg(env, symb_table, cdr(n),
                                               dag_info, defines));

    fprintf(out, ")\n\n");
  }
  return 1;
}

/*!
  \brief Writes the set of non-numeric constants as CONSTANTS
   statement

  Returns 1 if at least one char have been written, 0
   otherwise
*/
static int compile_write_udg_constants(const SymbTable_ptr symb_table,
                                       FILE* out)
{

  /* [CM] temporary skip */
  return 1;
  {
      const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
      const MasterPrinter_ptr wffprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

      boolean written_once = false;
      SymbTableIter iter;

      SYMB_TABLE_FOREACH(symb_table, iter, STT_CONSTANT) {
        node_ptr name = SymbTable_iter_get_symbol(symb_table, &iter);
        if (node_get_type(name) == ATOM || node_get_type(name) == DOT) {
          /* a name to be written */
          if (!written_once) {
            fprintf(out, "CONSTANTS\n ");
            written_once = true;
          }
          else fprintf(out, ", ");

          print_node(wffprint, out, name);
        }
      }

      if (written_once) {
        fprintf(out, " ;\n"); /* to close the statement */
        return 1;
      }

      return 0;
  }
}

/*!
  \brief Prints the flatten version of FSM of an SMV model.

  Prints on the specified file the flatten
   FSM of an SMV model, i.e. a list of all variable, defines, and all
   constrains (INIT, TRANS, INVAR, ASSIGNS, JUSTICE, COMPASSION).
   Specifications are NOT printed.

   layer_names is an array of names of layers that is typically
   obtained from the symbol table. fsm_name is a name of the output
   structure, usually it is "MODULE main".
*/
static void compile_write_udg_flat_fsm(const NuSMVEnv_ptr env,
                                       FILE* out,
                                       const SymbTable_ptr symb_table,
                                       const array_t* layer_names,
                                       const char* fsm_name,
                                       FlatHierarchy_ptr hierarchy,
                                       hash_ptr dag_info, hash_ptr defines)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  int i;
  const char* name;
  boolean printed = false;

  nusmv_assert(layer_names != (array_t*) NULL);

  fprintf(out, "[");

  arrayForEachItem(const char*, layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(symb_table, name);
    //    fprintf(out, "-- Input variables from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL) && SymbLayer_get_input_vars_num(layer) > 0) {
      printed = compile_write_udg_flatten_vars(symb_table, out,
                                               layer, STT_INPUT_VAR);
    }
  }

  if(printed) {
    fprintf(out, ",");
    printed = false;
  }

  arrayForEachItem(const char*, layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(symb_table, name);
    //    fprintf(out, "-- State variables from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL) && SymbLayer_get_state_vars_num(layer) > 0) {
      compile_write_udg_flatten_vars(symb_table, out, layer, STT_STATE_VAR);
    }
  }

  arrayForEachItem(const char*, layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(symb_table, name);
    //    fprintf(out, "-- Frozen variables from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL) &&
        SymbLayer_get_frozen_vars_num(layer) > 0) {
      compile_write_udg_flatten_vars(symb_table, out, layer, STT_FROZEN_VAR);
    }
  }

  /* DEFINEs */
  arrayForEachItem(const char*, layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(symb_table, name);
    //    fprintf(out, "-- Defines from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      compile_write_udg_flat_define(env, symb_table, out,
                                    layer, STT_DEFINE,
                                    dag_info, defines);
    }
  }

  /* ARRAY DEFINEs */
  arrayForEachItem(const char*, layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(symb_table, name);
    //    fprintf(out, "-- Array Defines from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      compile_write_flat_array_define_udg(env, symb_table, out,
                                          layer, STT_ARRAY_DEFINE,
                                          dag_info, defines);
    }
  }


  /* CONSTANTS */
  if (!opt_backward_comp(opts)) {
    if (compile_write_udg_constants(symb_table, out)) fprintf(out, "\n");
  }

  /* ASSIGNs */
  arrayForEachItem(const char*, layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(symb_table, name);
    //    fprintf(out, "-- Assignments from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      compile_write_udg_flat_asgn(env, symb_table, out,
                                  layer, STT_VAR,
                                  hierarchy, dag_info, defines);
    }
  }

  /* CONSTRAINS (except assigns) */
  if (compile_write_udg_flatten_expr_split(env, symb_table, out,
                                           FlatHierarchy_get_init(hierarchy),
                                           INIT_T,
                                           dag_info,
                                           defines)) fprintf(out, "\n");

  if (compile_write_udg_flatten_expr_split(env, symb_table, out,
                                           FlatHierarchy_get_invar(hierarchy),
                                           INVAR_T,
                                           dag_info,
                                           defines)) fprintf(out, "\n");

  if (compile_write_udg_flatten_expr_split(env, symb_table, out,
                                           FlatHierarchy_get_trans(hierarchy),
                                           TRANS_T,
                                           dag_info,
                                           defines)) fprintf(out, "\n");
  fprintf(out, "]\n");

  {
    node_ptr fc = FlatHierarchy_get_justice(hierarchy);
    boolean are_there_compassion =
      (Nil != FlatHierarchy_get_compassion(hierarchy));

    while (Nil != fc) {
      if (compile_write_udg_flatten_expr(
              env, symb_table, out, car(fc),
              /* For backward compatibility */
              are_there_compassion ?
              JUSTICE_T : FAIRNESS_T, dag_info, defines)) {
        fprintf(out, "\n");
      }
      fc = cdr(fc);
    }
  }

  if (compile_write_udg_flatten_expr_pair(
          env, symb_table, out,
          FlatHierarchy_get_compassion(hierarchy),
          COMPASSION_T, dag_info, defines)) {
    fprintf(out, "\n");
  }
  return ;
}

/*!
  \brief Prints the flatten specifications of an SMV model.

  Prints into the specified file the
   specifications of an SMV model.

*/
static void compile_write_udg_flat_specs(const NuSMVEnv_ptr env, FILE* out,
                                         const SymbTable_ptr st,
                                         FlatHierarchy_ptr hierarchy,
                                         hash_ptr dag_info, hash_ptr defines)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  { /* PSL specifications are not supported at the moment (see issue 2626) */
    node_ptr pslspec = FlatHierarchy_get_pslspec(hierarchy);
    if (pslspec != Nil) {
      StreamMgr_print_error(streams,  "\n********   WARNING   ********\n");
      StreamMgr_print_error(
          streams,
          "This version does not support the flattening of PSL properties.\n"
          "However, for user's convenience all the PSL properties will "
          "be dumped\n"
          "as comments in the output file.\n");
      StreamMgr_print_error(streams,  "******** END WARNING ********\n\n");

      fprintf(
          out,
          "--- Dumping of PSL properties is not supported by this version "
          "of the system.\n" \
          "--- However, the PSL properties had been dumped here for user's "
          "convenience,\n" \
          "--- as the occurred in the original model. \n");
      compile_write_udg_flatten_psl(env, st, out, pslspec, dag_info, defines);
    }
  }
}

/*!
  \brief Prints the boolean FSM of an SMV model.

  Prints into the specified file the boolean FSM of an
   SMV model.
   bool_sexp_fsm should be a boolean Sexp FSM.
   layer_names is an array of layers whose variables will be printed,
   usually this parameter is a list of all layers committed to enc. The array
   should be ended by a NULL element.
*/
static void compile_write_udg_bool_fsm(const NuSMVEnv_ptr env,
                                       FILE* out,
                                       const SymbTable_ptr symb_table,
                                       NodeList_ptr layers,
                                       const char* fsm_name,
                                       BoolSexpFsm_ptr bool_sexp_fsm,
                                       hash_ptr dag_info, hash_ptr defines)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  BoolEnc_ptr benc;
  ListIter_ptr iter;

  /* must have been booleanized */
  nusmv_assert(SexpFsm_is_boolean(SEXP_FSM(bool_sexp_fsm)));

  benc = BoolSexpFsm_get_bool_enc(bool_sexp_fsm);

  fprintf(out,
          "-- Flattened Boolean FSM model  generated from %s\n"
          "-- Dumped layers are: ",
          get_input_file(opts));

  /* dumps the layer names: */
  iter = NodeList_get_first_iter(layers);
  while (!ListIter_is_end(iter)) {
    SymbLayer_ptr layer = SYMB_LAYER(NodeList_get_elem_at(layers, iter));
    fprintf(out, "%s", SymbLayer_get_name(layer));
    fprintf(out, " ");
    iter = ListIter_get_next(iter);
  }
  fprintf(out, "\n\n");

  fprintf(out, "%s\n", fsm_name);

  /* NOTE: Defines are not dumped, therefore should not be booleanized */

  /* Input variables */
  iter = NodeList_get_first_iter(layers);
  while (!ListIter_is_end(iter)) {
    SymbLayer_ptr layer = SYMB_LAYER(NodeList_get_elem_at(layers, iter));
    fprintf(out, "-- Input variables from layer '%s'\n",
            SymbLayer_get_name(layer));
    if (SymbLayer_get_input_vars_num(layer) > 0) {
      compile_write_udg_flatten_bool_vars(env, symb_table, benc, out,
                                          layer, STT_INPUT_VAR);
    }
    iter = ListIter_get_next(iter);
  }

  /* State variables */
  iter = NodeList_get_first_iter(layers);
  while (!ListIter_is_end(iter)) {
    SymbLayer_ptr layer = SYMB_LAYER(NodeList_get_elem_at(layers, iter));
    fprintf(out, "-- State variables from layer '%s'\n",
            SymbLayer_get_name(layer));
    if (SymbLayer_get_state_vars_num(layer) > 0) {
      compile_write_udg_flatten_bool_vars(env, symb_table, benc, out,
                                          layer, STT_STATE_VAR);
    }
    iter = ListIter_get_next(iter);
  }

  /* Frozen variables */
  iter = NodeList_get_first_iter(layers);
  while (!ListIter_is_end(iter)) {
    SymbLayer_ptr layer = SYMB_LAYER(NodeList_get_elem_at(layers, iter));
    fprintf(out, "-- Frozen variables from layer '%s'\n",
            SymbLayer_get_name(layer));
    if (SymbLayer_get_frozen_vars_num(layer) > 0) {
      compile_write_udg_flatten_bool_vars(env, symb_table, benc, out,
                                          layer, STT_FROZEN_VAR);
    }
    iter = ListIter_get_next(iter);
  }

  /* CONSTANTS */
  if (!opt_backward_comp(opts)) {
    if (compile_write_udg_constants(symb_table, out)) fprintf(out, "\n");
  }

  /* CONSTRAINS */
  if (compile_write_udg_flatten_expr_split(
          env, symb_table, out,
          SexpFsm_get_init(SEXP_FSM(bool_sexp_fsm)),
          INIT_T, dag_info, defines)) {
    fprintf(out, "\n");
  }

  if (compile_write_udg_flatten_expr_split(
          env, symb_table, out,
          SexpFsm_get_invar(SEXP_FSM(bool_sexp_fsm)),
          INVAR_T, dag_info, defines)) {
    fprintf(out, "\n");
  }

  if (compile_write_udg_flatten_expr_split(
          env, symb_table, out,
          SexpFsm_get_trans(SEXP_FSM(bool_sexp_fsm)),
          TRANS_T, dag_info, defines)) {
    fprintf(out, "\n");
  }


  {
    node_ptr fc = SexpFsm_get_justice(SEXP_FSM(bool_sexp_fsm));
    boolean are_there_compassion =
      (Nil != SexpFsm_get_compassion(SEXP_FSM(bool_sexp_fsm)));

    while(Nil != fc) {
      if (compile_write_udg_flatten_expr(env, symb_table, out,
                                         car(fc),
                                         /* For backward compatibility */
                                         are_there_compassion ?
                                         JUSTICE_T : FAIRNESS_T,
                                         dag_info, defines)) {
        fprintf(out, "\n");
      }
      fc = cdr(fc);
    }
  }

  if (compile_write_udg_flatten_expr_pair(
          env, symb_table, out,
          SexpFsm_get_compassion(SEXP_FSM(bool_sexp_fsm)),
          COMPASSION_T, dag_info, defines)) {
    fprintf(out, "\n\n");
  }

}

/*!
  \brief Private service to print a boolean specification


*/
static void compile_write_udg_bool_spec(const NuSMVEnv_ptr env,
                                        FILE* out, BddEnc_ptr enc,
                                        node_ptr spec, const char* msg,
                                        SymbLayer_ptr det_layer,
                                        hash_ptr dag_info, hash_ptr defines)
{
  if (compile_write_udg_flatten_bfexpr(env,
                                       enc,
                                       BaseEnc_get_symb_table(BASE_ENC(enc)),
                                       det_layer, out, spec,
                                       msg, dag_info, defines))
    fprintf(out, "\n");
}

/*!
  \brief Prints the boolean specifications of an SMV model.

  Prints into the specified file the booleanized
   specifications of an SMV model.

   NOTE: a temporary layer will be created during the dumping for
   determinization variables that derived from the booleanization of
   the specifications. These variable declarations will be printed
   after the specs.

*/
static void compile_write_udg_bool_specs(const NuSMVEnv_ptr env,
                                         FILE* out,
                                         BddEnc_ptr enc,
                                         SymbLayer_ptr det_layer,
                                         FlatHierarchy_ptr hierarchy,
                                         hash_ptr dag_info,
                                         hash_ptr defines)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

  /* here we create a temporary layer, in order to hold all
     determinization variables that will be created by the
     booleanization of the properties. This layer will be destroyed
     after the printing of the determinization variables that it will
     possibly contain */
  SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(enc));

  /* dumps the properties */
  compile_write_udg_bool_spec(env, out, enc,
                              FlatHierarchy_get_spec(hierarchy),
                              "CTLSPEC\n", det_layer,
                              dag_info, defines);
  compile_write_udg_bool_spec(env, out, enc,
                              FlatHierarchy_get_compute(hierarchy),
                              "COMPUTE\n", det_layer,
                              dag_info, defines);
  compile_write_udg_bool_spec(env, out, enc,
                              FlatHierarchy_get_ltlspec(hierarchy),
                              "LTLSPEC\n", det_layer,
                              dag_info, defines);
  compile_write_udg_bool_spec(env, out, enc,
                              FlatHierarchy_get_invarspec(hierarchy),
                              "INVARSPEC\n", det_layer,
                              dag_info, defines);

  { /* PSL specifications are not supported at the moment (see issue 2626) */
    node_ptr pslspec = FlatHierarchy_get_pslspec(hierarchy);
    if (pslspec != Nil) {
      StreamMgr_print_error(streams,  "\n********   WARNING   ********\n");
      StreamMgr_print_error(streams,
              "This version does not support the booleanization of "
                            "PSL properties.\n"
              "However, for user's convenience all the PSL properties "
                            "will be dumped\n"
              "as comments in the output file.\n");
      StreamMgr_print_error(streams,  "******** END WARNING ********\n\n");

      fprintf(out,
              "--- Dumping of PSL properties is not supported by this version "
              "of the system.\n"\
              "--- However, the PSL properties had been dumped here for user's "
              "convenience,\n"\
              "--- as the occurred in the original model. \n");
      compile_write_udg_flatten_psl(env, st, out, pslspec, dag_info, defines);
    }
  }

  /* Dumping of the determinization vars,
     and destruction of the temporary layer */
  if (SymbLayer_get_input_vars_num(det_layer) > 0) {
    fprintf(out, "-- Determinization variables of specifications:\n");
    compile_write_udg_flatten_bool_vars(
        env, st,
        BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(enc)),
        out, det_layer, STT_INPUT_VAR);
  }
}

/*!
  \brief

  Private service of function Compile_convert_to_dag_udg
*/
static node_ptr compile_convert_to_dag_aux_udg(const NuSMVEnv_ptr env,
                                               SymbTable_ptr symb_table,
                                               node_ptr expr, hash_ptr hash,
                                               unsigned int num_thres,
                                               unsigned int dep_thres,
                                               hash_ptr defines,
                                               const char* defines_prefix)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  UStringMgr_ptr strings =
      USTRING_MGR(NuSMVEnv_get_value(env, ENV_STRING_MGR));
  node_ptr info;
  node_ptr define;

  if (expr == Nil) return Nil;

  if (defines != (hash_ptr) NULL) {
    define = find_assoc(defines, expr);
    if (define != Nil) {
      /* found a previously inserted define that substitutes the expression */
      unsigned int count;
      nusmv_assert(node_get_type(define) == COLON);

      /* Increment hit counter */
      INCREMENT_HITS_NUMBER();

      /* this counter keeps track of really used defines, for later dumping.
         setcdr can be used here as the node was created with new_node  */
      count = PTR_TO_INT(cdr(define));

      setcdr(define, PTR_FROM_INT(node_ptr, count+1));
      nusmv_assert(node_get_type(car(define)) == EQDEF);
      return car(car(define)); /* returns the name */
    }
  }

  if (hash != (hash_ptr) NULL) {
    nusmv_assert(defines != (hash_ptr) NULL);

    info = find_assoc(hash, expr);
    if (info != Nil) {
      unsigned int count;
      unsigned int depth;

      /* found a node that might be substituted if needed */
      compile_unpack_dag_info_udg(info, &count, &depth);
      if (count >= num_thres || (count > 1 && depth >= dep_thres)) {
        /* simplifies to a new dag node: continue on children */
        node_ptr name;
        node_ptr left = compile_convert_to_dag_aux_udg(
            env, symb_table, car(expr), hash,
            num_thres, dep_thres,
            defines, defines_prefix);
        node_ptr right = compile_convert_to_dag_aux_udg(
            env, symb_table,
            cdr(expr), hash,
            num_thres, dep_thres,
            defines, defines_prefix);
        node_ptr body = find_node(nodemgr, node_get_type(expr), left, right);

        { /* finds a good name for the define */
          const char* form = "%sexpr%d";
          char* buf = ALLOC(char, strlen(form) + strlen(defines_prefix) + 10);
          sprintf(buf, form, defines_prefix, ++defines_count);
          name = find_node(nodemgr, ATOM,
                           (node_ptr) UStringMgr_find_string(strings, buf),
                           Nil);
          FREE(buf);
        }
        name = SymbTable_get_fresh_symbol_name(symb_table, "__expr");

        define = new_node(nodemgr, COLON, new_node(nodemgr, EQDEF, name, body),
                          PTR_FROM_INT(node_ptr, 1));
        insert_assoc(defines, expr, define);

        return name;
      }
    }
  }

  /* no substitution found or needed */
  switch (node_get_type(expr)) {
    /* leaves */
  case FAILURE:
  case TRUEEXP: case FALSEEXP:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD: case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC: case NUMBER_REAL: case NUMBER_EXP:
  case UWCONST: case SWCONST:
  case ATOM: case BIT: case ARRAY: case SELF: case DOT:
    return expr;

  default:
    {
      node_ptr left = compile_convert_to_dag_aux_udg(
          env, symb_table, car(expr), hash,
          num_thres, dep_thres,
          defines, defines_prefix);
      node_ptr right = compile_convert_to_dag_aux_udg(
          env, symb_table, cdr(expr), hash,
          num_thres, dep_thres,
          defines, defines_prefix);

      return find_node(nodemgr, node_get_type(expr), left, right);
    }
  }
}

/*!
  \brief

  If det_layer is not NULL, then hierarchy is
   to be considered boolean, and specifications will be booleanized,
   If det_layer is null, then also enc can be null
*/
static hash_ptr
compile_create_dag_info_from_hierarchy_udg(SymbTable_ptr st,
                                           FlatHierarchy_ptr hierarchy,
                                           SymbLayer_ptr det_layer,
                                           BddEnc_ptr enc)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  hash_ptr dag_info = new_assoc();
  node_ptr specs[] = { /* FlatHierarchy_get_spec(hierarchy),  */
                       /* FlatHierarchy_get_compute(hierarchy), */
                       /* FlatHierarchy_get_ltlspec(hierarchy), */
    /* [AMa] We can safely compute dag info about invarspecs. Other */
    /* type of properties may lead to invalid defines declaration */
    /* (e.g. containing CTL/LTL operators) */
                       FlatHierarchy_get_invarspec(hierarchy)
                       /* TODO[RC]: psl is not supported by the flattener
                          (issue 2626) */
                       /* FlatHierarchy_get_pslspec(hierarchy), */
  };
  int i;

  nusmv_assert(dag_info != (hash_ptr) NULL);

  /* extracts info from the FSM */
  Compile_make_dag_info(env, FlatHierarchy_get_init(hierarchy), dag_info);
  Compile_make_dag_info(env, FlatHierarchy_get_invar(hierarchy), dag_info);
  Compile_make_dag_info(env, FlatHierarchy_get_trans(hierarchy), dag_info);
  Compile_make_dag_info(env, FlatHierarchy_get_justice(hierarchy), dag_info);
  Compile_make_dag_info(env, FlatHierarchy_get_compassion(hierarchy),
                        dag_info);

  { /* learn from ASSIGNs */
    Set_t vars = FlatHierarchy_get_vars(hierarchy);
    Set_Iterator_t iter;

    SET_FOREACH(vars, iter) {
      node_ptr name = Set_GetMember(vars, iter);
      node_ptr init_name = find_node(nodemgr, SMALLINIT, name, Nil);
      node_ptr next_name = find_node(nodemgr, NEXT, name, Nil);
      Compile_make_dag_info(env, FlatHierarchy_lookup_assign(hierarchy,
                                                        name), dag_info);
      Compile_make_dag_info(env, FlatHierarchy_lookup_assign(hierarchy,
                                                        init_name), dag_info);
      Compile_make_dag_info(env, FlatHierarchy_lookup_assign(hierarchy,
                                                        next_name), dag_info);
    }
  }

  {
    /* Extracts DAG info from the defines */
    SymbTableIter iter;
    SYMB_TABLE_FOREACH(st, iter, STT_DEFINE) {
      node_ptr define = SymbTable_iter_get_symbol(st, &iter);
      node_ptr body = SymbTable_get_define_body(st, define);
      node_ptr ctx = SymbTable_get_define_context(st, define);

      if (Nil != ctx) body = Compile_FlattenSexp(st, body, ctx);

      Compile_make_dag_info(env, body, dag_info);
    }
  }

  /* extracts info from the specifications */
  for (i=0; i < sizeof(specs)/sizeof(specs[0]); ++i) {
    node_ptr spec = (det_layer != SYMB_LAYER(NULL)) ?
      Compile_expr2bexpr(enc, det_layer, specs[i])
      : Compile_FlattenSexp(st, specs[i], Nil);
    Compile_make_dag_info(env, spec, dag_info);
  }

  return dag_info;
}

/*!
  \brief

  Returns a node COLON(NUMBER count, NUMBER depth)
*/
static node_ptr compile_make_dag_info_aux_udg(const NuSMVEnv_ptr env,
                                              node_ptr expr, hash_ptr hash)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr info;

  if (expr == Nil) return compile_pack_dag_info_udg(nodemgr, 1, 0);
  info = find_assoc(hash, expr);
  if (info != Nil) {
    unsigned int count;
    unsigned int depth;
    compile_unpack_dag_info_udg(info, &count, &depth);
    compile_set_dag_info_udg(info, count+1, depth);
    return info;
  }

  switch (node_get_type(expr)) {
    /* leaves */
  case FAILURE:
  case TRUEEXP: case FALSEEXP:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD: case NUMBER_SIGNED_WORD:
  case UWCONST: case SWCONST:
  case NUMBER_FRAC: case NUMBER_REAL: case NUMBER_EXP:
  case ATOM: case BIT: case ARRAY: case SELF: case DOT:
    return compile_pack_dag_info_udg(nodemgr, 1, 0);

    /* cases not no be stored */
  case COLON:
  case NEXT:
    {
      unsigned int count = 0;
      unsigned int depth = 0;
      node_ptr left = compile_make_dag_info_aux_udg(env, car(expr), hash);
      node_ptr right = compile_make_dag_info_aux_udg(env, cdr(expr), hash);
      nusmv_assert(left != Nil || right != Nil); /* cannot be a leaf */
      if (left != Nil) {
        compile_unpack_dag_info_udg(left, &count, &depth);
      }
      if (right != Nil) {
        unsigned int rdepth;
        compile_unpack_dag_info_udg(left, &count, &rdepth);
        depth = MAX(rdepth, depth);
      }

      return compile_pack_dag_info_udg(nodemgr, 1, depth+1);
    }

  default:
    {
      unsigned int count = 0;
      unsigned int depth = 0;
      node_ptr left = compile_make_dag_info_aux_udg(env, car(expr), hash);
      node_ptr right = compile_make_dag_info_aux_udg(env, cdr(expr), hash);
      nusmv_assert(left != Nil || right != Nil); /* cannot be a leaf */
      if (left != Nil) {
        compile_unpack_dag_info_udg(left, &count, &depth);
      }
      if (right != Nil) {
        unsigned int rdepth;
        compile_unpack_dag_info_udg(left, &count, &rdepth);
        depth = MAX(rdepth, depth);
      }

      info = compile_pack_dag_info_udg(nodemgr, 1, depth+1);
      insert_assoc(hash, expr, info);
      return info;
    }
  }
}

/*!
  \brief

  Packs given count and depth into a node
*/
static node_ptr compile_pack_dag_info_udg(NodeMgr_ptr nodemgr,
                                          unsigned int count,
                                          unsigned int depth)
{
  return new_node(nodemgr, COLON,
                  PTR_FROM_INT(node_ptr, count),
                  PTR_FROM_INT(node_ptr, depth));
}

/*!
  \brief

  Unpacks given node to count and deptch
*/
static void compile_unpack_dag_info_udg(node_ptr info,
                                        unsigned int* count,
                                        unsigned int* depth)
{
  nusmv_assert(node_get_type(info) == COLON);
  *count = PTR_TO_INT(car(info));
  *depth = PTR_TO_INT(cdr(info));
}

/*!
  \brief

  Sets count and depth
*/
static void compile_set_dag_info_udg(node_ptr info,
                                     unsigned int count, unsigned int depth)
{
  nusmv_assert(node_get_type(info) == COLON);
  /* setcar and setcdr are admitted here as info was created with new_node,
     and there exist no possibility to gamble the node hash */
  setcar(info, PTR_FROM_INT(node_ptr, count));
  setcdr(info, PTR_FROM_INT(node_ptr, depth));
}

/*!
  \brief Internal service of Compile_destroy_dag_info_udg


*/
static assoc_retval compile_free_node_udg(char *key, char *data, char * arg)
{
  const NodeMgr_ptr nodemgr = NODE_MGR(arg);

  if (data != (char*) NULL) free_node(nodemgr, (node_ptr) data);
  return ASSOC_DELETE;
}

/*!
  \brief Internal service of Compile_destroy_dag_info_udg


*/
static assoc_retval compile_free_define_udg(char *key, char *data, char * arg)
{
 const NodeMgr_ptr nodemgr = NODE_MGR(arg);

  if (data != (char*) NULL) {
    free_node(nodemgr, car((node_ptr) data));
    free_node(nodemgr, (node_ptr) data);
  }
  return ASSOC_DELETE;
}

/*!
  \brief  Print to the given file the array define represerntation


*/
static boolean is_array_define_cell_udg(const SymbTable_ptr st,
                                         const node_ptr name) {
  if (node_get_type(name) == ARRAY) {
    return is_array_define_cell_udg(st, car(name));
  }
  else {
    return SymbTable_is_symbol_array_define(st, name);
  }
}

/*!
  \brief Menthod that prints the given node in udg format


*/
static int compile_write_udg_print_node(const NuSMVEnv_ptr env,
                                        FILE* out,
                                        node_ptr n,
                                        boolean close,
                                        boolean shared,
                                        const char* style)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  if (n == Nil) return 1;
  if (n == (node_ptr) -1) return _PRINT(n, "*no value*", "", "");

#ifdef DEBUG
  compile_write_print_node_type(out, n);
  fprintf(out, " \n\n<<");
  print_node(wffprint, out, n);
  //print_sexp(out, n);
  fprintf(out, ">> \n");
#endif

  switch (node_get_type(n)) {
  case FAILURE: {
    error_unreachable_code();
    return _PRINT(n, "FAILURE", NODE_STYLE_1, CONS_COLOR);
    {
    char buf[20];
    int chars = snprintf(buf, 20, "\", line %d)",
                         ErrorMgr_failure_get_lineno(errmgr, n));
    SNPRINTF_CHECK(chars, 20);

    return _PRINT(n, "FAILURE", "", "") &&
      _PRINT(n, ErrorMgr_failure_get_msg(errmgr, n), "", "") &&
      _PRINT(n, buf, "", "");
    }
  }

  case ATTIME:
    error_unreachable_code();
    return _PRINT(n, "@", "", "") &&
      _THROW(cdr(n)) &&
      _PRINT(n, "{", "", "") &&
      _THROW(car(n)) && _PRINT(n, "}", "", "");

  case TRUEEXP: return _PRINT_NS(n, "T", NODE_STYLE_1, CONS_COLOR);
  case FALSEEXP: return _PRINT_NS(n, "F", NODE_STYLE_1, CONS_COLOR);
  case SELF: return _PRINT(n, "self", "", "");
  case BOOLEAN: return _PRINT(n, "boolean", "", "");

  case ATOM:
    if (!_PRINT_NS(n,

 UStringMgr_get_string_text((string_ptr) car(n)),
                   NODE_STYLE_1,
                   CONS_COLOR)) return 0;
    if (cdr(n)) {
      char buf[20];
      int chars = snprintf(buf, 20, "_%d", NODE_TO_INT(cdr(n)));
      SNPRINTF_CHECK(chars, 20);
      return _PRINT(n, buf, "", VAR_COLOR);
    }
    return 1;

  case NUMBER:
    {
      char buf[20];
      int chars = snprintf(buf, 20, "%d", NODE_TO_INT(car(n)));

      SNPRINTF_CHECK(chars, 20);

      return _PRINT_NS(n, buf, NODE_STYLE_1, CONS_COLOR);
    }

  case NUMBER_UNSIGNED_WORD:
    return _PRINT(n, WordNumber_to_based_string(WORD_NUMBER(car(n)),
                                                get_output_word_format(opts),
                                                false),
                  NODE_STYLE_1,
                  CONS_COLOR);
  case NUMBER_SIGNED_WORD:
    return _PRINT(n, WordNumber_to_based_string(WORD_NUMBER(car(n)),
                                                get_output_word_format(opts),
                                                true),
                  NODE_STYLE_1,
                  CONS_COLOR);

  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
    return _PRINT(n, UStringMgr_get_string_text((string_ptr) car(n)), "", "");

  case UWCONST:
    return _PRINT(n, "uwconst(", "", "") &&
      _THROW(car(n)) && _PRINT(n, ", ", "", "") &&
      _THROW(cdr(n)) && _PRINT(n, ")", "", "");

  case SWCONST:
    return _PRINT(n, "swconst(", "", "") &&
      _THROW(car(n)) && _PRINT(n, ", ", "", "") &&
      _THROW(cdr(n)) && _PRINT(n, ")", "", "");

  case WRESIZE:
    return _PRINT(n, "resize(", "", "") &&
      _THROW(car(n)) && _PRINT(n, ", ", "", "") &&
      _THROW(cdr(n)) && _PRINT(n, ")", "", "");
  case WSIZEOF:
    return _PRINT(n, "sizeof(", "", "") &&
      _THROW(car(n)) && _PRINT(n, ")", "", "");
  case CAST_TOINT:
    return _PRINT(n, "toint(", "", "") &&
      _THROW(car(n)) && _PRINT(n, ")", "", "");

  case DOT:
    _PRINT(n, sprint_node(wffprint, n), "", VAR_COLOR);
    break;
  case BIT:
    {
      char buf[30];
      int chars = snprintf(buf, 30, ".%d", NODE_TO_INT(cdr(n)));
      SNPRINTF_CHECK(chars, 30);

      return _THROW(car(n)) && _PRINT(n, buf, "", "");
    }

  case CONTEXT:
    return _THROW(cdr(n)) &&
      ((car(n) == Nil) ||
       (_PRINT(n, " IN ", "", "") && _THROW(car(n))));

  case CONS:
    return _THROW(car(n)) &&
      ((cdr(n) == Nil) ||
       (_PRINT(n, ", ", "", "") && _THROW(cdr(n))));

  case CASE:
    if(Nil != car(n)){
      if(FAILURE == node_get_type(cdr(n)))
        compile_write_udg_print_node(env, out, cdr(car(n)), true, shared, "");
      else
        compile_write_udg_print_3_aryc_color(env, out,
                                             n,
                                             "case",
                                             car(car(n)),
                                             cdr(car(n)),
                                             cdr(n),
                                             true,
                                             shared,
                                             INPUT_CASE_COLOR,
                                             CONTROL_CASE_COLOR,
                                             STANDARD_COLOR);
    }else{
      compile_write_udg_print_1_ary(env, out, n, "case", true, shared, style);
    }
    break;
    break;

  case COLON:
    compile_write_udg_print_2_ary(env, out, n, ":", true, shared,
                                  style, style);
    break;

  case IFTHENELSE:
    compile_write_udg_print_3_aryc_color(env, out,
                                             n,
                                             "ite",
                                             car(car(n)),
                                             cdr(car(n)),
                                             cdr(n),
                                             true,
                                             shared,
                                             INPUT_CASE_COLOR,
                                             CONTROL_CASE_COLOR,
                                             STANDARD_COLOR);
    break;

  case ARRAY:
    compile_write_udg_print_2_ary(env, out, n, "[]", true, shared,
                                  style, style);
    return 1;
    return _THROW(car(n)) && _PRINT(n, "[", "", "") &&
      _THROW(cdr(n)) && _PRINT(n, "]", "", "");

  case ARRAY_TYPE:
    compile_write_udg_print_node(env, out, car(n), true, true, "");
    return 1;

  case BIT_SELECTION:{
    char *arg = strdup("[");
    arg = strcat(arg, sprint_node(wffprint, car(cdr(n))));
    arg = strcat(arg, ":");
    arg = strcat(arg, sprint_node(wffprint, cdr(cdr(n))));
    arg = strcat(arg, "]");
    compile_write_udg_print_3_aryc(env, out, n, arg, car(n),
                                   NULL, NULL, true, shared);
    return 1;
  }
  case CONSTANTS:
    return _PRINT(n, "CONSTANTS ", "", CONS_COLOR) &&
      _THROW(car(n)) && _PRINT(n, ";", "", "");

    /* this is a expression of word type, i.e. a list of concatenated bits.
       Since the assumed operation is CONCATENATION, its priority is used.
       The expression may be as signed as well as unsigned.
    */
  case UNSIGNED_WORD: {

    assert(false);
    {
    node_ptr iter = car(n);
    nusmv_assert(CONS == node_get_type(iter)); /* a list of bits */

    if (!_PRINT(n, "(", "", "")) return 0;
    if (!_THROW(car(iter))) return 0;

    for (iter = cdr(iter); iter != Nil; iter = cdr(iter)) {
      if (!_PRINT(n, " :: ", "", "")) return 0;
      if (!_THROW(car(iter))) return 0;
    }

    if (!_PRINT(n, ")", "", "")) return 0;
    return 1;
    }
  }

  case WAWRITE:
    return _PRINT(n, "WRITE(", "", "") &&
      _THROW(car(n)) && _PRINT(n, ", ", "", "") &&
      _THROW(car(cdr(n))) && _PRINT(n, ", ", "", "") &&
      _THROW(cdr(cdr(n))) && _PRINT(n, ")", "", "");

  case WAREAD:
    return _PRINT(n, "READ(", "", "") &&
      _THROW(car(n)) && _PRINT(n, ", ", "", "") &&
      _THROW(cdr(n)) && _PRINT(n, ")", "", "");

  case NEXT:
    _PRINT(n, sprint_node(wffprint, n), "", NEXT_COLOR);
    break;
  case SMALLINIT:
    _PRINT(n, sprint_node(wffprint, n), "", INIT_COLOR);
    break;
  case CAST_WORD1:
    compile_write_udg_print_1_ary(env, out, n, "word1", true, shared, style);
    break;

  case CAST_BOOL:
    compile_write_udg_print_1_ary(env, out, n, "bool", true, shared, style);
    break;

  case CAST_SIGNED:
    compile_write_udg_print_1_ary(env, out, n, "signed", true, shared, style);
    break;
  case CAST_UNSIGNED:
    compile_write_udg_print_1_ary(env, out, n, "unsigned", true,
                                  shared, style);
    break;
  case EXTEND:{
    char *arg = strdup("extend_\0");
    arg = strcat(arg, sprint_node(wffprint, cdr(n)));
    compile_write_udg_print_3_aryc(env, out, n, arg, car(n),
                                   NULL, NULL, true, shared);
    break;
  }
  case TWODOTS:
    compile_write_udg_print_2_ary(env, out, n, "..", true, shared,
                                  style, style);
    break;
  case IMPLIES:
    compile_write_udg_print_2_arya(env, out, n, "->", true, true);
    break;
  case IFF:
    compile_write_udg_print_2_ary(env, out, n, "<->", true, true,
                                  style, style);
    break;
  case XOR:
    compile_write_udg_print_2_ary(env, out, n, "xor", true, shared,
                                  style, style);
    break;
  case XNOR:
    compile_write_udg_print_2_ary(env, out, n, "xnor", true, shared,
                                  style, style);
    break;
  case OR:
    compile_write_udg_print_2_ary(env, out, n, "|", true, shared,
                                  style, style);
    break;
  case AND:
    compile_write_udg_print_2_ary(env, out, n, "&", true, shared,
                                  style, style);
    break;
  case EX:
    compile_write_udg_print_1_ary(env, out, n, "EX", true, shared, style);
    break;
  case AX:
    compile_write_udg_print_1_ary(env, out, n, "AX", true, shared, style);
    break;
  case EF:
    compile_write_udg_print_1_ary(env, out, n, "EF", true, shared, style);
    break;
  case AF:
    compile_write_udg_print_1_ary(env, out, n, "AF", true, shared, style);
    break;
  case EG:
    compile_write_udg_print_1_ary(env, out, n, "EG", true, shared, style);
    break;
  case AG:
    compile_write_udg_print_1_ary(env, out, n, "AG", true, shared, style);
    break;
  case OP_NEXT:
    compile_write_udg_print_1_ary(env, out, n, "X", true, shared, style);
    break;
  case OP_PREC:
    compile_write_udg_print_1_ary(env, out, n, "Y", true, shared, style);
    break;
  case OP_NOTPRECNOT:
    compile_write_udg_print_1_ary(env, out, n, "Z", true, shared, style);
    break;
  case OP_GLOBAL:
    compile_write_udg_print_1_ary(env, out, n, "G", true, shared, style);
    break;
  case OP_HISTORICAL:
    compile_write_udg_print_1_ary(env, out, n, "H", true, shared, style);
    break;
  case OP_FUTURE:
    compile_write_udg_print_1_ary(env, out, n, "F", true, shared, style);
    break;
  case OP_ONCE:
    compile_write_udg_print_1_ary(env, out, n, "O", true, shared, style);
    break;
  case UNTIL:
    compile_write_udg_print_2_ary(env, out, n, "U", true, shared,
                                  style, style);
    break;
  case SINCE:
    compile_write_udg_print_2_ary(env, out, n, "S", true, shared,
                                  style, style);
    break;
  case RELEASES:
    compile_write_udg_print_2_ary(env, out, n, "V", true, shared,
                                  style, style);
    break;
  case TRIGGERED:
    compile_write_udg_print_2_ary(env, out, n, "T", true, shared,
                                  style, style);
    break;

  case EU:
    if (!_PRINT(n, "E", "", "")) return 0;
    break;

  case AU:
    if (!_PRINT(n, "A", "", "")) return 0;
    break;

  case EBU:
    if (!_PRINT(n, "E", "", "")) return 0;
    break;

  case ABU:
    if (!_PRINT(n, "A", "", "")) return 0;
    break;

  case EBF:
    compile_write_udg_print_2_ary(env, out, n, "EBF", true, shared,
                                  style, style);
    break;
  case ABF:
    compile_write_udg_print_2_ary(env, out, n, "ABF", true, shared,
                                  style, style);
    break;
  case EBG:
    compile_write_udg_print_2_ary(env, out, n, "EBG", true, shared,
                                  style, style);
    break;
  case ABG:
    compile_write_udg_print_2_ary(env, out, n, "ABG", true, shared,
                                  style, style);
    break;

  case MINU:
    if (!_PRINT(n, "MIN", "", "")) return 0;
    break;

  case MAXU:
    if (!_PRINT(n, "MAX", "", "")) return 0;
    break;

  case EQUAL:
    if((SMALLINIT == node_get_type(car(n))) ||
       (NEXT == node_get_type(car(n)))){
      if(Nil != find_assoc(visited, car(n))){
        compile_write_udg_print_node(env, out, car(n), true, false, style);
      }else{
        compile_write_udg_print_node(env, out, car(n), false, false, style);
        fprintf(out, EDGE_1_UDG EDGE_ATTS_C "%s" CLOSE_EDGE, style);
        compile_write_udg_print_node(env, out, cdr(n), true, true, style);
        fprintf(out, CLOSE_NODE);
      }
    }else
      if((SMALLINIT == node_get_type(cdr(n))) ||
         (NEXT == node_get_type(cdr(n)))){
        if(Nil != find_assoc(visited, cdr(n))){
          compile_write_udg_print_node(env, out, cdr(n), true, false, style);
        }else{
          compile_write_udg_print_node(env, out, cdr(n), false, false, style);
          fprintf(out, EDGE_1_UDG EDGE_ATTS_C "%s" CLOSE_EDGE, style);
          compile_write_udg_print_node(env, out, cdr(n), true, true, style);
          fprintf(out, CLOSE_NODE);
        }
      }else
        compile_write_udg_print_2_ary(env, out, n, "=", true, shared,
                                      style, style);
    break;
  case NOTEQUAL:
    compile_write_udg_print_2_ary(env, out, n, "!=", true, shared,
                                  style, style);
    break;
  case LT:
    compile_write_udg_print_2_ary(env, out, n, "<", true, shared,
                                  style, style);
    break;
  case GT:
    compile_write_udg_print_2_ary(env, out, n, ">", true, shared,
                                  style, style);
    break;
  case LE:
    compile_write_udg_print_2_ary(env, out, n, "<=", true, shared,
                                  style, style);
    break;
  case GE:
    compile_write_udg_print_2_ary(env, out, n, ">=", true, shared,
                                  style, style);
    break;
  case SETIN:
    compile_write_udg_print_2_ary(env, out, n, "in", true, shared,
                                  style, style);
    break;
  case UNION:
    compile_write_udg_print_2_ary(env, out, n, "union", true, shared,
                                  style, style);
    break;
  case LSHIFT:
    compile_write_udg_print_2_ary(env, out, n, "<<", true, shared,
                                  style, style);
    break;
  case RSHIFT:
    compile_write_udg_print_2_ary(env, out, n, ">>", true, shared,
                                  style, style);
    break;
  case LROTATE:
    compile_write_udg_print_2_ary(env, out, n, "<<<", true, shared,
                                  style, style);
    break;
  case RROTATE:
    compile_write_udg_print_2_ary(env, out, n, ">>>", true, shared,
                                  style, style);
    break;
  case MOD:
    compile_write_udg_print_2_ary(env, out, n, "mod", true, shared,
                                  style, style);
    break;

  case PLUS:
    if (cdr(n) == (node_ptr) NULL) { /* checks if unary */
      compile_write_udg_print_1_ary(env, out, n, "+", true, shared, style);
    }
    else {
      compile_write_udg_print_2_ary(env, out, n, "+", true, shared,
                                    style, style);
    }
    break;

  case UMINUS:
    compile_write_udg_print_1_ary(env, out, n, "-", true, shared, style);
    break;

  case FLOOR:
    compile_write_udg_print_1_ary(env, out, n, "floor", true, shared, style);
    break;

  case MINUS:
    if (cdr(n) == (node_ptr) NULL) { /* checks if unary */
      compile_write_udg_print_1_ary(env, out, n, "-", true, shared, style);
    }
    else {
      compile_write_udg_print_2_ary(env, out, n, "-", true, shared,
                                    style, style);
    }
    break;

  case TIMES:
    compile_write_udg_print_2_ary(env, out, n, "*", true, shared,
                                  style, style);
    break;
  case DIVIDE:
    compile_write_udg_print_2_ary(env, out, n, "/", true, shared,
                                  style, style);
    break;
  case CONCATENATION:
    compile_write_udg_print_2_ary(env, out, n, "::", true, shared,
                                  style, style);
    break;
  case NOT:
    compile_write_udg_print_1_ary(env, out, n, "not", true, shared, style);
    break;
  case VAR: break;
  case IVAR: break;
  case EQDEF:
    if(Nil != find_assoc(visited, car(n))){
      compile_write_udg_print_node(env, out, car(n), true, false, style);
    }else{
      compile_write_udg_print_node(env, out, car(n), false, false, style);
      fprintf(out, EDGE_1_UDG EDGE_ATTS);
      compile_write_udg_print_node(env, out, cdr(n), true, true, style);
      fprintf(out, CLOSE_NODE);
    }
    break;

  default:
    ErrorMgr_internal_error(errmgr, "compile_write_udg_print_node: "
                            "not supported type = %d",
                   node_get_type(n));

  }
  return 1;

}

/*!
  \brief Virtual menthod that prints the given node
   (core nodes are handled here)
*/
static inline int insert_assoc_w (hash_ptr hash, node_ptr key, node_ptr value)
{
  insert_assoc(hash, key, value);
  return 1;
}

/*!
  \brief Printer in udg format for a node with children arity equal to 2


*/
static inline int compile_write_udg_print_2_ary(const NuSMVEnv_ptr env,
                                                FILE* buffer,
                                                node_ptr code,
                                                const char* str,
                                                boolean close,
                                                boolean shared,
                                                const char* color1,
                                                const char* color2)
{
  return compile_write_udg_print_3_aryc_color(env,
                                              buffer,
                                              code,
                                              str,
                                              car(code),
                                              cdr(code),
                                              NULL,
                                              close,
                                              shared,
                                              color1,
                                              color2,
                                              color2);
}

/*!
  \brief Printer in udg format for a node with children arity equal to 2


*/
static inline int compile_write_udg_print_2_arya(const NuSMVEnv_ptr env,
                                                 FILE* buffer,
                                                 node_ptr code,
                                                 const char* str,
                                                 boolean close,
                                                 boolean shared)
{
  if(Nil != find_assoc(visited, code)){
    fprintf(buffer, "r(\"%p\")", (void*) code);
  }else{
    fprintf(buffer, NODE_1_UDG NODE_2_UDG
            "%s" NODE_3_UDG ",[" EDGE_1_UDG EDGE_ATTS,
            (void*) code,
            str);
    compile_write_udg_print_node(env, buffer, car(code), close, shared, "");
    fprintf(buffer, ")," EDGE_1_UDG EDGE_ATTS_C CONTROL_CASE_COLOR CLOSE_EDGE);
    compile_write_udg_print_node(env, buffer, cdr(code), close, shared, "");
    fprintf(buffer, CLOSE_NODE);
    insert_assoc(visited, code, (void*)1);
  }
  return 1;
}

/*!
  \brief Printer in udg format for a node with a child


*/
static inline int compile_write_udg_print_1_ary(const NuSMVEnv_ptr env,
                                                FILE* buffer,
                                                node_ptr code,
                                                const char* str,
                                                boolean close,
                                                boolean shared,
                                                const char* color1)
{
  return compile_write_udg_print_3_aryc_color(env,
                                              buffer,
                                              code,
                                              str,
                                              car(code),
                                              NULL,
                                              NULL,
                                              close,
                                              shared,
                                              color1,
                                              "",
                                              color1);
}

/*!
  \brief Printer in udg format for a node with children arity equal to 3

  The children are provided explicitly
*/
static inline int compile_write_udg_print_3_aryc(const NuSMVEnv_ptr env,
                                                 FILE* buffer,
                                                 node_ptr code,
                                                 const char* str,
                                                 node_ptr fst,
                                                 node_ptr snd,
                                                 node_ptr trd,
                                                 boolean close,
                                                 boolean shared)
{
  return compile_write_udg_print_3_aryc_color(env,
                                              buffer,
                                              code,
                                              str,
                                              fst,
                                              snd,
                                              trd,
                                              close,
                                              shared, "", "", "");
}

/*!
  \brief Printer in udg format for a node with children arity equal to 3
   with different colors

  The children are provided explicitly
*/
static inline int compile_write_udg_print_3_aryc_color(const NuSMVEnv_ptr env,
                                                       FILE* buffer,
                                                       node_ptr code,
                                                       const char* str,
                                                       node_ptr fst,
                                                       node_ptr snd,
                                                       node_ptr trd,
                                                       boolean close,
                                                       boolean shared,
                                                       const char* color1,
                                                       const char* color2,
                                                       const char* color3)
{
  boolean isvisited = (Nil != find_assoc(visited, code));

  if(!isvisited){
    if(shared){
      fprintf(buffer, NODE_1_UDG NODE_2_UDG "%s" NODE_3_UDG
              ",[" EDGE_1_UDG EDGE_ATTS_C "%s" CLOSE_EDGE,
              (void*) code,
              str,
              (strcmp(color1,"")?color1:STANDARD_COLOR));
    }else{
      fprintf(buffer, NODE_1E_UDG NODE_2_UDG "%s" NODE_3_UDG
              ",[" EDGE_1_UDG EDGE_ATTS_C "%s" CLOSE_EDGE,
              str,
              (strcmp(color1,"")?color1:STANDARD_COLOR));
    }
    compile_write_udg_print_node(env, buffer, fst, close, shared, color3);
    if(NULL != snd){
      fprintf(buffer, ")," EDGE_1_UDG EDGE_ATTS_C "%s" CLOSE_EDGE,
              (strcmp(color2,"")?color2:STANDARD_COLOR));
      compile_write_udg_print_node(env, buffer, snd, close, shared, color3);
      if(NULL != trd){
        fprintf(buffer, ")," EDGE_1_UDG EDGE_ATTS_C "%s" CLOSE_EDGE,
                (strcmp(color3,"")?color3:STANDARD_COLOR));
        compile_write_udg_print_node(env, buffer, trd, close, shared, color3);
      }
    }
    fprintf(buffer, CLOSE_NODE);
    insert_assoc(visited, code, (void*) 1);
  }else{
    if(shared){
      fprintf(buffer, "r(\"%p\")", (void*) code);
    }else{
      fprintf(buffer, NODE_1E_UDG NODE_2_UDG "%s" NODE_3_UDG
              ",[" EDGE_1_UDG EDGE_ATTS_C "%s" CLOSE_EDGE,
              str,
              (strcmp(color1,"")?color1:STANDARD_COLOR));
      compile_write_udg_print_node(env, buffer, fst, close, shared, color3);
      if(NULL != snd){
        fprintf(buffer, ")," EDGE_1_UDG EDGE_ATTS_C "%s" CLOSE_EDGE,
                (strcmp(color2,"")?color2:STANDARD_COLOR));
        compile_write_udg_print_node(env, buffer, snd, close, shared, color3);
        if(NULL != trd){
          fprintf(buffer, ")," EDGE_1_UDG EDGE_ATTS_C "%s" CLOSE_EDGE,
                  (strcmp(color3,"")?color3:STANDARD_COLOR));
          compile_write_udg_print_node(env, buffer, trd, close, shared,
                                       color3);
        }
      }
      fprintf(buffer, CLOSE_NODE);
    }
  }
  return isvisited;
}

#ifdef DEBUG

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
static void compile_write_print_node_type(FILE* buffer, node_ptr node)
{
  fprintf(buffer, " -");

  switch (node_get_type(node)) {
  case TRANS: fprintf(buffer, "TRANS"); break; /* 101 */
  case INIT: fprintf(buffer, "INIT"); break;
  case INVAR: fprintf(buffer, "INVAR"); break;
  case ASSIGN: fprintf(buffer, "ASSIGN"); break;
  case FAIRNESS: fprintf(buffer, "FAIRNESS"); break;
  case JUSTICE: fprintf(buffer, "JUSTICE"); break;
  case COMPASSION: fprintf(buffer, "COMPASSION"); break;
  case SPEC: fprintf(buffer, "SPEC"); break;
  case LTLSPEC: fprintf(buffer, "LTLSPEC"); break;
  case PSLSPEC: fprintf(buffer, "PSLSPEC"); break; /* 110 */
  case INVARSPEC: fprintf(buffer, "INVARSPEC"); break;
  case COMPUTE: fprintf(buffer, "COMPUTE"); break;
  case DEFINE: fprintf(buffer, "DEFINE"); break;
  case ISA: fprintf(buffer, "ISA"); break;
  case CONSTRAINT: fprintf(buffer, "CONSTRAINT"); break;
  case MODULE: fprintf(buffer, "MODULE"); break;
  case PROCESS: fprintf(buffer, "PROCESS"); break;
  case MODTYPE: fprintf(buffer, "MODTYPE"); break;
  case LAMBDA: fprintf(buffer, "LAMBDA"); break; /* 120 */
  case CONSTANTS: fprintf(buffer, "CONSTANTS"); break;
  case PRED: fprintf(buffer, "PRED"); break;
  case ATTIME: fprintf(buffer, "ATTIME"); break;
  case PREDS_LIST: fprintf(buffer, "PREDS_LIST"); break;
  case MIRROR: fprintf(buffer, "MIRROR"); break;
  case NUSMV_STATEMENTS_SYMBOL_LAST:
    fprintf(buffer, "NUSMV_STATEMENTS_SYMBOL_LAST"); break;
  case NUSMV_EXPR_SYMBOL_FIRST:
    fprintf(buffer, "NUSMV_EXPR_SYMBOL_FIRST"); break;
  case FAILURE: fprintf(buffer, "FAILURE"); break;
  case CONTEXT: fprintf(buffer, "CONTEXT"); break;
  case EU: fprintf(buffer, "EU"); break;
  case AU: fprintf(buffer, "AU"); break;
  case EBU: fprintf(buffer, "EBU"); break;
  case ABU: fprintf(buffer, "ABU"); break;
  case MINU: fprintf(buffer, "MINU"); break;
  case MAXU: fprintf(buffer, "MAXU"); break;
  case VAR: fprintf(buffer, "VAR"); break;
  case FROZENVAR: fprintf(buffer, "FROZENVAR"); break;
  case IVAR: fprintf(buffer, "IVAR"); break;
  case BOOLEAN: fprintf(buffer, "BOOLEAN"); break;
  case ARRAY: fprintf(buffer, "ARRAY"); break; /* 140 */
  case OF: fprintf(buffer, "OF"); break;
  case SCALAR: fprintf(buffer, "SCALAR"); break;
  case CONS: fprintf(buffer, "CONS"); break;
  case BDD: fprintf(buffer, "BDD"); break;
  case SEMI: fprintf(buffer, "SEMI"); break;
  case LP: fprintf(buffer, "LP"); break;
  case RP: fprintf(buffer, "RP"); break;
  case LB: fprintf(buffer, "LB"); break;
  case RB: fprintf(buffer, "RB"); break;
  case LCB: fprintf(buffer, "LCB"); break;
  case RCB: fprintf(buffer, "RCB"); break;
  case EQDEF: fprintf(buffer, "EQDEF"); break;
  case TWODOTS: fprintf(buffer, "TWODOTS"); break;
  case FALSEEXP: fprintf(buffer, "FALSEEXP"); break;
  case TRUEEXP: fprintf(buffer, "TRUEEXP"); break;
  case SELF: fprintf(buffer, "SELF"); break;
  case CASE: fprintf(buffer, "CASE"); break;
  case ESAC: fprintf(buffer, "ESAC"); break;
  case COLON: fprintf(buffer, "COLON"); break;
  case IFTHENELSE: fprintf(buffer, "IFTHENELSE"); break; /* 160 */
  case INCONTEXT: fprintf(buffer, "INCONTEXT"); break;
  case SIMPWFF: fprintf(buffer, "SIMPWFF"); break;
  case LTLWFF: fprintf(buffer, "LTLWFF"); break;
  case CTLWFF: fprintf(buffer, "CTLWFF"); break;
  case COMPWFF: fprintf(buffer, "COMPWFF"); break;
  case ATOM: fprintf(buffer, "ATOM"); break;
  case NUMBER: fprintf(buffer, "NUMBER"); break;
  case COMMA: fprintf(buffer, "COMMA"); break;
  case IMPLIES: fprintf(buffer, "IMPLIES"); break;
  case IFF: fprintf(buffer, "IFF"); break;
  case OR: fprintf(buffer, "OR"); break;
  case XOR: fprintf(buffer, "XOR"); break;
  case XNOR: fprintf(buffer, "XNOR"); break;
  case AND: fprintf(buffer, "AND"); break;
  case NOT: fprintf(buffer, "NOT"); break;
  case EX: fprintf(buffer, "EX"); break;
  case AX: fprintf(buffer, "AX"); break;
  case EF: fprintf(buffer, "EF"); break;
  case AF: fprintf(buffer, "AF"); break;
  case EG: fprintf(buffer, "EG"); break; /* 180 */
  case AG: fprintf(buffer, "AG"); break;
  case EE: fprintf(buffer, "EE"); break;
  case SINCE: fprintf(buffer, "SINCE"); break;
  case UNTIL: fprintf(buffer, "UNTIL"); break;
  case TRIGGERED: fprintf(buffer, "TRIGGERED"); break;
  case RELEASES: fprintf(buffer, "RELEASES"); break;
  case EBF: fprintf(buffer, "EBF"); break;
  case EBG: fprintf(buffer, "EBG"); break;
  case ABF: fprintf(buffer, "ABF"); break;
  case ABG: fprintf(buffer, "ABG"); break;
  case BUNTIL: fprintf(buffer, "BUNTIL"); break;
  case MMIN: fprintf(buffer, "MMIN"); break;
  case MMAX: fprintf(buffer, "MMAX"); break;
  case OP_NEXT: fprintf(buffer, "OP_NEXT"); break;
  case OP_GLOBAL: fprintf(buffer, "OP_GLOBAL"); break;
  case OP_FUTURE: fprintf(buffer, "OP_FUTURE"); break;
  case OP_PREC: fprintf(buffer, "OP_PREC"); break;
  case OP_NOTPRECNOT: fprintf(buffer, "OP_NOTPRECNOT"); break;
  case OP_HISTORICAL: fprintf(buffer, "OP_HISTORICAL"); break; /* 200 */
  case OP_ONCE: fprintf(buffer, "OP_ONCE"); break;
  case EQUAL: fprintf(buffer, "EQUAL"); break;
  case NOTEQUAL: fprintf(buffer, "NOTEQUAL"); break;
  case LT: fprintf(buffer, "LT"); break;
  case GT: fprintf(buffer, "GT"); break;
  case LE: fprintf(buffer, "LE"); break;
  case GE: fprintf(buffer, "GE"); break;
  case UNION: fprintf(buffer, "UNION"); break;
  case SETIN: fprintf(buffer, "SETIN"); break;
  case MOD: fprintf(buffer, "MOD"); break;
  case PLUS: fprintf(buffer, "PLUS"); break;
  case MINUS: fprintf(buffer, "MINUS"); break;
  case TIMES: fprintf(buffer, "TIMES"); break;
  case DIVIDE: fprintf(buffer, "DIVIDE"); break;
  case UMINUS: fprintf(buffer, "UMINUS"); break;
  case NEXT: fprintf(buffer, "NEXT"); break;
  case SMALLINIT: fprintf(buffer, "SMALLINIT"); break;
  case DOT: fprintf(buffer, "DOT"); break;
  case BIT: fprintf(buffer, "BIT"); break;
  case RANGE: fprintf(buffer, "RANGE"); break; /* 220 */
  case UNSIGNED_WORD: fprintf(buffer, "UNSIGNED_WORD"); break;
  case SIGNED_WORD: fprintf(buffer, "SIGNED_WORD"); break;
  case INTEGER: fprintf(buffer, "INTEGER"); break;
  case REAL: fprintf(buffer, "REAL"); break;
  case NUMBER_UNSIGNED_WORD: fprintf(buffer, "NUMBER_UNSIGNED_WORD"); break;
  case NUMBER_SIGNED_WORD: fprintf(buffer, "NUMBER_SIGNED_WORD"); break;
  case NUMBER_FRAC: fprintf(buffer, "NUMBER_FRAC"); break;
  case NUMBER_REAL: fprintf(buffer, "NUMBER_REAL"); break;
  case NUMBER_EXP: fprintf(buffer, "NUMBER_EXP"); break;
  case LSHIFT: fprintf(buffer, "LSHIFT"); break; /* 230 */
  case RSHIFT: fprintf(buffer, "RSHIFT"); break;
  case LROTATE: fprintf(buffer, "LROTATE"); break;
  case RROTATE: fprintf(buffer, "RROTATE"); break;
  case BIT_SELECTION: fprintf(buffer, "BIT_SELECTION"); break;
  case CONCATENATION: fprintf(buffer, "CONCATENATION"); break;
  case CAST_BOOL: fprintf(buffer, "CAST_BOOL"); break;
  case CAST_WORD1: fprintf(buffer, "CAST_WORD1"); break;
  case CAST_SIGNED: fprintf(buffer, "CAST_SIGNED"); break;
  case CAST_UNSIGNED: fprintf(buffer, "CAST_UNSIGNED"); break;
  case EXTEND: fprintf(buffer, "EXTEND"); break;
  case WORDARRAY: fprintf(buffer, "WORDARRAY"); break;
  case WAREAD: fprintf(buffer, "WAREAD"); break;
  case WAWRITE: fprintf(buffer, "WAWRITE"); break;
  case UWCONST: fprintf(buffer, "UWCONST"); break;
  case SWCONST: fprintf(buffer, "SWCONST"); break;
  case WRESIZE: fprintf(buffer, "WRESIZE"); break;
  case WSIZEOF: fprintf(buffer, "WSIZEOF"); break;
  case CAST_TOINT: fprintf(buffer, "CAST_TOINT"); break;
  case COMPID: fprintf(buffer, "COMPID"); break;
  case ARRAY_TYPE: fprintf(buffer, "ARRAY_TYPE"); break;
  case ARRAY_DEF: fprintf(buffer, "ARRAY_DEF"); break;
  case NUSMV_EXPR_SYMBOL_LAST:
    fprintf(buffer, "NUSMV_EXPR_SYMBOL_LAST"); break;
  case NUSMV_CORE_SYMBOL_LAST:
    fprintf(buffer, "NUSMV_CORE_SYMBOL_LAST"); break;
  }
  fprintf(buffer, "- ");
}
#endif
