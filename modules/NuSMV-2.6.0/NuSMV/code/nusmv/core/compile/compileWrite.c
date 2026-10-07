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
  \author Marco Roveri, Roberto Cavada
  \brief Creation of an SMV file containing the flattened or booleanized
   model.

  Creation of an SMV file containing the flattened or
   booleanized model, processes will be removed by explicitly
   introducing a process variable and modifying assignments to take
   care of inertia.

*/


#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/normalizers/MasterNormalizer.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/compile/compileInt.h"

#include "nusmv/core/compile/symb_table/SymbLayer.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/psl/pslNode.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/utils/assoc.h"
#include "nusmv/core/utils/ustring.h"

#include "nusmv/core/node/anonymizers/NodeAnonymizerBase.h"
#include "nusmv/core/node/anonymizers/NodeAnonymizerST.h"

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/* Return value in case an error occurs */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define TYPE_ERROR ((node_ptr) -1)


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/
/*!
  \brief CompileWriteData structure, used internal for passing parameters
         from top level functions to low level functions.
         Used just in this file.


*/



typedef struct CompileWriteData_TAG{
  SymbTable_ptr symb_table;
  FILE* out;
  boolean force_flattening;
  const char* fsm_name;
  SymbLayer_ptr det_layer;
  const array_t* layer_names;
  FlatHierarchy_ptr hierarchy;
  BddEnc_ptr enc;
  NodeAnonymizerBase_ptr anonymizer;
  BoolSexpFsm_ptr bool_sexp_fsm;
  NodeList_ptr nodelist_layer;
  hash_ptr dag_info;
  hash_ptr defines;
  hash_ptr cdh;
} CompileWriteData;


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
/* Note about reentrancy: This var is ok but not thread safe */
static unsigned int dag_hits = 0;

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

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

/* Write statistics if requested */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define PRINT_DAG_STATS() if (opt_get_daggifier_statistics(opts)) {  \
  int i;                                                                \
  for(i=0; i<80; i++) StreamMgr_print_error(streams,  "*");                       \
  StreamMgr_print_error(streams,  "\n DAG Statistics:\n");                        \
  StreamMgr_print_error(streams,                                                  \
          "\tNumber of introduced defines: %d\n",                       \
          st_count(defines));                                           \
  StreamMgr_print_error(streams,                                                  \
          "\tNumber of hits: %d\n",                                     \
          GET_DAG_HITS_NUMBER());                                       \
  for(i=0; i<80; i++) StreamMgr_print_error(streams,  "*");                       \
  StreamMgr_print_error(streams,  "\n");                                          \
  }

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static boolean is_array_define_element(const SymbTable_ptr st,
                                       const node_ptr name);

/* TODO[AT] refactoring is need. */
/* many functions can be merged with a parameter to distinguish :
   normal output, obfuscated output and restricted output.
*/

/* -- functions related to defines ------- */
static int
compile_write_flat_define(const NuSMVEnv_ptr env,
                          const CompileWriteData* cwd,
                          const NodeList_ptr names);
static int
compile_write_obfuscated_flat_define(const NuSMVEnv_ptr env,
                                     const CompileWriteData* cwd,
                                     SymbLayer_ptr layer);
static int
compile_write_flat_define_aux(const NuSMVEnv_ptr env,
                              const CompileWriteData* cwd,
                              node_ptr name,
                              hash_ptr printed_arrays);
static int
compile_write_obfuscated_flat_define_aux(const NuSMVEnv_ptr env,
                                         const CompileWriteData* cwd,
                                         node_ptr name,
                                         hash_ptr printed_arrays);

/* -- functions related to vars ------- */
static int
compile_write_flatten_vars(const NuSMVEnv_ptr env,
                           const CompileWriteData* cwd,
                           const SymbLayer_ptr layer,
                           SymbLayerIter* iter);

static int
compile_write_obfuscated_flatten_vars(const NuSMVEnv_ptr env,
                                      const CompileWriteData* cwd,
                                      const SymbLayer_ptr layer,
                                      const SymbTableType type);

static int
compile_write_flatten_vars_aux(const NuSMVEnv_ptr env,
                               const CompileWriteData* cwd,
                               const node_ptr name,
                               hash_ptr printed);
static boolean
compile_write_obfuscated_flatten_vars_aux(const NuSMVEnv_ptr env,
                                          const CompileWriteData* cwd,
                                          const node_ptr name,
                                          hash_ptr printed);

/* -- functions related to functions ------- */
static int
compile_write_flatten_funs(const NuSMVEnv_ptr env,
                           const CompileWriteData* cwd,
                           const SymbLayer_ptr layer,
                           SymbLayerIter* iter);

static int
compile_write_obfuscated_flatten_funs(const NuSMVEnv_ptr env,
                                      const CompileWriteData* cwd,
                                      const SymbLayer_ptr layer,
                                      const SymbTableType type);

static int
compile_write_flatten_funs_aux(const NuSMVEnv_ptr env,
                               const CompileWriteData* cwd,
                               const node_ptr name,
                               hash_ptr printed);

static boolean
compile_write_obfuscated_flatten_funs_aux(const NuSMVEnv_ptr env,
                                          const CompileWriteData* cwd,
                                          const node_ptr name,
                                          hash_ptr printed);

/* -- functions related to FSM ------- */



static void
compile_print_assign(const NuSMVEnv_ptr env,
                     const CompileWriteData* cwd,
                     node_ptr lhs,
                     node_ptr rhs);

static int
compile_write_flat_asgn(const NuSMVEnv_ptr env,
                        const CompileWriteData* cwd,
                        const NodeList_ptr vars);

static int
compile_write_obfuscated_flat_asgn(const NuSMVEnv_ptr env,
                                   const CompileWriteData* cwd,
                                   const SymbLayer_ptr layer);

static int
compile_write_flatten_bool_vars(const NuSMVEnv_ptr env,
                                           const CompileWriteData* cwd,
                                           const BoolEnc_ptr bool_enc,
                                           const SymbLayer_ptr layer,
                                           const SymbTableType type);

static int
compile_write_flatten_expr(const NuSMVEnv_ptr env,
                           const CompileWriteData* cwd,
                           node_ptr n, const char* s);

static int
compile_write_obfuscated_flatten_expr(const NuSMVEnv_ptr env,
                                      const CompileWriteData* cwd,
                                      node_ptr n,
                                      const char* s);

static int
compile_write_flatten_expr_split(const NuSMVEnv_ptr env,
                                 const CompileWriteData* cwd,
                                 node_ptr n, const char* s);

static int
compile_write_obfuscated_flatten_expr_split(const NuSMVEnv_ptr env,
                                            const CompileWriteData* cwd,
                                            node_ptr n,
                                            const char* s);

static int
compile_write_flatten_spec(const NuSMVEnv_ptr env,
                           const CompileWriteData* cwd,
                           node_ptr n, const char* s);

static int
compile_write_obfuscated_flatten_spec(const NuSMVEnv_ptr env,
                                      const CompileWriteData* cwd,
                                      node_ptr n,
                                      const char* s);

static int
compile_write_flatten_spec_split(const NuSMVEnv_ptr env,
                                 const CompileWriteData* cwd,
                                 node_ptr n, const char* s);

static int
compile_write_obfuscated_flatten_spec_split(const NuSMVEnv_ptr env,
                                            const CompileWriteData* cwd,
                                            node_ptr n,
                                            const char* s);

static int
compile_write_flatten_expr_pair(const NuSMVEnv_ptr env,
                                const CompileWriteData* cwd,
                                node_ptr l, const char* s);

static int
compile_write_obfuscated_flatten_expr_pair(const NuSMVEnv_ptr env,
                                           const CompileWriteData* cwd,
                                           node_ptr l,
                                           const char* s);

static int
compile_write_flatten_bfexpr(const NuSMVEnv_ptr env,
                             const CompileWriteData* cwd,
                             node_ptr n, const char* s);

static int
compile_write_flatten_psl(const NuSMVEnv_ptr env,
                          const CompileWriteData* cwd,
                          node_ptr n);

static void
compile_write_flat_fsm(const NuSMVEnv_ptr env,
                       const CompileWriteData* cwd);

static boolean
compile_write_is_var_in_set(const SymbLayer_ptr layer,
                            const node_ptr sym, void* arg);

static void
compile_write_restricted_flat_fsm(const NuSMVEnv_ptr env,
                                  const CompileWriteData* cwd);

static void
compile_write_obfuscated_flat_fsm(const NuSMVEnv_ptr env,
                                  const CompileWriteData* cwd);

static void
compile_write_bool_fsm(const NuSMVEnv_ptr env,
                       const CompileWriteData* cwd);

static void
compile_write_flat_spec(const NuSMVEnv_ptr env,
                        const CompileWriteData* cwd,
                        node_ptr spec, const char* msg);

static void
compile_write_flat_specs(const NuSMVEnv_ptr env,
                         const CompileWriteData* cwd);

static void
compile_write_obfuscated_flat_specs(const NuSMVEnv_ptr env,
                                    const CompileWriteData* cwd);

static void
compile_write_bool_spec(const NuSMVEnv_ptr env,
                        const CompileWriteData* cwd,
                        node_ptr spec, const char* msg);

static void
compile_write_bool_specs(const NuSMVEnv_ptr env,
                         const CompileWriteData* cwd);

static int
compile_write_constants(const SymbTable_ptr symb_table, FILE* out);

static int
compile_write_obfuscated_constants(const CompileWriteData* cwd);

static hash_ptr
compile_create_dag_info_from_hierarchy(const CompileWriteData* cwd);

static node_ptr compile_pack_dag_info(NodeMgr_ptr nodemgr,
                                      unsigned int count,
                                      unsigned int depth,
                                      boolean admissible);

static void compile_unpack_dag_info(node_ptr info,
                                    unsigned int* count,
                                    unsigned int* depth,
                                    boolean* admissible);

static void compile_set_dag_info(node_ptr info,
                                 unsigned int count,
                                 unsigned int depth,
                                 boolean admissible);

static node_ptr compile_convert_to_dag_aux(const NuSMVEnv_ptr env,
                                           const CompileWriteData* cwd,
                                           node_ptr expr,
                                           unsigned int num_thres,
                                           unsigned int dep_thres,
                                           const char* defines_prefix);

static node_ptr compile_make_dag_info_aux(NodeMgr_ptr nodemgr,
                                          node_ptr expr, hash_ptr hash);

static assoc_retval compile_free_node(char *key, char *data, char * arg);

static assoc_retval compile_free_define(char *key, char *data, char * arg);

static void
compile_symbtype_obfuscated_print(SymbType_ptr type,
                                  const CompileWriteData* cwd);

static void
compile_write_obfuscated_dag_defines(const NuSMVEnv_ptr env,
                                     const CompileWriteData* cwd);

static node_ptr
compile_get_rid_of_define_chain(const CompileWriteData* cwd,
                                                node_ptr expr);

static node_ptr
__create_define_name(SymbTable_ptr st,
                     const char * prefix, node_ptr body);

static void
compile_write_data_init(CompileWriteData * cwd);

static void
compile_write_data_deinit(CompileWriteData * cwd);

static void compile_visit_dag_defines(
    hash_ptr defines,
    boolean (*cb)(node_ptr define_eqdef,
                  unsigned int count,
                  void* arg),
    void* arg);

/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

void Compile_WriteFlattenModel(const NuSMVEnv_ptr env,
                               FILE* out,
                               const SymbTable_ptr st,
                               const array_t* layer_names,
                               const char* fsm_name,
                               FlatHierarchy_ptr hierarchy,
                               boolean force_flattening)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  CompileWriteData cwdata;

  /* these are used for making the tree a dag */
  hash_ptr dag_info = (hash_ptr) NULL;
  hash_ptr defines = (hash_ptr) NULL;
  /* to remove chain of defines */
  hash_ptr cdh = (hash_ptr) NULL;

  cdh = new_assoc();
  nusmv_assert((hash_ptr)NULL != cdh);

  RESET_DAG_HITS_NUMBER();

  compile_write_data_init(&cwdata);
  cwdata.out = out;
  cwdata.symb_table =  st;
  cwdata.layer_names = layer_names;
  cwdata.fsm_name = fsm_name;
  cwdata.hierarchy = hierarchy;
  cwdata.force_flattening = force_flattening;
  cwdata.cdh = cdh;

  if (opt_is_daggifier_enabled(opts)) {
    dag_info = compile_create_dag_info_from_hierarchy(&cwdata);
    defines = new_assoc();
    nusmv_assert(defines != (hash_ptr) NULL);
  }

  cwdata.dag_info = dag_info;
  cwdata.defines = defines;

  /* dumps the FSM */
  compile_write_flat_fsm(env, &cwdata);

  /* dumps the specifications */
  compile_write_flat_specs(env, &cwdata);

  if (opt_is_daggifier_enabled(opts)) {
    Compile_write_dag_defines(env, cwdata.out, cwdata.defines);
    PRINT_DAG_STATS();
    Compile_destroy_dag_info(env, cwdata.dag_info, cwdata.defines);
  }

  if ((hash_ptr) NULL != cwdata.defines) free_assoc(cwdata.defines);
  if ((hash_ptr) NULL != cwdata.dag_info) free_assoc(cwdata.dag_info);
  if ((hash_ptr) NULL != cwdata.cdh) free_assoc(cwdata.cdh);

}

void Compile_WriteRestrictedFlattenModel(const NuSMVEnv_ptr env,
                                         FILE* out,
                                         const SymbTable_ptr st,
                                         const array_t* layer_names,
                                         const char* fsm_name,
                                         FlatHierarchy_ptr hierarchy,
                                         boolean force_flattening)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  CompileWriteData cwdata;

  /* these are used for making the tree a dag */
  hash_ptr dag_info = (hash_ptr) NULL;
  hash_ptr defines = (hash_ptr) NULL;
  /* to remove chain of defines */
  hash_ptr cdh = (hash_ptr) NULL;

  cdh = new_assoc();
  nusmv_assert((hash_ptr)NULL != cdh);

  RESET_DAG_HITS_NUMBER();

  compile_write_data_init(&cwdata);
  cwdata.out = out;
  cwdata.symb_table =  st;
  cwdata.layer_names = layer_names;
  cwdata.fsm_name = fsm_name;
  cwdata.hierarchy = hierarchy;
  cwdata.force_flattening = force_flattening;
  cwdata.cdh = cdh;

  if (opt_is_daggifier_enabled(opts)) {
    dag_info = compile_create_dag_info_from_hierarchy(&cwdata);
    defines = new_assoc();
    nusmv_assert((hash_ptr) NULL != defines);
  }

  cwdata.dag_info = dag_info;
  cwdata.defines = defines;


  /* dumps the FSM */
  compile_write_restricted_flat_fsm(env, &cwdata);

  /* dumps the specifications */
  compile_write_flat_specs(env, &cwdata);

  if (opt_is_daggifier_enabled(opts)) {
    Compile_write_dag_defines(env, cwdata.out, cwdata.defines);
    PRINT_DAG_STATS();
    Compile_destroy_dag_info(env, cwdata.dag_info, cwdata.defines);
  }

  if ((hash_ptr) NULL != cwdata.defines) free_assoc(cwdata.defines);
  if ((hash_ptr) NULL != cwdata.dag_info) free_assoc(cwdata.dag_info);
  if ((hash_ptr) NULL != cwdata.cdh) free_assoc(cwdata.cdh);
}

void Compile_WriteObfuscatedFlattenModel(const NuSMVEnv_ptr env,
                                         FILE* out,
                                         const SymbTable_ptr st,
                                         const array_t* layer_names,
                                         const char* fsm_name,
                                         FlatHierarchy_ptr hierarchy,
                                         boolean print_map,
                                         boolean force_flattening,
                                         NodeAnonymizerBase_ptr anonymizer)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  CompileWriteData cwdata;

  /* these are used for making the tree a dag */
  hash_ptr dag_info = (hash_ptr) NULL;
  hash_ptr defines = (hash_ptr) NULL;
  /* to remove chain of defines */
  hash_ptr cdh = (hash_ptr) NULL;

  cdh = new_assoc();
  nusmv_assert((hash_ptr)NULL != cdh);

  RESET_DAG_HITS_NUMBER();

  compile_write_data_init(&cwdata);
  cwdata.out = out;
  cwdata.symb_table =  st;
  cwdata.layer_names = layer_names;
  cwdata.fsm_name = fsm_name;
  cwdata.hierarchy = hierarchy;
  cwdata.force_flattening = force_flattening;
  cwdata.anonymizer = anonymizer;
  cwdata.cdh = cdh;

  if (opt_is_daggifier_enabled(opts)) {
    dag_info = compile_create_dag_info_from_hierarchy(&cwdata);
    defines = new_assoc();
    nusmv_assert(defines != (hash_ptr) NULL);
  }

  cwdata.dag_info = dag_info;
  cwdata.defines = defines;


  /* dumps the FSM */
  compile_write_obfuscated_flat_fsm(env, &cwdata);

  /* dumps the specifications */
  compile_write_obfuscated_flat_specs(env, &cwdata);

  if (opt_is_daggifier_enabled(opts)) {
    compile_write_obfuscated_dag_defines(env, &cwdata);
  }

  /* If requested, output the obfuscation map */
  if (print_map && ! NodeAnonymizerBase_is_map_empty(anonymizer)) {
    /* TODO[MD] Here we need a different stream for the map, forcing stdout just
       for debugging  */
    NodeAnonymizerBase_print_map(anonymizer,
                                 StreamMgr_get_output_stream(streams));
  }

  if (opt_is_daggifier_enabled(opts)) {
    PRINT_DAG_STATS();
    Compile_destroy_dag_info(env, cwdata.dag_info, cwdata.defines);
  }

  if ((hash_ptr) NULL != cwdata.defines) free_assoc(cwdata.defines);
  if ((hash_ptr) NULL != cwdata.dag_info) free_assoc(cwdata.dag_info);
  if ((hash_ptr) NULL != cwdata.cdh) free_assoc(cwdata.cdh);
}

void Compile_WriteFlattenFsm(const NuSMVEnv_ptr env,
                             FILE* out,
                             const SymbTable_ptr st,
                             const array_t* layer_names,
                             const char* fsm_name,
                             FlatHierarchy_ptr hierarchy,
                             boolean force_flattening)
{
  hash_ptr cdh;
  CompileWriteData cwdata;

  cdh = new_assoc();
  nusmv_assert((hash_ptr)NULL != cdh);

  compile_write_data_init(&cwdata);
  cwdata.out = out;
  cwdata.symb_table =  st;
  cwdata.layer_names = layer_names;
  cwdata.fsm_name = fsm_name;
  cwdata.hierarchy = hierarchy;
  cwdata.force_flattening = force_flattening;
  cwdata.cdh = cdh;

  compile_write_flat_fsm(env, &cwdata);

  if ((hash_ptr)NULL != cwdata.cdh) free_assoc(cwdata.cdh);
}

void Compile_WriteFlattenSpecs(const NuSMVEnv_ptr env,
                               FILE* out,
                               const SymbTable_ptr st,
                               FlatHierarchy_ptr hierarchy,
                               boolean force_flattening)
{

  hash_ptr cdh = new_assoc();
  CompileWriteData cwdata;

  compile_write_data_init(&cwdata);
  cwdata.out = out;
  cwdata.symb_table =  st;
  cwdata.hierarchy = hierarchy;
  cwdata.force_flattening = force_flattening;
  cwdata.cdh = cdh;

  compile_write_flat_specs(env, &cwdata);

  if ((hash_ptr)NULL != cwdata.cdh)
      free_assoc(cwdata.cdh);

}

void Compile_WriteBoolModel(const NuSMVEnv_ptr env,
                            FILE* out,
                            BddEnc_ptr enc,
                            NodeList_ptr layers,
                            const char* fsm_name,
                            BoolSexpFsm_ptr bool_sexp_fsm,
                            boolean force_flattening)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  FlatHierarchy_ptr fh;
  SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(enc));
  SymbLayer_ptr det_layer;
  CompileWriteData cwdata;

  /* these are used for making the tree a dag */
  hash_ptr dag_info = (hash_ptr) NULL;
  hash_ptr defines = (hash_ptr) NULL;
  /* to remove chain of defines */
  hash_ptr cdh = (hash_ptr) NULL;

  cdh = new_assoc();
  nusmv_assert((hash_ptr)NULL != cdh);

  det_layer = SymbTable_create_layer(st, (char*) NULL, /*temp name*/
                                     SYMB_LAYER_POS_DEFAULT);

  NodeList_append(layers, (node_ptr) det_layer);

  fh = SexpFsm_get_hierarchy(SEXP_FSM(bool_sexp_fsm));

  compile_write_data_init(&cwdata);
  cwdata.out = out;
  cwdata.symb_table = st;
  cwdata.fsm_name = fsm_name;
  cwdata.hierarchy = fh;
  cwdata.force_flattening = force_flattening;
  cwdata.det_layer = det_layer;
  cwdata.nodelist_layer = layers;
  cwdata.enc = enc;
  cwdata.bool_sexp_fsm = bool_sexp_fsm;
  cwdata.cdh = cdh;

  if (opt_is_daggifier_enabled(opts)) {
    dag_info = compile_create_dag_info_from_hierarchy(&cwdata);
    defines = new_assoc();
    nusmv_assert(defines != (hash_ptr) NULL);
  }
  cwdata.dag_info = dag_info;
  cwdata.defines = defines;


  /* dumps the FSM */
  compile_write_bool_fsm(env, &cwdata);

  compile_write_bool_specs(env, &cwdata);

  if (opt_is_daggifier_enabled(opts)) {
    Compile_write_dag_defines(env, cwdata.out, cwdata.defines);
    PRINT_DAG_STATS();
    Compile_destroy_dag_info(env, cwdata.dag_info, cwdata.defines);
  }

  if ((hash_ptr) NULL != cwdata.defines) free_assoc(cwdata.defines);
  if ((hash_ptr) NULL != cwdata.dag_info) free_assoc(cwdata.dag_info);
  if ((hash_ptr) NULL != cwdata.cdh) free_assoc(cwdata.cdh);

  SymbTable_remove_layer(st, det_layer);
}

void Compile_WriteBoolFsm(const NuSMVEnv_ptr env,
                          FILE* out, const SymbTable_ptr st,
                          NodeList_ptr layers, const char* fsm_name,
                          BoolSexpFsm_ptr bool_sexp_fsm,
                          boolean force_flattening)
{
  hash_ptr cdh = new_assoc();
  CompileWriteData cwdata;

  compile_write_data_init(&cwdata);
  cwdata.out = out;
  cwdata.symb_table =  st;
  cwdata.fsm_name = fsm_name;
  cwdata.force_flattening = force_flattening;
  cwdata.bool_sexp_fsm = bool_sexp_fsm;
  cwdata.nodelist_layer = layers;
  cwdata.cdh = cdh;

  compile_write_bool_fsm(env, &cwdata);

  if ((hash_ptr) NULL != cwdata.cdh)
      free_assoc(cwdata.cdh);
}

void Compile_WriteBoolSpecs(const NuSMVEnv_ptr env,
                            FILE* out,
                            BddEnc_ptr enc,
                            FlatHierarchy_ptr hierarchy)
{
  hash_ptr cdh = new_assoc();
  SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(enc));
  SymbLayer_ptr det_layer;
  CompileWriteData cwdata;

  det_layer = SymbTable_create_layer(st, (char*) NULL, /*temp name*/
                                     SYMB_LAYER_POS_DEFAULT);

  compile_write_data_init(&cwdata);
  cwdata.out = out;
  cwdata.hierarchy = hierarchy;
  cwdata.det_layer = det_layer;
  cwdata.enc = enc;
  cwdata.cdh = cdh;

  compile_write_bool_specs(env, &cwdata);

  if ((hash_ptr) NULL != cwdata.cdh) free_assoc(cwdata.cdh);
  SymbTable_remove_layer(st, det_layer);
}

node_ptr Compile_make_dag_info(const NuSMVEnv_ptr env,
                               node_ptr expr, hash_ptr hash)
{
  const MasterNormalizer_ptr normalizer =
    MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  return compile_make_dag_info_aux(nodemgr,
                                   MasterNormalizer_normalize_node(normalizer, expr),
                                   hash);
}

void Compile_destroy_dag_info(const NuSMVEnv_ptr env,
                              hash_ptr dag_info, hash_ptr defines)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  clear_assoc_and_free_entries_arg(dag_info, compile_free_node, (char*)nodemgr);
  clear_assoc_and_free_entries_arg(defines, compile_free_define, (char*)nodemgr);
}


struct WriteDagDefinesCbArg {
  const OptsHandler_ptr opts;
  Logger_ptr logger;
  const MasterPrinter_ptr wffprinter;
  FILE* out;
  boolean header_printed;
};

static boolean
compile_write_dag_defines_cb(node_ptr define_eqdef,
                             unsigned int count,
                             void* _arg)
{

  if (count > 0) {
    struct WriteDagDefinesCbArg* arg = (struct WriteDagDefinesCbArg*) _arg;

    if (!arg->header_printed) {
      fprintf(arg->out, "-- Symbols introduced by the dumper:\n");
      arg->header_printed = true;
    }
    if (opt_verbose_level_gt( arg->opts, 0)) {
          Logger_log(arg->logger, "-- occurrences: %d\n", count+1);
    }

    fprintf(arg->out, "DEFINE ");
    print_node(arg->wffprinter, arg->out, define_eqdef);
    fprintf(arg->out, " ;\n\n");
  }

  return true;  /* keep traveral */
}

void Compile_write_dag_defines(const NuSMVEnv_ptr env, FILE* out,
                               hash_ptr defines)
{
  struct WriteDagDefinesCbArg cb_arg = {
    .opts = OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER)),
    .logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER)),
    .wffprinter = MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER)),
    .out = out,
    .header_printed = false,
  };

  compile_visit_dag_defines(defines, &compile_write_dag_defines_cb,
                            &cb_arg);
}


static boolean
compile_declare_dag_defines_in_layer_cb(node_ptr define_eqdef,
                                        unsigned int count,
                                        void* _arg)
{
  if (count > 0) {
    SymbLayer_ptr layer = (SymbLayer_ptr) _arg;
    SymbLayer_declare_define(layer, car(define_eqdef),
                             Nil /*context is assumed*/,
                             cdr(define_eqdef));
  }

  return true;  /* keep traveral */
}

void Compile_declare_dag_defines_in_layer(SymbLayer_ptr layer,
                                          hash_ptr defines)

{
  compile_visit_dag_defines(defines,
                            &compile_declare_dag_defines_in_layer_cb, layer);
}



node_ptr Compile_convert_to_dag(const NuSMVEnv_ptr env,
                                SymbTable_ptr symb_table,
                                node_ptr expr,
                                hash_ptr dag_hash,
                                hash_ptr defines)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterNormalizer_ptr normalizer =
    MASTER_NORMALIZER(NuSMVEnv_get_value(env, ENV_NODE_NORMALIZER));

  CompileWriteData cwdata;

  compile_write_data_init(&cwdata);
  cwdata.symb_table = symb_table;
  cwdata.dag_info = dag_hash;
  cwdata.defines = defines;

  return compile_convert_to_dag_aux(env, &cwdata,
                                    MasterNormalizer_normalize_node(normalizer, expr),
                                    opt_get_daggifier_counter_threshold(opts),
                                    opt_get_daggifier_depth_threshold(opts),
                                    opt_traces_hiding_prefix(opts));
}

void Compile_print_array_define(const NuSMVEnv_ptr env,
                                FILE* out,
                                const node_ptr n)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  node_ptr iter;
  switch (node_get_type(n)) {
  case ARRAY_DEF:
    nusmv_assert(Nil == cdr(n));

    fprintf(out, "[ ");

    for (iter = car(n); iter != Nil; iter = cdr(iter)) {
      nusmv_assert(CONS == node_get_type(iter));
      Compile_print_array_define(env, out, car(iter));
      if (cdr(iter) != Nil) fprintf(out, ", ");
    }
    fprintf(out, " ]");
    break;

  default:
    print_node(wffprint, out, n);
  }
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief Returns true iff this name is sub-element of
   an array define.

  If name refers to an array element the index has to be
   a NUMBER. The name has to be a define or array define identifier.
*/
static boolean
is_array_define_element(const SymbTable_ptr symb_table, const node_ptr name)
{
  /* See description of flattener_core_flatten for docs about arrays. */

  /* array may have only NUMBER subscript */
  nusmv_assert(ARRAY != node_get_type(name) ||
               NUMBER == node_get_type(cdr(name)));

  /* Note that for
     DEFINE d := [1,2,3];
            d[4] := OK;
     d[4] is not part of d, but d[1] is.
  */
  if (ARRAY == node_get_type(name) &&
      SymbTable_is_symbol_array_define(symb_table, car(name))) {
    int val = node_get_int(cdr(name));
    node_ptr body = SymbTable_get_array_define_body(symb_table, car(name));
    nusmv_assert(ARRAY_DEF == node_get_type(body));
    if (val >= 0 && val < llength(car(body)))
        return true;
  }
  return false;
}

/*!
  \brief Writes DEFINE declarations in SMV format on a file.


*/
static int compile_write_flat_define(const NuSMVEnv_ptr env,
                                     const CompileWriteData* cwd,
                                     const NodeList_ptr names)
{
  ListIter_ptr iter;
  hash_ptr printed_arrays;

  if (NodeList_get_length(names) == 0) return 1;

  fprintf(cwd->out, "DEFINE\n");
  printed_arrays = new_assoc();

  iter = NodeList_get_first_iter(names);
  while (! ListIter_is_end(iter)) {
    node_ptr name = NodeList_get_elem_at(names, iter);

    compile_write_flat_define_aux(env,
                                  cwd,
                                  name,
                                  printed_arrays);
    iter = ListIter_get_next(iter);
  }

  free_assoc(printed_arrays);
  fprintf(cwd->out, "\n");
  return 1;
}

/*!
  \brief Writes DEFINE declarations in SMV format on a file.

  This function behaves exactly like compile_write_flat_define
   except that identifiers a re obfuscated before.
*/
static int compile_write_obfuscated_flat_define(const NuSMVEnv_ptr env,
                                                const CompileWriteData* cwd,
                                                SymbLayer_ptr layer)
{
  SymbLayerIter iter;
  hash_ptr printed_arrays;

  if (SymbLayer_get_defines_num(layer) == 0) return 1;

  fprintf(cwd->out, "DEFINE\n");
  printed_arrays = new_assoc();

  SYMB_LAYER_FOREACH(layer, iter, STT_DEFINE) {
    node_ptr name = SymbLayer_iter_get_symbol(layer, &iter);

    compile_write_obfuscated_flat_define_aux(env, cwd,
                                             name,
                                             printed_arrays);
  }

  free_assoc(printed_arrays);
  fprintf(cwd->out, "\n");
  return 1;
}

/*!
  \brief Writes a DEFINE declarations in SMV format on a file.

  If a define happens to be an array define's element
   then array is output (and remembered in printed_arrays)
   instead of the original identifiers.
*/
static int
compile_write_flat_define_aux(const NuSMVEnv_ptr env,
                              const CompileWriteData* cwd,
                              node_ptr name,
                              hash_ptr printed_arrays)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  /* MODIFY THIS FUNCTION ONLY TOGETHER WITH
     compile_write_obfuscated_flat_define_aux */

  if (is_array_define_element(cwd->symb_table, name)) {
    /* output the outer array but not this identifier */
    compile_write_flat_define_aux(env, cwd,
                                  car(name),
                                  printed_arrays);
  }
  else {
    node_ptr fdef;
    /* this is a proper define or array define */
    if (SymbTable_is_symbol_define(cwd->symb_table, name)) {
      node_ptr ctx;
      fdef = SymbTable_get_define_body(cwd->symb_table, name);
      ctx = SymbTable_get_define_context(cwd->symb_table, name);
      if (cwd->force_flattening || Nil != ctx) {
        fdef = Compile_FlattenSexp(cwd->symb_table, fdef, ctx);
      }
    }
    else {
      node_ptr ctx;
      nusmv_assert(SymbTable_is_symbol_array_define(cwd->symb_table, name));
      /* print array define only if not yet printed */
      if (Nil != find_assoc(printed_arrays, name))
          return 1;
      insert_assoc(printed_arrays, name, NODE_PTR(1));

      fdef = SymbTable_get_array_define_body(cwd->symb_table, name);
      ctx = SymbTable_get_array_define_context(cwd->symb_table, name);
      if (cwd->force_flattening || Nil != ctx) {
        fdef = Compile_FlattenSexp(cwd->symb_table, fdef, ctx);
      }
    }
    nusmv_assert(fdef != Nil);

    print_node(wffprint, cwd->out, name);
    fprintf(cwd->out, " := ");

    /* get rid of chains of cwd->defines. useful only for normal cwd->defines */
    fdef = CompileFlatten_resolve_define_chains(cwd->symb_table, fdef, Nil);

    print_node(wffprint, cwd->out,
               Compile_convert_to_dag(env,
                                      cwd->symb_table,
                                      fdef,
                                      cwd->dag_info,
                                      cwd->defines));
    fprintf(cwd->out, ";\n");
  }
  return 1;
}

/*!
  \brief Writes a DEFINE declarations in SMV format on a file.

  This function behaves example like
   compile_write_flat_define_aux
   except that identifiers are obfuscated before being printed.


  \sa compile_write_flat_define_aux
*/
static int
compile_write_obfuscated_flat_define_aux(const NuSMVEnv_ptr env,
                                         const CompileWriteData* cwd,
                                         node_ptr name,
                                         hash_ptr printed_arrays)
{
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  /* MODIFY THIS FUNCTION ONLY TOGETHER WITH
     compile_write_flat_define_aux */

  if (is_array_define_element(cwd->symb_table, name)) {
    /* output the outer array but not this identifier */
    compile_write_obfuscated_flat_define_aux(env,
                                             cwd,
                                             car(name),
                                             printed_arrays);
  }
  else {
    node_ptr fdef;
    node_ptr name_anonymous = NULL;
    node_ptr fdef_anonymous = NULL;

    /* this is a proper define or array define */
    if (SymbTable_is_symbol_define(cwd->symb_table, name)) {
      node_ptr ctx;
      fdef = SymbTable_get_define_body(cwd->symb_table, name);
      ctx = SymbTable_get_define_context(cwd->symb_table, name);
      if (cwd->force_flattening || Nil != ctx) {
        fdef = Compile_FlattenSexp(cwd->symb_table, fdef, ctx);
      }
    }
    else {
      node_ptr ctx;
      nusmv_assert(SymbTable_is_symbol_array_define(cwd->symb_table, name));
      /* print array define only if not yet printed */
      if (Nil != find_assoc(printed_arrays, name))
          return 1;
      insert_assoc(printed_arrays, name, NODE_PTR(1));

      fdef = SymbTable_get_array_define_body(cwd->symb_table, name);
      ctx = SymbTable_get_array_define_context(cwd->symb_table, name);
      if (cwd->force_flattening || Nil != ctx) {
        fdef = Compile_FlattenSexp(cwd->symb_table, fdef, ctx);
      }
    }
    nusmv_assert(fdef != Nil);

    name_anonymous = NodeAnonymizerBase_map_expr(cwd->anonymizer, name);
    print_node(wffprint, cwd->out, name_anonymous);
    fprintf(cwd->out, " := ");

    /* get rid of chains of cwd->defines. useful only for normal cwd->defines */
    fdef = CompileFlatten_resolve_define_chains(cwd->symb_table, fdef, Nil);
    fdef = Compile_convert_to_dag(env, cwd->symb_table, fdef,
                                  cwd->dag_info, cwd->defines);
    fdef_anonymous = NodeAnonymizerBase_map_expr(cwd->anonymizer, fdef);

    print_node(wffprint, cwd->out, fdef_anonymous);
    fprintf(cwd->out, ";\n");
  }
  return 1;
}

/*!
  \brief Writes VAR, FROZENVAR, and IVAR declarations in
   SMV format on a file.


*/
static int compile_write_flatten_vars(const NuSMVEnv_ptr env,
                                      const CompileWriteData* cwd,
                                      const SymbLayer_ptr layer,
                                      SymbLayerIter* iter)
{
  hash_ptr printed_arrays;
  unsigned int count = 0;

  enum { WFV_UNKNOWN, WFV_INPUT, WFV_STATE, WFV_FROZEN } last_insert = WFV_UNKNOWN;

  printed_arrays = new_assoc();

  while (!SymbLayer_iter_is_end(layer, iter)) {
    node_ptr name = SymbLayer_iter_get_symbol(layer, iter);

    count ++;

    if (SymbTable_is_symbol_var(cwd->symb_table, name)) {

      if (SymbTable_is_symbol_state_var(cwd->symb_table, name)
          && last_insert != WFV_STATE) {
        fprintf(cwd->out, "VAR\n");
        last_insert = WFV_STATE;
      }
      else if (SymbTable_is_symbol_frozen_var(cwd->symb_table, name)
               && last_insert != WFV_FROZEN) {
        fprintf(cwd->out, "FROZENVAR\n");
        last_insert = WFV_FROZEN;
      }
      else if (SymbTable_is_symbol_input_var(cwd->symb_table, name)
               &&
               last_insert != WFV_INPUT) {
        fprintf(cwd->out, "IVAR\n");
        last_insert = WFV_INPUT;
      }
      else {
        nusmv_assert(last_insert != WFV_UNKNOWN);
      }
      /* print the var declaration */
      compile_write_flatten_vars_aux(env, cwd, name,
                                     printed_arrays);
    }

    SymbLayer_iter_next(layer, iter);
  } /* loop */

  /* Destroy the printed arrays assoc */
  free_assoc(printed_arrays);

  if (0 == count) return 0;

  fprintf(cwd->out, "\n");
  return 1;
}

/*!
  \brief Writes FUN declarations in SMV format on a file.


*/
static int compile_write_flatten_funs(const NuSMVEnv_ptr env,
                                      const CompileWriteData* cwd,
                                      const SymbLayer_ptr layer,
                                      SymbLayerIter* iter)
{
  hash_ptr printed_arrays;
  unsigned int count = 0;

  printed_arrays = new_assoc();

  while (!SymbLayer_iter_is_end(layer, iter)) {
    node_ptr name = SymbLayer_iter_get_symbol(layer, iter);

    count ++;

    if (SymbTable_is_symbol_function(cwd->symb_table, name)) {

      fprintf(cwd->out, "FUN\n");
      /* print the fun declaration */
      compile_write_flatten_funs_aux(env, cwd, name,
                                     printed_arrays);
    }

    SymbLayer_iter_next(layer, iter);
  } /* loop */

  /* Destroy the printed arrays assoc */
  free_assoc(printed_arrays);

  if (0 == count) return 0;

  fprintf(cwd->out, "\n");
  return 1;
}

/*!
  \brief Writes VAR, FROZENVAR, and IVAR declarations in
   SMV format on a file.


*/
static int compile_write_obfuscated_flatten_vars(const NuSMVEnv_ptr env,
                                                 const CompileWriteData* cwd,
                                                 const SymbLayer_ptr layer,
                                                 const SymbTableType type)
{
  SymbLayerIter iter;
  hash_ptr printed_arrays;

  enum { WFV_UNKNOWN, WFV_INPUT, WFV_STATE, WFV_FROZEN } last_insert = WFV_UNKNOWN;

  printed_arrays = new_assoc();

  SYMB_LAYER_FOREACH(layer, iter, type) {
    node_ptr name = SymbLayer_iter_get_symbol(layer, &iter);

    if (SymbTable_is_symbol_var(cwd->symb_table, name)) {

      if (SymbTable_is_symbol_state_var(cwd->symb_table, name)
          && last_insert != WFV_STATE) {
        fprintf(cwd->out, "VAR\n");
        last_insert = WFV_STATE;
      }
      else if (SymbTable_is_symbol_frozen_var(cwd->symb_table, name)
               && last_insert != WFV_FROZEN) {
        fprintf(cwd->out, "FROZENVAR\n");
        last_insert = WFV_FROZEN;
      }
      else if (SymbTable_is_symbol_input_var(cwd->symb_table, name)
               &&
               last_insert != WFV_INPUT) {
        fprintf(cwd->out, "IVAR\n");
        last_insert = WFV_INPUT;
      }
      else {
        nusmv_assert(last_insert != WFV_UNKNOWN);
      }

      /* print the var declaration */
      compile_write_obfuscated_flatten_vars_aux(env, cwd, name,
                                                printed_arrays);
    }

  } /* loop */

  /* Destroy the printed arrays assoc */
  free_assoc(printed_arrays);

  fprintf(cwd->out, "\n");
  return 1;
}

/*!
  \brief Writes FUN declarations in SMV format on a file.


*/
static int compile_write_obfuscated_flatten_funs(const NuSMVEnv_ptr env,
                                                 const CompileWriteData* cwd,
                                                 const SymbLayer_ptr layer,
                                                 const SymbTableType type)
{


  SymbLayerIter iter;
  hash_ptr printed_arrays;
  unsigned int count = 0;

  printed_arrays = new_assoc();

  SYMB_LAYER_FOREACH(layer, iter, type) {
    node_ptr name = SymbLayer_iter_get_symbol(layer, &iter);

    count ++;

    if (SymbTable_is_symbol_function(cwd->symb_table, name)) {

      fprintf(cwd->out, "FUN\n");
      /* print the fun declaration */
      compile_write_obfuscated_flatten_funs_aux(env, cwd,
                                                name,
                                                printed_arrays);
    }

  } /* loop */

  /* Destroy the printed arrays assoc */
  free_assoc(printed_arrays);

  if (0 == count) return 0;

  fprintf(cwd->out, "\n");
  return 1;
}

/*!
  \brief Print the variable declaration.

  If the identifier contains an index subscript in its
   name then at first the identifier check for being a part of an array.
   In this case array is output (and remembered in "printed") instead of
   the var. Otherwise, the identifier is output.
*/
static int compile_write_flatten_vars_aux(const NuSMVEnv_ptr env,
                                          const CompileWriteData* cwd,
                                          const node_ptr name,
                                          hash_ptr printed)
{
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  /* MODIFY THIS FUNCTION ONLY TOGETHER WITH
     compile_write_obfuscated_flatten_vars_aux */

  if (SymbTable_is_symbol_array_var_element(cwd->symb_table, name)) {
      /* this identifier is a subpart of array => print the outer array only */
    compile_write_flatten_vars_aux(env, cwd, car(name), printed);
  }
  else {
    /* this identifier is an array or an individual identifier
         declared with index subscript in its name */
    if (SymbTable_is_symbol_variable_array(cwd->symb_table, name)) {
      if (find_assoc(printed, name) == Nil) {
        /* print array only if not printed yet */
        SymbType_ptr type = SymbTable_get_variable_array_type(cwd->symb_table, name);
        nusmv_assert(type != (SymbType_ptr) NULL);

        print_node(wffprint, cwd->out, name);
        fprintf(cwd->out, " : ");
        SymbType_print(type, wffprint, cwd->out);
        fprintf(cwd->out, ";\n");
        insert_assoc(printed, name, (node_ptr) type);
      }
    }
    else { /* this is a normal variable */
      print_node(wffprint, cwd->out, name);
      fprintf(cwd->out, " : ");
      SymbType_print(SymbTable_get_var_type(cwd->symb_table, name), wffprint, cwd->out);
      fprintf(cwd->out, ";\n"); /* end of the variable output */
    }
  }
  return 1;
}

/*!
  \brief Print the function declaration.

  Print the function declaration.
*/
static int compile_write_flatten_funs_aux(const NuSMVEnv_ptr env,
                                          const CompileWriteData* cwd,
                                          const node_ptr name,
                                          hash_ptr printed)
{
    const MasterPrinter_ptr wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    if (SymbTable_is_symbol_function(cwd->symb_table, name)) {
      if (find_assoc(printed, name) == Nil) {
        NodeList_ptr args;
        SymbType_ptr return_type;
        NFunction_ptr function = SymbTable_get_function(cwd->symb_table, name);
        ListIter_ptr iter;

        nusmv_assert(function != (NFunction_ptr) NULL);

        args = NFunction_get_args(function);
        return_type = NFunction_get_return_type(function);
        print_node(wffprint, cwd->out, name);
        fprintf(cwd->out, " : ");

        for (iter=NodeList_get_first_iter(args); !ListIter_is_end(iter);) {
          node_ptr arg = NodeList_get_elem_at(args, iter);
          SymbType_print(SYMB_TYPE(arg), wffprint, cwd->out);
          iter=ListIter_get_next(iter);
          if (! ListIter_is_end(iter)){
            fprintf(cwd->out, " * ");
          }
        }

        fprintf(cwd->out, " -> ");
        SymbType_print(return_type, wffprint, cwd->out);
        fprintf(cwd->out, ";\n");
        insert_assoc(printed, name, (node_ptr) function);
      }
    }

  return 1;
}

/*!
  \brief Print the variable declaration after obfuscation

  The function works exactly like
   compile_write_flatten_vars_aux but all identifiers
   are obfuscated before being printed.

  \sa compile_write_flatten_vars_aux
*/
static boolean
compile_write_obfuscated_flatten_vars_aux(const NuSMVEnv_ptr env,
                                          const CompileWriteData* cwd,
                                          const node_ptr name,
                                          hash_ptr printed)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  /* MODIFY THIS FUNCTION ONLY TOGETHER WITH
     compile_write_flatten_vars_aux */

  if (SymbTable_is_symbol_array_var_element(cwd->symb_table, name)) {
    /* this identifier is a subpart of array => print the outer array only */
    return compile_write_obfuscated_flatten_vars_aux(env,
                                                     cwd,
                                                     car(name),
                                                     printed);
  }
  else {
    /* this identifier is an array or an individual identifier
       declared with index subscript in its name */
    if (SymbTable_is_symbol_variable_array(cwd->symb_table, name)) {
      if (find_assoc(printed, name) == Nil) {
        /* print array only if not printed yet */
        SymbType_ptr type = SymbTable_get_variable_array_type(cwd->symb_table, name);
        node_ptr name_anonymous = NodeAnonymizerBase_map_expr(cwd->anonymizer, name);

        nusmv_assert(type != (SymbType_ptr) NULL);

        print_node(wffprint, cwd->out, name_anonymous);
        fprintf(cwd->out, " : ");
        compile_symbtype_obfuscated_print(type, cwd);
        fprintf(cwd->out, ";\n");
        insert_assoc(printed, name, (node_ptr) type);
      }
    }
    else { /* this is a normal variable */
      node_ptr name_anonymous = NodeAnonymizerBase_map_expr(cwd->anonymizer, name);
      print_node(wffprint, cwd->out, name_anonymous);
      fprintf(cwd->out, " : ");
      compile_symbtype_obfuscated_print(SymbTable_get_var_type(cwd->symb_table,
                                                               name),
                                        cwd);
      fprintf(cwd->out, ";\n"); /* end of the variable output */
    }
  }

  return true;
}

/*!
  \brief Print the function declaration after obfuscation

  The function works exactly like
   compile_write_flatten_funs_aux but all identifiers
   are obfuscated before being printed.

  \sa compile_write_flatten_funs_aux
*/
static boolean
compile_write_obfuscated_flatten_funs_aux(const NuSMVEnv_ptr env,
                                          const CompileWriteData* cwd,
                                          const node_ptr name,
                                          hash_ptr printed)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

    if (SymbTable_is_symbol_function(cwd->symb_table, name)) {
      if (find_assoc(printed, name) == Nil) {
        node_ptr name_anonymous = NodeAnonymizerBase_map_expr(cwd->anonymizer, name);
        NFunction_ptr function = SymbTable_get_function(cwd->symb_table, name);
        NodeList_ptr args;
        SymbType_ptr return_type;
        ListIter_ptr iter;

        nusmv_assert(function != (NFunction_ptr) NULL);

        args = NFunction_get_args(function);
        return_type = NFunction_get_return_type(function);

        print_node(wffprint, cwd->out, name_anonymous);
        fprintf(cwd->out, " : ");

        for (iter=NodeList_get_first_iter(args); !ListIter_is_end(iter); ) {
          node_ptr arg = NodeList_get_elem_at(args, iter);
          compile_symbtype_obfuscated_print(SYMB_TYPE(arg), cwd);
          iter=ListIter_get_next(iter);
          if (! ListIter_is_end(iter)){
            fprintf(cwd->out, " * ");
          }
        }


        fprintf(cwd->out, " -> ");

        compile_symbtype_obfuscated_print(return_type, cwd);


        fprintf(cwd->out, ";\n");
        insert_assoc(printed, name, (node_ptr) function);


      }
    }

  return true;
}

/*!
  \brief Writes flattened ASSIGN declarations in SMV format on a
   file.

  Writes flattened ASSIGN declarations in SMV format on a
   file.
*/
static int compile_write_flat_asgn(const NuSMVEnv_ptr env,
                                   const CompileWriteData* cwd,
                                   const NodeList_ptr vars)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  ListIter_ptr iter;

  NODE_LIST_FOREACH(vars, iter) {
    node_ptr name = NodeList_get_elem_at(vars, iter);
    node_ptr init_name = find_node(nodemgr, SMALLINIT, name, Nil);
    node_ptr next_name = find_node(nodemgr, NEXT, name, Nil);
    node_ptr invar_expr = compile_get_rid_of_define_chain(cwd,
                            FlatHierarchy_lookup_assign(cwd->hierarchy, name));
    node_ptr init_expr = compile_get_rid_of_define_chain(cwd,
                            FlatHierarchy_lookup_assign(cwd->hierarchy, init_name));
    node_ptr next_expr = compile_get_rid_of_define_chain(cwd,
                            FlatHierarchy_lookup_assign(cwd->hierarchy, next_name));

    if ((init_expr != (node_ptr) NULL) ||
        (next_expr != (node_ptr) NULL) ||
        (invar_expr != (node_ptr) NULL)) {
      fprintf(cwd->out, "ASSIGN\n");
    }

    if (init_expr != (node_ptr) NULL) compile_print_assign(env,
                                                           cwd,
                                                           init_name,
                                                           init_expr);
    if (invar_expr != (node_ptr) NULL) compile_print_assign(env,
                                                            cwd,
                                                            name,
                                                            invar_expr);
    if (next_expr != (node_ptr) NULL) compile_print_assign(env,
                                                           cwd,
                                                           next_name,
                                                           next_expr);

    if ((init_expr != (node_ptr)NULL) ||
        (next_expr != (node_ptr)NULL) ||
        (invar_expr != (node_ptr)NULL)) {
      fprintf(cwd->out, "\n");
    }
  }

  fprintf(cwd->out, "\n");
  return 1;
}

/*!
  \brief Writes flattened ASSIGN declarations in SMV format on a
   file.

  Writes flattened ASSIGN declarations in SMV format on a
   file.
*/
static int compile_write_obfuscated_flat_asgn(const NuSMVEnv_ptr env,
                                              const CompileWriteData* cwd,
                                              const SymbLayer_ptr layer)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  SymbLayerIter iter;

  SYMB_LAYER_FOREACH(layer, iter, STT_VAR) {
    node_ptr name = SymbLayer_iter_get_symbol(layer, &iter);
    node_ptr init_name = find_node(nodemgr, SMALLINIT, name, Nil);
    node_ptr next_name = find_node(nodemgr, NEXT, name, Nil);
    node_ptr invar_expr =
      NodeAnonymizerBase_map_expr(cwd->anonymizer,
                              compile_get_rid_of_define_chain(cwd,
                                                              FlatHierarchy_lookup_assign(cwd->hierarchy,
                                                                                          name)));
    node_ptr init_expr =
      NodeAnonymizerBase_map_expr(cwd->anonymizer,
                              compile_get_rid_of_define_chain(cwd,
                                                              FlatHierarchy_lookup_assign(cwd->hierarchy,
                                                                                          init_name)));

    node_ptr next_expr =
      NodeAnonymizerBase_map_expr(cwd->anonymizer,
                              compile_get_rid_of_define_chain(cwd,
                                                              FlatHierarchy_lookup_assign(cwd->hierarchy,
                                                                                          next_name)));


    init_name = NodeAnonymizerBase_map_expr(cwd->anonymizer, init_name);
    next_name = NodeAnonymizerBase_map_expr(cwd->anonymizer, next_name);
    name = NodeAnonymizerBase_map_expr(cwd->anonymizer, name);

    if ((init_expr != (node_ptr) NULL) ||
        (next_expr != (node_ptr) NULL) ||
        (invar_expr != (node_ptr) NULL)) {
      fprintf(cwd->out, "ASSIGN\n");
    }

    if (init_expr != (node_ptr) NULL) compile_print_assign(env,
                                                           cwd,
                                                           init_name,
                                                           init_expr);
    if (invar_expr != (node_ptr) NULL) compile_print_assign(env,
                                                            cwd,
                                                            name,
                                                            invar_expr);
    if (next_expr != (node_ptr) NULL) compile_print_assign(env,
                                                           cwd,
                                                           next_name,
                                                           next_expr);

    if ((init_expr != (node_ptr)NULL) ||
        (next_expr != (node_ptr)NULL) ||
        (invar_expr != (node_ptr)NULL)) {
      fprintf(cwd->out, "\n");
    }
  }

  fprintf(cwd->out, "\n");
  return 1;
}

/*!
  \brief Prints an assignement statement

  Prints an assignement statement
*/
static void compile_print_assign(const NuSMVEnv_ptr env,
                                 const CompileWriteData* cwd,
                                 node_ptr lhs,
                                 node_ptr rhs)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  print_node(wffprint, cwd->out, lhs);
  fprintf(cwd->out, " := ");
  print_node(wffprint, cwd->out,
             Compile_convert_to_dag(env, cwd->symb_table, rhs,
                                    cwd->dag_info, cwd->defines));
  fprintf(cwd->out, ";\n");
}

/*!
  \brief Writes boolean VAR, FROZENVAR and IVAR declarations in
   SMV format on a file. Non boolean vars are dumped as defines for the sake of
   readability of conterexamples.


*/
static int compile_write_flatten_bool_vars(const NuSMVEnv_ptr env,
                                           const CompileWriteData* cwd,
                                           const BoolEnc_ptr bool_enc,
                                           const SymbLayer_ptr layer,
                                           const SymbTableType type)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  SymbLayerIter iter;
  enum { WFV_UNKNOWN, WFV_DEFINE,
         WFV_INPUT, WFV_STATE, WFV_FROZEN } last_insert = WFV_UNKNOWN;

  SYMB_LAYER_FOREACH(layer, iter, type) {
    node_ptr name = SymbLayer_iter_get_symbol(layer, &iter);

    if (SymbTable_is_symbol_var(cwd->symb_table, name)) {
      if (!SymbTable_is_symbol_bool_var(cwd->symb_table, name)) {
        if (!opt_backward_comp(opts)) {
          /* dumps the scalar variable as a define */
          node_ptr body;
          if (last_insert != WFV_DEFINE) {
            fprintf(cwd->out, "DEFINE\n");
            last_insert = WFV_DEFINE;
          }
          print_node(wffprint, cwd->out, name);
          fprintf(cwd->out, " := ");
          body = BoolEnc_get_var_encoding(bool_enc, name);
          print_node(wffprint, cwd->out, body);
          fprintf(cwd->out, ";\n");
        }
      }

      else {
        /* dumps it as boolean var */
        if (SymbTable_is_symbol_state_var(cwd->symb_table, name)
            && last_insert != WFV_STATE) {
          fprintf(cwd->out, "VAR\n");
          last_insert = WFV_STATE;
        }
        else if (SymbTable_is_symbol_frozen_var(cwd->symb_table, name)
                 && last_insert != WFV_FROZEN) {
          fprintf(cwd->out, "FROZENVAR\n");
          last_insert = WFV_FROZEN;
        }
        else if (SymbTable_is_symbol_input_var(cwd->symb_table, name)
                 &&
                 last_insert != WFV_INPUT) {
          fprintf(cwd->out, "IVAR\n");
          last_insert = WFV_INPUT;
        }

        print_node(wffprint, cwd->out, name);
        fprintf(cwd->out, " : ");
        SymbType_print(SymbTable_get_var_type(cwd->symb_table, name), wffprint, cwd->out);
        fprintf(cwd->out, ";\n"); /* end of the variable output */
      }
    }
  } /* loop */

  fprintf(cwd->out, "\n");
  return 1;
}

/*!
  \brief Writes flattened spec in SMV format on a file.

  Writes a generic spec prefixed by a given
   string in SMV format on a file.
*/
static int
compile_write_flatten_spec_split(const NuSMVEnv_ptr env,
                                 const CompileWriteData* cwd,
                                 node_ptr n, const char* s)
{
  if (n == Nil) return 0;

  switch (node_get_type(n)) {
  case CONS:
  case AND:
    compile_write_flatten_spec_split(env, cwd, car(n), s);

    compile_write_flatten_spec_split(env, cwd, cdr(n), s);
    break;

  default:
    compile_write_flatten_spec(env, cwd, n, s);
  } /* switch */

  return 1;
}

/*!
  \brief Writes flattened spec in SMV format on a file.

  Writes a generic spec prefixed by a given
   string in SMV format on a file.
*/
static int
compile_write_obfuscated_flatten_spec_split(const NuSMVEnv_ptr env,
                                            const CompileWriteData* cwd,
                                            node_ptr n,
                                            const char* s)
{
  if (n == Nil) return 0;

  switch (node_get_type(n)) {
  case CONS:
  case AND:
    compile_write_obfuscated_flatten_spec_split(env,
                                                cwd,
                                                car(n),
                                                s);

    compile_write_obfuscated_flatten_spec_split(env,
                                                cwd,
                                                cdr(n),
                                                s);
    break;

  default:
    compile_write_obfuscated_flatten_spec(env,
                                          cwd,
                                          n,
                                          s);
  } /* switch */

  return 1;
}

/*!
  \brief Writes flattened spec in SMV format on a file.

  Writes a generic spec prefixed by a given
   string in SMV format on a file.
   Returns true if at least one character was printed, and false otherwise.
*/
static int compile_write_flatten_spec(const NuSMVEnv_ptr env,
                                      const CompileWriteData* cwd,
                                      node_ptr n, const char* s)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
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

  fprintf(cwd->out, "%s ", s);

  /* Support for property Names: Old property structure is in car(n),
     property name is in cdr(n).  */
  if (Nil != name){
    fprintf(cwd->out, "NAME ");
    print_node(wffprint, cwd->out, name);
    fprintf(cwd->out, " := ");
  }

  /* flatten only if required, i.e. there is explicit context */
  if (CONTEXT == node_get_type(expr)) {
    if (cwd->force_flattening || (Nil != car(expr))) {
      expr = Compile_FlattenSexp(cwd->symb_table, cdr(expr), car(expr));
    }
  }

  print_node(wffprint, cwd->out,
             Compile_convert_to_dag(env, cwd->symb_table, expr,
                                    cwd->dag_info, cwd->defines));
  fprintf(cwd->out, ";\n\n");
  return 1;
}

/*!
  \brief Writes flattened spec in SMV format on a file.

  Writes a generic spec prefixed by a given
   string in SMV format on a file.
   Returns true if at least one character was printed, and false otherwise.
*/
static int compile_write_obfuscated_flatten_spec(const NuSMVEnv_ptr env,
                                                 const CompileWriteData* cwd,
                                                 node_ptr n,
                                                 const char* s)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
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

  fprintf(cwd->out, "%s ", s);

  /* Support for property Names: Old property structure is in car(n),
     property name is in cdr(n).  */
  if (Nil != name){
    fprintf(cwd->out, "NAME ");
    print_node(wffprint, cwd->out, name);
    fprintf(cwd->out, " := ");
  }

  /* flatten only if required, i.e. there is explicit context */
  if (CONTEXT == node_get_type(expr)) {
    if(cwd->force_flattening || (Nil != car(expr))) {
      expr = Compile_FlattenSexp(cwd->symb_table, cdr(expr), car(expr));
    }
  }

  print_node(wffprint, cwd->out,
             NodeAnonymizerBase_map_expr(
                 cwd->anonymizer,
                 Compile_convert_to_dag(env, cwd->symb_table,
                                        expr,
                                        cwd->dag_info,
                                        cwd->defines)));
  fprintf(cwd->out, "\n\n");
  return 1;
}

/*!
  \brief Writes flattened expression in SMV format on a file.

  Writes a generic expression prefixed by a given
   string in SMV format on a file.
*/
static int
compile_write_obfuscated_flatten_expr_split(const NuSMVEnv_ptr env,
                                            const CompileWriteData* cwd,
                                            node_ptr n,
                                            const char* s)
{
  if (n == Nil) return 0;

  switch (node_get_type(n)) {
  case CONS:
  case AND:
    compile_write_obfuscated_flatten_expr_split(env,
                                                cwd,
                                                car(n),
                                                s);

    compile_write_obfuscated_flatten_expr_split(env,
                                                cwd,
                                                cdr(n),
                                                s);
    break;

  default:
    compile_write_obfuscated_flatten_expr(env,
                                          cwd,
                                          n,
                                          s);
  } /* switch */

  return 1;
}

/*!
  \brief Writes flattened expression in SMV format on a file.

  Writes a generic expression prefixed by a given
   string in SMV format on a file.
*/
static int
compile_write_flatten_expr_split(const NuSMVEnv_ptr env,
                                 const CompileWriteData* cwd,
                                 node_ptr n, const char* s)
{
  if (n == Nil) return 0;

  switch (node_get_type(n)) {
  case CONS:
  case AND:
    compile_write_flatten_expr_split(env, cwd, car(n), s);

    compile_write_flatten_expr_split(env, cwd, cdr(n), s);
    break;

  default:
    compile_write_flatten_expr(env, cwd, n, s);
  } /* switch */

  return 1;
}

/*!
  \brief Writes flattened expression in SMV format on a file.

  Writes a generic expression prefixed by a given
   string in SMV format on a file.
   Returns true if at least one character was printed, and false otherwise.
*/
static int compile_write_flatten_expr(const NuSMVEnv_ptr env,
                                      const CompileWriteData* cwd,
                                      node_ptr n, const char* s)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  if (n == Nil || (node_get_type(n) == TRUEEXP &&
                   /* this check is optimization */
                   n == find_node(nodemgr, TRUEEXP, Nil, Nil))) return 0;

  /* flatten only if required, i.e. there is explicit context */
  if (CONTEXT == node_get_type(n)) {
    if (cwd->force_flattening || (Nil != car(n))) {
      n = Compile_FlattenSexp(cwd->symb_table, cdr(n), car(n));
    }
  }
  {
    node_ptr n1 = compile_get_rid_of_define_chain(cwd, n);
    if (n != n1) n = n1;
  }
  fprintf(cwd->out, "%s ", s);
  print_node(wffprint, cwd->out,
             Compile_convert_to_dag(env, cwd->symb_table, n,
                                    cwd->dag_info, cwd->defines));
  fprintf(cwd->out, "\n\n");
  return 1;
}

/*!
  \brief Writes flattened expression in SMV format on a file.

  Writes a generic expression prefixed by a given
   string in SMV format on a file.
   Returns true if at least one character was printed, and false otherwise.
*/
static int compile_write_obfuscated_flatten_expr(const NuSMVEnv_ptr env,
                                                 const CompileWriteData* cwd,
                                                 node_ptr n,
                                                 const char* s)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
  node_ptr n_anonymous = NULL;

  if (n == Nil || (node_get_type(n) == TRUEEXP &&
                   /* this check is optimization */
                   n == find_node(nodemgr, TRUEEXP, Nil, Nil))) return 0;

  /* flatten only if required, i.e. there is explicit context */
  if (CONTEXT == node_get_type(n)) {
    if(cwd->force_flattening || (Nil != car(n))) {
      n = Compile_FlattenSexp(cwd->symb_table, cdr(n), car(n));
    }
  }
  {
    node_ptr n1 = compile_get_rid_of_define_chain(cwd, n);
    if (n != n1) n = n1;
  }
  fprintf(cwd->out, "%s ", s);

  n_anonymous = NodeAnonymizerBase_map_expr(
      cwd->anonymizer,
      Compile_convert_to_dag(env, cwd->symb_table, n,
                             cwd->dag_info, cwd->defines));

  print_node(wffprint, cwd->out, n_anonymous);
  fprintf(cwd->out, "\n\n");
  return 1;
}

/*!
  \brief Writes PSL properties as they are.


*/
static int compile_write_flatten_psl(const NuSMVEnv_ptr env,
                                     const CompileWriteData* cwd,
                                     node_ptr n)
{
  if (n == Nil) return 0;

  switch (node_get_type(n)) {
  case CONS:
  case AND:
    compile_write_flatten_psl(env, cwd, car(n));
    compile_write_flatten_psl(env, cwd, cdr(n));
    break;

  default:
    {
      const MasterPrinter_ptr wffprint =
        MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
      node_ptr expr, name, dagged;

      nusmv_assert(PSLSPEC == node_get_type(n));

      expr = car(n);
      name = cdr(n);
      dagged = Compile_convert_to_dag(env,
                                      cwd->symb_table,
                                      expr,
                                      cwd->dag_info,
                                      cwd->defines);

      fprintf(cwd->out, "-- PSLSPEC\n--   ");

      /* Support for property names */
      if (Nil != name){
        fprintf(cwd->out, "NAME ");
        print_node(wffprint, cwd->out, name);
        fprintf(cwd->out, " := ");
      }

      print_node(wffprint, cwd->out, dagged);
      fprintf(cwd->out, "\n\n");
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
static int compile_write_flatten_bfexpr(const NuSMVEnv_ptr env,
                                        const CompileWriteData* cwd,
                                        node_ptr n, const char* s)
{
  const ExprMgr_ptr exprs = EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  /* Nil and TRUEEXP indicate the end of a list */
  if (n == Nil || ExprMgr_is_true(exprs, n)) return 0;

  switch (node_get_type(n)) {
  case CONS:
  case AND:
    compile_write_flatten_bfexpr(env, cwd, car(n), s);
    compile_write_flatten_bfexpr(env, cwd, cdr(n), s);
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
      fprintf(cwd->out, "\n%s", s);

      if (Nil != name){
        fprintf(cwd->out, "NAME ");
        print_node(wffprint, cwd->out, name);
        fprintf(cwd->out, " := ");
      }

      expr = Compile_convert_to_dag(
          env, cwd->symb_table,
          Compile_expr2bexpr(cwd->enc, cwd->det_layer, expr),
          cwd->dag_info, cwd->defines);

      print_node(wffprint, cwd->out, expr);
      fprintf(cwd->out, "\n");
    }
  } /* switch */

  return 1;
}

/*!
  \brief Writes flattened expression pairs in SMV format on a
   file.

  Writes a list of flattened expression pairs prefixed by
   a given string in SMV format on a file.
*/
static int compile_write_flatten_expr_pair(const NuSMVEnv_ptr env,
                                           const CompileWriteData* cwd,
                                           node_ptr l, const char* s)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  if (l == Nil)
      return 0;

  while (l) {
    node_ptr n = car(l);
    l = cdr(l);
    nusmv_assert(node_get_type(n) == CONS);

    fprintf(cwd->out, "%s (", s);

    if (node_get_type(n) == CONTEXT) {
      node_ptr fn;

      fn = n;
      if (cwd->force_flattening || (Nil != car(n))) {
        /* flats the context */
        fn = Compile_FlattenSexp(cwd->symb_table, car(n), Nil);
      }
      fn = Compile_convert_to_dag(env, cwd->symb_table, fn,
                                  cwd->dag_info, cwd->defines);
      print_node(wffprint, cwd->out, fn);
    }
    else print_node(wffprint, cwd->out,
                    Compile_convert_to_dag(env, cwd->symb_table,
                                           car(n),
                                           cwd->dag_info,
                                           cwd->defines));

    fprintf(cwd->out, ", ");
    if (node_get_type(n) == CONTEXT) {
      node_ptr fn;

      fn = n;
      if (cwd->force_flattening || (Nil != car(n))) {
        /* flats the definition */
        fn = Compile_FlattenSexp(cwd->symb_table, cdr(n), Nil);
      }
      fn = Compile_convert_to_dag(env, cwd->symb_table, fn,
                                  cwd->dag_info, cwd->defines);
      print_node(wffprint, cwd->out, fn);
    }
    else print_node(wffprint, cwd->out,
                    Compile_convert_to_dag(env, cwd->symb_table,
                                           cdr(n),
                                           cwd->dag_info,
                                           cwd->defines));

    fprintf(cwd->out, ")\n\n");
  }
  return 1;
}

/*!
  \brief Writes flattened expression pairs in SMV format on a
   file.

  Writes a list of flattened expression pairs prefixed by
   a given string in SMV format on a file.
*/
static int
compile_write_obfuscated_flatten_expr_pair(const NuSMVEnv_ptr env,
                                           const CompileWriteData* cwd,
                                           node_ptr l,
                                           const char* s)
{
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  if (l == Nil) return 0;

  while (l) {
    node_ptr n = car(l);
    l = cdr(l);
    nusmv_assert(node_get_type(n) == CONS);

    fprintf(cwd->out, "%s (", s);

    if (node_get_type(n) == CONTEXT) {
      node_ptr fn;

      fn = n;
      if (cwd->force_flattening || (Nil != car(n))) {
        /* flats the context */
        fn = Compile_FlattenSexp(cwd->symb_table, car(n), Nil);
      }
      fn = Compile_convert_to_dag(env, cwd->symb_table, fn,
                                  cwd->dag_info, cwd->defines);
      fn = NodeAnonymizerBase_map_expr(cwd->anonymizer, fn);
      print_node(wffprint, cwd->out, fn);
    }
    else print_node(wffprint, cwd->out,
                    NodeAnonymizerBase_map_expr(
                        cwd->anonymizer,
                        Compile_convert_to_dag(env,
                                               cwd->symb_table,
                                               car(n),
                                               cwd->dag_info,
                                               cwd->defines)));

    fprintf(cwd->out, ", ");
    if (node_get_type(n) == CONTEXT) {
      node_ptr fn;

      fn = n;
      if (cwd->force_flattening || (Nil != car(n))) {
        /* flats the definition */
        fn = Compile_FlattenSexp(cwd->symb_table, cdr(n), Nil);
      }
      fn = Compile_convert_to_dag(env,
                                  cwd->symb_table, fn,
                                  cwd->dag_info, cwd->defines);
      fn = NodeAnonymizerBase_map_expr( cwd->anonymizer,  fn);
      print_node(wffprint, cwd->out, fn);
    }
    else print_node(wffprint, cwd->out,
                    NodeAnonymizerBase_map_expr(
                        cwd->anonymizer,
                        Compile_convert_to_dag(env,
                                               cwd->symb_table,
                                               cdr(n),
                                               cwd->dag_info,
                                               cwd->defines)));

    fprintf(cwd->out, ")\n\n");
  }
  return 1;
}

/*!
  \brief Writes the set of non-numeric constants as CONSTANTS
   statement

  Returns 1 if at least one char have been written, 0
   otherwise
*/
static int compile_write_constants(const SymbTable_ptr symb_table, FILE* out)
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

/*!
  \brief Writes the set of non-numeric constants as CONSTANTS
   statement

  Returns 1 if at least one char have been written, 0
   otherwise
*/
static int compile_write_obfuscated_constants(const CompileWriteData* cwd)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(cwd->symb_table));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  boolean written_once = false;
  SymbTableIter iter;

  SYMB_TABLE_FOREACH(cwd->symb_table, iter, STT_CONSTANT) {
    node_ptr name = SymbTable_iter_get_symbol(cwd->symb_table, &iter);
    node_ptr name_anonymous = NULL;

    if (node_get_type(name) == ATOM || node_get_type(name) == DOT) {
      /* a name to be written */
      if (!written_once) {
        fprintf(cwd->out, "CONSTANTS\n ");
        written_once = true;
      }
      else fprintf(cwd->out, ", ");

      name_anonymous = NodeAnonymizerBase_map_expr(cwd->anonymizer, name);
      print_node(wffprint, cwd->out, name_anonymous);
    }
  }

  if (written_once) {
    fprintf(cwd->out, " ;\n"); /* to close the statement */
    return 1;
  }

  return 0;
}

/*!
  \brief Prints the obfuscated flatten version of FSM of an
   SMV model.



  \sa compile_write_flat_fsm
*/
static void compile_write_obfuscated_flat_fsm(const NuSMVEnv_ptr env,
                                              const CompileWriteData* cwd)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  int i;
  const char* name;
  char* ifile = get_input_file(opts);

  nusmv_assert(cwd->layer_names != (array_t*) NULL);

  fprintf(cwd->out, "-- Obfuscated and flattened FSM model generated from %s\n"
          "-- Dumped layers are: ", ((char*)NULL != ifile ? ifile : "stdin"));

  /* dumps the layer names: */
  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    fprintf(cwd->out, "%s ", name);
  }
  fprintf(cwd->out, "\n\n");

  fprintf(cwd->out, "%s\n", cwd->fsm_name);

  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- Input variables from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      if (SymbLayer_get_input_vars_num(layer) > 0) {
        compile_write_obfuscated_flatten_vars(env, cwd,
                                              layer, STT_INPUT_VAR);
      }
    }
  }

  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- State variables from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      if (SymbLayer_get_state_vars_num(layer) > 0) {
        compile_write_obfuscated_flatten_vars(env, cwd,
                                              layer, STT_STATE_VAR);
      }
    }
  }

  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- Frozen variables from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      if (SymbLayer_get_frozen_vars_num(layer) > 0) {
        compile_write_obfuscated_flatten_vars(env, cwd,
                                              layer, STT_FROZEN_VAR);
      }
    }
  }

  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- Functions from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      if (SymbLayer_get_functions_num(layer) > 0) {
        compile_write_obfuscated_flatten_funs(env, cwd, layer, STT_FUNCTION);
      }
    }
  }


  /* NOTE that array variables (i.e. variable_array) are not output as
     they are output during vars outputting in
     compile_write_flatten_vars */

  /* DEFINEs */
  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- Defines from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      compile_write_obfuscated_flat_define(env,
                                           cwd,
                                           layer);
    }
  }

  /* NOTE that array cwd->defines (i.e. array_define) are not output as
     they are output during normal cwd->defines outputting in
     compile_write_obfuscated_flat_define  */

  /* CONSTANTS */
  if (!opt_backward_comp(opts)) {
    if (compile_write_obfuscated_constants(cwd)) {
      fprintf(cwd->out, "\n");
    }
  }

  /* ASSIGNs */
  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- Assignments from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      compile_write_obfuscated_flat_asgn(env,
                                         cwd,
                                         layer);
    }
  }

  /* CONSTRAINS (except assigns) */
  if (compile_write_obfuscated_flatten_expr_split(env, cwd,
                                                  FlatHierarchy_get_init(cwd->hierarchy),
                                                  "INIT\n")) {
    fprintf(cwd->out, "\n");
  }

  if (compile_write_obfuscated_flatten_expr_split(env, cwd,
                                                  FlatHierarchy_get_invar(cwd->hierarchy),
                                                  "INVAR\n")) {
    fprintf(cwd->out, "\n");
  }

  if (compile_write_obfuscated_flatten_expr_split(env, cwd,
                                                  FlatHierarchy_get_trans(cwd->hierarchy),
                                                  "TRANS\n")) {
    fprintf(cwd->out, "\n");
  }

  {
    node_ptr fc = FlatHierarchy_get_justice(cwd->hierarchy);
    boolean are_there_compassion =
      (Nil != FlatHierarchy_get_compassion(cwd->hierarchy));

    while (Nil != fc) {
      if (compile_write_obfuscated_flatten_expr(env, cwd, car(fc),
                                                /* For backward compatibility */
                                                are_there_compassion ?
                                                "JUSTICE\n" : "FAIRNESS\n")) {
        fprintf(cwd->out, "\n");
      }
      fc = cdr(fc);
    }
  }

  if (compile_write_obfuscated_flatten_expr_pair(env, cwd,
                                                 FlatHierarchy_get_compassion(cwd->hierarchy),
                                                 "COMPASSION\n")) {
    fprintf(cwd->out, "\n");
  }
  return ;
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
static void compile_write_flat_fsm(const NuSMVEnv_ptr env,
                                   const CompileWriteData* cwd)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  SymbLayerIter iter;
  NodeList_ptr tmp;
  int i;
  const char* name;
  char* ifile = get_input_file(opts);

  nusmv_assert(cwd->layer_names != (array_t*) NULL);

  fprintf(cwd->out, "-- Flattened FSM model generated from %s\n"
          "-- Dumped layers are: ", ((char*)NULL != ifile ? ifile : "stdin"));

  /* dumps the layer names: */
  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    fprintf(cwd->out, "%s ", name);
  }
  fprintf(cwd->out, "\n\n");

  fprintf(cwd->out, "%s\n", cwd->fsm_name);

  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- Input variables from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      SymbLayer_gen_iter(layer, &iter, STT_INPUT_VAR);
      compile_write_flatten_vars(env, cwd, layer, &iter);
    }
  }

  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- State variables from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      SymbLayer_gen_iter(layer, &iter, STT_STATE_VAR);
      compile_write_flatten_vars(env, cwd, layer, &iter);
    }
  }

  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- Frozen variables from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      SymbLayer_gen_iter(layer, &iter, STT_FROZEN_VAR);
      compile_write_flatten_vars(env, cwd, layer, &iter);
    }
  }

  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- Functions from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      SymbLayer_gen_iter(layer, &iter, STT_FUNCTION);
      compile_write_flatten_funs(env, cwd, layer, &iter);
    }
  }


  /* NOTE that array variables (i.e. variable_array) are not output as
     they are output during vars outputting in
     compile_write_flatten_vars */

  /* DEFINEs */
  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    NodeList_ptr defines_list;

    SymbLayer_gen_iter(layer, &iter, STT_DEFINE);
    defines_list = SymbLayer_iter_to_list(layer, iter);

    fprintf(cwd->out, "-- Defines from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      compile_write_flat_define(env,
                                cwd,
                                defines_list);
    }

    NodeList_destroy(defines_list);
  }

  /* NOTE that array cwd->defines (i.e. array_define) are not output as
     they are output during normal cwd->defines outputting in
     compile_write_flat_define  */

  /* CONSTANTS */
  if (!opt_backward_comp(opts)) {
    if (compile_write_constants(cwd->symb_table, cwd->out)) fprintf(cwd->out, "\n");
  }

  /* ASSIGNs */
  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- Assignments from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      SymbLayer_gen_iter(layer, &iter, STT_VAR);
      tmp = SymbLayer_iter_to_list(layer, iter);

      compile_write_flat_asgn(env, cwd, tmp);
      NodeList_destroy(tmp);
    }
  }

  /* CONSTRAINS (except assigns) */
  if (compile_write_flatten_expr_split(env, cwd,
                                       FlatHierarchy_get_init(cwd->hierarchy),
                                       "INIT\n")) {
    fprintf(cwd->out, "\n");
  }

  if (compile_write_flatten_expr_split(env, cwd,
                                       FlatHierarchy_get_invar(cwd->hierarchy),
                                       "INVAR\n")) {
    fprintf(cwd->out, "\n");
  }
  if (compile_write_flatten_expr_split(env, cwd,
                                       FlatHierarchy_get_trans(cwd->hierarchy),
                                       "TRANS\n")) {
    fprintf(cwd->out, "\n");
  }
  {
    node_ptr fc = FlatHierarchy_get_justice(cwd->hierarchy);
    boolean are_there_compassion =
      (Nil != FlatHierarchy_get_compassion(cwd->hierarchy));

    while (Nil != fc) {
      if (compile_write_flatten_expr(env, cwd, car(fc),
                                     /* For backward compatibility */
                                     are_there_compassion ?
                                     "JUSTICE\n" : "FAIRNESS\n")) {
        fprintf(cwd->out, "\n");
      }
      fc = cdr(fc);
    }
  }

  if (compile_write_flatten_expr_pair(env,
                                      cwd,
                                      FlatHierarchy_get_compassion(cwd->hierarchy),
                                      "COMPASSION\n")) {
    fprintf(cwd->out, "\n");
  }
  return ;
}

/*!
  \brief Filter for a SymbLayer iter, filters out variables
                       that are not in the set given as argument

  Filter for a SymbLayer iter, filters out variables
                       that are not in the set given as argument
*/
static boolean
compile_write_is_var_in_set(const SymbLayer_ptr layer,
                            const node_ptr sym, void* arg)
{
  Set_t keep_vars = (Set_t)arg;

  return Set_IsMember(keep_vars, sym);
}

/*!
  \brief Prints the restricted flatten version of FSM of
                       an SMV model.

  Prints on the specified file the flatten
   FSM of an SMV model, i.e. a list of restricted variables, defines, and all
   constrains (INIT, TRANS, INVAR, ASSIGNS, JUSTICE, COMPASSION) restricted to
   the set of variables in the FlatHierarchy.
   Specifications are NOT printed.

   layer_names is an array of names of layers that is typically
   obtained from the symbol table. fsm_name is a name of the output
   structure, usually it is "MODULE main".
*/
static void compile_write_restricted_flat_fsm(const NuSMVEnv_ptr env,
                                              const CompileWriteData* cwd)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  Set_t restrict_on = FlatHierarchy_get_vars(cwd->hierarchy);

  int i;
  const char* name;
  char* ifile = get_input_file(opts);

  nusmv_assert(cwd->layer_names != (array_t*) NULL);

  fprintf(cwd->out, "-- Flattened FSM model generated from %s\n"
          "-- Dumped layers are: ", ((char*)NULL != ifile ? ifile : "stdin"));

  /* dumps the layer names: */
  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    fprintf(cwd->out, "%s ", name);
  }
  fprintf(cwd->out, "\n\n");

  fprintf(cwd->out, "%s\n", cwd->fsm_name);

  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- Input variables from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      SymbLayerIter iter;

      SymbLayer_gen_iter(layer, &iter, STT_INPUT_VAR);
      SymbLayer_iter_set_filter(layer, &iter,
                                compile_write_is_var_in_set, restrict_on);
      compile_write_flatten_vars(env, cwd, layer, &iter);
    }
  }

  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- State variables from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      SymbLayerIter iter;

      SymbLayer_gen_iter(layer, &iter, STT_STATE_VAR);
      SymbLayer_iter_set_filter(layer, &iter,
                                compile_write_is_var_in_set, restrict_on);
      compile_write_flatten_vars(env, cwd, layer, &iter);
    }
  }

  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- Frozen variables from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      SymbLayerIter iter;

      SymbLayer_gen_iter(layer, &iter, STT_FROZEN_VAR);
      SymbLayer_iter_set_filter(layer, &iter,
                                compile_write_is_var_in_set, restrict_on);
      compile_write_flatten_vars(env, cwd, layer, &iter);
    }
  }

  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- Functions from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      SymbLayerIter iter;

      SymbLayer_gen_iter(layer, &iter, STT_FUNCTION);
      SymbLayer_iter_set_filter(layer, &iter,
                                compile_write_is_var_in_set, restrict_on);
      compile_write_flatten_funs(env, cwd, layer, &iter);
    }
  }

  /* NOTE that array variables (i.e. variable_array) are not output as
     they are output during vars outputting in
     compile_write_flatten_vars */

  {
    /* DEFINEs */
    arrayForEachItem(const char*, cwd->layer_names, i, name) {
      SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);

      fprintf(cwd->out, "-- Defines from layer '%s'\n", name);
      if (layer != SYMB_LAYER(NULL)) {
        /* We want to add only cwd->defines that actually only contain
           variables that are in the COI. Otherwise, we may have
           expressions refering to undeclare variables */

        NodeList_ptr restricted_defines = NodeList_create();
        SymbLayerIter iter;

        SYMB_LAYER_FOREACH(layer, iter, STT_DEFINE) {
          node_ptr define = SymbLayer_iter_get_symbol(layer, &iter);

          node_ptr body = SymbTable_get_define_body(cwd->symb_table,
                                                    define);
          node_ptr ctx = SymbTable_get_define_context(cwd->symb_table,
                                                      define);

          Set_t deps =  Formula_GetDependencies(cwd->symb_table, body, ctx);

          if (Set_Contains(restrict_on, deps)) {
            NodeList_append(restricted_defines, define);
          }
        }

        compile_write_flat_define(env,
                                  cwd,
                                  restricted_defines);

      NodeList_destroy(restricted_defines);
      }
    }
  }

  /* NOTE that array cwd->defines (i.e. array_define) are not output as
     they are output during normal cwd->defines outputting in
     compile_write_flat_define  */

  /* CONSTANTS */
  if (!opt_backward_comp(opts)) {
    if (compile_write_constants(cwd->symb_table, cwd->out)) fprintf(cwd->out, "\n");
  }

  /* ASSIGNs */
  arrayForEachItem(const char*, cwd->layer_names, i, name) {
    SymbLayer_ptr layer = SymbTable_get_layer(cwd->symb_table, name);
    fprintf(cwd->out, "-- Assignments from layer '%s'\n", name);
    if (layer != SYMB_LAYER(NULL)) {
      SymbLayerIter iter;
      Set_t layer_vars;
      NodeList_ptr tmp;

      SymbLayer_gen_iter(layer, &iter, STT_VAR);
      layer_vars = SymbLayer_iter_to_set(layer, iter);

      layer_vars = Set_Intersection(layer_vars, restrict_on);

      tmp  = Set_Set2List(layer_vars);
      compile_write_flat_asgn(env, cwd, tmp);
      Set_ReleaseSet(layer_vars);
    }
  }

  /* CONSTRAINS (except assigns) */
  if (compile_write_flatten_expr_split(env, cwd,
                                       FlatHierarchy_get_init(cwd->hierarchy),
                                       "INIT\n")) {
    fprintf(cwd->out, "\n");
  }

  if (compile_write_flatten_expr_split(env, cwd,
                                       FlatHierarchy_get_invar(cwd->hierarchy),
                                       "INVAR\n")) {
    fprintf(cwd->out, "\n");
  }

  if (compile_write_flatten_expr_split(env, cwd,
                                       FlatHierarchy_get_trans(cwd->hierarchy),
                                       "TRANS\n")) {
    fprintf(cwd->out, "\n");
  }

  {
    node_ptr fc = FlatHierarchy_get_justice(cwd->hierarchy);
    boolean are_there_compassion =
      (Nil != FlatHierarchy_get_compassion(cwd->hierarchy));

    while (Nil != fc) {
      if (compile_write_flatten_expr(env, cwd, car(fc),
                                     /* For backward compatibility */
                                     are_there_compassion ?
                                     "JUSTICE\n" : "FAIRNESS\n")) {
        fprintf(cwd->out, "\n");
      }
      fc = cdr(fc);
    }
  }

  if (compile_write_flatten_expr_pair(env,
                                      cwd,
                                      FlatHierarchy_get_compassion(cwd->hierarchy),
                                      "COMPASSION\n")) {
    fprintf(cwd->out, "\n");
  }
  return ;
}

/*!
  \brief Prints the given flatten specifications.

  Prints into the specified file the flatten
   specifications.
*/
static void compile_write_flat_spec(const NuSMVEnv_ptr env,
                                    const CompileWriteData* cwd,
                                    node_ptr spec, const char* msg)
{
  if (compile_write_flatten_spec_split(env, cwd, spec, msg)) {
    fprintf(cwd->out, "\n");
  }
}

/*!
  \brief Prints the given flatten specifications.

  Prints into the specified file the flatten
   specifications.
*/

static void compile_write_obfuscated_flat_spec(const NuSMVEnv_ptr env,
                                               const CompileWriteData* cwd,
                                               node_ptr spec,
                                               const char* msg)
{
  if (compile_write_obfuscated_flatten_spec_split(env,
                                                  cwd,
                                                  spec,
                                                  msg)) {
    fprintf(cwd->out, "\n");
  }
}

/*!
  \brief Prints the flatten specifications of an SMV model.

  Prints into the specified file the
   specifications of an SMV model.

*/
static void compile_write_flat_specs(const NuSMVEnv_ptr env,
                                     const CompileWriteData* cwd)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  /* dumps the properties */
  compile_write_flat_spec(env, cwd, FlatHierarchy_get_spec(cwd->hierarchy),
                          "CTLSPEC\n");
  compile_write_flat_spec(env, cwd, FlatHierarchy_get_compute(cwd->hierarchy),
                          "COMPUTE\n");
  compile_write_flat_spec(env, cwd, FlatHierarchy_get_ltlspec(cwd->hierarchy),
                          "LTLSPEC\n");
  compile_write_flat_spec(env, cwd, FlatHierarchy_get_invarspec(cwd->hierarchy),
                          "INVARSPEC\n");

  { /* PSL specifications are not supported at the moment */
    node_ptr pslspec = FlatHierarchy_get_pslspec(cwd->hierarchy);
    if (pslspec != Nil) {
      /* see issue https://essvn.fbk.eu/bugs/view.php?id=2626 */
      StreamMgr_print_error(streams,  "\n********   WARNING   ********\n");
      StreamMgr_print_error(streams,
              "This version does not support the flattening of PSL properties.\n"
              "However, for user's convenience all the PSL properties will be dumped\n"
              "as comments in the output file.\n");
      StreamMgr_print_error(streams,  "******** END WARNING ********\n\n");

      fprintf(cwd->out,
              "--- Dumping of PSL properties is not supported by this version of the system.\n"\
              "--- However, the PSL properties had been dumped here for user's convenience,\n"\
              "--- as the occurred in the original model. \n");
      compile_write_flatten_psl(env, cwd, pslspec);
    }
  }
}

/*!
  \brief Prints the obfuscated flatten specifications of an
   SMV model.



  \sa compile_write_flat_specs
*/
static void compile_write_obfuscated_flat_specs(const NuSMVEnv_ptr env,
                                                const CompileWriteData* cwd)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  /* dumps the properties */
  compile_write_obfuscated_flat_spec(env,
                                     cwd,
                                     FlatHierarchy_get_spec(cwd->hierarchy),
                                     "CTLSPEC\n");
  compile_write_obfuscated_flat_spec(env,
                                     cwd,
                                     FlatHierarchy_get_compute(cwd->hierarchy),
                                     "COMPUTE\n");
  compile_write_obfuscated_flat_spec(env,
                                     cwd,
                                     FlatHierarchy_get_ltlspec(cwd->hierarchy),
                                     "LTLSPEC\n");
  compile_write_obfuscated_flat_spec(env,
                                     cwd,
                                     FlatHierarchy_get_invarspec(cwd->hierarchy),
                                     "INVARSPEC\n");

  { /* PSL specifications are not supported at the moment */
    node_ptr pslspec = FlatHierarchy_get_pslspec(cwd->hierarchy);
    if (pslspec != Nil) {
      /* see issue https://essvn.fbk.eu/bugs/view.php?id=2626 */
      StreamMgr_print_error(streams,  "\n********   WARNING   ********\n");
      StreamMgr_print_error(streams,
              "This version does not support the flattening of PSL properties.\n"
              "However, for user's convenience all the PSL properties will be dumped\n"
              "as comments in the output file.\n");
      StreamMgr_print_error(streams,  "******** END WARNING ********\n\n");

      fprintf(cwd->out,
              "--- Dumping of PSL properties is not supported by this version of the system.\n"\
              "--- However, the PSL properties had been dumped here for user's convenience,\n"\
              "--- as the occurred in the original model. \n");
      compile_write_flatten_psl(env, cwd, pslspec);
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
static void compile_write_bool_fsm(const NuSMVEnv_ptr env,
                                   const CompileWriteData* cwd)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

  BoolEnc_ptr benc;
  ListIter_ptr iter;
  char* ifile = get_input_file(opts);
  /* must have been booleanized */
  nusmv_assert(SexpFsm_is_boolean(SEXP_FSM(cwd->bool_sexp_fsm)));

  benc = BoolSexpFsm_get_bool_enc(cwd->bool_sexp_fsm);

  fprintf(cwd->out,
          "-- Flattened Boolean FSM model  generated from %s\n"
          "-- Dumped cwd->layers are: ",
          ((char*)NULL != ifile ? ifile : "stdin"));

  /* dumps the layer names: */
  iter = NodeList_get_first_iter(cwd->nodelist_layer);
  while (!ListIter_is_end(iter)) {
    SymbLayer_ptr layer = SYMB_LAYER(NodeList_get_elem_at(cwd->nodelist_layer, iter));
    fprintf(cwd->out, "%s", SymbLayer_get_name(layer));
    fprintf(cwd->out, " ");
    iter = ListIter_get_next(iter);
  }
  fprintf(cwd->out, "\n\n");

  fprintf(cwd->out, "%s\n", cwd->fsm_name);

  /* NOTE: Defines are not dumped, therefore should not be booleanized */

  /* iter = NodeList_get_first_iter(cwd->layers); */
  /* while (!ListIter_is_end(iter)) { */
  /*   SymbLayer_ptr layer = SYMB_LAYER(NodeList_get_elem_at(cwd->layers, iter)); */
  /*   NodeList_ptr defines_list; */
  /*   SymbLayerIter sliter; */

  /*   SymbLayer_gen_iter(layer, &sliter, STT_DEFINE); */
  /*   defines_list = SymbLayer_iter_to_list(layer, sliter); */

  /*   fprintf(out, "-- Defines from layer '%s'\n", SymbLayer_get_name(layer)); */
  /*   if (layer != SYMB_LAYER(NULL)) { */
  /*     compile_write_flat_define(cwd->symb_table, */
  /*                               out, */
  /*                               defines_list, */
  /*                               cwd->dag_info, */
  /*                               cwd->defines, */
  /*                               cwd->force_flattening); */
  /*   } */

  /*   NodeList_destroy(defines_list); */
  /*   iter = ListIter_get_next(iter); */
  /* } */

  /* Input variables */
  iter = NodeList_get_first_iter(cwd->nodelist_layer);
  while (!ListIter_is_end(iter)) {
    SymbLayer_ptr layer = SYMB_LAYER(NodeList_get_elem_at(cwd->nodelist_layer, iter));
    fprintf(cwd->out, "-- Input variables from layer '%s'\n",
            SymbLayer_get_name(layer));
    if (SymbLayer_get_input_vars_num(layer) > 0) {
      compile_write_flatten_bool_vars(env, cwd, benc,
                                      layer, STT_INPUT_VAR);
    }
    iter = ListIter_get_next(iter);
  }

  /* State variables */
  iter = NodeList_get_first_iter(cwd->nodelist_layer);
  while (!ListIter_is_end(iter)) {
    SymbLayer_ptr layer = SYMB_LAYER(NodeList_get_elem_at(cwd->nodelist_layer, iter));
    fprintf(cwd->out, "-- State variables from layer '%s'\n",
            SymbLayer_get_name(layer));
    if (SymbLayer_get_state_vars_num(layer) > 0) {
      compile_write_flatten_bool_vars(env, cwd, benc,
                                      layer, STT_STATE_VAR);
    }
    iter = ListIter_get_next(iter);
  }

  /* Frozen variables */
  iter = NodeList_get_first_iter(cwd->nodelist_layer);
  while (!ListIter_is_end(iter)) {
    SymbLayer_ptr layer =
        SYMB_LAYER(NodeList_get_elem_at(cwd->nodelist_layer, iter));
    fprintf(cwd->out, "-- Frozen variables from layer '%s'\n",
            SymbLayer_get_name(layer));
    if (SymbLayer_get_frozen_vars_num(layer) > 0) {
      compile_write_flatten_bool_vars(env, cwd, benc,
                                      layer, STT_FROZEN_VAR);
    }
    iter = ListIter_get_next(iter);
  }

  /* CONSTANTS */
  if (!opt_backward_comp(opts)) {
    if (compile_write_constants(cwd->symb_table, cwd->out))
      fprintf(cwd->out, "\n");
  }

  /* CONSTRAINS */
  if (compile_write_flatten_expr_split(env, cwd,
                                SexpFsm_get_init(SEXP_FSM(cwd->bool_sexp_fsm)),
                                       "INIT\n")) {
    fprintf(cwd->out, "\n");
  }

  if (compile_write_flatten_expr_split(env, cwd,
                                SexpFsm_get_invar(SEXP_FSM(cwd->bool_sexp_fsm)),
                                       "INVAR\n")) {
    fprintf(cwd->out, "\n");
  }

  if (compile_write_flatten_expr_split(env, cwd,
                                SexpFsm_get_trans(SEXP_FSM(cwd->bool_sexp_fsm)),
                                       "TRANS\n")) {
    fprintf(cwd->out, "\n");
  }


  {
    node_ptr fc = SexpFsm_get_justice(SEXP_FSM(cwd->bool_sexp_fsm));
    boolean are_there_compassion =
      (Nil != SexpFsm_get_compassion(SEXP_FSM(cwd->bool_sexp_fsm)));

    while(Nil != fc) {
      if (compile_write_flatten_expr(env, cwd,
                                     car(fc),
                                     /* For backward compatibility */
                                     are_there_compassion ?
                                     "JUSTICE\n" : "FAIRNESS\n")) {
        fprintf(cwd->out, "\n");
      }
      fc = cdr(fc);
    }
  }

  if (compile_write_flatten_expr_pair(env, cwd,
                           SexpFsm_get_compassion(SEXP_FSM(cwd->bool_sexp_fsm)),
                                     "COMPASSION\n")) {
    fprintf(cwd->out, "\n\n");
  }

}

/*!
  \brief Private service to print a boolean specification


*/
static void compile_write_bool_spec(const NuSMVEnv_ptr env,
                                    const CompileWriteData* cwd,
                                    node_ptr spec, const char* msg)
{
  if (compile_write_flatten_bfexpr(env, cwd, spec, msg)) {
    fprintf(cwd->out, "\n");
  }
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
static void compile_write_bool_specs(const NuSMVEnv_ptr env,
                                     const CompileWriteData* cwd)
{
  const StreamMgr_ptr streams =
    STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));
  /* here we create a temporary layer, in order to hold all
     determinization variables that will be created by the
     booleanization of the properties. This layer will be destroyed
     after the printing of the determinization variables that it will
     possibly contain */
  SymbTable_ptr st = BaseEnc_get_symb_table(BASE_ENC(cwd->enc));

  /* dumps the properties */
  compile_write_bool_spec(env, cwd,
                          FlatHierarchy_get_spec(cwd->hierarchy),
                          "CTLSPEC\n");
  compile_write_bool_spec(env, cwd, FlatHierarchy_get_compute(cwd->hierarchy),
                          "COMPUTE\n");
  compile_write_bool_spec(env, cwd, FlatHierarchy_get_ltlspec(cwd->hierarchy),
                          "LTLSPEC\n");
  compile_write_bool_spec(env, cwd, FlatHierarchy_get_invarspec(cwd->hierarchy),
                          "INVARSPEC\n");

  { /* PSL specifications are not supported at the moment */
    node_ptr pslspec = FlatHierarchy_get_pslspec(cwd->hierarchy);
    if (pslspec != Nil) {
      /* see issue https://essvn.fbk.eu/bugs/view.php?id=2626 */
      StreamMgr_print_error(streams,  "\n********   WARNING   ********\n");
      StreamMgr_print_error(streams,
              "This version does not support the booleanization of PSL properties.\n"
              "However, for user's convenience all the PSL properties will be dumped\n"
              "as comments in the output file.\n");
      StreamMgr_print_error(streams,  "******** END WARNING ********\n\n");

      fprintf(cwd->out,
              "--- Dumping of PSL properties is not supported by this version of the system.\n"\
              "--- However, the PSL properties had been dumped here for user's convenience,\n"\
              "--- as the occurred in the original model. \n");
      compile_write_flatten_psl(env, cwd, pslspec);
    }
  }

  /* Dumping of the determinization vars,
     and destruction of the temporary layer */
  if (SymbLayer_get_input_vars_num(cwd->det_layer) > 0) {
    fprintf(cwd->out, "-- Determinization variables of specifications:\n");
    compile_write_flatten_bool_vars(env, cwd,
                                    BoolEncClient_get_bool_enc(BOOL_ENC_CLIENT(cwd->enc)),
                                    cwd->det_layer, STT_INPUT_VAR);
  }
}

/*!
  \brief

  Private service of function Compile_convert_to_dag
*/
static node_ptr compile_convert_to_dag_aux(const NuSMVEnv_ptr env,
                                           const CompileWriteData* cwd,
                                           node_ptr expr,
                                           unsigned int num_thres,
                                           unsigned int dep_thres,
                                           const char* defines_prefix)
{
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  node_ptr info;
  node_ptr define;

  if (expr == Nil) return Nil;

  /* We ignore defining sysmbols for leaves */
  switch (node_get_type(expr)) {
    /* leaves */
  case FAILURE:
  case TRUEEXP: case FALSEEXP:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD: case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC: case NUMBER_REAL: case NUMBER_EXP:
  case UWCONST: case SWCONST:
  case ATOM: case BIT: case ARRAY: case SELF: case DOT:
  case NFUNCTION:
    /* [CM] This needs a better management */
    return expr;
  default:
    /* This is intentionally empty */
    break;
  }

  if (cwd->defines != (hash_ptr) NULL) {
    define = find_assoc(cwd->defines, expr);
    if (define != Nil) {
      /* found a previously inserted define that substitutes the expression */
      unsigned int count;
      nusmv_assert(node_get_type(define) == COLON);

      /* Increment hit counter */
      INCREMENT_HITS_NUMBER();

      /* this counter keeps track of really used cwd->defines, for later dumping.
         setcdr can be used here as the node was created with new_node  */
      count = PTR_TO_INT(cdr(define));

      setcdr(define, PTR_FROM_INT(node_ptr, count+1));
      nusmv_assert(node_get_type(car(define)) == EQDEF);
      return car(car(define)); /* returns the name */
    }
  }

  if (cwd->dag_info != (hash_ptr) NULL) {
    nusmv_assert(cwd->defines != (hash_ptr) NULL);

    info = find_assoc(cwd->dag_info, expr);
    if (info != Nil) {
      unsigned int count;
      unsigned int depth;
      boolean admissible;

      /* found a node that might be substituted if needed */
      compile_unpack_dag_info(info, &count, &depth, &admissible);
      if (admissible &&
          (count >= num_thres || (count > 1 && depth >= dep_thres))) {
        /* simplifies to a new dag node: continue on children */
        node_ptr name;
        node_ptr left = compile_convert_to_dag_aux(env, cwd,
                                                   car(expr),
                                                   num_thres, dep_thres,
                                                   defines_prefix);
        node_ptr right = compile_convert_to_dag_aux(env, cwd,
                                                    cdr(expr),
                                                    num_thres, dep_thres,
                                                    defines_prefix);
        node_ptr body = find_node(nodemgr, node_get_type(expr), left, right);

        name = __create_define_name(cwd->symb_table, defines_prefix, body);

        define = new_node(nodemgr, COLON, new_node(nodemgr, EQDEF, name, body),
                          PTR_FROM_INT(node_ptr, 1));
        insert_assoc(cwd->defines, expr, define);

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
      node_ptr left = compile_convert_to_dag_aux(env, cwd, car(expr),
                                                 num_thres, dep_thres,
                                                 defines_prefix);
      node_ptr right = compile_convert_to_dag_aux(env, cwd, cdr(expr),
                                                  num_thres, dep_thres,
                                                  defines_prefix);
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
compile_create_dag_info_from_hierarchy(const CompileWriteData* cwd)
{
  const NuSMVEnv_ptr env =
      EnvObject_get_environment(ENV_OBJECT(cwd->symb_table));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  hash_ptr dag_info = new_assoc();
  node_ptr specs[] = {
    FlatHierarchy_get_spec(cwd->hierarchy),
    FlatHierarchy_get_compute(cwd->hierarchy),
    FlatHierarchy_get_ltlspec(cwd->hierarchy),
    FlatHierarchy_get_invarspec(cwd->hierarchy)
    /* see issue https://essvn.fbk.eu/bugs/view.php?id=2626 */
    /* FlatHierarchy_get_pslspec(hierarchy)*/,
  };
  int i;

  nusmv_assert(dag_info != (hash_ptr) NULL);

  /* extracts info from the FSM */
  Compile_make_dag_info(env, compile_get_rid_of_define_chain(cwd,
      FlatHierarchy_get_init(cwd->hierarchy)), dag_info);
  Compile_make_dag_info(env, compile_get_rid_of_define_chain(cwd,
     FlatHierarchy_get_invar(cwd->hierarchy)), dag_info);
  Compile_make_dag_info(env, compile_get_rid_of_define_chain(cwd,
     FlatHierarchy_get_trans(cwd->hierarchy)), dag_info);
  Compile_make_dag_info(env, compile_get_rid_of_define_chain(cwd,
     FlatHierarchy_get_justice(cwd->hierarchy)), dag_info);
  Compile_make_dag_info(env, compile_get_rid_of_define_chain(cwd,
     FlatHierarchy_get_compassion(cwd->hierarchy)), dag_info);

  { /* learn from ASSIGNs */
    Set_t vars = FlatHierarchy_get_vars(cwd->hierarchy);
    Set_Iterator_t iter;

    SET_FOREACH(vars, iter) {
      node_ptr name = Set_GetMember(vars, iter);
      node_ptr init_name = find_node(nodemgr, SMALLINIT, name, Nil);
      node_ptr next_name = find_node(nodemgr, NEXT, name, Nil);
      Compile_make_dag_info(env, compile_get_rid_of_define_chain(cwd,
         FlatHierarchy_lookup_assign(cwd->hierarchy, name)), dag_info);
      Compile_make_dag_info(env, compile_get_rid_of_define_chain(cwd,
         FlatHierarchy_lookup_assign(cwd->hierarchy, init_name)), dag_info);
      Compile_make_dag_info(env, compile_get_rid_of_define_chain(cwd,
         FlatHierarchy_lookup_assign(cwd->hierarchy, next_name)), dag_info);
    }
  }

  {
    /* Extracts DAG info from the defines */
    SymbTableIter iter;

    SYMB_TABLE_FOREACH(cwd->symb_table, iter, STT_DEFINE) {
      node_ptr define = SymbTable_iter_get_symbol(cwd->symb_table, &iter);
      node_ptr body = SymbTable_get_define_body(cwd->symb_table, define);
      node_ptr ctx = SymbTable_get_define_context(cwd->symb_table, define);
      if (cwd->force_flattening || (Nil != ctx)) {
        body = Compile_FlattenSexp(cwd->symb_table, body, ctx);
      }

      {
        node_ptr b =
            compile_get_rid_of_define_chain(cwd, body);
        if (b != body) body = b;
      }
      Compile_make_dag_info(env, body, dag_info);
    }
  }

  /* extracts info from the specifications */
  for (i=0; i < sizeof(specs)/sizeof(specs[0]); ++i) {
    node_ptr spec;
    if (cwd->det_layer != SYMB_LAYER(NULL)) {
      spec = Compile_expr2bexpr(cwd->enc, cwd->det_layer, specs[i]);
    }
    else {
      node_ptr ctx;

      if ((Nil != specs[i]) && (Nil != car(specs[i])) &&
          (CONTEXT == node_get_type(car(specs[i])))) {
        ctx = caar(specs[i]);
        if (cwd->force_flattening || (Nil != ctx)) {
          spec = Compile_FlattenSexp(cwd->symb_table, specs[i], Nil);
        }
        else {
          spec = specs[i];
        }
      }
      else {
        spec = Nil;
      }
    }
    {
      node_ptr s =
          compile_get_rid_of_define_chain(cwd, spec);
      if (s != spec) spec = s;
    }
    Compile_make_dag_info(env, spec, dag_info);
  }

  return dag_info;
}

/*!
  \brief

  Returns a node COLON(NUMBER count, NUMBER depth)
*/
static node_ptr compile_make_dag_info_aux(NodeMgr_ptr nodemgr,
                                          node_ptr expr, hash_ptr hash)
{
  node_ptr info;

  if (expr == Nil) return compile_pack_dag_info(nodemgr, 1, 0, true);
  info = find_assoc(hash, expr);
  if (info != Nil) {
    unsigned int count;
    unsigned int depth;
    boolean admissible;

    compile_unpack_dag_info(info, &count, &depth, &admissible);
    compile_set_dag_info(info, count+1, depth, admissible);
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
    return compile_pack_dag_info(nodemgr, 1, 0, true);

    /* cases not to be stored */
  case COLON:
  case NEXT:
    {
      unsigned int count = 0;
      unsigned int depth = 0;
      boolean ladmissible, radmissible;

      node_ptr left = compile_make_dag_info_aux(nodemgr, car(expr), hash);
      node_ptr right = compile_make_dag_info_aux(nodemgr, cdr(expr), hash);
      nusmv_assert(left != Nil || right != Nil); /* cannot be a leaf */
      if (left != Nil) {
        compile_unpack_dag_info(left, &count, &depth, &ladmissible);
      }
      if (right != Nil) {
        unsigned int rdepth;
        compile_unpack_dag_info(right, &count, &rdepth, &radmissible);
        depth = MAX(rdepth, depth);
      }

      return compile_pack_dag_info(nodemgr, 1, depth+1, radmissible && ladmissible);
    }

    /* Not admissible cases */
  case OP_NEXT:
  case OP_GLOBAL:
  case OP_FUTURE:
  case OP_PREC:
  case OP_NOTPRECNOT:
  case OP_HISTORICAL:
  case OP_ONCE:
  case EU:
  case AU:
  case EBU:
  case ABU:
  case MINU:
  case MAXU:
  case EX:
  case AX:
  case EF:
  case AF:
  case EG:
  case AG:
  case SINCE:
  case UNTIL:
  case TRIGGERED:
  case RELEASES:
  case EBF:
  case EBG:
  case ABF:
  case ABG:
    {
      unsigned int count = 0;
      unsigned int depth = 0;
      boolean ladmissible, radmissible;

      node_ptr left = compile_make_dag_info_aux(nodemgr, car(expr), hash);
      node_ptr right = compile_make_dag_info_aux(nodemgr, cdr(expr), hash);
      nusmv_assert(left != Nil || right != Nil); /* cannot be a leaf */
      if (left != Nil) {
        compile_unpack_dag_info(left, &count, &depth, &ladmissible);
      }
      if (right != Nil) {
        unsigned int rdepth;
        compile_unpack_dag_info(right, &count, &rdepth, &radmissible);
        depth = MAX(rdepth, depth);
      }

      return compile_pack_dag_info(nodemgr, 1, depth+1, false);
    }


  default:
    {
      unsigned int depth = 0;
      unsigned int count = 0; /* this is dummy use */
      node_ptr left = compile_make_dag_info_aux(nodemgr, car(expr), hash);
      node_ptr right = compile_make_dag_info_aux(nodemgr, cdr(expr), hash);
      boolean ladmissible, radmissible;

      nusmv_assert(left != Nil || right != Nil); /* cannot be a leaf */
      if (left != Nil) {
        compile_unpack_dag_info(left, &count, &depth, &ladmissible);
      }
      if (right != Nil) {
        unsigned int rdepth;
        compile_unpack_dag_info(right, &count, &rdepth, &radmissible);
        depth = MAX(rdepth, depth);
      }

      info = compile_pack_dag_info(nodemgr, 1, depth+1, ladmissible && radmissible);
      insert_assoc(hash, expr, info);
      return info;
    }
  }
}

/*!
  \brief

  Packs given count and depth into a node
*/
static node_ptr compile_pack_dag_info(NodeMgr_ptr nodemgr,
                                      unsigned int count,
                                      unsigned int depth,
                                      boolean admissible)
{
  return new_node(nodemgr, COLON,
                  new_node(nodemgr, COLON,
                           PTR_FROM_INT(node_ptr, count),
                           PTR_FROM_INT(node_ptr, depth)),
                  PTR_FROM_INT(node_ptr, admissible));
}

/*!
  \brief

  Unpacks given node to count and deptch
*/
static void compile_unpack_dag_info(node_ptr info,
                                    unsigned int* count,
                                    unsigned int* depth,
                                    boolean* admissible)
{
  nusmv_assert(node_get_type(info) == COLON);
  *count = PTR_TO_INT(caar(info));
  *depth = PTR_TO_INT(cdar(info));
  *admissible = (boolean) cdr(info);
}

/*!
  \brief

  Sets count and depth
*/
static void compile_set_dag_info(node_ptr info,
                                 unsigned int count,
                                 unsigned int depth,
                                 boolean admissible)
{
  nusmv_assert(node_get_type(info) == COLON);
  /* setcar and setcdr are admitted here as info was created with new_node,
     and there exist no possibility to gamble the node hash */
  setcar(car(info), PTR_FROM_INT(node_ptr, count));
  setcdr(car(info), PTR_FROM_INT(node_ptr, depth));
  setcdr(info, PTR_FROM_INT(node_ptr, admissible));
}

/*!
  \brief Internal service of Compile_destroy_dag_info


*/
static assoc_retval compile_free_node(char *key, char *data, char * arg)
{
  NodeMgr_ptr nodemgr = NODE_MGR(arg);

  if (Nil != car((node_ptr)data)) free_node(nodemgr, car((node_ptr) data));
  if (data != (char*) NULL) free_node(nodemgr, (node_ptr) data);
  return ASSOC_DELETE;
}

/*!
  \brief Internal service of Compile_destroy_dag_info


*/
static assoc_retval compile_free_define(char *key, char *data, char * arg)
{
  NodeMgr_ptr nodemgr = NODE_MGR(arg);

  if (data != (char*) NULL) {
    free_node(nodemgr, car((node_ptr) data));
    free_node(nodemgr, (node_ptr) data);
  }
  return ASSOC_DELETE;
}

/*!
  \brief Prints the obfuscation of the given type


*/
static void compile_symbtype_obfuscated_print(SymbType_ptr type,
                                              const CompileWriteData* cwd)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(cwd->symb_table));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  if (SymbType_is_enum(type) && (!SymbType_is_boolean(type))) {
    node_ptr l = SymbType_get_enum_type_values(type);
    fprintf(cwd->out, "{");
    while (l != Nil) {
      node_ptr l_anonymous = NodeAnonymizerBase_map_expr(cwd->anonymizer, car(l));
      print_node(wffprint, cwd->out, l_anonymous);
      l = cdr(l);
      if (l != Nil) { fprintf(cwd->out, ", "); }
    }
    fprintf(cwd->out, "}");
  }
  else if (SymbType_is_array(type)) {
    /* print the array and proceed to the subtype */
      fprintf(cwd->out, "array %d..%d of ",
              SymbType_get_array_lower_bound(type),
              SymbType_get_array_upper_bound(type));
      compile_symbtype_obfuscated_print(SymbType_get_array_subtype(type), cwd);
  }
  else {
    SymbType_print(type, wffprint, cwd->out);
  }
}

/*!
  \brief Get rids of chain of defines

  Get rids of chain of defines until it reaches a
   DEFINE whose body is not atomic (i.e. a variable, a constant, or a
   complex expression). It assumes the expression being flattened.
*/
static node_ptr compile_get_rid_of_define_chain(const CompileWriteData* cwd,
                                                node_ptr expr)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(cwd->symb_table));
  node_ptr res;

  if (Nil == expr) return expr;

  res = find_assoc(cwd->cdh, expr);

  if (Nil != res) return res;

  switch(node_get_type(expr)) {
  case TRUEEXP:
  case FALSEEXP:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case FAILURE:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
    return expr;

  case ATOM:
  case DOT:
  case ARRAY:
  case BIT:
  case NFUNCTION:
    {
      ResolveSymbol_ptr rs;
      node_ptr name;

      rs = SymbTable_resolve_symbol(cwd->symb_table, expr, Nil);
      name = ResolveSymbol_get_resolved_name(rs);

      if (ResolveSymbol_is_constant(rs)) { return name; }
      if (ResolveSymbol_is_var(rs)) { return name; }
      if (ResolveSymbol_is_function(rs)) { return name; }
      if (ResolveSymbol_is_define(rs)) {
        res = CompileFlatten_resolve_define_chains(cwd->symb_table, name, Nil);
        insert_assoc(cwd->cdh, expr, res);
        return res;
      }
      insert_assoc(cwd->cdh, expr, expr);
      return expr;
    }
  default:
    {
      const NodeMgr_ptr nodemgr =
        NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

      res = find_node(nodemgr, node_get_type(expr),
                      compile_get_rid_of_define_chain(cwd, car(expr)),
                      compile_get_rid_of_define_chain(cwd, cdr(expr)));
      insert_assoc(cwd->cdh, expr, res);
      return res;
    }
  }
}

/*!
  \brief


*/
static void compile_write_obfuscated_dag_defines(const NuSMVEnv_ptr env,
                                                 const CompileWriteData* cwd)
{
  const OptsHandler_ptr opts =
    OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  char *key, *value;
  st_generator *gen;
  boolean msg_printed = false;

  st_foreach_item(cwd->defines, gen, &key, &value) {
    node_ptr define = (node_ptr) value;
    nusmv_assert(define == Nil || node_get_type(define) == COLON);
    if (define != Nil) {
      unsigned int count = PTR_TO_INT(cdr(define));
      if (count > 0) {
        if (!msg_printed) {
          fprintf(cwd->out, "-- Symbols introduced by the dumper:\n");
          msg_printed = true;
        }
        if (opt_verbose_level_gt(opts, 0)) {
          Logger_ptr logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
          Logger_log(logger, "-- occurrences: %d\n", count+1);
        }
        fprintf(cwd->out, "DEFINE ");
        print_node(wffprint, cwd->out, NodeAnonymizerBase_map_expr( cwd->anonymizer,
                                                               car(define)));
        fprintf(cwd->out, " ;\n\n");
      }
    }
  }
}


/*!
  \brief Creates a meaningful name for defines needed for dag printing
*/
node_ptr __create_define_name(SymbTable_ptr st,
                              const char * prefix, node_ptr body)
{
  return SymbTable_get_fresh_symbol_name(st, "__expr");
}


/*!
  \brief Init CompileWriteData structure


*/
static void
compile_write_data_init(CompileWriteData * cwd)
{
  cwd->symb_table = NULL;
  cwd->out =  NULL;
  cwd->force_flattening = false;
  cwd->fsm_name = false;
  cwd->det_layer =  SYMB_LAYER(NULL);
  cwd->layer_names  = NULL;
  cwd->hierarchy  = NULL;;
  cwd->enc  = BDD_ENC(NULL);
  cwd->anonymizer = NULL;;
  cwd->bool_sexp_fsm = NULL;
  cwd->nodelist_layer  = NULL;
  cwd->dag_info = (hash_ptr) NULL;
  cwd->defines = (hash_ptr) NULL;
  cwd->cdh = (hash_ptr) NULL;
}

/*!
  \brief Deinit CompileWriteData structure


*/
static void
compile_write_data_deinit(CompileWriteData * cwd)
{

}


/* visits the list of dag defines */
static void compile_visit_dag_defines(
    hash_ptr defines,
    boolean (*cb)(node_ptr define_eqdef,
                  unsigned int count,
                  void* arg),
    void* arg)
{
  char *key, *value;
  st_generator *gen;

  st_foreach_item(defines, gen, &key, &value) {
    node_ptr define = (node_ptr) value;
    nusmv_assert(define == Nil || node_get_type(define) == COLON);

    if (define != Nil) {
      node_ptr eqdef;
      unsigned int count;
      boolean keep_visit;

      eqdef = car(define);
      nusmv_assert(EQDEF == node_get_type(eqdef));
      count = PTR_TO_INT(cdr(define));

      keep_visit = cb(eqdef, count, arg);
      if (!keep_visit)
        break;
    }
  }
}
