/* ---------------------------------------------------------------------------


  This file is part of the ``parser.psl'' package of NuSMV version 2.
  Copyright (C) 2005 by FBK-irst.

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
  \brief Psl package private interface

  \todo: Missing description

*/


#ifndef __NUSMV_CORE_PARSER_PSL_PSL_INT_H__
#define __NUSMV_CORE_PARSER_PSL_PSL_INT_H__

#include "nusmv/core/parser/psl/pslNode.h"
#include "nusmv/core/parser/parserInt.h" /* for YY_BUFFER_STATE */
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/utils/error.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief This integer value represents the count of a count-free
  starred repeated sere. For example {a}\[*\] that has no count.

  
*/
#define PSL_EMPTYSTAR PSL_NULL


/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
int psl_yylex(void);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
int psl_yyparse(void);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
void psl_yyrestart(FILE* input_file);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
YY_BUFFER_STATE psl_yy_scan_buffer(char *base, size_t size);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
YY_BUFFER_STATE psl_yy_create_buffer(FILE *file, int size);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
void psl_yy_switch_to_buffer(YY_BUFFER_STATE new_buffer);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
void psl_yy_delete_buffer(YY_BUFFER_STATE b);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
YY_BUFFER_STATE psl_yy_scan_string(const char *yy_str);

/*!
  \brief Creates a new PSL node, re-using already an existing
node if there is one

  WARNING: If this function is being called to build a
 branch of the parse-tree from a branch coming from the parsing phase
 (i.e. it is a token, and not a symbol), the token *must* be converted
 to a PSL node by calling psl_conv_op(TOK2PSL, op) 

  \se None

  \sa optional
*/
PslNode_ptr
psl_new_node(NodeMgr_ptr nodemgr,
             PslOp _type, PslNode_ptr left, PslNode_ptr right);

/*!
  \brief Sets the given expression's left branch

  

  \se None
*/
void psl_node_set_left(PslNode_ptr n, PslNode_ptr l);

/*!
  \brief Sets the given expression's right branch

  

  \se None
*/
void psl_node_set_right(PslNode_ptr n, PslNode_ptr r);

/*!
  \brief Maker for a NUMBER node

  

  \se None
*/
PslNode_ptr psl_node_make_number(NodeMgr_ptr nodemgr,
                                        int value);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
PslOp psl_conv_op(const NuSMVEnv_ptr env,
                         PslOpConvType type, PslOp op);

/*!
  \brief Returns true if the given expression is a SERE

  A SERE can be in the form {a}, {a};{b}, {a}:{b},
{a}\[*\], {a}\[+\], {a}|{b}, {a}&{a}, {a}&&{b} 

  \se None
*/
boolean psl_node_is_sere(PslNode_ptr expr);

/*!
  \brief Returns true if the given starred sere can be handled by the
system. 

  precond: expr must be a repeated sere

  \se None
*/
boolean
psl_node_is_handled_star(const NuSMVEnv_ptr env,
                         PslNode_ptr expr, boolean toplevel);

/*!
  \brief Returns the count of a starred sere.

  Returned value can be either a positive integer value, or
the constant PSL_EMPTYSTAR to represent an empty starred sere.

  \se None
*/
PslNode_ptr
psl_node_sere_star_get_count(const PslNode_ptr e);

/*!
  \brief Returns true if the given sere contains a single
propositional expression

  

  \se None
*/
boolean psl_node_sere_is_propositional(PslNode_ptr e);

/*!
  \brief Returns true if the given expr is a repeated sere

  

  \se None
*/
boolean psl_node_sere_is_repeated(PslNode_ptr e);

/*!
  \brief Returns true if the given expr is a starred repeated sere

  

  \se None
*/
boolean psl_node_sere_is_star(PslNode_ptr e);

/*!
  \brief Returns true if the given expr is in the form <empty>\[*\],
with or without a counter.

  

  \se None
*/
boolean psl_node_sere_is_standalone_star(PslNode_ptr e);

/*!
  \brief Returns true if the given expression a plus repeated sere

  

  \se None
*/
boolean psl_node_sere_is_plus(PslNode_ptr e);

/*!
  \brief Returns true if the given repeated sere is in the form
<empty>\[+\]

  

  \se None
*/
boolean psl_node_sere_is_standalone_plus(PslNode_ptr e);

/*!
  \brief Returns true if the given starred repeated sere as also
a counter

  

  \se None
*/
boolean psl_node_sere_is_star_count(PslNode_ptr e);

/*!
  \brief Returns the repeated expression associated to the repeated
sere

  

  \se None
*/
PslNode_ptr psl_node_sere_repeated_get_expr(PslNode_ptr e);

/*!
  \brief Returns true if the given expr is a starred-eq repeated sere

  

  \se None
*/
boolean psl_node_sere_is_stareq(PslNode_ptr e);

/*!
  \brief Returns true if the given expr is a starred-minusgt repeated sere

  

  \se None
*/
boolean psl_node_sere_is_starminusgt(PslNode_ptr e);

/*!
  \brief Maker for the sere compound

  Warning: the operator must be a symbol, not a token.
This means that psl_conv_op must be called to convert tokens before.

  \se None
*/
PslNode_ptr
psl_node_make_sere_compound(NodeMgr_ptr nodemgr,
                            PslNode_ptr seq1,
                            PslOp op, PslNode_ptr seq2);

/*!
  \brief Returns true if the given expression is a sere compound

  

  \se None
*/
boolean psl_node_is_sere_compound_binary(PslNode_ptr e);

/*!
  \brief Returns true if the given expression is a suffix
implication

  

  \se None
*/
boolean psl_node_is_suffix_implication(PslNode_ptr expr);

/*!
  \brief Returns true if the given expression is a weak suffix
implication

  

  \se None
*/
boolean psl_node_is_suffix_implication_weak(PslNode_ptr expr);

/*!
  \brief Returns true if the given expression is a strong suffix
implication

  

  \se None
*/
boolean psl_node_is_suffix_implication_strong(PslNode_ptr expr);

/*!
  \brief Returns the premise of the given suffix implication

  

  \se None
*/
PslNode_ptr
psl_node_suffix_implication_get_premise(PslNode_ptr e);

/*!
  \brief Returns the consequence of the given suffix implication

  

  \se None
*/
PslNode_ptr
psl_node_suffix_implication_get_consequence(PslNode_ptr e);

/*!
  \brief Returns true if there are no holes in the given concat sere
to be filled in.

  

  \se None
*/
boolean psl_node_sere_is_concat_holes_free(PslNode_ptr e);

/*!
  \brief Returns true if the given expression is a concat or fusion
sere.

  

  \se None
*/
boolean psl_node_sere_is_concat_fusion(PslNode_ptr e);

/*!
  \brief [Returns true if there are no holes in the given
fusion/concat sere to be filled in.]

Description        []

SideEffects        [None]

SeeAlso            []

*****************************************************************************[EXTRACT_DOC_NOTE: * /]


  

  \se None
*/
boolean psl_node_sere_is_concat_fusion_holes_free(PslNode_ptr e);

/*!
  \brief Prunes aways the given branch from the given tree

  

  \se None
*/
PslNode_ptr psl_node_prune(NodeMgr_ptr nodemgr,
                                  PslNode_ptr tree, PslNode_ptr branch);

/*!
  \brief Returns true if the given expression is a propositional
starred sere.

  

  \se None
*/
boolean psl_node_is_propstar(PslNode_ptr e);

/*!
  \brief Returns true if the given expression is a sere in the form
{ s2 && s1 } 

  

  \se None
*/
boolean psl_node_sere_is_2ampersand(PslNode_ptr e);

/*!
  \brief Maker for a list

  This gets the element to insert at top level, and
the list for next

  \se None
*/
PslNode_ptr
psl_node_make_cons(NodeMgr_ptr nodemgr,
                   PslNode_ptr elem, PslNode_ptr next);

/*!
  \brief Maker for a list, does not use find_node

  This gets the element to insert at top level, and
the list for next

  \se None
*/
PslNode_ptr
psl_node_make_cons_new(NodeMgr_ptr nodemgr,
                       PslNode_ptr elem, PslNode_ptr next);

/*!
  \brief Returns true if the given node is a list

  

  \se None
*/
boolean psl_node_is_cons(PslNode_ptr e);

/*!
  \brief Reverse a list.

  Returns a new sequence containing the same
  elements as 'e' but in reverse order

  \se None
*/
PslNode_ptr psl_node_cons_reverse(PslNode_ptr e);

/*!
  \brief Returns true if the given expression is If Then Else

  

  \se None
*/
boolean psl_node_is_ite(PslNode_ptr _ite);

/*!
  \brief Returns the condition of the given ITE node

  

  \se None
*/
PslNode_ptr psl_node_get_ite_cond(PslNode_ptr _ite);

/*!
  \brief Returns the 'then' branch of the given ITE node

  

  \se None
*/
PslNode_ptr psl_node_get_ite_then(PslNode_ptr _ite);

/*!
  \brief Returns the 'else' branch of the given ITE node

  

  \se None
*/
PslNode_ptr psl_node_get_ite_else(PslNode_ptr _ite);

/*!
  \brief Returns true if the given expression is a case expression

  

  \se None
*/
boolean psl_node_is_case(PslNode_ptr _case);

/*!
  \brief Returns the condition of the given case node

  

  \se None
*/
PslNode_ptr psl_node_get_case_cond(PslNode_ptr _case);

/*!
  \brief Returns the 'then' branch of the given case node

  

  \se None
*/
PslNode_ptr psl_node_get_case_then(PslNode_ptr _case);

/*!
  \brief Returns the next case node of the given case.

  

  \se None
*/
PslNode_ptr psl_node_get_case_next(PslNode_ptr _case);

/*!
  \brief Returns true if the given numbers are equal

  

  \se None
*/
boolean
psl_node_is_num_equal(PslNode_ptr _id1, PslNode_ptr _id2);

/*!
  \brief Returns true if the given node is the PSL syntactic type
'boolean'

  

  \se None
*/
boolean psl_node_is_boolean_type(PslNode_ptr expr);

/*!
  \brief Returns true if the given node is an identifier

  The top level operator of an ID can be DOT,
ATOM or ARRAY

  \se None
*/
boolean psl_node_is_id(PslNode_ptr expr);

/*!
  \brief Returns true if two ids are equal

  

  \se None
*/
boolean psl_node_is_id_equal(PslNode_ptr _id1, PslNode_ptr _id2);

/*!
  \brief Returns true if the given node is a leaf, i.e. PSL_NULL, a
   number, a boolean constant, or an atom.

  

  \se None
*/
boolean psl_node_is_leaf(PslNode_ptr expr);

/*!
  \brief Returns true if the given expression is a replicated
property

  

  \se None
*/
boolean psl_node_is_repl_prop(PslNode_ptr _prop);

/*!
  \brief Given a replicated property, returns the node that contains
the replicator.

  

  \se None

  \sa psl_node_repl_prop_get_property
*/
PslNode_ptr psl_node_repl_prop_get_replicator(PslNode_ptr _prop);

/*!
  \brief Given a replicated property, returns the node that contains
the property.

  

  \se None

  \sa psl_node_repl_prop_get_replicator
*/
PslNode_ptr psl_node_repl_prop_get_property(PslNode_ptr _prop);

/*!
  \brief Returns true if the given expression represents a
replicator.

  

  \se None
*/
boolean psl_node_is_replicator(PslNode_ptr _repl);

/*!
  \brief Maker for a NEXT* family node

  Warning: the operator must be a symbol, not a token.
This means that psl_conv_op must be called to convert tokens before.

  \se None
*/
PslNode_ptr
psl_node_make_extended_next(NodeMgr_ptr nodemgr,
                            PslOp op, PslNode_ptr expr,
                            PslNode_ptr when,
                            PslNode_ptr condition);

/*!
  \brief Given a psl node returns true iff the expression belongs to
the next operators family.

  

  \se None
*/
boolean psl_node_is_extended_next(PslNode_ptr e);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
void psl_yyerror(char* s, ...);

#endif /* __NUSMV_CORE_PARSER_PSL_PSL_INT_H__ */
