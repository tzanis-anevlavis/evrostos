/* ---------------------------------------------------------------------------

  This file is part of the ``compile.flattening'' package of NuSMV version 2.
  Copyright (C) 2013 by FBK-irst.

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
  \author Sergio Mover
  \brief Implementation of class 'FlattenerCore'

  Implementation of the core flattener.

*/


#include "nusmv/core/compile/flattening/FlattenerCore.h"
#include "nusmv/core/compile/flattening/FlattenerCore_private.h"

#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/compile/compileInt.h"

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/utils/Logger.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/range.h"
#include "nusmv/core/utils/WordNumberMgr.h"

#include "nusmv/core/node/NodeMgr.h"

#include "nusmv/core/utils/WordNumberMgr.h"



/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
/* See 'FlattenerCore_private.h' for class 'FlattenerCore' definition. */

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void flattener_core_finalize(Object_ptr object, void* dummy);

static node_ptr
flattener_core_construct_array_multiplexer(
    FlattenerBase_ptr self,
    node_ptr array, node_ptr index,
    boolean is_array_next,
    SymbTable_ptr symb_table,
    hash_ptr def_hash,
    const MasterCompileFlattener_def_mode mode);

static node_ptr
flattener_core_push_array_index_down(
    FlattenerBase_ptr self,
    node_ptr array, node_ptr index,
    boolean is_array_next,
    SymbTable_ptr st,
    hash_ptr def_hash,
    const MasterCompileFlattener_def_mode mode);

static SymbType_ptr
_compute_constarray_type(SymbTable_ptr symb_table, node_ptr array_type);

static node_ptr
_resolve_name_or_nop(SymbTable_ptr, node_ptr, node_ptr);

static node_ptr
_flatten_resolve_variable_array_eqdef(
    FlattenerBase_ptr, SymbTable_ptr,
    hash_ptr, node_ptr, int, node_ptr,
    node_ptr, MasterCompileFlattener_def_mode);

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

FlattenerCore_ptr FlattenerCore_create(const NuSMVEnv_ptr env,
                                       const char* name)
{
  FlattenerCore_ptr self = ALLOC(FlattenerCore, 1);
  FLATTENER_CORE_CHECK_INSTANCE(self);

  flattener_core_init(self, env, name,
                      NUSMV_CORE_SYMBOL_FIRST,
                      NUSMV_CORE_SYMBOL_LAST - NUSMV_CORE_SYMBOL_FIRST);
  return self;
}

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

void flattener_core_init(FlattenerCore_ptr self, const NuSMVEnv_ptr env,
                         const char* name, int low, size_t num)

{
  /* base class initialization */
  flattener_base_init(FLATTENER_BASE(self), env, name, low,
                      num, true /*handles NULL*/);


  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = flattener_core_finalize;
  OVERRIDE(FlattenerBase, flatten) = flattener_core_flatten;
}

void flattener_core_deinit(FlattenerCore_ptr self)
{
  /* members deinitialization */


  /* base class deinitialization */
  flattener_base_deinit(FLATTENER_BASE(self));
}


/*!
   DOCUMENTATION ABOUT ARRAY:

      In NuSMV ARRAY has 2 meanings, it can be a part of identifier
   (which we call identifier-with-brackets) or a part of
   expression. For example, VAR v[5] : boolean; declares a new
   identifier-with-brackets v[5] where [5] is a part of
   identifier. Thus v[d], where d is a define equal to 5, is not a
   valid identifier as well as v[4+1] or v, whereas v[5] is valid.

   For "VAR v : array 1..5 of boolean;" v[5] is identifier (array
   elements are declared in symbol table) v[d] is not,
   but both are valid expressions.

   This difference is important for code, e.g.
     DEFINE d := v;
     INVARSPEC d[5];
   If v[5] is declared as individual identifier this code is invalid
   because v is not declared whereas if v is an array the code becomes
   valid.

   Flattener additionally makes every ARRAY-expression normalized.
   For example, d[i+1] is changed to
   case i+1=0 : v[0]; i+1=1 : v[1]; ... FAILURE; esac.
   Such a way every v[N] become a legal identifier wrt symbol table.
   Note that above normalization is done independent if defines are set
   to be expanded or not.

   NOTE: arrays of modules are not supported. Thus ARRAY before DOT
   can be legal only through identifier-with-brackets declaration,
   e.g.  for v[3].b to be valid it is necessary to declare v[3] as
   module instance.

   NOTE FOR DEVELOPERS !!!
   Currently this function applies find_atom to the constants and IDs
   and new_node to operations nodes. If this approach changes
   then internal function ltl_rewrite_input has to be changed as well.
*/
node_ptr flattener_core_flatten(FlattenerBase_ptr self,
                                SymbTable_ptr symb_table,
                                hash_ptr def_hash,
                                node_ptr sexp,
                                node_ptr context,
                                MasterCompileFlattener_def_mode mode)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  node_ptr result = Nil;
  extern int nusmv_yylineno;
  int temp = nusmv_yylineno;

  if (sexp == Nil) return sexp;

  nusmv_yylineno = node_get_lineno(sexp);

  switch (node_get_type(sexp)) {
    /* base cases for which no flattening necessary */
  case FAILURE:
  case TRUEEXP:
  case FALSEEXP:
  case NUMBER:
  case NUMBER_UNSIGNED_WORD:
  case NUMBER_SIGNED_WORD:
  case NUMBER_FRAC:
  case NUMBER_REAL:
  case NUMBER_EXP:
    result = find_atom(nodemgr, sexp);
    break;

  case SWCONST:
  case UWCONST: /* word constants are removed from the expressions,
                   as they are handled only at flattening time */
    {
      const WordNumberMgr_ptr words =
        WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));


      node_ptr value = _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr width = _THROW(cdr(sexp), symb_table, def_hash, context, mode);

      int width_int;
      int value_int;
      WordNumber_ptr value_word;

      WordNumberValue tmp;
      int type = node_get_type(sexp);

      value = CompileFlatten_resolve_number(symb_table, value, context);
      width = CompileFlatten_resolve_number(symb_table, width, context);

      /*  --- process the width: it can be an integer or word number
          in range [0,max-allowed-width] */
      if ((node_ptr) NULL == width) {
        ErrorMgr_rpterr(errmgr,
                        "width specifier of swconst/uwconst operator is "
                        "not a constant");
      }

      switch (node_get_type(width)) {
      case NUMBER:
        width_int = node_get_int(width);
        break;

      case NUMBER_UNSIGNED_WORD:
        tmp = WordNumber_get_unsigned_value(WORD_NUMBER(car(width)));
        width_int = tmp;
        if (tmp != width_int) {
          ErrorMgr_rpterr(errmgr,
                          "width specifier of swconst/uwconst operator is "
                          "not representable as int");
        }
        break;

      case NUMBER_SIGNED_WORD:
        tmp = WordNumber_get_signed_value(WORD_NUMBER(car(width)));
        width_int = tmp;
        if (tmp != width_int) {
          ErrorMgr_rpterr(errmgr,
                          "width specifier of swconst/uwconst operator is "
                          "not representable as int");
        }
        break;

      default: /* error */
        ErrorMgr_rpterr(errmgr,
                        "width specifier of swconst/uwconst operator is "
                        "not an integer or word constant");
      }

      if (width_int <= 0 || width_int > WordNumber_max_width()) {
        ErrorMgr_rpterr(errmgr, "width specifier is out of range [0, %i]",
               WordNumber_max_width());
      }

      /*  --- process the value: it can be only integer and
          has to be representable with given width */
      if ((node_ptr) NULL == value  || NUMBER != node_get_type(value)) {
        ErrorMgr_rpterr(errmgr,
                        "value specifier of swconst/uwconst operator is not "
                        "an integer constant");
      }

      value_int = node_get_int(value);

      /* two shifts are done because shift by the full width isn't allowed in
         C. If value is positive, an extra bit of width is needed to avoid
         overflow. */
      if ((value_int > 0 &&
           ((UWCONST == type && value_int >> (width_int-1) >> 1 != 0) ||
            (SWCONST == type && value_int >> (width_int-2) >> 1 != 0))) ||
          (value_int < 0 && value_int >> (width_int-1) != -1)) {
        ErrorMgr_rpterr(errmgr,
                        "value specifier of swconst/uwconst operator is not "
                        "representable with provided width");
      }

      value_word = (value_int >= 0)
        ? WordNumberMgr_integer_to_word_number(
            words, (WordNumberValue)value_int, width_int)
        : WordNumberMgr_signed_integer_to_word_number(
              words, (WordNumberValue)value_int, width_int);

      nusmv_assert(WORD_NUMBER(NULL) != value_word);

      result = find_node(nodemgr,
                         (UWCONST == type)?
                         NUMBER_UNSIGNED_WORD :
                         NUMBER_SIGNED_WORD,
                         NODE_PTR(value_word), Nil);
      break;
    }

  case ATTIME:
    {
      node_ptr left  = _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr right = _THROW(cdr(sexp), symb_table, def_hash, context, mode);
      result = find_node(nodemgr, ATTIME, left, right);
      break;
    }

  case BIT:
    result = sexp;
    break;

  case ATOM:
    {
      ResolveSymbol_ptr rs;
      node_ptr name;

      rs = SymbTable_resolve_symbol(symb_table, sexp, context);

      name = ResolveSymbol_get_resolved_name(rs);

      if (ResolveSymbol_is_error(rs)) ResolveSymbol_throw_error(rs, env);

      if (ResolveSymbol_is_constant(rs)) {
        result = name;
        break;
      }

      if (ResolveSymbol_is_parameter(rs)) {
        node_ptr param;
        param = SymbTable_get_flatten_actual_parameter(symb_table, name);
        /* The result of the flattening is then flattening of parameters */
        result = _THROW(param, symb_table, def_hash, context, mode);
        break;
      }

      /* It can be a defined symbol, a running condition or a variable */
      result =
        MasterCompileFlattener_get_definition(
            MASTER_COMPILE_FLATTENER((NODE_WALKER(self))->master),
            symb_table,
            name, mode);

      break;
    }

  case SELF:
    {
      const StreamMgr_ptr streams =
        STREAM_MGR(NuSMVEnv_get_value(env, ENV_STREAM_MANAGER));

      StreamMgr_print_error(streams,
                            "flattener_core_flatten: invalid usage "
                            "of identifier \"self\"\n");

      ErrorMgr_nusmv_exit(errmgr, 1);
    }
    break;

  case DOT:
    {
      ResolveSymbol_ptr rs;
      node_ptr name;

      rs = SymbTable_resolve_symbol(symb_table, sexp, context);
      name = ResolveSymbol_get_resolved_name(rs);

      if (ResolveSymbol_is_error(rs)) ResolveSymbol_throw_error(rs, env);

      if (ResolveSymbol_is_parameter(rs)) {
        node_ptr param;
        param = SymbTable_get_flatten_actual_parameter(symb_table, name);
        /* The result of the flattening is then flattening of parameters */
        result = _THROW(param, symb_table, def_hash, context, mode);
        break;
      }

      result =
        MasterCompileFlattener_get_definition(
            MASTER_COMPILE_FLATTENER((NODE_WALKER(self))->master),
            symb_table,
            name, mode);
      break;
    }

  case ARRAY:
    {
      boolean is_next;
      node_ptr array, index, tmp;
      ResolveSymbol_ptr rs;
      node_ptr name;

      rs = SymbTable_resolve_symbol(symb_table, sexp, context);
      name = ResolveSymbol_get_resolved_name(rs);

      /* ! See description of flattener_core_flatten for
         information about how ARRAY are implemented !
      */

      /* at first resolve the name and it is already declared
         then return the results */
      if (Nil != name &&
          ResolveSymbol_is_defined(rs)) {
        result =
          MasterCompileFlattener_get_definition(
              MASTER_COMPILE_FLATTENER((NODE_WALKER(self))->master),
              symb_table,
              name, mode);
        break;
      }

      /* this is array-expression, not identifier-with-brackets */
      array = car(sexp);
      index = cdr(sexp);
      is_next = false;

      if (NEXT == node_get_type(array)) { /* exp of form "next(m)[i]" */
        is_next = true;
        array = car(array);
      }

      /* Resolve the array LHS which can be arbitrary expression */
      array = _THROW(array, symb_table, def_hash, context, mode);


      /* get rid of defines */
      while (SymbTable_is_symbol_define(symb_table, array)) {
        node_ptr body = SymbTable_get_define_body(symb_table, array);
        node_ptr ctx = SymbTable_get_define_context(symb_table, array);
        /* expression is arbitrary => flatten it by standard procedure */
        array = _THROW(body, symb_table, def_hash, ctx, mode);
      }

      /* after flattening a new NEXT may appear (e.g. from defines) */
      if (NEXT == node_get_type(array)) {
        nusmv_assert(!is_next); /*double next*/
        is_next = true;
        array = car(array);
      }

      /* index is arbitrary exp => flatten it by standard procedure
         and then try to get constant value if it is possible */
      index = _THROW(index, symb_table, def_hash, context, mode);

      tmp = CompileFlatten_resolve_number(symb_table, index, context);
      if (tmp != Nil) index = tmp; /* The current index is constant */

      if (Nil == index) {
        /* The index has to be solved */
        error_unreachable_code();
      }

      /* convert word constant to integer constant */
      if (NUMBER_SIGNED_WORD == node_get_type(index) ||
          NUMBER_UNSIGNED_WORD == node_get_type(index)) {
        WordNumberValue val
          = WordNumber_get_unsigned_value((WordNumber_ptr) car(index));
        index = find_node(nodemgr, NUMBER, NODE_FROM_INT((int)val), Nil);

        /* if below assertion is violated it means that int is not enough
           to represent indexes of arrays. Representation of nodes
           have to be changed then (as NUMBER keep numbers in int). */
        nusmv_assert(((int)val) == val);
      }

      if (IFTHENELSE == node_get_type(array) ||
          CASE == node_get_type(array)) {
        /* If left child of ARRAY is if-then-else then push index-access
           down to leaves of if-then-else and resolve identifiers there.
           Note that both array and index are already resolved thus
           context is not required.
        */
        result = flattener_core_push_array_index_down(self, array, index,
                                                      is_next, symb_table,
                                                      def_hash,
                                                      mode);
        break;
      }

      /* if not if-then-else then only identifier is possible on LHS.
         Actually, since type checking is not done yet, also
         invalid expression is possible */

      if (NUMBER != node_get_type(index)) {
        /* Index is an (dynamic) expression => convert
           the index-access to if-then-else, e.g.
           a[i] is converted to
           case i = 0 : a[0];
                i = 1 : a[1];
                ...
                i = N : a[N];
                failure;
           esac;
        */
        result = flattener_core_construct_array_multiplexer(
            self,
            array, index,
            is_next, symb_table,
            def_hash, mode);

        break;
      }

      /* we have a constant index => check that it is in the range */
      {
        SymbType_ptr type = TypeChecker_get_expression_type(
                                       SymbTable_get_type_checker(symb_table),
                                       array, Nil);
        int lower_bound, upper_bound, val;

        if (!SymbType_is_array(type)) {
          ErrorMgr_error_lhs_of_index_is_not_array(errmgr);
        }
        lower_bound = SymbType_get_array_lower_bound(type);
        upper_bound = SymbType_get_array_upper_bound(type);
        val = node_get_int(index);
        if (val < lower_bound || val > upper_bound) {
          ErrorMgr_error_array_out_of_bounds(
              errmgr, val, lower_bound, upper_bound);
        }
      }

      /* index is a constant => create the array expression and
         expand if required.*/
      array = find_node(nodemgr, ARRAY, array, index);
      if (Flattener_Expand_Def_Mode == mode) {
        array =
          MasterCompileFlattener_get_definition(
              MASTER_COMPILE_FLATTENER((NODE_WALKER(self))->master),
              symb_table,
              array, mode);
      }
      /* add the final next if required */
      if (is_next) array = find_node(nodemgr, NEXT, array, Nil);
      result = array;
      break;
    }

  case SPEC:
  case INVARSPEC:
  case LTLSPEC:
  case PSLSPEC:
  case COMPUTE:
    {
      node_ptr ctx = car(sexp);
      result = _THROW(cdr(ctx), symb_table, def_hash, car(ctx), mode);
    }
    break;

  case CONTEXT:
    /* (CONTEXT (cxt . expr)) */
    result = _THROW(cdr(sexp), symb_table, def_hash, car(sexp), mode);
    break;

  case NEXT:
    {
      node_ptr body = _THROW(car(sexp), symb_table, def_hash, context, mode);
      result = new_node(nodemgr, node_get_type(sexp), body, Nil);
      break;
    }

    /* Unary operators */
  case NOT:
  case CAST_WORD1:
  case CAST_SIGNED:
  case CAST_UNSIGNED:
  case UMINUS:
    {
      node_ptr body = _THROW(car(sexp), symb_table, def_hash, context, mode);
      result = new_node(nodemgr, node_get_type(sexp), body, Nil);
      break;
    }

  case FLOOR:
    {
      node_ptr body;
      SymbType_ptr type;

      body = _THROW(car(sexp), symb_table, def_hash, context, mode);
      type =
        TypeChecker_get_expression_type(SymbTable_get_type_checker(symb_table),
                                        body, Nil);
      if (SymbType_is_integer(type)) {
        result = body;
      }
      else result = new_node(nodemgr, node_get_type(sexp), body, Nil);
      break;
    }

  case CAST_BOOL:
    {
      const NuSMVEnv_ptr env =
          EnvObject_get_environment(ENV_OBJECT(symb_table));
      const WordNumberMgr_ptr words =
        WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));
      const ExprMgr_ptr exprs =
          EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

      node_ptr body = _THROW(car(sexp), symb_table, def_hash, context, mode);
      SymbType_ptr type =
        TypeChecker_get_expression_type(
            SymbTable_get_type_checker(symb_table), body, Nil);

      /* No cast needed */
      if (SymbType_is_boolean(type)) {
        result = body;
      }
      else if (SymbType_is_infinite_precision(type) ||
               SymbType_is_pure_int_enum(type)) {
        /* int = 0 ? FALSE : TRUE */
        node_ptr zero = find_node(nodemgr, NUMBER, NODE_FROM_INT(0), Nil);
        result = new_node(
            nodemgr, CASE,
            new_node(
                nodemgr, COLON,
                new_node(nodemgr, EQUAL, body, zero),
                ExprMgr_false(exprs)), ExprMgr_true(exprs));
      }
      else if (SymbType_is_word_1(type)) {
        /* w1 = 0ud1_0 ? FALSE : TRUE */
        node_ptr w0ud1 = find_node(
            nodemgr, NUMBER_UNSIGNED_WORD,
            NODE_PTR(WordNumberMgr_integer_to_word_number(words, 0, 1)), Nil);
        result = new_node(
            nodemgr, CASE,
            new_node(
                nodemgr, COLON,
                new_node(nodemgr, EQUAL, body, w0ud1),
                ExprMgr_false(exprs)), ExprMgr_true(exprs));
      }
      else {
        ErrorMgr_error_invalid_bool_cast(errmgr, body);
      }

      break;
    }

  case CAST_TOINT:
    {
      const NuSMVEnv_ptr env =
          EnvObject_get_environment(ENV_OBJECT(symb_table));
      node_ptr body;
      SymbType_ptr type;

      body = _THROW(car(sexp), symb_table, def_hash, context, mode);

      type = TypeChecker_get_expression_type(
          SymbTable_get_type_checker(symb_table), body, Nil);
      /* No cast needed */
      if (SymbType_is_infinite_precision(type) ||
          SymbType_is_pure_int_enum(type)) {
        result = body;
      }
      else if (SymbType_is_boolean(type)) {
        node_ptr one = find_node(nodemgr, NUMBER, NODE_FROM_INT(1), Nil);
        node_ptr zero = find_node(nodemgr, NUMBER, NODE_FROM_INT(0), Nil);

        result = new_node(nodemgr, CASE,
                          new_node(nodemgr, COLON, body, one), zero);
      }
      else if (SymbType_is_word(type)) {
        result = compile_flatten_rewrite_word_toint_cast(env, body, type);
      }
      else {
        ErrorMgr_error_invalid_toint_cast(errmgr, body);
      }
      break;
    }

  case COUNT:
    {
      TypeChecker_ptr tc = SymbTable_get_type_checker(symb_table);
      node_ptr list = car(sexp);
      node_ptr new_expr = Nil;
      nusmv_assert(Nil != list);

      /* Process the first element. There must be at least one
         parameter. */
      do {
        SymbType_ptr type;
        node_ptr cur_expr = car(list);
        node_ptr toint;

        /* TODO[AMa] The use of the Typechecker here for throwing an error is
           not a good practice. */
        /* First of all, check if this is a boolean expression */
        type = TypeChecker_get_expression_type(tc, cur_expr, context);
        if (!SymbType_is_boolean(type)) {
          ErrorMgr_error_invalid_count_operator(errmgr, cur_expr);
        }

        /* Rewrite the expression as toint(expr) */
        toint = _THROW(new_node(nodemgr, CAST_TOINT, cur_expr, Nil),
                       symb_table, def_hash,
                       context,
                       mode);

        if (Nil == new_expr) {
          new_expr = toint;
        }
        else {
          new_expr = new_node(nodemgr, PLUS, toint, new_expr);
        }

        list = cdr(list);
      } while (Nil != list);

      result = new_expr;
    }
    break;

  case TYPEOF:
    {
      node_ptr left = _THROW(car(sexp), symb_table, def_hash, context, mode);
      /* SymbType_ptr type =  SymbTable_get_var_type(symb_table, left);
         result = SymbType_to_node(type, nodemgr); */
      result = new_node(nodemgr, TYPEOF, left, Nil);
      break;
    }

    /* binary operators */

    /* [AI] internal local arrays: nothing to do */
  case INTERNAL_ARRAY_STRUCT:
    {
      node_ptr left  = car(sexp);
      node_ptr right = cdr(sexp);

      nusmv_assert(TWODOTS == node_get_type(left));
      result = new_node(nodemgr, INTERNAL_ARRAY_STRUCT, left, right);
      break;
    }

  case CONS:
  case AND:
    {
      node_ptr left  = _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr right = _THROW(cdr(sexp), symb_table, def_hash, context, mode);

      /* normal case of and/con */
      result = new_node(nodemgr, node_get_type(sexp), left, right);
      break;
    }

  case OR:
  case XOR:
  case XNOR:
  case IMPLIES:
  case IFF:
  case PLUS:
  case MINUS:
  case TIMES:
  case DIVIDE:
  case MOD:
  case LSHIFT:
  case RSHIFT:
  case LROTATE:
  case RROTATE:
  case LT:
  case GT:
  case LE:
  case GE:
  case UNION:
  case SETIN:
  case CONCATENATION:
  case COLON: /* can be part of CASE or BIT_SELECTION only */
              /* EDIT: it is also part of IFTHENELSE*/
  case CASE:
  case IFTHENELSE:
  case CAST_TO_UNSIGNED_WORD:
    {
      node_ptr left  = _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr right = _THROW(cdr(sexp), symb_table, def_hash, context, mode);

      result = new_node(nodemgr, node_get_type(sexp), left, right);
      break;
    }

  case CONST_ARRAY:
    {
      /* [AI] TODO: unbounded arrays */
      SymbType_ptr type;
      node_ptr array_type, const_array_value, array_name;

      array_type = _THROW(car(sexp), symb_table, def_hash, context, mode);
      const_array_value =
          _THROW(cdr(sexp), symb_table, def_hash, context, mode);

      type = _compute_constarray_type(symb_table, array_type);

      if (SymbType_is_intarray(type) || SymbType_is_wordarray(type)) {
        result =
            new_node(nodemgr, CONST_ARRAY, array_type, const_array_value);
        break;
      }

      if (SymbType_is_array(type)) {
        int i;
        SymbType_ptr const_array_type =
            SymbTable_get_variable_array_type(symb_table, car(array_type));
        int lower_bound = SymbType_get_array_lower_bound(const_array_type);
        int upper_bound = SymbType_get_array_upper_bound(const_array_type);
        int size = upper_bound-lower_bound+1;
        /* creating local array */
        node_ptr* local_array = ALLOC(node_ptr, size);

        for (i = 0; i < size; ++i) {
          local_array[i] = const_array_value;
        }

        result = find_node(nodemgr, INTERNAL_ARRAY_STRUCT,
                           find_node(nodemgr, TWODOTS,
                                     NODE_FROM_INT(lower_bound),
                                     NODE_FROM_INT(upper_bound)),
                           (node_ptr)local_array);
        break;
      }
      nusmv_assert(false);
    }

  case EQUAL:
  case NOTEQUAL:
    {
      node_ptr left  = _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr right = _THROW(cdr(sexp), symb_table, def_hash, context, mode);

      /* TODO: [AI] check that left and right are not both const_array */

      /* Check if
         left is array symbol
         left is next of array
         left is internal array structure */
      if (SymbTable_is_symbol_variable_array(symb_table, left) ||
          (node_get_type(left) == NEXT &&
           SymbTable_is_symbol_variable_array(symb_table, car(left))) ||
          node_get_type(left) == INTERNAL_ARRAY_STRUCT)
      {
        /* Left array case */
        const ExprMgr_ptr exprs =
            EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

        int left_lower_bound, left_upper_bound;
        node_ptr* left_local_array;
        boolean left_is_next, left_is_local_array;
        node_ptr left_res;

        int right_lower_bound, right_upper_bound;
        node_ptr* right_local_array;
        boolean right_is_next, right_is_local_array;
        node_ptr right_res;

        /* left case */
        left_is_next = left_is_local_array = false;

        if (node_get_type(left)==NEXT) {
          left_is_next = true;
          left = car(left);
        }

        /* -- Find the lower and upper bound of the array.
           First, check if the id is an array variable */
        if (SymbTable_is_symbol_variable_array(symb_table, left)) {
          SymbType_ptr type =
              SymbTable_get_variable_array_type(symb_table, left);

          /* This symbol has to be an array variable of array type */
          nusmv_assert((SymbType_ptr)NULL != type);
          nusmv_assert(SymbType_is_array(type));

          left_lower_bound = SymbType_get_array_lower_bound(type);
          left_upper_bound = SymbType_get_array_upper_bound(type);
        }
        /* array may be an array define */
        else if (SymbTable_is_symbol_array_define(symb_table, left)) {
          node_ptr body =
              SymbTable_get_array_define_body(symb_table, left);

          left_lower_bound = 0;
          left_upper_bound = llength(car(body)) - 1;

          nusmv_assert(ARRAY_DEF == node_get_type(body));
          nusmv_assert(left_upper_bound >= 0);
        }
        /* internal array structure case */
        else if (node_get_type(left) == INTERNAL_ARRAY_STRUCT) {
          node_ptr array_size = car(left);

          nusmv_assert(TWODOTS == node_get_type(array_size));

          left_is_local_array = true;
          left_local_array = (node_ptr *)cdr(left);
          left_lower_bound = NODE_TO_INT(car(array_size));
          left_upper_bound = NODE_TO_INT(cdr(array_size));
        }
        else {
          /* array has to be array var or array define but it is not.
             Actually, since type checking is not done yet LHS of
             array subscripting can be invalid expression.
          */
          ErrorMgr_error_lhs_of_index_is_not_array(errmgr);
          /* TODO: we may get the type of expression and obtain the
             bounds from their. Then constraints of having var array
             or array define can be removed */
        }

        /* Build the out of bounds case */
        left_res = ErrorMgr_failure_make(errmgr, "array access out of bounds",
                                         FAILURE_ARRAY_OUT_OF_BOUNDS,
                                         nusmv_yylineno);

        /* Right array case */
        right_is_next = right_is_local_array = false;

        if (node_get_type(right) == NEXT) {
          right_is_next = true;
          right = car(right);
        }

        /* -- Find the lower and upper bound of the array.
           First, check if the id is an array variable */
        if (SymbTable_is_symbol_variable_array(symb_table, right)) {
          SymbType_ptr type =
              SymbTable_get_variable_array_type(symb_table, right);

          /* This symbol has to be an array variable of array type */
          nusmv_assert((SymbType_ptr)NULL != type);
          nusmv_assert(SymbType_is_array(type));

          right_lower_bound = SymbType_get_array_lower_bound(type);
          right_upper_bound = SymbType_get_array_upper_bound(type);
        }
        /* array may be an array define */
        else if (SymbTable_is_symbol_array_define(symb_table, right)) {
          node_ptr body = SymbTable_get_array_define_body(symb_table, right);

          right_lower_bound = 0;
          right_upper_bound = llength(car(body)) - 1;

          nusmv_assert(ARRAY_DEF == node_get_type(body));
          nusmv_assert(left_upper_bound >= 0);
        }
        /* internal array structure case */
        else if (node_get_type(right) == INTERNAL_ARRAY_STRUCT) {
          node_ptr array_size = car(right);

          nusmv_assert(TWODOTS == node_get_type(array_size));

          right_is_local_array = true;
          right_local_array = (node_ptr *)cdr(right);
          array_size = car(car(right));
          right_lower_bound = NODE_TO_INT(car(array_size));
          right_upper_bound = NODE_TO_INT(cdr(array_size));
        }
        else {
          /* array has to be array var or array define but it is not.
             Actually, since type checking is not done yet LHS of
             array subscripting can be invalid expression.
          */
          ErrorMgr_error_lhs_of_index_is_not_array(errmgr);
          /* TODO: we may get the type of expression and obtain the
             bounds from their. Then constraints of having var array
             or array define can be removed */
        }

        /* Build the out of bounds case */
        right_res =
            ErrorMgr_failure_make(errmgr, "array access out of bounds",
                                  FAILURE_ARRAY_OUT_OF_BOUNDS,
                                  nusmv_yylineno);

        /* Array sizes are the same */
        nusmv_assert(left_lower_bound==right_lower_bound);
        nusmv_assert(left_upper_bound==right_upper_bound);

        {
          int i;
          for (i = left_upper_bound; i >= left_lower_bound; --i) {
            node_ptr num;
            node_ptr cond;
            node_ptr left_body, right_body;

            if (!left_is_local_array) {
              num = find_node(nodemgr, NUMBER, NODE_FROM_INT(i), Nil);
              left_body = find_node(nodemgr, ARRAY, left, num);
              /* it is necessary to fully resolve newly created ARRAY
                 expression for 2 reasons:
                 1) the expression can be a define and the flatting mode
                 is set to define-expansion.
                 2) the array ID (left child of ARRAY) may be a define
                 which are always resolved in ARRAY expression. */
              left_body = _THROW(left_body, symb_table, def_hash, Nil, mode);
            }
            else {
              left_body = left_local_array[i-left_lower_bound];
            }
            if (!right_is_local_array) {
              num = find_node(nodemgr, NUMBER, NODE_FROM_INT(i), Nil);
              right_body = find_node(nodemgr, ARRAY, right, num);
              /* it is necessary to fully resolve newly created ARRAY
                 expression for 2 reasons:
                 1) the expression can be a define and the flatting mode
                 is set to define-expansion.
                 2) the array ID (left child of ARRAY) may be a define
                 which are always resolved in ARRAY expression. */
              right_body = _THROW(right_body, symb_table, def_hash,
                                  Nil, mode);
            }
            else {
              right_body = right_local_array[i-left_lower_bound];
            }

            /* handling next case */
            if (left_is_next) {
              left_body = find_node(nodemgr, NEXT, left_body, Nil);
            }
            if (right_is_next) {
              right_body = find_node(nodemgr, NEXT, right_body, Nil);
            }

            cond = ((node_get_type(sexp) == EQUAL)?
                    ExprMgr_equal(exprs, left_body,
                                  right_body, symb_table)
                    :
                    ExprMgr_notequal(exprs, left_body,
                                     right_body, symb_table));

            /* This might be another array, thus we may need to
             * further expand it */
            cond = _THROW(cond, symb_table, def_hash, Nil, mode);

            if (left_upper_bound - left_lower_bound == 0 ||
                i == left_upper_bound) {
              result = cond;
            }
            else {
                /* TODO: MRTOCHECK */
              if (node_get_type(sexp) == EQUAL) {
                result = ExprMgr_and(exprs, cond, result);
              }
              else {
                result = ExprMgr_or(exprs, cond, result);
              }
            }
          }  /* for loop */
        }
      }
      else {
        result = new_node(nodemgr, node_get_type(sexp), left, right);
      }

      break;
    }

  case WAREAD:
    {
      node_ptr array = _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr index = _THROW(cdr(sexp), symb_table, def_hash, context, mode);
      SymbType_ptr arraytype =
            TypeChecker_get_expression_type(
                SymbTable_get_type_checker(symb_table), array, Nil);

      if (node_get_type(array) == INTERNAL_ARRAY_STRUCT) {
        node_ptr* local_array = (node_ptr *)cdr(array);
        node_ptr array_size = car(array);
        node_ptr tmp;
        int upper_bound, lower_bound;

        nusmv_assert(TWODOTS == node_get_type(array_size));

        upper_bound = NODE_TO_INT(car(array_size));
        lower_bound = NODE_TO_INT(cdr(array_size));

        /* simple case */
        tmp = CompileFlatten_resolve_number(symb_table, index, context);
        if (Nil != tmp) { /* The current index is constant */
          int i;
          index = tmp;
          /* convert word constant to integer constant */
          if (NUMBER_SIGNED_WORD == node_get_type(index) ||
              NUMBER_UNSIGNED_WORD == node_get_type(index)) {
            WordNumberValue val
              = WordNumber_get_unsigned_value((WordNumber_ptr) car(index));
            index = find_node(nodemgr, NUMBER, NODE_FROM_INT((int)val), Nil);

            /* if below assertion is violated it means that int is not enough
               to represent indexes of arrays. Representation of nodes
               have to be changed then (as NUMBER keep numbers in int). */
            nusmv_assert(((int)val) == val);
          }

          i = node_get_int(index);
          if (i < lower_bound || i > upper_bound) {
            ErrorMgr_error_array_out_of_bounds(
                errmgr, i, lower_bound, upper_bound);
          }
          result = local_array[i-lower_bound];
        }
        /* multiplexer case */
        else {
          const WordNumberMgr_ptr words =
            WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));
          const ExprMgr_ptr exprs =
              EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
          SymbType_ptr indextype =
            TypeChecker_get_expression_type(
                SymbTable_get_type_checker(symb_table), index, Nil);
          int i;

          result = ErrorMgr_failure_make(errmgr, "array access out of bounds",
                                         FAILURE_ARRAY_OUT_OF_BOUNDS,
                                         nusmv_yylineno);
          for (i = upper_bound; i >= lower_bound; --i) {
            node_ptr num;
            node_ptr eq;

            if (SymbType_is_word(indextype)) {
              int size = SymbType_get_word_width(indextype);
              if (SymbType_is_signed_word(indextype)) {
                num = find_node(
                    nodemgr, NUMBER_SIGNED_WORD,
                    (node_ptr) WordNumberMgr_signed_integer_to_word_number(
                        words, (WordNumberValue) i, size),
                    Nil);
              }
              else {
                num = find_node(
                    nodemgr, NUMBER_UNSIGNED_WORD,
                    (node_ptr) WordNumberMgr_integer_to_word_number(
                        words, (WordNumberValue) i, size),
                    Nil);
              }
            }
            else {
              num = find_node(nodemgr, NUMBER, NODE_FROM_INT(i), Nil);
            }
            eq = ExprMgr_equal(exprs, index, num, symb_table);
            result = ExprMgr_ite(exprs, eq,
                                 local_array[i-lower_bound],
                                 result, symb_table);
          }
        }
      }
      else if (SymbType_is_intarray(arraytype) ||
               SymbType_is_wordarray(arraytype)) {
        result = new_node(nodemgr, WAREAD, array, index);
      }
      else { /* normal array case */
        result = find_node(nodemgr, ARRAY, array, index);
      }
      break;
    }

  case WAWRITE:
    {
      const WordNumberMgr_ptr words =
        WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));
      const ExprMgr_ptr exprs =
          EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));
      node_ptr* local_array;
      boolean is_local_array, is_index_constant;
      int lower_bound, upper_bound;

      node_ptr array =
          _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr index =
          _THROW(car(cdr(sexp)), symb_table, def_hash, context, mode);
      node_ptr value =
          _THROW(cdr(cdr(sexp)), symb_table, def_hash, context, mode);
      SymbType_ptr arraytype =
            TypeChecker_get_expression_type(
                SymbTable_get_type_checker(symb_table),
                array, Nil);
      SymbType_ptr indextype;

      /* [AI] unbounded arrays case or bounded array */
      if (SymbType_is_intarray(arraytype) ||
          SymbType_is_wordarray(arraytype)) {
        result = new_node(nodemgr, WAWRITE, array,
                          new_node(nodemgr, WAWRITE, index, value));
        break;
      }

      /* [AI] bounded array case with array expansion
      bounded arrays are treated as now arrays (subrange) type */
      is_local_array = is_index_constant = false;

      /* -- Find the lower and upper bound of the array.
         First, check if the id is an array variable */
      if (SymbTable_is_symbol_variable_array(symb_table, array)) {
        SymbType_ptr type =
            SymbTable_get_variable_array_type(symb_table, array);

        /* This symbol has to be an array variable of array type */
        nusmv_assert((SymbType_ptr)NULL != type);
        nusmv_assert(SymbType_is_array(type));

        lower_bound = SymbType_get_array_lower_bound(type);
        upper_bound = SymbType_get_array_upper_bound(type);
      }
      /* array may be an array define */
      else if (SymbTable_is_symbol_array_define(symb_table, array)) {
        node_ptr body = SymbTable_get_array_define_body(symb_table, array);

        lower_bound = 0;
        upper_bound = llength(car(body)) - 1;

        nusmv_assert(ARRAY_DEF == node_get_type(body));
        nusmv_assert(upper_bound >= 0);
      }
      else if (node_get_type(array) == INTERNAL_ARRAY_STRUCT) {
        node_ptr array_size = car(array);

        nusmv_assert(TWODOTS == node_get_type(array_size));
        is_local_array = true;
        local_array = (node_ptr *)cdr(array);
        lower_bound = NODE_TO_INT(car(array_size));
        upper_bound = NODE_TO_INT(cdr(array_size));
      }
      else {
        /* array has to be array var or array define but it is not.
           Actually, since type checking is not done yet LHS of array
           subscripting can be invalid expression.
        */
        ErrorMgr_error_lhs_of_index_is_not_array(errmgr);
        /* TODO: we may get the type of expression and obtain the bounds
           from their. Then constraints of having var array or array define
           can be removed */
      }

      /* assuming the lower bound starts from zero */
      // TODO: CHECK CHECK
      nusmv_assert(lower_bound <= upper_bound && lower_bound == 0);

      /* create local array for write function , if not already a
       * local array */
      if (!is_local_array) {
        /* TODO: take care of memory */
        local_array = ALLOC(node_ptr, upper_bound-lower_bound+1);
      }

      indextype =
        TypeChecker_get_expression_type(
            SymbTable_get_type_checker(symb_table),
            index, Nil);

      /* Optimization: if the index is a word (which have a limited domain)
         we may limit the range of indexes to word domain */
      if (SymbType_is_word(indextype)) {
        int size = SymbType_get_word_width(indextype);
        WordNumber_ptr maxWord;
        unsigned long long maxValue;

        /* max possible value which is (2^size - 1) */
        if (SymbType_is_signed_word(indextype)) {
          maxWord = WordNumberMgr_max_signed_value(words, size);
          maxValue = WordNumber_get_signed_value(maxWord);
        }
        else {
          maxWord = WordNumberMgr_max_unsigned_value(words, size);
          maxValue = WordNumber_get_unsigned_value(maxWord);
        }
      }

      {  /* check index is constant */
        node_ptr tmp =
            CompileFlatten_resolve_number(symb_table, index, context);
        if (tmp != Nil) { /* The current index is constant */
          index = tmp;
          is_index_constant = true;

          /* convert word constant to integer constant */
          if (NUMBER_SIGNED_WORD == node_get_type(index) ||
              NUMBER_UNSIGNED_WORD == node_get_type(index)) {
            WordNumberValue val =
                WordNumber_get_unsigned_value((WordNumber_ptr) car(index));
            index = find_node(nodemgr, NUMBER, NODE_FROM_INT((int)val), Nil);

            /* if below assertion is violated it means that int is not enough
               to represent indexes of arrays. Representation of nodes
               have to be changed then (as NUMBER keep numbers in int). */
            nusmv_assert(((int)val) == val);
          }

          {
            int j = node_get_int(index);
            if (j < lower_bound || j > upper_bound) {
              ErrorMgr_error_array_out_of_bounds(errmgr, j,
                                                 lower_bound, upper_bound);
            }
          }
        }
      }

      if (!is_local_array) {
        /* Build the out of bounds case */
        node_ptr res =
            ErrorMgr_failure_make(errmgr, "array access out of bounds",
                                  FAILURE_ARRAY_OUT_OF_BOUNDS,
                                  nusmv_yylineno);
        int i;

        /* initializaing the local array */
        for (i = upper_bound; i >= lower_bound; --i) {
          node_ptr num;
          node_ptr neq;
          node_ptr body;

          if (SymbType_is_word(indextype)) {
            int size = SymbType_get_word_width(indextype);
            if (SymbType_is_signed_word(indextype)) {
              num = find_node(
                  nodemgr,
                  NUMBER_SIGNED_WORD,
                  (node_ptr) WordNumberMgr_signed_integer_to_word_number(
                      words, (WordNumberValue) i, size),
                  Nil);
            }
            else {
              num = find_node(
                  nodemgr,
                  NUMBER_UNSIGNED_WORD,
                  (node_ptr) WordNumberMgr_integer_to_word_number(
                      words, (WordNumberValue) i, size),
                  Nil);
            }
          }
          else {
            num = find_node(nodemgr, NUMBER, NODE_FROM_INT(i), Nil);
          }

          body = find_node(nodemgr, ARRAY, array, num);
          /* it is necessary to fully resolve newly created ARRAY
             expression for 2 reasons:
             1) the expression can be a define and the flatting mode
                is set to define-expansion.
             2) the array ID (left child of ARRAY) may be a define
                which are always resolved in ARRAY expression. */
          body = _THROW(body, symb_table, def_hash, Nil, mode);

          if (is_index_constant) {/* constant case */
            int j = node_get_int(index);
            res = (i==j)? value : body;
          }
          else {
            neq = ExprMgr_notequal(exprs, index, num, symb_table);
            res = ExprMgr_ite(exprs, neq, body, value, symb_table);
          }

          local_array[i - lower_bound] = res;
        }  /* end for loop */
      }
      else {  /* modifying local array */
        int i;
        for (i = upper_bound; i >= lower_bound; --i) {
          if (is_index_constant) {/* constant case */
            int j = node_get_int(index);
            if (i==j)
              local_array[i - lower_bound] = value;
          }
          else {
            node_ptr num;
            node_ptr neq;
            if (SymbType_is_word(indextype)) {
              int size = SymbType_get_word_width(indextype);
              if (SymbType_is_signed_word(indextype)) {
                num = find_node(
                    nodemgr,
                    NUMBER_SIGNED_WORD,
                    (node_ptr) WordNumberMgr_signed_integer_to_word_number(
                        words,
                        (WordNumberValue) i, size),
                    Nil);
              }
              else {
                num = find_node(
                    nodemgr,
                    NUMBER_UNSIGNED_WORD,
                    (node_ptr) WordNumberMgr_integer_to_word_number(
                        words, (WordNumberValue) i, size),
                    Nil);
              }
            }
            else {
              num = find_node(nodemgr, NUMBER, NODE_FROM_INT(i), Nil);
            }
            neq = ExprMgr_notequal(exprs, index, num, symb_table);
            local_array[i - lower_bound] =
              ExprMgr_ite(exprs, neq, local_array[i - lower_bound],
                          value, symb_table);
          }
        }
      }

      result = new_node(nodemgr, INTERNAL_ARRAY_STRUCT,
                        find_node(nodemgr, TWODOTS,
                                  NODE_FROM_INT(lower_bound),
                                  NODE_FROM_INT(upper_bound)),
                        (node_ptr)local_array);
      break;
    }

  case WRESIZE:
    {
      node_ptr left  = _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr right = _THROW(cdr(sexp), symb_table, def_hash, context, mode);

      { /* just check */
        node_ptr value = CompileFlatten_resolve_number(symb_table,
                                                       right,
                                                       context);

        if ((Nil == value) || (NUMBER != node_get_type(value))) {
          ErrorMgr_error_not_constant_resize_width(errmgr, cdr(sexp));
        }
      }
      result = new_node(nodemgr, node_get_type(sexp), left, right);
      break;
    }

  case WSIZEOF:
    {
      node_ptr body = _THROW(car(sexp), symb_table, def_hash, context, mode);
      SymbType_ptr type =
        TypeChecker_get_expression_type(
            SymbTable_get_type_checker(symb_table), body, Nil);

      if (SymbType_is_error(type)) {
        ErrorMgr_error_not_word_wsizeof(errmgr, car(sexp));
      }

      {
        int width = SymbType_get_word_width(type);
        nusmv_assert(0 < width);

        result = find_node(nodemgr, NUMBER, NODE_FROM_INT(width), Nil);
        break;
      }
    }

  case EXTEND:
    {
      node_ptr base = _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr delta = _THROW(cdr(sexp), symb_table, def_hash, context, mode);

      {
        node_ptr value = CompileFlatten_resolve_number(symb_table,
                                                       delta,
                                                       context);

        if ((Nil == value) || NUMBER != node_get_type(value)) {

          ErrorMgr_error_not_constant_extend_width(errmgr, cdr(sexp));
        }
      }
      result = new_node(nodemgr, EXTEND, base, delta);
      break;
    }

  case BIT_SELECTION:
    {
      node_ptr base = _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr bits = _THROW(cdr(sexp), symb_table, def_hash, context, mode);

      result = new_node(nodemgr, BIT_SELECTION, base, bits);
      break;
    }

  case TWODOTS:
    {
      /* We don't need to expand it, eval did it */
      result = sexp;
      break;
    }

    /* CTL Unary operators */
  case EX:
  case AX:
  case EF:
  case AF:
  case EG:
  case AG:
    {
      node_ptr body = _THROW(car(sexp), symb_table, def_hash, context, mode);

      result = new_node(nodemgr, node_get_type(sexp), body, Nil);
      break;
    }

    /* CTL bynary operators */
  case EU:
  case AU:
    {
      node_ptr left  = _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr right = _THROW(cdr(sexp), symb_table, def_hash, context, mode);

      result = new_node(nodemgr, node_get_type(sexp), left, right);
      break;
    }

    /* CTL bounded Temporal Operators */
  case EBF:
  case ABF:
  case EBG:
  case ABG:
  case EBU:
  case ABU:
    {
      node_ptr body  = _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr range = cdr(sexp);

      /* checks the range: */
      if (! Utils_check_subrange_not_negative(range) ) {
        ErrorMgr_error_invalid_subrange(errmgr, range);
      }

      result = new_node(nodemgr, node_get_type(sexp), body, range);
      break;
    }

    /* LTL unary temporal operators */
  case OP_NEXT:
  case OP_PREC:
  case OP_NOTPRECNOT:
  case OP_FUTURE:
  case OP_ONCE:
  case OP_GLOBAL:
  case OP_HISTORICAL:
    {
      node_ptr body  = _THROW(car(sexp), symb_table, def_hash, context, mode);
      /* To consider possible bounds on the LTL O, H, G, F operators. */
      node_ptr right =
          ((Nil != cdr(sexp)) ?
           _THROW(cdr(sexp), symb_table, def_hash, context, mode) : Nil);
      result = new_node(nodemgr, node_get_type(sexp), body, right);
      break;
    }

    /* LTL binary temporal operators */
  case UNTIL:
  case RELEASES:
  case SINCE:
  case TRIGGERED:
    {
      node_ptr left   = _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr right  = _THROW(cdr(sexp), symb_table, def_hash, context, mode);

      result = new_node(nodemgr, node_get_type(sexp), left, right);
      break;
    }

    /* MIN and MAX operators */
  case MINU:
  case MAXU:
    {
      node_ptr left   = _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr right  = _THROW(cdr(sexp), symb_table, def_hash, context, mode);

      result = new_node(nodemgr, node_get_type(sexp), left, right);
      break;
    }
  case EQDEF:
    {
      node_ptr left  = car(sexp);
      node_ptr right = cdr(sexp) ;
      node_ptr res_left, res_right;
      node_ptr name;

      switch (node_get_type(left)) {
      case SMALLINIT:
      case NEXT:
        /* we are dealing with init(x) := init_expr or next(x) := next_expr */
        name = _THROW(car(left), symb_table, def_hash, context, mode);
        res_left = new_node(nodemgr, node_get_type(left), name, Nil);
        res_right = _THROW(right, symb_table, def_hash, context, mode);
        break;

      default:
        /* we are dealing with x := simple_expr */
        name = res_left = _THROW(left, symb_table, def_hash, context, mode);
        res_right = _THROW(right, symb_table, def_hash, context, mode);
        break;
      }

      /* line number reporting in error messages).
         Here only assertions may be. */
      if ((IFTHENELSE == node_get_type(name)) ||
          (CASE == node_get_type(name))) {
        /* Note about array defines: Dynamic indexes are not allowed
             in assignment. Thus no if-then-else expression is
             possible here. */
        ErrorMgr_rpterr(errmgr,
                        "Expressions not allowed in array subscripts on "
                        "left hand side of assignments");
      }
      name = _resolve_name_or_nop(symb_table, name, context);
      if (SymbTable_is_symbol_variable_array(symb_table, name)) {
        result =
          _flatten_resolve_variable_array_eqdef(self, symb_table, def_hash,
                                                name, node_get_type(left),
                                                res_right, context, mode);
        break;
      }
      if (node_get_type(name) == BIT_SELECTION) {
        error_bit_selection_assignment_not_supported(env, left);
      }
      /* name is expected to be a variable */
      if (!SymbTable_is_symbol_var(symb_table, name)) {
        ErrorMgr_error_assign_expected_var(errmgr, sexp);
      }

      result = new_node(nodemgr, EQDEF, res_left, res_right);
      break;
    }

  case ARRAY_DEF:
    {
      node_ptr left  = _THROW(car(sexp), symb_table, def_hash, context, mode);
      node_ptr right = _THROW(cdr(sexp), symb_table, def_hash, context, mode);

      result = new_node(nodemgr, ARRAY_DEF, left, right);
      break;
    }

  case NFUNCTION:
    {
      node_ptr params = _THROW(cdr(sexp), symb_table, def_hash, context, mode);
      ResolveSymbol_ptr rs =
          SymbTable_resolve_symbol(symb_table, car(sexp), context);
      node_ptr resolvedName = ResolveSymbol_get_resolved_name(rs);

      if (ResolveSymbol_is_parameter(rs)) {
        node_ptr param;
        /* The result of the flattening is then flattening of parameters */

        param =
            SymbTable_get_flatten_actual_parameter(symb_table, resolvedName);
        result = _THROW(param, symb_table, def_hash, context, mode);

        rs = SymbTable_resolve_symbol(symb_table, result, Nil);
        resolvedName = ResolveSymbol_get_resolved_name(rs);
      }


      result = new_node(nodemgr, NFUNCTION, resolvedName, params);
    }
    break;

  default:
    break;
    /* StreamMgr_print_error(streams, */
    /*         "flattener_core_flatten: undefined node type (%d)\n", */
    /*         node_get_type(sexp)); */
    /* ErrorMgr_nusmv_exit(errmgr, 1); */
  }

  /* nusmv_assert(result != Nil); */
  nusmv_yylineno = temp;
  return result;
}


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The FlattenerCore class virtual finalizer

  Called by the class destructor
*/
static void flattener_core_finalize(Object_ptr object, void* dummy)
{
  FlattenerCore_ptr self = FLATTENER_CORE(object);

  flattener_core_deinit(self);
  FREE(self);
}

/*!
  \brief Create array multiplexer in order to get rid of dynamic
   indexes.

  This function takes index-access expression
   with dynamic index and returns if-then-else expression
   with all indexes are constants
   E.g.:
      a[i]
   is converted to
      case i = min-index : a [0];
           i = min-index+1 : a [1];
           ...
           i = max-index : a [max-index];
           failure;
       esac;

   Precondition: array is allowed to be an array variable or
   array define only.

   The array and its index have to be resolved already, i.e.
   context is not required.

   Flat is_array_next signals that array has to be
   wrapped in NEXT, whereas index is not.

   The minimal and maximal indexes are obtained from the type
   of the array.

*/
static node_ptr
flattener_core_construct_array_multiplexer(
    FlattenerBase_ptr self,
    node_ptr array, node_ptr index,
    boolean is_array_next,
    SymbTable_ptr symb_table,
    hash_ptr def_hash,
    const MasterCompileFlattener_def_mode mode)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const WordNumberMgr_ptr words =
    WORD_NUMBER_MGR(NuSMVEnv_get_value(env, ENV_WORD_NUMBER_MGR));
  const ExprMgr_ptr exprs =
      EXPR_MGR(NuSMVEnv_get_value(env, ENV_EXPR_MANAGER));

  /* see ARRAY description at the beginning of flattener_core_flatten */
  int lower_bound, upper_bound;

  /* -- Find the lower and upper bound of the array.
     First, check if the id is an array variable */
  if (SymbTable_is_symbol_variable_array(symb_table, array)) {
    SymbType_ptr type = SymbTable_get_variable_array_type(symb_table, array);

    /* This symbol has to be an array variable of array type */
    nusmv_assert((SymbType_ptr)NULL != type);
    nusmv_assert(SymbType_is_array(type));

    lower_bound = SymbType_get_array_lower_bound(type);
    upper_bound = SymbType_get_array_upper_bound(type);
  }
  /* array may be an array define */
  else if (SymbTable_is_symbol_array_define(symb_table, array)) {
    node_ptr body = SymbTable_get_array_define_body(symb_table, array);

    lower_bound = 0;
    upper_bound = llength(car(body)) - 1;

    nusmv_assert(ARRAY_DEF == node_get_type(body));
    nusmv_assert(upper_bound >= 0);
  }
  else {
    /* array has to be array var or array define but it is not.
       Actually, since type checking is not done yet LHS of array
       subscripting can be invalid expression.
    */
    ErrorMgr_error_lhs_of_index_is_not_array(errmgr);
    /* TODO: we may get the type of expression and obtain the bounds
       from their. Then constraints of having var array or array define
       can be removed */
  }

  {
    /* Build the out of bounds case */
    node_ptr res = ErrorMgr_failure_make(errmgr, "array access out of bounds",
                                         FAILURE_ARRAY_OUT_OF_BOUNDS,
                                         nusmv_yylineno);
    int i;
    SymbType_ptr indextype =
        TypeChecker_get_expression_type(
            SymbTable_get_type_checker(symb_table), index, Nil);

    /* Optimization: if the index is a word (which have a limited domain)
       we may limit the range of indexes to word domain */
    if (SymbType_is_word(indextype)) {
      int size = SymbType_get_word_width(indextype);
      WordNumber_ptr maxWord;
      unsigned long long maxValue;

      /* max possible value which is (2^size - 1) */
      if (SymbType_is_signed_word(indextype)) {
        maxWord = WordNumberMgr_max_signed_value(words, size);
        maxValue = WordNumber_get_signed_value(maxWord);
      }
      else {
        maxWord = WordNumberMgr_max_unsigned_value(words, size);
        maxValue = WordNumber_get_unsigned_value(maxWord);
      }

      /* Limit the minimal value:
         unsigned word has minimal value 0
         the singed one has minimal value -(2^(size-1)), i.e. -(max-value+1) */
      if (SymbType_is_unsigned_word(indextype) && lower_bound < 0) {
        lower_bound = 0;
      }
      else if (SymbType_is_signed_word(indextype) &&
               lower_bound < 0 &&
               (-lower_bound) > (maxValue + 1)) {
        lower_bound = -(maxValue + 1);
        nusmv_assert(lower_bound < 0); /* no overflow is possible here */
      }

      /* upper bound cannot be greater than max word value */
      if (upper_bound > 0 /* to avoid signed/unsigned cmp and casts */ &&
          upper_bound > maxValue) {
        upper_bound = maxValue;
        nusmv_assert(maxValue == upper_bound); /* overflow detection */
      }
    }

    for (i=upper_bound; i>=lower_bound; --i) {
      node_ptr num;
      node_ptr eq;
      node_ptr body;

      if (SymbType_is_word(indextype)) {
        int size = SymbType_get_word_width(indextype);
        if (SymbType_is_signed_word(indextype)) {
          num = find_node(
              nodemgr,
              NUMBER_SIGNED_WORD,
              (node_ptr) WordNumberMgr_signed_integer_to_word_number(
                  words, (WordNumberValue)i, size),
              Nil);
        }
        else {
          num = find_node(
              nodemgr,
              NUMBER_UNSIGNED_WORD,
              (node_ptr) WordNumberMgr_integer_to_word_number(
                  words, (WordNumberValue)i, size),
              Nil);
        }
      }
      else {
        num = find_node(nodemgr, NUMBER, NODE_FROM_INT(i), Nil);
      }
      eq = ExprMgr_equal(exprs, index, num, symb_table);
      body = find_node(nodemgr, ARRAY, array, num);
      /* it is necessary to fully resolve newly created ARRAY
         expression for 2 reasons:
         1) the expression can be a define and the flatting mode
         is set to define-expansion.
         2) the array ID (left child of ARRAY) may be a define
         which are always resolved in ARRAY expression. */
      body = _THROW(body, symb_table, def_hash, Nil, mode);

      if (is_array_next)
        body = find_node(nodemgr, NEXT, body, Nil);

      res = ExprMgr_ite(exprs, eq, body, res, symb_table);
    }

    return res;
  }
}

/*!
  \brief Pushes the index-access operator down
   to if-then-else expressions leaves.

  An index-access operator can be applied
   to if-then-else expression. In such case this function is used
   to push the index-access operator down. E.g.
   (a ? b : c)[i] will be converted to (a ? b[i] : c[i]).

   Flag is_array_next signals that array expression has to
   be wrapped in next whereas index remains intact.

   If idx is a variable index multiplexer is generated (on
   leaves).
*/
static node_ptr
flattener_core_push_array_index_down(
    FlattenerBase_ptr self,
    node_ptr array, node_ptr index,
    boolean is_array_next,
    SymbTable_ptr st,
    hash_ptr def_hash,
    const MasterCompileFlattener_def_mode mode)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  /* see array description in comment at the beginning of
   * flattener_core_flatten */

  switch (node_get_type(array)) {
  case CASE:
  case IFTHENELSE: {
    node_ptr left, right;
    left = flattener_core_push_array_index_down(self, car(array), index,
                                                is_array_next, st,
                                                def_hash, mode);
    right = flattener_core_push_array_index_down(self, cdr(array), index,
                                                 is_array_next, st,
                                                 def_hash, mode);
    return find_node(nodemgr, node_get_type(array), left, right);
  }

  case COLON: {
    node_ptr left, right;

    left = car(array);
    if (is_array_next)
      left = find_node(nodemgr, NEXT, left, Nil);

    right =
        flattener_core_push_array_index_down(self, cdr(array), index,
                                             is_array_next, st,
                                             def_hash, mode);

    return find_node(nodemgr, COLON, left, right);
  }

    /* --- a leave is reached --- */
  case FAILURE:  /* failure remains unchanged */
    return array;

  case NEXT:  /* next is passed as parameter */
    nusmv_assert(!is_array_next); /* double next */
    return flattener_core_push_array_index_down(self, car(array), index, true,
                                                st, def_hash, mode);

  default:  /* a normal expression */
    {
      node_ptr expr;
      /* it is necessary to put next around array.
         Optimization: if index is a number then next can be put around
         whole array expression. */
      if (NUMBER == node_get_type(index)) /* words were converted to int */ {
        expr = find_node(nodemgr, ARRAY, array, index);
        if (is_array_next) expr = find_node(nodemgr, NEXT, expr, Nil);
      }
      else {
        expr = array;
        if (is_array_next) expr = find_node(nodemgr, NEXT, array, Nil);
        expr = find_node(nodemgr, ARRAY, expr, index);
      }
      /* Here we again invoke recursion.
         There are 2 reasons to do so:

         1) If index is a dynamic expression then created array
            expression has to be resolved by multiplexer (i.e.
            CompileFlatten_construct_array_multiplexer) but we are
            lazy to compute bound thus we simply invoke recursively to
            do whatever is required

         2) Define expansion may be required to be done.  Again we are
            lazy to check here if a generated expression is a define
            or not. Thus we simply invoke recursively
            to do the job.
      */
      return  _THROW(expr, st, def_hash, Nil, mode);
    }
  } /* switch */
}


static SymbType_ptr _compute_constarray_type(SymbTable_ptr st,
                                             node_ptr array_type)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  SymbType_ptr type;
  ResolveSymbol_ptr rs;
  node_ptr name;

  /* We are only expecting TYPEOF here for the time being */
  nusmv_assert(TYPEOF == node_get_type(array_type));

  name = car(array_type);

  /* The array_type is expected to be already flattened */
  rs = SymbTable_resolve_symbol(st, name, Nil);

  name = ResolveSymbol_get_resolved_name(rs);

  if (ResolveSymbol_is_error(rs)) ResolveSymbol_throw_error(rs, env);

  if (ResolveSymbol_is_array(rs) || ResolveSymbol_is_var(rs)) {
    if (SymbTable_is_symbol_variable_array(st, name)) {
      type = SymbTable_get_variable_array_type(st, name);
    }
    else if (SymbTable_is_symbol_var(st, name)) {
      type = SymbTable_get_var_type(st, name);
    }
    else {
      nusmv_assert(false);
    }
  }
  else {
    nusmv_assert(false);
  }
  return type;
}


static node_ptr _resolve_name_or_nop(SymbTable_ptr st,
                                     node_ptr expr, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
  ResolveSymbol_ptr rs;

  /* The array_type is expected to be already flattened */
  rs = SymbTable_resolve_symbol(st, expr, context);

  if (ResolveSymbol_is_array(rs) || ResolveSymbol_is_var(rs)) {
    return ResolveSymbol_get_resolved_name(rs);
  }
  return expr;
}

/*!
  \brief Interannly expands variable array assignments

  This function is responsible for internal variable array assignment
  expansion. I.e.

  VAR a array -2..5 of word[32];
  ASSIGN next(a) := CONSTARRAY(typeof(a), 0ud32_10)

  VAR a array -2..5 of word[32];
  ASSIGN
   next(a[-2]) := 0ud32_10;
   ....
   next(a[5]) := 0ud32_10;

  It hadles also array of array.
  VAR a : array -2..5 of word[32];
  VAR b : array 1..2 of array 2..4 of boolean;
  VAR d : array 1..2 of array 2..4 of boolean;
  VAR c : array 2..4 of boolean;


  ASSIGN
    next(d[1]) := CONSTARRAY(typeof(c), TRUE);
    next(d[2]) := CONSTARRAY(typeof(c), TRUE);
    next(b) := CONSTARRAY(typeof(b), CONSTARRAY(typeof(c), TRUE));

  Becomes:

  ASSIGN next(d[1][2]) := TRUE;
  ....
  ASSIGN next(d[2][4]) := TRUE;
  ASSIGN next(b[1][2]) := TRUE;
  ....
  ASSIGN next(b[2][4]) := TRUE;
*/

static node_ptr _flatten_resolve_variable_array_eqdef(
    FlattenerBase_ptr self,
    SymbTable_ptr symb_table,
    hash_ptr def_hash,
    node_ptr left,
    int left_type,
    node_ptr right,
    node_ptr context,
    MasterCompileFlattener_def_mode mode)
{
  SymbType_ptr type;
  int left_lower_bound, left_upper_bound;
  int right_lower_bound, right_upper_bound;
  boolean right_is_local_array;
  node_ptr* right_local_array;
  node_ptr result;
  boolean right_is_next;

  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(self));
  const ErrorMgr_ptr emgr =
      ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));

  nusmv_assert((SMALLINIT == left_type) || (NEXT == left_type));

  left = _resolve_name_or_nop(symb_table, left, context);

  /* Here we expect to see only variable_array */
  nusmv_assert(SymbTable_is_symbol_variable_array(symb_table, left));

  right_is_next = false;

  /* checking next for right hand side */
  if (NEXT == node_get_type(right)) {
    right_is_next = true;
    right = car(right);
  }

  /* left hand side is a array variable */
  type = SymbTable_get_variable_array_type(symb_table, left);

  /* This symbol has to be an array variable of array type */
  nusmv_assert((SymbType_ptr)NULL != type);
  nusmv_assert(SymbType_is_array(type));

  left_lower_bound = SymbType_get_array_lower_bound(type);
  left_upper_bound = SymbType_get_array_upper_bound(type);

  /* Right array case */
  right_is_local_array = false;

  /* Find the lower and upper bound of the array.  First, check if the
     id is an array variable */
  right = _resolve_name_or_nop(symb_table, right, context);

  if (SymbTable_is_symbol_variable_array(symb_table, right)) {
    SymbType_ptr type = SymbTable_get_variable_array_type(symb_table, right);

    /* This symbol has to be an array variable of array type */
    nusmv_assert((SymbType_ptr)NULL != type);
    nusmv_assert(SymbType_is_array(type));

    right_lower_bound = SymbType_get_array_lower_bound(type);
    right_upper_bound = SymbType_get_array_upper_bound(type);
  }
  /* array may be an array define */
  else if (SymbTable_is_symbol_array_define(symb_table, right)) {
    node_ptr body = SymbTable_get_array_define_body(symb_table, right);

    right_lower_bound = 0;
    right_upper_bound = llength(car(body)) - 1;

    nusmv_assert(ARRAY_DEF == node_get_type(body));
    nusmv_assert(left_upper_bound >= 0);
  }
  /* internal array structure case */
  else if (node_get_type(right) == INTERNAL_ARRAY_STRUCT) {
    node_ptr array_size = car(right);

    nusmv_assert(TWODOTS == node_get_type(array_size));

    right_is_local_array = true;
    right_local_array = (node_ptr *)cdr(right);

    right_lower_bound = NODE_TO_INT(car(array_size));
    right_upper_bound = NODE_TO_INT(cdr(array_size));
  }
  else {
    /* array has to be array var or array define but it is not.
       Actually, since type checking is not done yet LHS of array
       subscripting can be invalid expression.
    */
    ErrorMgr_error_lhs_of_index_is_not_array(emgr);
    /* TODO: we may get the type of expression and obtain the bounds
       from their. Then constraints of having var array or array define
       can be removed */
  }

  /* Array sizes shall be the same */
  nusmv_assert(left_lower_bound == right_lower_bound);
  nusmv_assert(left_upper_bound == right_upper_bound);

  {
    int i;
    for (i = left_upper_bound; i >= left_lower_bound; --i) {
      node_ptr num, cond, left_body, right_body;

      num = find_node(nodemgr, NUMBER, NODE_FROM_INT(i), Nil);
      left_body = find_node(nodemgr, ARRAY, left, num);

      if (right_is_local_array) {
        right_body = right_local_array[i - left_lower_bound];
      }
      else {
        right_body = find_node(nodemgr, ARRAY, right, num);
        /* it is necessary to fully resolve newly created ARRAY
           expression for 2 reasons:
           1) the expression can be a define and the flatting mode
              is set to define-expansion.
           2) the array ID (left child of ARRAY) may be a define
              which are always resolved in ARRAY expression. */
        right_body = _THROW(right_body, symb_table, def_hash, Nil, mode);
      }

      /* appending next/init */
      left_body = find_node(nodemgr, left_type, left_body, Nil);

      if (right_is_next) {
        right_body = find_node(nodemgr, NEXT, right_body, Nil);
      }

      /* Producing EQDEF on elements of array */
      cond = new_node(nodemgr, EQDEF, left_body, right_body);

      /* To handle possible nexting of CONST_ARRAY and array
         variables in the left and side */
      cond = _THROW(cond, symb_table, def_hash, context, mode);

      if ((left_upper_bound - left_lower_bound == 0) ||
          (i == left_upper_bound)) {
        result = cond;
      }
      else {
        result = new_node(nodemgr, AND, result, cond);
      }
    }  /* for loop */
  }

  return result;
}
