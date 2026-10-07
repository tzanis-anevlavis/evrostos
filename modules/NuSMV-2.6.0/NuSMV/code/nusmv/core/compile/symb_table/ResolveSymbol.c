/* ---------------------------------------------------------------------------

  This file is part of the ``compile'' package of NuSMV version 2.
  Copyright (C) 2010 by FBK-irst.

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
  \author Marco Roveri Alessandro Mariotti
  \brief Implementation of class 'ResolveSymbol'

  Basic routines for resolving a symbol

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/utils/ErrorMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/compile/symb_table/ResolveSymbol.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/compile/compile.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/utils/error.h"
#include "nusmv/core/utils/Logger.h"

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
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

typedef struct ResolveSymbol_TAG
{
  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */
  boolean initialized;

  boolean isVar;
  boolean isDefine;
  boolean isArrayDef;
  boolean isArray;
  boolean isParameter;
  boolean isConstantSimple;
  boolean isConstantComplex;
  boolean isFunction;

  node_ptr resolvedName;
  node_ptr name;
  node_ptr context;
} ResolveSymbol;

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/
extern int nusmv_yylineno;
/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define CHECK_INITIALIZED(self)                 \
  nusmv_assert(true == self->initialized)

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

static void resolve_symbol_init(ResolveSymbol_ptr self);
static void resolve_symbol_deinit(ResolveSymbol_ptr self);


static node_ptr
resolve_symbol_resolve_name(const SymbTable_ptr symb_table,
                            node_ptr n, node_ptr context);

static node_ptr
resolve_symbol_resolve_name_recur(const SymbTable_ptr symb_table,
                                  node_ptr n, node_ptr context);


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

ResolveSymbol_ptr ResolveSymbol_create(void)
{
  ResolveSymbol_ptr self = ALLOC(ResolveSymbol, 1);
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);

  resolve_symbol_init(self);
  return self;
}

void ResolveSymbol_destroy(ResolveSymbol_ptr self)
{
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);

  resolve_symbol_deinit(self);
  FREE(self);
}

boolean ResolveSymbol_is_undefined(ResolveSymbol_ptr self)
{
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  CHECK_INITIALIZED(self);
  return ((self->isVar       + self->isDefine         +
           self->isArrayDef  + self->isArray          +
           self->isParameter + self->isConstantSimple +
           self->isConstantComplex + self->isFunction) == 0) ? true : false;
}

boolean ResolveSymbol_is_defined(ResolveSymbol_ptr self)
{
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  CHECK_INITIALIZED(self);
  return ((self->isVar       + self->isDefine         +
           self->isArrayDef  + self->isArray          +
           self->isParameter + self->isConstantSimple +
           self->isConstantComplex + self->isFunction) == 0) ? false : true;
}

boolean ResolveSymbol_is_ambiguous(ResolveSymbol_ptr self)
{
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  CHECK_INITIALIZED(self);
  return ((self->isVar       + self->isDefine         +
           self->isArrayDef  + self->isArray          +
           self->isParameter + self->isConstantSimple +
           self->isConstantComplex + self->isFunction) > 1) ? true : false;
}

boolean ResolveSymbol_is_var(ResolveSymbol_ptr self)
{
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  CHECK_INITIALIZED(self);
  return self->isVar;
}

boolean ResolveSymbol_is_define(ResolveSymbol_ptr self)
{
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  CHECK_INITIALIZED(self);
  return self->isDefine;
}

boolean ResolveSymbol_is_function(ResolveSymbol_ptr self)
{
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  CHECK_INITIALIZED(self);
  return self->isFunction;
}

boolean ResolveSymbol_is_constant(ResolveSymbol_ptr self)
{
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  CHECK_INITIALIZED(self);
  return (1 == (self->isConstantSimple +
                self->isConstantComplex)) ? true : false;
}

boolean ResolveSymbol_is_parameter(ResolveSymbol_ptr self)
{
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  CHECK_INITIALIZED(self);
  return self->isParameter;
}

boolean ResolveSymbol_is_array(ResolveSymbol_ptr self)
{
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  CHECK_INITIALIZED(self);
  return self->isArray;
}

boolean ResolveSymbol_is_array_def(ResolveSymbol_ptr self)
{
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  CHECK_INITIALIZED(self);
  return self->isArrayDef;
}

node_ptr ResolveSymbol_get_resolved_name(ResolveSymbol_ptr self)
{
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  return self->resolvedName;
}

boolean ResolveSymbol_is_error(ResolveSymbol_ptr self)
{
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  CHECK_INITIALIZED(self);
  return ResolveSymbol_is_ambiguous(self) ||
    ResolveSymbol_is_undefined(self);
}

char* ResolveSymbol_get_error_message(ResolveSymbol_ptr self,
                                      MasterPrinter_ptr printer)
{
  char* message = (char*)NULL;

  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  CHECK_INITIALIZED(self);
  nusmv_assert(ResolveSymbol_is_error(self));

  if (ResolveSymbol_is_undefined(self)) {
    char* undef = sprint_node(printer, self->resolvedName);

    message = ALLOC(char, strlen(undef) + 23);

    sprintf(message, "\"%s\" undefined", undef);
  }
  else if (ResolveSymbol_is_ambiguous(self)) {
    char* s1 = sprint_node(printer, self->name);
    char* s2 = sprint_node(printer, self->context);

    message = ALLOC(char, strlen(s1) + strlen(s2) + 29);

    sprintf(message, "Symbol \"%s\" is ambiguous in \"%s\"", s1, s2);
  }

  return message;
}

void ResolveSymbol_print_error_message(ResolveSymbol_ptr self,
                                       MasterPrinter_ptr printer,
                                       FILE* stream)
{
  char* err;
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  CHECK_INITIALIZED(self);

  err = ResolveSymbol_get_error_message(self, printer);

  fprintf(stream, "%s\n", err);

  FREE(err);
}

void ResolveSymbol_throw_error(ResolveSymbol_ptr self,
                               const NuSMVEnv_ptr env)
{
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  char *err;
  const MasterPrinter_ptr wffprint =
    MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));

  RESOLVE_SYMBOL_CHECK_INSTANCE(self);
  CHECK_INITIALIZED(self);

  err = ResolveSymbol_get_error_message(self, wffprint);

  ErrorMgr_rpterr(errmgr, "%s", err);
  FREE(err);
}

node_ptr ResolveSymbol_resolve(ResolveSymbol_ptr self, SymbTable_ptr st,
                               node_ptr name, node_ptr context)
{
  RESOLVE_SYMBOL_CHECK_INSTANCE(self);

  {
    const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(st));
    const NodeMgr_ptr nodemgr =
      NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
    Logger_ptr const logger = LOGGER(NuSMVEnv_get_value(env, ENV_LOGGER));
    MasterPrinter_ptr const wffprint =
      MASTER_PRINTER(NuSMVEnv_get_value(env, ENV_WFF_PRINTER));
    OptsHandler_ptr const opts =
      OPTS_HANDLER(NuSMVEnv_get_value(env, ENV_OPTS_HANDLER));

    node_ptr simpleName, complexName;

    nusmv_assert(Nil != name);

#ifdef RESOLVE_SYMBOL_DEBUG
    Logger_vnlog_trace(logger, wffprint, opts, "Input:\nname %N\ncontext %N\n",
                       name, context);
#endif

      /* Being ResolveSymbol_resolve the main function of the class, we
         can reset all fields, in order to work on a clean structure. */
      resolve_symbol_init(self);

    /* Build uniq internal representation for name */
    simpleName = find_atom(nodemgr, name);
    /* Build the complex name: resolve_symbol_resolve_name cannot be
       applied to a parameter, and parameters can be only simple ATOM
       prefixed with the context name */
    complexName = (ATOM == node_get_type(name)) ?
      find_node(nodemgr, DOT, context, simpleName) :
      resolve_symbol_resolve_name(st, simpleName, context);

    self->name = simpleName;
    self->context = context;

    /* only ATOMs can be parameters */
    self->isVar = SymbTable_is_symbol_var(st, complexName);
    self->isDefine = SymbTable_is_symbol_define(st, complexName);
    self->isArray = SymbTable_is_symbol_variable_array(st, complexName);
    self->isArrayDef = SymbTable_is_symbol_array_define(st, complexName);
    self->isParameter = SymbTable_is_symbol_parameter(st, complexName);
    self->isFunction = SymbTable_is_symbol_function(st, complexName);

    /* Pick the symbol suffix, and then check if there exists a constant
       with that name. If so, this symbol is ambiguous. Remember that
       constants have global scope and should not clash with any other
       symbol. */
    if (Nil != complexName) {
      node_ptr curr = complexName;

      while (DOT == node_get_type(curr)) { curr = cdr(curr); }

      if (ATOM == node_get_type(curr)) {
        self->isConstantSimple = SymbTable_is_symbol_constant(st, curr);
      }
    }

    /* We may have that simpleName and complexName are the same: If a
       constant is declared as (DOT Nil (ATOM name)), for example. But
       this is not ambiguous, so we just have to set only one of the two
       constants fields */
    if (simpleName != complexName) {
      /* Check if simpleName is a constant */
      self->isConstantSimple |= SymbTable_is_symbol_constant(st, simpleName);
    }

    /* Check if complexName is a constant */
    self->isConstantComplex = SymbTable_is_symbol_constant(st, complexName);

    self->resolvedName = NODE_FROM_INT(-1);
    if (self->isConstantSimple) {
      self->resolvedName = simpleName;
    }
    else {
      self->resolvedName = complexName;
    }
    self->initialized = true;

#ifdef RESOLVE_SYMBOL_DEBUG
    Logger_vnlog_trace(logger, wffprint, opts,
                       "Output:\nname %N\n",
                       self->resolvedName);
#endif

    return self->resolvedName;
  }
}


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

/*!
  \brief The ResolveSymbol class private initializer

  The ResolveSymbol class private initializer

  \sa ResolveSymbol_create
*/
static void resolve_symbol_init(ResolveSymbol_ptr self)
{
  /* members initialization */
  self->isVar = self->isDefine = self->isArray = false;
  self->isArrayDef = self->isParameter = self->isFunction = false;
  self->isConstantSimple = self->isConstantComplex = false;
  self->resolvedName = NODE_FROM_INT(-1);
  self->name = NODE_FROM_INT(-1);
  self->context = NODE_FROM_INT(-1);
  self->initialized = false;
}

/*!
  \brief The ResolveSymbol class private deinitializer

  The ResolveSymbol class private deinitializer

  \sa ResolveSymbol_destroy
*/
static void resolve_symbol_deinit(ResolveSymbol_ptr self)
{
  /* members deinitialization */

}


/**AutomaticEnd***************************************************************/

/*!
  \brief Takes an expression representing an identifier
   and recursively normalizes it.

  The result of this function is a properly formed identifier,
  find_atom-ed, and suitable to access hash tables such as symbol
  table, etc.

  An input expression may be a simple or complex (with DOT)
  identifier, a bit or an array element.

  Note: identifiers are not expanded, i.e. defines and formal parameters
  are not substituted by the corresponding expressions.

  Nil is returned if the given expression is not syntactically an
  identifier.

  Currently, arrays are additionally flattened to maintain old code.
  See the description of flattener_core_flatten for info about ARRAY.

  \sa resolve_symbol_resolve_name_recur
*/
static node_ptr resolve_symbol_resolve_name(const SymbTable_ptr symb_table,
                                            node_ptr name, node_ptr context)
{
  node_ptr res;
  int temp;

  if (Nil == name)
    return Nil;

  temp = nusmv_yylineno;
  nusmv_yylineno = node_get_lineno(name);
  res = resolve_symbol_resolve_name_recur(symb_table, name, context);
  nusmv_yylineno = temp;

  if (TYPE_ERROR == res)
    return Nil;

  return res;
}

/*!
  \brief Performs the name "normalization", i.e.
   applies find_node and merges context with the identifier.


   If name is complex and first ATOM is a parameter then the parameter is
   substituted by its value (in order to pass modules in parameters and
   access their members).
   Returns TYPE_ERROR if not resolvable name is provided

  \sa resolve_symbol_resolve_name
*/
static node_ptr
resolve_symbol_resolve_name_recur(const SymbTable_ptr symb_table,
                                   node_ptr n, node_ptr context)
{
  const NuSMVEnv_ptr env = EnvObject_get_environment(ENV_OBJECT(symb_table));
  const NodeMgr_ptr nodemgr =
    NODE_MGR(NuSMVEnv_get_value(env, ENV_NODE_MGR));
  const ErrorMgr_ptr errmgr =
    ERROR_MGR(NuSMVEnv_get_value(env, ENV_ERROR_MANAGER));
  node_ptr temp, name;

  /* Warning : nusmv_yylineno should NOT be set here.
     The upper level function has to decide which line number to use */

  switch (node_get_type(n)) {

  case CONTEXT:
    return resolve_symbol_resolve_name_recur(symb_table, cdr(n), car(n));

  case ATOM:
    name = find_node(nodemgr, DOT, context, find_atom(nodemgr, n));
    return name;

  case NUMBER:
    return find_atom(nodemgr, n);

  case BIT:
    temp = resolve_symbol_resolve_name_recur(symb_table, car(n), context);
    if (TYPE_ERROR == temp) {
      ErrorMgr_rpterr(errmgr, "error in name resolution, operator bit");
    }
    return find_node(nodemgr, BIT, temp, cdr(n)); /* cdr(n) is a int */

  case DOT:
    temp = (node_ptr) NULL;
    if (car(n) != (node_ptr) NULL) {
      temp = resolve_symbol_resolve_name_recur(symb_table, car(n), context);

      /* Check if temp is a module parameter*/
      while (SymbTable_is_symbol_parameter(symb_table, temp)) {
        int line_tmp = nusmv_yylineno;
        /* unflattened version is accessed here !! */
        nusmv_yylineno =
            node_get_lineno(SymbTable_get_actual_parameter(symb_table, temp));
        /* Recursively solve the parameter (required for modules passed
           as parameters). note: here flattened version is accessed */
        temp = resolve_symbol_resolve_name_recur(
            symb_table,
            SymbTable_get_flatten_actual_parameter(symb_table, temp),
            Nil);

        nusmv_yylineno = line_tmp;
      }

      if (TYPE_ERROR == temp) {
        ErrorMgr_rpterr(errmgr, "error in name resolution, operator = .");
      }
    }
    /* on the right of DOT can be only ATOM */
    return find_node(nodemgr, DOT, temp, find_atom(nodemgr, cdr(n)));

  case ARRAY: {
    node_ptr index;
    /* ARRAY may be an expression and may be an identifier-with-brackets.
       Here we care only if array is an identifier. In this case
       index has to be a NUMBER. */
    temp = resolve_symbol_resolve_name_recur(symb_table, car(n), context);
    if (TYPE_ERROR == temp)
      return temp; /* array is expression */

    /* Check if temp is a module parameter*/
    while (SymbTable_is_symbol_parameter(symb_table, temp)) {
      int line_tmp = nusmv_yylineno;
      /* unflattened version is accessed here !! */
      nusmv_yylineno = node_get_lineno(
          SymbTable_get_actual_parameter(symb_table, temp));
      /* Recursively solve the parameter (required for modules passed
         as parameters). note: here flattened version is accessed */
      temp = resolve_symbol_resolve_name_recur(
          symb_table,
          SymbTable_get_flatten_actual_parameter(symb_table, temp),
          Nil);

      nusmv_yylineno = line_tmp;
    }

    /* on the right of [] we care only about NUMBER or MINUS NUMBER */
    index = cdr(n);
    if (node_get_type(index) == NUMBER) {
      index = find_atom(nodemgr, index);
    }
    else if (node_get_type(index) == UMINUS &&
        node_get_type(car(index)) == NUMBER) {
      index = find_node(nodemgr, NUMBER,
                        PTR_FROM_INT(node_ptr, -node_get_int(car(index))),
                        Nil);
    }
    else {
      /* keep the index-expression as it is since this exp is not identifier
         and we do not care about its normalization */
    }

    return find_node(nodemgr, ARRAY, temp, index);
  }

  case BIT_SELECTION:
    {
      /* not clear why bit selection is here, but it was introduced in
         74da89fc800d4062d7b6e89e955d85a2907918af and seems to be
         needed to solve a bug (not better specified) */
      node_ptr name;
      node_ptr t1, t2;
      name = resolve_symbol_resolve_name_recur(symb_table, car(n), context);
      if (name == TYPE_ERROR)
        return TYPE_ERROR;

      nusmv_assert(node_get_type(cdr(n)) == COLON);
      t1 = resolve_symbol_resolve_name_recur(symb_table, car(cdr(n)), context);
      if (t1 == TYPE_ERROR)
        return TYPE_ERROR;

      t2 = resolve_symbol_resolve_name_recur(symb_table, cdr(cdr(n)), context);
      if (t2 == TYPE_ERROR)
        return TYPE_ERROR;

      return find_node(nodemgr, BIT_SELECTION, name,
                       find_node(nodemgr, COLON, t1, t2));
    }

  case SELF:
    return context;

  default:
    break;  /* TYPE_ERROR */
  }

  return TYPE_ERROR;
}
