
#ifndef __NUSMV_CORE_PARSER_ORD_ORD_INT_H__
#define __NUSMV_CORE_PARSER_ORD_ORD_INT_H__

#include "nusmv/core/parser/ord/ParserOrd.h"
#include "nusmv/core/utils/utils.h"
#include "nusmv/core/opt/opt.h"


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
#ifndef YY_TYPEDEF_YY_BUFFER_STATE
#define YY_TYPEDEF_YY_BUFFER_STATE
typedef struct yy_buffer_state* YY_BUFFER_STATE;
#endif


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

extern int parser_ord_lineno;
extern FILE* parser_ord_in;


/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
void parser_ord_set_global_parser(ParserOrd_ptr parser);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
void parser_ord_reset_global_parser(ParserOrd_ptr parser);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
ParserOrd_ptr parser_ord_get_global_parser(void);


/* from generated code: */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
int parser_ord_lex(void);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
int parser_ord_parse(void);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
void parser_ord_restart(FILE* input_file);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
void 
parser_ord__switch_to_buffer(YY_BUFFER_STATE new_buffer);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
YY_BUFFER_STATE 
parser_ord__create_buffer(FILE* file, int size);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
void parser_ord__delete_buffer(YY_BUFFER_STATE buf);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
YY_BUFFER_STATE 
parser_ord__scan_string(const char* str);


#endif /* __NUSMV_CORE_PARSER_ORD_ORD_INT_H__ */
