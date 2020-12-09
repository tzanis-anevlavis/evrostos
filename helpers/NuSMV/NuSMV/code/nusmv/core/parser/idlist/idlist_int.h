
#ifndef __NUSMV_CORE_PARSER_IDLIST_IDLIST_INT_H__
#define __NUSMV_CORE_PARSER_IDLIST_IDLIST_INT_H__

#include "nusmv/core/parser/idlist/ParserIdList.h"
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

extern int parser_idlist_lineno;
extern FILE* parser_idlist_in;


/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
void parser_idlist_set_global_parser(ParserIdList_ptr parser);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
void parser_idlist_reset_global_parser(ParserIdList_ptr parser);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
ParserIdList_ptr parser_idlist_get_global_parser(void);


/* from generated code: */

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
int parser_idlist_lex(void);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
int parser_idlist_parse(void);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
void parser_idlist_restart(FILE* input_file);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
void 
parser_idlist__switch_to_buffer(YY_BUFFER_STATE new_buffer);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
YY_BUFFER_STATE 
parser_idlist__create_buffer(FILE* file, int size);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
void parser_idlist__delete_buffer(YY_BUFFER_STATE buf);

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
YY_BUFFER_STATE 
parser_idlist__scan_string(const char* str);


#endif /* __NUSMV_CORE_PARSER_IDLIST_IDLIST_INT_H__ */
