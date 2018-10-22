flex_target(nusmv_core_idlist_lexer idlist_input.l
            "${CMAKE_CURRENT_BINARY_DIR}/idlist_input.c"
            )
    
bison_target(nusmv_core_idlist_parser idlist_grammar.y
             "${CMAKE_CURRENT_BINARY_DIR}/idlist_grammar.c"
             COMPILE_FLAGS "-d -p parser_idlist_")

set(SRCS
  ParserIdList.c
  "${CMAKE_CURRENT_BINARY_DIR}/idlist_grammar.c"
  "${CMAKE_CURRENT_BINARY_DIR}/idlist_input.c"
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(ParserIdList.h)
