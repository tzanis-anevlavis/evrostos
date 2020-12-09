flex_target(nusmv_core_psl_lexer psl_input.l
            "${CMAKE_CURRENT_BINARY_DIR}/psl_input.c"
            COMPILE_FLAGS "-Ppsl_yy")
    
bison_target(nusmv_core_psl_parser psl_grammar.y
             "${CMAKE_CURRENT_BINARY_DIR}/psl_grammar.c"
             COMPILE_FLAGS "-d -p psl_yy")

set(SRCS
  pslExpr.c
  pslNode.c
  pslConv.c
  "${CMAKE_CURRENT_BINARY_DIR}/psl_grammar.c"
  "${CMAKE_CURRENT_BINARY_DIR}/psl_input.c"
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  pslExpr.h
  pslNode.h
  "${CMAKE_CURRENT_BINARY_DIR}/psl_grammar.h"
  psl_symbols.h
  )
