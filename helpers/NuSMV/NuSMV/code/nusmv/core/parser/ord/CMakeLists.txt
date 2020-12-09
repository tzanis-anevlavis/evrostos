flex_target(nusmv_core_ord_lexer ord_input.l
            "${CMAKE_CURRENT_BINARY_DIR}/ord_input.c"
            )

bison_target(nusmv_core_ord_parser ord_grammar.y
             "${CMAKE_CURRENT_BINARY_DIR}/ord_grammar.c"
             COMPILE_FLAGS "-d -p parser_ord_")

set(SRCS
  ParserOrd.c
  "${CMAKE_CURRENT_BINARY_DIR}/ord_grammar.c"
  "${CMAKE_CURRENT_BINARY_DIR}/ord_input.c"
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(ParserOrd.h)
