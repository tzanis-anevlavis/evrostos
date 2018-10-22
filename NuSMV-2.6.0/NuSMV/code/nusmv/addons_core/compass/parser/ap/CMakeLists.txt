flex_target(nusmv_compass_ap_lexer ap_input.l
            "${CMAKE_CURRENT_BINARY_DIR}/ap_input.c"
            )
if (MSVC) 
    add_definitions(-DYY_NO_UNISTD_H)
endif()

bison_target(nusmv_compass_ap_parser ap_grammar.y
             "${CMAKE_CURRENT_BINARY_DIR}/ap_grammar.c"
             COMPILE_FLAGS "-d -p parser_ap_")

set(SRCS
    ParserAp.c
    "${CMAKE_CURRENT_BINARY_DIR}/ap_input.c"
    "${CMAKE_CURRENT_BINARY_DIR}/ap_grammar.c"
    )

nusmv_add_pkg_addon(${SRCS})

nusmv_add_pkg_headers(ParserAp.h)
