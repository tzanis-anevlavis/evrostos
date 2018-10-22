flex_target(nusmv_compass_prob_lexer prob_input.l
            "${CMAKE_CURRENT_BINARY_DIR}/prob_input.c"
            )

if (MSVC) 
    add_definitions(-DYY_NO_UNISTD_H)
endif()

bison_target(nusmv_compass_prob_parser prob_grammar.y
             "${CMAKE_CURRENT_BINARY_DIR}/prob_grammar.c"
             COMPILE_FLAGS "-d -p parser_prob_")

set(SRCS
    ParserProb.c
    "${CMAKE_CURRENT_BINARY_DIR}/prob_input.c"
    "${CMAKE_CURRENT_BINARY_DIR}/prob_grammar.c"
    )

nusmv_add_pkg_addon(${SRCS})

nusmv_add_pkg_headers(ParserProb.h)
