if (MSVC) 
    add_definitions(-DYY_NO_UNISTD_H)
endif()

set(SUBDIRS
  ord
  idlist
  psl
  )
foreach(_d ${SUBDIRS})
    add_subdirectory(${_d})
endforeach()

nusmv_combine_scanner("${CMAKE_CURRENT_BINARY_DIR}/input.l"
  input.l.1.25
  input.l.1.50
  input.l.2.25
  input.l.2.50
  input.l.2.51
  input.l.2.75
  input.l.3.50
  )

nusmv_combine_grammar(begin "${CMAKE_CURRENT_BINARY_DIR}/grammar.y"
  grammar.y.1.25
  grammar.y.1.50
  grammar.y.2.50
  grammar.y.2.51
  grammar.y.3.50
  )

flex_target(nusmv_core_lexer
            "${CMAKE_CURRENT_BINARY_DIR}/input.l"
            "${CMAKE_CURRENT_BINARY_DIR}/input.c"
            COMPILE_FLAGS "-Pnusmv_yy")

bison_target(nusmv_core_parser
             "${CMAKE_CURRENT_BINARY_DIR}/grammar.y"
             "${CMAKE_CURRENT_BINARY_DIR}/grammar.c"
             COMPILE_FLAGS "-d -p nusmv_yy")

set(SRCS
  parserUtil.c
  parser.c
  parserPkg.c
  )
nusmv_add_pkg_lib(${SRCS})

set(GRAMMAR_SRCS
    "${CMAKE_CURRENT_BINARY_DIR}/input.c"
    "${CMAKE_CURRENT_BINARY_DIR}/grammar.c"
    )
nusmv_get_pkg_name(_curpkg)
nusmv_add_pkg(${_curpkg}_grammar grammar ${GRAMMAR_SRCS})

nusmv_add_pkg_headers(parser.h symbols.h  parserInt.h)
