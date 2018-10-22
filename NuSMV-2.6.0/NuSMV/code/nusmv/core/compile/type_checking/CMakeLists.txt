set(SRCS
  TypeChecker.c
  checkers/CheckerBase.c
  checkers/CheckerCore.c
  checkers/CheckerPsl.c
  checkers/checkersInt.c
  checkers/CheckerStatement.c
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  TypeChecker.h
  checkers/CheckerBase.h
  checkers/CheckerCore.h
  checkers/CheckerPsl.h
  )
