add_subdirectory(solvers)

set(SRCS
  satUtils.c
  SatSolver.c
  SatIncSolver.c
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  sat.h
  SatSolver.h
  SatIncSolver.h  
  )

