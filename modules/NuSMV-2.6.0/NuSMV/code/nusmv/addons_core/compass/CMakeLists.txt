add_subdirectory(parser)

set(SRCS
  compassPkg.c
  compile/ProbAssign.c
  compile/compile.c
  sigref/sigrefWrite.c
  compass.c
  compassCmd.c
  )

nusmv_add_pkg_addon(${SRCS})

nusmv_add_pkg_headers(compass.h compile/ProbAssign.h compassCmd.h)
