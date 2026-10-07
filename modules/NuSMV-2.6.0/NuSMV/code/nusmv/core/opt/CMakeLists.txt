set(SRCS
  OptsHandler.c
  optPkg.c
  opt.c
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  opt.h
  OptsHandler.h
  optPkg.h
  )
