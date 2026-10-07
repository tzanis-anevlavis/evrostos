set(SRCS
  beCnf.c
  beManager.c
  bePkg.c
  beRbcManager.c
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(be.h bePkg.h beRbcManager.h)
