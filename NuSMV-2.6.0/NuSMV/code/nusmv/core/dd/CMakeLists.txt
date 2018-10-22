set(SRCS
  DDMgr.c
  dd.c
  VarsHandler.c  
)

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(DDMgr.h dd.h VarsHandler.h)
