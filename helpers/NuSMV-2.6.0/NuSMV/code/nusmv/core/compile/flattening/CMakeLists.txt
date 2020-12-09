set(SRCS
  FlattenerBase.c
  FlattenerCore.c
  flatteningPkg.c
  MasterCompileFlattener.c
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  flatteningPkg.h
  flattening_int.h
  MasterCompileFlattener.h
  MasterCompileFlattener_private.h
  FlattenerBase.h
  FlattenerBase_private.h
  FlattenerCore.h
  FlattenerCore_private.h
  )
