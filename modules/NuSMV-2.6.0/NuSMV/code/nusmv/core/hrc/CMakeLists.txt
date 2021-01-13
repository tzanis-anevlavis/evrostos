add_subdirectory(dumpers)

set(SRCS
  hrc.c
  hrcDump.c
  HrcFlattener.c
  HrcNode.c
  hrcPrefixUtils.c
  hrcSymbTableUtils.c
  HrcVarDependencies.c
  hrcWrite.c
)

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  hrc.h
  HrcFlattener.h
  HrcNode.h
  HrcNode_private.h
  hrcPrefixUtils.h
  HrcVarDependencies.h
  hrcSymbTableUtils.h
  )

