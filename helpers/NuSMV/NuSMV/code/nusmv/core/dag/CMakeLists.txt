set(SRCS
  dagDfs.c
  dagEnStat.c
  dagManager.c
  dagStat.c
  dagVertex.c
  )

nusmv_get_pkg_name(_curpkg)
nusmv_add_pkg(${_curpkg} rbc ${SRCS})

nusmv_add_pkg_headers(dag.h)
