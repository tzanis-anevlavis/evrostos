set(SUBDIRS
  normalizers
  printers
  anonymizers
  )
foreach(_d ${SUBDIRS})
    add_subdirectory(${_d})
endforeach()

set(SRCS
  node.c
  MasterNodeWalker.c
  NodeMgr.c
  nodePkg.c
  nodePrint.c
  NodeWalker.c
  nodeWffPrint.c
)

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  node.h
  MasterNodeWalker.h
  NodeWalker.h
  NodeMgr.h
  NodeWalker_private.h
  )
