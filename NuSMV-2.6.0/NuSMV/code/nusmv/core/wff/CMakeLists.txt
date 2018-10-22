set(SUBDIRS
    w2w
    lr
    )
foreach(_d ${SUBDIRS})
    add_subdirectory(${_d})
endforeach()

set(SRCS
  wffPkg.c
  exprWff.c
  ExprMgr.c
  wffRewrite.c
)

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(wff.h ExprMgr.h wffRewrite.h)
