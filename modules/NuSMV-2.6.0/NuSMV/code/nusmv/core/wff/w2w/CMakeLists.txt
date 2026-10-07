set(SRCS
  wff2nnf.c
)

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(w2w.h w2wInt.h)
