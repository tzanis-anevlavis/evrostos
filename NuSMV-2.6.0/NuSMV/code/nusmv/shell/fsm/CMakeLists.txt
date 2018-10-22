set(SRCS
  bdd/bddCmd.c
  )

nusmv_add_pkg_shell(${SRCS})

nusmv_add_pkg_headers(bdd/bddCmd.h)
