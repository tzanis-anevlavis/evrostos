set(SRCS
  bmcCmd.c
  sbmc/sbmcCmd.c
  )

nusmv_add_pkg_shell(${SRCS})

nusmv_add_pkg_headers(bmcCmd.h sbmc/sbmcCmd.h)
