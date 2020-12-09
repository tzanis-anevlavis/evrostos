set(SRCS
  ltl.c
  ltlCompassion.c
  ltl2smv/ltl2smv.c
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(ltl.h ltl2smv/ltl2smv.h)
