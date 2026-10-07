set(SRC
  cpu_time.c
  cpu_stats.c
  getopt.c
  safe_mem.c
  strsav.c
  texpand.c
  ptime.c
  prtime.c
  pipefork.c
  pathsearch.c
  stub.c
  tmpfile.c
  datalimit.c
  )

cudd_add_headers(util.h)           
cudd_add_library(util ${SRC})
