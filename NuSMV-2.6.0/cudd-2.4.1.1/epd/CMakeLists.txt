set(SRC
  epd.c
  )

cudd_add_headers(epd.h)
cudd_add_library(epd ${SRC})
