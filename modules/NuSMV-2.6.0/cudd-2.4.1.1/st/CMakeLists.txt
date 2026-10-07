set(SRC
  st.c
  )

cudd_add_headers(st.h)
cudd_add_library(st ${SRC})
