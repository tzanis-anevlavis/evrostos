set(SRC
  mtrBasic.c
  mtrGroup.c
  )

cudd_add_headers(mtr.h)           
cudd_add_library(mtr ${SRC})
