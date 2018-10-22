set(SRCS
  rbcCnf.c
  rbcCnfCompact.c
  rbcCnfSimple.c
  rbcFormula.c
  rbcInline.c
  rbcManager.c
  rbcOutput.c
  rbcStat.c
  rbcSubst.c
  rbcUtils.c
  ConjSet.c
  InlineResult.c
  clg/clgClg.c  
)

nusmv_get_pkg_name(_curpkg)
nusmv_add_pkg(${_curpkg} rbc ${SRCS})

nusmv_add_pkg_headers(
  rbc.h
  rbcInt.h
  ConjSet.h
  InlineResult.h
  clg/clg.h
  )
