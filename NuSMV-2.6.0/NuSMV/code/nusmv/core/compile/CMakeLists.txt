set(SUBDIRS
  dependency
  flattening
  symb_table
  type_checking
  )
foreach(_d ${SUBDIRS})
    add_subdirectory(${_d})
endforeach()

set(SRCS
  compileBEval.c
#  compileBEvalNew.c
  compile.c
  compileCheck.c
  compileCone.c
  compileExpr.c
  compileFlatten.c
  compileHrc.c
  compilePkg.c
  compileStruct.c
  compileUtil.c
  compileWrite.c
  compileWriteUdg.c
  FlatHierarchy.c
  PredicateExtractor.c
  PredicateNormaliser.c  
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  compile.h
  FlatHierarchy.h
  PredicateExtractor.h
  PredicateNormaliser.h
  compileUtil.h
  )
