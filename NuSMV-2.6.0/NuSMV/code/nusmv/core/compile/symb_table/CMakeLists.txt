set(SRCS
  NFunction.c
  ResolveSymbol.c
  SymbCache.c
  SymbLayer.c
  symb_table.c
  SymbTable.c
  SymbType.c
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  NFunction.h
  SymbCache.h
  SymbLayer.h
  symb_table.h
  SymbType_private.h
  ResolveSymbol.h
  SymbCache_private.h
  SymbLayer_private.h
  SymbTable.h
  SymbType.h
  )
