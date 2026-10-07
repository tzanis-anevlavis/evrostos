set(SRCS
  Prop.c
  PropDb.c
  propPkg.c
  Prop_Rewriter.c
  propProp.c
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  Prop.h
  Prop_private.h
  PropDb.h
  PropDb_private.h
  propPkg.h
  Prop_Rewriter.h
  Prop_Rewriter_private.h
  propProp.h
  )
