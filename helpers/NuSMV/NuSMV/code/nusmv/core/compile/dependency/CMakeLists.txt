set(SRCS
  DependencyBase.c
  DependencyCore.c
  DependencyPsl.c
  dependencyPkg.c
  FormulaDependency.c
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  dependencyPkg.h
  dependency_int.h
  FormulaDependency.h
  FormulaDependency_private.h
  DependencyBase.h
  DependencyBase_private.h
  DependencyCore.h
  DependencyCore_private.h
  DependencyPsl.h
  DependencyPsl_private.h
  )

