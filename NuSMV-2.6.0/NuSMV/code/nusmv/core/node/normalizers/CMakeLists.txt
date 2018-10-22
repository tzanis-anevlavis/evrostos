set(SRCS
  MasterNormalizer.c
  NormalizerBase.c
  NormalizerCore.c
  NormalizerPsl.c
)

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  MasterNormalizer.h
  NormalizerBase.h
  NormalizerCore.h
  NormalizerPsl.h
  NormalizerBase_private.h  
  )
