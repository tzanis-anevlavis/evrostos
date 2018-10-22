set(SRCS
  enc.c
  operators.c
  base/BaseEnc.c
  base/BoolEncClient.c
  bdd/BddEnc.c
  bdd/BddEncBddPrintWff.c
  bdd/BddEncCache.c
  be/BeEnc.c
  bool/BitValues.c
  bool/BoolEnc.c
  utils/AddArray.c
  utils/OrdGroups.c
  utils/utils.c
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  enc.h
  operators.h
  
  base/BaseEnc.h
  base/BaseEnc_private.h
  base/BoolEncClient.h
  base/BoolEncClient_private.h
  
  bool/BitValues.h
  bool/BoolEnc.h
  bool/BoolEnc_private.h
  
  bdd/BddEncCache.h
  bdd/BddEnc.h
  bdd/BddEnc_private.h
  bdd/bdd.h
  
  be/BeEnc.h
  be/BeEnc_private.h

  utils/AddArray.h
  utils/OrdGroups.h
  )
