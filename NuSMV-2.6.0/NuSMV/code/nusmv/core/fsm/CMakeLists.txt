set(SRCS
fsm.c
FsmBuilder.c

bdd/BddFsm.c
bdd/BddFsmPrint.c
bdd/BddFsmCache.c
bdd/bddMisc.c
bdd/FairnessList.c

be/BeFsm.c

sexp/BoolSexpFsm.c
sexp/SexpFsm.c
)

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  FsmBuilder.h
  fsm.h

  sexp/BoolSexpFsm.h
  sexp/SexpFsm_private.h
  sexp/BoolSexpFsm_private.h
  sexp/SexpFsm.h
  sexp/sexp.h

  bdd/BddFsm.h
  bdd/BddFsm_private.h
  bdd/bdd.h
  bdd/FairnessList.h

  be/BeFsm.h
  )
