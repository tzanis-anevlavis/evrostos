set(SRCS
  bdd/BddTrans.c
  bdd/Cluster.c
  bdd/ClusterList.c
  bdd/ClusterOptions.c
  generic/GenericTrans.c
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  trans.h
  transInt.h
  
  bdd/bdd.h
  bdd/BddTrans.h
  bdd/Cluster.h
  bdd/ClusterList.h
  bdd/ClusterOptions.h

  generic/GenericTrans.h
  generic/GenericTrans_private.h
  )
