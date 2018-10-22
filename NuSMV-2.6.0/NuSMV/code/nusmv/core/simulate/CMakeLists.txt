set(SRCS
  simulate.c 
  simulateTransSet.c
  SimulateState.c
  simulatePkg.c
  )

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  simulate.h
  simulateTransSet.h
  SimulateState.h
  SimulateState_private.h
  )
