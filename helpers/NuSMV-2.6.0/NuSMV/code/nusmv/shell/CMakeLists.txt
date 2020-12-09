set(SUBDIRS
  parser
  enc
  ltl
  mc
  opt
  prop
  simulate
  trace
  bmc
  compile
  dd
  fsm
  cinit
  utils
  hrc
  cmd
  )

foreach(_d ${SUBDIRS})
    add_subdirectory(${_d})
endforeach()
