set(EXAMPLE_DIRS
  abp
  example_irst
  prod-cons
  tcas guidance
  production-cell
  brp
  queue
  deadlock
  p-queue
  reactor
  example_cmu
  pci
  smv-dist
  msi
  bmc_tutorial
  m4
  psl-samples
  )

install(DIRECTORY ${EXAMPLE_DIRS} DESTINATION share/nusmv/examples
        PATTERN "CVS" EXCLUDE
        PATTERN "*~" EXCLUDE
        PATTERN ".*" EXCLUDE
        )
      