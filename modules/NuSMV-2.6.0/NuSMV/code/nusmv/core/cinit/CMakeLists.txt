set(SRCS
  cinitInit.c
  NuSMVEnv.c
  cinitData.c
  cinitVers.c
  cinitBatch.c
)

if (PACKAGE_MODE)
  add_definitions(-DDATADIR=\"~\")
else()
  add_definitions(-DDATADIR=\"${PROJECT_BINARY_DIR}\")
endif()

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(cinit.h NuSMVEnv.h cinitInt.h)
