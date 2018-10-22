set(SRCS
  cinit.c
  cinitCmd.c
  )

if (PACKAGE_MODE)
  add_definitions(-DDATADIR=\"~\")
else()
  add_definitions(-DDATADIR=\"${PROJECT_BINARY_DIR}\")
endif()

nusmv_add_pkg_shell(${SRCS})

nusmv_add_pkg_headers(cinitCmd.h cinit.h)
